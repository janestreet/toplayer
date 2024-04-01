open! Core
open Js_of_ocaml
open Virtual_dom
module Effect = Vdom.Effect

let add_event_listener = Element_listener.add_event_listener

let hide_on_mouseleave ~hoverable_inside ~anchor ~popover ~grace_period =
  let hovering () =
    match hoverable_inside with
    | true -> Popover_dom.is_hovered anchor && Popover_dom.is_hovered popover
    | false -> Popover_dom.is_hovered anchor
  in
  let timeout = ref None in
  let on_leave _ =
    match grace_period with
    | None -> Effect.of_sync_fun Popover_dom.hide_popover popover
    | Some grace_period ->
      let time_ms = Time_ns.Span.to_ms grace_period in
      let timeout_handle =
        Dom_html.setTimeout
          (fun () -> if not (hovering ()) then Popover_dom.hide_popover popover)
          time_ms
      in
      timeout := Some timeout_handle;
      Effect.Ignore
  in
  let on_enter _ =
    Option.iter !timeout ~f:Dom_html.clearTimeout;
    timeout := None;
    Effect.Ignore
  in
  let anchor_events =
    [ add_event_listener anchor Dom_events.Typ.pointerleave on_leave
    ; add_event_listener anchor Dom_events.Typ.pointerenter on_enter
    ]
  in
  let popover_events =
    if hoverable_inside
    then
      [ add_event_listener popover Dom_events.Typ.pointerleave on_leave
      ; add_event_listener popover Dom_events.Typ.pointerenter on_enter
      ]
    else []
  in
  anchor_events @ popover_events
;;

let show_on_mouseenter ~anchor ~popover ~delay =
  let timeout = ref None in
  let on_enter _ =
    match delay with
    | None -> Effect.of_sync_fun Popover_dom.show_popover popover
    | Some delay ->
      let time_ms = Time_ns.Span.to_ms delay in
      let timeout_handle =
        Dom_html.setTimeout
          (fun () ->
            if Popover_dom.is_hovered anchor then Popover_dom.show_popover popover)
          time_ms
      in
      timeout := Some timeout_handle;
      Effect.Ignore
  in
  let on_leave _ =
    Option.iter !timeout ~f:Dom_html.clearTimeout;
    timeout := None;
    Effect.Ignore
  in
  [ add_event_listener anchor Dom_events.Typ.pointerenter on_enter
  ; add_event_listener popover Dom_events.Typ.pointerleave on_leave
  ]
;;

open Floating_positioning_new

module Tooltip_attr = struct
  module Impl = struct
    module Input = struct
      type t =
        { content : Vdom_with_phys_equal.t
        ; arrow : Vdom_with_phys_equal.t option
        ; position : Position.t
        ; alignment : Alignment.t
        ; offset : Offset.t
        ; hoverable_inside : bool
        ; show_delay : Time_ns.Span.t option
        ; hide_grace_period : Time_ns.Span.t option
        }
      [@@deriving sexp_of, equal]

      let combine _ b =
        Firebug.console##warn "Multiple tooltips cannot be attached on the same element";
        b
      ;;
    end

    module State = struct
      type state =
        { portal : Portal.t
        ; popover_listener : Dom_html.event_listener_id
        ; input : Input.t
        ; is_open : bool
        }

      type t = state option ref

      let update t ~f ~after =
        match !t with
        | None -> ()
        | Some state ->
          let new_state = f state in
          t := Some new_state;
          after new_state
      ;;
    end

    let wrap_content
      ~anchor
      ~is_open
      { Input.position
      ; alignment
      ; offset
      ; content
      ; arrow
      ; show_delay
      ; hide_grace_period
      ; hoverable_inside
      }
      =
      let position_attr =
        match is_open with
        | false -> Vdom.Attr.empty
        | true ->
          position_me
            ~arrow_selector:Popover_dom.arrow_selector
            ~position
            ~alignment
            ~offset
            (Anchor.of_element anchor)
      in
      Popover_dom.node
        ?arrow
        ~kind:`Auto
        ~extra_attrs:
          [ position_attr
          ; Element_listener.create (fun popover ->
              show_on_mouseenter ~anchor ~popover ~delay:show_delay
              @ hide_on_mouseleave
                  ~hoverable_inside
                  ~anchor
                  ~popover
                  ~grace_period:hide_grace_period)
          ]
        content
    ;;

    (* Auto-positioning can be very expensive. Since pages can have many, many, (many)
       tooltips, we only want to apply auto-positioning to them while they are open.
       This also helps prevent some jitter bugs with nested tooltips, since we're never
       calculating a "wrong" position. *)
    let attach_popover_listener ~popover set_is_open =
      add_event_listener popover (Dom_html.Event.make "beforetoggle") (fun toggle_event ->
        match toggle_event##.newState |> Js.to_string with
        | "open" -> Effect.of_sync_fun set_is_open true
        | "closed" | _ -> Effect.of_sync_fun set_is_open false)
    ;;

    let init _ _ = ref None

    let on_mount (input : Input.t) (state_ref : State.t) anchor =
      let state =
        let portal =
          Portal.create
            (Portal.For_popovers.find_popover_portal_root anchor)
            (wrap_content ~anchor ~is_open:false input)
        in
        let popover = Portal.element portal in
        let set_is_open is_open =
          State.update
            state_ref
            ~f:(fun state -> { state with is_open })
            ~after:(fun state ->
              Portal.apply_patch
                portal
                (wrap_content ~anchor ~is_open:state.is_open state.input))
        in
        let popover_listener = attach_popover_listener ~popover set_is_open in
        { State.portal; popover_listener; input; is_open = false }
      in
      state_ref := Some state
    ;;

    let on_mount = `Schedule_animation_frame on_mount

    let update ~old_input ~new_input (state_ref : State.t) anchor =
      match Input.equal old_input new_input with
      | true -> ()
      | false ->
        State.update
          state_ref
          ~f:(fun state -> { state with input = new_input })
          ~after:(function
            (* We patch on open, so no reason to update popovers while they are closed. *)
            | { is_open = false; _ } -> ()
            | { is_open = true; _ } as state ->
              Portal.apply_patch
                state.portal
                (wrap_content ~anchor ~is_open:state.is_open state.input))
    ;;

    let destroy _ (state_ref : State.t) _ =
      match !state_ref with
      | None -> ()
      | Some { portal; popover_listener; _ } ->
        Dom_html.removeEventListener popover_listener;
        Portal.destroy portal
    ;;
  end

  include Impl
  include Vdom.Attr.Hooks.Make (Impl)
end

let attr
  ?(position = Position.Auto)
  ?(alignment = Alignment.Center)
  ?(offset = Offset.zero)
  ?show_delay
  ?hide_grace_period
  ?(hoverable_inside = false)
  ?arrow
  content
  =
  Tooltip_attr.create
    { position
    ; alignment
    ; offset
    ; content
    ; arrow
    ; hoverable_inside
    ; show_delay
    ; hide_grace_period
    }
  |> Vdom.Attr.create_hook "vdom_tooltip"
;;
