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

let show_on_mouseenter ~anchor ~delay ~open_popover =
  let timeout = ref None in
  let on_enter _ =
    match delay with
    | None -> Effect.of_thunk open_popover
    | Some delay ->
      let time_ms = Time_ns.Span.to_ms delay in
      let timeout_handle =
        Dom_html.setTimeout
          (fun () -> if Popover_dom.is_hovered anchor then open_popover ())
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
  ; add_event_listener anchor Dom_events.Typ.pointerleave on_leave
  ]
;;

open Floating_positioning_new

module Tooltip_attr = struct
  module Impl = struct
    module Input = struct
      type t =
        { content : Vdom_with_phys_equal.Node.t
        ; tooltip_attrs : Vdom_with_phys_equal.Attr.t list
        ; arrow : Vdom_with_phys_equal.Node.t option
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
        { portal : Portal.t option
        ; open_on_hover_listeners : Dom_html.event_listener_id list
        ; input : Input.t
        }

      type t = state option ref

      let update t ~f =
        match !t with
        | None -> ()
        | Some state ->
          let new_state = f state in
          t := Some new_state
      ;;
    end

    let close_popover (state_ref : State.t) =
      Effect.of_thunk (fun () ->
        State.update state_ref ~f:(fun state ->
          Option.iter state.portal ~f:(fun portal -> Portal.destroy portal);
          { state with portal = None }))
    ;;

    let wrap_content
      ~anchor
      ~close_popover
      { Input.position
      ; alignment
      ; offset
      ; content
      ; tooltip_attrs
      ; arrow
      ; hide_grace_period
      ; hoverable_inside
      ; show_delay = _
      }
      =
      let position_attr =
        let before_position tooltip =
          (* This runs after the popover has been placed in the DOM, so we need this to
             avoid a race condition where the user mouses over the anchor,
             and [showPopover] ends up being called after the mouse leaves,
             and therefore the "close on mouseleave" listener never runs,
             and the popover stays open. *)
          if Popover_dom.is_hovered anchor
          then Popover_dom.show_popover tooltip
          else Effect.Expert.handle_non_dom_event_exn close_popover
        in
        position_me
          ~prepare:before_position
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
          (tooltip_attrs
           @ [ position_attr
             ; Element_listener.create (fun popover ->
                 hide_on_mouseleave
                   ~hoverable_inside
                   ~anchor
                   ~popover
                   ~grace_period:hide_grace_period)
             ; Vdom.Attr.on_toggle (fun evt ->
                 match Js.Unsafe.get evt "newState" |> Js.to_string with
                 | "closed" -> close_popover
                 | "open" | _ -> Effect.Ignore)
             ])
        content
    ;;

    let init _ _ = ref None

    let open_popover (state_ref : State.t) anchor () =
      State.update state_ref ~f:(fun state ->
        match state.portal with
        | Some _ -> state
        | None ->
          let portal =
            Portal.create
              (Portal.For_popovers.find_popover_portal_root anchor)
              (wrap_content ~anchor ~close_popover:(close_popover state_ref) state.input)
          in
          { state with portal = Some portal })
    ;;

    let on_mount (input : Input.t) (state_ref : State.t) anchor =
      let state =
        let open_on_hover_listeners =
          show_on_mouseenter
            ~anchor
            ~delay:input.show_delay
            ~open_popover:(open_popover state_ref anchor)
        in
        { State.portal = None; open_on_hover_listeners; input }
      in
      state_ref := Some state
    ;;

    let on_mount = `Schedule_animation_frame on_mount

    let update ~old_input ~new_input (state_ref : State.t) anchor =
      match Input.equal old_input new_input with
      | true -> ()
      | false ->
        State.update state_ref ~f:(fun state ->
          let new_portal =
            match state with
            | { portal = None; _ } -> None
            | { portal = Some portal; _ } as state ->
              Portal.apply_patch
                portal
                (wrap_content
                   ~anchor
                   ~close_popover:(close_popover state_ref)
                   state.input)
              |> Some
          in
          let open_on_hover_listeners =
            match
              [%equal: Time_ns.Span.t option] old_input.show_delay new_input.show_delay
            with
            | true -> state.open_on_hover_listeners
            | false ->
              List.iter state.open_on_hover_listeners ~f:Dom_html.removeEventListener;
              show_on_mouseenter
                ~anchor
                ~delay:new_input.show_delay
                ~open_popover:(open_popover state_ref anchor)
          in
          { input = new_input; open_on_hover_listeners; portal = new_portal })
    ;;

    let destroy _ (state_ref : State.t) _ =
      match !state_ref with
      | None -> ()
      | Some { portal; open_on_hover_listeners; _ } ->
        List.iter open_on_hover_listeners ~f:Dom_html.removeEventListener;
        Option.iter portal ~f:Portal.destroy
    ;;
  end

  include Impl
  include Vdom.Attr.Hooks.Make (Impl)
end

let attr
  ?(tooltip_attrs = [])
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
    { tooltip_attrs
    ; position
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
