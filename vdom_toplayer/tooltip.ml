open! Core
open Js_of_ocaml
open Virtual_dom
module Effect = Vdom.Effect
module Portal = Byo_portal_private

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
      let timeout_handle =
        Testable_timeout.set_timeout
          ~f:(fun () -> if not (hovering ()) then Popover_dom.hide_popover popover)
          grace_period
      in
      timeout := Some timeout_handle;
      Effect.Ignore
  in
  let on_enter _ =
    Option.iter !timeout ~f:Testable_timeout.cancel;
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
      let timeout_handle =
        Testable_timeout.set_timeout
          ~f:(fun () -> if Popover_dom.is_hovered anchor then open_popover ())
          delay
      in
      timeout := Some timeout_handle;
      Effect.Ignore
  in
  let on_leave _ =
    Option.iter !timeout ~f:Testable_timeout.cancel;
    timeout := None;
    Effect.Ignore
  in
  [ add_event_listener anchor Dom_events.Typ.pointerenter on_enter
  ; add_event_listener anchor Dom_events.Typ.pointerleave on_leave
  ]
;;

open Byo_toplayer_private_floating

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
        ; light_dismiss : bool
        ; show_delay : Time_ns.Span.t option
        ; hide_grace_period : Time_ns.Span.t option
        }
      [@@deriving equal]

      let combine _ b =
        Console.console##warn
          (Js.string "Multiple tooltips cannot be attached on the same element");
        b
      ;;

      let sexp_of_t _ = Sexp.Atom "<omitted>"
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
      ; light_dismiss
      ; show_delay = _
      }
      =
      let position_attr =
        position_me
          ~prepare:Popover_dom.show_popover
          ~arrow_selector:Popover_dom.arrow_selector
          ~position
          ~alignment
          ~offset
          (Anchor.of_element anchor)
      in
      Popover_dom.node
        ?arrow
        ~kind:(if light_dismiss then `Auto else `Manual)
          (* Tooltips should never get focus. But if they somehow do, might as well
             restore it on close. *)
        ~restore_focus_on_close:true
        ~overflow_auto_wrapper:false
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
            (* The portalling root should always be connected to the document. *)
            Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
              Portal.create
                ~parent:(Popover_dom.find_popover_portal_root anchor)
                (wrap_content
                   ~anchor
                   ~close_popover:(close_popover state_ref)
                   state.input))
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

    let on_mount = `Schedule_immediately_after_this_dom_patch_completes on_mount

    let update ~old_input ~new_input (state_ref : State.t) anchor =
      match Input.equal old_input new_input with
      | true -> ()
      | false ->
        State.update state_ref ~f:(fun state ->
          let new_portal =
            match state with
            | { portal = None; _ } -> None
            | { portal = Some portal; _ } ->
              (* We are applying a patch to an already-connected portal. *)
              Vdom.Node.For_changing_dom.with_on_mount_at_end (fun () ->
                Portal.apply_patch
                  portal
                  (wrap_content
                     ~anchor
                     ~close_popover:(close_popover state_ref)
                     new_input))
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
      State.update state_ref ~f:(fun state ->
        Option.iter state.portal ~f:Portal.destroy;
        List.iter state.open_on_hover_listeners ~f:Dom_html.removeEventListener;
        { state with portal = None; open_on_hover_listeners = [] })
    ;;
  end

  include Impl
  include Vdom.Attr.Hooks.Make (Impl)
end

let hook_name = "vdom_tooltip"

let attr
  ?(tooltip_attrs = [])
  ?(position = Position.Auto)
  ?(alignment = Alignment.Center)
  ?(offset = Offset.zero)
  ?show_delay
  ?hide_grace_period
  ?(hoverable_inside = false)
  ?(light_dismiss = true)
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
    ; light_dismiss
    ; show_delay
    ; hide_grace_period
    }
  |> Vdom.Attr.create_hook hook_name
;;

module For_testing_tooltip_hook = struct
  type t = Tooltip_attr.Input.t =
    { content : Vdom_with_phys_equal.Node.t
    ; tooltip_attrs : Vdom_with_phys_equal.Attr.t list
    ; arrow : Vdom_with_phys_equal.Node.t option
    ; position : Position.t
    ; alignment : Alignment.t
    ; offset : Offset.t
    ; hoverable_inside : bool
    ; light_dismiss : bool [@sexp_drop_if fun _ -> true]
    ; show_delay : Time_ns.Span.t option
    ; hide_grace_period : Time_ns.Span.t option
    }
  [@@deriving sexp_of]

  let type_id = Tooltip_attr.For_testing.type_id
  let hook_name = hook_name
end
