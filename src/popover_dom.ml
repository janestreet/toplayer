open! Core
open Js_of_ocaml
open Virtual_dom

let focus_if_document_has_focus (e : Dom_html.element Js.t) =
  (* If we are in an iframe, we don't want to steal focus unless we have focus. *)
  let document : < hasFocus : bool Js.t Js.meth > Js.t =
    Js_of_ocaml.Js.Unsafe.coerce Dom_html.document
  in
  if Js_of_ocaml.Js.to_bool document##hasFocus then e##focus
;;

let show_popover (e : Dom_html.element Js.t) =
  let () = Js.Unsafe.meth_call e "showPopover" [||] in
  if !Config.mark_events
  then Javascript_profiling.mark ~prominent:true "showPopover called"
;;

let hide_popover (e : Dom_html.element Js.t) = Js.Unsafe.meth_call e "hidePopover" [||]
let toggle_popover (e : Dom_html.element Js.t) = Js.Unsafe.meth_call e "togglePopover" [||]

let is_hovered (e : Dom_html.element Js.t) =
  Js.Unsafe.meth_call e "matches" [| Js.Unsafe.inject (Js.string ":hover") |]
  |> Js.to_bool
;;

let is_popover (e : Dom_html.element Js.t) =
  e##hasAttribute (Js.string "popover") |> Js.to_bool
;;

let is_open (e : Dom_html.element Js.t) =
  Js.Unsafe.meth_call e "matches" [| Js.Unsafe.inject (Js.string ":popover-open") |]
  |> Js.to_bool
;;

(* By default, popover elements have `margin:auto`,
   which isn't what we want with floating_positioning.
   We also make overflow be `visible`, since scrollbars in tooltips are unfortunate. *)
let unset_browser_styling =
  let module Style =
    [%css
    stylesheet
      {|
        @layer vdom_toplayer.unset_browser_popover_styles {
          .popover {
            margin: unset;
            overflow: visible;

            &:popover-open {
              display: flex;
            }
          }
        }
        |}]
  in
  Style.popover
;;

let element_contains node other_node =
  let open Js_of_ocaml in
  Js.Unsafe.meth_call node "contains" [| Js.Unsafe.inject other_node |] |> Js.to_bool
;;

module Restore_focus_on_close = Vdom.Attr.Hooks.Make (struct
    module Input = struct
      type t = unit [@@deriving sexp_of]

      let combine () () = ()
    end

    module State = struct
      type t =
        { restore_focus_to : Dom_html.element Js.t option
        ; focusin_listener : Dom_html.event_listener_id
        ; focusout_listener : Dom_html.event_listener_id
        ; focus_was_inside_before_close : bool ref
        }
    end

    let init () popover_root =
      let focus_was_inside_before_close = ref false in
      let focusin_listener =
        Element_listener.add_event_listener
          popover_root
          (Dom.Event.make "focusin")
          (fun _ ->
             focus_was_inside_before_close := true;
             Ui_effect.Ignore)
      in
      let focusout_listener =
        Element_listener.add_event_listener
          popover_root
          (Dom.Event.make "focusout")
          (fun (e : Dom_html.focusEvent Js.t) ->
             (* If the relatedTarget is null, then the popover element is being
                destroyed, and so the focus was inside the popover. *)
             let related_target =
               e##.relatedTarget |> Js.Optdef.to_option |> Option.bind ~f:Js.Opt.to_option
             in
             focus_was_inside_before_close := Option.is_none related_target;
             Ui_effect.Ignore)
      in
      let restore_focus_to = Dom_html.document##.activeElement |> Js.Opt.to_option in
      { State.restore_focus_to
      ; focusin_listener
      ; focusout_listener
      ; focus_was_inside_before_close
      }
    ;;

    let on_mount = `Do_nothing
    let update ~old_input:() ~new_input:() _state _element = ()

    let destroy
      ()
      { State.focusin_listener
      ; focusout_listener
      ; restore_focus_to
      ; focus_was_inside_before_close
      }
      _element
      =
      Dom_html.removeEventListener focusin_listener;
      Dom_html.removeEventListener focusout_listener;
      match !focus_was_inside_before_close, restore_focus_to with
      | true, Some restore_focus_to -> focus_if_document_has_focus restore_focus_to
      | true, None | false, _ -> ()
    ;;
  end)

let restore_focus_on_close =
  Vdom.Attr.create_hook
    "vdom_toplayer_restore_focus_on_close"
    (Restore_focus_on_close.create ())
;;

let focus_popover_on_open =
  Vdom.Attr.on_toggle (fun evt ->
    match Js.Unsafe.get evt "newState" |> Js.to_string with
    | "open" ->
      (match evt##.currentTarget |> Js.Opt.to_option with
       | None -> Vdom.Effect.Ignore
       | Some popover_root ->
         let focus_effect =
           Vdom.Effect.of_thunk (fun () -> focus_if_document_has_focus popover_root)
         in
         (match Dom_html.document##.activeElement |> Js.Opt.to_option with
          | None -> focus_effect
          | Some focused ->
            if element_contains popover_root focused
            then Vdom.Effect.Ignore
            else focus_effect))
    | "closed" | _ -> Vdom.Effect.Ignore)
;;

(* Popovers / tooltips should not be sequentially navigatable, or focusable unless open.
   This also prevents a bug in the Incr_dom focus stealing code, which prevents content
   inside popovers from being focused because they are not in an element which has
   `tabindex` set. *)
let tabindex_attr = Vdom.Attr.tabindex (-1)

let attrs kind =
  Vdom.Attr.many
    [ Vdom.Attr.create
        "popover"
        (match kind with
         | `Auto -> "auto"
         | `Manual -> "manual")
    ; unset_browser_styling
    ; tabindex_attr
    ; restore_focus_on_close
    ; Floating_positioning_new.Accessors.floating_styling
    ]
;;

let arrow_data = "data-floating-ui-arrow-parent"

let wrap_arrow node =
  Vdom.Node.div
    ~attrs:
      [ Vdom.Attr.create arrow_data ""
      ; Floating_positioning_new.Accessors.arrow_container
      ]
    [ node ]
;;

let arrow_selector = [%string "[%{arrow_data}]"]

let node ?arrow ~kind ~extra_attrs content =
  Vdom.Node.div
    ~attrs:([ attrs kind; Portal.For_popovers.nestable_popover_attr ] @ extra_attrs)
    (* [nested_popover_root] MUST be the last child, otherwise nested popovers
       will break.*)
    [ Vdom.Node.div
        ~attrs:
          [ {%css|
                overflow: auto;
                height: 100%;
                width: 100%;
                max-width: inherit;
                max-height: inherit;
              |}
          ]
        [ content ]
    ; Option.value_map
        arrow
        ~f:wrap_arrow
        ~default:(Vdom.Node.none_deprecated [@alert "-deprecated"])
    ; Portal.For_popovers.nested_popover_root
    ]
;;
