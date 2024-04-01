open! Core
open Js_of_ocaml
open Virtual_dom

let show_popover (e : Dom_html.element Js.t) = Js.Unsafe.meth_call e "showPopover" [||]
let hide_popover (e : Dom_html.element Js.t) = Js.Unsafe.meth_call e "hidePopover" [||]
let toggle_popover (e : Dom_html.element Js.t) = Js.Unsafe.meth_call e "togglePopover" [||]

let is_hovered (e : Dom_html.element Js.t) =
  Js.Unsafe.meth_call e "matches" [| Js.Unsafe.inject (Js.string ":hover") |]
;;

let is_popover (e : Dom_html.element Js.t) =
  e##hasAttribute (Js.string "popover") |> Js.to_bool
;;

let is_open (e : Dom_html.element Js.t) =
  Js.Unsafe.meth_call e "matches" [| Js.Unsafe.inject (Js.string ":popover-open") |]
;;

(* By default, popover elements have `margin:auto`,
   which isn't what we want with floating_positioning.
   We also remove border/padding, so that users can set it themselves. *)
let unset_browser_styling =
  [%css
    {|margin: unset; border: unset; padding: unset; overflow: visible; color: unset; background-color: unset;|}]
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
    [ content
    ; Option.value_map arrow ~f:wrap_arrow ~default:Vdom.Node.none
    ; Portal.For_popovers.nested_popover_root
    ]
;;
