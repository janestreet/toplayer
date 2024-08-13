open! Core
open Virtual_dom
open Floating_positioning_new

val attr
  :  ?popover_attrs:Vdom.Attr.t list
  -> ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?match_anchor_side_length:Match_anchor_side.t
  -> ?arrow:Vdom.Node.t
  -> Vdom.Node.t
  -> Vdom.Attr.t

val node
  :  ?popover_attrs:Vdom.Attr.t list
  -> ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?match_anchor_side_length:Match_anchor_side.t
  -> ?arrow:Vdom.Node.t
  -> popover_content:Vdom.Node.t
  -> Anchor.t
  -> Vdom.Node.t

module For_testing_popover_hook : sig
  type for_one =
    { content : Vdom.Node.t
    ; popover_attrs : Vdom.Attr.t list
    ; arrow : Vdom.Node.t option
    ; position : Position.t
    ; alignment : Alignment.t
    ; offset : Offset.t
    ; match_anchor_side_length : Match_anchor_side.t option
    }
  [@@deriving sexp_of]

  type t = for_one list [@@deriving sexp_of]

  val type_id : t Type_equal.Id.t
  val hook_name : string
end

module For_testing_bonsai_web_ui_toplayer : sig
  val wrap_anchored_popover
    :  position:Position.t
    -> alignment:Alignment.t
    -> offset:Offset.t
    -> match_anchor_side_length:Match_anchor_side.t option
    -> content:Vdom.Node.t
    -> popover_attrs:Vdom.Attr.t list
    -> arrow:Vdom.Node.t option
    -> anchor:Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t
    -> Vdom.Node.t
end
