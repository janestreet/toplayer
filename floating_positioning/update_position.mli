open! Core
open Js_of_ocaml
open Bindings

module Side : sig
  type t =
    | Top
    | Bottom
    | Left
    | Right
end

module Match_anchor_side : sig
  type t =
    | Grow_to_match
    | Match_exactly
    | Shrink_to_match
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Accessors : sig
  val floating_styling : Virtual_dom.Vdom.Attr.t
  val arrow_container : Virtual_dom.Vdom.Attr.t
end

val position_within_viewport : Dom_html.element Js.t -> Strategy.t -> unit

val single_update
  :  anchor:Bindings.Reference_element.t
  -> floating:Dom_html.element Js.t
  -> arrow_selector:string option
  -> match_anchor_side_length:Match_anchor_side.t option
  -> Side.t option
  -> Alignment.t option
  -> Offset.t
  -> Strategy.t
  -> unit

val clear_floating_properties : Dom_html.element Js.t -> unit
