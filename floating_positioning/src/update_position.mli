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

module Accessors : sig
  val floating_styling : Virtual_dom.Vdom.Attr.t
  val arrow_container : Virtual_dom.Vdom.Attr.t
end

val single_update
  :  anchor:Bindings.Reference_element.t
  -> floating:Dom_html.element Js.t
  -> arrow_selector:string option
  -> Side.t option
  -> Alignment.t option
  -> Offset.t
  -> Strategy.t
  -> unit
