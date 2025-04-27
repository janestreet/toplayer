open! Core
open Virtual_dom
open Floating_positioning_new

val attr
  :  ?tooltip_attrs:Vdom.Attr.t list
  -> ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?show_delay:Time_ns.Span.t
  -> ?hide_grace_period:Time_ns.Span.t
  -> ?hoverable_inside:bool
  -> ?light_dismiss:bool
  -> ?arrow:Vdom.Node.t
  -> Vdom.Node.t
  -> Vdom.Attr.t

module For_testing_tooltip_hook : sig
  type t =
    { content : Vdom.Node.t
    ; tooltip_attrs : Vdom.Attr.t list
    ; arrow : Vdom.Node.t option
    ; position : Position.t
    ; alignment : Alignment.t
    ; offset : Offset.t
    ; hoverable_inside : bool
    ; light_dismiss : bool
    ; show_delay : Time_ns.Span.t option
    ; hide_grace_period : Time_ns.Span.t option
    }
  [@@deriving sexp_of]

  val type_id : t Type_equal.Id.t
  val hook_name : string
end
