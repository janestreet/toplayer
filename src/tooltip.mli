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
  -> ?arrow:Vdom.Node.t
  -> Vdom.Node.t
  -> Vdom.Attr.t
