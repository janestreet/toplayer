open! Core
open Virtual_dom
open Floating_positioning_new

val attr
  :  ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?arrow:Vdom.Node.t
  -> Vdom.Node.t
  -> Vdom.Attr.t

val node
  :  ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?arrow:Vdom.Node.t
  -> popover_content:Vdom.Node.t
  -> Anchor.t
  -> Vdom.Node.t

(* For virtual positioned popover. *)
val show_on_mount : Vdom.Attr.t
