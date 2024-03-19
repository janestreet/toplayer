open! Core
open Virtual_dom
open Js_of_ocaml

type t

val apply_patch : t -> Vdom.Node.t -> unit
val create : Vdom.Node.t -> t
val destroy : t -> unit
val element : t -> Dom_html.element Js.t
