open! Core
open Virtual_dom
open Js_of_ocaml

(** This library is a private, vdom-only implementation of portalling, intended for use in
    [Byo_portal] and [Vdom_toplayer]. Do not use it directly. *)

type t

val apply_patch : t -> Vdom.Node.t -> t
val create : parent:Dom_html.element Js.t -> Vdom.Node.t -> t
val destroy : t -> unit
val element : t -> Dom_html.element Js.t

(** {2 global root} *)
val global_toplayer_root : unit -> Dom_html.element Js.t

val ensure_global_toplayer_root_mounted : unit -> unit

module For_testing : sig
  val vdom : t -> Vdom.Node.t
end
