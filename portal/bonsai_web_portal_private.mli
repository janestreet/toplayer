open! Core
open Virtual_dom
open Js_of_ocaml

(** This library is a private, vdom-only implementation of portalling, intended for use in
    [Bonsai_web_portal] and [Vdom_toplayer]. Do not use it directly. *)

type t

val apply_patch : t -> Vdom.Node.t -> t
val create : parent:Dom_html.element Js.t -> Vdom.Node.t -> t
val destroy : t -> unit
val element : t -> Dom_html.element Js.t

(** {2 global root} *)

module Toplayer_root : sig
  (** Determines whether the toplayer root is a child of the HTML element or the body
      element.

      By default, this is the HTML element, as this was more backwards-compatible when
      toplayer was added. It is recommended to always use the HTML element as the parent,
      as now newer systems may also assume this. However, making the toplayer a child of
      the body element can be useful when embedding certain components that assume they
      always have a body element as an ancestor. *)

  type t =
    | Child_of_html
    | Child_of_body
end

(** Defaults to [Child_of_html]. Only applies to when the toplayer root is created for the
    first time, so this must be changed early for it to take effect. *)
val toplayer_root : Toplayer_root.t ref

val global_toplayer_root : unit -> Dom_html.element Js.t
val ensure_global_toplayer_root_mounted : unit -> unit

module For_testing : sig
  val vdom : t -> Vdom.Node.t
end
