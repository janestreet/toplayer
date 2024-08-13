open! Core
open Virtual_dom
open Js_of_ocaml

type t

val apply_patch : t -> Vdom.Node.t -> t
val create : Dom_html.element Js.t -> Vdom.Node.t -> t
val destroy : t -> unit
val element : t -> Dom_html.element Js.t

module For_popovers : sig
  val find_nearest_popover_ancestor
    :  Dom_html.element Js.t
    -> Dom_html.element Js.t option

  val find_popover_portal_root : Dom_html.element Js.t -> Dom_html.element Js.t

  (** [nestable_popover_attr] should be set on the <div popover=... /> DOM element of
      Bonsai tooltips/popovers, under which other popovers might be nested.

      [nested_popover_root] must be the last child of any DOM element that has
      [nestable_popover_attr].

      This is fragile (for us maintainers), but gives us performance wins. *)
  val nestable_popover_attr : Vdom.Attr.t

  (** [nested_popover_root] should be included at the top-level of a given popovers
      contents such that it can be used as the portal root for any child popover
      elements. *)
  val nested_popover_root : Vdom.Node.t
end

module For_testing : sig
  val vdom : t -> Vdom.Node.t
end
