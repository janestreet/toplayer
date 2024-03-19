open! Core
open! Js_of_ocaml

(** The Floating_positioning library is for positioning an element relative to a reference
    element. It is intended for component authors, and generally should not be used directly
    in implementations of web UIs.

    These are used to implement positioning for `lib/vdom_toplayer`.
*)

(** {2 Config Types} *)

module Position : sig
  type t =
    | Auto
    | Top
    | Bottom
    | Left
    | Right
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Alignment : sig
  type t =
    | Center
    | Start
    | End
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Offset : sig
  (** Allows controlling how far the floating element is positioned away from the anchor.
      Usually, you don't want to set cross_axis. *)
  type t =
    { main_axis : float
    ; cross_axis : float
    }
  [@@deriving sexp, sexp_grammar, equal, compare]

  (** Apply no offset. *)
  val zero : t
end

module Strategy : sig
  (** Use `Absolute` for floating elements with `position:absolute`, and `Fixed` for those
      with `position:fixed`, or that are in the browser top-layer. *)
  type t =
    | Absolute
    | Fixed
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

(** {2 Anchoring} *)

module Anchor : sig
  type t [@@deriving sexp_of]

  (** An element in the dom relative to which the floating element will be positioned. *)
  val of_element : Dom_html.element Js.t -> t

  (** [top], [bottom], and [left], [right] are the # of pixels down and right from the
      top left corner to form the (top, bottom), and (left, right) borders of the
      virtual bounding box. *)
  val of_bounding_box : top:float -> left:float -> bottom:float -> right:float -> t

  (** [x] and [y] are the # of pixels right/down from the top left corner. *)
  val of_coordinate : x:float -> y:float -> t
end

(** {2 Positioning Primitives} *)
type auto_update_handle

(** Sets the position of the floating element relative to the anchor.
    Can be useful for one-off positioning calls or virtual positioning.
    For real DOM anchors, you'll generally want to use [auto_update_position].

    [arrow_selector] can be used to indicate a child element of [floating] that should be a
    positioning indicator: https://floating-ui.com/docs/arrow. *)
val update_position
  :  ?arrow_selector:string
  -> anchor:Anchor.t
  -> floating:Dom_html.element Js.t
  -> Position.t
  -> Alignment.t
  -> Offset.t
  -> Strategy.t
  -> unit

(** Like [update_position], but automatically repositions the floating element
    when the anchor changes.

    This can be expensive, don't use it to position elements with [display:none]
    while they are not visible.

    Known bug: popovers attached to moving elements might disconnect when positioning
    params change due to some weird interactions between vdom diffing and [floating_ui]'s
    use of [IntersectionObserver]. This could be fixed by running auto-update on every
    frame, but isn't currently implemented because it's not a likely use case.
*)
val auto_update_position
  :  ?arrow_selector:string
  -> anchor:Anchor.t
  -> floating:Dom_html.element Js.t
  -> Position.t
  -> Alignment.t
  -> Offset.t
  -> Strategy.t
  -> auto_update_handle

val cancel_auto_update : auto_update_handle -> unit

(** {2 Control and positioning hooks} *)

(** [position_me] returns an attr which, when added to a vdom node, will automatically
    position it relative to the anchor, with auto-update.  *)
val position_me
  :  ?arrow_selector:string
  -> ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> Anchor.t
  -> Virtual_dom.Vdom.Attr.t

(** {2 Accessors for positioning data provided by floating_positioning} *)

module Accessors : sig
  (** These set up some "config" styles that help floating ui run more smoothly,
      and constrain the floating element to the max available width/height. *)
  val floating_styling : Virtual_dom.Vdom.Attr.t

  (** If using an arrow, it should be placed in an element that has this attribute.
      It will position and z-index the contents. *)
  val arrow_container : Virtual_dom.Vdom.Attr.t
end
