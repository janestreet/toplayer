open! Core
open! Js_of_ocaml

(** Do not use this library directly! Use [Byo_toplayer] instead. *)

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

module Match_anchor_side : sig
  (** [Grow_to_match] will set [min-width] or [min-height]; [Match_exactly] will set
      [width] or [height], and [Shrink_to_match] will set [max-width] or [max-height].

      If not set here, max height and width will be set to the available space. *)
  type t =
    | Grow_to_match
    | Match_exactly
    | Shrink_to_match
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

(** {2 Anchoring} *)

module Anchor : sig
  type t [@@deriving sexp_of]

  (** An element in the dom relative to which the floating element will be positioned. *)
  val of_element : Dom_html.element Js.t -> t

  (** [top], [bottom], and [left], [right] are the # of pixels down and right from the top
      left corner to form the (top, bottom), and (left, right) borders of the virtual
      bounding box.

      [relative_to] defaults to [`Viewport], and determines whether the coordinates you
      provide should be in terms of "Client" or "Page" coordinate systems:
      https://developer.mozilla.org/en-US/docs/Web/CSS/CSSOM_view/Coordinate_systems *)
  val of_bounding_box
    :  relative_to:[ `Viewport | `Document ]
    -> top:float
    -> left:float
    -> bottom:float
    -> right:float
    -> t

  (** [x] and [y] are the # of pixels right/down from the top left corner.

      [relative_to] defaults to [`Viewport], and determines whether the coordinates you
      provide should be in terms of "Client" or "Page" coordinate systems:
      https://developer.mozilla.org/en-US/docs/Web/CSS/CSSOM_view/Coordinate_systems *)
  val of_coordinate : relative_to:[ `Viewport | `Document ] -> x:float -> y:float -> t
end

(** Absolutely positioned popovers start at the top left corner of the viewport before
    autopositioning kicks in. If the popover is focused via [autofocus] or
    [Effect.Focus.on_activate], and the popover has [position:absolute], and
    autopositioning has not yet completed, this focusing will scroll the page to the top.

    Because floating UI positioning is asynchronous, we always need to pre-position the
    popovers into the viewport before opening it.

    [position_within_viewport] should be called on any toplayer element that can be
    focused and is positioned via floating positioning, before it is opened. *)
val position_within_viewport : Dom_html.element Js.t -> Anchor.t -> unit

(** {2 Control and positioning hooks} *)

(** [position_me] returns an attr which, when added to a vdom node, will automatically
    position it relative to the anchor, with auto-update.

    If provided, [prepare] will run once befre positioning is applied, when the element is
    mounted. The most common use case is opening a popover before starting autopositioning
    to avoid a flicker.

    If [match_anchor_side_length] is set to true, the popover's main axis (width if
    position is Top/Bottom, height if position is Left/Right) will be set to have a length
    equal to the corresponding axis of the anchor. This is particularly useful for
    dropdowns and typeaheads. *)
val position_me
  :  ?prepare:(Dom_html.element Js.t -> unit)
  -> ?arrow_selector:string
  -> ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?match_anchor_side_length:Match_anchor_side.t
  -> Anchor.t
  -> Virtual_dom.Vdom.Attr.t

(** {2 Accessors for styles and data provided by floating_positioning} *)

module Accessors : sig
  (** These set up some "config" styles that help floating ui run more smoothly, and
      constrain the floating element to the max available width/height. *)
  val floating_styling : Virtual_dom.Vdom.Attr.t

  (** If using an arrow, it should be placed in an element that has this attribute. It
      will position and z-index the contents. *)
  val arrow_container : Virtual_dom.Vdom.Attr.t
end

module For_testing_position_me_hook : sig
  type t =
    { prepare : Dom_html.element Js.t -> unit
    ; position : Position.t
    ; alignment : Alignment.t
    ; offset : Offset.t
    ; match_anchor_side_length : Match_anchor_side.t option
    ; arrow_selector : string option
    ; anchor : Anchor.t
    }

  val type_id : t Type_equal.Id.t
  val hook_name : string
end
