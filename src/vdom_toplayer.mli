open! Core
open Virtual_dom
module Position := Floating_positioning_new.Position
module Alignment := Floating_positioning_new.Alignment
module Offset := Floating_positioning_new.Offset
module Anchor := Floating_positioning_new.Anchor
module Match_anchor_side := Floating_positioning_new.Match_anchor_side

(** This library contains vdom utils for creating and positioning popovers, tooltips, and
    modals.

    It is intended for authors of UI component libraries. You'll probably want to set some
    custom atts for borders/padding/style, possibly provide an `arrow` element, and offer
    a slightly simpler API for users of your UI component library.

    [popover] is intended to be used through the [bonsai_web_ui_toplayer] library.

    Note that the DOM for all popovers will be placed outside of the app root.
    If you want global event listeners (including keyboard shortcuts) to work inside
    popovers, they should be set via [Vdom.Attr.Global_listeners].
    Similarly, global styles should be set using the [:root] pseudo-class, and
    [Inline_css.Private.Dynamic.attr]. *)

(** The optional [arrow] argument allows automatically positioning an "arrow" element to
    point towards the center of the floating element's anchor.

    https://floating-ui.com/docs/arrow

    Arrows will be automatically placed along the correct edge of the floating element,
    and rotated so that the "top" of the provided arrow points towards the anchor. *)

(** Returns an attr which, when attached to a Vdom node, will create a tooltip attached
    to that node. The tooltip will open/close on hover in/out, subject to [show_delay]
    and [hide_grace_period]. If [hoverable_inside] is set, the tooltip will not disappear
    if the cursor is moved inside it within [hide_grace_period]. *)
val tooltip
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

(** Returns an attr which, when attached to a Vdom node, will create a popover positioned
    relative to that node. The popover's visibility should be controlled by attaching
    / detaching the attr to/from the anchor; the browser-level `showPopover()` and
    `hidePopover()` functions should not be used. This allows us to own the state
    controlling popover visibility. *)
val popover
  :  ?popover_attrs:Vdom.Attr.t list
  -> ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?match_anchor_side_length:Match_anchor_side.t
  -> ?arrow:Vdom.Node.t
  -> Vdom.Node.t
  -> Vdom.Attr.t

module For_use_in_portals : sig
  (** The implementations of these functions assume that their DOM is placed outside of
      the app root. If placed inside the app root, [modal] **will always** be inert,
      and [popover_custom] will be inert when any modal is open, even it is on top.

      Do not place these directly into your app's vdom!  *)

  (** Returns an vdom node which will position an always-open DOM popover element relative
      to some anchor, which could e.g. be constructed from some Dom element, or a virtual
      bounding box. *)
  val popover_custom
    :  ?popover_attrs:Vdom.Attr.t list
    -> ?position:Position.t
    -> ?alignment:Alignment.t
    -> ?offset:Offset.t
    -> ?match_anchor_side_length:Match_anchor_side.t
    -> ?arrow:Vdom.Node.t
    -> popover_content:Vdom.Node.t
    -> Anchor.t
    -> Vdom.Node.t

  (** Returns a popover that makes everything under it inert.
      The implementation assumes that the modal itself is outside of the app root.
      If this is placed inside the app root, it will make the whole document (including
      itself) inert, effectively disabling the entire page. *)
  val modal
    :  ?modal_attrs:Vdom.Attr.t list
    -> ?lock_body_scroll:bool
    -> Vdom.Node.t
    -> Vdom.Node.t
end

module For_bonsai_web_ui_toplayer : sig
  open Js_of_ocaml

  (** Listens for "toggle" events on the popover it is attached to. Every open,
    if nothing is already focused inside the popover, will focus the popover root. *)
  val focus_popover_on_open : Vdom.Attr.t

  val find_nearest_popover_ancestor
    :  Dom_html.element Js.t
    -> Dom_html.element Js.t option

  module Portal : sig
    type t

    val apply_patch : t -> Vdom.Node.t -> t
    val create : Dom_html.element Js.t -> Vdom.Node.t -> t
    val destroy : t -> unit
    val element : t -> Dom_html.element Js.t

    module For_testing : sig
      val vdom : t -> Vdom.Node.t
    end
  end
end

module For_testing_popover_hook : sig
  type for_one =
    { content : Vdom.Node.t
    ; popover_attrs : Vdom.Attr.t list
    ; arrow : Vdom.Node.t option
    ; position : Position.t
    ; alignment : Alignment.t
    ; offset : Offset.t
    ; match_anchor_side_length : Match_anchor_side.t option
    }
  [@@deriving sexp_of]

  type t = for_one list [@@deriving sexp_of]

  val type_id : t Type_equal.Id.t
  val hook_name : string
end

module For_testing_bonsai_web_ui_toplayer : sig
  include module type of Popover.For_testing_bonsai_web_ui_toplayer
  include module type of Modal.For_testing_bonsai_web_ui_toplayer
end

module For_debugging_frame_delay : sig
  val mark_events : bool ref
end
