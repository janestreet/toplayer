open! Core
open Virtual_dom
module Position := Floating_positioning_new.Position
module Alignment := Floating_positioning_new.Alignment
module Offset := Floating_positioning_new.Offset
module Anchor := Floating_positioning_new.Anchor

(** This library contains vdom utils for creating and positioning popovers, tooltips, and
    modals.

    It is intended for authors of UI component libraries. You'll probably want to set some
    custom atts for borders/padding/style, possibly provide an `arrow` element, and offer
    a slightly simpler API for users of your UI component library.

    [popover] is intended to be used through the [bonsai_web_ui_popover] library.

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
  :  ?position:Position.t
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
  :  ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?arrow:Vdom.Node.t
  -> Vdom.Node.t
  -> Vdom.Attr.t

(** Returns an vdom node which will position an always-open DOM popover element relative
    to some anchor, which could e.g. be constructed from some Dom element, or a virtual
    bounding box. *)
val popover_custom
  :  ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?arrow:Vdom.Node.t
  -> popover_content:Vdom.Node.t
  -> Anchor.t
  -> Vdom.Node.t
