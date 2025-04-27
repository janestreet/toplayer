open! Core
open Virtual_dom
module Position := Floating_positioning_new.Position
module Alignment := Floating_positioning_new.Alignment
module Offset := Floating_positioning_new.Offset
module Match_anchor_side := Floating_positioning_new.Match_anchor_side

(** This library contains vdom utils for creating and positioning popovers, tooltips, and
    modals.

    It is intended for authors of UI component libraries. You'll probably want to set some
    custom atts for borders/padding/style, possibly provide an `arrow` element, and offer
    a slightly simpler API for users of your UI component library.

    [popover] is intended to be used through the [bonsai_web_ui_toplayer] library.

    Note that the DOM for all popovers will be placed outside of the app root. If you want
    global event listeners (including keyboard shortcuts) to work inside popovers, they
    should be set via [Vdom.Attr.Global_listeners]. Similarly, global styles should be set
    using the [:root] pseudo-class, and [Inline_css.Private.Dynamic.attr]. *)

(** The optional [arrow] argument allows automatically positioning an "arrow" element to
    point towards the center of the floating element's anchor.

    https://floating-ui.com/docs/arrow

    Arrows will be automatically placed along the correct edge of the floating element,
    and rotated so that the "top" of the provided arrow points towards the anchor. *)

(** Returns an attr which, when attached to a Vdom node, will create a tooltip attached to
    that node. The tooltip will open/close on hover in/out, subject to [show_delay] and
    [hide_grace_period]. If [hoverable_inside] is set, the tooltip will not disappear if
    the cursor is moved inside it within [hide_grace_period]. If [light_dismiss] (default:
    true), the tooltip will close on click outside, escape, or if other tooltips with
    [light_dismiss:true] open. *)
val tooltip
  :  ?tooltip_attrs:Vdom.Attr.t list
  -> ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?show_delay:Time_ns.Span.t
  -> ?hide_grace_period:Time_ns.Span.t
  -> ?hoverable_inside:bool
  -> ?light_dismiss:bool
  -> ?arrow:Vdom.Node.t
  -> Vdom.Node.t
  -> Vdom.Attr.t

(** Returns an attr which, when attached to a Vdom node, will create a popover positioned
    relative to that node. The popover's visibility should be controlled by attaching /
    detaching the attr to/from the anchor; the browser-level `showPopover()` and
    `hidePopover()` functions should not be used. This allows us to own the state
    controlling popover visibility. *)
val popover
  :  ?popover_attrs:Vdom.Attr.t list
  -> ?position:Position.t
  -> ?alignment:Alignment.t
  -> ?offset:Offset.t
  -> ?match_anchor_side_length:Match_anchor_side.t
  -> ?restore_focus_on_close:bool
  -> overflow_auto_wrapper:bool
  -> ?arrow:Vdom.Node.t
  -> Vdom.Node.t
  -> Vdom.Attr.t

module For_bonsai_web_ui_toplayer : sig
  (** The implementations of [popover_custom] and [modal] assume that their DOM is placed
      outside of the app root. If placed inside the app root, [modal] **will always** be
      inert, and [popover_custom] will be inert when any modal is open, even it is on top.

      DO NOT USE THESE DIRECTLY IN YOUR APP! *)
  open Js_of_ocaml

  (** Wraps the input in a popover. Will not open or position the popover. *)
  val popover_custom
    :  ?popover_attrs:Vdom.Attr.t list
    -> ?restore_focus_on_close:bool
    -> overflow_auto_wrapper:bool
    -> ?arrow:Vdom.Node.t
    -> popover_content:Vdom.Node.t
    -> unit
    -> Vdom.Node.t

  (** Returns a popover that makes everything under it inert. The implementation assumes
      that the modal itself is outside of the app root. If this is placed inside the app
      root, it will make the whole document (including itself) inert, effectively
      disabling the entire page. *)
  val modal
    :  ?modal_attrs:Vdom.Attr.t list
    -> ?lock_body_scroll:bool
    -> ?restore_focus_on_close:bool
    -> overflow_auto_wrapper:bool
    -> Vdom.Node.t
    -> Vdom.Node.t

  (** Listens for "toggle" events on the popover it is attached to. Every open, if nothing
      is already focused inside the popover, will focus the popover root. *)
  val focus_popover_on_open : Vdom.Attr.t

  val show_on_mount : Vdom.Attr.t
  val show_popover : Dom_html.element Js.t -> unit
  val arrow_selector : string

  val find_nearest_popover_ancestor
    :  Dom_html.element Js.t
    -> Dom_html.element Js.t option
end

module For_byo_menu : sig
  val safe_triangle : submenu_id:string -> Vdom.Attr.t
end

module For_testing_popover_hook : sig
  include module type of Popover.For_testing_popover_hook
end

module For_testing_tooltip_hook : sig
  include module type of Tooltip.For_testing_tooltip_hook
end

module For_testing_bonsai_web_ui_toplayer : sig
  include module type of Popover.For_testing_bonsai_web_ui_toplayer
  include module type of Modal.For_testing_bonsai_web_ui_toplayer
end

module For_jsdom_tests : sig
  val reset_inertness : unit -> unit
end
