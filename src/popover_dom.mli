(** A collection of low-level utils for working with <... popover=... /> DOM elements. *)

open! Core
open Js_of_ocaml
open Virtual_dom

val show_popover : Dom_html.element Js.t -> unit
val hide_popover : Dom_html.element Js.t -> unit
val toggle_popover : Dom_html.element Js.t -> unit
val is_hovered : Dom_html.element Js.t -> bool
val is_popover : Dom_html.element Js.t -> bool
val is_open : Dom_html.element Js.t -> bool

(** We wrap the content in a div that:
    - Adds a `popover` attribute, with either `manual` or `auto` value
    - Unsets browser styles
    - Sets tabindex = -1
    - Constrains the popover's max-height and max-width to the maximum available height/width
    - Applies some "default" max-width and placement positioning so that floating_ui
      can start from a clean slate
    - Adds in the arrow element, as well as a container widget for nested popovers *)
val node
  :  ?arrow:Vdom.Node.t
  -> kind:[< `Auto | `Manual ]
  -> extra_attrs:Vdom.Attr.t list
  -> Vdom.Node.t
  -> Vdom.Node.t

val arrow_selector : string

(** Listens for "toggle" events on the popover it is attached to. Every open,
    if nothing is already focused inside the popover, will focus the popover root. *)
val focus_popover_on_open : Vdom.Attr.t
