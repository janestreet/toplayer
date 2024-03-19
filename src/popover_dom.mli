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

(** Creates attrs that indicate an HTML element to be a popover.
    In particular, we:
    - Add a `popover` attribute, with either `manual` or `auto` value
    - Unset browser styles
    - Set tabindex = -1
*)
val attrs : [ `Auto | `Manual ] -> Vdom.Attr.t

(** Wraps a vdom node in a div that assigns positioning to be a floating ui arrow. *)
val arrow : Vdom.Node.t -> Vdom.Node.t

val arrow_selector : string
