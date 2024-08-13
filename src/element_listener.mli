open Js_of_ocaml
open Virtual_dom

(** A lower-level API for creating event listeners with access to the element they are
    attached to. Not easily testable.  *)

val add_event_listener
  :  (#Dom_html.eventTarget as 'a) Js.t
  -> (#Dom_html.event as 'b) Js.t Dom.Event.typ
  -> ('b Js.t -> unit Ui_effect.t)
  -> Dom.event_listener_id

val create : (Dom_html.element Js.t -> Dom.event_listener_id list) -> Vdom.Attr.t
