open Js_of_ocaml
open Virtual_dom

(** Used to create event listeners with access to the element itself.  *)

val add_event_listener
  :  (#Dom_html.eventTarget as 'a) Js.t
  -> (#Dom_html.event as 'b) Js.t Dom.Event.typ
  -> ('b Js.t -> unit Ui_effect.t)
  -> Dom.event_listener_id

val create : (Dom_html.element Js.t -> Dom.event_listener_id list) -> Vdom.Attr.t
