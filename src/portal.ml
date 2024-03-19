open! Core
open Virtual_dom
open Js_of_ocaml

let root () = Dom_html.document##.body

type t =
  { mutable element : Dom_html.element Js.t
  ; mutable vdom : Vdom.Node.t
  }
[@@deriving fields ~getters]

let apply_patch portal vdom =
  let patch = Vdom.Node.Patch.create ~previous:portal.vdom ~current:vdom in
  match Vdom.Node.Patch.is_empty patch with
  | true -> ()
  | false ->
    portal.element <- Vdom.Node.Patch.apply patch portal.element;
    portal.vdom <- vdom
;;

let create vdom =
  let portal =
    let element = Dom_html.createDiv Dom_html.document in
    Dom.appendChild (root ()) element;
    { element; vdom = Vdom.Node.div [] }
  in
  apply_patch portal vdom;
  portal
;;

let destroy portal =
  Dom.removeChild (root ()) portal.element;
  (* After removing the portal from the dom, we still need to apply a patch that removes
     any existing elements from [contents], so that the apppropriate hooks are triggered
     by [Vdom].

     We could do this before removing the portal as well, but this way we get to
     visually remove the portal immediately before computing the patch. *)
  apply_patch portal Vdom.Node.none
;;
