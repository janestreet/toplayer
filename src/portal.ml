open! Core
open Virtual_dom
open Js_of_ocaml

type t =
  { parent : Dom_html.element Js.t
  ; mutable element : Dom_html.element Js.t
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

let create parent vdom =
  let portal =
    let element = Dom_html.createDiv Dom_html.document in
    Dom.appendChild parent element;
    { parent; element; vdom = Vdom.Node.div [] }
  in
  apply_patch portal vdom;
  portal
;;

let destroy portal =
  Dom.removeChild portal.parent portal.element;
  (* After removing the portal from the dom, we still need to apply a patch that removes
     any existing elements from [contents], so that the apppropriate hooks are triggered
     by [Vdom].

     We could do this before removing the portal as well, but this way we get to
     visually remove the portal immediately before computing the patch. *)
  apply_patch portal Vdom.Node.none
;;

module For_popovers = struct
  let with_hash ~__LOC__:loc txt =
    let hash = Md5.digest_string (txt ^ loc) |> Md5.to_hex in
    [%string "%{txt}-%{hash}"]
  ;;

  let nested_popover_root_const = with_hash ~__LOC__ "nested-popover-root-priv"
  let nestable_popover_const = with_hash ~__LOC__ "data-bonsai-popover"

  let find_popover_portal_root (anchor : Dom_html.element Js.t) =
    let (root : Dom_html.element Js.t option) =
      let%bind.Option popover_ancestor =
        anchor##closest (Js.string [%string "[%{nestable_popover_const}]"])
        |> Js.Opt.to_option
      in
      Js.Unsafe.get popover_ancestor "lastElementChild" |> Js.Opt.to_option
    in
    match root with
    | Some node -> node
    | None -> Dom_html.document##.body
  ;;

  let portal_root class_ =
    let id = Type_equal.Id.create ~name:class_ Sexplib.Conv.sexp_of_opaque in
    let init () =
      let div = Dom_html.createDiv Dom_html.document in
      div##setAttribute (Js.string "class") (Js.string class_);
      div##setAttribute (Js.string "style") (Js.string "display: contents");
      (), (div :> Dom_html.element Js.t)
    in
    Vdom.Node.widget ~id ~init ()
  ;;

  let nestable_popover_attr = Vdom.Attr.create nestable_popover_const ""
  let nested_popover_root = portal_root nested_popover_root_const
end
