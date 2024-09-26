open! Core
open Virtual_dom
open Js_of_ocaml

type t =
  { parent : Dom_html.element Js.t
  ; element : Dom_html.element Js.t
  ; vdom : Vdom.Node.t
  }
[@@deriving fields ~getters]

let apply_patch_for_browser portal vdom =
  match phys_equal portal.vdom vdom with
  | true -> portal
  | false ->
    let patch = Vdom.Node.Patch.create ~previous:portal.vdom ~current:vdom in
    (match Vdom.Node.Patch.is_empty patch with
     | true -> portal
     | false -> { portal with element = Vdom.Node.Patch.apply patch portal.element; vdom })
;;

let apply_patch_for_test portal vdom =
  match phys_equal portal.vdom vdom with
  | true -> portal
  | false -> { portal with vdom }
;;

let apply_patch =
  match Am_running_how_js.am_in_browser with
  | true -> apply_patch_for_browser
  | false -> apply_patch_for_test
;;

let create_for_browser parent vdom =
  let portal =
    let element = Dom_html.createDiv Dom_html.document in
    Dom.appendChild parent element;
    { parent; element; vdom = Vdom.Node.div [] }
  in
  apply_patch portal vdom
;;

let create_for_test parent vdom =
  (* No use trying to create a DOM element in tests. *)
  { parent; element = parent; vdom }
;;

let create =
  match Am_running_how_js.am_in_browser with
  | true -> create_for_browser
  | false -> create_for_test
;;

let destroy_for_browser portal =
  Dom.removeChild portal.parent portal.element;
  (* After removing the portal from the dom, we still need to apply a patch that removes
     any existing elements from [contents], so that the apppropriate hooks are triggered
     by [Vdom].

     We could do this before removing the portal as well, but this way we get to
     visually remove the portal immediately before computing the patch.

     The use of [none_deprecated] is correct here, because we want to remove the
     element, not replace it with a new one. *)
  apply_patch portal (Vdom.Node.none_deprecated [@alert "-deprecated"])
  |> (ignore : t -> unit)
;;

let destroy_for_tests _ = ()

let destroy =
  match Am_running_how_js.am_in_browser with
  | true -> destroy_for_browser
  | false -> destroy_for_tests
;;

module For_popovers = struct
  let nested_popover_root_const =
    "nested-popover-root-priv-2ecfd118-f7b7-11ee-abec-aa63f6b8d3b4"
  ;;

  let nestable_popover_const = "data-bonsai-popover-356c4f74-f7b7-11ee-8823-aa63f6b8d3b4"

  let find_nearest_popover_ancestor (element : Dom_html.element Js.t) =
    element##closest (Js.string [%string "[%{nestable_popover_const}]"])
    |> Js.Opt.to_option
  ;;

  let find_popover_portal_root (anchor : Dom_html.element Js.t) =
    let (root : Dom_html.element Js.t option) =
      let%bind.Option popover_ancestor = find_nearest_popover_ancestor anchor in
      Js.Unsafe.get popover_ancestor "lastElementChild" |> Js.Opt.to_option
    in
    match root with
    | Some node -> node
    | None -> Dom_html.document##.documentElement
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

module For_testing = struct
  let vdom t = t.vdom
end
