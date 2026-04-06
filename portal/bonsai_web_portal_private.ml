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

let apply_patch' =
  match Am_running_how_js.am_in_browser_like_api with
  | true -> apply_patch_for_browser
  | false -> apply_patch_for_test
;;

let apply_patch t vdom =
  let r = apply_patch' t vdom in
  r
;;

let create_for_browser ~parent vdom =
  let portal =
    let element = Dom_html.createDiv Dom_html.document in
    Dom.appendChild parent element;
    { parent; element; vdom = Vdom.Node.div [] }
  in
  apply_patch' portal vdom
;;

let create_for_test ~parent vdom =
  (* No use trying to create a DOM element in tests. *)
  { parent; element = parent; vdom }
;;

let create =
  match Am_running_how_js.am_in_browser_like_api with
  | true -> create_for_browser
  | false -> create_for_test
;;

let destroy_for_browser portal =
  Dom.removeChild portal.parent portal.element;
  (* After removing the portal from the dom, we still need to apply a patch that removes
     any existing elements from [contents], so that the apppropriate hooks are triggered
     by [Vdom].

     We could do this before removing the portal as well, but this way we get to visually
     remove the portal immediately before computing the patch.

     The use of [none_deprecated] is correct here, because we want to remove the element,
     not replace it with a new one. *)
  apply_patch portal (Vdom.Node.none_deprecated [@alert "-deprecated"])
  |> (ignore : t -> unit)
;;

let destroy_for_tests _ = ()

let destroy =
  match Am_running_how_js.am_in_browser_like_api with
  | true -> destroy_for_browser
  | false -> destroy_for_tests
;;

module Global_root = struct
  (* This class is here mostly for documentation: if you inspect element a Bonsai app, it
     explains why there's an extra div under the document root. The random string at the
     end is part of a UUID, and is intended to discourage people from using this class for
     styling.

     It's also used in tests to ensure that we can restore a toplayer root after clearing
     the dom. *)
  let global_root_class = "toplayer_portal_root_aa63f6b8d3b4"

  let create_global_toplayer_root () =
    let elt = Dom_html.document##createElement (Js.string "div") in
    elt##setAttribute (Js.string "class") (Js.string global_root_class);
    let (_ : Dom.node Js.t) =
      Dom_html.document##.documentElement##appendChild (elt :> Dom.node Js.t)
    in
    elt
  ;;

  let singleton_global_toplayer_root = lazy (create_global_toplayer_root ())

  let global_toplayer_root () =
    if am_running_test
    then (
      let existing_root =
        Dom_html.document##querySelector (Js.string [%string ".%{global_root_class}"])
        |> Js.Opt.to_option
      in
      match existing_root with
      | Some root -> root
      | None -> create_global_toplayer_root ())
    else force singleton_global_toplayer_root
  ;;

  let global_toplayer_root () =
    if Am_running_how_js.am_in_browser_like_api
    then global_toplayer_root ()
    else Js_of_ocaml.Js.Unsafe.obj [||]
  ;;

  let ensure_global_toplayer_root_mounted () =
    match Am_running_how_js.am_running_how with
    | `Browser | `Browser_benchmark | `Browser_test | `Node_jsdom_test ->
      let root = global_toplayer_root () in
      ignore (root : Dom_html.element Js.t)
    | `Node_benchmark | `Node_test | `Node -> ()
  ;;
end

include Global_root

module For_testing = struct
  let vdom t = t.vdom
end
