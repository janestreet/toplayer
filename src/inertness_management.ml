open! Core
open Js_of_ocaml
open Virtual_dom

let set_inert (element : Dom_html.element Js.t) =
  element##setAttribute (Js.string "inert") (Js.string "")
;;

let remove_inert (element : Dom_html.element Js.t) =
  element##removeAttribute (Js.string "inert")
;;

module Int_key : sig
  type t [@@deriving sexp, equal, compare, hash]

  val create : unit -> t
end = struct
  let global_val = ref 0

  include Int

  let create () =
    incr global_val;
    !global_val
  ;;
end

module Modal_key = struct
  include Int_key
end

module Subscriber_key = struct
  include Int_key
end

let open_modals = Hashtbl.create (module Modal_key)
let subscribers = Hashtbl.create (module Subscriber_key)

module Sequenced_id = struct
  let global_counter = ref Int63.zero

  let get_next () =
    let r = !global_counter in
    Int63.incr global_counter;
    r
  ;;
end

let subscribe f =
  let key = Subscriber_key.create () in
  Hashtbl.add_exn subscribers ~key ~data:f;
  key
;;

let unsubscribe key = Hashtbl.remove subscribers key

let on_state_change () =
  let modal_opened_most_recently_at =
    Hashtbl.fold open_modals ~init:None ~f:(fun ~key:_ ~data:timestamp -> function
      | None -> Some timestamp
      | Some acc_timestamp -> Int63.max timestamp acc_timestamp |> Some)
  in
  let update_app_root_inertness =
    match modal_opened_most_recently_at with
    | None -> remove_inert
    | Some _ -> set_inert
  in
  Incr_dom.Start_app.Private_for_toplayer_to_mutate_inertness.connected_app_roots ()
  |> List.iter ~f:update_app_root_inertness;
  Hashtbl.iter subscribers ~f:(fun f -> f ~modal_opened_most_recently_at)
;;

let open_modal () =
  let key = Modal_key.create () in
  let opened_at = Sequenced_id.get_next () in
  Hashtbl.add_exn open_modals ~key ~data:opened_at;
  on_state_change ();
  key, opened_at
;;

let close_modal key =
  Hashtbl.remove open_modals key;
  on_state_change ()
;;

let reset () =
  Hashtbl.clear open_modals;
  Hashtbl.clear subscribers;
  on_state_change ()
;;

let inertness_subscription ~element ~opened_at ~modal_opened_most_recently_at =
  match modal_opened_most_recently_at with
  | None -> remove_inert element
  | Some modal_opened_most_recently_at ->
    if Int63.(opened_at < modal_opened_most_recently_at)
    then set_inert element
    else remove_inert element
;;

module For_modal = Vdom.Attr.Hooks.Make (struct
    module State = struct
      type t =
        { modal_key : Modal_key.t
        ; subscriber_key : Subscriber_key.t
        }
    end

    module Input = struct
      type t = unit [@@deriving sexp, equal]

      let combine () () = ()
    end

    let init () element =
      let modal_key, opened_at = open_modal () in
      { State.modal_key
      ; subscriber_key = subscribe (inertness_subscription ~element ~opened_at)
      }
    ;;

    let on_mount = `Do_nothing
    let update ~old_input:_ ~new_input:_ _ _elem = ()

    let destroy () { State.modal_key; subscriber_key } _ =
      (* [unsubscribe] should run first so that we don't run a useless subscriber. *)
      unsubscribe subscriber_key;
      close_modal modal_key
    ;;
  end)

module For_popover = Vdom.Attr.Hooks.Make (struct
    module State = struct
      type t = Subscriber_key.t
    end

    module Input = struct
      type t = unit [@@deriving sexp, equal]

      let combine () () = ()
    end

    let init () element =
      subscribe (inertness_subscription ~element ~opened_at:(Sequenced_id.get_next ()))
    ;;

    let on_mount = `Do_nothing
    let update ~old_input:_ ~new_input:_ _ _elem = ()
    let destroy () subscriber_key _ = unsubscribe subscriber_key
  end)

let for_modal =
  For_modal.create () |> Vdom.Attr.create_hook "vdom_toplayer_modal_inertness"
;;

let for_popover =
  For_popover.create () |> Vdom.Attr.create_hook "vdom_toplayer_popover_inertness"
;;
