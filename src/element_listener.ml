open Js_of_ocaml
open Virtual_dom

let add_event_listener element event f =
  Dom_html.addEventListenerWithOptions
    element
    event
    ~passive:Js._true
    (Dom_html.handler (fun ev ->
       Vdom.Effect.Expert.handle ev (f ev);
       Js._true))
;;

include Vdom.Attr.Hooks.Make (struct
  module Input = struct
    type t = Dom_html.element Js.t -> Dom_html.event_listener_id list [@@deriving sexp_of]

    let combine f g element = f element @ g element
  end

  module State = struct
    type t = { mutable listeners : Dom_html.event_listener_id list }
  end

  let init f element = { State.listeners = f element }

  let destroy _input { State.listeners } _element =
    List.iter Dom_html.removeEventListener listeners
  ;;

  let update ~old_input ~new_input:f (state : State.t) element =
    (* if the callback function changes, cancel the old one and re-install *)
    destroy old_input state element;
    state.listeners <- f element
  ;;

  let on_mount = `Do_nothing
end)

let create f = create f |> Vdom.Attr.create_hook "toplayer_private_listener"
