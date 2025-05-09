open Core
open Js_of_ocaml

module Js = struct
  type 'a t = 'a Js.t

  (* [_ Js.t] is [Ojs.t]. *)

  let t_to_js : ('a -> Gen_js_api.Ojs.t) -> 'a Js.t -> Gen_js_api.Ojs.t =
    fun _ -> Obj.magic
  ;;

  let t_of_js : (Gen_js_api.Ojs.t -> 'a) -> Gen_js_api.Ojs.t -> 'a Js.t =
    fun _ -> Obj.magic
  ;;
end

module Dom_html = struct
  type element = Js_of_ocaml.Dom_html.element

  (* [element] should only be converted when it's a type parameter of [Js.t]. *)

  let element_to_js _ = failwith "BUG"
  let element_of_js _ = failwith "BUG"
end
