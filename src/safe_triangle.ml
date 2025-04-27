open! Core
open! Virtual_dom
open Js_of_ocaml
module Portal = Byo_portal_private

(* Enable this to visually render the triangle for debugging. *)
let show_triangle_for_debugging = false

let debug_colors =
  if show_triangle_for_debugging
  then Vdom.Attr.string_property "fill" "rgb(255 0 0 / 0.3)"
  else Vdom.Attr.string_property "fill" "rgb(0 0 0 / 0.0)"
;;

module Hook = struct
  module Impl = struct
    module State = struct
      type t =
        { mutable submenu_id : string
        ; portal : Portal.t ref
        ; mouse_move_handler : Dom_html.event_listener_id
        }
    end

    module Input = struct
      type t = string [@@deriving sexp_of]

      let combine _ _ = failwith "BUG: Can't combine multiple safe triangles"
    end

    let draw
      ~destroy
      ~(event : Dom_html.mouseEvent Js.t)
      ~(submenu : Dom_html.element Js.t)
      =
      let client_rect = submenu##getBoundingClientRect in
      let num_to_int number = Float.to_int (Js.to_float number) in
      let child_menu_left_of_pointer =
        num_to_int client_rect##.left > num_to_int event##.clientX
      in
      let svg_width =
        if child_menu_left_of_pointer
        then num_to_int client_rect##.left - num_to_int event##.clientX
        else num_to_int event##.clientX - num_to_int client_rect##.right
      in
      let svg_height = num_to_int client_rect##.height in
      let svg_left =
        if child_menu_left_of_pointer
        then num_to_int event##.clientX
        else num_to_int event##.clientX - svg_width
      in
      (* Position of the pointer in the SVG. *)
      let pointer_edge_x = if child_menu_left_of_pointer then 0 else svg_width in
      let pointer_edge_y = num_to_int event##.clientY - num_to_int client_rect##.top in
      (* Position of the child menu in the SVG. *)
      let opposite_edge_x = if child_menu_left_of_pointer then svg_width else 0 in
      Vdom.Node.create_svg
        "svg"
        ~attrs:
          [ Css_gen.(
              position `Fixed
              @> width (`Px svg_width)
              @> height (`Px svg_height)
              @> z_index 9999
              @> top (`Px (num_to_int client_rect##.top))
              @> left (`Px svg_left)
              @> create ~field:"pointer-events" ~value:"none")
            |> Vdom.Attr.style
          ; Vdom.Attr.on_contextmenu (const Vdom.Effect.Prevent_default)
          ]
        [ Vdom.Node.create_svg
            "path"
            ~attrs:
              [ debug_colors
              ; Vdom.Attr.style (Css_gen.create ~field:"pointer-events" ~value:"auto")
              ; Vdom.Attr.on_mouseout (fun (_ : Dom_html.mouseEvent Js.t) -> destroy)
              ; Vdom.Attr.string_property
                  "d"
                  [%string
                    "M %{pointer_edge_x#Int} %{pointer_edge_y#Int} L \
                     %{opposite_edge_x#Int} %{svg_height#Int} L %{opposite_edge_x#Int} 0 \
                     z"]
              ]
            []
        ]
    ;;

    let init submenu_id elem =
      let portal =
        ref
          (Portal.create
             ~parent:(Popover_dom.find_popover_portal_root elem)
             Vdom.Node.none)
      in
      let destroy () = portal := Portal.apply_patch !portal Vdom.Node.none in
      let mouse_move_handler =
        Dom_html.addEventListener
          elem
          Dom_html.Event.mousemove
          (Dom.handler (fun event ->
             Js.Opt.case
               (Dom_html.document##getElementById (Js.string submenu_id))
               (fun () -> destroy ())
               (fun submenu ->
                 let destroy = Vdom.Effect.of_thunk destroy in
                 portal := Portal.apply_patch !portal (draw ~destroy ~event ~submenu));
             Js.bool true))
          (Js.bool false)
      in
      { State.submenu_id; portal; mouse_move_handler }
    ;;

    let on_mount = `Do_nothing
    let update ~old_input:_ ~new_input (state : State.t) _ = state.submenu_id <- new_input

    let destroy _ { State.portal; mouse_move_handler; _ } _ =
      Dom_html.removeEventListener mouse_move_handler;
      Portal.destroy !portal
    ;;
  end

  include Vdom.Attr.Hooks.Make (Impl)
end

let attr ~submenu_id =
  Vdom.Attr.create_hook "skyline-context-menu-safe-triangle" (Hook.create submenu_id)
;;
