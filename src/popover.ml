open! Core
open Virtual_dom
open Floating_positioning_new

module Show_on_mount = Vdom.Attr.Hooks.Make (struct
  module State = Unit

  module Input = struct
    type t = unit [@@deriving sexp, equal]

    let combine () () = ()
  end

  let init () _elem = ()
  let on_mount () () elem = Popover_dom.show_popover elem
  let on_mount = `Schedule_animation_frame on_mount
  let update ~old_input:() ~new_input:() () _elem = ()
  let destroy () () _ = ()
end)

let show_on_mount =
  Show_on_mount.create () |> Vdom.Attr.create_hook "vdom_toplayer_show_on_mount"
;;

module Popover_attr = struct
  module Impl = struct
    module Input = struct
      module For_one = struct
        type t =
          { content : Vdom_with_phys_equal.t
          ; arrow : Vdom_with_phys_equal.t option
          ; position : Position.t
          ; alignment : Alignment.t
          ; offset : Offset.t
          }
        [@@deriving sexp_of, equal]

        let equal a b = phys_equal a b || equal a b
      end

      type t = For_one.t list [@@deriving sexp_of, equal]

      let combine a b = a @ b
    end

    module State = struct
      module For_one = struct
        type t =
          { portal : Portal.t
          ; input : Input.For_one.t
          }
      end

      type t = For_one.t list ref
    end

    let wrap_content { Input.For_one.position; alignment; offset; content; arrow } ~anchor
      =
      let position_attr =
        Floating_positioning_new.position_me
          ~arrow_selector:Popover_dom.arrow_selector
          ~position
          ~alignment
          ~offset
          (Floating_positioning_new.Anchor.of_element anchor)
      in
      Vdom.Node.div
        ~attrs:[ Popover_dom.attrs `Manual; position_attr; show_on_mount ]
        [ content; Option.value_map arrow ~f:Popover_dom.arrow ~default:Vdom.Node.none ]
    ;;

    let create_one (input : Input.For_one.t) ~anchor =
      let portal = Portal.create (wrap_content input ~anchor) in
      { State.For_one.portal; input }
    ;;

    let update_one input (state : State.For_one.t) ~anchor =
      match Input.For_one.equal input state.input with
      | true -> state
      | false ->
        Portal.apply_patch state.portal (wrap_content input ~anchor);
        { state with input }
    ;;

    let destroy_one { State.For_one.portal; input = _ } = Portal.destroy portal
    let init _ _ = ref []

    let on_mount all_inputs state_ref anchor =
      let state = List.map all_inputs ~f:(create_one ~anchor) in
      state_ref := state
    ;;

    let on_mount = `Schedule_animation_frame on_mount

    let update ~old_input ~(new_input : Input.t) (state_ref : State.t) anchor =
      match phys_equal old_input new_input with
      | true -> ()
      | false ->
        let zipped, remainder = List.zip_with_remainder new_input !state_ref in
        let updated_state =
          List.map zipped ~f:(fun (input, state) -> update_one input state ~anchor)
        in
        let state_from_remainder =
          match remainder with
          | None -> []
          | Some (Second old_states) ->
            List.iter old_states ~f:destroy_one;
            []
          | Some (First new_inputs) -> List.map new_inputs ~f:(create_one ~anchor)
        in
        state_ref := updated_state @ state_from_remainder
    ;;

    let destroy _ (state : State.t) _ = List.iter !state ~f:destroy_one
  end

  include Impl
  include Vdom.Attr.Hooks.Make (Impl)
end

let attr
  ?(position = Position.Auto)
  ?(alignment = Alignment.Center)
  ?(offset = Offset.zero)
  ?arrow
  content
  =
  Popover_attr.create [ { position; alignment; offset; content; arrow } ]
  |> Vdom.Attr.create_hook [%string "vdom_toplayer"]
;;

let node
  ?(position = Position.Auto)
  ?(alignment = Alignment.Center)
  ?(offset = Offset.zero)
  ?arrow
  ~popover_content
  anchor
  =
  Vdom.Node.div
    ~attrs:
      [ Popover_dom.attrs `Manual
      ; show_on_mount
      ; position_me
          ~arrow_selector:Popover_dom.arrow_selector
          ~position
          ~alignment
          ~offset
          anchor
      ]
    [ popover_content
    ; Option.value_map arrow ~f:Popover_dom.arrow ~default:Vdom.Node.none
    ]
;;
