open! Core
open Virtual_dom
open Floating_positioning_new
module Portal = Byo_portal_private

module Popover_attr = struct
  module Impl = struct
    module Input = struct
      module For_one = struct
        type t =
          { content : Vdom_with_phys_equal.Node.t
          ; popover_attrs : Vdom_with_phys_equal.Attr.t list
          ; arrow : Vdom_with_phys_equal.Node.t option
          ; restore_focus_on_close : bool
          ; overflow_auto_wrapper : bool
          ; position : Position.t
          ; alignment : Alignment.t
          ; offset : Offset.t
          ; match_anchor_side_length : Match_anchor_side.t option
          }
        [@@deriving equal]

        let equal a b = phys_equal a b || equal a b
      end

      type t = For_one.t list [@@deriving equal]

      let combine a b = a @ b
      let sexp_of_t _ = Sexp.Atom "<omitted>"
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

    let wrap_content
      { Input.For_one.position
      ; alignment
      ; offset
      ; match_anchor_side_length
      ; content
      ; restore_focus_on_close
      ; overflow_auto_wrapper
      ; popover_attrs
      ; arrow
      }
      ~anchor
      =
      let position_attr =
        Floating_positioning_new.position_me
          ~prepare:Popover_dom.show_popover
          ~arrow_selector:Popover_dom.arrow_selector
          ~position
          ~alignment
          ~offset
          ?match_anchor_side_length
          (Floating_positioning_new.Anchor.of_element anchor)
      in
      Popover_dom.node
        ?arrow
        ~restore_focus_on_close
        ~overflow_auto_wrapper
        ~extra_attrs:(popover_attrs @ [ Inertness_management.for_popover; position_attr ])
        ~kind:`Manual
        content
    ;;

    let create_one (input : Input.For_one.t) ~anchor =
      let parent = Popover_dom.find_popover_portal_root anchor in
      let portal = Portal.create ~parent (wrap_content input ~anchor) in
      { State.For_one.portal; input }
    ;;

    let update_one input (state : State.For_one.t) ~anchor =
      match Input.For_one.equal input state.input with
      | true -> state
      | false ->
        let new_portal = Portal.apply_patch state.portal (wrap_content input ~anchor) in
        { input; portal = new_portal }
    ;;

    let destroy_one { State.For_one.portal; input = _ } = Portal.destroy portal
    let init _ _ = ref []

    let on_mount all_inputs state_ref anchor =
      let state = List.map all_inputs ~f:(create_one ~anchor) in
      state_ref := state
    ;;

    let on_mount = `Schedule_immediately_after_this_dom_patch_completes on_mount

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

let hook_name = "vdom_toplayer_popover"

let attr
  ?(popover_attrs = [])
  ?(position = Position.Auto)
  ?(alignment = Alignment.Center)
  ?(offset = Offset.zero)
  ?match_anchor_side_length
  ?(restore_focus_on_close = true)
  ~overflow_auto_wrapper
  ?arrow
  content
  =
  Popover_attr.create
    [ { position
      ; alignment
      ; offset
      ; restore_focus_on_close
      ; match_anchor_side_length
      ; content
      ; popover_attrs
      ; overflow_auto_wrapper
      ; arrow
      }
    ]
  |> Vdom.Attr.create_hook hook_name
;;

let custom
  ?(popover_attrs = [])
  ?(restore_focus_on_close = true)
  ~overflow_auto_wrapper
  ?arrow
  ~popover_content
  ()
  =
  Popover_dom.node
    ?arrow
    ~kind:`Manual
    ~restore_focus_on_close
    ~overflow_auto_wrapper
    ~extra_attrs:(popover_attrs @ [ Inertness_management.for_popover ])
    popover_content
;;

module For_testing_popover_hook = struct
  type for_one = Popover_attr.Input.For_one.t =
    { content : Vdom_with_phys_equal.Node.t
    ; popover_attrs : Vdom_with_phys_equal.Attr.t list
    ; arrow : Vdom_with_phys_equal.Node.t option
    ; restore_focus_on_close : bool
    ; overflow_auto_wrapper : bool
    ; position : Position.t
    ; alignment : Alignment.t
    ; offset : Offset.t
    ; match_anchor_side_length : Match_anchor_side.t option
    }
  [@@deriving sexp_of]

  type t = for_one list [@@deriving sexp_of]

  let type_id = Popover_attr.For_testing.type_id
  let hook_name = hook_name
end

module For_testing_bonsai_web_ui_toplayer = struct
  let wrap_anchored_popover
    ~position
    ~alignment
    ~offset
    ~match_anchor_side_length
    ~restore_focus_on_close
    ~overflow_auto_wrapper
    ~content
    ~popover_attrs
    ~arrow
    ~anchor
    =
    Popover_attr.Impl.wrap_content
      { position
      ; alignment
      ; offset
      ; match_anchor_side_length
      ; restore_focus_on_close
      ; overflow_auto_wrapper
      ; content
      ; popover_attrs
      ; arrow
      }
      ~anchor
  ;;
end
