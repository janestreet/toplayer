open! Core
open! Js_of_ocaml
open Virtual_dom
module Accessors = Update_position.Accessors
module Strategy = Bindings.Strategy
module Match_anchor_side = Update_position.Match_anchor_side

module Position = struct
  type t =
    | Auto
    | Top
    | Bottom
    | Left
    | Right
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Alignment = struct
  type t =
    | Center
    | Start
    | End
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Offset = struct
  include Bindings.Offset

  let zero = { main_axis = 0.; cross_axis = 0. }
end

module Anchor = struct
  type t =
    { anchor : Bindings.Reference_element.t
    ; strategy : Strategy.t
    }

  let sexp_of_t (_ : t) = Sexp.Atom "<anchor>"
  let equal : t -> t -> bool = phys_equal
  let of_element element = { anchor = `Dom element; strategy = Absolute }

  let of_function strategy get_bounding_client_rect =
    { anchor =
        `Virtual { Bindings.Reference_element.Virtual_element.get_bounding_client_rect }
    ; strategy
    }
  ;;

  let of_bounding_box ~relative_to ~top ~left ~bottom ~right =
    of_function
      (match relative_to with
       | `Viewport -> Fixed
       | `Document -> Absolute)
      (fun () ->
        (* Floating UI takes coordinates relative to the viewport. If we are positioning
           the element relative to the document, we need to subtract out the scroll
           position. *)
        let offset_x, offset_y =
          match relative_to with
          | `Viewport -> 0., 0.
          | `Document ->
            (* optdef, because scroll is not implemented in JSDom. *)
            let window
              : < scrollX : Js.number Js.t Js.optdef Js.readonly_prop
                ; scrollY : Js.number Js.t Js.optdef Js.readonly_prop >
                  Js.t
              =
              Js.Unsafe.coerce Dom_html.window
            in
            let get x =
              Js.Optdef.to_option x |> Option.value_map ~f:Js.float_of_number ~default:0.
            in
            get window##.scrollX, get window##.scrollY
        in
        let top = top -. offset_y in
        let left = left -. offset_x in
        let bottom = bottom -. offset_y in
        let right = right -. offset_x in
        { top
        ; left
        ; y = top
        ; x = left
        ; bottom
        ; right
        ; width = Float.(right - left)
        ; height = Float.(bottom - top)
        })
  ;;

  let of_coordinate ~relative_to ~x ~y =
    of_bounding_box ~relative_to ~top:y ~bottom:y ~left:x ~right:x
  ;;
end

type auto_update_handle = Bindings.Auto_update_handle.t

let update_position
  ?arrow_selector
  ~anchor
  ~floating
  ~match_anchor_side_length
  position
  alignment
  offset
  strategy
  =
  let side =
    match position with
    | Position.Auto -> None
    | Top -> Some Update_position.Side.Top
    | Bottom -> Some Bottom
    | Left -> Some Left
    | Right -> Some Right
  in
  let alignment =
    match alignment with
    | Alignment.Center -> None
    | Start -> Some Bindings.Alignment.Start
    | End -> Some End
  in
  Update_position.single_update
    ~anchor
    ~floating
    ~match_anchor_side_length
    ~arrow_selector
    side
    alignment
    offset
    strategy
;;

let position_within_viewport element { Anchor.strategy; _ } =
  Update_position.position_within_viewport element strategy
;;

let auto_update_position
  ?arrow_selector
  ~anchor:{ Anchor.anchor; strategy }
  ~floating
  ~match_anchor_side_length
  position
  alignment
  offset
  =
  Bindings.Auto_update_handle.create ~anchor ~floating ~update:(fun () ->
    update_position
      ?arrow_selector
      ~anchor
      ~floating
      ~match_anchor_side_length
      position
      alignment
      offset
      strategy)
;;

let cancel_auto_update = Bindings.Auto_update_handle.cleanup

module Position_element = struct
  module Impl = struct
    module State = struct
      type t = auto_update_handle option ref
    end

    module Input = struct
      type t =
        { (* This runs only once, so we don't need to check if it changed for updates. *)
          prepare : (Dom_html.element Js.t -> unit[@equal.ignore])
        ; position : Position.t
        ; alignment : Alignment.t
        ; offset : Offset.t
        ; match_anchor_side_length : Match_anchor_side.t option
        ; arrow_selector : string option
        ; anchor : Anchor.t
        }
      [@@deriving sexp_of, equal]

      let combine _ _ =
        failwith "An element may not be positioned relative to 2 different anchors"
      ;;
    end

    let init _ _ = ref None

    let auto_update_position
      { Input.position
      ; alignment
      ; offset
      ; match_anchor_side_length
      ; arrow_selector
      ; anchor
      ; _
      }
      element
      =
      auto_update_position
        ?arrow_selector
        ~anchor
        ~floating:element
        ~match_anchor_side_length
        position
        alignment
        offset
    ;;

    let on_mount (input : Input.t) handle element =
      position_within_viewport element input.anchor;
      input.prepare element;
      Option.iter !handle ~f:cancel_auto_update;
      handle := Some (auto_update_position input element)
    ;;

    let update ~old_input ~new_input handle element =
      match phys_equal old_input new_input || Input.equal old_input new_input with
      | true -> ()
      | false ->
        Option.iter !handle ~f:cancel_auto_update;
        handle := Some (auto_update_position new_input element)
    ;;

    let on_mount = `Schedule_immediately_after_this_dom_patch_completes on_mount

    let destroy _ handle element =
      Update_position.clear_floating_properties element;
      Option.iter !handle ~f:cancel_auto_update
    ;;
  end

  include Impl
  include Vdom.Attr.Hooks.Make (Impl)
end

let hook_name = "floating_positioning_virtual"

let position_me
  ?(prepare = (ignore : Dom_html.element Js.t -> unit))
  ?arrow_selector
  ?(position = Position.Auto)
  ?(alignment = Alignment.Center)
  ?(offset = Offset.zero)
  ?match_anchor_side_length
  anchor
  =
  Position_element.create
    { prepare
    ; position
    ; alignment
    ; offset
    ; match_anchor_side_length
    ; arrow_selector
    ; anchor
    }
  |> Vdom.Attr.create_hook hook_name
  |> Vdom.Attr.combine
       (Css_gen.position
          (match anchor.Anchor.strategy with
           | Strategy.Fixed -> `Fixed
           | Absolute -> `Absolute)
        |> Vdom.Attr.style)
;;

module For_testing_position_me_hook = struct
  type t = Position_element.Input.t =
    { prepare : Dom_html.element Js.t -> unit
    ; position : Position.t
    ; alignment : Alignment.t
    ; offset : Offset.t
    ; match_anchor_side_length : Match_anchor_side.t option
    ; arrow_selector : string option
    ; anchor : Anchor.t
    }

  let type_id = Position_element.For_testing.type_id
  let hook_name = hook_name
end
