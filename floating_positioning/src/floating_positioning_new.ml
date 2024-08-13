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
  type t = Bindings.Reference_element.t

  let sexp_of_t = const (Sexp.Atom "<anchor>")
  let equal : t -> t -> bool = phys_equal
  let of_element element = `Dom element

  let of_function get_bounding_client_rect =
    `Virtual { Bindings.Reference_element.Virtual_element.get_bounding_client_rect }
  ;;

  let of_bounding_box ~top ~left ~bottom ~right =
    of_function (fun () ->
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

  let of_coordinate ~x ~y = of_bounding_box ~top:y ~bottom:y ~left:x ~right:x
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

let auto_update_position
  ?arrow_selector
  ~anchor
  ~floating
  ~match_anchor_side_length
  position
  alignment
  offset
  strategy
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
        ; strategy : Strategy.t
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
      ; strategy
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
        strategy
    ;;

    let on_mount (input : Input.t) handle element =
      input.prepare element;
      Option.iter !handle ~f:cancel_auto_update;
      handle := Some (auto_update_position input element)
    ;;

    let update ~old_input ~new_input handle element =
      if phys_equal old_input new_input || Input.equal old_input new_input
      then ()
      else
        Dom_html.window##requestAnimationFrame
          (Js.wrap_callback (fun (_timestamp : Js.number Js.t) ->
             Option.iter !handle ~f:cancel_auto_update;
             handle := Some (auto_update_position new_input element)))
        |> (ignore : Dom_html.animation_frame_request_id -> unit)
    ;;

    let on_mount = `Schedule_animation_frame on_mount
    let destroy _ handle _ = Option.iter !handle ~f:cancel_auto_update
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
    ; strategy = Fixed
    ; match_anchor_side_length
    ; arrow_selector
    ; anchor
    }
  |> Vdom.Attr.create_hook hook_name
;;

module For_testing_position_me_hook = struct
  type t = Position_element.Input.t =
    { prepare : Dom_html.element Js.t -> unit
    ; position : Position.t
    ; alignment : Alignment.t
    ; offset : Offset.t
    ; strategy : Strategy.t
    ; match_anchor_side_length : Match_anchor_side.t option
    ; arrow_selector : string option
    ; anchor : Anchor.t
    }

  let type_id = Position_element.For_testing.type_id
  let hook_name = hook_name
end
