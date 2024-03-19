open! Core
open! Js_of_ocaml
open Virtual_dom
module Accessors = Update_position.Accessors
module Strategy = Bindings.Strategy

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

let update_position ?arrow_selector ~anchor ~floating position alignment offset strategy =
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
  position
  alignment
  offset
  strategy
  =
  Bindings.Auto_update_handle.create ~anchor ~floating ~update:(fun () ->
    update_position ?arrow_selector ~anchor ~floating position alignment offset strategy)
;;

let cancel_auto_update = Bindings.Auto_update_handle.cleanup

module Position_element = struct
  module Impl = struct
    module State = struct
      type t = auto_update_handle option ref
    end

    module Input = struct
      type t =
        { position : Position.t
        ; alignment : Alignment.t
        ; offset : Offset.t
        ; strategy : Strategy.t
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
      { Input.position; alignment; offset; strategy; arrow_selector; anchor }
      element
      =
      auto_update_position
        ?arrow_selector
        ~anchor
        ~floating:element
        position
        alignment
        offset
        strategy
    ;;

    let on_mount input handle element =
      Option.iter !handle ~f:cancel_auto_update;
      handle := Some (auto_update_position input element)
    ;;

    let update ~old_input ~new_input handle element =
      if phys_equal old_input new_input || Input.equal old_input new_input
      then ()
      else
        Dom_html.window##requestAnimationFrame
          (Js.wrap_callback (fun (_timestamp : float) ->
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

let position_me
  ?arrow_selector
  ?(position = Position.Auto)
  ?(alignment = Alignment.Center)
  ?(offset = Offset.zero)
  anchor
  =
  Position_element.create
    { position; alignment; offset; strategy = Fixed; arrow_selector; anchor }
  |> Vdom.Attr.create_hook "floating_positioning_virtual"
;;
