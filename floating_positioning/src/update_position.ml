open! Core
open Js_of_ocaml
open Bindings

module Side = struct
  type t =
    | Top
    | Bottom
    | Left
    | Right

  let to_string = function
    | Top -> "top"
    | Bottom -> "bottom"
    | Left -> "left"
    | Right -> "right"
  ;;

  let of_placement = function
    | Placement.Top -> Top
    | Top_start -> Top
    | Top_end -> Top
    | Bottom -> Bottom
    | Bottom_start -> Bottom
    | Bottom_end -> Bottom
    | Right -> Right
    | Right_start -> Right
    | Right_end -> Right
    | Left -> Left
    | Left_start -> Left
    | Left_end -> Left
  ;;

  let flip = function
    | Top -> Bottom
    | Bottom -> Top
    | Left -> Right
    | Right -> Left
  ;;
end

module Accessors = struct
  let floating_arrow_top = "--floatingArrowTop"
  let floating_arrow_left = "--floatingArrowLeft"
  let floating_available_height = "--floatingAvailableHeight"
  let floating_available_width = "--floatingAvailableWidth"
  let data_floating_placement = "data-floating-placement"
  let data_floating_arrow_placement = "data-floating-arrow-placement"

  (* width: max-content ensures that the width stays steady during recomputation.
     `top/left` provide a consistent starting point for floating_ui to work from;
     these will be overriden by the `top`/`left` style attributes.

     https://floating-ui.com/docs/computePosition#usage
  *)
  let floating_styling =
    let module Style =
    [%css
    stylesheet
      {|
      @layer {
        .floating {
          width: max-content;
          top: 0;
          left: 0;
          max-height: %{`Var floating_available_height#Css_gen.Length};
          max-width: %{`Var floating_available_width#Css_gen.Length};
        }
      }
    |}]
    in
    Style.floating
  ;;

  let arrow_container =
    [%css
      {|
  left: %{`Var floating_arrow_left#Css_gen.Length};
  top: %{`Var floating_arrow_top#Css_gen.Length};
  position: absolute;
  display:flex;
  align-items:center;
  justify-content: center;
  z-index: -1000;

  &[data-floating-arrow-placement=top] {
    top: 0;
    transform: translateY(-50%);
  }
  &[data-floating-arrow-placement=bottom] {
    bottom: 0;
    transform: translateY(50%) rotate(180deg);
  }
  &[data-floating-arrow-placement=left] {
    left: 0;
    transform: translateX(-50%) rotate(-90deg);
  }
  &[data-floating-arrow-placement=right] {
    right: 0;
    transform: translateX(50%) rotate(90deg) ;
  }
      |}]
  ;;
end

let placement side alignment =
  match side, alignment with
  | Side.Top, None -> Placement.Top
  | Top, Some Alignment.Start -> Top_start
  | Top, Some End -> Top_end
  | Bottom, None -> Bottom
  | Bottom, Some Start -> Bottom_start
  | Bottom, Some End -> Bottom_end
  | Right, None -> Right
  | Right, Some Start -> Right_start
  | Right, Some End -> Right_end
  | Left, None -> Left
  | Left, Some Start -> Left_start
  | Left, Some End -> Left_end
;;

let set_style element property value =
  element##.style##setProperty (Js.string property) (Js.string value) Js.undefined
  |> (ignore : Js.js_string Js.t -> unit)
;;

let remove_style element property =
  element##.style##removeProperty (Js.string property)
  |> (ignore : Js.js_string Js.t -> unit)
;;

let set_or_remove_style element property = function
  | None -> remove_style element property
  | Some value -> set_style element property value
;;

let format_px px = Virtual_dom.Dom_float.to_string_fixed 8 px ^ "px"

let single_update
  ~anchor
  ~(floating : Dom_html.element Js.t)
  ~arrow_selector
  side
  alignment
  (offset : Offset.t)
  strategy
  =
  let padding = if Float.(offset.main_axis > 0.) then Some offset.main_axis else None in
  let offset_middleware =
    if Float.(offset.main_axis > 0.) || Float.(offset.cross_axis <> 0.)
    then [ Middleware.Offset.create offset ]
    else []
  in
  let placement, placement_middleware =
    match side with
    | None ->
      let auto_placement = Middleware.Auto_placement.create { alignment; padding } in
      Some Placement.Top, [ auto_placement ]
    | Some side ->
      ( Some (placement side alignment)
      , [ Middleware.Flip.create { padding }
        ; Middleware.Shift.create
            { padding
            ; limiter =
                Middleware.Shift.Limiter.create { main_axis = true; cross_axis = true }
            }
        ] )
  in
  let arrow_element =
    let%bind.Option arrow_selector = arrow_selector in
    floating##querySelector (Js.string arrow_selector) |> Js.Opt.to_option
  in
  let arrow_middleware =
    match arrow_element with
    | None -> []
    | Some arrow_element ->
      [ Middleware.Arrow.create { element = arrow_element; padding } ]
  in
  let size_middleware =
    [ Middleware.Size.create
        { apply =
            (fun { available_height; available_width } ->
              set_style
                floating
                Accessors.floating_available_height
                (format_px available_height);
              set_style
                floating
                Accessors.floating_available_width
                (format_px available_width))
        }
    ]
  in
  let middleware =
    offset_middleware @ placement_middleware @ size_middleware @ arrow_middleware
  in
  Compute_position.create ~anchor ~floating { placement; strategy; middleware }
  |> fun x ->
  Compute_position.then_
    x
    (fun { Compute_position.Then_args.x; y; placement; middleware_data; _ } ->
    let side = Side.of_placement placement in
    set_style floating "top" (format_px y);
    set_style floating "left" (format_px x);
    floating##setAttribute
      (Js.string Accessors.data_floating_placement)
      (Js.string (Side.to_string side));
    match middleware_data, arrow_element with
    | Some { arrow = Some { x; y } }, Some arrow_element ->
      set_or_remove_style
        arrow_element
        Accessors.floating_arrow_top
        (Option.map y ~f:format_px);
      set_or_remove_style
        arrow_element
        Accessors.floating_arrow_left
        (Option.map x ~f:format_px);
      arrow_element##setAttribute
        (Js.string Accessors.data_floating_arrow_placement)
        (Js.string (Side.to_string (Side.flip side)))
    | _ -> ())
;;
