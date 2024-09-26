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

module Match_anchor_side = struct
  type t =
    | Grow_to_match
    | Match_exactly
    | Shrink_to_match
  [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Accessors = struct
  let floating_arrow_top = "--floatingArrowTop"
  let floating_arrow_left = "--floatingArrowLeft"
  let floating_max_height = "--floatingMaxHeight"
  let floating_max_width = "--floatingAvailableWidth"
  let floating_height = "--floatingHeight"
  let floating_width = "--floatingWidth"
  let floating_min_height = "--floatingMinHeight"
  let floating_min_width = "--floatingMinWidth"
  let data_floating_placement = "data-floating-placement"
  let data_floating_arrow_placement = "data-floating-arrow-placement"

  (* height/width: fit-content are browser defaults for popovers.
     `top/left` provide a consistent starting point for floating_ui to work from;
     these will be overriden by the `top`/`left` style attributes.

     https://floating-ui.com/docs/computePosition#usage

     `box-sizing: border-box` is required for anchor side length matching to work properly.

     This is all in a layer, so it can be overriden by [extra_attrs]
  *)
  let floating_styling =
    let module Style =
      [%css
      stylesheet
        {|
          @layer floating_positioning_new.floating_styling {
            .floating {
              top: 0;
              left: 0;
              box-sizing: border-box;

              height: %{`Var_with_default (floating_height, `Raw "fit-content")#Css_gen.Length};
              width: %{`Var_with_default (floating_width, `Raw "fit-content")#Css_gen.Length};
              min-height: %{`Var floating_min_height#Css_gen.Length};
              min-width: %{`Var floating_min_width#Css_gen.Length};
              max-width: %{`Var_with_default (floating_max_width, Css_gen.Length.percent100)#Css_gen.Length};
              max-height: %{`Var floating_max_height#Css_gen.Length};
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
        display: flex;
        align-items: center;
        justify-content: center;
        z-index: -1000;

        &[data-floating-arrow-placement="top"] {
          top: 0;
          transform: translateY(-50%);
        }
        &[data-floating-arrow-placement="bottom"] {
          bottom: 0;
          transform: translateY(50%) rotate(180deg);
        }
        &[data-floating-arrow-placement="left"] {
          left: 0;
          transform: translateX(-50%) rotate(-90deg);
        }
        &[data-floating-arrow-placement="right"] {
          right: 0;
          transform: translateX(50%) rotate(90deg);
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
  ~match_anchor_side_length
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
    let%bind.Option arrow_selector in
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
            (fun { available_height; available_width; rects; placement } ->
              match match_anchor_side_length with
              | None ->
                List.iter
                  Accessors.
                    [ floating_min_height
                    ; floating_min_width
                    ; floating_height
                    ; floating_width
                    ]
                  ~f:(remove_style floating);
                set_style
                  floating
                  Accessors.floating_max_height
                  (format_px available_height);
                set_style
                  floating
                  Accessors.floating_max_width
                  (format_px available_width)
              | Some min_or_exact ->
                let anchor_height, anchor_width =
                  match placement with
                  | Top | Top_start | Top_end | Bottom | Bottom_start | Bottom_end ->
                    None, Some rects.reference.width
                  | Right | Right_start | Right_end | Left | Left_start | Left_end ->
                    Some rects.reference.height, None
                in
                let min_height, height, max_height =
                  match min_or_exact with
                  | Match_anchor_side.Match_exactly ->
                    None, anchor_height, available_height
                  | Grow_to_match -> anchor_height, None, available_height
                  | Shrink_to_match ->
                    None, None, Option.value anchor_height ~default:available_height
                in
                set_or_remove_style
                  floating
                  Accessors.floating_min_height
                  (Option.map min_height ~f:format_px);
                set_or_remove_style
                  floating
                  Accessors.floating_height
                  (Option.map height ~f:format_px);
                set_style floating Accessors.floating_max_height (format_px max_height);
                let min_width, width, max_width =
                  match min_or_exact with
                  | Match_anchor_side.Match_exactly -> None, anchor_width, available_width
                  | Grow_to_match -> anchor_width, None, available_width
                  | Shrink_to_match ->
                    None, None, Option.value anchor_width ~default:available_width
                in
                set_or_remove_style
                  floating
                  Accessors.floating_min_width
                  (Option.map min_width ~f:format_px);
                set_or_remove_style
                  floating
                  Accessors.floating_width
                  (Option.map width ~f:format_px);
                set_style floating Accessors.floating_max_width (format_px max_width))
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
