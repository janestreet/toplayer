open! Core
open! Js_of_ocaml
open! Gen_js_api
open Custom_converters

module Alignment : sig
  type t =
    | Start [@js "start"]
    | End [@js "end"]
  [@@js.enum] [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Placement : sig
  type t =
    | Top [@js "top"]
    | Top_start [@js "top-start"]
    | Top_end [@js "top-end"]
    | Bottom [@js "bottom"]
    | Bottom_start [@js "bottom-start"]
    | Bottom_end [@js "bottom-end"]
    | Right [@js "right"]
    | Right_start [@js "right-start"]
    | Right_end [@js "right-end"]
    | Left [@js "left"]
    | Left_start [@js "left-start"]
    | Left_end [@js "left-end"]
  [@@js.enum] [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Strategy : sig
  type t =
    | Absolute [@js "absolute"]
    | Fixed [@js "fixed"]
  [@@js.enum] [@@deriving sexp, sexp_grammar, equal, compare, enumerate]
end

module Offset : sig
  type t =
    { main_axis : float
    ; cross_axis : float
    }
  [@@deriving sexp, sexp_grammar, equal, compare]
end

module Middleware : sig
  type t

  module Offset : sig
    val create : Offset.t -> t [@@js.global "FloatingUIDOM.offset"]
  end

  module Size : sig
    module Options : sig
      module Apply_options : sig
        type t =
          { available_width : float
          ; available_height : float
          }
      end

      type t = { apply : Apply_options.t -> unit }
    end

    val create : Options.t -> t [@@js.global "FloatingUIDOM.size"]
  end

  module Flip : sig
    module Options : sig
      type t = { padding : float option }
    end

    val create : Options.t -> t [@@js.global "FloatingUIDOM.flip"]
  end

  module Shift : sig
    module Limiter : sig
      module Options : sig
        type t =
          { main_axis : bool
          ; cross_axis : bool
          }
      end

      type t

      val create : Options.t -> t [@@js.global "FloatingUIDOM.limitShift"]
    end

    module Options : sig
      type t =
        { padding : float option
        ; limiter : Limiter.t
        }
    end

    val create : Options.t -> t [@@js.global "FloatingUIDOM.shift"]
  end

  module Auto_placement : sig
    module Options : sig
      type t =
        { alignment : Alignment.t option
        ; padding : float option
        }
    end

    val create : Options.t -> t [@@js.global "FloatingUIDOM.autoPlacement"]
  end

  module Arrow : sig
    module Options : sig
      type t =
        { element : Dom_html.element Js.t
        ; padding : float option
        }
    end

    val create : Options.t -> t [@@js.global "FloatingUIDOM.arrow"]
  end
end

module Reference_element : sig
  module Client_rect_object : sig
    type t =
      { width : float
      ; height : float
      ; x : float
      ; y : float
      ; top : float
      ; left : float
      ; right : float
      ; bottom : float
      }
  end

  module Virtual_element : sig
    type t = { get_bounding_client_rect : unit -> Client_rect_object.t }
  end

  type t =
    ([ `Virtual of Virtual_element.t
     | `Dom of Dom_html.element Js.t
     ]
    [@js.union])
end

module Compute_position : sig
  module Options : sig
    type t =
      { placement : Placement.t option
      ; strategy : Strategy.t
      ; middleware : Middleware.t list
      }
  end

  module Then_args : sig
    type arrow =
      { x : float option
      ; y : float option
      }

    type middleware_data = { arrow : arrow option }

    type t =
      { x : float
      ; y : float
      ; placement : Placement.t
      ; strategy : Strategy.t
      ; middleware_data : middleware_data option
      }
  end

  type t

  val then_ : t -> (Then_args.t -> unit) -> unit [@@js.call "then"]

  val create
    :  anchor:Reference_element.t
    -> floating:Dom_html.element Js.t
    -> Options.t
    -> t
    [@@js.global "FloatingUIDOM.computePosition"]
end

module Auto_update_handle : sig
  type t

  val create
    :  anchor:Reference_element.t
    -> floating:Dom_html.element Js.t
    -> update:(unit -> unit)
    -> t
    [@@js.global "FloatingUIDOM.autoUpdate"]

  val cleanup : t -> unit [@@js.apply]
end
