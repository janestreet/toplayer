open! Core
open Virtual_dom

module Node : sig
  type t = Vdom.Node.t [@@deriving sexp_of, equal]
end

module Attr : sig
  type t = Vdom.Attr.t [@@deriving sexp_of, equal]
end
