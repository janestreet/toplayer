open! Core
open Virtual_dom

type t = Vdom.Node.t [@@deriving sexp_of, equal]
