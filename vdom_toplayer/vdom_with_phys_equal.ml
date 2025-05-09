open! Core
open Virtual_dom

module Node = struct
  type t = Vdom.Node.t

  let sexp_of_t _ = Sexp.Atom "<vdom.node>"
  let equal = phys_equal
end

module Attr = struct
  type t = Vdom.Attr.t

  let sexp_of_t _ = Sexp.Atom "<vdom.attr>"
  let equal = phys_equal
end
