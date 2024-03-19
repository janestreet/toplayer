open! Core
open Virtual_dom

type t = Vdom.Node.t

let sexp_of_t _ = Sexp.Atom "<vdom>"
let equal = phys_equal
