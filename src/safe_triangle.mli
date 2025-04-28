open! Core
open! Virtual_dom

(** A safe triangle that can be used with submenus to make them mouse-accessible in a more
    natural way.

    See https://bjk5.com/post/44698559168/breaking-down-amazons-mega-dropdown for a quick
    introduction to this concept. *)
val attr : submenu_id:string -> Vdom.Attr.t
