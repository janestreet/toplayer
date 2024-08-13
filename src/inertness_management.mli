open! Core
open Virtual_dom

(** When a [dialog] HTML element is opened via [showModal], the whole DOM tree will
    become inert, and the dialog will be exempt.

    We're not using [dialog], so instead, we need to explicitly add/remove the inertness
    property from the app root, and any popovers under the topmost open modal.  *)
val for_popover : Vdom.Attr.t

val for_modal : Vdom.Attr.t
