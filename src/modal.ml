open! Core
open Virtual_dom

let lock_body_scroll_testing_attr_name = "data-lock-body-scroll-for-testing"

let lock_body_scroll_attr =
  (* Unfortunately, locking scroll by setting [overflow: hidden] results in a small
     layout shift, because of the scrollbar disappearing / reappearing.

     Setting[:root]'s [scrollbar-gutter] to [stable] or [stable both-edges] fixes this if
     content is overflowing at time of modal open, but if it is not, that will create a
     layout shift for adding the gutter. We don't want to set [scrollbar-gutter] for all
     apps, because that unconditionally wastes ~16px on each side.

     It might be possible to reimplement this locking via JS code that runs on modal
     activate / deactivate, but the semantics would be very difficult to get right,
     because:

     - Content might change while the page is opened
     - We can't get the scrollbar width while we have set [overflow: hidden]

     For now, the best path forward seems to be for apps that care about the layout shift
     to set up [scrollbar-gutter] themselves
  *)
  let actual_attr =
    Inline_css.Private.Dynamic.attr
      [%string {| @layer kado.app { body { overflow: hidden !important; } }|}]
  in
  match am_running_test with
  | false -> actual_attr
  | true ->
    Vdom.Attr.combine actual_attr (Vdom.Attr.create lock_body_scroll_testing_attr_name "")
;;

(* We need this only on modals because they are not constrained
   automatically by floating UI's size middleware. *)
let restrict_height_to_viewport = {%css|max-height: 100vh;|}
let testing_modal_attr_name = "data-testing-modal"

let testing_modal_attr =
  match am_running_test with
  | true -> Vdom.Attr.create testing_modal_attr_name ""
  | false -> Vdom.Attr.empty
;;

let node
  ?(modal_attrs = [])
  ?(lock_body_scroll = false)
  ?(restore_focus_on_close = true)
  ~overflow_auto_wrapper
  contents
  =
  let scroll_attr = if lock_body_scroll then lock_body_scroll_attr else Vdom.Attr.empty in
  Popover_dom.node
    ~restore_focus_on_close
    ~overflow_auto_wrapper
    ~extra_attrs:
      (Popover_dom.show_on_mount
       :: Inertness_management.for_modal
       :: scroll_attr
       :: restrict_height_to_viewport
       :: testing_modal_attr
       :: modal_attrs)
    ~kind:`Manual
    contents
;;

module For_testing_bonsai_web_ui_toplayer = struct
  let modal_attr_name = testing_modal_attr_name
  let lock_body_scroll_attr_name = lock_body_scroll_testing_attr_name
end
