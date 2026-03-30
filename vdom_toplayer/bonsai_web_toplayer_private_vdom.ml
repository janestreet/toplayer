open! Core
module Position = Bonsai_web_toplayer_private_floating.Position
module Alignment = Bonsai_web_toplayer_private_floating.Alignment
module Offset = Bonsai_web_toplayer_private_floating.Offset
module Match_anchor_side = Bonsai_web_toplayer_private_floating.Match_anchor_side
module Restore_focus_on_close = Popover_dom.Restore_focus_on_close

let tooltip = Tooltip.attr
let popover = Popover.attr

module For_bonsai_web_toplayer = struct
  let show_popover = Popover_dom.show_popover
  let focus_popover_on_open = Popover_dom.focus_popover_on_open
  let show_on_mount = Popover_dom.show_on_mount
  let arrow_selector = Popover_dom.arrow_selector
  let find_nearest_popover_ancestor = Popover_dom.find_nearest_popover_ancestor
  let popover_custom = Popover.custom
  let modal = Modal.node
end

module For_bonsai_web_menu = struct
  let safe_triangle = Safe_triangle.attr
end

module For_testing_popover_hook = Popover.For_testing_popover_hook
module For_testing_tooltip_hook = Tooltip.For_testing_tooltip_hook

module For_testing_bonsai_web_toplayer = struct
  include Popover.For_testing_bonsai_web_toplayer
  include Modal.For_testing_bonsai_web_toplayer
end

module For_jsdom_tests = struct
  let reset_inertness = Inertness_management.reset
end
