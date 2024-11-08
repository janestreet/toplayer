open! Core

let tooltip = Tooltip.attr
let popover = Popover.attr

module For_use_in_portals = struct
  let popover_custom = Popover.node
  let modal = Modal.node
end

module For_bonsai_web_ui_toplayer = struct
  let find_nearest_popover_ancestor = Portal.For_popovers.find_nearest_popover_ancestor
  let focus_popover_on_open = Popover_dom.focus_popover_on_open

  module Portal = Portal
end

module For_testing_popover_hook = Popover.For_testing_popover_hook

module For_testing_bonsai_web_ui_toplayer = struct
  include Popover.For_testing_bonsai_web_ui_toplayer
  include Modal.For_testing_bonsai_web_ui_toplayer
end

module For_debugging_frame_delay = struct
  let mark_events = Config.mark_events
end
