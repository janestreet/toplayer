"Vdom Popover, Tooltip"
=======================

This library contains vdom utils for creating and positioning popovers, tooltips, and
modals.

Positioning is done via `floating_positioning`: bindings to the "floating ui" JS library.

Placement is done through a combination of the
[browser top-layer](https://developer.mozilla.org/en-US/docs/Glossary/Top_layer) and
[portaling](https://react.dev/reference/react-dom/createPortal).
This enables a convenient API, where all users need to do is attach a `Vdom.Attr.t` to the
popover / tooltip's anchor element.
