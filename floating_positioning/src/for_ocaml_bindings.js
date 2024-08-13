function _iterableToArrayLimit(r, l) {
  var t = null == r ? null : "undefined" != typeof Symbol && r[Symbol.iterator] || r["@@iterator"];
  if (null != t) {
    var e,
      n,
      i,
      u,
      a = [],
      f = !0,
      o = !1;
    try {
      if (i = (t = t.call(r)).next, 0 === l) {
        if (Object(t) !== t) return;
        f = !1;
      } else for (; !(f = (e = i.call(t)).done) && (a.push(e.value), a.length !== l); f = !0);
    } catch (r) {
      o = !0, n = r;
    } finally {
      try {
        if (!f && null != t.return && (u = t.return(), Object(u) !== u)) return;
      } finally {
        if (o) throw n;
      }
    }
    return a;
  }
}
function ownKeys(e, r) {
  var t = Object.keys(e);
  if (Object.getOwnPropertySymbols) {
    var o = Object.getOwnPropertySymbols(e);
    r && (o = o.filter(function (r) {
      return Object.getOwnPropertyDescriptor(e, r).enumerable;
    })), t.push.apply(t, o);
  }
  return t;
}
function _objectSpread2(e) {
  for (var r = 1; r < arguments.length; r++) {
    var t = null != arguments[r] ? arguments[r] : {};
    r % 2 ? ownKeys(Object(t), !0).forEach(function (r) {
      _defineProperty(e, r, t[r]);
    }) : Object.getOwnPropertyDescriptors ? Object.defineProperties(e, Object.getOwnPropertyDescriptors(t)) : ownKeys(Object(t)).forEach(function (r) {
      Object.defineProperty(e, r, Object.getOwnPropertyDescriptor(t, r));
    });
  }
  return e;
}
function _regeneratorRuntime() {
  _regeneratorRuntime = function () {
    return e;
  };
  var t,
    e = {},
    r = Object.prototype,
    n = r.hasOwnProperty,
    o = Object.defineProperty || function (t, e, r) {
      t[e] = r.value;
    },
    i = "function" == typeof Symbol ? Symbol : {},
    a = i.iterator || "@@iterator",
    c = i.asyncIterator || "@@asyncIterator",
    u = i.toStringTag || "@@toStringTag";
  function define(t, e, r) {
    return Object.defineProperty(t, e, {
      value: r,
      enumerable: !0,
      configurable: !0,
      writable: !0
    }), t[e];
  }
  try {
    define({}, "");
  } catch (t) {
    define = function (t, e, r) {
      return t[e] = r;
    };
  }
  function wrap(t, e, r, n) {
    var i = e && e.prototype instanceof Generator ? e : Generator,
      a = Object.create(i.prototype),
      c = new Context(n || []);
    return o(a, "_invoke", {
      value: makeInvokeMethod(t, r, c)
    }), a;
  }
  function tryCatch(t, e, r) {
    try {
      return {
        type: "normal",
        arg: t.call(e, r)
      };
    } catch (t) {
      return {
        type: "throw",
        arg: t
      };
    }
  }
  e.wrap = wrap;
  var h = "suspendedStart",
    l = "suspendedYield",
    f = "executing",
    s = "completed",
    y = {};
  function Generator() {}
  function GeneratorFunction() {}
  function GeneratorFunctionPrototype() {}
  var p = {};
  define(p, a, function () {
    return this;
  });
  var d = Object.getPrototypeOf,
    v = d && d(d(values([])));
  v && v !== r && n.call(v, a) && (p = v);
  var g = GeneratorFunctionPrototype.prototype = Generator.prototype = Object.create(p);
  function defineIteratorMethods(t) {
    ["next", "throw", "return"].forEach(function (e) {
      define(t, e, function (t) {
        return this._invoke(e, t);
      });
    });
  }
  function AsyncIterator(t, e) {
    function invoke(r, o, i, a) {
      var c = tryCatch(t[r], t, o);
      if ("throw" !== c.type) {
        var u = c.arg,
          h = u.value;
        return h && "object" == typeof h && n.call(h, "__await") ? e.resolve(h.__await).then(function (t) {
          invoke("next", t, i, a);
        }, function (t) {
          invoke("throw", t, i, a);
        }) : e.resolve(h).then(function (t) {
          u.value = t, i(u);
        }, function (t) {
          return invoke("throw", t, i, a);
        });
      }
      a(c.arg);
    }
    var r;
    o(this, "_invoke", {
      value: function (t, n) {
        function callInvokeWithMethodAndArg() {
          return new e(function (e, r) {
            invoke(t, n, e, r);
          });
        }
        return r = r ? r.then(callInvokeWithMethodAndArg, callInvokeWithMethodAndArg) : callInvokeWithMethodAndArg();
      }
    });
  }
  function makeInvokeMethod(e, r, n) {
    var o = h;
    return function (i, a) {
      if (o === f) throw Error("Generator is already running");
      if (o === s) {
        if ("throw" === i) throw a;
        return {
          value: t,
          done: !0
        };
      }
      for (n.method = i, n.arg = a;;) {
        var c = n.delegate;
        if (c) {
          var u = maybeInvokeDelegate(c, n);
          if (u) {
            if (u === y) continue;
            return u;
          }
        }
        if ("next" === n.method) n.sent = n._sent = n.arg;else if ("throw" === n.method) {
          if (o === h) throw o = s, n.arg;
          n.dispatchException(n.arg);
        } else "return" === n.method && n.abrupt("return", n.arg);
        o = f;
        var p = tryCatch(e, r, n);
        if ("normal" === p.type) {
          if (o = n.done ? s : l, p.arg === y) continue;
          return {
            value: p.arg,
            done: n.done
          };
        }
        "throw" === p.type && (o = s, n.method = "throw", n.arg = p.arg);
      }
    };
  }
  function maybeInvokeDelegate(e, r) {
    var n = r.method,
      o = e.iterator[n];
    if (o === t) return r.delegate = null, "throw" === n && e.iterator.return && (r.method = "return", r.arg = t, maybeInvokeDelegate(e, r), "throw" === r.method) || "return" !== n && (r.method = "throw", r.arg = new TypeError("The iterator does not provide a '" + n + "' method")), y;
    var i = tryCatch(o, e.iterator, r.arg);
    if ("throw" === i.type) return r.method = "throw", r.arg = i.arg, r.delegate = null, y;
    var a = i.arg;
    return a ? a.done ? (r[e.resultName] = a.value, r.next = e.nextLoc, "return" !== r.method && (r.method = "next", r.arg = t), r.delegate = null, y) : a : (r.method = "throw", r.arg = new TypeError("iterator result is not an object"), r.delegate = null, y);
  }
  function pushTryEntry(t) {
    var e = {
      tryLoc: t[0]
    };
    1 in t && (e.catchLoc = t[1]), 2 in t && (e.finallyLoc = t[2], e.afterLoc = t[3]), this.tryEntries.push(e);
  }
  function resetTryEntry(t) {
    var e = t.completion || {};
    e.type = "normal", delete e.arg, t.completion = e;
  }
  function Context(t) {
    this.tryEntries = [{
      tryLoc: "root"
    }], t.forEach(pushTryEntry, this), this.reset(!0);
  }
  function values(e) {
    if (e || "" === e) {
      var r = e[a];
      if (r) return r.call(e);
      if ("function" == typeof e.next) return e;
      if (!isNaN(e.length)) {
        var o = -1,
          i = function next() {
            for (; ++o < e.length;) if (n.call(e, o)) return next.value = e[o], next.done = !1, next;
            return next.value = t, next.done = !0, next;
          };
        return i.next = i;
      }
    }
    throw new TypeError(typeof e + " is not iterable");
  }
  return GeneratorFunction.prototype = GeneratorFunctionPrototype, o(g, "constructor", {
    value: GeneratorFunctionPrototype,
    configurable: !0
  }), o(GeneratorFunctionPrototype, "constructor", {
    value: GeneratorFunction,
    configurable: !0
  }), GeneratorFunction.displayName = define(GeneratorFunctionPrototype, u, "GeneratorFunction"), e.isGeneratorFunction = function (t) {
    var e = "function" == typeof t && t.constructor;
    return !!e && (e === GeneratorFunction || "GeneratorFunction" === (e.displayName || e.name));
  }, e.mark = function (t) {
    return Object.setPrototypeOf ? Object.setPrototypeOf(t, GeneratorFunctionPrototype) : (t.__proto__ = GeneratorFunctionPrototype, define(t, u, "GeneratorFunction")), t.prototype = Object.create(g), t;
  }, e.awrap = function (t) {
    return {
      __await: t
    };
  }, defineIteratorMethods(AsyncIterator.prototype), define(AsyncIterator.prototype, c, function () {
    return this;
  }), e.AsyncIterator = AsyncIterator, e.async = function (t, r, n, o, i) {
    void 0 === i && (i = Promise);
    var a = new AsyncIterator(wrap(t, r, n, o), i);
    return e.isGeneratorFunction(r) ? a : a.next().then(function (t) {
      return t.done ? t.value : a.next();
    });
  }, defineIteratorMethods(g), define(g, u, "Generator"), define(g, a, function () {
    return this;
  }), define(g, "toString", function () {
    return "[object Generator]";
  }), e.keys = function (t) {
    var e = Object(t),
      r = [];
    for (var n in e) r.push(n);
    return r.reverse(), function next() {
      for (; r.length;) {
        var t = r.pop();
        if (t in e) return next.value = t, next.done = !1, next;
      }
      return next.done = !0, next;
    };
  }, e.values = values, Context.prototype = {
    constructor: Context,
    reset: function (e) {
      if (this.prev = 0, this.next = 0, this.sent = this._sent = t, this.done = !1, this.delegate = null, this.method = "next", this.arg = t, this.tryEntries.forEach(resetTryEntry), !e) for (var r in this) "t" === r.charAt(0) && n.call(this, r) && !isNaN(+r.slice(1)) && (this[r] = t);
    },
    stop: function () {
      this.done = !0;
      var t = this.tryEntries[0].completion;
      if ("throw" === t.type) throw t.arg;
      return this.rval;
    },
    dispatchException: function (e) {
      if (this.done) throw e;
      var r = this;
      function handle(n, o) {
        return a.type = "throw", a.arg = e, r.next = n, o && (r.method = "next", r.arg = t), !!o;
      }
      for (var o = this.tryEntries.length - 1; o >= 0; --o) {
        var i = this.tryEntries[o],
          a = i.completion;
        if ("root" === i.tryLoc) return handle("end");
        if (i.tryLoc <= this.prev) {
          var c = n.call(i, "catchLoc"),
            u = n.call(i, "finallyLoc");
          if (c && u) {
            if (this.prev < i.catchLoc) return handle(i.catchLoc, !0);
            if (this.prev < i.finallyLoc) return handle(i.finallyLoc);
          } else if (c) {
            if (this.prev < i.catchLoc) return handle(i.catchLoc, !0);
          } else {
            if (!u) throw Error("try statement without catch or finally");
            if (this.prev < i.finallyLoc) return handle(i.finallyLoc);
          }
        }
      }
    },
    abrupt: function (t, e) {
      for (var r = this.tryEntries.length - 1; r >= 0; --r) {
        var o = this.tryEntries[r];
        if (o.tryLoc <= this.prev && n.call(o, "finallyLoc") && this.prev < o.finallyLoc) {
          var i = o;
          break;
        }
      }
      i && ("break" === t || "continue" === t) && i.tryLoc <= e && e <= i.finallyLoc && (i = null);
      var a = i ? i.completion : {};
      return a.type = t, a.arg = e, i ? (this.method = "next", this.next = i.finallyLoc, y) : this.complete(a);
    },
    complete: function (t, e) {
      if ("throw" === t.type) throw t.arg;
      return "break" === t.type || "continue" === t.type ? this.next = t.arg : "return" === t.type ? (this.rval = this.arg = t.arg, this.method = "return", this.next = "end") : "normal" === t.type && e && (this.next = e), y;
    },
    finish: function (t) {
      for (var e = this.tryEntries.length - 1; e >= 0; --e) {
        var r = this.tryEntries[e];
        if (r.finallyLoc === t) return this.complete(r.completion, r.afterLoc), resetTryEntry(r), y;
      }
    },
    catch: function (t) {
      for (var e = this.tryEntries.length - 1; e >= 0; --e) {
        var r = this.tryEntries[e];
        if (r.tryLoc === t) {
          var n = r.completion;
          if ("throw" === n.type) {
            var o = n.arg;
            resetTryEntry(r);
          }
          return o;
        }
      }
      throw Error("illegal catch attempt");
    },
    delegateYield: function (e, r, n) {
      return this.delegate = {
        iterator: values(e),
        resultName: r,
        nextLoc: n
      }, "next" === this.method && (this.arg = t), y;
    }
  }, e;
}
function _toPrimitive(t, r) {
  if ("object" != typeof t || !t) return t;
  var e = t[Symbol.toPrimitive];
  if (void 0 !== e) {
    var i = e.call(t, r || "default");
    if ("object" != typeof i) return i;
    throw new TypeError("@@toPrimitive must return a primitive value.");
  }
  return ("string" === r ? String : Number)(t);
}
function _toPropertyKey(t) {
  var i = _toPrimitive(t, "string");
  return "symbol" == typeof i ? i : i + "";
}
function _typeof(o) {
  "@babel/helpers - typeof";

  return _typeof = "function" == typeof Symbol && "symbol" == typeof Symbol.iterator ? function (o) {
    return typeof o;
  } : function (o) {
    return o && "function" == typeof Symbol && o.constructor === Symbol && o !== Symbol.prototype ? "symbol" : typeof o;
  }, _typeof(o);
}
function asyncGeneratorStep(gen, resolve, reject, _next, _throw, key, arg) {
  try {
    var info = gen[key](arg);
    var value = info.value;
  } catch (error) {
    reject(error);
    return;
  }
  if (info.done) {
    resolve(value);
  } else {
    Promise.resolve(value).then(_next, _throw);
  }
}
function _asyncToGenerator(fn) {
  return function () {
    var self = this,
      args = arguments;
    return new Promise(function (resolve, reject) {
      var gen = fn.apply(self, args);
      function _next(value) {
        asyncGeneratorStep(gen, resolve, reject, _next, _throw, "next", value);
      }
      function _throw(err) {
        asyncGeneratorStep(gen, resolve, reject, _next, _throw, "throw", err);
      }
      _next(undefined);
    });
  };
}
function _defineProperty(obj, key, value) {
  key = _toPropertyKey(key);
  if (key in obj) {
    Object.defineProperty(obj, key, {
      value: value,
      enumerable: true,
      configurable: true,
      writable: true
    });
  } else {
    obj[key] = value;
  }
  return obj;
}
function _objectWithoutPropertiesLoose(source, excluded) {
  if (source == null) return {};
  var target = {};
  for (var key in source) {
    if (Object.prototype.hasOwnProperty.call(source, key)) {
      if (excluded.indexOf(key) >= 0) continue;
      target[key] = source[key];
    }
  }
  return target;
}
function _objectWithoutProperties(source, excluded) {
  if (source == null) return {};
  var target = _objectWithoutPropertiesLoose(source, excluded);
  var key, i;
  if (Object.getOwnPropertySymbols) {
    var sourceSymbolKeys = Object.getOwnPropertySymbols(source);
    for (i = 0; i < sourceSymbolKeys.length; i++) {
      key = sourceSymbolKeys[i];
      if (excluded.indexOf(key) >= 0) continue;
      if (!Object.prototype.propertyIsEnumerable.call(source, key)) continue;
      target[key] = source[key];
    }
  }
  return target;
}
function _slicedToArray(arr, i) {
  return _arrayWithHoles(arr) || _iterableToArrayLimit(arr, i) || _unsupportedIterableToArray(arr, i) || _nonIterableRest();
}
function _toConsumableArray(arr) {
  return _arrayWithoutHoles(arr) || _iterableToArray(arr) || _unsupportedIterableToArray(arr) || _nonIterableSpread();
}
function _arrayWithoutHoles(arr) {
  if (Array.isArray(arr)) return _arrayLikeToArray(arr);
}
function _arrayWithHoles(arr) {
  if (Array.isArray(arr)) return arr;
}
function _iterableToArray(iter) {
  if (typeof Symbol !== "undefined" && iter[Symbol.iterator] != null || iter["@@iterator"] != null) return Array.from(iter);
}
function _unsupportedIterableToArray(o, minLen) {
  if (!o) return;
  if (typeof o === "string") return _arrayLikeToArray(o, minLen);
  var n = Object.prototype.toString.call(o).slice(8, -1);
  if (n === "Object" && o.constructor) n = o.constructor.name;
  if (n === "Map" || n === "Set") return Array.from(o);
  if (n === "Arguments" || /^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(n)) return _arrayLikeToArray(o, minLen);
}
function _arrayLikeToArray(arr, len) {
  if (len == null || len > arr.length) len = arr.length;
  for (var i = 0, arr2 = new Array(len); i < len; i++) arr2[i] = arr[i];
  return arr2;
}
function _nonIterableSpread() {
  throw new TypeError("Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}
function _nonIterableRest() {
  throw new TypeError("Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.");
}

/**
 * Custom positioning reference element.
 * @see https://floating-ui.com/docs/virtual-elements
 */

var sides = ['top', 'right', 'bottom', 'left'];
var alignments = ['start', 'end'];
var placements = /*#__PURE__*/sides.reduce(function (acc, side) {
  return acc.concat(side, side + "-" + alignments[0], side + "-" + alignments[1]);
}, []);
var min = Math.min;
var max = Math.max;
var round = Math.round;
var floor = Math.floor;
var createCoords = function createCoords(v) {
  return {
    x: v,
    y: v
  };
};
var oppositeSideMap = {
  left: 'right',
  right: 'left',
  bottom: 'top',
  top: 'bottom'
};
var oppositeAlignmentMap = {
  start: 'end',
  end: 'start'
};
function clamp(start, value, end) {
  return max(start, min(value, end));
}
function evaluate(value, param) {
  return typeof value === 'function' ? value(param) : value;
}
function getSide(placement) {
  return placement.split('-')[0];
}
function getAlignment(placement) {
  return placement.split('-')[1];
}
function getOppositeAxis(axis) {
  return axis === 'x' ? 'y' : 'x';
}
function getAxisLength(axis) {
  return axis === 'y' ? 'height' : 'width';
}
function getSideAxis(placement) {
  return ['top', 'bottom'].includes(getSide(placement)) ? 'y' : 'x';
}
function getAlignmentAxis(placement) {
  return getOppositeAxis(getSideAxis(placement));
}
function getAlignmentSides(placement, rects, rtl) {
  if (rtl === void 0) {
    rtl = false;
  }
  var alignment = getAlignment(placement);
  var alignmentAxis = getAlignmentAxis(placement);
  var length = getAxisLength(alignmentAxis);
  var mainAlignmentSide = alignmentAxis === 'x' ? alignment === (rtl ? 'end' : 'start') ? 'right' : 'left' : alignment === 'start' ? 'bottom' : 'top';
  if (rects.reference[length] > rects.floating[length]) {
    mainAlignmentSide = getOppositePlacement(mainAlignmentSide);
  }
  return [mainAlignmentSide, getOppositePlacement(mainAlignmentSide)];
}
function getExpandedPlacements(placement) {
  var oppositePlacement = getOppositePlacement(placement);
  return [getOppositeAlignmentPlacement(placement), oppositePlacement, getOppositeAlignmentPlacement(oppositePlacement)];
}
function getOppositeAlignmentPlacement(placement) {
  return placement.replace(/start|end/g, function (alignment) {
    return oppositeAlignmentMap[alignment];
  });
}
function getSideList(side, isStart, rtl) {
  var lr = ['left', 'right'];
  var rl = ['right', 'left'];
  var tb = ['top', 'bottom'];
  var bt = ['bottom', 'top'];
  switch (side) {
    case 'top':
    case 'bottom':
      if (rtl) return isStart ? rl : lr;
      return isStart ? lr : rl;
    case 'left':
    case 'right':
      return isStart ? tb : bt;
    default:
      return [];
  }
}
function getOppositeAxisPlacements(placement, flipAlignment, direction, rtl) {
  var alignment = getAlignment(placement);
  var list = getSideList(getSide(placement), direction === 'start', rtl);
  if (alignment) {
    list = list.map(function (side) {
      return side + "-" + alignment;
    });
    if (flipAlignment) {
      list = list.concat(list.map(getOppositeAlignmentPlacement));
    }
  }
  return list;
}
function getOppositePlacement(placement) {
  return placement.replace(/left|right|bottom|top/g, function (side) {
    return oppositeSideMap[side];
  });
}
function expandPaddingObject(padding) {
  return _objectSpread2({
    top: 0,
    right: 0,
    bottom: 0,
    left: 0
  }, padding);
}
function getPaddingObject(padding) {
  return typeof padding !== 'number' ? expandPaddingObject(padding) : {
    top: padding,
    right: padding,
    bottom: padding,
    left: padding
  };
}
function rectToClientRect(rect) {
  var x = rect.x,
    y = rect.y,
    width = rect.width,
    height = rect.height;
  return {
    width: width,
    height: height,
    top: y,
    left: x,
    right: x + width,
    bottom: y + height,
    x: x,
    y: y
  };
}

var _excluded = ["crossAxis", "alignment", "allowedPlacements", "autoAlignment"],
  _excluded2 = ["mainAxis", "crossAxis", "fallbackPlacements", "fallbackStrategy", "fallbackAxisSideDirection", "flipAlignment"],
  _excluded3 = ["strategy"],
  _excluded4 = ["mainAxis", "crossAxis", "limiter"],
  _excluded5 = ["apply"];
function computeCoordsFromPlacement(_ref, placement, rtl) {
  var reference = _ref.reference,
    floating = _ref.floating;
  var sideAxis = getSideAxis(placement);
  var alignmentAxis = getAlignmentAxis(placement);
  var alignLength = getAxisLength(alignmentAxis);
  var side = getSide(placement);
  var isVertical = sideAxis === 'y';
  var commonX = reference.x + reference.width / 2 - floating.width / 2;
  var commonY = reference.y + reference.height / 2 - floating.height / 2;
  var commonAlign = reference[alignLength] / 2 - floating[alignLength] / 2;
  var coords;
  switch (side) {
    case 'top':
      coords = {
        x: commonX,
        y: reference.y - floating.height
      };
      break;
    case 'bottom':
      coords = {
        x: commonX,
        y: reference.y + reference.height
      };
      break;
    case 'right':
      coords = {
        x: reference.x + reference.width,
        y: commonY
      };
      break;
    case 'left':
      coords = {
        x: reference.x - floating.width,
        y: commonY
      };
      break;
    default:
      coords = {
        x: reference.x,
        y: reference.y
      };
  }
  switch (getAlignment(placement)) {
    case 'start':
      coords[alignmentAxis] -= commonAlign * (rtl && isVertical ? -1 : 1);
      break;
    case 'end':
      coords[alignmentAxis] += commonAlign * (rtl && isVertical ? -1 : 1);
      break;
  }
  return coords;
}

/**
 * Computes the `x` and `y` coordinates that will place the floating element
 * next to a given reference element.
 *
 * This export does not have any `platform` interface logic. You will need to
 * write one for the platform you are using Floating UI with.
 */
var computePosition$1 = /*#__PURE__*/function () {
  var _ref2 = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee(reference, floating, config) {
    var _config$placement, placement, _config$strategy, strategy, _config$middleware, middleware, platform, validMiddleware, rtl, rects, _computeCoordsFromPla, x, y, statefulPlacement, middlewareData, resetCount, i, _validMiddleware$i, name, fn, _yield$fn, nextX, nextY, data, reset, _computeCoordsFromPla2;
    return _regeneratorRuntime().wrap(function _callee$(_context) {
      while (1) switch (_context.prev = _context.next) {
        case 0:
          _config$placement = config.placement, placement = _config$placement === void 0 ? 'bottom' : _config$placement, _config$strategy = config.strategy, strategy = _config$strategy === void 0 ? 'absolute' : _config$strategy, _config$middleware = config.middleware, middleware = _config$middleware === void 0 ? [] : _config$middleware, platform = config.platform;
          validMiddleware = middleware.filter(Boolean);
          _context.next = 4;
          return platform.isRTL == null ? void 0 : platform.isRTL(floating);
        case 4:
          rtl = _context.sent;
          _context.next = 7;
          return platform.getElementRects({
            reference: reference,
            floating: floating,
            strategy: strategy
          });
        case 7:
          rects = _context.sent;
          _computeCoordsFromPla = computeCoordsFromPlacement(rects, placement, rtl), x = _computeCoordsFromPla.x, y = _computeCoordsFromPla.y;
          statefulPlacement = placement;
          middlewareData = {};
          resetCount = 0;
          i = 0;
        case 13:
          if (!(i < validMiddleware.length)) {
            _context.next = 45;
            break;
          }
          _validMiddleware$i = validMiddleware[i], name = _validMiddleware$i.name, fn = _validMiddleware$i.fn;
          _context.next = 17;
          return fn({
            x: x,
            y: y,
            initialPlacement: placement,
            placement: statefulPlacement,
            strategy: strategy,
            middlewareData: middlewareData,
            rects: rects,
            platform: platform,
            elements: {
              reference: reference,
              floating: floating
            }
          });
        case 17:
          _yield$fn = _context.sent;
          nextX = _yield$fn.x;
          nextY = _yield$fn.y;
          data = _yield$fn.data;
          reset = _yield$fn.reset;
          x = nextX != null ? nextX : x;
          y = nextY != null ? nextY : y;
          middlewareData = _objectSpread2(_objectSpread2({}, middlewareData), {}, _defineProperty({}, name, _objectSpread2(_objectSpread2({}, middlewareData[name]), data)));
          if (!(reset && resetCount <= 50)) {
            _context.next = 42;
            break;
          }
          resetCount++;
          if (!(_typeof(reset) === 'object')) {
            _context.next = 41;
            break;
          }
          if (reset.placement) {
            statefulPlacement = reset.placement;
          }
          if (!reset.rects) {
            _context.next = 38;
            break;
          }
          if (!(reset.rects === true)) {
            _context.next = 36;
            break;
          }
          _context.next = 33;
          return platform.getElementRects({
            reference: reference,
            floating: floating,
            strategy: strategy
          });
        case 33:
          _context.t0 = _context.sent;
          _context.next = 37;
          break;
        case 36:
          _context.t0 = reset.rects;
        case 37:
          rects = _context.t0;
        case 38:
          _computeCoordsFromPla2 = computeCoordsFromPlacement(rects, statefulPlacement, rtl);
          x = _computeCoordsFromPla2.x;
          y = _computeCoordsFromPla2.y;
        case 41:
          i = -1;
        case 42:
          i++;
          _context.next = 13;
          break;
        case 45:
          return _context.abrupt("return", {
            x: x,
            y: y,
            placement: statefulPlacement,
            strategy: strategy,
            middlewareData: middlewareData
          });
        case 46:
        case "end":
          return _context.stop();
      }
    }, _callee);
  }));
  return function computePosition(_x, _x2, _x3) {
    return _ref2.apply(this, arguments);
  };
}();

/**
 * Resolves with an object of overflow side offsets that determine how much the
 * element is overflowing a given clipping boundary on each side.
 * - positive = overflowing the boundary by that number of pixels
 * - negative = how many pixels left before it will overflow
 * - 0 = lies flush with the boundary
 * @see https://floating-ui.com/docs/detectOverflow
 */
function detectOverflow$1(_x4, _x5) {
  return _detectOverflow.apply(this, arguments);
}
/**
 * Provides data to position an inner element of the floating element so that it
 * appears centered to the reference element.
 * @see https://floating-ui.com/docs/arrow
 */
function _detectOverflow() {
  _detectOverflow = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee10(state, options) {
    var _await$platform$isEle, x, y, platform, rects, elements, strategy, _evaluate8, _evaluate8$boundary, boundary, _evaluate8$rootBounda, rootBoundary, _evaluate8$elementCon, elementContext, _evaluate8$altBoundar, altBoundary, _evaluate8$padding, padding, paddingObject, altContext, element, clippingClientRect, rect, offsetParent, offsetScale, elementClientRect;
    return _regeneratorRuntime().wrap(function _callee10$(_context10) {
      while (1) switch (_context10.prev = _context10.next) {
        case 0:
          if (options === void 0) {
            options = {};
          }
          x = state.x, y = state.y, platform = state.platform, rects = state.rects, elements = state.elements, strategy = state.strategy;
          _evaluate8 = evaluate(options, state), _evaluate8$boundary = _evaluate8.boundary, boundary = _evaluate8$boundary === void 0 ? 'clippingAncestors' : _evaluate8$boundary, _evaluate8$rootBounda = _evaluate8.rootBoundary, rootBoundary = _evaluate8$rootBounda === void 0 ? 'viewport' : _evaluate8$rootBounda, _evaluate8$elementCon = _evaluate8.elementContext, elementContext = _evaluate8$elementCon === void 0 ? 'floating' : _evaluate8$elementCon, _evaluate8$altBoundar = _evaluate8.altBoundary, altBoundary = _evaluate8$altBoundar === void 0 ? false : _evaluate8$altBoundar, _evaluate8$padding = _evaluate8.padding, padding = _evaluate8$padding === void 0 ? 0 : _evaluate8$padding;
          paddingObject = getPaddingObject(padding);
          altContext = elementContext === 'floating' ? 'reference' : 'floating';
          element = elements[altBoundary ? altContext : elementContext];
          _context10.t0 = rectToClientRect;
          _context10.t1 = platform;
          _context10.next = 10;
          return platform.isElement == null ? void 0 : platform.isElement(element);
        case 10:
          _context10.t2 = _await$platform$isEle = _context10.sent;
          if (!(_context10.t2 != null)) {
            _context10.next = 15;
            break;
          }
          _context10.t3 = _await$platform$isEle;
          _context10.next = 16;
          break;
        case 15:
          _context10.t3 = true;
        case 16:
          if (!_context10.t3) {
            _context10.next = 20;
            break;
          }
          _context10.t4 = element;
          _context10.next = 26;
          break;
        case 20:
          _context10.t5 = element.contextElement;
          if (_context10.t5) {
            _context10.next = 25;
            break;
          }
          _context10.next = 24;
          return platform.getDocumentElement == null ? void 0 : platform.getDocumentElement(elements.floating);
        case 24:
          _context10.t5 = _context10.sent;
        case 25:
          _context10.t4 = _context10.t5;
        case 26:
          _context10.t6 = _context10.t4;
          _context10.t7 = boundary;
          _context10.t8 = rootBoundary;
          _context10.t9 = strategy;
          _context10.t10 = {
            element: _context10.t6,
            boundary: _context10.t7,
            rootBoundary: _context10.t8,
            strategy: _context10.t9
          };
          _context10.next = 33;
          return _context10.t1.getClippingRect.call(_context10.t1, _context10.t10);
        case 33:
          _context10.t11 = _context10.sent;
          clippingClientRect = (0, _context10.t0)(_context10.t11);
          rect = elementContext === 'floating' ? {
            x: x,
            y: y,
            width: rects.floating.width,
            height: rects.floating.height
          } : rects.reference;
          _context10.next = 38;
          return platform.getOffsetParent == null ? void 0 : platform.getOffsetParent(elements.floating);
        case 38:
          offsetParent = _context10.sent;
          _context10.next = 41;
          return platform.isElement == null ? void 0 : platform.isElement(offsetParent);
        case 41:
          if (!_context10.sent) {
            _context10.next = 50;
            break;
          }
          _context10.next = 44;
          return platform.getScale == null ? void 0 : platform.getScale(offsetParent);
        case 44:
          _context10.t13 = _context10.sent;
          if (_context10.t13) {
            _context10.next = 47;
            break;
          }
          _context10.t13 = {
            x: 1,
            y: 1
          };
        case 47:
          _context10.t12 = _context10.t13;
          _context10.next = 51;
          break;
        case 50:
          _context10.t12 = {
            x: 1,
            y: 1
          };
        case 51:
          offsetScale = _context10.t12;
          _context10.t14 = rectToClientRect;
          if (!platform.convertOffsetParentRelativeRectToViewportRelativeRect) {
            _context10.next = 59;
            break;
          }
          _context10.next = 56;
          return platform.convertOffsetParentRelativeRectToViewportRelativeRect({
            elements: elements,
            rect: rect,
            offsetParent: offsetParent,
            strategy: strategy
          });
        case 56:
          _context10.t15 = _context10.sent;
          _context10.next = 60;
          break;
        case 59:
          _context10.t15 = rect;
        case 60:
          _context10.t16 = _context10.t15;
          elementClientRect = (0, _context10.t14)(_context10.t16);
          return _context10.abrupt("return", {
            top: (clippingClientRect.top - elementClientRect.top + paddingObject.top) / offsetScale.y,
            bottom: (elementClientRect.bottom - clippingClientRect.bottom + paddingObject.bottom) / offsetScale.y,
            left: (clippingClientRect.left - elementClientRect.left + paddingObject.left) / offsetScale.x,
            right: (elementClientRect.right - clippingClientRect.right + paddingObject.right) / offsetScale.x
          });
        case 63:
        case "end":
          return _context10.stop();
      }
    }, _callee10);
  }));
  return _detectOverflow.apply(this, arguments);
}
var arrow$1 = function arrow(options) {
  return {
    name: 'arrow',
    options: options,
    fn: function fn(state) {
      return _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee2() {
        var x, y, placement, rects, platform, elements, middlewareData, _ref3, element, _ref3$padding, padding, paddingObject, coords, axis, length, arrowDimensions, isYAxis, minProp, maxProp, clientProp, endDiff, startDiff, arrowOffsetParent, clientSize, centerToReference, largestPossiblePadding, minPadding, maxPadding, min$1, max, center, offset, shouldAddOffset, alignmentOffset;
        return _regeneratorRuntime().wrap(function _callee2$(_context2) {
          while (1) switch (_context2.prev = _context2.next) {
            case 0:
              x = state.x, y = state.y, placement = state.placement, rects = state.rects, platform = state.platform, elements = state.elements, middlewareData = state.middlewareData; // Since `element` is required, we don't Partial<> the type.
              _ref3 = evaluate(options, state) || {}, element = _ref3.element, _ref3$padding = _ref3.padding, padding = _ref3$padding === void 0 ? 0 : _ref3$padding;
              if (!(element == null)) {
                _context2.next = 4;
                break;
              }
              return _context2.abrupt("return", {});
            case 4:
              paddingObject = getPaddingObject(padding);
              coords = {
                x: x,
                y: y
              };
              axis = getAlignmentAxis(placement);
              length = getAxisLength(axis);
              _context2.next = 10;
              return platform.getDimensions(element);
            case 10:
              arrowDimensions = _context2.sent;
              isYAxis = axis === 'y';
              minProp = isYAxis ? 'top' : 'left';
              maxProp = isYAxis ? 'bottom' : 'right';
              clientProp = isYAxis ? 'clientHeight' : 'clientWidth';
              endDiff = rects.reference[length] + rects.reference[axis] - coords[axis] - rects.floating[length];
              startDiff = coords[axis] - rects.reference[axis];
              _context2.next = 19;
              return platform.getOffsetParent == null ? void 0 : platform.getOffsetParent(element);
            case 19:
              arrowOffsetParent = _context2.sent;
              clientSize = arrowOffsetParent ? arrowOffsetParent[clientProp] : 0; // DOM platform can return `window` as the `offsetParent`.
              _context2.t0 = !clientSize;
              if (_context2.t0) {
                _context2.next = 26;
                break;
              }
              _context2.next = 25;
              return platform.isElement == null ? void 0 : platform.isElement(arrowOffsetParent);
            case 25:
              _context2.t0 = !_context2.sent;
            case 26:
              if (!_context2.t0) {
                _context2.next = 28;
                break;
              }
              clientSize = elements.floating[clientProp] || rects.floating[length];
            case 28:
              centerToReference = endDiff / 2 - startDiff / 2; // If the padding is large enough that it causes the arrow to no longer be
              // centered, modify the padding so that it is centered.
              largestPossiblePadding = clientSize / 2 - arrowDimensions[length] / 2 - 1;
              minPadding = min(paddingObject[minProp], largestPossiblePadding);
              maxPadding = min(paddingObject[maxProp], largestPossiblePadding); // Make sure the arrow doesn't overflow the floating element if the center
              // point is outside the floating element's bounds.
              min$1 = minPadding;
              max = clientSize - arrowDimensions[length] - maxPadding;
              center = clientSize / 2 - arrowDimensions[length] / 2 + centerToReference;
              offset = clamp(min$1, center, max); // If the reference is small enough that the arrow's padding causes it to
              // to point to nothing for an aligned placement, adjust the offset of the
              // floating element itself. To ensure `shift()` continues to take action,
              // a single reset is performed when this is true.
              shouldAddOffset = !middlewareData.arrow && getAlignment(placement) != null && center !== offset && rects.reference[length] / 2 - (center < min$1 ? minPadding : maxPadding) - arrowDimensions[length] / 2 < 0;
              alignmentOffset = shouldAddOffset ? center < min$1 ? center - min$1 : center - max : 0;
              return _context2.abrupt("return", _defineProperty(_defineProperty(_defineProperty({}, axis, coords[axis] + alignmentOffset), "data", _objectSpread2(_defineProperty(_defineProperty({}, axis, offset), "centerOffset", center - offset - alignmentOffset), shouldAddOffset && {
                alignmentOffset: alignmentOffset
              })), "reset", shouldAddOffset));
            case 39:
            case "end":
              return _context2.stop();
          }
        }, _callee2);
      }))();
    }
  };
};
function getPlacementList(alignment, autoAlignment, allowedPlacements) {
  var allowedPlacementsSortedByAlignment = alignment ? [].concat(_toConsumableArray(allowedPlacements.filter(function (placement) {
    return getAlignment(placement) === alignment;
  })), _toConsumableArray(allowedPlacements.filter(function (placement) {
    return getAlignment(placement) !== alignment;
  }))) : allowedPlacements.filter(function (placement) {
    return getSide(placement) === placement;
  });
  return allowedPlacementsSortedByAlignment.filter(function (placement) {
    if (alignment) {
      return getAlignment(placement) === alignment || (autoAlignment ? getOppositeAlignmentPlacement(placement) !== placement : false);
    }
    return true;
  });
}
/**
 * Optimizes the visibility of the floating element by choosing the placement
 * that has the most space available automatically, without needing to specify a
 * preferred placement. Alternative to `flip`.
 * @see https://floating-ui.com/docs/autoPlacement
 */
var autoPlacement$1 = function autoPlacement(options) {
  if (options === void 0) {
    options = {};
  }
  return {
    name: 'autoPlacement',
    options: options,
    fn: function fn(state) {
      return _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee3() {
        var _middlewareData$autoP, _middlewareData$autoP2, _placementsThatFitOnE, rects, middlewareData, placement, platform, elements, _evaluate, _evaluate$crossAxis, crossAxis, alignment, _evaluate$allowedPlac, allowedPlacements, _evaluate$autoAlignme, autoAlignment, detectOverflowOptions, placements$1, overflow, currentIndex, currentPlacement, alignmentSides, currentOverflows, allOverflows, nextPlacement, placementsSortedByMostSpace, placementsThatFitOnEachSide, resetPlacement;
        return _regeneratorRuntime().wrap(function _callee3$(_context3) {
          while (1) switch (_context3.prev = _context3.next) {
            case 0:
              rects = state.rects, middlewareData = state.middlewareData, placement = state.placement, platform = state.platform, elements = state.elements;
              _evaluate = evaluate(options, state), _evaluate$crossAxis = _evaluate.crossAxis, crossAxis = _evaluate$crossAxis === void 0 ? false : _evaluate$crossAxis, alignment = _evaluate.alignment, _evaluate$allowedPlac = _evaluate.allowedPlacements, allowedPlacements = _evaluate$allowedPlac === void 0 ? placements : _evaluate$allowedPlac, _evaluate$autoAlignme = _evaluate.autoAlignment, autoAlignment = _evaluate$autoAlignme === void 0 ? true : _evaluate$autoAlignme, detectOverflowOptions = _objectWithoutProperties(_evaluate, _excluded);
              placements$1 = alignment !== undefined || allowedPlacements === placements ? getPlacementList(alignment || null, autoAlignment, allowedPlacements) : allowedPlacements;
              _context3.next = 5;
              return detectOverflow$1(state, detectOverflowOptions);
            case 5:
              overflow = _context3.sent;
              currentIndex = ((_middlewareData$autoP = middlewareData.autoPlacement) == null ? void 0 : _middlewareData$autoP.index) || 0;
              currentPlacement = placements$1[currentIndex];
              if (!(currentPlacement == null)) {
                _context3.next = 10;
                break;
              }
              return _context3.abrupt("return", {});
            case 10:
              _context3.t0 = getAlignmentSides;
              _context3.t1 = currentPlacement;
              _context3.t2 = rects;
              _context3.next = 15;
              return platform.isRTL == null ? void 0 : platform.isRTL(elements.floating);
            case 15:
              _context3.t3 = _context3.sent;
              alignmentSides = (0, _context3.t0)(_context3.t1, _context3.t2, _context3.t3);
              if (!(placement !== currentPlacement)) {
                _context3.next = 19;
                break;
              }
              return _context3.abrupt("return", {
                reset: {
                  placement: placements$1[0]
                }
              });
            case 19:
              currentOverflows = [overflow[getSide(currentPlacement)], overflow[alignmentSides[0]], overflow[alignmentSides[1]]];
              allOverflows = [].concat(_toConsumableArray(((_middlewareData$autoP2 = middlewareData.autoPlacement) == null ? void 0 : _middlewareData$autoP2.overflows) || []), [{
                placement: currentPlacement,
                overflows: currentOverflows
              }]);
              nextPlacement = placements$1[currentIndex + 1]; // There are more placements to check.
              if (!nextPlacement) {
                _context3.next = 24;
                break;
              }
              return _context3.abrupt("return", {
                data: {
                  index: currentIndex + 1,
                  overflows: allOverflows
                },
                reset: {
                  placement: nextPlacement
                }
              });
            case 24:
              placementsSortedByMostSpace = allOverflows.map(function (d) {
                var alignment = getAlignment(d.placement);
                return [d.placement, alignment && crossAxis ?
                // Check along the mainAxis and main crossAxis side.
                d.overflows.slice(0, 2).reduce(function (acc, v) {
                  return acc + v;
                }, 0) :
                // Check only the mainAxis.
                d.overflows[0], d.overflows];
              }).sort(function (a, b) {
                return a[1] - b[1];
              });
              placementsThatFitOnEachSide = placementsSortedByMostSpace.filter(function (d) {
                return d[2].slice(0,
                // Aligned placements should not check their opposite crossAxis
                // side.
                getAlignment(d[0]) ? 2 : 3).every(function (v) {
                  return v <= 0;
                });
              });
              resetPlacement = ((_placementsThatFitOnE = placementsThatFitOnEachSide[0]) == null ? void 0 : _placementsThatFitOnE[0]) || placementsSortedByMostSpace[0][0];
              if (!(resetPlacement !== placement)) {
                _context3.next = 29;
                break;
              }
              return _context3.abrupt("return", {
                data: {
                  index: currentIndex + 1,
                  overflows: allOverflows
                },
                reset: {
                  placement: resetPlacement
                }
              });
            case 29:
              return _context3.abrupt("return", {});
            case 30:
            case "end":
              return _context3.stop();
          }
        }, _callee3);
      }))();
    }
  };
};

/**
 * Optimizes the visibility of the floating element by flipping the `placement`
 * in order to keep it in view when the preferred placement(s) will overflow the
 * clipping boundary. Alternative to `autoPlacement`.
 * @see https://floating-ui.com/docs/flip
 */
var flip$1 = function flip(options) {
  if (options === void 0) {
    options = {};
  }
  return {
    name: 'flip',
    options: options,
    fn: function fn(state) {
      return _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee4() {
        var _middlewareData$arrow, _middlewareData$flip, placement, middlewareData, rects, initialPlacement, platform, elements, _evaluate2, _evaluate2$mainAxis, checkMainAxis, _evaluate2$crossAxis, checkCrossAxis, specifiedFallbackPlacements, _evaluate2$fallbackSt, fallbackStrategy, _evaluate2$fallbackAx, fallbackAxisSideDirection, _evaluate2$flipAlignm, flipAlignment, detectOverflowOptions, side, isBasePlacement, rtl, fallbackPlacements, placements, overflow, overflows, overflowsData, _sides, _middlewareData$flip2, _overflowsData$filter, nextIndex, nextPlacement, resetPlacement, _overflowsData$map$so, _placement;
        return _regeneratorRuntime().wrap(function _callee4$(_context4) {
          while (1) switch (_context4.prev = _context4.next) {
            case 0:
              placement = state.placement, middlewareData = state.middlewareData, rects = state.rects, initialPlacement = state.initialPlacement, platform = state.platform, elements = state.elements;
              _evaluate2 = evaluate(options, state), _evaluate2$mainAxis = _evaluate2.mainAxis, checkMainAxis = _evaluate2$mainAxis === void 0 ? true : _evaluate2$mainAxis, _evaluate2$crossAxis = _evaluate2.crossAxis, checkCrossAxis = _evaluate2$crossAxis === void 0 ? true : _evaluate2$crossAxis, specifiedFallbackPlacements = _evaluate2.fallbackPlacements, _evaluate2$fallbackSt = _evaluate2.fallbackStrategy, fallbackStrategy = _evaluate2$fallbackSt === void 0 ? 'bestFit' : _evaluate2$fallbackSt, _evaluate2$fallbackAx = _evaluate2.fallbackAxisSideDirection, fallbackAxisSideDirection = _evaluate2$fallbackAx === void 0 ? 'none' : _evaluate2$fallbackAx, _evaluate2$flipAlignm = _evaluate2.flipAlignment, flipAlignment = _evaluate2$flipAlignm === void 0 ? true : _evaluate2$flipAlignm, detectOverflowOptions = _objectWithoutProperties(_evaluate2, _excluded2); // If a reset by the arrow was caused due to an alignment offset being
              // added, we should skip any logic now since `flip()` has already done its
              // work.
              // https://github.com/floating-ui/floating-ui/issues/2549#issuecomment-1719601643
              if (!((_middlewareData$arrow = middlewareData.arrow) != null && _middlewareData$arrow.alignmentOffset)) {
                _context4.next = 4;
                break;
              }
              return _context4.abrupt("return", {});
            case 4:
              side = getSide(placement);
              isBasePlacement = getSide(initialPlacement) === initialPlacement;
              _context4.next = 8;
              return platform.isRTL == null ? void 0 : platform.isRTL(elements.floating);
            case 8:
              rtl = _context4.sent;
              fallbackPlacements = specifiedFallbackPlacements || (isBasePlacement || !flipAlignment ? [getOppositePlacement(initialPlacement)] : getExpandedPlacements(initialPlacement));
              if (!specifiedFallbackPlacements && fallbackAxisSideDirection !== 'none') {
                fallbackPlacements.push.apply(fallbackPlacements, _toConsumableArray(getOppositeAxisPlacements(initialPlacement, flipAlignment, fallbackAxisSideDirection, rtl)));
              }
              placements = [initialPlacement].concat(_toConsumableArray(fallbackPlacements));
              _context4.next = 14;
              return detectOverflow$1(state, detectOverflowOptions);
            case 14:
              overflow = _context4.sent;
              overflows = [];
              overflowsData = ((_middlewareData$flip = middlewareData.flip) == null ? void 0 : _middlewareData$flip.overflows) || [];
              if (checkMainAxis) {
                overflows.push(overflow[side]);
              }
              if (checkCrossAxis) {
                _sides = getAlignmentSides(placement, rects, rtl);
                overflows.push(overflow[_sides[0]], overflow[_sides[1]]);
              }
              overflowsData = [].concat(_toConsumableArray(overflowsData), [{
                placement: placement,
                overflows: overflows
              }]);

              // One or more sides is overflowing.
              if (overflows.every(function (side) {
                return side <= 0;
              })) {
                _context4.next = 37;
                break;
              }
              nextIndex = (((_middlewareData$flip2 = middlewareData.flip) == null ? void 0 : _middlewareData$flip2.index) || 0) + 1;
              nextPlacement = placements[nextIndex];
              if (!nextPlacement) {
                _context4.next = 25;
                break;
              }
              return _context4.abrupt("return", {
                data: {
                  index: nextIndex,
                  overflows: overflowsData
                },
                reset: {
                  placement: nextPlacement
                }
              });
            case 25:
              // First, find the candidates that fit on the mainAxis side of overflow,
              // then find the placement that fits the best on the main crossAxis side.
              resetPlacement = (_overflowsData$filter = overflowsData.filter(function (d) {
                return d.overflows[0] <= 0;
              }).sort(function (a, b) {
                return a.overflows[1] - b.overflows[1];
              })[0]) == null ? void 0 : _overflowsData$filter.placement; // Otherwise fallback.
              if (resetPlacement) {
                _context4.next = 35;
                break;
              }
              _context4.t0 = fallbackStrategy;
              _context4.next = _context4.t0 === 'bestFit' ? 30 : _context4.t0 === 'initialPlacement' ? 33 : 35;
              break;
            case 30:
              _placement = (_overflowsData$map$so = overflowsData.map(function (d) {
                return [d.placement, d.overflows.filter(function (overflow) {
                  return overflow > 0;
                }).reduce(function (acc, overflow) {
                  return acc + overflow;
                }, 0)];
              }).sort(function (a, b) {
                return a[1] - b[1];
              })[0]) == null ? void 0 : _overflowsData$map$so[0];
              if (_placement) {
                resetPlacement = _placement;
              }
              return _context4.abrupt("break", 35);
            case 33:
              resetPlacement = initialPlacement;
              return _context4.abrupt("break", 35);
            case 35:
              if (!(placement !== resetPlacement)) {
                _context4.next = 37;
                break;
              }
              return _context4.abrupt("return", {
                reset: {
                  placement: resetPlacement
                }
              });
            case 37:
              return _context4.abrupt("return", {});
            case 38:
            case "end":
              return _context4.stop();
          }
        }, _callee4);
      }))();
    }
  };
};
function getSideOffsets(overflow, rect) {
  return {
    top: overflow.top - rect.height,
    right: overflow.right - rect.width,
    bottom: overflow.bottom - rect.height,
    left: overflow.left - rect.width
  };
}
function isAnySideFullyClipped(overflow) {
  return sides.some(function (side) {
    return overflow[side] >= 0;
  });
}
/**
 * Provides data to hide the floating element in applicable situations, such as
 * when it is not in the same clipping context as the reference element.
 * @see https://floating-ui.com/docs/hide
 */
var hide$1 = function hide(options) {
  if (options === void 0) {
    options = {};
  }
  return {
    name: 'hide',
    options: options,
    fn: function fn(state) {
      return _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee5() {
        var rects, _evaluate3, _evaluate3$strategy, strategy, detectOverflowOptions, overflow, offsets, _overflow, _offsets;
        return _regeneratorRuntime().wrap(function _callee5$(_context5) {
          while (1) switch (_context5.prev = _context5.next) {
            case 0:
              rects = state.rects;
              _evaluate3 = evaluate(options, state), _evaluate3$strategy = _evaluate3.strategy, strategy = _evaluate3$strategy === void 0 ? 'referenceHidden' : _evaluate3$strategy, detectOverflowOptions = _objectWithoutProperties(_evaluate3, _excluded3);
              _context5.t0 = strategy;
              _context5.next = _context5.t0 === 'referenceHidden' ? 5 : _context5.t0 === 'escaped' ? 10 : 15;
              break;
            case 5:
              _context5.next = 7;
              return detectOverflow$1(state, _objectSpread2(_objectSpread2({}, detectOverflowOptions), {}, {
                elementContext: 'reference'
              }));
            case 7:
              overflow = _context5.sent;
              offsets = getSideOffsets(overflow, rects.reference);
              return _context5.abrupt("return", {
                data: {
                  referenceHiddenOffsets: offsets,
                  referenceHidden: isAnySideFullyClipped(offsets)
                }
              });
            case 10:
              _context5.next = 12;
              return detectOverflow$1(state, _objectSpread2(_objectSpread2({}, detectOverflowOptions), {}, {
                altBoundary: true
              }));
            case 12:
              _overflow = _context5.sent;
              _offsets = getSideOffsets(_overflow, rects.floating);
              return _context5.abrupt("return", {
                data: {
                  escapedOffsets: _offsets,
                  escaped: isAnySideFullyClipped(_offsets)
                }
              });
            case 15:
              return _context5.abrupt("return", {});
            case 16:
            case "end":
              return _context5.stop();
          }
        }, _callee5);
      }))();
    }
  };
};
function getBoundingRect(rects) {
  var minX = min.apply(void 0, _toConsumableArray(rects.map(function (rect) {
    return rect.left;
  })));
  var minY = min.apply(void 0, _toConsumableArray(rects.map(function (rect) {
    return rect.top;
  })));
  var maxX = max.apply(void 0, _toConsumableArray(rects.map(function (rect) {
    return rect.right;
  })));
  var maxY = max.apply(void 0, _toConsumableArray(rects.map(function (rect) {
    return rect.bottom;
  })));
  return {
    x: minX,
    y: minY,
    width: maxX - minX,
    height: maxY - minY
  };
}
function getRectsByLine(rects) {
  var sortedRects = rects.slice().sort(function (a, b) {
    return a.y - b.y;
  });
  var groups = [];
  var prevRect = null;
  for (var i = 0; i < sortedRects.length; i++) {
    var rect = sortedRects[i];
    if (!prevRect || rect.y - prevRect.y > prevRect.height / 2) {
      groups.push([rect]);
    } else {
      groups[groups.length - 1].push(rect);
    }
    prevRect = rect;
  }
  return groups.map(function (rect) {
    return rectToClientRect(getBoundingRect(rect));
  });
}
/**
 * Provides improved positioning for inline reference elements that can span
 * over multiple lines, such as hyperlinks or range selections.
 * @see https://floating-ui.com/docs/inline
 */
var inline$1 = function inline(options) {
  if (options === void 0) {
    options = {};
  }
  return {
    name: 'inline',
    options: options,
    fn: function fn(state) {
      return _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee6() {
        var placement, elements, rects, platform, strategy, _evaluate4, _evaluate4$padding, padding, x, y, nativeClientRects, clientRects, fallback, paddingObject, getBoundingClientRect, resetRects;
        return _regeneratorRuntime().wrap(function _callee6$(_context6) {
          while (1) switch (_context6.prev = _context6.next) {
            case 0:
              getBoundingClientRect = function _getBoundingClientRec() {
                // There are two rects and they are disjoined.
                if (clientRects.length === 2 && clientRects[0].left > clientRects[1].right && x != null && y != null) {
                  // Find the first rect in which the point is fully inside.
                  return clientRects.find(function (rect) {
                    return x > rect.left - paddingObject.left && x < rect.right + paddingObject.right && y > rect.top - paddingObject.top && y < rect.bottom + paddingObject.bottom;
                  }) || fallback;
                }

                // There are 2 or more connected rects.
                if (clientRects.length >= 2) {
                  if (getSideAxis(placement) === 'y') {
                    var firstRect = clientRects[0];
                    var lastRect = clientRects[clientRects.length - 1];
                    var isTop = getSide(placement) === 'top';
                    var _top = firstRect.top;
                    var _bottom = lastRect.bottom;
                    var _left = isTop ? firstRect.left : lastRect.left;
                    var _right = isTop ? firstRect.right : lastRect.right;
                    var _width = _right - _left;
                    var _height = _bottom - _top;
                    return {
                      top: _top,
                      bottom: _bottom,
                      left: _left,
                      right: _right,
                      width: _width,
                      height: _height,
                      x: _left,
                      y: _top
                    };
                  }
                  var isLeftSide = getSide(placement) === 'left';
                  var maxRight = max.apply(void 0, _toConsumableArray(clientRects.map(function (rect) {
                    return rect.right;
                  })));
                  var minLeft = min.apply(void 0, _toConsumableArray(clientRects.map(function (rect) {
                    return rect.left;
                  })));
                  var measureRects = clientRects.filter(function (rect) {
                    return isLeftSide ? rect.left === minLeft : rect.right === maxRight;
                  });
                  var top = measureRects[0].top;
                  var bottom = measureRects[measureRects.length - 1].bottom;
                  var left = minLeft;
                  var right = maxRight;
                  var width = right - left;
                  var height = bottom - top;
                  return {
                    top: top,
                    bottom: bottom,
                    left: left,
                    right: right,
                    width: width,
                    height: height,
                    x: left,
                    y: top
                  };
                }
                return fallback;
              };
              placement = state.placement, elements = state.elements, rects = state.rects, platform = state.platform, strategy = state.strategy; // A MouseEvent's client{X,Y} coords can be up to 2 pixels off a
              // ClientRect's bounds, despite the event listener being triggered. A
              // padding of 2 seems to handle this issue.
              _evaluate4 = evaluate(options, state), _evaluate4$padding = _evaluate4.padding, padding = _evaluate4$padding === void 0 ? 2 : _evaluate4$padding, x = _evaluate4.x, y = _evaluate4.y;
              _context6.t0 = Array;
              _context6.next = 6;
              return platform.getClientRects == null ? void 0 : platform.getClientRects(elements.reference);
            case 6:
              _context6.t1 = _context6.sent;
              if (_context6.t1) {
                _context6.next = 9;
                break;
              }
              _context6.t1 = [];
            case 9:
              _context6.t2 = _context6.t1;
              nativeClientRects = _context6.t0.from.call(_context6.t0, _context6.t2);
              clientRects = getRectsByLine(nativeClientRects);
              fallback = rectToClientRect(getBoundingRect(nativeClientRects));
              paddingObject = getPaddingObject(padding);
              _context6.next = 16;
              return platform.getElementRects({
                reference: {
                  getBoundingClientRect: getBoundingClientRect
                },
                floating: elements.floating,
                strategy: strategy
              });
            case 16:
              resetRects = _context6.sent;
              if (!(rects.reference.x !== resetRects.reference.x || rects.reference.y !== resetRects.reference.y || rects.reference.width !== resetRects.reference.width || rects.reference.height !== resetRects.reference.height)) {
                _context6.next = 19;
                break;
              }
              return _context6.abrupt("return", {
                reset: {
                  rects: resetRects
                }
              });
            case 19:
              return _context6.abrupt("return", {});
            case 20:
            case "end":
              return _context6.stop();
          }
        }, _callee6);
      }))();
    }
  };
};

// For type backwards-compatibility, the `OffsetOptions` type was also
// Derivable.
function convertValueToCoords(_x6, _x7) {
  return _convertValueToCoords.apply(this, arguments);
}
/**
 * Modifies the placement by translating the floating element along the
 * specified axes.
 * A number (shorthand for `mainAxis` or distance), or an axes configuration
 * object may be passed.
 * @see https://floating-ui.com/docs/offset
 */
function _convertValueToCoords() {
  _convertValueToCoords = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee11(state, options) {
    var placement, platform, elements, rtl, side, alignment, isVertical, mainAxisMulti, crossAxisMulti, rawValue, _ref6, mainAxis, crossAxis, alignmentAxis;
    return _regeneratorRuntime().wrap(function _callee11$(_context11) {
      while (1) switch (_context11.prev = _context11.next) {
        case 0:
          placement = state.placement, platform = state.platform, elements = state.elements;
          _context11.next = 3;
          return platform.isRTL == null ? void 0 : platform.isRTL(elements.floating);
        case 3:
          rtl = _context11.sent;
          side = getSide(placement);
          alignment = getAlignment(placement);
          isVertical = getSideAxis(placement) === 'y';
          mainAxisMulti = ['left', 'top'].includes(side) ? -1 : 1;
          crossAxisMulti = rtl && isVertical ? -1 : 1;
          rawValue = evaluate(options, state); // eslint-disable-next-line prefer-const
          _ref6 = typeof rawValue === 'number' ? {
            mainAxis: rawValue,
            crossAxis: 0,
            alignmentAxis: null
          } : _objectSpread2({
            mainAxis: 0,
            crossAxis: 0,
            alignmentAxis: null
          }, rawValue), mainAxis = _ref6.mainAxis, crossAxis = _ref6.crossAxis, alignmentAxis = _ref6.alignmentAxis;
          if (alignment && typeof alignmentAxis === 'number') {
            crossAxis = alignment === 'end' ? alignmentAxis * -1 : alignmentAxis;
          }
          return _context11.abrupt("return", isVertical ? {
            x: crossAxis * crossAxisMulti,
            y: mainAxis * mainAxisMulti
          } : {
            x: mainAxis * mainAxisMulti,
            y: crossAxis * crossAxisMulti
          });
        case 13:
        case "end":
          return _context11.stop();
      }
    }, _callee11);
  }));
  return _convertValueToCoords.apply(this, arguments);
}
var offset$1 = function offset(options) {
  if (options === void 0) {
    options = 0;
  }
  return {
    name: 'offset',
    options: options,
    fn: function fn(state) {
      return _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee7() {
        var _middlewareData$offse, _middlewareData$arrow, x, y, placement, middlewareData, diffCoords;
        return _regeneratorRuntime().wrap(function _callee7$(_context7) {
          while (1) switch (_context7.prev = _context7.next) {
            case 0:
              x = state.x, y = state.y, placement = state.placement, middlewareData = state.middlewareData;
              _context7.next = 3;
              return convertValueToCoords(state, options);
            case 3:
              diffCoords = _context7.sent;
              if (!(placement === ((_middlewareData$offse = middlewareData.offset) == null ? void 0 : _middlewareData$offse.placement) && (_middlewareData$arrow = middlewareData.arrow) != null && _middlewareData$arrow.alignmentOffset)) {
                _context7.next = 6;
                break;
              }
              return _context7.abrupt("return", {});
            case 6:
              return _context7.abrupt("return", {
                x: x + diffCoords.x,
                y: y + diffCoords.y,
                data: _objectSpread2(_objectSpread2({}, diffCoords), {}, {
                  placement: placement
                })
              });
            case 7:
            case "end":
              return _context7.stop();
          }
        }, _callee7);
      }))();
    }
  };
};

/**
 * Optimizes the visibility of the floating element by shifting it in order to
 * keep it in view when it will overflow the clipping boundary.
 * @see https://floating-ui.com/docs/shift
 */
var shift$1 = function shift(options) {
  if (options === void 0) {
    options = {};
  }
  return {
    name: 'shift',
    options: options,
    fn: function fn(state) {
      return _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee8() {
        var x, y, placement, _evaluate5, _evaluate5$mainAxis, checkMainAxis, _evaluate5$crossAxis, checkCrossAxis, _evaluate5$limiter, limiter, detectOverflowOptions, coords, overflow, crossAxis, mainAxis, mainAxisCoord, crossAxisCoord, minSide, maxSide, _min, _max, _minSide, _maxSide, _min2, _max2, limitedCoords;
        return _regeneratorRuntime().wrap(function _callee8$(_context8) {
          while (1) switch (_context8.prev = _context8.next) {
            case 0:
              x = state.x, y = state.y, placement = state.placement;
              _evaluate5 = evaluate(options, state), _evaluate5$mainAxis = _evaluate5.mainAxis, checkMainAxis = _evaluate5$mainAxis === void 0 ? true : _evaluate5$mainAxis, _evaluate5$crossAxis = _evaluate5.crossAxis, checkCrossAxis = _evaluate5$crossAxis === void 0 ? false : _evaluate5$crossAxis, _evaluate5$limiter = _evaluate5.limiter, limiter = _evaluate5$limiter === void 0 ? {
                fn: function fn(_ref) {
                  var x = _ref.x,
                    y = _ref.y;
                  return {
                    x: x,
                    y: y
                  };
                }
              } : _evaluate5$limiter, detectOverflowOptions = _objectWithoutProperties(_evaluate5, _excluded4);
              coords = {
                x: x,
                y: y
              };
              _context8.next = 5;
              return detectOverflow$1(state, detectOverflowOptions);
            case 5:
              overflow = _context8.sent;
              crossAxis = getSideAxis(getSide(placement));
              mainAxis = getOppositeAxis(crossAxis);
              mainAxisCoord = coords[mainAxis];
              crossAxisCoord = coords[crossAxis];
              if (checkMainAxis) {
                minSide = mainAxis === 'y' ? 'top' : 'left';
                maxSide = mainAxis === 'y' ? 'bottom' : 'right';
                _min = mainAxisCoord + overflow[minSide];
                _max = mainAxisCoord - overflow[maxSide];
                mainAxisCoord = clamp(_min, mainAxisCoord, _max);
              }
              if (checkCrossAxis) {
                _minSide = crossAxis === 'y' ? 'top' : 'left';
                _maxSide = crossAxis === 'y' ? 'bottom' : 'right';
                _min2 = crossAxisCoord + overflow[_minSide];
                _max2 = crossAxisCoord - overflow[_maxSide];
                crossAxisCoord = clamp(_min2, crossAxisCoord, _max2);
              }
              limitedCoords = limiter.fn(_objectSpread2(_objectSpread2({}, state), {}, _defineProperty(_defineProperty({}, mainAxis, mainAxisCoord), crossAxis, crossAxisCoord)));
              return _context8.abrupt("return", _objectSpread2(_objectSpread2({}, limitedCoords), {}, {
                data: {
                  x: limitedCoords.x - x,
                  y: limitedCoords.y - y
                }
              }));
            case 14:
            case "end":
              return _context8.stop();
          }
        }, _callee8);
      }))();
    }
  };
};
/**
 * Built-in `limiter` that will stop `shift()` at a certain point.
 */
var limitShift$1 = function limitShift(options) {
  if (options === void 0) {
    options = {};
  }
  return {
    options: options,
    fn: function fn(state) {
      var x = state.x,
        y = state.y,
        placement = state.placement,
        rects = state.rects,
        middlewareData = state.middlewareData;
      var _evaluate6 = evaluate(options, state),
        _evaluate6$offset = _evaluate6.offset,
        offset = _evaluate6$offset === void 0 ? 0 : _evaluate6$offset,
        _evaluate6$mainAxis = _evaluate6.mainAxis,
        checkMainAxis = _evaluate6$mainAxis === void 0 ? true : _evaluate6$mainAxis,
        _evaluate6$crossAxis = _evaluate6.crossAxis,
        checkCrossAxis = _evaluate6$crossAxis === void 0 ? true : _evaluate6$crossAxis;
      var coords = {
        x: x,
        y: y
      };
      var crossAxis = getSideAxis(placement);
      var mainAxis = getOppositeAxis(crossAxis);
      var mainAxisCoord = coords[mainAxis];
      var crossAxisCoord = coords[crossAxis];
      var rawOffset = evaluate(offset, state);
      var computedOffset = typeof rawOffset === 'number' ? {
        mainAxis: rawOffset,
        crossAxis: 0
      } : _objectSpread2({
        mainAxis: 0,
        crossAxis: 0
      }, rawOffset);
      if (checkMainAxis) {
        var len = mainAxis === 'y' ? 'height' : 'width';
        var limitMin = rects.reference[mainAxis] - rects.floating[len] + computedOffset.mainAxis;
        var limitMax = rects.reference[mainAxis] + rects.reference[len] - computedOffset.mainAxis;
        if (mainAxisCoord < limitMin) {
          mainAxisCoord = limitMin;
        } else if (mainAxisCoord > limitMax) {
          mainAxisCoord = limitMax;
        }
      }
      if (checkCrossAxis) {
        var _middlewareData$offse, _middlewareData$offse2;
        var _len = mainAxis === 'y' ? 'width' : 'height';
        var isOriginSide = ['top', 'left'].includes(getSide(placement));
        var _limitMin = rects.reference[crossAxis] - rects.floating[_len] + (isOriginSide ? ((_middlewareData$offse = middlewareData.offset) == null ? void 0 : _middlewareData$offse[crossAxis]) || 0 : 0) + (isOriginSide ? 0 : computedOffset.crossAxis);
        var _limitMax = rects.reference[crossAxis] + rects.reference[_len] + (isOriginSide ? 0 : ((_middlewareData$offse2 = middlewareData.offset) == null ? void 0 : _middlewareData$offse2[crossAxis]) || 0) - (isOriginSide ? computedOffset.crossAxis : 0);
        if (crossAxisCoord < _limitMin) {
          crossAxisCoord = _limitMin;
        } else if (crossAxisCoord > _limitMax) {
          crossAxisCoord = _limitMax;
        }
      }
      return _defineProperty(_defineProperty({}, mainAxis, mainAxisCoord), crossAxis, crossAxisCoord);
    }
  };
};

/**
 * Provides data that allows you to change the size of the floating element —
 * for instance, prevent it from overflowing the clipping boundary or match the
 * width of the reference element.
 * @see https://floating-ui.com/docs/size
 */
var size$1 = function size(options) {
  if (options === void 0) {
    options = {};
  }
  return {
    name: 'size',
    options: options,
    fn: function fn(state) {
      return _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee9() {
        var placement, rects, platform, elements, _evaluate7, _evaluate7$apply, apply, detectOverflowOptions, overflow, side, alignment, isYAxis, _rects$floating, width, height, heightSide, widthSide, maximumClippingHeight, maximumClippingWidth, overflowAvailableHeight, overflowAvailableWidth, noShift, availableHeight, availableWidth, xMin, xMax, yMin, yMax, nextDimensions;
        return _regeneratorRuntime().wrap(function _callee9$(_context9) {
          while (1) switch (_context9.prev = _context9.next) {
            case 0:
              placement = state.placement, rects = state.rects, platform = state.platform, elements = state.elements;
              _evaluate7 = evaluate(options, state), _evaluate7$apply = _evaluate7.apply, apply = _evaluate7$apply === void 0 ? function () {} : _evaluate7$apply, detectOverflowOptions = _objectWithoutProperties(_evaluate7, _excluded5);
              _context9.next = 4;
              return detectOverflow$1(state, detectOverflowOptions);
            case 4:
              overflow = _context9.sent;
              side = getSide(placement);
              alignment = getAlignment(placement);
              isYAxis = getSideAxis(placement) === 'y';
              _rects$floating = rects.floating, width = _rects$floating.width, height = _rects$floating.height;
              if (!(side === 'top' || side === 'bottom')) {
                _context9.next = 28;
                break;
              }
              heightSide = side;
              _context9.t0 = alignment;
              _context9.next = 14;
              return platform.isRTL == null ? void 0 : platform.isRTL(elements.floating);
            case 14:
              if (!_context9.sent) {
                _context9.next = 18;
                break;
              }
              _context9.t1 = 'start';
              _context9.next = 19;
              break;
            case 18:
              _context9.t1 = 'end';
            case 19:
              _context9.t2 = _context9.t1;
              if (!(_context9.t0 === _context9.t2)) {
                _context9.next = 24;
                break;
              }
              _context9.t3 = 'left';
              _context9.next = 25;
              break;
            case 24:
              _context9.t3 = 'right';
            case 25:
              widthSide = _context9.t3;
              _context9.next = 30;
              break;
            case 28:
              widthSide = side;
              heightSide = alignment === 'end' ? 'top' : 'bottom';
            case 30:
              maximumClippingHeight = height - overflow.top - overflow.bottom;
              maximumClippingWidth = width - overflow.left - overflow.right;
              overflowAvailableHeight = min(height - overflow[heightSide], maximumClippingHeight);
              overflowAvailableWidth = min(width - overflow[widthSide], maximumClippingWidth);
              noShift = !state.middlewareData.shift;
              availableHeight = overflowAvailableHeight;
              availableWidth = overflowAvailableWidth;
              if (isYAxis) {
                availableWidth = alignment || noShift ? min(overflowAvailableWidth, maximumClippingWidth) : maximumClippingWidth;
              } else {
                availableHeight = alignment || noShift ? min(overflowAvailableHeight, maximumClippingHeight) : maximumClippingHeight;
              }
              if (noShift && !alignment) {
                xMin = max(overflow.left, 0);
                xMax = max(overflow.right, 0);
                yMin = max(overflow.top, 0);
                yMax = max(overflow.bottom, 0);
                if (isYAxis) {
                  availableWidth = width - 2 * (xMin !== 0 || xMax !== 0 ? xMin + xMax : max(overflow.left, overflow.right));
                } else {
                  availableHeight = height - 2 * (yMin !== 0 || yMax !== 0 ? yMin + yMax : max(overflow.top, overflow.bottom));
                }
              }
              _context9.next = 41;
              return apply(_objectSpread2(_objectSpread2({}, state), {}, {
                availableWidth: availableWidth,
                availableHeight: availableHeight
              }));
            case 41:
              _context9.next = 43;
              return platform.getDimensions(elements.floating);
            case 43:
              nextDimensions = _context9.sent;
              if (!(width !== nextDimensions.width || height !== nextDimensions.height)) {
                _context9.next = 46;
                break;
              }
              return _context9.abrupt("return", {
                reset: {
                  rects: true
                }
              });
            case 46:
              return _context9.abrupt("return", {});
            case 47:
            case "end":
              return _context9.stop();
          }
        }, _callee9);
      }))();
    }
  };
};

function getNodeName(node) {
  if (isNode(node)) {
    return (node.nodeName || '').toLowerCase();
  }
  // Mocked nodes in testing environments may not be instances of Node. By
  // returning `#document` an infinite loop won't occur.
  // https://github.com/floating-ui/floating-ui/issues/2317
  return '#document';
}
function getWindow(node) {
  var _node$ownerDocument;
  return (node == null || (_node$ownerDocument = node.ownerDocument) == null ? void 0 : _node$ownerDocument.defaultView) || window;
}
function getDocumentElement(node) {
  var _ref;
  return (_ref = (isNode(node) ? node.ownerDocument : node.document) || window.document) == null ? void 0 : _ref.documentElement;
}
function isNode(value) {
  return value instanceof Node || value instanceof getWindow(value).Node;
}
function isElement(value) {
  return value instanceof Element || value instanceof getWindow(value).Element;
}
function isHTMLElement(value) {
  return value instanceof HTMLElement || value instanceof getWindow(value).HTMLElement;
}
function isShadowRoot(value) {
  // Browsers without `ShadowRoot` support.
  if (typeof ShadowRoot === 'undefined') {
    return false;
  }
  return value instanceof ShadowRoot || value instanceof getWindow(value).ShadowRoot;
}
function isOverflowElement(element) {
  var _getComputedStyle = getComputedStyle(element),
    overflow = _getComputedStyle.overflow,
    overflowX = _getComputedStyle.overflowX,
    overflowY = _getComputedStyle.overflowY,
    display = _getComputedStyle.display;
  return /auto|scroll|overlay|hidden|clip/.test(overflow + overflowY + overflowX) && !['inline', 'contents'].includes(display);
}
function isTableElement(element) {
  return ['table', 'td', 'th'].includes(getNodeName(element));
}
function isContainingBlock(element) {
  var webkit = isWebKit();
  var css = getComputedStyle(element);

  // https://developer.mozilla.org/en-US/docs/Web/CSS/Containing_block#identifying_the_containing_block
  return css.transform !== 'none' || css.perspective !== 'none' || (css.containerType ? css.containerType !== 'normal' : false) || !webkit && (css.backdropFilter ? css.backdropFilter !== 'none' : false) || !webkit && (css.filter ? css.filter !== 'none' : false) || ['transform', 'perspective', 'filter'].some(function (value) {
    return (css.willChange || '').includes(value);
  }) || ['paint', 'layout', 'strict', 'content'].some(function (value) {
    return (css.contain || '').includes(value);
  });
}
function getContainingBlock(element) {
  var currentNode = getParentNode(element);
  while (isHTMLElement(currentNode) && !isLastTraversableNode(currentNode)) {
    if (isContainingBlock(currentNode)) {
      return currentNode;
    }
    currentNode = getParentNode(currentNode);
  }
  return null;
}
function isWebKit() {
  if (typeof CSS === 'undefined' || !CSS.supports) return false;
  return CSS.supports('-webkit-backdrop-filter', 'none');
}
function isLastTraversableNode(node) {
  return ['html', 'body', '#document'].includes(getNodeName(node));
}
function getComputedStyle(element) {
  return getWindow(element).getComputedStyle(element);
}
function getNodeScroll(element) {
  if (isElement(element)) {
    return {
      scrollLeft: element.scrollLeft,
      scrollTop: element.scrollTop
    };
  }
  return {
    scrollLeft: element.pageXOffset,
    scrollTop: element.pageYOffset
  };
}
function getParentNode(node) {
  if (getNodeName(node) === 'html') {
    return node;
  }
  var result =
  // Step into the shadow DOM of the parent of a slotted node.
  node.assignedSlot ||
  // DOM Element detected.
  node.parentNode ||
  // ShadowRoot detected.
  isShadowRoot(node) && node.host ||
  // Fallback.
  getDocumentElement(node);
  return isShadowRoot(result) ? result.host : result;
}
function getNearestOverflowAncestor(node) {
  var parentNode = getParentNode(node);
  if (isLastTraversableNode(parentNode)) {
    return node.ownerDocument ? node.ownerDocument.body : node.body;
  }
  if (isHTMLElement(parentNode) && isOverflowElement(parentNode)) {
    return parentNode;
  }
  return getNearestOverflowAncestor(parentNode);
}
function getOverflowAncestors(node, list, traverseIframes) {
  var _node$ownerDocument2;
  if (list === void 0) {
    list = [];
  }
  if (traverseIframes === void 0) {
    traverseIframes = true;
  }
  var scrollableAncestor = getNearestOverflowAncestor(node);
  var isBody = scrollableAncestor === ((_node$ownerDocument2 = node.ownerDocument) == null ? void 0 : _node$ownerDocument2.body);
  var win = getWindow(scrollableAncestor);
  if (isBody) {
    return list.concat(win, win.visualViewport || [], isOverflowElement(scrollableAncestor) ? scrollableAncestor : [], win.frameElement && traverseIframes ? getOverflowAncestors(win.frameElement) : []);
  }
  return list.concat(scrollableAncestor, getOverflowAncestors(scrollableAncestor, [], traverseIframes));
}

function getCssDimensions(element) {
  var css = getComputedStyle(element);
  // In testing environments, the `width` and `height` properties are empty
  // strings for SVG elements, returning NaN. Fallback to `0` in this case.
  var width = parseFloat(css.width) || 0;
  var height = parseFloat(css.height) || 0;
  var hasOffset = isHTMLElement(element);
  var offsetWidth = hasOffset ? element.offsetWidth : width;
  var offsetHeight = hasOffset ? element.offsetHeight : height;
  var shouldFallback = round(width) !== offsetWidth || round(height) !== offsetHeight;
  if (shouldFallback) {
    width = offsetWidth;
    height = offsetHeight;
  }
  return {
    width: width,
    height: height,
    $: shouldFallback
  };
}
function unwrapElement(element) {
  return !isElement(element) ? element.contextElement : element;
}
function getScale(element) {
  var domElement = unwrapElement(element);
  if (!isHTMLElement(domElement)) {
    return createCoords(1);
  }
  var rect = domElement.getBoundingClientRect();
  var _getCssDimensions = getCssDimensions(domElement),
    width = _getCssDimensions.width,
    height = _getCssDimensions.height,
    $ = _getCssDimensions.$;
  var x = ($ ? round(rect.width) : rect.width) / width;
  var y = ($ ? round(rect.height) : rect.height) / height;

  // 0, NaN, or Infinity should always fallback to 1.

  if (!x || !Number.isFinite(x)) {
    x = 1;
  }
  if (!y || !Number.isFinite(y)) {
    y = 1;
  }
  return {
    x: x,
    y: y
  };
}
var noOffsets = /*#__PURE__*/createCoords(0);
function getVisualOffsets(element) {
  var win = getWindow(element);
  if (!isWebKit() || !win.visualViewport) {
    return noOffsets;
  }
  return {
    x: win.visualViewport.offsetLeft,
    y: win.visualViewport.offsetTop
  };
}
function shouldAddVisualOffsets(element, isFixed, floatingOffsetParent) {
  if (isFixed === void 0) {
    isFixed = false;
  }
  if (!floatingOffsetParent || isFixed && floatingOffsetParent !== getWindow(element)) {
    return false;
  }
  return isFixed;
}
function getBoundingClientRect(element, includeScale, isFixedStrategy, offsetParent) {
  if (includeScale === void 0) {
    includeScale = false;
  }
  if (isFixedStrategy === void 0) {
    isFixedStrategy = false;
  }
  var clientRect = element.getBoundingClientRect();
  var domElement = unwrapElement(element);
  var scale = createCoords(1);
  if (includeScale) {
    if (offsetParent) {
      if (isElement(offsetParent)) {
        scale = getScale(offsetParent);
      }
    } else {
      scale = getScale(element);
    }
  }
  var visualOffsets = shouldAddVisualOffsets(domElement, isFixedStrategy, offsetParent) ? getVisualOffsets(domElement) : createCoords(0);
  var x = (clientRect.left + visualOffsets.x) / scale.x;
  var y = (clientRect.top + visualOffsets.y) / scale.y;
  var width = clientRect.width / scale.x;
  var height = clientRect.height / scale.y;
  if (domElement) {
    var win = getWindow(domElement);
    var offsetWin = offsetParent && isElement(offsetParent) ? getWindow(offsetParent) : offsetParent;
    var currentWin = win;
    var currentIFrame = currentWin.frameElement;
    while (currentIFrame && offsetParent && offsetWin !== currentWin) {
      var iframeScale = getScale(currentIFrame);
      var iframeRect = currentIFrame.getBoundingClientRect();
      var css = getComputedStyle(currentIFrame);
      var left = iframeRect.left + (currentIFrame.clientLeft + parseFloat(css.paddingLeft)) * iframeScale.x;
      var top = iframeRect.top + (currentIFrame.clientTop + parseFloat(css.paddingTop)) * iframeScale.y;
      x *= iframeScale.x;
      y *= iframeScale.y;
      width *= iframeScale.x;
      height *= iframeScale.y;
      x += left;
      y += top;
      currentWin = getWindow(currentIFrame);
      currentIFrame = currentWin.frameElement;
    }
  }
  return rectToClientRect({
    width: width,
    height: height,
    x: x,
    y: y
  });
}
var topLayerSelectors = [':popover-open', ':modal'];
function isTopLayer(element) {
  return topLayerSelectors.some(function (selector) {
    try {
      return element.matches(selector);
    } catch (e) {
      return false;
    }
  });
}
function convertOffsetParentRelativeRectToViewportRelativeRect(_ref) {
  var elements = _ref.elements,
    rect = _ref.rect,
    offsetParent = _ref.offsetParent,
    strategy = _ref.strategy;
  var isFixed = strategy === 'fixed';
  var documentElement = getDocumentElement(offsetParent);
  var topLayer = elements ? isTopLayer(elements.floating) : false;
  if (offsetParent === documentElement || topLayer && isFixed) {
    return rect;
  }
  var scroll = {
    scrollLeft: 0,
    scrollTop: 0
  };
  var scale = createCoords(1);
  var offsets = createCoords(0);
  var isOffsetParentAnElement = isHTMLElement(offsetParent);
  if (isOffsetParentAnElement || !isOffsetParentAnElement && !isFixed) {
    if (getNodeName(offsetParent) !== 'body' || isOverflowElement(documentElement)) {
      scroll = getNodeScroll(offsetParent);
    }
    if (isHTMLElement(offsetParent)) {
      var offsetRect = getBoundingClientRect(offsetParent);
      scale = getScale(offsetParent);
      offsets.x = offsetRect.x + offsetParent.clientLeft;
      offsets.y = offsetRect.y + offsetParent.clientTop;
    }
  }
  return {
    width: rect.width * scale.x,
    height: rect.height * scale.y,
    x: rect.x * scale.x - scroll.scrollLeft * scale.x + offsets.x,
    y: rect.y * scale.y - scroll.scrollTop * scale.y + offsets.y
  };
}
function getClientRects(element) {
  return Array.from(element.getClientRects());
}
function getWindowScrollBarX(element) {
  // If <html> has a CSS width greater than the viewport, then this will be
  // incorrect for RTL.
  return getBoundingClientRect(getDocumentElement(element)).left + getNodeScroll(element).scrollLeft;
}

// Gets the entire size of the scrollable document area, even extending outside
// of the `<html>` and `<body>` rect bounds if horizontally scrollable.
function getDocumentRect(element) {
  var html = getDocumentElement(element);
  var scroll = getNodeScroll(element);
  var body = element.ownerDocument.body;
  var width = max(html.scrollWidth, html.clientWidth, body.scrollWidth, body.clientWidth);
  var height = max(html.scrollHeight, html.clientHeight, body.scrollHeight, body.clientHeight);
  var x = -scroll.scrollLeft + getWindowScrollBarX(element);
  var y = -scroll.scrollTop;
  if (getComputedStyle(body).direction === 'rtl') {
    x += max(html.clientWidth, body.clientWidth) - width;
  }
  return {
    width: width,
    height: height,
    x: x,
    y: y
  };
}
function getViewportRect(element, strategy) {
  var win = getWindow(element);
  var html = getDocumentElement(element);
  var visualViewport = win.visualViewport;
  var width = html.clientWidth;
  var height = html.clientHeight;
  var x = 0;
  var y = 0;
  if (visualViewport) {
    width = visualViewport.width;
    height = visualViewport.height;
    var visualViewportBased = isWebKit();
    if (!visualViewportBased || visualViewportBased && strategy === 'fixed') {
      x = visualViewport.offsetLeft;
      y = visualViewport.offsetTop;
    }
  }
  return {
    width: width,
    height: height,
    x: x,
    y: y
  };
}

// Returns the inner client rect, subtracting scrollbars if present.
function getInnerBoundingClientRect(element, strategy) {
  var clientRect = getBoundingClientRect(element, true, strategy === 'fixed');
  var top = clientRect.top + element.clientTop;
  var left = clientRect.left + element.clientLeft;
  var scale = isHTMLElement(element) ? getScale(element) : createCoords(1);
  var width = element.clientWidth * scale.x;
  var height = element.clientHeight * scale.y;
  var x = left * scale.x;
  var y = top * scale.y;
  return {
    width: width,
    height: height,
    x: x,
    y: y
  };
}
function getClientRectFromClippingAncestor(element, clippingAncestor, strategy) {
  var rect;
  if (clippingAncestor === 'viewport') {
    rect = getViewportRect(element, strategy);
  } else if (clippingAncestor === 'document') {
    rect = getDocumentRect(getDocumentElement(element));
  } else if (isElement(clippingAncestor)) {
    rect = getInnerBoundingClientRect(clippingAncestor, strategy);
  } else {
    var visualOffsets = getVisualOffsets(element);
    rect = _objectSpread2(_objectSpread2({}, clippingAncestor), {}, {
      x: clippingAncestor.x - visualOffsets.x,
      y: clippingAncestor.y - visualOffsets.y
    });
  }
  return rectToClientRect(rect);
}
function hasFixedPositionAncestor(element, stopNode) {
  var parentNode = getParentNode(element);
  if (parentNode === stopNode || !isElement(parentNode) || isLastTraversableNode(parentNode)) {
    return false;
  }
  return getComputedStyle(parentNode).position === 'fixed' || hasFixedPositionAncestor(parentNode, stopNode);
}

// A "clipping ancestor" is an `overflow` element with the characteristic of
// clipping (or hiding) child elements. This returns all clipping ancestors
// of the given element up the tree.
function getClippingElementAncestors(element, cache) {
  var cachedResult = cache.get(element);
  if (cachedResult) {
    return cachedResult;
  }
  var result = getOverflowAncestors(element, [], false).filter(function (el) {
    return isElement(el) && getNodeName(el) !== 'body';
  });
  var currentContainingBlockComputedStyle = null;
  var elementIsFixed = getComputedStyle(element).position === 'fixed';
  var currentNode = elementIsFixed ? getParentNode(element) : element;

  // https://developer.mozilla.org/en-US/docs/Web/CSS/Containing_block#identifying_the_containing_block
  while (isElement(currentNode) && !isLastTraversableNode(currentNode)) {
    var computedStyle = getComputedStyle(currentNode);
    var currentNodeIsContaining = isContainingBlock(currentNode);
    if (!currentNodeIsContaining && computedStyle.position === 'fixed') {
      currentContainingBlockComputedStyle = null;
    }
    var shouldDropCurrentNode = elementIsFixed ? !currentNodeIsContaining && !currentContainingBlockComputedStyle : !currentNodeIsContaining && computedStyle.position === 'static' && !!currentContainingBlockComputedStyle && ['absolute', 'fixed'].includes(currentContainingBlockComputedStyle.position) || isOverflowElement(currentNode) && !currentNodeIsContaining && hasFixedPositionAncestor(element, currentNode);
    if (shouldDropCurrentNode) {
      // Drop non-containing blocks.
      result = result.filter(function (ancestor) {
        return ancestor !== currentNode;
      });
    } else {
      // Record last containing block for next iteration.
      currentContainingBlockComputedStyle = computedStyle;
    }
    currentNode = getParentNode(currentNode);
  }
  cache.set(element, result);
  return result;
}

// Gets the maximum area that the element is visible in due to any number of
// clipping ancestors.
function getClippingRect(_ref) {
  var element = _ref.element,
    boundary = _ref.boundary,
    rootBoundary = _ref.rootBoundary,
    strategy = _ref.strategy;
  var elementClippingAncestors = boundary === 'clippingAncestors' ? isTopLayer(element) ? [] : getClippingElementAncestors(element, this._c) : [].concat(boundary);
  var clippingAncestors = [].concat(_toConsumableArray(elementClippingAncestors), [rootBoundary]);
  var firstClippingAncestor = clippingAncestors[0];
  var clippingRect = clippingAncestors.reduce(function (accRect, clippingAncestor) {
    var rect = getClientRectFromClippingAncestor(element, clippingAncestor, strategy);
    accRect.top = max(rect.top, accRect.top);
    accRect.right = min(rect.right, accRect.right);
    accRect.bottom = min(rect.bottom, accRect.bottom);
    accRect.left = max(rect.left, accRect.left);
    return accRect;
  }, getClientRectFromClippingAncestor(element, firstClippingAncestor, strategy));
  return {
    width: clippingRect.right - clippingRect.left,
    height: clippingRect.bottom - clippingRect.top,
    x: clippingRect.left,
    y: clippingRect.top
  };
}
function getDimensions(element) {
  var _getCssDimensions2 = getCssDimensions(element),
    width = _getCssDimensions2.width,
    height = _getCssDimensions2.height;
  return {
    width: width,
    height: height
  };
}
function getRectRelativeToOffsetParent(element, offsetParent, strategy) {
  var isOffsetParentAnElement = isHTMLElement(offsetParent);
  var documentElement = getDocumentElement(offsetParent);
  var isFixed = strategy === 'fixed';
  var rect = getBoundingClientRect(element, true, isFixed, offsetParent);
  var scroll = {
    scrollLeft: 0,
    scrollTop: 0
  };
  var offsets = createCoords(0);
  if (isOffsetParentAnElement || !isOffsetParentAnElement && !isFixed) {
    if (getNodeName(offsetParent) !== 'body' || isOverflowElement(documentElement)) {
      scroll = getNodeScroll(offsetParent);
    }
    if (isOffsetParentAnElement) {
      var offsetRect = getBoundingClientRect(offsetParent, true, isFixed, offsetParent);
      offsets.x = offsetRect.x + offsetParent.clientLeft;
      offsets.y = offsetRect.y + offsetParent.clientTop;
    } else if (documentElement) {
      offsets.x = getWindowScrollBarX(documentElement);
    }
  }
  var x = rect.left + scroll.scrollLeft - offsets.x;
  var y = rect.top + scroll.scrollTop - offsets.y;
  return {
    x: x,
    y: y,
    width: rect.width,
    height: rect.height
  };
}
function isStaticPositioned(element) {
  return getComputedStyle(element).position === 'static';
}
function getTrueOffsetParent(element, polyfill) {
  if (!isHTMLElement(element) || getComputedStyle(element).position === 'fixed') {
    return null;
  }
  if (polyfill) {
    return polyfill(element);
  }
  return element.offsetParent;
}

// Gets the closest ancestor positioned element. Handles some edge cases,
// such as table ancestors and cross browser bugs.
function getOffsetParent(element, polyfill) {
  var win = getWindow(element);
  if (isTopLayer(element)) {
    return win;
  }
  if (!isHTMLElement(element)) {
    var svgOffsetParent = getParentNode(element);
    while (svgOffsetParent && !isLastTraversableNode(svgOffsetParent)) {
      if (isElement(svgOffsetParent) && !isStaticPositioned(svgOffsetParent)) {
        return svgOffsetParent;
      }
      svgOffsetParent = getParentNode(svgOffsetParent);
    }
    return win;
  }
  var offsetParent = getTrueOffsetParent(element, polyfill);
  while (offsetParent && isTableElement(offsetParent) && isStaticPositioned(offsetParent)) {
    offsetParent = getTrueOffsetParent(offsetParent, polyfill);
  }
  if (offsetParent && isLastTraversableNode(offsetParent) && isStaticPositioned(offsetParent) && !isContainingBlock(offsetParent)) {
    return win;
  }
  return offsetParent || getContainingBlock(element) || win;
}
var getElementRects = /*#__PURE__*/function () {
  var _ref2 = _asyncToGenerator( /*#__PURE__*/_regeneratorRuntime().mark(function _callee(data) {
    var getOffsetParentFn, getDimensionsFn, floatingDimensions;
    return _regeneratorRuntime().wrap(function _callee$(_context) {
      while (1) switch (_context.prev = _context.next) {
        case 0:
          getOffsetParentFn = this.getOffsetParent || getOffsetParent;
          getDimensionsFn = this.getDimensions;
          _context.next = 4;
          return getDimensionsFn(data.floating);
        case 4:
          floatingDimensions = _context.sent;
          _context.t0 = getRectRelativeToOffsetParent;
          _context.t1 = data.reference;
          _context.next = 9;
          return getOffsetParentFn(data.floating);
        case 9:
          _context.t2 = _context.sent;
          _context.t3 = data.strategy;
          _context.t4 = (0, _context.t0)(_context.t1, _context.t2, _context.t3);
          _context.t5 = {
            x: 0,
            y: 0,
            width: floatingDimensions.width,
            height: floatingDimensions.height
          };
          return _context.abrupt("return", {
            reference: _context.t4,
            floating: _context.t5
          });
        case 14:
        case "end":
          return _context.stop();
      }
    }, _callee, this);
  }));
  return function getElementRects(_x) {
    return _ref2.apply(this, arguments);
  };
}();
function isRTL(element) {
  return getComputedStyle(element).direction === 'rtl';
}
var platform = {
  convertOffsetParentRelativeRectToViewportRelativeRect: convertOffsetParentRelativeRectToViewportRelativeRect,
  getDocumentElement: getDocumentElement,
  getClippingRect: getClippingRect,
  getOffsetParent: getOffsetParent,
  getElementRects: getElementRects,
  getClientRects: getClientRects,
  getDimensions: getDimensions,
  getScale: getScale,
  isElement: isElement,
  isRTL: isRTL
};

// https://samthor.au/2021/observing-dom/
function observeMove(element, onMove) {
  var io = null;
  var timeoutId;
  var root = getDocumentElement(element);
  function cleanup() {
    var _io;
    clearTimeout(timeoutId);
    (_io = io) == null || _io.disconnect();
    io = null;
  }
  function refresh(skip, threshold) {
    if (skip === void 0) {
      skip = false;
    }
    if (threshold === void 0) {
      threshold = 1;
    }
    cleanup();
    var _element$getBoundingC = element.getBoundingClientRect(),
      left = _element$getBoundingC.left,
      top = _element$getBoundingC.top,
      width = _element$getBoundingC.width,
      height = _element$getBoundingC.height;
    if (!skip) {
      onMove();
    }
    if (!width || !height) {
      return;
    }
    var insetTop = floor(top);
    var insetRight = floor(root.clientWidth - (left + width));
    var insetBottom = floor(root.clientHeight - (top + height));
    var insetLeft = floor(left);
    var rootMargin = -insetTop + "px " + -insetRight + "px " + -insetBottom + "px " + -insetLeft + "px";
    var options = {
      rootMargin: rootMargin,
      threshold: max(0, min(1, threshold)) || 1
    };
    var isFirstUpdate = true;
    function handleObserve(entries) {
      var ratio = entries[0].intersectionRatio;
      if (ratio !== threshold) {
        if (!isFirstUpdate) {
          return refresh();
        }
        if (!ratio) {
          // If the reference is clipped, the ratio is 0. Throttle the refresh
          // to prevent an infinite loop of updates.
          timeoutId = setTimeout(function () {
            refresh(false, 1e-7);
          }, 1000);
        } else {
          refresh(false, ratio);
        }
      }
      isFirstUpdate = false;
    }

    // Older browsers don't support a `document` as the root and will throw an
    // error.
    try {
      io = new IntersectionObserver(handleObserve, _objectSpread2(_objectSpread2({}, options), {}, {
        // Handle <iframe>s
        root: root.ownerDocument
      }));
    } catch (e) {
      io = new IntersectionObserver(handleObserve, options);
    }
    io.observe(element);
  }
  refresh(true);
  return cleanup;
}

/**
 * Automatically updates the position of the floating element when necessary.
 * Should only be called when the floating element is mounted on the DOM or
 * visible on the screen.
 * @returns cleanup function that should be invoked when the floating element is
 * removed from the DOM or hidden from the screen.
 * @see https://floating-ui.com/docs/autoUpdate
 */
function autoUpdate(reference, floating, update, options) {
  if (options === void 0) {
    options = {};
  }
  var _options = options,
    _options$ancestorScro = _options.ancestorScroll,
    ancestorScroll = _options$ancestorScro === void 0 ? true : _options$ancestorScro,
    _options$ancestorResi = _options.ancestorResize,
    ancestorResize = _options$ancestorResi === void 0 ? true : _options$ancestorResi,
    _options$elementResiz = _options.elementResize,
    elementResize = _options$elementResiz === void 0 ? typeof ResizeObserver === 'function' : _options$elementResiz,
    _options$layoutShift = _options.layoutShift,
    layoutShift = _options$layoutShift === void 0 ? typeof IntersectionObserver === 'function' : _options$layoutShift,
    _options$animationFra = _options.animationFrame,
    animationFrame = _options$animationFra === void 0 ? false : _options$animationFra;
  var referenceEl = unwrapElement(reference);
  var ancestors = ancestorScroll || ancestorResize ? [].concat(_toConsumableArray(referenceEl ? getOverflowAncestors(referenceEl) : []), _toConsumableArray(getOverflowAncestors(floating))) : [];
  ancestors.forEach(function (ancestor) {
    ancestorScroll && ancestor.addEventListener('scroll', update, {
      passive: true
    });
    ancestorResize && ancestor.addEventListener('resize', update);
  });
  var cleanupIo = referenceEl && layoutShift ? observeMove(referenceEl, update) : null;
  var reobserveFrame = -1;
  var resizeObserver = null;
  if (elementResize) {
    resizeObserver = new ResizeObserver(function (_ref) {
      var _ref3 = _slicedToArray(_ref, 1),
        firstEntry = _ref3[0];
      if (firstEntry && firstEntry.target === referenceEl && resizeObserver) {
        // Prevent update loops when using the `size` middleware.
        // https://github.com/floating-ui/floating-ui/issues/1740
        resizeObserver.unobserve(floating);
        cancelAnimationFrame(reobserveFrame);
        reobserveFrame = requestAnimationFrame(function () {
          var _resizeObserver;
          (_resizeObserver = resizeObserver) == null || _resizeObserver.observe(floating);
        });
      }
      update();
    });
    if (referenceEl && !animationFrame) {
      resizeObserver.observe(referenceEl);
    }
    resizeObserver.observe(floating);
  }
  var frameId;
  var prevRefRect = animationFrame ? getBoundingClientRect(reference) : null;
  if (animationFrame) {
    frameLoop();
  }
  function frameLoop() {
    var nextRefRect = getBoundingClientRect(reference);
    if (prevRefRect && (nextRefRect.x !== prevRefRect.x || nextRefRect.y !== prevRefRect.y || nextRefRect.width !== prevRefRect.width || nextRefRect.height !== prevRefRect.height)) {
      update();
    }
    prevRefRect = nextRefRect;
    frameId = requestAnimationFrame(frameLoop);
  }
  update();
  return function () {
    var _resizeObserver2;
    ancestors.forEach(function (ancestor) {
      ancestorScroll && ancestor.removeEventListener('scroll', update);
      ancestorResize && ancestor.removeEventListener('resize', update);
    });
    cleanupIo == null || cleanupIo();
    (_resizeObserver2 = resizeObserver) == null || _resizeObserver2.disconnect();
    resizeObserver = null;
    if (animationFrame) {
      cancelAnimationFrame(frameId);
    }
  };
}

/**
 * Resolves with an object of overflow side offsets that determine how much the
 * element is overflowing a given clipping boundary on each side.
 * - positive = overflowing the boundary by that number of pixels
 * - negative = how many pixels left before it will overflow
 * - 0 = lies flush with the boundary
 * @see https://floating-ui.com/docs/detectOverflow
 */
var detectOverflow = detectOverflow$1;

/**
 * Modifies the placement by translating the floating element along the
 * specified axes.
 * A number (shorthand for `mainAxis` or distance), or an axes configuration
 * object may be passed.
 * @see https://floating-ui.com/docs/offset
 */
var offset = offset$1;

/**
 * Optimizes the visibility of the floating element by choosing the placement
 * that has the most space available automatically, without needing to specify a
 * preferred placement. Alternative to `flip`.
 * @see https://floating-ui.com/docs/autoPlacement
 */
var autoPlacement = autoPlacement$1;

/**
 * Optimizes the visibility of the floating element by shifting it in order to
 * keep it in view when it will overflow the clipping boundary.
 * @see https://floating-ui.com/docs/shift
 */
var shift = shift$1;

/**
 * Optimizes the visibility of the floating element by flipping the `placement`
 * in order to keep it in view when the preferred placement(s) will overflow the
 * clipping boundary. Alternative to `autoPlacement`.
 * @see https://floating-ui.com/docs/flip
 */
var flip = flip$1;

/**
 * Provides data that allows you to change the size of the floating element —
 * for instance, prevent it from overflowing the clipping boundary or match the
 * width of the reference element.
 * @see https://floating-ui.com/docs/size
 */
var size = size$1;

/**
 * Provides data to hide the floating element in applicable situations, such as
 * when it is not in the same clipping context as the reference element.
 * @see https://floating-ui.com/docs/hide
 */
var hide = hide$1;

/**
 * Provides data to position an inner element of the floating element so that it
 * appears centered to the reference element.
 * @see https://floating-ui.com/docs/arrow
 */
var arrow = arrow$1;

/**
 * Provides improved positioning for inline reference elements that can span
 * over multiple lines, such as hyperlinks or range selections.
 * @see https://floating-ui.com/docs/inline
 */
var inline = inline$1;

/**
 * Built-in `limiter` that will stop `shift()` at a certain point.
 */
var limitShift = limitShift$1;

/**
 * Computes the `x` and `y` coordinates that will place the floating element
 * next to a given reference element.
 */
var computePosition = function computePosition(reference, floating, options) {
  // This caches the expensive `getClippingElementAncestors` function so that
  // multiple lifecycle resets re-use the same result. It only lives for a
  // single call. If other functions become expensive, we can add them as well.
  var cache = new Map();
  var mergedOptions = _objectSpread2({
    platform: platform
  }, options);
  var platformWithCache = _objectSpread2(_objectSpread2({}, mergedOptions.platform), {}, {
    _c: cache
  });
  return computePosition$1(reference, floating, _objectSpread2(_objectSpread2({}, mergedOptions), {}, {
    platform: platformWithCache
  }));
};

var FloatingUIDOM = /*#__PURE__*/Object.freeze({
  __proto__: null,
  arrow: arrow,
  autoPlacement: autoPlacement,
  autoUpdate: autoUpdate,
  computePosition: computePosition,
  detectOverflow: detectOverflow,
  flip: flip,
  getOverflowAncestors: getOverflowAncestors,
  hide: hide,
  inline: inline,
  limitShift: limitShift,
  offset: offset,
  platform: platform,
  shift: shift,
  size: size
});

globalThis.FloatingUIDOM = FloatingUIDOM;
