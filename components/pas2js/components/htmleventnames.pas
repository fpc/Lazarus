unit htmleventnames;

{$mode ObjFPC}
{$h+}

interface

Const
  sEventAbort = 'abort';
  SEventAnimationCancel = 'animationcancel';
  SEventAnimationEnd = 'animationend';
  SEventAnimationIteration = 'animationiteration';
  SEventAnimationStart = 'animationstart';
  sEventAuxClick = 'auxclick';
  sEventBlur = 'blur';
  SEventCancel = 'cancel';
  SEventCanPlay = 'canplay';
  SEventCanPlayThrough = 'canplaythrough';
  SEventChange = 'change';
  sEventClick = 'click';
  sEventCompositionEnd = 'compositionend';
  sEventCompositionStart = 'compositionstart';
  sEventCompositionUpdate = 'compositionupdate';
  sEventContextMenu = 'contextmenu';
  sEventCopy = 'copy';
  sEventCut = 'cut';
  sEventCueChange = 'cuechange';
  sEventDblClick = 'dblclick';
  sEventDurationChange = 'durationchange';
  sEventEnded  = 'ended';
  sEventError  = 'error';
  sEventFocus = 'focus';
  sEventFocusIn  = 'focusin';
  sEventFocusOut  = 'focusout';
  SEventGotPointerCapture = 'gotpointercapture';
  SEventInput = 'input';
  SEventInvalid = 'invalid';
  sEventKeyDown = 'keydown';
  sEventKeyPress = 'keypress';
  sEventKeyUp = 'keyup';
  sEventLoad = 'load';
  sEventLoadedData = 'loadeddata';
  sEventLoadedMetaData = 'loadedmetadata';
  sEventLoadend = 'loadend';
  sEventLoadStart = 'loadstart';
  SEventLostPointerCapture = 'lostpointercapture';
  sEventMouseDown = 'mousedown';
  sEventMouseEnter = 'mouseenter';
  sEventMouseLeave = 'mouseleave';
  sEventMouseMove = 'mousemove';
  sEventMouseOut = 'mouseout';
  sEventMouseUp = 'mouseup';
  sEventOverFlow = 'overflow';
  sEventPaste = 'paste';
  sEventPause = 'pause';
  sEventPlay = 'play';
  SEventPointerCancel = 'pointercancel';
  SEventPointerDown = 'pointerdown';
  SEventPointerEnter = 'pointerenter';
  SEventPointerLeave = 'pointerleave';
  SEventPointerMove = 'pointermove';
  SEventPointerOut = 'pointerout';
  SEventPointerOver = 'pointerover';
  SEventPointerUp = 'pointerup';
  sEventReset = 'reset';
  sEventResize = 'resize';
  sEventScroll = 'scroll';
  sEventSelect = 'select';
  sEventSubmit = 'submit';
  sEventTouchStart = 'touchstart';
  SEventTransitionCancel = 'transitioncancel';
  SEventTransitionEnd = 'transitionend';
  SEventTransitionRun = 'transitionrun';
  SEventTransitionStart = 'transitionstart';
  SEventWheel = 'wheel';

Type
  THTMLEvent = (
     heAbort,  heBlur, heCancel, heChange, heClick,
     heContextMenu, heCopy,  heCut, heDblClick, heError,
     heFocus, heInput, heInvalid, heKeyDown, heKeyPress,
     heKeyUp, heMouseDown, heMouseEnter, heMouseLeave, heMouseMove,
     heMouseOut, heMouseUp, heOverFlow,  hePaste,  heReset,
     heResize,  heScroll,  heSelect, heSubmit, heWheel);
  THTMLEvents = set of THTMLEvent;

Const
  HTMLEventNameArray : Array [THTMLEvent] of string = (
    sEventAbort, sEventBlur, SEventCancel, SEventChange, sEventClick ,
    sEventContextMenu, sEventCopy, sEventCut, sEventDblClick, sEventError,
    sEventFocus, SEventInput, SEventInvalid, sEventKeyDown, sEventKeyPress,
    sEventKeyUp, sEventMouseDown, sEventMouseEnter, sEventMouseLeave, sEventMouseMove,
    sEventMouseOut, sEventMouseUp, sEventOverFlow, sEventPaste, sEventReset,
    sEventResize, sEventScroll, sEventSelect, sEventSubmit, SEventWheel);

implementation

end.

