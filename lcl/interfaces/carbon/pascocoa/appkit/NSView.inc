{%mainunit appkit.pas}
(*
	NSView.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSVIEW_PAS_H}
{$define NSVIEW_PAS_H}

{$include NSResponder.inc}
{$include AppKitDefines.inc}
{$include NSGraphics.inc}

const
  NSViewNotSizable = 0;
  NSViewMinXMargin = 1;
  NSViewWidthSizable = 2;
  NSViewMaxXMargin = 4;
  NSViewMinYMargin = 8;
  NSViewHeightSizable = 16;
  NSViewMaxYMargin = 32;

type
  NSBorderType = (
    NSNoBorder = 0, 
    NSLineBorder = 1, 
    NSBezelBorder = 2, 
    NSGrooveBorder = 3
  );


type
  __VFlags = packed  record 
    _bitflags1 : LongWord;
    _bitflags2 : LongWord;
  end;
  _VFlags = __VFlags;

type
  NSTrackingRectTag = Integer;

type
  NSToolTipTag = Integer;

//APPKIT_EXTERN NSString *NSViewFrameDidChangeNotification;
//APPKIT_EXTERN NSString *NSViewFocusDidChangeNotification;
//APPKIT_EXTERN NSString *NSViewBoundsDidChangeNotification;
//APPKIT_EXTERN NSString *NSViewGlobalFrameDidChangeNotification;
{$endif}
{$endif}

{$ifdef FORWARD}
  NSView = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSVIEW_PAS_C}
{$define NSVIEW_PAS_C}

  TProcNSViewcompare0 = function (_param1: lobjc.id; _param2: lobjc.id; _param3: Pointer): Integer; cdecl;

{$include NSResponder.inc}
{$include AppKitDefines.inc}
{$include NSGraphics.inc}

  { NSView }

  NSView = class(NSResponder)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    constructor initWithFrame(_frameRect: NSRect);
    function window: lobjc.id;{NSWindow}
    function superview: lobjc.id;{NSView}
    function subviews: CFArrayRef;
    function isDescendantOf(_aView: lobjc.id {NSView}): LongBool;
    function ancestorSharedWithView(_aView: lobjc.id {NSView}): lobjc.id;{NSView}
    function opaqueAncestor: lobjc.id;{NSView}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure setHidden(_flag: LongBool);
    function isHidden: LongBool;
    function isHiddenOrHasHiddenAncestor: LongBool;
    procedure getRectsBeingDrawn_count(_rects: NSRect; _count: Integer);
    function needsToDrawRect(_aRect: NSRect): LongBool;
    function wantsDefaultClipping: LongBool;
    {.$endif}
    procedure addSubview(_aView: lobjc.id {NSView});
    procedure addSubview_positioned_relativeTo(_aView: lobjc.id {NSView}; _place: NSWindowOrderingMode; _otherView: lobjc.id {NSView});
    procedure sortSubviewsUsingFunction_context(_compare: TProcNSViewcompare0; _context: Pointer);
    procedure viewWillMoveToWindow(_newWindow: lobjc.id {NSWindow});
    procedure viewDidMoveToWindow;
    procedure viewWillMoveToSuperview(_newSuperview: lobjc.id {NSView});
    procedure viewDidMoveToSuperview;
    procedure didAddSubview(_subview: lobjc.id {NSView});
    procedure willRemoveSubview(_subview: lobjc.id {NSView});
    procedure removeFromSuperview;
    procedure replaceSubview_with(_oldView: lobjc.id {NSView}; _newView: lobjc.id {NSView});
    procedure removeFromSuperviewWithoutNeedingDisplay;
    procedure setPostsFrameChangedNotifications(_flag: LongBool);
    function postsFrameChangedNotifications: LongBool;
    procedure resizeSubviewsWithOldSize(_oldSize: NSSize);
    procedure resizeWithOldSuperviewSize(_oldSize: NSSize);
    procedure setAutoresizesSubviews(_flag: LongBool);
    function autoresizesSubviews: LongBool;
    procedure setAutoresizingMask(_mask: LongWord);
    function autoresizingMask: LongWord;
    procedure setFrameOrigin(_newOrigin: NSPoint);
    procedure setFrameSize(_newSize: NSSize);
    procedure setFrame(_frameRect: NSRect);
    function frame: NSRect;
    procedure setFrameRotation(_angle: Single);
    function frameRotation: Single;
    procedure setBoundsOrigin(_newOrigin: NSPoint);
    procedure setBoundsSize(_newSize: NSSize);
    procedure setBoundsRotation(_angle: Single);
    function boundsRotation: Single;
    procedure translateOriginToPoint(_translation: NSPoint);
    procedure scaleUnitSquareToSize(_newUnitSize: NSSize);
    procedure rotateByAngle(_angle: Single);
    procedure setBounds(_aRect: NSRect);
    function bounds: NSRect;
    function isFlipped: LongBool;
    function isRotatedFromBase: LongBool;
    function isRotatedOrScaledFromBase: LongBool;
    function isOpaque: LongBool;
    function convertPoint_fromView(_aPoint: NSPoint; _aView: lobjc.id {NSView}): NSPoint;
    function convertPoint_toView(_aPoint: NSPoint; _aView: lobjc.id {NSView}): NSPoint;
    function convertSize_fromView(_aSize: NSSize; _aView: lobjc.id {NSView}): NSSize;
    function convertSize_toView(_aSize: NSSize; _aView: lobjc.id {NSView}): NSSize;
    function convertRect_fromView(_aRect: NSRect; _aView: lobjc.id {NSView}): NSRect;
    function convertRect_toView(_aRect: NSRect; _aView: lobjc.id {NSView}): NSRect;
    function centerScanRect(_aRect: NSRect): NSRect;
    function canDraw: LongBool;
    procedure setNeedsDisplay(_flag: LongBool);
    procedure setNeedsDisplayInRect(_invalidRect: NSRect);
    function needsDisplay: LongBool;
    procedure lockFocus;
    procedure unlockFocus;
    function lockFocusIfCanDraw: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function lockFocusIfCanDrawInContext(_context: lobjc.id {NSGraphicsContext}): LongBool;
    {.$endif}
    constructor focusView;
    function visibleRect: NSRect;
    procedure display;
    procedure displayIfNeeded;
    procedure displayIfNeededIgnoringOpacity;
    procedure displayRect(_rect: NSRect);
    procedure displayIfNeededInRect(_rect: NSRect);
    procedure displayRectIgnoringOpacity(_rect: NSRect);
    procedure displayIfNeededInRectIgnoringOpacity(_rect: NSRect);
    procedure drawRect(_rect: NSRect);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    procedure displayRectIgnoringOpacity_inContext(_aRect: NSRect; _context: lobjc.id {NSGraphicsContext});
    function bitmapImageRepForCachingDisplayInRect(_rect: NSRect): lobjc.id;{NSBitmapImageRep}
    procedure cacheDisplayInRect_toBitmapImageRep(_rect: NSRect; _bitmapImageRep: lobjc.id {NSBitmapImageRep});
    {.$endif}
    function gState: Integer;
    procedure allocateGState;
    procedure releaseGState;
    procedure setUpGState;
    procedure renewGState;
    procedure scrollPoint(_aPoint: NSPoint);
    function scrollRectToVisible(_aRect: NSRect): LongBool;
    function autoscroll(_theEvent: lobjc.id {NSEvent}): LongBool;
    function adjustScroll(_newVisible: NSRect): NSRect;
    procedure scrollRect_by(_aRect: NSRect; _delta: NSSize);
    function hitTest(_aPoint: NSPoint): lobjc.id;{NSView}
    function mouse_inRect(_aPoint: NSPoint; _aRect: NSRect): LongBool;
    function viewWithTag(_aTag: Integer): lobjc.id;
    function tag: Integer;
    function performKeyEquivalent(_theEvent: lobjc.id {NSEvent}): LongBool;
    function acceptsFirstMouse(_theEvent: lobjc.id {NSEvent}): LongBool;
    function shouldDelayWindowOrderingForEvent(_theEvent: lobjc.id {NSEvent}): LongBool;
    function needsPanelToBecomeKey: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    function mouseDownCanMoveWindow: LongBool;
    {.$endif}
    procedure addCursorRect_cursor(_aRect: NSRect; _anObj: lobjc.id {NSCursor});
    procedure removeCursorRect_cursor(_aRect: NSRect; _anObj: lobjc.id {NSCursor});
    procedure discardCursorRects;
    procedure resetCursorRects;
    function addTrackingRect_owner_userData_assumeInside(_aRect: NSRect; _anObject: lobjc.id; _data: Pointer; _flag: LongBool): NSTrackingRectTag;
    procedure removeTrackingRect(_tag: NSTrackingRectTag);
    function shouldDrawColor: LongBool;
    procedure setPostsBoundsChangedNotifications(_flag: LongBool);
    function postsBoundsChangedNotifications: LongBool;
    function enclosingScrollView: lobjc.id;{NSScrollView}
    function menuForEvent(_event: lobjc.id {NSEvent}): lobjc.id;{NSMenu}
    class function defaultMenu: lobjc.id;{NSMenu}
    procedure setToolTip(__string: CFStringRef);
    function toolTip: CFStringRef;
    function addToolTipRect_owner_userData(_aRect: NSRect; _anObject: lobjc.id; _data: Pointer): NSToolTipTag;
    procedure removeToolTip(_tag: NSToolTipTag);
    procedure removeAllToolTips;
    //  Live resize support
    //  a view receives viewWillStartLiveResize before the frame is first changed for a
    // live resize
    procedure viewWillStartLiveResize;
    //  a view receives viewWillEndLiveResize after the frame is last changed for a live
    // resize
    procedure viewDidEndLiveResize;
    //  inLiveResize can be called from drawRect: to decide between cheap and full drawing
    function inLiveResize: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    //  A view that returns YES for -preservesContentDuringLiveResize is responsible for
    // invalidating its own dirty rects during live resize
    function preservesContentDuringLiveResize: LongBool;
    //  -rectPreservedDuringLiveResize indicates the rect the view previously occupied,
    // in the current coordinate system of the view
    function rectPreservedDuringLiveResize: NSRect;
    //  On return from -getRectsExposedDuringLiveResize, exposedRects indicates the parts
    // of the view that are newly exposed (at most 4 rects).  *count indicates how many
    // rects are in the exposedRects list
    procedure getRectsExposedDuringLiveResize;
    {.$endif}
    function performMnemonic(_theString: CFStringRef): LongBool;
    procedure setNextKeyView(_next: lobjc.id {NSView});
    function nextKeyView: lobjc.id;{NSView}
    function previousKeyView: lobjc.id;{NSView}
    function nextValidKeyView: lobjc.id;{NSView}
    function previousValidKeyView: lobjc.id;{NSView}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    function canBecomeKeyView: LongBool;
    {.$endif}
    procedure setKeyboardFocusRingNeedsDisplayInRect(_rect: NSRect);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure setFocusRingType(_focusRingType: NSFocusRingType);
    function focusRingType: NSFocusRingType;
    class function defaultFocusRingType: NSFocusRingType;
    {.$endif}
    //  EPS/PDF generation
    procedure writeEPSInsideRect_toPasteboard(_rect: NSRect; _pasteboard: lobjc.id {NSPasteboard});
    function dataWithEPSInsideRect(_rect: NSRect): CFDataRef;
    procedure writePDFInsideRect_toPasteboard(_rect: NSRect; _pasteboard: lobjc.id {NSPasteboard});
    function dataWithPDFInsideRect(_rect: NSRect): CFDataRef;
    //  Printing action method (Note fax: is obsolete)
    procedure print(_sender: lobjc.id);
    //  Pagination
    function knowsPageRange(_range: NSRangePointer): LongBool;
    function heightAdjustLimit: Single;
    function widthAdjustLimit: Single;
    procedure adjustPageWidthNew_left_right_limit(_newRight: Single; _oldLeft: Single; _oldRight: Single; _rightLimit: Single);
    procedure adjustPageHeightNew_top_bottom_limit(_newBottom: Single; _oldTop: Single; _oldBottom: Single; _bottomLimit: Single);
    function rectForPage(_page: Integer): NSRect;
    function locationOfPrintRect(_aRect: NSRect): NSPoint;
    procedure drawPageBorderWithSize(_borderSize: NSSize);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function pageHeader: lobjc.id;{NSAttributedString}
    function pageFooter: lobjc.id;{NSAttributedString}
    {.$endif}
    // ** This method is obsolete.  It will never be invoked from within AppKit, and NSView's
    // implementation of it does nothing. **
    procedure drawSheetBorderWithSize(_borderSize: NSSize);
    //  Printing
    //  Returns print job title. Default implementation first tries its window's NSDocument
    // (displayName), then window's title
    function printJobTitle: CFStringRef;
    procedure beginDocument;
    procedure endDocument;
    procedure beginPageInRect_atPlacement(_aRect: NSRect; _location: NSPoint);
    procedure endPage;
    procedure dragImage_at_offset_event_pasteboard_source_slideBack(_anImage: lobjc.id {NSImage}; _viewLocation: NSPoint; _initialOffset: NSSize; _event: lobjc.id {NSEvent}; _pboard: lobjc.id {NSPasteboard}; _sourceObj: lobjc.id; _slideFlag: LongBool);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function registeredDraggedTypes: CFArrayRef;
    {.$endif}
    procedure registerForDraggedTypes(_newTypes: CFArrayRef);
    procedure unregisterDraggedTypes;
    function dragFile_fromRect_slideBack_event(_filename: CFStringRef; _rect: NSRect; _aFlag: LongBool; _event: lobjc.id {NSEvent}): LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    function dragPromisedFilesOfTypes_fromRect_source_slideBack_event(_typeArray: CFArrayRef; _rect: NSRect; _sourceObject: lobjc.id; _aFlag: LongBool; _event: lobjc.id {NSEvent}): LongBool;
    {.$endif}
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSView_NSView = 'NSView';
  StrNSView_initWithFrame = 'initWithFrame:';
  StrNSView_window = 'window';
  StrNSView_superview = 'superview';
  StrNSView_subviews = 'subviews';
  StrNSView_isDescendantOf = 'isDescendantOf:';
  StrNSView_ancestorSharedWithView = 'ancestorSharedWithView:';
  StrNSView_opaqueAncestor = 'opaqueAncestor';
  StrNSView_setHidden = 'setHidden:';
  StrNSView_isHidden = 'isHidden';
  StrNSView_isHiddenOrHasHiddenAncestor = 'isHiddenOrHasHiddenAncestor';
  StrNSView_getRectsBeingDrawn_count = 'getRectsBeingDrawn:count:';
  StrNSView_needsToDrawRect = 'needsToDrawRect:';
  StrNSView_wantsDefaultClipping = 'wantsDefaultClipping';
  StrNSView_addSubview = 'addSubview:';
  StrNSView_addSubview_positioned_relativeTo = 'addSubview:positioned:relativeTo:';
  StrNSView_sortSubviewsUsingFunction_context = 'sortSubviewsUsingFunction:context:';
  StrNSView_viewWillMoveToWindow = 'viewWillMoveToWindow:';
  StrNSView_viewDidMoveToWindow = 'viewDidMoveToWindow';
  StrNSView_viewWillMoveToSuperview = 'viewWillMoveToSuperview:';
  StrNSView_viewDidMoveToSuperview = 'viewDidMoveToSuperview';
  StrNSView_didAddSubview = 'didAddSubview:';
  StrNSView_willRemoveSubview = 'willRemoveSubview:';
  StrNSView_removeFromSuperview = 'removeFromSuperview';
  StrNSView_replaceSubview_with = 'replaceSubview:with:';
  StrNSView_removeFromSuperviewWithoutNeedingDisplay = 'removeFromSuperviewWithoutNeedingDisplay';
  StrNSView_setPostsFrameChangedNotifications = 'setPostsFrameChangedNotifications:';
  StrNSView_postsFrameChangedNotifications = 'postsFrameChangedNotifications';
  StrNSView_resizeSubviewsWithOldSize = 'resizeSubviewsWithOldSize:';
  StrNSView_resizeWithOldSuperviewSize = 'resizeWithOldSuperviewSize:';
  StrNSView_setAutoresizesSubviews = 'setAutoresizesSubviews:';
  StrNSView_autoresizesSubviews = 'autoresizesSubviews';
  StrNSView_setAutoresizingMask = 'setAutoresizingMask:';
  StrNSView_autoresizingMask = 'autoresizingMask';
  StrNSView_setFrameOrigin = 'setFrameOrigin:';
  StrNSView_setFrameSize = 'setFrameSize:';
  StrNSView_setFrame = 'setFrame:';
  StrNSView_frame = 'frame';
  StrNSView_setFrameRotation = 'setFrameRotation:';
  StrNSView_frameRotation = 'frameRotation';
  StrNSView_setBoundsOrigin = 'setBoundsOrigin:';
  StrNSView_setBoundsSize = 'setBoundsSize:';
  StrNSView_setBoundsRotation = 'setBoundsRotation:';
  StrNSView_boundsRotation = 'boundsRotation';
  StrNSView_translateOriginToPoint = 'translateOriginToPoint:';
  StrNSView_scaleUnitSquareToSize = 'scaleUnitSquareToSize:';
  StrNSView_rotateByAngle = 'rotateByAngle:';
  StrNSView_setBounds = 'setBounds:';
  StrNSView_bounds = 'bounds';
  StrNSView_isFlipped = 'isFlipped';
  StrNSView_isRotatedFromBase = 'isRotatedFromBase';
  StrNSView_isRotatedOrScaledFromBase = 'isRotatedOrScaledFromBase';
  StrNSView_isOpaque = 'isOpaque';
  StrNSView_convertPoint_fromView = 'convertPoint:fromView:';
  StrNSView_convertPoint_toView = 'convertPoint:toView:';
  StrNSView_convertSize_fromView = 'convertSize:fromView:';
  StrNSView_convertSize_toView = 'convertSize:toView:';
  StrNSView_convertRect_fromView = 'convertRect:fromView:';
  StrNSView_convertRect_toView = 'convertRect:toView:';
  StrNSView_centerScanRect = 'centerScanRect:';
  StrNSView_canDraw = 'canDraw';
  StrNSView_setNeedsDisplay = 'setNeedsDisplay:';
  StrNSView_setNeedsDisplayInRect = 'setNeedsDisplayInRect:';
  StrNSView_needsDisplay = 'needsDisplay';
  StrNSView_lockFocus = 'lockFocus';
  StrNSView_unlockFocus = 'unlockFocus';
  StrNSView_lockFocusIfCanDraw = 'lockFocusIfCanDraw';
  StrNSView_lockFocusIfCanDrawInContext = 'lockFocusIfCanDrawInContext:';
  StrNSView_focusView = 'focusView';
  StrNSView_visibleRect = 'visibleRect';
  StrNSView_display = 'display';
  StrNSView_displayIfNeeded = 'displayIfNeeded';
  StrNSView_displayIfNeededIgnoringOpacity = 'displayIfNeededIgnoringOpacity';
  StrNSView_displayRect = 'displayRect:';
  StrNSView_displayIfNeededInRect = 'displayIfNeededInRect:';
  StrNSView_displayRectIgnoringOpacity = 'displayRectIgnoringOpacity:';
  StrNSView_displayIfNeededInRectIgnoringOpacity = 'displayIfNeededInRectIgnoringOpacity:';
  StrNSView_drawRect = 'drawRect:';
  StrNSView_displayRectIgnoringOpacity_inContext = 'displayRectIgnoringOpacity:inContext:';
  StrNSView_bitmapImageRepForCachingDisplayInRect = 'bitmapImageRepForCachingDisplayInRect:';
  StrNSView_cacheDisplayInRect_toBitmapImageRep = 'cacheDisplayInRect:toBitmapImageRep:';
  StrNSView_gState = 'gState';
  StrNSView_allocateGState = 'allocateGState';
  StrNSView_releaseGState = 'releaseGState';
  StrNSView_setUpGState = 'setUpGState';
  StrNSView_renewGState = 'renewGState';
  StrNSView_scrollPoint = 'scrollPoint:';
  StrNSView_scrollRectToVisible = 'scrollRectToVisible:';
  StrNSView_autoscroll = 'autoscroll:';
  StrNSView_adjustScroll = 'adjustScroll:';
  StrNSView_scrollRect_by = 'scrollRect:by:';
  StrNSView_hitTest = 'hitTest:';
  StrNSView_mouse_inRect = 'mouse:inRect:';
  StrNSView_viewWithTag = 'viewWithTag:';
  StrNSView_tag = 'tag';
  StrNSView_performKeyEquivalent = 'performKeyEquivalent:';
  StrNSView_acceptsFirstMouse = 'acceptsFirstMouse:';
  StrNSView_shouldDelayWindowOrderingForEvent = 'shouldDelayWindowOrderingForEvent:';
  StrNSView_needsPanelToBecomeKey = 'needsPanelToBecomeKey';
  StrNSView_mouseDownCanMoveWindow = 'mouseDownCanMoveWindow';
  StrNSView_addCursorRect_cursor = 'addCursorRect:cursor:';
  StrNSView_removeCursorRect_cursor = 'removeCursorRect:cursor:';
  StrNSView_discardCursorRects = 'discardCursorRects';
  StrNSView_resetCursorRects = 'resetCursorRects';
  StrNSView_addTrackingRect_owner_userData_assumeInside = 'addTrackingRect:owner:userData:assumeInside:';
  StrNSView_removeTrackingRect = 'removeTrackingRect:';
  StrNSView_shouldDrawColor = 'shouldDrawColor';
  StrNSView_setPostsBoundsChangedNotifications = 'setPostsBoundsChangedNotifications:';
  StrNSView_postsBoundsChangedNotifications = 'postsBoundsChangedNotifications';
  StrNSView_enclosingScrollView = 'enclosingScrollView';
  StrNSView_menuForEvent = 'menuForEvent:';
  StrNSView_defaultMenu = 'defaultMenu';
  StrNSView_setToolTip = 'setToolTip:';
  StrNSView_toolTip = 'toolTip';
  StrNSView_addToolTipRect_owner_userData = 'addToolTipRect:owner:userData:';
  StrNSView_removeToolTip = 'removeToolTip:';
  StrNSView_removeAllToolTips = 'removeAllToolTips';
  StrNSView_viewWillStartLiveResize = 'viewWillStartLiveResize';
  StrNSView_viewDidEndLiveResize = 'viewDidEndLiveResize';
  StrNSView_inLiveResize = 'inLiveResize';
  StrNSView_preservesContentDuringLiveResize = 'preservesContentDuringLiveResize';
  StrNSView_rectPreservedDuringLiveResize = 'rectPreservedDuringLiveResize';
  StrNSView_getRectsExposedDuringLiveResize = 'getRectsExposedDuringLiveResize';

  StrNSView_performMnemonic = 'performMnemonic:';
  StrNSView_setNextKeyView = 'setNextKeyView:';
  StrNSView_nextKeyView = 'nextKeyView';
  StrNSView_previousKeyView = 'previousKeyView';
  StrNSView_nextValidKeyView = 'nextValidKeyView';
  StrNSView_previousValidKeyView = 'previousValidKeyView';
  StrNSView_canBecomeKeyView = 'canBecomeKeyView';
  StrNSView_setKeyboardFocusRingNeedsDisplayInRect = 'setKeyboardFocusRingNeedsDisplayInRect:';
  StrNSView_setFocusRingType = 'setFocusRingType:';
  StrNSView_focusRingType = 'focusRingType';
  StrNSView_defaultFocusRingType = 'defaultFocusRingType';

  StrNSView_writeEPSInsideRect_toPasteboard = 'writeEPSInsideRect:toPasteboard:';
  StrNSView_dataWithEPSInsideRect = 'dataWithEPSInsideRect:';
  StrNSView_writePDFInsideRect_toPasteboard = 'writePDFInsideRect:toPasteboard:';
  StrNSView_dataWithPDFInsideRect = 'dataWithPDFInsideRect:';
  StrNSView_print = 'print:';
  StrNSView_knowsPageRange = 'knowsPageRange:';
  StrNSView_heightAdjustLimit = 'heightAdjustLimit';
  StrNSView_widthAdjustLimit = 'widthAdjustLimit';
  StrNSView_adjustPageWidthNew_left_right_limit = 'adjustPageWidthNew:left:right:limit:';
  StrNSView_adjustPageHeightNew_top_bottom_limit = 'adjustPageHeightNew:top:bottom:limit:';
  StrNSView_rectForPage = 'rectForPage:';
  StrNSView_locationOfPrintRect = 'locationOfPrintRect:';
  StrNSView_drawPageBorderWithSize = 'drawPageBorderWithSize:';
  StrNSView_pageHeader = 'pageHeader';
  StrNSView_pageFooter = 'pageFooter';
  StrNSView_drawSheetBorderWithSize = 'drawSheetBorderWithSize:';
  StrNSView_printJobTitle = 'printJobTitle';
  StrNSView_beginDocument = 'beginDocument';
  StrNSView_endDocument = 'endDocument';
  StrNSView_beginPageInRect_atPlacement = 'beginPageInRect:atPlacement:';
  StrNSView_endPage = 'endPage';

  StrNSView_dragImage_at_offset_event_pasteboard_source_slideBack = 'dragImage:at:offset:event:pasteboard:source:slideBack:';
  StrNSView_registeredDraggedTypes = 'registeredDraggedTypes';
  StrNSView_registerForDraggedTypes = 'registerForDraggedTypes:';
  StrNSView_unregisterDraggedTypes = 'unregisterDraggedTypes';
  StrNSView_dragFile_fromRect_slideBack_event = 'dragFile:fromRect:slideBack:event:';
  StrNSView_dragPromisedFilesOfTypes_fromRect_source_slideBack_event = 'dragPromisedFilesOfTypes:fromRect:source:slideBack:event:';

{ NSView }

class function NSView.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSView_NSView);
end;

constructor NSView.initWithFrame(_frameRect: NSRect);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_frameRect: NSRect): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSView_initWithFrame)), _frameRect);
end;

function NSView.window: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_window)), []));
end;

function NSView.superview: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_superview)), []));
end;

function NSView.subviews: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_subviews)), []));
end;

function NSView.isDescendantOf(_aView: lobjc.id {NSView}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aView: lobjc.id {NSView}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_isDescendantOf)), _aView));
end;

function NSView.ancestorSharedWithView(_aView: lobjc.id {NSView}): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aView: lobjc.id {NSView}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSView_ancestorSharedWithView)), _aView));
end;

function NSView.opaqueAncestor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_opaqueAncestor)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSView.setHidden(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setHidden)), _flag);
end;

function NSView.isHidden: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_isHidden)), []));
end;

function NSView.isHiddenOrHasHiddenAncestor: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_isHiddenOrHasHiddenAncestor)), []));
end;

procedure NSView.getRectsBeingDrawn_count(_rects: NSRect; _count: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rects: NSRect; _count: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_getRectsBeingDrawn_count)), _rects, _count);
end;

function NSView.needsToDrawRect(_aRect: NSRect): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aRect: NSRect): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_needsToDrawRect)), _aRect));
end;

function NSView.wantsDefaultClipping: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_wantsDefaultClipping)), []));
end;

{.$endif}
procedure NSView.addSubview(_aView: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_addSubview)), _aView);
end;

procedure NSView.addSubview_positioned_relativeTo(_aView: lobjc.id {NSView}; _place: NSWindowOrderingMode; _otherView: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aView: lobjc.id {NSView}; _place: NSWindowOrderingMode; _otherView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_addSubview_positioned_relativeTo)), _aView, _place, _otherView);
end;

procedure NSView.sortSubviewsUsingFunction_context(_compare: TProcNSViewcompare0; _context: Pointer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_compare: TProcNSViewcompare0; _context: Pointer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_sortSubviewsUsingFunction_context)), _compare, _context);
end;

procedure NSView.viewWillMoveToWindow(_newWindow: lobjc.id {NSWindow});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newWindow: lobjc.id {NSWindow}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_viewWillMoveToWindow)), _newWindow);
end;

procedure NSView.viewDidMoveToWindow;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_viewDidMoveToWindow)), []);
end;

procedure NSView.viewWillMoveToSuperview(_newSuperview: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newSuperview: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_viewWillMoveToSuperview)), _newSuperview);
end;

procedure NSView.viewDidMoveToSuperview;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_viewDidMoveToSuperview)), []);
end;

procedure NSView.didAddSubview(_subview: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_subview: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_didAddSubview)), _subview);
end;

procedure NSView.willRemoveSubview(_subview: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_subview: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_willRemoveSubview)), _subview);
end;

procedure NSView.removeFromSuperview;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_removeFromSuperview)), []);
end;

procedure NSView.replaceSubview_with(_oldView: lobjc.id {NSView}; _newView: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_oldView: lobjc.id {NSView}; _newView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_replaceSubview_with)), _oldView, _newView);
end;

procedure NSView.removeFromSuperviewWithoutNeedingDisplay;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_removeFromSuperviewWithoutNeedingDisplay)), []);
end;

procedure NSView.setPostsFrameChangedNotifications(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setPostsFrameChangedNotifications)), _flag);
end;

function NSView.postsFrameChangedNotifications: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_postsFrameChangedNotifications)), []));
end;

procedure NSView.resizeSubviewsWithOldSize(_oldSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_oldSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_resizeSubviewsWithOldSize)), _oldSize);
end;

procedure NSView.resizeWithOldSuperviewSize(_oldSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_oldSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_resizeWithOldSuperviewSize)), _oldSize);
end;

procedure NSView.setAutoresizesSubviews(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setAutoresizesSubviews)), _flag);
end;

function NSView.autoresizesSubviews: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_autoresizesSubviews)), []));
end;

procedure NSView.setAutoresizingMask(_mask: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_mask: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setAutoresizingMask)), _mask);
end;

function NSView.autoresizingMask: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_autoresizingMask)), []));
end;

procedure NSView.setFrameOrigin(_newOrigin: NSPoint);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newOrigin: NSPoint); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setFrameOrigin)), _newOrigin);
end;

procedure NSView.setFrameSize(_newSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setFrameSize)), _newSize);
end;

procedure NSView.setFrame(_frameRect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_frameRect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setFrame)), _frameRect);
end;

function NSView.frame: NSRect;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSView_frame)), []);
end;

procedure NSView.setFrameRotation(_angle: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_angle: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setFrameRotation)), _angle);
end;

function NSView.frameRotation: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_frameRotation)), []));
end;

procedure NSView.setBoundsOrigin(_newOrigin: NSPoint);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newOrigin: NSPoint); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setBoundsOrigin)), _newOrigin);
end;

procedure NSView.setBoundsSize(_newSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setBoundsSize)), _newSize);
end;

procedure NSView.setBoundsRotation(_angle: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_angle: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setBoundsRotation)), _angle);
end;

function NSView.boundsRotation: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_boundsRotation)), []));
end;

procedure NSView.translateOriginToPoint(_translation: NSPoint);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_translation: NSPoint); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_translateOriginToPoint)), _translation);
end;

procedure NSView.scaleUnitSquareToSize(_newUnitSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newUnitSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_scaleUnitSquareToSize)), _newUnitSize);
end;

procedure NSView.rotateByAngle(_angle: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_angle: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_rotateByAngle)), _angle);
end;

procedure NSView.setBounds(_aRect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aRect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setBounds)), _aRect);
end;

function NSView.bounds: NSRect;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSView_bounds)), []);
end;

function NSView.isFlipped: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_isFlipped)), []));
end;

function NSView.isRotatedFromBase: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_isRotatedFromBase)), []));
end;

function NSView.isRotatedOrScaledFromBase: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_isRotatedOrScaledFromBase)), []));
end;

function NSView.isOpaque: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_isOpaque)), []));
end;

function NSView.convertPoint_fromView(_aPoint: NSPoint; _aView: lobjc.id {NSView}): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aPoint: NSPoint; _aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSView_convertPoint_fromView)), _aPoint, _aView);
end;

function NSView.convertPoint_toView(_aPoint: NSPoint; _aView: lobjc.id {NSView}): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aPoint: NSPoint; _aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSView_convertPoint_toView)), _aPoint, _aView);
end;

function NSView.convertSize_fromView(_aSize: NSSize; _aView: lobjc.id {NSView}): NSSize;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aSize: NSSize; _aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSView_convertSize_fromView)), _aSize, _aView);
end;

function NSView.convertSize_toView(_aSize: NSSize; _aView: lobjc.id {NSView}): NSSize;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aSize: NSSize; _aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSView_convertSize_toView)), _aSize, _aView);
end;

function NSView.convertRect_fromView(_aRect: NSRect; _aView: lobjc.id {NSView}): NSRect;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aRect: NSRect; _aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSView_convertRect_fromView)), _aRect, _aView);
end;

function NSView.convertRect_toView(_aRect: NSRect; _aView: lobjc.id {NSView}): NSRect;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aRect: NSRect; _aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSView_convertRect_toView)), _aRect, _aView);
end;

function NSView.centerScanRect(_aRect: NSRect): NSRect;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aRect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSView_centerScanRect)), _aRect);
end;

function NSView.canDraw: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_canDraw)), []));
end;

procedure NSView.setNeedsDisplay(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setNeedsDisplay)), _flag);
end;

procedure NSView.setNeedsDisplayInRect(_invalidRect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_invalidRect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setNeedsDisplayInRect)), _invalidRect);
end;

function NSView.needsDisplay: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_needsDisplay)), []));
end;

procedure NSView.lockFocus;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_lockFocus)), []);
end;

procedure NSView.unlockFocus;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_unlockFocus)), []);
end;

function NSView.lockFocusIfCanDraw: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_lockFocusIfCanDraw)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSView.lockFocusIfCanDrawInContext(_context: lobjc.id {NSGraphicsContext}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_context: lobjc.id {NSGraphicsContext}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_lockFocusIfCanDrawInContext)), _context));
end;

{.$endif}
constructor NSView.focusView;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSView_focusView)));
end;

function NSView.visibleRect: NSRect;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSView_visibleRect)), []);
end;

procedure NSView.display;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_display)), []);
end;

procedure NSView.displayIfNeeded;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_displayIfNeeded)), []);
end;

procedure NSView.displayIfNeededIgnoringOpacity;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_displayIfNeededIgnoringOpacity)), []);
end;

procedure NSView.displayRect(_rect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_displayRect)), _rect);
end;

procedure NSView.displayIfNeededInRect(_rect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_displayIfNeededInRect)), _rect);
end;

procedure NSView.displayRectIgnoringOpacity(_rect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_displayRectIgnoringOpacity)), _rect);
end;

procedure NSView.displayIfNeededInRectIgnoringOpacity(_rect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_displayIfNeededInRectIgnoringOpacity)), _rect);
end;

procedure NSView.drawRect(_rect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_drawRect)), _rect);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
procedure NSView.displayRectIgnoringOpacity_inContext(_aRect: NSRect; _context: lobjc.id {NSGraphicsContext});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aRect: NSRect; _context: lobjc.id {NSGraphicsContext}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_displayRectIgnoringOpacity_inContext)), _aRect, _context);
end;

function NSView.bitmapImageRepForCachingDisplayInRect(_rect: NSRect): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_rect: NSRect): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSView_bitmapImageRepForCachingDisplayInRect)), _rect));
end;

procedure NSView.cacheDisplayInRect_toBitmapImageRep(_rect: NSRect; _bitmapImageRep: lobjc.id {NSBitmapImageRep});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect; _bitmapImageRep: lobjc.id {NSBitmapImageRep}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_cacheDisplayInRect_toBitmapImageRep)), _rect, _bitmapImageRep);
end;

{.$endif}
function NSView.gState: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_gState)), []));
end;

procedure NSView.allocateGState;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_allocateGState)), []);
end;

procedure NSView.releaseGState;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_releaseGState)), []);
end;

procedure NSView.setUpGState;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_setUpGState)), []);
end;

procedure NSView.renewGState;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_renewGState)), []);
end;

procedure NSView.scrollPoint(_aPoint: NSPoint);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aPoint: NSPoint); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_scrollPoint)), _aPoint);
end;

function NSView.scrollRectToVisible(_aRect: NSRect): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aRect: NSRect): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_scrollRectToVisible)), _aRect));
end;

function NSView.autoscroll(_theEvent: lobjc.id {NSEvent}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theEvent: lobjc.id {NSEvent}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_autoscroll)), _theEvent));
end;

function NSView.adjustScroll(_newVisible: NSRect): NSRect;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_newVisible: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSView_adjustScroll)), _newVisible);
end;

procedure NSView.scrollRect_by(_aRect: NSRect; _delta: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aRect: NSRect; _delta: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_scrollRect_by)), _aRect, _delta);
end;

function NSView.hitTest(_aPoint: NSPoint): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aPoint: NSPoint): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSView_hitTest)), _aPoint));
end;

function NSView.mouse_inRect(_aPoint: NSPoint; _aRect: NSRect): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aPoint: NSPoint; _aRect: NSRect): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_mouse_inRect)), _aPoint, _aRect));
end;

function NSView.viewWithTag(_aTag: Integer): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aTag: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSView_viewWithTag)), _aTag));
end;

function NSView.tag: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_tag)), []));
end;

function NSView.performKeyEquivalent(_theEvent: lobjc.id {NSEvent}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theEvent: lobjc.id {NSEvent}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_performKeyEquivalent)), _theEvent));
end;

function NSView.acceptsFirstMouse(_theEvent: lobjc.id {NSEvent}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theEvent: lobjc.id {NSEvent}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_acceptsFirstMouse)), _theEvent));
end;

function NSView.shouldDelayWindowOrderingForEvent(_theEvent: lobjc.id {NSEvent}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theEvent: lobjc.id {NSEvent}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_shouldDelayWindowOrderingForEvent)), _theEvent));
end;

function NSView.needsPanelToBecomeKey: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_needsPanelToBecomeKey)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
function NSView.mouseDownCanMoveWindow: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_mouseDownCanMoveWindow)), []));
end;

{.$endif}
procedure NSView.addCursorRect_cursor(_aRect: NSRect; _anObj: lobjc.id {NSCursor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aRect: NSRect; _anObj: lobjc.id {NSCursor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_addCursorRect_cursor)), _aRect, _anObj);
end;

procedure NSView.removeCursorRect_cursor(_aRect: NSRect; _anObj: lobjc.id {NSCursor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aRect: NSRect; _anObj: lobjc.id {NSCursor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_removeCursorRect_cursor)), _aRect, _anObj);
end;

procedure NSView.discardCursorRects;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_discardCursorRects)), []);
end;

procedure NSView.resetCursorRects;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_resetCursorRects)), []);
end;

function NSView.addTrackingRect_owner_userData_assumeInside(_aRect: NSRect; _anObject: lobjc.id; _data: Pointer; _flag: LongBool): NSTrackingRectTag;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aRect: NSRect; _anObject: lobjc.id; _data: Pointer; _flag: LongBool): NSTrackingRectTag; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSTrackingRectTag(vmethod(Handle, sel_registerName(PChar(StrNSView_addTrackingRect_owner_userData_assumeInside)), _aRect, _anObject, _data, _flag));
end;

procedure NSView.removeTrackingRect(_tag: NSTrackingRectTag);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_tag: NSTrackingRectTag); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_removeTrackingRect)), _tag);
end;

function NSView.shouldDrawColor: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_shouldDrawColor)), []));
end;

procedure NSView.setPostsBoundsChangedNotifications(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setPostsBoundsChangedNotifications)), _flag);
end;

function NSView.postsBoundsChangedNotifications: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_postsBoundsChangedNotifications)), []));
end;

function NSView.enclosingScrollView: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_enclosingScrollView)), []));
end;

function NSView.menuForEvent(_event: lobjc.id {NSEvent}): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_event: lobjc.id {NSEvent}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSView_menuForEvent)), _event));
end;

class function NSView.defaultMenu: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(getClass, sel_registerName(PChar(StrNSView_defaultMenu)), []));
end;

procedure NSView.setToolTip(__string: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__string: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setToolTip)), __string);
end;

function NSView.toolTip: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_toolTip)), []));
end;

function NSView.addToolTipRect_owner_userData(_aRect: NSRect; _anObject: lobjc.id; _data: Pointer): NSToolTipTag;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aRect: NSRect; _anObject: lobjc.id; _data: Pointer): NSToolTipTag; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSToolTipTag(vmethod(Handle, sel_registerName(PChar(StrNSView_addToolTipRect_owner_userData)), _aRect, _anObject, _data));
end;

procedure NSView.removeToolTip(_tag: NSToolTipTag);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_tag: NSToolTipTag); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_removeToolTip)), _tag);
end;

procedure NSView.removeAllToolTips;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_removeAllToolTips)), []);
end;

procedure NSView.viewWillStartLiveResize;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_viewWillStartLiveResize)), []);
end;

procedure NSView.viewDidEndLiveResize;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_viewDidEndLiveResize)), []);
end;

function NSView.inLiveResize: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_inLiveResize)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSView.preservesContentDuringLiveResize: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_preservesContentDuringLiveResize)), []));
end;

function NSView.rectPreservedDuringLiveResize: NSRect;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSView_rectPreservedDuringLiveResize)), []);
end;

procedure NSView.getRectsExposedDuringLiveResize;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_getRectsExposedDuringLiveResize)), []);
end;

{.$endif}
function NSView.performMnemonic(_theString: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_theString: CFStringRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_performMnemonic)), _theString));
end;

procedure NSView.setNextKeyView(_next: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_next: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setNextKeyView)), _next);
end;

function NSView.nextKeyView: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_nextKeyView)), []));
end;

function NSView.previousKeyView: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_previousKeyView)), []));
end;

function NSView.nextValidKeyView: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_nextValidKeyView)), []));
end;

function NSView.previousValidKeyView: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_previousValidKeyView)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
function NSView.canBecomeKeyView: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_canBecomeKeyView)), []));
end;

{.$endif}
procedure NSView.setKeyboardFocusRingNeedsDisplayInRect(_rect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setKeyboardFocusRingNeedsDisplayInRect)), _rect);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSView.setFocusRingType(_focusRingType: NSFocusRingType);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_focusRingType: NSFocusRingType); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_setFocusRingType)), _focusRingType);
end;

function NSView.focusRingType: NSFocusRingType;
begin
  Result := NSFocusRingType(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_focusRingType)), []));
end;

class function NSView.defaultFocusRingType: NSFocusRingType;
begin
  Result := NSFocusRingType(objc_msgSend(getClass, sel_registerName(PChar(StrNSView_defaultFocusRingType)), []));
end;

{.$endif}
procedure NSView.writeEPSInsideRect_toPasteboard(_rect: NSRect; _pasteboard: lobjc.id {NSPasteboard});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect; _pasteboard: lobjc.id {NSPasteboard}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_writeEPSInsideRect_toPasteboard)), _rect, _pasteboard);
end;

function NSView.dataWithEPSInsideRect(_rect: NSRect): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_rect: NSRect): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSView_dataWithEPSInsideRect)), _rect));
end;

procedure NSView.writePDFInsideRect_toPasteboard(_rect: NSRect; _pasteboard: lobjc.id {NSPasteboard});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect; _pasteboard: lobjc.id {NSPasteboard}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_writePDFInsideRect_toPasteboard)), _rect, _pasteboard);
end;

function NSView.dataWithPDFInsideRect(_rect: NSRect): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_rect: NSRect): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSView_dataWithPDFInsideRect)), _rect));
end;

procedure NSView.print(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_print)), _sender);
end;

function NSView.knowsPageRange(_range: NSRangePointer): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_range: NSRangePointer): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_knowsPageRange)), _range));
end;

function NSView.heightAdjustLimit: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_heightAdjustLimit)), []));
end;

function NSView.widthAdjustLimit: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_widthAdjustLimit)), []));
end;

procedure NSView.adjustPageWidthNew_left_right_limit(_newRight: Single; _oldLeft: Single; _oldRight: Single; _rightLimit: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newRight: Single; _oldLeft: Single; _oldRight: Single; _rightLimit: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_adjustPageWidthNew_left_right_limit)), _newRight, _oldLeft, _oldRight, _rightLimit);
end;

procedure NSView.adjustPageHeightNew_top_bottom_limit(_newBottom: Single; _oldTop: Single; _oldBottom: Single; _bottomLimit: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newBottom: Single; _oldTop: Single; _oldBottom: Single; _bottomLimit: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_adjustPageHeightNew_top_bottom_limit)), _newBottom, _oldTop, _oldBottom, _bottomLimit);
end;

function NSView.rectForPage(_page: Integer): NSRect;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_page: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSView_rectForPage)), _page);
end;

function NSView.locationOfPrintRect(_aRect: NSRect): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aRect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSView_locationOfPrintRect)), _aRect);
end;

procedure NSView.drawPageBorderWithSize(_borderSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_borderSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_drawPageBorderWithSize)), _borderSize);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSView.pageHeader: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_pageHeader)), []));
end;

function NSView.pageFooter: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_pageFooter)), []));
end;

{.$endif}
procedure NSView.drawSheetBorderWithSize(_borderSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_borderSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_drawSheetBorderWithSize)), _borderSize);
end;

function NSView.printJobTitle: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_printJobTitle)), []));
end;

procedure NSView.beginDocument;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_beginDocument)), []);
end;

procedure NSView.endDocument;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_endDocument)), []);
end;

procedure NSView.beginPageInRect_atPlacement(_aRect: NSRect; _location: NSPoint);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aRect: NSRect; _location: NSPoint); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_beginPageInRect_atPlacement)), _aRect, _location);
end;

procedure NSView.endPage;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_endPage)), []);
end;

procedure NSView.dragImage_at_offset_event_pasteboard_source_slideBack(_anImage: lobjc.id {NSImage}; _viewLocation: NSPoint; _initialOffset: NSSize; _event: lobjc.id {NSEvent}; _pboard: lobjc.id {NSPasteboard}; _sourceObj: lobjc.id; _slideFlag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anImage: lobjc.id {NSImage}; _viewLocation: NSPoint; _initialOffset: NSSize; _event: lobjc.id {NSEvent}; _pboard: lobjc.id {NSPasteboard}; _sourceObj: lobjc.id; _slideFlag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_dragImage_at_offset_event_pasteboard_source_slideBack)), _anImage, _viewLocation, _initialOffset, _event, _pboard, _sourceObj, _slideFlag);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSView.registeredDraggedTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSView_registeredDraggedTypes)), []));
end;

{.$endif}
procedure NSView.registerForDraggedTypes(_newTypes: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newTypes: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSView_registerForDraggedTypes)), _newTypes);
end;

procedure NSView.unregisterDraggedTypes;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSView_unregisterDraggedTypes)), []);
end;

function NSView.dragFile_fromRect_slideBack_event(_filename: CFStringRef; _rect: NSRect; _aFlag: LongBool; _event: lobjc.id {NSEvent}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_filename: CFStringRef; _rect: NSRect; _aFlag: LongBool; _event: lobjc.id {NSEvent}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_dragFile_fromRect_slideBack_event)), _filename, _rect, _aFlag, _event));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
function NSView.dragPromisedFilesOfTypes_fromRect_source_slideBack_event(_typeArray: CFArrayRef; _rect: NSRect; _sourceObject: lobjc.id; _aFlag: LongBool; _event: lobjc.id {NSEvent}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_typeArray: CFArrayRef; _rect: NSRect; _sourceObject: lobjc.id; _aFlag: LongBool; _event: lobjc.id {NSEvent}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSView_dragPromisedFilesOfTypes_fromRect_source_slideBack_event)), _typeArray, _rect, _sourceObject, _aFlag, _event));
end;

{.$endif}
{$endif}
