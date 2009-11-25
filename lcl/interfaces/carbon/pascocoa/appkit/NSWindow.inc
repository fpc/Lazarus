{%mainunit appkit.pas}
(*
	NSWindow.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSWINDOW_PAS_H}
{$define NSWINDOW_PAS_H}

{$include NSResponder.inc}
{$include NSGraphics.inc}
{$include AppKitDefines.inc}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}
  {.$else}
  {.$endif}
const
  NSAppKitVersionNumberWithCustomSheetPosition = 686.0;
  NSNormalWindowLevel = kCGNormalWindowLevelKey;
  NSFloatingWindowLevel = kCGFloatingWindowLevelKey;
  NSSubmenuWindowLevel = kCGTornOffMenuWindowLevelKey;
  NSTornOffMenuWindowLevel = kCGTornOffMenuWindowLevelKey;
  NSMainMenuWindowLevel = kCGMainMenuWindowLevelKey;
  NSStatusWindowLevel = kCGStatusWindowLevelKey;
  NSDockWindowLevel = kCGDockWindowLevelKey;
  NSModalPanelWindowLevel = kCGModalPanelWindowLevelKey;
  NSPopUpMenuWindowLevel = kCGPopUpMenuWindowLevelKey;
  NSScreenSaverWindowLevel = kCGScreenSaverWindowLevelKey;
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}
  {.$else}
  {.$endif}

const
  NSBorderlessWindowMask = 0;
  NSTitledWindowMask = 1 shl 0;
  NSClosableWindowMask = 1 shl 1;
  NSMiniaturizableWindowMask = 1 shl 2;
  NSResizableWindowMask = 1 shl 3;

  //  Specifies a window with textured background (eg. metal)
const
  NSTexturedBackgroundWindowMask = 1 shl 8;

  //  Specifies a window that ignores the userSpaceScaleFactor of the NSScreen on which
  // it is created.  Currently restricted to borderless windows (NSBorderlessWindowMask)
const
  NSUnscaledWindowMask = 1 shl 11;

  //  Specifies a window whose titlebar and toolbar have a unified look - that is, a continuous
  // background
const
  NSUnifiedTitleAndToolbarWindowMask = 1 shl 12;

  //  used with NSRunLoop's performSelector:target:argument:order:modes:
const
  NSDisplayWindowRunLoopOrdering = 600000;
  NSResetCursorRectsRunLoopOrdering = 700000;

type
  NSSelectionDirection = (
    NSDirectSelection = 0, 
    NSSelectingNext, 
    NSSelectingPrevious
  );


type
  NSWindowButton = (
    NSWindowCloseButton, 
    NSWindowMiniaturizeButton, 
    NSWindowZoomButton, 
    NSWindowToolbarButton, 
    NSWindowDocumentIconButton
  );


//APPKIT_EXTERN NSString *NSWindowDidBecomeKeyNotification;
//APPKIT_EXTERN NSString *NSWindowDidBecomeMainNotification;
//APPKIT_EXTERN NSString *NSWindowDidChangeScreenNotification;
//APPKIT_EXTERN NSString *NSWindowDidDeminiaturizeNotification;
//APPKIT_EXTERN NSString *NSWindowDidExposeNotification;			// userInfo key:  @"NSExposedRect"
//APPKIT_EXTERN NSString *NSWindowDidMiniaturizeNotification;
//APPKIT_EXTERN NSString *NSWindowDidMoveNotification;
//APPKIT_EXTERN NSString *NSWindowDidResignKeyNotification;
//APPKIT_EXTERN NSString *NSWindowDidResignMainNotification;
//APPKIT_EXTERN NSString *NSWindowDidResizeNotification;
//APPKIT_EXTERN NSString *NSWindowDidUpdateNotification;
//APPKIT_EXTERN NSString *NSWindowWillCloseNotification;
//APPKIT_EXTERN NSString *NSWindowWillMiniaturizeNotification;
//APPKIT_EXTERN NSString *NSWindowWillMoveNotification;
//APPKIT_EXTERN NSString *NSWindowWillBeginSheetNotification;
//APPKIT_EXTERN NSString *NSWindowDidEndSheetNotification;
//APPKIT_EXTERN NSString *NSWindowDidChangeScreenProfileNotification  AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER;
{$endif}
{$endif}

{$ifdef FORWARD}
  NSWindow = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSWINDOW_PAS_C}
{$define NSWINDOW_PAS_C}

{$include NSResponder.inc}
{$include NSGraphics.inc}
{$include AppKitDefines.inc}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    {.$endif}

  { NSWindow }

  NSWindow = class(NSResponder)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    class function frameRectForContentRect_styleMask(_cRect: NSRect; _aStyle: LongWord): NSRect;
    class function contentRectForFrameRect_styleMask(_fRect: NSRect; _aStyle: LongWord): NSRect;
    class function minFrameWidthWithTitle_styleMask(_aTitle: CFStringRef; _aStyle: LongWord): Single;
    class function defaultDepthLimit: NSWindowDepth;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    function frameRectForContentRect(_contentRect: NSRect): NSRect;
    function contentRectForFrameRect(_frameRect: NSRect): NSRect;
    {.$endif}
    constructor initWithContentRect_styleMask_backing_defer(_contentRect: NSRect; _aStyle: LongWord; _bufferingType: NSBackingStoreType; _flag: LongBool);
    constructor initWithContentRect_styleMask_backing_defer_screen(_contentRect: NSRect; _aStyle: LongWord; _bufferingType: NSBackingStoreType; _flag: LongBool; _screen: lobjc.id {NSScreen});
    function title: CFStringRef;
    procedure setTitle(_aString: CFStringRef);
    function representedFilename: CFStringRef;
    procedure setRepresentedFilename(_aString: CFStringRef);
    procedure setTitleWithRepresentedFilename(_filename: CFStringRef);
    procedure setExcludedFromWindowsMenu(_flag: LongBool);
    function isExcludedFromWindowsMenu: LongBool;
    procedure setContentView(_aView: lobjc.id {NSView});
    function contentView: lobjc.id;
    procedure setDelegate(_anObject: lobjc.id);
    function delegate: lobjc.id;
    function windowNumber: Integer;
    function styleMask: LongWord;
    function fieldEditor_forObject(_createFlag: LongBool; _anObject: lobjc.id): lobjc.id;{NSText}
    procedure endEditingFor(_anObject: lobjc.id);
    function constrainFrameRect_toScreen(_frameRect: NSRect; _screen: lobjc.id {NSScreen}): NSRect;
    procedure setFrame_display(_frameRect: NSRect; _flag: LongBool);
    procedure setContentSize(_aSize: NSSize);
    procedure setFrameOrigin(_aPoint: NSPoint);
    procedure setFrameTopLeftPoint(_aPoint: NSPoint);
    function cascadeTopLeftFromPoint(_topLeftPoint: NSPoint): NSPoint;
    function frame: NSRect;
    //  smooth resize
    //  subclasses can override animationResizeTime: to control the total time for the frame
    // change.
    //  newFrame is the rect passed into setFrame:display:animate:
    function animationResizeTime(_newFrame: NSRect): NSTimeInterval;
    //  setFrame:display:animate: is equivalent to setFrame:display: if animateFlag is NO
    //  If animationFlag is YES, this method will perform a smooth resize of the window,
    // where the total time for the resize is specified by -animationResizeTime:
    procedure setFrame_display_animate(_frameRect: NSRect; _displayFlag: LongBool; _animateFlag: LongBool);
    //  show/hide resize corner (does not affect whether window is resizable)
    procedure setShowsResizeIndicator(_show: LongBool);
    function showsResizeIndicator: LongBool;
    procedure setResizeIncrements(_increments: NSSize);
    function resizeIncrements: NSSize;
    procedure setAspectRatio(_ratio: NSSize);
    function aspectRatio: NSSize;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure setContentResizeIncrements(_increments: NSSize);
    function contentResizeIncrements: NSSize;
    procedure setContentAspectRatio(_ratio: NSSize);
    function contentAspectRatio: NSSize;
    {.$endif}
    procedure useOptimizedDrawing(_flag: LongBool);
    procedure disableFlushWindow;
    procedure enableFlushWindow;
    function isFlushWindowDisabled: LongBool;
    procedure flushWindow;
    procedure flushWindowIfNeeded;
    procedure setViewsNeedDisplay(_flag: LongBool);
    function viewsNeedDisplay: LongBool;
    procedure displayIfNeeded;
    procedure display;
    procedure setAutodisplay(_flag: LongBool);
    function isAutodisplay: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function preservesContentDuringLiveResize: LongBool;
    procedure setPreservesContentDuringLiveResize(_flag: LongBool);
    {.$endif}
    procedure update;
    function makeFirstResponder(_aResponder: lobjc.id {NSResponder}): LongBool;
    function firstResponder: lobjc.id;{NSResponder}
    function resizeFlags: Integer;
    procedure keyDown(_theEvent: lobjc.id {NSEvent});
    procedure close;
    procedure setReleasedWhenClosed(_flag: LongBool);
    function isReleasedWhenClosed: LongBool;
    procedure miniaturize(_sender: lobjc.id);
    procedure deminiaturize(_sender: lobjc.id);
    function isZoomed: LongBool;
    procedure zoom(_sender: lobjc.id);
    function isMiniaturized: LongBool;
    function tryToPerform_with(_anAction: SEL; _anObject: lobjc.id): LongBool;
    function validRequestorForSendType_returnType(_sendType: CFStringRef; _returnType: CFStringRef): lobjc.id;
    procedure setBackgroundColor(_color: lobjc.id {NSColor});
    function backgroundColor: lobjc.id;{NSColor}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    procedure setMovableByWindowBackground(_flag: LongBool);
    function isMovableByWindowBackground: LongBool;
    {.$endif}
    procedure setHidesOnDeactivate(_flag: LongBool);
    function hidesOnDeactivate: LongBool;
    //  indicate whether a window can be hidden during -[NSApplication hide:].  Default
    // is YES
    procedure setCanHide(_flag: LongBool);
    function canHide: LongBool;
    procedure center;
    procedure makeKeyAndOrderFront(_sender: lobjc.id);
    procedure orderFront(_sender: lobjc.id);
    procedure orderBack(_sender: lobjc.id);
    procedure orderOut(_sender: lobjc.id);
    procedure orderWindow_relativeTo(_place: NSWindowOrderingMode; _otherWin: Integer);
    procedure orderFrontRegardless;
    procedure setMiniwindowImage(_image: lobjc.id {NSImage});
    procedure setMiniwindowTitle(_title: CFStringRef);
    function miniwindowImage: lobjc.id;{NSImage}
    function miniwindowTitle: CFStringRef;
    procedure setDocumentEdited(_flag: LongBool);
    function isDocumentEdited: LongBool;
    function isVisible: LongBool;
    function isKeyWindow: LongBool;
    function isMainWindow: LongBool;
    function canBecomeKeyWindow: LongBool;
    function canBecomeMainWindow: LongBool;
    procedure makeKeyWindow;
    procedure makeMainWindow;
    procedure becomeKeyWindow;
    procedure resignKeyWindow;
    procedure becomeMainWindow;
    procedure resignMainWindow;
    function worksWhenModal: LongBool;
    function convertBaseToScreen(_aPoint: NSPoint): NSPoint;
    function convertScreenToBase(_aPoint: NSPoint): NSPoint;
    procedure performClose(_sender: lobjc.id);
    procedure performMiniaturize(_sender: lobjc.id);
    procedure performZoom(_sender: lobjc.id);
    function gState: Integer;
    procedure setOneShot(_flag: LongBool);
    function isOneShot: LongBool;
    function dataWithEPSInsideRect(_rect: NSRect): CFDataRef;
    function dataWithPDFInsideRect(_rect: NSRect): CFDataRef;
    procedure print(_sender: lobjc.id);
    procedure disableCursorRects;
    procedure enableCursorRects;
    procedure discardCursorRects;
    function areCursorRectsEnabled: LongBool;
    procedure invalidateCursorRectsForView(_aView: lobjc.id {NSView});
    procedure resetCursorRects;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure setAllowsToolTipsWhenApplicationIsInactive(_allowWhenInactive: LongBool);
    //  Default is NO. Set to YES to allow a window to display tooltips even when the application
    // is in the background.  Note that, enabling tooltips in an inactive application will
    // cause the app to do work any time the mouse passes over the window.  This can degrade
    // system performance.
    function allowsToolTipsWhenApplicationIsInactive: LongBool;
    {.$endif}
    procedure setBackingType(_bufferingType: NSBackingStoreType);
    function backingType: NSBackingStoreType;
    procedure setLevel(_newLevel: Integer);
    function level: Integer;
    procedure setDepthLimit(_limit: NSWindowDepth);
    function depthLimit: NSWindowDepth;
    procedure setDynamicDepthLimit(_flag: LongBool);
    function hasDynamicDepthLimit: LongBool;
    function screen: lobjc.id;{NSScreen}
    function deepestScreen: lobjc.id;{NSScreen}
    function canStoreColor: LongBool;
    procedure setHasShadow(_hasShadow: LongBool);
    function hasShadow: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    procedure invalidateShadow;
    {.$endif}
    procedure setAlphaValue(_windowAlpha: Single);
    function alphaValue: Single;
    procedure setOpaque(_isOpaque: LongBool);
    function isOpaque: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function displaysWhenScreenProfileChanges: LongBool;
    procedure setDisplaysWhenScreenProfileChanges(_flag: LongBool);
    procedure disableScreenUpdatesUntilFlush;
    {.$endif}
    function stringWithSavedFrame: CFStringRef;
    procedure setFrameFromString(__string: CFStringRef);
    procedure saveFrameUsingName(_name: CFStringRef);
    //  Set force=YES to use setFrameUsingName on a non-resizable window
    function setFrameUsingName_force(_name: CFStringRef; _force: LongBool): LongBool;
    function setFrameUsingName(_name: CFStringRef): LongBool;
    function setFrameAutosaveName(_name: CFStringRef): LongBool;
    function frameAutosaveName: CFStringRef;
    class procedure removeFrameUsingName(_name: CFStringRef);
    procedure cacheImageInRect(_aRect: NSRect);
    procedure restoreCachedImage;
    procedure discardCachedImage;
    function minSize: NSSize;
    function maxSize: NSSize;
    procedure setMinSize(_size: NSSize);
    procedure setMaxSize(_size: NSSize);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    function contentMinSize: NSSize;
    function contentMaxSize: NSSize;
    procedure setContentMinSize(_size: NSSize);
    procedure setContentMaxSize(_size: NSSize);
    {.$endif}
    function nextEventMatchingMask(_mask: LongWord): lobjc.id;{NSEvent}
    function nextEventMatchingMask_untilDate_inMode_dequeue(_mask: LongWord; _expiration: CFDateRef; _mode: CFStringRef; _deqFlag: LongBool): lobjc.id;{NSEvent}
    procedure discardEventsMatchingMask_beforeEvent(_mask: LongWord; _lastEvent: lobjc.id {NSEvent});
    procedure postEvent_atStart(_event: lobjc.id {NSEvent}; _flag: LongBool);
    function currentEvent: lobjc.id;{NSEvent}
    procedure setAcceptsMouseMovedEvents(_flag: LongBool);
    function acceptsMouseMovedEvents: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    procedure setIgnoresMouseEvents(_flag: LongBool);
    function ignoresMouseEvents: LongBool;
    {.$endif}
    function deviceDescription: CFDictionaryRef;
    procedure sendEvent(_theEvent: lobjc.id {NSEvent});
    function mouseLocationOutsideOfEventStream: NSPoint;
    class procedure menuChanged(_menu: lobjc.id {NSMenu});
    function windowController: lobjc.id;
    procedure setWindowController(_windowController: lobjc.id {NSWindowController});
    function isSheet: LongBool;
    function attachedSheet: lobjc.id;{NSWindow}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    class function standardWindowButton_forStyleMask(_b: NSWindowButton; _styleMask: LongWord): lobjc.id;{NSButton}
    function standardWindowButton(_b: NSWindowButton): lobjc.id;{NSButton}
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    procedure addChildWindow_ordered(_childWin: lobjc.id {NSWindow}; _place: NSWindowOrderingMode);
    procedure removeChildWindow(_childWin: lobjc.id {NSWindow});
    function childWindows: CFArrayRef;
    function parentWindow: lobjc.id;{NSWindow}
    procedure setParentWindow(_window: lobjc.id {NSWindow});
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    //  Returns NSGraphicsContext used to render the receiver's content on the screen for
    // the calling thread.
    function graphicsContext: lobjc.id;{NSGraphicsContext}
    //  Returns scale factor applied to view coordinate system to get to base coordinate
    // system of window
    function userSpaceScaleFactor: Single;
    {.$endif}
    procedure setInitialFirstResponder(_view: lobjc.id {NSView});
    function initialFirstResponder: lobjc.id;{NSView}
    procedure selectNextKeyView(_sender: lobjc.id);
    procedure selectPreviousKeyView(_sender: lobjc.id);
    procedure selectKeyViewFollowingView(_aView: lobjc.id {NSView});
    procedure selectKeyViewPrecedingView(_aView: lobjc.id {NSView});
    function keyViewSelectionDirection: NSSelectionDirection;
    procedure setDefaultButtonCell(_defButt: lobjc.id {NSButtonCell});
    function defaultButtonCell: lobjc.id;{NSButtonCell}
    procedure disableKeyEquivalentForDefaultButtonCell;
    procedure enableKeyEquivalentForDefaultButtonCell;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    procedure setAutorecalculatesKeyViewLoop(_flag: LongBool);
    function autorecalculatesKeyViewLoop: LongBool;
    procedure recalculateKeyViewLoop;
    {.$endif}
    procedure setToolbar(_toolbar: lobjc.id {NSToolbar});
    function toolbar: lobjc.id;{NSToolbar}
    procedure toggleToolbarShown(_sender: lobjc.id);
    procedure runToolbarCustomizationPalette(_sender: lobjc.id);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    procedure setShowsToolbarButton(_show: LongBool);
    function showsToolbarButton: LongBool;
    {.$endif}
    procedure dragImage_at_offset_event_pasteboard_source_slideBack(_anImage: lobjc.id {NSImage}; _baseLocation: NSPoint; _initialOffset: NSSize; _event: lobjc.id {NSEvent}; _pboard: lobjc.id {NSPasteboard}; _sourceObj: lobjc.id; _slideFlag: LongBool);
    procedure registerForDraggedTypes(_newTypes: CFArrayRef);
    procedure unregisterDraggedTypes;
    function windowHandle: Pointer;
    //  create an NSWindow for a Carbon window - windowRef must be a Carbon WindowRef -
    // see MacWindows.h
    constructor initWithWindowRef(_windowRef: Pointer);
    //  return the Carbon WindowRef for this window, creating if necessary: - see MacWindows.h
    function windowRef: Pointer;
  end;

    {.$else}
    {.$endif}
{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSWindow_NSWindow = 'NSWindow';
  StrNSWindow_frameRectForContentRect_styleMask = 'frameRectForContentRect:styleMask:';
  StrNSWindow_contentRectForFrameRect_styleMask = 'contentRectForFrameRect:styleMask:';
  StrNSWindow_minFrameWidthWithTitle_styleMask = 'minFrameWidthWithTitle:styleMask:';
  StrNSWindow_defaultDepthLimit = 'defaultDepthLimit';
  StrNSWindow_frameRectForContentRect = 'frameRectForContentRect:';
  StrNSWindow_contentRectForFrameRect = 'contentRectForFrameRect:';
  StrNSWindow_initWithContentRect_styleMask_backing_defer = 'initWithContentRect:styleMask:backing:defer:';
  StrNSWindow_initWithContentRect_styleMask_backing_defer_screen = 'initWithContentRect:styleMask:backing:defer:screen:';
  StrNSWindow_title = 'title';
  StrNSWindow_setTitle = 'setTitle:';
  StrNSWindow_representedFilename = 'representedFilename';
  StrNSWindow_setRepresentedFilename = 'setRepresentedFilename:';
  StrNSWindow_setTitleWithRepresentedFilename = 'setTitleWithRepresentedFilename:';
  StrNSWindow_setExcludedFromWindowsMenu = 'setExcludedFromWindowsMenu:';
  StrNSWindow_isExcludedFromWindowsMenu = 'isExcludedFromWindowsMenu';
  StrNSWindow_setContentView = 'setContentView:';
  StrNSWindow_contentView = 'contentView';
  StrNSWindow_setDelegate = 'setDelegate:';
  StrNSWindow_delegate = 'delegate';
  StrNSWindow_windowNumber = 'windowNumber';
  StrNSWindow_styleMask = 'styleMask';
  StrNSWindow_fieldEditor_forObject = 'fieldEditor:forObject:';
  StrNSWindow_endEditingFor = 'endEditingFor:';
  StrNSWindow_constrainFrameRect_toScreen = 'constrainFrameRect:toScreen:';
  StrNSWindow_setFrame_display = 'setFrame:display:';
  StrNSWindow_setContentSize = 'setContentSize:';
  StrNSWindow_setFrameOrigin = 'setFrameOrigin:';
  StrNSWindow_setFrameTopLeftPoint = 'setFrameTopLeftPoint:';
  StrNSWindow_cascadeTopLeftFromPoint = 'cascadeTopLeftFromPoint:';
  StrNSWindow_frame = 'frame';
  StrNSWindow_animationResizeTime = 'animationResizeTime:';
  StrNSWindow_setFrame_display_animate = 'setFrame:display:animate:';
  StrNSWindow_setShowsResizeIndicator = 'setShowsResizeIndicator:';
  StrNSWindow_showsResizeIndicator = 'showsResizeIndicator';
  StrNSWindow_setResizeIncrements = 'setResizeIncrements:';
  StrNSWindow_resizeIncrements = 'resizeIncrements';
  StrNSWindow_setAspectRatio = 'setAspectRatio:';
  StrNSWindow_aspectRatio = 'aspectRatio';
  StrNSWindow_setContentResizeIncrements = 'setContentResizeIncrements:';
  StrNSWindow_contentResizeIncrements = 'contentResizeIncrements';
  StrNSWindow_setContentAspectRatio = 'setContentAspectRatio:';
  StrNSWindow_contentAspectRatio = 'contentAspectRatio';
  StrNSWindow_useOptimizedDrawing = 'useOptimizedDrawing:';
  StrNSWindow_disableFlushWindow = 'disableFlushWindow';
  StrNSWindow_enableFlushWindow = 'enableFlushWindow';
  StrNSWindow_isFlushWindowDisabled = 'isFlushWindowDisabled';
  StrNSWindow_flushWindow = 'flushWindow';
  StrNSWindow_flushWindowIfNeeded = 'flushWindowIfNeeded';
  StrNSWindow_setViewsNeedDisplay = 'setViewsNeedDisplay:';
  StrNSWindow_viewsNeedDisplay = 'viewsNeedDisplay';
  StrNSWindow_displayIfNeeded = 'displayIfNeeded';
  StrNSWindow_display = 'display';
  StrNSWindow_setAutodisplay = 'setAutodisplay:';
  StrNSWindow_isAutodisplay = 'isAutodisplay';
  StrNSWindow_preservesContentDuringLiveResize = 'preservesContentDuringLiveResize';
  StrNSWindow_setPreservesContentDuringLiveResize = 'setPreservesContentDuringLiveResize:';
  StrNSWindow_update = 'update';
  StrNSWindow_makeFirstResponder = 'makeFirstResponder:';
  StrNSWindow_firstResponder = 'firstResponder';
  StrNSWindow_resizeFlags = 'resizeFlags';
  StrNSWindow_keyDown = 'keyDown:';
  StrNSWindow_close = 'close';
  StrNSWindow_setReleasedWhenClosed = 'setReleasedWhenClosed:';
  StrNSWindow_isReleasedWhenClosed = 'isReleasedWhenClosed';
  StrNSWindow_miniaturize = 'miniaturize:';
  StrNSWindow_deminiaturize = 'deminiaturize:';
  StrNSWindow_isZoomed = 'isZoomed';
  StrNSWindow_zoom = 'zoom:';
  StrNSWindow_isMiniaturized = 'isMiniaturized';
  StrNSWindow_tryToPerform_with = 'tryToPerform:with:';
  StrNSWindow_validRequestorForSendType_returnType = 'validRequestorForSendType:returnType:';
  StrNSWindow_setBackgroundColor = 'setBackgroundColor:';
  StrNSWindow_backgroundColor = 'backgroundColor';
  StrNSWindow_setMovableByWindowBackground = 'setMovableByWindowBackground:';
  StrNSWindow_isMovableByWindowBackground = 'isMovableByWindowBackground';
  StrNSWindow_setHidesOnDeactivate = 'setHidesOnDeactivate:';
  StrNSWindow_hidesOnDeactivate = 'hidesOnDeactivate';
  StrNSWindow_setCanHide = 'setCanHide:';
  StrNSWindow_canHide = 'canHide';
  StrNSWindow_center = 'center';
  StrNSWindow_makeKeyAndOrderFront = 'makeKeyAndOrderFront:';
  StrNSWindow_orderFront = 'orderFront:';
  StrNSWindow_orderBack = 'orderBack:';
  StrNSWindow_orderOut = 'orderOut:';
  StrNSWindow_orderWindow_relativeTo = 'orderWindow:relativeTo:';
  StrNSWindow_orderFrontRegardless = 'orderFrontRegardless';
  StrNSWindow_setMiniwindowImage = 'setMiniwindowImage:';
  StrNSWindow_setMiniwindowTitle = 'setMiniwindowTitle:';
  StrNSWindow_miniwindowImage = 'miniwindowImage';
  StrNSWindow_miniwindowTitle = 'miniwindowTitle';
  StrNSWindow_setDocumentEdited = 'setDocumentEdited:';
  StrNSWindow_isDocumentEdited = 'isDocumentEdited';
  StrNSWindow_isVisible = 'isVisible';
  StrNSWindow_isKeyWindow = 'isKeyWindow';
  StrNSWindow_isMainWindow = 'isMainWindow';
  StrNSWindow_canBecomeKeyWindow = 'canBecomeKeyWindow';
  StrNSWindow_canBecomeMainWindow = 'canBecomeMainWindow';
  StrNSWindow_makeKeyWindow = 'makeKeyWindow';
  StrNSWindow_makeMainWindow = 'makeMainWindow';
  StrNSWindow_becomeKeyWindow = 'becomeKeyWindow';
  StrNSWindow_resignKeyWindow = 'resignKeyWindow';
  StrNSWindow_becomeMainWindow = 'becomeMainWindow';
  StrNSWindow_resignMainWindow = 'resignMainWindow';
  StrNSWindow_worksWhenModal = 'worksWhenModal';
  StrNSWindow_convertBaseToScreen = 'convertBaseToScreen:';
  StrNSWindow_convertScreenToBase = 'convertScreenToBase:';
  StrNSWindow_performClose = 'performClose:';
  StrNSWindow_performMiniaturize = 'performMiniaturize:';
  StrNSWindow_performZoom = 'performZoom:';
  StrNSWindow_gState = 'gState';
  StrNSWindow_setOneShot = 'setOneShot:';
  StrNSWindow_isOneShot = 'isOneShot';
  StrNSWindow_dataWithEPSInsideRect = 'dataWithEPSInsideRect:';
  StrNSWindow_dataWithPDFInsideRect = 'dataWithPDFInsideRect:';
  StrNSWindow_print = 'print:';
  StrNSWindow_disableCursorRects = 'disableCursorRects';
  StrNSWindow_enableCursorRects = 'enableCursorRects';
  StrNSWindow_discardCursorRects = 'discardCursorRects';
  StrNSWindow_areCursorRectsEnabled = 'areCursorRectsEnabled';
  StrNSWindow_invalidateCursorRectsForView = 'invalidateCursorRectsForView:';
  StrNSWindow_resetCursorRects = 'resetCursorRects';
  StrNSWindow_setAllowsToolTipsWhenApplicationIsInactive = 'setAllowsToolTipsWhenApplicationIsInactive:';
  StrNSWindow_allowsToolTipsWhenApplicationIsInactive = 'allowsToolTipsWhenApplicationIsInactive';
  StrNSWindow_setBackingType = 'setBackingType:';
  StrNSWindow_backingType = 'backingType';
  StrNSWindow_setLevel = 'setLevel:';
  StrNSWindow_level = 'level';
  StrNSWindow_setDepthLimit = 'setDepthLimit:';
  StrNSWindow_depthLimit = 'depthLimit';
  StrNSWindow_setDynamicDepthLimit = 'setDynamicDepthLimit:';
  StrNSWindow_hasDynamicDepthLimit = 'hasDynamicDepthLimit';
  StrNSWindow_screen = 'screen';
  StrNSWindow_deepestScreen = 'deepestScreen';
  StrNSWindow_canStoreColor = 'canStoreColor';
  StrNSWindow_setHasShadow = 'setHasShadow:';
  StrNSWindow_hasShadow = 'hasShadow';
  StrNSWindow_invalidateShadow = 'invalidateShadow';
  StrNSWindow_setAlphaValue = 'setAlphaValue:';
  StrNSWindow_alphaValue = 'alphaValue';
  StrNSWindow_setOpaque = 'setOpaque:';
  StrNSWindow_isOpaque = 'isOpaque';
  StrNSWindow_displaysWhenScreenProfileChanges = 'displaysWhenScreenProfileChanges';
  StrNSWindow_setDisplaysWhenScreenProfileChanges = 'setDisplaysWhenScreenProfileChanges:';
  StrNSWindow_disableScreenUpdatesUntilFlush = 'disableScreenUpdatesUntilFlush';
  StrNSWindow_stringWithSavedFrame = 'stringWithSavedFrame';
  StrNSWindow_setFrameFromString = 'setFrameFromString:';
  StrNSWindow_saveFrameUsingName = 'saveFrameUsingName:';
  StrNSWindow_setFrameUsingName_force = 'setFrameUsingName:force:';
  StrNSWindow_setFrameUsingName = 'setFrameUsingName:';
  StrNSWindow_setFrameAutosaveName = 'setFrameAutosaveName:';
  StrNSWindow_frameAutosaveName = 'frameAutosaveName';
  StrNSWindow_removeFrameUsingName = 'removeFrameUsingName:';
  StrNSWindow_cacheImageInRect = 'cacheImageInRect:';
  StrNSWindow_restoreCachedImage = 'restoreCachedImage';
  StrNSWindow_discardCachedImage = 'discardCachedImage';
  StrNSWindow_minSize = 'minSize';
  StrNSWindow_maxSize = 'maxSize';
  StrNSWindow_setMinSize = 'setMinSize:';
  StrNSWindow_setMaxSize = 'setMaxSize:';
  StrNSWindow_contentMinSize = 'contentMinSize';
  StrNSWindow_contentMaxSize = 'contentMaxSize';
  StrNSWindow_setContentMinSize = 'setContentMinSize:';
  StrNSWindow_setContentMaxSize = 'setContentMaxSize:';
  StrNSWindow_nextEventMatchingMask = 'nextEventMatchingMask:';
  StrNSWindow_nextEventMatchingMask_untilDate_inMode_dequeue = 'nextEventMatchingMask:untilDate:inMode:dequeue:';
  StrNSWindow_discardEventsMatchingMask_beforeEvent = 'discardEventsMatchingMask:beforeEvent:';
  StrNSWindow_postEvent_atStart = 'postEvent:atStart:';
  StrNSWindow_currentEvent = 'currentEvent';
  StrNSWindow_setAcceptsMouseMovedEvents = 'setAcceptsMouseMovedEvents:';
  StrNSWindow_acceptsMouseMovedEvents = 'acceptsMouseMovedEvents';
  StrNSWindow_setIgnoresMouseEvents = 'setIgnoresMouseEvents:';
  StrNSWindow_ignoresMouseEvents = 'ignoresMouseEvents';
  StrNSWindow_deviceDescription = 'deviceDescription';
  StrNSWindow_sendEvent = 'sendEvent:';
  StrNSWindow_mouseLocationOutsideOfEventStream = 'mouseLocationOutsideOfEventStream';
  StrNSWindow_menuChanged = 'menuChanged:';
  StrNSWindow_windowController = 'windowController';
  StrNSWindow_setWindowController = 'setWindowController:';
  StrNSWindow_isSheet = 'isSheet';
  StrNSWindow_attachedSheet = 'attachedSheet';
  StrNSWindow_standardWindowButton_forStyleMask = 'standardWindowButton:forStyleMask:';
  StrNSWindow_standardWindowButton = 'standardWindowButton:';
  StrNSWindow_addChildWindow_ordered = 'addChildWindow:ordered:';
  StrNSWindow_removeChildWindow = 'removeChildWindow:';
  StrNSWindow_childWindows = 'childWindows';
  StrNSWindow_parentWindow = 'parentWindow';
  StrNSWindow_setParentWindow = 'setParentWindow:';
  StrNSWindow_graphicsContext = 'graphicsContext';
  StrNSWindow_userSpaceScaleFactor = 'userSpaceScaleFactor';

  StrNSWindow_setInitialFirstResponder = 'setInitialFirstResponder:';
  StrNSWindow_initialFirstResponder = 'initialFirstResponder';
  StrNSWindow_selectNextKeyView = 'selectNextKeyView:';
  StrNSWindow_selectPreviousKeyView = 'selectPreviousKeyView:';
  StrNSWindow_selectKeyViewFollowingView = 'selectKeyViewFollowingView:';
  StrNSWindow_selectKeyViewPrecedingView = 'selectKeyViewPrecedingView:';
  StrNSWindow_keyViewSelectionDirection = 'keyViewSelectionDirection';
  StrNSWindow_setDefaultButtonCell = 'setDefaultButtonCell:';
  StrNSWindow_defaultButtonCell = 'defaultButtonCell';
  StrNSWindow_disableKeyEquivalentForDefaultButtonCell = 'disableKeyEquivalentForDefaultButtonCell';
  StrNSWindow_enableKeyEquivalentForDefaultButtonCell = 'enableKeyEquivalentForDefaultButtonCell';
  StrNSWindow_setAutorecalculatesKeyViewLoop = 'setAutorecalculatesKeyViewLoop:';
  StrNSWindow_autorecalculatesKeyViewLoop = 'autorecalculatesKeyViewLoop';
  StrNSWindow_recalculateKeyViewLoop = 'recalculateKeyViewLoop';

  StrNSWindow_setToolbar = 'setToolbar:';
  StrNSWindow_toolbar = 'toolbar';
  StrNSWindow_toggleToolbarShown = 'toggleToolbarShown:';
  StrNSWindow_runToolbarCustomizationPalette = 'runToolbarCustomizationPalette:';
  StrNSWindow_setShowsToolbarButton = 'setShowsToolbarButton:';
  StrNSWindow_showsToolbarButton = 'showsToolbarButton';

  StrNSWindow_dragImage_at_offset_event_pasteboard_source_slideBack = 'dragImage:at:offset:event:pasteboard:source:slideBack:';
  StrNSWindow_registerForDraggedTypes = 'registerForDraggedTypes:';
  StrNSWindow_unregisterDraggedTypes = 'unregisterDraggedTypes';

  StrNSWindow_windowHandle = 'windowHandle';

  StrNSWindow_initWithWindowRef = 'initWithWindowRef:';
  StrNSWindow_windowRef = 'windowRef';

{ NSWindow }

class function NSWindow.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSWindow_NSWindow);
end;

class function NSWindow.frameRectForContentRect_styleMask(_cRect: NSRect; _aStyle: LongWord): NSRect;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_cRect: NSRect; _aStyle: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, getClass, sel_registerName(PChar(StrNSWindow_frameRectForContentRect_styleMask)), _cRect, _aStyle);
end;

class function NSWindow.contentRectForFrameRect_styleMask(_fRect: NSRect; _aStyle: LongWord): NSRect;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_fRect: NSRect; _aStyle: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, getClass, sel_registerName(PChar(StrNSWindow_contentRectForFrameRect_styleMask)), _fRect, _aStyle);
end;

class function NSWindow.minFrameWidthWithTitle_styleMask(_aTitle: CFStringRef; _aStyle: LongWord): Single;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aTitle: CFStringRef; _aStyle: LongWord): Single; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Single(vmethod(getClass, sel_registerName(PChar(StrNSWindow_minFrameWidthWithTitle_styleMask)), _aTitle, _aStyle));
end;

class function NSWindow.defaultDepthLimit: NSWindowDepth;
begin
  Result := NSWindowDepth(objc_msgSend(getClass, sel_registerName(PChar(StrNSWindow_defaultDepthLimit)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
function NSWindow.frameRectForContentRect(_contentRect: NSRect): NSRect;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_contentRect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSWindow_frameRectForContentRect)), _contentRect);
end;

function NSWindow.contentRectForFrameRect(_frameRect: NSRect): NSRect;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_frameRect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSWindow_contentRectForFrameRect)), _frameRect);
end;

{.$endif}
constructor NSWindow.initWithContentRect_styleMask_backing_defer(_contentRect: NSRect; _aStyle: LongWord; _bufferingType: NSBackingStoreType; _flag: LongBool);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_contentRect: NSRect; _aStyle: LongWord; _bufferingType: NSBackingStoreType; _flag: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSWindow_initWithContentRect_styleMask_backing_defer)), _contentRect, _aStyle, _bufferingType, _flag);
end;

constructor NSWindow.initWithContentRect_styleMask_backing_defer_screen(_contentRect: NSRect; _aStyle: LongWord; _bufferingType: NSBackingStoreType; _flag: LongBool; _screen: lobjc.id {NSScreen});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_contentRect: NSRect; _aStyle: LongWord; _bufferingType: NSBackingStoreType; _flag: LongBool; _screen: lobjc.id {NSScreen}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSWindow_initWithContentRect_styleMask_backing_defer_screen)), _contentRect, _aStyle, _bufferingType, _flag, _screen);
end;

function NSWindow.title: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_title)), []));
end;

procedure NSWindow.setTitle(_aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setTitle)), _aString);
end;

function NSWindow.representedFilename: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_representedFilename)), []));
end;

procedure NSWindow.setRepresentedFilename(_aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setRepresentedFilename)), _aString);
end;

procedure NSWindow.setTitleWithRepresentedFilename(_filename: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_filename: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setTitleWithRepresentedFilename)), _filename);
end;

procedure NSWindow.setExcludedFromWindowsMenu(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setExcludedFromWindowsMenu)), _flag);
end;

function NSWindow.isExcludedFromWindowsMenu: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isExcludedFromWindowsMenu)), []));
end;

procedure NSWindow.setContentView(_aView: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setContentView)), _aView);
end;

function NSWindow.contentView: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_contentView)), []));
end;

procedure NSWindow.setDelegate(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setDelegate)), _anObject);
end;

function NSWindow.delegate: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_delegate)), []));
end;

function NSWindow.windowNumber: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_windowNumber)), []));
end;

function NSWindow.styleMask: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_styleMask)), []));
end;

function NSWindow.fieldEditor_forObject(_createFlag: LongBool; _anObject: lobjc.id): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_createFlag: LongBool; _anObject: lobjc.id): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSWindow_fieldEditor_forObject)), _createFlag, _anObject));
end;

procedure NSWindow.endEditingFor(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_endEditingFor)), _anObject);
end;

function NSWindow.constrainFrameRect_toScreen(_frameRect: NSRect; _screen: lobjc.id {NSScreen}): NSRect;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_frameRect: NSRect; _screen: lobjc.id {NSScreen}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSWindow_constrainFrameRect_toScreen)), _frameRect, _screen);
end;

procedure NSWindow.setFrame_display(_frameRect: NSRect; _flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_frameRect: NSRect; _flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setFrame_display)), _frameRect, _flag);
end;

procedure NSWindow.setContentSize(_aSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setContentSize)), _aSize);
end;

procedure NSWindow.setFrameOrigin(_aPoint: NSPoint);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aPoint: NSPoint); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setFrameOrigin)), _aPoint);
end;

procedure NSWindow.setFrameTopLeftPoint(_aPoint: NSPoint);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aPoint: NSPoint); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setFrameTopLeftPoint)), _aPoint);
end;

function NSWindow.cascadeTopLeftFromPoint(_topLeftPoint: NSPoint): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_topLeftPoint: NSPoint); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSWindow_cascadeTopLeftFromPoint)), _topLeftPoint);
end;

function NSWindow.frame: NSRect;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSWindow_frame)), []);
end;

function NSWindow.animationResizeTime(_newFrame: NSRect): NSTimeInterval;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_newFrame: NSRect): NSTimeInterval; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_stret);
  Result := vmethod(Handle, sel_registerName(PChar(StrNSWindow_animationResizeTime)), _newFrame);
end;

procedure NSWindow.setFrame_display_animate(_frameRect: NSRect; _displayFlag: LongBool; _animateFlag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_frameRect: NSRect; _displayFlag: LongBool; _animateFlag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setFrame_display_animate)), _frameRect, _displayFlag, _animateFlag);
end;

procedure NSWindow.setShowsResizeIndicator(_show: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_show: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setShowsResizeIndicator)), _show);
end;

function NSWindow.showsResizeIndicator: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_showsResizeIndicator)), []));
end;

procedure NSWindow.setResizeIncrements(_increments: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_increments: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setResizeIncrements)), _increments);
end;

function NSWindow.resizeIncrements: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSWindow_resizeIncrements)), []);
end;

procedure NSWindow.setAspectRatio(_ratio: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_ratio: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setAspectRatio)), _ratio);
end;

function NSWindow.aspectRatio: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSWindow_aspectRatio)), []);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSWindow.setContentResizeIncrements(_increments: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_increments: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setContentResizeIncrements)), _increments);
end;

function NSWindow.contentResizeIncrements: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSWindow_contentResizeIncrements)), []);
end;

procedure NSWindow.setContentAspectRatio(_ratio: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_ratio: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setContentAspectRatio)), _ratio);
end;

function NSWindow.contentAspectRatio: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSWindow_contentAspectRatio)), []);
end;

{.$endif}
procedure NSWindow.useOptimizedDrawing(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_useOptimizedDrawing)), _flag);
end;

procedure NSWindow.disableFlushWindow;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_disableFlushWindow)), []);
end;

procedure NSWindow.enableFlushWindow;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_enableFlushWindow)), []);
end;

function NSWindow.isFlushWindowDisabled: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isFlushWindowDisabled)), []));
end;

procedure NSWindow.flushWindow;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_flushWindow)), []);
end;

procedure NSWindow.flushWindowIfNeeded;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_flushWindowIfNeeded)), []);
end;

procedure NSWindow.setViewsNeedDisplay(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setViewsNeedDisplay)), _flag);
end;

function NSWindow.viewsNeedDisplay: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_viewsNeedDisplay)), []));
end;

procedure NSWindow.displayIfNeeded;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_displayIfNeeded)), []);
end;

procedure NSWindow.display;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_display)), []);
end;

procedure NSWindow.setAutodisplay(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setAutodisplay)), _flag);
end;

function NSWindow.isAutodisplay: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isAutodisplay)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSWindow.preservesContentDuringLiveResize: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_preservesContentDuringLiveResize)), []));
end;

procedure NSWindow.setPreservesContentDuringLiveResize(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setPreservesContentDuringLiveResize)), _flag);
end;

{.$endif}
procedure NSWindow.update;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_update)), []);
end;

function NSWindow.makeFirstResponder(_aResponder: lobjc.id {NSResponder}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aResponder: lobjc.id {NSResponder}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSWindow_makeFirstResponder)), _aResponder));
end;

function NSWindow.firstResponder: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_firstResponder)), []));
end;

function NSWindow.resizeFlags: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_resizeFlags)), []));
end;

procedure NSWindow.keyDown(_theEvent: lobjc.id {NSEvent});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_theEvent: lobjc.id {NSEvent}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_keyDown)), _theEvent);
end;

procedure NSWindow.close;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_close)), []);
end;

procedure NSWindow.setReleasedWhenClosed(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setReleasedWhenClosed)), _flag);
end;

function NSWindow.isReleasedWhenClosed: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isReleasedWhenClosed)), []));
end;

procedure NSWindow.miniaturize(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_miniaturize)), _sender);
end;

procedure NSWindow.deminiaturize(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_deminiaturize)), _sender);
end;

function NSWindow.isZoomed: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isZoomed)), []));
end;

procedure NSWindow.zoom(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_zoom)), _sender);
end;

function NSWindow.isMiniaturized: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isMiniaturized)), []));
end;

function NSWindow.tryToPerform_with(_anAction: SEL; _anObject: lobjc.id): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_anAction: SEL; _anObject: lobjc.id): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSWindow_tryToPerform_with)), _anAction, _anObject));
end;

function NSWindow.validRequestorForSendType_returnType(_sendType: CFStringRef; _returnType: CFStringRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_sendType: CFStringRef; _returnType: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSWindow_validRequestorForSendType_returnType)), _sendType, _returnType));
end;

procedure NSWindow.setBackgroundColor(_color: lobjc.id {NSColor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_color: lobjc.id {NSColor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setBackgroundColor)), _color);
end;

function NSWindow.backgroundColor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_backgroundColor)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
procedure NSWindow.setMovableByWindowBackground(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setMovableByWindowBackground)), _flag);
end;

function NSWindow.isMovableByWindowBackground: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isMovableByWindowBackground)), []));
end;

{.$endif}
procedure NSWindow.setHidesOnDeactivate(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setHidesOnDeactivate)), _flag);
end;

function NSWindow.hidesOnDeactivate: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_hidesOnDeactivate)), []));
end;

procedure NSWindow.setCanHide(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setCanHide)), _flag);
end;

function NSWindow.canHide: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_canHide)), []));
end;

procedure NSWindow.center;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_center)), []);
end;

procedure NSWindow.makeKeyAndOrderFront(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_makeKeyAndOrderFront)), _sender);
end;

procedure NSWindow.orderFront(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_orderFront)), _sender);
end;

procedure NSWindow.orderBack(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_orderBack)), _sender);
end;

procedure NSWindow.orderOut(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_orderOut)), _sender);
end;

procedure NSWindow.orderWindow_relativeTo(_place: NSWindowOrderingMode; _otherWin: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_place: NSWindowOrderingMode; _otherWin: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_orderWindow_relativeTo)), _place, _otherWin);
end;

procedure NSWindow.orderFrontRegardless;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_orderFrontRegardless)), []);
end;

procedure NSWindow.setMiniwindowImage(_image: lobjc.id {NSImage});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_image: lobjc.id {NSImage}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setMiniwindowImage)), _image);
end;

procedure NSWindow.setMiniwindowTitle(_title: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_title: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setMiniwindowTitle)), _title);
end;

function NSWindow.miniwindowImage: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_miniwindowImage)), []));
end;

function NSWindow.miniwindowTitle: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_miniwindowTitle)), []));
end;

procedure NSWindow.setDocumentEdited(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setDocumentEdited)), _flag);
end;

function NSWindow.isDocumentEdited: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isDocumentEdited)), []));
end;

function NSWindow.isVisible: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isVisible)), []));
end;

function NSWindow.isKeyWindow: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isKeyWindow)), []));
end;

function NSWindow.isMainWindow: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isMainWindow)), []));
end;

function NSWindow.canBecomeKeyWindow: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_canBecomeKeyWindow)), []));
end;

function NSWindow.canBecomeMainWindow: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_canBecomeMainWindow)), []));
end;

procedure NSWindow.makeKeyWindow;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_makeKeyWindow)), []);
end;

procedure NSWindow.makeMainWindow;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_makeMainWindow)), []);
end;

procedure NSWindow.becomeKeyWindow;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_becomeKeyWindow)), []);
end;

procedure NSWindow.resignKeyWindow;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_resignKeyWindow)), []);
end;

procedure NSWindow.becomeMainWindow;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_becomeMainWindow)), []);
end;

procedure NSWindow.resignMainWindow;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_resignMainWindow)), []);
end;

function NSWindow.worksWhenModal: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_worksWhenModal)), []));
end;

function NSWindow.convertBaseToScreen(_aPoint: NSPoint): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aPoint: NSPoint); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSWindow_convertBaseToScreen)), _aPoint);
end;

function NSWindow.convertScreenToBase(_aPoint: NSPoint): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aPoint: NSPoint); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSWindow_convertScreenToBase)), _aPoint);
end;

procedure NSWindow.performClose(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_performClose)), _sender);
end;

procedure NSWindow.performMiniaturize(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_performMiniaturize)), _sender);
end;

procedure NSWindow.performZoom(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_performZoom)), _sender);
end;

function NSWindow.gState: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_gState)), []));
end;

procedure NSWindow.setOneShot(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setOneShot)), _flag);
end;

function NSWindow.isOneShot: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isOneShot)), []));
end;

function NSWindow.dataWithEPSInsideRect(_rect: NSRect): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_rect: NSRect): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSWindow_dataWithEPSInsideRect)), _rect));
end;

function NSWindow.dataWithPDFInsideRect(_rect: NSRect): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_rect: NSRect): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSWindow_dataWithPDFInsideRect)), _rect));
end;

procedure NSWindow.print(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_print)), _sender);
end;

procedure NSWindow.disableCursorRects;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_disableCursorRects)), []);
end;

procedure NSWindow.enableCursorRects;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_enableCursorRects)), []);
end;

procedure NSWindow.discardCursorRects;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_discardCursorRects)), []);
end;

function NSWindow.areCursorRectsEnabled: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_areCursorRectsEnabled)), []));
end;

procedure NSWindow.invalidateCursorRectsForView(_aView: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_invalidateCursorRectsForView)), _aView);
end;

procedure NSWindow.resetCursorRects;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_resetCursorRects)), []);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSWindow.setAllowsToolTipsWhenApplicationIsInactive(_allowWhenInactive: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_allowWhenInactive: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setAllowsToolTipsWhenApplicationIsInactive)), _allowWhenInactive);
end;

function NSWindow.allowsToolTipsWhenApplicationIsInactive: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_allowsToolTipsWhenApplicationIsInactive)), []));
end;

{.$endif}
procedure NSWindow.setBackingType(_bufferingType: NSBackingStoreType);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_bufferingType: NSBackingStoreType); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setBackingType)), _bufferingType);
end;

function NSWindow.backingType: NSBackingStoreType;
begin
  Result := NSBackingStoreType(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_backingType)), []));
end;

procedure NSWindow.setLevel(_newLevel: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newLevel: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setLevel)), _newLevel);
end;

function NSWindow.level: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_level)), []));
end;

procedure NSWindow.setDepthLimit(_limit: NSWindowDepth);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_limit: NSWindowDepth); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setDepthLimit)), _limit);
end;

function NSWindow.depthLimit: NSWindowDepth;
begin
  Result := NSWindowDepth(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_depthLimit)), []));
end;

procedure NSWindow.setDynamicDepthLimit(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setDynamicDepthLimit)), _flag);
end;

function NSWindow.hasDynamicDepthLimit: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_hasDynamicDepthLimit)), []));
end;

function NSWindow.screen: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_screen)), []));
end;

function NSWindow.deepestScreen: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_deepestScreen)), []));
end;

function NSWindow.canStoreColor: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_canStoreColor)), []));
end;

procedure NSWindow.setHasShadow(_hasShadow: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_hasShadow: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setHasShadow)), _hasShadow);
end;

function NSWindow.hasShadow: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_hasShadow)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
procedure NSWindow.invalidateShadow;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_invalidateShadow)), []);
end;

{.$endif}
procedure NSWindow.setAlphaValue(_windowAlpha: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_windowAlpha: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setAlphaValue)), _windowAlpha);
end;

function NSWindow.alphaValue: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_alphaValue)), []));
end;

procedure NSWindow.setOpaque(_isOpaque: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_isOpaque: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setOpaque)), _isOpaque);
end;

function NSWindow.isOpaque: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isOpaque)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSWindow.displaysWhenScreenProfileChanges: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_displaysWhenScreenProfileChanges)), []));
end;

procedure NSWindow.setDisplaysWhenScreenProfileChanges(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setDisplaysWhenScreenProfileChanges)), _flag);
end;

procedure NSWindow.disableScreenUpdatesUntilFlush;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_disableScreenUpdatesUntilFlush)), []);
end;

{.$endif}
function NSWindow.stringWithSavedFrame: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_stringWithSavedFrame)), []));
end;

procedure NSWindow.setFrameFromString(__string: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__string: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setFrameFromString)), __string);
end;

procedure NSWindow.saveFrameUsingName(_name: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_name: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_saveFrameUsingName)), _name);
end;

function NSWindow.setFrameUsingName_force(_name: CFStringRef; _force: LongBool): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_name: CFStringRef; _force: LongBool): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSWindow_setFrameUsingName_force)), _name, _force));
end;

function NSWindow.setFrameUsingName(_name: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_name: CFStringRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSWindow_setFrameUsingName)), _name));
end;

function NSWindow.setFrameAutosaveName(_name: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_name: CFStringRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSWindow_setFrameAutosaveName)), _name));
end;

function NSWindow.frameAutosaveName: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_frameAutosaveName)), []));
end;

class procedure NSWindow.removeFrameUsingName(_name: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_name: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSWindow_removeFrameUsingName)), _name);
end;

procedure NSWindow.cacheImageInRect(_aRect: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aRect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_cacheImageInRect)), _aRect);
end;

procedure NSWindow.restoreCachedImage;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_restoreCachedImage)), []);
end;

procedure NSWindow.discardCachedImage;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_discardCachedImage)), []);
end;

function NSWindow.minSize: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSWindow_minSize)), []);
end;

function NSWindow.maxSize: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSWindow_maxSize)), []);
end;

procedure NSWindow.setMinSize(_size: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_size: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setMinSize)), _size);
end;

procedure NSWindow.setMaxSize(_size: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_size: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setMaxSize)), _size);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
function NSWindow.contentMinSize: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSWindow_contentMinSize)), []);
end;

function NSWindow.contentMaxSize: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSWindow_contentMaxSize)), []);
end;

procedure NSWindow.setContentMinSize(_size: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_size: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setContentMinSize)), _size);
end;

procedure NSWindow.setContentMaxSize(_size: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_size: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setContentMaxSize)), _size);
end;

{.$endif}
function NSWindow.nextEventMatchingMask(_mask: LongWord): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_mask: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSWindow_nextEventMatchingMask)), _mask));
end;

function NSWindow.nextEventMatchingMask_untilDate_inMode_dequeue(_mask: LongWord; _expiration: CFDateRef; _mode: CFStringRef; _deqFlag: LongBool): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_mask: LongWord; _expiration: CFDateRef; _mode: CFStringRef; _deqFlag: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSWindow_nextEventMatchingMask_untilDate_inMode_dequeue)), _mask, _expiration, _mode, _deqFlag));
end;

procedure NSWindow.discardEventsMatchingMask_beforeEvent(_mask: LongWord; _lastEvent: lobjc.id {NSEvent});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_mask: LongWord; _lastEvent: lobjc.id {NSEvent}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_discardEventsMatchingMask_beforeEvent)), _mask, _lastEvent);
end;

procedure NSWindow.postEvent_atStart(_event: lobjc.id {NSEvent}; _flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_event: lobjc.id {NSEvent}; _flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_postEvent_atStart)), _event, _flag);
end;

function NSWindow.currentEvent: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_currentEvent)), []));
end;

procedure NSWindow.setAcceptsMouseMovedEvents(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setAcceptsMouseMovedEvents)), _flag);
end;

function NSWindow.acceptsMouseMovedEvents: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_acceptsMouseMovedEvents)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
procedure NSWindow.setIgnoresMouseEvents(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setIgnoresMouseEvents)), _flag);
end;

function NSWindow.ignoresMouseEvents: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_ignoresMouseEvents)), []));
end;

{.$endif}
function NSWindow.deviceDescription: CFDictionaryRef;
begin
  Result := CFDictionaryRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_deviceDescription)), []));
end;

procedure NSWindow.sendEvent(_theEvent: lobjc.id {NSEvent});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_theEvent: lobjc.id {NSEvent}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_sendEvent)), _theEvent);
end;

function NSWindow.mouseLocationOutsideOfEventStream: NSPoint;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSWindow_mouseLocationOutsideOfEventStream)), []);
end;

class procedure NSWindow.menuChanged(_menu: lobjc.id {NSMenu});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_menu: lobjc.id {NSMenu}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSWindow_menuChanged)), _menu);
end;

function NSWindow.windowController: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_windowController)), []));
end;

procedure NSWindow.setWindowController(_windowController: lobjc.id {NSWindowController});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_windowController: lobjc.id {NSWindowController}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setWindowController)), _windowController);
end;

function NSWindow.isSheet: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_isSheet)), []));
end;

function NSWindow.attachedSheet: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_attachedSheet)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
class function NSWindow.standardWindowButton_forStyleMask(_b: NSWindowButton; _styleMask: LongWord): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_b: NSWindowButton; _styleMask: LongWord): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(getClass, sel_registerName(PChar(StrNSWindow_standardWindowButton_forStyleMask)), _b, _styleMask));
end;

function NSWindow.standardWindowButton(_b: NSWindowButton): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_b: NSWindowButton): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSWindow_standardWindowButton)), _b));
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
procedure NSWindow.addChildWindow_ordered(_childWin: lobjc.id {NSWindow}; _place: NSWindowOrderingMode);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_childWin: lobjc.id {NSWindow}; _place: NSWindowOrderingMode); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_addChildWindow_ordered)), _childWin, _place);
end;

procedure NSWindow.removeChildWindow(_childWin: lobjc.id {NSWindow});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_childWin: lobjc.id {NSWindow}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_removeChildWindow)), _childWin);
end;

function NSWindow.childWindows: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_childWindows)), []));
end;

function NSWindow.parentWindow: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_parentWindow)), []));
end;

procedure NSWindow.setParentWindow(_window: lobjc.id {NSWindow});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_window: lobjc.id {NSWindow}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setParentWindow)), _window);
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSWindow.graphicsContext: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_graphicsContext)), []));
end;

function NSWindow.userSpaceScaleFactor: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_userSpaceScaleFactor)), []));
end;

{.$endif}
procedure NSWindow.setInitialFirstResponder(_view: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_view: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setInitialFirstResponder)), _view);
end;

function NSWindow.initialFirstResponder: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_initialFirstResponder)), []));
end;

procedure NSWindow.selectNextKeyView(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_selectNextKeyView)), _sender);
end;

procedure NSWindow.selectPreviousKeyView(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_selectPreviousKeyView)), _sender);
end;

procedure NSWindow.selectKeyViewFollowingView(_aView: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_selectKeyViewFollowingView)), _aView);
end;

procedure NSWindow.selectKeyViewPrecedingView(_aView: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_selectKeyViewPrecedingView)), _aView);
end;

function NSWindow.keyViewSelectionDirection: NSSelectionDirection;
begin
  Result := NSSelectionDirection(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_keyViewSelectionDirection)), []));
end;

procedure NSWindow.setDefaultButtonCell(_defButt: lobjc.id {NSButtonCell});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_defButt: lobjc.id {NSButtonCell}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setDefaultButtonCell)), _defButt);
end;

function NSWindow.defaultButtonCell: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_defaultButtonCell)), []));
end;

procedure NSWindow.disableKeyEquivalentForDefaultButtonCell;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_disableKeyEquivalentForDefaultButtonCell)), []);
end;

procedure NSWindow.enableKeyEquivalentForDefaultButtonCell;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_enableKeyEquivalentForDefaultButtonCell)), []);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
procedure NSWindow.setAutorecalculatesKeyViewLoop(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setAutorecalculatesKeyViewLoop)), _flag);
end;

function NSWindow.autorecalculatesKeyViewLoop: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_autorecalculatesKeyViewLoop)), []));
end;

procedure NSWindow.recalculateKeyViewLoop;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_recalculateKeyViewLoop)), []);
end;

{.$endif}
procedure NSWindow.setToolbar(_toolbar: lobjc.id {NSToolbar});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_toolbar: lobjc.id {NSToolbar}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setToolbar)), _toolbar);
end;

function NSWindow.toolbar: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_toolbar)), []));
end;

procedure NSWindow.toggleToolbarShown(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_toggleToolbarShown)), _sender);
end;

procedure NSWindow.runToolbarCustomizationPalette(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_runToolbarCustomizationPalette)), _sender);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
procedure NSWindow.setShowsToolbarButton(_show: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_show: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_setShowsToolbarButton)), _show);
end;

function NSWindow.showsToolbarButton: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_showsToolbarButton)), []));
end;

{.$endif}
procedure NSWindow.dragImage_at_offset_event_pasteboard_source_slideBack(_anImage: lobjc.id {NSImage}; _baseLocation: NSPoint; _initialOffset: NSSize; _event: lobjc.id {NSEvent}; _pboard: lobjc.id {NSPasteboard}; _sourceObj: lobjc.id; _slideFlag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anImage: lobjc.id {NSImage}; _baseLocation: NSPoint; _initialOffset: NSSize; _event: lobjc.id {NSEvent}; _pboard: lobjc.id {NSPasteboard}; _sourceObj: lobjc.id; _slideFlag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_dragImage_at_offset_event_pasteboard_source_slideBack)), _anImage, _baseLocation, _initialOffset, _event, _pboard, _sourceObj, _slideFlag);
end;

procedure NSWindow.registerForDraggedTypes(_newTypes: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newTypes: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSWindow_registerForDraggedTypes)), _newTypes);
end;

procedure NSWindow.unregisterDraggedTypes;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_unregisterDraggedTypes)), []);
end;

function NSWindow.windowHandle: Pointer;
begin
  Result := Pointer(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_windowHandle)), []));
end;

constructor NSWindow.initWithWindowRef(_windowRef: Pointer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_windowRef: Pointer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSWindow_initWithWindowRef)), _windowRef);
end;

function NSWindow.windowRef: Pointer;
begin
  Result := Pointer(objc_msgSend(Handle, sel_registerName(PChar(StrNSWindow_windowRef)), []));
end;

{$endif}
