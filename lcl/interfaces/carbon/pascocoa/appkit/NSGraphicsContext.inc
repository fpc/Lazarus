{%mainunit appkit.pas}
(*
        NSGraphicsContext.h
        Application Kit
        Copyright (c) 1997-2004, Apple Computer, Inc.
        All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSGRAPHICSCONTEXT_PAS_H}
{$define NSGRAPHICSCONTEXT_PAS_H}

{$include AppKitDefines.inc}
{$include NSGraphics.inc}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
  {.$endif}

//APPKIT_EXTERN NSString *NSGraphicsContextDestinationAttributeName; // Can be an instance of NSWindow, NSMutableData, NSBitmapImageRep, or NSURL.
//APPKIT_EXTERN NSString *NSGraphicsContextRepresentationFormatAttributeName; // Specifies destination file format
//APPKIT_EXTERN NSString *NSGraphicsContextPSFormat;
//APPKIT_EXTERN NSString *NSGraphicsContextPDFFormat;
type
  NSImageInterpolation = (
    NSImageInterpolationDefault, 
    NSImageInterpolationNone, 
    NSImageInterpolationLow, 
    NSImageInterpolationHigh
  );


{$endif}
{$endif}

{$ifdef FORWARD}
  NSGraphicsContext = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSGRAPHICSCONTEXT_PAS_C}
{$define NSGRAPHICSCONTEXT_PAS_C}

{$include AppKitDefines.inc}
{$include NSGraphics.inc}

  { NSGraphicsContext }

  NSGraphicsContext = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    //  Instantiates from an appropriate concrete subclass depending on NSGraphicsContextDestinationAttributeName
    // attribute
    constructor graphicsContextWithAttributes(_attributes: CFDictionaryRef);
    //  Convenience cover method for [NSGraphicsContext graphicsContextWithAttributes:[NSDictionary
    // dictionaryWithObject:<NSWindow instance> forKey:NSGraphicsContextDestinationAttributeName]]
    constructor graphicsContextWithWindow(_window: lobjc.id {NSWindow});
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    //  Convenience cover method for [NSGraphicsContext graphicsContextWithAttributes:[NSDictionary
    // dictionaryWithObject:<NSBitmapImageRep instance> forKey:NSGraphicsContextDestinationAttributeName]].
    // Only NSBitmapImageRep instances with isPlanar == NO are supported.
    constructor graphicsContextWithBitmapImageRep(_bitmapRep: lobjc.id {NSBitmapImageRep});
    //  Instantiates from an arbitrary graphicsPort (usually CGContextRef).  initialFlippedState
    // is used to determine the return value from -isFlipped when no view is focused in
    // the context.
    constructor graphicsContextWithGraphicsPort_flipped(_graphicsPort: Pointer; _initialFlippedState: LongBool);
    {.$endif}
    //  Setting and identifying the current context in the thread
    constructor currentContext;
    class procedure setCurrentContext(_context: lobjc.id {NSGraphicsContext});
    //  Convenience cover method for [[NSGraphicsContext currentContext] isDrawingToScreen]
    class function currentContextDrawingToScreen: LongBool;
    //  Graphics state controls
    //  Calls -saveGraphicsContext, and pushes the current context on per-thread stack.
    // It's functional equivalent of PSgsave
    class procedure saveGraphicsState_;
    //  Pops a context from per-thread stack, makes it current, and calls -resotreGraphicsContext.
    // It's functional equivalent of PSgrestore
    class procedure restoreGraphicsState_;
    //  Makes gState's context current, and resets graphics state. The gState must be created
    // in the calling thread
    class procedure setGraphicsState(_gState: Integer);
    //  Returns attributes used to create this instance
    function attributes: CFDictionaryRef;
    //  Testing the drawing destination
    function isDrawingToScreen: LongBool;
    //  Controlling the context
    procedure saveGraphicsState;
    procedure restoreGraphicsState;
    procedure flushGraphics;
    //  Focus Stack support (abstract)
    function focusStack: Pointer;
    procedure setFocusStack(_stack: Pointer);
    //  Platform specific drawing context (usually CGContextRef)
    function graphicsPort: Pointer;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    //  Returns the flip state of the receiver.  The state is determined by messaging -isFlipped
    // to the focus view in the context.  If no view has focus, returns NO unless the receiver
    // is instantiated via graphicsContextWithGraphicsPort:drawingToScreen:flipped: with
    // initialFlippedState == YES.
    function isFlipped: LongBool;
    {.$endif}
    procedure setShouldAntialias(_antialias: LongBool);
    function shouldAntialias: LongBool;
    procedure setImageInterpolation(_interpolation: NSImageInterpolation);
    function imageInterpolation: NSImageInterpolation;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    procedure setPatternPhase(_phase: NSPoint);
    function patternPhase: NSPoint;
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    procedure setCompositingOperation(_operation: NSCompositingOperation);
    function compositingOperation: NSCompositingOperation;
    {.$endif}
    function CIContext: lobjc.id;{CIContext}
  end;

    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    {.$endif}
{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSGraphicsContext_NSGraphicsContext = 'NSGraphicsContext';
  StrNSGraphicsContext_graphicsContextWithAttributes = 'graphicsContextWithAttributes:';
  StrNSGraphicsContext_graphicsContextWithWindow = 'graphicsContextWithWindow:';
  StrNSGraphicsContext_graphicsContextWithBitmapImageRep = 'graphicsContextWithBitmapImageRep:';
  StrNSGraphicsContext_graphicsContextWithGraphicsPort_flipped = 'graphicsContextWithGraphicsPort:flipped:';
  StrNSGraphicsContext_currentContext = 'currentContext';
  StrNSGraphicsContext_setCurrentContext = 'setCurrentContext:';
  StrNSGraphicsContext_currentContextDrawingToScreen = 'currentContextDrawingToScreen';
  StrNSGraphicsContext_saveGraphicsState = 'saveGraphicsState';
  StrNSGraphicsContext_restoreGraphicsState = 'restoreGraphicsState';
  StrNSGraphicsContext_setGraphicsState = 'setGraphicsState:';
  StrNSGraphicsContext_attributes = 'attributes';
  StrNSGraphicsContext_isDrawingToScreen = 'isDrawingToScreen';
  StrNSGraphicsContext_flushGraphics = 'flushGraphics';
  StrNSGraphicsContext_focusStack = 'focusStack';
  StrNSGraphicsContext_setFocusStack = 'setFocusStack:';
  StrNSGraphicsContext_graphicsPort = 'graphicsPort';
  StrNSGraphicsContext_isFlipped = 'isFlipped';

  StrNSGraphicsContext_setShouldAntialias = 'setShouldAntialias:';
  StrNSGraphicsContext_shouldAntialias = 'shouldAntialias';
  StrNSGraphicsContext_setImageInterpolation = 'setImageInterpolation:';
  StrNSGraphicsContext_imageInterpolation = 'imageInterpolation';
  StrNSGraphicsContext_setPatternPhase = 'setPatternPhase:';
  StrNSGraphicsContext_patternPhase = 'patternPhase';
  StrNSGraphicsContext_setCompositingOperation = 'setCompositingOperation:';
  StrNSGraphicsContext_compositingOperation = 'compositingOperation';

  StrNSGraphicsContext_CIContext = 'CIContext';

{ NSGraphicsContext }

class function NSGraphicsContext.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSGraphicsContext_NSGraphicsContext);
end;

constructor NSGraphicsContext.graphicsContextWithAttributes(_attributes: CFDictionaryRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_attributes: CFDictionaryRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSGraphicsContext_graphicsContextWithAttributes)), _attributes);
end;

constructor NSGraphicsContext.graphicsContextWithWindow(_window: lobjc.id {NSWindow});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_window: lobjc.id {NSWindow}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSGraphicsContext_graphicsContextWithWindow)), _window);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
constructor NSGraphicsContext.graphicsContextWithBitmapImageRep(_bitmapRep: lobjc.id {NSBitmapImageRep});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bitmapRep: lobjc.id {NSBitmapImageRep}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSGraphicsContext_graphicsContextWithBitmapImageRep)), _bitmapRep);
end;

constructor NSGraphicsContext.graphicsContextWithGraphicsPort_flipped(_graphicsPort: Pointer; _initialFlippedState: LongBool);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_graphicsPort: Pointer; _initialFlippedState: LongBool): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSGraphicsContext_graphicsContextWithGraphicsPort_flipped)), _graphicsPort, _initialFlippedState);
end;

{.$endif}
constructor NSGraphicsContext.currentContext;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSGraphicsContext_currentContext)));
end;

class procedure NSGraphicsContext.setCurrentContext(_context: lobjc.id {NSGraphicsContext});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_context: lobjc.id {NSGraphicsContext}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSGraphicsContext_setCurrentContext)), _context);
end;

class function NSGraphicsContext.currentContextDrawingToScreen: LongBool;
begin
  Result := LongBool(objc_msgSend(getClass, sel_registerName(PChar(StrNSGraphicsContext_currentContextDrawingToScreen)), []));
end;

class procedure NSGraphicsContext.saveGraphicsState_;
begin
  objc_msgSend(getClass, sel_registerName(PChar(StrNSGraphicsContext_saveGraphicsState)), []);
end;

class procedure NSGraphicsContext.restoreGraphicsState_;
begin
  objc_msgSend(getClass, sel_registerName(PChar(StrNSGraphicsContext_restoreGraphicsState)), []);
end;

class procedure NSGraphicsContext.setGraphicsState(_gState: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_gState: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSGraphicsContext_setGraphicsState)), _gState);
end;

function NSGraphicsContext.attributes: CFDictionaryRef;
begin
  Result := CFDictionaryRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_attributes)), []));
end;

function NSGraphicsContext.isDrawingToScreen: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_isDrawingToScreen)), []));
end;

procedure NSGraphicsContext.saveGraphicsState;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_saveGraphicsState)), []);
end;

procedure NSGraphicsContext.restoreGraphicsState;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_restoreGraphicsState)), []);
end;

procedure NSGraphicsContext.flushGraphics;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_flushGraphics)), []);
end;

function NSGraphicsContext.focusStack: Pointer;
begin
  Result := Pointer(objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_focusStack)), []));
end;

procedure NSGraphicsContext.setFocusStack(_stack: Pointer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_stack: Pointer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSGraphicsContext_setFocusStack)), _stack);
end;

function NSGraphicsContext.graphicsPort: Pointer;
begin
  Result := Pointer(objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_graphicsPort)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSGraphicsContext.isFlipped: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_isFlipped)), []));
end;

{.$endif}
procedure NSGraphicsContext.setShouldAntialias(_antialias: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_antialias: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSGraphicsContext_setShouldAntialias)), _antialias);
end;

function NSGraphicsContext.shouldAntialias: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_shouldAntialias)), []));
end;

procedure NSGraphicsContext.setImageInterpolation(_interpolation: NSImageInterpolation);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_interpolation: NSImageInterpolation); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSGraphicsContext_setImageInterpolation)), _interpolation);
end;

function NSGraphicsContext.imageInterpolation: NSImageInterpolation;
begin
  Result := NSImageInterpolation(objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_imageInterpolation)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
procedure NSGraphicsContext.setPatternPhase(_phase: NSPoint);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_phase: NSPoint); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSGraphicsContext_setPatternPhase)), _phase);
end;

function NSGraphicsContext.patternPhase: NSPoint;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSGraphicsContext_patternPhase)), []);
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
procedure NSGraphicsContext.setCompositingOperation(_operation: NSCompositingOperation);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_operation: NSCompositingOperation); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSGraphicsContext_setCompositingOperation)), _operation);
end;

function NSGraphicsContext.compositingOperation: NSCompositingOperation;
begin
  Result := NSCompositingOperation(objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_compositingOperation)), []));
end;

{.$endif}
function NSGraphicsContext.CIContext: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSGraphicsContext_CIContext)), []));
end;

{$endif}
