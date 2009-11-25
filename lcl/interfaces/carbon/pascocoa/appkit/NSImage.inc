{%mainunit appkit.pas}
(*
	NSImage.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSIMAGE_PAS_H}
{$define NSIMAGE_PAS_H}

{$include NSGraphics.inc}
{$include NSBitmapImageRep.inc}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}
  {.$endif}

type
  NSImageLoadStatus = (
    NSImageLoadStatusCompleted, 
    NSImageLoadStatusCancelled, 
    NSImageLoadStatusInvalidData, 
    NSImageLoadStatusUnexpectedEOF, 
    NSImageLoadStatusReadError
  );


type
  NSImageCacheMode = (
    NSImageCacheDefault, 
    NSImageCacheAlways, 
    NSImageCacheBySize, 
    NSImageCacheNever
  );


{$endif}
{$endif}

{$ifdef FORWARD}
  NSImage = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSIMAGE_PAS_C}
{$define NSIMAGE_PAS_C}

{$include NSGraphics.inc}
{$include NSBitmapImageRep.inc}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    {.$endif}

  { NSImage }

  NSImage = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    constructor imageNamed(_name: CFStringRef);
    //  If this finds & creates the image, only name is saved when archived
    constructor initWithSize(_aSize: NSSize);
    constructor initWithData(_data: CFDataRef);
    //  When archived, saves contents
    constructor initWithContentsOfFile(_fileName: CFStringRef);
    //  When archived, saves contents
    constructor initWithContentsOfURL(_url: CFURLRef);
    //  When archived, saves contents
    constructor initByReferencingFile(_fileName: CFStringRef);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    constructor initByReferencingURL(_url: CFURLRef);
    {.$endif}
    constructor initWithPasteboard(_pasteboard: lobjc.id {NSPasteboard});
    procedure setSize(_aSize: NSSize);
    function size: NSSize;
    function setName(__string: CFStringRef): LongBool;
    function name: CFStringRef;
    procedure setScalesWhenResized(_flag: LongBool);
    function scalesWhenResized: LongBool;
    procedure setDataRetained(_flag: LongBool);
    function isDataRetained: LongBool;
    procedure setCachedSeparately(_flag: LongBool);
    function isCachedSeparately: LongBool;
    procedure setCacheDepthMatchesImageDepth(_flag: LongBool);
    function cacheDepthMatchesImageDepth: LongBool;
    procedure setBackgroundColor(_aColor: lobjc.id {NSColor});
    function backgroundColor: lobjc.id;{NSColor}
    procedure setUsesEPSOnResolutionMismatch(_flag: LongBool);
    function usesEPSOnResolutionMismatch: LongBool;
    procedure setPrefersColorMatch(_flag: LongBool);
    function prefersColorMatch: LongBool;
    procedure setMatchesOnMultipleResolution(_flag: LongBool);
    function matchesOnMultipleResolution: LongBool;
    procedure dissolveToPoint_fraction(_point: NSPoint; _aFloat: Single);
    procedure dissolveToPoint_fromRect_fraction(_point: NSPoint; _rect: NSRect; _aFloat: Single);
    procedure compositeToPoint_operation(_point: NSPoint; _op: NSCompositingOperation);
    procedure compositeToPoint_fromRect_operation(_point: NSPoint; _rect: NSRect; _op: NSCompositingOperation);
    procedure compositeToPoint_operation_fraction(_point: NSPoint; _op: NSCompositingOperation; _delta: Single);
    procedure compositeToPoint_fromRect_operation_fraction(_point: NSPoint; _rect: NSRect; _op: NSCompositingOperation; _delta: Single);
    procedure drawAtPoint_fromRect_operation_fraction(_point: NSPoint; _fromRect: NSRect; _op: NSCompositingOperation; _delta: Single);
    procedure drawInRect_fromRect_operation_fraction(_rect: NSRect; _fromRect: NSRect; _op: NSCompositingOperation; _delta: Single);
    function drawRepresentation_inRect(_imageRep: lobjc.id {NSImageRep}; _rect: NSRect): LongBool;
    procedure recache;
    function TIFFRepresentation: CFDataRef;
    function TIFFRepresentationUsingCompression_factor(_comp: NSTIFFCompression; _aFloat: Single): CFDataRef;
    function representations: CFArrayRef;
    procedure addRepresentations(_imageReps: CFArrayRef);
    procedure addRepresentation(_imageRep: lobjc.id {NSImageRep});
    procedure removeRepresentation(_imageRep: lobjc.id {NSImageRep});
    function isValid: LongBool;
    procedure lockFocus;
    procedure lockFocusOnRepresentation(_imageRepresentation: lobjc.id {NSImageRep});
    procedure unlockFocus;
    function bestRepresentationForDevice(_deviceDescription: CFDictionaryRef): lobjc.id;{NSImageRep}
    procedure setDelegate(_anObject: lobjc.id);
    function delegate: lobjc.id;
    //  These return union of all the types registered with NSImageRep.
    class function imageUnfilteredFileTypes: CFArrayRef;
    class function imageUnfilteredPasteboardTypes: CFArrayRef;
    class function imageFileTypes: CFArrayRef;
    class function imagePasteboardTypes: CFArrayRef;
    class function canInitWithPasteboard(_pasteboard: lobjc.id {NSPasteboard}): LongBool;
    procedure setFlipped(_flag: LongBool);
    function isFlipped: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    procedure cancelIncrementalLoad;
    procedure setCacheMode(_mode: NSImageCacheMode);
    function cacheMode: NSImageCacheMode;
    {.$endif}
    constructor initWithIconHandle(_icon: Pointer);
    constructor initWithBitmapHandle(_bitmap: Pointer);
  end;

    {.$endif}
{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSImage_NSImage = 'NSImage';
  StrNSImage_imageNamed = 'imageNamed:';
  StrNSImage_initWithSize = 'initWithSize:';
  StrNSImage_initWithData = 'initWithData:';
  StrNSImage_initWithContentsOfFile = 'initWithContentsOfFile:';
  StrNSImage_initWithContentsOfURL = 'initWithContentsOfURL:';
  StrNSImage_initByReferencingFile = 'initByReferencingFile:';
  StrNSImage_initByReferencingURL = 'initByReferencingURL:';
  StrNSImage_initWithPasteboard = 'initWithPasteboard:';
  StrNSImage_setSize = 'setSize:';
  StrNSImage_size = 'size';
  StrNSImage_setName = 'setName:';
  StrNSImage_name = 'name';
  StrNSImage_setScalesWhenResized = 'setScalesWhenResized:';
  StrNSImage_scalesWhenResized = 'scalesWhenResized';
  StrNSImage_setDataRetained = 'setDataRetained:';
  StrNSImage_isDataRetained = 'isDataRetained';
  StrNSImage_setCachedSeparately = 'setCachedSeparately:';
  StrNSImage_isCachedSeparately = 'isCachedSeparately';
  StrNSImage_setCacheDepthMatchesImageDepth = 'setCacheDepthMatchesImageDepth:';
  StrNSImage_cacheDepthMatchesImageDepth = 'cacheDepthMatchesImageDepth';
  StrNSImage_setBackgroundColor = 'setBackgroundColor:';
  StrNSImage_backgroundColor = 'backgroundColor';
  StrNSImage_setUsesEPSOnResolutionMismatch = 'setUsesEPSOnResolutionMismatch:';
  StrNSImage_usesEPSOnResolutionMismatch = 'usesEPSOnResolutionMismatch';
  StrNSImage_setPrefersColorMatch = 'setPrefersColorMatch:';
  StrNSImage_prefersColorMatch = 'prefersColorMatch';
  StrNSImage_setMatchesOnMultipleResolution = 'setMatchesOnMultipleResolution:';
  StrNSImage_matchesOnMultipleResolution = 'matchesOnMultipleResolution';
  StrNSImage_dissolveToPoint_fraction = 'dissolveToPoint:fraction:';
  StrNSImage_dissolveToPoint_fromRect_fraction = 'dissolveToPoint:fromRect:fraction:';
  StrNSImage_compositeToPoint_operation = 'compositeToPoint:operation:';
  StrNSImage_compositeToPoint_fromRect_operation = 'compositeToPoint:fromRect:operation:';
  StrNSImage_compositeToPoint_operation_fraction = 'compositeToPoint:operation:fraction:';
  StrNSImage_compositeToPoint_fromRect_operation_fraction = 'compositeToPoint:fromRect:operation:fraction:';
  StrNSImage_drawAtPoint_fromRect_operation_fraction = 'drawAtPoint:fromRect:operation:fraction:';
  StrNSImage_drawInRect_fromRect_operation_fraction = 'drawInRect:fromRect:operation:fraction:';
  StrNSImage_drawRepresentation_inRect = 'drawRepresentation:inRect:';
  StrNSImage_recache = 'recache';
  StrNSImage_TIFFRepresentation = 'TIFFRepresentation';
  StrNSImage_TIFFRepresentationUsingCompression_factor = 'TIFFRepresentationUsingCompression:factor:';
  StrNSImage_representations = 'representations';
  StrNSImage_addRepresentations = 'addRepresentations:';
  StrNSImage_addRepresentation = 'addRepresentation:';
  StrNSImage_removeRepresentation = 'removeRepresentation:';
  StrNSImage_isValid = 'isValid';
  StrNSImage_lockFocus = 'lockFocus';
  StrNSImage_lockFocusOnRepresentation = 'lockFocusOnRepresentation:';
  StrNSImage_unlockFocus = 'unlockFocus';
  StrNSImage_bestRepresentationForDevice = 'bestRepresentationForDevice:';
  StrNSImage_setDelegate = 'setDelegate:';
  StrNSImage_delegate = 'delegate';
  StrNSImage_imageUnfilteredFileTypes = 'imageUnfilteredFileTypes';
  StrNSImage_imageUnfilteredPasteboardTypes = 'imageUnfilteredPasteboardTypes';
  StrNSImage_imageFileTypes = 'imageFileTypes';
  StrNSImage_imagePasteboardTypes = 'imagePasteboardTypes';
  StrNSImage_canInitWithPasteboard = 'canInitWithPasteboard:';
  StrNSImage_setFlipped = 'setFlipped:';
  StrNSImage_isFlipped = 'isFlipped';
  StrNSImage_cancelIncrementalLoad = 'cancelIncrementalLoad';
  StrNSImage_setCacheMode = 'setCacheMode:';
  StrNSImage_cacheMode = 'cacheMode';

  StrNSImage_initWithIconHandle = 'initWithIconHandle:';
  StrNSImage_initWithBitmapHandle = 'initWithBitmapHandle:';

{ NSImage }

class function NSImage.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSImage_NSImage);
end;

constructor NSImage.imageNamed(_name: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_name: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSImage_imageNamed)), _name);
end;

constructor NSImage.initWithSize(_aSize: NSSize);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aSize: NSSize): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSImage_initWithSize)), _aSize);
end;

constructor NSImage.initWithData(_data: CFDataRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_data: CFDataRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSImage_initWithData)), _data);
end;

constructor NSImage.initWithContentsOfFile(_fileName: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fileName: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSImage_initWithContentsOfFile)), _fileName);
end;

constructor NSImage.initWithContentsOfURL(_url: CFURLRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSImage_initWithContentsOfURL)), _url);
end;

constructor NSImage.initByReferencingFile(_fileName: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fileName: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSImage_initByReferencingFile)), _fileName);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
constructor NSImage.initByReferencingURL(_url: CFURLRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSImage_initByReferencingURL)), _url);
end;

{.$endif}
constructor NSImage.initWithPasteboard(_pasteboard: lobjc.id {NSPasteboard});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pasteboard: lobjc.id {NSPasteboard}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSImage_initWithPasteboard)), _pasteboard);
end;

procedure NSImage.setSize(_aSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setSize)), _aSize);
end;

function NSImage.size: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSImage_size)), []);
end;

function NSImage.setName(__string: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__string: CFStringRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSImage_setName)), __string));
end;

function NSImage.name: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_name)), []));
end;

procedure NSImage.setScalesWhenResized(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setScalesWhenResized)), _flag);
end;

function NSImage.scalesWhenResized: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_scalesWhenResized)), []));
end;

procedure NSImage.setDataRetained(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setDataRetained)), _flag);
end;

function NSImage.isDataRetained: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_isDataRetained)), []));
end;

procedure NSImage.setCachedSeparately(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setCachedSeparately)), _flag);
end;

function NSImage.isCachedSeparately: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_isCachedSeparately)), []));
end;

procedure NSImage.setCacheDepthMatchesImageDepth(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setCacheDepthMatchesImageDepth)), _flag);
end;

function NSImage.cacheDepthMatchesImageDepth: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_cacheDepthMatchesImageDepth)), []));
end;

procedure NSImage.setBackgroundColor(_aColor: lobjc.id {NSColor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aColor: lobjc.id {NSColor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setBackgroundColor)), _aColor);
end;

function NSImage.backgroundColor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_backgroundColor)), []));
end;

procedure NSImage.setUsesEPSOnResolutionMismatch(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setUsesEPSOnResolutionMismatch)), _flag);
end;

function NSImage.usesEPSOnResolutionMismatch: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_usesEPSOnResolutionMismatch)), []));
end;

procedure NSImage.setPrefersColorMatch(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setPrefersColorMatch)), _flag);
end;

function NSImage.prefersColorMatch: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_prefersColorMatch)), []));
end;

procedure NSImage.setMatchesOnMultipleResolution(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setMatchesOnMultipleResolution)), _flag);
end;

function NSImage.matchesOnMultipleResolution: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_matchesOnMultipleResolution)), []));
end;

procedure NSImage.dissolveToPoint_fraction(_point: NSPoint; _aFloat: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_point: NSPoint; _aFloat: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_dissolveToPoint_fraction)), _point, _aFloat);
end;

procedure NSImage.dissolveToPoint_fromRect_fraction(_point: NSPoint; _rect: NSRect; _aFloat: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_point: NSPoint; _rect: NSRect; _aFloat: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_dissolveToPoint_fromRect_fraction)), _point, _rect, _aFloat);
end;

procedure NSImage.compositeToPoint_operation(_point: NSPoint; _op: NSCompositingOperation);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_point: NSPoint; _op: NSCompositingOperation); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_compositeToPoint_operation)), _point, _op);
end;

procedure NSImage.compositeToPoint_fromRect_operation(_point: NSPoint; _rect: NSRect; _op: NSCompositingOperation);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_point: NSPoint; _rect: NSRect; _op: NSCompositingOperation); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_compositeToPoint_fromRect_operation)), _point, _rect, _op);
end;

procedure NSImage.compositeToPoint_operation_fraction(_point: NSPoint; _op: NSCompositingOperation; _delta: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_point: NSPoint; _op: NSCompositingOperation; _delta: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_compositeToPoint_operation_fraction)), _point, _op, _delta);
end;

procedure NSImage.compositeToPoint_fromRect_operation_fraction(_point: NSPoint; _rect: NSRect; _op: NSCompositingOperation; _delta: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_point: NSPoint; _rect: NSRect; _op: NSCompositingOperation; _delta: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_compositeToPoint_fromRect_operation_fraction)), _point, _rect, _op, _delta);
end;

procedure NSImage.drawAtPoint_fromRect_operation_fraction(_point: NSPoint; _fromRect: NSRect; _op: NSCompositingOperation; _delta: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_point: NSPoint; _fromRect: NSRect; _op: NSCompositingOperation; _delta: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_drawAtPoint_fromRect_operation_fraction)), _point, _fromRect, _op, _delta);
end;

procedure NSImage.drawInRect_fromRect_operation_fraction(_rect: NSRect; _fromRect: NSRect; _op: NSCompositingOperation; _delta: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect; _fromRect: NSRect; _op: NSCompositingOperation; _delta: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_drawInRect_fromRect_operation_fraction)), _rect, _fromRect, _op, _delta);
end;

function NSImage.drawRepresentation_inRect(_imageRep: lobjc.id {NSImageRep}; _rect: NSRect): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_imageRep: lobjc.id {NSImageRep}; _rect: NSRect): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSImage_drawRepresentation_inRect)), _imageRep, _rect));
end;

procedure NSImage.recache;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_recache)), []);
end;

function NSImage.TIFFRepresentation: CFDataRef;
begin
  Result := CFDataRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_TIFFRepresentation)), []));
end;

function NSImage.TIFFRepresentationUsingCompression_factor(_comp: NSTIFFCompression; _aFloat: Single): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_comp: NSTIFFCompression; _aFloat: Single): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSImage_TIFFRepresentationUsingCompression_factor)), _comp, _aFloat));
end;

function NSImage.representations: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_representations)), []));
end;

procedure NSImage.addRepresentations(_imageReps: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_imageReps: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_addRepresentations)), _imageReps);
end;

procedure NSImage.addRepresentation(_imageRep: lobjc.id {NSImageRep});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_imageRep: lobjc.id {NSImageRep}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_addRepresentation)), _imageRep);
end;

procedure NSImage.removeRepresentation(_imageRep: lobjc.id {NSImageRep});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_imageRep: lobjc.id {NSImageRep}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_removeRepresentation)), _imageRep);
end;

function NSImage.isValid: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_isValid)), []));
end;

procedure NSImage.lockFocus;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_lockFocus)), []);
end;

procedure NSImage.lockFocusOnRepresentation(_imageRepresentation: lobjc.id {NSImageRep});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_imageRepresentation: lobjc.id {NSImageRep}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_lockFocusOnRepresentation)), _imageRepresentation);
end;

procedure NSImage.unlockFocus;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_unlockFocus)), []);
end;

function NSImage.bestRepresentationForDevice(_deviceDescription: CFDictionaryRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_deviceDescription: CFDictionaryRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSImage_bestRepresentationForDevice)), _deviceDescription));
end;

procedure NSImage.setDelegate(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setDelegate)), _anObject);
end;

function NSImage.delegate: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_delegate)), []));
end;

class function NSImage.imageUnfilteredFileTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSImage_imageUnfilteredFileTypes)), []));
end;

class function NSImage.imageUnfilteredPasteboardTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSImage_imageUnfilteredPasteboardTypes)), []));
end;

class function NSImage.imageFileTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSImage_imageFileTypes)), []));
end;

class function NSImage.imagePasteboardTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSImage_imagePasteboardTypes)), []));
end;

class function NSImage.canInitWithPasteboard(_pasteboard: lobjc.id {NSPasteboard}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pasteboard: lobjc.id {NSPasteboard}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(getClass, sel_registerName(PChar(StrNSImage_canInitWithPasteboard)), _pasteboard));
end;

procedure NSImage.setFlipped(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setFlipped)), _flag);
end;

function NSImage.isFlipped: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_isFlipped)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
procedure NSImage.cancelIncrementalLoad;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_cancelIncrementalLoad)), []);
end;

procedure NSImage.setCacheMode(_mode: NSImageCacheMode);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_mode: NSImageCacheMode); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImage_setCacheMode)), _mode);
end;

function NSImage.cacheMode: NSImageCacheMode;
begin
  Result := NSImageCacheMode(objc_msgSend(Handle, sel_registerName(PChar(StrNSImage_cacheMode)), []));
end;

{.$endif}
constructor NSImage.initWithIconHandle(_icon: Pointer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_icon: Pointer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSImage_initWithIconHandle)), _icon);
end;

constructor NSImage.initWithBitmapHandle(_bitmap: Pointer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bitmap: Pointer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSImage_initWithBitmapHandle)), _bitmap);
end;

{$endif}
