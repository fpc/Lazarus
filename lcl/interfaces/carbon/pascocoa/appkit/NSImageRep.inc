{%mainunit appkit.pas}
(*
	NSImageRep.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSIMAGEREP_PAS_H}
{$define NSIMAGEREP_PAS_H}

{$include AppKitDefines.inc}
//const
//  NSImageRepRegistryChangedNotification = NSImageRepRegistryDidChangeNotification { obsolete name };

  //  NSImageRepMatchesDevice indicates the value is variable, depending on the output
  // device. It can be passed in (or received back) as the value of bitsPerSample, pixelsWide,
  // and pixelsHigh.
const
  NSImageRepMatchesDevice = 0;

//APPKIT_EXTERN NSString *NSImageRepRegistryDidChangeNotification;
{$endif}
{$endif}

{$ifdef FORWARD}
  NSImageRep = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSIMAGEREP_PAS_C}
{$define NSIMAGEREP_PAS_C}

{$include AppKitDefines.inc}

  { NSImageRep }

  NSImageRep = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    //  Drawing methods. drawAtPoint: and drawInRect: go through draw after modifying the
    // CTM.
    function draw: LongBool;
    function drawAtPoint(_point: NSPoint): LongBool;
    function drawInRect(_rect: NSRect): LongBool;
    //  Methods to return info about the image. NSImageRep provides storage for all of these;
    // however, it's illegal to set them in some subclasses.
    procedure setSize(_aSize: NSSize);
    function size: NSSize;
    procedure setAlpha(_flag: LongBool);
    function hasAlpha: LongBool;
    procedure setOpaque(_flag: LongBool);
    function isOpaque: LongBool;
    procedure setColorSpaceName(__string: CFStringRef);
    function colorSpaceName: CFStringRef;
    procedure setBitsPerSample(_anInt: Integer);
    function bitsPerSample: Integer;
    procedure setPixelsWide(_anInt: Integer);
    function pixelsWide: Integer;
    procedure setPixelsHigh(_anInt: Integer);
    function pixelsHigh: Integer;
    //  The rest of the methods all deal with subclassers which can read/write data in files
    // or pasteboards.
    //  Registry management: Subclasses which deal with file & pasteboard types should register
    // themselves. These classes should also implement imageUnfilteredFileTypes, imageUnfilteredPasteboardTypes,
    // initWithData:, canInitWithData:, imageRepWithData:, and, if they have the ability
    // to read multiple images from a file, imageRepsWithData:. These last three should
    // not do any filtering; all filtering is automatic.
    class procedure registerImageRepClass(_imageRepClass: _Class);
    class procedure unregisterImageRepClass(_imageRepClass: _Class);
    class function registeredImageRepClasses: CFArrayRef;
    class function imageRepClassForFileType(__type: CFStringRef): _Class;
    class function imageRepClassForPasteboardType(__type: CFStringRef): _Class;
    class function imageRepClassForData(_data: CFDataRef): _Class;
    //  Should be overridden by subclassers to load an unfiltered image.
    class function canInitWithData(_data: CFDataRef): LongBool;
    //  Implemented by subclassers to indicate what data types they can deal with.
    class function imageUnfilteredFileTypes: CFArrayRef;
    class function imageUnfilteredPasteboardTypes: CFArrayRef;
    //  These expand the unfiltered lists returned by imageUnfilteredFileTypes and imageUnfilteredPasteboardTypes.
    class function imageFileTypes: CFArrayRef;
    class function imagePasteboardTypes: CFArrayRef;
    //  Convenience method: Checks to see if any of the types on the pasteboard can be understood
    // by a registered imagerep class after filtering or if the pasteboard contains a filename
    // that can be understood by a registered imagerep class after filtering. If sent to
    // a subclass, does this for just the types understood by the subclass.
    class function canInitWithPasteboard(_pasteboard: lobjc.id {NSPasteboard}): LongBool;
    //  Convenience methods: Checks to see if the provided file or pasteboard types can
    // be understood by a registered imagerep class after filtering; if so, calls imageRepsWithData:
    // or imageRepWithData:. If sent to a subclass, does this just for the types understood
    // by that subclass.
    class function imageRepsWithContentsOfFile(_filename: CFStringRef): CFArrayRef;
    constructor imageRepWithContentsOfFile(_filename: CFStringRef);
    class function imageRepsWithContentsOfURL(_url: CFURLRef): CFArrayRef;
    constructor imageRepWithContentsOfURL(_url: CFURLRef);
    class function imageRepsWithPasteboard(_pasteboard: lobjc.id {NSPasteboard}): CFArrayRef;
    constructor imageRepWithPasteboard(_pasteboard: lobjc.id {NSPasteboard});
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSImageRep_NSImageRep = 'NSImageRep';
  StrNSImageRep_draw = 'draw';
  StrNSImageRep_drawAtPoint = 'drawAtPoint:';
  StrNSImageRep_drawInRect = 'drawInRect:';
  StrNSImageRep_setSize = 'setSize:';
  StrNSImageRep_size = 'size';
  StrNSImageRep_setAlpha = 'setAlpha:';
  StrNSImageRep_hasAlpha = 'hasAlpha';
  StrNSImageRep_setOpaque = 'setOpaque:';
  StrNSImageRep_isOpaque = 'isOpaque';
  StrNSImageRep_setColorSpaceName = 'setColorSpaceName:';
  StrNSImageRep_colorSpaceName = 'colorSpaceName';
  StrNSImageRep_setBitsPerSample = 'setBitsPerSample:';
  StrNSImageRep_bitsPerSample = 'bitsPerSample';
  StrNSImageRep_setPixelsWide = 'setPixelsWide:';
  StrNSImageRep_pixelsWide = 'pixelsWide';
  StrNSImageRep_setPixelsHigh = 'setPixelsHigh:';
  StrNSImageRep_pixelsHigh = 'pixelsHigh';
  StrNSImageRep_registerImageRepClass = 'registerImageRepClass:';
  StrNSImageRep_unregisterImageRepClass = 'unregisterImageRepClass:';
  StrNSImageRep_registeredImageRepClasses = 'registeredImageRepClasses';
  StrNSImageRep_imageRepClassForFileType = 'imageRepClassForFileType:';
  StrNSImageRep_imageRepClassForPasteboardType = 'imageRepClassForPasteboardType:';
  StrNSImageRep_imageRepClassForData = 'imageRepClassForData:';
  StrNSImageRep_canInitWithData = 'canInitWithData:';
  StrNSImageRep_imageUnfilteredFileTypes = 'imageUnfilteredFileTypes';
  StrNSImageRep_imageUnfilteredPasteboardTypes = 'imageUnfilteredPasteboardTypes';
  StrNSImageRep_imageFileTypes = 'imageFileTypes';
  StrNSImageRep_imagePasteboardTypes = 'imagePasteboardTypes';
  StrNSImageRep_canInitWithPasteboard = 'canInitWithPasteboard:';
  StrNSImageRep_imageRepsWithContentsOfFile = 'imageRepsWithContentsOfFile:';
  StrNSImageRep_imageRepWithContentsOfFile = 'imageRepWithContentsOfFile:';
  StrNSImageRep_imageRepsWithContentsOfURL = 'imageRepsWithContentsOfURL:';
  StrNSImageRep_imageRepWithContentsOfURL = 'imageRepWithContentsOfURL:';
  StrNSImageRep_imageRepsWithPasteboard = 'imageRepsWithPasteboard:';
  StrNSImageRep_imageRepWithPasteboard = 'imageRepWithPasteboard:';

{ NSImageRep }

class function NSImageRep.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSImageRep_NSImageRep);
end;

function NSImageRep.draw: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImageRep_draw)), []));
end;

function NSImageRep.drawAtPoint(_point: NSPoint): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_point: NSPoint): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSImageRep_drawAtPoint)), _point));
end;

function NSImageRep.drawInRect(_rect: NSRect): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_rect: NSRect): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSImageRep_drawInRect)), _rect));
end;

procedure NSImageRep.setSize(_aSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImageRep_setSize)), _aSize);
end;

function NSImageRep.size: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSImageRep_size)), []);
end;

procedure NSImageRep.setAlpha(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImageRep_setAlpha)), _flag);
end;

function NSImageRep.hasAlpha: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImageRep_hasAlpha)), []));
end;

procedure NSImageRep.setOpaque(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImageRep_setOpaque)), _flag);
end;

function NSImageRep.isOpaque: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSImageRep_isOpaque)), []));
end;

procedure NSImageRep.setColorSpaceName(__string: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__string: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImageRep_setColorSpaceName)), __string);
end;

function NSImageRep.colorSpaceName: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSImageRep_colorSpaceName)), []));
end;

procedure NSImageRep.setBitsPerSample(_anInt: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anInt: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImageRep_setBitsPerSample)), _anInt);
end;

function NSImageRep.bitsPerSample: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSImageRep_bitsPerSample)), []));
end;

procedure NSImageRep.setPixelsWide(_anInt: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anInt: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImageRep_setPixelsWide)), _anInt);
end;

function NSImageRep.pixelsWide: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSImageRep_pixelsWide)), []));
end;

procedure NSImageRep.setPixelsHigh(_anInt: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anInt: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSImageRep_setPixelsHigh)), _anInt);
end;

function NSImageRep.pixelsHigh: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSImageRep_pixelsHigh)), []));
end;

class procedure NSImageRep.registerImageRepClass(_imageRepClass: _Class);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_imageRepClass: _Class); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSImageRep_registerImageRepClass)), _imageRepClass);
end;

class procedure NSImageRep.unregisterImageRepClass(_imageRepClass: _Class);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_imageRepClass: _Class); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSImageRep_unregisterImageRepClass)), _imageRepClass);
end;

class function NSImageRep.registeredImageRepClasses: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSImageRep_registeredImageRepClasses)), []));
end;

class function NSImageRep.imageRepClassForFileType(__type: CFStringRef): _Class;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__type: CFStringRef): _Class; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := _Class(vmethod(getClass, sel_registerName(PChar(StrNSImageRep_imageRepClassForFileType)), __type));
end;

class function NSImageRep.imageRepClassForPasteboardType(__type: CFStringRef): _Class;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__type: CFStringRef): _Class; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := _Class(vmethod(getClass, sel_registerName(PChar(StrNSImageRep_imageRepClassForPasteboardType)), __type));
end;

class function NSImageRep.imageRepClassForData(_data: CFDataRef): _Class;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_data: CFDataRef): _Class; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := _Class(vmethod(getClass, sel_registerName(PChar(StrNSImageRep_imageRepClassForData)), _data));
end;

class function NSImageRep.canInitWithData(_data: CFDataRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_data: CFDataRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(getClass, sel_registerName(PChar(StrNSImageRep_canInitWithData)), _data));
end;

class function NSImageRep.imageUnfilteredFileTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSImageRep_imageUnfilteredFileTypes)), []));
end;

class function NSImageRep.imageUnfilteredPasteboardTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSImageRep_imageUnfilteredPasteboardTypes)), []));
end;

class function NSImageRep.imageFileTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSImageRep_imageFileTypes)), []));
end;

class function NSImageRep.imagePasteboardTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSImageRep_imagePasteboardTypes)), []));
end;

class function NSImageRep.canInitWithPasteboard(_pasteboard: lobjc.id {NSPasteboard}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pasteboard: lobjc.id {NSPasteboard}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(getClass, sel_registerName(PChar(StrNSImageRep_canInitWithPasteboard)), _pasteboard));
end;

class function NSImageRep.imageRepsWithContentsOfFile(_filename: CFStringRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_filename: CFStringRef): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(getClass, sel_registerName(PChar(StrNSImageRep_imageRepsWithContentsOfFile)), _filename));
end;

constructor NSImageRep.imageRepWithContentsOfFile(_filename: CFStringRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_filename: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSImageRep_imageRepWithContentsOfFile)), _filename);
end;

class function NSImageRep.imageRepsWithContentsOfURL(_url: CFURLRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(getClass, sel_registerName(PChar(StrNSImageRep_imageRepsWithContentsOfURL)), _url));
end;

constructor NSImageRep.imageRepWithContentsOfURL(_url: CFURLRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_url: CFURLRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSImageRep_imageRepWithContentsOfURL)), _url);
end;

class function NSImageRep.imageRepsWithPasteboard(_pasteboard: lobjc.id {NSPasteboard}): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pasteboard: lobjc.id {NSPasteboard}): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(getClass, sel_registerName(PChar(StrNSImageRep_imageRepsWithPasteboard)), _pasteboard));
end;

constructor NSImageRep.imageRepWithPasteboard(_pasteboard: lobjc.id {NSPasteboard});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_pasteboard: lobjc.id {NSPasteboard}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSImageRep_imageRepWithPasteboard)), _pasteboard);
end;

{$endif}
