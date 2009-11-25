{%mainunit appkit.pas}
(*
	NSBitmapImageRep.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSBITMAPIMAGEREP_PAS_H}
{$define NSBITMAPIMAGEREP_PAS_H}

{$include NSImageRep.inc}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
  {.$endif}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
  {.$endif}
  {.$endif}

type
  NSTIFFCompression = (
    NSTIFFCompressionNone = 1, 
    NSTIFFCompressionCCITTFAX3 = 3, 
    NSTIFFCompressionCCITTFAX4 = 4, 
    NSTIFFCompressionLZW = 5, 
    NSTIFFCompressionJPEG = 6, 
    NSTIFFCompressionNEXT = 32766, 
    NSTIFFCompressionPackBits = 32773, 
    NSTIFFCompressionOldJPEG = 32865
  );


type
  NSBitmapImageFileType = (
    NSTIFFFileType, 
    NSBMPFileType, 
    NSGIFFileType, 
    NSJPEGFileType, 
    NSPNGFileType, 
    NSJPEG2000FileType
  );


type
  NSImageRepLoadStatus = (
    NSImageRepLoadStatusUnknownType = 1, 
    NSImageRepLoadStatusReadingHeader = 2, 
    NSImageRepLoadStatusWillNeedAllData = 3, 
    NSImageRepLoadStatusInvalidData = 4, 
    NSImageRepLoadStatusUnexpectedEOF = 5, 
    NSImageRepLoadStatusCompleted = 6
  );


type
  NSBitmapFormat = (
    NSAlphaFirstBitmapFormat = 1 shl 0, 
    NSAlphaNonpremultipliedBitmapFormat = 1 shl 1, 
    NSFloatingPointSamplesBitmapFormat = 1 shl 2
  );


//APPKIT_EXTERN NSString* NSImageCompressionMethod;	// TIFF input/output (NSTIFFCompression in NSNumber)
//APPKIT_EXTERN NSString* NSImageCompressionFactor;	// TIFF/JPEG input/output (float in NSNumber)
//APPKIT_EXTERN NSString* NSImageDitherTransparency;	// GIF output (BOOL in NSNumber)
//APPKIT_EXTERN NSString* NSImageRGBColorTable;		// GIF input/output (packed RGB in NSData)
//APPKIT_EXTERN NSString* NSImageInterlaced;		// PNG output (BOOL in NSNumber)
//APPKIT_EXTERN NSString* NSImageColorSyncProfileData;	// TIFF,GIF input/output (NSData)
//APPKIT_EXTERN NSString* NSImageFrameCount            AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER;	// GIF input (int in NSNumber) (read-only)
//APPKIT_EXTERN NSString* NSImageCurrentFrame          AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER;	// GIF input (int in NSNumber)
//APPKIT_EXTERN NSString* NSImageCurrentFrameDuration  AVAILABLE_MAC_OS_X_VERSION_10_2_AND_LATER;	// GIF input (float in NSNumber) (read-only)
//APPKIT_EXTERN NSString* NSImageLoopCount             AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER;	// GIF input (int in NSNumber) (read-only)
//APPKIT_EXTERN NSString* NSImageGamma                 AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER;	// PNG input/output (float in NSNumber)
//APPKIT_EXTERN NSString* NSImageProgressive           AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER;	// JPEG input/output (BOOL in NSNumber)
//APPKIT_EXTERN NSString* NSImageEXIFData              AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER;	// JPEG input/output (NSDictionary)
{$endif}
{$endif}

{$ifdef FORWARD}
  NSBitmapImageRep = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBITMAPIMAGEREP_PAS_C}
{$define NSBITMAPIMAGEREP_PAS_C}

{$include NSImageRep.inc}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    {.$endif}

  { NSBitmapImageRep }

  NSBitmapImageRep = class(NSImageRep)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    //  Cache
    constructor initWithFocusedViewRect(_rect: NSRect);
    constructor initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel(_planes: PChar; _width: Integer; _height: Integer; _bps: Integer; _spp: Integer; _alpha: LongBool; _isPlanar: LongBool; _colorSpaceName: CFStringRef; _rBytes: Integer; _pBits: Integer);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    constructor initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel(_planes: PChar; _width: Integer; _height: Integer; _bps: Integer; _spp: Integer; _alpha: LongBool; _isPlanar: LongBool; _colorSpaceName: CFStringRef; _bitmapFormat: NSBitmapFormat; _rBytes: Integer; _pBits: Integer);
    {.$endif}
    class function imageRepsWithData(_tiffData: CFDataRef): CFArrayRef;
    //  TIFFs can contain multiple images
    constructor imageRepWithData(_tiffData: CFDataRef);
    //  Convenience of initWithData:
    constructor initWithData(_tiffData: CFDataRef);
    function bitmapData: PChar;
    procedure getBitmapDataPlanes(_data: PChar);
    function isPlanar: LongBool;
    function samplesPerPixel: Integer;
    function bitsPerPixel: Integer;
    function bytesPerRow: Integer;
    function bytesPerPlane: Integer;
    function numberOfPlanes: Integer;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function bitmapFormat: NSBitmapFormat;
    {.$endif}
    procedure getCompression_factor(_compression: NSTIFFCompression; _factor: Single);
    procedure setCompression_factor(_compression: NSTIFFCompression; _factor: Single);
    function TIFFRepresentation: CFDataRef;
    function TIFFRepresentationUsingCompression_factor(_comp: NSTIFFCompression; _factor: Single): CFDataRef;
    class function TIFFRepresentationOfImageRepsInArray(__array: CFArrayRef): CFDataRef;
    class function TIFFRepresentationOfImageRepsInArray_usingCompression_factor(__array: CFArrayRef; _comp: NSTIFFCompression; _factor: Single): CFDataRef;
    class procedure getTIFFCompressionTypes_count(_list: NSTIFFCompression; _numTypes: Integer);
    class function localizedNameForTIFFCompressionType(_compression: NSTIFFCompression): CFStringRef;
    function canBeCompressedUsing(_compression: NSTIFFCompression): LongBool;
    //  Support for colorization of grayscale images.  Maps the image such that:
    // Gray value of midPoint -> midPointColor, black -> shadowColor, white -> lightColor.
    // Works on images with 8-bit SPP; thus either 8-bit gray or 24-bit color (with optional
    // alpha).
    procedure colorizeByMappingGray_toColor_blackMapping_whiteMapping(_midPoint: Single; _midPointColor: lobjc.id {NSColor}; _shadowColor: lobjc.id {NSColor}; _lightColor: lobjc.id {NSColor});
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    constructor initForIncrementalLoad;
    function incrementalLoadFromData_complete(_data: CFDataRef; _complete: LongBool): Integer;
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    procedure setColor_atX_y(_color: lobjc.id {NSColor}; _x: Integer; _y: Integer);
    function colorAtX_y(_x: Integer; _y: Integer): lobjc.id;{NSColor}
    procedure getPixel;
    procedure setPixel;
    {.$endif}
    constructor initWithIconHandle(_icon: Pointer);
    constructor initWithBitmapHandle(_bitmap: Pointer);
    class function representationOfImageRepsInArray_usingType_properties(_imageReps: CFArrayRef; _storageType: NSBitmapImageFileType; _properties: CFDictionaryRef): CFDataRef;
    function representationUsingType_properties(_storageType: NSBitmapImageFileType; _properties: CFDictionaryRef): CFDataRef;
    procedure setProperty_withValue(__property: CFStringRef; _value: lobjc.id);
    function valueForProperty(__property: CFStringRef): lobjc.id;
  end;

    {.$endif}
{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSBitmapImageRep_NSBitmapImageRep = 'NSBitmapImageRep';
  StrNSBitmapImageRep_initWithFocusedViewRect = 'initWithFocusedViewRect:';
  StrNSBitmapImageRep_initWithBitmapDataPlanes_1 = 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:';
  StrNSBitmapImageRep_initWithBitmapDataPlanes_2 = 'initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bitmapFormat:bytesPerRow:bitsPerPixel:';
  StrNSBitmapImageRep_imageRepsWithData = 'imageRepsWithData:';
  StrNSBitmapImageRep_imageRepWithData = 'imageRepWithData:';
  StrNSBitmapImageRep_initWithData = 'initWithData:';
  StrNSBitmapImageRep_bitmapData = 'bitmapData';
  StrNSBitmapImageRep_getBitmapDataPlanes = 'getBitmapDataPlanes:';
  StrNSBitmapImageRep_isPlanar = 'isPlanar';
  StrNSBitmapImageRep_samplesPerPixel = 'samplesPerPixel';
  StrNSBitmapImageRep_bitsPerPixel = 'bitsPerPixel';
  StrNSBitmapImageRep_bytesPerRow = 'bytesPerRow';
  StrNSBitmapImageRep_bytesPerPlane = 'bytesPerPlane';
  StrNSBitmapImageRep_numberOfPlanes = 'numberOfPlanes';
  StrNSBitmapImageRep_bitmapFormat = 'bitmapFormat';
  StrNSBitmapImageRep_getCompression_factor = 'getCompression:factor:';
  StrNSBitmapImageRep_setCompression_factor = 'setCompression:factor:';
  StrNSBitmapImageRep_TIFFRepresentation = 'TIFFRepresentation';
  StrNSBitmapImageRep_TIFFRepresentationUsingCompression_factor = 'TIFFRepresentationUsingCompression:factor:';
  StrNSBitmapImageRep_TIFFRepresentationOfImageRepsInArray = 'TIFFRepresentationOfImageRepsInArray:';
  StrNSBitmapImageRep_TIFFRepresentationOfImageRepsInArray_usingCompression_factor = 'TIFFRepresentationOfImageRepsInArray:usingCompression:factor:';
  StrNSBitmapImageRep_getTIFFCompressionTypes_count = 'getTIFFCompressionTypes:count:';
  StrNSBitmapImageRep_localizedNameForTIFFCompressionType = 'localizedNameForTIFFCompressionType:';
  StrNSBitmapImageRep_canBeCompressedUsing = 'canBeCompressedUsing:';
  StrNSBitmapImageRep_colorizeByMappingGray_toColor_blackMapping_whiteMapping = 'colorizeByMappingGray:toColor:blackMapping:whiteMapping:';
  StrNSBitmapImageRep_initForIncrementalLoad = 'initForIncrementalLoad';
  StrNSBitmapImageRep_incrementalLoadFromData_complete = 'incrementalLoadFromData:complete:';
  StrNSBitmapImageRep_setColor_atX_y = 'setColor:atX:y:';
  StrNSBitmapImageRep_colorAtX_y = 'colorAtX:y:';
  StrNSBitmapImageRep_getPixel = 'getPixel';
  StrNSBitmapImageRep_setPixel = 'setPixel';

  StrNSBitmapImageRep_initWithIconHandle = 'initWithIconHandle:';
  StrNSBitmapImageRep_initWithBitmapHandle = 'initWithBitmapHandle:';

  StrNSBitmapImageRep_representationOfImageRepsInArray_usingType_properties = 'representationOfImageRepsInArray:usingType:properties:';
  StrNSBitmapImageRep_representationUsingType_properties = 'representationUsingType:properties:';
  StrNSBitmapImageRep_setProperty_withValue = 'setProperty:withValue:';
  StrNSBitmapImageRep_valueForProperty = 'valueForProperty:';

{ NSBitmapImageRep }

class function NSBitmapImageRep.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSBitmapImageRep_NSBitmapImageRep);
end;

constructor NSBitmapImageRep.initWithFocusedViewRect(_rect: NSRect);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_rect: NSRect): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSBitmapImageRep_initWithFocusedViewRect)), _rect);
end;

constructor NSBitmapImageRep.initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel(_planes: PChar; _width: Integer; _height: Integer; _bps: Integer; _spp: Integer; _alpha: LongBool; _isPlanar: LongBool; _colorSpaceName: CFStringRef; _rBytes: Integer; _pBits: Integer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_planes: PChar; _width: Integer; _height: Integer; _bps: Integer; _spp: Integer; _alpha: LongBool; _isPlanar: LongBool; _colorSpaceName: CFStringRef; _rBytes: Integer; _pBits: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSBitmapImageRep_initWithBitmapDataPlanes_1)), _planes, _width, _height, _bps, _spp, _alpha, _isPlanar, _colorSpaceName, _rBytes, _pBits);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
constructor NSBitmapImageRep.initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bitmapFormat_bytesPerRow_bitsPerPixel(_planes: PChar; _width: Integer; _height: Integer; _bps: Integer; _spp: Integer; _alpha: LongBool; _isPlanar: LongBool; _colorSpaceName: CFStringRef; _bitmapFormat: NSBitmapFormat; _rBytes: Integer; _pBits: Integer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_planes: PChar; _width: Integer; _height: Integer; _bps: Integer; _spp: Integer; _alpha: LongBool; _isPlanar: LongBool; _colorSpaceName: CFStringRef; _bitmapFormat: NSBitmapFormat; _rBytes: Integer; _pBits: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSBitmapImageRep_initWithBitmapDataPlanes_2)), _planes, _width, _height, _bps, _spp, _alpha, _isPlanar, _colorSpaceName, _bitmapFormat, _rBytes, _pBits);
end;

{.$endif}
class function NSBitmapImageRep.imageRepsWithData(_tiffData: CFDataRef): CFArrayRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_tiffData: CFDataRef): CFArrayRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFArrayRef(vmethod(getClass, sel_registerName(PChar(StrNSBitmapImageRep_imageRepsWithData)), _tiffData));
end;

constructor NSBitmapImageRep.imageRepWithData(_tiffData: CFDataRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_tiffData: CFDataRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSBitmapImageRep_imageRepWithData)), _tiffData);
end;

constructor NSBitmapImageRep.initWithData(_tiffData: CFDataRef);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_tiffData: CFDataRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSBitmapImageRep_initWithData)), _tiffData);
end;

function NSBitmapImageRep.bitmapData: PChar;
begin
  Result := PChar(objc_msgSend(Handle, sel_registerName(PChar(StrNSBitmapImageRep_bitmapData)), []));
end;

procedure NSBitmapImageRep.getBitmapDataPlanes(_data: PChar);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_data: PChar); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_getBitmapDataPlanes)), _data);
end;

function NSBitmapImageRep.isPlanar: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSBitmapImageRep_isPlanar)), []));
end;

function NSBitmapImageRep.samplesPerPixel: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSBitmapImageRep_samplesPerPixel)), []));
end;

function NSBitmapImageRep.bitsPerPixel: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSBitmapImageRep_bitsPerPixel)), []));
end;

function NSBitmapImageRep.bytesPerRow: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSBitmapImageRep_bytesPerRow)), []));
end;

function NSBitmapImageRep.bytesPerPlane: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSBitmapImageRep_bytesPerPlane)), []));
end;

function NSBitmapImageRep.numberOfPlanes: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSBitmapImageRep_numberOfPlanes)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSBitmapImageRep.bitmapFormat: NSBitmapFormat;
begin
  Result := NSBitmapFormat(objc_msgSend(Handle, sel_registerName(PChar(StrNSBitmapImageRep_bitmapFormat)), []));
end;

{.$endif}
procedure NSBitmapImageRep.getCompression_factor(_compression: NSTIFFCompression; _factor: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_compression: NSTIFFCompression; _factor: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_getCompression_factor)), _compression, _factor);
end;

procedure NSBitmapImageRep.setCompression_factor(_compression: NSTIFFCompression; _factor: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_compression: NSTIFFCompression; _factor: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_setCompression_factor)), _compression, _factor);
end;

function NSBitmapImageRep.TIFFRepresentation: CFDataRef;
begin
  Result := CFDataRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBitmapImageRep_TIFFRepresentation)), []));
end;

function NSBitmapImageRep.TIFFRepresentationUsingCompression_factor(_comp: NSTIFFCompression; _factor: Single): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_comp: NSTIFFCompression; _factor: Single): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_TIFFRepresentationUsingCompression_factor)), _comp, _factor));
end;

class function NSBitmapImageRep.TIFFRepresentationOfImageRepsInArray(__array: CFArrayRef): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__array: CFArrayRef): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(getClass, sel_registerName(PChar(StrNSBitmapImageRep_TIFFRepresentationOfImageRepsInArray)), __array));
end;

class function NSBitmapImageRep.TIFFRepresentationOfImageRepsInArray_usingCompression_factor(__array: CFArrayRef; _comp: NSTIFFCompression; _factor: Single): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__array: CFArrayRef; _comp: NSTIFFCompression; _factor: Single): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(getClass, sel_registerName(PChar(StrNSBitmapImageRep_TIFFRepresentationOfImageRepsInArray_usingCompression_factor)), __array, _comp, _factor));
end;

class procedure NSBitmapImageRep.getTIFFCompressionTypes_count(_list: NSTIFFCompression; _numTypes: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_list: NSTIFFCompression; _numTypes: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSBitmapImageRep_getTIFFCompressionTypes_count)), _list, _numTypes);
end;

class function NSBitmapImageRep.localizedNameForTIFFCompressionType(_compression: NSTIFFCompression): CFStringRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_compression: NSTIFFCompression): CFStringRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFStringRef(vmethod(getClass, sel_registerName(PChar(StrNSBitmapImageRep_localizedNameForTIFFCompressionType)), _compression));
end;

function NSBitmapImageRep.canBeCompressedUsing(_compression: NSTIFFCompression): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_compression: NSTIFFCompression): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_canBeCompressedUsing)), _compression));
end;

procedure NSBitmapImageRep.colorizeByMappingGray_toColor_blackMapping_whiteMapping(_midPoint: Single; _midPointColor: lobjc.id {NSColor}; _shadowColor: lobjc.id {NSColor}; _lightColor: lobjc.id {NSColor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_midPoint: Single; _midPointColor: lobjc.id {NSColor}; _shadowColor: lobjc.id {NSColor}; _lightColor: lobjc.id {NSColor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_colorizeByMappingGray_toColor_blackMapping_whiteMapping)), _midPoint, _midPointColor, _shadowColor, _lightColor);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
constructor NSBitmapImageRep.initForIncrementalLoad;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSBitmapImageRep_initForIncrementalLoad)));
end;

function NSBitmapImageRep.incrementalLoadFromData_complete(_data: CFDataRef; _complete: LongBool): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_data: CFDataRef; _complete: LongBool): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_incrementalLoadFromData_complete)), _data, _complete));
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
procedure NSBitmapImageRep.setColor_atX_y(_color: lobjc.id {NSColor}; _x: Integer; _y: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_color: lobjc.id {NSColor}; _x: Integer; _y: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_setColor_atX_y)), _color, _x, _y);
end;

function NSBitmapImageRep.colorAtX_y(_x: Integer; _y: Integer): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_x: Integer; _y: Integer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_colorAtX_y)), _x, _y));
end;

procedure NSBitmapImageRep.getPixel;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSBitmapImageRep_getPixel)), []);
end;

procedure NSBitmapImageRep.setPixel;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSBitmapImageRep_setPixel)), []);
end;

{.$endif}
constructor NSBitmapImageRep.initWithIconHandle(_icon: Pointer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_icon: Pointer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSBitmapImageRep_initWithIconHandle)), _icon);
end;

constructor NSBitmapImageRep.initWithBitmapHandle(_bitmap: Pointer);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_bitmap: Pointer): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  allocbuf := objc_msgSend(ClassID, sel_registerName(PChar(Str_alloc)), []);
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(allocbuf, sel_registerName(PChar(StrNSBitmapImageRep_initWithBitmapHandle)), _bitmap);
end;

class function NSBitmapImageRep.representationOfImageRepsInArray_usingType_properties(_imageReps: CFArrayRef; _storageType: NSBitmapImageFileType; _properties: CFDictionaryRef): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_imageReps: CFArrayRef; _storageType: NSBitmapImageFileType; _properties: CFDictionaryRef): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(getClass, sel_registerName(PChar(StrNSBitmapImageRep_representationOfImageRepsInArray_usingType_properties)), _imageReps, _storageType, _properties));
end;

function NSBitmapImageRep.representationUsingType_properties(_storageType: NSBitmapImageFileType; _properties: CFDictionaryRef): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_storageType: NSBitmapImageFileType; _properties: CFDictionaryRef): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_representationUsingType_properties)), _storageType, _properties));
end;

procedure NSBitmapImageRep.setProperty_withValue(__property: CFStringRef; _value: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__property: CFStringRef; _value: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_setProperty_withValue)), __property, _value);
end;

function NSBitmapImageRep.valueForProperty(__property: CFStringRef): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__property: CFStringRef): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSBitmapImageRep_valueForProperty)), __property));
end;

{$endif}
