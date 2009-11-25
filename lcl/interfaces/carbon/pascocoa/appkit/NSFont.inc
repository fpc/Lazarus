{%mainunit appkit.pas}
(*
	NSFont.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSFONT_PAS_H}
{$define NSFONT_PAS_H}


//#import <Foundation/NSObject.h>
//#import <Foundation/NSString.h> // for NSStringEncoding
//#import <AppKit/AppKitDefines.h>
//#import <AppKit/NSCell.h> // for NSControlSize
type
  NSGlyph = LongWord;

const
  NSControlGlyph = $00FFFFFF;
  NSNullGlyph = $0;

//const float *NSFontIdentityMatrix;
type
  NSMultibyteGlyphPacking = (
    NSNativeShortGlyphPacking = 5
  );


type
  NSFontRenderingMode = (
    NSFontDefaultRenderingMode = 0, 
    NSFontAntialiasedRenderingMode = 1, 
    NSFontIntegerAdvancementsRenderingMode = 2, 
    NSFontAntialiasedIntegerAdvancementsRenderingMode = 3
  );


//NSString *NSAntialiasThresholdChangedNotification AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER;
//NSString *NSFontSetChangedNotification AVAILABLE_MAC_OS_X_VERSION_10_4_AND_LATER;
type
  NSGlyphRelation = (
    NSGlyphBelow = 1, 
    NSGlyphAbove = 2
  );


  //  Only NSNativeShortGlyphPacking is supported on Mac OS X.
const
  NSOneByteGlyphPacking = 0;
  NSJapaneseEUCGlyphPacking = NSOneByteGlyphPacking + 1;
  NSAsciiWithDoubleByteEUCGlyphPacking = NSOneByteGlyphPacking + 2;
  NSTwoByteGlyphPacking = NSOneByteGlyphPacking + 3;
  NSFourByteGlyphPacking = NSOneByteGlyphPacking + 4;

//NSString *NSAFMFamilyName         AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMFontName           AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMFormatVersion      AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMFullName           AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMNotice             AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMVersion            AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMWeight             AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMEncodingScheme     AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMCharacterSet       AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMCapHeight          AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMXHeight            AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMAscender           AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMDescender          AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMUnderlinePosition  AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMUnderlineThickness AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMItalicAngle        AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
//NSString *NSAFMMappingScheme      AVAILABLE_MAC_OS_X_VERSION_10_0_AND_LATER_BUT_DEPRECATED_IN_MAC_OS_X_VERSION_10_4;
{$endif}
{$endif}

{$ifdef FORWARD}
  NSFont = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSFONT_PAS_C}
{$define NSFONT_PAS_C}


  { NSFont }

  NSFont = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    //  All instance variables are private
    // ******** Factory ********
    constructor fontWithName_size(_fontName: CFStringRef; _fontSize: Single);
    constructor fontWithName_matrix(_fontName: CFStringRef; _fontMatrix: Single);
    constructor fontWithDescriptor_size(_fontDescriptor: lobjc.id {NSFontDescriptor}; _fontSize: Single);
    //  Instantiates an NSFont object matching fontDescriptor. If textTransform is non-nil,
    // it has precedence over NSFontMatrixAttribute in fontDescriptor.
    constructor fontWithDescriptor_textTransform(_fontDescriptor: lobjc.id {NSFontDescriptor}; _textTransform: lobjc.id {NSAffineTransform});
    constructor userFontOfSize(_fontSize: Single);
    //  Aqua Application font
    constructor userFixedPitchFontOfSize(_fontSize: Single);
    //  Aqua fixed-pitch font
    class procedure setUserFont(_aFont: lobjc.id {NSFont});
    //  set preference for Application font.
    class procedure setUserFixedPitchFont(_aFont: lobjc.id {NSFont});
    //  set preference for fixed-pitch.
    //  UI font settings
    constructor systemFontOfSize(_fontSize: Single);
    //  Aqua System font
    constructor boldSystemFontOfSize(_fontSize: Single);
    //  Aqua System font (emphasized)
    constructor labelFontOfSize(_fontSize: Single);
    //  Aqua label font
    constructor titleBarFontOfSize(_fontSize: Single);
    constructor menuFontOfSize(_fontSize: Single);
    constructor menuBarFontOfSize(_fontSize: Single);
    constructor messageFontOfSize(_fontSize: Single);
    constructor paletteFontOfSize(_fontSize: Single);
    constructor toolTipsFontOfSize(_fontSize: Single);
    constructor controlContentFontOfSize(_fontSize: Single);
    //  UI font size settings
    class function systemFontSize: Single;
    //  size of the standard System font.
    class function smallSystemFontSize: Single;
    //  size of standard small System font.
    class function labelFontSize: Single;
    class function systemFontSizeForControlSize(_controlSize: NSControlSize): Single;
    function fontName: CFStringRef;
    function pointSize: Single;
    function matrix: Single;
    function familyName: CFStringRef;
    function displayName: CFStringRef;
    function fontDescriptor: lobjc.id;{NSFontDescriptor}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function textTransform: lobjc.id;{NSAffineTransform}
    function numberOfGlyphs: LongWord;
    function mostCompatibleStringEncoding: NSStringEncoding;
    function glyphWithName(_aName: CFStringRef): NSGlyph;
    function coveredCharacterSet: CFCharacterSetRef;
    function boundingRectForFont: NSRect;
    function maximumAdvancement: NSSize;
    function ascender: Single;
    function descender: Single;
    function leading: Single;
    function underlinePosition: Single;
    function underlineThickness: Single;
    function italicAngle: Single;
    function capHeight: Single;
    function xHeight: Single;
    function isFixedPitch: LongBool;
    // ******** Glyph metrics ********
    function boundingRectForGlyph(_aGlyph: NSGlyph): NSRect;
    function advancementForGlyph(_ag: NSGlyph): NSSize;
//    procedure getBoundingRects_forGlyphs_count(_bounds: NSRectArray; _glyphs: NSGlyph; _glyphCount: LongWord);
//    procedure getAdvancements_forGlyphs_count(_advancements: NSSizeArray; _glyphs: NSGlyph; _glyphCount: LongWord);
//    procedure getAdvancements_forPackedGlyphs_length(_advancements: NSSizeArray; _packedGlyphs: Pointer; _length: LongWord);
    procedure set_;
    procedure setInContext(_graphicsContext: lobjc.id {NSGraphicsContext});
    function printerFont: lobjc.id;{NSFont}
    function screenFont: lobjc.id;{NSFont}
    function screenFontWithRenderingMode(_renderingMode: NSFontRenderingMode): lobjc.id;{NSFont}
    function renderingMode: NSFontRenderingMode;
    class procedure useFont(_fontName: CFStringRef);
    //  This is now automatically handled by Quartz.
    function widthOfString(__string: CFStringRef): Single;
    //  This API never returns correct value. Use NSStringDrawing API instead.
    function isBaseFont: LongBool;
    function afmDictionary: CFDictionaryRef;
    function glyphIsEncoded(_aGlyph: NSGlyph): LongBool;
    //  Can be deduced by aGlyph < [NSFont numberOfGlyphs] since only NSNativeShortGlyphPacking
    // is supported.
    function defaultLineHeightForFont: Single;
    //  Use -[NSLayoutManager defaultLineHeightForFont:] instead.
    class function preferredFontNames: CFArrayRef;
    //  NSFontCascadeListAttribute offers more powerful font substitution management
    class procedure setPreferredFontNames(_fontNameArray: CFArrayRef);
    function encodingScheme: CFStringRef;
    function glyphPacking: NSMultibyteGlyphPacking;
    //  The context-sensitive inter-glyph spacing is now performed at the typesetting stage.
    function positionOfGlyph_precededByGlyph_isNominal(_curGlyph: NSGlyph; _prevGlyph: NSGlyph; _nominal: LongBool): NSPoint;
    function positionsForCompositeSequence_numberOfGlyphs_pointArray(_someGlyphs: NSGlyph; _numGlyphs: Integer; _points: NSPointArray): Integer;
    function positionOfGlyph_struckOverGlyph_metricsExist(_curGlyph: NSGlyph; _prevGlyph: NSGlyph; _exist: LongBool): NSPoint;
    function positionOfGlyph_struckOverRect_metricsExist(_aGlyph: NSGlyph; _aRect: NSRect; _exist: LongBool): NSPoint;
    function positionOfGlyph_forCharacter_struckOverRect(_aGlyph: NSGlyph; _aChar: unichar; _aRect: NSRect): NSPoint;
//    function positionOfGlyph_withRelation_toBaseGlyph_totalAdvancement_metricsExist(_thisGlyph: NSGlyph; _rel: NSGlyphRelation; _baseGlyph: NSGlyph; _adv: NSSizePointer; _exist: LongBool): NSPoint;
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSFont_NSFont = 'NSFont';
  StrNSFont_fontWithName_size = 'fontWithName:size:';
  StrNSFont_fontWithName_matrix = 'fontWithName:matrix:';
  StrNSFont_fontWithDescriptor_size = 'fontWithDescriptor:size:';
  StrNSFont_fontWithDescriptor_textTransform = 'fontWithDescriptor:textTransform:';
  StrNSFont_userFontOfSize = 'userFontOfSize:';
  StrNSFont_userFixedPitchFontOfSize = 'userFixedPitchFontOfSize:';
  StrNSFont_setUserFont = 'setUserFont:';
  StrNSFont_setUserFixedPitchFont = 'setUserFixedPitchFont:';
  StrNSFont_systemFontOfSize = 'systemFontOfSize:';
  StrNSFont_boldSystemFontOfSize = 'boldSystemFontOfSize:';
  StrNSFont_labelFontOfSize = 'labelFontOfSize:';
  StrNSFont_titleBarFontOfSize = 'titleBarFontOfSize:';
  StrNSFont_menuFontOfSize = 'menuFontOfSize:';
  StrNSFont_menuBarFontOfSize = 'menuBarFontOfSize:';
  StrNSFont_messageFontOfSize = 'messageFontOfSize:';
  StrNSFont_paletteFontOfSize = 'paletteFontOfSize:';
  StrNSFont_toolTipsFontOfSize = 'toolTipsFontOfSize:';
  StrNSFont_controlContentFontOfSize = 'controlContentFontOfSize:';
  StrNSFont_systemFontSize = 'systemFontSize';
  StrNSFont_smallSystemFontSize = 'smallSystemFontSize';
  StrNSFont_labelFontSize = 'labelFontSize';
  StrNSFont_systemFontSizeForControlSize = 'systemFontSizeForControlSize:';
  StrNSFont_fontName = 'fontName';
  StrNSFont_pointSize = 'pointSize';
  StrNSFont_matrix = 'matrix';
  StrNSFont_familyName = 'familyName';
  StrNSFont_displayName = 'displayName';
  StrNSFont_fontDescriptor = 'fontDescriptor';
  StrNSFont_textTransform = 'textTransform';
  StrNSFont_numberOfGlyphs = 'numberOfGlyphs';
  StrNSFont_mostCompatibleStringEncoding = 'mostCompatibleStringEncoding';
  StrNSFont_glyphWithName = 'glyphWithName:';
  StrNSFont_coveredCharacterSet = 'coveredCharacterSet';
  StrNSFont_boundingRectForFont = 'boundingRectForFont';
  StrNSFont_maximumAdvancement = 'maximumAdvancement';
  StrNSFont_ascender = 'ascender';
  StrNSFont_descender = 'descender';
  StrNSFont_leading = 'leading';
  StrNSFont_underlinePosition = 'underlinePosition';
  StrNSFont_underlineThickness = 'underlineThickness';
  StrNSFont_italicAngle = 'italicAngle';
  StrNSFont_capHeight = 'capHeight';
  StrNSFont_xHeight = 'xHeight';
  StrNSFont_isFixedPitch = 'isFixedPitch';
  StrNSFont_boundingRectForGlyph = 'boundingRectForGlyph:';
  StrNSFont_advancementForGlyph = 'advancementForGlyph:';
  StrNSFont_getBoundingRects_forGlyphs_count = 'getBoundingRects:forGlyphs:count:';
  StrNSFont_getAdvancements_forGlyphs_count = 'getAdvancements:forGlyphs:count:';
  StrNSFont_getAdvancements_forPackedGlyphs_length = 'getAdvancements:forPackedGlyphs:length:';
  StrNSFont_set = 'set';
  StrNSFont_setInContext = 'setInContext:';
  StrNSFont_printerFont = 'printerFont';
  StrNSFont_screenFont = 'screenFont';
  StrNSFont_screenFontWithRenderingMode = 'screenFontWithRenderingMode:';
  StrNSFont_renderingMode = 'renderingMode';

  StrNSFont_useFont = 'useFont:';
  StrNSFont_widthOfString = 'widthOfString:';
  StrNSFont_isBaseFont = 'isBaseFont';
  StrNSFont_afmDictionary = 'afmDictionary';
  StrNSFont_glyphIsEncoded = 'glyphIsEncoded:';
  StrNSFont_defaultLineHeightForFont = 'defaultLineHeightForFont';
  StrNSFont_preferredFontNames = 'preferredFontNames';
  StrNSFont_setPreferredFontNames = 'setPreferredFontNames:';
  StrNSFont_encodingScheme = 'encodingScheme';
  StrNSFont_glyphPacking = 'glyphPacking';
  StrNSFont_positionOfGlyph_precededByGlyph_isNominal = 'positionOfGlyph:precededByGlyph:isNominal:';
  StrNSFont_positionsForCompositeSequence_numberOfGlyphs_pointArray = 'positionsForCompositeSequence:numberOfGlyphs:pointArray:';
  StrNSFont_positionOfGlyph_struckOverGlyph_metricsExist = 'positionOfGlyph:struckOverGlyph:metricsExist:';
  StrNSFont_positionOfGlyph_struckOverRect_metricsExist = 'positionOfGlyph:struckOverRect:metricsExist:';
  StrNSFont_positionOfGlyph_forCharacter_struckOverRect = 'positionOfGlyph:forCharacter:struckOverRect:';
  StrNSFont_positionOfGlyph_withRelation_toBaseGlyph_totalAdvancement_metricsExist = 'positionOfGlyph:withRelation:toBaseGlyph:totalAdvancement:metricsExist:';

{ NSFont }

class function NSFont.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSFont_NSFont);
end;

constructor NSFont.fontWithName_size(_fontName: CFStringRef; _fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontName: CFStringRef; _fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_fontWithName_size)), _fontName, _fontSize);
end;

constructor NSFont.fontWithName_matrix(_fontName: CFStringRef; _fontMatrix: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontName: CFStringRef; _fontMatrix: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_fontWithName_matrix)), _fontName, _fontMatrix);
end;

constructor NSFont.fontWithDescriptor_size(_fontDescriptor: lobjc.id {NSFontDescriptor}; _fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontDescriptor: lobjc.id {NSFontDescriptor}; _fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_fontWithDescriptor_size)), _fontDescriptor, _fontSize);
end;

constructor NSFont.fontWithDescriptor_textTransform(_fontDescriptor: lobjc.id {NSFontDescriptor}; _textTransform: lobjc.id {NSAffineTransform});
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontDescriptor: lobjc.id {NSFontDescriptor}; _textTransform: lobjc.id {NSAffineTransform}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_fontWithDescriptor_textTransform)), _fontDescriptor, _textTransform);
end;

constructor NSFont.userFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_userFontOfSize)), _fontSize);
end;

constructor NSFont.userFixedPitchFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_userFixedPitchFontOfSize)), _fontSize);
end;

class procedure NSFont.setUserFont(_aFont: lobjc.id {NSFont});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aFont: lobjc.id {NSFont}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSFont_setUserFont)), _aFont);
end;

class procedure NSFont.setUserFixedPitchFont(_aFont: lobjc.id {NSFont});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aFont: lobjc.id {NSFont}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSFont_setUserFixedPitchFont)), _aFont);
end;

constructor NSFont.systemFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_systemFontOfSize)), _fontSize);
end;

constructor NSFont.boldSystemFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_boldSystemFontOfSize)), _fontSize);
end;

constructor NSFont.labelFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_labelFontOfSize)), _fontSize);
end;

constructor NSFont.titleBarFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_titleBarFontOfSize)), _fontSize);
end;

constructor NSFont.menuFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_menuFontOfSize)), _fontSize);
end;

constructor NSFont.menuBarFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_menuBarFontOfSize)), _fontSize);
end;

constructor NSFont.messageFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_messageFontOfSize)), _fontSize);
end;

constructor NSFont.paletteFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_paletteFontOfSize)), _fontSize);
end;

constructor NSFont.toolTipsFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_toolTipsFontOfSize)), _fontSize);
end;

constructor NSFont.controlContentFontOfSize(_fontSize: Single);
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fontSize: Single): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSFont_controlContentFontOfSize)), _fontSize);
end;

class function NSFont.systemFontSize: Single;
begin
  Result := Single(objc_msgSend(getClass, sel_registerName(PChar(StrNSFont_systemFontSize)), []));
end;

class function NSFont.smallSystemFontSize: Single;
begin
  Result := Single(objc_msgSend(getClass, sel_registerName(PChar(StrNSFont_smallSystemFontSize)), []));
end;

class function NSFont.labelFontSize: Single;
begin
  Result := Single(objc_msgSend(getClass, sel_registerName(PChar(StrNSFont_labelFontSize)), []));
end;

class function NSFont.systemFontSizeForControlSize(_controlSize: NSControlSize): Single;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_controlSize: NSControlSize): Single; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Single(vmethod(getClass, sel_registerName(PChar(StrNSFont_systemFontSizeForControlSize)), _controlSize));
end;

function NSFont.fontName: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_fontName)), []));
end;

function NSFont.pointSize: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_pointSize)), []));
end;

function NSFont.matrix: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_matrix)), []));
end;

function NSFont.familyName: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_familyName)), []));
end;

function NSFont.displayName: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_displayName)), []));
end;

function NSFont.fontDescriptor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_fontDescriptor)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSFont.textTransform: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_textTransform)), []));
end;

function NSFont.numberOfGlyphs: LongWord;
begin
  Result := LongWord(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_numberOfGlyphs)), []));
end;

function NSFont.mostCompatibleStringEncoding: NSStringEncoding;
begin
  Result := NSStringEncoding(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_mostCompatibleStringEncoding)), []));
end;

function NSFont.glyphWithName(_aName: CFStringRef): NSGlyph;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aName: CFStringRef): NSGlyph; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := NSGlyph(vmethod(Handle, sel_registerName(PChar(StrNSFont_glyphWithName)), _aName));
end;

function NSFont.coveredCharacterSet: CFCharacterSetRef;
begin
  Result := CFCharacterSetRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_coveredCharacterSet)), []));
end;

function NSFont.boundingRectForFont: NSRect;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSFont_boundingRectForFont)), []);
end;

function NSFont.maximumAdvancement: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSFont_maximumAdvancement)), []);
end;

function NSFont.ascender: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_ascender)), []));
end;

function NSFont.descender: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_descender)), []));
end;

function NSFont.leading: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_leading)), []));
end;

function NSFont.underlinePosition: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_underlinePosition)), []));
end;

function NSFont.underlineThickness: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_underlineThickness)), []));
end;

function NSFont.italicAngle: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_italicAngle)), []));
end;

function NSFont.capHeight: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_capHeight)), []));
end;

function NSFont.xHeight: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_xHeight)), []));
end;

function NSFont.isFixedPitch: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_isFixedPitch)), []));
end;

function NSFont.boundingRectForGlyph(_aGlyph: NSGlyph): NSRect;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aGlyph: NSGlyph); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSFont_boundingRectForGlyph)), _aGlyph);
end;

function NSFont.advancementForGlyph(_ag: NSGlyph): NSSize;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_ag: NSGlyph); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSFont_advancementForGlyph)), _ag);
end;

{procedure NSFont.getBoundingRects_forGlyphs_count(_bounds: NSRectArray; _glyphs: NSGlyph; _glyphCount: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_bounds: NSRectArray; _glyphs: NSGlyph; _glyphCount: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFont_getBoundingRects_forGlyphs_count)), _bounds, _glyphs, _glyphCount);
end;

procedure NSFont.getAdvancements_forGlyphs_count(_advancements: NSSizeArray; _glyphs: NSGlyph; _glyphCount: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_advancements: NSSizeArray; _glyphs: NSGlyph; _glyphCount: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFont_getAdvancements_forGlyphs_count)), _advancements, _glyphs, _glyphCount);
end;

procedure NSFont.getAdvancements_forPackedGlyphs_length(_advancements: NSSizeArray; _packedGlyphs: Pointer; _length: LongWord);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_advancements: NSSizeArray; _packedGlyphs: Pointer; _length: LongWord); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFont_getAdvancements_forPackedGlyphs_length)), _advancements, _packedGlyphs, _length);
end;}

procedure NSFont.set_;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_set)), []);
end;

procedure NSFont.setInContext(_graphicsContext: lobjc.id {NSGraphicsContext});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_graphicsContext: lobjc.id {NSGraphicsContext}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSFont_setInContext)), _graphicsContext);
end;

function NSFont.printerFont: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_printerFont)), []));
end;

function NSFont.screenFont: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_screenFont)), []));
end;

function NSFont.screenFontWithRenderingMode(_renderingMode: NSFontRenderingMode): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_renderingMode: NSFontRenderingMode): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSFont_screenFontWithRenderingMode)), _renderingMode));
end;

function NSFont.renderingMode: NSFontRenderingMode;
begin
  Result := NSFontRenderingMode(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_renderingMode)), []));
end;

class procedure NSFont.useFont(_fontName: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_fontName: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSFont_useFont)), _fontName);
end;

function NSFont.widthOfString(__string: CFStringRef): Single;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;__string: CFStringRef): Single; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Single(vmethod(Handle, sel_registerName(PChar(StrNSFont_widthOfString)), __string));
end;

function NSFont.isBaseFont: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_isBaseFont)), []));
end;

function NSFont.afmDictionary: CFDictionaryRef;
begin
  Result := CFDictionaryRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_afmDictionary)), []));
end;

function NSFont.glyphIsEncoded(_aGlyph: NSGlyph): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_aGlyph: NSGlyph): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSFont_glyphIsEncoded)), _aGlyph));
end;

function NSFont.defaultLineHeightForFont: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_defaultLineHeightForFont)), []));
end;

class function NSFont.preferredFontNames: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(getClass, sel_registerName(PChar(StrNSFont_preferredFontNames)), []));
end;

class procedure NSFont.setPreferredFontNames(_fontNameArray: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_fontNameArray: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(getClass, sel_registerName(PChar(StrNSFont_setPreferredFontNames)), _fontNameArray);
end;

function NSFont.encodingScheme: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_encodingScheme)), []));
end;

function NSFont.glyphPacking: NSMultibyteGlyphPacking;
begin
  Result := NSMultibyteGlyphPacking(objc_msgSend(Handle, sel_registerName(PChar(StrNSFont_glyphPacking)), []));
end;

function NSFont.positionOfGlyph_precededByGlyph_isNominal(_curGlyph: NSGlyph; _prevGlyph: NSGlyph; _nominal: LongBool): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_curGlyph: NSGlyph; _prevGlyph: NSGlyph; _nominal: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSFont_positionOfGlyph_precededByGlyph_isNominal)), _curGlyph, _prevGlyph, _nominal);
end;

function NSFont.positionsForCompositeSequence_numberOfGlyphs_pointArray(_someGlyphs: NSGlyph; _numGlyphs: Integer; _points: NSPointArray): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_someGlyphs: NSGlyph; _numGlyphs: Integer; _points: NSPointArray): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSFont_positionsForCompositeSequence_numberOfGlyphs_pointArray)), _someGlyphs, _numGlyphs, _points));
end;

function NSFont.positionOfGlyph_struckOverGlyph_metricsExist(_curGlyph: NSGlyph; _prevGlyph: NSGlyph; _exist: LongBool): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_curGlyph: NSGlyph; _prevGlyph: NSGlyph; _exist: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSFont_positionOfGlyph_struckOverGlyph_metricsExist)), _curGlyph, _prevGlyph, _exist);
end;

function NSFont.positionOfGlyph_struckOverRect_metricsExist(_aGlyph: NSGlyph; _aRect: NSRect; _exist: LongBool): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aGlyph: NSGlyph; _aRect: NSRect; _exist: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSFont_positionOfGlyph_struckOverRect_metricsExist)), _aGlyph, _aRect, _exist);
end;

function NSFont.positionOfGlyph_forCharacter_struckOverRect(_aGlyph: NSGlyph; _aChar: unichar; _aRect: NSRect): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_aGlyph: NSGlyph; _aChar: unichar; _aRect: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSFont_positionOfGlyph_forCharacter_struckOverRect)), _aGlyph, _aChar, _aRect);
end;

{function NSFont.positionOfGlyph_withRelation_toBaseGlyph_totalAdvancement_metricsExist(_thisGlyph: NSGlyph; _rel: NSGlyphRelation; _baseGlyph: NSGlyph; _adv: NSSizePointer; _exist: LongBool): NSPoint;
type
  TmsgSendWrapper = procedure (result_param: Pointer; param1: lobjc.id; param2: SEL;_thisGlyph: NSGlyph; _rel: NSGlyphRelation; _baseGlyph: NSGlyph; _adv: NSSizePointer; _exist: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend_fpret);
  vmethod(@Result, Handle, sel_registerName(PChar(StrNSFont_positionOfGlyph_withRelation_toBaseGlyph_totalAdvancement_metricsExist)), _thisGlyph, _rel, _baseGlyph, _adv, _exist);
end;}

{$endif}
