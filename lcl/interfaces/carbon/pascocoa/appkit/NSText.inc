{%mainunit appkit.pas}
(*
	NSText.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSTEXT_PAS_H}
{$define NSTEXT_PAS_H}

{$include NSView.inc}
{$include NSSpellProtocol.inc}

  //  Various important unicodes
const
  NSParagraphSeparatorCharacter = $2029;
  NSLineSeparatorCharacter = $2028;
  NSTabCharacter = $0009;
  NSFormFeedCharacter = $000c;
  NSNewlineCharacter = $000a;
  NSCarriageReturnCharacter = $000d;
  NSEnterCharacter = $0003;
  NSBackspaceCharacter = $0008;
  NSBackTabCharacter = $0019;
  NSDeleteCharacter = $007f;

type
  NSTextAlignment = (
    NSLeftTextAlignment = 0, 
    NSRightTextAlignment = 1, 
    NSCenterTextAlignment = 2, 
    NSJustifiedTextAlignment = 3, 
    NSNaturalTextAlignment = 4
  );


type
  NSWritingDirection = (
    NSWritingDirectionNatural = 1, 
    NSWritingDirectionLeftToRight = 0, 
    NSWritingDirectionRightToLeft
  );


  //  Movement codes for movement between fields; these codes are the intValue of the
  // NSTextMovement key in NSTextDidEndEditing notifications, and are used when completions
  // change in the NSTextView method insertCompletion:forPartialWordRange:movement:isFinal:.
  //  Note that the value 0 is used for movements that do not fall under any of the other
  // values, hence NSOtherTextMovement is a more appropriate name than the previous NSIllegalTextMovement.
const
  NSIllegalTextMovement = 0;
  NSReturnTextMovement = $10;
  NSTabTextMovement = $11;
  NSBacktabTextMovement = $12;
  NSLeftTextMovement = $13;
  NSRightTextMovement = $14;
  NSUpTextMovement = $15;
  NSDownTextMovement = $16;
  NSCancelTextMovement = $17;
  NSOtherTextMovement = 0;

//APPKIT_EXTERN NSString *NSTextDidBeginEditingNotification;
//APPKIT_EXTERN NSString *NSTextDidEndEditingNotification;	// userInfo key:  @"NSTextMovement"
//APPKIT_EXTERN NSString *NSTextDidChangeNotification;
{$endif}
{$endif}

{$ifdef FORWARD}
  NSText = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXT_PAS_C}
{$define NSTEXT_PAS_C}

{$include NSView.inc}
{$include NSSpellProtocol.inc}

  { NSText }

  NSText = class(NSView)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    function string_: CFStringRef;
    procedure setString(__string: CFStringRef);
    procedure replaceCharactersInRange_withString(_range: NSRange; _aString: CFStringRef);
    procedure replaceCharactersInRange_withRTF(_range: NSRange; _rtfData: CFDataRef);
    procedure replaceCharactersInRange_withRTFD(_range: NSRange; _rtfdData: CFDataRef);
    function RTFFromRange(_range: NSRange): CFDataRef;
    function RTFDFromRange(_range: NSRange): CFDataRef;
    function writeRTFDToFile_atomically(_path: CFStringRef; _flag: LongBool): LongBool;
    function readRTFDFromFile(_path: CFStringRef): LongBool;
    function delegate: lobjc.id;
    procedure setDelegate(_anObject: lobjc.id);
    function isEditable: LongBool;
    procedure setEditable(_flag: LongBool);
    function isSelectable: LongBool;
    procedure setSelectable(_flag: LongBool);
    function isRichText: LongBool;
    procedure setRichText(_flag: LongBool);
    //  If NO, also clears setImportsGraphics:
    function importsGraphics: LongBool;
    procedure setImportsGraphics(_flag: LongBool);
    //  If YES, also sets setRichText:
    procedure setBackgroundColor(_color: lobjc.id {NSColor});
    function backgroundColor: lobjc.id;{NSColor}
    procedure setDrawsBackground(_flag: LongBool);
    function drawsBackground: LongBool;
    procedure setFont(_obj: lobjc.id {NSFont});
    function font: lobjc.id;{NSFont}
    procedure setTextColor(_color: lobjc.id {NSColor});
    function textColor: lobjc.id;{NSColor}
    function alignment: NSTextAlignment;
    procedure setAlignment(_mode: NSTextAlignment);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function baseWritingDirection: NSWritingDirection;
    procedure setBaseWritingDirection(_writingDirection: NSWritingDirection);
    {.$endif}
    procedure setFieldEditor(_flag: LongBool);
    //  A BOOL to indicate whether to end on CR, TAB, etc
    function isFieldEditor: LongBool;
    procedure setTextColor_range(_color: lobjc.id {NSColor}; _range: NSRange);
    procedure setFont_range(_font: lobjc.id {NSFont}; _range: NSRange);
    function usesFontPanel: LongBool;
    procedure setUsesFontPanel(_flag: LongBool);
    function maxSize: NSSize;
    procedure setMaxSize(_newMaxSize: NSSize);
    function minSize: NSSize;
    procedure setMinSize(_newMinSize: NSSize);
    function isHorizontallyResizable: LongBool;
    procedure setHorizontallyResizable(_flag: LongBool);
    function isVerticallyResizable: LongBool;
    procedure setVerticallyResizable(_flag: LongBool);
    procedure sizeToFit;
    procedure copy(_sender: lobjc.id);
    procedure copyFont(_sender: lobjc.id);
    procedure copyRuler(_sender: lobjc.id);
    procedure cut(_sender: lobjc.id);
    procedure delete(_sender: lobjc.id);
    procedure paste(_sender: lobjc.id);
    procedure pasteFont(_sender: lobjc.id);
    procedure pasteRuler(_sender: lobjc.id);
    procedure selectAll(_sender: lobjc.id);
    procedure changeFont(_sender: lobjc.id);
    procedure alignLeft(_sender: lobjc.id);
    procedure alignRight(_sender: lobjc.id);
    procedure alignCenter(_sender: lobjc.id);
    procedure subscript(_sender: lobjc.id);
    procedure superscript(_sender: lobjc.id);
    procedure underline(_sender: lobjc.id);
    procedure unscript(_sender: lobjc.id);
    procedure showGuessPanel(_sender: lobjc.id);
    procedure checkSpelling(_sender: lobjc.id);
    procedure toggleRuler(_sender: lobjc.id);
    function isRulerVisible: LongBool;
    function selectedRange: NSRange;
    procedure setSelectedRange(_range: NSRange);
    procedure scrollRangeToVisible(_range: NSRange);
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSText_NSText = 'NSText';
  StrNSText_string = 'string';
  StrNSText_setString = 'setString:';
  StrNSText_replaceCharactersInRange_withString = 'replaceCharactersInRange:withString:';
  StrNSText_replaceCharactersInRange_withRTF = 'replaceCharactersInRange:withRTF:';
  StrNSText_replaceCharactersInRange_withRTFD = 'replaceCharactersInRange:withRTFD:';
  StrNSText_RTFFromRange = 'RTFFromRange:';
  StrNSText_RTFDFromRange = 'RTFDFromRange:';
  StrNSText_writeRTFDToFile_atomically = 'writeRTFDToFile:atomically:';
  StrNSText_readRTFDFromFile = 'readRTFDFromFile:';
  StrNSText_delegate = 'delegate';
  StrNSText_setDelegate = 'setDelegate:';
  StrNSText_isEditable = 'isEditable';
  StrNSText_setEditable = 'setEditable:';
  StrNSText_isSelectable = 'isSelectable';
  StrNSText_setSelectable = 'setSelectable:';
  StrNSText_isRichText = 'isRichText';
  StrNSText_setRichText = 'setRichText:';
  StrNSText_importsGraphics = 'importsGraphics';
  StrNSText_setImportsGraphics = 'setImportsGraphics:';
  StrNSText_setBackgroundColor = 'setBackgroundColor:';
  StrNSText_backgroundColor = 'backgroundColor';
  StrNSText_setDrawsBackground = 'setDrawsBackground:';
  StrNSText_drawsBackground = 'drawsBackground';
  StrNSText_setFont = 'setFont:';
  StrNSText_font = 'font';
  StrNSText_setTextColor = 'setTextColor:';
  StrNSText_textColor = 'textColor';
  StrNSText_alignment = 'alignment';
  StrNSText_setAlignment = 'setAlignment:';
  StrNSText_baseWritingDirection = 'baseWritingDirection';
  StrNSText_setBaseWritingDirection = 'setBaseWritingDirection:';
  StrNSText_setFieldEditor = 'setFieldEditor:';
  StrNSText_isFieldEditor = 'isFieldEditor';
  StrNSText_setTextColor_range = 'setTextColor:range:';
  StrNSText_setFont_range = 'setFont:range:';
  StrNSText_usesFontPanel = 'usesFontPanel';
  StrNSText_setUsesFontPanel = 'setUsesFontPanel:';
  StrNSText_maxSize = 'maxSize';
  StrNSText_setMaxSize = 'setMaxSize:';
  StrNSText_minSize = 'minSize';
  StrNSText_setMinSize = 'setMinSize:';
  StrNSText_isHorizontallyResizable = 'isHorizontallyResizable';
  StrNSText_setHorizontallyResizable = 'setHorizontallyResizable:';
  StrNSText_isVerticallyResizable = 'isVerticallyResizable';
  StrNSText_setVerticallyResizable = 'setVerticallyResizable:';
  StrNSText_sizeToFit = 'sizeToFit';
  StrNSText_copy = 'copy:';
  StrNSText_copyFont = 'copyFont:';
  StrNSText_copyRuler = 'copyRuler:';
  StrNSText_cut = 'cut:';
  StrNSText_delete = 'delete:';
  StrNSText_paste = 'paste:';
  StrNSText_pasteFont = 'pasteFont:';
  StrNSText_pasteRuler = 'pasteRuler:';
  StrNSText_selectAll = 'selectAll:';
  StrNSText_changeFont = 'changeFont:';
  StrNSText_alignLeft = 'alignLeft:';
  StrNSText_alignRight = 'alignRight:';
  StrNSText_alignCenter = 'alignCenter:';
  StrNSText_subscript = 'subscript:';
  StrNSText_superscript = 'superscript:';
  StrNSText_underline = 'underline:';
  StrNSText_unscript = 'unscript:';
  StrNSText_showGuessPanel = 'showGuessPanel:';
  StrNSText_checkSpelling = 'checkSpelling:';
  StrNSText_toggleRuler = 'toggleRuler:';
  StrNSText_isRulerVisible = 'isRulerVisible';
  StrNSText_selectedRange = 'selectedRange';
  StrNSText_setSelectedRange = 'setSelectedRange:';
  StrNSText_scrollRangeToVisible = 'scrollRangeToVisible:';

{ NSText }

class function NSText.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSText_NSText);
end;

function NSText.string_: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_string)), []));
end;

procedure NSText.setString(__string: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__string: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setString)), __string);
end;

procedure NSText.replaceCharactersInRange_withString(_range: NSRange; _aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange; _aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_replaceCharactersInRange_withString)), _range, _aString);
end;

procedure NSText.replaceCharactersInRange_withRTF(_range: NSRange; _rtfData: CFDataRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange; _rtfData: CFDataRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_replaceCharactersInRange_withRTF)), _range, _rtfData);
end;

procedure NSText.replaceCharactersInRange_withRTFD(_range: NSRange; _rtfdData: CFDataRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange; _rtfdData: CFDataRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_replaceCharactersInRange_withRTFD)), _range, _rtfdData);
end;

function NSText.RTFFromRange(_range: NSRange): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_range: NSRange): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSText_RTFFromRange)), _range));
end;

function NSText.RTFDFromRange(_range: NSRange): CFDataRef;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_range: NSRange): CFDataRef; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := CFDataRef(vmethod(Handle, sel_registerName(PChar(StrNSText_RTFDFromRange)), _range));
end;

function NSText.writeRTFDToFile_atomically(_path: CFStringRef; _flag: LongBool): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _flag: LongBool): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSText_writeRTFDToFile_atomically)), _path, _flag));
end;

function NSText.readRTFDFromFile(_path: CFStringRef): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSText_readRTFDFromFile)), _path));
end;

function NSText.delegate: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_delegate)), []));
end;

procedure NSText.setDelegate(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setDelegate)), _anObject);
end;

function NSText.isEditable: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_isEditable)), []));
end;

procedure NSText.setEditable(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setEditable)), _flag);
end;

function NSText.isSelectable: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_isSelectable)), []));
end;

procedure NSText.setSelectable(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setSelectable)), _flag);
end;

function NSText.isRichText: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_isRichText)), []));
end;

procedure NSText.setRichText(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setRichText)), _flag);
end;

function NSText.importsGraphics: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_importsGraphics)), []));
end;

procedure NSText.setImportsGraphics(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setImportsGraphics)), _flag);
end;

procedure NSText.setBackgroundColor(_color: lobjc.id {NSColor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_color: lobjc.id {NSColor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setBackgroundColor)), _color);
end;

function NSText.backgroundColor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_backgroundColor)), []));
end;

procedure NSText.setDrawsBackground(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setDrawsBackground)), _flag);
end;

function NSText.drawsBackground: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_drawsBackground)), []));
end;

procedure NSText.setFont(_obj: lobjc.id {NSFont});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_obj: lobjc.id {NSFont}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setFont)), _obj);
end;

function NSText.font: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_font)), []));
end;

procedure NSText.setTextColor(_color: lobjc.id {NSColor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_color: lobjc.id {NSColor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setTextColor)), _color);
end;

function NSText.textColor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_textColor)), []));
end;

function NSText.alignment: NSTextAlignment;
begin
  Result := NSTextAlignment(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_alignment)), []));
end;

procedure NSText.setAlignment(_mode: NSTextAlignment);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_mode: NSTextAlignment); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setAlignment)), _mode);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSText.baseWritingDirection: NSWritingDirection;
begin
  Result := NSWritingDirection(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_baseWritingDirection)), []));
end;

procedure NSText.setBaseWritingDirection(_writingDirection: NSWritingDirection);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_writingDirection: NSWritingDirection); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setBaseWritingDirection)), _writingDirection);
end;

{.$endif}
procedure NSText.setFieldEditor(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setFieldEditor)), _flag);
end;

function NSText.isFieldEditor: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_isFieldEditor)), []));
end;

procedure NSText.setTextColor_range(_color: lobjc.id {NSColor}; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_color: lobjc.id {NSColor}; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setTextColor_range)), _color, _range);
end;

procedure NSText.setFont_range(_font: lobjc.id {NSFont}; _range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_font: lobjc.id {NSFont}; _range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setFont_range)), _font, _range);
end;

function NSText.usesFontPanel: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_usesFontPanel)), []));
end;

procedure NSText.setUsesFontPanel(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setUsesFontPanel)), _flag);
end;

function NSText.maxSize: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSText_maxSize)), []);
end;

procedure NSText.setMaxSize(_newMaxSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newMaxSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setMaxSize)), _newMaxSize);
end;

function NSText.minSize: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSText_minSize)), []);
end;

procedure NSText.setMinSize(_newMinSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_newMinSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setMinSize)), _newMinSize);
end;

function NSText.isHorizontallyResizable: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_isHorizontallyResizable)), []));
end;

procedure NSText.setHorizontallyResizable(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setHorizontallyResizable)), _flag);
end;

function NSText.isVerticallyResizable: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_isVerticallyResizable)), []));
end;

procedure NSText.setVerticallyResizable(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setVerticallyResizable)), _flag);
end;

procedure NSText.sizeToFit;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSText_sizeToFit)), []);
end;

procedure NSText.copy(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_copy)), _sender);
end;

procedure NSText.copyFont(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_copyFont)), _sender);
end;

procedure NSText.copyRuler(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_copyRuler)), _sender);
end;

procedure NSText.cut(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_cut)), _sender);
end;

procedure NSText.delete(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_delete)), _sender);
end;

procedure NSText.paste(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_paste)), _sender);
end;

procedure NSText.pasteFont(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_pasteFont)), _sender);
end;

procedure NSText.pasteRuler(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_pasteRuler)), _sender);
end;

procedure NSText.selectAll(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_selectAll)), _sender);
end;

procedure NSText.changeFont(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_changeFont)), _sender);
end;

procedure NSText.alignLeft(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_alignLeft)), _sender);
end;

procedure NSText.alignRight(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_alignRight)), _sender);
end;

procedure NSText.alignCenter(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_alignCenter)), _sender);
end;

procedure NSText.subscript(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_subscript)), _sender);
end;

procedure NSText.superscript(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_superscript)), _sender);
end;

procedure NSText.underline(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_underline)), _sender);
end;

procedure NSText.unscript(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_unscript)), _sender);
end;

procedure NSText.showGuessPanel(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_showGuessPanel)), _sender);
end;

procedure NSText.checkSpelling(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_checkSpelling)), _sender);
end;

procedure NSText.toggleRuler(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_toggleRuler)), _sender);
end;

function NSText.isRulerVisible: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSText_isRulerVisible)), []));
end;

function NSText.selectedRange: NSRange;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSText_selectedRange)), []);
end;

procedure NSText.setSelectedRange(_range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_setSelectedRange)), _range);
end;

procedure NSText.scrollRangeToVisible(_range: NSRange);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_range: NSRange); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSText_scrollRangeToVisible)), _range);
end;

{$endif}
