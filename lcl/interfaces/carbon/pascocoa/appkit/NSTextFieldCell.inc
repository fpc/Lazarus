{%mainunit appkit.pas}
(*
	NSTextFieldCell.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSTEXTFIELDCELL_PAS_H}
{$define NSTEXTFIELDCELL_PAS_H}

{$include NSActionCell.inc}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}
  {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
  {.$endif}

type
  NSTextFieldBezelStyle = (
    NSTextFieldSquareBezel = 0, 
    NSTextFieldRoundedBezel = 1
  );


{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextFieldCell = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTFIELDCELL_PAS_C}
{$define NSTEXTFIELDCELL_PAS_C}

{$include NSActionCell.inc}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    {.$endif}

  { NSTextFieldCell }

  NSTextFieldCell = class(NSActionCell)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    procedure setBackgroundColor(_color: lobjc.id {NSColor});
    function backgroundColor: lobjc.id;{NSColor}
    procedure setDrawsBackground(_flag: LongBool);
    function drawsBackground: LongBool;
    procedure setTextColor(_color: lobjc.id {NSColor});
    function textColor: lobjc.id;{NSColor}
    function setUpFieldEditorAttributes(_textObj: lobjc.id {NSText}): lobjc.id;{NSText}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    procedure setBezelStyle(_style: NSTextFieldBezelStyle);
    function bezelStyle: NSTextFieldBezelStyle;
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure setPlaceholderString(__string: CFStringRef);
    function placeholderString: CFStringRef;
    procedure setPlaceholderAttributedString(__string: lobjc.id {NSAttributedString});
    function placeholderAttributedString: lobjc.id;{NSAttributedString}
    {.$endif}
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSTextFieldCell_NSTextFieldCell = 'NSTextFieldCell';
  StrNSTextFieldCell_setBackgroundColor = 'setBackgroundColor:';
  StrNSTextFieldCell_backgroundColor = 'backgroundColor';
  StrNSTextFieldCell_setDrawsBackground = 'setDrawsBackground:';
  StrNSTextFieldCell_drawsBackground = 'drawsBackground';
  StrNSTextFieldCell_setTextColor = 'setTextColor:';
  StrNSTextFieldCell_textColor = 'textColor';
  StrNSTextFieldCell_setUpFieldEditorAttributes = 'setUpFieldEditorAttributes:';
  StrNSTextFieldCell_setBezelStyle = 'setBezelStyle:';
  StrNSTextFieldCell_bezelStyle = 'bezelStyle';
  StrNSTextFieldCell_setPlaceholderString = 'setPlaceholderString:';
  StrNSTextFieldCell_placeholderString = 'placeholderString';
  StrNSTextFieldCell_setPlaceholderAttributedString = 'setPlaceholderAttributedString:';
  StrNSTextFieldCell_placeholderAttributedString = 'placeholderAttributedString';

{ NSTextFieldCell }

class function NSTextFieldCell.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSTextFieldCell_NSTextFieldCell);
end;

procedure NSTextFieldCell.setBackgroundColor(_color: lobjc.id {NSColor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_color: lobjc.id {NSColor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextFieldCell_setBackgroundColor)), _color);
end;

function NSTextFieldCell.backgroundColor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextFieldCell_backgroundColor)), []));
end;

procedure NSTextFieldCell.setDrawsBackground(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextFieldCell_setDrawsBackground)), _flag);
end;

function NSTextFieldCell.drawsBackground: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextFieldCell_drawsBackground)), []));
end;

procedure NSTextFieldCell.setTextColor(_color: lobjc.id {NSColor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_color: lobjc.id {NSColor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextFieldCell_setTextColor)), _color);
end;

function NSTextFieldCell.textColor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextFieldCell_textColor)), []));
end;

function NSTextFieldCell.setUpFieldEditorAttributes(_textObj: lobjc.id {NSText}): lobjc.id;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_textObj: lobjc.id {NSText}): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := lobjc.id(vmethod(Handle, sel_registerName(PChar(StrNSTextFieldCell_setUpFieldEditorAttributes)), _textObj));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
procedure NSTextFieldCell.setBezelStyle(_style: NSTextFieldBezelStyle);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_style: NSTextFieldBezelStyle); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextFieldCell_setBezelStyle)), _style);
end;

function NSTextFieldCell.bezelStyle: NSTextFieldBezelStyle;
begin
  Result := NSTextFieldBezelStyle(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextFieldCell_bezelStyle)), []));
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSTextFieldCell.setPlaceholderString(__string: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__string: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextFieldCell_setPlaceholderString)), __string);
end;

function NSTextFieldCell.placeholderString: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextFieldCell_placeholderString)), []));
end;

procedure NSTextFieldCell.setPlaceholderAttributedString(__string: lobjc.id {NSAttributedString});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__string: lobjc.id {NSAttributedString}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextFieldCell_setPlaceholderAttributedString)), __string);
end;

function NSTextFieldCell.placeholderAttributedString: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextFieldCell_placeholderAttributedString)), []));
end;

{.$endif}
{$endif}
