{%mainunit appkit.pas}
(*
	NSTextField.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSTEXTFIELD_PAS_H}
{$define NSTEXTFIELD_PAS_H}

{$include NSControl.inc}
{$include NSTextFieldCell.inc}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSTextField = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSTEXTFIELD_PAS_C}
{$define NSTEXTFIELD_PAS_C}

{$include NSControl.inc}
{$include NSTextFieldCell.inc}

  { NSTextField }

  NSTextField = class(NSControl)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    procedure setBackgroundColor(_color: lobjc.id {NSColor});
    function backgroundColor: lobjc.id;{NSColor}
    procedure setDrawsBackground(_flag: LongBool);
    function drawsBackground: LongBool;
    procedure setTextColor(_color: lobjc.id {NSColor});
    function textColor: lobjc.id;{NSColor}
    function isBordered: LongBool;
    procedure setBordered(_flag: LongBool);
    function isBezeled: LongBool;
    procedure setBezeled(_flag: LongBool);
    function isEditable: LongBool;
    procedure setEditable(_flag: LongBool);
    function isSelectable: LongBool;
    procedure setSelectable(_flag: LongBool);
    procedure selectText(_sender: lobjc.id);
    function delegate: lobjc.id;
    procedure setDelegate(_anObject: lobjc.id);
    function textShouldBeginEditing(_textObject: lobjc.id {NSText}): LongBool;
    function textShouldEndEditing(_textObject: lobjc.id {NSText}): LongBool;
    procedure textDidBeginEditing(_notification: lobjc.id {NSNotification});
    procedure textDidEndEditing(_notification: lobjc.id {NSNotification});
    procedure textDidChange(_notification: lobjc.id {NSNotification});
    function acceptsFirstResponder: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
    procedure setBezelStyle(_style: NSTextFieldBezelStyle);
    function bezelStyle: NSTextFieldBezelStyle;
    {.$endif}
    procedure setTitleWithMnemonic(_stringWithAmpersand: CFStringRef);
    function allowsEditingTextAttributes: LongBool;
    procedure setAllowsEditingTextAttributes(_flag: LongBool);
    function importsGraphics: LongBool;
    procedure setImportsGraphics(_flag: LongBool);
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSTextField_NSTextField = 'NSTextField';
  StrNSTextField_setBackgroundColor = 'setBackgroundColor:';
  StrNSTextField_backgroundColor = 'backgroundColor';
  StrNSTextField_setDrawsBackground = 'setDrawsBackground:';
  StrNSTextField_drawsBackground = 'drawsBackground';
  StrNSTextField_setTextColor = 'setTextColor:';
  StrNSTextField_textColor = 'textColor';
  StrNSTextField_isBordered = 'isBordered';
  StrNSTextField_setBordered = 'setBordered:';
  StrNSTextField_isBezeled = 'isBezeled';
  StrNSTextField_setBezeled = 'setBezeled:';
  StrNSTextField_isEditable = 'isEditable';
  StrNSTextField_setEditable = 'setEditable:';
  StrNSTextField_isSelectable = 'isSelectable';
  StrNSTextField_setSelectable = 'setSelectable:';
  StrNSTextField_selectText = 'selectText:';
  StrNSTextField_delegate = 'delegate';
  StrNSTextField_setDelegate = 'setDelegate:';
  StrNSTextField_textShouldBeginEditing = 'textShouldBeginEditing:';
  StrNSTextField_textShouldEndEditing = 'textShouldEndEditing:';
  StrNSTextField_textDidBeginEditing = 'textDidBeginEditing:';
  StrNSTextField_textDidEndEditing = 'textDidEndEditing:';
  StrNSTextField_textDidChange = 'textDidChange:';
  StrNSTextField_acceptsFirstResponder = 'acceptsFirstResponder';
  StrNSTextField_setBezelStyle = 'setBezelStyle:';
  StrNSTextField_bezelStyle = 'bezelStyle';

  StrNSTextField_setTitleWithMnemonic = 'setTitleWithMnemonic:';

  StrNSTextField_allowsEditingTextAttributes = 'allowsEditingTextAttributes';
  StrNSTextField_setAllowsEditingTextAttributes = 'setAllowsEditingTextAttributes:';
  StrNSTextField_importsGraphics = 'importsGraphics';
  StrNSTextField_setImportsGraphics = 'setImportsGraphics:';

{ NSTextField }

class function NSTextField.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSTextField_NSTextField);
end;

procedure NSTextField.setBackgroundColor(_color: lobjc.id {NSColor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_color: lobjc.id {NSColor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setBackgroundColor)), _color);
end;

function NSTextField.backgroundColor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_backgroundColor)), []));
end;

procedure NSTextField.setDrawsBackground(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setDrawsBackground)), _flag);
end;

function NSTextField.drawsBackground: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_drawsBackground)), []));
end;

procedure NSTextField.setTextColor(_color: lobjc.id {NSColor});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_color: lobjc.id {NSColor}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setTextColor)), _color);
end;

function NSTextField.textColor: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_textColor)), []));
end;

function NSTextField.isBordered: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_isBordered)), []));
end;

procedure NSTextField.setBordered(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setBordered)), _flag);
end;

function NSTextField.isBezeled: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_isBezeled)), []));
end;

procedure NSTextField.setBezeled(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setBezeled)), _flag);
end;

function NSTextField.isEditable: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_isEditable)), []));
end;

procedure NSTextField.setEditable(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setEditable)), _flag);
end;

function NSTextField.isSelectable: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_isSelectable)), []));
end;

procedure NSTextField.setSelectable(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setSelectable)), _flag);
end;

procedure NSTextField.selectText(_sender: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_sender: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_selectText)), _sender);
end;

function NSTextField.delegate: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_delegate)), []));
end;

procedure NSTextField.setDelegate(_anObject: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_anObject: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setDelegate)), _anObject);
end;

function NSTextField.textShouldBeginEditing(_textObject: lobjc.id {NSText}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_textObject: lobjc.id {NSText}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextField_textShouldBeginEditing)), _textObject));
end;

function NSTextField.textShouldEndEditing(_textObject: lobjc.id {NSText}): LongBool;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_textObject: lobjc.id {NSText}): LongBool; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := LongBool(vmethod(Handle, sel_registerName(PChar(StrNSTextField_textShouldEndEditing)), _textObject));
end;

procedure NSTextField.textDidBeginEditing(_notification: lobjc.id {NSNotification});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_notification: lobjc.id {NSNotification}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_textDidBeginEditing)), _notification);
end;

procedure NSTextField.textDidEndEditing(_notification: lobjc.id {NSNotification});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_notification: lobjc.id {NSNotification}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_textDidEndEditing)), _notification);
end;

procedure NSTextField.textDidChange(_notification: lobjc.id {NSNotification});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_notification: lobjc.id {NSNotification}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_textDidChange)), _notification);
end;

function NSTextField.acceptsFirstResponder: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_acceptsFirstResponder)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_2}
procedure NSTextField.setBezelStyle(_style: NSTextFieldBezelStyle);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_style: NSTextFieldBezelStyle); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setBezelStyle)), _style);
end;

function NSTextField.bezelStyle: NSTextFieldBezelStyle;
begin
  Result := NSTextFieldBezelStyle(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_bezelStyle)), []));
end;

{.$endif}
procedure NSTextField.setTitleWithMnemonic(_stringWithAmpersand: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_stringWithAmpersand: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setTitleWithMnemonic)), _stringWithAmpersand);
end;

function NSTextField.allowsEditingTextAttributes: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_allowsEditingTextAttributes)), []));
end;

procedure NSTextField.setAllowsEditingTextAttributes(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setAllowsEditingTextAttributes)), _flag);
end;

function NSTextField.importsGraphics: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSTextField_importsGraphics)), []));
end;

procedure NSTextField.setImportsGraphics(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSTextField_setImportsGraphics)), _flag);
end;

{$endif}
