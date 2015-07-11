{%mainunit appkit.pas}
(*
	NSBox.h
	Application Kit
	Copyright (c) 1994-2005, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSBOX_PAS_H}
{$define NSBOX_PAS_H}

{$include NSView.inc}

type
  NSTitlePosition = (
    NSNoTitle = 0, 
    NSAboveTop = 1, 
    NSAtTop = 2, 
    NSBelowTop = 3, 
    NSAboveBottom = 4, 
    NSAtBottom = 5, 
    NSBelowBottom = 6
  );


type
  NSBoxType = (
    NSBoxPrimary = 0, 
    NSBoxSecondary = 1, 
    NSBoxSeparator = 2, 
    NSBoxOldStyle = 3
  );


{$endif}
{$endif}

{$ifdef FORWARD}
  NSBox = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSBOX_PAS_C}
{$define NSBOX_PAS_C}

{$include NSView.inc}

  { NSBox }

  NSBox = class(NSView)
  public
    class function getClass: lobjc.id; override;
    // All instance variables are private
    function borderType: NSBorderType;
    function titlePosition: NSTitlePosition;
    procedure setBorderType(_aType: NSBorderType);
    procedure setBoxType(_boxType: NSBoxType);
    function boxType: NSBoxType;
    procedure setTitlePosition(_aPosition: NSTitlePosition);
    function title: CFStringRef;
    procedure setTitle(_aString: CFStringRef);
    function titleFont: lobjc.id;{NSFont}
    procedure setTitleFont(_fontObj: lobjc.id {NSFont});
    function borderRect: NSRect;
    function titleRect: NSRect;
    function titleCell: lobjc.id;
    procedure sizeToFit;
    function contentViewMargins: NSSize;
    procedure setContentViewMargins(_offsetSize: NSSize);
    procedure setFrameFromContentFrame(_contentFrame: NSRect);
    function contentView: lobjc.id;
    procedure setContentView(_aView: lobjc.id {NSView});
    procedure setTitleWithMnemonic(_stringWithAmpersand: CFStringRef);
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSBox_NSBox = 'NSBox';
  StrNSBox_borderType = 'borderType';
  StrNSBox_titlePosition = 'titlePosition';
  StrNSBox_setBorderType = 'setBorderType:';
  StrNSBox_setBoxType = 'setBoxType:';
  StrNSBox_boxType = 'boxType';
  StrNSBox_setTitlePosition = 'setTitlePosition:';
  StrNSBox_title = 'title';
  StrNSBox_setTitle = 'setTitle:';
  StrNSBox_titleFont = 'titleFont';
  StrNSBox_setTitleFont = 'setTitleFont:';
  StrNSBox_borderRect = 'borderRect';
  StrNSBox_titleRect = 'titleRect';
  StrNSBox_titleCell = 'titleCell';
  StrNSBox_sizeToFit = 'sizeToFit';
  StrNSBox_contentViewMargins = 'contentViewMargins';
  StrNSBox_setContentViewMargins = 'setContentViewMargins:';
  StrNSBox_setFrameFromContentFrame = 'setFrameFromContentFrame:';
  StrNSBox_contentView = 'contentView';
  StrNSBox_setContentView = 'setContentView:';

  StrNSBox_setTitleWithMnemonic = 'setTitleWithMnemonic:';

{ NSBox }

class function NSBox.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSBox_NSBox);
end;

function NSBox.borderType: NSBorderType;
begin
  Result := NSBorderType(objc_msgSend(Handle, sel_registerName(PChar(StrNSBox_borderType)), []));
end;

function NSBox.titlePosition: NSTitlePosition;
begin
  Result := NSTitlePosition(objc_msgSend(Handle, sel_registerName(PChar(StrNSBox_titlePosition)), []));
end;

procedure NSBox.setBorderType(_aType: NSBorderType);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aType: NSBorderType); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBox_setBorderType)), _aType);
end;

procedure NSBox.setBoxType(_boxType: NSBoxType);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_boxType: NSBoxType); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBox_setBoxType)), _boxType);
end;

function NSBox.boxType: NSBoxType;
begin
  Result := NSBoxType(objc_msgSend(Handle, sel_registerName(PChar(StrNSBox_boxType)), []));
end;

procedure NSBox.setTitlePosition(_aPosition: NSTitlePosition);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aPosition: NSTitlePosition); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBox_setTitlePosition)), _aPosition);
end;

function NSBox.title: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSBox_title)), []));
end;

procedure NSBox.setTitle(_aString: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aString: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBox_setTitle)), _aString);
end;

function NSBox.titleFont: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSBox_titleFont)), []));
end;

procedure NSBox.setTitleFont(_fontObj: lobjc.id {NSFont});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_fontObj: lobjc.id {NSFont}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBox_setTitleFont)), _fontObj);
end;

function NSBox.borderRect: NSRect;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSBox_borderRect)), []);
end;

function NSBox.titleRect: NSRect;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSBox_titleRect)), []);
end;

function NSBox.titleCell: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSBox_titleCell)), []));
end;

procedure NSBox.sizeToFit;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSBox_sizeToFit)), []);
end;

function NSBox.contentViewMargins: NSSize;
begin
  objc_msgSend_stret(@Result, Handle, sel_registerName(PChar(StrNSBox_contentViewMargins)), []);
end;

procedure NSBox.setContentViewMargins(_offsetSize: NSSize);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_offsetSize: NSSize); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBox_setContentViewMargins)), _offsetSize);
end;

procedure NSBox.setFrameFromContentFrame(_contentFrame: NSRect);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_contentFrame: NSRect); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBox_setFrameFromContentFrame)), _contentFrame);
end;

function NSBox.contentView: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSBox_contentView)), []));
end;

procedure NSBox.setContentView(_aView: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_aView: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBox_setContentView)), _aView);
end;

procedure NSBox.setTitleWithMnemonic(_stringWithAmpersand: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_stringWithAmpersand: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSBox_setTitleWithMnemonic)), _stringWithAmpersand);
end;

{$endif}
