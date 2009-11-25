{%mainunit appkit.pas}
(*
        NSStatusItem.h
        Application Kit
        Copyright (c) 1997-2005, Apple Computer, Inc.
        All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSSTATUSITEM_PAS_H}
{$define NSSTATUSITEM_PAS_H}


type
  _SBIFlags = packed  record 
    _bitflags1 : LongWord;
  end;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSStatusItem = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTATUSITEM_PAS_C}
{$define NSSTATUSITEM_PAS_C}


  { NSStatusItem }

  NSStatusItem = class(NSObject)
  public
    class function getClass: lobjc.id; override;
    function statusBar: lobjc.id;{NSStatusBar}
    function length: Single;
    procedure setLength(_length: Single);
    function action: SEL;
    procedure setAction(_action: SEL);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
    function doubleAction: SEL;
    procedure setDoubleAction(_action: SEL);
    {.$endif}
    function target: lobjc.id;
    procedure setTarget(_target: lobjc.id);
    function title: CFStringRef;
    procedure setTitle(_title: CFStringRef);
    function attributedTitle: lobjc.id;{NSAttributedString}
    procedure setAttributedTitle(_title: lobjc.id {NSAttributedString});
    function image: lobjc.id;{NSImage}
    procedure setImage(_image: lobjc.id {NSImage});
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    function alternateImage: lobjc.id;{NSImage}
    procedure setAlternateImage(_image: lobjc.id {NSImage});
    {.$endif}
    function menu: lobjc.id;{NSMenu}
    procedure setMenu(_menu: lobjc.id {NSMenu});
    function isEnabled: LongBool;
    procedure setEnabled(_enabled: LongBool);
    function toolTip: CFStringRef;
    procedure setToolTip(_toolTip: CFStringRef);
    procedure setHighlightMode(_highlightMode: LongBool);
    function highlightMode: LongBool;
    procedure sendActionOn(_mask: Integer);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure popUpStatusItemMenu(_menu: lobjc.id {NSMenu});
    procedure drawStatusBarBackgroundInRect_withHighlight(_rect: NSRect; _highlight: LongBool);
    {.$endif}
    function view: lobjc.id;{NSView}
    procedure setView(_view: lobjc.id {NSView});
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSStatusItem_NSStatusItem = 'NSStatusItem';
  StrNSStatusItem_statusBar = 'statusBar';
  StrNSStatusItem_length = 'length';
  StrNSStatusItem_setLength = 'setLength:';

  StrNSStatusItem_action = 'action';
  StrNSStatusItem_setAction = 'setAction:';
  StrNSStatusItem_doubleAction = 'doubleAction';
  StrNSStatusItem_setDoubleAction = 'setDoubleAction:';
  StrNSStatusItem_target = 'target';
  StrNSStatusItem_setTarget = 'setTarget:';
  StrNSStatusItem_title = 'title';
  StrNSStatusItem_setTitle = 'setTitle:';
  StrNSStatusItem_attributedTitle = 'attributedTitle';
  StrNSStatusItem_setAttributedTitle = 'setAttributedTitle:';
  StrNSStatusItem_image = 'image';
  StrNSStatusItem_setImage = 'setImage:';
  StrNSStatusItem_alternateImage = 'alternateImage';
  StrNSStatusItem_setAlternateImage = 'setAlternateImage:';
  StrNSStatusItem_menu = 'menu';
  StrNSStatusItem_setMenu = 'setMenu:';
  StrNSStatusItem_isEnabled = 'isEnabled';
  StrNSStatusItem_setEnabled = 'setEnabled:';
  StrNSStatusItem_toolTip = 'toolTip';
  StrNSStatusItem_setToolTip = 'setToolTip:';
  StrNSStatusItem_setHighlightMode = 'setHighlightMode:';
  StrNSStatusItem_highlightMode = 'highlightMode';
  StrNSStatusItem_sendActionOn = 'sendActionOn:';
  StrNSStatusItem_popUpStatusItemMenu = 'popUpStatusItemMenu:';
  StrNSStatusItem_drawStatusBarBackgroundInRect_withHighlight = 'drawStatusBarBackgroundInRect:withHighlight:';

  StrNSStatusItem_view = 'view';
  StrNSStatusItem_setView = 'setView:';

{ NSStatusItem }

class function NSStatusItem.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSStatusItem_NSStatusItem);
end;

function NSStatusItem.statusBar: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_statusBar)), []));
end;

function NSStatusItem.length: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_length)), []));
end;

procedure NSStatusItem.setLength(_length: Single);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_length: Single); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setLength)), _length);
end;

function NSStatusItem.action: SEL;
begin
  Result := SEL(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_action)), []));
end;

procedure NSStatusItem.setAction(_action: SEL);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_action: SEL); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setAction)), _action);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_4}
function NSStatusItem.doubleAction: SEL;
begin
  Result := SEL(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_doubleAction)), []));
end;

procedure NSStatusItem.setDoubleAction(_action: SEL);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_action: SEL); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setDoubleAction)), _action);
end;

{.$endif}
function NSStatusItem.target: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_target)), []));
end;

procedure NSStatusItem.setTarget(_target: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_target: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setTarget)), _target);
end;

function NSStatusItem.title: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_title)), []));
end;

procedure NSStatusItem.setTitle(_title: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_title: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setTitle)), _title);
end;

function NSStatusItem.attributedTitle: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_attributedTitle)), []));
end;

procedure NSStatusItem.setAttributedTitle(_title: lobjc.id {NSAttributedString});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_title: lobjc.id {NSAttributedString}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setAttributedTitle)), _title);
end;

function NSStatusItem.image: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_image)), []));
end;

procedure NSStatusItem.setImage(_image: lobjc.id {NSImage});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_image: lobjc.id {NSImage}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setImage)), _image);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
function NSStatusItem.alternateImage: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_alternateImage)), []));
end;

procedure NSStatusItem.setAlternateImage(_image: lobjc.id {NSImage});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_image: lobjc.id {NSImage}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setAlternateImage)), _image);
end;

{.$endif}
function NSStatusItem.menu: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_menu)), []));
end;

procedure NSStatusItem.setMenu(_menu: lobjc.id {NSMenu});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_menu: lobjc.id {NSMenu}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setMenu)), _menu);
end;

function NSStatusItem.isEnabled: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_isEnabled)), []));
end;

procedure NSStatusItem.setEnabled(_enabled: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_enabled: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setEnabled)), _enabled);
end;

function NSStatusItem.toolTip: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_toolTip)), []));
end;

procedure NSStatusItem.setToolTip(_toolTip: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_toolTip: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setToolTip)), _toolTip);
end;

procedure NSStatusItem.setHighlightMode(_highlightMode: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_highlightMode: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setHighlightMode)), _highlightMode);
end;

function NSStatusItem.highlightMode: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_highlightMode)), []));
end;

procedure NSStatusItem.sendActionOn(_mask: Integer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_mask: Integer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_sendActionOn)), _mask);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSStatusItem.popUpStatusItemMenu(_menu: lobjc.id {NSMenu});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_menu: lobjc.id {NSMenu}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_popUpStatusItemMenu)), _menu);
end;

procedure NSStatusItem.drawStatusBarBackgroundInRect_withHighlight(_rect: NSRect; _highlight: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_rect: NSRect; _highlight: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_drawStatusBarBackgroundInRect_withHighlight)), _rect, _highlight);
end;

{.$endif}
function NSStatusItem.view: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusItem_view)), []));
end;

procedure NSStatusItem.setView(_view: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_view: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusItem_setView)), _view);
end;

{$endif}
