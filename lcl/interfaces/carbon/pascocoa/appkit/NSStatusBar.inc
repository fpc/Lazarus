{%mainunit appkit.pas}
(*
        NSStatusBar.h
        Application Kit
        Copyright (c) 1997-2005, Apple Computer, Inc.
        All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSSTATUSBAR_PAS_H}
{$define NSSTATUSBAR_PAS_H}

const
  NSVariableStatusItemLength = (-1);
  NSSquareStatusItemLength = (-2);

{$endif}
{$endif}

{$ifdef FORWARD}
  NSStatusBar = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSTATUSBAR_PAS_C}
{$define NSSTATUSBAR_PAS_C}


  { NSStatusBar }

  NSStatusBar = class(NSObject)
  public
    class function getClass: objc.id; override;
    constructor systemStatusBar;
    function statusItemWithLength(_length: Single): objc.id;{NSStatusItem}
    procedure removeStatusItem(_item: objc.id {NSStatusItem});
    function isVertical: LongBool;
    function thickness: Single;
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSStatusBar_NSStatusBar = 'NSStatusBar';
  StrNSStatusBar_systemStatusBar = 'systemStatusBar';
  StrNSStatusBar_statusItemWithLength = 'statusItemWithLength:';
  StrNSStatusBar_removeStatusItem = 'removeStatusItem:';
  StrNSStatusBar_isVertical = 'isVertical';
  StrNSStatusBar_thickness = 'thickness';

{ NSStatusBar }

class function NSStatusBar.getClass: objc.id;
begin
  Result := objc_getClass(StrNSStatusBar_NSStatusBar);
end;

constructor NSStatusBar.systemStatusBar;
type
  TmsgSendWrapper = function (param1: objc.id; param2: SEL): objc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSStatusBar_systemStatusBar)));
end;

function NSStatusBar.statusItemWithLength(_length: Single): objc.id;
type
  TmsgSendWrapper = function (param1: objc.id; param2: SEL;_length: Single): objc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := objc.id(vmethod(Handle, sel_registerName(PChar(StrNSStatusBar_statusItemWithLength)), _length));
end;

procedure NSStatusBar.removeStatusItem(_item: objc.id {NSStatusItem});
type
  TmsgSendWrapper = procedure (param1: objc.id; param2: SEL;_item: objc.id {NSStatusItem}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSStatusBar_removeStatusItem)), _item);
end;

function NSStatusBar.isVertical: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusBar_isVertical)), []));
end;

function NSStatusBar.thickness: Single;
begin
  Result := Single(objc_msgSend(Handle, sel_registerName(PChar(StrNSStatusBar_thickness)), []));
end;

{$endif}
