{%mainunit appkit.pas}
(*
	NSOpenPanel.h
	Application Kit
	Copyright (c) 1994-2004, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSOPENPANEL_PAS_H}
{$define NSOPENPANEL_PAS_H}

{$include NSSavePanel.inc}

{$endif}
{$endif}

{$ifdef FORWARD}
  NSOpenPanel = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSOPENPANEL_PAS_C}
{$define NSOPENPANEL_PAS_C}

{$include NSSavePanel.inc}

  { NSOpenPanel }

  NSOpenPanel = class(NSSavePanel)
  public
    class function getClass: lobjc.id; override;
    constructor openPanel;
    function URLs: CFArrayRef;
    function filenames: CFArrayRef;
    function resolvesAliases: LongBool;
    procedure setResolvesAliases(_flag: LongBool);
    function canChooseDirectories: LongBool;
    procedure setCanChooseDirectories(_flag: LongBool);
    function allowsMultipleSelection: LongBool;
    procedure setAllowsMultipleSelection(_flag: LongBool);
    function canChooseFiles: LongBool;
    procedure setCanChooseFiles(_flag: LongBool);
    procedure beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo(_path: CFStringRef; _name: CFStringRef; _fileTypes: CFArrayRef; _docWindow: lobjc.id {NSWindow}; _delegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    procedure beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfo(_path: CFStringRef; _name: CFStringRef; _fileTypes: CFArrayRef; _delegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer);
    {.$endif}
    function runModalForDirectory_file_types(_path: CFStringRef; _name: CFStringRef; _fileTypes: CFArrayRef): Integer;
    function runModalForTypes(_fileTypes: CFArrayRef): Integer;
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSOpenPanel_NSOpenPanel = 'NSOpenPanel';
  StrNSOpenPanel_openPanel = 'openPanel';
  StrNSOpenPanel_URLs = 'URLs';
  StrNSOpenPanel_filenames = 'filenames';
  StrNSOpenPanel_resolvesAliases = 'resolvesAliases';
  StrNSOpenPanel_setResolvesAliases = 'setResolvesAliases:';
  StrNSOpenPanel_canChooseDirectories = 'canChooseDirectories';
  StrNSOpenPanel_setCanChooseDirectories = 'setCanChooseDirectories:';
  StrNSOpenPanel_allowsMultipleSelection = 'allowsMultipleSelection';
  StrNSOpenPanel_setAllowsMultipleSelection = 'setAllowsMultipleSelection:';
  StrNSOpenPanel_canChooseFiles = 'canChooseFiles';
  StrNSOpenPanel_setCanChooseFiles = 'setCanChooseFiles:';

  StrNSOpenPanel_beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo = 'beginSheetForDirectory:file:types:modalForWindow:modalDelegate:didEndSelector:contextInfo:';
  StrNSOpenPanel_beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfo = 'beginForDirectory:file:types:modelessDelegate:didEndSelector:contextInfo:';
  StrNSOpenPanel_runModalForDirectory_file_types = 'runModalForDirectory:file:types:';
  StrNSOpenPanel_runModalForTypes = 'runModalForTypes:';

{ NSOpenPanel }

class function NSOpenPanel.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSOpenPanel_NSOpenPanel);
end;

constructor NSOpenPanel.openPanel;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSOpenPanel_openPanel)));
end;

function NSOpenPanel.URLs: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSOpenPanel_URLs)), []));
end;

function NSOpenPanel.filenames: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSOpenPanel_filenames)), []));
end;

function NSOpenPanel.resolvesAliases: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSOpenPanel_resolvesAliases)), []));
end;

procedure NSOpenPanel.setResolvesAliases(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSOpenPanel_setResolvesAliases)), _flag);
end;

function NSOpenPanel.canChooseDirectories: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSOpenPanel_canChooseDirectories)), []));
end;

procedure NSOpenPanel.setCanChooseDirectories(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSOpenPanel_setCanChooseDirectories)), _flag);
end;

function NSOpenPanel.allowsMultipleSelection: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSOpenPanel_allowsMultipleSelection)), []));
end;

procedure NSOpenPanel.setAllowsMultipleSelection(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSOpenPanel_setAllowsMultipleSelection)), _flag);
end;

function NSOpenPanel.canChooseFiles: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSOpenPanel_canChooseFiles)), []));
end;

procedure NSOpenPanel.setCanChooseFiles(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSOpenPanel_setCanChooseFiles)), _flag);
end;

procedure NSOpenPanel.beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo(_path: CFStringRef; _name: CFStringRef; _fileTypes: CFArrayRef; _docWindow: lobjc.id {NSWindow}; _delegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_path: CFStringRef; _name: CFStringRef; _fileTypes: CFArrayRef; _docWindow: lobjc.id {NSWindow}; _delegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSOpenPanel_beginSheetForDirectory_file_types_modalForWindow_modalDelegate_didEndSelector_contextInfo)), _path, _name, _fileTypes, _docWindow, _delegate, _didEndSelector, _contextInfo);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
procedure NSOpenPanel.beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfo(_path: CFStringRef; _name: CFStringRef; _fileTypes: CFArrayRef; _delegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_path: CFStringRef; _name: CFStringRef; _fileTypes: CFArrayRef; _delegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSOpenPanel_beginForDirectory_file_types_modelessDelegate_didEndSelector_contextInfo)), _path, _name, _fileTypes, _delegate, _didEndSelector, _contextInfo);
end;

{.$endif}
function NSOpenPanel.runModalForDirectory_file_types(_path: CFStringRef; _name: CFStringRef; _fileTypes: CFArrayRef): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _name: CFStringRef; _fileTypes: CFArrayRef): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSOpenPanel_runModalForDirectory_file_types)), _path, _name, _fileTypes));
end;

function NSOpenPanel.runModalForTypes(_fileTypes: CFArrayRef): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_fileTypes: CFArrayRef): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSOpenPanel_runModalForTypes)), _fileTypes));
end;

{$endif}
