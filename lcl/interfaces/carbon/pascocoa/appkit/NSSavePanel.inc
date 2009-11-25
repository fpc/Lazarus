{%mainunit appkit.pas}
(*
	NSSavePanel.h
	Application Kit
	Copyright (c) 1994-2004, Apple Computer, Inc.
	All rights reserved.
*)

{$ifdef HEADER}
{$ifndef NSSAVEPANEL_PAS_H}
{$define NSSAVEPANEL_PAS_H}

{$include NSNibDeclarations.inc}
{$include NSPanel.inc}

const
  NSFileHandlingPanelCancelButton = NSCancelButton;
  NSFileHandlingPanelOKButton = NSOKButton;

type
  __SPFlags = packed  record 
    _bitflags1 : LongWord;
  end;
  _SPFlags = __SPFlags;

{$endif}
{$endif}

{$ifdef FORWARD}
  NSSavePanel = class;

{$endif}

{$ifdef CLASSES}
{$ifndef NSSAVEPANEL_PAS_C}
{$define NSSAVEPANEL_PAS_C}

{$include NSNibDeclarations.inc}
{$include NSPanel.inc}

  { NSSavePanel }

  NSSavePanel = class(NSPanel)
  public
    class function getClass: lobjc.id; override;
    constructor savePanel;
    function URL: CFURLRef;
    function filename: CFStringRef;
    function directory: CFStringRef;
    procedure setDirectory(_path: CFStringRef);
    function requiredFileType: CFStringRef;
    procedure setRequiredFileType(__type: CFStringRef);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    function allowedFileTypes: CFArrayRef;
    procedure setAllowedFileTypes(_types: CFArrayRef);
    function allowsOtherFileTypes: LongBool;
    procedure setAllowsOtherFileTypes(_flag: LongBool);
    {.$endif}
    function accessoryView: lobjc.id;{NSView}
    procedure setAccessoryView(_view: lobjc.id {NSView});
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    function delegate: lobjc.id;
    {.$endif}
    procedure setDelegate(_delegate: lobjc.id);
    function isExpanded: LongBool;
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    function canCreateDirectories: LongBool;
    procedure setCanCreateDirectories(_flag: LongBool);
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    function canSelectHiddenExtension: LongBool;
    {.$endif}
    procedure setCanSelectHiddenExtension(_flag: LongBool);
    function isExtensionHidden: LongBool;
    procedure setExtensionHidden(_flag: LongBool);
    function treatsFilePackagesAsDirectories: LongBool;
    procedure setTreatsFilePackagesAsDirectories(_flag: LongBool);
    function prompt: CFStringRef;
    procedure setPrompt(_prompt: CFStringRef);
    function title: CFStringRef;
    procedure setTitle(_title: CFStringRef);
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    function nameFieldLabel: CFStringRef;
    procedure setNameFieldLabel(__label: CFStringRef);
    {.$endif}
    {.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
    function message: CFStringRef;
    procedure setMessage(_message: CFStringRef);
    {.$endif}
    procedure validateVisibleColumns;
    //  A method that was deprecated in Mac OS 10.3.  -[NSSavePanel selectText:] does nothing.
    function selectText(_sender: lobjc.id): IBAction;
    function ok(_sender: lobjc.id): IBAction;
    function cancel(_sender: lobjc.id): IBAction;
    procedure beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo(_path: CFStringRef; _name: CFStringRef; _docWindow: lobjc.id {NSWindow}; _delegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer);
    function runModalForDirectory_file(_path: CFStringRef; _name: CFStringRef): Integer;
    function runModal: Integer;
  end;

{$endif}
{$endif}

{$ifdef IMPLEMENTATION}
const
  StrNSSavePanel_NSSavePanel = 'NSSavePanel';
  StrNSSavePanel_savePanel = 'savePanel';
  StrNSSavePanel_URL = 'URL';
  StrNSSavePanel_filename = 'filename';
  StrNSSavePanel_directory = 'directory';
  StrNSSavePanel_setDirectory = 'setDirectory:';
  StrNSSavePanel_requiredFileType = 'requiredFileType';
  StrNSSavePanel_setRequiredFileType = 'setRequiredFileType:';
  StrNSSavePanel_allowedFileTypes = 'allowedFileTypes';
  StrNSSavePanel_setAllowedFileTypes = 'setAllowedFileTypes:';
  StrNSSavePanel_allowsOtherFileTypes = 'allowsOtherFileTypes';
  StrNSSavePanel_setAllowsOtherFileTypes = 'setAllowsOtherFileTypes:';
  StrNSSavePanel_accessoryView = 'accessoryView';
  StrNSSavePanel_setAccessoryView = 'setAccessoryView:';
  StrNSSavePanel_delegate = 'delegate';
  StrNSSavePanel_setDelegate = 'setDelegate:';
  StrNSSavePanel_isExpanded = 'isExpanded';
  StrNSSavePanel_canCreateDirectories = 'canCreateDirectories';
  StrNSSavePanel_setCanCreateDirectories = 'setCanCreateDirectories:';
  StrNSSavePanel_canSelectHiddenExtension = 'canSelectHiddenExtension';
  StrNSSavePanel_setCanSelectHiddenExtension = 'setCanSelectHiddenExtension:';
  StrNSSavePanel_isExtensionHidden = 'isExtensionHidden';
  StrNSSavePanel_setExtensionHidden = 'setExtensionHidden:';
  StrNSSavePanel_treatsFilePackagesAsDirectories = 'treatsFilePackagesAsDirectories';
  StrNSSavePanel_setTreatsFilePackagesAsDirectories = 'setTreatsFilePackagesAsDirectories:';
  StrNSSavePanel_prompt = 'prompt';
  StrNSSavePanel_setPrompt = 'setPrompt:';
  StrNSSavePanel_title = 'title';
  StrNSSavePanel_setTitle = 'setTitle:';
  StrNSSavePanel_nameFieldLabel = 'nameFieldLabel';
  StrNSSavePanel_setNameFieldLabel = 'setNameFieldLabel:';
  StrNSSavePanel_message = 'message';
  StrNSSavePanel_setMessage = 'setMessage:';
  StrNSSavePanel_validateVisibleColumns = 'validateVisibleColumns';
  StrNSSavePanel_selectText = 'selectText:';

  StrNSSavePanel_ok = 'ok:';
  StrNSSavePanel_cancel = 'cancel:';
  StrNSSavePanel_beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo = 'beginSheetForDirectory:file:modalForWindow:modalDelegate:didEndSelector:contextInfo:';
  StrNSSavePanel_runModalForDirectory_file = 'runModalForDirectory:file:';
  StrNSSavePanel_runModal = 'runModal';

{ NSSavePanel }

class function NSSavePanel.getClass: lobjc.id;
begin
  Result := objc_getClass(StrNSSavePanel_NSSavePanel);
end;

constructor NSSavePanel.savePanel;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL): lobjc.id; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  ClassID := getClass();
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Handle := vmethod(ClassID, sel_registerName(PChar(StrNSSavePanel_savePanel)));
end;

function NSSavePanel.URL: CFURLRef;
begin
  Result := CFURLRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_URL)), []));
end;

function NSSavePanel.filename: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_filename)), []));
end;

function NSSavePanel.directory: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_directory)), []));
end;

procedure NSSavePanel.setDirectory(_path: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_path: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setDirectory)), _path);
end;

function NSSavePanel.requiredFileType: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_requiredFileType)), []));
end;

procedure NSSavePanel.setRequiredFileType(__type: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__type: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setRequiredFileType)), __type);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
function NSSavePanel.allowedFileTypes: CFArrayRef;
begin
  Result := CFArrayRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_allowedFileTypes)), []));
end;

procedure NSSavePanel.setAllowedFileTypes(_types: CFArrayRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_types: CFArrayRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setAllowedFileTypes)), _types);
end;

function NSSavePanel.allowsOtherFileTypes: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_allowsOtherFileTypes)), []));
end;

procedure NSSavePanel.setAllowsOtherFileTypes(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setAllowsOtherFileTypes)), _flag);
end;

{.$endif}
function NSSavePanel.accessoryView: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_accessoryView)), []));
end;

procedure NSSavePanel.setAccessoryView(_view: lobjc.id {NSView});
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_view: lobjc.id {NSView}); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setAccessoryView)), _view);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
function NSSavePanel.delegate: lobjc.id;
begin
  Result := lobjc.id(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_delegate)), []));
end;

{.$endif}
procedure NSSavePanel.setDelegate(_delegate: lobjc.id);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_delegate: lobjc.id); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setDelegate)), _delegate);
end;

function NSSavePanel.isExpanded: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_isExpanded)), []));
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
function NSSavePanel.canCreateDirectories: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_canCreateDirectories)), []));
end;

procedure NSSavePanel.setCanCreateDirectories(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setCanCreateDirectories)), _flag);
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
function NSSavePanel.canSelectHiddenExtension: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_canSelectHiddenExtension)), []));
end;

{.$endif}
procedure NSSavePanel.setCanSelectHiddenExtension(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setCanSelectHiddenExtension)), _flag);
end;

function NSSavePanel.isExtensionHidden: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_isExtensionHidden)), []));
end;

procedure NSSavePanel.setExtensionHidden(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setExtensionHidden)), _flag);
end;

function NSSavePanel.treatsFilePackagesAsDirectories: LongBool;
begin
  Result := LongBool(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_treatsFilePackagesAsDirectories)), []));
end;

procedure NSSavePanel.setTreatsFilePackagesAsDirectories(_flag: LongBool);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_flag: LongBool); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setTreatsFilePackagesAsDirectories)), _flag);
end;

function NSSavePanel.prompt: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_prompt)), []));
end;

procedure NSSavePanel.setPrompt(_prompt: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_prompt: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setPrompt)), _prompt);
end;

function NSSavePanel.title: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_title)), []));
end;

procedure NSSavePanel.setTitle(_title: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_title: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setTitle)), _title);
end;

{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
function NSSavePanel.nameFieldLabel: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_nameFieldLabel)), []));
end;

procedure NSSavePanel.setNameFieldLabel(__label: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;__label: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setNameFieldLabel)), __label);
end;

{.$endif}
{.$ifdef  MAC_OS_X_VERSION_MAX_ALLOWED >= MAC_OS_X_VERSION_10_3}
function NSSavePanel.message: CFStringRef;
begin
  Result := CFStringRef(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_message)), []));
end;

procedure NSSavePanel.setMessage(_message: CFStringRef);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_message: CFStringRef); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_setMessage)), _message);
end;

{.$endif}
procedure NSSavePanel.validateVisibleColumns;
begin
  objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_validateVisibleColumns)), []);
end;

function NSSavePanel.selectText(_sender: lobjc.id): IBAction;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_sender: lobjc.id): IBAction; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := IBAction(vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_selectText)), _sender));
end;

function NSSavePanel.ok(_sender: lobjc.id): IBAction;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_sender: lobjc.id): IBAction; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := IBAction(vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_ok)), _sender));
end;

function NSSavePanel.cancel(_sender: lobjc.id): IBAction;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_sender: lobjc.id): IBAction; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := IBAction(vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_cancel)), _sender));
end;

procedure NSSavePanel.beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo(_path: CFStringRef; _name: CFStringRef; _docWindow: lobjc.id {NSWindow}; _delegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer);
type
  TmsgSendWrapper = procedure (param1: lobjc.id; param2: SEL;_path: CFStringRef; _name: CFStringRef; _docWindow: lobjc.id {NSWindow}; _delegate: lobjc.id; _didEndSelector: SEL; _contextInfo: Pointer); cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_beginSheetForDirectory_file_modalForWindow_modalDelegate_didEndSelector_contextInfo)), _path, _name, _docWindow, _delegate, _didEndSelector, _contextInfo);
end;

function NSSavePanel.runModalForDirectory_file(_path: CFStringRef; _name: CFStringRef): Integer;
type
  TmsgSendWrapper = function (param1: lobjc.id; param2: SEL;_path: CFStringRef; _name: CFStringRef): Integer; cdecl;
var
  vmethod: TmsgSendWrapper;
begin
  vmethod := TmsgSendWrapper(@objc_msgSend);
  Result := Integer(vmethod(Handle, sel_registerName(PChar(StrNSSavePanel_runModalForDirectory_file)), _path, _name));
end;

function NSSavePanel.runModal: Integer;
begin
  Result := Integer(objc_msgSend(Handle, sel_registerName(PChar(StrNSSavePanel_runModal)), []));
end;

{$endif}
