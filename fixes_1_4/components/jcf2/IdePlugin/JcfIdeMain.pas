unit JcfIdeMain;

{ AFS 7 Jan 2K
  JEDI Code Format IDE plugin main class

  global object that implements the callbacks from the menu items }


{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is JcfIdeMain, released May 2003.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

uses
  { delphi }Windows, SysUtils, Classes,
  { delphi design time }
  ToolsAPI,
  { local}
  EditorConverter, FileConverter, ConvertTypes;

type
  TJcfIdeMain = class(TObject)
  private
    fcEditorConverter: TEditorConverter;
    fcFileConverter: TFileConverter;

    procedure MakeEditorConverter;

    procedure LogIDEMessage(const psFile, psMessage: string;
      const peMessageType: TStatusMessageType;
      const piY, piX: integer);
    procedure FormatFile(const psFileName: string);

    procedure ClearToolMessages;
    procedure ConvertEditor(const pciEditor: IOTASourceEditor);


  protected
  public
    constructor Create;
    destructor Destroy; override;

    procedure ShortcutKeyCallback(const Context: IOTAKeyContext;
      KeyCode: TShortcut; var BindingResult: TKeyBindingResult);

    procedure DoFormatCurrentIDEWindow(Sender: TObject);
    procedure DoFormatProject(Sender: TObject);
    procedure DoFormatOpen(Sender: TObject);
    procedure DoRegistrySettings(Sender: TObject);
    procedure DoFormatSettings(Sender: TObject);
    procedure DoAbout(Sender: TObject);
  end;


implementation

uses
  { delphi }
  Menus, Dialogs, Controls,
  { local }
  JcfStringUtils,
  fAllSettings, fAbout, JcfRegistrySettings, fRegistrySettings;


function FileIsAllowedType(const psFileName: string): boolean;
const
  ALLOWED_FILE_TYPES: array[1..3] of string = ('.pas', '.dpr', '.dpk');
begin
  Result := StrIsOneOf(StrRight(psFileName, 4), ALLOWED_FILE_TYPES);
end;


{ the function GetCurrentProject is from Erik's OpenTools API FAQ and Resources
  http://www.gexperts.org/opentools/
}
// Modified from code posted by Ray Lischner (www.tempest-sw.com)
function GetCurrentProject: IOTAProject;
var
  Services: IOTAModuleServices;
  Module: IOTAModule;
  Project: IOTAProject;
  ProjectGroup: IOTAProjectGroup;
  MultipleProjects: boolean;
  I: integer;
begin
  Result   := nil;
  MultipleProjects := False;
  Services := BorlandIDEServices as IOTAModuleServices;
  for I := 0 to Services.ModuleCount - 1 do
  begin
    Module := Services.Modules[I];
    if Module.QueryInterface(IOTAProjectGroup, ProjectGroup) = S_OK then
    begin
      Result := ProjectGroup.ActiveProject;
      Exit;
    end
    else if Module.QueryInterface(IOTAProject, Project) = S_OK then
    begin
      if Result = nil then
        // Found the first project, so save it
        Result := Project
      else
        MultipleProjects := True;
      // It doesn't look good, but keep searching for a project group
    end;
  end;
  if MultipleProjects then
    Result := nil;
end;


constructor TJcfIdeMain.Create;
begin
  inherited;
  { both of these are created on demand }
  fcEditorConverter := nil;
  fcFileConverter   := nil;
end;

destructor TJcfIdeMain.Destroy;
begin
  FreeAndNil(fcEditorConverter);
  FreeAndNil(fcFileConverter);
  inherited;
end;

procedure TJcfIdeMain.DoFormatCurrentIDEWindow(Sender: TObject);
var
  hRes:      HResult;
  lciEditManager: IOTAEditorServices;
  lciEditor: IOTASourceEditor;
begin
  // get the current editor window
  hRes := BorlandIDEServices.QueryInterface(IOTAEditorServices, lciEditManager);
  if hRes <> S_OK then
    exit;
  if lciEditManager = nil then
    exit;

  lciEditor := lciEditManager.TopBuffer;
  if (lciEditor = nil) or (lciEditor.EditViewCount = 0) then
  begin
    LogIdeMessage('', 'No current window', mtInputError, -1, -1);
    exit;
  end;

  ConvertEditor(lciEditor);
end;

procedure TJcfIdeMain.ConvertEditor(const pciEditor: IOTASourceEditor);
begin
  MakeEditorConverter;

  ClearToolMessages;
  fcEditorConverter.Clear;
  fcEditorConverter.BeforeConvert;
  fcEditorConverter.Convert(pciEditor);
  fcEditorConverter.AfterConvert;
end;

procedure TJcfIdeMain.DoFormatProject(Sender: TObject);
var
  lciProject: IOTAProject;
  lciModule:  IOTAModuleInfo;
  {$IFDEF VER170}
  lciAction: IOTAActionServices;
  {$ENDIF}
  liLoop:     integer;
  lsMsg:      string;
begin
  {$IFDEF VER170}
  lciAction := BorlandIDEServices as IOTAActionServices;
  {$ENDIF}
  lciProject := GetCurrentProject;
  if lciProject = nil then
    exit;

  lsMsg := 'JEDI Code Format of ' + lciProject.FileName + NativeLineBreak +
    'Are you sure that you want to format all ' + IntToStr(lciProject.GetModuleCount) +
    ' files in the project.';

  if MessageDlg(lsMsg, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    exit;

  ClearToolMessages;

  { loop through all modules in the project }
  for liLoop := 0 to lciProject.GetModuleCount - 1 do
  begin
    lciModule := lciProject.GetModule(liLoop);
    FormatFile(lciModule.FileName);
    {$IFDEF VER170}
    lciAction.ReloadFile(lciModule.FileName);
    {$ENDIF}
  end;
end;

procedure TJcfIdeMain.DoFormatOpen(Sender: TObject);
var
  hRes:      HResult;
  lciEditManager: IOTAEditorServices;
  lciIterateBuffers: IOTAEditBufferIterator;
  lciEditor: IOTASourceEditor;
  liLoop:    integer;
begin
  hRes := BorlandIDEServices.QueryInterface(IOTAEditorServices, lciEditManager);
  if hRes <> S_OK then
    exit;
  if lciEditManager = nil then
    exit;

  MakeEditorConverter;

  lciIterateBuffers := nil;
  lciEditManager.GetEditBufferIterator(lciIterateBuffers);
  if lciIterateBuffers = nil then
    exit;

  ClearToolMessages;
  fcEditorConverter.BeforeConvert;

  for liLoop := 0 to lciIterateBuffers.Count - 1 do
  begin
    lciEditor := lciIterateBuffers.EditBuffers[liLoop];

    // check that it's open, and a .pas or .dpr
    if (lciEditor <> nil) and (lciEditor.EditViewCount > 0) and
      (FileIsAllowedType(lciEditor.FileName)) then
    begin
      fcEditorConverter.Convert(lciEditor);
    end;
  end;

  fcEditorConverter.AfterConvert;
end;


procedure TJcfIdeMain.FormatFile(const psFileName: string);
begin
  if not FileExists(psFileName) then
    exit;

  // check that it's a .pas or .dpr
  if not FileIsAllowedType(psFileName) then
    exit;

  if fcFileConverter = nil then
  begin
    fcFileConverter := TFileConverter.Create;
    fcFileConverter.OnStatusMessage := LogIDEMessage;
  end;

  fcFileConverter.ProcessFile(psFileName);
end;

procedure TJcfIdeMain.DoFormatSettings(Sender: TObject);
var
  lfAllSettings: TFormAllSettings;
begin
  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  lfAllSettings := TFormAllSettings.Create(nil);
  try
    lfAllSettings.Execute;
  finally
    lfAllSettings.Release;
  end;
end;

procedure TJcfIdeMain.DoAbout(Sender: TObject);
var
  lcAbout: TfrmAboutBox;
begin
  lcAbout := TfrmAboutBox.Create(nil);
  try
    lcAbout.ShowModal;
  finally
    lcAbout.Free;
  end;
end;

procedure TJcfIdeMain.DoRegistrySettings(Sender: TObject);
var
  lcAbout: TfmRegistrySettings;
begin
  if not GetRegSettings.HasRead then
    GetRegSettings.ReadAll;

  lcAbout := TfmRegistrySettings.Create(nil);
  try
    lcAbout.Execute;
  finally
    lcAbout.Free;
  end;
end;

procedure TJcfIdeMain.ShortcutKeyCallback(const Context: IOTAKeyContext;
  KeyCode: TShortcut; var BindingResult: TKeyBindingResult);
var
  liShortcut: TShortCut;
begin
  liShortcut := Shortcut(Ord('K'), [ssCtrl]);

  if KeyCode = liShortcut then
    DoFormatCurrentIDEWindow(nil);
end;

procedure TJcfIdeMain.LogIDEMessage(const psFile, psMessage: string;
  const peMessageType: TStatusMessageType;
  const piY, piX: integer);
var
  lciMessages: IOTAMessageServices40;
  hRes: HResult;
begin
  { no empty lines in this log }
  if psMessage = '' then
    exit;

  hRes := BorlandIDEServices.QueryInterface(IOTAMessageServices40, lciMessages);
  if hRes <> S_OK then
    exit;
  if lciMessages = nil then
    exit;

  if (piY >= 0) and (piX >= 0) then
    lciMessages.AddToolMessage(psFile, psMessage, 'JCF', piY, piX)
  else
    lciMessages.AddTitleMessage('JCF: ' + psFile + ' ' + psMessage);

end;

procedure TJcfIdeMain.MakeEditorConverter;
begin
  if fcEditorConverter = nil then
  begin
    fcEditorConverter := TEditorConverter.Create;
    fcEditorConverter.OnStatusMessage := LogIDEMessage;
  end;

  Assert(fcEditorConverter <> nil);
end;

procedure TJcfIdeMain.ClearToolMessages;
var
  lciMessages: IOTAMessageServices40;
  hRes: HResult;
begin
  hRes := BorlandIDEServices.QueryInterface(IOTAMessageServices40, lciMessages);
  if hRes <> S_OK then
    exit;
  if lciMessages = nil then
    exit;

  lciMessages.ClearToolMessages;
end;

end.
