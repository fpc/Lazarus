{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditPlugins.pas, released 2001-10-17.

Author of this file is Flávio Etrusco.
Portions created by Flávio Etrusco are Copyright 2001 Flávio Etrusco.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditPlugins;

{$I synedit.inc}

interface

uses
  Classes, Menus, LCLType, SysUtils,
  SynEdit, SynEditKeyCmds, SynEditTypes, SynEditStrConst;

type

  { TLazSynMultiEditPlugin }

  TLazSynMultiEditPlugin = class(TLazSynEditPlugin)
  private
    FEditors: TList;
    FEditorWasAdded: Boolean;
    function GetEditorCount: integer;
    function GetEditors(aIndex: integer): TCustomSynEdit;
  protected
    function  IndexOfEditor(const AValue: TCustomSynEdit): integer;
    procedure BeforeEditorChange; override;
    procedure AfterEditorChange; override;
    function  DoRemoveEditor(AValue: TCustomSynEdit): integer;
    procedure DoEditorDestroyed(const AValue: TCustomSynEdit); override;
  public
    destructor Destroy; override;
    (* Add/RemoveEditor versus Editor
       Unless stated otherwise Plugins inherting from TLazSynMultiEditPlugin can
       either use Add/RemoveEditor or Editor (single-editor-property).
       If Editors are added via AddEditor, then an Editor only set via "Editor:="
       may be lost/ignored.
       If using AddEditor, the "Editor" property may be used to set/read the
       current Editor (out of those in the list). This does however depend on the
       inherited class.
    *)
    function  AddEditor(AValue: TCustomSynEdit): integer;
    function  RemoveEditor(AValue: TCustomSynEdit): integer;
    property Editors[aIndex: integer]: TCustomSynEdit read GetEditors;
    property EditorCount: integer read GetEditorCount;
  end;

  TAbstractSynPlugin = TLazSynMultiEditPlugin;

  TAbstractSynHookerPlugin = class(TAbstractSynPlugin)
  protected
    procedure HookEditor(aEditor: TCustomSynEdit; aCommandID: TSynEditorCommand;
      aOldShortCut, aNewShortCut: TShortCut; AFlags: THookedCommandFlags = [hcfPreExec, hcfPostExec]);
    procedure UnHookEditor(aEditor: TCustomSynEdit;
      aCommandID: TSynEditorCommand; aShortCut: TShortCut);
    procedure OnCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand;
      var aChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
      Data: pointer; HandlerData: pointer); virtual; abstract;
  end;

  TPluginState = (psNone, psExecuting, psAccepting, psCancelling);

  TAbstractSynSingleHookPlugin = class(TAbstractSynHookerPlugin)
  private
    fCommandID: TSynEditorCommand;
    function IsShortCutStored: Boolean;
    procedure SetShortCut(const Value: TShortCut);
  protected
    fState: TPluginState;
    fCurrentEditor: TCustomSynEdit;
    fShortCut: TShortCut;
    class function DefaultShortCut: TShortCut; virtual;
    procedure DoEditorAdded(aEditor: TCustomSynEdit); override;
    procedure DoEditorRemoving(aEditor: TCustomSynEdit); override;
    {}
    procedure DoExecute; virtual; abstract;
    procedure DoAccept; virtual; abstract;
    procedure DoCancel; virtual; abstract;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    property CommandID: TSynEditorCommand read fCommandID;
    {}
    property CurrentEditor: TCustomSynEdit read fCurrentEditor;
    function Executing: boolean;
    procedure Execute(aEditor: TCustomSynEdit);
    procedure Accept;
    procedure Cancel;
  published
    property ShortCut: TShortCut read fShortCut write SetShortCut
      stored IsShortCutStored;
  end deprecated;

  { use TAbstractSynCompletion for non-visual completion }

  TAbstractSynCompletion = class(TAbstractSynSingleHookPlugin)
  protected
    fCurrentString: String;
  protected
    procedure SetCurrentString(const Value: String); virtual;
    procedure OnCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand;
      var aChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
      Data: pointer; HandlerData: pointer); override;
    procedure DoExecute; override;
    procedure DoAccept; override;
    procedure DoCancel; override;
    function GetCurrentEditorString: String; virtual;
  public
    procedure AddEditor(aEditor: TCustomSynEdit);
    property CurrentString: String read fCurrentString write SetCurrentString;
  end deprecated;

function NewPluginCommand: TSynEditorCommand; deprecated; // Use AllocatePluginKeyRange
procedure ReleasePluginCommand(aCmd: TSynEditorCommand); deprecated;

implementation

function NewPluginCommand: TSynEditorCommand;
begin
  Result := AllocatePluginKeyRange(1);
end;

procedure ReleasePluginCommand(aCmd: TSynEditorCommand);
begin
end;

{ TLazSynMultiEditPlugin }

function TLazSynMultiEditPlugin.GetEditorCount: integer;
begin
  if FEditors = nil then
    Result := 0
  else
    Result := FEditors.Count;
end;

function TLazSynMultiEditPlugin.GetEditors(aIndex: integer): TCustomSynEdit;
begin
  if FEditors = nil then
    Result := nil
  else
    Result := TCustomSynEdit(FEditors[aIndex]);
end;

function TLazSynMultiEditPlugin.IndexOfEditor(const AValue: TCustomSynEdit): integer;
begin
  if FEditors = nil then
    Result := -1
  else
    Result := FEditors.IndexOf(AValue);
end;

procedure TLazSynMultiEditPlugin.BeforeEditorChange;
begin
  if (Editor = nil) or FEditorWasAdded then
    exit;

  // Current Editor was not explicitly added by user via "AddEditor"
  DoRemoveEditor(Editor);
end;

procedure TLazSynMultiEditPlugin.AfterEditorChange;
begin
  if (Editor = nil) then
    exit;
  FEditorWasAdded := IndexOfEditor(Editor) >= 0;
  if FEditorWasAdded  then
    exit;

  // Current Editor was not explicitly added by user via "AddEditor"
  // Temporary add it
  AddEditor(Editor);
  FEditorWasAdded := False; // Reset Flag, after AddEditor
end;

function TLazSynMultiEditPlugin.DoRemoveEditor(AValue: TCustomSynEdit): integer;
begin
  if IndexOfEditor(AValue) < 0 then begin
    Result := -1;
    Exit;
  end;

  DoEditorRemoving(AValue);
  UnRegisterFromEditor(AValue);
  Result := FEditors.Remove(AValue);
end;

procedure TLazSynMultiEditPlugin.DoEditorDestroyed(const AValue: TCustomSynEdit);
begin
  RemoveEditor(AValue);
  if EditorCount = 0 then
    inherited DoEditorDestroyed(nil); // Editor is nil now, so pass nil as param too
end;

function TLazSynMultiEditPlugin.AddEditor(AValue: TCustomSynEdit): integer;
begin
  if AValue = Editor then
    FEditorWasAdded := True;

  if IndexOfEditor(AValue) >= 0 then begin
    Result := -1;
    Exit;
  end;

  if FEditors = nil then
    FEditors := TList.Create;
  Result := FEditors.Add(AValue);
  RegisterToEditor(AValue);
  DoEditorAdded(AValue);
end;

function TLazSynMultiEditPlugin.RemoveEditor(AValue: TCustomSynEdit): integer;
begin
  if AValue = Editor then
    Editor := nil;

  Result := DoRemoveEditor(AValue);
end;

destructor TLazSynMultiEditPlugin.Destroy;
begin
  while EditorCount > 0 do
    RemoveEditor(Editors[0]);
  FreeAndNil(FEditors);
  inherited Destroy;
end;


{ TAbstractSynHookerPlugin }

procedure TAbstractSynHookerPlugin.HookEditor(aEditor: TCustomSynEdit;
  aCommandID: TSynEditorCommand; aOldShortCut, aNewShortCut: TShortCut;
  AFlags: THookedCommandFlags = [hcfPreExec, hcfPostExec]);
var
  iIndex: integer;
  iKeystroke: TSynEditKeyStroke;
begin
  Assert( aNewShortCut <> 0 );
  { shortcurts aren't created while in design-time }
  if [csDesigning] * ComponentState = [csDesigning] then
  begin
    if TCustomSynEdit(aEditor).Keystrokes.FindShortcut( aNewShortCut ) >= 0 then
      raise ESynKeyError.Create(SYNS_EDuplicateShortCut)
    else
      Exit;
  end;
  { tries to update old Keystroke }
  if aOldShortCut <> 0 then
  begin
    iIndex := TCustomSynEdit(aEditor).Keystrokes.FindShortcut( aOldShortCut );
    if (iIndex >= 0) then
    begin
      iKeystroke := TCustomSynEdit(aEditor).Keystrokes[iIndex];
      if iKeystroke.Command = aCommandID then
      begin
        iKeystroke.ShortCut := aNewShortCut;
        Exit;
      end;
    end;
  end;
  { new Keystroke }
  iKeystroke := TCustomSynEdit(aEditor).Keystrokes.Add;
  try
    iKeystroke.ShortCut := aNewShortCut;
  except
    iKeystroke.Free;
    raise;
  end;
  iKeystroke.Command := aCommandID;

  if AFlags <> [] then
    aEditor.RegisterCommandHandler( {$IFDEF FPC}@{$ENDIF}OnCommand, Self, AFlags);
end;

procedure TAbstractSynHookerPlugin.UnHookEditor(aEditor: TCustomSynEdit;
  aCommandID: TSynEditorCommand; aShortCut: TShortCut);
var
  iIndex: integer;
begin
  aEditor.UnregisterCommandHandler( {$IFDEF FPC}@{$ENDIF}OnCommand );
  iIndex := TCustomSynEdit(aEditor).Keystrokes.FindShortcut( aShortCut );
  if (iIndex >= 0) and
    (TCustomSynEdit(aEditor).Keystrokes[iIndex].Command = aCommandID) then
    TCustomSynEdit(aEditor).Keystrokes[iIndex].Free;
end;

{ TAbstractSynHookerPlugin }

procedure TAbstractSynSingleHookPlugin.Accept;
begin
  fState := psAccepting;
  try
    DoAccept;
  finally
    fCurrentEditor := nil;
    fState := psNone;
  end;
end;

procedure TAbstractSynSingleHookPlugin.Cancel;
begin
  fState := psCancelling;
  try
    DoCancel;
  finally
    fCurrentEditor := nil;
    fState := psNone;
  end;
end;

constructor TAbstractSynSingleHookPlugin.Create(aOwner: TComponent);
begin
  inherited;
  // TODO: subclasses should implement per class, not per instance
  fCommandID := AllocatePluginKeyRange(1);
  fShortCut := DefaultShortCut;
end;

class function TAbstractSynSingleHookPlugin.DefaultShortCut: TShortCut;
begin
  Result := 0;
end;

destructor TAbstractSynSingleHookPlugin.Destroy;
begin
  if Executing then
    Cancel;
  //ReleasePluginCommand( CommandID );
  inherited;
end;

procedure TAbstractSynSingleHookPlugin.DoEditorAdded(
  aEditor: TCustomSynEdit);
begin
  if ShortCut <> 0 then
    HookEditor( aEditor, CommandID, 0, ShortCut );
end;

procedure TAbstractSynSingleHookPlugin.Execute(aEditor: TCustomSynEdit);
begin
  if Executing then
    Cancel;
  Assert( fCurrentEditor = nil );
  fCurrentEditor := aEditor;
  Assert( fState = psNone );
  fState := psExecuting;
  try
    DoExecute;
  except
    Cancel;
    raise;
  end;
end;

function TAbstractSynSingleHookPlugin.Executing: boolean;
begin
  Result := fState = psExecuting;
end;

function TAbstractSynSingleHookPlugin.IsShortCutStored: Boolean;
begin
  Result := fShortCut <> DefaultShortCut;
end;

procedure TAbstractSynSingleHookPlugin.DoEditorRemoving(aEditor: TCustomSynEdit);
begin
  if ShortCut <> 0 then
    UnHookEditor( aEditor, CommandID, ShortCut );
  if Executing and (CurrentEditor = aEditor) then
    Cancel;
end;

procedure TAbstractSynSingleHookPlugin.SetShortCut(const Value: TShortCut);
var
  cEditor: integer;
begin
  if fShortCut <> Value then
  begin
    if Assigned(fEditors) then
      if Value <> 0 then
      begin
        for cEditor := 0 to fEditors.Count -1 do
          HookEditor( Editors[cEditor], CommandID, fShortCut, Value );
      end
      else
      begin
        for cEditor := 0 to fEditors.Count -1 do
          UnHookEditor( Editors[cEditor], CommandID, fShortCut );
      end;
    fShortCut := Value;
  end;
end;

{ TAbstractSynCompletion }

function TAbstractSynCompletion.GetCurrentEditorString: String;
var
  iString: String;
  cCol: integer;
  iIdentChars: TSynIdentChars;
begin
  iString := CurrentEditor.LineText;
  if (CurrentEditor.CaretX > 1) and
    (CurrentEditor.CaretX -1 <= Length(iString)) then
  begin
    iIdentChars := CurrentEditor.IdentChars;
    for cCol := CurrentEditor.CaretX -1 downto 1 do
      if not (iString[cCol] in iIdentChars) then
        break;
    Result := Copy( iString, cCol +1, CurrentEditor.CaretX - cCol -1);
  end;
end;

procedure TAbstractSynCompletion.DoAccept;
begin
  fCurrentString := '';
end;

procedure TAbstractSynCompletion.DoCancel;
begin
  fCurrentString := '';
end;

procedure TAbstractSynCompletion.DoExecute;
begin
  CurrentString := GetCurrentEditorString;
end;

procedure TAbstractSynCompletion.OnCommand(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean;
  var Command: TSynEditorCommand;
  var aChar: {$IFDEF SYN_LAZARUS}TUTF8Char{$ELSE}Char{$ENDIF};
  Data, HandlerData: pointer);
var
  iString: String;
begin
  if not Executing then
  begin
    if (Command = CommandID) then
    begin
      Execute( Sender as TCustomSynEdit );
      Handled := True;
    end;
  end
  else { Executing }
    if Sender = CurrentEditor then
    begin
      if not AfterProcessing then
      begin
          case Command of
            ecChar:
              if aChar = #27 then
              begin
                Cancel;
                Handled := True;
              end
              else
              begin
                {$IFDEF SYN_LAZARUS}
                if (length(aChar)<>1)
                or (not (aChar[1] in CurrentEditor.IdentChars)) then
                  Accept;
                {$ELSE}
                if not (aChar in CurrentEditor.IdentChars) then
                  Accept;
                {don't handle the char}
                {$ENDIF}
              end;
            ecLineBreak:
            begin
              Accept;
              Handled := True;
            end;
            ecLeft, ecSelLeft, ecColSelLeft:
              if CurrentString = '' then
                Handled := True;
            ecDeleteLastChar:
              if CurrentString = '' then
                Handled := True;
            ecTab:
              Accept;
            ecDeleteChar,
            ecRight, ecSelRight, ecColSelRight,
            ecLostFocus, ecGotFocus:
              ; {processed on AfterProcessing}
            else
              Cancel;
          end;
      end
      else { AfterProcessing }
        case Command of
          ecLostFocus, ecGotFocus,
          ecDeleteChar:
            ;
          ecDeleteLastChar,
          ecLeft, ecSelLeft, ecColSelLeft,
          ecChar:
            CurrentString := GetCurrentEditorString;
          ecRight, ecSelRight, ecColSelRight: begin
            iString := GetCurrentEditorString;
            if iString = '' then
              Cancel
            else
              CurrentString := iString;
          end;
          else
            if CurrentString <> GetCurrentEditorString then
              Cancel;
        end;
    end; {endif Sender = CurrentEditor}
end;

procedure TAbstractSynCompletion.SetCurrentString(const Value: String);
begin
  fCurrentString := Value;
end;

procedure TAbstractSynCompletion.AddEditor(aEditor: TCustomSynEdit);
begin
  inherited AddEditor(aEditor);
end;

end.
