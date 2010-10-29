{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit IDEOptionsIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Controls, Buttons, Forms;

const
  NoParent = -1;

type
  // forward
  TAbstractOptionsEditorDialog = class;

  // types

  TIDEOptionsHandler = (
    iohBeforeRead,
    iohAfterRead,
    iohBeforeWrite,
    iohAfterWrite
    );
  TIDEOptionsHandlers = set of TIDEOptionsHandler;

  TIDEOptionsWriteEvent = procedure(Sender: TObject; Restore: boolean) of object;

  { TAbstractIDEOptions }

  TAbstractIDEOptions = class(TPersistent)
  private
    fHandlers: array[TIDEOptionsHandler] of TMethodList;
    FOnAfterRead: TNotifyEvent;
    FOnAfterWrite: TIDEOptionsWriteEvent;
    FOnBeforeRead: TNotifyEvent;
    FOnBeforeWrite: TIDEOptionsWriteEvent;
  public
    constructor Create;
    destructor Destroy; override;

    class function GetGroupCaption: string; virtual; abstract;
    class function GetInstance: TAbstractIDEOptions; virtual; abstract;

    procedure DoBeforeRead; virtual;
    procedure DoAfterRead; virtual;
    procedure DoBeforeWrite(Restore: boolean); virtual;
    procedure DoAfterWrite(Restore: boolean); virtual;

    procedure AddHandlerBeforeRead(const Handler: TNotifyEvent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerBeforeRead(const Handler: TNotifyEvent);
    procedure AddHandlerAfterRead(const Handler: TNotifyEvent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerAfterRead(const Handler: TNotifyEvent);
    procedure AddHandlerBeforeWrite(const Handler: TIDEOptionsWriteEvent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerBeforeWrite(const Handler: TIDEOptionsWriteEvent);
    procedure AddHandlerAfterWrite(const Handler: TIDEOptionsWriteEvent; const AsFirst: boolean = true); // AsFirst means: first to call
    procedure RemoveHandlerAfterWrite(const Handler: TIDEOptionsWriteEvent);

    property OnBeforeRead: TNotifyEvent read FOnBeforeRead write FOnBeforeRead;
    property OnAfterRead: TNotifyEvent read FOnAfterRead write FOnAfterRead;
    property OnBeforeWrite: TIDEOptionsWriteEvent read FOnBeforeWrite write FOnBeforeWrite;
    property OnAfterWrite: TIDEOptionsWriteEvent read FOnAfterWrite write FOnAfterWrite;
  end;
  TAbstractIDEOptionsClass = class of TAbstractIDEOptions;

  TAbstractIDEEnvironmentOptions = class(TAbstractIDEOptions);
  TAbstractIDEProjectOptions = class(TAbstractIDEOptions);
  TAbstractIDEHelpOptions = class(TAbstractIDEEnvironmentOptions);

  TOnLoadIDEOptions = procedure(Sender: TObject; AOptions: TAbstractIDEOptions) of object;
  TOnSaveIDEOptions = procedure(Sender: TObject; AOptions: TAbstractIDEOptions) of object;

  { TAbstractIDEOptionsEditor }

  PIDEOptionsEditorRec = ^TIDEOptionsEditorRec;
  PIDEOptionsGroupRec = ^TIDEOptionsGroupRec;

  TAbstractIDEOptionsEditor = class(TFrame)
  private
    FOnChange: TNotifyEvent;
    FOnLoadIDEOptions: TOnLoadIDEOptions;
    FOnSaveIDEOptions: TOnSaveIDEOptions;
    FRec: PIDEOptionsEditorRec;
    FGroupRec: PIDEOptionsGroupRec;
  protected
    procedure DoOnChange;
  public
    function Check: Boolean; virtual;
    function GetTitle: String; virtual; abstract;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); virtual; abstract;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); virtual; abstract;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); virtual; abstract;
    procedure RestoreSettings(AOptions: TAbstractIDEOptions); virtual;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; virtual; abstract;
    class function DefaultCollapseChildNodes: Boolean; virtual;
    function FindOptionControl(AClass: TControlClass): TControl;

    property OnLoadIDEOptions: TOnLoadIDEOptions read FOnLoadIDEOptions write FOnLoadIDEOptions;
    property OnSaveIDEOptions: TOnSaveIDEOptions read FOnSaveIDEOptions write FOnSaveIDEOptions;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Rec: PIDEOptionsEditorRec read FRec write FRec;
    property GroupRec: PIDEOptionsGroupRec read FGroupRec write FGroupRec;
  end;
  TAbstractIDEOptionsEditorClass = class of TAbstractIDEOptionsEditor;

  TIDEOptionsEditorRec = record
    Index: Integer;
    Parent: Integer;
    EditorClass: TAbstractIDEOptionsEditorClass;
    Collapsed, DefaultCollapsed: Boolean;
  end;

  { TIDEOptionsEditorList }

  TIDEOptionsEditorList = class(TList)
  private
    function GetItem(AIndex: Integer): PIDEOptionsEditorRec;
    procedure SetItem(AIndex: Integer; const AValue: PIDEOptionsEditorRec);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetByIndex(AIndex: Integer): PIDEOptionsEditorRec;
  public
    procedure Resort;
    function Add(AEditorClass: TAbstractIDEOptionsEditorClass; AIndex, AParent: Integer): PIDEOptionsEditorRec; reintroduce;
    property Items[AIndex: Integer]: PIDEOptionsEditorRec read GetItem write SetItem; default;
  end;

  TIDEOptionsGroupRec = record
    Index: Integer;
    GroupClass: TAbstractIDEOptionsClass;
    Items: TIDEOptionsEditorList;
    Collapsed, DefaultCollapsed: Boolean;
  end;

  { TIDEOptionsGroupList }

  TIDEOptionsGroupList = class(TList)
  private
    FLastSelected: PIDEOptionsEditorRec;
    function GetItem(Position: Integer): PIDEOptionsGroupRec;
    procedure SetItem(Position: Integer; const AValue: PIDEOptionsGroupRec);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
  public
    procedure Resort;
    procedure DoAfterWrite(Restore: boolean);
    function GetByIndex(AIndex: Integer): PIDEOptionsGroupRec;
    function GetByGroupClass(AGroupClass: TAbstractIDEOptionsClass): PIDEOptionsGroupRec;
    function Add(AGroupIndex: Integer; AGroupClass: TAbstractIDEOptionsClass): PIDEOptionsGroupRec; reintroduce;
    property Items[Position: Integer]: PIDEOptionsGroupRec read GetItem write SetItem; default;
    property LastSelected: PIDEOptionsEditorRec read FLastSelected write FLastSelected;
  end;

  TAbstractOptionsEditorDialog = class(TForm)
  public
    function AddButton: TBitBtn; virtual; abstract;
    function AddControl(AControlClass: TControlClass): TControl; virtual; abstract; reintroduce;
    function FindEditor(AEditor: TAbstractIDEOptionsEditorClass): TAbstractIDEOptionsEditor; virtual; abstract;
    function FindEditor(GroupIndex, AIndex: integer): TAbstractIDEOptionsEditor; virtual; abstract;
    function FindEditorClass(GroupIndex, AIndex: integer): TAbstractIDEOptionsEditorClass; virtual; abstract;
    procedure OpenEditor(AEditor: TAbstractIDEOptionsEditorClass); virtual; abstract;
    procedure OpenEditor(GroupIndex, AIndex: integer); virtual; abstract;
  end;

function GetFreeIDEOptionsGroupIndex(AStartIndex: Integer): Integer;
function GetFreeIDEOptionsIndex(AGroupIndex: Integer; AStartIndex: Integer): Integer;
function RegisterIDEOptionsGroup(AGroupIndex: Integer;
           AGroupClass: TAbstractIDEOptionsClass;
           FindFreeIndex: boolean = true): PIDEOptionsGroupRec;
function RegisterIDEOptionsEditor(AGroupIndex: Integer;
           AEditorClass: TAbstractIDEOptionsEditorClass;
           AIndex: Integer; AParent: Integer = NoParent;
           AutoCreateGroup: boolean = false): PIDEOptionsEditorRec;

function IDEEditorGroups: TIDEOptionsGroupList;

const
  // predefined environment options groups
  GroupEnvironment  = 100;
    EnvOptionsFiles   = 100;
    EnvOptionsDesktop = 200;
    EnvOptionsWindow  = 300;
    EnvOptionsFormEd  = 400;
    EnvOptionsOI      = 500;
    EnvOptionsBackup  = 600;
    EnvOptionsNaming  = 700;
    EnvOptionsFpDoc   = 800;

  GroupEditor       = 200;
    EdtOptionsGeneral     = 100;
      EdtOptionsGeneralMisc = 101;
    EdtOptionsDisplay     = 200;
      EdtOptionsColors    = 500;
      EdtOptionsMarkup    = 502;
    EdtOptionsKeys        = 300;
    EdtOptionsMouse       = 400;
    EdtOptionsMouseAdv    = 401;
    EdtOptionsCodetools   = 600;
    EdtOptionsCodeFolding = 700;
      EdtOptionsCodeFoldingMouse = 701;
    EdtOptionsDrawDivider = 800;
    EdtOptionsMultiWindow = 900;

  GroupCodetools    = 300;
    CdtOptionsGeneral         = 100;
    CdtOptionsClassCompletion = 200;
    CdtOptionsCodeCreation    = 300;
    CdtOptionsWords           = 400;
    CdtOptionsLineSplitting   = 500;
    CdtOptionsSpace           = 600;
    CdtOptionsIdentCompletion = 700;

  GroupCodeExplorer = 350;
    cdeOptionsUpdate     = 100;
    cdeOptionsCategories = 200;
    cdeOptionsFigures    = 300;

  GroupDebugger     = 400;
    DbgOptionsGeneral            = 100;
    DbgOptionsEventLog           = 200;
    DbgOptionsLanguageExceptions = 300;
    DbgOptionsSignals            = 400;

  GroupHelp         = 500;
    HlpOptionsGeneral = 100;

  // predefined project options groups
  GroupProject      = 100100;
    ProjectOptionsApplication = 100;
    ProjectOptionsForms       = 200;
    ProjectOptionsLazDoc      = 300;
    ProjectOptionsSave        = 400;
    ProjectOptionsVersionInfo = 500;
    ProjectOptionsI18N        = 600;
    ProjectOptionsMisc        = 700;

  GroupCompiler     = 100200;
    CompilerOptionsSearchPaths    = 0100;
    CompilerOptionsBuildModes     = 0200;
    CompilerOptionsParsing        = 0300;
    CompilerOptionsCodeGeneration = 0400;
    CompilerOptionsLinking        = 0500;
    CompilerOptionsVerbosity      = 0600;
    CompilerOptionsMessages       = 0700;
    CompilerOptionsOther          = 0800;
    CompilerOptionsConditional    = 0900;
    CompilerOptionsInherited      = 1000;
    CompilerOptionsCompilation    = 1100;

implementation

var
  FIDEEditorGroups: TIDEOptionsGroupList;

function IDEEditorGroups: TIDEOptionsGroupList;
begin
  if FIDEEditorGroups = nil then
    FIDEEditorGroups := TIDEOptionsGroupList.Create;
  Result := FIDEEditorGroups;
end;

function RegisterIDEOptionsGroup(AGroupIndex: Integer;
  AGroupClass: TAbstractIDEOptionsClass; FindFreeIndex: boolean): PIDEOptionsGroupRec;
begin
  if FindFreeIndex then
    AGroupIndex:=GetFreeIDEOptionsGroupIndex(AGroupIndex);
  Result:=IDEEditorGroups.Add(AGroupIndex, AGroupClass);
end;

function RegisterIDEOptionsEditor(AGroupIndex: Integer;
  AEditorClass: TAbstractIDEOptionsEditorClass; AIndex: Integer;
  AParent: Integer; AutoCreateGroup: boolean): PIDEOptionsEditorRec;
var
  Rec: PIDEOptionsGroupRec;
begin
  Rec := IDEEditorGroups.GetByIndex(AGroupIndex);
  if Rec = nil then
  begin
    if not AutoCreateGroup then
      raise Exception.Create('RegisterIDEOptionsEditor: missing Group');
    Rec := RegisterIDEOptionsGroup(AGroupIndex, nil);
  end;

  if Rec^.Items = nil then
    Rec^.Items := TIDEOptionsEditorList.Create;
  Result:=Rec^.Items.Add(AEditorClass, AIndex, AParent);
end;

function GetFreeIDEOptionsGroupIndex(AStartIndex: Integer): Integer;
var
  I: Integer;
begin
  for I := AStartIndex to High(Integer) do
    if IDEEditorGroups.GetByIndex(I) = nil then
      Exit(I);
end;

function GetFreeIDEOptionsIndex(AGroupIndex: Integer; AStartIndex: Integer): Integer;
var
  Rec: PIDEOptionsGroupRec;
  I: Integer;
begin
  Result := -1; // -1 = error
  Rec := IDEEditorGroups.GetByIndex(AGroupIndex);
  if Rec = nil then
    Exit;

  for I := AStartIndex to High(Integer) do
    if Rec^.Items.GetByIndex(I) = nil then
      Exit(I);
end;

function GroupListCompare(Item1, Item2: Pointer): Integer;
var
  Rec1: PIDEOptionsGroupRec absolute Item1;
  Rec2: PIDEOptionsGroupRec absolute Item2;
begin
  if Rec1^.Index < Rec2^.Index then
    Result := -1
  else
  if Rec1^.Index > Rec2^.Index then
    Result := 1
  else
    Result := 0;
end;

function OptionsListCompare(Item1, Item2: Pointer): Integer;
var
  Rec1: PIDEOptionsEditorRec absolute Item1;
  Rec2: PIDEOptionsEditorRec absolute Item2;
begin
  if Rec1^.Index < Rec2^.Index then
    Result := -1
  else
  if Rec1^.Index > Rec2^.Index then
    Result := 1
  else
    Result := 0;
end;

{ TAbstractIDEOptionsEditor }

procedure TAbstractIDEOptionsEditor.DoOnChange;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

function TAbstractIDEOptionsEditor.Check: Boolean;
begin
  Result := True;
end;

procedure TAbstractIDEOptionsEditor.RestoreSettings(
  AOptions: TAbstractIDEOptions);
begin

end;

class function TAbstractIDEOptionsEditor.DefaultCollapseChildNodes: Boolean;
begin
  Result := False;
end;

function TAbstractIDEOptionsEditor.FindOptionControl(AClass: TControlClass
  ): TControl;

  function Search(AControl: TControl): TControl;
  var
    i: Integer;
    AWinControl: TWinControl;
  begin
    if AControl is AClass then
      exit(AControl);
    if AControl is TWinControl then begin
      AWinControl:=TWinControl(AControl);
      for i:=0 to AWinControl.ControlCount-1 do begin
        Result:=Search(AWinControl.Controls[i]);
        if Result<>nil then exit;
      end;
    end;
    Result:=nil;
  end;

begin
  Result:=Search(GetParentForm(Self));
end;

{ TIDEOptionsEditorList }

function TIDEOptionsEditorList.GetItem(AIndex: Integer): PIDEOptionsEditorRec;
begin
  Result := PIDEOptionsEditorRec(inherited Get(AIndex));
end;

procedure TIDEOptionsEditorList.SetItem(AIndex: Integer; const AValue: PIDEOptionsEditorRec);
begin
  inherited Put(AIndex, AValue);
end;

procedure TIDEOptionsEditorList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
    Dispose(PIDEOptionsEditorRec(Ptr));
  inherited Notify(Ptr, Action);
end;

function TIDEOptionsEditorList.GetByIndex(AIndex: Integer): PIDEOptionsEditorRec;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i]^.Index = AIndex then
    begin
      Result := Items[i];
      break;
    end;
end;

procedure TIDEOptionsEditorList.Resort;
begin
  Sort(@OptionsListCompare);
end;

function TIDEOptionsEditorList.Add(AEditorClass: TAbstractIDEOptionsEditorClass;
  AIndex, AParent: Integer): PIDEOptionsEditorRec;
begin
  Result := GetByIndex(AIndex);
  if Result = nil then
  begin
    New(Result);
    Result^.Index := AIndex;
    Result^.Parent := AParent;
    Result^.Collapsed := AEditorClass.DefaultCollapseChildNodes;
    Result^.DefaultCollapsed := AEditorClass.DefaultCollapseChildNodes;
    inherited Add(Result);
  end;

  Result^.EditorClass := AEditorClass;
end;

{ TIDEOptionsGroupList }

function TIDEOptionsGroupList.GetItem(Position: Integer): PIDEOptionsGroupRec;
begin
  Result := PIDEOptionsGroupRec(inherited Get(Position));
end;

procedure TIDEOptionsGroupList.SetItem(Position: Integer;
  const AValue: PIDEOptionsGroupRec);
begin
  inherited Put(Position, AValue);
end;

procedure TIDEOptionsGroupList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  if Action = lnDeleted then
  begin
    PIDEOptionsGroupRec(Ptr)^.Items.Free;
    Dispose(PIDEOptionsGroupRec(Ptr));
  end;
  inherited Notify(Ptr, Action);
end;

function TIDEOptionsGroupList.GetByIndex(AIndex: Integer): PIDEOptionsGroupRec;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
    if Items[i]^.Index = AIndex then
    begin
      Result := Items[i];
      break;
    end;
end;

function TIDEOptionsGroupList.GetByGroupClass(
  AGroupClass: TAbstractIDEOptionsClass): PIDEOptionsGroupRec;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
  begin
    Result:=Items[i];
    if Result^.GroupClass=AGroupClass then exit;
  end;
  Result:=nil;
end;

procedure TIDEOptionsGroupList.Resort;
var
  i: integer;
begin
  Sort(@GroupListCompare);
  for i := 0 to Count - 1 do
    if Items[i]^.Items <> nil then
      Items[i]^.Items.Resort;
end;

procedure TIDEOptionsGroupList.DoAfterWrite(Restore: boolean);
var
  i: integer;
  Rec: PIDEOptionsGroupRec;
  Instance: TAbstractIDEOptions;
begin
  for i := 0 to Count - 1 do
  begin
    Rec := Items[i];
    if Rec^.Items <> nil then
    begin
      if Rec^.GroupClass <> nil then
      begin
        Instance := Rec^.GroupClass.GetInstance;
        if Instance <> nil then
          Instance.DoAfterWrite(Restore);
      end;
    end;
  end;
end;

function TIDEOptionsGroupList.Add(AGroupIndex: Integer;
  AGroupClass: TAbstractIDEOptionsClass): PIDEOptionsGroupRec;
begin
  Result := GetByIndex(AGroupIndex);
  if Result = nil then
  begin
    New(Result);
    Result^.Index := AGroupIndex;
    Result^.Items := nil;
    Result^.Collapsed := False;
    Result^.DefaultCollapsed := False;
    inherited Add(Result);
  end;

  Result^.GroupClass := AGroupClass;
end;

{ TAbstractIDEOptions }

constructor TAbstractIDEOptions.Create;
var
  h: TIDEOptionsHandler;
begin
  for h:=low(TIDEOptionsHandler) to high(TIDEOptionsHandler) do
    fHandlers[h]:=TMethodList.Create;
end;

destructor TAbstractIDEOptions.Destroy;
var
  h: TIDEOptionsHandler;
begin
  for h:=low(TIDEOptionsHandler) to high(TIDEOptionsHandler) do
    FreeAndNil(fHandlers[h]);
  inherited Destroy;
end;

procedure TAbstractIDEOptions.DoBeforeRead;
begin
  if Assigned(FOnBeforeRead) then
    FOnBeforeRead(Self);
  fHandlers[iohBeforeRead].CallNotifyEvents(Self);
end;

procedure TAbstractIDEOptions.DoAfterRead;
begin
  if Assigned(FOnAfterRead) then
    FOnAfterRead(Self);
  fHandlers[iohAfterRead].CallNotifyEvents(Self);
end;

procedure TAbstractIDEOptions.DoBeforeWrite(Restore: boolean);
var
  i: LongInt;
begin
  if Assigned(FOnBeforeWrite) then
    FOnBeforeWrite(Self,Restore);
  i:=fHandlers[iohBeforeWrite].Count;
  while fHandlers[iohBeforeWrite].NextDownIndex(i) do
    TIDEOptionsWriteEvent(fHandlers[iohBeforeWrite][i])(Self,Restore);
end;

procedure TAbstractIDEOptions.DoAfterWrite(Restore: boolean);
var
  i: LongInt;
begin
  if Assigned(FOnAfterWrite) then
    FOnAfterWrite(Self,Restore);
  i:=fHandlers[iohBeforeWrite].Count;
  while fHandlers[iohBeforeWrite].NextDownIndex(i) do
    TIDEOptionsWriteEvent(fHandlers[iohBeforeWrite][i])(Self,Restore);
end;

procedure TAbstractIDEOptions.AddHandlerBeforeRead(const Handler: TNotifyEvent;
  const AsFirst: boolean);
begin
  fHandlers[iohBeforeRead].Add(TMethod(Handler),AsFirst);
end;

procedure TAbstractIDEOptions.RemoveHandlerBeforeRead(
  const Handler: TNotifyEvent);
begin
  fHandlers[iohBeforeRead].Remove(TMethod(Handler));
end;

procedure TAbstractIDEOptions.AddHandlerAfterRead(const Handler: TNotifyEvent;
  const AsFirst: boolean);
begin
  fHandlers[iohAfterRead].Add(TMethod(Handler),AsFirst);
end;

procedure TAbstractIDEOptions.RemoveHandlerAfterRead(const Handler: TNotifyEvent
  );
begin
  fHandlers[iohAfterRead].Remove(TMethod(Handler));
end;

procedure TAbstractIDEOptions.AddHandlerBeforeWrite(
  const Handler: TIDEOptionsWriteEvent; const AsFirst: boolean);
begin
  fHandlers[iohBeforeWrite].Add(TMethod(Handler),AsFirst);
end;

procedure TAbstractIDEOptions.RemoveHandlerBeforeWrite(
  const Handler: TIDEOptionsWriteEvent);
begin
  fHandlers[iohBeforeWrite].Remove(TMethod(Handler));
end;

procedure TAbstractIDEOptions.AddHandlerAfterWrite(
  const Handler: TIDEOptionsWriteEvent; const AsFirst: boolean);
begin
  fHandlers[iohAfterWrite].Add(TMethod(Handler),AsFirst);
end;

procedure TAbstractIDEOptions.RemoveHandlerAfterWrite(
  const Handler: TIDEOptionsWriteEvent);
begin
  fHandlers[iohAfterWrite].Remove(TMethod(Handler));
end;

initialization
  FIDEEditorGroups := nil;

finalization
  FreeAndNil(FIDEEditorGroups);
end.
