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
  Classes, SysUtils, Forms;

const
  NoParent = -1;

type
  // forward
  TAbstractOptionsEditorDialog = class;

  // types
  TAbstractIDEOptions = class(TPersistent)
  public
    class function GetGroupCaption:string; virtual; abstract;
  end;
  TAbstractIDEOptionsClass = class of TAbstractIDEOptions;

  TOnLoadIDEOptions = procedure(Sender: TObject; AOptions: TAbstractIDEOptions) of object;
  TOnSaveIDEOptions = procedure(Sender: TObject; AOptions: TAbstractIDEOptions) of object;

  { TAbstractIDEOptionsEditor }

  TAbstractIDEOptionsEditor = class(TFrame)
  private
    FOnLoadIDEOptions: TOnLoadIDEOptions;
    FOnSaveIDEOptions: TOnSaveIDEOptions;
  public
    function Check: Boolean; virtual;
    function GetTitle: String; virtual; abstract;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); virtual; abstract;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); virtual; abstract;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); virtual; abstract;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; virtual; abstract;

    property OnLoadIDEOptions: TOnLoadIDEOptions read FOnLoadIDEOptions write FOnLoadIDEOptions;
    property OnSaveIDEOptions: TOnSaveIDEOptions read FOnSaveIDEOptions write FOnSaveIDEOptions;
  end;
  TAbstractIDEOptionsEditorClass = class of TAbstractIDEOptionsEditor;

  TIDEOptionsEditorRec = record
    Index: Integer;
    Parent: Integer;
    EditorClass: TAbstractIDEOptionsEditorClass;
  end;
  PIDEOptionsEditorRec = ^TIDEOptionsEditorRec;

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
    procedure Add(AEditorClass: TAbstractIDEOptionsEditorClass; AIndex, AParent: Integer); reintroduce;
    property Items[AIndex: Integer]: PIDEOptionsEditorRec read GetItem write SetItem; default;
  end;

  TIDEOptionsGroupRec = record
    Index: Integer;
    GroupClass: TAbstractIDEOptionsClass;
    Items: TIDEOptionsEditorList;
  end;
  PIDEOptionsGroupRec = ^TIDEOptionsGroupRec;

  { TIDEOptionsGroupList }

  TIDEOptionsGroupList = class(TList)
  private
    function GetItem(AIndex: Integer): PIDEOptionsGroupRec;
    procedure SetItem(AIndex: Integer; const AValue: PIDEOptionsGroupRec);
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;
    function GetByIndex(AIndex: Integer): PIDEOptionsGroupRec;
  public
    procedure Resort;
    procedure Add(AGroupIndex: Integer; AGroupClass: TAbstractIDEOptionsClass); reintroduce;
    property Items[AIndex: Integer]: PIDEOptionsGroupRec read GetItem write SetItem; default;
  end;

  TAbstractOptionsEditorDialog = class(TForm)
  public
    procedure OpenEditor(AEditor: TAbstractIDEOptionsEditorClass); virtual; abstract;
    function FindEditor(AEditor: TAbstractIDEOptionsEditorClass): TAbstractIDEOptionsEditor; virtual; abstract;
  end;

procedure RegisterIDEOptionsGroup(AGroupIndex: Integer; AGroupClass: TAbstractIDEOptionsClass);
procedure RegisterIDEOptionsEditor(AGroupIndex: Integer; AEditorClass: TAbstractIDEOptionsEditorClass; AIndex: Integer; AParent: Integer = NoParent);

function IDEEditorGroups: TIDEOptionsGroupList;

const
  // options groups
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
    EdtOptionsKeys        = 300;
    EdtOptionsMouse       = 400;
    EdtOptionsMouseAdv    = 401;
    EdtOptionsColors      = 500;
    EdtOptionsCodetools   = 600;
    EdtOptionsCodeFolding = 700;
      EdtOptionsCodeFoldingMouse = 701;
    EdtOptionsDrawDivider = 800;

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

implementation

var
  FIDEEditorGroups: TIDEOptionsGroupList;

function IDEEditorGroups: TIDEOptionsGroupList;
begin
  if FIDEEditorGroups = nil then
    FIDEEditorGroups := TIDEOptionsGroupList.Create;
  Result := FIDEEditorGroups;
end;

procedure RegisterIDEOptionsGroup(AGroupIndex: Integer; AGroupClass:TAbstractIDEOptionsClass);
begin
  IDEEditorGroups.Add(AGroupIndex, AGroupClass);
end;

procedure RegisterIDEOptionsEditor(AGroupIndex: Integer; AEditorClass: TAbstractIDEOptionsEditorClass; AIndex: Integer; AParent: Integer = NoParent);
var
  Rec: PIDEOptionsGroupRec;
begin
  Rec := IDEEditorGroups.GetByIndex(AGroupIndex);
  if Rec = nil then
  begin
    RegisterIDEOptionsGroup(AGroupIndex, nil);
    Rec := IDEEditorGroups.GetByIndex(AGroupIndex);
  end;

  if Rec <> nil then
  begin
    if Rec^.Items = nil then
      Rec^.Items := TIDEOptionsEditorList.Create;
    Rec^.Items.Add(AEditorClass, AIndex, AParent);
  end;
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

function TAbstractIDEOptionsEditor.Check: Boolean;
begin
  Result := True;
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

procedure TIDEOptionsEditorList.Add(AEditorClass: TAbstractIDEOptionsEditorClass; AIndex, AParent: Integer);
var
  Rec: PIDEOptionsEditorRec;
begin
  Rec := GetByIndex(AIndex);
  if Rec = nil then
  begin
    New(Rec);
    Rec^.Index := AIndex;
    Rec^.Parent := AParent;
    inherited Add(Rec);
  end;

  Rec^.EditorClass := AEditorClass;
end;

{ TIDEOptionsGroupList }

function TIDEOptionsGroupList.GetItem(AIndex: Integer): PIDEOptionsGroupRec;
begin
  Result := PIDEOptionsGroupRec(inherited Get(AIndex));
end;

procedure TIDEOptionsGroupList.SetItem(AIndex: Integer;
  const AValue: PIDEOptionsGroupRec);
begin
  inherited Put(AIndex, AValue);
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

procedure TIDEOptionsGroupList.Resort;
var
  i: integer;
begin
  Sort(@GroupListCompare);
  for i := 0 to Count - 1 do
    if Items[i]^.Items <> nil then
      Items[i]^.Items.Resort;
end;

procedure TIDEOptionsGroupList.Add(AGroupIndex: Integer; AGroupClass: TAbstractIDEOptionsClass);
var
  Rec: PIDEOptionsGroupRec;
begin
  Rec := GetByIndex(AGroupIndex);
  if Rec = nil then
  begin
    New(Rec);
    Rec^.Index := AGroupIndex;
    Rec^.Items := nil;
    inherited Add(Rec);
  end;

  Rec^.GroupClass := AGroupClass;
end;

initialization
  FIDEEditorGroups := nil;

finalization
  FreeAndNil(FIDEEditorGroups);
end.
