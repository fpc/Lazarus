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

 Author: Mattias Gaertner

 Abstract:
   Modal dialog for editing build modes: add, delete, reorder, rename, diff.
}
unit BuildModesManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, Grids, Buttons, Menus,
  ButtonPanel, LCLProc, Graphics, IDEOptionsIntf, IDEDialogs, TransferMacros,
  Project, CompOptsIntf, CompilerOptions, Compiler_ModeMatrix,
  BuildModeDiffDlg, LazarusIDEStrConsts;

type

  { TBuildModesForm }

  TBuildModesForm = class(TForm)
    AddSpeedButton: TSpeedButton;
    DeleteSpeedButton: TSpeedButton;
    DiffSpeedButton: TSpeedButton;
    MoveDownSpeedButton: TSpeedButton;
    MoveUpSpeedButton: TSpeedButton;
    BuildModesGroupBox: TGroupBox;
    BuildModesPopupMenu: TPopupMenu;
    BuildModesStringGrid: TStringGrid;
    btnCreateDefaultModes: TButton;
    ButtonPanel1: TButtonPanel;
    NoteLabel: TLabel;
    procedure btnCreateDefaultModesClick(Sender: TObject);
    procedure BuildModesStringGridDrawCell(Sender: TObject; aCol,
      aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DiffSpeedButtonClick(Sender: TObject);
    procedure AddSpeedButtonClick(Sender: TObject);
    procedure DeleteSpeedButtonClick(Sender: TObject);
    procedure MoveDownSpeedButtonClick(Sender: TObject);
    procedure MoveUpSpeedButtonClick(Sender: TObject);
    procedure BuildModesCheckboxToggled(Sender: TObject;
      aCol, aRow: Integer; aState: TCheckboxState);
    procedure BuildModesStringGridSelection(Sender: TObject;
      aCol, aRow: Integer);
    procedure BuildModesStringGridValidateEntry(Sender: TObject;
      aCol, aRow: Integer; const OldValue: string; var NewValue: String);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    fActiveBuildMode: TProjectBuildMode;
    fBuildModes: TProjectBuildModes;
    fShowSession: boolean;
    fModeActiveCol: integer;
    fModeInSessionCol: integer;
    fModeNameCol: integer;
    procedure FillBuildModesGrid(aOnlyActiveState: Boolean = False);
    function GetActiveBuildMode: TProjectBuildMode;
    procedure SetActiveBuildMode(AValue: TProjectBuildMode);
    procedure UpdateBuildModeButtons;
    procedure SetShowSession(const AValue: boolean);
    procedure DoShowSession;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetSelectedBuildMode: TProjectBuildMode;
    procedure SetActiveBuildModeByID(const Identifier: string; SelectInGrid: boolean);
  public
    property ActiveBuildMode: TProjectBuildMode read GetActiveBuildMode write SetActiveBuildMode;
    property BuildModes: TProjectBuildModes read fBuildModes;
    property ShowSession: boolean read fShowSession write SetShowSession;
  end;

var
  OnLoadIDEOptionsHook: TOnLoadIDEOptions;
  OnSaveIDEOptionsHook: TOnSaveIDEOptions;

function ShowBuildModesDlg(aShowSession: Boolean): TModalResult;
procedure SwitchBuildMode(aBuildModeID: string; LoadSaveProjectOptions: boolean);
procedure UpdateBuildModeCombo(aCombo: TComboBox);


implementation

const
  DebugModeName = 'Debug';
  ReleaseModeName = 'Release';

{$R *.lfm}

function ShowBuildModesDlg(aShowSession: Boolean): TModalResult;
var
  frm: TBuildModesForm;
begin
  frm := TBuildModesForm.Create(nil);
  try
    Assert(Assigned(Project1), 'ShowBuildModesDlg: Project is not assigned.');
    // Save changes
    OnSaveIDEOptionsHook(Nil, Project1.CompilerOptions);
    // Copy to dialog
    frm.BuildModes.Assign(Project1.BuildModes, True);
    frm.SetActiveBuildModeByID(Project1.ActiveBuildMode.Identifier,true);
    frm.fShowSession:=aShowSession;
    // Show the form. Let user add / edit / delete.
    Result := frm.ShowModal;
    if Result = mrOk then
    begin
      // Copy back from dialog
      Project1.BuildModes.Assign(frm.BuildModes, True);
      // Switch
      Project1.ActiveBuildModeID:=frm.fActiveBuildMode.Identifier;
      IncreaseBuildMacroChangeStamp;
      // Load options
      if ModeMatrixFrame<>nil then
        ModeMatrixFrame.UpdateModes(true);
      OnLoadIDEOptionsHook(Nil, Project1.CompilerOptions);
    end;
  finally
    frm.Free;
  end;
end;

procedure SwitchBuildMode(aBuildModeID: string; LoadSaveProjectOptions: boolean);
begin
  if LoadSaveProjectOptions then
    OnSaveIDEOptionsHook(Nil, Project1.CompilerOptions);    // Save changes
  Project1.ActiveBuildModeID := aBuildModeID;               // Switch
  IncreaseBuildMacroChangeStamp;
  if LoadSaveProjectOptions then
    OnLoadIDEOptionsHook(Nil, Project1.CompilerOptions);    // Load options
end;

procedure UpdateBuildModeCombo(aCombo: TComboBox);
var
  i, ActiveIndex: Integer;
  CurMode: TProjectBuildMode;
  sl: TStringList;
begin
  ActiveIndex := 0;
  sl:=TStringList.Create;
  try
    {$IFDEF EnableOptionsAllBuildModes}
    sl.Add(lisAllBuildModes);
    {$ENDIF}
    for i := 0 to Project1.BuildModes.Count-1 do
    begin
      CurMode := Project1.BuildModes[i];
      sl.Add(CurMode.Identifier);
      if CurMode = Project1.ActiveBuildMode then
        ActiveIndex := sl.Count-1;  // Will be set as ItemIndex in Combo.
    end;
    aCombo.Items.Assign(sl);
    aCombo.ItemIndex := ActiveIndex;
  finally
    sl.Free;
  end;
end;

{ TBuildModesForm }

constructor TBuildModesForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBuildModes := TProjectBuildModes.Create(Nil);
end;

destructor TBuildModesForm.Destroy;
begin
  FreeAndNil(fBuildModes);
  inherited Destroy;
end;

procedure TBuildModesForm.FormCreate(Sender: TObject);
begin
  ;
end;

procedure TBuildModesForm.FormDestroy(Sender: TObject);
begin
  ;
end;

procedure TBuildModesForm.FormShow(Sender: TObject);
begin
  // options dialog
  BuildModesGroupBox.Caption:=lisBuildModes;
  DoShowSession;
  // modes
  FillBuildModesGrid;
  UpdateBuildModeButtons;

  AddSpeedButton.LoadGlyphFromLazarusResource('laz_add');
  DeleteSpeedButton.LoadGlyphFromLazarusResource('laz_delete');
  MoveUpSpeedButton.LoadGlyphFromLazarusResource('arrow_up');
  MoveDownSpeedButton.LoadGlyphFromLazarusResource('arrow_down');
  DiffSpeedButton.LoadGlyphFromLazarusResource('menu_tool_diff');
end;

procedure TBuildModesForm.DiffSpeedButtonClick(Sender: TObject);
begin
  // show diff dialog
  ShowBuildModeDiffDialog(BuildModes,GetSelectedBuildMode);
end;

procedure TBuildModesForm.btnCreateDefaultModesClick(Sender: TObject);
var
  CurMode: TProjectBuildMode;

  procedure AssignAndSetBooleans(aMode: TProjectBuildMode; IsDebug: Boolean);
  begin
    if CurMode<>nil then
      aMode.Assign(CurMode);               // clone from currently selected mode
    with aMode.CompilerOptions do
    begin
      // Smart linking
      SmartLinkUnit:=not IsDebug;
      LinkSmart:=not IsDebug;
      // Checks
      IOChecks:=IsDebug;
      RangeChecks:=IsDebug;
      OverflowChecks:=IsDebug;
      StackChecks:=IsDebug;
      IncludeAssertionCode:=IsDebug;
      // Debug flags
      GenerateDebugInfo:=IsDebug;
      UseExternalDbgSyms:=IsDebug;
      UseHeaptrc:=IsDebug;
      TrashVariables:=IsDebug;
    end;
  end;

var
  NewMode: TProjectBuildMode;
  i: Integer;
begin
  i:=BuildModesStringGrid.Row-1;
  if (i>=0) then
    CurMode:=fBuildModes[i]
  else
    CurMode:=nil;

  // Create Debug mode
  NewMode:=fBuildModes.Add(DebugModeName);
  AssignAndSetBooleans(NewMode, True);
  NewMode.CompilerOptions.OptimizationLevel:=1;       // Optimization
  NewMode.CompilerOptions.DebugInfoType:=dsDwarf2Set; // Debug
  fActiveBuildMode:=NewMode;                          // activate Debug mode

  // Create Release mode
  NewMode:=fBuildModes.Add(ReleaseModeName);
  AssignAndSetBooleans(NewMode, False);
  NewMode.CompilerOptions.OptimizationLevel:=3;       // Optimization
  NewMode.CompilerOptions.DebugInfoType:=dsAuto;      // Debug

  FillBuildModesGrid;               // show
  // select identifier
  BuildModesStringGrid.Col:=fModeNameCol;
  BuildModesStringGrid.Row:=BuildModesStringGrid.RowCount-1;
end;

procedure TBuildModesForm.AddSpeedButtonClick(Sender: TObject);
var
  i: Integer;
  NewName, Identifier: String;
  CurMode, NewMode: TProjectBuildMode;
begin
  // use current mode as template
  i:=BuildModesStringGrid.Row-1;
  if (i>=0) then
  begin
    Identifier:=BuildModesStringGrid.Cells[fModeNameCol,i+1];
    CurMode:=fBuildModes[i];
  end
  else begin
    Identifier:='Mode';
    CurMode:=nil;
  end;
  // find unique name
  i:=0;
  repeat
    inc(i);
    NewName:=Identifier+IntToStr(i);
  until fBuildModes.Find(NewName)=nil;
  // create new mode
  NewMode:=fBuildModes.Add(NewName);
  // clone from currently selected mode
  if CurMode<>nil then
    NewMode.Assign(CurMode);
  fActiveBuildMode:=NewMode; // activate
  FillBuildModesGrid;     // show
  // select identifier
  BuildModesStringGrid.Col:=fModeNameCol;
  BuildModesStringGrid.Row:=BuildModesStringGrid.RowCount-1;
  BuildModesStringGrid.EditorMode:=true;
end;

procedure TBuildModesForm.DeleteSpeedButtonClick(Sender: TObject);
var
  i: Integer;
  CurMode: TProjectBuildMode;
  Grid: TStringGrid;
begin
  Grid:=BuildModesStringGrid;
  i:=Grid.Row-1;
  if i<0 then exit;
  if fBuildModes.Count=1 then
  begin
    IDEMessageDialog(lisCCOErrorCaption, lisThereMustBeAtLeastOneBuildMode,
      mtError,[mbCancel]);
    exit;
  end;
  CurMode:=fBuildModes[i];
  // when delete the activated: activate another
  if fActiveBuildMode=CurMode then
  begin
    if i<fBuildModes.Count-1 then
      fActiveBuildMode:=fBuildModes[i+1]
    else
      fActiveBuildMode:=fBuildModes[i-1];
  end;
  if fActiveBuildMode=CurMode then begin
    debugln(['TBuildModesForm.BuildModeDeleteSpeedButtonClick activate failed']);
    exit;
  end;
  // delete mode
  fBuildModes.Delete(i);
  FillBuildModesGrid;
  // select next mode
  if i>=Grid.RowCount then
    Grid.Row:=Grid.RowCount-1
  else
    Grid.Row:=i;
end;

procedure TBuildModesForm.MoveDownSpeedButtonClick(Sender: TObject);
var
  i: Integer;
begin
  i:=BuildModesStringGrid.Row-1;
  if i+1>=fBuildModes.Count then exit;
  fBuildModes.Move(i,i+1);
  fBuildModes[0].InSession:=false;
  inc(i);
  FillBuildModesGrid;
  BuildModesStringGrid.Row:=i+1;
end;

procedure TBuildModesForm.MoveUpSpeedButtonClick(Sender: TObject);
var
  i: Integer;
begin
  i:=BuildModesStringGrid.Row-1;
  if i<=0 then exit;
  fBuildModes.Move(i,i-1);
  dec(i);
  fBuildModes[0].InSession:=false;
  FillBuildModesGrid;
  BuildModesStringGrid.Row:=i+1;
end;

procedure TBuildModesForm.BuildModesCheckboxToggled(Sender: TObject;
  aCol, aRow: Integer; aState: TCheckboxState);
var
  CurMode: TProjectBuildMode;
  i: Integer;
  Grid: TStringGrid;
begin
  //debugln(['TBuildModesForm.BuildModesCheckboxToggled Row=',aRow,' Col=',aCol,' ',ord(aState)]);
  i:=aRow-1;
  if (i<0) or (i>=fBuildModes.Count) then exit;
  CurMode:=fBuildModes[i];
  Grid:=BuildModesStringGrid;
  if aCol=fModeActiveCol then
  begin
    // activate
    if CurMode=fActiveBuildMode then begin
      //debugln(['TBuildModesForm.BuildModesCheckboxToggled, is ActiveBuildMode',i]);
      // Switch back to Checked state. There must always be an active mode
      Grid.Cells[aCol,aRow]:=Grid.Columns[aCol].ValueChecked;
    end
    else begin
      //debugln(['TBuildModesForm.BuildModesCheckboxToggled, another Mode',i]);
      fActiveBuildMode:=CurMode;
      FillBuildModesGrid(True);
    end;
  end else if aCol=fModeInSessionCol then
  begin
    // in session
    if (aState=cbChecked) and (i=0) then
    begin
      Grid.Cells[aCol,aRow]:=Grid.Columns[aCol].ValueUnchecked;
      NoteLabel.Caption:=lisTheDefaultModeMustBeStoredInProject;
      exit;
    end;
    CurMode.InSession:=aState=cbChecked;
  end;
end;

procedure TBuildModesForm.BuildModesStringGridSelection(Sender: TObject;
  aCol, aRow: Integer);
begin
  UpdateBuildModeButtons;
end;

procedure TBuildModesForm.BuildModesStringGridValidateEntry(Sender: TObject;
  aCol, aRow: Integer; const OldValue: string; var NewValue: String);
var
  CurMode: TProjectBuildMode;
  s: string;
  i: Integer;
  b: Boolean;
begin
  //debugln(['TBuildModesForm.BuildModesStringGridValidateEntry Row=',aRow,' Col=',aCol]);
  i:=aRow-1;
  if (i<0) or (i>=fBuildModes.Count) then exit;
  CurMode:=fBuildModes[i];
  if aCol=fModeInSessionCol then
  begin
    // in session
    b:=NewValue=BuildModesStringGrid.Columns[aCol].ValueChecked;
    if b and (i=0) then
    begin
      NewValue:=OldValue;
      IDEMessageDialog(lisCCOErrorCaption,lisTheDefaultModeMustBeStoredInProject,
                       mtError,[mbCancel]);
      exit;
    end;
    CurMode.InSession:=b;
  end
  else if aCol=fModeNameCol then
  begin
    // identifier
    s:=NewValue;
    for i:=1 to length(s) do
      if s[i]<' ' then
        s[i]:=' ';
    NewValue:=s;
    if CurMode.Identifier<>s then begin
      for i:=0 to fBuildModes.Count-1 do begin
        if (fBuildModes[i]<>CurMode)
        and (Comparetext(fBuildModes[i].Identifier,NewValue)=0) then begin
          IDEMessageDialog(lisDuplicateEntry,
            lisThereIsAlreadyABuildModeWithThisName, mtError, [mbCancel]);
          NewValue:=CurMode.Identifier;
          exit;
        end;
      end;
      BuildModes.RenameMatrixMode(CurMode.Identifier,s);
      CurMode.Identifier:=s;
    end;
  end;
  NoteLabel.Caption:='';
end;

procedure TBuildModesForm.FillBuildModesGrid(aOnlyActiveState: Boolean);
var
  i: Integer;
  CurMode: TProjectBuildMode;
  Grid: TStringGrid;
begin
  Grid:=BuildModesStringGrid;
  Grid.BeginUpdate;
  Grid.RowCount:=fBuildModes.Count+1;
  for i:=0 to fBuildModes.Count-1 do
  begin
    CurMode:=fBuildModes[i];
    // active
    if CurMode=fActiveBuildMode then
      Grid.Cells[fModeActiveCol,i+1]:=Grid.Columns[fModeActiveCol].ValueChecked
    else
      Grid.Cells[fModeActiveCol,i+1]:=Grid.Columns[fModeActiveCol].ValueUnchecked;
    if not aOnlyActiveState then
    begin
      // in session
      if fModeInSessionCol>=0 then
        if CurMode.InSession then
          Grid.Cells[fModeInSessionCol,i+1]:=Grid.Columns[fModeInSessionCol].ValueChecked
        else
          Grid.Cells[fModeInSessionCol,i+1]:=Grid.Columns[fModeInSessionCol].ValueUnchecked;
      // identifier
      Grid.Cells[fModeNameCol,i+1]:=CurMode.Identifier;
    end;
  end;
  Grid.EndUpdate(true);
end;

procedure TBuildModesForm.UpdateBuildModeButtons;
var
  i: Integer;
  CurMode: TProjectBuildMode;
  Identifier: string;
begin
  i:=BuildModesStringGrid.Row-1;
  if (fBuildModes<>nil) and (i>=0) and (i<fBuildModes.Count) then
  begin
    CurMode:=fBuildModes[i];
    Identifier:=BuildModesStringGrid.Cells[fModeNameCol,i+1];
  end
  else
    CurMode:=nil;
  // Dialog caption
  if Project1<>nil then
    Caption:=Format(dlgProjectOptionsFor,[Project1.GetTitleOrName])
            + ', '+copy(Identifier,1,12)
  else
    Caption:='No project';
  // Buttons
  AddSpeedButton.Hint:=Format(lisAddNewBuildModeCopyingSettingsFrom, [Identifier]);
  DeleteSpeedButton.Enabled:=(CurMode<>nil) and (fBuildModes.Count>1);
  DeleteSpeedButton.Hint:=Format(lisDeleteMode, [Identifier]);
  MoveUpSpeedButton.Enabled:=(CurMode<>nil) and (i>0);
  MoveUpSpeedButton.Hint:=Format(lisMoveOnePositionUp, [Identifier]);
  MoveDownSpeedButton.Enabled:=i<BuildModesStringGrid.RowCount-2;
  MoveDownSpeedButton.Hint:=Format(lisMoveOnePositionDown, [Identifier]);
  DiffSpeedButton.Hint:=lisShowDifferencesBetweenModes;
  NoteLabel.Caption:='';
  btnCreateDefaultModes.Caption:='Create Debug and Release modes';
  btnCreateDefaultModes.Hint:='';
  btnCreateDefaultModes.Visible := (fBuildModes.Find(DebugModeName)=Nil)
                               and (fBuildModes.Find(ReleaseModeName)=Nil);
end;

procedure TBuildModesForm.SetShowSession(const AValue: boolean);
begin
  if AValue=fShowSession then exit;
  fShowSession:=AValue;
  DoShowSession;
  FillBuildModesGrid;
end;

procedure TBuildModesForm.DoShowSession;
var
  Grid: TStringGrid;
begin
  Grid:=BuildModesStringGrid;
  Grid.BeginUpdate;
  fModeActiveCol:=0;
  if fShowSession then
  begin
    fModeInSessionCol:=1;
    fModeNameCol:=2;
    if Grid.Columns.Count<3 then
      Grid.Columns.Insert(fModeInSessionCol);
  end else begin
    fModeInSessionCol:=-1;
    fModeNameCol:=1;
    if Grid.Columns.Count>2 then
      Grid.Columns.Delete(1);
  end;
  BuildModesStringGrid.Columns[fModeActiveCol].Title.Caption:=lisActive;
  BuildModesStringGrid.Columns[fModeActiveCol].SizePriority:=1;
  BuildModesStringGrid.Columns[fModeActiveCol].ButtonStyle:=cbsCheckboxColumn;
  if fModeInSessionCol>=0 then
  begin
    BuildModesStringGrid.Columns[fModeInSessionCol].Title.Caption:=lisInSession;
    BuildModesStringGrid.Columns[fModeInSessionCol].SizePriority:=1;
    BuildModesStringGrid.Columns[fModeInSessionCol].ButtonStyle:=cbsCheckboxColumn;
  end;
  BuildModesStringGrid.Columns[fModeNameCol].Title.Caption:=lisName;
  BuildModesStringGrid.Columns[fModeNameCol].SizePriority:=10;
  BuildModesStringGrid.Columns[fModeNameCol].ButtonStyle:=cbsAuto;
  Grid.EndUpdate(true);
end;

function TBuildModesForm.GetSelectedBuildMode: TProjectBuildMode;
var
  i: LongInt;
begin
  Result:=nil;
  i:=BuildModesStringGrid.Row-1;
  if (i<0) or (i>=fBuildModes.Count) then exit;
  Result:=fBuildModes[i];
end;

procedure TBuildModesForm.OKButtonClick(Sender: TObject);
begin
  ;
end;

procedure TBuildModesForm.CancelButtonClick(Sender: TObject);
begin
  ;
end;

procedure TBuildModesForm.BuildModesStringGridDrawCell(Sender: TObject; aCol,
  aRow: Integer; aRect: TRect; aState: TGridDrawState);
begin
  if (aCol=1) and (aRow=1) then
    (Sender as TStringGrid).Canvas.FillRect(aRect);
end;

function TBuildModesForm.GetActiveBuildMode: TProjectBuildMode;
begin
  Result := fActiveBuildMode;
end;

procedure TBuildModesForm.SetActiveBuildMode(AValue: TProjectBuildMode);
begin
  fActiveBuildMode := AValue;
end;

procedure TBuildModesForm.SetActiveBuildModeByID(const Identifier: string;
  SelectInGrid: boolean);
var
  i: Integer;
begin
  for i:=0 to fBuildModes.Count-1 do
  begin
    if fBuildModes[i].Identifier=Identifier then
    begin
      ActiveBuildMode:=fBuildModes[i];
      if SelectInGrid then
        BuildModesStringGrid.Row:=i+1;
      Break;
    end;
  end;
end;

end.

