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

  Abstract:
    The frame for 'build modes' on the compiler options.
    Allows to add/delete/edit build modes and build macro values.
    It does not allow to define new build macros, only values.
}
unit BuildModesEditor;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLProc, Controls, FileUtil, Forms,
  Grids, Graphics, Menus, ComCtrls, Dialogs, AvgLvlTree, DefineTemplates,
  StdCtrls, GraphMath, ExtCtrls, Buttons,
  ProjectIntf, IDEImagesIntf, IDEOptionsIntf, CompOptsIntf,
  PackageDefs, compiler_inherited_options, TransferMacros,
  PathEditorDlg, Project, PackageSystem, LazarusIDEStrConsts, CompilerOptions,
  IDEProcs, BuildModeDiffDlg;

type

  { TBuildModesEditorFrame }

  TBuildModesEditorFrame = class(TAbstractIDEOptionsEditor)
    BuildModeDiffSpeedButton: TSpeedButton;
    BuildMacroValuesGroupBox: TGroupBox;
    BuildMacroValuesStringGrid: TStringGrid;
    BuildModeAddSpeedButton: TSpeedButton;
    BuildModeDeleteSpeedButton: TSpeedButton;
    BuildModeMoveDownSpeedButton: TSpeedButton;
    BuildModeMoveUpSpeedButton: TSpeedButton;
    BuildModesGroupBox: TGroupBox;
    BuildModesPopupMenu: TPopupMenu;
    BuildModesStringGrid: TStringGrid;
    Splitter1: TSplitter;
    procedure BuildModeDiffSpeedButtonClick(Sender: TObject);
    procedure BuildMacroValuesStringGridEditingDone(Sender: TObject);
    procedure BuildMacroValuesStringGridSelectEditor(Sender: TObject; aCol,
      aRow: Integer; var Editor: TWinControl);
    procedure BuildMacroValuesStringGridSelection(Sender: TObject; aCol,
      aRow: Integer);
    procedure BuildModeAddSpeedButtonClick(Sender: TObject);
    procedure BuildModeDeleteSpeedButtonClick(Sender: TObject);
    procedure BuildModeMoveDownSpeedButtonClick(Sender: TObject);
    procedure BuildModeMoveUpSpeedButtonClick(Sender: TObject);
    procedure BuildModesStringGridCheckboxToggled(sender: TObject; aCol,
      aRow: Integer; aState: TCheckboxState);
    procedure BuildModesStringGridSelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
    procedure BuildModesStringGridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure BuildModesStringGridValidateEntry(sender: TObject; aCol,
      aRow: Integer; const OldValue: string; var NewValue: String);
  private
    FLoadShowSessionFromProject: boolean;
    FMacroValues: TProjectBuildMacros;
    FProject: TProject;
    FShowSession: boolean;
    FSwitchingMode: boolean;
    fModeActiveCol: integer;
    fModeInSessionCol: integer;
    fModeNameCol: integer;
    procedure UpdateMacrosControls;
    function GetAllBuildMacros: TStrings;
    procedure CleanMacrosGrid;
    procedure SaveMacros(UpdateControls: boolean);
    procedure UpdateInheritedOptions;
    procedure FillBuildModesGrid;
    procedure UpdateBuildModeButtons;
    procedure ActivateMode(aMode: TProjectBuildMode);
    procedure UpdateShowSession;
    procedure SetShowSession(const AValue: boolean);
    procedure DoShowSession;
    procedure UpdateDialogCaption;
    function GetDialogCaption: string;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    property AProject: TProject read FProject;
    property MacroValues: TProjectBuildMacros read FMacroValues;
    property SwitchingMode: boolean read FSwitchingMode; // the active mode is currently switched
    property ShowSession: boolean read FShowSession write SetShowSession;
    property LoadShowSessionFromProjects: boolean read FLoadShowSessionFromProject
                                              write FLoadShowSessionFromProject;
    function GetSelectedBuildMode: TProjectBuildMode;
  end;

implementation

{$R *.lfm}

{ TBuildModesEditorFrame }

procedure TBuildModesEditorFrame.BuildMacroValuesStringGridSelectEditor(
  Sender: TObject; aCol, aRow: Integer; var Editor: TWinControl);
var
  PickList: TPickListCellEditor;
  sl: TStringList;
  Macros: TStrings;
  Grid: TStringGrid;
  MacroName: string;
  i: LongInt;
  Macro: TLazBuildMacro;
begin
  if MacroValues=nil then exit;
  Grid:=BuildMacroValuesStringGrid;
  //debugln(['TBuildModesEditorFrame.BuildMacroValuesStringGridSelectEditor ',acol,',',aRow,' ',DbgSName(Editor)]);
  if aCol=0 then begin
    // list all build MacroValues
    if not (Editor is TPickListCellEditor) then exit;
    PickList:=TPickListCellEditor(Editor);
    sl:=TStringList.Create;
    Macros:=nil;
    try
      if aRow=Grid.RowCount-1 then
        sl.Add('(none)')
      else
        sl.Add('(delete)');

      Macros:=GetAllBuildMacros;
      sl.AddStrings(Macros);

      PickList.Items.Assign(sl);
    finally
      Macros.Free;
      sl.Free;
    end;
  end else if aCol=1 then begin
    // list all possible values of current macro

    if not (Editor is TPickListCellEditor) then exit;
    PickList:=TPickListCellEditor(Editor);

    MacroName:=Grid.Cells[0,aRow];
    sl:=TStringList.Create;
    try
      Macros:=GetAllBuildMacros;
      i:=Macros.IndexOf(MacroName);
      if i>=0 then begin
        Macro:=TLazBuildMacro(Macros.Objects[i]);
        sl.AddStrings(Macro.Values);
      end else begin
        sl.Add('');
      end;

      PickList.Items.Assign(sl);
    finally
      Macros.Free;
      sl.Free;
    end;
  end;
end;

procedure TBuildModesEditorFrame.BuildMacroValuesStringGridEditingDone(
  Sender: TObject);
begin
  //debugln(['TBuildModesEditorFrame.BuildMacroValuesStringGridEditingDone ']);
  SaveMacros(true);
end;

procedure TBuildModesEditorFrame.BuildModeDiffSpeedButtonClick(Sender: TObject);
begin
  FSwitchingMode:=true;
  try
    // save changes
    OnSaveIDEOptions(Self,AProject.CompilerOptions);
    // show diff dialog
    ShowBuildModeDiffDialog(GetSelectedBuildMode);
    IncreaseBuildMacroChangeStamp;
    // load options
    OnLoadIDEOptions(Self,AProject.CompilerOptions);
  finally
    FSwitchingMode:=false;
  end;
end;

procedure TBuildModesEditorFrame.BuildMacroValuesStringGridSelection(
  Sender: TObject; aCol, aRow: Integer);
begin
  CleanMacrosGrid;
end;

procedure TBuildModesEditorFrame.BuildModeAddSpeedButtonClick(Sender: TObject);
var
  i: Integer;
  NewName: String;
  Identifier: String;
  CurMode: TProjectBuildMode;
  NewMode: TProjectBuildMode;
begin
  // use current mode as template
  i:=BuildModesStringGrid.Row-1;
  if (i>=0) then
  begin
    Identifier:=BuildModesStringGrid.Cells[fModeNameCol,i+1];
    CurMode:=AProject.BuildModes[i];
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
  until AProject.BuildModes.Find(NewName)=nil;
  // create new mode
  NewMode:=AProject.BuildModes.Add(NewName);
  // clone
  if CurMode<>nil then
    NewMode.Assign(CurMode);
  // show
  FillBuildModesGrid;
  // activate
  ActivateMode(NewMode);
  // select identifier
  BuildModesStringGrid.Col:=fModeNameCol;
  BuildModesStringGrid.Row:=BuildModesStringGrid.RowCount-1;
  BuildModesStringGrid.EditorMode:=true;
end;

procedure TBuildModesEditorFrame.BuildModeDeleteSpeedButtonClick(Sender: TObject);
var
  i: Integer;
  CurMode: TProjectBuildMode;
  Grid: TStringGrid;
begin
  Grid:=BuildModesStringGrid;
  i:=Grid.Row-1;
  if i<0 then exit;
  if AProject.BuildModes.Count=1 then
  begin
    MessageDlg(lisCCOErrorCaption, lisThereMustBeAtLeastOneBuildMode,
      mtError,[mbCancel],0);
    exit;
  end;
  CurMode:=AProject.BuildModes[i];
  // when delete the activated: activate another
  if AProject.ActiveBuildMode=CurMode then
  begin
    if i<AProject.BuildModes.Count-1 then
      ActivateMode(AProject.BuildModes[i+1])
    else
      ActivateMode(AProject.BuildModes[i-1]);
  end;
  if AProject.ActiveBuildMode=CurMode then begin
    debugln(['TBuildModesEditorFrame.BuildModeDeleteSpeedButtonClick activate failed']);
    exit;
  end;
  // delete mode
  AProject.BuildModes.Delete(i);
  FillBuildModesGrid;
  // select next mode
  if i>=Grid.RowCount then
    Grid.Row:=Grid.RowCount-1
  else
    Grid.Row:=i;
end;

procedure TBuildModesEditorFrame.BuildModeMoveDownSpeedButtonClick(
  Sender: TObject);
var
  i: Integer;
begin
  i:=BuildModesStringGrid.Row-1;
  if i+1>=AProject.BuildModes.Count then exit;
  AProject.BuildModes.Move(i,i+1);
  AProject.BuildModes[0].InSession:=false;
  inc(i);
  FillBuildModesGrid;
  BuildModesStringGrid.Row:=i+1;
end;

procedure TBuildModesEditorFrame.BuildModeMoveUpSpeedButtonClick(Sender: TObject);
var
  i: Integer;
begin
  i:=BuildModesStringGrid.Row-1;
  if i<=0 then exit;
  AProject.BuildModes.Move(i,i-1);
  dec(i);
  AProject.BuildModes[0].InSession:=false;
  FillBuildModesGrid;
  BuildModesStringGrid.Row:=i+1;
end;

procedure TBuildModesEditorFrame.BuildModesStringGridCheckboxToggled(
  sender: TObject; aCol, aRow: Integer; aState: TCheckboxState);
var
  CurMode: TProjectBuildMode;
  b: Boolean;
  i: Integer;
  Grid: TStringGrid;
begin
  //debugln(['TBuildModesEditorFrame.BuildModesStringGridCheckboxToggled Row=',aRow,' Col=',aCol,' ',ord(aState)]);
  i:=aRow-1;
  if (i<0) or (i>=AProject.BuildModes.Count) then exit;
  //debugln(['TBuildModesEditorFrame.BuildModesStringGridCheckboxToggled ',i]);
  CurMode:=AProject.BuildModes[i];
  Grid:=BuildModesStringGrid;
  if aCol=fModeActiveCol then
  begin
    // activate
    if CurMode=AProject.ActiveBuildMode then
      // there must always be an active mode
      Grid.Cells[aCol,aRow]:=Grid.Columns[aCol].ValueChecked
    else
      ActivateMode(CurMode);
  end else if aCol=fModeInSessionCol then
  begin
    // in session
    b:=aState=cbChecked;
    if b and (i=0) then
    begin
      Grid.Cells[aCol,aRow]:=Grid.Columns[aCol].ValueUnchecked;
      MessageDlg(lisCCOErrorCaption,
        lisTheFirstBuildModeIsTheDefaultModeAndMustBeStoredIn,
        mtError,[mbCancel],0);
      exit;
    end;
    CurMode.InSession:=b;
  end;
end;

procedure TBuildModesEditorFrame.BuildModesStringGridSelectCell(
  Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin

end;

procedure TBuildModesEditorFrame.BuildModesStringGridSelection(Sender: TObject;
  aCol, aRow: Integer);
begin
  UpdateBuildModeButtons;
end;

procedure TBuildModesEditorFrame.BuildModesStringGridValidateEntry(
  sender: TObject; aCol, aRow: Integer; const OldValue: string;
  var NewValue: String);
var
  CurMode: TProjectBuildMode;
  s: string;
  j: Integer;
  b: Boolean;
  i: Integer;
begin
  //debugln(['TBuildModesEditorFrame.BuildModesStringGridValidateEntry Row=',aRow,' Col=',aCol,' ',NewValue]);
  i:=aRow-1;
  if (i<0) or (i>=AProject.BuildModes.Count) then exit;
  //debugln(['TBuildModesEditorFrame.SaveModes ',i]);
  CurMode:=AProject.BuildModes[i];
  if aCol=fModeInSessionCol then
  begin
    // in session
    b:=NewValue=BuildModesStringGrid.Columns[aCol].ValueChecked;
    if b and (i=0) then
    begin
      NewValue:=OldValue;
      MessageDlg(lisCCOErrorCaption,
        lisTheFirstBuildModeIsTheDefaultModeAndMustBeStoredIn,
        mtError,[mbCancel],0);
      exit;
    end;
    CurMode.InSession:=b;
  end
  else if aCol=fModeNameCol then
  begin
    // identifier
    s:=NewValue;
    for j:=1 to length(s) do
      if s[j]<' ' then s[j]:=' ';
    CurMode.Identifier:=s;
    NewValue:=s;
    UpdateDialogCaption;
  end;
end;

procedure TBuildModesEditorFrame.UpdateMacrosControls;
var
  Grid: TStringGrid;
  i: Integer;
begin
  Grid:=BuildMacroValuesStringGrid;
  Grid.RowCount:=MacroValues.Count+2; // + titles + add button

  for i:=0 to MacroValues.Count-1 do begin
    Grid.Cells[0,i+1]:=MacroValues.Names[i];
    Grid.Cells[1,i+1]:=MacroValues.ValueFromIndex(i);
  end;
  i:=MacroValues.Count+1;
  Grid.Cells[0,i]:='(none)';
  Grid.Cells[1,i]:='';
end;

procedure TBuildModesEditorFrame.SetShowSession(const AValue: boolean);
begin
  if AValue=FShowSession then exit;
  FShowSession:=AValue;
  DoShowSession;
  FillBuildModesGrid;
end;

procedure TBuildModesEditorFrame.DoShowSession;
var
  Grid: TStringGrid;
begin
  Grid:=BuildModesStringGrid;
  Grid.BeginUpdate;
  fModeActiveCol:=0;
  if FShowSession then
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
  BuildModesStringGrid.Columns[fModeNameCol].Title.Caption:=lisDebugOptionsFrmName;
  BuildModesStringGrid.Columns[fModeNameCol].SizePriority:=10;
  BuildModesStringGrid.Columns[fModeNameCol].ButtonStyle:=cbsAuto;
  Grid.EndUpdate(true);
end;

procedure TBuildModesEditorFrame.UpdateDialogCaption;
var
  Form: TCustomForm;
begin
  Form:=GetParentForm(Self);
  if Form<>nil then
    Form.Caption:=GetDialogCaption;
end;

function TBuildModesEditorFrame.GetDialogCaption: string;
begin
  if AProject<>nil then
  begin
    Result := aProject.Title;
    if Result = '' then
      Result := ExtractFilenameOnly(aProject.ProjectInfoFile);
    Result:=Format(dlgProjectOptionsFor, [Result]);
    if AProject.BuildModes.Count>1 then
      Result:=Result+', '+copy(AProject.ActiveBuildMode.GetCaption,1,12);
  end else
    Result:='TBuildModesEditorFrame.GetDialogCaption: no project';
end;

function TBuildModesEditorFrame.GetAllBuildMacros: TStrings;

  procedure Add(aBuildMacro: TLazBuildMacro);
  begin
    if GetAllBuildMacros.IndexOf(aBuildMacro.Identifier)>=0 then exit;
    GetAllBuildMacros.AddObject(aBuildMacro.Identifier,aBuildMacro);
  end;

  procedure Add(CompOpts: TLazCompilerOptions);
  var
    i: Integer;
  begin
    for i:=0 to CompOpts.BuildMacros.Count-1 do
      Add(CompOpts.BuildMacros[i]);
  end;

var
  PkgList: TFPList;
  APackage: TLazPackage;
  i: Integer;
begin
  Result:=TStringList.Create;
  if AProject=nil then exit;
  Add(AProject.CompilerOptions);
  PkgList:=nil;
  try
    PackageGraph.GetAllRequiredPackages(AProject.FirstRequiredDependency,PkgList);
    if PkgList<>nil then begin
      for i:=0 to PkgList.Count-1 do begin
        if TObject(PkgList[i]) is TLazPackage then begin
          APackage:=TLazPackage(PkgList[i]);
          Add(APackage.CompilerOptions);
        end;
      end;
    end;
  finally
    PkgList.Free;
  end;

  TStringList(Result).Sort;
end;

procedure TBuildModesEditorFrame.CleanMacrosGrid;
var
  Grid: TStringGrid;
  aRow: Integer;
  MacroName: string;
  NeedNewRow: Boolean;
begin
  Grid:=BuildMacroValuesStringGrid;
  // delete rows
  for aRow:=Grid.RowCount-2 downto 1 do begin
    if aRow=Grid.Row then continue; // row is selected
    MacroName:=Grid.Cells[0,aRow];
    if (MacroName<>'') and IsValidIdent(MacroName) then continue; // valid macro name
    // delete row
    Grid.DeleteColRow(false,aRow);
  end;
  NeedNewRow:=Grid.RowCount<2;
  if (not NeedNewRow) then begin
    MacroName:=Grid.Cells[0,Grid.RowCount-1];
    if (MacroName<>'') and IsValidIdent(MacroName) then
      NeedNewRow:=true;
  end;
  if NeedNewRow then begin
    Grid.RowCount:=Grid.RowCount+1;
    Grid.Cells[0,Grid.RowCount-1]:='(new)';
    Grid.Cells[1,Grid.RowCount-1]:='';
  end;
end;

procedure TBuildModesEditorFrame.SaveMacros(UpdateControls: boolean);
var
  Grid: TStringGrid;
  aRow: Integer;
  MacroName: string;
  Values: TStringList;
  Value: string;
begin
  if MacroValues=nil then exit;
  Grid:=BuildMacroValuesStringGrid;
  Values:=TStringList.Create;
  try
    for aRow:=1 to Grid.RowCount-1 do begin
      MacroName:=Grid.Cells[0,aRow];
      if (MacroName='') or (not IsValidIdent(MacroName)) then continue;
      Value:=Grid.Cells[1,aRow];
      Values.Values[MacroName]:=Value;
    end;
    //debugln(['TBuildModesEditorFrame.Save changed=',not MacroValues.Equals(Values),' New="',Values.Text,'"']);
    if not MacroValues.Equals(Values) then begin
      // has changed
      MacroValues.Assign(Values);
      IncreaseBuildMacroChangeStamp;
      if UpdateControls then begin
        UpdateInheritedOptions;
      end;
    end;
  finally
    Values.Free;
  end;
end;

procedure TBuildModesEditorFrame.UpdateInheritedOptions;
var
  InhOptionCtrl: TCompilerInheritedOptionsFrame;
begin
  InhOptionCtrl:=TCompilerInheritedOptionsFrame(
                               FindOptionControl(TCompilerInheritedOptionsFrame));
  //debugln(['TBuildModesEditorFrame.UpdateInheritedOptions ',DbgSName(InhOptionCtrl)]);
  if InhOptionCtrl=nil then exit;
  InhOptionCtrl.UpdateInheritedTree(AProject.CompilerOptions);
end;

procedure TBuildModesEditorFrame.FillBuildModesGrid;
var
  i: Integer;
  CurMode: TProjectBuildMode;
  Grid: TStringGrid;
begin
  if AProject=nil then exit;

  Grid:=BuildModesStringGrid;
  Grid.BeginUpdate;
  Grid.RowCount:=AProject.BuildModes.Count+1;

  for i:=0 to AProject.BuildModes.Count-1 do begin
    CurMode:=AProject.BuildModes[i];
    // active
    if CurMode=AProject.ActiveBuildMode then
      Grid.Cells[fModeActiveCol,i+1]:=Grid.Columns[fModeActiveCol].ValueChecked
    else
      Grid.Cells[fModeActiveCol,i+1]:=Grid.Columns[fModeActiveCol].ValueUnchecked;
    // in session
    if fModeInSessionCol>=0 then
      if CurMode.InSession then
        Grid.Cells[fModeInSessionCol,i+1]:=Grid.Columns[fModeInSessionCol].ValueChecked
      else
        Grid.Cells[fModeInSessionCol,i+1]:=Grid.Columns[fModeInSessionCol].ValueUnchecked;
    // identifier
    Grid.Cells[fModeNameCol,i+1]:=CurMode.Identifier;
  end;
  Grid.EndUpdate(true);
end;

procedure TBuildModesEditorFrame.UpdateBuildModeButtons;
var
  i: Integer;
  CurMode: TProjectBuildMode;
  Identifier: string;
begin
  i:=BuildModesStringGrid.Row-1;
  if (AProject<>nil) and (AProject.BuildModes<>nil)
  and (i>=0) and (i<AProject.BuildModes.Count) then
  begin
    CurMode:=AProject.BuildModes[i];
    Identifier:=BuildModesStringGrid.Cells[fModeNameCol,i+1];
  end
  else
    CurMode:=nil;

  BuildModeAddSpeedButton.Hint:=Format(lisAddNewBuildModeCopyingSettingsFrom, [
    Identifier]);
  BuildModeDeleteSpeedButton.Enabled:=(CurMode<>nil) and (AProject.BuildModes.Count>1);
  BuildModeDeleteSpeedButton.Hint:=Format(lisDeleteMode, [Identifier]);
  BuildModeMoveUpSpeedButton.Enabled:=(CurMode<>nil) and (i>0);
  BuildModeMoveUpSpeedButton.Hint:=Format(lisMoveOnePositionUp, [Identifier]);
  BuildModeMoveDownSpeedButton.Enabled:=
                       i<BuildModesStringGrid.RowCount-2;
  BuildModeMoveDownSpeedButton.Hint:=Format(lisMoveOnePositionDown, [Identifier]
    );
  BuildModeDiffSpeedButton.Hint:=lisShowDifferencesBetweenModes;
end;

procedure TBuildModesEditorFrame.ActivateMode(aMode: TProjectBuildMode);
begin
  if aMode=AProject.ActiveBuildMode then exit;
  FSwitchingMode:=true;
  try
    // save changes
    OnSaveIDEOptions(Self,AProject.CompilerOptions);
    // switch
    AProject.ActiveBuildMode:=aMode;
    IncreaseBuildMacroChangeStamp;
    // load options
    OnLoadIDEOptions(Self,AProject.CompilerOptions);
  finally
    FSwitchingMode:=false;
  end;
end;

procedure TBuildModesEditorFrame.UpdateShowSession;
begin
  if LoadShowSessionFromProjects then
    ShowSession:=(AProject<>nil)
      and (AProject.SessionStorage in [pssInProjectDir,pssInIDEConfig]);
end;

constructor TBuildModesEditorFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLoadShowSessionFromProject:=true;
end;

destructor TBuildModesEditorFrame.Destroy;
begin
  inherited Destroy;
end;

function TBuildModesEditorFrame.GetTitle: String;
begin
  Result := lisBuildModes;
end;

procedure TBuildModesEditorFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  Grid: TStringGrid;
begin
  BuildModesGroupBox.Caption:=lisBuildModes;
  DoShowSession;

  BuildModeAddSpeedButton.LoadGlyphFromLazarusResource('laz_add');
  BuildModeDeleteSpeedButton.LoadGlyphFromLazarusResource('laz_delete');
  BuildModeMoveUpSpeedButton.LoadGlyphFromLazarusResource('arrow_up');
  BuildModeMoveDownSpeedButton.LoadGlyphFromLazarusResource('arrow_down');
  BuildModeDiffSpeedButton.LoadGlyphFromLazarusResource('menu_tool_diff');

  BuildMacroValuesGroupBox.Caption:=lisSetMacroValues;
  Grid:=BuildMacroValuesStringGrid;
  Grid.Columns.Add;
  Grid.Columns[0].Title.Caption:=lisMacroName;
  Grid.Columns[0].ButtonStyle:=cbsPickList;
  Grid.Columns.Add;
  Grid.Columns[1].Title.Caption:=lisMacroValue;
  Grid.Columns[1].ButtonStyle:=cbsPickList;
end;

procedure TBuildModesEditorFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  PCOptions: TProjectCompilerOptions;
begin
  //debugln(['TBuildModesEditorFrame.ReadSettings ',DbgSName(AOptions)]);
  if AOptions is TProjectCompilerOptions then begin
    PCOptions:=TProjectCompilerOptions(AOptions);
    FProject:=PCOptions.LazProject;
    FMacroValues:=FProject.ActiveBuildMode.MacroValues;

    // modes
    UpdateShowSession;
    FillBuildModesGrid;
    UpdateBuildModeButtons;

    // macros
    MacroValues.Assign(FProject.MacroValues);
    UpdateMacrosControls;

    // options dialog
    UpdateDialogCaption;
  end;
end;

procedure TBuildModesEditorFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions is TProjectCompilerOptions then begin
    SaveMacros(false);
  end;
end;

class function TBuildModesEditorFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectCompilerOptions;
end;

function TBuildModesEditorFrame.GetSelectedBuildMode: TProjectBuildMode;
var
  i: LongInt;
begin
  Result:=nil;
  if aProject=nil then exit;
  i:=BuildModesStringGrid.Row-1;
  if (i<0) or (i>=AProject.BuildModes.Count) then exit;
  Result:=AProject.BuildModes[i];
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TBuildModesEditorFrame,
    CompilerOptionsBuildModes);

end.

