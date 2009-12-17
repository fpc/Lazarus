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
unit BuildModesEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Controls, FileUtil, LResources, Forms, Grids,
  Menus, ComCtrls, Dialogs, AvgLvlTree,
  ProjectIntf, IDEImagesIntf,
  Project, PackageSystem, LazarusIDEStrConsts, CompilerOptions, IDEProcs;

type

  { TBuildModeGridRow }

  TBuildModeGridRow = class
  private
    FFlag: TBuildModeFlag;
    FIndexInGroup: integer;
    FMode: TBuildMode;
  public
    constructor Create(aMode: TBuildMode; aFlag: TBuildModeFlag);
    destructor Destroy; override;
    property Mode: TBuildMode read FMode;
    property Flag: TBuildModeFlag read FFlag;
    property IndexInGroup: integer read FIndexInGroup write FIndexInGroup;
  end;

  { TBuildModesGrid }

  TBuildModesGrid = class(TStringGrid)
  private
    FGraph: TBuildModeGraph;
    FGroupModeCount: integer;
    FModeRows: TFPList; // list of TBuildModeGridRow
    function GetSelectedModeRow: TBuildModeGridRow;
    function GetModeRowCount: integer;
    function GetModeRows(Index: integer): TBuildModeGridRow;
    procedure ClearModeRows;
    procedure FillGridRow(i: integer);
  protected
    function ValidateEntry(const ACol,ARow:Integer; const OldValue:string;
                           var NewValue:string): boolean; override;
    function ValidateCell(const ACol, ARow: Integer;
                           var NewValue:string): boolean;
    procedure UpdateIndexInGroup(aRow: integer);
    procedure UpdateTypePickList;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function AddNewBuildMode: TBuildMode;
    function InsertNewBuildFlagBehind: TBuildModeFlag;
    procedure DeleteSelectedModeRow;
    property Graph: TBuildModeGraph read FGraph;
    procedure RebuildGrid; // call this after Graph changed
    property ModeRowCount: integer read GetModeRowCount;
    property ModeRows[Index: integer]: TBuildModeGridRow read GetModeRows;
    property GroupModeCount: integer read FGroupModeCount; // number of modes that are group of modes
    property SelectedModeRow: TBuildModeGridRow read GetSelectedModeRow;
  end;

  { TBuildModesEditorFrame }

  TBuildModesEditorFrame = class(TFrame)
    BuildModesPopupMenu: TPopupMenu;
    BuildModesToolBar: TToolBar;
    NewBuildModeToolButton: TToolButton;
    NewBuildFlagToolButton: TToolButton;
    DeleteBMRowToolButton: TToolButton;
    NewBuildModeGroupToolButton: TToolButton;
    procedure DeleteBMRowToolButtonClick(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure NewBuildFlagToolButtonClick(Sender: TObject);
    procedure NewBuildModeToolButtonClick(Sender: TObject);
  private
    FGrid: TBuildModesGrid;
    procedure UpdateButtons;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetGraph(Graph: TBuildModeGraph);
    property Grid: TBuildModesGrid read FGrid;
  end;

function BuildModeFlagTypeCaptions(f: TBuildModeFlagType): string;
function CaptionToBuildModeFlagType(s: string): TBuildModeFlagType;

implementation

function BuildModeFlagTypeCaptions(f: TBuildModeFlagType): string;
begin
  case f of
  bmftAddUnitPath: Result:='+UnitPath';
  bmftAddIncludePath: Result:='+IncludePath';
  bmftAddLinkerPath: Result:='+linkerPath';
  bmftAddObjectPath: Result:='+ObjectPath';
  bmftAddLinkerOption: Result:='+LinkerOptions';
  bmftAddCustomOption: Result:='+CustomOptions';
  else Result:='';
  end;
end;

function CaptionToBuildModeFlagType(s: string): TBuildModeFlagType;
begin
  if s='' then exit(bmftNone);
  for Result:=low(Result) to high(Result) do
    if SysUtils.CompareText(s,BuildModeFlagTypeCaptions(Result))=0 then exit;
  if IsValidIdent(s) then
    Result:=bmftSetVariable
  else
    Result:=bmftNone;
end;

{ TBuildModesGrid }

function TBuildModesGrid.GetModeRows(Index: integer): TBuildModeGridRow;
begin
  Result:=TBuildModeGridRow(FModeRows[Index]);
end;

procedure TBuildModesGrid.ClearModeRows;
var
  i: Integer;
begin
  for i:=0 to FModeRows.Count-1 do
    TObject(FModeRows[i]).Free;
  FModeRows.Clear;
  FGroupModeCount:=0;
end;

procedure TBuildModesGrid.FillGridRow(i: integer);
var
  CurRow: TBuildModeGridRow;
  j: Integer;
  CurFlag: TBuildModeFlag;
  TypeStr: String;
  ValueStr: String;
  TypeCol: Integer;
  ValueCol: Integer;
begin
  TypeCol:=GroupModeCount+1;
  ValueCol:=TypeCol+1;
  if i=0 then begin
    Columns[0].Title.Caption:=lisBuildMode;
    for i:=1 to GroupModeCount do Columns[i].Title.Caption:='';
    Columns[TypeCol].Title.Caption:=dlgEnvType;
    Columns[ValueCol].Title.Caption:=dlgValueColor;
  end else begin
    CurRow:=ModeRows[i-1];
    // name
    if CurRow.IndexInGroup=0 then
      Cells[0,i]:=CurRow.Mode.Name
    else
      Cells[0,i]:='';
    // included by
    for j:=0 to GroupModeCount-1 do
      Cells[j+1,i]:='';
    // type + value
    CurFlag:=CurRow.Flag;
    TypeStr:='';
    ValueStr:='';
    if CurFlag<>nil then begin
      if CurFlag.FlagType=bmftSetVariable then
      begin
        TypeStr:=CurFlag.Variable;
        ValueStr:=CurFlag.Value;
      end else
        TypeStr:=BuildModeFlagTypeCaptions(CurFlag.FlagType);
    end;
    Cells[TypeCol,i]:=TypeStr;
    Cells[ValueCol,i]:=ValueStr;
  end;
end;

function TBuildModesGrid.ValidateEntry(const ACol, ARow: Integer;
  const OldValue: string; var NewValue: string): boolean;
begin
  //DebugLn(['TBuildModesGrid.ValidateEntry ',aCol]);
  Result:=inherited ValidateEntry(aCol, aRow, OldValue, NewValue);
  if not Result then exit;
  Result:=ValidateCell(ACol,ARow,NewValue);
end;

function TBuildModesGrid.ValidateCell(const ACol, ARow: Integer;
  var NewValue: string): boolean;
var
  CurModeRow: TBuildModeGridRow;
  TypeCol: Integer;
  ValueCol: Integer;
  FlagType: TBuildModeFlagType;
begin
  Result:=true;
  if (aRow>=1) and (aRow<=ModeRowCount) then begin
    CurModeRow:=ModeRows[aRow-1];
    TypeCol:=GroupModeCount+1;
    ValueCol:=TypeCol+1;
    //DebugLn(['TBuildModesGrid.ValidateCell aCol=',acol,' aRow=',arow,' ValueCol=',ValueCol]);
    if aCol=0 then begin
      if CurModeRow.IndexInGroup=0 then
      begin
        // set new mode name
        NewValue:=Graph.FixModeName(NewValue,CurModeRow.Mode);
        CurModeRow.Mode.Name:=NewValue;
      end else begin
        // this is a sub flag => should be empty
        NewValue:='';
      end;
    end else if ACol=TypeCol then begin
      if CurModeRow.Mode.ShowIncludes then begin
        // this is a group mode => no flags allowed
        NewValue:='';
      end else begin
        NewValue:=SpecialCharsToSpaces(NewValue,true);
        FlagType:=CaptionToBuildModeFlagType(NewValue);
        if (CurModeRow.Flag=nil) and (FlagType<>bmftNone) then begin
          // create flag
          CurModeRow.FFlag:=CurModeRow.Mode.AddFlag(FlagType,'','');
        end else if CurModeRow.Flag<>nil then
          // set new FlagType
          CurModeRow.Flag.FlagType:=FlagType;
        if FlagType=bmftSetVariable then
          // set variable name
          CurModeRow.Flag.Variable:=NewValue
        else
          // clean up variable name
          CurModeRow.Flag.Variable:='';
      end;
    end else if ACol=ValueCol then begin
      if CurModeRow.Mode.ShowIncludes then begin
        // this is a group mode => no flags allowed
        NewValue:='';
      end else begin
        NewValue:=SpecialCharsToSpaces(NewValue,true);
        if (CurModeRow.Flag=nil) or (CurModeRow.Flag.FlagType=bmftNone) then
          // no flag => no value
          NewValue:=''
        else
          // set new value
          CurModeRow.Flag.Value:=NewValue;
      end;
    end;
  end;
end;

procedure TBuildModesGrid.UpdateIndexInGroup(aRow: integer);
var
  IndexInGroup: Integer;
  Mode: TBuildMode;
  Index: LongInt;
begin
  Index:=aRow-1;
  Mode:=ModeRows[Index].Mode;
  while (Index>0) and (Mode=ModeRows[Index-1].Mode) do
    dec(Index);
  IndexInGroup:=0;
  while (Index<ModeRowCount) and (ModeRows[Index].Mode=Mode) do
  begin
    ModeRows[Index].IndexInGroup:=IndexInGroup;
    inc(Index);
    inc(IndexInGroup);
  end;
end;

procedure TBuildModesGrid.UpdateTypePickList;
var
  Identifiers: TStringToStringTree;

  procedure AddVar(V: TLazBuildVariable);
  begin
    if (V.Identifier='') or (not IsValidIdent(V.Identifier)) then exit;
    if Identifiers.Contains(V.Identifier) then exit;
    Identifiers[V.Identifier]:='';
  end;

  procedure AddVars(Vars: TLazBuildVariables);
  var
    i: Integer;
  begin
    for i:=0 to Vars.Count-1 do
      AddVar(Vars.Items[i]);
  end;

var
  TypeCol: Integer;
  i: Integer;
  Node: TAvgLvlTreeNode;
  t: TBuildModeFlagType;
  s: String;
  sl: TStringList;
begin
  TypeCol:=GroupModeCount+1;
  Identifiers:=TStringToStringTree.Create(false);
  sl:=nil;
  try
    // add types
    for t:=low(TBuildModeFlagType) to high(TBuildModeFlagType) do
    begin
      s:=BuildModeFlagTypeCaptions(t);
      if s<>'' then
        Identifiers[s]:='';
    end;

    // add standard variable names
    Identifiers['TargetOS']:='';
    Identifiers['TargetCPU']:='';
    // add package variable names
    for i:=0 to PackageGraph.Count-1 do
      AddVars(PackageGraph.Packages[i].CompilerOptions.BuildVariables);
    // add project variable names
    AddVars(Project1.CompilerOptions.BuildVariables);

    sl:=TStringList.Create;
    Node:=Identifiers.Tree.FindLowest;
    while Node<>nil do begin
      sl.Add(PStringToStringItem(Node.Data)^.Name);
      Node:=Identifiers.Tree.FindSuccessor(Node);
    end;
    Columns[TypeCol].PickList:=sl;
  finally
    sl.Free;
    Identifiers.Free;
  end;
end;

function TBuildModesGrid.GetModeRowCount: integer;
begin
  Result:=FModeRows.Count;
end;

function TBuildModesGrid.GetSelectedModeRow: TBuildModeGridRow;
begin
  if (Row<1) or (Row>ModeRowCount) then
    Result:=nil
  else
    Result:=ModeRows[Row-1];
end;

constructor TBuildModesGrid.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fGraph:=TBuildModeGraph.Create;
  FModeRows:=TFPList.Create;
  Options:=Options+[goEditing];
end;

destructor TBuildModesGrid.Destroy;
begin
  ClearModeRows;
  FreeAndNil(FModeRows);
  FreeAndNil(FGraph);
  inherited Destroy;
end;

function TBuildModesGrid.AddNewBuildMode: TBuildMode;
var
  GridRow: TBuildModeGridRow;
  CurFlag: TBuildModeFlag;
begin
  Result:=Graph.AddMode(Graph.GetUniqueModeName(nil,nil));
  CurFlag:=Result.AddFlag(bmftNone,'');
  RowCount:=RowCount+1;
  GridRow:=TBuildModeGridRow.Create(Result,CurFlag);
  FModeRows.Add(GridRow);
  FillGridRow(RowCount-1);
end;

function TBuildModesGrid.InsertNewBuildFlagBehind: TBuildModeFlag;
var
  CurModeRow: TBuildModeGridRow;
  InsertPos: Integer;
  GridRow: TBuildModeGridRow;
begin
  if (Row<1) or (Row>ModeRowCount) then
  begin
    MessageDlg(lisUnableToAddSetting,
      lisPleaseSelectABuildModeFirst, mtError, [mbCancel], 0);
    exit;
  end;
  //DebugLn(['TBuildModesGrid.InsertNewBuildFlagBehind ',Row]);
  CurModeRow:=ModeRows[Row-1];
  if CurModeRow.Mode.ShowIncludes then
  begin
    MessageDlg(lisUnableToAddSetting,
      Format(lisIsAGroupASettingCanOnlyBeAddedToNormalBuildModes, [
        CurModeRow.Mode.Name]),
      mtError, [mbCancel], 0);
    exit;
  end;
  Result:=CurModeRow.Mode.InsertFlag(CurModeRow.IndexInGroup+1,bmftNone,'','');
  InsertPos:=Row+1;
  GridRow:=TBuildModeGridRow.Create(CurModeRow.Mode,Result);
  FModeRows.Insert(InsertPos-1,GridRow);
  UpdateIndexInGroup(InsertPos);
  InsertColRow(false,InsertPos);
  FillGridRow(InsertPos);
  Row:=InsertPos;
end;

procedure TBuildModesGrid.DeleteSelectedModeRow;
var
  CurModeRow: TBuildModeGridRow;
  GroupCol: LongInt;
begin
  if (Row<1) or (Row>ModeRowCount) then
  begin
    MessageDlg(lisUnableToDelete,
      lisPleaseSelectABuildModeFirst, mtError, [mbCancel], 0);
    exit;
  end;
  CurModeRow:=ModeRows[Row-1];
  if (not CurModeRow.Mode.ShowIncludes) and (CurModeRow.Mode.FlagCount>1) then
  begin
    // delete flag
    if (CurModeRow.Flag.Value<>'') or (CurModeRow.Flag.Variable<>'') then
    begin
      if MessageDlg(lisDeleteSetting2,
        Format(lisDeleteSetting3, ['"', BuildModeFlagTypeCaptions(
          CurModeRow.Flag.FlagType), '"']),
        mtConfirmation,[mbYes,mbNo],0)<>mrYes
      then
        exit;
    end;
    CurModeRow.Mode.DeleteFlag(CurModeRow.IndexInGroup);
    DeleteColRow(false,Row);
  end else begin
    // delete build mode
    if MessageDlg(lisDeleteBuildMode2,
      Format(lisDeleteBuildMode3, ['"', CurModeRow.Mode.Name, '"']),
      mtConfirmation,[mbYes,mbNo],0)<>mrYes
    then
      exit;
    if CurModeRow.Mode.ShowIncludes then begin
      // delete a group build mode
      GroupCol:=Row;
      DeleteColRow(true,GroupCol);
    end;
    // delete a normal build mode
    FModeRows.Remove(CurModeRow);
    DeleteColRow(false,Row);
    Graph.DeleteMode(CurModeRow.Mode);
  end;
end;

procedure TBuildModesGrid.RebuildGrid;
var
  GroupInsertPos: Integer;

  procedure AddRow(CurMode: TBuildMode; NewRow: TBuildModeGridRow);
  var
    InsertPos: LongInt;
  begin
    if CurMode.ShowIncludes then begin
      InsertPos:=GroupInsertPos;
      inc(GroupInsertPos);
    end else begin
      InsertPos:=FModeRows.Count;
    end;
    if (InsertPos=0) or (ModeRows[InsertPos-1].Mode<>CurMode) then
      NewRow.IndexInGroup:=0
    else
      NewRow.IndexInGroup:=ModeRows[InsertPos-1].IndexInGroup+1;
    FModeRows.Insert(InsertPos,NewRow);
  end;

var
  i: Integer;
  CurMode: TBuildMode;
  NewRow: TBuildModeGridRow;
  j: Integer;
  TypeCol: Integer;
  ValueCol: Integer;
  CurFlag: TBuildModeFlag;
begin
  ClearModeRows;
  GroupInsertPos:=0;
  // create rows
  for i:=0 to Graph.ModeCount-1 do begin
    CurMode:=Graph.Modes[i];
    if CurMode.ShowIncludes then inc(FGroupModeCount);
    if (CurMode.FlagCount=0) then begin
      // no flags => create an empty one
      NewRow:=TBuildModeGridRow.Create(CurMode,nil);
      AddRow(CurMode,NewRow);
    end else begin
      for j:=0 to CurMode.FlagCount-1 do begin
        CurFlag:=CurMode.Flags[j];
        NewRow:=TBuildModeGridRow.Create(CurMode,CurFlag);
        AddRow(CurMode,NewRow);
      end;
    end;
  end;
  // setup grid
  RowCount:=FModeRows.Count+1;
  FixedRows:=1;
  FixedCols:=0;
  DebugLn(['TBuildModesGrid.RebuildGrid AAA0 ',Columns.Count,' ',ColCount,' ',TypeCol,' ',GroupModeCount]);
  while Columns.Count<GroupModeCount+3 do
    Columns.Add;
  while Columns.Count>GroupModeCount+3 do
    Columns.Delete(Columns.Count-1);
  TypeCol:=GroupModeCount+1;
  DebugLn(['TBuildModesGrid.RebuildGrid AAA1 ',Columns.Count,' ',ColCount,' ',TypeCol,' ',GroupModeCount]);
  ValueCol:=TypeCol+1;
  Columns[0].Width:=150;
  for i:=1 to TypeCol-1 do
    Columns[i].Width:=20;
  DebugLn(['TBuildModesGrid.RebuildGrid AAA2 ',Columns.Count,' ',ColCount,' ',TypeCol,' ',GroupModeCount]);
  Columns[TypeCol].Width:=120;
  Columns[TypeCol].ButtonStyle:=cbsPickList;
  Columns[ValueCol].Width:=1000;

  // fill cells
  for i:=0 to ModeRowCount do
    FillGridRow(i);

  UpdateTypePickList;
end;

{ TBuildModeGridRow }

constructor TBuildModeGridRow.Create(aMode: TBuildMode; aFlag: TBuildModeFlag);
begin
  FMode:=aMode;
  FFlag:=aFlag;
end;

destructor TBuildModeGridRow.Destroy;
begin
  inherited Destroy;
end;

{ TBuildModesEditorFrame }

procedure TBuildModesEditorFrame.NewBuildModeToolButtonClick(Sender: TObject);
begin
  Grid.AddNewBuildMode;
end;

procedure TBuildModesEditorFrame.UpdateButtons;
var
  Mode: TBuildModeGridRow;
begin
  Mode:=Grid.SelectedModeRow;
  NewBuildFlagToolButton.Enabled:=(Mode<>nil) and (not Mode.Mode.ShowIncludes);
  DeleteBMRowToolButton.Enabled:=(Mode<>nil);
  if (Mode<>nil) and (not Mode.Mode.ShowIncludes) and (Mode.Mode.FlagCount>1)
  then
    DeleteBMRowToolButton.Hint:=lisDeleteSetting
  else
    DeleteBMRowToolButton.Hint:=lisDeleteBuildMode;
end;

procedure TBuildModesEditorFrame.NewBuildFlagToolButtonClick(Sender: TObject);
begin
  Grid.InsertNewBuildFlagBehind;
end;

procedure TBuildModesEditorFrame.GridSelectCell(Sender: TObject; aCol,
  aRow: Integer; var CanSelect: Boolean);
begin
  UpdateButtons;
end;

procedure TBuildModesEditorFrame.DeleteBMRowToolButtonClick(Sender: TObject);
begin
  Grid.DeleteSelectedModeRow;
end;

constructor TBuildModesEditorFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FGrid:=TBuildModesGrid.Create(Self);
  with Grid do begin
    Name:='Grid';
    Parent:=Self;
    Align:=alClient;
    OnSelectCell:=@GridSelectCell;
  end;

  BuildModesToolBar.Images := IDEImages.Images_16;
  NewBuildModeToolButton.Hint:=lisNewBuildMode;
  NewBuildModeToolButton.ImageIndex := IDEImages.LoadImage(16, 'laz_add');
  NewBuildFlagToolButton.Hint:=lisNewSetting;
  NewBuildFlagToolButton.ImageIndex := IDEImages.LoadImage(16, 'laz_edit');
  DeleteBMRowToolButton.Hint:=lisDeleteRow;
  DeleteBMRowToolButton.ImageIndex := IDEImages.LoadImage(16, 'laz_delete');
  NewBuildModeGroupToolButton.Hint:=lisNewGroupASetOfModes;
  NewBuildModeGroupToolButton.ImageIndex:=IDEImages.LoadImage(16, 'laz_add');

  // laz_edit, arrow_up, arrow_down
  UpdateButtons;
end;

destructor TBuildModesEditorFrame.Destroy;
begin
  inherited Destroy;
end;

procedure TBuildModesEditorFrame.SetGraph(Graph: TBuildModeGraph);
begin
  Grid.Graph.Assign(Graph);
  Grid.RebuildGrid;
end;

initialization
  {$I buildmodeseditor.lrs}

end.

