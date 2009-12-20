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
  Math, Classes, SysUtils, LCLProc, Controls, FileUtil, LResources, Forms,
  Grids, Graphics, Menus, ComCtrls, Dialogs, AvgLvlTree, DefineTemplates,
  StdCtrls, GraphMath,
  ProjectIntf, IDEImagesIntf,
  PathEditorDlg, Project, PackageSystem, LazarusIDEStrConsts, CompilerOptions,
  IDEProcs;

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
    FRebuilding: boolean;
    FShowGroupGrid: boolean;
    function GetSelectedModeRow: TBuildModeGridRow;
    function GetModeRowCount: integer;
    function GetModeRows(Index: integer): TBuildModeGridRow;
    procedure ClearModeRows;
    procedure FillGridRow(i: integer);
    procedure SetShowGroupGrid(const AValue: boolean);
  protected
    function ValidateEntry(const ACol,ARow:Integer; const OldValue:string;
                           var NewValue:string): boolean; override;
    function ValidateCell(const ACol, ARow: Integer;
                           var NewValue:string): boolean;
    procedure UpdateIndexInGroup(aRow: integer);
    procedure UpdateTypePickList;
    procedure UpdateValuePickList;
    procedure FindBuildVariable(const Identifier: string;
      out Vars: TLazBuildVariables; out aVariable: TLazBuildVariable);
    function SelectCell(aCol, aRow: Integer): boolean; override;
    procedure BuildModesGridEditButtonClick(Sender: TObject);
    procedure GetCheckBoxState(const aCol, aRow: Integer;
                               var aState: TCheckboxState); override;
    procedure SetCheckboxState(const aCol, aRow: Integer;
                               const aState: TCheckboxState); override;
    procedure SetEditText(aCol, aRow: Longint; const aValue: string);
            override;
    procedure DrawCell(aCol, aRow: Integer; aRect: TRect;
          aState: TGridDrawState); override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function AddNewBuildMode(Group: boolean): TBuildMode;
    function InsertNewBuildFlagBehind: TBuildModeFlag;
    procedure DeleteSelectedModeRow;
    function BuildGroupToCol(GroupIndex: integer): integer;
    function ColToBuildGroup(aCol: integer): integer;
    function CellToInclude(aCol, aRow: integer): boolean;
    property Graph: TBuildModeGraph read FGraph;
    procedure RebuildGrid; // call this after Graph has changed
    property ModeRowCount: integer read GetModeRowCount;
    property ModeRows[Index: integer]: TBuildModeGridRow read GetModeRows;
    property GroupModeCount: integer read FGroupModeCount; // number of modes that are group of modes
    property SelectedModeRow: TBuildModeGridRow read GetSelectedModeRow;
    property ShowGroupGrid: boolean read FShowGroupGrid write SetShowGroupGrid;
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
    procedure GridSelection(Sender: TObject; aCol, aRow: Integer);
    procedure NewBuildFlagToolButtonClick(Sender: TObject);
    procedure NewBuildModeGroupToolButtonClick(Sender: TObject);
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
  bmftAddLinkerPath: Result:='+LinkerPath';
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
    for j:=0 to GroupModeCount-1 do begin
      if CellToInclude(j+1,i) then
        Cells[j+1,i]:=Columns[j+1].ValueChecked
      else
        Cells[j+1,i]:=Columns[j+1].ValueUnchecked;
      DebugLn(['TBuildModesGrid.FillGridRow ',j+1,' ',i,' ',Cells[j+1,i]]);
    end;
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

procedure TBuildModesGrid.SetShowGroupGrid(const AValue: boolean);
begin
  if FShowGroupGrid=AValue then exit;
  FShowGroupGrid:=AValue;
  Invalidate;
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
        UpdateValuePickList;
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
    // add project variable names
    AddVars(Project1.CompilerOptions.BuildVariables);
    // add package variable names
    for i:=0 to PackageGraph.Count-1 do
      AddVars(PackageGraph.Packages[i].CompilerOptions.BuildVariables);

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

procedure TBuildModesGrid.UpdateValuePickList;
var
  CurModeRow: TBuildModeGridRow;
  sl: TStringList;
  ValueCol: Integer;
  Identifier: String;
  i: integer;
  Vars: TLazBuildVariables;
  aVariable: TLazBuildVariable;
begin
  ValueCol:=GroupModeCount+2;
  if ValueCol>=Columns.Count then exit;
  CurModeRow:=GetSelectedModeRow;
  sl:=TStringList.Create;
  try
    if (CurModeRow<>nil) and (CurModeRow.Flag<>nil) then begin
      if CurModeRow.Flag.FlagType=bmftSetVariable then begin
        Identifier:=CurModeRow.Flag.Variable;
        // check standard variables
        if SysUtils.CompareText(Identifier,'TargetOS')=0 then begin
          for i:=low(FPCOperatingSystemNames) to high(FPCOperatingSystemNames) do
            sl.Add(FPCOperatingSystemNames[i]);
        end
        else if SysUtils.CompareText(Identifier,'TargetCPU')=0 then begin
          for i:=low(FPCProcessorNames) to high(FPCProcessorNames) do
            sl.Add(FPCProcessorNames[i]);
        end
        else begin
          // search build variable
          FindBuildVariable(Identifier,Vars,aVariable);
          if aVariable<>nil then
            sl.Assign(aVariable.Values);
        end;
        Columns[ValueCol].ButtonStyle:=cbsPickList;
      end else if CurModeRow.Flag.FlagType in BuildModeFlagPaths then
        Columns[ValueCol].ButtonStyle:=cbsEllipsis;
    end;
    sl.Sort;
    Columns[ValueCol].PickList:=sl;
  finally
    sl.Free;
  end;
end;

procedure TBuildModesGrid.FindBuildVariable(const Identifier: string; out
  Vars: TLazBuildVariables; out aVariable: TLazBuildVariable);

  function CheckVars(CurVars: TLazBuildVariables): boolean;
  var
    CurVar: TLazBuildVariable;
    i: Integer;
  begin
    for i:=0 to CurVars.Count-1 do begin
      CurVar:=CurVars[i];
      if SysUtils.CompareText(Identifier,CurVar.Identifier)=0 then
      begin
        Vars:=CurVars;
        aVariable:=CurVar;
        exit(true);
      end;
    end;
    Result:=false;
  end;

var
  i: Integer;
begin
  Vars:=nil;
  aVariable:=nil;

  // check project variables
  if CheckVars(Project1.CompilerOptions.BuildVariables) then
    exit;
  // check package variables
  for i:=0 to PackageGraph.Count-1 do
    if CheckVars(PackageGraph.Packages[i].CompilerOptions.BuildVariables) then
      exit;
end;

function TBuildModesGrid.SelectCell(aCol, aRow: Integer): boolean;
begin
  Result:=inherited SelectCell(aCol, aRow);
  if (not FRebuilding)
  and (not (csDestroyingHandle in ControlState))
  then
    UpdateValuePickList;
end;

function TBuildModesGrid.GetModeRowCount: integer;
begin
  Result:=FModeRows.Count;
end;

procedure TBuildModesGrid.BuildModesGridEditButtonClick(Sender: TObject);
var
  CurModeRow: TBuildModeGridRow;
  CurPathEditor: TPathEditorDialog;
  ValueCol: Integer;
begin
  CurModeRow:=SelectedModeRow;
  if CurModeRow=nil then exit;
  if CurModeRow.Flag=nil then exit;
  ValueCol:=GroupModeCount+2;
  if CurModeRow.Flag.FlagType in BuildModeFlagPaths then begin
    CurPathEditor:=TPathEditorDialog.Create(nil);
    try
      CurPathEditor.BaseDirectory:=Project1.ProjectDirectory;
      CurPathEditor.Path:=CurModeRow.Flag.Value;
      CurPathEditor.Templates:='';
      if CurPathEditor.ShowModal=mrOk then begin
        CurModeRow.Flag.Value:=CurPathEditor.Path;
        Cells[ValueCol,Row]:=CurModeRow.Flag.Value;
      end;
    finally
      CurPathEditor.Free;
    end;
  end;
end;

procedure TBuildModesGrid.GetCheckBoxState(const aCol, aRow: Integer;
  var aState: TCheckboxState);
begin
  if CellToInclude(aCol,aRow) then
    aState:=cbChecked
  else
    aState:=cbUnchecked;
  //DebugLn(['TBuildModesGrid.GetCheckBoxState ',acol,' ',arow,' ',ord(aState)]);
end;

procedure TBuildModesGrid.SetCheckboxState(const aCol, aRow: Integer;
  const aState: TCheckboxState);
var
  CurModeRow: TBuildModeGridRow;
  GrpID: LongInt;
  GrpMode: TBuildMode;
  NewState: TCheckBoxState;
begin
  DebugLn(['TBuildModesGrid.SetCheckboxState ',acol,' ',arow,' ',ord(aState)]);
  NewState:=cbUnchecked;
  if (aRow>=1) and (aRow<=ModeRowCount) and (ModeRows[aRow-1].IndexInGroup=0)
  then begin
    CurModeRow:=ModeRows[aRow-1];
    GrpID:=ColToBuildGroup(aCol);
    if (GrpID>=0) and (GrpID<GroupModeCount) then begin
      if ShowGroupGrid or (not CurModeRow.Mode.ShowIncludes) then begin
        GrpMode:=ModeRows[GrpID].Mode;
        if CurModeRow.Mode=GrpMode then begin
          // invalid circle
          DebugLn(['TBuildModesGrid.SetCheckboxState invalid circle']);
        end else if CurModeRow.Mode.IsIncludedBy(GrpMode)<>(aState=cbChecked) then
        begin
          // state changed
          DebugLn(['TBuildModesGrid.SetCheckboxState STATE CHANGED']);
          if aState=cbChecked then begin
            GrpMode.Include(CurModeRow.Mode);
            NewState:=cbChecked;
          end else begin
            GrpMode.Exclude(CurModeRow.Mode);
          end;
        end else begin
          // state kept
          DebugLn(['TBuildModesGrid.SetCheckboxState STATE KEPT']);
          if CurModeRow.Mode.IsIncludedBy(GrpMode) then
            NewState:=cbChecked;
        end;
      end else begin
        // invalid column
        DebugLn(['TBuildModesGrid.SetCheckboxState invalid col ',ACol,' ',GrpId,' ',GroupModeCount]);
      end;
    end else begin
      // group grid is hidden => keep state
      DebugLn(['TBuildModesGrid.SetCheckboxState group grid is hidden']);
      if CellToInclude(aCol,aRow) then
        NewState:=cbChecked;
    end;
  end else begin
    // invalid row
    DebugLn(['TBuildModesGrid.SetCheckboxState invalid row']);
  end;
  DebugLn(['TBuildModesGrid.SetCheckboxState END ',aCol,' ',aRow,' ',ord(NewState)]);
  inherited SetCheckboxState(aCol, aRow, NewState);
  {for i:=1 to ModeRowCount do begin
    dbgout('  '+dbgs(i));
    for j:=1 to GroupModeCount do begin
      dbgout(' '+dbgs(CellToInclude(j,i)));
    end;
    debugln;
  end;}
end;

procedure TBuildModesGrid.SetEditText(aCol, aRow: Longint; const aValue: string
  );
begin
  inherited SetEditText(aCol, aRow, aValue);
  //DebugLn(['TBuildModesGrid.SetEditText ',aCol,' ',aRow,' ',aValue]);
end;

procedure TBuildModesGrid.DrawCell(aCol, aRow: Integer; aRect: TRect;
  aState: TGridDrawState);
const
  w = 3;

  procedure DrawGrid(Right, Bottom: boolean);
  begin
    if Right then begin
      // draw right grid
      Canvas.Pen.Style := GridLineStyle;
      Canvas.Pen.Color := GridLineColor;
      Canvas.Pen.Width := GridLineWidth;
      Canvas.MoveTo(aRect.Right-1,aRect.Top);
      Canvas.LineTo(aRect.Right-1,aRect.Bottom);
    end;
    if Bottom then begin
      // draw bottom grid
      Canvas.Pen.Style := GridLineStyle;
      Canvas.Pen.Color := GridLineColor;
      Canvas.Pen.Width := GridLineWidth;
      Canvas.MoveTo(aRect.Left,aRect.Bottom-1);
      Canvas.LineTo(aRect.Right,aRect.Bottom-1);
    end;
  end;

var
  CurModeRow: TBuildModeGridRow;
  TypeCol: Integer;
  GrpID: LongInt;
  x: Integer;
  y: Integer;
  RowGrpID: Integer;
  BezierPoints: PPoint;
  r: Integer;
begin
  if (aRow>=1) and (aRow<=ModeRowCount) then begin
    TypeCol:=GroupModeCount+1;
    CurModeRow:=ModeRows[aRow-1];
    //DebugLn(['TBuildModesGrid.DrawCell ',aCol,' ',aRow,' IndexInGroup=',CurModeRow.IndexInGroup]);
    if (CurModeRow.IndexInGroup>0) and (aCol<TypeCol) then begin
      // only the first line of a group shows the name and groups
      // => clear
      PrepareCanvas(aCol, aRow, aState);
      Canvas.FillRect(aRect);
      DrawGrid(aCol=TypeCol-1,
               CurModeRow.IndexInGroup=CurModeRow.Mode.FlagCount-1);
      exit;
    end else if CurModeRow.Mode.ShowIncludes then begin
      GrpID:=ColToBuildGroup(aCol);
      if aCol>=TypeCol then begin
        // groups have no type/value
        PrepareCanvas(aCol, aRow, aState);
        Canvas.FillRect(aRect);
        if aRow=GroupModeCount then
          DrawGrid(false,true);
        exit;
      end else if (GrpID>=0) and (GrpID<GroupModeCount) then begin
        // draw paths from mode row to mode column
        PrepareCanvas(aCol, aRow, aState);
        Canvas.FillRect(aRect);
        Canvas.Brush.Style := bsSolid;
        Canvas.Brush.Color := clBtnFace;
        Canvas.Pen.Style := psClear;
        x:=(aRect.Left+aRect.Right) div 2;
        y:=(aRect.Top+aRect.Bottom) div 2;
        r:=Min(aRect.Right-aRect.Left,aRect.Bottom-aRect.Top) div 2;
        RowGrpID:=aRow-1;
        if GrpID=RowGrpID then begin
          // draw curve left to bottom
          GetMem(BezierPoints,SizeOf(TPoint)*10);
          BezierPoints[0]:=Point(x-r,y-w); // middle,left
            BezierPoints[1]:=Point(x-r+10,y-w); // to the right
            BezierPoints[2]:=Point(x+w,y+r-10); // downwards
          BezierPoints[3]:=Point(x+w,y+r); // bottom, middle
            BezierPoints[4]:=Point(x+w-10,y+r); // to the left
            BezierPoints[5]:=Point(x-w-10,y+r); // to the left
          BezierPoints[6]:=Point(x-w,y+r); // bottom, middle
            BezierPoints[7]:=Point(x-w,y+r-10); // upwards
            BezierPoints[8]:=Point(x-r-10,y+w); // to the left
          BezierPoints[9]:=Point(x-r,y+w); // middle,left
          Canvas.PolyBezier(BezierPoints,10,true,true);
          Freemem(BezierPoints);
          Canvas.FillRect(aRect.Left,y-w,x-r,y+w+1);
          Canvas.FillRect(x-w,y+r,x+w+1,aRect.Bottom);
        end else if GrpID>RowGrpID then begin
          // draw left to right
          Canvas.FillRect(aRect.Left,y-w,aRect.Right,y+w+1);
        end else begin
          // draw top to bottom
          Canvas.FillRect(x-w,aRect.Top,x+w+1,aRect.Bottom);
        end;
        if aRow=GroupModeCount then
          DrawGrid(false,true);
        exit;
      end;
    end;
  end;
  //DebugLn(['TBuildModesGrid.DrawCell draw default ',aCol,' ',aRow]);
  inherited DrawCell(aCol, aRow, aRect, aState);
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
  OnEditButtonClick:=@BuildModesGridEditButtonClick;
end;

destructor TBuildModesGrid.Destroy;
begin
  ClearModeRows;
  FreeAndNil(FModeRows);
  FreeAndNil(FGraph);
  inherited Destroy;
end;

function TBuildModesGrid.AddNewBuildMode(Group: boolean): TBuildMode;
var
  GridRow: TBuildModeGridRow;
  CurFlag: TBuildModeFlag;
  InsertPos: LongInt;
  InsertRow: Integer;
  InsertCol: LongInt;
begin
  FRebuilding:=true;
  try
    Result:=Graph.AddMode(Graph.GetUniqueModeName(nil,nil));
    if Group then
    begin
      Result.ShowIncludes:=true;
      CurFlag:=nil;
      InsertPos:=GroupModeCount;
      InsertCol:=BuildGroupToCol(GroupModeCount-1);
      InsertColRow(true,InsertCol);
      Columns[InsertCol].Title.Caption:=' ';
      Columns[InsertCol].ButtonStyle:=cbsCheckboxColumn;
      inc(FGroupModeCount);
    end else begin
      CurFlag:=Result.AddFlag(bmftNone,'');
      InsertPos:=ModeRowCount;
    end;
    InsertRow:=InsertPos+1;
    InsertColRow(false,InsertRow);
    GridRow:=TBuildModeGridRow.Create(Result,CurFlag);
    FModeRows.Insert(InsertPos,GridRow);
    FillGridRow(InsertRow);
  finally
    FRebuilding:=false;
  end;
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

function TBuildModesGrid.BuildGroupToCol(GroupIndex: integer): integer;
begin
  Result:=GroupModeCount-GroupIndex;
end;

function TBuildModesGrid.ColToBuildGroup(aCol: integer): integer;
begin
  Result:=GroupModeCount-aCol;
end;

function TBuildModesGrid.CellToInclude(aCol, aRow: integer): boolean;
var
  GrpID: LongInt;
  CurMode: TBuildModeGridRow;
begin
  if (aRow>=1) and (ARow<=GroupModeCount) then begin
    CurMode:=ModeRows[aRow-1];
    GrpID:=ColToBuildGroup(aCol);
    if (GrpID>=0) and (GrpID<GroupModeCount) then begin
      if CurMode.Mode.IsIncludedBy(ModeRows[GrpID].Mode) then
        exit(true);
    end;
  end;
  Result:=false;
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
  FRebuilding:=true;
  try
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
    while Columns.Count<GroupModeCount+3 do
      Columns.Add;
    while Columns.Count>GroupModeCount+3 do
      Columns.Delete(Columns.Count-1);
    TypeCol:=GroupModeCount+1;
    ValueCol:=TypeCol+1;
    Columns[0].Width:=150;
    for i:=1 to TypeCol-1 do begin
      Columns[i].Width:=15;
      Columns[i].ButtonStyle:=cbsCheckboxColumn;
    end;
    Columns[TypeCol].Width:=120;
    Columns[TypeCol].ButtonStyle:=cbsPickList;
    Columns[ValueCol].Width:=300;
    Columns[ValueCol].ButtonStyle:=cbsPickList;

    // fill cells
    for i:=0 to ModeRowCount do
      FillGridRow(i);

  finally
    FRebuilding:=false;
  end;
  UpdateTypePickList;
  UpdateValuePickList;
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
  Grid.AddNewBuildMode(false);
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

procedure TBuildModesEditorFrame.NewBuildModeGroupToolButtonClick(
  Sender: TObject);
begin
  Grid.AddNewBuildMode(true);
end;

procedure TBuildModesEditorFrame.DeleteBMRowToolButtonClick(Sender: TObject);
begin
  Grid.DeleteSelectedModeRow;
end;

procedure TBuildModesEditorFrame.GridSelection(Sender: TObject; aCol,
  aRow: Integer);
begin
  UpdateButtons;
end;

constructor TBuildModesEditorFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FGrid:=TBuildModesGrid.Create(Self);
  with Grid do begin
    Name:='Grid';
    Parent:=Self;
    Align:=alClient;
    //OnCellSelected:=@GridCellSelected;
    OnSelection:=@GridSelection;
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

