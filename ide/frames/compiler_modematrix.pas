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

 ToDo:
   - load old build macro values into matrix
   - save matrix options for old build macro values
   - ifdef old frame
   - ifdef old macro value classes
   - wiki
   - undo: combine changes while editing a cell
   - remove old frame
   - remove old macro value classes
   - resourcestring for value types?
}
unit Compiler_ModeMatrix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, LazFileUtils, LazLogger,
  LResources, Forms, Controls, Graphics, ComCtrls, Menus,
  KeywordFuncLists,
  IDEOptionsIntf, IDEImagesIntf, CompOptsIntf,
  EnvironmentOpts,
  PackageSystem, PackageDefs, Project, LazarusIDEStrConsts, TransferMacros,
  ModeMatrixOpts, ModeMatrixCtrl;

type

  { TCompOptModeMatrix }

  TCompOptModeMatrix = class(TAbstractIDEOptionsEditor)
    BMMatrixToolBar: TToolBar;
    BMMDeleteToolButton: TToolButton;
    BMMMoveDownToolButton: TToolButton;
    BMMMoveUpToolButton: TToolButton;
    BMMRedoToolButton: TToolButton;
    BMMUndoToolButton: TToolButton;
    BMMNewCustomOptionMenuItem: TMenuItem;
    BMMAddPopupMenu: TPopupMenu;
    BMMNewTargetMenuItem: TMenuItem;
    BMMAddToolButton: TToolButton;
    BMMNewIDEMacroMenuItem: TMenuItem;
    BMMNewOutDirMenuItem: TMenuItem;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure BMMDeleteToolButtonClick(Sender: TObject);
    procedure BMMMoveDownToolButtonClick(Sender: TObject);
    procedure BMMMoveUpToolButtonClick(Sender: TObject);
    procedure BMMAddPopupMenuPopup(Sender: TObject);
    procedure BMMNewIDEMacroMenuItemClick(Sender: TObject);
    procedure BMMNewOutDirMenuItemClick(Sender: TObject);
    procedure BMMRedoToolButtonClick(Sender: TObject);
    procedure BMMUndoToolButtonClick(Sender: TObject);
    procedure BMMNewCustomOptionMenuItemClick(Sender: TObject);
    procedure BMMNewTargetMenuItemClick(Sender: TObject);
    procedure BMMAddToolButtonClick(Sender: TObject);
    procedure GridEditingDone(Sender: TObject);
    procedure GridGetCellHightlightColor(Sender: TObject; aCol, aRow: integer;
      var aColor: TColor);
    procedure GridSelection(Sender: TObject; {%H-}aCol, {%H-}aRow: Integer);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure GridShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure OnAddMacroMenuItemClick(Sender: TObject);
  private
    FErrorColor: TColor;
    FGrid: TGroupedMatrixControl;
    FGroupIDE: TGroupedMatrixGroup;
    FGroupProject: TGroupedMatrixGroup;
    FGroupSession: TGroupedMatrixGroup;
    FIDEColor: TColor;
    FProject: TProject;
    FProjectColor: TColor;
    FSessionColor: TColor;
    fOldIDEOptions: TBuildMatrixOptions;
    fOldSharedOptions: TBuildMatrixOptions;
    fOldSessionOptions: TBuildMatrixOptions;
    fCaptionPatternMacroName: string;
    fCaptionPatternMacroValue: string;
    procedure DoWriteSettings;
    procedure MoveRow(Direction: integer);
    procedure UpdateButtons;
    function AddTarget(StorageGroup: TGroupedMatrixGroup): TGroupedMatrixGroup;
    procedure CreateNewOption(aTyp, aValue: string);
    procedure CreateNewTarget;
    function GetCaptionValue(aCaption, aPattern: string): string;
  protected
    procedure VisibleChanged; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    procedure RestoreSettings(AOptions: TAbstractIDEOptions); override;
    procedure UpdateModes(UpdateGrid: boolean = true);
    procedure UpdateActiveMode;
  public
    property Grid: TGroupedMatrixControl read FGrid;
    property GroupIDE: TGroupedMatrixGroup read FGroupIDE;
    property GroupProject: TGroupedMatrixGroup read FGroupProject;
    property GroupSession: TGroupedMatrixGroup read FGroupSession;
    property IDEColor: TColor read FIDEColor write FIDEColor;
    property ProjectColor: TColor read FProjectColor write FProjectColor;
    property SessionColor: TColor read FSessionColor write FSessionColor;
    property ErrorColor: TColor read FErrorColor write FErrorColor;
    property LazProject: TProject read FProject;
  end;

// assign
function IsEqual(Options: TBuildMatrixOptions; StorageGroup: TGroupedMatrixGroup): boolean;
procedure AssignBuildMatrixOptionsToGroup(Options: TBuildMatrixOptions;
  Matrix: TGroupedMatrix; StorageGroup: TGroupedMatrixGroup);
procedure AssignBuildMatrixGroupToOptions(StorageGroup: TGroupedMatrixGroup;
  Options: TBuildMatrixOptions; InvalidateBuildMacros: boolean);

// targets, see BuildMatrixTargetFits
function TargetsPrefix: string;
function AddMatrixTarget(Matrix: TGroupedMatrix; StorageGroup: TGroupedMatrixGroup): TGroupedMatrixGroup;
function BuildMatrixTargetsAsHint(const Targets: String): String;

// type
function BuildMatrixOptionTypeCaption(Typ: TBuildMatrixOptionType): string;
function CaptionToBuildMatrixOptionType(s: string): TBuildMatrixOptionType;
function BuildMatrixOptionTypeHint(Typ: TBuildMatrixOptionType): string;
function BuildMatrixDefaultValue(Typ: TBuildMatrixOptionType): string;

var
  ModeMatrixFrame: TCompOptModeMatrix = nil;

implementation

function BuildMatrixOptionTypeCaption(Typ: TBuildMatrixOptionType): string;
begin
  case Typ of
  bmotCustom: Result:='Custom';
  bmotOutDir: Result:='OutDir';
  bmotIDEMacro: Result:='IDE Macro';
  else Result:='?';
  end;
end;

function CaptionToBuildMatrixOptionType(s: string): TBuildMatrixOptionType;
begin
  for Result:=low(TBuildMatrixOptionType) to high(TBuildMatrixOptionType) do
    if s=BuildMatrixOptionTypeCaption(Result) then exit;
  Result:=bmotCustom;
end;

function BuildMatrixOptionTypeHint(Typ: TBuildMatrixOptionType): string;
begin
  case Typ of
  bmotCustom: Result:=lisMMAppendArbitraryFpcOptionsEGO1GhtlDFlag;
  bmotOutDir: Result:=lisMMOverrideOutputDirectoryFUOfTarget;
  bmotIDEMacro: Result:=lisMMSetAnIDEMacroEGLCLWidgetTypeWin32;
  else Result:='?';
  end;
end;

function BuildMatrixDefaultValue(Typ: TBuildMatrixOptionType): string;
begin
  Result:='';
  case Typ of
  bmotIDEMacro: Result:='MacroName:=Value';
  bmotOutDir: Result:='lib/$(TargetCPU)-$(TargetOS)/$(BuildMode)';
  end;
end;

function IsEqual(Options: TBuildMatrixOptions; StorageGroup: TGroupedMatrixGroup
  ): boolean;
// ignore empty targets
var
  OptIndex: Integer;
  GrpIndex: Integer;
  Target: TGroupedMatrixGroup;
  i: Integer;
  ValueRow: TGroupedMatrixValue;
  Option: TBuildMatrixOption;
  MacroName: string;
  MacroValue: string;
begin
  Result:=false;
  OptIndex:=0;
  for GrpIndex:=0 to StorageGroup.Count-1 do begin
    Target:=TGroupedMatrixGroup(StorageGroup[GrpIndex]);
    if not (Target is TGroupedMatrixGroup) then begin
      debugln(['IsEqual StorageGroup expected group, but found ',DbgSName(Target)]);
      exit;
    end;
    for i:=0 to Target.Count-1 do begin
      ValueRow:=TGroupedMatrixValue(Target[i]);
      if not (ValueRow is TGroupedMatrixValue) then begin
        debugln(['IsEqual Target expected Value, but found ',DbgSName(ValueRow)]);
        exit;
      end;
      if OptIndex>=Options.Count then exit;
      // compare option
      Option:=Options[OptIndex];
      if Option.Targets<>Target.Value then exit;
      if Option.Modes<>ValueRow.GetNormalizedModes then exit;
      if Option.Typ<>CaptionToBuildMatrixOptionType(ValueRow.Typ) then exit;
      if Option.Typ=bmotIDEMacro then begin
        SplitMatrixMacro(ValueRow.Value,MacroName,MacroValue,false);
        if Option.MacroName<>MacroName then exit;
        if Option.Value<>MacroValue then exit;
      end else begin
        if Option.Value<>ValueRow.Value then exit;
      end;
      inc(OptIndex);
    end;
  end;
  Result:=OptIndex=Options.Count;
end;

procedure AssignBuildMatrixOptionsToGroup(Options: TBuildMatrixOptions;
  Matrix: TGroupedMatrix; StorageGroup: TGroupedMatrixGroup);
var
  OptIndex: Integer;
  Option: TBuildMatrixOption;
  TargetGrp: TGroupedMatrixGroup;
  Value: String;
begin
  if IsEqual(Options,StorageGroup) then exit;
  StorageGroup.Clear;
  TargetGrp:=nil;
  for OptIndex:=0 to Options.Count-1 do begin
    Option:=Options[OptIndex];
    if (TargetGrp=nil) or (TargetGrp.Value<>Option.Targets) then
      TargetGrp:=AddMatrixTarget(Matrix,StorageGroup);
    Value:=Option.Value;
    if Option.Typ=bmotIDEMacro then
      Value:=Option.MacroName+':='+Value;
    Matrix.AddValue(TargetGrp,Option.Modes,
                    BuildMatrixOptionTypeCaption(Option.Typ),Value);
  end;
end;

procedure AssignBuildMatrixGroupToOptions(StorageGroup: TGroupedMatrixGroup;
  Options: TBuildMatrixOptions; InvalidateBuildMacros: boolean);
var
  GrpIndex: Integer;
  Target: TGroupedMatrixGroup;
  ValueRow: TGroupedMatrixValue;
  i: Integer;
  Option: TBuildMatrixOption;
  MacroName: string;
  MacroValue: string;
begin
  if IsEqual(Options,StorageGroup) then exit;
  Options.Clear;
  for GrpIndex:=0 to StorageGroup.Count-1 do begin
    Target:=TGroupedMatrixGroup(StorageGroup[GrpIndex]);
    if not (Target is TGroupedMatrixGroup) then begin
      debugln(['AssignBuildMatrixGroupToOptions StorageGroup expected group, but found ',DbgSName(Target)]);
      exit;
    end;
    for i:=0 to Target.Count-1 do begin
      ValueRow:=TGroupedMatrixValue(Target[i]);
      if not (ValueRow is TGroupedMatrixValue) then begin
        debugln(['AssignBuildMatrixGroupToOptions Target expected Value, but found ',DbgSName(ValueRow)]);
        exit;
      end;
      Option:=Options.Add(CaptionToBuildMatrixOptionType(ValueRow.Typ),
                          Target.Value);
      Option.Modes:=ValueRow.GetNormalizedModes;
      if Option.Typ=bmotIDEMacro then begin
        SplitMatrixMacro(ValueRow.Value,MacroName,MacroValue,false);
        Option.MacroName:=MacroName;
        Option.Value:=MacroValue;
        //debugln(['AssignBuildMatrixGroupToOptions Name="',MacroName,'" Value="',MacroValue,'"']);
      end else begin
        Option.Value:=ValueRow.Value;
      end;
    end;
  end;
  if InvalidateBuildMacros then
    IncreaseBuildMacroChangeStamp;
end;

function TargetsPrefix: string;
begin
  Result:=lisMMTargets;
end;

function AddMatrixTarget(Matrix: TGroupedMatrix; StorageGroup: TGroupedMatrixGroup
  ): TGroupedMatrixGroup;
begin
  Result:=Matrix.AddGroup(StorageGroup,TargetsPrefix,'*');
  Result.Writable:=true;
end;

function BuildMatrixTargetsAsHint(const Targets: String): String;
var
  ExcludeProject: Boolean;
  IncludeProject: Boolean;
  All: Boolean;
  Includes: String;
  Excludes: String;
  Target: String;
  StartP: Integer;
  p: Integer;
begin
  Result:=CheckBuildMatrixTargetsSyntax(Targets);
  if Result<>'' then begin
    Result:=lisWarning+Result;
    exit;
  end;
  p:=1;
  Excludes:='';
  Includes:='';
  All:=false;
  IncludeProject:=false;
  ExcludeProject:=false;
  while (p<=length(Targets)) do begin
    StartP:=p;
    while (p<=length(Targets)) and (Targets[p]<>',') do inc(p);
    Target:=copy(Targets,StartP,p-StartP);
    if Target<>'' then begin
      if Target[1]='-' then begin
        system.Delete(Target,1,1);
        if Target<>'' then begin
          if Target=BuildMatrixProjectName then
            ExcludeProject:=true
          else begin
            if Excludes<>'' then Excludes+=',';
            Excludes+=Target;
          end;
        end;
      end else begin
        if Target='*' then
          All:=true
        else if Target=BuildMatrixProjectName then
          IncludeProject:=true
        else begin
          if Includes<>'' then Includes+=',';
          Includes+=Target;
        end;
      end;
    end;
    inc(p);
  end;
  if ExcludeProject then
    IncludeProject:=false;
  if All then begin
    if ExcludeProject then
      Result+=lisMMApplyToAllPackages+LineEnding
    else
      Result+=lisMMApplyToAllPackagesAndProjects+LineEnding;
  end
  else begin
    if IncludeProject then
      Result+=lisMMApplyToProject+LineEnding;
    if Includes<>'' then
      Result+=Format(lisMMApplyToAllPackagesMatching, [Includes])+LineEnding;
  end;
  if Excludes<>'' then
    Result+=Format(lisMMExcludeAllPackagesMatching, [Excludes])+LineEnding;
end;

{$R *.lfm}

{ TCompOptModeMatrix }

procedure TCompOptModeMatrix.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  UpdateButtons;
end;

procedure TCompOptModeMatrix.GridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  MatRow: TGroupedMatrixRow;
  ValueRow: TGroupedMatrixValue;
  NewValue: String;
  Typ: TBuildMatrixOptionType;
  OldTyp: TBuildMatrixOptionType;
begin
  if ACol=Grid.TypeCol then begin
    if ARow<Grid.FixedRows then exit;
    MatRow:=Grid.Matrix[ARow-Grid.FixedRows];
    if MatRow is TGroupedMatrixValue then begin
      ValueRow:=TGroupedMatrixValue(MatRow);
      OldTyp:=CaptionToBuildMatrixOptionType(ValueRow.Typ);
      if ValueRow.Value=BuildMatrixDefaultValue(OldTyp) then begin
        // change default value
        Typ:=CaptionToBuildMatrixOptionType(Value);
        NewValue:=BuildMatrixDefaultValue(Typ);
        ValueRow.Value:=NewValue;
        Grid.InvalidateCell(Grid.ValueCol,ARow);
      end;
    end;
  end;
end;

procedure TCompOptModeMatrix.GridShowHint(Sender: TObject; HintInfo: PHintInfo);
var
  aCol: Longint;
  aRow: Longint;
  MatRow: TGroupedMatrixRow;
  h: String;
  GroupRow: TGroupedMatrixGroup;
  Targets: String;
  ValueRow: TGroupedMatrixValue;
  MacroName: string;
  MacroValue: string;
begin
  aCol:=0;
  aRow:=0;
  Grid.MouseToCell(HintInfo^.CursorPos.X,HintInfo^.CursorPos.Y,aCol,aRow);
  if aRow<Grid.FixedCols then exit;
  MatRow:=Grid.Matrix[aRow-Grid.FixedRows];
  h:='';
  if MatRow is TGroupedMatrixGroup then begin
    GroupRow:=TGroupedMatrixGroup(MatRow);
    if GroupRow.Group<>nil then begin
      // a target group
      Targets:=GroupRow.Value;
      h:=BuildMatrixTargetsAsHint(Targets);
    end;
  end else if MatRow is TGroupedMatrixValue then begin
    ValueRow:=TGroupedMatrixValue(MatRow);
    if ValueRow.Typ=BuildMatrixOptionTypeCaption(bmotIDEMacro) then begin
      h:='';
      try
        SplitMatrixMacro(ValueRow.Value,MacroName,MacroValue,true);
      except
        on E: EMMMacroSyntaxException do begin
          h:=lisError+E.Message;
        end;
      end;
    end;
  end;
  HintInfo^.HintStr:=h;
end;

procedure TCompOptModeMatrix.OnAddMacroMenuItemClick(Sender: TObject);
var
  ValueMenuItem: TMenuItem;
  MacroMenuItem: TMenuItem;
  MacroName: String;
  Value: String;
begin
  ValueMenuItem:=Sender as TMenuItem;
  MacroMenuItem:=ValueMenuItem.Parent;
  MacroName:=GetCaptionValue(MacroMenuItem.Caption,fCaptionPatternMacroName);
  Value:=GetCaptionValue(ValueMenuItem.Caption,fCaptionPatternMacroValue);
  CreateNewOption(BuildMatrixOptionTypeCaption(bmotIDEMacro),MacroName+':='+Value);
end;

procedure TCompOptModeMatrix.BMMNewCustomOptionMenuItemClick(Sender: TObject);
begin
  CreateNewOption(BuildMatrixOptionTypeCaption(bmotCustom),BuildMatrixDefaultValue(bmotCustom));
end;

procedure TCompOptModeMatrix.BMMUndoToolButtonClick(Sender: TObject);
begin
  Grid.Undo;
  UpdateButtons;
end;

procedure TCompOptModeMatrix.BMMNewTargetMenuItemClick(Sender: TObject);
begin
  CreateNewTarget;
end;

procedure TCompOptModeMatrix.BMMAddToolButtonClick(Sender: TObject);
var
  p: TPoint;
begin
  p:=BMMAddToolButton.ClientToScreen(Point(0,BMMAddToolButton.Height));
  BMMAddPopupMenu.PopUp(p.x,p.y);
end;

procedure TCompOptModeMatrix.GridEditingDone(Sender: TObject);
begin
  //DebugLn(['TFrame1.GridEditingDone ']);
  UpdateButtons;
end;

procedure TCompOptModeMatrix.GridGetCellHightlightColor(Sender: TObject; aCol,
  aRow: integer; var aColor: TColor);
var
  MatRow: TGroupedMatrixRow;
  ValueRow: TGroupedMatrixValue;
  MacroName: string;
  MacroValue: string;
  GroupRow: TGroupedMatrixGroup;
  Targets: String;
begin
  if aCol=Grid.ValueCol then begin
    if aRow<Grid.FixedRows then exit;
    MatRow:=Grid.Matrix[aRow-1];
    if MatRow is TGroupedMatrixGroup then begin
      GroupRow:=TGroupedMatrixGroup(MatRow);
      if GroupRow.Group<>nil then begin
        // a target group
        Targets:=GroupRow.Value;
        if CheckBuildMatrixTargetsSyntax(Targets)<>'' then
          aColor:=ErrorColor;
      end;
    end else if MatRow is TGroupedMatrixValue then begin
      ValueRow:=TGroupedMatrixValue(MatRow);
      if ValueRow.Typ=BuildMatrixOptionTypeCaption(bmotIDEMacro) then begin
        if not SplitMatrixMacro(ValueRow.Value,MacroName,MacroValue,false) then
          aColor:=ErrorColor;
      end;
    end;
  end;
end;

procedure TCompOptModeMatrix.BMMRedoToolButtonClick(Sender: TObject);
begin
  Grid.Redo;
  UpdateButtons;
end;

procedure TCompOptModeMatrix.BMMMoveUpToolButtonClick(Sender: TObject);
begin
  MoveRow(-1);
end;

procedure TCompOptModeMatrix.BMMAddPopupMenuPopup(Sender: TObject);
var
  i: Integer;
  Pkg: TLazPackage;
  Macros: TLazBuildMacros;
  j: Integer;
  Macro: TLazBuildMacro;
  List: TStringList;
  MenuIndex: Integer;
  MacroMenuItem: TMenuItem;
  ValueMenuItem: TMenuItem;
begin
  List:=TStringList.Create;
  try
    for i:=0 to PackageGraph.Count-1 do begin
      Pkg:=PackageGraph[i];
      Macros:=Pkg.CompilerOptions.BuildMacros;
      for j:=0 to Macros.Count-1 do begin
        Macro:=Macros[j];
        if not IsValidIdent(Macro.Identifier) then continue;
        List.AddObject(Macro.Identifier,Macro);
      end;
    end;
    List.Sort;
    MenuIndex:=BMMNewTargetMenuItem.MenuIndex;
    for i:=0 to List.Count-1 do begin
      inc(MenuIndex);
      Macro:=TLazBuildMacro(List.Objects[i]);
      if BMMAddPopupMenu.Items.Count=MenuIndex then
        BMMAddPopupMenu.Items.Add(TMenuItem.Create(Self));
      MacroMenuItem:=BMMAddPopupMenu.Items[MenuIndex];
      MacroMenuItem.Caption:=Format(fCaptionPatternMacroName,[Macro.Identifier]);
      if Macro.Values<>nil then begin
        for j:=0 to Macro.Values.Count-1 do begin
          if j=MacroMenuItem.Count then
            MacroMenuItem.Add(TMenuItem.Create(Self));
          ValueMenuItem:=MacroMenuItem.Items[j];
          ValueMenuItem.Caption:=Format(fCaptionPatternMacroValue,[Macro.Values[j]]);
          ValueMenuItem.OnClick:=@OnAddMacroMenuItemClick;
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TCompOptModeMatrix.BMMNewIDEMacroMenuItemClick(Sender: TObject);
begin
  CreateNewOption(BuildMatrixOptionTypeCaption(bmotIDEMacro),BuildMatrixDefaultValue(bmotIDEMacro));
end;

procedure TCompOptModeMatrix.BMMNewOutDirMenuItemClick(Sender: TObject);
begin
  CreateNewOption(BuildMatrixOptionTypeCaption(bmotOutDir),BuildMatrixDefaultValue(bmotOutDir));
end;

procedure TCompOptModeMatrix.BMMMoveDownToolButtonClick(Sender: TObject);
begin
  MoveRow(1);
end;

procedure TCompOptModeMatrix.BMMDeleteToolButtonClick(Sender: TObject);
var
  aRow: Integer;
  MatRow: TGroupedMatrixRow;
begin
  aRow:=Grid.Row;
  if aRow<1 then exit;
  MatRow:=Grid.Matrix[aRow-1];
  if MatRow.Group=nil then begin
    // storage groups can not be deleted
    exit;
  end;
  Grid.DeleteMatrixRow(aRow);
  UpdateButtons;
end;

procedure TCompOptModeMatrix.UpdateButtons;
var
  aRow: Integer;
  MatRow: TGroupedMatrixRow;
begin
  aRow:=Grid.Row;
  if (aRow>0) and (aRow<=Grid.Matrix.RowCount) then begin
    MatRow:=Grid.Matrix[aRow-1];
  end else
    MatRow:=nil;

  // allow to delete targets and value rows
  BMMDeleteToolButton.Enabled:=(MatRow<>nil) and (MatRow.Group<>nil);
  //
  BMMUndoToolButton.Enabled:=Grid.CanUndo;
  BMMRedoToolButton.Enabled:=Grid.CanRedo;
  // move up/down
  BMMMoveUpToolButton.Enabled:=(MatRow<>nil) and (MatRow.Group<>nil)
                        and ((MatRow.GetPreviousSibling<>nil)
                           or (MatRow.GetTopLvlItem.GetPreviousSibling<>nil));
  BMMMoveDownToolButton.Enabled:=(MatRow<>nil) and (MatRow.Group<>nil)
                        and  (MatRow.GetNextSkipChildren<>nil);
end;

function TCompOptModeMatrix.AddTarget(StorageGroup: TGroupedMatrixGroup
  ): TGroupedMatrixGroup;
begin
  Result:=AddMatrixTarget(Grid.Matrix,StorageGroup);
end;

procedure TCompOptModeMatrix.CreateNewOption(aTyp, aValue: string);
var
  aRow: Integer;
  MatRow: TGroupedMatrixRow;
  Group: TGroupedMatrixGroup;
  NewRow: TGroupedMatrixValue;

  procedure CreateOption;
  begin
    if aTyp='' then
      aTyp:=Grid.TypeColumn.PickList.Names[0];
    NewRow:=Grid.Matrix.AddValue(Group,Grid.Modes[Grid.ActiveMode].Caption,aTyp,aValue);
  end;

begin
  aRow:=Grid.Row;
  if aRow<Grid.FixedRows then aRow:=Grid.FixedRows;
  NewRow:=nil;
  Grid.MatrixChanging;
  try
    Grid.StoreUndo;
    MatRow:=Grid.Matrix[aRow-1];
    if MatRow is TGroupedMatrixGroup then begin
      Group:=TGroupedMatrixGroup(MatRow);
      if Group.Group=nil then begin
        if Group.Count=0 then begin
          // storage group without target => add a target
          Group:=AddTarget(Group);
        end;
      end;
      // add option as first item of Group
      CreateOption;
    end else begin
      // add behind current value
      Group:=MatRow.Group;
      CreateOption;
      Group.Move(Group.Count-1,MatRow.GetGroupIndex+1);
    end;
    Grid.Matrix.RebuildRows;
  finally
    Grid.MatrixChanged;
  end;
  if NewRow<>nil then
    Grid.Row:=Grid.Matrix.IndexOfRow(NewRow)+1;
  UpdateButtons;
end;

procedure TCompOptModeMatrix.CreateNewTarget;
var
  aRow: Integer;
  MatRow: TGroupedMatrixRow;
  Group: TGroupedMatrixGroup;
  NewRow: TGroupedMatrixGroup;
begin
  aRow:=Grid.Row;
  if aRow<Grid.FixedRows then aRow:=Grid.FixedRows;
  NewRow:=nil;
  Grid.MatrixChanging;
  try
    Grid.StoreUndo;
    MatRow:=Grid.Matrix[aRow-1];
    if MatRow is TGroupedMatrixGroup then
      Group:=TGroupedMatrixGroup(MatRow)
    else
      Group:=MatRow.Group;
    if Group.Group=nil then begin
      // Group is a storage group
      // => add as first target of storage group
      NewRow:=AddTarget(Group);
      Group.Move(Group.Count-1,0);
    end else begin
      // Group is a target
      // => add target behind current target
      NewRow:=AddTarget(Group.Group);
      Group.Group.Move(Group.Group.Count-1,Group.GetGroupIndex+1);
    end;
    Grid.Matrix.RebuildRows;
  finally
    Grid.MatrixChanged;
  end;
  if NewRow<>nil then
    Grid.Row:=Grid.Matrix.IndexOfRow(NewRow)+1;
  UpdateButtons;
end;

function TCompOptModeMatrix.GetCaptionValue(aCaption, aPattern: string): string;
var
  p: SizeInt;
begin
  Result:='';
  p:=Pos('%s',aPattern);
  if p<1 then exit;
  Result:=copy(aCaption,p,length(aCaption)-length(aPattern)+2);
end;

procedure TCompOptModeMatrix.VisibleChanged;
begin
  inherited VisibleChanged;
  if (not Visible) and (LazProject<>nil) then
    DoWriteSettings;
end;

procedure TCompOptModeMatrix.UpdateModes(UpdateGrid: boolean);
var
  i: Integer;
  BuildMode: TProjectBuildMode;
  aColor: TColor;
  GridHasChanged: Boolean;
  aMode: TGroupedMatrixMode;
  BuildModes: TProjectBuildModes;
begin
  GridHasChanged:=false;
  // add/update build modes
  BuildModes:=LazProject.BuildModes;
  for i:=0 to BuildModes.Count-1 do begin
    BuildMode:=BuildModes[i];
    aColor:=clDefault;
    if BuildMode.InSession then aColor:=SessionColor;
    if i=Grid.Modes.Count then begin
      Grid.Modes.Add(BuildMode.Identifier,aColor);
      GridHasChanged:=true;
    end
    else begin
      aMode:=Grid.Modes[i];
      if aMode.Caption<>BuildMode.Identifier then begin
        aMode.Caption:=BuildMode.Identifier;
        GridHasChanged:=true;
      end;
      aMode.Color:=aColor;
    end;
  end;

  UpdateActiveMode;

  if UpdateGrid and GridHasChanged then
    Grid.MatrixChanged;
end;

procedure TCompOptModeMatrix.UpdateActiveMode;
var
  i: Integer;
begin
  if LazProject=nil then exit;
  i:=LazProject.BuildModes.IndexOf(LazProject.ActiveBuildMode);
  if (i<0) or (i>=Grid.Modes.Count) then exit;
  Grid.ActiveMode:=i;
end;

procedure TCompOptModeMatrix.MoveRow(Direction: integer);
var
  MatRow: TGroupedMatrixRow;
  aRow: Integer;
  TargetGroup: TGroupedMatrixGroup;
  i: Integer;
  TargetStorage: TGroupedMatrixGroup;
begin
  aRow:=Grid.Row;
  if aRow<1 then exit;
  MatRow:=Grid.Matrix[aRow-1];
  if MatRow.Group=nil then begin
    // storage groups can not be moved
    debugln(['TFrame1.MoveRow storage groups can not be moved']);
    exit;
  end;
  Grid.MatrixChanging;
  i:=MatRow.GetGroupIndex;
  if Direction<0 then begin
    if i>0 then begin
      // move up in group
      Grid.StoreUndo;
      MatRow.Group.Move(i,i-1);
    end else begin
      // move to previous group
      TargetGroup:=TGroupedMatrixGroup(MatRow.Group.GetPreviousSibling);
      if TargetGroup=nil then begin
        if MatRow is TGroupedMatrixValue then begin
          // move value to last target of previous storage
          if (MatRow.Group.Group=nil) then begin
            debugln(['TFrame1.MoveRow value has no storage+target']);
            exit;
          end;
          TargetStorage:=TGroupedMatrixGroup(MatRow.Group.Group.GetPreviousSibling);
          if TargetStorage=nil then begin
            debugln(['TFrame1.MoveRow no previous storage for value']);
            exit;
          end;
          if TargetStorage.Count>0 then begin
            TargetGroup:=TargetStorage[TargetStorage.Count-1] as TGroupedMatrixGroup;
          end else begin
            // add first target
            TargetGroup:=AddTarget(TargetStorage);
          end;
        end else begin
          // this is already the first target of the first storage
          debugln(['TFrame1.MoveRow no previous storage for target']);
          exit;
        end;
      end;
      // move MatRow to TargetGroup as last
      Grid.StoreUndo;
      MatRow.Group:=TargetGroup;
    end;
  end else begin
    if i+1<MatRow.Group.Count then begin
      // move down in group
      Grid.StoreUndo;
      MatRow.Group.Move(i,i+1);
    end else begin
      // move to next group
      TargetGroup:=TGroupedMatrixGroup(MatRow.Group.GetNextSibling);
      if TargetGroup=nil then begin
        if MatRow is TGroupedMatrixValue then begin
          // move value to first target of next storage
          if (MatRow.Group.Group=nil) then begin
            debugln(['TFrame1.MoveRow value has no storage+target']);
            exit;
          end;
          TargetStorage:=TGroupedMatrixGroup(MatRow.Group.Group.GetNextSibling);
          if TargetStorage=nil then begin
            debugln(['TFrame1.MoveRow no next storage for value']);
            exit;
          end;
          if TargetStorage.Count>0 then begin
            TargetGroup:=TargetStorage[0] as TGroupedMatrixGroup;
          end else begin
            // add first target
            TargetGroup:=AddTarget(TargetStorage);
          end;
        end else begin
          // this is already the last target of the last storage
          debugln(['TFrame1.MoveRow no next storage for target']);
          exit;
        end;
      end;
      // move MatRow to TargetGroup as first
      Grid.StoreUndo;
      MatRow.Group:=TargetGroup;
      TargetGroup.Move(TargetGroup.Count-1,0);
    end;
  end;
  Grid.Matrix.RebuildRows;
  Grid.MatrixChanged;
  Grid.Row:=Grid.Matrix.IndexOfRow(MatRow)+1;
  UpdateButtons;
end;

procedure TCompOptModeMatrix.DoWriteSettings;
begin
  // write IDE options
  AssignBuildMatrixGroupToOptions(GroupIDE,
    EnvironmentOptions.BuildMatrixOptions, true);
  // write Project options
  AssignBuildMatrixGroupToOptions(GroupProject,
    LazProject.BuildModes.SharedMatrixOptions, true);
  // write Session options
  AssignBuildMatrixGroupToOptions(GroupSession,
    LazProject.BuildModes.SessionMatrixOptions, true);
end;

constructor TCompOptModeMatrix.Create(TheOwner: TComponent);
var
  t: TBuildMatrixOptionType;
begin
  inherited Create(TheOwner);
  ModeMatrixFrame:=Self;

  fOldIDEOptions:=TBuildMatrixOptions.Create;
  fOldSharedOptions:=TBuildMatrixOptions.Create;
  fOldSessionOptions:=TBuildMatrixOptions.Create;

  IDEColor:=RGBToColor(200,255,255);
  ProjectColor:=RGBToColor(255,255,255);
  SessionColor:=RGBToColor(255,255,200);
  ErrorColor:=RGBToColor(255,128,128);

  FGrid:=TGroupedMatrixControl.Create(Self);
  with Grid do begin
    Name:='ModeMatrixControl';
    Align:=alClient;
    for t:=low(TBuildMatrixOptionType) to high(TBuildMatrixOptionType) do
      TypeColumn.PickList.Add(BuildMatrixOptionTypeCaption(t)+'='+BuildMatrixOptionTypeHint(t));
    Parent:=Self;
    OnSelection:=@GridSelection;
    OnEditingDone:=@GridEditingDone;
    ShowHint:=true;
    OnShowHint:=@GridShowHint;
    OnGetCellHightlightColor:=@GridGetCellHightlightColor;
    OnSetEditText:=@GridSetEditText;
  end;

  fGroupIDE:=Grid.Matrix.AddGroup(nil, lisMMStoredInIDEEnvironmentoptionsXml);
  GroupIDE.Color:=IDEColor;

  fGroupProject:=Grid.Matrix.AddGroup(nil, lisMMStoredInProjectLpi);
  GroupProject.Color:=ProjectColor;

  fGroupSession:=Grid.Matrix.AddGroup(nil, lisMMStoredInSessionOfProjectLps);
  GroupSession.Color:=SessionColor;

  BMMatrixToolBar.Images:=IDEImages.Images_16;

  BMMMoveUpToolButton.ShowCaption:=false;
  BMMMoveUpToolButton.ImageIndex:=IDEImages.LoadImage(16,'arrow_up');
  BMMMoveUpToolButton.Hint:=lisMMMoveSelectedItemUp;

  BMMMoveDownToolButton.ShowCaption:=false;
  BMMMoveDownToolButton.ImageIndex:=IDEImages.LoadImage(16,'arrow_down');
  BMMMoveDownToolButton.Hint:=lisMMMoveSelectedItemDown;

  BMMUndoToolButton.Caption:=lisUndo;
  BMMUndoToolButton.Hint:=lisMMUndoLastChangeToThisGrid;

  BMMRedoToolButton.Caption:=lisRedo;
  BMMRedoToolButton.Hint:=lisMMRedoLastUndoToThisGrid;

  BMMAddToolButton.Caption:=lisAdd;

  BMMDeleteToolButton.Caption:=lisDelete;
  BMMDeleteToolButton.Hint:=lisMMDeleteTheSelectedTargetOrOption;

  BMMNewTargetMenuItem.Caption:=lisMMNewTarget;
  BMMNewTargetMenuItem.Hint:=lisMMCreateANewGroupOfOptions;

  BMMNewCustomOptionMenuItem.Caption:=lisMMCustomOption;
  BMMNewIDEMacroMenuItem.Caption:=lisMMIDEMacro;
  BMMNewOutDirMenuItem.Caption:=lisMMOutputDirectory;

  fCaptionPatternMacroName:=lisMMSetS;
  fCaptionPatternMacroValue:=lisMMValueS;

  UpdateButtons;
end;

destructor TCompOptModeMatrix.Destroy;
begin
  ModeMatrixFrame:=nil;
  FreeAndNil(fOldIDEOptions);
  FreeAndNil(fOldSharedOptions);
  FreeAndNil(fOldSessionOptions);
  inherited Destroy;
end;

function TCompOptModeMatrix.GetTitle: String;
begin
  Result:=lisMMModeMatrix;
end;

procedure TCompOptModeMatrix.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  //debugln(['TCompOptModeMatrix.Setup ',DbgSName(ADialog)]);

end;

class function TCompOptModeMatrix.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectCompilerOptions;
end;

procedure TCompOptModeMatrix.ReadSettings(AOptions: TAbstractIDEOptions);
var
  CompOptions: TProjectCompilerOptions;
begin
  //debugln(['TCompOptModeMatrix.ReadSettings ',DbgSName(AOptions)]);
  if not (AOptions is TProjectCompilerOptions) then exit;
  CompOptions:=TProjectCompilerOptions(AOptions);
  if FProject=CompOptions.LazProject then begin
    // options already loaded, only active compiler options are reloaded
    UpdateActiveMode;
    exit;
  end;

  fProject:=CompOptions.LazProject;

  UpdateModes(false);

  // read IDE options
  AssignBuildMatrixOptionsToGroup(EnvironmentOptions.BuildMatrixOptions,
    Grid.Matrix,GroupIDE);
  fOldIDEOptions.Assign(EnvironmentOptions.BuildMatrixOptions);
  // read Project options
  AssignBuildMatrixOptionsToGroup(LazProject.BuildModes.SharedMatrixOptions,
    Grid.Matrix,GroupProject);
  fOldSharedOptions.Assign(LazProject.BuildModes.SharedMatrixOptions);
  // read Session options
  AssignBuildMatrixOptionsToGroup(LazProject.BuildModes.SessionMatrixOptions,
    Grid.Matrix,GroupSession);
  fOldSessionOptions.Assign(LazProject.BuildModes.SessionMatrixOptions);

  // update Grid
  Grid.MatrixChanged;

  // select project
  Grid.Row:=Grid.Matrix.IndexOfRow(GroupProject)+1;
  Grid.Col:=Grid.FixedCols;
end;

procedure TCompOptModeMatrix.WriteSettings(AOptions: TAbstractIDEOptions);
var
  CompOptions: TProjectCompilerOptions;
begin
  if not (AOptions is TProjectCompilerOptions) then exit;
  CompOptions:=TProjectCompilerOptions(AOptions);
  fProject:=CompOptions.LazProject;

  DoWriteSettings;
end;

procedure TCompOptModeMatrix.RestoreSettings(AOptions: TAbstractIDEOptions);
var
  CompOptions: TProjectCompilerOptions;
begin
  if not (AOptions is TProjectCompilerOptions) then exit;
  CompOptions:=TProjectCompilerOptions(AOptions);
  fProject:=CompOptions.LazProject;

  // write IDE options
  EnvironmentOptions.BuildMatrixOptions.Assign(fOldIDEOptions);
  // write Project options
  LazProject.BuildModes.SharedMatrixOptions.Assign(fOldSharedOptions);
  // write Session options
  LazProject.BuildModes.SessionMatrixOptions.Assign(fOldSessionOptions);

  IncreaseCompilerParseStamp;
end;

initialization
  {$IFDEF EnableModeMatrix}
  RegisterIDEOptionsEditor(GroupCompiler, TCompOptModeMatrix,
    CompilerOptionsModeMatrix);
  {$ENDIF}

end.

