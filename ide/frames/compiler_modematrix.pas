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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

 Author: Mattias Gaertner

 Abstract:
   Options frame for build mode matrix options.

 ToDo:
   - editor for targets
   - show pick list icon for type column
   - undo: combine changes while editing a cell
}
unit Compiler_ModeMatrix;

{$mode objfpc}{$H+}

interface

uses
  //Classes, SysUtils, types, LazLoggerBase, LazUTF8, Controls, Graphics, ComCtrls,
  //Menus, LCLProc, IDEOptionsIntf, IDEImagesIntf, CompOptsIntf, EnvironmentOpts,
  //PackageSystem, PackageDefs, Project, LazarusIDEStrConsts, TransferMacros,
  //ModeMatrixOpts, ModeMatrixCtrl, compiler_config_target;
  Classes, SysUtils, types,
  // LazUtils
  LazLoggerBase, LazUTF8,
  // LCL
  LCLProc, Controls, Graphics, ComCtrls, Menus,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf, IDEImagesIntf, CompOptsIntf,
  // IDE
  EnvironmentOpts, PackageSystem, PackageDefs, Project, LazarusIDEStrConsts,
  TransferMacros, ModeMatrixOpts, ModeMatrixCtrl, compiler_config_target;

type

  { TCompOptModeMatrixFrame }

  TCompOptModeMatrixFrame = class(TAbstractIDEOptionsEditor)
    BMMAddLclWidgetPopupMenu: TPopupMenu;
    BMMatrixToolBar: TToolBar;
    BMMDeleteButton: TToolButton;
    BMMMoveDownButton: TToolButton;
    BMMMoveUpButton: TToolButton;
    BMMLCLWidgetTypeMenuItem: TMenuItem;
    BMMRedoToolButton: TToolButton;
    BMMUndoButton: TToolButton;
    BMMNewCustomOptionMenuItem: TMenuItem;
    BMMAddOtherPopupMenu: TPopupMenu;
    BMMNewTargetMenuItem: TMenuItem;
    BMMAddOtherButton: TToolButton;
    BMMNewIDEMacroMenuItem: TMenuItem;
    BMMNewOutDirMenuItem: TMenuItem;
    MoveSepToolButton: TToolButton;
    DoSepToolButton: TToolButton;
    AddSepToolButton: TToolButton;
    BMMAddLclWidgetButton: TToolButton;
    LCLMacroSepToolButton: TToolButton;
    BMMSystemEncodingButton: TToolButton;
    DeleteSepToolButton: TToolButton;
    procedure BMMDeleteButtonClick(Sender: TObject);
    procedure BMMMoveDownButtonClick(Sender: TObject);
    procedure BMMMoveUpButtonClick(Sender: TObject);
    procedure BMMNewIDEMacroMenuItemClick(Sender: TObject);
    procedure BMMNewOutDirMenuItemClick(Sender: TObject);
    procedure BMMRedoToolButtonClick(Sender: TObject);
    procedure BMMUndoButtonClick(Sender: TObject);
    procedure BMMNewCustomOptionMenuItemClick(Sender: TObject);
    procedure BMMNewTargetMenuItemClick(Sender: TObject);
    procedure BMMAddLclWidgetButtonClick(Sender: TObject);
    procedure BMMAddOtherButtonClick(Sender: TObject);
    procedure GridEditingDone(Sender: TObject);
    procedure GridGetCellHightlightColor(Sender: TObject; aCol, aRow: integer;
      var aColor: TColor);
    procedure GridSelection(Sender: TObject; {%H-}aCol, {%H-}aRow: Integer);
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
    procedure GridShowHint(Sender: TObject; HintInfo: PHintInfo);
    procedure OnAddMacroMenuItemClick(Sender: TObject);
    procedure OnAddLCLWidgetTypeClick(Sender: TObject);
    procedure BMMSystemEncodingButtonClick(Sender: TObject);
  private
    FDialog: TAbstractOptionsEditorDialog;
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
    procedure AddLCLWidgetTypeValues(ParentMenu: TPopupMenu; Mcr: TLazBuildMacro);
    procedure AddMacroValues(ParentMI: TMenuItem; Mcr: TLazBuildMacro);
    function ActiveModeAsText: string;
    function CheckAndUpdateSystemEncoding(UpdateIt: Boolean; out WasUpdated: Boolean): Boolean;
    function HasSystemEncoding: Boolean;
    function SupportSystemEncoding: Boolean;
    procedure DoWriteSettings;
    procedure FillMenus;
    procedure MoveRow(Direction: integer);
    procedure UpdateButtons;
    function AddTarget(StorageGroup: TGroupedMatrixGroup): TGroupedMatrixGroup;
    procedure CreateNewOption(aTyp, aValue: string);
    procedure CreateNewTarget;
    function GetCaptionValue(aCaption, aPattern: string): string;
    procedure UpdateEnabledModesInGrid(Options: TBuildMatrixOptions;
                       StorageGroup: TGroupedMatrixGroup; var HasChanged: boolean);
    procedure UpdateGridStorageGroups;
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
  ModeMatrixFrame: TCompOptModeMatrixFrame = nil;

implementation

const
  DisableUTF8RTL = '-dDisableUTF8RTL';

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

function IsEqual(Options: TBuildMatrixOptions; StorageGroup: TGroupedMatrixGroup): boolean;
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
      //debugln(['IsEqual ',Option.AsString,' Targets="',Option.Targets,'" Target.Value="',Target.Value,'"']);
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
    if (TargetGrp=nil) or (TargetGrp.Value<>Option.Targets) then begin
      TargetGrp:=AddMatrixTarget(Matrix,StorageGroup);
      TargetGrp.Value:=Option.Targets;
    end;
    Value:=Option.Value;
    if Option.Typ=bmotIDEMacro then
      Value:=Option.MacroName+':='+Value;
    Matrix.AddValue(TargetGrp,Option.Modes,
                    BuildMatrixOptionTypeCaption(Option.Typ),Value,Option.ID);
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
  Targets: String;
begin
  if IsEqual(Options,StorageGroup) then exit;
  Options.Clear;
  for GrpIndex:=0 to StorageGroup.Count-1 do begin
    Target:=TGroupedMatrixGroup(StorageGroup[GrpIndex]);
    if not (Target is TGroupedMatrixGroup) then begin
      debugln(['AssignBuildMatrixGroupToOptions StorageGroup "',StorageGroup.AsString,'", expected group, but found ',DbgSName(Target)]);
      exit;
    end;
    Targets:=UTF8Trim(Target.Value);
    //debugln(['AssignBuildMatrixGroupToOptions Targets=',Targets]);
    for i:=0 to Target.Count-1 do begin
      ValueRow:=TGroupedMatrixValue(Target[i]);
      if not (ValueRow is TGroupedMatrixValue) then begin
        debugln(['AssignBuildMatrixGroupToOptions Target expected Value, but found ',DbgSName(ValueRow)]);
        exit;
      end;
      Option:=Options.Add(CaptionToBuildMatrixOptionType(ValueRow.Typ),Targets);
      Option.Modes:=ValueRow.GetNormalizedModes;
      Option.ID:=ValueRow.ID;
      if Option.Typ=bmotIDEMacro then begin
        SplitMatrixMacro(ValueRow.Value,MacroName,MacroValue,false);
        Option.MacroName:=MacroName;
        Option.Value:=UTF8Trim(MacroValue);
        //debugln(['AssignBuildMatrixGroupToOptions Name="',MacroName,'" Value="',MacroValue,'"']);
      end else begin
        Option.Value:=UTF8Trim(ValueRow.Value);
      end;
      //debugln(['AssignBuildMatrixGroupToOptions Option=',Option.AsString]);
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

{ TCompOptModeMatrixFrame }

procedure TCompOptModeMatrixFrame.GridSelection(Sender: TObject; aCol, aRow: Integer);
begin
  UpdateButtons;
end;

procedure TCompOptModeMatrixFrame.GridSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
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
      end;
      Grid.InvalidateCell(Grid.ValueCol,ARow);
    end;
  end;
end;

procedure TCompOptModeMatrixFrame.GridShowHint(Sender: TObject; HintInfo: PHintInfo);
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

procedure TCompOptModeMatrixFrame.OnAddMacroMenuItemClick(Sender: TObject);
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

procedure TCompOptModeMatrixFrame.OnAddLCLWidgetTypeClick(Sender: TObject);
var
  TargetFrame: TCompilerConfigTargetFrame;
  ValueMenuItem: TMenuItem;
  Value: String;
begin
  ValueMenuItem:=Sender as TMenuItem;
  Value:=GetCaptionValue(ValueMenuItem.Caption,fCaptionPatternMacroValue);
  CreateNewOption(BuildMatrixOptionTypeCaption(bmotIDEMacro),'LCLWidgetType:='+Value);
  // Update LCLWidgetType to Config and Target page. ToDo: update also when deleting or changing.
  TargetFrame := TCompilerConfigTargetFrame(FDialog.FindEditor(TCompilerConfigTargetFrame));
  Assert(Assigned(TargetFrame));
  TargetFrame.UpdateWidgetSet(Value);
end;

procedure TCompOptModeMatrixFrame.BMMNewCustomOptionMenuItemClick(Sender: TObject);
begin
  CreateNewOption(BuildMatrixOptionTypeCaption(bmotCustom),BuildMatrixDefaultValue(bmotCustom));
end;

procedure TCompOptModeMatrixFrame.BMMUndoButtonClick(Sender: TObject);
begin
  Grid.Undo;
  UpdateGridStorageGroups;
  UpdateButtons;
end;

procedure TCompOptModeMatrixFrame.BMMNewTargetMenuItemClick(Sender: TObject);
begin
  CreateNewTarget;
end;

procedure TCompOptModeMatrixFrame.BMMAddLclWidgetButtonClick(Sender: TObject);
var
  p: TPoint;
begin
  p:=BMMAddLclWidgetButton.ClientToScreen(Point(0,BMMAddLclWidgetButton.Height));
  BMMAddLclWidgetPopupMenu.PopUp(p.x,p.y);
end;

procedure TCompOptModeMatrixFrame.BMMAddOtherButtonClick(Sender: TObject);
var
  p: TPoint;
begin
  p:=BMMAddOtherButton.ClientToScreen(Point(0,BMMAddOtherButton.Height));
  BMMAddOtherPopupMenu.PopUp(p.x,p.y);
end;

procedure TCompOptModeMatrixFrame.BMMSystemEncodingButtonClick(Sender: TObject);
begin
  SupportSystemEncoding;
end;

procedure TCompOptModeMatrixFrame.GridEditingDone(Sender: TObject);
begin
  //DebugLn(['TFrame1.GridEditingDone ']);
  UpdateButtons;
end;

procedure TCompOptModeMatrixFrame.GridGetCellHightlightColor(Sender: TObject; aCol,
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

procedure TCompOptModeMatrixFrame.BMMRedoToolButtonClick(Sender: TObject);
begin
  Grid.Redo;
  UpdateGridStorageGroups;
  UpdateButtons;
end;

procedure TCompOptModeMatrixFrame.BMMMoveUpButtonClick(Sender: TObject);
begin
  MoveRow(-1);
end;

procedure TCompOptModeMatrixFrame.AddLCLWidgetTypeValues(ParentMenu: TPopupMenu; Mcr: TLazBuildMacro);
var
  i: Integer;
  ValueMI: TMenuItem;
begin
  if Mcr.Values<>nil then begin
    for i:=0 to Mcr.Values.Count-1 do begin
      if i=ParentMenu.Items.Count then
        ParentMenu.Items.Add(TMenuItem.Create(Self));
      ValueMI:=ParentMenu.Items[i];
      ValueMI.Caption:=Format(fCaptionPatternMacroValue,[Mcr.Values[i]]);
      ValueMI.OnClick:=@OnAddLCLWidgetTypeClick;
    end;
  end;
end;

procedure TCompOptModeMatrixFrame.AddMacroValues(ParentMI: TMenuItem; Mcr: TLazBuildMacro);
var
  i: Integer;
  ValueMI: TMenuItem;
begin
  if Mcr.Values<>nil then begin
    for i:=0 to Mcr.Values.Count-1 do begin
      if i=ParentMI.Count then
        ParentMI.Add(TMenuItem.Create(Self));
      ValueMI:=ParentMI.Items[i];
      ValueMI.Caption:=Format(fCaptionPatternMacroValue,[Mcr.Values[i]]);
      ValueMI.OnClick:=@OnAddMacroMenuItemClick;
    end;
  end;
end;

procedure TCompOptModeMatrixFrame.FillMenus;
var
  i, j: Integer;
  Pkg: TLazPackage;
  Macros: TLazBuildMacros;
  Macro: TLazBuildMacro;
  LCLWidgetTypeMacro: TLazBuildMacro;
  List: TStringList;
  MenuIndex: Integer;
  MacroMenuItem: TMenuItem;
  PkgList: TFPList;
begin
  LCLWidgetTypeMacro:=Nil;
  PkgList:=nil;
  List:=TStringList.Create;
  try
    // First collect all macros from all used packages to a sorted list.
    PackageGraph.GetAllRequiredPackages(nil,LazProject.FirstRequiredDependency,PkgList);
    if PkgList<>nil then begin
      for i:=0 to PkgList.Count-1 do begin
        Pkg:=TLazPackage(PkgList[i]);
        Macros:=Pkg.CompilerOptions.BuildMacros;
        for j:=0 to Macros.Count-1 do begin
          Macro:=Macros[j];
          if Macro.Identifier = 'LCLWidgetType' then
            LCLWidgetTypeMacro:=Macro
          else if IsValidIdent(Macro.Identifier) then
            List.AddObject(Macro.Identifier,Macro);
        end;
      end;
    end;
    List.Sort;
    // LCLWidgetType gets its own button.
    BMMAddLclWidgetButton.Enabled:=Assigned(LCLWidgetTypeMacro);
    if Assigned(LCLWidgetTypeMacro) then
      AddLCLWidgetTypeValues(BMMAddLclWidgetPopupMenu, LCLWidgetTypeMacro);
    // Place other macros to the popup menu opened from "Add" button.
    MenuIndex:=BMMNewTargetMenuItem.MenuIndex;
    for i:=0 to List.Count-1 do begin
      inc(MenuIndex);
      Macro:=TLazBuildMacro(List.Objects[i]);
      if BMMAddOtherPopupMenu.Items.Count=MenuIndex then
        BMMAddOtherPopupMenu.Items.Add(TMenuItem.Create(Self));
      MacroMenuItem:=BMMAddOtherPopupMenu.Items[MenuIndex];
      MacroMenuItem.Caption:=Format(fCaptionPatternMacroName,[Macro.Identifier]);
      AddMacroValues(MacroMenuItem, Macro);
    end;
  finally
    PkgList.Free;
    List.Free;
  end;
end;

procedure TCompOptModeMatrixFrame.BMMNewIDEMacroMenuItemClick(Sender: TObject);
begin
  CreateNewOption(BuildMatrixOptionTypeCaption(bmotIDEMacro),BuildMatrixDefaultValue(bmotIDEMacro));
end;

procedure TCompOptModeMatrixFrame.BMMNewOutDirMenuItemClick(Sender: TObject);
begin
  CreateNewOption(BuildMatrixOptionTypeCaption(bmotOutDir),BuildMatrixDefaultValue(bmotOutDir));
end;

procedure TCompOptModeMatrixFrame.BMMMoveDownButtonClick(Sender: TObject);
begin
  MoveRow(1);
end;

procedure TCompOptModeMatrixFrame.BMMDeleteButtonClick(Sender: TObject);
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

procedure TCompOptModeMatrixFrame.UpdateButtons;
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
  BMMDeleteButton.Enabled:=(MatRow<>nil) and (MatRow.Group<>nil);
  //
  BMMUndoButton.Enabled:=Grid.CanUndo;
  BMMRedoToolButton.Enabled:=Grid.CanRedo;
  // move up/down
  BMMMoveUpButton.Enabled:=(MatRow<>nil) and (MatRow.Group<>nil)
                        and ((MatRow.GetPreviousSibling<>nil)
                           or (MatRow.GetTopLvlItem.GetPreviousSibling<>nil));
  BMMMoveDownButton.Enabled:=(MatRow<>nil) and (MatRow.Group<>nil)
                        and  (MatRow.GetNextSkipChildren<>nil);
end;

function TCompOptModeMatrixFrame.AddTarget(StorageGroup: TGroupedMatrixGroup
  ): TGroupedMatrixGroup;
begin
  Result:=AddMatrixTarget(Grid.Matrix,StorageGroup);
end;

function TCompOptModeMatrixFrame.ActiveModeAsText: string;
begin
  Result:=Grid.Modes[Grid.ActiveMode].Caption;
end;

procedure TCompOptModeMatrixFrame.CreateNewOption(aTyp, aValue: string);
var
  aRow: Integer;
  MatRow: TGroupedMatrixRow;
  Group: TGroupedMatrixGroup;
  NewRow: TGroupedMatrixValue;
begin
  aRow:=Grid.Row;
  if aRow<Grid.FixedRows then aRow:=Grid.FixedRows;
  NewRow:=nil;
  Grid.MatrixChanging;
  try
    Grid.StoreUndo;
    if aTyp='' then
      aTyp:=Grid.TypeColumn.PickList.Names[0];
    MatRow:=Grid.Matrix[aRow-1];
    //debugln(['TCompOptModeMatrix.CreateNewOption ',DbgSName(MatRow),' ',MatRow.AsString]);
    if MatRow is TGroupedMatrixGroup then begin
      Group:=TGroupedMatrixGroup(MatRow);
      if Group.Group=nil then begin
        if Group.Count=0 then begin
          // storage group without target => add a target
          Group:=AddTarget(Group);
        end else begin
          // add to first target
          Group:=Group[0] as TGroupedMatrixGroup;
        end;
      end;
      // add option as first item of Group
      NewRow:=Grid.Matrix.AddValue(Group, ActiveModeAsText, aTyp, aValue,
                                   CreateBuildMatrixOptionGUID);
    end else begin
      // add behind current value
      Group:=MatRow.Group;
      NewRow:=Grid.Matrix.AddValue(Group, ActiveModeAsText, aTyp, aValue,
                                   CreateBuildMatrixOptionGUID);
      Group.Move(Group.Count-1,MatRow.GetGroupIndex+1);
    end;
    Grid.Matrix.RebuildRows;
    //Grid.Matrix.WriteDebugReport;
  finally
    Grid.MatrixChanged;
  end;
  if NewRow<>nil then
    Grid.Row:=Grid.Matrix.IndexOfRow(NewRow)+1;
  UpdateButtons;
end;

procedure TCompOptModeMatrixFrame.CreateNewTarget;
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

function TCompOptModeMatrixFrame.GetCaptionValue(aCaption, aPattern: string): string;
var
  p: SizeInt;
begin
  Result:='';
  p:=Pos('%s',aPattern);
  if p<1 then exit;
  Result:=copy(aCaption,p,length(aCaption)-length(aPattern)+2);
end;

procedure TCompOptModeMatrixFrame.UpdateEnabledModesInGrid(
  Options: TBuildMatrixOptions; StorageGroup: TGroupedMatrixGroup;
  var HasChanged: boolean);
// update enabled modes in grid
var
  GrpIndex: Integer;
  Target: TGroupedMatrixGroup;
  i: Integer;
  ValueRow: TGroupedMatrixValue;
  OptionIndex: Integer;
  Option: TBuildMatrixOption;
begin
  OptionIndex:=0;
  for GrpIndex:=0 to StorageGroup.Count-1 do begin
    Target:=TGroupedMatrixGroup(StorageGroup[GrpIndex]);
    if not (Target is TGroupedMatrixGroup) then
      exit;
    //debugln(['TCompOptModeMatrix.UpdateEnabledModesInGrid Target=',Target.AsString]);
    for i:=0 to Target.Count-1 do begin
      ValueRow:=TGroupedMatrixValue(Target[i]);
      if not (ValueRow is TGroupedMatrixValue) then
        exit;
      //debugln(['TCompOptModeMatrix.UpdateEnabledModesInGrid ValueRow=',ValueRow.AsString]);
      if OptionIndex>=Options.Count then exit;
      Option:=Options[OptionIndex];
      //debugln(['TCompOptModeMatrix.UpdateEnabledModesInGrid Option.Modes="',dbgstr(Option.Modes),'" ValueRow.GetNormalizedModes="',dbgstr(ValueRow.GetNormalizedModes),'"']);
      if Option.Modes<>ValueRow.GetNormalizedModes then begin
        HasChanged:=true;
        ValueRow.ModeList.Text:=Option.Modes;
      end;
      inc(OptionIndex);
    end;
  end;
end;

procedure TCompOptModeMatrixFrame.UpdateGridStorageGroups;
var
  i, j: Integer;
  MatRow: TGroupedMatrixRow;
  GroupRow: TGroupedMatrixGroup;
begin
  fGroupIDE:=nil;
  fGroupProject:=nil;
  fGroupSession:=nil;
  j:=0;
  for i:=0 to Grid.Matrix.RowCount-1 do begin
    MatRow:=Grid.Matrix.Rows[i];
    if (MatRow.Group=nil) and (MatRow is TGroupedMatrixGroup) then begin
      GroupRow:=TGroupedMatrixGroup(MatRow);
      inc(j);
      case j of
      1: fGroupIDE:=GroupRow;
      2: fGroupProject:=GroupRow;
      3: fGroupSession:=GroupRow;
      end;
    end;
  end;
  if fGroupSession=nil then
    raise Exception.Create('grid lost the session storage group');
end;

procedure TCompOptModeMatrixFrame.VisibleChanged;
begin
  inherited VisibleChanged;
  if (not Visible) and (LazProject<>nil) then
    DoWriteSettings;
end;

function TCompOptModeMatrixFrame.CheckAndUpdateSystemEncoding(UpdateIt: Boolean;
  out WasUpdated: Boolean): Boolean;
// Returns True if the support already was there.
var
  GrpIndex: Integer;
  Target: TGroupedMatrixGroup;
  i: Integer;
  ValueRow: TGroupedMatrixValue;
  AMode: String;
begin
  Result := False;
  WasUpdated := False;
  for GrpIndex:=0 to GroupProject.Count-1 do
  begin
    Target := TGroupedMatrixGroup(GroupProject[GrpIndex]);
    if not (Target is TGroupedMatrixGroup) then
      exit;
    for i:=0 to Target.Count-1 do
    begin
      ValueRow := TGroupedMatrixValue(Target[i]);
      if not (ValueRow is TGroupedMatrixValue) then
        exit;
      Result := (ValueRow.Typ = 'Custom') and (ValueRow.Value = DisableUTF8RTL);
      if Result then
      begin
        AMode := ActiveModeAsText;
        Result := (ValueRow.ModeList.IndexOf(AMode)>=0);
        if (not Result) and UpdateIt then
        begin
          Grid.MatrixChanging;
          try
            ValueRow.ModeList.Add(AMode);
          finally
            Grid.MatrixChanged;
          end;
          WasUpdated := True;
        end;
        Exit;
      end;
    end;
  end;
end;

function TCompOptModeMatrixFrame.HasSystemEncoding: Boolean;
var
  Dummy: Boolean;
begin
  Result := CheckAndUpdateSystemEncoding(False, Dummy);
end;

function TCompOptModeMatrixFrame.SupportSystemEncoding: Boolean;
// Add a compiler flag to use system encoding for string.
// Returns true if the flag was really added and did not exist earlier.
var
  WasUpdated: Boolean;
begin
  Result := not CheckAndUpdateSystemEncoding(True, WasUpdated);
  if Result and not WasUpdated then
  begin
    CreateNewOption(BuildMatrixOptionTypeCaption(bmotCustom), DisableUTF8RTL);
    UpdateModes;
  end;
end;

procedure TCompOptModeMatrixFrame.UpdateModes(UpdateGrid: boolean);
var
  i: Integer;
  aColor: TColor;
  GridHasChanged: Boolean;
  ValuesHaveChanged: Boolean;
  aMode: TGroupedMatrixMode;
  BuildMode: TProjectBuildMode;
  BuildModes: TProjectBuildModes;
begin
  GridHasChanged:=false;
  ValuesHaveChanged:=false;

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
      //debugln(['TCompOptModeMatrix.UpdateModes aMode.Caption=',aMode.Caption,' BuildMode.Identifier=',BuildMode.Identifier]);
      if aMode.Caption<>BuildMode.Identifier then begin
        aMode.Caption:=BuildMode.Identifier;
        GridHasChanged:=true;
      end;
      if aMode.Color<>aColor then begin
        ValuesHaveChanged:=true;
        aMode.Color:=aColor;
      end;
    end;
  end;
  // delete leftover build modes
  while Grid.Modes.Count>BuildModes.Count do begin
    Grid.Modes.Delete(Grid.Modes.Count-1);
    GridHasChanged:=true;
  end;

  UpdateEnabledModesInGrid(EnvironmentOptions.BuildMatrixOptions,GroupIDE,ValuesHaveChanged);
  UpdateEnabledModesInGrid(LazProject.BuildModes.SharedMatrixOptions,GroupProject,ValuesHaveChanged);
  UpdateEnabledModesInGrid(LazProject.BuildModes.SessionMatrixOptions,GroupSession,ValuesHaveChanged);

  UpdateActiveMode;

  //debugln(['TCompOptModeMatrix.UpdateModes UpdateGrid=',UpdateGrid,' GridHasChanged=',GridHasChanged]);
  if UpdateGrid and GridHasChanged then
    Grid.MatrixChanged
  else if GridHasChanged or ValuesHaveChanged then
    Grid.Invalidate;
end;

procedure TCompOptModeMatrixFrame.UpdateActiveMode;
var
  i: Integer;
begin
  i:=LazProject.BuildModes.IndexOf(LazProject.ActiveBuildMode);
  if i>=Grid.Modes.Count then exit;
  Grid.ActiveMode:=i;
end;

procedure TCompOptModeMatrixFrame.MoveRow(Direction: integer);
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

procedure TCompOptModeMatrixFrame.DoWriteSettings;
begin
  // write IDE options
  AssignBuildMatrixGroupToOptions(GroupIDE,
    EnvironmentOptions.BuildMatrixOptions,true);
  // write Project options
  AssignBuildMatrixGroupToOptions(GroupProject,
    LazProject.BuildModes.SharedMatrixOptions,true);
  // write Session options
  AssignBuildMatrixGroupToOptions(GroupSession,
    LazProject.BuildModes.SessionMatrixOptions,true);
end;

constructor TCompOptModeMatrixFrame.Create(TheOwner: TComponent);
var
  t: TBuildMatrixOptionType;
begin
  inherited Create(TheOwner);
  ModeMatrixFrame:=Self;

  fOldIDEOptions:=TBuildMatrixOptions.Create;
  fOldSharedOptions:=TBuildMatrixOptions.Create;
  fOldSessionOptions:=TBuildMatrixOptions.Create;

  IDEColor:=RGBToColor(255,255,255);
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

  BMMMoveUpButton.ShowCaption:=false;
  BMMMoveUpButton.ImageIndex:=IDEImages.LoadImage('arrow_up');
  BMMMoveUpButton.Hint:=lisMMMoveSelectedItemUp;

  BMMMoveDownButton.ShowCaption:=false;
  BMMMoveDownButton.ImageIndex:=IDEImages.LoadImage('arrow_down');
  BMMMoveDownButton.Hint:=lisMMMoveSelectedItemDown;

  BMMUndoButton.ShowCaption:=false;
  BMMUndoButton.ImageIndex:=IDEImages.LoadImage('menu_undo');
  BMMUndoButton.Hint:=lisMMUndoLastChangeToThisGrid;

  BMMRedoToolButton.ShowCaption:=false;
  BMMRedoToolButton.ImageIndex:=IDEImages.LoadImage('menu_redo');
  BMMRedoToolButton.Hint:=lisMMRedoLastUndoToThisGrid;

  BMMDeleteButton.Caption:=lisDelete;
  BMMDeleteButton.Hint:=lisMMDeleteTheSelectedTargetOrOption;

  BMMNewTargetMenuItem.Caption:=lisMMNewTarget;
  BMMNewTargetMenuItem.Hint:=lisMMCreateANewGroupOfOptions;

  BMMNewCustomOptionMenuItem.Caption:=lisMMCustomOption;
  BMMNewIDEMacroMenuItem.Caption:=lisMMIDEMacro;
  BMMNewOutDirMenuItem.Caption:=lisMMOverrideOutputDirectory;

  fCaptionPatternMacroName:=lisMMSetS;
  fCaptionPatternMacroValue:=lisMMValueS;

  BMMAddLclWidgetButton.Caption:=Format(fCaptionPatternMacroName,['LCLWidgetType']);
  BMMAddLclWidgetButton.Hint := lisMMWidgetSetAvailableForLCLProject;
  BMMAddOtherButton.Caption:=lisAdd;

  BMMSystemEncodingButton.Caption:=lisMMUseSystemEncoding;
  BMMSystemEncodingButton.Hint:=lisMMUseSystemEncodingHint;

  UpdateButtons;
end;

destructor TCompOptModeMatrixFrame.Destroy;
begin
  ModeMatrixFrame:=nil;
  FreeAndNil(fOldIDEOptions);
  FreeAndNil(fOldSharedOptions);
  FreeAndNil(fOldSessionOptions);
  inherited Destroy;
end;

function TCompOptModeMatrixFrame.GetTitle: String;
begin
  Result:=lisMMAdditionsAndOverrides;
end;

procedure TCompOptModeMatrixFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  FDialog := ADialog;
end;

class function TCompOptModeMatrixFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectCompilerOptions;
end;

procedure TCompOptModeMatrixFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  CompOptions: TProjectCompilerOptions;
begin
  //debugln(['TCompOptModeMatrix.ReadSettings ',DbgSName(AOptions)]);
  if not (AOptions is TProjectCompilerOptions) then exit;
  CompOptions:=TProjectCompilerOptions(AOptions);
  if LazProject=CompOptions.LazProject then begin
    // options already loaded, only active compiler options are reloaded
    UpdateActiveMode;
    exit;
  end;

  fProject:=CompOptions.LazProject;

  UpdateModes(false);
  FillMenus;

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

procedure TCompOptModeMatrixFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  CompOptions: TProjectCompilerOptions;
begin
  if not (AOptions is TProjectCompilerOptions) then exit;
  CompOptions:=TProjectCompilerOptions(AOptions);
  fProject:=CompOptions.LazProject;

  DoWriteSettings;
end;

procedure TCompOptModeMatrixFrame.RestoreSettings(AOptions: TAbstractIDEOptions);
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
  RegisterIDEOptionsEditor(GroupCompiler, TCompOptModeMatrixFrame,
    CompilerOptionsAdditionsAndOverrides);

end.

