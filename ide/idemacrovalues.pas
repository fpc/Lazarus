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
    The frame for 'IDE macro values' on the compiler options.
    Allows to add/delete/edit values for existing macros.
    It does not allow to define new IDE macros, only values.
}
unit IdeMacroValues;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLProc, Controls, FileUtil, Forms,
  Grids, Graphics, Menus, ComCtrls, Dialogs, AvgLvlTree, DefineTemplates,
  StdCtrls, GraphMath, ExtCtrls, Buttons,
  ProjectIntf, IDEImagesIntf, IDEOptionsIntf, CompOptsIntf,
  PackageDefs, compiler_inherited_options, TransferMacros,
  PathEditorDlg, Project, PackageSystem, LazarusIDEStrConsts, CompilerOptions,
  IDEProcs;

type

  { TIdeMacroValuesFrame }

  TIdeMacroValuesFrame = class(TAbstractIDEOptionsEditor)
    IdeMacroValuesGroupBox: TGroupBox;
    IdeMacroValuesStringGrid: TStringGrid;
    MenuItem1: TMenuItem;
    procedure IdeMacroValuesStringGridEditingDone(Sender: TObject);
    procedure IdeMacroValuesStringGridSelectEditor(Sender: TObject; aCol,
      aRow: Integer; var Editor: TWinControl);
    procedure IdeMacroValuesStringGridSelection(Sender: TObject; aCol,
      aRow: Integer);
  private
    FLoadShowSessionFromProject: boolean;
    FMacroValues: TProjectBuildMacros;
    FProject: TProject;
    FShowSession: boolean;
    FSwitchingMode: boolean;
    procedure UpdateMacrosControls;
    function GetAllIdeMacros: TStrings;
    procedure CleanMacrosGrid;
    procedure SaveMacros(UpdateControls: boolean);
    procedure UpdateInheritedOptions;
    procedure ActivateMode(aMode: TProjectBuildMode);
    procedure UpdateShowSession;
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
    property ShowSession: boolean read FShowSession write FShowSession;
    property LoadShowSessionFromProjects: boolean read FLoadShowSessionFromProject
                                              write FLoadShowSessionFromProject;
  end;

implementation

{$R *.lfm}

{ TIdeMacroValuesFrame }

procedure TIdeMacroValuesFrame.IdeMacroValuesStringGridSelectEditor(
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
  Grid:=IdeMacroValuesStringGrid;
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

      Macros:=GetAllIdeMacros;
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
      Macros:=GetAllIdeMacros;
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

procedure TIdeMacroValuesFrame.IdeMacroValuesStringGridEditingDone(Sender: TObject);
begin
  SaveMacros(true);
end;

procedure TIdeMacroValuesFrame.IdeMacroValuesStringGridSelection(
  Sender: TObject; aCol, aRow: Integer);
begin
  CleanMacrosGrid;
end;

procedure TIdeMacroValuesFrame.UpdateMacrosControls;
var
  Grid: TStringGrid;
  i: Integer;
begin
  Grid:=IdeMacroValuesStringGrid;
  Grid.RowCount:=MacroValues.Count+2; // + titles + add button

  for i:=0 to MacroValues.Count-1 do begin
    Grid.Cells[0,i+1]:=MacroValues.Names[i];
    Grid.Cells[1,i+1]:=MacroValues.ValueFromIndex(i);
  end;
  i:=MacroValues.Count+1;
  Grid.Cells[0,i]:='(none)';
  Grid.Cells[1,i]:='';
end;

procedure TIdeMacroValuesFrame.UpdateDialogCaption;
var
  Form: TCustomForm;
begin
  Form:=GetParentForm(Self);
  if Form<>nil then
    Form.Caption:=GetDialogCaption;
end;

function TIdeMacroValuesFrame.GetDialogCaption: string;
begin
  if AProject<>nil then
  begin
    Result := aProject.GetTitleOrName;
    Result:=Format(dlgProjectOptionsFor, [Result]);
    if AProject.BuildModes.Count>1 then
      Result:=Result+', '+copy(AProject.ActiveBuildMode.GetCaption,1,12);
  end else
    Result:='TIdeMacroValuesFrame.GetDialogCaption: no project';
end;

function TIdeMacroValuesFrame.GetAllIdeMacros: TStrings;

  procedure Add(aBuildMacro: TLazBuildMacro);
  begin
    if GetAllIdeMacros.IndexOf(aBuildMacro.Identifier)>=0 then exit;
    GetAllIdeMacros.AddObject(aBuildMacro.Identifier,aBuildMacro);
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
    PackageGraph.GetAllRequiredPackages(nil,AProject.FirstRequiredDependency,PkgList);
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

procedure TIdeMacroValuesFrame.CleanMacrosGrid;
var
  Grid: TStringGrid;
  aRow: Integer;
  MacroName: string;
  NeedNewRow: Boolean;
begin
  Grid:=IdeMacroValuesStringGrid;
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

procedure TIdeMacroValuesFrame.SaveMacros(UpdateControls: boolean);
var
  Grid: TStringGrid;
  aRow: Integer;
  MacroName: string;
  Values: TStringList;
  Value: string;
begin
  if MacroValues=nil then exit;
  Grid:=IdeMacroValuesStringGrid;
  Values:=TStringList.Create;
  try
    for aRow:=1 to Grid.RowCount-1 do begin
      MacroName:=Grid.Cells[0,aRow];
      if (MacroName='') or (not IsValidIdent(MacroName)) then continue;
      Value:=Grid.Cells[1,aRow];
      Values.Values[MacroName]:=Value;
    end;
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

procedure TIdeMacroValuesFrame.UpdateInheritedOptions;
var
  InhOptionCtrl: TCompilerInheritedOptionsFrame;
begin
  InhOptionCtrl:=TCompilerInheritedOptionsFrame(
                               FindOptionControl(TCompilerInheritedOptionsFrame));
  if InhOptionCtrl=nil then exit;
  InhOptionCtrl.UpdateInheritedTree(AProject.CompilerOptions);
end;

procedure TIdeMacroValuesFrame.ActivateMode(aMode: TProjectBuildMode);
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

procedure TIdeMacroValuesFrame.UpdateShowSession;
begin
  if LoadShowSessionFromProjects then
    ShowSession:=(AProject<>nil)
      and (AProject.SessionStorage in [pssInProjectDir,pssInIDEConfig]);
end;

constructor TIdeMacroValuesFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FLoadShowSessionFromProject:=true;
end;

destructor TIdeMacroValuesFrame.Destroy;
begin
  inherited Destroy;
end;

function TIdeMacroValuesFrame.GetTitle: String;
begin
  Result := dlgIdeMacroValues;
end;

procedure TIdeMacroValuesFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  Grid: TStringGrid;
begin
  IdeMacroValuesGroupBox.Caption:=lisIDEMacroValuesForFPCMacrosUseCustomOptions;
  Grid:=IdeMacroValuesStringGrid;
  Grid.Columns.Add;
  Grid.Columns[0].Title.Caption:=lisMacroName;
  Grid.Columns[0].ButtonStyle:=cbsPickList;
  Grid.Columns.Add;
  Grid.Columns[1].Title.Caption:=lisMacroValue;
  Grid.Columns[1].ButtonStyle:=cbsPickList;
end;

procedure TIdeMacroValuesFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  PCOptions: TProjectCompilerOptions;
begin
  if AOptions is TProjectCompilerOptions then begin
    PCOptions:=TProjectCompilerOptions(AOptions);
    FProject:=PCOptions.LazProject;
    FMacroValues:=FProject.ActiveBuildMode.MacroValues;
    // modes
    UpdateShowSession;
    // macros
    MacroValues.Assign(FProject.MacroValues);
    UpdateMacrosControls;
    // options dialog
    UpdateDialogCaption;
  end;
end;

procedure TIdeMacroValuesFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if AOptions is TProjectCompilerOptions then
    SaveMacros(false);
end;

class function TIdeMacroValuesFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectCompilerOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupCompiler, TIdeMacroValuesFrame, CompilerOptionsMacroValues);

end.

