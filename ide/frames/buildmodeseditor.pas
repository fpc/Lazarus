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
    Allows to edit build modes and build macro values.
    It does not allow to define new build macros.
}
unit BuildModesEditor;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLProc, Controls, FileUtil, Forms,
  Grids, Graphics, Menus, ComCtrls, Dialogs, AvgLvlTree, DefineTemplates,
  StdCtrls, GraphMath, ExtCtrls, Buttons,
  ProjectIntf, IDEImagesIntf, IDEOptionsIntf,
  PackageDefs,
  PathEditorDlg, Project, PackageSystem, LazarusIDEStrConsts, CompilerOptions,
  IDEProcs;

type


  { TBuildModesEditorFrame }

  TBuildModesEditorFrame = class(TAbstractIDEOptionsEditor)
    BuildMacroValuesGroupBox: TGroupBox;
    BuildMacroValuesStringGrid: TStringGrid;
    BuildModesGroupBox: TGroupBox;
    BuildModesPopupMenu: TPopupMenu;
    Splitter1: TSplitter;
    procedure BuildMacroValuesStringGridEditButtonClick(Sender: TObject);
    procedure BuildMacroValuesStringGridMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BuildMacroValuesStringGridPickListSelect(Sender: TObject);
    procedure BuildMacroValuesStringGridSelectCell(Sender: TObject; aCol,
      aRow: Integer; var CanSelect: Boolean);
    procedure BuildMacroValuesStringGridSelectEditor(Sender: TObject; aCol,
      aRow: Integer; var Editor: TWinControl);
  private
    FMacroValues: TProjectBuildMacros;
    FProject: TProject;
    procedure UpdateMacrosControls;
    function GetAllBuildMacros: TStrings;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    property AProject: TProject read FProject;
    property MacroValues: TProjectBuildMacros read FMacroValues;
  end;

implementation

{$R *.lfm}

{ TBuildModesEditorFrame }

procedure TBuildModesEditorFrame.BuildMacroValuesStringGridEditButtonClick(
  Sender: TObject);
var
  Grid: TStringGrid;
begin
  Grid:=BuildMacroValuesStringGrid;
  //debugln(['TBuildModesEditorFrame.BuildMacroValuesStringGridEditButtonClick Col=',Grid.Col,' Row=',Grid.Row]);
  if Grid.Col=0 then begin
    if Grid.Row=MacroValues.Count+1 then begin
      // add new row

    end else if Grid.Row>0 then begin
      // delete row

    end;
  end;
end;

procedure TBuildModesEditorFrame.BuildMacroValuesStringGridMouseUp(
  Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Grid: TStringGrid;
  ACol: Longint;
  ARow: Longint;
begin
  Grid:=BuildMacroValuesStringGrid;
  Grid.MouseToCell(X,Y,ACol,ARow);
  //debugln(['TBuildModesEditorFrame.BuildMacroValuesStringGridMouseUp ',ACol,',',ARow]);
  if ARow=MacroValues.Count+1 then begin

  end;
end;

procedure TBuildModesEditorFrame.BuildMacroValuesStringGridPickListSelect(
  Sender: TObject);
begin
  //debugln(['TBuildModesEditorFrame.BuildMacroValuesStringGridPickListSelect ',Grid.Col,',',Grid.Row]);
end;

procedure TBuildModesEditorFrame.BuildMacroValuesStringGridSelectCell(
  Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  //debugln(['TBuildModesEditorFrame.BuildMacroValuesStringGridSelectCell ',Grid.Col,',',Grid.Row,' ',aCol,',',aRow]);
end;

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
      if aRow=MacroValues.Count+1 then
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

function TBuildModesEditorFrame.GetTitle: String;
begin
  Result := 'Build modes';
end;

procedure TBuildModesEditorFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
var
  Grid: TStringGrid;
begin
  BuildModesGroupBox.Caption:='Build modes';

  BuildMacroValuesGroupBox.Caption:='Set macro values';
  Grid:=BuildMacroValuesStringGrid;
  Grid.Columns.Add;
  Grid.Columns[0].Title.Caption:='Macro name';
  Grid.Columns[0].ButtonStyle:=cbsPickList;
  Grid.Columns.Add;
  Grid.Columns[1].Title.Caption:='Macro value';
  Grid.Columns[1].ButtonStyle:=cbsPickList;
end;

procedure TBuildModesEditorFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  PCOptions: TProjectCompilerOptions;
begin
  //debugln(['TBuildModesEditorFrame.ReadSettings ',DbgSName(AOptions)]);
  if AOptions is TProjectCompilerOptions then begin
    PCOptions:=TProjectCompilerOptions(AOptions);
    FProject:=PCOptions.Project;
    FMacroValues:=FProject.MacroValues;
    UpdateMacrosControls;
  end;
end;

procedure TBuildModesEditorFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin

end;

class function TBuildModesEditorFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TProjectCompilerOptions;
end;

{$IFDEF EnableBuildModes}
initialization
  RegisterIDEOptionsEditor(GroupCompiler, TBuildModesEditorFrame,
    CompilerOptionsBuildModes);
{$ENDIF}
end.

