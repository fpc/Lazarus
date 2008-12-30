{***************************************************************************
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
unit options_compiler_buildmodes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Controls, LResources, Forms, StdCtrls, Grids,
  Buttons, ExtCtrls, Dialogs,
  IDEImagesIntf, ProjectIntf, CompilerOptions,
  Options_Compiler_Conditionals, LazarusIDEStrConsts, CompOptsModes;

type

  { TCompOptBuildModesFrame }

  TCompOptBuildModesFrame = class(TFrame)
    DefaultValueEditor: TCompOptsConditionalsFrame;
    DefaultValueGroupBox: TGroupBox;
    ValuesGroupBox: TGroupBox;
    ModesGroupBox: TGroupBox;
    ModesListBox: TListBox;
    NewSpeedButton: TSpeedButton;
    DeleteSpeedButton: TSpeedButton;
    MoveDownSpeedButton: TSpeedButton;
    MoveUpSpeedButton: TSpeedButton;
    MainSplitter: TSplitter;
    ValuesSplitter: TSplitter;
    ValuesStringGrid: TStringGrid;
    procedure DeleteSpeedButtonClick(Sender: TObject);
    procedure MoveDownSpeedButtonClick(Sender: TObject);
    procedure MoveUpSpeedButtonClick(Sender: TObject);
    procedure NewSpeedButtonClick(Sender: TObject);
  private
    FBuildModes: TIDEBuildModes;
    procedure SetBuildModes(const AValue: TIDEBuildModes);
    procedure UpdateModes;
    procedure UpdateValues;
    procedure UpdateDefaultValue;
    procedure UpdateButtons;
    function GetSelectedBuildMode(out BuildMode: TIDEBuildMode): boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property BuildModes: TIDEBuildModes read FBuildModes write SetBuildModes;
  end;

implementation

{ TCompOptBuildModesFrame }

procedure TCompOptBuildModesFrame.NewSpeedButtonClick(Sender: TObject);
begin

end;

procedure TCompOptBuildModesFrame.DeleteSpeedButtonClick(Sender: TObject);
var
  BuildMode: TIDEBuildMode;
  i: LongInt;
begin
  if not GetSelectedBuildMode(BuildMode) then exit;
  i:=ModesListBox.ItemIndex;
  if MessageDlg('Confirm delete',
    'Delete build mode "'+BuildMode.Identifier+'"?',
    mtConfirmation,[mbYes,mbCancel],0)<>mrYes
  then exit;
  BuildModes.Delete(i);
  ModesListBox.Items.Delete(i);
  if i=ModesListBox.Items.Count then
    dec(i);
  ModesListBox.ItemIndex:=i;
end;

procedure TCompOptBuildModesFrame.MoveDownSpeedButtonClick(Sender: TObject);
var
  i: LongInt;
  BuildMode: TIDEBuildMode;
begin
  if not GetSelectedBuildMode(BuildMode) then exit;
  i:=ModesListBox.ItemIndex;
  if i<ModesListBox.Items.Count-1 then begin
    BuildModes.Move(i,i+1);
    ModesListBox.Items.Move(i,i+1);
    ModesListBox.ItemIndex:=i+1;
  end;
end;

procedure TCompOptBuildModesFrame.MoveUpSpeedButtonClick(Sender: TObject);
var
  i: LongInt;
  BuildMode: TIDEBuildMode;
begin
  if not GetSelectedBuildMode(BuildMode) then exit;
  i:=ModesListBox.ItemIndex;
  if i>0 then begin
    BuildModes.Move(i,i-1);
    ModesListBox.Items.Move(i,i-1);
    ModesListBox.ItemIndex:=i-1;
  end;
end;

procedure TCompOptBuildModesFrame.SetBuildModes(const AValue: TIDEBuildModes);
begin
  if FBuildModes=AValue then exit;
  FBuildModes:=AValue;
  UpdateModes;
end;

procedure TCompOptBuildModesFrame.UpdateModes;
var
  i: Integer;
begin
  ModesListBox.Items.BeginUpdate;
  ModesListBox.Items.Clear;
  if BuildModes<>nil then begin
    for i:=0 to BuildModes.Count-1 do begin
      ModesListBox.Items.Add(BuildModes.Items[i].Identifier);
    end;
  end;
  ModesListBox.Items.EndUpdate;
  UpdateValues;
  UpdateButtons;
end;

procedure TCompOptBuildModesFrame.UpdateValues;
var
  BuildMode: TIDEBuildMode;
  i: Integer;
begin
  if not GetSelectedBuildMode(BuildMode) then exit;
  ValuesStringGrid.ColCount:=2;
  ValuesStringGrid.FixedCols:=0;
  ValuesStringGrid.RowCount:=BuildMode.Values.Count+1;
  ValuesStringGrid.FixedRows:=1;
  ValuesStringGrid.Cells[0,0]:='Value';
  ValuesStringGrid.Cells[1,0]:='Description';
  ValuesStringGrid.ColWidths[0]:=90;
  ValuesStringGrid.ColWidths[1]:=120;
  for i:=0 to BuildMode.Values.Count-1 do begin
    ValuesStringGrid.Cells[0,i+1]:=BuildMode.Values[i];
    if i<BuildMode.ValueDescriptions.Count then
      ValuesStringGrid.Cells[1,i+1]:=BuildMode.ValueDescriptions[i]
    else
      ValuesStringGrid.Cells[1,i+1]:='';
  end;
  UpdateDefaultValue;
end;

procedure TCompOptBuildModesFrame.UpdateDefaultValue;
var
  BuildMode: TIDEBuildMode;
begin
  if not GetSelectedBuildMode(BuildMode) then exit;
  DefaultValueEditor.Conditionals:=TCompOptConditionals(BuildMode.DefaultValue);
end;

procedure TCompOptBuildModesFrame.UpdateButtons;
begin
  NewSpeedButton.Enabled:=BuildModes<>nil;
  DeleteSpeedButton.Enabled:=(ModesListBox.ItemIndex>=0);
  MoveDownSpeedButton.Enabled:=(ModesListBox.ItemIndex>=0)
                        and (ModesListBox.ItemIndex<ModesListBox.Items.Count-1);
  MoveUpSpeedButton.Enabled:=(ModesListBox.ItemIndex>0);
end;

function TCompOptBuildModesFrame.GetSelectedBuildMode(
  out BuildMode: TIDEBuildMode): boolean;
begin
  BuildMode:=nil;
  if BuildModes=nil then exit(false);
  if ModesListBox.ItemIndex<0 then exit(false);
  BuildMode:=TIDEBuildMode(BuildModes.Items[ModesListBox.ItemIndex]);
  Result:=true;
end;

constructor TCompOptBuildModesFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  DefaultValueGroupBox.Caption:='Default value';
  ValuesGroupBox.Caption:='Values';
  ModesGroupBox.Caption:='Build modes';
  NewSpeedButton.LoadGlyphFromLazarusResource('menu_new');
  NewSpeedButton.ShowHint:=true;
  NewSpeedButton.Hint:='Create new build mode';
  DeleteSpeedButton.LoadGlyphFromLazarusResource('menu_project_remove');
  DeleteSpeedButton.ShowHint:=true;
  DeleteSpeedButton.Hint:='Delete ...';
  MoveDownSpeedButton.LoadGlyphFromLazarusResource('arrow_down');
  MoveDownSpeedButton.ShowHint:=true;
  MoveDownSpeedButton.Hint:='Move down';
  MoveUpSpeedButton.LoadGlyphFromLazarusResource('arrow_up');
  MoveUpSpeedButton.ShowHint:=true;
  MoveUpSpeedButton.Hint:='Move up';
end;

destructor TCompOptBuildModesFrame.Destroy;
begin

  inherited Destroy;
end;

initialization
  {$I options_compiler_buildmodes.lrs}

end.

