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
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, Grids, Buttons,
  ExtCtrls,
  IDEImagesIntf, ProjectIntf, CompilerOptions,
  Options_Compiler_Conditionals, LazarusIDEStrConsts;

type

  { TCompOptBuildModesFrame }

  TCompOptBuildModesFrame = class(TFrame)
    CompOptsConditionalsFrame1: TCompOptsConditionalsFrame;
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
  private
    FBuildModes: TIDEBuildModes;
    procedure SetBuildModes(const AValue: TIDEBuildModes);
    procedure UpdateModes;
    procedure UpdateValues;
    procedure UpdateDefaultValue;
    procedure UpdateButtons;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property BuildModes: TIDEBuildModes read FBuildModes write SetBuildModes;
  end;

implementation

{ TCompOptBuildModesFrame }

procedure TCompOptBuildModesFrame.SetBuildModes(const AValue: TIDEBuildModes);
begin
  if FBuildModes=AValue then exit;
  FBuildModes:=AValue;
  UpdateModes;
end;

procedure TCompOptBuildModesFrame.UpdateModes;
begin

  UpdateValues;
end;

procedure TCompOptBuildModesFrame.UpdateValues;
begin
  UpdateDefaultValue;
end;

procedure TCompOptBuildModesFrame.UpdateDefaultValue;
begin

end;

procedure TCompOptBuildModesFrame.UpdateButtons;
begin

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

