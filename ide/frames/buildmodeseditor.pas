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
  Math, Classes, SysUtils, LCLProc, Controls, FileUtil, Forms,
  Grids, Graphics, Menus, ComCtrls, Dialogs, AvgLvlTree, DefineTemplates,
  StdCtrls, GraphMath, ExtCtrls, Buttons,
  ProjectIntf, IDEImagesIntf, IDEOptionsIntf,
  PathEditorDlg, Project, PackageSystem, LazarusIDEStrConsts, CompilerOptions,
  IDEProcs;

type


  { TBuildModesEditorFrame }

  TBuildModesEditorFrame = class(TAbstractIDEOptionsEditor)
    BuildModesPopupMenu: TPopupMenu;
    BuildModeBtnPanel: TPanel;
    NewBuildModeSpeedButton: TSpeedButton;
    DeleteBMRowSpeedButton: TSpeedButton;
    procedure DeleteBMRowButtonClick(Sender: TObject);
    procedure NewBuildModeButtonClick(Sender: TObject);
  private
    procedure UpdateButtons;
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TBuildModesEditorFrame }

procedure TBuildModesEditorFrame.NewBuildModeButtonClick(Sender: TObject);
begin

end;

procedure TBuildModesEditorFrame.UpdateButtons;
begin

end;

function TBuildModesEditorFrame.GetTitle: String;
begin
  Result := 'Build modes';
end;

procedure TBuildModesEditorFrame.DeleteBMRowButtonClick(Sender: TObject);
begin

end;

procedure TBuildModesEditorFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  NewBuildModeSpeedButton.Hint:=lisNewBuildMode;
  NewBuildModeSpeedButton.LoadGlyphFromLazarusResource('laz_add');
  DeleteBMRowSpeedButton.Hint:=lisDeleteRow;
  DeleteBMRowSpeedButton.LoadGlyphFromLazarusResource('laz_delete');

  // laz_edit, arrow_up, arrow_down
  UpdateButtons;
end;

procedure TBuildModesEditorFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin

end;

procedure TBuildModesEditorFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  // todo:
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

