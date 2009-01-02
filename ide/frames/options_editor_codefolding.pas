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
unit options_editor_codefolding;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, StdCtrls, ExtCtrls, Spin,
  EditorOptions, LazarusIDEStrConsts, IDEOptionsIntf;

type

  { TEditorCodefoldingOptionsFrame }

  TEditorCodefoldingOptionsFrame = class(TAbstractIDEOptionsEditor)
    Bevel1: TBevel;
    chkAllowSkipGutterSeparatorDraw: TCheckBox;
    chkCodeFoldingEnabled: TCheckBox;
    edDividerDrawLevel: TSpinEdit;
    lblDividerDrawLevel: TLabel;
    procedure chkCodeFoldingEnabledChange(Sender: TObject);
  private
    { private declarations }
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{ TEditorCodefoldingOptionsFrame }

procedure TEditorCodefoldingOptionsFrame.chkCodeFoldingEnabledChange(Sender: TObject);
begin
  lblDividerDrawLevel.Enabled := chkCodeFoldingEnabled.Checked;
  edDividerDrawLevel.Enabled  := chkCodeFoldingEnabled.Checked;
  chkAllowSkipGutterSeparatorDraw.Enabled := chkCodeFoldingEnabled.Checked;
end;

function TEditorCodefoldingOptionsFrame.GetTitle: String;
begin
  Result := dlgUseCodeFolding;
end;

procedure TEditorCodefoldingOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  chkCodeFoldingEnabled.Caption := dlgUseCodeFolding;
  lblDividerDrawLevel.Caption := dlgCFDividerDrawLevel + ':';
  chkAllowSkipGutterSeparatorDraw.Caption := dlgAllowSkipGutterSeparatorDraw;
end;

procedure TEditorCodefoldingOptionsFrame.ReadSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    chkCodeFoldingEnabled.Checked := UseCodeFolding;
    edDividerDrawLevel.Value := CFDividerDrawLevel;
    chkAllowSkipGutterSeparatorDraw.Checked := AllowSkipGutterSeparatorDraw;
  end;
end;

procedure TEditorCodefoldingOptionsFrame.WriteSettings(
  AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEditorOptions do
  begin
    UseCodeFolding := chkCodeFoldingEnabled.Checked;
    CFDividerDrawLevel := edDividerDrawLevel.Value;
    AllowSkipGutterSeparatorDraw := chkAllowSkipGutterSeparatorDraw.Checked;
  end;
end;

class function TEditorCodefoldingOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEditorOptions;
end;

initialization
  {$I options_editor_codefolding.lrs}
  RegisterIDEOptionsEditor(GroupEditor, TEditorCodefoldingOptionsFrame, EdtOptionsCodeFolding);
end.

