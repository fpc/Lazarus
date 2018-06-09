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
}
unit fpdoc_options;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  // LCL
  Dialogs, StdCtrls, EditBtn,
  // IdeIntf
  IDEOptionsIntf, IDEOptEditorIntf,
  // IDE
  EnvironmentOpts, LazarusIDEStrConsts, IDEProcs;

type

  { TFpDocOptionsFrame }

  TFpDocOptionsFrame = class(TAbstractIDEOptionsEditor)
    LazDocAddPathButton: TButton;
    LazDocDeletePathButton: TButton;
    LazDocListBox: TListBox;
    LazDocPathEdit: TDirectoryEdit;
    LazDocPathsGroupBox: TGroupBox;
    procedure LazDocDeletePathButtonClick(Sender: TObject);
    procedure LazDocAddPathButtonClick(Sender: TObject);
    procedure LazDocListBoxSelectionChange(Sender: TObject; {%H-}User: boolean);
    procedure LazDocPathEditChange(Sender: TObject);
  private
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

{ TFpDocOptionsFrame }

function TFpDocOptionsFrame.GetTitle: String;
begin
  Result := lisFPDocEditor;
end;

procedure TFpDocOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  LazDocPathsGroupBox.Caption := lisCodeHelpGroupBox;
  LazDocAddPathButton.Caption := lisCodeHelpAddPathButton;
  LazDocDeletePathButton.Caption := lisCodeHelpDeletePathButton;

  LazDocPathEdit.Clear;
end;

procedure TFpDocOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
    SplitString(FPDocPaths, ';', LazDocListBox.Items);
end;

procedure TFpDocOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
    FPDocPaths := StringListToText(LazDocListBox.Items, ';', true);
end;

procedure TFpDocOptionsFrame.LazDocAddPathButtonClick(Sender: TObject);
begin
  if LazDocPathEdit.Text <> '' then begin
    LazDocListBox.Items.Add(LazDocPathEdit.Text);
    LazDocPathEdit.Text := '';
  end;
end;

procedure TFpDocOptionsFrame.LazDocDeletePathButtonClick(Sender: TObject);
begin
  if LazDocListBox.ItemIndex >= 0 then begin
    LazDocListBox.Items.Delete(LazDocListBox.ItemIndex);
    LazDocListBoxSelectionChange(LazDocListBox, True);
  end;
end;

procedure TFpDocOptionsFrame.LazDocListBoxSelectionChange(Sender: TObject; User: boolean);
begin
  LazDocDeletePathButton.Enabled:=(Sender as TListBox).ItemIndex <> -1;
end;

procedure TFpDocOptionsFrame.LazDocPathEditChange(Sender: TObject);
begin
  LazDocAddPathButton.Enabled:= LazDocPathEdit.Text <> '';
end;

class function TFpDocOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TFpDocOptionsFrame, EnvOptionsFpDoc);
end.

