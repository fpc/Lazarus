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

  Author: Ondrej Pokorny

  Abstract:
    A simple dialog to select class section.
}
unit ChooseClassSectionDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, SourceChanger, LazarusIDEStrConsts, EnvironmentOpts;

type
  //this dialog can easily be reused.
  //for now it is used only in the method event assignment code creation

  TChooseClassSectionDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    NewIdentLabel: TLabel;
    SectionsListBox: TListBox;
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure SectionsListBoxDblClick(Sender: TObject);
    procedure SectionsListBoxKeyPress(Sender: TObject; var Key: char);
  protected
    procedure DoCreate; override;
  public

  end;

function ChooseClassSectionDialog(const ACaption, ANewIdent: string; ADefault: TInsertClassSectionResult;
  out Section: TInsertClassSectionResult): Boolean;
function ShowEventMethodSectionDialog(const ANewIdent: string; out Section: TInsertClassSectionResult): Boolean;
function ShowVarSectionDialog(const ANewIdent: string; out Section: TInsertClassSectionResult): Boolean;

implementation

{$R *.lfm}

function ChooseClassSectionDialog(const ACaption, ANewIdent: string; ADefault: TInsertClassSectionResult;
  out Section: TInsertClassSectionResult): Boolean;
var
  Dlg: TChooseClassSectionDialog;
begin
  Dlg := TChooseClassSectionDialog.Create(Application);
  try
    Dlg.Caption := ACaption;
    if ANewIdent<>'' then
      Dlg.NewIdentLabel.Caption := ANewIdent
    else
      Dlg.NewIdentLabel.Visible := False;
    Dlg.PopupMode := pmAuto;
    if Ord(ADefault) < Dlg.SectionsListBox.Count then
      Dlg.SectionsListBox.ItemIndex := Ord(ADefault)
    else
      Dlg.SectionsListBox.ItemIndex := 0;
    Result := Dlg.ShowModal = mrOK;
    if Result then
      Section := TInsertClassSectionResult(Dlg.SectionsListBox.ItemIndex)
    else
      Section := icsrPrivate;
  finally
    Dlg.Free;
  end;
end;

function ShowEventMethodSectionDialog(const ANewIdent: string; out
  Section: TInsertClassSectionResult): Boolean;
begin
  Result := ChooseClassSectionDialog(lisChooseClassSectionDlgForMethodCaption,
    ANewIdent, EnvironmentOptions.LastEventMethodSectionPrompt, Section);
  if Result then
    EnvironmentOptions.LastEventMethodSectionPrompt := Section;
end;

function ShowVarSectionDialog(const ANewIdent: string; out
  Section: TInsertClassSectionResult): Boolean;
begin
  Result := ChooseClassSectionDialog(lisChooseClassSectionDlgForVariableCaption,
    ANewIdent, EnvironmentOptions.LastVarSectionPrompt, Section);
  if Result then
    EnvironmentOptions.LastVarSectionPrompt := Section;
end;

{ TChooseClassSectionDialog }

procedure TChooseClassSectionDialog.DoCreate;
begin
  inherited DoCreate;

  Assert(Ord(High(TInsertClassSectionResult)) = 3,  'TChooseClassSectionDialog.DoCreate: High(TInsertClassSectionResult) <> 3');
  SectionsListBox.Items.Add(lisPrivate);
  SectionsListBox.Items.Add(lisProtected);
  SectionsListBox.Items.Add(lisEMDPublic);
  SectionsListBox.Items.Add(lisEMDPublished);
end;

procedure TChooseClassSectionDialog.FormKeyPress(Sender: TObject;
  var Key: char);
begin
  if Key = #27 then
    ModalResult := mrCancel;
end;

procedure TChooseClassSectionDialog.SectionsListBoxDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TChooseClassSectionDialog.SectionsListBoxKeyPress(
  Sender: TObject; var Key: char);
begin
  if Key = #13 then
    ModalResult := mrOK;
end;

initialization
  ShowEventMethodSectionPrompt := @ShowEventMethodSectionDialog;
  ShowVarSectionPrompt := @ShowVarSectionDialog;

end.

