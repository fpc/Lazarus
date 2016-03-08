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
    A simple dialog to select code creation options.
}
unit CodeCreationDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, SourceChanger, LazarusIDEStrConsts, EnvironmentOpts, CodeCompletionTool,
  ExtCtrls;

type
  //this dialog can easily be reused.
  //for now it is used only in the method event assignment code creation

  TCodeCreationDialog = class(TForm)
    ButtonPanel: TButtonPanel;
    NewIdentLabel: TLabel;
    SectionRadioGroup: TRadioGroup;
    LocationRadioGroup: TRadioGroup;
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure LocationRadioGroupClick(Sender: TObject);
    procedure SectionRadioGroupDblClick(Sender: TObject);
    procedure SectionRadioGroupKeyPress(Sender: TObject; var Key: char);
  protected
    procedure DoCreate; override;
    procedure DoShow; override;
  public

  end;

function ShowCodeCreationDialog(const ANewIdent: string; const AIsMethod: Boolean;
    out Options: TCodeCreationDlgResult): Boolean;

implementation

{$R *.lfm}

function ShowCodeCreationDialog(const ANewIdent: string; const AIsMethod: Boolean;
  out Options: TCodeCreationDlgResult): Boolean;
var
  Dlg: TCodeCreationDialog;
begin
  if AIsMethod then
    Options := EnvironmentOptions.LastEventMethodCCResult
  else
    Options := EnvironmentOptions.LastVariableCCResult;

  Dlg := TCodeCreationDialog.Create(Application);
  try
    Dlg.Caption := lisCodeCreationDialogCaption;
    Dlg.LocationRadioGroup.Caption := lisCodeCreationDialogLocation;
    Dlg.SectionRadioGroup.Caption := lisCodeCreationDialogClassSection;

    if ANewIdent<>'' then
      Dlg.NewIdentLabel.Caption := ANewIdent
    else
      Dlg.NewIdentLabel.Visible := False;

    if Ord(Options.ClassSection) < Dlg.SectionRadioGroup.Items.Count then
      Dlg.SectionRadioGroup.ItemIndex := Ord(Options.ClassSection)
    else
      Dlg.SectionRadioGroup.ItemIndex := 0;

    if Ord(Options.Location) < Dlg.LocationRadioGroup.Items.Count then
      Dlg.LocationRadioGroup.ItemIndex := Ord(Options.Location)
    else
      Dlg.LocationRadioGroup.ItemIndex := 0;

    if AIsMethod then
    begin
      Dlg.LocationRadioGroup.ItemIndex := Ord(cclClass);
      Dlg.LocationRadioGroup.Enabled := False;
    end;

    Result := Dlg.ShowModal = mrOK;

    if Result then
    begin
      Options.ClassSection := TInsertClassSection(Dlg.SectionRadioGroup.ItemIndex);
      Options.Location := TCreateCodeLocation(Dlg.LocationRadioGroup.ItemIndex);
      if AIsMethod then
        EnvironmentOptions.LastEventMethodCCResult := Options
      else
        EnvironmentOptions.LastVariableCCResult := Options;
    end;
  finally
    Dlg.Free;
  end;
end;

{ TCodeCreationDialog }

procedure TCodeCreationDialog.DoCreate;
var
  S: TInsertClassSection;
  L: TCreateCodeLocation;
begin
  inherited DoCreate;

  KeyPreview := True;

  SectionRadioGroup.Items.Clear;
  for S := Low(TInsertClassSection) to High(TInsertClassSection) do
    SectionRadioGroup.Items.Add(InsertClassSectionAmpNames[S]);

  LocationRadioGroup.Items.Clear;
  for L := Low(TCreateCodeLocation) to High(TCreateCodeLocation) do
    LocationRadioGroup.Items.Add(CreateCodeLocationAmpNames[L]);
end;

procedure TCodeCreationDialog.DoShow;
begin
  inherited DoShow;

  LocationRadioGroupClick(nil);
end;

procedure TCodeCreationDialog.FormKeyPress(Sender: TObject;
  var Key: char);
begin
  case Key of
    #27: ModalResult := mrCancel;
    'p': SectionRadioGroup.ItemIndex := Ord(icsPrivate);
    'r': SectionRadioGroup.ItemIndex := Ord(icsProtected);
    'u': SectionRadioGroup.ItemIndex := Ord(icsPublic);
    's': SectionRadioGroup.ItemIndex := Ord(icsPublished);
    'l': LocationRadioGroup.ItemIndex := Ord(cclLocal);
    'c': LocationRadioGroup.ItemIndex := Ord(cclClass);
  end;
end;

procedure TCodeCreationDialog.LocationRadioGroupClick(Sender: TObject);
begin
  SectionRadioGroup.Enabled := LocationRadioGroup.ItemIndex = Ord(cclClass);
end;

procedure TCodeCreationDialog.SectionRadioGroupDblClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TCodeCreationDialog.SectionRadioGroupKeyPress(
  Sender: TObject; var Key: char);
begin
  if Key = #13 then
    ModalResult := mrOK;
end;

initialization
  ShowCodeCreationDlg := @ShowCodeCreationDialog;

end.

