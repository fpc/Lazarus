{  $Id$  }
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

  Author: Mattias Gaertner

  Abstract:
    Dialog to choose an IDE keymapping scheme.
}
unit KeymapSchemeDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, KeyMapping, LazarusIDEStrConsts, ButtonPanel;

type

  { TChooseKeySchemeDlg }

  TChooseKeySchemeDlg = class(TForm)
    ButtonPanel: TButtonPanel;
    NoteLabel: TLABEL;
    SchemeRadiogroup: TRADIOGROUP;
    procedure ChooseKeySchemeDlgCREATE(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
  private
    function GetKeymapScheme: string;
    procedure SetKeymapScheme(const AValue: string);
  public
    property KeymapScheme: string read GetKeymapScheme write SetKeymapScheme;// untranslated
  end;

function ShowChooseKeySchemeDialog(var NewScheme: string): TModalResult;

implementation

{$R *.lfm}

uses 
  IDEContextHelpEdit;

function ShowChooseKeySchemeDialog(var NewScheme: string): TModalResult;
var
  ChooseKeySchemeDlg: TChooseKeySchemeDlg;
begin
  ChooseKeySchemeDlg:=TChooseKeySchemeDlg.Create(nil);
  ChooseKeySchemeDlg.KeymapScheme:=NewScheme;
  Result:=ChooseKeySchemeDlg.ShowModal;
  if Result=mrOk then
    NewScheme:=ChooseKeySchemeDlg.KeymapScheme;
  ChooseKeySchemeDlg.Free;
end;

{ TChooseKeySchemeDlg }

procedure TChooseKeySchemeDlg.ChooseKeySchemeDlgCREATE(Sender: TObject);
begin
  Caption:=lisKMChooseKeymappingScheme;
  NoteLabel.Caption:=lisKMNoteAllKeysWillBeSetToTheValuesOfTheChosenScheme;
  SchemeRadiogroup.Caption:=lisKMKeymappingScheme;

  ButtonPanel.HelpButton.OnClick := @HelpButtonClick;

  with SchemeRadiogroup.Items do begin
    Clear;
    // keep order of TKeyMapScheme
    Add(lisKMLazarusDefault);
    Add(lisKMClassic);
    Add(lisKMMacOSXApple);
    Add(lisKMMacOSXLaz);
    // do not add custom
  end;
end;

procedure TChooseKeySchemeDlg.HelpButtonClick(Sender: TObject);
begin
  ShowContextHelpForIDE(Self);
end;

function TChooseKeySchemeDlg.GetKeymapScheme: string;
begin
  if SchemeRadiogroup.ItemIndex<0 then
    Result:=KeyMapSchemeNames[kmsLazarus]
  else if SchemeRadiogroup.ItemIndex<ord(kmsCustom) then
    Result:=KeyMapSchemeNames[TKeyMapScheme(SchemeRadiogroup.ItemIndex)]
  else
    Result:=SchemeRadiogroup.Items[SchemeRadiogroup.ItemIndex];
end;

procedure TChooseKeySchemeDlg.SetKeymapScheme(const AValue: string);
var
  kms: TKeyMapScheme;
begin
  kms:=KeySchemeNameToSchemeType(AValue);
  if kms=kmsCustom then begin
    if SchemeRadiogroup.Items.Count<=ord(kms) then
      SchemeRadiogroup.Items.Add(AValue)
    else
      SchemeRadiogroup.Items[SchemeRadiogroup.Items.Count-1]:=AValue;
  end;
  SchemeRadiogroup.ItemIndex:=ord(kms);
end;

end.

