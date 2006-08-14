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
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, LazarusIDEStrConsts;

type
  TChooseKeySchemeDlg = class(TForm)
    OkButton: TBUTTON;
    CancelButton: TBUTTON;
    NoteLabel: TLABEL;
    SchemeRadiogroup: TRADIOGROUP;
    procedure ChooseKeySchemeDlgCREATE(Sender: TObject);
  private
  public
    function GetKeymapScheme: string;
  end;

function ShowChooseKeySchemeDialog(var NewScheme: string): TModalResult;

implementation

function ShowChooseKeySchemeDialog(var NewScheme: string): TModalResult;
var
  ChooseKeySchemeDlg: TChooseKeySchemeDlg;
begin
  ChooseKeySchemeDlg:=TChooseKeySchemeDlg.Create(nil);
  Result:=ChooseKeySchemeDlg.ShowModal;
  if Result=mrOk then
    NewScheme:=ChooseKeySchemeDlg.GetKeymapScheme;
  ChooseKeySchemeDlg.Free;
end;

{ TChooseKeySchemeDlg }

procedure TChooseKeySchemeDlg.ChooseKeySchemeDlgCREATE(Sender: TObject);
begin
  Caption:=lisKMChooseKeymappingScheme;
  NoteLabel.Caption:=lisKMNoteAllKeysWillBeSetToTheValuesOfTheChoosenScheme;
  SchemeRadiogroup.Caption:=lisKMKeymappingScheme;
  OkButton.Caption:=lisOkBtn;
  CancelButton.Caption:=dlgCancel;
end;

function TChooseKeySchemeDlg.GetKeymapScheme: string;
begin
  case SchemeRadiogroup.ItemIndex of
  1: Result:=lisKMClassic;
  else Result:='';
  end;
end;

initialization
  {$I keymapschemedlg.lrs}

end.

