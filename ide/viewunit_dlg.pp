{  $Id$  }
{
 /***************************************************************************
                          ViewUnit_dlg.pp
                          ---------------
   TViewUnit is the application dialog for displaying all units in a project.


   Initial Revision  : Sat Feb 19 17:42 CST 1999


 ***************************************************************************/

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
unit ViewUnit_Dlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Controls, Forms, Dialogs, LResources, Buttons, StdCtrls,
  LazarusIdeStrConsts, IDEOptionDefs;

type
  TViewUnitsEntry = class
  public
    Name: string;
    ID: integer;
    Selected: boolean;
    constructor Create(const AName: string; AnID: integer; ASelected: boolean);
  end;

  TViewUnits = class(TForm)
    Edit: TEdit;
    ListBox: TListBox;
    btnOK: TButton;
    btnCancel: TButton;
    MultiSelectCheckBox: TCheckBox;
    Procedure btnOKClick(Sender :TObject);
    Procedure btnCancelClick(Sender :TObject);
    procedure MultiselectCheckBoxClick(Sender :TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;


function ShowViewUnitsDlg(Entries: TList; MultiSelect: boolean;
  const Caption: string): TModalResult;
  // Entries is a list of TViewUnitsEntry(s)


implementation


function ShowViewUnitsDlg(Entries: TList;
  MultiSelect: boolean; const Caption: string): TModalResult;
var ViewUnits: TViewUnits;
  i: integer;
begin
  ViewUnits:=TViewUnits.Create(Application);
  try
    ViewUnits.Caption:=Caption;
    ViewUnits.MultiselectCheckBox.Enabled:=MultiSelect;
    ViewUnits.MultiselectCheckBox.Checked:=MultiSelect;
    ViewUnits.ListBox.Multiselect:=ViewUnits.MultiselectCheckBox.Checked;
    with ViewUnits.ListBox.Items do begin
      BeginUpdate;
      Clear;
      for i:=0 to Entries.Count-1 do
        Add(TViewUnitsEntry(Entries[i]).Name);
      EndUpdate;
    end;
    for i:=0 to Entries.Count-1 do
      ViewUnits.ListBox.Selected[i]:=TViewUnitsEntry(Entries[i]).Selected;
    Result:=ViewUnits.ShowModal;
    if Result=mrOk then begin
      for i:=0 to Entries.Count-1 do begin
        TViewUnitsEntry(Entries[i]).Selected:=ViewUnits.ListBox.Selected[i];
      end;
    end;
  finally
    ViewUnits.Free;
  end;
end;

{ TViewUnitsEntry }

constructor TViewUnitsEntry.Create(const AName: string; AnID: integer;
  ASelected: boolean);
begin
  inherited Create;
  Name:=AName;
  ID:=AnID;
  Selected:=ASelected;
end;

{ TViewUnits }

constructor TViewUnits.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IDEDialogLayoutList.ApplyLayout(Self,450,300);
  btnOK.Caption:='Ok';
  btnOk.Left:=ClientWidth-btnOk.Width-5;
  btnCancel.Caption:='Cancel';
  btnCancel.Left:=btnOk.Left;
  MultiSelectCheckBox.Caption:='Multi Select';
  MultiSelectCheckBox.Left:=btnOk.Left;
end;

Procedure TViewUnits.btnOKClick(Sender : TOBject);
Begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrOK;
End;

Procedure TViewUnits.btnCancelClick(Sender : TOBject);
Begin
  IDEDialogLayoutList.SaveLayout(Self);
  ModalResult := mrCancel;
end;

procedure TViewUnits.MultiselectCheckBoxClick(Sender :TObject);
begin
  ListBox.Multiselect:=MultiselectCheckBox.Checked;
end;


initialization
 {$I viewunits1.lrs}


end.
{
  $Log$
  Revision 1.17  2004/04/03 13:35:14  mattias
  fixed view unit dialog using lfm

  Revision 1.16  2003/05/02 22:22:15  mattias
  localization, added context policy to make resource string dialog

  Revision 1.15  2003/03/08 01:33:35  mattias
  localization from Olivier

  Revision 1.14  2003/02/28 19:10:25  mattias
  added new ... dialog

  Revision 1.13  2002/10/14 08:27:37  lazarus
  MG: view units/forms dialog size is now saved

  Revision 1.12  2002/09/20 07:26:37  lazarus
  MG: applied localization from Vasily

  Revision 1.11  2002/05/10 06:57:47  lazarus
  MG: updated licenses

  Revision 1.10  2002/04/16 15:22:50  lazarus
  MG: added form resizes

  Revision 1.9  2002/02/17 19:34:45  lazarus
  MG: fixed view units/forms

  Revision 1.8  2001/04/04 12:20:34  lazarus
  MG: added  add to/remove from project, small bugfixes

  Revision 1.7  2001/03/08 15:59:06  lazarus
  IDE bugfixes and viewunit/forms functionality

  Revision 1.6  2001/01/16 23:30:45  lazarus
  trying to determine what's crashing LAzarus on load.
  Shane

  Revision 1.4  2001/01/14 03:56:57  lazarus
  Shane

  Revision 1.3  2001/01/13 06:11:07  lazarus
  Minor fixes
  Shane

  Revision 1.2  2001/01/05 17:44:37  lazarus
  ViewUnits1, ViewForms1 and MessageDlg are all loaded from their resources and all controls are auto-created on them.
  There are still a few problems with some controls so I haven't converted all forms.
  Shane

  Revision 1.1  2000/07/13 10:27:48  michael
  + Initial import

}

