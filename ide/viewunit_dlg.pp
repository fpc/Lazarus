{  $Id$  }
{
 /***************************************************************************
                          ViewUnit_dlg.pp
                        -------------------
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
  LazarusIdeStrConsts;

type
  TViewUnitsEntry = class
  public
    Name: string;
    ID: integer;
    Selected: boolean;
    constructor Create(const AName: string; AnID: integer; ASelected: boolean);
  end;

  TViewUnits = class(TForm)
    ListBox: TListBox;
    btnOK : TButton;
    btnCancel : TButton;
    MultiselectCheckBox: TCheckBox;
    procedure ViewUnitsResize(Sender: TObject);
    Procedure btnOKClick(Sender :TObject);
    Procedure btnCancelClick(Sender :TObject);
    procedure MultiselectCheckBoxClick(Sender :TObject);
  public
    constructor Create(AOwner: TComponent); override;	
  end;


function ShowViewUnitsDlg(Entries: TList; MultiSelect: boolean;
  Caption: string): TModalResult;
   // Entries is a list of TViewUnitsEntry(s)


implementation


function ShowViewUnitsDlg(Entries: TList;
  MultiSelect: boolean; Caption: string): TModalResult;
var ViewUnits: TViewUnits;
  i: integer;
begin
  ViewUnits:=TViewUnits.Create(Application);
  try
    ViewUnits.Caption:=Caption;
    ViewUnits.ListBox.Visible:=false;
    ViewUnits.MultiselectCheckBox.Enabled:=MultiSelect;
    with ViewUnits.ListBox.Items do begin
      BeginUpdate;
      Clear;
      for i:=0 to Entries.Count-1 do
        Add(TViewUnitsEntry(Entries[i]).Name);
      EndUpdate;
    end;
    for i:=0 to Entries.Count-1 do
      ViewUnits.ListBox.Selected[i]:=TViewUnitsEntry(Entries[i]).Selected;
    ViewUnits.ListBox.Visible:=true;
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

constructor TViewUnits.Create(AOwner: TComponent);	
var  Pad : Integer;
begin
  inherited Create(AOwner);

  if LazarusResources.Find(Classname)=nil then begin
    Caption := 'View Project Units';
    Width:=325;
    Height:=200;
    Position:=poScreenCenter;
    Pad := 10;
    OnResize:=@ViewUnitsResize;

    btnOK := TButton.Create(Self);
    with btnOk do begin
      Parent := Self;
      Left := Self.Width - 90;
      Top := pad;
      Width := 75;
      Height := 25;
      Caption := 'OK';
      Visible := True;
      OnClick := @btnOKClick;
      Name := 'btnOK';
    end;

    btnCancel := TButton.Create(Self);
    with btnCancel do begin
      Parent := Self;
      Left := Self.Width - 90;
      Top := btnOK.Top + btnOK.Height + pad;
      Width := 75;
      Height := 25;
      Caption := dlgCancel;
      Visible := True;
      Name := 'btnCancel';
      OnClick := @btnCancelClick;
    end;

    Listbox:= TListBox.Create(Self);
    with Listbox do begin
      Parent:= Self;
      Top:= Pad;
      Left:= Pad;
      Width:= Self.Width - (Self.Width - btnOK.Left) - (2*pad);
      Height:= Self.Height - Top - Pad;
      MultiSelect:= false;
      Name := 'Listbox';
      Visible:= true;
    end;
    
    MultiselectCheckBox:=TCheckBox.Create(Self);
    with MultiselectCheckBox do begin
      Parent:=Self;
      Name:='MultiselectCheckBox';
      Left:=btnOK.Left;
      Top:=btnCancel.Top+btnCancel.Height+2*pad;
      Width:=btnOk.Width;
      Height:=25;
      Caption:=dlgMulti;
      Checked:=false;
      OnClick:=@MultiselectCheckBoxClick;
      Visible:=true;
    end;
  end;
  ViewUnitsResize(nil);
end;

procedure TViewUnits.ViewUnitsResize(Sender: TObject);
var Pad: integer;
begin
  Pad:=10;
  
  with btnOk do begin
    Left := Self.Width - 90;
    Top := pad;
    Width := 75;
    Height := 25;
  end;

  with btnCancel do begin
    Left := Self.Width - 90;
    Top := btnOK.Top + btnOK.Height + pad;
    Width := 75;
    Height := 25;
  end;

  with Listbox do begin
    Top:= Pad;
    Left:= Pad;
    Width:= Self.Width - (Self.Width - btnOK.Left) - (2*pad);
    Height:= Self.Height - Top - Pad;
  end;

  with MultiselectCheckBox do begin
    Left:=btnOK.Left;
    Top:=btnCancel.Top+btnCancel.Height+2*pad;
    Width:=btnOk.Width;
    Height:=25;
  end;
end;

Procedure TViewUnits.btnOKClick(Sender : TOBject);
Begin
  ModalResult := mrOK;
End;


Procedure TViewUnits.btnCancelClick(Sender : TOBject);
Begin
  ModalResult := mrCancel;
end;

procedure TViewUnits.MultiselectCheckBoxClick(Sender :TObject);
begin
  ListBox.Multiselect:=MultiselectCheckBox.Checked;
end;


initialization
{ $I viewunits1.lrs}


end.
{
  $Log$
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

  Revision 1.8  2000/05/10 02:34:43  lazarus
  Changed writelns to Asserts except for ERROR and WARNING messages.   CAW

  Revision 1.7  2000/03/24 14:40:41  lazarus
  A little polishing and bug fixing.

  Revision 1.6  2000/03/19 03:52:08  lazarus
  Added onclick events for the speedbuttons.
  Shane

  Revision 1.5  2000/03/03 20:22:02  lazarus
  Trying to add TBitBtn
  Shane

  Revision 1.4  2000/02/24 09:10:12  lazarus
  TListBox.Selected bug fixed.

  Revision 1.3  2000/02/22 21:29:42  lazarus
  Added a few more options in the editor like closeing a unit.  Also am keeping track of what page , if any, they are currently on.
  Shane

  Revision 1.2  2000/02/21 21:08:29  lazarus
  Bug fix in GetCaption.  Added the line to check if a handle is allocated for a csEdit.   Otherwise when creating it, it check's it's caption.  It then sends a LM_GETTEXT and the edit isn't created, so it calls LM_CREATE which in turn checks the caption again, etc.
  Shane

  Revision 1.1  2000/02/21 17:38:04  lazarus
  Added modalresult to TCustomForm
  Added a View Units dialog box
  Added a View Forms dialog box
  Added a New Unit menu selection
  Added a New Form menu selection
  Shane


}


























































