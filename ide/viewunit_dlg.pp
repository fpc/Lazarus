{  $Id$  }
{
 /***************************************************************************
                          ViewUnit_dlg.pp
                             -------------------
                   TViewUnit is the application dialog for displaying all units in a project.


                   Initial Revision  : Sat Feb 19 17:42 CST 1999


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
{$H+}
unit ViewUnit_Dlg;

{$mode objfpc}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,LResources,buttons,stdctrls;


type
	  
 TViewUnits1 = class(TFORM)
    Edit1: TEdit;
    ListBox1: TListBox;
    btnOK : TButton;
    btnCancel : TButton;
    Procedure btnOKClick(Sender : TOBject);
    Procedure btnCancelClick(Sender : TOBject);
    Procedure Listbox1Click(Sender : TObject);
protected
public
  //  constructor Create(AOwner: TComponent); override;	
end;

var
ViewUnits1 : TViewUnits1;

implementation

  {
constructor TViewUnits1.Create(AOwner: TComponent);	
var
Pad : Integer;
begin

  inherited Create(AOwner);
  Caption := 'View Project Units';
  Left := 0;
  Top := 0;
  Width := 325;
  height := 200;
  Pad := 10;
  position := poScreenCenter;
  Name := 'ViewUnits1';

  btnOK := TButton.Create(Self);
  btnOK.Parent := Self;
  btnOK.Left := ClientWidth - 90;
  btnOK.Top := pad;
  btnOK.Width := 75;
  btnOK.Height := 25;
  btnOK.Caption := 'OK';
  btnOK.Visible := True;
  btnOK.OnClick := @btnOKClick;
  btnOK.Name := 'btnOK';

  btnCancel := TButton.Create(Self);
  btnCancel.Parent := Self;
  btnCancel.Left := ClientWidth - 90;
  btnCancel.Top := btnOK.Top + btnOK.Height + pad;
  btnCancel.Width := 75;
  btnCancel.Height := 25;
  btnCancel.Caption := 'Cancel';
  btnCancel.Visible := True;
  btnCancel.Name := 'btnCancel';
  btnCancel.OnClick := @btnCancelClick;

  Edit1 := TEdit.Create(Self);
  Edit1.Parent := Self;
  Edit1.Left := pad;
  Edit1.Top := pad;
  edit1.Width := ClientWidth - (ClientWidth - btnOK.Left) - (2*pad);
  Edit1.Height := 25;
  Edit1.Visible := True;
  Edit1.Name := 'Edit1';

  Listbox1:= TListBox.Create(Self);
  with Listbox1 do begin
    Parent:= Self;
    Top:= Edit1.Height + Edit1.Top + Pad;
    Left:= pad;
    Width:= ClientWidth - (ClientWidth - btnOK.Left) - (2*pad);
    Height:= Self.Height - Top - pad;
    Visible:= true;
    MultiSelect:= false;
    Name := 'Listbox1';
    OnClick := @ListBox1Click;
  end;



end;
       }

Procedure TViewUnits1.btnOKClick(Sender : TOBject);
Begin
{
Search the list to see if it is already on a page.
If so, simply set that page to the front.  If not
then load it into a new page.
}
modalresult := mrOK;
End;


Procedure TViewUnits1.btnCancelClick(Sender : TOBject);
Begin
ModalResult := mrCancel;
end;

Procedure TViewUnits1.listbox1Click(Sender : TObject);
Var
I : Integer;
Begin
if Listbox1.Items.Count > 0 then
   Begin
   for i := 0 to Listbox1.Items.Count-1 do
    Begin
     if Listbox1.Selected[I] then
        Begin
	Assert(False, 'Trace:Selected index is '+ IntToStr(i) + ' and test is ' + Listbox1.Items.Strings[i]);
	Edit1.Text := Listbox1.Items.Strings[i];
        Break;
        end;
    end;
   end;
end;



initialization
{Do not change the following}
{<LAZARUSFORMDEF>}
{$I viewunits1.lrs}
{<LAZARUSFORMDEFEND>}
{}


end.
{
  $Log$
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


























































