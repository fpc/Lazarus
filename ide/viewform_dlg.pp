{  $Id$  }
{
 /***************************************************************************
                          ViewForm_dlg.pp
                             -------------------
                   TViewForms is the application dialog for displaying all forms in a project.


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
unit ViewForm_Dlg;

{$mode objfpc}

interface

uses
  Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,LResources,buttons,stdctrls;


type
	  
 TViewForms1 = class(TFORM)
    Edit1: TEdit;
    ListBox1: TListBox;
    btnOK : TButton;
    btnCancel : TButton;
    Procedure btnOKClick(Sender : TOBject);
    Procedure btnCancelClick(Sender : TOBject);
    Procedure listbox1Click(Sender : TObject);
protected
public
//    constructor Create(AOwner: TComponent); override;	
end;

var
ViewForms1 : TViewForms1;

implementation

{
constructor TViewForms1.Create(AOwner: TComponent);	
var
Pad : Integer;
begin
  inherited Create(AOwner);
  Caption := 'View Project Forms';
  Left := 0;
  Top := 0;
  Width := 325;
  height := 200;
  Pad := 10;
  position := poScreenCenter;
  Name := 'ViewForms1';

  btnOK := TButton.Create(Self);
  btnOK.Parent := Self;
  btnOK.Left := ClientWidth - 90;
  btnOK.Top := pad;
  btnOK.Width := 75;
  btnOK.Height := 25;
  btnOK.Caption := 'OK';
  btnOK.Visible := True;
  btnOK.Name := 'btnOK';
  btnOK.OnClick := @btnOKClick;

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
  Edit1.Text := 'Edit1';
  Edit1.Name := 'Edit1';


  Listbox1:= TListBox.Create(Self);
  with Listbox1 do begin
    Parent:= Self;
    Top:= Edit1.Height + Edit1.Top + Pad;
    Left:= pad;
    Width:= ClientWidth - (ClientWidth - btnOK.Left) - (2*pad);
    Height:= Self.Height - Top - pad;
    Visible:= true;
    BorderStyle:= bsNone;
    MultiSelect:= false;
    Listbox1.Name := 'Listbox1';
    OnClick :=@listbox1Click;


//    Selected[1]:= true;
  end;

end;
}

Procedure TViewForms1.btnOKClick(Sender : TOBject);
Begin
{
Search the list to see if it is already on a page.
If so, simply set that page to the front.  If not
then load it into a new page.
}

//Close the dialog box.
ModalResult := mrOK;

End;


Procedure TViewForms1.btnCancelClick(Sender : TOBject);
Begin
ModalResult := mrCancel;

end;

Procedure TViewForms1.listbox1Click(Sender : TObject);
Var
I : Integer;
Begin
if Listbox1.Items.Count > 0 then
   Begin
   for i := 0 to Listbox1.Items.Count-1 do
    Begin
     if Listbox1.Selected[I] then
        Begin
	Edit1.Text := Listbox1.Items.Strings[i];
        Break;
        end;
    end;
   end;
end;


initialization
{Do not change the following}
{<LAZARUSFORMDEF>}
{$I viewforms1.lrs}
{<LAZARUSFORMDEFEND>}
{}



end.
{
  $Log$
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

  Revision 1.4  2000/03/19 03:52:08  lazarus
  Added onclick events for the speedbuttons.
  Shane

  Revision 1.3  2000/03/03 20:22:02  lazarus
  Trying to add TBitBtn
  Shane

  Revision 1.2  2000/02/22 21:29:42  lazarus
  Added a few more options in the editor like closeing a unit.  Also am keeping track of what page , if any, they are currently on.
  Shane

  Revision 1.1  2000/02/21 17:38:04  lazarus
  Added modalresult to TCustomForm
  Added a View Units dialog box
  Added a View Forms dialog box
  Added a New Unit menu selection
  Added a New Form menu selection
  Shane


}


























































