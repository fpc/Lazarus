{
 /***************************************************************************
                          dlgMEssage.pp  -
                             -------------------
                   TMessagedlg is responsible for displaying the
                   PPC386 compiler messages.


                   Initial Revision  : Mon Apr 17th 2000


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
unit dlgMessage;

{$mode objfpc}

interface

uses
  classes, sysutils, controls, stdctrls,forms;

 type

TMessageDlg = class(TForm)
   MessageView : TListBox;
   private
     Function GetMessage : String;
   public
     constructor Create(AOwner : TComponent);
     Procedure Add(Texts : String);
     Procedure Clear;
     Function GetSelectedLineIndex : Integer;
     property Message : String read GetMessage;
   end;

var
  MessageDlg : TMessageDlg;
implementation



{------------------------------------------------------------------------------}
{  TMessageDlg.Create                                                           }
{------------------------------------------------------------------------------}
constructor TMessageDlg.Create(AOwner : TComponent);
Begin
inherited Create(AOwner);

MessageView := TListBox.Create(Self);
  With MessageView do
   Begin
    Parent:= Self;
    Align:= alClient;
    Visible:= true;
   end;
end;

{------------------------------------------------------------------------------}
{  TMessageDlg.Add                                                           }
{------------------------------------------------------------------------------}
Procedure  TMessageDlg.Add(Texts : String);
Begin
MessageView.Items.Add(Texts);
end;

{------------------------------------------------------------------------------}
{  TMessageDlg.Clear                                                           }
{------------------------------------------------------------------------------}
Procedure  TMessageDlg.Clear;
Begin
MEssageView.Items.Clear;
end;

{------------------------------------------------------------------------------}
{  TMessageDlg.GetMessage                                                           }
{------------------------------------------------------------------------------}
Function  TMessageDlg.GetMessage : String;
var
I : Integer;
Begin
Result := '';
if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then
   Begin
   for i := 0 to MessageView.Items.Count-1 do
    Begin
     if MessageView.Selected[I] then
        Begin
	Result := MessageView.Items.Strings[i];
        Break;
        end;
    end;
   end;
end;

Function TMessageDlg.GetSelectedLineIndex : Integer;
var
I : Integer;
Begin
Result := -1;
if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then
   Begin
   for i := 0 to MessageView.Items.Count-1 do
    Begin
     if MessageView.Selected[I] then
        Begin
	Result := I;
        Break;
        end;
    end;
   end;
end;

end.

