{
 /***************************************************************************
                           MsgView.pp - compiler message view
                           ----------------------------------
                   TMessagesView is responsible for displaying the
                   PPC386 compiler messages.


                   Initial Revision  : Mon Apr 17th 2000


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
unit MsgView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, Forms, LResources, IDEOptionDefs,
  EnvironmentOpts;

type

  TMessagesView = class(TForm)
    MessageView : TListBox;
  private
    FLastLineIsProgress: boolean;
    FOnSelectionChanged: TNotifyEvent;
    Function GetMessage: String;
    Procedure MessageViewClicked(sender : TObject);
    procedure SetLastLineIsProgress(const AValue: boolean);
  protected
    Function GetSelectedLineIndex: Integer;
    procedure SetSelectedLineIndex(const AValue: Integer);
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Add(const Msg: String);
    procedure AddSeparator;
    procedure ShowProgress(const Msg: String);
    procedure ClearTillLastSeparator;
    procedure ShowTopMessage;
    function MsgCount: integer;
    procedure Clear;
  public
    property LastLineIsProgress: boolean read FLastLineIsProgress write SetLastLineIsProgress;
    property Message : String read GetMessage;
    property SelectedMessageIndex: Integer
      read GetSelectedLineIndex write SetSelectedLineIndex;
    property OnSelectionChanged : TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

const
  MessagesView : TMessagesView = nil;


implementation

const SeparatorLine = '----------------------------';

{ TMessagesView }


{------------------------------------------------------------------------------
  TMessagesView.Create
------------------------------------------------------------------------------}
constructor TMessagesView.Create(TheOwner : TComponent);
var ALayout: TIDEWindowLayout;
Begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Caption:='Messages';
    MessageView := TListBox.Create(Self);
    With MessageView do Begin
      Parent:= Self;
      Align:= alClient;
      Visible:= true;
    end;
  end;
  Name := DefaultMessagesViewName;
  ALayout:=EnvironmentOptions.IDEWindowLayoutList.
    ItemByFormID(DefaultMessagesViewName);
  ALayout.Form:=TForm(Self);
  ALayout.Apply;
end;

{------------------------------------------------------------------------------
  TMessagesView.Add
------------------------------------------------------------------------------}
Procedure  TMessagesView.Add(const Msg: String);
Begin
  if FLastLineIsProgress then begin
    MessageView.Items[MessageView.Items.Count-1]:=Msg;
    FLastLineIsProgress:=false;
  end else begin
    MessageView.Items.Add(Msg);
  end;
end;

Procedure TMessagesView.AddSeparator;
begin
  Add(SeparatorLine);
end;

procedure TMessagesView.ShowProgress(const Msg: String);
begin
  if FLastLineIsProgress then
    MessageView.Items[MessageView.Items.Count-1]:=Msg
  else begin
    MessageView.Items.Add(Msg);
    FLastLineIsProgress:=true;
  end;
  MessageView.TopIndex:=MessageView.Items.Count-1;
end;

procedure TMessagesView.ClearTillLastSeparator;
var LastSeparator: integer;
begin
  with MessageView do begin
    LastSeparator:=Items.Count-1;
    while (LastSeparator>=0) and (Items[LastSeparator]<>SeparatorLine) do
      dec(LastSeparator);
    if LastSeparator>=0 then begin
      while (Items.Count>LastSeparator) do begin
        Items.Delete(Items.Count-1);
        FLastLineIsProgress:=false;
      end;
    end;
  end;
end;

procedure TMessagesView.ShowTopMessage;
begin
  if MessageView.Items.Count>0 then
    MessageView.TopIndex:=0;
end;

function TMessagesView.MsgCount: integer;
begin
  Result:=MessageView.Items.Count;
end;

{------------------------------------------------------------------------------
  TMessagesView.Clear
------------------------------------------------------------------------------}
Procedure  TMessagesView.Clear;
Begin
  MessageView.Clear;
  FLastLineIsProgress:=false;
  if not Assigned(MessagesView.MessageView.OnClick) then
    MessageView.OnClick := @MessageViewClicked;
end;

{------------------------------------------------------------------------------
  TMessagesView.GetMessage
------------------------------------------------------------------------------}
Function TMessagesView.GetMessage: String;
Begin
  Result := '';
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then
    Result := MessageView.Items.Strings[GetSelectedLineIndex];
end;

Function TMessagesView.GetSelectedLineIndex : Integer;
var
  I : Integer;
Begin
  Result := -1;
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then Begin
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

Procedure TMessagesView.MessageViewClicked(sender : TObject);
begin
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then Begin
    If Assigned(OnSelectionChanged) then
      OnSelectionChanged(self);
  end;
end;

procedure TMessagesView.SetLastLineIsProgress(const AValue: boolean);
begin
  if FLastLineIsProgress=AValue then exit;
  if FLastLineIsProgress then
    MessageView.Items.Delete(MessageView.Items.Count-1);
  FLastLineIsProgress:=AValue;
end;

procedure TMessagesView.SetSelectedLineIndex(const AValue: Integer);
begin
  MessageView.ItemIndex:=AValue;
  MessageView.TopIndex:=MessageView.ItemIndex;
end;

initialization
  { $I msgview.lrs}


end.

