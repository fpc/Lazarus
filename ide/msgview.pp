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
    Function GetMessage : String;
    Procedure MessageViewClicked(sender : TObject);
    FOnSelectionChanged : TNotifyEvent;
    LastSelectedIndex : Integer;
  protected
    Function GetSelectedLineIndex : Integer;
  public
    constructor Create(AOwner : TComponent); override;
    procedure Add(const Texts : String);
    procedure AddSeparator;
    procedure ClearTillLastSeparator;
    function MsgCount: integer;
    procedure Clear;
    property Message : String read GetMessage;
    property SelectedMessageIndex : Integer read GetSelectedLineIndex;
    property OnSelectionChanged : TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
  end;

const
  MessagesView : TMessagesView = nil;


implementation

const SeparatorLine = '----------------------------';

{ TMessagesView }


{------------------------------------------------------------------------------}
{  TMessagesView.Create                                                        }
{------------------------------------------------------------------------------}
constructor TMessagesView.Create(AOwner : TComponent);
var ALayout: TIDEWindowLayout;
Begin
  inherited Create(AOwner);
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
  LastSelectedIndex := -1;
  ALayout:=EnvironmentOptions.IDEWindowLayoutList.
    ItemByFormID(DefaultMessagesViewName);
  ALayout.Form:=TForm(Self);
  ALayout.Apply;
end;


{------------------------------------------------------------------------------}
{  TMessagesView.Add                                                           }
{------------------------------------------------------------------------------}
Procedure  TMessagesView.Add(const Texts : String);
Begin
  MessageView.Items.Add(Texts);
end;

Procedure TMessagesView.AddSeparator;
begin
  Add(SeparatorLine);
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
      end;
    end;
  end;
end;

function TMessagesView.MsgCount: integer;
begin
  Result:=MessageView.Items.Count;
end;

{------------------------------------------------------------------------------}
{  TMessagesView.Clear                                                           }
{------------------------------------------------------------------------------}
Procedure  TMessagesView.Clear;
Begin
  MessageView.Clear;

  if not Assigned(MessagesView.MessageView.OnCLick) then //:= @MessagesView.MessageViewClicked;
     MessageView.OnClick := @MessageViewClicked;
end;

{------------------------------------------------------------------------------}
{  TMessagesView.GetMessage                                                           }
{------------------------------------------------------------------------------}
Function  TMessagesView.GetMessage : String;
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
var
  Temp : Integer;  //this temporarily holds the line # of the selection
begin
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then
      Begin
         Temp := GetSelectedLineIndex;
         if Temp <> LastSelectedIndex then
            Begin
               LastSelectedIndex := Temp;
               If Assigned(OnSelectionChanged) then
                  OnSelectionChanged(self);
             end;

      end;
      
end;

initialization
  { $I msgview.lrs}


end.

