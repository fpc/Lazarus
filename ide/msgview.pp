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
  Classes, SysUtils, Controls, StdCtrls, Forms, LResources, IDEProcs,
  IDEOptionDefs, EnvironmentOpts, LazarusIDEStrConsts;

type

  TMessagesView = class(TForm)
    MessageView : TListBox;
    procedure MessageViewDblClicked(Sender: TObject);
    Procedure MessageViewClicked(sender : TObject);
  private
    FDirectories: TStringList;
    FLastLineIsProgress: boolean;
    FOnSelectionChanged: TNotifyEvent;
    function GetDirectory: string;
    Function GetMessage: String;
    procedure SetLastLineIsProgress(const AValue: boolean);
  protected
    Function GetSelectedLineIndex: Integer;
    procedure SetSelectedLineIndex(const AValue: Integer);
    procedure SetMsgDirectory(Index: integer; const CurDir: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Add(const Msg, CurDir: String; ProgressLine: boolean);
    procedure AddMsg(const Msg, CurDir: String);
    procedure AddProgress(const Msg, CurDir: String);
    procedure AddSeparator;
    procedure ClearTillLastSeparator;
    procedure ShowTopMessage;
    function MsgCount: integer;
    procedure Clear;
    procedure GetMessageAt(Index: integer; var Msg, MsgDirectory: string);
  public
    property LastLineIsProgress: boolean read FLastLineIsProgress
                                         write SetLastLineIsProgress;
    property Message: String read GetMessage;
    property Directory: string read GetDirectory;
    property SelectedMessageIndex: Integer read GetSelectedLineIndex
                                           write SetSelectedLineIndex;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged
                                              write FOnSelectionChanged;
  end;

var
  MessagesView: TMessagesView;


implementation

const SeparatorLine = '---------------------------------------------';

{ TMessagesView }


{------------------------------------------------------------------------------
  TMessagesView.Create
------------------------------------------------------------------------------}
constructor TMessagesView.Create(TheOwner : TComponent);
var ALayout: TIDEWindowLayout;
Begin
  inherited Create(TheOwner);
  if LazarusResources.Find(ClassName)=nil then begin
    Caption:=lisMenuViewMessages;
    MessageView := TListBox.Create(Self);
    With MessageView do Begin
      Parent:= Self;
      Align:= alClient;
    end;
  end;
  Name := NonModalIDEWindowNames[nmiwMessagesViewName];
  ALayout:=EnvironmentOptions.IDEWindowLayoutList.
                                               ItemByEnum(nmiwMessagesViewName);
  ALayout.Form:=TForm(Self);
  ALayout.Apply;
end;

destructor TMessagesView.Destroy;
begin
  FreeAndNil(FDirectories);
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TMessagesView.Add
------------------------------------------------------------------------------}
Procedure TMessagesView.Add(const Msg, CurDir: String; ProgressLine: boolean);
var
  i: Integer;
Begin
  if FLastLineIsProgress then begin
    MessageView.Items[MessageView.Items.Count-1]:=Msg;
  end else begin
    MessageView.Items.Add(Msg);
  end;
  FLastLineIsProgress:=ProgressLine;
  i:=MessageView.Items.Count-1;
  SetMsgDirectory(i,CurDir);
  MessageView.TopIndex:=MessageView.Items.Count-1;
end;

procedure TMessagesView.AddMsg(const Msg, CurDir: String);
begin
  Add(Msg,CurDir,false);
end;

procedure TMessagesView.AddProgress(const Msg, CurDir: String);
begin
  Add(Msg,CurDir,true);
end;

Procedure TMessagesView.AddSeparator;
begin
  Add(SeparatorLine,'',false);
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
  if not Assigned(MessagesView.MessageView.OnDblClick) then
    MessageView.OnDblClick :=@MessageViewDblClicked;
end;

procedure TMessagesView.GetMessageAt(Index: integer;
  var Msg, MsgDirectory: string);
begin
  // consistency checks
  if (Index<0) then
    RaiseException('TMessagesView.GetMessageAt');
  if MessageView.Items.Count<=Index then
    RaiseException('TMessagesView.GetMessageAt');
  if (FDirectories=nil) then
    RaiseException('TMessagesView.GetMessageAt');
  if (FDirectories.Count<=Index) then
    RaiseException('TMessagesView.GetMessageAt');
  Msg:=MessageView.Items[Index];
  MsgDirectory:=FDirectories[Index];
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

procedure TMessagesView.MessageViewDblClicked(Sender: TObject);
begin
  if not EnvironmentOptions.MsgViewDblClickJumps then exit;
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then Begin
    If Assigned(OnSelectionChanged) then
      OnSelectionChanged(self);
  end;
end;

Procedure TMessagesView.MessageViewClicked(sender : TObject);
begin
  if EnvironmentOptions.MsgViewDblClickJumps then exit;
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then Begin
    If Assigned(OnSelectionChanged) then
      OnSelectionChanged(self);
  end;
end;

function TMessagesView.GetDirectory: string;
var
  i: Integer;
begin
  Result := '';
  i:=GetSelectedLineIndex;
  if (FDirectories<>nil) and (FDirectories.Count>i) then
    Result := FDirectories[i];
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

procedure TMessagesView.SetMsgDirectory(Index: integer; const CurDir: string);
begin
  if FDirectories=nil then FDirectories:=TStringList.Create;
  while FDirectories.Count<=Index do FDirectories.Add('');
  FDirectories[Index]:=CurDir;
end;

initialization
  MessagesView:=nil;
  { $I msgview.lrs}


end.

