{
 /***************************************************************************
                         MsgView.pp - compiler message view
                         ----------------------------------
                   TMessagesView is responsible for displaying the
                   fpc/make/codetools messages.


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
  Classes, SysUtils, LCLProc, Controls, StdCtrls, Forms, Menus, LResources,
  ClipBrd, Dialogs, InputHistory, FileUtil,
  IDEProcs, IDEOptionDefs, DialogProcs, IDECommands, EnvironmentOpts,
  LazarusIDEStrConsts;

type
  { TMessageLine }

  TMessageLine = class
  private
    FDirectory: string;
    FMsg: string;
    FOriginalIndex: integer;
    FParts: TStrings;
    FPosition: integer;
    FVisiblePosition: integer;
    procedure SetDirectory(const AValue: string);
    procedure SetMsg(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Msg: string read FMsg write SetMsg;
    property Directory: string read FDirectory write SetDirectory;
    property Position: integer read FPosition;
    property VisiblePosition: integer read FVisiblePosition;
    property OriginalIndex: integer read FOriginalIndex;
    property Parts: TStrings read FParts write FParts;
  end;
  

  { TMessagesView }
  
  TOnFilterLine = procedure(MsgLine: TMessageLine; var Show: boolean) of object;
  
  TMessagesView = class(TForm)
    MessageView: TListBox;
    MainPopupMenu: TPopupMenu;
    CopyAllMenuItem: TMenuItem;
    SaveAllToFileMenuItem: TMenuItem;
    procedure CopyAllMenuItemClick(Sender: TObject);
    procedure MessageViewDblClicked(Sender: TObject);
    Procedure MessageViewClicked(sender : TObject);
    procedure MessagesViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SaveAllToFileMenuItemClick(Sender: TObject);
  private
    FItems: TList; // list of TMessageLine
    FVisibleItems: TList; // list of TMessageLine (visible Items of FItems)
    FLastLineIsProgress: boolean;
    FOnSelectionChanged: TNotifyEvent;
    function GetDirectory: string;
    function GetItems(Index: integer): TMessageLine;
    Function GetMessage: String;
    function GetVisibleItems(Index: integer): TMessageLine;
    procedure SetLastLineIsProgress(const AValue: boolean);
  protected
    fBlockCount: integer;
    Function GetSelectedLineIndex: Integer;
    procedure SetSelectedLineIndex(const AValue: Integer);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteLine(Index: integer);
    procedure Add(const Msg, CurDir: String; ProgressLine,
                  VisibleLine: boolean; OriginalIndex: integer);
    procedure AddMsg(const Msg, CurDir: String; OriginalIndex: integer);
    procedure AddProgress(const Msg, CurDir: String);
    procedure AddSeparator;
    procedure ClearTillLastSeparator;
    procedure ShowTopMessage;
    procedure Clear;
    procedure GetVisibleMessageAt(Index: integer; var Msg, MsgDirectory: string);
    procedure BeginBlock;
    procedure EndBlock;
    procedure ClearItems;
    function ItemCount: integer;
    function VisibleItemCount: integer;
    function MsgCount: integer;
    procedure FilterLines(Filter: TOnFilterLine);
    procedure SaveMessagesToFile(const Filename: string);
  public
    property LastLineIsProgress: boolean read FLastLineIsProgress
                                         write SetLastLineIsProgress;
    property Message: String read GetMessage;
    property Directory: string read GetDirectory;
    property SelectedMessageIndex: Integer read GetSelectedLineIndex  // visible index
                                           write SetSelectedLineIndex;
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged
                                              write FOnSelectionChanged;
    property Items[Index: integer]: TMessageLine read GetItems;
    property VisibleItems[Index: integer]: TMessageLine read GetVisibleItems;
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
Begin
  inherited Create(TheOwner);
  Name := NonModalIDEWindowNames[nmiwMessagesViewName];
  FItems:=TList.Create;
  FVisibleItems:=TList.Create;

  Caption:=lisMenuViewMessages;
  MessageView := TListBox.Create(Self);
  With MessageView do Begin
    Parent:= Self;
    Align:= alClient;
  end;
  
  MainPopupMenu:=TPopupMenu.Create(Self);
  MessageView.PopupMenu:=MainPopupMenu;
  
  CopyAllMenuItem:=TMenuItem.Create(Self);
  with CopyAllMenuItem do begin
    Name:='CopyAllMenuItem';
    Caption:=lisCopyAllMessagesToClipboard;
    OnClick:=@CopyAllMenuItemClick;
  end;
  MainPopupMenu.Items.Add(CopyAllMenuItem);
  
  SaveAllToFileMenuItem:=TMenuItem.Create(Self);
  with SaveAllToFileMenuItem do begin
    Name:='SaveAllToFileMenuItem';
    Caption:=lisSaveAllMessagesToFile;
    OnClick:=@SaveAllToFileMenuItemClick;
  end;
  MainPopupMenu.Items.Add(SaveAllToFileMenuItem);

  EnvironmentOptions.IDEWindowLayoutList.Apply(Self,Name);
  KeyPreview:=true;
  OnKeyDown:=@MessagesViewKeyDown;
end;

destructor TMessagesView.Destroy;
begin
  ClearItems;
  FreeThenNil(FItems);
  FreeThenNil(FVisibleItems);
  inherited Destroy;
end;

procedure TMessagesView.DeleteLine(Index: integer);
var
  Line: TMessageLine;
  VisibleIndex: Integer;
  i: Integer;
begin
  Line:=Items[Index];
  FItems.Delete(Line.Position);
  VisibleIndex:=Line.VisiblePosition;
  if VisibleIndex>=0 then begin
    MessageView.Items.Delete(VisibleIndex);
    FVisibleItems.Delete(VisibleIndex);
  end;
  Line.Free;
  // adjust Positions
  for i:=Index to FItems.Count-1 do begin
    Line:=Items[i];
    dec(Line.FPosition);
    if Line.VisiblePosition>VisibleIndex then
      dec(Line.FVisiblePosition);
  end;
end;

{------------------------------------------------------------------------------
  TMessagesView.Add
------------------------------------------------------------------------------}
Procedure TMessagesView.Add(const Msg, CurDir: String; ProgressLine,
  VisibleLine: boolean; OriginalIndex: integer);
var
  NewMsg: TMessageLine;
  i: Integer;
Begin
  NewMsg:=TMessageLine.Create;
  NewMsg.Msg:=Msg;
  NewMsg.Directory:=CurDir;
  NewMsg.FPosition:=FItems.Count;
  NewMsg.FOriginalIndex:=OriginalIndex;
  FItems.Add(NewMsg);

  if VisibleLine then begin
    if FLastLineIsProgress then begin
      // replace old progress line
      i:=FVisibleItems.Count-1;
      VisibleItems[i].FVisiblePosition:=-1;
      FVisibleItems.Delete(i);
      MessageView.Items[i]:=Msg;
    end else begin
      // add line
      MessageView.Items.Add(Msg);
    end;
    NewMsg.FVisiblePosition:=FVisibleItems.Count;
    FVisibleItems.Add(NewMsg);
    FLastLineIsProgress:=ProgressLine;
    MessageView.TopIndex:=MessageView.Items.Count-1;
  end;
end;

procedure TMessagesView.AddMsg(const Msg, CurDir: String;
  OriginalIndex: integer);
begin
  Add(Msg,CurDir,false,true,OriginalIndex);
end;

procedure TMessagesView.AddProgress(const Msg, CurDir: String);
begin
  Add(Msg,CurDir,true,true,-1);
end;

Procedure TMessagesView.AddSeparator;
begin
  Add(SeparatorLine,'',false,true,-1);
end;

procedure TMessagesView.ClearTillLastSeparator;
var LastSeparator: integer;
begin
  BeginBlock;
  LastSeparator:=VisibleItemCount-1;
  while (LastSeparator>=0)
  and (VisibleItems[LastSeparator].Msg<>SeparatorLine) do
    dec(LastSeparator);
  if LastSeparator>=0 then begin
    while (VisibleItemCount>LastSeparator) do
      DeleteLine(ItemCount-1);
    FLastLineIsProgress:=false;
  end;
  EndBlock;
end;

procedure TMessagesView.ShowTopMessage;
begin
  if MessageView.Items.Count>0 then
    MessageView.TopIndex:=0;
end;

function TMessagesView.MsgCount: integer;
begin
  Result:=VisibleItemCount;
end;

procedure TMessagesView.FilterLines(Filter: TOnFilterLine);
// recalculate visible lines
var
  i: Integer;
  Line: TMessageLine;
  ShowLine: Boolean;
begin
  // remove temporary lines
  ClearTillLastSeparator;
  FLastLineIsProgress:=false;
  // recalculate visible lines
  FVisibleItems.Clear;
  for i:=0 to FItems.Count-1 do begin
    Line:=Items[i];
    ShowLine:=true;
    Filter(Line,ShowLine);
    if ShowLine then begin
      Line.FVisiblePosition:=FVisibleItems.Count;
      FVisibleItems.Add(Line);
    end else
      Line.FVisiblePosition:=-1;
  end;
  // rebuild MessageView.Items
  MessageView.Items.BeginUpdate;
  for i:=0 to FVisibleItems.Count-1 do begin
    Line:=VisibleItems[i];
    if MessageView.Items.Count>i then
      MessageView.Items[i]:=Line.Msg
    else
      MessageView.Items.Add(Line.Msg);
  end;
  while MessageView.Items.Count>FVisibleItems.Count do
    MessageView.Items.Delete(MessageView.Items.Count-1);
  MessageView.Items.EndUpdate;
end;

procedure TMessagesView.SaveMessagesToFile(const Filename: string);
begin
  SaveStringToFile(Filename,MessageView.Items.Text,[]);
end;

{------------------------------------------------------------------------------
  TMessagesView.Clear
------------------------------------------------------------------------------}
Procedure TMessagesView.Clear;
Begin
  if fBlockCount>0 then exit;
  FLastLineIsProgress:=false;
  ClearItems;
  if not Assigned(MessageView.OnClick) then
    MessageView.OnClick := @MessageViewClicked;
  if not Assigned(MessageView.OnDblClick) then
    MessageView.OnDblClick :=@MessageViewDblClicked;
end;

procedure TMessagesView.GetVisibleMessageAt(Index: integer;
  var Msg, MsgDirectory: string);
begin
  // consistency checks
  if (Index<0) then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if MessageView.Items.Count<=Index then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if (FItems=nil) then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if (FItems.Count<=Index) then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  Msg:=VisibleItems[Index].Msg;
  MsgDirectory:=VisibleItems[Index].Directory;
end;

procedure TMessagesView.BeginBlock;
begin
  Clear;
  inc(fBlockCount);
end;

procedure TMessagesView.EndBlock;
begin
  if fBlockCount<=0 then RaiseException('TMessagesView.EndBlock Internal Error');
  dec(fBlockCount);
end;

procedure TMessagesView.ClearItems;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do TObject(FItems[i]).Free;
  FItems.Clear;
  FVisibleItems.Clear;
  MessageView.Clear;
end;

function TMessagesView.ItemCount: integer;
begin
  Result:=FItems.Count;
end;

function TMessagesView.VisibleItemCount: integer;
begin
  Result:=FVisibleItems.Count;
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

function TMessagesView.GetVisibleItems(Index: integer): TMessageLine;
begin
  Result:=TMessageLine(FVisibleItems[Index]);
end;

procedure TMessagesView.MessageViewDblClicked(Sender: TObject);
begin
  if not EnvironmentOptions.MsgViewDblClickJumps then exit;
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then Begin
    If Assigned(OnSelectionChanged) then
      OnSelectionChanged(self);
  end;
end;

procedure TMessagesView.CopyAllMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText:=MessageView.Items.Text;
end;

Procedure TMessagesView.MessageViewClicked(sender : TObject);
begin
  if EnvironmentOptions.MsgViewDblClickJumps then exit;
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then Begin
    If Assigned(OnSelectionChanged) then
      OnSelectionChanged(self);
  end;
end;

procedure TMessagesView.MessagesViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  //debugln('TMessagesView.MessagesViewKeyDown ',dbgs(Key));
  ExecuteIDECommand(Self,Key,Shift);
end;

procedure TMessagesView.SaveAllToFileMenuItemClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  AFilename: string;
begin
  SaveDialog:=TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.Title:='Save messages to file (*.txt)';
    SaveDialog.Options:=SaveDialog.Options+[ofPathMustExist];
    if SaveDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(SaveDialog.Filename);
      if ExtractFileExt(AFilename)='' then
        AFilename:=AFilename+'.txt';
      SaveMessagesToFile(AFilename);
    end;
    InputHistories.StoreFileDialogSettings(SaveDialog);
  finally
    SaveDialog.Free;
  end;
end;

function TMessagesView.GetDirectory: string;
var
  i: Integer;
begin
  Result := '';
  i:=GetSelectedLineIndex;
  if (FVisibleItems.Count>i) then
    Result := VisibleItems[i].Msg;
end;

function TMessagesView.GetItems(Index: integer): TMessageLine;
begin
  Result:=TMessageLine(FItems[Index]);
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

{ TMessageLine }

procedure TMessageLine.SetDirectory(const AValue: string);
begin
  if FDirectory=AValue then exit;
  FDirectory:=AValue;
end;

procedure TMessageLine.SetMsg(const AValue: string);
begin
  if FMsg=AValue then exit;
  FMsg:=AValue;
end;

constructor TMessageLine.Create;
begin
  FPosition:=-1;
  FVisiblePosition:=-1;
end;

destructor TMessageLine.Destroy;
begin
  FParts.Free;
  inherited Destroy;
end;

initialization
  MessagesView:=nil;

end.

