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
  Classes,
  ClipBrd,
  Controls,
  DialogProcs,
  Dialogs,
  EnvironmentOpts,
  FileUtil,
  Forms,
  IDECommands,
  IDEOptionDefs,
  IDEProcs,
  InputHistory,
  KeyMapping,
  LazarusIDEStrConsts,
  LCLProc,
  LResources,
  MenuIntf,
  Menus,
  StdCtrls,
  SysUtils;

type
  { TMessageLine }
  TMessageLine = class
  private
    FDirectory: string;
    FMsg:      string;
    FOriginalIndex: integer;
    FParts:    TStrings;
    FPosition: integer;
    FVisiblePosition: integer;
    procedure SetDirectory(const AValue: string);
    procedure SetMsg(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    property Msg: string Read FMsg Write SetMsg;
    property Directory: string Read FDirectory Write SetDirectory;
    property Position: integer Read FPosition;
    property VisiblePosition: integer Read FVisiblePosition;
    property OriginalIndex: integer Read FOriginalIndex;
    property Parts: TStrings Read FParts Write FParts;
  end;

  TOnFilterLine = procedure(MsgLine: TMessageLine; var Show: boolean) of object;

  { TMessagesView }
  TMessagesView = class(TForm)
    MessageView:   TListBox;
    MainPopupMenu: TPopupMenu;
    procedure CopyAllMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure HelpMenuItemClick(Sender: TObject);
    procedure MessageViewDblClicked(Sender: TObject);
    procedure MessageViewClicked(Sender: TObject);
    procedure MessagesViewKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure SaveAllToFileMenuItemClick(Sender: TObject);
  private
    FItems: TList; // list of TMessageLine
    FVisibleItems: TList; // list of TMessageLine (visible Items of FItems)
    FLastLineIsProgress: boolean;
    FOnSelectionChanged: TNotifyEvent;
    function GetDirectory: string;
    function GetItems(Index: integer): TMessageLine;
    function GetMessage: string;
    function GetVisibleItems(Index: integer): TMessageLine;
    procedure SetLastLineIsProgress(const AValue: boolean);
  protected
    fBlockCount: integer;
    function GetSelectedLineIndex: integer;
    procedure SetSelectedLineIndex(const AValue: integer);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteLine(Index: integer);
    procedure Add(const Msg, CurDir: string;
      ProgressLine, VisibleLine: boolean; OriginalIndex: integer);
    procedure AddMsg(const Msg, CurDir: string; OriginalIndex: integer);
    procedure AddProgress(const Msg, CurDir: string);
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
    property LastLineIsProgress: boolean Read FLastLineIsProgress
      Write SetLastLineIsProgress;
    property Message: string Read GetMessage;
    property Directory: string Read GetDirectory;
    property SelectedMessageIndex: integer Read GetSelectedLineIndex  // visible index
      Write SetSelectedLineIndex;
    property OnSelectionChanged: TNotifyEvent
      Read FOnSelectionChanged Write FOnSelectionChanged;
    property Items[Index: integer]: TMessageLine Read GetItems;
    property VisibleItems[Index: integer]: TMessageLine Read GetVisibleItems;
  end;

var
  MessagesView: TMessagesView;
  MsgCopyIDEMenuCommand: TIDEMenuCommand;
  MsgCopyAllIDEMenuCommand: TIDEMenuCommand;
  MsgHelpIDEMenuCommand: TIDEMenuCommand;
  MsgSaveAllToFileIDEMenuCommand: TIDEMenuCommand;

const
  MessagesMenuRootName = 'Messages';

procedure RegisterStandardMessagesViewMenuItems;

implementation

const
  SeparatorLine = '---------------------------------------------';

procedure RegisterStandardMessagesViewMenuItems;
var
  Path: string;
begin
  MessagesMenuRoot := RegisterIDEMenuRoot(MessagesMenuRootName);
  Path := MessagesMenuRoot.Name;
  MsgCopyIDEMenuCommand := RegisterIDEMenuCommand(Path, 'Copy selected',
    lisCopySelectedMessagesToClipboard);
  MsgCopyAllIDEMenuCommand := RegisterIDEMenuCommand(Path, 'Copy all',
    lisCopyAllMessagesToClipboard);
  MsgHelpIDEMenuCommand := RegisterIDEMenuCommand(Path, 'Help',
    srVK_HELP);
  MsgSaveAllToFileIDEMenuCommand :=
    RegisterIDEMenuCommand(Path, 'Copy selected',
    lisSaveAllMessagesToFile);
end;

{------------------------------------------------------------------------------
  TMessagesView.Create
------------------------------------------------------------------------------}
constructor TMessagesView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name   := NonModalIDEWindowNames[nmiwMessagesViewName];
  FItems := TList.Create;
  FVisibleItems := TList.Create;

  Caption := lisMenuViewMessages;

  // assign the root TMenuItem to the registered menu root.
  // This will automatically create all registered items
  MessagesMenuRoot.MenuItem := MainPopupMenu.Items;
  //MainPopupMenu.Items.WriteDebugReport('TMessagesView.Create ');

  MsgHelpIDEMenuCommand.OnClick    := @HelpMenuItemClick;
  MsgCopyIDEMenuCommand.OnClick    := @CopyMenuItemClick;
  MsgCopyAllIDEMenuCommand.OnClick := @CopyAllMenuItemClick;
  MsgSaveAllToFileIDEMenuCommand.OnClick := @SaveAllToFileMenuItemClick;

  EnvironmentOptions.IDEWindowLayoutList.Apply(Self, Name);
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
  VisibleIndex: integer;
  i:    integer;
begin
  Line := Items[Index];
  FItems.Delete(Line.Position);
  VisibleIndex := Line.VisiblePosition;
  if VisibleIndex >= 0 then
  begin
    MessageView.Items.Delete(VisibleIndex);
    FVisibleItems.Delete(VisibleIndex);
  end;
  Line.Free;
  // adjust Positions
  for i := Index to FItems.Count - 1 do
  begin
    Line := Items[i];
    Dec(Line.FPosition);
    if Line.VisiblePosition > VisibleIndex then
      Dec(Line.FVisiblePosition);
  end;
end;

{------------------------------------------------------------------------------
  TMessagesView.Add
------------------------------------------------------------------------------}
procedure TMessagesView.Add(const Msg, CurDir: string;
  ProgressLine, VisibleLine: boolean; OriginalIndex: integer);
var
  NewMsg: TMessageLine;
  i:      integer;
begin
  NewMsg     := TMessageLine.Create;
  NewMsg.Msg := Msg;
  NewMsg.Directory := CurDir;
  NewMsg.FPosition := FItems.Count;
  NewMsg.FOriginalIndex := OriginalIndex;
  FItems.Add(NewMsg);

  if VisibleLine then
  begin
    if FLastLineIsProgress then
    begin
      // replace old progress line
      i := FVisibleItems.Count - 1;
      VisibleItems[i].FVisiblePosition := -1;
      FVisibleItems.Delete(i);
      MessageView.Items[i] := Msg;
    end
    else
      MessageView.Items.Add(Msg)// add line
    ;
    NewMsg.FVisiblePosition := FVisibleItems.Count;
    FVisibleItems.Add(NewMsg);
    FLastLineIsProgress  := ProgressLine;
    MessageView.TopIndex := MessageView.Items.Count - 1;
  end;
end;

procedure TMessagesView.AddMsg(const Msg, CurDir: string; OriginalIndex: integer);
begin
  Add(Msg, CurDir, False, True, OriginalIndex);
end;

procedure TMessagesView.AddProgress(const Msg, CurDir: string);
begin
  Add(Msg, CurDir, True, True, -1);
end;

procedure TMessagesView.AddSeparator;
begin
  Add(SeparatorLine, '', False, True, -1);
end;

procedure TMessagesView.ClearTillLastSeparator;
var
  LastSeparator: integer;
begin
  BeginBlock;
  LastSeparator := VisibleItemCount - 1;
  while (LastSeparator >= 0) and (VisibleItems[LastSeparator].Msg <> SeparatorLine) do
    Dec(LastSeparator);
  if LastSeparator >= 0 then
  begin
    while (VisibleItemCount > LastSeparator) do
      DeleteLine(ItemCount - 1);
    FLastLineIsProgress := False;
  end;
  EndBlock;
end;

procedure TMessagesView.ShowTopMessage;
begin
  if MessageView.Items.Count > 0 then
    MessageView.TopIndex := 0;
end;

function TMessagesView.MsgCount: integer;
begin
  Result := VisibleItemCount;
end;

procedure TMessagesView.FilterLines(Filter: TOnFilterLine);
// recalculate visible lines
var
  i:    integer;
  Line: TMessageLine;
  ShowLine: boolean;
begin
  // remove temporary lines
  ClearTillLastSeparator;
  FLastLineIsProgress := False;
  // recalculate visible lines
  FVisibleItems.Clear;
  for i := 0 to FItems.Count - 1 do
  begin
    Line     := Items[i];
    ShowLine := True;
    Filter(Line, ShowLine);
    if ShowLine then
    begin
      Line.FVisiblePosition := FVisibleItems.Count;
      FVisibleItems.Add(Line);
    end
    else
      Line.FVisiblePosition := -1;
  end;
  // rebuild MessageView.Items
  MessageView.Items.BeginUpdate;
  for i := 0 to FVisibleItems.Count - 1 do
  begin
    Line := VisibleItems[i];
    if MessageView.Items.Count > i then
      MessageView.Items[i] := Line.Msg
    else
      MessageView.Items.Add(Line.Msg);
  end;
  while MessageView.Items.Count > FVisibleItems.Count do
    MessageView.Items.Delete(MessageView.Items.Count - 1);
  MessageView.Items.EndUpdate;
end;

procedure TMessagesView.SaveMessagesToFile(const Filename: string);
begin
  SaveStringToFile(Filename, MessageView.Items.Text, []);
end;

{------------------------------------------------------------------------------
  TMessagesView.Clear
------------------------------------------------------------------------------}
procedure TMessagesView.Clear;
begin
  if fBlockCount > 0 then
    exit;
  FLastLineIsProgress := False;
  ClearItems;
  if not Assigned(MessageView.OnClick) then
    MessageView.OnClick := @MessageViewClicked;
  if not Assigned(MessageView.OnDblClick) then
    MessageView.OnDblClick := @MessageViewDblClicked;
end;

procedure TMessagesView.GetVisibleMessageAt(Index: integer;
  var Msg, MsgDirectory: string);
begin
  // consistency checks
  if (Index < 0) then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if MessageView.Items.Count <= Index then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if (FItems = nil) then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if (FItems.Count <= Index) then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  Msg := VisibleItems[Index].Msg;
  MsgDirectory := VisibleItems[Index].Directory;
end;

procedure TMessagesView.BeginBlock;
begin
  Clear;
  Inc(fBlockCount);
end;

procedure TMessagesView.EndBlock;
begin
  if fBlockCount <= 0 then
    RaiseException('TMessagesView.EndBlock Internal Error');
  Dec(fBlockCount);
end;

procedure TMessagesView.ClearItems;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
  FVisibleItems.Clear;
  MessageView.Clear;
end;

function TMessagesView.ItemCount: integer;
begin
  Result := FItems.Count;
end;

function TMessagesView.VisibleItemCount: integer;
begin
  Result := FVisibleItems.Count;
end;

{------------------------------------------------------------------------------
  TMessagesView.GetMessage
------------------------------------------------------------------------------}
function TMessagesView.GetMessage: string;
begin
  Result := '';
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then
    Result := MessageView.Items.Strings[GetSelectedLineIndex];
end;

function TMessagesView.GetVisibleItems(Index: integer): TMessageLine;
begin
  Result := TMessageLine(FVisibleItems[Index]);
end;

procedure TMessagesView.MessageViewDblClicked(Sender: TObject);
begin
  if not EnvironmentOptions.MsgViewDblClickJumps then
    exit;
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then
    if Assigned(OnSelectionChanged) then
      OnSelectionChanged(self);
end;

procedure TMessagesView.CopyAllMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText := MessageView.Items.Text;
end;

procedure TMessagesView.CopyMenuItemClick(Sender: TObject);
begin
  if MessageView.ItemIndex < 0 then
    exit;
  Clipboard.AsText := MessageView.GetSelectedText;
end;

procedure TMessagesView.HelpMenuItemClick(Sender: TObject);
begin
  ExecuteIDECommand(Self, ecContextHelp);
end;

procedure TMessagesView.MessageViewClicked(Sender: TObject);
begin
  if EnvironmentOptions.MsgViewDblClickJumps then
    exit;
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then
    if Assigned(OnSelectionChanged) then
      OnSelectionChanged(self);
end;

procedure TMessagesView.MessagesViewKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  //debugln('TMessagesView.MessagesViewKeyDown ',dbgs(Key));
  ExecuteIDEShortCut(Self, Key, Shift);
end;

procedure TMessagesView.SaveAllToFileMenuItemClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  AFilename:  string;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.Title   := 'Save messages to file (*.txt)';
    SaveDialog.Options := SaveDialog.Options + [ofPathMustExist];
    if SaveDialog.Execute then
    begin
      AFilename := CleanAndExpandFilename(SaveDialog.Filename);
      if ExtractFileExt(AFilename) = '' then
        AFilename := AFilename + '.txt';
      SaveMessagesToFile(AFilename);
    end;
    InputHistories.StoreFileDialogSettings(SaveDialog);
  finally
    SaveDialog.Free;
  end;
end;

function TMessagesView.GetDirectory: string;
var
  i: integer;
begin
  Result := '';
  i      := GetSelectedLineIndex;
  if (FVisibleItems.Count > i) then
    Result := VisibleItems[i].Msg;
end;

function TMessagesView.GetItems(Index: integer): TMessageLine;
begin
  Result := TMessageLine(FItems[Index]);
end;

function TMessagesView.GetSelectedLineIndex: integer;
var
  I: integer;
begin
  Result := -1;
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then
    for i := 0 to MessageView.Items.Count - 1 do
      if MessageView.Selected[I] then
      begin
        Result := I;
        Break;
      end;
end;

procedure TMessagesView.SetLastLineIsProgress(const AValue: boolean);
begin
  if FLastLineIsProgress = AValue then
    exit;
  if FLastLineIsProgress then
    MessageView.Items.Delete(MessageView.Items.Count - 1);
  FLastLineIsProgress := AValue;
end;

procedure TMessagesView.SetSelectedLineIndex(const AValue: integer);
begin
  MessageView.ItemIndex := AValue;
  MessageView.TopIndex  := MessageView.ItemIndex;
end;

{ TMessageLine }

procedure TMessageLine.SetDirectory(const AValue: string);
begin
  if FDirectory = AValue then
    exit;
  FDirectory := AValue;
end;

procedure TMessageLine.SetMsg(const AValue: string);
begin
  if FMsg = AValue then
    exit;
  FMsg := AValue;
end;

constructor TMessageLine.Create;
begin
  FPosition := -1;
  FVisiblePosition := -1;
end;

destructor TMessageLine.Destroy;
begin
  FParts.Free;
  inherited Destroy;
end;

initialization
  MessagesView := nil;
  {$I msgview.lrs}

end.
