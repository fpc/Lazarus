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
  MsgIntf,
  Menus,
  StdCtrls,
  SysUtils;

type
  { TMessagesView }
  
  TMessagesView = class(TForm)
    MessageView:   TListBox;
    MainPopupMenu: TPopupMenu;
    procedure CopyAllMenuItemClick(Sender: TObject);
    procedure CopyAllAndHiddenMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure HelpMenuItemClick(Sender: TObject);
    procedure MainPopupMenuPopup(Sender: TObject);
    procedure MessageViewDblClicked(Sender: TObject);
    procedure MessageViewClicked(Sender: TObject);
    procedure MessageViewExit(Sender: TObject);
    procedure MessagesViewKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure MessageViewDrawItem(Control: TWinControl; Index: Integer;
       ARect: TRect; State: TOwnerDrawState);
    procedure SaveAllToFileMenuItemClick(Sender: TObject);
    procedure OnQuickFixClick(Sender: TObject);
  private
    FItems: TFPList; // list of TIDEMessageLine
    FVisibleItems: TFPList; // list of TIDEMessageLine (visible Items of FItems)
    FLastLineIsProgress: boolean;
    FOnSelectionChanged: TNotifyEvent;
    FQuickFixItems: TFPList; // list of current TIDEMsgQuickFixItem
    function GetDirectory: string;
    function GetItems(Index: integer): TIDEMessageLine;
    function GetMessage: string;
    function GetMessageLine: TIDEMessageLine;
    function GetVisibleItems(Index: integer): TIDEMessageLine;
    procedure SetLastLineIsProgress(const AValue: boolean);
    procedure DoSelectionChange;
  protected
    fBlockCount: integer;
    FLastSelectedIndex: integer;
    function GetSelectedLineIndex: integer;
    procedure SetSelectedLineIndex(const AValue: integer);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteLine(Index: integer);
    procedure Add(const Msg, CurDir: string;
                  ProgressLine, VisibleLine: boolean; OriginalIndex: integer);
    procedure AddMsg(const Msg, CurDir: string; OriginalIndex: integer);
    procedure AddProgress(const Msg, CurDir: string; OriginalIndex: integer);
    procedure AddSeparator;
    procedure CollectLineParts(Sender: TObject; SrcLines: TIDEMessageLineList);
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
    property Items[Index: integer]: TIDEMessageLine Read GetItems;
    property VisibleItems[Index: integer]: TIDEMessageLine Read GetVisibleItems;
  end;

var
  MessagesView: TMessagesView;
  MsgCopyIDEMenuCommand: TIDEMenuCommand;
  MsgCopyAllIDEMenuCommand: TIDEMenuCommand;
  MsgCopyAllAndHiddenIDEMenuCommand: TIDEMenuCommand;
  MsgHelpIDEMenuCommand: TIDEMenuCommand;
  MsgSaveAllToFileIDEMenuCommand: TIDEMenuCommand;
  MsgQuickFixIDEMenuSection: TIDEMenuSection;

const
  MessagesMenuRootName = 'Messages';

procedure RegisterStandardMessagesViewMenuItems;

function MessageLinesAsText(ListOfTIDEMessageLine: TFPList): string;

implementation

uses
  Graphics,     // used for TColor
  LCLType;      // used for TOwnerDrawState

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
  MsgCopyAllAndHiddenIDEMenuCommand := RegisterIDEMenuCommand(Path,
    'Copy all, including hidden messages',
    lisCopyAllAndHiddenMessagesToClipboard);
  MsgHelpIDEMenuCommand := RegisterIDEMenuCommand(Path, 'Help',
    srVK_HELP);
  MsgSaveAllToFileIDEMenuCommand :=
    RegisterIDEMenuCommand(Path, 'Copy selected',
    lisSaveAllMessagesToFile);
  MsgQuickFixIDEMenuSection := RegisterIDEMenuSection(Path, 'Quick Fix');
end;

function MessageLinesAsText(ListOfTIDEMessageLine: TFPList): string;
var
  i: Integer;
  NewLength: Integer;
  Line: TIDEMessageLine;
  p: Integer;
  e: string;
  LineEndingLength: Integer;
begin
  if (ListOfTIDEMessageLine=nil) or (ListOfTIDEMessageLine.Count=0) then exit('');
  NewLength:=0;
  e:=LineEnding;
  LineEndingLength:=length(e);
  for i:=0 to ListOfTIDEMessageLine.Count-1 do begin
    Line:=TIDEMessageLine(ListOfTIDEMessageLine[i]);
    inc(NewLength,length(Line.Msg)+LineEndingLength);
  end;
  SetLength(Result,NewLength);
  p:=1;
  for i:=0 to ListOfTIDEMessageLine.Count-1 do begin
    Line:=TIDEMessageLine(ListOfTIDEMessageLine[i]);
    if Line.Msg<>'' then begin
      System.Move(Line.Msg[1],Result[p],length(Line.Msg));
      inc(p,length(Line.Msg));
    end;
    System.Move(e[1],Result[p],LineEndingLength);
    inc(p,LineEndingLength);
  end;
end;

{------------------------------------------------------------------------------
  TMessagesView.Create
------------------------------------------------------------------------------}
constructor TMessagesView.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name   := NonModalIDEWindowNames[nmiwMessagesViewName];
  FItems := TFPList.Create;
  FVisibleItems := TFPList.Create;
  FLastSelectedIndex := -1;

  Caption := lisMenuViewMessages;
  MessageView.Style       := lbOwnerDrawFixed;
  MessageView.OnDrawItem  := @MessageViewDrawItem;

  // assign the root TMenuItem to the registered menu root.
  // This will automatically create all registered items
  MessagesMenuRoot.MenuItem := MainPopupMenu.Items;
  //MainPopupMenu.Items.WriteDebugReport('TMessagesView.Create ');

  MsgHelpIDEMenuCommand.OnClick    := @HelpMenuItemClick;
  MsgCopyIDEMenuCommand.OnClick    := @CopyMenuItemClick;
  MsgCopyAllIDEMenuCommand.OnClick := @CopyAllMenuItemClick;
  MsgCopyAllAndHiddenIDEMenuCommand.OnClick := @CopyAllAndHiddenMenuItemClick;
  MsgSaveAllToFileIDEMenuCommand.OnClick := @SaveAllToFileMenuItemClick;

  EnvironmentOptions.IDEWindowLayoutList.Apply(Self, Name);
  
  FQuickFixItems:=TFPList.Create;
end;

destructor TMessagesView.Destroy;
begin
  ClearItems;
  FreeThenNil(FItems);
  FreeThenNil(FVisibleItems);
  FreeThenNil(FQuickFixItems);
  inherited Destroy;
end;

procedure TMessagesView.DeleteLine(Index: integer);
var
  Line: TIDEMessageLine;
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
    Line.Position:=Line.Position-1;
    if Line.VisiblePosition > VisibleIndex then
      Line.VisiblePosition:=Line.VisiblePosition-1;
  end;
end;

{------------------------------------------------------------------------------
  TMessagesView.Add
------------------------------------------------------------------------------}
procedure TMessagesView.Add(const Msg, CurDir: string;
  ProgressLine, VisibleLine: boolean; OriginalIndex: integer);
var
  NewMsg: TIDEMessageLine;
  i:      integer;
begin
  NewMsg     := TIDEMessageLine.Create;
  NewMsg.Msg := Msg;
  NewMsg.Directory := CurDir;
  NewMsg.Position := FItems.Count;
  NewMsg.OriginalIndex := OriginalIndex;
  //DebugLn('TMessagesView.Add FItems.Count=',dbgs(FItems.Count),' OriginalIndex=',dbgs(OriginalIndex));
  FItems.Add(NewMsg);

  if VisibleLine then
  begin
    if FLastLineIsProgress then
    begin
      // replace old progress line
      i := FVisibleItems.Count - 1;
      VisibleItems[i].VisiblePosition := -1;
      FVisibleItems.Delete(i);
      MessageView.Items[i] := Msg;
    end
    else begin
      MessageView.Items.Add(Msg)// add line
    end;
    NewMsg.VisiblePosition := FVisibleItems.Count;
    FVisibleItems.Add(NewMsg);
    FLastLineIsProgress  := ProgressLine;
    MessageView.TopIndex := MessageView.Items.Count - 1;
  end;
end;

procedure TMessagesView.AddMsg(const Msg, CurDir: string; OriginalIndex: integer);
begin
  Add(Msg, CurDir, False, True, OriginalIndex);
end;

procedure TMessagesView.AddProgress(const Msg, CurDir: string;
  OriginalIndex: integer);
begin
  Add(Msg, CurDir, True, True, OriginalIndex);
end;

procedure TMessagesView.AddSeparator;
begin
  Add(SeparatorLine, '', False, True, -1);
end;

procedure TMessagesView.CollectLineParts(Sender: TObject;
  SrcLines: TIDEMessageLineList);
var
  i: Integer;
  SrcLine: TIDEMessageLine;
  DestLine: TIDEMessageLine;
begin
  //DebugLn('TMessagesView.CollectLineParts ',dbgsName(Sender),' ',dbgsName(SrcLines));
  if Sender=nil then ;
  if SrcLines=nil then exit;
  for i:=0 to SrcLines.Count-1 do begin
    SrcLine:=SrcLines[i];
    DestLine:=Items[i];
    if (SrcLine.OriginalIndex=DestLine.OriginalIndex) then begin
      if SrcLine.Parts<>nil then begin
        if DestLine.Parts=nil then
          DestLine.Parts:=TStringList.Create;
        DestLine.Parts.Assign(SrcLine.Parts);
      end else if DestLine.Parts<>nil then
        DestLine.Parts.Clear;
    end else begin
      //DebugLn('TMessagesView.CollectLineParts WARNING: ',dbgs(SrcLine.OriginalIndex),'<>',dbgs(DestLine.OriginalIndex));
    end;
  end;
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
  Line: TIDEMessageLine;
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
      Line.VisiblePosition := FVisibleItems.Count;
      FVisibleItems.Add(Line);
    end
    else
      Line.VisiblePosition := -1;
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

function TMessagesView.GetMessageLine: TIDEMessageLine;
var
  i: LongInt;
begin
  Result:=nil;
  i:=GetSelectedLineIndex;
  if (i>=0) and (i<FVisibleItems.Count) then
    Result:=VisibleItems[i];
end;

function TMessagesView.GetVisibleItems(Index: integer): TIDEMessageLine;
begin
  Result := TIDEMessageLine(FVisibleItems[Index]);
end;

procedure TMessagesView.MessageViewDblClicked(Sender: TObject);
begin
  if not EnvironmentOptions.MsgViewDblClickJumps then
    exit;
  DoSelectionChange;
end;

procedure TMessagesView.CopyAllMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText := MessageView.Items.Text;
end;

procedure TMessagesView.CopyAllAndHiddenMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText := MessageLinesAsText(FItems);
end;

procedure TMessagesView.CopyMenuItemClick(Sender: TObject);
begin
  if MessageView.ItemIndex < 0 then
    exit;
  Clipboard.AsText := MessageView.GetSelectedText;
end;

procedure TMessagesView.FormDeactivate(Sender: TObject);
begin
  FLastSelectedIndex:=-1;
end;

procedure TMessagesView.HelpMenuItemClick(Sender: TObject);
begin
  ExecuteIDECommand(Self, ecContextHelp);
end;

procedure TMessagesView.MainPopupMenuPopup(Sender: TObject);
var
  i: LongInt;
  j: Integer;
  QuickFixItem: TIDEMsgQuickFixItem;
  Msg: TIDEMessageLine;
begin
  MsgQuickFixIDEMenuSection.Clear;
  Msg:=GetMessageLine;
  FQuickFixItems.Clear;
  if Msg<>nil then begin
    for j:=0 to IDEMsgQuickFixes.Count-1 do begin
      QuickFixItem:=IDEMsgQuickFixes[j];
      DebugLn('TMessagesView.MainPopupMenuPopup "',Msg.Msg,'" ',QuickFixItem.Name);
      if QuickFixItem.IsApplicable(Msg) then begin
        FQuickFixItems.Add(QuickFixItem);
      end;
    end;
    for i:=0 to FQuickFixItems.Count-1 do begin
      QuickFixItem:=TIDEMsgQuickFixItem(FQuickFixItems[i]);
      RegisterIDEMenuCommand(MsgQuickFixIDEMenuSection,
                             QuickFixItem.Name,
                             QuickFixItem.Caption,
                             @OnQuickFixClick);
    end;
  end;
end;

procedure TMessagesView.MessageViewClicked(Sender: TObject);
begin
  if EnvironmentOptions.MsgViewDblClickJumps then
    exit;
  DoSelectionChange;
end;

procedure TMessagesView.MessageViewExit(Sender: TObject);
begin
  FLastSelectedIndex:=-1;
end;

procedure TMessagesView.MessagesViewKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  //debugln('TMessagesView.MessagesViewKeyDown ',dbgs(Key));
  ExecuteIDEShortCut(Self, Key, Shift);
end;

//------------------------------------------------------------------------------
procedure TMessagesView.MessageViewDrawItem(Control: TWinControl;
   Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  TheText: string;
  cl: TColor;
const
  cHint         = 'Hint: User defined:';
  cNote         = 'Note: User defined:';
  clMsgHint     = clBlue;
  clMsgNote     = clGreen;
  cLeftSpacer   = 3;
begin
  MessageView.Canvas.FillRect(ARect);
  TheText := MessageView.Items[Index];

  cl := MessageView.Canvas.Font.Color;   // save original color

  { Only use custom colors if not selected, otherwise it is difficult to read }
  if not (odSelected in State)
  then begin
    if Pos(cNote, TheText) > 0
    then MessageView.Canvas.Font.Color := clMsgNote
    else if Pos(cHint, TheText) > 0
    then  MessageView.Canvas.Font.Color := clMsgHint
  end;

  MessageView.Canvas.TextOut(ARect.Left + cLeftSpacer, ARect.Top + 1, TheText);
  MessageView.Canvas.Font.Color := cl;   // restore original color
end;

//------------------------------------------------------------------------------
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

procedure TMessagesView.OnQuickFixClick(Sender: TObject);
var
  i: Integer;
  QuickFixItem: TIDEMsgQuickFixItem;
  Msg: TIDEMessageLine;
begin
  Msg:=GetMessageLine;
  for i:=0 to FQuickFixItems.Count-1 do begin
    QuickFixItem:=TIDEMsgQuickFixItem(FQuickFixItems[i]);
    if QuickFixItem.Caption=(Sender as TIDEMenuItem).Caption then begin
      QuickFixItem.Execute(Msg);
    end;
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

function TMessagesView.GetItems(Index: integer): TIDEMessageLine;
begin
  Result := TIDEMessageLine(FItems[Index]);
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

procedure TMessagesView.DoSelectionChange;
var
  NewSelectedIndex: LongInt;
begin
  if (MessageView.Items.Count > 0) and (MessageView.SelCount > 0) then begin
    NewSelectedIndex:=GetSelectedLineIndex;
    if NewSelectedIndex<>FLastSelectedIndex then begin
      FLastSelectedIndex:=NewSelectedIndex;
      if Assigned(OnSelectionChanged) then
        OnSelectionChanged(Self);
    end;
  end;
end;

procedure TMessagesView.SetSelectedLineIndex(const AValue: integer);
begin
  MessageView.ItemIndex := AValue;
  MessageView.TopIndex  := MessageView.ItemIndex;
end;

initialization
  MessagesView := nil;
  {$I msgview.lrs}

end.
