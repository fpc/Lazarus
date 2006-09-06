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
  Classes, SysUtils, AVL_Tree,
  LCLProc, LResources, ClipBrd, Controls, Dialogs, FileUtil, Forms, Menus,
  StdCtrls,
  IDEExternToolIntf, IDECommands, MenuIntf, IDEMsgIntf,
  DialogProcs, EnvironmentOpts,
  LazarusIDEStrConsts, IDEOptionDefs, IDEProcs, InputHistory, KeyMapping;

type

  { TLazMessageLine }

  TLazMessageLine = class(TIDEMessageLine)
  private
    FColumn: integer;
    FFilename: string;
    FLineNumber: integer;
    FNode: TAVLTreeNode;
  public
    property Node: TAVLTreeNode read FNode write FNode;
    property Filename: string read FFilename write FFilename;
    property LineNumber: integer read FLineNumber write FLineNumber;
    property Column: integer read FColumn write FColumn;
  end;

  { TMessagesView }
  
  TMessagesView = class(TIDEMessagesWindowInterface)
    MessageListBox:   TListBox;
    MainPopupMenu: TPopupMenu;
    procedure CopyAllMenuItemClick(Sender: TObject);
    procedure CopyAllAndHiddenMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure HelpMenuItemClick(Sender: TObject);
    procedure ClearMenuItemClick(Sender: TObject);
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
    FItems: TFPList; // list of TLazMessageLine
    FVisibleItems: TFPList; // list of TLazMessageLine (visible Items of FItems)
    FSrcPositions: TAVLTree;// tree of TLazMessageLine sorted for Filename and LineNumber
    FLastLineIsProgress: boolean;
    FOnSelectionChanged: TNotifyEvent;
    FQuickFixItems: TFPList; // list of current TIDEMsgQuickFixItem
    function GetDirectory: string;
    function GetItems(Index: integer): TLazMessageLine;
    function GetMessage: string;
    function GetMessageLine: TLazMessageLine;
    function GetVisibleItems(Index: integer): TLazMessageLine;
    procedure SetLastLineIsProgress(const AValue: boolean);
    procedure DoSelectionChange;
  protected
    fBlockCount: integer;
    FLastSelectedIndex: integer;
    function GetSelectedLineIndex: integer;
    procedure SetSelectedLineIndex(const AValue: integer);
    function FindNextItem(const Filename: string;
                          FirstLine, LineCount: integer): TAVLTreeNode;
    procedure UpdateMsgSrcPos(Line: TLazMessageLine);
    function GetLines(Index: integer): TIDEMessageLine; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteLine(Index: integer);
    procedure Add(const Msg, CurDir: string;
                  ProgressLine, VisibleLine: boolean; OriginalIndex: integer);
    procedure AddMsg(const Msg, CurDir: string; OriginalIndex: integer); override;
    procedure AddProgress(ScanLine: TIDEScanMessageLine);
    procedure AddSeparator;
    procedure CollectLineParts(Sender: TObject; SrcLines: TIDEMessageLineList);
    procedure ClearTillLastSeparator;
    procedure ShowTopMessage;
    procedure Clear; override;
    procedure GetVisibleMessageAt(Index: integer; var Msg, MsgDirectory: string);
    procedure BeginBlock; override;
    procedure EndBlock; override;
    procedure ClearItems;
    function LinesCount: integer; override;
    function VisibleItemCount: integer;
    function MsgCount: integer;
    procedure FilterLines(Filter: TOnFilterLine);
    procedure SaveMessagesToFile(const Filename: string);
    procedure SrcEditLinesInsertedDeleted(const Filename: string;
                                          FirstLine, LineCount: Integer);
    procedure UpdateMsgLineInListBox(Line: TLazMessageLine);
    function ExecuteMsgLinePlugin(Step: TIMQuickFixStep): boolean;
    procedure ConsistencyCheck;
  public
    property LastLineIsProgress: boolean read FLastLineIsProgress
                                         write SetLastLineIsProgress;
    property Message: string read GetMessage;
    property Directory: string read GetDirectory;
    property SelectedMessageIndex: integer read GetSelectedLineIndex  // visible index
                                           write SetSelectedLineIndex;
    property OnSelectionChanged: TNotifyEvent
                             read FOnSelectionChanged write FOnSelectionChanged;
    property Items[Index: integer]: TLazMessageLine read GetItems;
    property VisibleItems[Index: integer]: TLazMessageLine read GetVisibleItems;
  end;

var
  MessagesView: TMessagesView = nil;
  MsgClearIDEMenuCommand: TIDEMenuCommand;
  MsgCopyIDEMenuCommand: TIDEMenuCommand;
  MsgCopyAllIDEMenuCommand: TIDEMenuCommand;
  MsgCopyAllAndHiddenIDEMenuCommand: TIDEMenuCommand;
  MsgHelpIDEMenuCommand: TIDEMenuCommand;
  MsgSaveAllToFileIDEMenuCommand: TIDEMenuCommand;
  MsgQuickFixIDEMenuSection: TIDEMenuSection;

const
  MessagesMenuRootName = 'Messages';

procedure RegisterStandardMessagesViewMenuItems;

function MessageLinesAsText(ListOfTLazMessageLine: TFPList): string;

implementation

uses
  Graphics,     // used for TColor
  LCLType;      // used for TOwnerDrawState

const
  SeparatorLine = '---------------------------------------------';
  
type
  TMsgSrcPos = record
    Filename: string;
    LineNumber: integer;
  end;
  PMsgSrcPos = ^TMsgSrcPos;

function CompareMsgSrcPositions(Data1, Data2: Pointer): integer;
var
  Pos1: TLazMessageLine;
  Pos2: TLazMessageLine;
begin
  Pos1:=TLazMessageLine(Data1);
  Pos2:=TLazMessageLine(Data2);
  Result:=CompareFilenames(Pos1.Filename,Pos2.Filename);
  if Result<>0 then exit;
  if Pos1.LineNumber>Pos2.LineNumber then
    Result:=1
  else if Pos1.LineNumber<Pos2.LineNumber then
    Result:=-1
  else
    Result:=0;
end;

function CompareMsgSrcPosWithMsgSrcPosition(Data1, Data2: Pointer): integer;
var
  Pos1: PMsgSrcPos;
  Pos2: TLazMessageLine;
begin
  Pos1:=PMsgSrcPos(Data1);
  Pos2:=TLazMessageLine(Data2);
  Result:=CompareFilenames(Pos1^.Filename,Pos2.Filename);
  if Result<>0 then exit;
  if Pos1^.LineNumber>Pos2.LineNumber then
    Result:=1
  else if Pos1^.LineNumber<Pos2.LineNumber then
    Result:=-1
  else
    Result:=0;
end;

procedure RegisterStandardMessagesViewMenuItems;
var
  Path: string;
begin
  MessagesMenuRoot := RegisterIDEMenuRoot(MessagesMenuRootName);
  Path := MessagesMenuRoot.Name;
  MsgClearIDEMenuCommand :=
    RegisterIDEMenuCommand(Path, 'Clear', srVK_CLEAR);
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

function MessageLinesAsText(ListOfTLazMessageLine: TFPList): string;
var
  i: Integer;
  NewLength: Integer;
  Line: TLazMessageLine;
  p: Integer;
  e: string;
  LineEndingLength: Integer;
begin
  if (ListOfTLazMessageLine=nil) or (ListOfTLazMessageLine.Count=0) then exit('');
  NewLength:=0;
  e:=LineEnding;
  LineEndingLength:=length(e);
  for i:=0 to ListOfTLazMessageLine.Count-1 do begin
    Line:=TLazMessageLine(ListOfTLazMessageLine[i]);
    inc(NewLength,length(Line.Msg)+LineEndingLength);
  end;
  SetLength(Result,NewLength);
  p:=1;
  for i:=0 to ListOfTLazMessageLine.Count-1 do begin
    Line:=TLazMessageLine(ListOfTLazMessageLine[i]);
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
  IDEMessagesWindow:=Self;
  inherited Create(TheOwner);
  Name   := NonModalIDEWindowNames[nmiwMessagesViewName];
  FItems := TFPList.Create;
  FVisibleItems := TFPList.Create;
  FSrcPositions := TAVLTree.Create(@CompareMsgSrcPositions);
  FLastSelectedIndex := -1;

  Caption := lisMenuViewMessages;
  MessageListBox.Style       := lbOwnerDrawFixed;
  MessageListBox.OnDrawItem  := @MessageViewDrawItem;

  // assign the root TMenuItem to the registered menu root.
  // This will automatically create all registered items
  MessagesMenuRoot.MenuItem := MainPopupMenu.Items;
  //MainPopupMenu.Items.WriteDebugReport('TMessagesView.Create ');

  MsgHelpIDEMenuCommand.OnClick    := @HelpMenuItemClick;
  MsgClearIDEMenuCommand.OnClick   := @ClearMenuItemClick;
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
  FreeThenNil(FSrcPositions);
  FreeThenNil(FItems);
  FreeThenNil(FVisibleItems);
  FreeThenNil(FQuickFixItems);
  inherited Destroy;
  if IDEMessagesWindow=nil then
    IDEMessagesWindow:=nil;
  if MessagesView=Self then
    MessagesView:=nil;
end;

procedure TMessagesView.DeleteLine(Index: integer);
var
  Line: TLazMessageLine;
  VisibleIndex: integer;
  i: integer;
begin
  Line := Items[Index];

  // remove line from lists and tree
  if Line.Node<>nil then begin
    FSrcPositions.Delete(Line.Node);
    Line.Node:=nil;
  end;
  FItems.Delete(Line.Position);
  VisibleIndex := Line.VisiblePosition;
  if VisibleIndex >= 0 then
  begin
    MessageListBox.Items.Delete(VisibleIndex);
    FVisibleItems.Delete(VisibleIndex);
  end;
  
  // free Line
  Line.Free;
  
  // adjust Positions
  for i := Index to FItems.Count - 1 do
  begin
    Line := Items[i];
    Line.Position:=Line.Position-1;
    if Line.VisiblePosition > VisibleIndex then
      Line.VisiblePosition:=Line.VisiblePosition-1;
  end;
  //ConsistencyCheck;
end;

{------------------------------------------------------------------------------
  TMessagesView.Add
------------------------------------------------------------------------------}
procedure TMessagesView.Add(const Msg, CurDir: string;
  ProgressLine, VisibleLine: boolean; OriginalIndex: integer);
var
  NewMsg: TLazMessageLine;
  i:      integer;
  LastItem: TLazMessageLine;
begin
  //ConsistencyCheck;
  //DebugLn('TMessagesView.Add START ItemCount=',dbgs(ItemCount),' VisibleCount=',dbgs(VisibleItemCount),' ListBoxCount=',dbgs(MessageListBox.Items.Count),' ProgressLine=',dbgs(ProgressLine),' VisibleLine=',dbgs(VisibleLine),' OriginalIndex=',dbgs(OriginalIndex),' Msg="',Msg,'"');
  NewMsg:=nil;
  if LinesCount>0 then begin
    LastItem:=Items[LinesCount-1];
    if (OriginalIndex>=0) and (LastItem.OriginalIndex=OriginalIndex) then begin
      // already added
      NewMsg:=LastItem;
    end;
  end;
  if NewMsg=nil then begin
    NewMsg := TLazMessageLine.Create;
    FItems.Add(NewMsg);
  end;
  
  NewMsg.Msg := Msg;
  NewMsg.Directory := CurDir;
  NewMsg.Position := FItems.Count-1;
  NewMsg.OriginalIndex := OriginalIndex;
  //DebugLn('TMessagesView.Add FItems.Count=',dbgs(FItems.Count),' OriginalIndex=',dbgs(OriginalIndex));

  if VisibleLine then
  begin
    if FLastLineIsProgress then
    begin
      // replace old progress line
      i := FVisibleItems.Count - 1;
      VisibleItems[i].VisiblePosition := -1;
      FVisibleItems.Delete(i);
      MessageListBox.Items[i] := Msg;
    end
    else begin
      // add new line
      MessageListBox.Items.Add(Msg)// add line
    end;
    NewMsg.VisiblePosition := FVisibleItems.Count;
    FVisibleItems.Add(NewMsg);
    FLastLineIsProgress  := ProgressLine;
    MessageListBox.TopIndex := MessageListBox.Items.Count - 1;
  end;
  //ConsistencyCheck;
end;

procedure TMessagesView.AddMsg(const Msg, CurDir: string; OriginalIndex: integer);
begin
  Add(Msg, CurDir, False, True, OriginalIndex);
end;

procedure TMessagesView.AddProgress(ScanLine: TIDEScanMessageLine);
begin
  Add(ScanLine.Line, ScanLine.WorkingDirectory, True, True,ScanLine.LineNumber);
end;

procedure TMessagesView.AddSeparator;
begin
  Add(SeparatorLine, '', False, True, -1);
end;

procedure TMessagesView.CollectLineParts(Sender: TObject;
  SrcLines: TIDEMessageLineList);
  
  {function MsgAsString(Msg: TLazMessageLine): string;
  begin
    Result:=Msg.Msg;
    if Msg.Parts<>nil then
      Result:=Result+' '+Msg.Parts.Text;
  end;}
  
  procedure ImproveMessages(StartIndex: Integer);
  var
    i: LongInt;
    ALine: TLazMessageLine;
    QuickFixItem: TIDEMsgQuickFixItem;
    j: Integer;
    OldMsg: String;
  begin
    for i:=StartIndex to FItems.Count-1 do begin
      ALine:=Items[i];
      for j:=0 to IDEMsgQuickFixes.Count-1 do begin
        QuickFixItem:=IDEMsgQuickFixes[j];
        if (imqfoImproveMessage in QuickFixItem.Steps)
        and QuickFixItem.IsApplicable(ALine) then begin
          OldMsg:=ALine.Msg;
          QuickFixItem.Execute(ALine,imqfoImproveMessage);
          UpdateMsgSrcPos(ALine);
          if OldMsg<>ALine.Msg then
            UpdateMsgLineInListBox(ALine);
        end;
      end;
    end;
  end;
  
var
  i: Integer;
  SrcLine: TIDEMessageLine;
  DestLine: TLazMessageLine;
  StartOriginalIndex: LongInt;
  DestIndex: Integer;
  DestStartIndex: Integer;
begin
  //DebugLn('TMessagesView.CollectLineParts ',dbgsName(Sender),' ',dbgsName(SrcLines));
  if Sender=nil then ;
  if (SrcLines=nil) or (SrcLines.Count=0) then exit;
  
  StartOriginalIndex:=SrcLines[0].OriginalIndex;
  DestStartIndex:=LinesCount-1;
  while (DestStartIndex>=0)
  and (Items[DestStartIndex].OriginalIndex<>StartOriginalIndex) do
    dec(DestStartIndex);
  
  DestIndex:=DestStartIndex;
  for i:=0 to SrcLines.Count-1 do begin
    SrcLine:=SrcLines[i];
    if DestIndex>=FItems.Count then break;
    DestLine:=Items[DestIndex];
    
    // copy parts
    if (SrcLine.OriginalIndex=DestLine.OriginalIndex) then begin
      if SrcLine.Parts<>nil then begin
        if DestLine.Parts=nil then
          DestLine.Parts:=TStringList.Create;
        DestLine.Parts.Assign(SrcLine.Parts);
        //DebugLn('TMessagesView.CollectLineParts i=',dbgs(i),' Parts=',DestLine.Parts.Text);
      end else if DestLine.Parts<>nil then
        DestLine.Parts.Clear;
      UpdateMsgSrcPos(DestLine);
    end else begin
      DebugLn('TMessagesView.CollectLineParts WARNING: ',dbgs(SrcLine.OriginalIndex),'<>',dbgs(DestLine.OriginalIndex),' SrcLine=',SrcLine.Msg);
    end;

    inc(DestIndex);
  end;
  
  ImproveMessages(DestStartIndex);
  
  
  {for i:=0 to SrcLines.Count-1 do begin
    SrcLine:=SrcLines[i];
    DebugLn('TMessagesView.CollectLineParts i=',dbgs(i),' SrcLine=',MsgAsString(SrcLine));
  end;
  for i:=0 to LinesCount-1 do begin
    DestLine:=Items[i];
    DebugLn('TMessagesView.CollectLineParts i=',dbgs(i),' DestLine=',MsgAsString(DestLine));
  end;}
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
      DeleteLine(LinesCount - 1);
    FLastLineIsProgress := False;
  end;
  EndBlock;
end;

procedure TMessagesView.ShowTopMessage;
begin
  if MessageListBox.Items.Count > 0 then
    MessageListBox.TopIndex := 0;
end;

function TMessagesView.MsgCount: integer;
begin
  Result := VisibleItemCount;
end;

procedure TMessagesView.FilterLines(Filter: TOnFilterLine);
// recalculate visible lines
var
  i:    integer;
  Line: TLazMessageLine;
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
  // rebuild MessageListBox.Items
  MessageListBox.Items.BeginUpdate;
  for i := 0 to FVisibleItems.Count - 1 do
  begin
    Line := VisibleItems[i];
    if MessageListBox.Items.Count > i then
      MessageListBox.Items[i] := Line.Msg
    else
      MessageListBox.Items.Add(Line.Msg);
  end;
  while MessageListBox.Items.Count > FVisibleItems.Count do
    MessageListBox.Items.Delete(MessageListBox.Items.Count - 1);
  MessageListBox.Items.EndUpdate;
end;

procedure TMessagesView.SaveMessagesToFile(const Filename: string);
begin
  SaveStringToFile(Filename, MessageListBox.Items.Text, []);
end;

procedure TMessagesView.SrcEditLinesInsertedDeleted(const Filename: string;
  FirstLine, LineCount: Integer);
var
  ANode: TAVLTreeNode;
  Line: TLazMessageLine;
  OldLineNumber: LongInt;
begin
  if LineCount=0 then exit;
  //DebugLn('TMessagesView.SrcEditLinesInsertedDeleted ',Filename,' First=',dbgs(FirstLine),' Count=',dbgs(LineCount));
  
  ANode:=FindNextItem(Filename,FirstLine,LineCount);
  while ANode<>nil do begin
    Line:=TLazMessageLine(ANode.Data);
    if CompareFilenames(Line.Filename,Filename)<>0 then break;
    //DebugLn('TMessagesView.SrcEditLinesInsertedDeleted ',dbgs(Line.LineNumber),'->',dbgs(Line.LineNumber+LineCount));
    OldLineNumber:=Line.LineNumber;
    if (LineCount<0) and (OldLineNumber>=FirstLine)
    and (OldLineNumber<FirstLine-LineCount) then begin
      // line deleted
      Line.LineNumber:=FirstLine;
    end else begin
      // line moved
      inc(Line.LineNumber,LineCount);
    end;
    if OldLineNumber<>Line.LineNumber then begin
      // update line number
      if Line.Parts<>nil then
        Line.Parts.Values['Line']:=IntToStr(Line.LineNumber);
      Line.SetSourcePosition('',Line.LineNumber,0);
      //DebugLn('TMessagesView.SrcEditLinesInsertedDeleted ',Line.Msg,' ',dbgs(Line.VisiblePosition));
      UpdateMsgLineInListBox(Line);
    end;
    
    ANode:=FSrcPositions.FindSuccessor(ANode);
  end;
end;

procedure TMessagesView.UpdateMsgLineInListBox(Line: TLazMessageLine);
begin
  if (Line.VisiblePosition>=0)
  and (Line.VisiblePosition<MessageListBox.Items.Count) then begin
    MessageListBox.Items[Line.VisiblePosition]:=Line.Msg;
  end;
end;

function TMessagesView.ExecuteMsgLinePlugin(Step: TIMQuickFixStep): boolean;
var
  i: Integer;
  QuickFixItem: TIDEMsgQuickFixItem;
  Msg: TLazMessageLine;
begin
  Result:=false;
  Msg:=GetMessageLine;
  if Msg=nil then exit;
  for i:=0 to IDEMsgQuickFixes.Count-1 do begin
    QuickFixItem:=IDEMsgQuickFixes[i];
    //DebugLn(['TMessagesView.ExecuteMsgLinePlugin ',Msg.Msg,' ',QuickFixItem.Name]);
    if (Step in QuickFixItem.Steps)
    and QuickFixItem.IsApplicable(Msg) then begin
      Result:=true;
      QuickFixItem.Execute(Msg,Step);
      if Msg.Msg='' then begin
        // message fixed -> delete
        DeleteLine(Msg.Position);
      end else begin
        UpdateMsgSrcPos(Msg);
        UpdateMsgLineInListBox(Msg);
      end;
      exit;
    end;
  end;
end;

{------------------------------------------------------------------------------
  TMessagesView.Clear
------------------------------------------------------------------------------}
procedure TMessagesView.Clear;
begin
  if Self=nil then exit;
  if fBlockCount>0 then begin
    // keep the old blocks
    exit;
  end;
  ClearItems;
  if not Assigned(MessageListBox.OnClick) then
    MessageListBox.OnClick := @MessageViewClicked;
  if not Assigned(MessageListBox.OnDblClick) then
    MessageListBox.OnDblClick := @MessageViewDblClicked;
end;

procedure TMessagesView.GetVisibleMessageAt(Index: integer;
  var Msg, MsgDirectory: string);
begin
  // consistency checks
  if (Index < 0) then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if MessageListBox.Items.Count <= Index then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if (FVisibleItems = nil) then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if (FVisibleItems.Count <= Index) then
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
  FSrcPositions.Clear;
  for i := 0 to FItems.Count - 1 do
    TObject(FItems[i]).Free;
  FItems.Clear;
  FVisibleItems.Clear;
  MessageListBox.Clear;
  FLastLineIsProgress:=false;
end;

function TMessagesView.LinesCount: integer;
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
  if (MessageListBox.Items.Count > 0) and (MessageListBox.SelCount > 0) then
    Result := MessageListBox.Items.Strings[GetSelectedLineIndex];
end;

function TMessagesView.GetMessageLine: TLazMessageLine;
var
  i: LongInt;
begin
  Result:=nil;
  i:=GetSelectedLineIndex;
  if (i>=0) and (i<FVisibleItems.Count) then
    Result:=VisibleItems[i];
end;

function TMessagesView.GetVisibleItems(Index: integer): TLazMessageLine;
begin
  Result := TLazMessageLine(FVisibleItems[Index]);
end;

procedure TMessagesView.MessageViewDblClicked(Sender: TObject);
begin
  if not EnvironmentOptions.MsgViewDblClickJumps then
    exit;
  DoSelectionChange;
end;

procedure TMessagesView.CopyAllMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText := MessageListBox.Items.Text;
end;

procedure TMessagesView.CopyAllAndHiddenMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText := MessageLinesAsText(FItems);
end;

procedure TMessagesView.CopyMenuItemClick(Sender: TObject);
begin
  if MessageListBox.ItemIndex < 0 then
    exit;
  Clipboard.AsText := MessageListBox.GetSelectedText;
end;

procedure TMessagesView.FormDeactivate(Sender: TObject);
begin
  FLastSelectedIndex:=-1;
end;

procedure TMessagesView.HelpMenuItemClick(Sender: TObject);
begin
  ExecuteIDECommand(Self, ecContextHelp);
end;

procedure TMessagesView.ClearMenuItemClick(Sender: TObject);
begin
  Clear;
end;

procedure TMessagesView.MainPopupMenuPopup(Sender: TObject);
var
  i: LongInt;
  j: Integer;
  QuickFixItem: TIDEMsgQuickFixItem;
  Msg: TLazMessageLine;
begin
  MsgQuickFixIDEMenuSection.Clear;
  Msg:=GetMessageLine;
  FQuickFixItems.Clear;
  if Msg<>nil then begin
    for j:=0 to IDEMsgQuickFixes.Count-1 do begin
      QuickFixItem:=IDEMsgQuickFixes[j];
      //DebugLn('TMessagesView.MainPopupMenuPopup "',Msg.Msg,'" ',QuickFixItem.Name);
      if (imqfoMenuItem in QuickFixItem.Steps)
      and QuickFixItem.IsApplicable(Msg) then begin
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

procedure TMessagesView.MessageViewDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  TheText: string;
  cl: TColor;
const
  cHint         = 'Hint: User defined:';
  cNote         = 'Note: User defined:';
  cWarning      = 'Warning: User defined:';
  clMsgHint     = clBlue;
  clMsgNote     = clGreen;
  clMsgWarning  = clRed;
  cLeftSpacer   = 3;
begin
  MessageListBox.Canvas.FillRect(ARect);
  //DebugLn('TMessagesView.MessageViewDrawItem Index=',dbgs(Index),' Count=',dbgs(MessageListBox.Items.Count));
  TheText := MessageListBox.Items[Index];

  cl := MessageListBox.Canvas.Font.Color;   // save original color

  { Only use custom colors if not selected, otherwise it is difficult to read }
  if not (odSelected in State)
  then begin
    if Pos(cNote, TheText) > 0 then
      MessageListBox.Canvas.Font.Color := clMsgNote
    else if Pos(cHint, TheText) > 0 then
      MessageListBox.Canvas.Font.Color := clMsgHint
    else if Pos(cWarning, TheText) > 0 then
      MessageListBox.Canvas.Font.Color := clMsgWarning;
  end;

  MessageListBox.Canvas.TextOut(ARect.Left + cLeftSpacer, ARect.Top + 1, TheText);
  MessageListBox.Canvas.Font.Color := cl;   // restore original color
end;

procedure TMessagesView.SaveAllToFileMenuItemClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  AFilename:  string;
begin
  SaveDialog := TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.Title   := lisMVSaveMessagesToFileTxt;
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
  Msg: TLazMessageLine;
begin
  Msg:=GetMessageLine;
  if Msg=nil then exit;
  for i:=0 to FQuickFixItems.Count-1 do begin
    QuickFixItem:=TIDEMsgQuickFixItem(FQuickFixItems[i]);
    if (QuickFixItem.Caption=(Sender as TIDEMenuItem).Caption)
    and (imqfoMenuItem in QuickFixItem.Steps) then begin
      //ConsistencyCheck;
      //DebugLn('TMessagesView.OnQuickFixClick ',Msg.Msg,' ',dbgs(Msg.VisiblePosition),' ',dbgs(Msg.Position),' ',Items[Msg.Position].Msg);
      QuickFixItem.Execute(Msg,imqfoMenuItem);
      if Msg.Msg='' then begin
        // messages fixed -> delete
        //DebugLn('TMessagesView.OnQuickFixClick ',dbgs(Msg.VisiblePosition),' ',dbgs(Msg.Position));
        DeleteLine(Msg.Position);
      end else begin
        UpdateMsgSrcPos(Msg);
        UpdateMsgLineInListBox(Msg);
      end;
      exit;
      //ConsistencyCheck;
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

function TMessagesView.GetItems(Index: integer): TLazMessageLine;
begin
  Result := TLazMessageLine(FItems[Index]);
end;

function TMessagesView.GetSelectedLineIndex: integer;
var
  I: integer;
begin
  Result := -1;
  if (MessageListBox.Items.Count > 0) and (MessageListBox.SelCount > 0) then
    for i := 0 to MessageListBox.Items.Count - 1 do
      if MessageListBox.Selected[I] then
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
    MessageListBox.Items.Delete(MessageListBox.Items.Count - 1);
  FLastLineIsProgress := AValue;
end;

procedure TMessagesView.DoSelectionChange;
var
  NewSelectedIndex: LongInt;
begin
  if (MessageListBox.Items.Count > 0) and (MessageListBox.SelCount > 0) then begin
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
  MessageListBox.ItemIndex := AValue;
  MessageListBox.TopIndex  := MessageListBox.ItemIndex;
end;

procedure TMessagesView.UpdateMsgSrcPos(Line: TLazMessageLine);
begin
  if Line.Node<>nil then begin
    FSrcPositions.Delete(Line.Node);
    Line.Node:=nil;
  end;
  Line.GetSourcePosition(Line.Filename,Line.LineNumber,Line.Column);
  if Line.LineNumber>0 then
    Line.Node:=FSrcPositions.Add(Line);
end;

function TMessagesView.GetLines(Index: integer): TIDEMessageLine;
begin
  Result:=Items[Index];
end;

procedure TMessagesView.ConsistencyCheck;
var
  i: Integer;
  Line: TLazMessageLine;
begin
  writeln('TMessagesView.ConsistencyCheck ');
  if FSrcPositions.ConsistencyCheck<>0 then
    RaiseGDBException('TMessagesView.ConsistencyCheck FSrcPositions.ConsistencyCheck');
  for i:=0 to FItems.Count-1 do begin
    Line:=Items[i];
    if Line.Position<>i then
      RaiseGDBException('TMessagesView.ConsistencyCheck i='+dbgs(i)+' "'+Line.Msg+'" Position='+dbgs(Line.Position));
    if (Line.VisiblePosition>=0) and (VisibleItems[Line.VisiblePosition]<>Line) then
      RaiseGDBException('TMessagesView.ConsistencyCheck i='+dbgs(i)+' "'+Line.Msg+'" VisiblePosition='+dbgs(Line.VisiblePosition)+' '+VisibleItems[Line.VisiblePosition].Msg);
    if (Line.VisiblePosition>=0) and (MessageListBox.Items[Line.VisiblePosition]<>Line.Msg) then
      RaiseGDBException('TMessagesView.ConsistencyCheck i='+dbgs(i)+' "'+Line.Msg+'" VisiblePosition='+dbgs(Line.VisiblePosition)+' Listbox="'+MessageListBox.Items[Line.VisiblePosition]+'"');
  end;
  for i:=0 to FVisibleItems.Count-1 do begin
    Line:=VisibleItems[i];
    if (Line.VisiblePosition<>i) then
      RaiseGDBException('TMessagesView.ConsistencyCheck Visible i='+dbgs(i)+' "'+Line.Msg+'" VisiblePosition='+dbgs(Line.VisiblePosition));
    if (Line<>Items[Line.Position]) then
      RaiseGDBException('TMessagesView.ConsistencyCheck Visible i='+dbgs(i)+' "'+Line.Msg+'" Position='+dbgs(Line.Position));
  end;
  if FLastLineIsProgress and (FVisibleItems.Count=0) then
    RaiseGDBException('TMessagesView.ConsistencyCheck FLastLineIsProgress and FVisibleItems.Count=0');
end;

function TMessagesView.FindNextItem(const Filename: string; FirstLine,
  LineCount: integer): TAVLTreeNode;
var
  MsgSrcPos: TMsgSrcPos;
  Comp: LongInt;
begin
  Result:=FSrcPositions.Root;
  //DebugLn('TMessagesView.FindNextItem ',dbgs(Result));
  if Result=nil then exit;
  MsgSrcPos.Filename:=Filename;
  MsgSrcPos.LineNumber:=FirstLine;
  while true do begin
    Comp:=CompareMsgSrcPosWithMsgSrcPosition(@MsgSrcPos,
                                             TLazMessageLine(Result.Data));
    //DebugLn('TMessagesView.FindNextItem Comp=',dbgs(Comp),' ',TLazMessageLine(Result.Data).Filename,' ',dbgs(TLazMessageLine(Result.Data).LineNumber));
    if Comp=0 then begin
      Result:=FSrcPositions.FindLeftMostSameKey(Result);
      exit;
    end;
    if Comp<0 then begin
      if Result.Left<>nil then
        Result:=Result.Left
      else
        exit;
    end else begin
      if Result.Right<>nil then
        Result:=Result.Right
      else begin
        Result:=FSrcPositions.FindSuccessor(Result);
        exit;
      end;
    end;
  end;
end;

initialization
  MessagesView := nil;
  {$I msgview.lrs}

end.
