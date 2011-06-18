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

{$I ide.inc}

uses
  Classes, SysUtils, AVL_Tree,
  LCLProc, LCLType, ClipBrd, Controls, Dialogs, FileUtil, Forms,
  Menus, ExtCtrls, StdCtrls, ComCtrls, Graphics,
  CodeToolManager,
  IDEImagesIntf, IDEExternToolIntf, IDECommands, MenuIntf, IDEMsgIntf,
  SrcEditorIntf, LazIDEIntf,
  DialogProcs, EnvironmentOpts, SourceMarks,
  LazarusIDEStrConsts, IDEOptionDefs, IDEProcs, InputHistory, infobuild,
  KeyMapping;

type

  { TMessageViewMarklings }

  TMessageViewMarklings = class(TSourceMarklingProducer)
  private
  public
    function GetMarklings(aFilename: string;
                       out FreeList, FreeMarklings: boolean): TFPList; override;
  end;

  TLazMsgLineFlag = (
    lmlfHasQuickFixValid,
    lmlfHasQuickFix
    );
  TLazMsgLineFlags = set of TLazMsgLineFlag;

  { TLazMessageLine }

  TLazMessageLine = class(TIDEMessageLine)
  private
    FColumn: integer;
    FFilename: string;
    FFlags: TLazMsgLineFlags;
    FLineNumber: integer;
    FNode: TAVLTreeNode;
  public
    procedure UpdateSourcePosition;
    property Node: TAVLTreeNode read FNode write FNode;
    property Filename: string read FFilename write FFilename;
    property LineNumber: integer read FLineNumber write FLineNumber;
    property Column: integer read FColumn write FColumn;
    property Flags: TLazMsgLineFlags read FFlags write FFlags;
  end;

  { TMessagesView }
  
  TMessagesView = class(TIDEMessagesWindowInterface)
    IdleTimer1: TIdleTimer;
    MessageTreeView: TTreeView;
    MainPopupMenu: TPopupMenu;
    procedure CopyAllMenuItemClick(Sender: TObject);
    procedure CopyAllAndHiddenMenuItemClick(Sender: TObject);
    procedure CopyMenuItemClick(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure HelpMenuItemClick(Sender: TObject);
    procedure ClearMenuItemClick(Sender: TObject);
    procedure IdleTimer1Timer(Sender: TObject);
    procedure MainPopupMenuPopup(Sender: TObject);
    procedure MessageViewDblClicked(Sender: TObject);
    procedure MessageViewClicked(Sender: TObject);
    procedure MessageViewExit(Sender: TObject);
    procedure MessagesViewKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure MessageViewDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; Stage: TCustomDrawStage;
      var PaintImages, DefaultDraw: Boolean);
    procedure SaveAllToFileMenuItemClick(Sender: TObject);
    procedure OnQuickFixClick(Sender: TObject);
  private
    FItems: TFPList; // list of TLazMessageLine
    FMarklings: TMessageViewMarklings;
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
    function UpdateMsgIcons: boolean; // true if complete
  protected
    fBlockLevel: integer;
    FLastSelectedIndex: integer;
    ImgIDNone: integer;
    ImgIDHasQuickFix: integer;
    ImgIDInformation: integer;
    ImgIDHint: integer;
    ImgIDNote: integer;
    ImgIDWarning: integer;
    ImgIDError: integer;
    ImgIDFatal: integer;
    function GetSelectedLineIndex: integer;
    procedure SetSelectedLineIndex(const AValue: integer);
    function FindNextItem(const Filename: string;
                          FirstLine, LineCount: integer): TAVLTreeNode;
    procedure UpdateMsgSrcPos(Line: TLazMessageLine);
    function GetLines(Index: integer): TIDEMessageLine; override;
    procedure Changed;
    procedure SetTVNodeImage(TVNode: TTreeNode; Msg: TLazMessageLine);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdateNotification(Sender: TObject);
    procedure EndUpdateNotification(Sender: TObject);
    procedure DeleteLine(Index: integer);
    procedure Add(const Msg, CurDir: string;
                  ProgressLine, VisibleLine: boolean; OriginalIndex: integer;
                  Parts: TStrings);
    procedure AddMsg(const Msg, CurDir: string; OriginalIndex: integer;
                     Parts: TStrings = nil); override;
    procedure AddProgress(ScanLine: TIDEScanMessageLine);
    procedure AddSeparator;
    procedure CollectLineParts(Sender: TObject; SrcLines: TIDEMessageLineList);
    procedure ClearTillLastSeparator;
    procedure ShowTopMessage;
    procedure Clear; override;
    procedure GetVisibleMessageAt(Index: integer; var Msg, MsgDirectory: string);
    procedure BeginBlock(ClearOldBlocks: Boolean = true); override;
    procedure EndBlock; override;
    procedure ClearItems;
    function LinesCount: integer; override;
    function VisibleItemCount: integer;
    function MsgCount: integer;
    procedure FilterLines(Filter: TOnFilterLine);
    function GetVisibleMessagesAsText: string;
    function GetSelectedMessagesAsText: string;
    procedure SaveMessagesToFile(const Filename: string);
    procedure SrcEditLinesInsertedDeleted(const Filename: string;
                                          FirstLine, LineCount: Integer);
    procedure UpdateMsgLineInListBox(Line: TLazMessageLine);
    function ExecuteMsgLinePlugin(Step: TIMQuickFixStep): boolean;
    procedure HideLine(Line: TLazMessageLine);
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
    property Marklings: TMessageViewMarklings read FMarklings;
  end;

var
  MessagesView: TMessagesView = nil;
  MsgQuickFixIDEMenuSection: TIDEMenuSection;
  MsgClearIDEMenuCommand: TIDEMenuCommand;
  MsgCopyIDEMenuCommand: TIDEMenuCommand;
  MsgCopyAllIDEMenuCommand: TIDEMenuCommand;
  MsgCopyAllAndHiddenIDEMenuCommand: TIDEMenuCommand;
  MsgHelpIDEMenuCommand: TIDEMenuCommand;
  MsgSaveAllToFileIDEMenuCommand: TIDEMenuCommand;

const
  MessagesMenuRootName = 'Messages';

procedure RegisterStandardMessagesViewMenuItems;

function MessageLinesAsText(ListOfTLazMessageLine: TFPList): string;

implementation

{$R *.lfm}

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
  MsgQuickFixIDEMenuSection := RegisterIDEMenuSection(Path, 'Quick Fix');
  MsgClearIDEMenuCommand :=
    RegisterIDEMenuCommand(Path, 'Clear', lisUIDClear);
  MsgCopyIDEMenuCommand := RegisterIDEMenuCommand(Path, 'Copy selected',
    lisCopySelectedMessagesToClipboard);
  MsgCopyAllIDEMenuCommand := RegisterIDEMenuCommand(Path, 'Copy all',
    lisCopyAllShownMessagesToClipboard);
  MsgCopyAllAndHiddenIDEMenuCommand := RegisterIDEMenuCommand(Path,
    'Copy all, including hidden messages',
    lisCopyAllShownAndHiddenMessagesToClipboard);
  MsgHelpIDEMenuCommand := RegisterIDEMenuCommand(Path, 'Help',
    lisPckEditHelp);
  MsgSaveAllToFileIDEMenuCommand :=
    RegisterIDEMenuCommand(Path, 'Copy selected',
    lisSaveAllMessagesToFile);
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
  FQuickFixItems:=TFPList.Create;

  ImgIDNone := -1;
  ImgIDInformation := IDEImages.LoadImage(12, 'state12x12_information');
  ImgIDHint := IDEImages.LoadImage(12, 'state12x12_hint');
  ImgIDNote := IDEImages.LoadImage(12, 'state12x12_note');
  ImgIDWarning := IDEImages.LoadImage(12, 'state12x12_warning');
  ImgIDError := IDEImages.LoadImage(12, 'state12x12_error');
  ImgIDFatal := IDEImages.LoadImage(12, 'state12x12_fatal');
  ImgIDHasQuickFix := IDEImages.LoadImage(12, 'quickfix12x12');
  MessageTreeView.Images:=IDEImages.Images_12;

  Caption := lisMenuViewMessages;
  MessageTreeView.OnAdvancedCustomDrawItem := @MessageViewDrawItem;

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

  FMarklings:=TMessageViewMarklings.Create(Self);
  SourceEditorManagerIntf.RegisterMarklingProducer(FMarklings);
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

procedure TMessagesView.BeginUpdateNotification(Sender: TObject);
begin
  if MessageTreeView<>nil then MessageTreeView.BeginUpdate;
end;

procedure TMessagesView.EndUpdateNotification(Sender: TObject);
begin
  if MessageTreeView<>nil then MessageTreeView.EndUpdate;
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
    MessageTreeView.Items.TopLvlItems[VisibleIndex].Free;
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
  Changed;
end;

{------------------------------------------------------------------------------
  TMessagesView.Add
------------------------------------------------------------------------------}
procedure TMessagesView.Add(const Msg, CurDir: string;
  ProgressLine, VisibleLine: boolean; OriginalIndex: integer;
  Parts: TStrings);
var
  NewMsg: TLazMessageLine;
  i:      integer;
  LastItem: TLazMessageLine;
  ToStoreMessage : Boolean;
  TVNode: TTreeNode;
begin
  ToStoreMessage := VisibleLine;
  //ConsistencyCheck;
  //DebugLn('TMessagesView.Add START ItemCount=',dbgs(ItemCount),' VisibleCount=',dbgs(VisibleItemCount),' ListBoxCount=',dbgs(MessageTreeView.Items.Count),' ProgressLine=',dbgs(ProgressLine),' VisibleLine=',dbgs(VisibleLine),' OriginalIndex=',dbgs(OriginalIndex),' Msg="',Msg,'"');
  NewMsg:=nil;
  if LinesCount>0 then begin
    LastItem:=Items[LinesCount-1];
    if (OriginalIndex>=0) and (LastItem.OriginalIndex=OriginalIndex) then begin
      // already added
      NewMsg:=LastItem;
      ToStoreMessage := False;
    end;
  end;

  if ToStoreMessage then CompileProgress.SetStatus(Msg);
  
  if NewMsg=nil then begin
    NewMsg := TLazMessageLine.Create;
    FItems.Add(NewMsg);
  end;

  NewMsg.Msg := Msg;
  NewMsg.Directory := CurDir;
  NewMsg.Position := FItems.Count-1;
  NewMsg.OriginalIndex := OriginalIndex;
  if Parts<>nil then
  begin
    if NewMsg.Parts=nil then
      NewMsg.Parts:=TStringList.Create;
    NewMsg.Parts.Assign(Parts);
    NewMsg.UpdateSourcePosition;
  end;
  //DebugLn('TMessagesView.Add FItems.Count=',dbgs(FItems.Count),' OriginalIndex=',dbgs(OriginalIndex));

  if VisibleLine then
  begin
    if FLastLineIsProgress then
    begin
      // replace old progress line
      i := FVisibleItems.Count - 1;
      VisibleItems[i].VisiblePosition := -1;
      FVisibleItems.Delete(i);
      TVNode:=MessageTreeView.Items.TopLvlItems[i];
      TVNode.Text := Msg;
    end
    else begin
      // add new line
      TVNode:=MessageTreeView.Items.Add(nil,Msg);// add line
    end;
    NewMsg.VisiblePosition := FVisibleItems.Count;
    FVisibleItems.Add(NewMsg);
    FLastLineIsProgress  := ProgressLine;
    SetTVNodeImage(TVNode,NewMsg);
    if MessageTreeView.Items.Count>0 then
      MessageTreeView.Items.TopLvlItems[MessageTreeView.Items.Count-1].MakeVisible;
    //DebugLn(['TMessagesView.Add ',MessageTreeView.TopIndex]);
  end;
  //ConsistencyCheck;
  Changed;

  //debugln(['TMessagesView.Add ',NewMsg.Filename,' ',NewMsg.LineNumber,',',NewMsg.Column,' ',NewMsg.Msg]);
  if NewMsg.Filename<>'' then
    Marklings.InvalidateFile(NewMsg.Filename);
end;

procedure TMessagesView.AddMsg(const Msg, CurDir: string; OriginalIndex: integer;
  Parts: TStrings);
begin
  Add(Msg, CurDir, False, True, OriginalIndex, Parts);
end;

procedure TMessagesView.AddProgress(ScanLine: TIDEScanMessageLine);
begin
  Add(ScanLine.Line, ScanLine.WorkingDirectory, True, True, ScanLine.LineNumber, nil);
end;

procedure TMessagesView.AddSeparator;
begin
  Add(SeparatorLine, '', False, True, -1, nil);
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
    OldVisible: Boolean;
  begin
    for i:=StartIndex to FItems.Count-1 do begin
      ALine:=Items[i];
      for j:=0 to IDEMsgQuickFixes.Count-1 do begin
        QuickFixItem:=IDEMsgQuickFixes[j];
        if (imqfoImproveMessage in QuickFixItem.Steps)
        and QuickFixItem.IsApplicable(ALine) then begin
          OldMsg:=ALine.Msg;
          OldVisible:=ALine.VisiblePosition>=0;
          ALine.Visible:=OldVisible;
          QuickFixItem.Execute(ALine,imqfoImproveMessage);
          UpdateMsgSrcPos(ALine);
          if (OldVisible<>ALine.Visible) then begin
            if not ALine.Visible then
              HideLine(ALine);
          end;
          if (OldMsg<>ALine.Msg) then begin
            UpdateMsgLineInListBox(ALine);
          end;
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
  BeginBlock(false);
  try
    LastSeparator := VisibleItemCount - 1;
    while (LastSeparator >= 0)
    and (VisibleItems[LastSeparator].Msg <> SeparatorLine) do
      Dec(LastSeparator);
    if LastSeparator >= 0 then
    begin
      while (VisibleItemCount > LastSeparator) do
        DeleteLine(LinesCount - 1);
      FLastLineIsProgress := False;
    end;
  finally
    EndBlock;
  end;
end;

procedure TMessagesView.ShowTopMessage;
begin
  if MessageTreeView.Items.Count > 0 then begin
    MessageTreeView.Items.TopLvlItems[MessageTreeView.Items.Count-1].MakeVisible;
    //DebugLn(['TMessagesView.ShowTopMessage ']);
  end;
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
  TVNode: TTreeNode;
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
    else begin
      Line.VisiblePosition := -1;
    end;
  end;
  // rebuild MessageTreeView.Items
  MessageTreeView.BeginUpdate;
  for i := 0 to FVisibleItems.Count - 1 do
  begin
    Line := VisibleItems[i];
    if MessageTreeView.Items.Count > i then
    begin
      TVNode:=MessageTreeView.Items.TopLvlItems[i];
      TVNode.Text := Line.Msg;
    end else
    begin
      TVNode:=MessageTreeView.Items.Add(nil,Line.Msg);
    end;
    SetTVNodeImage(TVNode,Line);
  end;
  while MessageTreeView.Items.Count > FVisibleItems.Count do
    MessageTreeView.Items.TopLvlItems[MessageTreeView.Items.Count - 1].Free;
  MessageTreeView.EndUpdate;
end;

function TMessagesView.GetVisibleMessagesAsText: string;
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  for i:=0 to MessageTreeView.Items.Count-1 do
    sl.Add(MessageTreeView.Items.TopLvlItems[i].Text);
  Result:=sl.Text;
  sl.Free;
end;

function TMessagesView.GetSelectedMessagesAsText: string;
var
  sl: TStringList;
  node: TTreeNode;
begin
  sl:=TStringList.Create;
  node := MessageTreeView.GetFirstMultiSelected;
  while assigned(node) do
  begin
    sl.Add(node.Text);
    node := node.GetNextMultiSelected;
  end;
  Result:=sl.Text;
  sl.Free;
end;

procedure TMessagesView.SaveMessagesToFile(const Filename: string);
// save visible messages to file
begin
  SaveStringToFile(Filename, MessageLinesAsText(FItems), []);
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
      Line.LineNumber:= Line.LineNumber+LineCount;
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
  and (Line.VisiblePosition<MessageTreeView.Items.Count) then begin
    MessageTreeView.Items.TopLvlItems[Line.VisiblePosition].Text:=Line.Msg;
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

procedure TMessagesView.HideLine(Line: TLazMessageLine);
var
  i: Integer;
  OldIndex: LongInt;
begin
  OldIndex:=Line.VisiblePosition;
  if OldIndex<0 then exit;
  //DebugLn(['TMessagesView.HideLine ',OldIndex]);
  if (OldIndex>=0) and (OldIndex<FVisibleItems.Count)
  then begin
    // adjust all VisiblePosition
    for i:=OldIndex+1 to FVisibleItems.Count-1 do begin
      TLazMessageLine(FVisibleItems[i]).VisiblePosition:=
        TLazMessageLine(FVisibleItems[i]).VisiblePosition-1;
    end;
    FVisibleItems.Delete(OldIndex);
    Line.VisiblePosition:=-1;
  end;
  // remove from FVisibleItems and from LCL control
  if (OldIndex>=0)
  and (OldIndex<MessageTreeView.Items.TopLvlCount) then begin
    MessageTreeView.Items.TopLvlItems[OldIndex].Delete;
  end;
  //for i:=0 to MessageTreeView.Items.TopLvlCount-1 do begin
  //  DebugLn(['TMessagesView.HideLine ',i,' ',MessageTreeView.Items.TopLvlItems[i].Text]);
  //end;
end;

{------------------------------------------------------------------------------
  TMessagesView.Clear
------------------------------------------------------------------------------}
procedure TMessagesView.Clear;
begin
  if Self=nil then exit;
  if fBlockLevel>0 then begin
    // keep the old blocks
    exit;
  end;
  ClearItems;
  if not Assigned(MessageTreeView.OnClick) then
    MessageTreeView.OnClick := @MessageViewClicked;
  if not Assigned(MessageTreeView.OnDblClick) then
    MessageTreeView.OnDblClick := @MessageViewDblClicked;
end;

procedure TMessagesView.GetVisibleMessageAt(Index: integer;
  var Msg, MsgDirectory: string);
begin
  // consistency checks
  if (Index < 0) then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if MessageTreeView.Items.Count <= Index then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if (FVisibleItems = nil) then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  if (FVisibleItems.Count <= Index) then
    RaiseException('TMessagesView.GetVisibleMessageAt');
  Msg := VisibleItems[Index].Msg;
  MsgDirectory := VisibleItems[Index].Directory;
end;

procedure TMessagesView.BeginBlock(ClearOldBlocks: Boolean = true);
begin
  if ClearOldBlocks then
    Clear;
  //if fBlockLevel=0 then DumpStack;
  Inc(fBlockLevel);
end;

procedure TMessagesView.EndBlock;
begin
  if fBlockLevel <= 0 then
    RaiseException('TMessagesView.EndBlock Internal Error');
  Dec(fBlockLevel);
  //if fBlockLevel=0 then DumpStack;
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
  MessageTreeView.Items.Clear;
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
  if (MessageTreeView.Selected<>nil) then
    Result := MessageTreeView.Selected.Text;
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
  Clipboard.AsText := GetVisibleMessagesAsText;
end;

procedure TMessagesView.CopyAllAndHiddenMenuItemClick(Sender: TObject);
begin
  Clipboard.AsText := MessageLinesAsText(FItems);
end;

procedure TMessagesView.CopyMenuItemClick(Sender: TObject);
begin
  if MessageTreeView.GetFirstMultiSelected=nil then exit;
  Clipboard.AsText := GetSelectedMessagesAsText;
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

procedure TMessagesView.IdleTimer1Timer(Sender: TObject);
begin
  if UpdateMsgIcons then begin
    // done
    IdleTimer1.FireOnIdle:=false;
    IdleTimer1.AutoEnabled:=false;
    IdleTimer1.Enabled:=false;
    exit;
  end;
  // not yet complete: next on idle
  IdleTimer1.FireOnIdle:=true;
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

procedure TMessagesView.MessageViewDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; Stage: TCustomDrawStage;
  var PaintImages, DefaultDraw: Boolean);
var
  TheText: string;
  ARect: TRect;
const
  cHint         = 'Hint: User defined:';
  cNote         = 'Note: User defined:';
  cWarning      = 'Warning: User defined:';
  clMsgHint     = clBlue;
  clMsgNote     = clGreen;
  clMsgWarning  = clRed;
  cLeftSpacer   = 0;
  
  procedure ChangeFontColor(c: TColor);
  var
    OldFontColor: TColor;
  begin
    OldFontColor := MessageTreeView.Canvas.Font.Color;   // save original color
    ARect:=Node.DisplayRect(true);
    MessageTreeView.Canvas.FillRect(ARect);
    MessageTreeView.Canvas.Font.Color := c;
    MessageTreeView.Canvas.TextOut(ARect.Left + cLeftSpacer, ARect.Top + 1, TheText);
    MessageTreeView.Canvas.Font.Color := OldFontColor;   // restore original color
  end;
  
begin
  if Stage<>cdPostPaint then exit;
  // a paint means, something has changed (scrolling, expand, collapse, resize, ...)
  Changed;

  //DebugLn(['TMessagesView.MessageViewDrawItem Index=',Node.Index,' Count=',MessageTreeView.Items.Count,' TheText="',TheText,'"']);

  { Only use custom colors if not selected, otherwise it is difficult to read }
  if not (cdsSelected in State)
  then begin
    TheText := Node.Text;
    if Pos(cNote, TheText) > 0 then
      ChangeFontColor(clMsgNote)
    else if Pos(cHint, TheText) > 0 then
      ChangeFontColor(clMsgHint)
    else if Pos(cWarning, TheText) > 0 then
      ChangeFontColor(clMsgWarning);
  end;
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
begin
  Result := -1;
  if (MessageTreeView.Selected<>nil) then
    Result:=MessageTreeView.Selected.AbsoluteIndex;
end;

procedure TMessagesView.SetLastLineIsProgress(const AValue: boolean);
begin
  if FLastLineIsProgress = AValue then
    exit;
  if FLastLineIsProgress and (MessageTreeView.Items.Count>0) then
    MessageTreeView.Items.TopLvlItems[MessageTreeView.Items.Count - 1].Free;
  FLastLineIsProgress := AValue;
end;

procedure TMessagesView.DoSelectionChange;
var
  NewSelectedIndex: LongInt;
begin
  if (MessageTreeView.Selected<>nil) then
  begin
    NewSelectedIndex:=GetSelectedLineIndex;
    if NewSelectedIndex<>FLastSelectedIndex then begin
      FLastSelectedIndex:=NewSelectedIndex;
      if Assigned(OnSelectionChanged) then
        OnSelectionChanged(Self);
    end;
  end;
end;

function TMessagesView.UpdateMsgIcons: boolean;
var
  TVNode: TTreeNode;
  Index: LongInt;
  Msg: TLazMessageLine;
  i: Integer;
  QuickFixItem: TIDEMsgQuickFixItem;
begin
  //debugln(['TMessagesView.UpdateMsgIcons START']);
  Result:=true;
  TVNode:=MessageTreeView.TopItem;
  while TVNode<>nil do begin
    Index:=TVNode.AbsoluteIndex;
    if Index<VisibleItemCount then begin
      Msg:=VisibleItems[Index];
      if not (lmlfHasQuickFixValid in Msg.Flags) then begin
        //debugln(['TMessagesView.UpdateMsgIcons ',Msg.Msg]);
        Msg.Flags:=Msg.Flags+[lmlfHasQuickFixValid];
        CodeToolBoss.ActivateWriteLock;
        try
          for i:=0 to IDEMsgQuickFixes.Count-1 do begin
            QuickFixItem:=IDEMsgQuickFixes[i];
            //debugln(['TMessagesView.UpdateMsgIcons ',QuickFixItem.Name]);
            if (imqfoMenuItem in QuickFixItem.Steps)
            and QuickFixItem.IsApplicable(Msg) then begin
              Msg.Flags:=Msg.Flags+[lmlfHasQuickFix];
              TVNode.ImageIndex:=ImgIDHasQuickFix;
              TVNode.SelectedIndex:=ImgIDHasQuickFix;
            end;
          end;
        finally
          CodeToolBoss.DeactivateWriteLock;
        end;
        // next on idle
        exit(false);
      end;
    end;
    TVNode:=TVNode.GetNextVisible;
  end;
end;

procedure TMessagesView.SetSelectedLineIndex(const AValue: integer);
begin
  if AValue>=0 then begin
    MessageTreeView.Items.TopLvlItems[AValue].Selected := true;
    //MessageTreeView.TopItem  := MessageTreeView.Selected;
  end else
    MessageTreeView.Selected := nil;
end;

procedure TMessagesView.UpdateMsgSrcPos(Line: TLazMessageLine);
begin
  if Line.Node<>nil then begin
    FSrcPositions.Delete(Line.Node);
    Line.Node:=nil;
  end;
  Line.UpdateSourcePosition;
  if Line.LineNumber>0 then
    Line.Node:=FSrcPositions.Add(Line);
end;

function TMessagesView.GetLines(Index: integer): TIDEMessageLine;
begin
  Result:=Items[Index];
end;

procedure TMessagesView.Changed;
begin
  IdleTimer1.AutoEnabled:=true;
end;

procedure TMessagesView.SetTVNodeImage(TVNode: TTreeNode; Msg: TLazMessageLine
  );
var
  Typ: string;
  ImgID: LongInt;
begin
  ImgID:=ImgIDNone;
  if (lmlfHasQuickFixValid in Msg.Flags)
  and (lmlfHasQuickFix in Msg.Flags) then
    ImgID:=ImgIDHasQuickFix
  else if Msg.Parts<>nil then begin
    if Msg.Parts.Values['Stage']='FPC' then begin
      Typ:=Msg.Parts.Values['Type'];
      if Typ='Hint' then
        ImgID:=ImgIDHint
      else if Typ='Note' then
        ImgID:=ImgIDNote
      else if Typ='Warning' then
        ImgID:=ImgIDWarning
      else if Typ='Error' then
        ImgID:=ImgIDError
      else if Typ='Fatal' then
        ImgID:=ImgIDFatal
      else
        ImgID:=ImgIDInformation;
    end;
  end;
  TVNode.ImageIndex:=ImgID;
  TVNode.SelectedIndex:=TVNode.ImageIndex;
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
    if (Line.VisiblePosition>=0) and (MessageTreeView.Items[Line.VisiblePosition].Text<>Line.Msg) then
      RaiseGDBException('TMessagesView.ConsistencyCheck i='+dbgs(i)+' "'+Line.Msg+'" VisiblePosition='+dbgs(Line.VisiblePosition)+' Listbox="'+MessageTreeView.Items[Line.VisiblePosition].Text+'"');
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

{ TLazMessageLine }

procedure TLazMessageLine.UpdateSourcePosition;
begin
  GetSourcePosition(FFilename, FLineNumber, FColumn);
end;

{ TMessageViewMarklings }

function TMessageViewMarklings.GetMarklings(aFilename: string; out FreeList,
  FreeMarklings: boolean): TFPList;
var
  i: Integer;
  Msg: TLazMessageLine;
  aType: TSourceMarklingType;
  Markling: TSourceMarkling;
  TypeStr: string;
begin
  Result:=TFPList.Create;
  FreeList:=true;
  FreeMarklings:=true;
  for i:=0 to MessagesView.VisibleItemCount-1 do
  begin
    Msg:=MessagesView.VisibleItems[i];
    //debugln(['TMessageViewMarklings.GetMarklings ',CompareFilenames(Msg.Filename,aFilename),' ',Msg.Filename,' ',Msg.LineNumber,',',Msg.Column,' ',Msg.Msg]);
    if CompareFilenames(Msg.Filename,aFilename)=0 then
    begin
      aType:=smtHint;
      if Msg.Parts<>nil then
      begin
        TypeStr:=Msg.Parts.Values['Type'];
        if TypeStr='Error' then
          aType:=smtError
        else if TypeStr='Warning' then
          aType:=smtWarning
        else if TypeStr='Note' then
          aType:=smtNote;
      end;
      Markling:=TSourceMarkling.Create(Self,i,Msg.LineNumber,Msg.Column,aType);
      Result.Add(Markling);
    end;
  end;
end;

end.
