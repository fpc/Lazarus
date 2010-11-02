{ $Id$ }
{               ----------------------------------------------  
                 callstackdlg.pp  -  Overview of the callstack 
                ---------------------------------------------- 
 
 @created(Sun Apr 28th WET 2002)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains the Call Stack debugger dialog.
 
 
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
unit CallStackDlg;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, Debugger, DebuggerDlg, Menus, ClipBrd, ExtCtrls, StdCtrls, Spin,
  ActnList, MainBase, IDEImagesIntf, IDECommands;

type

  { TCallStackDlg }

  TCallStackDlg = class(TDebuggerDlg)
    aclActions: TActionList;
    actCopyAll: TAction;
    actToggleBreakPoint: TAction;
    actViewBottom: TAction;
    actViewTop: TAction;
    actViewLimit: TAction;
    actViewGoto: TAction;
    actViewMore: TAction;
    actSetCurrent: TAction;
    actShow: TAction;
    popToggle: TMenuItem;
    ToolButtonTop: TToolButton;
    ToolButtonBottom: TToolButton;
    ToolButtonCopyAll: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    txtGoto: TEdit;
    lvCallStack: TListView;
    Panel1: TPanel;
    popLimit50: TMenuItem;
    popLimit25: TMenuItem;
    popLimit10: TMenuItem;
    popCopyAll: TMenuItem;
    N1: TMenuItem;
    popSetAsCurrent: TMenuItem;
    popShow: TMenuItem;
    mnuPopup: TPopupMenu;
    mnuLimit: TPopupMenu;
    ToolBar1: TToolBar;
    ToolButtonShow: TToolButton;
    ToolButtonCurrent: TToolButton;
    ToolButton4: TToolButton;
    ToolButtonMore: TToolButton;
    ToolButtonMax: TToolButton;
    ToolButtonGoto: TToolButton;
    procedure actToggleBreakPointExecute(Sender: TObject);
    procedure actViewBottomExecute(Sender: TObject);
    procedure actViewGotoExecute(Sender: TObject);
    procedure actViewMoreExecute(Sender: TObject);
    procedure actViewLimitExecute(Sender: TObject);
    procedure actViewTopExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvCallStackClick(Sender: TObject);
    procedure lvCallStackKeyPress(Sender: TObject; var Key: char);
    procedure popCountClick(Sender: TObject);
    procedure txtGotoKeyPress(Sender: TObject; var Key: char);
    procedure lvCallStackDBLCLICK(Sender: TObject);
    procedure actCopyAllClick(Sender: TObject);
    procedure actSetAsCurrentClick(Sender : TObject);
    procedure actShowClick(Sender: TObject);
  private
    FBreakPoints: TIDEBreakPoints;
    FCallStack: TIDECallStack;
    FCallStackNotification: TIDECallStackNotification;
    FBreakpointsNotification: TIDEBreakPointsNotification;
    FViewCount: Integer;
    FViewLimit: Integer;
    FViewStart: Integer;
    function GetImageIndex(Entry: TCallStackEntry): Integer;
    procedure SetBreakPoints(const AValue: TIDEBreakPoints);
    procedure SetViewLimit(const AValue: Integer);
    procedure SetViewStart(AStart: Integer);
    procedure SetViewMax;
    procedure BreakPointChanged(const ASender: TIDEBreakPoints; const ABreakpoint: TIDEBreakPoint);
    procedure CallStackChanged(Sender: TObject);
    procedure CallStackCurrent(Sender: TObject);
    procedure GotoIndex(AIndex: Integer);
    function  GetCurrentEntry: TCallStackEntry;
    function  GetFunction(const Entry: TCallStackEntry): string;
    procedure SetCallStack(const AValue: TIDECallStack);
    procedure UpdateView;
    procedure JumpToSource;
    procedure CopyToClipBoard;
    procedure ToggleBreakpoint(Item: TListItem);
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property BreakPoints: TIDEBreakPoints read FBreakPoints write SetBreakPoints;
    property CallStack: TIDECallStack read FCallStack write SetCallStack;
    property ViewLimit: Integer read FViewLimit write SetViewLimit;
  end;


implementation

{$R *.lfm}

uses
  BaseDebugManager, LCLProc, LazarusIDEStrConsts;

var
  imgCurrentLine: Integer;
  imgSourceLine: Integer;
  imgNoSourceLine: Integer;
  imgBreakPoint: Integer;
  imgCurrentLineAtBreakPoint: Integer;

{ TCallStackDlg }

constructor TCallStackDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCallStackNotification := TIDECallStackNotification.Create;
  FCallStackNotification.AddReference;
  FCallStackNotification.OnChange := @CallStackChanged;
  FCallStackNotification.OnCurrent := @CallStackCurrent;

  FBreakpointsNotification := TIDEBreakPointsNotification.Create;
  FBreakpointsNotification.AddReference;
  FBreakpointsNotification.OnAdd :=@BreakPointChanged;
  FBreakpointsNotification.OnUpdate := @BreakPointChanged;
  FBreakpointsNotification.OnRemove := @BreakPointChanged;

  FViewLimit := 10;
  FViewCount := 10;
  FViewStart := 0;
  actViewLimit.Caption := popLimit10.Caption;
  actToggleBreakPoint.ShortCut:= IDECommandList.FindIDECommand(ecToggleBreakPoint).AsShortCut;
end;

procedure TCallStackDlg.CallStackChanged(Sender: TObject);
begin
  if FViewStart = 0
  then UpdateView
  else SetViewStart(0);
  SetViewMax;
end;

procedure TCallStackDlg.CallStackCurrent(Sender: TObject);
begin
  UpdateView;
end;

function TCallStackDlg.GetImageIndex(Entry: TCallStackEntry): Integer;

  function HasBreakPoint(Entry: TCallStackEntry): Boolean; inline;
  var
    FileName: String;
  begin
    if BreakPoints = nil then
      Exit(False);
    FileName := Entry.Source;
    Result := DebugBoss.GetFullFilename(FileName, False);
    if Result then
      Result := BreakPoints.Find(FileName, Entry.Line) <> nil;
  end;

begin
  if HasBreakPoint(Entry) then
  begin
    if Entry.Current then
      Result := imgCurrentLineAtBreakPoint
    else
      Result := imgBreakPoint;
  end
  else
  begin
    if Entry.Current then
      Result := imgCurrentLine
    else
    if Entry.Source = '' then
      Result := imgNoSourceLine
    else
      Result := imgSourceLine;
  end;
end;

procedure TCallStackDlg.UpdateView;
var
  n: Integer;
  Item: TListItem;
  Entry: TCallStackEntry;
  First, Count: Integer;
  Source: String;
begin
  BeginUpdate;
  try
    if (CallStack = nil) or (CallStack.Count=0)
    then begin
      txtGoto.Text:= '0';
      lvCallStack.Items.Clear;
      exit;
    end;

    First := FViewStart;
    if First + FViewLimit <= CallStack.Count
    then Count := FViewLimit
    else Count := CallStack.Count - First;

    // Reuse entries, so add and remove only
    // Remove unneded
    for n := lvCallStack.Items.Count - 1 downto Count do
      lvCallStack.Items.Delete(n);

    // Add needed
    for n := lvCallStack.Items.Count to Count - 1 do
    begin
      Item := lvCallStack.Items.Add;
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
    end;

    CallStack.PrepareRange(First, Count);
    for n := 0 to Count - 1 do
    begin
      Item := lvCallStack.Items[n];
      Entry := CallStack.Entries[First + n];
      if Entry = nil
      then begin
        Item.Caption := '';
        Item.ImageIndex := imgNoSourceLine;
        Item.SubItems[0] := '????';
        Item.SubItems[1] := '';
        Item.SubItems[2] := '';
        Item.SubItems[3] := '';
      end
      else begin
        Item.ImageIndex := GetImageIndex(Entry);
        Item.SubItems[0] := IntToStr(Entry.Index);
        Source := Entry.Source;
        if Source = '' then // we do not have a source file => just show an adress
          Source := ':' + IntToHex(Entry.Adress, 8);
        Item.SubItems[1] := Source;
        Item.SubItems[2] := IntToStr(Entry.Line);
        Item.SubItems[3] := GetFunction(Entry);
      end;
    end;
    
  finally
    EndUpdate;
  end;
end;

destructor TCallStackDlg.Destroy;
begin
  SetBreakPoints(nil);
  FBreakpointsNotification.OnAdd := nil;
  FBreakpointsNotification.OnUpdate := nil;
  FBreakpointsNotification.OnRemove := nil;
  FBreakpointsNotification.ReleaseReference;

  SetCallstack(nil);
  FCallStackNotification.OnChange := nil;
  FCallStackNotification.ReleaseReference;
  inherited Destroy;
end;

procedure TCallStackDlg.DoBeginUpdate;
begin
  lvCallStack.BeginUpdate;
end;

procedure TCallStackDlg.DoEndUpdate;
begin
  lvCallStack.EndUpdate;
end;

function TCallStackDlg.GetCurrentEntry: TCallStackEntry;
var
  CurItem: TListItem;
  idx: Integer;
begin
  Result := nil;
  if Callstack = nil then Exit;
  
  CurItem := lvCallStack.Selected;
  if CurItem = nil then Exit;

  idx := FViewStart + CurItem.Index;
  if idx >= CallStack.Count then Exit;

  Result := CallStack.Entries[idx];
end;

procedure TCallStackDlg.JumpToSource;
var
  Entry: TCallStackEntry;
  Filename: String;
begin
  Entry := GetCurrentEntry;
  if Entry = nil then Exit;

  // check the full name first
  Filename := Entry.FullFileName;
  if (Filename = '') or not DebugBoss.GetFullFilename(Filename, False) then
  begin
    // if fails the check the short file name
    Filename := Entry.Source;
    if (FileName = '') or not DebugBoss.GetFullFilename(Filename, True) then
      Exit;
  end;
  MainIDE.DoJumpToSourcePosition(Filename, 0, Entry.Line, 0, True, True);
end;

procedure TCallStackDlg.CopyToClipBoard;
var
  n: integer;
  Entry: TCallStackEntry;
  S: String;
begin
  Clipboard.Clear;
  
  if (CallStack=nil) or (CallStack.Count=0) then exit;
  
  S := '';
  for n:= 0 to CallStack.Count-1 do
  begin
    Entry:=CallStack.Entries[n];
    if Entry <> nil
    then S := S + format('#%d %s at %s:%d', [n, GetFunction(Entry), Entry.Source, Entry.Line])
    else S := S + format('#%d ????', [n]);
    S := S + LineEnding;
  end;
  ClipBoard.AsText := S;
end;

procedure TCallStackDlg.ToggleBreakpoint(Item: TListItem);
var
  idx: Integer;
  Entry: TCallStackEntry;
  BreakPoint: TIDEBreakPoint;
  FileName: String;
begin
  if (Item <> nil) and (BreakPoints <> nil) then
  begin
    idx := FViewStart + Item.Index;
    if idx >= CallStack.Count then Exit;
    Entry := CallStack.Entries[idx];
    FileName := Entry.Source;
    if (FileName = '') or not DebugBoss.GetFullFilename(FileName, False) then
      Exit;
    BreakPoint := BreakPoints.Find(FileName, Entry.Line);
    if BreakPoint <> nil then
      DebugBoss.DoDeleteBreakPoint(BreakPoint.Source, BreakPoint.Line)
    else
      DebugBoss.DoCreateBreakPoint(FileName, Entry.Line, False);
  end;
end;

procedure TCallStackDlg.lvCallStackDBLCLICK(Sender: TObject);
begin
  JumpToSource;
end;

procedure TCallStackDlg.popCountClick(Sender: TObject);
begin
  if FViewCount = TMenuItem(Sender).Tag then Exit;
  FViewCount := TMenuItem(Sender).Tag;
  ViewLimit := FViewCount;
  actViewLimit.Caption := TMenuItem(Sender).Caption;
end;

procedure TCallStackDlg.txtGotoKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    '0'..'9', #8 : ;
    #13 : SetViewStart(StrToIntDef(txtGoto.Text, 0));
  else
    Key := #0;
  end;
end;

procedure TCallStackDlg.actCopyAllClick(Sender: TObject);
begin
  CopyToClipBoard;
end;

procedure TCallStackDlg.actSetAsCurrentClick(Sender : TObject);
var
  Entry: TCallStackEntry;
begin
  Entry := GetCurrentEntry;
  if Entry = nil then Exit;

  CallStack.Current := Entry;
end;

procedure TCallStackDlg.actShowClick(Sender: TObject);
begin
  JumpToSource;
end;

procedure TCallStackDlg.actViewBottomExecute(Sender: TObject);
begin
  if CallStack <> nil
  then SetViewStart(CallStack.Count - FViewLimit)
  else SetViewStart(0);
end;

procedure TCallStackDlg.actToggleBreakPointExecute(Sender: TObject);
begin
  ToggleBreakpoint(lvCallStack.Selected);
end;

procedure TCallStackDlg.actViewGotoExecute(Sender: TObject);
begin
  SetViewStart(StrToIntDef(txtGoto.Text, 0));
end;

procedure TCallStackDlg.actViewMoreExecute(Sender: TObject);
begin
  ViewLimit := ViewLimit + FViewCount;
end;

procedure TCallStackDlg.actViewTopExecute(Sender: TObject);
begin
  SetViewStart(0);
end;

procedure TCallStackDlg.BreakPointChanged(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
var
  i, idx: Integer;
  Entry: TCallStackEntry;
begin
  if BreakPoints = nil then
    Exit;

  for i := 0 to lvCallStack.Items.Count - 1 do
  begin
    idx := FViewStart + lvCallStack.Items[i].Index;
    if idx >= CallStack.Count then
      Continue;
    Entry := CallStack.Entries[idx];
    if Entry <> nil then
      lvCallStack.Items[i].ImageIndex := GetImageIndex(Entry)
    else
      lvCallStack.Items[i].ImageIndex := imgNoSourceLine;
  end;
end;

procedure TCallStackDlg.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Caption:= lisMenuViewCallStack;
  ToolButtonShow.Caption:= lisShow;
  ToolButtonCurrent.Caption:= lisCurrent;
  for i:= 0 to mnuLimit.Items.Count-1 do
    mnuLimit.Items[i].Caption:= Format(lisMaxS, [mnuLimit.Items[i].Tag]);
  ToolButtonMore.Caption:= lisMore;
  ToolButtonTop.Caption:= lisTop;
  ToolButtonBottom.Caption:= lisBottom;
  ToolButtonGoto.Caption:=lisGotoSelectedSourceLine;
  ToolButtonCopyAll.Caption:= lisCopyAll;
  lvCallStack.Columns[1].Caption:= lisIndex;
  lvCallStack.Columns[2].Caption:= lisCEOModeSource;
  lvCallStack.Columns[3].Caption:= dlgAddHiAttrGroupLine;
  lvCallStack.Columns[4].Caption:= lisFunction;

  ToolBar1.Images := IDEImages.Images_16;
  ToolButtonShow.ImageIndex := IDEImages.LoadImage(16, 'callstack_show');
  ToolButtonMore.ImageIndex := IDEImages.LoadImage(16, 'callstack_more');
  ToolButtonTop.ImageIndex := IDEImages.LoadImage(16, 'callstack_top');
  ToolButtonBottom.ImageIndex := IDEImages.LoadImage(16, 'callstack_bottom');
  ToolButtonGoto.ImageIndex := IDEImages.LoadImage(16, 'callstack_goto');
  ToolButtonCopyAll.ImageIndex := IDEImages.LoadImage(16, 'laz_copy');

  lvCallStack.SmallImages := IDEImages.Images_16;
  imgCurrentLine := IDEImages.LoadImage(16, 'debugger_current_line');
  imgSourceLine := IDEImages.LoadImage(16, 'debugger_source_line');
  imgNoSourceLine := IDEImages.LoadImage(16, 'debugger_nosource_line');
  imgBreakPoint := IDEImages.LoadImage(16, 'ActiveBreakPoint');
  imgCurrentLineAtBreakPoint := IDEImages.LoadImage(16, 'debugger_current_line_breakpoint');

  popShow.Caption:= lisShow;
  popToggle.Caption:= uemToggleBreakpoint;
  popSetAsCurrent.Caption:= lisCurrent;
  popCopyAll.Caption:= lisCopyAll;

end;

procedure TCallStackDlg.lvCallStackClick(Sender: TObject);
var
  P: TPoint;
  Item: TListItem;
begin
  // toggle breakpoint
  P := lvCallStack.ScreenToClient(Mouse.CursorPos);
  Item := lvCallStack.GetItemAt(P.X, P.Y);
  // if clicked on the first column of a valid item
  if (Item <> nil) and (P.X <= lvCallStack.Column[0].Width) then
    ToggleBreakPoint(Item);
end;

procedure TCallStackDlg.lvCallStackKeyPress(Sender: TObject; var Key: char);
begin
  if key = #13
  then lvCallStackDBLCLICK(Sender);
end;

procedure TCallStackDlg.actViewLimitExecute(Sender: TObject);
begin
  ViewLimit := FViewCount;
end;

procedure TCallStackDlg.SetViewStart(AStart: Integer);
begin
  if CallStack = nil then Exit;
  
  if (AStart > CallStack.Count - FViewLimit)
  then AStart := CallStack.Count - FViewLimit;
  if AStart < 0 then AStart := 0;
  if FViewStart = AStart then Exit;
  
  FViewStart:= AStart;
  txtGoto.Text:= IntToStr(AStart);
  UpdateView;
end;

procedure TCallStackDlg.SetViewMax;
begin
//  If CallStack = nil
//  then lblViewCnt.Caption:= '0'
//  else lblViewCnt.Caption:= IntToStr(CallStack.Count);
end;

procedure TCallStackDlg.SetCallStack(const AValue: TIDECallStack);
begin
  if FCallStack = AValue then Exit;

  BeginUpdate;
  try
    if FCallStack <> nil
    then begin
      FCallStack.RemoveNotification(FCallStackNotification);
    end;

    FCallStack := AValue;

    if FCallStack <> nil
    then begin
      FCallStack.AddNotification(FCallStackNotification);
    end;

    CallStackChanged(FCallStack);
  finally
    EndUpdate;
  end;
end;

procedure TCallStackDlg.SetViewLimit(const AValue: Integer);
begin
  if FViewLimit = AValue then Exit;
  if (CallStack <> nil)
  and (FViewStart + FViewLimit >= CallStack.Count)
  and (AValue > FViewLimit)
  then begin
    FViewStart := CallStack.Count - AValue;
    if FViewStart < 0 then FViewStart := 0;
  end;
  FViewLimit := AValue;
  UpdateView;
end;

procedure TCallStackDlg.SetBreakPoints(const AValue: TIDEBreakPoints);
begin
  if FBreakPoints = AValue then Exit;

  if FBreakPoints <> nil
  then begin
    FBreakPoints.RemoveNotification(FBreakpointsNotification);
  end;

  FBreakPoints := AValue;

  if FBreakPoints <> nil
  then begin
    FBreakPoints.AddNotification(FBreakpointsNotification);
  end;
  UpdateView;
end;

function TCallStackDlg.GetFunction(const Entry: TCallStackEntry): string;
var
  S: String;
  m: Integer;
begin
  S := '';
  for m := 0 to Entry.ArgumentCount - 1 do
  begin
    if S <> '' then
      S := S + ', ';
    S := S + Entry.ArgumentValues[m];
  end;
  if S <> '' then
    S := '(' + S + ')';
  Result := Entry.FunctionName + S;
end;

procedure TCallStackDlg.GotoIndex(AIndex: Integer);
begin
  if AIndex < 0 then Exit;
  if AIndex >= FCallstack.Count then Exit;
  

end;

end.

