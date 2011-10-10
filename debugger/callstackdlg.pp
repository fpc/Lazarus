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
  ActnList, MainIntf, MainBase, IDEImagesIntf, IDECommands;

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
    ToolButtonPower: TToolButton;
    ToolButton2: TToolButton;
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
    procedure popCountClick(Sender: TObject);
    procedure ToolButtonPowerClick(Sender: TObject);
    procedure txtGotoKeyPress(Sender: TObject; var Key: char);
    procedure lvCallStackDBLCLICK(Sender: TObject);
    procedure actCopyAllClick(Sender: TObject);
    procedure actSetAsCurrentClick(Sender : TObject);
    procedure actShowClick(Sender: TObject);
  private
    FImgBreakPointDisabled: Integer;
    FImgCurrentLineAtBreakPointDisabled: Integer;
    FViewCount: Integer;
    FViewLimit: Integer;
    FViewStart: Integer;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    FInUpdateView: Boolean;
    FUpdateFlags: set of (ufNeedUpdating);
    function GetImageIndex(Entry: TCallStackEntry): Integer;
    procedure SetViewLimit(const AValue: Integer);
    procedure SetViewStart(AStart: Integer);
    procedure SetViewMax;
    procedure GotoIndex(AIndex: Integer);
    function  GetCurrentEntry: TCallStackEntry;
    function  GetFunction(const Entry: TCallStackEntry): string;
    procedure UpdateView;
    procedure JumpToSource;
    procedure CopyToClipBoard;
    procedure ToggleBreakpoint(Item: TListItem);
  protected
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    procedure DisableAllActions;
    procedure EnableAllActions;
    function  GetSelectedSnapshot: TSnapshot;
    function  GetSelectedThreads(Snap: TSnapshot): TThreads;
    function  GetSelectedCallstack: TCallStack;
    procedure DoBreakPointsChanged; override;
    procedure BreakPointChanged(const ASender: TIDEBreakPoints; const ABreakpoint: TIDEBreakPoint);
    procedure CallStackChanged(Sender: TObject);
    procedure CallStackCurrent(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property BreakPoints;
    property CallStackMonitor;
    property ThreadsMonitor;
    property SnapshotManager;
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
  CallStackNotification.OnChange   := @CallStackChanged;
  CallStackNotification.OnCurrent  := @CallStackCurrent;
  BreakpointsNotification.OnAdd    := @BreakPointChanged;
  BreakpointsNotification.OnUpdate := @BreakPointChanged;
  BreakpointsNotification.OnRemove := @BreakPointChanged;
  ThreadsNotification.OnCurrent    := @CallStackChanged;
  SnapshotNotification.OnCurrent   := @CallStackChanged;

  FViewLimit := 10;
  FViewCount := 10;
  FViewStart := 0;
  FInUpdateView := False;
  actViewLimit.Caption := popLimit10.Caption;
  actToggleBreakPoint.ShortCut := IDECommandList.FindIDECommand(ecToggleBreakPoint).AsShortCut;
end;

procedure TCallStackDlg.CallStackChanged(Sender: TObject);
begin
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataWindow: TCallStackDlg.CallStackChanged from ',  DbgSName(Sender), ' Upd:', IsUpdating]); {$ENDIF}
  if not ToolButtonPower.Down then exit;
  if FViewStart = 0
  then UpdateView
  else SetViewStart(0);
  SetViewMax;
end;

procedure TCallStackDlg.CallStackCurrent(Sender: TObject);
begin
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataWindow: TCallStackDlg.CallStackCurrent from ',  DbgSName(Sender), '  Upd:', IsUpdating]); {$ENDIF}
  if not ToolButtonPower.Down then exit;
  UpdateView;
end;

function TCallStackDlg.GetImageIndex(Entry: TCallStackEntry): Integer;

  function GetBreakPoint(Entry: TCallStackEntry): TIDEBreakPoint; inline;
  var
    FileName: String;
  begin
    Result := nil;
    if BreakPoints = nil then Exit;
    if DebugBoss.GetFullFilename(Entry.UnitInfo, FileName, False)
    then Result := BreakPoints.Find(FileName, Entry.Line);
  end;

var
  b: TIDEBreakPoint;
begin
  b := GetBreakPoint(Entry);
  if b <> nil then
  begin
    if b.Enabled then begin;
      if Entry.IsCurrent
      then Result := imgCurrentLineAtBreakPoint
      else Result := imgBreakPoint;
    end else begin
      if Entry.IsCurrent
      then Result := FImgCurrentLineAtBreakPointDisabled
      else Result := FImgBreakPointDisabled;
    end
  end
  else
  begin
    if Entry.IsCurrent then
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
  Snap: TSnapshot;
  CStack: TCallStack;
begin
  if (not ToolButtonPower.Down) or FInUpdateView then exit;
  if IsUpdating then begin
    {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataWindow: TCallStackDlg.UpdateView in IsUpdating']); {$ENDIF}
    Include(FUpdateFlags, ufNeedUpdating);
    exit;
  end;
  {$IFDEF DBG_DATA_MONITORS} try DebugLnEnter(['DebugDataWindow: >>ENTER: TCallStackDlg.UpdateView']); {$ENDIF}
  Exclude(FUpdateFlags, ufNeedUpdating);


  BeginUpdate;
  lvCallStack.BeginUpdate;
  try
    Snap := GetSelectedSnapshot;
    if Snap <> nil
    then Caption:= lisMenuViewCallStack + ' (' + Snap.LocationAsText + ')'
    else Caption:= lisMenuViewCallStack;

    FInUpdateView := True; // ignore change triggered by count, if there is a change event, then Count will be updated already
    CStack := GetSelectedCallstack;
    if CStack <> nil then CStack.Count; // trigger the update-notification, if executed immediately
    FInUpdateView := False;

    if (CStack = nil) or ((Snap <> nil) and (CStack.Count = 0)) then begin
      lvCallStack.Items.Clear;
      Item := lvCallStack.Items.Add;
      Item.SubItems.Add('');
      Item.SubItems.Add(lisCallStackNotEvaluated);
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      exit;
    end;

    if (CStack.Count=0)
    then begin
      txtGoto.Text:= '0';
      lvCallStack.Items.Clear;
      exit;
    end;


    if Snap <> nil then begin
      First := 0;
      Count := CStack.Count;
    end else begin
      First := FViewStart;
      if First + FViewLimit <= CStack.Count
      then Count := FViewLimit
      else Count := CStack.Count - First;
    end;

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

    FInUpdateView := True;
    CStack.PrepareRange(First, Count);
    FInUpdateView := False;
    for n := 0 to Count - 1 do
    begin
      Item := lvCallStack.Items[n];
      Entry := CStack.Entries[First + n];
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
          Source := ':' + IntToHex(Entry.Address, 8);
        Item.SubItems[1] := Source;
        Item.SubItems[2] := IntToStr(Entry.Line); // TODO: if editor is open, map line SrcEdit.DebugToSourceLine
        Item.SubItems[3] := GetFunction(Entry);
      end;
    end;
    
  finally
    FInUpdateView := False;
    lvCallStack.EndUpdate;
    EndUpdate;
  end;
  {$IFDEF DBG_DATA_MONITORS} finally DebugLnExit(['DebugDataWindow: <<EXIT: TCallStackDlg.UpdateView']); end; {$ENDIF}
end;

procedure TCallStackDlg.DoBeginUpdate;
begin
  DisableAllActions;
  lvCallStack.BeginUpdate;
end;

procedure TCallStackDlg.DoEndUpdate;
begin
  if ufNeedUpdating in FUpdateFlags then UpdateView;
  lvCallStack.EndUpdate;
  EnableAllActions;
end;

procedure TCallStackDlg.DisableAllActions;
var
  i: Integer;
begin
  for i := 0 to aclActions.ActionCount - 1 do
    (aclActions.Actions[i] as TAction).Enabled := False;
end;

procedure TCallStackDlg.EnableAllActions;
var
  i: Integer;
  Snap: TSnapshot;
begin
  for i := 0 to aclActions.ActionCount - 1 do
    (aclActions.Actions[i] as TAction).Enabled := True;
  Snap := GetSelectedSnapshot;
  if snap <> nil then begin
    actViewLimit.Enabled := False;
    actViewMore.Enabled := False;
  end;
  ToolButtonPower.Enabled := Snap = nil;
end;

function TCallStackDlg.GetSelectedSnapshot: TSnapshot;
begin
  Result := nil;
  if (SnapshotManager <> nil) and (SnapshotManager.SelectedEntry <> nil)
  then Result := SnapshotManager.SelectedEntry;
end;

function TCallStackDlg.GetSelectedThreads(Snap: TSnapshot): TThreads;
begin
  if ThreadsMonitor = nil then exit(nil);
  if Snap = nil
  then Result := ThreadsMonitor.CurrentThreads
  else Result := ThreadsMonitor.Snapshots[Snap];
end;

function TCallStackDlg.GetSelectedCallstack: TCallStack;
var
  Snap: TSnapshot;
  Threads: TThreads;
  tid: LongInt;
begin
  if (CallStackMonitor = nil) or (ThreadsMonitor = nil)
  then begin
    Result := nil;
    exit;
  end;

  Snap := GetSelectedSnapshot;
  Threads := GetSelectedThreads(Snap);
  // There should always be a thread object
  Assert(Threads<>nil, 'TCallStackDlg.GetSelectedCallstack missing thread object');
  if Threads <> nil
  then tid := Threads.CurrentThreadId
  else tid := 1;

  if (Snap <> nil)
  then Result := CallStackMonitor.Snapshots[Snap].EntriesForThreads[tid]
  else Result := CallStackMonitor.CurrentCallStackList.EntriesForThreads[tid];
end;

function TCallStackDlg.GetCurrentEntry: TCallStackEntry;
var
  CurItem: TListItem;
  idx: Integer;
begin
  Result := nil;
  if GetSelectedCallstack = nil then Exit;
  
  CurItem := lvCallStack.Selected;
  if CurItem = nil then Exit;

  idx := FViewStart + CurItem.Index;
  if idx >= GetSelectedCallstack.Count then Exit;

  Result := GetSelectedCallstack.Entries[idx];
end;

procedure TCallStackDlg.JumpToSource;
var
  Entry: TCallStackEntry;
begin
  Entry := GetCurrentEntry;
  if Entry = nil then Exit;

  JumpToUnitSource(Entry.UnitInfo, Entry.Line);
end;

procedure TCallStackDlg.CopyToClipBoard;
var
  n: integer;
  Entry: TCallStackEntry;
  S: String;
begin
  Clipboard.Clear;
  
  if (GetSelectedCallstack=nil) or (GetSelectedCallstack.Count=0) then exit;
  
  S := '';
  // GetSelectedCallstack.PrepareRange();
  for n:= 0 to GetSelectedCallstack.Count-1 do
  begin
    Entry:=GetSelectedCallstack.Entries[n];
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
  Ctrl: Boolean;
begin
  Ctrl := ssCtrl in GetKeyShiftState;

  try
    DisableAllActions;
    if (Item <> nil) and (BreakPoints <> nil) then
    begin
      idx := FViewStart + Item.Index;
      if idx >= GetSelectedCallstack.Count then Exit;
      Entry := GetSelectedCallstack.Entries[idx];
      if not DebugBoss.GetFullFilename(Entry.UnitInfo, FileName, False) then
        Exit;
      BreakPoint := BreakPoints.Find(FileName, Entry.Line);
      if BreakPoint <> nil then begin
        if Ctrl
        then BreakPoint.Enabled := not BreakPoint.Enabled
        else DebugBoss.DoDeleteBreakPoint(BreakPoint.Source, BreakPoint.Line)
      end else begin
        DebugBoss.LockCommandProcessing;
        try
          DebugBoss.DoCreateBreakPoint(FileName, Entry.Line, False, BreakPoint);
          if Ctrl and (BreakPoint <> nil)
          then BreakPoint.Enabled := False;
        finally
          DebugBoss.UnLockCommandProcessing;
        end;
      end;
    end;
  finally
    EnableAllActions;
  end;
end;

procedure TCallStackDlg.DoBreakPointsChanged;
begin
  UpdateView;
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

procedure TCallStackDlg.ToolButtonPowerClick(Sender: TObject);
begin
  if ToolButtonPower.Down
  then begin
    ToolButtonPower.ImageIndex := FPowerImgIdx;
    UpdateView;
  end
  else ToolButtonPower.ImageIndex := FPowerImgIdxGrey;
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
  try
  DisableAllActions;
    Entry := GetCurrentEntry;
    if Entry = nil then Exit;

    GetSelectedCallstack.ChangeCurrentIndex(Entry.Index);
    if GetSelectedSnapshot <> nil
    then CallStackMonitor.NotifyCurrent; // TODO: move to snapshot callstack object
  finally
    EnableAllActions;
  end;
end;

procedure TCallStackDlg.actShowClick(Sender: TObject);
begin
  JumpToSource;
end;

procedure TCallStackDlg.actViewBottomExecute(Sender: TObject);
begin
  try
    DisableAllActions;
    if GetSelectedCallstack <> nil
    then SetViewStart(GetSelectedCallstack.Count - FViewLimit)
    else SetViewStart(0);
  finally
    EnableAllActions;
  end;
end;

procedure TCallStackDlg.actToggleBreakPointExecute(Sender: TObject);
begin
  ToggleBreakpoint(lvCallStack.Selected);
end;

procedure TCallStackDlg.actViewGotoExecute(Sender: TObject);
begin
  try
    DisableAllActions;
    SetViewStart(StrToIntDef(txtGoto.Text, 0));
  finally
    EnableAllActions;
  end;
end;

procedure TCallStackDlg.actViewMoreExecute(Sender: TObject);
begin
  try
    DisableAllActions;
    ToolButtonPower.Down := True;
    ToolButtonPowerClick(nil);
    ViewLimit := ViewLimit + FViewCount;
  finally
    EnableAllActions;
  end;
end;

procedure TCallStackDlg.actViewTopExecute(Sender: TObject);
begin
  try
    DisableAllActions;
    ToolButtonPower.Down := True;
    ToolButtonPowerClick(nil);
    SetViewStart(0);
  finally
    EnableAllActions;
  end;
end;

procedure TCallStackDlg.BreakPointChanged(const ASender: TIDEBreakPoints;
  const ABreakpoint: TIDEBreakPoint);
var
  i, idx: Integer;
  Entry: TCallStackEntry;
  Stack: TCallStack;
begin
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataWindow: TCallStackDlg.BreakPointChanged ',  DbgSName(ASender), '  Upd:', IsUpdating]); {$ENDIF}
  Stack := GetSelectedCallstack;
  if (BreakPoints = nil) or (Stack = nil) then
    Exit;

  for i := 0 to lvCallStack.Items.Count - 1 do
  begin
    idx := FViewStart + lvCallStack.Items[i].Index;
    if idx >= Stack.Count then
      Continue;
    Entry := Stack.Entries[idx];
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
  Caption := lisMenuViewCallStack;
  ToolButtonPower.Caption := lisDbgWinPower;
  ToolButtonPower.Hint := lisDbgWinPowerHint;
  for i:= 0 to mnuLimit.Items.Count-1 do
    mnuLimit.Items[i].Caption:= Format(lisMaxS, [mnuLimit.Items[i].Tag]);
  actViewMore.Caption := lisMore;
  actViewTop.Caption := lisTop;
  actViewBottom.Caption := lisBottom;
  actViewGoto.Caption := lisGotoSelectedSourceLine;
  actShow.Caption := lisViewSource;
  actToggleBreakPoint.Caption := uemToggleBreakpoint;
  actSetCurrent.Caption := lisCurrent;
  actCopyAll.Caption := lisCopyAll;

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
  FPowerImgIdx := IDEImages.LoadImage(16, 'debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage(16, 'debugger_power_grey');
  ToolButtonPower.ImageIndex := FPowerImgIdx;

  lvCallStack.SmallImages := IDEImages.Images_16;
  imgCurrentLine := IDEImages.LoadImage(16, 'debugger_current_line');
  imgSourceLine := IDEImages.LoadImage(16, 'debugger_source_line');
  imgNoSourceLine := IDEImages.LoadImage(16, 'debugger_nosource_line');
  imgBreakPoint := IDEImages.LoadImage(16, 'ActiveBreakPoint');
  imgCurrentLineAtBreakPoint := IDEImages.LoadImage(16, 'debugger_current_line_breakpoint');
  FImgBreakPointDisabled := IDEImages.LoadImage(16, 'InactiveBreakPoint');
  FImgCurrentLineAtBreakPointDisabled := IDEImages.LoadImage(16, 'debugger_current_line_disabled_breakpoint');

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

procedure TCallStackDlg.actViewLimitExecute(Sender: TObject);
begin
  try
    DisableAllActions;
    ToolButtonPower.Down := True;
    ToolButtonPowerClick(nil);
    ViewLimit := FViewCount;
  finally
    EnableAllActions;
  end;
end;

procedure TCallStackDlg.SetViewStart(AStart: Integer);
begin
  if GetSelectedCallstack = nil then Exit;
  ToolButtonPower.Down := True;
  ToolButtonPowerClick(nil);

  if (AStart > GetSelectedCallstack.Count - FViewLimit)
  then AStart := GetSelectedCallstack.Count - FViewLimit;
  if AStart < 0 then AStart := 0;
  if FViewStart = AStart then Exit;
  
  FViewStart:= AStart;
  txtGoto.Text:= IntToStr(AStart);
  UpdateView;
end;

procedure TCallStackDlg.SetViewMax;
begin
//  If GetSelectedCallstack = nil
//  then lblViewCnt.Caption:= '0'
//  else lblViewCnt.Caption:= IntToStr(GetSelectedCallstack.Count);
end;

procedure TCallStackDlg.SetViewLimit(const AValue: Integer);
begin
  ToolButtonPower.Down := True;
  ToolButtonPowerClick(nil);
  if FViewLimit = AValue then Exit;
  if (GetSelectedCallstack <> nil)
  and (FViewStart + FViewLimit >= GetSelectedCallstack.Count)
  and (AValue > FViewLimit)
  then begin
    FViewStart := GetSelectedCallstack.Count - AValue;
    if FViewStart < 0 then FViewStart := 0;
  end;
  FViewLimit := AValue;
  UpdateView;
end;

function TCallStackDlg.GetFunction(const Entry: TCallStackEntry): string;
begin
  Result := Entry.GetFunctionWithArg;
end;

procedure TCallStackDlg.GotoIndex(AIndex: Integer);
begin
  if AIndex < 0 then Exit;
  if AIndex >= GetSelectedCallstack.Count then Exit;
  

end;

end.

