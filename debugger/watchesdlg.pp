{ $Id$ }
{               ----------------------------------------------
                 watchesdlg.pp  -  Overview of watches
                ----------------------------------------------

 @created(Fri Dec 14st WET 2001)
 @lastmod($Date$)
 @author(Shane Miller)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains the watches dialog.


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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}

unit WatchesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, math, sysutils, LazLoggerBase, Clipbrd,
  {$ifdef Windows} ActiveX, {$else} laz.FakeActiveX, {$endif}
  IDEWindowIntf, Menus, ComCtrls, ActnList, ExtCtrls, StdCtrls, LCLType,
  IDEImagesIntf, LazarusIDEStrConsts, DebuggerStrConst, Debugger,
  DebuggerTreeView, DebuggerDlg, DbgIntfBaseTypes, DbgIntfDebuggerBase,
  DbgIntfMiscClasses, SynEdit, laz.VirtualTrees, LazDebuggerIntf,
  BaseDebugManager, EnvironmentOpts;

type

  TWatchesDlgStateFlags = set of (
    wdsfUpdating,
    wdsfNeedDeleteAll,
    wdsfNeedDeleteCurrent
  );

  { TWatchesDlg }

  TWatchesDlg = class(TDebuggerDlg)
    actDeleteAll: TAction;
    actDeleteSelected: TAction;
    actDisableAll: TAction;
    actDisableSelected: TAction;
    actEnableAll: TAction;
    actEnableSelected: TAction;
    actAddWatch: TAction;
    actAddWatchPoint: TAction;
    actCopyName: TAction;
    actCopyValue: TAction;
    actInspect: TAction;
    actEvaluate: TAction;
    actToggleInspectSite: TAction;
    actToggleCurrentEnable: TAction;
    actPower: TAction;
    ActionList1: TActionList;
    actProperties: TAction;
    InspectLabel: TLabel;
    tvWatches: TDbgTreeView;
    InspectMemo: TMemo;
    MenuItem1: TMenuItem;
    nbInspect: TNotebook;
    Page1: TPage;
    popInspect: TMenuItem;
    popEvaluate: TMenuItem;
    popCopyName: TMenuItem;
    popCopyValue: TMenuItem;
    N3: TMenuItem;
    popAddWatchPoint: TMenuItem;
    mnuPopup: TPopupMenu;
    popAdd: TMenuItem;
    N1: TMenuItem; //--------------
    popProperties: TMenuItem;
    popEnabled: TMenuItem;
    popDelete: TMenuItem;
    N2: TMenuItem; //--------------
    popDisableAll: TMenuItem;
    popEnableAll: TMenuItem;
    popDeleteAll: TMenuItem;
    InspectSplitter: TSplitter;
    ToolBar1: TToolBar;
    ToolButtonInspectView: TToolButton;
    ToolButtonProperties: TToolButton;
    ToolButtonAdd: TToolButton;
    ToolButtonPower: TToolButton;
    ToolButton10: TToolButton;
    ToolButton2: TToolButton;
    ToolButtonEnable: TToolButton;
    ToolButtonDisable: TToolButton;
    ToolButtonTrash: TToolButton;
    ToolButton6: TToolButton;
    ToolButtonEnableAll: TToolButton;
    ToolButtonDisableAll: TToolButton;
    ToolButtonTrashAll: TToolButton;
    procedure actAddWatchPointExecute(Sender: TObject);
    procedure actCopyNameExecute(Sender: TObject);
    procedure actCopyValueExecute(Sender: TObject);
    procedure actDisableSelectedExecute(Sender: TObject);
    procedure actEnableSelectedExecute(Sender: TObject);
    procedure actEvaluateExecute(Sender: TObject);
    procedure actInspectExecute(Sender: TObject);
    procedure actPowerExecute(Sender: TObject);
    procedure actToggleInspectSiteExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure tvWatchesChange(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure tvWatchesDragDrop(Sender: TBaseVirtualTree; Source: TObject;
      DataObject: IDataObject; Formats: TFormatArray; Shift: TShiftState;
      const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
    procedure tvWatchesDragOver(Sender: TBaseVirtualTree; Source: TObject;
      Shift: TShiftState; State: TDragState; const Pt: TPoint; Mode: TDropMode;
      var Effect: LongWord; var Accept: Boolean);
    procedure tvWatchesFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure tvWatchesNodeDblClick(Sender: TBaseVirtualTree;
      const HitInfo: THitInfo);
    procedure popAddClick(Sender: TObject);
    procedure popPropertiesClick(Sender: TObject);
    procedure popEnabledClick(Sender: TObject);
    procedure popDeleteClick(Sender: TObject);
    procedure popDisableAllClick(Sender: TObject);
    procedure popEnableAllClick(Sender: TObject);
    procedure popDeleteAllClick(Sender: TObject);
  private
    function GetWatches: TIdeWatches;
    procedure ContextChanged(Sender: TObject);
    procedure SnapshotChanged(Sender: TObject);
  private
    FWatchesInView: TIdeWatches;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    FUpdateAllNeeded, FUpdatingAll: Boolean;
    FStateFlags: TWatchesDlgStateFlags;
    function GetSelected: TIdeWatch; // The focused Selected Node
    function  GetThreadId: Integer;
    function  GetSelectedThreads(Snap: TSnapshot): TIdeThreads;
    function GetStackframe: Integer;
    procedure WatchAdd(const {%H-}ASender: TIdeWatches; const AWatch: TIdeWatch);
    procedure WatchUpdate(const ASender: TIdeWatches; const AWatch: TIdeWatch);
    procedure WatchRemove(const {%H-}ASender: TIdeWatches; const AWatch: TIdeWatch);

    procedure UpdateInspectPane;
    procedure UpdateItem(const VNode: PVirtualNode; const AWatch: TIdeWatch);
    procedure UpdateAll;
    procedure DisableAllActions;
    function  GetSelectedSnapshot: TSnapshot;
    property Watches: TIdeWatches read GetWatches;
  protected
    procedure DoEndUpdate; override;
    procedure DoWatchesChanged; override;
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    property WatchesMonitor;
    property ThreadsMonitor;
    property CallStackMonitor;
    property BreakPoints;
    property SnapshotManager;
  end;


implementation

{$R *.lfm}

var
  DBG_DATA_MONITORS: PLazLoggerLogGroup;
  WatchWindowCreator: TIDEWindowCreator;
const
  COL_WATCH_EXPR  = 1;
  COL_WATCH_VALUE = 2;
  COL_SPLITTER_INSPECT = 3;
  COL_WIDTHS: Array[0..2] of integer = ( 100,  200, 200);

function WatchesDlgColSizeGetter(AForm: TCustomForm; AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := AForm is TWatchesDlg;
  if Result then
    Result := TWatchesDlg(AForm).ColSizeGetter(AColId, ASize);
end;

procedure WatchesDlgColSizeSetter(AForm: TCustomForm; AColId: Integer; ASize: Integer);
begin
  if AForm is TWatchesDlg then
    TWatchesDlg(AForm).ColSizeSetter(AColId, ASize);
end;

{ TWatchesDlg }

constructor TWatchesDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWatchesInView := nil;
  FStateFlags := [];
  nbInspect.Visible := False;

  WatchesNotification.OnAdd       := @WatchAdd;
  WatchesNotification.OnUpdate    := @WatchUpdate;
  WatchesNotification.OnRemove    := @WatchRemove;
  ThreadsNotification.OnCurrent   := @ContextChanged;
  CallstackNotification.OnCurrent := @ContextChanged;
  SnapshotNotification.OnCurrent  := @SnapshotChanged;

  ActionList1.Images := IDEImages.Images_16;
  ToolBar1.Images := IDEImages.Images_16;
  mnuPopup.Images := IDEImages.Images_16;

  FPowerImgIdx := IDEImages.LoadImage('debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage('debugger_power_grey');
  actPower.ImageIndex := FPowerImgIdx;
  actPower.Caption := lisDbgWinPower;
  actPower.Hint := lisDbgWinPowerHint;

  actAddWatch.Caption:=lisBtnAdd;
  actAddWatch.ImageIndex := IDEImages.LoadImage('laz_add');

  actToggleCurrentEnable.Caption := lisBtnEnabled;

  actEnableSelected.Caption := lisDbgItemEnable;
  actEnableSelected.Hint    := lisDbgItemEnableHint;
  actEnableSelected.ImageIndex := IDEImages.LoadImage('debugger_enable');

  actDisableSelected.Caption := lisDbgItemDisable;
  actDisableSelected.Hint    := lisDbgItemDisableHint;
  actDisableSelected.ImageIndex := IDEImages.LoadImage('debugger_disable');

  actDeleteSelected.Caption := lisBtnDelete;
  actDeleteSelected.Hint    := lisDbgItemDeleteHint;
  actDeleteSelected.ImageIndex := IDEImages.LoadImage('laz_delete');

  actEnableAll.Caption := liswlENableAll; //lisDbgAllItemEnable;
  actEnableAll.Hint    := lisDbgAllItemEnableHint;
  actEnableAll.ImageIndex := IDEImages.LoadImage('debugger_enable_all');

  actDisableAll.Caption := liswlDIsableAll; //lisDbgAllItemDisable;
  actDisableAll.Hint    := lisDbgAllItemDisableHint;
  actDisableAll.ImageIndex := IDEImages.LoadImage('debugger_disable_all');

  actDeleteAll.Caption := liswlDeLeteAll; //lisDbgAllItemDelete;
  actDeleteAll.Hint    := lisDbgAllItemDeleteHint;
  actDeleteAll.ImageIndex := IDEImages.LoadImage('menu_clean');

  actProperties.Caption:= liswlProperties;
  actProperties.ImageIndex := IDEImages.LoadImage('menu_environment_options');

  actToggleInspectSite.Caption:= liswlInspectPane;
  actToggleInspectSite.ImageIndex := IDEImages.LoadImage('debugger_inspect');

  actAddWatchPoint.Caption := lisWatchToWatchPoint;

  actCopyName.Caption := lisLocalsDlgCopyName;
  actCopyValue.Caption := lisLocalsDlgCopyValue;

  actInspect.Caption := lisInspect;
  actEvaluate.Caption := lisEvaluateModify;

  Caption:=liswlWatchList;

  tvWatches.Header.Columns[0].Text := liswlExpression;
  tvWatches.Header.Columns[1].Text := dlgValueColor;
  tvWatches.Header.Columns[0].Width := COL_WIDTHS[COL_WATCH_EXPR];
  tvWatches.Header.Columns[1].Width := COL_WIDTHS[COL_WATCH_VALUE];
end;

function TWatchesDlg.GetSelected: TIdeWatch;
begin
  Result := TIdeWatch(tvWatches.FocusedItem(True));
end;

function TWatchesDlg.GetThreadId: Integer;
var
  Threads: TIdeThreads;
begin
  Result := -1;
  if (ThreadsMonitor = nil) then exit;
  Threads := GetSelectedThreads(GetSelectedSnapshot);
  if Threads <> nil
  then Result := Threads.CurrentThreadId
  else Result := 1;
end;

function TWatchesDlg.GetSelectedThreads(Snap: TSnapshot): TIdeThreads;
begin
  if ThreadsMonitor = nil then exit(nil);
  if Snap = nil
  then Result := ThreadsMonitor.CurrentThreads
  else Result := ThreadsMonitor.Snapshots[Snap];
end;

function TWatchesDlg.GetStackframe: Integer;
var
  Snap: TSnapshot;
  Threads: TIdeThreads;
  tid: LongInt;
  Stack: TIdeCallStack;
begin
  if (CallStackMonitor = nil) or (ThreadsMonitor = nil)
  then begin
    Result := 0;
    exit;
  end;

  Snap := GetSelectedSnapshot;
  Threads := GetSelectedThreads(Snap);
  if Threads <> nil
  then tid := Threads.CurrentThreadId
  else tid := 1;

  if (Snap <> nil)
  then Stack := CallStackMonitor.Snapshots[Snap].EntriesForThreads[tid]
  else Stack := CallStackMonitor.CurrentCallStackList.EntriesForThreads[tid];

  if Stack <> nil
  then Result := Stack.CurrentIndex
  else Result := 0;
end;

procedure TWatchesDlg.tvWatchesChange(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
  ItemSelected: Boolean;
  Watch, VNdWatch: TIdeWatch;
  SelCanEnable, SelCanDisable: Boolean;
  AllCanEnable, AllCanDisable: Boolean;
  VNode: PVirtualNode;
begin
  if FUpdatingAll then exit;
  if GetSelectedSnapshot <> nil then begin
    actToggleCurrentEnable.Enabled := False;
    actToggleCurrentEnable.Checked := False;
    actEnableSelected.Enabled := False;
    actDisableSelected.Enabled := False;
    actDeleteSelected.Enabled := False;
    actEnableAll.Enabled := False;
    actDisableAll.Enabled := False;
    actDeleteAll.Enabled := False;
    actProperties.Enabled := False;
    actAddWatch.Enabled := False;
    actPower.Enabled := False;
    actAddWatchPoint.Enabled := False;
    actEvaluate.Enabled := False;
    actInspect.Enabled := False;
    UpdateInspectPane;
    exit;
  end;

  Watch:= GetSelected;
  ItemSelected := Watch <> nil;
  SelCanEnable := False;
  SelCanDisable := False;
  AllCanEnable := False;
  AllCanDisable := False;

  for VNode in tvWatches.NoInitNodes do begin
    VNdWatch := TIdeWatch(tvWatches.NodeItem[VNode]);
    if VNdWatch <> nil then begin
      if tvWatches.Selected[VNode] then begin
        SelCanEnable := SelCanEnable or not VNdWatch.Enabled;
        SelCanDisable := SelCanDisable or VNdWatch.Enabled;
      end;
      AllCanEnable := AllCanEnable or not VNdWatch.Enabled;
      AllCanDisable := AllCanDisable or VNdWatch.Enabled;
    end;
  end;

  actToggleCurrentEnable.Enabled := ItemSelected;
  actToggleCurrentEnable.Checked := ItemSelected and Watch.Enabled;

  actEnableSelected.Enabled := SelCanEnable;
  actDisableSelected.Enabled := SelCanDisable;
  actDeleteSelected.Enabled := tvWatches.SelectedCount > 0;

  actAddWatchPoint.Enabled := ItemSelected;
  actEvaluate.Enabled := ItemSelected;
  actInspect.Enabled := ItemSelected;

  actEnableAll.Enabled := AllCanEnable;
  actDisableAll.Enabled := AllCanDisable;
  actDeleteAll.Enabled := tvWatches.RootNodeCount > 0;

  actCopyName.Enabled := ItemSelected;
  actCopyValue.Enabled := ItemSelected;

  actProperties.Enabled := ItemSelected;
  actAddWatch.Enabled := True;
  actPower.Enabled := True;

  actToggleInspectSite.Enabled := True;

  UpdateInspectPane;
end;

procedure TWatchesDlg.tvWatchesNodeDblClick(Sender: TBaseVirtualTree;
  const HitInfo: THitInfo);
begin
  if GetSelectedSnapshot <> nil then exit;
  if (HitInfo.HitNode <> nil) and (GetSelected <> nil) then
    popPropertiesClick(Sender)
  else
    popAddClick(Sender);
end;

procedure TWatchesDlg.tvWatchesDragDrop(Sender: TBaseVirtualTree;
  Source: TObject; DataObject: IDataObject; Formats: TFormatArray;
  Shift: TShiftState; const Pt: TPoint; var Effect: LongWord; Mode: TDropMode);
var
  s: String;
  NewWatch: TCurrentWatch;
begin
  s := '';
  if (Source is TSynEdit) then s := TSynEdit(Source).SelText;
  if (Source is TCustomEdit) then s := TCustomEdit(Source).SelText;

  if s <> '' then begin
    DebugBoss.Watches.CurrentWatches.BeginUpdate;
    try
      NewWatch := DebugBoss.Watches.CurrentWatches.Add(s);
      NewWatch.DisplayFormat := wdfDefault;
      NewWatch.Enabled       := True;
      if EnvironmentOptions.DebuggerAutoSetInstanceFromClass then
        NewWatch.EvaluateFlags := [defClassAutoCast];
    finally
      DebugBoss.Watches.CurrentWatches.EndUpdate;
    end;
  end;
end;

procedure TWatchesDlg.tvWatchesDragOver(Sender: TBaseVirtualTree;
  Source: TObject; Shift: TShiftState; State: TDragState; const Pt: TPoint;
  Mode: TDropMode; var Effect: LongWord; var Accept: Boolean);
begin
  Accept := ( (Source is TSynEdit) and (TSynEdit(Source).SelAvail) ) or
            ( (Source is TCustomEdit) and (TCustomEdit(Source).SelText <> '') );
end;

procedure TWatchesDlg.tvWatchesFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
begin
  tvWatchesChange(Sender, Node);
end;

procedure TWatchesDlg.FormDestroy(Sender: TObject);
begin
  //DebugLn('TWatchesDlg.FormDestroy ',DbgSName(Self));
end;

procedure TWatchesDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  s: String;
  NewWatch: TCurrentWatch;
begin
  if (Shift * [ssShift, ssAlt, ssAltGr, ssCtrl] = [ssCtrl]) and (Key = VK_V)
  then begin
    Key := 0;
    s := Clipboard.AsText;
    if s <> '' then begin
      DebugBoss.Watches.CurrentWatches.BeginUpdate;
      try
        NewWatch := DebugBoss.Watches.CurrentWatches.Add(s);
        NewWatch.DisplayFormat := wdfDefault;
        NewWatch.Enabled       := True;
        if EnvironmentOptions.DebuggerAutoSetInstanceFromClass then
          NewWatch.EvaluateFlags := [defClassAutoCast];
      finally
        DebugBoss.Watches.CurrentWatches.EndUpdate;
      end;
    end;

    exit;
  end;

  inherited FormKeyDown(Sender, Key, Shift);
end;

procedure TWatchesDlg.FormShow(Sender: TObject);
begin
  UpdateAll;
end;

procedure TWatchesDlg.actPowerExecute(Sender: TObject);
begin
  if ToolButtonPower.Down
  then begin
    actPower.ImageIndex := FPowerImgIdx;
    ToolButtonPower.ImageIndex := FPowerImgIdx;
    UpdateAll;
  end
  else begin
    actPower.ImageIndex := FPowerImgIdxGrey;
    ToolButtonPower.ImageIndex := FPowerImgIdxGrey;
  end;
end;

procedure TWatchesDlg.actToggleInspectSiteExecute(Sender: TObject);
begin
  InspectSplitter.Visible := ToolButtonInspectView.Down;
  nbInspect.Visible := ToolButtonInspectView.Down;
  InspectSplitter.Left :=  nbInspect.Left - 1;
  if ToolButtonInspectView.Down then
    UpdateInspectPane;
end;

procedure TWatchesDlg.ContextChanged(Sender: TObject);
begin
  DebugLn(DBG_DATA_MONITORS, ['DebugDataWindow: TWatchesDlg.ContextChanged ',  DbgSName(Sender), '  Upd:', IsUpdating]);
  if (DebugBoss <> nil) and (DebugBoss.State in [dsPause, dsInternalPause]) then
    UpdateAll;
end;

procedure TWatchesDlg.actEnableSelectedExecute(Sender: TObject);
var
  VNode: PVirtualNode;
  VNdWatch: TIdeWatch;
begin
  try
    DisableAllActions;
    for VNode in tvWatches.SelectedNodes do
    begin
      VNdWatch := TIdeWatch(tvWatches.NodeItem[VNode]);
      VNdWatch.Enabled := True;
    end;
  finally
    tvWatchesChange(nil, nil);
  end;
end;

procedure TWatchesDlg.actEvaluateExecute(Sender: TObject);
var
  CurWatch: TIdeWatch;
begin
  CurWatch := GetSelected;
  if CurWatch <> nil then
    DebugBoss.EvaluateModify(CurWatch.Expression);
end;

procedure TWatchesDlg.actInspectExecute(Sender: TObject);
var
  CurWatch: TIdeWatch;
begin
  CurWatch := GetSelected;
  if CurWatch <> nil then
    DebugBoss.Inspect(CurWatch.Expression);
end;

procedure TWatchesDlg.actDisableSelectedExecute(Sender: TObject);
var
  VNode: PVirtualNode;
  VNdWatch: TIdeWatch;
begin
  try
    DisableAllActions;
    for VNode in tvWatches.SelectedNodes do
    begin
      VNdWatch := TIdeWatch(tvWatches.NodeItem[VNode]);
      VNdWatch.Enabled := False;
    end;
  finally
    tvWatchesChange(nil, nil);
  end;
end;

procedure TWatchesDlg.actAddWatchPointExecute(Sender: TObject);
var
  NewBreakpoint: TIDEBreakPoint;
  Watch: TIdeWatch;
begin
  Watch := GetSelected;
  if Watch = nil then Exit;
  NewBreakpoint := BreakPoints.Add(Watch.Expression, wpsGlobal, wpkWrite, True);
  if DebugBoss.ShowBreakPointProperties(NewBreakpoint) <> mrOk then
    ReleaseRefAndNil(NewBreakpoint)
  else
    NewBreakpoint.EndUpdate;
end;

procedure TWatchesDlg.actCopyNameExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := tvWatches.GetFocusedNode;
  if Node = nil then
    exit;
  Clipboard.Open;
  Clipboard.AsText := tvWatches.NodeText[Node, COL_WATCH_EXPR-1];
  Clipboard.Close;
end;

procedure TWatchesDlg.actCopyValueExecute(Sender: TObject);
var
  Node: PVirtualNode;
begin
  Node := tvWatches.GetFocusedNode;
  if Node = nil then
    exit;
  Clipboard.Open;
  Clipboard.AsText := tvWatches.NodeText[Node, COL_WATCH_VALUE-1];
  Clipboard.Close;
end;

procedure TWatchesDlg.popAddClick(Sender: TObject);
begin
  try
    DisableAllActions;
    DebugBoss.ShowWatchProperties(nil);
  finally
    tvWatchesChange(nil, nil);
  end;
end;

procedure TWatchesDlg.popDeleteAllClick(Sender: TObject);
var
  VNode: PVirtualNode;
begin
  Include(FStateFlags, wdsfNeedDeleteAll);
  if wdsfUpdating in FStateFlags then exit;
  Exclude(FStateFlags, wdsfNeedDeleteAll);
  tvWatches.BeginUpdate;
  try
    DisableAllActions;
    VNode := tvWatches.GetFirst;
    while VNode <> nil do begin
      tvWatches.NodeItem[VNode].Free;
      VNode := tvWatches.GetFirst;
    end;
  finally
    tvWatches.EndUpdate;
    tvWatchesChange(nil, nil);
  end;
end;

procedure TWatchesDlg.SnapshotChanged(Sender: TObject);
var
  NewWatches: TIdeWatches;
begin
  DebugLn(DBG_DATA_MONITORS, ['DebugDataWindow: TWatchesDlg.SnapshotChanged ',  DbgSName(Sender), '  Upd:', IsUpdating]);
  tvWatches.BeginUpdate;
  // prevent lvWatchesSelectItem when deleting the itews. Snapsot may have been deleted
  FUpdatingAll := True; // will be reset by UpdateAll
  try
    NewWatches := Watches;
    if FWatchesInView <> NewWatches
    then tvWatches.Clear;
    FWatchesInView := NewWatches;
    UpdateAll;
  finally
    FUpdatingAll := False; // wan reset by UpdateAll anyway
    tvWatches.EndUpdate;
  end;
end;

function TWatchesDlg.GetWatches: TIdeWatches;
var
  Snap: TSnapshot;
begin
  Result := nil;
  if WatchesMonitor = nil then exit;

  Snap := GetSelectedSnapshot;

  if Snap <> nil
  then Result := WatchesMonitor.Snapshots[Snap]
  else Result := WatchesMonitor.CurrentWatches;
end;

procedure TWatchesDlg.DoEndUpdate;
begin
  inherited DoEndUpdate;
  if FUpdateAllNeeded then begin
    FUpdateAllNeeded := False;
    UpdateAll;
  end;
end;

procedure TWatchesDlg.DoWatchesChanged;
begin
  UpdateAll;
end;

function TWatchesDlg.ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
begin
  if AColId = COL_SPLITTER_INSPECT then begin
    ASize := nbInspect.Width;
    Result := ASize <> COL_WIDTHS[AColId - 1];
  end
  else
  if (AColId - 1 >= 0) and (AColId - 1 < tvWatches.Header.Columns.Count) then begin
    ASize := tvWatches.Header.Columns[AColId - 1].Width;
    Result := ASize <> COL_WIDTHS[AColId - 1];
  end
  else
    Result := False;
end;

procedure TWatchesDlg.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_WATCH_EXPR:  tvWatches.Header.Columns[0].Width := ASize;
    COL_WATCH_VALUE: tvWatches.Header.Columns[1].Width := ASize;
    COL_SPLITTER_INSPECT: nbInspect.Width := ASize;
  end;
end;

procedure TWatchesDlg.popDeleteClick(Sender: TObject);
var
  VNode: PVirtualNode;
begin
  Include(FStateFlags, wdsfNeedDeleteCurrent);
  if (wdsfUpdating in FStateFlags) then exit;
  Exclude(FStateFlags, wdsfNeedDeleteCurrent);
  try
    DisableAllActions;
    VNode := tvWatches.GetFirstSelected;
    while VNode <> nil do begin
      tvWatches.DeleteNodeEx(VNode, True);
      VNode := tvWatches.GetFirstSelected;
    end;
  finally
    tvWatchesChange(nil, nil);
  end;
end;

procedure TWatchesDlg.popDisableAllClick(Sender: TObject);
var
  VNode: PVirtualNode;
  VNdWatch: TIdeWatch;
begin
  try
    DisableAllActions;
    for VNode in tvWatches.NoInitNodes do
    begin
      VNdWatch := TIdeWatch(tvWatches.NodeItem[VNode]);
      VNdWatch.Enabled := False;
    end;
  finally
    tvWatchesChange(nil, nil);
  end;
end;

procedure TWatchesDlg.popEnableAllClick(Sender: TObject);
var
  VNode: PVirtualNode;
  VNdWatch: TIdeWatch;
begin
  try
    DisableAllActions;
    for VNode in tvWatches.NoInitNodes do
    begin
      VNdWatch := TIdeWatch(tvWatches.NodeItem[VNode]);
      VNdWatch.Enabled := True;
    end;
  finally
    tvWatchesChange(nil, nil);
  end;
end;

procedure TWatchesDlg.popEnabledClick(Sender: TObject);
var
  Watch: TIdeWatch;
begin
  try
    DisableAllActions;
    Watch := GetSelected;
    if Watch = nil then Exit;
    popEnabled.Checked := not popEnabled.Checked;
    Watch.Enabled := popEnabled.Checked;
  finally
    tvWatchesChange(nil, nil);
  end;
end;

procedure TWatchesDlg.popPropertiesClick(Sender: TObject);
begin
  if GetSelectedSnapshot <> nil then exit;
  try
    DisableAllActions;
    DebugBoss.ShowWatchProperties(TCurrentWatch(GetSelected));
  finally
    tvWatchesChange(nil, nil);
  end;
end;

procedure TWatchesDlg.UpdateInspectPane;
var
  Watch: TIdeWatch;
  i: integer;
  d: TIdeWatchValue;
  t: TDBGType;
begin
  if not nbInspect.Visible then exit;
  DebugBoss.LockCommandProcessing;
  try

  InspectMemo.Clear;
  InspectLabel.Caption := '...';

  Watch:=GetSelected;
  if Watch = nil then
    exit;
  InspectLabel.Caption := Watch.Expression;

  d := Watch.Values[GetThreadId, GetStackframe];
  if d = nil then begin
    InspectMemo.WordWrap := True;
    InspectMemo.Text := '<evaluating>';
    exit;
  end;

  t := d.TypeInfo;

  if (t <> nil) and (t.Fields <> nil) and (t.Fields.Count > 0) and
     (d.DisplayFormat in [wdfDefault, wdfStructure])
  then begin
    InspectMemo.WordWrap := False;
    InspectMemo.Lines.BeginUpdate;
    try
      for i := 0 to t.Fields.Count - 1 do
        case t.Fields[i].DBGType.Kind of
          skSimple:
            begin
              if t.Fields[i].DBGType.Value.AsString='$0' then begin
                if t.Fields[i].DBGType.TypeName='ANSISTRING' then
                  InspectMemo.Append(t.Fields[i].Name + ': ''''')
                else
                  InspectMemo.Append(t.Fields[i].Name + ': nil');
              end
              else
                InspectMemo.Append(t.Fields[i].Name + ': ' + t.Fields[i].DBGType.Value.AsString);
            end;
          skProcedure, skFunction, skProcedureRef, skFunctionRef: ;
          else
            InspectMemo.Append(t.Fields[i].Name + ': ' + t.Fields[i].DBGType.Value.AsString);
        end;
    finally
      InspectMemo.Lines.EndUpdate;
    end;
    exit;
  end;

  InspectMemo.WordWrap := True;
  InspectMemo.Text := d.Value;
  finally
    DebugBoss.UnLockCommandProcessing;
  end;
end;

procedure TWatchesDlg.UpdateItem(const VNode: PVirtualNode;
  const AWatch: TIdeWatch);
  function ClearMultiline(const AValue: ansistring): ansistring;
  var
    j: SizeInt;
    ow: SizeInt;
    NewLine: Boolean;
  begin
    ow:=0;
    SetLength(Result{%H-},Length(AValue));
    NewLine:=true;
    for j := 1 to Length(AValue) do begin
      if (AValue[j]=#13) or (AValue[j]=#10) then begin
        NewLine:=true;
        inc(ow);
        Result[ow]:=#32; // insert one space instead of new line
      end
      else if Avalue[j] in [#9,#32] then begin
        if not NewLine then begin // strip leading spaces after new line
          inc(ow);
          Result[ow]:=#32;
        end;
      end else begin
        inc(ow);
        Result[ow]:=AValue[j];
        NewLine:=false;
      end;
    end;
    If ow>255 then begin
      //Limit watch to 255 chars in length
      Result:=Copy(Result,1,252)+'...';
    end else begin
      SetLength(Result,ow);
    end;
  end;
var
  WatchValue: TIdeWatchValue;
  WatchValueStr: string;
begin
  if (not ToolButtonPower.Down) or (not Visible) then exit;
  if (ThreadsMonitor = nil) or (CallStackMonitor = nil) then exit;
  if GetStackframe < 0 then exit; // TODO need dedicated validity property

  include(FStateFlags, wdsfUpdating);
  DebugBoss.LockCommandProcessing;
  try
    tvWatches.NodeText[VNode, COL_WATCH_EXPR-1]:= AWatch.Expression;
    WatchValue := AWatch.Values[GetThreadId, GetStackframe];
    if (WatchValue <> nil) and
       ( (GetSelectedSnapshot = nil) or not(WatchValue.Validity in [ddsUnknown, ddsEvaluating, ddsRequested]) )
    then begin
      WatchValueStr := ClearMultiline(DebugBoss.FormatValue(WatchValue.TypeInfo, WatchValue.Value));
      if (WatchValue.TypeInfo <> nil) and
         (WatchValue.TypeInfo.Attributes * [saArray, saDynArray] <> []) and
         (WatchValue.TypeInfo.Len >= 0)
      then tvWatches.NodeText[VNode, COL_WATCH_VALUE-1] := Format(drsLen, [WatchValue.TypeInfo.Len]) + WatchValueStr
      else tvWatches.NodeText[VNode, COL_WATCH_VALUE-1] := WatchValueStr;
    end
    else
      tvWatches.NodeText[VNode, COL_WATCH_VALUE-1]:= '<not evaluated>';
    exclude(FStateFlags, wdsfUpdating);
    if wdsfNeedDeleteCurrent in FStateFlags then
      popDeleteClick(nil);
    if wdsfNeedDeleteAll in FStateFlags then
      popDeleteAllClick(nil);

  finally
    DebugBoss.UnLockCommandProcessing;
    tvWatches.Invalidate;
  end;
end;

procedure TWatchesDlg.UpdateAll;
var
  i, l: Integer;
  Snap: TSnapshot;
begin
  if Watches = nil then exit;
  if IsUpdating then begin
    DebugLn(DBG_DATA_MONITORS, ['DebugDataWindow: TWatchesDlg.UpdateAll: TWatchesDlg.UpdateAll  in IsUpdating:']);
    FUpdateAllNeeded := True;
    exit;
  end;
  try DebugLnEnter(DBG_DATA_MONITORS, ['DebugDataWindow: TWatchesDlg.UpdateAll: >>ENTER: TWatchesDlg.UpdateAll ']);

  Snap := GetSelectedSnapshot;
  if Snap <> nil
  then Caption:= liswlWatchList + ' (' + Snap.LocationAsText + ')'
  else Caption:= liswlWatchList;

  FUpdatingAll := True;
  DebugBoss.LockCommandProcessing;
  tvWatches.BeginUpdate;
  try
    l := Watches.Count;
    i := 0;
    while i < l do begin
      WatchUpdate(Watches, Watches.Items[i]);
      if l <> Watches.Count then begin
        i := Max(0, i - Max(0, Watches.Count - l));
        l := Watches.Count;
      end;
      inc(i);
    end;
  finally
    FUpdatingAll := False;
    tvWatches.EndUpdate;
    DebugBoss.UnLockCommandProcessing;
    tvWatchesChange(nil, nil);
  end;
  finally DebugLnExit(DBG_DATA_MONITORS, ['DebugDataWindow: TWatchesDlg.UpdateAll: <<EXIT: TWatchesDlg.UpdateAll ']); end;
end;

procedure TWatchesDlg.DisableAllActions;
var
  i: Integer;
begin
  for i := 0 to ActionList1.ActionCount - 1 do
    (ActionList1.Actions[i] as TAction).Enabled := False;
end;

function TWatchesDlg.GetSelectedSnapshot: TSnapshot;
begin
  Result := nil;
  if (SnapshotManager <> nil) and (SnapshotManager.SelectedEntry <> nil)
  then Result := SnapshotManager.SelectedEntry;
end;

procedure TWatchesDlg.WatchAdd(const ASender: TIdeWatches; const AWatch: TIdeWatch);
var
  VNode: PVirtualNode;
begin
  VNode := tvWatches.FindNodeForItem(AWatch);
  if VNode = nil
  then begin
    VNode := tvWatches.AddChild(nil, AWatch);
    tvWatches.SelectNode(VNode);
  end;
  
  UpdateItem(VNode, AWatch);
  tvWatchesChange(nil, nil);
end;

procedure TWatchesDlg.WatchUpdate(const ASender: TIdeWatches; const AWatch: TIdeWatch);
var
  VNode: PVirtualNode;
begin
  if AWatch = nil then Exit; // TODO: update all
  if AWatch.Collection <> FWatchesInView then exit;
  try DebugLnEnter(DBG_DATA_MONITORS, ['DebugDataWindow: TWatchesDlg.WatchUpdate  Upd:', IsUpdating, '  Watch=',AWatch.Expression]);

  VNode := tvWatches.FindNodeForItem(AWatch);
  if VNode = nil
  then WatchAdd(ASender, AWatch)
  else UpdateItem(VNode, AWatch);

  // TODO: if AWatch <> Selected, then prevent UpdateInspectPane
  // Selected may update later, and calling UpdateInspectPane will schedule an unsucesful attemptn to fetch data
  if not FUpdatingAll
  then tvWatchesChange(nil, nil);
  finally DebugLnExit(DBG_DATA_MONITORS, ['DebugDataWindow: TWatchesDlg.WatchUpdate']); end;
end;

procedure TWatchesDlg.WatchRemove(const ASender: TIdeWatches; const AWatch: TIdeWatch);
begin
  tvWatches.DeleteNode(tvWatches.FindNodeForItem(AWatch));
  tvWatchesChange(nil, nil);
end;

initialization

  WatchWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtWatches]);
  WatchWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  WatchWindowCreator.OnSetDividerSize := @WatchesDlgColSizeSetter;
  WatchWindowCreator.OnGetDividerSize := @WatchesDlgColSizeGetter;
  WatchWindowCreator.DividerTemplate.Add('ColumnWatchExpr',  COL_WATCH_EXPR,  @drsColWidthExpression);
  WatchWindowCreator.DividerTemplate.Add('ColumnWatchValue', COL_WATCH_VALUE, @drsColWidthValue);
  WatchWindowCreator.DividerTemplate.Add('SplitterWatchInspect', COL_SPLITTER_INSPECT, @drsWatchSplitterInspect);
  WatchWindowCreator.CreateSimpleLayout;

  DBG_DATA_MONITORS := DebugLogger.FindOrRegisterLogGroup('DBG_DATA_MONITORS' {$IFDEF DBG_DATA_MONITORS} , True {$ENDIF} );

end.

