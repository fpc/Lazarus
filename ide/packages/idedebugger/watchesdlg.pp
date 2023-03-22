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
  Classes, Forms, Controls, math, sysutils, LazLoggerBase, LazUTF8, Clipbrd,
  {$ifdef Windows} ActiveX, {$else} laz.FakeActiveX, {$endif}
  IDEWindowIntf, Menus, ComCtrls, ActnList, ExtCtrls, StdCtrls, LCLType,
  LMessages, IDEImagesIntf, Debugger, DebuggerTreeView, IdeDebuggerBase,
  DebuggerDlg, DbgIntfBaseTypes, DbgIntfDebuggerBase, DbgIntfMiscClasses,
  SynEdit, laz.VirtualTrees, SpinEx, LazDebuggerIntf, LazDebuggerIntfBaseTypes,
  BaseDebugManager, EnvironmentOpts, IdeDebuggerWatchResult,
  IdeDebuggerWatchResPrinter, ArrayNavigationFrame, IdeDebuggerUtils,
  IdeIntfStrConsts, IdeDebuggerStringConstants, DbgTreeViewWatchData;

type

  TWatchesDlgStateFlags = set of (
    wdsfUpdating,
    wdsfNeedDeleteAll,
    wdsfNeedDeleteCurrent,
    wdsDeleting
  );

  TDbgTreeViewWatchValueMgr = class;

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
    ToolButton1: TToolButton;
    btnShowDataAddr: TToolButton;
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
    procedure btnShowDataAddrClick(Sender: TObject);
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
    FQueuedUnLockCommandProcessing: Boolean;
    procedure DoUnLockCommandProcessing(Data: PtrInt);
    function GetWatches: TIdeWatches;
    procedure ContextChanged(Sender: TObject);
    procedure SnapshotChanged(Sender: TObject);
  private
    FWatchTreeMgr: TDbgTreeViewWatchValueMgr;
    FWatchPrinter: TWatchResultPrinter;
    FWatchesInView: TIdeWatches;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    FUpdateAllNeeded, FInEndUpdate: Boolean;
    FWatchInUpDateItem, FCurrentWatchInUpDateItem: TIdeWatch;
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
    procedure DoBeginUpdate; override;
    procedure DoEndUpdate; override;
    procedure DoWatchesChanged; override;
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function IsShortcut(var Message: TLMKey): Boolean; override;
    property WatchesMonitor;
    property ThreadsMonitor;
    property CallStackMonitor;
    property BreakPoints;
    property SnapshotManager;
  end;

  { TDbgTreeViewWatchValueMgr }

  TDbgTreeViewWatchValueMgr = class(TDbgTreeViewWatchDataMgr)
  private
    FQueuedUnLockCommandProcessing: Boolean;
    procedure DoUnLockCommandProcessing(Data: PtrInt);
  protected
    FWatchDlg: TWatchesDlg;

    function WatchAbleResultFromNode(AVNode: PVirtualNode): IWatchAbleResultIntf; override;
    function WatchAbleResultFromObject(AWatchAble: TObject): IWatchAbleResultIntf; override;

    procedure UpdateColumnsText(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode); override;
    procedure ConfigureNewSubItem(AWatchAble: TObject); override;
    procedure UpdateSubItems(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf;
      AVNode: PVirtualNode; out ChildCount: LongWord); override;
    procedure UpdateSubItemsLocked(AWatchAble: TObject; AWatchAbleResult: IWatchAbleResultIntf;
      AVNode: PVirtualNode; out ChildCount: LongWord); override;
  end;


implementation

{$R *.lfm}

var
  DBG_DATA_MONITORS: PLazLoggerLogGroup;
  WatchWindowCreator: TIDEWindowCreator;
const
  COL_WATCH_EXPR  = 1;
  COL_WATCH_VALUE = 2;
  COL_WATCH_DATAADDR = 4;
  COL_SPLITTER_INSPECT = 3;
  COL_WIDTHS: Array[1..4] of integer = ( 100,  200, 300, 150);

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
  FWatchTreeMgr := TDbgTreeViewWatchValueMgr.Create(tvWatches);
  FWatchTreeMgr.FWatchDlg := Self;

  FWatchPrinter := TWatchResultPrinter.Create;
  FWatchPrinter.FormatFlags := [rpfClearMultiLine];
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

  btnShowDataAddr.ImageIndex := IDEImages.LoadImage('ce_implementation');

  Caption:=liswlWatchList;

  tvWatches.Header.Columns[0].Text := liswlExpression;
  tvWatches.Header.Columns[1].Text := dlgValueColor;
  tvWatches.Header.Columns[2].Text := dlgValueDataAddr;
  tvWatches.Header.Columns[0].Width := COL_WIDTHS[COL_WATCH_EXPR];
  tvWatches.Header.Columns[1].Width := COL_WIDTHS[COL_WATCH_VALUE];
  tvWatches.Header.Columns[2].Width := COL_WIDTHS[COL_WATCH_DATAADDR];

  //tvWatches.OnItemRemoved := @DoItemRemovedFromView;
end;

destructor TWatchesDlg.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  if FQueuedUnLockCommandProcessing then
    DebugBoss.UnLockCommandProcessing;
  FQueuedUnLockCommandProcessing := False;

  FreeAndNil(FWatchPrinter);
  FWatchTreeMgr.Free;
  tvWatches.Clear; // Must clear all nodes before any owned "Nav := TArrayNavigationBar" are freed;
  inherited Destroy;
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
  AllCanEnable, AllCanDisable, HasTopWatchSelected: Boolean;
  VNode: PVirtualNode;
begin
  if IsUpdating then exit;
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
  HasTopWatchSelected := False;

  for VNode in tvWatches.NoInitNodes do begin
    VNdWatch := TIdeWatch(tvWatches.NodeItem[VNode]);
    if VNdWatch <> nil then begin
      if tvWatches.Selected[VNode] then begin
        SelCanEnable := SelCanEnable or not VNdWatch.Enabled;
        SelCanDisable := SelCanDisable or VNdWatch.Enabled;
        HasTopWatchSelected := HasTopWatchSelected or (VNdWatch = VNdWatch.TopParentWatch);
      end;
      AllCanEnable := AllCanEnable or not VNdWatch.Enabled;
      AllCanDisable := AllCanDisable or VNdWatch.Enabled;
    end;
  end;

  actToggleCurrentEnable.Enabled := ItemSelected;
  actToggleCurrentEnable.Checked := ItemSelected and Watch.Enabled;

  actEnableSelected.Enabled := SelCanEnable;
  actDisableSelected.Enabled := SelCanDisable;
  actDeleteSelected.Enabled := (tvWatches.SelectedCount > 0) and HasTopWatchSelected;

  actAddWatchPoint.Enabled := ItemSelected;
  actEvaluate.Enabled := ItemSelected;
  actInspect.Enabled := ItemSelected;

  actEnableAll.Enabled := AllCanEnable;
  actDisableAll.Enabled := AllCanDisable;
  actDeleteAll.Enabled := tvWatches.RootNodeCount > 0;

  actCopyName.Enabled := ItemSelected;
  actCopyValue.Enabled := ItemSelected;

  actProperties.Enabled := ItemSelected and (Watch.TopParentWatch = Watch);
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
  Nodes: TNodeArray;
  Target, N, NTarget, NewNode: PVirtualNode;
  AWatch, ASourceWatch, ATargetWatch: TIdeWatch;
  NewIdx: Integer;
begin
  if Source = tvWatches then begin
    if (not (FWatchesInView is TCurrentWatches)) or (GetSelectedSnapshot <> nil) then
      exit;

    Nodes := tvWatches.GetSortedSelection(True);
    Target := tvWatches.GetNodeAt(Pt);
    if (Target = nil) then
      exit;

    if Mode = dmAbove then begin
      // Insert above
      if tvWatches.Selected[Target] then begin
        NTarget := tvWatches.GetPreviousSiblingNoInit(Target);
        while (NTarget <> nil) and tvWatches.Selected[NTarget] do begin
          Target := NTarget;
          NTarget := tvWatches.GetPreviousSiblingNoInit(Target);
        end;
      end;
      if Target <> nil then
        Target := tvWatches.GetPreviousSiblingNoInit(Target);
    end
    else
    if tvWatches.Selected[Target] then begin
      // Insert below
      NTarget := tvWatches.GetNextSiblingNoInit(Target);
      while (NTarget <> nil) and tvWatches.Selected[NTarget] do begin
        Target := NTarget;
        NTarget := tvWatches.GetNextSiblingNoInit(Target);
      end;
    end;
    if Target = nil then
      NTarget := tvWatches.GetFirstChildNoInit(nil)
    else
      NTarget := tvWatches.GetNextSiblingNoInit(Target);

    BeginUpdate;
    try

    for N in Nodes do begin
      if tvWatches.NodeParent[N] = nil then begin
        // Move top/outer node
        if (N = Target) or (N = NTarget) then
          continue; // already in place

        AWatch := TIdeWatch(tvWatches.NodeItem[N]);
        NewNode := N;
      end
      else begin
        // Copy child node
        ASourceWatch := TIdeWatch(tvWatches.NodeItem[N]);
        assert(ASourceWatch <> nil, 'TWatchesDlg.tvWatchesDragDrop: ASourceWatch <> nil');
        if ASourceWatch = nil then
          Continue;

        AWatch := FWatchesInView.Add(ASourceWatch.Expression);
        AWatch.Assign(ASourceWatch);

        NewNode := tvWatches.FindNodeForItem(AWatch);
      end;

      if Target = nil then
        tvWatches.MoveTo(NewNode, Target, amAddChildFirst, False)
      else
        tvWatches.MoveTo(NewNode, Target, amInsertAfter, False);

      assert(AWatch <> nil, 'TWatchesDlg.tvWatchesDragDrop: AWatch <> nil');
      if AWatch = nil then
        Continue;
      NewIdx := 0;
      if Target <> nil then begin
        ATargetWatch := TIdeWatch(tvWatches.NodeItem[Target]);
        assert(ATargetWatch <> nil, 'TWatchesDlg.tvWatchesDragDrop: ATargetWatch <> nil');
        if ATargetWatch <> nil then begin
          NewIdx := ATargetWatch.Index;
          if NewIdx < AWatch.Index then
            inc(NewIdx);
        end;
      end;
      AWatch.Index := NewIdx;
      AWatch.DisplayName := '';

      Target := NewNode;
      NTarget := tvWatches.GetNextSiblingNoInit(Target);
    end;

    DebugBoss.Watches.DoModified;
    finally
      EndUpdate;
    end;

    exit;
  end;

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
var
  N, Target: PVirtualNode;
begin
  Accept := ( (Source is TSynEdit) and (TSynEdit(Source).SelAvail) ) or
            ( (Source is TCustomEdit) and (TCustomEdit(Source).SelText <> '') ) or
            ( (Source = tvWatches) and (tvWatches.SelectedCount > 0) and
              (GetSelectedSnapshot = nil)
            )
            ;

  if Accept and (Source = tvWatches) then begin
    Target := tvWatches.GetNodeAt(Pt);
    Accept := (Target <> nil) and (tvWatches.NodeParent[Target] = nil);
    if Accept then
      case Mode of
        dmAbove: ;
        dmBelow: Accept := not tvWatches.Expanded[Target];
        else     Accept := false;
      end;
    if not Accept then
      exit;


    for N in tvWatches.SelectedNodes(True) do begin
      if tvWatches.NodeItem[N] = nil then begin
        Accept := False;
        exit;
      end;
    end;
  end;
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
  if (ActiveControl <> nil) and (
       (ActiveControl is TCustomEdit) or (ActiveControl is TCustomSpinEditEx)
     )
  then
    exit;

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

procedure TWatchesDlg.btnShowDataAddrClick(Sender: TObject);
begin
  if btnShowDataAddr.Down then
    tvWatches.Header.Columns[2].Options := tvWatches.Header.Columns[2].Options + [coVisible]
  else
    tvWatches.Header.Columns[2].Options := tvWatches.Header.Columns[2].Options - [coVisible];
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
    BeginUpdate;
    for VNode in tvWatches.SelectedNodes do
    begin
      VNdWatch := TIdeWatch(tvWatches.NodeItem[VNode]);
      if VNdWatch <> nil then
        VNdWatch.Enabled := True;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWatchesDlg.actEvaluateExecute(Sender: TObject);
var
  CurWatch: TIdeWatch;
begin
  CurWatch := GetSelected;
  if CurWatch <> nil then
    DebugBoss.EvaluateModify(CurWatch.Expression, CurWatch);
end;

procedure TWatchesDlg.actInspectExecute(Sender: TObject);
var
  CurWatch: TIdeWatch;
begin
  CurWatch := GetSelected;
  if CurWatch <> nil then
    DebugBoss.Inspect(CurWatch.Expression, CurWatch);
end;

procedure TWatchesDlg.actDisableSelectedExecute(Sender: TObject);
var
  VNode: PVirtualNode;
  VNdWatch: TIdeWatch;
begin
  try
    DisableAllActions;
    BeginUpdate;
    for VNode in tvWatches.SelectedNodes do
    begin
      VNdWatch := TIdeWatch(tvWatches.NodeItem[VNode]);
      if VNdWatch <> nil then
        VNdWatch.Enabled := False;
    end;
  finally
    EndUpdate;
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
  AWatch: TIdeWatch;
begin
  Node := tvWatches.GetFocusedNode;
  if Node = nil then
    exit;
  Clipboard.Open;
  AWatch := TIdeWatch(tvWatches.NodeItem[Node]);
  if AWatch <> nil then
    Clipboard.AsText := AWatch.Expression
  else
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
  DisableAllActions;
  BeginUpdate;
  DebugBoss.Watches.CurrentWatches.BeginUpdate;
  try
    include(FStateFlags, wdsDeleting);
    VNode := tvWatches.GetFirst;
    while VNode <> nil do begin
      tvWatches.NodeItem[VNode].Free;
      tvWatches.DeleteNode(VNode);
      VNode := tvWatches.GetFirst;
    end;
    tvWatches.Clear;
  finally
    exclude(FStateFlags, wdsDeleting);
    DebugBoss.Watches.CurrentWatches.EndUpdate;
    EndUpdate;
  end;
end;

procedure TWatchesDlg.SnapshotChanged(Sender: TObject);
var
  NewWatches: TIdeWatches;
begin
  DebugLn(DBG_DATA_MONITORS, ['DebugDataWindow: TWatchesDlg.SnapshotChanged ',  DbgSName(Sender), '  Upd:', IsUpdating]);
  BeginUpdate;
  // prevent lvWatchesSelectItem when deleting the itews. Snapsot may have been deleted
  try
    NewWatches := Watches;
    if FWatchesInView <> NewWatches
    then tvWatches.Clear;
    FWatchesInView := NewWatches;
    UpdateAll;
  finally
    EndUpdate;
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

procedure TWatchesDlg.DoUnLockCommandProcessing(Data: PtrInt);
begin
  FQueuedUnLockCommandProcessing := False;
  DebugBoss.UnLockCommandProcessing;
end;

procedure TWatchesDlg.DoBeginUpdate;
begin
  inherited DoBeginUpdate;
  tvWatches.BeginUpdate;
end;

procedure TWatchesDlg.DoEndUpdate;
begin
  if FInEndUpdate then begin
    tvWatches.EndUpdate;
    exit;
  end;

  inherited DoEndUpdate;
  if FUpdateAllNeeded then begin
    FInEndUpdate := True;
    try
      FUpdateAllNeeded := False;
      UpdateAll;
      if FUpdateAllNeeded then begin
        FUpdateAllNeeded := False;
        UpdateAll;
        DebugLn(FUpdateAllNeeded, ['TWatchesDlg failed to UpdateAll']);
      end;
    finally
      FUpdateAllNeeded := False;
      FInEndUpdate := False;
    end;
  end;

  tvWatches.EndUpdate;
  tvWatchesChange(nil, nil);
end;

procedure TWatchesDlg.DoWatchesChanged;
begin
  UpdateAll;
end;

function TWatchesDlg.ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
begin
  if AColId = COL_SPLITTER_INSPECT then begin
    ASize := nbInspect.Width;
    Result := ASize <> COL_WIDTHS[AColId];
  end
  else
  if (AColId = COL_WATCH_DATAADDR) then begin
    ASize := tvWatches.Header.Columns[2].Width;
    Result := ASize <> COL_WIDTHS[AColId];
  end
  else
  if (AColId >= 1) and (AColId <= tvWatches.Header.Columns.Count) then begin
    ASize := tvWatches.Header.Columns[AColId - 1].Width;
    Result := ASize <> COL_WIDTHS[AColId];
  end
  else
    Result := False;
end;

procedure TWatchesDlg.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_WATCH_EXPR:  tvWatches.Header.Columns[0].Width := ASize;
    COL_WATCH_VALUE: tvWatches.Header.Columns[1].Width := ASize;
    COL_WATCH_DATAADDR: tvWatches.Header.Columns[2].Width := ASize;
    COL_SPLITTER_INSPECT: nbInspect.Width := ASize;
  end;
end;

procedure TWatchesDlg.popDeleteClick(Sender: TObject);
var
  VNode, NNode: PVirtualNode;
  w: TIdeWatch;
begin
  DisableAllActions;
  BeginUpdate;
  DebugBoss.Watches.CurrentWatches.BeginUpdate;
// Subwatches are not included in this BeginUpdate.....
  try
    include(FStateFlags, wdsDeleting);
    VNode := tvWatches.GetFirstSelected;
    while VNode <> nil do begin
      NNode := tvWatches.GetNextSelected(VNode);
      w := TIdeWatch(tvWatches.NodeItem[VNode]);
      if (w <> nil) and (w = w.TopParentWatch) then begin
        if tvWatches.NodeItem[VNode] = FWatchInUpDateItem then
          Include(FStateFlags, wdsfNeedDeleteCurrent)
        else
          tvWatches.DeleteNodeEx(VNode, True);
      end;
      VNode := NNode;
    end;
  finally
    exclude(FStateFlags, wdsDeleting);
    DebugBoss.Watches.CurrentWatches.EndUpdate;
    EndUpdate;
  end;
end;

procedure TWatchesDlg.popDisableAllClick(Sender: TObject);
var
  VNode: PVirtualNode;
  VNdWatch: TIdeWatch;
begin
  try
    DisableAllActions;
    BeginUpdate;
    for VNode in tvWatches.NoInitNodes do
    begin
      VNdWatch := TIdeWatch(tvWatches.NodeItem[VNode]);
      if VNdWatch <> nil then
        VNdWatch.Enabled := False;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWatchesDlg.popEnableAllClick(Sender: TObject);
var
  VNode: PVirtualNode;
  VNdWatch: TIdeWatch;
begin
  try
    DisableAllActions;
    BeginUpdate;
    for VNode in tvWatches.NoInitNodes do
    begin
      VNdWatch := TIdeWatch(tvWatches.NodeItem[VNode]);
      if VNdWatch <> nil then
        VNdWatch.Enabled := True;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TWatchesDlg.popEnabledClick(Sender: TObject);
var
  Watch: TIdeWatch;
begin
  try
    DisableAllActions;
    BeginUpdate;
    Watch := GetSelected;
    if Watch = nil then Exit;
    popEnabled.Checked := not popEnabled.Checked;
    Watch.Enabled := popEnabled.Checked;
  finally
    EndUpdate;
  end;
end;

procedure TWatchesDlg.popPropertiesClick(Sender: TObject);
var
  Watch: TCurrentWatch;
begin
  if GetSelectedSnapshot <> nil then exit;
  try
    DisableAllActions;
    Watch := TCurrentWatch(GetSelected);
    if (Watch.TopParentWatch = Watch) then
      DebugBoss.ShowWatchProperties(Watch);
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
  s: String;
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

  if not(Watch.Enabled and Watch.HasAllValidParents(GetThreadId, GetStackframe)) then begin
    InspectMemo.WordWrap := True;
    InspectMemo.Text := '<evaluating>';
    exit;
  end;

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
  FWatchPrinter.FormatFlags := [rpfIndent, rpfMultiLine];
  if d.ResultData <> nil then
    s := FWatchPrinter.PrintWatchValue(d.ResultData, d.DisplayFormat)
  else
    s := d.Value;
  InspectMemo.Text := DebugBoss.FormatValue(d.TypeInfo, s);
  finally
    FWatchPrinter.FormatFlags := [rpfClearMultiLine];
    DebugBoss.UnLockCommandProcessing;
  end;
end;

procedure TWatchesDlg.UpdateItem(const VNode: PVirtualNode;
  const AWatch: TIdeWatch);
  function DoDelayedDelete: Boolean;
  begin
    // In case the debugger did ProcessMessages, and a "delete" action was triggered
    Result := FStateFlags * [wdsfNeedDeleteCurrent, wdsfNeedDeleteAll] <> [];
    if Result then
      exclude(FStateFlags, wdsfUpdating);
    if wdsfNeedDeleteAll in FStateFlags then
      popDeleteAllClick(nil)
    else
    if wdsfNeedDeleteCurrent in FStateFlags then begin
      Exclude(FStateFlags, wdsfNeedDeleteCurrent);
      tvWatches.DeleteNodeEx(VNode, True);
    end;
  end;
var
  WatchValue: TIdeWatchValue;
  WatchValueStr: string;
  TypInfo: TDBGType;
  HasChildren, IsOuterUpdate: Boolean;
  c: LongWord;
  da: TDBGPtr;
begin
  if (not ToolButtonPower.Down) or (not Visible) then exit;
  if (ThreadsMonitor = nil) or (CallStackMonitor = nil) then exit;
  if GetStackframe < 0 then exit; // TODO need dedicated validity property

  if not tvWatches.FullyVisible[VNode] then
    exit;

  if wdsfUpdating in FStateFlags then begin
    if FCurrentWatchInUpDateItem <> AWatch then  // The watch got data while we requested it, that is fine
      FUpdateAllNeeded := True;
    exit;
  end;

  BeginUpdate;
  include(FStateFlags, wdsfUpdating);
  DebugBoss.LockCommandProcessing;
  IsOuterUpdate := FWatchInUpDateItem = nil;
  if IsOuterUpdate then
    FWatchInUpDateItem := AWatch.TopParentWatch;
  FCurrentWatchInUpDateItem := AWatch;
  try
    FWatchTreeMgr.UpdateWatchData(AWatch, VNode);
  finally
    if IsOuterUpdate then
      FWatchInUpDateItem := nil;
    FCurrentWatchInUpDateItem := nil;
    exclude(FStateFlags, wdsfUpdating);
    DoDelayedDelete;
    EndUpdate;
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

  DebugBoss.LockCommandProcessing;
  BeginUpdate;
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
    EndUpdate;
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

function TWatchesDlg.IsShortcut(var Message: TLMKey): Boolean;
begin
  Result := false;
  if (ActiveControl <> nil) and (
       (ActiveControl is TCustomEdit) or (ActiveControl is TCustomSpinEditEx)
     )
  then
    exit;
  Result := inherited IsShortcut(Message);
end;

procedure TWatchesDlg.WatchAdd(const ASender: TIdeWatches; const AWatch: TIdeWatch);
var
  VNode: PVirtualNode;
begin
  if AWatch.Collection <> FWatchesInView then
    exit;

  BeginUpdate;
  try
    FWatchTreeMgr.AddWatchData(AWatch);
  finally
    EndUpdate;
  end;
end;

procedure TWatchesDlg.WatchUpdate(const ASender: TIdeWatches; const AWatch: TIdeWatch);
var
  VNode: PVirtualNode;
begin
  if AWatch = nil then Exit; // TODO: update all
  if AWatch.TopParentWatch.Collection <> FWatchesInView then exit;
  try DebugLnEnter(DBG_DATA_MONITORS, ['DebugDataWindow: TWatchesDlg.WatchUpdate  Upd:', IsUpdating, '  Watch=',AWatch.Expression]);

  VNode := tvWatches.FindNodeForItem(AWatch);
  if (VNode = nil) and (AWatch <> AWatch.TopParentWatch) then
    exit;

  BeginUpdate;
  try
    if VNode = nil
    then WatchAdd(ASender, AWatch)
    else UpdateItem(VNode, AWatch);
  finally
    EndUpdate;
  end;

  // TODO: if AWatch <> Selected, then prevent UpdateInspectPane
  // Selected may update later, and calling UpdateInspectPane will schedule an unsucesful attemptn to fetch data
  finally DebugLnExit(DBG_DATA_MONITORS, ['DebugDataWindow: TWatchesDlg.WatchUpdate']); end;
end;

procedure TWatchesDlg.WatchRemove(const ASender: TIdeWatches; const AWatch: TIdeWatch);
begin
  if wdsDeleting in FStateFlags then
    exit;
  tvWatches.DeleteNode(tvWatches.FindNodeForItem(AWatch));
  tvWatchesChange(nil, nil);
end;

{ TDbgTreeViewWatchValueMgr }

procedure TDbgTreeViewWatchValueMgr.DoUnLockCommandProcessing(Data: PtrInt);
begin
  FQueuedUnLockCommandProcessing := False;
  DebugBoss.UnLockCommandProcessing;
end;

function TDbgTreeViewWatchValueMgr.WatchAbleResultFromNode(AVNode: PVirtualNode): IWatchAbleResultIntf;
var
  AWatchAble: TObject;
begin
  AWatchAble := TreeView.NodeItem[AVNode];
  if AWatchAble = nil then exit(nil);

  Result := TIdeWatch(AWatchAble).Values[FWatchDlg.GetThreadId, FWatchDlg.GetStackframe];
end;

function TDbgTreeViewWatchValueMgr.WatchAbleResultFromObject(AWatchAble: TObject): IWatchAbleResultIntf;
var
  nd: TObject;
begin
  if AWatchAble = nil then exit(nil);

  Result := TIdeWatch(AWatchAble).Values[FWatchDlg.GetThreadId, FWatchDlg.GetStackframe];
end;

procedure TDbgTreeViewWatchValueMgr.UpdateColumnsText(AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode);
var
  WatchValueStr: String;
  da: TDBGPtr;
begin
  TreeView.NodeText[AVNode, COL_WATCH_EXPR-1]:= TIdeWatch(AWatchAble).DisplayName;
  TreeView.NodeText[AVNode, 2] := '';

  if (AWatchAbleResult = nil) then begin
    TreeView.NodeText[AVNode, COL_WATCH_VALUE-1]:= '<not evaluated>';
    exit;
  end;


  if AWatchAbleResult.Enabled then begin
    if (FWatchDlg.GetSelectedSnapshot = nil) or  // live watch
       (AWatchAbleResult.Validity in [ddsValid, ddsInvalid, ddsError]) // snapshot
    then begin
      if (AWatchAbleResult.Validity = ddsValid) and (AWatchAbleResult.ResultData <> nil) then begin
        FWatchDlg.FWatchPrinter.FormatFlags := [rpfClearMultiLine];
        WatchValueStr := FWatchDlg.FWatchPrinter.PrintWatchValue(AWatchAbleResult.ResultData, AWatchAbleResult.DisplayFormat);
        WatchValueStr := DebugBoss.FormatValue(AWatchAbleResult.TypeInfo, WatchValueStr);
        if (AWatchAbleResult.ResultData.ValueKind = rdkArray) and (AWatchAbleResult.ResultData.ArrayLength > 0)
        then TreeView.NodeText[AVNode, COL_WATCH_VALUE-1] := Format(drsLen, [AWatchAbleResult.ResultData.ArrayLength]) + WatchValueStr
        else TreeView.NodeText[AVNode, COL_WATCH_VALUE-1] := WatchValueStr;
        if AWatchAbleResult.ResultData.HasDataAddress then begin
          da := AWatchAbleResult.ResultData.DataAddress;
          if da = 0
          then TreeView.NodeText[AVNode, 2] := 'nil'
          else TreeView.NodeText[AVNode, 2] := '$' + IntToHex(da, HexDigicCount(da, 4, True));
        end
      end
      else begin
        if (AWatchAbleResult.TypeInfo <> nil) and
           (AWatchAbleResult.TypeInfo.Attributes * [saArray, saDynArray] <> []) and
           (AWatchAbleResult.TypeInfo.Len >= 0)
        then TreeView.NodeText[AVNode, COL_WATCH_VALUE-1] := Format(drsLen, [AWatchAbleResult.TypeInfo.Len]) + AWatchAbleResult.Value
        else TreeView.NodeText[AVNode, COL_WATCH_VALUE-1] := ClearMultiline(AWatchAbleResult.Value);
      end;
    end
    else
    if (FWatchDlg.GetSelectedSnapshot = nil) and
       (DebugBoss <> nil) and (DebugBoss.State in [dsPause, dsInternalPause])
    then
      TreeView.NodeText[AVNode, COL_WATCH_VALUE-1]:= '<evaluating>'
    else
      TreeView.NodeText[AVNode, COL_WATCH_VALUE-1]:= '<not evaluated>';
  end
  else
    TreeView.NodeText[AVNode, COL_WATCH_VALUE-1]:= '<disabled>';

end;

procedure TDbgTreeViewWatchValueMgr.ConfigureNewSubItem(AWatchAble: TObject);
begin
  if (AWatchAble <> nil) and (AWatchAble is TCurrentWatch) then
    TCurrentWatch(AWatchAble).DisplayFormat := wdfDefault;
end;

procedure TDbgTreeViewWatchValueMgr.UpdateSubItems(AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out
  ChildCount: LongWord);
begin
  exclude(FWatchDlg.FStateFlags, wdsfUpdating);
  FWatchDlg.FCurrentWatchInUpDateItem := nil;
  inherited UpdateSubItems(AWatchAble, AWatchAbleResult, AVNode, ChildCount);
end;

procedure TDbgTreeViewWatchValueMgr.UpdateSubItemsLocked(AWatchAble: TObject;
  AWatchAbleResult: IWatchAbleResultIntf; AVNode: PVirtualNode; out
  ChildCount: LongWord);
begin
  DebugBoss.LockCommandProcessing;
  DebugBoss.Watches.CurrentWatches.BeginUpdate;
  TIdeWatch(AWatchAble).BeginChildUpdate;
  try
    UpdateSubItems(AWatchAble, AWatchAbleResult, AVNode, ChildCount);
  finally
    TIdeWatch(AWatchAble).EndChildUpdate;
    DebugBoss.Watches.CurrentWatches.EndUpdate;
    if not FQueuedUnLockCommandProcessing then
      Application.QueueAsyncCall(@DoUnLockCommandProcessing, 0);
    FQueuedUnLockCommandProcessing := True;
  end;
end;

initialization

  WatchWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtWatches]);
  WatchWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  WatchWindowCreator.OnSetDividerSize := @WatchesDlgColSizeSetter;
  WatchWindowCreator.OnGetDividerSize := @WatchesDlgColSizeGetter;
  WatchWindowCreator.DividerTemplate.Add('ColumnWatchExpr',  COL_WATCH_EXPR,  @drsColWidthExpression);
  WatchWindowCreator.DividerTemplate.Add('ColumnWatchValue', COL_WATCH_VALUE, @drsColWidthValue);
  WatchWindowCreator.DividerTemplate.Add('ColumnWatchDataAddr', COL_WATCH_DATAADDR, @drsColWidthValue);
  WatchWindowCreator.DividerTemplate.Add('SplitterWatchInspect', COL_SPLITTER_INSPECT, @drsWatchSplitterInspect);
  WatchWindowCreator.CreateSimpleLayout;

  DBG_DATA_MONITORS := DebugLogger.FindOrRegisterLogGroup('DBG_DATA_MONITORS' {$IFDEF DBG_DATA_MONITORS} , True {$ENDIF} );

end.

