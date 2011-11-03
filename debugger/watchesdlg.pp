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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}

unit WatchesDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs, math,
  IDEWindowIntf, IDEOptionDefs,
  StdCtrls, Buttons, Menus, ComCtrls, LCLType, ActnList, IDEImagesIntf,
  EnvironmentOpts, LazarusIDEStrConsts, DebuggerStrConst,
  Debugger, DebuggerDlg, BaseDebugManager;

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
    actToggleCurrentEnable: TAction;
    actPower: TAction;
    ActionList1: TActionList;
    actProperties: TAction;
    lvWatches: TListView;
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
    ToolBar1: TToolBar;
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
    procedure actDisableSelectedExecute(Sender: TObject);
    procedure actEnableSelectedExecute(Sender: TObject);
    procedure actPowerExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvWatchesDblClick(Sender: TObject);
    procedure lvWatchesSelectItem(Sender: TObject; AItem: TListItem; Selected: Boolean);
    procedure popAddClick(Sender: TObject);
    procedure popPropertiesClick(Sender: TObject);
    procedure popEnabledClick(Sender: TObject);
    procedure popDeleteClick(Sender: TObject);
    procedure popDisableAllClick(Sender: TObject);
    procedure popEnableAllClick(Sender: TObject);
    procedure popDeleteAllClick(Sender: TObject);
  private
    function GetWatches: TWatches;
    procedure ContextChanged(Sender: TObject);
    procedure SnapshotChanged(Sender: TObject);
  private
    FWatchesInView: TWatches;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    FUpdateAllNeeded, FUpdatingAll: Boolean;
    FStateFlags: TWatchesDlgStateFlags;
    function GetSelected: TCurrentWatch;
    function  GetThreadId: Integer;
    function  GetSelectedThreads(Snap: TSnapshot): TThreads;
    function GetStackframe: Integer;
    procedure WatchAdd(const ASender: TWatches; const AWatch: TWatch);
    procedure WatchUpdate(const ASender: TWatches; const AWatch: TWatch);
    procedure WatchRemove(const ASender: TWatches; const AWatch: TWatch);

    procedure UpdateItem(const AItem: TListItem; const AWatch: TWatch);
    procedure UpdateAll;
    procedure DisableAllActions;
    function  GetSelectedSnapshot: TSnapshot;
    property Watches: TWatches read GetWatches;
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
  WatchWindowCreator: TIDEWindowCreator;
const
  COL_WATCH_EXPR  = 1;
  COL_WATCH_VALUE = 2;

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

  WatchesNotification.OnAdd       := @WatchAdd;
  WatchesNotification.OnUpdate    := @WatchUpdate;
  WatchesNotification.OnRemove    := @WatchRemove;
  ThreadsNotification.OnCurrent   := @ContextChanged;
  CallstackNotification.OnCurrent := @ContextChanged;
  SnapshotNotification.OnCurrent  := @SnapshotChanged;

  ActionList1.Images := IDEImages.Images_16;
  ToolBar1.Images := IDEImages.Images_16;
  mnuPopup.Images := IDEImages.Images_16;

  FPowerImgIdx := IDEImages.LoadImage(16, 'debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage(16, 'debugger_power_grey');
  actPower.ImageIndex := FPowerImgIdx;
  actPower.Caption := lisDbgWinPower;
  actPower.Hint := lisDbgWinPowerHint;

  actAddWatch.Caption:=liswlAdd;
  actAddWatch.ImageIndex := IDEImages.LoadImage(16, 'laz_add');

  actToggleCurrentEnable.Caption := liswlEnabled;

  actEnableSelected.Caption := lisDbgItemEnable;
  actEnableSelected.Hint    := lisDbgItemEnableHint;
  actEnableSelected.ImageIndex := IDEImages.LoadImage(16, 'debugger_enable');

  actDisableSelected.Caption := lisDbgItemDisable;
  actDisableSelected.Hint    := lisDbgItemDisableHint;
  actDisableSelected.ImageIndex := IDEImages.LoadImage(16, 'debugger_disable');

  actDeleteSelected.Caption := liswlDelete; //lisDbgItemDelete;
  actDeleteSelected.Hint    := lisDbgItemDeleteHint;
  actDeleteSelected.ImageIndex := IDEImages.LoadImage(16, 'laz_delete');

  actEnableAll.Caption := liswlENableAll; //lisDbgAllItemEnable;
  actEnableAll.Hint    := lisDbgAllItemEnableHint;
  actEnableAll.ImageIndex := IDEImages.LoadImage(16, 'debugger_enable_all');

  actDisableAll.Caption := liswlDIsableAll; //lisDbgAllItemDisable;
  actDisableAll.Hint    := lisDbgAllItemDisableHint;
  actDisableAll.ImageIndex := IDEImages.LoadImage(16, 'debugger_disable_all');

  actDeleteAll.Caption := liswlDeLeteAll; //lisDbgAllItemDelete;
  actDeleteAll.Hint    := lisDbgAllItemDeleteHint;
  actDeleteAll.ImageIndex := IDEImages.LoadImage(16, 'menu_clean');

  actProperties.Caption:= liswlProperties;
  actProperties.ImageIndex := IDEImages.LoadImage(16, 'menu_environment_options');

  actAddWatchPoint.Caption := lisWatchToWatchPoint;

  Caption:=liswlWatchList;

  lvWatches.Columns[0].Caption:=liswlExpression;
  lvWatches.Columns[1].Caption:=dlgValueColor;
  lvWatches.Column[0].Width := 100;
  lvWatches.Column[1].Width := 200;
end;

function TWatchesDlg.GetSelected: TCurrentWatch;
var
  Item: TListItem;
begin
  Item := lvWatches.Selected;
  if Item = nil
  then Result := nil
  else Result := TCurrentWatch(Item.Data);
end;

function TWatchesDlg.GetThreadId: Integer;
var
  Threads: TThreads;
begin
  Result := -1;
  if (ThreadsMonitor = nil) then exit;
  Threads := GetSelectedThreads(GetSelectedSnapshot);
  if Threads <> nil
  then Result := Threads.CurrentThreadId
  else Result := 1;
end;

function TWatchesDlg.GetSelectedThreads(Snap: TSnapshot): TThreads;
begin
  if ThreadsMonitor = nil then exit(nil);
  if Snap = nil
  then Result := ThreadsMonitor.CurrentThreads
  else Result := ThreadsMonitor.Snapshots[Snap];
end;

function TWatchesDlg.GetStackframe: Integer;
var
  Snap: TSnapshot;
  Threads: TThreads;
  tid: LongInt;
  Stack: TCallStack;
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

procedure TWatchesDlg.lvWatchesSelectItem(Sender: TObject; AItem: TListItem; Selected: Boolean);
var
  ItemSelected: Boolean;
  Watch: TCurrentWatch;
  SelCanEnable, SelCanDisable: Boolean;
  AllCanEnable, AllCanDisable: Boolean;
  i: Integer;
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
    exit;
  end;

  ItemSelected := lvWatches.Selected <> nil;
  if ItemSelected then
    Watch:=TCurrentWatch(lvWatches.Selected.Data)
  else
    Watch:=nil;
  SelCanEnable := False;
  SelCanDisable := False;
  AllCanEnable := False;
  AllCanDisable := False;
  for i := 0 to lvWatches.Items.Count - 1 do begin
    if lvWatches.Items[i].Data = nil then
      continue;
    if lvWatches.Items[i].Selected then begin
      SelCanEnable := SelCanEnable or not TCurrentWatch(lvWatches.Items[i].Data).Enabled;
      SelCanDisable := SelCanDisable or TCurrentWatch(lvWatches.Items[i].Data).Enabled;
    end;
    AllCanEnable := AllCanEnable or not TCurrentWatch(lvWatches.Items[i].Data).Enabled;
    AllCanDisable := AllCanDisable or TCurrentWatch(lvWatches.Items[i].Data).Enabled;
  end;

  actToggleCurrentEnable.Enabled := ItemSelected;
  actToggleCurrentEnable.Checked := ItemSelected and Watch.Enabled;

  actEnableSelected.Enabled := SelCanEnable;
  actDisableSelected.Enabled := SelCanDisable;
  actDeleteSelected.Enabled := ItemSelected;

  actAddWatchPoint.Enabled := ItemSelected;

  actEnableAll.Enabled := AllCanEnable;
  actDisableAll.Enabled := AllCanDisable;
  actDeleteAll.Enabled := lvWatches.Items.Count > 0;

  actProperties.Enabled := ItemSelected;
  actAddWatch.Enabled := True;
  actPower.Enabled := True;
end;

procedure TWatchesDlg.lvWatchesDblClick(Sender: TObject);
begin
  if GetSelectedSnapshot <> nil then exit;
  if lvWatches.SelCount >= 0 then
    popPropertiesClick(Sender)
  else
    popAddClick(Sender);
end;

procedure TWatchesDlg.FormDestroy(Sender: TObject);
begin
  //DebugLn('TWatchesDlg.FormDestroy ',DbgSName(Self));
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

procedure TWatchesDlg.ContextChanged(Sender: TObject);
begin
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataWindow: TWatchesDlg.ContextChanged ',  DbgSName(Sender), '  Upd:', IsUpdating]); {$ENDIF}
  UpdateAll;
end;

procedure TWatchesDlg.actEnableSelectedExecute(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  try
    DisableAllActions;
    for n := 0 to lvWatches.Items.Count -1 do
    begin
      Item := lvWatches.Items[n];
      if Item.Selected then
        TCurrentWatch(Item.Data).Enabled := True;
    end;
  finally
    lvWatchesSelectItem(nil, nil, False);
  end;
end;

procedure TWatchesDlg.actDisableSelectedExecute(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  try
    DisableAllActions;
    for n := 0 to lvWatches.Items.Count -1 do
    begin
      Item := lvWatches.Items[n];
      if Item.Selected then
        TCurrentWatch(Item.Data).Enabled := False;
    end;
  finally
    lvWatchesSelectItem(nil, nil, False);
  end;
end;

procedure TWatchesDlg.actAddWatchPointExecute(Sender: TObject);
var
  NewBreakpoint: TIDEBreakPoint;
  Watch: TCurrentWatch;
begin
  Watch := GetSelected;
  if Watch = nil then Exit;
  NewBreakpoint := BreakPoints.Add(Watch.Expression, wpsGlobal, wpkWrite);
  if DebugBoss.ShowBreakPointProperties(NewBreakpoint) <> mrOk then
    NewBreakpoint.Free;
end;

procedure TWatchesDlg.popAddClick(Sender: TObject);
begin
  try
    DisableAllActions;
    DebugBoss.ShowWatchProperties(nil);
  finally
    lvWatchesSelectItem(nil, nil, False);
  end;
end;

procedure TWatchesDlg.popDeleteAllClick(Sender: TObject);
var
  n: Integer;
begin
  Include(FStateFlags, wdsfNeedDeleteAll);
  if wdsfUpdating in FStateFlags then exit;
  Exclude(FStateFlags, wdsfNeedDeleteAll);
  try
    DisableAllActions;
    for n := lvWatches.Items.Count - 1 downto 0 do
      TCurrentWatch(lvWatches.Items[n].Data).Free;
  finally
    lvWatchesSelectItem(nil, nil, False);
  end;
end;

procedure TWatchesDlg.SnapshotChanged(Sender: TObject);
var
  NewWatches: TWatches;
begin
  {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataWindow: TWatchesDlg.SnapshotChanged ',  DbgSName(Sender), '  Upd:', IsUpdating]); {$ENDIF}
  lvWatches.BeginUpdate;
  try
    NewWatches := Watches;
    if FWatchesInView <> NewWatches
    then lvWatches.Items.Clear;
    FWatchesInView := NewWatches;
    UpdateAll;
  finally
    lvWatches.EndUpdate;
  end;
end;

function TWatchesDlg.GetWatches: TWatches;
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
  Result := True;
  case AColId of
    COL_WATCH_EXPR:  ASize := lvWatches.Column[0].Width;
    COL_WATCH_VALUE: ASize := lvWatches.Column[1].Width;
    else
      Result := False;
  end;
end;

procedure TWatchesDlg.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_WATCH_EXPR:  lvWatches.Column[0].Width := ASize;
    COL_WATCH_VALUE: lvWatches.Column[1].Width := ASize;
  end;
end;

procedure TWatchesDlg.popDeleteClick(Sender: TObject);
var
  Item: TCurrentWatch;
begin
  Include(FStateFlags, wdsfNeedDeleteCurrent);
  if (wdsfUpdating in FStateFlags) then exit;
  Exclude(FStateFlags, wdsfNeedDeleteCurrent);
  try
    DisableAllActions;
    repeat
      Item := GetSelected;
      Item.Free;
    until Item = nil;
    //GetSelected.Free;
  finally
    lvWatchesSelectItem(nil, nil, False);
  end;
end;

procedure TWatchesDlg.popDisableAllClick(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  try
    DisableAllActions;
    for n := 0 to lvWatches.Items.Count - 1 do
    begin
      Item := lvWatches.Items[n];
      if Item.Data <> nil
      then TCurrentWatch(Item.Data).Enabled := False;
    end;
  finally
    lvWatchesSelectItem(nil, nil, False);
  end;
end;

procedure TWatchesDlg.popEnableAllClick(Sender: TObject);
var
  n: Integer;
  Item: TListItem;
begin
  try
    DisableAllActions;
    for n := 0 to lvWatches.Items.Count - 1 do
    begin
      Item := lvWatches.Items[n];
      if Item.Data <> nil
      then TCurrentWatch(Item.Data).Enabled := True;
    end;
  finally
    lvWatchesSelectItem(nil, nil, False);
  end;
end;

procedure TWatchesDlg.popEnabledClick(Sender: TObject);
var
  Watch: TCurrentWatch;
begin
  try
    DisableAllActions;
    Watch := GetSelected;
    if Watch = nil then Exit;
    popEnabled.Checked := not popEnabled.Checked;
    Watch.Enabled := popEnabled.Checked;
  finally
    lvWatchesSelectItem(nil, nil, False);
  end;
end;

procedure TWatchesDlg.popPropertiesClick(Sender: TObject);
begin
  try
    DisableAllActions;
    DebugBoss.ShowWatchProperties(GetSelected);
  finally
    lvWatchesSelectItem(nil, nil, False);
  end;
end;

procedure TWatchesDlg.UpdateItem(const AItem: TListItem; const AWatch: TWatch);
  function ClearMultiline(const AValue: ansistring): ansistring;
  var
    j: SizeInt;
    ow: SizeInt;
    NewLine: Boolean;
  begin
    ow:=0;
    SetLength(Result,Length(AValue));
    NewLine:=true;
    for j := 1 to Length(AValue) do begin
      if (AValue[j]=#13) or (AValue[j]=#10) then begin
        NewLine:=true;
      end else if Avalue[j]=#32 then begin
        if not NewLine then begin
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
  WatchValue: TWatchValue;
begin
// Expression
// Result
  if (not ToolButtonPower.Down) or (not Visible) then exit;
  if (ThreadsMonitor = nil) or (CallStackMonitor = nil) then exit;
  if GetStackframe < 0 then exit; // TODO need dedicated validity property

  include(FStateFlags, wdsfUpdating);
  AItem.Caption := AWatch.Expression;
  WatchValue := AWatch.Values[GetThreadId, GetStackframe];
  if (WatchValue <> nil) and
     ( (GetSelectedSnapshot = nil) or not(WatchValue.Validity in [ddsUnknown, ddsEvaluating, ddsRequested]) )
  then AItem.SubItems[0] := ClearMultiline(WatchValue.Value)
  else AItem.SubItems[0] := '<not evaluated>';
  exclude(FStateFlags, wdsfUpdating);
  if wdsfNeedDeleteCurrent in FStateFlags then
    popDeleteClick(nil);
  if wdsfNeedDeleteAll in FStateFlags then
    popDeleteAllClick(nil);
end;

procedure TWatchesDlg.UpdateAll;
var
  i, l: Integer;
  Snap: TSnapshot;
begin
  if Watches = nil then exit;
  if IsUpdating then begin
    {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataWindow: TWatchesDlg.UpdateAll: TWatchesDlg.UpdateAll  in IsUpdating:']); {$ENDIF}
    FUpdateAllNeeded := True;
    exit;
  end;
  {$IFDEF DBG_DATA_MONITORS} try DebugLnEnter(['DebugDataWindow: TWatchesDlg.UpdateAll: >>ENTER: TWatchesDlg.UpdateAll ']); {$ENDIF}

  Snap := GetSelectedSnapshot;
  if Snap <> nil
  then Caption:= liswlWatchList + ' (' + Snap.LocationAsText + ')'
  else Caption:= liswlWatchList;

  FUpdatingAll := True;
  lvWatches.BeginUpdate;
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
    lvWatches.EndUpdate;
    lvWatchesSelectItem(nil, nil, False);
  end;
  {$IFDEF DBG_DATA_MONITORS} finally DebugLnExit(['DebugDataWindow: TWatchesDlg.UpdateAll: <<EXIT: TWatchesDlg.UpdateAll ']); end; {$ENDIF}
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

procedure TWatchesDlg.WatchAdd(const ASender: TWatches; const AWatch: TWatch);
var
  Item: TListItem;
begin
  Item := lvWatches.Items.FindData(AWatch);
  if Item = nil
  then begin
    Item := lvWatches.Items.Add;
    Item.Data := AWatch;
    Item.SubItems.Add('');
    Item.Selected := True;
  end;
  
  UpdateItem(Item, AWatch);
  lvWatchesSelectItem(nil, nil, False);
end;

procedure TWatchesDlg.WatchUpdate(const ASender: TWatches; const AWatch: TWatch);
var
  Item: TListItem;
begin
  if AWatch = nil then Exit; // TODO: update all
  if AWatch.Collection <> FWatchesInView then exit;
  {$IFDEF DBG_DATA_MONITORS} try DebugLnEnter(['DebugDataWindow: TWatchesDlg.WatchUpdate  Upd:', IsUpdating, '  Watch=',AWatch.Expression]); {$ENDIF}

  Item := lvWatches.Items.FindData(AWatch);
  if Item = nil
  then WatchAdd(ASender, AWatch)
  else UpdateItem(Item, AWatch);

  if not FUpdatingAll
  then lvWatchesSelectItem(nil, nil, False);
  {$IFDEF DBG_DATA_MONITORS} finally DebugLnExit(['DebugDataWindow: TWatchesDlg.WatchUpdate']); end; {$ENDIF}
end;

procedure TWatchesDlg.WatchRemove(const ASender: TWatches; const AWatch: TWatch);
begin
  lvWatches.Items.FindData(AWatch).Free;
  lvWatchesSelectItem(nil, nil, False);
end;

initialization

  WatchWindowCreator := IDEWindowCreators.Add(NonModalIDEWindowNames[nmiwWatches]);
  WatchWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  WatchWindowCreator.OnSetDividerSize := @WatchesDlgColSizeSetter;
  WatchWindowCreator.OnGetDividerSize := @WatchesDlgColSizeGetter;
  WatchWindowCreator.DividerTemplate.Add('ColumnWatchExpr',  COL_WATCH_EXPR,  drsColWidthExpression);
  WatchWindowCreator.DividerTemplate.Add('ColumnWatchValue', COL_WATCH_VALUE, drsColWidthValue);

end.

