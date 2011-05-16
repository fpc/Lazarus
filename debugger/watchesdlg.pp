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
  StdCtrls, Buttons, Menus, ComCtrls, LCLType, ActnList, IDEImagesIntf,
  LazarusIDEStrConsts, Debugger, DebuggerDlg, BaseDebugManager;

type

  TWatchesDlgStateFlags = set of (wdsfUpdating, wdsfNeedDeleteAll, wdsfNeedDeleteCurrent);

  { TWatchesDlg }

  TWatchesDlg = class(TDebuggerDlg)
    actDeleteAll: TAction;
    actDeleteSelected: TAction;
    actDisableAll: TAction;
    actDisableSelected: TAction;
    actEnableAll: TAction;
    actEnableSelected: TAction;
    actAddWatch: TAction;
    actToggleCurrentEnable: TAction;
    actPower: TAction;
    ActionList1: TActionList;
    actProperties: TAction;
    lvWatches: TListView;
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
    procedure actDisableSelectedExecute(Sender: TObject);
    procedure actEnableSelectedExecute(Sender: TObject);
    procedure actPowerExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
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
    FCallStackMonitor: TCallStackMonitor;
    FSnapshotManager: TSnapshotManager;
    FThreadsMonitor: TThreadsMonitor;
    FWatchesMonitor: TWatchesMonitor;
    FSnapshotNotification: TSnapshotNotification;
    FWatchesNotification: TWatchesNotification;
    FThreadsNotification: TThreadsNotification;
    FCallstackNotification: TCallStackNotification;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    FUpdateAllNeeded, FUpdatingAll: Boolean;
    FStateFlags: TWatchesDlgStateFlags;
    function GetSelected: TCurrentWatch;
    function  GetThreadId: Integer;
    function  GetSelectedThreads(Snap: TSnapshot): TThreads;
    function GetStackframe: Integer;
    procedure SetSnapshotManager(const AValue: TSnapshotManager);
    procedure SetCallStackMonitor(const AValue: TCallStackMonitor);
    procedure SetThreadsMonitor(const AValue: TThreadsMonitor);
    procedure SetWatchesMonitor(const AValue: TWatchesMonitor);
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property WatchesMonitor: TWatchesMonitor read FWatchesMonitor write SetWatchesMonitor;
    property ThreadsMonitor: TThreadsMonitor read FThreadsMonitor write SetThreadsMonitor;
    property CallStackMonitor: TCallStackMonitor read FCallStackMonitor write SetCallStackMonitor;
    property SnapshotManager: TSnapshotManager read FSnapshotManager write SetSnapshotManager;
  end;


implementation

{$R *.lfm}

{ TWatchesDlg }

constructor TWatchesDlg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWatchesInView := nil;

  FWatchesNotification := TWatchesNotification.Create;
  FWatchesNotification.AddReference;
  FWatchesNotification.OnAdd := @WatchAdd;
  FWatchesNotification.OnUpdate := @WatchUpdate;
  FWatchesNotification.OnRemove := @WatchRemove;
  FStateFlags := [];

  FThreadsNotification := TThreadsNotification.Create;
  FThreadsNotification.AddReference;
  FThreadsNotification.OnCurrent := @ContextChanged;

  FCallstackNotification := TCallStackNotification.Create;
  FCallstackNotification.AddReference;
  FCallstackNotification.OnCurrent  := @ContextChanged;

  FSnapshotNotification := TSnapshotNotification.Create;
  FSnapshotNotification.AddReference;
  FSnapshotNotification.OnChange    := @SnapshotChanged;
  FSnapshotNotification.OnCurrent  := @SnapshotChanged;

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

  Caption:=liswlWatchList;

  lvWatches.Columns[0].Caption:=liswlExpression;
  lvWatches.Columns[1].Caption:=dlgValueColor;
  lvWatches.Column[0].Width := 100;
  lvWatches.Column[1].Width := 200;
end;

destructor TWatchesDlg.Destroy;
begin
  //DebugLn('TWatchesDlg.Destroy ',DbgSName(Self));
  SetWatchesMonitor(nil);
  FWatchesNotification.OnAdd := nil;
  FWatchesNotification.OnUpdate := nil;
  FWatchesNotification.OnRemove := nil;
  FWatchesNotification.ReleaseReference;
  FThreadsNotification.OnCurrent := nil;
  FThreadsNotification.ReleaseReference;
  FCallstackNotification.OnCurrent := nil;
  FCallstackNotification.ReleaseReference;
  SetSnapshotManager(nil);
  FSnapshotNotification.OnChange := nil;
  FSnapshotNotification.OnCurrent := nil;
  FSnapshotNotification.ReleaseReference;
  inherited Destroy;
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
  if (FThreadsMonitor = nil) then exit;
  Threads := GetSelectedThreads(GetSelectedSnapshot);
  if Threads <> nil
  then Result := Threads.CurrentThreadId
  else Result := 1;
end;

function TWatchesDlg.GetSelectedThreads(Snap: TSnapshot): TThreads;
begin
  if FThreadsMonitor = nil then exit(nil);
  if Snap = nil
  then Result := FThreadsMonitor.CurrentThreads
  else Result := FThreadsMonitor.Snapshots[Snap];
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

procedure TWatchesDlg.SetSnapshotManager(const AValue: TSnapshotManager);
begin
  if FSnapshotManager = AValue then exit;
  if FSnapshotManager <> nil then FSnapshotManager.RemoveNotification(FSnapshotNotification);
  FSnapshotManager := AValue;
  if FSnapshotManager <> nil then FSnapshotManager.AddNotification(FSnapshotNotification);
  SnapshotChanged(nil);
end;

procedure TWatchesDlg.SetCallStackMonitor(const AValue: TCallStackMonitor);
begin
  if FCallStackMonitor = AValue then exit;
  BeginUpdate;
  try
    if FCallStackMonitor <> nil
    then FCallStackMonitor.RemoveNotification(FCallstackNotification);

    FCallStackMonitor := AValue;

    if FCallStackMonitor <> nil
    then FCallStackMonitor.AddNotification(FCallstackNotification);

    UpdateAll;
  finally
    EndUpdate;
  end;
end;

procedure TWatchesDlg.SetThreadsMonitor(const AValue: TThreadsMonitor);
begin
  if FThreadsMonitor = AValue then exit;
  BeginUpdate;
  try
    if FThreadsMonitor <> nil
    then FThreadsMonitor.RemoveNotification(FThreadsNotification);

    FThreadsMonitor := AValue;

    if FThreadsMonitor <> nil
    then FThreadsMonitor.AddNotification(FThreadsNotification);

    UpdateAll;
  finally
    EndUpdate;
  end;
end;

procedure TWatchesDlg.SetWatchesMonitor(const AValue: TWatchesMonitor);
begin
  if FWatchesMonitor = AValue then Exit;

  BeginUpdate;
  try
    lvWatches.Items.Clear;

    if FWatchesMonitor <> nil
    then begin
      FWatchesMonitor.RemoveNotification(FWatchesNotification);
    end;

    FWatchesMonitor:=AValue;

    if FWatchesMonitor <> nil
    then begin
      FWatchesMonitor.AddNotification(FWatchesNotification);
      UpdateAll;
    end;
    
  finally
    EndUpdate;
  end;
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

procedure TWatchesDlg.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  //DebugLn('TWatchesDlg.FormCloseQuery ',dbgs(CanClose));
end;

procedure TWatchesDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  //DebugLn('TWatchesDlg.FormClose ',dbgs(ord(CloseAction)));
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
  if FWatchesMonitor = nil then exit;

  Snap := GetSelectedSnapshot;

  if Snap <> nil
  then Result := FWatchesMonitor.Snapshots[Snap]
  else Result := FWatchesMonitor.CurrentWatches;
end;

procedure TWatchesDlg.DoEndUpdate;
begin
  inherited DoEndUpdate;
  if FUpdateAllNeeded then begin
    FUpdateAllNeeded := False;
    UpdateAll;
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
  if (FThreadsMonitor = nil) or (FCallStackMonitor = nil) then exit;
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
  Snap := GetSelectedSnapshot;
  if Snap <> nil
  then Caption:= liswlWatchList + ' (' + Snap.LocationAsText + ')'
  else Caption:= liswlWatchList;

  if Watches = nil then exit;
  if UpdateCount > 0 then begin
    FUpdateAllNeeded := True;
    exit;
  end;

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
  if (SnapshotManager <> nil) and (SnapshotManager.HistorySelected)
  then Result := SnapshotManager.SelectedEntry;
end;

procedure TWatchesDlg.WatchAdd(const ASender: TWatches; const AWatch: TWatch);
var
  Item: TListItem;
  Watch: TCurrentWatch;
begin
  Item := lvWatches.Items.FindData(AWatch);
  if Item = nil
  then begin
    Item := lvWatches.Items.Add;
    Item.Data := AWatch;
    Item.SubItems.Add('');
    Item.Selected := True;
  end;
  
  Watch := GetSelected;
  if Watch <> nil then Watch.Enabled := True;

  UpdateItem(Item, AWatch);
  lvWatchesSelectItem(nil, nil, False);
end;

procedure TWatchesDlg.WatchUpdate(const ASender: TWatches; const AWatch: TWatch);
var
  Item: TListItem;
begin
  if AWatch = nil then Exit;
  if AWatch.Collection <> FWatchesInView then exit;

  Item := lvWatches.Items.FindData(AWatch);
  if Item = nil
  then WatchAdd(ASender, AWatch)
  else UpdateItem(Item, AWatch);

  if not FUpdatingAll
  then lvWatchesSelectItem(nil, nil, False);
end;

procedure TWatchesDlg.WatchRemove(const ASender: TWatches; const AWatch: TWatch);
begin
  lvWatches.Items.FindData(AWatch).Free;
  lvWatchesSelectItem(nil, nil, False);
end;

end.

