unit HistoryDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Debugger, DebuggerDlg, LazarusIDEStrConsts,
  BaseDebugManager, MainBase, IDEImagesIntf;

type

  { THistoryDialog }

  THistoryDialog = class(TDebuggerDlg)
    lvHistory: TListView;
    tbMakeSnap: TToolButton;
    ToolBar1: TToolBar;
    tbHistorySelected: TToolButton;
    tbPower: TToolButton;
    tbClear: TToolButton;
    ToolButton1: TToolButton;
    tbHist: TToolButton;
    tbSnap: TToolButton;
    tbRemove: TToolButton;
    ToolButton4: TToolButton;
    procedure lvHistoryDblClick(Sender: TObject);
    procedure lvHistorySelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure tbClearClick(Sender: TObject);
    procedure tbHistClick(Sender: TObject);
    procedure tbHistorySelectedClick(Sender: TObject);
    procedure tbMakeSnapClick(Sender: TObject);
    procedure tbPowerClick(Sender: TObject);
    procedure tbRemoveClick(Sender: TObject);
  private
    FSnapshotManager: TSnapshotManager;
    FSnapshotNotification: TSnapshotNotification;
    FInSnapshotChanged: Boolean;
    imgCurrentLine: Integer;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    FEnabledImgIdx, FDisabledIdx: Integer;
    procedure SetSnapshotManager(const AValue: TSnapshotManager);
    procedure SnapshotChanged(Sender: TObject);
    procedure UpdateToolbar;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property SnapshotManager: TSnapshotManager read FSnapshotManager write SetSnapshotManager;
  end;

implementation

{$R *.lfm}

{ THistoryDialog }

procedure THistoryDialog.lvHistoryDblClick(Sender: TObject);
begin
  if tbHist.Down then begin
    if (FSnapshotManager.HistoryIndex = lvHistory.Selected.Index) and
       (FSnapshotManager.HistorySelected)
    then begin
      FSnapshotManager.HistorySelected := False;
    end
    else begin
      FSnapshotManager.HistoryIndex := lvHistory.Selected.Index;
      FSnapshotManager.HistorySelected := True;
    end;
  end else begin
    if (FSnapshotManager.SnapshotIndex = lvHistory.Selected.Index) and
       (FSnapshotManager.SnapshotSelected)
    then begin
      FSnapshotManager.SnapshotSelected := False;
    end
    else begin
      FSnapshotManager.SnapshotIndex := lvHistory.Selected.Index;
      FSnapshotManager.SnapshotSelected := True;
    end;
  end;
end;

procedure THistoryDialog.lvHistorySelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  UpdateToolbar;
end;

procedure THistoryDialog.tbClearClick(Sender: TObject);
begin
  if FSnapshotManager <> nil
  then FSnapshotManager.Clear;
end;

procedure THistoryDialog.tbHistClick(Sender: TObject);
begin
  if (FSnapshotManager = nil) or (FInSnapshotChanged) then exit;
  if tbHistorySelected.Down then begin
    if tbSnap.Down then FSnapshotManager.SnapshotSelected := True;
    if tbHist.Down then FSnapshotManager.HistorySelected := True;
  end;
  SnapshotChanged(nil);
end;

procedure THistoryDialog.tbHistorySelectedClick(Sender: TObject);
begin
  if tbHistorySelected.Down
  then tbHistorySelected.ImageIndex := FEnabledImgIdx
  else tbHistorySelected.ImageIndex := FDisabledIdx;
  if FSnapshotManager = nil then exit;
  if tbHist.Down
  then FSnapshotManager.HistorySelected := tbHistorySelected.Down
  else FSnapshotManager.SnapshotSelected := tbHistorySelected.Down
end;

procedure THistoryDialog.tbMakeSnapClick(Sender: TObject);
begin
  if (FSnapshotManager = nil) or (FSnapshotManager.Current = nil) then exit;
  FSnapshotManager.Current.AddToSnapshots;
end;

procedure THistoryDialog.tbPowerClick(Sender: TObject);
begin
  if tbPower.Down
  then tbPower.ImageIndex := FPowerImgIdx
  else tbPower.ImageIndex := FPowerImgIdxGrey;
  if FSnapshotManager <> nil
  then FSnapshotManager.Active := tbPower.Down;
end;

procedure THistoryDialog.tbRemoveClick(Sender: TObject);
begin
  if lvHistory.Selected = nil then exit;
  if tbHist.Down then begin
    FSnapshotManager.History[lvHistory.Selected.Index].RemoveFromHistory;
  end else begin
    FSnapshotManager.Snapshots[lvHistory.Selected.Index].RemoveFromSnapshots;
  end;
end;

procedure THistoryDialog.SnapshotChanged(Sender: TObject);
var
  i, j, cur: Integer;
  Item: TListItem;
  Lst: TSnapshotList;
begin
  if (FSnapshotManager = nil) or FInSnapshotChanged then exit;

  FInSnapshotChanged:= True;
  try
    UpdateToolbar;
  finally
    FInSnapshotChanged := False;
  end;

  j := -1;
  lvHistory.BeginUpdate;
  try
    if tbSnap.Down
    then begin
      Lst := FSnapshotManager.Snapshots;
      if FSnapshotManager.SnapshotSelected
      then cur := FSnapshotManager.SnapshotIndex
      else cur := -1;
    end else begin
      Lst := FSnapshotManager.History;
      if FSnapshotManager.HistorySelected
      then cur := FSnapshotManager.HistoryIndex
      else cur := -1;
    end;

    i := Lst.Count;
    while lvHistory.Items.Count > i do lvHistory.Items.Delete(i);
    while lvHistory.Items.Count < i do begin
      Item := lvHistory.Items.Add;
      Item.SubItems.add('');
      Item.SubItems.add('');
    end;

    if Lst.Count = 0 then exit;

    for i := 0 to Lst.Count - 1 do begin
      lvHistory.Items[i].Caption := '';
      if (i = cur)
      then begin
        lvHistory.Items[i].ImageIndex := imgCurrentLine;
        j := i;
      end
      else lvHistory.Items[i].ImageIndex := -1;

      lvHistory.Items[i].SubItems[0] := TimeToStr(Lst[i].TimeStamp);
      lvHistory.Items[i].SubItems[1] := Lst[i].LocationAsText;
      lvHistory.Items[i].Data        := Lst[i];
    end;

  finally
    lvHistory.EndUpdate;
  end;
  if j >= 0
  then lvHistory.Items[j].MakeVisible(False);
end;

procedure THistoryDialog.UpdateToolbar;
var
  Lst: TSnapshotList;
  Sel: Boolean;
begin
  if FSnapshotManager.Snapshots.Count > 0 then begin
    tbSnap.Enabled := True;
  end else begin
    tbSnap.Enabled := False;
    tbHist.Down := True;
  end;

  if tbSnap.Down
  then begin
    Lst := FSnapshotManager.Snapshots;
    Sel := FSnapshotManager.SnapshotSelected;
  end else begin
    Lst := FSnapshotManager.History;
    Sel := FSnapshotManager.HistorySelected;
  end;

  tbHistorySelected.Enabled := Lst.Count > 0;
  if not tbHistorySelected.Enabled
  then tbHistorySelected.Down := False
  else tbHistorySelected.Down := Sel;
  tbHistorySelected.Click;

  tbClear.Enabled := (FSnapshotManager.History.Count > 0) or (FSnapshotManager.Snapshots.Count > 0);

  tbMakeSnap.Enabled := (FSnapshotManager.Current <> nil) and (not FSnapshotManager.Current.IsSnapshot);
  tbRemove.Enabled := lvHistory.Selected <> nil;
end;

procedure THistoryDialog.SetSnapshotManager(const AValue: TSnapshotManager);
begin
  if FSnapshotManager = AValue then exit;
  if FSnapshotManager <> nil then FSnapshotManager.RemoveNotification(FSnapshotNotification);
  FSnapshotManager := AValue;
  if FSnapshotManager <> nil then FSnapshotManager.AddNotification(FSnapshotNotification);
  SnapshotChanged(nil);
end;

constructor THistoryDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FInSnapshotChanged := False;
  Caption:= histdlgFormName;
  lvHistory.Column[0].Caption := histdlgColumnCur;
  lvHistory.Column[1].Caption := histdlgColumnTime;
  lvHistory.Column[2].Caption := histdlgColumnLoc;

  FSnapshotNotification := TSnapshotNotification.Create;
  FSnapshotNotification.AddReference;
  FSnapshotNotification.OnChange := @SnapshotChanged;
  FSnapshotNotification.OnCurrent := @SnapshotChanged;

  imgCurrentLine := IDEImages.LoadImage(16, 'debugger_current_line');
  lvHistory.SmallImages := IDEImages.Images_16;

  ToolBar1.Images := IDEImages.Images_16;

  FPowerImgIdx     := IDEImages.LoadImage(16, 'debugger_power');
  FPowerImgIdxGrey := IDEImages.LoadImage(16, 'debugger_power_grey');
  FEnabledImgIdx   := IDEImages.LoadImage(16, 'debugger_enable');
  FDisabledIdx     := IDEImages.LoadImage(16, 'debugger_disable');

  tbPower.Hint := histdlgBtnPowerHint;
  tbHistorySelected.Hint := histdlgBtnEnableHint;

  tbClear.ImageIndex := IDEImages.LoadImage(16, 'menu_clean');
  tbClear.Hint  := histdlgBtnClearHint;

  tbHist.ImageIndex := IDEImages.LoadImage(16, 'clock');
  tbHist.Hint  := histdlgBtnShowHistHint;

  tbSnap.ImageIndex := IDEImages.LoadImage(16, 'camera');
  tbSnap.Hint  := histdlgBtnShowSnapHint;

  tbMakeSnap.ImageIndex := IDEImages.LoadImage(16, 'camera_add');
  tbMakeSnap.Hint  := histdlgBtnMakeSnapHint;

  tbRemove.ImageIndex := IDEImages.LoadImage(16, 'laz_delete');
  tbRemove.Hint  := histdlgBtnRemoveHint;

  tbPowerClick(nil);
  tbHistorySelectedClick(nil);
end;

destructor THistoryDialog.Destroy;
begin
  SetSnapshotManager(nil);
  FSnapshotNotification.OnChange := nil;
  FSnapshotNotification.OnCurrent := nil;
  FSnapshotNotification.ReleaseReference;
  inherited Destroy;
end;

end.

