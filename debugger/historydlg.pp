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
    ToolBar1: TToolBar;
    tbHistorySelected: TToolButton;
    tbPower: TToolButton;
    tbClear: TToolButton;
    ToolButton1: TToolButton;
    procedure lvHistoryDblClick(Sender: TObject);
    procedure tbClearClick(Sender: TObject);
    procedure tbHistorySelectedClick(Sender: TObject);
    procedure tbPowerClick(Sender: TObject);
  private
    FSnapshotManager: TSnapshotManager;
    FSnapshotNotification: TSnapshotNotification;
    FInSnapshotChanged: Boolean;
    imgCurrentLine: Integer;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    FEnabledImgIdx, FDisabledIdx: Integer;
    procedure SetSnapshotManager(const AValue: TSnapshotManager);
    procedure SnapshotChanged(Sender: TObject);
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
  if (FSnapshotManager.HistoryIndex = lvHistory.Selected.Index) and
     (FSnapshotManager.HistorySelected)
  then begin
    FSnapshotManager.HistorySelected := False;
  end
  else begin
    FSnapshotManager.HistoryIndex := lvHistory.Selected.Index;
    FSnapshotManager.HistorySelected := True;
  end;
end;

procedure THistoryDialog.tbClearClick(Sender: TObject);
begin
  if FSnapshotManager <> nil
  then FSnapshotManager.Clear;
end;

procedure THistoryDialog.tbHistorySelectedClick(Sender: TObject);
begin
  if tbHistorySelected.Down
  then tbHistorySelected.ImageIndex := FEnabledImgIdx
  else tbHistorySelected.ImageIndex := FDisabledIdx;
  if FSnapshotManager <> nil
  then FSnapshotManager.HistorySelected := tbHistorySelected.Down;
end;

procedure THistoryDialog.tbPowerClick(Sender: TObject);
begin
  if tbPower.Down
  then tbPower.ImageIndex := FPowerImgIdx
  else tbPower.ImageIndex := FPowerImgIdxGrey;
  if FSnapshotManager <> nil
  then FSnapshotManager.Active := tbPower.Down;
end;

procedure THistoryDialog.SnapshotChanged(Sender: TObject);
var
  i, j: Integer;
  Item: TListItem;
begin
  if (FSnapshotManager = nil) or FInSnapshotChanged then exit;

  FInSnapshotChanged:= True;
  try
    tbHistorySelected.Enabled := FSnapshotManager.HistoryCount > 0;
    if not tbHistorySelected.Enabled
    then tbHistorySelected.Down := False
    else tbHistorySelected.Down := FSnapshotManager.HistorySelected;
    tbHistorySelected.Click;

    tbClear.Enabled := FSnapshotManager.HistoryCount > 0;
  finally
    FInSnapshotChanged := False;
  end;

  j := -1;
  lvHistory.BeginUpdate;
  try

    i := SnapshotManager.HistoryCount;
    while lvHistory.Items.Count > i do lvHistory.Items.Delete(i);
    while lvHistory.Items.Count < i do begin
      Item := lvHistory.Items.Add;
      Item.SubItems.add('');
      Item.SubItems.add('');
    end;

    if FSnapshotManager.HistoryCount = 0 then exit;

    for i := 0 to FSnapshotManager.HistoryCount - 1 do begin
      lvHistory.Items[i].Caption := '';
      if (i = FSnapshotManager.HistoryIndex) and FSnapshotManager.HistorySelected
      then begin
        lvHistory.Items[i].ImageIndex := imgCurrentLine;
        j := i;
      end
      else lvHistory.Items[i].ImageIndex := -1;

      lvHistory.Items[i].SubItems[0] := TimeToStr(FSnapshotManager.HistoryEntries[i].TimeStamp);
      lvHistory.Items[i].SubItems[1] := FSnapshotManager.HistoryEntries[i].LocationAsText;
      lvHistory.Items[i].Data := FSnapshotManager.HistoryEntries[i];
    end;

  finally
    lvHistory.EndUpdate;
  end;
  if j >= 0
  then lvHistory.Items[j].MakeVisible(False);
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

  tbClear.ImageIndex := IDEImages.LoadImage(16, 'menu_clean');
  tbPower.Hint := histdlgBtnPowerHint;
  tbHistorySelected.Hint := histdlgBtnEnableHint;
  tbClear.Hint  := histdlgBtnClearHint;

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

