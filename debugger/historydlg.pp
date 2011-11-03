unit HistoryDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Debugger, DebuggerDlg, LazarusIDEStrConsts, Forms,
  IDEWindowIntf, IDEOptionDefs, DebuggerStrConst,
  BaseDebugManager, MainBase, IDEImagesIntf, Clipbrd, Dialogs;

type

  { THistoryDialog }

  THistoryDialog = class(TDebuggerDlg)
    lvHistory: TListView;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    tbMakeSnap: TToolButton;
    ToolBar1: TToolBar;
    tbHistorySelected: TToolButton;
    tbPower: TToolButton;
    tbClear: TToolButton;
    ToolButton1: TToolButton;
    tbHist: TToolButton;
    tbSnap: TToolButton;
    tbRemove: TToolButton;
    ToolButton2: TToolButton;
    tbExport: TToolButton;
    ToolButton4: TToolButton;
    tbImport: TToolButton;
    procedure lvHistoryDblClick(Sender: TObject);
    procedure lvHistorySelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
    procedure tbClearClick(Sender: TObject);
    procedure tbHistClick(Sender: TObject);
    procedure tbHistorySelectedClick(Sender: TObject);
    procedure tbMakeSnapClick(Sender: TObject);
    procedure tbPowerClick(Sender: TObject);
    procedure tbRemoveClick(Sender: TObject);
    procedure tbExportClick(Sender: TObject);
    procedure tbImportClick(Sender: TObject);
  private
    FInSnapshotChanged: Boolean;
    imgCurrentLine: Integer;
    FPowerImgIdx, FPowerImgIdxGrey: Integer;
    FEnabledImgIdx, FDisabledIdx: Integer;
    procedure SnapshotChanged(Sender: TObject);
    procedure UpdateToolbar;
  protected
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property SnapshotManager;
  end;

implementation

{$R *.lfm}

var
  HistoryDlgWindowCreator: TIDEWindowCreator;

const
  COL_HISTORY_CUR    = 1;
  COL_HISTORY_TIME   = 2;
  COL_HISTORY_LOC    = 3;

function HistoryDlgColSizeGetter(AForm: TCustomForm; AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := AForm is THistoryDialog;
  if Result then
    Result := THistoryDialog(AForm).ColSizeGetter(AColId, ASize);
end;

procedure HistoryDlgColSizeSetter(AForm: TCustomForm; AColId: Integer; ASize: Integer);
begin
  if AForm is THistoryDialog then
    THistoryDialog(AForm).ColSizeSetter(AColId, ASize);
end;

{ THistoryDialog }

procedure THistoryDialog.lvHistoryDblClick(Sender: TObject);
begin
  if (lvHistory.Items.Count = 0) or (lvHistory.Selected = nil) then exit;
  if tbHist.Down then begin
    if (SnapshotManager.HistoryIndex = lvHistory.Selected.Index) and
       (SnapshotManager.HistorySelected)
    then begin
      SnapshotManager.HistorySelected := False;
    end
    else begin
      SnapshotManager.HistoryIndex := lvHistory.Selected.Index;
      SnapshotManager.HistorySelected := True;
    end;
  end else begin
    if (SnapshotManager.SnapshotIndex = lvHistory.Selected.Index) and
       (SnapshotManager.SnapshotSelected)
    then begin
      SnapshotManager.SnapshotSelected := False;
    end
    else begin
      SnapshotManager.SnapshotIndex := lvHistory.Selected.Index;
      SnapshotManager.SnapshotSelected := True;
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
  if SnapshotManager <> nil
  then SnapshotManager.Clear;
end;

procedure THistoryDialog.tbHistClick(Sender: TObject);
begin
  if (SnapshotManager = nil) or (FInSnapshotChanged) then exit;
  if tbHistorySelected.Down then begin
    if tbSnap.Down then SnapshotManager.SnapshotSelected := True;
    if tbHist.Down then SnapshotManager.HistorySelected := True;
  end;
  SnapshotChanged(nil);
end;

procedure THistoryDialog.tbHistorySelectedClick(Sender: TObject);
begin
  if tbHistorySelected.Down
  then tbHistorySelected.ImageIndex := FEnabledImgIdx
  else tbHistorySelected.ImageIndex := FDisabledIdx;
  if SnapshotManager = nil then exit;
  if tbHist.Down
  then SnapshotManager.HistorySelected := tbHistorySelected.Down
  else SnapshotManager.SnapshotSelected := tbHistorySelected.Down
end;

procedure THistoryDialog.tbMakeSnapClick(Sender: TObject);
begin
  if (SnapshotManager = nil) or (SnapshotManager.Current = nil) then exit;
  SnapshotManager.Current.AddToSnapshots;
end;

procedure THistoryDialog.tbPowerClick(Sender: TObject);
begin
  if tbPower.Down
  then tbPower.ImageIndex := FPowerImgIdx
  else tbPower.ImageIndex := FPowerImgIdxGrey;
  if SnapshotManager <> nil
  then SnapshotManager.Active := tbPower.Down;
end;

procedure THistoryDialog.tbRemoveClick(Sender: TObject);
begin
  if lvHistory.Selected = nil then exit;
  if tbHist.Down then begin
    SnapshotManager.History[lvHistory.Selected.Index].RemoveFromHistory;
  end else begin
    SnapshotManager.Snapshots[lvHistory.Selected.Index].RemoveFromSnapshots;
  end;
end;

procedure THistoryDialog.tbExportClick(Sender: TObject);
var
  tl: TStringList;
begin
  if (SnapshotManager = nil) then exit;
  if SaveDialog1.Execute then begin
    tl := TStringList.Create;
    tl.Text := SnapshotManager.GetAsXML;
    tl.SaveToFile(SaveDialog1.FileName);
    tl.Free;
  end;
end;

procedure THistoryDialog.tbImportClick(Sender: TObject);
var
  tl: TStringList;
begin
  if (SnapshotManager = nil) then exit;
  if OpenDialog1.Execute then begin
    tl := TStringList.Create;
    tl.LoadFromFile(OpenDialog1.FileName);
    SnapshotManager.SetFromXML(tl.Text);
    tl.Free;
  end;
end;

procedure THistoryDialog.SnapshotChanged(Sender: TObject);
var
  i, j, cur: Integer;
  Item: TListItem;
  Lst: TSnapshotList;
begin
  if (SnapshotManager = nil) or FInSnapshotChanged then exit;

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
      Lst := SnapshotManager.Snapshots;
      if SnapshotManager.SnapshotSelected
      then cur := SnapshotManager.SnapshotIndex
      else cur := -1;
    end else begin
      Lst := SnapshotManager.History;
      if SnapshotManager.HistorySelected
      then cur := SnapshotManager.HistoryIndex
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
  if SnapshotManager.Snapshots.Count > 0 then begin
    tbSnap.Enabled := True;
  end else begin
    tbSnap.Enabled := False;
    tbHist.Down := True;
  end;

  if tbSnap.Down
  then begin
    Lst := SnapshotManager.Snapshots;
    Sel := SnapshotManager.SnapshotSelected;
  end else begin
    Lst := SnapshotManager.History;
    Sel := SnapshotManager.HistorySelected;
  end;

  tbHistorySelected.Enabled := Lst.Count > 0;
  if not tbHistorySelected.Enabled
  then tbHistorySelected.Down := False
  else tbHistorySelected.Down := Sel;
  tbHistorySelected.Click;

  tbClear.Enabled := (SnapshotManager.History.Count > 0) or (SnapshotManager.Snapshots.Count > 0);

  tbMakeSnap.Enabled := (SnapshotManager.Current <> nil) and (not SnapshotManager.Current.IsSnapshot);
  tbRemove.Enabled := lvHistory.Selected <> nil;
end;

function THistoryDialog.ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := True;
  case AColId of
    COL_HISTORY_CUR:    ASize := lvHistory.Column[0].Width;
    COL_HISTORY_TIME:   ASize := lvHistory.Column[1].Width;
    COL_HISTORY_LOC:    ASize := lvHistory.Column[2].Width;
    else
      Result := False;
  end;
end;

procedure THistoryDialog.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_HISTORY_CUR:    lvHistory.Column[0].Width := ASize;
    COL_HISTORY_TIME:   lvHistory.Column[1].Width := ASize;
    COL_HISTORY_LOC:    lvHistory.Column[2].Width := ASize;
  end;
end;

constructor THistoryDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FInSnapshotChanged := False;
  Caption:= histdlgFormName;
  lvHistory.Column[0].Caption := histdlgColumnCur;
  lvHistory.Column[1].Caption := histdlgColumnTime;
  lvHistory.Column[2].Caption := histdlgColumnLoc;

  SnapshotNotification.OnChange  := @SnapshotChanged;
  SnapshotNotification.OnCurrent := @SnapshotChanged;

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

  tbImport.ImageIndex := IDEImages.LoadImage(16, 'laz_open');
  tbImport.Hint  := histdlgBtnImport;

  tbExport.ImageIndex := IDEImages.LoadImage(16, 'laz_save');
  tbExport.Hint  := histdlgBtnExport;

  tbPowerClick(nil);
  tbHistorySelectedClick(nil);
end;

initialization

  HistoryDlgWindowCreator := IDEWindowCreators.Add(NonModalIDEWindowNames[nmiHistory]);
  HistoryDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  HistoryDlgWindowCreator.OnSetDividerSize := @HistoryDlgColSizeSetter;
  HistoryDlgWindowCreator.OnGetDividerSize := @HistoryDlgColSizeGetter;
  HistoryDlgWindowCreator.DividerTemplate.Add('HistoryColCur',      COL_HISTORY_CUR,  drsHistoryColWidthCurrent);
  HistoryDlgWindowCreator.DividerTemplate.Add('HistoryColTime',     COL_HISTORY_TIME, drsHistoryColWidthTime);
  HistoryDlgWindowCreator.DividerTemplate.Add('HistoryColLocation', COL_HISTORY_LOC,  drsHistoryColWidthLocation);

end.

