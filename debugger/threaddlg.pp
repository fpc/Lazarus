unit ThreadDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, LCLProc,
  Debugger, DebuggerDlg, Forms, LazarusIDEStrConsts, IDEWindowIntf, DebuggerStrConst,
  BaseDebugManager, IDEImagesIntf;

type

  { TThreadsDlg }

  TThreadsDlg = class(TDebuggerDlg)
    lvThreads: TListView;
    ToolBar1: TToolBar;
    tbCurrent: TToolButton;
    tbGoto: TToolButton;
    procedure lvThreadsDblClick(Sender: TObject);
    procedure tbCurrentClick(Sender: TObject);
  private
    imgCurrentLine: Integer;
    FUpdateFlags: set of (ufThreadChanged);
    procedure JumpToSource;
    function  GetSelectedSnapshot: TSnapshot;
    function GetSelectedThreads(Snap: TSnapshot): TThreads;
  protected
    procedure DoEndUpdate; override;
    procedure ThreadsChanged(Sender: TObject);
    function  ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
    procedure ColSizeSetter(AColId: Integer; ASize: Integer);
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    property ThreadsMonitor;
    property SnapshotManager;
  end;

implementation

{$R *.lfm}

var
  ThreadDlgWindowCreator: TIDEWindowCreator;

const
  COL_THREAD_BRKPOINT = 1;
  COL_THREAD_INDEX    = 2;
  COL_THREAD_NAME     = 3;
  COL_THREAD_STATE    = 4;
  COL_THREAD_SOURCE   = 5;
  COL_THREAD_LINE     = 6;
  COL_THREAD_FUNC     = 7;

function ThreadsDlgColSizeGetter(AForm: TCustomForm; AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := AForm is TThreadsDlg;
  if Result then
    Result := TThreadsDlg(AForm).ColSizeGetter(AColId, ASize);
end;

procedure ThreadsDlgColSizeSetter(AForm: TCustomForm; AColId: Integer; ASize: Integer);
begin
  if AForm is TThreadsDlg then
    TThreadsDlg(AForm).ColSizeSetter(AColId, ASize);
end;

{ TThreadsDlg }

procedure TThreadsDlg.ThreadsChanged(Sender: TObject);
var
  i: Integer;
  s: String;
  Item: TListItem;
  Threads: TThreads;
  Snap: TSnapshot;
begin
  if IsUpdating then begin
    {$IFDEF DBG_DATA_MONITORS} DebugLn(['DebugDataWindow: TThreadsDlg.ThreadsChanged from ',  DbgSName(Sender), ' in IsUpdating']); {$ENDIF}

    Include(FUpdateFlags, ufThreadChanged);
    exit;
  end;
  {$IFDEF DBG_DATA_MONITORS} try DebugLnEnter(['DebugDataMonitor: >>ENTER: TThreadsDlg.ThreadsChanged from ',  DbgSName(Sender)]); {$ENDIF}
  Exclude(FUpdateFlags, ufThreadChanged);

  BeginUpdate;
  lvThreads.BeginUpdate;
  try
    if ThreadsMonitor = nil then begin
      lvThreads.Clear;
      exit;
    end;

    Snap := GetSelectedSnapshot;
    Threads := GetSelectedThreads(Snap);
    if (Snap <> nil)
    then begin
      Caption:= lisThreads + ' ('+ Snap.LocationAsText +')';
    end
    else begin
      Caption:= lisThreads;
    end;

    if (Threads = nil) or ((Snap <> nil) and (Threads.Count=0)) then begin
      lvThreads.Clear;
      Item := lvThreads.Items.Add;
      Item.SubItems.add('');
      Item.SubItems.add('');
      Item.SubItems.add('');
      Item.SubItems.add(lisThreadsNotEvaluated);
      Item.SubItems.add('');
      Item.SubItems.add('');
      exit;
    end;

    i := Threads.Count;
    while lvThreads.Items.Count > i do lvThreads.Items.Delete(i);
    while lvThreads.Items.Count < i do begin
      Item := lvThreads.Items.Add;
      Item.SubItems.add('');
      Item.SubItems.add('');
      Item.SubItems.add('');
      Item.SubItems.add('');
      Item.SubItems.add('');
      Item.SubItems.add('');
    end;

    for i := 0 to Threads.Count - 1 do begin
      lvThreads.Items[i].Caption := '';
      if Threads[i].ThreadId = Threads.CurrentThreadId
      then lvThreads.Items[i].ImageIndex := imgCurrentLine
      else lvThreads.Items[i].ImageIndex := -1;
      lvThreads.Items[i].SubItems[0] := IntToStr(Threads[i].ThreadId);
      lvThreads.Items[i].SubItems[1] := Threads[i].ThreadName;
      lvThreads.Items[i].SubItems[2] := Threads[i].ThreadState;
      s := Threads[i].Source;
      if s = '' then s := ':' + IntToHex(Threads[i].Address, 8);
      lvThreads.Items[i].SubItems[3] := s;
      lvThreads.Items[i].SubItems[4] := IntToStr(Threads[i].Line);
      lvThreads.Items[i].SubItems[5] := Threads[i].GetFunctionWithArg;
      lvThreads.Items[i].Data := Threads[i];
    end;
  finally
    lvThreads.EndUpdate;
    EndUpdate;
  end;
  {$IFDEF DBG_DATA_MONITORS} finally DebugLnExit(['DebugDataMonitor: <<EXIT: TThreadsDlg.ThreadsChanged']); end; {$ENDIF}
end;

function TThreadsDlg.ColSizeGetter(AColId: Integer; var ASize: Integer): Boolean;
begin
  Result := True;
  case AColId of
    COL_THREAD_BRKPOINT: ASize := lvThreads.Column[0].Width;
    COL_THREAD_INDEX:    ASize := lvThreads.Column[1].Width;
    COL_THREAD_NAME:     ASize := lvThreads.Column[2].Width;
    COL_THREAD_STATE:    ASize := lvThreads.Column[3].Width;
    COL_THREAD_SOURCE:   ASize := lvThreads.Column[4].Width;
    COL_THREAD_LINE:     ASize := lvThreads.Column[5].Width;
    COL_THREAD_FUNC:     ASize := lvThreads.Column[6].Width;
    else
      Result := False;
  end;
end;

procedure TThreadsDlg.ColSizeSetter(AColId: Integer; ASize: Integer);
begin
  case AColId of
    COL_THREAD_BRKPOINT: lvThreads.Column[0].Width := ASize;
    COL_THREAD_INDEX:    lvThreads.Column[1].Width := ASize;
    COL_THREAD_NAME:     lvThreads.Column[2].Width := ASize;
    COL_THREAD_STATE:    lvThreads.Column[3].Width := ASize;
    COL_THREAD_SOURCE:   lvThreads.Column[4].Width := ASize;
    COL_THREAD_LINE:     lvThreads.Column[5].Width := ASize;
    COL_THREAD_FUNC:     lvThreads.Column[6].Width := ASize;
  end;
end;

procedure TThreadsDlg.tbCurrentClick(Sender: TObject);
var
  Item: TListItem;
  id: LongInt;
  Threads: TThreads;
begin
  Item := lvThreads.Selected;
  if Item = nil then exit;
  id := StrToIntDef(Item.SubItems[0], -1);
  if id < 0 then exit;
  if GetSelectedSnapshot = nil
  then ThreadsMonitor.ChangeCurrentThread(id)
  else begin
    Threads := GetSelectedThreads(GetSelectedSnapshot);
    if Threads <> nil
    then Threads.CurrentThreadId := id;
    ThreadsMonitor.CurrentChanged;
  end;
end;

procedure TThreadsDlg.lvThreadsDblClick(Sender: TObject);
begin
  JumpToSource;
end;

procedure TThreadsDlg.JumpToSource;
var
  Entry: TThreadEntry;
  Item: TListItem;
begin
  Item := lvThreads.Selected;
  if Item = nil then exit;
  Entry := TThreadEntry(Item.Data);
  if Entry = nil then Exit;

  JumpToUnitSource(Entry.UnitInfo, Entry.Line);
end;

function TThreadsDlg.GetSelectedSnapshot: TSnapshot;
begin
  Result := nil;
  if (SnapshotManager <> nil) and (SnapshotManager.SelectedEntry <> nil)
  then Result := SnapshotManager.SelectedEntry;
end;

function TThreadsDlg.GetSelectedThreads(Snap: TSnapshot): TThreads;
begin
  if Snap = nil
  then Result := ThreadsMonitor.CurrentThreads
  else Result := ThreadsMonitor.Snapshots[Snap];
end;

procedure TThreadsDlg.DoEndUpdate;
begin
  if ufThreadChanged in FUpdateFlags then ThreadsChanged(nil);
end;

constructor TThreadsDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Caption:= lisThreads;
  lvThreads.Column[1].Caption := lisThreadsID;
  lvThreads.Column[2].Caption := lisThreadsName;
  lvThreads.Column[3].Caption := lisThreadsState;
  lvThreads.Column[4].Caption := lisThreadsSrc;
  lvThreads.Column[5].Caption := lisThreadsLine;
  lvThreads.Column[6].Caption := lisThreadsFunc;
  tbCurrent.Caption := lisThreadsCurrent;
  tbGoto.Caption := lisThreadsGoto;

  SnapshotNotification.OnCurrent := @ThreadsChanged;
  ThreadsNotification.OnChange   := @ThreadsChanged;

  imgCurrentLine := IDEImages.LoadImage(16, 'debugger_current_line');
  lvThreads.SmallImages := IDEImages.Images_16;
end;

initialization

  ThreadDlgWindowCreator := IDEWindowCreators.Add(DebugDialogNames[ddtThreads]);
  ThreadDlgWindowCreator.OnCreateFormProc := @CreateDebugDialog;
  ThreadDlgWindowCreator.OnSetDividerSize := @ThreadsDlgColSizeSetter;
  ThreadDlgWindowCreator.OnGetDividerSize := @ThreadsDlgColSizeGetter;
  ThreadDlgWindowCreator.DividerTemplate.Add('ColumnThreadBrkPoint', COL_THREAD_BRKPOINT,  drsColWidthBrkPointImg);
  ThreadDlgWindowCreator.DividerTemplate.Add('ColumnThreadIndex',    COL_THREAD_INDEX,     drsColWidthIndex);
  ThreadDlgWindowCreator.DividerTemplate.Add('ColumnThreadName',     COL_THREAD_NAME,      drsColWidthName);
  ThreadDlgWindowCreator.DividerTemplate.Add('ColumnThreadState',    COL_THREAD_STATE,     drsColWidthState);
  ThreadDlgWindowCreator.DividerTemplate.Add('ColumnThreadSource',   COL_THREAD_SOURCE,    drsColWidthSource);
  ThreadDlgWindowCreator.DividerTemplate.Add('ColumnThreadLine',     COL_THREAD_LINE,      drsColWidthLine);
  ThreadDlgWindowCreator.DividerTemplate.Add('ColumnThreadFunc',     COL_THREAD_FUNC,      drsColWidthFunc);
  ThreadDlgWindowCreator.CreateSimpleLayout;

end.

