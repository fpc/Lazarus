unit ThreadDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls, Debugger, DebuggerDlg, LazarusIDEStrConsts,
  BaseDebugManager, MainBase, IDEImagesIntf;

type

  { TThreadsDlg }

  TThreadsDlg = class(TDebuggerDlg)
    lvThreads: TListView;
    ToolBar1: TToolBar;
    tbCurrent: TToolButton;
    tbGoto: TToolButton;
    procedure lvThreadsDblClick(Sender: TObject);
    procedure tbCurrentClick(Sender: TObject);
    procedure ThreadsChanged(Sender: TObject);
  private
    FSnapshotManager: TSnapshotManager;
    FThreadNotification: TThreadsNotification;
    FSnapshotNotification: TSnapshotNotification;
    FThreadsMonitor: TThreadsMonitor;
    imgCurrentLine: Integer;
    procedure SetSnapshotManager(const AValue: TSnapshotManager);
    procedure SnapshotChanged(Sender: TObject);
    procedure SetThreadsMonitor(const AValue: TThreadsMonitor);
    procedure JumpToSource;
    function  GetSelectedSnapshot: TSnapshot;
    function GetSelectedThreads(Snap: TSnapshot): TThreads;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ThreadsMonitor: TThreadsMonitor read FThreadsMonitor write SetThreadsMonitor;
    property SnapshotManager: TSnapshotManager read FSnapshotManager write SetSnapshotManager;
  end; 

implementation

{$R *.lfm}

{ TThreadsDlg }

procedure TThreadsDlg.ThreadsChanged(Sender: TObject);
var
  i: Integer;
  s: String;
  Item: TListItem;
  Threads: TThreads;
  Snap: TSnapshot;
begin
  if FThreadsMonitor = nil then begin
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

  if Threads = nil then begin
    lvThreads.Clear;
    // Todo: display "no info available"
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
  then FThreadsMonitor.ChangeCurrentThread(id)
  else begin
    Threads := GetSelectedThreads(GetSelectedSnapshot);
    if Threads <> nil
    then Threads.CurrentThreadId := id;
    FThreadsMonitor.CurrentChanged;
  end;
end;

procedure TThreadsDlg.lvThreadsDblClick(Sender: TObject);
begin
  JumpToSource;
end;

procedure TThreadsDlg.SnapshotChanged(Sender: TObject);
begin
  ThreadsChanged(nil);
end;

procedure TThreadsDlg.SetSnapshotManager(const AValue: TSnapshotManager);
begin
  if FSnapshotManager = AValue then exit;
  if FSnapshotManager <> nil then FSnapshotManager.RemoveNotification(FSnapshotNotification);
  FSnapshotManager := AValue;
  if FSnapshotManager <> nil then FSnapshotManager.AddNotification(FSnapshotNotification);
  ThreadsChanged(FSnapshotManager);
end;

procedure TThreadsDlg.SetThreadsMonitor(const AValue: TThreadsMonitor);
begin
  if FThreadsMonitor = AValue then exit;
  if FThreadsMonitor <> nil then FThreadsMonitor.RemoveNotification(FThreadNotification);
  FThreadsMonitor := AValue;
  if FThreadsMonitor <> nil then FThreadsMonitor.AddNotification(FThreadNotification);
  ThreadsChanged(FThreadsMonitor);
end;

procedure TThreadsDlg.JumpToSource;
var
  Entry: TThreadEntry;
  Filename: String;
  Item: TListItem;
begin
  Item := lvThreads.Selected;
  if Item = nil then exit;
  Entry := TThreadEntry(Item.Data);
  if Entry = nil then Exit;

  // avoid any process-messages, so this proc can not be re-entered (avoid opening one files many times)
  DebugBoss.LockCommandProcessing;
  try
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
  finally
    DebugBoss.UnLockCommandProcessing;
  end;end;

function TThreadsDlg.GetSelectedSnapshot: TSnapshot;
begin
  Result := nil;
  if (SnapshotManager <> nil) and (SnapshotManager.HistorySelected)
  then Result := SnapshotManager.SelectedEntry;
end;

function TThreadsDlg.GetSelectedThreads(Snap: TSnapshot): TThreads;
begin
  if Snap = nil
  then Result := FThreadsMonitor.CurrentThreads
  else Result := FThreadsMonitor.Snapshots[Snap];
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

  FThreadNotification := TThreadsNotification.Create;
  FThreadNotification.AddReference;
  FThreadNotification.OnChange  := @ThreadsChanged;

  FSnapshotNotification := TSnapshotNotification.Create;
  FSnapshotNotification.AddReference;
  FSnapshotNotification.OnChange   := @SnapshotChanged;
  FSnapshotNotification.OnCurrent  := @SnapshotChanged;

  imgCurrentLine := IDEImages.LoadImage(16, 'debugger_current_line');
  lvThreads.SmallImages := IDEImages.Images_16;
end;

destructor TThreadsDlg.Destroy;
begin
  SetThreadsMonitor(nil);
  FThreadNotification.OnChange := nil;
  FThreadNotification.ReleaseReference;
  SetSnapshotManager(nil);
  FSnapshotNotification.OnChange := nil;
  FSnapshotNotification.OnCurrent := nil;
  FSnapshotNotification.ReleaseReference;
  inherited Destroy;
end;

end.

