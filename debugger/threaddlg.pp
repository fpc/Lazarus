unit ThreadDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Debugger, DebuggerDlg, LazarusIDEStrConsts, BaseDebugManager, MainBase;

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
    { private declarations }
    FThreadNotification: TIDEThreadsNotification;
    FThreadsMonitor: TThreadsMonitor;
    procedure SetThreadsMonitor(const AValue: TThreadsMonitor);
    procedure JumpToSource;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ThreadsMonitor: TThreadsMonitor read FThreadsMonitor write SetThreadsMonitor;
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
begin
  if FThreadsMonitor = nil then begin
    lvThreads.Clear;
    exit;
  end;

  Threads := FThreadsMonitor.CurrentThreads;
  lvThreads.Items.Count := Threads.Count;

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
    if Threads[i].ThreadId = Threads.CurrentThreadId
    then lvThreads.Items[i].Caption := '*'
    else lvThreads.Items[i].Caption := '';
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
begin
  Item := lvThreads.Selected;
  if Item = nil then exit;
  id := StrToIntDef(Item.SubItems[0], -1);
  if id < 0 then exit;
  FThreadsMonitor.ChangeCurrentThread(id);
end;

procedure TThreadsDlg.lvThreadsDblClick(Sender: TObject);
begin
  JumpToSource;
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

  FThreadNotification := TIDEThreadsNotification.Create;
  FThreadNotification.AddReference;
  FThreadNotification.OnChange  := @ThreadsChanged;
end;

destructor TThreadsDlg.Destroy;
begin
  SetThreadsMonitor(nil);
  FThreadNotification.OnChange := nil;
  FThreadNotification.ReleaseReference;
  inherited Destroy;
end;

end.

