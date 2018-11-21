unit TestDbgControlForm;

{$mode objfpc}{$H+}

interface

uses
  Interfaces, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, ComCtrls, CheckLst, TestDbgControl, TestDbgConfig,
  TTestDbgExecuteables, VirtualTrees;

type

  { TDbgTestControlForm }

  TDbgTestControlForm = class(TForm)
    chkDbg: TTreeView;
    chkFpc: TTreeView;
    chkSym: TCheckListBox;
    chkBit: TCheckListBox;
    CheckWriteLogs: TCheckBox;
    chkTests: TTreeView;
    Edit1: TEdit;
    EditLogDir: TFileNameEdit;
    ilNodeStates: TImageList;
    Label1: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    ToolBar1: TToolBar;
    ToolBar2: TToolBar;
    ToolBar4: TToolBar;
    btnTestAll: TToolButton;
    btnTestNone: TToolButton;
    btnFpcAll: TToolButton;
    btnFpcNone: TToolButton;
    btnGdbAll: TToolButton;
    btnGdbNone: TToolButton;
    WriteLogsOnErr: TCheckBox;
    procedure btnFpcAllClick(Sender: TObject);
    procedure btnFpcNoneClick(Sender: TObject);
    procedure btnGdbAllClick(Sender: TObject);
    procedure btnGdbNoneClick(Sender: TObject);
    procedure btnTestAllClick(Sender: TObject);
    procedure btnTestNoneClick(Sender: TObject);
    procedure chkTestsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private

  public
    procedure DbgShow(Data: PtrInt);
  end;

var
  DbgTestControlForm: TDbgTestControlForm;

function CanCpuBits(b: TCpuBitType): Boolean;
function CanSymType(s: TSymbolType): Boolean;
function CanCompiler(name: string): Boolean;
function CanDebugger(name: string): Boolean;
function CanTest(id: Pointer): Boolean;
function GetTestPattern: string;

procedure SetLogPath(path: string);
function GetLogPath: string;
function GetWriteLog: TWriteLogConfig;

procedure RegisterCompiler(name: string);
procedure RegisterDebugger(name: string);
function RegisterTest(Name: String; Parent: Pointer = nil): Pointer;

implementation

type
  TTreeNodeState=(tsUnChecked, tsChecked);

function CanCpuBits(b: TCpuBitType): Boolean;
begin
  Result := DbgTestControlForm.chkBit.Checked[ord(b)];
end;

function CanSymType(s: TSymbolType): Boolean;
begin
  Result := DbgTestControlForm.chkSym.Checked[ord(s)];
end;

function CanCompiler(name: string): Boolean;
var
  i: Integer;
begin
  i := DbgTestControlForm.chkFpc.Items.Count - 1;
  while i >= 0 do begin
    if DbgTestControlForm.chkFpc.Items[i].Text = name then begin
      Result := DbgTestControlForm.chkFpc.Items[i].StateIndex = ord(tsChecked);
      exit;
    end;
    dec(i);
  end;
  Result := False;
end;

function CanDebugger(name: string): Boolean;
var
  i: Integer;
begin
  i := DbgTestControlForm.chkDbg.Items.Count - 1;
  while i >= 0 do begin
    if DbgTestControlForm.chkDbg.Items[i].Text = name then begin
      Result := DbgTestControlForm.chkDbg.Items[i].StateIndex = ord(tsChecked);
      exit;
    end;
    dec(i);
  end;
  Result := False;
end;

function CanTest(id: Pointer): Boolean;
begin
  Result := TTreeNode(id).StateIndex = ord(tsChecked);
  while Result and (TTreeNode(id).Parent <> nil) do begin
    id := TTreeNode(id).Parent;
    Result := Result and TTreeNode(id).StateIndex = ord(tsChecked);
  end;
end;

function GetTestPattern: string;
begin
  Result := DbgTestControlForm.Edit1.Text;
end;

procedure SetLogPath(path: string);
begin
  DbgTestControlForm.EditLogDir.Text := path;
end;

function GetLogPath: string;
begin
  Result := DbgTestControlForm.EditLogDir.Text;
end;

function GetWriteLog: TWriteLogConfig;
begin
  Result := wlNever;
  if DbgTestControlForm.WriteLogsOnErr.Checked then
    Result := wlOnError;
  if DbgTestControlForm.CheckWriteLogs.Checked then
    Result := wlAlways;
end;

procedure RegisterCompiler(name: string);
begin
  DbgTestControlForm.chkFpc.Items.Add(nil, Name)
  .StateIndex := ord(tsChecked);
  DbgTestControlForm.chkFpc.FullExpand;
end;

procedure RegisterDebugger(name: string);
begin
  DbgTestControlForm.chkDbg.Items.Add(nil, name)
  .StateIndex := ord(tsChecked);
  DbgTestControlForm.chkDbg.FullExpand;
end;

function RegisterTest(Name: String; Parent: Pointer): Pointer;
begin
  Result := Pointer(DbgTestControlForm.chkTests.Items.AddChild(TTreeNode(Parent), name));
  TTreeNode(Result).StateIndex := ord(tsChecked);
  if Parent <> nil then
  DbgTestControlForm.chkTests.FullExpand;
end;

{$R *.lfm}

{ TDbgTestControlForm }

procedure TDbgTestControlForm.chkTestsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  lNode: TTreeNode;
  ht: THitTests;
begin
  ht := (Sender as TTreeview).GetHitTestInfoAt(X, Y);
  if ht * [htOnStateIcon, htOnLabel] = [] then exit;

  lNode := (Sender as TTreeview).GetNodeAt(X, Y);
  if lNode <> nil then
  case lNode.StateIndex of
      0: lNode.StateIndex := 1;
      1: lNode.StateIndex := 0;
    end;
end;

procedure TDbgTestControlForm.btnFpcAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to DbgTestControlForm.chkFpc.Items.Count - 1 do
    DbgTestControlForm.chkFpc.Items[i].StateIndex := ord(tsChecked);
end;

procedure TDbgTestControlForm.btnFpcNoneClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to DbgTestControlForm.chkFpc.Items.Count - 1 do
    DbgTestControlForm.chkFpc.Items[i].StateIndex := ord(tsUnChecked);
end;

procedure TDbgTestControlForm.btnGdbAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to DbgTestControlForm.chkDbg.Items.Count - 1 do
    DbgTestControlForm.chkDbg.Items[i].StateIndex := ord(tsChecked);
end;

procedure TDbgTestControlForm.btnGdbNoneClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to DbgTestControlForm.chkDbg.Items.Count - 1 do
    DbgTestControlForm.chkDbg.Items[i].StateIndex := ord(tsUnChecked);
end;

procedure TDbgTestControlForm.btnTestAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to DbgTestControlForm.chkTests.Items.Count - 1 do
    DbgTestControlForm.chkTests.Items[i].StateIndex := ord(tsChecked);
end;

procedure TDbgTestControlForm.btnTestNoneClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to DbgTestControlForm.chkTests.Items.Count - 1 do
    DbgTestControlForm.chkTests.Items[i].StateIndex := ord(tsUnChecked);
end;

procedure TDbgTestControlForm.DbgShow(Data: PtrInt);
var
  s: TSymbolType;
  b: TCpuBitType;
  t: String;
begin
  for s := low(TSymbolType) to high(TSymbolType) do begin
    WriteStr(t, s);
    chkSym.AddItem(t, nil);
  end;
  chkSym.CheckAll(cbChecked);

  for b := low(TCpuBitType) to high(TCpuBitType) do begin
    WriteStr(t, b);
    chkBit.AddItem(t, nil);
  end;
  chkBit.CheckAll(cbChecked);

  Show;
end;

initialization
  DbgTestControlForm := TDbgTestControlForm.Create(Application);
  Application.QueueAsyncCall(@DbgTestControlForm.DbgShow, 0);

  CanCpuBitsProc := @CanCpuBits;
  CanSymTypeProc := @CanSymType;
  CanCompilerProc := @CanCompiler;
  CanDebuggerProc := @CanDebugger;
  CanTestProc := @CanTest;
  GetTestPatternProc := @GetTestPattern;
  SetLogPathProc := @SetLogPath;
  GetLogPathProc := @GetLogPath;
  GetWriteLogProc := @GetWriteLog;
  RegisterCompilerProc := @RegisterCompiler;
  RegisterDebuggerProc := @RegisterDebugger;
  RegisterTestProc := @RegisterTest;

end.

