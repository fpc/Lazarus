unit DebugAttachDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, LCLType, Contnrs, LazarusIDEStrConsts;

type

  // Used to enumerate running processes.
  TRunningProcessInfo = class
  public
    PID: Cardinal;
    ImageName: string;
    constructor Create(APID: Cardinal; const AImageName: string);
  end;

  TRunningProcessInfoList = TObjectList;

  { TDebugAttachDialogForm }

  TDebugAttachDialogForm = class(TForm)
    btnRefresh: TButton;
    btnAttach: TButton;
    btnCancel: TButton;
    labelRunningProcesses: TLabel;
    lvProcesses: TListView;
    procedure btnRefreshClick(Sender: TObject);
    procedure lvProcessesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvProcessesData(Sender: TObject; Item: TListItem);
    procedure lvProcessesDblClick(Sender: TObject);
    procedure lvProcessesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FPidString: string;
    FList: TRunningProcessInfoList;

    // Must return chosen process id as string in PidString and mrOk as result
    // on success.
    function ChooseProcess(AList: TRunningProcessInfoList; out PidString: string): TModalResult;
  public
  end;

var
  DebugAttachDialogForm: TDebugAttachDialogForm;

// Ask user for Process ID to attach to and returns it in a string form.
function GetPidForAttach: string;

implementation

{$ifdef windows}
uses
  Windows,
  JwaTlHelp32;

// Enumerate running processes.
// Result must be always set: True if enumeration supported or False otherwise.
// If AList is not nil it must be filled with TRunningProcessInfo items.
function EnumerateProcesses(AList: TRunningProcessInfoList): boolean;
var
  hShot: HANDLE;
  pe: tagPROCESSENTRY32W;
  item: TRunningProcessInfo;
begin
  Result := True; // we can enumerate processes

  if not Assigned(AList) then
    Exit;

  hShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hShot = INVALID_HANDLE_VALUE then
    Exit;

  try
    FillByte(pe, SizeOf(pe), 0);
    pe.dwSize := SizeOf(pe);
    if Process32FirstW(hShot, pe) then
    repeat
      item := TRunningProcessInfo.Create(pe.th32ProcessID, pe.szExeFile);
      AList.Add(item);
    until not Process32NextW(hShot, pe);
  finally
    CloseHandle(hShot);
  end;
end;
{$else}
{$ifdef linux}
uses
  LazUTF8Classes;

function EnumerateProcesses(AList: TRunningProcessInfoList): boolean;

  function GetProcName(Pid: THandle): String;
  var
    S: TStream;
    Sz: Integer;
  begin
    S := TFileStreamUTF8.Create('/proc/' + IntToStr(Pid) + '/cmdline', fmOpenRead or fmShareDenyNone);
    try
      SetLength(Result, 255);
      Sz := S.Read(Result[1], 255);
      SetLength(Result, Sz);
    finally
      S.Free;
    end;
  end;

var
  Rec: TSearchRec;
  ProcName: String;
  Pid: THandle;
  Code: Integer;
  item: TRunningProcessInfo;
begin
  Result := True;

  if not Assigned(AList) then
    Exit;

  if FindFirstUTF8('/proc/*', faDirectory, Rec) = 0 then
  begin
    repeat
      Val(Rec.Name, Pid, Code);
      if (Code = 0) then
      begin
        ProcName := GetProcName(Pid);
        item := TRunningProcessInfo.Create(Pid, ProcName);
        AList.Add(item);
      end;
    until FindNextUTF8(Rec) <> 0;
  end;
  FindCloseUTF8(Rec);
end;
{$else}
function EnumerateProcesses(AList: TRunningProcessInfoList): boolean;
begin
  Result := False;
end;
{$endif}
{$endif}

{ TRunningProcessInfo }

constructor TRunningProcessInfo.Create(APID: Cardinal; const AImageName: string);
begin
  self.PID := APID;
  self.ImageName := AImageName;
end;

function GetPidForAttach: string;
var
  ProcessList: TRunningProcessInfoList;
begin
  Result := '';

  // Check if we can enumerate processes.
  if not EnumerateProcesses(nil) then
  begin
    // If we can't just ask PID as string.
    InputQuery(rsAttachTo, rsEnterPID, Result);
    Exit;
  end;

  // Enumerate.
  DebugAttachDialogForm := TDebugAttachDialogForm.Create(nil);
  try
    ProcessList := TRunningProcessInfoList.Create(True);
    try
      EnumerateProcesses(ProcessList);
      if DebugAttachDialogForm.ChooseProcess(ProcessList, Result) <> mrOK then
        Result := '';
    finally
      FreeAndNil(ProcessList);
    end;
  finally
    FreeAndNil(DebugAttachDialogForm);
  end;
end;

{$R *.lfm}

{ TDebugAttachDialogForm }

procedure TDebugAttachDialogForm.lvProcessesData(Sender: TObject;
  Item: TListItem);
var
  info: TRunningProcessInfo;
begin
  if Item.Index <> -1 then
  begin
    info := TRunningProcessInfo(FList.Items[Item.Index]);
    Item.Caption := info.ImageName;
    Item.SubItems.Add(IntToStr(info.PID));
  end;
end;

procedure TDebugAttachDialogForm.lvProcessesDblClick(Sender: TObject);
begin
  if lvProcesses.ItemIndex <> -1 then
    ModalResult := mrOK;
end;

procedure TDebugAttachDialogForm.lvProcessesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      ModalResult := mrOK;
    VK_ESCAPE:
      ModalResult := mrCancel;
  end;
end;

procedure TDebugAttachDialogForm.lvProcessesChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
var
  info: TRunningProcessInfo;
begin
  if Item.Index <> -1 then
  begin
    info := TRunningProcessInfo(FList.Items[Item.Index]);
    FPidString := IntToStr(info.PID);
    btnAttach.Enabled := True;
  end;
end;

procedure TDebugAttachDialogForm.btnRefreshClick(Sender: TObject);
begin
  lvProcesses.Items.Clear;
  FList.Clear;
  EnumerateProcesses(FList);
  lvProcesses.Items.Count := FList.Count;
end;

function TDebugAttachDialogForm.ChooseProcess(AList: TRunningProcessInfoList;
  out PidString: string): TModalResult;
begin
  FPidString := '';
  FList := AList;
  lvProcesses.Items.Count := AList.Count;
  Result := ShowModal;
  if Result = mrOK then
    PidString := FPidString;
end;

end.

