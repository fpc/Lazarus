unit DebugAttachDialog;

{$mode objfpc}{$H+}
{$ifdef darwin}
  {$modeswitch ObjectiveC1}
{$endif}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ComCtrls,
  LCLType, LazFileUtils, DbgIntfDebuggerBase,
  LazarusIDEStrConsts, BaseDebugManager, Debugger;

type
  {$IFDEF darwin}
  TMyDummyObcCClass = objcclass(NSObject)
    // dummy class to get rid of FPC messages unit objcbase not used
    b: BOOL;
  end;
  {$ENDIF}

  { TDebugAttachDialogForm }

  TDebugAttachDialogForm = class(TForm)
    btnRefresh: TButton;
    btnAttach: TButton;
    btnCancel: TButton;
    labelRunningProcesses: TLabel;
    lvProcesses: TListView;
    procedure btnRefreshClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lvProcessesColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvProcessesData(Sender: TObject; Item: TListItem);
    procedure lvProcessesDblClick(Sender: TObject);
    procedure lvProcessesKeyDown(Sender: TObject; var Key: Word;
      {%H-}Shift: TShiftState);
    procedure lvProcessesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
  private
    FPidString: string;
    FList: TRunningProcessInfoList;

    // Must return chosen process id as string in PidString and mrOk as result
    // on success.
    function ChooseProcess(AList: TRunningProcessInfoList; out PidString: string): TModalResult;
  public
    FSortColumn: Integer;
    FSortBackward: Boolean;
  end;

var
  DebugAttachDialogForm: TDebugAttachDialogForm;

// Ask user for Process ID to attach to and returns it in a string form.
function GetPidForAttach: string;

implementation

{$ifdef windows}
uses
  Windows
  {$ifndef WIN9XPLATFORM}
  ,JwaTlHelp32
  {$endif};

// Enumerate running processes.
// Result must be always set: True if enumeration supported or False otherwise.
// If AList is not nil it must be filled with TRunningProcessInfo items.
function EnumerateProcesses(AList: TRunningProcessInfoList): boolean;
{$ifndef WIN9XPLATFORM}
var
  hShot: HANDLE;
  pe: tagPROCESSENTRY32W;
  item: TRunningProcessInfo;
{$endif}
begin
  {$ifdef WIN9XPLATFORM}
  Result := False;
  {$else}
  Result := True; // we can enumerate processes
  if not Assigned(AList) then
    Exit;

  hShot := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if hShot = INVALID_HANDLE_VALUE then
    Exit;

  try
    FillByte(pe{%H-}, SizeOf(pe), 0);
    pe.dwSize := SizeOf(pe);
    if Process32FirstW(hShot, pe) then
    repeat
      item := TRunningProcessInfo.Create(pe.th32ProcessID, pe.szExeFile);
      AList.Add(item);
    until not Process32NextW(hShot, pe);
  finally
    CloseHandle(hShot);
  end;
  {$endif}
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
{$ifdef darwin}
uses
  MacOSAll, CocoaAll;

function CFStringToStr(AString: CFStringRef; Encoding: CFStringEncoding = kCFStringEncodingUTF8): String;
var
  Str: Pointer;
  StrSize: CFIndex;
  StrRange: CFRange;
begin
  if AString = nil then
  begin
    Result := '';
    Exit;
  end;

  // Try the quick way first
  Str := CFStringGetCStringPtr(AString, Encoding);
  if Str <> nil then
    Result := PChar(Str)
  else
  begin
    // if that doesn't work this will
    StrRange.location := 0;
    StrRange.length := CFStringGetLength(AString);

    StrSize:=0;
    CFStringGetBytes(AString, StrRange, Encoding,
      Ord('?'), False, nil, 0, StrSize);
    SetLength(Result, StrSize);

    if StrSize > 0 then
      CFStringGetBytes(AString, StrRange, Encoding,
        Ord('?'), False, @Result[1], StrSize, StrSize);
  end;
end;

function EnumerateProcesses(AList: TRunningProcessInfoList): boolean;
var
  Arr: NSArray;
  App: NSRunningApplication;
  I: Integer;
  item: TRunningProcessInfo;
begin
  Result := True; // we can enumerate processes

  if not Assigned(AList) then
    Exit;

  // If it is not possible to get the process-list from the debugger,
  // use NSRunningApplication as fallback method. This list is not complete,
  // though. But better then nothing.
  Arr := NSWorkspace.sharedWorkspace.runningApplications;
  for I := 0 to Arr.count - 1 do
  begin
    App := NSRunningApplication(Arr.objectAtIndex(I));
    item := TRunningProcessInfo.Create(App.processIdentifier, CFStringToStr(CFStringRef(App.localizedName)));
    AList.Add(item);
  end;
end;
{$else}
function EnumerateProcesses(AList: TRunningProcessInfoList): boolean;
begin
  Result := False;
end;
{$endif}
{$endif}
{$endif}

function GetPidForAttach: string;
var
  ProcessList: TRunningProcessInfoList;
begin
  Result := '';

  ProcessList := TRunningProcessInfoList.Create(True);
  try
    // Check if we can enumerate processes.
    if not DebugBoss.FillProcessList(ProcessList) then
      if not EnumerateProcesses(ProcessList) then
      begin
        // If we can't just ask PID as string.
        InputQuery(rsAttachTo, rsEnterPID, Result);
        Exit;
      end;

    // Enumerate.
    DebugAttachDialogForm := TDebugAttachDialogForm.Create(nil);
    try
      if DebugAttachDialogForm.ChooseProcess(ProcessList, Result) <> mrOK then
        Result := '';
    finally
      FreeAndNil(DebugAttachDialogForm);
    end;

  finally
    FreeAndNil(ProcessList);
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

procedure TDebugAttachDialogForm.lvProcessesSelectItem(Sender: TObject; Item: TListItem; Selected: Boolean);
var
  info: TRunningProcessInfo;
begin
  if (Item.Index <> -1) And Selected then
  begin
    info := TRunningProcessInfo(FList.Items[Item.Index]);
    FPidString := IntToStr(info.PID);
    btnAttach.Enabled := True;
  end;
end;

function CompareListItems(Item1, Item2: Pointer): Integer;
begin
  case DebugAttachDialogForm.FSortColumn of
    0: Result := AnsiStrComp(pchar(TRunningProcessInfo(Item1).ImageName),
                             pchar(TRunningProcessInfo(Item2).ImageName));
    1: Result := integer(int64(TRunningProcessInfo(Item1).PID) -
                         int64(TRunningProcessInfo(Item2).PID));
    else Result := 0;
  end;
  if DebugAttachDialogForm.FSortBackward then
    Result := -Result;
end;

procedure TDebugAttachDialogForm.lvProcessesColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if FSortColumn = Column.Index then
    FSortBackward := not FSortBackward
  else
    FSortBackward := False;
  FSortColumn := Column.Index;

  if FSortColumn >= 0 then
    FList.Sort(@CompareListItems);

  lvProcesses.Items.Clear;
  lvProcesses.Items.Count := FList.Count;
end;

procedure TDebugAttachDialogForm.btnRefreshClick(Sender: TObject);
begin
  lvProcesses.Items.Clear;
  FSortColumn := -1;
  FList.Clear;
  if not DebugBoss.FillProcessList(FList)
  then
    EnumerateProcesses(FList);
  lvProcesses.Items.Count := FList.Count;
end;

procedure TDebugAttachDialogForm.FormCreate(Sender: TObject);
begin
  Caption:=rsAttachTo;
  labelRunningProcesses.Caption:=lisDADRunningProcesses;
  lvProcesses.Column[0].Caption:=lisDADImageName;
  lvProcesses.Column[1].Caption:=lisDADPID;
  btnRefresh.Caption:=dlgUnitDepRefresh;
  btnAttach.Caption:=lisDADAttach;
  btnCancel.Caption:=lisCancel;
end;

function TDebugAttachDialogForm.ChooseProcess(AList: TRunningProcessInfoList;
  out PidString: string): TModalResult;
begin
  FPidString := '';
  FList := AList;
  FSortColumn := -1;
  lvProcesses.Items.Count := AList.Count;
  Result := ShowModal;
  if Result = mrOK then
    PidString := FPidString;
end;

end.

