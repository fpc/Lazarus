{
 /***************************************************************************
                               AsyncProcess.pp
                               ---------------
                   Initial Revision  : Tue Dec 06 09:00:00 CET 2005


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit AsyncProcess;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, LCLProc, FileUtil, InterfaceBase, LCLIntf;

type

  { TProcessUTF8 }

  TProcessUTF8 = class(TProcess)
  private
    FApplicationNameUTF8: string;
    FCommandLineUTF8: string;
    FConsoleTitleUTF8: string;
    FCurrentDirectoryUTF8: string;
    FDesktopUTF8: string;
    FEnvironmentUTF8: TStrings;
    procedure SetApplicationNameUTF8(const AValue: string);
    procedure SetCommandLineUTF8(const AValue: string);
    procedure SetConsoleTitleUTF8(const AValue: string);
    procedure SetCurrentDirectoryUTF8(const AValue: string);
    procedure SetDesktopUTF8(const AValue: string);
    procedure SetEnvironmentUTF8(const AValue: TStrings);
    procedure UpdateEnvironment;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
    property ApplicationName: string read FApplicationNameUTF8 write SetApplicationNameUTF8;
    property CommandLine: string read FCommandLineUTF8 write SetCommandLineUTF8;
    property ConsoleTitle: string read FConsoleTitleUTF8 write SetConsoleTitleUTF8;
    property CurrentDirectory: string read FCurrentDirectoryUTF8 write SetCurrentDirectoryUTF8;
    property Desktop: string read FDesktopUTF8 write SetDesktopUTF8;
    property Environment: TStrings read FEnvironmentUTF8 write SetEnvironmentUTF8;
  end;


  { TAsyncProcess }

  TAsyncProcess = class(TProcessUTF8)
  private
    FPipeHandler: PPipeEventHandler;
    FProcessHandler: PProcessEventHandler;
    FOnReadData: TNotifyEvent;
    FOnTerminate: TNotifyEvent;
  protected
    function GetNumBytesAvailable: dword;
    procedure HandlePipeInput(AData: PtrInt; AReasons: TPipeReasons);
    procedure HandleProcessTermination(AData: PtrInt; AReason: TChildExitReason; AInfo: dword);
    procedure UnhookPipeHandle;
    procedure UnhookProcessHandle;
  public
    procedure Execute; override;
    destructor Destroy; override;
    property NumBytesAvailable: dword read GetNumBytesAvailable;
  published
    property OnReadData: TNotifyEvent read FOnReadData write FOnReadData;// You must read all the data in this event. Otherwise it is called again.
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

procedure Register;


implementation

function TAsyncProcess.GetNumBytesAvailable: dword;
begin
  if not (poUsePipes in Options) then
    Result := 0
  else
    Result := Output.NumBytesAvailable;
end;

destructor TAsyncProcess.Destroy;
begin
  UnhookProcessHandle;
  UnhookPipeHandle;
  inherited;
end;

procedure TAsyncProcess.UnhookProcessHandle;
begin
  if FProcessHandler <> nil then
    RemoveProcessEventHandler(FProcessHandler);
end;

procedure TAsyncProcess.UnhookPipeHandle;
begin
  if FPipeHandler <> nil then
    RemovePipeEventHandler(FPipeHandler);
end;

procedure TAsyncProcess.HandlePipeInput(AData: PtrInt; AReasons: TPipeReasons);
begin
  if prBroken in AReasons then
    UnhookPipeHandle;
  if prDataAvailable in AReasons then
    if FOnReadData <> nil then
      FOnReadData(Self);
end;

procedure TAsyncProcess.HandleProcessTermination(AData: PtrInt; AReason: TChildExitReason; AInfo: dword);
begin
  UnhookProcessHandle;
  UnhookPipeHandle;
  if FOnTerminate <> nil then
    FOnTerminate(Self);
end;

procedure TAsyncProcess.Execute;
begin
  inherited Execute;

  if poUsePipes in Options then
    FPipeHandler := AddPipeEventHandler(Output.Handle, @HandlePipeInput, 0);
  FProcessHandler := AddProcessEventHandler(ProcessHandle, @HandleProcessTermination, 0);
end;

{ TProcessUTF8 }

procedure TProcessUTF8.SetApplicationNameUTF8(const AValue: string);
begin
  if FApplicationNameUTF8=AValue then exit;
  FApplicationNameUTF8:=AValue;
  inherited ApplicationName:=UTF8ToSys(FApplicationNameUTF8);
end;

procedure TProcessUTF8.SetCommandLineUTF8(const AValue: string);
begin
  if FCommandLineUTF8=AValue then exit;
  FCommandLineUTF8:=AValue;
  inherited CommandLine:=UTF8ToSys(FCommandLineUTF8);
end;

procedure TProcessUTF8.SetConsoleTitleUTF8(const AValue: string);
begin
  if FConsoleTitleUTF8=AValue then exit;
  FConsoleTitleUTF8:=AValue;
  inherited ConsoleTitle:=UTF8ToSys(FConsoleTitleUTF8);
end;

procedure TProcessUTF8.SetCurrentDirectoryUTF8(const AValue: string);
begin
  if FCurrentDirectoryUTF8=AValue then exit;
  FCurrentDirectoryUTF8:=AValue;
  inherited CurrentDirectory:=UTF8ToSys(FCurrentDirectoryUTF8);
end;

procedure TProcessUTF8.SetDesktopUTF8(const AValue: string);
begin
  if FDesktopUTF8=AValue then exit;
  FDesktopUTF8:=AValue;
  inherited Desktop:=UTF8ToSys(FDesktopUTF8);
end;

procedure TProcessUTF8.SetEnvironmentUTF8(const AValue: TStrings);
begin
  if (FEnvironmentUTF8=AValue)
  or ((AValue<>nil) and FEnvironmentUTF8.Equals(AValue)) then exit;
  FEnvironmentUTF8.Assign(AValue);
end;

procedure TProcessUTF8.UpdateEnvironment;
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    for i:=0 to FEnvironmentUTF8.Count-1 do
      sl.Add(UTF8ToSys(FEnvironmentUTF8[i]));
    inherited Environment:=sl;
  finally
    sl.Free;
  end;
end;

constructor TProcessUTF8.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnvironmentUTF8:=TStringList.Create;
end;

destructor TProcessUTF8.Destroy;
begin
  FreeAndNil(FEnvironmentUTF8);
  inherited Destroy;
end;

procedure TProcessUTF8.Execute;
begin
  UpdateEnvironment;
  inherited Execute;
end;

procedure Register;
begin
  RegisterComponents('System',[TProcessUTF8,TAsyncProcess]);
end;

end.
