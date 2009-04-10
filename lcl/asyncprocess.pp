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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
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
  Classes, SysUtils, Process, LCLProc, FileUtil, InterfaceBase, LCLIntf, UTF8Process;

type

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

procedure Register;
begin
  RegisterComponents('System',[TAsyncProcess]);
end;

end.
