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
 *  See the file COPYING.LCL, included in this distribution,                 *
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
  Classes, Process, InterfaceBase, LCLIntf;

type
  TAsyncProcess = class(TProcess)
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
  published
    property NumBytesAvailable: dword read GetNumBytesAvailable;
    property OnReadData: TNotifyEvent read FOnReadData write FOnReadData;
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

implementation

{$ifdef WIN32}

uses Windows;

function TAsyncProcess.GetNumBytesAvailable: dword;
begin
  if not (poUsePipes in Options) then
    Result := 0
  else
  if not PeekNamedPipe(Output.Handle, nil, 0, nil, @Result, nil) then
    Result := 0;
end;

{$else}

uses BaseUnix, TermIO;

{$ifdef BSD}
const
  FIONREAD = $4004667;
{$endif}

function TAsyncProcess.GetNumBytesAvailable: dword;
begin
  if not (poUsePipes in Options) then
    Result := 0
  else begin
    // FIONREAD -> bytes available for reading without blocking
    // FIONSPACE -> bytes available for writing without blocking
    //   does not work on all platforms (not defined on linux e.g.)
    if fpioctl(Output.Handle, FIONREAD, @Result) = -1 then
      Result := 0;
  end;
end;

{$endif}

destructor TAsyncProcess.Destroy;
begin
  UnhookProcessHandle;
  UnhookPipeHandle;
  inherited;
end;

procedure TAsyncProcess.UnhookProcessHandle;
begin
  if FProcessHandler <> nil then
  begin
    RemoveProcessEventHandler(FProcessHandler);
    FProcessHandler := nil;
  end;
end;

procedure TAsyncProcess.UnhookPipeHandle;
begin
  if FPipeHandler <> nil then
  begin
    RemovePipeEventHandler(FPipeHandler);
    FPipeHandler := nil;
  end;
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
  inherited;

  if poUsePipes in Options then
    FPipeHandler := AddPipeEventHandler(Output.Handle, @HandlePipeInput, 0);
  FProcessHandler := AddProcessEventHandler(ProcessHandle, @HandleProcessTermination, 0);
end;

end.

