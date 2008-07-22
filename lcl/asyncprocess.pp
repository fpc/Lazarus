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

{$IF defined(VER2_0_2) and defined(win32)}
// FPC <= 2.0.2 compatibility code
// WINDOWS define was added after FPC 2.0.2
  {$define WINDOWS}
{$endif}

interface

uses
  Classes, Process, LCLProc, InterfaceBase, LCLIntf;

type

  { TAsyncProcess }

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
    property NumBytesAvailable: dword read GetNumBytesAvailable;
  published
    property OnReadData: TNotifyEvent read FOnReadData write FOnReadData;// You must read all the data in this event. Otherwise it is called again.
    property OnTerminate: TNotifyEvent read FOnTerminate write FOnTerminate;
  end;

implementation

{$ifdef WINDOWS}

uses Windows;

function TAsyncProcess.GetNumBytesAvailable: dword;
begin
{$ifdef wince}
  // Windows CE doesn´t have the API function PeekNamedPipe
  Result := 0;
{$else}
  if not (poUsePipes in Options) then
    Result := 0
  else
  if not PeekNamedPipe(Output.Handle, nil, 0, nil, @Result, nil) then
    Result := 0;
{$endif}
end;

{$else below for not Windows}

uses BaseUnix, TermIO;

function TAsyncProcess.GetNumBytesAvailable: dword;
begin
  if not (poUsePipes in Options) then
    Result := 0
  else begin
    // FIONREAD -> bytes available for reading without blocking
    // FIONSPACE -> bytes available for writing without blocking
    //   does not work on all platforms (not defined on linux e.g.)
    if fpioctl(Output.Handle, FIONREAD, @Result)<0 then
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

end.
