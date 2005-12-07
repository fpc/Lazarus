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

{$IFDEF Linux}
  {$DEFINE UseLinuxThreading}
{$ENDIF}

interface

uses
  Classes, Process, InterfaceBase, LCLIntf;

type
  TAsyncProcess = class(TProcess)
  private
    FOnReadData: TNotifyEvent;
    FOnTerminate: TNotifyEvent;
  protected
    function GetNumBytesAvailable: dword;
    procedure HandlePipeInput(AData: PtrInt; AReasons: TPipeReasons);
    procedure HandleProcessTermination(AData: PtrInt; AReason: TChildExitReason; AInfo: dword);
  public
    procedure Execute; override;
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

function TAsyncProcess.GetNumBytesAvailable: dword;
begin
  if not (poUsePipes in Options) then
    Result := 0
  else begin
    // FIONREAD -> bytes available for reading without blocking
    // FIONSPACE -> bytes available for writing without blocking
    //   does not work on all platforms (not defined on linux e.g.)
    {$ifdef UseLinuxThreading}
    if fpioctl(Output.Handle, $4004667f, @Result) = -1 then
    {$ENDIF}
      Result := 0;
  end;
end;

{$endif}

procedure TAsyncProcess.HandlePipeInput(AData: PtrInt; AReasons: TPipeReasons);
begin
  if prBroken in AReasons then
    RemovePipeEventHandler(Output.Handle);
  if prDataAvailable in AReasons then
    if FOnReadData <> nil then
      FOnReadData(Self);
end;

procedure TAsyncProcess.HandleProcessTermination(AData: PtrInt; AReason: TChildExitReason; AInfo: dword);
begin
  RemoveProcessEventHandler(ProcessHandle);
  RemovePipeEventHandler(Output.Handle);
  if FOnTerminate <> nil then
    FOnTerminate(Self);
end;

procedure TAsyncProcess.Execute;
begin
  inherited;

  if poUsePipes in Options then
    AddPipeEventHandler(Output.Handle, @HandlePipeInput, 0);
  AddProcessEventHandler(ProcessHandle, @HandleProcessTermination, 0);
end;

end.

