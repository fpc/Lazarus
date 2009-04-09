{
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

  LCL Test 5_1 for TAsyncProcess

  Showing a form and starting via TAsyncProcess test5_1worker.
  
  Requirements:
    1. Compile LCL with TAsyncProcess support: -dUseAsyncProcess
    2. Compile test5_1worker.pas.
}
program test5_1asyncprocess;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, Math, Classes, SysUtils, Process, LCLProc, DynQueue, FileUtil,
  Forms, Controls, AsyncProcess;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure Form1Idle(Sender: TObject; var Done: Boolean);
    procedure OnAsyncReadData(Sender: TObject);
    procedure OnAsyncTerminate(Sender: TObject);
  private
    FAsyncProcessTerminated: Boolean;
    FStopExecute: Boolean;
    FTheProcess: TProcess;
    FAsyncOutput: TDynamicDataQueue;
    FUseAsyncProcess: Boolean;
  public
    constructor Create(TheOwner: TComponent); override;
    property AsyncProcessTerminated: boolean read FAsyncProcessTerminated;
    property StopExecute: Boolean read FStopExecute write FStopExecute;
    property TheProcess: TProcess read FTheProcess;
    property UseAsyncProcess: Boolean read FUseAsyncProcess write FUseAsyncProcess;
  end;

var
  Form1: TForm1;

{ TForm1 }

procedure TForm1.Form1Idle(Sender: TObject; var Done: Boolean);
const
  BufSize = 1024;
var
  i, Count, LineStart : longint;
  OutputLine, Buf : String;
  TheAsyncProcess: TAsyncProcess;
begin
  DebugLn(['TForm1.Form1Idle START']);
  if UseAsyncProcess then
    FTheProcess:=TAsyncProcess.Create(nil)
  else
    FTheProcess:=TProcess.Create(nil);
  TheProcess.CommandLine:=AppendPathDelim(GetCurrentDir)+'test5_1worker';
  if not FileExists(TheProcess.CommandLine) then begin
    DebugLn(['TForm1.Form1Idle File not found: ',TheProcess.CommandLine]);
    exit;
  end;
  TheProcess.Options:= [poUsePipes,poStdErrToOutPut];
  TheProcess.ShowWindow := swoHide;

  SetLength(Buf,BufSize);

  OutputLine:='';

  if TheProcess is TAsyncProcess then begin
    TheAsyncProcess:=TAsyncProcess(TheProcess);
    TheAsyncProcess.OnReadData:=@OnAsyncReadData;
    TheAsyncProcess.OnTerminate:=@OnAsyncTerminate;
    FAsyncOutput:=TDynamicDataQueue.Create;
  end else
    TheAsyncProcess:=nil;

  TheProcess.Execute;
  DebugLn(['TForm1.Form1Idle start looping ...']);
  repeat
    Application.ProcessMessages;
    DebugLn(['TForm1.Form1Idle looping ...']);
    if StopExecute then begin
      DebugLn(['TForm1.Form1Idle Aborting...']);
      TheProcess.Terminate(0);
      DebugLn(['TForm1.Form1Idle Aborted']);
      break;
    end;

    Count:=0;
    if (TheAsyncProcess<>nil) then begin
      // using non blocking TAsyncProcess
      Count:=FAsyncOutput.Size;
      DebugLn(['TForm1.Form1Idle Count=',Count]);
      if (Count=0) and AsyncProcessTerminated then break;
      if Count>0 then
        Count:=FAsyncOutput.Pop(Buf[1],Min(Count,length(Buf)))
      else
        Sleep(100);
    end;
    if (TheAsyncProcess=nil) and (TheProcess.Output<>nil) then begin
      // using a blocking TProcess
      DebugLn(['TForm1.Form1Idle reading ...']);
      Count:=TheProcess.Output.Read(Buf[1],length(Buf));
      DebugLn(['TForm1.Form1Idle read ',Count]);
      if Count=0 then begin
        // no output on blocking means, process has ended
        break;
      end;
    end;

    LineStart:=1;
    i:=1;
    while i<=Count do begin
      if Buf[i] in [#10,#13] then begin
        OutputLine:=OutputLine+copy(Buf,LineStart,i-LineStart);
        DebugLn(['TForm1.Form1Idle OutputLine="',OutputLine,'"']);
        OutputLine:='';
        if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1])
        then
          inc(i);
        LineStart:=i+1;
      end;
      inc(i);
    end;
    OutputLine:=OutputLine+copy(Buf,LineStart,Count-LineStart+1);
  until false;
  DebugLn('TForm1.Form1Idle After Loop');
  TheProcess.WaitOnExit;
  DebugLn('TForm1.Form1Idle TheProcess.ExitStatus=',dbgs(TheProcess.ExitStatus));

  TheProcess.Free;
  fTheProcess:=nil;
  FAsyncOutput.Free;
  FAsyncOutput:=nil;
end;

procedure TForm1.OnAsyncReadData(Sender: TObject);
var
  Count: LongWord;
  s: string;
begin
  Count:=TAsyncProcess(TheProcess).NumBytesAvailable;
  s:='';
  if Count>0 then begin
    FAsyncOutput.Push(TStream(TAsyncProcess(TheProcess).Output),Count);
    DebugLn(['TForm1.OnAsyncReadData Size=',FAsyncOutput.Size,' ',DbgSName(TAsyncProcess(TheProcess).Output)]);
    SetLength(s,Count);
    FAsyncOutput.Top(s[1],Count);
  end;
  DebugLn(['TForm1.OnAsyncReadData ',Count,' ',TAsyncProcess(TheProcess).NumBytesAvailable]);
  DebugLn(DbgStr(s));
  DumpStack;
end;

procedure TForm1.OnAsyncTerminate(Sender: TObject);
begin
  DebugLn(['TForm1.OnAsyncTerminate ']);
  FAsyncProcessTerminated:=true;
end;

constructor TForm1.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Application.OnIdle:=@Form1Idle;
end;

begin
  Application.Initialize;
  Application.CreateForm(TForm1,Form1);
  Form1.UseAsyncProcess:=ParamStr(1)<>'process';
  Application.Run;
end.

