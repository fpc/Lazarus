{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpdloop.pas  -  FP standalone debugger - Debugger main loop
 ---------------------------------------------------------------------------

 This unit contains the main loop of the debugger. It waits for a debug
 event and handles it

 ---------------------------------------------------------------------------

 @created(Mon Apr 10th WET 2006)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.nl>)

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************
}
unit FPDLoop;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, LazFileUtils, LazUTF8, FpDbgInfo, FpDbgClasses,
  FpDbgDisasX86, DbgIntfBaseTypes, FpDbgDwarf, FpdMemoryTools, CustApp;

type

  { TFPDLoop }

  TFPDLoop = class(TCustomApplication)
  private
    FLast: string;
    FMemReader: TDbgMemReader;
    FMemManager: TFpDbgMemManager;
    FMemConvertor: TFpDbgMemConvertor;
    procedure ShowDisas;
    procedure ShowCode;
    procedure GControllerExceptionEvent(var continue: boolean; const ExceptionClass, ExceptionMessage: string);
    procedure GControllerCreateProcessEvent(var continue: boolean);
    procedure GControllerHitBreakpointEvent(var continue: boolean; const Breakpoint: TFpDbgBreakpoint;
      AnEventType: TFPDEvent; AMoreHitEventsPending: Boolean);
    procedure GControllerProcessExitEvent(ExitCode: DWord);
    procedure GControllerDebugInfoLoaded(Sender: TObject);
  protected
    Procedure DoRun; override;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  end;

implementation

uses
  FPDCommand,
  FpDbgUtil,
  FPDGlobal;

type

  { TPDDbgMemReader }

  TPDDbgMemReader = class(TDbgMemReader)
  protected
    function GetDbgProcess: TDbgProcess; override;
  end;


resourcestring
  sBreakpointReached = 'Breakpoint reached at %s.';
  sProcessPaused = 'Process paused.';
  sProcessExited = 'Process ended with exit-code %d.';

{ TPDDbgMemReader }

function TPDDbgMemReader.GetDbgProcess: TDbgProcess;
begin
  result := GController.CurrentProcess;
end;

{ TFPDLoop }

procedure TFPDLoop.GControllerExceptionEvent(var continue: boolean; const ExceptionClass, ExceptionMessage: string);
begin
  if not continue then
  begin
    ShowCode;
    ShowDisas;
  end;
  if ExceptionMessage<>'' then
  begin
    writeln('Program raised exception class '''+ExceptionClass+'''. Exception message:');
    writeln(ExceptionMessage);
  end
  else
    writeln('Program raised exception class '''+ExceptionClass+'''.');
end;

procedure TFPDLoop.GControllerProcessExitEvent(ExitCode: DWord);
begin
  writeln(format(sProcessExited,[ExitCode]));
end;

procedure TFPDLoop.GControllerDebugInfoLoaded(Sender: TObject);
begin
  TFpDwarfInfo(GController.CurrentProcess.DbgInfo).MemManager := FMemManager;
end;

procedure TFPDLoop.ShowDisas;
var
  a: TDbgPtr;
  Code, CodeBytes: String;
  CodeBin: array[0..20] of Byte;
  p: pointer;
  i: integer;
begin
  WriteLN('===');
  a := GController.CurrentThread.GetInstructionPointerRegisterValue;
  for i := 0 to 5 do
  begin
    Write('  [', FormatAddress(a), ']');

    if not GController.CurrentProcess.ReadData(a,sizeof(CodeBin),CodeBin)
    then begin
      //debugln('Disassemble: Failed to read memory at %s.', [FormatAddress(a)]);
      Code := '??';
      CodeBytes := '??';
      Inc(a);
      Exit;
    end;
    p := @CodeBin;

    GController.CurrentProcess.Disassembler
      .Disassemble(p, CodeBytes, Code);

    WriteLN(' ', CodeBytes:20, '    ', Code);
    Inc(a, PtrUInt(p) - PtrUInt(@CodeBin));
  end;
end;

procedure TFPDLoop.ShowCode;
var
  a: TDbgPtr;
  sym, symproc: TFpSymbol;
  S: TStringList;
  AName: String;
begin
  WriteLN('===');
  a := GController.CurrentThread.GetInstructionPointerRegisterValue;
  sym := GController.CurrentProcess.FindProcSymbol(a);
  if sym = nil
  then begin
    WriteLn('  [', FormatAddress(a), '] ???');
    Exit;
  end;

  symproc := sym;
  while not (symproc.kind in [skProcedure, skFunction]) do
    symproc := symproc.Parent;

  if sym <> symproc
  then begin
    if symproc = nil
    then WriteLn('???')
    else begin
      WriteLn(symproc.FileName, ' ', symproc.Line, ':', symproc.Column, ' ', symproc.Name);
    end;
    Write(' ');
  end;

  WriteLn(sym.FileName, ' ', sym.Line, ':', sym.Column, ' ', sym.Name);
  Write('  [', FormatAddress(sym.Address), '+', a-sym.Address.Address, '] ');

  AName := sym.Filename;
  if not FileExistsUTF8(AName)
  then begin
    if ExtractFilePath(AName) = ''
    then begin
      AName := IncludeTrailingPathDelimiter(ExtractFilePath(GController.ExecutableFilename)) + AName;
      if not FileExistsUTF8(AName)
      then AName := '';
    end
    else AName := '';
  end;

  if AName = ''
  then begin
    WriteLn(' File not found');
    Exit;
  end;

  S := TStringList.Create;
  try
    S.LoadFromFile(UTF8ToSys(AName));
    if S.Count < sym.Line
    then WriteLn('Line not found')
    else WriteLn(S[sym.Line - 1]);
  except
    on E: Exception do WriteLn(E.Message);
  end;
  S.Free;

  sym.ReleaseReference;
end;

procedure TFPDLoop.GControllerCreateProcessEvent(var continue: boolean);
begin
  continue:=false;
end;

procedure TFPDLoop.GControllerHitBreakpointEvent(var continue: boolean; const Breakpoint: TFpDbgBreakpoint;
  AnEventType: TFPDEvent; AMoreHitEventsPending: Boolean);
begin
  if assigned(Breakpoint) then
    writeln(Format(sBreakpointReached, ['' {FormatAddress(Breakpoint.Location)}]))
  else
    writeln(sProcessPaused);
  if not continue then
  begin
    ShowCode;
    ShowDisas;
  end;
end;

procedure TFPDLoop.DoRun;
var
  S: String;
  b: boolean;
begin
  Write('FPD>');
  ReadLn(S);
  if S <> ''
  then FLast := S;
  if FLast <> '' then
    begin
    HandleCommand(FLast, b);
    while b do
      begin
      GController.ProcessLoop;
      GController.SendEvents(b);
      end;
    end;
end;

procedure TFPDLoop.Initialize;
begin
  inherited Initialize;
  FMemReader := TPDDbgMemReader.Create;
  FMemConvertor := TFpDbgMemConvertorLittleEndian.Create;
  FMemManager := TFpDbgMemManager.Create(FMemReader, FMemConvertor);
  //TODO: Maybe DebugLogger.OnLog ....
  //GController.OnLog:=@OnLog;
  GController.OnHitBreakpointEvent:=@GControllerHitBreakpointEvent;
  GController.OnCreateProcessEvent:=@GControllerCreateProcessEvent;
  GController.OnExceptionEvent:=@GControllerExceptionEvent;
  GController.OnProcessExitEvent:=@GControllerProcessExitEvent;
  GController.OnDebugInfoLoaded:=@GControllerDebugInfoLoaded;
end;

destructor TFPDLoop.Destroy;
begin
  FMemManager.Free;
  FMemReader.Free;
  FMemConvertor.Free;
  inherited Destroy;
end;

initialization
  CustomApplication:=TFPDLoop.Create(nil);
finalization
  CustomApplication.Free;
end.
