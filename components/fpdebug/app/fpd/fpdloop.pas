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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit FPDLoop;
{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, FpDbgInfo, FpDbgClasses, FpDbgDisasX86, DbgIntfBaseTypes,
  CustApp;

type

  { TFPDLoop }

  TFPDLoop = class(TCustomApplication)
  private
    FLast: string;
    procedure ShowDisas;
    procedure ShowCode;
    procedure GControllerCreateProcessEvent(var continue: boolean);
    procedure GControllerHitBreakpointEvent(var continue: boolean; const Breakpoint: TDbgBreakpoint);
  protected
    Procedure DoRun; override;
  public
    procedure Initialize; override;
  end;

implementation

uses
  FPDCommand,
  FpDbgUtil,
  FPDGlobal;

{ TFPDLoop }

procedure TFPDLoop.ShowDisas;
var
  a: TDbgPtr;
  Code, CodeBytes: String;
  CodeBin: array[0..20] of Byte;
  p: pointer;
  i: integer;
begin
  WriteLN('===');
  a := GController.CurrentProcess.GetInstructionPointerRegisterValue;
  for i := 0 to 5 do
  begin
    Write('  [', FormatAddress(a), ']');

    if not GController.CurrentProcess.ReadData(a,sizeof(CodeBin),CodeBin)
    then begin
      //Log('Disassemble: Failed to read memory at %s.', [FormatAddress(a)]);
      Code := '??';
      CodeBytes := '??';
      Inc(a);
      Exit;
    end;
    p := @CodeBin;

    Disassemble(p, GMode=dm64, CodeBytes, Code);

    WriteLN(' ', CodeBytes:20, '    ', Code);
    Inc(a, PtrUInt(p) - PtrUInt(@CodeBin));
  end;
end;

procedure TFPDLoop.ShowCode;
var
  a: TDbgPtr;
  sym, symproc: TFpDbgSymbol;
  S: TStringList;
  AName: String;
begin
  WriteLN('===');
  a := GController.CurrentProcess.GetInstructionPointerRegisterValue;
  sym := GController.CurrentProcess.FindSymbol(a);
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
end;

procedure TFPDLoop.GControllerCreateProcessEvent(var continue: boolean);
begin
  continue:=false;
end;

procedure TFPDLoop.GControllerHitBreakpointEvent(var continue: boolean; const Breakpoint: TDbgBreakpoint);
begin
  if assigned(Breakpoint) then
  begin
    ShowCode;
    ShowDisas;
    continue:=false;
  end
  else
  begin
    continue:=true;
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
  GController.OnHitBreakpointEvent:=@GControllerHitBreakpointEvent;
  GController.OnCreateProcessEvent:=@GControllerCreateProcessEvent;
end;

initialization
  CustomApplication:=TFPDLoop.Create(nil);
finalization
  CustomApplication.Free;
end.
