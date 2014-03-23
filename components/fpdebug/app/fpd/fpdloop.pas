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
  Classes, SysUtils, FileUtil, FpDbgInfo, FpDbgClasses, FpDbgDisasX86, DbgIntfBaseTypes;

procedure DebugLoop;


implementation

uses
  FPDGlobal, FpDbgUtil, FpdMemoryTools;

procedure DebugLoop;

  procedure ShowDisas;
  var
    a: TDbgPtr;
    Code, CodeBytes: String;
    CodeBin: array[0..20] of Byte;
    p: pointer;
    i: integer;
  begin
    WriteLN('===');
    a := GCurrentProcess.GetInstructionPointerRegisterValue;
    for i := 0 to 5 do
    begin
      Write('  [', FormatAddress(a), ']');

      if not GCurrentProcess.ReadData(a,sizeof(CodeBin),CodeBin)
      then begin
        Log('Disassemble: Failed to read memory at %s.', [FormatAddress(a)]);
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
  
  procedure ShowCode;
  var
    a: TDbgPtr;
    sym, symproc: TFpDbgSymbol;
    S: TStringList;
    Name: String;
  begin
    WriteLN('===');
    a := GCurrentProcess.GetInstructionPointerRegisterValue;
    sym := GCurrentProcess.FindSymbol(a);
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
    Write('  [', FormatAddress(LocToAddrOrNil(sym.Address)), '+', a-LocToAddrOrNil(sym.Address), '] ');

    Name := sym.Filename;
    if not FileExistsUTF8(Name)
    then begin
      if ExtractFilePath(Name) = ''
      then begin
        Name := IncludeTrailingPathDelimiter(ExtractFilePath(GFileName)) + Name;
        if not FileExistsUTF8(Name)
        then Name := '';
      end
      else Name := '';
    end;
    
    if Name = ''
    then begin
      WriteLn(' File not found');
      Exit;
    end;

    S := TStringList.Create;
    try
      S.LoadFromFile(UTF8ToSys(Name));
      if S.Count < sym.Line
      then WriteLn('Line not found')
      else WriteLn(S[sym.Line - 1]);
    except
      on E: Exception do WriteLn(E.Message);
    end;
    S.Free;
  end;

var
  AFirstLoop: boolean;
  AProcessIdentifier: THandle;
  AThreadIdentifier: THandle;
  ALib: TDbgLibrary;

begin
  repeat
    if (GState in [dsStop, dsPause, dsEvent])
    then begin
      GCurrentProcess.Continue(GCurrentProcess, GCurrentThread, GState);
      GState := dsRun;
    end;

    if not GCurrentProcess.WaitForDebugEvent(AProcessIdentifier, AThreadIdentifier) then Continue;

    if assigned(GCurrentProcess) and not assigned(GMainProcess) then
      begin
      GMainProcess:=GCurrentProcess;
      AFirstLoop:=true;
      end
    else
      AFirstLoop:=false;
    GCurrentProcess := nil;
    GCurrentThread := nil;
    if not GetProcess(AProcessIdentifier, GCurrentProcess) and not AFirstLoop then Continue;

    if AFirstLoop then
      GCurrentProcess := GMainProcess;

    if not GCurrentProcess.GetThread(AThreadIdentifier, GCurrentThread)
    then WriteLN('LOOP: Unable to retrieve current thread');

    GState := dsEvent;
    begin
      case GCurrentProcess.ResolveDebugEvent(GCurrentThread) of
        deException     : GState := dsPause;
        deCreateProcess :
          begin
            GProcessMap.Add(AProcessIdentifier, GCurrentProcess);

            //if AEvent.CreateProcessInfo.lpBaseOfImage <> nil
            //then DumpPEImage(AEvent.CreateProcessInfo.hProcess, TDbgPtr(AEvent.CreateProcessInfo.lpBaseOfImage));

            if GBreakOnLibraryLoad
            then GState := dsPause;
          end;
        deExitProcess :
          begin
            if GCurrentProcess = GMainProcess then GMainProcess := nil;
            GProcessMap.Delete(AProcessIdentifier);

            GState := dsStop;
            WriteLN('Process stopped with exitcode: ', GCurrentProcess.ExitCode);
          end;
        deLoadLibrary :
          begin
            if GCurrentProcess.GetLib(GCurrentProcess.LastEventProcessIdentifier, ALib)
            and (GImageInfo <> iiNone)
            then begin
              WriteLN('Name: ', ALib.Name);
              //if GImageInfo = iiDetail
              //then DumpPEImage(Proc.Handle, Lib.BaseAddr);
            end;
            if GBreakOnLibraryLoad
            then GState := dsPause;

          end;
        deBreakpoint :
          begin
            writeln(Format('Reached breakpoint at %s.',[FormatAddress(GCurrentProcess.GetInstructionPointerRegisterValue)]));
            GState:=dsPause;
          end;
      end; {case}
    end;
  until (GState in [dsStop, dsPause, dsQuit]);

  if GState = dsPause
  then begin
    ShowDisas;
    ShowCode;
  end;
end;

end.
