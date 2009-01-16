{ $Id$ }
{
 ---------------------------------------------------------------------------
 fpd  -  FP standalone windows debugger
 ---------------------------------------------------------------------------

 fpwd is a concept Free Pascal Windows Debugger. It is mainly used to thest
 the windebugger classes, but it may grow someday to a fully functional
 debugger written in pascal.

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
program fpd;
{$mode objfpc}{$H+}
{$APPTYPE CONSOLE}
uses
  SysUtils,
  Windows,
  FPDCommand,
  FPDGlobal,
  FPDLoop,
  FPDPEImage,
  FPDType,
  DbgClasses, DbgWinExtra, DbgPETypes, DbgDwarfConst, DbgDwarf;

function CtrlCHandler(CtrlType: Cardinal): BOOL; stdcall;
begin
  Result := False;
  case CtrlType of
    CTRL_C_EVENT,
    CTRL_BREAK_EVENT: begin
      if GState <> dsRun then Exit;
      if GMainProcess = nil then Exit;
      GMainProcess.Interrupt;

      Result := True;
    end;
    CTRL_CLOSE_EVENT: begin
      if (GState in [dsRun, dsPause]) and (GMainProcess <> nil)
      then TerminateProcess(GMainProcess.Handle, 0);
//      GState := dsQuit;
    end;
  end;
end;

var
  S, Last: String;
begin
  Write('FPDebugger on ', {$I %FPCTARGETOS%}, ' for ', {$I %FPCTARGETCPU%});
  WriteLn(' (', {$I %DATE%}, ' ', {$I %TIME%}, ' FPC: ', {$I %FPCVERSION%}, ')' );
  WriteLn('Copyright (c) 2006-2009 by Marc Weustink');
  WriteLN('starting....');
  
  if ParamCount > 0
  then begin
    GFileName := ParamStr(1);
    WriteLN('Using file: ', GFileName);
  end;

  SetConsoleCtrlHandler(@CtrlCHandler, True);
  repeat
    Write('FPD>');
    ReadLn(S);
    if S <> ''
    then Last := S;
    if Last = '' then Continue;
    HandleCommand(Last);
  until GState = dsQuit;
  SetConsoleCtrlHandler(@CtrlCHandler, False);
end.

