{ $Id$ }
{              ----------------------------------------------
                SSHGDBDebugger.pp  -  Debugger class for GDB
                                      through SSH
               ----------------------------------------------

 @created(Wed Jul 23rd WET 2003)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains the debugger class for the GDB/MI debugger through SSH.

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
unit SSHGDBMIDebugger;

{$mode objfpc}
{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Controls, GDBMIDebugger, DBGUtils;
  
type
  TSSHGDBMIDebugger = class(TGDBMIDebugger)
  private
  protected
    function  ParseInitialization: Boolean; override;
  public
  end;


implementation

{ TSSHGDBMIDebugger }

function TSSHGDBMIDebugger.ParseInitialization: Boolean;
var
  Line, S: String;
begin
  Result := False;
  
  Line := StripLN(ReadLine);
  while Line = '' do
  begin
    Line := ReadLine(True);
    if Pos('(gdb)', Line) > 0 then Break;
    Line := StripLN(ReadLine);
  end;
  
  if Pos('authenticity', Line) > 0
  then begin
    //
    S := Line + LINE_END + ReadLine + ReadLine;
    if MessageDlg('Debugger', S, mtConfirmation, [mbYes, mbNo], 0) <> mrYes
    then begin
      SendCmdLn('no');
      Exit;
    end;
    SendCmdLn('yes');
    repeat
      Line := StripLN(ReadLine);
    until Pos('password:', Line) > 0
  end;

  while Pos('password:', Line) > 0 do
  begin
    if not InputQuery('Debugger', 'Enter ' + Line, S)
    then begin
      DebugProcess.Terminate(0);
      Exit;
    end;
    // peek the line from here, we might have a prompt
    Line := ReadLine(True);
    if Pos('(gdb)', Line) > 0 then Break;

    // something else, read the line
    Line := ReadLine;
    if MessageDlg('Debugger',
      'Response: ' + LINE_END + Line + LINE_END + 'Continue ?',
      mtConfirmation, [mbYes, mbNo], 0) <> mrYes
    then begin
      DebugProcess.Terminate(0);
      Exit;
    end;
    // Check again if we got a prompt
    Line := ReadLine(True);
    if Pos('(gdb)', Line) > 0 then Break;

    // Next attempt
    Line := StripLN(ReadLine);
  end;
  
  if Pos('(gdb)', Line) > 0
  then Result := inherited ParseInitialization
  else begin
    // We got an unexpected result
    MessageDlg('Debugger',
      'Unexpected result:' + LINE_END + Line + LINE_END + 'The debugger wil be terminated.',
      mtInformation, [mbOK], 0);
    DebugProcess.Terminate(0);
  end;
end;

end.
{ =============================================================================

  $Log$
  Revision 1.1  2003/07/24 08:52:46  marc
  + Added SSHGDB debugger

  
}


