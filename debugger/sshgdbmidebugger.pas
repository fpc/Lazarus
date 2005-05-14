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
  Classes, SysUtils, Dialogs, Controls, LazConf, GDBMIDebugger, DBGUtils,
  BaseDebugManager, Debugger, PropEdits, Graphics, LCLProc;
  
type
  TSSHGDBMIDebugger = class(TGDBMIDebugger)
  private
  protected
    function ParseInitialization: Boolean; override;
  public
    class function CreateProperties: TDebuggerProperties; override;  // Creates debuggerproperties
    class function Caption: String; override;
    class function ExePaths: String; override;
  end;

  TSSHGDBMIDebuggerProperties = class(TGDBMIDebuggerProperties)
  private
    FNote: String; //dummy
  published
    property Note: String read FNote write FNote;
  end;

implementation

type

  TSSHGDBMINotePropertyEditor = class(TStringPropertyEditor)
  private
  protected
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure SetValue(const NewValue: ansistring); override;
    procedure PropMeasureHeight(const NewValue: ansistring;  ACanvas:TCanvas;
                                var AHeight:Integer); override;
  end;

{ TSSHGDBMINotePropertyEditor }

function TSSHGDBMINotePropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paReadOnly];
end;

function TSSHGDBMINotePropertyEditor.GetValue: ansistring;
begin
  Result := 'The GNU debugger through ssh allows to remote debug via a ssh'
          + ' connection. See docs/RemoteDebugging.txt for details. The path'
          + ' must contain the ssh client filename, the hostname with an optional'
          + ' username and the filename of gdb on the remote computer.'
          + ' For example: "/usr/bin/ssh username@hostname gdb"';
end;

procedure TSSHGDBMINotePropertyEditor.PropMeasureHeight(const NewValue: ansistring; ACanvas: TCanvas; var AHeight: Integer);
begin
  AHeight := 100;
end;

procedure TSSHGDBMINotePropertyEditor.SetValue (const NewValue: ansistring);
begin
  // cannot write to note
end;


{ TSSHGDBMIDebugger }

function TSSHGDBMIDebugger.Caption: String;
begin
  Result := 'GNU debugger through SSH (gdb)';
end;

function TSSHGDBMIDebugger.CreateProperties: TDebuggerProperties;
begin
  Result := TSSHGDBMIDebuggerProperties.Create;
end;

function TSSHGDBMIDebugger.ExePaths: String;
begin
  Result := '/usr/bin/ssh user@remote /usr/bin/gdb';
end;

function TSSHGDBMIDebugger.ParseInitialization: Boolean;

  function CheckReadLine(var ALine: String): Boolean;
  // does a checked read
  // returns True if we should process it
  // returns False if it is the gdb prompt
  begin
    ALine := ReadLine(True);
    Result := Pos('(gdb)', ALine) = 0;
    if Result
    then ALine := StripLN(ReadLine);
  end;
var
  Line: String;
begin
  Result := False;
  
  // strip leading empty lines
  while CheckReadLine(Line) and (Line = '') do;

  // succesfull login ?
  while Pos('try again', Line) > 0 do CheckReadLine(Line);

(*
  if Pos('authenticity', Line) > 0
  then begin
    //
    S := Line + LineEnding + ReadLine + ReadLine;
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
*)

(*
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
      'Response: ' + LineEnding + Line + LineEnding + 'Continue ?',
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
*)
  if Pos('(gdb)', Line) > 0
  then Result := inherited ParseInitialization
  else begin
    // We got an unexpected result
    MessageDlg('Debugger',
      'Unexpected result:' + LineEnding + Line + LineEnding + 'The debugger wil be terminated.',
      mtInformation, [mbOK], 0);
    DebugProcess.Terminate(0);
  end;
end;

initialization
  RegisterPropertyEditor(TypeInfo(String), TSSHGDBMIDebuggerProperties, 'Note', TSSHGDBMINotePropertyEditor);
  
  RegisterDebugger(TSSHGDBMIDebugger);

end.
{ =============================================================================

  $Log$
  Revision 1.10  2005/05/14 12:09:36  marc
  * included debugger result tye in execcommand (start fixing debugging on Mac OSX)

  Revision 1.9  2004/01/17 13:29:04  mattias
  using now fpc constant LineEnding   from Vincent

  Revision 1.8  2004/01/09 00:10:51  marc
  * More debugger properties
  * Fixed debugger for regcall callingconvention in RTL

  Revision 1.7  2004/01/04 03:53:36  marc
  * Changed TComponentSelectionList to TPersistentSelectionList
  + Added SSHdebugger property

  Revision 1.6  2003/12/27 11:22:37  mattias
  minor fixes

  Revision 1.5  2003/10/22 17:50:16  mattias
  updated rpm scripts

  Revision 1.4  2003/10/16 23:54:27  marc
  Implemented new gtk keyevent handling

  Revision 1.3  2003/08/15 14:28:48  mattias
  clean up win32 ifdefs

  Revision 1.2  2003/07/30 23:15:39  marc
  * Added RegisterDebugger

  Revision 1.1  2003/07/24 08:52:46  marc
  + Added SSHGDB debugger

  
}


