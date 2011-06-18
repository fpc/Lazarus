{ $Id$ }
{                    ----------------------------------------
                       DebuggerDlg.pp  -  Base class for all
                         debugger related forms
                     ----------------------------------------

 @created(Wed Mar 16st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)

 This unit contains the base class for all debugger related dialogs.
 All common info needed for the IDE is found in this class

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
unit DebuggerDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, Controls, IDEProcs, Debugger, EnvironmentOpts, IDEOptionDefs,
  EditorOptions, IDECommands;

type

  { TDebuggerDlg }

  TDebuggerDlg = class(TForm)
  private
    FUpdateCount: integer;
  protected
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure DoBeginUpdate; virtual;
    procedure DoEndUpdate; virtual;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    function UpdateCount: integer;
  end;
  TDebuggerDlgClass = class of TDebuggerDlg;

var
  OnProcessCommand: procedure(Sender: TObject; Command: word; var Handled: boolean) of object;

implementation

{ TDebuggerDlg }

procedure TDebuggerDlg.BeginUpdate;
begin
  Inc(FUpdateCount);
  if FUpdateCount = 1 then DoBeginUpdate;
end;

procedure TDebuggerDlg.EndUpdate;
begin
  if FUpdateCount < 1 then RaiseException('TDebuggerDlg.EndUpdate');
  Dec(FUpdateCount);
  if FUpdateCount = 0 then DoEndUpdate;
end;

function TDebuggerDlg.UpdateCount: integer;
begin
  Result := FUpdateCount;
end;

procedure TDebuggerDlg.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Command: Word;
  Handled: Boolean;
begin
  Command := EditorOpts.KeyMap.TranslateKey(Key,Shift,TDebuggerDlg);

  if Assigned(OnProcessCommand) and (Command <> ecNone)
  then begin
    OnProcessCommand(Self,Command,Handled);
    Key := 0;
  end;
end;

(*
procedure TDebuggerDlg.SetDebugger(const ADebugger: TDebugger);
begin
  FDebugger := ADebugger;
end;
*)
procedure TDebuggerDlg.DoClose(var CloseAction: TCloseAction);
begin
  CloseAction := caFree; // we default to free
  inherited DoClose(CloseAction);
end;

procedure TDebuggerDlg.DoBeginUpdate;
begin
end;

procedure TDebuggerDlg.DoEndUpdate;
begin
end;

end.
