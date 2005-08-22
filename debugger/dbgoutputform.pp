{ $Id$ }
{                        ----------------------------------------  
                          dbgoutputform.pp  -  Shows target output 
                         ---------------------------------------- 
 
 @created(Wed Feb 25st WET 2001)
 @lastmod($Date$)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

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
unit DBGOutputForm;

{$mode objfpc}
{$H+}

interface

uses
  Classes, Graphics, Controls, Forms, Dialogs, LResources,
  Buttons, StdCtrls, Menus, DebuggerDlg;

type
  TDbgOutputForm = class(TDebuggerDlg)
    txtOutput: TMemo;
    mnuPopup: TPopupMenu;
    popClear: TMenuItem;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure popClearClick(Sender: TObject);
  private
  protected
    procedure Loaded; override;
  public
    procedure AddText(const AText: String);
    procedure Clear;
    procedure SetLogText(Lines: TStrings);
    procedure GetLogText(Lines: TStrings);
  end;

implementation

procedure TDbgOutputForm.AddText(const AText: String);
begin
  txtOutput.Lines.Add(AText);
end;

procedure TDbgOutputForm.Clear;
begin             
  txtOutput.Lines.Clear; 
end;

procedure TDbgOutputForm.SetLogText(Lines: TStrings);
begin
  txtOutput.Lines.Assign(Lines);
end;

procedure TDbgOutputForm.GetLogText(Lines: TStrings);
begin
  Lines.Assign(txtOutput.Lines);
end;

procedure TDbgOutputForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TDbgOutputForm.FormCreate(Sender: TObject);
begin
  txtOutput.Lines.Clear;
end;

procedure TDbgOutputForm.Loaded;
begin
  inherited Loaded;
  
  // Not yet through resources
  txtOutput.Scrollbars := ssBoth;  
end;

procedure TDbgOutputForm.popClearClick(Sender: TObject);
begin
  Clear;
end;

initialization
  {$I dbgoutputform.lrs}

end.
{
  $Log$
  Revision 1.10  2004/02/02 16:59:27  mattias
  more Actions  TAction, TBasicAction, ...

  Revision 1.9  2004/01/05 15:22:42  mattias
  improved debugger: saved log, error handling in initialization, better reinitialize

  Revision 1.8  2002/05/30 22:45:57  lazarus
  MWE:
    - Removed menucreation from loaded since streaming works

  Revision 1.7  2002/05/10 07:00:08  lazarus
  MG: fixed typo

  Revision 1.6  2002/05/10 06:57:47  lazarus
  MG: updated licenses

  Revision 1.5  2002/04/24 20:42:29  lazarus
  MWE:
    + Added watches
    * Updated watches and watchproperty dialog to load as resource
    = renamed debugger resource files from *.lrc to *.lrs
    * Temporary fixed language problems on GDB (bug #508)
    * Made Debugmanager dialog handling more generic

  Revision 1.4  2002/03/23 15:54:30  lazarus
  MWE:
    + Added locals dialog
    * Modified breakpoints dialog (load as resource)
    + Added generic debuggerdlg class
    = Reorganized main.pp, all debbugger relater routines are moved
      to include/ide_debugger.inc

  Revision 1.3  2002/03/09 02:03:59  lazarus
  MWE:
    * Upgraded gdb debugger to gdb/mi debugger
    * Set default value for autpopoup
    * Added Clear popup to debugger output window

  Revision 1.2  2002/02/20 23:33:24  lazarus
  MWE:
    + Published OnClick for TMenuItem
    + Published PopupMenu property for TEdit and TMemo (Doesn't work yet)
    * Fixed debugger running twice
    + Added Debugger output form
    * Enabled breakpoints

  Revision 1.1  2001/11/05 00:12:51  lazarus
  MWE: First steps of a debugger.

}
