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
  Classes, Forms, IDEProcs, Debugger, EnvironmentOpts;

type
  TDebuggerDlgClass = class of TDebuggerDlg;
  
  TDebuggerDlg = class(TForm)
  private
    FDebugger: TDebugger;
  protected                                              
    procedure SetDebugger(const ADebugger: TDebugger); virtual;
    procedure DoClose(var Action: TCloseAction); override;
  public
    destructor Destroy; override;
    property Debugger: TDebugger read FDebugger write SetDebugger;
  end;

implementation 
          
{ TDebuggerDlg }          
          
destructor TDebuggerDlg.Destroy; 
begin
  Debugger := nil;
  inherited;
end;

procedure TDebuggerDlg.SetDebugger(const ADebugger: TDebugger);
begin
  FDebugger := ADebugger; 
end;

procedure TDebuggerDlg.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  EnvironmentOptions.IDEWindowLayoutList.ItemByForm(Self).GetCurrentPosition;
end;

{ =============================================================================
  $Log$
  Revision 1.3  2003/05/18 10:42:58  mattias
  implemented deleting empty submenus

  Revision 1.2  2002/05/10 06:57:48  lazarus
  MG: updated licenses

  Revision 1.1  2002/03/23 15:54:30  lazarus
  MWE:
    + Added locals dialog
    * Modified breakpoints dialog (load as resource)
    + Added generic debuggerdlg class
    = Reorganized main.pp, all debbugger relater routines are moved
      to include/ide_debugger.inc

  
}
end.
