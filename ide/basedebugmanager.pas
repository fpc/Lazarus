{  $Id$  }
{
 /***************************************************************************
                         basedebugmanager.pp
                         -------------------
 TBaseDebugManager is the base class for TDebugManager, which controls all
 debugging related stuff in the IDE. The base class is mostly abstract.


 ***************************************************************************/

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
unit BaseDebugManager;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, Forms, Debugger;

type
  TBaseDebugManager = class(TComponent)
  protected
    function  GetState: TDBGState; virtual; abstract;
    function  GetCommands: TDBGCommands; virtual; abstract;
  public
    procedure ConnectMainBarEvents; virtual; abstract;
    procedure ConnectSourceNotebookEvents; virtual; abstract;
    procedure SetupMainBarShortCuts; virtual; abstract;

    function DoInitDebugger: TModalResult; virtual; abstract;
    function DoPauseProject: TModalResult; virtual; abstract;
    function DoStepIntoProject: TModalResult; virtual; abstract;
    function DoStepOverProject: TModalResult; virtual; abstract;
    function DoRunToCursor: TModalResult; virtual; abstract;
    function DoStopProject: TModalResult; virtual; abstract;
    
    procedure RunDebugger; virtual; abstract;
    procedure EndDebugging; virtual; abstract;
    function Evaluate(const AExpression: String; var AResult: String): Boolean; virtual; abstract; // Evaluates the given expression, returns true if valid
    
    property Commands: TDBGCommands read GetCommands;              // All current available commands of the debugger
    property State: TDBGState read GetState;                       // The current state of the debugger
    
  end;

const
  DebugBoss: TBaseDebugManager = nil;

implementation

end.

