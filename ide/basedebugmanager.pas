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
  Classes, SysUtils, Forms, Project, SourceMarks, Debugger, ProjectDefs,
  Laz_XMLCfg;

type
  { TBaseDebugManager }
  
  TDebugManagerState = (
    dmsInitializingDebuggerObject,
    dmsInitializingDebuggerObjectFailed,
    dmsDebuggerObjectBroken  // the debugger entered the error state
    );
  TDebugManagerStates = set of TDebugManagerState;

  TBaseDebugManager = class(TComponent)
  private
    function GetDebuggerClass(const AIndex: Integer): TDebuggerClass;
  protected
    FDestroying: boolean;
    FCallStack: TIDECallStack;
    FExceptions: TIDEExceptions;
    FSignals: TIDESignals;
    FBreakPoints: TIDEBreakPoints;
    FLocals: TIDELocals;
    FWatches: TIDEWatches;
    FManagerStates: TDebugManagerStates;
    function  FindDebuggerClass(const Astring: String): TDebuggerClass;
    function  GetState: TDBGState; virtual; abstract;
    function  GetCommands: TDBGCommands; virtual; abstract;
  public
    procedure ConnectMainBarEvents; virtual; abstract;
    procedure ConnectSourceNotebookEvents; virtual; abstract;
    procedure SetupMainBarShortCuts; virtual; abstract;
    procedure UpdateButtonsAndMenuItems; virtual; abstract;

    procedure LoadProjectSpecificInfo(XMLConfig: TXMLConfig;
                                      Merge: boolean); virtual; abstract;
    procedure SaveProjectSpecificInfo(XMLConfig: TXMLConfig;
                                      Flags: TProjectWriteFlags); virtual; abstract;

    function  DebuggerCount: Integer;

    procedure DoRestoreDebuggerMarks(AnUnitInfo: TUnitInfo); virtual; abstract;

    function InitDebugger: Boolean; virtual; abstract;
    
    function DoPauseProject: TModalResult; virtual; abstract;
    function DoStepIntoProject: TModalResult; virtual; abstract;
    function DoStepOverProject: TModalResult; virtual; abstract;
    function DoRunToCursor: TModalResult; virtual; abstract;
    function DoStopProject: TModalResult; virtual; abstract;
    procedure DoToggleCallStack; virtual; abstract;
    procedure ProcessCommand(Command: word; var Handled: boolean); virtual; abstract;

    function RunDebugger: TModalResult; virtual; abstract;
    procedure EndDebugging; virtual; abstract;
    function Evaluate(const AExpression: String; var AResult: String
                     ): Boolean; virtual; abstract; // Evaluates the given expression, returns true if valid
    
    function DoCreateBreakPoint(const AFilename: string; ALine: integer;
                                WarnIfNoDebugger: boolean): TModalResult; virtual; abstract;
    function DoDeleteBreakPoint(const AFilename: string; ALine: integer
                                ): TModalResult; virtual; abstract;
    function DoDeleteBreakPointAtMark(const ASourceMark: TSourceMark
                                     ): TModalResult; virtual; abstract;

    function ShowBreakPointProperties(const ABreakpoint: TIDEBreakPoint): TModalresult; virtual; abstract;
    function ShowWatchProperties(const AWatch: TIDEWatch): TModalresult; virtual; abstract;

  public
    property Commands: TDBGCommands read GetCommands;  // All current available commands of the debugger
    property Debuggers[const AIndex: Integer]: TDebuggerClass read GetDebuggerClass;
    property Destroying: boolean read FDestroying;
    property State: TDBGState read GetState;           // The current state of the debugger

    property BreakPoints: TIDEBreakPoints read FBreakpoints;   // A list of breakpoints for the current project
    property Exceptions: TIDEExceptions read FExceptions;      // A list of exceptions we should ignore
    property CallStack: TIDECallStack read FCallStack;
    property Locals: TIDELocals read FLocals;
    property Signals: TIDESignals read FSignals;               // A list of actions for signals we know of
    property Watches: TIDEWatches read FWatches;
  end;

procedure RegisterDebugger(const ADebuggerClass: TDebuggerClass);

var
  DebugBoss: TBaseDebugManager;

implementation

var
  MDebuggerClasses: TStringList;

procedure RegisterDebugger(const ADebuggerClass: TDebuggerClass);
begin
  MDebuggerClasses.AddObject(ADebuggerClass.ClassName, TObject(Pointer(ADebuggerClass)));
end;


{ TBaseDebugManager }

function TBaseDebugManager.DebuggerCount: Integer;
begin
  Result := MDebuggerClasses.Count;
end;

function TBaseDebugManager.FindDebuggerClass(const AString: String): TDebuggerClass;
var
  idx: Integer;
begin
  idx := MDebuggerClasses.IndexOf(AString);
  if idx = -1
  then Result := nil
  else Result := TDebuggerClass(MDebuggerClasses.Objects[idx]);
end;

function TBaseDebugManager.GetDebuggerClass(const AIndex: Integer): TDebuggerClass;
begin
  Result := TDebuggerClass(MDebuggerClasses.Objects[AIndex]);
end;

initialization
  DebugBoss := nil;
  MDebuggerClasses := TStringList.Create;
  MDebuggerClasses.Sorted := True;
  MDebuggerClasses.Duplicates := dupError;
  
finalization
  FreeAndNil(MDebuggerClasses);
  
end.



