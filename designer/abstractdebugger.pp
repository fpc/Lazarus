{
 /***************************************************************************
                               AbstractDebugger.pp
                             -------------------




 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/
}
unit AbstractDebugger;

{$mode objfpc}

interface

uses
  classes, Controls, forms,buttons,sysutils, Graphics,Extctrls;

type

  TAbstractDebugger = class
   public
     procedure Go; abstract; virtual;//Starts / continues debugging
     procedure Pause; abstract; virtual;//Stops running
     procedure Stop; abstract; virtual;//quit debugging
     procedure StepOver; abstract; virtual;
     procedure StepInto; abstract; virtual;
     procedure Goto(const ASource: String; ALine: Integer); abstract; virtual;//Executes til a certain point
     procedure ExecuteTo(const ASource: String; ALine: Integer); abstract; virtual; //Executes til a certain point
     procedure Goto; abstract; virtual; //No execute, only set exec point
     property FileName: String; abstract; virtual;//The name of the exe to be debugged
     property Flags: TCommandFlags; abstract; virtual; //All available flags of the debugger
     property OnCurrent: TDebuggerCurrentLineEvent; abstract; virtual; //Passes info about the current line being debugged
     property BreakPoints: TDBGBreakPoints; abstract; virtual; //list of all breakpoints
     property Variables: TDBGVariables; abstract; virtual; //list of all watches localvars etc
     property OnState: TDebuggerStateEvent; abstract; virtual; //Fires when the current state of the debugger changes
   end;

implementation

end.
