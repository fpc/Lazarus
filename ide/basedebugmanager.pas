{  $Id$  }
{
 /***************************************************************************
                         basedebugmanager.pp
                         -------------------
 TBaseDebugManager is the base class for TDebugManager, which controls all
 debugging related stuff in the IDE. The base class is mostly abstract.


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
unit BaseDebugManager;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, Forms;

type
  TBaseDebugManager = class(TComponent)
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
  end;

var DebugBoss: TBaseDebugManager;

implementation

end.

