{ $Id: wincedef.pp 9243 2006-05-05 05:52:08Z mattias $
                         ------------------------------
                         gtkdef.pp  -  Type definitions
                         ------------------------------

 @created(Wed Jan 24st WET 2001)
 @lastmod($Date: 2006-05-05 09:22:08 +0330 (Fri, 05 May 2006) $)
 @author(Marc Weustink <marc@@lazarus.dommelstein.net>)

 This unit contains type definitions needed in the Windows <-> LCL interface

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

unit WinCEDef;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, LCLType;

const
// Used by TCalendar
  MCM_FIRST         = $1000;
  MCM_GETCURSEL     = MCM_FIRST + 1;
  MCM_SETCURSEL     =  MCM_FIRST + 2;
  MCM_GETMINREQRECT = MCM_FIRST + 9;
  MCS_WEEKNUMBERS   = $0004;

type
  { lazarus win32 Interface definition for additional timer data needed to find the callback}
  PWinCETimerInfo = ^TWinCETimerinfo;
  TWinCETimerInfo = record
    TimerID: UINT;         // the windows timer ID for this timer
    TimerFunc: TFNTimerProc; // owner function to handle timer
  end;

var
  // FTimerData contains the currently running timers
  FTimerData : TList;   // list of PWin32Timerinfo

implementation

end.
