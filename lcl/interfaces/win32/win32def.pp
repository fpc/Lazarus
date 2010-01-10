{ $Id$
                         ------------------------------
                         win32def.pp  -  Type definitions
                         ------------------------------

 @created(Wed Jan 24st WET 2001)
 @lastmod($Date$)
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

unit Win32Def;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, LCLType, Interfacebase;

const
  // it is not good to use WM_USER since many programs use it.
  WM_LCL_SOCK_ASYNC = WM_USER + $500;

type
  { lazarus win32 Interface definition for additional timer data needed to find the callback}
  PWin32TimerInfo = ^TWin32Timerinfo;
  TWin32TimerInfo = record
    TimerID: UINT;         // the windows timer ID for this timer
    TimerFunc: TWSTimerProc; // owner function to handle timer
  end;

var
  // FTimerData contains the currently running timers
  FTimerData : TList;   // list of PWin32Timerinfo

implementation

end.
