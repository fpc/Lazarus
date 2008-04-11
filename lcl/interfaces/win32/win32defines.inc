{%MainUnit win32int.pp}
{ $Id: gtkdefines.inc 13666 2008-01-08 11:15:07Z paul $ }

{*****************************************************************************
  Centralized includefile for common win32 defines
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{$IFDEF Trace}
{$ASSERTIONS ON}
{$ENDIF}

// defining the following will print all messages as they are being handled
// valuable for investigation of message trees / interrelations
{ $define MSG_DEBUG}

// for now, DisableWindowsUnicodeSupport
{$DEFINE DisableWindowsUnicodeSupport}
{$IFNDEF DisableWindowsUnicodeSupport}
// Enable WindowsUnicodeSupport for fpc 2.2.1 and higher
{$IFNDEF VER2_2_0}
{$DEFINE WindowsUnicodeSupport}
{$ENDIF VER2_2_0}
{$ENDIF DisableWindowsUnicodeSupport}

