{ $Id: win32int.pp 14801 2008-04-11 11:22:20Z vincents $ }
{%MainUnit win32int.pp}

{*****************************************************************************
  Centralized includefile for common win32 defines
 *****************************************************************************

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

{$IFDEF Trace}
{$ASSERTIONS ON}
{$ENDIF}

// defining the following will print all messages as they are being handled
// valuable for investigation of message trees / interrelations
{ $define MSG_DEBUG}

{$IFNDEF DisableWindowsUnicodeSupport}
// Enable WindowsUnicodeSupport for fpc 2.2.1 and higher
{$IFNDEF VER2_2_0}
{$DEFINE WindowsUnicodeSupport}
{$ENDIF VER2_2_0}
{$ENDIF DisableWindowsUnicodeSupport}

{$IFDEF WindowsUnicodeSupport}
{$IFDEF VER2_2_0}
// A check to prevent that WindowsUnicodeSupport is enabled for fpc 2.2.0
{$FATAL Use fpc 2.2.1 or later, if you want to enable Windows Unicode Support}
{$ENDIF}
{$ENDIF}

{$DEFINE UseVistaDialogs}
{.$DEFINE RedirectDestroyMessages}
