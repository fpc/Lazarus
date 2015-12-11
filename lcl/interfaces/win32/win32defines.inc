{ $Id: win32int.pp 14801 2008-04-11 11:22:20Z vincents $ }
{%MainUnit win32int.pp}

{*****************************************************************************
  Centralized includefile for common win32 defines
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

{$IFDEF Trace}
{$ASSERTIONS ON}
{$ENDIF}

// defining the following will print all messages as they are being handled
// valuable for investigation of message trees / interrelations
{ $define MSG_DEBUG}

{.$DEFINE RedirectDestroyMessages}
