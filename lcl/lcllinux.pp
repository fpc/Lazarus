
{  $Id$  }
{
 /***************************************************************************
                                LCLLinux.pp
                             -------------------
                             Component Library Windows Controls
                   Initial Revision  : Fri Jul 23 20:00:00 PDT 1999


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

{
@author(Curtis White <cwhite@aracnet.com>)
@created(17-Oct-1999)
@lastmod(17-Oct-1999)

This unit is being created specifically for compatibility with Delphi. It
should only be used for constants and type definitions that are included in
the Delphi Windows unit. This is only done for compatibiltiy.

}

unit LCLLinux;
{$mode objfpc}{$H+}

interface

uses Classes, LCLType, VCLGlobals, GraphType;

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

// All winapi related stuff
{$I winapih.inc}

function MakeLong(A,B : Word) : LongInt;
function MakeWord(A,B : Byte) : Word;
implementation

uses

  SysUtils, Interfaces, Strings, Math;

function MakeLong(A,B : Word) : LongInt;
begin
  Result := A or B shl 16;
end;

function MakeWord(A,B : Byte) : Word;
Begin
  Result := A or B shl 8;
end;

{$I winapi.inc}

end.

{
  $Log$
  Revision 1.15  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.14  2002/01/02 15:24:58  lazarus
  MG: added TCanvas.Polygon and TCanvas.Polyline

  Revision 1.13  2001/11/12 16:56:07  lazarus
  MG: CLIPBOARD

  Revision 1.12  2001/11/01 18:48:52  lazarus
  Changed Application.Messagebox to use TMessageBox class.
  Added icon images for mtError and mtConfirmation
  Shane

  Revision 1.11  2001/10/31 21:43:28  lazarus
  Added code for TApplication to get it ready to accept exceptions.
  Shane

  Revision 1.10  2001/09/30 08:34:49  lazarus
  MG: fixed mem leaks and fixed range check errors

  Revision 1.9  2001/06/20 13:35:51  lazarus
  MG: added VK_IRREGULAR and key grabbing

  Revision 1.8  2001/06/15 10:31:06  lazarus
  MG: set longstrings as default

  Revision 1.7  2001/04/06 22:28:09  lazarus
  * TTimer uses winapi interface now instead of sendmessage interface, stoppok

  Revision 1.6  2001/03/26 14:58:31  lazarus
  MG: setwindowpos + bugfixes

  Revision 1.5  2001/02/01 16:45:19  lazarus
  Started the code completion.
  Shane

  Revision 1.4  2000/09/10 23:08:30  lazarus
  MWE:
    + Added CreateCompatibeleBitamp function
    + Updated TWinControl.WMPaint
    + Added some checks to avoid gtk/gdk errors
    - Removed no fixed warning from GetDC
    - Removed some output

  Revision 1.3  2000/08/11 14:59:09  lazarus
  Adding all the Synedit files.
  Changed the GDK_KEY_PRESS and GDK_KEY_RELEASE stuff to fix the problem in the editor with the shift key being ignored.
  Shane

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import
}
