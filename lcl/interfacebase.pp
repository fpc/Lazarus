{
 /***************************************************************************
                               InterfaceBase.pp
                             -------------------
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
@author(TInterfaceBase - Marc Weustink <weus@quicknet.nl>)                       
@created(13-Nov-1999)
@lastmod(13-Nov-1999)

Detailed description of the Unit.
} 

unit InterfaceBase;

{$mode objfpc}
{$LONGSTRINGS ON}
interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  LCLLinux, LCLType, VCLGlobals, Classes, Controls, LMessages, GraphType;

type

  { TInterfaceBase }
  {
    @abstract(Short description of the class.)
    Introduced by Marc Weustink <weus@quicknet.nl>
    Currently maintained by Marc Weustink <weus@quicknet.nl>
  }
  TInterfaceBase = Class(TObject)
  private
  protected
  public
    procedure AppTerminate; virtual; abstract;
    procedure DoEvents; virtual; abstract;
    procedure HandleEvents; virtual; abstract;
    procedure WaitMessage; virtual; abstract;
    procedure Init; virtual; abstract;
    function GetText(Sender: TControl; var Text: String): Boolean; virtual; abstract;
    function  IntSendMessage3(LM_Message : Integer; Sender : TObject; data : pointer) : integer; virtual; abstract;
    procedure SetCallback(Msg : LongInt; Sender : TObject); virtual; abstract;
    procedure RemoveCallbacks(Sender : TObject); virtual; abstract;
    function UpdateHint(Sender: TObject): Integer; virtual; abstract;
    function RecreateWnd(Sender: TObject): Integer; virtual; abstract;

    
    {$DEFINE IF_BASE_MEMBER}
    {$I winapih.inc}
    {$UNDEF IF_BASE_MEMBER}
  end;


implementation

{$I interfacebase.inc}


initialization

finalization

end.

{
  $Log$
  Revision 1.5  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.4  2001/07/01 23:33:13  lazarus
  MG: added WaitMessage and HandleEvents is now non blocking

  Revision 1.3  2001/03/27 21:12:53  lazarus
  MWE:
    + Turned on longstrings
    + modified memotest to add lines

  Revision 1.2  2001/02/01 19:34:50  lazarus
  TScrollbar created and a lot of code added.

  It's cose to working.
  Shane

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.6  2000/05/27 22:20:55  lazarus
  MWE & VRS:
    + Added new hint code

  Revision 1.5  2000/03/23 22:48:56  lazarus
  MWE & Hans-Joachim Ott <hjott@compuserve.com>:
    + added replacement for LM_GetText

  Revision 1.4  2000/01/31 20:00:22  lazarus
  Added code for Application.ProcessMessages.  Needs work.
  Added TScreen.Width and TScreen.Height.  Added the code into
  GetSystemMetrics for these two properties.
  Shane

  Revision 1.3  1999/12/08 00:56:07  lazarus
  MWE:
    Fixed menus. Events aren't enabled yet (dumps --> invalid typecast ??)

  Revision 1.2  1999/11/13 12:58:03  lazarus
  MWE:
    Converted to Unix files :-)

  Revision 1.1  1999/11/13 12:53:53  lazarus
  MWE:
    Started to implement some platform dependent WINAPI stuff
    These are now part of InterfaceObject.

}
