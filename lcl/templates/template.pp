{
 /***************************************************************************
                               ????????.pp
                             -------------------
                             Component Library ???????? Controls
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
@author(TMyClass - Author Name <author@emailaddress.com>)                       
@author(TMyOtherClass - Other Author Name <otherauthor@emailaddress.com>)                       
@created(11-Aug-1999)
@lastmod(11-Aug-1999)

Detailed description of the Unit.
} 

unit ????????;

//{$mode delphi}
{$mode objfpc}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
  //glib, gdk, gtk,
  Classes, Controls, SysUtils, LCLType, StdCtrls, lMessages;


type

  { TMyClass }
  {
    @abstract(Short description of the class.)
    Introduced by Author Name <author@emailaddress.com>
    Currently maintained by Maintainer Name <mainter@emailaddress.com>
  }
  TMyClass = Class(TCustomControl)
  private
    { Private variables and methods }
  protected
    { Protected variables and methods }
  public
    { Public variables and methods }
    constructor Create(AOwner : TComponent);
    destructor Destroy;
  published
    { Published variables and methods }
  end;


implementation

uses Interfaces;

{$I template.inc}


initialization

finalization

end.

{
  $Log$
  Revision 1.2  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.1  2000/07/13 10:28:31  michael
  + Initial import

  Revision 1.2  1999/10/19 19:59:15  lazarus
  Added clipbrd.pp and clipbrd.inc.  Nothing done to them yet.
  Shane

  Revision 1.1  1999/08/12 16:21:54  lazarus
  Templates initially created    CAW

}

