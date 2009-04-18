{
 /***************************************************************************
                               ????????.pp
                             -------------------
                             Component Library ???????? Controls
                   Initial Revision  : Fri Jul 23 20:00:00 PDT 1999


 ***************************************************************************/

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


