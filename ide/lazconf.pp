{
 /***************************************************************************
                               lazconf.pp
                             -------------------
                           Lazarus Config Functions
                   Initial Revision  : Tue Apr 18 22:10:00 CET 2000

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
@author(Config Path Functions - Curtis White <cwhite@aracnet.com>)
@created(18-Apr-2000)
@lastmod(18-Apr-2000)

This unit contains functions to manage OS specific configuration path information 
from within Lazarus. 
}
unit LazConf;

{$mode objfpc}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
//  Windows,
  SysUtils, Classes;

  { Config Path Functions }
  {
    @abstract(Use a these functions to manage config paths )
    Introduced by Curtis White <cwhite@aracnet.com>
    Currently maintained by Curtis White <cwhite@aracnet.com>
  }

  function GetPrimaryConfigPath: String;
  function GetSecondaryConfigPath: String;
  procedure CreatePrimaryConfigPath;


implementation

{$I lazconf.inc}


end.

{
  $Log$
  Revision 1.2  2001/02/06 13:55:23  lazarus
  Changed the files from mode delphi to mode objfpc
  Shane

  Revision 1.1  2000/07/13 10:27:47  michael
  + Initial import

  Revision 1.1  2000/04/25 01:24:35  lazarus
  Adding lazconf.pp interface file.                              CAW


}

