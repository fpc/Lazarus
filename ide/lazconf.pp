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

This unit contains functions to manage OS specific configuration path
information from within Lazarus.
}
unit LazConf;

{$mode objfpc}{$H+}

interface

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

uses
//  Windows,
  SysUtils, Classes, IDEProcs;

  { Config Path Functions }
  {
    @abstract(Use a these functions to manage config paths )
    Introduced by Curtis White <cwhite@aracnet.com>
    Currently maintained by Curtis White <cwhite@aracnet.com>
  }

  { The primary config path is the local or user specific path.
    If the primary config path does not exists, it will automatically be
    created.
    The secondary config path is for templates. The IDE will never write to it.
    If a config file is not found in the primary config file, Lazarus will
    copy the template file from the secondary config file. If there is no
    template file, the IDE will create a default file.
  }
  function GetPrimaryConfigPath: String;
  function GetSecondaryConfigPath: String;
  procedure CreatePrimaryConfigPath;
  procedure SetPrimaryConfigPath(const NewValue: String);
  procedure SetSecondaryConfigPath(const NewValue: String);
  procedure CopySecondaryConfigFile(const AFilename: String);
  
  function FindDefaultCompilerPath: string;

implementation

{$I lazconf.inc}


end.

{
  $Log$
  Revision 1.5  2002/03/22 17:36:09  lazarus
  MG: added include link history

  Revision 1.4  2001/12/10 08:44:23  lazarus
  MG: added search for compiler, if not set

  Revision 1.3  2001/05/27 11:52:00  lazarus
  MG: added --primary-config-path=<filename> cmd line option

  Revision 1.2  2001/02/06 13:55:23  lazarus
  Changed the files from mode delphi to mode objfpc
  Shane

  Revision 1.1  2000/07/13 10:27:47  michael
  + Initial import

  Revision 1.1  2000/04/25 01:24:35  lazarus
  Adding lazconf.pp interface file.                              CAW


}

