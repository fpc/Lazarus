{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Miscellaneous options of the lazarus IDE.
}
unit MiscOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BuildLazDialog, LazConf, IDEProcs, Laz_XMLCfg;

type
  TMiscellaneousOptions = class
  private
    fBuildLazOpts: TBuildLazarusOptions;
    fFilename: string;
    function GetFilename: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property BuildLazOpts: TBuildLazarusOptions
      read fBuildLazOpts write fBuildLazOpts;
    property Filename: string read GetFilename;
  end;


var MiscellaneousOptions: TMiscellaneousOptions;


implementation


const
  MiscOptsFilename = 'miscellaneousoptions.xml';
  MiscOptsVersion = 1;

{ TMiscellaneousOptions }

constructor TMiscellaneousOptions.Create;
begin
  inherited Create;
  BuildLazOpts:=TBuildLazarusOptions.Create;
end;

destructor TMiscellaneousOptions.Destroy;
begin
  BuildLazOpts.Free;
  inherited Destroy;
end;

function TMiscellaneousOptions.GetFilename: string;
var
  ConfFileName: string;
begin
  if fFilename='' then begin
    ConfFileName:=SetDirSeparators(GetPrimaryConfigPath+'/'+MiscOptsFilename);
    CopySecondaryConfigFile(MiscOptsFilename);
    if (not FileExists(ConfFileName)) then begin
      writeln('NOTE: miscellaneous options file not found - using defaults');
    end;
    FFilename:=ConfFilename;
  end;
  Result:=fFilename;
end;

procedure TMiscellaneousOptions.Load;
var XMLConfig: TXMLConfig;
  FileVersion: integer;
begin
  try
    XMLConfig:=TXMLConfig.Create(GetFilename);
  except
    writeln('ERROR: unable to open miscellaneous options "',GetFilename,'"');
    exit;
  end;
  try
    try
      FileVersion:=XMLConfig.GetValue('MiscellaneousOptions/Version/Value',0);

      if (FileVersion<MiscOptsVersion) and (FileVersion<>0) then
        writeln('NOTE: converting old miscellaneous options ...');

      BuildLazOpts.Load(XMLConfig,'MiscellaneousOptions/BuildLazarusOptions/');
    finally
      XMLConfig.Free;
    end;
  except
    writeln('ERROR: unable read miscellaneous options from "',GetFilename,'"');
  end;
end;

procedure TMiscellaneousOptions.Save;
var XMLConfig: TXMLConfig;
begin
  try
    XMLConfig:=TXMLConfig.Create(GetFilename);
  except
    writeln('ERROR: unable to open miscellaneous options "',GetFilename,'"');
    exit;
  end;
  try
    try
      XMLConfig.SetValue('MiscellaneousOptions/Version/Value',MiscOptsVersion);

      BuildLazOpts.Save(XMLConfig,'MiscellaneousOptions/BuildLazarusOptions/');
      
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    writeln('ERROR: unable read miscellaneous options from "',GetFilename,'"');
  end;
end;

end.


