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
  TSortDirection = (sdAscending, sdDescending);
  TSortDomain = (sdWords, sdLines, sdParagraphs);

  TMiscellaneousOptions = class
  private
    fBuildLazOpts: TBuildLazarusOptions;
    fFilename: string;
    FSortSelDirection: TSortDirection;
    FSortSelDomain: TSortDomain;
    function GetFilename: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property Filename: string read GetFilename;

    property BuildLazOpts: TBuildLazarusOptions
                                         read fBuildLazOpts write fBuildLazOpts;
    property SortSelDirection: TSortDirection read FSortSelDirection
                                              write FSortSelDirection;
    property SortSelDomain: TSortDomain read FSortSelDomain write FSortSelDomain;
  end;

const
  SortDirectionNames: array[TSortDirection] of string = (
    'Ascending', 'Descending');
  SortDomainNames: array[TSortDomain] of string = (
    'Words', 'Lines', 'Paragraphs');

var MiscellaneousOptions: TMiscellaneousOptions;

function SortDirectionNameToType(const s: string): TSortDirection;
function SortDomainNameToType(const s: string): TSortDomain;


implementation


const
  MiscOptsFilename = 'miscellaneousoptions.xml';
  MiscOptsVersion = 1;

function SortDirectionNameToType(const s: string): TSortDirection;
begin
  for Result:=Low(TSortDirection) to High(TSortDirection) do
    if AnsiCompareText(SortDirectionNames[Result],s)=0 then exit;
  Result:=sdAscending;
end;

function SortDomainNameToType(const s: string): TSortDomain;
begin
  for Result:=Low(TSortDomain) to High(TSortDomain) do
    if AnsiCompareText(SortDomainNames[Result],s)=0 then exit;
  Result:=sdLines;
end;

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
  Path: String;
begin
  try
    XMLConfig:=TXMLConfig.Create(GetFilename);
  except
    writeln('ERROR: unable to open miscellaneous options "',GetFilename,'"');
    exit;
  end;
  try
    try
      Path:='MiscellaneousOptions/';
      FileVersion:=XMLConfig.GetValue(Path+'Version/Value',0);

      if (FileVersion<MiscOptsVersion) and (FileVersion<>0) then
        writeln('NOTE: converting old miscellaneous options ...');

      BuildLazOpts.Load(XMLConfig,Path+'BuildLazarusOptions/');
      SortSelDirection:=SortDirectionNameToType(XMLConfig.GetValue(
           Path+'SortSelection/Direction',SortDirectionNames[sdAscending]));
      SortSelDomain:=SortDomainNameToType(XMLConfig.GetValue(
           Path+'SortSelection/Domain',SortDomainNames[sdLines]));
    finally
      XMLConfig.Free;
    end;
  except
    writeln('ERROR: unable read miscellaneous options from "',GetFilename,'"');
  end;
end;

procedure TMiscellaneousOptions.Save;
var XMLConfig: TXMLConfig;
  Path: String;
begin
  try
    XMLConfig:=TXMLConfig.Create(GetFilename);
  except
    writeln('ERROR: unable to open miscellaneous options "',GetFilename,'"');
    exit;
  end;
  try
    try
      Path:='MiscellaneousOptions/';
      XMLConfig.SetValue(Path+'Version/Value',MiscOptsVersion);

      BuildLazOpts.Save(XMLConfig,Path+'BuildLazarusOptions/');
      XMLConfig.SetDeleteValue(Path+'SortSelection/Direction',
           SortDirectionNames[SortSelDirection],
           SortDirectionNames[sdAscending]);
      XMLConfig.SetDeleteValue(Path+'SortSelection/Domain',
           SortDomainNames[SortSelDomain],SortDomainNames[sdLines]);

      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    writeln('ERROR: unable read miscellaneous options from "',GetFilename,'"');
  end;
end;

end.


