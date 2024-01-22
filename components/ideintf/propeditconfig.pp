{
/***************************************************************************
                             propeditconfig.pp
                             -----------------

 ***************************************************************************/

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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Bart Broersma

  Abstract:
    Miscellaneous config settings for property editors.
    Currently just for MaskEdit property editor.

}
unit PropEditConfig;

{$mode objfpc}{$H+}

interface

uses
  // RTL + LCL
  Classes, SysUtils,
  // LazUtils
  LazFileUtils, LazLoggerBase, LazUTF8, Laz2_XMLCfg,
  // IdeIntf
  LazIDEIntf;

const
  DefaultFilename = 'propeditconfig.xml';
  PropEditConfigVersion = 0;
  pSampleMaskEditFile = 'MaskEdit/SampleEditMaskFile';

type

  { TPropEditConfigs }

  TPropEditConfigs = class
  private
    FFilename: String;
    FSampleEditMaskFilename: String;
  protected
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  public
    constructor Create;
    procedure Load;
    procedure Save;

    property Filename: String read FFilename;
    property SampleEditMaskFilename: string read FSampleEditMaskFilename write FSampleEditMaskFilename;
  end;


var
  PropEditConfigs: TPropEditConfigs = nil;  // do NOT free yourself, it'll be auto-destroyed at finalization

implementation

{ TPropEditConfigs }

procedure TPropEditConfigs.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  FSampleEditMaskFilename := XMLConfig.GetValue(Path + pSampleMaskEditFile,'');
end;

procedure TPropEditConfigs.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetValue(Path + pSampleMaskEditFile,FSampleEditMaskFilename);
end;

constructor TPropEditConfigs.Create;
begin
   if Assigned(LazarusIDE) then
     FFilename := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)  + DefaultFilename;
   //if IDE does not exist, FFilename will be empty string, in effect making Load and Save silently fail
end;

procedure TPropEditConfigs.Load;
var
  XMLConfig: TXMLConfig;
begin
  try
    XMLConfig := TXMLConfig.Create(FFileName);
    LoadFromXMLConfig(XMLConfig,'PropEditConfig/');
    XMLConfig.Free;
  except
    on E: Exception do
    begin
      DebugLn('[TPropEditConfigs.Load]  error reading "',FFilename,'" ',E.Message);
    end;
  end;
end;

procedure TPropEditConfigs.Save;
var
  XMLConfig: TXMLConfig;
begin
  try
    InvalidateFileStateCache;
    XMLConfig:=TXMLConfig.CreateClean(FFileName);

    XMLConfig.SetDeleteValue('PropEditConfig/PropEditConfigVersion/',PropEditConfigVersion,0);
    SaveToXMLConfig(XMLConfig,'PropEditConfig/');
    XMLConfig.Flush;
    XMLConfig.Free;
  except
    on E: Exception do
    begin
      DebugLn('[TInputHistories.Save]  error writing "',FFilename,'" ',E.Message);
    end;
  end;
end;

finalization
  if Assigned(PropEditConfigs) then
    FreeandNil(PropEditConfigs);

end.
