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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    The IDE keeps book about loading projects and forms. When an error occurs,
    that kills the IDE, it will not open it automatically again the next time.

}
unit IDEProtocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc,
  // LazUtils
  LazConfigStorage, LazFileUtils,
  // IdeIntf
  BaseIDEIntf,
  // IDE
  LazConf;

const
  IDEProtocolOptsVersion: integer = 1;
  IDEProtocolFilename = 'protocol.xml';

type

  { TIDEProtocol }

  TIDEProtocol = class
  private
    FFilename: string;
    FLastProjectLoadingCrashed: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
  public
    property Filename: string read FFilename write FFilename;
    property LastProjectLoadingCrashed: boolean read FLastProjectLoadingCrashed
                                               write FLastProjectLoadingCrashed;
  end;
  
var
  IDEProtocolOpts: TIDEProtocol;

implementation

{ TIDEProtocol }

constructor TIDEProtocol.Create;
begin
end;

destructor TIDEProtocol.Destroy;
begin
  inherited Destroy;
end;

procedure TIDEProtocol.Load;
var
  Config: TConfigStorage;
begin
  if Filename='' then
    Filename:=AppendPathDelim(GetPrimaryConfigPath)+IDEProtocolFilename;
  try
    Config:=DefaultConfigClass.Create(Filename,true);
    try
      LoadFromConfig(Config,'Protocol/');
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('[TIDEProtocol.Load]  error reading "',Filename,'": ',E.Message);
    end;
  end;
end;

procedure TIDEProtocol.Save;
var
  Config: TConfigStorage;
begin
  if Filename='' then
    Filename:=AppendPathDelim(GetPrimaryConfigPath)+IDEProtocolFilename;
  try
    Config:=DefaultConfigClass.Create(Filename,false);
    try
      SaveToConfig(Config,'Protocol/');
      Config.WriteToDisk;
    finally
      Config.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('[TIDEProtocol.Save]  error writing "',Filename,'": ',E.Message);
    end;
  end;
end;

procedure TIDEProtocol.LoadFromConfig(Config: TConfigStorage; const Path: string);
begin
  FLastProjectLoadingCrashed:=Config.GetValue(Path+'LastProjectLoading/Failed',false);
end;

procedure TIDEProtocol.SaveToConfig(Config: TConfigStorage; const Path: string);
begin
  Config.SetValue(Path+'Version',IDEProtocolOptsVersion);
  Config.SetDeleteValue(Path+'LastProjectLoading/Failed',
                        FLastProjectLoadingCrashed,false);
end;

end.

