unit Fppkg_EnvironmentOptions;

{ Package detail form for the lazarus package manager

  Copyright (C) 2018 Joost van der Sluis

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  IDEOptionsIntf,
  IDEOptEditorIntf,
  LazIDEIntf,
  LazFileCache,
  LazFileUtils,
  LCLProc,
  XMLConf,
  StdCtrls,
  ExtCtrls,
  fppkg_const;

type

  { TFppkgEnvironmentOptions }

  TFppkgEnvironmentOptions = class(TAbstractIDEEnvironmentOptions)
  private
    FInstallFPMakeDependencies: Boolean;
    FConfFileName: string;
    FUseFPMakeWhenPossible: Boolean;
    function GetConfFileName: string;
  public
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    procedure Load();
    procedure Save();
    procedure DoAfterWrite(Restore: boolean); override;
  published
    property InstallFPMakeDependencies: Boolean read FInstallFPMakeDependencies write FInstallFPMakeDependencies;
    property UseFPMakeWhenPossible: Boolean read FUseFPMakeWhenPossible write FUseFPMakeWhenPossible;
  end;

  { TFppkgEnvironmentOptionsFrame }

  TFppkgEnvironmentOptionsFrame = class(TAbstractIDEOptionsEditor)
    cbInstallDependencies: TCheckBox;
    cbUseFPMakeWhenPossible: TCheckBox;
  public
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    function GetTitle: String; override;
  end;


procedure Register;

var
  FppkgOptions: TFppkgEnvironmentOptions = nil;

implementation

{$R *.lfm}

const
  DefaultConfFileName = 'fppkg.xml';

procedure Register;
var
  OptionsGroup: Integer;
begin
  FppkgOptions := TFppkgEnvironmentOptions.Create;

  OptionsGroup := GetFreeIDEOptionsGroupIndex(GroupEditor);
  RegisterIDEOptionsGroup(OptionsGroup, TFppkgEnvironmentOptions);
  RegisterIDEOptionsEditor(OptionsGroup, TFppkgEnvironmentOptionsFrame, 1);

  FppkgOptions.Load();
end;

{ TFppkgEnvironmentOptionsFrame }

procedure TFppkgEnvironmentOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
end;

procedure TFppkgEnvironmentOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  FppkgOptions: TFppkgEnvironmentOptions;
begin
  FppkgOptions := AOptions as TFppkgEnvironmentOptions;
  cbInstallDependencies.Checked := FppkgOptions.InstallFPMakeDependencies;
  cbUseFPMakeWhenPossible.Checked := FppkgOptions.UseFPMakeWhenPossible;
end;

procedure TFppkgEnvironmentOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  FppkgOptions: TFppkgEnvironmentOptions;
begin
  FppkgOptions := AOptions as TFppkgEnvironmentOptions;
  FppkgOptions.InstallFPMakeDependencies := cbInstallDependencies.Checked;
  FppkgOptions.UseFPMakeWhenPossible := cbUseFPMakeWhenPossible.Checked;
end;

class function TFppkgEnvironmentOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TFppkgEnvironmentOptions;
end;

function TFppkgEnvironmentOptionsFrame.GetTitle: String;
begin
  Result := rsFppkgEnvironmentOptionsCaption;
end;

{ TFppkgEnvironmentOptions }

function TFppkgEnvironmentOptions.GetConfFileName: string;
begin
  Result := FConfFileName;
  if Result <> '' then
    exit;
  FConfFileName := AppendPathDelim(LazarusIDE.GetPrimaryConfigPath) + DefaultConfFileName;
  LazarusIDE.CopySecondaryConfigFile(DefaultConfFileName);
  Result := FConfFileName;
end;

class function TFppkgEnvironmentOptions.GetGroupCaption: string;
begin
  Result := rsFppkgEnvironmentOptions;
end;

class function TFppkgEnvironmentOptions.GetInstance: TAbstractIDEOptions;
begin
  Result:=FppkgOptions;
end;

procedure TFppkgEnvironmentOptions.Load();
var
  XMLConfig: TXMLConfig;
begin
  try
    XMLConfig := TXMLConfig.Create(nil);
    try
      XMLConfig.Filename := GetConfFileName;
      InstallFPMakeDependencies := XMLConfig.GetValue('InstallDependencies/Value',False);
      UseFPMakeWhenPossible := XMLConfig.GetValue('UseFPMakeWhenPossible/Value',False);
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do
      DebugLn('[TFppkgEnvironmentOptions.Load]  error reading "',FConfFileName,'": ',E.Message);
  end;
end;

procedure TFppkgEnvironmentOptions.Save();
var
  XMLConfig: TXMLConfig;
begin
  try
    InvalidateFileStateCache;
    XMLConfig:=TXMLConfig.Create(Nil);
    try
      XMLConfig.Filename:=GetConfFileName;
      XMLConfig.SetValue('InstallDependencies/Value',InstallFPMakeDependencies);
      XMLConfig.SetValue('UseFPMakeWhenPossible/Value',UseFPMakeWhenPossible);

      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do
      DebugLn('[TFppkgEnvironmentOptions.Save]  error writing "',FConfFileName,'": ',E.Message);
  end;
end;

procedure TFppkgEnvironmentOptions.DoAfterWrite(Restore: boolean);
begin
  inherited DoAfterWrite(Restore);
  if not Restore then
   Save();
end;

finalization
  FreeAndNil(FppkgOptions);
end.

