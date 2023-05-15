unit delphioptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazConfigStorage;

Const
  DefaultCompilerFileName = 'dcc32.exe';
  DefaultGenConfig = True;
  DefaultConfigExtension = '.conf';
  DefaultConvertPathsToUnix = {$IFDEF UNIX}True{$ELSE}False{$ENDIF};
  DefaultAdditionalOptions = '';

Type

  { TDelphiToolOptions }

  TDelphiToolOptions = Class(TPersistent)
  Private
    FAdditionalOptions: String;
    FCompilerFileName: String;
    FConfigFileExtension: String;
    FConvertPathsToUnix: Boolean;
    FGenerateConfigFile: Boolean;
    class var _Instance : TDelphiToolOptions;
  Protected
    procedure LoadFromConfig(Cfg: TConfigStorage); virtual;
    procedure SaveToConfig(Cfg: TConfigStorage); virtual;
  Public
    Constructor Create; virtual;
    class constructor init;
    class destructor Done;
    procedure Load;
    procedure Save;
    procedure Reset;
    class property Instance : TDelphiToolOptions Read _Instance;
    Property CompilerFileName : String Read FCompilerFileName Write FCompilerFileName;
    Property ConfigFileExtension : String Read FConfigFileExtension Write FConfigFileExtension;
    Property ConvertPathsToUnix : Boolean Read FConvertPathsToUnix Write FConvertPathsToUnix;
    Property AdditionalOptions : String Read FAdditionalOptions Write FAdditionalOptions;
  end;

Function DelphiToolOptions : TDelphiToolOptions;

implementation

uses BaseIDEIntf, strdelphitool;

function DelphiToolOptions: TDelphiToolOptions;
begin
  Result:=TDelphiToolOptions.Instance;
end;

{ TDelphiToolOptions }

procedure TDelphiToolOptions.LoadFromConfig(Cfg: TConfigStorage);
begin
  CompilerFilename:=Cfg.GetValue(KeyCompiler, CompilerFilename);
  ConfigFileExtension:=Cfg.GetValue(KeyConfigFileExt, ConfigFileExtension);
  ConvertPathsToUnix:=Cfg.GetValue(KeyConvertPaths, ConvertPathsToUnix);
  AdditionalOptions:=Cfg.GetValue(KeyAdditionalOptions, AdditionalOptions);
end;

procedure TDelphiToolOptions.SaveToConfig(Cfg: TConfigStorage);
begin
  Cfg.SetDeleteValue(KeyCompiler, CompilerFilename, DefaultCompilerFileName);
  Cfg.SetDeleteValue(KeyConfigFileExt, ConfigFileExtension, DefaultConfigExtension);
  Cfg.SetDeleteValue(KeyConvertPaths, ConvertPathsToUnix, DefaultConvertPathsToUnix);
  Cfg.SetDeleteValue(KeyAdditionalOptions, AdditionalOptions, DefaultAdditionalOptions);
end;

constructor TDelphiToolOptions.Create;
begin
  Reset;
end;

class constructor TDelphiToolOptions.init;
begin
  _Instance:=TDelphiToolOptions.Create;
end;

class destructor TDelphiToolOptions.Done;
begin
  FreeAndNil(_Instance)
end;

procedure TDelphiToolOptions.Load;

var
  Cfg: TConfigStorage;

begin
  Cfg:=GetIDEConfigStorage(DelphiToolsOptionsFile,true);
  try
    LoadFromConfig(Cfg);
  finally
    Cfg.Free;
  end;
end;

procedure TDelphiToolOptions.Save;

var
  Cfg: TConfigStorage;

begin
  Cfg:=GetIDEConfigStorage(DelphiToolsOptionsFile,false);
  try
    SaveToConfig(Cfg);
  finally
    Cfg.Free;
  end;
end;

procedure TDelphiToolOptions.Reset;
begin
  CompilerFileName:=DefaultCompilerFileName;
  ConfigFileExtension:=DefaultConfigExtension;
  ConvertPathsToUnix:=DefaultConvertPathsToUnix;
  AdditionalOptions:=DefaultAdditionalOptions;
end;

end.

