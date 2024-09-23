unit schemaeditorconf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, inifiles;

Const
  DefaultDisableMySQLVersionCheck = False;

Type

  { TSchemaSettings }

  TSchemaSettings = class(TPersistent)
  private
    FCurrentFile: String;
    FDisableMySQLVersionCheck: Boolean;
  Public
    Constructor Create; virtual;
    Procedure Reset; virtual;
    Class Function DefaultFileName : String;
    Procedure LoadFromIni(aIni : TCustomIniFile); virtual;
    Procedure SaveToIni(aIni : TCustomIniFile); virtual;
    Procedure LoadFromFile(const aFileName : String);
    Procedure SaveToFile(const aFileName : String);
    Property CurrentFile : String Read FCurrentFile;
  Published
    Property DisableMySQLVersionCheck : Boolean Read FDisableMySQLVersionCheck Write FDisableMySQLVersionCheck;
  end;

Function SchemaSettings : TSchemaSettings;


implementation

const
  SConnections = 'SchemaConnections';
  KeyDisableMySQLVersionCheck = 'DisableMySQLVersionCheck';

var
  _settings : TSchemaSettings;

Function SchemaSettings : TSchemaSettings;

begin
  if not assigned(_settings) then
    _settings:=TSchemaSettings.Create;
  Result:=_settings;
end;

{ TSchemaSettings }

constructor TSchemaSettings.Create;
begin
  Reset;
end;

procedure TSchemaSettings.Reset;

begin
  DisableMySQLVersionCheck:=DefaultDisableMySQLVersionCheck;
end;

class function TSchemaSettings.DefaultFileName: String;
begin
  Result:=GetAppConfigFile(False);
end;

procedure TSchemaSettings.LoadFromIni(aIni: TCustomIniFile);

begin
  With aIni do
    begin
    DisableMySQLVersionCheck:=ReadBool(SConnections,KeyDisableMySQLVersionCheck,DisableMySQLVersionCheck);
    end;
end;

procedure TSchemaSettings.SaveToIni(aIni: TCustomIniFile);

begin
  With aIni do
    begin
    WriteBool(SConnections,KeyDisableMySQLVersionCheck,DisableMySQLVersionCheck);
    end;
end;

procedure TSchemaSettings.LoadFromFile(const aFileName: String);

var
  Ini : TMemIniFile;

begin
  FCurrentFile:=AFileName;
  Ini:=TMemIniFile.Create(aFileName);
  try
    LoadFromIni(Ini);
  finally
    Ini.Free;
  end;
end;

procedure TSchemaSettings.SaveToFile(const aFileName: String);
var
  Ini : TMemIniFile;

begin
  FCurrentFile:=AFileName;
  Ini:=TMemIniFile.Create(aFileName);
  try
    SaveToIni(Ini);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

initialization

finalization
  _settings.Free;
end.

