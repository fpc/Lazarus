unit dbconfig;

{ Small unit that retrieves connection settings for your database

  Copyright (c) 2012 Reinier Olislagers

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

//todo: add command line support (--dbtype=, --db=, --dbhost=, dbuser=, dbpass=, dbcharset=
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type
  { TDBConnectionConfig }
  TDBConnectionConfig = class(TObject)
  private
    FDBCharset: string;
    FDBType: string;
    FDBHost: string;
    FDBPath: string;
    FDBUser: string;
    FDBPassword: string;
    FSettingsFileIsRead: boolean; //indicates if we read in settings file
    FSettingsFile: string;
    function GetDBCharset: string;
    function GetDBHost: string;
    function GetDBPassword: string;
    function GetDBPath: string;
    function GetDBType: string;
    function GetDBUser: string;
    function GetDefaultSettingsFile:string;
    procedure ReadINIFile;
    procedure SetDBType(AValue: string);
    procedure SetSettingsFile(AValue: string);
  public
    property DBCharset: string read GetDBCharset write FDBCharset; //Character set used for database (e.g. UTF8)
    property DBHost: string read GetDBHost write FDBHost; //Database host/server (name or IP address). Leave empty for embedded
    property DBPath: string read GetDBPath write FDBPath; //Path/database name
    property DBUser: string read GetDBUser write FDBUser; //User name needed for database (e.g. sa, SYSDBA)
    property DBPassword: string read GetDBPassword write FDBPassword; //Password needed for user name
    property DBType: string read GetDBType write SetDBType; //Type of database connection, e.g. Firebird, Oracle
    property SettingsFile: string read FSettingsFile write SetSettingsFile; //ini file to read settings from. If empty defaults to <programname>.ini
    constructor Create;
    constructor Create(DefaultType:string; DefaultHost:string=''; DefaultPath:string='data.fdb';
      DefaultUser:string='SYSDBA'; DefaultPassword:string='masterkey'; DefaultCharSet:string='UTF8');
    destructor Destroy; override;
  end;

implementation

{ TDBConnectionConfig }

function TDBConnectionConfig.GetDBHost: string;
begin
  if not(FSettingsFileIsRead) then ReadINIFile;
  result:=FDBHost;
end;

function TDBConnectionConfig.GetDBCharset: string;
begin
  if not(FSettingsFileIsRead) then ReadINIFile;
  result:=FDBCharset;
end;

function TDBConnectionConfig.GetDBPassword: string;
begin
  if not(FSettingsFileIsRead) then ReadINIFile;
  result:=FDBPassword;
end;

function TDBConnectionConfig.GetDBPath: string;
begin
  if not(FSettingsFileIsRead) then ReadINIFile;
  result:=FDBPath;
end;

function TDBConnectionConfig.GetDBType: string;
begin
  if not(FSettingsFileIsRead) then ReadINIFile;
  result:=FDBType;
end;

function TDBConnectionConfig.GetDBUser: string;
begin
  if not(FSettingsFileIsRead) then ReadINIFile;
  result:=FDBUser;
end;

function TDBConnectionConfig.GetDefaultSettingsFile:string;
begin
  result:=ChangeFileExt(ExtractFileName(ParamStr(0)), '.ini');
end;

procedure TDBConnectionConfig.SetSettingsFile(AValue: string);
begin
  // If empty value given, use the program name
  if AValue='' then AValue:=GetDefaultSettingsFile;
  if FSettingsFile=AValue then Exit;
  FSettingsFile:=AValue;
  // Read from file if present
  ReadINIFile;
end;


procedure TDBConnectionConfig.SetDBType(AValue: string);
begin
  if FDBType=AValue then Exit;
  case UpperCase(AValue) of
    'FIREBIRD': FDBType:='Firebird';
    'POSTGRES', 'POSTGRESQL': FDBType:='PostgreSQL';
    'SQLITE','SQLITE3': FDBType:='SQLite';
  else FDBType:=AValue;
  end;
end;

procedure TDBConnectionConfig.ReadINIFile;
var
  INI: TIniFile;
begin
  if FileExists(FSettingsFile) then
  begin
    INI := TINIFile.Create(FSettingsFile);
    try
      FDBType := INI.ReadString('Database', 'DatabaseType', FDBType); //Default to Firebird
      FDBHost := INI.ReadString('Database', 'Host', FDBHost);
      FDBPath := INI.ReadString('Database', 'Database', FDBPath);
      FDBUser := INI.ReadString('Database', 'User', 'SYSDBA');
      FDBPassword := INI.ReadString('Database', 'Password', 'masterkey');
      FSettingsFileIsRead:=true;
    finally
      INI.Free;
    end;
  end;
end;

constructor TDBConnectionConfig.Create;
begin
  inherited Create;
  // Defaults
  FSettingsFile:=GetDefaultSettingsFile;
  FSettingsFileIsRead:=false;
  FDBType := 'Firebird';
  FDBHost := ''; //embedded: no hostname
  FDBPath := 'data.fdb';
  FDBUser := 'SYSDBA';
  FDBPassword := 'masterkey';
end;

constructor TDBConnectionConfig.Create(DefaultType:string; DefaultHost:string=''; DefaultPath:string='data.fdb';
  DefaultUser:string='SYSDBA'; DefaultPassword:string='masterkey'; DefaultCharSet:string='UTF8');
begin
  // First call regular constructor:
  Create;
  //... then override properties with what we specified:
  FDBCharset:=DefaultCharset;
  FDBHost:=DefaultHost;
  FDBPassword:=DefaultPassword;
  FDBPath:=DefaultPath;
  FDBType:=DefaultType;
  FDBUser:=DefaultUser;
end;

destructor TDBConnectionConfig.Destroy;
begin
  inherited Destroy;
end;

end.

