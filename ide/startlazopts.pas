unit StartLazOpts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  XMLCfg;
  
type
  TStartLazarusOptions = class
  private
    FFilename: string;
    FLazarusDir: string;
    procedure SetFilename(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
    property LazarusDir: string read FLazarusDir write FLazarusDir;
    property Filename: string read FFilename write SetFilename;
  end;

implementation

{ TStartLazarusOptions }

procedure TStartLazarusOptions.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

constructor TStartLazarusOptions.Create;
begin
  FLazarusDir := ExtractFilePath(ExpandFileName(ParamStr(0)));
end;

destructor TStartLazarusOptions.Destroy;
begin
  inherited Destroy;
end;

procedure TStartLazarusOptions.Load;
begin

end;

procedure TStartLazarusOptions.Save;
begin

end;

end.

