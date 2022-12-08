unit ToolBarOptionsBase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg;

const
  IDEToolBarConfigVersion = 1;  // File version in configuration.

type

  { TIDEToolBarOptionsBase }

  TIDEToolBarOptionsBase = class
  private
    FButtonNames: TStringList;
  protected
    procedure LoadButtonNames(XMLConfig: TXMLConfig; SubPath: String);
    procedure SaveButtonNames(XMLConfig: TXMLConfig; SubPath: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Equals(Opts: TIDEToolBarOptionsBase): boolean; overload;
    procedure Assign(Source: TIDEToolBarOptionsBase);
    //procedure Load(XMLConfig: TXMLConfig; Path: String);
    //procedure Save(XMLConfig: TXMLConfig; Path: String);
  published
    property ButtonNames: TStringList read FButtonNames; // write FButtonNames;
  end;

const
  cIDEToolbarDivider = '---------------';
  cTailItemCaption = '                                             ';

implementation

{ TIDEToolBarOptionsBase }

constructor TIDEToolBarOptionsBase.Create;
begin
  FButtonNames := TStringList.Create;
end;

destructor TIDEToolBarOptionsBase.Destroy;
begin
  FButtonNames.Free;
  inherited Destroy;
end;

procedure TIDEToolBarOptionsBase.Clear;
begin
  FButtonNames.Clear;
end;

function TIDEToolBarOptionsBase.Equals(Opts: TIDEToolBarOptionsBase): boolean;
begin
  Result := FButtonNames.Equals(Opts.FButtonNames);
end;

procedure TIDEToolBarOptionsBase.Assign(Source: TIDEToolBarOptionsBase);
begin
  FButtonNames.Assign(Source.FButtonNames);
end;

procedure TIDEToolBarOptionsBase.LoadButtonNames(XMLConfig: TXMLConfig; SubPath: String);
var
  ButtonCount: Integer;
  ButtonName: string;
  I, FileVersion: Integer;
begin
  FileVersion := XMLConfig.GetValue(SubPath + 'Version', 0);
  ButtonCount := XMLConfig.GetValue(SubPath + 'Count', 0);
  if (FileVersion < 1) and (ButtonCount = 0) then  // Old format
    ButtonCount := XMLConfig.GetValue(SubPath + 'ButtonCount/Value', 0);
  for I := 1 to ButtonCount do
  begin
    ButtonName := XMLConfig.GetValue(SubPath + 'Button' + IntToStr(I) + '/Name', '');
    if (FileVersion < 1) and (ButtonName = '') then  // Old format
      ButtonName := XMLConfig.GetValue(SubPath + 'Buttons/Name' + IntToStr(I) + '/Value', '');
    if ButtonName <> '' then
      ButtonNames.Add(ButtonName);
  end;
end;

procedure TIDEToolBarOptionsBase.SaveButtonNames(XMLConfig: TXMLConfig; SubPath: String);
var
  I: Integer;
begin
  XMLConfig.SetValue(SubPath + 'Version', IDEToolBarConfigVersion);
  XMLConfig.SetDeleteValue(SubPath + 'Count', ButtonNames.Count, 0);
  for I := 0 to ButtonNames.Count-1 do
    XMLConfig.SetDeleteValue(SubPath + 'Button' + IntToStr(I+1) + '/Name', ButtonNames[I], '');
end;

end.

