unit IdeDebuggerFpDbgValueConv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Laz2_XMLCfg, FpDebugValueConvertors;

type

  { TIdeFpDbgConverterConfig }

  TIdeFpDbgConverterConfig = class(TFpDbgConverterConfig)
  private
    FEnabled: Boolean;
    FName: String;
  public
    procedure Assign(ASource: TFpDbgConverterConfig); override;
    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Name: String read FName write FName;
    property MatchKinds;
  end;

  { TIdeFpDbgConverterConfigList }

  TIdeFpDbgConverterConfigList = class(TFpDbgConverterConfigList)
  private
    FChanged: Boolean;
    function GetIdeItems(Index: Integer): TIdeFpDbgConverterConfig;
    procedure PutIdeItems(Index: Integer; AValue: TIdeFpDbgConverterConfig);
  public
    procedure AssignEnabledTo(ADest: TFpDbgConverterConfigList);

    procedure LoadDataFromXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);
    procedure SaveDataToXMLConfig(const AConfig: TRttiXMLConfig; const APath: string);

    function IdeItemByName(AName: String): TIdeFpDbgConverterConfig;

    property IdeItems[Index: Integer]: TIdeFpDbgConverterConfig read GetIdeItems write PutIdeItems; default;
    property Changed: Boolean read FChanged write FChanged;
  end;


implementation

{ TIdeFpDbgConverterConfig }

procedure TIdeFpDbgConverterConfig.Assign(ASource: TFpDbgConverterConfig);
var
  Src: TIdeFpDbgConverterConfig absolute ASource;
begin
  inherited Assign(ASource);
  if ASource is TIdeFpDbgConverterConfig then begin
    FName := Src.FName;
    FEnabled := Src.FEnabled;
  end;
end;

procedure TIdeFpDbgConverterConfig.LoadDataFromXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  s: String;
  c: TFpDbgValueConverterClass;
  obj: TFpDbgValueConverter;
begin
  AConfig.ReadObject(APath + 'Filter/', Self);
  MatchTypeNames.CommaText := AConfig.GetValue(APath + 'Filter/MatchTypeNames', '');

  s := AConfig.GetValue(APath + 'ConvClass', '');
  c := ValueConverterClassList.FindByClassName(s);
  if c = nil then
    exit;

  obj := c.Create;
  AConfig.ReadObject(APath + 'Conv/', obj);
  Converter := obj;
end;

procedure TIdeFpDbgConverterConfig.SaveDataToXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
begin
  AConfig.WriteObject(APath + 'Filter/', Self);
  AConfig.SetDeleteValue(APath + 'Filter/MatchTypeNames', MatchTypeNames.CommaText, '');

  AConfig.SetValue(APath + 'ConvClass', Converter.ClassName);
  AConfig.WriteObject(APath + 'Conv/', Converter);
end;

{ TIdeFpDbgConverterConfigList }

function TIdeFpDbgConverterConfigList.GetIdeItems(Index: Integer
  ): TIdeFpDbgConverterConfig;
begin
  Result := TIdeFpDbgConverterConfig(Items[Index]);
  assert(Result is TIdeFpDbgConverterConfig, 'TIdeFpDbgConverterConfigList.GetIdeItems: Result is TIdeFpDbgConverterConfig');
end;

procedure TIdeFpDbgConverterConfigList.PutIdeItems(Index: Integer;
  AValue: TIdeFpDbgConverterConfig);
begin
  Items[Index] := AValue;
end;

procedure TIdeFpDbgConverterConfigList.AssignEnabledTo(
  ADest: TFpDbgConverterConfigList);
var
  i: Integer;
begin
  ADest.Clear;
  for i := 0 to Count - 1 do
    if IdeItems[i].Enabled then
      ADest.Add(Items[i].CreateCopy);
end;

procedure TIdeFpDbgConverterConfigList.LoadDataFromXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  i, c: Integer;
  obj: TIdeFpDbgConverterConfig;
begin
  clear;
  c := AConfig.GetChildCount(APath);
  for i := 0 to c - 1 do begin
    obj := TIdeFpDbgConverterConfig.Create(nil);
    obj.LoadDataFromXMLConfig(AConfig, APath + 'Entry[' + IntToStr(i+1) + ']/');
    if obj.Converter <> nil then
      Add(obj)
    else
      obj.Free;
  end
end;

procedure TIdeFpDbgConverterConfigList.SaveDataToXMLConfig(
  const AConfig: TRttiXMLConfig; const APath: string);
var
  i: Integer;
begin
  AConfig.DeletePath(APath);
  for i := 0 to Count - 1 do
    IdeItems[i].SaveDataToXMLConfig(AConfig, APath + 'Entry[' + IntToStr(i+1) + ']/');
end;

function TIdeFpDbgConverterConfigList.IdeItemByName(AName: String
  ): TIdeFpDbgConverterConfig;
var
  i: Integer;
begin
  Result := nil;
  i := Count - 1;
  while (i >= 0) and (IdeItems[i].Name <> AName) do
    dec(i);
  if i >= 0 then
    Result := IdeItems[i];
end;

end.

