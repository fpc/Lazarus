unit IdeDebuggerWatchResultJSon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdeDebuggerWatchResult, DbgIntfBaseTypes, fpjson,
  jsonparser, jsonscanner;

type

  { TWatchResultDataJSonBase }

  TWatchResultDataJSonBase = class(TWatchResultDataString)
  private
    FInternalJSon: TJSONData;
    FIndex: Integer;
    FCurData: TWatchResultDataJSonBase;
    FJsonAddressKey: String;
    FJsonTypenameKey: String;

    function JSon: TJSONData; inline;
  protected
    function GetAsString: String; override;
    function GetDataAddress: TDBGPtr; override;
    function GetHasDataAddress: Boolean; override;
    function GetTypeName: String; override;

    // arary
    function  GetCount: Integer; override;
    function  GetLength: Integer; override;
    function GetLowBound: Int64; override;
    function  GetSelectedEntry: TWatchResultData; override;

    // struct
    function GetFieldCount: Integer; override;
    function GetFields(AnIndex: Integer): TWatchResultDataFieldInfo; override;
  public
    destructor Destroy; override;
    procedure Assign(ASource: TWatchResultData; ATypeOnly: Boolean = False); override;
    procedure SetSelectedIndex(AnIndex: Integer); override;

    function HandleExpressionSuffix(ASuffix: String): TWatchResultData; override;

    property JsonAddressKey: String read FJsonAddressKey write FJsonAddressKey;
    property JsonTypenameKey: String read FJsonTypenameKey write FJsonTypenameKey;
  end;

  { TWatchResultDataJSon }

  TWatchResultDataJSon = class(TWatchResultDataJSonBase)
    destructor Destroy; override;
  end;

implementation

{ TWatchResultDataJSonBase }

function TWatchResultDataJSonBase.JSon: TJSONData;
Var
  P : TJSONParser;
begin
  if FInternalJSon = nil then
    try
      //FInternalJSon := GetJSON(AsString);
      P := TJSONParser.Create(AsString, [joUTF8,joComments,joIgnoreTrailingComma
        {$IF FPC_VERSION >= 030202} , joBOMCheck {$ENDIF}
        {,joIgnoreDuplicates}]);
      try
        FInternalJSon := P.Parse;
      finally
        P.Free;
      end;
    except
      FInternalJSon := nil;
    end;
  Result := FInternalJSon;
end;

procedure TWatchResultDataJSonBase.Assign(ASource: TWatchResultData;
  ATypeOnly: Boolean);
begin
  inherited Assign(ASource, ATypeOnly);
  if not (ASource is TWatchResultDataJSonBase) then
    exit;

  FIndex := TWatchResultDataJSonBase(ASource).FIndex;
  Self.Create(AsString);
end;

function TWatchResultDataJSonBase.GetAsString: String;
begin
  Result := inherited GetAsString;
  if (Result = '') and (FInternalJSon <> nil) then
    Result := FInternalJSon.AsJSON;
end;

function TWatchResultDataJSonBase.GetDataAddress: TDBGPtr;
var
  j: TJSONData;
begin
  j := JSon;
  if (FJsonAddressKey = '') or (j = nil) or not(j is TJSONObject)
  then
    exit(inherited GetDataAddress);

  try
    j := TJSONObject(j).Elements[FJsonAddressKey];
  except
    j := nil;
  end;

  if j = nil then
    exit(inherited GetDataAddress);

  if j is TJSONString then begin
    if not TryStrToQWord(j.AsString, Result) then
      Result := inherited GetDataAddress;
    exit;
  end;

  if ((j is TJSONFloatNumber)) or not(j is TJSONNumber) then
    exit(inherited GetDataAddress);

  if j is TJSONInt64Number then
    Result := TDBGPtr(j.AsInt64)
  else
    Result := j.AsQWord;
end;

function TWatchResultDataJSonBase.GetHasDataAddress: Boolean;
var
  j: TJSONData;
  d: QWord;
begin
  Result := inherited GetHasDataAddress;
  if Result then
    exit;

  j := JSon;
  Result := (FJsonAddressKey <> '') and (j <> nil) and (j is TJSONObject);
  if not Result then
    exit;

  try
    j := TJSONObject(j).Elements[FJsonAddressKey];
  except
    j := nil;
  end;
  if j = nil then
    exit(False);

  Result := ((j is TJSONNumber) and not (j is TJSONFloatNumber)) or
            ((j is TJSONString) and (TryStrToQWord(j.AsString, d)));
end;

function TWatchResultDataJSonBase.GetTypeName: String;
var
  j: TJSONData;
begin
  Result := '';
  j := JSon;
  if (FJsonTypenameKey = '') or (j = nil) or not(j is TJSONObject) then
    exit(inherited GetTypeName);

  try
    j := TJSONObject(j).Elements[FJsonTypenameKey];
  except
    j := nil;
  end;

  if (j = nil) or not(j is TJSONString) then
    exit(inherited GetTypeName);

  Result := j.AsString;
end;

function TWatchResultDataJSonBase.GetCount: Integer;
begin
  Result := 0;
  if (JSon = nil) or (JSon.JSONType <> jtArray) then
    exit;
  Result := JSon.Count;
end;

function TWatchResultDataJSonBase.GetLength: Integer;
begin
  Result := 0;
  if (JSon = nil) or (JSon.JSONType <> jtArray) then
    exit;
  Result := JSon.Count;
end;

procedure TWatchResultDataJSonBase.SetSelectedIndex(AnIndex: Integer);
begin
  if FIndex = AnIndex then
    exit;
  FIndex := AnIndex;
  if FCurData <> nil then
    FCurData.FInternalJSon := nil;
end;

function TWatchResultDataJSonBase.GetSelectedEntry: TWatchResultData;
begin
  if FCurData = nil then
    FCurData := TWatchResultDataJSonBase.Create('');
  if JSon <> nil then
    FCurData.FInternalJSon := JSon.Items[FIndex];

  TWatchResultDataJSon(FCurData).FJsonAddressKey := FJsonAddressKey;
  TWatchResultDataJSon(FCurData).FJsonTypenameKey := FJsonTypenameKey;

  Result := FCurData;
end;

function TWatchResultDataJSonBase.GetFieldCount: Integer;
begin
  Result := 0;
  if (JSon = nil) or (JSon.JSONType <> jtObject) or not(JSon is TJSONObject) then
    exit;
  Result := JSon.Count;
end;

function TWatchResultDataJSonBase.GetLowBound: Int64;
begin
  Result := 0;
end;

function TWatchResultDataJSonBase.GetFields(AnIndex: Integer
  ): TWatchResultDataFieldInfo;
begin
  FIndex := -1;
  Result := Default(TWatchResultDataFieldInfo);

  if FCurData = nil then
    FCurData := TWatchResultDataJSonBase.Create('');
  if (JSon <> nil) then begin
    FCurData.FInternalJSon := JSon.Items[AnIndex];
    Result.FieldName := TJSONObject(JSon).Names[AnIndex];
  end;

  TWatchResultDataJSon(FCurData).FJsonAddressKey := FJsonAddressKey;
  TWatchResultDataJSon(FCurData).FJsonTypenameKey := FJsonTypenameKey;

  Result.Field := FCurData;
  Result.Owner := Self;
end;

destructor TWatchResultDataJSonBase.Destroy;
begin
  inherited Destroy;
  FCurData.Free;
end;

function TWatchResultDataJSonBase.HandleExpressionSuffix(ASuffix: String
  ): TWatchResultData;
var
  SfxLen: SizeInt;
  Idx, Idx2, EndIdx: Integer;
  n: int64;
  NeedComma, InBracket: Boolean;
  js: TJSONData;
begin
  Result := Self;
  if ASuffix = '' then
    exit;

  NeedComma := False;
  InBracket := False;
  js := JSon;
  SfxLen := Length(ASuffix);
  Idx := 1;
  while (Idx <= SfxLen) and (js <> nil) do begin
    case ASuffix[Idx] of
      ' ', #9, #10, #13: begin
          inc(Idx);
        end;
      '{': begin
          if InBracket then
            break;
          NeedComma := False;
          InBracket := True;
          inc(Idx);
        end;
      '}': begin
          if not InBracket then
            break;
          NeedComma := False;
          InBracket := False;
          inc(Idx);
        end;
      ',': begin
          if not NeedComma then
            break;
          NeedComma := False;
          inc(Idx);
        end;
      '"': begin
          if NeedComma or not InBracket then
            break;

          EndIdx := Idx+1;
          if not(js is TJSONObject) then
            break;
          while (EndIdx<=SfxLen) and (ASuffix[EndIdx] <> '"') do
            inc(EndIdx);
          if (EndIdx > SfxLen) or (EndIdx-Idx <= 1) then
            break;

          try
            js := TJSONObject(js).Elements[copy(ASuffix, Idx+1, EndIdx-Idx-1)];
          except
            EndIdx := -1;
          end;
          if EndIdx < 0 then
            break;

          NeedComma := True;
          Idx := EndIdx+1;
        end;
      '$', '&', '%', '0'..'9': begin
          if NeedComma or not InBracket then
            break;

          Idx2 := Idx;
          if (Idx2 < SfxLen) and (ASuffix[Idx2+1] in ['x', 'X']) then
            inc(Idx2);
          EndIdx := Idx2+1;

          if ASuffix[Idx2] in ['$', 'x', 'X'] then begin
            while (EndIdx<=SfxLen) and (ASuffix[EndIdx] in ['0'..'9', 'a'..'f', 'A'..'F']) do
              inc(EndIdx);
          end
          else
          if ASuffix[Idx] = '&' then begin
            while (EndIdx<=SfxLen) and (ASuffix[EndIdx] in ['0'..'7']) do
              inc(EndIdx);
          end
          else
          if ASuffix[Idx] = '%' then begin
            while (EndIdx<=SfxLen) and (ASuffix[EndIdx] in ['0'..'1']) do
              inc(EndIdx);
          end
          else begin
            dec(Idx2);
            while (EndIdx<=SfxLen) and (ASuffix[EndIdx] in ['0'..'9']) do
              inc(EndIdx);
          end;

          if (EndIdx > SfxLen) or (EndIdx = Idx2 + 1) then
            break;
          if not TryStrToInt64(copy(ASuffix, Idx, EndIdx-Idx), n) then
            break;

          try
            js := js.Items[n];
          except
            EndIdx := -1;
          end;
          if EndIdx < 0 then
            break;

          NeedComma := True;
          Idx := EndIdx;
        end;

      otherwise
        break;
    end;
  end;

  if (Idx > SfxLen) and (js <> nil) and (js <> JSon) then begin
    Result := TWatchResultDataJSon.Create(js.AsJSON);
    TWatchResultDataJSon(Result).FJsonAddressKey := FJsonAddressKey;
    TWatchResultDataJSon(Result).FJsonTypenameKey := FJsonTypenameKey;
    exit;
  end;

  Result := TWatchResultDataError.Create('Can''t evaluate: ' + copy(ASuffix, Idx, SfxLen));
end;

destructor TWatchResultDataJSon.Destroy;
begin
  FInternalJSon.Free;
  inherited Destroy;
end;

end.

