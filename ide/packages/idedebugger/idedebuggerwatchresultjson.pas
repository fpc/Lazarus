unit IdeDebuggerWatchResultJSon;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IdeDebuggerWatchResult, fpjson, jsonparser, jsonscanner;

type

  { TWatchResultDataJSonBase }

  TWatchResultDataJSonBase = class(TWatchResultDataPrePrinted)
  private
    FInternalJSon: TJSONData;
    FIndex: Integer;
    FCurData: TWatchResultDataJSonBase;

    function JSon: TJSONData; inline;
  protected
    function GetAsString: String; override;

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
      P := TJSONParser.Create(AsString, [joUTF8,joComments,joIgnoreTrailingComma,joBOMCheck{,joIgnoreDuplicates}]);
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
  if FInternalJSon <> nil then
    Self.Create(FInternalJSon.AsJSON);
end;

function TWatchResultDataJSonBase.GetAsString: String;
begin
  Result := inherited GetAsString;
  if (Result = '') and (FInternalJSon <> nil) then
    Result := FInternalJSon.AsJSON;
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

  if (Idx > SfxLen) and (js <> nil) then begin
    Result := TWatchResultDataJSon.Create(js.AsJSON);
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

