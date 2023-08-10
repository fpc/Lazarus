unit fraparams;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, db, Grids;

type

  { TfraParams }

  TfraParams = class(TFrame)
    SGParams: TStringGrid;
    procedure SGParamsEditingDone(Sender: TObject);
    procedure SGParamsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
  private
    FParamHistory: TParams;
    FParams: TParams;
    procedure AddValueToHistory(P: TParam);
    procedure ApplyHistoryValues;
    procedure ApplyValue(P: TParam; aValue: UTF8String);
    procedure DisplayParams;
    procedure FillDataTypePicklist;
    procedure SetParams(AValue: TParams);

  public
    constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Property Params : TParams Read FParams Write SetParams;
    Property ParamHistory : TParams Read FParamHistory;
  end;

implementation

uses typinfo, fmtbcd;

const
  colName = 0;
  colType = 1;
  colNull = 2;
  colValue = 3;

  SupportedParams = [ftString, ftSmallint, ftInteger, ftWord,
    ftBoolean, ftFloat, ftCurrency, ftBCD, ftDate,  ftTime, ftDateTime,
    ftMemo, ftFixedChar, ftWideString, ftLargeint, ftVariant, ftGuid,
    ftTimeStamp, ftFMTBcd, ftFixedWideChar, ftWideMemo];


{$R *.lfm}

Function DataTypeToString (T : TFieldType) : String;

begin
  Str(T,Result);
  Delete(Result,1,2);
end;

Function StringToDataType (aValue : String) : TFieldType;

var
  I : Integer;

begin
  if (Length(aValue)>1) and ((Upcase(Avalue[1])<>'F') or (Upcase(Avalue[1])<>'T')) then
    aValue:='ft'+aValue;
  I:=GetEnumValue(TypeInfo(TFieldType),aValue);
  if I=-1 then
    Result:=ftUnknown
  else
    Result:=TFieldType(I);
end;

{ TfraParams }

function CompareParams(Item1, Item2: TCollectionItem): Integer;
begin
  Result:=CompareText(TParam(Item1).Name,TParam(Item2).Name);
end;

procedure TfraParams.SGParamsEditingDone(Sender: TObject);
begin

end;

procedure TfraParams.ApplyValue(P : TParam; aValue : UTF8String);

var
  T : TFieldType;

begin
  T:=P.DataType;
  Case T of
    ftString,
    ftMemo,
    ftFixedChar : P.AsString:=aValue;
    ftWideString,
    ftFixedWideChar,
    ftWideMemo : P.AsUnicodeString:=UTF8Decode(aValue);
    ftSmallint : P.AsSmallInt:=StrToInt(aValue);
    ftInteger : P.Asinteger:=StrToInt(aValue);
    ftWord  : P.AsWord:=StrToInt(aValue);
    ftBoolean : P.AsBoolean:=StrToBool(aValue);
    ftFloat : P.AsFloat:=StrToFloat(aValue);
    ftCurrency : P.AsCurrency:=StrToCurr(aValue);
    ftBCD : P.AsBCD:=StrToBCD(aValue);
    ftDate : P.AsDate:=StrToDate(aValue);
    ftTime : P.AsTime:=StrToTime(aValue);
    ftDateTime : P.AsDateTime:=StrToDateTime(aValue);
    ftLargeint : P.AsLargeInt:=StrToInt64(aValue);
    ftVariant : P.Value:=aValue;
    ftGuid : P.AsString:=aValue;
    ftTimeStamp : P.AsDate:=StrToDateTime(aValue);
    ftFMTBcd : P.AsFMTBCD:=StrToBCD(aValue);
  else
    // Not supported;
  end;
  // To make sure we have the correct type
  P.DataType:=T;
end;

procedure TfraParams.AddValueToHistory(P : TParam);

Var
  PHist : TParam;

begin
  PHist:=FParamHistory.FindParam(P.Name);
  if PHist=Nil then
    PHist:=(FParamHistory.Add as TParam);
  PHist.Assign(P);
end;

procedure TfraParams.SGParamsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);

Var
  P : TParam;
  T : TFieldType;

begin
  Dec(aRow);
  if (aRow>=0) and (aRow<FParams.Count) then
    P:=FParams[aRow]
  else
    Exit;
  T:=P.DataType;
  Case aCol of
    colName : ; // Cannot happen, read-only ?
    colType : P.DataType:=StringToDataType(Value) ;
    colNull : if Value='1' then
                begin
                P.Clear;
                P.DataType:=T;
                end;
    colValue : begin
               ApplyValue(P,Value);
               AddValueToHistory(P);
               SGParams.Cells[colNull,aRow+1]:=IntToStr(Ord(P.IsNull));
               end
  else
    // Should not happen either
  end;
end;

procedure TfraParams.ApplyHistoryValues;

Var
  Dest,Src: TParam;
  I : Integer;

begin
  For I:=0 to FParams.Count-1 do
    begin
    Dest:=FParams[i];
    Src:=FParamHistory.FindParam(Dest.Name);
    if Assigned(Src) then
      Dest.Assign(Src);
    end;
end;

procedure TfraParams.DisplayParams;

Var
  P : TParam;
  aRow : integer;
  S : String;

begin
  SGParams.RowCount:=FParams.Count+1;
  FParams.Sort(@CompareParams);
  aRow:=0;
  For P in FParams do
    begin
    Inc(aRow);
    SGParams.Cells[colName,aRow]:=P.Name;
    SGParams.Cells[colType,aRow]:=DataTypeToString(P.DataType);
    SGParams.Cells[colNull,aRow]:=IntToStr(Ord(P.IsNull));
    if not P.IsNull then
      S:=P.Value
    else
      S:='';
    SGParams.Cells[colValue,aRow]:=S;
    end;
end;

procedure TfraParams.SetParams(AValue: TParams);
begin
  if FParams=AValue then Exit;
  FParams.Assign(AValue);
  ApplyHistoryValues;
  DisplayParams;
end;

Procedure TfraParams.FillDataTypePicklist;

var
  L : TStrings;
  T : TFieldType;

begin
  L:=SGParams.Columns[colType].PickList;
  For T in TFieldType do
    if T in SupportedParams then
      L.Add(DataTypeToString(T));
end;

constructor TfraParams.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FParams:=TParams.Create(Self);
  FParamHistory:=TParams.Create(Self);
  FillDataTypePicklist;
end;

destructor TfraParams.Destroy;
begin
  FreeAndNil(FParams);
  FreeAndNil(FParamHistory);
  inherited Destroy;
end;



end.

