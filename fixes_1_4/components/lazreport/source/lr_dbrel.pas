
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             DB related stuff            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_DBRel;

interface

{$I LR_Vers.inc}

uses
  SysUtils, Classes, DB;

const
  frEmptyBookmark = nil;

type
  { TODO -oalexs : Remove this }
  TfrBookmark = TBookmark;
  TfrTDataSet =class(TDataSet);
  TfrTField = class(TField);
  TfrTBlobField = class(TBlobField);

function frIsBlob(Field: TfrTField): Boolean;
function frIsBookmarksEqual(DataSet: TfrTDataSet; b1, b2: TfrBookmark): Boolean;
procedure frGetFieldNames(DataSet: TfrTDataSet; List: TStrings);
function frGetBookmark(DataSet: TfrTDataSet): TfrBookmark;
procedure frFreeBookmark(DataSet: TfrTDataSet; Bookmark: TfrBookmark);
procedure frGotoBookmark(DataSet: TfrTDataSet; Bookmark: TfrBookmark);
function frGetDataSource(Owner: TComponent; d: TDataSet): TDataSource;

function lrGetFieldValue(F:TField):Variant;

const
  TypeStringField = [ftString, ftMemo, ftFmtMemo, ftFixedChar, ftWideString,
    ftFixedWideChar, ftWideMemo];

  TypeNumericField = [ftFloat, ftCurrency, ftBCD, ftDate,  ftTime, ftDateTime,
    ftTimeStamp];

  TypeIntegerField = [ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint];

  TypeBooleanField = [ftBoolean];

implementation
uses LR_Utils;

function frIsBlob(Field: TfrTField): Boolean;
begin
  Result := (Field <> nil) and (Field.DataType in [ftBlob..ftTypedBinary]);
end;

procedure frGetFieldNames(DataSet: TfrTDataSet; List: TStrings);
begin
  if not Assigned(DataSet) then exit;
  if DataSet.FieldCount > 0 then
    DataSet.GetFieldNames(List)
  else
  begin
{    DataSet.Open;
    DataSet.GetFieldNames(List);
    DataSet.Close;}
    DataSet.FieldDefs.Update;
    DataSet.FieldDefs.GetItemNames(List);
  end;
end;

function frGetBookmark(DataSet: TfrTDataSet): TfrBookmark;
begin
  Result := DataSet.GetBookmark;
end;

procedure frGotoBookmark(DataSet: TfrTDataSet; Bookmark: TfrBookmark);
begin
  DataSet.GotoBookmark(BookMark);
end;

function frGetDataSource(Owner: TComponent; d: TDataSet): TDataSource;
var
  i: Integer;
  sl: TStringList;
  ds: TDataSource;
begin
  sl := TStringList.Create;
  Result := nil;
  frGetComponents(Owner, TDataSource, sl, nil);
  for i := 0 to sl.Count - 1 do
  begin
    ds := frFindComponent(Owner, sl[i]) as TDataSource;
    if (ds <> nil) and (ds.DataSet = d) then
    begin
      Result := ds;
      break;
    end;
  end;
  sl.Free;
end;

function lrGetFieldValue(F: TField): Variant;
begin
  if Assigned(F) then
  begin
    if F.IsNull then
    begin
      if F.DataType in TypeStringField then
        Result:=''
      else
      if F.DataType in (TypeIntegerField + TypeNumericField) then
        Result:=0
      else
      if F.DataType in TypeBooleanField then
        Result:=false
      else
        Result:=null
    end
    else
      Result:=F.Value;
  end
  else
    Result:=null;
end;

procedure frFreeBookmark(DataSet: TfrTDataSet; Bookmark: TfrBookmark);
begin
  DataSet.FreeBookmark(BookMark);
end;

function frIsBookmarksEqual(DataSet: TfrTDataSet; b1, b2: TfrBookmark): Boolean;
begin
  Result := DataSet.CompareBookmarks(b1, b2) = 0;
end;

end.
