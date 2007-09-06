
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


implementation

function frIsBlob(Field: TfrTField): Boolean;
begin
  Result := (Field <> nil) and (Field.DataType in [ftBlob..ftTypedBinary]);
end;

procedure frGetFieldNames(DataSet: TfrTDataSet; List: TStrings);
begin
  DataSet.GetFieldNames(List);
end;

function frGetBookmark(DataSet: TfrTDataSet): TfrBookmark;
begin
  Result := DataSet.GetBookmark;
end;

procedure frGotoBookmark(DataSet: TfrTDataSet; Bookmark: TfrBookmark);
begin
  DataSet.GotoBookmark(BookMark);
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
