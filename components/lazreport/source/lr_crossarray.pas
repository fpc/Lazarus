{ LazReport cross-tab control

  Copyright (C) 2014 alexs alexs75.at.yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit lr_CrossArray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

type

  TVariantArray = array of Variant;

  { TVariantList }

  TVariantList = class
  private
    FItems:TVariantArray;
    FCount: integer;
    function GetItems(AIndex: integer): Variant;
    procedure SetItems(AIndex: integer; AValue: Variant);
  public
    constructor Create;
    destructor Destroy; override;
    function Insert(AValue:Variant):integer;
    function Find(AValue:Variant; out Index: Integer): Boolean;
    function AsString(AIndex: Integer):string;
    procedure Clear;
    property Count:integer read FCount;
    property Items[AIndex:integer]:Variant read GetItems write SetItems;
  end;

  { TExItem }

  TExItem = class
  private
    FCelCol:Variant;
    FValue:Variant;
    FDataset: TDataset;
    FBookmark:TBookMark;
  public
    procedure SaveBookmark(Ds: TDataset);
    procedure GotoBookmark;
    function IsBookmarkValid: boolean;
    destructor destroy; override;
  end;

type

  { TExRow }

  TExRow = class(TFPList)
  private
    FRow:Variant;
    function GetCell(ACol: Variant): Variant;
    function GetCellData(ACol: Variant): TExItem;
    procedure SetCell(ACol: Variant; AValue: Variant);
    function Find(ACol:Variant; out Index: Integer): Boolean;
  public
    destructor Destroy; override;
    property Cell[ACol:Variant]:Variant read GetCell write SetCell; default;
    property CellData[ACol:Variant]:TExItem read GetCellData;
  end;

  { TExVarArray }

  TExVarArray = class
  private
    FColCount: integer;
    FRowCount: integer;
    FRows:TFPList;
    FRowHeader:TVariantList;
    FColHeader:TVariantList;
    function GetCell(ACol, ARow: variant): variant;
    function GetCellData(ACol, ARow : variant): TExItem;
    function GetColCount: integer;
    function GetColHeader(ACol: integer): Variant;
    function GetRowCount: integer;
    function GetRowHeader(ARow: integer): Variant;
    procedure SetCell(ACol, ARow: variant; AValue: Variant);
    function Find(ARow:variant; out Index: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Cell[ACol, ARow : variant]:variant read GetCell write SetCell;default;
    property CellData[ACol, ARow : variant]:TExItem read GetCellData;
    property ColCount:integer read GetColCount;
    property RowCount:integer read GetRowCount;
    property ColHeader[ACol:integer]:Variant read GetColHeader;
    property RowHeader[ARow:integer]:Variant read GetRowHeader;
  end;

implementation
uses math, variants;

{ TExItem }

function CompareVariant(AVal1, AVAl2:Variant):integer;
begin
  if AVal1>AVAl2 then
    Result := 1
  else
  if AVal1<AVAl2 then
    Result := -1
  else
    Result :=0;
end;

{ TVariantList }

function TVariantList.GetItems(AIndex: integer): Variant;
begin
  if (AIndex>=0) and (AIndex < Count) then
    Result:=FItems[AIndex]
  else
    raise Exception.CreateFmt('Index % out of bounds %d:%d', [AIndex, 0, Count-1]);
end;

procedure TVariantList.SetItems(AIndex: integer; AValue: Variant);
begin
  if (AIndex>=0) and (AIndex < Count) then
    FItems[AIndex]:=AValue
  else
    raise Exception.CreateFmt('Index % out of bounds %d:%d', [AIndex, 0, Count-1]);
end;

constructor TVariantList.Create;
begin
  inherited Create;
  SetLength(FItems, 10);
  FCount:=0;
end;

destructor TVariantList.Destroy;
begin
  Clear;
  SetLength(FItems, 0);
  inherited Destroy;
end;

function TVariantList.Insert(AValue: Variant): integer;
var
  FIndex: Integer;
  i: Integer;
begin
  if Length(FItems) = FCount  then
    SetLength(FItems, FCount + 100);

  if not Find(AValue, FIndex) then
  begin
    for i:=FCount-1 downto FIndex do
      FItems[i+1]:=FItems[i];
    FItems[FIndex]:=AValue;
    Inc(FCount);
  end;
end;

function TVariantList.Find(AValue: Variant; out Index: Integer): Boolean;
var
  L: Integer;
  R: Integer;
  I: Integer;
  Dir: Integer;
begin
  Result := false;
  // Use binary search.
  L := 0;
  R := Count - 1;
  while L<=R do
  begin
    I := (L+R) div 2;
    Dir := CompareVariant(FItems[i], AValue);
    if Dir < 0 then
      L := I+1
    else
    begin
      R := I-1;
      if Dir = 0 then
      begin
        Result := true;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TVariantList.AsString(AIndex: Integer): string;
begin
  Result:=VarToStr(GetItems(AIndex));
end;

procedure TVariantList.Clear;
var
  i: Integer;
begin
  for i:=0 to FCount-1 do
   FItems[i]:=null;
  FCount:=0;
end;

procedure TExItem.SaveBookmark(Ds: TDataset);
begin
  if IsBookmarkValid then
    FDataset.FreeBookmark(FBookmark);
  FDataset := Ds;
  FBookmark := FDataset.GetBookmark;
end;

procedure TExItem.GotoBookmark;
begin
  FDataset.GotoBookmark(FBookmark);
end;

function TExItem.IsBookmarkValid: boolean;
begin
  result := (FDataset<>nil) and FDataset.BookmarkValid(FBookmark)
end;

destructor TExItem.destroy;
begin
  if IsBookmarkValid then
    FDataset.FreeBookmark(FBookmark);
  inherited destroy;
end;

{ TExRow }

function TExRow.GetCell(ACol: Variant): Variant;
var
  i:integer;
begin
  if Find(ACol, i) then
    Result:=TExItem(Items[i]).FValue
  else
    Result:=null;
end;

function TExRow.GetCellData(ACol: Variant): TExItem;
var
  i:integer;
begin
  if Find(ACol, i) then
    Result:=TExItem(Items[i])
  else
    Result:=nil;
end;

procedure TExRow.SetCell(ACol: Variant; AValue: Variant);
var
  R:TExItem;
  i:integer;
begin
  if Find(ACol, i) then
    TExItem(Items[i]).FValue:=AValue
  else
  begin
    R:=TExItem.Create;
    R.FValue:=AValue;
    R.FCelCol:=ACol;
    Insert(i, R);
  end;
end;

function TExRow.Find(ACol: Variant; out Index: Integer): Boolean;
var
  I,L,R,Dir: Integer;
begin
  Result := false;
  // Use binary search.
  L := 0;
  R := Count - 1;
  while L<=R do
  begin
    I := (L+R) div 2;
    Dir := CompareVariant(TExItem(Items[i]).FCelCol,ACol);
    if Dir < 0 then
      L := I+1
    else
    begin
      R := I-1;
      if Dir = 0 then
      begin
        Result := true;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

destructor TExRow.Destroy;
var
  i: Integer;
begin
  for i:=0 to Count-1 do
  begin
    TExItem(Items[i]).Free;
    Items[i]:=nil;
  end;
  inherited Destroy;
end;

{ TExVarArray }

function TExVarArray.GetCell(ACol, ARow: variant): variant;
var
  i:integer;
begin
  if Find(ARow, i) then
    Result:=TExRow(FRows[i]).Cell[ACol]
  else
    Result:=null;
end;

function TExVarArray.GetCellData(ACol, ARow: variant): TExItem;
var
  i:integer;
begin
  if Find(ARow, i) then
    Result:=TExRow(FRows[i]).CellData[ACol]
  else
    Result:=nil;
end;

function TExVarArray.GetColCount: integer;
begin
  Result:=FColHeader.Count;
end;

function TExVarArray.GetColHeader(ACol: integer): Variant;
begin
  if (ACol>=0) and (ACol<FColHeader.Count) then
    Result:=FColHeader.Items[ACol]
  else
    Result:=null;
end;

function TExVarArray.GetRowCount: integer;
begin
  Result:=FRowHeader.Count;
end;

function TExVarArray.GetRowHeader(ARow: integer): Variant;
begin
  if (ARow>=0) and (ARow<FRowHeader.Count) then
    Result:=FRowHeader.Items[ARow]
  else
    Result:=null;
end;

procedure TExVarArray.SetCell(ACol, ARow: variant; AValue: Variant);
var
  R:TExRow;
  i:integer;
begin
  if Find(ARow, i) then
    R:=TExRow(FRows[i])
  else
  begin
    R:=TExRow.Create;
    R.FRow:=ARow;
    FRows.Insert(i, R);
  end;
  R.Cell[ACol]:=AValue;
  FRowCount:=Max(FRowCount, FRows.Count);
  FColCount:=Max(FColCount, R.Count);

  if not FColHeader.Find(ACol, i) then
    FColHeader.Insert(ACol);

  if not FRowHeader.Find(ARow, i) then
    FRowHeader.Insert(ARow);
end;

function TExVarArray.Find(ARow: variant; out Index: Integer): Boolean;
var
  I,L,R,Dir: Integer;
begin
  Result := false;
  // Use binary search.
  L := 0;
  R := FRows.Count - 1;
  while L<=R do
  begin
    I := (L+R) div 2;
    Dir := CompareVariant(TExRow(FRows[i]).FRow, ARow);
    if Dir < 0 then
      L := I+1
    else
    begin
      R := I-1;
      if Dir = 0 then
      begin
        Result := true;
        L := I;
      end;
    end;
  end;
  Index := L;
end;


constructor TExVarArray.Create;
begin
  inherited Create;
  FRows:=TFPList.Create;
  FColHeader:=TVariantList.Create;
  FRowHeader:=TVariantList.Create;
end;

destructor TExVarArray.Destroy;
var
  i: Integer;
begin
  for i:=0 to FRows.Count - 1 do
  begin
    TExRow(FRows.Items[i]).Free;
    FRows.Items[i]:=nil;
  end;
  FRows.Free;
  FreeAndNil(FColHeader);
  FreeAndNil(FRowHeader);
  inherited Destroy;
end;

procedure TExVarArray.Clear;
var
  i: Integer;
begin
  FColHeader.Clear;
  FRowHeader.Clear;

  for i:=0 to FRows.Count - 1 do
  begin
    TExRow(FRows.Items[i]).Free;
    FRows.Items[i]:=nil;
  end;
  FRows.Clear;
end;

end.

