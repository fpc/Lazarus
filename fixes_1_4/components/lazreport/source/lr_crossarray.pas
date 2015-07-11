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
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit lr_CrossArray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB;

type

  { TExItem }

  TExItem = class
  private
    FCelCol:string;
    FValue:Variant;
    FDataset: TDataset;
    FBookmark:TBookMark;
  public
    procedure SaveBookmark(Ds: TDataset);
    function IsBookmarkValid: boolean;
    destructor destroy; override;
  end;

type

  { TExRow }

  TExRow = class(TFPList)
  private
    FRow:string;
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
    FColHeader:TStringList;
    FRowHeader:TStringList;
    function GetCell(ACol, ARow: variant): variant;
    function GetCellData(ACol, ARow : variant): TExItem;
    function GetColCount: integer;
    function GetColHeader(ACol: integer): string;
    function GetRowCount: integer;
    function GetRowHeader(ARow: integer): string;
    procedure SetCell(ACol, ARow: variant; AValue: variant);
    function Find(ARow:variant; out Index: Integer): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Cell[ACol, ARow : variant]:variant read GetCell write SetCell;default;
    property CellData[ACol, ARow : variant]:TExItem read GetCellData;
    property ColCount:integer read GetColCount;
    property RowCount:integer read GetRowCount;
    property ColHeader[ACol:integer]:string read GetColHeader;
    property RowHeader[ARow:integer]:string read GetRowHeader;
  end;

implementation
uses math, variants;

{ TExItem }

procedure TExItem.SaveBookmark(Ds: TDataset);
begin
  if IsBookmarkValid then
    FDataset.FreeBookmark(FBookmark);
  FDataset := Ds;
  FBookmark := FDataset.GetBookmark;
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
  S1, S2:string;
begin
  Result := false;
  // Use binary search.
  L := 0;
  R := Count - 1;
  while L<=R do
  begin
    I := (L+R) div 2;
//    Dir := CompareStr(TExItem(Items[i]).FCelCol, VarToStr(ACol));
    S1:=TExItem(Items[i]).FCelCol;
    S2:=VarToStr(ACol);
    Dir := CompareStr(S1, S2);
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

function TExVarArray.GetColHeader(ACol: integer): string;
begin
  if (ACol>=0) and (ACol<FColHeader.Count) then
    Result:=FColHeader[ACol]
  else
    Result:='';
end;

function TExVarArray.GetRowCount: integer;
begin
  Result:=FRowHeader.Count;
end;

function TExVarArray.GetRowHeader(ARow: integer): string;
begin
  if (ARow>=0) and (ARow<FRowHeader.Count) then
    Result:=FRowHeader[ARow]
  else
    Result:='';
end;

procedure TExVarArray.SetCell(ACol, ARow: variant; AValue: variant);
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

  i:=FColHeader.IndexOf(VarToStr(ACol));
  if i<0 then
    FColHeader.Add(VarToStr(ACol));

  i:=FRowHeader.IndexOf(VarToStr(ARow));
  if i<0 then
    FRowHeader.Add(VarToStr(ARow));

end;

function TExVarArray.Find(ARow: variant; out Index: Integer): Boolean;
var
  I,L,R,Dir: Integer;
  S1, S2:string;
begin
  Result := false;
  // Use binary search.
  L := 0;
  R := FRows.Count - 1;
  S2:=VarToStr(ARow);
  while L<=R do
  begin
    I := (L+R) div 2;
//    Dir := CompareStr(TExRow(FRows[i]).FRow, VarToStr(ARow));
    S1:=TExRow(FRows[i]).FRow;
    Dir := CompareStr(S1, S2);
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
  FColHeader:=TStringList.Create;
  FColHeader.Sorted:=true;
  FRowHeader:=TStringList.Create;
  FRowHeader.Sorted:=true;
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

