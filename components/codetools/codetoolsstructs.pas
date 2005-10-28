{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Most codetools returns simple values like a single code position or a
    string. But some creates lists of data.
    This unit provides structures for complex results.
  
    TCodeXYPositions - a list of PCodeXYPosition

}
unit CodeToolsStructs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CodeCache, CodeAtom;
  
type
  TResourcestringInsertPolicy = (
    rsipNone,          // do not add/insert
    rsipAppend,        // append at end
    rsipAlphabetically,// insert alphabetically
    rsipContext        // insert context sensitive
    );

  TPascalClassSection = (
    pcsPrivate,
    pcsProtected,
    pcsPublic,
    pcsPublished
    );

  { TCodeXYPositions - a list of PCodeXYPosition }

  TCodeXYPositions = class
  private
    FItems: TFPList; // list of PCodeXYPosition, can be nil
    function GetCaretsXY(Index: integer): TPoint;
    function GetCodes(Index: integer): TCodeBuffer;
    function GetItems(Index: integer): PCodeXYPosition;
    procedure SetCaretsXY(Index: integer; const AValue: TPoint);
    procedure SetCodes(Index: integer; const AValue: TCodeBuffer);
    procedure SetItems(Index: integer; const AValue: PCodeXYPosition);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(const Position: TCodeXYPosition): integer;
    function Add(X,Y: integer; Code: TCodeBuffer): integer;
    procedure Assign(Source: TCodeXYPositions);
    function IsEqual(Source: TCodeXYPositions): boolean;
    function Count: integer;
    procedure Delete(Index: integer);
    function CreateCopy: TCodeXYPositions;
  public
    property Items[Index: integer]: PCodeXYPosition
                                          read GetItems write SetItems; default;
    property CaretsXY[Index: integer]: TPoint read GetCaretsXY write SetCaretsXY;
    property Codes[Index: integer]: TCodeBuffer read GetCodes write SetCodes;
  end;
  
const
  PascalClassSectionKeywords: array[TPascalClassSection] of string = (
    'private',
    'protected',
    'public',
    'published'
    );
    

implementation


{ TCodeXYPositions }

function TCodeXYPositions.GetItems(Index: integer): PCodeXYPosition;
begin
  Result:=PCodeXYPosition(FItems[Index]);
end;

function TCodeXYPositions.GetCaretsXY(Index: integer): TPoint;
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Result:=Point(Item^.X,Item^.Y);
end;

function TCodeXYPositions.GetCodes(Index: integer): TCodeBuffer;
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Result:=Item^.Code;
end;

procedure TCodeXYPositions.SetCaretsXY(Index: integer; const AValue: TPoint);
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Item^.X:=AValue.X;
  Item^.Y:=AValue.Y;
end;

procedure TCodeXYPositions.SetCodes(Index: integer; const AValue: TCodeBuffer);
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Item^.Code:=AValue;
end;

procedure TCodeXYPositions.SetItems(Index: integer;
  const AValue: PCodeXYPosition);
begin
  FItems[Index]:=AValue;
end;

constructor TCodeXYPositions.Create;
begin

end;

destructor TCodeXYPositions.Destroy;
begin
  Clear;
  FItems.Free;
  FItems:=nil;
  inherited Destroy;
end;

procedure TCodeXYPositions.Clear;
var
  i: Integer;
  Item: PCodeXYPosition;
begin
  if FItems<>nil then begin
    for i:=0 to FItems.Count-1 do begin
      Item:=Items[i];
      Dispose(Item);
    end;
    FItems.Clear;
  end;
end;

function TCodeXYPositions.Add(const Position: TCodeXYPosition): integer;
var
  NewItem: PCodeXYPosition;
begin
  New(NewItem);
  NewItem^:=Position;
  if FItems=nil then FItems:=TFPList.Create;
  Result:=FItems.Add(NewItem);
end;

function TCodeXYPositions.Add(X, Y: integer; Code: TCodeBuffer): integer;
var
  NewItem: TCodeXYPosition;
begin
  NewItem.X:=X;
  NewItem.Y:=Y;
  NewItem.Code:=Code;
  Result:=Add(NewItem);
end;

procedure TCodeXYPositions.Assign(Source: TCodeXYPositions);
var
  i: Integer;
begin
  if IsEqual(Source) then exit;
  Clear;
  for i:=0 to Source.Count-1 do
    Add(Source[i]^);
end;

function TCodeXYPositions.IsEqual(Source: TCodeXYPositions): boolean;
var
  SrcItem: TCodeXYPosition;
  CurItem: TCodeXYPosition;
  i: Integer;
begin
  if Source=Self then
    Result:=true
  else if (Source=nil) or (Source.Count<>Count) then
    Result:=false
  else begin
    for i:=0 to Count-1 do begin
      SrcItem:=Source[i]^;
      CurItem:=Items[i]^;
      if (SrcItem.X<>CurItem.X)
      or (SrcItem.Y<>CurItem.Y)
      or (SrcItem.Code<>CurItem.Code)
      then begin
        Result:=false;
        exit;
      end;
    end;
    Result:=true;
  end;
end;

function TCodeXYPositions.Count: integer;
begin
  if FItems<>nil then
    Result:=FItems.Count
  else
    Result:=0;
end;

procedure TCodeXYPositions.Delete(Index: integer);
var
  Item: PCodeXYPosition;
begin
  Item:=Items[Index];
  Dispose(Item);
  FItems.Delete(Index);
end;

function TCodeXYPositions.CreateCopy: TCodeXYPositions;
begin
  Result:=TCodeXYPositions.Create;
  Result.Assign(Self);
end;

end.

