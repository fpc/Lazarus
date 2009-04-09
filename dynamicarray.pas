{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Jesus Reyes

  Abstract:
    Dynamic array support for TCustomGrid, TDrawGrid and TStringGrid
}

unit DynamicArray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

type
  EArray=Class(Exception);
  
  TOnNotifyItem = Procedure(Sender: TObject; Col,Row: integer; Var Item: Pointer) of Object;
  TOnExchangeItem = procedure (Sender: TObject; Index, WithIndex: Integer) of Object;

  TArray=Class
  private
    FCols: TList;
    FOnDestroyItem: TOnNotifyItem;
    FOnNewItem: TonNotifyItem;
    function Getarr(Col, Row: Integer): Pointer;
    procedure Setarr(Col, Row: Integer; const AValue: Pointer);
    procedure ClearCol(L: TList; Col: Integer);
    procedure Aumentar_Rows(col,Rows: Integer; L: TList);
    procedure DestroyItem(Col,Row: Integer; P: Pointer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetLength(Cols,Rows: Integer);

    procedure DeleteColRow(IsColumn: Boolean; Index: Integer);
    procedure MoveColRow(IsColumn:Boolean; FromIndex, ToIndex: Integer);
    procedure ExchangeColRow(IsColumn:Boolean; Index, WithIndex: Integer);
    procedure Clear;
    
    Property Arr[Col,Row: Integer]: Pointer read GetArr write SetArr; default;
    Property OnDestroyItem: TOnNotifyItem read FOnDestroyItem write FOnDestroyItem;
    Property OnNewItem: TOnNotifyItem read FOnNewItem write FOnNewItem;
  end;

implementation

{ TArray }

function TArray.Getarr(Col, Row: Integer): Pointer;
begin
  // Checar dimensiones
  Result := TList(FCols[Col])[Row];
end;

procedure TArray.Setarr(Col, Row: Integer; const AValue: Pointer);
begin
  // Checar dimensiones
  TList(FCols[Col])[Row] := AValue;
end;

procedure TArray.ClearCol(L: TList; Col: Integer);
var
   j: Integer;
begin
  if L<>nil then begin
    for j:=0 to L.Count-1 do DestroyItem(Col,J, L[J]);
    L.Clear;
  end;
end;

procedure TArray.Clear;
var
   i: Integer;
begin
  {$Ifdef dbgMem}DebugLn('TArray.Clear');{$endif}
  for i:=0 to FCols.Count-1 do begin
    ClearCol(TList(FCols[i]), i);
    TList(FCols[i]).Free;
  end;
  FCols.Clear;
end;

constructor TArray.Create;
begin
  inherited Create;
  FCols := TList.Create;
end;

destructor TArray.Destroy;
begin
  {$Ifdef dbgMem}DebugLn('TArray.Destroy FCols.Count=',dbgs(FCols.Count));{$endif}
  Clear;
  FCols.free;
  inherited Destroy;
end;

procedure TArray.Aumentar_Rows(col,rows: Integer; L: TList);
var
   i,j: Integer;
   P: Pointer;
begin
  //DebugLn('TArray.Aumentar_Rows: Col=',Col,' Rows=',Rows);
  i:=L.Count;
  j:=Rows-L.Count;
  while j>0 do begin
    P:=nil;
    if Assigned(OnNewItem) Then OnNewItem(Self, col, i, P);
    L.Add(P);
    dec(j);
    inc(i);
  end;
end;

procedure TArray.DestroyItem(Col, Row: Integer; P: Pointer);
begin
  if (P<>nil)And Assigned(OnDestroyItem) then OnDestroyItem(Self, Col, Row, P);
end;

procedure TArray.SetLength(Cols, Rows: Integer);
var
   i,j: integer;
   L: TList;
   //P: Pointer;
Begin
  {$IfDef DbgMem}DebugLn('TArray.SetLength: Cols=',dbgs(Cols),' Rows=',dbgs(Rows));{$Endif}
  //
  // Ajustar columnas
  //
  if FCols.Count>Cols then begin
    // Hay mas columnas de las que debe.
    // Destruir las columnas innecesarias
    for i:=Cols to fCols.Count-1 do begin
      L:=TList(FCols[i]);
      ClearCol(L, i);
      L.Free;
      L:=nil;
    end;
  end;
  FCols.Count:=Cols;
     
  //
  // Ajustar Renglones
  //
  for i:=0 to fCols.Count-1 do begin
    L:=TList(FCols[i]);
    if L=nil then L:=TList.Create;
    if L.Count>Rows then begin
      for j:=Rows to L.Count-1 do DestroyItem(i,j,L[j]);
      L.Count:=Rows;
    end;
    Aumentar_Rows(i, Rows, L);
    FCols[i]:=L;
  end;
end;

procedure TArray.DeleteColRow(IsColumn: Boolean; Index: Integer);
var
  i: Integer;
  L: TList;
begin
  if IsColumn then begin
    {$Ifdef dbgMem}DebugLn('TArray.DeleteColRow Col=',dbgs(Index));{$endif}
    L:=TList(FCols[Index]);
    If L<>nil then begin
      ClearCol(L, Index);
      FCols.Delete(Index);
      L.Free;
    end;
  end else begin
    {$Ifdef dbgMem}DebugLn('TArray.DeleteColRow Row=',dbgs(Index));{$endif}
    for i:=0 to fCols.Count - 1 do begin
      L:=TList(fcols[i]);
      if L<>nil then begin
        DestroyItem(i, Index, L[Index]);
        L.Delete(Index);
      end;
    end;
  end;
end;

procedure TArray.MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
var
  i: Integer;
begin
  If IsColumn then begin
    FCols.Move(FromIndex, ToIndex);
  end else begin
    for i:=0 to FCols.Count-1 do
      TList(Fcols[i]).Move(FromIndex,ToIndex);
  end;
end;

procedure TArray.ExchangeColRow(IsColumn: Boolean; Index, WithIndex: Integer);
var
  i: Integer;
begin
  if IsColumn then begin
    FCols.Exchange(Index, WithIndex);
  end else begin
    for i:=0 to FCols.Count-1 do
      TList(FCols[i]).Exchange(Index, WithIndex);
  end;
end;

end.
