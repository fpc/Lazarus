{
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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

Type
  EArray=Class(Exception);
  
  TOnNotifyItem = Procedure(Sender: TObject; Col,Row: integer; Var Item: Pointer) of Object;
  TOnExchangeItem = procedure (Sender: TObject; Index, WithIndex: Integer) of Object;

  TArray=Class
  Private
    FCols: TList;
    FOnDestroyItem: TOnNotifyItem;
    FOnNewItem: TonNotifyItem;
    function Getarr(Col, Row: Integer): Pointer;
    procedure Setarr(Col, Row: Integer; Const Avalue: Pointer);
    procedure ClearCol(L: TList; Col: Integer);
    procedure Aumentar_Rows(col,Rows: Integer; L: TList);
    procedure DestroyItem(Col,Row: Integer; P: Pointer);
  Public
    Constructor Create;
    Destructor Destroy; Override;
    procedure SetLength(Cols,Rows: Integer);

    procedure DeleteColRow(IsColumn: Boolean; Index: Integer);
    procedure MoveColRow(IsColumn:Boolean; FromIndex, ToIndex: Integer);
    procedure ExchangeColRow(IsColumn:Boolean; Index, WithIndex: Integer);
    procedure Clear;
    
    Property Arr[Col,Row: Integer]: Pointer Read GetArr Write SetArr; Default;
    Property OnDestroyItem: TOnNotifyItem Read FOnDestroyItem Write FOnDestroyItem;
    Property OnNewItem: TOnNotifyItem Read FOnNewItem Write FOnNewItem;
  End;

implementation

{ TArray }

function TArray.Getarr(Col, Row: Integer): Pointer;
Begin
  // Checar dimensiones
  Result:= TList(FCols[Col])[Row];
End;

procedure TArray.Setarr(Col, Row: Integer; Const Avalue: Pointer);
Begin
  // Checar dimensiones
  TList(FCols[Col])[Row]:=aValue;
End;

procedure TArray.ClearCol(L: TList; Col: Integer);
Var
   j: Integer;
Begin
  If L<>Nil Then begin
    For j:=0 to L.Count-1 do DestroyItem(Col,J, L[J]);
    L.Clear;
  End;
End;

procedure TArray.Clear;
Var
   i: Integer;
Begin
  {$Ifdef dbgMem}DebugLn('TArray.Clear');{$endif}
  For i:=0 to FCols.Count-1 do begin
    ClearCol(TList(FCols[i]), i);
    TList(FCols[i]).Free;
  End;
  FCols.Clear;
End;

Constructor TArray.Create;
Begin
  Inherited Create;
  FCols:=TList.Create;
End;

Destructor TArray.Destroy;
Begin
  {$Ifdef dbgMem}DebugLn('TArray.Destroy FCols.Count=',dbgs(FCols.Count));{$endif}
  Clear;
  FCols.free;
  Inherited Destroy;
End;

procedure TArray.Aumentar_Rows(col,rows: Integer; L: TList);
var
   i,j: Integer;
   P:Pointer;
begin
  //DebugLn('TArray.Aumentar_Rows: Col=',Col,' Rows=',Rows);
  i:=L.Count;
  j:=Rows-L.Count;
  While j>0 do begin
    P:=nil;
    if Assigned(OnNewItem) Then OnNewItem(Self, col, i, P);
    L.Add(P);
    dec(j);
    inc(i);
  End;
End;

procedure TArray.DestroyItem(Col, Row: Integer; P: Pointer);
begin
  If (P<>nil)And Assigned(OnDestroyItem) Then OnDestroyItem(Self, Col, Row, P);
end;

procedure TArray.SetLength(Cols, Rows: Integer);
Var
   i,j: integer;
   L: TList;
   //P: Pointer;
Begin
  {$IfDef DbgMem}DebugLn('TArray.SetLength: Cols=',dbgs(Cols),' Rows=',dbgs(Rows));{$Endif}
  //
  // Ajustar columnas
  //
  If FCols.Count>Cols Then begin
    // Hay mas columnas de las que debe.
    // Destruir las columnas innecesarias
    for i:=Cols to fCols.Count-1 do begin
      L:=TList(FCols[i]);
      ClearCol(L, i);
      L.Free;
      L:=nil;
    End;
  End;
  FCols.Count:=Cols;
     
  //
  // Ajustar Renglones
  //
  For i:=0 to fCols.Count-1 do begin
    L:=TList(FCols[i]);
    If L=nil Then L:=TList.Create;
    If L.Count>Rows Then begin
      For j:=Rows to L.Count-1 do DestroyItem(i,j,L[j]);
      L.Count:=Rows;
    End;
    Aumentar_Rows(i, Rows, L);
    FCols[i]:=L;
  End;
End;

procedure TArray.DeleteColRow(IsColumn: Boolean; Index: Integer);
Var
  i: Integer;
  L: TList;
begin
  If IsColumn Then begin
    {$Ifdef dbgMem}DebugLn('TArray.DeleteColRow Col=',dbgs(Index));{$endif}
    L:=TList(FCols[Index]);
    If L<>nil then begin
      ClearCol(L, Index);
      FCols.Delete(Index);
      L.Free;
    End;
  End Else begin
    {$Ifdef dbgMem}DebugLn('TArray.DeleteColRow Row=',dbgs(Index));{$endif}
    For i:=0 to fCols.Count-1 do begin
      L:=TList(fcols[i]);
      If L<>nil then Begin
        DestroyItem(i, Index, L[Index]);
        L.Delete(Index);
      End;
    End;
  End;
end;

procedure TArray.MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
Var
  i: Integer;
begin
  If IsColumn Then begin
    FCols.Move(FromIndex, ToIndex);
  End Else begin
    For i:=0 to FCols.Count-1 do
      TList(Fcols[i]).Move(FromIndex,ToIndex);
  End;
end;

procedure TArray.ExchangeColRow(IsColumn: Boolean; Index, WithIndex: Integer);
Var
  i: Integer;
begin
  If IsColumn Then begin
    FCols.Exchange(Index, WithIndex);
  End Else begin
    For i:=0 to FCols.Count-1 do
      TList(FCols[i]).Exchange(Index, WithIndex);
  End;
end;

end.
