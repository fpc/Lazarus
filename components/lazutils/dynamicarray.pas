{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Jesus Reyes

  Abstract:
    A dynamic 2-dimensional array of Pointers.
    Used for TCustomGrid, TDrawGrid and TStringGrid.
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

  TPointerPointerArray=Class
  private
    FCols: TFpList;
    FOnDestroyItem: TOnNotifyItem;
    FOnNewItem: TonNotifyItem;
    function Getarr(Col, Row: Integer): Pointer;
    procedure Setarr(Col, Row: Integer; const AValue: Pointer);
    procedure ClearCol(L: TFpList; Col: Integer);
    procedure Aumentar_Rows(col,Rows: Integer; L: TFpList);
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

{ TPointerPointerArray }

function TPointerPointerArray.Getarr(Col, Row: Integer): Pointer;
begin
  // Checar dimensiones
  Result := TFpList(FCols[Col])[Row];
end;

procedure TPointerPointerArray.Setarr(Col, Row: Integer; const AValue: Pointer);
begin
  // Checar dimensiones
  TFpList(FCols[Col])[Row] := AValue;
end;

procedure TPointerPointerArray.ClearCol(L: TFpList; Col: Integer);
var
   j: Integer;
begin
  if L<>nil then begin
    for j:=0 to L.Count-1 do DestroyItem(Col,J, L[J]);
    L.Clear;
  end;
end;

procedure TPointerPointerArray.Clear;
var
   i: Integer;
begin
  {$Ifdef dbgMem}DebugLn('TArray.Clear');{$endif}
  for i:=0 to FCols.Count-1 do begin
    ClearCol(TFpList(FCols[i]), i);
    TFpList(FCols[i]).Free;
  end;
  FCols.Clear;
end;

constructor TPointerPointerArray.Create;
begin
  inherited Create;
  FCols := TFpList.Create;
end;

destructor TPointerPointerArray.Destroy;
begin
  {$Ifdef dbgMem}DebugLn('TArray.Destroy FCols.Count=',dbgs(FCols.Count));{$endif}
  Clear;
  FCols.free;
  inherited Destroy;
end;

procedure TPointerPointerArray.Aumentar_Rows(col,rows: Integer; L: TFpList);
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

procedure TPointerPointerArray.DestroyItem(Col, Row: Integer; P: Pointer);
begin
  if (P<>nil)And Assigned(OnDestroyItem) then OnDestroyItem(Self, Col, Row, P);
end;

procedure TPointerPointerArray.SetLength(Cols, Rows: Integer);
var
   i,j: integer;
   L: TFpList;
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
      L:=TFpList(FCols[i]);
      ClearCol(L, i);
      FreeAndNil(L);
    end;
  end;
  FCols.Count:=Cols;
     
  //
  // Ajustar Renglones
  //
  for i:=0 to fCols.Count-1 do begin
    L:=TFpList(FCols[i]);
    if L=nil then L:=TFpList.Create;
    if L.Count>Rows then begin
      for j:=Rows to L.Count-1 do DestroyItem(i,j,L[j]);
      L.Count:=Rows;
    end;
    Aumentar_Rows(i, Rows, L);
    FCols[i]:=L;
  end;
end;

procedure TPointerPointerArray.DeleteColRow(IsColumn: Boolean; Index: Integer);
var
  i: Integer;
  L: TFpList;
begin
  if IsColumn then begin
    {$Ifdef dbgMem}DebugLn('TArray.DeleteColRow Col=',dbgs(Index));{$endif}
    L:=TFpList(FCols[Index]);
    If L<>nil then begin
      ClearCol(L, Index);
      FCols.Delete(Index);
      L.Free;
    end;
  end else begin
    {$Ifdef dbgMem}DebugLn('TArray.DeleteColRow Row=',dbgs(Index));{$endif}
    for i:=0 to fCols.Count - 1 do begin
      L:=TFpList(fcols[i]);
      if L<>nil then begin
        DestroyItem(i, Index, L[Index]);
        L.Delete(Index);
      end;
    end;
  end;
end;

procedure TPointerPointerArray.MoveColRow(IsColumn: Boolean; FromIndex, ToIndex: Integer);
var
  i: Integer;
begin
  If IsColumn then begin
    FCols.Move(FromIndex, ToIndex);
  end else begin
    for i:=0 to FCols.Count-1 do
      TFpList(Fcols[i]).Move(FromIndex,ToIndex);
  end;
end;

procedure TPointerPointerArray.ExchangeColRow(IsColumn: Boolean; Index, WithIndex: Integer);
var
  i: Integer;
begin
  if IsColumn then begin
    FCols.Exchange(Index, WithIndex);
  end else begin
    for i:=0 to FCols.Count-1 do
      TFpList(FCols[i]).Exchange(Index, WithIndex);
  end;
end;

end.
