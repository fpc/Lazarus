{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit LazMethodList;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, RtlConsts; 

type
  { TMethodList - array of TMethod }

  TMethodList = class
  private type
    TItemsEnumerator = record
    private
      Owner: TMethodList;
      Index: Integer;
      Reverse: Boolean;
      function GetCurrent: TMethod;
    public
      procedure Init(AOwner: TMethodList; AReverse: Boolean);
      function MoveNext: Boolean;
      function GetEnumerator: TItemsEnumerator;
      property Current: TMethod read GetCurrent;
    end;
  private
    FAllowDuplicates: boolean;
    FItems: ^TMethod;
    FCount: integer;
    function GetItems(Index: integer): TMethod;
    procedure SetAllowDuplicates(AValue: boolean);
    procedure SetItems(Index: integer; const AValue: TMethod);
    procedure InternalInsert(Index: integer; const AMethod: TMethod);
    procedure RaiseIndexOutOfBounds(Index: integer);
  public
    destructor Destroy; override;
    function Count: integer;
    function NextDownIndex(var Index: integer): boolean;
    function IndexOf(const AMethod: TMethod): integer;
    procedure Delete(Index: integer);
    procedure Remove(const AMethod: TMethod);
    procedure Add(const AMethod: TMethod);
    procedure Add(const AMethod: TMethod; AsLast: boolean);
    procedure Insert(Index: integer; const AMethod: TMethod);
    procedure Move(OldIndex, NewIndex: integer);
    procedure RemoveAllMethodsOfObject(const AnObject: TObject);
    procedure CallNotifyEvents(Sender: TObject); // calls from Count-1 downto 0, all methods must be TNotifyEvent
    function GetReversedEnumerator: TItemsEnumerator;
    function GetEnumerator: TItemsEnumerator;
  public
    property Items[Index: integer]: TMethod read GetItems write SetItems; default;
    property AllowDuplicates: boolean read FAllowDuplicates write SetAllowDuplicates; // default false, changed in Lazarus 1.3
  end;

function CompareMethods(const m1, m2: TMethod): boolean;


implementation

function CompareMethods(const m1, m2: TMethod): boolean;
begin
  Result:=(m1.Code=m2.Code) and (m1.Data=m2.Data);
end;

{ TMethodList.TItemsEnumerator }

function TMethodList.TItemsEnumerator.GetCurrent: TMethod;
begin
  Result := Owner[Index];
end;

function TMethodList.TItemsEnumerator.GetEnumerator: TItemsEnumerator;
begin
  Result := Self;
end;

procedure TMethodList.TItemsEnumerator.Init(AOwner: TMethodList;
  AReverse: Boolean);
begin
  Owner := AOwner;
  Reverse := AReverse;
  if Reverse then
    Index := AOwner.Count
  else
    Index := -1;
end;

function TMethodList.TItemsEnumerator.MoveNext: Boolean;
begin
  if Reverse then
  begin
    Dec(Index);
    Result := Index >= 0;
  end else
  begin
    Inc(Index);
    Result := Index < Owner.Count;
  end;
end;

{ TMethodList }

function TMethodList.GetItems(Index: integer): TMethod;
begin
  Result:=FItems[Index];
end;

function TMethodList.GetReversedEnumerator: TItemsEnumerator;
begin
  Result.Init(Self, True);
end;

procedure TMethodList.SetAllowDuplicates(AValue: boolean);
var
  i, j: Integer;
begin
  if FAllowDuplicates=AValue then Exit;
  FAllowDuplicates:=AValue;
  if not AllowDuplicates then
  begin
    i:=0;
    while i<FCount do
    begin
      j:=i+1;
      while j<FCount do
      begin
        if (FItems[i].Code=FItems[j].Code)
        and (FItems[i].Data=FItems[j].Data) then
          Delete(j)
        else
          inc(j);
      end;
      inc(i);
    end;
  end;
end;

procedure TMethodList.SetItems(Index: integer; const AValue: TMethod);

  procedure RaiseDuplicate;
  begin
    raise EListError.CreateFmt(SDuplicateItem,[AValue.Code]);
  end;

begin
  if (not AllowDuplicates) and (IndexOf(AValue)<>Index) then
    RaiseDuplicate;
  FItems[Index]:=AValue;
end;

procedure TMethodList.InternalInsert(Index: integer; const AMethod: TMethod);
begin
  inc(FCount);
  ReAllocMem(FItems,FCount*SizeOf(TMethod));
  if Index<FCount then
    System.Move(FItems[Index],FItems[Index+1],(FCount-Index-1)*SizeOf(TMethod));
  FItems[Index]:=AMethod;
end;

procedure TMethodList.RaiseIndexOutOfBounds(Index: integer);
begin
  raise EListError.CreateFmt(SListIndexError,[Index]);
end;

destructor TMethodList.Destroy;
begin
  ReAllocMem(FItems,0);
  inherited Destroy;
end;

function TMethodList.GetEnumerator: TItemsEnumerator;
begin
  Result.Init(Self, False);
end;

function TMethodList.Count: integer;
begin
  if Self<>nil then
    Result:=FCount
  else
    Result:=0;
end;

function TMethodList.NextDownIndex(var Index: integer): boolean;
begin
  if Self<>nil then begin
    dec(Index);
    if (Index>=FCount) then
      Index:=FCount-1;
  end else
    Index:=-1;
  Result:=(Index>=0);
end;

function TMethodList.IndexOf(const AMethod: TMethod): integer;
begin
  if Self<>nil then begin
    Result:=FCount-1;
    while Result>=0 do begin
      if (FItems[Result].Code=AMethod.Code)
      and (FItems[Result].Data=AMethod.Data) then exit;
      dec(Result);
    end;
  end else
    Result:=-1;
end;

procedure TMethodList.Delete(Index: integer);
begin
  dec(FCount);
  if FCount>Index then
    System.Move(FItems[Index+1],FItems[Index],(FCount-Index)*SizeOf(TMethod));
  ReAllocMem(FItems,FCount*SizeOf(TMethod));
end;

procedure TMethodList.Remove(const AMethod: TMethod);
var
  i: integer;
begin
  if Self<>nil then begin
    i:=IndexOf(AMethod);
    if i>=0 then Delete(i);
  end;
end;

procedure TMethodList.Add(const AMethod: TMethod);
var
  i: Integer;
begin
  if AllowDuplicates then
    i:=-1
  else
    i:=IndexOf(AMethod);
  if (i<0) then
  begin
    inc(FCount);
    ReAllocMem(FItems,FCount*SizeOf(TMethod));
  end else begin
    if i=FCount-1 then exit;
    System.Move(FItems[i+1],FItems[i],SizeOf(TMethod)*(FCount-i-1));
  end;
  FItems[FCount-1]:=AMethod;
end;

procedure TMethodList.Add(const AMethod: TMethod; AsLast: boolean);
begin
  if AsLast then
    Add(AMethod)
  else
    Insert(0,AMethod);
end;

procedure TMethodList.Insert(Index: integer; const AMethod: TMethod);
var
  i: Integer;
begin
  if AllowDuplicates then
    i:=-1
  else
    i:=IndexOf(AMethod);
  if i<0 then
  begin
    if (Index<0) or (Index>FCount) then
      RaiseIndexOutOfBounds(Index);
    InternalInsert(Index,AMethod)
  end else
    Move(i,Index);
end;

procedure TMethodList.Move(OldIndex, NewIndex: integer);
var
  MovingMethod: TMethod;
begin
  if OldIndex=NewIndex then exit;
  if (NewIndex<0) or (NewIndex>=FCount) then
    RaiseIndexOutOfBounds(NewIndex);
  MovingMethod:=FItems[OldIndex];
  if OldIndex>NewIndex then
    System.Move(FItems[NewIndex],FItems[NewIndex+1],
                SizeOf(TMethod)*(OldIndex-NewIndex))
  else
    System.Move(FItems[NewIndex+1],FItems[NewIndex],
                SizeOf(TMethod)*(NewIndex-OldIndex));
  FItems[NewIndex]:=MovingMethod;
end;

procedure TMethodList.RemoveAllMethodsOfObject(const AnObject: TObject);
var
  i: Integer;
begin
  if Self=nil then exit;
  i:=FCount-1;
  while i>=0 do begin
    if TObject(FItems[i].Data)=AnObject then Delete(i);
    dec(i);
  end;
end;

procedure TMethodList.CallNotifyEvents(Sender: TObject);
var
  i: LongInt;
begin
  i:=Count;
  while NextDownIndex(i) do
    TNotifyEvent(Items[i])(Sender);
end;

end.

