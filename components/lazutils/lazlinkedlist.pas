{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
 
  Authors: Mattias Gaertner, Jeroen van Iddekinge

  Abstract:
    Defines the simple double connected queue TLinkList.
    It supports Adding, Deleting, getting First and getting Last in O(1).
    Finding can be done in time O(n).
}
unit LazLinkedList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLinkListItem = class
    Next  : TLinkListItem;
    Prior : TLinkListItem;
    procedure ResetItem; virtual;
  end;

  TLinkList = class
  private
    FFirstFree: TLinkListItem;
    FFreeCount: integer;
    FFirst: TLinkListItem;
    FLast: TLinkListItem;
    FCount: integer;
    procedure DisposeItem(AnItem: TLinkListItem);
    procedure Unbind(AnItem: TLinkListItem);
  protected
    function CreateItem: TLinkListItem; virtual; abstract;
    function GetNewItem: TLinkListItem;
    procedure AddAsLast(AnItem: TLinkListItem);
  public
    property First: TLinkListItem read FFirst;
    property Last: TLinkListItem read FLast;
    property Count: integer read FCount;
    procedure Delete(AnItem: TLinkListItem);
    procedure MoveToLast(AnItem: TLinkListItem);
    procedure Clear;
    function ConsistencyCheck: integer;
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TLinkList }

procedure TLinkListItem.ResetItem;
begin
  Next := nil;
  Prior := nil;
end;

constructor TLinkList.Create;
begin
  inherited Create;
end;

destructor TLinkList.Destroy;
var AnItem: TLinkListItem;
begin
  Clear;
  // clear the free list
  while FFirstFree<>nil do begin
    AnItem:=FFirstFree;
    FFirstFree:=AnItem.Next;
    AnItem.Destroy;
  end;
  inherited Destroy;
end;

procedure TLinkList.Delete(AnItem: TLinkListItem);
begin
  if AnItem=nil then exit;
  Unbind(AnItem);
  AnItem.Destroy;
end;

procedure TLinkList.MoveToLast(AnItem: TLinkListItem);
begin
  if AnItem=nil then exit;
  Unbind(AnItem);
  AddAsLast(AnItem);
end;

procedure TLinkList.Clear;
begin
  while First<>nil do Delete(First);
end;

function TLinkList.GetNewItem: TLinkListItem;
begin
  if FFirstFree<>nil then begin
    Result:=FFirstFree;
    FFirstFree:=FFirstFree.Next;
    if FFirstFree<>nil then
      FFirstFree.Prior:=nil;
    dec(FFreeCount);
  end else begin
    Result := CreateItem;
  end;
  Result.Next:=nil;
  Result.Prior:=nil;
end;

procedure TLinkList.DisposeItem(AnItem: TLinkListItem);
var i: integer;
begin
  if FFreeCount<=2*FCount then begin
    AnItem.ResetItem;
    AnItem.Next:=FFirstFree;
    FFirstFree:=AnItem;
    if AnItem.Next<>nil then AnItem.Next.Prior:=AnItem;
    inc(FFreeCount);
  end else begin
    AnItem.Destroy;
    if (FCount+5)<2*FFreeCount then begin
      for i:=1 to 2 do begin
        if FFirstFree<>nil then begin
          AnItem:=FFirstFree;
          FFirstFree:=FFirstFree.Next;
          if FFirstFree<>nil then
            FFirstFree.Prior:=nil;
	  AnItem.Destroy;
          dec(FFreeCount);
        end;
      end;
    end;
  end;
end;

procedure TLinkList.Unbind(AnItem: TLinkListItem);
begin
  if AnItem=nil then exit;
  if FFirst=AnItem then FFirst:=FFirst.Next;
  if FLast=AnItem then FLast:=FLast.Prior;
  if AnItem.Prior<>nil then AnItem.Prior.Next:=AnItem.Next;
  if AnItem.Next<>nil then AnItem.Next.Prior:=AnItem.Prior;
  AnItem.Prior:=nil;
  AnItem.Next:=nil;
  dec(FCount);
end;

procedure TLinkList.AddAsLast(AnItem: TLinkListItem);
begin
  AnItem.Prior:=FLast;
  AnItem.Next:=nil;
  FLast:=AnItem;
  if AnItem.Prior<>nil then
    AnItem.Prior.Next:=AnItem
  else
    FFirst:=AnItem;
  inc(FCount);
end;

function TLinkList.ConsistencyCheck: integer;
var RealCount: integer;
  AnItem: TLinkListItem;
begin
  // test free list
  RealCount:=0;
  AnItem:=FFirstFree;
  while AnItem<>nil do begin
    inc(RealCount);
    AnItem:=AnItem.Next;
  end;
  if FFreeCount<>RealCount then begin
    Result:=-1;  exit;
  end;
  // test items
  RealCount:=0;
  AnItem:=FFirst;
  while AnItem<>nil do begin
    if (AnItem.Next<>nil) and (AnItem.Next.Prior<>AnItem) then begin
      Result:=-2;  exit;
    end;
    if (AnItem.Prior<>nil) and (AnItem.Prior.Next<>AnItem) then begin
      Result:=-3;  exit;
    end;
    inc(RealCount);
    AnItem:=AnItem.Next;
  end;
  if FCount<>RealCount then begin
    Result:=-4;  exit;
  end;
  Result:=0;
end;

end.

