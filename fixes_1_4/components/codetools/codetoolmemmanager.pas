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
    Defines TCodeToolMemManager, which is the base class for the various
    memory manager in the codetools. An own memory manager is somewhat faster
    and makes debugging and profiling easier.
}
unit CodeToolMemManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PCodeToolMemManagerItem = ^TCodeToolMemManagerItem;
  TCodeToolMemManagerItem = record
    Next: PCodeToolMemManagerItem;
  end;

  // memory manager template
  TCodeToolMemManager = class
  private
    procedure SetMaxFreeRatio(NewValue: integer);
    procedure SetMinFree(NewValue: integer);
  protected
    FFirstFree: PCodeToolMemManagerItem;
    FFreeCount: integer;
    FCount: integer;
    FMinFree: integer;
    FMaxFreeRatio: integer;
    {$IFDEF DebugCTMemManager}
    FAllocatedCount: int64;
    FFreedCount: int64;
    {$ENDIF}
    procedure DisposeItem(AnItem: PCodeToolMemManagerItem);
    function NewItem: PCodeToolMemManagerItem;
    procedure FreeFirstItem; virtual;
  public
    property MinimumFreeCount: integer read FMinFree write SetMinFree;
    property MaximumFreeCountRatio: integer
        read FMaxFreeRatio write SetMaxFreeRatio; // in one eighth steps
    property Count: integer read FCount;
    property FreeCount: integer read FFreeCount;
    {$IFDEF DebugCTMemManager}
    property AllocatedCount: int64 read FAllocatedCount;
    property FreedCount: int64 read FFreedCount;
    {$ENDIF}
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
  end;


implementation


{ TCodeToolMemManager }

procedure TCodeToolMemManager.Clear;
begin
  while FFirstFree<>nil do begin
    FreeFirstItem;
    {$IFDEF DebugCTMemManager}
    inc(FFreedCount);
    {$ENDIF}
  end;
  FFreeCount:=0;
end;

constructor TCodeToolMemManager.Create;
begin
  inherited Create;
  FFirstFree:=nil;
  FFreeCount:=0;
  FCount:=0;
  {$IFDEF DebugCTMemManager}
  FAllocatedCount:=0;
  FFreedCount:=0;
  {$ENDIF}
  FMinFree:=100000;
  FMaxFreeRatio:=8; // 1:1
end;

destructor TCodeToolMemManager.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TCodeToolMemManager.DisposeItem(AnItem: PCodeToolMemManagerItem);
begin
  if (FFreeCount<FMinFree) or (FFreeCount<((FCount shr 3)*FMaxFreeRatio)) then
  begin
    // add ANode to Free list
    //AddItemToFreeList(AnItem);
    inc(FFreeCount);
  end else begin
    // free list full -> free the ANode
    //FreeItem(AnItem);
    {$IFDEF DebugCTMemManager}
    inc(FFreedCount);
    {$ENDIF}
  end;
  dec(FCount);
end;

function TCodeToolMemManager.NewItem: PCodeToolMemManagerItem;
begin
  if FFirstFree<>nil then begin
    // take from free list
    Result:=FFirstFree;
    FFirstFree:=FFirstFree^.Next;
    Result^.Next:=nil;
    dec(FFreeCount);
  end else begin
    // free list empty -> create new node
    New(Result);
    {$IFDEF DebugCTMemManager}
    inc(FAllocatedCount);
    {$ENDIF}
  end;
  inc(FCount);
end;

procedure TCodeToolMemManager.SetMaxFreeRatio(NewValue: integer);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMaxFreeRatio then exit;
  FMaxFreeRatio:=NewValue;
end;

procedure TCodeToolMemManager.SetMinFree(NewValue: integer);
begin
  if NewValue<0 then NewValue:=0;
  if NewValue=FMinFree then exit;
  FMinFree:=NewValue;
end;

procedure TCodeToolMemManager.FreeFirstItem;
var Item: PCodeToolMemManagerItem;
begin
  Item:=FFirstFree;
  FFirstFree:=FFirstFree^.Next;
  Dispose(Item);
end;

end.

