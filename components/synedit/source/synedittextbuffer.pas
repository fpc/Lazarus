{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynEditTextBuffer.pas, released 2000-04-07.
The Original Code is based on parts of mwCustomEdit.pas by Martin Waldenburg,
part of the mwEdit component suite.
Portions created by Martin Waldenburg are Copyright (C) 1998 Martin Waldenburg.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynEditTextBuffer;

{$I SynEdit.inc}
{$DEFINE MWE_FPC}
{$IFDEF MWE_FPC}
  {$mode objfpc}
  {$H+}
{$ENDIF}
interface

uses
  Classes,
//ADDED FOR LAZARUS
  {$IFDEF MWE_FPC}
     LCLLinux,
  {$ELSE}
     Windows,
  {$ENDIF}
//ENDADDED
 SynEditTypes;

type
  TIndexEvent = procedure(Index: Integer) of object;

  TSynEditList = class(TStringList)
  private
    fOnAdded: TNotifyEvent;
    fOnCleared: TNotifyEvent;
    fOnDeleted: TIndexEvent;
    fOnInserted: TIndexEvent;
    fOnPutted: TIndexEvent;
    fOnScanRanges: TNotifyEvent;
  protected
    procedure Put(Index: Integer; const S: string); override;
  public
    function Add(const S: string): Integer; override;
//ADDED FOR LAZARUS
  {$IFDEF MWE_FPC}
    procedure AddStrings(TheStrings: TStrings); override;
  {$ELSE}
    procedure AddStrings(Strings: TStrings); override;
  {$ENDIF}
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function Get(Index: integer): string; override;
    procedure Insert(Index: Integer; const S: string); override;
    property OnAdded: TNotifyEvent read FOnAdded write FOnAdded;
    property OnCleared: TNotifyEvent read fOnCleared write fOnCleared;
    property OnDeleted: TIndexEvent read FOnDeleted write FOnDeleted;
    property OnInserted: TIndexEvent read FOnInserted write FOnInserted;
    property OnPutted: TIndexEvent read FOnPutted write FOnPutted;
    property OnScanRanges: TNotifyEvent read fOnScanRanges write fOnScanRanges;
  end;

  TSynChangeReason = (crInsert, crPaste, crDragDropInsert,
    crDeleteAfterCursor, crDelete, crSelDelete, crDragDropDelete,
    crLineBreak, crIndent, crUnindent, crNothing);

  TSynEditUndoItem = class(TObject)
  public
    fChangeReason: TSynChangeReason;
    fChangeSelMode: TSynSelectionMode;
    fChangeStartPos: TPoint;
    fChangeEndPos: TPoint;
    fChangeStr: string;
  end;

  TSynEditUndoList = class(TObject)
  private
    fItems: TList;
//    fLocked: boolean;                                                         //mh 2000-07-22
    fLockCount: integer;                                                        //mh 2000-07-22
    fMaxUndoActions: integer;
    fOnAdded: TNotifyEvent;
    procedure EnsureMaxEntries;
    function GetCanUndo: boolean;
    function GetItemCount: integer;
    procedure SetMaxUndoActions(Value: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChange(AReason: TSynChangeReason; AStart, AEnd: TPoint;
      ChangeText: string; SelMode: TSynSelectionMode);
    procedure Clear;
    procedure Lock;                                                             //mh 2000-07-22
    function PeekItem: TSynEditUndoItem;
    function PopItem: TSynEditUndoItem;
    procedure PushItem(Item: TSynEditUndoItem);
    procedure Unlock;                                                           //mh 2000-07-22
  public
    property CanUndo: boolean read GetCanUndo;
    property ItemCount: integer read GetItemCount;
//    property Locked: boolean read fLocked write fLocked;                      //mh 2000-07-22
    property MaxUndoActions: integer read fMaxUndoActions
      write SetMaxUndoActions;
    property OnAddedUndo: TNotifyEvent read fOnAdded write fOnAdded;
  end;

implementation

{ TSynEditList }

function TSynEditList.Add(const S: string): Integer;
begin
  BeginUpdate;
  Result := inherited Add(S);
  if Assigned(FOnAdded) then FOnAdded(Self);
  EndUpdate;
end;


//ADDED FOR LAZARUS
  {$IFDEF MWE_FPC}
procedure TSynEditList.AddStrings(TheStrings: TStrings);
  {$ELSE}
procedure TSynEditList.AddStrings(Strings: TStrings);
  {$ENDIF}
var
  I: Integer;
begin
  BeginUpdate;
  try
//ADDED FOR LAZARUS
  {$IFDEF MWE_FPC}
    for I := 0 to TheStrings.Count - 1 do
      inherited Add(TheStrings[I]);
  {$ELSE}
    for I := 0 to Strings.Count - 1 do
      inherited Add(Strings[I]);
  {$ENDIF}
    if Assigned(fOnScanRanges) then
      fOnScanRanges(Self);
  finally
    EndUpdate;
  end;
end;

procedure TSynEditList.Clear;
begin
  BeginUpdate;
  if Assigned(fOnCleared) then fOnCleared(Self);
  inherited Clear;
  EndUpdate;
end;

procedure TSynEditList.Delete(Index: Integer);
begin
  BeginUpdate;
  inherited Delete(Index);
  if Assigned(FOnDeleted) then fOnDeleted(Index);
  EndUpdate;
end;

function TSynEditList.Get(Index: integer): string;
begin
  if (Index >= 0) and (Index < Count) then
    Result := inherited Get(Index)
  else
    Result := '';
end;

procedure TSynEditList.Insert(Index: Integer; const S: string);
begin
  BeginUpdate;
  inherited Insert(Index, S);
  if Assigned(FOnInserted) then fOnInserted(Index);
  EndUpdate;
end;

procedure TSynEditList.Put(Index: Integer; const S: string);
begin
  BeginUpdate;
  if (Index = 0) and (Count = 0) then
    Add(S)
  else begin
    inherited Put(Index, S);
    if Assigned(FOnPutted) then fOnPutted(Index);
  end;
  EndUpdate;
end;

{ TSynEditUndoList }

constructor TSynEditUndoList.Create;
begin
  inherited Create;
  fItems := TList.Create;
  fMaxUndoActions := 1024;
end;

destructor TSynEditUndoList.Destroy;
begin
  Clear;
  fItems.Free;
  inherited Destroy;
end;

procedure TSynEditUndoList.AddChange(AReason: TSynChangeReason; AStart,
  AEnd: TPoint; ChangeText: string; SelMode: TSynSelectionMode);
var
  NewItem: TSynEditUndoItem;
begin
//  if not fLocked then begin                                                   //mh 2000-07-22
  if fLockCount = 0 then begin                                                  //mh 2000-07-22     
    NewItem := TSynEditUndoItem.Create;
    try
      with NewItem do begin
        fChangeReason := AReason;
        fChangeSelMode := SelMode;
        fChangeStartPos := AStart;
        fChangeEndPos := AEnd;
        fChangeStr := ChangeText;
      end;
      PushItem(NewItem);
    except
      NewItem.Free;
      raise;
    end;
  end;
end;

procedure TSynEditUndoList.Clear;
var
  i: integer;
begin
  for i := 0 to fItems.Count - 1 do
    TSynEditUndoItem(fItems[i]).Free;
  fItems.Clear;
end;

procedure TSynEditUndoList.EnsureMaxEntries;
var
  Item: TSynEditUndoItem;
begin
  while fItems.Count > fMaxUndoActions do begin

//ADDED FOR LAZARUS
  {$IFDEF MWE_FPC}
    Item := TSynEditUndoItem(fItems.Items[0]);
  {$ELSE}
    Item := fItems[0];
  {$ENDIF}
//ENDADDED
    Item.Free;
    fItems.Delete(0);

  end;
end;

function TSynEditUndoList.GetCanUndo: boolean;
begin
  Result := fItems.Count > 0;
end;

function TSynEditUndoList.GetItemCount: integer;
begin
  Result := fItems.Count;
end;

{begin}                                                                         //mh 2000-07-22
procedure TSynEditUndoList.Lock;
begin
  Inc(fLockCount);
end;
{end}                                                                           //mh 2000-07-22

function TSynEditUndoList.PeekItem: TSynEditUndoItem;
var
  iLast: integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then
//ADDED FOR LAZARUS
  {$IFDEF MWE_FPC}
    Result := TSynEditUndoItem(fItems[iLast]);
  {$ELSE}
    Result := fItems[iLast];
  {$ENDIF}
//ENDADDED
end;

function TSynEditUndoList.PopItem: TSynEditUndoItem;
var
  iLast: integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then begin
//ADDED FOR LAZARUS
  {$IFDEF MWE_FPC}
    Result := TSynEditUndoItem(fItems[iLast]);
  {$ELSE}
    Result := fItems[iLast];
  {$ENDIF}
//ENDADDED
    fItems.Delete(iLast);
  end;
end;

procedure TSynEditUndoList.PushItem(Item: TSynEditUndoItem);
begin
  if Assigned(Item) then begin
    fItems.Add(Item);
    EnsureMaxEntries;
    if Assigned(fOnAdded) then
      fOnAdded(Self);
  end;
end;

procedure TSynEditUndoList.SetMaxUndoActions(Value: integer);
begin
  if Value < 0 then
    Value := 0;
  if Value <> fMaxUndoActions then begin
    fMaxUndoActions := Value;
    EnsureMaxEntries;
  end;
end;

{begin}                                                                         //mh 2000-07-22
procedure TSynEditUndoList.Unlock;
begin
  if fLockCount > 0 then
    Dec(fLockCount);
end;
{end}                                                                           //mh 2000-07-22

end.

