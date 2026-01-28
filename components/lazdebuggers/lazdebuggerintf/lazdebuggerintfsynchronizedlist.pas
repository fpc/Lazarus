{***************************************************************************
 *                                                                         *
 * This unit is distributed under the LGPL version 2                       *
 *                                                                         *
 * Additionally this unit can be used under any newer version (3 or up)    *
 * of the LGPL                                                             *
 *                                                                         *
 * Users are also granted the same "linking exception" as defined          *
 * for the LCL.                                                            *
 * See the LCL license for details                                         *
 *                                                                         *
 *                                                                         *
 ***************************************************************************
 @author(Martin Friebe)
}
unit LazDebuggerIntfSynchronizedList;

{$mode objfpc}{$H+}
{$Interfaces CORBA}

interface

uses
  Classes, SysUtils;

type

  (* About the ChangeStamp:

     Each type should have one global source for the ChangeStamp.
     If any entry changes, it should get a new value, that therefore is newer
     than any known value by any list that may exist (and that may compare to
     the entries ChangeStamp)

     If a List updates it can either set its own ChangeStamp to the highest seen
     value in all entries, or to the current global value.

     If an entry is added/removed to/from a list (that can be source for sync)
     then that entry must update its ChangeStamp (even if it had no changes of
     its own).
     That way it will be noted as new by any list that may synchronize from it,
     even if that list had not known the entry before.

     If a list is not used a source for sync, then add/remove of entries does not
     need to update the entries ChangeStamps
  *)

  IDbgSynchronizedEntryIntf = interface ['{A05705C7-7443-43A0-9B32-EDAD5D888051}']
    function  GetID: QWord;

    property ID: QWord read GetID;
  end;

  IDbgSynchronizedOriginEntryIntf = interface(IDbgSynchronizedEntryIntf) ['{4431BB7D-5509-45BE-A659-9E7782987E31}']
    function  GetChangeStamp: QWord;

    procedure AddReference;
    procedure ReleaseReference;

    function  IsDeleted: boolean;

    property ChangeStamp: QWord read GetChangeStamp;
  end;

  IDbgSynchronizedTargetEntryIntf = interface(IDbgSynchronizedEntryIntf) ['{7C13D736-DC63-41B1-9105-5721DF39DB8B}']
    function HasDeletedOrigin: boolean;
  end;

  IDbgSynchronizedOriginListIntf = interface ['{3DA08DFE-B061-499C-9FA0-8ECA57F19496}']
    function  Count: Integer;
    function  GetSyncOriginEntry(AnIndex: Integer): IDbgSynchronizedOriginEntryIntf;
    function  GetChangeStamp: QWord;

    property Entries[AnIndex: integer]: IDbgSynchronizedOriginEntryIntf read GetSyncOriginEntry; default;
    property ChangeStamp: QWord read GetChangeStamp;
  end;

  IDbgSyncAbleOriginIntf = interface ['{D565EBFB-3143-413D-98A6-C951504DFFBD}']
    function GetSynchronizedListIntf: IDbgSynchronizedOriginListIntf;
  end;

  { TDbgSynchronizedListTemplate }

  generic TDbgSynchronizedListTemplate<_BASE: class; _SRC_LIST: IDbgSyncAbleOriginIntf> = class(_BASE)
  private
    procedure DoAfterDelete; virtual;
  protected
    function  GetIndexOfId(AnID: QWord): Integer; virtual; abstract;
    function  GetSyncTargetEntry(AnIndex: integer): IDbgSynchronizedTargetEntryIntf; virtual; abstract;

    procedure AddSyncedEntryFor(AnOriginIndex: integer; ASrcList: _SRC_LIST; AnOrigin: IDbgSynchronizedOriginEntryIntf); virtual; abstract;
    procedure UpdateSyncedEntryAt(AnOriginIndex, ATargetIndex: integer; ASrcList: _SRC_LIST; AnOrigin: IDbgSynchronizedOriginEntryIntf); virtual; abstract;
    procedure DeleteSyncedEntryAt(AnOriginIndex, ATargetIndex: integer; ASrcList: _SRC_LIST; AnOrigin: IDbgSynchronizedOriginEntryIntf); virtual; abstract;

    procedure UpdateListFrom(ASourceList: _SRC_LIST; AnOwnCount: integer; var AnOwnChangeStamp: QWord);
    procedure RemoveEntriesWithDeletedOrigin(AnOwnCount: integer);
  end;

  { TDbgSynchronizedListExTemplate }

  generic TDbgSynchronizedListExTemplate<_BASE: class; _SRC_LIST: IDbgSyncAbleOriginIntf> =
    class(specialize TDbgSynchronizedListTemplate<_BASE, _SRC_LIST>)
  strict private
    FHighDstIndex, FMaxIndex: Integer;
    FHighDstId: QWord;
  private
    procedure DoAfterDelete; override;
  protected
    // Should sort lists before calling UpdateListFrom
    function GetIndexOfId(AnID: QWord): Integer; override; final;

    procedure UpdateListFrom(ASourceList: _SRC_LIST; AnOwnCount: integer; var AnOwnChangeStamp: QWord); reintroduce;
  end;

//generic procedure UpdateSynchronizedListFrom<_TARGET, _ENTRY>(ASourceList: IDbgSynchronizedOriginListIntf; ATarget: _TARGET);


implementation

{ TDbgSynchronizedListTemplate }

procedure TDbgSynchronizedListTemplate.DoAfterDelete;
begin
  //
end;

procedure TDbgSynchronizedListTemplate.UpdateListFrom(ASourceList: _SRC_LIST; AnOwnCount: integer;
  var AnOwnChangeStamp: QWord);
var
  AnOriginList: IDbgSynchronizedOriginListIntf;
  OriginEntry: IDbgSynchronizedOriginEntryIntf;
  TargetIndex, OriginIndex: Integer;
  OriginChg, NewChangeStamp: QWord;
begin
  AnOriginList := ASourceList.GetSynchronizedListIntf;
  if AnOriginList.ChangeStamp <= AnOwnChangeStamp then
    exit;

  NewChangeStamp := 0;

  OriginIndex := AnOriginList.Count;
  while OriginIndex > 0 do begin
    dec(OriginIndex);
    OriginEntry := AnOriginList.Entries[OriginIndex];
    OriginChg := OriginEntry.ChangeStamp;
    if OriginChg <= AnOwnChangeStamp then
      continue;

    if OriginChg > NewChangeStamp then
      NewChangeStamp := OriginChg;

    TargetIndex := GetIndexOfId(OriginEntry.ID);
    if TargetIndex >= 0 then begin
      // update
      if OriginEntry.IsDeleted then begin
        DeleteSyncedEntryAt(OriginIndex, TargetIndex, ASourceList, OriginEntry);
        DoAfterDelete;
      end
      else
        UpdateSyncedEntryAt(OriginIndex, TargetIndex, ASourceList, OriginEntry);
    end
    else begin
      // add new // ignore if src is deleted (was never added)
      if not OriginEntry.IsDeleted then
        AddSyncedEntryFor(OriginIndex, ASourceList, OriginEntry);
    end;
  end;

  if NewChangeStamp > 0 then
    AnOwnChangeStamp := NewChangeStamp;
end;

procedure TDbgSynchronizedListTemplate.RemoveEntriesWithDeletedOrigin(AnOwnCount: integer);
var
  i: Integer;
  m: IDbgSynchronizedTargetEntryIntf;
begin
  i := AnOwnCount - 1;
  while i >= 0 do begin
    m := GetSyncTargetEntry(i);
    if m.HasDeletedOrigin then
      DeleteSyncedEntryAt(-1, i, nil, nil);
    dec(i);
  end;
end;

{ TDbgSynchronizedListExTemplate }

procedure TDbgSynchronizedListExTemplate.DoAfterDelete;
begin
  dec(FMaxIndex);
  if FHighDstIndex > 0 then
    dec(FHighDstIndex);
end;

function TDbgSynchronizedListExTemplate.GetIndexOfId(AnID: QWord): Integer;
var
  h, DstId: QWord;
  c: Integer;
  e: IDbgSynchronizedTargetEntryIntf;
begin
  h := FHighDstId;
  if AnID <= h then begin
    Result := 0;
    h := 0;
  end
  else
    Result := FHighDstIndex;
  c := FMaxIndex;

  while (Result <= c) do begin
    e := GetSyncTargetEntry(Result);
    DstId := e.ID;
    if DstId > h then
      h := DstId;

    if DstId = AnID then begin
      FHighDstIndex := Result;
      FHighDstId    := h;
      exit;
    end;
    inc(Result);
  end;

  FHighDstIndex := Result;
  FHighDstId := h;
  Result := -1;
end;

procedure TDbgSynchronizedListExTemplate.UpdateListFrom(ASourceList: _SRC_LIST;
  AnOwnCount: integer; var AnOwnChangeStamp: QWord);
begin
  FHighDstIndex := 0;
  FHighDstId := 0;
  FMaxIndex := AnOwnCount - 1;
  inherited UpdateListFrom(ASourceList, AnOwnCount, AnOwnChangeStamp);
end;

end.

