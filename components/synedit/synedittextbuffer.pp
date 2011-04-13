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

{$I synedit.inc}

interface

uses
  Classes, SysUtils, LCLProc, LCLIntf, LCLType,
  SynEditTextBase, SynEditMiscProcs, SynEditMiscClasses;

type
  TSynEditFlagsClass = class end; // For Register

  TSynEditStringFlag = (
    sfModified,              // a line is modified and not saved after
    sfSaved                  // a line is modified and saved after
  );
  TSynEditStringFlags = set of TSynEditStringFlag;
  PSynEditStringFlags = ^TSynEditStringFlags;

  TStringListIndexEvent = procedure(Index: Integer) of object;

  { TLineRangeNotificationList }

  TLineRangeNotificationList = Class(TSynMethodList)
  public
    Procedure CallRangeNotifyEvents(Sender: TSynEditStrings; aIndex, aCount: Integer);
  end;

  { TLineEditNotificationList }

  TLineEditNotificationList = Class(TSynMethodList)
  public
    Procedure CallRangeNotifyEvents(Sender: TSynEditStrings;
                                    aLinePos, aBytePos, aCount, aLineBrkCnt: Integer; aText: String);
  end;

  { TSynEditStringMemory }

  TSynEditStringMemory = class(TSynEditStorageMem)
  private
    FRangeList: TSynManagedStorageMemList;
    FRangeListLock: Integer;
    function GetFlags(Index: Integer): TSynEditStringFlags;
    function GetObject(Index: Integer): TObject;
    function GetRange(Index: Pointer): TSynManagedStorageMem;
    function GetString(Index: Integer): String;
    procedure SetFlags(Index: Integer; const AValue: TSynEditStringFlags);
    procedure SetObject(Index: Integer; const AValue: TObject);
    procedure SetRange(Index: Pointer; const AValue: TSynManagedStorageMem);
    procedure SetString(Index: Integer; const AValue: String);
  protected
    procedure Move(AFrom, ATo, ALen: Integer); override;
    procedure SetCount(const AValue: Integer); override;
    procedure SetCapacity(const AValue: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure InsertRows(AIndex, ACount: Integer); override;
    procedure DeleteRows(AIndex, ACount: Integer); override;
    function  GetPChar(ALineIndex: Integer; out ALen: Integer): PChar; // experimental
    property Strings[Index: Integer]: String read GetString write SetString; default;
    property Objects[Index: Integer]: TObject read GetObject write SetObject;
    property RangeList[Index: Pointer]: TSynManagedStorageMem read GetRange write SetRange;
    property Flags[Index: Integer]: TSynEditStringFlags read GetFlags write SetFlags;
  end;

  { TSynEditStringList }

  TSynEditStringList = class(TSynEditStrings)
  private
    FList: TSynEditStringMemory;

    FAttachedSynEditList: TFPList;
    FNotifyLists: Array [TSynEditNotifyReason] of TSynMethodList;
    FCachedNotify: Boolean;
    FCachedNotifyStart, FCachedNotifyCount: Integer;
    FCachedNotifySender: TSynEditStrings;

    FIgnoreSendNotification: array [TSynEditNotifyReason] of Integer;
    fDosFileFormat: boolean;
    fIndexOfLongestLine: integer;
    FRedoList: TSynEditUndoList;
    FUndoList: TSynEditUndoList;
    FIsUndoing, FIsRedoing: Boolean;
    FIsInDecPaintLock: Boolean;

    FModified: Boolean;
    FTextChangeStamp: int64;

    function GetAttachedSynEdits(Index: Integer): TSynEditBase;
    function GetFlags(Index: Integer): TSynEditStringFlags;
    procedure Grow;
    procedure InsertItem(Index: integer; const S: string);
    procedure SetFlags(Index: Integer; const AValue: TSynEditStringFlags);
    procedure SetModified(const AValue: Boolean);
    procedure SendCachedNotify;
  protected
    function GetExpandedString(Index: integer): string; override;
    function GetLengthOfLongestLine: integer; override;
    function GetTextChangeStamp: int64; override;

    function GetRedoList: TSynEditUndoList; override;
    function GetUndoList: TSynEditUndoList; override;
    function GetCurUndoList: TSynEditUndoList; override;
    procedure SetIsUndoing(const AValue: Boolean); override;
    function  GetIsUndoing: Boolean; override;
    procedure SetIsRedoing(const AValue: Boolean); override;
    function  GetIsRedoing: Boolean; override;
    procedure UndoRedoAdded(Sender: TObject);
    procedure IgnoreSendNotification(AReason: TSynEditNotifyReason;
                                     IncIgnore: Boolean); override;

    function GetRange(Index: Pointer): TSynManagedStorageMem; override;
    procedure PutRange(Index: Pointer; const ARange: TSynManagedStorageMem); override;
    function Get(Index: integer): string; override;
    function GetCapacity: integer;
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}                             //mh 2000-10-18
    function GetCount: integer; override;
    procedure SetCount(const AValue: Integer);
    function GetObject(Index: integer): TObject; override;
    procedure Put(Index: integer; const S: string); override;
    procedure PutObject(Index: integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: integer);
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}                             //mh 2000-10-18
    procedure SetUpdateState(Updating: Boolean; Sender: TObject); override;

    procedure UndoEditLinesDelete(LogY, ACount: Integer);
    procedure IncreaseTextChangeStamp;
    procedure DoGetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer; PWidths: PPhysicalCharWidth); override;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): integer; override;
    procedure AddStrings(AStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure DeleteLines(Index, NumLines: integer); override;
    procedure Insert(Index: integer; const S: string); override;
    procedure InsertLines(Index, NumLines: integer); override;
    procedure InsertStrings(Index: integer; NewStrings: TStrings); override;
    function  GetPChar(ALineIndex: Integer; out ALen: Integer): PChar; override; // experimental
    procedure MarkModified(AFirst, ALast: Integer);
    procedure MarkSaved;
    procedure AddGenericHandler(AReason: TSynEditNotifyReason;
                AHandler: TMethod); override;
    procedure RemoveGenericHandler(AReason: TSynEditNotifyReason;
                AHandler: TMethod); override;
    procedure SendNotification(AReason: TSynEditNotifyReason;
                ASender: TSynEditStrings; aIndex, aCount: Integer;
                aBytePos: Integer = -1; aLen: Integer = 0; aTxt: String = ''); override;
    procedure SendNotification(AReason: TSynEditNotifyReason;
                ASender: TObject); override;
    procedure FlushNotificationCache; override;
    procedure AttachSynEdit(AEdit: TSynEditBase);
    procedure DetachSynEdit(AEdit: TSynEditBase);
    function  AttachedSynEditCount: Integer;
    property  AttachedSynEdits[Index: Integer]: TSynEditBase read GetAttachedSynEdits;
    procedure CopyHanlders(OtherLines: TSynEditStringList; AOwner: TObject = nil);
    procedure RemoveHanlders(AOwner: TObject);
  public
    property DosFileFormat: boolean read fDosFileFormat write fDosFileFormat;    
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
    property Flags[Index: Integer]: TSynEditStringFlags read GetFlags
      write SetFlags;
    property Modified: Boolean read FModified write SetModified;
  public
    property UndoList: TSynEditUndoList read GetUndoList write fUndoList;
    property RedoList: TSynEditUndoList read GetRedoList write fRedoList;
    procedure EditInsert(LogX, LogY: Integer; AText: String); override;
    function  EditDelete(LogX, LogY, ByteLen: Integer): String; override;
    procedure EditLineBreak(LogX, LogY: Integer); override;
    procedure EditLineJoin(LogY: Integer; FillText: String = ''); override;
    procedure EditLinesInsert(LogY, ACount: Integer; AText: String = ''); override;
    procedure EditLinesDelete(LogY, ACount: Integer); override;
    procedure EditUndo(Item: TSynEditUndoItem); override;
    procedure EditRedo(Item: TSynEditUndoItem); override;
  public
    PaintLockOwner: TSynEditBase;
  end;

  ESynEditStringList = class(Exception);
{end}                                                                           //mh 2000-10-10

implementation

{$IFNDEF FPC}
  {$IFDEF SYN_COMPILER_3_UP}
resourcestring
  {$ELSE}
const
  {$ENDIF}
{$ELSE}
const
{$ENDIF}
  SListIndexOutOfBounds = 'Invalid stringlist index %d';

type

  { TSynEditUndoTxtInsert }

  TSynEditUndoTxtInsert = class(TSynEditUndoItem)
  private
    FPosX, FPosY, FLen: Integer;
  protected
    function DebugString: String; override;
  public
    constructor Create(APosX, APosY, ALen: Integer);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoTxtDelete }

  TSynEditUndoTxtDelete = class(TSynEditUndoItem)
  private
    FPosX, FPosY: Integer;
    FText: String;
  protected
    function DebugString: String; override;
  public
    constructor Create(APosX, APosY: Integer; AText: String);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoTxtLineBreak }

  TSynEditUndoTxtLineBreak = class(TSynEditUndoItem)
  private
    FPosY: Integer;
  protected
    function DebugString: String; override;
  public
    constructor Create(APosY: Integer);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoTxtLineJoin }

  TSynEditUndoTxtLineJoin = class(TSynEditUndoItem)
  private
    FPosX, FPosY: Integer;
  protected
    function DebugString: String; override;
  public
    constructor Create(APosX, APosY: Integer);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoTxtLinesIns }

  TSynEditUndoTxtLinesIns = class(TSynEditUndoItem)
  private
    FPosY, FCount: Integer;
  protected
    function DebugString: String; override;
  public
    constructor Create(ALine, ACount: Integer);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoTxtLinesDel }

  TSynEditUndoTxtLinesDel = class(TSynEditUndoItem)
  private
    FPosY, FCount: Integer;
  protected
    function DebugString: String; override;
  public
    constructor Create(ALine, ACount: Integer);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

{ TSynEditUndoTxtInsert }
 function TSynEditUndoTxtInsert.DebugString: String;
begin
  Result := 'X='+dbgs(FPosX) + ' Y='+ dbgs(FPosY) + ' len=' + dbgs(FLen);
end;

constructor TSynEditUndoTxtInsert.Create(APosX, APosY, ALen: Integer);
begin
  FPosX := APosX;
  FPosY := APosY;
  FLen  := ALen;
  {$IFDEF SynUndoDebug}debugln(['---  Undo Insert ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtInsert.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebug}if Result then debugln(['---  Undo Perform ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
  if Result then
    TSynEditStringList(Caller).EditDelete(FPosX, FPosY, FLen);
end;

{ TSynEditUndoTxtDelete }
 function TSynEditUndoTxtDelete.DebugString: String;
begin
  Result := 'X='+dbgs(FPosX) + ' Y='+ dbgs(FPosY) + ' text=' + FText;
end;

constructor TSynEditUndoTxtDelete.Create(APosX, APosY: Integer; AText: String);
begin
  FPosX := APosX;
  FPosY := APosY;
  FText := AText;
  {$IFDEF SynUndoDebug}debugln(['---  Undo Insert ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtDelete.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebug}if Result then debugln(['---  Undo Perform ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
  if Result then
    TSynEditStringList(Caller).EditInsert(FPosX, FPosY, FText);
end;

{ TSynEditUndoTxtLineBreak }
 function TSynEditUndoTxtLineBreak.DebugString: String;
begin
  Result := ' Y='+ dbgs(FPosY);
end;

constructor TSynEditUndoTxtLineBreak.Create(APosY: Integer);
begin
  FPosY := APosY;
  {$IFDEF SynUndoDebug}debugln(['---  Undo Insert ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtLineBreak.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebug}if Result then debugln(['---  Undo Perform ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
  if Result then
    TSynEditStringList(Caller).EditLineJoin(FPosY)
end;

{ TSynEditUndoTxtLineJoin }
 function TSynEditUndoTxtLineJoin.DebugString: String;
begin
  Result := 'X='+dbgs(FPosX) + ' Y='+ dbgs(FPosY);
end;

constructor TSynEditUndoTxtLineJoin.Create(APosX, APosY: Integer);
begin
  FPosX := APosX;
  FPosY := APosY;
  {$IFDEF SynUndoDebug}debugln(['---  Undo Insert ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtLineJoin.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebug}if Result then debugln(['---  Undo Perform ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
  if Result then
    TSynEditStringList(Caller).EditLineBreak(FPosX, FPosY)
end;

{ TSynEditUndoTxtLinesIns }
 function TSynEditUndoTxtLinesIns.DebugString: String;
begin
  Result := 'Y='+dbgs(FPosY) + ' Cnt='+ dbgs(FCount);
end;

constructor TSynEditUndoTxtLinesIns.Create(ALine, ACount: Integer);
begin
  FPosY  := ALine;
  FCount := ACount;
  {$IFDEF SynUndoDebug}debugln(['---  Undo Insert ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtLinesIns.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebug}if Result then debugln(['---  Undo Perform ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
  if Result then
    TSynEditStringList(Caller).UndoEditLinesDelete(FPosY, FCount)
end;

{ TSynEditUndoTxtLinesDel }
 function TSynEditUndoTxtLinesDel.DebugString: String;
begin
  Result := 'Y='+dbgs(FPosY) + ' Cnt='+ dbgs(FCount);
end;

constructor TSynEditUndoTxtLinesDel.Create(ALine, ACount: Integer);
begin
  FPosY  := ALine;
  FCount := ACount;
  {$IFDEF SynUndoDebug}debugln(['---  Undo Insert ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtLinesDel.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebug}if Result then debugln(['---  Undo Perform ',DbgSName(self),dbgs(Self), ' - ', DebugString]);{$ENDIF}
  if Result then
    TSynEditStringList(Caller).EditLinesInsert(FPosY, FCount)
end;


{ TSynEditStringList }

procedure ListIndexOutOfBounds(Index: integer);
begin
  raise ESynEditStringList.CreateFmt(SListIndexOutOfBounds, [Index]);
end;

constructor TSynEditStringList.Create;
var
  r: TSynEditNotifyReason;
begin
  fList := TSynEditStringMemory.Create;

  FAttachedSynEditList := TFPList.Create;
  FUndoList := TSynEditUndoList.Create;
  fUndoList.OnAddedUndo := {$IFDEF FPC}@{$ENDIF}UndoRedoAdded;
  FRedoList := TSynEditUndoList.Create;
  fRedoList.OnAddedUndo := {$IFDEF FPC}@{$ENDIF}UndoRedoAdded;
  FIsUndoing := False;
  FIsRedoing := False;
  FModified := False;

  for r := low(TSynEditNotifyReason) to high(TSynEditNotifyReason)
  do case r of
    senrLineCount, senrLineChange, senrHighlightChanged:
      FNotifyLists[r] := TLineRangeNotificationList.Create;
    senrEditAction:
      FNotifyLists[r] := TLineEditNotificationList.Create;
    else
      FNotifyLists[r] := TSynMethodList.Create;
  end;

  for r := low(TSynEditNotifyReason) to high(TSynEditNotifyReason) do
    FIgnoreSendNotification[r] := 0;
  inherited Create;
  fDosFileFormat := TRUE;
{begin}                                                                         //mh 2000-10-19
  fIndexOfLongestLine := -1;
{end}                                                                           //mh 2000-10-19
end;

destructor TSynEditStringList.Destroy;
var
  i: TSynEditNotifyReason;
begin
  inherited Destroy;
  SetCount(0);
  SetCapacity(0);
  for i := low(TSynEditNotifyReason) to high(TSynEditNotifyReason) do
    FreeAndNil(FNotifyLists[i]);
  FreeAndNil(FUndoList);
  FreeAndNil(FRedoList);
  FreeAndNil(FAttachedSynEditList);

  FreeAndNil(fList);
end;

function TSynEditStringList.Add(const S: string): integer;
begin
  BeginUpdate;
  Result := Count;
  InsertItem(Result, S);
  SendNotification(senrLineCount, self, Result, Count - Result);
  EndUpdate;
end;

procedure TSynEditStringList.AddStrings(AStrings: TStrings);
var
  i, FirstAdded: integer;
begin
{begin}                                                                         //mh 2000-10-19
  if AStrings.Count > 0 then begin
    fIndexOfLongestLine := -1;
    BeginUpdate;
    try
      i := Count + AStrings.Count;
      if i > Capacity then
        SetCapacity((i + 15) and (not 15));
      FirstAdded := Count;
      for i := 0 to AStrings.Count - 1 do begin
        SetCount(Count + 1);
        with fList do begin
          Strings[Count-1] := AStrings[i];
          Objects[Count-1] := AStrings.Objects[i];
        end;
        Flags[Count-1] := [];
      end;
      SendNotification(senrLineCount, self, FirstAdded, Count - FirstAdded);
    finally
      EndUpdate;
    end;
  end;
{end}                                                                           //mh 2000-10-19
end;

procedure TSynEditStringList.Clear;
var
  c: Integer;
begin
  c := Count;
  if c <> 0 then begin
    BeginUpdate;
    SetCount(0);
    SetCapacity(0);
    SendNotification(senrLineCount, self, 0, -c);
    SendNotification(senrCleared, Self);
    EndUpdate;
  end;
  fIndexOfLongestLine := -1;
end;

procedure TSynEditStringList.Delete(Index: integer);
begin
  // Ensure correct index, so DeleteLines will not throw exception
  if (Index < 0) or (Index >= Count) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  FList.DeleteRows(Index, 1);
  IncreaseTextChangeStamp;
  fIndexOfLongestLine := -1;
  SendNotification(senrLineCount, self, Index, -1);
  EndUpdate;
end;

procedure TSynEditStringList.DeleteLines(Index, NumLines: Integer);
begin
  if NumLines > 0 then begin
    // Ensure correct index, so DeleteLines will not throw exception
    if (Index < 0) or (Index + NumLines > Count) then
      ListIndexOutOfBounds(Index);
    BeginUpdate;
    FList.DeleteRows(Index, NumLines);
    IncreaseTextChangeStamp;
    SendNotification(senrLineCount, self, Index, -NumLines);
    EndUpdate;
  end;
end;

function TSynEditStringList.GetFlags(Index: Integer): TSynEditStringFlags;
begin
  if (Index >= 0) and (Index < Count) then
    Result := FList.Flags[Index]
  else
    Result := [];
end;

function TSynEditStringList.GetAttachedSynEdits(Index: Integer): TSynEditBase;
begin
  Result := TSynEditBase(FAttachedSynEditList[Index]);
end;

function TSynEditStringList.Get(Index: integer): string;
begin
  if (Index >= 0) and (Index < Count) then
    Result := fList[Index]
  else
    Result := '';
end;

function TSynEditStringList.GetCapacity: integer;
begin
  Result := fList.Capacity;
end;

function TSynEditStringList.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TSynEditStringList.SetCount(const AValue: Integer);
begin
  IncreaseTextChangeStamp;
  fList.Count := AValue;
end;

{begin}                                                                         //mh 2000-10-19
function TSynEditStringList.GetExpandedString(Index: integer): string;
begin
  if (Index >= 0) and (Index < Count) then begin
    Result := FList[Index];
  end else
    Result := '';
end;

function TSynEditStringList.GetLengthOfLongestLine: integer;                    //mh 2000-10-19
var
  i, j, MaxLen: integer;
begin
  if fIndexOfLongestLine < 0 then begin
    MaxLen := 0;
    if Count > 0 then begin
      for i := 0 to Count - 1 do begin
        j := length(FList[i]);
        if j > MaxLen then begin
          MaxLen := j;
          fIndexOfLongestLine := i;
        end;
      end;
    end;
  end;
  if (fIndexOfLongestLine >= 0) and (fIndexOfLongestLine < Count) then
    Result := length(FList[fIndexOfLongestLine])
  else
    Result := 0;
end;

function TSynEditStringList.GetTextChangeStamp: int64;
begin
  Result := FTextChangeStamp;
end;

function TSynEditStringList.GetRedoList: TSynEditUndoList;
begin
  Result := fRedoList;
end;

function TSynEditStringList.GetUndoList: TSynEditUndoList;
begin
  Result := fUndoList;
end;

function TSynEditStringList.GetCurUndoList: TSynEditUndoList;
begin
  if FIsUndoing then
    Result := fRedoList
  else
    Result := fUndoList;
end;

procedure TSynEditStringList.SetIsUndoing(const AValue: Boolean);
begin
  FIsUndoing := AValue;
end;

function TSynEditStringList.GetIsUndoing: Boolean;
begin
  Result := FIsUndoing;
end;

procedure TSynEditStringList.SetIsRedoing(const AValue: Boolean);
begin
  FIsRedoing := AValue;
end;

function TSynEditStringList.GetIsRedoing: Boolean;
begin
  Result := FIsRedoing;
end;

procedure TSynEditStringList.UndoRedoAdded(Sender: TObject);
begin
  // we have to clear the redo information, since adding undo info removes
  // the necessary context to undo earlier edit actions
  if (Sender = fUndoList) and not (fUndoList.IsInsideRedo) then
    fRedoList.Clear;
  if fUndoList.UnModifiedMarkerExists then
    Modified := not fUndoList.IsTopMarkedAsUnmodified
  else if fRedoList.UnModifiedMarkerExists then
    Modified := not fRedoList.IsTopMarkedAsUnmodified
  else
    Modified := fUndoList.CanUndo or fUndoList.FullUndoImpossible;

  SendNotification(senrUndoRedoAdded, Sender);
end;

// Maps the Physical Width (ScreenCells) to each character
// Multibyte Chars have thw width on the first byte, and a 0 Width for all other bytes
procedure TSynEditStringList.DoGetPhysicalCharWidths(Line: PChar;
  LineLen, Index: Integer; PWidths: PPhysicalCharWidth);
var
  i, j: Integer;
begin
  if not IsUtf8 then begin
    for i := 0 to LineLen-1 do
      PWidths[i] := 1;
    exit;
  end;

  j := 0;
  for i := 0 to LineLen-1 do begin
    if j = 0 then begin
      PWidths^ := 1;
      j := UTF8CharacterLength(Line);
      inc(Line, j);
    end else begin
      PWidths^ := 0;
    end;
    dec(j);
    inc(PWidths);
  end;
end;

procedure TSynEditStringList.AttachSynEdit(AEdit: TSynEditBase);
begin
  if FAttachedSynEditList.IndexOf(AEdit) < 0 then
    FAttachedSynEditList.Add(AEdit);
end;

procedure TSynEditStringList.DetachSynEdit(AEdit: TSynEditBase);
begin
  FAttachedSynEditList.Remove(AEdit);
end;

function TSynEditStringList.AttachedSynEditCount: Integer;
begin
  Result := FAttachedSynEditList.Count;
end;

function TSynEditStringList.GetObject(Index: integer): TObject;
begin
  if (Index >= 0) and (Index < Count) then
    Result := fList.Objects[Index]
  else
    Result := nil;
end;

function TSynEditStringList.GetRange(Index: Pointer): TSynManagedStorageMem;
begin
  Result := FList.RangeList[Index];
end;

procedure TSynEditStringList.Grow;
var
  Delta: Integer;
begin
  if Capacity > 64 then
    Delta := Capacity div 4
  else
    Delta := 16;
  SetCapacity(Capacity + Delta);
end;

procedure TSynEditStringList.Insert(Index: integer; const S: string);
var
  OldCnt : integer;
begin
  if (Index < 0) or (Index > Count) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  OldCnt:=Count;
  InsertItem(Index, S);
  SendNotification(senrLineCount, self, Index, Count - OldCnt);
  EndUpdate;
end;

procedure TSynEditStringList.InsertItem(Index: integer; const S: string);
begin
  // Ensure correct index, so DeleteLines will not throw exception
  if (Index < 0) or (Index > Count) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  if Count = Capacity then
    Grow;
  FList.InsertRows(Index, 1);
  IncreaseTextChangeStamp;
  fIndexOfLongestLine := -1;                                                    //mh 2000-10-19
  fList[Index] := S;
  FList.Objects[Index] := nil;
  Flags[Index] := [];
  EndUpdate;
end;

{begin}                                                                         // DJLP 2000-11-01
procedure TSynEditStringList.InsertLines(Index, NumLines: integer);
begin
  if NumLines > 0 then begin
    // Ensure correct index, so DeleteLines will not throw exception
    if (Index < 0) or (Index > Count) then
      ListIndexOutOfBounds(Index);
    BeginUpdate;
    try
      if Capacity<Count + NumLines then
        SetCapacity(Count + NumLines);
      FList.InsertRows(Index, NumLines);
      IncreaseTextChangeStamp;
      SendNotification(senrLineCount, self, Index, NumLines);
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSynEditStringList.InsertStrings(Index: integer;
  NewStrings: TStrings);
var
  i, Cnt: integer;
begin
  Cnt := NewStrings.Count;
  if Cnt > 0 then begin
    BeginUpdate;
    try
    InsertLines(Index, Cnt);
    for i := 0 to Cnt - 1 do
      Strings[Index + i] := NewStrings[i];
    finally
      EndUpdate;
    end;
  end;
end;

function TSynEditStringList.GetPChar(ALineIndex: Integer; out ALen: Integer): PChar;
begin
  Result := FList.GetPChar(ALineIndex, ALen);
end;

{end}                                                                           // DJLP 2000-11-01

procedure TSynEditStringList.Put(Index: integer; const S: string);
begin
  if (Index = 0) and (Count = 0) then
    Add(S)
  else begin
    if (Index < 0) or (Index >= Count) then
      ListIndexOutOfBounds(Index);
    BeginUpdate;
    fIndexOfLongestLine := -1;
    FList[Index] := S;
    IncreaseTextChangeStamp;
    SendNotification(senrLineChange, self, Index, 1);
    EndUpdate;
  end;
end;

procedure TSynEditStringList.PutObject(Index: integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= Count) then
    ListIndexOutOfBounds(Index);
  if fList.Objects[Index] = AObject then exit;
  BeginUpdate;
  fList.Objects[Index]:= AObject;
  EndUpdate;
end;

procedure TSynEditStringList.PutRange(Index: Pointer; const ARange: TSynManagedStorageMem);
begin
  FList.RangeList[Index] := ARange;
end;

procedure TSynEditStringList.SetFlags(Index: Integer; const AValue: TSynEditStringFlags);
begin
  FList.Flags[Index] := AValue;
end;

procedure TSynEditStringList.SetModified(const AValue: Boolean);
begin
  if AValue then
    IncreaseTextChangeStamp;
  if FModified = AValue then exit;
  FModified := AValue;
  if not FModified then
  begin
    // the current state should be the unmodified state.
    FUndoList.MarkTopAsUnmodified;
    FRedoList.MarkTopAsUnmodified;
  end;
  SendNotification(senrModifiedChanged, Self);
end;

procedure TSynEditStringList.SendCachedNotify;
begin
//debugln(['--- send cached notify  ', FCachedNotifyStart,' / ',FCachedNotifyCount]);
  if FCachedNotifyCount <> 0 then;
    TLineRangeNotificationList(FNotifyLists[senrLineCount])
      .CallRangeNotifyEvents(FCachedNotifySender, FCachedNotifyStart, FCachedNotifyCount);
  FCachedNotify := False;
end;

procedure TSynEditStringList.MarkModified(AFirst, ALast: Integer);
var
  Index: Integer;
begin
  for Index := AFirst - 1 to ALast - 1 do
    if (Index >= 0) and (Index < Count) then
      Flags[Index] := Flags[Index] + [sfModified] - [sfSaved];
end;

procedure TSynEditStringList.MarkSaved;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
    if sfModified in Flags[Index] then
      Flags[Index] := Flags[Index] + [sfSaved];
end;

procedure TSynEditStringList.AddGenericHandler(AReason: TSynEditNotifyReason; AHandler: TMethod);
begin
  FNotifyLists[AReason].Add(AHandler);
end;

procedure TSynEditStringList.RemoveGenericHandler(AReason: TSynEditNotifyReason; AHandler: TMethod);
begin
  FNotifyLists[AReason].Remove(AHandler);
end;

procedure TSynEditStringList.CopyHanlders(OtherLines: TSynEditStringList; AOwner: TObject = nil);
var
  i: TSynEditNotifyReason;
begin
  for i := low(TSynEditNotifyReason) to high(TSynEditNotifyReason) do
    FNotifyLists[i].AddCopyFrom(OtherLines.FNotifyLists[i], AOwner);
end;

procedure TSynEditStringList.RemoveHanlders(AOwner: TObject);
var
  i: TSynEditNotifyReason;
begin
  for i := low(TSynEditNotifyReason) to high(TSynEditNotifyReason) do
    FNotifyLists[i].RemoveAllMethodsOfObject(AOwner);
end;

procedure TSynEditStringList.SetCapacity(NewCapacity: integer);
begin
  if NewCapacity < Count then
    fList.Count := NewCapacity;
  fList.SetCapacity(NewCapacity);
  IncreaseTextChangeStamp;
end;

procedure TSynEditStringList.SetUpdateState(Updating: Boolean; Sender: TObject);
begin
  if FIsInDecPaintLock then exit;
  if Updating then begin
    SendNotification(senrIncPaintLock, Sender);       // DoIncPaintLock
    SendNotification(senrAfterIncPaintLock, Sender);
    FCachedNotify := False;
  end else begin
    if FCachedNotify then
      SendCachedNotify;
    FIsInDecPaintLock := True;
    try
      SendNotification(senrBeforeDecPaintLock, Sender);
      SendNotification(senrDecPaintLock, Sender);       // DoDecPaintLock
    finally
      FIsInDecPaintLock := False;
    end;
  end;
end;

procedure TSynEditStringList.EditInsert(LogX, LogY: Integer; AText: String);
var
  s: string;
begin
  s := Strings[LogY - 1];
  if LogX - 1 > Length(s) then begin
    AText := StringOfChar(' ', LogX - 1 - Length(s)) + AText;
    LogX := Length(s) + 1;
  end;
  Strings[LogY - 1] := copy(s,1, LogX - 1) + AText + copy(s, LogX, length(s));
  CurUndoList.AddChange(TSynEditUndoTxtInsert.Create(LogX, LogY, Length(AText)));
  MarkModified(LogY, LogY);
  SendNotification(senrEditAction, self, LogY, 0, LogX, length(AText), AText);
end;

function TSynEditStringList.EditDelete(LogX, LogY, ByteLen: Integer): String;
var
  s: string;
begin
  s := Strings[LogY - 1];
  if LogX - 1 > Length(s) then
    exit;
  Result := copy(s, LogX, ByteLen);
  Strings[LogY - 1] := copy(s,1, LogX - 1) + copy(s, LogX +  ByteLen, length(s));
  CurUndoList.AddChange(TSynEditUndoTxtDelete.Create(LogX, LogY, Result));
  MarkModified(LogY, LogY);
  SendNotification(senrEditAction, self, LogY, 0, LogX, -ByteLen, '');
end;

procedure TSynEditStringList.EditLineBreak(LogX, LogY: Integer);
var
  s: string;
begin
  if Count = 0 then Add('');
  s := Strings[LogY - 1];
  if LogX - 1 < length(s) then
    Strings[LogY - 1] := copy(s, 1, LogX - 1);
  Insert(LogY, copy(s, LogX, length(s)));
  CurUndoList.AddChange(TSynEditUndoTxtLineBreak.Create(LogY));
  MarkModified(LogY, LogY + 1);
  SendNotification(senrEditAction, self, LogY, 1, LogX, 0, '');
end;

procedure TSynEditStringList.EditLineJoin(LogY: Integer; FillText: String = '');
var
  t: string;
begin
  t := Strings[LogY - 1];
  if FillText <> ''  then
    EditInsert(1 + Length(t), LogY, FillText);
  CurUndoList.AddChange(TSynEditUndoTxtLineJoin.Create(1 + Length(Strings[LogY-1]),
                                                    LogY));
  t := t + FillText;
  Strings[LogY - 1] := t + Strings[LogY] ;
  Delete(LogY);
  MarkModified(LogY, LogY);
  SendNotification(senrEditAction, self, LogY, -1, 1+length(t), 0, '');
end;

procedure TSynEditStringList.EditLinesInsert(LogY, ACount: Integer;
  AText: String = '');
begin
  InsertLines(LogY - 1, ACount);
  CurUndoList.AddChange(TSynEditUndoTxtLinesIns.Create(LogY, ACount));
  SendNotification(senrEditAction, self, LogY, ACount, 1, 0, '');
  if AText <> '' then
    EditInsert(1, LogY, AText);
  MarkModified(LogY, LogY + ACount - 1);
end;

procedure TSynEditStringList.EditLinesDelete(LogY, ACount: Integer);
var
  i: Integer;
begin
  for i := LogY to LogY + ACount - 1 do
    EditDelete(1, i, length(Strings[i-1]));
  DeleteLines(LogY - 1, ACount);
  CurUndoList.AddChange(TSynEditUndoTxtLinesDel.Create(LogY, ACount));
  SendNotification(senrEditAction, self, LogY, -ACount, 1, 0, '');
end;

procedure TSynEditStringList.EditUndo(Item: TSynEditUndoItem);
begin
  EditRedo(Item);
end;

procedure TSynEditStringList.UndoEditLinesDelete(LogY, ACount: Integer);
begin
  CurUndoList.AddChange(TSynEditUndoTxtLinesDel.Create(LogY, ACount));
  DeleteLines(LogY - 1, ACount);
  SendNotification(senrEditAction, self, LogY, -ACount, 1, 0, '');
end;

procedure TSynEditStringList.IncreaseTextChangeStamp;
begin
  if FTextChangeStamp=High(FTextChangeStamp) then
    FTextChangeStamp:=Low(FTextChangeStamp)
  else
    inc(FTextChangeStamp);
end;

procedure TSynEditStringList.EditRedo(Item: TSynEditUndoItem);
begin
  Item.PerformUndo(self);
end;

procedure TSynEditStringList.SendNotification(AReason: TSynEditNotifyReason;
  ASender: TSynEditStrings; aIndex, aCount: Integer;
  aBytePos: Integer = -1; aLen: Integer = 0; aTxt: String = '');
begin
  if FIgnoreSendNotification[AReason] > 0 then exit;

  if IsUpdating then begin;
    if AReason = senrLineCount then begin
      // maybe cache and combine
      if not FCachedNotify then begin
        FCachedNotify       := True;
        FCachedNotifySender := ASender;
        FCachedNotifyStart  := aIndex;
        FCachedNotifyCount  := aCount;
        exit;
      end
      else
      if (FCachedNotifySender = ASender) and (aIndex >= FCachedNotifyStart) and
         (FCachedNotifyCount > 0) and
         (aIndex <= FCachedNotifyStart + FCachedNotifyCount) and
         ((aCount > 0) or (aIndex - aCount <= FCachedNotifyStart + FCachedNotifyCount))
      then begin
        FCachedNotifyCount := FCachedNotifyCount + aCount;
        if FCachedNotifyCount = 0 then
          FCachedNotify := False;
        exit;
      end;
    end;
    if FCachedNotify and (AReason = senrLineChange) and
       (ASender = FCachedNotifySender) and (FCachedNotifyCount > 0) and
       (aIndex >= FCachedNotifyStart) and
       (aIndex + aCount {- 1} <= FCachedNotifyStart + FCachedNotifyCount {- 1})
    then
      exit; // Will send senrLineCount instead

    if FCachedNotify then
      SendCachedNotify;
  end;

  case AReason of
    senrLineChange, senrLineCount, senrHighlightChanged:
      TLineRangeNotificationList(FNotifyLists[AReason])
        .CallRangeNotifyEvents(ASender, aIndex, aCount);
    senrEditAction:
        // aindex is mis-named (linepos) for edit action
        TLineEditNotificationList(FNotifyLists[AReason])
          .CallRangeNotifyEvents(ASender, aIndex, aBytePos, aLen, aCount, aTxt);
    else
      raise Exception.Create('Invalid');
  end;
end;

procedure TSynEditStringList.SendNotification(AReason: TSynEditNotifyReason;
  ASender: TObject);
begin
  if FCachedNotify then
    SendCachedNotify;
  if AReason in [senrLineChange, senrLineCount, senrHighlightChanged, senrEditAction] then
    raise Exception.Create('Invalid');
  FNotifyLists[AReason].CallNotifyEvents(ASender);
end;

procedure TSynEditStringList.FlushNotificationCache;
begin
  if FCachedNotify then
    SendCachedNotify;
end;

procedure TSynEditStringList.IgnoreSendNotification(AReason: TSynEditNotifyReason;
  IncIgnore: Boolean);
begin
  if IncIgnore then
    inc(FIgnoreSendNotification[AReason])
  else
  if FIgnoreSendNotification[AReason] > 0 then
    dec(FIgnoreSendNotification[AReason])
end;

{ TSynEditStringMemory }
type
  PObject = ^TObject;

constructor TSynEditStringMemory.Create;
const
  FlagSize = ((SizeOf(TSynEditStringFlags) + 1 ) Div 2) * 2; // ensure boundary
begin
  inherited Create;
  ItemSize := SizeOf(String) + SizeOf(TObject) + FlagSize;
  FRangeList := TSynManagedStorageMemList.Create;
  FRangeListLock := 0;
end;

destructor TSynEditStringMemory.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FRangeList);
end;

procedure TSynEditStringMemory.InsertRows(AIndex, ACount: Integer);
begin
  // Managed lists to get Mave, Count, instead of InsertRows
  inc(FRangeListLock);
  inherited InsertRows(AIndex, ACount);
  dec(FRangeListLock);
  FRangeList.CallInsertedLines(AIndex, ACount);
end;

procedure TSynEditStringMemory.DeleteRows(AIndex, ACount: Integer);
begin
  // Managed lists to get Mave, Count, instead of InsertRows
  inc(FRangeListLock);
  inherited DeleteRows(AIndex, ACount);
  dec(FRangeListLock);
  FRangeList.CallDeletedLines(AIndex, ACount);
end;

function TSynEditStringMemory.GetPChar(ALineIndex: Integer; out ALen: Integer): PChar;
begin
  ALen   := length((PString(ItemPointer[ALineIndex]))^);
  Result := (PPChar(ItemPointer[ALineIndex]))^;
end;

procedure TSynEditStringMemory.Move(AFrom, ATo, ALen: Integer);
var
  Len, i: Integer;
begin
  if ATo < AFrom then begin
    Len := Min(ALen, AFrom-ATo);
    for i:=ATo to ATo + Len -1 do Strings[i]:='';
  end else begin
    Len := Min(ALen, ATo-AFrom);
    for i:=ATo+Alen-Len to ATo+ALen -1 do Strings[i]:='';
  end;
  inherited Move(AFrom, ATo, ALen);
  FRangeList.CallMove(AFrom, ATo, ALen);
end;

procedure TSynEditStringMemory.SetCount(const AValue: Integer);
var
  OldCount, i : Integer;
begin
  If Count = AValue then exit;
  for i:= AValue to Count-1 do
    Strings[i]:='';
  OldCount := Count;
  inherited SetCount(AValue);
  FRangeList.ChildCounts := AValue;
  if FRangeListLock = 0 then begin
    if OldCount > Count then
      FRangeList.CallDeletedLines(Count, OldCount - Count)
    else
      FRangeList.CallInsertedLines(OldCount, Count - OldCount);
  end;
end;

function TSynEditStringMemory.GetString(Index: Integer): String;
begin
  Result := (PString(ItemPointer[Index]))^;
end;

procedure TSynEditStringMemory.SetFlags(Index: Integer; const AValue: TSynEditStringFlags);
begin
  (PSynEditStringFlags(ItemPointer[Index] + SizeOf(String) + SizeOf(TObject) ))^ := AValue;
end;

procedure TSynEditStringMemory.SetString(Index: Integer; const AValue: String);
begin
  (PString(ItemPointer[Index]))^ := AValue;
  if FRangeListLock = 0 then
    FRangeList.CallLineTextChanged(Index);
end;

procedure TSynEditStringMemory.SetCapacity(const AValue: Integer);
begin
  inherited SetCapacity(AValue);
  FRangeList.ChildCapacities := AValue;
end;

function TSynEditStringMemory.GetObject(Index: Integer): TObject;
begin
  Result := (PObject(ItemPointer[Index] + SizeOf(String)))^;
end;

function TSynEditStringMemory.GetFlags(Index: Integer): TSynEditStringFlags;
begin
  Result := (PSynEditStringFlags(ItemPointer[Index] + SizeOf(String) + SizeOf(TObject) ))^;
end;

function TSynEditStringMemory.GetRange(Index: Pointer): TSynManagedStorageMem;
begin
  Result := FRangeList[Index];
end;

procedure TSynEditStringMemory.SetObject(Index: Integer; const AValue: TObject);
begin
  (PObject(ItemPointer[Index] + SizeOf(String)))^ := AValue;
end;

procedure TSynEditStringMemory.SetRange(Index: Pointer; const AValue: TSynManagedStorageMem);
begin
  FRangeList[Index] := AValue;

  if AValue <> nil then begin
    AValue.Capacity := Capacity;
    AValue.Count := Count;
  end;
end;

{ TLineRangeNotificationList }

procedure TLineRangeNotificationList.CallRangeNotifyEvents(Sender: TSynEditStrings;
  aIndex, aCount: Integer);
var
  i: LongInt;
begin
  i:=Count;
  while NextDownIndex(i) do
    TStringListLineCountEvent(Items[i])(Sender, aIndex, aCount);
end;

{ TLineEditNotificationList }

procedure TLineEditNotificationList.CallRangeNotifyEvents(Sender: TSynEditStrings;
  aLinePos, aBytePos, aCount, aLineBrkCnt: Integer; aText: String);
var
  i: LongInt;
begin
  i:=Count;
  while NextDownIndex(i) do
    TStringListLineEditEvent(Items[i])(Sender, aLinePos, aBytePos, aCount,
                             aLineBrkCnt, aText);
end;

end.

