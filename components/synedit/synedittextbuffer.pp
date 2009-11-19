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
  Classes, SysUtils, SynEditTextBase,
  FileUtil, LCLProc, LCLIntf, LCLType,
  SynEditTypes, SynEditMiscProcs, SynEditMiscClasses;

const
  NullRange = TSynEditRange(nil);

type
  TSynEditFlagsClass = class end; // For Register

  TSynEditStringFlag = (
    sfModified,              // a line is modified and not saved after
    sfSaved,                 // a line is modified and saved after
    sfDebugMark              // a line where debugger can stop (for lazarus only)
  );
  TSynEditStringFlags = set of TSynEditStringFlag;

  TStringListIndexEvent = procedure(Index: Integer) of object;

  TSynEditStringAttribute = record
    Index: TClass;
    Size: Word;
    Pos: Integer;
  end;

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

  TSynEditStringRangeEntry = record
    Index: TClass;
    Data: TSynEditStorageMem;
  end;

  TSynEditStringMemory = class(TSynEditStorageMem)
  private
    FAttributeSize: Integer;
    FRangeList: Array of TSynEditStringRangeEntry;
    function GetAttribute(Index: Integer; Pos: Integer; Size: Word): Pointer;
    function GetAttributeSize: Integer;
    function GetObject(Index: Integer): TObject;
    function GetRange(Index: TClass): TSynEditStorageMem;
    function GetString(Index: Integer): String;
    procedure SetAttribute(Index: Integer; Pos: Integer; Size: Word; const AValue: Pointer);
    procedure SetAttributeSize(const AValue: Integer);
    procedure SetObject(Index: Integer; const AValue: TObject);
    procedure SetRange(Index: TClass; const AValue: TSynEditStorageMem);
    procedure SetString(Index: Integer; const AValue: String);
  protected
    procedure SetCount(const AValue: Integer); override;
    function ItemSize: Integer; override;
    procedure SetCapacity(const AValue: Integer); override;
  public
    constructor Create;
    procedure Move(AFrom, ATo, ALen: Integer); override;

    property Strings[Index: Integer]: String read GetString write SetString; default;
    property Objects[Index: Integer]: TObject read GetObject write SetObject;
    property Attribute[Index: Integer; Pos: Integer; Size: Word]: Pointer
      read  GetAttribute write SetAttribute;
    property AttributeSize: Integer read  GetAttributeSize write SetAttributeSize;

    property RangeList[Index: TClass]: TSynEditStorageMem read GetRange write SetRange;
  end;

  { TSynEditStringList }

  TSynEditStringList = class(TSynEditStrings)
  private
    FList: TSynEditStringMemory;
    FAttributeList: Array of TSynEditStringAttribute;

    FLineRangeNotificationList: TLineRangeNotificationList; // LineCount
    FLineChangeNotificationList: TLineRangeNotificationList; // ContentChange (not called on add...)
    FLineEditNotificationList: TLineEditNotificationList;
    FRefCount: integer;
    FUndoRedoAddedNotificationList: TSynMethodList;
    FOnChangeList: TSynMethodList;
    FOnChangingList: TSynMethodList;
    FOnClearedList: TSynMethodList;

    FIgnoreSendNotification: array [TSynEditNotifyReason] of Integer;
    fDosFileFormat: boolean;
    fIndexOfLongestLine: integer;
    FRedoList: TSynEditUndoList;
    FUndoList: TSynEditUndoList;
    FIsUndoing, FIsRedoing: Boolean;

    function GetFlags(Index: Integer): TSynEditStringFlags;
    procedure Grow;
    procedure InsertItem(Index: integer; const S: string);
    function ClassIndexForAttribute(AttrIndex: TClass): Integer;
    Procedure SetAttributeSize(NewSize: Integer);
    procedure SetFlags(Index: Integer; const AValue: TSynEditStringFlags);
  protected
    function GetExpandedString(Index: integer): string; override;
    function GetLengthOfLongestLine: integer; override;

    function GetRedoList: TSynEditUndoList; override;
    function GetUndoList: TSynEditUndoList; override;
    function GetCurUndoList: TSynEditUndoList; override;
    procedure SetIsUndoing(const AValue: Boolean); override;
    function  GetIsUndoing: Boolean; override;
    procedure SetIsRedoing(const AValue: Boolean); override;
    function  GetIsRedoing: Boolean; override;
    procedure UndoRedoAdded(Sender: TObject);
    procedure SendNotification(AReason: TSynEditNotifyReason;
                ASender: TSynEditStrings; aIndex, aCount: Integer;
                aBytePos: Integer = -1; aLen: Integer = 0; aTxt: String = ''); override;
    procedure IgnoreSendNotification(AReason: TSynEditNotifyReason;
                                     IncIgnore: Boolean); override;

    function GetRange(Index: TClass): TSynEditStorageMem; override;
    procedure PutRange(Index: TClass; const ARange: TSynEditStorageMem); override;
    function  GetAttribute(const Owner: TClass; const Index: Integer): Pointer; override;
    procedure SetAttribute(const Owner: TClass; const Index: Integer; const AValue: Pointer); override;
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
    procedure SetUpdateState(Updating: Boolean); override;

    procedure UndoEditLinesDelete(LogY, ACount: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): integer; override;
    procedure AddStrings(AStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure RegisterAttribute(const Index: TClass; const Size: Word); override;
    procedure DeleteLines(Index, NumLines: integer); override;
    procedure Insert(Index: integer; const S: string); override;
    procedure InsertLines(Index, NumLines: integer); override;
    procedure InsertStrings(Index: integer; NewStrings: TStrings); override;
    procedure MarkModified(AFirst, ALast: Integer);
    procedure MarkSaved;
    procedure SetDebugMarks(AFirst, ALast: Integer);
    procedure ClearDebugMarks;
    procedure AddGenericHandler(AReason: TSynEditNotifyReason;
                AHandler: TMethod); override;
    procedure RemoveGenericHandler(AReason: TSynEditNotifyReason;
                AHandler: TMethod); override;
    {$IFDEF SynDualView}
    procedure CopyHanlders(OtherLines: TSynEditStringList; AOwner: TObject = nil);
    procedure RemoveHanlders(AOwner: TObject);
    {$ENDIF}
    procedure IncRefCount;
    procedure DecRefCount;
    property  RefCount: integer read FRefCount;
   function GetPhysicalCharWidths(const Line: String; Index: Integer): TPhysicalCharWidths; override;
  public
    property DosFileFormat: boolean read fDosFileFormat write fDosFileFormat;    
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
    property Flags[Index: Integer]: TSynEditStringFlags read GetFlags
      write SetFlags;
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
  public
    constructor Create(APosX, APosY, ALen: Integer);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoTxtDelete }

  TSynEditUndoTxtDelete = class(TSynEditUndoItem)
  private
    FPosX, FPosY: Integer;
    FText: String;
  public
    constructor Create(APosX, APosY: Integer; AText: String);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoTxtLineBreak }

  TSynEditUndoTxtLineBreak = class(TSynEditUndoItem)
  private
    FPosY: Integer;
  public
    constructor Create(APosY: Integer);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoTxtLineJoin }

  TSynEditUndoTxtLineJoin = class(TSynEditUndoItem)
  private
    FPosX, FPosY: Integer;
  public
    constructor Create(APosX, APosY: Integer);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoTxtLinesIns }

  TSynEditUndoTxtLinesIns = class(TSynEditUndoItem)
  private
    FPosY, FCount: Integer;
  public
    constructor Create(ALine, ACount: Integer);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

  { TSynEditUndoTxtLinesDel }

  TSynEditUndoTxtLinesDel = class(TSynEditUndoItem)
  private
    FPosY, FCount: Integer;
  public
    constructor Create(ALine, ACount: Integer);
    function PerformUndo(Caller: TObject): Boolean; override;
  end;

{ TSynEditUndoTxtInsert }

constructor TSynEditUndoTxtInsert.Create(APosX, APosY, ALen: Integer);
begin
  FPosX := APosX;
  FPosY := APosY;
  FLen  := ALen;
end;

function TSynEditUndoTxtInsert.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  if Result then
    TSynEditStringList(Caller).EditDelete(FPosX, FPosY, FLen);
end;

{ TSynEditUndoTxtDelete }

constructor TSynEditUndoTxtDelete.Create(APosX, APosY: Integer; AText: String);
begin
  FPosX := APosX;
  FPosY := APosY;
  FText := AText;
end;

function TSynEditUndoTxtDelete.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  if Result then
    TSynEditStringList(Caller).EditInsert(FPosX, FPosY, FText);
end;

{ TSynEditUndoTxtLineBreak }

constructor TSynEditUndoTxtLineBreak.Create(APosY: Integer);
begin
  FPosY := APosY;
end;

function TSynEditUndoTxtLineBreak.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  if Result then
    TSynEditStringList(Caller).EditLineJoin(FPosY)
end;

{ TSynEditUndoTxtLineJoin }

constructor TSynEditUndoTxtLineJoin.Create(APosX, APosY: Integer);
begin
  FPosX := APosX;
  FPosY := APosY;
end;

function TSynEditUndoTxtLineJoin.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  if Result then
    TSynEditStringList(Caller).EditLineBreak(FPosX, FPosY)
end;

{ TSynEditUndoTxtLinesIns }

constructor TSynEditUndoTxtLinesIns.Create(ALine, ACount: Integer);
begin
  FPosY  := ALine;
  FCount := ACount;
end;

function TSynEditUndoTxtLinesIns.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  if Result then
    TSynEditStringList(Caller).UndoEditLinesDelete(FPosY, FCount)
end;

{ TSynEditUndoTxtLinesDel }

constructor TSynEditUndoTxtLinesDel.Create(ALine, ACount: Integer);
begin
  FPosY  := ALine;
  FCount := ACount;
end;

function TSynEditUndoTxtLinesDel.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
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
  FRefCount := 1;

  FUndoList := TSynEditUndoList.Create;
  fUndoList.OnAddedUndo := {$IFDEF FPC}@{$ENDIF}UndoRedoAdded;
  FRedoList := TSynEditUndoList.Create;
  fRedoList.OnAddedUndo := {$IFDEF FPC}@{$ENDIF}UndoRedoAdded;
  FIsUndoing := False;
  FIsRedoing := False;

  FLineRangeNotificationList := TLineRangeNotificationList.Create;
  FLineChangeNotificationList := TLineRangeNotificationList.Create;
  FLineEditNotificationList := TLineEditNotificationList.Create;
  FUndoRedoAddedNotificationList := TLineEditNotificationList.Create;
  FOnChangeList := TSynMethodList.Create;
  FOnChangingList := TSynMethodList.Create;
  FOnClearedList := TSynMethodList.Create;

  for r := low(TSynEditNotifyReason) to high(TSynEditNotifyReason) do
    FIgnoreSendNotification[r] := 0;
  inherited Create;
  SetAttributeSize(0);
  RegisterAttribute(TSynEditFlagsClass, SizeOf(TSynEditStringFlag));
  fDosFileFormat := TRUE;
{begin}                                                                         //mh 2000-10-19
  fIndexOfLongestLine := -1;
{end}                                                                           //mh 2000-10-19
end;

destructor TSynEditStringList.Destroy;
begin
  fAttributeList := nil;
  inherited Destroy;
  SetCount(0);
  SetCapacity(0);
  FreeAndNil(FLineRangeNotificationList);
  FreeAndNil(FLineChangeNotificationList);
  FreeAndNil(FLineEditNotificationList);
  FreeAndNil(FUndoRedoAddedNotificationList);
  FreeAndNil(FOnChangeList);
  FreeAndNil(FOnChangingList);
  FreeAndNil(FOnClearedList);
  FreeAndNil(FUndoList);
  FreeAndNil(FRedoList);

  FreeAndNil(fList);
end;

function TSynEditStringList.Add(const S: string): integer;
begin
  BeginUpdate;
  Result := Count;
  InsertItem(Result, S);
  FLineRangeNotificationList.CallRangeNotifyEvents(self, Result, Count - Result);
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
      FLineRangeNotificationList.CallRangeNotifyEvents(self, FirstAdded, Count - FirstAdded);
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
    FOnClearedList.CallNotifyEvents(Self);
    FLineRangeNotificationList.CallRangeNotifyEvents(self, 0, -c);
    EndUpdate;
  end;
  fIndexOfLongestLine := -1;
end;

procedure TSynEditStringList.Delete(Index: integer);
begin
  if (Index < 0) or (Index > Count) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  if Index < Count-1 then
    fList.Move(Index + 1, Index, Count-Index-1);
  SetCount(Count - 1);
  fIndexOfLongestLine := -1;
  FLineRangeNotificationList.CallRangeNotifyEvents(self, Index, -1);
  EndUpdate;
end;

procedure TSynEditStringList.DeleteLines(Index, NumLines: Integer);
var
  LinesAfter: integer;
begin
  if NumLines > 0 then begin
    if (Index < 0) or (Index >= Count) then
      ListIndexOutOfBounds(Index);
    LinesAfter := Count - (Index + NumLines);
    if LinesAfter < 0 then
      NumLines := Count - Index;
    if LinesAfter > 0 then begin
      BeginUpdate;
      try
          fList.Move(Index + NumLines, Index, LinesAfter);
      finally
        EndUpdate;
      end;
    end;
    SetCount(Count - NumLines);
    FLineRangeNotificationList.CallRangeNotifyEvents(self, Index, -NumLines);
  end;
end;

function TSynEditStringList.GetFlags(Index: Integer): TSynEditStringFlags;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TSynEditStringFlags(Integer(PtrUInt(GetAttribute(TSynEditFlagsClass, Index))))
  else
    Result := [];
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
  FUndoRedoAddedNotificationList.CallNotifyEvents(Sender);
end;

// Maps the Physical Width (ScreenCells) to each character
// Multibyte Chars have thw width on the first byte, and a 0 Width for all other bytes
function TSynEditStringList.GetPhysicalCharWidths(const Line: String; Index: Integer): TPhysicalCharWidths;
var
  i, j: Integer;
begin
  SetLength(Result, Length(Line));
  i := 0;
  j := 0;
  while i < length(Line) do begin
    if j > 0 then begin
      Result[i] := 0;
      dec(j);
    end else begin
      Result[i] := 1;
      if IsUtf8 then
        j := UTF8CharacterLength(@Line[i+1]) - 1;
    end;
    inc(i);
  end;
end;

function TSynEditStringList.GetObject(Index: integer): TObject;
begin
  if (Index >= 0) and (Index < Count) then
    Result := fList.Objects[Index]
  else
    Result := nil;
end;

function TSynEditStringList.GetRange(Index: TClass): TSynEditStorageMem;
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
  FLineRangeNotificationList.CallRangeNotifyEvents(self, Index, Count - OldCnt);
  EndUpdate;
end;

procedure TSynEditStringList.InsertItem(Index: integer; const S: string);
begin
  BeginUpdate;
  if Count = Capacity then
    Grow;
  if Index < Count then
    FList.Move(Index, Index+1, Count - Index);
  fIndexOfLongestLine := -1;                                                    //mh 2000-10-19
  SetCount(Count + 1);
  fList[Index] := S;
  FList.Objects[Index] := nil;
  Flags[Index] := [];
  EndUpdate;
end;

{begin}                                                                         // DJLP 2000-11-01
procedure TSynEditStringList.InsertLines(Index, NumLines: integer);
begin
  if NumLines > 0 then begin
    BeginUpdate;
    try
      if Capacity<Count + NumLines then
        SetCapacity(Count + NumLines);
      if Index < Count then
        FList.Move(Index, Index + NumLines, Count-Index);
      SetCount(Count + NumLines);
      FLineRangeNotificationList.CallRangeNotifyEvents(self, Index, NumLines);
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

procedure TSynEditStringList.PutRange(Index: TClass; const ARange: TSynEditStorageMem);
begin
  FList.RangeList[Index] := ARange;
end;

function TSynEditStringList.GetAttribute(const Owner: TClass; const Index: Integer): Pointer;
var
  i: Integer;
begin
  if (Index = 0) and (Count = 0) then
    exit(nil);
  if (Index < 0) or (Index >= Count) then
    ListIndexOutOfBounds(Index);
  i := ClassIndexForAttribute(Owner);
  if i < 0 then
    raise ESynEditStringList.CreateFmt('Unknown Attribute', []);
  Result := FList.Attribute[Index, FAttributeList[i].Pos, FAttributeList[i].Size]
end;

procedure TSynEditStringList.SetAttribute(const Owner: TClass; const Index: Integer; const AValue: Pointer);
var
  i: Integer;
begin
  if (Index = 0) and (Count = 0) then
    Add('');
  if (Index < 0) or (Index >= Count) then
    ListIndexOutOfBounds(Index);
  i := ClassIndexForAttribute(Owner);
  if i < 0 then
    raise ESynEditStringList.CreateFmt('Unknown Attribute', []);
  FList.Attribute[Index, FAttributeList[i].Pos, FAttributeList[i].Size] := AValue;
end;

procedure TSynEditStringList.RegisterAttribute(const Index: TClass; const Size: Word);
var
  i: Integer;
begin
  if ClassIndexForAttribute(Index) >= 0 then
    raise ESynEditStringList.CreateFmt('Duplicate Attribute', []);
  i := Length(fAttributeList);
  SetLength(fAttributeList, i+1);
  fAttributeList[i].Index := Index;
  fAttributeList[i].Size := Size;
  if i= 0 then
    fAttributeList[i].Pos := 0
  else
    fAttributeList[i].Pos := fAttributeList[i-1].Pos + fAttributeList[i-1].Size;
  SetAttributeSize(fAttributeList[i].Pos + Size);
end;

function TSynEditStringList.ClassIndexForAttribute(AttrIndex: TClass): Integer;
var
  i: Integer;
begin
  for i := 0 to high(fAttributeList) do
    if fAttributeList[i].Index = AttrIndex then
      exit(i);
  result := -1;
end;

procedure TSynEditStringList.SetAttributeSize(NewSize: Integer);
begin
  if FList.AttributeSize = NewSize then exit;
  if Count > 0 then
    raise ESynEditStringList.CreateFmt('Add Attribute only allowed with zero lines', []);
  FList.AttributeSize := NewSize;
end;

procedure TSynEditStringList.SetFlags(Index: Integer; const AValue: TSynEditStringFlags);
begin
  SetAttribute(TSynEditFlagsClass, Index, Pointer(PtrUInt(Integer(AValue))));
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

procedure TSynEditStringList.SetDebugMarks(AFirst, ALast: Integer);
var
  Index: Integer;
begin
  for Index := AFirst to ALast do
    if (Index >= 0) and (Index < Count) then
      Flags[Index] := Flags[Index] + [sfDebugMark];
end;

procedure TSynEditStringList.ClearDebugMarks;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
    Flags[Index] := Flags[Index] - [sfDebugMark];
end;

procedure TSynEditStringList.AddGenericHandler(AReason: TSynEditNotifyReason; AHandler: TMethod);
begin
  case AReason of
    senrLineChange : FLineChangeNotificationList.Add(AHandler);
    senrLineCount : FLineRangeNotificationList.Add(AHandler);
    senrTextEdit: FLineEditNotificationList.Add(TMethod(AHandler));
    senrBeginUpdate : FOnChangingList.Add(AHandler);
    senrEndUpdate : FOnChangeList.Add(AHandler);
    senrCleared : FOnClearedList.Add(AHandler);
    senrUndoRedoAdded : FUndoRedoAddedNotificationList.Add(AHandler);
  end;
end;

procedure TSynEditStringList.RemoveGenericHandler(AReason: TSynEditNotifyReason; AHandler: TMethod);
begin
  case AReason of
    senrLineChange : FLineChangeNotificationList.Remove(AHandler);
    senrLineCount : FLineRangeNotificationList.Remove(AHandler);
    senrTextEdit: FLineEditNotificationList.Remove(TMethod(AHandler));
    senrBeginUpdate : FOnChangingList.Remove(AHandler);
    senrEndUpdate : FOnChangeList.Remove(AHandler);
    senrCleared : FOnClearedList.Remove(AHandler);
    senrUndoRedoAdded : FUndoRedoAddedNotificationList.Remove(AHandler);
  end;
end;

{$IFDEF SynDualView}
procedure TSynEditStringList.CopyHanlders(OtherLines: TSynEditStringList; AOwner: TObject = nil);
begin
  FLineRangeNotificationList.AddCopyFrom(OtherLines.FLineRangeNotificationList, AOwner);
  FLineChangeNotificationList.AddCopyFrom(OtherLines.FLineChangeNotificationList, AOwner);
  FLineEditNotificationList.AddCopyFrom(OtherLines.FLineEditNotificationList, AOwner);
  FUndoRedoAddedNotificationList.AddCopyFrom(OtherLines.FUndoRedoAddedNotificationList, AOwner);
  FOnChangeList.AddCopyFrom(OtherLines.FOnChangeList, AOwner);
  FOnChangingList.AddCopyFrom(OtherLines.FOnChangingList, AOwner);
  FOnClearedList.AddCopyFrom(OtherLines.FOnClearedList, AOwner);
end;

procedure TSynEditStringList.RemoveHanlders(AOwner: TObject);
begin
  FLineRangeNotificationList.RemoveAllMethodsOfObject(AOwner);
  FLineChangeNotificationList.RemoveAllMethodsOfObject(AOwner);
  FLineEditNotificationList.RemoveAllMethodsOfObject(AOwner);
  FUndoRedoAddedNotificationList.RemoveAllMethodsOfObject(AOwner);
  FOnChangeList.RemoveAllMethodsOfObject(AOwner);
  FOnChangingList.RemoveAllMethodsOfObject(AOwner);
  FOnClearedList.RemoveAllMethodsOfObject(AOwner);
end;
{$ENDIF}

procedure TSynEditStringList.IncRefCount;
begin
  inc(FRefCount);
end;

procedure TSynEditStringList.DecRefCount;
begin
  dec(FRefCount);
end;

procedure TSynEditStringList.SetCapacity(NewCapacity: integer);
begin
  fList.SetCapacity(NewCapacity);
end;

procedure TSynEditStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then begin
    FOnChangingList.CallNotifyEvents(Self);
  end else begin
    FOnChangeList.CallNotifyEvents(Self);
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

procedure TSynEditStringList.EditRedo(Item: TSynEditUndoItem);
begin
  Item.PerformUndo(self);
end;

procedure TSynEditStringList.SendNotification(AReason: TSynEditNotifyReason;
  ASender: TSynEditStrings; aIndex, aCount: Integer;
  aBytePos: Integer = -1; aLen: Integer = 0; aTxt: String = '');
begin
  if FIgnoreSendNotification[AReason] > 0 then exit;
  case AReason of
    senrLineChange:
      FLineChangeNotificationList.CallRangeNotifyEvents(ASender, aIndex, aCount);
    senrLineCount:
      FLineRangeNotificationList.CallRangeNotifyEvents(ASender, aIndex, aCount);
    senrEditAction:
        FLineEditNotificationList.CallRangeNotifyEvents(ASender, aIndex, // aindex is mis-named (linepos) for edit action
                                                  aBytePos, aLen, aCount, aTxt);
  end;
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

const
  AttributeOfset = SizeOf(String) + SizeOf(TObject);

constructor TSynEditStringMemory.Create;
begin
  inherited Create;
  AttributeSize := 0;
  FRangeList := nil;
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
  for i := 0 to length(FRangeList) - 1 do
    FRangeList[i].Data.Move(AFrom, ATo, ALen);
end;

procedure TSynEditStringMemory.SetCount(const AValue: Integer);
var
  i : Integer;
begin
  If Count = AValue then exit;
  for i:= AValue to Count-1 do Strings[i]:='';
  inherited SetCount(AValue);
  for i := 0 to length(FRangeList) - 1 do
    FRangeList[i].Data.Count := AValue;
end;

function TSynEditStringMemory.GetAttributeSize: Integer;
begin
  Result := FAttributeSize - SizeOf(String) - SizeOf(TObject)
end;

procedure TSynEditStringMemory.SetAttributeSize(const AValue: Integer);
var
  c: LongInt;
begin
  if FAttributeSize = AValue + SizeOf(String) + SizeOf(TObject) then exit;;
  c := Capacity;
  Capacity := 0;
  FAttributeSize := AValue + SizeOf(String) + SizeOf(TObject);
  Capacity := c;
end;

function TSynEditStringMemory.GetString(Index: Integer): String;
begin
  Result := (PString(Mem + Index * FAttributeSize))^;
end;

procedure TSynEditStringMemory.SetString(Index: Integer; const AValue: String);
begin
  (PString(Mem + Index * FAttributeSize))^ := AValue;
end;

function TSynEditStringMemory.ItemSize: Integer;
begin
  Result := FAttributeSize;
end;

procedure TSynEditStringMemory.SetCapacity(const AValue: Integer);
var
  i: Integer;
begin
  inherited SetCapacity(AValue);
  for i := 0 to length(FRangeList) - 1 do
    FRangeList[i].Data.Capacity := AValue;
end;

function TSynEditStringMemory.GetObject(Index: Integer): TObject;
begin
  Result := (PObject(Mem + Index * FAttributeSize + SizeOf(String)))^;
end;

function TSynEditStringMemory.GetRange(Index: TClass): TSynEditStorageMem;
var
  i: Integer;
begin
  for i := 0 to length(FRangeList) - 1 do
    if FRangeList[i].Index = Index then
      exit(FRangeList[i].Data);
  Result := nil;
end;

procedure TSynEditStringMemory.SetObject(Index: Integer; const AValue: TObject);
begin
  (PObject(Mem + Index * FAttributeSize + SizeOf(String)))^ := AValue;
end;

procedure TSynEditStringMemory.SetRange(Index: TClass; const AValue: TSynEditStorageMem);
var
  i, j: Integer;
begin
  i := length(FRangeList) - 1;
  while (i >= 0) and (FRangeList[i].Index <> Index) do
    dec(i);

  if i < 0 then begin
    i := length(FRangeList);
    SetLength(FRangeList, i + 1);
    FRangeList[i].Index := Index;
  end
  else
    if AValue <> nil then
      DebugLn(['TSynEditStringMemory.SetRange - Overwriting old range at index=', i, ' index=', dbgs(Index)]);

  FRangeList[i].Data := AValue;

  if AValue <> nil then begin
    AValue.Capacity := Capacity;
    AValue.Count := Count;
  end else begin
    for j := i to length(FRangeList) - 2 do
      FRangeList[j] := FRangeList[j+1];
    SetLength(FRangeList, length(FRangeList) - 1);
  end;
end;

function TSynEditStringMemory.GetAttribute(Index: Integer; Pos: Integer; Size: Word): Pointer;
begin
  case Size of
    1 : Result := Pointer(PtrUInt((PByte(Mem + Index * FAttributeSize + AttributeOfset + Pos))^));
    2 : Result := Pointer(PtrUInt((PWord(Mem + Index * FAttributeSize + AttributeOfset + Pos))^));
    4 : Result := Pointer(PtrUInt((PLongWord(Mem + Index * FAttributeSize + AttributeOfset + Pos))^));
    8 : Result := Pointer(PtrUInt((PQWord(Mem + Index * FAttributeSize + AttributeOfset + Pos))^));
  end;
end;

procedure TSynEditStringMemory.SetAttribute(Index: Integer; Pos: Integer; Size: Word; const AValue: Pointer);
begin
  case Size of
    1 : (PByte(Mem + Index * FAttributeSize + AttributeOfset + Pos))^ := Byte(PtrUInt(AValue));
    2 : (PWord(Mem + Index * FAttributeSize + AttributeOfset + Pos))^ := Word(PtrUInt(AValue));
    4 : (PLongWord(Mem + Index * FAttributeSize + AttributeOfset + Pos))^ := LongWord(PtrUInt(AValue));
    8 : (PQWord(Mem + Index * FAttributeSize + AttributeOfset + Pos))^ := QWord(PtrUInt(AValue));
  end;
end;

{ TLineRangeNotificationList }

procedure TLineRangeNotificationList.CallRangeNotifyEvents(Sender: TSynEditStrings; aIndex, aCount: Integer);
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

