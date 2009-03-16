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
  FileUtil, LCLProc, FPCAdds, LCLIntf, LCLType,
  SynEditTypes, SynEditMiscProcs;

const
  NullRange = TSynEditRange(-1);

type
  TSynEditRangeClass = class end; // For Register
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

  TLineRangeNotificationList = Class(TMethodList)
  public
    Procedure CallRangeNotifyEvents(Sender: TSynEditStrings; aIndex, aCount: Integer);
  end;


  { TSynEditStringMemory }

  TSynEditStringMemory = class
  private
    FMem: ^Byte;
    FCount, FCapacity: Integer;
    FAttributeSize: Integer;
    function GetAttribute(Index: Integer; Pos: Integer; Size: Word): Pointer;
    function GetAttributeSize: Integer;
    function GetCapacity: Integer;
    function GetObject(Index: Integer): TObject;
    function GetString(Index: Integer): String;
    procedure SetAttribute(Index: Integer; Pos: Integer; Size: Word; const AValue: Pointer);
    procedure SetAttributeSize(const AValue: Integer);
    procedure SetCapacity(const AValue: Integer);
    procedure SetCount(const AValue: Integer);
    procedure SetObject(Index: Integer; const AValue: TObject);
    procedure SetString(Index: Integer; const AValue: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Move(AFrom, ATo, ALen: Integer);

    property Strings[Index: Integer]: String read GetString write SetString; default;
    property Objects[Index: Integer]: TObject read GetObject write SetObject;
    property Attribute[Index: Integer; Pos: Integer; Size: Word]: Pointer
      read  GetAttribute write SetAttribute;
    property Capacity: Integer read GetCapacity write SetCapacity;
    // Count must be maintained by owner
    property Count: Integer read FCount write SetCount;
    property AttributeSize: Integer read  GetAttributeSize write SetAttributeSize;
  end;

  { TSynEditStringList }

  TSynEditStringList = class(TSynEditStrings)
  private
    FList: TSynEditStringMemory;
    FAttributeList: Array of TSynEditStringAttribute;
    FLineRangeNotificationList: TLineRangeNotificationList; // LineCount
    FLineChangeNotificationList: TLineRangeNotificationList; // ContentChange (not called on add...)
    fDosFileFormat: boolean;
    fIndexOfLongestLine: integer;
    fOnChange: TNotifyEvent;
    fOnChanging: TNotifyEvent;
    fRedoList: TSynEditUndoList;
    fUndoList: TSynEditUndoList;
    FIsUndoing: Boolean;

    {$IFDEF SYN_LAZARUS}
    function GetFlags(Index: Integer): TSynEditStringFlags;
    {$ENDIF}
    procedure Grow;
    procedure InsertItem(Index: integer; const S: string);
    function ClassIndexForAttribute(AttrIndex: TClass): Integer;
    Procedure SetAttributeSize(NewSize: Integer);
    procedure SetFlags(Index: Integer; const AValue: TSynEditStringFlags);
  protected
    fOnCleared: TNotifyEvent;
    function GetExpandedString(Index: integer): string; override;
    function GetLengthOfLongestLine: integer; override;

    function GetRedoList: TSynEditUndoList; override;
    function GetUndoList: TSynEditUndoList; override;
    procedure SetIsUndoing(const AValue: Boolean); override;
    procedure SendNotification(AReason: TSynEditNotifyReason;
                ASender: TSynEditStrings; aIndex, aCount: Integer); override;

    function GetRange(Index: integer): TSynEditRange; {$IFDEF SYN_LAZARUS}override;{$ENDIF}
    procedure PutRange(Index: integer; ARange: TSynEditRange); {$IFDEF SYN_LAZARUS}override;{$ENDIF}
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
    procedure DeleteLines(Index, NumLines: integer);                            // DJLP 2000-11-01
      {$IFDEF SYN_LAZARUS}override;{$ENDIF}
    procedure Insert(Index: integer; const S: string); override;
    procedure InsertLines(Index, NumLines: integer);                            // DJLP 2000-11-01
      {$IFDEF SYN_LAZARUS}override;{$ENDIF}
    procedure InsertStrings(Index: integer; NewStrings: TStrings);              // DJLP 2000-11-01
      {$IFDEF SYN_LAZARUS}override;{$ENDIF}
    {$IFDEF SYN_LAZARUS}
    procedure ClearRanges(ARange: TSynEditRange); override;
    procedure MarkModified(AFirst, ALast: Integer);
    procedure MarkSaved;
    procedure SetDebugMarks(AFirst, ALast: Integer);
    procedure ClearDebugMarks;
    {$ENDIF}
    procedure AddChangeHandler(AReason: TSynEditNotifyReason;
                AHandler: TStringListLineCountEvent); override;
    procedure RemoveChangeHandler(AReason: TSynEditNotifyReason;
                AHandler: TStringListLineCountEvent); override;
    function GetPhysicalCharWidths(const Line: String; Index: Integer): TPhysicalCharWidths; override;
  public
    property DosFileFormat: boolean read fDosFileFormat write fDosFileFormat;    
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
    property Ranges[Index: integer]: TSynEditRange read GetRange write PutRange;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
    property OnCleared: TNotifyEvent read fOnCleared write fOnCleared;
    {$IFDEF SYN_LAZARUS}
    property Flags[Index: Integer]: TSynEditStringFlags read GetFlags
      write SetFlags;
    {$ENDIF}
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
begin
  fList := TSynEditStringMemory.Create;
  FLineRangeNotificationList := TLineRangeNotificationList.Create;
  FLineChangeNotificationList := TLineRangeNotificationList.Create;
  inherited Create;
  SetAttributeSize(0);
  RegisterAttribute(TSynEditRangeClass, SizeOf(Pointer));
  RegisterAttribute(TSynEditFlagsClass, SizeOf(TSynEditStringFlag));
  fDosFileFormat := TRUE;
{begin}                                                                         //mh 2000-10-19
  fIndexOfLongestLine := -1;
{end}                                                                           //mh 2000-10-19
end;

destructor TSynEditStringList.Destroy;
begin
  fOnChange := nil;
  fOnChanging := nil;
  fAttributeList := nil;
  inherited Destroy;
  SetCount(0);
  SetCapacity(0);
  FreeAndNil(FLineRangeNotificationList);
  FreeAndNil(FLineChangeNotificationList);
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
        SetAttribute(TSynEditRangeClass, Count-1, NullRange);
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
    if Assigned(fOnCleared) then
      fOnCleared(Self);
    FLineRangeNotificationList.CallRangeNotifyEvents(self, 0, -c);
    EndUpdate;
  end;
  fIndexOfLongestLine := -1;                                                    //mh 2000-10-19
end;

procedure TSynEditStringList.Delete(Index: integer);
begin
  if (Index < 0) or (Index > Count) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  if Index < Count-1 then
    fList.Move(Index + 1, Index, Count-Index-1);
  SetCount(Count - 1);
  fIndexOfLongestLine := -1;                                                    //mh 2000-10-19
  FLineRangeNotificationList.CallRangeNotifyEvents(self, Index, -1);
  EndUpdate;
end;

{begin}                                                                         // DJLP 2000-11-01
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
{end}                                                                           // DJLP 2000-11-01

{$IFDEF SYN_LAZARUS}
function TSynEditStringList.GetFlags(Index: Integer): TSynEditStringFlags;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TSynEditStringFlags(Integer(PtrUInt(GetAttribute(TSynEditFlagsClass, Index))))
  else
    Result := [];
end;
{$ENDIF}

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
  if FIsUndoing then
    Result := fRedoList
  else
    Result := fUndoList;
end;

procedure TSynEditStringList.SetIsUndoing(const AValue: Boolean);
begin
  FIsUndoing := AValue;
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

function TSynEditStringList.GetRange(Index: integer): TSynEditRange;
begin
  if (Index >= 0) and (Index < Count) then
    Result := TSynEditRange(GetAttribute(TSynEditRangeClass, Index))
  else
    Result := nil;
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
{$IFDEF SYN_LAZARUS}
var
  OldCnt : integer;
{$ENDIF}
begin
  if (Index < 0) or (Index > Count) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  {$IFDEF SYN_LAZARUS}
  OldCnt:=Count;
  {$ENDIF}
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
  Ranges[Index] := NullRange;
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
    FLineChangeNotificationList.CallRangeNotifyEvents(self, Index, 1);
    EndUpdate;
  end;
end;

procedure TSynEditStringList.PutObject(Index: integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= Count) then
    ListIndexOutOfBounds(Index);
  {$IFDEF SYN_LAZARUS}
  if fList.Objects[Index] = AObject then exit;
  {$ENDIF}
  BeginUpdate;
  fList.Objects[Index]:= AObject;
  EndUpdate;
end;

procedure TSynEditStringList.PutRange(Index: integer; ARange: TSynEditRange);
begin
  {$IFDEF SYN_LAZARUS}
  // do not call BeginUpdate/EndUpdate. It would call the too generic OnChange
  // events
  SetAttribute(TSynEditRangeClass, Index, Pointer(PtrUInt(ARange)));
  {$ELSE}
  BeginUpdate;
  SetAttribute(TSynEditRangeClass, Index, Pointer(PtrUInt(ARange)));
  EndUpdate;
  {$ENDIF}
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

{$IFDEF SYN_LAZARUS}
procedure TSynEditStringList.ClearRanges(ARange: TSynEditRange);
var
  Index: Integer;
begin
  for Index:=0 to Count-1 do
    Ranges[Index] := ARange;
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

procedure TSynEditStringList.AddChangeHandler(AReason: TSynEditNotifyReason; AHandler: TStringListLineCountEvent);
begin
  case AReason of
    senrLineChange : FLineChangeNotificationList.Add(TMethod(AHandler));
    senrLineCount : FLineRangeNotificationList.Add(TMethod(AHandler));
  end;
end;

procedure TSynEditStringList.RemoveChangeHandler(AReason: TSynEditNotifyReason; AHandler: TStringListLineCountEvent);
begin
  case AReason of
    senrLineChange : FLineChangeNotificationList.Remove(TMethod(AHandler));
    senrLineCount : FLineRangeNotificationList.Remove(TMethod(AHandler));
  end;
end;

{$ENDIF}

procedure TSynEditStringList.SetCapacity(NewCapacity: integer);
begin
  fList.SetCapacity(NewCapacity);
end;

procedure TSynEditStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then begin
    if Assigned(fOnChanging) then
      fOnChanging(Self);
  end else begin
    if Assigned(fOnChange) then
      fOnChange(Self);
  end;
end;

procedure TSynEditStringList.EditInsert(LogX, LogY: Integer; AText: String);
var
  s: string;
begin
  s := Strings[LogY - 1];
  if LogX - 1 > Length(s) then
    AText := StringOfChar(' ', LogX - 1 - Length(s)) + AText;
  Strings[LogY - 1] := copy(s,1, LogX - 1) + AText + copy(s, LogX, length(s));
  UndoList.AddChange(TSynEditUndoTxtInsert.Create(LogX, LogY, Length(AText)));
  MarkModified(LogY, LogY);
end;

function TSynEditStringList.EditDelete(LogX, LogY, ByteLen: Integer): String;
var
  s: string;
begin
  s := Strings[LogY - 1];
  Result := copy(s, LogX, ByteLen);
  Strings[LogY - 1] := copy(s,1, LogX - 1) + copy(s, LogX +  ByteLen, length(s));
  UndoList.AddChange(TSynEditUndoTxtDelete.Create(LogX, LogY, Result));
  MarkModified(LogY, LogY);
end;

procedure TSynEditStringList.EditLineBreak(LogX, LogY: Integer);
var
  s: string;
begin
  s := Strings[LogY - 1];
  Strings[LogY - 1] := copy(s, 1, LogX - 1);
  Insert(LogY, copy(s, LogX, length(s)));
  UndoList.AddChange(TSynEditUndoTxtLineBreak.Create(LogY));
  MarkModified(LogY, LogY + 1);
end;

procedure TSynEditStringList.EditLineJoin(LogY: Integer; FillText: String = '');
var
  t: string;
begin
  t := Strings[LogY - 1];
  if FillText <> ''  then
    EditInsert(1 + Length(t), LogY, FillText);
  UndoList.AddChange(TSynEditUndoTxtLineJoin.Create(1 + Length(Strings[LogY-1]),
                                                    LogY));
  Strings[LogY - 1] := t + FillText + Strings[LogY] ;
  Delete(LogY);
  MarkModified(LogY, LogY);
end;

procedure TSynEditStringList.EditLinesInsert(LogY, ACount: Integer;
  AText: String = '');
begin
  InsertLines(LogY - 1, ACount);
  UndoList.AddChange(TSynEditUndoTxtLinesIns.Create(LogY, ACount));
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
  UndoList.AddChange(TSynEditUndoTxtLinesDel.Create(LogY, ACount));
end;

procedure TSynEditStringList.EditUndo(Item: TSynEditUndoItem);
begin
  IsUndoing := True;
  try
    EditRedo(Item);
  finally
    IsUndoing := False;
  end;
end;

procedure TSynEditStringList.UndoEditLinesDelete(LogY, ACount: Integer);
begin
  UndoList.AddChange(TSynEditUndoTxtLinesDel.Create(LogY, ACount));
  DeleteLines(LogY - 1, ACount);
end;

procedure TSynEditStringList.EditRedo(Item: TSynEditUndoItem);
begin
  Item.PerformUndo(self);
end;

procedure TSynEditStringList.SendNotification(AReason: TSynEditNotifyReason; ASender: TSynEditStrings; aIndex, aCount: Integer);
begin
  case AReason of
    senrLineChange:
      FLineChangeNotificationList.CallRangeNotifyEvents(ASender, aIndex, aCount);
    senrLineCount:
      FLineRangeNotificationList.CallRangeNotifyEvents(ASender, aIndex, aCount);
  end;
end;

{ TSynEditStringMemory }
type
  PObject = ^TObject;

const
  AttributeOfset = SizeOf(String) + SizeOf(TObject);

constructor TSynEditStringMemory.Create;
begin
  inherited Create;
  FCapacity := 0;
  FCount := 0;
  AttributeSize := 0;
end;

destructor TSynEditStringMemory.Destroy;
begin
  SetCount(0);
  SetCapacity(0);
  inherited Destroy;
end;

procedure TSynEditStringMemory.Move(AFrom, ATo, ALen: Integer);
var
  i, len: Integer;
begin
  //debugln(['TSynEditStringMemory.Move(',AFrom, ',', ATo, ', ',ALen,')']);
  if ATo < AFrom then begin
    Len := Min(ALen, AFrom-ATo);
    for i:=ATo to ATo + Len -1 do Strings[i]:='';
    System.Move((FMem+AFrom*FAttributeSize)^, (FMem+ATo*FAttributeSize)^, Alen*FAttributeSize);
    FillChar((FMem+(AFrom+ALen-Len)*FAttributeSize)^, Len*FAttributeSize, 0);
  end else begin
    Len := Min(ALen, ATo-AFrom);
    for i:=ATo+Alen-Len to ATo+ALen -1 do Strings[i]:='';
    System.Move((FMem+AFrom*FAttributeSize)^, (FMem+ATo*FAttributeSize)^, Alen*FAttributeSize);
    FillChar((FMem+AFrom*FAttributeSize)^, Len*FAttributeSize, 0);
  end;
end;

function TSynEditStringMemory.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TSynEditStringMemory.SetCapacity(const AValue: Integer);
begin
  if FCapacity = AValue then exit;;
  FMem := ReallocMem(FMem, AValue * FAttributeSize);
  if AValue > FCapacity then
    FillChar((FMem+FCapacity*FAttributeSize)^, (AValue-FCapacity)*FAttributeSize, 0);
  FCapacity := AValue;
end;

procedure TSynEditStringMemory.SetCount(const AValue: Integer);
var
  i : Integer;
begin
  If FCount = AValue then exit;
  for i:= AValue to FCount-1 do Strings[i]:='';
  FCount := AValue;
end;


function TSynEditStringMemory.GetAttributeSize: Integer;
begin
  Result := FAttributeSize - SizeOf(String) - SizeOf(TObject)
end;

procedure TSynEditStringMemory.SetAttributeSize(const AValue: Integer);
begin
  if FAttributeSize = AValue + SizeOf(String) + SizeOf(TObject) then exit;;
  FAttributeSize := AValue + SizeOf(String) + SizeOf(TObject);
  SetCapacity(FCapacity);
  // Todo: Move existing records
end;

function TSynEditStringMemory.GetString(Index: Integer): String;
begin
  Result := (PString(FMem + Index * FAttributeSize))^;
end;

procedure TSynEditStringMemory.SetString(Index: Integer; const AValue: String);
begin
  (PString(FMem + Index * FAttributeSize))^ := AValue;
end;

function TSynEditStringMemory.GetObject(Index: Integer): TObject;
begin
  Result := (PObject(FMem + Index * FAttributeSize + SizeOf(String)))^;
end;

procedure TSynEditStringMemory.SetObject(Index: Integer; const AValue: TObject);
begin
  (PObject(FMem + Index * FAttributeSize + SizeOf(String)))^ := AValue;
end;

function TSynEditStringMemory.GetAttribute(Index: Integer; Pos: Integer; Size: Word): Pointer;
begin
  case Size of
    1 : Result := Pointer(PtrUInt((PByte(FMem + Index * FAttributeSize + AttributeOfset + Pos))^));
    2 : Result := Pointer(PtrUInt((PWord(FMem + Index * FAttributeSize + AttributeOfset + Pos))^));
    4 : Result := Pointer(PtrUInt((PLongWord(FMem + Index * FAttributeSize + AttributeOfset + Pos))^));
    8 : Result := Pointer(PtrUInt((PQWord(FMem + Index * FAttributeSize + AttributeOfset + Pos))^));
  end;
end;

procedure TSynEditStringMemory.SetAttribute(Index: Integer; Pos: Integer; Size: Word; const AValue: Pointer);
begin
  case Size of
    1 : (PByte(FMem + Index * FAttributeSize + AttributeOfset + Pos))^ := Byte(PtrUInt(AValue));
    2 : (PWord(FMem + Index * FAttributeSize + AttributeOfset + Pos))^ := Word(PtrUInt(AValue));
    4 : (PLongWord(FMem + Index * FAttributeSize + AttributeOfset + Pos))^ := LongWord(PtrUInt(AValue));
    8 : (PQWord(FMem + Index * FAttributeSize + AttributeOfset + Pos))^ := QWord(PtrUInt(AValue));
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

end.

