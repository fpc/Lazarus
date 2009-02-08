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
  {$IFDEF SYN_LAZARUS}
  FileUtil, LCLProc, FPCAdds, LCLIntf, LCLType,
  {$ELSE}
  Windows,
  {$ENDIF}
  SynEditTypes, SynEditMiscProcs;                   //mh 2000-10-19

type
{begin}                                                                         //mh 2000-10-10
  {$IFNDEF SYN_LAZARUS}
  TSynEditRange = pointer;
  {$ENDIF}
  TSynEditRangeClass = class end; // For Register
  TSynEditFlagsClass = class end; // For Register

  TSynEditStringFlag = (
    sfModified,              // a line is modified and not saved after
    sfSaved                  // a line is modified and saved after
  );
  TSynEditStringFlags = set of TSynEditStringFlag;

  TSynChangeReason = (crInsert, crPaste, crDragDropInsert,
    // Note: crSelDelete and crDragDropDelete have been deleted, because
    //   several undo entries can be chained together now via the ChangeNumber
    //   see also TCustomSynEdit.[Begin|End]UndoBlock methods
    crDeleteAfterCursor, crDelete, {crSelDelete, crDragDropDelete, }            //mh 2000-11-20
    crLineBreak, crIndent, crUnindent,
    crSilentDelete, crSilentDeleteAfterCursor,                                  //mh 2000-10-30
    crNothing {$IFDEF SYN_LAZARUS}, crTrimSpace, crTrimRealSpace {$ENDIF});

const
  SynChangeReasonNames : Array [TSynChangeReason] of string =
   ('crInsert', 'crPaste', 'crDragDropInsert',
    'crDeleteAfterCursor', 'crDelete', {'crSelDelete', 'crDragDropDelete', }
    'crLineBreak', 'crIndent', 'crUnindent',
    'crSilentDelete', 'crSilentDeleteAfterCursor',
    'crNothing' {$IFDEF SYN_LAZARUS}, 'crTrimSpace', 'crTrimRealSpace' {$ENDIF});

  NullRange = TSynEditRange(-1);

type
  TStringListIndexEvent = procedure(Index: Integer) of object;

  TSynEditStringAttribute = record
    Index: TClass;
    Size: Word;
    Pos: Integer;
  end;

  { TLineRangeNotificationList }

  TLineRangeNotificationList = Class(TMethodList)
  public
    procedure CallRangeNotifyEvents(Sender: TSynEditStrings; aIndex, aCount: Integer);
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
    procedure Exchange(Index1, Index2: integer); override;
    procedure Insert(Index: integer; const S: string); override;
    procedure InsertLines(Index, NumLines: integer);                            // DJLP 2000-11-01
      {$IFDEF SYN_LAZARUS}override;{$ENDIF}
    procedure InsertStrings(Index: integer; NewStrings: TStrings);              // DJLP 2000-11-01
      {$IFDEF SYN_LAZARUS}override;{$ENDIF}
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    {$IFDEF SYN_LAZARUS}
    procedure ClearRanges(ARange: TSynEditRange); override;
    procedure MarkModified(AFirst, ALast: Integer; AUndo: Boolean; AReason: TSynChangeReason);
    procedure MarkSaved;
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
  end;

  ESynEditStringList = class(Exception);
{end}                                                                           //mh 2000-10-10

  { TSynEditUndoItem }

  TSynEditUndoItem = class(TObject)
  public
    fChangeReason: TSynChangeReason;
    fChangeSelMode: TSynSelectionMode;
    fChangeStartPos: TPoint; // logical position (byte)
    fChangeEndPos: TPoint; // logical position (byte)
    fChangeStr: string;
    fChangeNumber: integer;                                                     //sbs 2000-11-19
    {$IFDEF SYN_LAZARUS}
    function ChangeStartPos: TPoint; // logical position (byte)
    function ChangeEndPos: TPoint; // logical position (byte)
    {$ENDIF}
  end;

  { TSynEditUndoList }

  TSynEditUndoList = class(TObject)
  private
    fBlockChangeNumber: integer;                                                //sbs 2000-11-19
    fBlockCount: integer;                                                       //sbs 2000-11-19
    fFullUndoImposible: boolean;                                                //mh 2000-10-03
    fItems: TList;
    fLockCount: integer;
    fMaxUndoActions: integer;
    fNextChangeNumber: integer;                                                 //sbs 2000-11-19
    fOnAdded: TNotifyEvent;
    {$IFDEF SYN_LAZARUS}
    fUnModifiedItem: integer;
    {$ENDIF}
    procedure EnsureMaxEntries;
    function GetCanUndo: boolean;
    function GetItemCount: integer;
    procedure SetMaxUndoActions(Value: integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddChange(AReason: TSynChangeReason; AStart, AEnd: TPoint;
      ChangeText: string; SelMode: TSynSelectionMode);
    procedure AppendToLastChange(AReason: TSynChangeReason; AStart, AEnd: TPoint;
      ChangeText: string; SelMode: TSynSelectionMode);
    procedure BeginBlock;                                                       //sbs 2000-11-19
    procedure Clear;
    procedure EndBlock;                                                         //sbs 2000-11-19
    procedure Lock;
    function PeekItem: TSynEditUndoItem;
    function PopItem: TSynEditUndoItem;
    procedure PushItem(Item: TSynEditUndoItem);
    procedure Unlock;
    function IsLocked: Boolean;
    {$IFDEF SYN_LAZARUS}
    procedure MarkTopAsUnmodified;
    function IsTopMarkedAsUnmodified: boolean;
    function UnModifiedMarkerExists: boolean;
    {$ENDIF}
  public
    property BlockChangeNumber: integer read fBlockChangeNumber                 //sbs 2000-11-19
      write fBlockChangeNumber;
    property CanUndo: boolean read GetCanUndo;
    property FullUndoImpossible: boolean read fFullUndoImposible;               //mh 2000-10-03
    property ItemCount: integer read GetItemCount;
    property MaxUndoActions: integer read fMaxUndoActions
      write SetMaxUndoActions;
    property OnAddedUndo: TNotifyEvent read fOnAdded write fOnAdded;
  end;

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

{ TSynEditFiler }

type
  TSynEditFiler = class(TObject)
  protected
    fBuffer: PChar;
    fBufPtr: Cardinal;
    fBufSize: Cardinal;
    fDosFile: boolean;
    fFiler: TFileStream;
    procedure Flush; virtual;
    procedure SetBufferSize(NewSize: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
  public
    property DosFile: boolean read fDosFile write fDosFile;
  end;

constructor TSynEditFiler.Create;
const
  kByte = 1024;
begin
  inherited Create;
  fDosFile := FALSE;
  SetBufferSize(16 * kByte);
  fBuffer[0] := #0;
end;

destructor TSynEditFiler.Destroy;
begin
  Flush;
  fFiler.Free;
  SetBufferSize(0);
  inherited Destroy;
end;

procedure TSynEditFiler.Flush;
begin
end;

procedure TSynEditFiler.SetBufferSize(NewSize: Cardinal);
begin
  if NewSize <> fBufSize then begin
    ReallocMem(fBuffer, NewSize);
    fBufSize := NewSize;
  end;
end;

{ TSynEditFileReader }

type
  TSynEditFileReader = class(TSynEditFiler)
  protected
    {$IFDEF SYN_LAZARUS}
    fFilePos: TStreamSeekType;
    fFileSize: TStreamSeekType;
    {$ELSE}
    fFilePos: Cardinal;
    fFileSize: Cardinal;
    {$ENDIF}
    procedure FillBuffer;
  public
    constructor Create(const FileName: string);
    function EOF: boolean;
    function ReadLine: string;
  end;

constructor TSynEditFileReader.Create(const FileName: string);
begin
  inherited Create;
  fFiler := TFileStream.Create(UTF8ToSys(FileName), fmOpenRead{ ToDo: or fmShareDenyWrite});
  fFileSize := fFiler.Size;
  fFiler.Seek(0, soFromBeginning);
end;

function TSynEditFileReader.EOF: boolean;
begin
  Result := (fBuffer[fBufPtr] = #0) and (fFilePos >= fFileSize);
end;

procedure TSynEditFileReader.FillBuffer;
var
  Count: Cardinal;
begin
  if fBufPtr >= fBufSize - 1 then
    fBufPtr := 0;
  Count := fFileSize - fFilePos;
  if Count >= fBufSize - fBufPtr then
    Count := fBufSize - fBufPtr - 1;
  fFiler.ReadBuffer(fBuffer[fBufPtr], Count);
  fBuffer[fBufPtr + Count] := #0;
  fFilePos := fFilePos + Count;
  fBufPtr := 0;
end;

function TSynEditFileReader.ReadLine: string;
var
  E, P, S: PChar;
begin
  Result := '';
  repeat
    S := PChar(@fBuffer[fBufPtr]);
    if S[0] = #0 then begin
      FillBuffer;
      S := PChar(@fBuffer[0]);
    end;
    E := PChar(@fBuffer[fBufSize]);
    P := S;
    while P + 2 < E do begin
      case P[0] of
        #10, #13:
          begin
            SetString(Result, S, P - S);
            {$IFDEF SYN_LAZARUS}
            // a single #13 is used in Mac OS files
            if (P[0] = #13) and (P[1] = #10) then begin
            {$ELSE}
            if P[0] = #13 then begin
            {$ENDIF}
              fDosFile := TRUE;
              Inc(P);
            end;
            Inc(P);
            fBufPtr := P - fBuffer;
            exit;
          end;
        #0:
          if fFilePos >= fFileSize then begin
            fBufPtr := P - fBuffer;
            SetString(Result, S, P - S);
            exit;
          end;
      end;
      Inc(P);
    end;
    // put the partial string to the start of the buffer, and refill the buffer
    Inc(P);
    if S > fBuffer then
      StrLCopy(fBuffer, S, P - S);
    fBufPtr := P - S;
    fBuffer[fBufPtr] := #0;
    // if line is longer than half the buffer then grow it first
    if 2 * Cardinal(P - S) > fBufSize then
      SetBufferSize(fBufSize + fBufSize);
  until FALSE;
end;

{ TSynEditFileWriter }

type
  TSynEditFileWriter = class(TSynEditFiler)
  protected
    procedure Flush; override;
  public
    constructor Create(const FileName: string);
    procedure WriteLine(const S: string);
  end;

constructor TSynEditFileWriter.Create(const FileName: string);
begin
  inherited Create;
  fFiler := TFileStream.Create(UTF8ToSys(FileName), fmCreate);
  fFiler.Seek(0, soFromBeginning);
end;

procedure TSynEditFileWriter.Flush;
begin
  if fBufPtr > 0 then begin
    fFiler.WriteBuffer(fBuffer[0], fBufPtr);
    fBufPtr := 0;
  end;
end;

procedure TSynEditFileWriter.WriteLine(const S: string);
var
  L, NL: Cardinal;
begin
  L := Length(S);
  NL := 1 + Ord(fDosFile);
  repeat
    if fBufPtr + L + NL <= fBufSize then begin
      if L > 0 then begin
        Move(S[1], fBuffer[fBufPtr], L);
        fBufPtr := fBufPtr + L;
      end;
      if fDosFile then begin
        fBuffer[fBufPtr] := #13;
        Inc(fBufPtr);
      end;
      fBuffer[fBufPtr] := #10;
      Inc(fBufPtr);
      exit;
    end;
    Flush;
    if L + NL > fBufSize then
      SetBufferSize(L + NL);
  until FALSE;
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

procedure TSynEditStringList.Exchange(Index1, Index2: integer);
begin
  if (Index1 < 0) or (Index1 >= Count) then
    ListIndexOutOfBounds(Index1);
  if (Index2 < 0) or (Index2 >= Count) then
    ListIndexOutOfBounds(Index2);
  BeginUpdate;
  If Count+1 >= Capacity then Grow;
  FList.Move(Index1, Count, 1);
  FList.Move(Index2, Index1, 1);
  FList.Move(Count, Index2, 1);
  FList.Move(Count+1, Count, 1); // clean it
  if fIndexOfLongestLine = Index1 then
    fIndexOfLongestLine := Index2
  else if fIndexOfLongestLine = Index2 then
    fIndexOfLongestLine := Index1;
  EndUpdate;
end;

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

{end}                                                                           //mh 2000-10-19

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

procedure TSynEditStringList.LoadFromFile(const FileName: string);
var
  Reader: TSynEditFileReader;
begin
  Reader := TSynEditFileReader.Create(FileName);
  try
    BeginUpdate;
    try
      Clear;
      while not Reader.EOF do
        Add(Reader.ReadLine);
      fDosFileFormat := Reader.DosFile;
    finally
      EndUpdate;
    end;
  finally
    Reader.Free;
  end;
end;

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
  if (Index < 0) or (Index >= Count) then
    ListIndexOutOfBounds(Index);
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
  if (Index < 0) or (Index >= Count) then
    ListIndexOutOfBounds(Index);
  i := ClassIndexForAttribute(Owner);
  if i < 0 then
    raise ESynEditStringList.CreateFmt('Unknown Attribute', []);
  Result := FList.Attribute[Index, FAttributeList[i].Pos, FAttributeList[i].Size];
end;

procedure TSynEditStringList.SetAttribute(const Owner: TClass; const Index: Integer; const AValue: Pointer);
var
  i: Integer;
begin
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

procedure TSynEditStringList.SaveToFile(const FileName: string);
var
  Writer: TSynEditFileWriter;
  i: integer;
begin
  Writer := TSynEditFileWriter.Create(FileName);
  try
    Writer.DosFile := fDosFileFormat;
    for i := 0 to Count - 1 do
      Writer.WriteLine(Get(i));
  finally
    Writer.Free;
  end;
end;

{$IFDEF SYN_LAZARUS}
procedure TSynEditStringList.ClearRanges(ARange: TSynEditRange);
var
  Index: Integer;
begin
  for Index:=0 to Count-1 do
    Ranges[Index] := ARange;
end;

procedure TSynEditStringList.MarkModified(AFirst, ALast: Integer;
  AUndo: Boolean; AReason: TSynChangeReason);
var
  Index: Integer;
begin
  // AUndo = True => this change is also pushed to the undo list, False => to the redo list
  // AReason - a reason of change

  for Index := AFirst to ALast do
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

{ TSynEditUndoList }

constructor TSynEditUndoList.Create;
begin
  inherited Create;
  fItems := TList.Create;
  fMaxUndoActions := 1024;
  fNextChangeNumber := 1;                                                       //sbs 2000-11-19
  {$IFDEF SYN_LAZARUS}
  fUnModifiedItem:=-1;
  {$ENDIF}
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
  if fLockCount = 0 then begin
    NewItem := TSynEditUndoItem.Create;
    try
      with NewItem do begin
        fChangeReason := AReason;
        fChangeSelMode := SelMode;
        fChangeStartPos := AStart;
        fChangeEndPos := AEnd;
        fChangeStr := ChangeText;
{begin}                                                                         //sbs 2000-11-19
        if fBlockChangeNumber <> 0 then
          fChangeNumber := fBlockChangeNumber
        else begin
          fChangeNumber := fNextChangeNumber;
          if fBlockCount = 0 then begin
            Inc(fNextChangeNumber);
            if fNextChangeNumber = 0 then
              Inc(fNextChangeNumber);
          end;
        end;
{end}                                                                           //sbs 2000-11-19
      end;
      (* DebugLn(['TSynEditUndoList.AddChange ChangeNumber=',NewItem.fChangeNumber,
               '  Reason=', SynChangeReasonNames[AReason],'  Astart=',dbgs(AStart),
               ' AEnd=',dbgs(AEnd),'  SelMode=',ord(SelMode)]); *)
      PushItem(NewItem);
    except
      NewItem.Free;
      raise;
    end;
  end;
end;

procedure TSynEditUndoList.AppendToLastChange(AReason: TSynChangeReason; AStart,
  AEnd: TPoint; ChangeText: string; SelMode: TSynSelectionMode);
var
  NewItem: TSynEditUndoItem;
begin
  if (fLockCount = 0) and (PeekItem <> nil) then begin
    if (fItems.Count = fUnModifiedItem) then
      inc(fUnModifiedItem);
    NewItem := TSynEditUndoItem.Create;
    try
      with NewItem do begin
        fChangeReason := AReason;
        fChangeSelMode := SelMode;
        fChangeStartPos := AStart;
        fChangeEndPos := AEnd;
        fChangeStr := ChangeText;
        fChangeNumber := PeekItem.fChangeNumber;
      end;
      //PushItem(NewItem);
      fItems.Add(NewItem);
      EnsureMaxEntries;
    except
      NewItem.Free;
      raise;
    end;
  end;
end;

{begin}                                                                         //sbs 2000-11-19
procedure TSynEditUndoList.BeginBlock;
begin
  Inc(fBlockCount);
  fBlockChangeNumber := fNextChangeNumber;
end;
{end}                                                                           //sbs 2000-11-19

procedure TSynEditUndoList.Clear;
var
  i: integer;
begin
  for i := 0 to fItems.Count - 1 do
    TSynEditUndoItem(fItems[i]).Free;
  fItems.Clear;
  fFullUndoImposible := FALSE;                                                  //mh 2000-10-03
  {$IFDEF SYN_LAZARUS}
  fUnModifiedItem:=-1;
  {$ENDIF}
end;

{begin}                                                                         //sbs 2000-11-19
procedure TSynEditUndoList.EndBlock;
begin
  if fBlockCount > 0 then begin
    Dec(fBlockCount);
    if fBlockCount = 0 then begin
      fBlockChangeNumber := 0;
      Inc(fNextChangeNumber);
      if fNextChangeNumber = 0 then
        Inc(fNextChangeNumber);
    end;
  end;
end;
{end}                                                                           //sbs 2000-11-19

procedure TSynEditUndoList.EnsureMaxEntries;
var
  Item: TSynEditUndoItem;
begin
  if fItems.Count > fMaxUndoActions then begin                                  //mh 2000-10-03
    fFullUndoImposible := TRUE;                                                 //mh 2000-10-03
    while fItems.Count > fMaxUndoActions do begin
      Item := TSynEditUndoItem(fItems[0]);
      Item.Free;
      fItems.Delete(0);
      {$IFDEF SYN_LAZARUS}
      if fUnModifiedItem>=0 then dec(fUnModifiedItem);
      {$ENDIF}
    end;
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

procedure TSynEditUndoList.Lock;
begin
  Inc(fLockCount);
end;

function TSynEditUndoList.PeekItem: TSynEditUndoItem;
var
  iLast: integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then
    Result := TSynEditUndoItem(fItems[iLast]);
end;

function TSynEditUndoList.PopItem: TSynEditUndoItem;
var
  iLast: integer;
begin
  Result := nil;
  iLast := fItems.Count - 1;
  if iLast >= 0 then begin
    Result := TSynEditUndoItem(fItems[iLast]);
    fItems.Delete(iLast);
    {$IFDEF SYN_LAZARUS}
    if fUnModifiedItem>fItems.Count then fUnModifiedItem:=-1;
    {$ENDIF}
    (*DebugLn(['TSynEditUndoList.PopItem=',Result.fChangeNumber,
               '  Reason=', SynChangeReasonNames[Result.fChangeReason],'  Astart=',dbgs(Result.fChangeStartPos),
               ' AEnd=',dbgs(result.fChangeEndPos),'  SelMode=',ord(result.fChangeSelMode )]);*)
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

procedure TSynEditUndoList.Unlock;
begin
  if fLockCount > 0 then
    Dec(fLockCount);
end;

function TSynEditUndoList.IsLocked: Boolean;
begin
  Result := fLockCount > 0;
end;

{$IFDEF SYN_LAZARUS}
procedure TSynEditUndoList.MarkTopAsUnmodified;
begin
  fUnModifiedItem:=fItems.Count;
end;

function TSynEditUndoList.IsTopMarkedAsUnmodified: boolean;
begin
  Result:=(fItems.Count=fUnModifiedItem);
end;

function TSynEditUndoList.UnModifiedMarkerExists: boolean;
begin
  Result:=fUnModifiedItem>=0;
end;

{$ENDIF}

{ TSynEditUndoItem }

{$IFDEF SYN_LAZARUS}
function TSynEditUndoItem.ChangeStartPos: TPoint;
begin
  if (fChangeStartPos.Y < fChangeEndPos.Y)
    or ((fChangeStartPos.Y = fChangeEndPos.Y) and (fChangeStartPos.X < fChangeEndPos.X))
  then result := fChangeStartPos
  else result := fChangeEndPos;
end;

function TSynEditUndoItem.ChangeEndPos: TPoint;
begin
  if (fChangeStartPos.Y < fChangeEndPos.Y)
    or ((fChangeStartPos.Y = fChangeEndPos.Y) and (fChangeStartPos.X < fChangeEndPos.X))
  then result := fChangeEndPos
  else result := fChangeStartPos;
end;
{$ENDIF}

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

