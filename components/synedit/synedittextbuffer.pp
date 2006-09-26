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
  Classes, SysUtils,
  {$IFDEF SYN_LAZARUS}
  LCLProc, FPCAdds, LCLIntf, LCLType,
  {$ELSE}
  Windows,
  {$ENDIF}
  SynEditTypes, SynEditMiscProcs;                   //mh 2000-10-19

type
{begin}                                                                         //mh 2000-10-10
  TSynEditRange = pointer;

{begin}                                                                         //mh 2000-10-19
  TSynEditStringFlag = (sfHasTabs, sfHasNoTabs, sfExpandedLengthUnknown);
  TSynEditStringFlags = set of TSynEditStringFlag;
  {$IFDEF SYN_LAZARUS}
  TSynEditCodeFoldType = (
    cfNone,       // line is not in a block
    cfCollapsed,  // line is start of collapsed block
    cfExpanded,   // line is start of expanded block
    cfContinue,   // line is middle part of block(s)
    cfEnd         // line is end of block(s)
    );
  {$ENDIF}
{end}                                                                           //mh 2000-10-19

  PSynEditStringRec = ^TSynEditStringRec;
  TSynEditStringRec = record
    fString: string;
    fObject: TObject;
    fRange: TSynEditRange; // range at start of line
{begin}                                                                         //mh 2000-10-19
    fExpandedLength: integer;
    fFlags: TSynEditStringFlags;
    {$IFDEF SYN_LAZARUS}
    fFolded: boolean;
    fFoldMinLevel: LongInt; // minimum block depth in this line
    fFoldEndLevel: LongInt; // block depth at end of this line
    fFoldType: TSynEditCodeFoldType;
    {$ENDIF}
{end}                                                                           //mh 2000-10-19
  end;

const
  SynEditCodeFoldTypeNames: array[TSynEditCodeFoldType] of string = (
    'cfNone',
    'cfCollapsed',
    'cfExpanded',
    'cfContinue',
    'cfEnd'
    );

  SynEditStringRecSize = SizeOf(TSynEditStringRec);
  MaxSynEditStrings = MaxInt div SynEditStringRecSize;

  NullRange = TSynEditRange(-1);

type
  PSynEditStringRecList = ^TSynEditStringRecList;
  TSynEditStringRecList = array[0..MaxSynEditStrings - 1] of TSynEditStringRec;

  TStringListIndexEvent = procedure(Index: Integer) of object;        

  { TSynEditStringList }

  TSynEditStringList = class(TStrings)
  private
    fList: PSynEditStringRecList;
    fCount: integer;
    fCapacity: integer;
    fDosFileFormat: boolean;
{begin}                                                                         //mh 2000-10-19
    fConvertTabsProc: TConvertTabsProcEx;
    {$IFDEF SYN_LAZARUS}
    fSimulateConvertTabsProc: TSimulateConvertTabsProcEx;
    {$ENDIF}
    fIndexOfLongestLine: integer;
    fTabWidth: integer;
{end}                                                                           //mh 2000-10-19
    fOnChange: TNotifyEvent;
    fOnChanging: TNotifyEvent;
{begin}                                                                         //mh 2000-10-19
    function ExpandedString(Index: integer): string;
    {$IFDEF SYN_LAZARUS}
    function ExpandedStringLength(Index: integer): integer;
    function GetFoldEndLevel(Index: integer): integer;
    function GetFoldMinLevel(Index: integer): integer;
    function GetFoldType(Index: integer): TSynEditCodeFoldType;
    function GetFolded(Index: integer): boolean;
    procedure SetFoldEndLevel(Index: integer; const AValue: integer);
    procedure SetFoldMinLevel(Index: integer; const AValue: integer);
    procedure SetFoldType(Index: integer; const AValue: TSynEditCodeFoldType);
    procedure SetFolded(Index: integer; const AValue: boolean);
    {$ENDIF}
    function GetExpandedString(Index: integer): string;
    function GetLengthOfLongestLine: integer;
{end}                                                                           //mh 2000-10-19
    function GetRange(Index: integer): TSynEditRange;
    procedure Grow;
    procedure InsertItem(Index: integer; const S: string);
    procedure PutRange(Index: integer; ARange: TSynEditRange);
  protected
    fLongestLineIndex: integer;                                                 //mh 2000-10-19
    fOnAdded: TStringListIndexEvent;
    fOnCleared: TNotifyEvent;
    fOnDeleted: TStringListIndexEvent;
    fOnInserted: TStringListIndexEvent;
    fOnPutted: TStringListIndexEvent;
    function Get(Index: integer): string; override;
    function GetCapacity: integer;
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}                             //mh 2000-10-18
    function GetCount: integer; override;
    function GetObject(Index: integer): TObject; override;
    procedure Put(Index: integer; const S: string); override;
    procedure PutObject(Index: integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: integer);
      {$IFDEF SYN_COMPILER_3_UP} override; {$ENDIF}                             //mh 2000-10-18
    procedure SetTabWidth(Value: integer);                                      //mh 2000-10-19
    procedure SetUpdateState(Updating: Boolean); override;
    {$IFDEF SYN_LAZARUS}
    procedure SetTextStr(const Value: string); override;
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const S: string): integer; override;
    procedure AddStrings(AStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure DeleteLines(Index, NumLines: integer);                            // DJLP 2000-11-01
    procedure Exchange(Index1, Index2: integer); override;
    procedure Insert(Index: integer; const S: string); override;
    procedure InsertLines(Index, NumLines: integer);                            // DJLP 2000-11-01
    procedure InsertStrings(Index: integer; NewStrings: TStrings);              // DJLP 2000-11-01
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    {$IFDEF SYN_LAZARUS}
    procedure ClearRanges(ARange: TSynEditRange);
    {$ENDIF}
  public
    property DosFileFormat: boolean read fDosFileFormat write fDosFileFormat;
{begin}                                                                         //mh 2000-10-19
    property ExpandedStrings[Index: integer]: string read GetExpandedString;
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
{end}                                                                           //mh 2000-10-19
    property Ranges[Index: integer]: TSynEditRange read GetRange write PutRange;
    property TabWidth: integer read fTabWidth write SetTabWidth;                //mh 2000-10-19
    property OnAdded: TStringListIndexEvent read fOnAdded write fOnAdded;
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    property OnChanging: TNotifyEvent read fOnChanging write fOnChanging;
    property OnCleared: TNotifyEvent read fOnCleared write fOnCleared;
    property OnDeleted: TStringListIndexEvent read fOnDeleted write fOnDeleted;
    property OnInserted: TStringListIndexEvent read fOnInserted
      write fOnInserted;
    property OnPutted: TStringListIndexEvent read fOnPutted write fOnPutted;
    {$IFDEF SYN_LAZARUS}
    property Folded[Index: integer]: boolean read GetFolded write SetFolded;
    property FoldMinLevel[Index: integer]: integer read GetFoldMinLevel
                                                   write SetFoldMinLevel;
    property FoldEndLevel[Index: integer]: integer read GetFoldEndLevel
                                                   write SetFoldEndLevel;
    property FoldType[Index: integer]: TSynEditCodeFoldType read GetFoldType
                                                            write SetFoldType;
    {$ENDIF}
  end;

  ESynEditStringList = class(Exception);
{end}                                                                           //mh 2000-10-10

  TSynChangeReason = (crInsert, crPaste, crDragDropInsert,
    // Note: crSelDelete and crDragDropDelete have been deleted, because
    //   several undo entries can be chained together now via the ChangeNumber
    //   see also TCustomSynEdit.[Begin|End]UndoBlock methods
    crDeleteAfterCursor, crDelete, {crSelDelete, crDragDropDelete, }            //mh 2000-11-20
    crLineBreak, crIndent, crUnindent,
    crSilentDelete, crSilentDeleteAfterCursor,                                  //mh 2000-10-30
    crNothing);

  TSynEditUndoItem = class(TObject)
  public
    fChangeReason: TSynChangeReason;
    fChangeSelMode: TSynSelectionMode;
    fChangeStartPos: TPoint; // logical position (byte)
    fChangeEndPos: TPoint; // logical position (byte)
    fChangeStr: string;
    fChangeNumber: integer;                                                     //sbs 2000-11-19
  end;

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
    procedure BeginBlock;                                                       //sbs 2000-11-19
    procedure Clear;
    procedure EndBlock;                                                         //sbs 2000-11-19
    procedure Lock;
    function PeekItem: TSynEditUndoItem;
    function PopItem: TSynEditUndoItem;
    procedure PushItem(Item: TSynEditUndoItem);
    procedure Unlock;
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

{$IFDEF SYN_COMPILER_3_UP}                                                      //mh 2000-10-18
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
  fFiler := TFileStream.Create(FileName, fmOpenRead{ ToDo: or fmShareDenyWrite});
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
            if P[0] = #13 then begin
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
  fFiler := TFileStream.Create(FileName, fmCreate);
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
  inherited Create;
  fDosFileFormat := TRUE;
{begin}                                                                         //mh 2000-10-19
  fIndexOfLongestLine := -1;
  TabWidth := 8;
{end}                                                                           //mh 2000-10-19
end;

destructor TSynEditStringList.Destroy;
{$IFDEF FPC}
var i: integer;
{$ENDIF}
begin
  fOnChange := nil;
  fOnChanging := nil;
  inherited Destroy;
  {$IFDEF FPC}
  for i:=0 to fCount-1 do fList^[i].fString:='';
  {$ENDIF}
  fCount := 0;
  SetCapacity(0);
end;

function TSynEditStringList.Add(const S: string): integer;
begin
  BeginUpdate;
  Result := fCount;
  InsertItem(Result, S);
  if Assigned(fOnAdded) then
    fOnAdded(Result);
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
      i := fCount + AStrings.Count;
      if i > fCapacity then
        SetCapacity((i + 15) and (not 15));
      FirstAdded := fCount;
      for i := 0 to AStrings.Count - 1 do begin
        with fList^[fCount] do begin
          Pointer(fString) := nil;
          fString := AStrings[i];
          fObject := AStrings.Objects[i];
          fRange := NullRange;
          fExpandedLength := -1;
          fFlags := [sfExpandedLengthUnknown];
          {$IFDEF SYN_LAZARUS}
          fFolded:=false;
          fFoldMinLevel:=0;
          fFoldEndLevel:=0;
          fFoldType:=cfNone;
          {$ENDIF}
        end;
        Inc(fCount);
      end;
      if Assigned(fOnAdded) then
        fOnAdded(FirstAdded);
    finally
      EndUpdate;
    end;
  end;
{end}                                                                           //mh 2000-10-19
end;

procedure TSynEditStringList.Clear;
{$IFDEF FPC}
var i: integer;
{$ENDIF}
begin
  if fCount <> 0 then begin
    BeginUpdate;
    {$IFDEF FPC}
    for i:=0 to fCount-1 do fList^[i].fString:='';
    {$ENDIF}
    fCount := 0;
    SetCapacity(0);
    if Assigned(fOnCleared) then
      fOnCleared(Self);
    EndUpdate;
  end;
  fIndexOfLongestLine := -1;                                                    //mh 2000-10-19
end;

procedure TSynEditStringList.Delete(Index: integer);
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  {$IFDEF FPC}
  fList^[Index].fString:='';
  {$ENDIF}
  Dec(fCount);
  if Index < fCount then begin
    System.Move(fList^[Index + 1], fList^[Index],
      (fCount - Index) * SynEditStringRecSize);
  end;
  fIndexOfLongestLine := -1;                                                    //mh 2000-10-19
  if Assigned(fOnDeleted) then
    fOnDeleted(Index);
  EndUpdate;
end;

{begin}                                                                         // DJLP 2000-11-01
procedure TSynEditStringList.DeleteLines(Index, NumLines: Integer);
var
  LinesAfter: integer;
  {$IFDEF FPC}
  i: integer;
  {$ENDIF}
begin
  if NumLines > 0 then begin
    if (Index < 0) or (Index >= fCount) then
      ListIndexOutOfBounds(Index);
    LinesAfter := fCount - (Index + NumLines);
    if LinesAfter < 0 then
      NumLines := fCount - Index;
    {$IFDEF FPC}
    // free the strings
    for i:=Index to Index+NumLines-1 do
      fList^[i].fString:='';
    {$ENDIF}
    if LinesAfter > 0 then begin
      BeginUpdate;
      try
        // move
        System.Move(fList^[Index + NumLines], fList^[Index],
          LinesAfter * SynEditStringRecSize);
        {$IFDEF FPC}
        // clear unused references
        FillChar(fList^[Index + LinesAfter], NumLines * SynEditStringRecSize,0);
        {$ENDIF}
      finally
        EndUpdate;
      end;
    end;
    Dec(fCount, NumLines);
    if Assigned(fOnDeleted) then                                       
      fOnDeleted(Index);
  end;
end;
{end}                                                                           // DJLP 2000-11-01

procedure TSynEditStringList.Exchange(Index1, Index2: integer);
var
  Temp: TSynEditStringRec;
begin
  if (Index1 < 0) or (Index1 >= fCount) then
    ListIndexOutOfBounds(Index1);
  if (Index2 < 0) or (Index2 >= fCount) then
    ListIndexOutOfBounds(Index2);
  BeginUpdate;
  Temp := fList^[Index1];
  fList^[Index1] := fList^[Index2];
  fList^[Index2] := Temp;
{begin}                                                                         //mh 2000-10-19
  if fIndexOfLongestLine = Index1 then
    fIndexOfLongestLine := Index2
  else if fIndexOfLongestLine = Index2 then
    fIndexOfLongestLine := Index1;
{end}                                                                           //mh 2000-10-19
  EndUpdate;
end;

{begin}                                                                         //mh 2000-10-19
function TSynEditStringList.ExpandedString(Index: integer): string;
var
  HasTabs: boolean;
begin
  with fList^[Index] do
    if fString = '' then begin
      Result := '';
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Include(fFlags, sfHasNoTabs);
      fExpandedLength := 0;
    end else begin
      Result := fConvertTabsProc(fString, fTabWidth, HasTabs);
      fExpandedLength := Length(Result);
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Exclude(fFlags, sfHasNoTabs);
      if HasTabs then
        Include(fFlags, sfHasTabs)
      else
        Include(fFlags, sfHasNoTabs);
    end;
end;
{end}                                                                           //mh 2000-10-19

{$IFDEF SYN_LAZARUS}
function TSynEditStringList.ExpandedStringLength(Index: integer): integer;
var
  HasTabs: boolean;
begin
  with fList^[Index] do
    if length(fString) = 0 then begin
      Result := 0;
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Include(fFlags, sfHasNoTabs);
      fExpandedLength := 0;
    end else begin
      Result := fSimulateConvertTabsProc(fString, fTabWidth, HasTabs);
      fExpandedLength := Result;
      Exclude(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Exclude(fFlags, sfHasNoTabs);
      if HasTabs then
        Include(fFlags, sfHasTabs)
      else
        Include(fFlags, sfHasNoTabs);
    end;
end;

function TSynEditStringList.GetFoldEndLevel(Index: integer): integer;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fFoldEndLevel
  else
    Result := 0;
end;

function TSynEditStringList.GetFoldMinLevel(Index: integer): integer;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fFoldMinLevel
  else
    Result := 0;
end;

function TSynEditStringList.GetFoldType(Index: integer): TSynEditCodeFoldType;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fFoldType
  else
    Result := cfNone;
end;

function TSynEditStringList.GetFolded(Index: integer): boolean;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fFolded
  else
    Result := false;
  //if Result then debugln('TSynEditStringList.GetFolded Index=',dbgs(Index));
end;

procedure TSynEditStringList.SetFoldEndLevel(Index: integer;
  const AValue: integer);
begin
  if (Index >= 0) and (Index < fCount) then
    fList^[Index].fFoldEndLevel := AValue;
end;

procedure TSynEditStringList.SetFoldMinLevel(Index: integer;
  const AValue: integer);
begin
  if (Index >= 0) and (Index < fCount) then
    fList^[Index].fFoldMinLevel := AValue;
end;

procedure TSynEditStringList.SetFoldType(Index: integer;
  const AValue: TSynEditCodeFoldType);
begin
  if (Index >= 0) and (Index < fCount) then
    fList^[Index].fFoldType := AValue;
end;

procedure TSynEditStringList.SetFolded(Index: integer; const AValue: boolean);
begin
  //if AValue then RaiseGDBException('');
  if (Index >= 0) and (Index < fCount) then
    fList^[Index].fFolded := AValue;
end;
{$ENDIF}

function TSynEditStringList.Get(Index: integer): string;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fString
  else
    Result := '';
end;

function TSynEditStringList.GetCapacity: integer;
begin
  Result := fCapacity;
end;

function TSynEditStringList.GetCount: integer;
begin
  Result := fCount;
end;

{begin}                                                                         //mh 2000-10-19
function TSynEditStringList.GetExpandedString(Index: integer): string;
begin
  if (Index >= 0) and (Index < fCount) then begin
    if sfHasNoTabs in fList^[Index].fFlags then
      Result := fList^[Index].fString
    else
      Result := ExpandedString(Index);
  end else
    Result := '';
end;

function TSynEditStringList.GetLengthOfLongestLine: integer;                    //mh 2000-10-19
var
  i, MaxLen: integer;
  PRec: PSynEditStringRec;
begin
  if fIndexOfLongestLine < 0 then begin
    MaxLen := 0;
    if fCount > 0 then begin
      PRec := @fList^[0];
      for i := 0 to fCount - 1 do begin
        if sfExpandedLengthUnknown in PRec^.fFlags then
          {$IFDEF SYN_LAZARUS}
          ExpandedStringLength(i);
          {$ELSE}
          ExpandedString(i);
          {$ENDIF}
        if PRec^.fExpandedLength > MaxLen then begin
          MaxLen := PRec^.fExpandedLength;
          fIndexOfLongestLine := i;
        end;
        Inc(PRec);
      end;
    end;
  end;
  if (fIndexOfLongestLine >= 0) and (fIndexOfLongestLine < fCount) then
    Result := fList^[fIndexOfLongestLine].fExpandedLength
  else
    Result := 0;
end;
{end}                                                                           //mh 2000-10-19

function TSynEditStringList.GetObject(Index: integer): TObject;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fObject
  else
    Result := nil;
end;

function TSynEditStringList.GetRange(Index: integer): TSynEditRange;
begin
  if (Index >= 0) and (Index < fCount) then
    Result := fList^[Index].fRange
  else
    Result := nil;
end;

procedure TSynEditStringList.Grow;
var
  Delta: Integer;
begin
  if fCapacity > 64 then
    Delta := fCapacity div 4
  else
    Delta := 16;
  SetCapacity(fCapacity + Delta);
end;

procedure TSynEditStringList.Insert(Index: integer; const S: string);
begin
  if (Index < 0) or (Index > fCount) then
    ListIndexOutOfBounds(Index);
  BeginUpdate;
  InsertItem(Index, S);
  if Assigned(fOnInserted) then
    fOnInserted(Index);
  EndUpdate;
end;

procedure TSynEditStringList.InsertItem(Index: integer; const S: string);
begin
  BeginUpdate;
  if fCount = fCapacity then
    Grow;
  if Index < fCount then begin
    System.Move(fList^[Index], fList^[Index + 1],
      (fCount - Index) * SynEditStringRecSize);
  end;
  fIndexOfLongestLine := -1;                                                    //mh 2000-10-19
  with fList^[Index] do begin
    Pointer(fString) := nil;
    fString := S;
    fObject := nil;
    fRange := NullRange;
{begin}                                                                         //mh 2000-10-19
    fExpandedLength := -1;
    fFlags := [sfExpandedLengthUnknown];
    {$IFDEF SYN_LAZARUS}
    fFolded:=false;
    fFoldMinLevel:=0;
    fFoldEndLevel:=0;
    fFoldType:=cfNone;
    {$ENDIF}
{end}                                                                           //mh 2000-10-19
  end;
  Inc(fCount);
  EndUpdate;
end;

{begin}                                                                         // DJLP 2000-11-01
procedure TSynEditStringList.InsertLines(Index, NumLines: integer);
begin
  if NumLines > 0 then begin
    BeginUpdate;
    try
      {$IFDEF FPC}
      if FCapacity<fCount + NumLines then
      {$ENDIF}
        SetCapacity(fCount + NumLines);
      if Index < fCount then begin
        System.Move(fList^[Index], fList^[Index + NumLines],
          (fCount - Index) * SynEditStringRecSize);
      end;
      FillChar(fList^[Index], NumLines * SynEditStringRecSize, 0);
      Inc(fCount, NumLines);
      if Assigned(fOnAdded) then
        fOnAdded(Index);
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
  if (Index = 0) and (fCount = 0) then
    Add(S)
  else begin
    if (Index < 0) or (Index >= fCount) then
      ListIndexOutOfBounds(Index);
    BeginUpdate;
{begin}                                                                         //mh 2000-10-19
    fIndexOfLongestLine := -1;
    with fList^[Index] do begin
      Include(fFlags, sfExpandedLengthUnknown);
      Exclude(fFlags, sfHasTabs);
      Exclude(fFlags, sfHasNoTabs);
      fString := S;
    end;
{end}                                                                           //mh 2000-10-19
    if Assigned(fOnPutted) then
      fOnPutted(Index);
    EndUpdate;
  end;
end;

procedure TSynEditStringList.PutObject(Index: integer; AObject: TObject);
begin
  if (Index < 0) or (Index >= fCount) then
    ListIndexOutOfBounds(Index);
  {$IFDEF SYN_LAZARUS}
  if fList^[Index].fObject = AObject then exit;
  {$ENDIF}
  BeginUpdate;
  fList^[Index].fObject := AObject;
  EndUpdate;
end;

procedure TSynEditStringList.PutRange(Index: integer; ARange: TSynEditRange);
begin
  if (Index < 0) or (Index >= fCount) then
    ListIndexOutOfBounds(Index);
  {$IFDEF SYN_LAZARUS}
  // do not call BeginUpdate/EndUpdate. It would call the too generic OnChange
  // events
  fList^[Index].fRange := ARange;
  {$ELSE}
  BeginUpdate;
  fList^[Index].fRange := ARange;
  EndUpdate;
  {$ENDIF}
end;

procedure TSynEditStringList.SaveToFile(const FileName: string);
var
  Writer: TSynEditFileWriter;
  i: integer;
begin
  Writer := TSynEditFileWriter.Create(FileName);
  try
    Writer.DosFile := fDosFileFormat;
    for i := 0 to fCount - 1 do
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
  for Index:=0 to fCount-1 do
    fList^[Index].fRange := ARange;
end;
{$ENDIF}

procedure TSynEditStringList.SetCapacity(NewCapacity: integer);
begin
  ReallocMem(fList, NewCapacity * SynEditStringRecSize);
  fCapacity := NewCapacity;
end;

{begin}                                                                         //mh 2000-10-19
procedure TSynEditStringList.SetTabWidth(Value: integer);
var
  i: integer;
begin
  if Value <> fTabWidth then begin
    fTabWidth := Value;
    fConvertTabsProc := GetBestConvertTabsProcEx(fTabWidth);
    {$IFDEF SYN_LAZARUS}
    fSimulateConvertTabsProc := GetBestSimulateConvertTabsProcEx(fTabWidth);
    {$ENDIF}
    fIndexOfLongestLine := -1;
{begin}                                                                         //mh 2000-11-08
    for i := 0 to fCount - 1 do
      with fList^[i] do begin
        fExpandedLength := -1;
        Exclude(fFlags, sfHasNoTabs);
        Include(fFlags, sfExpandedLengthUnknown);
      end;
{end}                                                                           //mh 2000-11-08
  end;
end;
{end}                                                                           //mh 2000-10-19

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

{$IFDEF SYN_LAZARUS}
procedure TSynEditStringList.SetTextStr(const Value: string);
var
  StartPos: Integer;
  p: Integer;
  Len: Integer;
begin
  BeginUpdate;
  try
    Clear;
    p:=1;
    StartPos:=p;
    Len:=length(Value);
    while p<=Len do begin
      if not (Value[p] in [#10,#13]) then begin
        inc(p);
      end else begin
        Add(copy(Value,StartPos,p-StartPos));
        inc(p);
        if (p<=Len) and (Value[p] in [#10,#13]) and (Value[p-1]<>Value[p]) then
          inc(p);
        StartPos:=p;
      end;
    end;
    if StartPos<=Len then
      Add(copy(Value,StartPos,Len-StartPos+1));
  finally
    EndUpdate;
  end;
end;
{$ENDIF}

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
      PushItem(NewItem);
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

end.

