{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

-------------------------------------------------------------------------------}
unit LazSynEditText;

{$I synedit.inc}
{$IFOPT C+}
  {$DEFINE AssertSynMemIndex}
  {$DEFINE SynAssert}
{$ENDIF}
{$IFDEF SynAssert}
  {$DEFINE AssertSynMemIndex}
{$ENDIF}

interface

uses
  Classes, SysUtils, Graphics, LCLProc, SynEditTypes, SynEditMiscProcs,
  SynEditHighlighter, SynEditKeyCmds, SynEditTextBase;

type
  TSynEditStrings = class;

  TStringListLineCountEvent = procedure(Sender: TSynEditStrings;
                                        Index, Count: Integer) of object;
  TStringListLineEditEvent = procedure(Sender: TSynEditStrings;
                                       LinePos, BytePos, Count, LineBrkCnt: Integer;
                                       Text: String) of object;

  TSynEditNotifyReason = ( // TStringListLineCountEvent
                           senrLineCount,        // Lines Inserted or Deleted (if not empty, they will trigger senrLineChange too)
                           senrLineChange,       // Lines modified (also triggered by senrEditAction)
                           senrHighlightChanged, // used by Highlighter (invalidate and fold checks needed)
                           // TStringListLineEditEvent
                           senrEditAction,       // EditInsert, EditDelete, EditLineBreak, ...
                           // TNotifyEvent
                           senrCleared,
                           senrUndoRedoAdded,
                           senrModifiedChanged,  // The modified flag was changed
                           // Paintlocks are managed by SynEdit, but need distribution to shared edits
                           senrIncOwnedPaintLock, // Inform other SynEdits (ForeignPaintLock)
                           senrDecOwnedPaintLock,
                           senrIncPaintLock,      // Actual PaintLock
                           senrDecPaintLock,
                           senrAfterIncPaintLock, // For plugins, etc...
                           senrBeforeDecPaintLock,
                           senrTextBufferChanging, // About to change
                           senrTextBufferChanged
                          );

  TPhysicalCharWidths = Array of Shortint;
  TPhysicalCharWidth = ShortInt;
  PPhysicalCharWidth = ^TPhysicalCharWidth;

  { TSynLogicalPhysicalConvertor }
const
  SYN_LP_MIN_ALLOC = 1024; // Keep at least n*SizeOf(TPhysicalCharWidths) allocated
type
  TSynLogicalPhysicalConvertor = class
  private
    FLines: TSynEditStrings;
    FCurrentWidths: TPhysicalCharWidths;
    FCurrentWidthsLen, FCurrentWidthsAlloc: Integer;
    FCurrentLine: Integer;
    FTextChangeStamp, FViewChangeStamp: Int64;
    // TODOtab-width
    procedure PrepareWidthsForLine(AIndex: Integer; AForce: Boolean = False);
  public
    constructor Create(ALines: TSynEditStrings);
    destructor Destroy; override;
    // Line is 0-based // Column is 1-based
    function PhysicalToLogical(AIndex, AColumn: Integer): Integer;
    function PhysicalToLogical(AIndex, AColumn: Integer; out AColOffset: Integer): Integer;
    // ACharPos 1=before 1st char
    function LogicalToPhysical(AIndex, ABytePos: Integer): Integer;
    function LogicalToPhysical(AIndex, ABytePos: Integer; var AColOffset: Integer): Integer;
  end;

  (*
  TLazSynDisplayView:
    - Represents the visible text
      e.g. excludes folds, maps wrapped lines, ...

  TSynEditStrings:
    - Represents the entire text,
      inluding temporary chars or text that may not be displayed.
      Temporary chars (e.g. trailing spaces) may not be accesibl via SynEdit.Lines
    - Can be Edited
  *)

  TLazSynDisplayTokenInfo = record
    TokenStart: PChar;
    TokenLength: integer;
    TokenAttr: TSynHighlighterAttributes
  end;

  { TLazSynDisplayView }

  TLazSynDisplayView = class
  private
    FNextView: TLazSynDisplayView;
  public
    constructor Create;
    property NextView: TLazSynDisplayView read FNextView write FNextView;
  public
    procedure InitHighlighterTokens(AHighlighter: TSynCustomHighlighter); virtual;
    procedure SetHighlighterTokensLine(ALine: TLineIdx; out ARealLine: TLineIdx); virtual;
    procedure FinishHighlighterTokens; virtual;
    function  GetNextHighlighterToken(out ATokenInfo: TLazSynDisplayTokenInfo): Boolean; virtual;
    function GetDrawDividerInfo: TSynDividerDrawConfigSetting; virtual;
    // todo: gutter info
  end;

  { TLazSynDisplayViewEx }

  TLazSynDisplayViewEx = class(TLazSynDisplayView)
  private
    FInitialized: boolean;
    FCurrentTokenHighlighter: TSynCustomHighlighter;
    FCurrentTokenLine: TLineIdx;
    procedure SetCurrentTokenLine(AValue: TLineIdx);
  protected
    property Initialized: boolean read FInitialized;
    property CurrentTokenHighlighter: TSynCustomHighlighter read FCurrentTokenHighlighter;
    property CurrentTokenLine: TLineIdx read FCurrentTokenLine write SetCurrentTokenLine;
  public
    constructor Create;
    procedure InitHighlighterTokens(AHighlighter: TSynCustomHighlighter); override;
    procedure FinishHighlighterTokens; override;
  end;

  { TSynEditStrings }

  TSynEditStrings = class(TSynEditStringsBase)
  private
    FSenderUpdateCount: Integer;
    FLogPhysConvertor :TSynLogicalPhysicalConvertor;
  protected
    FIsUtf8: Boolean;
    function  GetIsUtf8 : Boolean; virtual;
    procedure SetIsUtf8(const AValue : Boolean); virtual;

    function GetExpandedString(Index: integer): string; virtual; abstract;
    function GetLengthOfLongestLine: integer; virtual; abstract;
    procedure SetTextStr(const Value: string); override;
    function GetTextChangeStamp: int64; virtual; abstract;
    function GetViewChangeStamp: int64; virtual;

    function GetIsInEditAction: Boolean; virtual; abstract;
    procedure IncIsInEditAction; virtual; abstract;
    procedure DecIsInEditAction; virtual; abstract;
    function GetUndoList: TSynEditUndoList; virtual; abstract;
    function GetRedoList: TSynEditUndoList; virtual; abstract;
    function GetCurUndoList: TSynEditUndoList; virtual; abstract;
    procedure SetIsUndoing(const AValue: Boolean); virtual; abstract;
    function  GetIsUndoing: Boolean; virtual; abstract;
    procedure SetIsRedoing(const AValue: Boolean); virtual; abstract;
    function  GetIsRedoing: Boolean; virtual; abstract;
    procedure IgnoreSendNotification(AReason: TSynEditNotifyReason;
                                     ReEnable: Boolean); virtual; abstract;

    procedure SetUpdateState(Updating: Boolean); override;
    procedure SetUpdateState(Updating: Boolean; Sender: TObject); virtual; abstract;

    procedure DoGetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer; PWidths: PPhysicalCharWidth); virtual; abstract;

    function GetDisplayView: TLazSynDisplayView; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginUpdate(Sender: TObject); overload;
    procedure EndUpdate(Sender: TObject); overload;
    function  IsUpdating: Boolean;

    // Currently Lines are physical
    procedure DeleteLines(Index, NumLines: integer); virtual; abstract;
    procedure InsertLines(Index, NumLines: integer); virtual; abstract;
    procedure InsertStrings(Index: integer; NewStrings: TStrings); virtual; abstract;

    procedure AddGenericHandler(AReason: TSynEditNotifyReason;
                AHandler: TMethod); virtual; abstract;
    procedure AddChangeHandler(AReason: TSynEditNotifyReason;
                AHandler: TStringListLineCountEvent);
    procedure AddNotifyHandler(AReason: TSynEditNotifyReason;
                AHandler: TNotifyEvent);

    procedure RemoveGenericHandler(AReason: TSynEditNotifyReason;
                AHandler: TMethod); virtual; abstract;
    procedure RemoveChangeHandler(AReason: TSynEditNotifyReason;
                AHandler: TStringListLineCountEvent);
    procedure RemoveNotifyHandler(AReason: TSynEditNotifyReason;
                AHandler: TNotifyEvent);

    procedure AddEditHandler(AHandler: TStringListLineEditEvent);
    procedure RemoveEditHandler(AHandler: TStringListLineEditEvent);
    procedure SendHighlightChanged(aIndex, aCount: Integer); override;
    procedure SendNotification(AReason: TSynEditNotifyReason;
                ASender: TSynEditStrings; aIndex, aCount: Integer;
                aBytePos: Integer = -1; aLen: Integer = 0;
                aTxt: String = ''); virtual; abstract;
    procedure SendNotification(AReason: TSynEditNotifyReason;
                ASender: TObject); virtual; abstract;
   procedure FlushNotificationCache; virtual; abstract;
  public
    function GetPhysicalCharWidths(Index: Integer): TPhysicalCharWidths;
    function GetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer): TPhysicalCharWidths;
    // Byte to Char
    function LogicalToPhysicalPos(const p: TPoint): TPoint;
    function LogicalToPhysicalCol(const Line: String;
                                  Index, LogicalPos: integer): integer; virtual;
    // Char to Byte
    function PhysicalToLogicalPos(const p: TPoint): TPoint;
    function PhysicalToLogicalCol(const Line: string;
                                  Index, PhysicalPos: integer): integer; virtual;
    property LogPhysConvertor :TSynLogicalPhysicalConvertor read FLogPhysConvertor;
  public
    // Currently Lines are physical
    procedure EditInsert(LogX, LogY: Integer; AText: String); virtual; abstract;
    function  EditDelete(LogX, LogY, ByteLen: Integer): String; virtual; abstract;
    procedure EditLineBreak(LogX, LogY: Integer); virtual; abstract;
    procedure EditLineJoin(LogY: Integer; FillText: String = ''); virtual; abstract;
    procedure EditLinesInsert(LogY, ACount: Integer; AText: String = ''); virtual; abstract;
    procedure EditLinesDelete(LogY, ACount: Integer); virtual; abstract;
    procedure EditUndo(Item: TSynEditUndoItem); virtual; abstract;
    procedure EditRedo(Item: TSynEditUndoItem); virtual; abstract;
    property IsInEditAction: Boolean read GetIsInEditAction;
    property UndoList: TSynEditUndoList read GetUndoList;
    property RedoList: TSynEditUndoList read GetRedoList;
    property CurUndoList: TSynEditUndoList read GetCurUndoList; // Re or Undo (Redo while undoing)
    property IsUndoing: Boolean read GetIsUndoing write SetIsUndoing;
    property IsRedoing: Boolean read GetIsRedoing write SetIsRedoing;
  public
    property TextChangeStamp: int64 read GetTextChangeStamp;
    property ViewChangeStamp: int64 read GetViewChangeStamp; // tabs-size, trailing-spaces, ...
    property ExpandedStrings[Index: integer]: string read GetExpandedString;
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
    property IsUtf8: Boolean read GetIsUtf8 write SetIsUtf8;
  public
    property DisplayView: TLazSynDisplayView read GetDisplayView;
  end;

  { TSynEditStringsLinked }

  TSynEditStringsLinked = class(TSynEditStrings)
  private
    procedure SetSynStrings(AValue: TSynEditStrings);
  protected
    fSynStrings: TSynEditStrings;

    function  GetIsUtf8 : Boolean;  override;
    procedure SetIsUtf8(const AValue : Boolean);  override;
    function GetTextChangeStamp: int64; override;
    function GetViewChangeStamp: int64; override;

    function GetRange(Index: Pointer): TSynManagedStorageMem; override;
    procedure PutRange(Index: Pointer; const ARange: TSynManagedStorageMem); override;

    function GetExpandedString(Index: integer): string; override;
    function GetLengthOfLongestLine: integer; override;

    procedure IgnoreSendNotification(AReason: TSynEditNotifyReason;
                                     IncIgnore: Boolean); override;
    function GetIsInEditAction: Boolean; override;
    procedure IncIsInEditAction; override;
    procedure DecIsInEditAction; override;
    function GetUndoList: TSynEditUndoList; override;
    function GetRedoList: TSynEditUndoList; override;
    function GetCurUndoList: TSynEditUndoList; override;
    procedure SetIsUndoing(const AValue: Boolean); override;
    function  GetIsUndoing: Boolean; override;
    procedure SetIsRedoing(const AValue: Boolean); override;
    function  GetIsRedoing: Boolean; override;
  protected
    function GetCount: integer; override;
    function GetCapacity: integer;
      {$IFDEF SYN_COMPILER_3_UP} override; {$ELSE} virtual; {$ENDIF}
    procedure SetCapacity(NewCapacity: integer);
      {$IFDEF SYN_COMPILER_3_UP} override; {$ELSE} virtual; {$ENDIF}
    function  Get(Index: integer): string; override;
    function  GetObject(Index: integer): TObject; override;
    procedure Put(Index: integer; const S: string); override;
    procedure PutObject(Index: integer; AObject: TObject); override;

    procedure SetUpdateState(Updating: Boolean; Sender: TObject); override;
    procedure DoGetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer; PWidths: PPhysicalCharWidth); override;

    function GetDisplayView: TLazSynDisplayView; override;
  public
    constructor Create(ASynStringSource: TSynEditStrings);

    function Add(const S: string): integer; override;
    procedure AddStrings(AStrings: TStrings); override;
    procedure Clear; override;
    procedure Delete(Index: integer); override;
    procedure DeleteLines(Index, NumLines: integer);  override;
    procedure Insert(Index: integer; const S: string); override;
    procedure InsertLines(Index, NumLines: integer); override;
    procedure InsertStrings(Index: integer; NewStrings: TStrings); override;
    function  GetPChar(ALineIndex: Integer; out ALen: Integer): PChar; override; // experimental

    procedure AddGenericHandler(AReason: TSynEditNotifyReason;
                AHandler: TMethod); override;
    procedure RemoveGenericHandler(AReason: TSynEditNotifyReason;
                AHandler: TMethod); override;
    procedure SendNotification(AReason: TSynEditNotifyReason;
                ASender: TSynEditStrings; aIndex, aCount: Integer;
                aBytePos: Integer = -1; aLen: Integer = 0;
                aTxt: String = ''); override;
    procedure SendNotification(AReason: TSynEditNotifyReason;
                ASender: TObject); override;
   procedure FlushNotificationCache; override;

    //function GetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer): TPhysicalCharWidths; override;
    property NextLines: TSynEditStrings read fSynStrings write SetSynStrings;
  public
    // LogX, LogY are 1-based
    procedure EditInsert(LogX, LogY: Integer; AText: String); override;
    function  EditDelete(LogX, LogY, ByteLen: Integer): String; override;
    procedure EditLineBreak(LogX, LogY: Integer); override;
    procedure EditLineJoin(LogY: Integer; FillText: String = ''); override;
    procedure EditLinesInsert(LogY, ACount: Integer; AText: String = ''); override;
    procedure EditLinesDelete(LogY, ACount: Integer); override;
    procedure EditUndo(Item: TSynEditUndoItem); override;
    procedure EditRedo(Item: TSynEditUndoItem); override;
  end;


implementation

{ TLazSynDisplayViewEx }

procedure TLazSynDisplayViewEx.SetCurrentTokenLine(AValue: TLineIdx);
begin
  {$IFDEF SynAssert}
  if  not Initialized then
    raise Exception.Create('uninitialized SetCurrentTokenLine');
  {$ENDIF}
  FCurrentTokenLine := AValue;
end;

constructor TLazSynDisplayViewEx.Create;
begin
  inherited Create;
  FInitialized := False;
end;

procedure TLazSynDisplayViewEx.InitHighlighterTokens(AHighlighter: TSynCustomHighlighter);
begin
  {$IFDEF SynAssert}
  if FInitialized then
    raise Exception.Create('Nested InitHighlighterTokensForLine');
  {$ENDIF}
  FCurrentTokenHighlighter := AHighlighter;
  FCurrentTokenLine := -1;
  FInitialized := True;
  inherited;
end;

procedure TLazSynDisplayViewEx.FinishHighlighterTokens;
begin
  FCurrentTokenHighlighter := nil;
  FInitialized := False;
  inherited;
end;

{ TLazSynDisplayView }

constructor TLazSynDisplayView.Create;
begin
  FNextView := nil;
end;

procedure TLazSynDisplayView.InitHighlighterTokens(AHighlighter: TSynCustomHighlighter);
begin
  if assigned(FNextView) then
    FNextView.InitHighlighterTokens(AHighlighter);
end;

procedure TLazSynDisplayView.SetHighlighterTokensLine(ALine: TLineIdx; out ARealLine: TLineIdx);
begin
  if assigned(FNextView) then
    FNextView.SetHighlighterTokensLine(ALine, ARealLine);
end;

procedure TLazSynDisplayView.FinishHighlighterTokens;
begin
  if assigned(FNextView) then
    FNextView.FinishHighlighterTokens;
end;

function TLazSynDisplayView.GetNextHighlighterToken(out ATokenInfo: TLazSynDisplayTokenInfo): Boolean;
begin
  if assigned(FNextView) then
    Result := FNextView.GetNextHighlighterToken(ATokenInfo)
  else
    Result := False;
end;

function TLazSynDisplayView.GetDrawDividerInfo: TSynDividerDrawConfigSetting;
begin
  if assigned(FNextView) then
    Result := FNextView.GetDrawDividerInfo
  else
    Result.Color := clNone;
end;

{ TSynLogicalPhysicalConvertor }

procedure TSynLogicalPhysicalConvertor.PrepareWidthsForLine(AIndex: Integer;
  AForce: Boolean);
var
  LineLen: Integer;
  Line: PChar;
//const dbg_cnt: integer = 0;
begin
  if (not AForce) and (FCurrentLine = AIndex) and
     (FLines.TextChangeStamp = FTextChangeStamp) and (FLines.ViewChangeStamp = FViewChangeStamp)
  then begin
    //debugln(['**************** RE-USING widths: ', AIndex,' (',dbgs(Pointer(self)),')']);
    //dbg_cnt := dbg_cnt + 1;
    exit;
  end;

  if (AIndex < 0) or (AIndex >= FLines.Count) then begin
    FCurrentWidthsLen := 0;
    FViewChangeStamp := FLines.ViewChangeStamp;
    FTextChangeStamp := FLines.TextChangeStamp;
    exit;
  end;

  Line := FLines.GetPChar(AIndex, LineLen);
  if LineLen > FCurrentWidthsAlloc then begin
    //debugln(['**************** COMPUTING widths (grow): ', AIndex,' (',dbgs(Pointer(self)),') old-alloc=', FCurrentWidthsAlloc, '  new-len=',LineLen]);
    SetLength(FCurrentWidths, LineLen);
    FCurrentWidthsAlloc := LineLen;
  end
  else if FCurrentWidthsAlloc > Max(Max(LineLen, FCurrentWidthsLen)*4, SYN_LP_MIN_ALLOC) then begin
    //debugln(['**************** COMPUTING widths (shrink): ', AIndex,' (',dbgs(Pointer(self)),') old-alloc=', FCurrentWidthsAlloc, '  new-len=',LineLen]);
    FCurrentWidthsAlloc := Max(Max(LineLen, FCurrentWidthsLen), SYN_LP_MIN_ALLOC) ;
    SetLength(FCurrentWidths, FCurrentWidthsAlloc);
  //end
  //else begin
  //  debugln(['**************** COMPUTING widths: ', AIndex,' (',dbgs(Pointer(self)),') alloc=',FCurrentWidthsAlloc]);
  end;
  //debugln(['**************** NEW for index:: ', AIndex,' (',dbgs(Pointer(self)),') after index: ', FCurrentLine, ' used ', dbg_cnt,' times // old-alloc=', FCurrentWidthsAlloc, '  new-len=',LineLen, '  viewchg:',dbgs(not(FViewChangeStamp=FLines.ViewChangeStamp)),' txtchg:',dbgs(not(FTextChangeStamp=FLines.TextChangeStamp))]); dbg_cnt := 0;

  FCurrentWidthsLen := LineLen;
  if LineLen > 0 then
    FLines.DoGetPhysicalCharWidths(Line, LineLen, AIndex, @FCurrentWidths[0]);
  FViewChangeStamp := FLines.ViewChangeStamp;
  FTextChangeStamp := FLines.TextChangeStamp;
  FCurrentLine := AIndex;
end;

constructor TSynLogicalPhysicalConvertor.Create(ALines: TSynEditStrings);
begin
  FLines := ALines;
  FCurrentLine := -1;
  FCurrentWidths := nil;
  FCurrentWidthsLen := 0;
  FCurrentWidthsAlloc := 0;
end;

destructor TSynLogicalPhysicalConvertor.Destroy;
begin
  SetLength(FCurrentWidths, 0);
  inherited Destroy;
end;

function TSynLogicalPhysicalConvertor.PhysicalToLogical(AIndex,
  AColumn: Integer): Integer;
var
  ColOffs: Integer;
begin
  Result := PhysicalToLogical(AIndex, AColumn, ColOffs);
end;

function TSynLogicalPhysicalConvertor.PhysicalToLogical(AIndex, AColumn: Integer;
  out AColOffset: Integer): Integer;
var
  BytePos, ScreenPos, ScreenPosOld: integer;
begin
  PrepareWidthsForLine(AIndex);

  ScreenPos := 1;
  BytePos := 0;
  while BytePos < FCurrentWidthsLen do begin
    ScreenPosOld := ScreenPos;
    ScreenPos := ScreenPos + FCurrentWidths[BytePos];
    inc(BytePos);
    if ScreenPos > AColumn then begin
      AColOffset := AColumn - ScreenPosOld;
      exit(BytePos);
    end;
  end;

  // Column at/past end of line
  AColOffset := 0;
  Result := BytePos + 1 + AColumn - ScreenPos;
end;

function TSynLogicalPhysicalConvertor.LogicalToPhysical(AIndex,
  ABytePos: Integer): Integer;
var
  ColOffs: Integer;
begin
  ColOffs := 0;
  Result := LogicalToPhysical(AIndex, ABytePos, ColOffs);
end;

function TSynLogicalPhysicalConvertor.LogicalToPhysical(AIndex, ABytePos: Integer;
  var AColOffset: Integer): Integer;
var
  i: integer;
  CharWidths: TPhysicalCharWidths;
begin
  {$IFDEF AssertSynMemIndex}
  if (ABytePos <= 0) then
    raise Exception.Create(Format('Bad Bytpos for PhystoLogical BytePos=%d ColOffs= %d idx= %d',[ABytePos, AColOffset, AIndex]));
  {$ENDIF}
  if (ABytePos = 0) or ((ABytePos = 1) and (AColOffset=0)) then
    exit(ABytePos);
  PrepareWidthsForLine(AIndex);

  dec(ABytePos);
  if ABytePos >= FCurrentWidthsLen then begin
    Result := 1 + ABytePos - FCurrentWidthsLen;
    ABytePos := FCurrentWidthsLen;
    AColOffset := 0;
  end
  else begin
    AColOffset := Min(AColOffset, FCurrentWidths[ABytePos]-1);
    Result := 1 + AColOffset;
  end;

  for i := 0 to ABytePos - 1 do
    Result := Result + FCurrentWidths[i];
end;

{ TSynEditStrings }

constructor TSynEditStrings.Create;
begin
  FLogPhysConvertor := TSynLogicalPhysicalConvertor.Create(self);
  inherited Create;
  IsUtf8 := True;
end;

destructor TSynEditStrings.Destroy;
begin
  FreeAndNil(FLogPhysConvertor);
  inherited Destroy;
end;

procedure TSynEditStrings.BeginUpdate(Sender: TObject);
begin
  if FSenderUpdateCount = 0 then
    SetUpdateState(true, Sender);
  inc(FSenderUpdateCount);
end;

procedure TSynEditStrings.EndUpdate(Sender: TObject);
begin
  If FSenderUpdateCount>0 then
    Dec(FSenderUpdateCount);
  if FSenderUpdateCount=0 then
    SetUpdateState(False, Sender);
end;

function TSynEditStrings.IsUpdating: Boolean;
begin
  Result := (FSenderUpdateCount > 0) or (UpdateCount > 0);
end;

procedure TSynEditStrings.AddChangeHandler(AReason: TSynEditNotifyReason;
  AHandler: TStringListLineCountEvent);
begin
  AddGenericHandler(AReason, TMethod(AHandler));
end;

procedure TSynEditStrings.AddNotifyHandler(AReason: TSynEditNotifyReason;
  AHandler: TNotifyEvent);
begin
  AddGenericHandler(AReason, TMethod(AHandler));
end;

procedure TSynEditStrings.RemoveChangeHandler(AReason: TSynEditNotifyReason;
  AHandler: TStringListLineCountEvent);
begin
  RemoveGenericHandler(AReason, TMethod(AHandler));
end;

procedure TSynEditStrings.RemoveNotifyHandler(AReason: TSynEditNotifyReason;
  AHandler: TNotifyEvent);
begin
  RemoveGenericHandler(AReason, TMethod(AHandler));
end;

procedure TSynEditStrings.AddEditHandler(AHandler: TStringListLineEditEvent);
begin
  AddGenericHandler(senrEditAction, TMethod(AHandler));
end;

procedure TSynEditStrings.RemoveEditHandler(AHandler: TStringListLineEditEvent);
begin
  RemoveGenericHandler(senrEditAction, TMethod(AHandler));
end;

procedure TSynEditStrings.SendHighlightChanged(aIndex, aCount: Integer);
begin
  SendNotification(senrHighlightChanged, Self, aIndex, aCount);
end;

function TSynEditStrings.GetPhysicalCharWidths(Index: Integer): TPhysicalCharWidths;
var
  s: string;
begin
  s := Strings[Index];
  Result := GetPhysicalCharWidths(PChar(s), length(s), Index);
end;

function TSynEditStrings.GetPhysicalCharWidths(Line: PChar; LineLen,
  Index: Integer): TPhysicalCharWidths;
begin
  SetLength(Result, LineLen);
  if LineLen = 0 then
    exit;
  DoGetPhysicalCharWidths(Line, LineLen, Index, @Result[0]);
end;

function TSynEditStrings.GetDisplayView: TLazSynDisplayView;
begin
  Result := nil;
end;

function TSynEditStrings.GetIsUtf8 : Boolean;
begin
  Result := FIsUtf8;
end;

procedure TSynEditStrings.SetIsUtf8(const AValue : Boolean);
begin
  FIsUtf8 := AValue;
end;

procedure TSynEditStrings.SetTextStr(const Value : string);
var
  StartPos: PChar;
  p: PChar;
  Last: PChar;
  sl: TStringList;
  s: string;
begin
  if Value='' then begin
    Clear;
    exit;
  end;
  BeginUpdate;
  sl:=TStringList.Create;
  try
    Clear;
    p:=PChar(Value);
    StartPos:=p;
    Last:=p+length(Value);
    while p<Last do begin
      if not (p^ in [#10,#13]) then begin
        inc(p);
      end else begin
        SetLength(s,p-StartPos);
        if s<>'' then
          System.Move(StartPos^,s[1],length(s));
        sl.Add(s);
        if (p[1] in [#10,#13]) and (p[1]<>p^) then
          inc(p);
        inc(p);
        StartPos:=p;
      end;
    end;
    if StartPos<Last then begin
      SetLength(s,Last-StartPos);
      if s<>'' then
        System.Move(StartPos^,s[1],length(s));
      sl.Add(s);
    end;
    AddStrings(sl);
  finally
    sl.Free;
    EndUpdate;
  end;
end;

function TSynEditStrings.GetViewChangeStamp: int64;
begin
  Result := 0;
end;

procedure TSynEditStrings.SetUpdateState(Updating: Boolean);
begin
  // Update/check "FSenderUpdateCount", to avoid extra locking/unlocking
  if Updating then
    BeginUpdate(nil)
  else
    EndUpdate(nil);
end;

function TSynEditStrings.LogicalToPhysicalPos(const p : TPoint) : TPoint;
begin
  Result := p;
  Result.X := FLogPhysConvertor.LogicalToPhysical(p.y - 1, p.x);
//  if Result.Y - 1 < Count then
//    Result.X:=LogicalToPhysicalCol(self[Result.Y - 1], Result.Y, Result.X);
end;

function TSynEditStrings.LogicalToPhysicalCol(const Line : String;
  Index, LogicalPos: integer) : integer;
var
  i, ByteLen: integer;
  CharWidths: TPhysicalCharWidths;
begin
  CharWidths := GetPhysicalCharWidths(Pchar(Line), length(Line), Index);
  ByteLen := length(Line);
  dec(LogicalPos);

  if LogicalPos > ByteLen then begin
    Result := 1 + LogicalPos - ByteLen;
    LogicalPos := ByteLen;
  end
  else
    Result := 1;

  for i := 0 to LogicalPos - 1 do
    Result := Result + CharWidths[i];
end;

function TSynEditStrings.PhysicalToLogicalPos(const p : TPoint) : TPoint;
begin
  Result := p;
  Result.X := FLogPhysConvertor.PhysicalToLogical(p.y - 1, p.x);
//  if (Result.Y>=1) and (Result.Y <= Count) then
//    Result.X:=PhysicalToLogicalCol(self[Result.Y - 1], Result.Y - 1, Result.X);
end;

function TSynEditStrings.PhysicalToLogicalCol(const Line : string;
  Index, PhysicalPos : integer) : integer;
var
  BytePos, ByteLen: integer;
  ScreenPos: integer;
  CharWidths: TPhysicalCharWidths;
begin
  CharWidths := GetPhysicalCharWidths(PChar(Line), length(Line), Index);
  ByteLen := Length(Line);
  ScreenPos := 1;
  BytePos := 0;

  while BytePos < ByteLen do begin
    if ScreenPos + CharWidths[BytePos] > PhysicalPos then
      exit(BytePos+1);
    ScreenPos := ScreenPos + CharWidths[BytePos];
    inc(BytePos);
  end;

  Result := BytePos + 1 + PhysicalPos - ScreenPos;
end;

{ TSynEditStringsLinked }

constructor TSynEditStringsLinked.Create(ASynStringSource: TSynEditStrings);
begin
  fSynStrings := ASynStringSource;
  Inherited Create;
end;

function TSynEditStringsLinked.Add(const S: string): integer;
begin
  Result := fSynStrings.Add(S);
end;

procedure TSynEditStringsLinked.AddStrings(AStrings: TStrings);
begin
  fSynStrings.AddStrings(AStrings);
end;

procedure TSynEditStringsLinked.Clear;
begin
  fSynStrings.Clear;
end;

procedure TSynEditStringsLinked.Delete(Index: integer);
begin
  fSynStrings.Delete(Index);
end;

procedure TSynEditStringsLinked.DeleteLines(Index, NumLines: integer);
begin
  fSynStrings.DeleteLines(Index, NumLines);
end;

procedure TSynEditStringsLinked.Insert(Index: integer; const S: string);
begin
  fSynStrings.Insert(Index, S);
end;

procedure TSynEditStringsLinked.InsertLines(Index, NumLines: integer);
begin
  fSynStrings.InsertLines(Index, NumLines);
end;

procedure TSynEditStringsLinked.InsertStrings(Index: integer; NewStrings: TStrings);
begin
  fSynStrings.InsertStrings(Index, NewStrings);
end;

function TSynEditStringsLinked.GetPChar(ALineIndex: Integer; out ALen: Integer): PChar;
begin
  Result := fSynStrings.GetPChar(ALineIndex, ALen);
end;

procedure TSynEditStringsLinked.SetIsUndoing(const AValue: Boolean);
begin
  fSynStrings.IsUndoing := AValue;
end;

function TSynEditStringsLinked.GetIsUndoing: Boolean;
begin
  Result := fSynStrings.IsUndoing;
end;

procedure TSynEditStringsLinked.SetIsRedoing(const AValue: Boolean);
begin
  fSynStrings.IsRedoing := AValue;
end;

function TSynEditStringsLinked.GetIsRedoing: Boolean;
begin
  Result := fSynStrings.IsRedoing;
end;

procedure TSynEditStringsLinked.SetSynStrings(AValue: TSynEditStrings);
begin
  if fSynStrings = AValue then Exit;
  fSynStrings := AValue;
  if DisplayView <> nil then begin
    if fSynStrings = nil
    then DisplayView.NextView := nil
    else DisplayView.NextView := fSynStrings.DisplayView;
  end;
end;

function TSynEditStringsLinked.GetIsUtf8: Boolean;
begin
  Result := FSynStrings.IsUtf8;
end;

procedure TSynEditStringsLinked.SetIsUtf8(const AValue: Boolean);
begin
  FSynStrings.IsUtf8 := AValue;
end;

function TSynEditStringsLinked.GetTextChangeStamp: int64;
begin
  Result := fSynStrings.GetTextChangeStamp;
end;

function TSynEditStringsLinked.GetViewChangeStamp: int64;
begin
  Result := fSynStrings.GetViewChangeStamp;
end;

//Ranges
function TSynEditStringsLinked.GetRange(Index: Pointer): TSynManagedStorageMem;
begin
  Result:= fSynStrings.Ranges[Index];
end;

procedure TSynEditStringsLinked.PutRange(Index: Pointer; const ARange: TSynManagedStorageMem);
begin
  fSynStrings.Ranges[Index] := ARange;
end;

function TSynEditStringsLinked.GetExpandedString(Index: integer): string;
begin
  Result:= fSynStrings.GetExpandedString(Index);
end;

function TSynEditStringsLinked.GetLengthOfLongestLine: integer;
begin
  Result:= fSynStrings.GetLengthOfLongestLine;
end;

function TSynEditStringsLinked.GetRedoList: TSynEditUndoList;
begin
  Result := fSynStrings.GetRedoList;
end;

function TSynEditStringsLinked.GetUndoList: TSynEditUndoList;
begin
  Result := fSynStrings.GetUndoList;
end;

function TSynEditStringsLinked.GetCurUndoList: TSynEditUndoList;
begin
  Result := fSynStrings.GetCurUndoList;
end;

procedure TSynEditStringsLinked.AddGenericHandler(AReason: TSynEditNotifyReason; AHandler: TMethod);
begin
  fSynStrings.AddGenericHandler(AReason, AHandler);
end;

procedure TSynEditStringsLinked.RemoveGenericHandler(AReason: TSynEditNotifyReason; AHandler: TMethod);
begin
  fSynStrings.RemoveGenericHandler(AReason, AHandler);
end;

// Count
function TSynEditStringsLinked.GetCount: integer;
begin
  Result:= fSynStrings.Count;
end;

function TSynEditStringsLinked.GetCapacity: integer;
begin
  Result:= fSynStrings.Capacity;
end;

procedure TSynEditStringsLinked.SetCapacity(NewCapacity: integer);
begin
  fSynStrings.Capacity := NewCapacity;
end;

function TSynEditStringsLinked.Get(Index: integer): string;
begin
  Result:= fSynStrings.Get(Index);
end;

function TSynEditStringsLinked.GetObject(Index: integer): TObject;
begin
  Result:= fSynStrings.GetObject(Index);
end;

procedure TSynEditStringsLinked.Put(Index: integer; const S: string);
begin
  fSynStrings.Put(Index, S);
end;

procedure TSynEditStringsLinked.PutObject(Index: integer; AObject: TObject);
begin
  fSynStrings.PutObject(Index, AObject);
end;

//function TSynEditStringsLinked.GetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer): TPhysicalCharWidths;
//begin
//  Result := fSynStrings.GetPhysicalCharWidths(Line, LineLen, Index);
//end;

procedure TSynEditStringsLinked.SetUpdateState(Updating: Boolean; Sender: TObject);
begin
  // Update/check "FSenderUpdateCount" in linked lists too (avoid extra locking/unlocking)
  if Updating then
    fSynStrings.BeginUpdate(Sender)
  else
    fSynStrings.EndUpdate(Sender);
end;

procedure TSynEditStringsLinked.DoGetPhysicalCharWidths(Line: PChar;
  LineLen, Index: Integer; PWidths: PPhysicalCharWidth);
begin
  fSynStrings.DoGetPhysicalCharWidths(Line, LineLen, Index, PWidths);
end;

function TSynEditStringsLinked.GetDisplayView: TLazSynDisplayView;
begin
  Result := fSynStrings.GetDisplayView;
end;

procedure TSynEditStringsLinked.EditInsert(LogX, LogY: Integer; AText: String);
begin
  fSynStrings.EditInsert(LogX, LogY, AText);
end;

function TSynEditStringsLinked.EditDelete(LogX, LogY, ByteLen: Integer): String;
begin
  Result := fSynStrings.EditDelete(LogX, LogY, ByteLen);
end;

procedure TSynEditStringsLinked.EditLineBreak(LogX, LogY: Integer);
begin
  fSynStrings.EditLineBreak(LogX, LogY);
end;

procedure TSynEditStringsLinked.EditLineJoin(LogY: Integer;
  FillText: String = '');
begin
  fSynStrings.EditLineJoin(LogY, FillText);
end;

procedure TSynEditStringsLinked.EditLinesInsert(LogY, ACount: Integer; AText: String = '');
begin
  fSynStrings.EditLinesInsert(LogY, ACount, AText);
end;

procedure TSynEditStringsLinked.EditLinesDelete(LogY, ACount: Integer);
begin
  fSynStrings.EditLinesDelete(LogY, ACount);
end;

procedure TSynEditStringsLinked.EditUndo(Item: TSynEditUndoItem);
begin
  fSynStrings.EditUndo(Item);
end;

procedure TSynEditStringsLinked.EditRedo(Item: TSynEditUndoItem);
begin
  fSynStrings.EditRedo(Item);
end;

procedure TSynEditStringsLinked.SendNotification(AReason: TSynEditNotifyReason;
  ASender: TSynEditStrings; aIndex, aCount: Integer;
  aBytePos: Integer = -1; aLen: Integer = 0; aTxt: String = '');
begin
  fSynStrings.SendNotification(AReason, ASender, aIndex, aCount, aBytePos, aLen, aTxt);
end;

procedure TSynEditStringsLinked.SendNotification(AReason: TSynEditNotifyReason;
  ASender: TObject);
begin
  fSynStrings.SendNotification(AReason, ASender);
end;

procedure TSynEditStringsLinked.FlushNotificationCache;
begin
  fSynStrings.FlushNotificationCache;
end;

procedure TSynEditStringsLinked.IgnoreSendNotification(AReason: TSynEditNotifyReason;
  IncIgnore: Boolean);
begin
  fSynStrings.IgnoreSendNotification(AReason, IncIgnore);
end;

function TSynEditStringsLinked.GetIsInEditAction: Boolean;
begin
  Result := fSynStrings.GetIsInEditAction;
end;

procedure TSynEditStringsLinked.IncIsInEditAction;
begin
  fSynStrings.IncIsInEditAction;
end;

procedure TSynEditStringsLinked.DecIsInEditAction;
begin
  fSynStrings.DecIsInEditAction;
end;

end.

