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

{$IFOPT C+}
  {$DEFINE SynAssert}
{$ENDIF}
{$IFDEF SynUndoDebug} {$Define SynUndoDebugItems} {$ENDIF}

interface

uses
  Classes, SysUtils, Graphics, LCLProc, LCLIntf, LCLType,
  SynEditTypes, LazSynEditText, SynEditTextBase, SynEditMiscProcs, SynEditMiscClasses,
  SynEditHighlighter;

type
  TSynEditFlagsClass = class end; // For Register

  TSynEditStringFlag = (
    sfModified,              // a line is modified and not saved after
    sfSaved                  // a line is modified and saved after
  );
  TSynEditStringFlags = set of TSynEditStringFlag;
  PSynEditStringFlags = ^TSynEditStringFlags;

  TStringListIndexEvent = procedure(Index: Integer) of object;

  { TLinesModifiedNotificationList }

  TLinesModifiedNotificationList = Class(TSynMethodList)
  public
    Procedure CallRangeNotifyEvents(Sender: TSynEditStrings; aIndex, aNewCount, aOldCount: Integer);
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

  TSynEditStringList = class;

  { TLazSynDisplayBuffer }

  TLazSynDisplayBuffer = class(TLazSynDisplayViewEx)
  private
    FBuffer: TSynEditStringList;
    FAtLineStart: Boolean;
  public
    constructor Create(ABuffer: TSynEditStringList);
    procedure SetHighlighterTokensLine(ALine: TLineIdx; out ARealLine: TLineIdx); override;
    function  GetNextHighlighterToken(out ATokenInfo: TLazSynDisplayTokenInfo): Boolean; override;
    function GetDrawDividerInfo: TSynDividerDrawConfigSetting; override;
    function GetLinesCount: Integer; override;

    function TextToViewIndex(ATextIndex: TLineIdx): TLineRange; override;
    function ViewToTextIndex(AViewIndex: TLineIdx): TLineIdx; override;
    function ViewToTextIndexEx(AViewIndex: TLineIdx; out AViewRange: TLineRange): TLineIdx; override;
  end;

  { TSynEditStringList }

  TSynEditStringList = class(TSynEditStringListBase)
  private
    FList: TSynEditStringMemory;
    FDisplayView: TLazSynDisplayBuffer;

    FAttachedSynEditList: TFPList;
    FNotifyLists: Array [TSynEditNotifyReason] of TSynMethodList;
    FCachedNotify: Boolean;
    FCachedNotifyStart, FCachedNotifyCount: Integer;
    FCachedNotifySender: TSynEditStrings;
    FModifiedNotifyStart, FModifiedNotifyNewCount, FModifiedNotifyOldCount: Integer;

    FIsInEditAction: Integer;
    FIgnoreSendNotification: array [TSynEditNotifyReason] of Integer;
    fDosFileFormat: boolean;
    fIndexOfLongestLine: integer;
    FRedoList: TSynEditUndoList;
    FUndoList: TSynEditUndoList;
    FIsUndoing, FIsRedoing: Boolean;
    FIsInDecPaintLock: Boolean;

    FIsUtf8: Boolean;
    FModified: Boolean;
    FTextChangeStamp: int64;

    function GetAttachedSynEdits(Index: Integer): TSynEditBase;
    function GetFlags(Index: Integer): TSynEditStringFlags;
    procedure Grow;
    procedure InsertItem(Index: integer; const S: string);
    procedure SetFlags(Index: Integer; const AValue: TSynEditStringFlags);
    procedure SetModified(const AValue: Boolean);
  protected
    function  GetIsUtf8 : Boolean;  override;
    procedure SetIsUtf8(const AValue : Boolean);  override;
    function GetExpandedString(Index: integer): string; override;
    function GetLengthOfLongestLine: integer; override;
    function GetTextChangeStamp: int64; override;

    function GetIsInEditAction: Boolean; override;
    procedure IncIsInEditAction; override;
    procedure DecIsInEditAction; override;
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
    function GetCapacity: integer; override;
    function GetCount: integer; override;
    procedure SetCount(const AValue: Integer);
    function GetObject(Index: integer): TObject; override;
    procedure Put(Index: integer; const S: string); override;
    procedure PutObject(Index: integer; AObject: TObject); override;
    procedure SetCapacity(NewCapacity: integer); override;
    procedure MaybeSendSenrLinesModified; inline;
    procedure SetUpdateState(Updating: Boolean; Sender: TObject); override;

    procedure UndoEditLinesDelete(LogY, ACount: Integer);
    procedure IncreaseTextChangeStamp;
    procedure DoGetPhysicalCharWidths(Line: PChar; LineLen, Index: Integer; PWidths: PPhysicalCharWidth); override;
    function  LogicPosIsCombining(const AChar: PChar): Boolean; inline;

    function GetDisplayView: TLazSynDisplayView; override;

    procedure AddManagedHandler(AReason: TSynEditNotifyReason;
                AHandler: TMethod); override;
    procedure RemoveManagedHandler(AReason: TSynEditNotifyReason;
                AHandler: TMethod); override;
    procedure RemoveManagedHanlders(AOwner: TObject); override;
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
    procedure SendNotification(AReason: TSynEditNotifyReason;
                ASender: TSynEditStrings; aIndex, aCount: Integer); override;
    procedure SendNotification(AReason: TSynEditNotifyReason;
                ASender: TSynEditStrings; aIndex, aCount: Integer;
                aBytePos: Integer; aLen: Integer; aTxt: String); override;
    procedure SendNotification(AReason: TSynEditNotifyReason;
                ASender: TObject); override;
    procedure FlushNotificationCache; override;
    procedure AttachSynEdit(AEdit: TSynEditBase);
    procedure DetachSynEdit(AEdit: TSynEditBase);
    function  AttachedSynEditCount: Integer;
    property  AttachedSynEdits[Index: Integer]: TSynEditBase read GetAttachedSynEdits;
    procedure CopyHanlders(OtherLines: TSynEditStringList; AOwner: TObject = nil);
    procedure SendCachedNotify; // ToDO: review caching versus changes to topline and other values
  public
    property DosFileFormat: boolean read fDosFileFormat write fDosFileFormat;    
    property LengthOfLongestLine: integer read GetLengthOfLongestLine;
    property Flags[Index: Integer]: TSynEditStringFlags read GetFlags
      write SetFlags;
    property Modified: Boolean read FModified write SetModified;
  public
    // Char bounds // 1 based (1 is the 1st char in the line)
    function LogicPosAddChars(const ALine: String; ALogicalPos, ACount: integer;
                              AFlags: LPosFlags = []): Integer; override;
    function LogicPosIsAtChar(const ALine: String; ALogicalPos: integer;
                              AFlags: LPosFlags = []): Boolean; override;
    function LogicPosAdjustToChar(const ALine: String; ALogicalPos: integer;
                                  AFlags: LPosFlags = []): Integer; override;
    property UndoList: TSynEditUndoList read GetUndoList write fUndoList;
    property RedoList: TSynEditUndoList read GetRedoList write fRedoList;
    procedure EditInsert(LogX, LogY: Integer; AText: String); override;
    function  EditDelete(LogX, LogY, ByteLen: Integer): String; override;
    function  EditReplace(LogX, LogY, ByteLen: Integer; AText: String): String; override;
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

const
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

{ TLazSynDisplayBuffer }

constructor TLazSynDisplayBuffer.Create(ABuffer: TSynEditStringList);
begin
  inherited Create;
  FBuffer := ABuffer;
end;

procedure TLazSynDisplayBuffer.SetHighlighterTokensLine(ALine: TLineIdx; out ARealLine: TLineIdx);
begin
  CurrentTokenLine := ALine;
  ARealLine := ALine;
  FAtLineStart := True;
end;

function TLazSynDisplayBuffer.GetNextHighlighterToken(out ATokenInfo: TLazSynDisplayTokenInfo): Boolean;
begin
  Result := False;
  if not Initialized then exit;

  if CurrentTokenHighlighter = nil then begin
    Result := FAtLineStart;
    if not Result then exit;
    ATokenInfo.TokenStart := FBuffer.GetPChar(CurrentTokenLine, ATokenInfo.TokenLength);
    ATokenInfo.TokenAttr := nil;
    FAtLineStart := False;
  end
  else begin
    if FAtLineStart then
      CurrentTokenHighlighter.StartAtLineIndex(CurrentTokenLine);
    FAtLineStart := False;

    Result := not CurrentTokenHighlighter.GetEol;
    if not Result then begin
      ATokenInfo.TokenStart := nil;
      ATokenInfo.TokenLength := 0;
      ATokenInfo.TokenAttr := CurrentTokenHighlighter.GetEndOfLineAttribute;
      Result := ATokenInfo.TokenAttr <> nil;
      exit;
    end;

    CurrentTokenHighlighter.GetTokenEx(ATokenInfo.TokenStart, ATokenInfo.TokenLength);
    ATokenInfo.TokenAttr := CurrentTokenHighlighter.GetTokenAttribute;
    CurrentTokenHighlighter.Next;
  end;
end;

function TLazSynDisplayBuffer.GetDrawDividerInfo: TSynDividerDrawConfigSetting;
begin
  if CurrentTokenHighlighter <> nil then
    Result := CurrentTokenHighlighter.DrawDivider[CurrentTokenLine]
  else
    Result.Color := clNone;
end;

function TLazSynDisplayBuffer.GetLinesCount: Integer;
begin
  Result := FBuffer.Count;
end;

function TLazSynDisplayBuffer.TextToViewIndex(ATextIndex: TLineIdx): TLineRange;
begin
  Result.Top := ATextIndex;
  Result.Bottom := ATextIndex;
end;

function TLazSynDisplayBuffer.ViewToTextIndex(AViewIndex: TLineIdx): TLineIdx;
begin
  Result := AViewIndex;
end;

function TLazSynDisplayBuffer.ViewToTextIndexEx(AViewIndex: TLineIdx; out
  AViewRange: TLineRange): TLineIdx;
begin
  Result := AViewIndex;
  AViewRange.Top := AViewIndex;
  AViewRange.Bottom := AViewIndex;
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
  {$IFDEF SynUndoDebugItems}debugln(['---  Undo Insert ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtInsert.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebugItems}if Result then debugln(['---  Undo Perform ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
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
  {$IFDEF SynUndoDebugItems}debugln(['---  Undo Insert ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtDelete.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebugItems}if Result then debugln(['---  Undo Perform ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
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
  {$IFDEF SynUndoDebugItems}debugln(['---  Undo Insert ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtLineBreak.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebugItems}if Result then debugln(['---  Undo Perform ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
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
  {$IFDEF SynUndoDebugItems}debugln(['---  Undo Insert ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtLineJoin.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebugItems}if Result then debugln(['---  Undo Perform ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
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
  {$IFDEF SynUndoDebugItems}debugln(['---  Undo Insert ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtLinesIns.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebugItems}if Result then debugln(['---  Undo Perform ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
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
  {$IFDEF SynUndoDebugItems}debugln(['---  Undo Insert ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
end;

function TSynEditUndoTxtLinesDel.PerformUndo(Caller: TObject): Boolean;
begin
  Result := Caller is TSynEditStringList;
  {$IFDEF SynUndoDebugItems}if Result then debugln(['---  Undo Perform ',DbgSName(self), ' ', dbgs(Self), ' - ', DebugString]);{$ENDIF}
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
  FDisplayView := TLazSynDisplayBuffer.Create(Self);

  FAttachedSynEditList := TFPList.Create;
  FUndoList := TSynEditUndoList.Create;
  fUndoList.OnAddedUndo := @UndoRedoAdded;
  FRedoList := TSynEditUndoList.Create;
  fRedoList.OnAddedUndo := @UndoRedoAdded;
  FIsUndoing := False;
  FIsRedoing := False;
  FModified := False;
  FIsInEditAction := 0;

  for r := low(TSynEditNotifyReason) to high(TSynEditNotifyReason)
  do case r of
    senrLineCount, senrLineChange, senrHighlightChanged:
      FNotifyLists[r] := TLineRangeNotificationList.Create;
    senrLinesModified:
      FNotifyLists[r] := TLinesModifiedNotificationList.Create;
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

  FreeAndNil(FDisplayView);
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

procedure TSynEditStringList.DeleteLines(Index, NumLines: integer);
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

function TSynEditStringList.GetIsInEditAction: Boolean;
begin
  Result := FIsInEditAction > 0;
end;

procedure TSynEditStringList.IncIsInEditAction;
begin
  inc(FIsInEditAction);
end;

procedure TSynEditStringList.DecIsInEditAction;
begin
  dec(FIsInEditAction);
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
  if FIsUndoing = AValue then
    exit;

  if not AValue then
    SendNotification(senrEndUndoRedo, Self); // before UNDO ends

  FIsUndoing := AValue;

  if AValue then
    SendNotification(senrBeginUndoRedo, Self); // after UNDO started
end;

function TSynEditStringList.GetIsUndoing: Boolean;
begin
  Result := FIsUndoing;
end;

procedure TSynEditStringList.SetIsRedoing(const AValue: Boolean);
begin
  if FIsRedoing = AValue then
    exit;

  if not AValue then
    SendNotification(senrEndUndoRedo, Self); // before UNDO ends

  FIsRedoing := AValue;

  if AValue then
    SendNotification(senrBeginUndoRedo, Self); // after UNDO started
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
  i: Integer;
begin
  if not IsUtf8 then begin
    for i := 0 to LineLen-1 do
      PWidths[i] := 1;
    exit;
  end;

  for i := 0 to LineLen-1 do begin
    case Line^ of
      #$00..#$7F:
        PWidths^ := 1;
      #$80..#$BF:
        PWidths^ := 0;
      else
        if LogicPosIsCombining(Line) then
          PWidths^ := 0
        else
          PWidths^ := 1;
      //#$CC:
      //  if ((Line+1)^ in [#$80..#$FF]) and (i>0)
      //  then PWidths^ := 0  // Combining Diacritical Marks (belongs to previos char) 0300-036F
      //  else PWidths^ := 1;
      //#$CD:
      //  if ((Line+1)^ in [#$00..#$AF]) and (i>0)
      //  then PWidths^ := 0  // Combining Diacritical Marks
      //  else PWidths^ := 1;
      //#$E1:
      //  if (((Line+1)^ = #$B7) and ((Line+2)^ in [#$80..#$BF])) and (i>0)
      //  then PWidths^ := 0  // Combining Diacritical Marks Supplement 1DC0-1DFF
      //  else PWidths^ := 1;
      //#$E2:
      //  if (((Line+1)^ = #$83) and ((Line+2)^ in [#$90..#$FF])) and (i>0)
      //  then PWidths^ := 0  // Combining Diacritical Marks for Symbols 20D0-20FF
      //  else PWidths^ := 1;
      //#$EF:
      //  if (((Line+1)^ = #$B8) and ((Line+2)^ in [#$A0..#$AF])) and (i>0)
      //  then PWidths^ := 0  // Combining half Marks FE20-FE2F
      //  else PWidths^ := 1;
      //else
      //  PWidths^ := 1;
    end;
    inc(PWidths);
    inc(Line);
  end;

end;

function TSynEditStringList.LogicPosIsCombining(const AChar: PChar): Boolean;
begin
  Result := (
   ( (AChar[0] = #$CC) ) or                                                       // Combining Diacritical Marks (belongs to previos char) 0300-036F
   ( (AChar[0] = #$CD) and (AChar[1] in [#$80..#$AF]) ) or                        // Combining Diacritical Marks
   ( (AChar[0] = #$D8) and (AChar[1] in [#$90..#$9A]) ) or                        // Arabic 0610 (d890)..061A (d89a)
   ( (AChar[0] = #$D9) and (AChar[1] in [#$8b..#$9f, #$B0]) ) or                  // Arabic 064B (d98b)..065F (d99f) // 0670 (d9b0)
   ( (AChar[0] = #$DB) and (AChar[1] in [#$96..#$9C, #$9F..#$A4, #$A7..#$A8, #$AA..#$AD]) ) or // Arabic 06D6 (db96)..  .. ..06EA (dbaa)
   ( (AChar[0] = #$E0) and (AChar[1] = #$A3) and (AChar[2] in [#$A4..#$BE]) ) or  // Arabic 08E4 (e0a3a4) ..08FE (e0a3be)
   ( (AChar[0] = #$E1) and (AChar[1] = #$B7) ) or                                 // Combining Diacritical Marks Supplement 1DC0-1DFF
   ( (AChar[0] = #$E2) and (AChar[1] = #$83) and (AChar[2] in [#$90..#$FF]) ) or  // Combining Diacritical Marks for Symbols 20D0-20FF
   ( (AChar[0] = #$EF) and (AChar[1] = #$B8) and (AChar[2] in [#$A0..#$AF]) )     // Combining half Marks FE20-FE2F
  );
end;

function TSynEditStringList.GetDisplayView: TLazSynDisplayView;
begin
  Result := FDisplayView;
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

function TSynEditStringList.GetIsUtf8: Boolean;
begin
  Result := FIsUtf8;
end;

procedure TSynEditStringList.SetIsUtf8(const AValue: Boolean);
begin
  FIsUtf8 := AValue;
end;

procedure TSynEditStringList.SendCachedNotify;
begin
//debugln(['--- send cached notify  ', FCachedNotifyStart,' / ',FCachedNotifyCount]);
  if (FCachedNotifyCount <> 0) and FCachedNotify then begin
    FCachedNotify := False;
    TLineRangeNotificationList(FNotifyLists[senrLineCount])
      .CallRangeNotifyEvents(FCachedNotifySender, FCachedNotifyStart, FCachedNotifyCount);
  end;
end;

function TSynEditStringList.LogicPosAddChars(const ALine: String; ALogicalPos,
  ACount: integer; AFlags: LPosFlags): Integer;
var
  l: Integer;
begin
  // UTF8 handing of chars
  Result := ALogicalPos;
  l := length(ALine);
  if ACount > 0 then begin
    while (Result < l) and (ACount > 0) do begin
      inc(Result);
      if (ALine[Result] in [#0..#127, #192..#255]) and
         ( (lpStopAtCodePoint in AFlags) or (not LogicPosIsCombining(@ALine[Result])) )
      then
        dec(ACount);
    end;
    if lpAllowPastEOL in AFlags then
      Result := Result + ACount;

    if (Result <= l) then
      while (Result > 1) and
            ( (not(ALine[Result] in [#0..#127, #192..#255])) or
              ( (not(lpStopAtCodePoint in AFlags)) and LogicPosIsCombining(@ALine[Result]) )
            )
      do
        dec(Result);
  end else begin
    while (Result > 1) and (ACount < 0) do begin
      dec(Result);
      if (Result > l) or (Result = 1) or
         ( (ALine[Result] in [#0..#127, #192..#255]) and
           ( (lpStopAtCodePoint in AFlags) or (not LogicPosIsCombining(@ALine[Result])) )
         )
      then
        inc(ACount);
    end;
  end;
end;

function TSynEditStringList.LogicPosIsAtChar(const ALine: String; ALogicalPos: integer;
  AFlags: LPosFlags): Boolean;
begin
  // UTF8 handing of chars
  Result := (lpAllowPastEol in AFlags) and (ALogicalPos >= 1);
  if (ALogicalPos < 1) or (ALogicalPos > length(ALine)) then exit;
  Result := ALine[ALogicalPos] in [#0..#127, #192..#255];

  if Result then
    Result := (ALogicalPos = 1) or
              (lpStopAtCodePoint in AFlags) or
              (not LogicPosIsCombining(@ALine[ALogicalPos]));
end;

function TSynEditStringList.LogicPosAdjustToChar(const ALine: String; ALogicalPos: integer;
  AFlags: LPosFlags): Integer;
begin
  // UTF8 handing of chars
  Result := ALogicalPos;
  if (ALogicalPos < 1) or (ALogicalPos > length(ALine)) then exit;

  if lpAdjustToNext in AFlags then begin
    while (Result <= length(ALine)) and
      ( (not(ALine[Result] in [#0..#127, #192..#255])) or
        ((Result <> 1) and
         (not(lpStopAtCodePoint in AFlags)) and LogicPosIsCombining(@ALine[Result])
        )
      )
    do
      inc(Result);
  end;

  if (not (lpAllowPastEol in AFlags)) and (Result > length(ALine)) then
    Result := length(ALine); // + 1
  if (Result > length(ALine)) then exit;

  while (Result > 1) and
    ( (not(ALine[Result] in [#0..#127, #192..#255])) or
      ( (not(lpStopAtCodePoint in AFlags)) and LogicPosIsCombining(@ALine[Result]) )
    )
  do
    dec(Result);
end;

procedure TSynEditStringList.MarkModified(AFirst, ALast: Integer);
var
  Index: Integer;
begin
  for Index := AFirst - 1 to ALast - 1 do
    if (Index >= 0) or (Index < Count) then
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

procedure TSynEditStringList.AddManagedHandler(AReason: TSynEditNotifyReason;
  AHandler: TMethod);
begin
  FNotifyLists[AReason].Add(AHandler);
end;

procedure TSynEditStringList.RemoveManagedHandler(AReason: TSynEditNotifyReason;
  AHandler: TMethod);
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

procedure TSynEditStringList.RemoveManagedHanlders(AOwner: TObject);
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

procedure TSynEditStringList.MaybeSendSenrLinesModified;
begin
    assert( (FModifiedNotifyOldCount >= 0) and (FModifiedNotifyNewCount >= 0), 'FModifiedNotify___Count >= 0');
    if (FModifiedNotifyOldCount > 0) or (FModifiedNotifyNewCount > 0) then
      TLinesModifiedNotificationList(FNotifyLists[senrLinesModified])
        .CallRangeNotifyEvents(Self, FModifiedNotifyStart, FModifiedNotifyNewCount, FModifiedNotifyOldCount);
end;

procedure TSynEditStringList.SetUpdateState(Updating: Boolean; Sender: TObject);
begin
  if FIsInDecPaintLock then exit;
  if Updating then begin
    SendNotification(senrBeforeIncPaintLock, Sender);
    SendNotification(senrIncPaintLock, Sender);       // DoIncPaintLock
    SendNotification(senrAfterIncPaintLock, Sender);
    FCachedNotify := False;
    FModifiedNotifyStart := -1;
    FModifiedNotifyOldCount := 0;
    FModifiedNotifyNewCount := 0;
  end else begin
    if FCachedNotify then
      SendCachedNotify;
    MaybeSendSenrLinesModified; // must be before senrDecPaintLock is sent
    FIsInDecPaintLock := True;
    try
      SendNotification(senrBeforeDecPaintLock, Sender);
      SendNotification(senrDecPaintLock, Sender);       // DoDecPaintLock
      SendNotification(senrAfterDecPaintLock, Sender);
    finally
      FIsInDecPaintLock := False;
    end;
  end;
end;

procedure TSynEditStringList.EditInsert(LogX, LogY: Integer; AText: String);
var
  s: string;
begin
  IncIsInEditAction;
  s := Strings[LogY - 1];
  if LogX - 1 > Length(s) then begin
    AText := StringOfChar(' ', LogX - 1 - Length(s)) + AText;
    LogX := Length(s) + 1;
  end;
  Strings[LogY - 1] := copy(s,1, LogX - 1) + AText + copy(s, LogX, length(s));
  if AText <> '' then
    CurUndoList.AddChange(TSynEditUndoTxtInsert.Create(LogX, LogY, Length(AText)));
  MarkModified(LogY, LogY);
  SendNotification(senrEditAction, self, LogY, 0, LogX, length(AText), AText);
  DecIsInEditAction;
end;

function TSynEditStringList.EditDelete(LogX, LogY, ByteLen: Integer): String;
var
  s: string;
begin
  Result := '';
  if ByteLen <= 0 then
    exit;
  IncIsInEditAction;
  s := Strings[LogY - 1];
  if LogX - 1 <= Length(s) then begin
    Result := copy(s, LogX, ByteLen);
    Strings[LogY - 1] := copy(s,1, LogX - 1) + copy(s, LogX +  ByteLen, length(s));
    if Result <> '' then
      CurUndoList.AddChange(TSynEditUndoTxtDelete.Create(LogX, LogY, Result));
    MarkModified(LogY, LogY);
  end;
  SendNotification(senrEditAction, self, LogY, 0, LogX, -ByteLen, '');
  DecIsInEditAction;
end;

function TSynEditStringList.EditReplace(LogX, LogY, ByteLen: Integer; AText: String): String;
var
  s, s2: string;
begin
  IncIsInEditAction;

  if ByteLen <= 0 then
    ByteLen := 0;
  s := Strings[LogY - 1];
  if LogX - 1 > Length(s) then begin
    AText := StringOfChar(' ', LogX - 1 - Length(s)) + AText;
    LogX := Length(s) + 1;
  end;

  if LogX - 1 + ByteLen > Length(s) then
    ByteLen := Length(s) - (LogX-1);
  Result := copy(s, LogX, ByteLen);

  SetLength(s2, Length(s) - ByteLen + Length(AText));
  if LogX > 1 then
    system.Move(s[1], s2[1], LogX-1);
  if AText <> '' then
    system.Move(AText[1], s2[LogX], Length(AText));
  if Length(s)-(LogX-1)-ByteLen > 0 then
    system.Move(s[LogX+ByteLen], s2[LogX+Length(AText)], Length(s)-(LogX-1)-ByteLen);
  Strings[LogY - 1] := s2;
  //Strings[LogY - 1] := copy(s,1, LogX - 1) + AText + copy(s, LogX +  ByteLen, length(s));

  if Result <> '' then
    CurUndoList.AddChange(TSynEditUndoTxtDelete.Create(LogX, LogY, Result));
  if AText <> '' then
    CurUndoList.AddChange(TSynEditUndoTxtInsert.Create(LogX, LogY, Length(AText)));

  MarkModified(LogY, LogY);
  SendNotification(senrEditAction, self, LogY, 0, LogX, -ByteLen, '');
  SendNotification(senrEditAction, self, LogY, 0, LogX, length(AText), AText);
  DecIsInEditAction;
end;

procedure TSynEditStringList.EditLineBreak(LogX, LogY: Integer);
var
  s: string;
begin
  IncIsInEditAction;
  if Count = 0 then Add('');
  s := Strings[LogY - 1];
  if LogX - 1 < length(s) then
    Strings[LogY - 1] := copy(s, 1, LogX - 1);
  Insert(LogY, copy(s, LogX, length(s)));
  CurUndoList.AddChange(TSynEditUndoTxtLineBreak.Create(LogY));
  MarkModified(LogY, LogY + 1);
  SendNotification(senrEditAction, self, LogY, 1, LogX, 0, '');
  DecIsInEditAction;
end;

procedure TSynEditStringList.EditLineJoin(LogY: Integer; FillText: String = '');
var
  t: string;
begin
  IncIsInEditAction;
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
  DecIsInEditAction;
end;

procedure TSynEditStringList.EditLinesInsert(LogY, ACount: Integer;
  AText: String = '');
begin
  IncIsInEditAction;
  InsertLines(LogY - 1, ACount);
  CurUndoList.AddChange(TSynEditUndoTxtLinesIns.Create(LogY, ACount));
  SendNotification(senrEditAction, self, LogY, ACount, 1, 0, '');
  if AText <> '' then
    EditInsert(1, LogY, AText);
  MarkModified(LogY, LogY + ACount - 1);
  DecIsInEditAction;
end;

procedure TSynEditStringList.EditLinesDelete(LogY, ACount: Integer);
var
  i: Integer;
begin
  IncIsInEditAction;
  for i := LogY to LogY + ACount - 1 do
    EditDelete(1, i, length(Strings[i-1]));
  DeleteLines(LogY - 1, ACount);
  CurUndoList.AddChange(TSynEditUndoTxtLinesDel.Create(LogY, ACount));
  SendNotification(senrEditAction, self, LogY, -ACount, 1, 0, '');
  DecIsInEditAction;
end;

procedure TSynEditStringList.EditUndo(Item: TSynEditUndoItem);
begin
  IncIsInEditAction; // all undo calls edit actions
  EditRedo(Item);
  DecIsInEditAction;
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
  IncIsInEditAction; // all undo calls edit actions
  Item.PerformUndo(self);
  DecIsInEditAction;
end;

procedure TSynEditStringList.SendNotification(AReason: TSynEditNotifyReason;
  ASender: TSynEditStrings; aIndex, aCount: Integer);
var
  i, oldcount, overlap: Integer;
begin
  assert(AReason in [senrLineChange, senrLineCount, senrLinesModified, senrHighlightChanged], 'Correct SendNotification');
  if FIgnoreSendNotification[AReason] > 0 then exit;

  if IsUpdating and (AReason in [senrLineChange, senrLineCount]) then begin
    // senrLinesModified
    assert( (FModifiedNotifyOldCount >= 0) and (FModifiedNotifyNewCount >= 0), 'FModifiedNotify___Count >= 0');
    assert(aIndex >= 0, 'SendNotification index');
    if (FModifiedNotifyOldCount = 0) and (FModifiedNotifyNewCount = 0) then
      FModifiedNotifyStart := aIndex;

    if aIndex < FModifiedNotifyStart then begin
      i := FModifiedNotifyStart - aIndex;
      FModifiedNotifyStart := aIndex;
      FModifiedNotifyNewCount := FModifiedNotifyNewCount + i;
      FModifiedNotifyOldCount := FModifiedNotifyOldCount + i;
    end;

    oldcount := 0;
    if AReason = senrLineCount then begin
      if aCount < 0 then begin
        oldcount := -aCount;
        if (aIndex < FModifiedNotifyStart + FModifiedNotifyNewCount) then begin
          overlap := (FModifiedNotifyStart + FModifiedNotifyNewCount) - aIndex;
          if overlap > oldcount then overlap := oldcount;
          FModifiedNotifyNewCount := FModifiedNotifyNewCount - overlap;
          oldcount := oldcount - overlap;
        end;
        FModifiedNotifyOldCount := FModifiedNotifyOldCount + oldcount;
        oldcount := 0;
      end
      else begin
        FModifiedNotifyNewCount := FModifiedNotifyNewCount + aCount;
        oldcount := aCount; // because already added to newcount
      end;
    end
    else
    if AReason = senrLineChange then begin
      oldcount := aCount;
    end;

    if aIndex + oldcount > FModifiedNotifyStart + FModifiedNotifyNewCount then begin
      i := (aIndex + oldcount) - (FModifiedNotifyStart + FModifiedNotifyNewCount);
      FModifiedNotifyNewCount := FModifiedNotifyNewCount + i;
      FModifiedNotifyOldCount := FModifiedNotifyOldCount + i;
    end;

    // CacheNotify
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
    end
    else
    if FCachedNotify and (AReason = senrLineChange) and
       (ASender = FCachedNotifySender) and (FCachedNotifyCount > 0) and
       (aIndex >= FCachedNotifyStart) and
       (aIndex + aCount {- 1} <= FCachedNotifyStart + FCachedNotifyCount {- 1})
    then
      exit; // Will send senrLineCount instead

    if FCachedNotify then
      SendCachedNotify;
  end
  else begin
    case AReason of
      senrLineChange:
        TLinesModifiedNotificationList(FNotifyLists[senrLinesModified])
          .CallRangeNotifyEvents(ASender, aIndex, aCount, aCount);
      senrLineCount:
        TLinesModifiedNotificationList(FNotifyLists[senrLinesModified])
          .CallRangeNotifyEvents(ASender, aIndex, aCount, 0);
    end;
  end;

  TLineRangeNotificationList(FNotifyLists[AReason])
    .CallRangeNotifyEvents(ASender, aIndex, aCount);
end;

procedure TSynEditStringList.SendNotification(AReason: TSynEditNotifyReason;
  ASender: TSynEditStrings; aIndex, aCount: Integer;
  aBytePos: Integer; aLen: Integer; aTxt: String);
begin
  assert(AReason in [senrEditAction], 'Correct SendNotification');
  if FIgnoreSendNotification[AReason] > 0 then exit;

  if FCachedNotify then
    SendCachedNotify;

  // aindex is mis-named (linepos) for edit action
  TLineEditNotificationList(FNotifyLists[AReason])
    .CallRangeNotifyEvents(ASender, aIndex, aBytePos, aLen, aCount, aTxt);
end;

procedure TSynEditStringList.SendNotification(AReason: TSynEditNotifyReason;
  ASender: TObject);
begin
  if AReason in [senrLineChange, senrLineCount, senrLinesModified, senrHighlightChanged, senrEditAction] then
    raise Exception.Create('Invalid');
  if FCachedNotify then
    SendCachedNotify;
  if (AReason in [senrCleared, senrTextBufferChanging]) then
    MaybeSendSenrLinesModified;
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

{ TLinesModifiedNotificationList }

procedure TLinesModifiedNotificationList.CallRangeNotifyEvents(Sender: TSynEditStrings; aIndex,
  aNewCount, aOldCount: Integer);
var
  i: LongInt;
begin
  i:=Count;
  while NextDownIndex(i) do
    TStringListLinesModifiedEvent(Items[i])(Sender, aIndex, aNewCount, aOldCount);
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

