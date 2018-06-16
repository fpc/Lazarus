unit SynPluginMultiCaret;

{$mode objfpc}{$H+}

{$DEFINE SynMultiCaretAssert}
{off $DEFINE SynMultiCaretDebug}

{$IfDef SynMultiCaretAssert}
  {$ASSERTIONS on}
{$ENDIF}
{ $INLINE off}
interface

uses
  Classes, SysUtils, SynEdit, SynEditPointClasses, SynEditKeyCmds, SynEditTypes,
  LazSynTextArea, SynEditMiscProcs, LazSynEditText, SynEditMiscClasses, SynEditMouseCmds,
  SynEditStrConst, SynEditTextTrimmer, SynEditTextBase,
  {$IfDef SynMultiCaretDebug} LazLoggerBase, {$ELSE} LazLoggerDummy, {$ENDIF}
  LCLType, Controls, Graphics, Clipbrd;

const

  emcPluginMultiCaretToggleCaret = emcPluginFirstMultiCaret + 0;
  emcPluginMultiCaretSelectionToCarets = emcPluginFirstMultiCaret + 1;

  ecPluginMultiCaretSetCaret    = ecPluginFirstMultiCaret + 0;
  ecPluginMultiCaretUnsetCaret  = ecPluginFirstMultiCaret + 1;
  ecPluginMultiCaretToggleCaret = ecPluginFirstMultiCaret + 2;
  ecPluginMultiCaretClearAll    = ecPluginFirstMultiCaret + 3;

  ecPluginMultiCaretModeCancelOnMove  = ecPluginFirstMultiCaret + 4;
  ecPluginMultiCaretModeMoveAll       = ecPluginFirstMultiCaret + 5;

  // last
  ecPluginLastMultiCaret = ecPluginFirstMultiCaret + 5;

const
  EMPTY_LIST_LEN = 8;

type

   TSynMultiCaretCommandAction = (
     ccaDefaultAction,       // build in default, if any
     ccaNoneRepeatCommand,   // Run Command (onc), clear carets IF any changes (text,selection,main-caret)
     ccaRepeatCommand,       // Repeat the command for each caret
     ccaRepeatCommandPerLine, // Repeat the command for the first caret on each line
     ccaClearCarets,         // Always Clear all carets
     ccaAdjustCarets        // Run the command once (for main-caret), keep and adjust all carets
   );
   TSynMultiCaretCommandFlag = ( // for extension
     ccfDummy // do not use
   );
   TSynMultiCaretCommandFlags = set of TSynMultiCaretCommandFlag;

   TSynMultiCaretBeforeCommand = procedure(Sender: TObject;
     ACommand: TSynEditorCommand;
     var AnAction: TSynMultiCaretCommandAction;
     var AFlags: TSynMultiCaretCommandFlags) of object;

  TLogCaretPointArray = Array of TLogCaretPoint;
  TSynPluginMultiCaretVisualList = class;

  { TSynPluginMultiCaretVisual }

  TSynPluginMultiCaretVisual = class(TSynEditScreenCaret)
  private
    FListIndex: Integer;
    FUsedList: TSynPluginMultiCaretVisualList;
    FUnUsedList: TSynPluginMultiCaretVisualList;
    {$IfDef SynMultiCaretAssert}
    FIsUsed: Boolean;
    {$ENDIF}
  public
    constructor Create(AHandleOwner: TWinControl;
      APainterClass: TSynEditScreenCaretPainterClass;
      AnUsedList, AnUnUsedList: TSynPluginMultiCaretVisualList);
    procedure MoveToUsed;
    procedure MoveToUnUsed;
    property ListIndex: Integer read FListIndex;
    property UsedList: TSynPluginMultiCaretVisualList read FUsedList;
    property UnUsedList: TSynPluginMultiCaretVisualList read FUnUsedList;
  end;

  { TSynPluginMultiCaretVisualList }

  TSynPluginMultiCaretVisualList = class
  private
    FList: Array of TSynPluginMultiCaretVisual;
    FCount: Integer;
    function GetScreenCaret(Index: Integer): TSynPluginMultiCaretVisual;
  public
    destructor Destroy; override;
    procedure Add(AScreenCaret: TSynPluginMultiCaretVisual);
    procedure Remove(AScreenCaret: TSynPluginMultiCaretVisual);
    procedure Clear; // free visuals
    function  Count: Integer;
    property  ScreenCaret[Index: Integer]: TSynPluginMultiCaretVisual read GetScreenCaret; default;
  end;

  TCaretFlag = (cfMainCaret, cfNoneVisual, cfAddDuplicate, cfIterationDone);
  TCaretFlags = set of TCaretFlag;

  { TSynPluginMultiCaretList }

  TSynPluginMultiCaretList = class
  private type
    //TCaretFlag = (cfMainCaret, cfNoneVisual);
    //TCaretFlags = set of TCaretFlag;
    TCaretData = record
      x, y, offs: Integer; // logical
      KeepX: Integer;
      Flags: TCaretFlags;
      Visual: TSynPluginMultiCaretVisual;
    end;
    PCaretData = ^TCaretData;
  private
    FLowIndex, FHighIndex: Integer;
    FMainCaretIndex: Integer;
    FMergeLock: Integer;
    FCarets: Array of TCaretData;
    function FindEqOrNextCaretRawIdx(X, Y, Offs: Integer; LowIdx: integer = -1; HighIdx: integer = -1): Integer;
    function GetCaret(Index: Integer): TPoint; inline;
    function GetCaretFull(Index: Integer): TLogCaretPoint; inline;
    function GetCaretKeepX(Index: Integer): Integer; inline;
    function GetCaretOffs(Index: Integer): Integer; inline;
    function GetCaretX(Index: Integer): Integer; inline;
    function GetCaretY(Index: Integer): Integer; inline;
    function GetFlags(Index: Integer): TCaretFlags;
    function GetMainCaretIndex: Integer;
    function GetVisual(Index: Integer): TSynPluginMultiCaretVisual; inline;
    procedure SetCaret(Index: Integer; AValue: TPoint); inline;
    procedure SetCaretFull(Index: Integer; AValue: TLogCaretPoint); inline;
    procedure SetCaretKeepX(Index: Integer; AValue: Integer); inline;
    procedure SetCaretOffs(Index: Integer; AValue: Integer); inline;
    procedure SetCaretX(Index: Integer; AValue: Integer); inline;
    procedure SetCaretY(Index: Integer; AValue: Integer); inline;
    procedure SetVisual(Index: Integer; AValue: TSynPluginMultiCaretVisual); inline;

    function  InternalRemoveCaretEx(RawIndex: Integer; AlternativeRawIndex: Integer = -1): Integer;
    function  InternalRemoveCaret(RawIndex: Integer): integer;
    procedure AdjustAfterChange(RawIndex: Integer); inline;
  public
    constructor Create;
    function  AddCaret(X, Y, Offs: Integer; flags: TCaretFlags = []; PhysX: Integer = -1): Integer;
    procedure RemoveCaret(Index: Integer);
    procedure Clear(AFreeVisual: Boolean = False; ACapacity: Integer = EMPTY_LIST_LEN);
    function  Count: Integer;
    function  Capacity: Integer;
    procedure ImportFromSortedList(AMultiCaretList: TLogCaretPointArray);
    function  FindCaretIdx(X, Y, Offs: Integer): Integer;
    function  FindEqOrNextCaretIdx(X, Y, Offs: Integer; LowIdx: integer = -1; HighIdx: integer = -1): Integer;
    procedure AdjustAllAfterEdit(aLinePos, aBytePos, aCount, aLineBrkCnt: Integer);
    procedure FindAndRemoveMergedCarets;
    procedure IncMergeLock;
    procedure DecMergeLock;

    property Caret[Index: Integer]: TPoint read GetCaret write SetCaret;
    property CaretFull[Index: Integer]: TLogCaretPoint read GetCaretFull write SetCaretFull;
    property CaretX[Index: Integer]: Integer read GetCaretX write SetCaretX;
    property CaretOffs[Index: Integer]: Integer read GetCaretOffs write SetCaretOffs;
    property CaretKeepX[Index: Integer]: Integer read GetCaretKeepX write SetCaretKeepX;
    property CaretY[Index: Integer]: Integer read GetCaretY write SetCaretY;
    property Visual[Index: Integer]: TSynPluginMultiCaretVisual read GetVisual write SetVisual;
    property Flags[Index: Integer]: TCaretFlags read GetFlags;
    property MainCaretIndex: Integer read GetMainCaretIndex;

  private
    FCurrenCaret, FBeforeNextCaret: PCaretData;
    FIterationDoneCount: Integer;
    FLowCaret, FHighCaret: PCaretData;  // used in AdjustAfterChange
    FIteratoreMode: (mciNone, mciUp, mciDown);
    function GetCurrentCaretFlags: TCaretFlags; inline;
    function GetCurrentCaretFull: TLogCaretPoint; inline;
    function GetCurrentCaretKeepX: Integer; inline;
    procedure SetCurrentCaretFull(AValue: TLogCaretPoint); inline;
    procedure SetCurrentCaretKeepX(AValue: Integer); inline;

    procedure AdjustAfterChange(ACaret: PCaretData);
  public
    // During iteration no calls to add/remove are allowed
    procedure StartIteratorAtFirst; // valid after first call to IterateNextUp
    function  IterateNextUp: Boolean; inline;
    procedure StartIteratorAtLast;
    function  IterateNextDown: Boolean; inline;
    function  CanPeekCaret(AIndexOffset: Integer): Boolean; inline;
    function  PeekCaretY(AIndexOffset: Integer): Integer; inline;
    function  PeekCaretFull(AIndexOffset: Integer): TLogCaretPoint; inline;
    //procedure AbortIterator;

    property CurrentCaretFull: TLogCaretPoint read GetCurrentCaretFull write SetCurrentCaretFull;
    property CurrentCaretKeepX: Integer read GetCurrentCaretKeepX write SetCurrentCaretKeepX;
    property CurrentCaretFlags: TCaretFlags read GetCurrentCaretFlags;
    //property CurrentCaret: TPoint read GetCurrentCaret  write SetCurrentCaret;
    //property CurrentCaretX: Integer read GetCurrentCaretX write SetCurrentCaretX;
    //property CurrentCaretOffs: Integer read GetCurrentCaretOffs write SetCurrentCaretOffs;
    //property CurrentCaretY: Integer read GetCurrentCaretY write SetCurrentCaretY;
  end;

  { TSynPluginMultiCaretBase }

  TSynPluginMultiCaretBase = class(TLazSynEditPlugin)
  private
    FCarets: TSynPluginMultiCaretList;
    FColor: TColor;
    FRestoreSingleCaretPainter: Boolean;
    FSingleCaretClass: TSynEditScreenCaretPainterClass;
    FUsedList: TSynPluginMultiCaretVisualList;
    FUnUsedList: TSynPluginMultiCaretVisualList;
    FInPaint: Boolean;
    FPaintClip: TRect;

    FCustomPixelWidth, FCustomPixelHeight: Array [TSynCaretType] of Integer;
    FCustomOffsetX, FCustomOffsetY: Array [TSynCaretType] of Integer;
    FCustomFlags: Array [TSynCaretType] of TSynCustomCaretSizeFlags;

    FPaintLock: Integer;
    FPaintLockFlags: set of
      (plfUpdateCaretsPos, plfDeferUpdateCaretsPos, plfMergeCarets,
       plfBoundsChanged, plfTextSizeChanged);

    function  GetTextArea: TLazSynTextArea;
    procedure DoTextSizeChanged(Sender: TObject);
    procedure DoBoundsChanged(Sender: TObject);
    procedure MergeAndRemoveCarets(AForce: Boolean = False);
    function  IsCaretMergeRequested: Boolean;
    procedure DoEditorPaintEvent(Sender: TObject; EventType: TSynPaintEvent;
      const prcClip: TRect);
    procedure DoEditorScrollEvent(Sender: TObject; EventType: TSynScrollEvent; dx,
      dy: Integer; const prcScroll, prcClip: TRect);
    procedure DoEditorStatusChanged(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoAfterDecPaintLock(Sender: TObject); virtual;
    procedure DoBeforeIncPaintLock(Sender: TObject); virtual;
    procedure DoBufferChanged(Sender: TObject); virtual;
    procedure SetColor(AValue: TColor);
    property TextArea: TLazSynTextArea read GetTextArea;
    function CreateVisual: TSynPluginMultiCaretVisual; virtual;
    function GetVisual: TSynPluginMultiCaretVisual;
  protected
    procedure UpdateMainCaret;
    function  AddCaret(X, Y, Offs: Integer; flags: TCaretFlags = []; PhysX: Integer = -1): Integer;
    procedure RemoveCaret(Index: Integer);
    procedure UpdateCaretsPos;
    procedure ClearCarets;
    function  CaretsCount: Integer;
    procedure DoCleared; virtual;

    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
                            aLineBrkCnt: Integer; aText: String); virtual;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;

    property Carets: TSynPluginMultiCaretList read FCarets;
    property PaintLock: Integer read FPaintLock;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetCaretTypeSize(AType: TSynCaretType; AWidth, AHeight, AXOffs, AYOffs: Integer; AFlags: TSynCustomCaretSizeFlags);
    property Color: TColor read FColor write SetColor;
    property RestoreSingleCaretPainter: Boolean read FRestoreSingleCaretPainter write FRestoreSingleCaretPainter;
  end;

  { TSynPluginMultiCaretMouseActions }

  TSynPluginMultiCaretMouseActions = class(TSynEditMouseActions)
  public
    procedure ResetDefaults; override;
  end;

  { TSynPluginMultiCaretKeyStrokes }

  TSynPluginMultiCaretKeyStrokes = class(TSynEditKeyStrokes)
  public
    procedure ResetDefaults; override;
  end;

  TSynPluginMultiCaretMode = (
    mcmCancelOnCaretMove,
    mcmMoveAllCarets,
    // Only for ActiveMode
    mcmNoCarets,
    mcmAddingCarets // move main caret, keep others
  );
  TSynPluginMultiCaretDefaultMode = mcmCancelOnCaretMove..mcmMoveAllCarets;

  TSynPluginMultiCaretStateFlag = (
    sfProcessingCmd, sfProcessingMain, sfProcessingRepeat,
    sfNoChangeIndicator,
    sfExtendingColumnSel, sfSkipCaretsAtSelection,
    sfCreateCaretAtCurrentPos,
    sfSkipSelChanged, sfSkipCaretChanged,
    sfSkipUndoCarets
  );
  TSynPluginMultiCaretStateFlags = set of TSynPluginMultiCaretStateFlag;

  TSynMultiCaretOption = (
    smcoDeleteSkipLineBreak      // ecDeleteChar will not join lines
  );
  TSynMultiCaretOptions = set of TSynMultiCaretOption;

  { TSynEditUndoMultiCaret }

  TSynEditUndoMultiCaret = class(TSynEditUndoItem)
  private
    FCaretUndoItem: TSynEditUndoItem;
    FBeginBlock: Boolean;
    FActiveMode: TSynPluginMultiCaretMode;
    FMultiCaretList: TLogCaretPointArray;
  protected
    function IsEqualContent(AnItem: TSynEditUndoItem): Boolean; override;
    function DebugString: String; override;
  public
    constructor Create(ACaretUndoItem: TSynEditUndoItem; ABeginBlock: Boolean);
    destructor Destroy; override;
    constructor AddCaretsFrom(AList: TSynPluginMultiCaretList);
    function IsCaretInfo: Boolean; override;
    function PerformUndo(Caller: TObject): Boolean; override;
    property ActiveMode: TSynPluginMultiCaretMode read FActiveMode write FActiveMode;
  end;

  { TSynCustomPluginMultiCaret }

  TSynCustomPluginMultiCaret = class(TSynPluginMultiCaretBase)
  private
    FActiveMode: TSynPluginMultiCaretMode;
    FDefaultColumnSelectMode: TSynPluginMultiCaretDefaultMode;
    FDefaultMode: TSynPluginMultiCaretDefaultMode;
    FEnableWithColumnSelection: Boolean;
    FKeyStrokes: TSynPluginMultiCaretKeyStrokes;
    FOnBeforeCommand: TSynMultiCaretBeforeCommand;
    FOptions: TSynMultiCaretOptions;
    FStateFlags: TSynPluginMultiCaretStateFlags;
    FMouseActions: TSynPluginMultiCaretMouseActions;
    FSelY1, FSelY2, FSelX: Integer;
    FColSelDoneY1, FColSelDoneY2, FColSelDonePhysX: Integer;
    FSpaceTrimmerLocked: Boolean;
    FForeignPaintLock, FNestedCommandProcessor: Integer;

    function GetIsInMainExecution: Boolean;
    function GetIsInRepeatExecution: Boolean;
    procedure RemoveCaretsInSelection;
    procedure SetActiveMode(AValue: TSynPluginMultiCaretMode);
    procedure SetDefaultColumnSelectMode(AValue: TSynPluginMultiCaretDefaultMode);
    procedure SetDefaultMode(AValue: TSynPluginMultiCaretDefaultMode);
    procedure SetSkipCaretAtSel;

    procedure UpdateCaretForUndo(var AnUndoItem: TSynEditUndoItem; AnIsBeginUndo: Boolean);
    function HandleUndoRedoItem(Caller: TObject; Item: TSynEditUndoItem): Boolean;

    procedure LockSpaceTrimmer; // Todo: per line lock / reverse: trimmer should ask / add event for trimmer via caretObj
    procedure UnLockSpaceTrimmer;
  protected
    function LogPhysConvertor: TSynLogicalPhysicalConvertor; inline;
    function PhysicalToLogical(AIndex, AColumn: Integer; out AColOffset: Integer;
                               ACharSide: TSynPhysCharSide= cspDefault;
                               AFlags: TSynLogPhysFlags = []): Integer; inline;


    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;
    procedure DoBufferChanged(Sender: TObject); override;

    procedure DoAfterDecPaintLock(Sender: TObject); override;
    procedure DoIncForeignPaintLock(Sender: TObject);
    procedure DoDecForeignPaintLock(Sender: TObject);

    procedure DoCleared; override;
    procedure DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos, aCount,
      aLineBrkCnt: Integer; aText: String); override;
    procedure DoCaretChanged(Sender: TObject);
    procedure DoSelectionChanged(Sender: TObject);
    procedure DoBeforeSetSelText(Sender: TObject; AMode: TSynSelectionMode; ANewText: PChar);
    procedure TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
      var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; FinishComboOnly: Boolean;
      var ComboKeyStrokes: TSynEditKeyStrokes);
    procedure ProcessMySynCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char;
      Data: pointer; HandlerData: pointer);
    procedure ProcessAllSynCommand(Sender: TObject; AfterProcessing: boolean;
              var Handled: boolean; var Command: TSynEditorCommand;
              var AChar: TUTF8Char; Data: pointer; HandlerData: pointer);
    function MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
                         HandleActionProc: TSynEditMouseActionHandler): Boolean;
    function DoHandleMouseAction(AnAction: TSynEditMouseAction;
                                 var AnInfo: TSynEditMouseActionInfo): Boolean;

    procedure AddStateFlags(AFlags: TSynPluginMultiCaretStateFlags; AnOnlyIfLocked: Boolean);
    function CreateVisual: TSynPluginMultiCaretVisual; override;
    property ViewedTextBuffer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddCaretAtLogPos(X, Y, Offs: Integer);
    property IsInMainExecution: Boolean read GetIsInMainExecution;
    property IsInRepeatExecution: Boolean read GetIsInRepeatExecution;
    property MouseActions: TSynPluginMultiCaretMouseActions read FMouseActions;
    property KeyStrokes: TSynPluginMultiCaretKeyStrokes read FKeyStrokes;
    property EnableWithColumnSelection: Boolean read FEnableWithColumnSelection write FEnableWithColumnSelection default True;
    property ActiveMode: TSynPluginMultiCaretMode read FActiveMode write SetActiveMode;
    property DefaultMode: TSynPluginMultiCaretDefaultMode read FDefaultMode write SetDefaultMode default mcmMoveAllCarets;
    property DefaultColumnSelectMode: TSynPluginMultiCaretDefaultMode
      read FDefaultColumnSelectMode write SetDefaultColumnSelectMode default mcmCancelOnCaretMove;
    property Options: TSynMultiCaretOptions read FOptions write FOptions;
    property OnBeforeCommand: TSynMultiCaretBeforeCommand read FOnBeforeCommand write FOnBeforeCommand;
  end;

  TSynPluginMultiCaret = class(TSynCustomPluginMultiCaret)
  published
    property MouseActions;
    property KeyStrokes;
    property EnableWithColumnSelection;
    property DefaultMode;
    property DefaultColumnSelectMode;
    property OnBeforeCommand;
  end;

implementation

{$IfDef SynMultiCaretDebug}
var
  SynMCaretDebug: PLazLoggerLogGroup;
{$EndIf}

const
  SynMouseCommandNames: array [0..1] of TIdentMapEntry = (
    (Value: emcPluginMultiCaretToggleCaret; Name: 'emcPluginMultiCaretToggleCaret'),
    (Value: emcPluginMultiCaretSelectionToCarets; Name: 'emcPluginMultiCaretSelectionToCarets')
  );

const
  EditorKeyCommandStrs: array[0..5] of TIdentMapEntry = (
    (Value: ecPluginMultiCaretSetCaret;         Name: 'ecPluginMultiCaretSetCaret'),
    (Value: ecPluginMultiCaretUnsetCaret;       Name: 'ecPluginMultiCaretUnsetCaret'),
    (Value: ecPluginMultiCaretToggleCaret;      Name: 'ecPluginMultiCaretToggleCaret'),
    (Value: ecPluginMultiCaretClearAll;         Name: 'ecPluginMultiCaretClearAll'),
    (Value: ecPluginMultiCaretModeCancelOnMove; Name: 'ecPluginMultiCaretModeCancelOnMove'),
    (Value: ecPluginMultiCaretModeMoveAll;      Name: 'ecPluginMultiCaretModeMoveAll')
  );

function IdentToKeyCommand(const Ident: string; var Cmd: longint): boolean;
begin
  Result := IdentToInt(Ident, Cmd, EditorKeyCommandStrs);
end;

function KeyCommandToIdent(Cmd: longint; var Ident: string): boolean;
begin
  Result := (Cmd >= ecPluginFirstMultiCaret) and (Cmd <= ecPluginLastMultiCaret);
  if not Result then exit;
  Result := IntToIdent(Cmd, Ident, EditorKeyCommandStrs);
end;

procedure GetEditorCommandValues(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := Low(EditorKeyCommandStrs) to High(EditorKeyCommandStrs) do
    Proc(EditorKeyCommandStrs[I].Name);
end;

function SynMouseCmdToIdent(SynMouseCmd: Longint; var Ident: String): Boolean;
begin
  Ident := '';
  Result := IntToIdent(SynMouseCmd, Ident, SynMouseCommandNames);
end;

function IdentToSynMouseCmd(const Ident: string; var SynMouseCmd: Longint): Boolean;
begin
  SynMouseCmd := 0;
  Result := IdentToInt(Ident, SynMouseCmd, SynMouseCommandNames);
end;

procedure GetEditorMouseCommandValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := Low(SynMouseCommandNames) to High(SynMouseCommandNames) do
    Proc(SynMouseCommandNames[I].Name);
end;

function MouseCommandName(emc: TSynEditorMouseCommand): String;
begin
  case emc of
    emcPluginMultiCaretToggleCaret:       Result := SYNS_emcPluginMultiCaretToggleCaret;
    emcPluginMultiCaretSelectionToCarets: Result := SYNS_emcPluginMultiCaretSelectionToCarets;
    else
      Result := '';
  end;
end;

function MouseCommandConfigName(emc: TSynEditorMouseCommand): String;
begin
  case emc of
    emcPluginMultiCaretToggleCaret,
    emcPluginMultiCaretSelectionToCarets:   Result := '';
    else
      Result := '';
  end;
end;

{ TSynPluginMultiCaretVisual }

constructor TSynPluginMultiCaretVisual.Create(AHandleOwner: TWinControl;
  APainterClass: TSynEditScreenCaretPainterClass; AnUsedList,
  AnUnUsedList: TSynPluginMultiCaretVisualList);
begin
  FListIndex := -1;
  FUsedList := AnUsedList;
  FUnUsedList := AnUnUsedList;
  inherited Create(AHandleOwner, APainterClass);
end;

procedure TSynPluginMultiCaretVisual.MoveToUsed;
begin
  {$IfDef SynMultiCaretAssert}
  assert((FListIndex < 0) or (not FIsUsed), 'TSynPluginMultiCaretVisual.MoveToUsed: not yet on list');
  FIsUsed := True;
  {$ENDIF}
  if FListIndex >= 0 then
    FUnUsedList.Remove(Self);
  FUsedList.Add(Self);
end;

procedure TSynPluginMultiCaretVisual.MoveToUnUsed;
begin
  {$IfDef SynMultiCaretAssert}
  assert((FListIndex < 0) or FIsUsed, 'TSynPluginMultiCaretVisual.MoveToUnUsed: not yet on list');
  FIsUsed := False;
  {$ENDIF}
  if FListIndex >= 0 then
    FUsedList.Remove(Self);
  FUnUsedList.Add(Self);
  Visible := False;
end;

{ TSynPluginMultiCaretVisualList }

function TSynPluginMultiCaretVisualList.GetScreenCaret(Index: Integer): TSynPluginMultiCaretVisual;
begin
  Result := FList[Index];
end;

destructor TSynPluginMultiCaretVisualList.Destroy;
begin
  inherited Destroy;
  Clear;
end;

procedure TSynPluginMultiCaretVisualList.Add(AScreenCaret: TSynPluginMultiCaretVisual);
begin
  if (AScreenCaret.ListIndex >= 0) and (AScreenCaret.ListIndex < FCount) and
     (FList[AScreenCaret.ListIndex] = AScreenCaret)
  then begin
    assert(False, 'TSynPluginMultiCaretVisualList.Add: not on list');
    exit;
  end;

  {$IfDef SynMultiCaretDebug}
  if FCount = Length(FList) then debugln(SynMCaretDebug, ['TSynPluginMultiCaretVisualList.Add ', FCount + max(16, FCount div 16)]);
  {$EndIf}
  if FCount = Length(FList) then
    SetLength(FList, FCount + max(16, FCount div 16));

  FList[FCount] := AScreenCaret;
  AScreenCaret.FListIndex := FCount;
  inc(FCount);
end;

procedure TSynPluginMultiCaretVisualList.Remove(AScreenCaret: TSynPluginMultiCaretVisual);
var
  t: TSynPluginMultiCaretVisual;
begin
  if (AScreenCaret.ListIndex < 0) or (AScreenCaret.ListIndex >= FCount) or
     (FList[AScreenCaret.ListIndex] <> AScreenCaret)
  then begin
    assert(False, 'TSynPluginMultiCaretVisualList.Remove: not on list');
    exit;
  end;
  if AScreenCaret.ListIndex < FCount then begin
    t := FList[FCount - 1];
    FList[AScreenCaret.ListIndex] := t;
    t.FListIndex := AScreenCaret.ListIndex;
  end;
  AScreenCaret.FListIndex := -1;
  dec(FCount);
end;

procedure TSynPluginMultiCaretVisualList.Clear;
var
  i: Integer;
begin
  for i := 0 to FCount - 1 do
    FList[i].Free;
  FCount := 0;
  SetLength(FList, EMPTY_LIST_LEN);
end;

function TSynPluginMultiCaretVisualList.Count: Integer;
begin
  Result := FCount;
end;

{ TSynPluginMultiCaretList }

function TSynPluginMultiCaretList.FindEqOrNextCaretRawIdx(X, Y, Offs: Integer;
  LowIdx: integer; HighIdx: integer): Integer;
var
  l, h: integer;
  cp: ^TCaretData;
begin
  if LowIdx < 0
  then l := FLowIndex
  else l := LowIdx;
  if HighIdx < 0
  then h := FHighIndex
  else h := HighIdx;

  if h < l then begin
    Result := h;
    exit;
  end;

  Result := (l + h) div 2;
  // FPC does not optimize the repeated array access
  while (h > l) do begin
    cp := @FCarets[Result];
    if (cp^.y > y) or
       ( (cp^.y = y) and
         ( (cp^.x > x) or
           ((cp^.x = x) and (cp^.offs >= Offs))
         )
       )
    then
      h := Result
    else
      l := Result + 1;
    Result := cardinal(l + h) div 2;
  end;
  cp := @FCarets[Result];
  if (cp^.y < y) or
     ( (cp^.y = y) and
       (cp^.x < x) or
       ((cp^.x = x) and (cp^.offs < Offs))
     )
  then
    inc(Result);
end;

function TSynPluginMultiCaretList.GetCaret(Index: Integer): TPoint;
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.GetCaret: (Index>=FLowIndex) and (Index <= FHighIndex)');
  Result.x := FCarets[Index].x;
  Result.y := FCarets[Index].y;
end;

function TSynPluginMultiCaretList.GetCaretFull(Index: Integer): TLogCaretPoint;
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.GetCaretX: (Index>=FLowIndex) and (Index <= FHighIndex)');
  Result.X := FCarets[Index].x;
  Result.Y := FCarets[Index].y;
  Result.Offs := FCarets[Index].offs;
end;

function TSynPluginMultiCaretList.GetCaretKeepX(Index: Integer): Integer;
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.GetCaretX: (Index>=FLowIndex) and (Index <= FHighIndex)');
  Result := FCarets[Index].KeepX;
end;

function TSynPluginMultiCaretList.GetCaretOffs(Index: Integer): Integer;
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.GetCaretX: (Index>=FLowIndex) and (Index <= FHighIndex)');
  Result := FCarets[Index].offs;
end;

function TSynPluginMultiCaretList.GetCaretX(Index: Integer): Integer;
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.GetCaretX: (Index>=FLowIndex) and (Index <= FHighIndex)');
  Result := FCarets[Index].x;
end;

function TSynPluginMultiCaretList.GetCaretY(Index: Integer): Integer;
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.GetCaretY: (Index>=FLowIndex) and (Index <= FHighIndex)');
  Result := FCarets[Index].y;
end;

function TSynPluginMultiCaretList.GetFlags(Index: Integer): TCaretFlags;
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.GetFlags: (Index>=FLowIndex) and (Index <= FHighIndex)');
  Result := FCarets[Index].Flags;
end;

function TSynPluginMultiCaretList.GetMainCaretIndex: Integer;
begin
  if FMainCaretIndex >= FLowIndex then
    Result := FMainCaretIndex - FLowIndex
  else
    Result := -1;
end;

function TSynPluginMultiCaretList.GetVisual(Index: Integer): TSynPluginMultiCaretVisual;
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.GetVisual: (Index>=FLowIndex) and (Index <= FHighIndex)');
  Result := FCarets[Index].Visual;
end;

procedure TSynPluginMultiCaretList.SetCaret(Index: Integer; AValue: TPoint);
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.SetCaret: (Index>=FLowIndex) and (Index <= FHighIndex)');
  if (FCarets[Index].x = AValue.x) and (FCarets[Index].y = AValue.y) then exit;
  FCarets[Index].x := AValue.x;
  FCarets[Index].y := AValue.y;
  AdjustAfterChange(Index);
end;

procedure TSynPluginMultiCaretList.SetCaretFull(Index: Integer; AValue: TLogCaretPoint);
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.SetCaretX: (Index>=FLowIndex) and (Index <= FHighIndex)');
  if (FCarets[Index].x = AValue.x) and (FCarets[Index].y = AValue.y) and (FCarets[Index].offs = AValue.Offs) then
    exit;
  FCarets[Index].x := AValue.X;
  FCarets[Index].y := AValue.Y;
  FCarets[Index].offs := AValue.Offs;
  AdjustAfterChange(Index);
end;

procedure TSynPluginMultiCaretList.SetCaretKeepX(Index: Integer; AValue: Integer);
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.SetCaretX: (Index>=FLowIndex) and (Index <= FHighIndex)');
  //if FCarets[Index].KeepX = AValue then exit;
  FCarets[Index].KeepX := AValue;
end;

procedure TSynPluginMultiCaretList.SetCaretOffs(Index: Integer; AValue: Integer);
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.SetCaretX: (Index>=FLowIndex) and (Index <= FHighIndex)');
  if FCarets[Index].offs = AValue then exit;
  FCarets[Index].offs := AValue;
  AdjustAfterChange(Index);
end;

procedure TSynPluginMultiCaretList.SetCaretX(Index: Integer; AValue: Integer);
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.SetCaretX: (Index>=FLowIndex) and (Index <= FHighIndex)');
  if FCarets[Index].x = AValue then exit;
  FCarets[Index].x := AValue;
  AdjustAfterChange(Index);
end;

procedure TSynPluginMultiCaretList.SetCaretY(Index: Integer; AValue: Integer);
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.SetCaretY: (Index>=FLowIndex) and (Index <= FHighIndex)');
  if FCarets[Index].y = AValue then exit;
  FCarets[Index].y := AValue;
  AdjustAfterChange(Index);
end;

procedure TSynPluginMultiCaretList.SetVisual(Index: Integer; AValue: TSynPluginMultiCaretVisual);
begin
  Index := Index + FLowIndex;
  assert((Index>=FLowIndex) and (Index <= FHighIndex), 'TSynPluginMultiCaretList.SetVisual: (Index>=FLowIndex) and (Index <= FHighIndex)');
  if FCarets[Index].Visual <> nil then
    FCarets[Index].Visual.MoveToUnUsed;
  FCarets[Index].Visual := AValue;
  if AValue <> nil then
    AValue.MoveToUsed;
end;

function TSynPluginMultiCaretList.InternalRemoveCaretEx(RawIndex: Integer;
  AlternativeRawIndex: Integer): Integer;
begin
  assert((RawIndex>=FLowIndex) and (RawIndex <= FHighIndex), 'TSynPluginMultiCaretList.InternalRemoveCaretEx: (Index>=FLowIndex) and (Index <= FHighIndex)');
  if (RawIndex = FMainCaretIndex) and (AlternativeRawIndex >= FLowIndex) then
    Result := InternalRemoveCaret(AlternativeRawIndex)
  else
    Result := InternalRemoveCaret(RawIndex);
end;

function TSynPluginMultiCaretList.InternalRemoveCaret(RawIndex: Integer): integer;
begin
  assert(FIteratoreMode=mciNone, 'TSynPluginMultiCaretList.AddCaret: FIteratoreMode=mciNone');
  assert((RawIndex>=FLowIndex) and (RawIndex <= FHighIndex), 'TSynPluginMultiCaretList.InternalRemoveCaret: (RawIndex>=FLowIndex) and (RawIndex <= FHighIndex)');
  Result := 0; // change to LowCaret .. RawIndex

  if FCarets[RawIndex].Visual <> nil then
    FCarets[RawIndex].Visual.MoveToUnUsed;
  if RawIndex = FMainCaretIndex then
    FMainCaretIndex := -1;

  if RawIndex > (FHighIndex + FLowIndex) div 2 then begin
    if (RawIndex < FHighIndex) then
      Move(FCarets[RawIndex+1], FCarets[RawIndex], (FHighIndex - RawIndex) * SizeOf(FCarets[0]));
    dec(FHighIndex);
    if RawIndex < FMainCaretIndex then
      dec(FMainCaretIndex);
  end
  else begin
    if (RawIndex > FLowIndex) then
      Move(FCarets[FLowIndex], FCarets[FLowIndex+1], (RawIndex - FLowIndex) * SizeOf(FCarets[0]));
    inc(FLowIndex);
    if RawIndex > FMainCaretIndex then
      inc(FMainCaretIndex);
    Result := 1; // FLowIndex was increasde by 1;
  end;

  //debugln(SynMCaretDebug, ['TSynPluginMultiCaretList.InternalRemoveCaret ', RawIndex, ' , ', count]);
end;

procedure TSynPluginMultiCaretList.AdjustAfterChange(RawIndex: Integer);
begin
  assert(FIteratoreMode=mciNone, 'TSynPluginMultiCaretList.AddCaret: FIteratoreMode=mciNone');
  FLowCaret := @FCarets[FLowIndex];
  FHighCaret := @FCarets[FHighIndex];
  AdjustAfterChange(@FCarets[RawIndex]);
end;

constructor TSynPluginMultiCaretList.Create;
begin
  FLowIndex := 0;
  FHighIndex := -1;
  FMainCaretIndex := -1;
end;

function TSynPluginMultiCaretList.AddCaret(X, Y, Offs: Integer; flags: TCaretFlags;
  PhysX: Integer): Integer;
var
  NewCarets: Array of TCaretData;
  Len, AddLen, i, Middle: Integer;
begin
  assert(FIteratoreMode=mciNone, 'TSynPluginMultiCaretList.AddCaret: FIteratoreMode=mciNone');
  Result := FindEqOrNextCaretRawIdx(x, y, Offs);
  if Result < FLowIndex then
    Result := FLowIndex;
  if (Result <= FHighIndex) and (FCarets[Result].x = x) and (FCarets[Result].y = y) and
     (FCarets[Result].offs = Offs) and not(cfAddDuplicate in flags)
  then begin
    if cfMainCaret in flags then begin
      FMainCaretIndex := Result;
      FCarets[Result].Flags := flags + [cfMainCaret];
    end;
    // TODO maybe update PhysX;
    Result := Result - FLowIndex;
    exit;
  end;

  Len := length(FCarets) - 1;
  Middle := (FLowIndex + FHighIndex) div 2;
  if (FLowIndex > 0) and ((Result < Middle) or (FHighIndex = len))
  then begin
    // use space in front of list
    if (Result > FHighIndex) and (FHighIndex = High(FCarets))   // moving all entries
    then i := max(FLowIndex div 2 - 1, 0)                       // Make some room at the end of the list
    else i := 0;
    if Result > FLowIndex then
      Move(FCarets[FLowIndex], FCarets[FLowIndex-1-i], (Result-FLowIndex) * SizeOf(FCarets[0]));
    FLowIndex  := FLowIndex - 1 - i;
    FHighIndex := FHighIndex - i;
    Result     := Result - 1 - i;
    if Result > FMainCaretIndex
    then FMainCaretIndex := FMainCaretIndex - 1 - i
    else FMainCaretIndex := FMainCaretIndex - i;
  end
  else
  if FHighIndex < Len then begin
    // use space at end of list
    if (Result = FLowIndex) and (FLowIndex = 0)              // moving all entries
    then i := max((High(FCarets)-FHighIndex) div 2 - 1, 0)   // Make some room at the start of the list
    else i := 0;
    if Result <= FHighIndex then
      Move(FCarets[Result], FCarets[Result+1+i], (FHighIndex-Result+1) * SizeOf(FCarets[0]));
    FHighIndex := FHighIndex + 1 + i;
    FLowIndex  := FLowIndex + i;
    Result     := Result + i;
    if Result <= FMainCaretIndex
    then FMainCaretIndex := FMainCaretIndex + 1 + i
    else FMainCaretIndex := FMainCaretIndex + i;
  end
  else begin
    // realloc all
    AddLen := Max(32, Len div 8);
    SetLength(NewCarets, Len + 2 * AddLen);
    i := Result-FLowIndex;
    if i > 0 then
      Move(FCarets[FLowIndex], NewCarets[AddLen], (i) * SizeOf(FCarets[0]));
    if Result <= FHighIndex then
      Move(FCarets[Result], NewCarets[AddLen+i+1], (FHighIndex-Result+1) * SizeOf(FCarets[0]));

    if Result <= FMainCaretIndex
    then FMainCaretIndex := FMainCaretIndex - FLowIndex + AddLen + 1
    else FMainCaretIndex := FMainCaretIndex - FLowIndex + AddLen;

    FLowIndex := AddLen;
    FHighIndex := AddLen + Len + 1;
    Result := i + AddLen;
    FCarets := NewCarets;
  end;

  FCarets[Result].x := x;
  FCarets[Result].offs := Offs;
  FCarets[Result].y := y;
  FCarets[Result].KeepX := PhysX;
  FCarets[Result].Visual := nil;
  FCarets[Result].Flags := flags - [cfAddDuplicate];

  if cfMainCaret in flags then
    FMainCaretIndex := Result;

  Result := Result - FLowIndex;
end;

procedure TSynPluginMultiCaretList.RemoveCaret(Index: Integer);
begin
  assert(FIteratoreMode=mciNone, 'TSynPluginMultiCaretList.RemoveCaret: FIteratoreMode=mciNone');
  InternalRemoveCaret(Index+FLowIndex);
end;

procedure TSynPluginMultiCaretList.Clear(AFreeVisual: Boolean; ACapacity: Integer);
var
  i: Integer;
begin
  assert(FIteratoreMode=mciNone, 'TSynPluginMultiCaretList.Clear: FIteratoreMode=mciNone');
  if AFreeVisual then
  begin
    for i := FLowIndex to FHighIndex do
      if FCarets[i].Visual <> nil then begin
        FCarets[i].Visual.UsedList.Remove(FCarets[i].Visual);
        FCarets[i].Visual.Free;
      end
  end
  else
    for i := FLowIndex to FHighIndex do
      if FCarets[i].Visual <> nil then
        FCarets[i].Visual.MoveToUnUsed;
  SetLength(FCarets, ACapacity);
  FLowIndex := Cardinal(ACapacity) div 2;
  FHighIndex := FLowIndex - 1;
  FMainCaretIndex := -1;
end;

function TSynPluginMultiCaretList.Count: Integer;
begin
  Result := FHighIndex - FLowIndex + 1;
end;

function TSynPluginMultiCaretList.Capacity: Integer;
begin
  Result := Length(FCarets);
end;

procedure TSynPluginMultiCaretList.ImportFromSortedList(AMultiCaretList: TLogCaretPointArray);
var
  i: Integer;
  c: PCaretData;
begin
  Clear(False, Length(AMultiCaretList) + 32);
  FLowIndex := 16;
  FHighIndex := FLowIndex + High(AMultiCaretList);
  c := @FCarets[FLowIndex];
  for i := 0 to High(AMultiCaretList) do begin
    c^.x := AMultiCaretList[i].X;
    c^.offs := AMultiCaretList[i].Offs;
    c^.y := AMultiCaretList[i].Y;
    c^.KeepX := -1;
    c^.Visual := nil;
    c^.Flags := [];
    inc(c);
  end;
end;

function TSynPluginMultiCaretList.FindCaretIdx(X, Y, Offs: Integer): Integer;
begin
  Result := FindEqOrNextCaretRawIdx(x, y, offs);
  if Result < FLowIndex then
    exit(-1);
  if (Result > FHighIndex) or (FCarets[Result].x <> x) or (FCarets[Result].offs <> Offs) or
     (FCarets[Result].y <> y)
  then
    Result := -1
  else
    Result := Result - FLowIndex;
end;

function TSynPluginMultiCaretList.FindEqOrNextCaretIdx(X, Y, Offs: Integer; LowIdx: integer;
  HighIdx: integer): Integer;
begin
  if LowIdx >= 0 then inc(LowIdx, FLowIndex);
  if HighIdx >= 0 then inc(HighIdx, FLowIndex);
  Result := FindEqOrNextCaretRawIdx(x, y, offs, LowIdx, HighIdx);
  if (Result > FHighIndex)
  then
    Result := -1
  else
    Result := Result - FLowIndex;
end;

procedure TSynPluginMultiCaretList.AdjustAllAfterEdit(aLinePos, aBytePos, aCount,
  aLineBrkCnt: Integer);
var
  i, j, lowest: Integer;
begin
  if Count = 0 then exit;
  lowest := FindEqOrNextCaretRawIdx(aBytePos, aLinePos, 0);
  if lowest < FLowIndex then lowest := FLowIndex;

  if aLineBrkCnt = 0 then begin
    if aCount < 0 then begin
      i := lowest;
      while i <= FHighIndex do begin
        if (FCarets[i].y = aLinePos) and (FCarets[i].x >= aBytePos) then
          FCarets[i].x := Max(aBytePos, FCarets[i].x + aCount)
        else
          break;
        inc(i);
      end;
    end
    else begin // aCount >= 0
      for i := lowest to FHighIndex do begin
        if (FCarets[i].y = aLinePos) and (FCarets[i].x >= aBytePos) then
          FCarets[i].x := FCarets[i].x + aCount
        else
          break;
      end;
    end;
  end
  else // aLineBrkCnt = 0
  begin // aCount is always 0 (aBytePos:=max(1,aBytePos+aCount)) // aBytePos is the end of line
    if aLineBrkCnt < 0 then begin
      j := aLinePos+(-aLineBrkCnt);
      i := lowest;
      while i <= FHighIndex do begin
        if (FCarets[i].y < j) then
          FCarets[i].x := aBytePos;
        if (FCarets[i].y = j) then
          FCarets[i].x := FCarets[i].x - 1 + aBytePos
        else
          break;
        FCarets[i].y := aLinePos;
        inc(i);
      end;
      while i <= FHighIndex do begin
        FCarets[i].y := FCarets[i].y + aLineBrkCnt;
        inc(i);
      end;
    end
    else begin // aLineBrkCnt >= 0
      i := lowest;
      while i <= FHighIndex do begin
        if (FCarets[i].y = aLinePos) then
          FCarets[i].x := FCarets[i].x + 1 - aBytePos
        else
          break;
        FCarets[i].y := FCarets[i].y + aLineBrkCnt;
        inc(i);
      end;
      while i <= FHighIndex do begin
        FCarets[i].y := FCarets[i].y + aLineBrkCnt;
        inc(i);
      end;
    end;
  end;
end;

procedure TSynPluginMultiCaretList.FindAndRemoveMergedCarets;
var
  i, i2: Integer;
  c: TCaretData;
begin
  i := FLowIndex + 1;
  while i <= FHighIndex do begin
    if (FCarets[i].y = FCarets[i-1].y) and (FCarets[i].x = FCarets[i-1].x) then begin
      i := i + InternalRemoveCaretEx(i, i-1);
      continue;
    end;
    if (FCarets[i].y < FCarets[i-1].y) or
       ((FCarets[i].y = FCarets[i-1].y) and (FCarets[i].x < FCarets[i-1].x))
    then begin
      // should not happen
      {$IfDef SynMultiCaretDebug}
      debugln(SynMCaretDebug, ['TSynPluginMultiCaretList.FindAndRemoveMergedCarets BUBBLE SORTING']);
      {$EndIf}
      i2 := i;
      c := FCarets[i2];
      repeat
        FCarets[i2] := FCarets[i2-1];
        dec(i2);
      until (i2 = FLowIndex) or (FCarets[i2].y > FCarets[i2-1].y) or
            ((FCarets[i2].y = FCarets[i2-1].y) and (FCarets[i2].x > FCarets[i2-1].x));
      FCarets[i2] := c;
      if FMainCaretIndex = i then
        FMainCaretIndex := i2;
      if (FMainCaretIndex < i) and (FMainCaretIndex >= i2) then
        inc(FMainCaretIndex);
    end;
    inc(i);
  end;
end;

procedure TSynPluginMultiCaretList.IncMergeLock;
begin
  inc(FMergeLock);
end;

procedure TSynPluginMultiCaretList.DecMergeLock;
begin
  dec(FMergeLock);
end;

function TSynPluginMultiCaretList.GetCurrentCaretFull: TLogCaretPoint;
begin
  Result.X := FCurrenCaret^.x;
  Result.Y := FCurrenCaret^.y;
  Result.Offs := FCurrenCaret^.offs;
end;

function TSynPluginMultiCaretList.GetCurrentCaretFlags: TCaretFlags;
begin
  Result := FCurrenCaret^.Flags;
end;

function TSynPluginMultiCaretList.GetCurrentCaretKeepX: Integer;
begin
  Result := FCurrenCaret^.KeepX;
end;

procedure TSynPluginMultiCaretList.SetCurrentCaretFull(AValue: TLogCaretPoint);
begin
  FCurrenCaret^.x := AValue.X;
  FCurrenCaret^.y := AValue.Y;
  FCurrenCaret^.offs := AValue.Offs;
  AdjustAfterChange(FCurrenCaret);
end;

procedure TSynPluginMultiCaretList.SetCurrentCaretKeepX(AValue: Integer);
begin
  FCurrenCaret^.KeepX := AValue;
  AdjustAfterChange(FCurrenCaret);
end;

procedure TSynPluginMultiCaretList.AdjustAfterChange(ACaret: PCaretData);
  function ToRawIndex(C: PCaretData): Integer;
  begin
    Result := (C - PCaretData(@FCarets[0])); // div SizeOf(FCarets[0]);
  end;
var
  NewCaretPos, HelpCaretPos: PCaretData;
  NewCaretIdx, y, x, o: Integer;
  v: TCaretData;
begin
  assert((ACaret>=FLowCaret) and (ACaret <= FHighCaret) and (ACaret <> nil), 'TSynPluginMultiCaretList.AdjustAfterChange: (ACaret>=FLowCaret) and (ACaret <= FHighCaret)');
  // if iterating then this must only be called with fcurrentcaret
  assert((FIteratoreMode=mciNone) or ((ACaret = FCurrenCaret)), 'TSynPluginMultiCaretList.AdjustAfterChange: (FIteratoreMode=mciNone) or (ACaret = FCurrenCaret)');

  y := ACaret^.y;

  if (ACaret > FLowCaret) then begin
    NewCaretPos := ACaret - 1;
    if (y <= NewCaretPos^.y) then begin
      x := ACaret^.x;
      if (y < NewCaretPos^.y) or (x <= NewCaretPos^.x) then begin
        o := ACaret^.offs;
        if (x < NewCaretPos^.x) or ( (x = NewCaretPos^.x) and (o <= NewCaretPos^.offs) )
        then begin
          HelpCaretPos := NewCaretPos - 1;
          if (HelpCaretPos >= FLowCaret) and
             ( (y < HelpCaretPos^.y) or
               ( (y = HelpCaretPos^.y) and
                 ( (x < HelpCaretPos^.x) or ( (x = HelpCaretPos^.x) and (o < HelpCaretPos^.offs) ) )
               ) )
          then begin
            NewCaretIdx := FindEqOrNextCaretRawIdx(x,y,o, FLowIndex, ToRawIndex(HelpCaretPos));
            if NewCaretIdx > FHighIndex then NewCaretIdx := FHighIndex;
            NewCaretPos := @FCarets[NewCaretIdx];
          end;

          if (y = NewCaretPos^.y) and (x = NewCaretPos^.x) and (o = NewCaretPos^.offs) then begin
            if FMergeLock = 0 then
              InternalRemoveCaretEx(ToRawIndex(ACaret), ToRawIndex(NewCaretPos));
            exit;
          end;
          v := ACaret^;
          {$IfDef SynMultiCaretDebug}
      debugln(SynMCaretDebug, ['TSynPluginMultiCaretList.AdjustAfterChange ', ToRawIndex(NewCaretPos), ' ',ToRawIndex(ACaret)]);
          {$EndIf}
          Move(NewCaretPos^, (NewCaretPos+1)^, Pointer(ACaret)-Pointer(NewCaretPos));
          NewCaretPos^ := v;

          assert(FBeforeNextCaret=nil, 'TSynPluginMultiCaretList.AdjustAfterChange: FBeforeNextCaret=nil');
          FCurrenCaret := NewCaretPos; // move down
          case FIteratoreMode of
            mciUp:   FBeforeNextCaret := ACaret; // continue at ACaret+1;
            mciDown: begin
              FBeforeNextCaret := ACaret + 1;  // continue at ACaret;
              Include(FCurrenCaret^.Flags, cfIterationDone);
              inc(FIterationDoneCount);
            end;
          end;
        end
      end;
    end;
  end;

  if (ACaret < FHighCaret) then begin
    NewCaretPos := ACaret + 1;
    if (y >= NewCaretPos^.y) then begin
      x := ACaret^.x;
      if (y > NewCaretPos^.y) or (x >= NewCaretPos^.x) then begin
        o := ACaret^.offs;
        if (x > NewCaretPos^.x) or ( (x = NewCaretPos^.x) and (o >= NewCaretPos^.offs) )
        then begin
          HelpCaretPos := NewCaretPos + 1;
          if (HelpCaretPos <= FHighCaret) and
             ( (y > HelpCaretPos^.y) or
               ( (y = HelpCaretPos^.y) and
                 ( (x > HelpCaretPos^.x) or ( (x = HelpCaretPos^.x) and (o > HelpCaretPos^.offs) ) )
               ) )
          then begin
            NewCaretIdx := FindEqOrNextCaretRawIdx(x,y,o, ToRawIndex(HelpCaretPos), FHighIndex);
            if NewCaretIdx < FLowIndex then NewCaretIdx := FLowIndex;
            NewCaretPos := @FCarets[NewCaretIdx];
          end;

          if (y = NewCaretPos^.y) and (x = NewCaretPos^.x) and (o = NewCaretPos^.offs) then begin
            if FMergeLock = 0 then
              InternalRemoveCaretEx(ToRawIndex(ACaret), ToRawIndex(NewCaretPos));
            exit;
          end;
          v := ACaret^;
          {$IfDef SynMultiCaretDebug}
      debugln(SynMCaretDebug, ['TSynPluginMultiCaretList.AdjustAfterChange ', ToRawIndex(NewCaretPos), ' ',ToRawIndex(ACaret)]);
          {$EndIf}
          Move((ACaret+1)^, ACaret^, Pointer(NewCaretPos)-Pointer(ACaret));
          NewCaretPos^ := v;

          assert(FBeforeNextCaret=nil, 'TSynPluginMultiCaretList.AdjustAfterChange: FBeforeNextCaret=nil');
          FCurrenCaret := NewCaretPos; // move down
          case FIteratoreMode of
            mciDown:   FBeforeNextCaret := ACaret; // continue at ACaret-1;
            mciUp: begin
              FBeforeNextCaret := ACaret - 1;  // continue at ACaret;
              Include(FCurrenCaret^.Flags, cfIterationDone);
              inc(FIterationDoneCount);
            end;
          end;
        end;
      end;
    end;
  end;

end;

procedure TSynPluginMultiCaretList.StartIteratorAtFirst;
begin
  FBeforeNextCaret := nil;
  if Length(FCarets) = 0 then begin
    FLowCaret := nil;
    FHighCaret := nil;
    FCurrenCaret := nil;
    exit;
  end;
  FLowCaret := @FCarets[FLowIndex];
  FHighCaret := @FCarets[FHighIndex];
  FCurrenCaret := FLowCaret - 1;
  FIteratoreMode := mciUp;
end;

function TSynPluginMultiCaretList.IterateNextUp: Boolean;
begin
  if FBeforeNextCaret <> nil then begin
    FCurrenCaret := FBeforeNextCaret;
    FBeforeNextCaret := nil;
  end;
  repeat
    Result := FCurrenCaret < FHighCaret;
    if not Result then begin
      FIteratoreMode := mciNone;
      assert(FIterationDoneCount = 0, 'TSynPluginMultiCaretList.IterateNextUp: FIterationDoneCount = 0');
      exit;
    end;
    inc(FCurrenCaret);
    if not(cfIterationDone in FCurrenCaret^.Flags) then
      break;
    Exclude(FCurrenCaret^.Flags, cfIterationDone);
    dec(FIterationDoneCount);
  until False;
end;

procedure TSynPluginMultiCaretList.StartIteratorAtLast;
begin
  FBeforeNextCaret := nil;
  if Length(FCarets) = 0 then begin
    FLowCaret := nil;
    FHighCaret := nil;
    FCurrenCaret := nil;
    exit;
  end;
  FLowCaret := @FCarets[FLowIndex];
  FHighCaret := @FCarets[FHighIndex];
  FCurrenCaret := FHighCaret + 1;
  FIteratoreMode := mciDown;
end;

function TSynPluginMultiCaretList.IterateNextDown: Boolean;
begin
  if FBeforeNextCaret <> nil then begin
    FCurrenCaret := FBeforeNextCaret;
    FBeforeNextCaret := nil;
  end;
  repeat
    Result := FCurrenCaret > FLowCaret;
    if not Result then begin
      FIteratoreMode := mciNone;
      assert(FIterationDoneCount = 0, 'TSynPluginMultiCaretList.IterateNextDown: FIterationDoneCount = 0');
      exit;
    end;
    dec(FCurrenCaret);
    if not(cfIterationDone in FCurrenCaret^.Flags) then
      break;
    Exclude(FCurrenCaret^.Flags, cfIterationDone);
    dec(FIterationDoneCount);
  until False;
end;

function TSynPluginMultiCaretList.CanPeekCaret(AIndexOffset: Integer): Boolean;
begin
  if AIndexOffset < 0 then
    Result := FCurrenCaret + AIndexOffset >= FLowCaret
  else
    Result := FCurrenCaret + AIndexOffset <= FHighCaret;
end;

function TSynPluginMultiCaretList.PeekCaretY(AIndexOffset: Integer): Integer;
begin
  Result := (FCurrenCaret+AIndexOffset)^.y;
end;

function TSynPluginMultiCaretList.PeekCaretFull(AIndexOffset: Integer): TLogCaretPoint;
begin
  Result.X := (FCurrenCaret+AIndexOffset)^.x;
  Result.Y := (FCurrenCaret+AIndexOffset)^.y;
  Result.Offs := (FCurrenCaret+AIndexOffset)^.offs;
end;

{ TSynPluginMultiCaretBase }

procedure TSynPluginMultiCaretBase.DoBoundsChanged(Sender: TObject);
var
  i: Integer;
  ta: TLazSynTextArea;
begin
  if FPaintLock > 0 then begin
    include(FPaintLockFlags, plfBoundsChanged);
    exit;
  end;

  ta := TextArea;
  for i := 0 to FUsedList.Count - 1 do
    FUsedList[i].ClipRect := ta.Bounds;
  UpdateCaretsPos;
end;

procedure TSynPluginMultiCaretBase.MergeAndRemoveCarets(AForce: Boolean);
var
  i: Integer;
begin
  if (FPaintLock > 0) and (not AForce) then begin
    include(FPaintLockFlags, plfMergeCarets);
    exit;
  end;

  Carets.FindAndRemoveMergedCarets;
  i := Carets.FindCaretIdx(CaretObj.BytePos, CaretObj.LinePos, CaretObj.BytePosOffset);
  if i >= 0 then
    RemoveCaret(i);
end;

function TSynPluginMultiCaretBase.IsCaretMergeRequested: Boolean;
begin
  Result := plfMergeCarets in FPaintLockFlags;
end;

procedure TSynPluginMultiCaretBase.DoLinesEdited(Sender: TSynEditStrings; aLinePos, aBytePos,
  aCount, aLineBrkCnt: Integer; aText: String);
begin
  Carets.AdjustAllAfterEdit(aLinePos, aBytePos, aCount, aLineBrkCnt);
  MergeAndRemoveCarets;
end;

procedure TSynPluginMultiCaretBase.SetColor(AValue: TColor);
var
  i: Integer;
begin
  if FColor = AValue then Exit;
  FColor := AValue;
  for i := 0 to FUsedList.Count - 1 do
    TSynEditScreenCaretPainterInternal(FUsedList[i].Painter).Color := FColor;
end;

function TSynPluginMultiCaretBase.CreateVisual: TSynPluginMultiCaretVisual;
begin
  Result := TSynPluginMultiCaretVisual.Create(Editor,
    TSynEditScreenCaretPainterInternal,
    FUsedList, FUnUsedList);
  Result.PaintTimer:= ScreenCaret.PaintTimer;
end;

function TSynPluginMultiCaretBase.GetVisual: TSynPluginMultiCaretVisual;
var
  ta: TLazSynTextArea;
  i: TSynCaretType;
begin
  if FUnUsedList.Count > 0 then
    Result := FUnUsedList[FUnUsedList.Count-1]
  else
    Result := CreateVisual;

  ta := TextArea;
  Result.ClipRect   := ta.Bounds;
  Result.CharHeight := ta.LineHeight  - Max(0, ta.ExtraLineSpacing);
  Result.CharWidth  := ta.CharWidth;
  if Editor.InsertMode then
    Result.DisplayType := Editor.InsertCaret
  else
    Result.DisplayType := Editor.OverwriteCaret;
  for i := low(TSynCaretType) to high(TSynCaretType) do
    Result.SetCaretTypeSize(i, FCustomPixelWidth[i], FCustomPixelHeight[i], FCustomOffsetX[i], FCustomOffsetY[i], FCustomFlags[i]);
  TSynEditScreenCaretPainterInternal(Result.Painter).Color := FColor;
end;

procedure TSynPluginMultiCaretBase.UpdateMainCaret;
begin
  if not FRestoreSingleCaretPainter then begin
    // always set internal painter
    if ScreenCaret.Painter.ClassType <> TSynEditScreenCaretPainterInternal then
      ScreenCaret.ChangePainter(TSynEditScreenCaretPainterInternal);
    exit;
  end;

  if Carets = nil then exit;

  if Carets.Count = 0 then begin
    if (ScreenCaret.Painter.ClassType = TSynEditScreenCaretPainterInternal) and
       (ScreenCaret.Painter.ClassType <> FSingleCaretClass) then
      ScreenCaret.ChangePainter(FSingleCaretClass);
    FSingleCaretClass := nil;
  end
  else begin
    // store current class
    if FSingleCaretClass = nil then
      FSingleCaretClass := TSynEditScreenCaretPainterClass(ScreenCaret.Painter.ClassType);
    if ScreenCaret.Painter.ClassType <> TSynEditScreenCaretPainterInternal then
      ScreenCaret.ChangePainter(TSynEditScreenCaretPainterInternal);
  end;
end;

procedure TSynPluginMultiCaretBase.DoTextSizeChanged(Sender: TObject);
var
  i: Integer;
  ta: TLazSynTextArea;
begin
  if FPaintLock > 0 then begin
    include(FPaintLockFlags, plfTextSizeChanged);
    exit;
  end;

  ta := TextArea;
  for i := 0 to FUsedList.Count - 1 do begin
    FUsedList[i].CharHeight := ta.LineHeight  - Max(0, ta.ExtraLineSpacing);
    FUsedList[i].CharWidth  := ta.CharWidth;
  end;
  UpdateCaretsPos;
end;

procedure TSynPluginMultiCaretBase.DoEditorPaintEvent(Sender: TObject;
  EventType: TSynPaintEvent; const prcClip: TRect);
var
  i: Integer;
begin
  if EventType = peAfterPaint then
    UpdateCaretsPos;

  case EventType of
    peBeforePaint:
      begin
        FInPaint := True;
        FPaintClip := prcClip;
        for i := 0 to FUsedList.Count - 1 do
          FUsedList[i].BeginPaint(prcClip);
        for i := 0 to FUnUsedList.Count - 1 do
          FUnUsedList[i].BeginPaint(prcClip);
      end;
    peAfterPaint:
      begin
        FInPaint := False;
        for i := 0 to FUsedList.Count - 1 do
          FUsedList[i].FinishPaint(prcClip);
        for i := 0 to FUnUsedList.Count - 1 do
          FUnUsedList[i].FinishPaint(prcClip);
      end;
  end;
end;

procedure TSynPluginMultiCaretBase.DoEditorScrollEvent(Sender: TObject;
  EventType: TSynScrollEvent; dx, dy: Integer; const prcScroll, prcClip: TRect);
var
  i: Integer;
begin
  case EventType of
    peBeforeScroll:
      for i := 0 to FUsedList.Count - 1 do
        FUsedList[i].BeginScroll(dx, dy, prcScroll, prcClip);
    peAfterScroll:
      for i := 0 to FUsedList.Count - 1 do
        FUsedList[i].FinishScroll(dx, dy, prcScroll, prcClip, True);
    peAfterScrollFailed:
      for i := 0 to FUsedList.Count - 1 do
        FUsedList[i].FinishScroll(dx, dy, prcScroll, prcClip, False);
  end;

  if EventType = peAfterScroll then
    UpdateCaretsPos;
end;

procedure TSynPluginMultiCaretBase.DoEditorStatusChanged(Sender: TObject;
  Changes: TSynStatusChanges);
var
  i: Integer;
  v: Boolean;
begin
  if scFocus in Changes then begin
    v := (Editor.Focused or (eoPersistentCaret in Editor.Options)) and not (eoNoCaret in Editor.Options);
    for i := 0 to FUsedList.Count - 1 do
      FUsedList[i].Visible := v;
  end;
  if scInsertMode in Changes then
    for i := 0 to FUsedList.Count - 1 do
      if Editor.InsertMode
      then FUsedList[i].DisplayType := Editor.InsertCaret
      else FUsedList[i].DisplayType := Editor.OverwriteCaret;
  if scOptions in Changes then begin
    for i := 0 to FUsedList.Count - 1 do begin
      if Editor.InsertMode
      then FUsedList[i].DisplayType := Editor.InsertCaret
      else FUsedList[i].DisplayType := Editor.OverwriteCaret;
      UpdateCaretsPos;
    end;
  end;
end;

procedure TSynPluginMultiCaretBase.DoAfterDecPaintLock(Sender: TObject);
begin
  if FPaintLock > 0 then
    Dec(FPaintLock);
  if FPaintLock > 0 then
    exit;

  Include(FPaintLockFlags, plfDeferUpdateCaretsPos);
  if plfBoundsChanged in FPaintLockFlags then
    DoBoundsChanged(nil);
  if plfTextSizeChanged in FPaintLockFlags then
    DoTextSizeChanged(nil);
  if plfMergeCarets in FPaintLockFlags then
    MergeAndRemoveCarets;
  Exclude(FPaintLockFlags, plfDeferUpdateCaretsPos);
  if plfUpdateCaretsPos in FPaintLockFlags then
    UpdateCaretsPos;
  FPaintLockFlags := [];

  ScreenCaret.UnLock; // unlock timer
end;

procedure TSynPluginMultiCaretBase.DoBeforeIncPaintLock(Sender: TObject);
begin
  inc(FPaintLock);

  if FPaintLock = 1 then
    ScreenCaret.Lock; // lock timer
end;

function TSynPluginMultiCaretBase.GetTextArea: TLazSynTextArea;
begin
  Result := TLazSynSurfaceManager(PaintArea).TextArea;
end;

function TSynPluginMultiCaretBase.AddCaret(X, Y, Offs: Integer; flags: TCaretFlags;
  PhysX: Integer): Integer;
var
  y1, y2: Integer;
begin
  Result := Carets.AddCaret(x,y, Offs, flags, PhysX);
  UpdateMainCaret;

  if cfNoneVisual in flags then
    exit;

  if FPaintLock > 0 then begin
    UpdateCaretsPos;
    exit;
  end;

  if (eoNoCaret in Editor.Options) then begin
    Carets.Visual[Result] := nil;
    exit;
  end;

  y1 := Editor.RowToScreenRow(y);
  if (y1 < 0) or (y1 > Editor.LinesInWindow + 1) then
    y := -1; // not visible
  if y > 1 then
    y2 := Editor.RowToScreenRow(y-1)
  else
    y2 := -1;

  if (y > 0) and (y1 <> y2) or (y=1) then begin
    if Carets.Visual[Result] = nil then
      Carets.Visual[Result] := GetVisual;
    x := ViewedTextBuffer.LogPhysConvertor.LogicalToPhysical(ToIdx(y), x, Offs); // TODO: check if offs was adjusted? But should not happen for NEW caret
    Carets.Visual[Result].DisplayPos := TextArea.RowColumnToPixels(Point(x, y1));
    Carets.Visual[Result].Visible := (eoPersistentCaret in Editor.Options) or Editor.Focused;
  end
  else
    Carets.Visual[Result] := nil;
end;

procedure TSynPluginMultiCaretBase.RemoveCaret(Index: Integer);
begin
  Carets.RemoveCaret(Index);
  UpdateMainCaret;
end;

procedure TSynPluginMultiCaretBase.UpdateCaretsPos;
var
  i, x, y, o, w: Integer;
  y1, y2: Integer;
  vis: Boolean;
begin
  if plfDeferUpdateCaretsPos in FPaintLockFlags then exit;
  if FPaintLock > 0 then begin
    include(FPaintLockFlags, plfUpdateCaretsPos);
    exit;
  end;
  if (eoNoCaret in Editor.Options) then begin
    for i := 0 to CaretsCount - 1 do
      Carets.Visual[i] := nil;
    exit;
  end;

  vis := (eoPersistentCaret in Editor.Options) or Editor.Focused;

  w := Editor.LinesInWindow + 1;
  for i := 0 to CaretsCount - 1 do begin
    if cfNoneVisual in Carets.Flags[i] then continue;

    x := Carets.CaretX[i];
    y := Carets.CaretY[i];
    o := Carets.CaretOffs[i];
    y1 := Editor.RowToScreenRow(y);
    if (y1 < 0) or (y1 > w) then begin
      Carets.Visual[i] := nil;
      continue;
    end;

    if y > 1 then
      y2 := Editor.RowToScreenRow(y-1);

    if (y1 <> y2) or (y=1) then begin
      if Carets.Visual[i] = nil then
        Carets.Visual[i] := GetVisual;
      x := ViewedTextBuffer.LogPhysConvertor.LogicalToPhysical(ToIdx(y), x, o);
      Carets.Visual[i].Lock;
      Carets.Visual[i].DisplayPos := TextArea.RowColumnToPixels(Point(x, y1));
      Carets.Visual[i].Visible := vis;
      Carets.Visual[i].UnLock;
//todo: remove if duplicate
      // check if offs was adjusted
      //if o <> Carets.CaretOffs[i] then
      //  Carets.CaretOffs[i] := o;
    end
    else
      Carets.Visual[i] := nil;
  end;
end;

procedure TSynPluginMultiCaretBase.ClearCarets;
begin
  Carets.Clear(True);
  UpdateMainCaret;
  FUsedList.Clear;
  FUnUsedList.Clear;
  DoCleared;
end;

function TSynPluginMultiCaretBase.CaretsCount: Integer;
begin
  Result := Carets.Count;
end;

procedure TSynPluginMultiCaretBase.DoCleared;
begin
  //
end;

procedure TSynPluginMultiCaretBase.DoBufferChanged(Sender: TObject);
begin
  TSynEditStrings(Sender).RemoveNotifyHandler(senrAfterDecPaintLock, @DoAfterDecPaintLock);
  TSynEditStrings(Sender).RemoveNotifyHandler(senrBeforeIncPaintLock, @DoBeforeIncPaintLock);
  TSynEditStrings(Sender).RemoveEditHandler(@DoLinesEdited);
  ViewedTextBuffer.AddEditHandler(@DoLinesEdited);
  ViewedTextBuffer.AddNotifyHandler(senrBeforeIncPaintLock, @DoBeforeIncPaintLock);
  ViewedTextBuffer.AddNotifyHandler(senrAfterDecPaintLock, @DoAfterDecPaintLock);
end;

procedure TSynPluginMultiCaretBase.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  inherited DoEditorRemoving(AValue);
  if Editor <> nil then begin
    ClearCarets;
    TextArea.RemoveBoundsChangeHandler(@DoBoundsChanged);
    TextArea.RemoveTextSizeChangeHandler(@DoTextSizeChanged);
    Editor.UnRegisterStatusChangedHandler(@DoEditorStatusChanged);
    Editor.UnRegisterScrollEventHandler(@DoEditorScrollEvent);
    Editor.UnRegisterPaintEventHandler(@DoEditorPaintEvent);
    ViewedTextBuffer.RemoveNotifyHandler(senrAfterDecPaintLock, @DoAfterDecPaintLock);
    ViewedTextBuffer.RemoveNotifyHandler(senrBeforeIncPaintLock, @DoBeforeIncPaintLock);
    ViewedTextBuffer.RemoveEditHandler(@DoLinesEdited);
    ViewedTextBuffer.RemoveNotifyHandler(senrTextBufferChanged, @DoBufferChanged);
  end;
end;

procedure TSynPluginMultiCaretBase.DoEditorAdded(AValue: TCustomSynEdit);
begin
  if Editor <> nil then begin
    ViewedTextBuffer.AddNotifyHandler(senrTextBufferChanged, @DoBufferChanged);
    ViewedTextBuffer.AddEditHandler(@DoLinesEdited);
    ViewedTextBuffer.AddNotifyHandler(senrBeforeIncPaintLock, @DoBeforeIncPaintLock);
    ViewedTextBuffer.AddNotifyHandler(senrAfterDecPaintLock, @DoAfterDecPaintLock);
    Editor.RegisterPaintEventHandler(@DoEditorPaintEvent, [peBeforePaint, peAfterPaint]);
    Editor.RegisterScrollEventHandler(@DoEditorScrollEvent, [peBeforeScroll, peAfterScroll, peAfterScrollFailed]);
    Editor.RegisterStatusChangedHandler(@DoEditorStatusChanged, [scInsertMode, scFocus, scOptions]);
    TextArea.AddTextSizeChangeHandler(@DoTextSizeChanged);
    TextArea.AddBoundsChangeHandler(@DoBoundsChanged);

    FSingleCaretClass := TSynEditScreenCaretPainterClass(ScreenCaret.Painter.ClassType);
    UpdateMainCaret;
  end;
  inherited DoEditorAdded(AValue);
end;

constructor TSynPluginMultiCaretBase.Create(AOwner: TComponent);
begin
  FRestoreSingleCaretPainter := True;
  inherited Create(AOwner);
  FColor := clBlack;
  FCarets := TSynPluginMultiCaretList.Create;
  FUsedList := TSynPluginMultiCaretVisualList.Create;
  FUnUsedList := TSynPluginMultiCaretVisualList.Create;
end;

destructor TSynPluginMultiCaretBase.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FCarets);
  FreeAndNil(FUsedList);
  FreeAndNil(FUnUsedList);
end;

procedure TSynPluginMultiCaretBase.SetCaretTypeSize(AType: TSynCaretType; AWidth, AHeight,
  AXOffs, AYOffs: Integer; AFlags: TSynCustomCaretSizeFlags);
var
  i: Integer;
begin
  FCustomPixelWidth[AType] := AWidth;
  FCustomPixelHeight[AType] := AHeight;
  FCustomOffsetX[AType] := AXOffs;
  FCustomOffsetY[AType] := AYOffs;
  FCustomFlags[AType] := AFlags;

  for i := 0 to FUsedList.Count - 1 do
    FUsedList[i].SetCaretTypeSize(AType, AWidth, AHeight, AXOffs, AYOffs, AFlags)
end;

{ TSynPluginMultiCaretMouseActions }

procedure TSynPluginMultiCaretMouseActions.ResetDefaults;
begin
  Clear;
  AddCommand(emcPluginMultiCaretToggleCaret, False, mbXLeft, ccAny, cdDown, [ssShift, ssCtrl], [ssShift,ssCtrl,ssAlt]);
end;

{ TSynPluginMultiCaretKeyStrokes }

procedure TSynPluginMultiCaretKeyStrokes.ResetDefaults;
  procedure AddKey(const ACmd: TSynEditorCommand; const AKey: word;
     const AShift: TShiftState; const AShiftMask: TShiftState = []);
  begin
    with Add do
    begin
      Key       := AKey;
      Shift     := AShift;
      ShiftMask := AShiftMask;
      Command   := ACmd;
    end;
  end;
begin
  inherited ResetDefaults;
  AddKey(ecPluginMultiCaretToggleCaret, VK_SPACE, [ssShift, ssCtrl], [ssShift,ssCtrl,ssAlt]);
  AddKey(ecPluginMultiCaretClearAll, VK_ESCAPE, [ssShift, ssCtrl], [ssShift,ssCtrl,ssAlt]);
end;

{ TSynEditUndoMultiCaret }

function TSynEditUndoMultiCaret.IsEqualContent(AnItem: TSynEditUndoItem): Boolean;
begin
  Result := (FCaretUndoItem = nil) or
            FCaretUndoItem.IsEqual(TSynEditUndoMultiCaret(AnItem).FCaretUndoItem);
  Result := Result and
            (FActiveMode = TSynEditUndoMultiCaret(AnItem).FActiveMode) and
            (Length(FMultiCaretList) = Length(TSynEditUndoMultiCaret(AnItem).FMultiCaretList));
  if Result then
    Result := 0 = CompareByte(FMultiCaretList[0], TSynEditUndoMultiCaret(AnItem).FMultiCaretList[0],
      Length(FMultiCaretList)*SizeOf(FMultiCaretList[0]));
end;

function TSynEditUndoMultiCaret.DebugString: String;
begin
  Result := 'TSynEditUndoMultiCaret '+IntToStr(Length(FMultiCaretList));
  //if FCaretUndoItem <> nil then
  //  Result := Result + ' / ' + FCaretUndoItem.DebugString;
end;

constructor TSynEditUndoMultiCaret.Create(ACaretUndoItem: TSynEditUndoItem;
  ABeginBlock: Boolean);
begin
  FBeginBlock := ABeginBlock;
  FCaretUndoItem := ACaretUndoItem;
end;

destructor TSynEditUndoMultiCaret.Destroy;
begin
  FCaretUndoItem.Free;
  inherited Destroy;
end;

constructor TSynEditUndoMultiCaret.AddCaretsFrom(AList: TSynPluginMultiCaretList);
var
  i, j: Integer;
begin
  SetLength(FMultiCaretList, AList.Count);
  j := 0;
  for i := 0 to AList.Count-1 do
    if not (cfNoneVisual in AList.Flags[i]) then begin
      FMultiCaretList[j] := AList.CaretFull[i];
      inc(j);
    end;
  SetLength(FMultiCaretList, j);
end;

function TSynEditUndoMultiCaret.IsCaretInfo: Boolean;
begin
  Result := True;
end;

function TSynEditUndoMultiCaret.PerformUndo(Caller: TObject): Boolean;
var
  C: TSynCustomPluginMultiCaret;
  AnRedoItem: TSynEditUndoMultiCaret;
  UList: TSynEditUndoList;
begin
  Result := Caller is TSynCustomPluginMultiCaret;
  if not Result then exit;
  C := TSynCustomPluginMultiCaret(Caller);
  Result := (FCaretUndoItem <> nil) and FCaretUndoItem.PerformUndo(C.Editor);
  if Result then begin
    if FBeginBlock then begin
      C.Carets.ImportFromSortedList(FMultiCaretList);
      C.ActiveMode := ActiveMode;
      C.UpdateCaretsPos;
      C.AddStateFlags([sfSkipSelChanged, sfSkipCaretChanged], True);
    end;
    // redo
    UList := C.ViewedTextBuffer.CurUndoList;
    if UList.CurrentGroup = nil then exit; // should never happen / just added the caret.
    AnRedoItem := TSynEditUndoMultiCaret.Create(UList.CurrentGroup.Pop, not FBeginBlock);
    AnRedoItem.FMultiCaretList := FMultiCaretList;
    AnRedoItem.ActiveMode := ActiveMode;
    UList.AddChange(AnRedoItem);
  end;
end;

{ TSynCustomPluginMultiCaret }

procedure TSynCustomPluginMultiCaret.TranslateKey(Sender: TObject; Code: word;
  SState: TShiftState; var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; FinishComboOnly: Boolean;
  var ComboKeyStrokes: TSynEditKeyStrokes);
begin
  if Handled then
  exit;
  if not FinishComboOnly then
    FKeyStrokes.ResetKeyCombo;
  Command := FKeyStrokes.FindKeycodeEx(Code, SState, Data, IsStartOfCombo, FinishComboOnly, ComboKeyStrokes);
  if (Command = ecPluginMultiCaretClearAll) and (FCarets.Count = 0) then
    Command := ecNone;

  Handled := (Command <> ecNone) or IsStartOfCombo;
end;

procedure TSynCustomPluginMultiCaret.RemoveCaretsInSelection;
var
  i, x, y: Integer;
  bb, be: TPoint;
  sm: TSynSelectionMode;
begin
  bb := SelectionObj.FirstLineBytePos;
  be := SelectionObj.LastLineBytePos;
  sm := SelectionObj.ActiveSelectionMode;
  if sm = smLine then begin
    bb.x := 0;
    be.x := MaxInt;
  end;
  if (sm = smColumn) and (bb.x > be.x) then begin
    if bb.x = be.x then
      exit;
    i    := bb.x;
    bb.x := be.x;
    be.x := i;
  end;

  i := CaretsCount;
  while i > 0 do begin
    dec(i);
    x := Carets.Caret[i].x;
    y := Carets.Caret[i].y;
    if (y < bb.y) or
       (y > be.y) or
       (  ((y = bb.y) or (sm = smColumn)) and (x <= bb.x)  ) or
       (  ((y = be.y) or (sm = smColumn)) and (x >= be.x)  )
    then
      Continue;
    RemoveCaret(i);
  end;
end;

function TSynCustomPluginMultiCaret.GetIsInMainExecution: Boolean;
begin
  Result := sfProcessingMain in FStateFlags;
end;

function TSynCustomPluginMultiCaret.GetIsInRepeatExecution: Boolean;
begin
  Result := sfProcessingRepeat in FStateFlags;
end;

procedure TSynCustomPluginMultiCaret.SetActiveMode(AValue: TSynPluginMultiCaretMode);
begin
  if FActiveMode = AValue then Exit;
  FActiveMode := AValue;
  if FActiveMode = mcmNoCarets then begin
    ClearCarets;
    UnLockSpaceTrimmer;
  end
  else
    LockSpaceTrimmer;
end;

procedure TSynCustomPluginMultiCaret.SetDefaultColumnSelectMode(AValue: TSynPluginMultiCaretDefaultMode);
begin
  if FDefaultColumnSelectMode = AValue then Exit;
  FDefaultColumnSelectMode := AValue;
end;

procedure TSynCustomPluginMultiCaret.SetDefaultMode(AValue: TSynPluginMultiCaretDefaultMode);
begin
  if FDefaultMode = AValue then Exit;
  FDefaultMode := AValue;
end;

procedure TSynCustomPluginMultiCaret.SetSkipCaretAtSel;
begin
  Include(FStateFlags, sfSkipCaretsAtSelection);
  FSelY1 := SelectionObj.FirstLineBytePos.y;
  FSelY2 := SelectionObj.LastLineBytePos.y;
  FSelX  := SelectionObj.FirstLineBytePos.x;
end;

procedure TSynCustomPluginMultiCaret.UpdateCaretForUndo(var AnUndoItem: TSynEditUndoItem;
  AnIsBeginUndo: Boolean);
begin
  if (FStateFlags * [sfProcessingCmd, sfSkipUndoCarets] =  [sfProcessingCmd]) and // active edit
     (CaretsCount > 0)
  then begin
    AnUndoItem := TSynEditUndoMultiCaret.Create(AnUndoItem, AnIsBeginUndo);
    TSynEditUndoMultiCaret(AnUndoItem).AddCaretsFrom(Carets);
    TSynEditUndoMultiCaret(AnUndoItem).ActiveMode := ActiveMode;
  end;
end;

function TSynCustomPluginMultiCaret.HandleUndoRedoItem(Caller: TObject;
  Item: TSynEditUndoItem): Boolean;
begin
  Result := Caller = Editor;
  if not Result then exit;
  Result := Item.PerformUndo(Self);
end;

procedure TSynCustomPluginMultiCaret.LockSpaceTrimmer;
var
  b: TSynEditStrings;
begin
  if FSpaceTrimmerLocked then exit;
  FSpaceTrimmerLocked := True;
  b := ViewedTextBuffer;
  while b <> nil do begin
    if b is TSynEditStringTrimmingList then TSynEditStringTrimmingList(b).Lock;
    if b is TSynEditStringsLinked then
      b := TSynEditStringsLinked(b).NextLines
    else
      b := nil;
  end;
end;

procedure TSynCustomPluginMultiCaret.UnLockSpaceTrimmer;
var
  b: TSynEditStrings;
begin
  if not FSpaceTrimmerLocked then exit;
  FSpaceTrimmerLocked := False;
  b := ViewedTextBuffer;
  while b <> nil do begin
    if b is TSynEditStringTrimmingList then TSynEditStringTrimmingList(b).UnLock;
    if b is TSynEditStringsLinked then
      b := TSynEditStringsLinked(b).NextLines
    else
      b := nil;
  end;
end;

function TSynCustomPluginMultiCaret.LogPhysConvertor: TSynLogicalPhysicalConvertor;
begin
  Result := ViewedTextBuffer.LogPhysConvertor;
end;

function TSynCustomPluginMultiCaret.PhysicalToLogical(AIndex, AColumn: Integer; out
  AColOffset: Integer; ACharSide: TSynPhysCharSide; AFlags: TSynLogPhysFlags): Integer;
var
  s: String;
begin
  Result := LogPhysConvertor.PhysicalToLogical(AIndex, AColumn, AColOffset, ACharSide, AFlags);
  if (AColOffset > 0) then begin
    if (eoCaretSkipTab in Editor.Options2) then
      AColOffset := 0
    else
    begin
      s := ViewedTextBuffer[AIndex];
      if (Result > Length(s)) or (s[Result] <> #9) then
        AColOffset := 0;
    end;
  end;
end;

procedure TSynCustomPluginMultiCaret.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  if Editor <> nil then begin
    ViewedTextBuffer.RemoveNotifyHandler(senrDecOwnedPaintLock, @DoDecForeignPaintLock);
    ViewedTextBuffer.RemoveNotifyHandler(senrIncOwnedPaintLock, @DoIncForeignPaintLock);
    ViewedTextBuffer.UndoList.UnregisterUpdateCaretUndo(@UpdateCaretForUndo);
    CaretObj.RemoveChangeHandler(@DoCaretChanged);
    SelectionObj.RemoveChangeHandler(@DoSelectionChanged);
    Editor.UnregisterCommandHandler(@ProcessAllSynCommand);
    Editor.UnregisterCommandHandler(@ProcessMySynCommand);
    Editor.UnRegisterKeyTranslationHandler(@TranslateKey);
    Editor.UnregisterMouseActionSearchHandler(@MaybeHandleMouseAction);
    Editor.UnregisterMouseActionExecHandler(@DoHandleMouseAction);
    Editor.UnRegisterUndoRedoItemHandler(@HandleUndoRedoItem);
  end;
  inherited DoEditorRemoving(AValue);
end;

procedure TSynCustomPluginMultiCaret.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  if Editor <> nil then begin
    Editor.RegisterUndoRedoItemHandler(@HandleUndoRedoItem);
    Editor.RegisterMouseActionSearchHandler(@MaybeHandleMouseAction);
    Editor.RegisterMouseActionExecHandler(@DoHandleMouseAction);
    Editor.RegisterCommandHandler(@ProcessAllSynCommand, nil, [hcfInit, hcfFinish]);
    Editor.RegisterCommandHandler(@ProcessMySynCommand, nil, [hcfPreExec]);
    Editor.RegisterKeyTranslationHandler(@TranslateKey);
    SelectionObj.AddChangeHandler(@DoSelectionChanged);
    CaretObj.AddChangeHandler(@DoCaretChanged);
    ViewedTextBuffer.UndoList.RegisterUpdateCaretUndo(@UpdateCaretForUndo);
    ViewedTextBuffer.AddNotifyHandler(senrIncOwnedPaintLock, @DoIncForeignPaintLock);
    ViewedTextBuffer.AddNotifyHandler(senrDecOwnedPaintLock, @DoDecForeignPaintLock);
  end;
end;

procedure TSynCustomPluginMultiCaret.DoBufferChanged(Sender: TObject);
begin
  inherited DoBufferChanged(Sender);
  TSynEditStrings(Sender).RemoveNotifyHandler(senrDecOwnedPaintLock, @DoDecForeignPaintLock);
  TSynEditStrings(Sender).RemoveNotifyHandler(senrIncOwnedPaintLock, @DoIncForeignPaintLock);
  TSynEditStrings(Sender).UndoList.UnregisterUpdateCaretUndo(@UpdateCaretForUndo);
  ViewedTextBuffer.UndoList.RegisterUpdateCaretUndo(@UpdateCaretForUndo);
  ViewedTextBuffer.AddNotifyHandler(senrIncOwnedPaintLock, @DoIncForeignPaintLock);
  ViewedTextBuffer.AddNotifyHandler(senrDecOwnedPaintLock, @DoDecForeignPaintLock);
end;

procedure TSynCustomPluginMultiCaret.DoAfterDecPaintLock(Sender: TObject);
begin
  if FPaintLock > 1 then begin
    inherited DoAfterDecPaintLock(Sender);
    exit;
  end;

  UpdateCaretsPos;
  inherited DoAfterDecPaintLock(Sender);
  FStateFlags := FStateFlags - [sfExtendingColumnSel, sfSkipSelChanged, sfSkipCaretChanged];
end;

procedure TSynCustomPluginMultiCaret.DoIncForeignPaintLock(Sender: TObject);
begin
  if Sender = Editor then exit;
  inc(FForeignPaintLock);
end;

procedure TSynCustomPluginMultiCaret.DoDecForeignPaintLock(Sender: TObject);
begin
  if Sender = Editor then exit;
  dec(FForeignPaintLock);
end;

procedure TSynCustomPluginMultiCaret.DoCleared;
begin
  inherited DoCleared;
  ActiveMode := mcmNoCarets;
  Exclude(FStateFlags, sfCreateCaretAtCurrentPos);
  FColSelDoneY1 := -1;
  FColSelDoneY2 := -2;
  FColSelDonePhysX := -1;
end;

procedure TSynCustomPluginMultiCaret.DoLinesEdited(Sender: TSynEditStrings; aLinePos,
  aBytePos, aCount, aLineBrkCnt: Integer; aText: String);
begin
  if (FStateFlags * [sfProcessingCmd] = []) and
     (FForeignPaintLock = 0)
  then
    ClearCarets;


  inherited DoLinesEdited(Sender, aLinePos, aBytePos, aCount, aLineBrkCnt, aText);
  FStateFlags := FStateFlags - [sfCreateCaretAtCurrentPos, sfSkipCaretsAtSelection, sfNoChangeIndicator];
end;

procedure TSynCustomPluginMultiCaret.DoCaretChanged(Sender: TObject);
var
  p: TLogCaretPoint;
begin
  Exclude(FStateFlags, sfNoChangeIndicator);
  if (sfCreateCaretAtCurrentPos in FStateFlags) then begin
    p := CaretObj.OldFullLogicalPos;
    AddCaret(p.x, p.y, p.Offs);
    exclude(FStateFlags, sfCreateCaretAtCurrentPos);
    exit;
  end;
  if (FStateFlags * [sfProcessingCmd, sfExtendingColumnSel, sfSkipCaretChanged] <> []) or
     (ActiveMode = mcmAddingCarets) or
     (FForeignPaintLock > 0)
  then
    exit;

  ClearCarets;
end;

procedure TSynCustomPluginMultiCaret.DoSelectionChanged(Sender: TObject);
  procedure AddCarets(StartY, EndY, PhysX: Integer);
  var
    i, XLog, Offs: Integer;
    CurCar: TLogCaretPoint;
  begin
    i:= -1;
    CurCar.Y := -1;
    while StartY <= EndY do begin
      XLog := PhysicalToLogical(ToIdx(StartY), PhysX, Offs);
      if StartY >= CurCar.Y then begin
        i := Carets.FindEqOrNextCaretIdx(XLog, StartY, Offs, i+1);
        if i >= 0 then
          CurCar := Carets.CaretFull[i];
      end;
      if (CurCar.x <> XLog) or (CurCar.Offs <> Offs) or (CurCar.y <> StartY) then
        AddCaret(XLog, StartY, Offs, [], PhysX); // TODO: pass "i-1" as KnowIndexOfCaretBefore (limit bin search)
      inc(StartY);
    end;
  end;
  procedure RemoveCarets(StartY, EndY, PhysX: Integer);
  var
    i, XLog, Offs: Integer;
  begin
    XLog := PhysicalToLogical(ToIdx(StartY), PhysX, Offs);
    i := Carets.FindEqOrNextCaretIdx(XLog, StartY, Offs);
    if i >= 0 then begin
      while Carets.CaretY[i] <= EndY do begin
        if (Carets.CaretX[i] = XLog) and (Carets.CaretOffs[i] = Offs) then
          RemoveCaret(i)
        else
          inc(i);
        if i >= CaretsCount then
          break;
        if StartY <> Carets.CaretY[i] then begin
          StartY := Carets.CaretY[i];
          XLog := PhysicalToLogical(ToIdx(StartY), PhysX, Offs);
        end;
      end;
    end;
  end;
var
  i: Integer;
  XPhys, XLog, Offs: Integer;
  SelFirstY, SelLastY, CurY: Integer;
  CurCaret: TLogCaretPoint;
begin
  Exclude(FStateFlags, sfNoChangeIndicator);
  if (FStateFlags * [sfProcessingCmd, sfSkipSelChanged] <> []) or
     (FForeignPaintLock > 0)
  then exit;
  SelFirstY := Editor.BlockBegin.y;
  SelLastY := Editor.BlockEnd.y;
  If not ((SelFirstY <> SelLastY) and (Editor.SelectionMode = smColumn) and EnableWithColumnSelection) then begin
    ClearCarets;
    exit;
  end;


  Include(FStateFlags, sfExtendingColumnSel);
  if SelFirstY = CaretObj.LinePos then inc(SelFirstY);
  if SelLastY = CaretObj.LinePos then dec(SelLastY);

  if (FColSelDoneY2 >= FColSelDoneY1) then begin
    // Delete carets at top, that are no longer in selection
    if SelFirstY > FColSelDoneY1 then begin
      RemoveCarets(FColSelDoneY1, SelFirstY - 1, FColSelDonePhysX);
      FColSelDoneY1 := SelFirstY;
    end;
    // Delete carets at bottom, that are no longer in selection
    if SelLastY < FColSelDoneY2 then begin
      RemoveCarets(SelLastY + 1, FColSelDoneY2, FColSelDonePhysX);
      FColSelDoneY2 := SelLastY;
    end;
  end;

  XPhys := Editor.CaretX;
  if (FColSelDoneY2 >= FColSelDoneY1) and (XPhys <> FColSelDonePhysX) then begin
    // Move carets X
    CurY := FColSelDoneY1;
    XLog := PhysicalToLogical(ToIdx(CurY), FColSelDonePhysX, Offs);
    i := Carets.FindEqOrNextCaretIdx(XLog, CurY, Offs);
    if i >= 0 then begin
      while True do begin
        CurCaret := Carets.CaretFull[i];
        if CurCaret.Y > FColSelDoneY2 then
          break;
        if (CurCaret.X = XLog) and (CurCaret.Offs = Offs) then begin
          CurCaret.X := PhysicalToLogical(ToIdx(CurCaret.Y), XPhys, CurCaret.Offs);
          Carets.CaretFull[i] := CurCaret;
          Carets.CaretKeepX[i] := XPhys;
        end;
        inc(i);
        if i >= CaretsCount then
          break;
        if CurY <> Carets.CaretY[i] then begin
          CurY := Carets.CaretY[i];
          XLog := PhysicalToLogical(ToIdx(CurY), FColSelDonePhysX, Offs);
        end;
      end;
    end;
    FColSelDonePhysX := XPhys;
  end;

  if (FColSelDoneY2 < FColSelDoneY1) then begin
    // New Selection
    AddCarets(SelFirstY, SelLastY, XPhys);
    FColSelDoneY1 := SelFirstY;
    FColSelDoneY2 := SelLastY;
    FColSelDonePhysX := XPhys;
  end
  else
  begin
    // Extend
    if SelFirstY < FColSelDoneY1 then begin
      AddCarets(SelFirstY, FColSelDoneY1 - 1, FColSelDonePhysX);
      FColSelDoneY1 := SelFirstY;
    end;
    if SelLastY > FColSelDoneY2 then begin
      AddCarets(FColSelDoneY2 + 1, SelLastY, FColSelDonePhysX);
      FColSelDoneY2 := SelLastY;
    end;
  end;

  i := Carets.FindCaretIdx(CaretObj.BytePos, CaretObj.LinePos, CaretObj.BytePosOffset);
  if i >= 0 then
    RemoveCaret(i);

  if ActiveMode = mcmNoCarets then
    ActiveMode := DefaultColumnSelectMode;
end;

procedure TSynCustomPluginMultiCaret.DoBeforeSetSelText(Sender: TObject; AMode: TSynSelectionMode;
  ANewText: PChar);
var
  skip: Boolean;
begin
  SelectionObj.RemoveBeforeSetSelTextHandler(@DoBeforeSetSelText);

  // only here if selectionexists and is smColumn;
  skip := //Editor.SelAvail and (SelectionObj.ActiveSelectionMode = smColumn) and
     not(eoPersistentBlock in Editor.Options2);
  if skip then
    SetSkipCaretAtSel;

  RemoveCaretsInSelection;
  SelectionObj.SelText := '';

  if Carets.MainCaretIndex >= 0 then begin
    Editor.LogicalCaretXY := Carets.Caret[Carets.MainCaretIndex];
    FSelX := Carets.Caret[Carets.MainCaretIndex].x;
  end
  else
    assert(False, 'TSynCustomPluginMultiCaret.ProcessAllSynCommand: Maincaret index not found');

  if skip then
    Include(FStateFlags, sfSkipCaretsAtSelection); // restore the flag
end;

procedure TSynCustomPluginMultiCaret.ProcessMySynCommand(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean; var Command: TSynEditorCommand;
  var AChar: TUTF8Char; Data: pointer; HandlerData: pointer);
var
  i: Integer;
begin
  // hcfPreExec
  if Handled then exit;

  Handled := True;
  case Command of
    ecPluginMultiCaretSetCaret: begin
        if Carets.FindCaretIdx(CaretObj.BytePos, CaretObj.LinePos, CaretObj.BytePosOffset) < 0 then
          include(FStateFlags, sfCreateCaretAtCurrentPos);
        ActiveMode := mcmAddingCarets;
      end;
    ecPluginMultiCaretUnsetCaret: begin
        exclude(FStateFlags, sfCreateCaretAtCurrentPos);
        i := Carets.FindCaretIdx(CaretObj.BytePos, CaretObj.LinePos, CaretObj.BytePosOffset);
        if i >= 0 then
          RemoveCaret(i);
        ActiveMode := mcmAddingCarets;
      end;
    ecPluginMultiCaretToggleCaret: begin
        i := Carets.FindCaretIdx(CaretObj.BytePos, CaretObj.LinePos, CaretObj.BytePosOffset);
        if (i >= 0) or (sfCreateCaretAtCurrentPos in FStateFlags) then begin
          exclude(FStateFlags, sfCreateCaretAtCurrentPos);
          if i >= 0 then
            RemoveCaret(i);
        end
        else begin
          include(FStateFlags, sfCreateCaretAtCurrentPos);
        end;
        ActiveMode := mcmAddingCarets;
      end;
    ecPluginMultiCaretClearAll: begin
      Handled := FCarets.Count > 0;
      ClearCarets;
      if not SelectionObj.SelAvail then
        SelectionObj.Clear; // clear invisibel selection
    end;

    ecPluginMultiCaretModeCancelOnMove: ActiveMode := mcmCancelOnCaretMove;
    ecPluginMultiCaretModeMoveAll:      ActiveMode := mcmMoveAllCarets;
    else
      Handled := False;
  end;
end;

procedure TSynCustomPluginMultiCaret.ProcessAllSynCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);

  procedure ExecCommandRepeated(AOnePerLine: Boolean = False; AForceAll: Boolean = False);
  var
    i, y: Integer;
    p: TLogCaretPoint;
    skip, noChange, SelAvail, IsUser: Boolean;
    MainY: Integer;
  begin
    Handled := True;
    Editor.BeginUpdate(True);
    FCarets.IncMergeLock;
    try
      AddCaret(Editor.LogicalCaretXY.x, Editor.CaretY, CaretObj.BytePosOffset,
        [cfMainCaret, cfNoneVisual {, cfAddDuplicate}], CaretObj.KeepCaretXPos);

      // Execute Command at current caret pos
      Include(FStateFlags, sfProcessingMain);
      Include(FStateFlags, sfNoChangeIndicator);
      if Editor.SelAvail and (SelectionObj.ActiveSelectionMode = smColumn) then
        SelectionObj.AddBeforeSetSelTextHandler(@DoBeforeSetSelText);
      Editor.CommandProcessor(Command, AChar, data, [hcfInit, hcfFinish]);
      SelectionObj.RemoveBeforeSetSelTextHandler(@DoBeforeSetSelText);
      Exclude(FStateFlags, sfProcessingMain);
      noChange := sfNoChangeIndicator in FStateFlags;
      Exclude(FStateFlags, sfNoChangeIndicator);

      if noChange and not AForceAll then begin
        if Carets.MainCaretIndex >= 0 then
          RemoveCaret(Carets.MainCaretIndex)
        else
          assert(False, 'TSynCustomPluginMultiCaret.ProcessAllSynCommand: Maincaret index not found');
        exit;
      end;

      // Repeat command
      Include(FStateFlags, sfProcessingRepeat);
      CaretObj.IncForcePastEOL;
      skip := sfSkipCaretsAtSelection in FStateFlags;
      MainY := CaretObj.LinePos;
      SelAvail := Editor.SelAvail;
      IsUser := Command >= ecUserFirst;

      y := FSelY2;
      Carets.StartIteratorAtLast;
      while Carets.IterateNextDown do begin
        if cfMainCaret in Carets.CurrentCaretFlags then
          continue;
        p := Carets.CurrentCaretFull;
        if y > p.y then y := p.y;
        if (skip) and (y >= FSelY1) and
           (y = p.y) and (FSelX = p.x)
        then begin
          dec(y);
          continue;
        end;
        if AOnePerLine and
           ( (p.y = MainY) or
             ( Carets.CanPeekCaret(-1) and (Carets.PeekCaretY(-1) = p.y) ) )
        then
          continue;

        CaretObj.FullLogicalPos := p;
        if IsUser and not SelAvail then
          SelectionObj.StartLineBytePos := Point(p.x, p.y);
        i := Carets.CurrentCaretKeepX;
        if i > 0 then
          CaretObj.KeepCaretXPos := i;
        Editor.CommandProcessor(Command, AChar, nil, [hcfInit, hcfFinish]);
        Carets.CurrentCaretFull := CaretObj.FullLogicalPos;
        Carets.CurrentCaretKeepX := -1;
      end;

      CaretObj.DecForcePastEOL;
      Exclude(FStateFlags, sfProcessingRepeat);

      if Carets.MainCaretIndex >= 0 then begin
        CaretObj.FullLogicalPos := Carets.CaretFull[Carets.MainCaretIndex];
        //CaretObj.KeepCaretXPos := Carets.CaretKeepX[Carets.MainCaretIndex];
        RemoveCaret(Carets.MainCaretIndex);
      end
      else
        assert(False, 'TSynCustomPluginMultiCaret.ProcessAllSynCommand: Maincaret index not found');
    finally
      Exclude(FStateFlags, sfSkipCaretsAtSelection);
      FCarets.DecMergeLock;
      MergeAndRemoveCarets;
      Editor.EndUpdate;
    end;
  end;

  procedure ExecCaretMoveRepeated;
  var
    k, xk: Integer;
    c: TLogCaretPoint;
  begin
    Handled := True;
    Editor.BeginUpdate(True);
    FCarets.IncMergeLock;
    try
      // Execute Command at current caret pos
      Include(FStateFlags, sfProcessingMain);
      Editor.CommandProcessor(Command, AChar, data, [hcfInit, hcfFinish]);
      c := CaretObj.FullLogicalPos;
      xk := CaretObj.KeepCaretXPos;
      Exclude(FStateFlags, sfProcessingMain);

      // Repeat command
      Include(FStateFlags, sfProcessingRepeat);
      case Command of
        ecLeft, ecUp, ecWordLeft, ecLineStart, ecPageUp, ecPageLeft,
        ecPageTop, ecLineTextStart, ecWordEndLeft, ecHalfWordLeft, ecSmartWordLeft:
          begin
            Carets.StartIteratorAtFirst;
            while Carets.IterateNextUp do begin
              CaretObj.FullLogicalPos := Carets.CurrentCaretFull;
              k := Carets.CurrentCaretKeepX;
              if k > 0 then
                CaretObj.KeepCaretXPos := k;
              Editor.CommandProcessor(Command, AChar, nil, [hcfInit, hcfFinish]);
              Carets.CurrentCaretFull := CaretObj.FullLogicalPos;
              Carets.CurrentCaretKeepX := CaretObj.KeepCaretXPos;
            end;
          end;
        ecEditorTop, ecEditorBottom: ClearCarets;
        else
          begin
            Carets.StartIteratorAtLast;
            while Carets.IterateNextDown do begin
              CaretObj.FullLogicalPos := Carets.CurrentCaretFull;
              k := Carets.CurrentCaretKeepX;
              if k > 0 then
                CaretObj.KeepCaretXPos := k;
              Editor.CommandProcessor(Command, AChar, nil, [hcfInit, hcfFinish]);
              Carets.CurrentCaretFull := CaretObj.FullLogicalPos;
              Carets.CurrentCaretKeepX := CaretObj.KeepCaretXPos;
            end;
        end;
      end;
      Exclude(FStateFlags, sfProcessingRepeat);

    finally
      FCarets.DecMergeLock;
      CaretObj.FullLogicalPos := c;
      CaretObj.KeepCaretXPos := xk;
      MergeAndRemoveCarets;
      Editor.EndUpdate;
    end;
  end;

  procedure StartEditing;
  begin
    Include(FStateFlags, sfProcessingCmd);
    if (ActiveMode = mcmAddingCarets) and (not Editor.ReadOnly) then
      ActiveMode := DefaultMode;
  end;

var
  ClipHelper: TSynClipboardStream;
  Action: TSynMultiCaretCommandAction;
  Flags: TSynMultiCaretCommandFlags;
begin
  // hcfFinish
  if AfterProcessing then begin
    if (FNestedCommandProcessor > 0) then begin
      dec(FNestedCommandProcessor);
      exit;
    end;

    FStateFlags := FStateFlags - [sfProcessingCmd, sfSkipUndoCarets, sfExtendingColumnSel];
    if (CaretsCount = 0) then
      exit;

    if IsCaretMergeRequested then
      MergeAndRemoveCarets(True); // is case of several commands in one paintlock
    UpdateCaretsPos;

    exit;
  end;


  // hcfInit
  (* use Editor.CommandProcessor(... SkipInit=[hcfInit, hcfFinish])
     command is already initialized / prevent macro recorder from recording again.
  *)

  if (sfProcessingCmd in FStateFlags) then
    inc(FNestedCommandProcessor);
  if (sfProcessingCmd in FStateFlags) or (CaretsCount = 0) then
    exit;
  if Handled then
    exit;


  Action := ccaDefaultAction;
  case Command of
    ecCopy, ecCut:                  Action := ccaNoneRepeatCommand;
    ecGotoMarker0..ecGotoMarker9:   Action := ccaClearCarets;
    ecSelectAll:                    Action := ccaClearCarets;
    ecDeleteChar:                   if smcoDeleteSkipLineBreak in Options then
                                      Command := ecDeleteCharNoCrLf;
    else
      if Command >= ecUserFirst then
        Action := ccaNoneRepeatCommand;
  end;
  Flags := [];
  if FOnBeforeCommand <> nil then
    FOnBeforeCommand(Self, Command, Action, Flags);

  case Action of
    //ccaDefaultAction: ;
    ccaNoneRepeatCommand: begin
        exit;
      end;
    ccaRepeatCommand: begin
        StartEditing;
        ExecCommandRepeated;
        exit;
      end;
    ccaRepeatCommandPerLine: begin
        StartEditing;
        ExecCommandRepeated(True);
        exit;
      end;
    ccaClearCarets: begin
        ClearCarets;
        exit;
      end;
    ccaAdjustCarets: begin
        Include(FStateFlags, sfProcessingCmd);
        exit;
      end;
  end;

  case Command of
  // TODO: delete and smColumn -- only delete once
    ecDeleteLastChar..ecDeleteLine, ecDeleteCharNoCrLf,
    ecLineBreak..ecChar:
      begin
        StartEditing;
        if Editor.ReadOnly then exit;
        ExecCommandRepeated(False, Command = ecDeleteCharNoCrLf);
      end;
    ecPaste:
      begin
        StartEditing;
        if Editor.ReadOnly then exit;

        if (SelectionObj.ActiveSelectionMode = smColumn) and
           (SelectionObj.StartLinePos <> SelectionObj.EndLinePos)
        then begin
          ClipHelper := TSynClipboardStream.Create;
          try
            ClipHelper.ReadFromClipboard(Clipboard);
            if ClipHelper.SelectionMode = smColumn then begin
              Exclude(FStateFlags, sfProcessingCmd);
              exit;
            end;
          finally
            ClipHelper.Free;
          end;
        end;

        ExecCommandRepeated;
      end;
    ecTab..ecShiftTab:
      begin
        StartEditing;
        if Editor.ReadOnly then exit;
        if (eoTabIndent in Editor.Options) and Editor.SelAvail then begin
          if (SelectionObj.ActiveSelectionMode = smColumn) then begin
            // no indent for column mode, when multicaret
            Editor.BeginUpdate(True);
            try
              AddCaret(Editor.LogicalCaretXY.x, Editor.CaretY, CaretObj.BytePosOffset, [cfMainCaret, cfNoneVisual, cfAddDuplicate]);
              Editor.SelText := '';
              if Carets.MainCaretIndex >= 0 then begin
                Editor.LogicalCaretXY := Carets.Caret[Carets.MainCaretIndex];
                RemoveCaret(Carets.MainCaretIndex);
              end
              else
                assert(False, 'TSynCustomPluginMultiCaret.ProcessAllSynCommand: Maincaret index not found');
              ExecCommandRepeated;
            finally
              Editor.EndUpdate;
            end;
          end
          else // exec once and adjust
            exit;
        end
        else
          ExecCommandRepeated;
      end;
    ecSelColCmdRangeStart..ecSelColCmdRangeEnd:
      begin
        Include(FStateFlags, sfSkipUndoCarets);
        Include(FStateFlags, sfExtendingColumnSel);
      end;
    ecLeft..ecSmartWordRight: begin
        Include(FStateFlags, sfSkipUndoCarets);
        if ActiveMode = mcmMoveAllCarets then begin
          Include(FStateFlags, sfProcessingCmd);
          ExecCaretMoveRepeated;
        end
        else
        if ActiveMode = mcmAddingCarets then
          Include(FStateFlags, sfProcessingCmd)
        else
          ClearCarets;
      end;
    ecUndo, ecRedo:
      begin
        // handle now / prevent carets from being cleared
        Include(FStateFlags, sfProcessingCmd);
        Include(FStateFlags, sfSkipUndoCarets);
        Carets.Clear(False, Carets.Capacity); // will be restored at end of undo
        Editor.CommandProcessor(Command, AChar, data, [hcfInit, hcfFinish]);
        Handled := True;
      end;
    ecPluginFirstMultiCaret..ecPluginLastMultiCaret: ; // ignore and handle in hcfPreExec
    else
      begin
        StartEditing;
        if Editor.ReadOnly then exit;
        ExecCommandRepeated;
      end;
  end;

  //Exclude(FStateFlags, sfSkipCaretsAtSelection);
end;

function TSynCustomPluginMultiCaret.MaybeHandleMouseAction(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := HandleActionProc(FMouseActions, AnInfo);
end;

function TSynCustomPluginMultiCaret.DoHandleMouseAction(AnAction: TSynEditMouseAction;
  var AnInfo: TSynEditMouseActionInfo): Boolean;
var
  i, j: Integer;
begin
  Result := False;

  case AnAction.Command of
    emcPluginMultiCaretToggleCaret:
      begin
        Result := True;
        i := Carets.FindCaretIdx(AnInfo.NewCaret.BytePos, AnInfo.NewCaret.LinePos, AnInfo.NewCaret.BytePosOffset);
        if i >= 0 then
          RemoveCaret(i)
        else
        if (AnInfo.NewCaret.BytePos <> CaretObj.BytePos) or (AnInfo.NewCaret.LinePos <> CaretObj.LinePos) then begin
          AddCaret(AnInfo.NewCaret.BytePos, AnInfo.NewCaret.LinePos, AnInfo.NewCaret.BytePosOffset);
        end;
        if CaretsCount > 0 then
          ActiveMode := DefaultMode
        else
          ActiveMode := mcmNoCarets;
        exclude(FStateFlags, sfCreateCaretAtCurrentPos);
      end;
    emcPluginMultiCaretSelectionToCarets:
      begin
        Result := True;
        j := SelectionObj.LastLineBytePos.y;
        i := SelectionObj.FirstLineBytePos.y;
        SelectionObj.Clear;
        CaretObj.LineBytePos := Point(Length(ViewedTextBuffer[ToIdx(j)])+1, j);
        while i < j do begin
          AddCaret(Length(ViewedTextBuffer[ToIdx(i)])+1, i, 0);
          inc(i);
        end;
        if CaretsCount > 0 then
          ActiveMode := DefaultMode;
        if FPaintLock > 0 then
          FStateFlags := FStateFlags + [sfSkipSelChanged, sfSkipCaretChanged];
      end;
  end;
end;

procedure TSynCustomPluginMultiCaret.AddStateFlags(AFlags: TSynPluginMultiCaretStateFlags;
  AnOnlyIfLocked: Boolean);
begin
  if (not AnOnlyIfLocked) or (FPaintLock > 0) then
    FStateFlags := FStateFlags + AFlags;
end;

function TSynCustomPluginMultiCaret.CreateVisual: TSynPluginMultiCaretVisual;
begin
  Result := inherited CreateVisual;
  if FInPaint then
    Result.BeginPaint(FPaintClip);
end;

constructor TSynCustomPluginMultiCaret.Create(AOwner: TComponent);
begin
  FMouseActions := TSynPluginMultiCaretMouseActions.Create(Self);
  FMouseActions.ResetDefaults;
  FKeyStrokes := TSynPluginMultiCaretKeyStrokes.Create(Self);
  FKeyStrokes.ResetDefaults;
  FEnableWithColumnSelection := True;
  FActiveMode := mcmNoCarets;
  FDefaultMode := mcmMoveAllCarets;
  FDefaultColumnSelectMode := mcmCancelOnCaretMove;
  inherited Create(AOwner);
end;

destructor TSynCustomPluginMultiCaret.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FMouseActions);
  FreeAndNil(FKeyStrokes);
end;

procedure TSynCustomPluginMultiCaret.AddCaretAtLogPos(X, Y, Offs: Integer);
begin
  AddCaret(x, y, Offs);
  if ActiveMode = mcmNoCarets then
    ActiveMode := FDefaultMode;
end;

initialization
  RegisterMouseCmdIdentProcs(@IdentToSynMouseCmd, @SynMouseCmdToIdent);
  RegisterExtraGetEditorMouseCommandValues(@GetEditorMouseCommandValues);
  RegisterMouseCmdNameAndOptProcs(@MouseCommandName, @MouseCommandConfigName);

  RegisterKeyCmdIdentProcs(@IdentToKeyCommand,  @KeyCommandToIdent);
  RegisterExtraGetEditorCommandValues(@GetEditorCommandValues);

{$IfDef SynMultiCaretDebug}
  SynMCaretDebug := DebugLogger.FindOrRegisterLogGroup('SynMultiCaretDebug' {$IFDEF SynMultiCaretDebug} , True {$ENDIF} );
{$ENDIF}
end.

