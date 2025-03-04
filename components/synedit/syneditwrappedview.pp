unit SynEditWrappedView;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, LCLType, Forms, LazLoggerBase,
  // SynEdit
  LazSynEditText, SynEdit, SynEditViewedLineMap, SynEditTypes,
  SynEditMiscProcs, SynEditMiscClasses, SynEditKeyCmds, SynEditFoldedView;

type
  TLazSynEditLineWrapPlugin = class;


  TLazSynEditWrapCaretPos = (wcpEOL, wcpBOL);

  { TLazSynWordWrapMapAVLTree }

  TLazSynWordWrapMapAVLTree = class(TSynLineMapAVLTree)
  private
    FWrapPlugin: TLazSynEditLineWrapPlugin;
  protected
    function CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode; override;
  end;

  { TLazSynWordWrapView }

  TLazSynWordWrapView = class(TSynEditLineMappingView)
  private
    FWrapPlugin: TLazSynEditLineWrapPlugin;
  protected
    function CreateDisplayViewEx: TLazSynDisplayLineMapping; override;
    function CreateLineMappingData: TSynLineMapAVLTree; override;
    property LineMappingData;
  public
    constructor Create(AWrapPlugin: TLazSynEditLineWrapPlugin);
    procedure InternalGetInfoForViewedXY(AViewedXY: TViewedPoint;
      AFlags: TViewedXYInfoFlags; out AViewedXYInfo: TViewedXYInfo;
      ALogPhysConvertor: TSynLogicalPhysicalConvertor); override;
  end;

  { TLazSynDisplayWordWrap }

  TLazSynDisplayWordWrap = class(TLazSynDisplayLineMapping)
  private
    FWrapPlugin: TLazSynEditLineWrapPlugin;

    FCurSubLineLogStartIdx, FCurSubLineNextLogStartIdx, FCurRealLineByteLen: Integer;
    FCurSubLinePhysStartIdx, FPrevSubLinePhysWidth: Integer;
    FCurToken: TLazSynDisplayTokenInfo;
    FCurLineLogIdx: Integer;
    FCurLineWrapIndentString: String;
    FCurrentSubLineMarkupInfo: TSynSelectedColorMergeResult;
  public
    constructor Create(AWrappedView: TLazSynWordWrapView; AWrapPlugin: TLazSynEditLineWrapPlugin);
    destructor Destroy; override;
    procedure SetHighlighterTokensLine(AWrappedLine: TLineIdx; out
      ARealLine: TLineIdx; out ASubLineIdx, AStartBytePos, AStartPhysPos, ALineByteLen: Integer); override;
    function GetNextHighlighterToken(out ATokenInfo: TLazSynDisplayTokenInfo): Boolean; override;
  end;


  TSynEditLineMapKeyStrokes = class(TSynEditKeyStrokes)
  end;

  { TLazSynEditLineWrapPlugin }

  TLazSynEditLineWrapPlugin = class(TLazSynEditPlugin, ISynLineWrapProvider)
  private
    FCurrentWrapColumn: Integer;
    FCaretWrapPos: TLazSynEditWrapCaretPos;
    FKeyStrokes: TSynEditLineMapKeyStrokes;
    FMarkupInfoWrapEol: TSynSelectedColor;
    FMarkupInfoWrapIndent: TSynSelectedColor;
    FMarkupInfoWrapSubLine: TSynSelectedColor;

    FMinWrapWidth: Integer;
    FMaxWrapWidth: Integer;
    FOverrideHomeEndKeyDefaults: boolean;
    FWrapIndentMaxAbs: Integer;
    FWrapIndentMaxRel: Integer;
    FWrapIndentMinAbs: Integer;
    FWrapIndentIsOffset: Boolean;
    FWrapIndentWidth: Integer;

    FLineMapView: TLazSynWordWrapView;

    procedure DoLinesChanged(Sender: TObject);
    procedure DoMarkupChanged(Sender: TObject);
    procedure DoHandleCreated(Sender: TObject; Changes: TSynStatusChanges);
    procedure DoTriggerValidate(Data: PtrInt);
    procedure DoWidthChanged(Sender: TObject; Changes: TSynStatusChanges);
    function GetLineMappingData: TSynLineMapAVLTree;
    function GetWrapColumn: Integer;
    procedure SetKeyStrokes(AValue: TSynEditLineMapKeyStrokes);

    procedure SetWrapIndentMaxAbs(AValue: Integer);
    procedure SetWrapIndentMaxRel(AValue: Integer);
    procedure SetWrapIndentMinAbs(AValue: Integer);
    procedure SetMinWrapWidth(AValue: Integer);
    procedure SetMaxWrapWidth(AValue: Integer);
    procedure SetWrapIndentIsOffset(AValue: Boolean);
    procedure SetWrapIndentWidth(AValue: Integer);
  protected
    procedure SetEditor(const AValue: TCustomSynEdit); override;
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;

    procedure TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
      var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
      var Command: TSynEditorCommand; FinishComboOnly: Boolean;
      var ComboKeyStrokes: TSynEditKeyStrokes);
    procedure ProcessSynCommandInit(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUtf8Char; Data: pointer;
      HandlerData: pointer);
    procedure ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
              var Handled: boolean; var Command: TSynEditorCommand;
              var AChar: TUTF8Char; Data: pointer; HandlerData: pointer);


    function CalculateIndentFor(ALine: PChar; AMaxWidth: Integer; const PhysCharWidths: TPhysicalCharWidths): Integer;
    function CalculateNextBreak(ALine: PChar; ALogStartFrom: IntIdx; AMaxWidth: Integer;
      const PhysCharWidths: TPhysicalCharWidths; out APhysWidth: Integer): IntIdx;
    function  GetSublineCount (ALine: String; AMaxWidth: Integer; const APhysCharWidths: TPhysicalCharWidths): Integer; inline;
    procedure GetSublineBounds(ALine: String; AMaxWidth, AWrapIndent: Integer;
      const APhysCharWidths: TPhysicalCharWidths; ASubLine: Integer; out ALogStartX,
      ANextLogStartX, APhysStart: IntIdx; out APhysWidth: integer); // APhysStart is based on LTR only
    procedure GetSublineBounds(ALine: String; AMaxWidth, AWrapIndent: Integer; const APhysCharWidths: TPhysicalCharWidths;
      ASubLine: Integer;
      out ALogStartX, ANextLogStartX, APhysStart: IntIdx; out APhysWidth: integer;
      var AViewedX: integer;
      out APhysX: integer);
    procedure GetSublineBounds(var AViewedXY: TViewedPoint;
      out ALogStartX, ANextLogStartX, APhysStart: IntIdx; out APhysWidth, AFirstViewedX, ALastViewedX: integer);
    function  GetSubLineFromX (ALine: String; AMaxWidth, AWrapIndent: Integer; const APhysCharWidths: TPhysicalCharWidths; var APhysToViewedXPos: Integer): integer;

    procedure GetWrapInfoForViewedXY(var AViewedXY: TViewedPoint; AFlags: TViewedXYInfoFlags; out AFirstViewedX: IntPos; ALogPhysConvertor: TSynLogicalPhysicalConvertor);

    function TextXYToLineXY(ATextXY: TPhysPoint): TViewedSubPoint_0;
    function ViewedSubLineXYToTextX(ARealLine: IntPos; ALineXY: TViewedSubPoint_0): Integer;
    function CalculateWrapForLine(ALineIdx: IntIdx; AMaxWidth: integer): Integer; inline;

    property LineMapView: TLazSynWordWrapView read FLineMapView;
    property LineMappingData: TSynLineMapAVLTree read GetLineMappingData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure WrapAll;
    procedure ValidateAll;

    property WrapColumn: Integer read GetWrapColumn;

  published
    property CaretWrapPos: TLazSynEditWrapCaretPos read FCaretWrapPos write FCaretWrapPos;

    property KeyStrokes: TSynEditLineMapKeyStrokes read FKeyStrokes write SetKeyStrokes;
    property OverrideHomeEndKeyDefaults: boolean read FOverrideHomeEndKeyDefaults write FOverrideHomeEndKeyDefaults;

    property MinWrapWidth: Integer read FMinWrapWidth write SetMinWrapWidth;
    property MaxWrapWidth: Integer read FMaxWrapWidth write SetMaxWrapWidth;
    property WrapIndentWidth: Integer read FWrapIndentWidth write SetWrapIndentWidth;
    property WrapIndentIsOffset: Boolean read FWrapIndentIsOffset write SetWrapIndentIsOffset;
    property WrapIndentMinAbs: Integer read FWrapIndentMinAbs write SetWrapIndentMinAbs;
    property WrapIndentMaxAbs: Integer read FWrapIndentMaxAbs write SetWrapIndentMaxAbs;
    property WrapIndentMaxRel: Integer read FWrapIndentMaxRel write SetWrapIndentMaxRel;

    property MarkupInfoWrapSubLine: TSynSelectedColor read FMarkupInfoWrapSubLine;
    property MarkupInfoWrapIndent:  TSynSelectedColor read FMarkupInfoWrapIndent;
    property MarkupInfoWrapEol:     TSynSelectedColor read FMarkupInfoWrapEol;
  end;

const
  ecSynPLineWrapLineStart         = ecPluginFirstLineWrap +  0;
  ecSynPLineWrapLineEnd           = ecPluginFirstLineWrap +  1;
  ecSynPLineWrapSelLineStart      = ecPluginFirstLineWrap +  2;
  ecSynPLineWrapSelLineEnd        = ecPluginFirstLineWrap +  3;
  ecSynPLineWrapColSelLineStart   = ecPluginFirstLineWrap +  4;
  ecSynPLineWrapColSelLineEnd     = ecPluginFirstLineWrap +  5;

  ecSynPLineWrapCount     = 6;


implementation

{ TLazSynDisplayWordWrap }

constructor TLazSynDisplayWordWrap.Create(AWrappedView: TLazSynWordWrapView;
  AWrapPlugin: TLazSynEditLineWrapPlugin);
begin
  FWrapPlugin := AWrapPlugin;
  FCurrentSubLineMarkupInfo := TSynSelectedColorMergeResult.Create(nil);
  inherited Create(AWrappedView);
end;

destructor TLazSynDisplayWordWrap.Destroy;
begin
  FCurrentSubLineMarkupInfo.Free;
  inherited Destroy;
end;

procedure TLazSynDisplayWordWrap.SetHighlighterTokensLine(
  AWrappedLine: TLineIdx; out ARealLine: TLineIdx; out ASubLineIdx, AStartBytePos,
  AStartPhysPos, ALineByteLen: Integer);
var
  IsNext: Boolean;
  PrevSub: IntIdx;
  LineTxt: String;
  PWidth: TPhysicalCharWidths;
  PhysWidth, MaxW, CurLineWrapInd: Integer;
begin
  IsNext := (AWrappedLine = FCurWrappedLine + 1) and (FCurWrappedLine >= 0);
  PrevSub := FCurrentWrapSubline;

  inherited SetHighlighterTokensLine(AWrappedLine, ARealLine, ASubLineIdx, AStartBytePos, AStartPhysPos, FCurRealLineByteLen);

  LineTxt := FLineMappingView.NextLines.Strings[ARealLine];
  FLineMappingView.LogPhysConvertor.CurrentLine := ARealLine;
  PWidth := FLineMappingView.LogPhysConvertor.CurrentWidthsDirect;
  //PWidth  := FLineMappingView.GetPhysicalCharWidths(ARealLine);
  MaxW    := FWrapPlugin.WrapColumn;

  CurLineWrapInd := 0;
  if (FCurrentWrapSubline > 0) then begin
    CurLineWrapInd := FWrapPlugin.CalculateIndentFor(PChar(LineTxt), MaxW, PWidth);
    FCurLineWrapIndentString := StringOfChar(' ', CurLineWrapInd);
    assert(CurLineWrapInd>=0, 'TLazSynDisplayWordWrap.SetHighlighterTokensLine: CurLineWrapInd>=0');
  end;

  if IsNext and (FCurrentWrapSubline = PrevSub + 1) then begin
    FCurSubLineLogStartIdx := FCurSubLineNextLogStartIdx;
    // 2nd or lower line / deduct CurLineWrapInd
    FCurSubLineNextLogStartIdx := FWrapPlugin.CalculateNextBreak(PChar(LineTxt), FCurSubLineNextLogStartIdx,
      MaxW-CurLineWrapInd, PWidth, PhysWidth);
    FCurSubLinePhysStartIdx := FCurSubLinePhysStartIdx + FPrevSubLinePhysWidth;
    FPrevSubLinePhysWidth := PhysWidth;
  end
  else begin
    // CurLineWrapInd may be incorrectly 0 / but only if retriving bounds for a "first line"
    FWrapPlugin.GetSublineBounds(LineTxt, MaxW, CurLineWrapInd, PWidth, FCurrentWrapSubline,
      FCurSubLineLogStartIdx, FCurSubLineNextLogStartIdx, FCurSubLinePhysStartIdx, FPrevSubLinePhysWidth);
  end;
  AStartBytePos := AStartBytePos + FCurSubLineLogStartIdx;
  AStartPhysPos := ToPos(FCurSubLinePhysStartIdx);
  ALineByteLen := FCurSubLineNextLogStartIdx - FCurSubLineLogStartIdx;

  FCurLineLogIdx := -CurLineWrapInd;
end;

function TLazSynDisplayWordWrap.GetNextHighlighterToken(out
  ATokenInfo: TLazSynDisplayTokenInfo): Boolean;
var
  PreStart: Integer;
  LineBnd1, LineBnd2: TLazSynDisplayTokenBound;
begin
  ATokenInfo := Default(TLazSynDisplayTokenInfo);
  if FCurLineLogIdx < 0 then begin
    Result := True;
    ATokenInfo.TokenStart := PChar(FCurLineWrapIndentString);
    ATokenInfo.TokenLength := -FCurLineLogIdx;
    ATokenInfo.TokenAttr := FWrapPlugin.MarkupInfoWrapIndent;
    ATokenInfo.TokenOrigin := dtoBeforeText;
    FCurLineLogIdx := 0;
    exit;
  end;

  If (FCurLineLogIdx >= FCurSubLineNextLogStartIdx) and (FCurSubLineNextLogStartIdx < FCurRealLineByteLen) then begin
    ATokenInfo.TokenOrigin := dtoAfterWrapped;
    ATokenInfo.TokenAttr := FWrapPlugin.MarkupInfoWrapEol;
    Result := True; // TokenStart = nil => no text
    exit;
  end;

  repeat
    PreStart := FCurSubLineLogStartIdx - FCurLineLogIdx;
    Result := inherited GetNextHighlighterToken(ATokenInfo);
    if (not Result) or (ATokenInfo.TokenLength <= 0) then begin
      exit;
    end;
    FCurToken := ATokenInfo;

    FCurLineLogIdx := FCurLineLogIdx + ATokenInfo.TokenLength;
  until FCurLineLogIdx > FCurSubLineLogStartIdx;

  if PreStart > 0 then begin
    ATokenInfo.TokenStart := ATokenInfo.TokenStart + PreStart;
    ATokenInfo.TokenLength := ATokenInfo.TokenLength - PreStart;
    Result := ATokenInfo.TokenLength > 0;
    if not Result then begin
      ATokenInfo.TokenOrigin := dtoAfterWrapped;
      ATokenInfo.TokenAttr := FWrapPlugin.MarkupInfoWrapEol;
      Result := False;
      exit;
    end;
  end;


  If FCurLineLogIdx > FCurSubLineNextLogStartIdx then begin
    ATokenInfo.TokenLength := ATokenInfo.TokenLength - (FCurLineLogIdx - FCurSubLineNextLogStartIdx);
    Result := ATokenInfo.TokenLength > 0;
    if not Result then begin
      ATokenInfo.TokenOrigin := dtoAfterWrapped;
      ATokenInfo.TokenAttr := FWrapPlugin.MarkupInfoWrapEol;
      Result := False;
      exit;
    end;
  end;

  if (FCurrentWrapSubline > 0) and (FWrapPlugin.MarkupInfoWrapSubLine <> nil) and
     (FWrapPlugin.MarkupInfoWrapSubLine.IsEnabled)
  then begin
    LineBnd1.Logical  := ToPos(FCurSubLineLogStartIdx);
    LineBnd1.Physical := -1;
    LineBnd1.Offset   := 0;
    LineBnd2.Logical  := ToPos(FCurSubLineNextLogStartIdx);
    LineBnd2.Physical := -1;
    LineBnd2.Offset   := 0;

    FCurrentSubLineMarkupInfo.Clear;
    if ATokenInfo.TokenAttr <> nil then begin
      FCurrentSubLineMarkupInfo.Assign(ATokenInfo.TokenAttr);
      FCurrentSubLineMarkupInfo.Merge(FWrapPlugin.MarkupInfoWrapSubLine, LineBnd1, LineBnd2);
    end
    else
      FCurrentSubLineMarkupInfo.Assign(FWrapPlugin.MarkupInfoWrapSubLine);
    ATokenInfo.TokenAttr := FCurrentSubLineMarkupInfo;
  end;
end;

{ TLazSynEditLineWrapPlugin }

procedure TLazSynEditLineWrapPlugin.DoLinesChanged(Sender: TObject);
begin
  ValidateAll;
end;

procedure TLazSynEditLineWrapPlugin.DoMarkupChanged(Sender: TObject);
begin
  FMarkupInfoWrapIndent.FrameEdges := sfeLeft;
end;

procedure TLazSynEditLineWrapPlugin.DoHandleCreated(Sender: TObject; Changes: TSynStatusChanges);
begin
  Application.QueueAsyncCall(@DoTriggerValidate, 0); // just in case there is no resize
end;

procedure TLazSynEditLineWrapPlugin.DoTriggerValidate(Data: PtrInt);
begin
  DoWidthChanged(nil, [scCharsInWindow]);
end;

procedure TLazSynEditLineWrapPlugin.DoWidthChanged(Sender: TObject;
  Changes: TSynStatusChanges);
var
  w: Integer;
begin
  if not Editor.HandleAllocated then exit;

  Application.RemoveAsyncCalls(Self);
  w := WrapColumn;
  if FCurrentWrapColumn = w then
    exit;
  FCurrentWrapColumn := w;
  FLineMapView.KnownLengthOfLongestLine := w;
  FLineMapView.InvalidateLines(0, FLineMapView.NextLines.Count);
end;

function TLazSynEditLineWrapPlugin.GetLineMappingData: TSynLineMapAVLTree;
begin
  Result := FLineMapView.LineMappingData;
end;

function TLazSynEditLineWrapPlugin.GetWrapColumn: Integer;
begin
  Result := TSynEdit(Editor).CharsInWindow - 1;
  if Result < FMinWrapWidth then
    Result := FMinWrapWidth;
  if (FMaxWrapWidth > 0) and (Result > FMaxWrapWidth) then
    Result := FMaxWrapWidth;
end;

procedure TLazSynEditLineWrapPlugin.SetKeyStrokes(AValue: TSynEditLineMapKeyStrokes);
begin
  if AValue = nil then
    FKeyStrokes.Clear
  else
    FKeyStrokes.Assign(AValue);
end;

procedure TLazSynEditLineWrapPlugin.SetWrapIndentMaxAbs(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FWrapIndentMaxAbs = AValue then Exit;
  FWrapIndentMaxAbs := AValue;
  WrapAll;
end;

procedure TLazSynEditLineWrapPlugin.SetWrapIndentMaxRel(AValue: Integer);
begin
  if AValue > 100 then
    AValue := 100;
  if FWrapIndentMaxRel = AValue then Exit;
  FWrapIndentMaxRel := AValue;
  WrapAll;
end;

procedure TLazSynEditLineWrapPlugin.SetWrapIndentMinAbs(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FWrapIndentMinAbs = AValue then Exit;
  FWrapIndentMinAbs := AValue;
  WrapAll;
end;

procedure TLazSynEditLineWrapPlugin.SetMinWrapWidth(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1;
  if FMinWrapWidth = AValue then Exit;

  FMinWrapWidth := AValue;
  if (AValue > 0) and (FMaxWrapWidth > 0) and (FMaxWrapWidth < AValue) then
    FMaxWrapWidth := AValue;
  DoWidthChanged(nil, [scCharsInWindow]);
end;

procedure TLazSynEditLineWrapPlugin.SetMaxWrapWidth(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0;
  if FMaxWrapWidth = AValue then Exit;

  FMaxWrapWidth := AValue;
  if (AValue > 0) and (FMinWrapWidth > AValue) then
    FMinWrapWidth := AValue;
  DoWidthChanged(nil, [scCharsInWindow]);
end;

procedure TLazSynEditLineWrapPlugin.SetWrapIndentIsOffset(AValue: Boolean);
begin
  if FWrapIndentIsOffset = AValue then Exit;
  FWrapIndentIsOffset := AValue;
  WrapAll;
end;

procedure TLazSynEditLineWrapPlugin.SetWrapIndentWidth(AValue: Integer);
begin
  // can be negative
  if FWrapIndentWidth = AValue then Exit;
  FWrapIndentWidth := AValue;
  WrapAll;
end;

procedure TLazSynEditLineWrapPlugin.SetEditor(const AValue: TCustomSynEdit);
begin
  if (Editor <> nil) and (AValue <> nil) then
    raise Exception.Create('Not allowed to change editor');
  inherited SetEditor(AValue);
end;

procedure TLazSynEditLineWrapPlugin.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  if Editor <> nil then begin
    Editor.UnregisterCommandHandler(@ProcessSynCommand);
    Editor.UnregisterCommandHandler(@ProcessSynCommandInit);
    Editor.UnRegisterKeyTranslationHandler(@TranslateKey);
  end;
  inherited DoEditorRemoving(AValue);
end;

procedure TLazSynEditLineWrapPlugin.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  if Editor <> nil then begin
    Editor.RegisterCommandHandler(@ProcessSynCommandInit, nil, [hcfInit]);
    Editor.RegisterCommandHandler(@ProcessSynCommand, nil, [hcfPreExec]);
    Editor.RegisterKeyTranslationHandler(@TranslateKey);
  end;
end;

procedure TLazSynEditLineWrapPlugin.TranslateKey(Sender: TObject; Code: word; SState: TShiftState;
  var Data: pointer; var IsStartOfCombo: boolean; var Handled: boolean;
  var Command: TSynEditorCommand; FinishComboOnly: Boolean; var ComboKeyStrokes: TSynEditKeyStrokes
  );
begin
  if Handled then
    exit;

  if not FinishComboOnly then
    FKeyStrokes.ResetKeyCombo;
  Command := FKeyStrokes.FindKeycodeEx(Code, SState, Data, IsStartOfCombo, FinishComboOnly, ComboKeyStrokes);

  Handled := (Command <> ecNone) or IsStartOfCombo;
  if IsStartOfCombo then
    ComboKeyStrokes := FKeyStrokes;
end;

procedure TLazSynEditLineWrapPlugin.ProcessSynCommandInit(Sender: TObject;
  AfterProcessing: boolean; var Handled: boolean; var Command: TSynEditorCommand;
  var AChar: TUtf8Char; Data: pointer; HandlerData: pointer);
var
  p: TPoint;
  LogStartX, NextLogStartX, PhysStartX: IntIdx;
  PhysWidth, FirstX, LastX: integer;
begin
  if FOverrideHomeEndKeyDefaults then begin
    case Command of
      ecSynPLineWrapLineStart:       Command := ecLineStart;
      ecSynPLineWrapLineEnd:         Command := ecLineEnd;
      ecSynPLineWrapSelLineStart:    Command := ecSelLineStart;
      ecSynPLineWrapSelLineEnd:      Command := ecSelLineEnd;
      ecSynPLineWrapColSelLineStart: Command := ecColSelLineStart;
      ecSynPLineWrapColSelLineEnd:   Command := ecColSelLineEnd;
      ecLineStart:       Command := ecSynPLineWrapLineStart;
      ecLineEnd:         Command := ecSynPLineWrapLineEnd;
      ecSelLineStart:    Command := ecSynPLineWrapSelLineStart;
      ecSelLineEnd:      Command := ecSynPLineWrapSelLineEnd;
      ecColSelLineStart: Command := ecSynPLineWrapColSelLineStart;
      ecColSelLineEnd:   Command := ecSynPLineWrapColSelLineEnd;
    end;
  end;

  case Command of
    ecSynPLineWrapLineStart, ecSynPLineWrapSelLineStart, ecSynPLineWrapColSelLineStart: begin
        p := CaretObj.ViewedLineCharPos;
        GetSublineBounds(p, LogStartX, NextLogStartX, PhysStartX, PhysWidth, FirstX, LastX);
        if (p.X = FirstX) or (LogStartX = 0) then
          case Command of
            ecSynPLineWrapLineStart:       Command := ecLineStart;
            ecSynPLineWrapSelLineStart:    Command := ecSelLineStart;
            ecSynPLineWrapColSelLineStart: Command := ecColSelLineStart;
          end;
      end;
    ecSynPLineWrapLineEnd, ecSynPLineWrapSelLineEnd, ecSynPLineWrapColSelLineEnd: begin
        p := CaretObj.ViewedLineCharPos;
        GetSublineBounds(p, LogStartX, NextLogStartX, PhysStartX, PhysWidth, FirstX, LastX);
        if (p.X = LastX) or (NextLogStartX = 0) then
          case Command of
            ecSynPLineWrapLineEnd:       Command := ecLineEnd;
            ecSynPLineWrapSelLineEnd:    Command := ecSelLineEnd;
            ecSynPLineWrapColSelLineEnd: Command := ecColSelLineEnd;
          end;
      end;
  end;
end;

procedure TLazSynEditLineWrapPlugin.ProcessSynCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
var
  p: TPoint;
begin
  if Handled then exit;

  if (Command = ecSynPLineWrapColSelLineStart) or (Command = ecSynPLineWrapColSelLineEnd) then begin
    SelectionObj.ActiveSelectionMode := smColumn;
    SelectionObj.AutoExtend := True;
  end;
  if (Command = ecSynPLineWrapSelLineStart) or (Command = ecSynPLineWrapSelLineEnd) then begin
    SelectionObj.ActiveSelectionMode := SelectionObj.SelectionMode;
    SelectionObj.AutoExtend := True;
  end;

  case Command of
    ecSynPLineWrapLineStart, ecSynPLineWrapSelLineStart, ecSynPLineWrapColSelLineStart: begin
        CaretObj.ChangeOnTouch;
        p := CaretObj.ViewedLineCharPos;
        p.x := 1;
        CaretObj.ViewedLineCharPos := p;
      end;
    ecSynPLineWrapLineEnd, ecSynPLineWrapSelLineEnd, ecSynPLineWrapColSelLineEnd: begin
        CaretObj.ChangeOnTouch;
        p := CaretObj.ViewedLineCharPos;
        p.x := WrapColumn + 1;
        CaretObj.ViewedLineCharPos := p;
      end;
  end;
end;

function TLazSynEditLineWrapPlugin.CalculateIndentFor(ALine: PChar; AMaxWidth: Integer;
  const PhysCharWidths: TPhysicalCharWidths): Integer;
var
  i: Integer;
begin
  if FWrapIndentIsOffset then begin
    Result := 0;
    i := CountLeadWhiteSpace(ALine);
    while i > 0 do begin
      dec(i);
      Result := Result + (PhysCharWidths[i] and PCWMask);
    end;
    Result := Result + FWrapIndentWidth;
    if Result < FWrapIndentMinAbs then
      Result := FWrapIndentMinAbs;
  end
  else
    Result := FWrapIndentWidth;

  if FWrapIndentMaxAbs > 0 then
    Result := Min(FWrapIndentMaxAbs, Result);

  if FWrapIndentMaxRel > 0 then
    Result := Min(MulDiv(AMaxWidth, FWrapIndentMaxRel, 100), Result)
  else
  if FWrapIndentMaxRel < 0 then
    Result := Min(Max(0, AMaxWidth + FWrapIndentMaxRel), Result); // keep at least n columns visible

  Result := Max(0, Result);
  if Result >= AMaxWidth then
    Result := Max(0, AMaxWidth-1);
end;

function TLazSynEditLineWrapPlugin.CalculateNextBreak(ALine: PChar;
  ALogStartFrom: IntIdx; AMaxWidth: Integer;
  const PhysCharWidths: TPhysicalCharWidths; out APhysWidth: Integer): IntIdx;
const
  // todo, other break chars // utf8
  BREAKCHARS = [#9, #32, '.', ',', ':', ';', '=', '-', '+', '*', '/', '(', ')', '{', '}', '[', ']', '!', '<', '>'];
var
  PhysWidthPtr: PByte;
  CurCharPhysWidth: Cardinal;
  LastGoodPos: PChar;
begin
  if (ALine = nil) or (ALine^ = #0) then
    exit(0);

  PhysWidthPtr := @PhysCharWidths[ALogStartFrom];
  APhysWidth := AMaxWidth;
  Result := ALogStartFrom;
  ALine := ALine + ALogStartFrom;
  LastGoodPos := ALine;

  while ALine <> nil do begin
    if ALine^ in BREAKCHARS then
      while ALine^ in BREAKCHARS do begin
        CurCharPhysWidth := PhysWidthPtr^ and PCWMask;
        if CurCharPhysWidth <= AMaxWidth then begin
          inc(ALine);
          inc(PhysWidthPtr);
          inc(Result);
          dec(AMaxWidth, CurCharPhysWidth);
        end
        else begin
          ALine := nil; // break outer loop
          break;
        end;
      end

    else begin
      CurCharPhysWidth := 0;
      LastGoodPos := ALine;
      while (ALine^ <> #0) and not (ALine^ in BREAKCHARS) do begin
        CurCharPhysWidth := CurCharPhysWidth + PhysWidthPtr^ and PCWMask;
        inc(ALine);
        inc(PhysWidthPtr);
      end;

      if (CurCharPhysWidth > 0) and (CurCharPhysWidth <= AMaxWidth) then begin
        inc(Result, ALine-LastGoodPos);
        dec(AMaxWidth, CurCharPhysWidth);
      end
      else begin
        ALine := nil; // break outer loop
        break;
      end;
    end;
  end;

  if Result = ALogStartFrom then begin
    PhysWidthPtr := @PhysCharWidths[0];
    ALine := LastGoodPos;
    while ALine^ <> #0 do begin
      CurCharPhysWidth := PhysWidthPtr^ and PCWMask;
      if (CurCharPhysWidth <= AMaxWidth) or (Result = ALogStartFrom) then begin
        inc(ALine);
        inc(PhysWidthPtr);
        inc(Result);
        dec(AMaxWidth, CurCharPhysWidth);
      end
      else
        break;
    end;
  end;
  APhysWidth := APhysWidth - AMaxWidth;
end;

function TLazSynEditLineWrapPlugin.GetSublineCount(ALine: String;
  AMaxWidth: Integer; const APhysCharWidths: TPhysicalCharWidths): Integer;
var
  x, dummy: Integer;
begin
  Result := 1;
  if Length(ALine) = 0 then
    exit;
  x := CalculateNextBreak(PChar(ALine), 0, AMaxWidth, APhysCharWidths, dummy);
  AMaxWidth := AMaxWidth - CalculateIndentFor(PChar(ALine), AMaxWidth, APhysCharWidths);
  assert(AMaxWidth>0, 'TLazSynEditLineWrapPlugin.GetSublineCount: AMaxWidth>0');
  while (x < Length(ALine)) do begin
    inc(Result);
    x := CalculateNextBreak(PChar(ALine), x, AMaxWidth, APhysCharWidths, dummy);
  end;
end;

function FindPreviousCharStart(const APhysCharWidths: TPhysicalCharWidths; AnX: integer): Integer; inline;
begin
  assert(AnX > 0, 'FindPreviousCharStart: AnX > 0');
  Result := AnX - 1;
  while (Result >= 0) and (APhysCharWidths[Result] = 0) do
    dec(Result);
end;
function CalculateRtlLead(const APhysCharWidths: TPhysicalCharWidths; AnX: integer): Integer; //inline;
begin
  Result := 0;
  while (AnX >= 0) and
        ( (APhysCharWidths[AnX] = 0) or ((APhysCharWidths[AnX] and PCWFlagRTL) <> 0) )
  do begin
    Result := Result + (APhysCharWidths[AnX] and PCWMask);
    dec(AnX);
  end;
end;
function CalculateRtlRemainder(const APhysCharWidths: TPhysicalCharWidths; ALineLen, AnX: integer): Integer; //inline;
begin
  Result := 0;
  while (AnX < ALineLen) and
        ( (APhysCharWidths[AnX] = 0) or ((APhysCharWidths[AnX] and PCWFlagRTL) <> 0) )
  do begin
    Result := Result + (APhysCharWidths[AnX] and PCWMask);
    inc(AnX);
  end;
end;


procedure TLazSynEditLineWrapPlugin.GetSublineBounds(ALine: String; AMaxWidth,
  AWrapIndent: Integer; const APhysCharWidths: TPhysicalCharWidths; ASubLine: Integer; out
  ALogStartX, ANextLogStartX, APhysStart: IntIdx; out APhysWidth: integer);
begin
  ALogStartX := 0;
  ANextLogStartX := 0;
  APhysStart := 0;
  if Length(ALine) = 0 then
    exit;
  ANextLogStartX := CalculateNextBreak(PChar(ALine), ALogStartX, AMaxWidth, APhysCharWidths, APhysWidth);

  AMaxWidth := AMaxWidth - AWrapIndent;
  assert(AMaxWidth>0, 'TLazSynEditLineWrapPlugin.GetSublineBounds: AMaxWidth>0');
  while ASubLine > 0 do begin
    ALogStartX := ANextLogStartX;
    APhysStart := APhysStart + APhysWidth;
    ANextLogStartX := CalculateNextBreak(PChar(ALine), ALogStartX, AMaxWidth, APhysCharWidths, APhysWidth);
    dec(ASubLine);
  end;
end;

procedure TLazSynEditLineWrapPlugin.GetSublineBounds(ALine: String; AMaxWidth,
  AWrapIndent: Integer; const APhysCharWidths: TPhysicalCharWidths; ASubLine: Integer; out
  ALogStartX, ANextLogStartX, APhysStart: IntIdx; out APhysWidth: integer; var AViewedX: integer;
  out APhysX: integer);
var
  x, RtlRemainderWidth, RtlLeadWitdh: Integer;
begin
  GetSublineBounds(ALine, AMaxWidth, AWrapIndent, APhysCharWidths, ASubLine, ALogStartX, ANextLogStartX, APhysStart, APhysWidth);

  if (ASubLine > 0) then begin
    AViewedX := max(1, AViewedX - AWrapIndent);
    if (AViewedX = 1) and (FCaretWrapPos = wcpEOL) then
      AViewedX := 2;
  end;
  if (ANextLogStartX > 0) and (ANextLogStartX < Length(ALine)) then begin
    x := APhysWidth;
    if FCaretWrapPos = wcpEOL then
      inc(x);
    if AViewedX > x then
      AViewedX := x;
  end;

  APhysX := AViewedX;

  if (ALogStartX > 0) and ((APhysCharWidths[ALogStartX] and PCWFlagRTL) <> 0) then begin
    x := FindPreviousCharStart(APhysCharWidths, ALogStartX);
    if (APhysCharWidths[x] and PCWFlagRTL) <> 0 then begin
      RtlRemainderWidth := CalculateRtlRemainder(APhysCharWidths, Length(ALine), ALogStartX);
      if APhysX < RtlRemainderWidth then begin
        RtlLeadWitdh := CalculateRtlLead(APhysCharWidths, x);
        APhysX := APhysX
                  + (APhysStart - RtlLeadWitdh)
                  + Max(0,RtlRemainderWidth - APhysWidth);
        exit;
      end;
    end;
  end;

  if (ANextLogStartX > 0) and (ANextLogStartX < Length(ALine)) and ((APhysCharWidths[ANextLogStartX] and PCWFlagRTL) <> 0) then begin
    x := FindPreviousCharStart(APhysCharWidths, ANextLogStartX);
    RtlLeadWitdh := CalculateRtlLead(APhysCharWidths, x);
    assert(APhysWidth >= RtlLeadWitdh, 'TLazSynEditLineWrapPlugin.GetSublineBounds: APhysWidth > RtlLeadWitdh');
    if APhysX - 1 > APhysWidth - RtlLeadWitdh then begin
      RtlRemainderWidth := CalculateRtlRemainder(APhysCharWidths, Length(ALine), ANextLogStartX);
      APhysX := APhysX + APhysStart + RtlRemainderWidth;
      exit;
    end;
  end;

  APhysX := APhysX + APhysStart;
end;

procedure TLazSynEditLineWrapPlugin.GetSublineBounds(var AViewedXY: TViewedPoint; out ALogStartX,
  ANextLogStartX, APhysStart: IntIdx; out APhysWidth, AFirstViewedX, ALastViewedX: integer);
var
  SubLineOffset, YIdx: TLineIdx;
  LineTxt: String;
  PWidth: TPhysicalCharWidths;
  WrapInd, AMaxWidth: Integer;
  ALogPhysConvertor: TSynLogicalPhysicalConvertor;
begin
  YIdx := FLineMapView.LineMappingData.GetLineForForWrap(ToIdx(AViewedXY.y), SubLineOffset);
  YIdx := FLineMapView.NextLines.ViewToTextIndex(YIdx);

  LineTxt := FLineMapView.Strings[YIdx];
  ALogPhysConvertor := FLineMapView.LogPhysConvertor;
  ALogPhysConvertor.CurrentLine := YIdx;
  PWidth  := ALogPhysConvertor.CurrentWidthsDirect;
  AMaxWidth := WrapColumn;
  WrapInd := CalculateIndentFor(PChar(LineTxt), AMaxWidth, PWidth);

  GetSublineBounds(LineTxt, AMaxWidth, WrapInd, PWidth, SubLineOffset, ALogStartX, ANextLogStartX, APhysStart, APhysWidth);

  if SubLineOffset = 0 then
    WrapInd := 0;
  case CaretWrapPos of
    wcpEOL: begin
        AFirstViewedX := 2 + WrapInd;
        ALastViewedX  := 1 + WrapInd + APhysWidth;
      end;
    wcpBOL: begin
        AFirstViewedX := 1 + WrapInd;
        ALastViewedX  := WrapInd + APhysWidth;
      end;
  end;
  if SubLineOffset = 0 then
    AFirstViewedX := 1;
  if ANextLogStartX >= Length(LineTxt) then begin
    ANextLogStartX := 0;
    ALastViewedX  := 1 + WrapInd + APhysWidth;
  end;
end;

function TLazSynEditLineWrapPlugin.GetSubLineFromX(ALine: String; AMaxWidth, AWrapIndent: Integer;
  const APhysCharWidths: TPhysicalCharWidths; var APhysToViewedXPos: Integer): integer;
var
  x, PhysWidth, AMaxWidth2, x2: Integer;
  RtlLeadWitdh, RtlRemainderWidth, PhysToViewedInRTLOffs, LtrLead: integer;
begin
  Result := 0;
  if Length(ALine) = 0 then
    exit;
  Result := -1;
  x := 0;
  RtlRemainderWidth := 0;
  PhysToViewedInRTLOffs := 0;

  AMaxWidth2 := AMaxWidth - AWrapIndent;
  assert(AMaxWidth2>0, 'TLazSynEditLineWrapPlugin.GetSublineCount: AMaxWidth>0');

  APhysToViewedXPos := ToIdx(APhysToViewedXPos);
  while (x < Length(ALine)) do begin
    inc(Result);
    x := CalculateNextBreak(PChar(ALine), x, AMaxWidth, APhysCharWidths, PhysWidth);
    AMaxWidth := AMaxWidth2;
    if (x >= Length(ALine)) and (PhysToViewedInRTLOffs = 0) then
      break;

    LtrLead := 0;
    if RtlRemainderWidth > 0 then
      RtlRemainderWidth := Max(0, RtlRemainderWidth - PhysWidth);

    if (RtlRemainderWidth = 0) and
       (PhysToViewedInRTLOffs = 0) and
       (x > 0) and ((APhysCharWidths[x] and PCWFlagRTL) <> 0) then begin
      x2 := FindPreviousCharStart(APhysCharWidths, x);
      if ((APhysCharWidths[x2] and PCWFlagRTL) <> 0) then begin
        // IN RTL run
        RtlLeadWitdh := CalculateRtlLead(APhysCharWidths, x2);
        assert(RtlLeadWitdh <= PhysWidth, 'TLazSynEditLineWrapPlugin.GetSubLineFromX: RtlLeadWitdh <= PhysWidth');

        if (APhysToViewedXPos <= PhysWidth - RtlLeadWitdh) then
          break;

        RtlRemainderWidth := CalculateRtlRemainder(APhysCharWidths, Length(ALine), x);

        LtrLead := PhysWidth - RtlLeadWitdh;
        PhysToViewedInRTLOffs := APhysToViewedXPos - LtrLead;

        if PhysToViewedInRTLOffs >= RtlLeadWitdh + RtlRemainderWidth then //NOT_IN_THIS_RUN;
          PhysToViewedInRTLOffs := 0;
      end;
    end;

    if (PhysToViewedInRTLOffs > 0) then begin
      if (PhysToViewedInRTLOffs > RtlRemainderWidth) or
         ( (FCaretWrapPos = wcpBOL) and (PhysToViewedInRTLOffs = RtlRemainderWidth) )
      then begin // within RtlLeadWitdh on the right side
        //APhysToViewedXPos := APhysToViewedXPos - RtlRemainderWidth;
        APhysToViewedXPos := LtrLead + PhysToViewedInRTLOffs - RtlRemainderWidth;
        break;
      end;
    end
    else begin
      if (FCaretWrapPos = wcpBOL) and (PhysWidth = APhysToViewedXPos) and (x < Length(ALine))
      then begin
        inc(Result);
        APhysToViewedXPos := APhysToViewedXPos - PhysWidth;
        break;
      end;
      if PhysWidth >= APhysToViewedXPos then
        break;
      APhysToViewedXPos := APhysToViewedXPos - PhysWidth;
    end;
  end;
  APhysToViewedXPos := ToPos(APhysToViewedXPos);
end;

procedure TLazSynEditLineWrapPlugin.GetWrapInfoForViewedXY(
  var AViewedXY: TViewedPoint; AFlags: TViewedXYInfoFlags;
  out AFirstViewedX: IntPos; ALogPhysConvertor: TSynLogicalPhysicalConvertor);
var
  SubLineOffset, YIdx: TLineIdx;
  LineTxt: String;
  PWidth: TPhysicalCharWidths;
  BoundLogX, NextBoundLogX, BoundPhysX: IntIdx;
  PhysWidth, WrapInd, AMaxWidth, PhysXPos: Integer;
begin

  YIdx := FLineMapView.LineMappingData.GetLineForForWrap(ToIdx(AViewedXY.y), SubLineOffset);
  YIdx := FLineMapView.NextLines.ViewToTextIndex(YIdx);

  LineTxt := FLineMapView.Strings[YIdx];
  ALogPhysConvertor.CurrentLine := YIdx;
  PWidth  := ALogPhysConvertor.CurrentWidthsDirect;
  AMaxWidth := WrapColumn;
  WrapInd := CalculateIndentFor(PChar(LineTxt), AMaxWidth, PWidth);

  GetSublineBounds(LineTxt, AMaxWidth, WrapInd, PWidth, SubLineOffset, BoundLogX, NextBoundLogX, BoundPhysX, PhysWidth, AViewedXY.x, PhysXPos);

  if SubLineOffset = 0 then
    AFirstViewedX := 0
  else
  case CaretWrapPos of
    wcpEOL: begin
        AFirstViewedX := 2 + WrapInd;
      end;
    wcpBOL: begin
        AFirstViewedX := 1 + WrapInd;
      end;
  end;

  AViewedXY.x := PhysXPos;
  AViewedXY.y := ToPos(YIdx);
end;

function TLazSynEditLineWrapPlugin.TextXYToLineXY(ATextXY: TPhysPoint
  ): TViewedSubPoint_0;
var
  AMaxWidth, WrapInd: Integer;
  ALine: String;
  APhysCharWidths: TPhysicalCharWidths;
begin
  FLineMapView.LogPhysConvertor.CurrentLine := ATextXY.y;
  ALine := FLineMapView.NextLines.Strings[ATextXY.y];
  APhysCharWidths := FLineMapView.LogPhysConvertor.CurrentWidthsDirect;
  AMaxWidth := WrapColumn;
  WrapInd := CalculateIndentFor(PChar(ALine), AMaxWidth, APhysCharWidths);
  Result.x := ATextXY.x;
  Result.y :=
    GetSubLineFromX(ALine, AMaxWidth, WrapInd, APhysCharWidths, Result.x);
  //if Result.x <> ATextXY.x then
  if Result.y > 0 then
    Result.x := Result.x + WrapInd; // Result is on sub-line
end;

function TLazSynEditLineWrapPlugin.ViewedSubLineXYToTextX(ARealLine: IntPos;
  ALineXY: TViewedSubPoint_0): Integer;
var
  AMaxWidth, WrapInd, ANextLogX, APhysWidth: Integer;
  ALine: String;
  APhysCharWidths: TPhysicalCharWidths;
  dummy, PhysXPos: integer;
begin
  FLineMapView.LogPhysConvertor.CurrentLine := ARealLine;
  ALine := FLineMapView.NextLines.Strings[ARealLine];
  APhysCharWidths := FLineMapView.LogPhysConvertor.CurrentWidthsDirect;
  AMaxWidth := WrapColumn;
  WrapInd := CalculateIndentFor(PChar(ALine), AMaxWidth, APhysCharWidths);

  GetSublineBounds(ALine, AMaxWidth, WrapInd, APhysCharWidths, ALineXY.y, dummy, ANextLogX, Result, APhysWidth, ALineXY.x, PhysXPos);
  Result := PhysXPos;
end;

function TLazSynEditLineWrapPlugin.CalculateWrapForLine(ALineIdx: IntIdx;
  AMaxWidth: integer): Integer;
begin
  FLineMapView.LogPhysConvertor.CurrentLine := ALineIdx;
  Result := GetSublineCount(FLineMapView.NextLines.Strings[ALineIdx], AMaxWidth,
    FLineMapView.LogPhysConvertor.CurrentWidthsDirect);
end;

constructor TLazSynEditLineWrapPlugin.Create(AOwner: TComponent);
var
  Fld: TSynEditStringsLinked;
begin
  inherited Create(AOwner);

  FMarkupInfoWrapSubLine := TSynSelectedColor.Create;
  FMarkupInfoWrapIndent  := TSynSelectedColor.Create;
  FMarkupInfoWrapEol     := TSynSelectedColor.Create;
  FMarkupInfoWrapSubLine.Clear;
  FMarkupInfoWrapIndent.Clear;
  FMarkupInfoWrapEol.Clear;
  FMarkupInfoWrapIndent.FrameEdges := sfeLeft;
  FMarkupInfoWrapIndent.OnChange := @DoMarkupChanged;

  FKeystrokes := TSynEditLineMapKeyStrokes.Create(Self);

  FLineMapView := TLazSynWordWrapView.Create(Self);

  Fld := TSynEdit(Editor).TextViewsManager.SynTextViewByClass[TSynEditFoldedView];
  if Fld <> nil then
    TSynEdit(Editor).TextViewsManager.AddTextView(FLineMapView, TSynEdit(Editor).TextViewsManager.IndexOf(Fld))
  else
    TSynEdit(Editor).TextViewsManager.AddTextView(FLineMapView);

  FLineMapView.AddLinesChangedHandler(@DoLinesChanged);
  TSynEdit(Editor).RegisterStatusChangedHandler(@DoWidthChanged, [scCharsInWindow]);
  TSynEdit(Editor).RegisterStatusChangedHandler(@DoHandleCreated, [scHandleCreated]);
  FMinWrapWidth := 1;
  FLineMapView.KnownLengthOfLongestLine := WrapColumn;
  WrapAll;
end;

destructor TLazSynEditLineWrapPlugin.Destroy;
begin
  Application.RemoveAsyncCalls(Self);
  if Editor <> nil then begin
    TSynEdit(Editor).UnRegisterStatusChangedHandler(@DoWidthChanged);
    TSynEdit(Editor).UnRegisterStatusChangedHandler(@DoHandleCreated);
    if (FLineMapView <> nil) and not (csDestroying in Editor.Componentstate) then begin
      TSynEdit(Editor).TextViewsManager.RemoveSynTextView(FLineMapView, True);
      TSynEdit(Editor).Invalidate;
    end;
   end;
  inherited Destroy;

  FMarkupInfoWrapSubLine.Free;
  FMarkupInfoWrapIndent.Free;
  FMarkupInfoWrapEol.Free;
  FKeystrokes.Free;
end;

procedure TLazSynEditLineWrapPlugin.WrapAll;
var
  c: Integer;
begin
  FLineMapView.LineMappingData.Clear;
  c := FLineMapView.NextLines.Count;
  if c > 0 then
    FLineMapView.LineMappingData.AdjustForLinesInserted(0, c, 0);
  ValidateAll;
end;

procedure TLazSynEditLineWrapPlugin.ValidateAll;
var
  AMaxWidth, i, w: Integer;
  LowLine, HighLine, TopViewIdx, TopLineIdx, TopSubLine: TLineIdx;
  tsub: TLineRange;
begin
  if not FLineMapView.LineMappingData.NeedsValidation then exit;
  if not Editor.HandleAllocated then exit;

  TopViewIdx := ToIdx(TSynEdit(Editor).TopView);
  TopLineIdx := ViewedTextBuffer.DisplayView.ViewToTextIndexEx(TopViewIdx, tsub);
  TopSubLine := TopViewIdx - tsub.Top;

  AMaxWidth := WrapColumn;

  while FLineMapView.LineMappingData.NextBlockForValidation(LowLine, HighLine) do begin
    for i := LowLine to HighLine do begin
      w := CalculateWrapForLine(i, AMaxWidth);
      FLineMapView.LineMappingData.ValidateLine(i, w);
    end;
  end;
  FLineMapView.LineMappingData.EndValidate;
  FLineMapView.SendNotification(senrLineMappingChanged, FLineMapView, 0, 0);

  tsub := ViewedTextBuffer.DisplayView.TextToViewIndex(TopLineIdx);
  TSynEdit(Editor).Topview := ToPos(tsub.Top + Min(TopSubLine, tsub.Bottom - tsub.Top));

  TSynEdit(Editor).Invalidate;
end;

{ TLazSynWordWrapMapAVLTree }

function TLazSynWordWrapMapAVLTree.CreateNode(APosition: Integer): TSynSizedDifferentialAVLNode;
begin
  Result := TSynEditLineMapPage.Create(Self, FWrapPlugin);
end;

{ TLazSynWordWrapView }

function TLazSynWordWrapView.CreateDisplayViewEx: TLazSynDisplayLineMapping;
begin
  Result := TLazSynDisplayWordWrap.Create(Self, FWrapPlugin);
end;

function TLazSynWordWrapView.CreateLineMappingData: TSynLineMapAVLTree;
begin
  Result := TLazSynWordWrapMapAVLTree.Create;
  TLazSynWordWrapMapAVLTree(Result).FWrapPlugin := FWrapPlugin;
end;

constructor TLazSynWordWrapView.Create(AWrapPlugin: TLazSynEditLineWrapPlugin);
begin
  FWrapPlugin := AWrapPlugin;
  inherited Create;
end;

procedure TLazSynWordWrapView.InternalGetInfoForViewedXY(AViewedXY: TViewedPoint;
  AFlags: TViewedXYInfoFlags; out AViewedXYInfo: TViewedXYInfo;
  ALogPhysConvertor: TSynLogicalPhysicalConvertor);
var
  FirstViewedX: IntPos;
begin
  FWrapPlugin.GetWrapInfoForViewedXY(AViewedXY, AFlags, FirstViewedX, ALogPhysConvertor);

  inherited InternalGetInfoForViewedXY(AViewedXY, AFlags, AViewedXYInfo,
    ALogPhysConvertor);

  AViewedXYInfo.CorrectedViewedXY :=
    YToPos(LineMappingData.TextXYIdxToViewXYIdx(YToIdx(AViewedXYInfo.CorrectedViewedXY)));
  AViewedXYInfo.FirstViewedX := FirstViewedX;
end;

const
  SynPluginLineWrapCommandStrs: array[0..ecSynPLineWrapCount-1] of TIdentMapEntry = (
    (Value: ecSynPLineWrapLineStart;       Name: 'ecSynPLineWrapLineStart'),
    (Value: ecSynPLineWrapLineEnd;         Name: 'ecSynPLineWrapLineEnd'),
    (Value: ecSynPLineWrapSelLineStart;    Name: 'ecSynPLineWrapSelLineStart'),
    (Value: ecSynPLineWrapSelLineEnd;      Name: 'ecSynPLineWrapSelLineEnd'),
    (Value: ecSynPLineWrapColSelLineStart; Name: 'ecSynPLineWrapColSelLineStart'),
    (Value: ecSynPLineWrapColSelLineEnd;   Name: 'ecSynPLineWrapColSelLineEnd')
  );

function IdentToLineWrapCommand(const Ident: string; var Cmd: longint): boolean;
begin
  Result := IdentToInt(Ident, Cmd, SynPluginLineWrapCommandStrs);
end;

function LineWrapCommandToIdent(Cmd: longint; var Ident: string): boolean;
begin
  Result := (Cmd >= ecPluginFirstLineWrap) and (Cmd - ecPluginFirstLineWrap < ecSynPLineWrapCount);
  if not Result then exit;
  Result := IntToIdent(Cmd, Ident, SynPluginLineWrapCommandStrs);
end;

procedure GetEditorCommandValues(Proc: TGetStrProc);
var
  i: integer;
begin
  for i := Low(SynPluginLineWrapCommandStrs) to High(SynPluginLineWrapCommandStrs) do
    Proc(SynPluginLineWrapCommandStrs[I].Name);
end;


initialization
  RegisterKeyCmdIdentProcs(@IdentToLineWrapCommand,
                           @LineWrapCommandToIdent);
  RegisterExtraGetEditorCommandValues(@GetEditorCommandValues);

end.

