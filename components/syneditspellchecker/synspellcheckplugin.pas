{
 *****************************************************************************
  This file is part of the SynEditSpellChecker package from the Lazarus IDE.

  This content of this file is licensed: Modified LGPL-2
  Or at the users choice: Modified LGPL-3
  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Alternatively, the contents of this file may be used under the terms of the
  Mozilla Public License Version 1.1 or 2.0 http://www.mozilla.org/MPL/

  A copy used under either License can have the other Licenses removed from this
  header. A note should be added that the original file is available with the
  above choice of License.
  Used units may have to be amended according to the choice.
 *****************************************************************************
}
unit SynSpellCheckPlugin;

{$mode objfpc}{$H+}

interface

uses
  // LCL
  Classes, SysUtils, Graphics, Menus,
  // LazEdit
  LazEditMiscProcs, LazEditHighlighter, LazEditTypes, LazEditASyncRunner,
  // SynEdit
  SynEdit, SynEditMarkupHighAll, SynEditMiscClasses, SynEditMouseCmds, SynGutterLineOverview,
  LazSynEditText,
  // SynSpell
  SynSpellDictionary, SynSpellCheckWordBreaker, LazLoggerBase;

type

  { TSynPluginSpellCheckMouseActions }

  TSynPluginSpellCheckMouseActions = class(TSynEditMouseActions)
  public
    procedure ResetDefaults; override;
    procedure AddCmdSuggestion(const AButton: TSynMouseButton; const AShift, AShiftMask: TShiftState);
  end;

  TSynSpellCheckHighlighterOption = (
    hoNoNumbers,
    hoNoKnownWords
  );
  TSynSpellCheckHighlighterOptions = set of TSynSpellCheckHighlighterOption;

  { TSynMarkupSpellCheck }

  TSynMarkupSpellCheck = class(TSynEditMarkupHighlightAllBase)
  strict private
    FWordBreaker: TSynSpellWordBreaker;
    FWordChecker: TSynSpellWordChecker;
  private
    FHighlighter: TLazEditCustomHighlighter;
    FHighlighterOpts: TSynSpellCheckHighlighterOptions;
    procedure DoConfChanged(Sender: TObject);
    procedure SetHighlighterOpts(AValue: TSynSpellCheckHighlighterOptions);
    procedure SetOverViewColor(AValue: TColor);
    procedure SetWordBreaker(AValue: TSynSpellWordBreaker);
    procedure SetWordChecker(AValue: TSynSpellWordChecker);
    procedure DoHighlightChanged(Sender: TSynEditStrings; AIndex, ACount : Integer);
  protected
    procedure SetLines(const AValue: TSynEditStringsLinked); override;
    function GetASyncPriority(AnSearchInVisibleLines: boolean): TTLazEditTaskPriorities; override;
    //function LimitedValidationStartLine: IntPos; override;
    function LimitedValidationEndLine: IntPos; override;
    procedure FindInitialize; override;
    function FindMatches(AStartPoint, AnEndPoint: TPoint; var AnIndex: Integer;
      AStopAfterLine: Integer = - 1; ABackward: Boolean = False): TPoint; override;
    function HasSearchData: Boolean; override;
    function SearchStringMaxLines: Integer; override;
  public
    destructor Destroy; override;
    function HasMatchAt(p : TPoint): Boolean;

    property OverViewColor: TColor write SetOverViewColor;
    property WordBreaker: TSynSpellWordBreaker read FWordBreaker write SetWordBreaker;
    property WordChecker: TSynSpellWordChecker read FWordChecker write SetWordChecker;
    property HighlighterOpts: TSynSpellCheckHighlighterOptions read FHighlighterOpts write SetHighlighterOpts;
  end;

  { TSynCustomPluginSpellCheck }

  TSynCustomPluginSpellCheck = class(TLazSynEditPlugin)
  strict private
    FMarkup: TSynMarkupSpellCheck;
    FMarkupInfo: TLazEditHighlighterAttributesModifier;
    FMouseActions: TSynPluginSpellCheckMouseActions;
    FPopupSuggestions: TPopupMenu;
    FSuggestionStart, FSuggestionEnd: TPoint;
    FAddDictWords: array of string;
    FWordBreaker: TSynSpellWordBreaker;
    FWordChecker: TSynSpellWordChecker;
  private
    FHighlighterOpts: TSynSpellCheckHighlighterOptions;
    FOverViewColor: TColor;
    procedure DoMarkupChanged(Sender: TObject);
    function GetSynSpellDict: TSynSpellDictionary;
    procedure MenuAddWordClicked(Sender: TObject);
    procedure MenuSuggestionClicked(Sender: TObject);
    procedure SetHighlighterOpts(AValue: TSynSpellCheckHighlighterOptions);
    procedure SetOverViewColor(AValue: TColor);
  protected
    procedure DoEditorRemoving(AValue: TCustomSynEdit); override;
    procedure DoEditorAdded(AValue: TCustomSynEdit); override;

    function DoHandleMouseAction(AnAction: TSynEditMouseAction; var AnInfo: TSynEditMouseActionInfo): boolean;
    function DoMouseActionSearch(var AnInfo: TSynEditMouseActionInfo;
      HandleActionProc: TSynEditMouseActionHandler): Boolean;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    procedure Setup(AWordBreakerClass: TSynSpellWordBreakerClass;
                    AWordCheckerClass: TSynSpellWordCheckerClass;
                    ADictionary: TSynSpellDictionary);

    property MarkupInfo : TLazEditHighlighterAttributesModifier read FMarkupInfo;
    property OverViewColor: TColor read FOverViewColor write SetOverViewColor;
    property MouseActions: TSynPluginSpellCheckMouseActions read FMouseActions;
    property SynSpellDict: TSynSpellDictionary read GetSynSpellDict;
    property WordBreaker: TSynSpellWordBreaker read FWordBreaker;
    property WordChecker: TSynSpellWordChecker read FWordChecker;
    property HighlighterOpts: TSynSpellCheckHighlighterOptions read FHighlighterOpts write SetHighlighterOpts;
  end;


  { TSynPluginSpellCheck }

  TSynPluginSpellCheck = class(TSynCustomPluginSpellCheck)
  private
    function GetWordBreaker: TSynSpellWordBreakerSimpleUtf8;
    function GetWordChecker: TSynSpellWordCheckerSimple;
  published
    constructor Create(AnOwner: TComponent); override;
    procedure Setup(ADictionary: TSynSpellDictionary); reintroduce;

    property WordBreaker: TSynSpellWordBreakerSimpleUtf8 read GetWordBreaker;
    property WordChecker: TSynSpellWordCheckerSimple read GetWordChecker;
  end;

implementation

const
  EMC_SPELLCHECK_MOUSE_CMD_COUNT = 1;

  emcSpellCheckMouseCmdSuggestions = 0;

  SpellCheckMouseCommandStrs: array[0..EMC_SPELLCHECK_MOUSE_CMD_COUNT-1] of TIdentMapEntry = (
    (Value: emcSpellCheckMouseCmdSuggestions;       Name: 'emcSpellCheckMouseCmdSuggestions')
  );

var
  emcSpellCheckMouseCommandOffset: integer;

function  IdentToSpellCheckMouseCommand(const Ident: string; var Cmd: longint): boolean;
begin
  Result := IdentToInt(Ident, Cmd, SpellCheckMouseCommandStrs);
  Cmd := Cmd + emcSpellCheckMouseCommandOffset;
end;

function  SpellCheckMouseCommandToIdent(Cmd: longint; var Ident: string): boolean;
begin
  Result := IntToIdent(Cmd - emcSpellCheckMouseCommandOffset, Ident, SpellCheckMouseCommandStrs);
end;

procedure GetSpellCheckMouseCommandValues(Proc: TGetStrProc);
var
  i: Integer;
begin
  for i := Low(SpellCheckMouseCommandStrs) to High(SpellCheckMouseCommandStrs) do
    Proc(SpellCheckMouseCommandStrs[I].Name);
end;

function EditorMouseCommandToDescriptionString(cmd: TSynEditorMouseCommand
  ): String;
begin
  case cmd - emcSpellCheckMouseCommandOffset of
    emcSpellCheckMouseCmdSuggestions:   Result := 'Open spelling suggestions popup';
    else
      Result := '';
  end;
end;

function EditorMouseCommandToConfigString(cmd: TSynEditorMouseCommand): String;
begin
  Result := '';
end;

{ TSynPluginSpellCheckMouseActions }

procedure TSynPluginSpellCheckMouseActions.ResetDefaults;
begin
  Clear;

  AddCommand(emcSpellCheckMouseCmdSuggestions + emcSpellCheckMouseCommandOffset,
    False, mbXRight,  ccSingle, cdUp, [crLastDownPos, crLastDownButton, crLastDownShift],
    [], [ssAlt, ssCtrl, ssShift]);
end;

procedure TSynPluginSpellCheckMouseActions.AddCmdSuggestion(const AButton: TSynMouseButton;
  const AShift, AShiftMask: TShiftState);
begin
  AddCommand(emcSpellCheckMouseCmdSuggestions + emcSpellCheckMouseCommandOffset,
    False, AButton,  ccSingle, cdUp, [crLastDownPos, crLastDownButton, crLastDownShift],
    AShift, AShiftMask);
end;

{ TSynMarkupSpellCheck }

procedure TSynMarkupSpellCheck.DoConfChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TSynMarkupSpellCheck.SetHighlighterOpts(AValue: TSynSpellCheckHighlighterOptions);
begin
  if FHighlighterOpts = AValue then Exit;
  FHighlighterOpts := AValue;

  if FHighlighterOpts <> [] then begin
    FHighlighter := SynEdit.Highlighter;
    Lines.AddChangeHandler(senrHighlightChanged, @DoHighlightChanged);
  end
  else
    FHighlighter := nil;

  Invalidate;
end;

procedure TSynMarkupSpellCheck.DoHighlightChanged(Sender: TSynEditStrings; AIndex, ACount: Integer
  );
begin
  if FHighlighterOpts <> [] then begin
    FHighlighter := SynEdit.Highlighter;
    ValidateMatches;
  end;
end;

procedure TSynMarkupSpellCheck.SetLines(const AValue: TSynEditStringsLinked);
begin
  if Lines <> nil then
    Lines.RemoveChangeHandler(senrHighlightChanged, @DoHighlightChanged);
  inherited SetLines(AValue);
  if (Lines <> nil) and (FHighlighterOpts <> []) then begin
    FHighlighter := SynEdit.Highlighter;
    Lines.AddChangeHandler(senrHighlightChanged, @DoHighlightChanged);
  end;
end;

procedure TSynMarkupSpellCheck.SetOverViewColor(AValue: TColor);
var
  OG: TSynGutterLineOverview;
begin
  if AValue <> clNone then begin
    OG := TSynEdit(SynEdit).RightGutter.LineOverviewPart;
    if OG = nil then
      OG := TSynEdit(SynEdit).Gutter.LineOverviewPart;
    if OG <> nil then begin
      if OverViewGutterPart = nil then
        CreateOverviewGutterPart(OG, 14); // below "current word"
      OverViewGutterPart.Color := AValue;
      ScanMode := smsmASyncForceAll;
    end;
  end
  else
  if OverViewGutterPart <> nil then begin
    OverViewGutterPart.Destroy;
    ScanMode := smsmDirect;
  end;
end;

procedure TSynMarkupSpellCheck.SetWordBreaker(AValue: TSynSpellWordBreaker);
begin
  if FWordBreaker = AValue then Exit;
  if FWordBreaker <> nil then
    FWordBreaker.OnChanged := nil;
  FWordBreaker := AValue;
  if FWordBreaker <> nil then
    FWordBreaker.OnChanged := @DoConfChanged;
end;

procedure TSynMarkupSpellCheck.SetWordChecker(AValue: TSynSpellWordChecker);
begin
  if FWordChecker = AValue then Exit;
  if FWordChecker <> nil then
    FWordChecker.OnChanged := nil;
  FWordChecker := AValue;
  if FWordChecker <> nil then
    FWordChecker.OnChanged := @DoConfChanged;
end;

function TSynMarkupSpellCheck.GetASyncPriority(AnSearchInVisibleLines: boolean
  ): TTLazEditTaskPriorities;
begin
  if AnSearchInVisibleLines then
    Result := [tpPaintOverview, tpPriorTop]  // between tpPaintExtra and overview
  else
    Result := [tpPaintOverview];
end;

function TSynMarkupSpellCheck.LimitedValidationEndLine: IntPos;
begin
  Result := 0;
  if FHighlighter = nil then
    exit;

  FHighlighter.CurrentLines := Lines;
  Result := ToPos(FHighlighter.FirstUnpreparedLine - 1);
if Result > 0 then writeln('limit ', Result);
end;

procedure TSynMarkupSpellCheck.FindInitialize;
begin
  if (FWordBreaker = nil) or (FWordChecker = nil) then exit;
  FWordBreaker.GetReady;
  FWordChecker.GetReady;
end;

function TSynMarkupSpellCheck.FindMatches(AStartPoint, AnEndPoint: TPoint; var AnIndex: Integer;
  AStopAfterLine: Integer; ABackward: Boolean): TPoint;
var
  LineTxt: String;
  LineIdx, WordStart, WordLen, ErrLen, hx: Integer;
  ErrStart: IntPos;
  MoreErr: Boolean;
begin
  if (FWordBreaker = nil) or (FWordChecker = nil) then exit;

  if FHighlighter <> nil then
    FHighlighter.CurrentLines := Lines;

  LineIdx := ToIdx(AStartPoint.Y);
  repeat
    LineTxt := Lines[LineIdx];

    if FHighlighter <> nil then
      FHighlighter.StartAtLineIndex(LineIdx);

    WordBreaker.SetLine(LineTxt);
    while WordBreaker.NextWord(WordStart, WordLen) do begin
      if FHighlighter <> nil then begin
        FHighlighter.NextToLogX(ToIdx(WordStart)+1);
        hx := ToPos(FHighlighter.GetTokenPos);
        if (hx <= WordStart) and (hx + FHighlighter.GetTokenLen >= WordStart + WordLen) then begin
          if (hoNoNumbers in FHighlighterOpts) and
             (FHighlighter.GetTokenClass in [tcNumber])
          then
            continue;
          if (hoNoKnownWords in FHighlighterOpts) and
             (tdKnownWord in FHighlighter.GetTokenDetails)
          then
            continue;
        end;
      end;


      if WordChecker.CheckWord(LineTxt, WordStart, WordLen) then
        continue;

      repeat
        MoreErr := WordChecker.GetNextError(ErrStart, ErrLen, True);
        ErrStart := WordStart + ErrStart;
        Matches.Insert(AnIndex, Point(ErrStart, ToPos(LineIdx)), Point(ErrStart+ErrLen, ToPos(LineIdx)));
        inc(AnIndex);
      until not MoreErr;
    end;

    inc(LineIdx);
    if ( (AStopAfterLine > 0) and (LineIdx > ToIdx(AStopAfterLine)) ) or
       (GetTickCount64 > AsyncEndTickTime)
    then begin
      Result := Point(1, ToPos(LineIdx));
      exit;
    end;

  until (LineIdx >= ToIdx(AnEndPoint.Y));
  Result := AnEndPoint;
end;

function TSynMarkupSpellCheck.HasSearchData: Boolean;
begin
  Result := (FWordBreaker <> nil) and FWordBreaker.GetReady and
            (FWordChecker <> nil) and FWordChecker.GetReady;
end;

function TSynMarkupSpellCheck.SearchStringMaxLines: Integer;
begin
  Result := 1;
end;

destructor TSynMarkupSpellCheck.Destroy;
begin
  inherited Destroy;
  if Lines <> nil then
    Lines.RemoveChangeHandler(senrHighlightChanged, @DoHighlightChanged);
end;

function TSynMarkupSpellCheck.HasMatchAt(p: TPoint): Boolean;
var
  i: Integer;
begin
  i := Matches.IndexOf(p);
  Result := (i >= 0) and
    (Matches.StartPoint[i] <= p) and
    (Matches.EndPoint[i] >= p);
end;

{ TSynCustomPluginSpellCheck }

procedure TSynCustomPluginSpellCheck.DoMarkupChanged(Sender: TObject);
begin
  if FMarkup <> nil then
    FMarkup.MarkupInfo.Assign(FMarkupInfo);
end;

function TSynCustomPluginSpellCheck.GetSynSpellDict: TSynSpellDictionary;
begin
  Result := FWordChecker.SynSpellDictionary;
end;

procedure TSynCustomPluginSpellCheck.MenuAddWordClicked(Sender: TObject);
var
  i: PtrInt;
begin
  i := TMenuItem(Sender).Tag;
  if (i >= 0) and (i < Length(FAddDictWords)) then begin
    SynSpellDict.AddToPersonal(FAddDictWords[i]);
    SynSpellDict.SavePersonal;
  end;
end;

procedure TSynCustomPluginSpellCheck.MenuSuggestionClicked(Sender: TObject);
begin
  if FSuggestionStart.Y < 0 then exit;
  Editor.SetTextBetweenPoints(FSuggestionStart, FSuggestionEnd, TMenuItem(Sender).Caption,
    [setExtendBlock], scamAdjust);
end;

procedure TSynCustomPluginSpellCheck.SetHighlighterOpts(AValue: TSynSpellCheckHighlighterOptions);
begin
  if FHighlighterOpts = AValue then Exit;
  FHighlighterOpts := AValue;

  if FMarkup <> nil then
    FMarkup.HighlighterOpts := FHighlighterOpts;
end;

procedure TSynCustomPluginSpellCheck.SetOverViewColor(AValue: TColor);
begin
  if FOverViewColor = AValue then Exit;
  FOverViewColor := AValue;

  if FMarkup <> nil then
    FMarkup.OverViewColor := FOverViewColor;
end;

procedure TSynCustomPluginSpellCheck.DoEditorRemoving(AValue: TCustomSynEdit);
begin
  AValue.UnregisterMouseActionSearchHandler(@DoMouseActionSearch);
  AValue.UnregisterMouseActionExecHandler(@DoHandleMouseAction);
  AValue.MarkupManager.RemoveMarkUp(FMarkup);

  FreeAndNil(FMarkup);
  inherited DoEditorRemoving(AValue);
end;

procedure TSynCustomPluginSpellCheck.DoEditorAdded(AValue: TCustomSynEdit);
begin
  inherited DoEditorAdded(AValue);
  FMarkup := TSynMarkupSpellCheck.Create(AValue);
  FMarkup.WordBreaker := FWordBreaker;
  FMarkup.WordChecker := FWordChecker;

  AValue.MarkupManager.AddMarkUp(FMarkup);
  FMarkup.OverViewColor := FOverViewColor;
  FMarkup.HighlighterOpts := FHighlighterOpts;
  AValue.RegisterMouseActionExecHandler(@DoHandleMouseAction);
  AValue.RegisterMouseActionSearchHandler(@DoMouseActionSearch);

  DoMarkupChanged(nil);
end;

function TSynCustomPluginSpellCheck.DoHandleMouseAction(AnAction: TSynEditMouseAction;
  var AnInfo: TSynEditMouseActionInfo): boolean;
var
  LineTxt: String;
  i, WordStart, WordLen, PartLen, x: Integer;
  PartStart: IntIdx;
  sg: TStringArray;
  mi: TMenuItem;
begin
  Result := False;
  if (FMarkup = nil) then
    exit;

  case AnAction.Command - emcSpellCheckMouseCommandOffset of
    emcSpellCheckMouseCmdSuggestions: begin
        Result := True;

        sg := nil;
        FAddDictWords := nil;
        FSuggestionStart.Y := -1;
        LineTxt := Editor.Lines[ToIdx(AnInfo.NewCaret.LinePos)];
        FWordBreaker.SetLine(LineTxt);
        x := AnInfo.NewCaret.BytePos;
        if FWordBreaker.WordAt(x, WordStart, WordLen) then begin
          SetLength(FAddDictWords, 1);
          SetString(FAddDictWords[0], @LineTxt[WordStart], WordLen);

          FWordChecker.SetWord(LineTxt, WordStart, WordLen);
          if FWordChecker.GetBoundaryAt(x - WordStart, PartStart, PartLen) and
             (PartLen > 0)
          then begin
            if (PartLen < WordLen) then begin
              SetLength(FAddDictWords, 2);
              SetString(FAddDictWords[1], @LineTxt[WordStart+PartStart], PartLen);
            end;

            if ssfSuggest in SynSpellDict.Features then
              sg := WordChecker.GetWordSuggestions(PartStart, PartLen, 15);
            FSuggestionStart := Point(WordStart, AnInfo.NewCaret.LinePos);
            FSuggestionEnd   := Point(WordStart+WordLen, AnInfo.NewCaret.LinePos);
          end;
        end;

        FPopupSuggestions.Items.Clear;

        if ssfPersonal in SynSpellDict.Features then begin
          for i := 0 to Length(FAddDictWords) - 1 do begin
            mi := TMenuItem.Create(Editor);
            mi.Caption := Format('Add %s', [FAddDictWords[i]]);
            mi.OnClick := @MenuAddWordClicked;
            mi.Tag := i;
            FPopupSuggestions.Items.Add(mi);
          end;

          if (Length(FAddDictWords) > 0) then
            FPopupSuggestions.Items.AddSeparator;
        end;

        if ssfSuggest in SynSpellDict.Features then begin
          if Length(sg) = 0 then begin
            mi := TMenuItem.Create(Editor);
            mi.Caption := 'No suggestions';
            FPopupSuggestions.Items.Add(mi);
          end;

          for i := 0 to Length(sg) - 1 do begin
            mi := TMenuItem.Create(Editor);
            mi.Caption := sg[i];
            mi.OnClick := @MenuSuggestionClicked;
            FPopupSuggestions.Items.Add(mi);
          end;
        end;

        if FPopupSuggestions.Items.Count = 0 then
          exit(False);

        AnInfo.ActionResult.DoPopUpEvent := FPopupSuggestions.Items.Count > 0;
        AnInfo.ActionResult.PopUpEventY := AnInfo.NewCaret.LinePos;
        AnInfo.ActionResult.PopUpEventX := AnInfo.NewCaret.CharPos;
        AnInfo.ActionResult.PopUpMenu := FPopupSuggestions;
      end;
  end;
end;

function TSynCustomPluginSpellCheck.DoMouseActionSearch(var AnInfo: TSynEditMouseActionInfo;
  HandleActionProc: TSynEditMouseActionHandler): Boolean;
begin
  Result := (FMarkup <> nil) and FMarkup.HasMatchAt(AnInfo.NewCaret.LineBytePos);

  if Result then
    Result := HandleActionProc(FMouseActions, AnInfo);
end;

constructor TSynCustomPluginSpellCheck.Create(AnOwner: TComponent);
begin
  FMarkupInfo := TLazEditHighlighterAttributesModifier.Create;
  FMarkupInfo.AddChangeHandler(@DoMarkupChanged);

  FMouseActions := TSynPluginSpellCheckMouseActions.Create(nil);
  FMouseActions.ResetDefaults;

  FPopupSuggestions := TPopupMenu.Create(nil);

  inherited Create(AnOwner);
end;

destructor TSynCustomPluginSpellCheck.Destroy;
begin
  inherited Destroy;
  FMarkupInfo.RemoveChangeHandler(@DoMarkupChanged);
  FMarkupInfo.Free;
  FMarkup.Free;
  FMouseActions.Free;
  FPopupSuggestions.Free;
  FWordBreaker.Free;
  FWordChecker.Free;
end;

procedure TSynCustomPluginSpellCheck.Setup(AWordBreakerClass: TSynSpellWordBreakerClass;
  AWordCheckerClass: TSynSpellWordCheckerClass; ADictionary: TSynSpellDictionary);
begin
  FWordBreaker.Free;
  FWordChecker.Free;

  FWordBreaker := AWordBreakerClass.Create;
  FWordChecker := AWordCheckerClass.Create;
  FWordChecker.SynSpellDictionary := ADictionary;

  if FMarkup <> nil then begin
    FMarkup.WordBreaker  := FWordBreaker;
    FMarkup.WordChecker := FWordChecker;
  end;
end;

function TSynPluginSpellCheck.GetWordBreaker: TSynSpellWordBreakerSimpleUtf8;
begin
  Result := inherited WordBreaker as TSynSpellWordBreakerSimpleUtf8;
end;

function TSynPluginSpellCheck.GetWordChecker: TSynSpellWordCheckerSimple;
begin
  Result := inherited WordChecker as TSynSpellWordCheckerSimple;
end;

constructor TSynPluginSpellCheck.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);
  inherited Setup(TSynSpellWordBreakerSimpleUtf8, TSynSpellWordCheckerSimple, nil);
end;

procedure TSynPluginSpellCheck.Setup(ADictionary: TSynSpellDictionary);
begin
  WordChecker.SynSpellDictionary := ADictionary;
end;

initialization
  emcSpellCheckMouseCommandOffset := AllocatePluginMouseRange(EMC_SPELLCHECK_MOUSE_CMD_COUNT);
  RegisterMouseCmdIdentProcs(@IdentToSpellCheckMouseCommand,
                             @SpellCheckMouseCommandToIdent);
  RegisterExtraGetEditorMouseCommandValues(@GetSpellCheckMouseCommandValues);
  RegisterMouseCmdNameAndOptProcs(@EditorMouseCommandToDescriptionString,
                                  @EditorMouseCommandToConfigString);

end.

