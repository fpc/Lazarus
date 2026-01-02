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
unit SynEditMarkupBracket;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, SynEditMarkup, SynEditMiscClasses, SynEditTypes,
  LazSynEditText, LazEditTextAttributes, LazEditMatchingBracketUtils, LazEditTypes,
  LazEditASyncRunner;

type
  TSynEditBracketHighlightStyle = (
    sbhsLeftOfCursor,
    sbhsRightOfCursor,
    sbhsBoth
  );

  { TSynEditMarkupBracket }

  TSynEditMarkupBracket = class(TSynEditMarkup)
  private
    // Physical Position
    FBracketHighlightPos: TLogTokenPos;
    FBracketHighlightAntiPos: TLogTokenPos;
    FHighlightStyle: TSynEditBracketHighlightStyle;
    FNeedInvalidate: Boolean;
    FBracketChars: Tcharset;
    FBracketFinder: TBracketFinderAsync;
    procedure DoBracketSearchDone(ASender: TBracketFinderAsync);
    procedure SetHighlightStyle(const AValue: TSynEditBracketHighlightStyle);
    procedure DoHighlighterChanged(pSender: TSynEditStrings; pIndex, pCount: Integer);
  protected
    procedure SetLines(const AValue: TSynEditStringsLinked); override;
    procedure SetMatchingBracketPair(StartBracket, EndBracket: TLogTokenPos);
    procedure FindMatchingBracketPair(LogCaret: TPoint);
    procedure DoCaretChanged(Sender: TObject); override;
    procedure DoTextChanged(StartLine, EndLine, ACountDiff: Integer); override;
    procedure DoMarkupChanged(AMarkup: TLazEditTextAttribute); override;
    procedure DoEnabledChanged(Sender: TObject); override;
    procedure DoVisibleChanged(AVisible: Boolean); override;
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;
    procedure DecPaintLock; override;

    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const AnRtlInfo: TLazSynDisplayRtlInfo): TLazEditTextAttributeModifier; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;

    procedure InvalidateBracketHighlight;
    property HighlightStyle: TSynEditBracketHighlightStyle read FHighlightStyle write SetHighlightStyle;
  end;

implementation

{ TSynEditMarkupBracket }

constructor TSynEditMarkupBracket.Create(ASynEdit : TSynEditBase);
begin
  inherited Create(ASynEdit);
  FBracketHighlightPos.Y := -1;
  FBracketHighlightAntiPos.Y := -1;
  FHighlightStyle := sbhsBoth;
  MarkupInfo.Foreground := clNone;
  MarkupInfo.Background := clNone;
  MarkupInfo.Style := [fsBold];
  MarkupInfo.StyleMask := [];
  FBracketChars := GetBracketCharSet(SynEdit.Highlighter);
end;

destructor TSynEditMarkupBracket.Destroy;
begin
  FBracketFinder.Destroy;
  if Lines <> nil then
    Lines.RemoveChangeHandler(senrHighlightChanged, @DoHighlighterChanged);
  inherited Destroy;
end;

procedure TSynEditMarkupBracket.DecPaintLock;
begin
  inherited DecPaintLock;
  if (FPaintLock = 0) and FNeedInvalidate then
    InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.SetHighlightStyle(
  const AValue: TSynEditBracketHighlightStyle);
begin
  if FHighlightStyle <> AValue then
  begin
    FHighlightStyle := AValue;
    InvalidateBracketHighlight;
  end;
end;

procedure TSynEditMarkupBracket.DoHighlighterChanged(pSender: TSynEditStrings; pIndex,
  pCount: Integer);
begin
  if pIndex < 0 then
    FBracketChars := GetBracketCharSet(SynEdit.Highlighter);
end;

procedure TSynEditMarkupBracket.SetLines(const AValue: TSynEditStringsLinked);
begin
  if Lines <> nil then
    Lines.RemoveChangeHandler(senrHighlightChanged, @DoHighlighterChanged);
  inherited SetLines(AValue);
  if Lines <> nil then
    Lines.AddChangeHandler(senrHighlightChanged, @DoHighlighterChanged);
end;

procedure TSynEditMarkupBracket.SetMatchingBracketPair(StartBracket, EndBracket: TLogTokenPos);
var
  SwapPos: TLogTokenPos;
begin
  if (EndBracket.Y > 0)
  and ((EndBracket.Y < StartBracket.Y) or ((EndBracket.Y = StartBracket.Y) and (EndBracket.X < StartBracket.X)))
  then begin
    SwapPos    := EndBracket;
    EndBracket := StartBracket;
    StartBracket     := SwapPos;
  end;

  // invalidate old bracket highlighting, if changed
  if (FBracketHighlightPos.Y > 0)
  and ((FBracketHighlightPos.Y <> StartBracket.Y) or (FBracketHighlightPos.X <> StartBracket.X))
  then begin
    //DebugLn('TCustomSynEdit.InvalidateBracketHighlight A Y=',dbgs(FBracketHighlightPos));
    InvalidateSynLines(FBracketHighlightPos.Y,FBracketHighlightPos.Y);
  end;

  if (FBracketHighlightAntiPos.Y > 0)
  and (FBracketHighlightPos.Y <> FBracketHighlightAntiPos.Y)
  and ((FBracketHighlightAntiPos.Y <> EndBracket.Y) or (FBracketHighlightAntiPos.X <> EndBracket.X))
  then
    InvalidateSynLines(FBracketHighlightAntiPos.Y,FBracketHighlightAntiPos.Y);

  // invalidate new bracket highlighting, if changed
  if StartBracket.Y>0 then begin
    //DebugLn('TCustomSynEdit.InvalidateBracketHighlight C Y=',dbgs(StartBracket.Y),' X=',dbgs(StartBracket.X),' Y=',dbgs(EndBracket.Y),' X=',dbgs(EndBracket.X));
    if ((FBracketHighlightPos.Y <> StartBracket.Y) or (FBracketHighlightPos.X <> StartBracket.X))
    then InvalidateSynLines(StartBracket.Y, StartBracket.Y);

    if ((StartBracket.Y <> EndBracket.Y)
        or ((FBracketHighlightPos.Y = StartBracket.Y) and (FBracketHighlightPos.X = StartBracket.X))
       )
    and ((FBracketHighlightAntiPos.Y <> EndBracket.Y) or (FBracketHighlightAntiPos.X <> EndBracket.X))
    then InvalidateSynLines(EndBracket.Y, EndBracket.Y);
  end;
  FBracketHighlightPos     := StartBracket;
  FBracketHighlightAntiPos := EndBracket;
end;

procedure TSynEditMarkupBracket.DoBracketSearchDone(ASender: TBracketFinderAsync);
begin
  if (not ASender.CloseBracketFound) then begin
    SetMatchingBracketPair(Point(-1,-1), Point(-1,-1));
    exit;
  end;

  SetMatchingBracketPair(ASender.OpenBracketPos, ASender.CloseBracketPos);
end;

procedure TSynEditMarkupBracket.FindMatchingBracketPair(LogCaret: TPoint);
var
  StartLine: String;
  dir: TLazEditBracketSearchDirection;
begin
  SetMatchingBracketPair(Point(-1,-1), Point(-1,-1));
  if (LogCaret.Y < 1) or (LogCaret.Y > Lines.Count) or (LogCaret.X < 1) then
    Exit;

  StartLine := Lines[LogCaret.Y - 1];
  case HighlightStyle of
    sbhsLeftOfCursor:  begin
      dir := bsdLeft;
      if (LogCaret.X < 2) or (LogCaret.X-1 > Length(StartLine)) or
         not (StartLine[LogCaret.X-1] in FBracketChars)
      then
        exit;
    end;
    sbhsRightOfCursor: begin
      dir := bsdRightOrPartRight;
      if (LogCaret.X > Length(StartLine)) or (not (StartLine[LogCaret.X] in FBracketChars))
      then
        exit;
    end;
    sbhsBoth:          begin
      if not(
        ( (LogCaret.X <= Length(StartLine)) and (StartLine[LogCaret.X] in FBracketChars) ) or
        ( (LogCaret.X > 1) and (LogCaret.X-1 <= Length(StartLine)) and (StartLine[LogCaret.X-1] in FBracketChars) )
      )
      then
        exit;
      dir := bsdLeftThenRight;
    end;
  end;

  FBracketFinder.Cancel;
  FBracketFinder.Run(SynEdit, LogCaret, dir, @DoBracketSearchDone,
    SynEdit.Highlighter, Lines, [tpPaintExtra]);
end;

procedure TSynEditMarkupBracket.DoCaretChanged(Sender: TObject);
begin
  InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
begin
  InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.DoMarkupChanged(AMarkup: TLazEditTextAttribute);
begin
  InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.DoEnabledChanged(Sender: TObject);
begin
  InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.DoVisibleChanged(AVisible: Boolean);
begin
  inherited DoVisibleChanged(AVisible);
  if SynEdit.IsVisible then
    InvalidateBracketHighlight;
end;

procedure TSynEditMarkupBracket.InvalidateBracketHighlight;
begin
  FBracketFinder.Cancel;
  FNeedInvalidate := True;
  if (Caret = nil) or (not SynEdit.HandleAllocated) or (FPaintLock > 0) or
     (not SynEdit.IsVisible)
  then
    exit;

  FNeedInvalidate := False;
  if eoBracketHighlight in SynEdit.Options then
    FindMatchingBracketPair(Caret.LineBytePos);
end;

function TSynEditMarkupBracket.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TLazEditTextAttributeModifier;
begin
  Result := nil;
  if ((FBracketHighlightPos.y = aRow) and  (aStartCol.Logical >= FBracketHighlightPos.x) and
      (aStartCol.Logical < FBracketHighlightPos.x + FBracketHighlightPos.Len)
     )
  or ((FBracketHighlightAntiPos.y = aRow) and (aStartCol.Logical >= FBracketHighlightAntiPos.x) and
      (aStartCol.Logical < FBracketHighlightAntiPos.x + FBracketHighlightAntiPos.Len)
     )
  then begin
    Result := MarkupInfo;
    MarkupInfo.SetFrameBoundsLog(aStartCol.Logical, aStartCol.Logical + 1); // bracket is alvays 1 byte
  end;
end;

procedure TSynEditMarkupBracket.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
  ANextLog: Integer);
begin
  ANextLog := -1;
  ANextPhys := -1;
  if (FBracketHighlightPos.y = aRow) then begin
    if  (FBracketHighlightPos.x > aStartCol.Logical )
    then ANextLog := FBracketHighlightPos.x
    else if  (FBracketHighlightPos.x + FBracketHighlightPos.Len > aStartCol.Logical )
    then ANextLog := FBracketHighlightPos.x + FBracketHighlightPos.Len; // end of bracket
  end;
  if (FBracketHighlightAntiPos.y = aRow) then begin
    if  (FBracketHighlightAntiPos.x > aStartCol.Logical )
    and ((FBracketHighlightAntiPos.x < ANextLog) or (ANextLog < 0))
    then ANextLog := FBracketHighlightAntiPos.x
    else if  (FBracketHighlightAntiPos.x + FBracketHighlightAntiPos.Len > aStartCol.Logical )
    and ((FBracketHighlightAntiPos.x + FBracketHighlightAntiPos.Len < ANextLog) or (ANextLog < 0))
    then ANextLog := FBracketHighlightAntiPos.x + FBracketHighlightAntiPos.Len;
  end
end;

end.

