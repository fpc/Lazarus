{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Abstract:
    TIDEDiffViewer shows the differences of two texts as unified diff,
    similar to GitLab: the source lines are shown syntax highlighted, removed
    lines with a red background, added lines with a green background, and
    the gutter shows the old and new line numbers.
    The diff is computed with codetools' DiffPatch.
    Colors, font and highlighter are taken from the IDE's editor options.
}
unit IDEDiffViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  // LCL
  LCLType, Graphics, Controls,
  // LazEdit
  LazEditTextGridPainter, LazEditTextAttributes, LazEditHighlighter,
  // SynEdit
  SynEdit, SynGutterBase, SynEditMarkupSpecialLine, SynHighlighterDiff,
  LazSynEditText, LazSynTextArea, SynEditMiscProcs, SynEditTypes,
  // CodeTools
  DiffPatch,
  // IdeIntf
  EditorSyntaxHighlighterDef,
  // IDE
  EditorOptions, SourceEditor;

type
  TIDEDiffLineKind = (
    dlkContext, // unchanged line
    dlkAdded,   // line only in new text
    dlkRemoved, // line only in old text
    dlkHunk     // hunk header @@ -a,b +c,d @@
    );

  TIDEDiffLineInfo = record
    Kind: TIDEDiffLineKind;
    OldLine: integer; // 1-based, 0 = none
    NewLine: integer; // 1-based, 0 = none
  end;
  TIDEDiffLineInfos = array of TIDEDiffLineInfo;

  TIDEDiffViewer = class;

const
  DefaultDiffContextLines = 3;

type

  { TIDEDiffGutterPart - shows old and new line numbers and +/- }

  TIDEDiffGutterPart = class(TSynGutterPartBase)
  private
    FViewer: TIDEDiffViewer;
    FDigits: integer;
  protected
    procedure Init; override;
    function PreferedWidthAtCurrentPPI: Integer; override;
    procedure FontChanged(Sender: TObject; Changes: TSynStatusChanges);
  public
    destructor Destroy; override;
    procedure Paint(Canvas: TCanvas; AClip: TRect; FirstLine, LastLine: integer); override;
    procedure UpdateDigits;
    property Viewer: TIDEDiffViewer read FViewer;
  end;

  { TIDEDiffViewer }

  TIDEDiffViewer = class(TCustomControl)
  private
    FContextLines: integer;
    FDiff: TDiffOutput;
    FGutterPart: TIDEDiffGutterPart;
    FHighlighterId: TIdeSyntaxHighlighterID;
    FLineInfos: TIDEDiffLineInfos;
    FSynEdit: TSynEdit;
    FAddedColor: TColor;
    FRemovedColor: TColor;
    FAddedGutterColor: TColor;
    FRemovedGutterColor: TColor;
    FHunkForeground: TColor;
    FHunkBackground: TColor;
    function GetLineInfo(Index: integer): TIDEDiffLineInfo;
    function GetLineCount: integer;
    function GetShowWholeText: boolean;
    procedure SetShowWholeText(const AValue: boolean);
    procedure SetContextLines(const AValue: integer);
    procedure SetHighlighterId(const AValue: TIdeSyntaxHighlighterID);
    procedure SynEditSpecialLineMarkup(Sender: TObject;
      const Info: TSpecialLineMarkupExInfo; var Special: boolean;
      Markup: TLazEditTextAttributeModifier);
    procedure UpdateDiffColors;
    procedure RebuildView;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure SetTexts(const OldText, NewText: string; Flags: TTextDiffFlags);
    procedure SetHighlighterByFilename(const Filename: string);
    procedure ApplyEditorOptions; // read IDE editor options
    function GetTextDiff: string; // classic text diff of last SetTexts
    function IsDiffStart(Index: integer): boolean; // 0-based, first line of a block of changed lines
    function FindDiff(FromLine: integer; Forward: boolean): integer; // 1-based, 0 if not found
    function HasDiffs: boolean;
    function GotoNextDiff: boolean;
    function GotoPrevDiff: boolean;
    function DiffBlockStart(Line: integer): integer; // 1-based, 0 if Line is not changed
    function DiffBlockEnd(Line: integer): integer; // 1-based, 0 if Line is not changed
    procedure ShowDiffBlock(StartLine: integer; Forward: boolean); // 1-based
    function GetLineColor(Kind: TIDEDiffLineKind; ForGutter: boolean): TColor;
    property ContextLines: integer read FContextLines write SetContextLines default DefaultDiffContextLines; // <0 means show all
    property ShowWholeText: boolean read GetShowWholeText write SetShowWholeText; // ContextLines<0
    property Diff: TDiffOutput read FDiff;
    property HighlighterId: TIdeSyntaxHighlighterID read FHighlighterId write SetHighlighterId;
    property LineCount: integer read GetLineCount;
    property LineInfos[Index: integer]: TIDEDiffLineInfo read GetLineInfo; // 0-based
    property SynEdit: TSynEdit read FSynEdit;
  end;

function BlendColors(Color1, Color2: TColor; Percent2: integer): TColor;

implementation

function BlendColors(Color1, Color2: TColor; Percent2: integer): TColor;
var
  c1, c2: TColor;
  r, g, b: integer;
begin
  c1 := ColorToRGB(Color1);
  c2 := ColorToRGB(Color2);
  r := (Red(c1)*(100-Percent2) + Red(c2)*Percent2) div 100;
  g := (Green(c1)*(100-Percent2) + Green(c2)*Percent2) div 100;
  b := (Blue(c1)*(100-Percent2) + Blue(c2)*Percent2) div 100;
  Result := RGBToColor(r, g, b);
end;

{ TIDEDiffGutterPart }

procedure TIDEDiffGutterPart.Init;
begin
  inherited Init;
  FriendEdit.RegisterStatusChangedHandler(@FontChanged, [scFontOrStyleChanged]);
end;

function TIDEDiffGutterPart.PreferedWidthAtCurrentPPI: Integer;
begin
  // old number, space, new number, space, sign, space
  Result := (2*FDigits + 4) * Gutter.TextDrawer.CharWidth + 2;
end;

procedure TIDEDiffGutterPart.FontChanged(Sender: TObject;
  Changes: TSynStatusChanges);
begin
  DoAutoSize;
end;

destructor TIDEDiffGutterPart.Destroy;
begin
  FriendEdit.UnRegisterStatusChangedHandler(@FontChanged);
  inherited Destroy;
end;

procedure TIDEDiffGutterPart.UpdateDigits;
var
  i, MaxLine, NewDigits: integer;
  Info: TIDEDiffLineInfo;
begin
  MaxLine := 0;
  if FViewer <> nil then
    for i := 0 to FViewer.LineCount-1 do begin
      Info := FViewer.FLineInfos[i];
      MaxLine := Max(MaxLine, Max(Info.OldLine, Info.NewLine));
    end;
  NewDigits := Max(2, length(IntToStr(MaxLine)));
  if NewDigits = FDigits then exit;
  FDigits := NewDigits;
  if AutoSize then
    DoAutoSize;
  DoChange(Self);
end;

procedure TIDEDiffGutterPart.Paint(Canvas: TCanvas; AClip: TRect; FirstLine,
  LastLine: integer);

  function NumToStr(n: integer): string;
  begin
    if n > 0 then
      Result := IntToStr(n)
    else
      Result := '';
    Result := StringOfChar(' ', FDigits-length(Result)) + Result;
  end;

var
  TextDrawer: TLazEditTextGridPainter;
  i, LineHeight, LineIdx, Cnt: integer;
  rcLine: TRect;
  LineRange: TLineRange;
  t: integer;
  s: string;
  Info: TIDEDiffLineInfo;
  BackColor: TColor;
begin
  if not Visible then exit;
  TextDrawer := Gutter.TextDrawer;
  LineHeight := SynEdit.LineHeight;
  Cnt := FViewer.LineCount;
  t := ToIdx(GutterArea.TextArea.TopViewedLine);
  TextDrawer.BeginCustomCanvas(Canvas);
  try
    TextDrawer.ForeColor := MarkupInfoInternal.Foreground;
    TextDrawer.SetFrame(clNone, slsSolid);
    TextDrawer.Style := MarkupInfoInternal.Style;
    rcLine := AClip;
    rcLine.Bottom := AClip.Top;
    for i := t + FirstLine to t + LastLine do
    begin
      LineIdx := ViewedTextBuffer.DisplayView.ViewToTextIndexEx(i, LineRange);
      if (LineIdx < 0) or (LineIdx >= TextBuffer.Count) then break;
      rcLine.Top := rcLine.Bottom;
      Inc(rcLine.Bottom, LineHeight);
      s := '';
      BackColor := MarkupInfoInternal.Background;
      if LineIdx < Cnt then begin
        Info := FViewer.FLineInfos[LineIdx];
        BackColor := FViewer.GetLineColor(Info.Kind, true);
        if BackColor = clNone then
          BackColor := MarkupInfoInternal.Background;
        if i = LineRange.Top then begin
          case Info.Kind of
          dlkContext: s := NumToStr(Info.OldLine) + ' ' + NumToStr(Info.NewLine) + '   ';
          dlkAdded: s := NumToStr(0) + ' ' + NumToStr(Info.NewLine) + ' + ';
          dlkRemoved: s := NumToStr(Info.OldLine) + ' ' + NumToStr(0) + ' - ';
          dlkHunk: s := StringOfChar(' ', FDigits-3) + '...';
          end;
        end;
      end;
      TextDrawer.BackColor := BackColor;
      TextDrawer.NewTextOut(rcLine.Left, rcLine.Top, ETO_OPAQUE or ETO_CLIPPED,
        rcLine, PChar(Pointer(s)), length(s), nil);
    end;

    // erase the remaining area
    if AClip.Bottom > rcLine.Bottom then
    begin
      TextDrawer.BackColor := MarkupInfoInternal.Background;
      rcLine.Top := rcLine.Bottom;
      rcLine.Bottom := AClip.Bottom;
      TextDrawer.FillRect(rcLine);
    end;
  finally
    TextDrawer.EndCustomCanvas;
  end;
end;

{ TIDEDiffViewer }

constructor TIDEDiffViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FContextLines := DefaultDiffContextLines;
  FHighlighterId := IdeHighlighterNoneID;
  FAddedColor := clNone;
  FRemovedColor := clNone;
  FAddedGutterColor := clNone;
  FRemovedGutterColor := clNone;
  FHunkForeground := clNone;
  FHunkBackground := clNone;

  FSynEdit := TSynEdit.Create(Self);
  FSynEdit.Name := 'DiffSynEdit';
  FSynEdit.Align := alClient;
  FSynEdit.ReadOnly := true;
  FSynEdit.OnSpecialLineMarkupEx := @SynEditSpecialLineMarkup;
  FSynEdit.Parent := Self;

  FGutterPart := TIDEDiffGutterPart.Create(FSynEdit.Gutter.Parts);
  FGutterPart.FViewer := Self;
  FGutterPart.Index := 0;
  FGutterPart.UpdateDigits;

  ApplyEditorOptions;
end;

destructor TIDEDiffViewer.Destroy;
begin
  FreeAndNil(FDiff);
  inherited Destroy;
end;

procedure TIDEDiffViewer.Clear;
begin
  FreeAndNil(FDiff);
  FLineInfos := nil;
  FSynEdit.Lines.Clear;
  FGutterPart.UpdateDigits;
  FSynEdit.Invalidate;
end;

function TIDEDiffViewer.GetLineInfo(Index: integer): TIDEDiffLineInfo;
begin
  Result := FLineInfos[Index];
end;

function TIDEDiffViewer.GetLineCount: integer;
begin
  Result := length(FLineInfos);
end;

function TIDEDiffViewer.GetShowWholeText: boolean;
begin
  Result := FContextLines < 0;
end;

procedure TIDEDiffViewer.SetShowWholeText(const AValue: boolean);
begin
  if AValue then
    ContextLines := -1
  else
    ContextLines := DefaultDiffContextLines;
end;

procedure TIDEDiffViewer.SetContextLines(const AValue: integer);
begin
  if FContextLines = AValue then exit;
  FContextLines := AValue;
  RebuildView;
end;

procedure TIDEDiffViewer.SetHighlighterId(const AValue: TIdeSyntaxHighlighterID);
begin
  if FHighlighterId = AValue then exit;
  FHighlighterId := AValue;
  ApplyEditorOptions;
end;

procedure TIDEDiffViewer.SetHighlighterByFilename(const Filename: string);
begin
  HighlighterId := FilenameToLazSyntaxHighlighter(Filename);
end;

procedure TIDEDiffViewer.SynEditSpecialLineMarkup(Sender: TObject;
  const Info: TSpecialLineMarkupExInfo; var Special: boolean;
  Markup: TLazEditTextAttributeModifier);
var
  Idx: integer;
  Kind: TIDEDiffLineKind;
begin
  Idx := Info.Line - 1;
  if (Idx < 0) or (Idx >= length(FLineInfos)) then exit;
  Kind := FLineInfos[Idx].Kind;
  case Kind of
  dlkAdded, dlkRemoved:
    begin
      Special := true;
      Markup.Foreground := clNone;
      Markup.Background := GetLineColor(Kind, false);
    end;
  dlkHunk:
    begin
      Special := true;
      Markup.Foreground := FHunkForeground;
      Markup.Background := FHunkBackground;
    end;
  end;
end;

function TIDEDiffViewer.GetLineColor(Kind: TIDEDiffLineKind; ForGutter: boolean
  ): TColor;
begin
  case Kind of
  dlkAdded:
    if ForGutter then
      Result := FAddedGutterColor
    else
      Result := FAddedColor;
  dlkRemoved:
    if ForGutter then
      Result := FRemovedGutterColor
    else
      Result := FRemovedColor;
  dlkHunk:
    Result := FHunkBackground;
  else
    Result := clNone;
  end;
end;

procedure TIDEDiffViewer.UpdateDiffColors;
var
  DiffHL: TLazEditCustomHighlighter;
  DiffSyn: TSynDiffSyn;
  EditBg, EditFg: TColor;

  function LineColor(Attr: TLazEditHighlighterAttributes; Default: TColor;
    Percent: integer): TColor;
  begin
    Result := clNone;
    if Attr <> nil then begin
      if Attr.Background <> clNone then
        exit(Attr.Background);
      Result := Attr.Foreground;
    end;
    if Result = clNone then
      Result := Default;
    Result := BlendColors(EditBg, Result, Percent);
  end;

begin
  EditBg := FSynEdit.Color;
  if (EditBg = clNone) or (EditBg = clDefault) then
    EditBg := clWindow;
  EditFg := FSynEdit.Font.Color;
  if (EditFg = clNone) or (EditFg = clDefault) then
    EditFg := clWindowText;

  DiffSyn := nil;
  DiffHL := EditorOpts.HighlighterList.SharedInstances[
              IdeSyntaxHighlighters.GetIdForLazSyntaxHighlighter(lshDiff)];
  if DiffHL is TSynDiffSyn then
    DiffSyn := TSynDiffSyn(DiffHL);

  if DiffSyn <> nil then begin
    FAddedColor := LineColor(DiffSyn.LineAddedAttri, clGreen, 15);
    FAddedGutterColor := LineColor(DiffSyn.LineAddedAttri, clGreen, 30);
    FRemovedColor := LineColor(DiffSyn.LineRemovedAttri, clRed, 15);
    FRemovedGutterColor := LineColor(DiffSyn.LineRemovedAttri, clRed, 30);
    FHunkBackground := LineColor(DiffSyn.ChunkMarkerAttri, clBlue, 10);
  end else begin
    FAddedColor := LineColor(nil, clGreen, 15);
    FAddedGutterColor := LineColor(nil, clGreen, 30);
    FRemovedColor := LineColor(nil, clRed, 15);
    FRemovedGutterColor := LineColor(nil, clRed, 30);
    FHunkBackground := LineColor(nil, clBlue, 10);
  end;
  FHunkForeground := BlendColors(EditFg, EditBg, 35);
end;

procedure TIDEDiffViewer.ApplyEditorOptions;
var
  Id: TIdeSyntaxHighlighterID;
begin
  FSynEdit.BeginUpdate(false);
  try
    Id := FHighlighterId;
    if (Id < 0) or (Id >= EditorOpts.HighlighterList.Count) then
      Id := IdeHighlighterNoneID;
    if EditorOpts.UseSyntaxHighlight and (Id <> IdeHighlighterNoneID) then
      FSynEdit.Highlighter := EditorOpts.HighlighterList.SharedSynInstances[Id]
    else
      FSynEdit.Highlighter := nil;

    EditorOpts.GetSynEditSettings(FSynEdit, nil, Id);

    // viewer specific settings
    FSynEdit.ReadOnly := true;
    FSynEdit.Gutter.Visible := true;
    if FSynEdit.Gutter.LineNumberPart <> nil then
      FSynEdit.Gutter.LineNumberPart.Visible := false;
    if FSynEdit.Gutter.ChangesPart <> nil then
      FSynEdit.Gutter.ChangesPart.Visible := false;
    if FSynEdit.Gutter.MarksPart <> nil then
      FSynEdit.Gutter.MarksPart.Visible := false;
    if FSynEdit.Gutter.CodeFoldPart <> nil then
      FSynEdit.Gutter.CodeFoldPart.Visible := false;
    FGutterPart.Visible := true;
    FGutterPart.AutoSize := true;
    FGutterPart.DoAutoSize;

    UpdateDiffColors;
  finally
    FSynEdit.EndUpdate;
  end;
  FSynEdit.Invalidate;
end;

function TIDEDiffViewer.GetTextDiff: string;
begin
  if FDiff = nil then
    Result := ''
  else
    Result := FDiff.CreateTextDiff;
end;

function TIDEDiffViewer.IsDiffStart(Index: integer): boolean;
begin
  Result := false;
  if (Index < 0) or (Index >= length(FLineInfos)) then exit;
  if not (FLineInfos[Index].Kind in [dlkAdded, dlkRemoved]) then exit;
  if Index = 0 then
    exit(true);
  Result := not (FLineInfos[Index-1].Kind in [dlkAdded, dlkRemoved]);
end;

function TIDEDiffViewer.FindDiff(FromLine: integer; Forward: boolean): integer;
var
  i: integer;
begin
  if Forward then begin
    for i := Max(0, FromLine) to length(FLineInfos)-1 do
      if IsDiffStart(i) then
        exit(i+1);
  end else begin
    for i := Min(length(FLineInfos), FromLine-1)-1 downto 0 do
      if IsDiffStart(i) then
        exit(i+1);
  end;
  Result := 0;
end;

function TIDEDiffViewer.HasDiffs: boolean;
begin
  Result := FindDiff(0, true) > 0;
end;

function TIDEDiffViewer.GotoNextDiff: boolean;
var
  Line: integer;
begin
  // search behind the caret
  Line := FindDiff(FSynEdit.CaretY, true);
  Result := Line > 0;
  if Result then
    ShowDiffBlock(Line, true);
end;

function TIDEDiffViewer.GotoPrevDiff: boolean;
var
  Line, BlockStart: integer;
begin
  // search in front of the block at the caret
  Line := FSynEdit.CaretY;
  BlockStart := DiffBlockStart(Line);
  if BlockStart > 0 then
    Line := BlockStart;
  Line := FindDiff(Line, false);
  Result := Line > 0;
  if Result then
    ShowDiffBlock(Line, false);
end;

function TIDEDiffViewer.DiffBlockStart(Line: integer): integer;
var
  Idx: integer;
begin
  Result := 0;
  Idx := Line-1;
  if (Idx < 0) or (Idx >= length(FLineInfos)) then exit;
  if not (FLineInfos[Idx].Kind in [dlkAdded, dlkRemoved]) then exit;
  while (Idx > 0) and (FLineInfos[Idx-1].Kind in [dlkAdded, dlkRemoved]) do
    dec(Idx);
  Result := Idx+1;
end;

function TIDEDiffViewer.DiffBlockEnd(Line: integer): integer;
var
  Idx: integer;
begin
  Result := 0;
  Idx := Line-1;
  if (Idx < 0) or (Idx >= length(FLineInfos)) then exit;
  if not (FLineInfos[Idx].Kind in [dlkAdded, dlkRemoved]) then exit;
  while (Idx+1 < length(FLineInfos))
      and (FLineInfos[Idx+1].Kind in [dlkAdded, dlkRemoved]) do
    inc(Idx);
  Result := Idx+1;
end;

procedure TIDEDiffViewer.ShowDiffBlock(StartLine: integer; Forward: boolean);
// Center the changed lines. If they do not fit, show the start when going
// forward and the end when going backward.
var
  EndLine, BlockLen, WinLines, NewTop: integer;
begin
  EndLine := DiffBlockEnd(StartLine);
  if EndLine < StartLine then
    EndLine := StartLine;
  BlockLen := EndLine-StartLine+1;
  WinLines := Max(1, FSynEdit.LinesInWindow);
  if BlockLen <= WinLines then begin
    NewTop := StartLine - (WinLines-BlockLen) div 2;
    FSynEdit.CaretXY := Point(1, StartLine);
  end else if Forward then begin
    NewTop := StartLine;
    FSynEdit.CaretXY := Point(1, StartLine);
  end else begin
    NewTop := EndLine-WinLines+1;
    FSynEdit.CaretXY := Point(1, EndLine);
  end;
  FSynEdit.TopLine := Max(1, NewTop);
end;

procedure TIDEDiffViewer.SetTexts(const OldText, NewText: string;
  Flags: TTextDiffFlags);
begin
  FreeAndNil(FDiff);
  FDiff := TDiffOutput.Create(OldText, NewText, Flags);
  FDiff.Compute;
  RebuildView;
end;

procedure TIDEDiffViewer.RebuildView;
var
  OldLines, NewLines: TDiffLines;
  AllInfos: TIDEDiffLineInfos;
  AllCount: integer;

  procedure AddInfo(aKind: TIDEDiffLineKind; aOld, aNew: integer);
  begin
    if AllCount = length(AllInfos) then
      SetLength(AllInfos, Max(64, 2*AllCount));
    AllInfos[AllCount].Kind := aKind;
    AllInfos[AllCount].OldLine := aOld;
    AllInfos[AllCount].NewLine := aNew;
    inc(AllCount);
  end;

  function GetLineText(const Info: TIDEDiffLineInfo): string;
  begin
    Result := '';
    if Info.NewLine > 0 then begin
      with NewLines[Info.NewLine-1] do
        Result := copy(FDiff.Text2, StartPos, EndPos-StartPos);
    end else if Info.OldLine > 0 then begin
      with OldLines[Info.OldLine-1] do
        Result := copy(FDiff.Text1, StartPos, EndPos-StartPos);
    end;
  end;

  procedure CollectAllLines;
  var
    i, k, n: integer;
    Change: TDiffChange;
    aOld, aNew: integer;
  begin
    for i := 0 to FDiff.Count-1 do begin
      Change := FDiff[i];
      case Change.Kind of
      dckNone:
        begin
          // with ignore flags the old and new range can differ in length
          n := Max(Change.OldLast-Change.OldFirst, Change.NewLast-Change.NewFirst);
          for k := 0 to n do begin
            aOld := Change.OldFirst+k;
            if aOld > Change.OldLast then
              aOld := 0;
            aNew := Change.NewFirst+k;
            if aNew > Change.NewLast then
              aNew := 0;
            AddInfo(dlkContext, aOld, aNew);
          end;
        end;
      dckAdd, dckDelete, dckModify:
        begin
          for k := Change.OldFirst to Change.OldLast do
            AddInfo(dlkRemoved, k, 0);
          for k := Change.NewFirst to Change.NewLast do
            AddInfo(dlkAdded, 0, k);
        end;
      end;
    end;
  end;

  function HunkHeader(FromIdx, ToIdx: integer): string;
  var
    i, OldFirst, OldCount, NewFirst, NewCount: integer;
  begin
    OldFirst := 0;
    OldCount := 0;
    NewFirst := 0;
    NewCount := 0;
    for i := FromIdx to ToIdx do begin
      if AllInfos[i].OldLine > 0 then begin
        if OldFirst = 0 then
          OldFirst := AllInfos[i].OldLine;
        inc(OldCount);
      end;
      if AllInfos[i].NewLine > 0 then begin
        if NewFirst = 0 then
          NewFirst := AllInfos[i].NewLine;
        inc(NewCount);
      end;
    end;
    Result := '@@ -' + IntToStr(OldFirst) + ',' + IntToStr(OldCount)
             + ' +' + IntToStr(NewFirst) + ',' + IntToStr(NewCount) + ' @@';
  end;

var
  ShowLine: array of boolean;
  i, j, Cnt, HunkStart, HunkEnd: integer;
  sl: TStringList;
  HasChanges: boolean;
begin
  FLineInfos := nil;
  sl := TStringList.Create;
  try
    if FDiff <> nil then begin
      OldLines := SplitDiffLines(FDiff.Text1);
      NewLines := SplitDiffLines(FDiff.Text2);
      AllInfos := nil;
      AllCount := 0;
      CollectAllLines;

      // mark lines to show: changed lines plus context
      SetLength(ShowLine, AllCount);
      HasChanges := false;
      for i := 0 to AllCount-1 do
        ShowLine[i] := FContextLines < 0;
      for i := 0 to AllCount-1 do begin
        if AllInfos[i].Kind = dlkContext then continue;
        HasChanges := true;
        if FContextLines < 0 then continue;
        for j := Max(0, i-FContextLines) to Min(AllCount-1, i+FContextLines) do
          ShowLine[j] := true;
      end;

      if HasChanges or ShowWholeText then begin
        SetLength(FLineInfos, AllCount + AllCount div 2 + 1);
        Cnt := 0;
        i := 0;
        while i < AllCount do begin
          if not ShowLine[i] then begin
            inc(i);
            continue;
          end;
          HunkStart := i;
          HunkEnd := i;
          while (HunkEnd+1 < AllCount) and ShowLine[HunkEnd+1] do
            inc(HunkEnd);
          // hunk header, not needed for the whole text
          if not ShowWholeText then begin
            FLineInfos[Cnt].Kind := dlkHunk;
            FLineInfos[Cnt].OldLine := 0;
            FLineInfos[Cnt].NewLine := 0;
            inc(Cnt);
            sl.Add(HunkHeader(HunkStart, HunkEnd));
          end;
          // hunk lines
          for j := HunkStart to HunkEnd do begin
            FLineInfos[Cnt] := AllInfos[j];
            inc(Cnt);
            sl.Add(GetLineText(AllInfos[j]));
          end;
          i := HunkEnd+1;
        end;
        SetLength(FLineInfos, Cnt);
      end;
    end;

    FSynEdit.BeginUpdate(false);
    try
      FSynEdit.Lines.Assign(sl);
      FSynEdit.CaretXY := Point(1, 1);
      FSynEdit.TopLine := 1;
    finally
      FSynEdit.EndUpdate;
    end;
  finally
    sl.Free;
  end;
  FGutterPart.UpdateDigits;
  FSynEdit.Invalidate;
end;

end.

