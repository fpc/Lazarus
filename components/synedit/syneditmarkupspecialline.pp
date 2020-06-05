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
unit SynEditMarkupSpecialLine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditMarkup, SynEditMiscClasses, Controls, LCLProc;

type

  TSpecialLineMarkupEvent = procedure(Sender: TObject; Line: integer;
    var Special: boolean; Markup: TSynSelectedColor) of object;
  TSpecialLineColorsEvent = procedure(Sender: TObject; Line: integer;
    var Special: boolean; var FG, BG: TColor) of object;

  { TSynEditMarkupSpecialLine }

  TSynEditMarkupSpecialLine = class(TSynEditMarkup)
  private
    FMarkupLineHighlightInfo: TSynSelectedColor;
    FOnSpecialLineColors: TSpecialLineColorsEvent;
    FOnSpecialLineMarkup: TSpecialLineMarkupEvent;
    FSpecialLine : Boolean;
    FHighlightedLine: Integer;
  protected
    procedure DoMarkupLineHighlightInfoChange(Sender: TObject);
    function HasLineHighlight: Boolean;
    procedure DoCaretChanged(Sender: TObject); override;
    procedure DoTopLineChanged(OldTopLine : Integer); override;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); override;
    procedure DoTextChanged(StartLine, EndLine, ACountDiff: Integer); override;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    procedure DoEnabledChanged(Sender: TObject); override;
  public
    constructor Create(ASynEdit: TSynEditBase);
    destructor Destroy; override;

    procedure PrepareMarkupForRow(ARow: Integer); override;
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;
    function GetMarkupAttributeAtWrapEnd(const aRow: Integer;
      const aWrapCol: TLazSynDisplayTokenBound): TSynSelectedColor; override;
    function RealEnabled: Boolean; override;

    procedure InvalidateLineHighlight;

    property MarkupLineHighlightInfo: TSynSelectedColor read FMarkupLineHighlightInfo;

    property OnSpecialLineColors: TSpecialLineColorsEvent
      read FOnSpecialLineColors write FOnSpecialLineColors;
    property OnSpecialLineMarkup: TSpecialLineMarkupEvent
      read FOnSpecialLineMarkup write FOnSpecialLineMarkup;
  end;

implementation

{ TSynEditMarkupBracket }

procedure TSynEditMarkupSpecialLine.DoMarkupLineHighlightInfoChange(
  Sender: TObject);
begin
  if FHighlightedLine > 0 then
    InvalidateSynLines(FHighlightedLine, FHighlightedLine)
  else
    InvalidateLineHighlight;
end;

function TSynEditMarkupSpecialLine.HasLineHighlight: Boolean;
begin
  Result :=
    (FMarkupLineHighlightInfo.Background <> clNone) or
    (FMarkupLineHighlightInfo.Foreground <> clNone);
end;

procedure TSynEditMarkupSpecialLine.DoCaretChanged(Sender: TObject);
begin
  InvalidateLineHighlight;
end;

procedure TSynEditMarkupSpecialLine.DoTopLineChanged(OldTopLine: Integer);
begin
  InvalidateLineHighlight;
end;

procedure TSynEditMarkupSpecialLine.DoLinesInWindoChanged(OldLinesInWindow: Integer);
begin
  InvalidateLineHighlight;
end;

procedure TSynEditMarkupSpecialLine.DoTextChanged(StartLine, EndLine,
  ACountDiff: Integer);
begin
  InvalidateLineHighlight;
end;

procedure TSynEditMarkupSpecialLine.DoMarkupChanged(AMarkup: TSynSelectedColor);
begin
  InvalidateLineHighlight;
end;

procedure TSynEditMarkupSpecialLine.DoEnabledChanged(Sender: TObject);
begin
  InvalidateLineHighlight;
end;

function TSynEditMarkupSpecialLine.RealEnabled: Boolean;
begin
  Result := Enabled and (not IsTempDisabled) and
    (Assigned(FOnSpecialLineMarkup) or Assigned(FOnSpecialLineColors) or
     HasLineHighlight
    );
end;

constructor TSynEditMarkupSpecialLine.Create(ASynEdit: TSynEditBase);
begin
  inherited Create(ASynEdit);

  FHighlightedLine := -1;
  FMarkupLineHighlightInfo := TSynSelectedColor.Create;
  FMarkupLineHighlightInfo.Background := clNone;
  FMarkupLineHighlightInfo.Foreground := clNone;
  FMarkupLineHighlightInfo.OnChange := @DoMarkupLineHighlightInfoChange;

  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
end;

destructor TSynEditMarkupSpecialLine.Destroy;
begin
  FMarkupLineHighlightInfo.Free;
  inherited Destroy;
end;

procedure TSynEditMarkupSpecialLine.PrepareMarkupForRow(ARow: Integer);
var
  colFg, colBg: TColor;
begin
  FSpecialLine := False;
  if Assigned(FOnSpecialLineMarkup) then
    FOnSpecialLineMarkup(SynEdit, ARow, FSpecialLine, MarkupInfo);
    
  if Assigned(FOnSpecialLineColors) then
  begin
    if FSpecialLine then
    begin
      colFg := MarkupInfo.Foreground;
      colBg := MarkupInfo.Background;
    end else
    begin
      colFg := clNone;
      colBg := clNone;
    end;
    FOnSpecialLineColors(SynEdit, ARow, FSpecialLine, colFg, colBg);
    MarkupInfo.Foreground := colFg;
    MarkupInfo.Background := colBg;
  end;

  // if line is not special then check whether it is a current line
  // if it is so then use own bg,fg colors to setup highlight
  if not FSpecialLine then
  begin
    if (FHighlightedLine = ARow) and HasLineHighlight then
    begin
      FSpecialLine := True;
      MarkupInfo.Assign(FMarkupLineHighlightInfo);
    end;
  end;
end;

function TSynEditMarkupSpecialLine.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
begin
  Result := nil;
  MarkupInfo.SetFrameBoundsPhys(1, MaxInt);
  if FSpecialLine then
    Result := MarkupInfo;
end;

procedure TSynEditMarkupSpecialLine.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
  ANextLog: Integer);
begin
  ANextLog := -1;
  ANextPhys := -1; // always valid for the whole line
end;

function TSynEditMarkupSpecialLine.GetMarkupAttributeAtWrapEnd(
  const aRow: Integer; const aWrapCol: TLazSynDisplayTokenBound
  ): TSynSelectedColor;
begin
  Result := nil;
  MarkupInfo.SetFrameBoundsPhys(1, MaxInt);
  if FSpecialLine then
    Result := MarkupInfo;
end;

procedure TSynEditMarkupSpecialLine.InvalidateLineHighlight;
var
  NewLine: Integer;
begin
  if (not HasLineHighlight) or (Caret = nil) then
    Exit;

  NewLine := Caret.LinePos;

  // invalidate old line highlighting, if changed
  if (FHighlightedLine > 0) and (NewLine <> FHighlightedLine) then
    InvalidateSynLines(FHighlightedLine, FHighlightedLine);

  // invalidate new line highlighting, if changed
  if (NewLine > 0) and (NewLine <> FHighlightedLine) then
    InvalidateSynLines(NewLine, NewLine);

  FHighlightedLine := NewLine;
end;

end.

