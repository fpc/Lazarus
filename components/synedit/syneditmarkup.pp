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
unit SynEditMarkup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditTextBase, SynEditPointClasses,
  SynEditMiscClasses, SynEditMiscProcs, Controls, SynEditHighlighter, LCLProc;

type

  TInvalidateLines = procedure(FirstLine, LastLine: integer) of Object;

type
  TSynEditMarkupClass = class of TSynEditMarkup;

  { TSynEditMarkup }

  TSynEditMarkup = class(TObject)
  private
    FMarkupInfo : TSynSelectedColor;
    FLines : TSynEditStrings;
    FCaret : TSynEditCaret;
    FTopLine, FLinesInWindow : Integer;
    FSynEdit : TSynEditBase;
    FInvalidateLinesMethod : TInvalidateLines;
    FEnabled: Boolean;
    FTempEnable: Integer;

    function GetBGColor : TColor;
    function GetEnabled: Boolean;
    function GetFGColor : TColor;
    function GetFrameColor: TColor;
    function GetFrameStyle: TSynLineStyle;
    function GetStyle : TFontStyles;
    procedure SetBGColor(const AValue : TColor);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetFGColor(const AValue : TColor);
    procedure SetFrameColor(const AValue : TColor);
    procedure SetStyle(const AValue : TFontStyles);

  protected
    FPaintLock: Integer;
    procedure MarkupChanged(AMarkup: TObject);

    procedure SetInvalidateLinesMethod(const AValue : TInvalidateLines); virtual;
    procedure SetLines(const AValue : TSynEditStrings); virtual;
    procedure SetTopLine(const AValue : Integer); virtual;
    procedure SetLinesInWindow(const AValue : Integer); virtual;
    procedure SetCaret(const AValue : TSynEditCaret); virtual;

    procedure DoEnabledChanged(Sender: TObject); virtual;
    procedure DoCaretChanged(Sender: TObject); virtual;
    procedure DoTopLineChanged(OldTopLine : Integer); virtual;
    procedure DoLinesInWindoChanged(OldLinesInWindow : Integer); virtual;
    procedure DoTextChanged(StartLine, EndLine : Integer); virtual;
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); virtual;

    procedure InvalidateSynLines(FirstLine, LastLine: integer); // Call Synedt to invalidate lines
    function ScreenRowToRow(aRow : Integer) : Integer;
    function RowToScreenRow(aRow : Integer) : Integer;
    function LogicalToPhysicalPos(const p: TPoint): TPoint;
    function PhysicalToLogicalPos(const p: TPoint): TPoint;
    function Highlighter: TSynCustomHighlighter;
    function OwnedByMgr: Boolean; virtual; // overwrite, do prevent destruction by mgr

    property SynEdit : TSynEditBase read fSynEdit;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;
    Procedure PrepareMarkupForRow(aRow : Integer); virtual;
    Procedure FinishMarkupForRow(aRow : Integer); virtual;
    Procedure EndMarkup; virtual;
    Function GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor; virtual; abstract;
    Function GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer; virtual; abstract;
    procedure MergeMarkupAttributeAtRowCol(const aRow, aCol, AEndCol : Integer; AMarkup: TSynSelectedColor); virtual;

    // Notifications about Changes to the text
    Procedure TextChanged(aFirstCodeLine, aLastCodeLine: Integer); virtual;
    Procedure TempDisable;
    Procedure TempEnable;
    procedure IncPaintLock; virtual;
    procedure DecPaintLock; virtual;

    property MarkupInfo : TSynSelectedColor read fMarkupInfo;
    property FGColor : TColor read GetFGColor;
    property BGColor : TColor read GetBGColor;
    property FrameColor: TColor read GetFrameColor;
    property FrameStyle: TSynLineStyle read GetFrameStyle;
    property Style : TFontStyles read GetStyle;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property Lines : TSynEditStrings read fLines write SetLines;
    property Caret : TSynEditCaret read fCaret write SetCaret;
    property TopLine : Integer read fTopLine write SetTopLine;
    property LinesInWindow : Integer read fLinesInWindow write SetLinesInWindow;
    property InvalidateLinesMethod : TInvalidateLines write SetInvalidateLinesMethod;
  end;

  { TSynEditMarkupManager }

  TSynEditMarkupManager = class(TSynEditMarkup) { TODO: Forward onchange calls to all others }
  private
    fMarkUpList : TList;
    function GetMarkup(Index: integer): TSynEditMarkup;
    function GetMarkupByClass(Index: TSynEditMarkupClass): TSynEditMarkup;

  protected
    procedure SetInvalidateLinesMethod(const AValue : TInvalidateLines); override;
    procedure SetLines(const AValue : TSynEditStrings); override;
    procedure SetTopLine(const AValue : Integer); override;
    procedure SetLinesInWindow(const AValue : Integer); override;
    procedure SetCaret(const AValue : TSynEditCaret); override;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;
    procedure IncPaintLock; override;
    procedure DecPaintLock; override;

    Procedure AddMarkUp(aMarkUp : TSynEditMarkup; AsFirst: Boolean = False);
    Procedure RemoveMarkUp(aMarkUp : TSynEditMarkup);
    function Count: Integer;
    property Markup[Index: integer]: TSynEditMarkup
      read GetMarkup;
    property MarkupByClass[Index: TSynEditMarkupClass]: TSynEditMarkup
      read GetMarkupByClass;

    Procedure PrepareMarkupForRow(aRow : Integer); override;
    Procedure FinishMarkupForRow(aRow : Integer); override;
    Procedure EndMarkup; override;
    Function  GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor; override;
    Function  GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer; override;
    procedure MergeMarkupAttributeAtRowCol(const aRow, aCol, AEndCol : Integer; AMarkup: TSynSelectedColor); override;

    // Notifications about Changes to the text
    Procedure TextChanged(aFirstCodeLine, aLastCodeLine: Integer); override;
  end;


  
implementation
uses SynEdit;

{ TSynEditMarkup }

function TSynEditMarkup.GetBGColor : TColor;
begin
  result := fMarkupInfo.Background;
end;

function TSynEditMarkup.GetEnabled: Boolean;
begin
  Result := FEnabled and (FTempEnable = 0);
end;

function TSynEditMarkup.GetFGColor : TColor;
begin
  result := fMarkupInfo.Foreground;
end;

function TSynEditMarkup.GetFrameColor: TColor;
begin
  Result := fMarkupInfo.FrameColor;
end;

function TSynEditMarkup.GetFrameStyle: TSynLineStyle;
begin
  Result := FMarkupInfo.FrameStyle;
end;

function TSynEditMarkup.GetStyle : TFontStyles;
begin
  Result := fMarkupInfo.Style;
end;

procedure TSynEditMarkup.SetBGColor(const AValue : TColor);
begin
  if fMarkupInfo.Background = AValue then exit;
  fMarkupInfo.Background := AValue;
end;

procedure TSynEditMarkup.SetEnabled(const AValue: Boolean);
begin
  if AValue = FEnabled then exit;
  FEnabled := AValue;
  DoEnabledChanged(self);
end;

procedure TSynEditMarkup.SetFGColor(const AValue : TColor);
begin
  if fMarkupInfo.Foreground = AValue then exit;
  fMarkupInfo.Foreground := AValue;
end;

procedure TSynEditMarkup.SetFrameColor(const AValue: TColor);
begin
  if fMarkupInfo.FrameColor = AValue then exit;
  fMarkupInfo.FrameColor := AValue;
end;

procedure TSynEditMarkup.SetStyle(const AValue : TFontStyles);
begin
  if fMarkupInfo.Style = AValue then exit;
  fMarkupInfo.Style := AValue;
end;

procedure TSynEditMarkup.MarkupChanged(AMarkup : TObject);
begin
  DoMarkupChanged(AMarkup as TSynSelectedColor);
end;

procedure TSynEditMarkup.SetLines(const AValue : TSynEditStrings);
begin
  if fLines = AValue then exit;
  fLines := AValue;
end;

procedure TSynEditMarkup.SetInvalidateLinesMethod(const AValue : TInvalidateLines);
begin
  if fInvalidateLinesMethod = AValue then exit;
  fInvalidateLinesMethod := AValue;
end;

procedure TSynEditMarkup.SetCaret(const AValue : TSynEditCaret);
var
  r: Boolean;
begin
  // inly register caret change callback, if handler is overriden
  r := TMethod(@Self.DoCaretChanged).Code <> Pointer(@TSynEditMarkup.DoCaretChanged);
  if r and (FCaret <> nil) then
    FCaret.RemoveChangeHandler(@DoCaretChanged);
  FCaret := AValue;
  if r and (FCaret <> nil) then
    FCaret.AddChangeHandler(@DoCaretChanged);
end;

procedure TSynEditMarkup.DoEnabledChanged(Sender: TObject);
begin
end;

procedure TSynEditMarkup.SetTopLine(const AValue : Integer);
var
  OldValue : Integer;
begin
  if fTopLine = AValue then exit;
  OldValue :=fTopLine;
  fTopLine := AValue;
  DoTopLineChanged(OldValue);
end;

procedure TSynEditMarkup.SetLinesInWindow(const AValue : Integer);
var
  OldValue : Integer;
begin
  if FLinesInWindow = AValue then exit;
  OldValue :=FLinesInWindow;
  FLinesInWindow := AValue;
  DoLinesInWindoChanged(OldValue);
end;

procedure TSynEditMarkup.DoCaretChanged(Sender: TObject);
begin
end;

procedure TSynEditMarkup.DoTopLineChanged(OldTopLine : Integer);
begin
end;

procedure TSynEditMarkup.DoLinesInWindoChanged(OldLinesInWindow : Integer);
begin
end;

procedure TSynEditMarkup.DoTextChanged(StartLine, EndLine: Integer);
begin
end;

procedure TSynEditMarkup.DoMarkupChanged(AMarkup : TSynSelectedColor);
begin
end;

procedure TSynEditMarkup.InvalidateSynLines(FirstLine, LastLine : integer);
begin
  if assigned(fInvalidateLinesMethod)
  then fInvalidateLinesMethod(FirstLine, LastLine);
end;

function TSynEditMarkup.ScreenRowToRow(aRow : Integer) : Integer;
begin
  Result := TSynEdit(SynEdit).ScreenRowToRow(aRow);
end;

function TSynEditMarkup.RowToScreenRow(aRow : Integer) : Integer;
begin
  Result := TSynEdit(SynEdit).RowToScreenRow(aRow);
end;

function TSynEditMarkup.LogicalToPhysicalPos(const p : TPoint) : TPoint;
begin
  Result := FLines.LogicalToPhysicalPos(p);
end;

function TSynEditMarkup.PhysicalToLogicalPos(const p: TPoint): TPoint;
begin
  Result := FLines.PhysicalToLogicalPos(p);
end;

function TSynEditMarkup.Highlighter : TSynCustomHighlighter;
begin
  Result := TSynEdit(SynEdit).Highlighter;
end;

function TSynEditMarkup.OwnedByMgr: Boolean;
begin
  Result := True;
end;

constructor TSynEditMarkup.Create(ASynEdit : TSynEditBase);
begin
  inherited Create();
  fSynEdit := ASynEdit;
  FEnabled := true;
  FTempEnable := 0;
  fMarkupInfo := TSynSelectedColor.Create;
  fMarkupInfo.OnChange := @MarkupChanged;
end;

destructor TSynEditMarkup.Destroy;
begin
  // unregister caret handler
  Caret := nil;
  FreeAndNil(fMarkupInfo);
  inherited Destroy;
end;

procedure TSynEditMarkup.FinishMarkupForRow(aRow : Integer);
begin
end;

procedure TSynEditMarkup.EndMarkup;
begin
end;

procedure TSynEditMarkup.MergeMarkupAttributeAtRowCol(const aRow, aCol, AEndCol: Integer;
  AMarkup: TSynSelectedColor);
var
  c: TSynSelectedColor;
begin
  c := GetMarkupAttributeAtRowCol(aRow, aCol);
  if assigned(c) then
    AMarkup.Merge(c, aCol, AEndCol);
end;

procedure TSynEditMarkup.TextChanged(aFirstCodeLine, aLastCodeLine: Integer);
begin
  DoTextChanged(aFirstCodeLine, aLastCodeLine);
end;

procedure TSynEditMarkup.TempDisable;
begin
  inc(FTempEnable);
end;

procedure TSynEditMarkup.TempEnable;
begin
  if FTempEnable > 0 then
    dec(FTempEnable);
end;

procedure TSynEditMarkup.IncPaintLock;
begin
  inc(FPaintLock);
end;

procedure TSynEditMarkup.DecPaintLock;
begin
  dec(FPaintLock);
end;

procedure TSynEditMarkup.PrepareMarkupForRow(aRow : Integer);
begin
end;

{ TSynEditMarkupManager }

constructor TSynEditMarkupManager.Create(ASynEdit : TSynEditBase);
begin
  inherited Create(ASynEdit);
  fMarkUpList := TList.Create;
end;

destructor TSynEditMarkupManager.Destroy;
var
  i : integer;
begin
  for i := 0 to fMarkUpList.Count-1 do
    if TSynEditMarkup(fMarkUpList[i]).OwnedByMgr then
      TSynEditMarkup(fMarkUpList[i]).destroy;
  FreeAndNil(fMarkUpList);
  inherited Destroy;
end;

procedure TSynEditMarkupManager.IncPaintLock;
var
  i: Integer;
begin
  inherited IncPaintLock;
  for i := 0 to fMarkUpList.Count-1 do
    TSynEditMarkup(fMarkUpList[i]).IncPaintLock;
end;

procedure TSynEditMarkupManager.DecPaintLock;
var
  i: Integer;
begin
  inherited DecPaintLock;
  for i := 0 to fMarkUpList.Count-1 do
    TSynEditMarkup(fMarkUpList[i]).DecPaintLock;
end;

procedure TSynEditMarkupManager.AddMarkUp(aMarkUp : TSynEditMarkup; AsFirst: Boolean = False);
begin
  if AsFirst then
    fMarkUpList.Insert(0, aMarkUp)
  else
    fMarkUpList.Add(aMarkUp);
  aMarkUp.Lines := Lines;
  aMarkUp.Caret := Caret;
  aMarkUp.TopLine := TopLine;
  aMarkUp.LinesInWindow := LinesInWindow;
  aMarkUp.InvalidateLinesMethod := FInvalidateLinesMethod;
end;

procedure TSynEditMarkupManager.RemoveMarkUp(aMarkUp: TSynEditMarkup);
var
  i: LongInt;
begin
  i := fMarkUpList.IndexOf(aMarkUp);
  if i >= 0 then begin
    // unregister the caret handler, no longer controled by this synedit
    TSynEditMarkup(fMarkUpList[i]).Caret := nil;
    fMarkUpList.Delete(i);
  end;
end;

function TSynEditMarkupManager.Count: Integer;
begin
  Result := fMarkUpList.Count;
end;

procedure TSynEditMarkupManager.FinishMarkupForRow(aRow : Integer);
var
  i : integer;
begin
  for i := 0 to fMarkUpList.Count-1 do
    if TSynEditMarkup(fMarkUpList[i]).Enabled then
      TSynEditMarkup(fMarkUpList[i]).FinishMarkupForRow(aRow);
end;

procedure TSynEditMarkupManager.EndMarkup;
var
  i : integer;
begin
  for i := 0 to fMarkUpList.Count-1 do
    if TSynEditMarkup(fMarkUpList[i]).Enabled then
      TSynEditMarkup(fMarkUpList[i]).EndMarkup;
end;

procedure TSynEditMarkupManager.PrepareMarkupForRow(aRow : Integer);
var
  i : integer;
begin
  for i := 0 to fMarkUpList.Count-1 do
    if TSynEditMarkup(fMarkUpList[i]).Enabled then
      TSynEditMarkup(fMarkUpList[i]).PrepareMarkupForRow(aRow);
end;

procedure TSynEditMarkupManager.MergeMarkupAttributeAtRowCol
  (const aRow, aCol, AEndCol: Integer; AMarkup: TSynSelectedColor);
var
  i : integer;
begin
  for i := 0 to fMarkUpList.Count-1 do begin
    if TSynEditMarkup(fMarkUpList[i]).Enabled then
      TSynEditMarkup(fMarkUpList[i]).MergeMarkupAttributeAtRowCol
        (aRow, aCol, AEndCol, AMarkup);
  end;
end;

function TSynEditMarkupManager.GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor;
begin
  Result := MarkupInfo;
  Result.Clear;
  MergeMarkupAttributeAtRowCol(aRow, aCol, GetNextMarkupColAfterRowCol(aRow, aCol) - 1, Result);
end;

function TSynEditMarkupManager.GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer;
var
  i, j : integer;
begin
  if fMarkUpList.Count = 0
  then exit(-1);
  Result := TSynEditMarkup(fMarkUpList[0]).GetNextMarkupColAfterRowCol(aRow, aCol);
  for i := 1 to fMarkUpList.Count-1 do begin
    if not TSynEditMarkup(fMarkUpList[i]).Enabled then continue;
    j := TSynEditMarkup(fMarkUpList[i]).GetNextMarkupColAfterRowCol(aRow, aCol);
    if ((j>0) and (j < Result)) or (Result<0) then Result := j;
  end;
end;

procedure TSynEditMarkupManager.TextChanged(aFirstCodeLine, aLastCodeLine: Integer);
var
  i : integer;
begin
  for i := 0 to fMarkUpList.Count-1 do
    TSynEditMarkup(fMarkUpList[i]).TextChanged(aFirstCodeLine, aLastCodeLine);
end;

function TSynEditMarkupManager.GetMarkup(Index: integer): TSynEditMarkup;
begin
  Result := TSynEditMarkup(fMarkUpList[Index]);
end;

function TSynEditMarkupManager.GetMarkupByClass(Index: TSynEditMarkupClass): TSynEditMarkup;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to fMarkUpList.Count-1 do
    if TSynEditMarkup(fMarkUpList[i]).ClassType = Index then
      exit(TSynEditMarkup(fMarkUpList[i]));
end;

procedure TSynEditMarkupManager.SetInvalidateLinesMethod(const AValue : TInvalidateLines);
var
  i : integer;
begin
  inherited SetInvalidateLinesMethod(AValue);
  for i := 0 to fMarkUpList.Count-1 do
    TSynEditMarkup(fMarkUpList[i]).SetInvalidateLinesMethod(AValue);
end;

procedure TSynEditMarkupManager.SetLines(const AValue : TSynEditStrings);
var
  i : integer;
begin
  inherited SetLines(AValue);
  for i := 0 to fMarkUpList.Count-1 do
    TSynEditMarkup(fMarkUpList[i]).SetLines(AValue) ;
end;

procedure TSynEditMarkupManager.SetTopLine(const AValue : Integer);
var
  i : integer;
begin
  inherited SetTopLine(AValue);
  for i := 0 to fMarkUpList.Count-1 do
    TSynEditMarkup(fMarkUpList[i]).SetTopLine(AValue);
end;

procedure TSynEditMarkupManager.SetLinesInWindow(const AValue : Integer);
var
  i : integer;
begin
  inherited SetLinesInWindow(AValue);
  for i := 0 to fMarkUpList.Count-1 do
    TSynEditMarkup(fMarkUpList[i]).SetLinesInWindow(AValue);
end;

procedure TSynEditMarkupManager.SetCaret(const AValue : TSynEditCaret);
var
  i : integer;
begin
  inherited SetCaret(AValue);
  if fMarkUpList = nil then exit;
  for i := 0 to fMarkUpList.Count-1 do
    TSynEditMarkup(fMarkUpList[i]).SetCaret(AValue);
end;

end.


(*
  procedure CalculateNextChangePos(FromPos: Integer);
    begin
      ChangePos:= nc2+1; // Draw the Rest
      ChangeTyp := [];

      hsCol1   := fMarkupHighAll.GetNextMarkupColAfterRowCol(CurLine, FromPos-1);

      if (nC1Sel >= FromPos) then begin
        ChangePos := nC1Sel;
        ChangeTyp := [cSelOn];
      end;
      if ((nC2Sel >= FromPos) and (nC2Sel <= ChangePos)) then begin
        if (nC2Sel < ChangePos)
        then ChangeTyp := [cSelOff]
        else include(ChangeTyp, cSelOff);
        ChangePos := nC2Sel;
      end;

      if ((hsCol1 >= FromPos) and (hsCol1 <= ChangePos)) then begin
        hsMarkup := fMarkupHighAll.GetMarkupAttributeAtRowCol(CurLine, hsCol1);
if hsMarkup<>nil then begin;
        if (hsCol1 < ChangePos)
        then ChangeTyp := [CHLightOn]
        else include(ChangeTyp, CHLightOn);
end else begin
        if (hsCol1 < ChangePos)
        then ChangeTyp := [CHLightOff]
        else include(ChangeTyp, CHLightOff);
end;
        ChangePos := hsCol1;

        // only because this is last
      end;

    end;
  var
    CurrentColor : TDrawingColors;
    WantedColor : Set of TDrawingColors;
  procedure ExecuteChangePos;
    var
      NewColor : TDrawingColors;
    begin
      if (cSelOn in ChangeTyp)     then include(WantedColor, dcSelected);
      if (cSelOff in ChangeTyp)    then exclude(WantedColor, dcSelected);
      if (CHLightOn in ChangeTyp)  then include(WantedColor, dcHighlighted);
      if (CHLightOff in ChangeTyp) then exclude(WantedColor, dcHighlighted);

      if dcSelected in WantedColor
      then NewColor:= dcSelected
      else if dcHighlighted in WantedColor
      then NewColor:= dcHighlighted
      else NewColor:= dcNormal;

      if NewColor <> CurrentColor then SetDrawingColors(NewColor);
      CurrentColor:=NewColor;
    end;


*)
