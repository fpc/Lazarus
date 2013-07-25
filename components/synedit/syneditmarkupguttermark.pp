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
unit SynEditMarkupGutterMark;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LCLProc,
  SynEditMarkup, SynEditMiscClasses, SynEditMarks;

type

  TMarkSection = record
    StartX, EndX: Integer;  // Physical
    Priority: Integer;
    Markup: TSynSelectedColor;
  end;
  PMarkSection = ^TMarkSection;

  { TSynEditMarkupMark }

  TSynEditMarkupMark = class(TSynEditMark)
  private
    FSourceMarkup: TSynSelectedColor;
  public
    property SourceMarkup: TSynSelectedColor read FSourceMarkup write FSourceMarkup;
  end;


  { TSynEditMarkupGutterMark }

  TSynEditMarkupGutterMark = class(TSynEditMarkup)
  // TODO: subscribe to mark changes for line invalidation => currently done by synedit itself
  private
    FRowData: Array of TMarkSection;
    FWordBreaker: TSynWordBreaker;
  protected
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
  public
    constructor Create(ASynEdit: TSynEditBase; AWordBreaker: TSynWordBreaker);

    procedure PrepareMarkupForRow(ARow: Integer); override;
    function GetMarkupAttributeAtRowCol(const aRow: Integer;
                                        const aStartCol: TLazSynDisplayTokenBound;
                                        const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor; override;
    procedure GetNextMarkupColAfterRowCol(const aRow: Integer;
                                         const aStartCol: TLazSynDisplayTokenBound;
                                         const AnRtlInfo: TLazSynDisplayRtlInfo;
                                         out   ANextPhys, ANextLog: Integer); override;
  end;

implementation

uses
  SynEdit;


{ TSynEditMarkupGutterMark }

procedure TSynEditMarkupGutterMark.DoMarkupChanged(AMarkup: TSynSelectedColor);
begin
  inherited DoMarkupChanged(AMarkup);
  SynEdit.Invalidate;
end;

constructor TSynEditMarkupGutterMark.Create(ASynEdit: TSynEditBase;
  AWordBreaker: TSynWordBreaker);
begin
  FWordBreaker := AWordBreaker;
  inherited Create(ASynEdit);
end;

procedure TSynEditMarkupGutterMark.PrepareMarkupForRow(ARow: Integer);
var
  MLine: TSynEditMarkLine;
  i, j: Integer;
  s: string;
  Markup: TSynEditMarkupMark;
  Section: PMarkSection;
  x: Integer;
begin
  MLine := TCustomSynEdit(SynEdit).Marks.Line[ARow];
  if MLine = nil then begin
    SetLength(FRowData, 0);
    exit;
  end;
  SetLength(FRowData, MLine.Count);

  j := 0;
  s := '';
  for i := 0 to MLine.Count - 1 do begin
    if not (MLine[i] is TSynEditMarkupMark) then
      continue;
    Markup:=TSynEditMarkupMark(MLine[i]);
    if Markup.SourceMarkup = nil then
      continue;

    Section := @FRowData[j];
    Section^.Markup := Markup.SourceMarkup;
    Section^.Priority := Markup.Priority;

    if s='' then
      s := Lines[ARow];
    if s='' then break;

    x := FWordBreaker.PrevBoundary(s, Markup.Column, True);
    if x < 1 then
      x := 1;
    Section^.StartX := LogicalToPhysicalPos(Point(x, ARow)).x;

    x := FWordBreaker.NextBoundary(s, Markup.Column);
    if x < 1 then
      x := length(s) + 1;
    Section^.EndX   := LogicalToPhysicalPos(Point(x, ARow)).x;

    if (Section^.StartX > 0) and (Section^.EndX > 0) and (Section^.StartX<Section^.EndX)
    then
      inc(j);
  end;

  SetLength(FRowData, j);
end;

function TSynEditMarkupGutterMark.GetMarkupAttributeAtRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo): TSynSelectedColor;
var
  i, FoundPri: Integer;
  Section: PMarkSection;
begin
  FoundPri := 0;
  Result := nil;
  for i := 0 to length(FRowData) - 1 do begin
    Section := @FRowData[i];
    if (Section^.StartX <= aStartCol.Physical) and (Section^.EndX > aStartCol.Physical) and
       ( (Section^.Priority < FoundPri) or (Result=nil) )
    then begin
      Result := Section^.Markup;
      MarkupInfo.SetFrameBoundsPhys(Section^.StartX, Section^.EndX);
      FoundPri := Section^.Priority;
    end;
  end;
end;

procedure TSynEditMarkupGutterMark.GetNextMarkupColAfterRowCol(const aRow: Integer;
  const aStartCol: TLazSynDisplayTokenBound; const AnRtlInfo: TLazSynDisplayRtlInfo; out ANextPhys,
  ANextLog: Integer);
// return the next StartX or EndX after aStartCol

  procedure Improve(Col: integer); inline;
  begin
    if Col <= aStartCol.Physical then exit;
    if Col > ANextPhys then exit;
    ANextPhys:=Col;
  end;

var
  i: Integer;
  Section: PMarkSection;
begin
  ANextLog := -1;
  if length(FRowData) = 0 then
  begin
    ANextPhys := -1;
    exit;
  end;
  ANextPhys := High(ANextPhys);
  for i := 0 to length(FRowData) - 1 do begin
    Section := @FRowData[i];
    Improve(Section^.StartX);
    Improve(Section^.EndX);
  end;
  if ANextPhys = High(ANextPhys) then
    ANextPhys := -1;
end;

end.

