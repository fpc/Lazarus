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
    function GetMarkupAttributeAtRowCol(const ARow, ACol: Integer): TSynSelectedColor; override;
    function GetNextMarkupColAfterRowCol(const ARow, ACol: Integer): Integer; override;
    //procedure MergeMarkupAttributeAtRowCol(const aRow, aCol, AEndCol : Integer; AMarkup: TSynSelectedColor); override;
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
begin
  MLine := TSynEdit(SynEdit).Marks.Line[ARow];
  if MLine = nil then begin
    SetLength(FRowData, 0);
    exit;
  end;
  SetLength(FRowData, MLine.Count);

  j := 0;
  for i := 0 to MLine.Count - 1 do begin
    if not ( (MLine[i] is TSynEditMarkupMark) and
             (TSynEditMarkupMark(MLine[i]).SourceMarkup <> nil) )
    then
      continue;

    FRowData[i].Markup := TSynEditMarkupMark(MLine[i]).SourceMarkup;
    FRowData[i].Priority := MLine[i].Priority;

    s := Lines[MLine[i].Line - 1];
    FRowData[i].StartX := LogicalToPhysicalPos
      (Point(FWordBreaker.PrevBoundary(s, MLine[i].Column, True), MLine[i].Line)).x;
    FRowData[i].EndX   := LogicalToPhysicalPos
      (Point(FWordBreaker.NextBoundary(s, MLine[i].Column), MLine[i].Line)).x;

    if (FRowData[i].StartX > 0) and (FRowData[i].EndX > 0) then
      inc(j);
  end;

  SetLength(FRowData, j);
end;

function TSynEditMarkupGutterMark.GetMarkupAttributeAtRowCol(const ARow,
  ACol: Integer): TSynSelectedColor;
var
  i, FoundPri: Integer;
begin
  FoundPri := 0;
  Result := nil;
  for i := 0 to length(FRowData) - 1 do begin
    if (FRowData[i].StartX <= ACol) and (FRowData[i].EndX > ACol) and
       ( (FRowData[i].Priority < FoundPri) or (i = 0) )
    then begin
      Result := FRowData[i].Markup;
      Result.StartX := FRowData[i].StartX;
      Result.EndX := FRowData[i].EndX-1;
      FoundPri := FRowData[i].Priority;
    end;
  end;
end;

function TSynEditMarkupGutterMark.GetNextMarkupColAfterRowCol(const ARow,
  ACol: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  if length(FRowData) = 0 then
    exit;
  for i := 0 to length(FRowData) - 1 do begin
    if FRowData[i].StartX < Result then
      Result := FRowData[0].StartX;;
    if FRowData[i].EndX < Result then
      Result := FRowData[0].EndX;;
  end;
end;

end.

