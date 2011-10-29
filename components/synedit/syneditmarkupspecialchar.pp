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
unit SynEditMarkupSpecialChar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, LCLProc,
  SynEditMarkup, SynEditTypes, SynEditMiscClasses;

type

  { TSynEditMarkupSpecialChar }

  TSynEditMarkupSpecialChar = class(TSynEditMarkup)
  private
    FVisibleSpecialChars: TSynVisibleSpecialChars;
    FHasMarkup: Boolean;
    FCurLine: String;
    FCurStart, FCurEnd: integer;
    procedure SetVisibleSpecialChars(AValue: TSynVisibleSpecialChars);
  protected
    procedure DoMarkupChanged(AMarkup: TSynSelectedColor); override;
    function IsSpecial(pos: Integer): Boolean; inline;
  public
    constructor Create(ASynEdit : TSynEditBase);
    destructor Destroy; override;

    Procedure PrepareMarkupForRow(aRow : Integer); override;
    Function GetMarkupAttributeAtRowCol(const aRow, aCol: Integer) : TSynSelectedColor; override;
    Function GetNextMarkupColAfterRowCol(const aRow, aCol: Integer) : Integer; override;

    property VisibleSpecialChars: TSynVisibleSpecialChars read FVisibleSpecialChars write SetVisibleSpecialChars;
  end;

implementation

{ TSynEditMarkupSpecialChar }

procedure TSynEditMarkupSpecialChar.SetVisibleSpecialChars(AValue: TSynVisibleSpecialChars);
begin
  if FVisibleSpecialChars = AValue then Exit;
  FVisibleSpecialChars := AValue;
  SynEdit.Invalidate;
end;

procedure TSynEditMarkupSpecialChar.DoMarkupChanged(AMarkup: TSynSelectedColor);
begin
  inherited DoMarkupChanged(AMarkup);
  FHasMarkup := AMarkup.IsEnabled;
  SynEdit.Invalidate;
end;

function TSynEditMarkupSpecialChar.IsSpecial(pos: Integer): Boolean;
begin
  if (pos < 1) or (pos > Length(FCurLine)) then exit(False);
  Result := ( (vscSpace in FVisibleSpecialChars) and (FCurLine[pos] in [' ']) ) or
            ( (FVisibleSpecialChars*[vscTabAtFirst, vscTabAtLast] <> []) and (FCurLine[pos] in [#9]) )
            ;
end;

constructor TSynEditMarkupSpecialChar.Create(ASynEdit : TSynEditBase);
begin
  inherited Create(ASynEdit);
  MarkupInfo.Clear;
  FHasMarkup := False;
end;

destructor TSynEditMarkupSpecialChar.Destroy;
begin
  inherited Destroy;
end;

procedure TSynEditMarkupSpecialChar.PrepareMarkupForRow(aRow : Integer);
begin
  FCurLine := '';
  FCurStart := -1;
  FCurEnd := -1;
  if (not FHasMarkup) or (FVisibleSpecialChars = []) then exit;
  FCurLine := Lines[aRow-1];
end;

function TSynEditMarkupSpecialChar.GetMarkupAttributeAtRowCol(const aRow, aCol: Integer) : TSynSelectedColor;
begin
  Result := nil;
  if (FCurLine='') or (not (FHasMarkup and (FVisibleSpecialChars <> []))) then exit;

  if (aCol >= FCurStart) and (aCol < FCurEnd) then begin
    Result := MarkupInfo;
    Result.StartX := FCurStart;
    Result.EndX := FCurEnd - 1;
  end;
end;

function TSynEditMarkupSpecialChar.GetNextMarkupColAfterRowCol(const aRow, aCol: Integer) : Integer;
var
  s: Boolean;
  i, LogCol: Integer;
begin
  Result := -1;
  if (FCurLine='') or (not (FHasMarkup and (FVisibleSpecialChars <> []))) then exit;

  if aCol < FCurStart then exit(FCurStart);
  if aCol < FCurEnd then exit(FCurEnd);

  LogCol := PhysicalToLogicalPos(Point(aCol, aRow)).x;
  if LogCol > Length(FCurLine) then exit;
  if (LogCol = Length(FCurLine)) then begin
    if IsSpecial(LogCol) then
      Result := LogicalToPhysicalPos(Point(Length(FCurLine)+1, aRow)).x;
      FCurEnd := Result;
    exit;
  end;

  // search next space-seq
  i := LogCol;
  s := IsSpecial(LogCol);
  if s then
    FCurStart := aCol
  else begin
    while (i <= Length(FCurLine)) and (not IsSpecial(i)) do inc(i);
    FCurStart := LogicalToPhysicalPos(Point(i, aRow)).x;
  end;

  while (i <= Length(FCurLine)) and (IsSpecial(i)) do inc(i);
  FCurEnd := LogicalToPhysicalPos(Point(i, aRow)).x;

  if aCol < FCurStart then exit(FCurStart);
  if aCol < FCurEnd then exit(FCurEnd);
end;

end.

