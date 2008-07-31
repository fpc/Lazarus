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
    fOnSpecialLineColors : TSpecialLineColorsEvent;
    fOnSpecialLineMarkup : TSpecialLineMarkupEvent;
    bSpecialLine : Boolean;
  public
    constructor Create(ASynEdit : TCustomControl);

    Procedure PrepareMarkupForRow(aRow : Integer); override;
    Function GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor; override;
    Function GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer; override;

    property OnSpecialLineColors: TSpecialLineColorsEvent
      read fOnSpecialLineColors write fOnSpecialLineColors;
    property OnSpecialLineMarkup: TSpecialLineMarkupEvent
      read fOnSpecialLineMarkup write fOnSpecialLineMarkup;
  end;

implementation
uses SynEdit;


{ TSynEditMarkupBracket }

constructor TSynEditMarkupSpecialLine.Create(ASynEdit : TCustomControl);
begin
  inherited Create(ASynEdit);
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
end;

procedure TSynEditMarkupSpecialLine.PrepareMarkupForRow(aRow : Integer);
var
  colFg, colBg : TColor;
begin
  bSpecialLine := False;
  if Assigned(fOnSpecialLineMarkup) then
    fOnSpecialLineMarkup(SynEdit, aRow, bSpecialLine, MarkupInfo);
    
  if Assigned(fOnSpecialLineColors) then begin
    If bSpecialLine then begin
      colFg := MarkupInfo.Foreground;
      colBg := MarkupInfo.Background;
    end else begin
      colFg := clNone;
      colBg := clNone;
    end;
    fOnSpecialLineColors(SynEdit, aRow, bSpecialLine, colFg, colBg);
    MarkupInfo.Foreground := colFg;
    MarkupInfo.Background := colBg;
  end;
end;

function TSynEditMarkupSpecialLine.GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor;
begin
  Result := nil;
  if bSpecialLine then result := MarkupInfo;
end;

function TSynEditMarkupSpecialLine.GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer;
begin
  result := -1; // always valid for the whole line
end;

end.

