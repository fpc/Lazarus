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
    FOnSpecialLineColors: TSpecialLineColorsEvent;
    FOnSpecialLineMarkup: TSpecialLineMarkupEvent;
    FSpecialLine : Boolean;
  public
    constructor Create(ASynEdit: TCustomControl);

    procedure PrepareMarkupForRow(ARow: Integer); override;
    function GetMarkupAttributeAtRowCol(const ARow, ACol: Integer): TSynSelectedColor; override;
    function GetNextMarkupColAfterRowCol(const ARow, ACol: Integer): Integer; override;

    property OnSpecialLineColors: TSpecialLineColorsEvent
      read FOnSpecialLineColors write FOnSpecialLineColors;
    property OnSpecialLineMarkup: TSpecialLineMarkupEvent
      read FOnSpecialLineMarkup write FOnSpecialLineMarkup;
  end;

implementation

{ TSynEditMarkupBracket }

constructor TSynEditMarkupSpecialLine.Create(ASynEdit: TCustomControl);
begin
  inherited Create(ASynEdit);
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
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
end;

function TSynEditMarkupSpecialLine.GetMarkupAttributeAtRowCol(const ARow, ACol : Integer): TSynSelectedColor;
begin
  Result := nil;
  if FSpecialLine then
    Result := MarkupInfo;
end;

function TSynEditMarkupSpecialLine.GetNextMarkupColAfterRowCol(const ARow, ACol : Integer): Integer;
begin
  Result := -1; // always valid for the whole line
end;

end.

