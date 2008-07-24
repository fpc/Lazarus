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
      read fOnSpecialLineColors write fOnSpecialLineColors; deprecated; // use SpecialMarkup instead
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

