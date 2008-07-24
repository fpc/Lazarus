unit SynEditMarkupCtrlMouseLink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditMarkup, SynEditMiscClasses, Controls, LCLProc;

type

  { TSynEditMarkupCtrlMouseLink }

  TSynEditMarkupCtrlMouseLink = class(TSynEditMarkup)
  private
    FCtrlMouseLine : Integer;
    FCtrlMouseX1 : Integer;
    FCtrlMouseX2 : Integer;
  public
    constructor Create(ASynEdit : TCustomControl);

    Procedure EndMarkup; override;
    Function GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor; override;
    Function GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer; override;

    property CtrlMouseLine : Integer read FCtrlMouseLine write FCtrlMouseLine;
    property CtrlMouseX1 : Integer read FCtrlMouseX1 write FCtrlMouseX1;
    property CtrlMouseX2 : Integer read FCtrlMouseX2 write FCtrlMouseX2;
  end;

implementation
uses SynEdit; //, SynEditTypes;

{ TSynEditMarkupCtrlMouseLink }

constructor TSynEditMarkupCtrlMouseLink.Create(ASynEdit : TCustomControl);
begin
  inherited Create(ASynEdit);
  FCtrlMouseLine:=-1;
  MarkupInfo.Style := [];
  MarkupInfo.StyleMask := [];
  MarkupInfo.Foreground := clBlue; {TODO:  invert blue to bg .... see below}
  MarkupInfo.Background := clNone;
end;

procedure TSynEditMarkupCtrlMouseLink.EndMarkup;
var
  LineLeft, LineTop, LineRight: integer;
  temp : TPoint;
begin
  if FCtrlMouseLine < 1 then exit;
  LineTop := (RowToScreenRow(FCtrlMouseLine)+1)*TSynEdit(SynEdit).LineHeight-1;
  Temp := LogicalToPhysicalPos(Point(FCtrlMouseX1, FCtrlMouseLine));
  LineLeft := TSynEdit(SynEdit).ScreenColumnToXValue(Temp.x);
  Temp := LogicalToPhysicalPos(Point(FCtrlMouseX2, FCtrlMouseLine));
  LineRight := TSynEdit(SynEdit).ScreenColumnToXValue(Temp.x);
  with TSynEdit(SynEdit).Canvas do begin
    Pen.Color := MarkupInfo.Foreground;
    MoveTo(LineLeft,LineTop);
    LineTo(LineRight,LineTop);
  end;
end;

function TSynEditMarkupCtrlMouseLink.GetMarkupAttributeAtRowCol(const aRow, aCol : Integer) : TSynSelectedColor;
begin
  Result := nil;
  if (aRow <> FCtrlMouseLine) or ((aCol < FCtrlMouseX1) or (aCol >= FCtrlMouseX2))
  then exit;
  Result := MarkupInfo;
end;

function TSynEditMarkupCtrlMouseLink.GetNextMarkupColAfterRowCol(const aRow, aCol : Integer) : Integer;
begin
  Result := -1;
  if FCtrlMouseLine <> aRow
  then exit;

  if aCol < FCtrlMouseX1
  then Result := FCtrlMouseX1;
  if (aCol < FCtrlMouseX2) and (aCol >= FCtrlMouseX1)
  then Result := FCtrlMouseX2;
end;

end.

