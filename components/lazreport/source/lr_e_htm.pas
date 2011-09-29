
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            HTM export filter            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_E_HTM;

interface

{$I lr_vers.inc}
{$COPERATORS on}

uses
  Classes, SysUtils, LResources,
  Graphics, GraphType, Controls, Forms, Dialogs, LR_E_TXT,
  LCLType, LCLIntf, LR_Class;

type

  { TStyleDesc }
  TStyleDesc = record
    styleID: AnsiString;
    styleInfo: AnsiString;
  end;

  { TfrHTMExport }

  TfrHTMExport = class(TComponent)
  public
    constructor Create(aOwner: TComponent); override;
  end;

  { TfrHTMExportFilter }

  TfrHTMExportFilter = class(TfrTextExportFilter)
  private
    cssStyles: array of TStyleDesc;
    FUseCSS: boolean;
    styleStartLine: integer;
    outputLines: TStringList;
    FLastField: PfrTextRec;
    function AddStyle(p: PfrTextRec): Integer;
    function ColorToHex(c: TColor): AnsiString;
    function StyleIndex(p: PfrTextRec; AddIfNotFound: boolean = true): Integer;
    function TextStyleID(p: PfrTextRec): AnsiString;
  protected
    procedure AppendLine(const s: AnsiString);
    procedure InsertLine(const s: AnsiString; position: Integer);
    function  GetviewText(View:TfrView): string; override;
    procedure CalcXCoords(var x,w: integer); override;
  public
    constructor Create(AStream: TStream); override;
    destructor Destroy; override;
    procedure OnData(x, y: Integer; View: TfrView); override;
    procedure OnText(X, Y: Integer; const Text: String; View: TfrView); override;
    procedure OnEndPage; override;
    procedure OnEndDoc; override;

    property UseCSS: boolean read FUseCSS write FUseCSS;
  end;


implementation

uses LR_Const;


constructor TfrHTMExportFilter.Create(AStream: TStream);
var
  s: String;
begin
  inherited Create(AStream);
  outputLines:= TStringList.Create;
  SetLength(cssStyles, 0);

  s:= '<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN" "http://www.w3.org/TR/html4/loose.dtd">' + LineEnding;
  AppendLine(s);
  s:= '<html>' + LineEnding +
      '<head>' + LineEnding +
      '<meta name="generator" content="LazReport html exporter">' + LineEnding +
      '<meta http-equiv="content-type" content="text/html; charset=UTF-8">' + LineEnding +
      '<title>LazReport Exported Report</title>' + LineEnding;  // TODO: improve
  AppendLine(s);
  s:= '<!-- CSS section start -->' + LineEnding +
      '<style type="text/css">' + LineEnding;
  AppendLine(s);
  styleStartLine:= outputLines.Count;

  s:= '</style>' + LineEnding +
      '<!-- CSS section end -->' + LineEnding +
      '</head>' + LineEnding + LineEnding;
  AppendLine(s);
  s:= '<body bgColor="#FFFFFF">' + LineEnding;
  AppendLine(s);

  FUseCSS := true;
end;

destructor TfrHTMExportFilter.Destroy;
begin
  SetLength(cssStyles, 0);
  outputLines.Free;
  inherited Destroy;
end;


{%REGION 'procedure TfrHTMExportFilter.OnEndPage' }
procedure TfrHTMExportFilter.OnEndPage;
var
  i, j, n, cw, xp, xp2: integer;
  p: PfrTextRec;
  s, s1, s2, s3, s4, sp, sAlign, sStyle, sEmpCells, sColSpan: AnsiString;
  xPos: TStringList;

  function GetHTMLFontSize(Size: integer): string;
  begin
    case Size of
      6, 7: Result := '1';
      8, 9: Result := '2';
      14..17: Result := '4';
      18..23: Result := '5';
      24..35: Result := '6'
    else
      Result := '7';
    end;
  end;

  function GetHTMLFontStyle(Style: integer): string;
  begin
    Result := '';
    if (Style and $1) <> 0 then
      Result := '<i>';
    if (Style and $2) <> 0 then
      Result := Result + '<b>';
    if (Style and $4) <> 0 then
      Result := Result + '<u>';
  end;

  function GetEndHTMLFontStyle(Style: Integer): String;
  begin
    Result := '';
    if (Style and $4) <> 0 then
      Result := '</u>';
    if (Style and $2) <> 0 then
      Result := Result + '</b>';
    if (Style and $1) <> 0 then
      Result := Result + '</i>';
  end;

  function FormatCellText(const sIn: AnsiString): AnsiString;
  var
    c, m: Integer;
  begin
    Result:= '';
    c:=1;
    while (c<=Length(sIn)) and (sIn[c]=' ') do
      inc(c);
    dec(c);
    for m:=1 to c do
      Result:= Result + '&nbsp;';
     Result:= Result + Copy(sIn, c+1, Length(sIn)-c);
  end;

begin

  n := Lines.Count - 1;
  while n >= 0 do
  begin
    if Lines[n] <> nil then
      break;
    Dec(n);
  end;

  xPos:= TStringList.Create;
  xPos.Sorted:= true;
  for i := 0 to n do
  begin
    p := PfrTextRec(Lines[i]);
    while p <> nil do
    begin
      s:= Format('%.5d', [p^.X]);
      if xPos.IndexOf(s) < 0 then
        xPos.Add(s);
      s:= Format('%.5d', [p^.X + p^.W]);
      if xPos.IndexOf(s) < 0 then
        xPos.Add(s);
      p:= p^.Next;
    end;
  end;

  s := '<table align="center" width="90%">'+LineEnding;
  s += '<tr>';
  for j:=1 to xPos.Count do
    s += '<td></td>';
  s += '</tr>'+LineEnding;
  AppendLine(s);

  for i := 0 to n do
  begin

    p := PfrTextRec(Lines[i]);
    s := '<tr>';
    cw:= 0;
    while p <> nil do
    begin

      s1:= '';
      s2:= '';
      s3:= '';
      s4:= '';
      sEmpCells:= '';
      sColSpan:= '';
      sAlign:= '';
      sStyle:= '';

      if (p^.FontColor = clWhite) or (p^.FontColor = clNone) then
        p^.FontColor := clBlack;

      if FUseCSS then
      begin
        sStyle:= Format(' class="fs%d"', [StyleIndex(p, true)]);
      end
      else
      begin
        if p^.FontColor <> clBlack then
          s1:= ' Color="' + ColorToHex(p^.FontColor) + '"';
        // most reports is done with font size = 10..13 - treat it as default font
        if not (p^.FontSize in [10..13]) then
          s1 := s1 + ' Size=' + GetHTMLFontSize(p^.FontSize);
        if p^.FontStyle <> 0 then
        begin
          s2 := GetHTMLFontStyle(p^.FontStyle);
          s3 := GetEndHTMLFontStyle(p^.FontStyle);
        end;
        if s1 <> '' then
        begin
          s1 := '<Font' + s1 + '>';
          s4 := '</Font>';
        end;
      end;

      case p^.Alignment of
        taRightJustify: sAlign:= ' align="right"';
        taCenter:       sAlign:= ' align="center"';
      end;

      sp:= Format('%.5d', [p^.X]);
      xp:= xPos.IndexOf(sp);
      sp:= Format('%.5d', [p^.X + p^.W]);
      xp2 := 0;
      xPos.Find(sp, xp2);
      if Assigned(p^.Next) then
      begin
        sp:= Format('%.5d', [p^.Next^.X]);
        if xPos.IndexOf(sp)<xp2 then
          xp2:= xPos.IndexOf(sp);
      end;
      if xp>cw then
        if (xp-cw)>1 then
          sEmpCells:= Format('<td colspan=%d></td>', [xp - cw])
        else
          sEmpCells:= '<td></td>';

      if (xp2-xp)>1 then
        sColSpan:= Format(' colspan=%d', [xp2 - xp]);
      cw:= xp2;

      s := Format('%s%s<td%s%s%s>%s%s%s%s%s</td>', [s, sEmpCells, sAlign, sStyle,
                  sColSpan, s1, s2, FormatCellText(p^.Text), s3, s4]);
      p := p^.Next;
    end;

    if s = '<tr>' then
      s += '<td></td>';

    s += '</tr>';
    AppendLine(s + LineEnding);

  end;

  xPos.Free;

  s := '</table>' + LineEnding;
  AppendLine(s);
end;
{%ENDREGION }


function TfrHTMExportFilter.TextStyleID(p: PfrTextRec): AnsiString;
var
  x: Integer;
begin
  Result:= '(none)';
  if p=nil then
    exit;
  Result := p^.FontName;
  Result += LowerCase(IntToHex(p^.FontSize, 2) + IntToHex(p^.FontStyle, 2) +
                      IntToHex(p^.FontColor, 8) + IntToHex(p^.FillColor, 8) +
                      IntToHex(Integer(p^.Borders), 2));
  for x:=1 to Length(Result) do
    if not (Result[x] in ['$', '%', '&', '0'..'9', '@'..'z']) then
      Result[x]:= '_';
end;


function TfrHTMExportFilter.StyleIndex(p: PfrTextRec; AddIfNotFound: boolean): integer;
var
  s: string;
  x: integer;
begin
  Result:= -1;
  s:= TextStyleID(p);
  for x:=0 to High(cssStyles) do
    if cssStyles[x].styleID = s then
    begin
      Result:= x;
      break;
    end;
  if (Result<0) and AddIfNotFound then
    Result:= AddStyle(p);
end;


function TfrHTMExportFilter.AddStyle(p: PfrTextRec): Integer;
var
  s: string;
begin
  Result:= Length(cssStyles);
  SetLength(cssStyles, Result+1);
  cssStyles[Result].styleID:= TextStyleID(p);
  s:= '';
  if Assigned(p) then
  begin
    // s += Format(' /* Cell Style "%s" */'#10, [cssStyles[Result].styleID]);
    s += Format(' td.fs%d {%s', [Result,LineEnding]);
    s += Format('  font-family: "%s";%s', [p^.FontName,LineEnding]);
    s += Format('  font-size: %dpt;%s', [p^.FontSize,LineEnding]);
    if (p^.FontStyle and $1) <> 0 then
      s += '  font-style: italic;'+LineEnding;
    if (p^.FontStyle and $2) <> 0 then
      s += '  font-weight: bold;'+LineEnding;
    if (p^.FontStyle and $4) <> 0 then
      s += '  text-decoration: underline;'+LineEnding;
    if (p^.FontColor <> clNone) and (p^.FontColor <> clDefault) and (p^.FontColor <> clBlack) then
      s += Format('  color: %s;%s', [ColorToHex(p^.FontColor),LineEnding]);
    if (p^.FillColor <> clNone) and (p^.FillColor <> clDefault) and (p^.FillColor <> clWhite) then
      s += Format('  background-color: %s;%s', [ColorToHex(p^.FillColor),LineEnding]);
    if (p^.Borders <> []) then
    begin
      case p^.BorderStyle of
        frsSolid:      s += '  border-style: solid;'+LineEnding;
        frsDash:       s += '  border-style: dashed;'+LineEnding;
        frsDot,
        frsDashDot,
        frsDashDotDot: s += '  border-style: dotted;'+LineEnding;
        frsDouble:     s += '  border-style: double;'+LineEnding;
      end;
      if not (frbLeft in p^.Borders) then
        s += '  border-left-style: none;'+LineEnding;
      if not (frbTop in p^.Borders) then
        s += '  border-top-style: none;'+LineEnding;
      if not (frbRight in p^.Borders) then
        s += '  border-right-style: none;'+LineEnding;
      if not (frbBottom in p^.Borders) then
        s += '  border-bottom-style: none;'+LineEnding;
      s += Format('  border-width: %dpx;%s', [p^.BorderWidth,LineEnding]);
      s += Format('  border-color: %s;%s', [ColorToHex(p^.BorderColor),LineEnding]);
    end;
    s += ' } '+LineEnding+LineEnding;
  end;
  cssStyles[Result].styleInfo:= s;
end;


function TfrHTMExportFilter.ColorToHex(c: TColor): AnsiString;
var
  s: AnsiString;
begin
  s:= IntToHex(ColorToRGB(c), 8);
  Result:= '#' + Copy(s, 7, 2) + Copy(s, 5, 2) + Copy(s, 3, 2);
end;


procedure TfrHTMExportFilter.AppendLine(const s: AnsiString);
begin
  outputLines.Add(s);
end;


procedure TfrHTMExportFilter.InsertLine(const s: AnsiString; position: Integer);
begin
  outputLines.Insert(position, s);
end;

procedure TfrHTMExportFilter.OnData(x, y: Integer; View: TfrView);
begin
  FLastField := AddData(x, y, View);
end;

procedure TfrHTMExportFilter.OnText(X, Y: Integer; const Text: String;
  View: TfrView);
begin
  if FLastField^.Text='' then
    FLastField^.Text := Text
  else
    FLastField^.Text := FLastField^.Text + '<br>' + Text;
end;

function TfrHTMExportFilter.GetviewText(View: TfrView): string;
begin
  result := '';
end;

procedure TfrHTMExportFilter.CalcXCoords(var x, w: integer);
begin
  x := round(x/UsedFont);
  w := round(w/UsedFont);
end;


procedure TfrHTMExportFilter.OnEndDoc;
var
  s: string;
  x: Integer;
begin
  s := '</body>'+LineEnding+'</html>'+LineEnding;
  AppendLine(s);
  for x:=0 to High(cssStyles) do
    InsertLine(cssStyles[x].StyleInfo, styleStartLine + x);
  for x:= 0 to Pred(outputLines.Count) do
    if Length(outputLines[x])>0 then
      Stream.Write(outputLines[x][1], Length(outputLines[x]));
end;


{ TfrHTMExport }

constructor TfrHTMExport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  frRegisterExportFilter(TfrHTMExportFilter, sHTMFile + ' (*.htm)', '*.htm');
end;

end.
