
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            CSV export filter            }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_E_CSV;

interface

{$I lr_vers.inc}

uses
  Classes, SysUtils, LResources,
  Graphics,GraphType, Controls, Forms, Dialogs,LR_E_TXT,
  LCLType,LCLIntf,lr_utils,lr_class;

type

  TfrQuoteType = (qtNone, qtQuoteChar);

  TfrCSVExport = class(TComponent) 
  public
    Constructor Create(aOwner : TComponent); override;
  end;

  { TfrCSVExportFilter }

  TfrCSVExportFilter = class(TfrTextExportFilter)
  private
    FQuoteChar: TUTF8Char;
    FQuoteType: TfrQuoteType;
    FSeparator: TUTF8Char;
    FCurY     : Integer;
  protected
    procedure GetUsedFont; override;
  public
    constructor Create(AStream: TStream); override;
    procedure OnBeginPage; override;
    procedure OnEndPage; override;
    procedure OnData(x, y: Integer; View: TfrView); override;
    procedure OnText(X, Y: Integer; const Text: String; View: TfrView); override;

    property QuoteChar: TUTF8Char read FQuoteChar write FQuoteChar;
    property QuoteType: TfrQuoteType read FQuoteType write FQuoteType;
    property Separator: TUTF8Char read FSeparator write FSeparator;
  end;


implementation

uses LR_Const;

const
  FIELD_GRAIN  = 32;  // granularity of fields when converting pixel positions

procedure TfrCSVExportFilter.GetUsedFont;
begin
  // never ask usedfont dialog in CSV exporter
end;

constructor TfrCSVExportFilter.Create(AStream: TStream);
begin
  inherited Create(AStream);
  FQuoteType := qtQuoteChar;
  FQuoteChar := '"';
  FSeparator := ';';
end;

procedure TfrCSVExportFilter.OnBeginPage;
begin
  inherited OnBeginPage;
  FCurY := -1;
end;

function CompareIntervals(Item1, Item2: Pointer): Integer;
begin
  result := PtrInt(Item1)-PtrInt(Item2);
end;

procedure TfrCSVExportFilter.OnEndPage;
var
  i,n: Integer;
  p: PfrTextRec;
  s: String;

  procedure AddStr(aStr: string);
  begin
    if QuoteType=qtNone then begin
      if s = '' then
        s := aStr
      else
        s := aStr + Separator + aStr;
    end else begin
      if s = '' then
        s := UTF8Quotedstr(aStr, QuoteChar)
      else
        s := s + Separator + UTF8Quotedstr(aStr, QuoteChar);
    end;
  end;

begin

  n := Lines.Count - 1;
  while n >= 0 do
  begin
    if Lines[n] <> nil then break;
    Dec(n);
  end;

  for i := 0 to n do
  begin
    s := '';
    p := PfrTextRec(Lines[i]);
    while p<>nil do begin
      if P^.Typ in [gtMemo,gtAddin] then
        AddStr(P^.Text);
      p := p^.Next;
    end;
    s := s + LineEnding;
    Stream.Write(s[1], Length(s));
  end;
end;

procedure TfrCSVExportFilter.OnData(x, y: Integer; View: TfrView);
var
  p, p1, p2: PfrTextRec;
  i: Integer;
  s: string;
begin

  if (View = nil) or not (View.ParentBandType in BandTypes) then
    exit;
  if View.Flags and flStartRecord<>0 then
    Inc(FCurY);

  p1 := PfrTextRec(Lines[FCurY]);

  GetMem(p, SizeOf(TfrTextRec));
  FillChar(p^, SizeOf(TfrTextRec), 0);
  p^.Next := nil;
  p^.X := X;
  P^.Typ := View.Typ;
  p^.Text := '';
  for i:=0 to View.Memo.Count-1 do begin
    P^.Text := P^.Text + View.Memo[i];
    if i<>View.Memo.Count-1 then
      P^.Text := P^.Text + LineEnding;
  end;
  if View is TfrMemoView then
    with View as TfrMemoView do
    begin
      p^.FontName := Font.Name;
      p^.FontSize := Font.Size;
      p^.FontStyle := frGetFontStyle(Font.Style);
      p^.FontColor := Font.Color;
      p^.FontCharset := Font.Charset;
    end;
  p^.FillColor := View.FillColor;

  if p1 = nil then
    Lines[FCurY] := TObject(p)
  else
  begin
    p2 := p1;
    while (p1 <> nil) and (p1^.X < p^.X) do
    begin
      p2 := p1;
      p1 := p1^.Next;
    end;
    if p2 <> p1 then
    begin
      p2^.Next := p;
      p^.Next := p1;
    end
    else
    begin
      Lines[FCurY] := TObject(p);
      p^.Next := p1;
    end;
  end;
end;

procedure TfrCSVExportFilter.OnText(X, Y: Integer; const Text: String;
  View: TfrView);
begin
  //
end;

constructor TfrCSVExport.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  frRegisterExportFilter(TfrCSVExportFilter, sCSVFile + ' (*.csv)', '*.csv');
end;

end.

