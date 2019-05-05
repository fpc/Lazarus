
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
  Classes, SysUtils, LResources, LazUTF8,
  Graphics,GraphType, Controls, Forms, Dialogs,LR_E_TXT,
  LCLType,LCLIntf,lr_utils,lr_class;

type

  TfrQuoteType = (qtNone, qtQuoteChar, qtAutoQuote);

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
  protected
    procedure GetUsedFont; override;
    procedure CalcXCoords(var {%H-}x,{%H-}w: integer); override;
  public
    constructor Create(AStream: TStream); override;
    procedure OnEndPage; override;
    procedure OnData(x, y: Integer; View: TfrView); override;
    procedure OnText({%H-}X, {%H-}Y: Integer; const {%H-}Text: String; {%H-}View: TfrView); override;

    property QuoteChar: TUTF8Char read FQuoteChar write FQuoteChar;
    property QuoteType: TfrQuoteType read FQuoteType write FQuoteType;
    property Separator: TUTF8Char read FSeparator write FSeparator;
  end;


implementation

uses LR_Const;

procedure TfrCSVExportFilter.GetUsedFont;
begin
  // never ask usedfont dialog in CSV exporter
end;

procedure TfrCSVExportFilter.CalcXCoords(var x, w: integer);
begin
  // do not convert x coords in CSV exporter
end;

constructor TfrCSVExportFilter.Create(AStream: TStream);
begin
  inherited Create(AStream);
  FQuoteType := qtQuoteChar;
  FQuoteChar := '"';
  FSeparator := ',';
  BandTypes := [btMasterHeader,btMasterData,btColumnHeader];
end;

function CompareIntervals(Item1, Item2: Pointer): Integer;
begin
  result := Integer({%H-}PtrInt(Item1)-{%H-}PtrInt(Item2));
end;

procedure TfrCSVExportFilter.OnEndPage;
var
  i,n: Integer;
  p: PfrTextRec;
  s: String;

  procedure AddStr(aStr: string);
  var
    Qt: TfrQuoteType;
  begin
    Qt := QuoteType;
    if Qt=qtAutoQuote then begin
      if pos(Separator, aStr)<>0 then
        Qt := qtQuoteChar
      else
        Qt := qtNone;
    end;
    if Qt=qtNone then begin
      if s = '' then
        s := aStr
      else
        s := s + Separator + aStr;
    end else
    begin
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
begin
  AddData(x, y, View);
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

