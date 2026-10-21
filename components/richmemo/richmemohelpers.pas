{
 richmemohelpers.pas

 Author: Dmitry 'skalogryz' Boyarintsev

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit RichMemoHelpers;

interface

{$IFDEF FPC_FULLVERSION >= 20600}
uses
  SysUtils, Graphics, RichMemo;

type
  TRichEditFromRichMemo = class(TObject);
  TTextAttributes = class(TRichEditFromRichMemo);
  TParaAttributes = class(TRichEditFromRichMemo);

  TRichEditAlignment = (taLeftJustify, taRightJustify, taCenter, taFullJustify);

  { TRichEditTextAttributes }

  TRichEditTextAttributes = class helper for TTextAttributes
  private
    function GetColor: TColor;
    function GetStyles: TFontStyles;
    procedure SetColor(AValue: TColor);
    function GetName: string;
    procedure SetName(const AValue: string);
    function GetSize: Integer;
    procedure SetSize(const ASize: Integer);
    procedure SetStyles(AValue: TFontStyles);
  public
    property Color: TColor read GetColor write SetColor;
    property Name: string read GetName write SetName;
    property Size: Integer read GetSize write SetSize;
    property Style: TFontStyles read GetStyles write SetStyles;
  end;

  { TRichEditParaAttributes }

  TRichEditParaAttributes = class helper for TParaAttributes
  private
    function GetFirstIndent: Integer;
    function GetLeftIndent: Integer;
    function GetRightIndent: Integer;
    function GetTab(Index: Byte): Integer;
    function GetTabCount: Integer;
    procedure SetFirstIndent(AValue: Integer);
    procedure SetLeftIndent(AValue: Integer);
    procedure SetRightIndent(AValue: Integer);
    procedure SetTab(Index: Byte; AValue: Integer);
    procedure SetTabCount(AValue: Integer);
  protected
    function GetAlignment: TRichEditAlignment;
    procedure SetAlignment(const AAlignment: TRichEditAlignment);
  public
    property Alignment: TRichEditAlignment read GetAlignment write SetAlignment;
    property FirstIndent: Integer read GetFirstIndent write SetFirstIndent;
    property LeftIndent: Integer read GetLeftIndent write SetLeftIndent;
    property RightIndent: Integer read GetRightIndent write SetRightIndent;
    property Tab[Index: Byte]: Integer read GetTab write SetTab;
    property TabCount: Integer read GetTabCount write SetTabCount;
  end;

  { TRichEditForMemo }

  TSearchType = (stWholeWord, stMatchCase);
  TSearchTypes = set of TSearchType;

  TRichEditForMemo = class helper for TCustomRichMemo
  public
    function SelAttributes: TTextAttributes;
    function Paragraph: TParaAttributes;
    function FindText(const SearchStr: String; StartPos, Length: Integer; Options: TSearchTypes): Integer;
    procedure Print(const ACaption: String); overload;
  end;
{$ELSE}
  {$WARNING Class Helpers require FPC 2.6.0 or later, RichEdit compatible methods will not be available }
{$ENDIF}

implementation

{$IFDEF FPC_FULLVERSION >= 20600}

{ TRichEditTextAttributes }

function TRichEditTextAttributes.GetColor: TColor;
var
  prm : TFontParams;
  m   : TCustomRichMemo;
begin
  m := TCustomRichMemo(TObject(Self));
  m.GetTextAttributes(m.SelStart, prm);
  Result:=prm.Color;
end;

function TRichEditTextAttributes.GetStyles: TFontStyles;
var
  prm : TFontParams;
  m   : TCustomRichMemo;
begin
  m := TCustomRichMemo(TObject(Self));
  m.GetTextAttributes(m.SelStart, prm);
  Result:=prm.Style;
end;

function TRichEditTextAttributes.GetName: string;
var
  m : TCustomRichMemo;
  prm: TFontParams;
begin
  m := TCustomRichMemo(TObject(Self));
  m.GetTextAttributes(m.SelStart, prm);
  Result:=prm.Name;
end;

procedure TRichEditTextAttributes.SetColor(AValue: TColor);
var
  m : TCustomRichMemo;
begin
  m := TCustomRichMemo(TObject(Self));
  m.SetRangeParams( m.SelStart, m.SelLength, [tmm_Color], '', 0, AValue, [], []);
end;

procedure TRichEditTextAttributes.SetName(const AValue: string);
var
  m : TCustomRichMemo;
begin
  m := TCustomRichMemo(TObject(Self));
  m.SetRangeParams( m.SelStart, m.SelLength, [tmm_Name], AValue, 0, 0, [], []);
end;

function TRichEditTextAttributes.GetSize: Integer;
var
  m  : TCustomRichMemo;
  prm: TFontParams;
begin
  m := TCustomRichMemo(TObject(Self));
  m.GetTextAttributes(m.SelStart, prm);
  Result:=prm.Size;
end;

procedure TRichEditTextAttributes.SetSize(const ASize: Integer);
var
  m : TCustomRichMemo;
begin
  m := TCustomRichMemo(TObject(Self));
  m.SetRangeParams( m.SelStart, m.SelLength, [tmm_Size], '', ASize, 0, [], []);
end;

const
  AllFontStyles : TFontStyles = [fsBold, fsItalic, fsUnderline, fsStrikeOut];

procedure TRichEditTextAttributes.SetStyles(AValue: TFontStyles);
var
  m   : TCustomRichMemo;
begin
  m := TCustomRichMemo(TObject(Self));
  m.SetRangeParams(m.SelStart, m.SelLength, [tmm_Styles], '', 0, 0, AValue, AllFontStyles - AValue);
end;

{ TRichEditParaAttributes }

function TRichEditParaAttributes.GetFirstIndent: Integer;
var
  m   : TCustomRichMemo;
  mt  : TParaMetric;
begin
  m := TCustomRichMemo(TObject(Self));
  m.GetParaMetric( m.SelStart, mt);
  Result := Round((mt.FirstLine - mt.HeadIndent));
end;

function TRichEditParaAttributes.GetLeftIndent: Integer;
var
  m   : TCustomRichMemo;
  mt  : TParaMetric;
begin
  m := TCustomRichMemo(TObject(Self));
  m.GetParaMetric( m.SelStart, mt);
  Result := Round(( mt.HeadIndent) );
end;

function TRichEditParaAttributes.GetRightIndent: Integer;
var
  m   : TCustomRichMemo;
  mt  : TParaMetric;
begin
  m := TCustomRichMemo(TObject(Self));
  m.GetParaMetric( m.SelStart, mt);
  Result := Round(( mt.TailIndent));
end;

function TRichEditParaAttributes.GetTab(Index: Byte): Integer;
var
  m  : TCustomRichMemo;
  stop : TTabStopList;
  idx : integer;
begin
  idx:=Index;
  m:=TCustomRichMemo(TObject(Self));
  m.GetParaTabs(m.SelStart, stop);
  if (idx<0) or (idx>=stop.Count) then Result:=0
  else Result:=round(stop.Tabs[idx].Offset);
end;

function TRichEditParaAttributes.GetTabCount: Integer;
var
  m  : TCustomRichMemo;
  stop : TTabStopList;
begin
  m:=TCustomRichMemo(TObject(Self));
  m.GetParaTabs(m.SelStart, stop);
  Result:=stop.Count;
end;

procedure TRichEditParaAttributes.SetFirstIndent(AValue: Integer);
var
  m   : TCustomRichMemo;
  mt  : TParaMetric;
begin
  m := TCustomRichMemo(TObject(Self));
  m.GetParaMetric( m.SelStart, mt);
  mt.FirstLine:=mt.HeadIndent + AValue;
  m.SetParaMetric( m.SelStart, m.SelLength, mt);
end;

procedure TRichEditParaAttributes.SetLeftIndent(AValue: Integer);
var
  m   : TCustomRichMemo;
  mt  : TParaMetric;
begin
  m := TCustomRichMemo(TObject(Self));
  m.GetParaMetric( m.SelStart, mt);
  mt.HeadIndent:=AValue;
  m.SetParaMetric( m.SelStart, m.SelLength, mt);
end;

procedure TRichEditParaAttributes.SetRightIndent(AValue: Integer);
var
  m   : TCustomRichMemo;
  mt  : TParaMetric;
begin
  m := TCustomRichMemo(TObject(Self));
  m.GetParaMetric( m.SelStart, mt);
  mt.TailIndent:=AValue;
  m.SetParaMetric( m.SelStart, m.SelLength, mt);
end;

procedure TRichEditParaAttributes.SetTab(Index: Byte; AValue: Integer);
var
  m  : TCustomRichMemo;
  stop : TTabStopList;
  idx : integer;
begin
  idx:=Index;
  m:=TCustomRichMemo(TObject(Self));
  m.GetParaTabs(m.SelStart, stop);
  if (idx<0) or (idx>=stop.Count) then
    Exit
  else begin
    stop.Tabs[idx].Offset:=AValue;
    m.SetParaTabs(m.SelStart, m.SelLength, stop);
  end;
end;

procedure TRichEditParaAttributes.SetTabCount(AValue: Integer);
var
  m  : TCustomRichMemo;
  stop : TTabStopList;
begin
  m:=TCustomRichMemo(TObject(Self));
  m.GetParaTabs(m.SelStart, stop);
  if stop.Count<AValue then
    SetLength(stop.Tabs, AValue);
  stop.Count:=AValue;
  m.SetParaTabs(m.SelStart, m.SelLength, stop);
end;

function TRichEditParaAttributes.GetAlignment: TRichEditAlignment;
var
  m  : TCustomRichMemo;
  al :TParaAlignment;
begin
  m:=TCustomRichMemo(TObject(Self));
  m.GetParaAlignment(m.SelStart, al);
  case al of
    paRight:   Result:=taRightJustify;
    paCenter:  Result:=taCenter;
    paJustify: Result:=taFullJustify;
  else
    Result:=taLeftJustify;
  end;
end;

procedure TRichEditParaAttributes.SetAlignment(const AAlignment: TRichEditAlignment);
var
  m  : TCustomRichMemo;
const
  ReToMemA : array [TRichEditAlignment] of TParaAlignment = (paLeft, paRight, paCenter, paJustify);
begin
  m:=TCustomRichMemo(TObject(Self));
  m.SetParaAlignment(m.SelStart, m.SelLength, ReToMemA[AAlignment]);
end;

{ TRichEditForMemo }

function TRichEditForMemo.SelAttributes: TTextAttributes;
begin
  Result:=TTextAttributes(TObject(Self));
end;

function TRichEditForMemo.Paragraph: TParaAttributes;
begin
  Result:=TParaAttributes(TObject(Self));
end;

function TRichEditForMemo.FindText(const SearchStr: String; StartPos,
  Length: Integer; Options: TSearchTypes): Integer;
var
  sub : WideString;
  src : WideString;
begin
  src := UTF8Decode( TCustomRichMemo(Self).Text );
  sub := UTF8Decode(SearchStr);
  if not (stMatchCase in Options) then begin
    src:=WideUpperCase(src);
    sub:=WideUpperCase(sub);
  end;
  src:=Copy(src, StartPos+1, Length);
  Result:=Pos(sub, src);
  if Result<=0 then Result:=-1
  else Result:=StartPos+Result-1;
end;

procedure TRichEditForMemo.Print(const ACaption: String);
var
  prm : TPrintParams;
begin
  InitPrintParams(prm);
  prm.JobTitle:=ACaption;
  Print(prm);
end;

{$ENDIF}

end.

