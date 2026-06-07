unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, AttrGrid;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    AttrLabel: TLabel;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  TypInfo;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  //StringGrid1.DefAlignment := taRightJustify;
  //StringGrid1.DefLayout := tlBottom;

  StringGrid1.CellFontSize[4, 2] := 14;
  StringGrid1.CellFontName[4, 2] := 'Courier';
  StringGrid1.Cells[4, 2] :=
    StringGrid1.CellFontName[4, 2] + LineEnding +
    'FontSize ' + IntToStr(StringGrid1.CellFontSize[4, 2]);
  StringGrid1.CellBkColor[3, 3] := clYellow;
  StringGrid1.CellFontColor[2, 1] := clRed;
  StringGrid1.CellFontStyle[1, 1] := [fsBold];
  StringGrid1.CellAlignment[1, 1] := taCenter;
  StringGrid1.CellWordWrap[2, 2] := true;
  StringGrid1.CellLayout[1, 2] := tlTop;
  StringGrid1.Cells[2, 2] := 'This is a long text.';
  StringGrid1.CellFontStyle[4, 4] := [fsItalic, fsUnderline];
  StringGrid1.CellAlignment[4, 4] := taRightJustify;
  StringGrid1.CellBkColor[0, 4] := clMoneyGreen;
  StringGrid1.RowHeights[2] := 2*StringGrid1.DefaultRowHeight;
  StringGrid1.ColWidths[4] := 120;
  StringGrid1Click(nil);
end;

procedure TForm1.StringGrid1Click(Sender: TObject);
var
  attr: PCellAttr;
begin
  attr := StringGrid1.CellAttr[StringGrid1.Col, StringGrid1.Row];
  if attr = nil then
    AttrLabel.Caption := 'Cell attributes:' + LineEnding +
      '  (not used)'
  else
    AttrLabel.Caption := Format(
      'Cell attributes:' + LineEnding +
      '         Alignment: %s' + LineEnding +
      '            Layout: %s' + LineEnding +
      '  Background color: %s' + LineEnding +
      '         Font name: %s' + LineEnding +
      '         Font size: %d' + LineEnding +
      '        Font color: %s' + LineEnding +
      '        Font style: %s' + LineEnding +
      '         Word-wrap: %s',
      [GetEnumName(TypeInfo(TAlignment), integer(attr^.Alignment)),
       GetEnumName(TypeInfo(TTextLayout), integer(attr^.Layout)),
       ColorToString(attr^.BkColor),
       attr^.FontName, attr^.FontSize,
       ColorToString(attr^.FontColor),
       SetToString(PTypeInfo(TypeInfo(TFontstyles)), Integer(attr^.FontStyle), true),
       BoolToStr(attr^.WordWrap, true)
      ]);
end;

end.

