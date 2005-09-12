UNIT Main;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  About, StdCtrls, ExtCtrls;

TYPE

  { TForm1 }

  TForm1 = CLASS(TForm)
    Bevel1: TBEVEL;
    LanguageButton: TBUTTON;
    EndButton: TBUTTON;
    AboutButton: TBUTTON;
    CalculateButton: TBUTTON;
    Edit1: TEDIT;
    AshWednesdayLabel: TLABEL;
    YearLabel: TLABEL;
    GoodFridayLabel: TLABEL;
    EasterMondayLabel: TLABEL;
    AscensionDayLabel: TLABEL;
    PentecostLabel: TLABEL;
    CorpusCristiLabel: TLABEL;
    Listbox1: TLISTBOX;
    PROCEDURE Button1CLICK(Sender: TObject);
    PROCEDURE Button2CLICK(Sender: TObject);
    PROCEDURE Button3CLICK(Sender: TObject);
    procedure Form1CREATE(Sender: TObject);
    PROCEDURE Form1SHOW(Sender: TObject);
    procedure LanguageButtonCLICK(Sender: TObject);
    procedure Listbox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  PUBLIC
    FUNCTION CalcEasterday(aYear: Word): TDateTime;
  END;

VAR
  Form1: TForm1; 

IMPLEMENTATION

PROCEDURE TForm1.Button1CLICK(Sender: TObject);
VAR
  Easter: TDateTime;
  aYear : WORD;
BEGIN
  ListBox1.Clear;
  TRY
    aYear := StrToInt(Edit1.Text);
  EXCEPT
    ShowMessage('Fehlerhafte Eingabe des Jahrs!');
    Exit;
  END;
  Easter := CalcEasterday(aYear);
  ListBox1.Items.Add(DateToStr(Easter - 46));
  ListBox1.Items.Add(DateToStr(Easter -  2));
  ListBox1.Items.Add(DateToStr(Easter +  1));
  ListBox1.Items.Add(DateToStr(Easter + 39));
  ListBox1.Items.Add(DateToStr(Easter + 50));
  ListBox1.Items.Add(DateToStr(Easter + 60));
END;

PROCEDURE TForm1.Button2CLICK(Sender: TObject);
BEGIN
  Close;
END;

procedure TForm1.Button3CLICK(Sender: TObject);
begin
  AboutBox.ShowModal;
end;

procedure TForm1.Form1CREATE(Sender: TObject);
begin

end;

PROCEDURE TForm1.Form1SHOW(Sender: TObject);
BEGIN
  Button1CLICK(Sender);
END;

procedure TForm1.LanguageButtonCLICK(Sender: TObject);
begin
  if LanguageButton.Caption='English' then begin
    LanguageButton.Caption:='Deutsch';
    EndButton.Caption:='Exit';
    AboutButton.Caption:='About';
    CalculateButton.Caption:='Calculate';
    AshWednesdayLabel.Caption:='Ash Wednesday';
    YearLabel.Caption:='Year';
    GoodFridayLabel.Caption:='Good Friday';
    EasterMondayLabel.Caption:='Easter Monday';
    AscensionDayLabel.Caption:='Ascension Day';
    PentecostLabel.Caption:='Pentecost';
    CorpusCristiLabel.Caption:='Corspus Cristi';
  end else begin
    LanguageButton.Caption:='English';
    EndButton.Caption:='Exit';
    AboutButton.Caption:='Info';
    CalculateButton.Caption:='Berechne';
    AshWednesdayLabel.Caption:='Aschermittwoch';
    YearLabel.Caption:='Jahr';
    GoodFridayLabel.Caption:='Karfreitag';
    EasterMondayLabel.Caption:='Ostermontag';
    AscensionDayLabel.Caption:='Himmelfahrt';
    PentecostLabel.Caption:='Pfingsten';
    CorpusCristiLabel.Caption:='Fronleichnam';
  end;
end;

procedure TForm1.Listbox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
begin
  Listbox1.Canvas.TextRect(ARect, ARect.Left+2, ARect.Top, Listbox1.Items[Index]);
end;

FUNCTION TForm1.CalcEasterday(aYear: WORD): TDateTime;
VAR
  A, B, C, D, E, F, G, H, I, J, K, L, M, N: INTEGER;
  vDay, vMonth: WORD;
BEGIN
  A := aYear MOD 19;
  B := aYear DIV 100;
  C := aYear MOD 100;

  D := B DIV 4;
  E := B MOD 4;

  F := (B + 8) DIV 25;
  G := (B - F + 1) DIV 3;
  H := (19 * A + B - D - G + 15) MOD 30;
  I := C DIV 4;
  J := C MOD 4;
  K := (32 + 2 * E + 2 * I - H - J) MOD 7;
  L := (A + 11 * H + 22 * K) DIV 451;
  M := (H + K - 7 * L + 114) DIV 31;
  N := (H + K - 7 * L + 114) MOD 31;

  vDay := N + 1;
  IF M = 3 THEN vMonth := 3 ELSE vMonth := 4;
  Result := EncodeDate(aYear, vMonth, vDay);
END;

INITIALIZATION
  {$I main.lrs}

END.

