unit Unit2;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LocalizedForms;

type

  { TForm2 }

  TForm2 = class(TLocalizedForm)
    CheckGroup: TCheckGroup;
    GroupBox1: TGroupBox;
    LblSum: TLabel;
    LblMoney: TLabel;
    LblTodayIs: TLabel;
    RadioGroup: TRadioGroup;
    procedure CheckGroupItemClick(Sender: TObject; Index: integer);
    procedure FormCreate(Sender: TObject);
    procedure RadioGroupSelectionChanged(Sender: TObject);
  private
    procedure CalculateSum;
  protected
    procedure UpdateTranslation(ALang: String); override;
  public
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

uses
  LConvEncoding, // for ConvertEncoding - see "UpdateTranslation"
  StringsUnit;

procedure TForm2.FormCreate(Sender: TObject);
begin
  UpdateTranslation(CurrentLang);
end;

{ This procedure shows
  - how to combine translated resource strings by means of the format statement
  - how to use arrays of resource strings: use pointers to the resource strings! }
procedure TForm2.CalculateSum;
const
  numbers: array[1..9] of PString = (@rsOne, @rsTwo, @rsThree, @rsFour, @rsFive,
    @rsSix, @rsSeven, @rsEight, @rsNine);
var
  sum: Integer;
begin
  sum := RadioGroup.ItemIndex + 1;
  if CheckGroup.Checked[0] then sum := sum + 1;
  if CheckGroup.Checked[1] then sum := sum + 2;
  if CheckGroup.Checked[2] then sum := sum + 3;
  LblSum.Caption := Format(rsSumOfSelectedNumbers, [numbers[sum]^]);
end;

procedure TForm2.CheckGroupItemClick(Sender: TObject; Index: integer);
begin
  CalculateSum;
end;

procedure TForm2.RadioGroupSelectionChanged(Sender: TObject);
begin
  CalculateSum;
end;

procedure TForm2.UpdateTranslation(ALang: String);
var
  s: String;
begin
  inherited;

  { DefaultTranslator cannot execute code, i.e. strings combined by means of
    the Format statement are not translated automatically. Such a string is
    created for the caption of LblSum in "CalculateSum" - we have to call this
    method here to get that label translated. }
  CalculateSum;

  { There is a complication for the labels LblTodayIs which displays the current
    date, and with LblMoney with displays some amount of money with the currency
    sign.
    Formatting for these data is extracted from the DefaultFormatSettings.
    The resulting strings are encoded in ansi and do not display locale-specific
    characters. To get this right they have to be converted to UTF8.
    Usually, it is sufficient to call SysToUTF8 for this purpose. Our example,
    however, allows for Hebrew characters which are usually not contained in the
    typical code pages. Therefore, we use'll a more general procedure based on
    ConvertEncoding which allows to specify the source code page which had been
    determined when UpdateFormatSettings had been called in the LocalizedForms
    unit. }
  s := ConvertEncoding(
    FormatDateTime(DefaultFormatSettings.LongDateFormat, date),  // string to convert
    CodePage,      // source encoding as defined by "CodePage"
    EncodingUTF8   // destination encoding - UTF8
  );
  LblTodayIs.Caption := Format(rsTodayIs, [s]);
  { Note: "ConvertEncoding" requires the unit LConvEncoding in the uses clause. }

  { Now the same with LblMoney... }
  LblMoney.Caption := ConvertEncoding(
    Format('%.*n %s', [
      DefaultFormatSettings.CurrencyDecimals,
      10e6,
      DefaultFormatSettings.CurrencyString
    ]),
    CodePage, EncodingUTF8
  );

  { The items of TRadiogroup are not translated automatically. }
  with RadioGroup do begin
    Items[0] := rsOne;
    Items[1] := rsTwo;
    Items[2] := rsThree;
  end;

  { We should translate CheckGroup here also. But we don't do this here
    to demonstrate the effect when the items of the CheckGroup are not
    translated explicitly as we did with the RadioGroup. }
end;

end.

