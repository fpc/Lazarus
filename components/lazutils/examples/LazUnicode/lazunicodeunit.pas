unit LazUnicodeUnit;

{$IFDEF FPC}
 {$mode objfpc}{$H+}
{$ENDIF}

// For testing the UTF16 version.
{$IF DEFINED(FPC) and DEFINED(UseUTF16)}
{$ModeSwitch UnicodeStrings}   // Sets also FPC_UNICODESTRINGS.
{$ENDIF}

{$IF DEFINED(FPC_UNICODESTRINGS) or not DEFINED(FPC)}
 {$DEFINE ReallyUseUTF16}       // FPC with UTF-16 or Delphi
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LazUnicode;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

const
  Eyes = '👀';
  Thai = 'ฃ';   // No idea what it means.
  WineGlass = '🍷';
  Heart = '💓';  // or '♡';
  // Accents in combining codepoints. Last one has 2 consecutive combining marks.
  Combining = 'ÓÓỐỐỚỚÒÒỒỒỎỎỔỔỞỞỌỌBあC'#$CC#$81#$CC#$B2;
  //ArEnStr1 = 'مAرBحCبDاE';

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  s_UTF8: {$IFDEF FPC}AnsiString{$ELSE}String{$ENDIF};
  s, ch: String;
  //CodeUnit: Char;
  i: NativeInt;
  cpIter: TCodePointEnumerator;
  ucIter: TUnicodeCharacterEnumerator;
begin
  Memo1.Lines.Clear;
  s_UTF8 := 'Pöö ' + Eyes + Thai + '. Have ' + WineGlass + ' for FPC' + Heart + 'Lazarus';
  s := s_UTF8;                              // Converts encoding when needed.
  {$IFDEF ReallyUseUTF16}
  Memo1.Lines.Add('Using UnicodeString + UTF-16');
  {$ELSE}
  Memo1.Lines.Add('Using AnsiString + UTF-8');
  {$ENDIF}
  Memo1.Lines.Add(Format('Sizeof(Char) = %d.', [Sizeof(Char)]));
  Memo1.Lines.Add('Testing with string: ' + s);

  Memo1.Lines.Add('');
  ch := CodePointCopy(s, 14, 1);       // Should return the wine glass.
  Memo1.Lines.Add('Testing CodePointCopy. SubString = "' + ch + '"');

  i := CodePointLength(s);             // Should return 30.
  Memo1.Lines.Add(Format('Testing CodePointLength. Result = %d.', [i]));

  // Constant must be assigned to AnsiString when using the UTF-8 system.
  s_UTF8 := WineGlass;
  i := CodePointPos(s_UTF8, s);        // Should return 14.
  Memo1.Lines.Add(Format('Testing CodePointPos. Result = %d.', [i]));
  s_UTF8 := '☐';
  i := CodePointPos(s_UTF8, s);        // Should return 0.
  Memo1.Lines.Add(Format('Testing CodePointPos for non-existent char. Result = %d.', [i]));

  // Use CodePoint enumerator explicitly
  Memo1.Lines.Add('');
  Memo1.Lines.Add('*** Using CodePoint enumerator explicitly: ***');
  cpIter := TCodePointEnumerator.Create(s);
  while cpIter.MoveNext do
    Memo1.Lines.Add(Format('ch=%s has %d codeunits.',
                           [cpIter.Current, cpIter.CurrentCodeUnitCount]));
  cpIter.Free;

  s_UTF8 := Combining;
  s := s_UTF8;                              // Converts encoding when needed.
  Memo1.Lines.Add('');
  Memo1.Lines.Add('Testing with string: ' + s);

  // Use UnicodeCharacter enumerator explicitly
  Memo1.Lines.Add('');
  Memo1.Lines.Add('*** Using UnicodeCharacter enumerator explicitly: ***');
  ucIter := TUnicodeCharacterEnumerator.Create(s);
  while ucIter.MoveNext do
    Memo1.Lines.Add(Format('ch=%s has %d codepoints and %d codeunits.',
           [ucIter.Current, ucIter.CurrentCodePointCount, ucIter.CurrentCodeUnitCount]));
  ucIter.Free;

  {$IFDEF FPC}
  // Use for-in loop for Unicode Characters.
  Memo1.Lines.Add('');
  Memo1.Lines.Add('*** Using for-in loop for Unicode Characters : ***');
  for ch in s do
    Memo1.Lines.Add('ch=' + ch);
  {$ENDIF}

  // for-in loop for codeunits using a Char variable still works.
  {    Uncomment to test.
  Memo1.Lines.Add('');
  Memo1.Lines.Add('*** Using for-in loop for codeunits: ***');
  for CodeUnit in s do
    Memo1.Lines.Add('CodeUnit=',CodeUnit);        // The output makes no sense obviously.
  }
end;

end.

