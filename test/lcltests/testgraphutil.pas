unit TestGraphUtil;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, TestGlobals,
  LCLType, Graphics, GraphUtil;

type

  TGraphUtilTest= class(TTestCase)
  published
    procedure TestRGBtoHLS;
    procedure TestRGBtoHSV;
    procedure TestRGBtoHSVRange;
    procedure TestHLStoRGB;
    procedure TestHSVtoRGB;
    procedure TestHSVtoRGBTriple;

    procedure TestNormalizeRect;
    procedure TestWordWrap;
  end;

implementation

type
  TTestColor = record
    Color: TColor;
    R: Byte;      // Red 0..255
    G: Byte;      // Green 0..255
    B: Byte;      // Blue 0..255
    H: Integer;   // Hue in HSV and HLS models, 0..359
    S: Integer;   // Saturation in HSV model, 0..255
    V: Integer;   // Value in HSV model, 0..255
    Shls: Integer;// Saturation in HLS model, 0..255
    L: Integer;   // Lightness in HLS model, 0..255
  end;

const
  // HSV/HLS values from https://www.peko-step.com/en/tool/hslrgb_en.html
  // Range (H) is 0..350, Range(S, V, L) is 0..255
  TEST_COLORS: array[0..23] of TTestColor = (
    (Color: clBlack;      R:  0; G:  0; B:  0; H:  0; S:  0; V:  0; Shls:  0; L:  0),
    (Color: clMaroon;     R:128; G:  0; B:  0; H:  0; S:255; V:128; Shls:255; L: 64),
    (Color: clGreen;      R:  0; G:128; B:  0; H:120; S:255; V:128; Shls:255; L: 64),
    (Color: clOlive;      R:128; G:128; B:  0; H: 60; S:255; V:128; Shls:255; L: 64),
    (Color: clNavy;       R:  0; G:  0; B:128; H:240; S:255; V:128; Shls:255; L: 64),
    (Color: clPurple;     R:128; G:  0; B:128; H:300; S:255; V:128; Shls:255; L: 64),
    (Color: clTeal;       R:  0; G:128; B:128; H:180; S:255; V:128; Shls:255; L: 64),
    (Color: clGray;       R:128; G:128; B:128; H:  0; S:  0; V:128; Shls:  0; L:128),
    (Color: clSilver;     R:192; G:192; B:192; H:  0; S:  0; V:192; Shls:  0; L:192),
    (Color: clRed;        R:255; G:  0; B:  0; H:  0; S:255; V:255; Shls:255; L:127),
    (Color: clLime;       R:  0; G:255; B:  0; H:120; S:255; V:255; Shls:255; L:127),
    (Color: clYellow;     R:255; G:255; B:  0; H: 60; S:255; V:255; Shls:255; L:127),
    (Color: clBlue;       R:  0; G:  0; B:255; H:240; S:255; V:255; Shls:255; L:127),
    (Color: clFuchsia;    R:255; G:  0; B:255; H:300; S:255; V:255; Shls:255; L:127),
    (Color: clAqua;       R:  0; G:255; B:255; H:180; S:255; V:255; Shls:255; L:127),
    (Color: clDkGray;     R:128; G:128; B:128; H:  0; S:  0; V:128; Shls:  0; L:128),
    (Color: clWhite;      R:255; G:255; B:255; H:  0; S:  0; V:255; Shls:  0; L:255),
    (Color: clMoneyGreen; R:192; G:220; B:192; H:120; S: 32; V:220; Shls: 72; L:206),
    (Color: clSkyBlue;    R:166; G:202; B:240; H:210; S: 78; V:240; Shls:181; L:203),
    (Color: clCream;      R:255; G:251; B:240; H: 44; S: 15; V:255; Shls:255; L:247),
    (Color: clMedGray;    R:160; G:160; B:164; H:240; S:  6; V:164; Shls:  5; L:162),
    (Color: $1E90FF;      R: 30; G:144; B:255; H:209; S:225; V:255; Shls:255; L:142),  // DodgerBlue
    (Color: $ADFF2F;      R:173; G:255; B: 47; H: 83; S:208; V:255; Shls:255; L:151),  // GreenYellow
    (Color: $CD5C5C;      R:205; G: 92; B: 92; H:  0; S:140; V:205; Shls:135; L:148)   // IndianRed
  );

{ Tests the conversion from RGB to HLS. Is used by the GraphUtil routines
  RGBToHLS and ColorToHLS.
  NOTE: The hue value ranges between 0 and 255. The internet source providing
  the test colors, however, has hue ranging between 0 and 360. Therefore, a
  conversion between both systems is needed which can cause some rounding error. }
procedure TGraphUtilTest.TestRGBtoHLS;
const
  EPS = 1.0;
var
  i: Integer;
  hh, ll, ss: byte;
  errorStr: String;
begin
  for i := 0 to High(TEST_COLORS) do
    with TEST_COLORS[i] do
    begin
      RGBtoHLS(R, G, B, hh, ll, ss);
      errorStr := ' mismatch for color ' + ColorToString(Color);
      AssertEquals('Hue' + errorStr, 1.0*H, hh/255*360, EPS);
      AssertEquals('Lightness' + errorStr, L, ll, EPS);
      AssertEquals('Saturation' + errorStr, Shls, ss, EPS);
    end;
end;

{ Tests the conversion from RGB to HSV. Is used by the GraphUtil routines
  RGBToHSV, ColorToHSV.
  NOTE: H, S, V are expressed in these routines as float numbers ranging
  between 0.0 and 1.0. Since the internet source providing the test color
  values has Hue (0..360), S (0..255) and V (0..255), a corresponding
  multiplication must be made in the comparison. }
procedure TGraphUtilTest.TestRGBtoHSV;
const
  EPS = 1.0;
var
  i: Integer;
  hh, ss, vv: Double;
  errorStr: String;
begin
  for i := 0 to High(TEST_COLORS) do
    with TEST_COLORS[i] do
    begin
      RGBtoHSV(R, G, B, hh, ss, vv);
      errorStr := ' mismatch for color ' + ColorToString(Color);
      AssertEquals('Hue' + errorStr, 1.0*H, hh*360, EPS);
      AssertEquals('Saturation' + errorStr, 1.0*S, ss*255, EPS);
      AssertEquals('Value' + errorStr, 1.0*V, vv*255, EPS);
    end;
end;

procedure TGraphUtilTest.TestRGBToHSVRange;
const
  EPS = 1.0;
var
  i: Integer;
  hh, ss, vv: Integer;
  errorStr: String;
begin
  for i := 0 to High(TEST_COLORS) do
    with TEST_COLORS[i] do
    begin
      RGBToHSVRange(R, G, B, hh, ss, vv);
      errorStr := ' mismatch of color ' + ColorToString(Color);
      //                             expected  actual
      //                                    |  |
      AssertEquals('Hue' + errorStr,        H, hh, EPS);
      AssertEquals('Saturation' + errorStr, S, ss, EPS);
      AssertEquals('Value' + errorStr,      V, vv, EPS);
    end;
end;

procedure TGraphUtilTest.TestHLSToRGB;
const
  EPS = 4.0;    // pretty large tolerance required due to renormalization of hue value...
var
  i: Integer;
  rr, gg, bb: Byte;
  errorStr: String;
begin
  for i := 0 to High(TEST_COLORS) do
    with TEST_COLORS[i] do
    begin
      HLStoRGB(round(H/360*255), L, Shls, rr, gg, bb);
      errorStr := ' mismatch of color ' + ColorToString(Color);
      AssertEquals('Red' + errorStr, R, rr, EPS);  // allow tolerance of EPS digits
      AssertEquals('Green' + errorStr, G, gg, EPS);
      AssertEquals('Blue' + errorStr, B, bb, EPS);
    end;
end;

procedure TGraphUtilTest.TestHSVToRGB;
const
  EPS = 2.0;   // Allow tolerance because the compare values are rounded to integers.
var
  i: Integer;
  rr, gg, bb: integer;
  errorStr: String;
begin
  for i := 0 to High(TEST_COLORS) do
    with TEST_COLORS[i] do
    begin
      HSVtoRGB(H/360, S/255, V/255, rr, gg, bb);
      errorStr := ' mismatch of color ' + ColorToString(Color);
      AssertEquals('Red' + errorStr, R, rr, EPS);  // allow tolerance of EPS digits
      AssertEquals('Green' + errorStr, G, gg, EPS);
      AssertEquals('Blue' + errorStr, B, bb, EPS);
    end;
end;

{ Tests the conversion from HSV to RGB. The tested routine HSVtoRGBTriple is
  used by the GraphUtil routines HSVtoRGBTriple, HSVtoRGBRange, HSVRangeToColor,
  HSVtoRGBQuad.
  Hue is expressed as integer between 0 and 359, Saturation and Value are
  integers between 0 and 255. }
procedure TGraphUtilTest.TestHSVToRGBTriple;
const
  EPS = 3.0;   // Value adjusted so that all test colors pass.
var
  i: Integer;
  rgbt: TRGBTriple;
  errorStr: String;
begin
  for i := 0 to High(TEST_COLORS) do
    with TEST_COLORS[i] do
    begin
      rgbt := HSVtoRGBTriple(H, S, V);
      errorStr := ' mismatch of color ' + ColorToString(Color);
      AssertEquals('RGBTriple.rgbRED' + errorStr, 1.0*R, 1.0*rgbt.rgbtRed, EPS);
      AssertEquals('RGBTriple.rgbGREEN' + errorStr, 1.0*G, 1.0*rgbt.rgbtGreen, EPS);
      AssertEquals('RGBTriple.rgbBLUE' + errorStr, 1.0*B, 1.0*rgbt.rgbtBlue, EPS);
    end;
end;

procedure TGraphUtilTest.TestNormalizeRect;
var
  R: TRect;
begin
  R := NormalizeRect(Rect(100, 0, 0, 10));
  AssertEquals('Test1: Rect.Left mismatch', 0, R.Left);
  AssertEquals('Test1: Rect.Top mismatch', 0, R.Top);
  AssertEquals('Test1: Rect.Right mismatch', 100, R.Right);
  AssertEquals('Test1: Rest.Bottom mismatch', 10, R.Bottom);

  R := NormalizeRect(Rect(0, 10, 100, 0));
  AssertEquals('Test1: Rect.Left mismatch', 0, R.Left);
  AssertEquals('Test1: Rect.Top mismatch', 0, R.Top);
  AssertEquals('Test1: Rect.Right mismatch', 100, R.Right);
  AssertEquals('Test1: Rest.Bottom mismatch', 10, R.Bottom);

  R := NormalizeRect(Rect(100, 10, 0, 0));
  AssertEquals('Test1: Rect.Left mismatch', 0, R.Left);
  AssertEquals('Test1: Rect.Top mismatch', 0, R.Top);
  AssertEquals('Test1: Rect.Right mismatch', 100, R.Right);
  AssertEquals('Test1: Rest.Bottom mismatch', 10, R.Bottom);
end;

procedure TGraphUtilTest.TestWordWrap;
var
  L: TStringList;
  bmp: TBitmap;
  w: Integer;
begin
  bmp := TBitmap.Create; // Just to have a valid canvas for text width measuring...
  try
    bmp.SetSize(10, 10);
    L := TStringList.Create;
    try
      // Single line
      WordWrap(bmp.Canvas.Font, 'abc', 1000, L);
      AssertEquals('Single line mismatch', 'abc', L[0]);

      L.Clear;
      // 2 lines with linebreak
      WordWrap(bmp.Canvas.Font, 'abc'+LineEnding+'def', 1000, L);
      AssertEquals('Two lines with linebreak: 1st line mismatch', 'abc', L[0]);
      AssertEquals('Two lines with linebreak: 2nd line mismatch', 'def', L[1]);

      L.Clear;
      // Nothing after linebreak
      WordWrap(bmp.Canvas.Font, 'abc'+LineEnding, 1000, L);
      AssertEquals('Nothing after linebreak: 1st line mismatch', 'abc', L[0]);
      AssertEquals('Nothing after linebreak: empty 2nd line mismatch', '', L[1]);

      L.Clear;
      // Wordwrap at space
      w := bmp.Canvas.TextWidth('abc-'); // + 1;
      WordWrap(bmp.Canvas.Font, 'abc def', w, L);
      AssertEquals('Wordwrap at space: 1st line mismatch', 'abc', L[0]);
      AssertEquals('Wordwrap at space: 2nd line mismatch', 'def', L[1]);

      L.Clear;
      // Wordwrap at tab
      WordWrap(bmp.Canvas.Font, 'abc'#9'def', w, L);
      AssertEquals('Wordwrap at tab: 1st line mismatch', 'abc', L[0]);
      AssertEquals('Wordwrap at tab: 2nd line mismatch', 'def', L[1]);

      L.Clear;
      // Wordwrap at hyphen
      WordWrap(bmp.Canvas.Font, 'abc-def', w, L);
      AssertEquals('Wordwrap at hyphen: 1st line mismatch', 'abc-', L[0]);
      AssertEquals('Wordwrap at hyphen: 2nd line mismatch', 'def', L[1]);

      L.Clear;
      // Wordwrap and linebreak
      WordWrap(bmp.Canvas.Font, 'abc def'+LineEnding+'g', w, L);
      AssertEquals('Wordwrap and linebreak: 1st line mismatch', 'abc', L[0]);
      AssertEquals('Wordwrap and linebreak: 2nd line mismatch', 'def', L[1]);
      AssertEquals('Wordwrap and linebreak: 2nd line mismatch', 'g', L[2]);

    finally
      L.Free;
    end;

  finally
    bmp.Free;
  end;
end;

initialization
  AddToLCLTestSuite(TgraphUtilTest);

end.

