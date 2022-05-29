unit IpHtmlUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Forms, 
  IpHtmlTypes;
  
function ColorFromString(S: String): TColor;
function TryColorFromString(S: String; out AColor: TColor; out AErrMsg: String): Boolean;

function GetAlignmentForStr(S: string; ADefault: TIpHtmlAlign = haDefault): TIpHtmlAlign;

function AnsiToEscape(const S: string): string;
function EscapeToAnsi(const S: string): string;
function NoBreakToSpace(const S: string): string;

function FindFontName(const AFontList: string): string;

function MaxI2(const I1, I2: Integer) : Integer;
function MinI2(const I1, I2: Integer) : Integer;


implementation

uses
  Translations, LazUTF8, 
  IpConst,IpUtils;

const
  htmlNamedColors: array[0..140] of record
    s: string;
    c: TColor;
  end = ( // alphabetically ordered
    (s:'ALICEBLUE'; c:$FFF8F0),
    (s:'ANTIQUEWHITE'; c:$D7EBFA),
    (s:'AQUA'; c: $FFFF00),
    (s:'AQUAMARINE'; c:$D4FF7F),
    (s:'AZURE'; c:$FFFFF0),
    (s:'BEIGE'; c:$DCF5F5),
    (s:'BISQUE'; c:$C4E4FF),
    (s:'BLACK'; c:clBlack),
    (s:'BLANCHEDALMOND'; c:$CDEBFF),
    (s:'BLUE'; c:$FF0000),
    (s:'BLUEVIOLET'; c:$E22B8A),
    (s:'BROWN'; c:$2A2AA5),
    (s:'BURLYWOOD'; c:$87B8DE),
    (s:'CADETBLUE'; c:$A09E5F),
    (s:'CHARTREUSE'; c:$00FF7F),
    (s:'CHOCOLATE'; c:$1E69D2),
    (s:'CORAL'; c:$507FFF),
    (s:'CORNFLOWERBLUE'; c:$ED9564),
    (s:'CORNSILK'; c:$DCF8FF),
    (s:'CRIMSON'; c:$3C14DC),
    (s:'CYAN'; c: $FFFF00),
    (s:'DARKBLUE'; c:$8B0000),
    (s:'DARKCYAN'; c:$8B8B00),
    (s:'DARKGOLDENROD'; c:$0B86B8),
    (s:'DARKGRAY'; c:$A9A9A9),
    (s:'DARKGREEN'; c:$006400),
    (s:'DARKKHAKI'; c:$6BB7BD),
    (s:'DARKMAGENTA'; c:$8B008B),
    (s:'DARKOLIVEGREEN'; c:$2F6B55),
    (s:'DARKORANGE'; c:$008CFF),
    (s:'DARKORCHID'; c:$CC3299),
    (s:'DARKRED'; c:$00008B),
    (s:'DARKSALMON'; c:$7A96E9),
    (s:'DARKSEAGREEN'; c:$8FBC8F),
    (s:'DARKSLATEBLUE' ; c:$8B3D48),
    (s:'DARKSLATEGRAY'; c:$4F4F2F),
    (s:'DARKTURQUOISE'; c:$D1CE00),
    (s:'DARKVIOLET'; c:$D30094),
    (s:'DARKYELLOW'; c:$008080),
    (s:'DEEPPINK'; c:$9314FF),
    (s:'DEEPSKYBLUE'; c:$FFBF00),
    (s:'DIMGRAY'; c:$696969),
    (s:'DODGERBLUE'; c:$FF901E),
    (s:'FIREBRICK'; c:$2222B2),
    (s:'FLORALWHITE'; c:$F0FAFF),
    (s:'FORESTGREEN'; c:$228B22),
    (s:'FUCHSIA'; c:$FF00FF),
    (s:'GAINSBORO'; c:$DCDCDC),
    (s:'GHOSTWHITE'; c:$FFF8F8),
    (s:'GOLD'; c:$00D7FF),
    (s:'GOLDENROD'; c:$20A5DA),
    (s:'GRAY'; c:$808080),
    (s:'GREEN'; c:$008000),
    (s:'GREENYELLOW'; c:$2FFFAD),
    (s:'HONEYDEW'; c:$F0FFF0),
    (s:'HOTPINK'; c:$B469FF),
    (s:'INDIANRED'; c:$5C5CCD),
    (s:'INDIGO'; c:$82004B),
    (s:'IVORY'; c:$F0FFFF),
    (s:'KHAKI'; c:$8CE6F0),
    (s:'LAVENDER'; c:$FAE6E6),
    (s:'LAVENDERBLUSH'; c:$F5F0FF),
    (s:'LAWNGREEN'; c:$00FC7C),
    (s:'LEMONCHIFFON'; c:$CDFAFF),
    (s:'LIGHTBLUE'; c:$E6D8AD),
    (s:'LIGHTCORAL'; c:$8080F0),
    (s:'LIGHTCYAN'; c:$FFFFE0),
    (s:'LIGHTGOLDENRODYELLOW'; c:$D2FAFA),
    (s:'LIGHTGREEN'; c:$90EE90),
    (s:'LIGHTGREY'; c:$D3D3D3),
    (s:'LIGHTPINK'; c:$C1B6FF),
    (s:'LIGHTSALMON'; c:$7AA0FF),
    (s:'LIGHTSEAGREEN'; c:$AAB220),
    (s:'LIGHTSKYBLUE'; c:$FACE87),
    (s:'LIGHTSLATEGRAY'; c:$998877),
    (s:'LIGHTSTEELBLUE'; c:$DEC4B0),
    (s:'LIGHTYELLOW'; c:$E0FFFF),
    (s:'LIME'; c:$00FF00),
    (s:'LIMEGREEN'; c:$32CD32),
    (s:'LINEN'; c:$E6F0FA),
    (s:'MAGENTA'; c:$FF00FF),
    (s:'MAROON'; c:$000080),
    (s:'MEDIUMAQUAMARINE'; c:$AACD66),
    (s:'MEDIUMBLUE'; c:$CD0000),
    (s:'MEDIUMORCHID'; c:$D355BA),
    (s:'MEDIUMPURPLE'; c:$DB7093),
    (s:'MEDIUMSEAGREEN'; c:$71B33C),
    (s:'MEDIUMSLATEBLUE'; c:$EE687B),
    (s:'MEDIUMSPRINGGREEN'; c:$9AFA00),
    (s:'MEDIUMTURQUOISE'; c:$CCD148),
    (s:'MEDIUMVIOLETRED'; c:$8515C7),
    (s:'MIDNIGHTBLUE'; c:$701919),
    (s:'MINTCREAM'; c:$FAFFF5),
    (s:'MISTYROSE'; c:$E1E4FF),
    (s:'MOCCASIN'; c:$B5E4FF),
    (s:'NAVAJOWHITE'; c:$ADDEFF),
    (s:'NAVY'; c:$800000),
    (s:'OLDLACE'; c:$E6F5FD),
    (s:'OLIVE'; c:$008080),
    (s:'OLIVEDRAB'; c:$238E6B),
    (s:'ORANGE'; c:$00A5FF),
    (s:'ORANGERED'; c:$0045FF),
    (s:'ORCHID'; c:$D670DA),
    (s:'PALEGOLDENROD'; c:$AAE8EE),
    (s:'PALEGREEN'; c:$98FB98),
    (s:'PALETURQUOISE'; c:$EEEEAF),
    (s:'PALEVIOLETRED'; c:$9370DB),
    (s:'PAPAYAWHIP'; c:$D5EFFF),
    (s:'PEACHPUFF'; c:$B9DAFF),
    (s:'PERU'; c:$3F85CD),
    (s:'PINK'; c:$CBC0FF),
    (s:'PLUM'; c:$DDA0DD),
    (s:'POWDERBLUE'; c:$E6E0B0),
    (s:'PURPLE'; c:$800080),
    (s:'RED'; c:$0000FF),
    (s:'ROSYBROWN'; c:$8F8FBC),
    (s:'ROYALBLUE'; c:$901604),
    (s:'SADDLEBROWN'; c:$13458B),
    (s:'SALMON'; c:$7280FA),
    (s:'SANDYBROWN'; c:$60A4F4),
    (s:'SEAGREEN'; c:$578B2E),
    (s:'SEASHELL'; c:$EEF5FF),
    (s:'SIENNA'; c:$2D52A0),
    (s:'SILVER'; c:$C0C0C0),
    (s:'SKYBLUE'; c:$EBCE87),
    (s:'SLATEBLUE'; c:$CD5A6A),
    (s:'SLATEGRAY'; c:$908070),
    (s:'SNOW'; c:$FAFAFF),
    (s:'SPRINGGREEN'; c:$7FFF00),
    (s:'STEELBLUE'; c:$B48246),
    (s:'TAN'; c:$8CB4D2),
    (s:'TEAL'; c:$808000),
    (s:'THISTLE'; c:$D8BFD8),
    (s:'TOMATO'; c:$4763FF),
    (s:'TURQUOISE'; c:$D0E040),
    (s:'VIOLET'; c:$EE82EE),
    (s:'WHEAT'; c:$B3DEF5),
    (s:'WHITE'; c:$FFFFFF),
    (s:'WHITESMOKE'; c:$F5F5F5),
    (s:'YELLOW'; c:$00FFFF),
    (s:'YELLOWGREEN'; c:$32CD9A)
  );

function BinSearchNamedColor(const AColorStr: string; var AColor: TColor): boolean;
var
  First: Integer;
  Last: Integer;
  Pivot: Integer;
begin
  First := Low(htmlNamedColors); //Sets the first item of the range
  Last := High(htmlNamedColors); //Sets the last item of the range
  Result := False; //Initializes the Found flag (Not found yet)

  //If First > Last then the searched item doesn't exist
  //If the item is found the loop will stop
  while (First <= Last) {and (not Result)} do
  begin
    //Gets the middle of the selected range
    Pivot := (First + Last) div 2;
    //Compares the String in the middle with the searched one
    if htmlNamedColors[Pivot].s = AColorStr then
    begin
      Result  := True;
      AColor := htmlNamedColors[Pivot].c;
      exit;
    end
    //If the Item in the middle has a bigger value than
    //the searched item, then select the first half
    else if htmlNamedColors[Pivot].s > AColorStr then
      Last := Pivot - 1
        //else select the second half
    else
      First := Pivot + 1;
  end;
end;

function TryColorFromString(S: String; out AColor: TColor; out AErrMsg: String): Boolean;
var
  R, G, B, Err: Integer;
begin
  Result := false;
  
  AColor := clNone;
  if S = '' then
  begin
    Result := true;
    Exit;
  end;
  
  S := UpperCase(S);
  if S[1] = '#' then
  begin
    if Length(S) <> 7 then
    begin
      AErrMsg := SHtmlInvColor + S;
      Result := false;
    end
    else begin
      val('$'+Copy(S,2,2), R, Err);
      if Err <> 0 then
        R := 255;
      val('$'+Copy(S,4,2), G, Err);
      if Err <> 0 then
        G := 255;
      val('$'+Copy(S,6,2), B, Err);
      if Err <> 0 then
        B := 255;
      AColor := RGBToColor(R, G, B);
      Result := true;
    end;
  end else
  if BinSearchNamedColor(S, AColor) then 
  begin
    Result := true;
    exit;
  end else
  if Length(S) = 6 then
    try
      val('$'+Copy(S,1,2), R, Err);
      if Err <> 0 then
        R := 255;
      val('$'+Copy(S,3,2), G, Err);
      if Err <> 0 then
        G := 255;
      val('$'+Copy(S,5,2), B, Err);
      if Err <> 0 then
        B := 255;
      AColor := RGBToColor(R, G, B);
      Result := true;
    except
      AErrMsg := SHtmlInvColor + S;
    end;
end;

function ColorFromString(S: String): TColor;
var
  msg: String;
begin
  if not TryColorFromString(S, Result, msg) then
    Result := clNone;
end;

function GetAlignmentForStr(S: string;
  ADefault: TIpHtmlAlign = haDefault): TIpHtmlAlign;
begin
  S := UpperCase(S);
  if Length(S) = 0 then
  begin
    Result := ADefault;
    exit;
  end;
  
  case S[1] of
    'C','M': 
      if S = 'CHAR' then 
        Result := haChar
      else if (S = 'CENTER') or (S = 'MIDDLE') then
        Result := haCenter;
    'J': 
      if S = 'JUSTIFY' then Result := haJustify;
    'L': 
      if (S = 'LEFT') then Result := haLeft;
    'R': 
      if S = 'RIGHT' then Result := haRight;
    else 
      Result := ADefault;
  end;
end;

const
  CodeCount = 126;
  
  {Sorted by Size where size is Length(Name).
  Make sure you respect this when adding new items}
  Codes: array[0..Pred(CodeCount)] of record
    Size: Integer;
    Name: String;
    Value: String;
    ValueUtf8: String; //UTF8 DiBo33
  end = (
    (Size: 2; Name: 'gt';   Value: '>'; ValueUtf8: #$3E),
    (Size: 2; Name: 'lt';   Value: '<'; ValueUtf8: #$3C),
    (Size: 3; Name: 'amp';  Value: '&'; ValueUtf8: #$26),
    (Size: 3; Name: 'deg';  Value: #176; ValueUtf8: #$C2#$B0),
    (Size: 3; Name: 'ETH';  Value: #208; ValueUtf8: #$C3#$90),
    (Size: 3; Name: 'eth';  Value: #240; ValueUtf8: #$C3#$B0),
    (Size: 3; Name: 'not';  Value: #172; ValueUtf8: #$C2#$AC),
    (Size: 3; Name: 'reg';  Value: #174; ValueUtf8: #$C2#$AE),
    (Size: 3; Name: 'shy';  Value: ShyChar; ValueUtf8: ShyChar),
    (Size: 3; Name: 'uml';  Value: #168; ValueUtf8: #$C2#$A8),
    (Size: 3; Name: 'yen';  Value: #165; ValueUtf8: #$C2#$A5),
    (Size: 4; Name: 'Auml'; Value: #196; ValueUtf8: #$C3#$84),
    (Size: 4; Name: 'auml'; Value: #228; ValueUtf8: #$C3#$A4),
    (Size: 4; Name: 'bull'; Value: #149; ValueUtf8: #$E2#$80#$A2),
    (Size: 4; Name: 'cent'; Value: #162; ValueUtf8: #$C2#$A2),
    (Size: 4; Name: 'circ'; Value: '^';  ValueUtf8: #$5E),
    (Size: 4; Name: 'copy'; Value: #169; ValueUtf8: #$C2#$A9),
    (Size: 4; Name: 'Euml'; Value: #203; ValueUtf8: #$C3#$8B),
    (Size: 4; Name: 'euml'; Value: #235; ValueUtf8: #$C3#$AB),
    (Size: 4; Name: 'euro'; Value: #128; ValueUtf8: #$E2#$82#$AC),
    (Size: 4; Name: 'fnof'; Value: #131; ValueUtf8: #$C6#$92),
    (Size: 4; Name: 'Iuml'; Value: #207; ValueUtf8: #$C3#$8F),
    (Size: 4; Name: 'iuml'; Value: #239; ValueUtf8: #$C3#$AF),
    (Size: 4; Name: 'macr'; Value: #175; ValueUtf8: #$C2#$AF),
    (Size: 4; Name: 'nbsp'; Value: NbspChar; ValueUtf8: NbspChar),
    (Size: 4; Name: 'ordf'; Value: #170; ValueUtf8: #$C2#$AA),
    (Size: 4; Name: 'ordm'; Value: #186; ValueUtf8: #$C2#$BA),
    (Size: 4; Name: 'Ouml'; Value: #214; ValueUtf8: #$C3#$96),
    (Size: 4; Name: 'ouml'; Value: #246; ValueUtf8: #$C3#$B6),
    (Size: 4; Name: 'para'; Value: #182; ValueUtf8: #$C2#$B6),
    (Size: 4; Name: 'quot'; Value: '"';  ValueUtf8: #$22),
    (Size: 4; Name: 'sect'; Value: #167; ValueUtf8: #$C2#$A7),
    (Size: 4; Name: 'sup1'; Value: #185; ValueUtf8: #$C2#$B9),
    (Size: 4; Name: 'sup2'; Value: #178; ValueUtf8: #$C2#$B2),
    (Size: 4; Name: 'sup3'; Value: #179; ValueUtf8: #$C2#$B3),
    (Size: 4; Name: 'Uuml'; Value: #220; ValueUtf8: #$C3#$9C),
    (Size: 4; Name: 'uuml'; Value: #252; ValueUtf8: #$C3#$BC),
    (Size: 4; Name: 'Yuml'; Value: #159; ValueUtf8: #$C5#$B8),
    (Size: 4; Name: 'yuml'; Value: #255; ValueUtf8: #$C3#$BF),
    (Size: 5; Name: 'Acirc'; Value: #194; ValueUtf8: #$C3#$82),
    (Size: 5; Name: 'acirc'; Value: #226; ValueUtf8: #$C3#$A2),
    (Size: 5; Name: 'acute'; Value: #180; ValueUtf8: #$C2#$B4),
    (Size: 5; Name: 'AElig'; Value: #198; ValueUtf8: #$C3#$86),
    (Size: 5; Name: 'aelig'; Value: #230; ValueUtf8: #$C3#$A6),
    (Size: 5; Name: 'Aring'; Value: #197; ValueUtf8: #$C3#$85),
    (Size: 5; Name: 'aring'; Value: #229; ValueUtf8: #$C3#$A5),
    (Size: 5; Name: 'cedil'; Value: #184; ValueUtf8: #$C2#$B8),
    (Size: 5; Name: 'Ecirc'; Value: #202; ValueUtf8: #$C3#$8A),
    (Size: 5; Name: 'ecirc'; Value: #234; ValueUtf8: #$C3#$AA),
    (Size: 5; Name: 'frasl'; Value: '/';  ValueUtf8: #$2F),
    (Size: 5; Name: 'Icirc'; Value: #206; ValueUtf8: #$C3#$8E),
    (Size: 5; Name: 'icirc'; Value: #238; ValueUtf8: #$C3#$AE),
    (Size: 5; Name: 'iexcl'; Value: #161; ValueUtf8: #$C2#$A1),
    (Size: 5; Name: 'laquo'; Value: #171; ValueUtf8: #$C2#$AB),
    (Size: 5; Name: 'ldquo'; Value: #147; ValueUtf8: #$E2#$80#$9C),
    (Size: 5; Name: 'lsquo'; Value: #145; ValueUtf8: #$E2#$80#$98),
    (Size: 5; Name: 'mdash'; Value: #151; ValueUtf8: #$E2#$80#$94),
    (Size: 5; Name: 'micro'; Value: #181; ValueUtf8: #$C2#$B5),
    (Size: 5; Name: 'minus'; Value: '-';  ValueUtf8: #$2D),
    (Size: 5; Name: 'ndash'; Value: #150; ValueUtf8: #$E2#$80#$93),
    (Size: 5; Name: 'Ocirc'; Value: #212; ValueUtf8: #$C3#$94),
    (Size: 5; Name: 'ocirc'; Value: #244; ValueUtf8: #$C3#$B4),
    (Size: 5; Name: 'OElig'; Value: #140; ValueUtf8: #$C5#$92),
    (Size: 5; Name: 'oelig'; Value: #156; ValueUtf8: #$C5#$93),
    (Size: 5; Name: 'pound'; Value: #163; ValueUtf8: #$C2#$A3),
    (Size: 5; Name: 'raquo'; Value: #187; ValueUtf8: #$C2#$BB),
    (Size: 5; Name: 'rdquo'; Value: #148; ValueUtf8: #$E2#$80#$9D),
    (Size: 5; Name: 'rsquo'; Value: #146; ValueUtf8: #$E2#$80#$99),
    (Size: 5; Name: 'szlig'; Value: #223; ValueUtf8: #$C3#$9F),
    (Size: 5; Name: 'THORN'; Value: #222; ValueUtf8: #$C3#$9E),
    (Size: 5; Name: 'thorn'; Value: #254; ValueUtf8: #$C3#$BE),
    (Size: 5; Name: 'tilde'; Value: '~';  ValueUtf8: #$7E),
    (Size: 5; Name: 'times'; Value: #215; ValueUtf8: #$C3#$97),
    (Size: 5; Name: 'trade'; Value: #153; ValueUtf8: #$E2#$84#$A2),
    (Size: 5; Name: 'Ucirc'; Value: #219; ValueUtf8: #$C3#$9B),
    (Size: 5; Name: 'ucirc'; Value: #251; ValueUtf8: #$C3#$BB),
    (Size: 6; Name: 'Aacute'; Value: #193; ValueUtf8: #$C3#$81),
    (Size: 6; Name: 'aacute'; Value: #225; ValueUtf8: #$C3#$A1),
    (Size: 6; Name: 'Agrave'; Value: #192; ValueUtf8: #$C3#$80),
    (Size: 6; Name: 'agrave'; Value: #224; ValueUtf8: #$C3#$A0),
    (Size: 6; Name: 'Atilde'; Value: #195; ValueUtf8: #$C3#$83),
    (Size: 6; Name: 'atilde'; Value: #227; ValueUtf8: #$C3#$A3),
    (Size: 6; Name: 'brvbar'; Value: #166; ValueUtf8: #$C2#$A6),
    (Size: 6; Name: 'Ccedil'; Value: #199; ValueUtf8: #$C3#$87),
    (Size: 6; Name: 'ccedil'; Value: #231; ValueUtf8: #$C3#$A7),
    (Size: 6; Name: 'curren'; Value: #164; ValueUtf8: #$C2#$A4),
    (Size: 6; Name: 'dagger'; Value: #134; ValueUtf8: #$E2#$80#$A0),
    (Size: 6; Name: 'Dagger'; Value: #135; ValueUtf8: #$E2#$80#$A1),
    (Size: 6; Name: 'divide'; Value: #247; ValueUtf8: #$C3#$B7),
    (Size: 6; Name: 'Eacute'; Value: #201; ValueUtf8: #$C3#$89),
    (Size: 6; Name: 'eacute'; Value: #233; ValueUtf8: #$C3#$A9),
    (Size: 6; Name: 'Egrave'; Value: #200; ValueUtf8: #$C3#$88),
    (Size: 6; Name: 'egrave'; Value: #232; ValueUtf8: #$C3#$A8),
    (Size: 6; Name: 'frac12'; Value: #189; ValueUtf8: #$C2#$BD),
    (Size: 6; Name: 'frac14'; Value: #188; ValueUtf8: #$C2#$BC),
    (Size: 6; Name: 'frac34'; Value: #190; ValueUtf8: #$C2#$BE),
    (Size: 6; Name: 'hellip'; Value: #133; ValueUtf8: #$E2#$80#$A6),
    (Size: 6; Name: 'Iacute'; Value: #205; ValueUtf8: #$C3#$8D),
    (Size: 6; Name: 'iacute'; Value: #237; ValueUtf8: #$C3#$AD),
    (Size: 6; Name: 'Igrave'; Value: #204; ValueUtf8: #$C3#$8C),
    (Size: 6; Name: 'igrave'; Value: #236; ValueUtf8: #$C3#$AC),
    (Size: 6; Name: 'iquest'; Value: #191; ValueUtf8: #$C2#$BF),
    (Size: 6; Name: 'lsaquo'; Value: #139; ValueUtf8: #$E2#$80#$B9),
    (Size: 6; Name: 'middot'; Value: #183; ValueUtf8: #$C2#$B7),
    (Size: 6; Name: 'Ntilde'; Value: #209; ValueUtf8: #$C3#$91),
    (Size: 6; Name: 'ntilde'; Value: #241; ValueUtf8: #$C3#$B1),
    (Size: 6; Name: 'Oacute'; Value: #211; ValueUtf8: #$C3#$93),
    (Size: 6; Name: 'oacute'; Value: #243; ValueUtf8: #$C3#$B3),
    (Size: 6; Name: 'Ograve'; Value: #210; ValueUtf8: #$C3#$92),
    (Size: 6; Name: 'ograve'; Value: #242; ValueUtf8: #$C3#$B2),
    (Size: 6; Name: 'Oslash'; Value: #216; ValueUtf8: #$C3#$98),
    (Size: 6; Name: 'oslash'; Value: #248; ValueUtf8: #$C3#$B8),
    (Size: 6; Name: 'Otilde'; Value: #213; ValueUtf8: #$C3#$95),
    (Size: 6; Name: 'otilde'; Value: #245; ValueUtf8: #$C3#$B5),
    (Size: 6; Name: 'permil'; Value: #137; ValueUtf8: #$E2#$80#$B0),
    (Size: 6; Name: 'plusmn'; Value: #177; ValueUtf8: #$C2#$B1),
    (Size: 6; Name: 'rsaquo'; Value: #155; ValueUtf8: #$E2#$80#$BA),
    (Size: 6; Name: 'Scaron'; Value: #138; ValueUtf8: #$C5#$A0),
    (Size: 6; Name: 'scaron'; Value: #154; ValueUtf8: #$C5#$A1),
    (Size: 6; Name: 'Uacute'; Value: #218; ValueUtf8: #$C3#$9A),
    (Size: 6; Name: 'uacute'; Value: #250; ValueUtf8: #$C3#$BA),
    (Size: 6; Name: 'Ugrave'; Value: #217; ValueUtf8: #$C3#$99),
    (Size: 6; Name: 'ugrave'; Value: #249; ValueUtf8: #$C3#$B9),
    (Size: 6; Name: 'Yacute'; Value: #221; ValueUtf8: #$C3#$9D),
    (Size: 6; Name: 'yacute'; Value: #253; ValueUtf8: #$C3#$BD),
    (Size: 6; Name: 'xxxxxx'; Value: NAnchorChar; ValueUtf8: NAnchorChar)
  );
  
function ParseConstant(const S: string; OnUtf8: boolean = false): string;
var
  Error: Integer;
  Index1: Integer;
  Index2: Integer;
  Size1: Integer;
  Found: Boolean;
begin {'Complete boolean eval' must be off}
  Result := ' ';
  Size1 := Length(S);
  if Size1 = 0 then Exit;
  if (S[1] in ['$', '0'..'9']) then
  begin
    Val(S, Index1, Error);
    if (Error = 0) then
    begin
      if not OnUTF8 and (Index1 >= 32) and (Index1 <= 255) then
        Result := Chr(Index1)
      else
      begin
        Result := UnicodeToUTF8(Index1);
        if Result = NbspUTF8 then Result := NbspChar;
      end;
    end;
  end else
  begin
    Index1 := 0;
    repeat
      if Size1 = Codes[Index1].Size then
      begin
        Found := True;
        Index2 := 1;
        while Index2 <= Size1 do
        begin
          if S[Index2] <> Codes[Index1].Name[Index2] then
          begin
            Found := False;
            Break;
          end;
          Inc(Index2);
        end;
        if Found then
        begin
          if OnUtf8 then 
            Result := Codes[Index1].ValueUTF8
          else 
            Result := Codes[Index1].Value;
          Break;
        end;
      end;
      Inc(Index1);
    until (Index1 >= CodeCount) or (Codes[Index1].Size > Size1);
  end;
end;

{- returns the string with & escapes expanded}
procedure ExpandEscapes(var S: string);
var
  i, j : Integer;
  Co : string;
  Ch : AnsiChar;
  St : string;
begin
  i := Length(S);
  while i > 0 do begin
    if S[i] = '&' then begin
      j := i;
      while (j < length(S)) and not (S[j] in [';', ' ']) do
        Inc(j);
      Co := copy(S, i + 1, j - i - 1);
      if Co <> '' then begin
        if Co[1] = '#' then begin
          Delete(Co, 1, 1);
          if UpCase(Co[1]) = 'X' then begin
            Delete(Co, 1, 1);
            Insert('$', Co, 1);
          end;
        end;
        Delete(S, i, j - i + 1);
        if SystemCharSetIsUTF8 then begin
          St := ParseConstant(Co, true);
          Insert(St, S, i)
        end else begin
          Ch := ParseConstant(Co)[1];
          Insert(Ch, S, i);
        end;
      end;
    end;
    Dec(i);
  end;
end;

function EscapeToAnsi(const S: string): string;
var
  P : Integer;
begin
  Result := S;
  P := CharPos('&', S);
  if P <> 0 then
    ExpandEscapes(Result);
end;

{ Returns the string with & escapes}
function AnsiToEscape(const S: string): string;
var
  i : Integer;
  procedure replaceCharBy(newStr: string);
  begin
    Result[i] := '&';
    Insert(newStr, Result, i + 1);
  end;

begin
  Result := S;
  i := length(Result);
  while i > 0 do begin
    case Result[i] of
    ShyChar : replaceCharBy('shy;');
    NbspChar : replaceCharBy('nbsp;');
    '"' : replaceCharBy('quot;');
    '&' : replaceCharBy('amp;');
    '<' : replaceCharBy('lt;');
    '>' : replaceCharBy('gt;');
    end;
    Dec(i);
  end;
end;

function NoBreakToSpace(const S: string): string;
var
  P, n : Integer;
begin
  SetLength(Result, Length(S));
  n := 0;
  P := 1;
  while P <= Length(S) do
  begin
    inc(n);
    if S[P] = NbspChar then
      Result[n] := ' '
    else if (P < Length(S)) and (S[P] = NbspUtf8[1]) and (S[P+1] = NbspUtf8[2]) then
    begin
      Result[n] := ' ';
      inc(P);
    end else
      Result[n] := S[P];
    inc(P);
  end;
  SetLength(Result, n);
end;

function FindFontName(const AFontList: string): string;

  function CheckFonts(ATestFontList: array of String): String;
  var
    i: Integer;
  begin
    for i:=0 to High(ATestFontList) do begin
      Result := ATestFontList[i];
      if Screen.Fonts.IndexOf(Result) > -1 then
        exit;
    end;
    Result := '';
  end;

var
  L: TStringList;
  i: Integer;
begin
  L := TStringList.Create;
  try
    L.CommaText := AFontList;
    for i:=0 to L.Count-1 do begin
      Result := L[i];
      if Screen.Fonts.IndexOf(Result) > -1 then
        exit;
      if SameText(Result, 'sans-serif') then begin
        Result := Checkfonts(['Arial', 'Helvetica', 'Liberation Sans']);
        if Result = '' then
          Result := Screen.MenuFont.Name;
        exit;
      end else
      if SameText(Result, 'serif') then begin
        Result := CheckFonts(['Times', 'Times New Roman', 'Liberation Serif']);
        if Result = '' then
          Result := Screen.MenuFont.Name;
        exit;
      end else
      if SameText(Result, 'monospace') then begin
        Result := CheckFonts(['Courier New', 'Courier', 'Liberation Mono']);
        if Result = '' then
          Result := Screen.MenuFont.Name;
        exit;
      end else
        Result := Screen.MenuFont.Name;
    end;
  finally
    L.Free;
  end;
end;

function MaxI2(const I1, I2: Integer) : Integer;
begin
  Result := I1;
  if I2 > I1 then
    Result := I2;
end;

function MinI2(const I1, I2: Integer) : Integer;
begin
  Result := I1;
  if I2 < I1 then
    Result := I2;
end;


end.

