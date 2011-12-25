{$IFDEF CSS_INTERFACE}
 TCSSGroup = class
 end;

 TCSSFontStyle = (cfsNormal, cfsItalic, cfsOblique, cfsInherit);
 TCSSFontWeight = (cfwNormal, cfwBold, cfwBolder, cfwLighter, cfw100, cfw200,
                    cfw300, cfw400 , cfw500, cfw600, cfw700, cfw800, cfw900);
 TCSSFontVariant = (cfvNormal, cfvSmallCaps, cfvInherit);

 TCSSBorderStyle = (cbsNone, cbsHidden, cbsDotted, cbsDashed, cbsSolid, cbsDouble,
                   cbsGroove, cbsRidge, cbsInset, cbsOutset);
 TCSSMarginStyle = (cmsNone,
                    cmsAuto, // use default
                    cmsPx,  // pixel
                    cmsPt, cmsEm, cmsPercent // currently not supported
                    );

 TCSSMargin = record
   Style: TCSSMarginStyle;
   Size: integer; // negative values are allowed (not implemented)
 end;


 { TCSSFont }

 TCSSFont = class
 private
   FFamily: String;
   FSize: String;
   FStyle: TCSSFontStyle;
   FWeight: TCSSFontWeight;
 published
   property Name: String read FFamily write FFamily;
   property Size: String read FSize write FSize;
   property Style: TCSSFontStyle read FStyle write FStyle;
   //proprety Variant: TCSSFontVariant
   property Weight: TCSSFontWeight read FWeight write FWeight;
 end;

 { TCSSBorder }

 TCSSBorder = class
 private
   FColor: TColor;
   FStyle: TCSSBorderStyle;
   FWidth: Integer;
 public
   constructor Create;
 published
   property Color: TColor read FColor write FColor;
   property Style: TCSSBorderStyle read FStyle write FStyle;
   property Width: Integer read FWidth write FWidth;
 end;

 { TCSSProps }

 TCSSProps = class
 private
   FBorder: TCSSBorder;
   FClassIDs: TStringList;
   FBGColor: TColor;
   FBorderBottom: TCSSBorderStyle;
   FBorderLeft: TCSSBorderStyle;
   FBorderRight: TCSSBorderStyle;
   FBorderTop: TCSSBorderStyle;
   FColor: TColor;
   FFont: TCSSFont;
   FAlignment: TIpHtmlAlign;
   FMarginBottom: TCSSMargin;
   FMarginLeft: TCSSMargin;
   FMarginRight: TCSSMargin;
   FMarginTop: TCSSMargin;
   function GetCommandArgs(ACommand: String): TStringList;
   function GetCommandName(ACommand: String): String;
 public
   property MarginTop: TCSSMargin read FMarginTop write FMarginTop;
   property MarginLeft: TCSSMargin read FMarginLeft write FMarginLeft;
   property MarginBottom: TCSSMargin read FMarginBottom write FMarginBottom;
   property MarginRight: TCSSMargin read FMarginRight write FMarginRight;
 published
   property Font: TCSSFont read  FFont write FFont;
   property Color: TColor read FColor write FColor;
   property BGColor: TColor read FBGColor write FBGColor;
   property Border: TCSSBorder read FBorder write FBorder;
   property BorderTop: TCSSBorderStyle read FBorderTop write FBorderTop;
   property BorderLeft: TCSSBorderStyle read FBorderLeft write FBorderLeft;
   property BorderBottom: TCSSBorderStyle read FBorderBottom write FBorderBottom;
   property BorderRight: TCSSBorderStyle read FBorderRight write FBorderRight;
   property Alignment: TIpHtmlAlign read FAlignment write FAlignment;
 public
   constructor Create;
   destructor Destroy; override;
   procedure ReadCommands(ACommands: TStrings);
 end;

 { TCSSGlobalProps }

 TCSSGlobalProps = class
   FElements: TFPObjectHashTable;
 public
   constructor Create;
   destructor Destroy; override;
   {$IFDEF IP_LAZARUS_DBG}
   procedure DumpProps;
   {$ENDIF}
   function GetElement(AElementID: String; ClassID: String = ''; CreateIfNotExist: Boolean = False): TCSSProps;
 end;

{$ELSE implementation}
type

  { TCSSReader }

  TCSSReader = class
    FStream: TStream;
    FGlobalProps: TCSSGlobalProps;
    function GetStatementElements(AStatement: String): TStringList;
    function GetStatementCommands(AStatement: String): TStringList;
    function CheckIsComment: Boolean;
    procedure EatWhiteSpace;
    procedure ParseCSS;
    procedure EatComment;
    function FindStatement(out AStatement: String): Boolean;
    function EOF: Boolean;
    constructor Create(AStream: TStream; AGlobalProps: TCSSGlobalProps);
  end;

function IsWhiteSpace(AChar: Char; ExcludeSpaces: Boolean = False): Boolean;
begin
  Result := AChar in [#9, #10, #11, #13];
  if not Result and not ExcludeSpaces then
    Result := AChar = ' ';
end;


function SeperateCommands(Commands: String): TStringList;
var
 i, fpos1, fpos2: Integer;
 Command: String;
begin
  Result := TStringList.Create;
  FPos1 := 1;

  for i := 1 to Length(Commands) do
  begin
    if Commands[i] = ';' then
    begin
      Command := Copy(Commands, FPos1, i-FPos1);
      FPos1 := i+1;
      for FPos2 := Length(Command) downto 1 do
        if IsWhiteSpace(Command[FPos2], True) then
          Delete(Command, FPos2, 1);
      Result.Add(Trim(Command));
    end;
  end;
  Command := Trim(Copy(Commands, FPos1, Length(Commands)));
  if Command <> '' then
  begin
    Result.Add(Command);
  end;

end;

function FontWeightFromString(S: String): TCSSFontWeight;
begin
  Result := cfwNormal;
  S := trim(LowerCase(S));
  case S[1] of
    '1': if S = '100' then Result := cfw100;
    '2': if S = '200' then Result := cfw200;
    '3': if S = '300' then Result := cfw300;
    '4': if S = '400' then Result := cfw400;
    '5': if S = '500' then Result := cfw500;
    '6': if S = '600' then Result := cfw600;
    '7': if S = '700' then Result := cfw700;
    '8': if S = '800' then Result := cfw800;
    '9': if S = '900' then Result := cfw900;
    'b': if S = 'bold' then Result := cfwBold
         else if S = 'bolder' then Result := cfwBolder;
    'l': if S = 'lighter' then Result := cfwLighter;
  end;
end;

const
  htmlNamedColors : array[0..140] of record
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

function BinSearchNamedColor(const sColor: string; var color: TColor): boolean;
var
  First: Integer;
  Last: Integer;
  Pivot: Integer;
begin
  First  := Low(htmlNamedColors); //Sets the first item of the range
  Last   := High(htmlNamedColors); //Sets the last item of the range
  Result  := False; //Initializes the Found flag (Not found yet)

  //If First > Last then the searched item doesn't exist
  //If the item is found the loop will stop
  while (First <= Last) {and (not Result)} do
  begin
    //Gets the middle of the selected range
    Pivot := (First + Last) div 2;
    //Compares the String in the middle with the searched one
    if htmlNamedColors[Pivot].s = sColor then
    begin
      Result  := True;
      color := htmlNamedColors[Pivot].c;
      exit;
    end
    //If the Item in the middle has a bigger value than
    //the searched item, then select the first half
    else if htmlNamedColors[Pivot].s > sColor then
      Last := Pivot - 1
        //else select the second half
    else
      First := Pivot + 1;
  end;
end;

function ColorFromString(S: String): TColor;
var
  R, G, B, Err : Integer;
begin
  Result := -1;
  if S = '' then
    Exit;
  S := UpperCase(S);
  if S[1] = '#' then
    if length(S) <> 7 then
      exit
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
      Exit(RGB(R, G, B));
    end
  else
  if BinSearchNamedColor(S, result) then exit
  else
  if length(S) = 6 then
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
      Result := RGB(R, G, B);
    except
        Result := -1;
    end
  //else WriteLn('>>>>> Unknwn Color! = ', S);

end;

function SizePxFromString(S: String): Integer;
begin
  S := Copy(S, 1, Pos('PX', UpperCase(S)) - 1);
  Result := StrToIntDef(S, 0);
end;

function StrToCssMargin(S: string): TCSSMargin;
var
  i: SizeInt;
begin
  S:=lowercase(S);
  Result.Style:=cmsAuto;
  Result.Size:=0;
  if (S='') or (S='auto') then exit;

  i:=Pos('px',S);
  if i>0 then begin
    Result.Style:=cmsPx;
    Result.Size:=StrToIntDef(copy(S,1,i-1),0);
    exit;
  end;

  i:=Pos('em',S);
  if i>0 then begin
    Result.Style:=cmsEm;
    Result.Size:=StrToIntDef(copy(S,1,i-1),0);
    exit;
  end;

  i:=Pos('%',S);
  if i>0 then begin
    Result.Style:=cmsPercent;
    Result.Size:=StrToIntDef(copy(S,1,i-1),0);
    exit;
  end;

  // a number without unit is px
  Result.Style:=cmsPx;
  Result.Size:=StrToIntDef(S,0);
end;

function CSSFontStyleFromName(S: String): TCSSFontStyle;
begin
  Result := cfsNormal;
  if length(s)<2 then exit;
  case S[2] of
    'b': if S = 'oblique' then Result := cfsOblique;
    'n': if S = 'inherit' then Result := cfsInherit;
    't': if S = 'italic' then Result := cfsItalic;
  end;
end;

function BorderStyleFromString(S: String): TCSSBorderStyle;
begin
  Result := cbsNone;
  S := LowerCase(S);
  case S[1] of
    'd':
      if S = 'dotted' then
        Result := cbsDotted
      else
      if S = 'dashed' then
        Result := cbsDashed
      else
      if S = 'double' then
        Result := cbsDouble;
    'g': if S = 'groove' then Result := cbsGroove;
    'h': if S = 'hidden' then Result := cbsHidden;
    'i': if S = 'inset' then Result := cbsInset;
    'o': if S = 'outset' then Result := cbsOutset;
    'r': if S = 'ridge' then Result := cbsRidge;
    's': if S = 'solid' then Result := cbsSolid;
  end;
end;


{ TCSSReader }

function TCSSReader.GetStatementElements(AStatement: String): TStringList;
var
 i, fpos: Integer;
 Elements : String;
 Element: String;
 ElementClass: String;
begin
  Result := TStringList.Create;
  fpos := Pos('{', AStatement);
  if fpos > 0 then
  begin
    Elements := Copy(AStatement,1,fpos-1);
    for i := Length(Elements) downto 1 do
      if IsWhiteSpace(Elements[i]) then
        Delete(Elements, i, 1);
     Result.Delimiter := ',';
     Result.DelimitedText := Elements;
  end;

  for i := 0 to Result.Count-1 do begin
    Element := LowerCase(Result[i]);
    ElementClass := '';
    fpos := Pos('.', Element);
    if fpos = 0 then
    begin
      Result.Objects[i] := FGlobalProps.GetElement(Element, '', True);
    end
    else begin
      ElementClass := LowerCase(Copy(Element, FPos+1, Length(Element)));
      Element := LowerCase(Copy(Element, 1, FPos-1));
      Result.Objects[i] := FGlobalProps.GetElement(Element, ElementClass, True);
    end;
  end;

end;

function TCSSReader.GetStatementCommands(AStatement: String): TStringList;
var
 fpos1, fpos2: Integer;
 Commands: String;
begin
  fpos1 := Pos('{', AStatement)+1;
  fpos2 := Pos('}', AStatement)-1;
  Commands := Copy(AStatement, fpos1, fpos2-fpos1+1);
  Result := SeperateCommands(Commands);
end;

function TCSSProps.GetCommandArgs(ACommand: String): TStringList;
var
  i: Integer;
  WantArg: Boolean;
  Arg: String;
  Start: Integer;
  Quote: char;
  WantChar: Boolean;
  Len: Integer;
begin
  Result := TStringList.Create;
  Start := Pos(':', ACommand)+1;


  WantArg := True;
  Quote := #0;
  for i := Start to Length(ACommand) do
  begin
    if (Quote = #0) and (ACommand[i] in ['"','''']) then
    begin
      Quote := ACommand[i];
      Start := i+1;
      continue;
    end;
    if Quote<>#0 then begin
      if ACommand[i]=Quote then begin
        Quote:=#0;
        Arg := Copy(ACommand, Start, i-Start);
        Result.Add(Arg);
      end;
      continue;
    end;
    if WantArg then
    begin
      if IsWhiteSpace(ACommand[i]) then
        Continue;
      Start := i;
      WantArg := False;
      continue;
    end
    else // we have an arg we are reading ...
    begin
      WantChar:=not (ACommand[i] in [';',' ',#9]);
      if (i<Length(ACommand)) and WantChar
      then
        continue;
      WantArg := True;
      Len:=i-Start;
      if WantChar then inc(Len);
      Arg := Copy(ACommand, Start, Len);
      Result.Add(Arg);
    end;
  end;
end;

function TCSSProps.GetCommandName(ACommand: String): String;
begin
  Result := Copy(ACommand, 1, Pos(':', ACommand)-1);
end;

function TCSSReader.CheckIsComment: Boolean;
begin
  Result := False;
  if EOF then
    exit;
  Result := char(FStream.ReadByte) = '*';
  if not Result then
    FStream.Position := FStream.Position-1;
end;

procedure TCSSReader.EatWhiteSpace;
var
  Buf: char;
  //comment: integer;
begin
  //comment:=0;
  while not EOF do
  begin
    Buf := char(FStream.ReadByte);
    if (Buf = '/') and not EOF then
    begin
      if CheckIsComment then
      begin
        EatComment;
        Buf := ' ';
      end;
    end;
    if (IsWhiteSpace(Buf) = False) then
    begin
      FStream.Position := FStream.Position-1;
      break;
    end;
  end;
end;

procedure TCSSReader.ParseCSS;
var
  Statement: String;
  Elements: TStringList;
  Commands: TStringList;
  Element: TCSSProps;
  I: Integer;
begin
  while FindStatement(Statement) do begin
   Elements := GetStatementElements(Statement);
   Commands := GetStatementCommands(Statement);
   for I := 0 to Elements.Count-1 do
   begin
     Element := TCSSProps(Elements.Objects[I]);
     Element.ReadCommands(Commands);
   end;
   Elements.Free;
   Commands.Free;
  end;
end;

procedure TCSSReader.EatComment;
var
  Buf: array[0..1] of char;
begin
  Buf := #0#0;
  while (EOF = False) and (Buf <> '*/') do
  begin
    Buf[0] := Buf[1];
    FStream.Read(Buf[1], 1);
  end;
end;

function TCSSReader.FindStatement(out AStatement: String): Boolean;
var
 Buf: char;
 Buf1: array[0..1023] of char;
 RCount: Integer;
 FStart, FEnd: Integer;
begin
  Result := False;
  EatWhiteSpace;

  AStatement := '';

  FStart := FStream.Position;
  while not EOF do
  begin

    Buf := Char(FStream.ReadByte);
    FEnd := FStream.Position;
    if (Buf = '/') and CheckIsComment then
    begin
      FStream.Position := FStart;
      RCount := FStream.Read(Buf1[0], FEnd-FStart-1);
      AStatement := AStatement + Copy(Buf1,0,RCount);
      FStream.Position := FEnd+1;
      EatComment;
      FStart := FStream.Position;
    end
    else if Buf = '}' then
    begin
      Result := True;
      FStream.Position := FStart;
      RCount := FStream.Read(Buf1[0], FEnd-FStart);
      AStatement := AStatement + Copy(Buf1,0,RCount);
      break;
    end;
  end;
end;

function TCSSReader.EOF: Boolean;
begin
  Result := FStream.Position >= FStream.Size;
end;

constructor TCSSReader.Create(AStream: TStream; AGlobalProps: TCSSGlobalProps);
begin
  inherited Create;
  FStream := AStream;
  FGlobalProps:= AGlobalProps;
end;

{ TCSSProps }

constructor TCSSProps.Create;
begin
  FFont := TCSSFont.Create;
  FBGColor := -1;
  FColor := -1;
  FAlignment := haUnknown;
  FBorder := TCSSBorder.Create;
end;

destructor TCSSProps.Destroy;
var
  i: Integer;
begin
  if Assigned(FClassIDs) then
  begin
    for i := 0 to FClassIDs.Count-1 do
      FClassIDs.Objects[i].Free;
    FClassIDs.Free;
  end;
  FFont.Free;
  FBorder.Free;
  inherited Destroy;
end;

procedure TCSSProps.ReadCommands(ACommands: TStrings);
var
  Args: TStringlist;
  ACommand: String;
  Command: String;
  I: Integer;
begin
  for I := 0 to ACommands.Count-1 do
  begin
    ACommand := ACommands[I];
    if ACommand='' then continue;
    Command := LowerCase(GetCommandName(ACommand));
    if Command='' then continue;

    Args := GetCommandArgs(ACommand);
    try
      case Command[1] of
      'c': if Command = 'color' then
             if Args.Count > 0 then
               Color := ColorFromString(Args[0])
             else
               Color := clDefault;

      'b': if Command = 'background-color' then
           begin
             if Args.Count > 0 then
               BGColor := ColorFromString(Args[0])
             else
               BGColor := clDefault
           end
           else if Command = 'background' then
           begin
             if Args.Count > 0 then BGColor := ColorFromString(Args[0]);
             if Args.Count > 1 then ; // background image
             if Args.Count > 2 then ; // background image repeat
             if Args.Count > 3 then ; // background attachment
             if Args.Count > 4 then ; // background position
           end
           else if Command = 'border' then
           begin
             if Args.Count > 0 then Border.Width := SizePxFromString(Args[0]);
             if Args.Count > 1 then Border.Style := BorderStyleFromString(Args[1]);
             if Args.Count > 2 then Border.Color := ColorFromString(Args[2]);
           end
           else if Command = 'border-width' then
           begin
             Border.Width := SizePxFromString(Args[0]);
           end
           else if Command = 'border-color' then
           begin
             Border.Color := ColorFromString(Args[0]);
           end
           else if Command = 'border-style' then
           begin
             Border.Style := BorderStyleFromString(Args[0]);
           end;

      'm':
        if Command = 'margin-top' then
        begin
          if Args.Count > 0 then MarginTop := StrToCssMargin(Args[0]);
        end else if Command = 'margin-left' then
        begin
          if Args.Count > 0 then MarginLeft := StrToCssMargin(Args[0]);
        end else if Command = 'margin-bottom' then
        begin
          if Args.Count > 0 then MarginBottom := StrToCssMargin(Args[0]);
        end else if Command = 'margin-right' then
        begin
          if Args.Count > 0 then MarginRight := StrToCssMargin(Args[0]);
        end else if Command = 'margin' then
        begin
          case Args.Count of
          1:begin
              // 1 arg: all four the same
              MarginTop := StrToCssMargin(Args[0]);
              MarginBottom := MarginTop;
              MarginLeft := MarginTop;
              MarginRight := MarginTop;
            end;
          2:begin
              // 2 args: top+bottom and left+right
              MarginTop := StrToCssMargin(Args[0]);
              MarginBottom := MarginTop;
              MarginLeft := StrToCssMargin(Args[1]);
              MarginRight := MarginLeft;
            end;
          3:begin
              // 3 args: top right bottom
              MarginTop := StrToCssMargin(Args[0]);
              MarginRight := StrToCssMargin(Args[1]);
              MarginBottom := StrToCssMargin(Args[2]);
            end;
          4:begin
              // 4 args: top right bottom left
              MarginTop := StrToCssMargin(Args[0]);
              MarginRight := StrToCssMargin(Args[1]);
              MarginBottom := StrToCssMargin(Args[2]);
              MarginLeft := StrToCssMargin(Args[3]);
            end;
          end;
        end;

      't': if (Command = 'text-align') and (Args.Count = 1) then
             if Args.Count > 0 then
               Alignment := GetAlignmentForStr(Args[0])
             else
               Alignment := GetAlignmentForStr('');


      'f':
          if length(Command) > 7 then
          begin
            case Command[7] of
              'a': if (Command = 'font-family')and (Args.Count > 0) then
                     Font.Name := Args[0];
              'i': if (Command = 'font-size') and (Args.Count > 0) then
                     Font.Size := Args[0];
              't': if (Command = 'font-style') and (Args.Count > 0) then
                     Font.Style := CSSFontStyleFromName(Args[0]);
              'e': if (Command = 'font-weight') and (Args.Count > 0) then
                     Font.Weight := FontWeightFromString(Args[0]);
            end;
          end;
      end;
    finally
      Args.Free;
    end;
  end;
end;

{ TCSSGlobalProps }

constructor TCSSGlobalProps.Create;
begin
  FElements := TFPObjectHashTable.Create(True);
end;

destructor TCSSGlobalProps.Destroy;
var
  i: Integer;
begin
  FElements.Free;
  inherited Destroy;
end;

function TCSSGlobalProps.GetElement(AElementID: String;
  ClassID: String; CreateIfNotExist: Boolean): TCSSProps;
var
  ElementName: String;

  procedure LookForElement(const aElement: string);
  begin
    if length(ClassID) > 0 then
      ElementName := aElement + '.' + ClassId
    else
      ElementName := aElement;

    // The element names are already lowercase, this is
    // already done in the css parser. And the html parser
    // can only deliver its own built-in node names anyways.
    // Also the names are not expected to be longer than
    // ShortString (this would need to be a ridiculously
    // long ClassID), should this ever happen then
    // it would be silently truncated in the following
    // type conversion to ShortString.
    {$warning GetElement() is called a *lot* of times, this
    is one of the hot spots, maybe some of the AnsiString
    usage around here could be avoided a little bit}
    Result := TCSSProps(FElements.Items[ElementName]);
  end;

begin
  Result := nil;
  if (length(ClassID) = 0) and (length(AElementID) = 0) then
    exit;

  LookForElement(aElementID);
  if (Result=nil) and not CreateIfNotExist then
    LookForelement('*');

  if (Result = nil) and CreateIfNotExist then
  begin
    Result := TCSSProps.Create;
    FElements.Add(ElementName, Result);
  end;
end;

{ TCSSBorder }

constructor TCSSBorder.Create;
begin
  inherited Create;
  FWidth := -1;
  FColor := clBlack;
  FStyle := cbsNone;
end;

{$IFDEF IP_LAZARUS_DBG}
procedure DumpTCSSProps(aProp: TCSSProps);
var
  str: string;
begin
    WriteLn('Color : ', aProp.Color);
    WriteLn('BgColor : ', aProp.BGColor);
    WriteLn('Font : ', aProp.Font.Name, ':', aProp.Font.FFamily, ':', aProp.Font.Size);
    case aProp.Alignment of
       haDefault: str:= 'haDefault';
       haLeft: str:= 'haLeft';
       haCenter: str:= 'haCenter';
       haRight: str:= 'haRight';
       haJustify: str:= 'haJustify';
       haChar: str:= 'haChar';
       else str:= 'haUnknow';
    end;
    WriteLn('Align : ', str);
end;

procedure TCSSGlobalProps.DumpProps;
var
   n: integer;
begin
   for n:=0 to FElements.Count-1 do
   begin
        WriteLn('CSS for >>>: ', FElements[n]);
        DumpTCSSProps(TCSSProps(FElements.Objects[n]));
   end;
end;
{$endif}
{$ENDIF}
