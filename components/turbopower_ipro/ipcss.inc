{$IFDEF CSS_INTERFACE}
 TCSSGroup = class
 end;
 
 TCSSFontStyle = (cfsNormal, cfsItalic, cfsOblique, cfsInherit);
 TCSSFontWeight = (cfwNormal, cfwBold, cfwBolder, cfwLighter, cfw100, cfw200,
                    cfw300, cfw400 , cfw500, cfw600, cfw700, cfw800, cfw900);
 TCSSFontVariant = (cfvNormal, cfvSmallCaps, cfvInherit);
 
 TCSSBorderStyle = (cbsNone, cbsHidden, cbsDotted, cbsDashed, cbsSolid, cbsDouble,
                   cbsGroove, cbsRidge, cbsInset, cbsOutset);

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
 published
   property Color: TColor read FColor write FColor;
   property Style: TCSSBorderStyle read FStyle write FStyle;
 end;
 
 { TCSSProps }

 TCSSProps = class
 private
   FClassIDs: TStringList;
   FBGColor: TColor;
   FBorderBottom: TCSSBorderStyle;
   FBorderLeft: TCSSBorderStyle;
   FBorderRight: TCSSBorderStyle;
   FBorderTop: TCSSBorderStyle;
   FColor: TColor;
   FFont: TCSSFont;
   function GetCommandArgs(ACommand: String): TStringList;
   function GetCommandName(ACommand: String): String;
 published
   property Font: TCSSFont read  FFont write FFont;
   property Color: TColor read FColor write FColor;
   property BGColor: TColor read FBGColor write FBGColor;
   property BorderTop: TCSSBorderStyle read FBorderTop write FBorderTop;
   property BorderLeft: TCSSBorderStyle read FBorderLeft write FBorderLeft;
   property BorderBottom: TCSSBorderStyle read FBorderBottom write FBorderBottom;
   property BorderRight: TCSSBorderStyle read FBorderRight write FBorderRight;
 public
   constructor Create;
   destructor Destroy; override;
   procedure ReadCommands(ACommands: TStrings);
 end;
 
 { TCSSGlobalProps }

 TCSSGlobalProps = class
   FElements: TStringList;
 public
   constructor Create;
   destructor Destroy; override;
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
  S := LowerCase(S);
  if S = 'bold' then
    Result := cfwBold
  else if S = 'bolder' then
    Result := cfwBolder
  else if S = 'lighter' then
    Result := cfwLighter
  else if S = '100' then
    Result := cfw100
  else if S = '200' then
    Result := cfw200
  else if S = '300' then
    Result := cfw300
  else if S = '400' then
    Result := cfw400
  else if S = '500' then
    Result := cfw500
  else if S = '600' then
    Result := cfw600
  else if S = '700' then
    Result := cfw700
  else if S = '800' then
    Result := cfw800
  else if S = '900' then
    Result := cfw900

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
  if S = 'BLACK' then
    Result := clBlack
  else if S = 'STEELBLUE' then
    Result := $B48246
  else if S = 'ROYALBLUE' then
    Result := $901604
  else if S = 'CORNFLOWERBLUE' then
    Result := $ED9564
  else if S = 'LIGHTSTEELBLUE' then
    Result := $DEC4B0
  else if S = 'MEDIUMSLATEBLUE' then
    Result := $EE687B
  else if S = 'SLATEBLUE' then
    Result := $CD5A6A
  else if S = 'DARKSLATEBLUE'  then
    Result := $8B3D48
  else if S = 'MIDNIGHTBLUE' then
    Result := $701919
  else if S = 'NAVY' then
    Result := $800000
  else if S = 'DARKBLUE' then
    Result := $8B0000
  else if S = 'MEDIUMBLUE' then
    Result := $CD0000
  else if S = 'BLUE' then
    Result := $FF0000
  else if S = 'DODGERBLUE' then
    Result := $FF901E
  else if S = 'DEEPSKYBLUE' then
    Result := $FFBF00
  else if S = 'LIGHTSKYBLUE' then
    Result := $FACE87
  else if S = 'SKYBLUE' then
    Result := $EBCE87
  else if S = 'LIGHTBLUE' then
    Result := $E6D8AD
  else if S = 'POWDERBLUE' then
    Result := $E6E0B0
  else if S = 'AZURE' then
    Result := $FFFFF0
  else if S = 'LIGHTCYAN' then
    Result := $FFFFE0
  else if S = 'PALETURQUOISE' then
    Result := $EEEEAF
  else if S = 'MEDIUMTURQUOISE' then
    Result := $CCD148
  else if S = 'LIGHTSEAGREEN' then
    Result := $AAB220
  else if S = 'DARKCYAN' then
    Result := $8B8B00
  else if S = 'TEAL' then
    Result := $808000
  else if S = 'CADETBLUE' then
    Result := $A09E5F
  else if S = 'DARKTURQUOISE' then
    Result := $D1CE00
  else if S = 'AQUA' then
    Result :=  $FFFF00
  else if S = 'CYAN' then
    Result :=  $FFFF00
  else if S = 'TURQUOISE' then
    Result := $D0E040
  else if S = 'AQUAMARINE' then
    Result := $D4FF7F
  else if S = 'MEDIUMAQUAMARINE' then
    Result := $AACD66
  else if S = 'DARKSEAGREEN' then
    Result := $8FBC8F
  else if S = 'MEDIUMSEAGREEN' then
    Result := $71B33C
  else if S = 'SEAGREEN' then
    Result := $578B2E
  else if S = 'DARKGREEN' then
    Result := $006400
  else if S = 'GREEN' then
    Result := $008000
  else if S = 'FORESTGREEN' then
    Result := $228B22
  else if S = 'LIMEGREEN' then
    Result := $32CD32
  else if S = 'LIME' then
    Result := $00FF00
  else if S = 'CHARTREUSE' then
    Result := $00FF7F
  else if S = 'LAWNGREEN' then
    Result := $00FC7C
  else if S = 'GREENYELLOW' then
    Result := $2FFFAD
  else if S = 'YELLOWGREEN' then
    Result := $32CD9A
  else if S = 'PALEGREEN' then
    Result := $98FB98
  else if S = 'LIGHTGREEN' then
    Result := $90EE90
  else if S = 'SPRINGGREEN' then
    Result := $7FFF00
  else if S = 'MEDIUMSPRINGGREEN' then
    Result := $9AFA00
  else if S = 'DARKOLIVEGREEN' then
    Result := $2F6B55
  else if S = 'OLIVEDRAB' then
    Result := $238E6B
  else if S = 'OLIVE' then
    Result := $008080
  else if S = 'DARKYELLOW' then
    Result := $008080
  else if S = 'DARKKHAKI' then
    Result := $6BB7BD
  else if S = 'DARKGOLDENROD' then
    Result := $0B86B8
  else if S = 'GOLDENROD' then
    Result := $20A5DA
  else if S = 'GOLD' then
    Result := $00D7FF
  else if S = 'YELLOW' then
    Result := $00FFFF
  else if S = 'KHAKI' then
    Result := $8CE6F0
  else if S = 'PALEGOLDENROD' then
    Result := $AAE8EE
  else if S = 'BLANCHEDALMOND' then
    Result := $CDEBFF
  else if S = 'MOCCASIN' then
    Result := $B5E4FF
  else if S = 'WHEAT' then
    Result := $B3DEF5
  else if S = 'NAVAJOWHITE' then
    Result := $ADDEFF
  else if S = 'BURLYWOOD' then
    Result := $87B8DE
  else if S = 'TAN' then
    Result := $8CB4D2
  else if S = 'ROSYBROWN' then
    Result := $8F8FBC
  else if S = 'SIENNA' then
    Result := $2D52A0
  else if S = 'SADDLEBROWN' then
    Result := $13458B
  else if S = 'CHOCOLATE' then
    Result := $1E69D2
  else if S = 'PERU' then
    Result := $3F85CD
  else if S = 'SANDYBROWN' then
    Result := $60A4F4
  else if S = 'DARKRED' then
    Result := $00008B
  else if S = 'MAROON' then
    Result := $000080
  else if S = 'BROWN' then
    Result := $2A2AA5
  else if S = 'FIREBRICK' then
    Result := $2222B2
  else if S = 'INDIANRED' then
    Result := $5C5CCD
  else if S = 'LIGHTCORAL' then
    Result := $8080F0
  else if S = 'SALMON' then
    Result := $7280FA
  else if S = 'DARKSALMON' then
    Result := $7A96E9
  else if S = 'LIGHTSALMON' then
    Result := $7AA0FF
  else if S = 'CORAL' then
    Result := $507FFF
  else if S = 'TOMATO' then
    Result := $4763FF
  else if S = 'DARKORANGE' then
    Result := $008CFF
  else if S = 'ORANGE' then
    Result := $00A5FF
  else if S = 'ORANGERED' then
    Result := $0045FF
  else if S = 'CRIMSON' then
    Result := $3C14DC
  else if S = 'RED' then
    Result := $0000FF
  else if S = 'DEEPPINK' then
    Result := $9314FF
  else if S = 'FUCHSIA' then
    Result := $FF00FF
  else if S = 'MAGENTA' then
    Result := $FF00FF
  else if S = 'HOTPINK' then
    Result := $B469FF
  else if S = 'LIGHTPINK' then
    Result := $C1B6FF
  else if S = 'PINK' then
    Result := $CBC0FF
  else if S = 'PALEVIOLETRED' then
    Result := $9370DB
  else if S = 'MEDIUMVIOLETRED' then
    Result := $8515C7
  else if S = 'PURPLE' then
    Result := $800080
  else if S = 'DARKMAGENTA' then
    Result := $8B008B
  else if S = 'MEDIUMPURPLE' then
    Result := $DB7093
  else if S = 'BLUEVIOLET' then
    Result := $E22B8A
  else if S = 'INDIGO' then
    Result := $82004B
  else if S = 'DARKVIOLET' then
    Result := $D30094
  else if S = 'DARKORCHID' then
    Result := $CC3299
  else if S = 'MEDIUMORCHID' then
    Result := $D355BA
  else if S = 'ORCHID' then
    Result := $D670DA
  else if S = 'VIOLET' then
    Result :=$EE82EE
  else if S = 'PLUM' then
    Result := $DDA0DD
  else if S = 'THISTLE' then
    Result := $D8BFD8
  else if S = 'LAVENDER' then
    Result := $FAE6E6
  else if S = 'GHOSTWHITE' then
    Result := $FFF8F8
  else if S = 'ALICEBLUE' then
    Result := $FFF8F0
  else if S = 'MINTCREAM' then
    Result := $FAFFF5
  else if S = 'HONEYDEW' then
    Result := $F0FFF0
  else if S = 'LIGHTGOLDENRODYELLOW' then
    Result := $D2FAFA
  else if S = 'LEMONCHIFFON' then
    Result := $CDFAFF
  else if S = 'CORNSILK' then
    Result := $DCF8FF
  else if S = 'LIGHTYELLOW' then
    Result := $E0FFFF
  else if S = 'IVORY' then
    Result := $F0FFFF
  else if S = 'FLORALWHITE' then
    Result := $F0FAFF
  else if S = 'LINEN' then
    Result := $E6F0FA
  else if S = 'OLDLACE' then
    Result := $E6F5FD
  else if S = 'ANTIQUEWHITE' then
    Result := $D7EBFA
  else if S = 'BISQUE' then
    Result := $C4E4FF
  else if S = 'PEACHPUFF' then
    Result := $B9DAFF
  else if S = 'PAPAYAWHIP' then
    Result := $D5EFFF
  else if S = 'BEIGE' then
    Result := $DCF5F5
  else if S = 'SEASHELL' then
    Result := $EEF5FF
  else if S = 'LAVENDERBLUSH' then
    Result := $F5F0FF
  else if S = 'MISTYROSE' then
    Result := $E1E4FF
  else if S = 'SNOW' then
    Result := $FAFAFF
  else if S = 'WHITE' then
    Result := $FFFFFF
  else if S = 'WHITESMOKE' then
    Result := $F5F5F5
  else if S = 'GAINSBORO' then
    Result := $DCDCDC
  else if S = 'LIGHTGREY' then
    Result := $D3D3D3
  else if S = 'SILVER' then
    Result := $C0C0C0
  else if S = 'DARKGRAY' then
    Result := $A9A9A9
  else if S = 'GRAY' then
    Result := $808080
  else if S = 'LIGHTSLATEGRAY' then
    Result := $998877
  else if S = 'SLATEGRAY' then
    Result := $908070
  else if S = 'DIMGRAY' then
    Result := $696969
  else if S = 'DARKSLATEGRAY' then
    Result := $4F4F2F
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

function CSSFontStyleFromName(S: String): TCSSFontStyle;
begin
  Result := cfsNormal;
  if S = 'italic' then
    Result := cfsItalic
  else if S = 'oblique' then
    Result := cfsOblique
  else if S = 'inherit' then
    Result := cfsInherit;
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
    for i := Length(Elements) downto 0 do
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
  Commands := Copy(AStatement, fpos1, fpos2-fpos1);
  Result := SeperateCommands(Commands);
end;

function TCSSProps.GetCommandArgs(ACommand: String): TStringList;
var
  i: Integer;
  WantQuote: Boolean;
  WantArg: Boolean;
  Arg: String;
  Start: Integer;
begin
  Result := TStringList.Create;
  Start := Pos(':', ACommand)+1;

  WantQuote := False;
  WantArg := True;
  for i := Start to Length(ACommand) do
  begin
    if (WantQuote = False) and (ACommand[i] = '"') then
    begin
      WantQuote := True;
      Start := i+1;
      continue;
    end;
    if WantQuote and (ACommand[i] = '"') then
    begin
      WantQuote := False;
      Arg := Copy(ACommand, Start, i-1);
      Result.Add(Arg);
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
      if (i <> Length(ACommand)) and (not IsWhiteSpace(ACommand[i])) then
        continue;
      WantArg := True;
      Arg := Copy(ACommand, Start, i-1);
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
begin
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
 Buf1: array[0..255] of char;
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
      RCount := FStream.Read(Buf1[0], FEnd-FStart);
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
  Result := FStream.Position >= FStream.Size-1;
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
  FClassIDs := TStringList.Create;
  FFont := TCSSFont.Create;
  FBGColor := -1;
  FColor := -1;
end;

destructor TCSSProps.Destroy;
var
  i: Integer;
begin
  for i := 0 to FClassIDs.Count-1 do
    FClassIDs.Objects[i].Free;
  FClassIDs.Free;
  FFont.Free;
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
    Args := GetCommandArgs(ACommand);
    Command := LowerCase(GetCommandName(ACommand));

    //WriteLn('Got Command: "', Command,'" With args:');
    //WriteLn(Args.Text);
  
    if Command = 'color' then
      Color := ColorFromString(Args[0])
    else if Command = 'background-color' then
      BGColor := ColorFromString(Args[0])
    else if Command = 'background' then
    begin
      if Args.Count > 0 then BGColor := ColorFromString(Args[0]);
      if Args.Count > 1 then ; // background image
      if Args.Count > 2 then ; // background image repeat
      if Args.Count > 3 then ; // background attachment
      if Args.Count > 4 then ; // background position
    end
    else if Command = 'font-family' then
      Font.Name := Args[0]
    else if Command = 'font-size' then
      Font.Size := Args[0]
    else if Command = 'font-style' then
      Font.Style := CSSFontStyleFromName(Args[0])
    else if Command = 'font-weight' then
      Font.Weight := FontWeightFromString(Args[0]);
  
    Args.Free;
  end;
end;

{ TCSSGlobalProps }

constructor TCSSGlobalProps.Create;
begin
  FElements := TStringList.Create;
end;

destructor TCSSGlobalProps.Destroy;
var
  i: Integer;
begin
  for i := 0 to FElements.Count-1 do
    FElements.Objects[i].Free;
  FElements.Free;
  inherited Destroy;
end;

function TCSSGlobalProps.GetElement(AElementID: String;
  ClassID: String; CreateIfNotExist: Boolean): TCSSProps;
var
  ElementIndex: Integer;
  ElementName: String;
begin
  Result := nil;
  if (ClassID = '') and (AElementID = '') then
    exit;
  if ClassID <> '' then
    ElementName := LowerCase(AElementID+'.'+ClassID)
  else
    ElementName := LowerCase(AElementID);
  ElementIndex := FElements.IndexOf(ElementName);
  if ElementIndex <> -1 then
  begin
    Result := TCSSProps(FElements.Objects[ElementIndex]);
  end;

  if (Result = nil) and CreateIfNotExist then
  begin
    Result := TCSSProps.Create;
    FElements.AddObject(ElementName, Result);
  end;
end;

{$ENDIF}
