unit IpCSS;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils, Graphics,
  IpHtmlTypes, IpHtmlUtils;

type
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
  TCSSListType    = (ltNone, ltULCircle, ltULDisc, ltULSquare, 
                     ltOLDecimal, ltOLLowerAlpha, ltOLUpperAlpha, ltOLLowerRoman, ltOLUpperRoman
                    ); 

  TCSSMargin = record
    Style: TCSSMarginStyle;
    Size: single; // negative values are allowed (not implemented)
  end;

  TCSSLengthType =  (cltUndefined, cltAbsolute, cltPercent);
  TCSSLength = record
    LengthValue: Integer;
    LengthType: TCSSLengthType
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

  { TCSSProps represents a set of properties from the CSS stylesheet, for
    example everything within one selector or the contents of a style attribute
    or even many applicable CSS styles for one node merged into one. It has
    methods for parsing CSS text and for merging two such objects into one}
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
    FListType: TCSSListType;
    FMarginBottom: TCSSMargin;
    FMarginLeft: TCSSMargin;
    FMarginRight: TCSSMargin;
    FMarginTop: TCSSMargin;
    FWidth: TCSSLength;
    function GetCommandArgs(ACommand: String): TStringList;
    function GetCommandName(ACommand: String): String;
  public
    property MarginTop: TCSSMargin read FMarginTop write FMarginTop;
    property MarginLeft: TCSSMargin read FMarginLeft write FMarginLeft;
    property MarginBottom: TCSSMargin read FMarginBottom write FMarginBottom;
    property MarginRight: TCSSMargin read FMarginRight write FMarginRight;
    property Width: TCSSLength read FWidth write FWidth;
  published
    property Alignment: TIpHtmlAlign read FAlignment write FAlignment;
    property Font: TCSSFont read  FFont write FFont;
    property Color: TColor read FColor write FColor;
    property BGColor: TColor read FBGColor write FBGColor;
    property Border: TCSSBorder read FBorder write FBorder;
    property BorderTop: TCSSBorderStyle read FBorderTop write FBorderTop;
    property BorderLeft: TCSSBorderStyle read FBorderLeft write FBorderLeft;
    property BorderBottom: TCSSBorderStyle read FBorderBottom write FBorderBottom;
    property BorderRight: TCSSBorderStyle read FBorderRight write FBorderRight;
    property ListType: TCSSListType read FListType write FListType;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadCommands(ACommands: TStrings);
    procedure MergeAdditionalProps(AProps: TCSSProps);
  end;

  { TCSSGlobalProps serves as a global list of TCSSProps objects, it is
    populated when parsing the CSS and then used to look up the CSS styles
    for a certain CSS selector (the selector is supplied as a string and it
    returns a reference to the TCSSProps object for this selector). The
    contained TCSSProps objects are created and owned by TCSSGlobalProps }
  TCSSGlobalProps = class
    FElements: TFPObjectHashTable;
  protected
    {$IFDEF IP_LAZARUS_DBG}
    procedure DoDumpProps(Item: TObject; const Key: String; var Continue: Boolean);
    {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    {$IFDEF IP_LAZARUS_DBG}
    procedure DumpProps;
    {$ENDIF}
    function GetPropsObject(ATagName: String; AClassID: String = ''; CreateIfNotExist: Boolean = False): TCSSProps;
  end;
  
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
  
function SeparateCommands(Commands: String): TStringList;
  
implementation

uses
  LazStringUtils;

function ForceRange(x, xmin, xmax: Integer): Integer;
begin
  if x < xmin then
    Result := xmin
  else if x > xmax then
    Result := xmax
  else
    Result := x;
end;

function IsWhiteSpace(AChar: Char; ExcludeSpaces: Boolean = False): Boolean;
begin
  Result := AChar in [#9, #10, #11, #13];
  if not Result and not ExcludeSpaces then
    Result := AChar = ' ';
end;

function StrToCSSLength(AValue: String): TCssLength;
var
  P, Err: Integer;
begin
  P := Pos('%', AValue);
  if P <> 0 then begin
    Result.LengthType := cltPercent;
    Delete(AValue, P, 1);
  end else
    Result.LengthType := cltAbsolute;
  val(AValue, Result.LengthValue, Err);
  if (Err <> 0) or (Result.LengthValue < 0) then
    Result.LengthType := cltUndefined
  else if (Result.LengthType = cltPercent) and (Result.LengthValue > 100) then
    Result.LengthValue := 100;
end;

function SeparateCommands(Commands: String): TStringList;
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
  S := trim(S);
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
    'B','b': if CompareText(S, 'bold') = 0 then Result := cfwBold
        else if CompareText(S, 'bolder') = 0 then Result := cfwBolder;
    'L','l': if CompareText(S, 'lighter') = 0 then Result := cfwLighter;
  end;
end;

function SizePxFromString(S: String): Integer;
begin
  S := Copy(S, 1, PosI('px', S)-1);
  Result := StrToIntDef(S, 0);
end;

function StrToCssMargin(const S: string): TCSSMargin;
var
  i: SizeInt;
begin
  Result.Style:=cmsAuto;
  Result.Size:=0;
  if (S='') or (CompareText(S,'auto')=0) then exit;

  i:=PosI('px',S);
  if i>0 then begin
    Result.Style:=cmsPx;
    Result.Size:=StrToIntDef(copy(S,1,i-1),0);
    exit;
  end;

  i:=PosI('em',S);
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

function ListTypeFromString(S: String): TCSSListType;
begin
  Result := ltNone;
  if S <> '' then
  begin
    S := UpperCase(S);
    case S[1] of
      'C': if S = 'CIRCLE' then 
             Result := ltULCircle;
      'D': if S = 'DISC' then 
             Result := ltULDisc
           else if S = 'DECIMAL' then 
             Result := ltOLDecimal;
      'L': if S = 'LOWER-ALPHA' then
             Result := ltOLLowerAlpha
           else if S = 'LOWER-ROMAN' then
             Result := ltOLLowerRoman;
      'S': if S = 'SQUARE' then 
             Result := ltULSquare;
      'U': if S = 'UPPER-ALPHA' then 
             Result := ltOLUpperAlpha
           else if S = 'UPPER-ROMAN' then
             Result := ltOLUpperRoman;
    end;
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
    {$IFDEF CSS_CASESENSITIVE_CLASSID}
    Element := Result[i];
    {$ELSE}
    Element := LowerCase(Result[i]);
    {$ENDIF}
    ElementClass := '';
    fpos := Pos('.', Element);
    if fpos = 0 then
    begin
      Result.Objects[i] := FGlobalProps.GetPropsObject(Element, '', True);
    end
    else begin
      {$IFDEF CSS_CASESENSITIVE_CLASSID}
      ElementClass := Copy(Element, FPos+1, Length(Element));
      Element := Copy(Element, 1, FPos-1);
      {$ELSE}
      ElementClass := LowerCase(Copy(Element, FPos+1, Length(Element)));
      Element := LowerCase(Copy(Element, 1, FPos-1));
      {$ENDIF}
      Result.Objects[i] := FGlobalProps.GetPropsObject(Element, ElementClass, True);
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
  Result := SeparateCommands(Commands);
end;

function TCSSProps.GetCommandArgs(ACommand: String): TStringList;
var
  i: Integer;
  WantArg: Boolean;
  Arg: String;
  Start: Integer;
  Quote: char;
  WantChar: Boolean;
  WantPar: Boolean;
  Len: Integer;
begin
  Result := TStringList.Create;
  Start := Pos(':', ACommand)+1;

  WantArg := True;
  WantPar := false;
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
      if ACommand[i] = '(' then WantPar := True;
      if ACommand[i] = ')' then WantPar := False;
      if (i<Length(ACommand)) and (WantChar or WantPar)
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
   try
     for I := 0 to Elements.Count-1 do
     begin
       Element := TCSSProps(Elements.Objects[I]);
       Element.ReadCommands(Commands);
     end;
   finally
     Elements.Free;
     Commands.Free;
   end;
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
 Buf1: string;
 RCount: Integer;
 FStart, FEnd: Integer;
begin
  Result := False;
  EatWhiteSpace;

  AStatement := '';

  SetLength(Buf1,1023);
  FStart := FStream.Position;
  while not EOF do
  begin

    Buf := Char(FStream.ReadByte);
    FEnd := FStream.Position;
    if (Buf = '/') and CheckIsComment then
    begin
      FStream.Position := FStart;
      if length(Buf1)<FEnd-FStart then
        setlength(Buf1,FEnd-FStart);
      RCount := FStream.Read(Buf1[1], FEnd-FStart-1);
      AStatement := AStatement + Copy(Buf1,0,RCount);
      FStream.Position := FEnd+1;
      EatComment;
      FStart := FStream.Position;
    end
    else if Buf = '}' then
    begin
      Result := True;
      FStream.Position := FStart;
      if length(Buf1)<FEnd-FStart then
        setlength(Buf1,FEnd-FStart);
      RCount := FStream.Read(Buf1[1], FEnd-FStart);
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
  FBGColor := clNone;
  FColor := clNone;
  FAlignment := haUnknown;
  FBorder := TCSSBorder.Create;
  FWidth.LengthType := cltUndefined;
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
  Cmd: String;
  I: Integer;
begin
  for I := 0 to ACommands.Count-1 do
  begin
    ACommand := ACommands[I];
    if ACommand='' then continue;
    Cmd := LowerCase(GetCommandName(ACommand));
    if Cmd='' then continue;

    Args := GetCommandArgs(ACommand);
    try
      case Cmd[1] of
      'c': if Cmd = 'color' then
             if Args.Count > 0 then
               Color := ColorFromString(Args[0])
             else
               Color := clDefault;

      'b': if Cmd = 'background-color' then begin
             if Args.Count > 0 then
               BGColor := ColorFromString(Args[0])
             else
               BGColor := clDefault;
           end else
           if Cmd = 'background' then
           begin
             if Args.Count > 0 then BGColor := ColorFromString(Args[0]);
             if Args.Count > 1 then ; // background image
             if Args.Count > 2 then ; // background image repeat
             if Args.Count > 3 then ; // background attachment
             if Args.Count > 4 then ; // background position
           end
           else if Cmd = 'border' then
           begin
             if Args.Count > 0 then Border.Width := SizePxFromString(Args[0]);
             if Args.Count > 1 then Border.Style := BorderStyleFromString(Args[1]);
             if Args.Count > 2 then Border.Color := ColorFromString(Args[2]);
           end
           else if Cmd = 'border-width' then
           begin
             if Args.Count > 0 then Border.Width := SizePxFromString(Args[0]);
           end
           else if Cmd = 'border-color' then
           begin
             if Args.Count > 0 then Border.Color := ColorFromString(Args[0]);
           end
           else if Cmd = 'border-style' then
           begin
             if Args.Count > 0 then Border.Style := BorderStyleFromString(Args[0]);
           end;
           
      'l': if Cmd = 'list-style-type' then
             if Args.Count > 0 then FListType := ListTypeFromString(Args[0]);

      'm':
        if Cmd = 'margin-top' then begin
          if Args.Count > 0 then MarginTop := StrToCssMargin(Args[0]);
        end
        else if Cmd = 'margin-left' then begin
          if Args.Count > 0 then MarginLeft := StrToCssMargin(Args[0]);
        end
        else if Cmd = 'margin-bottom' then begin
          if Args.Count > 0 then MarginBottom := StrToCssMargin(Args[0]);
        end else if Cmd = 'margin-right' then begin
          if Args.Count > 0 then MarginRight := StrToCssMargin(Args[0]);
        end else if Cmd = 'margin' then begin
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

      't': if (Cmd = 'text-align') then
           begin
             if Args.Count > 0 then Alignment := GetAlignmentForStr(Args[0]);
           end;

      'f':
          if (Length(Cmd) > 7) and (Args.Count > 0) then
            case Cmd[7] of
              'a': if (Cmd = 'font-family') then
                     Font.Name := Args.CommaText; //Args[0];
              'i': if (Cmd = 'font-size') then
                     Font.Size := Args[0];
              't': if (Cmd = 'font-style') then
                     Font.Style := CSSFontStyleFromName(Args[0]);
              'e': if (Cmd = 'font-weight') then
                     Font.Weight := FontWeightFromString(Args[0]);
            end;
      'w':
        if (Cmd = 'width') and (Args.Count > 0) then
          FWidth := StrToCSSLength(Args[0]);
      end;
    finally
      Args.Free;
    end;
  end;
end;

procedure TCSSProps.MergeAdditionalProps(AProps: TCSSProps);
begin
  if AProps.Color <> clNone then Color := AProps.Color;
  if AProps.BGColor <> clNone then BGColor := AProps.BGColor;
  if AProps.Alignment <> haUnknown then Alignment := AProps.Alignment;
  if AProps.Font.Name <> '' then Font.Name := AProps.Font.Name;
  if AProps.Font.Size <> '' then Font.Size := AProps.Font.Size;
  if AProps.Font.Style <> cfsNormal then Font.Style := AProps.Font.Style;
  if AProps.Font.Weight <> cfwNormal then Font.Weight := AProps.Font.Weight;

  if AProps.MarginBottom.Style <> cmsNone then
    FMarginBottom.Style := AProps.MarginBottom.Style;
  if AProps.MarginBottom.Size <> 0 then
    FMarginBottom.Size := AProps.MarginBottom.Size;

  if AProps.MarginLeft.Style <> cmsNone then
    FMarginLeft.Style := AProps.MarginLeft.Style;
  if AProps.MarginLeft.Size <> 0 then
    FMarginLeft.Size := AProps.MarginLeft.Size;

  if AProps.MarginRight.Style <> cmsNone then
    FMarginRight.Style := AProps.MarginRight.Style;
  if AProps.MarginRight.Size <> 0 then
    FMarginRight.Size := AProps.MarginRight.Size;

  if AProps.MarginTop.Style <> cmsNone then
    FMarginTop.Style := AProps.MarginTop.Style;
  if AProps.MarginTop.Size <> 0 then
    FMarginTop.Size := AProps.MarginTop.Size;

  if AProps.ListType <> ltNone then
    FListType := AProps.ListType;
  
  if AProps.Width.LengthType <> cltUndefined then
    FWidth := AProps.Width;
end;


{ TCSSGlobalProps }

constructor TCSSGlobalProps.Create;
begin
  FElements := TFPObjectHashTable.Create(True);
end;

destructor TCSSGlobalProps.Destroy;
begin
  FElements.Free;
  inherited Destroy;
end;

{$IFDEF IP_LAZARUS_DBG}
procedure TCSSGlobalProps.DoDumpProps(Item: TObject; const Key: String; var Continue: Boolean);
var
  lProp: TCSSProps;
begin
  lProp := TCSSProps(Item);
  WriteLn('CSS for  >>>: ', Key);
  WriteLn('  Color : ', lProp.Color);
  WriteLn('  BgColor : ', lProp.BGColor);
  WriteLn('  Font : ', lProp.Font.Name, ':', lProp.Font.FFamily, ':', lProp.Font.Size);
  WriteLn('  Align : ' + GetEnumName(TypeInfo(TIpHtmlAlign), ord(lProp.Alignment)));
end;

procedure TCSSGlobalProps.DumpProps;
begin
  FElements.Iterate(DoDumpProps);
end;
{$endif}

function TCSSGlobalProps.GetPropsObject(ATagName: String;
  AClassID: String; CreateIfNotExist: Boolean): TCSSProps;
var
  Selector: String;

  procedure Lookup(const AName: String);
  begin
    if length(AClassID) > 0 then
      {$IFDEF CSS_CASESENSITIVE_CLASSID}
      Selector := AName + '.' + AClassID
      {$ELSE}
      Selector := AName + '.' + Lowercase(AClassID)
      {$ENDIF}
    else
      Selector := AName;

    // The css selectors are already lowercase, this is
    // already done in the css parser. And the html parser
    // can only deliver its own built-in tag names anyways.
    // Also the names are not expected to be longer than
    // ShortString (this would need to be a ridiculously
    // long ClassID), should this ever happen then
    // it would be silently truncated in the following
    // type conversion to ShortString.
    Result := TCSSProps(FElements.Items[Selector]);
  end;

begin
  Result := nil;
  if (length(AClassID) = 0) and (length(ATagName) = 0) then
    exit;

  Lookup(ATagName);
  if (Result=nil) and not CreateIfNotExist then
    Lookup('*');

  if (Result = nil) and CreateIfNotExist then
  begin
    Result := TCSSProps.Create;
    FElements.Add(Selector, Result);
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

end.

