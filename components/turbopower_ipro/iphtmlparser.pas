unit IpHtmlParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ipConst, ipUtils, ipHtmlTypes, ipHtmlUtils, ipCSS, ipHtmlClasses, ipHtml;

type  
  TIpHtmlParser = class(TIpHtmlBasicParser)
  private
    FCharSP: Integer;
    FCharStack: array [0..7] of AnsiChar;
    FCharStream: TStream;
    FCurFrameSet: TIpHtmlNodeFRAMESET;
    FCurToken: TIpHtmlToken;
    FCurURL: string;
    FDocCharSet: String;
    FGlobalPos: Integer;
    FHasBOM: Boolean;
    FHasFrames: Boolean;
    FHaveToken: Boolean;
    FInBlock: Integer;
    FIndexPhrase: string;
    FInPre: Integer;
    FLastWasClose: Boolean;
    FLastWasSpace: Boolean;
    FLineNumber: Integer;
    FLineOffset: Integer;
    FListLevel: Integer;
    FOwner: TIpHtml;
    FParmBuf: PChar;
    FParmBufSize: Integer;
    FParmValueArray: array[TIpHtmlAttributesSet] of string;
    FTitleNode : TIpHtmlNodeTITLE;
    FTokenBuffer: TIpHtmlToken;
    FTokenStringBuf: PChar; {array[16383] of AnsiChar;}
    TBW: Integer;
    
    function GetFlagErrors: Boolean;
    
    procedure ClearParmValueArray;
    procedure ParmValueArrayAdd(const sName, sValue: string);
    
    procedure ReportError(const AErrorMsg: string);
    procedure ReportExpectedError(const AErrorMsg: string);
    procedure ReportExpectedToken(const AToken: TIpHtmlToken);
    
  protected
    function ColorFromString(S: string): TColor;
    procedure EnsureClosure(const EndToken: TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
    function GetChar: AnsiChar;
    function GetTokenString: string;
    function HtmlTokenListIndexOf(const TokenString: PAnsiChar): integer;
    function IsWhiteSpace: Boolean;
    function NextChar: AnsiChar;
    procedure NextNonBlankToken;
    procedure NextRealToken;
    procedure NextToken;
    procedure PutChar(Ch: AnsiChar);    
    procedure PutToken(AToken: TIpHtmlToken);
    procedure SkipTextTokens;
    
  protected
    // parser helper routines
    function ParseAlignment: TIpHtmlAlign;
    function ParseBoolean(const AttrNameSet: TIpHtmlAttributesSet): Boolean;
    function ParseBRClear: TIpHtmlBreakClear;
    function ParseButtonType: TIpHtmlButtonType;
    function ParseCellAlign(ADefault: TIpHtmlAlign): TIpHtmlAlign;
    procedure ParseCenter(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    function ParseDir: TIpHtmlDirection;
    function ParseFrameProp(ADefault: TIpHtmlFrameProp): TIpHtmlFrameProp;
    function ParseFrameScrollingProp: TIpHtmlFrameScrolling;
    function ParseHtmlInteger2(const AttrNameSet: TIpHtmlAttributesSet; 
      ADefault: Integer): TIpHtmlInteger;
    function ParseHyperLength(const AttrNameSet: TIpHtmlAttributesSet;
      const ADefault: string): TIpHtmlLength;
    function ParseHyperMultiLength(const AttrNameSet: TIpHtmlAttributesSet;
      const ADefault: string): TIpHtmlMultiLength;
    function ParseHyperMultiLengthList(const AttrNameSet: TIpHtmlAttributesSet;
      const ADefault: string): TIpHtmlMultiLengthList;
    procedure ParseIFrame(AParent: TIpHtmlNode);
    function ParseImageAlignment(ADefault: TIpHtmlImageAlign): TIpHtmlImageAlign;
    procedure ParseImg(AParent: TIpHtmlNode);
    function ParseInputType: TIpHtmlInputType;
    function ParseInteger(const AttrNameSet: TIpHtmlAttributesSet; 
      ADefault: Integer): Integer;
    function ParseMethod: TIpHtmlFormMethod;
    function ParseObjectValueType: TIpHtmlObjectValueType;
    function ParseOLStyle(ADefault: TIpHtmlOLStyle): TIpHtmlOLStyle;
    function ParsePixels(const AttrNameSet: TIpHtmlAttributesSet;
      const ADefault: string): TIpHtmlPixels;
    function ParseRelSize: TIpHtmlRelSize;
    function ParseRules(ADefault: TIpHtmlRules): TIpHtmlRules;
    function ParseShape: TIpHtmlMapShape;
    function ParseULStyle(ADefault: TIpHtmlULType): TIpHtmlULType;
    function ParseVAlignment: TIpHtmlVAlign;
    function ParseVAlignment2: TIpHtmlVAlignment2;
    function ParseVAlignment3: TIpHtmlVAlign3;
    
  protected
    // Methods for parsing html nodes
    procedure ParseAddress(AParent: TIpHtmlNode);
    procedure ParseAnchor(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseApplet(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseBase;
    procedure ParseBaseFont(AParent: TIpHtmlNode);
    procedure ParseBlink(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseBlock(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseBlockQuote(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseBody(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseBodyText(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseBR(AParent: TIpHtmlNode);
    procedure ParseColGroup(AParent: TIpHtmlNode);
    procedure ParseDefinitionList(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseDefinitionListItems(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseDel(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseDIV(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseFont(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseFontStyle(AParent: TIpHtmlNode; StartToken: TIpHtmlToken; 
      const EndTokens: TIpHtmlTokenSet);
    procedure ParseForm(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseFormFields(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseFrame(AParent: TIpHtmlNode);
    procedure ParseFrameSet(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseHead(AParent: TIpHtmlNode);
    procedure ParseHeader(AParent: TIpHtmlNode; EndToken: TIpHtmlToken; ASize: Integer);
    procedure ParseHeadItems(AParent: TIpHtmlNode);
    procedure ParseHR(AParent: TIpHtmlNode);
    procedure ParseHtml;
    procedure ParseInline(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseIns(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseIsIndex;
    procedure ParseLeft(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseLink(AParent: TIpHtmlNode);
    procedure ParseListItems(AParent: TIpHtmlNodeCore; EndToken: TIpHtmlToken; 
      const EndTokens: TIpHtmlTokenSet);
    procedure ParseMap(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseMeta(AParent: TIpHtmlNode);
    procedure ParseNOBR(AParent: TIpHtmlNode);
    procedure ParseNoFrames(AParent: TIpHtmlNode);
    procedure ParseNoScript(AParent: TIpHtmlNode);
    procedure ParseObject(AParent: TIpHtmlNode);
    procedure ParseOrderedList(AParent: TIpHtmlNode; EndToken: TIpHtmlToken; 
      const EndTokens: TIpHtmlTokenSet);
    procedure ParseParagraph(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParsePhraseElement(AParent: TIpHtmlNode; StartToken, EndToken: TIpHtmlToken; 
      const EndTokens: TIpHtmlTokenSet);
    procedure ParsePre(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseQ(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseRight(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseScript(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseSpan(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseStyle(AParent: TIpHtmlNode);
    procedure ParseStyleSheet(AParent: TIpHtmlNode; HRef: String);
    procedure ParseTable(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseTableBody(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseTableRow(AParent: TIpHtmlNode; const EndTokens : TIpHtmlTokenSet);
    procedure ParseTableRows(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseText(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
    procedure ParseTitle(AParent: TIpHtmlNode);
    procedure ParseUnorderedList(AParent: TIpHtmlNode; EndToken: TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);

    property FlagErrors: Boolean read GetFlagErrors;
    
  public
    constructor Create(AOwner: TIpHtml; AStream: TStream);
    function Execute: Boolean; override;
    function FindAttribute(const AttrNameSet: TIpHtmlAttributesSet): string; override;
       
    property DocCharset: String read FDocCharset;
    property FrameSet: TIpHtmlNodeFRAMESET read FCurFrameSet;
    property HasFrames: Boolean read FHasFrames;
    property TitleNode: TIpHtmlNodeTITLE read FTitleNode;
  end;
  
implementation

uses
  LConvEncoding, LazUTF8, LazStringUtils, Translations,
  IpHtmlNodes;

{ TIpHtmlParser }

constructor TIpHtmlParser.Create(AOwner: TIpHtml; AStream: TStream);
begin
  inherited Create;
  FCharStream := AStream;
  FOwner := AOwner;
end;

procedure TIpHtmlParser.ClearParmValueArray;
var
  n: TIpHtmlAttributesSet;
begin
  for n := Low(FParmValueArray) to High(FParmValueArray) do
    FParmValueArray[n] := '';
//    SetLength(FParmValueArray[n], 0);
end;

function TIpHtmlParser.ColorFromString(S: String): TColor;
var
  err: String;
begin
  if TryColorFromString(S, Result, err) then
    //
  else
  begin
    ReportError(err);
    Result := clNone;
  end;
end;

procedure TIpHtmlParser.EnsureClosure(const EndToken: TIpHtmlToken;
  const EndTokens: TIpHtmlTokenSet);
begin
  if FCurToken = EndToken then
    NextToken
  else
  if FCurToken in EndTokens then
  else
    if FlagErrors then
      ReportExpectedToken(EndToken);
end;

function TIpHtmlParser.Execute: Boolean;
var
  ch1, ch2, ch3: AnsiChar;
  startPos: Int64;
begin
  Result := false;

  Getmem(FTokenStringBuf, FCharStream.Size * 4 + 65536);
  try
    FGlobalPos := 0;
    FLineNumber := 1;
    FLineOffset := 0;
    FCharSP := 0;
    FListLevel := 0;
    FDocCharset := '';
    FHasBOM := false;
    FHasFrames := false;
    startPos := FCharStream.Position;
    ch1 := GetChar;
    ch2 := GetChar;
    if (ch1 = #$FE) and (ch2 = #$FF) then begin
      FDocCharset := 'UCS-2BE';
      raise Exception.CreateFmt('%s document encoding not supported!', [FDocCharset]);
    end else
    if (ch1 = #$FF) and (ch2 = #$FE) then begin
      FDocCharset := 'UCS-2LE';
      raise Exception.CreateFmt('%s document encoding not supported!', [FDocCharset]);
    end else
    if (ch1 = #$EF) and (ch2=#$BB) then begin
      ch3 := GetChar;
      if ch3 = #$BF then begin
        FDocCharset := 'UTF-8';
        FHasBOM := true;
      end else begin
        PutChar(ch3);
        PutChar(ch2);
        PutChar(ch1);
      end;
    end else begin
      PutChar(ch2);
      PutChar(ch1);
    end;

    repeat
      NextToken;
    until FCurToken in [IpHtmlTagHtml, IpHtmlTagFRAMESET, IpHtmlTagEOF];
    
    if FCurToken = IpHtmlTagEOF then begin
      FCharStream.Position := startPos;
      FCharSP := 0;
      FListLevel := 0;
      repeat
        NextToken;
      until FCurToken <> IpHtmlTagText;
    end;
    if FCurToken = IpHtmlTagEOF then Exit;
    ParseHtml;
    Result := true;
  finally
    FreeMem(FTokenStringBuf);
    FTokenStringBuf := nil;
    if FParmBuf <> nil then begin
      FreeMem(FParmBuf);
      FParmBuf := nil;
      FParmBufSize := 0;
    end;
  end;
end;

function TIpHtmlParser.FindAttribute(const AttrNameSet: TIpHtmlAttributesSet): string;
begin
  Result := FParmValueArray[AttrNameSet];
end;

function TIpHtmlParser.GetChar: AnsiChar;
var
  trimming, done: Boolean;
begin
  trimming := False;
  repeat
    done := True;
    if (FCharSP > 0) then begin
      Dec(FCharSP);
      Result := FCharStack[FCharSP];
    end else begin
      Result := NextChar;
    end;
    if (FInPre = 0) and (FCurToken <> IpHtmlTagPRE) then begin
      if (Result <= ' ') and (Result > #0) then begin
        if (Result < ' ') and FLastWasClose then begin
          done := False;
          trimming := True;
        end else
          if trimming then
            done := False
          else
            if FLastWasSpace then
              Done := False
            else begin
              Result := ' ';
              FLastWasSpace := True;
            end;
      end else
        FLastWasSpace := False;
    end;
  until done;
  FLastWasClose := (Result = '>');
end;

function TIpHtmlParser.GetFlagErrors: Boolean;
begin
  Result := FOwner.FlagErrors;
end;

function TIpHtmlParser.GetTokenString: string;
begin
  FTokenStringBuf[TBW] := #0;
  Result := StrPas(FTokenStringBuf);
end;

function TIpHtmlParser.HtmlTokenListIndexOf(const TokenString: PAnsiChar): integer;
var
  vFirst: Integer;
  vLast: Integer;
  vPivot: Integer;
  vicmp: integer;
begin
  vFirst  := Low(IpHtmlTokens); //Sets the first item of the range
  vLast   := High(IpHtmlTokens); //Sets the last item of the range
  Result  := -1; //Initializes the Found flag (Not found yet)

  //If First > Last then the searched item doesn't exist
  //If the item is found the loop will stop
  while (vFirst <= vLast) do
  begin
    //Gets the middle of the selected range
    vPivot := (vFirst + vLast) div 2;
    //Compares the String in the middle with the searched one
    vicmp := strcomp(IpHtmlTokens[vPivot].pc, TokenString);
    if vicmp = 0 then
    begin
      Result  := vPivot;
      exit;
    end
    //If the Item in the middle has a bigger value than
    //the searched item, then select the first half
    else if vicmp > 0 then
      vLast := vPivot - 1    //else select the second half
    else
      vFirst := vPivot + 1;
  end;
end;

function TIpHtmlParser.IsWhiteSpace: Boolean;
var
  i : Integer;
begin
  Result := False;
  for i := 0 to TBW - 1 do
    if FTokenStringBuf[i] > ' ' then
      Exit;
  Result := True;
end;

function TIpHtmlParser.NextChar: AnsiChar;
begin
  Result := #0;
  if FCharStream.Read(Result, 1) = 0 then
    Result := #0
  else begin
    Inc(FGlobalPos);
    if Result = #10 then begin
      Inc(FLineNumber);
      FLineOffset := 0;
    end else
      Inc(FLineOffset);
  end;
end;

procedure TIpHtmlParser.NextNonBlankToken;
begin
  repeat
    NextToken;
  until (FCurToken <> IpHtmlTagText) or not IsWhiteSpace;
end;

procedure TIpHtmlParser.NextRealToken;
begin
  repeat
    NextToken;
  until FCurToken <> IpHtmlTagText;
end;

procedure TIpHtmlParser.NextToken;
var
  parmName: string;
  PBW: Integer;
  i: Integer;
  inValue, inQuote, inAttr: Boolean;
  seenEqual, seenQuotes: Boolean;
  ctl, done, endFound: Boolean;
  quoteChar: AnsiChar;
  ch: AnsiChar;

  procedure AddParmChar(const Ch: AnsiChar);
  begin
    if PBW >= FParmBufSize - 1 then begin
      Inc(FParmBufSize, 4096);
      ReallocMem(FParmBuf, FParmBufSize);
    end;
    FParmBuf[PBW] := Ch;
    Inc(PBW);
  end;

  function ParmString: string;
  begin
    if PBW = 0 then
      Result := ''
    else begin
      FParmBuf[PBW] := #0;
      Result := StrPas(FParmBuf);
      PBW := 0;
    end;
  end;

  procedure AddTokenChar(const Ch: AnsiChar);
  begin
    FTokenStringBuf[TBW] := Ch;
    Inc(TBW);
  end;

begin
  if FHaveToken then begin
    FCurToken := FTokenBuffer;
    FHaveToken := False;
    Exit;
  end;
  quoteChar := ' ';
  repeat
    TBW := 0;
    PBW := 0;
    ClearParmValueArray;
    ch := GetChar;
    if ch = #0 then begin
      FCurToken := IpHtmlTagEof;
      Exit;
    end;
    if ch = '<' then begin
      ch := GetChar;
      if ch = '!' then begin
        if GetChar = '-' then begin
          if GetChar <> '-' then
            if FlagErrors then
              ReportError(SHtmlDashExp);
          ch := GetChar;
          repeat
            while ch <> '-' do begin
              if ch = #0 then
                break;
              ch := GetChar;
            end;
            if (ch = #0) then
              break
            else begin
              ch := GetChar;
              if ch = #0 then
                break;
              if ch = '-' then begin
                ch := GetChar;
                while (ch = '-') do
                  ch := GetChar;
                while not (ch in [#0, '>']) do
                  ch := GetChar;
                break;
              end;
            end;
          until false;
          FCurToken := IpHtmlTagComment;
        end else begin
          ch := GetChar;
          while ch <> '>' do
            ch := GetChar;
          FCurToken := IpHtmlTagComment;
        end;
      end else begin
        while ch <> '>' do begin
          if ch <= ' ' then begin
            ch := ' ';
            break;
          end;
          if ch in [#33..#255] then
            AddTokenChar(UpCase(ch));
          ch := GetChar;
        end;
        if ch = ' ' then begin
          ch := GetChar;
          {list :== [attr]* ">"}
          {attr :== [" "]* attr-name [attr-value]}
          {attr-value :== [" "]* "=" [" "]* value}
          {value :== ['"']* string ['"']*}
          inAttr := False;
          inValue := False;
          inQuote := False;
          seenEqual := False;
          seenQuotes := False;
          parmName := '';
          PBW := 0;
          while True do begin
            case ch of
              #0 : break;
              #1..#31 :
                if inAttr then begin
                  inAttr := False;
                  parmName := ParmString;
                  seenEqual := False;
                end else
                if inValue then begin
                  if parmName <> '' then begin
                    ParmValueArrayAdd(UpperCase(parmName), ParmString);
                    parmName := '';
                  end;
                  inValue := False;
                  seenEqual := False;
                  seenQuotes := False;
                end;
              ' ', '/' :
                if inQuote then
                  AddParmChar(ch)
                else
                if inAttr then begin
                  inAttr := False;
                  parmName := ParmString;
                  seenEqual := False;
                end else
                if inValue then begin
                  if parmName <> '' then begin
                    ParmValueArrayAdd(UpperCase(parmName), ParmString);
                    parmName := '';
                  end;
                  inValue := False;
                  seenEqual := False;
                  seenQuotes := False;
                end;
              '''' :
                if inQuote then
                begin 
                  if quoteChar = '''' then
                    inQuote := False
                  else
                    AddParmChar('''');
                end else 
                begin
                  inQuote := True;
                  seenQuotes := True;
                  quoteChar := '''';
                end;
              '"' :
                if inQuote then
                begin
                  if quoteChar = '"' then
                    inQuote := False
                  else
                    AddParmChar('"')
                end else
                begin
                  inQuote := True;
                  seenQuotes := True;
                  quoteChar := '"';
                end;
              '<', '>' :
                begin
                  if inQuote then
                    AddParmChar(ch)
                  else begin
                    if inValue then begin
                      if parmName <> '' then begin
                        ParmValueArrayAdd(UpperCase(parmName), ParmString);
                        parmName := '';
                      end;
                    end;
                    break;
                  end;
                end;
              '=' :
                begin
                  seenEqual := True;
                  if inAttr then begin
                    parmName := ParmString;
                    inAttr := False;
                  end else
                    if inValue then
                      AddParmChar(ch)
                end;
              else
                if inAttr or inValue then
                  AddParmChar(ch)
                else
                  if seenEqual and (inQuote or not seenQuotes) then begin
                    inValue := True;
                    AddParmChar(ch);
                  end else begin
                    if (parmName <> '') and not seenQuotes then begin
                      parmName := UpperCase(parmName);
                      ParmValueArrayAdd(parmName, parmName);
                    end;
                    parmName := '';
                    AddParmChar(ch);
                    seenEqual := False;
                    seenQuotes := False;
                    inValue := False;
                    inAttr := True;
                  end;
            end;
            ch := GetChar;
          end;
          
          if inAttr then begin
            parmName := UpperCase(ParmString);
            if (parmName <> '') then begin
              ParmValueArrayAdd(parmName, parmName);
            end;
          end;
        end;

        { Check if this is a token of the form <tok/> }
        if (TBW > 0) and (FTokenStringBuf[TBW - 1] = '/') then begin
          {It is, set EndFound flag and convert to normal open token}
          endFound := True;
          Dec(TBW);
        end else
          endFound := False;
        FTokenStringBuf[TBW] := #0;
        FCurToken := IpHtmlTagUnknown;
        i := HtmlTokenListIndexOf(FTokenStringBuf);
        if i <> -1 then
          FCurToken := IpHtmlTokens[i].tk;

        {If the token was a single terminated token ( <tok/>
         as opposed to normal a <tok></tok> sequence), we fake
         it by pushing a close token to match the open token
         which was mangled above where EndFound was set.}

        if (FCurToken <> IpHtmlTagUnknown) and endFound then
          if succ(FCurToken) in IpEndTokenSet then
            PutToken(succ(FCurToken));
      end;
    end else begin
      FCurToken := IpHtmlTagText;
      repeat
        done := True;
        ctl := False;
        while ch <> '<' do begin
          case ch of
            #0 :
              break;
            #10, #13 :
              begin
                ctl := True;
                if FInPre > 0 then
                  AddTokenChar(ch);
              end
            else
              AddTokenChar(ch);
          end;
          ch := GetChar;
        end;
        
        if ch <> #0 then begin
          ch := GetChar;
          while (ch > #0) and (ch < ' ') do
            ch := GetChar;
          case ch of
            '/', '!', 'a'..'z','A'..'Z' :
              begin
                PutChar(ch);
                PutChar('<');
              end
            else
              begin
                AddTokenChar('<');
                AddTokenChar(Ch);
                done := False;
                ch := GetChar;
              end;
          end;
        end;
        
        if (FInPre = 0) and ctl and IsWhiteSpace then
          FCurToken := IpHtmlTagCOMMENT;
      until Done;
    end;
    
    // Eat script blocks that could confuse the parsing
    // example www.sqlite.org has javascript to write dynamic
    // content inside a table
    if FCurToken = IpHtmlTagSCRIPT then 
      ParseScript(FOwner.HtmlNode, []);
  until
    (FCurToken <> IpHtmlTagCOMMENT) and 
    ((FCurToken <> IpHtmlTagText) or (FInBlock > 0) or (FInPre > 0) or not IsWhiteSpace);
end;

procedure TIpHtmlParser.ParmValueArrayAdd(const sName, sValue: string);
var
  vFirst, vLast, vPivot: Integer;
begin
  vFirst := Ord(Low(TIpHtmlAttributesSet)); //Sets the first item of the range
  vLast  := Ord(High(TIpHtmlAttributesSet)); //Sets the last item of the range

  //If First > Last then the searched item doesn't exist
  //If the item is found the loop will stop
  while (vFirst <= vLast) do
  begin
    //Gets the middle of the selected range
    vPivot := (vFirst + vLast) div 2;
    //Compares the String in the middle with the searched one
    if TIpHtmlAttributesNames[TIpHtmlAttributesSet(vPivot)] = sName then
    begin
      FParmValueArray[TIpHtmlAttributesSet(vPivot)] := sValue;
      Exit;
    end
    //If the Item in the middle has a bigger value than
    //the searched item, then select the first half
    else if TIpHtmlAttributesNames[TIpHtmlAttributesSet(vPivot)] > sName then
      vLast := Pred(vPivot)//else select the second half
    else
      vFirst := Succ(vPivot);
  end;
end;
 
procedure TIpHtmlParser.ParseAddress(AParent: TIpHtmlNode);
var
  newPara: TIpHtmlNodeADDRESS;
begin
  newPara := TIpHtmlNodeADDRESS.Create(AParent);
  newPara.ParseBaseProps(FOwner);
  NextToken;
  ParseBodyText(newPara, [IpHtmlTagADDRESSend]);
  if FCurToken = IpHtmlTagADDRESSend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagADDRESSend);
end;

function TIpHtmlParser.ParseAlignment: TIpHtmlAlign;
begin
  Result := GetAlignmentForStr(FindAttribute(htmlAttrALIGN), haDefault); 
end;

procedure TIpHtmlParser.ParseAnchor(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curAnchor: TIpHtmlNodeA;
begin
  curAnchor := TIpHtmlNodeA.Create(AParent);
  FOwner.TabList.Add(curAnchor);
  with curAnchor do begin
    Name := FindAttribute(htmlAttrNAME);
    HRef := FindAttribute(htmlAttrHREF);
    Rel := FindAttribute(htmlAttrREL);
    Rev := FindAttribute(htmlAttrREV);
    Title := FindAttribute(htmlAttrTITLE);
    ParseBaseProps(FOwner);
    Shape := ParseShape;
    TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
    Target := FindAttribute(htmlAttrTARGET);
  end;
  NextToken;
  ParseBodyText(curAnchor, EndTokens + [IpHtmlTagAend] - [IpHtmlTagA]);
  if FCurToken = IpHtmlTagAend then
    NextToken
  else
  if FCurToken = IpHtmlTagA then
  else
  if FCurToken in EndTokens then
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagAend);
  if (curAnchor.ChildCount = 0) and (CurAnchor.Name <> '') then
    TIpHtmlNodeText.Create(curAnchor).EscapedText := '&xxxxxx;';  //wp: ???
end;

procedure TIpHtmlParser.ParseApplet(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curApplet: TIpHtmlNodeAPPLET;
  curParam: TIpHtmlNodePARAM;
begin
  curApplet := TIpHtmlNodeAPPLET.Create(AParent);
  with curApplet do begin
    Codebase := FindAttribute(htmlAttrCODEBASE);
    Code := FindAttribute(htmlAttrCODE);
    Alt := FindAttribute(htmlAttrALT);
    Name := FindAttribute(htmlAttrNAME);
    Height := ParseInteger(htmlAttrHEIGHT, -1);
    Width := ParseHyperLength(htmlAttrWIDTH, '');
    Width.OnChange := @WidthChanged;
    Align := ParseImageAlignment(hiaBottom);
    HSpace := ParseInteger(htmlAttrHSPACE, 1);
    VSpace := ParseInteger(htmlAttrVSPACE, 1);
    Archive := FindAttribute(htmlAttrARCHIVE);
    ObjectCode := FindAttribute(htmlAttrOBJECT);
    Id := FindAttribute(htmlAttrID);
    ClassID := FindAttribute(htmlAttrCLASS);
    Title := FindAttribute(htmlAttrTITLE);
    Style := FindAttribute(htmlAttrSTYLE);
  end;
  NextToken;
  while not (FCurToken in EndTokens + [IpHtmlTagAPPLETend]) do begin
    case FCurToken of
      IpHtmlTagPARAM:
        begin
          curParam := TIpHtmlNodePARAM.Create(curApplet);
          with curParam do begin
            Name := FindAttribute(htmlAttrNAME);
            Value := FindAttribute(htmlAttrVALUE);
            Id := FindAttribute(htmlAttrID);
            ValueType := ParseObjectValueType;
          end;
          NextToken;
        end;
      else
        ParseText(curApplet, [IpHtmlTagAPPLETend, IpHtmlTagPARAM]);
    end;
  end;
  EnsureClosure(IpHtmlTagAPPLETend, EndTokens);
end;

procedure TIpHtmlParser.ParseBase;
begin
  NextToken;
end;

procedure TIpHtmlParser.ParseBaseFont(AParent: TIpHtmlNode);
var
  curBasefont: TIpHtmlNodeBASEFONT;
begin
  curBasefont := TIpHtmlNodeBASEFONT.Create(AParent);
//  if CurBasefont=nil then ;                                  // ???? What's this?????
  curBasefont.Size := ParseInteger(htmlAttrSIZE, 3);
  NextToken;
end;

procedure TIpHtmlParser.ParseBlink(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curBlink: TIpHtmlNodeBLINK;
begin
  curBlink := TIpHtmlNodeBLINK.Create(AParent);
  NextToken;
  ParseBodyText(curBlink, EndTokens + [IpHtmlTagBLINKend]);
  EnsureClosure(IpHtmlTagBLINKend, EndTokens);
end;

procedure TIpHtmlParser.ParseBlock(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
begin
  case FCurToken of
    IpHtmlTagH1: 
      ParseHeader(AParent, IpHtmlTagH1end, 1);
    IpHtmlTagH2: 
      ParseHeader(AParent, IpHtmlTagH2end, 2);
    IpHtmlTagH3: 
      ParseHeader(AParent, IpHtmlTagH3end, 3);
    IpHtmlTagH4: 
      ParseHeader(AParent, IpHtmlTagH4end, 4);
    IpHtmlTagH5: 
      ParseHeader(AParent, IpHtmlTagH5end, 5);
    IpHtmlTagH6: 
      ParseHeader(AParent, IpHtmlTagH6end, 6);
    {IpHtmlTagP: 
      ParseParagraph(AParent, EndTokens);} {moved to inline}
    IpHtmlTagDIR: 
      ParseUnorderedList(AParent, IpHtmlTagDIRend, EndTokens);
    IpHtmlTagMENU: 
      ParseUnorderedList(AParent, IpHtmlTagMENUend, EndTokens);
    IpHtmlTagUL: 
      ParseUnorderedList(AParent, IpHtmlTagULend, EndTokens);
    IpHtmlTagDL: 
      ParseDefinitionList(AParent, EndTokens);
    IpHtmlTagOL: 
      ParseOrderedList(AParent, IpHtmlTagOLend, EndTokens);
    IpHtmlTagPRE: 
      ParsePre(AParent, EndTokens);
    IpHtmlTagBLOCKQUOTE: 
      ParseBlockQuote(AParent, EndTokens);
    IpHtmlTagFORM: 
      ParseForm(AParent, EndTokens);
    IpHtmlTagTABLE: 
      ParseTable(AParent, EndTokens);
    IpHtmlTagIMG: 
      ParseIMG(AParent);
    IpHtmlTagOBJECT: 
      ParseObject(AParent);
    IpHtmlTagAPPLET: 
      ParseApplet(AParent, EndTokens);
    IpHtmlTagADDRESS: 
      ParseAddress(AParent);
    IpHtmlTagEof: 
      Exit;
    IpHtmlTagFRAMESET: 
      ParseFrameSet(AParent, EndTokens + [IpHtmlTagFRAMESETend]);
    IpHtmlTagUnknown:
      if FlagErrors then
        ReportError(SHtmlUnknownTok)
      else
        NextToken;
  end;
end;

procedure TIpHtmlParser.ParseBlockQuote(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  BQ: TIpHtmlNodeBLOCKQUOTE;
begin
  BQ := TIpHtmlNodeBLOCKQUOTE.Create(AParent);
  BQ.ParseBaseProps(FOwner);
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagBLOCKQUOTEend]);
  EnsureClosure(IpHtmlTagBLOCKQUOTEend, EndTokens);
end;

procedure TIpHtmlParser.ParseBody(AParent: TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
begin
  if FCurToken = IpHtmlTagFRAMESET then begin
    ParseFrameSet(AParent, EndTokens);
    Exit;
  end;
  
  {lead token is optional}
  if FCurToken = IpHtmlTagBODY then begin
    TIpHtmlNodeBODY.Create(AParent);
    with FOwner.Body do begin
      BgColor := ColorFromString(FindAttribute(htmlAttrBGCOLOR));
      TextColor := ColorFromString(FindAttribute(htmlAttrTEXT));
      Link := ColorFromString(FindAttribute(htmlAttrLINK));
      VLink := ColorFromString(FindAttribute(htmlAttrVLINK));
      ALink := ColorFromString(FindAttribute(htmlAttrALINK));
      Background := FindAttribute(htmlAttrBACKGROUND);
      ParseBaseProps(FOwner);
      LoadAndApplyCSSProps;
    end;
    NextToken;
    ParseBodyText(FOwner.Body, EndTokens + [IpHtmlTagBODYend]);
    EnsureClosure(IpHtmlTagBODYend, EndTokens);
  end else begin
    ParseBodyText(AParent, EndTokens + [IpHtmlTagBODYend]);
    FOwner.FixMissingBodyTag;
    if FCurToken = IpHtmlTagBODYend then
      NextToken;
  end;
end;

procedure TIpHtmlParser.ParseBodyText(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
begin
  Inc(FInBlock);
  try
    while not (FCurToken in EndTokens) do begin
      case FCurToken of
        IpHtmlTagH1,
        IpHtmlTagH2,
        IpHtmlTagH3,
        IpHtmlTagH4,
        IpHtmlTagH5,
        IpHtmlTagH6,
        {IpHtmlTagP,}
        IpHtmlTagDIR,
        IpHtmlTagMENU,
        IpHtmlTagUL,
        IpHtmlTagDL,
        IpHtmlTagOL,
        IpHtmlTagPRE,
        IpHtmlTagBLOCKQUOTE,
        IpHtmlTagFORM,
        IpHtmlTagTABLE,
        IpHtmlTagIMG,
        IpHtmlTagOBJECT,
        IpHtmlTagAPPLET,
        IpHtmlTagADDRESS,
        IpHtmlTagFRAMESET :
          ParseBlock(AParent, EndTokens);
        IpHtmlTagBODY :
          begin
            if FOwner.Body = nil then begin
              TIpHtmlNodeBODY.Create(AParent);
              NextToken;
              ParseBodyText(FOwner.Body, EndTokens);
            end
            else
              ParseInline(AParent, EndTokens);
          end;
        IpHtmlTagEof :
          Exit;
        else
          ParseInline(AParent, EndTokens);
      end;
    end;
  finally
    Dec(FInBlock);
  end;
end;

function TIpHtmlParser.ParseBoolean(const AttrNameSet: TIpHtmlAttributesSet): Boolean;
begin
  Result := Length(FParmValueArray[AttrNameSet]) > 0;
end;

procedure TIpHtmlParser.ParseBR(AParent: TIpHtmlNode);
var
  br: TIpHtmlNodeBR;
begin
  br := TIpHtmlNodeBR.Create(AParent);
  br.Clear := ParseBRClear;
  br.Id := FindAttribute(htmlAttrID);
  br.ClassId :=FindAttribute(htmlAttrCLASS);
  br.Title := FindAttribute(htmlAttrTITLE);
  br.Style := FindAttribute(htmlAttrSTYLE);
  NextToken;
end;

function TIpHtmlParser.ParseBRClear: TIpHtmlBreakClear;
var
  S : string;
begin
  Result := hbcNone;
  S := UpperCase(FindAttribute(htmlAttrCLEAR));
  if Length(S) = 0 then 
    exit;
  
  case S[1] of
    'A','C': if (S = 'ALL') or (S = 'CLEAR') then Result := hbcAll;
    'L': if S = 'LEFT' then Result := hbcLeft;
    'R': if S = 'RIGHT' then Result := hbcRight;
    else
      if FlagErrors then
        ReportError(SHtmlInvAlign);
  end;
end;

function TIpHtmlParser.ParseButtonType: TIpHtmlButtonType;
const
  TIpHtmlButtonTypeNames : array[TIpHtmlButtonType] of string = (
    'SUBMIT', 'RESET', 'BUTTON'
  );
var
  S: string;
begin
  Result := hbtSubmit;
  S := UpperCase(FindAttribute(htmlAttrTYPE));
  if Length(S) > 0 then
  begin
    for Result := Low(TIpHtmlButtonType) to High(TIpHtmlButtonType) do
      if S = TIpHtmlButtonTypeNames[Result] then exit;
    if FlagErrors then
      ReportError(SHtmlInvType);
  end;
end;

function TIpHtmlParser.ParseCellAlign(ADefault: TIpHtmlAlign): TIpHtmlAlign;
begin
  Result := GetAlignmentForStr(FindAttribute(htmlAttrALIGN), ADefault);
end;

procedure TIpHtmlParser.ParseCenter(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curContainer: TIpHtmlNodeDIV;
begin
  curContainer := TIpHtmlNodeDIV.Create(AParent);
  with curContainer do
    Align := haCenter;
  NextToken;
  ParseBodyText(curContainer, EndTokens + [IpHtmlTagCENTERend]);
  EnsureClosure(IpHtmlTagCENTERend, EndTokens);
end;

procedure TIpHtmlParser.ParseColGroup(AParent: TIpHtmlNode);
var
  curColGroup: TIpHtmlNodeCOLGROUP;
  curCol: TIpHtmlNodeCOL;
begin
  while FCurToken = IpHtmlTagCOLGROUP do begin
    curColGroup := TIpHtmlNodeCOLGROUP.Create(AParent);
    with curColGroup do begin
      ParseBaseProps(FOwner);
      Span := ParseInteger(htmlAttrSPAN, 1);
      Width := ParseHyperMultiLength(htmlAttrWIDTH, '');
    end;
    NextToken;
    SkipTextTokens;
    while FCurToken = IpHtmlTagCOL do begin
      curCol := TIpHtmlNodeCOL.Create(curColGroup);
      with curCol do begin
        ParseBaseProps(FOwner);
        Span := ParseInteger(htmlAttrSPAN, 1);
        Width := ParseHyperMultiLength(htmlAttrWIDTH, '');
      end;
      NextToken;
      SkipTextTokens;
    end;
    if FCurToken = IpHtmlTagCOLGROUPend then
      NextToken;
  end;
end;

procedure TIpHtmlParser.ParseDefinitionList(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  newDL: TIpHtmlNodeDL;
begin
  newDL := TIpHtmlNodeDL.Create(AParent);
  newDL.ParseBaseProps(FOwner);
  newDL.Compact := ParseBoolean(htmlAttrCOMPACT);
  NextToken;
  ParseDefinitionListItems(newDL, EndTokens + [IpHtmlTagDLend]);
  EnsureClosure(IpHtmlTagDLend, EndTokens);
end;

procedure TIpHtmlParser.ParseDefinitionListItems(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curDT: TIpHtmlNodeDT;
  curDD: TIpHtmlNodeDD;
begin
  while not (FCurToken in EndTokens) do begin
    case FCurToken of
      IpHtmlTagDT :
        begin
          curDT := TIpHtmlNodeDT.Create(AParent);
          curDT.ParseBaseProps(FOwner);
          NextToken;
          ParseBodyText(curDT, [IpHtmlTagDD, IpHtmlTagDTend] + EndTokens);
          if FCurToken = IpHtmlTagDTend then
            NextToken;
        end;
      IpHtmlTagDD :
        begin
          curDD := TIpHtmlNodeDD.Create(AParent);
          curDD.ParseBaseProps(FOwner);
          NextToken;
          ParseBodyText(curDD, [IpHtmlTagDT, IpHtmlTagDDend] + EndTokens);
          if FCurToken = IpHtmlTagDDend then
            NextToken;
        end;
      else
        ParseBodyText(AParent, EndTokens + [IpHtmlTagDT, IpHtmlTagDD]);
    end;
  end;
end;

procedure TIpHtmlParser.ParseDel(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  BQ: TIpHtmlNodeDEL;
begin
  BQ:= TIpHtmlNodeDEL.Create(AParent);
  BQ.ParseBaseProps(FOwner);
  BQ.Cite := FindAttribute(htmlAttrCITE);
  BQ.Datetime := FindAttribute(htmlAttrDATETIME);
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagDELend]);
  EnsureClosure(IpHtmlTagDELend, EndTokens);
end;

function TIpHtmlParser.ParseDir: TIpHtmlDirection;
var
  S : string;
begin
  Result := hdLTR;
  S := UpperCase(FindAttribute(htmlAttrDIR));
  if (S = '') then
    Result := hdNone
  else if (S = 'LTR') then
    Result := hdLTR
  else if (S = 'RTL') then
    Result := hdRTL
  else
    if FlagErrors then
      ReportError(SHtmlInvDir);
end;

procedure TIpHtmlParser.ParseDiv(AParent: TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
var
  curDIV: TIpHtmlNodeDIV;
begin
  curDIV := TIpHtmlNodeDIV.Create(AParent);
  with curDIV do begin
    Align := ParseAlignment;
    ParseBaseProps(FOwner);
  end;
  NextToken;
  ParseBodyText(curDIV, EndTokens + [IpHtmlTagDIVend]);
  EnsureClosure(IpHtmlTagDIVend, EndTokens);
end;

procedure TIpHtmlParser.ParseFont(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curFont: TIpHtmlNodeFONT;
begin
  curFont := TIpHtmlNodeFONT.Create(AParent);
  with curFont do begin
    Face := FindAttribute(htmlAttrFACE);
    Size.Free;
    Size := nil;
    Size := ParseRelSize{('+0')};
    Size.OnChange := @SizeChanged;
    Color := ColorFromString(FindAttribute(htmlAttrCOLOR));
    ParseBaseProps(FOwner);
  end;
  NextToken;
  ParseBodyText(curFont, EndTokens + [IpHtmlTagFONTend]);
  EnsureClosure(IpHtmlTagFONTend, EndTokens);
end;

procedure TIpHtmlParser.ParseFontStyle(AParent: TIpHtmlNode;
  StartToken: TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
var
  curStyle: TIpHtmlNodeFontStyle;
begin
  curStyle := TIpHtmlNodeFontStyle.Create(AParent);
  case StartToken of
    IpHtmlTagTT :
      curStyle.Style := hfsTT;
    IpHtmlTagI :
      curStyle.Style := hfsI;
    IpHtmlTagB :
      curStyle.Style := hfsB;
    IpHtmlTagU :
      curStyle.Style := hfsU;
    IpHtmlTagSTRIKE :
      curStyle.Style := hfsSTRIKE;
    IpHtmlTagS :
      curStyle.Style := hfsS;
    IpHtmlTagBIG :
      curStyle.Style := hfsBIG;
    IpHtmlTagSMALL :
      curStyle.Style := hfsSMALL;
    IpHtmlTagSUB :
      curStyle.Style := hfsSUB;
    IpHtmlTagSUP :
      curStyle.Style := hfsSUP;
  end;
  curStyle.ParseBaseProps(FOwner);
  NextToken;
  ParseBodyText(curStyle, EndTokens);
  EnsureClosure(succ(StartToken), EndTokens);
end;

procedure TIpHtmlParser.ParseForm(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  newForm : TIpHtmlNodeFORM;
begin
  newForm := TIpHtmlNodeFORM.Create(AParent);
  with newForm do begin
    Action := FindAttribute(htmlAttrACTION);
    Method := ParseMethod;
    Enctype := FindAttribute(htmlAttrENCTYPE);
    Name := FindAttribute(htmlAttrNAME);
    AcceptCharset := FindAttribute(htmlAttrACCEPT_CHARSET);
    Accept := FindAttribute(htmlAttrACCEPT);
    if Enctype = '' then
      Enctype := 'application/x-www-form-urlencoded';
    if AcceptCharset = '' then
      AcceptCharset := 'UNKNOWN';
    ParseBaseProps(FOwner);
  end;
  NextToken;
  ParseBodyText(newForm, EndTokens + [IpHtmlTagFORMend]);
  EnsureClosure(IpHtmlTagFORMend, EndTokens);
end;

procedure TIpHtmlParser.ParseFormFields(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curSelect: TIpHtmlNodeSELECT;
  curTextArea: TIpHtmlNodeTEXTAREA;
  curButton: TIpHtmlNodeBUTTON;
  curOptGroup: TIpHtmlNodeOPTGROUP;
  curLabel: TIpHtmlNodeLABEL;
  curFieldset: TIpHtmlNodeFIELDSET;
  curLegend: TIpHtmlNodeLEGEND;
  curOption: TIpHtmlNodeOPTION;
  curInput: TIpHtmlNodeINPUT;
begin
  while not (FCurToken in EndTokens) do begin
    case FCurToken of
      IpHtmlTagINPUT:
        begin
          curInput := TIpHtmlNodeINPUT.Create(AParent);
          FOwner.TabList.Add(curInput);
          with curInput do begin
            ParseBaseProps(FOwner);
            InputType := ParseInputType;
            Name := FindAttribute(htmlAttrNAME);
            Value := FindAttribute(htmlAttrVALUE);
            Checked := ParseBoolean(htmlAttrCHECKED);
            Size := ParseInteger(htmlAttrSIZE, -1);
            MaxLength := ParseInteger(htmlAttrMAXLENGTH, -1);
            Src := FindAttribute(htmlAttrSRC);
            Align := ParseImageAlignment(hiaBottom);
            Disabled := ParseBoolean(htmlAttrDISABLED);
            ReadOnly := ParseBoolean(htmlAttrREADONLY);
            Alt := FindAttribute(htmlAttrALT);
            TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
          end;
          NextToken;
        end;
      IpHtmlTagBUTTON:
        begin
          curButton := TIpHtmlNodeBUTTON.Create(AParent);
          FOwner.TabList.Add(curButton);
          with curButton do begin
            ParseBaseProps(FOwner);
            ButtonType := ParseButtonType;
            Name := FindAttribute(htmlAttrNAME);
            Value := FindAttribute(htmlAttrVALUE);
            Disabled := ParseBoolean(htmlAttrDISABLED);
            TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
          end;
          NextToken;
          ParseBodyText(curButton, EndTokens + [IpHtmlTagBUTTONend]);
          if FCurToken = IpHtmlTagBUTTONend then
            NextToken
          else
            if FlagErrors then
              ReportExpectedToken(IpHtmlTagBUTTONend);
        end;
      IpHtmlTagSELECT:
        begin
          curSelect := TIpHtmlNodeSELECT.Create(AParent);
          with curSelect do begin
            Name := FindAttribute(htmlAttrNAME);
            Size := ParseInteger(htmlAttrSIZE, -1);
            Width := ParseInteger(htmlAttrWIDTH, -1);
            ParseBaseProps(FOwner);
            Multiple := ParseBoolean(htmlAttrMULTIPLE);
            ComboBox := ParseBoolean(htmlAttrCOMBOBOX);
            Disabled := ParseBoolean(htmlAttrDISABLED);
            TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
            Alt := FindAttribute(htmlAttrALT);
          end;
          NextNonBlankToken;
          repeat
            case FCurToken of
              IpHtmlTagOPTION :
                begin
                  curOption := TIpHtmlNodeOPTION.Create(curSelect);
                  with curOption do begin
                    ParseBaseProps(FOwner);
                    Selected := ParseBoolean(htmlAttrSELECTED);
                    Value := FindAttribute(htmlAttrVALUE);
                    Disabled := ParseBoolean(htmlAttrDISABLED);
                    OptionLabel := FindAttribute(htmlAttrLABEL);
                  end;
                  NextNonBlankToken;
                  ParseText(curOption, EndTokens + [IpHtmlTagSelectEND, IpHtmlTagOption, IpHtmlTagOPTIONend]);
                  if FCurToken = IpHtmlTagOPTIONend then
                    NextNonBlankToken;
                end;
              IpHtmlTagOPTGROUP :
                begin
                  curOptGroup := TIpHtmlNodeOPTGROUP.Create(curSelect);
                  with curOptGroup do begin
                    ParseBaseProps(FOwner);
                    Disabled := ParseBoolean(htmlAttrDISABLED);
                    GroupLabel := FindAttribute(htmlAttrLABEL);
                  end;
                  NextNonBlankToken;
                  while FCurToken = IpHtmlTagOPTION do begin
                    curOption := TIpHtmlNodeOPTION.Create(curOptGroup);
                    FOwner.TabList.Add(curOption);
                    with curOption do begin
                      ParseBaseProps(FOwner);
                      Selected := ParseBoolean(htmlAttrSELECTED);
                      Value := FindAttribute(htmlAttrVALUE);
                      Disabled := ParseBoolean(htmlAttrDISABLED);
                      OptionLabel := FindAttribute(htmlAttrLABEL);
                    end;
                    NextNonBlankToken;
                    ParseText(curOption, EndTokens + [IpHtmlTagSelectEND, IpHtmlTagOption, IpHtmlTagOPTIONend]);
                    if FCurToken = IpHtmlTagOPTIONend then
                      NextNonBlankToken;
                  end;
                  if FCurToken = IpHtmlTagOPTGROUPend then
                    NextNonBlankToken
                  else
                  if FCurToken = IpHtmlTagOPTGROUP then
                  else
                  if FCurToken = IpHtmlTagOPTION then
                  else
                  if FCurToken = IpHtmlTagSELECTend then
                  else
                    if FlagErrors then
                      ReportExpectedToken(IpHtmlTagOPTGROUPend);
                end;
              else
                break;
            end;
          until False;
          if FCurToken = IpHtmlTagSELECTend then
            NextNonBlankToken;
        end;
      IpHtmlTagTEXTAREA:
        begin
          curTextArea := TIpHtmlNodeTEXTAREA.Create(AParent);
          FOwner.TabList.Add(curTextArea);
          with curTextArea do begin
            Name := FindAttribute(htmlAttrNAME);
            Rows := ParseInteger(htmlAttrROWS, 20);
            Cols := ParseInteger(htmlAttrCOLS, 20);
            ParseBaseProps(FOwner);
            Disabled := ParseBoolean(htmlAttrDISABLED);
            ReadOnly := ParseBoolean(htmlAttrREADONLY);
            TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
            Alt := FindAttribute(htmlAttrALT);
          end;
          NextToken;
          ParseText(curTextArea, [IpHtmlTagTEXTAREAend]);
          if FCurToken = IpHtmlTagTEXTAREAend then
            NextToken
          else
            if FlagErrors then
              ReportExpectedToken(IpHtmlTagTEXTAREAend);
        end;
      IpHtmlTagLABEL :
        begin
          curLabel := TIpHtmlNodeLABEL.Create(AParent);
          with curLabel do begin
            ParseBaseProps(FOwner);
            LabelFor := FindAttribute(htmlAttrLABEL);
          end;
          NextToken;
          ParseBodyText(curLabel, [IpHtmlTagLABELend]);
          if FCurToken = IpHtmlTagLABELend then
            NextToken
          else
            if FlagErrors then
              ReportExpectedToken(IpHtmlTagLABELend);
        end;
      IpHtmlTagFIELDSET :
        begin
          curFieldset := TIpHtmlNodeFIELDSET.Create(AParent);
          with curFieldset do
            ParseBaseProps(FOwner);
          NextToken;
          ParseFormFields(curFieldSet, EndTokens + [IpHtmlTagFIELDSETend]);
          if FCurToken = IpHtmlTagFIELDSETend then
            NextToken
          else
            if FlagErrors then
              ReportExpectedToken(IpHtmlTagFIELDSETend);
        end;
      IpHtmlTagLEGEND :
        begin
          curLegend := TIpHtmlNodeLEGEND.Create(AParent);
          with curLegend do begin
            ParseBaseProps(FOwner);
          end;
          NextToken;
          ParseBodyText(CurLegend, [IpHtmlTagLEGENDend]);
          if FCurToken = IpHtmlTagLEGENDend then
            NextToken
          else
            if FlagErrors then
              ReportExpectedToken(IpHtmlTagLEGENDend);
        end;
      else
        Exit;
    end;
  end;
end;

procedure TIpHtmlParser.ParseFrame(AParent: TIpHtmlNode);
var
  curFrame: TIpHtmlNodeFRAME;
begin
  curFrame := TIpHtmlNodeFRAME.Create(AParent);
  with curFrame do begin
    LongDesc := FindAttribute(htmlAttrLONGDESC);
    Name := FindAttribute(htmlAttrNAME);
    Src := FindAttribute(htmlAttrSRC);
    FrameBorder := ParseInteger(htmlAttrBORDER, 1);
    MarginWidth := ParseInteger(htmlAttrMARGINWIDTH, 1);
    MarginHeight := ParseInteger(htmlAttrMARGINHEIGHT, 1);
    NoResize := ParseBoolean(htmlAttrNORESIZE);
    Scrolling := ParseFrameScrollingProp;
    ParseBaseProps(FOwner);
  end;
  NextToken;
end;

function TIpHtmlParser.ParseFrameProp(ADefault: TIpHtmlFrameProp): TIpHtmlFrameProp;
var
  S: string;
begin
  Result := hfVoid;
  S := UpperCase(FindAttribute(htmlAttrFRAME));
  if Length(S) = 0 then
  begin
    Result := ADefault;
    exit;
  end;
  
  case S[1] of
    'A': if (S = 'ABOVE') then Result := hfAbove;
    'B': if S = 'BELOW' then Result := hfBelow
         else if S = 'BOX' then Result := hfBox
         else if S = 'BORDER' then Result := hfBorder;
    'H': if S = 'HSIDES' then Result := hfHSides;
    'L': if S = 'LHS' then Result := hfLhs;
    'R': if S = 'RHS' then Result := hfRhs;
    'V': if (S = 'VOID') then exit
         else if S = 'VSIDES' then
           Result := hfvSides;
    else
      if FlagErrors then
        ReportError(SHtmlInvFrame);
  end;
end;      

function TIpHtmlParser.ParseFrameScrollingProp: TIpHtmlFrameScrolling;
var
  S: string;
begin
  Result := hfsAuto;
  S := UpperCase(FindAttribute(htmlAttrSCROLLING));
  if (length(S) = 0) then exit;
  case S[1] of
    'A': if (S = 'AUTO') then exit;
    'N': if S = 'NO' then Result := hfsNo;
    'Y': if S = 'YES' then Result := hfsYes;
    else
      if FlagErrors then
        ReportError(SHtmlInvScroll);
  end;
end;

procedure TIpHtmlParser.ParseFrameSet(AParent: TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
begin
  FHasFrames := True;   
  while FCurToken = IpHtmlTagFRAMESET do begin
    FCurFrameSet := TIpHtmlNodeFRAMESET.Create(AParent);
    with FCurFrameSet do begin
      Rows := ParseHyperMultiLengthList(htmlAttrROWS, '100%');
      Cols := ParseHyperMultiLengthList(htmlAttrCOLS, '100%');
      Id := FindAttribute(htmlAttrID);
      ClassId := FindAttribute(htmlAttrCLASS);
      Title := FindAttribute(htmlAttrTITLE);
      Style := FindAttribute(htmlAttrSTYLE);
    end;
    NextToken;
    if FCurToken = IpHtmlTagFRAMESET then
      ParseFrameSet(FCurFrameSet, EndTokens + [IpHtmlTagFRAMESETend]);
    while FCurToken = IpHtmlTagFRAME do
      ParseFrame(FCurFrameSet);
    if FCurToken = IpHtmlTagNOFRAMES then
      ParseNOFRAMES(FCurFrameSet);
    if FCurToken = IpHtmlTagFRAMESETend then
      NextToken;
  end;
end;

procedure TIpHtmlParser.ParseHead(AParent: TIpHtmlNode);
var
  L: TStringListUTF8Fast;
begin
  {lead token is optional}
  if FCurToken = IpHtmlTagHEAD then begin
    NextToken;
    ParseHeadItems(TIpHtmlNodeHEAD.Create(AParent));
    if FCurToken = IpHtmlTagHEADend then
      NextToken;
  end;
  
  L := TStringListUTF8Fast.Create;
  try
    GetSupportedEncodings(L);
    if L.IndexOf(FDocCharset) = 0 then  // clear for UTF-8 to avoid conversion
      FDocCharset := '';
  finally
    L.Free;
  end;
end;

procedure TIpHtmlParser.ParseHeader(AParent: TIpHtmlNode; EndToken: TIpHtmlToken; 
  ASize: Integer);
var
  newHeader: TIpHtmlNodeHeader;
begin
  newHeader := TIpHtmlNodeHeader.Create(AParent);
  newHeader.ElementName := 'h' + IntToStr(ASize);
  newHeader.ParseBaseProps(FOwner);
  newHeader.Size := ASize;
  newHeader.Align := ParseAlignment;
  NextToken;
  ParseBodyText(newHeader, [EndToken]);
  if FCurToken = EndToken then
    NextToken
  else if FlagErrors then
    ReportExpectedToken(EndToken);
end;

procedure TIpHtmlParser.ParseHeadItems(AParent: TIpHtmlNode);
begin
  while not (FCurToken in [IpHtmlTagEOF, IpHtmlTagHEADend, IpHtmlTagFRAMESET, IpHtmlTagBODY]) do 
  begin
    case FCurToken of
      IpHtmlTagTITLE :
        ParseTitle(AParent);
      IpHtmlTagSTYLE :
        ParseStyle(AParent);
      IpHtmlTagSCRIPT :
        ParseScript(AParent, [IpHtmlTagEOF]);
      IpHtmlTagNOSCRIPT :
        ParseNoscript(AParent);
      IpHtmlTagISINDEX :
        ParseIsIndex;
      IpHtmlTagBASE :
        ParseBase;
      IpHtmlTagMETA :
        ParseMeta(AParent);
      IpHtmlTagLINK :
        ParseLink(AParent);
      else
        NextToken;    // unknown
    end;
  end;
end;

procedure TIpHtmlParser.ParseHR(AParent: TIpHtmlNode);
var
  newRule: TIpHtmlNodeHR;
begin
  newRule := TIpHtmlNodeHR.Create(AParent);
  with newRule do begin
    Align := ParseImageAlignment(hiaCenter);
    NoShade := ParseBoolean(htmlAttrNOSHADE);
    Size := ParseHtmlInteger2(htmlAttrSIZE, 1);
    Size.OnChange := @WidthChanged;
    Width := ParseHyperLength(htmlAttrWIDTH, '100%');
    Width.OnChange := @WidthChanged;
    Color := ColorFromString(FindAttribute(htmlAttrCOLOR));
    ParseBaseProps(FOwner);
  end;
  NextToken;
end;

procedure TIpHtmlParser.ParseHtml;
begin
  {lead token is optional}
  if FCurToken = IpHtmlTagHtml then begin
    FOwner.HtmlNode.Version := FindAttribute(htmlAttrVERSION);
    FOwner.HtmlNode.Lang := FindAttribute(htmlAttrLANG);
    FOwner.HtmlNode.Dir := ParseDir;
    NextToken;
    ParseHead(FOwner.HtmlNode); {may not be present}
    ParseBody(FOwner.HtmlNode, [IpHtmlTagHtmlend, IpHtmlTagEOF]); {may not be present}
    if FCurToken in [IpHtmlTagHtmlend, IpHtmlTagEOF] then
    else
      if FlagErrors then
        ReportExpectedToken(IpHtmlTagHtmlend);
    NextToken;
  end else begin
    ParseHead(FOwner.HtmlNode); {may not be present}
    ParseBody(FOwner.HtmlNode, [IpHtmlTagEof]); {may not be present}
  end;
end;

function TIpHtmlParser.ParseHyperLength(const AttrNameSet: TIpHtmlAttributesSet;
  const ADefault: string): TIpHtmlLength;
var
  S, units: string;
  n: Double;
  P, Err: Integer;
begin
  Result := TIpHtmlLength.Create;
  Result.LengthType := hlUndefined;
  S := FindAttribute(AttrNameSet);
  if Length(S) = 0 then
  begin
    if Length(aDefault) = 0 then 
      exit
    else 
      S := ADefault;
  end;
  P := CharPos('%', S);
  if P <> 0 then begin
    Result.LengthType := hlPercent;
    Delete(S, P, 1);
  end else
    Result.LengthType := hlAbsolute;
  // Remove non-numeric appendix
  units := '';
  for P := Length(S) downto 1 do
    if not (S[P] in ['0'..'9', '+', '-', '.']) then
      units := S[P] + units
    else begin
      SetLength(S, P);
      break;
    end;
  val(S, n, Err);
  if n < 0 then n := 0;
  Result.LengthValue := round(n);
  if (Err <> 0) or (Result.LengthValue < 0) then begin
    if FlagErrors then
      ReportError(SHtmlInvInt)
    else
      Result.LengthType := hlUndefined;
  end else
    if (Result.LengthType = hlPercent) and (Result.LengthValue > 100) then
      Result.LengthValue := 100;
end;

function TIpHtmlParser.ParseHyperMultiLength(const AttrNameSet: TIpHtmlAttributesSet;
  const ADefault: string): TIpHtmlMultiLength;
var
  S, units: string;
  n: Double;
  P, Err: Integer;
begin
  Result := TIpHtmlMultiLength.Create;
  Result.LengthType := hmlUndefined;
  S := FindAttribute(AttrNameSet);
  if Length(S) = 0 then
  begin
    if Length(ADefault) = 0 then 
      exit
    else 
      S := ADefault;
  end;
  P := CharPos('%', S);
  if P <> 0 then begin
    Result.LengthType := hmlPercent;
    Delete(S, P, 1);
  end else begin
    P := CharPos('*', S);
    if P <> 0 then begin
      Result.LengthType := hmlRelative;
      Delete(S, P, 1);
    end else
      Result.LengthType := hmlAbsolute;
  end;
  
  // Remove non-numeric appendix
  units := '';
  for P := Length(S) downto 1 do
    if not (S[P] in ['0'..'9', '+', '-', '.']) then
      units := S[P] + units
    else begin
      SetLength(S, P);
      break;
    end;

  val(s, n, Err);
  if n < 0 then n := 0;
  Result.LengthValue := round(n);
  if (Err <> 0) or (Result.LengthValue < 0) then begin
    if FlagErrors then
      ReportError(SHtmlInvInt)
    else
      Result.LengthType := hmlUndefined;
  end;
end;

function TIpHtmlParser.ParseHyperMultiLengthList(const AttrNameSet: TIpHtmlAttributesSet;
  const ADefault: string): TIpHtmlMultiLengthList;
var
  S, S2, units: string;
  B, E, P, Err: Integer;
  n: Double;
  NewEntry: TIpHtmlMultiLength;
begin
  Result := TIpHtmlMultiLengthList.Create;
  S := FindAttribute(AttrNameSet);
  if Length(S) = 0 then
  begin
    if length(ADefault) = 0 then 
      exit
    else 
      S := ADefault;
  end;
  B := 1;
  while B <= length(S) do begin
    E := B;
    repeat
      Inc(E);
    until (E > Length(S)) or (S[E] = ',');
    S2 := copy(S, B, E - B);
    newEntry := TIpHtmlMultiLength.Create;
    newEntry.LengthType := hmlUndefined;
    P := CharPos('%', S2);
    if P <> 0 then begin
      newEntry.LengthType := hmlPercent;
      Delete(S2, P, 1);
    end else begin
      P := CharPos('*', S2);
      if P <> 0 then begin
        newEntry.LengthType := hmlRelative;
        Delete(S2, P, 1);
      end else
        newEntry.LengthType := hmlAbsolute;
    end;
    if S2 = '' then
      newEntry.LengthValue := 0
    else begin
      // Remove non-numeric appendix
      units := '';
      for P := Length(S2) downto 1 do
        if not (S2[P] in ['0'..'9', '+', '-', '.']) then
          units := S2[P] + units
        else begin
          SetLength(S2, P);
          break;
        end;
      val(S2, n, Err);
      if n < 0 then n := 0;
      newEntry.LengthValue := round(n);
      if (Err <> 0) or (NewEntry.LengthValue < 0) then begin
        if FlagErrors then
          ReportError(SHtmlInvInt)
        else
          newEntry.LengthType := hmlUndefined;
      end;
    end;
    Result.AddEntry(newEntry);
    B := E + 1;
  end;
end;

procedure TIpHtmlParser.ParseIFrame(AParent: TIpHtmlNode);
var
  curFrame: TIpHtmlNodeIFRAME;
begin
  curFrame := TIpHtmlNodeIFRAME.Create(AParent);
  with curFrame do begin
    LongDesc := FindAttribute(htmlAttrLONGDESC);
    Name := FindAttribute(htmlAttrNAME);
    Src := FindAttribute(htmlAttrSRC);
    FrameBorder := ParseInteger(htmlAttrBORDER, 1);
    MarginWidth := ParseInteger(htmlAttrMARGINWIDTH, 1);
    MarginHeight := ParseInteger(htmlAttrMARGINHEIGHT, 1);
    Scrolling := ParseFrameScrollingProp;
    Align := ParseAlignment;
    Height := ParseHyperLength(htmlAttrHEIGHT, '');
    Height.OnChange := @WidthChanged;
    Width := ParseHyperLength(htmlAttrWIDTH, '');
    Width.OnChange := @WidthChanged;
    ParseBaseProps(FOwner);
  end;
  NextToken;
  ParseBodyText(curFrame, [IpHtmlTagIFRAMEend]);
  if FCurToken = IpHtmlTagIFRAMEend then
    NextToken;
end;

function TIpHtmlParser.ParseImageAlignment(ADefault: TIpHtmlImageAlign): TIpHtmlImageAlign;
const
  TIpHtmlImageAlignNames : array[TIpHtmlImageAlign] of string = (
    'TOP', 'MIDDLE', 'BOTTOM', 'LEFT', 'RIGHT', 'CENTER');
var
  S : string;
begin
  Result := aDefault;
  S := UpperCase(FindAttribute(htmlAttrALIGN));
  if Length(S) = 0 then exit;
  for Result := Low(TIpHtmlImageAlign) to High(TIpHtmlImageAlign) do
    if S = TIpHtmlImageAlignNames[Result] then 
      exit;
  
  if FlagErrors then
    ReportError(SHtmlInvAlign);
end;

procedure TIpHtmlParser.ParseImg(AParent: TIpHtmlNode);
var
  curIMG : TIpHtmlNodeIMG;
begin
  curIMG := TIpHtmlNodeIMG.Create(AParent);
  with curIMG do begin
    Src := FindAttribute(htmlAttrSRC);
    Alt := FindAttribute(htmlAttrALT);
    Align := ParseImageAlignment(hiaBottom);
    Height := ParsePixels(htmlAttrHEIGHT, '');
    Height.OnChange := @DimChanged;
    Width := ParseHyperLength(htmlAttrWIDTH, '');
    Width.OnChange := @DimChanged;
    Border := ParseInteger(htmlAttrBORDER, 0);
    HSpace := ParseInteger(htmlAttrHSPACE, 0);
    VSpace := ParseInteger(htmlAttrVSPACE, 0);
    UseMap := FindAttribute(htmlAttrUSEMAP);
    IsMap := ParseBoolean(htmlAttrISMAP);
    ParseBaseProps(FOwner);
    LongDesc := FindAttribute(htmlAttrLONGDESC);
    Name := FindAttribute(htmlAttrNAME);
  end;
  NextToken;
end;    

procedure TIpHtmlParser.ParseInline(AParent: TIpHtmlNode; const EndTokens: TIpHtmlTokenSet);
begin
  case FCurToken of
    IpHtmlTagP: 
      ParseParagraph(AParent, EndTokens); {moved from block}
    IpHtmlTagFont: 
      ParseFont(AParent, EndTokens);
    IpHtmlTagDIV: 
      ParseDiv(AParent, EndTokens);
    IpHtmlTagSPAN: 
      ParseSpan(AParent, EndTokens);
    IpHtmlTagLEFT: 
      ParseLeft(AParent, EndTokens);
    IpHtmlTagCENTER: 
      ParseCenter(AParent, EndTokens);
    IpHtmlTagRIGHT: 
      ParseRight(AParent, EndTokens);
    IpHtmlTagBLINK: 
      ParseBlink(AParent, EndTokens);
    IpHtmlTagQ: 
      ParseQ(AParent, EndTokens);
    IpHtmlTagHR: 
      ParseHR(AParent);
    IpHtmlTagTT, IpHtmlTagI, IpHtmlTagB, IpHtmlTagU, IpHtmlTagSTRIKE, IpHtmlTagS,
    IpHtmlTagBIG, IpHtmlTagSMALL, IpHtmlTagSUB, IpHtmlTagSUP :
      ParseFontStyle(AParent, FCurToken, EndTokens + [succ(FCurToken)]);
    IpHtmlTagEM, IpHtmlTagSTRONG, IpHtmlTagDFN, IpHtmlTagCODE,
    IpHtmlTagSAMP, IpHtmlTagKBD, IpHtmlTagVAR, IpHtmlTagCITE,
    IpHtmlTagABBR, IpHtmlTagACRONYM :
      ParsePhraseElement(AParent, FCurToken, succ(FCurToken), EndTokens);
    IpHtmlTagA: 
      ParseAnchor(AParent, EndTokens);
    IpHtmlTagBASEFONT: 
      ParseBaseFont(AParent);
    IpHtmlTagBR: 
      ParseBR(AParent);
    IpHtmlTagNOBR: 
      ParseNOBR(AParent);
    IpHtmlTagMAP: 
      ParseMAP(AParent, EndTokens);
    IpHtmlTagText:
      begin
        if FInPre > 0 then
          TIpHtmlNodeText.Create(AParent).ANSIText := GetTokenString
        else
          TIpHtmlNodeText.Create(AParent).EscapedText := GetTokenString;
        NextToken;
      end;
    IpHtmlTagINPUT,
    IpHtmlTagSELECT,
    IpHtmlTagButton,
    IpHtmlTagTEXTAREA:
      ParseFormFields(AParent, EndTokens);
    IpHtmlTagINS:
      ParseIns(AParent, EndTokens);
    IpHtmlTagDEL:
      ParseDel(AParent, EndTokens);
    IpHtmlTagIFRAME:
      ParseIFRAME(AParent);
    IpHtmlTagSCRIPT:
      ParseScript(AParent, EndTokens);
    IpHtmlTagNOSCRIPT:
      ParseNoscript(AParent);
    IpHtmlTagSTYLE:
      ParseStyle(AParent);
    else
      NextToken;
  end;
end;

function TIpHtmlParser.ParseInputType: TIpHtmlInputType;
const
  IpHtmlInputTypeNames: array[TIpHtmlInputType] of string = (
    'TEXT', 'PASSWORD', 'CHECKBOX', 'RADIO',
    'SUBMIT', 'RESET', 'FILE', 'HIDDEN', 'IMAGE', 'BUTTON'
  );
var
  S : string;
begin
  Result := hitText;
  S := UpperCase(FindAttribute(htmlAttrTYPE));
  if (Length(S) = 0) or (S = 'TEXTAREA') then
    //
  else
  begin
    for Result := Low(TIpHtmlInputType) to High(TIpHtmlInputType) do
      if S = IpHtmlInputTypeNames[Result] then exit;
    if FlagErrors then
      ReportError(SHtmlInvType);
  end;
end;

procedure TIpHtmlParser.ParseIns(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  BQ: TIpHtmlNodeINS;
begin
  BQ := TIpHtmlNodeINS.Create(AParent);
  BQ.ParseBaseProps(FOwner);
  BQ.Cite := FindAttribute(htmlAttrCITE);
  BQ.Datetime := FindAttribute(htmlAttrDATETIME);
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagINSend]);
  EnsureClosure(IpHtmlTagINSend, EndTokens);
end;

function TIpHtmlParser.ParseInteger(const AttrNameSet: TIpHtmlAttributesSet; 
  ADefault: Integer): Integer;
var
  S: string;
  Err: Integer;
  AttrName: string;
begin
  AttrName := TIpHtmlAttributesNames[AttrNameSet];
  S := FindAttribute(AttrNameSet);
  if Length(S) = 0 then
    Result := ADefault
  else
  if CompareText(S, AttrName) = 0 then
    Result := 1
  else begin
    Val(S, Result, Err);
    if Err <> 0 then begin
      Result := ADefault;
      if FlagErrors then
        ReportError(SHtmlInvInt)
    end;
  end;
end;

function TIpHtmlParser.ParseHtmlInteger2(const AttrNameSet: TIpHtmlAttributesSet;
  ADefault: Integer): TIpHtmlInteger;
begin
  Result := TIpHtmlInteger.Create(ParseInteger(AttrNameSet, aDefault));
end;

procedure TIpHtmlParser.ParseIsIndex;
begin
  FIndexPhrase := FindAttribute(htmlAttrPROMPT);
  NextToken;
end;

procedure TIpHtmlParser.ParseLeft(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curContainer: TIpHtmlNodeDIV;
begin
  curContainer := TIpHtmlNodeDIV.Create(AParent);
  with curContainer do
    Align := haLeft;
  NextToken;
  ParseBodyText(curContainer, EndTokens + [IpHtmlTagLEFTend]);
  EnsureClosure(IpHtmlTagLEFTend, EndTokens);
end;

procedure TIpHtmlParser.ParseLink(AParent: TIpHtmlNode);
begin
  with TIpHtmlNodeLINK.Create(AParent) do begin
    HRef := FindAttribute(htmlAttrHREF);
    Rel := FindAttribute(htmlAttrREL);
    Rev := FindAttribute(htmlAttrREV);
    Title := FindAttribute(htmlAttrTITLE);
    Type_ := LowerCase(FindAttribute(htmlAttrTYPE));
    if (LowerCase(Rel) = 'stylesheet') and (Type_ = 'text/css') then
      ParseStyleSheet(AParent, Href);
    ParseBaseProps(FOwner);
  end;
  NextToken;
end;

procedure TIpHtmlParser.ParseListItems(AParent: TIpHtmlNodeCore;
  EndToken: TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
var
  newListItem: TIpHtmlNodeLI;
begin
  while not (FCurToken in EndTokens) do begin
    case FCurToken of
      IpHtmlTagLI :
        begin
          newListItem := TIpHtmlNodeLI.Create(AParent);
          newListItem.ParseBaseProps(FOwner);
          newListItem.ListType := ParseULStyle(ulUndefined);
          newListItem.Value := ParseInteger(htmlAttrVALUE, -1);
          newListItem.Compact := ParseBoolean(htmlAttrCOMPACT);
          NextToken;
          ParseBodyText(
            newListItem,
            EndTokens + [EndToken, IpHtmlTagLI, IpHtmlTagLIend] - [IpHtmlTagP, IpHtmlTagPend]
          );
          if FCurToken = IpHtmlTagLIend then
            NextToken;
          SkipTextTokens;
        end;
      else
        ParseBodyText(AParent, EndTokens + [EndToken, IpHtmlTagLI]);
    end;
  end;
end;

procedure TIpHtmlParser.ParseMap(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curMap: TIpHtmlNodeMAP;
begin
  curMap := TIpHtmlNodeMAP.Create(AParent);
  curMap.Name := FindAttribute(htmlAttrNAME);
  curMap.ParseBaseProps(FOwner);
  NextToken;
  while not (FCurToken in EndTokens + [IpHtmlTagMAPend]) do begin
    case FCurToken of
      IpHtmlTagAREA :
        begin
          with TIpHtmlNodeAREA.Create(curMap) do begin
            Shape := ParseShape;
            Coords := FindAttribute(htmlAttrCOORDS);
            HRef := FindAttribute(htmlAttrHREF);
            NoHRef := ParseBoolean(htmlAttrNOHREF);
            Alt := FindAttribute(htmlAttrALT);
            TabIndex := ParseInteger(htmlAttrTABINDEX, -1);
            Target := FindAttribute(htmlAttrTARGET);
            ParseBaseProps(FOwner);
          end;
          NextToken;
        end;
      else
        if FlagErrors then
          ReportExpectedError('</MAP> or <AREA>')
        else
          NextToken;
    end;
  end;
  EnsureClosure(IpHtmlTagMAPend, EndTokens);
end;

procedure TIpHtmlParser.ParseMeta(AParent: TIpHtmlNode);
var
  i,j: Integer;
begin
  with TIpHtmlNodeMETA.Create(AParent) do begin
    HttpEquiv := FindAttribute(htmlAttrHTTP_EQUIV);
    Name := FindAttribute(htmlAttrNAME);
    Content := FindAttribute(htmlAttrCONTENT);
    if not FHasBOM then begin
      if SameText(HttpEquiv, 'content-type') then begin
        j := PosI('charset=', Content);
        if j>0 then begin
          j := j+8;
          i := j;
          while (j <= Length(Content)) do begin
            if Content[j] in [' ',';','"',','] then
              break;
            inc(j);
          end;
          fDocCharset := copy(content, i, j-i);
        end;
      end
      else
        fDocCharset := FindAttribute(htmlAttrCHARSET);
      if LazStartsText('windows', fDocCharset) then
        fDocCharset := NormalizeEncoding(StringReplace(fDocCharset, 'windows', 'cp', [rfIgnoreCase]));
    end;
    Scheme := FindAttribute(htmlAttrSCHEME);
  end;
  NextToken;
end;

function TIpHtmlParser.ParseMethod: TIpHtmlFormMethod;
var
  S: string;
begin
  Result := hfmGet;
  S := UpperCase(FindAttribute(htmlAttrMETHOD));
  if (Length(S) = 0) or (S = 'GET') then
  else
  if S = 'POST' then
    Result := hfmPost
  else
    if FlagErrors then
      ReportError(SHtmlInvMethod);
end;

procedure TIpHtmlParser.ParseNOBR(AParent: TIpHtmlNode);
begin
  NextToken;
  ParseBodyText(TIpHtmlNodeNOBR.Create(AParent), [IpHtmlTagNOBRend]);
  if FCurToken = IpHtmlTagNOBRend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagNOBRend);
end;

procedure TIpHtmlParser.ParseNoFrames(AParent: TIpHtmlNode);
var
  curNoFrames: TIpHtmlNodeNOFRAMES;
begin
  curNoFrames := TIpHtmlNodeNOFRAMES.Create(AParent);
  NextToken;
  ParseBodyText(curNoFrames, [IpHtmlTagNOFRAMESend, IpHtmlTagFRAMESETend]);
  if FCurToken = IpHtmlTagNOFRAMESend then
    NextToken;
end;

procedure TIpHtmlParser.ParseNoScript(AParent: TIpHtmlNode);
var
  curScript: TIpHtmlNodeNOSCRIPT;
begin
  curScript := TIpHtmlNodeNOSCRIPT.Create(AParent);
  with curScript do begin
    ParseBaseProps(FOwner);
  end;
  NextToken;
  ParseBodyText(curScript, [IpHtmlTagNOSCRIPTend]);
  if FCurToken = IpHtmlTagNOSCRIPTend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagNOSCRIPTend);
end;

procedure TIpHtmlParser.ParseObject(AParent: TIpHtmlNode);
var
  curObject: TIpHtmlNodeOBJECT;
  curParam: TIpHtmlNodePARAM;
begin
  curObject := TIpHtmlNodeOBJECT.Create(AParent);
  with curOBJECT do begin
    ClassID := FindAttribute(htmlAttrCLASSID);
    Codebase := FindAttribute(htmlAttrCODEBASE);
    Data := FindAttribute(htmlAttrDATA);
    CodeType := FindAttribute(htmlAttrCODETYPE);
    Archive := FindAttribute(htmlAttrARCHIVE);
    Standby := FindAttribute(htmlAttrSTANDBY);
    Align := ParseImageAlignment(hiaBottom);
    Height := ParseInteger(htmlAttrHEIGHT, -1);
    Width := ParseHyperLength(htmlAttrWIDTH, '');
    Width.OnChange := @WidthChanged;
    Border := ParseInteger(htmlAttrBORDER, 0);
    HSpace := ParseInteger(htmlAttrHSPACE, 1);
    VSpace := ParseInteger(htmlAttrVSPACE, 1);
    UseMap := FindAttribute(htmlAttrUSEMAP);
    Declare := ParseBoolean(htmlAttrDECLARE);
    ParseBaseProps(FOwner);
    Name := FindAttribute(htmlAttrNAME);
  end;
  NextToken;
  while not (FCurToken = IpHtmlTagOBJECTend) do begin
    case FCurToken of
      IpHtmlTagPARAM :
        begin
          curParam := TIpHtmlNodePARAM.Create(curObject);
          with curParam do begin
            Name := FindAttribute(htmlAttrNAME);
            Value := FindAttribute(htmlAttrVALUE);
            Id := FindAttribute(htmlAttrID);
            ValueType := ParseObjectValueType;
          end;
          NextToken;
        end;
      else
        ParseText(curObject, [IpHtmlTagOBJECTend, IpHtmlTagPARAM]);
    end;
  end;
  if FCurToken = IpHtmlTagOBJECTend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagOBJECTend);
end;

function TIpHtmlParser.ParseObjectValueType: TIpHtmlObjectValueType;
var
  S: string;
begin
  Result := hovtData;
  S := UpperCase(FindAttribute(htmlAttrVALUETYPE));
  if Length(S) = 0 then 
    exit;
  case S[1] of
    'D': if S = 'DATA' then exit;
    'O': if S = 'OBJECT' then Result := hovtObject;
    'R': if S = 'REF' then Result := hovtRef;
    else
      if FlagErrors then
        ReportError(SHtmlInvValType);
  end;
end;

function TIpHtmlParser.ParseOLStyle(ADefault: TIpHtmlOLStyle): TIpHtmlOLStyle;
const
  TIpHtmlOLStyleNames : array[TIpHtmlOLStyle] of char = ('1', 'a', 'A', 'i', 'I');
var
  S : string;
begin
  Result := ADefault;
  S := FindAttribute(htmlAttrTYPE);
  if Length(S) > 0 then
  begin
    for Result := Low(TIpHtmlOLStyle) to High(TIpHtmlOLStyle) do
      if S = TIpHtmlOLStyleNames[Result] then exit;
    if FlagErrors then
      ReportError(SHtmlInvType);
  end;
end;

procedure TIpHtmlParser.ParseOrderedList(AParent: TIpHtmlNode;
  EndToken: TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
var
  newList: TIpHtmlNodeOL;
begin
  newList := TIpHtmlNodeOL.Create(AParent);
  newList.ParseBaseProps(FOwner);
  newList.Start := ParseInteger(htmlAttrSTART, 1);
  newList.Compact := ParseBoolean(htmlAttrCOMPACT);
  newList.Style := ParseOLStyle(olArabic);
  
  NextToken;
  
  Inc(FListLevel);
  ParseListItems(
    newList, 
    EndToken, 
    EndTokens + [EndToken] - [IpHtmlTagP, IpHtmlTagLI]
  );
  Dec(FListLevel);
  
  EnsureClosure(EndToken, EndTokens);
end;

procedure TIpHtmlParser.ParseParagraph(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  newPara: TIpHtmlNodeP;
begin
  newPara := TIpHtmlNodeP.Create(AParent);
  newPara.ParseBaseProps(FOwner);
  newPara.Align := ParseAlignment;
  NextToken;
  ParseBodyText(newPara, EndTokens + [IpHtmlTagPend, IpHtmlTagP, IpHtmltagTABLE]);
  if FCurToken = IpHtmlTagPend then
    NextToken
  else
  if FCurToken in (EndTokens + [IpHtmlTagP, IpHtmltagTABLE]) then
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagPend);
end;

procedure TIpHtmlParser.ParsePhraseElement(AParent: TIpHtmlNode;
  StartToken, EndToken: TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
var
  curPhrase: TIpHtmlNodePhrase;
begin
  curPhrase := TIpHtmlNodePhrase.Create(AParent);
  case StartToken of
    IpHtmlTagEM :
      curPhrase.Style := hpsEM;
    IpHtmlTagSTRONG :
      curPhrase.Style := hpsSTRONG;
    IpHtmlTagDFN :
      curPhrase.Style := hpsDFN;
    IpHtmlTagCODE :
      curPhrase.Style := hpsCODE;
    IpHtmlTagSAMP :
      curPhrase.Style := hpsSAMP;
    IpHtmlTagKBD :
      curPhrase.Style := hpsKBD;
    IpHtmlTagVAR :
      curPhrase.Style := hpsVAR;
    IpHtmlTagCITE :
      curPhrase.Style := hpsCITE;
    IpHtmlTagABBR :
      curPhrase.Style := hpsABBR;
    IpHtmlTagACRONYM :
      curPhrase.Style := hpsACRONYM;
  end;
  curPhrase.ParseBaseProps(FOwner);
  NextToken; // this can not be before previous line, as NextToken resets properties
  ParseBodyText(curPhrase, [EndToken] + EndTokens);
  if FCurToken = EndToken then
    NextToken
  else
  if FCurToken in EndTokens then
    //
  else
    if FlagErrors then
      ReportExpectedToken(EndToken);
end;

function TIpHtmlParser.ParsePixels(const AttrNameSet: TIpHtmlAttributesSet;
  const ADefault: string): TIpHtmlPixels;
var
  S: string;
  n, Err: Integer;
begin
  Result := TIpHtmlPixels.Create;
  S := FindAttribute(AttrNameSet);
  if (S = '') then
    S := ADefault;
  
  if S = '' then
    Result.PixelsType := hpUndefined
  else begin
    Result.PixelsType := hpAbsolute;
    val(S, n, Err);
    Result.Value := n;
    if (Err <> 0) or (Result.Value < 0) then begin
      if FlagErrors then
        ReportError(SHtmlInvInt)
      else
        Result.Value := 0;
    end;
  end;
end;

procedure TIpHtmlParser.ParsePre(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curContainer: TIpHtmlNodePRE;
begin
  curContainer := TIpHtmlNodePRE.Create(AParent);
  curContainer.ParseBaseProps(FOwner);
  
  Inc(FInPre);
  NextToken;
  ParseBodyText(curContainer, EndTokens + [IpHtmlTagPREend]);
  Dec(FInPre);
  
  EnsureClosure(IpHtmlTagPREend, EndTokens);
end;

procedure TIpHtmlParser.ParseQ(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  BQ: TIpHtmlNodeQ;
begin
  BQ:= TIpHtmlNodeQ.Create(AParent);
  BQ.ParseBaseProps(FOwner);
  NextToken;
  ParseBodyText(BQ, EndTokens + [IpHtmlTagQend]);
  EnsureClosure(IpHtmlTagQend, EndTokens);
end;               

function TIpHtmlParser.ParseRelSize: TIpHtmlRelSize;
var
  S: string;
  i, Err: Integer;
begin
  Result := TIpHtmlRelSize.Create;
  Result.SizeType := hrsUnspecified;
  S := FindAttribute(htmlAttrSIZE);
  if Length(S) = 0 then
    Exit; {S := Default;}
  
  Result.Value := 0;
  if (Length(S) > 1) and (S[1] = '+') then begin
    Result.SizeType := hrsRelative;
    Delete(S, 1, 1);
  end else
  if (Length(S) > 1) and (S[1] = '-') then begin
    Result.SizeType := hrsRelative;
  end else
    Result.SizeType := hrsAbsolute;
  
  Val(S, i, Err);
  Result.Value := i;
  if Err <> 0 then
    if FlagErrors then
      ReportError(SHtmlInvInt);
end;

procedure TIpHtmlParser.ParseRight(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curContainer: TIpHtmlNodeDIV;
begin
  curContainer := TIpHtmlNodeDIV.Create(AParent);
  with curContainer do
    Align := haRight;
  NextToken;
  ParseBodyText(curContainer, EndTokens + [IpHtmlTagRIGHTend]);
  EnsureClosure(IpHtmlTagRIGHTend, EndTokens);
end;

function TIpHtmlParser.ParseRules(ADefault: TIpHtmlRules): TIpHtmlRules;
var
  S: string;
begin
  Result := hrNone;
  S := UpperCase(FindAttribute(htmlAttrRULES));
  if Length(S) = 0 then
  begin
    Result := ADefault;
    exit;
  end;
  case S[1] of
    'A': if S = 'ALL' then Result := hrAll;
    'C': if S = 'COLS' then Result := hrCols;
    'G': if S = 'GROUPS' then Result := hrGroups;
    'N': if S = 'NONE' then exit;
    'R': if S = 'ROWS' then Result := hrRows;
    else
      if FlagErrors then
        ReportError(SHtmlInvRule);
  end;
end;

procedure TIpHtmlParser.ParseScript(AParent: TIpHtmlNode;
  const EndTokens: TIpHtmlTokenSet);
begin
  TIpHtmlNodeSCRIPT.Create(AParent);
  NextToken;
  if FCurToken <> IpHtmlTagScriptEnd then
    repeat
      NextToken;
    until (FCurToken = IpHtmlTagSCRIPTend) or (FCurToken in EndTokens);
  EnsureClosure(IpHtmlTagSCRIPTend, EndTokens);
end;

function TIpHtmlParser.ParseShape: TIpHtmlMapShape;
var
  S: string;
begin
  Result := hmsDefault;
  S := UpperCase(FindAttribute(htmlAttrSHAPE));
  if Length(S) = 0 then 
    exit;
  case S[1] of
    'C': if S = 'CIRCLE' then Result := hmsCircle;
    'D': if S = 'DEFAULT' then exit;
    'P': if (S = 'POLY') or (S = 'POLYGON') then Result := hmsPoly;
    'R': if (S = 'RECT') then Result := hmsRect;
    else if FlagErrors then ReportError(SHtmlInvShape);
  end;
end;

procedure TIpHtmlParser.ParseSpan(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curSPAN: TIpHtmlNodeSPAN;
begin
  curSPAN:= TIpHtmlNodeSPAN.Create(AParent);
  with curSPAN do begin
    Align := ParseAlignment;
    ParseBaseProps(FOwner);
  end;
  NextToken;
  ParseBodyText(curSPAN, EndTokens + [IpHtmlTagSPANend]);
  EnsureClosure(IpHtmlTagSPANend, EndTokens);
end;

procedure TIpHtmlParser.ParseStyle(AParent: TIpHtmlNode);
var
  curStyle: TIpHtmlNodeSTYLE;
begin
  curStyle := TIpHtmlNodeSTYLE.Create(AParent);
  with curStyle do begin
    Media := FindAttribute(htmlAttrMEDIA);
    Title := FindAttribute(htmlAttrTITLE);
    Type_ := FindAttribute(htmlAttrTYPE);
  end;
  NextToken;
  if FCurToken <> IpHtmlTagSTYLEend then begin
    if (FCurToken = IpHtmlTagText) and (AnsiCompareText(curStyle.Type_, 'text/css')=0) then
      ParseStyleSheet(curStyle, GetTokenString);
    ParseText(curStyle, [IpHtmlTagSTYLEend]);
  end;
  if FCurToken = IpHtmlTagSTYLEend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagSTYLEend);
end;

procedure TIpHtmlParser.ParseStyleSheet(AParent: TIpHtmlNode; HRef: String);
var
  styleStream: TStream;
begin
  //debugln(['TIpHtml.ParseStyleSheet ',href,' ',Parent is TIpHtmlNodeHEAD,' ',DbgSName(FDataProvider)]);
  styleStream := nil;
  
  if AParent is TIpHtmlNodeHEAD then begin
    if FOwner.DataProvider <> nil then begin
      Href := FOwner.DataProvider.BuildURL(FCurURL, HRef);
      styleStream := FOwner.DataProvider.DoGetStream(HRef);
    end;
  end else
  if AParent is TIpHtmlNodeSTYLE then
    styleStream := TStringStream.Create(Href);
    
  if styleStream <> nil then
    with TCSSReader.Create(styleStream, FOwner.CSS) do begin
      ParseCSS;
      Free;
      styleStream.Free;
    end;
end;

procedure TIpHtmlParser.ParseTABLE(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curTable: TIpHtmlNodeTABLE;
  curCaption: TIpHtmlNodeCAPTION;
begin
  curTable := TIpHtmlNodeTABLE.Create(AParent);
  with curTable do begin
    Align := ParseImageAlignment(hiaBottom);
    Width := ParseHyperLength(htmlAttrWIDTH, '');
    Width.OnChange := @WidthChanged;
    Border := ParseInteger(htmlAttrBORDER, 0);
    CellSpacing := ParseInteger(htmlAttrCELLSPACING, 2);
    CellPadding := ParseInteger(htmlAttrCELLPADDING, 2);
    BgColor := ColorFromString(FindAttribute(htmlAttrBGCOLOR));
    ParseBaseProps(FOwner);
    Summary := FindAttribute(htmlAttrSUMMARY);
    Frame := ParseFrameProp(Frame);
    Rules := ParseRules(Rules);
  end;

  repeat
    NextToken;
  until FCurToken in
     [IpHtmlTagCAPTION, IpHtmlTagCOLGROUP, IpHtmlTagTHEAD, IpHtmlTagTFOOT,
      IpHtmlTagTBODY, IpHtmlTagTR, IpHtmlTagTABLEend, IpHtmlTagEOF];

  if FCurToken = IpHtmlTagCAPTION then begin
    curCaption := TIpHtmlNodeCAPTION.Create(CurTable);
    curCaption.Align := ParseVAlignment2;
    curCaption.ParseBaseProps(FOwner);
    ParseBodyText(curCaption, [IpHtmlTagCAPTIONend, IpHtmlTagTABLEend, IpHtmlTagTBODY]);
    if FCurToken in EndTokens then
    else
    if FCurToken = IpHtmlTagCAPTIONend then
      NextToken
    else
      if FlagErrors then
        ReportExpectedToken(IpHtmlTagCAPTIONend)
      else begin
        while not (FCurToken in EndTokens + [IpHtmlTagCAPTIONend]) do
          NextToken;
        if FCurToken = IpHtmlTagCAPTIONend then
          NextToken;
      end;
    curTable.FCaption := curCaption;
  end;
  ParseColGroup(curTable);
  SkipTextTokens;
  ParseTableBody(
    curTable, 
    EndTokens + [IpHtmlTagTABLEend] - [IpHtmlTagTR, IpHtmlTagP, IpHtmlTagPend, IpHTMLTagCENTERend,
       IpHtmlTagLEFTend, IpHtmlTagRIGHTend, IpHtmlTagBLINKend, IpHtmlTagBLOCKQUOTEend]
  );
  SkipTextTokens;
  EnsureClosure(IpHtmlTagTABLEend, EndTokens);
end;

procedure TIpHtmlParser.ParseTableBody(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curHead: TIpHtmlNodeTHEAD;
  curFoot: TIpHtmlNodeTFOOT;
  curBody: TIpHtmlNodeTBODY;
begin
  if FCurToken = IpHtmlTagTHEAD then begin
    curHead := TIpHtmlNodeTHEAD.Create(AParent);
    curHead.ParseBaseProps(FOwner);
    curHead.Align := ParseCellAlign(haLeft);
    curHead.VAlign := ParseVAlignment3;
    NextToken;
    ParseTableRows(
      curHead,
      EndTokens + [IpHtmlTagTFOOT, IpHtmlTagTBODY, IpHtmlTagTHEADend] 
                - [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD]
    );
    if FCurToken = IpHtmlTagTHEADend then
      NextToken;
  end;
  if FCurToken = IpHtmlTagTFOOT then begin
    curFoot := TIpHtmlNodeTFOOT.Create(AParent);
    curFoot.ParseBaseProps(FOwner);
    curFoot.Align := ParseCellAlign(haLeft);
    curFoot.VAlign := ParseVAlignment3;
    NextToken;
    ParseTableRows(
      curFoot,
      EndTokens + [IpHtmlTagTBODY, IpHtmlTagTFOOTend] 
                - [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD]
    );
    if FCurToken = IpHtmlTagTFOOTend then
      NextToken;
  end;
  while not (FCurToken in EndTokens) do begin
    case FCurToken of
      IpHtmlTagTBODY :
        begin
          curBody := TIpHtmlNodeTBODY.Create(AParent);
          curBody.ParseBaseProps(FOwner);
          curBody.Align := ParseCellAlign(haLeft);
          curBody.VAlign := ParseVAlignment3;
          NextToken;
          ParseTableRows(
            curBody,
            EndTokens + [IpHtmlTagTBODYend] 
                      - [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD, IpHtmlTagTRend]
          );
          if FCurToken = IpHtmlTagTBODYend then
            NextToken;
        end;
      IpHtmlTagTR :
        begin
          curBody := TIpHtmlNodeTBODY.Create(AParent);
          ParseTableRows(
            curBody,
            EndTokens - [IpHtmlTagTR, IpHtmlTagTH, IpHtmlTagTD]
          );
        end;
      else
        Exit;
    end;
  end;
end;

procedure TIpHtmlParser.ParseTableRow(AParent: TIpHtmlNode; 
  const EndTokens : TIpHtmlTokenSet);
var
  curHeader: TIpHtmlNodeTH;
  curTableCell: TIpHtmlNodeTD;
begin
  while not (FCurToken in EndTokens) do begin
    case FCurToken of
      IpHtmlTagTH :
        begin
          curHeader := TIpHtmlNodeTH.Create(AParent);
          with curHeader do begin
            NoWrap := ParseBoolean(htmlAttrNOWRAP);
            RowSpan := ParseInteger(htmlAttrROWSPAN, 1);
            ColSpan := ParseInteger(htmlAttrCOLSPAN, 1);
            ParseBaseProps(FOwner);
            Align := ParseCellAlign(haCenter{haDefault});
            VAlign := ParseVAlignment3;
            Width := ParseHyperLength(htmlAttrWIDTH, '');
            Width.OnChange := @DimChanged;
            Height := ParsePixels(htmlAttrHEIGHT, '');
              {ParseInteger(htmlAttrHEIGHT, -1);}
            Height.OnChange := @DimChanged;
            BgColor := ColorFromString(FindAttribute(htmlAttrBGCOLOR));
          end;
          NextToken;
          ParseBodyText(
            curHeader,
            EndTokens + [IpHtmlTagTH, IpHtmlTagTHend, IpHtmlTagTD]
          );
          if FCurToken in [IpHtmlTagTHend, IpHtmlTagTDend] then
            NextRealToken;
        end;
      IpHtmlTagTD :
        begin
          curTableCell := TIpHtmlNodeTD.Create(AParent);
          with curTableCell do begin
            NoWrap := ParseBoolean(htmlAttrNOWRAP);
            RowSpan := ParseInteger(htmlAttrROWSPAN, 1);
            ColSpan := ParseInteger(htmlAttrCOLSPAN, 1);
            ParseBaseProps(FOwner);
            Align := ParseCellAlign(haDefault);
            VAlign := ParseVAlignment3;
            Width := ParseHyperLength(htmlAttrWIDTH, '');
            Width.OnChange := @DimChanged;
            Height := ParsePixels(htmlAttrHEIGHT, '');
              {ParseInteger(htmlAttrHEIGHT, -1);}
            Height.OnChange := @DimChanged;
            BgColor := ColorFromString(FindAttribute(htmlAttrBGCOLOR));
          end;
          NextToken;
          ParseBodyText(curTableCell, EndTokens + [IpHtmlTagTD, IpHtmlTagTDend]);
          if FCurToken = IpHtmlTagTDend then
            NextRealToken;
        end;
      else
        NextToken;
    end;
  end;
end;

procedure TIpHtmlParser.ParseTableRows(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);

  procedure FixupPercentages(CurRow: TIpHtmlNodeTR);
  var
    i, Pt, P0: Integer;
  begin
    Pt := 0;
    P0 := 0;
    for i := 0 to CurRow.ChildCount - 1 do
      if CurRow.ChildNode[i] is TIpHtmlNodeTableHeaderOrCell then
        case TIpHtmlNodeTableHeaderOrCell(CurRow.ChildNode[i]).Width.LengthType of
          hlUndefined :
            Inc(P0);
          hlPercent :
            Inc(Pt, TIpHtmlNodeTableHeaderOrCell(CurRow.ChildNode[i]).Width.LengthValue);
        end;
    if (Pt > 0) and (Pt < 100) and (P0 > 0) then begin
      Pt := (100 - Pt) div P0;
      for i := 0 to CurRow.ChildCount - 1 do
        if CurRow.ChildNode[i] is TIpHtmlNodeTableHeaderOrCell then
          with TIpHtmlNodeTableHeaderOrCell(CurRow.ChildNode[i]).Width do
            if LengthType = hlUndefined then begin
              LengthType := hlPercent;
              LengthValue := Pt;
            end;
    end;
  end;

var
  curRow: TIpHtmlNodeTR;
begin
  curRow := nil;
  while not (FCurToken in EndTokens) do
    case FCurToken of
      IpHtmlTagTR:
        begin
          if curRow <> nil then
            FixupPercentages(curRow);
          curRow := TIpHtmlNodeTR.Create(AParent);
          curRow.ParseBaseProps(FOwner);
          curRow.BgColor := ColorFromString(FindAttribute(htmlAttrBGCOLOR));
          curRow.Align := ParseAlignment;
          curRow.VAlign := ParseVAlignment;
          curRow.LoadAndApplyCSSProps;
          NextRealToken;
          ParseTableRow(
            curRow,
            EndTokens + [IpHtmlTagTRend, IpHtmlTagTR] - [IpHtmlTagTH, IpHtmlTagTD]
          );
          while FCurToken = IpHtmlTagTRend do
            NextToken;
        end;
      IpHtmlTagTH,
      IpHtmlTagTD:
        begin
          if curRow <> nil then
            FixupPercentages(CurRow);
          curRow := TIpHtmlNodeTR.Create(AParent);
          ParseTableRow(
            curRow,
            EndTokens + [IpHtmlTagTR] - [IpHtmlTagTH, IpHtmlTagTD]
          );
        end;
      else
        NextToken;
    end;
  
  if curRow <> nil then
    FixupPercentages(curRow);
end;

procedure TIpHtmlParser.ParseText(AParent: TIpHtmlNode; 
  const EndTokens: TIpHtmlTokenSet);
var
  curContainer: TIpHtmlNodeText;
begin
  while not (FCurToken in EndTokens) do begin
    case FCurToken of
      IpHtmlTagEof :
        Exit;
      IpHtmlTagText :
        begin
          curContainer := TIpHtmlNodeText.Create(AParent);
//          if curContainer=nil then ;
          if curContainer <> nil then
          begin
            curContainer.EscapedText := GetTokenString;
            NextToken;
          end;
        end;
      else
        NextToken;
    end;
  end;
end;

procedure TIpHtmlParser.ParseTitle(AParent: TIpHtmlNode);
var
  B: PAnsiChar;
begin
  FTitleNode := TIpHtmlNodeTITLE.Create(AParent);
  NextToken;
  if FCurToken = IpHtmlTagText then begin
    GetMem(B, Length(GetTokenString) + 1);
    try
      TrimFormatting(EscapeToAnsi(GetTokenString), B);
      FTitleNode.Title := B;
    finally
      FreeMem(B);
    end;
    NextToken;
  end;
  if FCurToken = IpHtmlTagTITLEend then
    NextToken
  else
    if FlagErrors then
      ReportExpectedToken(IpHtmlTagTITLEend);
end;

function TIpHtmlParser.ParseULStyle(ADefault: TIpHtmlULType): TIpHtmlULType;
var
  S: string;
begin
  Result := ADefault;
  S := UpperCase(FindAttribute(htmlAttrTYPE));
  if S <> '' then
    case S[1] of
      'C': if S = 'CIRCLE' then Result := ulCircle;
      'D': if S = 'DISC' then Result := ulDisc;
      'S': if S = 'SQUARE' then Result := ulSquare;
      else
        if FlagErrors then
          ReportError(SHtmlInvType);
    end;
end;

procedure TIpHtmlParser.ParseUnorderedList(AParent: TIpHtmlNode;
  EndToken: TIpHtmlToken; const EndTokens: TIpHtmlTokenSet);
var
  newList: TIpHtmlNodeList;
begin
  case Pred(EndToken) of
    IpHtmlTagDIR: 
      newList := TIpHtmlNodeDIR.Create(AParent);
    IpHtmlTagMENU: 
      newList := TIpHtmlNodeMENU.Create(AParent);
    else 
      {IpHtmlTagUL : }
      newList := TIpHtmlNodeUL.Create(AParent);
  end;
  newList.ParseBaseProps(FOwner); 
  case FListLevel of
    0 : newList.ListType := ParseULStyle(ulDisc);
    1 : newList.ListType := ParseULStyle(ulCircle);
    else
        newList.ListType := ParseULStyle(ulSquare);
  end;
  newList.Compact := ParseBoolean(htmlAttrCOMPACT);
    
  NextToken;
  
  Inc(FListLevel);
  ParseListItems(
    newList,
    EndToken, 
    EndTokens + [EndToken] - [IpHtmlTagP, IpHtmlTagLI]
  );
  Dec(FListLevel);
  
  EnsureClosure(EndToken, EndTokens);
end;

function TIpHtmlParser.ParseVAlignment: TIpHtmlVAlign;
var
  S: string;
begin
  Result := hvaMiddle;
  S := UpperCase(FindAttribute(htmlAttrVALIGN));
  if Length(S) = 0 then 
    exit;
  case S[1] of
    'B': if S = 'BOTTOM' then Result := hvaBottom;
    'C','M': if (S = 'MIDDLE') or (S = 'CENTER') then exit;
    'T': if S = 'TOP' then Result := hvaTop;
    else
      if FlagErrors then
        ReportError(SHtmlInvAlign);
  end;
end;

function TIpHtmlParser.ParseVAlignment2: TIpHtmlVAlignment2;
var
  S: string;
begin
  Result := hva2Top;
  S := UpperCase(FindAttribute(htmlAttrALIGN));
  if Length(S) = 0 then 
    exit;
  case S[1] of
    'B': if S = 'BOTTOM' then Result := hva2Bottom;
    'L': if S = 'LEFT' then Result := hva2Left;
    'R': if S = 'RIGHT' then Result := hva2Right;
    'T': if (S = 'TOP') then exit;
    else
      if FlagErrors then
        ReportError(SHtmlInvAlign);
  end;
end;

function TIpHtmlParser.ParseVAlignment3: TIpHtmlVAlign3;
var
  S : string;
begin
  Result := hva3Middle;
  S := UpperCase(FindAttribute(htmlAttrVALIGN));
  if Length(S) = 0 then
  begin
    Result := hva3Default;
    exit;
  end;
  
  case S[1] of
    'B': 
      if S = 'BOTTOM' then 
        Result := hva3Bottom
      else if S = 'BASELINE' then 
        Result := hva3Baseline;
    'C','M': 
      if (S = 'MIDDLE') or (S = 'CENTER') then 
        exit;
    'T': 
      if (S = 'TOP') then 
        Result := hva3Top;
    else
      if FlagErrors then
        ReportError(SHtmlInvAlign);
  end;
end;

procedure TIpHtmlParser.PutChar(Ch: AnsiChar);
begin
  if (FCharSP >= SizeOf(FCharStack)) then
    raise EIpHtmlException.Create(SHtmlCharStackOverfl);
  FCharStack[FCharSP] := Ch;
  Inc(FCharSP);
end;

procedure TIpHtmlParser.PutToken(AToken: TIpHtmlToken);
begin
  if FHaveToken then
    raise EIpHtmlException.Create(SHtmlTokenStackOverfl);
  FTokenBuffer := AToken;
  FHaveToken := True;
end;

procedure TIpHtmlParser.ReportError(const AErrorMsg: string);
begin
  raise Exception.CreateFmt(SHtmlLineError, [AErrorMsg, FLineNumber, FLineOffset]);
end;

procedure TIpHtmlParser.ReportExpectedError(const AErrorMsg: string);
begin
  ReportError(AErrorMsg + SHtmlExp);
end;

procedure TIpHtmlParser.ReportExpectedToken(const AToken: TIpHtmlToken);
var
  n: integer;
begin
  for n := Low(IpHtmlTokens) to High(IpHtmlTokens) do
    if IpHtmlTokens[n].tk = AToken then
    begin
      ReportExpectedError(IpHtmlTokens[n].pc);
      break;
    end;
end;

procedure TIpHtmlParser.SkipTextTokens;
begin
  while FCurToken = IpHtmlTagText do
    NextToken;
end;

end.

