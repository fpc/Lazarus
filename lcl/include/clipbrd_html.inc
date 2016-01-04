{%MainUnit ../clipbrd.pp}

{******************************************************************************
                    Clipboard and HTML format
 ******************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

type
  TBom = (bomUtf8, bomUtf16BE, bomUtf16LE, bomUndefined);

const
  HTML_FORMAT = {$IFDEF WINDOWS}'HTML Format'{$ELSE}'text/html'{$ENDIF};

{$IFDEF WINDOWS}
const
  {See: https://msdn.microsoft.com/en-us/library/aa767917%28v=vs.85%29.aspx
    HtmlClipHeader  =  'Version:0.9' + LineEnding +
                       'StartHTML:00000000' + LineEnding +
                       'EndHTML:00000000' + LineEnding +
                       'StartFragment:00000000' + LineEnding +
                       'EndFragment:00000000';  }

  SHtmlClipHeaderFmt = 'Version:0.9' + LineEnding +
                       'StartHTML:%.8d' + LineEnding +
                       'EndHTML:%.8d' + LineEnding +
                       'StartFragment:%.8d' + LineEnding +
                       'EndFragment:%.8d' + LineEnding;

  SStartFragment     = '<!--StartFragment-->';
  SEndFragment       = '<!--EndFragment-->';

  SStartHTMLKey      = 'StartHTML:';
  SEndHTMLKey        = 'EndHTML:';
  SStartFragmentKey  = 'StartFragment:';
  SEndFragmentKey    = 'EndFragment:';
{$ENDIF}

type
  THTMLTagFinder = class
  private
    FParser: THTMLParser;
    FStartTag: String;
    FEndTag: String;
    FStartTagPos: Integer;
    FEndTagPos: Integer;
    procedure FoundTagHandler(NoCaseTag, ActualTag: String);
  public
    constructor Create(const HTML,ATag: String);
    destructor Destroy; override;
    property StartTagPos: Integer read FStartTagPos;
    property EndTagPos: Integer read FEndTagPos;
  end;

{ Finds the (zero-based) positions of the start and end tag in the HTML string.
  Note: The tag is assumed to be without "<" and ">". }
constructor THTMLTagFinder.Create(const HTML, ATag: String);
begin
  FStartTagPos := -1;
  FEndTagPos := -1;
  FStartTag := '<' + Uppercase(ATag);
  FEndTag := '</' + Uppercase(ATag);
  FParser := THTMLParser.Create(HTML);
  FParser.OnFoundTag := @FoundTagHandler;
  FParser.Exec;
end;

destructor THTMLTagFinder.Destroy;
begin
  FParser.Free;
  inherited;
end;

procedure THTMLTagFinder.FoundTagHandler(NoCaseTag, ActualTag: String);
begin
  if (NoCaseTag = FStartTag + '>') or (pos(FStartTag+' ', NoCaseTag) = 1) then
    // Position immediately after the start tag
    FStartTagPos := FParser.CurrentPos + 1
  else
  if (NoCaseTag = FEndTag + '>') or (pos(FEndTag+' ', NoCaseTag) = 1) then
    // Position immediately before the end tag
    FEndTagPos := FParser.CurrentPos - Length(ActualTag) + 1;
end;

procedure MaybeInsertHtmlAndBodyTags(var HTML: String; out IsValid: Boolean);
var
  tagFinder: THTMLTagFinder;
  HS, HE, BS, BE: Boolean;
  pHS, pHE, pBS, pBE: Integer;
begin
  tagFinder := THTMLTagFinder.Create(HTML, 'BODY');
  try
    pBS := tagFinder.StartTagPos ;
    pBE := tagFinder.EndTagPos;
    BS := (pBS > -1);
    BE := (pBE > -1);
  finally
    tagFinder.Free;
  end;

  tagFinder := THTMLTagFinder.Create(HTML, 'HTML');
  try
    pHS := tagFinder.StartTagPos;
    pHE := tagFinder.EndTagPos;
    HS := (pHS > -1);
    HE := (pHE > -1);
  finally
    tagFinder.Free;
  end;
  IsValid := ((HS and HE) or (not HS and not HE)) and
             ((BS and BE) or (not BS and not BE));

  //Do not fix malformed HTML e.i. unmatched <html> or <body> tags
  if not IsValid then
    exit;
  if not BS then
  begin
    if HS then
    begin
      Insert('<body>',HTML,pHS+1);
      Insert('</body>',HTML,pHE+1+Length('<body>'));
    end
    else
      HTML := '<body>' + HTML + '</body>';
  end;
  if not HS then HTML := '<html>' + HTML + '</html>';
end;

{$IFDEF WINDOWS}
function InsertClipHeader(HTML: String): String;
var
  hdr, s1, s2, s3: String;
  htmlStart, htmlEnd, fragStart, fragEnd: Integer;
  tagFinder: THTMLTagFinder;
begin
  Result := '';

  // Find position of <BODY> and </BODY> tags
  tagFinder := THTMLTagFinder.Create(HTML, 'BODY');
  try
    fragStart := tagFinder.StartTagPos;
    fragEnd := tagFinder.EndTagPos;
    //this should not happen, since we added them in SetAsHtml
    if (fragStart = -1) or (fragEnd = -1) then
      exit;
  finally
    tagFinder.Free;
  end;

  // Split input string into three parts
  s1 := Copy(HTML, 1, fragStart);                     // before <body> tag
  s2 := Copy(HTML, fragStart+1, fragEnd - fragStart); // between <body> and </body>
  s3 := Copy(HTML, fragEnd+1, MaxInt);                // after </body>

  // Add dummy values to the header, just to get its length
  hdr := Format(SHtmlClipHeaderFmt, [0,0,0,0]);
  htmlStart := Length(hdr);

  // Calculate stream positions after adding header and fragment comments
  fragStart := fragStart + htmlStart + Length(SStartFragment);
  fragEnd := fragEnd + htmlStart + Length(SStartFragment);
  htmlEnd := Length(HTML) + htmlStart + Length(SStartFragment) + Length(SEndFragment);

  // Combine parts
  Result :=
    Format(SHtmlClipHeaderFmt, [htmlStart, htmlEnd, fragStart, fragEnd]) +
    s1 +
    SStartFragment +
    s2 +
    SEndFragment +
    s3;
end;
{$ENDIF}

{ In Windows, the clipboard returns a string like:
    Version:0.9
    StartHTML:00000165
    EndHTML:00000339
    StartFragment:00000199
    EndFragment:00000303
    SourceURL:http://wiki.lazarus.freepascal.org/Clipboard#HTML_source
    <html><body>
    <!--StartFragment-->Write html source to clipboard. This can be done with AddFormat either using TStream or memory Buffer.
    <!--EndFragment-->
    </body>
    </html>

  ExtractHtmlFragmentFromClipboardHtml returns the part of the input html
  string between the StartFragment and EndFragment positions.

  Note: the positions are in bytes in the stream, not codepoints!

  In non-Windows systems the part after the <BODY> and before the </BODY> tag
  is extracted.
}
function ExtractHtmlFragmentFromClipboardHtml(Html: Utf8String): Utf8String;
var
  {$IFDEF WINDOWS}
  P, PFragStart, PFragEnd: SizeInt;
  {$ELSE}
  tagfinder: THTMLTagFinder;
  {$ENDIF}
  FragStart, FragEnd: Integer;
  s: String;
begin
 {$IFDEF WINDOWS}
  Result := '';

  P := Pos(SStartHTMLKey, Html);               // "StartHTML:"
  if (P = 0) then Exit;

  P := Pos(SEndHTMLKey, Html);                 // "EndHTML:"
  if (P = 0) then Exit;

  PFragStart := Pos(SStartFragmentKey, Html);  // "StartFragment:"
  if (PFragStart = 0) then Exit;
  PFragStart := PFragStart + Length(SStartFragmentKey);
  P := PFragStart;
  while (P < Length(Html)) and (not (Html[P] in [#13,#10])) do Inc(P);
  if not (Html[P] in [#13,#10]) then Exit;
  s := Copy(Html, PFragStart, P - PFragStart);
  if not TryStrToInt(s, FragStart) then Exit;

  PFragEnd := Pos(SEndFragmentKey, Html);      // "EndFragment:"
  if (PFragEnd = 0) then Exit;
  PFragEnd := PFragEnd + Length(SEndFragmentKey);
  P := PFragEnd;
  while (P < Length(Html)) and (not (Html[P] in [#13,#10])) do Inc(P);
  if not (Html[P] in [#13,#10]) then Exit;
  s := Copy(Html, PFragEnd, P - PFragEnd);
  if not TryStrToInt(s, FragEnd) then Exit;
 {$ELSE}
  tagfinder := THTMLTagFinder.Create(HTML, 'BODY');
  try
    FragStart := tagFinder.StartTagPos;
    FragEnd := tagFinder.EndTagPos;
    if (FragStart = -1) or (FragEnd = -1) then
      exit;
  finally
    tagFinder.Free;
  end;
 {$ENDIF}

  Result := copy(Html, FragStart+1, FragEnd - FragStart);

  // Cleanup
  While (Result <> '') and (Result[1] in [' ', #13, #10]) do
    Delete(Result, 1, 1);
  While (Result <> '') and (Result[Length(Result)] in [' ', #13, #10]) do
    Delete(Result, Length(Result), 1);
end;

{$IFDEF WINDOWS}
{ ExtractHtmlFromClipboardHtml removes the header from the input html string.}
function ExtractHtmlFromClipboardHtml(Html: Utf8String): Utf8String;
var
  P, PHtmlStart, PHtmlEnd: SizeInt;
  HtmlStart, HtmlEnd: Integer;
  s: String;
begin
  Result := '';

  P := Pos(SStartFragmentKey, Html);              // "StartFragment:"
  if (P = 0) then Exit;

  P := Pos(SEndFragmentKey, Html);                // "EndFragment:"
  if (P = 0) then Exit;

  PHtmlStart := Pos(SStartHTMLKey, Html);         // "StartHTML:"
  if (PHtmlStart = 0) then Exit;
  PHtmlStart := PHtmlStart + Length(SStartHTMLKey);
  P := PHtmlStart;
  while (P < Length(Html)) and (not (Html[P] in [#13,#10])) do Inc(P);
  if not (Html[P] in [#13,#10]) then Exit;
  s := Copy(Html, PHtmlStart, P - PHtmlStart);
  if not TryStrToInt(s, HtmlStart) then Exit;

  PHtmlEnd := Pos(SEndHTMLKey, Html);             // "EndHTML:"
  if (PHtmlEnd = 0) then Exit;
  PHtmlEnd := PHtmlEnd + Length(SEndHTMLKey);
  P := PHtmlEnd;
  while (P < Length(Html)) and (not (Html[P] in [#13,#10])) do Inc(P);
  if not (Html[P] in [#13,#10]) then Exit;
  s := Copy(Html, PHtmlEnd, P - PHtmlEnd);
  if not TryStrToInt(s, HtmlEnd) then Exit;

  Result := copy(Html, HtmlStart+1, HtmlEnd - HtmlStart);
end;
{$ENDIF}

function GetBOMFromStream(Stream: TStream): TBom;
const
  Buf: Array[1..3] of Byte = (0,0,0);
begin
  Result := bomUndefined;
  Stream.Position := 0;
  if (Stream.Size > 2) then
    Stream.Read(Buf[1],3)
  else if (Stream.Size > 1) then
    Stream.Read(Buf[1],2);
  if ((Buf[1]=$FE) and (Buf[2]=$FF)) then
    Result := bomUtf16BE
  else if ((Buf[1]=$FF) and (Buf[2]=$FE)) then
    Result := bomUtf16LE
  else if ((Buf[1]=$EF) and (Buf[2]=$BB) and(Buf[3]=$BF)) then
    Result := bomUtf8;
end;

var
  cfHTML: TClipboardFormat = 0;

function CF_HTML: TClipboardFormat;
begin
  if cfHTML = 0 then
    cfHTML := RegisterClipboardFormat(HTML_FORMAT);
  Result := cfHTML;
end;

