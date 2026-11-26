{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Code highlighting hook for markdown renderers.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.CodeHighlight;

{$mode ObjFPC}{$H+}

interface

uses
  Markdown.Utils;

Type
  // Kind of a token in highlighted source code.
  TMarkdownCodeTokenKind = (ctkDefault, ctkKeyword, ctkComment, ctkDirective, ctkNumber, ctkString, ctkEscape,
                            ctkSymbol, ctkOperator, ctkAttribute, ctkKey, ctkSection, ctkError);

  // One token of highlighted source code.
  TMarkdownCodeToken = record
    Text : TMarkdownString;
    Kind : TMarkdownCodeTokenKind;
  end;
  TMarkdownCodeTokenArray = array of TMarkdownCodeToken;
  TMarkdownCodeTokenLines = array of TMarkdownCodeTokenArray;

  { TMarkdownCodeHighlighter }

  // Splits source code into tokens for the renderers.
  TMarkdownCodeHighlighter = class(TObject)
  private
    class var _DefaultHighlighter : TMarkdownCodeHighlighter;
  public
    class destructor Done;
    // Install aHighlighter as default highlighter and free the previous one. Nil removes the default highlighter.
    class procedure SetDefaultHighlighter(aHighlighter : TMarkdownCodeHighlighter);
    // Split aSource, written in language aLang, into tokens. Returns False when aLang is not supported.
    function Highlight(const aLang, aSource : TMarkdownString; out aTokens : TMarkdownCodeTokenArray) : Boolean; virtual; abstract;
    // Highlighter for renderers without highlighter of their own. Owned by this class.
    class property DefaultHighlighter : TMarkdownCodeHighlighter read _DefaultHighlighter;
  end;

const
  CodeTokenKindNames : Array[TMarkdownCodeTokenKind] of TMarkdownString = (
    'default', 'keyword', 'comment', 'directive', 'number', 'string', 'escape',
    'symbol', 'operator', 'attribute', 'key', 'section', 'error');

// Split aTokens into lines at #10, #13 and #13#10, without the line breaks.
// Every line break ends a line; text after the last line break forms one more line.
function SplitCodeTokenLines(const aTokens : TMarkdownCodeTokenArray) : TMarkdownCodeTokenLines;

implementation

// Split aTokens into lines at #10, #13 and #13#10, without the line breaks.
function SplitCodeTokenLines(const aTokens : TMarkdownCodeTokenArray) : TMarkdownCodeTokenLines;

var
  lLine : TMarkdownCodeTokenArray;
  lLineCount, lTokenCount : Integer;

  procedure AddPart(const aText : TMarkdownString; aKind : TMarkdownCodeTokenKind);

  begin
    if aText='' then
      Exit;
    if lTokenCount=Length(lLine) then
      SetLength(lLine,lTokenCount*2+4);
    lLine[lTokenCount].Text:=aText;
    lLine[lTokenCount].Kind:=aKind;
    Inc(lTokenCount);
  end;

  procedure EndLine;

  begin
    SetLength(lLine,lTokenCount);
    if lLineCount=Length(Result) then
      SetLength(Result,lLineCount*2+4);
    Result[lLineCount]:=lLine;
    Inc(lLineCount);
    lLine:=[];
    lTokenCount:=0;
  end;

var
  lToken : TMarkdownCodeToken;
  I, lStart, lLen : Integer;
  lAfterCR : Boolean;
  C : TMarkdownChar;

begin
  Result:=[];
  lLine:=[];
  lLineCount:=0;
  lTokenCount:=0;
  lAfterCR:=False;
  for lToken in aTokens do
    begin
    lLen:=Length(lToken.Text);
    lStart:=1;
    for I:=1 to lLen do
      begin
      C:=lToken.Text[I];
      if (C=#10) and lAfterCR then
        lStart:=I+1
      else if C in [#10,#13] then
        begin
        AddPart(Copy(lToken.Text,lStart,I-lStart),lToken.Kind);
        EndLine;
        lStart:=I+1;
        end;
      lAfterCR:=(C=#13);
      end;
    AddPart(Copy(lToken.Text,lStart,lLen-lStart+1),lToken.Kind);
    end;
  if lTokenCount>0 then
    EndLine;
  SetLength(Result,lLineCount);
end;


{ TMarkdownCodeHighlighter }

class destructor TMarkdownCodeHighlighter.Done;

begin
  SetDefaultHighlighter(Nil);
end;


class procedure TMarkdownCodeHighlighter.SetDefaultHighlighter(aHighlighter: TMarkdownCodeHighlighter);

begin
  if _DefaultHighlighter=aHighlighter then
    Exit;
  _DefaultHighlighter.Free;
  _DefaultHighlighter:=aHighlighter;
end;


end.
