{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Juha Manninen

  Abstract:
    Render HTML into plain text by using indentation, newlines and stripping tags.
}
unit HTML2TextRender;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LConvEncoding;

type

  { THTML2TextRenderer
    renders HTML into plain text as well as possible }

  THTML2TextRenderer = class
  private
    fHTML, fOutput: string;
    fMaxLines: integer;
    fInHeader: Boolean;
    fPendingSpace: Boolean;
    fPendingNewLineCnt: Integer;
    fCurTag: String;
    fIndent: integer;
    fLineCnt, fHtmlLen: Integer;
    p: Integer;
    procedure AddNewLine;
    procedure AddOneNewLine;
    function AddOutput(aText: String): Boolean;
    function HtmlTag: Boolean;
    procedure HtmlEntity;
    procedure Reset;
  public
    constructor Create(aHTML: string);
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    function Render(aMaxLines: integer = MaxInt): string;
  end;

implementation

{ THTML2TextRenderer }

constructor THTML2TextRenderer.Create(aHTML: string);
begin
  fHTML:=aHTML;
  // remove UTF8 BOM
  if copy(fHTML,1,3)=UTF8BOM then
    delete(fHTML,1,3);
end;

constructor THTML2TextRenderer.Create(Stream: TStream);
var
  s: string;
begin
  SetLength(s,Stream.Size);
  if s<>'' then
    Stream.Read(s[1],length(s));
  Create(s);
end;

destructor THTML2TextRenderer.Destroy;
begin
  inherited Destroy;
end;

procedure THTML2TextRenderer.Reset;
begin
  fOutput:='';
  fInHeader:=False;
  fPendingSpace:=False;
  fPendingNewLineCnt:=0;
  fLineCnt:=1;
end;

procedure THTML2TextRenderer.AddNewLine;
// set a pending linebreak to be added later
begin
  if (fOutput<>'') and not fInHeader then
    Inc(fPendingNewLineCnt);
end;

procedure THTML2TextRenderer.AddOneNewLine;
begin
  if (fOutput<>'') and not fInHeader then
    fPendingNewLineCnt:=1;
end;

function THTML2TextRenderer.AddOutput(aText: String): Boolean;
var
  i: Integer;
begin
  Result:=True;
  if fPendingSpace and (fPendingNewLineCnt=0) then
    fOutput:=fOutput+' ';      // Don't add space at end of line (before newline)
  fPendingSpace:=False;
  for i:=0 to fPendingNewLineCnt-1 do
  begin
    fOutput:=fOutput+LineEnding;
    Inc(fLineCnt);
    // Return False if max # of lines exceeded.
    if fLineCnt>fMaxLines then
    begin
      fOutput:=fOutput+LineEnding+'...';
      Exit(False);
    end;
  end;
  if fPendingNewLineCnt>0 then
  begin
    fOutput:=fOutput+StringOfChar(' ',fIndent*2);
    fPendingNewLineCnt:=0;
  end;
  fOutput:=fOutput+aText;
end;

function THTML2TextRenderer.HtmlTag: Boolean;
// separate a html tag and use it for layout. '<' is already found here.
//  ToDo: parse <div class="title"> and use it.
var
  Start: Integer;
begin
  // first separate a html tag.
  inc(p);
  Start:=p;
  if (p<=fHtmlLen) and (fHTML[p]='/') then
    inc(p);
  while (p<=fHtmlLen) and not (fHTML[p] in [' ','>','"','/',#9,#10,#13]) do
    inc(p);
  fCurTag:=UpperCase(copy(fHTML,Start,p-Start));
  while p<=fHtmlLen do
  begin
    if fHTML[p]='"' then begin       // skip attribute "value" inside tag
      inc(p);
      while (p<=fHtmlLen) and (fHTML[p]<>'"') do
        inc(p);
      if p>fHtmlLen then break;
    end;
    inc(p);
    if (fHTML[p-1]='>') then break;  // end of tag
  end;

  // adjust layout based on html tag, then remove it
  Result:=True;
  case fCurTag of
    'HTML':
        fInHeader:=True;             // it's a whole page
    'BODY':
        Reset;                 // start of body => ignore header and all its data
    'P', '/P', 'BR', '/UL':
        AddNewLine;
    'DIV': begin
        AddOneNewLine;
        Inc(fIndent);
      end;
    '/DIV': begin
        AddOneNewLine;
        Dec(fIndent);
      end;
    'LI':
      begin
        Inc(fIndent);
        // Don't leave empty lines before list item (not sure if this is good)
        AddOneNewLine;
        Result:=AddOutput('* ');     // Can return False if MaxLines was exceeded
      end;
    '/LI':
        Dec(fIndent);
    'A', '/A':                       // Link
        Result:=AddOutput(' _ ');
    'HR':
      begin
        AddOneNewLine;
        Result:=AddOutput('----------');
        //AddOneNewLine;
      end;
  end;
end;

procedure THTML2TextRenderer.HtmlEntity;
// entities: &lt; &gt; &amp; &nbsp;
begin
  if (p+2<fHtmlLen) and (fHTML[p+1]='l') and (fHTML[p+2]='t') and (fHTML[p+3]=';') then
  begin
    Inc(p,4);
    AddOutput('<');
  end else
  if (p+2<fHtmlLen) and (fHTML[p+1]='g') and (fHTML[p+2]='t') and (fHTML[p+3]=';') then
  begin
    Inc(p,4);
    AddOutput('>');
  end else
  if (p+4<fHtmlLen) and (fHTML[p+1]='n') and (fHTML[p+2]='b') and (fHTML[p+3]='s') and (fHTML[p+4]='p') and (fHTML[p+5]=';') then
  begin
    Inc(p,6);
    AddOutput(' ');
  end else
  if (p+3<fHtmlLen) and (fHTML[p+1]='a') and (fHTML[p+2]='m') and (fHTML[p+3]='p') and (fHTML[p+4]=';') then
  begin
    Inc(p,5);
    AddOutput('&');
  end;
end;

function THTML2TextRenderer.Render(aMaxLines: integer): string;
// Parse the HTML and render to plain text.
// Output is limited to aMaxLines lines.
var
  OkToGo: Boolean;
begin
  fMaxLines:=aMaxLines;
  Reset;
  p:=1;
  OkToGo:=True;
  fHtmlLen:=length(fHTML);
  while (p<=fHtmlLen) and OkToGo do
  begin
    case fHTML[p] of
      '<': OkToGo:=HtmlTag;     // Can return False if MaxLines was exceeded.
      '&': HtmlEntity;
      ' ',#9,#10,#13:           // WhiteSpace
        begin
          fPendingSpace:=True;
          inc(p);
        end;
      else
        begin
          AddOutput(fHTML[p]);  // Add text verbatim.
          inc(p);
        end;
    end;
  end;
  Result:=fOutput;
end;

end.

