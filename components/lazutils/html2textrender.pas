{
 *****************************************************************************
  This file is part of LazUtils.

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Juha Manninen

  Abstract:
    Render HTML into plain text by stripping tags and
     using indentation, newlines and extra characters including Unicode Emojis.
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
    fMaxLines, fMaxLineLen: Integer;
    fLineEndMark: String; // End of line, by default standard LineEnding
    fTitleMark: String; // Text at start/end of title text: <div class="title">...</div>
    fHorzLine: String; // Text for <hr> tag
    fLinkBegin: String; // Text before link, <a href="...">
    fLinkEnd: String; // Text after link
    fListItemMark: String; // Text for <li> items
    fMoreMark: String; // Text to add if too many lines
    fInHeader, fInDivTitle: Boolean;
    fPendingSpace: Boolean;
    fPendingNewLineCnt: Integer;
    fIndentStep: integer; // Increment (in spaces) for each nested HTML level
    fIndent, fLineLen: Integer;
    fLineCnt, fHtmlLen: Integer;
    p: Integer;
    procedure AddNewLine;
    procedure AddOneNewLine;
    function AddOutput(const aText: String): Boolean; overload;
    function HtmlTag: Boolean;
    function HtmlEntity: Boolean;
    procedure Reset;
    function SplitLongLine: Boolean;
  public
    constructor Create(const aHTML: string);
    constructor Create(const Stream: TStream);
    destructor Destroy; override;
    function Render(aMaxLines: integer = MaxInt): string;
  public
    property LineEndMark: String read fLineEndMark write fLineEndMark;
    property TitleMark: String read fTitleMark write fTitleMark;
    property HorzLineMark: String read fHorzLine write fHorzLine;
    property LinkBeginMark: String read fLinkBegin write fLinkBegin;
    property LinkEndMark: String read fLinkEnd write fLinkEnd;
    property ListItemMark: String read fListItemMark write fListItemMark;
    property MoreMark: String read fMoreMark write fMoreMark;
    property MaxLineLen: Integer read fMaxLineLen write fMaxLineLen;
    property IndentStep: Integer read fIndentStep write fIndentStep;
  end;

  function RenderHTML2Text(const AHTML: String): String;


implementation

function RenderHTML2Text(const AHTML: String): String;
begin
  with THTML2TextRenderer.Create(AHTML) do
    try
      Result:=Render;
    finally
      Free;
    end;
end;

{ THTML2TextRenderer }

constructor THTML2TextRenderer.Create(const aHTML: string);
begin
  fHTML:=aHTML;
  // remove UTF8 BOM
  if copy(fHTML,1,3)=UTF8BOM then
    delete(fHTML,1,3);
  // These can be changed by user later.
  fLineEndMark:=LineEnding;
  //fTitleMark:='🔹';
  //fTitleMark:='◆';
  //fTitleMark:='◇';
  fTitleMark:='◈';
  //fTitleMark:='◊';
  fHorzLine:= '——————————————————';
  fLinkBegin:='_';
  fLinkEnd:='_';
  fListItemMark:='✶ ';
  //fListItemMark:='✳ ';
  //fListItemMark:='✺ ';
  //fListItemMark:='⚫ ';
  //fListItemMark:='⚪ ';
  fMoreMark:='...';
  fMaxLineLen:=80;
  fIndentStep:=2;
end;

constructor THTML2TextRenderer.Create(const Stream: TStream);
var
  s: string;
begin
  SetLength(s{%H-},Stream.Size);
  if s<>'' then
    Stream.Read(s[1],length(s));
  Create(s);  // Call the constructor above.
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
  fIndent:=0;
  fLineLen:=0;
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
  if (fPendingNewLineCnt=0) and (fOutput<>'') and not fInHeader then
    fPendingNewLineCnt:=1;
end;

function THTML2TextRenderer.SplitLongLine: Boolean;
// Split long lines. Return True if actually split.
begin
  Result:=fLineLen>fMaxLineLen;
  if Result then
    AddNewLine;
end;

function THTML2TextRenderer.AddOutput(const aText: String): Boolean;
var
  i: Integer;
begin
  Result:=True;
  if fPendingSpace and (fPendingNewLineCnt=0) and not SplitLongLine then
  begin
    fOutput:=fOutput+' ';      // Don't add space at end of line (before newline)
    Inc(fLineLen);
  end;
  fPendingSpace:=False;
  for i:=0 to fPendingNewLineCnt-1 do
  begin
    fOutput:=fOutput+fLineEndMark;
    fLineLen:=0;
    Inc(fLineCnt);
    if fLineCnt>fMaxLines then
    begin
      fOutput:=fOutput+fLineEndMark+fMoreMark;
      Exit(False);             // Return False if max # of lines exceeded.
    end;
  end;
  if fPendingNewLineCnt>0 then
  begin
    i:=fIndent*fIndentStep;
    fOutput:=fOutput+StringOfChar(' ', i);
    Inc(fLineLen, i);
    fPendingNewLineCnt:=0;
  end;
  fOutput:=fOutput+aText;
  Inc(fLineLen, Length(aText));
  if aText='.' then            // Split also after '.'
    SplitLongLine;
end;

function THTML2TextRenderer.HtmlTag: Boolean;
// separate a html tag and use it for layout. '<' is already found here.
var
  Start: Integer;
  Tag, AttrName, AttrValue: String;
begin
  inc(p);
  // separate HTML tag itself.
  Start:=p;
  if (p<=fHtmlLen) and (fHTML[p]='/') then
    inc(p);
  while (p<=fHtmlLen) and not (fHTML[p] in [' ','>','"','/',#9,#10,#13]) do
    inc(p);
  Tag:=UpperCase(copy(fHTML,Start,p-Start));
  while p<=fHtmlLen do
  begin
    // Attribute name
    if fHTML[p]=' ' then
    begin
      inc(p);
      Start:=p;
      while (p<=fHtmlLen) and not (fHTML[p] in [' ','>','=',#9,#10,#13]) do
        inc(p);
      if p>fHtmlLen then break;
      if fHTML[p]='=' then
        AttrName:=copy(fHTML,Start,p-Start);
    end;
    // Attribute "value"
    if fHTML[p]='"' then
    begin
      inc(p);
      Start:=p;
      while (p<=fHtmlLen) and (fHTML[p]<>'"') do
        inc(p);
      if p>fHtmlLen then break;
      AttrValue:=copy(fHTML,Start,p-Start);
    end;
    inc(p);
    if (fHTML[p-1]='>') then break;  // end of tag
  end;

  // adjust layout based on HTML tag, then remove it
  Result:=True;
  case Tag of
    'HTML':
        fInHeader:=True;             // it's a whole page
    'BODY':
        Reset;                 // start of body => ignore header and all its data
    'P', '/P', 'BR', '/UL':
        AddNewLine;
    'DIV':
      begin
        fInDivTitle:=(CompareText(AttrName,'CLASS')=0)
                 and (CompareText(AttrValue,'TITLE')=0);
        if fInDivTitle then
        begin
          AddNewLine;
          Result:=AddOutput(fTitleMark+' ');
        end
        else
          AddOneNewLine;
        Inc(fIndent);
      end;
    '/DIV':
      begin
        if fInDivTitle then
        begin
          Result:=AddOutput(' '+fTitleMark);
          fInDivTitle:=False;
        end;
        AddOneNewLine;
        Dec(fIndent);
      end;
    'LI':
      begin
        Inc(fIndent);
        // Don't leave empty lines before list item (not sure if this is good)
        AddOneNewLine;
        Result:=AddOutput(fListItemMark);
      end;
    '/LI':
        Dec(fIndent);
    'A':                             // Link
        Result:=AddOutput(' '+fLinkBegin);
    '/A':
        Result:=AddOutput(fLinkEnd+' ');
    'HR':
      begin
        AddOneNewLine;
        Result:=AddOutput(fHorzLine);
        //AddOneNewLine;
      end;
  end;
end;

function THTML2TextRenderer.HtmlEntity: Boolean;
// entities: &nbsp; &lt; &gt; &amp;   '&' is found already here
const
  EntityMap: array[0..3] of array[0..1] of String = (
    ('nbsp;', ' '),   // &nbsp; happens most often. Let it be first.
    ('lt;',   '<'),
    ('gt;',   '>'),
    ('amp;',  '&')
  );
var
  Ent: String;
  i, j: Integer;
begin
  Inc(p);
  for i:=Low(EntityMap) to High(EntityMap) do
  begin
    Ent:=EntityMap[i][0];
    if (p+Length(Ent) >= fHtmlLen) then Break;
    j:=0;
    while j<Length(Ent) do
    begin
      if Ent[j+1] <> fHTML[p+j] then Break; // No match -> continue with next entity.
      Inc(j);
    end;
    if j=Length(Ent) then
    begin
      Inc(p,Length(Ent));
      Exit(AddOutput(EntityMap[i][1]));     // Match!
    end;
  end;
  Result:=AddOutput('&');     // Entity not found, add just '&'.
end;

function THTML2TextRenderer.Render(aMaxLines: integer): string;
// Parse the HTML and render to plain text.
// Output is limited to aMaxLines lines.
// Note: AddOutput, HtmlTag and HtmlEntity return False if MaxLines was exceeded.
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
      '&': OkToGo:=HtmlEntity;
      ' ',#9,#10,#13:           // WhiteSpace
        begin
          fPendingSpace:=True;
          inc(p);
        end;
      else
        begin
          OkToGo:=AddOutput(fHTML[p]);  // Add text verbatim.
          inc(p);
        end;
    end;
  end;
  Result:=fOutput;
end;

end.

