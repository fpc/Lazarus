{-------------------------------------------------------------------------------
The contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL")

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterPo.pp, released 2025-12-27.
Author: Michael Van Canneyt
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

-------------------------------------------------------------------------------}

{
  Nested list blocks are not yet fully supported, but for simple markdown this should be OK. 
}
unit SynHighlighterMarkdown;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter,
  SynEditMiscProcs, LazEditTextAttributes;

type
  TtkTokenKind = (tkBlockQuote, tkBold, tkCodeBlock, tkCodeInline, tkHeader,
    tkItalic, tkKey, tkLink, tkList, tkNull, tkSpace, tkText, tkUnknown);

  TRangeState = (rsUnknown, rsCodeBlock);

  { TSynMarkdownSyn }

  TSynMarkdownSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fTextAttri: TSynHighlighterAttributes;
    fHeaderAttri: TSynHighlighterAttributes;
    fBoldAttri: TSynHighlighterAttributes;
    fItalicAttri: TSynHighlighterAttributes;
    fCodeInlineAttri: TSynHighlighterAttributes;
    fCodeBlockAttri: TSynHighlighterAttributes;
    fListAttri: TSynHighlighterAttributes;
    fBlockQuoteAttri: TSynHighlighterAttributes;
    fLinkAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    
    fLine: PChar;
    Run: LongInt;
    fTokenPos: Integer;
    fLineLen: Integer;

    procedure BlockQuoteProc;
    procedure BoldProc;
    procedure CodeBlockProc;
    procedure CodeInlineProc;
    procedure CRProc;
    procedure HeaderProc;
    procedure ItalicProc;
    procedure LFProc;
    procedure LinkProc;
    procedure ListProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure TextProc;
    
    procedure SetAttribute(var AAttri: TSynHighlighterAttributes; AValue: TSynHighlighterAttributes);

  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    class function GetLanguageName: string; override;
    function GetTokenPos: Integer; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    procedure InitForScaningLine; override;
    procedure Next; override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TLazEditTextAttribute; override;
    function GetTokenKind: integer; override;
  published
    property TextAttri: TSynHighlighterAttributes read fTextAttri write fTextAttri;
    property HeaderAttri: TSynHighlighterAttributes read fHeaderAttri write fHeaderAttri;
    property BoldAttri: TSynHighlighterAttributes read fBoldAttri write fBoldAttri;
    property ItalicAttri: TSynHighlighterAttributes read fItalicAttri write fItalicAttri;
    property CodeInlineAttri: TSynHighlighterAttributes read fCodeInlineAttri write fCodeInlineAttri;
    property CodeBlockAttri: TSynHighlighterAttributes read fCodeBlockAttri write fCodeBlockAttri;
    property ListAttri: TSynHighlighterAttributes read fListAttri write fListAttri;
    property BlockQuoteAttri: TSynHighlighterAttributes read fBlockQuoteAttri write fBlockQuoteAttri;
    property LinkAttri: TSynHighlighterAttributes read fLinkAttri write fLinkAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
  end;

implementation

uses
  SynEditStrConst;

{ TSynMarkdownSyn }

constructor TSynMarkdownSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fTextAttri := TSynHighlighterAttributes.Create('Text', 'Text');
  fHeaderAttri := TSynHighlighterAttributes.Create('Header', 'Header');
  fBoldAttri := TSynHighlighterAttributes.Create('Bold', 'Bold');
  fItalicAttri := TSynHighlighterAttributes.Create('Italic', 'Italic');
  fCodeInlineAttri := TSynHighlighterAttributes.Create('Code Inline', 'CodeInline');
  fCodeBlockAttri := TSynHighlighterAttributes.Create('Code Block', 'CodeBlock');
  fListAttri := TSynHighlighterAttributes.Create('List', 'List');
  fBlockQuoteAttri := TSynHighlighterAttributes.Create('BlockQuote', 'BlockQuote');
  fLinkAttri := TSynHighlighterAttributes.Create('Link', 'Link');
  fSpaceAttri := TSynHighlighterAttributes.Create('Space', 'Space');

  AddAttribute(fTextAttri);
  AddAttribute(fHeaderAttri);
  AddAttribute(fBoldAttri);
  AddAttribute(fItalicAttri);
  AddAttribute(fCodeInlineAttri);
  AddAttribute(fCodeBlockAttri);
  AddAttribute(fListAttri);
  AddAttribute(fBlockQuoteAttri);
  AddAttribute(fLinkAttri);
  AddAttribute(fSpaceAttri);
  
  // Default attributes
  fHeaderAttri.Style := [fsBold];
  fHeaderAttri.Foreground := clMaroon;
  
  fBoldAttri.Style := [fsBold];
  
  fItalicAttri.Style := [fsItalic];
  
  fCodeInlineAttri.Background := clSilver;
  fCodeInlineAttri.Foreground := clRed;
  
  fCodeBlockAttri.Foreground := clBlue;
  
  fListAttri.Style := [fsBold];
  fListAttri.Foreground := clNavy;
  
  fBlockQuoteAttri.Foreground := clGreen;
  
  fLinkAttri.Foreground := clBlue;
  fLinkAttri.Style := [fsUnderline];

  SetAttributesOnChange(@DefHighlightChange);
end;

procedure TSynMarkdownSyn.SetAttribute(var AAttri: TSynHighlighterAttributes; AValue: TSynHighlighterAttributes);
begin
  AAttri.Assign(AValue);
end;

class function TSynMarkdownSyn.GetLanguageName: string;
begin
  Result := 'Markdown';
end;

function TSynMarkdownSyn.GetTokenPos: Integer;
begin
  Result:=FTokenPos;
end;

function TSynMarkdownSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_IDENTIFIER: Result := fTextAttri;
    else Result := nil;
  end;
end;

function TSynMarkdownSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynMarkdownSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynMarkdownSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenID;
end;

procedure TSynMarkdownSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrInt(Value));
end;

procedure TSynMarkdownSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynMarkdownSyn.InitForScaningLine;
begin
  inherited InitForScaningLine;
  fLine := PChar(CurrentLineText);
  fLineLen := Length(CurrentLineText);
  Run := 0;
  Next;
end;

procedure TSynMarkdownSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart := fLine + fTokenPos;
  TokenLength := Run - fTokenPos;
end;

function TSynMarkdownSyn.GetToken: string;
var
  Len: Integer;
begin
  Len := Run - fTokenPos;
  SetString(Result, fLine + fTokenPos, Len);
end;

function TSynMarkdownSyn.GetTokenAttribute: TLazEditTextAttribute;
begin
  case fTokenID of
    tkBlockQuote: Result := fBlockQuoteAttri;
    tkBold: Result := fBoldAttri;
    tkCodeBlock: Result := fCodeBlockAttri;
    tkCodeInline: Result := fCodeInlineAttri;
    tkHeader: Result := fHeaderAttri;
    tkItalic: Result := fItalicAttri;
    tkLink: Result := fLinkAttri;
    tkList: Result := fListAttri;
    tkSpace: Result := fSpaceAttri;
    tkText: Result := fTextAttri;
    else Result := fTextAttri;
  end;
end;

function TSynMarkdownSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenID);
end;

function TSynMarkdownSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['0'..'9', 'a'..'z', 'A'..'Z'];
end;

function TSynMarkdownSyn.GetSampleSource: string;
begin
  Result := '# Header'#13#10 +
            'Text with **bold** and *italic*.'#13#10 +
            'And `inline code`.'#13#10 +
            ''#13#10 +
            '* List item'#13#10 +
            '- List item'#13#10 +
            ''#13#10 +
            '```'#13#10 +
            'Code block'#13#10 +
            '```';
end;

procedure TSynMarkdownSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynMarkdownSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  while (fLine[Run] in [#1..#32]) do inc(Run);
end;

procedure TSynMarkdownSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynMarkdownSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynMarkdownSyn.HeaderProc;
begin
  fTokenID := tkHeader;
  while not (fLine[Run] in [#0, #10, #13]) do Inc(Run);
end;

procedure TSynMarkdownSyn.BlockQuoteProc;
begin
  fTokenID := tkBlockQuote;
  Inc(Run);
end;

procedure TSynMarkdownSyn.ListProc;
begin
  fTokenID := tkList;
  Inc(Run);
  if fLine[Run] = ' ' then Inc(Run);
end;

procedure TSynMarkdownSyn.CodeBlockProc;
begin
  fTokenID := tkCodeBlock;
  if fRange = rsUnknown then
    begin
    fRange := rsCodeBlock;
    // Consume the ``` and maybe language identifier
    while not (fLine[Run] in [#0, #10, #13]) do 
      Inc(Run);
    end
  else
    begin
    // We are ending a block
    fRange := rsUnknown;
    // Consume the ```
    while not (fLine[Run] in [#0, #10, #13]) do 
      Inc(Run);
    end;
end;

procedure TSynMarkdownSyn.CodeInlineProc;
begin
  fTokenID := tkCodeInline;
  Inc(Run); // Consume first `
  while not (fLine[Run] in [#0, #10, #13]) do
    begin
    if fLine[Run] = '`' then
      begin
      Inc(Run);
      Break;
      end;
    Inc(Run);
    end;
end;

procedure TSynMarkdownSyn.BoldProc;
begin
  fTokenID := tkBold;
  Inc(Run, 2); // Consume ** or __
  while not (fLine[Run] in [#0, #10, #13]) do
    begin
    if ((fLine[Run] = '*') and (fLine[Run+1] = '*')) or
       ((fLine[Run] = '_') and (fLine[Run+1] = '_')) then
      begin
      Inc(Run, 2);
      Break;
      end;
    Inc(Run);
  end;
end;

procedure TSynMarkdownSyn.ItalicProc;
begin
  fTokenID := tkItalic;
  Inc(Run); // Consume * or _
  while not (fLine[Run] in [#0, #10, #13]) do
    begin
    if (fLine[Run] = '*') or (fLine[Run] = '_') then
      begin
      // Avoid matching ** as two *
      if (fLine[Run+1] <> fLine[Run]) then 
        begin
        Inc(Run);
        Break;
        end
      else
        begin
        // Let's just skip it
        Inc(Run, 2); 
        Continue;
        end;
      end;
    Inc(Run);
    end;
end;

procedure TSynMarkdownSyn.LinkProc;
begin
  fTokenID := tkLink;
  Inc(Run); // Consume [
  while not (fLine[Run] in [#0, #10, #13]) do
    begin
    if fLine[Run] = ']' then
      begin
      Inc(Run);
      if fLine[Run] = '(' then
        begin
        Inc(Run);
        while not (fLine[Run] in [#0, #10, #13, ')']) do 
          Inc(Run);
        if fLine[Run] = ')' then Inc(Run);
        end;
      Break;
      end;
    Inc(Run);
    end;
end;

procedure TSynMarkdownSyn.TextProc;
begin
  fTokenID := tkText;
  while not (fLine[Run] in [#0, #10, #13]) do
    begin
    case fLine[Run] of
      '`': Break;
      '*': Break;
      '_': Break;
      '[': Break;
    end;
    Inc(Run);
    end;
end;

procedure TSynMarkdownSyn.Next;
var
  lRun : Integer;
begin
  fTokenPos := Run;
  // Handle Code Block State (Multi-line)
  if fRange = rsCodeBlock then
    begin
    // Check for end of block ```
    lRun:=Run;
    While (fLine[lRun]=' ') do
      inc(lRun);
    if (lRun<fLineLen-2) and (fLine[lRun] = '`') and (fLine[lRun+1] = '`') and (fLine[lRun+2] = '`') then
      begin
      CodeBlockProc;
      Exit;
      end;
    
    // Check if we are at EOL
    if fLine[Run] in [#0, #10, #13] then
      begin
      case fLine[Run] of
        #0: NullProc;
        #10: LFProc;
        #13: CRProc;
      end;
      Exit;
      end;

    // Otherwise, everything is code block
    fTokenID := tkCodeBlock;
    while not (fLine[Run] in [#0, #10, #13]) do 
      Inc(Run);
    Exit;
    end;

  // Handle Normal State
  case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
    ' ', #9: SpaceProc;
    '#':
      if Run = 0 then 
        HeaderProc
      else 
        TextProc;
    '>':
      if Run = 0 then BlockQuoteProc
      else TextProc;
    '*':
      if (Run = 0) and (fLine[Run+1] = ' ') then 
        ListProc
      else if (fLine[Run+1] = '*') then 
        BoldProc
      else 
        ItalicProc;
    '-':
      if (Run = 0) and (fLine[Run+1] = ' ') then 
        ListProc
      else 
        TextProc;
    '+':
      if (Run = 0) and (fLine[Run+1] = ' ') then 
        ListProc
      else 
        TextProc;
    '1':
      if (Run = 0) and (fLine[Run+1] = '.') and (fLine[Run+2] = ' ') then
        begin
        fTokenID := tkList;
        Inc(Run, 2);
        if fLine[Run] = ' ' then Inc(Run);
        end
      else 
        TextProc;
    '`':
      if (fLine[Run+1] = '`') and (fLine[Run+2] = '`') then 
        CodeBlockProc
      else 
        CodeInlineProc;
    '_':
      if (fLine[Run+1] = '_') then 
        BoldProc
      else 
        ItalicProc;
    '[': 
      LinkProc;
  else 
    TextProc;
  end;
end;

initialization
  RegisterPlaceableHighlighter(TSynMarkdownSyn);

end.
