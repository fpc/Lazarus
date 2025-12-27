{-------------------------------------------------------------------------------
The contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL")

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterMarkdown.pas
The Initial Author of this file is the Gemini CLI Agent.
All Rights Reserved.

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

-------------------------------------------------------------------------------}

{
  Nested list blocks are not yet fully supported, but for simple markdown this should be OK. 
}
unit SynHighlighterMarkdown;

{$I synedit.inc}
{$h+}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter,
  SynEditHighlighterFoldBase,
  SynEditMiscProcs, LazEditTextAttributes;


type
  TtkTokenKind = (tkBlockQuote, tkBold, tkCodeBlock, tkCodeInline, tkHeader,
    tkItalic, tkKey, tkLink, tkList, tkNull, tkSpace, tkText, tkUnknown);

  { TSynMarkdownSyn }

  TSynMarkdownSyn = class(TSynCustomFoldHighlighter)

  private
    Type
      TRangeState = (rsUnknown, rsCodeBlock);
    const
      MaxAttrs = 9;
  protected
    type
      TCodeFoldBlockType = (
      cfbtHeader1,
      cfbtHeader2,
      cfbtHeader3,
      cfbtHeader4,
      cfbtHeader5,
      cfbtHeader6,
      cfbtCodeBlock,
      cfbtNone
    );

  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fAttrs: array[0..MaxAttrs] of TSynHighlighterAttributes;
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
    function GetAttr(AIndex: Integer): TSynHighlighterAttributes;
    procedure SetAttr(AIndex: Integer; const aValue: TSynHighlighterAttributes);

    procedure SetAttribute(var AAttri: TSynHighlighterAttributes; AValue: TSynHighlighterAttributes);

  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;

    // Folding
    procedure CreateRootCodeFoldBlock; override;
    function GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig; override;
    function GetFoldConfigCount: Integer; override;
    function GetFoldConfigInternalCount: Integer; override;
    function StartMarkdownCodeFoldBlock(ABlockType: TCodeFoldBlockType): TSynCustomCodeFoldBlock;
    function TopMarkdownCodeFoldBlockType: TCodeFoldBlockType;

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
    property TextAttri: TSynHighlighterAttributes Index 0 read GetAttr write SetAttr;
    property HeaderAttri: TSynHighlighterAttributes Index 1 read GetAttr write SetAttr;
    property BoldAttri: TSynHighlighterAttributes Index 2 read GetAttr write SetAttr;
    property ItalicAttri: TSynHighlighterAttributes Index 3 read GetAttr write SetAttr;
    property CodeInlineAttri: TSynHighlighterAttributes Index 4 read GetAttr write SetAttr;
    property CodeBlockAttri: TSynHighlighterAttributes Index 5 read GetAttr write SetAttr;
    property ListAttri: TSynHighlighterAttributes Index 6 read GetAttr write SetAttr;
    property BlockQuoteAttri: TSynHighlighterAttributes Index 7 read GetAttr write SetAttr;
    property LinkAttri: TSynHighlighterAttributes Index 8 read GetAttr write SetAttr;
    property SpaceAttri: TSynHighlighterAttributes Index 9 read GetAttr write SetAttr;
  end;

implementation

uses
  SynEditStrConst;

{ TSynMarkdownSyn }

constructor TSynMarkdownSyn.Create(AOwner: TComponent);
var
  I : Integer;
begin
  inherited Create(AOwner);

  fAttrs[0] := TSynHighlighterAttributes.Create('Text', 'Text');
  fAttrs[1] := TSynHighlighterAttributes.Create('Header', 'Header');
  fAttrs[2] := TSynHighlighterAttributes.Create('Bold', 'Bold');
  fAttrs[3] := TSynHighlighterAttributes.Create('Italic', 'Italic');
  fAttrs[4] := TSynHighlighterAttributes.Create('Code Inline', 'CodeInline');
  fAttrs[5] := TSynHighlighterAttributes.Create('Code Block', 'CodeBlock');
  fAttrs[6] := TSynHighlighterAttributes.Create('List', 'List');
  fAttrs[7] := TSynHighlighterAttributes.Create('BlockQuote', 'BlockQuote');
  fAttrs[8] := TSynHighlighterAttributes.Create('Link', 'Link');
  fAttrs[9] := TSynHighlighterAttributes.Create('Space', 'Space');

  // Attributes registered with AddAttribute are freed automatically.
  For I:=0 to MaxAttrs do
    AddAttribute(fAttrs[i]);

  // Default attributes
  HeaderAttri.Style := [fsBold];
  HeaderAttri.Foreground := clMaroon;
  
  BoldAttri.Style := [fsBold];
  
  ItalicAttri.Style := [fsItalic];
  
  CodeInlineAttri.Background := clSilver;
  CodeInlineAttri.Foreground := clRed;
  
  CodeBlockAttri.Foreground := clBlue;
  
  ListAttri.Style := [fsBold];
  ListAttri.Foreground := clNavy;
  
  BlockQuoteAttri.Foreground := clGreen;
  
  LinkAttri.Foreground := clBlue;
  LinkAttri.Style := [fsUnderline];

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
    SYN_ATTR_WHITESPACE: Result := SpaceAttri;
    SYN_ATTR_IDENTIFIER: Result := TextAttri;
    else Result := nil;
  end;
end;

function TSynMarkdownSyn.GetEol: Boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynMarkdownSyn.GetRange: Pointer;
begin
  // We need to combine fRange (parsing state) with fold state from base class
  // Base class uses GetRange to return fold stack.
  // We need to store fRange separately or cast/combine?
  // TSynCustomFoldHighlighter uses CodeFoldRange.RangeType for extra data if needed?
  // Let's see how XML highlighter does it.
  // XML uses TRangeStore record.
  // For now, let's just delegate to inherited for fold range, but we lose fRange?
  // Actually TSynCustomFoldHighlighter.GetRange calls fRanges.GetEqual(FCodeFoldRange).
  // FCodeFoldRange.RangeType is a pointer. We can store fRange there.
  CodeFoldRange.RangeType := Pointer(PtrUInt(fRange));
  Result := inherited GetRange;
end;

function TSynMarkdownSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenID;
end;

procedure TSynMarkdownSyn.SetRange(Value: Pointer);
begin
  inherited SetRange(Value);
  fRange := TRangeState(PtrUInt(CodeFoldRange.RangeType));
end;

procedure TSynMarkdownSyn.ResetRange;
begin
  inherited ResetRange;
  fRange := rsUnknown;
  CodeFoldRange.RangeType := Pointer(PtrUInt(fRange));
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
    tkBlockQuote: Result := BlockQuoteAttri;
    tkBold: Result := BoldAttri;
    tkCodeBlock: Result := CodeBlockAttri;
    tkCodeInline: Result := CodeInlineAttri;
    tkHeader: Result := HeaderAttri;
    tkItalic: Result := ItalicAttri;
    tkLink: Result := LinkAttri;
    tkList: Result := ListAttri;
    tkSpace: Result := SpaceAttri;
    tkText: Result := TextAttri;
  else
    Result := TextAttri;
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

procedure TSynMarkdownSyn.SetAttr(AIndex: Integer; const aValue: TSynHighlighterAttributes);
begin
  fAttrs[AIndex].Assign(aValue);
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

function TSynMarkdownSyn.GetAttr(AIndex: Integer): TSynHighlighterAttributes;
begin
  Result:=fAttrs[AIndex];
end;

procedure TSynMarkdownSyn.LFProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
end;

procedure TSynMarkdownSyn.HeaderProc;
var
  Level: Integer;
begin
  fTokenID := tkHeader;
  Level := 0;
  while fLine[Run + Level] = '#' do Inc(Level);
  
  if (Level > 0) and (Level <= 6) then
  begin
    while (TopMarkdownCodeFoldBlockType <> cfbtNone) and 
          (TopMarkdownCodeFoldBlockType >= TCodeFoldBlockType(Ord(cfbtHeader1) + Level - 1)) do
      EndCodeFoldBlock;
      
    StartMarkdownCodeFoldBlock(TCodeFoldBlockType(Ord(cfbtHeader1) + Level - 1));
  end;

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
    StartMarkdownCodeFoldBlock(cfbtCodeBlock);
    // Consume the ``` and maybe language identifier
    while not (fLine[Run] in [#0, #10, #13]) do 
      Inc(Run);
    end
  else
    begin
    // We are ending a block
    fRange := rsUnknown;
    EndCodeFoldBlock; 
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

// Folding Implementation

procedure TSynMarkdownSyn.CreateRootCodeFoldBlock;
begin
  inherited CreateRootCodeFoldBlock;
  RootCodeFoldBlock.InitRootBlockType(Pointer(PtrInt(cfbtNone)));
end;

function TSynMarkdownSyn.GetFoldConfigInstance(Index: Integer): TSynCustomFoldConfig;
begin
  Result := inherited GetFoldConfigInstance(Index);
  Result.Enabled := True;
  Result.Modes := [fmFold];
end;

function TSynMarkdownSyn.GetFoldConfigCount: Integer;
begin
  Result := Ord(High(TCodeFoldBlockType)) - Ord(Low(TCodeFoldBlockType));
end;

function TSynMarkdownSyn.GetFoldConfigInternalCount: Integer;
begin
  Result := Ord(High(TCodeFoldBlockType)) - Ord(Low(TCodeFoldBlockType)) + 1;
end;

function TSynMarkdownSyn.StartMarkdownCodeFoldBlock(ABlockType: TCodeFoldBlockType): TSynCustomCodeFoldBlock;
begin
  Result := inherited StartCodeFoldBlock(Pointer(PtrInt(ABlockType)));
end;

function TSynMarkdownSyn.TopMarkdownCodeFoldBlockType: TCodeFoldBlockType;
begin
  Result := TCodeFoldBlockType(PtrUInt(TopCodeFoldBlockType));
end;

initialization
  RegisterPlaceableHighlighter(TSynMarkdownSyn);

end.
