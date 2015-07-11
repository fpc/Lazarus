unit SimpleHl;
(*
  This is an example how to implement your own highlighter.

  This example does allow to specify different colors for
  - text (defaults to not-highlighted)
  - spaces  (defaults to silver frame)
  - words, separated by spaces, that start with a,e,i,o,u  (defaults to bold)
  - the word "not"  (defaults to red background)

  See comments below and http://wiki.lazarus.freepascal.org/SynEdit_Highlighter

  How it works:

  - Creation
    The Highlighter creates Attributes that it can return the Words and Spaces.

  - SetLine
    Is called by SynEdit before a line gets painted (or before highlight info is needed)
    This is also called, each time the text changes fol *all* changed lines
    and may even be called for all lines after the change up to the end of text.

    After SetLine was called "GetToken*" should return information about the
    first token on the line.
    Note: Spaces are token too.

  - Next
    Scan to the next token, on the line that was set by "SetLine"
    "GetToken*"  should return info about that next token.

  - GetEOL
    Returns True, if "Next" was called while on the last token of the line.

  - GetTokenEx, GetTokenAttribute
    Provide info about the token found by "Next"

  - Next, GetEOL. GetToken*
    Are used by SynEdit to iterate over the Line.
    Important: The tokens returned for each line, must represent the original
    line-text (mothing added, nothing left out), and be returned in the correct order.

    They are called very often and should perform ath high speed.

  - GetToken, GetTokenPos, GetTokenKind
    SynEdit uses them e.g for finding matching brackets. If GetTokenKind returns different values per Attribute, then brackets only match, if they are of the same kind (e.g, if there was a string attribute, brackets outside a string would not match brackets inside a string)


*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter;

type

  { TSynDemoHl }


  TSynDemoHl = class(TSynCustomHighlighter)
  private
    FNotAttri: TSynHighlighterAttributes;
    fSpecialAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    procedure SetIdentifierAttri(AValue: TSynHighlighterAttributes);
    procedure SetNotAttri(AValue: TSynHighlighterAttributes);
    procedure SetSpaceAttri(AValue: TSynHighlighterAttributes);
    procedure SetSpecialAttri(AValue: TSynHighlighterAttributes);
  protected
    // accesible for the other examples
    FTokenPos, FTokenEnd: Integer;
    FLineText: String;
  public
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    procedure Next; override;
    function  GetEol: Boolean; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TSynHighlighterAttributes; override;
  public
    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes; override;
    constructor Create(AOwner: TComponent); override;
  published
    (* Define 4 Attributes, for the different highlights. *)
    property SpecialAttri: TSynHighlighterAttributes read fSpecialAttri
      write SetSpecialAttri;
    property NotAttri: TSynHighlighterAttributes read FNotAttri
      write SetNotAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write SetIdentifierAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write SetSpaceAttri;
  end;

implementation

constructor TSynDemoHl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  (* Create and initialize the attributes *)
  fSpecialAttri := TSynHighlighterAttributes.Create('special', 'special');
  AddAttribute(fSpecialAttri);
  fSpecialAttri.Style := [fsBold];

  FNotAttri := TSynHighlighterAttributes.Create('not', 'not');
  AddAttribute(FNotAttri);
  FNotAttri.Background := clRed;

  fIdentifierAttri := TSynHighlighterAttributes.Create('ident', 'ident');
  AddAttribute(fIdentifierAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create('space', 'space');
  AddAttribute(fSpaceAttri);
  fSpaceAttri.FrameColor := clSilver;
  fSpaceAttri.FrameEdges := sfeAround;
end;

(* Setters for attributes / This allows using in Object inspector*)
procedure TSynDemoHl.SetIdentifierAttri(AValue: TSynHighlighterAttributes);
begin
  fIdentifierAttri.Assign(AValue);
end;

procedure TSynDemoHl.SetNotAttri(AValue: TSynHighlighterAttributes);
begin
  FNotAttri.Assign(AValue);
end;

procedure TSynDemoHl.SetSpaceAttri(AValue: TSynHighlighterAttributes);
begin
  fSpaceAttri.Assign(AValue);
end;

procedure TSynDemoHl.SetSpecialAttri(AValue: TSynHighlighterAttributes);
begin
  fSpecialAttri.Assign(AValue);
end;

procedure TSynDemoHl.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  FLineText := NewValue;
  // Next will start at "FTokenEnd", so set this to 1
  FTokenEnd := 1;
  Next;
end;

procedure TSynDemoHl.Next;
var
  l: Integer;
begin
  // FTokenEnd should be at the start of the next Token (which is the Token we want)
  FTokenPos := FTokenEnd;
  // assume empty, will only happen for EOL
  FTokenEnd := FTokenPos;

  // Scan forward
  // FTokenEnd will be set 1 after the last char. That is:
  // - The first char of the next token
  // - or past the end of line (which allows GetEOL to work)

  l := length(FLineText);
  If FTokenPos > l then
    // At line end
    exit
  else
  if FLineText[FTokenEnd] in [#9, ' '] then
    // At Space? Find end of spaces
    while (FTokenEnd <= l) and (FLineText[FTokenEnd] in [#0..#32]) do inc (FTokenEnd)
  else
    // At None-Space? Find end of None-spaces
    while (FTokenEnd <= l) and not(FLineText[FTokenEnd] in [#9, ' ']) do inc (FTokenEnd)
end;

function TSynDemoHl.GetEol: Boolean;
begin
  Result := FTokenPos > length(FLineText);
end;

procedure TSynDemoHl.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart := @FLineText[FTokenPos];
  TokenLength := FTokenEnd - FTokenPos;
end;

function TSynDemoHl.GetTokenAttribute: TSynHighlighterAttributes;
begin
  // Match the text, specified by FTokenPos and FTokenEnd

  if FLineText[FTokenPos] in [#9, ' '] then
    Result := SpaceAttri
  else
  if LowerCase(FLineText[FTokenPos]) in ['a', 'e', 'i', 'o', 'u'] then
    Result := SpecialAttri
  else
  if LowerCase(copy(FLineText, FTokenPos, FTokenEnd - FTokenPos)) = 'not' then
    Result := NotAttri
  else
    Result := IdentifierAttri;
end;

function TSynDemoHl.GetToken: String;
begin
  Result := copy(FLineText, FTokenPos, FTokenEnd - FTokenPos);
end;

function TSynDemoHl.GetTokenPos: Integer;
begin
  Result := FTokenPos - 1;
end;

function TSynDemoHl.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  // Some default attributes
  case Index of
    SYN_ATTR_COMMENT: Result := fSpecialAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynDemoHl.GetTokenKind: integer;
var
  a: TSynHighlighterAttributes;
begin
  // Map Attribute into a unique number
  a := GetTokenAttribute;
  Result := 0;
  if a = fSpaceAttri then Result := 1;
  if a = fSpecialAttri then Result := 2;
  if a = fIdentifierAttri then Result := 3;
  if a = FNotAttri then Result := 4;
end;

end.

