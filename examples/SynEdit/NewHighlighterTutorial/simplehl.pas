(*
  This is an example how to implement your own highlighter.

  This example does allow to specify different colors for
  - text            (defaults to not-highlighted)
  - spaces          (defaults to fuchsia dotted underline)
  - the word "not"  (defaults to red color)
  - words, separated by spaces, that start with a,e,i,o,u  (defaults to bold)

  See comments below and http://wiki.lazarus.freepascal.org/SynEdit_Highlighter

  How it works:

  - Creation
    The Highlighter creates Attributes to store colors for the tokens (Words or groups of spaces).

  - InitForScanningLine
    Is called by SynEdit before a line gets painted (or before highlight info is needed)
    This is also called, each time the text changes for *all* changed lines
    and may even be called for all lines after the change up to the end of text.

    After InitForScanningLine was called "GetToken*" should return information about the
    first token on the line.
    Note: Spaces are token too.

  - Next
    Scan to the next token. "GetToken*"  should return info about that next token.

  - GetEOL
    Returns True, if "Next" was called while on the last token of the line.

  - GetTokenEx, GetTokenAttribute
    Provide info about the token found by "Next"

  - Next, GetEOL. GetToken___
    Are used by SynEdit to iterate over the Line.
    Important: The tokens returned for each line, must represent the original
    line-text (nothing added, nothing left out), and be returned in the correct order.

    They are called very often and should perform at high speed.

  - GetToken, GetTokenPos, GetTokenKind
    SynEdit uses them e.g for finding matching brackets. If GetTokenKind returns different values per Attribute, then brackets only match, if they are of the same kind (e.g, if there was a string attribute, brackets outside a string would not match brackets inside a string)

  Note:
  - The tokens in each lines are always scanned from the first to last (left to right in non-RTL text)
  - Lines can be scanned in any order.
    The highlighter can not store any info to be used across line boundaries.

*)

(* NOTE:
   This unit implements TSynDemoHl as the simplest highlighter. For this it uses
   the base class TLazEditCustomHighlighter.

   However, the highlighters in the other units (Context/Fold) need the exact same
   implementation as a base class, except they need other base classes.

   Therefore this unit, is used several times under different unit names.
*)
unit
{$IFDEF CompileSimpleHl_WithFold}  SimpleHlWithFold;
{$ELSE}
{$IFDEF CompileSimpleHl_WithRange} SimpleHlWithRange;
{$ELSE}                            SimpleHl;
{$ENDIF}
{$ENDIF}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  // LazEdit
  LazEditTextAttributes, LazEditHighlighter
  // SynEdit
  {$IFDEF CompileSimpleHl_WithFold}  , SynEditHighlighterFoldBase {$ENDIF}
  ;

type
  (* TsynDemoHl could inherit from TLazEditCustomHighlighter
     However in order to use it as base-class for TSynDemoHlContext it inherits
     fromTLazEditCustomRangesHighlighter
  *)

  { TSynDemoHl }

  TSynDemoHl = class(
    {$IFDEF CompileSimpleHl_WithFold}  TSynCustomFoldHighlighter
    {$ELSE}
    {$IFDEF CompileSimpleHl_WithRange} TLazEditCustomRangesHighlighter
    {$ELSE}                            TLazEditCustomHighlighter
    {$ENDIF}
    {$ENDIF}
  )
  private
    fIdentifierAttri: TLazEditHighlighterAttributes;
    fSpaceAttri: TLazEditHighlighterAttributes;
    FNotAttri: TLazEditHighlighterAttributes;
    fSpecialAttri: TLazEditHighlighterAttributes;
    procedure SetAttriColor(AnIndex: Integer; AValue: TLazEditHighlighterAttributes);
  protected
    (* NOTE: protected, to keep accessible for the other examples *)
    // The boundaries of the current token.
    // Initialized by InitForScanningLine
    // - FTokenPos is initialized by InitForScanningLine calling Next()
    // Valid only within each line.
    FTokenPos, FTokenEnd: Integer;
  public
    // Scan
    procedure InitForScanningLine; override;
    procedure Next; override;
    function  GetEol: Boolean; override;

    // Info on current token
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function  GetTokenAttribute: TLazEditTextAttribute; override;
  public
    // Info on current token
    function GetToken: String; override;
    function GetTokenPos: Integer; override;
    function GetTokenKind: integer; override;
    function GetTokenClassAttribute(ATkClass: TLazEditTokenClass;
      ATkDetails: TLazEditTokenDetails = []): TLazEditTextAttribute; override;

    constructor Create(AOwner: TComponent); override;
  published
    (* Define 4 Attributes, for the different highlights. *)
    property IdentifierAttri: TLazEditHighlighterAttributes index 0 read fIdentifierAttri write SetAttriColor;
    property SpaceAttri:      TLazEditHighlighterAttributes index 1 read fSpaceAttri write SetAttriColor;
    property SpecialAttri:    TLazEditHighlighterAttributes index 2 read fSpecialAttri write SetAttriColor;
    property NotAttri:        TLazEditHighlighterAttributes index 3 read FNotAttri write SetAttriColor;
  end;

implementation

constructor TSynDemoHl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  (* Create and initialize the attributes.
     AddAttribute adds them to the list (property Attributes). This also ensures
     they will be freed when the Highlighter is destroyed.
   *)
  fSpecialAttri := TLazEditHighlighterAttributes.Create('special', 'special');
  AddAttribute(fSpecialAttri);
  fSpecialAttri.Style := [fsBold];

  FNotAttri := TLazEditHighlighterAttributes.Create('not', 'not');
  AddAttribute(FNotAttri);
  FNotAttri.Foreground := clRed;

  fIdentifierAttri := TLazEditHighlighterAttributes.Create('ident', 'ident');
  AddAttribute(fIdentifierAttri);

  fSpaceAttri := TLazEditHighlighterAttributes.Create('space', 'space');
  AddAttribute(fSpaceAttri);
  fSpaceAttri.FrameColor := clFuchsia;
  fSpaceAttri.FrameEdges := sfeBottom;
  fSpaceAttri.FrameStyle := slsDotted;

  // Ensure the HL reacts to changes in the attributes. Do this once, if all attributes are created
  SetAttributesOnChange(@DefHighlightChange);
end;

(* Setter(s) for attributes / This allows using in Object inspector*)
procedure TSynDemoHl.SetAttriColor(AnIndex: Integer; AValue: TLazEditHighlighterAttributes);
begin
  case AnIndex of
    0: fIdentifierAttri.Assign(AValue);
    1: fSpaceAttri.Assign(AValue);
    2: fSpecialAttri.Assign(AValue);
    3: FNotAttri.Assign(AValue);
  end;
end;

procedure TSynDemoHl.InitForScanningLine;
begin
  inherited;
  (* Setting the end of the current token to the start of line.
     Calling Next, will then find the "next token" starting from that end, and
     therefore starting at the begin of the line (i.e. the first token)
  *)
  FTokenEnd := 1;
  Next;
end;

procedure TSynDemoHl.Next;
var
  l: Integer;
begin
  // FTokenEnd should be at the start of the next Token (which is the Token we want)
  FTokenPos := FTokenEnd;

  (* Scan forward to next token
     FTokenEnd will be set 1 after the last char. That is:
     - The first char of the next token
     - or past the end of line (which allows GetEOL to work)


     FTokenEnd (1 after the end of the last token) is the start of the new token.
     - If it points at a space, then scan until a none space is found
     - If it points to a none-space, scan until a space is found
  *)


  l := length(CurrentLineText);
  If FTokenPos > l then
    // At line end: FTokenPos = FTokenEnd => aka empty token
    exit
  else
  if CurrentLineText[FTokenEnd] in [#9, ' '] then
    // At Space? Find end of spaces
    while (FTokenEnd <= l) and (CurrentLineText[FTokenEnd] in [#9, ' ']) do inc (FTokenEnd)
  else
  if CurrentLineText[FTokenEnd] in [#0..#31] then
    // At unknown?
    while (FTokenEnd <= l) and (CurrentLineText[FTokenEnd] in [#0..#31]) do inc (FTokenEnd)
  else
    // At None-Space? Find end of None-spaces
    while (FTokenEnd <= l) and not(CurrentLineText[FTokenEnd] in [#9, ' ']) do inc (FTokenEnd)
end;

function TSynDemoHl.GetEol: Boolean;
begin
  Result := FTokenPos > length(CurrentLineText);
end;

procedure TSynDemoHl.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart := @CurrentLineText[FTokenPos];
  TokenLength := FTokenEnd - FTokenPos;
end;

function TSynDemoHl.GetTokenAttribute: TLazEditTextAttribute;
begin
  (* The current token goes from FTokenPos to FTokenEnd.
     Check, if it is
     - spaces
     - start with aeiou
     - = 'not'
     and return the matching Attribute
  *)

  if CurrentLineText[FTokenPos] in [#9, ' '] then
    Result := SpaceAttri
  else
  if LowerCase(CurrentLineText[FTokenPos]) in ['a', 'e', 'i', 'o', 'u'] then
    Result := SpecialAttri
  else
  if LowerCase(copy(CurrentLineText, FTokenPos, FTokenEnd - FTokenPos)) = 'not' then
    Result := NotAttri
  else
    Result := IdentifierAttri;
end;

function TSynDemoHl.GetToken: String;
begin
  Result := copy(CurrentLineText, FTokenPos, FTokenEnd - FTokenPos);
end;

function TSynDemoHl.GetTokenPos: Integer;
begin
  Result := FTokenPos - 1;
end;

function TSynDemoHl.GetTokenClassAttribute(ATkClass: TLazEditTokenClass;
  ATkDetails: TLazEditTokenDetails): TLazEditTextAttribute;
begin
  // Some default attributes
  case ATkClass of
    tcComment: Result := fSpecialAttri;
    tcIdentifier: Result := fIdentifierAttri;
    tcWhiteSpace: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynDemoHl.GetTokenKind: integer;
var
  a: TLazEditTextAttribute;
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

