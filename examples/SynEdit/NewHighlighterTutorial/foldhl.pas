unit FoldHl;
(*
  This is an example how to implement your own highlighter.

  This example extends the Simple and Context HL:
  - The token -(- and -)- (must be surrounded by space or line-begin/end to be
    a token of their own) will add foldable sections

    Multply -(- and -)- can be nested.

  See comments below and http://wiki.lazarus.freepascal.org/SynEdit_Highlighter

*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter, SynEditHighlighterFoldBase, ContextHL;

type

  (*   This is an EXACT COPY of SynEditHighlighter

       ONLY the base class is changed to add support for folding

       The new code follows below
  *)

  TSynDemoHlFoldBase = class(TSynCustomFoldHighlighter)
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

  (*   This is a COPY of SynEditHighlighter

       ONLY the base class is changed to add support for folding

       The new code follows below
  *)

  TSynDemoHlContextFoldBase = class(TSynDemoHlFoldBase)
  protected
    FCurRange: Integer;
  public
    procedure Next; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
  public
  (* The below needed to be changed and are in TSynDemoHlFold
     TSynDemoHlContextFoldBase uses Ranges itself.
     The Range needed here is therefore stored in a diff location
  *)
    //procedure SetRange(Value: Pointer); override;
    //procedure ResetRange; override;
    //function GetRange: Pointer; override;
  end;

  { TSynDemoHlContext }

  (* You can base this on either
     TSynDemoHlFoldBase or TSynDemoHlContextFoldBase

     Using ranges is NOT a condition for fold.
     (If changing, remove Range related code)

     Note that ranges to change.
  *)

  //TSynDemoHlFold = class(TSynDemoHlFoldBase)
  TSynDemoHlFold = class(TSynDemoHlContextFoldBase)
  public
    procedure Next; override;
  public
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function GetRange: Pointer; override;
  end;

implementation

{ TSynDemoHlFold }

procedure TSynDemoHlFold.Next;
begin
  inherited Next;
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = '-(-') then
    StartCodeFoldBlock(nil);
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = '-)-') then
    EndCodeFoldBlock;
end;

procedure TSynDemoHlFold.SetRange(Value: Pointer);
begin
  // must call the SetRange in TSynCustomFoldHighlighter
  inherited SetRange(Value);
  FCurRange := PtrInt(CodeFoldRange.RangeType);
 end;

procedure TSynDemoHlFold.ResetRange;
begin
  inherited ResetRange;
  FCurRange := 0;
end;

function TSynDemoHlFold.GetRange: Pointer;
begin
  // Store the range first
  CodeFoldRange.RangeType := Pointer(PtrInt(FCurRange));
  Result := inherited GetRange;
end;


(*   This is an EXACT COPY of SynEditHighlighter

     ONLY the base class is changed to add support for folding
*)

constructor TSynDemoHlFoldBase.Create(AOwner: TComponent);
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
procedure TSynDemoHlFoldBase.SetIdentifierAttri(AValue: TSynHighlighterAttributes);
begin
  fIdentifierAttri.Assign(AValue);
end;

procedure TSynDemoHlFoldBase.SetNotAttri(AValue: TSynHighlighterAttributes);
begin
  FNotAttri.Assign(AValue);
end;

procedure TSynDemoHlFoldBase.SetSpaceAttri(AValue: TSynHighlighterAttributes);
begin
  fSpaceAttri.Assign(AValue);
end;

procedure TSynDemoHlFoldBase.SetSpecialAttri(AValue: TSynHighlighterAttributes);
begin
  fSpecialAttri.Assign(AValue);
end;

procedure TSynDemoHlFoldBase.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  FLineText := NewValue;
  // Next will start at "FTokenEnd", so set this to 1
  FTokenEnd := 1;
  Next;
end;

procedure TSynDemoHlFoldBase.Next;
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

function TSynDemoHlFoldBase.GetEol: Boolean;
begin
  Result := FTokenPos > length(FLineText);
end;

procedure TSynDemoHlFoldBase.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenStart := @FLineText[FTokenPos];
  TokenLength := FTokenEnd - FTokenPos;
end;

function TSynDemoHlFoldBase.GetTokenAttribute: TSynHighlighterAttributes;
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

function TSynDemoHlFoldBase.GetToken: String;
begin
  Result := copy(FLineText, FTokenPos, FTokenEnd - FTokenPos);
end;

function TSynDemoHlFoldBase.GetTokenPos: Integer;
begin
  Result := FTokenPos - 1;
end;

function TSynDemoHlFoldBase.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  // Some default attributes
  case Index of
    SYN_ATTR_COMMENT: Result := fSpecialAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    else Result := nil;
  end;
end;

function TSynDemoHlFoldBase.GetTokenKind: integer;
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


(*   This is an EXACT COPY of SynEditHighlighter

     ONLY the base class is changed to add support for folding
*)

procedure TSynDemoHlContextFoldBase.Next;
begin
  inherited Next;
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = '--') then
    inc(FCurRange);
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = '++') and (FCurRange > 0) then
    dec(FCurRange);
end;

function TSynDemoHlContextFoldBase.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := inherited GetTokenAttribute;
  if (Result = SpecialAttri) and (FCurRange > 0) then
    Result := IdentifierAttribute;
end;


end.

