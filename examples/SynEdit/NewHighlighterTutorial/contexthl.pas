(*
  This is an example how to implement your own highlighter.

  This example extends the Simple HL:
  - It keeps track of the tokens -- and ++ (must be after a space or line-begin to be
    a token of their own).
    Any text surrounded by -- and ++ will
    - NOT apply the attribute(color) for words that start with a,e,i,o,u
    - apply a modifier color to the contained text, fading the color to grey. (merge/blend)

    Multiply ++ and -- can be nested. Then for each -- a ++ must be given,
    before the text-area ends.

  See comments below and http://wiki.lazarus.freepascal.org/SynEdit_Highlighter

*)

(* NOTE:
   This unit implements TSynDemoHl as a highlighter using *ranges*.
   The base class for this does not support folding.

   However, the fold highlighters example need the exact same
   implementation as a base class, except it needs an other base class.

   Therefore this unit, is used several times under different unit names.
*)
unit
{$IFDEF CompileSimpleHl_WithFold}  ContextHLWithFold;
{$ELSE}                            ContextHL;
{$ENDIF}
{$mode objfpc}{$H+}

interface

uses
  Classes,
  LazEditTextAttributes, LazEditHighlighter,
  {$IFDEF CompileSimpleHl_WithFold}  SimpleHlWithFold
  {$ELSE}                            SimpleHlWithRange
  {$ENDIF}
  ;

type

  { TSynDemoHlContext }

  TSynDemoHlContext = class(
    {$IFDEF CompileSimpleHl_WithFold}  SimpleHlWithFold.TSynDemoHl
    {$ELSE}                            SimpleHlWithRange.TSynDemoHl
    {$ENDIF}
  )
  private
    FPlusMinusAttri: TLazEditHighlighterAttributesModifier;
    procedure SetPlusMinusAttri(AValue: TLazEditHighlighterAttributesModifier);
  protected
    FCurRange: Integer;
  public
    procedure Next; override;

    (* GetTokenAttribute: amend result from inherited if inside a --/++ marked text block *)
    function GetTokenAttribute: TLazEditTextAttribute; override;
    (* GetTokenAttributeEx:
       Add a modifier color for --/++ blocks
       This will be merged, so it affects the existing text color.
       - Black will become grey
       - Red (for "not") will become a very light red.

       GetTokenAttribute allows the settings from multiple attributes to be applied together.
    *)
    function GetTokenAttributeEx: TLazCustomEditTextAttribute; override;
  public
    constructor Create(AnOwner: TComponent); override;
    (* ...Range
       Those methods store/retrieve the current state of the text.
       In this example that is, if there was one or more -- that have not yet been closed by ++

       While scanning any of the lines, the Highlighter keeps track for changes within it.
       At the end of the line, the amount of open -- is stored as the state.
       When the highlighter starts scanning a line, it retrieves the state that was
       stored by the line above.
     *)
     (* The fold highlighter includes managing the range for this highlighter.
        - This is because the fold highlighter requires the range for its own,
          and it redirects the storage for the range we have in this class
        - This may be solved different in future...
     *)
    {$IFnDEF CompileSimpleHl_WithFold}
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function GetRange: Pointer; override;
    {$ENDIF}
  published
    property PlusMinusAttri: TLazEditHighlighterAttributesModifier read FPlusMinusAttri
      write SetPlusMinusAttri;
  end;


implementation

{ TSynDemoHlContext }

procedure TSynDemoHlContext.SetPlusMinusAttri(AValue: TLazEditHighlighterAttributesModifier);
begin
  if FPlusMinusAttri = AValue then Exit;
  FPlusMinusAttri := AValue;
end;

procedure TSynDemoHlContext.Next;
begin
  FTokenPos := FTokenEnd;
  (* inherited will gather all NONE-SPACE into a single token.
     We want ++abc to be 2 tokens.
     This does NOT fix abc++, which is not covered by this example
  *)
  if (Length(CurrentLineText) >= FTokenPos + 2) and
     (CurrentLineText[FTokenPos] in ['-', '+']) and
     (CurrentLineText[FTokenPos+1] = CurrentLineText[FTokenPos]) and
     (CurrentLineText[FTokenPos+2] <> CurrentLineText[FTokenPos])
  then
    FTokenEnd := FTokenPos + 2
  else
    inherited Next;

  (* Check if this is a ++ or -- marker *)
  if (copy(CurrentLineText, FTokenPos, FTokenEnd - FTokenPos) = '--') then
    inc(FCurRange);
  if (copy(CurrentLineText, FTokenPos, FTokenEnd - FTokenPos) = '++') and (FCurRange > 0) then
    dec(FCurRange);
end;

function TSynDemoHlContext.GetTokenAttribute: TLazEditTextAttribute;
begin
  Result := inherited GetTokenAttribute;
  if (Result = SpecialAttri) and (FCurRange > 0) then
    Result := IdentifierAttri;
end;

function TSynDemoHlContext.GetTokenAttributeEx: TLazCustomEditTextAttribute;
begin
  Result := GetTokenAttribute;

  (* If between -- and ++ then fade the foreground color to light grey *)
  if FCurRange > 0 then
    MergeModifierToTokenAttribute(Result, FPlusMinusAttri);
end;

constructor TSynDemoHlContext.Create(AnOwner: TComponent);
begin
  inherited Create(AnOwner);

  FPlusMinusAttri := TLazEditHighlighterAttributesModifier.Create('PlusMinus', 'PlusMinus');
  AddAttribute(FPlusMinusAttri);
  FPlusMinusAttri.Foreground := $C0C0C0;
  FPlusMinusAttri.ForeAlpha := 192;
  SetAttributesOnChange(@DefHighlightChange);
end;

{$IFnDEF CompileSimpleHl_WithFold}

procedure TSynDemoHlContext.SetRange(Value: Pointer);
begin
  // Set the current range (for current line)
  // The value is provided from an internal storage, where it was kept since the last scan
  // This is the and value of the previous line, which is used as start for the new line
  FCurRange := PtrInt(Value);
end;

procedure TSynDemoHlContext.ResetRange;
begin
  FCurRange := 0;
end;

function TSynDemoHlContext.GetRange: Pointer;
begin
  // Get a store-able copy of the current (working) range
  Result := Pointer(PtrInt(FCurRange));
end;

{$ENDIF}

end.


