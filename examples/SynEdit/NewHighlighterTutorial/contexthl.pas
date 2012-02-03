unit ContextHL;
(*
  This is an example how to implement your own highlighter.

  This example extends the Simple HL:
  - The token -- and ++ (must be surrounded by space or line-begin/end to be
    a token of their own) will toggle words that start with a,e,i,o,u

    Multply ++ and -- can be nested. Then for each -- a ++ must be given,
    before the words highlicht again

  See comments below and http://wiki.lazarus.freepascal.org/SynEdit_Highlighter


*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, SynEditTypes, SynEditHighlighter, SimpleHl;

type

  { TSynDemoHlContext }

  TSynDemoHlContext = class(TSynDemoHl)
  protected
    FCurRange: Integer;
  public
    procedure Next; override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
  public
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
    function GetRange: Pointer; override;
  end;


implementation

{ TSynDemoHlContext }

procedure TSynDemoHlContext.Next;
begin
  inherited Next;
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = '--') then
    inc(FCurRange);
  if (copy(FLineText, FTokenPos, FTokenEnd - FTokenPos) = '++') and (FCurRange > 0) then
    dec(FCurRange);
end;

function TSynDemoHlContext.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := inherited GetTokenAttribute;
  if (Result = SpecialAttri) and (FCurRange > 0) then
    Result := IdentifierAttribute;
end;

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
  // Get a storable copy of the cuurent (working) range
  Result := Pointer(PtrInt(FCurRange));
end;

end.


