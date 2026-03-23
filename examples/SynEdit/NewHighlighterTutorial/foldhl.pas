unit FoldHl;
(*
  This is an example how to implement your own highlighter.

  This example extends the Simple and Context HL:
  - The token -(- and -)- (must be surrounded by space or line-begin/end to be
    a token of their own) will add foldable sections

    Multiply -(- and -)- can be nested.

  See comments below and http://wiki.lazarus.freepascal.org/SynEdit_Highlighter

*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  LazEditHighlighter,
  SynEditHighlighterFoldBase,
  ContextHLWithFold;

type

  TSynDemoHlFold = class(ContextHLWithFold.TSynDemoHlContext)
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
  if (copy(CurrentLineText, FTokenPos, FTokenEnd - FTokenPos) = '-(-') then
    StartCodeFoldBlock(nil);
  if (copy(CurrentLineText, FTokenPos, FTokenEnd - FTokenPos) = '-)-') then
    EndCodeFoldBlock;
end;

procedure TSynDemoHlFold.SetRange(Value: Pointer);
begin
  // must call the SetRange in TSynCustomFoldHighlighter
  // manage the range for TSynDemoHlContext
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



end.

