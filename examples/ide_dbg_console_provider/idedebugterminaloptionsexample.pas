{
  debugterminaloptions.pas
  ------------------------
  Presentation options for the pluggable in-IDE debug terminal panel.

  Deliberately self-contained (RTL-only): the terminal line-ending, local-echo
  and backspace-key concepts are generic to any character terminal, so they live
  here rather than being borrowed from any project-specific unit. This keeps the
  lazdebugterminal package free of external coupling and a clean candidate to
  move into the Lazarus source tree later.

  SPDX-License-Identifier: MIT
}
unit IdeDebugTerminalOptionsExample;

{$mode objfpc}{$H+}

interface

{$SCOPEDENUMS ON}
type
  { Wire line-ending bytes sent to the target when the user presses Enter. The
    on-screen newline a panel may echo locally is a separate concern (always a
    visual LF); this enum is purely the wire line-ending.
      CR #13   LF #10   CRLF #13#10   None (send nothing) }
  TDebugTerminalLineEnding = (CR, LF, CRLF, None);

  { Byte sent to the target when the user presses Backspace. BS = #8 (the common
    bare-metal choice); DEL = #127 (what many serial REPLs expect). }
  TDebugTerminalBackspaceKey = (BS, DEL);
{$SCOPEDENUMS OFF}

type
  { Presentation options applied to the terminal form. Built by the provider
    from its settings bag. Colours are held as plain LongInt (a TColor
    bit-pattern) so this record stays LCL-free; the form casts to TColor. }
  TDebugTerminalDisplayOptions = record
    LineEnding:      TDebugTerminalLineEnding;
    LocalEcho:       Boolean;
    Backspace:       TDebugTerminalBackspaceKey;
    BackgroundColor: LongInt;   { TColor bit-pattern; default clBlack $000000 }
    ForegroundColor: LongInt;   { TColor bit-pattern; default clWhite $FFFFFF }
  end;

{ The wire bytes a line-ending sends on Enter ('' for None). }
function LineEndingBytes(E: TDebugTerminalLineEnding): string;
{ The wire byte a backspace key sends on Backspace. }
function BackspaceByte(B: TDebugTerminalBackspaceKey): string;

implementation

function LineEndingBytes(E: TDebugTerminalLineEnding): string;
begin
  case E of
    TDebugTerminalLineEnding.CR:   Result := #13;
    TDebugTerminalLineEnding.LF:   Result := #10;
    TDebugTerminalLineEnding.CRLF: Result := #13#10;
  else
    Result := '';   { None }
  end;
end;

function BackspaceByte(B: TDebugTerminalBackspaceKey): string;
begin
  if B = TDebugTerminalBackspaceKey.DEL then
    Result := #127
  else
    Result := #8;   { BS }
end;

end.
