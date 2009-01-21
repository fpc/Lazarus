unit SynBeautifier;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, SynEditMiscClasses;

type
  { TSynCustomBeautifier }

  TSynCustomBeautifier = class(TComponent)
  public
    function CanUnindent(const Editor: TSynEditBase;  const Line: string;
                         const PhysCaretX: Integer): Boolean; virtual; abstract;
    function UnIndentLine(const Editor: TSynEditBase;  const Line: string;
                          const PhysCaret: TPoint; out DelChars, InsChars: String;
                          out CaretNewX: Integer): String; virtual; abstract;   // Todo InsChar are not supprted for undo
    function IndentLine(const Editor: TSynEditBase; Line: string;
                        const PhysCaret: TPoint; out DelChars, InsChars: String;
                        out CaretNewX: Integer;
                        RemoveCurrentIndent: Boolean = False): String;
                        virtual; abstract;                                      // Todo DelChar are not supprted for undo
    function GetIndentForLine(Editor: TSynEditBase; const Line: string;
                              const PhysCaret: TPoint): Integer; virtual; abstract;
  end;

  TSynBeautifierIndentType = (sbitSpace, sbitCopySpaceTab, sbitPositionCaret);

  { TSynBeautifier }

  TSynBeautifier = class(TSynCustomBeautifier)
  private
    FIndentType: TSynBeautifierIndentType;
  public
    function LeftSpaces(Editor: TSynEditBase; const Line: string;
                        Physical: boolean): Integer;
    function CanUnindent(const Editor: TSynEditBase;  const Line: string;
                         const PhysCaretX: Integer): Boolean; override;
    function UnIndentLine(const Editor: TSynEditBase;  const Line: string;
                          const PhysCaret: TPoint; out DelChars, InsChars: String;
                          out CaretNewX: Integer): String; override;            // Todo InsChar are not supprted for undo
    function IndentLine(const Editor: TSynEditBase; Line: string;
                        const PhysCaret: TPoint; out DelChars, InsChars: String;
                        out CaretNewX: Integer;
                        RemoveCurrentIndent: Boolean = False): String; override; // Todo DelChar are not supprted for undo
    function GetIndentForLine(Editor: TSynEditBase; const Line: string;
                              const PhysCaret: TPoint): Integer; override;
  published
    property IndentType: TSynBeautifierIndentType read FIndentType write FIndentType;
  end;

implementation
uses SynEdit;

{ TSynBeautifier }

function TSynBeautifier.LeftSpaces(Editor: TSynEditBase; const Line: string; Physical: boolean): Integer;
var
  p: PChar;
begin
  p := pointer(Line);
  if Assigned(p) then begin
    Result := 0;
    while p^ in [#1..#32] do begin
      Inc(p);
      Inc(Result);
    end;
    if Physical and (Result>0) then
      Result := TSynEdit(Editor).LogicalToPhysicalCol(Line, Result+1)-1;
  end else
    Result := 0;
end;

function TSynBeautifier.CanUnindent(const Editor: TSynEditBase; const Line: string; const PhysCaretX: Integer): Boolean;
begin
  Result := (LeftSpaces(Editor, Line, True) = PhysCaretX - 1);
end;

function TSynBeautifier.UnIndentLine(const Editor: TSynEditBase; const Line: string; const PhysCaret: TPoint; out DelChars, InsChars: String; out
  CaretNewX: Integer): String;
var
  SpaceCount1, SpaceCount2: Integer;
  BackCounter, LogSpacePos: Integer;
begin
  SpaceCount1 := LeftSpaces(Editor, Line, true);
  SpaceCount2 := 0;
  if (SpaceCount1 > 0) then begin
    BackCounter := PhysCaret.Y - 2;
    while BackCounter >= 0 do begin
      SpaceCount2 := LeftSpaces(Editor, Editor.RealLines[BackCounter], true);
      if SpaceCount2 < SpaceCount1 then
        break;
      Dec(BackCounter);
    end;
  end;
  if SpaceCount2 = SpaceCount1 then
    SpaceCount2 := 0;
  // remove visible spaces
  LogSpacePos := TSynEdit(Editor).PhysicalToLogicalCol(Line, SpaceCount2 + 1);
  CaretNewX :=  SpaceCount2 + 1;
  DelChars := copy(Line, LogSpacePos, PhysCaret.X - LogSpacePos);
  InsChars := ''; // TODO: if tabs were removed, maybe fill-up with spaces
  Result :=copy(Line, 1, LogSpacePos-1) + copy(Line, PhysCaret.X, MaxInt);
end;

function TSynBeautifier.IndentLine(const Editor: TSynEditBase; Line: string; const PhysCaret: TPoint; out DelChars, InsChars: String; out
  CaretNewX: Integer; RemoveCurrentIndent: Boolean): String;
var
  SpaceCount1, SpaceCount2: Integer;
  BackCounter: Integer;
  Temp: string;
begin
  DelChars := '';
  If RemoveCurrentIndent then begin
    SpaceCount1 := LeftSpaces(Editor, Line, False);
    DelChars := copy(Line, 1, SpaceCount1);
    Line := copy(Line, SpaceCount1 + 1, MaxInt);
  end;

  SpaceCount2 := 0;
  BackCounter := PhysCaret.Y - 1;
  if BackCounter > 0 then
    repeat
      Dec(BackCounter);
      Temp := Editor.RealLines[BackCounter];
      SpaceCount2 := LeftSpaces(Editor, Temp, True);
    until (BackCounter = 0) or (Temp <> '');

  case FIndentType of
    sbitSpace:
      InsChars := StringOfChar(' ', SpaceCount2);
    sbitCopySpaceTab:
      InsChars := copy(Temp, 1, TSynEdit(Editor).PhysicalToLogicalCol(Temp, SpaceCount2+1)-1);
    sbitPositionCaret:
      if Line <> '' then
        InsChars := StringOfChar(' ', SpaceCount2)
      else
        InsChars := '';
  end;
  Result := InsChars + Line;
  CaretNewX := TSynEdit(Editor).LogicalToPhysicalCol(Result, SpaceCount2+1);
end;

function TSynBeautifier.GetIndentForLine(Editor: TSynEditBase; const Line: string; const PhysCaret: TPoint): Integer;
var
  s1, s2: string;
begin
  IndentLine(Editor, Line, PhysCaret, s1, s2, Result, False);
end;

end.

