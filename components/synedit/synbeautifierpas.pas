{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.
-------------------------------------------------------------------------------}
unit SynBeautifierPas;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, LCLProc, SynEdit;
  
type

  { TSynBeautifierPas }

  TSynBeautifierPas = class(TSynCustomBeautifier)
  public
    function GetIndentForLineBreak(Editor: TCustomSynEdit;
                    InsertPos: TPoint; var NextText: string): integer; override;
  end;

implementation

{ TSynBeautifierPas }

function TSynBeautifierPas.GetIndentForLineBreak(Editor: TCustomSynEdit;
  InsertPos: TPoint; var NextText: string): integer;
var
  LastTextY: LongInt;
  Line: string;
  Lines: TStrings;
begin
  Result:=0;
  //DebugLn(['TSynCustomBeautifier.GetIndentForLineBreak ',dbgs(InsertPos)]);
  NextText:=Trim(NextText);
  if InsertPos.Y<1 then exit;
  LastTextY:=InsertPos.Y;
  Lines:=Editor.Lines;
  if LastTextY>Lines.Count then
    LastTextY:=Lines.Count;
  while (LastTextY>0) do begin
    Line:=Lines[LastTextY-1];
    if LastTextY=InsertPos.Y then
      Line:=copy(Line,1,InsertPos.X-1);
    if Line<>'' then begin
      Result:=LeftSpaces(Editor,Line,false);
      exit;
    end;
    dec(LastTextY);
  end;
end;

end.

