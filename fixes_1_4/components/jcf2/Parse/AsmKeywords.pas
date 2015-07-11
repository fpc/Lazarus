unit AsmKeywords;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is AsmKeywords, released November 2007.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele, Adem Baba

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

{$I JcfGlobal.inc}

interface

{ identify asm parameter keywords }

function IsAsmParamKeyword(const word: string): boolean;

implementation

uses
  SysUtils, Classes;

var
  mcWords: TStringList = nil;

procedure CreateWords;
begin

  {
  The list of reserved keywords for parameters contains names of CPU internal registers:
  RAX, EAX, AX, AL, AH, RBX, EBX, BX, BL, BH,
  RCX, ECX, CX, CL, CH, RDX, EDX, DX, DL, DH,
  RSI, ESI, SIL, SI, RDI, EDI, DIL, DI, RBP,
    ...
  }

  mcWords := TStringList.Create();

  mcWords.Add('RAX');
  mcWords.Add('EAX');
  mcWords.Add('AX');
  mcWords.Add('AL');
  mcWords.Add('AH');
  mcWords.Add('RBX');
  mcWords.Add('EBX');
  mcWords.Add('BX');
  mcWords.Add('BL');
  mcWords.Add('BH');

  mcWords.Add('RCX');
  mcWords.Add('ECX');
  mcWords.Add('CX');
  mcWords.Add('CL');
  mcWords.Add('CH');
  mcWords.Add('RDX');
  mcWords.Add('EDX');
  mcWords.Add('DX');
  mcWords.Add('DL');
  mcWords.Add('DH');
  
  mcWords.Add('RSI');
  mcWords.Add('ESI');
  mcWords.Add('SIL');
  mcWords.Add('SI');
  mcWords.Add('RDI');
  mcWords.Add('EDI');
  mcWords.Add('DIL');
  mcWords.Add('DI');
  mcWords.Add('RBP');

  {
   ...
  EBP, BPL, BP, RSP, ESP, SPL, SP, CS, DS, ES, FS, GS, EIP,
  R8L --> R15L, R8W -->  R15W, R8D --> R15D, R8 --> R15,
  ...
  }

  mcWords.Add('EBP');
  mcWords.Add('BPL');
  mcWords.Add('BP');
  mcWords.Add('RSP');
  mcWords.Add('ESP');
  mcWords.Add('SPL');
  mcWords.Add('SP');
  mcWords.Add('CS');
  mcWords.Add('DS');
  mcWords.Add('ES');
  mcWords.Add('FS');
  mcWords.Add('GS');
  mcWords.Add('EIP');

  mcWords.Add('R8L');
  mcWords.Add('R9L');
  mcWords.Add('R10L');
  mcWords.Add('R11L');
  mcWords.Add('R12L');
  mcWords.Add('R13L');
  mcWords.Add('R14L');
  mcWords.Add('R15L');

  mcWords.Add('R8W');
  mcWords.Add('R9W');
  mcWords.Add('R10W');
  mcWords.Add('R11W');
  mcWords.Add('R12W');
  mcWords.Add('R13W');
  mcWords.Add('R14W');
  mcWords.Add('R15W');

  mcWords.Add('R8D');
  mcWords.Add('R9D');
  mcWords.Add('R10D');
  mcWords.Add('R11D');
  mcWords.Add('R12D');
  mcWords.Add('R13D');
  mcWords.Add('R14D');
  mcWords.Add('R15D');

  mcWords.Add('R8');
  mcWords.Add('R9');
  mcWords.Add('R10');
  mcWords.Add('R11');
  mcWords.Add('R12');
  mcWords.Add('R13');
  mcWords.Add('R14');
  mcWords.Add('R15');

  {
  ...
  ST, MM0 --> MM7, XMM0 --> XMM15
  }

  mcWords.Add('ST');
  
  mcWords.Add('MM0');
  mcWords.Add('MM1');
  mcWords.Add('MM2');
  mcWords.Add('MM3');
  mcWords.Add('MM4');
  mcWords.Add('MM5');
  mcWords.Add('MM6');
  mcWords.Add('MM7');

  mcWords.Add('XMM0');
  mcWords.Add('XMM1');
  mcWords.Add('XMM2');
  mcWords.Add('XMM3');
  mcWords.Add('XMM4');
  mcWords.Add('XMM5');
  mcWords.Add('XMM6');
  mcWords.Add('XMM7');
  mcWords.Add('XMM8');
  mcWords.Add('XMM9');
  mcWords.Add('XMM10');
  mcWords.Add('XMM11');
  mcWords.Add('XMM12');
  mcWords.Add('XMM13');
  mcWords.Add('XMM14');
  mcWords.Add('XMM15');

  mcWords.Sort;
end;

function IsAsmParamKeyword(const word: string): boolean;
begin
  if mcWords = nil then
  begin
    CreateWords;
  end;

  Result := (mcWords.IndexOf(word) >= 0);

end;

initialization


finalization
  FreeAndNil(mcWords);
end.
