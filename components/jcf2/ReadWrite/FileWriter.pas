{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is Writer.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele. 
Portions created by Anthony Steele are Copyright (C) 1999-2000 Anthony Steele.
All Rights Reserved. 
Contributor(s): Anthony Steele. 

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

unit FileWriter;

{ AFS 28 November 1999
  Writer - final output stage of code formattter

  AFS 22 July 2K - optimised by using a string to store tokens,
  and writing the file at once
  }

{$I JcfGlobal.inc}

interface

uses SysUtils, CodeWriter;

type
  TFileWriter = class(TCodeWriter)
  private
    { properties }
    FOutputFileName: string;
    procedure SetOutputFileName(const Value: string);

  protected

  public
    constructor Create; override;

    procedure Close; override;

    property OutputFileName: string Read FOutputFileName Write SetOutputFileName;
  end;

implementation

constructor TFileWriter.Create;
begin
  inherited;
  FOutputFileName := '';
end;

procedure TFileWriter.SetOutputFileName(const Value: string);
begin
  FOutputFileName := Value;
end;

procedure TFileWriter.Close;
var
  lfOutput: file;
  pChars:   Pointer;
begin
  if BOF then
    exit;

  Assert(OutputFileName <> '');
  Assert( not FileExists(OutputFileName));

  BeforeWrite;
  pChars := pchar(fsDestText);

  { write the file }
  AssignFile(lfOutput, OutputFileName);
  Rewrite(lfOutput, 1);
  {$PUSH}
  {$WARNINGS OFF}
  BlockWrite(lfOutput, pChars^, Length(fsDestText));
  {$POP}
  CloseFile(lfOutput);

  { reset state }
  FOutputFileName := '';
  fsDestText := '';
  fbBOF := True;
end;

end.
