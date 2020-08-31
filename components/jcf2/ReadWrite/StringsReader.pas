unit StringsReader;

{
  AFS 1 Jan 2003
  Attach the formatter to TStrings
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is StringsReader, released May 2003.
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

{$I JcfGlobal.inc}

interface

uses
  Classes,
  { local }
  CodeReader;

type
  TStringsReader = class(TCodeReader)
  private
    { property implementation }
    FcInputStrings: TStrings;


  protected
    procedure ReadFromSource; override;
  public
    procedure Clear; override;

    property InputStrings: TStrings Read FcInputStrings Write FcInputStrings;
  end;


implementation

{ TSTringsReader }

procedure TStringsReader.Clear;
begin
  inherited;
  FcInputStrings := nil;
end;

procedure TStringsReader.ReadFromSource;
begin
  if fbHasRead then
    exit;

  // Open the file
  Assert((FcInputStrings <> nil), 'No source strings');

  fsSource := FcInputStrings.Text;

  fiSourceLength := Length(fsSource);

  fiReadIndex    := 1;
  fiBufferLength := 1;
  fbHasRead      := True;
end;

end.
