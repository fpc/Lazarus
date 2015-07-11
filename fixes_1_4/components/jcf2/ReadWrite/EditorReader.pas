unit EditorReader;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is EditorReader.pas, released January 2001.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2001 Anthony Steele.
All Rights Reserved. 
Contributor(s):
Anthony Steele.
Walter Prins

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

{ reader class for use in IDE pluggin - reads from editor interface }

uses
  { delphi design time }ToolsAPI,
  { local }CodeReader;

type



  TEditorReader = class(TCodeReader)
  private
    fciUnit: IOTASourceEditor;
  protected
    procedure ReadFromSource; override;

  public
    constructor Create; override;
    procedure SetEditorUnit(const pciUnit: IOTASourceEditor);
  end;


implementation

constructor TEditorReader.Create;
begin
  inherited;
  fciUnit := nil;
end;

procedure TEditorReader.SetEditorUnit(const pciUnit: IOTASourceEditor);
begin
  fciUnit := pciUnit;
end;

procedure TEditorReader.ReadFromSource;
const
  // 10 kb at a time should do it
  BUF_SIZE = 10240;
 //BUF_SIZE = 120; // small for testing
var
  lciEditorReader: IOTAEditReader;
  lsBuf:  string;
  lpBuf:  pchar;
  liActualSize, liPos: integer;
  lbDone: boolean;
  //liLoopCount: integer;
begin
  { get a reader from the unit }
  Assert(fciUnit <> nil);
  lciEditorReader := fciUnit.CreateReader;
  Assert(lciEditorReader <> nil);

  fsSource := '';

  // read it all. Unfortunately the API dictates that we will work in chunks

  liPos := 0;
  //liLoopCount := 0;

  lbDone := False;

  while not lbDone do
  begin
    // clear the buffer
    SetLength(lsBuf, BUF_SIZE);
    lpBuf := pchar(lsBuf);
    FillChar(lpBuf^, BUF_SIZE, 0);

    // get some text into the buffer
    liActualSize := lciEditorReader.GetText(liPos, lpBuf, BUF_SIZE);

    // store it
    {WP: Do not add the entire lsBuf to fsSource, as in cases where the entire source is less
     than 10Kb in total, there will be junk in the last part of the buffer.
     If this is copied, it shows up as extraneous tokens in the token list
     after the end of the unit proper.
     This then causes an assertion failure in procedure DoConvertUnit in unit Converter.pas,
     When these extra tokens are found that were not consumed by BuildParseTree

     The way is to ensure that you only append as many characters as you've actually read (liActualSize bytes)
     from the buffer into the fSource. }
    fsSource := fsSource + Copy(lsBuf, 1, liActualSize);
      //WP: Changed from just adding lsBuf

    // more stuff to read after this?
    liPos  := liPos + liActualSize;
    lbDone := (liActualSize < BUF_SIZE);
    //inc(liLoopCount);
  end;

  { release the reader -  must happen before the writer is used }
  lciEditorReader := nil;

  // debug ShowMessage(fsSource);
  //ShowMessage('read in ' + IntToStr(liLoopCount));
  fiSourceLength := Length(fsSource);
  fiReadIndex    := 1;
  fiBufferLength := 1;
  fbHasRead      := True;
end;

end.
