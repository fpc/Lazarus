unit EditorWriter;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is EditorWriter.pas, released January 2001.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 2001 Anthony Steele.
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

{ writer class for use in IDE pluggin - writes to the editor interface }

uses
  { delphi design time }ToolsAPI,
  { local }CodeWriter;

type

  TEditorWriter = class(TCodeWriter)
  private
    fciUnit: IOTASourceEditor;
  protected
  public

    procedure Close; override;

    constructor Create; override;
    procedure SetEditorUnit(const pciUnit: IOTASourceEditor);

  end;

implementation

constructor TEditorWriter.Create;
begin
  inherited;
  fciUnit := nil;
end;

procedure TEditorWriter.SetEditorUnit(const pciUnit: IOTASourceEditor);
begin
  fciUnit := pciUnit;
end;

procedure TEditorWriter.Close;
var
  lciEditorWriter: IOTAEditWriter;
//  liEndPos: integer;
begin
  if fciUnit = nil then
    exit;

  lciEditorWriter := fciUnit.CreateUndoableWriter;
  Assert(lciEditorWriter <> nil);

  if lciEditorWriter = nil then
    exit;

  BeforeWrite;

  //debug ShowMessage(fsDestText);

  { these next 2 steps should rather be done in one operation
    so as to be unitary in the undo history
    but I don't know how to do that, or if it is possible }

  { delete what's there }
  lciEditorWriter.DeleteTo(High(integer));
  { put the changed text in instead }
  lciEditorWriter.Insert(pchar(fsDestText));

  { delete after the 'end.' }
  //liEndPos := PosOfLastSolidText(fsDestText);
  //lciEditorWriter.CurrentPos

  // ditch the interfaces
  lciEditorWriter := nil;
  fciUnit := nil;

  fsDestText := '';
end;

end.
