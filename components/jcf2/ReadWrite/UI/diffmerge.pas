{
 /***************************************************************************
   diffmerge.pas - functions to merge new text with the JCF changes into the
   editor

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Domingo Galmés

  Abstract:
    Methods for creating diffs and applying them to the editor.

  Uses TextDiff
    https://github.com/rickard67/TextDiff
    http://www.angusj.com/delphi/textdiff.html
    Author : Angus Johnson - angusj-AT-myrealbox-DOT-com
    Copyright : © 2001-2008 Angus Johnson
    Updated by : Rickard Johansson (RJ TextEd)
}
unit diffmerge;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, crc,
  // LazUtils
  IntegerList,
  // IdeIntf
  SrcEditorIntf,
  Diff;

procedure DiffMergeEditor(aEdit: TSourceEditorInterface; const aNewText: string; aFromLine: integer = 1; aToLine: integer = maxint);

implementation

function HashLine(const line: string; aIgnoreFinalSpaces: boolean = False): cardinal;
var
  lLen: integer;
begin
  lLen := Length(line);
  if aIgnoreFinalSpaces then
  begin
    while (lLen > 0) and (line[lLen] in [' ', #9]) do
      Dec(lLen);
  end;
  if lLen = 0 then
    Result := CRC32(0, nil, lLen)
  else
    Result := CRC32(0, pbyte(@line[1]), lLen);
end;

//Line number 1 based as TSourceEditorInterface.
procedure FillHashList(aList: TCardinalList; aLines: TStrings; aFirstLine: integer = 1; aLastLine: integer = maxint);
var
  i: integer;
begin
  aList.Clear;
  Dec(aFirstLine);
  Dec(aLastLine);
  if aFirstLine < 0 then
    aFirstLine := 0;
  if aLastLine > aLines.Count - 1 then
    aLastLine := aLines.Count - 1;
  for i := aFirstLine to aLastLine do
    aList.Add(HashLine(aLines[i]));
end;

function EditorGetLastColumn(aEdit: TSourceEditorInterface; aLineNumber: integer): integer;
begin
  Result := length(aEdit.Lines[aLineNumber - 1]) + 1;
end;

procedure EditorDeleteLine(aEdit: TSourceEditorInterface; aLineNumber: integer);
var
  lStartPoint, lEndPoint: Tpoint;
begin
  if (aLineNumber < 0) or (aLineNumber > aEdit.Lines.Count) then
    Exit;
  lStartPoint.X := 1;
  lStartPoint.Y := aLineNumber;
  lEndPoint.X := 1;
  lEndPoint.Y := aLineNumber + 1;
  if lEndPoint.Y > aEdit.Lines.Count then
  begin
    lEndPoint.Y := aLineNumber;
    lEndPoint.X := EditorGetLastColumn(aEdit, aLineNumber);
    if aLineNumber > 1 then
    begin
      lStartPoint.X := EditorGetLastColumn(aEdit, aLineNumber - 1);
      lStartPoint.Y := aLineNumber - 1;
    end;
  end;
  aEdit.ReplaceText(lStartPoint, lEndPoint, '');
end;


procedure EditorInsertLine(aEdit: TSourceEditorInterface; aLineNumber: integer; aText: string);
begin
  aEdit.InsertLine(aLineNumber, aText, True);
end;

procedure EditorReplaceLine(aEdit: TSourceEditorInterface; aLineNumber: integer; aNewText: string);
begin
  aEdit.ReplaceLines(aLineNumber, aLineNumber, aNewText, True);
end;

procedure DiffMergeEditor(aEdit: TSourceEditorInterface; const aNewText: string; aFromLine: integer = 1; aToLine: integer = maxint);
var
  lDiff: TDiff;
  lI: integer;
  lListOldTextHashes, lListNewTextHashes: TCardinalList;
  lDeltaLines: integer;
  lStartLine: integer;
  lNewText: TStringList;
  lCursor: TPoint;
  lCursorAdjusted: boolean;
  lCursorNeedAdjustX: boolean;
begin
  lDiff := nil;
  lNewText := nil;
  lListOldTextHashes := nil;
  lListNewTextHashes := nil;
  lDeltaLines := 0;
  lStartLine := aFromLine - 1;  // 0 based offset.
  lCursor := aEdit.CursorTextXY;
  lCursorAdjusted := False;
  lCursorNeedAdjustX := False;
  try
    aEdit.BeginUpdate;
    aEdit.BeginUndoBlock;
    lDiff := TDiff.Create(nil);
    lNewText := TStringList.Create;
    lNewText.Text := aNewText;
    lListOldTextHashes := TCardinalList.Create;
    lListNewTextHashes := TCardinalList.Create;
    FillHashList(lListOldTextHashes, aEdit.Lines, aFromLine, aToLine);
    FillHashList(lListNewTextHashes, lNewText);
    lDiff.Execute(lListOldTextHashes, lListNewTextHashes);
    for lI := 0 to lDiff.Count - 1 do
    begin
      with lDiff.Compares[lI] do
      begin
        if (lCursorAdjusted = False) and (lCursor.Y = lStartLine + oldIndex1 + 1) then
        begin
          lCursor.Y := lCursor.Y + lDeltaLines;
          lCursorAdjusted := True;
          lCursorNeedAdjustX := Kind <> ckNone;
        end;
        case Kind of
          ckAdd:
          begin
            EditorInsertLine(aEdit, lStartLine + oldIndex2 + 1, lNewText[oldIndex2]);
            Inc(lDeltaLines);
          end;
          ckDelete:
          begin
            EditorDeleteLine(aEdit, lStartLine + oldIndex1 + 1 + lDeltaLines);
            Dec(lDeltaLines);
          end;
          ckModify:
          begin
            EditorReplaceLine(aEdit, lStartLine + oldIndex2 + 1, lNewText[oldIndex2]);
          end;
        end;
      end;
    end;
    if lCursorNeedAdjustX then
      lCursor.X := EditorGetLastColumn(aEdit, lCursor.Y);
    aEdit.CursorTextXY := lCursor;
  finally
    lDiff.Free;
    lListOldTextHashes.Free;
    lListNewTextHashes.Free;
    lNewText.Free;
    aEdit.EndUndoBlock;
    aEdit.EndUpdate;
  end;
end;

end.
