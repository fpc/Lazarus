{
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
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Functions to substitute code macros.
    Dialog to setup interactive macros by the programmer.
}
unit CodeMacroPrompt;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  SynEdit, MacroIntf, SrcEditorIntf;

type
  TCodeMacroPromptDlg = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end; 

function ExecuteCodeTemplate(SrcEdit: TSourceEditorInterface;
  const TemplateName, TemplateValue, TemplateComment,
  EndOfTokenChr: string;
  IndentToTokenStart: boolean): boolean;

implementation

function ExecuteCodeTemplate(SrcEdit: TSourceEditorInterface;
  const TemplateName, TemplateValue, TemplateComment,
  EndOfTokenChr: string;
  IndentToTokenStart: boolean): boolean;
var
  AEditor: TCustomSynEdit;
  p: TPoint;
  TokenStartX: LongInt;
  s: string;
  NewCaretPos: Boolean;
  Temp: TStringList;
  IndentLen: Integer;
  i: Integer;
  j: LongInt;
begin
  Result:=false;
  //debugln('ExecuteCodeTemplate ',dbgsName(SrcEdit),' ',dbgsName(SrcEdit.EditorControl));
  AEditor:=SrcEdit.EditorControl as TCustomSynEdit;
  
  AEditor.BeginUpdate;
  try
    // get caret position in text
    p := AEditor.LogicalCaretXY;

    // select token in editor
    TokenStartX:=p.x;
    s:=AEditor.Lines[p.y-1];
    if TokenStartX>length(s) then
      TokenStartX:=length(s);
    while (TokenStartX > 1) and (s[TokenStartX-1] > ' ')
    and (Pos(s[TokenStartX-1], EndOfTokenChr) = 0) do
      Dec(TokenStartX);
    AEditor.BlockBegin := Point(TokenStartX, p.y);
    AEditor.BlockEnd := p;
    
    // indent the completion string if necessary, determine the caret pos
    if IndentToTokenStart then begin
      IndentLen := TokenStartX - 1;
    end else begin
      // indent the same as the first line
      IndentLen:=1;
      if (p.y>0) and (p.y<=AEditor.Lines.Count) then begin
        s:=AEditor.Lines[p.y-1];
        while (IndentLen<p.x)
        and ((IndentLen>length(s)) or (s[IndentLen]<=' ')) do
          inc(IndentLen);
      end;
      IndentLen:=AEditor.LogicalToPhysicalCol(s,IndentLen);// consider tabs
      dec(IndentLen);
    end;
    p := AEditor.BlockBegin;
    NewCaretPos := False;
    Temp := TStringList.Create;
    try
      Temp.Text := TemplateValue;
      
      // add empty line at end if wanted
      s:=TemplateValue;
      if (s<>'') and (s[length(s)] in [#10,#13]) then
        Temp.Add('');

      // indent lines
      if (IndentLen > 0) and (Temp.Count > 1) then
      begin
        s := StringOfChar(' ', IndentLen);
        for i := 1 to Temp.Count - 1 do
          Temp[i] := s + Temp[i];
      end;
      // find first '|' and use it as caret position
      for i := 0 to Temp.Count - 1 do
      begin
        s := Temp[i];
        j := Pos('|', s);
        if j > 0 then
        begin
          Delete(s, j, 1);
          Temp[i] := s;
          NewCaretPos := TRUE;
          Inc(p.y, i);
          if i = 0 then
            Inc(p.x, j - 1)
          else
            p.x := j;
          break;
        end;
      end;
      s := Temp.Text;
      // strip the trailing #13#10 that was appended by the stringlist
      i := Length(s);
      if (i>=1) and (s[i] in [#10,#13]) then begin
        dec(i);
        if (i>=1) and (s[i] in [#10,#13]) and (s[i]<>s[i+1]) then
          dec(i);
        SetLength(s, i);
      end;
    finally
      Temp.Free;
    end;
    // replace the selected text and position the caret
    AEditor.SelText := s;
    // position the caret
    if NewCaretPos then
      AEditor.CaretXY := p;
    AEditor.EnsureCursorPosVisible;
  finally
    AEditor.EndUpdate;
  end;
  Result:=true;
end;

initialization
  {$I codemacroprompt.lrs}

end.

