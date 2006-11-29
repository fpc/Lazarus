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
  SynEdit, MacroIntf, LazIDEIntf, SrcEditorIntf;

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
function SubstituteCodeMacros(SrcEdit: TSourceEditorInterface;
  var Pattern: string): boolean;

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
  Pattern: String;
  LineText: String;
begin
  Result:=false;
  //debugln('ExecuteCodeTemplate ',dbgsName(SrcEdit),' ',dbgsName(SrcEdit.EditorControl));
  AEditor:=SrcEdit.EditorControl as TCustomSynEdit;
  
  Pattern:=TemplateValue;
  if copy(Pattern,1,length(CodeTemplateMacroMagic))=CodeTemplateMacroMagic
  then begin
    // macros enabled
    LazarusIDE.SaveSourceEditorChangesToCodeCache(-1);
    
    // remove first line (i.e. macro enabled flag)
    i:=length(CodeTemplateMacroMagic);
    while (i<=length(Pattern)) and (not (Pattern[i] in [#10,#13])) do inc(i);
    if (i<length(Pattern)) and (Pattern[i+1] in [#10,#13])
    and (Pattern[i+1]<>Pattern[i]) then
      inc(i);
    Pattern:=copy(Pattern,i+1,length(Pattern));
    
    if not SubstituteCodeMacros(SrcEdit,Pattern) then exit;
  end;

  AEditor.BeginUpdate;
  try
    // get old caret position in text
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
        and ((IndentLen>length(s)) or (s[IndentLen] in [#9,' '])) do
          inc(IndentLen);
      end;
      IndentLen:=AEditor.LogicalToPhysicalCol(s,IndentLen);// consider tabs
      dec(IndentLen);
    end;
    p := AEditor.BlockBegin;
    NewCaretPos := False;
    Temp := TStringList.Create;
    try
      Temp.Text := Pattern;
      
      // add empty line at end if wanted
      s:=Pattern;
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
    // delete double end separator (e.g. avoid creating two semicolons 'begin end;;')
    if (s<>'') and (System.Pos(s[length(s)],EndOfTokenChr)>0)
    and (AEditor.BlockEnd.Y>0) and (AEditor.BlockEnd.Y<=AEditor.Lines.Count)
    then begin
      // template ends with an EndOfTokenChr
      // check if at the end of selection is the same character
      LineText:=AEditor.Lines[AEditor.BlockEnd.Y-1];
      if copy(LineText,AEditor.BlockEnd.X,1)=s[length(s)] then
        System.Delete(s,length(s),1);
    end;
    // replace the selected text and position the caret
    AEditor.SelText := s;
    // position the caret
    if NewCaretPos then 
      AEditor.MoveCaretIgnoreEOL(p);
    AEditor.EnsureCursorPosVisible;
  finally
    AEditor.EndUpdate;
  end;
  Result:=true;
end;

function SubstituteCodeMacros(SrcEdit: TSourceEditorInterface;
  var Pattern: string): boolean;
  
const
  MaxLevel = 10; // prevent cycling

  function SubstituteMacros(var Pattern: string; Level: integer): boolean; forward;

  function SubstituteMacro(const MacroName, MacroParameter: string;
    var MacroValue: string; Level: Integer): boolean;
  var
    Macro: TIDECodeMacro;
    NewValue: String;
    ErrMsg: string;
  begin
    Result:=false;
    Macro:=IDECodeMacros.FindByName(MacroName);
    //debugln('SubstituteMacro A ',MacroName,' ',dbgs(Macro<>nil),' ',MacroParameter);
    if Macro<>nil then begin
      // macro found
      
      // substitute macros in Parameter
      MacroValue:=MacroParameter;
      if (Level<MaxLevel) and (not SubstituteMacros(MacroValue,Level+1)) then
        exit;

      if Macro.Interactive then begin
        // collect interactive macro
        debugln('SubstituteCodeMacros TODO interactive macros');
      end else begin
        // normal macro -> substitute
        NewValue:='';
        try
          if not Macro.GetValue(MacroValue,nil,SrcEdit,NewValue,ErrMsg) then
            exit;
        except
          exit;
        end;
        MacroValue:=NewValue;
      end;
    end else begin
      // macro unknown
      MacroValue:='UnknownMacro('+MacroName+')';
    end;
    if ErrMsg='' then ;
    Result:=true;
  end;
  
  function SubstituteMacros(var Pattern: string; Level: integer): boolean;
  var
    p: Integer;
    len: Integer;
    MacroStartPos: LongInt;
    MacroParamStartPos: LongInt;
    MacroParamEndPos: LongInt;
    MacroEndPos: LongInt;
    MacroName: String;
    MacroParameter: String;
    MacroValue: string;
  begin
    // replace as many macros as possible
    p:=1;
    len:=length(Pattern);
    while p<len do begin
      if Pattern[p]<>'$' then begin
        inc(p);
      end else begin
        // could be a macro start
        MacroStartPos:=p;
        inc(p);
        if Pattern[p+1]='$' then begin
          // $$ is a simple $ character
          System.Delete(Pattern,p,1);
          len:=length(Pattern);
        end else if Pattern[p+1] in ['a'..'z','A'..'Z'] then begin
          // read macro name
          while (p<len) and (Pattern[p] in ['a'..'z','A'..'Z','0'..'9','_']) do
            inc(p);
          if (p>len) or (p-MacroStartPos=1) or (Pattern[p]<>'(') then begin
            // missing name or missing round bracket open
            debugln('SubstituteMacros missing name or missing round bracket open');
          end else begin
            // round bracket open found
            inc(p);
            MacroParamStartPos:=p;
            Level:=1;
            while (p<=len) and (Level>0) do begin
              case Pattern[p] of
              '(': inc(Level);
              ')': dec(Level);
              end;
              inc(p);
            end;
            if Level=0 then begin
              // macro parameter end found
              MacroParamEndPos:=p-1;
              MacroEndPos:=p;
              MacroName:=copy(Pattern,MacroStartPos+1,
                                            MacroParamStartPos-MacroStartPos-2);
              MacroParameter:=copy(Pattern,MacroParamStartPos,
                                           MacroParamEndPos-MacroParamStartPos);
              if not SubstituteMacro(MacroName,MacroParameter,MacroValue,Level)
              then
                exit(false);
              //debugln('SubstituteMacros MacroName="',MacroName,'" MacroParameter="',MacroParameter,'" MacroValue="',MacroValue,'"');
              Pattern:=copy(Pattern,1,MacroStartPos-1)+MacroValue
                      +copy(Pattern,MacroEndPos,len);
              len:=length(Pattern);
              p:=MacroStartPos+length(MacroValue);
            end else begin
              // macro parameter end not found
              debugln('SubstituteMacros macro parameter end not found');
            end;
          end;
        end else begin
          // a normal $ character
        end;
      end;
    end;
    Result:=true;
  end;
  
begin
  Result:=SubstituteMacros(Pattern,0);
end;

initialization
  {$I codemacroprompt.lrs}

end.

