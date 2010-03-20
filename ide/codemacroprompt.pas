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
  Classes, SysUtils, LCLProc, Forms, Controls, Graphics, Dialogs,
  SynEditAutoComplete, SynPluginTemplateEdit, SynPluginSyncronizedEditBase, SynEdit,
  MacroIntf, LazIDEIntf, SrcEditorIntf;

type
  TCodeMacroPromptDlg = class(TForm)
  private
    { private declarations }
  public
    { public declarations }
  end;

  TLazSynPluginSyncronizedEditCell = class(TSynPluginSyncronizedEditCell)
  public
    CellValue: String;
  end;

  { TLazSynPluginSyncronizedEditList }

  TLazSynPluginSyncronizedEditList = class(TSynPluginSyncronizedEditList)
  private
  public
    function AddNew: TSynPluginSyncronizedEditCell; override;
  end;

  { TLazTemplateParser }

  TLazTemplateParser = class(TIDETemplateParser)
  private
    FCaret: TPoint;
    FEditCellList: TSynPluginSyncronizedEditList;
    FEnableMacros: Boolean;
    FIndent: String;
    FSrcTemplate: String;
    FDestTemplate: String;
    FSrcPosition: Integer;
    FDestPosition: Integer;
    FDestPosX: Integer;
    FDestPosY: Integer;
    FLevel: Integer;
    FSrcEdit: TSourceEditorInterface;
  protected
    // nested macros, get the X pos of the outer macro
    function GetSrcPosition: Integer; override;
    function GetDestPosition: Integer; override;
    function GetDestPosX: Integer; override;
    function GetDestPosY: Integer; override;
    function GetSrcTemplate: String; override;
    function GetDestTemplate: String; override;

    function SubstituteMacro(const MacroName, MacroParameter: string;
                             var MacroValue: string): boolean;
    function SubstituteMacros(var Template: String): boolean;
  public
    constructor Create(TheTemplate: String);
    destructor Destroy; override;

    function SubstituteCodeMacros(SrcEdit: TSourceEditorInterface): boolean;
    procedure TrimEOTChar(eot: Char);

    property EnableMacros: Boolean read FEnableMacros write FEnableMacros;
    property Indent: String read FIndent write FIndent;
    property DestCaret: TPoint read FCaret;

    property EditCellList: TSynPluginSyncronizedEditList read FEditCellList;
  end;

function ExecuteCodeTemplate(SrcEdit: TSourceEditorInterface;
  const TemplateName, TemplateValue, TemplateComment,
  EndOfTokenChr: string; Attributes: TStrings;
  IndentToTokenStart: boolean): boolean;

implementation

{$R *.lfm}

const
  MaxLevel = 10; // prevent cycling

{ TLazTemplateParser }

constructor TLazTemplateParser.Create(TheTemplate: String);
begin
  inherited Create;
  FEditCellList := TLazSynPluginSyncronizedEditList.Create;
  FSrcTemplate := TheTemplate;
  FDestTemplate := '';
  FSrcPosition := 1;
  FDestPosition := 1;
  FDestPosX := 1;
  FDestPosY := 1;
  FLevel := 0;
  FCaret.y := -1;
end;

destructor TLazTemplateParser.Destroy;
begin
  FEditCellList.Free;
  inherited Destroy;
end;

function TLazTemplateParser.GetSrcPosition: Integer;
begin
  Result := FSrcPosition;
end;

function TLazTemplateParser.GetDestPosition: Integer;
begin
  Result := FDestPosition;
end;

function TLazTemplateParser.GetDestPosX: Integer;
begin
  Result := FDestPosX;
end;

function TLazTemplateParser.GetDestPosY: Integer;
begin
  Result := FDestPosY;
end;

function TLazTemplateParser.GetSrcTemplate: String;
begin
  Result := FSrcTemplate;
end;

function TLazTemplateParser.GetDestTemplate: String;
begin
  Result := FDestTemplate;
end;

function TLazTemplateParser.SubstituteMacro(const MacroName, MacroParameter: string;
  var MacroValue: string): boolean;
var
  Macro: TIDECodeMacro;
  NewValue: String;
  ErrMsg: string;
begin
  Result := false;
  Macro := IDECodeMacros.FindByName(MacroName);
  //debugln('SubstituteMacro A ',MacroName,' ',dbgs(Macro<>nil),' ',MacroParameter);
  if Macro <> nil then begin
    // substitute macros in Parameter
    MacroValue := MacroParameter;
    if (FLevel < MaxLevel) and (not SubstituteMacros(MacroValue)) then
      exit;

    if Macro.Interactive then begin
      // collect interactive macro
      debugln('SubstitutMacros TODO interactive macros');
    end else begin
      // normal macro -> substitute
      NewValue:='';
      try
        if not Macro.GetValue(MacroValue, nil, FSrcEdit, NewValue, ErrMsg, self)
        then
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

function TLazTemplateParser.SubstituteMacros(var Template: String): boolean;

  procedure AppentToDest(S: String);
  var
    i, i2: Integer;
  begin
    i := 1;
    i2 := 1;
    while i <= length(S) do begin
      case s[i] of
        #10, #13:
          begin
            inc(i);
            if (i <= length(S)) and (s[i] in [#10,#13]) and (s[i] <> s[i-1]) then
              inc(i);
            if (FDestTemplate <> '') and (i > i2) and
               (FDestTemplate[length(FDestTemplate)] in [#10, #13])
            then
              FDestTemplate := FDestTemplate + FIndent;
            FDestTemplate := FDestTemplate + copy(s, i2, i - i2);
            i2 := i;
            FDestPosX := 1 + length(FIndent);
            inc(FDestPosY);
          end;
        else
          begin
            if (s[i] = '|') and (FCaret.y < 0) then begin
              System.Delete(s, i, 1);
              FCaret.y := FDestPosY;
              FCaret.x := FDestPosX;
            end
            else begin
              inc(i);
              inc(FDestPosX);
            end;
        end;
      end;
    end;
    if (FDestTemplate <> '') and (i > i2) and
       (FDestTemplate[length(FDestTemplate)] in [#10, #13])
    then
      FDestTemplate := FDestTemplate + FIndent;
    FDestTemplate := FDestTemplate + copy(s, i2, i - i2);
    FDestPosition := length(FDestTemplate);
  end;

var
  p: LongInt;
  len: Integer;
  SrcCopiedPos: LongInt;
  MacroStartPos: LongInt;
  MacroParamStartPos: LongInt;
  MacroParamEndPos: LongInt;
  MacroEndPos: LongInt;
  MacroName: String;
  MacroParameter: String;
  MacroValue: string;
  Lvl: Integer;
  SaveSrcPos: LongInt;
begin
  // replace as many macros as possible
  inc(FLevel);
  p:=1;
  SrcCopiedPos := 1;
  len:=length(Template);
  while p <= len do begin
    case Template[p] of
      '$':
        begin
          inc(p);
          // could be a macro start
          MacroStartPos := p - 1;
          MacroEndPos   := -1;

          if (p <= len) and (Template[p]='$') then begin
            System.Delete(Template, p, 1);
            inc(FSrcPosition);
          end
          else begin
            // find the macro end
            while (p <= len) and (Template[p] in ['a'..'z','A'..'Z','0'..'9','_']) do
              inc(p);
            if FEnableMacros and (p <= len) and (p-MacroStartPos > 1) and
               (Template[p] = '(') then
            begin
              inc(p);
              MacroParamStartPos:=p;
              Lvl:=1;
              while (p<=len) and (Lvl>0) do begin
                case Template[p] of
                '(': inc(Lvl);
                ')': dec(Lvl);
                end;
                inc(p);
              end;
              if Lvl=0 then begin
                // macro parameter end found
                MacroParamEndPos:=p-1;
                MacroEndPos:=p;
              end;
            end;
          end;

          if MacroEndPos < 0 then begin
            // not a macro
            p := MacroStartPos + 1;
            inc(FSrcPosition);
          end
          else begin
            // Got a Macro
            if FLevel = 1 then begin
              AppentToDest(copy(Template, SrcCopiedPos, MacroStartPos - SrcCopiedPos));
            end;
            FSrcPosition := FSrcPosition + MacroEndPos - MacroStartPos;
            // read macro name
            MacroName:=copy(Template,MacroStartPos+1,
                                          MacroParamStartPos-MacroStartPos-2);
            MacroParameter:=copy(Template,MacroParamStartPos,
                                         MacroParamEndPos-MacroParamStartPos);

            SaveSrcPos := FSrcPosition;
            if not SubstituteMacro(MacroName,MacroParameter,MacroValue) then
              exit(false);
            FSrcPosition := SaveSrcPos;
            //debugln('SubstituteMacros MacroName="',MacroName,'" MacroParameter="',MacroParameter,'" MacroValue="',MacroValue,'"');

            Template := copy(Template, 1, MacroStartPos-1)
                      + MacroValue
                      + copy(Template, MacroEndPos, len);
            len:=length(Template);
            p:=MacroStartPos+length(MacroValue);
            SrcCopiedPos := p;
            // scan the result for new lines
            if FLevel = 1 then
              AppentToDest(MacroValue);
          end;
          // else it is a normal $ character
        end;
      else
        begin
          inc(p);
          inc(FSrcPosition);
        end;
    end;
  end;

  if FLevel = 1 then begin
    AppentToDest(copy(Template, SrcCopiedPos, p - SrcCopiedPos));
  end;
  dec(FLevel);
  Result:=true;
end;

function TLazTemplateParser.SubstituteCodeMacros(SrcEdit: TSourceEditorInterface): boolean;
begin
  FDestTemplate := '';
  FDestPosition := 1;
  FDestPosX := 1;
  FDestPosY := 1;
  FLevel := 0;
  FSrcEdit := SrcEdit;
  Result := SubstituteMacros(FSrcTemplate);
end;

procedure TLazTemplateParser.TrimEOTChar(eot: Char);
begin
  if (FDestTemplate <> '') and (FDestTemplate[length(FDestTemplate)] = eot) then
    System.Delete(FDestTemplate, length(FDestTemplate), 1);
end;


function ExecuteCodeTemplate(SrcEdit: TSourceEditorInterface;
  const TemplateName, TemplateValue, TemplateComment,
  EndOfTokenChr: string; Attributes: TStrings;
  IndentToTokenStart: boolean): boolean;
var
  AEditor: TCustomSynEdit;
  p: TPoint;
  TokenStartX: LongInt;
  s: string;
  IndentLen: Integer;
  i: Integer;
  j: LongInt;
  Pattern: String;
  LineText: String;
  Parser: TLazTemplateParser;
begin
  Result:=false;
  //debugln('ExecuteCodeTemplate ',dbgsName(SrcEdit),' ',dbgsName(SrcEdit.EditorControl));
  AEditor:=SrcEdit.EditorControl as TCustomSynEdit;
  Pattern:=TemplateValue;

  Parser := TLazTemplateParser.Create(Pattern);
  AEditor.BeginUpdate;
  try
    p := AEditor.LogicalCaretXY;
    TokenStartX:=p.x;
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
      IndentLen:=AEditor.LogicalToPhysicalCol(s, p.y - 1, IndentLen);// consider tabs
      dec(IndentLen);
    end;

    Parser.EnableMacros := Attributes.IndexOfName(CodeTemplateEnableMacros)>=0;
    Parser.Indent := StringOfChar(' ', IndentLen);
    LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
    if not Parser.SubstituteCodeMacros(SrcEdit) then exit;

    s:=AEditor.Lines[p.y-1];
    if TokenStartX>length(s) then
      TokenStartX:=length(s)+1;
    j:=length(TemplateName);
    while (j>0)
    and (AnsiCompareText(copy(TemplateName,1,j),copy(s,TokenStartX-j,j))<>0) do
      dec(j);
    dec(TokenStartX,j);
    AEditor.BlockBegin := Point(TokenStartX, p.y);
    AEditor.BlockEnd := p;

    // New Caret
    p := Parser.DestCaret ;
    if p.y < 0 then
      p := AEditor.CaretXY
    else begin
      if p.y = 1 then
        p.x := p.x + TokenStartX - 1;
      p.y := p.y + AEditor.BlockBegin.y - 1; // Todo: logicalToPhysical
    end;

    // delete double end separator (e.g. avoid creating two semicolons 'begin end;;')
    if (AEditor.BlockEnd.Y>0) and (AEditor.BlockEnd.Y<=AEditor.Lines.Count)
    then begin
      LineText:=AEditor.Lines[AEditor.BlockEnd.Y-1];
      if AEditor.BlockEnd.X <= length(LineText) then
        i := pos(LineText[AEditor.BlockEnd.X], EndOfTokenChr)
      else
        i := -1;
      if i > 0 then
        Parser.TrimEOTChar(EndOfTokenChr[i]);
    end;

    i := AEditor.PluginCount - 1;
    while i >= 0 do begin
      if AEditor.Plugin[i] is TSynPluginTemplateEdit then begin
        TSynPluginTemplateEdit(AEditor.Plugin[i]).CellParserEnabled := False;
        TSynPluginTemplateEdit(AEditor.Plugin[i]).SetTemplate(Parser.DestTemplate, p);
        TSynPluginTemplateEdit(AEditor.Plugin[i]).AddEditCells(Parser.EditCellList);
        break;
      end;
      dec(i);
    end;
    if i < 0 then begin
      // replace the selected text and position the caret
      AEditor.SelText := Parser.DestTemplate;
      AEditor.MoveCaretIgnoreEOL(p);
    end;
  finally
    AEditor.EndUpdate;
    Parser.Free;
  end;
  Result:=true;
end;

{ TLazSynPluginSyncronizedEditList }

function TLazSynPluginSyncronizedEditList.AddNew: TSynPluginSyncronizedEditCell;
begin
  Result := TLazSynPluginSyncronizedEditCell.Create;
  Add(Result);
end;

end.

