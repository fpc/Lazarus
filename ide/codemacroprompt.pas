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
  BasicCodeTools, CodeToolManager,
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
    FIndent: integer;
    FKeepSubIndent: Boolean;
    FSrcTemplate: String;
    FDestTemplate: String;
    FSrcPosition: Integer;
    FDestPosition: Integer;
    FDestPosX: Integer;
    FDestPosY: Integer;
    FLevel: Integer;
    FSrcEdit: TSourceEditorInterface;
    FSubIndent: integer;
    FUseTabWidth: integer;
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
    property KeepSubIndent: Boolean read FKeepSubIndent write FKeepSubIndent;
    property Indent: integer read FIndent write FIndent;
    property SubIndent: integer read FSubIndent write FSubIndent;
    property DestCaret: TPoint read FCaret;
    property UseTabWidth: integer read FUseTabWidth write FUseTabWidth; // if >0 use tabs for indenting with this size

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
const
  TemplateTabWidth = 8;
var
  IndentLevel: Integer;
  CurLineIndent, LastLineIndent: Integer;
  IsLineStart: boolean;

  procedure AddIndent;
  var
    i: Integer;
    s: String;
  begin
    // compare the indentation of the current and the last line of the template
    if CurLineIndent>LastLineIndent then
      inc(IndentLevel)
    else if (IndentLevel>0) and (CurLineIndent<LastLineIndent) then
      dec(IndentLevel);
    //debugln(['AddIndent LastLineIndent=',LastLineIndent,' CurLineIndent=',CurLineIndent]);
    LastLineIndent:=CurLineIndent;
    // append space
    i:=Indent+IndentLevel*SubIndent;
    //debugln(['AddIndent Indent=',Indent,' IndentLevel=',IndentLevel,' SubIndent=',SubIndent,' UseTabWidth=',UseTabWidth]);
    s:=GetIndentStr(i,UseTabWidth);
    FDestTemplate += s;
    FDestPosX:=length(s)+1;
  end;

  procedure AppendToDest(S: String);
  // only called when FLevel=1
  var
    p, LastCopy: Integer;
  begin
    if IsLineStart then begin
      // remove old indent
      p:=length(FDestTemplate);
      while (p>=1) and (FDestTemplate[p] in [' ',#9]) do dec(p);
      FDestTemplate:=LeftStr(FDestTemplate,p);
    end;
    p := 1;
    LastCopy := 1;
    //debugln(['AppendToDest START S="',dbgstr(S),'" Indent=',Indent,' IsLineStart=',IsLineStart,' KeepSubIndent=',KeepSubIndent]);
    while p <= length(S) do begin
      if IsLineStart and (not KeepSubIndent) and (FDestTemplate<>'')
      then begin
        // at start of template line (not first line)
        LastCopy:=p;
        case s[p] of
        ' ':
          begin
            inc(CurLineIndent);
            inc(p);
            continue;
          end;
        #9:
          begin
            inc(p);
            inc(CurLineIndent,TemplateTabWidth);
            CurLineIndent:=CurLineIndent-(CurLineIndent mod TemplateTabWidth);
            continue;
          end;
        else
          // first character of line
          IsLineStart:=false;
          LastCopy:=p;
          AddIndent;
          // keep p and handle the character in the next step
        end;
      end;

      case s[p] of
      #10, #13:
        begin
          // copy line break
          inc(p);
          if (p <= length(S)) and (s[p] in [#10,#13]) and (s[p] <> s[p-1]) then
            inc(p);
          //debugln(['AppendToDest linebreak flush "',dbgstr(copy(s, LastCopy, p - LastCopy)),'"']);
          FDestTemplate += copy(s, LastCopy, p - LastCopy);
          LastCopy := p;
          FDestPosX := 1 + Indent;
          IsLineStart:=true;
          CurLineIndent:=0;
          inc(FDestPosY);
        end;
      else // case else
        if (s[p] = '|') and (FCaret.y < 0) then
        begin
          // place cursor
          System.Delete(s, p, 1);
          FCaret.y := FDestPosY;
          FCaret.x := FDestPosX;
          //debugln(['AppendToDest Caret=',dbgs(FCaret)]);
        end
        else begin
          inc(p);
          inc(FDestPosX);
          IsLineStart:=false;
        end;
      end;
    end;
    //debugln(['AppendToDest LAST flush "',dbgstr(copy(s, LastCopy, p - LastCopy)),'"']);
    if IsLineStart then
      AddIndent
    else
      FDestTemplate += copy(s, LastCopy, p - LastCopy);
    FDestPosition := length(FDestTemplate);
    //debugln(['AppendToDest END FDestTemplate="',dbgstr(FDestTemplate),'" FDestPosition=',FDestPosition,' FDestPosX=',FDestPosX,' FDestPosY=',FDestPosY]);
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
  IndentLevel:=0;
  CurLineIndent:=0;
  LastLineIndent:=0;
  IsLineStart:=false;
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
              AppendToDest(copy(Template, SrcCopiedPos, MacroStartPos - SrcCopiedPos));
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
              AppendToDest(MacroValue);
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
    AppendToDest(copy(Template, SrcCopiedPos, p - SrcCopiedPos));
    if IsLineStart and (not KeepSubIndent) and (FDestTemplate<>'') then
      AddIndent;
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
  AEditor: TSynEdit;
  p: TPoint;
  TokenStartX: LongInt;
  s: string;
  BaseIndent, LogBaseIndent: Integer;
  i: Integer;
  j: LongInt;
  Pattern: String;
  LineText: String;
  Parser: TLazTemplateParser;
  CodeToolBossOriginalIndent : Integer; // So I don't break anything else (hopefully)
begin
  Result:=false;
  //debugln('ExecuteCodeTemplate ',dbgsName(SrcEdit),' ',dbgsName(SrcEdit.EditorControl));
  AEditor:=SrcEdit.EditorControl as TSynEdit;
  Pattern:=TemplateValue;
  Parser := TLazTemplateParser.Create(Pattern);
  AEditor.BeginUpdate;
  try
    Parser.SubIndent:=AEditor.BlockIndent+AEditor.BlockTabIndent*AEditor.TabWidth;
    if (AEditor.BlockTabIndent=0) or (eoTabsToSpaces in AEditor.Options) then
      Parser.UseTabWidth:=0
    else
      Parser.UseTabWidth:=AEditor.BlockTabIndent*AEditor.TabWidth;

    p := AEditor.LogicalCaretXY;
    TokenStartX:=p.x;
    if IndentToTokenStart then begin
      BaseIndent := TokenStartX - 1;
    end else begin
      // indent the same as the first line
      BaseIndent:=1;
      if (p.y>0) and (p.y<=AEditor.Lines.Count) then begin
        s:=AEditor.Lines[p.y-1];
        while (BaseIndent<p.x)
        and ((BaseIndent>length(s)) or (s[BaseIndent] in [#9,' '])) do
          inc(BaseIndent);
      end;
      LogBaseIndent := BaseIndent - 1;
      BaseIndent:=AEditor.LogicalToPhysicalCol(s, p.y - 1, BaseIndent);// consider tabs
      dec(BaseIndent);
    end;

    Parser.EnableMacros := Attributes.IndexOfName(CodeTemplateEnableMacros)>=0;
    Parser.KeepSubIndent := Attributes.IndexOfName(CodeTemplateKeepSubIndent)>=0;
    Parser.Indent := LogBaseIndent;
    CodeToolBossOriginalIndent := CodeToolBoss.IndentSize;
    if Parser.KeepSubIndent then
      CodeToolBoss.IndentSize := BaseIndent // Use additional indentation
    else
      CodeToolBoss.IndentSize := 0; // Use current indentation
    try
      LazarusIDE.SaveSourceEditorChangesToCodeCache(nil);
      if not Parser.SubstituteCodeMacros(SrcEdit) then exit;
    finally
      CodeToolBoss.IndentSize := CodeToolBossOriginalIndent;
    end;

    s:=AEditor.Lines[p.y-1];
    if TokenStartX>length(s) then
      TokenStartX:=length(s)+1;
    j:=length(TemplateName);
    while (j>0)
    and (UTF8CompareText(copy(TemplateName,1,j),copy(s,TokenStartX-j,j))<>0) do
      dec(j);
    dec(TokenStartX,j);
    AEditor.BlockBegin := Point(TokenStartX, p.y);
    AEditor.BlockEnd := p;

    // New Caret
    p := Parser.DestCaret; // Logical
    if p.y >= 0 then begin
      if p.y = 1 then
        p.x := p.x + TokenStartX - 1;
      p.y := p.y + AEditor.BlockBegin.y - 1;
      // p must be logical, until template text is inserted
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

    if Parser.EditCellList.Count > 0 then
      i := AEditor.PluginCount - 1
    else
      i := -1;
    while i >= 0 do begin
      if AEditor.Plugin[i] is TSynPluginTemplateEdit then begin
        if p.y < 1 then
          p := AEditor.CaretXY;
        TSynPluginTemplateEdit(AEditor.Plugin[i]).CellParserEnabled := False;
        TSynPluginTemplateEdit(AEditor.Plugin[i]).SetTemplate(Parser.DestTemplate, p);
        TSynPluginTemplateEdit(AEditor.Plugin[i]).AddEditCells(Parser.EditCellList);
        break;
      end;
      dec(i);
    end;
    if i < 0 then begin
      // replace the selected text and position the caret
      AEditor.SetTextBetweenPoints(AEditor.BlockBegin, AEditor.BlockEnd, Parser.DestTemplate, [], scamEnd);
      if p.y > 0 then
        AEditor.MoveCaretIgnoreEOL(AEditor.LogicalToPhysicalPos(p));
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

