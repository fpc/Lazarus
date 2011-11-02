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
    TCodeTemplatesTool enhances TStandardCodeTool with the ability to insert
    code templates.
}
unit CodeTemplatesTool;

{$mode objfpc}{$H+}

interface

{$I codetools.inc}

uses
  {$IFDEF MEM_CHECK}
  MemCheck,
  {$ENDIF}
  Classes, SysUtils, FileProcs, CodeTree, CodeAtom, CodeCache, KeywordFuncLists,
  LinkScanner, AVL_Tree, SourceChanger,
  CustomCodeTool, PascalParserTool, CodeToolsStructs, StdCodeTools;

type
  TCodeTemplateSyntax = (
    ctsxDefault
    );
  { ctsxDefault:
  
    The backslash '\' makes special chars to normal chars.
    |  for cursor
    $(MacroName) for macro variables
    $MacroName(Param) for macro functions


    If the variable contains several lines, the lines are indented like the
    first line.
    Example:
      Inserting the text
      
        "begin"
        "  i:=1;"
        "end;"
      Into the text
      
        "begin"
        "  |"
        "end;"
        
      at the | results in
      
        "begin"
        "  begin"
        "    i:=1;"
        "  end;"
        "end;"


    Defined variables:
    
    $(Selection) - replaced by the current selection
    
    $(Indent) - replaced by nothing. Set Indent for multiline macros
                Example:
                  Text: " Text: $(Indent)$(Selection)"
                  Selection: "First"
                             "Second"
                  Result: " Text: First"
                          "       Second"
  }

  TCodeToolTemplate = class
  private
    FIndent: integer;
    FNewLineAtEnd: boolean;
    FNewLineAtStart: boolean;
    FSyntax: TCodeTemplateSyntax;
    FTemplate: string;
    procedure SetIndent(const AValue: integer);
    procedure SetNewLineAtEnd(const AValue: boolean);
    procedure SetNewLineAtStart(const AValue: boolean);
    procedure SetSyntax(const AValue: TCodeTemplateSyntax);
    procedure SetTemplate(const AValue: string);
  public
    property Template: string read FTemplate write SetTemplate;
    property NewLineAtStart: boolean read FNewLineAtStart write SetNewLineAtStart;
    property NewLineAtEnd: boolean read FNewLineAtEnd write SetNewLineAtEnd;
    property Syntax: TCodeTemplateSyntax read FSyntax write SetSyntax;
    property Indent: integer read FIndent write SetIndent;
  end;

  { TCodeTemplatesTool }

  TCodeTemplatesTool = class(TStandardCodeTool)
  public
    function InsertCodeTemplate(CursorPos,EndPos: TCodeXYPosition;
      TopLine: integer; CodeTemplate: TCodeToolTemplate;
      out NewPos: TCodeXYPosition; out NewTopLine: integer;
      SourceChangeCache: TSourceChangeCache): boolean;
    function ExtractProcedureHeader(CursorPos: TCodeXYPosition;
      Attributes: TProcHeadAttributes; var ProcHead: string): boolean;

    procedure CalcMemSize(Stats: TCTMemStats); override;
  end;

implementation

{ TCodeTemplatesTool }

function TCodeTemplatesTool.InsertCodeTemplate(CursorPos,
  EndPos: TCodeXYPosition; TopLine: integer; CodeTemplate: TCodeToolTemplate;
  out NewPos: TCodeXYPosition; out NewTopLine: integer;
  SourceChangeCache: TSourceChangeCache): boolean;
var
  NewText: TMemoryStream;
  TemplatePos: Integer;
  LastWrittenTemplatePos: Integer;
  TemplateStr: String;
  TemplateStrLen: Integer;
  
  procedure FlushTemplate;
  var
    FromPos: Integer;
    ToPos: Integer;
  begin
    FromPos:=LastWrittenTemplatePos+1;
    ToPos:=TemplatePos-1;
    if ToPos>TemplateStrLen then
      ToPos:=TemplateStrLen;
    if FromPos<=ToPos then
      NewText.Write(TemplateStr[FromPos],ToPos-FromPos+1);
    LastWrittenTemplatePos:=ToPos;
  end;
  
  procedure CalculateNewCursorPos;
  begin

  end;
  
  procedure ParseMacro;
  var
    MacroStart: Integer;
    MacroFuncNameStart: Integer;
    MacroFuncNameEnd: Integer;
    BracketLvl: Integer;
    MacroParamStart: Integer;
    MacroParamEnd: Integer;
  begin
    MacroStart:=TemplatePos;
    inc(MacroStart);
    MacroFuncNameStart:=TemplatePos;
    while (TemplatePos<=TemplateStrLen)
    and IsIdentChar[TemplateStr[TemplatePos]] do
      inc(TemplatePos);
    MacroFuncNameEnd:=TemplatePos;
    if (TemplatePos<=TemplateStrLen) and (TemplateStr[TemplatePos]='(') then
    begin
      inc(TemplatePos);
      MacroParamStart:=TemplatePos;
      MacroParamEnd:=MacroParamStart;
      BracketLvl:=1;
      while (TemplatePos<=TemplateStrLen) do begin
        case TemplateStr[TemplatePos] of
        '\':
          begin
            inc(TemplatePos);
            if (TemplatePos>TemplateStrLen) then break;
          end;
        '(': inc(BracketLvl);
        ')':
          begin
            dec(BracketLvl);
            if BracketLvl=0 then begin
              MacroParamEnd:=TemplatePos;
              break;
            end;
          end;
        end;
        inc(TemplatePos);
      end;
      if MacroFuncNameStart<MacroFuncNameEnd then begin
        // a macro function
        // ToDo
      end else if MacroParamStart<MacroParamEnd then begin
        // a macro variable
        // ToDo
      end;
    end;
    LastWrittenTemplatePos:=TemplatePos;
  end;

begin
  Result:=false;
  NewPos:=CursorPos;
  NewText:=TMemoryStream.Create;
  try
    // parse the template
    TemplatePos:=1;
    LastWrittenTemplatePos:=TemplatePos-1;
    TemplateStr:=CodeTemplate.Template;
    TemplateStrLen:=length(TemplateStr);
    while TemplatePos<=TemplateStrLen do begin
      case TemplateStr[TemplatePos] of
      '\':
        begin
          FlushTemplate;
          LastWrittenTemplatePos:=TemplatePos;
          inc(TemplatePos,2);
        end;
        
      '|':
        begin
          FlushTemplate;
          CalculateNewCursorPos;
          LastWrittenTemplatePos:=TemplatePos;
          inc(TemplatePos);
        end;

      '$':
        begin
          FlushTemplate;
          ParseMacro;
        end;

      else
        inc(TemplatePos);
      end;
    end;
    FlushTemplate;
    
    
  finally
    NewText.Free;
  end;
end;

function TCodeTemplatesTool.ExtractProcedureHeader(CursorPos: TCodeXYPosition;
  Attributes: TProcHeadAttributes; var ProcHead: string): boolean;
var
  CleanCursorPos: integer;
  ANode: TCodeTreeNode;
begin
  Result:=false;
  ProcHead:='';
  BuildTreeAndGetCleanPos(trTillCursor,lsrEnd,CursorPos,CleanCursorPos,[]);
  ANode:=FindDeepestNodeAtPos(CleanCursorPos,True);
  while (ANode<>nil) and (ANode.Desc<>ctnProcedure) do
    ANode:=ANode.Parent;
  if ANode=nil then exit;
  ProcHead:=ExtractProcHead(ANode,Attributes);
  Result:=true;
end;

procedure TCodeTemplatesTool.CalcMemSize(Stats: TCTMemStats);
begin
  inherited CalcMemSize(Stats);
end;

{ TCodeToolTemplate }

procedure TCodeToolTemplate.SetIndent(const AValue: integer);
begin
  if FIndent=AValue then exit;
  FIndent:=AValue;
end;

procedure TCodeToolTemplate.SetNewLineAtEnd(const AValue: boolean);
begin
  if FNewLineAtEnd=AValue then exit;
  FNewLineAtEnd:=AValue;
end;

procedure TCodeToolTemplate.SetNewLineAtStart(const AValue: boolean);
begin
  if FNewLineAtStart=AValue then exit;
  FNewLineAtStart:=AValue;
end;

procedure TCodeToolTemplate.SetSyntax(const AValue: TCodeTemplateSyntax);
begin
  if FSyntax=AValue then exit;
  FSyntax:=AValue;
end;

procedure TCodeToolTemplate.SetTemplate(const AValue: string);
begin
  if FTemplate=AValue then exit;
  FTemplate:=AValue;
end;

end.

