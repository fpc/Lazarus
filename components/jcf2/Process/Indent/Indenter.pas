unit Indenter;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code

The Original Code is Indenter, released May 2003.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
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

{ AFS 23 Feb 2003
  process to indent tokens
  Will borrow some old code, but mostly new

  This is the single most important (and possibly most complex)
  of all the processes
  it sets the indent for the start of the line
}

uses SwitchableVisitor;

type
  TIndenter = class(TSwitchableVisitor)
  protected
    function EnabledVisitSourceToken(const pcNode: TObject): Boolean; override;
  public
    constructor Create; override;
  end;

implementation

uses
  { delphi }
  SysUtils,
  { local }
  JcfStringUtils,
  SourceToken, Nesting, FormatFlags, JcfSettings, TokenUtils,
  Tokens, ParseTreeNode, ParseTreeNodeType;

{ true if the specified token type occurs before pt on the line }
function HasPreceedingTokenTypeOnLine(const pt: TSourceToken; const ptt: TTokenTypeSet): Boolean;
var
  lcTest: TSourceToken;
begin
  Result := False;
  lcTest := pt;

  repeat
    lcTest := lcTest.PriorToken;
    if lcTest <> nil then
      Result := (lcTest.TokenType in ptt);

  until Result or (lcTest = nil) or (lcTest.TokenType = ttReturn);
end;

function HasPreceedingSolidToken(const pt: TSourceToken): Boolean;
var
  lcTest: TSourceToken;
begin
  Result := False;
  lcTest := pt;

  repeat
    lcTest := lcTest.PriorToken;
    if lcTest <> nil then
      Result := lcTest.IsSolid;

  until Result or (lcTest = nil) or (lcTest.TokenType = ttReturn);
end;

function HasSolidTokenBeforeUnderSameParent(const pt: TSourceToken): Boolean;
var
  lcParent: TParseTreeNode;
  liLoop: integer;
begin
  Result := False;
  lcParent := pt.Parent;

  for liLoop := 0 to lcParent.IndexOfChild(pt) do
  begin
    if lcParent.ChildNodes[liLoop] is TSourceToken then
    begin
      if TSourceToken(lcParent.ChildNodes[liLoop]).IsSolid then
      begin
        Result := True;
        break;
      end;
    end;
  end;
end;

function InGlobalTypeOrVarOrConstSection(const pt: TSourceToken): Boolean;
begin
  // are we in a type or var section?
  if not pt.HasParentNode([nTypeSection, nVarSection, nConstSection]) then
  begin
    Result := False;
    exit;
  end;

  if pt.HasParentNode(MethodDeclarations) then
  begin
    Result := False;
    exit;
  end;

  Result := True;
end;

function IsIndented(const pt: TSourceToken): Boolean;
begin
  Result := IsFirstSolidTokenOnLine(pt);
  if Result then
  begin
    { don't indent } // when there's a comment before the token on a line
    if HasPreceedingTokenTypeOnLine(pt, [ttComment]) then
    begin
      Result := False;
    end;

  end
  else if IsSingleLineComment(pt) and (pt.CommentStyle <> eCompilerDirective) then
  begin
    { if it's a single line comment, with nothing on the line before it
      then may want to indent it }
    if not HasPreceedingSolidToken(pt) then
    begin
      if pt.HasParentNode(nBlock) then
      begin
        Result := FormattingSettings.Indent.KeepCommentsWithCodeInProcs;
      end
      else if pt.HasParentNode([nClassType, nInterfaceType]) then
      begin
        Result := FormattingSettings.Indent.KeepCommentsWithCodeInClassDef;
      end
      else if pt.HasParentNode([nDeclSection]) then
        Result := FormattingSettings.Indent.KeepCommentsWithCodeInGlobals
      else
        Result := FormattingSettings.Indent.KeepCommentsWithCodeElsewhere;
    end;
  end;
end;

{ is this token in an expression that starts on a previous line }
function IsRunOnExpr(const pt: TSourceToken): boolean;
var
  lcExpr:      TParseTreeNode;
  lcExprStart: TSourceToken;
begin
  Result := False;

  if pt = nil then
    exit;

  lcExpr := pt.GetParentNode(nExpression);
  if lcExpr <> nil then
  begin
    lcExprStart := lcExpr.FirstLeaf as TSourceToken;
    while (lcExprStart <> nil) and ( not lcExprStart.IsSolid) do
      lcExprStart := lcExprStart.NextToken;

    if lcExprStart.YPosition < pt.YPosition then
      Result := True;
  end;
end;

function IsInAssignExpr(const pt: TSourceToken): boolean;
begin
  Result := False;

  if pt = nil then
    exit;

  if not pt.HasParentNode(nAssignment) then
    exit;

  if not pt.IsOnRightOf([nAssignment], AssignmentDirectives) then
    exit;

  Result := True;
end;

function IsInProcedureParams(const pt: TSourceToken): boolean;
begin
  Result := False;

  if pt = nil then
    exit;

  if not pt.HasParentNode(nActualParams) then
    exit;

  if pt.Nestings.GetLevel(nlRoundBracket) = 0 then
    exit;

  // in a statement, in round brackets, also need .. ??

  Result := True;
end;


{ this is needed for nested types
  indent the inner class more than the outer }
function CountClassNesting(const pt: TParseTreeNode): integer;
begin
  Result := 0;

  if pt = nil then
    exit;

  if pt.NodeType in ObjectTypes then
    Result := 1;

  Result := Result + CountClassNesting(pt.Parent);
end;

function CountTypeNesting(const pt: TParseTreeNode): integer;
begin
  Result := 0;

  if pt = nil then
    exit;

  if pt.NodeType = nTypeSection then
    Result := 1;

  Result := Result + CountTypeNesting(pt.Parent);
end;

function IsRunOnProcDecl(const pt: TSourceToken): boolean;
begin
  Result := pt.HasParentNode(ProcedureHeadings) and
    (not (pt.TokenType in (ProcedureWords + [ttClass, ttComment])));
end;


{ true if this is the top of an if statement
  would be easier if there was a nIfStatement node type }
function IsIfBlockTop(const pn: TParseTreeNode): boolean;
var
  lcToken: TSourceToken;
begin
  Result := False;

  if pn.IsLeaf then
  begin
    lcToken := TSourceToken(pn);
    Result := (lcToken.TokenType in [ttIf, ttElse]);
  end;

  if not Result then
    Result := (pn.NodeType in [nIfBlock, nIfCondition]);

  if not Result then
    Result := (pn.NodeType = nStatement) and (pn.HasChildNode(ttIf, 1));
end;

function ElseDepth(const pt: TSourceToken): integer;
var
  lcParent: TParseTreeNode;
begin
  Result := 0;

  if InStatements(pt) then
  begin
    lcParent := pt;
    while (lcParent <> nil) do
    begin
      // increment for an if statement under an else
      if IsIfblockTop(lcParent) and lcParent.HasParentNode(nElseBlock, 2) then
      begin
        inc(Result);
        // skip the next two level, part of the same if-else block.
        if lcParent.NodeType <> nStatement then
          lcParent := lcParent.Parent;
        if lcParent <> nil then
          lcParent := lcParent.Parent;
      end;

      if lcParent <> nil then
        lcParent := lcParent.Parent;
    end;
  end;
end;

function CalculateIndent(const pt: TSourceToken): integer;
var
  liIndentCount: integer;
  lbHasIndentedRunOnLine: boolean;
  lbHasIndentedDecl: boolean;
  lcParent, lcChild: TParseTreeNode;
  liClassNestingCount: integer;
  liTypeNestingCount: integer;
  liVarConstIndent: integer;
begin
  Result := 0;
  lbHasIndentedRunOnLine := False;
  lbHasIndentedDecl := False;

  if pt = nil then
    exit;

  { object types }
  if pt.HasParentNode(ObjectTypes) then
  begin
    { indentation sections inside the class }
    if FormattingSettings.Indent.IndentVarAndConstInClass then
    begin
      liVarConstIndent := 2;
    end
    else
    begin
      liVarConstIndent := 1;
    end;

    if pt.TokenType in ClassVisibility + [ttStrict] then
      liIndentCount := 1
    else if (pt.TokenType = ttConst) and pt.HasParentNode(nConstSection, 1) then
      liIndentCount := liVarConstIndent
    else if (pt.TokenType = ttVar) and pt.HasParentNode(nVarSection, 1) then
      liIndentCount := liVarConstIndent
    else if (pt.TokenType = ttClass) and pt.HasParentNode(nClassVars, 1) then
      liIndentCount := 2

    else if pt.TokenType = ttEnd then
    begin
      // end is the end of the class unless it's the end of an anon record typed var
      if pt.HasParentNode(nRecordType) then
        liIndentCount := 2
      else
        liIndentCount := 1;
    end
    else
      liIndentCount := 2;

    // run on lines in properties
    if pt.HasParentNode(nProperty) and (not (pt.TokenType  in [ttProperty, ttComment, ttClass])) then
      Inc(liIndentCount);

    // run on lines in procs
    if IsRunOnProcDecl(pt) then
      Inc(liIndentCount);

    lbHasIndentedDecl := True;

    liClassNestingCount := CountClassNesting(pt);
    liIndentCount := liIndentCount + (liClassNestingCount - 1);

    if FormattingSettings.Indent.IndentNestedTypes then
    begin
      liTypeNestingCount := CountTypeNesting(pt);
      if (liTypeNestingCount > 1) then
      begin
        if pt.TokenType = ttType then
        begin
          liIndentCount := liIndentCount + (liTypeNestingCount - 2);
        end
        else
        begin
          liIndentCount := liIndentCount + (liTypeNestingCount - 1);
        end;
      end;
    end;
  end

  { indent vars, consts etc, e.g.
    implementation
    co nst
      foo = 3;
    v ar
      bar: integer;
  }
  else if pt.HasParentNode(nDeclSection) and (not pt.HasParentNode(ProcedureNodes)) then
  begin
    if pt.TokenType in Declarations + ProcedureWords then
    begin
      {
        the words 'var' and 'const' can be found in proc params
        the words 'procedure' and 'function' can be found in type defs, e.g. type Tfoo = procedure of object; }
      if (pt.TokenType in ProcedureWords + ParamTypes) and
        (pt.HasParentNode(nProcedureType) or pt.HasParentNode(nFormalParams)) then
        liIndentCount := 1
      else
        liIndentCount := 0;
    end
    else
      liIndentCount := 1;

    if pt.Nestings.GetLevel(nlProcedure) > 1 then
      liIndentCount := liIndentCount + (pt.Nestings.GetLevel(nlProcedure) - 1);
  end
  else
  begin
    { this section is for
      - procedure body
      - procedure declarations
    }

    { indent procedure body for various kinds of block }
    liIndentCount := pt.Nestings.GetLevel(nlBlock);
    if liIndentCount > 0 then
    begin
      // outdent keywords that start and end the block
      if pt.TokenType in BlockOutdentWords then
      begin
        Dec(liIndentCount);

        // not these in local record type decl
        if (pt.TokenType in [ttCase, ttEnd]) and (pt.HasParentNode(nRecordType)) then
          Inc(liIndentCount);

        // not these in  procedure params or procedure type
        if (pt.TokenType in ParamTypes) and pt.HasParentNode(nProcedureType) then
          Inc(liIndentCount);
      end
    end;

   { procedure formal params are not in the block }
    if pt.HasParentNode(nFormalParams) then
      Inc(liIndentCount);

    if (liIndentCount = 0) and pt.HasParentNode(nProcedureDirectives) then
      liIndentCount := 1;

    // else as an outdent for an exception block
    if (pt.TokenType = ttElse) and pt.HasParentNode(nOnExceptionHandler, 1) then
      Dec(liIndentCount);

    // while loop (and others?) expression is not yet in the block
    if pt.HasParentNode([nLoopHeaderExpr, nBlockHeaderExpr]) then
    begin
      //dec(liIndentCount);
      lbHasIndentedRunOnLine := True;
    end;

    if pt.Nestings.GetLevel(nlCaseSelector) > 0 then
    begin
      liIndentCount := liIndentCount + pt.Nestings.GetLevel(nlCaseSelector);
      // don't indent the case label again
      if pt.HasParentNode(nCaseLabel, 6) then
        Dec(liIndentCount)
      else if (pt.TokenType in [ttElse, ttOtherwise]) and pt.HasParentNode(nElseCase, 1) then
        Dec(liIndentCount);

      if not FormattingSettings.Indent.IndentCaseElse then
      begin
        liIndentCount :=  liIndentCount - pt.CountParentNodes(nElseCase);
      end;
    end;


    { nested procedures
    if pt.Nestings.GetLevel(nlProcedure) > 1 then
      liIndentCount := liIndentCount + (pt.Nestings.GetLevel(nlProcedure) - 1);
    }

    if pt.HasParentNode(nAsm) and pt.HasParentNode(nStatementList) then
      Inc(liIndentCount);

    { indent for run on line }
    if pt.HasParentNode(nStatement) and (pt.Nestings.GetLevel(nlRoundBracket) +
      (pt.Nestings.GetLevel(nlSquareBracket)) > 0) then
    begin
      Inc(liIndentCount);
      lbHasIndentedRunOnLine := True;
    end;

    // indent for uses clause
    if pt.HasParentNode(UsesClauses) and ( not (pt.TokenType in UsesWords)) then
    begin
      Inc(liIndentCount);

      { run on uses item }
      if pt.HasParentNode(nUsesItem, 1) and (pt.IsOnRightOf(nUsesItem, ttIn)) then
        Inc(liIndentCount);

    end;

    if (pt.TokenType = ttOn) and pt.HasParentNode(nOnExceptionHandler, 1) then
      Dec(liIndentCount);

    { run on lines such as
      SomeArray[
       index] := 3; }
    if ( not lbHasIndentedRunOnLine) and pt.HasParentNode(nDesignator) and
      (pt.TokenType <> ttComment) then
    begin
      lcParent := pt.Parent;
      if lcParent.NodeType = nIdentifier then
        lcParent := lcParent.Parent;

      lcChild := lcParent.FirstSolidLeaf;
      if (pt <> lcChild) then
      begin
        Inc(liIndentCount);
        lbHasIndentedRunOnLine := True;
      end;
    end;

    if FormattingSettings.Indent.IndentElse then
      liIndentCount := liIndentCount + ElseDepth(pt);
    

  end; // procedures

  { record declaration stuph }
  if pt.HasParentNode(nRecordType) then
  begin
    { nested record types }
    if pt.Nestings.GetLevel(nlRecordType) > 1 then
      liIndentCount := liIndentCount + (pt.Nestings.GetLevel(nlRecordType) - 1);

    if pt.Nestings.GetLevel(nlRecordVariantSection) > 0 then
    begin
      liIndentCount := liIndentCount + pt.Nestings.GetLevel(nlRecordVariantSection);
      if pt.TokenType = ttCase then
        Dec(liIndentCount);
    end;

    if pt.HasParentNode(nRecordVariant) and (RoundBracketLevel(pt) > 0) then
      Inc(liIndentCount);

    // delphi.net fns and procedures in records
    if pt.TokenType in (ProcedureWords) then
    begin
      Inc(liIndentCount);
    end
    // class procs, but not class vars - those are already done
    else if (pt.TokenType = ttClass) then
    begin
      if not pt.HasParentNode(nClassVars, 1) then
      begin
        Inc(liIndentCount);
      end;
    end;

    if (pt.TokenType <> ttEnd) then
    begin
      lbHasIndentedDecl := True;
      if not (pt.TokenType in ClassVisibility) then
        Inc(liIndentCount);
    end;

    // run on lines in procs
    if pt.HasParentNode(ProcedureHeadings) and
      ( not (pt.TokenType in (ProcedureWords + [ttClass, ttComment]))) then
      Inc(liIndentCount);

    if IsRunOnProcDecl(pt) then
      Inc(liIndentCount);
    
  end;


  { these apply everywhere
    mostly because they need to apply to decls
    either in or out of a proc }
  if pt.HasParentNode(nEnumeratedType) and (RoundBracketLevel(pt) > 0) then
  begin
    Inc(liIndentCount);
    lbHasIndentedRunOnLine := True;
  end;

  // indent for run-on const decl
  if ( not lbHasIndentedRunOnLine) and
    pt.HasParentNode(nConstDecl) and pt.IsOnRightOf(nConstDecl, ttEquals) then
  begin
    Inc(liIndentCount);
    lbHasIndentedRunOnLine := True;
  end;

  // indent for the type decl in a const
  if ( not lbHasIndentedRunOnLine) and
    pt.HasParentNode(nConstDecl) and pt.IsOnRightOf(nConstDecl, ttColon) and
    pt.HasParentNode(nType) then
  begin
    Inc(liIndentCount);
    lbHasIndentedRunOnLine := True;
  end;

  { run on expression }
  if not lbHasIndentedRunOnLine then
  begin
    if IsRunOnExpr(pt) then
      Inc(liIndentCount)
    else if IsInAssignExpr(pt) then
      Inc(liIndentCount)
    else if IsInProcedureParams(pt) then
      Inc(liIndentCount)
    { run-on type decl }
    else if pt.IsOnRightOf(nTypeDecl, ttEquals) and ( not lbHasIndentedDecl) and
      (pt.TokenType <> ttEnd) then
      Inc(liIndentCount);
  end;

  if ( not lbHasIndentedRunOnLine) and pt.HasParentNode(nArrayConstant) and
    ((RoundBracketLevel(pt) > 0) or (pt.TokenType in
    [ttOpenBracket, ttCloseBracket])) then
    Inc(liIndentCount);

  // indent statement after label
  if ( not lbHasIndentedRunOnLine) and
    (pt.Nestings.GetLevel(nlStatementLabel) > 0) and
    ( not pt.HasParentNode(nStatementLabel)) then
    Inc(liIndentCount);

  // program or library top level procs
  // re bug 1898723 - Identination of procedures in library
  if not FormattingSettings.Indent.IndentLibraryProcs then
  begin
    if pt.HasParentNode([nLibrary, nProgram]) and (liIndentCount >= 1) then
    begin
      if not pt.HasParentNode([nExports, nUses]) and (not InGlobalTypeOrVarOrConstSection(pt)) then
      begin
        if pt.HasParentNode([nCompoundStatement]) and not pt.HasParentNode([nDeclSection]) then
        begin
          // this is the program/lib main code block
          // it's flush left already
        end
        else
        begin
          Dec(liIndentCount);
        end;
      end;

    end;
  end;

  // indent all of procs except for the first line
  if FormattingSettings.Indent.IndentProcedureBody then
  begin
    if pt.HasParentNode(MethodDeclarations) then
    begin
      if not (pt.HasParentNode(MethodHeadings)) then
      begin
        inc(liIndentCount);
      end;
    end;
  end;

  Assert(liIndentCount >= 0, 'Indent count = ' + IntToStr(liIndentCount) +
    ' for ' + pt.Describe);

  Result := FormattingSettings.Indent.SpacesForIndentLevel(liIndentCount);

  // asm extra indent
  if IsInsideAsm(pt) and FormattingSettings.SetAsm.StatementIndentEnabled then
  begin
    Result := Result + FormattingSettings.SetAsm.StatementIndent;
  end;

  // IndentBeginEnd option to indent begin/end words a bit extra
  if FormattingSettings.Indent.IndentBeginEnd then
  begin
    if (pt.TokenType in [ttTry, ttExcept, ttFinally, ttBegin, ttEnd]) and InStatements(pt) then
    begin
      // filter out the begin/end that starts and ends a procedure
      if not pt.HasParentNode(nBlock, 2) then
      begin
        Result := Result + FormattingSettings.Indent.IndentBeginEndSpaces;
      end;
    end;
  end;

end;

constructor TIndenter.Create;
begin
  inherited;
  FormatFlags := FormatFlags + [eIndent];
end;

function TIndenter.EnabledVisitSourceToken(const pcNode: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
  lcPrev: TSourceToken;
  liPos:  integer;
  liDesiredIndent: integer;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcNode);

  if IsIndented(lcSourceToken) then
  begin
    liDesiredIndent := CalculateIndent(lcSourceToken);
    liPos := lcSourceToken.XPosition - 1;
    Assert(liPos >= 0);

    if liDesiredIndent < liPos then
    begin
      { delete some spaces before, if they exist }
      lcPrev := lcSourceToken.PriorToken;
      if (lcPrev <> nil) and (lcPrev.TokenType = ttWhiteSpace) then
      begin
        lcPrev.SourceCode :=
          StrRepeat(NativeSpace, liDesiredIndent - lcPrev.XPosition + 1);
      end;
      {
      else if liDesiredIndent > 0 then
      begin
        // no prev space token? Insert one
        prVisitResult.Action := aInsertBefore;
        prVisitResult.NewItem := NewSpace(liDesiredIndent);
      end;
      }
    end
    else if liDesiredIndent > liPos then
    begin
      Result := True;
      InsertTokenBefore(lcSourceToken, NewSpace(liDesiredIndent - liPos));
    end;
  end;

end;

end.
