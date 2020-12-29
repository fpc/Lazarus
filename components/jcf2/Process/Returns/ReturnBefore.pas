unit ReturnBefore;

{ AFS 10 Jan 2003
  Return before
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is ReturnBefore, released May 2003.
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

uses SwitchableVisitor;


type
  TReturnBefore = class(TSwitchableVisitor)
  private
    fiReturnsBefore, fiNextReturnsBefore: integer;

    fiReturnsBeforeProcedure: integer;

  protected
    procedure InspectSourceToken(const pcToken: TObject); override;

    function EnabledVisitSourceToken(const pcToken: TObject): Boolean; override;
  public
    constructor Create; override;

    function IsIncludedInSettings: boolean; override;
  end;


implementation

uses
  TokenUtils,
  SourceToken, Tokens, ParseTreeNode,
  ParseTreeNodeType, JcfSettings,
  FormatFlags, SettingsTypes;

const
  WordsReturnBefore: TTokenTypeSet =
    [ttEnd, ttUntil, ttTry, ttFinally, ttExcept
    //, ttConditionalCompilationRemoved
    ];

  WordsBlankLineBefore: TTokenTypeSet =
    [ttImplementation, ttInitialization, ttFinalization, ttUses];

{
the first token of structured type may have a blank line before it
    before class/interface def with body when it's not the first type.

    e.g.
      type
        foo = integer;

        TSomeClass = class...

    These start with a type name
   and have a parent node nTypeDecl, which in turn owns a Restricted type -> Class type
  }
function IsStructuredTypeStart(const pt: TSourceToken): Boolean;
var
  lcPrev: TSourceToken;
  lcParent: TParseTreeNode;
begin
  Result := False;
  
  if not IsIdentifier(pt, idStrict) then
    exit;

  if not pt.HasParentNode(nTypeDecl, 2) then
    exit;

  lcPrev := pt.PriorSolidToken;

  // not if there's an attibute or generic keyword before the identifier
  if (lcPrev.TokenType = ttCloseSquareBracket) and (lcPrev.HasParentNode(nAttribute)) or (lcPrev.TokenType = ttGeneric) then
    exit;

  if (lcPrev <> nil) and (lcPrev.TokenType <> ttType) then
  begin
    // identifier
    lcParent := pt.Parent;
    if lcParent.NodeType = nIdentifier then
      lcParent := lcParent.Parent
    else
      lcParent := nil;

    if (lcParent <> nil) then
    begin
      if (lcParent.NodeType = nTypeDecl) and
        lcParent.HasChildNode(ObjectTypes, 2) and
        lcParent.HasChildNode(ObjectBodies, 3) then
        exit(True);

      { likewise before a record type }
      if (lcParent.NodeType = nTypeDecl) and
        lcParent.HasChildNode(nRecordType, 2) and
        lcParent.HasChildNode(nFieldDeclaration, 3) then
        exit(True);
    end;
  end;
end;

function StartsAnonymousMethod(const pt: TSourceToken): Boolean;
begin
  Result := (pt.TokenType in [ttProcedure, ttFunction]) and
     pt.HasParentNode(nAnonymousMethod, 3)
end;


function NeedsBlankLine(const pt: TSourceToken): boolean;
var
  lcNext: TSourceToken;
begin
  Result := (pt.TokenType in WordsBlankLineBefore);
  if Result then
    exit;

  { function/proc body needs a blank line
   but not in RHSEquals of type defs,
   or in an anonymous procedure type
   and not in class & interface def,
   and not if precedeed by the class specified for class functions
   and not if it doesn't have a proc body

   IMHO should also have blank line before contained procs
   }
  if StartsAnonymousMethod(pt) then
    exit;
  if (pt.TokenType in ProcedureWords) and
    (not pt.IsOnRightOf(nTypeDecl, ttEquals)) and
    (not pt.HasParentNode(nProcedureType, 2)) and
    (not IsClassFunctionOrProperty(pt)) and
    (not IsGenericFunctionOrProperty(pt)) and
    (ProcedureHasBody(pt)) and
    (not IsIdentifier(pt, idAny)) then
    exit(True);

  // form dfm comment
  if IsDfmIncludeDirective(pt) or IsGenericResIncludeDirective(pt) then
    exit(True);

    { blank line before the words var, type or const at top level
      except for:
      type t2 = type integer;
      or

      var foo: System.&Type; }
  if (pt.TokenType in Declarations) and (pt.Nestings.Total = 0) and
    (not pt.IsOnRightOf(nTypeDecl, ttEquals)) and
    (not pt.HasParentNode(nType)) then
    exit(True);

  { start of class function body }
  if (pt.TokenType = ttClass) and
    ( not pt.HasParentNode([nVarDecl, nConstDecl, nClassDeclarations, nRecordType])) and
    ( not IsGenericFunctionOrProperty(pt)) and
    (pt.HasParentNode(nFunctionHeading, 1)) then
    exit(True);

  { interface, but not as a typedef }
  if (pt.TokenType = ttInterface) and not (pt.HasParentNode(nTypeDecl)) then
    exit(True);

  if IsStructuredTypeStart(pt) then
    exit(True);

  { end. where there is no initialization section code,
    ie 'end' is the first and only token in the init section   }
  if (pt.TokenType = ttEnd) and
    pt.HasParentNode(nInitSection, 1) and
    (pt.Parent.SolidChildCount = 1) then
  begin
    lcNext := pt.NextSolidToken;
    if (lcNext <> nil) and (lcNext.TokenTYpe = ttDot) then
      exit(True);
  end;

  //type
  //  TStr8 = string[8];
  if (pt.TokenType = ttOpenSquareBracket) and (pt.PriorToken.TokenType=ttString ) then
    exit(False);

  // attribute before type decl
  if (pt.TokenType = ttOpenSquareBracket) and
    pt.HasParentNode(nTypeDecl, 2) and
    (not pt.HasParentNode([nClassType, nRecordType])) then
    exit(True);

end;

function NeedsReturn(const pt, ptNext: TSourceToken): boolean;
var
  lcPrev: TSourceToken;
begin
  Result := False;

  if pt = nil then
    exit;

  if pt.HasParentNode(nAsm) then
    exit;

  if StartsAnonymousMethod(pt) then
    exit;

  if pt.TokenType = ttBegin then
  begin
    lcPrev:=pt.PriorSolidToken;
    if (lcPrev <> nil) and (not (lcPrev.TokenType in [ttThen,ttDo,ttElse,ttColon])) then
      exit(True);
  end;

  if (pt.TokenType in WordsReturnBefore) then
    exit(True);

  if pt.TokenType = ttElse then
  begin
    lcPrev := pt.PriorSolidToken;
    // return before else unless it's end..else and the style forbids it
    if (FormattingSettings.Returns.EndElseStyle = eAlways) or
      ((lcPrev <> nil) and (lcPrev.TokenType <> ttEnd)) then
      Result := True;
  end;

  { return before compiler directives }
  if (pt.CommentStyle = eCompilerDirective) and (CompilerDirectiveLineBreak(pt, True) = eAlways) then
  begin
    lcPrev := pt.PriorTokenWithExclusions([ttWhiteSpace]); 
    if (lcPrev <> nil) and (lcPrev.TokenType <> ttConditionalCompilationRemoved) then
      exit(True);
  end;

  { there is not always a return before 'type'
    e.g.
    type TMyInteger = type Integer;
    is legal, only a return before the first one

   var, const, type but not in parameter list }
  if (pt.TokenType in Declarations) and pt.HasParentNode(nTopLevelSections, 1) and
    ( not pt.IsOnRightOf(nTypeDecl, ttEquals)) then
    exit(True);

  { procedure & function in class def get return but not blank line before }
  if (pt.TokenType in ProcedureWords + [ttProperty]) and
    (pt.HasParentNode([nClassType, nRecordType])) and
    (not IsGenericFunctionOrProperty(pt)) and
    (not IsClassFunctionOrProperty(pt)) then
    exit(True);

  { nested procs and top level procs get it as well }
  if (pt.TokenType in ProcedureWords) and
    (not IsClassFunctionOrProperty(pt)) and
    (not pt.HasParentNode(nType)) and
    (not IsIdentifier(pt, idAny)) and
    (not IsGenericFunctionOrProperty(pt)) then
    exit(True);

  { start of class property }
  if (pt.TokenType = ttClass) and pt.HasParentNode([nProperty],1) then
    exit(True);

  { start of class function decl in class }
  if (pt.TokenType = ttClass) and pt.HasParentNode([nProcedureDecl, nFunctionDecl, nProperty]) and
    (not IsGenericFunctionOrProperty(pt)) and
    pt.HasParentNode(nClassDeclarations) and
    (not pt.HasParentNode([nVarDecl, nConstDecl])) then
    exit(True);

  { access specifiying directive (private, public et al) in a class def }
  if pt.HasParentNode([nClassType, nRecordType]) and IsClassDirective(pt) then
  begin
    { no return before the "private" in "strict private" }
    if (pt.TokenType in [ttPrivate, ttProtected]) then
    begin
      lcPrev := pt.PriorSolidToken;
      Result := (lcPrev = nil) or (lcPrev.TokenType <> ttStrict);
    end
    else
      Result := True;
    exit;
  end;

  { return before 'class' in class function }
  if (pt.TokenType = ttClass) and pt.HasParentNode(ProcedureHeadings) and
    (not IsGenericFunctionOrProperty(pt)) and
    (RoundBracketLevel(pt) < 1) then
    exit(True);

  { "uses UnitName in 'File'" has a blank line before UnitName }
  if IsIdentifier(pt, idStrict) and (pt.HasParentNode(nUses)) and (ptNext.TokenType = ttIn) then
    exit(True);

  if (pt.TokenType = ttOpenSquareBracket)then
  begin
    // start of guid in interface
    if pt.HasParentNode(nInterfaceTypeGuid, 1) then
      exit(True);

    // start of attribute
    if pt.HasParentNode(nAttribute) then
      exit(True);
  end;

end;

function StartsProcedure(const pcSourceToken: TSourceToken): boolean;
var
  lcPrev: TSourceToken;
begin
  Result := (pcSourceToken.TokenType in ProcedureWords + [ttClass] +[ttGeneric]) and
    pcSourceToken.HasParentNode(ProcedureNodes, 2);

  if Result then
  begin
    lcPrev := pcSourceToken.PriorSolidToken;

    // check that it's not "procedure" in "class procedure foo;"
    // or "reference to procedure
    if (lcPrev <> nil) and (lcPrev.TokenType in [ttClass, ttGeneric, ttTo]) then
      result := False;
  end;

  // check that it's not a forward
  if Result then
  begin
    Result := ProcedureHasBody(pcSourceToken);
  end;

  if Result then
  begin
    Result := not StartsAnonymousMethod(pcSourceToken);
  end;
end;

constructor TReturnBefore.Create;
begin
  inherited;

  fiReturnsBefore := 0;
  fiNextReturnsBefore := 0;
  FormatFlags := FormatFlags + [eAddReturn];

  // the number of returns is one greater than the number of blank lines
  fiReturnsBeforeProcedure := FormattingSettings.Returns.LinesBeforeProcedure + 1;
end;

function TReturnBefore.EnabledVisitSourceToken(const pcToken: TObject): Boolean;
var
  lcSourceToken: TSourceToken;
  lcNext, lcPrev: TSourceToken;
  liReturnsNeeded: integer;
  liLoop: integer;
begin
  Result := False;
  lcSourceToken := TSourceToken(pcToken);
  lcNext := lcSourceToken.NextToken;
  if lcNext = nil then
    exit;

  liReturnsNeeded := 0;

  if NeedsBlankLine(lcSourceToken) then
    liReturnsNeeded := 2
  else if NeedsReturn(lcSourceToken, lcNext) then
    liReturnsNeeded := 1;

  { returns before a procedure/function/method }
  if  (fiReturnsBeforeProcedure > 0) and StartsProcedure(lcSourceToken) then
  begin
    liReturnsNeeded := fiReturnsBeforeProcedure;
  end;


  { number to insert = needed - actual }
  liReturnsNeeded := liReturnsNeeded - fiReturnsBefore;

  if liReturnsNeeded > 0 then
  begin
    // current token index changed 
    Result := True;

    lcPrev := lcSourceToken.PriorToken;
    if lcPrev.TokenType = ttWhiteSpace then
      BlankToken(lcPrev);

    for liLoop := 0 to liReturnsNeeded - 1 do
    begin
      InsertTokenBefore(lcSourceToken, NewReturn);
    end;

  end;

end;

procedure TReturnBefore.InspectSourceToken(const pcToken: TObject);
var
  lcSourceToken: TSourceToken;
begin
  {
    inspect the tokens as they go past
    this is a running total, that is affeced by returns & non-white-space chars
   A comment line is as good as a blank line for this

    if we encounter the tokens <return> <spaces> <word-needing-return before> the flag must be set true
   }
  fiReturnsBefore := fiNextReturnsBefore;

  lcSourceToken := TSourceToken(pcToken);

  if (lcSourceToken.TokenType = ttReturn) then
    Inc(fiNextReturnsBefore)
  else if not (lcSourceToken.TokenType in [ttReturn, ttWhiteSpace, ttComment]) then
    fiNextReturnsBefore := 0;

end;

function TReturnBefore.IsIncludedInSettings: boolean;
begin
  Result := FormattingSettings.Returns.AddGoodReturns;
end;

end.
