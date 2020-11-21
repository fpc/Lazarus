unit ParseTreeNodeType;

{
  This enumeration describes all of the types of parse tree nodes
  that we are interested in
}

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is ParseTreeNodeType, released May 2003.
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

type
  { roles that the interior node can play }
  TParseTreeNodeType = (
    nUnknown,
    nLeaf,
    nProgram,
    nUnit,
    nUnitHeader,
    nUnitName,
    nPackage,
    nLibrary,
    nUses,
    nUsesItem,
    nRequires,
    nContains,
    nIdentList,
    nIdentifier,
    nInterfaceSection,
    nImplementationSection,
    nBlock,
    nStatementList,
    nDeclSection,
    nLabelDeclSection,
    nConstSection,
    nConstDecl,
    nTypeSection,
    nTypeDecl,
    nArrayConstant,
    nRecordConstant,
    nRecordFieldConstant,
    nType,
    nRestrictedType,
    nSubrangeType,
    nEnumeratedType,
    nArrayType,
    nRecordType,
    nFieldDeclaration,
    nRecordVariantSection,
    nRecordVariant,
    nSetType,
    nProcedureType,
    nVarSection,
    nVarDecl,
    nVarAbsolute,
    nVariableInit,
    nDesignator,
    nExpression,
    nTerm,
    nUnaryOp,
    nActualParams,
    nStatement,
    nAssignment,
    nInline,
    nInlineItem,
    nStatementLabel,
    nCompoundStatement,
    nIfCondition,
    nIfBlock,
    nElseBlock,
    nCaseStatement,
    nCaseSelector,
    nCaseLabels,
    nCaseLabel,
    nElseCase,
    nRepeatStatement,
    nWhileStatement,
    nLoopHeaderExpr,
    nBlockHeaderExpr,
    nForStatement,
    nWithStatement,
    nTryAndHandlerBlock,
    nTryBlock,
    nFinallyBlock,
    nExceptBlock,
    nExceptionHandlers,
    nOnExceptionHandler,
    nProcedureDecl,
    nFunctionDecl,
    nConstructorDecl,
    nDestructorDecl,
    nFunctionHeading,
    nProcedureHeading,
    nConstructorHeading,
    nDestructorHeading,
    nFormalParams,
    nFormalParam,
    nFunctionReturnType,
    nProcedureDirectives,
    nExternalDirective,
    nObjectType,
    nInitSection,
    nClassType,
    nClassHeritage,
    nClassBody,
    nClassVisibility,
    nClassDeclarations,
    nProperty,
    nPropertyParameterList,
    nPropertySpecifier,
    nInterfaceType,
    nInterfaceHeritage,
    nInterfaceTypeGuid,
    nInterfaceBody,
    nBracketedQual,
    nAsm,
    nAsmStatement,
    nAsmIdent,
    nAsmOpcode,
    nAsmParam,
    nAsmLabel,
    nHintDirectives,
    nPropertyDirective,
    nExports,
    nExportedProc,
    nLiteralString,
    nHashLiteralChar,
    nHatLiteralChar,
    nAttribute,
    nClassVars,
    nGeneric,
    nAnonymousMethod,
    nMethodReferenceType
    );

  TParseTreeNodeTypeSet = set of TParseTreeNodeType;

const
  DirectiveNodes: TParseTreeNodeTypeSet    =
    [nProcedureDirectives, nExternalDirective, nHintDirectives, nPropertyDirective];
  ProcedureNodes: TParseTreeNodeTypeSet    =
    [nProcedureDecl, nFunctionDecl, nConstructorDecl, nDestructorDecl];
  ProcedureHeadings: TParseTreeNodeTypeSet =
    [nFunctionHeading, nProcedureHeading, nConstructorHeading, nDestructorHeading];

  ObjectTypes: TParseTreeNodeTypeSet  = [nObjectType, nClassType, nInterfaceType];
  ObjectBodies: TParseTreeNodeTypeSet = [nClassBody, nInterfaceBody];

  { can declare these at the start of a procedure }
  InProcedureDeclSections: TParseTreeNodeTypeSet =
    [nVarSection, nConstSection, nLabelDeclSection, nTypeSection];

  UsesClauses: TParseTreeNodeTypeSet = [nUses, nRequires, nContains];

  TopOfProgramSections = [nProgram, nPackage, nLibrary];

  TopOfFileSection = [nProgram, nPackage, nLibrary, nUnit];

  { can find these blocks of def/dels outside of anything }
  nTopLevelSections = [nTypeSection, nConstSection, nVarSection,
    nLabelDeclSection, nExports];

  MethodDeclarations: TParseTreeNodeTypeSet =
    [nProcedureDecl, nFunctionDecl, nConstructorDecl, nDestructorDecl];
  MethodHeadings: TParseTreeNodeTypeSet =
    [nFunctionHeading, nProcedureHeading, nConstructorHeading, nDestructorHeading];

function NodeTypeToString(const pe: TParseTreeNodeType): string; inline;

implementation

uses SysUtils;

const
  TreeNodeTypeNames: array[TParseTreeNodeType] of string = (
    'UnkNown', 'Leaf', 'Program', 'Unit', 'Unit header', 'Unit name', 'Package', 'Library', 'Uses',
    'Uses Item', 'Requires', 'Contains', 'ident list', 'Identifier', 'Interface section',
    'Implementation section', 'Block', 'Statement list', 'Decl section', 'Label decl section',
    'const section', 'Const decl', 'type section', 'Type Decl', 'Array constant', 'Record Constant',
    'Field constant', 'Type', 'Restricted type', 'Subrange type', 'Enumerated type', 'Array type',
    'record type', 'Field declarations', 'Record variant section', 'Record variant', 'Set type',
    'procedure type', 'Var section', 'Var decl', 'Absolute var', 'Variable init', 'Designator',
    'Expression', 'Term', 'Unary op', 'Actual params', 'Statement', 'Assignment', 'Inline',
    'Inline item', 'Statement label', 'Compound statement', 'If Condition', 'If Block', 'Else block',
    'Case statement', 'Case selector', 'Case labels', 'Case label', 'else case', 'Repeat statement',
    'While Statement', 'Loop header expr', 'Block header expr', 'For statement', 'With statement',
    'try and handler block', 'try block', 'finally block', 'except block', 'Exception handlers',
    'On exception handler', 'Procedure decl', 'Function Decl', 'Constructor decl', 'Destructor decl',
    'Function heading', 'Procedure Heading', 'Constructor Heading', 'Destructor heading',
    'Formal params', 'formal param', 'Function Return type', 'Procedure directives',
    'external directive', 'object type', 'init section', 'class type', 'class heritage',
    'class body', 'class visiblity', 'class declarations', 'property', 'property param list',
    'property specifier', 'interface type', 'interface heritage', 'interface type guid',
    'interface body', 'bracketed qual', 'asm', 'asm statement', 'asm ident',
    'asm opcode', 'asm param', 'asm label', 'hint directives', 'property directive',
    'exports', 'exported proc', 'literal string', 'hash literal char', 'hat literal char',
    'Attribute', 'Class vars', 'Generic', 'Anonymous method', 'Method reference type'
    );

function NodeTypeToString(const pe: TParseTreeNodeType): string;
begin
  Result := TreeNodeTypeNames[pe];
end;    

end.
