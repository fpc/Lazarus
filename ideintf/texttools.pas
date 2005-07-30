{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Interface to various IDE tools manipulating text.
}
unit TextTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms;

  { Sorting }
type
  TSortDirection = (sdAscending, sdDescending);
  TSortDomain = (sdWords, sdLines, sdParagraphs);

  TShowSortSelectionDialogFunc = function(const TheText: string;
    Highlighter: TObject; var SortedText: string): TModalResult;
  TSortTextFunc = function(const TheText: string; Direction: TSortDirection;
    Domain: TSortDomain; CaseSensitive, IgnoreSpace: boolean): string;

var
  ShowSortSelectionDialogFunc: TShowSortSelectionDialogFunc;
  SortTextFunc: TSortTextFunc;
  
  
  { Regular expressions

    This is a simple interface to regular expressions. The syntax is similar
    to Perl regular expressions. An illegal pattern will raise an Exception.

    REMatches - function to test a regular expression.
    REVar - function to read the bracket values, found in the last call
            of REMatches.
    The ModifierStr sets the default values of r.e.syntax modifiers. Modifiers
    in r.e. (?ismx-ismx) will replace this default values.
    If you try to set unsupported modifier, Error will be called

     Modifier /i - caseinsensitive, initialized from RegExprModifierI
     Modifier /s - '.' works as any char (else as [^\n]),
     Modifier /g - Turns all operators to non-greedy. e.g. '*' works as '*?',
                   all '+' as '+?' and so on.
     Modifier /m - Treat string as multiple lines. That is, change `^' and `$'
                   from matching at only the very start or end of the string to
                   the start or end of any line anywhere within the string.

    Examples:
      if REMatches('Lazarus','aza') then ...

      if REMatches('Lazarus','a(.)a') then
        s:=REVar(1); // this will be the 'z'
  }
  
var
  REException: ExceptClass; // initialized by the IDE
  
function REMatches(const TheText, RegExpr: string;
                   const ModifierStr: string = ''): boolean;
function REVar(Index: Integer): string;
procedure REVarPos(Index: Integer; var MatchStart, MatchLength: integer);
function REVarCount: Integer;
function REReplace(const TheText, FindRegExpr, ReplaceRegExpr: string;
                    UseSubstutition: boolean;
                    const ModifierStr: string = ''): string;
function RESplit(const TheText, SeparatorRegExpr: string;
                 const ModifierStr: string = ''): TStrings;
procedure RESplit(const TheText, SeparatorRegExpr: string; Pieces: TStrings;
                  const ModifierStr: string = '');

// identifier
function CreateFirstIdentifier(const Identifier: string): string;
function CreateNextIdentifier(const Identifier: string): string;

// xml paths
function GetPathElement(const Path: string; StartPos: integer;
                        Stopper: char): string;


//------------------------------------------------------------------------------
// Internal stuff.

type
  TREMatchesFunction = function(const TheText, RegExpr, ModifierStr: string
                                ): boolean;
  TREVarFunction = function(Index: Integer): string;
  TREVarPosProcedure = procedure(Index: Integer;
                                 var MatchStart, MatchLength: integer);
  TREVarCountFunction = function: Integer;
  TREReplaceProcedure = function(const TheText, FindRegExpr,
                            ReplaceRegExpr: string; UseSubstutition: boolean;
                            const ModifierStr: string): string;
  TRESplitFunction = procedure(const TheText, SeparatorRegExpr: string;
                               Pieces: TStrings; const ModifierStr: string);
var
  REMatchesFunction: TREMatchesFunction; // initialized by the IDE ...
  REVarFunction: TREVarFunction;
  REVarPosProcedure: TREVarPosProcedure;
  REVarCountFunction: TREVarCountFunction;
  REReplaceProcedure: TREReplaceProcedure;
  RESplitFunction: TRESplitFunction;

implementation

function REMatches(const TheText, RegExpr: string;
  const ModifierStr: string): boolean;
begin
  Result:=REMatchesFunction(TheText,RegExpr,ModifierStr);
end;

function REVar(Index: Integer): string;
begin
  Result:=REVarFunction(Index);
end;

procedure REVarPos(Index: Integer; var MatchStart, MatchLength: integer);
begin
  REVarPosProcedure(Index,MatchStart,MatchLength);
end;

function REVarCount: Integer;
begin
  Result:=REVarCountFunction();
end;

function REReplace(const TheText, FindRegExpr, ReplaceRegExpr: string;
  UseSubstutition: boolean; const ModifierStr: string): string;
begin
  Result:=REReplaceProcedure(TheText,FindRegExpr,ReplaceRegExpr,UseSubstutition,
                             ModifierStr);
end;

procedure RESplit(const TheText, SeparatorRegExpr: string; Pieces: TStrings;
  const ModifierStr: string);
begin
  RESplitFunction(TheText,SeparatorRegExpr,Pieces,ModifierStr);
end;

function RESplit(const TheText, SeparatorRegExpr: string;
  const ModifierStr: string): TStrings;
begin
  Result:=TStringList.Create;
  RESplit(TheText,SeparatorRegExpr,Result,ModifierStr);
end;

function CreateFirstIdentifier(const Identifier: string): string;
// example: Ident59 becomes Ident1
var
  p: Integer;
begin
  p:=length(Identifier);
  while (p>=1) and (Identifier[p] in ['0'..'9']) do dec(p);
  Result:=copy(Identifier,1,p)+'1';
end;

function CreateNextIdentifier(const Identifier: string): string;
// example: Ident59 becomes Ident60
var
  p: Integer;
begin
  p:=length(Identifier);
  while (p>=1) and (Identifier[p] in ['0'..'9']) do dec(p);
  Result:=copy(Identifier,1,p)
          +IntToStr(1+StrToIntDef(copy(Identifier,p+1,length(Identifier)-p),0));
end;

function GetPathElement(const Path: string; StartPos: integer;
  Stopper: char): string;
var
  p: LongInt;
begin
  p:=StartPos;
  while (p<=length(Path)) and (Path[p]<>Stopper) do inc(p);
  Result:=copy(Path,StartPos,p-StartPos);
end;

end.

