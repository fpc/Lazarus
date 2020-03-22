{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Interface to various IDE tools manipulating text.
}
unit TextTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  UITypes;

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
    
    Important: These functions are not thread safe!

    REMatches - function to test a regular expression.
    REVar - function to read the bracket values, found in the last call
            of REMatches.
    The ModifierStr sets the default values of r.e.syntax modifiers. Modifiers
    in r.e. (?ismx-ismx) will replace this default values.
    If you try to set unsupported modifier, an exception is raised

     Modifier /i - caseinsensitive, initialized from RegExprModifierI
     Modifier /s - '.' works as any char (else as [^\n]),
     Modifier /g - Turns all operators to non-greedy. e.g. '*' works as '*?',
                   all '+' as '+?' and so on.
     Modifier /m - Treat string as multiple lines. That is, change `^' and `$'
                   from matching at only the very start or end of the string to
                   the start or end of any line anywhere within the string.

    Examples:
      if REMatches('Lazarus','aza') then ...

      if REMatches('Lazarus','a(.)a','i') then
        s:=REVar(1); // this will be the 'z'
  }
  
var
  REException: ExceptClass; // initialized by the IDE
  
function REMatches(const TheText, RegExpr: string;
                const ModifierStr: string = ''; StartPos: integer = 1): boolean;
function REVar(Index: Integer): string; // 1 is the first
procedure REVarPos(Index: Integer; out MatchStart, MatchLength: integer);
function REVarCount: Integer;
function REReplace(const TheText, FindRegExpr, ReplaceRegExpr: string;
                    UseSubstutition: boolean;
                    const ModifierStr: string = ''): string;
function RESplit(const TheText, SeparatorRegExpr: string;
                 const ModifierStr: string = ''): TStrings;
procedure RESplit(const TheText, SeparatorRegExpr: string; Pieces: TStrings;
                  const ModifierStr: string = '');

// xml paths
function GetPathElement(const Path: string; StartPos: integer;
                        Stopper: char): string;




//------------------------------------------------------------------------------
// Internal stuff.

type
  TREMatchesFunction = function(const TheText, RegExpr, ModifierStr: string;
                                StartPos: integer): boolean;
  TREVarFunction = function(Index: Integer): string;
  TREVarPosProcedure = procedure(Index: Integer;
                                 out MatchStart, MatchLength: integer);
  TREVarCountFunction = function: Integer;
  TREReplaceProcedure = function(const TheText, FindRegExpr,
                            ReplaceRegExpr: string; UseSubstutition: boolean;
                            const ModifierStr: string): string;
  TRESplitFunction = procedure(const TheText, SeparatorRegExpr: string;
                               Pieces: TStrings; const ModifierStr: string);
var
  REMatchesFunction: TREMatchesFunction = nil; // initialized by the IDE ...
  REVarFunction: TREVarFunction = nil;
  REVarPosProcedure: TREVarPosProcedure = nil;
  REVarCountFunction: TREVarCountFunction = nil;
  REReplaceProcedure: TREReplaceProcedure = nil;
  RESplitFunction: TRESplitFunction = nil;

implementation

function REMatches(const TheText, RegExpr: string;
  const ModifierStr: string; StartPos: integer): boolean;
begin
  Result:=REMatchesFunction(TheText,RegExpr,ModifierStr,StartPos);
end;

function REVar(Index: Integer): string;
begin
  Result:=REVarFunction(Index);
end;

procedure REVarPos(Index: Integer; out MatchStart, MatchLength: integer);
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

