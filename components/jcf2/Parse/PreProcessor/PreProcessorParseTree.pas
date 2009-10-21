unit PreProcessorParseTree;

{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is PreProcessorParseTree, released October 2003.
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

{ AFS 16 Oct 2003
  parse tree for preprocessor symbols
}

uses
  { delphi }
  Classes,
  { local }
  Tokens, SourceTokenList, SourceToken;

type
  TPreProcessorParseTree = class(TObject)
  private
    { stage of the token list }
    fcTokens: TSourceTokenList;
    fiCurrentTokenIndex: integer;

    { state from the preprocessor statements }
    fbPreprocessorIncluded: boolean;
    fcDefinedSymbols: TStringList;

    procedure ParseProcessorBlock;

    procedure ParseDefine(const psSymbol: string);
    procedure ParseUndef(const psSymbol: string);
    procedure ParseIfDef(const psSymbol: string);
    procedure ParseIfNotDef(const psSymbol: string);
    procedure ParseIfOpt(const psOption: string);
    procedure ParseIfExpr(const psExpr: string);
    procedure ParseElseIf(const psExpr: string; var pbAlreadyMatchedClause: boolean);
    procedure ParseElse(const pbAlreadyMatchedClause: boolean);

    procedure ParseNonPreProc(const peEndTokens: TPreProcessorSymbolTypeSet);
    procedure ParseOptTail(pbAlreadyMatchedClause: boolean);
    procedure ParsePreProcessorDirective(const peSymbolType: TPreProcessorSymbolType);
      overload;
    procedure ParsePreProcessorDirective(
      const peSymbolTypes: TPreProcessorSymbolTypeSet); overload;

    procedure RemoveDefinedSymbol(const psSymbol: string);
    function SymbolIsDefined(const psSymbol: string): boolean;


    function CurrentToken: TSourceToken;

    procedure CompactTokens;

    procedure NextToken;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ProcessTokenList(const pcTokens: TSourceTokenList);
    function EvalPreProcessorExpression(const psExpression: string): boolean;
    procedure AddDefinedSymbol(const psSymbol: string);
  end;

procedure RemoveConditionalCompilation(const pcTokens: TSourceTokenList);

implementation

uses
  { delphi }
  SysUtils, Forms,
  { local }
  PreProcessorExpressionTokenise, PreProcessorExpressionParser,
  ParseError, JcfSettings;

procedure RemoveConditionalCompilation(const pcTokens: TSourceTokenList);
var
  lcParser: TPreProcessorParseTree;
begin
  lcParser := TPreProcessorParseTree.Create;
  try
    lcParser.ProcessTokenList(pcTokens);
  finally
    lcParser.Free;
  end;
end;


{ right, we need a grammar

  unit -> non-preproc [preproc non-preproc]...

  preproc ->
    Define
    Undef
    Ifdef
    Ifndef
    Ifopt
    If

  Define ->
    '$DEFINE' symbol

  undef ->
    '$UNDEF' symbol

  ifdef ->
    '$IFDEF' symbol non-preproc opt-tail '$ENDIF'

  ifndef ->
    '$IFNDEF' symbol non-preproc opt-tail '$ENDIF'

  ifopt ->
    '$IFOFT' compileroption non-preproc opt-tail '$ENDIF'

  if ->
    '$IF' expr non-preproc opt-tail '$IFEND'

  opt-tail -> opt-elseif opt-else

  opt-elseif ->
    ['$ELSEIF' expr non-preproc]...
  opt-else ->
    ['$ELSE' non-preproc]

  non-preproc -> 0 or more statements that are not preproc
  symbol -> an identifier
  expr ->

  Build a tree, or try to exclude those not worthy?
  This will set the PreprocessedOut flag on tokens

}


constructor TPreProcessorParseTree.Create;
begin
  inherited;

  fiCurrentTokenIndex := 0;

  fcDefinedSymbols := TStringList.Create;
  fcDefinedSymbols.Sorted := True;
  fcDefinedSymbols.Duplicates := dupIgnore;

  // import user settings 
  fcDefinedSymbols.Assign(FormatSettings.PreProcessor.DefinedSymbols);
end;

destructor TPreProcessorParseTree.Destroy;
begin
  FreeAndNil(fcDefinedSymbols);

  inherited;
end;


procedure TPreProcessorParseTree.NextToken;
{$IFNDEF COMMAND_LINE}
const
   UPDATE_INTERVAL = 512;
{$ENDIF}
begin
  Inc(fiCurrentTokenIndex);
  
  {$IFNDEF COMMAND_LINE}  
  if (fiCurrentTokenIndex mod UPDATE_INTERVAL) = 0 then
     Application.ProcessMessages;
  {$ENDIF}
end;

function TPreProcessorParseTree.CurrentToken: TSourceToken;
begin
  if fcTokens = nil then
    Result := nil
  else if fiCurrentTokenIndex >= fcTokens.Count then
    Result := nil
  else
    Result := fcTokens.SourceTokens[fiCurrentTokenIndex];
end;

procedure TPreProcessorParseTree.ProcessTokenList(const pcTokens: TSourceTokenList);
var
  liLoop:  integer;
  lcToken: TSourceToken;
begin
  Assert(pcTokens <> nil);

  fcTokens := pcTokens;

  for liLoop := 0 to fcTokens.Count - 1 do
  begin
    lcToken := fcTokens.SourceTokens[liLoop];

    if lcToken.CommentStyle = eCompilerDirective then
      lcToken.GeneratePreProcessorData;
  end;


  {   unit -> non-preproc [preproc non-preproc]... }
  fiCurrentTokenIndex    := 0;
  fbPreprocessorIncluded := True;

  while fiCurrentTokenIndex < pcTokens.Count do
  begin
    lcToken := pcTokens.SourceTokens[fiCurrentTokenIndex];
    lcToken.PreProcessedOut := False;

    if lcToken.CommentStyle = eCompilerDirective then
      ParseProcessorBlock
    else
      NextToken;
  end;

  CompactTokens;
end;

procedure TPreProcessorParseTree.ParseProcessorBlock;
var
  lcToken: TSourceToken;
begin
{  preproc ->
    Define
    Undef
    Ifdef
    Ifndef
    Ifopt
    If
 }
  lcToken := CurrentToken;
  Assert(lcToken <> nil);

  case lcToken.PreprocessorSymbol of
    ppDefine:
      ParseDefine(lcToken.PreProcessorText);
    ppUndef:
      ParseUndef(lcToken.PreProcessorText);
    ppIfDef:
      ParseIfDef(lcToken.PreProcessorText);
    ppIfNotDef:
      ParseIfNotDef(lcToken.PreProcessorText);
    ppIfOpt:
      ParseIfOpt(lcToken.PreProcessorText);
    ppIfExpr:
      ParseIfExpr(lcToken.PreProcessorText);
    ppNone:
    begin
      { do nothing. this could be a "$R *.dfm" or other compiler directive 
        //raise TEParseError.Create('Unknown preprocessor symbol', lcToken);
      }
      NextToken;
    end;
    else
      raise TEParseError.Create('Unexpected preprocessor symbol', lcToken);
  end;
end;


procedure TPreProcessorParseTree.ParseDefine(const psSymbol: string);
begin
  AddDefinedSymbol(psSymbol);
  NextToken;
end;

procedure TPreProcessorParseTree.ParseUndef;
begin
  RemoveDefinedSymbol(psSymbol);
  NextToken;
end;


procedure TPreProcessorParseTree.ParseIfDef(const psSymbol: string);
var
  lbEval, lbWasIncluded: boolean;
begin
{  ifdef ->
    '$IFDEF' symbol non-preproc opt-tail '$ENDIF' }
  lbWasIncluded := fbPreprocessorIncluded;

  if fbPreprocessorIncluded then
    lbEval := SymbolIsDefined(psSymbol)
  else
    lbEval := False;

  fbPreprocessorIncluded := lbEval;
  NextToken;

  ParseNonPreProc(PREPROC_BLOCK_END);
  { must not include the else block if
   - the expr was true & the first block was in
   - the whole thing is inside a larger block that's out }
  ParseOptTail(lbEval or ( not lbWasIncluded));
  ParsePreProcessorDirective([ppEndIf, ppIfEnd]);

  fbPreprocessorIncluded := lbWasIncluded;
end;

procedure TPreProcessorParseTree.ParseIfExpr(const psExpr: string);
var
  lbEval, lbWasIncluded: boolean;
begin
  lbWasIncluded := fbPreprocessorIncluded;

  if fbPreprocessorIncluded then
    lbEval := EvalPreProcessorExpression(psExpr)
  else
    lbEval := False;

  fbPreprocessorIncluded := lbEval;
  NextToken;

  ParseNonPreProc(PREPROC_BLOCK_END);
  { must not include the else block if
   - the expr was true & the first block was in
   - the whole thing is inside a larger block that's out }
  ParseOptTail(lbEval or ( not lbWasIncluded));
  ParsePreProcessorDirective([ppEndIf, ppIfEnd]);
  fbPreprocessorIncluded := lbWasIncluded;

end;

procedure TPreProcessorParseTree.ParseIfNotDef(const psSymbol: string);
var
  lbEval, lbWasIncluded: boolean;
begin
  lbWasIncluded := fbPreprocessorIncluded;

  if fbPreprocessorIncluded then
    lbEval := not SymbolIsDefined(psSymbol)
  else
    lbEval := False;

  fbPreprocessorIncluded := lbEval;
  NextToken;

  ParseNonPreProc(PREPROC_BLOCK_END);
  { must not include the else block if
   - the expr was true & the first block was in
   - the whole thing is inside a larger block that's out }
  ParseOptTail(lbEval or ( not lbWasIncluded));
  ParsePreProcessorDirective([ppEndIf, ppIfEnd]);

  fbPreprocessorIncluded := lbWasIncluded;
end;

procedure TPreProcessorParseTree.ParseIfOpt(const psOption: string);
var
  lbEval, lbWasIncluded: boolean;
begin
  lbWasIncluded := fbPreprocessorIncluded;

  if fbPreprocessorIncluded then
    lbEval := FormatSettings.PreProcessor.OptionIsDefined(psOption)
  else
    lbEval := False;

  fbPreprocessorIncluded := lbEval;
  NextToken;

  ParseNonPreProc(PREPROC_BLOCK_END);
  { must not include the else block if
   - the expr was true & the first block was in
   - the whole thing is inside a larger block that's out }
  ParseOptTail(lbEval or ( not lbWasIncluded));
  ParsePreProcessorDirective([ppEndIf, ppIfEnd]);
  fbPreprocessorIncluded := lbWasIncluded;
end;

procedure TPreProcessorParseTree.ParsePreProcessorDirective(
  const peSymbolType: TPreProcessorSymbolType);
begin
  ParsePreProcessorDirective([peSymbolType]);
end;

procedure TPreProcessorParseTree.ParsePreProcessorDirective(
  const peSymbolTypes: TPreProcessorSymbolTypeSet);
var
  lcToken: TSourceToken;
begin
  lcToken := CurrentToken;
  Assert(lcToken <> nil, 'nil token, expected ' +
    PreProcSymbolTypeSetToString(peSymbolTypes));

  if lcToken.CommentStyle <> eCompilerDirective then
    raise TEParseError.Create('Expected compiler directive', lcToken);

  if not (lcToken.PreprocessorSymbol in peSymbolTypes) then
    raise TEParseError.Create('Expected compiler directive ' +
      PreProcSymbolTypeSetToString(peSymbolTypes), lcToken);

  NextToken;
end;


procedure TPreProcessorParseTree.ParseNonPreProc(
  const peEndTokens: TPreProcessorSymbolTypeSet);
var
  lcToken: TSourceToken;
begin
  { go forward until another preprocessor tag is found. or end of file }
  while (fiCurrentTokenIndex < fcTokens.Count) do
  begin
    lcToken := CurrentToken;
    Assert(lcToken <> nil);

    lcToken.PreProcessedOut := not fbPreprocessorIncluded;

    if (lcToken.CommentStyle = eCompilerDirective) then
    begin
      if lcToken.PreprocessorSymbol in peEndTokens then
      begin
        lcToken.PreProcessedOut := False;
        break;
      end;

      // if we are preproc'd in then parse these
      ParseProcessorBlock;
    end
    else
      NextToken;
  end;
end;

procedure TPreProcessorParseTree.ParseOptTail(pbAlreadyMatchedClause: boolean);
var
  lcToken: TSourceToken;
begin
  lcToken := CurrentToken;

  while (lcToken <> nil) and (lcToken.PreprocessorSymbol = ppElseIf) do
  begin
    ParseElseIf(lcToken.PreProcessorText, pbAlreadyMatchedClause);
    lcToken := CurrentToken;
  end;

  if (lcToken <> nil) and (lcToken.PreprocessorSymbol = ppElse) then
    ParseElse(pbAlreadyMatchedClause);
end;

procedure TPreProcessorParseTree.ParseElse(const pbAlreadyMatchedClause: boolean);
begin
  NextToken;

  fbPreprocessorIncluded := ( not pbAlreadyMatchedClause);
  ParseNonPreProc([ppEndIf, ppIfEnd]);
end;

procedure TPreProcessorParseTree.ParseElseIf(const psExpr: string;
  var pbAlreadyMatchedClause: boolean);
begin
  if not pbAlreadyMatchedClause then
  begin
    { if we have't already passed an if/elseif condition, then try this one
      if we have already passed one, then it doesn't matter if this one is true
      we're not going to go there. }
    fbPreprocessorIncluded := EvalPreProcessorExpression(psExpr);
    pbAlreadyMatchedClause := fbPreprocessorIncluded;
  end
  else
    fbPreprocessorIncluded := False;

  NextToken;

  ParseNonPreProc([ppElse, ppElseIf, ppEndIf, ppIfEnd]);
end;

procedure TPreProcessorParseTree.AddDefinedSymbol(const psSymbol: string);
begin
  if (psSymbol <> '') and ( not SymbolIsDefined(psSymbol)) then
    fcDefinedSymbols.Add(psSymbol);
end;

procedure TPreProcessorParseTree.RemoveDefinedSymbol(const psSymbol: string);
var
  liIndex: integer;
begin
  liIndex := fcDefinedSymbols.IndexOf(psSymbol);
  if liIndex >= 0 then
    fcDefinedSymbols.Delete(liIndex);
end;

function TPreProcessorParseTree.SymbolIsDefined(const psSymbol: string): boolean;
begin
  Result := fcDefinedSymbols.IndexOf(psSymbol) >= 0;
end;

function TPreProcessorParseTree.EvalPreProcessorExpression(
  const psExpression: string): boolean;
var
  lcTokeniser: TPreProcessorExpressionTokeniser;
  lcParser:    TPreProcessorExpressionParser;
begin
  Result := False;
  Assert(psExpression <> '');

  lcTokeniser := TPreProcessorExpressionTokeniser.Create;
  lcParser    := TPreProcessorExpressionParser.Create;
  try
    // tokenise
    try
      lcTokeniser.Expression := psExpression;
      lcTokeniser.Tokenise;
    except
      on E: Exception do
        raise TEParseError.Create('Exception tokenising "' + psExpression +
          '": ' + E.Message, CurrentToken);
    end;

    { !! unknown syntax. Accept expression as true ? fix in later version }
    if lcTokeniser.HasError then
    begin
      Result := True;
    end
    else
    begin

      // parse
      try
        lcParser.Tokens := lcTokeniser.Tokens;
        lcParser.DefinedSymbols := fcDefinedSymbols;

        Result := lcParser.Parse;
      except
        on E: Exception do
          raise TEParseError.Create('Exception parsing "' + psExpression +
            '": ' + E.Message, CurrentToken);
      end;
    end;
  finally
    lcTokeniser.Free;
    lcParser.Free;
  end;
end;


{ hide the bits that are preprocessed out }
procedure TPreProcessorParseTree.CompactTokens;
var
  liLoop:    integer;
  lsOutText: string;
  lcCurrentToken, lcExcludedText: TSourceToken;
begin

  // right, what's out?
  liLoop := 0;
  while liLoop < fcTokens.Count - 1 do
  begin
    lcCurrentToken := fcTokens.SourceTokens[liLoop];

    if lcCurrentToken.PreprocessedOut then
    begin
      lsOutText := '';

      while (lcCurrentToken <> nil) and lcCurrentToken.PreprocessedOut do
      begin
        lsOutText := lsOutText + lcCurrentToken.SourceCode;
        fcTokens.Delete(liLoop);
        lcCurrentToken.Free; // not an owning list, must manually avoid leak 

        { next one will have shifted up }
        lcCurrentToken := fcTokens.SourceTokens[liLoop];
      end;

      lcExcludedText := TSourceToken.Create;
      lcExcludedText.FileName := lcCurrentToken.FileName;
      lcExcludedText.TokenType := ttConditionalCompilationRemoved;
      lcExcludedText.SourceCode := lsOutText;

      fcTokens.Insert(liLoop, lcExcludedText);
    end
    else
      Inc(liLoop);
  end;
end;

end.
