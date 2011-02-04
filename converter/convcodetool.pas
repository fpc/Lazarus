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

  Author: Juha Manninen

  Abstract:
    Methods that use and extend codetools features. Needed by Delphi converter.
    Some of these methods could be made part of codetools.
}
unit ConvCodeTool;

{$mode objfpc}{$H+}

interface

uses
  // LCL+FCL
  Classes, SysUtils, FileProcs, Forms, Controls, DialogProcs, Dialogs,
  contnrs, strutils,
  // IDE
  LazarusIDEStrConsts, LazIDEIntf, FormEditor, IDEMsgIntf,
  // codetools
  CodeToolManager, StdCodeTools, CodeTree, CodeAtom, AVL_Tree,
  FindDeclarationTool, PascalReaderTool, PascalParserTool, LFMTrees,
  ExprEval, KeywordFuncLists, BasicCodeTools, LinkScanner,
  CodeCache, SourceChanger, CustomCodeTool, CodeToolsStructs, EventCodeTool,
  // Converter
  ConverterTypes, ConvertSettings, ReplaceNamesUnit, ReplaceFuncsUnit;

type

  { TCodeToolLink }

  TCodeToolLink = class
  private
  protected
    fCodeTool: TCodeTool;
    fCode: TCodeBuffer;
    fSrcCache: TSourceChangeCache;
    fIsMainFile: Boolean;                 // Main project / package file.
    fIsConsoleApp: Boolean;
    fAskAboutError: Boolean;
    fSettings: TConvertSettings;          // Conversion settings.
    procedure InitCodeTool;
    function HandleCodetoolError: TModalResult;
  public
    constructor Create(ACode: TCodeBuffer);
    destructor Destroy; override;
    procedure ResetMainScanner;
  public
    property CodeTool: TCodeTool read fCodeTool;
    property Code: TCodeBuffer read fCode;
    property SrcCache: TSourceChangeCache read fSrcCache;
    property IsMainFile: Boolean read fIsMainFile write fIsMainFile;
    property IsConsoleApp: Boolean read fIsConsoleApp write fIsConsoleApp;
    property AskAboutError: Boolean read fAskAboutError write fAskAboutError;
    property Settings: TConvertSettings read fSettings write fSettings;
  end;

  { TConvDelphiCodeTool }

  TConvDelphiCodeTool = class
  private
    fCTLink: TCodeToolLink;
    fHasFormFile: boolean;
    fLowerCaseRes: boolean;
    fAddUnitEvent: TAddUnitEvent;
    fDfmDirectiveStart: integer;
    fDfmDirectiveEnd: integer;
    // Delphi Function names to replace with FCL/LCL functions.
    fDefinedProcNames: TStringList;
    // List of TFuncReplacement.
    fFuncsToReplace: TObjectList;

    function AddModeDelphiDirective: boolean;
    function RenameResourceDirectives: boolean;
    function ReplaceFuncsInSource: boolean;
    function RememberProcDefinition(aNode: TCodeTreeNode): TCodeTreeNode;
    function ReplaceFuncCalls(aIsConsoleApp: boolean): boolean;
  public
    constructor Create(ACTLink: TCodeToolLink);
    destructor Destroy; override;
    function Convert: TModalResult;
    function FindApptypeConsole: boolean;
    function FixMainClassAncestor(const AClassName: string;
                                  AReplaceTypes: TStringToStringTree): boolean;
  public
    property HasFormFile: boolean read fHasFormFile write fHasFormFile;
    property LowerCaseRes: boolean read fLowerCaseRes write fLowerCaseRes;
    property AddUnitEvent: TAddUnitEvent read fAddUnitEvent write fAddUnitEvent;
  end;


implementation

{ TCodeToolLink }

constructor TCodeToolLink.Create(ACode: TCodeBuffer);
begin
  inherited Create;
  fCode:=ACode;
  fIsConsoleApp:=False;
  fAskAboutError:=True;
  InitCodeTool;
end;

destructor TCodeToolLink.Destroy;
begin
  inherited Destroy;
end;

procedure TCodeToolLink.InitCodeTool;
begin
  // Initialize codetools. (Copied from TCodeToolManager.)
  if not CodeToolBoss.InitCurCodeTool(fCode) then exit;
  try
    fCodeTool:=CodeToolBoss.CurCodeTool;
    fSrcCache:=CodeToolBoss.SourceChangeCache;
    ResetMainScanner;
  except
    on e: Exception do
      CodeToolBoss.HandleException(e);
  end;
end;

procedure TCodeToolLink.ResetMainScanner;
begin
  fSrcCache.MainScanner:=fCodeTool.Scanner;
end;

function TCodeToolLink.HandleCodetoolError: TModalResult;
// returns mrOk or mrAbort
const
  CodetoolsFoundError='The codetools found an error in unit %s:%s%s%s';
var
  ErrMsg: String;
begin
  ErrMsg:=CodeToolBoss.ErrorMessage;
  LazarusIDE.DoJumpToCodeToolBossError;
  if fAskAboutError then begin
    Result:=QuestionDlg(lisCCOErrorCaption,
      Format(CodetoolsFoundError, [ExtractFileName(fCode.Filename), #13, ErrMsg, #13]),
      mtWarning, [mrIgnore, lisIgnoreAndContinue, mrAbort], 0);
    if Result=mrIgnore then Result:=mrOK;
  end else begin
    Result:=mrOK;
  end;
end;

{ TConvDelphiCodeTool }

constructor TConvDelphiCodeTool.Create(ACTLink: TCodeToolLink);
begin
  inherited Create;
  fCTLink:=ACTLink;
  fLowerCaseRes:=false;
end;

destructor TConvDelphiCodeTool.Destroy;
begin
  inherited Destroy;
end;

function TConvDelphiCodeTool.Convert: TModalResult;
// add {$mode delphi} directive
// remove {$R *.dfm} or {$R *.xfm} directive
// Change {$R *.RES} to {$R *.res} if needed
// TODO: fix delphi ambiguouties like incomplete proc implementation headers
begin
  Result:=mrCancel;
  try
    fCTLink.SrcCache.BeginUpdate;
    try
      // these changes can be applied together without rescan
      if not AddModeDelphiDirective then exit;
      if not RenameResourceDirectives then exit;
      if fCTLink.Settings.FuncReplaceMode=rsEnabled then
        if not ReplaceFuncCalls(fCTLink.IsConsoleApp) then exit;
    finally
      fCTLink.SrcCache.EndUpdate;
    end;
    Result:=mrOK;
  except
    on e: Exception do begin
      CodeToolBoss.HandleException(e);
      Result:=fCTLink.HandleCodetoolError;
    end;
  end;
end;

function TConvDelphiCodeTool.FindApptypeConsole: boolean;
// Return true if there is {$APPTYPE CONSOLE} directive.
var
  ParamPos, ACleanPos: Integer;
begin
  Result:=false;
  ACleanPos:=0;
  with fCTLink.CodeTool do begin
    BuildTree(true);
    ACleanPos:=FindNextCompilerDirectiveWithName(Src, 1, 'Apptype',
                                                 Scanner.NestedComments, ParamPos);
    if (ACleanPos>0) and (ACleanPos<=SrcLen) and (ParamPos>0) then
      Result:=LowerCase(copy(Src,ParamPos,7))='console';
  end;
end;

function TConvDelphiCodeTool.AddModeDelphiDirective: boolean;
var
  ModeDirectivePos: integer;
  InsertPos: Integer;
  s: String;
begin
  Result:=false;
  with fCTLink.CodeTool do begin
    BuildTree(true);
    if not FindModeDirective(false,ModeDirectivePos) then begin
      // add {$MODE Delphi} behind source type
      if Tree.Root=nil then exit;
      MoveCursorToNodeStart(Tree.Root);
      ReadNextAtom; // 'unit', 'program', ..
      ReadNextAtom; // name
      ReadNextAtom; // semicolon
      InsertPos:=CurPos.EndPos;
      if fCTLink.Settings.SupportDelphi then
        s:='{$IFDEF FPC}'+LineEnding+'  {$MODE Delphi}'+LineEnding+'{$ENDIF}'
      else
        s:='{$MODE Delphi}';
      fCTLink.SrcCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,s);
    end;
    // changing mode requires rescan
    BuildTree(false);
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.RenameResourceDirectives: boolean;
// rename {$R *.dfm} directive to {$R *.lfm}, or lowercase it.
// lowercase {$R *.RES} to {$R *.res}
var
  ParamPos, ACleanPos: Integer;
  Key, LowKey, NewKey: String;
  s: string;
  AlreadyIsLfm: Boolean;
begin
  Result:=false;
  AlreadyIsLfm:=false;
  fDfmDirectiveStart:=-1;
  fDfmDirectiveEnd:=-1;
  ACleanPos:=1;
  // find $R directive
  with fCTLink.CodeTool do
    repeat
      ACleanPos:=FindNextCompilerDirectiveWithName(Src, ACleanPos, 'R',
                                                   Scanner.NestedComments, ParamPos);
      if (ACleanPos<1) or (ACleanPos>SrcLen) or (ParamPos>SrcLen-6) then break;
      NewKey:='';
      if (Src[ACleanPos]='{') and
         (Src[ParamPos]='*') and (Src[ParamPos+1]='.') and (Src[ParamPos+5]='}')
      then begin
        Key:=copy(Src,ParamPos+2,3);
        LowKey:=LowerCase(Key);
        // Form file resource rename or lowercase:
        if (LowKey='dfm') or (LowKey='xfm') then begin
          if fCTLink.Settings.SupportDelphi then begin
            // Use the same dfm file. Lowercase existing key.
            if fCTLink.Settings.SameDfmFile and (Key<>LowKey) then
              NewKey:=LowKey;
            // Later IFDEF will be added so that Delphi can still use .dfm.
            fDfmDirectiveStart:=ACleanPos;
            fDfmDirectiveEnd:=ParamPos+6;
          end
          else       // Change .dfm to .lfm.
            NewKey:='lfm';
        end
        // If there already is .lfm, prevent adding IFDEF for .dfm / .lfm.
        else if LowKey='lfm' then
          AlreadyIsLfm:=true
        // lowercase {$R *.RES} to {$R *.res}
        else if (Key='RES') and fLowerCaseRes then
          NewKey:=LowKey;
        // Now change code.
        if NewKey<>'' then
          if not fCTLink.SrcCache.Replace(gtNone,gtNone,ParamPos+2,ParamPos+5,NewKey) then exit;
      end;
      ACleanPos:=FindCommentEnd(Src, ACleanPos, Scanner.NestedComments);
    until false;
  // if there is already .lfm file, don't add IFDEF for .dfm / .lfm.
  if fCTLink.Settings.SupportDelphi and (fDfmDirectiveStart<>-1)
  and not AlreadyIsLfm then begin
    // Add IFDEF for .lfm and .dfm allowing Delphi to use .dfm.
    s:='{$IFNDEF FPC}'+LineEnding+
       '  {$R *.dfm}'+LineEnding+
       '{$ELSE}'+LineEnding+
       '  {$R *.lfm}'+LineEnding+
       '{$ENDIF}';
    Result:=fCTLink.SrcCache.Replace(gtNone,gtNone,fDfmDirectiveStart,fDfmDirectiveEnd,s);
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.FixMainClassAncestor(const AClassName: string;
                                    AReplaceTypes: TStringToStringTree): boolean;
// Replace the ancestor type of main form with a fall-back type if needed.
var
  ANode, InheritanceNode: TCodeTreeNode;
  TypeUpdater: TStringMapUpdater;
  OldType, NewType: String;
begin
  Result:=false;
  with fCTLink.CodeTool do begin
    BuildTree(true);
    // Find the class name that the main class inherits from.
    ANode:=FindClassNodeInUnit(AClassName,true,false,false,false);
    if ANode=nil then exit;
    InheritanceNode:=FindInheritanceNode(ANode);
    if InheritanceNode=nil then exit;
    ANode:=InheritanceNode.FirstChild;
    if ANode=nil then exit;
    if ANode.Desc=ctnIdentifier then begin
      MoveCursorToNodeStart(ANode);  // cursor to the identifier
      ReadNextAtom;
      OldType:=GetAtom;
    end;
    TypeUpdater:=TStringMapUpdater.Create(AReplaceTypes);
    try
      // Find replacement for ancestor type maybe using regexp syntax.
      if TypeUpdater.FindReplacement(OldType, NewType) then begin
        fCTLink.ResetMainScanner;
        if not fCTLink.SrcCache.Replace(gtNone, gtNone,
                               CurPos.StartPos, CurPos.EndPos, NewType) then exit;
        if not fCTLink.SrcCache.Apply then exit;
      end;
    finally
      TypeUpdater.Free;
    end;
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.ReplaceFuncsInSource: boolean;
// Replace the function names and parameters in source.
var
  ReplacementParams: TObjectList;           // Replacement parameters.

  function ParseReplacementParams(const aStr: string): integer;
  // Parse replacement params. They show which original params are copied where.
  // Returns the first position where comments can be searched from.
  var
    i, xNum, xStart, xLen: Integer;
  begin
    i:=0;
    while i<Length(aStr) do begin
      Inc(i);
      if aStr[i]='$' then begin
        xStart:=i;
        Inc(i);                           // Skip '$'
        while (i<Length(aStr)) and (aStr[i] in ['0'..'9']) do
          Inc(i);                         // Get the number after '$'
        xLen:=i-xStart;
        if xLen<2 then
          raise EDelphiConverterError.Create('"$" should be followed by a number.');
        xNum:=StrToInt(copy(aStr, xStart+1, xLen-1)); // Leave out '$', convert number.
        if xNum < 1 then
          raise EDelphiConverterError.Create(
                           'Replacement function parameter number should be >= 1.');
        ReplacementParams.Add(TReplacementParam.Create(xNum, xLen, xStart));
      end;
    end;
    if aStr[i]<>')' then
      raise EDelphiConverterError.Create('")" is missing from replacement function.');
    Result:=i+1;
  end;

  function InsertParams2Replacement(FuncInfo: TFuncReplacement): string;
  // Construct a new funcion call, inserting original parameters to replacement str.
  //  FuncInfo - Replacement string + parameters from the original function call.
  var
    RP: TReplacementParam;
    ss, se: String;
    i: integer;
  begin
    Result:=FuncInfo.ReplFunc;
    for i:=ReplacementParams.Count-1 downto 0 do begin
      RP:=TReplacementParam(ReplacementParams[i]);
      if RP.ParamNum<=FuncInfo.Params.Count then begin
        ss:=copy(Result, 1, RP.StrPosition-1);        // String before the param
        se:=copy(Result, RP.StrPosition+RP.ParamLen, MaxInt); // and after it.
        Result:=ss+FuncInfo.Params[RP.ParamNum-1]+se;
      end;
    end;
  end;

  function GetComment(const aStr: string; aPossibleStartPos: integer): string;
  // Extract and return a possible comment.
  var
    CommChBeg, CommBeg, CommEnd, i: Integer;   // Start and end of comment.
  begin
    Result:='';
    CommEnd:=Length(aStr);
    CommChBeg:=PosEx('//', aStr, aPossibleStartPos);
    if CommChBeg<>0 then
      CommBeg:=CommChBeg+2
    else begin
      CommChBeg:=PosEx('{', aStr, aPossibleStartPos);
      if CommChBeg<>0 then begin
        CommBeg:=CommChBeg+1;
        i:=PosEx('}', aStr, CommBeg);
        if i<>0 then
          CommEnd:=i-1;
      end;
    end;
    if CommChBeg<>0 then
      Result:=Trim(Copy(aStr, CommBeg, CommEnd-CommBeg+1));
  end;

var
  FuncInfo: TFuncReplacement;
  PossibleCommentPos: Integer;               // Start looking for comments here.
  i: Integer;
  s, NewFunc, Comment: String;
begin
  Result:=false;
  ReplacementParams:=TObjectList.Create;
  try
    // Replace from bottom to top.
    for i:=fFuncsToReplace.Count-1 downto 0 do begin
      FuncInfo:=TFuncReplacement(fFuncsToReplace[i]);
      // Update ReplacementParams.
      ReplacementParams.Clear;
      PossibleCommentPos:=ParseReplacementParams(FuncInfo.ReplFunc);
      // Replace only if the params match somehow, so eg. a variable is not replaced.
      if (FuncInfo.Params.Count>0) or (ReplacementParams.Count=0) then begin
        NewFunc:=InsertParams2Replacement(FuncInfo);
        Comment:=GetComment(FuncInfo.ReplFunc, PossibleCommentPos);
        // Separate function body
        NewFunc:=Format('%s%s { *Converted from %s* %s }',
          [NewFunc, FuncInfo.InclSemiColon, FuncInfo.FuncName, Comment]);
        // Old function call with params for IDE message output.
        s:=copy(fCTLink.CodeTool.Src, FuncInfo.StartPos, FuncInfo.EndPos-FuncInfo.StartPos);
        s:=StringReplace(s, sLineBreak, '', [rfReplaceAll]);
        // Now replace it.
        fCTLink.ResetMainScanner;
        if not fCTLink.SrcCache.Replace(gtNone, gtNone,
                            FuncInfo.StartPos, FuncInfo.EndPos, NewFunc) then exit;
        IDEMessagesWindow.AddMsg('Replaced call '+s, '', -1);
        IDEMessagesWindow.AddMsg('                  with '+NewFunc, '', -1);
        // Add the required unit name to uses section if needed.
        if Assigned(AddUnitEvent) and (FuncInfo.UnitName<>'') then
          AddUnitEvent(FuncInfo.UnitName);
      end;
    end;
  finally
    ReplacementParams.Free;
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.RememberProcDefinition(aNode: TCodeTreeNode): TCodeTreeNode;
// This is called when Node.Desc=ctnProcedureHead.
// Save the defined proc name so it is not replaced later.
var
  ProcName: string;
begin
  with fCTLink.CodeTool do begin
    MoveCursorToCleanPos(aNode.StartPos);
    ReadNextAtom;               // Read proc name.
    ProcName:=GetAtom;
    ReadNextAtom;
    if GetAtom<>'.' then        // Don't save a method name (like TClass.Method).
      fDefinedProcNames.Add(ProcName);
  end;
  Result:=aNode.Next;
end;

function TConvDelphiCodeTool.ReplaceFuncCalls(aIsConsoleApp: boolean): boolean;
// Copied and modified from TFindDeclarationTool.FindReferences.
// Search for calls to functions / procedures in a list from current unit's
// implementation section. Replace found calls with a given replacement.
var
  xStart: Integer;

  procedure CheckSemiColon(FuncInfo: TFuncReplacement);
  begin
    with fCTLink.CodeTool do
      if AtomIsChar(';') then begin
        FuncInfo.EndPos:=CurPos.EndPos;
        FuncInfo.InclSemiColon:=';';
      end;
  end;

  procedure ReadParams(FuncInfo: TFuncReplacement);
  var
    ExprStartPos, ExprEndPos, BracketCount: integer;
  begin
    FuncInfo.InclSemiColon:='';
    FuncInfo.StartPos:=xStart;
    with fCTLink.CodeTool do begin
      MoveCursorToCleanPos(xStart);
      ReadNextAtom;                     // Read func name.
      ReadNextAtom;                     // Read first atom after proc name.
      if AtomIsChar('(') then begin
        // read parameter list
        ReadNextAtom;
        if not AtomIsChar(')') then begin
          // read all expressions
          BracketCount:=0;
          while true do begin
            ExprStartPos:=CurPos.StartPos;
            // read til comma or bracket close
            repeat
              ReadNextAtom;
              if (CurPos.StartPos>SrcLen) or (CurPos.Flag=cafComma) then
                break;
              if CurPos.Flag=cafRoundBracketOpen then
                Inc(BracketCount)
              else if CurPos.Flag=cafRoundBracketClose then begin
                if BracketCount=0 then
                  break;
                Dec(BracketCount);
              end;
            until false;
            ExprEndPos:=CurPos.StartPos;
            // Add parameter to list
            FuncInfo.Params.Add(copy(Src,ExprStartPos,ExprEndPos-ExprStartPos));
            MoveCursorToCleanPos(ExprEndPos);
            ReadNextAtom;
            if AtomIsChar(')') then begin
              FuncInfo.EndPos:=CurPos.EndPos;
              ReadNextAtom;
              CheckSemiColon(FuncInfo);
              break;
            end;
            if not AtomIsChar(',') then
              raise EDelphiConverterError.Create('Bracket not found');
            ReadNextAtom;
          end;
        end;
      end
      else begin
        FuncInfo.EndPos:=CurPos.StartPos;
        CheckSemiColon(FuncInfo);
      end;
    end;
    FuncInfo.UpdateReplacement;
  end;

  procedure ReadFuncCall(MaxPos: Integer);
  var
    FuncDefInfo, FuncCallInfo: TFuncReplacement;
    FuncName: string;
    i, x, IdentEndPos: Integer;
  begin
    IdentEndPos:=xStart;
    with fCTLink.CodeTool, fCTLink.Settings do begin
      while (IdentEndPos<=MaxPos) and (IsIdentChar[Src[IdentEndPos]]) do
        inc(IdentEndPos);
      for i:=0 to ReplaceFuncs.Funcs.Count-1 do begin
        FuncName:=ReplaceFuncs.Funcs[i];
        if (IdentEndPos-xStart=length(FuncName))
        and (CompareIdentifiers(PChar(Pointer(FuncName)),@Src[xStart])=0)
        and not fDefinedProcNames.Find(FuncName, x)
        then begin
          FuncDefInfo:=ReplaceFuncs.FuncAtInd(i);
          if ReplaceFuncs.Categories.Find(FuncDefInfo.Category, x)
          and not (aIsConsoleApp and (FuncDefInfo.Category='UTF8Names'))
          then begin
            // Create a new replacement object for params, position and other info.
            FuncCallInfo:=TFuncReplacement.Create(FuncDefInfo);
            ReadParams(FuncCallInfo);
            IdentEndPos:=FuncCallInfo.EndPos; // Skip the params, too, for next search.
            fFuncsToReplace.Add(FuncCallInfo);
            Break;
          end;
        end;
      end;
    end;
    xStart:=IdentEndPos;
  end;

  function SearchFuncCalls(aNode: TCodeTreeNode): TCodeTreeNode;
  var
    CommentLvl: Integer;
    InStrConst: Boolean;
  begin
    xStart:=aNode.StartPos;
    with fCTLink.CodeTool do
    while xStart<=aNode.EndPos do begin
      case Src[xStart] of

      '{':                         // pascal comment
        begin
          inc(xStart);
          CommentLvl:=1;
          InStrConst:=false;
          while xStart<=aNode.EndPos do begin
            case Src[xStart] of
            '{': if Scanner.NestedComments then inc(CommentLvl);
            '}':
              begin
                dec(CommentLvl);
                if CommentLvl=0 then break;
              end;
            '''':
              InStrConst:=not InStrConst;
            end;
            inc(xStart);
          end;
          inc(xStart);
        end;

      '/':                         // Delphi comment
        if (Src[xStart+1]<>'/') then begin
          inc(xStart);
        end else begin
          inc(xStart,2);
          InStrConst:=false;
          while (xStart<=aNode.EndPos) do begin
            case Src[xStart] of
            #10,#13:
              break;
            '''':
              InStrConst:=not InStrConst;
            end;
            inc(xStart);
          end;
          inc(xStart);
          if (xStart<=aNode.EndPos) and (Src[xStart] in [#10,#13])
          and (Src[xStart-1]<>Src[xStart]) then
            inc(xStart);
        end;

      '(':                         // turbo pascal comment
        if (Src[xStart+1]<>'*') then begin
          inc(xStart);
        end else begin
          inc(xStart,3);
          InStrConst:=false;
          while (xStart<=aNode.EndPos) do begin
            case Src[xStart] of
            ')':
              if Src[xStart-1]='*' then break;
            '''':
              InStrConst:=not InStrConst;
            end;
            inc(xStart);
          end;
          inc(xStart);
        end;

      'a'..'z','A'..'Z','_':
        ReadFuncCall(aNode.EndPos);

      '''':
        begin                      // skip string constant
          inc(xStart);
          while (xStart<=aNode.EndPos) do begin
            if (not (Src[xStart] in ['''',#10,#13])) then
              inc(xStart)
            else begin
              inc(xStart);
              break;
            end;
          end;
        end;

      else
        inc(xStart);
      end;
    end;
    Result:=aNode.NextSkipChilds;
  end;

var
  Node: TCodeTreeNode;
begin
  Result:=false;
  with fCTLink.CodeTool do begin
    fFuncsToReplace:=TObjectList.Create;
    fDefinedProcNames:=TStringList.Create;
    fDefinedProcNames.Sorted:=True;
    fDefinedProcNames.Duplicates:=dupIgnore;
    ActivateGlobalWriteLock;
    try
      BuildTree(false);
      // Only convert identifiers in ctnBeginBlock nodes
      Node:=fCTLink.CodeTool.Tree.Root;
      while Node<>nil do begin
        if Node.Desc=ctnBeginBlock then
          Node:=SearchFuncCalls(Node)
        else if Node.Desc=ctnProcedureHead then
          Node:=RememberProcDefinition(Node)
        else
          Node:=Node.Next;
      end;
      if not ReplaceFuncsInSource then Exit;
    finally
      DeactivateGlobalWriteLock;
      fDefinedProcNames.Free;
      fFuncsToReplace.Free;
    end;
  end;
  Result:=true;
end;  // ReplaceFuncCalls

end.

