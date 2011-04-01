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
    function CheckTopOffsets(LFMBuf: TCodeBuffer; LFMTree: TLFMTree;
               VisOffsets: TVisualOffsets; ValueNodes: TObjectList): boolean;
  public
    property HasFormFile: boolean read fHasFormFile write fHasFormFile;
    property LowerCaseRes: boolean read fLowerCaseRes write fLowerCaseRes;
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
//      if not fSrcCache.Apply then exit;
    finally
      fCTLink.SrcCache.EndUpdate;
    end;
//    if not fCTLink.SrcCache.Apply then exit;
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
      if fCTLink.Settings.Target in [ctLazarusDelphi, ctLazarusDelphiSameDfm] then
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
          if fCTLink.Settings.Target in [ctLazarusDelphi, ctLazarusDelphiSameDfm] then begin
            // Use the same dfm file. Lowercase existing key.
            if (fCTLink.Settings.Target=ctLazarusDelphiSameDfm) and (Key<>LowKey) then
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
  if (fCTLink.Settings.Target=ctLazarusDelphi) and (fDfmDirectiveStart<>-1)
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
    BuildSubTreeForClass(ANode);
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

procedure SplitParam(const aStr: string; aDelimiter: Char; ResultList: TStringList);
// A modified split function. Removes '$' in front of every token.

  procedure SetItem(Start, Len: integer); // Add the item.
  begin
    while (aStr[Start]=' ') do begin      // Trim leading space.
      Inc(Start);
      Dec(Len);
    end;
    while (aStr[Start+Len-1]=' ') do      // Trim trailing space.
      Dec(Len);
    if (aStr[Start]='$') then begin       // Parameters must begin with '$'.
      Inc(Start);
      Dec(Len);
    end
    else
      raise EDelphiConverterError.Create('Replacement function parameter should start with "$".');
    ResultList.Add(Copy(aStr, Start, Len));
  end;

var
  i, Start, EndPlus1: Integer;
begin
  ResultList.Clear;
  Start:=1;
  repeat
    i:=Start;
    while (i<Length(aStr)) and (aStr[i]<>aDelimiter) do
      Inc(i);                             // Next delimiter.
    EndPlus1:=i;
    if i<Length(aStr) then
    begin
      SetItem(Start, EndPlus1-Start);
      Start:=i+1;                         // Start of next item.
    end
    else begin
      EndPlus1:=i+1;
      if EndPlus1>=Start then
        SetItem(Start, EndPlus1-Start);   // Copy the rest to last item.
      Break;                              // Out of the loop.
    end;
  until False;
end;

function TConvDelphiCodeTool.ReplaceFuncsInSource: boolean;
// Replace the function names and parameters in source.
var
  // Replacement parameter positions, will be converted to integers.
  ParamList: TStringList;
  BodyEnd: Integer;                     // End of function body.

  function ParseReplacementParams(const aStr: string): integer;
  // Parse replacement params. They show which original params are copied where.
  // Returns the first position where comments can be searched from.
  var
    ParamBeg, ParamEnd: Integer;        // Start and end of parameters.
    s: String;
  begin
    Result:=1;
    ParamBeg:=Pos('(', aStr);
    if ParamBeg>0 then begin
      ParamEnd:=PosEx(')', aStr, ParamBeg+1);
      if ParamEnd=0 then
        raise EDelphiConverterError.Create('")" is missing from replacement function.');
      s:=Copy(aStr, ParamBeg+1, ParamEnd-ParamBeg-1);
      SplitParam(s, ',', ParamList);    // The actual parameter list.
      BodyEnd:=ParamBeg-1;
      Result:=ParamEnd+1;
    end;
  end;

  function CollectParams(aParams: TStringList): string;
  // Collect parameters from original call. Construct and return a new parameter list.
  //  aParams - parameters from the original function call.
  var
    Param: String;
    ParamPos: Integer;             // Position of parameter in the original call.
    i: Integer;
  begin
    Result:='';
    for i:=0 to ParamList.Count-1 do begin
      ParamPos:=StrToInt(ParamList[i]);
      if ParamPos < 1 then
        raise EDelphiConverterError.Create('Replacement function parameter number should be >= 1.');
      Param:='nil';                // Default value if not found from original code.
      if ParamPos<=aParams.Count then
        Param:=aParams[ParamPos-1];
      if Result<>'' then
        Result:=Result+', ';
      Result:=Result+Param;
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
    if CommChBeg<>0 then begin
      if BodyEnd=-1 then
        BodyEnd:=CommChBeg-1;
      Result:=Trim(Copy(aStr, CommBeg, CommEnd-CommBeg+1));
    end;
  end;

var
  FuncInfo: TFuncReplacement;
  PossibleCommentPos: Integer;               // Start looking for comments here.
  i: Integer;
  s, NewFunc, NewParamStr, Comment: String;
begin
  Result:=false;
  ParamList:=TStringList.Create;
  try
    // Replace from bottom to top.
    for i:=fFuncsToReplace.Count-1 downto 0 do begin
      FuncInfo:=TFuncReplacement(fFuncsToReplace[i]);
      BodyEnd:=-1;
      // Update ParamList.
      PossibleCommentPos:=ParseReplacementParams(FuncInfo.ReplFunc);
      // Replace only if the params match somehow, so eg. a variable is not replaced.
      if (FuncInfo.Params.Count>0) or (ParamList.Count=0) then begin
        NewParamStr:=CollectParams(FuncInfo.Params);
        Comment:=GetComment(FuncInfo.ReplFunc, PossibleCommentPos);
        // Separate function body
        if BodyEnd=-1 then
          BodyEnd:=Length(FuncInfo.ReplFunc);
        NewFunc:=Trim(Copy(FuncInfo.ReplFunc, 1, BodyEnd));
        NewFunc:=Format('%s(%s)%s { *Converted from %s* %s }',
          [NewFunc, NewParamStr, FuncInfo.InclSemiColon, FuncInfo.FuncName, Comment]);
        // Old function call with params for IDE message output.
        s:=copy(fCTLink.CodeTool.Src, FuncInfo.StartPos, FuncInfo.EndPos-FuncInfo.StartPos);
        s:=StringReplace(s, sLineBreak, '', [rfReplaceAll]);
        // Now replace it.
        fCTLink.ResetMainScanner;
        if not fCTLink.SrcCache.Replace(gtNone, gtNone,
                            FuncInfo.StartPos, FuncInfo.EndPos, NewFunc) then exit;
        IDEMessagesWindow.AddMsg('Replaced call '+s, '', -1);
        IDEMessagesWindow.AddMsg('                  with '+NewFunc, '', -1);
      end;
    end;
  finally
    ParamList.Free;
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
              if (CurPos.Flag=cafRoundBracketOpen) then
                Inc(BracketCount)
              else if (CurPos.Flag=cafRoundBracketClose) then begin
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
        and (CompareIdentifiers(PChar(FuncName),@Src[xStart])=0)
        and not fDefinedProcNames.Find(FuncName, x)
        then begin
          FuncDefInfo:=ReplaceFuncs.FuncAtInd(i);
          if ReplaceFuncs.Categories.Find(FuncDefInfo.Category, x)
          // Categories.Objects[x] is used as a boolean flag.
          and Assigned(ReplaceFuncs.Categories.Objects[x])
          // UTF8 funcs are in LCL which console apps don't have -> don't change.
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

function TConvDelphiCodeTool.CheckTopOffsets(LFMBuf: TCodeBuffer; LFMTree: TLFMTree;
                     VisOffsets: TVisualOffsets; ValueNodes: TObjectList): boolean;
// Collect a list of coord attributes for components that are inside
//  a visual container component. An offset will be added to those attributes.
// Parameters: VisOffsets has names of parent container types.
//   ValueNodes - the found coord attributes are added here as TSrcPropOffset objects.
// Based on function CheckLFM.
var
  RootContext: TFindContext;

  function CheckLFMObjectValues(LFMObject: TLFMObjectNode;
    const ClassContext: TFindContext; GrandClassName: string): boolean; forward;

  function FindLFMIdentifier(LFMNode: TLFMTreeNode; const IdentName: string;
    const ClassContext: TFindContext; out IdentContext: TFindContext): boolean;
  var
    Params: TFindDeclarationParams;
    IsPublished: Boolean;
  begin
    Result:=false;
    IdentContext:=CleanFindContext;
    IsPublished:=false;
    if (ClassContext.Node=nil) or (not (ClassContext.Node.Desc in AllClasses)) then
      exit;
    Params:=TFindDeclarationParams.Create;
    try
      Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound,
                     fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
                     fdfIgnoreOverloadedProcs];
      Params.ContextNode:=ClassContext.Node;
      Params.SetIdentifier(ClassContext.Tool,PChar(Pointer(IdentName)),nil);
      try
        if ClassContext.Tool.FindIdentifierInContext(Params) then begin
          Result:=true;
          repeat
            IdentContext:=CreateFindContext(Params);
            if (not IsPublished)
            and (IdentContext.Node.HasParentOfType(ctnClassPublished)) then
              IsPublished:=true;
            if (IdentContext.Node<>nil)
            and (IdentContext.Node.Desc=ctnProperty)
            and (IdentContext.Tool.PropNodeIsTypeLess(IdentContext.Node)) then
            begin
              // this is a typeless property -> search further
              Params.Clear;
              Params.Flags:=[fdfSearchInAncestors, fdfIgnoreMissingParams,
                             fdfIgnoreCurContextNode, fdfIgnoreOverloadedProcs];
              Params.ContextNode:=IdentContext.Node.Parent;
              while (Params.ContextNode<>nil)
              and (not (Params.ContextNode.Desc in AllClasses)) do
                Params.ContextNode:=Params.ContextNode.Parent;
              if Params.ContextNode<>nil then begin
                Params.SetIdentifier(ClassContext.Tool,PChar(Pointer(IdentName)),nil);
                if not IdentContext.Tool.FindIdentifierInContext(Params) then
                  break;
              end;
            end else
              break;
          until false;
        end;
      except
        on E: ECodeToolError do ;        // ignore search/parse errors
      end;
    finally
      Params.Free;
    end;
  end;

  function FindClassNodeForLFMObject(LFMNode: TLFMTreeNode;
    StartTool: TFindDeclarationTool; DefinitionNode: TCodeTreeNode): TFindContext;
  var
    Params: TFindDeclarationParams;
    Identifier: PChar;
    OldInput: TFindDeclarationInput;
  begin
    Result:=CleanFindContext;
    if (DefinitionNode.Desc=ctnIdentifier) then
      Identifier:=@StartTool.Src[DefinitionNode.StartPos]
    else if DefinitionNode.Desc=ctnProperty then
      Identifier:=StartTool.GetPropertyTypeIdentifier(DefinitionNode)
    else
      Identifier:=nil;
    if Identifier=nil then exit;
    Params:=TFindDeclarationParams.Create;
    try
      Params.Flags:=[fdfSearchInAncestors, fdfExceptionOnNotFound,
                     fdfSearchInParentNodes, fdfExceptionOnPredefinedIdent,
                     fdfIgnoreMissingParams, fdfIgnoreOverloadedProcs];
      Params.ContextNode:=DefinitionNode;
      Params.SetIdentifier(StartTool,Identifier,nil);
      try
        Params.Save(OldInput);
        if StartTool.FindIdentifierInContext(Params) then begin
          Params.Load(OldInput,true);
          Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
          if (Result.Node=nil)
          or (not (Result.Node.Desc in AllClasses)) then
            Result:=CleanFindContext;
        end;
      except
        on E: ECodeToolError do ;        // ignore search/parse errors
      end;
    finally
      Params.Free;
    end;
  end;

  function FindClassContext(const ClassName: string): TFindContext;
  var
    Params: TFindDeclarationParams;
    Identifier: PChar;
    OldInput: TFindDeclarationInput;
    StartTool: TStandardCodeTool;
  begin
    Result:=CleanFindContext;
    Params:=TFindDeclarationParams.Create;
    StartTool:=fCTLink.CodeTool;
    Identifier:=PChar(Pointer(ClassName));
    try
      Params.Flags:=[fdfExceptionOnNotFound, fdfSearchInParentNodes,
                     fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
                     fdfIgnoreOverloadedProcs];
      with fCTLink.CodeTool do begin
        Params.ContextNode:=FindInterfaceNode;
        if Params.ContextNode=nil then
          Params.ContextNode:=FindMainUsesSection;
        Params.SetIdentifier(StartTool,Identifier,nil);
        try
          Params.Save(OldInput);
          if FindIdentifierInContext(Params) then begin
            Params.Load(OldInput,true);
            Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
            if (Result.Node=nil)
            or (not (Result.Node.Desc in AllClasses)) then
              Result:=CleanFindContext;
          end;
        except
          on E: ECodeToolError do ;          // ignore search/parse errors
        end;
      end;
    finally
      Params.Free;
    end;
  end;

  procedure CheckLFMChildObject(LFMObject: TLFMObjectNode; const ParentName: string);
  var
    VarTypeName: String;
    ChildContext: TFindContext;
    ClassContext: TFindContext;
    DefinitionNode: TCodeTreeNode;
  begin
    // find variable for object
    if LFMObject.Name='' then exit;
    if FindLFMIdentifier(LFMObject, LFMObject.Name, RootContext, ChildContext) then begin
      if ChildContext.Node=nil then exit;
      // check if identifier is a variable or property
      VarTypeName:='';
      if (ChildContext.Node.Desc=ctnVarDefinition) then begin
        DefinitionNode:=ChildContext.Tool.FindTypeNodeOfDefinition(ChildContext.Node);
        if DefinitionNode=nil then exit;
        VarTypeName:=ChildContext.Tool.ExtractDefinitionNodeType(ChildContext.Node);
      end else if (ChildContext.Node.Desc=ctnProperty) then begin
        DefinitionNode:=ChildContext.Node;
        VarTypeName:=ChildContext.Tool.ExtractPropType(ChildContext.Node,false,false);
      end else
        exit;
      // check if variable/property has a compatible type
      if (VarTypeName<>'') and (LFMObject.TypeName<>'')
          and (CompareIdentifiers(PChar(VarTypeName),
                                  PChar(LFMObject.TypeName))<>0) then  exit;
      // find class node
      ClassContext:=FindClassNodeForLFMObject(LFMObject, ChildContext.Tool, DefinitionNode);
    end else
      ClassContext:=FindClassContext(LFMObject.TypeName);  // try the object type
    // check child LFM nodes
    // ClassContext.Node=nil when the parent class is not found in source.
    if ClassContext.Node<>nil then
      CheckLFMObjectValues(LFMObject, ClassContext, ParentName);
  end;

  function FindClassNodeForPropertyType(LFMProperty: TLFMPropertyNode;
    const PropertyContext: TFindContext): TFindContext;
  var
    Params: TFindDeclarationParams;
  begin
    Result:=CleanFindContext;
    Params:=TFindDeclarationParams.Create;
    try
      Params.Flags:=[fdfSearchInAncestors,  fdfExceptionOnNotFound,
                     fdfSearchInParentNodes,fdfExceptionOnPredefinedIdent,
                     fdfIgnoreMissingParams,fdfIgnoreOverloadedProcs];
      Params.ContextNode:=PropertyContext.Node;
      Params.SetIdentifier(PropertyContext.Tool,nil,nil);
      try
        Result:=PropertyContext.Tool.FindBaseTypeOfNode(Params, PropertyContext.Node);
      except
        on E: ECodeToolError do ;              // ignore search/parse errors
      end;
    finally
      Params.Free;
    end;
  end;

  procedure CheckLFMProperty(LFMProperty: TLFMPropertyNode; const ParentContext: TFindContext;
    const GrandClassName, ParentClassName: string);
  // Check properties. Stores info about Top and Left properties for later adjustment.
  // Parameters: LFMProperty is the property node
  //   ParentContext is the context, where properties are searched (class or property).
  //   GrandClassName and ParentClassName are the class type names.
  var
    i, ind: Integer;
    ValNode: TLFMValueNode;
    CurName, Prop: string;
    CurPropContext: TFindContext;
    SearchContext: TFindContext;
  begin
    // find complete property name
    Prop:=LFMProperty.CompleteName;
    if Prop='' then exit;
    if (Prop='Top') or (Prop='Left') then begin
      if (GrandClassName<>'') and VisOffsets.Find(GrandClassName, ind) then begin
        if LFMProperty.FirstChild is TLFMValueNode then begin
          ValNode:=LFMProperty.FirstChild as TLFMValueNode;
          ValueNodes.Add(TSrcPropOffset.Create(GrandClassName,ParentClassName,
                                               Prop,ValNode.StartPos));
        end;
      end;
    end;
    // find every part of the property name
    SearchContext:=ParentContext;
    for i:=0 to LFMProperty.NameParts.Count-1 do begin
      if SearchContext.Node.Desc=ctnProperty then begin
        // get the type of the property and search the class node
        SearchContext:=FindClassNodeForPropertyType(LFMProperty, SearchContext);
        if SearchContext.Node=nil then exit;
      end;
      CurName:=LFMProperty.NameParts.Names[i];
      if not FindLFMIdentifier(LFMProperty, CurName, SearchContext, CurPropContext) then
        break;
      if CurPropContext.Node=nil then break;
      SearchContext:=CurPropContext;
    end;
  end;

  function CheckLFMObjectValues(LFMObject: TLFMObjectNode;
    const ClassContext: TFindContext; GrandClassName: string): boolean;
  var
    CurLFMNode: TLFMTreeNode;
    ParentName: string;
  begin
    ParentName:=ClassContext.Tool.ExtractClassName(ClassContext.Node, False);
    CurLFMNode:=LFMObject.FirstChild;
    while CurLFMNode<>nil do begin
      case CurLFMNode.TheType of
      lfmnObject:
        CheckLFMChildObject(TLFMObjectNode(CurLFMNode), ParentName);
      lfmnProperty:
        CheckLFMProperty(TLFMPropertyNode(CurLFMNode), ClassContext,
                         GrandClassName, ParentName);
      end;
      CurLFMNode:=CurLFMNode.NextSibling;
    end;
    Result:=true;
  end;

  function CheckLFMRoot(RootLFMNode: TLFMTreeNode): boolean;
  var
    LookupRootLFMNode: TLFMObjectNode;
    LookupRootTypeName: String;
    RootClassNode: TCodeTreeNode;
  begin
    Result:=false;
    // get root object node
    if (RootLFMNode=nil) or (not (RootLFMNode is TLFMObjectNode)) then exit;
    LookupRootLFMNode:=TLFMObjectNode(RootLFMNode);

    // get type name of root object
    LookupRootTypeName:=UpperCaseStr(LookupRootLFMNode.TypeName);
    if LookupRootTypeName='' then exit;

    // find root type
    RootClassNode:=fCTLink.CodeTool.FindClassNodeInUnit(LookupRootTypeName,
                                                        true,false,false,false);
    RootContext:=CleanFindContext;
    RootContext.Node:=RootClassNode;
    RootContext.Tool:=fCTLink.CodeTool;
    if RootClassNode=nil then exit;
    Result:=CheckLFMObjectValues(LookupRootLFMNode, RootContext, '');
  end;

var
  CurRootLFMNode: TLFMTreeNode;
begin
  Result:=false;
  // create tree from LFM file
  LFMTree:=DefaultLFMTrees.GetLFMTree(LFMBuf,true);
  fCTLink.CodeTool.ActivateGlobalWriteLock;
  try
    if not LFMTree.ParseIfNeeded then exit;
    // parse unit and find LookupRoot
    fCTLink.CodeTool.BuildTree(true);
    // find every identifier
    CurRootLFMNode:=LFMTree.Root;
    while CurRootLFMNode<>nil do begin
      if not CheckLFMRoot(CurRootLFMNode) then exit;
      CurRootLFMNode:=CurRootLFMNode.NextSibling;
    end;
  finally
    fCTLink.CodeTool.DeactivateGlobalWriteLock;
  end;
  Result:=LFMTree.FirstError=nil;
end;  // CheckTopOffsets


end.

