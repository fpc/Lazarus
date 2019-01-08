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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  // RTL + FCL
  Classes, SysUtils, contnrs, strutils,
  // LCL
  Forms, Controls,
  // CodeTools
  CodeToolManager, CodeTree, CodeAtom, FileProcs, KeywordFuncLists, BasicCodeTools,
  LinkScanner, CodeCache, SourceChanger,
  // LazUtils
  LazFileUtils, AvgLvlTree,
  // IdeIntf
  IDEExternToolIntf,
  // IDE
  LazarusIDEStrConsts,
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
    fAskAboutError: boolean;
    fSettings: TConvertSettings;          // Conversion settings.
    procedure InitCodeTool;
  public
    constructor Create(ACode: TCodeBuffer);
    destructor Destroy; override;
    procedure ResetMainScanner;
    function DummyReplacements: Boolean;
  public
    property CodeTool: TCodeTool read fCodeTool;
    property Code: TCodeBuffer read fCode;
    property SrcCache: TSourceChangeCache read fSrcCache;
    property AskAboutError: boolean read fAskAboutError write fAskAboutError;
    property Settings: TConvertSettings read fSettings write fSettings;
  end;

  { TConvDelphiCodeTool }

  TConvDelphiCodeTool = class
  private
    fCTLink: TCodeToolLink;
    fCTLinkCreated: boolean;
    fIsConsoleApp: boolean;
    fHasFormFile: boolean;
    fResAction: TResAction;
    fAddUnitEvent: TAddUnitEvent;
    // Delphi Function names to replace with FCL/LCL functions.
    fDefinedProcNames: TStringList;
    // List of TFuncReplacement.
    fFuncsToReplace: TObjectList;

    function AddModeDelphiDirective: boolean;
    function ReplaceFuncsInSource: boolean;
    function RememberProcDefinition(aNode: TCodeTreeNode): TCodeTreeNode;
    function ReplaceFuncCalls(aIsConsoleApp: boolean): boolean;
  public
    constructor Create(APascalBuffer: TCodeBuffer);
    constructor Create(ACTLink: TCodeToolLink);
    destructor Destroy; override;
    function Convert: TModalResult;
    function FindApptypeConsole: boolean;
    function RenameUnitIfNeeded: boolean;
    function RenameResourceDirectives: boolean;
    function FixMainClassAncestor(const AClassName: string;
                                  AReplaceTypes: TStringToStringTree): boolean;
  public
    property IsConsoleApp: boolean read fIsConsoleApp write fIsConsoleApp;
    property HasFormFile: boolean read fHasFormFile write fHasFormFile;
    property ResAction: TResAction read fResAction write fResAction;
    property AddUnitEvent: TAddUnitEvent read fAddUnitEvent write fAddUnitEvent;
  end;


implementation

{ TCodeToolLink }

constructor TCodeToolLink.Create(ACode: TCodeBuffer);
begin
  inherited Create;
  fCode:=ACode;
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
  fCodeTool:=nil;
  fSrcCache:=nil;
  if not CodeToolBoss.InitCurCodeTool(fCode) then exit;
  fCodeTool:=CodeToolBoss.CurCodeTool;
  fSrcCache:=CodeToolBoss.SourceChangeCache;
  ResetMainScanner;
  fCodeTool.Scanner.IgnoreMissingIncludeFiles:=True;
end;

procedure TCodeToolLink.ResetMainScanner;
begin
  fSrcCache.MainScanner:=fCodeTool.Scanner;
end;

function TCodeToolLink.DummyReplacements: Boolean;
// If Codetools cannot parse the code, do dummy replacement for all reserved words:
//  '.'+ReservedWord -> '.&'+ReservedWord, needed for OleVariant.
// Most Codetools functions cannot be used because the code is invalid,
//  but TSourceChangeCache.ReplaceEx works.
var
  p, AStart: Integer;
  Src: string;
  LastWasPoint: Boolean;
begin
  p:=1;
  LastWasPoint:=false;
  Src:=fCode.Source;
  repeat
    ReadRawNextPascalAtom(Src,p,AStart,false);
    if p>length(Src) then break;
    // Reserved words are in WordIsKeyWord list in CodeTools.
    if LastWasPoint and WordIsKeyWord.DoIdentifier(@Src[AStart]) then
    begin
      // '.'+ReservedWord was found
      if not fSrcCache.ReplaceEx(gtNone,gtNone,1,1,fCode,AStart,AStart,'&') then
        Exit(False);
    end;
    LastWasPoint:=Src[AStart]='.';
  until false;
  // Apply the changes in buffer
  if not fSrcCache.Apply then
    Exit(False);
  Result:=True;
end;

{ TConvDelphiCodeTool }

constructor TConvDelphiCodeTool.Create(APascalBuffer: TCodeBuffer);
begin
  debugln(['TConvDelphiCodeTool.Create ',DbgSName(APascalBuffer)]);
  debugln(['TConvDelphiCodeTool.Create ',APascalBuffer.Filename]);
  inherited Create;
  fCTLink:=TCodeToolLink.Create(APascalBuffer);
  fCTLink.AskAboutError:=False;
  fResAction:=raLowerCase;
  fIsConsoleApp:=False;
  fCTLinkCreated:=True;
  if Assigned(fCTLink.CodeTool) then
    fCTLink.CodeTool.BuildTree(lsrInitializationStart);
end;

constructor TConvDelphiCodeTool.Create(ACTLink: TCodeToolLink);
begin
  inherited Create;
  fCTLink:=ACTLink;
  fResAction:=raNone;
  fIsConsoleApp:=False;
  fCTLinkCreated:=False;
end;

destructor TConvDelphiCodeTool.Destroy;
begin
  if fCTLinkCreated and (fCTLink.SrcCache<>nil) and (fCTLink.CodeTool<>nil)
  and (fCTLink.SrcCache.MainScanner=fCTLink.CodeTool.Scanner) then begin
    fCTLink.SrcCache.Apply;
    FreeAndNil(fCTLink);
  end;
  inherited Destroy;
end;

function TConvDelphiCodeTool.Convert: TModalResult;
// Add {$mode delphi} directive
// Remove {$R *.dfm} or {$R *.xfm} directive
// Change {$R *.RES} to {$R *.res} if needed
// TODO: fix delphi ambiguouties like incomplete proc implementation headers
begin
  Result:=mrCancel;
  if fCTLink.CodeTool=nil then exit;
  fCTLink.SrcCache.BeginUpdate;
  try
    // these changes can be applied together without rescan
    if not RenameUnitIfNeeded then exit;
    if not AddModeDelphiDirective then exit;
    if not RenameResourceDirectives then exit;
    if fCTLink.Settings.FuncReplaceMode=rsEnabled then
      if not ReplaceFuncCalls(fIsConsoleApp) then exit;
  finally
    fCTLink.SrcCache.EndUpdate;
    fCTLink.SrcCache.Apply;
  end;
  Result:=mrOK;
end;

function TConvDelphiCodeTool.FindApptypeConsole: boolean;
// Return true if there is {$APPTYPE CONSOLE} directive.
var
  ParamPos, ACleanPos: Integer;
begin
  Result:=false;
  if fCTLink.CodeTool=nil then exit;
  ACleanPos:=0;
  with fCTLink.CodeTool do begin
    if Scanner=nil then exit;
    BuildTree(lsrImplementationStart);
    ACleanPos:=FindNextCompilerDirectiveWithName(Src, 1, 'Apptype',
                                                 Scanner.NestedComments, ParamPos);
    if (ACleanPos>0) and (ACleanPos<=SrcLen) and (ParamPos>0) then
      Result:=LowerCase(copy(Src,ParamPos,7))='console';
  end;
end;

function TConvDelphiCodeTool.RenameUnitIfNeeded: boolean;
// Change the unit name to match the disk name unless the disk name is all lowercase.
var
  NamePos: TAtomPosition;
  CaretPos: TCodeXYPosition;
  DiskNm, UnitNm: String;
begin
  Result:=false;
  //BuildTree(lsrSourceName);
  with fCTLink do begin
    DiskNm := ExtractFileNameOnly(Code.Filename);
    if LowerCase(DiskNm)<>DiskNm then begin    // Lowercase name is found always.
      if not CodeTool.GetSourceNamePos(NamePos) then exit;
      UnitNm:=copy(CodeTool.Src, NamePos.StartPos, NamePos.EndPos-NamePos.StartPos);
      if DiskNm<>UnitNm then begin
        SrcCache.MainScanner:=CodeTool.Scanner;
        SrcCache.Replace(gtNone, gtNone, NamePos.StartPos, NamePos.EndPos, DiskNm);
        if not SrcCache.Apply then exit;
        if CodeTool.CleanPosToCaret(NamePos.StartPos, CaretPos) then
          fSettings.AddLogLine(mluNote,
            Format(lisConvFixedUnitName, [UnitNm, DiskNm]), fCode.Filename,
            CaretPos.Y, CaretPos.X);
      end;
    end;
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.AddModeDelphiDirective: boolean;
var
  NamePos: TAtomPosition;
  CaretPos: TCodeXYPosition;
  InsPos: Integer;
  s: String;
begin
  Result:=false;
  with fCTLink do begin
    if CodeTool.FindModeDirective(true,InsPos) then
      exit(true);                                  // Already has mode directive.
    Assert(Assigned(CodeTool.Tree.Root), 'AddModeDelphiDirective: Tree root is Nil.');
    if not CodeTool.GetSourceNamePos(NamePos) then exit;  // "unit" or "program"
    CodeTool.MoveCursorToCleanPos(NamePos.EndPos);
    CodeTool.ReadNextAtom; // semicolon
    InsPos:=CodeTool.CurPos.EndPos;
    if Settings.SupportDelphi then
      s:='{$IFDEF FPC}'+LineEnding+'  {$MODE Delphi}'+LineEnding+'{$ENDIF}'
    else
      s:='{$MODE Delphi}';
    SrcCache.MainScanner:=CodeTool.Scanner;
    SrcCache.Replace(gtEmptyLine, gtEmptyLine, InsPos, InsPos, s);
    if not SrcCache.Apply then exit;
    if CodeTool.CleanPosToCaret(InsPos, CaretPos) then
      fSettings.AddLogLine(mluNote, lisConvAddedModeDelphiModifier,
                           fCode.Filename, CaretPos.Y, CaretPos.X);
    CodeTool.BuildTree(lsrEnd);   // changing mode requires rescan
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.RenameResourceDirectives: boolean;
// rename {$R *.dfm} directive to {$R *.lfm}, or lowercase it.
// lowercase {$R *.RES} to {$R *.res}, or change it to a comment
var
  ParamPos, CleanPos: Integer;
  Key, LowKey, NewKey: String;
  s: string;
begin
  Result:=false;
  with fCTLink do begin
    if CodeTool=nil then exit;
    CleanPos:=1;
    // find $R directive
    if CodeTool.Scanner=nil then exit;
    repeat
      CleanPos:=FindNextCompilerDirectiveWithName(Code.Source, CleanPos, 'R',
                                      CodeTool.Scanner.NestedComments, ParamPos);
      if (CleanPos<1) or (CleanPos>CodeTool.SrcLen)
                      or (ParamPos>CodeTool.SrcLen-6) then break;
      NewKey:='';
      if (Code.Source[CleanPos]='{')
      and (Code.Source[ParamPos]='*')
      and (Code.Source[ParamPos+1]='.')
      and (Code.Source[ParamPos+5]='}') then
      begin
        Key:=copy(Code.Source, ParamPos+2, 3);
        LowKey:=LowerCase(Key);
        // Form file resource rename or lowercase:
        if (LowKey='dfm') or (LowKey='xfm') then begin
          if Assigned(Settings) and Settings.SupportDelphi then begin
            // Use the same dfm file. Lowercase existing key.
            if Settings.SameDfmFile then begin
              if Key<>LowKey then
                NewKey:=LowKey;
            end
            else begin
              // Add IFDEF for .lfm and .dfm allowing Delphi to use .dfm.
              s:='{$IFnDEF FPC}'+LineEnding+
                 '  {$R *.dfm}'+LineEnding+
                 '{$ELSE}'+LineEnding+
                 '  {$R *.lfm}'+LineEnding+
                 '{$ENDIF}';
              if not SrcCache.ReplaceEx(gtNone, gtNone, CleanPos, ParamPos+5,
                                        Code, CleanPos, ParamPos+6, s) then exit;
            end;
          end
          else       // Change .dfm to .lfm.
            NewKey:='lfm';
        end
        // lowercase {$R *.RES} to {$R *.res}, or change it to a comment
        else if LowKey='res' then begin
          case fResAction of
            raLowerCase:
              if Key='RES' then
                NewKey:=LowKey;
            raDelete:        // Make it a comment by adding a dot (.)
              if not SrcCache.ReplaceEx(gtNone, gtNone, CleanPos, CleanPos+1,
                                        Code, CleanPos, CleanPos+1, '{.') then exit;
          end;
        end;
        // Change a single resource name.
        if NewKey<>'' then
          if not SrcCache.ReplaceEx(gtNone, gtNone, ParamPos+2, ParamPos+5,
                                    Code, ParamPos+2, ParamPos+5, NewKey) then exit;
      end;
      CleanPos:=FindCommentEnd(Code.Source, CleanPos, CodeTool.Scanner.NestedComments);
    until false;
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
  Assert(Assigned(fCTLink.CodeTool));
  with fCTLink.CodeTool do begin
    Assert(Assigned(Scanner));
    BuildTree(lsrImplementationStart);
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
  // Parse replacement params which are defined in configuration.
  // They show which original params are copied where.
  // Returns the first position where comments can be searched from.
  var
    i, xNum, xStart, xLen: Integer;
    HasBracket: Boolean;
  begin
    HasBracket:=False;
    i:=0;
    while i<Length(aStr) do begin
      Inc(i);
      if aStr[i]='(' then
        HasBracket:=True;
      if aStr[i]='$' then begin
        xStart:=i;
        Inc(i);                           // Skip '$'
        while (i<Length(aStr)) and (aStr[i] in ['0'..'9']) do
          Inc(i);                         // Get the number after '$'
        xLen:=i-xStart;
        if xLen<2 then
          raise EDelphiConverterError.Create(Format(lisConvShouldBeFollowedByNumber, [aStr]));
        xNum:=StrToInt(copy(aStr, xStart+1, xLen-1)); // Leave out '$', convert number.
        if xNum < 1 then
          raise EDelphiConverterError.Create(Format(lisConvReplFuncParameterNum, [aStr]));
        ReplacementParams.Add(TReplacementParam.Create(xNum, xLen, xStart));
      end;
    end;
    if HasBracket and (aStr[i]<>')') then
      raise EDelphiConverterError.Create(Format(lisConvBracketMissingFromReplFunc, [aStr]));
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
  // Extract and return a possible comment from replacement function definition.
  var
    CommChBeg, CommBeg, CommEnd, i: Integer;   // Start and end of comment.
  begin
    Result:='';
    CommBeg:=0;
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
  CaretPos: TCodeXYPosition;
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
      if (FuncInfo.Params.Count>0) or (ReplacementParams.Count=0) then
      with fCTLink do
      begin
        NewFunc:=InsertParams2Replacement(FuncInfo);
        // Separate function body
        NewFunc:=NewFunc+FuncInfo.InclEmptyBrackets+FuncInfo.InclSemiColon;
        if fSettings.FuncReplaceComment then
          NewFunc:=NewFunc+Format(lisConvConvertedFrom, [FuncInfo.FuncName]);
        Comment:=GetComment(FuncInfo.ReplFunc, PossibleCommentPos);
        if Comment<>'' then            // Possible comment from the configuration
          NewFunc:=NewFunc+' { ' +Comment+' }';
        // Old function call with params for IDE message output.
        s:=copy(CodeTool.Src, FuncInfo.StartPos, FuncInfo.EndPos-FuncInfo.StartPos);
        s:=StringReplace(s, #10, '', [rfReplaceAll]);
        s:=StringReplace(s, #13, '', [rfReplaceAll]);
        // Now replace it.
        ResetMainScanner;
        if not SrcCache.Replace(gtNone, gtNone,
                            FuncInfo.StartPos, FuncInfo.EndPos, NewFunc) then exit;
        if CodeTool.CleanPosToCaret(FuncInfo.StartPos, CaretPos) then
          fSettings.AddLogLine(mluNote,
            Format(lisConvReplacedCall, [s, NewFunc]), fCode.Filename,
            CaretPos.Y, CaretPos.X);
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
    if GetAtom<>'.' then        // Don't save a method name (like TMyClass.Method).
      fDefinedProcNames.Add(ProcName);
  end;
  Result:=aNode.Next;
end;

function TConvDelphiCodeTool.ReplaceFuncCalls(aIsConsoleApp: boolean): boolean;
// Copied and modified from TFindDeclarationTool.FindReferences.
// Search for calls to functions / procedures in a list from current unit's
// implementation section code. Replace found calls with a given replacement.
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
    ExprStartPos, ExprEndPos: integer;
    RoundBrLvl, SquareBrLvl: integer;
    HasParams, ShouldReadNextAtom: Boolean;
  begin
    FuncInfo.InclEmptyBrackets:='';
    FuncInfo.InclSemiColon:='';
    FuncInfo.StartPos:=xStart;
    with fCTLink.CodeTool do begin
      MoveCursorToCleanPos(xStart);
      ReadNextAtom;                     // Read proc name.
      ReadNextAtom;                     // Read first atom after proc name.
      HasParams:=AtomIsChar('(');
      if HasParams then begin
        // read parameter list
        ReadNextAtom;
        // Don't read twice inside a loop. Atom can be for example '['
        ShouldReadNextAtom:=False;
        HasParams:=not AtomIsChar(')');
        if HasParams then begin
          // read all expressions
          RoundBrLvl:=0;
          SquareBrLvl:=0;
          while true do begin
            ExprStartPos:=CurPos.StartPos;
            // read til comma or bracket close
            repeat
              if ShouldReadNextAtom then
                ReadNextAtom;
              ShouldReadNextAtom:=True;
              if CurPos.StartPos>SrcLen then
                break;
              if (CurPos.Flag=cafComma) and (RoundBrLvl=0) and (SquareBrLvl=0) then
                break;
              if CurPos.Flag=cafEdgedBracketOpen then
                Inc(SquareBrLvl)
              else if CurPos.Flag=cafEdgedBracketClose then
                Dec(SquareBrLvl)
              else if CurPos.Flag=cafRoundBracketOpen then
                Inc(RoundBrLvl)
              else if CurPos.Flag=cafRoundBracketClose then begin
                if RoundBrLvl=0 then
                  break;              // Closing bracket, end of parameters
                Dec(RoundBrLvl);
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
              raise EDelphiConverterError.Create(lisConvBracketNotFound);
            ReadNextAtom;
          end;
        end
        else begin
          FuncInfo.InclEmptyBrackets:='()';
          ReadNextAtom;
        end;
      end;
      if not HasParams then begin
        FuncInfo.EndPos:=CurPos.StartPos;
        CheckSemiColon(FuncInfo);
      end;
    end;
    FuncInfo.UpdateReplacement;
  end;

  procedure ReadFuncCall(MaxPos: Integer);
  var
    FuncDefInfo, FuncCallInfo: TFuncReplacement;
    IdentName: string;
    i, x, IdentEndPos, IdentLen: Integer;
  begin
    IdentEndPos:=xStart;
    with fCTLink.CodeTool, fCTLink.Settings do
    try
      while (IdentEndPos<=MaxPos) and (IsIdentChar[Src[IdentEndPos]]) do
        inc(IdentEndPos);
      IdentLen:=IdentEndPos-xStart;
      SetLength(IdentName, IdentLen);
      StrMove(PChar(IdentName), @Src[xStart], IdentLen);
      // Don't try to uselessly find short identifiers
      if (IdentLen<ReplaceFuncs.MinFuncLen) and (IdentName<>'Ptr') then Exit;
      if fDefinedProcNames.Find(IdentName, x) then Exit;
      if not ReplaceFuncs.Funcs.Find(IdentName, i) then Exit;
      // Now function name is found in replacement list, get function info.
      FuncDefInfo:=ReplaceFuncs.FuncAtInd(i);
      if ReplaceFuncs.Categories.Find(FuncDefInfo.Category, i)
      // Categories.Objects[i] is used as a boolean flag.
      and Assigned(ReplaceFuncs.Categories.Objects[i])
      // UTF8 funcs are in LCL which console apps don't have -> don't change.
      and not (aIsConsoleApp and (FuncDefInfo.Category='UTF8Names'))
      // Keep Windows funcs in a Windows application.
      and (fCTLink.Settings.CrossPlatform or (FuncDefInfo.Category<>'WindowsAPI'))
      then begin
        // Create a new replacement object for params, position and other info.
        FuncCallInfo:=TFuncReplacement.Create(FuncDefInfo);
        ReadParams(FuncCallInfo);
        IdentEndPos:=FuncCallInfo.EndPos; // Skip the params, too, for next search.
        fFuncsToReplace.Add(FuncCallInfo);
      end;
    finally
      xStart:=IdentEndPos;
    end;
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
      BuildTree(lsrEnd);
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

