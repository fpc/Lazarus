unit ConvCodeTool;

{$mode objfpc}{$H+}

interface

uses
  // LCL+FCL
  Classes, SysUtils, FileProcs, Forms, Controls, DialogProcs, Dialogs, contnrs,
  // IDE
  LazarusIDEStrConsts, LazIDEIntf, FormEditor, IDEMsgIntf,
  // codetools
  CodeToolManager, StdCodeTools, CodeTree, CodeAtom, AVL_Tree,
  FindDeclarationTool, PascalReaderTool, PascalParserTool, LFMTrees,
  CodeBeautifier, ExprEval, KeywordFuncLists, BasicCodeTools, LinkScanner,
  CodeCache, SourceChanger, CustomCodeTool, CodeToolsStructs, EventCodeTool,
  // Converter
  ConverterTypes, ConvertSettings, ReplaceNamesUnit;

type

  // For future, when .dfm form file can be used for both Delphi and Lazarus.
{  TFormFileAction = (faUseDfm, faRenameToLfm, faUseBothDfmAndLfm); }

  { TFuncCallPosition }

  TCalledFuncInfo = class
  private
//    fFuncName: string;
    fReplacement: string;
    fStartPos: Integer;
    fEndPos: Integer;
    fParams: TStringList; // fParamTypes: TExprTypeList;
  public
    constructor Create({aFuncName,} aReplacement: string);
    destructor Destroy; override;
  end;

  { TConvDelphiCodeTool }

  TConvDelphiCodeTool = class
  private
    fCodeTool: TCodeTool;
    fCode: TCodeBuffer;
    fSrcCache: TSourceChangeCache;
    fAsk: Boolean;
    fHasFormFile: boolean;
    fLowerCaseRes: boolean;
    fDfmDirectiveStart: integer;
    fDfmDirectiveEnd: integer;
    fTarget: TConvertTarget;
    // List of units to remove.
    fUnitsToRemove: TStringList;
    // Units to rename. Map of unit name -> real unit name.
    fUnitsToRename: TStringToStringTree;
    // List of units to be commented.
    fUnitsToComment: TStringList;
    // Delphi Function names to replace with FCL/LCL functions.
    fReplaceFuncs: TStringToStringTree;
    fFuncsToReplace: TObjectList;           // List of TCalledFuncInfo.
    function AddDelphiAndLCLSections: boolean;
    function AddModeDelphiDirective: boolean;
    function RenameResourceDirectives: boolean;
    function CommentOutUnits: boolean;
    function ReplaceFuncsInSource: boolean;
    function ReplaceFuncCalls: boolean;
    function HandleCodetoolError: TModalResult;
  public
    constructor Create(Code: TCodeBuffer);
    destructor Destroy; override;
    function Convert: TModalResult;
    function FindApptypeConsole: boolean;
    function RemoveUnits: boolean;
    function RenameUnits: boolean;
    function UsesSectionsToUnitnames: TStringList;
    function FixMainClassAncestor(const AClassName: string;
                                  AReplaceTypes: TStringToStringTree): boolean;
  public
    property Ask: Boolean read fAsk write fAsk;
    property HasFormFile: boolean read fHasFormFile write fHasFormFile;
    property LowerCaseRes: boolean read fLowerCaseRes write fLowerCaseRes;
    property Target: TConvertTarget read fTarget write fTarget;
    property UnitsToRemove: TStringList read fUnitsToRemove write fUnitsToRemove;
    property UnitsToRename: TStringToStringTree read fUnitsToRename write fUnitsToRename;
    property UnitsToComment: TStringList read fUnitsToComment write fUnitsToComment;
    property ReplaceFuncs: TStringToStringTree read fReplaceFuncs write fReplaceFuncs;
  end;


implementation

{ TCalledFuncInfo }

constructor TCalledFuncInfo.Create({aFuncName,} aReplacement: string);
begin
//  fFuncName:=aFuncName;
  fReplacement:=aReplacement;
  fParams:=TStringList.Create;
end;

destructor TCalledFuncInfo.Destroy;
begin
  fParams.Free;
  inherited Destroy;
end;


{ TConvDelphiCodeTool }

constructor TConvDelphiCodeTool.Create(Code: TCodeBuffer);
begin
  fCode:=Code;
  // Default values for vars.
  fAsk:=true;
  fLowerCaseRes:=false;
  fTarget:=ctLazarus;
  fUnitsToRemove:=nil;            // These are set from outside.
  fUnitsToComment:=nil;
  fUnitsToRename:=nil;
  // Initialize codetools. (Copied from TCodeToolManager.)
  if not CodeToolBoss.InitCurCodeTool(fCode) then exit;
  try
    fCodeTool:=CodeToolBoss.CurCodeTool;
    fSrcCache:=CodeToolBoss.SourceChangeCache;
    fSrcCache.MainScanner:=fCodeTool.Scanner;
  except
    on e: Exception do
      CodeToolBoss.HandleException(e);
  end;
end;

destructor TConvDelphiCodeTool.Destroy;
begin
  inherited Destroy;
end;

function TConvDelphiCodeTool.HandleCodetoolError: TModalResult;
// returns mrOk or mrAbort
const
  CodetoolsFoundError='The codetools found an error in unit %s:%s%s%s';
var
  ErrMsg: String;
begin
  ErrMsg:=CodeToolBoss.ErrorMessage;
  LazarusIDE.DoJumpToCodeToolBossError;
  if fAsk then begin
    Result:=QuestionDlg(lisCCOErrorCaption,
      Format(CodetoolsFoundError, [ExtractFileName(fCode.Filename), #13, ErrMsg, #13]),
      mtWarning, [mrIgnore, lisIgnoreAndContinue, mrAbort], 0);
    if Result=mrIgnore then Result:=mrOK;
  end else begin
    Result:=mrOK;
  end;
end;

function TConvDelphiCodeTool.Convert: TModalResult;
// add {$mode delphi} directive
// remove {$R *.dfm} or {$R *.xfm} directive
// Change {$R *.RES} to {$R *.res} if needed
// TODO: fix delphi ambiguouties like incomplete proc implementation headers
begin
  Result:=mrCancel;
  try
    fSrcCache.BeginUpdate;
    try
      // these changes can be applied together without rescan
      if not AddModeDelphiDirective then exit;
      if not RenameResourceDirectives then exit;
      if not ReplaceFuncCalls then exit;
      if not fSrcCache.Apply then exit;
    finally
      fSrcCache.EndUpdate;
    end;
    if fTarget=ctLazarus then begin
      // One way conversion -> remove, rename and comment out units.
      if not RemoveUnits then exit;
      if not RenameUnits then exit;
    end;
    if fTarget=ctLazarusAndDelphi then begin
      // Support Delphi. Add IFDEF blocks for units.
      if not AddDelphiAndLCLSections then exit;
    end
    else  // ctLazarus or ctLazarusWin -> comment units if needed.
      if not CommentOutUnits then exit;
    if not fCodeTool.FixUsedUnitCase(fSrcCache) then exit;
    if not fSrcCache.Apply then exit;
    Result:=mrOK;
  except
    on e: Exception do begin
      CodeToolBoss.HandleException(e);
      Result:=HandleCodetoolError;
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
  with fCodeTool do begin
    BuildTree(true);
    ACleanPos:=FindNextCompilerDirectiveWithName(Src, 1, 'Apptype',
                                                 Scanner.NestedComments, ParamPos);
    if (ACleanPos>0) and (ACleanPos<=SrcLen) and (ParamPos>0) then
      Result:=LowerCase(copy(Src,ParamPos,7))='console';
  end;
end;

function TConvDelphiCodeTool.AddDelphiAndLCLSections: boolean;
// add, remove and rename units for desired target.

  procedure RemoveUsesUnit(AUnitName: string);
  var
    UsesNode: TCodeTreeNode;
  begin
    fCodeTool.BuildTree(true);
    UsesNode:=fCodeTool.FindMainUsesSection;
    fCodeTool.MoveCursorToUsesStart(UsesNode);
    fCodeTool.RemoveUnitFromUsesSection(UsesNode, UpperCaseStr(AUnitName), fSrcCache);
  end;

var
  DelphiOnlyUnits: TStringList;  // Delphi specific units.
  LclOnlyUnits: TStringList;     // LCL specific units.
  RenameList: TStringList;
  UsesNode: TCodeTreeNode;
  s, nl: string;
  InsPos, i: Integer;
begin
  Result:=false;
  DelphiOnlyUnits:=TStringList.Create;
  LclOnlyUnits:=TStringList.Create;
  try
  fCodeTool.BuildTree(true);
  fSrcCache.MainScanner:=fCodeTool.Scanner;
  UsesNode:=fCodeTool.FindMainUsesSection;
  if UsesNode<>nil then begin
    fCodeTool.MoveCursorToUsesStart(UsesNode);
    InsPos:=fCodeTool.CurPos.StartPos;
    // Now don't remove or comment but add to Delphi block instead.
    for i:=0 to fUnitsToRemove.Count-1 do begin
      s:=fUnitsToRemove[i];
      RemoveUsesUnit(s);
      DelphiOnlyUnits.Append(s);
    end;
    for i:=0 to fUnitsToComment.Count-1 do begin
      s:=fUnitsToComment[i];
      RemoveUsesUnit(s);
      DelphiOnlyUnits.Append(s);
    end;
    RenameList:=TStringList.Create;
    try
      // Add replacement units to LCL block.
      fUnitsToRename.GetNames(RenameList);
      for i:=0 to RenameList.Count-1 do begin
        s:=RenameList[i];
        RemoveUsesUnit(s);
        DelphiOnlyUnits.Append(s);
        LclOnlyUnits.Append(fUnitsToRename[s]);
      end;
    finally
      RenameList.Free;
    end;
    if (LclOnlyUnits.Count>0) or (DelphiOnlyUnits.Count>0) then begin
      // Add LCL and Delphi sections for output.
      nl:=fSrcCache.BeautifyCodeOptions.LineEnd;
      s:='{$IFNDEF FPC}'+nl+'  ';
      for i:=0 to DelphiOnlyUnits.Count-1 do
        s:=s+DelphiOnlyUnits[i]+', ';
      s:=s+nl+'{$ELSE}'+nl+'  ';
      for i:=0 to LclOnlyUnits.Count-1 do
        s:=s+LclOnlyUnits[i]+', ';
      s:=s+nl+'{$ENDIF}';
      // Now add the generated lines.
      if not fSrcCache.Replace(gtEmptyLine,gtNewLine,InsPos,InsPos,s) then exit;
    end;
  end;
  Result:=true;
  finally
    LclOnlyUnits.Free;
    DelphiOnlyUnits.Free;
  end;
end;

function TConvDelphiCodeTool.AddModeDelphiDirective: boolean;
var
  ModeDirectivePos: integer;
  InsertPos: Integer;
  s, nl: String;
begin
  Result:=false;
  with fCodeTool do begin
    BuildTree(true);
    if not FindModeDirective(false,ModeDirectivePos) then begin
      // add {$MODE Delphi} behind source type
      if Tree.Root=nil then exit;
      MoveCursorToNodeStart(Tree.Root);
      ReadNextAtom; // 'unit', 'program', ..
      ReadNextAtom; // name
      ReadNextAtom; // semicolon
      InsertPos:=CurPos.EndPos;
      nl:=fSrcCache.BeautifyCodeOptions.LineEnd;
      if fTarget=ctLazarusAndDelphi then
        s:='{$IFDEF FPC}'+nl+'  {$MODE Delphi}'+nl+'{$ENDIF}'
      else
        s:='{$MODE Delphi}';
      fSrcCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,s);
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
  ParamPos: Integer;
  ACleanPos: Integer;
  Key, LowKey, NewKey: String;
  s, nl: string;
  AlreadyIsLfm: Boolean;
begin
  Result:=false;
  AlreadyIsLfm:=false;
  fDfmDirectiveStart:=-1;
  fDfmDirectiveEnd:=-1;
  ACleanPos:=1;
  // find $R directive
  with fCodeTool do
    repeat
      ACleanPos:=FindNextCompilerDirectiveWithName(Src, ACleanPos, 'R',
                                                   Scanner.NestedComments, ParamPos);
      if (ACleanPos<1) or (ACleanPos>SrcLen) or (ParamPos>SrcLen-6) then break;
      NewKey:='';
      if (Src[ACleanPos]='{') and
         (Src[ParamPos]='*') and (Src[ParamPos+1]='.') and
         (Src[ParamPos+5]='}')
      then begin
        Key:=copy(Src,ParamPos+2,3);
        LowKey:=LowerCase(Key);

        // Form file resource rename or lowercase:
        if (LowKey='dfm') or (LowKey='xfm') then begin
          // Lowercase existing key. (Future, when the same dfm file can be used)
//          faUseDfm: if Key<>LowKey then NewKey:=LowKey;
          if fTarget=ctLazarusAndDelphi then begin
            // Later IFDEF will be added so that Delphi can still use .dfm.
            fDfmDirectiveStart:=ACleanPos;
            fDfmDirectiveEnd:=ParamPos+6;
          end
          else       // Change .dfm to .lfm.
            NewKey:='lfm';
        end

        // If there already is .lfm, prevent adding IFDEF for .dfm / .lfm.
        else if LowKey='lfm' then begin
          AlreadyIsLfm:=true;
        end

        // lowercase {$R *.RES} to {$R *.res}
        else if (Key='RES') and fLowerCaseRes then
          NewKey:=LowKey;

        // Now change code.
        if NewKey<>'' then
          if not fSrcCache.Replace(gtNone,gtNone,ParamPos+2,ParamPos+5,NewKey) then exit;
      end;
      ACleanPos:=FindCommentEnd(Src, ACleanPos, Scanner.NestedComments);
    until false;
  // if there is already .lfm file, don't add IFDEF for .dfm / .lfm.
  if (fTarget=ctLazarusAndDelphi) and (fDfmDirectiveStart<>-1) and not AlreadyIsLfm then
  begin
    // Add IFDEF for .lfm and .dfm allowing Delphi to use .dfm.
    nl:=fSrcCache.BeautifyCodeOptions.LineEnd;
    s:='{$IFNDEF FPC}'+nl+
       '  {$R *.dfm}'+nl+
       '{$ELSE}'+nl+
       '  {$R *.lfm}'+nl+
       '{$ENDIF}';
    Result:=fSrcCache.Replace(gtNone,gtNone,fDfmDirectiveStart,fDfmDirectiveEnd,s);
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.RemoveUnits: boolean;
// Remove units
var
  i: Integer;
begin
  Result:=false;
  if Assigned(fUnitsToRemove) then begin
    for i:=0 to fUnitsToRemove.Count-1 do begin
      fSrcCache:=CodeToolBoss.SourceChangeCache;
      fSrcCache.MainScanner:=fCodeTool.Scanner;
      if not fCodeTool.RemoveUnitFromAllUsesSections(UpperCaseStr(fUnitsToRemove[i]),
                                                     fSrcCache) then
        exit;
      if not fSrcCache.Apply then exit;
    end;
  end;
  fUnitsToRemove.Clear;
  Result:=true;
end;

function TConvDelphiCodeTool.RenameUnits: boolean;
// Rename units
begin
  Result:=false;
  if Assigned(fUnitsToRename) then
    if not fCodeTool.ReplaceUsedUnits(fUnitsToRename, fSrcCache) then
      exit;
  fUnitsToRename.Clear;
  Result:=true;
end;

function TConvDelphiCodeTool.CommentOutUnits: boolean;
// Comment out missing units
begin
  Result:=false;
  if Assigned(fUnitsToComment) and (fUnitsToComment.Count>0) then
    if not fCodeTool.CommentUnitsInUsesSections(fUnitsToComment, fSrcCache) then
      exit;
  Result:=true;
end;

function TConvDelphiCodeTool.UsesSectionsToUnitnames: TStringList;
// Collect all unit names from uses sections to a StringList.
var
  UsesNode: TCodeTreeNode;
  ImplList: TStrings;
begin
  fCodeTool.BuildTree(true);
  fSrcCache.MainScanner:=fCodeTool.Scanner;
  UsesNode:=fCodeTool.FindMainUsesSection;
  Result:=TStringList(fCodeTool.UsesSectionToUnitnames(UsesNode));
  UsesNode:=fCodeTool.FindImplementationUsesSection;
  ImplList:=fCodeTool.UsesSectionToUnitnames(UsesNode);
  Result.AddStrings(ImplList);
  ImplList.Free;
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
  with fCodeTool do begin
    BuildTree(true);
    // Find the class name that the main class inherits from.
    ANode:=FindClassNodeInInterface(AClassName,true,false,false); // FindFirstClassNode;
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
    // Change the inheritance type to a fall-back type if needed.
    TypeUpdater:=TStringMapUpdater.Create(AReplaceTypes);
    try
      // Find replacement maybe using regexp syntax.
      if TypeUpdater.FindReplacement(OldType, NewType) then begin
        fSrcCache.MainScanner:=Scanner;
        if not fSrcCache.Replace(gtNone, gtNone,
                      CurPos.StartPos, CurPos.EndPos, NewType) then exit;
        if not fSrcCache.Apply then exit;
      end;
    finally
      TypeUpdater.Free;
    end;
  end;
  Result:=true;
end;

procedure SplitParam(const aStr: string; aDelimiter: Char; Result: TStringList);
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
      raise EConverterError.Create('Replacement function parameter should start with "$".');
    Result.Add(Copy(aStr, Start, Len));
  end;

var
  i, Start, EndPlus1: Integer;
begin
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
  FuncInfo: TCalledFuncInfo;
  ParamList: TStringList;
  BegPos, EndPos, ParamPos: Integer;
  i, j: Integer;
  s, FuncBody, NewFunc, Param: String;
begin
  Result:=false;
  ParamList:=TStringList.Create;
  try
    // Replace from bottom to top.
    for i:=fFuncsToReplace.Count-1 downto 0 do begin
      FuncInfo:=TCalledFuncInfo(fFuncsToReplace[i]);
      // Parse the replacement parameters.
      // They indicate which original params are copied where.
      BegPos:=Pos('(', FuncInfo.fReplacement);
      if BegPos>0 then begin
        EndPos:=Pos(')', FuncInfo.fReplacement);
        if EndPos=0 then
          raise EConverterError.Create('")" is missing from replacement function.');
        FuncBody:=Copy(FuncInfo.fReplacement, 1, BegPos-1);
        s:=Copy(FuncInfo.fReplacement, BegPos+1, EndPos-BegPos-1);
        SplitParam(s, ',', ParamList);      // The actual parameter list.
      end
      else begin
        FuncBody:=FuncInfo.fReplacement;
      end;
      // Collect parameters from original call and construct a new function call.
      s:='';
      for j:=0 to ParamList.Count-1 do begin
        ParamPos:=StrToInt(ParamList[j]);
        if ParamPos < 1 then
          raise EConverterError.Create('Replacement function parameter number should be >= 1.');
        Param:='nil';         // Default value if not found from original code.
        if ParamPos <= FuncInfo.fParams.Count then
          Param:=FuncInfo.fParams[ParamPos-1];
        if s<>'' then
          s:=s+', ';
        s:=s+Param;
      end;
      NewFunc:=FuncBody+'('+s+')';
      // Old function call with params for IDE message output.
      s:=copy(fCodeTool.Src, FuncInfo.fStartPos, FuncInfo.fEndPos-FuncInfo.fStartPos);
      s:=StringReplace(s, sLineBreak, '', [rfReplaceAll]);
      // Now replace it.
      fSrcCache.MainScanner:=fCodeTool.Scanner;
      if not fSrcCache.Replace(gtNone, gtNone,
                        FuncInfo.fStartPos, FuncInfo.fEndPos, NewFunc) then exit;
      IDEMessagesWindow.AddMsg('Replaced call to '+s, '', -1);
      IDEMessagesWindow.AddMsg('  with '+NewFunc, '', -1);
    end;
//    if fFuncsToReplace.Count > 0 then
//      if not fSrcCache.Apply then exit;     Applied in method Convert.
  finally
    ParamList.Free;
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.ReplaceFuncCalls: boolean;
// Copied and modified from TFindDeclarationTool.FindReferences.
// Search for calls to functions / procedures given in a list in current unit's
// implementation section. Add their positions to another list for replacement.
var
  FuncNames: TStringList;
  DeclarationNode: TCodeTreeNode;
  CursorNode: TCodeTreeNode;
  StartPos: Integer;
  MinPos, MaxPos: Integer;

  procedure ReadParams(FuncInfo: TCalledFuncInfo);
  var
    BracketClose: char;
    s: string;
    ExprStartPos, ExprEndPos: integer;
  begin
    FuncInfo.fStartPos:=StartPos;
    with fCodeTool do begin
      MoveCursorToCleanPos(StartPos);
      ReadNextAtom;                 // Read func name
      ReadNextAtom;                 // Read first atom after proc name
      if AtomIsChar('(') then begin
        BracketClose:=')';
        // read parameter list
        ReadNextAtom;
        if not AtomIsChar(BracketClose) then begin
          // read all expressions
          while true do begin
            ExprStartPos:=CurPos.StartPos;
            // read til comma or bracket close
            repeat
              ReadNextAtom;
              if (CurPos.StartPos>SrcLen)
              or (CurPos.Flag in [cafRoundBracketClose, cafComma]) then
                break;
            until false;
            ExprEndPos:=CurPos.StartPos;
            // Add parameter to list
            s:=copy(Src,ExprStartPos,ExprEndPos-ExprStartPos);
            FuncInfo.fParams.Add(s);
            MoveCursorToCleanPos(ExprEndPos);
            ReadNextAtom;
            if AtomIsChar(BracketClose) then begin
              FuncInfo.fEndPos:=CurPos.EndPos;
              break;
            end;
            if not AtomIsChar(',') then
              raise EConverterError.Create('Bracket not found');
            ReadNextAtom;
          end;
        end;
      end
      else if AtomIsChar(';') then
        FuncInfo.fEndPos:=CurPos.EndPos;
//raise EConverterError.Create('The called function is not ended like it should.');
    end;
  end;

  procedure ReadIdentifier;
  var
    IdentEndPos: LongInt;
    FuncInfo: TCalledFuncInfo;
    FuncName: string;
    i: Integer;
  begin
    IdentEndPos:=StartPos;
    with fCodeTool do begin
      while (IdentEndPos<=MaxPos) and (IsIdentChar[Src[IdentEndPos]]) do
        inc(IdentEndPos);
      for i:=0 to FuncNames.Count-1 do begin
        FuncName:=FuncNames[i];
        if (IdentEndPos-StartPos=length(FuncName))
        and (CompareIdentifiers(PChar(Pointer(FuncName)),@Src[StartPos])=0)
        then begin
          CursorNode:=BuildSubTreeAndFindDeepestNodeAtPos(StartPos,true);
          if CleanPosIsDeclarationIdentifier(StartPos,CursorNode) then
            // this identifier is another declaration with the same name
          else begin
            FuncInfo:=TCalledFuncInfo.Create({FuncName,} fReplaceFuncs[FuncName]);
            ReadParams(FuncInfo);
            fFuncsToReplace.Add(FuncInfo);
//            ExprTypes:=CreateParamExprListFromStatement(CurPos.EndPos, Params);
          end;
          Break;
        end;
      end;
    end;
    StartPos:=IdentEndPos;
  end;

  procedure SearchIdentifiers;
  var
    CommentLvl: Integer;
    InStrConst: Boolean;
  begin
    StartPos:=MinPos;
    with fCodeTool do
    while StartPos<=MaxPos do begin
      case Src[StartPos] of

      '{': // pascal comment
        begin
          inc(StartPos);
          CommentLvl:=1;
          InStrConst:=false;
          while StartPos<=MaxPos do begin
            case Src[StartPos] of
            '{': if Scanner.NestedComments then inc(CommentLvl);
            '}':
              begin
                dec(CommentLvl);
                if CommentLvl=0 then break;
              end;
            '''':
              InStrConst:=not InStrConst;
            end;
            inc(StartPos);
          end;
          inc(StartPos);
        end;

      '/':  // Delphi comment
        if (Src[StartPos+1]<>'/') then begin
          inc(StartPos);
        end else begin
          inc(StartPos,2);
          InStrConst:=false;
          while (StartPos<=MaxPos) do begin
            case Src[StartPos] of
            #10,#13:
              break;
            '''':
              InStrConst:=not InStrConst;
            end;
            inc(StartPos);
          end;
          inc(StartPos);
          if (StartPos<=MaxPos) and (Src[StartPos] in [#10,#13])
          and (Src[StartPos-1]<>Src[StartPos]) then
            inc(StartPos);
        end;

      '(': // turbo pascal comment
        if (Src[StartPos+1]<>'*') then begin
          inc(StartPos);
        end else begin
          inc(StartPos,3);
          InStrConst:=false;
          while (StartPos<=MaxPos) do begin
            case Src[StartPos] of
            ')':
              if Src[StartPos-1]='*' then break;
            '''':
              InStrConst:=not InStrConst;
            end;
            inc(StartPos);
          end;
          inc(StartPos);
        end;

      'a'..'z','A'..'Z','_':
        ReadIdentifier;

      '''':
        begin
          // skip string constant
          inc(StartPos);
          while (StartPos<=MaxPos) do begin
            if (not (Src[StartPos] in ['''',#10,#13])) then
              inc(StartPos)
            else begin
              inc(StartPos);
              break;
            end;
          end;
        end;

      else
        inc(StartPos);
      end;
    end;
  end;

begin
  Result:=false;
  with fCodeTool do begin
    fFuncsToReplace:=TObjectList.Create;
    FuncNames:=TStringList.Create;
    ActivateGlobalWriteLock;
    try
      fReplaceFuncs.GetNames(FuncNames);
      BuildTree(false);
      // Find calls for functions in the list
      // Does this make sense ???
      CursorNode:=fCodeTool.FindImplementationNode;
      if CursorNode=nil then begin
        CursorNode:=fCodeTool.FindMainBeginEndNode;
//     or CursorNode:=fCodeTool.FindMainUsesSection; ?
        if CursorNode=nil then Exit;
      end;
      MinPos:=CursorNode.StartPos;
      MaxPos:=SrcLen;
// was:  MinPos:=Tree.FindFirstPosition;
//      MaxPos:=Tree.FindLastPosition;
//      if MaxPos>SrcLen then
      SearchIdentifiers;
      if not ReplaceFuncsInSource then Exit;
    finally
      FuncNames.Free;
      fFuncsToReplace.Free;
      DeactivateGlobalWriteLock;
    end;
  end;
  Result:=true;
end;


end.

