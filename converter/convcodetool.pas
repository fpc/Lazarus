unit ConvCodeTool;

{$mode objfpc}{$H+}

interface

uses
  // LCL+FCL
  Classes, SysUtils, FileProcs, Forms, Controls, DialogProcs,
    // TypInfo, CodeToolsStrConsts, AVL_Tree, LFMTrees,
  // codetools
  CodeToolManager, StdCodeTools, CodeTree, CodeAtom, //IdentCompletionTool,
  FindDeclarationTool, PascalReaderTool, PascalParserTool,
  CodeBeautifier, ExprEval, KeywordFuncLists, BasicCodeTools, LinkScanner,
  CodeCache, SourceChanger, CustomCodeTool, CodeToolsStructs, EventCodeTool;

type

  { TConvCodeTool }

  TConvDelphiCodeTool = class // (TStandardCodeTool)
  private
    fCodeTool: TEventsCodeTool;
    fCode: TCodeBuffer;
    fSrcCache: TSourceChangeCache;
    fScanner: TLinkScanner;
    fAsk: Boolean;
    fAddLRSCode: boolean;
    fMakeLowerCaseRes: boolean;
    function AddModeDelphiDirective: boolean;
    function ConvertUsedUnits: boolean;
    function RemoveDFMResourceDirective: boolean;
    function LowerCaseMainResourceDirective: boolean;
    function AddLRSIncludeDirective: boolean;

  public
    constructor Create(Code: TCodeBuffer; Ask, MakeLowerCaseRes, AddLRSCode: boolean);
    destructor Destroy; override;
    function Convert: TModalResult;
  end;

implementation

{ TConvDelphiCodeTool }

constructor TConvDelphiCodeTool.Create(Code: TCodeBuffer;
                                 Ask, MakeLowerCaseRes, AddLRSCode: boolean);
begin
  fCode:=Code;
  fAsk:=Ask;
  fMakeLowerCaseRes:=MakeLowerCaseRes;
  fAddLRSCode:=AddLRSCode;
  // Initialize codetools. (Copied from TCodeToolManager.)
  if not CodeToolBoss.InitCurCodeTool(Code) then exit;
  try
    fCodeTool:=CodeToolBoss.CurCodeTool;
    fSrcCache:=CodeToolBoss.SourceChangeCache;
    if fSrcCache=nil then exit;
    fScanner:=fCodeTool.Scanner;
    fSrcCache.MainScanner:=fScanner;
  except
    on e: Exception do
      CodeToolBoss.HandleException(e);
  end;
end;

destructor TConvDelphiCodeTool.Destroy;
begin
  inherited Destroy;
end;

function TConvDelphiCodeTool.Convert: TModalResult;
begin
  Result:=mrCancel;
  try
    fSrcCache.BeginUpdate;
    try
      if not AddModeDelphiDirective then exit;
      if not RemoveDFMResourceDirective then exit;
      if not LowerCaseMainResourceDirective then exit;
      if not AddLRSIncludeDirective then exit;
      if not ConvertUsedUnits then exit;
      if not fSrcCache.Apply then exit;
    finally
      fSrcCache.EndUpdate;
    end;
    Result:=mrOK;
  except
    Result:=JumpToCodetoolErrorAndAskToAbort(fAsk);
  end;
end;

function TConvDelphiCodeTool.AddModeDelphiDirective: boolean;
var
  ModeDirectivePos: integer;
  InsertPos: Integer;
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
      fSrcCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,
        '{$MODE Delphi}');
      if not fSrcCache.Apply then exit;
    end;
    // changing mode requires rescan
    BuildTree(false);
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.ConvertUsedUnits: boolean;
// replace unit 'Windows' with 'LCLIntf' and add 'LResources'
// rename 'in' filenames to case sensitive filename
var
  NamePos, InPos: TAtomPosition;
begin
  Result:=false;
  if fCodeTool.FindUnitInAllUsesSections('WINDOWS',NamePos,InPos)
  and (InPos.StartPos<1) then begin
    if not fSrcCache.Replace(gtNone,gtNone,
                         NamePos.StartPos,NamePos.EndPos,'LCLIntf') then
    begin
      exit;
    end;
  end;
  if fAddLRSCode then
    if not fCodeTool.AddUnitToMainUsesSection('LResources','',fSrcCache) then
    begin
      exit;
    end;
  if not fCodeTool.RemoveUnitFromAllUsesSections('VARIANTS',fSrcCache) then
  begin
    exit;
  end;
  if not fCodeTool.FixUsedUnitCase(fSrcCache) then
  begin
    exit;
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.RemoveDFMResourceDirective: boolean;
// remove {$R *.dfm} or {$R *.xfm} directive
var
  ParamPos: Integer;
  ACleanPos: Integer;
  StartPos: Integer;
  s: String;
begin
  Result:=false;
  // find $R directive
  ACleanPos:=1;
  with fCodeTool do
    repeat
      ACleanPos:=FindNextCompilerDirectiveWithName(Src,ACleanPos,'R',
        fScanner.NestedComments,ParamPos);
      if (ACleanPos<1) or (ACleanPos>SrcLen) or (ParamPos>SrcLen) then break;
      s:=UpperCaseStr(copy(Src,ParamPos,6));
      if (Src[ACleanPos]='{')
      and ((s='*.DFM}') or (s='*.XFM}'))
      then begin
        StartPos:=FindLineEndOrCodeInFrontOfPosition(ACleanPos,true);
        if not fSrcCache.Replace(gtNone,gtNone,StartPos,ParamPos+6,'')
        then exit;
        break;
      end;
      ACleanPos:=FindCommentEnd(Src,ACleanPos,fScanner.NestedComments);
    until false;
  Result:=true;
end;

function TConvDelphiCodeTool.LowerCaseMainResourceDirective: boolean;
// convert {$R *.RES} to {$R *.res}
var
  ParamPos: Integer;
  ACleanPos: Integer;
  s: String;
begin
  if fMakeLowerCaseRes then begin
    Result:=false;
    // find $R directive
    ACleanPos:=1;
    with fCodeTool do
      repeat
        ACleanPos:=FindNextCompilerDirectiveWithName(Src,ACleanPos,'R',
          fScanner.NestedComments,ParamPos);
        if (ACleanPos<1) or (ACleanPos>SrcLen) or (ParamPos>SrcLen) then break;
        s:=copy(Src,ParamPos,6);
        if (Src[ACleanPos]='{') and (s='*.RES}')
        then begin
          if not fSrcCache.Replace(gtNone,gtNone,ParamPos+2,ParamPos+5,'res') then exit;
          break;
        end;
        ACleanPos:=FindCommentEnd(Src,ACleanPos,fScanner.NestedComments);
      until false;
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.AddLRSIncludeDirective: boolean;
// add initialization and {$i unit.lrs} include directive
var
  FirstInclude: TCodeBuffer;
  LRSFilename: String;
  InitializationNode: TCodeTreeNode;
  ImplementationNode: TCodeTreeNode;
  NewCode: String;
  InsertPos: Integer;
  LinkIndex: Integer;
begin
  Result:=false;
  if fAddLRSCode then begin
    LRSFilename:=ExtractFilenameOnly(fCodeTool.MainFilename)+'.lrs';
    LinkIndex:=-1;
    FirstInclude:=fCodeTool.FindNextIncludeInInitialization(LinkIndex);
    if (FirstInclude<>nil)
    and (CompareFilenames(FirstInclude.Filename,LRSFilename)=0) then begin
      // already there
      Result:=true;
      exit;
    end;
    if fCodeTool.Tree.Root.Desc=ctnUnit then begin
      InitializationNode:=fCodeTool.FindInitializationNode;
      NewCode:=GetIndentStr(fSrcCache.BeautifyCodeOptions.Indent)
               +'{$i '+LRSFilename+'}';
      if InitializationNode=nil then begin
        // add also an initialization section
        ImplementationNode:=fCodeTool.FindImplementationNode;
        InsertPos:=ImplementationNode.EndPos;
        NewCode:=fSrcCache.BeautifyCodeOptions.BeautifyKeyWord('initialization')
                 +fSrcCache.BeautifyCodeOptions.LineEnd+NewCode;
        if not fSrcCache.Replace(gtEmptyLine,gtEmptyLine,
                                 InsertPos,InsertPos,NewCode) then exit;
      end else begin
        InsertPos:=InitializationNode.StartPos+length('initialization');
        if not fSrcCache.Replace(gtNewLine,gtNewLine,
                                 InsertPos,InsertPos,NewCode) then exit;
      end;
    end else begin
      // only Units supported yet
      exit;
    end;
  end;
  Result:=true;
end;


end.

