unit ConvCodeTool;

{$mode objfpc}{$H+}

interface

uses
  // LCL+FCL
  Classes, SysUtils, FileProcs, Forms, Controls, DialogProcs, Dialogs,
  // IDE
  LazarusIDEStrConsts, LazIDEIntf,
  // codetools
  CodeToolManager, StdCodeTools, CodeTree, CodeAtom,
  FindDeclarationTool, PascalReaderTool, PascalParserTool,
  CodeBeautifier, ExprEval, KeywordFuncLists, BasicCodeTools, LinkScanner,
  CodeCache, SourceChanger, CustomCodeTool, CodeToolsStructs, EventCodeTool;

type

  { TConvDelphiCodeTool }

  TConvDelphiCodeTool = class // (TStandardCodeTool)
  private
    fCodeTool: TEventsCodeTool;
    fCode: TCodeBuffer;
    fSrcCache: TSourceChangeCache;
    fScanner: TLinkScanner;
    fAsk: Boolean;
    fAddLRSCode: boolean;
    fLowerCaseRes: boolean;
    // List of units to remove.
    fUnitsToRemove: TStringList;
    // Units to rename. Map of unit name -> real unit name.
    fUnitsToRename: TStringToStringTree;
    // List of units to add.
    fUnitsToAdd: TStringList;
    // List of units to be commented.
    fUnitsToComment: TStringList;
    function AddModeDelphiDirective: boolean;
    function RemoveDFMResourceDirective: boolean;
    function LowerCaseMainResourceDirective: boolean;
    function AddLRSIncludeDirective: boolean;
    function RemoveUnits: boolean;
    function RenameUnits: boolean;
    function AddUnits: boolean;
    function CommentOutUnits: boolean;
//    function ConvertUsedUnits: boolean;
    function HandleCodetoolError: TModalResult;
  public
    constructor Create(Code: TCodeBuffer);
    destructor Destroy; override;
    function Convert: TModalResult;
  public
    property Ask: Boolean read fAsk write fAsk;
    property AddLRSCode: boolean read fAddLRSCode write fAddLRSCode;
    property LowerCaseRes: boolean read fLowerCaseRes write fLowerCaseRes;
    property UnitsToRemove: TStringList read fUnitsToRemove write fUnitsToRemove;
    property UnitsToRename: TStringToStringTree read fUnitsToRename write fUnitsToRename;
    property UnitsToAdd: TStringList read fUnitsToAdd write fUnitsToAdd;
    property UnitsToComment: TStringList read fUnitsToComment write fUnitsToComment;
  end;

implementation

{ TConvDelphiCodeTool }

constructor TConvDelphiCodeTool.Create(Code: TCodeBuffer);
begin
  fCode:=Code;
  // Default values for vars.
  fAsk:=true;
  fLowerCaseRes:=false;
  fAddLRSCode:=false;
  fUnitsToComment:=nil;
  fUnitsToRename:=nil;
  // Initialize codetools. (Copied from TCodeToolManager.)
  if not CodeToolBoss.InitCurCodeTool(Code) then exit;
  try
    fCodeTool:=CodeToolBoss.CurCodeTool;
    fSrcCache:=CodeToolBoss.SourceChangeCache;
//    if fSrcCache=nil then exit;
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
// remove windows unit and add LResources, LCLIntf
// remove {$R *.dfm} or {$R *.xfm} directive
// Change {$R *.RES} to {$R *.res} if needed
// add initialization
// add {$i unit.lrs} directive
// TODO: fix delphi ambiguousities like incomplete proc implementation headers
begin
  Result:=mrCancel;
  try
    fSrcCache.BeginUpdate;
    try
      // these changes can be applied together without rescan
      if not AddModeDelphiDirective then exit;
      if not RemoveDFMResourceDirective then exit;
      if not LowerCaseMainResourceDirective then exit;
      if not AddLRSIncludeDirective then exit;
      if not fSrcCache.Apply then exit;
    finally
      fSrcCache.EndUpdate;
    end;
    if not RemoveUnits then exit;
    if not RenameUnits then exit;
    if not AddUnits then exit;
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
      fSrcCache.Replace(gtEmptyLine,gtEmptyLine,InsertPos,InsertPos,'{$MODE Delphi}');
//      if not fSrcCache.Apply then exit;
    end;
    // changing mode requires rescan
    BuildTree(false);
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
  if fLowerCaseRes then begin
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

function TConvDelphiCodeTool.RemoveUnits: boolean;
// Remove units
var
  i: Integer;
begin
  Result:=false;
  if Assigned(fUnitsToRemove) then begin
    for i := 0 to fUnitsToRemove.Count-1 do
      if not fCodeTool.RemoveUnitFromAllUsesSections(fUnitsToRemove[i], fSrcCache) then
        exit;
  end;
  Result:=true;
end;

function TConvDelphiCodeTool.RenameUnits: boolean;
// Rename units
begin
  Result:=false;
  if Assigned(fUnitsToRename) then
    if not fCodeTool.ReplaceUsedUnits(fUnitsToRename, fSrcCache) then
      exit;
  Result:=true;
end;

function TConvDelphiCodeTool.AddUnits: boolean;
// Add units
var
  i: Integer;
begin
  Result:=false;
  if Assigned(fUnitsToAdd) then
    for i := 0 to fUnitsToAdd.Count-1 do
    if not fCodeTool.AddUnitToMainUsesSection(fUnitsToAdd[i],'',fSrcCache) then
      exit;
  Result:=true;
end;

function TConvDelphiCodeTool.CommentOutUnits: boolean;
// Comment out missing units
begin
  Result:=false;
  if Assigned(fUnitsToComment) and (fUnitsToComment.Count>0) then
    if not fCodeTool.CommentUnitsInUsesSections(fUnitsToComment, fSrcCache) then
      exit;
//      IDEMessagesWindow.AddMsg('Error="'+CodeToolBoss.ErrorMessage+'"','',-1);
  Result:=true;
end;


end.

