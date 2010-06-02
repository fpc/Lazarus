unit ConvCodeTool;

{$mode objfpc}{$H+}

interface

uses
  // LCL+FCL
  Classes, SysUtils, FileProcs, Forms, Controls, DialogProcs, Dialogs,
  // IDE
  LazarusIDEStrConsts, LazIDEIntf, FormEditor,
  // codetools
  CodeToolManager, StdCodeTools, CodeTree, CodeAtom,
  FindDeclarationTool, PascalReaderTool, PascalParserTool, LFMTrees,
  CodeBeautifier, ExprEval, KeywordFuncLists, BasicCodeTools, LinkScanner,
  CodeCache, SourceChanger, CustomCodeTool, CodeToolsStructs, EventCodeTool,
  // Converter
  ConvertSettings, ReplaceNamesUnit;

type

  // For future, when .dfm form file can be used for both Delphi and Lazarus.
{  TFormFileAction = (faUseDfm, faRenameToLfm, faUseBothDfmAndLfm); }

  { TConvDelphiCodeTool }

  TConvDelphiCodeTool = class
  private
    fCodeTool: TEventsCodeTool;
    fCode: TCodeBuffer;
    fSrcCache: TSourceChangeCache;
    fAsk: Boolean;
    fHasFormFile: boolean;
    fUseBothDfmAndLfm: boolean;
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
    function AddDelphiAndLCLSections: boolean;
    function AddModeDelphiDirective: boolean;
    function RenameResourceDirectives: boolean;
    function CommentOutUnits: boolean;
    function HandleCodetoolError: TModalResult;
    procedure DefaultFindDefinePropertyForContext(
      const ClassContext, AncestorClassContext: TFindContext; LFMNode: TLFMTreeNode;
      const IdentName: string; var IsDefined: boolean);
  public
    constructor Create(Code: TCodeBuffer);
    destructor Destroy; override;
    function Convert: TModalResult;
    function RemoveUnits: boolean;
    function RenameUnits: boolean;
    function UsesSectionsToUnitnames: TStringList;
    function FixMainClassAncestor(AReplaceTypes: TStringToStringTree): boolean;
    function FixLFM(LFMBuf: TCodeBuffer; out LFMTree: TLFMTree): boolean;
//      const OnFindDefineProperty: TOnFindDefinePropertyForContext;
//      RootMustBeClassInIntf, ObjectsMustExists: boolean): boolean;
  public
    property Ask: Boolean read fAsk write fAsk;
    property UseBothDfmAndLfm: boolean read fUseBothDfmAndLfm write fUseBothDfmAndLfm;
    property HasFormFile: boolean read fHasFormFile write fHasFormFile;
    property LowerCaseRes: boolean read fLowerCaseRes write fLowerCaseRes;
    property Target: TConvertTarget read fTarget write fTarget;
    property UnitsToRemove: TStringList read fUnitsToRemove write fUnitsToRemove;
    property UnitsToRename: TStringToStringTree read fUnitsToRename write fUnitsToRename;
    property UnitsToComment: TStringList read fUnitsToComment write fUnitsToComment;
  end;

  // Global function
  function FixMainClassAncestor(Code: TCodeBuffer; AReplaceTypes: TStringToStringTree): boolean;


implementation


function FixMainClassAncestor(Code: TCodeBuffer;
                              AReplaceTypes: TStringToStringTree): boolean;
var
  ConvTool: TConvDelphiCodeTool;
begin
  ConvTool:=TConvDelphiCodeTool.Create(Code);
  try     Result:=ConvTool.FixMainClassAncestor(AReplaceTypes);
  finally ConvTool.Free;
  end;
end;

{ TConvDelphiCodeTool }

constructor TConvDelphiCodeTool.Create(Code: TCodeBuffer);
begin
  fCode:=Code;
  // Default values for vars.
  fAsk:=true;
  fLowerCaseRes:=false;
  fUseBothDfmAndLfm:=false;
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
// TODO: fix delphi ambiguousities like incomplete proc implementation headers
begin
  Result:=mrCancel;
  try
    fSrcCache.BeginUpdate;
    try
      // these changes can be applied together without rescan
      if not AddModeDelphiDirective then exit;
      if not RenameResourceDirectives then exit;
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
      ACleanPos:=FindNextCompilerDirectiveWithName(Src,ACleanPos,'R',
        fCodeTool.Scanner.NestedComments,ParamPos);
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
          if fUseBothDfmAndLfm then begin
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
      ACleanPos:=FindCommentEnd(Src,ACleanPos,fCodeTool.Scanner.NestedComments);
    until false;
  // if there is already .lfm file, don't add IFDEF for .dfm / .lfm.
  if fUseBothDfmAndLfm and (fDfmDirectiveStart<>-1) and not AlreadyIsLfm then
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


function TConvDelphiCodeTool.FixMainClassAncestor(AReplaceTypes: TStringToStringTree): boolean;
// Change a type that main form inherits from to a fall-back type,
//  if defined in AReplaceTypes.

  function FindFirstClassNode: TCodeTreeNode;
  // Search for the first class definition which is the only one for form files.
  var
    ANode, ClassNode: TCodeTreeNode;
  begin
    ANode:=fCodeTool.FindMainUsesSection;  // or fCodeTool.FindInterfaceNode;
    if ANode<>nil then
      ANode:=ANode.NextBrother;
    Result:=nil;
    while ANode<>nil do begin
      if ANode.Desc in [ctnTypeDefinition,ctnGenericType] then begin
        ClassNode:=fCodeTool.FindTypeNodeOfDefinition(ANode);
        if (ClassNode<>nil) and (ClassNode.Desc in AllClassObjects) then begin
          if (not ((ClassNode.SubDesc and ctnsForwardDeclaration)>0)) then begin
            Result:=ClassNode;
            exit;
          end;
        end;
      end;
      ANode:=ANode.Next;
    end;
  end;

var
  ANode, InheritanceNode: TCodeTreeNode;
  TypeUpdater: TStringMapUpdater;
  OldType, NewType: String;
  HasChanged: Boolean;
begin
  Result:=false;          //  fCodeTool.FindInheritanceNode
  with fCodeTool do begin
    BuildTree(true);
    if (AReplaceTypes=nil) or (AReplaceTypes.Tree.Count=0) then exit(true);

    // Find the class name that the main class inherits from.
    ANode:=FindFirstClassNode;
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
      HasChanged:=false;
      if TypeUpdater.FindReplacement(OldType, NewType) then begin
        // change type
        if not HasChanged then begin
          HasChanged:=true;
          fSrcCache.MainScanner:=Scanner;
        end;
        if not fSrcCache.Replace(gtNone,gtNone,
                                 CurPos.StartPos,CurPos.EndPos, NewType) then
          exit(false);
      end;
      if HasChanged then
        if not fSrcCache.Apply then exit;
    finally
      TypeUpdater.Free;
    end;
  end;
  Result:=true;
end;

//////////////////////////////////////

procedure TConvDelphiCodeTool.DefaultFindDefinePropertyForContext(
  const ClassContext, AncestorClassContext: TFindContext; LFMNode: TLFMTreeNode;
  const IdentName: string; var IsDefined: boolean);
var
  PersistentClassName: String;
  AncestorClassName: String;
begin
  PersistentClassName:=ClassContext.Tool.ExtractClassName(ClassContext.Node,false);
  AncestorClassName:='';
  if AncestorClassContext.Tool<>nil then
    AncestorClassName:=AncestorClassContext.Tool.ExtractClassName(
                                                  AncestorClassContext.Node,false);
  FormEditor1.FindDefineProperty(PersistentClassName,AncestorClassName,
                                 IdentName,IsDefined);
//  OnFindDefineProperty(ClassContext.Tool,
//                       PersistentClassName,AncestorClassName,IdentName,IsDefined);
end;

///////////////////////////////////////
function TConvDelphiCodeTool.FixLFM(LFMBuf: TCodeBuffer; out LFMTree: TLFMTree): boolean;
//  const OnFindDefineProperty: TOnFindDefinePropertyForContext;
//  RootMustBeClassInIntf, ObjectsMustExists: boolean): boolean;
var
  RootContext: TFindContext;

  function CheckLFMObjectValues(LFMObject: TLFMObjectNode;
    const ClassContext: TFindContext): boolean; forward;

  function FindNonPublishedDefineProperty(LFMNode: TLFMTreeNode;
    DefaultErrorPosition: integer;
    const IdentName: string; const ClassContext: TFindContext): boolean;
  var
    PropertyNode: TLFMPropertyNode;
    ObjectNode: TLFMObjectNode;
    AncestorClassContext: TFindContext;
    Params: TFindDeclarationParams;
    IsDefined: Boolean;
  begin
    Result:=false;
    if (not (LFMNode is TLFMPropertyNode)) then exit;
    PropertyNode:=TLFMPropertyNode(LFMNode);
    if (PropertyNode.Parent=nil)
    or (not (PropertyNode.Parent is TLFMObjectNode)) then exit;
    ObjectNode:=TLFMObjectNode(PropertyNode.Parent);
    // find define property
    IsDefined:=false;
    if true {Assigned(fCodeTool.OnFindDefineProperty)} then begin
      AncestorClassContext:=CleanFindContext;
      if ClassContext.Tool=fCodeTool {Self} then begin
        // the class is defined in this source
        // -> try to find the ancestor class
        if ObjectNode.AncestorContextValid then begin
          AncestorClassContext:=CreateFindContext(
                                  TFindDeclarationTool(ObjectNode.AncestorTool),
                                  TCodeTreeNode(ObjectNode.AncestorNode));
        end else begin
          {$IFDEF VerboseCheckLFM}
          debugln('FindNonPublishedDefineProperty Class is defined in this source: search ancestor ... ');
          {$ENDIF}
          Params:=TFindDeclarationParams.Create;
          try
            Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound,
                           fdfExceptionOnPredefinedIdent];
            Params.ContextNode:=ClassContext.Node;
            try
              if ClassContext.Tool.FindAncestorOfClass(ClassContext.Node,
                Params,true) then
              begin
                {$IFDEF VerboseCheckLFM}
                debugln('FindNonPublishedDefineProperty Ancestor found');
                {$ENDIF}
                AncestorClassContext:=CreateFindContext(Params);
                ObjectNode.AncestorTool:=AncestorClassContext.Tool;
                ObjectNode.AncestorNode:=AncestorClassContext.Node;
              end;
            except
              // ignore search/parse errors
              on E: ECodeToolError do ;
            end;
          finally
            Params.Free;
          end;
          ObjectNode.AncestorContextValid:=true;
        end;
      end;
      DefaultFindDefinePropertyForContext(ClassContext,AncestorClassContext,LFMNode,
        IdentName,IsDefined);
      if IsDefined then begin
        //debugln('FindNonPublishedDefineProperty Path=',LFMNode.GetPath,' IdentName="',IdentName,'"');
      end else begin
        {$IFDEF VerboseCheckLFM}
        debugln('FindNonPublishedDefineProperty Path=',LFMNode.GetPath,' NO DEFINE PROPERTIES');
        {$ENDIF}
      end;
    end;
    Result:=IsDefined;
  end;

  function FindLFMIdentifier(LFMNode: TLFMTreeNode;
    DefaultErrorPosition: integer;
    const IdentName: string; const ClassContext: TFindContext;
    SearchAlsoInDefineProperties, ErrorOnNotFound: boolean;
    out IdentContext: TFindContext): boolean;
  var
    Params: TFindDeclarationParams;
    IdentifierNotPublished: Boolean;
    IsPublished: Boolean;
  begin
    Result:=false;
    IdentContext:=CleanFindContext;
    IsPublished:=false;
    if (ClassContext.Node=nil)
    or (not (ClassContext.Node.Desc in AllClasses)) then begin
      DebugLn('TStandardCodeTool.CheckLFM.FindLFMIdentifier Internal error');
      exit;
    end;
    Params:=TFindDeclarationParams.Create;
    try
      Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound,
                     fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
                     fdfIgnoreOverloadedProcs];
      Params.ContextNode:=ClassContext.Node;
      Params.SetIdentifier(ClassContext.Tool,PChar(Pointer(IdentName)),nil);
      try
        {DebugLn('FindLFMIdentifier A ',
          ' Ident=',
          '"'+GetIdentifier(Params.Identifier)+'"',
          ' Context="'+ClassContext.Node.DescAsString,'" "',StringToPascalConst(copy(ClassContext.Tool.Src,ClassContext.Node.StartPos,20))+'"',
          ' File="'+ExtractFilename(ClassContext.Tool.MainFilename)+'"',
          ' Flags=['+FindDeclarationFlagsAsString(Params.Flags)+']'
          );}
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
              Params.Flags:=[fdfSearchInAncestors,
                             fdfIgnoreMissingParams,
                             fdfIgnoreCurContextNode,
                             fdfIgnoreOverloadedProcs];
              Params.ContextNode:=IdentContext.Node.Parent;
              while (Params.ContextNode<>nil)
              and (not (Params.ContextNode.Desc in AllClasses)) do
                Params.ContextNode:=Params.ContextNode.Parent;
              if Params.ContextNode<>nil then begin
                Params.SetIdentifier(ClassContext.Tool,PChar(Pointer(IdentName)),nil);
                if not IdentContext.Tool.FindIdentifierInContext(Params) then
                begin
                  DebugLn(['FindLFMIdentifier ERROR ancestor of property not found: ',FindContextToString(IdentContext),' IdentName=',IdentName]);
                  break;
                end;
              end;
            end else
              break;
          until false;
        end;
      except
        // ignore search/parse errors
        on E: ECodeToolError do ;
      end;
    finally
      Params.Free;
    end;

    IdentifierNotPublished:=not IsPublished;

    if (IdentContext.Node=nil) or IdentifierNotPublished then begin
      // no proper node found
      // -> search in DefineProperties
      if SearchAlsoInDefineProperties then begin
        //debugln('FindLFMIdentifier A SearchAlsoInDefineProperties=',dbgs(SearchAlsoInDefineProperties));
        if FindNonPublishedDefineProperty(LFMNode,DefaultErrorPosition,IdentName,ClassContext)
        then begin
          Result:=true;
        end;
      end;
    end;
    if (not Result) and ErrorOnNotFound then begin
      if (IdentContext.Node<>nil) and IdentifierNotPublished then begin
        LFMTree.AddError(lfmeIdentifierNotPublished,LFMNode,
                         'identifier '+IdentName+' is not published in class '
                         +'"'+ClassContext.Tool.ExtractClassName(ClassContext.Node,false)+'"',
                         DefaultErrorPosition);
      end else begin
        LFMTree.AddError(lfmeIdentifierNotFound,LFMNode,
                         'identifier '+IdentName+' not found in class '
                         +'"'+ClassContext.Tool.ExtractClassName(ClassContext.Node,false)+'"',
                         DefaultErrorPosition);
      end;
    end;
  end;

  function FindClassNodeForLFMObject(LFMNode: TLFMTreeNode;
    DefaultErrorPosition: integer;
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
      Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound,
        fdfSearchInParentNodes,
        fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
        fdfIgnoreOverloadedProcs];
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
        // ignore search/parse errors
        on E: ECodeToolError do ;
      end;
    finally
      Params.Free;
    end;
    if Result.Node=nil then begin
      // FindClassNodeForLFMObject
      LFMTree.AddError(lfmeIdentifierNotFound,LFMNode,
                       'class '+GetIdentifier(Identifier)+' not found',
                       DefaultErrorPosition);
      exit;
    end;
  end;

  function CreateFootNote(const Context: TFindContext): string;
  var
    Caret: TCodeXYPosition;
  begin
    Result:=' see '+Context.Tool.MainFilename;
    if Context.Tool.CleanPosToCaret(Context.Node.StartPos,Caret) then
      Result:=Result+'('+IntToStr(Caret.Y)+','+IntToStr(Caret.X)+')';
  end;

  function FindClassContext(const ClassName: string): TFindContext;
  var
    Params: TFindDeclarationParams;
    Identifier: PChar;
    OldInput: TFindDeclarationInput;
//    StartTool: TStandardCodeTool;
  begin
    Result:=CleanFindContext;
    Params:=TFindDeclarationParams.Create;
//    StartTool:=Self;
    Identifier:=PChar(Pointer(ClassName));
    try
      Params.Flags:=[fdfExceptionOnNotFound,
        fdfSearchInParentNodes,
        fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
        fdfIgnoreOverloadedProcs];
      Params.ContextNode:=fCodeTool.FindInterfaceNode;
      if Params.ContextNode=nil then
        Params.ContextNode:=fCodeTool.FindMainUsesSection;
      Params.SetIdentifier(fCodeTool {StartTool},Identifier,nil);
      try
        Params.Save(OldInput);
        if fCodeTool.FindIdentifierInContext(Params) then begin
          Params.Load(OldInput,true);
          Result:=Params.NewCodeTool.FindBaseTypeOfNode(Params,Params.NewNode);
          if (Result.Node=nil)
          or (not (Result.Node.Desc in AllClasses)) then
            Result:=CleanFindContext;
        end;
      except
        // ignore search/parse errors
        on E: ECodeToolError do ;
      end;
    finally
      Params.Free;
    end;
  end;

  procedure CheckLFMChildObject(LFMObject: TLFMObjectNode;
    const ParentContext: TFindContext; SearchAlsoInDefineProperties: boolean);
  var
    LFMObjectName: String;
    ChildContext: TFindContext;
    VariableTypeName: String;
    DefinitionNode: TCodeTreeNode;
    ClassContext: TFindContext;
  begin
    // find variable for object

    // find identifier in Lookup Root
    LFMObjectName:=LFMObject.Name;
    //DebugLn('CheckChildObject A LFMObjectName="',LFMObjectName,'"');
    if LFMObjectName='' then begin
      LFMTree.AddError(lfmeObjectNameMissing,LFMObject,'missing object name',
                       LFMObject.StartPos);
      exit;
    end;

    if not FindLFMIdentifier(LFMObject,LFMObject.NamePosition,
      LFMObjectName,RootContext,SearchAlsoInDefineProperties,true{ObjectsMustExists},
      ChildContext)
    then begin
      // object name not found
//!!!      if ObjectsMustExists then
        exit;
    end;

    if true {ObjectsMustExists or (ChildContext.Node<>nil)} then begin
      if ChildContext.Node=nil then begin
        // this is an extra entry, created via DefineProperties.
        // There is no generic way to test such things
        exit;
      end;

      // check if identifier is a variable or property
      VariableTypeName:='';
      if (ChildContext.Node.Desc=ctnVarDefinition) then begin
        DefinitionNode:=ChildContext.Tool.FindTypeNodeOfDefinition(
                                                             ChildContext.Node);
        if DefinitionNode=nil then begin
          ChildContext.Node:=DefinitionNode;
          LFMTree.AddError(lfmeObjectIncompatible,LFMObject,
                           LFMObjectName+' is not a variable.'
                           +CreateFootNote(ChildContext),
                           LFMObject.NamePosition);
          exit;
        end;

        VariableTypeName:=ChildContext.Tool.ExtractDefinitionNodeType(
                                                             ChildContext.Node);
      end else if (ChildContext.Node.Desc=ctnProperty) then begin
        DefinitionNode:=ChildContext.Node;
        VariableTypeName:=
               ChildContext.Tool.ExtractPropType(ChildContext.Node,false,false);
      end else begin
        LFMTree.AddError(lfmeObjectIncompatible,LFMObject,
                         LFMObjectName+' is not a variable'
                         +CreateFootNote(ChildContext),
                         LFMObject.NamePosition);
        exit;
      end;

      // check if variable/property has a compatible type
      if (VariableTypeName<>'') then begin
        if (LFMObject.TypeName<>'')
        and (CompareIdentifiers(PChar(VariableTypeName),
                                PChar(LFMObject.TypeName))<>0)
        then begin
          ChildContext.Node:=DefinitionNode;
          LFMTree.AddError(lfmeObjectIncompatible,LFMObject,
                         VariableTypeName+' expected, but '+LFMObject.TypeName+' found.'
                         +CreateFootNote(ChildContext),
                         LFMObject.NamePosition);
          exit;
        end;

        // check if variable/property type exists

      end;


      // find class node
      ClassContext:=FindClassNodeForLFMObject(LFMObject,LFMObject.TypeNamePosition,
                                              ChildContext.Tool,DefinitionNode);
    end else begin
      // try the object type
      ClassContext:=FindClassContext(LFMObject.TypeName);
      if ClassContext.Node=nil then begin
        // object type not found
        LFMTree.AddError(lfmeIdentifierNotFound,LFMObject,
                         'type '+LFMObject.TypeName+' not found',
                         LFMObject.TypeNamePosition);
        exit;
      end;
    end;
    if ClassContext.Node=nil then exit;

    // check child LFM nodes
    CheckLFMObjectValues(LFMObject,ClassContext);
  end;

  function FindClassNodeForPropertyType(LFMProperty: TLFMPropertyNode;
    DefaultErrorPosition: integer; const PropertyContext: TFindContext
    ): TFindContext;
  var
    Params: TFindDeclarationParams;
  begin
    Result:=CleanFindContext;
    Params:=TFindDeclarationParams.Create;
    try
      Params.Flags:=[fdfSearchInAncestors,fdfExceptionOnNotFound,
        fdfSearchInParentNodes,
        fdfExceptionOnPredefinedIdent,fdfIgnoreMissingParams,
        fdfIgnoreOverloadedProcs];
      Params.ContextNode:=PropertyContext.Node;
      Params.SetIdentifier(PropertyContext.Tool,nil,nil);
      try
        Result:=PropertyContext.Tool.FindBaseTypeOfNode(Params,
                                                        PropertyContext.Node);
      except
        // ignore search/parse errors
        on E: ECodeToolError do ;
      end;
    finally
      Params.Free;
    end;
    if Result.Node=nil then begin
      LFMTree.AddError(lfmePropertyHasNoSubProperties,LFMProperty,
                       'property has no sub properties',
                       DefaultErrorPosition);
      exit;
    end;
  end;

  procedure CheckLFMProperty(LFMProperty: TLFMPropertyNode;
    const ParentContext: TFindContext);
  // checks properties. For example lines like 'OnShow = FormShow'
  // or 'VertScrollBar.Range = 29'
  // LFMProperty is the property node
  // ParentContext is the context, where properties are searched.
  //               This can be a class or a property.
  var
    i: Integer;
    CurName: string;
    CurPropertyContext: TFindContext;
    SearchContext: TFindContext;
  begin
    // find complete property name
    //DebugLn('CheckLFMProperty A LFMProperty Name="',LFMProperty.CompleteName,'"');

    if LFMProperty.CompleteName='' then begin
      LFMTree.AddError(lfmePropertyNameMissing,LFMProperty,
                       'property without name',LFMProperty.StartPos);
      exit;
    end;

    // find every part of the property name
    SearchContext:=ParentContext;
    for i:=0 to LFMProperty.NameParts.Count-1 do begin
      if SearchContext.Node.Desc=ctnProperty then begin
        // get the type of the property and search the class node
        SearchContext:=FindClassNodeForPropertyType(LFMProperty,
          LFMProperty.NameParts.NamePositions[i],SearchContext);
        if SearchContext.Node=nil then exit;
      end;

      CurName:=LFMProperty.NameParts.Names[i];
      if not FindLFMIdentifier(LFMProperty,
                               LFMProperty.NameParts.NamePositions[i],
                               CurName,SearchContext,true,true,
                               CurPropertyContext)
      then
        break;
      if CurPropertyContext.Node=nil then begin
        // this is an extra entry, created via DefineProperties.
        // There is no generic way to test such things
        break;
      end;
      SearchContext:=CurPropertyContext;
    end;

    // ToDo: check value
  end;

  function CheckLFMObjectValues(LFMObject: TLFMObjectNode;
    const ClassContext: TFindContext): boolean;
  var
    CurLFMNode: TLFMTreeNode;
  begin
    //DebugLn('TStandardCodeTool.CheckLFM.CheckLFMObjectValues A ',LFMObject.Name,':',LFMObject.TypeName);
    CurLFMNode:=LFMObject.FirstChild;
    while CurLFMNode<>nil do begin
      //DebugLn('TStandardCodeTool.CheckLFM.CheckLFMObjectValues B ',CurLFMNode.ClassName);
      case CurLFMNode.TheType of

      lfmnObject:
        CheckLFMChildObject(TLFMObjectNode(CurLFMNode),ClassContext,false);

      lfmnProperty:
        CheckLFMProperty(TLFMPropertyNode(CurLFMNode),ClassContext);

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

    //DebugLn('TStandardCodeTool.CheckLFM.CheckLFMRoot checking root ...');
    // get root object node
    if (RootLFMNode=nil) or (not (RootLFMNode is TLFMObjectNode)) then begin
      LFMTree.AddError(lfmeMissingRoot,nil,'missing root object',1);
      exit;
    end;
    LookupRootLFMNode:=TLFMObjectNode(RootLFMNode);

    // get type name of root object
    LookupRootTypeName:=UpperCaseStr(LookupRootLFMNode.TypeName);
    if LookupRootTypeName='' then begin
      LFMTree.AddError(lfmeMissingRoot,nil,'missing type of root object',1);
      exit;
    end;

    // find root type
    if true {RootMustBeClassInIntf} then begin
      RootClassNode:=fCodeTool.FindClassNodeInInterface(LookupRootTypeName,true,false,false);
      RootContext:=CleanFindContext;
      RootContext.Node:=RootClassNode;
      RootContext.Tool:=fCodeTool;
    end else begin
      RootContext:=FindClassContext(LookupRootTypeName);
      RootClassNode:=RootContext.Node;
    end;
    if RootClassNode=nil then begin
      LFMTree.AddError(lfmeMissingRoot,LookupRootLFMNode,
                       'type '+LookupRootLFMNode.TypeName+' not found',
                       LookupRootLFMNode.TypeNamePosition);
      exit;
    end;
    Result:=CheckLFMObjectValues(LookupRootLFMNode,RootContext);
  end;

var
  CurRootLFMNode: TLFMTreeNode;
begin
  Result:=false;
  //DebugLn('TStandardCodeTool.CheckLFM A');
  // create tree from LFM file
  LFMTree:=DefaultLFMTrees.GetLFMTree(LFMBuf,true);
  fCodeTool.ActivateGlobalWriteLock;
  try
    //DebugLn('TStandardCodeTool.CheckLFM parsing LFM ...');
    if not LFMTree.ParseIfNeeded then exit;
    // parse unit and find LookupRoot
    //DebugLn('TStandardCodeTool.CheckLFM parsing unit ...');
    fCodeTool.BuildTree(true);
    // find every identifier
    //DebugLn('TStandardCodeTool.CheckLFM checking identifiers ...');
    CurRootLFMNode:=LFMTree.Root;
    while CurRootLFMNode<>nil do begin
      if not CheckLFMRoot(CurRootLFMNode) then exit;
      CurRootLFMNode:=CurRootLFMNode.NextSibling;
    end;
  finally
    fCodeTool.DeactivateGlobalWriteLock;
  end;

  Result:=LFMTree.FirstError=nil;
end;

//////////////////////////////////////

end.

