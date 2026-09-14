unit FindRenameIdentExec;

{$mode ObjFPC}{$H+}
{.$DEFINE EnableFPDocRename}

interface

uses
  // RTL + FCL
  Classes, SysUtils, AVL_Tree, Contnrs,
  // LCL
  Forms, Controls, Dialogs, StdCtrls, ExtCtrls, ComCtrls, LclIntf,
  // CodeTools
  KeywordFuncLists, CTUnitGraph, CodeTree, CodeAtom, LinkScanner,
  CustomCodeTool, CodeCache, FileProcs, BasicCodeTools, CodeToolManager,
  CodeToolsStructs, FindDeclarationTool, ChangeDeclarationTool,
  // LazUtils
  LazFileUtils, FileUtil, LazFileCache, LazStringUtils, AvgLvlTree, LazLoggerBase,
  {$IFDEF EnableFPDocRename}laz2_DOM,{$ENDIF}
  // BuildIntf
  PackageIntf,
  // IdeIntf
  LazIDEIntf, SrcEditorIntf, IDEWindowIntf, IDEDialogs,
  // IdeUtils
  InputHistory,
  // IdeConfig
  TransferMacros, IDEProcs, DialogProcs, SearchPathProcs, EnvironmentOpts, MiscOptions,
  // IdeProject
  Project,
  // IDE
  LazarusIDEStrConsts, SearchResultView, FindRenameIdentifier,
  CodeHelp, SourceFileManager, EditableProject;

type
  { TFindRenameIdentSeeker }

  TFindRenameIdentSeeker = class
  private
    FDeclXY: TPoint;
    FDeclCleanPos: integer;
    FDeclTool: TCodeTool;
    FDeclNode: TCodeTreeNode;
    FDeclCodeXY: TCodeXYPosition;
    FDeclTopLine: integer;
    FIdentifier: string;
    FModifiedDesigners: TFPList;  // List of TEditableUnitInfo
    FKind: TFRIdentifierKind;
    FPascalReferences: TObjectList; // list of TSrcNameRefs
    FLfmReferences: TCodeXYPositions;
    FRenamingFile: Boolean;
    FStartSrcCode: TCodeBuffer;
    FStartCaretXY: TPoint;
    FStartSrcEdit: TSourceEditorInterface;
    FOldFileName, FNewFilename: string;
    FOptions: TFindRenameIdentifierOptions;
    FTheFiles: TStringList;
    function AddExtraFiles: boolean;
    procedure AddOwnerFiles(aOwnerList: TFPList);
    procedure AddReferencesToResultView(SearchPageIndex: integer; RenameTo: string);
    procedure CheckDeclOfDesigner;
    function CheckUsesNode: boolean;
    function DeclarationCanBeInLFM: boolean;
    procedure Err(id: int64; Msg: string);
    function GatherIdentifierReferences(Flags: TFindRefsFlags): boolean;
    function GatherLFMsReferences(Flags: TFindRefsFlags): TModalResult;
    {$IFDEF EnableFPDocRename}
    function GatherFPDocReferencesForPascalFiles(DeclarationCode: TCodeBuffer;
      DeclarationCaretXY: TPoint; var ListOfLazFPDocNode: TFPList): TModalResult;
    function GatherReferencesInFPDocFile(
      const OldPackageName, OldModuleName, OldElementName, FPDocFilename: string;
      var ListOfLazFPDocNode: TFPList): TModalResult;
    {$ENDIF}
    function Initialize: TModalResult;
    function NodeIsRootClassComponent(aNode: TCodeTreeNode; RootComp: TComponent): boolean;
    function RenameAll: TModalResult;
    function RenameIdentifier_UnitFile(out OldRefs: TSrcNameRefs): TModalResult;
    function RenameProgram: TModalResult;
    function ShowIdentifierReferences(DeclFilename: string; RenameTo: string): TModalResult;
    function UpdateCodeNode: boolean;
  public
    constructor Create(AOptions: TFindRenameIdentifierOptions);
    destructor Destroy; override;
    function Execute(AllowRename, SetRenameActive: boolean): TModalResult;
  end;

function DoFindRenameIdentifier(
  AllowRename: boolean; // allow user to disable/enable rename
  SetRenameActive: boolean; // check rename
  Options: TFindRenameIdentifierOptions): TModalResult;


implementation

function DoFindRenameIdentifier(AllowRename: boolean; SetRenameActive: boolean;
  Options: TFindRenameIdentifierOptions): TModalResult;
var
  IdentSeeker: TFindRenameIdentSeeker;
begin
  IdentSeeker:=TFindRenameIdentSeeker.Create(Options);
  try
    Result:=IdentSeeker.Initialize;
    if Result=mrOK then
      Result:=IdentSeeker.Execute(AllowRename, SetRenameActive);
  finally
    IdentSeeker.Free;
  end;
end;

{ TFindRenameIdentSeeker }

constructor TFindRenameIdentSeeker.Create(AOptions: TFindRenameIdentifierOptions);
begin
  FOptions:=AOptions;
  FRenamingFile:=False;
end;

destructor TFindRenameIdentSeeker.Destroy;
begin
  inherited Destroy;
end;

function TFindRenameIdentSeeker.AddExtraFiles: boolean;
// TODO: replace TStringsList with a AVL tree
var
  i: Integer;
  CurFileMask: string;
  FileInfo: TSearchRec;
  CurDirectory: String;
  CurFilename: String;
  OnlyPascalSources: Boolean;
  SPMaskType: TSPMaskType;
  FilesTree: TFilenameToStringTree;
  FTItem: PStringToStringItem;
begin
  Result:=false;
  if (FOptions.ExtraFiles<>nil) then begin
    for i:=0 to FOptions.ExtraFiles.Count-1 do begin
      CurFileMask:=FOptions.ExtraFiles[i];
      if not GlobalMacroList.SubstituteStr(CurFileMask) then begin
        Err(20250206153855,'invalid file mask "'+CurFileMask+'"');
        exit;
      end;
      CurFileMask:=ChompPathDelim(CurFileMask);
      if not FilenameIsAbsolute(CurFileMask) then begin
        if LazarusIDE.ActiveProject.IsVirtual then continue;
        CurFileMask:=AppendPathDelim(LazarusIDE.ActiveProject.Directory+CurFileMask);
      end;
      CurFileMask:=TrimFilename(CurFileMask);
      SPMaskType:=GetSPMaskType(CurFileMask);
      if SPMaskType<>TSPMaskType.None then
      begin
        FilesTree:=TFilenameToStringTree.Create(false);
        try
          CollectFilesInSearchPath(CurFileMask,FilesTree);
          for FTItem in FilesTree do
          begin
            if not FilenameIsPascalSource(FTItem^.Name) then
              continue;
            if FileIsText(FTItem^.Name) then
              FTheFiles.Add(FTItem^.Name);
          end;
        finally
          FilesTree.Free;
        end;
        continue;
      end;

      OnlyPascalSources:=false;
      if DirPathExistsCached(CurFileMask) then begin
        // a whole directory
        OnlyPascalSources:=true;
        CurFileMask:=AppendPathDelim(CurFileMask)+AllFilesMask;
      end else if FileExistsCached(CurFileMask) then begin
        // single file
        FTheFiles.Add(CurFileMask);
        continue;
      end else begin
        // a mask
      end;
      if FindFirstUTF8(CurFileMask,faAnyFile,FileInfo)=0
      then begin
        CurDirectory:=AppendPathDelim(ExtractFilePath(CurFileMask));
        repeat
          // check if special file
          if (FileInfo.Name='.') or (FileInfo.Name='..') or (FileInfo.Name='')
          then
            continue;
          if OnlyPascalSources and not FilenameIsPascalSource(FileInfo.Name)
          then
            continue;
          CurFilename:=CurDirectory+FileInfo.Name;
          //debugln(['AddExtraFiles ',CurFilename]);
          if FileIsText(CurFilename) then
            FTheFiles.Add(CurFilename);
        until FindNextUTF8(FileInfo)<>0;
      end;
      FindCloseUTF8(FileInfo);
    end;
  end;
  Result:=true;
end;

procedure TFindRenameIdentSeeker.AddOwnerFiles(aOwnerList: TFPList);
var
  ExtraFiles: TStrings;
  Graph: TUsesGraph;
  UGUnit: TUGUnit;
  AVLNode: TAVLTreeNode;
  Completed: Boolean;
  i: Integer;
begin
  // start in all listed Files of the package(s)
  ExtraFiles:=PackageEditingInterface.GetSourceFilesOfOwners(aOwnerList);
  if ExtraFiles<>nil then
  begin
    // parse all used units
    Graph:=CodeToolBoss.CreateUsesGraph;
    try
      for i:=0 to ExtraFiles.Count-1 do
        Graph.AddStartUnit(ExtraFiles[i]);
      Graph.AddTargetUnit(FDeclCodeXY.Code.Filename);
      Graph.Parse(true,Completed);
      AVLNode:=Graph.FilesTree.FindLowest;
      while AVLNode<>nil do begin
        UGUnit:=TUGUnit(AVLNode.Data);
        FTheFiles.Add(UGUnit.Filename);
        AVLNode:=AVLNode.Successor;
      end;
    finally
      ExtraFiles.Free;
      Graph.Free;
    end;
  end;
end;

procedure TFindRenameIdentSeeker.AddReferencesToResultView(SearchPageIndex: integer;
  RenameTo: string);
var
  CodePos: PCodeXYPosition;
  CurLine, TrimmedLine, CurIdentifier: String;
  TrimCnt: Integer;
  ANode: TAVLTreeNode;
  CaretXY: TCodeXYPosition;
  i, Len, LastLine, Drift: integer;
  CleanPos: integer;
  EndPos: integer;
  CodeTool: TCodeTool;
  Refs: TSrcNameRefs;
  Tree: TAVLTree;
begin
  SearchResultsView.BeginUpdate(SearchPageIndex);
  SearchResultsView.Items[SearchPageIndex].Clear;
  if FPascalReferences<>nil then begin
    for i:=0 to FPascalReferences.Count-1 do begin
      Refs:=TSrcNameRefs(FPascalReferences[i]);
      Tree:=Refs.TreeOfPCodeXYPosition;
      if Tree=nil then continue;
      CurIdentifier:=Refs.NewLocalSrcName;
      if CurIdentifier='' then
        CurIdentifier:=RenameTo;
      if CurIdentifier='' then
        CurIdentifier:=FIdentifier;
      ANode:=Tree.FindHighest;
      LastLine:=-1;
      Drift:=0;
      while ANode<>nil do begin
        CodePos:=PCodeXYPosition(ANode.Data);
        ANode:=Tree.FindPrecessor(ANode);
        CurLine:=TrimRight(CodePos^.Code.GetLine(CodePos^.Y-1,false));
        TrimmedLine:=Trim(CurLine);
        TrimCnt:=length(CurLine)-length(TrimmedLine);
        //debugln('ShowReferences x=',dbgs(CodePos^.x),' y=',dbgs(CodePos^.y),' ',CurLine);
        Len:=length(CurIdentifier);
        if Pos('.',CurIdentifier)>0 then begin
          CodeToolBoss.Explore(CodePos^.Code,CodeTool,true);
          CaretXY.X:=CodePos^.X;
          CaretXY.Y:=CodePos^.Y;
          CaretXY.Code:=CodePos^.Code;
          if CodeTool.CaretToCleanPos(CaretXY,CleanPos)<>0 then
            continue;
          CodeTool.ExtractIdentifierWithPointsOutEndPos(CleanPos,EndPos,length(CurIdentifier));
          Len:=EndPos-CleanPos;
        end;
        if LastLine=CodePos^.Y then
          inc(Drift,Len-length(FIdentifier))
        else begin
          Drift:=0;
          LastLine:=CodePos^.Y;
        end;
        SearchResultsView.AddMatch(SearchPageIndex,
                                   CodePos^.Code.Filename,
                                   Point(CodePos^.X,CodePos^.Y),
                                   Point(CodePos^.X+Len,CodePos^.Y),
                                   TrimmedLine,
                                   CodePos^.X-TrimCnt+Drift, Len);
      end;
    end;
  end;
  if (FLFMReferences<>nil) and (FLFMReferences.Count>0) then begin
    if RenameTo<>'' then
      Len:=Length(RenameTo)
    else
      Len:=Length(FIdentifier);
    Tree:=CreateTreeOfPCodeXYPosition;
    for i:=0 to FLFMReferences.Count-1 do
      Tree.Add(FLFMReferences.Items[i]);
    ANode:=Tree.FindHighest;
    while ANode<>nil do begin
      CodePos:=PCodeXYPosition(ANode.Data);
      ANode:=Tree.FindPrecessor(ANode);
      CurLine:=TrimRight(CodePos^.Code.GetLine(CodePos^.Y-1,false));
      TrimmedLine:=Trim(CurLine);
      TrimCnt:=length(CurLine)-length(TrimmedLine);
      SearchResultsView.AddMatch(SearchPageIndex,
                                 CodePos^.Code.Filename,
                                 Point(CodePos^.X,CodePos^.Y),
                                 Point(CodePos^.X+Len,CodePos^.Y),
                                 TrimmedLine,
                                 CodePos^.X-TrimCnt, Len); // no drift expected
    end;
    Tree.Free;
  end;
  SearchResultsView.EndUpdate(SearchPageIndex);
end;

procedure TFindRenameIdentSeeker.CheckDeclOfDesigner;
var
  UnitInfo: TUnitInfo;
  Node, ParentNode: TCodeTreeNode;
begin
  // Note:
  // When a designer is open, the UnitInfo.Component is the root component
  // The IDE also opens all needed ancestors and frames.

  UnitInfo:=Project1.UnitWithFilename(FDeclTool.MainFilename);
  if (UnitInfo<>nil) and (UnitInfo.Component<>nil) then begin

    if FDeclNode.Desc=ctnProcedureHead then
      FDeclNode:=FDeclNode.Parent;
    ParentNode:=FDeclNode.Parent;
    if FDeclNode.Desc=ctnVarDefinition then begin
      if (ParentNode.Desc in AllMainSections)
      and SameText(FIdentifier, UnitInfo.Component.Name) then
      begin
        // renaming a designer root component
        // todo
        exit;
      end;

      if (ParentNode.Desc=ctnClassPublished) and (ParentNode.Parent.Desc=ctnClass) then
      begin
        Node:=ParentNode.Parent.Parent;
        if NodeIsRootClassComponent(Node,UnitInfo.Component) then begin
          // renaming a published field of the designer component class
          // todo
        end;
      end;
    end else if FDeclNode.Desc=ctnProcedure then begin
      if (ParentNode.Desc=ctnClassPublished) and (ParentNode.Parent.Desc=ctnClass) then
      begin
        Node:=ParentNode.Parent.Parent;
        if NodeIsRootClassComponent(Node,UnitInfo.Component) then begin
          // renaming a published method of the designer component class
          // todo
        end;
      end;
    end;
  end else begin
    //
  end;
end;

function TFindRenameIdentSeeker.CheckUsesNode: boolean;
var
  InFilename, aUnitName, Dir, Filename: string;
  NewCode: TCodeBuffer;
  NewTool: TCodeTool;
begin
  case FDeclNode.Desc of
  ctnSrcName: begin
    FKind:=friSourceName;
    exit(true);
  end;
  ctnUseUnit: ;
  ctnUseUnitNamespace,ctnUseUnitClearName:
    FDeclNode:=FDeclNode.Parent;
  else
    exit(true);
  end;
  // renaming a uses -> rename the unit
  // Execute unit
  Result:=false;
  FKind:=friSourceName;
  aUnitName:=RemoveAmpersands(FDeclTool.ExtractUsedUnitName(FDeclNode,@InFilename));
  if aUnitName='' then begin
    Err(20250206143851,'ExtractUsedUnitName failed');
    exit;
  end;
  Dir:=ExtractFilePath(FDeclTool.MainFilename);
  Filename:=CodeToolBoss.DirectoryCachePool.FindUnitSourceInCompletePath(Dir,aUnitName,InFilename);
  if Filename='' then begin
    Err(20250206143916,'unit "'+aUnitName+'" not found');
    exit;
  end;
  // load unit
  NewCode:=CodeToolBoss.LoadFile(Filename,true,false);
  if NewCode=nil then begin
    Err(20250206143931,'unable to load file "'+Filename+'"');
    exit;
  end;
  // parse
  if not CodeToolBoss.Explore(NewCode,NewTool,true) then begin
    debugln(['Error: (lazarus) [20250206142339] DoFindRenameIdentifier CodeToolBoss.Explore failed']);
    LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;
  FDeclTool:=NewTool;
  FDeclCodeXY.Code:=NewCode;
  FDeclCodeXY.X:=1;
  FDeclCodeXY.Y:=1;
  FDeclNode:=FDeclTool.GetSourceNameNode;
  if FDeclNode=nil then begin
    Err(20250206144454,'failed to find unit name');
    exit;
  end;
  FDeclTool.CleanPosToCaret(FDeclNode.StartPos,FDeclCodeXY);
  Result:=true;
end;

function TFindRenameIdentSeeker.DeclarationCanBeInLFM: boolean;
begin
  Result:=false;
  if FDeclNode.HasParentOfType(ctnImplementation) then
    exit; // cant be referenced in lfm
  if FDeclNode.Desc=ctnProcedureHead then
    FDeclNode:=FDeclNode.Parent;
  case FDeclNode.Desc of
  ctnProperty:
    ; // even private properties can later be made published -> must be searched
  ctnVarDefinition:
    case FDeclNode.Parent.Desc of
    ctnClassPublic: ; // maybe possible due to $RTTI
    ctnClassPublished: ;
    else
      exit; // not a public field, e.g. a parameter or local var
    end;
  ctnProcedure:
    case FDeclNode.Parent.Desc of
    ctnClassPublic: ; // maybe possible due to $RTTI
    ctnClassPublished: ;
    ctnClassPrivate,ctnClassProtected,ctnClassRequired,ctnClassOptional:
      if FDeclTool.ProcNodeHasSpecifier(FDeclNode,psVirtual)
          or FDeclTool.ProcNodeHasSpecifier(FDeclNode,psOverride) then
        // an override could be published
      else
        exit;
    else
      exit; // not a method
    end;
  ctnTypeDefinition: ;
  ctnEnumIdentifier: ;
  else
    exit;
  end;
  Result:=true;
end;

procedure TFindRenameIdentSeeker.Err(id: int64; Msg: string);
begin
  Msg:='DoFindRenameIdentifier: '+Msg;
  debugln(['Error: ',FDeclCodeXY.Code.Filename,'(',FDeclCodeXY.Y,',',FDeclCodeXY.X,') [',id,'] ',Msg]);
  CodeToolBoss.SetError(id,FDeclCodeXY.Code,FDeclCodeXY.Y,FDeclCodeXY.X,Msg);
  LazarusIDE.DoJumpToCodeToolBossError;
end;

function TFindRenameIdentSeeker.GatherIdentifierReferences(Flags: TFindRefsFlags): boolean;
var             // Add to ModifiedDesigners UnitInfo which have modified designer.
  i: Integer;
  LoadResult: TModalResult;
  Code: TCodeBuffer;
  ListOfPCodeXYPosition: TFPList;
  Cache: TFindIdentifierReferenceCache;
  TreeOfPCodeXYPosition: TAVLTree;
  Refs: TSrcNameRefs;
  FileN: String;
  SrcEditor: TSourceEditorInterface;
begin
  Result:=false;
  FPascalReferences:=nil;
  ListOfPCodeXYPosition:=nil;
  TreeOfPCodeXYPosition:=nil;
  Cache:=nil;
  try
    CleanUpFileList(FTheFiles);
    for i:=FTheFiles.Count-1 downto 0 do begin
      FileN:=FTheFiles[i];
      if FilenameIsAbsolute(FileN) and not FileExistsCached(FileN) then
        FTheFiles.Delete(i);
    end;

    if FDeclNode=nil then
      if not UpdateCodeNode then
        exit;

    if FDeclNode.Desc=ctnSrcName then begin
      // search source name references
      if not CodeToolBoss.FindSourceNameReferences(FDeclCodeXY.Code.Filename,FTheFiles,
        not FOptions.SearchInComments,FPascalReferences) then
      begin
        debugln('GatherIdentifierReferences CodeToolBoss.FindSourceNameReferences failed');
        if CodeToolBoss.ErrorMessage='' then
          CodeToolBoss.SetError(20250206162241,FDeclCodeXY.Code,FDeclCodeXY.Y,FDeclCodeXY.X,'CodeToolBoss.FindSourceNameReferences failed');
        LazarusIDE.DoJumpToCodeToolBossError;
        exit;
      end;
    end else begin
      // search FIdentifier in every file
      for i:=0 to FTheFiles.Count-1 do begin
        //debugln(['GatherIdentifierReferences ',FTheFiles[i]]);
        LoadResult:=LoadCodeBuffer(Code,FTheFiles[i],
                        [lbfCheckIfText,lbfUpdateFromDisk,lbfIgnoreMissing],true);
        if LoadResult=mrAbort then begin
          debugln('GatherIdentifierReferences unable to load "',FTheFiles[i],'"');
          exit;
        end;
        if LoadResult<>mrOk then continue;

        // search references
        CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
        if not CodeToolBoss.FindReferences(
          FDeclCodeXY.Code,FDeclCodeXY.X,FDeclCodeXY.Y,
          Code, not FOptions.SearchInComments, ListOfPCodeXYPosition, Cache, Flags) then
        begin
          debugln('GatherIdentifierReferences CodeToolBoss.FindReferences failed in "',Code.Filename,'"');
          if CodeToolBoss.ErrorMessage='' then
            CodeToolBoss.SetError(20250206161149,Code,1,1,'CodeToolBoss.FindReferences failed');
          LazarusIDE.DoJumpToCodeToolBossError;
          exit;
        end;
        //debugln('GatherIdentifierReferences FindReferences in "',Code.Filename,'" ',dbgs(ListOfPCodeXYPosition<>nil));

        // add to tree
        if ListOfPCodeXYPosition<>nil then begin
          if TreeOfPCodeXYPosition=nil then
            TreeOfPCodeXYPosition:=CodeToolBoss.CreateTreeOfPCodeXYPosition;
          CodeToolBoss.AddListToTreeOfPCodeXYPosition(ListOfPCodeXYPosition,
                                                TreeOfPCodeXYPosition,true,false);
          SrcEditor:=SourceEditorManagerIntf.SourceEditorIntfWithFilename(FTheFiles[i]);
          if (SrcEditor<>nil) and (SrcEditor.ModifiedDesign) then begin
            if (frfIncludingLFM in Flags) and (frfRename in Flags) then begin
              FModifiedDesigners.Add(Project1.UnitWithFilename(FTheFiles[i]));
              debugln(['Added a unit modified by designer: ', Code.Scanner.SourceName]);
            end;
          end;
        end;
      end;
      if TreeOfPCodeXYPosition<>nil then begin
        FPascalReferences:=TObjectList.Create(true);
        Refs:=TSrcNameRefs.Create;
        Refs.TreeOfPCodeXYPosition:=TreeOfPCodeXYPosition;
        TreeOfPCodeXYPosition:=nil;
        //if FPascalReferences=nil then    This makes no sense.
        //  FPascalReferences:=TObjectList.Create(true);
        FPascalReferences.Add(Refs);
      end;
    end;

    Result:=true;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    CodeToolBoss.FreeTreeOfPCodeXYPosition(TreeOfPCodeXYPosition);
    Cache.Free;
  end;
end;

function TFindRenameIdentSeeker.GatherLFMsReferences(Flags: TFindRefsFlags): TModalResult;
var
  i: integer;
  LFMBuffer, Code: TCodeBuffer;
  UnitInfo: TUnitInfo;
  LFMFilename, Filename, DeclFilename: String;
  aCache: CodeToolsStructs.TPointerToPointerTree;
begin
  Result:=mrOk;
  if FTheFiles=nil then exit;
  FLfmReferences:=nil;
  if FDeclNode=nil then exit;
  if not (frfIncludingLFM in Flags) then exit;
  DeclFilename:=FDeclTool.MainFilename;
  if not FilenameIsPascalUnit(FDeclTool.MainFilename) then exit;

  if not DeclarationCanBeInLFM then exit;

  debugln(['GatherLFMsReferences Files.Count=',FTheFiles.Count]);
  // Note: this only supports lfm, not other form formats like dfm or fmx

  aCache:=CodeToolsStructs.TPointerToPointerTree.Create;
  try
    if frfRename in Flags then
      CheckDeclOfDesigner;

    // search in other lfm
    for i:=0 to FTheFiles.Count-1 do begin
      UnitInfo:=Project1.UnitWithFilename(FTheFiles[i]);
      if UnitInfo=nil then
        continue;

      Filename:=UnitInfo.Filename;
      if not FilenameIsAbsolute(Filename) then continue;
      if not FilenameIsPascalSource(Filename) then continue;
      LFMFilename:=ChangeFileExt(Filename,'.lfm');
      if not FileExistsCached(LFMFilename) then
        continue;

      // check if FDeclTool in unit path
      if not CodeToolBoss.IsUnitInUnitPath(Filename,DeclFilename,aCache) then
        continue;

      // load lfm source
      LFMBuffer:=CodeToolBoss.LoadFile(LFMFilename,true,false);
      if LFMBuffer=nil then continue;

      // check if identifier exists in lfm
      if PosI(FIdentifier, LFMBuffer.Source)<1 then
        continue;

      // parse lfm
      Code:=CodeToolBoss.LoadFile(Filename,true,false);
      if Code=nil then continue;

      CodeToolBoss.GatherReferencesInLFM(Code, LFMBuffer, FIdentifier,
        FDeclTool, FDeclNode, FLfmReferences, Flags);
    end;
  finally
    aCache.Free;
  end;
  Result:= mrOK;
end;

{$IFDEF EnableFPDocRename}
function TFindRenameIdentSeeker.GatherFPDocReferencesForPascalFiles(
  DeclarationCode: TCodeBuffer; DeclarationCaretXY: TPoint;
  var ListOfLazFPDocNode: TFPList): TModalResult;
var
  PascalFilenames, FPDocFilenames: TFilenameToStringTree;
  CacheWasUsed: boolean;
  Chain: TCodeHelpElementChain;
  CHResult: TCodeHelpParseResult;
  CHElement: TCodeHelpElement;
  FPDocFilename: String;
  S2SItem: PStringToStringItem;
begin
  Result:=mrCancel;
  PascalFilenames:=nil;
  FPDocFilenames:=nil;
  try
    // gather FPDoc files
    CleanUpFileList(FTheFiles);

    PascalFilenames:=TFilenameToStringTree.Create(false);
    PascalFilenames.AddNames(FTheFiles);
    CodeHelpBoss.GetFPDocFilenamesForSources(PascalFilenames,true,FPDocFilenames);
    if FPDocFilenames=nil then begin
      DebugLn(['GatherFPDocReferences no fpdoc files found']);
      exit(mrOk);
    end;

    // get codehelp element
    CHResult:=CodeHelpBoss.GetElementChain(DeclarationCode,
             DeclarationCaretXY.X,DeclarationCaretXY.Y,true,Chain,CacheWasUsed);
    if CHResult<>chprSuccess then begin
      DebugLn(['GatherFPDocReferences CodeHelpBoss.GetElementChain failed']);
      exit;
    end;
    CHElement:=Chain[0];
    DebugLn(['GatherFPDocReferences OwnerName=',CHElement.ElementOwnerName,' FPDocPkg=',CHElement.ElementFPDocPackageName,' Name=',CHElement.ElementName]);

    // search FPDoc files
    for S2SItem in FPDocFilenames do begin
      FPDocFilename:=S2SItem^.Name;
      Result:=GatherReferencesInFPDocFile(
                CHElement.ElementFPDocPackageName,CHElement.ElementUnitName,
                CHElement.ElementName,
                FPDocFilename,ListOfLazFPDocNode);
      if Result<>mrOk then exit;
    end;

    Result:=mrOk;
  finally
    PascalFilenames.Free;
    FPDocFilenames.Free;
    if Result<>mrOk then begin
      FreeListObjects(ListOfLazFPDocNode,true);
      ListOfLazFPDocNode:=nil;
    end;
  end;
end;

function TFindRenameIdentSeeker.GatherReferencesInFPDocFile(const OldPackageName,
  OldModuleName, OldElementName, FPDocFilename: string;
  var ListOfLazFPDocNode: TFPList): TModalResult;
var
  DocFile: TLazFPDocFile;
  IsSamePackage: Boolean;
  IsSameModule: Boolean;// = same unit

  procedure CheckLink(Node: TDOMNode; Link: string);
  var
    p: LongInt;
    PackageName: String;
  begin
    if Link='' then exit;
    if Link[1]='#' then begin
      p:=System.Pos('.',Link);
      if p<1 then exit;
      PackageName:=copy(Link,2,p-2);
      if SysUtils.CompareText(PackageName,OldPackageName)<>0 then exit;
      delete(Link,1,p);
    end;
    if (SysUtils.CompareText(Link,OldElementName)=0)
    or (SysUtils.CompareText(Link,OldModuleName+'.'+OldElementName)=0) then
    begin
      DebugLn(['CheckLink Found: ',Link]);
      if ListOfLazFPDocNode=nil then
        ListOfLazFPDocNode:=TFPList.Create;
      ListOfLazFPDocNode.Add(TLazFPDocNode.Create(DocFile,Node));
    end;
  end;

  procedure SearchLinksInChildNodes(Node: TDomNode);
  // search recursively for links
  begin
    Node:=Node.FirstChild;
    while Node<>nil do begin
      if (Node.NodeName='link')
      and (Node is TDomElement) then begin
        CheckLink(Node,TDomElement(Node).GetAttribute('id'));
      end;
      SearchLinksInChildNodes(Node);
      Node:=Node.NextSibling;
    end;
  end;

var
  CHResult: TCodeHelpParseResult;
  CacheWasUsed: boolean;
  Node: TDOMNode;
begin
  Result:=mrCancel;
  DebugLn(['GatherFPDocReferences ',
    ' OldPackageName=',OldPackageName,
    ' OldModuleName=',OldModuleName,' OldElementName=',OldElementName,
    ' FPDocFilename=',FPDocFilename]);

  CHResult:=CodeHelpBoss.LoadFPDocFile(FPDocFilename,[chofUpdateFromDisk],
                                       DocFile,CacheWasUsed);
  if CHResult<>chprSuccess then begin
    DebugLn(['GatherReferencesInFPDocFile CodeHelpBoss.LoadFPDocFile failed File=',FPDocFilename]);
    exit(mrCancel);
  end;

  // search in Doc nodes
  IsSamePackage:=SysUtils.CompareText(DocFile.GetPackageName,OldPackageName)=0;
  IsSameModule:=SysUtils.CompareText(DocFile.GetModuleName,OldModuleName)=0;
  DebugLn(['GatherReferencesInFPDocFile ',DocFile.GetPackageName,'=',OldPackageName,' ',DocFile.GetModuleName,'=',OldModuleName]);
  Node:=DocFile.GetFirstElement;
  while Node<>nil do begin
    if Node is TDomElement then begin
      if (SysUtils.CompareText(TDomElement(Node).GetAttribute('name'),OldElementName)=0)
      and IsSamePackage and IsSameModule
      then begin
        // this is the element itself
        DebugLn(['GatherReferencesInFPDocFile Element itself found: ',Node.NodeName,' ',Node.NodeValue]);
        if ListOfLazFPDocNode=nil then
          ListOfLazFPDocNode:=TFPList.Create;
        ListOfLazFPDocNode.Add(TLazFPDocNode.Create(DocFile,Node));
      end;
      CheckLink(Node,TDomElement(Node).GetAttribute('link'));
      SearchLinksInChildNodes(Node);
    end;
    Node:=Node.NextSibling;
  end;

  Result:=mrOk;
end;
{$ENDIF}

function TFindRenameIdentSeeker.Initialize: TModalResult;
begin
  Result:=mrCancel;
  if not LazarusIDE.BeginCodeTools then exit;
  FStartSrcEdit:=SourceEditorManagerIntf.ActiveEditor;
  FStartSrcCode:=TCodeBuffer(FStartSrcEdit.CodeToolsBuffer);
  // Execute the main declaration
  FStartCaretXY:=FStartSrcEdit.CursorTextXY;
  if not CodeToolBoss.FindMainDeclaration(FStartSrcCode,
    FStartCaretXY.X, FStartCaretXY.Y,
    FDeclCodeXY.Code, FDeclCodeXY.X, FDeclCodeXY.Y, FDeclTopLine) then
  begin
    LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;
  FDeclTool:=CodeToolBoss.FindCodeToolForSource(FStartSrcCode);
  if FDeclTool=nil then begin
    if FStartSrcCode.LastIncludedByFile<>'' then
      FDeclTool:=CodeToolBoss.FindCodeToolForSource(
                 CodeToolBoss.FindFile(FStartSrcCode.LastIncludedByFile));
    if FDeclTool=nil then
      exit;
  end;
  FKind:=friDeclaration;
  if FDeclTool.TruePredefinedResult or FDeclTool.TrueSelf then begin  // stay where you are
    FDeclCodeXY.X:=FStartCaretXY.X;
    FDeclCodeXY.Y:=FStartCaretXY.Y;
    FDeclCodeXY.Code:=FStartSrcCode;
    FDeclTool.CaretToCleanPos(FDeclCodeXY,FDeclCleanPos);
    FDeclTopLine:=FDeclCodeXY.Y;
  end else begin
    if not UpdateCodeNode then exit;
    if not CheckUsesNode then exit; // also allows source name
  end;
  CodeToolBoss.GetIdentifierAt(FDeclCodeXY.Code,FDeclCodeXY.X,FDeclCodeXY.Y,FIdentifier);
  FDeclXY:=Point(FDeclCodeXY.X,FDeclCodeXY.Y);
  Result:=LazarusIDE.DoOpenFileAndJumpToPos(FDeclCodeXY.Code.Filename, FDeclXY,
    FDeclTopLine,-1,-1,[ofOnlyIfExists,ofRegularFile,ofDoNotLoadResource]);
end;

function TFindRenameIdentSeeker.NodeIsRootClassComponent(aNode: TCodeTreeNode;
  RootComp: TComponent): boolean;
var
  NodeName: String;
begin
  Result:=false;
  if aNode.Desc<>ctnTypeDefinition then exit;
  if not (ANode.Parent.Desc in AllMainSections) then exit;
  NodeName:=FDeclTool.ExtractIdentifier(aNode.StartPos);
  Result:=SameText(NodeName,RootComp.ClassName);
end;

function TFindRenameIdentSeeker.RenameAll: TModalResult;
var
  Refs: TSrcNameRefs;
  TreeOfPCodeXYPosition, LFMTreeOfPCodeXYPosition: TAVLTree;
  Code, LastCode: TCodeBuffer;
  AUnitInfo: TEditableUnitInfo;
  PasFilename: string;
  IsConflicted: Boolean;
  i, j: Integer;
begin
  // todo: check for conflicts and show user list (some checking already done)
  IsConflicted:=false;
  Result:=mrOk;
  if FKind=friSourceName then begin
    FPascalReferences:=nil;
    if not CodeToolBoss.RenameSourceNameReferences(FOldFileName,FNewFilename,
        FOptions.RenameTo,FPascalReferences) then
      Result:=mrCancel;
  end else begin
    if (FPascalReferences<>nil) and (FPascalReferences.Count>0) then begin
      Refs:=TSrcNameRefs(FPascalReferences[0]);
      TreeOfPCodeXYPosition:=Refs.TreeOfPCodeXYPosition;
      if not CodeToolBoss.RenameIdentifier(TreeOfPCodeXYPosition,
          FIdentifier, FOptions.RenameTo, FDeclCodeXY.Code, @FDeclXY) then
        Result:=mrCancel;
    end;
    LFMTreeOfPCodeXYPosition:=nil;
    if (FLfmReferences<>nil) and (FLfmReferences.Count>0) then begin
      try
        LFMTreeOfPCodeXYPosition:=CreateTreeOfPCodeXYPosition;
        for i:=0 to FLfmReferences.Count-1 do
          LFMTreeOfPCodeXYPosition.Add(FLfmReferences.Items[i]);

        if not CodeToolBoss.RenameIdentifierInLFMs(LFMTreeOfPCodeXYPosition,
          FIdentifier, FOptions.RenameTo) then begin
          // error occured, show something
          Result:=mrCancel;
        end;
      finally
        LFMTreeOfPCodeXYPosition.Free;
      end;
    end;
  end;
  if Result<>mrOk then begin
    if IsConflicted then
      IDEMessageDialog(lisRenamingConflict,
        Format(lisIdentifierIsAlreadyUsed2,[FOptions.RenameTo]),
        mtError,[mbOK])
    else
      LazarusIDE.DoJumpToCodeToolBossError;
      debugln('Error: (lazarus) DoFindRenameIdentifier unable to commit');
    exit(mrCancel);
  end;
  // ToDo: rename fpdoc references
  // hack designers
  if FLfmReferences<>nil then begin
    LastCode:=nil;
    for i:=0 to FLfmReferences.Count-1 do begin
      Code:=FLfmReferences.Items[i]^.Code;
      if (Code<>LastCode) then begin
        LastCode:=Code;
        // hack LastCode related designers
        AUnitInfo:=nil;
        for j:=low(PascalSourceExt) to high(PascalSourceExt) do begin
          PasFilename:=ExtractFileNameWithoutExt(Code.Filename)+PascalSourceExt[j];
          AUnitInfo:=TEditableUnitInfo(Project1.UnitWithFilename(PasFilename));
          if AUnitInfo<>nil then
            break;
        end;
        if AUnitInfo=nil then
          continue;
        if FModifiedDesigners.IndexOf(AUnitInfo)>=0 then begin
          if AUnitInfo.EditorInfoCount>1 then
            for j:= AUnitInfo.EditorInfoCount-1 downto 1 do
              CloseEditorFile(AUnitInfo.EditorInfo[j].EditorComponent,
                              [cfQuiet, cfCloseDependencies]);
        end;
        ReloadUnitComponent(AUnitInfo);
      end;
    end;
  end;
end;

function TFindRenameIdentSeeker.RenameIdentifier_UnitFile(out OldRefs: TSrcNameRefs): TModalResult;
var
  anUnitInfo: TEditableUnitInfo;
  LFMCode, LRSCode, OldCode, NewCode: TCodeBuffer;
  i: Integer;
  Refs: TSrcNameRefs;
  Tool: TCodeTool;
begin
  Result:=mrOk;
  OldRefs:=nil;
  if (CompareFilenames(FOldFileName,FNewFileName)=0)
      and (ExtractFileName(FOldFileName)=ExtractFilename(FNewFilename)) then exit;
  anUnitInfo:=nil;
  if Assigned(Project1) then
    anUnitInfo:=TEditableUnitInfo(Project1.UnitWithFilename(FOldFileName));
  if anUnitInfo=nil then begin
    debugln(['Error: RenameIdentifier_UnitFile missing unitinfo "',FOldFileName,'"']);
    exit(mrCancel);
  end;

  OldCode:=anUnitInfo.Source;
  if FPascalReferences<>nil then begin
    for i:=0 to FPascalReferences.Count-1 do begin
      Refs:=TSrcNameRefs(FPascalReferences[i]);
      if Refs.Tool.Scanner.MainCode=OldCode then begin
        OldRefs:=Refs;
        FPascalReferences.OwnsObjects:=false;
        FPascalReferences.Delete(i);
        FPascalReferences.OwnsObjects:=true;
        break;
      end;
    end;
  end;

  LFMCode:=nil;
  LRSCode:=nil;
  Result:=RenameUnit(anUnitInfo, FNewFilename, FOptions.RenameTo, LFMCode, LRSCode, true);

  if FOptions.RenameShowResult and (OldRefs<>nil) then begin
    NewCode:=anUnitInfo.Source;
    CodeToolBoss.Explore(NewCode,Tool,true);
    OldRefs.Tool:=Tool;
    OldRefs.NewLocalSrcName:=FOptions.RenameTo;
    ReplaceCodeInTreeOfPCodeXYPosition(OldRefs.TreeOfPCodeXYPosition,OldCode,NewCode);
  end;
end;

function TFindRenameIdentSeeker.RenameProgram: TModalResult;
// rename unit/program
var
  MovingFile, DoLowercase: Boolean;
  OldPath, OldFN, RenE: string;
begin
  Result:=mrOK;
  FOldFileName:=FDeclCodeXY.Code.Filename;
  OldPath:=ExtractFilePath(FOldFileName);
  OldFN:=ExtractFileName(FOldFileName);
  RenE:=RemoveAmpersands(FOptions.RenameTo)+ExtractFileExt(FOldFileName);
  DoLowercase:=false;
  if FOptions.RenameTo<>lowercase(FOptions.RenameTo) then begin
    // new Identifier is not lowercase
    case EnvironmentOptions.CharcaseFileAction of
    ccfaAsk:
      begin
        // If old unitname is mixed case and old file lowercase, no need to ask
        if IsLower(FIdentifier) or not IsLower(OldFN) then
        begin
          Result:=IDEQuestionDialog(lisFileNotLowercase,
            Format(lisTheUnitIsNotLowercaseTheFreePascalCompiler,
                   [RenE, LineEnding, LineEnding+LineEnding]),
            mtConfirmation,[mrYes,mrNo,mrCancel],'');
          case Result of
          mrYes: DoLowercase:=true;
          mrNo: ;
          else
            exit(mrCancel);
          end;
        end;
      end;
    ccfaAutoRename:
      // always lower case
      DoLowercase:=true;
    else
      // use mixed case for filename
    end;
  end;
  if DoLowercase then
    RenE:=LowerCase(RenE);
  FNewFilename:=OldPath+RenE;
  // Check if new file already exists (change in case is silently done)
  MovingFile:=CompareFilenames(OldPath,ExtractFilePath(FNewFilename))<>0;
  FRenamingFile:=MovingFile or (RenE<>OldFN);
  if (MovingFile or not SameText(RenE,OldFN))
  and CodeToolBoss.DirectoryCachePool.FileExists(FNewFilename,ctsfcAllCase)
  then begin
    IDEMessageDialog(lisRenamingAborted,
      Format(lisFileAlreadyExists,[FindDiskFilename(FNewFilename)]),
      mtError,[mbOK]);
    exit(mrCancel);
  end;
end;

function TFindRenameIdentSeeker.ShowIdentifierReferences(DeclFilename: string;
  RenameTo: string): TModalResult;
var
  OldSearchPageIndex: TTabSheet;
  SearchPageIndex: TTabSheet;
  lOptions: TLazFindInFileSearchOptions;
begin
  if (FPascalReferences=nil) or (FPascalReferences.Count=0) then exit(mrOk);

  Result:=mrCancel;
  LazarusIDE.DoShowSearchResultsView(iwgfShow);
  SearchPageIndex:=nil;
  try
    // create a search result page
    //debugln(['ShowIdentifierReferences ',DbgSName(SearchResultsView)]);
    if RenameTo = '' then
      lOptions := []
    else
      lOptions := [fifReplace];

    SearchPageIndex:=SearchResultsView.AddSearch(FIdentifier, RenameTo,
      ExtractFilePath(DeclFilename), '*.pas;*.pp;*.p;*.inc', lOptions);
    if SearchPageIndex = nil then exit;

    // list results
    SearchResultsView.BeginUpdate(SearchPageIndex.PageIndex);
    AddReferencesToResultView(SearchPageIndex.PageIndex, RenameTo);

    OldSearchPageIndex:=SearchPageIndex;
    SearchPageIndex:=nil;
    SearchResultsView.EndUpdate(OldSearchPageIndex.PageIndex, 'Ref: '+FIdentifier);
    IDEWindowCreators.ShowForm(SearchResultsView,true);
  finally
    if SearchPageIndex <> nil then
      SearchResultsView.EndUpdate(SearchPageIndex.PageIndex, 'Ref: '+FIdentifier);
  end;
  Result:=mrOK;
end;

function TFindRenameIdentSeeker.UpdateCodeNode: boolean;
// Update FDeclTool, FDeclNode and FDeclCleanPos
begin
  Result:=false;
  FDeclTool:=nil;
  FDeclNode:=nil;
  if FDeclCodeXY.Code=nil then exit;
  CodeToolBoss.Explore(FDeclCodeXY.Code,FDeclTool,false);
  if FDeclTool=nil then begin
    debugln(['Error: (lazarus) [20250206142319] DoFindRenameIdentifier CodeToolBoss.Explore failed']);
    LazarusIDE.DoJumpToCodeToolBossError;
    exit;
  end;
  if FDeclTool.CaretToCleanPos(FDeclCodeXY,FDeclCleanPos)<>0 then begin
    Err(20250206143746,'position not in Pascal');
    exit;
  end;
  FDeclNode:=FDeclTool.FindDeepestNodeAtPos(FDeclCleanPos,false);
  if FDeclNode=nil then begin
    Err(20250206143807,'no Pascal node');
    exit;
  end;
  if (FDeclNode.Desc=ctnIdentifier)
  and (FDeclNode.Parent.Desc in [ctnSrcName,ctnUseUnitClearName,ctnUseUnitNamespace])
  then
    FDeclNode:=FDeclNode.Parent;
  Result:=true;
end;

function TFindRenameIdentSeeker.Execute(AllowRename, SetRenameActive: boolean): TModalResult;
var
  StartTopLine, i: integer;
  OwnerList, ListOfLazFPDocNode: TFPList;
  OldChange, NewFileCreated: Boolean;
  FindRefFlags: TFindRefsFlags;
  OldRefs: TSrcNameRefs;
  AUnitInfo: TEditableUnitInfo;
begin
  StartTopLine:=FStartSrcEdit.TopLine;
  FTheFiles:=nil;
  OwnerList:=nil;
  FPascalReferences:=nil;
  FLfmReferences:=nil;
  ListOfLazFPDocNode:=nil;
  FNewFilename:='';
  NewFileCreated:=false;
  OldRefs:=nil;
  FModifiedDesigners:=TFPList.Create;
  try
    // let user choose the search scope
    Result:=ShowFindRenameIdentifierDialog(FDeclCodeXY.Code.Filename, FDeclXY,
      AllowRename, SetRenameActive, FKind);
    if Result<>mrOk then begin
      debugln('Error: (lazarus) DoFindRenameIdentifier failed: user cancelled dialog');
      exit;
    end;

    FOptions:=MiscellaneousOptions.FindRenameIdentifierOptions;
    if FOptions.Rename and (FKind=friSourceName) then begin
      Result:=RenameProgram;  // rename unit/program
      if Result<>mrOK then exit;
    end;

    if not UpdateCodeNode then exit(mrCancel);

    // create the file list
    FTheFiles:=TStringList.Create;
    if (FOptions.Scope = frCurrentUnit) then begin
      FTheFiles.Add(FStartSrcCode.Filename);
    end else begin
      FTheFiles.Add(FDeclCodeXY.Code.Filename);
      if CompareFilenames(FDeclCodeXY.Code.Filename,FStartSrcCode.Filename)<>0 then
        FTheFiles.Add(FStartSrcCode.Filename);
    end;

    // add packages, projects
    case FOptions.Scope of
    frProject:
      begin
        OwnerList:=TFPList.Create;
        OwnerList.Add(LazarusIDE.ActiveProject);
      end;
    frOwnerProjectPackage,frAllOpenProjectsAndPackages:
      begin
        OwnerList:=PackageEditingInterface.GetOwnersOfUnit(FStartSrcCode.Filename);
        if (OwnerList<>nil) and (OwnerList.Count=0) then
          FreeAndNil(OwnerList);
        if (OwnerList=nil) then
          OwnerList:=PackageEditingInterface.GetPossibleOwnersOfUnit(
            FStartSrcCode.Filename,[piosfExcludeOwned,piosfIncludeSourceDirectories]);
        if (OwnerList<>nil) and (OwnerList.Count=0) then
          FreeAndNil(OwnerList);
        if (OwnerList<>nil) then begin
          if FOptions.Scope=frAllOpenProjectsAndPackages then begin
            PackageEditingInterface.ExtendOwnerListWithUsedByOwners(OwnerList);
            ReverseList(OwnerList);
          end;
        end else begin
          // unknown unit -> search everywhere
          OwnerList:=TFPList.Create;
          OwnerList.Add(LazarusIDE.ActiveProject);
          for i:=0 to PackageEditingInterface.GetPackageCount-1 do
            OwnerList.Add(PackageEditingInterface.GetPackages(i));
          ReverseList(OwnerList);
        end;
      end;
    end;

    // get source files of packages and projects
    if OwnerList<>nil then
      AddOwnerFiles(OwnerList);
    //debugln(['DoFindRenameIdentifier ',FTheFiles.Text]);

    // add user defined extra files
    if not AddExtraFiles then
      exit(mrCancel);

    // search pascal source references
    FindRefFlags:=[];
    if FOptions.Rename then
      Include(FindRefFlags,frfRename);
    if FDeclTool.TruePredefinedResult or FDeclTool.TrueSelf then
      Include(FindRefFlags,frfPredefinedIdentifiers);
    if FOptions.Overrides then
      Include(FindRefFlags,frfMethodOverrides);
    if FOptions.IncludeLFMs then
      Include(FindRefFlags,frfIncludingLFM);

    if not GatherIdentifierReferences(FindRefFlags) then
    begin
      debugln('Error: 20250206162727 DoFindRenameIdentifier GatherIdentifierReferences failed');
      exit(mrCancel);
    end;

    if FModifiedDesigners.Count>0 then begin    // pending changes in designers detected
      for i:=0 to FModifiedDesigners.Count-1 do begin
        AUnitInfo:=TEditableUnitInfo(FModifiedDesigners[i]);
        Assert(Assigned(AUnitInfo), 'DoFindRenameIdentifier: AUnitInfo=Nil');
        if AUnitInfo.EditorInfoCount > 0 then
          SaveEditorFile(AUnitInfo.EditorInfo[0].EditorComponent, []);
      end;
      // code is modified, previous  gathering not reliable, must be repeated
      FreeAndNil(FPascalReferences);
      FModifiedDesigners.Clear;
      if not GatherIdentifierReferences(FindRefFlags) then
      begin
        debugln('Error: 20250206162727 DoFindRenameIdentifier GatherIdentifierReferences failed');
        exit(mrCancel);
      end;
    end;

    // search references in lfm files
    if (frfIncludingLFM in FindRefFlags) and (GatherLFMsReferences(FindRefFlags)<>mrOk) then
    begin
      debugln('Error: 20250506120810 DoFindRenameIdentifier GatherLFMsReferences failed');
      exit(mrCancel);
    end;

    {$IFDEF EnableFPDocRename}
    // search fpdoc references
    Result:=GatherFPDocReferencesForPascalFiles(FTheFiles,DeclarationUnitInfo.Source,
                                  DeclarationCaretXY,ListOfLazFPDocNode);
    if Result<>mrOk then begin
      debugln('Error: (lazarus) DoFindRenameIdentifier GatherFPDocReferences failed');
      exit;
    end;
    {$ENDIF}

    // ToDo: search i18n references
    // ToDo: search fpdoc references

    if FOptions.Rename then begin

      if FRenamingFile then begin
        // rename file, and associated lfm, res, etc,
        // keeping source editor and session data
        // rename source name in this file
        // -> extract the references (OldRefs) for this file to show them later
        Result:=RenameIdentifier_UnitFile(OldRefs);
        if Result<>mrOk then
          exit(mrCancel);

        FDeclCodeXY.Code:=CodeToolBoss.LoadFile(FNewFilename,false,false);
        NewFileCreated:=true;
      end;

      // rename FIdentifier
      OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
      LazarusIDE.OpenEditorsOnCodeToolChange:=true;
      try
        Result:=RenameAll;
        if Result<>mrOk then exit;
      finally
        LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
      end;

      if FOptions.RenameShowResult then begin
        if OldRefs<>nil then begin
          // re-add the references
          //debugln(['DoFindRenameIdentifier NewRefs: MainFilename="',OldRefs.Tool.MainFilename,'" NewName="',OldRefs.NewLocalSrcName,'"']);
          FPascalReferences.Insert(0,OldRefs);
          OldRefs:=nil;
        end;
        Result:=ShowIdentifierReferences(FDeclCodeXY.Code.Filename, FOptions.RenameTo);
      end;

    end
    else  //no renaming, only references - always shown
      Result:=ShowIdentifierReferences(FDeclCodeXY.Code.Filename, '');

  finally
    FModifiedDesigners.Free;
    OldRefs.Free;
    FTheFiles.Free;
    OwnerList.Free;
    FPascalReferences.Free;
    FLfmReferences.Free;
    FreeListObjects(ListOfLazFPDocNode,true);

    if FRenamingFile and NewFileCreated then
      // source renamed -> jump to new file
      Result:=LazarusIDE.DoOpenFileAndJumpToPos(FNewFilename, FDeclXY,
        StartTopLine,-1,-1,[ofOnlyIfExists,ofRegularFile,ofDoNotLoadResource])
    else
      // jump back to where user started
      Result:=LazarusIDE.DoOpenFileAndJumpToPos(FStartSrcCode.Filename, FStartCaretXY,
        StartTopLine,-1,-1,[ofOnlyIfExists,ofRegularFile,ofDoNotLoadResource]);
  end;
end;

end.

