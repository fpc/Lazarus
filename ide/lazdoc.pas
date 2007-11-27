{
/***************************************************************************
                               LazDoc.pas
                               ----------

 ***************************************************************************/

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
}
unit LazDoc;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil,
  CodeAtom, CodeTree, CodeToolManager, FindDeclarationTool, BasicCodeTools,
  CodeCache, CacheCodeTools,
  FileProcs, AvgLvlTree,
  Laz_DOM, Laz_XMLRead, Laz_XMLWrite,
  MacroIntf, PackageIntf, LazHelpIntf, ProjectIntf, LazIDEIntf,
  CompilerOptions, IDEProcs, PackageDefs, EnvironmentOpts;

type
  TFPDocItem = (
    fpdiShort,
    fpdiDescription,
    fpdiErrors,
    fpdiSeeAlso,
    fpdiExample
    );

  TFPDocNode = array [TFPDocItem] of String;
  
const
  FPDocItemNames: array[TFPDocItem] of shortstring = (
      'short',
      'descr',
      'errors',
      'seealso',
      'example'
    );

type

  { TLazFPDocFile }

  TLazFPDocFile = class
  public
    Filename: string;
    Doc: TXMLdocument;
    ChangeStep: integer;// the CodeBuffer.ChangeStep value, when Doc was build
    CodeBuffer: TCodeBuffer;
    destructor Destroy; override;
    function GetModuleNode: TDOMNode;
    function GetFirstElement: TDOMNode;
    function GetElementWithName(const ElementName: string): TDOMNode;
    function GetChildValuesAsString(Node: TDOMNode): String;
    function GetValuesFromNode(Node: TDOMNode): TFPDocNode;
  end;
  
  { TLDSourceToFPDocFile - cache item for source to FPDoc file mapping }

  TLDSourceToFPDocFile = class
  public
    SourceFilename: string;
    FPDocFilename: string;
    FPDocFilenameTimeStamp: integer;
  end;
  
  { TLazDocElement }

  TLazDocElement = class
  public
    CodeContext: TFindContext;
    CodeXYPos: TCodeXYPosition;
    ElementName: string;
    ElementNode: TDOMNode;
    FPDocFile: TLazFPDocFile;
  end;
  
  { TLazDocElementChain }

  TLazDocElementChain = class
  private
    FItems: TFPList; // list of TLazDocElement
    function GetCount: integer;
    function GetItems(Index: integer): TLazDocElement;
    function Add: TLazDocElement;
  public
    CodePos: TCodePosition;
    IDEChangeStep: integer;
    CodetoolsChangeStep: integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Items[Index: integer]: TLazDocElement read GetItems; default;
    property Count: integer read GetCount;
    function IsValid: boolean;
  end;
  
  TLazDocChangeEvent =
              procedure(Sender: TObject; LazDocFPFile: TLazFPDocFile) of object;
  
  TLazDocManagerHandler = (
    ldmhDocChanging,
    ldmhDocChanged
    );
    
  TLazDocParseResult = (
    ldprParsing, // means: done a small step, but not yet finished the job
    ldprFailed,
    ldprSuccess
    );
    
  { TLazDocManager }

  TLazDocManager = class
  private
    FDocs: TAvgLvlTree;// tree of loaded TLazFPDocFile
    FHandlers: array[TLazDocManagerHandler] of TMethodList;
    FSrcToDocMap: TAvgLvlTree; // tree of TLDSourceToFPDocFile sorted for SourceFilename
    FDeclarationCache: TDeclarationInheritanceCache;
    procedure AddHandler(HandlerType: TLazDocManagerHandler;
                         const AMethod: TMethod; AsLast: boolean = false);
    procedure RemoveHandler(HandlerType: TLazDocManagerHandler;
                            const AMethod: TMethod);
    procedure CallDocChangeEvents(HandlerType: TLazDocManagerHandler;
                                  Doc: TLazFPDocFile);
  public
    constructor Create;
    destructor Destroy; override;
    procedure FreeDocs;
    procedure ClearSrcToDocMap;
    
    function FindFPDocFile(const Filename: string): TLazFPDocFile;
    function LoadFPDocFile(const Filename: string;
                           UpdateFromDisk, Revert: Boolean;
                           out ADocFile: TLazFPDocFile;
                           out CacheWasUsed: boolean): Boolean;
    function GetFPDocFilenameForHelpContext(
                                       Context: TPascalHelpContextList;
                                       out CacheWasUsed: boolean): string;
    function GetFPDocFilenameForSource(SrcFilename: string;
                                       ResolveIncludeFiles: Boolean;
                                       out CacheWasUsed: boolean): string;
    function CodeNodeToElementName(Tool: TFindDeclarationTool;
                                   CodeNode: TCodeTreeNode): string;
    function GetFPDocNode(Tool: TCodeTool; CodeNode: TCodeTreeNode; Complete: boolean;
                          out FPDocFile: TLazFPDocFile; out DOMNode: TDOMNode;
                          out CacheWasUsed: boolean): TLazDocParseResult;
    function GetDeclarationChain(Code: TCodeBuffer; X, Y: integer;
                                 out ListOfPCodeXYPosition: TFPList;
                                 out CacheWasUsed: boolean): TLazDocParseResult;
    function GetElementChain(Code: TCodeBuffer; X, Y: integer; Complete: boolean;
                             out Chain: TLazDocElementChain;
                             out CacheWasUsed: boolean): TLazDocParseResult;
    function GetHint(Code: TCodeBuffer; X, Y: integer; Complete: boolean;
                     out Hint: string;
                     out CacheWasUsed: boolean): TLazDocParseResult;
  public
    // Event lists
    procedure RemoveAllHandlersOfObject(AnObject: TObject);
    procedure AddHandlerOnChanging(const OnDocChangingEvent: TLazDocChangeEvent;
                                   AsLast: boolean = false);
    procedure RemoveHandlerOnChanging(const OnDocChangingEvent: TLazDocChangeEvent);
    procedure AddHandlerOnChanged(const OnDocChangedEvent: TLazDocChangeEvent;
                                  AsLast: boolean = false);
    procedure RemoveHandlerOnChanged(const OnDocChangedEvent: TLazDocChangeEvent);
  end;

var
  LazDocBoss: TLazDocManager = nil;// set by the IDE
  
function CompareLazFPDocFilenames(Data1, Data2: Pointer): integer;
function CompareAnsistringWithLazFPDocFile(Key, Data: Pointer): integer;
function CompareLDSrc2DocSrcFilenames(Data1, Data2: Pointer): integer;
function CompareAnsistringWithLDSrc2DocSrcFile(Key, Data: Pointer): integer;


implementation


function CompareLazFPDocFilenames(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(TLazFPDocFile(Data1).Filename,
                           TLazFPDocFile(Data2).Filename);
end;

function CompareAnsistringWithLazFPDocFile(Key, Data: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(Key),TLazFPDocFile(Data).Filename);
end;

function CompareLDSrc2DocSrcFilenames(Data1, Data2: Pointer): integer;
begin
  Result:=CompareFilenames(TLDSourceToFPDocFile(Data1).SourceFilename,
                           TLDSourceToFPDocFile(Data2).SourceFilename);
end;

function CompareAnsistringWithLDSrc2DocSrcFile(Key, Data: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(Key),TLDSourceToFPDocFile(Data).SourceFilename);
end;

{ TLazFPDocFile }

destructor TLazFPDocFile.Destroy;
begin
  FreeAndNil(Doc);
  inherited Destroy;
end;

function TLazFPDocFile.GetModuleNode: TDOMNode;
begin
  Result:=nil;
  if Doc=nil then exit;

  // get first node
  Result := Doc.FindNode('fpdoc-descriptions');
  if Result=nil then begin
    //DebugLn(['TLazFPDocFile.GetModuleNode fpdoc-descriptions not found']);
    exit;
  end;

  // proceed to package
  Result := Result.FindNode('package');
  if Result=nil then begin
    //DebugLn(['TLazFPDocFile.GetModuleNode fpdoc-descriptions has no package']);
    exit;
  end;

  // proceed to module
  Result := Result.FindNode('module');
end;

function TLazFPDocFile.GetFirstElement: TDOMNode;
begin
  //get first module node
  Result := GetModuleNode;
  //DebugLn(['TLazFPDocFile.GetFirstElement GetModuleNode=',GetModuleNode<>nil]);
  if Result=nil then exit;

  //proceed to element
  Result := Result.FirstChild;
  while Result.NodeName <> 'element' do
    Result := Result.NextSibling;
end;

function TLazFPDocFile.GetElementWithName(const ElementName: string): TDOMNode;
begin
  Result:=GetFirstElement;
  //DebugLn(['TLazFPDocFile.GetElementWithName ',ElementName,' GetFirstElement=',GetFirstElement<>nil]);
  while Result<>nil do begin
    //DebugLn(['TLazFPDocFile.GetElementWithName ',dbgsName(Result)]);
    //if Result is TDomElement then DebugLn(['TLazFPDocFile.GetElementWithName ',TDomElement(Result).GetAttribute('name')]);
    if (Result is TDomElement)
    and (SysUtils.CompareText(TDomElement(Result).GetAttribute('name'),ElementName)=0)
    then
      exit;
    Result:=Result.NextSibling;
  end;
end;

function TLazFPDocFile.GetChildValuesAsString(Node: TDOMNode): String;
var
  Child: TDOMNode;
begin
  Result:='';
  Child:=Node.FirstChild;
  while Child<>nil do begin
    //DebugLn(['TLazFPDocFile.GetChildValuesAsString ',dbgsName(Child)]);
    if Child is TDOMText then
      Result:=Result+TDOMText(Child).Data;
    Child:=Child.NextSibling;
  end;
end;

function TLazFPDocFile.GetValuesFromNode(Node: TDOMNode): TFPDocNode;
// simple function to return the values as string
var
  S: String;
begin
  Node := Node.FirstChild;
  while Assigned(Node) do
  begin
    if (Node.NodeType = ELEMENT_NODE) then
    begin
      S := Node.NodeName;

      if S = 'short' then
        Result[fpdiShort] := GetChildValuesAsString(Node);

      if S = 'descr' then
        Result[fpdiDescription] := GetChildValuesAsString(Node);

      if S = 'errors' then
        Result[fpdiErrors] := GetChildValuesAsString(Node);

      if S = 'seealso' then
        Result[fpdiSeeAlso] := GetChildValuesAsString(Node);

      if S = 'example' then begin
        Result[fpdiExample] := Node.Attributes.GetNamedItem('file').NodeValue;
      end;
    end;
    Node := Node.NextSibling;
  end;
end;

procedure TLazDocManager.AddHandler(HandlerType: TLazDocManagerHandler;
  const AMethod: TMethod; AsLast: boolean);
begin
  if FHandlers[HandlerType]=nil then
    FHandlers[HandlerType]:=TMethodList.Create;
  FHandlers[HandlerType].Add(AMethod);
end;

procedure TLazDocManager.RemoveHandler(HandlerType: TLazDocManagerHandler;
  const AMethod: TMethod);
begin
  FHandlers[HandlerType].Remove(AMethod);
end;

procedure TLazDocManager.CallDocChangeEvents(HandlerType: TLazDocManagerHandler;
  Doc: TLazFPDocFile);
var
  i: LongInt;
begin
  i:=FHandlers[HandlerType].Count;
  while FHandlers[HandlerType].NextDownIndex(i) do
    TLazDocChangeEvent(FHandlers[HandlerType].Items[i])(Self,Doc);
end;

constructor TLazDocManager.Create;
begin
  FDocs:=TAvgLvlTree.Create(@CompareLazFPDocFilenames);
  FSrcToDocMap:=TAvgLvlTree.Create(@CompareLDSrc2DocSrcFilenames);
  FDeclarationCache:=TDeclarationInheritanceCache.Create(
                                  @CodeToolBoss.FindDeclarationAndOverload,
                                  @CodeToolBoss.GetCodeTreeNodesDeletedStep);
end;

destructor TLazDocManager.Destroy;
begin
  ClearSrcToDocMap;
  FreeDocs;
  FreeAndNil(FDocs);
  FreeAndNil(FSrcToDocMap);
  FreeAndNil(FDeclarationCache);
  inherited Destroy;
end;

function TLazDocManager.FindFPDocFile(const Filename: string): TLazFPDocFile;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FDocs.FindKey(Pointer(Filename),@CompareAnsistringWithLazFPDocFile);
  if Node<>nil then
    Result:=TLazFPDocFile(Node.Data)
  else
    Result:=nil;
end;

function TLazDocManager.LoadFPDocFile(const Filename: string; UpdateFromDisk,
  Revert: Boolean; out ADocFile: TLazFPDocFile; out CacheWasUsed: boolean): Boolean;
var
  MemStream: TMemoryStream;
begin
  Result:=false;
  CacheWasUsed:=true;
  ADocFile:=FindFPDocFile(Filename);
  if ADocFile=nil then begin
    ADocFile:=TLazFPDocFile.Create;
    ADocFile.Filename:=Filename;
    FDocs.Add(ADocFile);
  end;
  ADocFile.CodeBuffer:=CodeToolBoss.LoadFile(Filename,UpdateFromDisk,Revert);
  if ADocFile.CodeBuffer=nil then begin
    DebugLn(['TLazDocForm.LoadFPDocFile unable to load "',Filename,'"']);
    FreeAndNil(ADocFile.Doc);
    exit;
  end;
  if (ADocFile.Doc<>nil)
  and (ADocFile.ChangeStep=ADocFile.CodeBuffer.ChangeStep)
  then begin
    // no update needed
    exit(true);
  end;
  CacheWasUsed:=false;
  
  DebugLn(['TLazDocManager.LoadFPDocFile parsing ',ADocFile.Filename]);
  CallDocChangeEvents(ldmhDocChanging,ADocFile);

  // parse XML
  ADocFile.ChangeStep:=ADocFile.CodeBuffer.ChangeStep;
  FreeAndNil(ADocFile.Doc);

  MemStream:=TMemoryStream.Create;
  try
    ADocFile.CodeBuffer.SaveToStream(MemStream);
    MemStream.Position:=0;
    Result:=false;
    ReadXMLFile(ADocFile.Doc, MemStream);
    Result:=true;
  finally
    if not Result then
      FreeAndNil(ADocFile.Doc);
    MemStream.Free;
    CallDocChangeEvents(ldmhDocChanging,ADocFile);
  end;
end;

function TLazDocManager.GetFPDocFilenameForHelpContext(
  Context: TPascalHelpContextList; out CacheWasUsed: boolean): string;
var
  i: Integer;
  SrcFilename: String;
begin
  Result:='';
  CacheWasUsed:=true;
  if Context=nil then exit;
  for i:=0 to Context.Count-1 do begin
    if Context.Items[i].Descriptor<>pihcFilename then continue;
    SrcFilename:=Context.Items[i].Context;
    Result:=GetFPDocFilenameForSource(SrcFilename,true,CacheWasUsed);
    exit;
  end;
end;

function TLazDocManager.GetFPDocFilenameForSource(SrcFilename: string;
  ResolveIncludeFiles: Boolean; out CacheWasUsed: boolean): string;
var
  FPDocName: String;
  SearchPath: String;
  
  procedure AddSearchPath(Paths: string; const BaseDir: string);
  begin
    if Paths='' then exit;
    if not IDEMacros.CreateAbsoluteSearchPath(Paths,BaseDir) then exit;
    if Paths='' then exit;
    SearchPath:=SearchPath+';'+Paths;
  end;
  
  procedure CheckUnitOwners(CheckSourceDirectories: boolean);
  var
    PkgList: TFPList;
    i: Integer;
    APackage: TLazPackage;
    BaseDir: String;
    AProject: TLazProject;
  begin
    if not FilenameIsAbsolute(SrcFilename) then exit;
    
    if CheckSourceDirectories then begin
      PkgList:=PackageEditingInterface.GetOwnersOfUnit(SrcFilename);
    end else begin
      PkgList:=PackageEditingInterface.GetPossibleOwnersOfUnit(SrcFilename,[]);
    end;
    // get all packages owning the file
    if PkgList=nil then exit;
    try
      for i:=0 to PkgList.Count-1 do begin
        if TObject(PkgList[i]) is TLazProject then begin
          AProject:=TLazProject(PkgList[i]);
          if AProject.LazDocPaths='' then continue;
          BaseDir:=ExtractFilePath(AProject.ProjectInfoFile);
          if BaseDir='' then continue;
          // add lazdoc paths of project
          AddSearchPath(AProject.LazDocPaths,BaseDir);
        end else if TObject(PkgList[i]) is TLazPackage then begin
          APackage:=TLazPackage(PkgList[i]);
          if APackage.LazDocPaths='' then continue;
          BaseDir:=APackage.Directory;
          if BaseDir='' then continue;
          // add lazdoc paths of package
          AddSearchPath(APackage.LazDocPaths,BaseDir);
        end;
      end;
    finally
      PkgList.Free;
    end;
  end;
  
  procedure CheckIfInLazarus;
  var
    LazDir: String;
  begin
    if not FilenameIsAbsolute(SrcFilename) then exit;
    LazDir:=AppendPathDelim(EnvironmentOptions.LazarusDirectory);
    // check LCL
    if FileIsInPath(SrcFilename,LazDir+'lcl') then begin
      AddSearchPath(SetDirSeparators('docs/xml/lcl'),LazDir);
    end;
  end;

var
  CodeBuf: TCodeBuffer;
  AVLNode: TAvgLvlTreeNode;
  MapEntry: TLDSourceToFPDocFile;
begin
  Result:='';
  CacheWasUsed:=true;
  
  if ResolveIncludeFiles then begin
    CodeBuf:=CodeToolBoss.FindFile(SrcFilename);
    if CodeBuf<>nil then begin
      CodeBuf:=CodeToolBoss.GetMainCode(CodeBuf);
      if CodeBuf<>nil then begin
        SrcFilename:=CodeBuf.Filename;
      end;
    end;
  end;
  
  if not FilenameIsPascalSource(SrcFilename) then exit;
  
  // first try cache
  MapEntry:=nil;
  AVLNode:=FSrcToDocMap.FindKey(Pointer(SrcFilename),@CompareAnsistringWithLDSrc2DocSrcFile);
  if AVLNode<>nil then begin
    MapEntry:=TLDSourceToFPDocFile(AVLNode.Data);
    if MapEntry.FPDocFilenameTimeStamp=CompilerParseStamp then begin
      Result:=MapEntry.FPDocFilename;
      exit;
    end;
  end;
  CacheWasUsed:=false;
  
  DebugLn(['TLazDocManager.GetFPDocFilenameForSource searching SrcFilename=',SrcFilename]);

  // first check if the file is owned by any project/package
  SearchPath:='';
  CheckUnitOwners(false);
  CheckUnitOwners(true);
  CheckIfInLazarus;

  // finally add the default paths
  AddSearchPath(EnvironmentOptions.LazDocPaths,'');
  FPDocName:=lowercase(ExtractFileNameOnly(SrcFilename))+'.xml';
  DebugLn(['TLazDocManager.GetFPDocFilenameForSource Search ',FPDocName,' in "',SearchPath,'"']);
  Result:=SearchFileInPath(FPDocName,'',SearchPath,';',ctsfcAllCase);
  
  // save to cache
  if MapEntry=nil then begin
    MapEntry:=TLDSourceToFPDocFile.Create;
    MapEntry.SourceFilename:=SrcFilename;
    FSrcToDocMap.Add(MapEntry);
  end;
  MapEntry.FPDocFilename:=Result;
  MapEntry.FPDocFilenameTimeStamp:=CompilerParseStamp;
end;

function TLazDocManager.CodeNodeToElementName(Tool: TFindDeclarationTool;
  CodeNode: TCodeTreeNode): string;
var
  NodeName: String;
begin
  Result:='';
  while CodeNode<>nil do begin
    case CodeNode.Desc of
    ctnVarDefinition, ctnConstDefinition, ctnTypeDefinition, ctnGenericType:
      NodeName:=Tool.ExtractDefinitionName(CodeNode);
    ctnProperty:
      NodeName:=Tool.ExtractPropName(CodeNode,false);
    ctnProcedure:
      NodeName:=Tool.ExtractProcName(CodeNode,[]);
    else NodeName:='';
    end;
    if NodeName<>'' then begin
      if Result<>'' then
        Result:='.'+Result;
      Result:=NodeName+Result;
    end;
    CodeNode:=CodeNode.Parent;
  end;
end;

function TLazDocManager.GetFPDocNode(Tool: TCodeTool; CodeNode: TCodeTreeNode;
  Complete: boolean; out FPDocFile: TLazFPDocFile; out DOMNode: TDOMNode;
  out CacheWasUsed: boolean): TLazDocParseResult;
var
  SrcFilename: String;
  FPDocFilename: String;
  ElementName: String;
begin
  FPDocFile:=nil;
  DOMNode:=nil;
  CacheWasUsed:=true;
  
  // find corresponding FPDoc file
  SrcFilename:=Tool.MainFilename;
  FPDocFilename:=GetFPDocFilenameForSource(SrcFilename,false,CacheWasUsed);
  if FPDocFilename='' then exit(ldprFailed);
  if (not CacheWasUsed) and (not Complete) then exit(ldprParsing);

  // load FPDoc file
  if not LoadFPDocFile(FPDocFilename,true,false,FPDocFile,CacheWasUsed) then
    exit(ldprFailed);
  if (not CacheWasUsed) and (not Complete) then exit(ldprParsing);

  // find FPDoc node
  ElementName:=CodeNodeToElementName(Tool,CodeNode);
  if ElementName='' then exit(ldprFailed);
  DOMNode:=FPDocFile.GetElementWithName(ElementName);
  if DOMNode=nil then exit(ldprFailed);
  
  Result:=ldprSuccess;
end;

function TLazDocManager.GetDeclarationChain(Code: TCodeBuffer; X, Y: integer;
  out ListOfPCodeXYPosition: TFPList; out CacheWasUsed: boolean
  ): TLazDocParseResult;
begin
  if FDeclarationCache.FindDeclarations(Code,X,Y,ListOfPCodeXYPosition,
    CacheWasUsed)
  then
    Result:=ldprSuccess
  else
    Result:=ldprFailed;
end;

function TLazDocManager.GetElementChain(Code: TCodeBuffer; X, Y: integer;
  Complete: boolean; out Chain: TLazDocElementChain; out CacheWasUsed: boolean
  ): TLazDocParseResult;
var
  ListOfPCodeXYPosition: TFPList;
  i: Integer;
  CodePos: PCodeXYPosition;
  CurTool: TCodeTool;
  CleanPos: integer;
  LDElement: TLazDocElement;
  SrcFilename: String;
  FPDocFilename: String;
  Node: TCodeTreeNode;
begin
  Chain:=nil;
  ListOfPCodeXYPosition:=nil;
  try
    //DebugLn(['TLazDocManager.GetElementChain GetDeclarationChain...']);
    // get the declaration chain
    Result:=GetDeclarationChain(Code,X,Y,ListOfPCodeXYPosition,CacheWasUsed);
    if Result<>ldprSuccess then exit;
    if (not CacheWasUsed) and (not Complete) then exit(ldprParsing);
    
    DebugLn(['TLazDocManager.GetElementChain init the element chain: ListOfPCodeXYPosition.Count=',ListOfPCodeXYPosition.Count,' ...']);
    // init the element chain
    Result:=ldprParsing;
    Chain:=TLazDocElementChain.Create;
    Chain.CodePos.Code:=Code;
    Chain.IDEChangeStep:=CompilerParseStamp;
    Chain.CodetoolsChangeStep:=CodeToolBoss.CodeTreeNodesDeletedStep;
    Code.LineColToPosition(Y,X,Chain.CodePos.P);
    // fill the element chain
    for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
      // get source position of declaration
      CodePos:=PCodeXYPosition(ListOfPCodeXYPosition[i]);
      //DebugLn(['TLazDocManager.GetElementChain i=',i,' X=',CodePos^.X,' Y=',CodePos^.Y]);
      if (CodePos=nil) or (CodePos^.Code=nil) or (CodePos^.X<1) or (CodePos^.Y<1)
      then begin
        DebugLn(['TLazDocManager.GetElementChain i=',i,' invalid CodePos']);
        continue;
      end;

      // build CodeTree and find node
      if not CodeToolBoss.Explore(CodePos^.Code,CurTool,false,true) then begin
        DebugLn(['TLazDocManager.GetElementChain i=',i,' explore failed']);
        continue;
      end;
      if CurTool.CaretToCleanPos(CodePos^,CleanPos)<>0 then begin
        DebugLn(['TLazDocManager.GetElementChain i=',i,' invalid CodePos']);
        continue;
      end;
      
      Node:=CurTool.FindDeepestNodeAtPos(CleanPos,false);
      if Node=nil then begin
        DebugLn(['TLazDocManager.GetElementChain i=',i,' node not found']);
        continue;
      end;

      // use only definition nodes
      if (Node.Desc=ctnProcedureHead)
      and (Node.Parent<>nil) and (Node.Parent.Desc=ctnProcedure) then
        Node:=Node.Parent;
      if not (Node.Desc in
        (AllIdentifierDefinitions+[ctnProperty,ctnProcedure,ctnEnumIdentifier]))
      then begin
        DebugLn(['TLazDocManager.GetElementChain i=',i,' ignoring node ',Node.DescAsString]);
        continue;
      end;
      if (CurTool.NodeIsForwardDeclaration(Node)) then begin
        DebugLn(['TLazDocManager.GetElementChain i=',i,' ignoring forward']);
        continue;
      end;

      // add element
      LDElement:=Chain.Add;
      LDElement.CodeXYPos:=CodePos^;
      LDElement.CodeContext.Tool:=CurTool;
      LDElement.CodeContext.Node:=Node;
      //DebugLn(['TLazDocManager.GetElementChain i=',i,' CodeContext=',FindContextToString(LDElement.CodeContext)]);

      // find corresponding FPDoc file
      SrcFilename:=LDElement.CodeContext.Tool.MainFilename;
      FPDocFilename:=GetFPDocFilenameForSource(SrcFilename,false,CacheWasUsed);
      //DebugLn(['TLazDocManager.GetElementChain FPDocFilename=',FPDocFilename]);
      if (not CacheWasUsed) and (not Complete) then exit(ldprParsing);

      if FPDocFilename<>'' then begin
        // load FPDoc file
        LoadFPDocFile(FPDocFilename,true,false,LDElement.FPDocFile,
                      CacheWasUsed);
        if (not CacheWasUsed) and (not Complete) then exit(ldprParsing);
      end;
    end;
    
    // get fpdoc nodes
    for i:=0 to Chain.Count-1 do begin
      LDElement:=Chain[i];
      // get fpdoc element path
      LDElement.ElementName:=CodeNodeToElementName(LDElement.CodeContext.Tool,
                                                   LDElement.CodeContext.Node);
      //DebugLn(['TLazDocManager.GetElementChain i=',i,' Element=',LDElement.ElementName]);
      // get fpdoc node
      if (LDElement.FPDocFile<>nil) and (LDElement.ElementName<>'') then begin
        LDElement.ElementNode:=
                  LDElement.FPDocFile.GetElementWithName(LDElement.ElementName);
      end;
      //DebugLn(['TLazDocManager.GetElementChain ElementNode=',LDElement.ElementNode<>nil]);
    end;

    Result:=ldprSuccess;
  finally
    if Result<>ldprSuccess then
      FreeAndNil(Chain);
  end;
end;

function TLazDocManager.GetHint(Code: TCodeBuffer; X, Y: integer;
  Complete: boolean; out Hint: string; out CacheWasUsed: boolean
  ): TLazDocParseResult;
  
  function EndNow(var LastResult: TLazDocParseResult): boolean;
  begin
    if LastResult<>ldprSuccess then begin
      Result:=true;
      if Hint<>'' then
        LastResult:=ldprSuccess
      else
        LastResult:=ldprFailed;
      exit;
    end;
    if (not CacheWasUsed) and (not Complete) then begin
      Result:=true;
      LastResult:=ldprParsing;
    end;
    Result:=false;
  end;
  
var
  Chain: TLazDocElementChain;
  i: Integer;
  Item: TLazDocElement;
  NodeValues: TFPDocNode;
  f: TFPDocItem;
  ListOfPCodeXYPosition: TFPList;
  CodeXYPos: PCodeXYPosition;
  CommentStart: integer;
  NestedComments: Boolean;
  CommentStr: String;
  ItemAdded: Boolean;
  CommentCode: TCodeBuffer;
  j: Integer;
begin
  //DebugLn(['TLazDocManager.GetHint ',Code.Filename,' ',X,',',Y]);
  Hint:=CodeToolBoss.FindSmartHint(Code,X,Y);

  CacheWasUsed:=true;
  Chain:=nil;
  ListOfPCodeXYPosition:=nil;
  try
    //DebugLn(['TLazDocManager.GetHint GetElementChain...']);
    Result:=GetElementChain(Code,X,Y,Complete,Chain,CacheWasUsed);
    if EndNow(Result) then exit;

    if Chain<>nil then begin
      for i:=0 to Chain.Count-1 do begin
        Item:=Chain[i];
        ItemAdded:=false;
        DebugLn(['TLazDocManager.GetHint ',i,' Element=',Item.ElementName]);
        if Item.ElementNode<>nil then begin
          NodeValues:=Item.FPDocFile.GetValuesFromNode(Item.ElementNode);
          for f:=Low(TFPDocItem) to High(TFPDocItem) do
            DebugLn(['TLazDocManager.GetHint ',FPDocItemNames[f],' ',NodeValues[f]]);
          if NodeValues[fpdiShort]<>'' then begin
            Hint:=Hint+#13#13
                  +Item.ElementName+#13
                  +NodeValues[fpdiShort];
            ItemAdded:=true;
          end;
        end;
        
        // Add comments
        if CodeToolBoss.GetPasDocComments(Item.CodeXYPos.Code,
          Item.CodeXYPos.X,Item.CodeXYPos.Y,ListOfPCodeXYPosition)
        and (ListOfPCodeXYPosition<>nil) then
        begin
          NestedComments:=CodeToolBoss.GetNestedCommentsFlagForFile(
                                                  Item.CodeXYPos.Code.Filename);
          for j:=0 to ListOfPCodeXYPosition.Count-1 do begin
            CodeXYPos:=PCodeXYPosition(ListOfPCodeXYPosition[j]);
            CommentCode:=CodeXYPos^.Code;
            CommentCode.LineColToPosition(CodeXYPos^.Y,CodeXYPos^.X,CommentStart);
            if (CommentStart<1) or (CommentStart>CommentCode.SourceLength)
            then
              continue;
            CommentStr:=ExtractCommentContent(CommentCode.Source,CommentStart,
                                              NestedComments,true,true);
            if CommentStr<>'' then begin
              if not ItemAdded then begin
                Hint:=Hint+#13#13
                      +Item.ElementName+#13
                      +CommentStr;
              end else begin
                Hint:=Hint+#13
                      +CommentStr;
              end;
              ItemAdded:=true;
            end;
          end;
        end;
      end;
    end;
    Result:=ldprSuccess;
  finally
    FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
    Chain.Free;
  end;
  
  DebugLn(['TLazDocManager.GetHint END Hint="',Hint,'"']);
end;

procedure TLazDocManager.FreeDocs;
var
  AVLNode: TAvgLvlTreeNode;
begin
  AVLNode:=FDocs.FindLowest;
  while AVLNode<>nil do begin
    CallDocChangeEvents(ldmhDocChanging,TLazFPDocFile(AVLNode.Data));
    AVLNode:=FDocs.FindSuccessor(AVLNode);
  end;
  FDocs.FreeAndClear;
end;

procedure TLazDocManager.ClearSrcToDocMap;
begin
  FSrcToDocMap.FreeAndClear;
end;

procedure TLazDocManager.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TLazDocManagerHandler;
begin
  for HandlerType:=Low(TLazDocManagerHandler) to High(TLazDocManagerHandler) do
    FHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

procedure TLazDocManager.AddHandlerOnChanging(
  const OnDocChangingEvent: TLazDocChangeEvent; AsLast: boolean);
begin
  AddHandler(ldmhDocChanging,TMethod(OnDocChangingEvent),AsLast);
end;

procedure TLazDocManager.RemoveHandlerOnChanging(
  const OnDocChangingEvent: TLazDocChangeEvent);
begin
  RemoveHandler(ldmhDocChanging,TMethod(OnDocChangingEvent));
end;

procedure TLazDocManager.AddHandlerOnChanged(
  const OnDocChangedEvent: TLazDocChangeEvent; AsLast: boolean);
begin
  AddHandler(ldmhDocChanged,TMethod(OnDocChangedEvent),AsLast);
end;

procedure TLazDocManager.RemoveHandlerOnChanged(
  const OnDocChangedEvent: TLazDocChangeEvent);
begin
  RemoveHandler(ldmhDocChanged,TMethod(OnDocChangedEvent));
end;


{ TLazDocElementChain }

function TLazDocElementChain.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TLazDocElementChain.GetItems(Index: integer): TLazDocElement;
begin
  Result:=TLazDocElement(FItems[Index]);
end;

function TLazDocElementChain.Add: TLazDocElement;
begin
  Result:=TLazDocElement.Create;
  FItems.Add(Result);
end;

constructor TLazDocElementChain.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TLazDocElementChain.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TLazDocElementChain.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do TObject(FItems[i]).Free;
  FItems.Clear;
end;

function TLazDocElementChain.IsValid: boolean;
begin
  Result:=(IDEChangeStep=CompilerParseStamp)
    and (CodetoolsChangeStep=CodeToolBoss.CodeTreeNodesDeletedStep);
end;

end.

