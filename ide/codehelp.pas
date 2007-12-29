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
}
unit CodeHelp;

{$mode objfpc}{$H+}

{ $define VerboseLazDoc}

interface

uses
  Classes, SysUtils, LCLProc, Forms, Controls, FileUtil, Dialogs, AvgLvlTree,
  // codetools
  CodeAtom, CodeTree, CodeToolManager, FindDeclarationTool, BasicCodeTools,
  CodeCache, CacheCodeTools, FileProcs,
  Laz_DOM, Laz_XMLRead, Laz_XMLWrite,
  // IDEIntf
  MacroIntf, PackageIntf, LazHelpIntf, ProjectIntf, IDEDialogs, LazIDEIntf,
  // IDE
  CompilerOptions, IDEProcs, PackageDefs, EnvironmentOpts, DialogProcs;

type
  TFPDocItem = (
    fpdiShort,
    fpdiDescription,
    fpdiErrors,
    fpdiSeeAlso,
    fpdiExample
    );

  TFPDocElementValues = array [TFPDocItem] of String;
  
const
  FPDocItemNames: array[TFPDocItem] of shortstring = (
      'short',
      'descr',
      'errors',
      'seealso',
      'example'
    );

type

  TLazFPDocFileFlag = (
    ldffDocChangingCalled,
    ldffDocChangedNeedsCalling
    );
  TLazFPDocFileFlags = set of TLazFPDocFileFlag;

  { TLazFPDocFile }

  TLazFPDocFile = class
  private
    fUpdateLock: integer;
    FFlags: TLazFPDocFileFlags;
  public
    Filename: string;
    Doc: TXMLdocument;// IMPORTANT: if you change this, call DocChanging and DocChanged to notify the references
    DocModified: boolean;
    ChangeStep: integer;// the CodeBuffer.ChangeStep value, when Doc was build
    CodeBuffer: TCodeBuffer;
    destructor Destroy; override;
    function GetModuleNode: TDOMNode;
    function GetFirstElement: TDOMNode;
    function GetElementWithName(const ElementName: string;
                                CreateIfNotExists: boolean = false): TDOMNode;
    function GetChildValuesAsString(Node: TDOMNode): String;
    function GetValuesFromNode(Node: TDOMNode): TFPDocElementValues;
    function GetValueFromNode(Node: TDOMNode; Item: TFPDocItem): string;
    procedure SetChildValue(Node: TDOMNode; const ChildName: string; NewValue: string);
    procedure DocChanging;
    procedure DocChanged;
    procedure BeginUpdate;
    procedure EndUpdate;
  end;
  
  { TCHSourceToFPDocFile - cache item for source to FPDoc file mapping }

  TCHSourceToFPDocFile = class
  public
    SourceFilename: string;
    FPDocFilename: string;
    FPDocFilenameTimeStamp: integer;
    FilesTimeStamp: integer;
  end;
  
  { TCodeHelpElement }

  TCodeHelpElement = class
  public
    CodeContext: TFindContext;
    CodeXYPos: TCodeXYPosition;
    ElementName: string;
    ElementNode: TDOMNode;
    ElementNodeValid: boolean;
    FPDocFile: TLazFPDocFile;
  end;
  
  { TCodeHelpElementChain }

  TCodeHelpElementChain = class
  private
    FItems: TFPList; // list of TCodeHelpElement
    function GetCount: integer;
    function GetItems(Index: integer): TCodeHelpElement;
    function Add: TCodeHelpElement;
  public
    CodePos: TCodePosition;
    IDEChangeStep: integer;
    CodetoolsChangeStep: integer;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    property Items[Index: integer]: TCodeHelpElement read GetItems; default;
    property Count: integer read GetCount;
    function IndexOfFile(AFile: TLazFPDocFile): integer;
    function IsValid: boolean;
    procedure MakeValid;
    function DocFile: TLazFPDocFile;
  end;
  
  TCodeHelpChangeEvent = procedure(Sender: TObject; LazDocFPFile: TLazFPDocFile) of object;
  
  TCodeHelpManagerHandler = (
    chmhDocChanging,
    chmhDocChanged
    );
    
  TCodeHelpParseResult = (
    chprParsing, // means: done a small step, but not yet finished the job
    chprFailed,
    chprSuccess
    );
    
  { TLazDocManager }

  TCodeHelpManager = class
  private
    FDocs: TAvgLvlTree;// tree of loaded TLazFPDocFile
    FHandlers: array[TCodeHelpManagerHandler] of TMethodList;
    FSrcToDocMap: TAvgLvlTree; // tree of TCHSourceToFPDocFile sorted for SourceFilename
    FDeclarationCache: TDeclarationInheritanceCache;
    procedure AddHandler(HandlerType: TCodeHelpManagerHandler;
                         const AMethod: TMethod; AsLast: boolean = false);
    procedure RemoveHandler(HandlerType: TCodeHelpManagerHandler;
                            const AMethod: TMethod);
    procedure CallDocChangeEvents(HandlerType: TCodeHelpManagerHandler;
                                  Doc: TLazFPDocFile);
    function DoCreateFPDocFileForSource(const SrcFilename: string): string;
    function CreateFPDocFile(const ExpandedFilename, PackageName,
                             ModuleName: string): TCodeBuffer;
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
    function SaveFPDocFile(ADocFile: TLazFPDocFile): TModalResult;
    function GetFPDocFilenameForHelpContext(
                                       Context: TPascalHelpContextList;
                                       out CacheWasUsed: boolean): string;
    function GetFPDocFilenameForSource(SrcFilename: string;
                                       ResolveIncludeFiles: Boolean;
                                       out CacheWasUsed: boolean;
                                       CreateIfNotExists: boolean = false): string;
    function CodeNodeToElementName(Tool: TFindDeclarationTool;
                                   CodeNode: TCodeTreeNode): string;
    function GetFPDocNode(Tool: TCodeTool; CodeNode: TCodeTreeNode; Complete: boolean;
                          out FPDocFile: TLazFPDocFile; out DOMNode: TDOMNode;
                          out CacheWasUsed: boolean): TCodeHelpParseResult;
    function GetDeclarationChain(Code: TCodeBuffer; X, Y: integer;
                                 out ListOfPCodeXYPosition: TFPList;
                                 out CacheWasUsed: boolean): TCodeHelpParseResult;
    function GetCodeContext(CodePos: PCodeXYPosition;
                            out FindContext: TFindContext;
                            Complete: boolean;
                            out CacheWasUsed: boolean): TCodeHelpParseResult;
    function GetElementChain(Code: TCodeBuffer; X, Y: integer; Complete: boolean;
                             out Chain: TCodeHelpElementChain;
                             out CacheWasUsed: boolean): TCodeHelpParseResult;
    function GetHTMLHint(Code: TCodeBuffer; X, Y: integer; Complete: boolean;
                     out BaseURL, HTMLHint: string;
                     out CacheWasUsed: boolean): TCodeHelpParseResult;
    function CreateElement(Code: TCodeBuffer; X, Y: integer;
                           out Element: TCodeHelpElement): Boolean;
  public
    // Event lists
    procedure RemoveAllHandlersOfObject(AnObject: TObject);
    procedure AddHandlerOnChanging(const OnDocChangingEvent: TCodeHelpChangeEvent;
                                   AsLast: boolean = false);
    procedure RemoveHandlerOnChanging(const OnDocChangingEvent: TCodeHelpChangeEvent);
    procedure AddHandlerOnChanged(const OnDocChangedEvent: TCodeHelpChangeEvent;
                                  AsLast: boolean = false);
    procedure RemoveHandlerOnChanged(const OnDocChangedEvent: TCodeHelpChangeEvent);
  end;

var
  CodeHelpBoss: TCodeHelpManager = nil;// set by the IDE
  
function CompareLazFPDocFilenames(Data1, Data2: Pointer): integer;
function CompareAnsistringWithLazFPDocFile(Key, Data: Pointer): integer;
function CompareLDSrc2DocSrcFilenames(Data1, Data2: Pointer): integer;
function CompareAnsistringWithLDSrc2DocSrcFile(Key, Data: Pointer): integer;

function ToUnixLineEnding(const s: String): String;


implementation


function ToUnixLineEnding(const s: String): String;
var
  p: Integer;
begin
  Result:=s;
  p:=1;
  while (p<=length(s)) do begin
    if not (s[p] in [#10,#13]) then begin
      inc(p);
    end else begin
      // line ending
      if (p<length(s)) and (s[p+1] in [#10,#13]) and (s[p]<>s[p+1]) then begin
        // double character line ending
        Result:=copy(Result,1,p-1)+#10+copy(Result,p+2,length(Result));
      end else if s[p]=#13 then begin
        // single char line ending #13
        Result[p]:=#10;
      end;
      inc(p);
    end;
  end;
end;

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
  Result:=CompareFilenames(TCHSourceToFPDocFile(Data1).SourceFilename,
                           TCHSourceToFPDocFile(Data2).SourceFilename);
end;

function CompareAnsistringWithLDSrc2DocSrcFile(Key, Data: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(Key),TCHSourceToFPDocFile(Data).SourceFilename);
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
  while (Result<>nil) and (Result.NodeName <> 'element') do
    Result := Result.NextSibling;
end;

function TLazFPDocFile.GetElementWithName(const ElementName: string;
  CreateIfNotExists: boolean): TDOMNode;
var
  ModuleNode: TDOMNode;
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
  if (Result=nil) and CreateIfNotExists then begin
    DebugLn(['TLazFPDocFile.GetElementWithName creating ',ElementName]);
    ModuleNode:=GetModuleNode;
    if ModuleNode=nil then begin
      DebugLn(['TLazFPDocFile.GetElementWithName create failed: missing module name. ElementName=',ElementName]);
      exit;
    end;
    Result:=Doc.CreateElement('element');
    DocChanging;
    TDOMElement(Result).SetAttribute('name',ElementName);
    ModuleNode.AppendChild(Result);
    DocChanged;
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
    if Child is TDOMText then begin
      //DebugLn(['TLazFPDocFile.GetChildValuesAsString Data="',TDOMText(Child).Data,'" Length=',TDOMText(Child).Length]);
      Result:=Result+TDOMText(Child).Data;
    end;
    Child:=Child.NextSibling;
  end;
end;

function TLazFPDocFile.GetValuesFromNode(Node: TDOMNode): TFPDocElementValues;
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

      if S = 'example' then
        Result[fpdiExample] := Node.Attributes.GetNamedItem('file').NodeValue;
    end;
    Node := Node.NextSibling;
  end;
end;

function TLazFPDocFile.GetValueFromNode(Node: TDOMNode; Item: TFPDocItem
  ): string;
var
  Child: TDOMNode;
begin
  Result:='';
  Child:=Node.FindNode(FPDocItemNames[Item]);
  //DebugLn(['TLazFPDocFile.GetValueFromNode ',FPDocItemNames[Item],' Found=',Child<>nil]);
  if Child<>nil then begin
    if Item=fpdiExample then
      Result := Child.Attributes.GetNamedItem('file').NodeValue
    else
      Result := GetChildValuesAsString(Child);
  end;
end;

procedure TLazFPDocFile.SetChildValue(Node: TDOMNode; const ChildName: string;
  NewValue: string);
var
  Child: TDOMNode;
  TextNode: TDOMText;
begin
  Child:=Node.FindNode(ChildName);
  NewValue:=ToUnixLineEnding(NewValue);
  if Child=nil then begin
    {if ChildName = 'example' then begin
      OldNode:=Child.Attributes.GetNamedItem('file');
      NewValue:=FilenameToURLPath(NewValue);
      if (NewValue<>'')
      or (not (OldNode is TDOMAttr))
      or (TDOMAttr(OldNode).Value<>NewValue) then begin
        DebugLn(['TLazFPDocFile.SetChildValue Changing Name=',ChildName,' NewValue="',NewValue,'"']);
        // add or change example
        DocChanging;
        FileAttribute := Doc.CreateAttribute('file');
        FileAttribute.Value := NewValue;
        OldNode:=Node.Attributes.SetNamedItem(FileAttribute);
        OldNode.Free;
        DocChanged;
      end;
    end
    else }
    // add node
    if NewValue<>'' then begin
      DebugLn(['TLazFPDocFile.SetChildValue Adding Name=',ChildName,' NewValue="',NewValue,'"']);
      DocChanging;
      Child := Doc.CreateElement(ChildName);
      Node.AppendChild(Child);
      TextNode := Doc.CreateTextNode(NewValue);
      Child.AppendChild(TextNode);
      DocChanged;
    end;
  end else if GetChildValuesAsString(Child)<>NewValue then begin
    // change node
    DocChanging;
    while Child.FirstChild<>nil do
      Child.FirstChild.Free;
    DebugLn(['TLazDocForm.CheckAndWriteNode Changing ',Node.NodeName,' ChildName=',Child.NodeName,' OldValue=',Child.FirstChild.NodeValue,' NewValue="',NewValue,'"']);
    TextNode := Doc.CreateTextNode(NewValue);
    Child.AppendChild(TextNode);
    DocChanged;
  end;
end;

procedure TLazFPDocFile.DocChanging;
begin
  DocModified:=true;
  if (fUpdateLock>0) then begin
    if (ldffDocChangingCalled in FFlags) then exit;
    Include(FFlags,ldffDocChangingCalled);
  end;
  CodeHelpBoss.CallDocChangeEvents(chmhDocChanging,Self);
end;

procedure TLazFPDocFile.DocChanged;
begin
  if (fUpdateLock>0) then begin
    Include(FFlags,ldffDocChangedNeedsCalling);
    exit;
  end;
  Exclude(FFlags,ldffDocChangedNeedsCalling);
  CodeHelpBoss.CallDocChangeEvents(chmhDocChanged,Self);
end;

procedure TLazFPDocFile.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TLazFPDocFile.EndUpdate;
begin
  dec(fUpdateLock);
  if fUpdateLock<0 then RaiseGDBException('TLazFPDocFile.EndUpdate');
  if fUpdateLock=0 then begin
    Exclude(FFlags,ldffDocChangingCalled);
    if ldffDocChangedNeedsCalling in FFlags then
      DocChanged;
  end;
end;

procedure TCodeHelpManager.AddHandler(HandlerType: TCodeHelpManagerHandler;
  const AMethod: TMethod; AsLast: boolean);
begin
  if FHandlers[HandlerType]=nil then
    FHandlers[HandlerType]:=TMethodList.Create;
  FHandlers[HandlerType].Add(AMethod);
end;

procedure TCodeHelpManager.RemoveHandler(HandlerType: TCodeHelpManagerHandler;
  const AMethod: TMethod);
begin
  FHandlers[HandlerType].Remove(AMethod);
end;

procedure TCodeHelpManager.CallDocChangeEvents(HandlerType: TCodeHelpManagerHandler;
  Doc: TLazFPDocFile);
var
  i: LongInt;
begin
  i:=FHandlers[HandlerType].Count;
  while FHandlers[HandlerType].NextDownIndex(i) do
    TCodeHelpChangeEvent(FHandlers[HandlerType].Items[i])(Self,Doc);
end;

function TCodeHelpManager.DoCreateFPDocFileForSource(const SrcFilename: string
  ): string;
  
  procedure CleanUpPkgList(var PkgList: TFPList);
  var
    i: Integer;
    AProject: TLazProject;
    BaseDir: String;
    APackage: TLazPackage;
  begin
    if (PkgList=nil) then exit;
    for i:=PkgList.Count-1 downto 0 do begin
      if TObject(PkgList[i]) is TLazProject then begin
        AProject:=TLazProject(PkgList[i]);
        BaseDir:=ExtractFilePath(AProject.ProjectInfoFile);
        if BaseDir<>'' then continue;
      end else if TObject(PkgList[i]) is TLazPackage then begin
        APackage:=TLazPackage(PkgList[i]);
        BaseDir:=APackage.Directory;
        if BaseDir<>'' then continue;
      end;
      // this owner can not be used
      PkgList.Delete(i);
    end;
    if PkgList.Count=0 then
      FreeAndNil(PkgList);
      
    if PkgList.Count>1 then begin
      // there are more than one possible owners
      DebugLn(['TLazDocManager.DoCreateFPDocFileForSource.CleanUpPkgList Warning: overlapping projects/packages']);
    end;
  end;
  
  function SelectNewLazDocPaths(const Title, BaseDir: string): string;
  begin
    Result:=LazSelectDirectory('Choose LazDoc directory for '+Title,BaseDir);
  end;
  
var
  PkgList: TFPList;
  NewOwner: TObject;
  AProject: TLazProject;
  APackage: TLazPackage;
  p: Integer;
  LazDocPaths: String;
  LazDocPackageName: String;
  NewPath: String;
  BaseDir: String;
  Code: TCodeBuffer;
  CurUnitName: String;
  AVLNode: TAvgLvlTreeNode;
begin
  Result:='';
  DebugLn(['TLazDocManager.DoCreateFPDocFileForSource ',SrcFilename]);
  if not FilenameIsAbsolute(SrcFilename) then begin
    DebugLn(['TLazDocManager.DoCreateFPDocFileForSource failed, because file no absolute: ',SrcFilename]);
    exit;
  end;

  PkgList:=nil;
  try
    // get all packages owning the file
    PkgList:=PackageEditingInterface.GetOwnersOfUnit(SrcFilename);
    CleanUpPkgList(PkgList);
    if (PkgList=nil) then begin
      PkgList:=PackageEditingInterface.GetOwnersOfUnit(SrcFilename);
      CleanUpPkgList(PkgList);
    end;
    if PkgList=nil then begin
      // no package/project found
      MessageDlg('Package not found',
        'The unit '+SrcFilename+' is not owned be any package or project.'#13
        +'Please add the unit to a package or project.'#13
        +'Unable to create the fpdoc file.',mtError,[mbCancel],0);
      exit;
    end;

    NewOwner:=TObject(PkgList[0]);
    if NewOwner is TLazProject then begin
      AProject:=TLazProject(NewOwner);
      BaseDir:=ExtractFilePath(AProject.ProjectInfoFile);
      if AProject.LazDocPaths='' then
        AProject.LazDocPaths:=SelectNewLazDocPaths(AProject.ShortDescription,BaseDir);
      LazDocPaths:=AProject.LazDocPaths;
      LazDocPackageName:=ExtractFileNameOnly(AProject.ProjectInfoFile);
    end else if NewOwner is TLazPackage then begin
      APackage:=TLazPackage(NewOwner);
      BaseDir:=APackage.Directory;
      if APackage.LazDocPaths='' then
        APackage.LazDocPaths:=SelectNewLazDocPaths(APackage.Name,BaseDir);
      LazDocPaths:=APackage.LazDocPaths;
      LazDocPackageName:=APackage.Name;
    end else begin
      DebugLn(['TLazDocManager.DoCreateFPDocFileForSource unknown owner type ',dbgsName(NewOwner)]);
      exit;
    end;
      
    p:=1;
    repeat
      NewPath:=GetNextDirectoryInSearchPath(LazDocPaths,p);
      if not FilenameIsAbsolute(NewPath) then
        NewPath:=AppendPathDelim(BaseDir)+NewPath;
      if DirPathExistsCached(NewPath) then begin
        // fpdoc directory found
        Result:=AppendPathDelim(NewPath)+lowercase(ExtractFileNameOnly(SrcFilename))+'.xml';
        Code:=CodeToolBoss.LoadFile(SrcFilename,true,false);
        // get unitname
        CurUnitName:=ExtractFileNameOnly(SrcFilename);
        if Code<>nil then
          CurUnitName:=CodeToolBoss.GetSourceName(Code,false);
        // remove cache (source to fpdoc filename)
        AVLNode:=FSrcToDocMap.FindKey(Pointer(SrcFilename),
                                      @CompareAnsistringWithLDSrc2DocSrcFile);
        if AVLNode<>nil then
          FSrcToDocMap.FreeAndDelete(AVLNode);
        // create fpdoc file
        if CreateFPDocFile(Result,LazDocPackageName,CurUnitName)=nil then
          Result:='';
        exit;
      end;
    until false;
    
    // no valid directory found
    DebugLn(['TLazDocManager.DoCreateFPDocFileForSource LazDocModul="',LazDocPackageName,'" LazDocPaths="',LazDocPaths,'" ']);
    MessageDlg('No valid lazdoc path',
      LazDocPackageName+' does not have any valid lazdoc path.'#13
      +'Unable to create the fpdoc file for '+SrcFilename,mtError,[mbCancel],0);
  finally
    PkgList.Free;
  end;
end;

function TCodeHelpManager.CreateFPDocFile(const ExpandedFilename,
  PackageName, ModuleName: string): TCodeBuffer;
var
  Doc: TXMLDocument;
  DescrNode: TDOMElement;
  ms: TMemoryStream;
  s: string;
  ModuleNode: TDOMElement;
  PackageNode: TDOMElement;
begin
  Result:=nil;
  if FileExistsCached(ExpandedFilename) then begin
    Result:=CodeToolBoss.LoadFile(ExpandedFilename,true,false);
    exit;
  end;
  Result:=CodeToolBoss.CreateFile(ExpandedFilename);
  if Result=nil then begin
    MessageDlg('Unable to create file',
      'Unable to create file '+ExpandedFilename,mtError,[mbCancel],0);
    exit;
  end;
  
  Doc:=nil;
  ms:=nil;
  try
    Doc:=TXMLDocument.Create;
    // <fpdoc-descriptions>
    DescrNode:=Doc.CreateElement('fpdoc-descriptions');
    Doc.AppendChild(DescrNode);
    //   <package name="packagename">
    PackageNode:=Doc.CreateElement('package');
    PackageNode.SetAttribute('name',PackageName);
    DescrNode.AppendChild(PackageNode);
    //   <module name="unitname">
    ModuleNode:=Doc.CreateElement('module');
    ModuleNode.SetAttribute('name',ModuleName);
    PackageNode.AppendChild(ModuleNode);
    // write the XML to a string
    ms:=TMemoryStream.Create;
    WriteXMLFile(Doc,ms);
    ms.Position:=0;
    SetLength(s,ms.Size);
    if s<>'' then
      ms.Read(s[1],length(s));
    // copy to codebuffer
    //DebugLn(['TLazDocManager.CreateFPDocFile ',s]);
    Result.Source:=s;
    // save file
    if SaveCodeBuffer(Result)<>mrOk then
      Result:=nil;
  finally
    ms.Free;
    Doc.Free;
  end;
end;

constructor TCodeHelpManager.Create;
begin
  FDocs:=TAvgLvlTree.Create(@CompareLazFPDocFilenames);
  FSrcToDocMap:=TAvgLvlTree.Create(@CompareLDSrc2DocSrcFilenames);
  FDeclarationCache:=TDeclarationInheritanceCache.Create(
                                  @CodeToolBoss.FindDeclarationAndOverload,
                                  @CodeToolBoss.GetCodeTreeNodesDeletedStep);
end;

destructor TCodeHelpManager.Destroy;
begin
  ClearSrcToDocMap;
  FreeDocs;
  FreeAndNil(FDocs);
  FreeAndNil(FSrcToDocMap);
  FreeAndNil(FDeclarationCache);
  inherited Destroy;
end;

function TCodeHelpManager.FindFPDocFile(const Filename: string): TLazFPDocFile;
var
  Node: TAvgLvlTreeNode;
begin
  Node:=FDocs.FindKey(Pointer(Filename),@CompareAnsistringWithLazFPDocFile);
  if Node<>nil then
    Result:=TLazFPDocFile(Node.Data)
  else
    Result:=nil;
end;

function TCodeHelpManager.LoadFPDocFile(const Filename: string; UpdateFromDisk,
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
  if (ADocFile.Doc<>nil) then begin
    if (ADocFile.ChangeStep=ADocFile.CodeBuffer.ChangeStep) then begin
      // CodeBuffer has not changed
      if ADocFile.DocModified and Revert then begin
        // revert the modifications => rebuild the Doc from the CodeBuffer
      end else begin
        // no update needed
        exit(true);
      end;
    end;
  end;
  CacheWasUsed:=false;
  
  {$IFDEF VerboseLazDoc}
  DebugLn(['TLazDocManager.LoadFPDocFile parsing ',ADocFile.Filename]);
  {$ENDIF}
  CallDocChangeEvents(chmhDocChanging,ADocFile);

  // parse XML
  ADocFile.ChangeStep:=ADocFile.CodeBuffer.ChangeStep;
  ADocFile.DocModified:=false;
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
    CallDocChangeEvents(chmhDocChanging,ADocFile);
  end;
end;

function TCodeHelpManager.SaveFPDocFile(ADocFile: TLazFPDocFile): TModalResult;
var
  ms: TMemoryStream;
  s: string;
begin
  if (not ADocFile.DocModified)
  and (ADocFile.ChangeStep=ADocFile.CodeBuffer.ChangeStep)
  and (not ADocFile.CodeBuffer.FileOnDiskNeedsUpdate) then begin
    DebugLn(['TLazDocManager.SaveFPDocFile no save needed: ',ADocFile.Filename]);
    exit(mrOk);
  end;
  if (ADocFile.Doc=nil) then begin
    DebugLn(['TLazDocManager.SaveFPDocFile no Doc: ',ADocFile.Filename]);
    exit(mrOk);
  end;
  if not FilenameIsAbsolute(ADocFile.Filename) then begin
    DebugLn(['TLazDocManager.SaveFPDocFile no expanded filename: ',ADocFile.Filename]);
    exit(mrCancel);
  end;

  // write Doc to xml stream
  try
    ms:=TMemoryStream.Create;
    WriteXMLFile(ADocFile.Doc, ms);
    ms.Position:=0;
    SetLength(s,ms.Size);
    if s<>'' then
      ms.Read(s[1],length(s));
  finally
    ms.Free;
  end;

  // write to CodeBuffer
  ADocFile.CodeBuffer.Source:=s;
  ADocFile.DocModified:=false;
  if ADocFile.CodeBuffer.ChangeStep=ADocFile.ChangeStep then begin
    // doc was not really modified => do not save to keep file date
    DebugLn(['TLazDocManager.SaveFPDocFile Doc was not really modified ',ADocFile.Filename]);
    exit(mrOk);
  end;
  ADocFile.ChangeStep:=ADocFile.CodeBuffer.ChangeStep;
  
  // write to disk
  Result:=SaveCodeBuffer(ADocFile.CodeBuffer);
  DebugLn(['TLazDocManager.SaveFPDocFile saved ',ADocFile.Filename]);
end;

function TCodeHelpManager.GetFPDocFilenameForHelpContext(
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

function TCodeHelpManager.GetFPDocFilenameForSource(SrcFilename: string;
  ResolveIncludeFiles: Boolean; out CacheWasUsed: boolean;
  CreateIfNotExists: boolean): string;
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
      PkgList:=PackageEditingInterface.GetPossibleOwnersOfUnit(SrcFilename,[]);
    end else begin
      PkgList:=PackageEditingInterface.GetOwnersOfUnit(SrcFilename);
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
  MapEntry: TCHSourceToFPDocFile;
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
  
  if not FilenameIsPascalSource(SrcFilename) then begin
    DebugLn(['TLazDocManager.GetFPDocFilenameForSource error: not a source file: "',SrcFilename,'"']);
    exit;
  end;
  
  try
    // first try cache
    MapEntry:=nil;
    AVLNode:=FSrcToDocMap.FindKey(Pointer(SrcFilename),
                                  @CompareAnsistringWithLDSrc2DocSrcFile);
    if AVLNode<>nil then begin
      MapEntry:=TCHSourceToFPDocFile(AVLNode.Data);
      if (MapEntry.FPDocFilenameTimeStamp=CompilerParseStamp)
      and (MapEntry.FilesTimeStamp=FileStateCache.TimeStamp) then begin
        Result:=MapEntry.FPDocFilename;
        exit;
      end;
    end;
    CacheWasUsed:=false;

    {$IFDEF VerboseLazDoc}
    DebugLn(['TLazDocManager.GetFPDocFilenameForSource searching SrcFilename=',SrcFilename]);
    {$ENDIF}

    // first check if the file is owned by any project/package
    SearchPath:='';
    CheckUnitOwners(false);// first check if file is owned by a package/project
    CheckUnitOwners(true);// then check if the file is in a source directory of a package/project
    CheckIfInLazarus;

    // finally add the default paths
    AddSearchPath(EnvironmentOptions.LazDocPaths,'');
    FPDocName:=lowercase(ExtractFileNameOnly(SrcFilename))+'.xml';
    {$IFDEF VerboseLazDoc}
    DebugLn(['TLazDocManager.GetFPDocFilenameForSource Search ',FPDocName,' in "',SearchPath,'"']);
    {$ENDIF}
    Result:=SearchFileInPath(FPDocName,'',SearchPath,';',ctsfcAllCase);

    // save to cache
    if MapEntry=nil then begin
      MapEntry:=TCHSourceToFPDocFile.Create;
      MapEntry.SourceFilename:=SrcFilename;
      FSrcToDocMap.Add(MapEntry);
    end;
    MapEntry.FPDocFilename:=Result;
    MapEntry.FPDocFilenameTimeStamp:=CompilerParseStamp;
    MapEntry.FilesTimeStamp:=FileStateCache.TimeStamp;
  finally
    if (Result='') and CreateIfNotExists then begin
      Result:=DoCreateFPDocFileForSource(SrcFilename);
    end;
    {$IFDEF VerboseLazDoc}
    DebugLn(['TLazDocManager.GetFPDocFilenameForSource SrcFilename="',SrcFilename,'" Result="',Result,'"']);
    {$ENDIF}
  end;
end;

function TCodeHelpManager.CodeNodeToElementName(Tool: TFindDeclarationTool;
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

function TCodeHelpManager.GetFPDocNode(Tool: TCodeTool; CodeNode: TCodeTreeNode;
  Complete: boolean; out FPDocFile: TLazFPDocFile; out DOMNode: TDOMNode;
  out CacheWasUsed: boolean): TCodeHelpParseResult;
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
  if FPDocFilename='' then exit(chprFailed);
  if (not CacheWasUsed) and (not Complete) then exit(chprParsing);

  // load FPDoc file
  if not LoadFPDocFile(FPDocFilename,true,false,FPDocFile,CacheWasUsed) then
    exit(chprFailed);
  if (not CacheWasUsed) and (not Complete) then exit(chprParsing);

  // find FPDoc node
  ElementName:=CodeNodeToElementName(Tool,CodeNode);
  if ElementName='' then exit(chprFailed);
  DOMNode:=FPDocFile.GetElementWithName(ElementName);
  if DOMNode=nil then exit(chprFailed);
  
  Result:=chprSuccess;
end;

function TCodeHelpManager.GetDeclarationChain(Code: TCodeBuffer; X, Y: integer;
  out ListOfPCodeXYPosition: TFPList; out CacheWasUsed: boolean
  ): TCodeHelpParseResult;
begin
  if FDeclarationCache.FindDeclarations(Code,X,Y,ListOfPCodeXYPosition,
    CacheWasUsed)
  then
    Result:=chprSuccess
  else
    Result:=chprFailed;
end;

function TCodeHelpManager.GetCodeContext(CodePos: PCodeXYPosition; out
  FindContext: TFindContext; Complete: boolean; out CacheWasUsed: boolean
  ): TCodeHelpParseResult;
var
  CurTool: TCodeTool;
  CleanPos: integer;
  Node: TCodeTreeNode;
begin
  Result:=chprFailed;
  FindContext:=CleanFindContext;
  CacheWasUsed:=true;

  //DebugLn(['TLazDocManager.GetElementChain i=',i,' X=',CodePos^.X,' Y=',CodePos^.Y]);
  if (CodePos=nil) or (CodePos^.Code=nil) or (CodePos^.X<1) or (CodePos^.Y<1)
  then begin
    DebugLn(['TLazDocManager.GetElementChain invalid CodePos']);
    exit;
  end;

  // build CodeTree and find node
  if not CodeToolBoss.Explore(CodePos^.Code,CurTool,false,true) then begin
    DebugLn(['TLazDocManager.GetElementChain note: there was a parser error']);
  end;
  if CurTool=nil then begin
    DebugLn(['TLazDocManager.GetElementChain explore failed']);
    exit;
  end;
  if CurTool.CaretToCleanPos(CodePos^,CleanPos)<>0 then begin
    DebugLn(['TLazDocManager.GetElementChain invalid CodePos']);
    exit;
  end;

  Node:=CurTool.FindDeepestNodeAtPos(CleanPos,false);
  if Node=nil then begin
    DebugLn(['TLazDocManager.GetElementChain node not found']);
    exit;
  end;

  // use only definition nodes
  if (Node.Desc=ctnProcedureHead)
  and (Node.Parent<>nil) and (Node.Parent.Desc=ctnProcedure) then
    Node:=Node.Parent;
  if not (Node.Desc in
    (AllIdentifierDefinitions+[ctnProperty,ctnProcedure,ctnEnumIdentifier]))
  then begin
    DebugLn(['TLazDocManager.GetElementChain ignoring node ',Node.DescAsString]);
    exit;
  end;
  if (CurTool.NodeIsForwardDeclaration(Node)) then begin
    DebugLn(['TLazDocManager.GetElementChain ignoring forward']);
    exit;
  end;
  
  // success
  FindContext.Tool:=CurTool;
  FindContext.Node:=Node;
  Result:=chprSuccess;
end;

function TCodeHelpManager.GetElementChain(Code: TCodeBuffer; X, Y: integer;
  Complete: boolean; out Chain: TCodeHelpElementChain; out CacheWasUsed: boolean
  ): TCodeHelpParseResult;
var
  ListOfPCodeXYPosition: TFPList;
  i: Integer;
  CodePos: PCodeXYPosition;
  LDElement: TCodeHelpElement;
  SrcFilename: String;
  FPDocFilename: String;
  FindContext: TFindContext;
begin
  Chain:=nil;
  ListOfPCodeXYPosition:=nil;
  try
    //DebugLn(['TLazDocManager.GetElementChain GetDeclarationChain...']);
    // get the declaration chain
    Result:=GetDeclarationChain(Code,X,Y,ListOfPCodeXYPosition,CacheWasUsed);
    if Result<>chprSuccess then exit;
    if (not CacheWasUsed) and (not Complete) then exit(chprParsing);
    
    {$IFDEF VerboseLazDoc}
    DebugLn(['TLazDocManager.GetElementChain init the element chain: ListOfPCodeXYPosition.Count=',ListOfPCodeXYPosition.Count,' ...']);
    {$ENDIF}
    // init the element chain
    Result:=chprParsing;
    Chain:=TCodeHelpElementChain.Create;
    Chain.CodePos.Code:=Code;
    Chain.MakeValid;
    Code.LineColToPosition(Y,X,Chain.CodePos.P);
    // fill the element chain
    for i:=0 to ListOfPCodeXYPosition.Count-1 do begin
      // get source position of declaration
      CodePos:=PCodeXYPosition(ListOfPCodeXYPosition[i]);
      Result:=GetCodeContext(CodePos,FindContext,Complete,CacheWasUsed);
      if Result=chprFailed then continue; // skip invalid contexts
      if Result<>chprSuccess then continue;
      if (not CacheWasUsed) and (not Complete) then exit(chprParsing);

      // add element
      LDElement:=Chain.Add;
      LDElement.CodeXYPos:=CodePos^;
      LDElement.CodeContext:=FindContext;
      //DebugLn(['TLazDocManager.GetElementChain i=',i,' CodeContext=',FindContextToString(LDElement.CodeContext)]);

      // find corresponding FPDoc file
      SrcFilename:=LDElement.CodeContext.Tool.MainFilename;
      FPDocFilename:=GetFPDocFilenameForSource(SrcFilename,false,CacheWasUsed);
      //DebugLn(['TLazDocManager.GetElementChain FPDocFilename=',FPDocFilename]);
      if (not CacheWasUsed) and (not Complete) then exit(chprParsing);

      if FPDocFilename<>'' then begin
        // load FPDoc file
        LoadFPDocFile(FPDocFilename,true,false,LDElement.FPDocFile,
                      CacheWasUsed);
        if (not CacheWasUsed) and (not Complete) then exit(chprParsing);
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
        LDElement.ElementNodeValid:=true;
      end;
      //DebugLn(['TLazDocManager.GetElementChain ElementNode=',LDElement.ElementNode<>nil]);
    end;

    Result:=chprSuccess;
  finally
    if Result<>chprSuccess then
      FreeAndNil(Chain);
  end;
end;

function TCodeHelpManager.GetHTMLHint(Code: TCodeBuffer; X, Y: integer;
  Complete: boolean; out BaseURL, HTMLHint: string; out CacheWasUsed: boolean
  ): TCodeHelpParseResult;
const
  le = '<BR>'#13;
var
  IsHTML: boolean;
  
  function EndNow(var LastResult: TCodeHelpParseResult): boolean;
  begin
    if LastResult<>chprSuccess then begin
      Result:=true;
      if HTMLHint<>'' then
        LastResult:=chprSuccess
      else
        LastResult:=chprFailed;
      exit;
    end;
    if (not CacheWasUsed) and (not Complete) then begin
      Result:=true;
      LastResult:=chprParsing;
    end;
    Result:=false;
  end;
  
  procedure AddHTML(const s: string);
  begin
    if not IsHTML then begin
      IsHTML:=true;
      if HTMLHint<>'' then
        HTMLHint:=HTMLHint+le+le;
      HTMLHint:=HTMLHint+s;
    end;
  end;
  
var
  Chain: TCodeHelpElementChain;
  i: Integer;
  Item: TCodeHelpElement;
  NodeValues: TFPDocElementValues;
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
  BaseURL:='lazdoc://';
  IsHTML:=false;
  try
    HTMLHint:=CodeToolBoss.FindSmartHint(Code,X,Y);

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
              AddHTML(Item.ElementName+le
                      +NodeValues[fpdiShort]);
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
                  AddHTML(Item.ElementName+le
                          +CommentStr);
                end else begin
                  AddHTML(CommentStr);
                end;
                ItemAdded:=true;
              end;
            end;
          end;
        end;
      end;
      Result:=chprSuccess;
    finally
      FreeListOfPCodeXYPosition(ListOfPCodeXYPosition);
      Chain.Free;
    end;
  finally
    if IsHTML then
      HTMLHint:='<HTML><BODY>'+HTMLHint+'</BODY></HTML>';
  end;
  DebugLn(['TLazDocManager.GetHint END Hint="',HTMLHint,'"']);
end;

function TCodeHelpManager.CreateElement(Code: TCodeBuffer; X, Y: integer;
  out Element: TCodeHelpElement): Boolean;
var
  CacheWasUsed: boolean;
  SrcFilename: String;
  FPDocFilename: String;
begin
  Result:=false;
  Element:=nil;
  if Code=nil then begin
    DebugLn(['TLazDocManager.CreateElement failed Code=nil']);
    exit;
  end;
  DebugLn(['TLazDocManager.CreateElement START ',Code.Filename,' ',X,',',Y]);
  
  Element:=TCodeHelpElement.Create;
  try
    // check if code context can have a fpdoc element
    Element.CodeXYPos.Code:=Code;
    Element.CodeXYPos.X:=X;
    Element.CodeXYPos.Y:=Y;
    if GetCodeContext(@Element.CodeXYPos,Element.CodeContext,true,
      CacheWasUsed)<>chprSuccess then
    begin
      DebugLn(['TLazDocManager.CreateElement GetCodeContext failed for ',Code.Filename,' ',X,',',Y]);
      exit;
    end;
    Element.ElementName:=CodeNodeToElementName(Element.CodeContext.Tool,
                                               Element.CodeContext.Node);
    DebugLn(['TLazDocManager.CreateElement Element.ElementName=',Element.ElementName]);

    // find / create fpdoc file
    SrcFilename:=Element.CodeContext.Tool.MainFilename;
    FPDocFilename:=GetFPDocFilenameForSource(SrcFilename,false,CacheWasUsed,true);
    if FPDocFilename='' then begin
      // no fpdoc file
      DebugLn(['TLazDocManager.CreateElement unable to create fpdoc file for ',FPDocFilename]);
    end;
    DebugLn(['TLazDocManager.CreateElement FPDocFilename=',FPDocFilename]);

    // parse fpdoc file
    if not LoadFPDocFile(FPDocFilename,true,false,Element.FPDocFile,CacheWasUsed)
    then begin
      DebugLn(['TLazDocManager.CreateElement unable to load fpdoc file ',FPDocFilename]);
      exit;
    end;

    Element.ElementNode:=Element.FPDocFile.GetElementWithName(
                                                      Element.ElementName,true);
    Result:=Element.ElementNode<>nil;
  finally
    if not Result then
      FreeAndNil(Element);
  end;
end;

procedure TCodeHelpManager.FreeDocs;
var
  AVLNode: TAvgLvlTreeNode;
begin
  AVLNode:=FDocs.FindLowest;
  while AVLNode<>nil do begin
    CallDocChangeEvents(chmhDocChanging,TLazFPDocFile(AVLNode.Data));
    AVLNode:=FDocs.FindSuccessor(AVLNode);
  end;
  FDocs.FreeAndClear;
end;

procedure TCodeHelpManager.ClearSrcToDocMap;
begin
  FSrcToDocMap.FreeAndClear;
end;

procedure TCodeHelpManager.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TCodeHelpManagerHandler;
begin
  for HandlerType:=Low(TCodeHelpManagerHandler) to High(TCodeHelpManagerHandler) do
    FHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

procedure TCodeHelpManager.AddHandlerOnChanging(
  const OnDocChangingEvent: TCodeHelpChangeEvent; AsLast: boolean);
begin
  AddHandler(chmhDocChanging,TMethod(OnDocChangingEvent),AsLast);
end;

procedure TCodeHelpManager.RemoveHandlerOnChanging(
  const OnDocChangingEvent: TCodeHelpChangeEvent);
begin
  RemoveHandler(chmhDocChanging,TMethod(OnDocChangingEvent));
end;

procedure TCodeHelpManager.AddHandlerOnChanged(
  const OnDocChangedEvent: TCodeHelpChangeEvent; AsLast: boolean);
begin
  AddHandler(chmhDocChanged,TMethod(OnDocChangedEvent),AsLast);
end;

procedure TCodeHelpManager.RemoveHandlerOnChanged(
  const OnDocChangedEvent: TCodeHelpChangeEvent);
begin
  RemoveHandler(chmhDocChanged,TMethod(OnDocChangedEvent));
end;


{ TCodeHelpElementChain }

function TCodeHelpElementChain.GetCount: integer;
begin
  Result:=FItems.Count;
end;

function TCodeHelpElementChain.GetItems(Index: integer): TCodeHelpElement;
begin
  Result:=TCodeHelpElement(FItems[Index]);
end;

function TCodeHelpElementChain.Add: TCodeHelpElement;
begin
  Result:=TCodeHelpElement.Create;
  FItems.Add(Result);
end;

constructor TCodeHelpElementChain.Create;
begin
  FItems:=TFPList.Create;
end;

destructor TCodeHelpElementChain.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TCodeHelpElementChain.Clear;
var
  i: Integer;
begin
  for i:=0 to FItems.Count-1 do TObject(FItems[i]).Free;
  FItems.Clear;
end;

function TCodeHelpElementChain.IndexOfFile(AFile: TLazFPDocFile): integer;
begin
  Result:=FItems.Count-1;
  while (Result>=0) do begin
    if Items[Result].FPDocFile=AFile then exit;
    dec(Result);
  end;
end;

function TCodeHelpElementChain.IsValid: boolean;
begin
  Result:=(IDEChangeStep=CompilerParseStamp)
    and (CodetoolsChangeStep=CodeToolBoss.CodeTreeNodesDeletedStep);
end;

procedure TCodeHelpElementChain.MakeValid;
begin
  IDEChangeStep:=CompilerParseStamp;
  CodetoolsChangeStep:=CodeToolBoss.CodeTreeNodesDeletedStep;
end;

function TCodeHelpElementChain.DocFile: TLazFPDocFile;
begin
  Result:=nil;
  if (Count>0) then
    Result:=Items[0].FPDocFile;
end;

end.

