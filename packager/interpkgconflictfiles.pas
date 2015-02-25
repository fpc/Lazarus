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

  Author: Mattias Gaertner

  Abstract:
    Called after checking what packages need compile.
    Check source files and compiled files for name conflicts between packages.
}
unit InterPkgConflictFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, AVL_Tree, InterfaceBase, Forms, ComCtrls,
  FileProcs, LazFileUtils, BasicCodeTools, DefineTemplates, CodeToolManager,
  // IDEIntf
  ProjectIntf, CompOptsIntf,
  // IDE
  LazarusIDEStrConsts, CompilerOptions, EnvironmentOpts, IDEProcs,
  TransferMacros, LazConf, PackageDefs, PackageSystem;

function CheckInterPkgFiles(IDEObject: TObject;
  PkgList: TFPList; out FilesChanged: boolean
  ): boolean; // returns false if user cancelled

implementation

type
  TPGInterPkgOwnerInfo = class
  public
    Name: string;
    Owner: TObject;
    HasOptionUr: boolean;
    CompOptions: TBaseCompilerOptions;
    SrcDirs: string; // unitpath without inherited
    IncDirs: string; // incpath without inherited and without SrcDirs
    UnitOutDir: string; // can be empty -> if empty FPC creates ppu in SrcDirs
  end;

  { TPGInterPkgFile }

  TPGInterPkgFile = class
  public
    FullFilename: string;
    ShortFilename: string;
    AnUnitName: string;
    OwnerInfo: TPGInterPkgOwnerInfo;
    constructor Create(TheFullFilename, TheUnitName: string; Owner: TPGInterPkgOwnerInfo);
  end;

  { TPGIPAmbiguousFile }

  TPGIPAmbiguousFile = class
  public
    Filename: string;
    OwnerInfo: TPGInterPkgOwnerInfo;
    ConflictFilename: string;
    ConflictOwner: TPGInterPkgOwnerInfo;
  end;

  { TPGIPAmbiguousCompiledFile }

  TPGIPAmbiguousCompiledFile = class(TPGIPAmbiguousFile)
  public
    SrcFilename: string; // the corresponding .pas/.pp file, if it exists
    ConflictSrcFilename: string; // the corresponding .pas/.pp file, if it exists
  end;

  { TPGIPConflictsDialog }

  TPGIPConflictsDialog = class(TForm)
    ConflictsTreeView: TTreeView;
  private
    procedure Fill;
  public
    SrcFiles: TObjectList; // list of TPGIPAmbiguousFile
    CompiledFiles: TObjectList; // list of TPGIPAmbiguousCompiledFile
    procedure Init;
  end;

function ComparePGInterPkgFullFilenames(File1, File2: Pointer): integer;
var
  F1: TPGInterPkgFile absolute File1;
  F2: TPGInterPkgFile absolute File2;
begin
  Result:=CompareFilenames(F1.FullFilename,F2.FullFilename);
end;

function ComparePGInterPkgUnitnames(File1, File2: Pointer): integer;
var
  F1: TPGInterPkgFile absolute File1;
  F2: TPGInterPkgFile absolute File2;
begin
  Result:=CompareDottedIdentifiers(PChar(Pointer(F1.AnUnitName)),PChar(Pointer(F2.AnUnitName)));
end;

function ComparePGInterPkgShortFilename(File1, File2: Pointer): integer;
var
  F1: TPGInterPkgFile absolute File1;
  F2: TPGInterPkgFile absolute File2;
begin
  Result:=CompareFilenames(F1.ShortFilename,F2.ShortFilename);
end;

procedure TPGIPConflictsDialog.Fill;
begin

end;

procedure TPGIPConflictsDialog.Init;

  function AddChild(ParentNode: TTreeNode; Caption: string): TTreeNode;
  begin
    Result:=ConflictsTreeView.Items.AddChild(ParentNode,Caption);
  end;

var
  MainNode: TTreeNode;
  i: Integer;
  CurFile: TPGIPAmbiguousCompiledFile;
  ItemNode: TTreeNode;
  s: String;
begin
  ConflictsTreeView.Items.Clear;
  if CompiledFiles.Count>0 then
  begin
    MainNode:=ConflictsTreeView.Items.Add(nil,'Conflicting compiled files');
    for i:=0 to CompiledFiles.Count-1 do
    begin
      CurFile:=TPGIPAmbiguousCompiledFile(CompiledFiles[i]);
      ItemNode:=AddChild(MainNode,ExtractFilename(CurFile.Filename));
      // file owner
      if CurFile.OwnerInfo.Owner is TLazPackage then
        s:='Package: '+CurFile.OwnerInfo.Name
      else
        s:=CurFile.OwnerInfo.Name;
      AddChild(ItemNode,s);
      // file path and src
      AddChild(ItemNode,'File: '+CurFile.Filename);
      if CurFile.SrcFilename='' then
        s:='No source found'
      else
        s:='Src: '+CurFile.SrcFilename;
      AddChild(ItemNode,s);

      // conflict file owner
      if CurFile.ConflictOwner.Owner is TLazPackage then
        s:='Conflict Package: '+CurFile.ConflictOwner.Name
      else
        s:=CurFile.ConflictOwner.Name;
      AddChild(ItemNode,s);
      // conflict file path
      AddChild(ItemNode,'Conflict File: '+CurFile.ConflictFilename);
      if CurFile.ConflictSrcFilename='' then
        s:='No source found'
      else
        s:='Src: '+CurFile.ConflictSrcFilename;
      AddChild(ItemNode,s);
    end;
  end;
end;

{ TPGInterPkgFile }

constructor TPGInterPkgFile.Create(TheFullFilename, TheUnitName: string;
  Owner: TPGInterPkgOwnerInfo);
begin
  FullFilename:=TheFullFilename;
  ShortFilename:=ExtractFileName(FullFilename);
  AnUnitName:=TheUnitName;
  OwnerInfo:=Owner;
end;

function CheckInterPkgFiles(IDEObject: TObject; PkgList: TFPList; out
  FilesChanged: boolean): boolean;
{ Scan all source and output directories (Note: they are already cached, because
  this method is called after the checks if a compile is needed).
  Report strange ppu files and duplicate file names.

  IDEObject can be a TProject, TLazPackage or TLazPackageGraph(building IDE)
  PkgList is list of TLazPackage
}
var
  OwnerInfos: TObjectList; // list of TPGInterPkgOwnerInfo
  TargetOS: String;
  TargetCPU: String;
  LCLWidgetType: String;
  FullFiles: TAVLTree; // tree of TPGInterPkgFile sorted for FullFilename
  Units: TAVLTree; // tree of TPGInterPkgFile sorted for AnUnitName
  ShortFiles: TAVLTree; // tree of TPGInterPkgFile sorted for ShortFilename
  AmbiguousCompiledFiles: TObjectList; // list of TPGIPAmbiguousCompiledFile
  AmbiguousSrcFiles: TObjectList; // list of TPGIPAmbiguousFile

  procedure AddOwnerInfo(TheOwner: TObject);
  var
    LazDir: String;
    CustomOptions: String;
    p: Integer;
    OwnerInfo: TPGInterPkgOwnerInfo;
  begin
    OwnerInfo:=TPGInterPkgOwnerInfo.Create;
    OwnerInfos.Add(OwnerInfo);
    OwnerInfo.Owner:=TheOwner;
    if TheOwner is TLazPackage then
    begin
      OwnerInfo.Name:=TLazPackage(TheOwner).IDAsString;
      OwnerInfo.CompOptions:=TLazPackage(TheOwner).LazCompilerOptions as TBaseCompilerOptions;
    end else if TheOwner is TLazProject then
    begin
      OwnerInfo.Name:=TLazProject(TheOwner).GetTitleOrName;
      OwnerInfo.CompOptions:=TLazProject(TheOwner).LazCompilerOptions as TBaseCompilerOptions;
    end
    else if TheOwner=PackageGraph then begin
      // building IDE
      OwnerInfo.Name:='#IDE';
      LazDir:=AppendPathDelim(EnvironmentOptions.GetParsedLazarusDirectory);
      OwnerInfo.SrcDirs:=LazDir+'ide'
        +';'+LazDir+'debugger'
        +';'+LazDir+'packager'
        +';'+LazDir+'designer'
        +';'+LazDir+'converter';
      OwnerInfo.IncDirs:=OwnerInfo.SrcDirs
        +';'+LazDir+'ide'+PathDelim+'include'+PathDelim+TargetOS
        +';'+LazDir+'ide'+PathDelim+'include'+PathDelim+GetDefaultSrcOSForTargetOS(TargetOS);
      OwnerInfo.UnitOutDir:=LazDir+'units'+PathDelim+TargetCPU+'-'+TargetOS+PathDelim+LCLWidgetType;
    end;
    if OwnerInfo.CompOptions<>nil then begin
      OwnerInfo.SrcDirs:=OwnerInfo.CompOptions.GetPath(
                                    pcosUnitPath,icoNone,false,coptParsed,true);
      OwnerInfo.IncDirs:=OwnerInfo.CompOptions.GetPath(
                                 pcosIncludePath,icoNone,false,coptParsed,true);
      if OwnerInfo.CompOptions.UnitOutputDirectory<>'' then
        OwnerInfo.UnitOutDir:=OwnerInfo.CompOptions.GetUnitOutputDirectory(false);
      CustomOptions:=OwnerInfo.CompOptions.ParsedOpts.GetParsedValue(pcosCustomOptions);
      p:=1;
      OwnerInfo.HasOptionUr:=FindNextFPCParameter(CustomOptions,'-Ur',p)>0;
    end;
    OwnerInfo.IncDirs:=TrimSearchPath(RemoveSearchPaths(OwnerInfo.IncDirs,OwnerInfo.SrcDirs),'');
    OwnerInfo.UnitOutDir:=TrimFilename(OwnerInfo.UnitOutDir);
    OwnerInfo.SrcDirs:=TrimSearchPath(OwnerInfo.SrcDirs,'');
  end;

  procedure CollectFilesInDir(OwnerInfo: TPGInterPkgOwnerInfo; Dir: string;
    var SearchedDirs: string; IsIncDir: boolean);
  var
    Files: TStrings;
    aFilename: String;
    Ext: String;
    AnUnitName: String;
    NewFile: TPGInterPkgFile;
  begin
    if Dir='' then exit;
    if not FilenameIsAbsolute(Dir) then
    begin
      debugln(['Inconsistency: CollectFilesInDir dir no absolute: "',Dir,'" Owner=',OwnerInfo.Name]);
      exit;
    end;
    if SearchDirectoryInSearchPath(SearchedDirs,Dir)>0 then exit;
    SearchedDirs+=';'+Dir;
    Files:=nil;
    try
      CodeToolBoss.DirectoryCachePool.GetListing(Dir,Files,false);
      for aFilename in Files do
      begin
        if (aFilename='') or (aFilename='.') or (aFilename='..') then continue;
        Ext:=LowerCase(ExtractFileExt(aFilename));
        AnUnitName:='';
        case Ext of
        '.ppu','.o','.pas','.pp','.p':
          if IsIncDir then
          begin
            AnUnitName:=ExtractFilename(aFilename);
            if not IsDottedIdentifier(AnUnitName) then continue;
          end;
        '.inc': ;
        else
          continue;
        end;
        NewFile:=TPGInterPkgFile.Create(AppendPathDelim(Dir)+aFilename,
                                        AnUnitName,OwnerInfo);
        FullFiles.Add(NewFile);
        ShortFiles.Add(NewFile);
        if AnUnitName<>'' then
          Units.Add(NewFile);
      end;
    finally
      Files.Free;
    end;
  end;

  procedure CollectFilesOfOwner(OwnerInfo: TPGInterPkgOwnerInfo);
  var
    SearchedDirs: String;
    SearchPath: String;
    p: Integer;
    Dir: String;
  begin
    // find all unit and include FullFiles in src, inc and out dirs
    SearchedDirs:='';
    CollectFilesInDir(OwnerInfo,OwnerInfo.UnitOutDir,SearchedDirs,false);
    SearchPath:=OwnerInfo.SrcDirs;
    p:=1;
    repeat
      Dir:=GetNextDirectoryInSearchPath(SearchPath,p);
      if Dir='' then break;
      CollectFilesInDir(OwnerInfo,Dir,SearchedDirs,false);
    until false;
    SearchPath:=OwnerInfo.IncDirs;
    p:=1;
    repeat
      Dir:=GetNextDirectoryInSearchPath(SearchPath,p);
      if Dir='' then break;
      CollectFilesInDir(OwnerInfo,Dir,SearchedDirs,true);
    until false;
  end;

  procedure RemoveSecondaryFiles;
  // remove each .o file if there is an .ppu file, so that there is only one
  // warning per ppu file
  var
    Node: TAVLTreeNode;
    ONode: TAVLTreeNode;
    OFile: TPGInterPkgFile;
    PPUFileName: String;
    SearchFile: TPGInterPkgFile;
    PPUNode: TAVLTreeNode;
  begin
    Node:=Units.FindLowest;
    while Node<>nil do begin
      // for each .o file
      ONode:=Node;
      Node:=Units.FindSuccessor(Node);
      OFile:=TPGInterPkgFile(ONode.Data);
      if CompareFileExt(OFile.ShortFilename,'o')<>0 then continue;
      // search corresponding .ppu
      PPUFileName:=ChangeFileExt(OFile.FullFilename,'.ppu');
      SearchFile:=TPGInterPkgFile.Create(PPUFileName,'',nil);
      PPUNode:=FullFiles.Find(SearchFile);
      SearchFile.Free;
      if PPUNode=nil then continue;
      // remove .o file
      Units.RemovePointer(OFile);
      ShortFiles.RemovePointer(OFile);
      FullFiles.Delete(ONode);
    end;
  end;

  function OwnerHasDependency(Owner1, Owner2: TPGInterPkgOwnerInfo): boolean;
  // returns true if Owner1 depends on Owner2
  begin
    if Owner1=Owner2 then exit(true);
    if Owner1.Owner is TLazPackage then
    begin
      if Owner2.Owner is TLazPackage then
      begin
        Result:=PackageGraph.FindDependencyRecursively(
          TLazPackage(Owner1.Owner).FirstRequiredDependency,
          TLazPackage(Owner2.Owner))<>nil;
      end else begin
        // Owner1 is package, Owner2 is project/IDE => not possible
        Result:=false;
      end;
    end else begin
      // Owner1 is project or IDE => true
      Result:=true;
    end;
  end;

  function OptionUrAllowsDuplicate(File1, File2: TPGInterPkgFile): boolean;
  begin
    Result:=true;
    if File1.OwnerInfo.HasOptionUr
    and File2.OwnerInfo.HasOptionUr then
      exit;
    if File1.OwnerInfo.HasOptionUr
    and OwnerHasDependency(File2.OwnerInfo,File1.OwnerInfo) then
      exit;
    if File2.OwnerInfo.HasOptionUr
    and OwnerHasDependency(File1.OwnerInfo,File2.OwnerInfo) then
      exit;
    Result:=false;
  end;

  procedure CheckPPUFilesInWrongDirs;
  { Check if a ppu/o of pkg A has a unit (source or ppu) in another package B
    Unless A uses B and B has -Ur or A has -Ur and B uses A
    => IDE: delete+retry or ignore or cancel
    => lazbuild: warn }
  var
    CurNode: TAVLTreeNode;
    CurFile: TPGInterPkgFile;
    Ext: String;
    FirstNodeSameUnitname: TAVLTreeNode;
    OtherNode: TAVLTreeNode;
    OtherFile: TPGInterPkgFile;
    SrcFileNode: TAVLTreeNode;
    SrcFile: TPGInterPkgFile;
    AmbiguousFile: TPGIPAmbiguousCompiledFile;
  begin
    CurNode:=Units.FindLowest;
    FirstNodeSameUnitname:=nil;
    while CurNode<>nil do begin
      CurFile:=TPGInterPkgFile(CurNode.Data);
      if (FirstNodeSameUnitname=nil)
      or (ComparePGInterPkgUnitnames(CurFile,FirstNodeSameUnitname)<>0) then
        FirstNodeSameUnitname:=CurNode;
      CurNode:=Units.FindSuccessor(CurNode);
      Ext:=lowercase(ExtractFileExt(CurFile.ShortFilename));
      if (Ext<>'.ppu') and (Ext<>'.o') then continue;
      // check units with same name

      // ToDo: only one report per ppu

      OtherNode:=FirstNodeSameUnitname;
      while OtherNode<>nil do begin
        OtherFile:=TPGInterPkgFile(OtherNode.Data);
        if (ComparePGInterPkgUnitnames(CurFile,OtherFile)<>0) then break;
        // other unit with same name found
        OtherNode:=Units.FindSuccessor(OtherNode);
        if OtherFile.OwnerInfo=CurFile.OwnerInfo then continue;
        // ppu in one package, unit with same name in another package
        debugln(['CheckPPUFilesInWrongDirs duplicate units found: file1="',CurFile.FullFilename,'"(',CurFile.OwnerInfo.Name,') file2="',OtherFile.FullFilename,'"(',OtherFile.OwnerInfo.Name,')']);
        if OptionUrAllowsDuplicate(CurFile,OtherFile) then continue;
        // check if this package has a source for this ppu
        SrcFileNode:=FirstNodeSameUnitname;
        SrcFile:=nil;
        while SrcFileNode<>nil do begin
          SrcFile:=TPGInterPkgFile(SrcFileNode.Data);
          if (ComparePGInterPkgUnitnames(CurFile,SrcFile)<>0) then
          begin
            SrcFileNode:=nil;
            break;
          end;
          SrcFileNode:=Units.FindSuccessor(SrcFileNode);
          if SrcFile.OwnerInfo<>CurFile.OwnerInfo then continue;
          if FilenameIsPascalUnit(SrcFile.ShortFilename) then
            break; // yes, there is a unit source
        end;
        AmbiguousFile:=TPGIPAmbiguousCompiledFile.Create;
        AmbiguousFile.Filename:=CurFile.FullFilename;
        AmbiguousFile.OwnerInfo:=CurFile.OwnerInfo;
        AmbiguousFile.ConflictFilename:=OtherFile.FullFilename;
        AmbiguousFile.ConflictOwner:=OtherFile.OwnerInfo;
        if SrcFileNode<>nil then
        begin
          // ppu with src in one package, duplicate unit in another package
          // Note: This could be right if the other package does not use the src file
          // Otherwise: duplicate name (-Ur has been checked)
          debugln(['CheckPPUFilesInWrongDirs duplicate units found: file1="',CurFile.FullFilename,'"(',CurFile.OwnerInfo.Name,',source=',SrcFile.ShortFilename,') file2="',OtherFile.FullFilename,'"(',OtherFile.OwnerInfo.Name,')']);
          AmbiguousFile.SrcFilename:=SrcFile.FullFilename;
        end else begin
          // ppu with no src in one package, duplicate unit in another package
          // (-Ur has been checked)
          // => highly unlikely that this is right
          debugln(['CheckPPUFilesInWrongDirs duplicate units found: file1="',CurFile.FullFilename,'"(',CurFile.OwnerInfo.Name,',no source) file2="',OtherFile.FullFilename,'"(',OtherFile.OwnerInfo.Name,')']);
        end;
        AmbiguousCompiledFiles.Add(AmbiguousFile);
      end;
    end;
  end;

  procedure CheckDuplicateSrcFiles;
  { Check if a src file in pkg A exists in another package B
    Unless A uses B and B has -Ur or A has -Ur and B uses A
    => IDE: ignore or cancel
    => lazbuild: warn }
  var
    CurNode: TAVLTreeNode;
    CurFile: TPGInterPkgFile;
    FirstNodeSameShortName: TAVLTreeNode;
    OtherNode: TAVLTreeNode;
    OtherFile: TPGInterPkgFile;
    AmbiguousFile: TPGIPAmbiguousFile;
  begin
    CurNode:=ShortFiles.FindLowest;
    FirstNodeSameShortName:=nil;
    while CurNode<>nil do begin
      CurFile:=TPGInterPkgFile(CurNode.Data);
      if (FirstNodeSameShortName=nil)
      or (ComparePGInterPkgShortFilename(CurFile,FirstNodeSameShortName)<>0) then
        FirstNodeSameShortName:=CurNode;
      CurNode:=Units.FindSuccessor(CurNode);
      // check files with same short name
      OtherNode:=FirstNodeSameShortName;
      while OtherNode<>nil do begin
        OtherFile:=TPGInterPkgFile(OtherNode.Data);
        if (ComparePGInterPkgShortFilename(CurFile,OtherFile)<>0) then break;
        // other file with same short name found
        OtherNode:=Units.FindSuccessor(OtherNode);
        if OtherFile.OwnerInfo=CurFile.OwnerInfo then continue;
        // two files with same short name in different packages
        if OptionUrAllowsDuplicate(CurFile,OtherFile) then continue;
        debugln(['CheckDuplicateSrcFiles duplicate file found: file1="',CurFile.FullFilename,'"(',CurFile.OwnerInfo.Name,') file2="',OtherFile.FullFilename,'"(',OtherFile.OwnerInfo.Name,')']);
        AmbiguousFile:=TPGIPAmbiguousFile.Create;
        AmbiguousFile.Filename:=CurFile.FullFilename;
        AmbiguousFile.OwnerInfo:=CurFile.OwnerInfo;
        AmbiguousFile.ConflictFilename:=OtherFile.FullFilename;
        AmbiguousFile.ConflictOwner:=OtherFile.OwnerInfo;
        AmbiguousSrcFiles.Add(AmbiguousFile);
      end;
    end;
  end;

var
  i: Integer;
begin
  Result:=true;
  FilesChanged:=false;
  if (PkgList=nil) or (PkgList.Count=0) then exit;
  OwnerInfos:=TObjectList.create(true);
  FullFiles:=TAVLTree.Create(@ComparePGInterPkgFullFilenames);
  Units:=TAVLTree.Create(@ComparePGInterPkgUnitnames);
  ShortFiles:=TAVLTree.Create(@ComparePGInterPkgShortFilename);
  AmbiguousCompiledFiles:=TObjectList.create(true);
  AmbiguousSrcFiles:=TObjectList.create(true);
  try
    // get target OS, CPU and LCLWidgetType
    TargetOS:='$(TargetOS)';
    GlobalMacroList.SubstituteStr(TargetOS);
    if TargetOS='' then TargetOS:=GetCompiledTargetOS;
    TargetCPU:='$(TargetCPU)';
    GlobalMacroList.SubstituteStr(TargetCPU);
    if TargetCPU='' then TargetCPU:=GetCompiledTargetCPU;
    LCLWidgetType:='$(LCLWidgetType)';
    GlobalMacroList.SubstituteStr(LCLWidgetType);
    if LCLWidgetType='' then LCLWidgetType:=LCLPlatformDirNames[GetDefaultLCLWidgetType];

    // get search paths
    AddOwnerInfo(IDEObject);
    for i:=0 to PkgList.Count-1 do
      AddOwnerInfo(TObject(PkgList[i]));

    // collect FullFiles
    for i:=0 to OwnerInfos.Count-1 do
      CollectFilesOfOwner(TPGInterPkgOwnerInfo(OwnerInfos[i]));
    RemoveSecondaryFiles;

    // checks
    CheckPPUFilesInWrongDirs;
    CheckDuplicateSrcFiles;
    if (AmbiguousSrcFiles.Count=0)
    and (AmbiguousCompiledFiles.Count=0) then exit;


  finally
    AmbiguousCompiledFiles.Free;
    AmbiguousSrcFiles.Free;
    Units.Free;
    ShortFiles.Free;
    FullFiles.FreeAndClear;
    FullFiles.Free;
    OwnerInfos.Free;
  end;
end;

end.

