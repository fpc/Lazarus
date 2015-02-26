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

  ToDo:
    - Show conflicting sources.
    - allow to delete ppu files
    - save ignore
    - ignore
    - lazbuild: no form, just warn
}
unit InterPkgConflictFiles;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, InterfaceBase, Forms, ComCtrls,
  Controls, ButtonPanel, FileProcs, LazFileUtils, AvgLvlTree, BasicCodeTools,
  DefineTemplates, CodeToolManager,
  // IDEIntf
  ProjectIntf, CompOptsIntf, IDEWindowIntf,
  // IDE
  LazarusIDEStrConsts, CompilerOptions, EnvironmentOpts, IDEProcs,
  TransferMacros, LazConf, PackageDefs, PackageSystem;

type
  TPGInterPkgOwnerInfo = class
  public
    Name: string;
    Owner: TObject;
    HasOptionUr: boolean;
    CompOptions: TBaseCompilerOptions;
    BaseDir: string;
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
    ButtonPanel1: TButtonPanel;
    ConflictsTreeView: TTreeView;
    IDEDialogLayoutStorage1: TIDEDialogLayoutStorage;
  public
    SrcFiles: TObjectList; // list of TPGIPAmbiguousFile
    CompiledFiles: TObjectList; // list of TPGIPAmbiguousCompiledFile
    FilesChanged: boolean;
    procedure Init(TheSrcFiles, TheCompiledFiles: TObjectList);
  end;

function CheckInterPkgFiles(IDEObject: TObject;
  PkgList: TFPList; out FilesChanged: boolean
  ): boolean; // returns false if user cancelled

implementation

{$R *.lfm}

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

procedure TPGIPConflictsDialog.Init(TheSrcFiles, TheCompiledFiles: TObjectList);

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
  SrcFiles:=TheSrcFiles;
  CompiledFiles:=TheCompiledFiles;

  ConflictsTreeView.Items.BeginUpdate;
  ConflictsTreeView.Items.Clear;
  if CompiledFiles.Count>0 then
  begin
    MainNode:=ConflictsTreeView.Items.Add(nil,'Conflicting compiled files');
    for i:=0 to CompiledFiles.Count-1 do
    begin
      CurFile:=TPGIPAmbiguousCompiledFile(CompiledFiles[i]);
      s:=ExtractFilename(CurFile.Filename);
      // file owner
      if CurFile.OwnerInfo.Owner is TLazPackage then
        s+=' of package '+CurFile.OwnerInfo.Name
      else
        s+=' of '+CurFile.OwnerInfo.Name;
      ItemNode:=AddChild(MainNode,s);
      // file path and src
      AddChild(ItemNode,'File: '+CurFile.Filename);
      if CurFile.SrcFilename='' then
        s:='No source found'
      else
        s:='Source file: '+CurFile.SrcFilename;
      AddChild(ItemNode,s);

      // conflict file owner
      if CurFile.ConflictOwner.Owner is TLazPackage then
        s:='Conflict Package: '+CurFile.ConflictOwner.Name
      else
        s:=CurFile.ConflictOwner.Name;
      AddChild(ItemNode,s);
      // conflict file path
      AddChild(ItemNode,'Conflict File: '+CurFile.ConflictFilename);
      if not FilenameIsPascalSource(CurFile.ConflictFilename) then begin
        if CurFile.ConflictSrcFilename='' then
          s:='No source found'
        else
          s:='Source file: '+CurFile.ConflictSrcFilename;
        AddChild(ItemNode,s);
      end;
    end;
    MainNode.Expand(true);
  end;

  ConflictsTreeView.Items.EndUpdate;
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
  FullFiles: TAvgLvlTree; // tree of TPGInterPkgFile sorted for FullFilename
  Units: TAvgLvlTree; // tree of TPGInterPkgFile sorted for AnUnitName
  ShortFiles: TAvgLvlTree; // tree of TPGInterPkgFile sorted for ShortFilename
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
      OwnerInfo.BaseDir:=LazDir;
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
      OwnerInfo.BaseDir:=OwnerInfo.CompOptions.BaseDirectory;
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
    {$IFDEF VerboseCheckInterPkgFiles}
    debugln(['AddOwnerInfo Name="',OwnerInfo.Name,'"',
      ' SrcDirs="',CreateRelativeSearchPath(OwnerInfo.SrcDirs,OwnerInfo.BaseDir),'"',
      ' IncDirs="',CreateRelativeSearchPath(OwnerInfo.IncDirs,OwnerInfo.BaseDir),'"',
      ' UnitOutDir="',CreateRelativeSearchPath(OwnerInfo.UnitOutDir,OwnerInfo.BaseDir),'"',
      '']);
    {$ENDIF}
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
        if CompareFilenames(aFilename,'fpmake.pp')=0 then continue;
        Ext:=LowerCase(ExtractFileExt(aFilename));
        AnUnitName:='';
        case Ext of
        '.ppu','.o','.pas','.pp','.p':
          if not IsIncDir then
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
    Node: TAvgLvlTreeNode;
    ONode: TAvgLvlTreeNode;
    OFile: TPGInterPkgFile;
    PPUFileName: String;
    SearchFile: TPGInterPkgFile;
    PPUNode: TAvgLvlTreeNode;
  begin
    Node:=Units.FindLowest;
    while Node<>nil do begin
      // for each .o file
      ONode:=Node;
      Node:=Node.Successor;
      OFile:=TPGInterPkgFile(ONode.Data);
      if CompareFileExt(OFile.ShortFilename,'o')<>0 then continue;
      // search corresponding .ppu
      PPUFileName:=ChangeFileExt(OFile.FullFilename,'.ppu');
      SearchFile:=TPGInterPkgFile.Create(PPUFileName,'',nil);
      PPUNode:=FullFiles.Find(SearchFile);
      SearchFile.Free;
      if PPUNode=nil then continue;
      // remove .o file
      ShortFiles.RemovePointer(OFile);
      FullFiles.RemovePointer(OFile);
      Units.Delete(ONode);
      OFile.Free;
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

    function FindUnitSource(aPPUFile: TPGInterPkgFile): TPGInterPkgFile;
    // find a unit source (e.g. .pas) in same package

      function FindSrc(Node: TAvgLvlTreeNode; Left: boolean): TPGInterPkgFile;
      begin
        while Node<>nil do begin
          Result:=TPGInterPkgFile(Node.Data);
          if CompareFilenames(ExtractFileNameOnly(Result.ShortFilename),
            ExtractFileNameOnly(aPPUFile.ShortFilename))<>0 then break;
          if (aPPUFile.OwnerInfo=Result.OwnerInfo)
          and FilenameIsPascalSource(Result.ShortFilename) then exit;
          if Left then
            Node:=Node.Precessor
          else
            Node:=Node.Successor;
        end;
        Result:=nil;
      end;

    var
      StartNode: TAvgLvlTreeNode;
    begin
      StartNode:=ShortFiles.FindPointer(aPPUFile);
      Result:=FindSrc(StartNode,true);
      if Result<>nil then exit;
      Result:=FindSrc(StartNode,false);
    end;

  var
    CurNode: TAvgLvlTreeNode;
    aPPUFile: TPGInterPkgFile;
    Ext: String;
    FirstNodeSameUnitname: TAvgLvlTreeNode;
    OtherNode: TAvgLvlTreeNode;
    OtherFile: TPGInterPkgFile;
    SrcFile: TPGInterPkgFile;
    AmbiguousFile: TPGIPAmbiguousCompiledFile;
    OtherSrcFile: TPGInterPkgFile;
  begin
    CurNode:=Units.FindLowest;
    FirstNodeSameUnitname:=nil;
    while CurNode<>nil do begin
      aPPUFile:=TPGInterPkgFile(CurNode.Data);
      if (FirstNodeSameUnitname=nil)
      or (ComparePGInterPkgUnitnames(aPPUFile,TPGInterPkgFile(FirstNodeSameUnitname.Data))<>0) then
        FirstNodeSameUnitname:=CurNode;
      CurNode:=CurNode.Successor;
      Ext:=lowercase(ExtractFileExt(aPPUFile.ShortFilename));
      if (Ext<>'.ppu') and (Ext<>'.o') then continue;
      // check units with same name
      SrcFile:=nil;

      // ToDo: only one report per ppu

      OtherNode:=FirstNodeSameUnitname;
      while OtherNode<>nil do begin
        OtherFile:=TPGInterPkgFile(OtherNode.Data);
        if (ComparePGInterPkgUnitnames(aPPUFile,OtherFile)<>0) then break;
        // other unit with same name found
        OtherNode:=OtherNode.Successor;
        if OtherFile.OwnerInfo=aPPUFile.OwnerInfo then continue;

        if CompareFilenames(aPPUFile.FullFilename,OtherFile.FullFilename)=0 then
        begin
          // two packages share directories
          // it would would require a lenghty codetools check to find out
          // if this is right or wrong
          // => skip
          continue;
        end;

        // ppu in one package, unit with same name in another package
        if OptionUrAllowsDuplicate(aPPUFile,OtherFile) then continue;
        //debugln(['CheckPPUFilesInWrongDirs duplicate units found: file1="',aPPUFile.FullFilename,'"(',aPPUFile.OwnerInfo.Name,') file2="',OtherFile.FullFilename,'"(',OtherFile.OwnerInfo.Name,')']);
        // check if this package has a source for this ppu
        if SrcFile=nil then
          SrcFile:=FindUnitSource(aPPUFile);
        if FilenameIsPascalSource(OtherFile.ShortFilename) then
          OtherSrcFile:=nil
        else
          OtherSrcFile:=FindUnitSource(OtherFile);
        AmbiguousFile:=TPGIPAmbiguousCompiledFile.Create;
        AmbiguousFile.Filename:=aPPUFile.FullFilename;
        AmbiguousFile.OwnerInfo:=aPPUFile.OwnerInfo;
        AmbiguousFile.ConflictFilename:=OtherFile.FullFilename;
        AmbiguousFile.ConflictOwner:=OtherFile.OwnerInfo;
        if SrcFile<>nil then
        begin
          // ppu with src in one package, duplicate unit in another package
          // Note: This could be right if the other package does not use the src file
          // Otherwise: duplicate name (-Ur has been checked)
          debugln(['Warning: CheckPPUFilesInWrongDirs duplicate units found: file1="',aPPUFile.FullFilename,'"(',aPPUFile.OwnerInfo.Name,',source=',SrcFile.ShortFilename,') file2="',OtherFile.FullFilename,'"(',OtherFile.OwnerInfo.Name,')']);
          AmbiguousFile.SrcFilename:=SrcFile.FullFilename;
        end else begin
          // ppu with no src in one package, duplicate unit in another package
          // (-Ur has been checked)
          // => highly unlikely that this is right
          debugln(['Warning: CheckPPUFilesInWrongDirs duplicate units found: file1="',aPPUFile.FullFilename,'"(',aPPUFile.OwnerInfo.Name,',no source) file2="',OtherFile.FullFilename,'"(',OtherFile.OwnerInfo.Name,')']);
        end;
        if OtherSrcFile<>nil then
          AmbiguousFile.ConflictSrcFilename:=OtherSrcFile.FullFilename;
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
    CurNode: TAvgLvlTreeNode;
    CurFile: TPGInterPkgFile;
    FirstNodeSameShortName: TAvgLvlTreeNode;
    OtherNode: TAvgLvlTreeNode;
    OtherFile: TPGInterPkgFile;
    AmbiguousFile: TPGIPAmbiguousFile;
  begin
    CurNode:=ShortFiles.FindLowest;
    FirstNodeSameShortName:=nil;
    while CurNode<>nil do begin
      CurFile:=TPGInterPkgFile(CurNode.Data);
      if (FirstNodeSameShortName=nil)
      or (ComparePGInterPkgShortFilename(CurFile,TPGInterPkgFile(FirstNodeSameShortName.Data))<>0) then
        FirstNodeSameShortName:=CurNode;
      CurNode:=CurNode.Successor;
      if not FilenameIsPascalSource(CurFile.ShortFilename) then continue;
      // check files with same short name
      OtherNode:=FirstNodeSameShortName;
      while OtherNode<>nil do begin
        OtherFile:=TPGInterPkgFile(OtherNode.Data);
        if (ComparePGInterPkgShortFilename(CurFile,OtherFile)<>0) then break;

        if CompareFilenames(CurFile.FullFilename,OtherFile.FullFilename)=0 then
        begin
          // two packages share directories
          // it would would require a lenghty codetools check to find out
          // if this is right or wrong
          // => skip
          continue;
        end;

        // other file with same short name found
        OtherNode:=OtherNode.Successor;
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
  Dlg: TPGIPConflictsDialog;
begin
  Result:=true;
  FilesChanged:=false;
  if (PkgList=nil) or (PkgList.Count=0) then exit;
  OwnerInfos:=TObjectList.create(true);
  FullFiles:=TAvgLvlTree.Create(@ComparePGInterPkgFullFilenames);
  Units:=TAvgLvlTree.Create(@ComparePGInterPkgUnitnames);
  ShortFiles:=TAvgLvlTree.Create(@ComparePGInterPkgShortFilename);
  AmbiguousCompiledFiles:=TObjectList.create(true);
  AmbiguousSrcFiles:=TObjectList.create(true);
  Dlg:=nil;
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

    {$IFDEF VerboseCheckInterPkgFiles}
    debugln(['CheckInterPkgFiles TargetOS=',TargetOS,' TargetCPU=',TargetCPU,' LCLWidgetType=',LCLWidgetType]);
    {$ENDIF}

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

    // show warnings
    Dlg:=TPGIPConflictsDialog.Create(nil);
    Dlg.Init(AmbiguousSrcFiles,AmbiguousCompiledFiles);
    if Dlg.ShowModal<>mrOK then exit(false);
    FilesChanged:=Dlg.FilesChanged;
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

