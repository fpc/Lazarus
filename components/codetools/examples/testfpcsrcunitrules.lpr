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
    Write all duplicate ppu files and all duplicate unit source files.
}
program TestFPCSrcUnitRules;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, CustApp, Laz_AVL_Tree,
  // LazUtils
  LazFileUtils, AvgLvlTree, LazLogger,
  // CodeTools
  FileProcs, CodeToolManager, DefineTemplates, CodeToolsConfig;

const
  ConfigFilename = 'codetools.config';
type

  { TTestFPCSourceUnitRules }

  TTestFPCSourceUnitRules = class(TCustomApplication)
  private
    FCheckUnitName: string;
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    procedure Error(Msg: string; DoWriteHelp: Boolean);
    procedure WriteCompilerInfo(ConfigCache: TPCTargetConfigCache);
    procedure WriteNonExistingPPUPaths(ConfigCache: TPCTargetConfigCache);
    procedure WriteDuplicatesInPPUPath(ConfigCache: TPCTargetConfigCache);
    procedure WriteMissingPPUSources(UnitSet: TFPCUnitSetCache);
    procedure WriteDuplicateSources(UnitSet: TFPCUnitSetCache);
    procedure WriteUnitReport(UnitSet: TFPCUnitSetCache; const AnUnitName: string);
    property CheckUnitName: string read FCheckUnitName write FCheckUnitName;
  end;

{ TMyApplication }

procedure TTestFPCSourceUnitRules.DoRun;
var
  ErrorMsg: String;
  CompilerFilename: String;
  TargetOS: String;
  TargetCPU: String;
  FPCSrcDir: String;
  UnitSet: TFPCUnitSetCache;
  ConfigCache: TPCTargetConfigCache;
  Options: TCodeToolsOptions;
  Rescan: Boolean;
  SourceCache: TFPCSourceCache;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hc:T:P:F:u:','help compiler: targetos: targetcpu: fpcsrcdir: checkunit: rescan');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Halt;
  end;

  if not HasOption('F','fpcsrcdir') then
    Error('fpc source directory missing',true);

  if HasOption('c','compiler') then begin
    CompilerFilename:=GetOptionValue('c','compiler');
    CompilerFilename:=CleanAndExpandFilename(CompilerFilename);
  end else begin
    CompilerFilename:=GetDefaultCompilerFilename;
    CompilerFilename:=SearchFileInPath(CompilerFilename,'',
                    GetEnvironmentVariable('PATH'), PathSeparator,ctsfcDefault);
  end;
  TargetOS:=GetOptionValue('T','targetos');
  TargetCPU:=GetOptionValue('P','targetcpu');
  FPCSrcDir:=GetOptionValue('F','fpcsrcdir');
  FPCSrcDir:=CleanAndExpandDirectory(FPCSrcDir);
  CheckUnitName:=GetOptionValue('u','checkunit');
  Rescan:=HasOption('rescan');

  if not FileExistsUTF8(CompilerFilename) then
    Error('compiler file not found: '+CompilerFilename,false);
  if not DirPathExists(FPCSrcDir) then
    Error('FPC source directory not found: '+FPCSrcDir,false);

  Options:=TCodeToolsOptions.Create;
  Options.InitWithEnvironmentVariables;
  if FileExistsUTF8(ConfigFilename) then begin
    writeln('loading ',ConfigFilename);
    Options.LoadFromFile(ConfigFilename);
  end else begin
    writeln('no config yet: ',ConfigFilename);
  end;
  Options.FPCPath:=CompilerFilename;
  Options.FPCOptions:='';
  Options.TargetOS:=TargetOS;
  Options.TargetProcessor:=TargetCPU;
  Options.FPCSrcDir:=FPCSrcDir;

  CodeToolBoss.Init(Options);

  UnitSet:=CodeToolBoss.CompilerDefinesCache.FindUnitSet(CompilerFilename,
                                          TargetOS,TargetCPU,'',FPCSrcDir,true);
  UnitSet.Init;

  //writeln('saving ',ConfigFilename);
  Options.SaveToFile(ConfigFilename);
  Options.Free;

  ConfigCache:=UnitSet.GetConfigCache(false);
  if Rescan then begin
    ConfigCache.Clear;
    SourceCache:=UnitSet.GetSourceCache(false);
    SourceCache.Clear;
    UnitSet.GetUnitToSourceTree(true);
  end;
  WriteCompilerInfo(ConfigCache);
  WriteNonExistingPPUPaths(ConfigCache);
  WriteDuplicatesInPPUPath(ConfigCache);
  WriteMissingPPUSources(UnitSet);
  WriteDuplicateSources(UnitSet);
  if CheckUnitName<>'' then
    WriteUnitReport(UnitSet,CheckUnitName);

  // stop program loop
  Terminate;
end;

constructor TTestFPCSourceUnitRules.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TTestFPCSourceUnitRules.Destroy;
begin
  inherited Destroy;
end;

procedure TTestFPCSourceUnitRules.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln;
  writeln('  -c <compiler file name>, --compiler=<compiler file name>');
  writeln('         Default is to use environment variable PP.');
  writeln('         If this is not set, search for '+GetDefaultCompilerFilename);
  writeln;
  writeln('  -T <target OS>, --targetos=<target OS>');
  writeln('         Default is to use environment variable FPCTARGET.');
  writeln('         If this is not set, use the default of the compiler.');
  writeln;
  writeln('  -P <target CPU>, --targetcpu=<target CPU>');
  writeln('         Default is to use environment variable FPCTARGETCPU.');
  writeln('         If this is not set, use the default of the compiler.');
  writeln;
  writeln('  -F <FPC source directory>, --fpcsrcdir=<FPC source directory>');
  writeln('         Default is to use environment variable FPCDIR.');
  writeln('         There is no default.');
  writeln;
  writeln('  -u <unit name>, --checkunit=<unit name>');
  writeln('         Write a detailed report about this unit.');
  writeln;
  writeln('  --rescan  rescan compiler and FPC sources for this combination');
end;

procedure TTestFPCSourceUnitRules.Error(Msg: string; DoWriteHelp: Boolean);
begin
  writeln('Error: ',Msg);
  if DoWriteHelp then begin
    writeln;
    WriteHelp;
  end;
  Halt;
end;

procedure TTestFPCSourceUnitRules.WriteCompilerInfo(
  ConfigCache: TPCTargetConfigCache);
var
  i: Integer;
  CfgFile: TPCConfigFileState;
begin
  writeln('Compiler=',ConfigCache.Compiler);
  writeln('TargetOS=',ConfigCache.TargetOS);
  writeln('TargetCPU=',ConfigCache.TargetCPU);
  writeln('Options=',ConfigCache.CompilerOptions);
  writeln('RealCompiler=',ConfigCache.RealCompiler);
  writeln('RealTargetOS=',ConfigCache.RealTargetOS);
  writeln('RealTargetCPU=',ConfigCache.RealTargetCPU);
  writeln('RealCompilerInPATH=',ConfigCache.RealCompilerInPath);
  if ConfigCache.ConfigFiles<>nil then begin
    for i:=0 to ConfigCache.ConfigFiles.Count-1 do begin
      CfgFile:=ConfigCache.ConfigFiles[i];
      writeln('Config=',CfgFile.Filename,' Exists=',CfgFile.FileExists);
    end;
  end;
  if (ConfigCache.UnitPaths=nil) or (ConfigCache.UnitPaths.Count=0) then
    writeln('WARNING: no ppu search paths')
  else
    writeln('Number of PPU search paths=',ConfigCache.UnitPaths.Count);
end;

procedure TTestFPCSourceUnitRules.WriteNonExistingPPUPaths(
  ConfigCache: TPCTargetConfigCache);
var
  SearchPaths: TStrings;
  i: Integer;
  Dir: String;
begin
  SearchPaths:=ConfigCache.UnitPaths;
  if SearchPaths=nil then exit;
  for i:=0 to SearchPaths.Count-1 do begin
    Dir:=CleanAndExpandDirectory(SearchPaths[i]);
    if not DirPathExists(Dir) then begin
      writeln('WARNING: ppu search path does not exist: ',SearchPaths[i]);
    end;
  end;
end;

procedure TTestFPCSourceUnitRules.WriteDuplicatesInPPUPath(
  ConfigCache: TPCTargetConfigCache);
var
  i: Integer;
  Directory: String;
  FileInfo: TSearchRec;
  ShortFilename: String;
  Filename: String;
  Ext: String;
  LowerUnitname: String;
  SearchPaths: TStrings;
  IsSource: Boolean;
  IsPPU: Boolean;
  SourceFiles: TStringList;
  Units: TStringToStringTree;
  Item: PStringToStringItem;
  Node: TAVLTreeNode;
begin
  SearchPaths:=ConfigCache.UnitPaths;
  if SearchPaths=nil then exit;
  SourceFiles:=TStringList.Create;
  Units:=TStringToStringTree.Create(false);
  for i:=SearchPaths.Count-1 downto 0 do begin
    Directory:=CleanAndExpandDirectory(SearchPaths[i]);
    if FindFirstUTF8(Directory+FileMask,faAnyFile,FileInfo)=0 then begin
      repeat
        ShortFilename:=FileInfo.Name;
        if (ShortFilename='') or (ShortFilename='.') or (ShortFilename='..') then
          continue;
        Filename:=Directory+ShortFilename;
        Ext:=LowerCase(ExtractFileExt(ShortFilename));
        IsSource:=(Ext='.pas') or (Ext='.pp') or (Ext='.p');
        IsPPU:=(Ext='.ppu');
        if IsSource then
          SourceFiles.Add(Filename);
        if IsSource or IsPPU then begin
          LowerUnitname:=lowercase(ExtractFileNameOnly(Filename));
          if Units.Contains(LowerUnitname) then
            Units[LowerUnitname]:=Units[LowerUnitname]+';'+Filename
          else
            Units[LowerUnitname]:=Filename;
        end;
      until FindNextUTF8(FileInfo)<>0;
    end;
    FindCloseUTF8(FileInfo);
  end;
  if SourceFiles.Count<>0 then begin
    // source files in PPU search path
    writeln;
    writeln('WARNING: source files found in PPU search paths:');
    writeln(SourceFiles.Text);
    writeln;
  end;
  Node:=Units.Tree.FindLowest;
  i:=0;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    Filename:=Item^.Value;
    if System.Pos(';',Filename)>0 then begin
      // duplicate units
      if i=0 then writeln;
      inc(i);
      writeln('HINT: duplicate unit in PPU path: '+Filename);
    end;
    Node:=Units.Tree.FindSuccessor(Node);
  end;
  if i>0 then writeln;
  Units.Free;
  SourceFiles.Free;
end;

procedure TTestFPCSourceUnitRules.WriteMissingPPUSources(
  UnitSet: TFPCUnitSetCache);
var
  UnitToSrc: TStringToStringTree;
  Node: TAVLTreeNode;
  Item: PStringToStringItem;
  ConfigCache: TPCTargetConfigCache;
  aUnitName: String;
  Cnt: Integer;
  Filename: String;
  SourceCache: TFPCSourceCache;
  i: Integer;
  SrcRules: TFPCSourceRules;
  aTree: TStringToStringTree;
begin
  UnitToSrc:=UnitSet.GetUnitToSourceTree(false);
  ConfigCache:=UnitSet.GetConfigCache(false);
  SourceCache:=UnitSet.GetSourceCache(false);
  if ConfigCache.Units<>nil then begin
    Cnt:=0;
    Node:=ConfigCache.Units.Tree.FindLowest;
    while Node<>nil do begin
      Item:=PStringToStringItem(Node.Data);
      aUnitName:=Item^.Name;
      Filename:=Item^.Value;
      if CompareFileExt(Filename,'ppu',false)=0 then begin
        // a ppu in the PPU search path
        if UnitToSrc[aUnitName]='' then begin
          inc(Cnt);
          if Cnt=1 then writeln;
          writeln('WARNING: no source found for PPU file: '+Filename);
          for i:=0 to SourceCache.Files.Count-1 do begin
            if SysUtils.CompareText(ExtractFileNameOnly(SourceCache.Files[i]),aUnitName)=0
            then begin
              writeln('      Candidate: ',SourceCache.Files[i]);
              SrcRules:=UnitSet.GetSourceRules(false);
              aTree:=GatherUnitsInFPCSources(SourceCache.Files,
                ConfigCache.RealTargetOS,ConfigCache.RealTargetCPU,nil,
                SrcRules,aUnitName);
              aTree.Free;
            end;
          end;
        end;
      end;
      Node:=ConfigCache.Units.Tree.FindSuccessor(Node);
    end;
    if Cnt>0 then writeln;
  end;
end;

procedure TTestFPCSourceUnitRules.WriteDuplicateSources(
  UnitSet: TFPCUnitSetCache);
var
  SrcDuplicates: TStringToStringTree;
  Node: TAVLTreeNode;
  Cnt: Integer;
  Item: PStringToStringItem;
  aUnitName: String;
  Files: String;
  Units: TStringToStringTree;
  PPUFile: string;
begin
  SrcDuplicates:=UnitSet.GetSourceDuplicates(false);
  if SrcDuplicates=nil then exit;
  Units:=UnitSet.GetConfigCache(false).Units;

  // first list all duplicates with a ppu file (important)
  if Units<>nil then begin
    Cnt:=0;
    Node:=SrcDuplicates.Tree.FindLowest;
    while Node<>nil do begin
      Item:=PStringToStringItem(Node.Data);
      aUnitName:=Item^.Name;
      Files:=Item^.Value;
      PPUFile:=Units[aUnitName];
      if CompareFileExt(PPUFile,'ppu',false)=0 then begin
        if Cnt=0 then writeln;
        inc(Cnt);
        writeln('WARNING: duplicate source file for ppu ',aUnitName,' files=',Files);
      end;
      Node:=SrcDuplicates.Tree.FindSuccessor(Node);
    end;
    if Cnt>0 then writeln;
  end;

  // then list all duplicates without a ppu file (unimportant)
  Cnt:=0;
  Node:=SrcDuplicates.Tree.FindLowest;
  while Node<>nil do begin
    Item:=PStringToStringItem(Node.Data);
    aUnitName:=Item^.Name;
    Files:=Item^.Value;
    if (Units=nil) or (Units[aUnitName]='') then begin
      if Cnt=0 then writeln;
      inc(Cnt);
      writeln('HINT: duplicate source files: unit=',aUnitName,' files=',Files);
    end;
    Node:=SrcDuplicates.Tree.FindSuccessor(Node);
  end;
  if Cnt>0 then writeln;
end;

procedure TTestFPCSourceUnitRules.WriteUnitReport(UnitSet: TFPCUnitSetCache;
  const AnUnitName: string);
var
  ConfigCache: TPCTargetConfigCache;
  PPUFile: String;
  SourceCache: TFPCSourceCache;
  aTree: TStringToStringTree;
  SrcRules: TFPCSourceRules;
  FPM: TPCFPMFileState;
begin
  writeln;
  writeln('Unit report for ',AnUnitName);
  ConfigCache:=UnitSet.GetConfigCache(false);

  // in ppu search path
  PPUFile:='';
  if ConfigCache.Units<>nil then
    PPUFile:=ConfigCache.Units[AnUnitName];
  if PPUFile='' then
    writeln('  WARNING: ',AnUnitName,' is not in PPU search path')
  else if CompareFileExt(PPUFile,'ppu',false)<>0 then
    writeln('  WARNING: fpc ppu search path has a source and not a ppu for ',AnUnitName,': ',PPUFile)
  else
    writeln('  in PPU search path: ',PPUFile);

  // search in FPC sources
  SourceCache:=UnitSet.GetSourceCache(false);
  SrcRules:=UnitSet.GetSourceRules(false);
  if SourceCache.Files<>nil then begin
    aTree:=GatherUnitsInFPCSources(SourceCache.Files,
      ConfigCache.RealTargetOS,ConfigCache.RealTargetCPU,nil,
      SrcRules,AnUnitName);
    if (aTree=nil) or (aTree.Count=0) then
      writeln('  WARNING: no units in FPC sources: ',SourceCache.Directory)
    else
      writeln('  in FPC source dir: ',aTree[AnUnitName]);
    aTree.Free;
  end else
    writeln('  WARNING: no files in FPC sources: ',SourceCache.Directory);

  // search in FPM
  if ConfigCache.UnitToFPM<>nil then begin
    FPM:=TPCFPMFileState(ConfigCache.UnitToFPM[AnUnitName]);
    if FPM<>nil then begin
      writeln('  in fpm: ',FPM.Name,' File=',FPM.FPMFilename);
      writeln('  fpm source: ',FPM.UnitToSrc[AnUnitName]);
    end;
  end;
end;

var
  Application: TTestFPCSourceUnitRules;
begin
  Application:=TTestFPCSourceUnitRules.Create(nil);
  Application.Title:='TestFPCSrcUnitRules';
  Application.Run;
  Application.Free;
end.

