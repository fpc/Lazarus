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
  CodeToolManager, CodeCache, FileProcs, AvgLvlTree,
  Laz_DOM, Laz_XMLRead, Laz_XMLWrite,
  MacroIntf, PackageIntf, LazHelpIntf, ProjectIntf, LazIDEIntf,
  IDEProcs, PackageDefs, EnvironmentOpts;

type
  { TLazFPDocFile }

  TLazFPDocFile = class
  public
    Filename: string;
    Doc: TXMLdocument;
    ChangeStep: integer;// the CodeBuffer.ChangeStep value, when Doc was build
    CodeBuffer: TCodeBuffer;
    destructor Destroy; override;
  end;
  
  { TLazDocManager }

  TLazDocManager = class
  private
    FDocs: TAvgLvlTree;// tree of loaded TLazFPDocFile
  public
    constructor Create;
    destructor Destroy; override;
    function FindFPDocFile(const Filename: string): TLazFPDocFile;
    function LoadFPDocFile(const Filename: string;
                           UpdateFromDisk, Revert: Boolean;
                           out ADocFile: TLazFPDocFile): Boolean;
    function GetFPDocFilenameForHelpContext(
                                       Context: TPascalHelpContextList): string;
    function GetFPDocFilenameForSource(SrcFilename: string;
                                       ResolveIncludeFiles: Boolean): string;
    procedure FreeDocs;
  end;
  
function CompareLazFPDocFilenames(Data1, Data2: Pointer): integer;
function CompareAnsistringWithLazFPDocFile(Key, Data: Pointer): integer;


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

{ TLazFPDocFile }

destructor TLazFPDocFile.Destroy;
begin
  FreeAndNil(Doc);
  inherited Destroy;
end;

constructor TLazDocManager.Create;
begin
  FDocs:=TAvgLvlTree.Create(@CompareLazFPDocFilenames);
end;

destructor TLazDocManager.Destroy;
begin
  FreeDocs;
  FreeAndNil(FDocs);
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
  Revert: Boolean; out ADocFile: TLazFPDocFile): Boolean;
var
  MemStream: TMemoryStream;
begin
  Result:=false;
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
  
  DebugLn(['TLazDocManager.LoadFPDocFile parsing ',ADocFile.Filename]);

  // parse XML
  ADocFile.ChangeStep:=ADocFile.CodeBuffer.ChangeStep;
  FreeAndNil(ADocFile.Doc);

  MemStream:=TMemoryStream.Create;
  try
    ADocFile.CodeBuffer.SaveToStream(MemStream);
    MemStream.Position:=0;
    ReadXMLFile(ADocFile.Doc, MemStream);
    Result:=true;
  finally
    MemStream.Free;;
  end;
end;

function TLazDocManager.GetFPDocFilenameForHelpContext(
  Context: TPascalHelpContextList): string;
var
  i: Integer;
  SrcFilename: String;
begin
  Result:='';
  if Context=nil then exit;
  for i:=0 to Context.Count-1 do begin
    if Context.Items[i].Descriptor<>pihcFilename then continue;
    SrcFilename:=Context.Items[i].Context;
    Result:=GetFPDocFilenameForSource(SrcFilename,true);
    exit;
  end;
end;

function TLazDocManager.GetFPDocFilenameForSource(SrcFilename: string;
  ResolveIncludeFiles: Boolean): string;
var
  SrcDir: String;
  FPDocName: String;
  SearchPath: String;
  
  procedure AddSearchPath(Paths: string; const BaseDir: string);
  begin
    if Paths='' then exit;
    if not IDEMacros.CreateAbsoluteSearchPath(Paths,BaseDir) then exit;
    if Paths='' then exit;
    SearchPath:=SearchPath+';'+Paths;
  end;
  
  procedure CheckIfInProject(AProject: TLazProject);
  var
    ProjectDirs: String;
    BaseDir: String;
    Add: Boolean;
  begin
    if AProject=nil then exit;
    if AProject.LazDocPaths='' then exit;
    BaseDir:=ExtractFilePath(AProject.ProjectInfoFile);
    if BaseDir='' then exit;

    Add:=false;
    // search in project files
    if (AProject.FindFile(SrcFilename,[pfsfOnlyProjectFiles])<>nil) then begin
      Add:=true;
    end;
    if (not Add) and FilenameIsAbsolute(SrcFilename) then begin
      // search in project directories
      ProjectDirs:=AProject.LazCompilerOptions.OtherUnitFiles+';.';
      if not IDEMacros.CreateAbsoluteSearchPath(ProjectDirs,BaseDir) then exit;
      if FindPathInSearchPath(PChar(SrcDir),length(SrcDir),
        PChar(ProjectDirs),length(ProjectDirs))<>nil
      then
        Add:=true;
    end;
    if Add then
      AddSearchPath(AProject.LazDocPaths,BaseDir);
  end;
  
  procedure CheckIfInAPackage;
  var
    PkgList: TFPList;
    i: Integer;
    APackage: TLazPackage;
    BaseDir: String;
  begin
    if not FilenameIsAbsolute(SrcFilename) then exit;
    
    // get all packages owning the file
    PkgList:=PackageEditingInterface.GetOwnersOfUnit(SrcFilename);
    if PkgList=nil then exit;
    try
      for i:=0 to PkgList.Count-1 do begin
        if TObject(PkgList[i]) is TLazPackage then begin
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
begin
  Result:='';
  
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

  SrcDir:=ExtractFilePath(SrcFilename);

  SearchPath:='';
  CheckIfInProject(LazarusIDE.ActiveProject);
  CheckIfInAPackage;
  CheckIfInLazarus;
  // finally add default paths
  AddSearchPath(EnvironmentOptions.LazDocPaths,'');

  FPDocName:=lowercase(ExtractFileNameOnly(SrcFilename))+'.xml';
  DebugLn(['TLazDocManager.GetFPDocFilenameForSource Search ',FPDocName,' in "',SearchPath,'"']);
  Result:=SearchFileInPath(FPDocName,'',SearchPath,';',ctsfcAllCase);
end;

procedure TLazDocManager.FreeDocs;
begin
  FDocs.FreeAndClear;
end;

end.

