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

  Author: Mattias Gaertner / Juha Manninen

  Abstract:
    Convert Delphi projects/packages to lazarus projects/packages.
    This was refactored and cleaned from code in unit DelphiProject2Laz.
    Now it is objects oriented and easier to maintain / improve.
}
unit ConvertDelphi;

{$mode objfpc}{$H+}

interface

uses
  // LCL+FCL
  Classes, SysUtils, LCLProc, Forms, Controls, Dialogs, LResources, LConvEncoding,
  FileUtil, contnrs, IniFiles, AVL_Tree,
  // codetools
  CodeToolManager, DefineTemplates, CodeAtom, CodeCache, LinkScanner,
  FileProcs, CodeToolsStructs, FindDeclarationTool,
  // IDEIntf
  ComponentReg, IDEMsgIntf, MainIntf, LazIDEIntf, PackageIntf, ProjectIntf,
  // IDE
  IDEProcs, Project, ProjectDefs, DialogProcs, EditorOptions, CompilerOptions,
  PackageDefs, PackageSystem, PackageEditor, BasePkgManager, LazarusIDEStrConsts,
  // Converter
  ConverterTypes, ConvertSettings, ConvCodeTool, MissingUnits, MissingPropertiesDlg,
  UsedUnits, ReplaceNamesUnit;

const
  SettingDelphiModeTemplName = 'Setting Delphi Mode';

type

  TConvertUnitFlag = (
    cdtlufRenameLowercase, // rename the unit lowercase
    cdtlufCanAbort   // show 'Cancel all' button in error messages using mrAbort
  );
  TConvertUnitFlags = set of TConvertUnitFlag;

  TConvertDelphiPBase = class;

  { TCacheUnitsThread }

  TCacheUnitsThread = class(TThread)
  private
    fPath: string;
    fSearcher: TFileSearcher;
  protected
    procedure Execute; override;
  public
    constructor Create(aConverter: TConvertDelphiPBase; aPath: string);
    destructor Destroy; override;
  public
    property Searcher: TFileSearcher read fSearcher;
  end;

  { TDelphiUnit }

  TDelphiUnit = class
  private
    // Converter for the project or package this unit belongs to.
    // Nil if converting single unit.
    fOwnerConverter: TConvertDelphiPBase;
    fOrigUnitFilename: string;    // Original unit's file name, .pas
    fLazUnitFilename: string;
    // Extension of the new Lazarus file. If empty, gets the original file's ext.
    fLazFileExt: string;
    // Unit's info. Actually TUnitInfo, for projects only.
    fUnitInfo: TObject;
    // Actual code for unit and form file.
    fPascalBuffer: TCodeBuffer;
    fLFMBuffer: TCodeBuffer;
    fFlags: TConvertUnitFlags;
    // Link for codetools, shared by classes that need it.
    fCTLink: TCodeToolLink;
    // For adding, removing and replacing unit names is uses sections.
    fUsedUnitsTool: TUsedUnitsTool;
    function GetDfmFileName: string;
    function CopyAndLoadFile: TModalResult;
    function FixLfmFilenameAndLoad(ADfmFilename: string): TModalResult;
    function ConvertUnitFile: TModalResult;
    function ConvertFormFile: TModalResult;
    function FixIncludeFiles: TModalResult;
    function AskUnitPathFromUser: TModalResult;
  protected
  public
    constructor Create(AOwnerConverter: TConvertDelphiPBase; const AFilename: string;
                       aFlags: TConvertUnitFlags);
    destructor Destroy; override;
    function CheckFailed(PrevResult: TModalResult): TModalResult;
  public
    property LazFileExt: string read fLazFileExt write fLazFileExt;
  end;

  { TConvertDelphiPBase }

  // Takes care of error handling.
  // TConvertDelphiUnit ja TConvertDelphiProjPack inherit from this,
  //  wrapping either one unit or whole project / package conversion.
  TConvertDelphiPBase = class
  private
    // Original unit's, project's or package's file name, .pas .lpi .lpk .dpr .dpk
    fOrigFilename: string;
    fSettings: TConvertSettings;
    fErrorMsg: string;
    // IsConsoleApp is only updated for TConvertDelphiProjPack.
    fIsConsoleApp: Boolean;
    // Units found in user defined paths.
    fCachedUnitNames: TStringToStringTree;
    // Map of case incorrect unit name -> real unit name.
    fCachedRealFileNames: TStringToStringTree;
    // The user selected path when searching missing units.
    fPrevSelectedPath: string;
    // Missing units that are commented automatically in all units.
    fAllCommentedUnits: TStringList;
    function DoMissingUnits(AUsedUnitsTool: TUsedUnitsTool): integer; virtual;
    function GetCachedUnitPath(const AUnitName: string): string;
    procedure ShowEndingMessage(AStatus: TModalResult);
  public
    constructor Create(const AFilename, ADescription: string);
    destructor Destroy; override;
  public
    property ErrorMsg: string read fErrorMsg;
  end;

  { TConvertDelphiUnit }

  // Delphi unit conversion.
  TConvertDelphiUnit = class(TConvertDelphiPBase)
  private
  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    function Convert: TModalResult;
  end;

  { TConvertDelphiProjPack }

  // Base class for Delphi project and package conversion.
  // Code would be cleaner if TProject and TLazPackage inherited from same class.
  // Now they can't share much code.
  TConvertDelphiProjPack = class(TConvertDelphiPBase)
  private
    // Either Project or LazPackage. Typecasted to right types in property getter.
    fProjPack: TObject;
    fLazPFilename: string;             // .lpi or .lpk file name
    fDelphiPFilename: string;          // .dpr or .dpk file name
    fLazPSuffix: string;               // '.lpi' or '.lpk'
    fDelphiPSuffix: string;            // '.dpr' or '.dpk'
    // Unit search path for project settings.
    fUnitSearchPaths: TStringList;
    // Units that are found and will be added to project and converted.
    fUnitsToAddToProject: TStringList;
    fUseThreads: boolean;              // The project/package uses TThread.
    function ConvertSub: TModalResult;
    procedure CleanUpCompilerOptionsSearchPaths(Options: TBaseCompilerOptions);
    procedure SetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
    procedure UnsetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
    function ConvertAllFormFiles(ConverterList: TObjectList): TModalResult;
    function ReadDelphiConfigFiles: TModalResult;
    function ExtractOptionsFromDOF(const DOFFilename: string): TModalResult;
    function ExtractOptionsFromCFG(const CFGFilename: string): TModalResult;
    function DoMissingUnits(AUsedUnitsTool: TUsedUnitsTool): integer; override;
    function AddToProjectLater(AFileName: string): Boolean;
    function CheckPackageDep(AUnitName: string): Boolean;
    function TryAddPackageDep(AUnitName, ADefaultPkgName: string): Boolean;
  protected
    function CreateInstance: TModalResult; virtual; abstract;
    function CreateMainSourceFile: TModalResult; virtual;
    function ConvertMainSourceFile: TModalResult; virtual;
    function FindAllUnits: TModalResult; virtual; abstract;
    function ConvertAllUnits: TModalResult; virtual; abstract;
    function ExtractOptionsFromDelphiSource: TModalResult; virtual; abstract;
    // The following protected funcs are needed only because
    //  Project and LazPackage don't inherit from the same class.
    function GetCompOpts: TBaseCompilerOptions; virtual; abstract;
    function GetCustomDefines: TDefineTemplate; virtual; abstract;
    procedure CustomDefinesChanged; virtual; abstract;
    function GetMainName: string; virtual; abstract;
    function SaveAndMaybeClose(aFilename: string): TModalResult; virtual;
    procedure AddPackageDependency(const PackageName: string); virtual; abstract;
    function FindDependencyByName(const PackageName: string): TPkgDependency; virtual; abstract;
    procedure RemoveNonExistingFiles(RemoveFromUsesSection: boolean); virtual; abstract;
  public
    constructor Create(const AFilename, ADescription: string);
    destructor Destroy; override;
    function Convert: TModalResult;
  public
    property CompOpts: TBaseCompilerOptions read GetCompOpts;
    property CustomDefines: TDefineTemplate read GetCustomDefines;
    property MainName: string read GetMainName;
  end;


  { TConvertDelphiProject }

  // Delphi project conversion.
  TConvertDelphiProject = class(TConvertDelphiProjPack)
  private
    // Resource code
    fMainUnitConverter: TDelphiUnit;
    function AddUnit(AFileName: string; out OutUnitInfo: TUnitInfo): TModalResult;
    function CheckUnitForConversion(aFileName: string): Boolean;
    function GetLazProject: TProject;
    procedure SetLazProject(const AValue: TProject);
  protected
    function CreateInstance: TModalResult; override;
    function CreateMainSourceFile: TModalResult; override;
    function ConvertMainSourceFile: TModalResult; override;
    function FindAllUnits: TModalResult; override;
    function ConvertAllUnits: TModalResult; override;
    function ExtractOptionsFromDelphiSource: TModalResult; override;
    // Fake Project / Package virtual methods.
    function GetCompOpts: TBaseCompilerOptions; override;
    function GetCustomDefines: TDefineTemplate; override;
    procedure CustomDefinesChanged; override;
    function GetMainName: string; override;
    function SaveAndMaybeClose(Filename: string): TModalResult; override;
    procedure AddPackageDependency(const PackageName: string); override;
    function FindDependencyByName(const PackageName: string): TPkgDependency; override;
    procedure RemoveNonExistingFiles(RemoveFromUsesSection: boolean); override;
  public
    constructor Create(const aProjectFilename: string);
    destructor Destroy; override;
  public
    property LazProject: TProject read GetLazProject write SetLazProject;
  end;


  { TConvertDelphiPackage }

  // Delphi package conversion.
  TConvertDelphiPackage = class(TConvertDelphiProjPack)
  private
    // Delphi package code.
    fDpkCode: TCodeBuffer;
    function LoadDPKFile: TModalResult;
    function GetLazPackage: TLazPackage;
    procedure SetLazPackage(const AValue: TLazPackage);
  protected
    function CreateInstance: TModalResult; override;
    function FindAllUnits: TModalResult; override;
    function ConvertAllUnits: TModalResult; override;
    function ExtractOptionsFromDelphiSource: TModalResult; override;
    // Fake Project / Package virtual methods.
    function GetCompOpts: TBaseCompilerOptions; override;
    function GetCustomDefines: TDefineTemplate; override;
    procedure CustomDefinesChanged; override;
    function GetMainName: string; override;
    procedure AddPackageDependency(const PackageName: string); override;
    function FindDependencyByName(const PackageName: string): TPkgDependency; override;
    procedure RemoveNonExistingFiles(RemoveFromUsesSection: boolean); override;
  public
    constructor Create(const aPackageFilename: string);
    destructor Destroy; override;
  public
    property LazPackage: TLazPackage read GetLazPackage write SetLazPackage;
  end;

  { TConvertedDelphiProjectDescriptor }

  TConvertedDelphiProjectDescriptor = class(TProjectEmptyProgramDescriptor)
  private
  public
    function InitProject(AProject: TLazProject): TModalResult; override;
  end;

  { TConvertedDelphiPackageDescriptor }
{
  TConvertedDelphiPackageDescriptor = class(TPackageDescriptor)
  private
  public      // ToDo
    procedure InitPackage(APackage: TLazPackage); override;
  end;
}
  // Some global functions from delphiunit2laz are not (yet) converted to class methods.
  function ConvertDelphiAbsoluteToRelativeFile(const Filename: string;
                                               AProject: TProject): string;
  function ExpandDelphiFilename(const Filename: string; AProject: TProject): string;
  function ExpandDelphiSearchPath(const SearchPath: string;
                                  AProject: TProject): string;


implementation

function ConvertDelphiAbsoluteToRelativeFile(const Filename: string; AProject: TProject): string;
// often projects use paths near to their project directory. For example:
//   A project /somewhere/MyProjects/project1.dpr
// and a path C:\Delphi\MyProj\folder can mean, that the relative path is 'folder'
var
  ProjectDir: String;
  ShortProjectDir: String;
  p: LongInt;
begin
  Result:='';       // Default: ignore absolute paths
  ProjectDir:=AProject.ProjectDirectory;
  ShortProjectDir:=PathDelim+ExtractFileName(ChompPathDelim(ProjectDir))+PathDelim;
  p:=System.Pos(ShortProjectDir,Filename);
  if (p>0) then
    Result:=copy(Filename,p+length(ShortProjectDir),length(Filename));
end;

function ExpandDelphiFilename(const Filename: string; AProject: TProject): string;
var
  p: LongInt;
begin
  Result:=Filename;
  if Result='' then exit;
  Result:=TrimFilename(SetDirSeparators(Result));
  // check for $(Delphi) macro
  p:=System.Pos('$(DELPHI)',Result);
  if p>0 then begin
    // Delphi features are provided by FPC and Lazarus -> ignore
    Result:='';
  end;
  // check for other macros
  p:=System.Pos('$(',Result);
  if p>0 then begin
    // path macros are not supported -> ignore
    Result:='';
  end;
  if FilenameIsWinAbsolute(Result) then begin
    // absolute filenames are not portable
    Result:=ConvertDelphiAbsoluteToRelativeFile(Result,AProject);
  end;
  // change PathDelim
  Result:=TrimFilename(SetDirSeparators(Result));
end;

function ExpandDelphiSearchPath(const SearchPath: string;
  AProject: TProject): string;
var
  Paths: TStrings;
  i: Integer;
  CurPath: String;
  j: Integer;
begin
  Result:='';
  Paths:=SplitString(SearchPath,';');
  if Paths=nil then exit;
  try
    // expand Delphi paths
    for i:=0 to Paths.Count-1 do
      Paths[i]:=ExpandDelphiFilename(Paths[i],AProject);
    // remove doubles
    for i:=Paths.Count-1 downto 0 do begin
      CurPath:=Paths[i];
      if (CurPath='') then
        Paths.Delete(i)
      else begin
        j:=i-1;
        while (j>=0) and (CompareText(CurPath,Paths[i])<>0) do dec(j);
        if j>=0 then
          Paths.Delete(i);
      end;
    end;
    Result:='';
    for i:=0 to Paths.Count-1 do begin
      if i>0 then Result:=Result+';';
      Result:=Result+Paths[i];
    end;
  finally
    Paths.Free;
  end;
end;

type

  { TUnitsSearcher }

  TUnitsSearcher = class(TFileSearcher)
  private
    fConverter: TConvertDelphiPBase;
  protected
    procedure DoFileFound; override;
  public
    constructor Create(aConverter: TConvertDelphiPBase);
  end;

{ TUnitsSearcher }

constructor TUnitsSearcher.Create(aConverter: TConvertDelphiPBase);
begin
  inherited Create;
  fConverter := aConverter;
end;

procedure TUnitsSearcher.DoFileFound;
var
  RelPath, SubPath, sUnitName, fn: String;
begin
  RelPath:=FileUtil.CreateRelativePath(FileName, fConverter.fSettings.MainPath);
  SubPath:=ExtractFilePath(RelPath);
  fn:=ExtractFileName(RelPath);
  sUnitName:=ExtractFileNameOnly(fn);
  if (SubPath<>'') and (sUnitName<>'') then begin
    //DebugLn(['RelPath=',RelPath,'SubPath=',SubPath,'fn=',fn,'sUnitName=',sUnitName]);
    // Map path by unit name.
    fConverter.fCachedUnitNames[sUnitName]:=SubPath;
    // Map real unit name by uppercase unit name.
    fConverter.fCachedRealFileNames[UpperCase(sUnitName)]:=fn;
  end;
end;

{ TCacheUnitsThread }

constructor TCacheUnitsThread.Create(aConverter: TConvertDelphiPBase; aPath: string);
begin
  inherited Create(True);
  FreeOnTerminate:=True;
  // Create searcher already now. Its Stop method can be called anytime.
  fSearcher:=TUnitsSearcher.Create(aConverter);
  // The parent directory to be scanned
  fPath:=aPath;
end;

destructor TCacheUnitsThread.Destroy;
begin
  fSearcher.Free;
  inherited Destroy;
end;

procedure TCacheUnitsThread.Execute;
// Scan for unit files. This assumes that cache is not used while updating it.
// The main GUI thread must wait for this thread before starting conversion.

  function IsRootPath(APath: String): Boolean;
  // Crude function, it maybe needs support for UNC drives
  var
    D: String;
    Len: Integer;
  begin
    D := ExtractFileDrive(APath);
    Len := Length(D);
    System.Delete(APath, 1, Len);
    Result := (Length(APath) = 1) and (APath[1] in AllowDirectorySeparators);
  end;

begin
  //If a project is in a subfolder of the root, fPath will be root path.
  //Do not search the entire drive, or we may find Borland VCL files and convert them too
  if IsRootPath(fPath) then
    Sleep(1)     // Let the main thread execute, avoid possible synchr. problems.
  else
    // Scan for files and store to fCachedUnitNames, path relative to fSettings.MainPath.
    fSearcher.Search(fPath, '*.pas');
end;

{ TDelphiUnit }

constructor TDelphiUnit.Create(AOwnerConverter: TConvertDelphiPBase;
                            const AFilename: string; AFlags: TConvertUnitFlags);
begin
  inherited Create;
  fOwnerConverter:=AOwnerConverter;
  fOrigUnitFilename:=AFilename;
  fFlags:=AFlags;
  fLazFileExt:='';
  fUnitInfo:=nil;
  if not LazarusIDE.BeginCodeTools then
    IDEMessagesWindow.AddMsg(lisConvDelphiBeginCodeToolsFailed, '', -1);
  fCTLink:=Nil;                     // Will be created later.
  fUsedUnitsTool:=Nil;
end;

destructor TDelphiUnit.Destroy;
begin
  fUsedUnitsTool.Free;
  fCTLink.Free;
  inherited Destroy;
end;

function TDelphiUnit.GetDfmFileName: string;
begin
  Result:=ChangeFileExt(fOrigUnitFilename,'.dfm');
  if FileExistsUTF8(Result) then exit;
  Result:=ChangeFileExt(fOrigUnitFilename,'.DFM');
  if FileExistsUTF8(Result) then exit;
  Result:=ChangeFileExt(fOrigUnitFilename,'.xfm');
  if FileExistsUTF8(Result) then exit;
  Result:=ChangeFileExt(fOrigUnitFilename,'.XFM');
  if not FileExistsUTF8(Result) then
    Result:='';
end;

function TDelphiUnit.CopyAndLoadFile: TModalResult;
begin
  IDEMessagesWindow.AddMsg(Format(lisConvDelphiConvertingFile,
                                  [fOrigUnitFilename]), '', -1);
  Application.ProcessMessages;
  // ConvertUnit in place. File must be writable.
  Result:=CheckFileIsWritable(fOrigUnitFilename,[mbAbort]);
  if Result<>mrOk then exit;
  // close Delphi unit file in editor.
  Result:=LazarusIDE.DoCloseEditorFile(fOrigUnitFilename,[cfSaveFirst]);
  if Result<>mrOk then exit;
  // Copy/rename fLazUnitFilename based on fOrigUnitFilename.
  Result:=fOwnerConverter.fSettings.RenameDelphiToLazFile(fOrigUnitFilename,
                  fLazFileExt, fLazUnitFilename, cdtlufRenameLowercase in fFlags);
  if Result<>mrOK then exit;
  // Read the code in.
  fPascalBuffer:=nil;
  Result:=LoadCodeBuffer(fPascalBuffer,fLazUnitFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk],true);
  if Result<>mrOk then exit;
  // Change encoding to UTF-8
  if fPascalBuffer.DiskEncoding<>EncodingUTF8 then begin
    IDEMessagesWindow.AddMsg(Format(lisConvDelphiChangedEncodingToUTF8,
                                    [fPascalBuffer.DiskEncoding]), '', -1);
    fPascalBuffer.DiskEncoding:=EncodingUTF8; // Takes effect when buffer is saved.
  end;
  // Create a shared link for codetools.
  Assert(fCTLink=Nil, 'fCTLink should be Nil in CopyAndLoadFile');
  fCTLink:=TCodeToolLink.Create(fPascalBuffer);
  fCTLink.Settings:=fOwnerConverter.fSettings;
  fCTLink.AskAboutError:=fOwnerConverter is TConvertDelphiProjPack;
  // Fix include file names.
  Result:=FixIncludeFiles;
  if Result<>mrOk then exit;
  // Create a tool for missing units.
  fUsedUnitsTool:=TUsedUnitsTool.Create(fCTLink, fOrigUnitFilename);
  if fOwnerConverter is TConvertDelphiProjPack then
    with TConvertDelphiProjPack(fOwnerConverter) do begin
      fUsedUnitsTool.OnCheckPackageDependency:=@CheckPackageDep;
      fUsedUnitsTool.IsConsoleApp:=fIsConsoleApp;
    end;
  if fOwnerConverter is TConvertDelphiProject then
    with TConvertDelphiProject(fOwnerConverter) do
      fUsedUnitsTool.OnCheckUnitForConversion:=@CheckUnitForConversion;
end;

function TDelphiUnit.FixLfmFilenameAndLoad(ADfmFilename: string): TModalResult;
var
  LfmFilename: string;     // Lazarus .LFM file name.
  DFMConverter: TDFMConverter;
  TempLFMBuffer: TCodeBuffer;
begin
  Result:=mrOK;
  fLFMBuffer:=nil;
  if ADfmFilename<>'' then begin
    Result:=LazarusIDE.DoCloseEditorFile(ADfmFilename,[cfSaveFirst]);
    if Result<>mrOk then exit;
  end;
  if fOwnerConverter.fSettings.SameDfmFile then
    LfmFilename:=ADfmFilename
  else
    with fOwnerConverter do begin
      // Create a form file name based on the unit file name.
      LfmFilename:=fSettings.DelphiToLazFilename(fOrigUnitFilename, '.lfm',
                                                 cdtlufRenameLowercase in fFlags);
      if ADfmFilename<>'' then begin
        if FileExistsUTF8(LfmFilename) then
          if (FileAgeUTF8(LfmFilename)<FileAgeUTF8(ADfmFilename)) then
            DeleteFileUTF8(LfmFilename); // .lfm is older than .dfm -> remove .lfm
        if not FileExistsUTF8(LfmFilename) then begin
          // TODO: update project
          if fSettings.SupportDelphi and not fSettings.SameDfmFile then
            Result:=CopyFileWithErrorDialogs(ADfmFilename,LfmFilename,[mbAbort])
          else
            Result:=fSettings.RenameFile(ADfmFilename,LfmFilename);
          if Result<>mrOK then exit;
        end;
      end;
    end;
  // ConvertUnit .dfm file to .lfm file (without context type checking)
  if FileExistsUTF8(LfmFilename) then begin
    DFMConverter:=TDFMConverter.Create(IDEMessagesWindow);
    try
      Result:=DFMConverter.ConvertDfmToLfm(LfmFilename);
      if Result<>mrOk then exit;
    finally
      DFMConverter.Free;
    end;
    // Change encoding to UTF-8
    Result:=LoadCodeBuffer(TempLFMBuffer,LfmFilename,
                           [lbfCheckIfText,lbfUpdateFromDisk],true);
    if TempLFMBuffer.DiskEncoding<>EncodingUTF8 then begin
      IDEMessagesWindow.AddMsg(Format(lisConvDelphiChangedEncodingToUTF8,
                                      [TempLFMBuffer.DiskEncoding]), '', -1);
      TempLFMBuffer.DiskEncoding:=EncodingUTF8;
      TempLFMBuffer.Save;
    end;
    // Read form file code in.
    if not fOwnerConverter.fSettings.SameDfmFile then begin
      Result:=LoadCodeBuffer(fLFMBuffer,LfmFilename,
                             [lbfCheckIfText,lbfUpdateFromDisk],true);
    end;
  end;
end;

function TDelphiUnit.ConvertUnitFile: TModalResult;

  function ReduceMissingUnits: TModalResult;
  // Find or comment out some / all of missing units.
  begin
    Result:=mrOK;
    // Try to find from subdirectories scanned earlier.
    if fOwnerConverter.DoMissingUnits(fUsedUnitsTool)=0 then exit;
    // Comment out automatically units that were commented in other files.
    fUsedUnitsTool.MainUsedUnits.CommentAutomatic(fOwnerConverter.fAllCommentedUnits);
    fUsedUnitsTool.ImplUsedUnits.CommentAutomatic(fOwnerConverter.fAllCommentedUnits);
    // Remove omitted units from MissingUnits.
    fUsedUnitsTool.MainUsedUnits.OmitUnits;
    fUsedUnitsTool.ImplUsedUnits.OmitUnits;
    if fUsedUnitsTool.MissingUnitCount=0 then exit;
    // Interactive dialog for searching unit.
    Result:=AskUnitPathFromUser;
  end;

var
  DfmFilename: string;     // Delphi .DFM file name.
  ConvTool: TConvDelphiCodeTool;
begin
  // Get DFM file name and close it in editor.
  DfmFilename:=GetDfmFileName;
  Result:=FixLfmFilenameAndLoad(DfmFilename);
  if Result<>mrOk then exit;
  if fOwnerConverter.fSettings.UnitsReplaceMode<>rlDisabled then begin
    // Find and prepare the missing units. Don't replace yet.
    Result:=fUsedUnitsTool.Prepare;
    if Result<>mrOk then exit;
    if fUsedUnitsTool.MissingUnitCount>0 then begin
      Result:=ReduceMissingUnits;
      if Result<>mrOk then exit;
    end;
  end;
  // Do the actual code conversion.
  ConvTool:=TConvDelphiCodeTool.Create(fCTLink);
  try
    ConvTool.IsConsoleApp:=fOwnerConverter.fIsConsoleApp;
    ConvTool.ResAction:=raNone;
    if FileExistsUTF8(ChangeFileExt(fLazUnitFilename, '.res')) then
      ConvTool.ResAction:=raLowerCase
    else if not FileExistsUTF8(ChangeFileExt(fLazUnitFilename, '.RES')) then
      ConvTool.ResAction:=raDelete;  // No lower- or uppercase version exists -> delete
    ConvTool.HasFormFile:=DfmFilename<>'';
    ConvTool.AddUnitEvent:=@fUsedUnitsTool.AddUnitIfNeeded;
    Result:=ConvTool.Convert;
  finally
    ConvTool.Free;
  end;
end;

function TDelphiUnit.ConvertFormFile: TModalResult;
var
  LfmFixer: TLFMFixer;
begin
  // Fix the LFM file and the pascal unit, updates fPascalBuffer and fLFMBuffer.
  if fLFMBuffer<>nil then begin
    IDEMessagesWindow.AddMsg(Format(lisConvDelphiRepairingFormFile,
                                    [fLFMBuffer.Filename]), '', -1);
    Application.ProcessMessages;
    LfmFixer:=TLFMFixer.Create(fCTLink,fLFMBuffer,@IDEMessagesWindow.AddMsg);
    try
      LfmFixer.Settings:=fOwnerConverter.fSettings;
      LfmFixer.UsedUnitsTool:=fUsedUnitsTool;
      LfmFixer.RootMustBeClassInUnit:=true;
      LfmFixer.RootMustBeClassInIntf:=true;
      LfmFixer.ObjectsMustExist:=true;
      if LfmFixer.ConvertAndRepair<>mrOk then begin
        LazarusIDE.DoJumpToCompilerMessage(-1,true);
        fOwnerConverter.fErrorMsg:='Problems when repairing form file '
                                  +fOrigUnitFilename;
        exit(mrAbort);
      end;
    finally
      LfmFixer.Free;
    end;
    // save LFM file
    Result:=SaveCodeBufferToFile(fLFMBuffer,fLFMBuffer.Filename);
    if Result<>mrOk then exit;
  end;
  // After other changes: add, remove, fix and comment out units in uses sections.
  IDEMessagesWindow.AddMsg(Format(lisConvDelphiFixingUsedUnits,
                                  [fOrigUnitFilename]), '', -1);
  Result:=fUsedUnitsTool.ConvertUsed;
  if Result<>mrOk then exit;
  Result:=mrOk;
end;

function TDelphiUnit.AskUnitPathFromUser: TModalResult;
// Ask the user what to do with missing units.
var
  TryAgain: Boolean;
  CacheUnitsThread: TCacheUnitsThread;
  UnitDirDialog: TSelectDirectoryDialog;
begin
  with fUsedUnitsTool do
  repeat
    TryAgain:=False;
    Result:=AskMissingUnits(MainUsedUnits.MissingUnits, ImplUsedUnits.MissingUnits,
        ExtractFileName(fLazUnitFilename), fOwnerConverter.fSettings.SupportDelphi);
    case Result of
      // mrOK means: comment out.
      mrOK: begin
        if fOwnerConverter is TConvertDelphiProjPack then
          MoveMissingToComment(fOwnerConverter.fAllCommentedUnits)
        else
          MoveMissingToComment(Nil);
      end;
      // mrYes means: Search for unit path.
      mrYes: begin
        UnitDirDialog:=TSelectDirectoryDialog.Create(nil);
        try
          UnitDirDialog.InitialDir:=fOwnerConverter.fPrevSelectedPath;
          UnitDirDialog.Title:=lisConvDelphiAllSubDirsScanned;
          if UnitDirDialog.Execute then begin
            fOwnerConverter.fPrevSelectedPath:=ExtractFilePath(UnitDirDialog.Filename);
            // Add the new path to project if missing units are found.
            // We use a thread here only to reuse its code. No parallel operations now.
            CacheUnitsThread:=TCacheUnitsThread.Create(fOwnerConverter,
                                              fOwnerConverter.fPrevSelectedPath);
            CacheUnitsThread.Start;
            CacheUnitsThread.WaitFor; // Make sure the thread has finished before continuing.
            TryAgain:=fOwnerConverter.DoMissingUnits(fUsedUnitsTool)>0;
          end
          else
            TryAgain:=true;  // User canceled. Stay with the same unit.
        finally
          UnitDirDialog.Free;
        end;
        Result:=mrOK;        // Caller will check for Result<>mrOK
      end;
      // Skip this unit.
      mrIgnore: Exit;
      // Abort the whole conversion.
      mrAbort: begin
        fOwnerConverter.fErrorMsg:='User selected to end conversion with file '
                                  +fOrigUnitFilename;
        Exit;
      end;
    end;
  until not TryAgain;
end;

function TDelphiUnit.FixIncludeFiles: TModalResult;
// fix include filenames
var
  FoundIncludeFiles: TStrings;
  MissingIncludeFilesCodeXYPos: TFPList;
  CodePos: PCodeXYPosition;
  Msg: string;
  i: Integer;
  OldChange: Boolean;
begin
  Result:=mrOk;
  FoundIncludeFiles:=Nil;
  MissingIncludeFilesCodeXYPos:=Nil;
  OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
  LazarusIDE.OpenEditorsOnCodeToolChange:=False;
  with fCTLink do
  try
    CodeTool.CheckDirectoryCache;
    if CodeTool.DirectoryCache=nil then exit;
    if CodeTool.FixIncludeFilenames(Code,SrcCache,FoundIncludeFiles,MissingIncludeFilesCodeXYPos)
    then begin
      if Assigned(FoundIncludeFiles) then begin
        FoundIncludeFiles.Delimiter:=';';
        FoundIncludeFiles.StrictDelimiter:=True;
        Msg:='Repairing include files : ' + FoundIncludeFiles.DelimitedText;
        IDEMessagesWindow.AddMsg(Msg, '', -1);
      end;
    end
    else begin
      if MissingIncludeFilesCodeXYPos<>nil then begin
        for i:=0 to MissingIncludeFilesCodeXYPos.Count-1 do begin
          CodePos:=PCodeXYPosition(MissingIncludeFilesCodeXYPos[i]);
          Msg:=Format(lisConvDelphiMissingIncludeFile,
                 [CodePos^.Code.Filename,IntToStr(CodePos^.y),IntToStr(CodePos^.x)]);
          IDEMessagesWindow.AddMsg(Msg, '', -1);
        end;
      end;
      Application.ProcessMessages;
      fOwnerConverter.fErrorMsg:='Problems when fixing include files in file '
                                +fOrigUnitFilename;
      Result:=mrCancel;
    end;
  finally
    FoundIncludeFiles.Free;
    CodeCache.FreeListOfPCodeXYPosition(MissingIncludeFilesCodeXYPos);
    LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
  end;
end;

function TDelphiUnit.CheckFailed(PrevResult: TModalResult): TModalResult;
begin
  Result:=PrevResult;
  if Result=mrCancel then begin
    Result:=QuestionDlg(lisConvDelphiFailedConvertingUnit,
        Format(lisConvDelphiFailedToConvertUnit, [LineEnding,fOrigUnitFilename,LineEnding]),
        mtWarning, [mrIgnore, lisIgnoreAndContinue, mrAbort], 0);
    case Result  of
      mrIgnore : Result:=mrOK;
      mrAbort : fOwnerConverter.fErrorMsg:='User selected to end conversion with file '
                                           +fOrigUnitFilename;
    end;
  end;
end;


{ TConvertDelphiPBase }

constructor TConvertDelphiPBase.Create(const AFilename, ADescription: string);
begin
  inherited Create;
  fOrigFilename:=AFilename;
  fSettings:=TConvertSettings.Create(ADescription);
  fSettings.MainFilename:=fOrigFilename;
  fIsConsoleApp:=False;                   // Default = GUI app.
  fPrevSelectedPath:=fSettings.MainPath;
end;

destructor TConvertDelphiPBase.Destroy;
begin
  fSettings.Free;
  inherited Destroy;
end;

function TConvertDelphiPBase.DoMissingUnits(AUsedUnitsTool: TUsedUnitsTool): integer;
begin
  Result := 0;
end;

function TConvertDelphiPBase.GetCachedUnitPath(const AUnitName: string): string;
begin
  Result:=fCachedUnitNames[AUnitName];
end;

procedure TConvertDelphiPBase.ShowEndingMessage(AStatus: TModalResult);
begin
  if AStatus=mrOk then
    IDEMessagesWindow.AddMsg(lisConvDelphiConversionReady, '', -1)
  else begin
    if fErrorMsg<>'' then
      IDEMessagesWindow.AddMsg(Format(lisConvDelphiError,[fErrorMsg]),'',-1)
    else if CodeToolBoss.ErrorMessage<>'' then
      IDEMessagesWindow.AddMsg(Format(lisConvDelphiError,[CodeToolBoss.ErrorMessage]),'',-1);
    IDEMessagesWindow.AddMsg(lisConvDelphiConversionAborted, '', -1);
  end;
end;

{ TConvertDelphiUnit }

constructor TConvertDelphiUnit.Create(const AFilename: string);
begin
  inherited Create(AFilename, lisConvDelphiConvertDelphiUnit);
end;

destructor TConvertDelphiUnit.Destroy;
begin
  inherited Destroy;
end;

function TConvertDelphiUnit.Convert: TModalResult;
var
  DelphiUnit: TDelphiUnit;
begin
  IDEMessagesWindow.Clear;
  DelphiUnit := TDelphiUnit.Create(Self, fOrigFilename, []);
  try
    Result:=fSettings.RunForm(Nil);
    if Result=mrOK then begin
      with DelphiUnit do
      try
        Result:=CopyAndLoadFile;
        if Result<>mrOK then Exit;
        Result:=ConvertUnitFile;
        if Result<>mrOK then Exit;
        Result:=SaveCodeBufferToFile(fPascalBuffer,fLazUnitFilename);
        if Result<>mrOK then Exit;
        Result:=ConvertFormFile;
        if Result<>mrOK then Exit;
        Result:=LazarusIDE.DoSaveEditorFile(fLazUnitFilename,[]);
      except
        on e: EDelphiConverterError do begin
          fErrorMsg:=e.Message;
        end;
        else begin
          fErrorMsg:=CodeToolBoss.ErrorMessage;
        end;
        Result:=mrAbort;
      end;
    end;
  finally
    ShowEndingMessage(Result);
    DelphiUnit.Free;
  end;
end;

{ TConvertDelphiProjPack }

constructor TConvertDelphiProjPack.Create(const AFilename, ADescription: string);
begin
  inherited Create(AFilename, ADescription);
  fUseThreads:=False;
  fUnitSearchPaths:=TStringList.Create;
  fUnitSearchPaths.Delimiter:=';';
  fUnitSearchPaths.StrictDelimiter:=True;
  fCachedUnitNames:=TStringToStringTree.Create(False);
  fCachedRealFileNames:=TStringToStringTree.Create(True);
  fAllCommentedUnits:=TStringList.Create;
  fAllCommentedUnits.Sorted:=True;
  fUnitsToAddToProject:=TStringList.Create;
  fUnitsToAddToProject.Sorted:=True;
end;

destructor TConvertDelphiProjPack.Destroy;
begin
  fUnitsToAddToProject.Free;
  fAllCommentedUnits.Free;
  fCachedRealFileNames.Free;
  fCachedUnitNames.Free;
  fUnitSearchPaths.Free;
  inherited Destroy;
end;

function TConvertDelphiProjPack.Convert: TModalResult;
// Create or update a lazarus project (.lpi+.lpr) or package, convert source files.
var
  // The initial unit name cache is done in a thread so that GUI shows at once.
  CacheUnitsThread: TCacheUnitsThread;
  StartTime, EndTime: TDateTime;
  s: string;
begin
  if CompareFileExt(fOrigFilename,'.dproj',false)=0 then begin
    fErrorMsg:=
      '.dproj file is not supported yet. The file is used by Delphi 2007 and newer.'+
      ' Please select a .dpr file for projects or .dpk file for packages.';
    Exit(mrCancel);
  end;
  IDEMessagesWindow.Clear;
  // Start scanning unit files one level above project path. The GUI will appear
  // without delay but then we must wait for the thread before continuing.
  CacheUnitsThread:=TCacheUnitsThread.Create(Self,
                       TrimFilename(fSettings.MainPath+'..'+DirectorySeparator));
  try
  try
    Result:=fSettings.RunForm(CacheUnitsThread); // Get settings from user.
    if Result=mrOK then begin
      StartTime:=Now;
      // create/open lazarus project or package file
      fLazPFilename:=fSettings.DelphiToLazFilename(fOrigFilename, fLazPSuffix, false);

      // Find Delphi project / package file name
      if CompareFileExt(fOrigFilename,fDelphiPSuffix,false)=0 then
        fDelphiPFilename:=fOrigFilename
      else
        fDelphiPFilename:=ChangeFileExt(fOrigFilename,fDelphiPSuffix);
      if not FileExistsUTF8(fDelphiPFilename) then
        fDelphiPFilename:=FindDiskFileCaseInsensitive(fOrigFilename);
// ? fDelphiPFilename:=CodeToolBoss.DirectoryCachePool.FindDiskFilename(fDelphiPFilename);

      // Actual conversion.
      Result:=ConvertSub;
    end;
  except
    on e: EDelphiConverterError do begin
      fErrorMsg:=e.Message;
    end;
    else begin
      fErrorMsg:=CodeToolBoss.ErrorMessage;
    end;
    Result:=mrAbort;
  end;
  finally
    EndTime:=Now;
    s:=FormatDateTime('hh:nn:ss', EndTime-StartTime);
    if s<>'00:00:00' then
      IDEMessagesWindow.AddMsg(Format(lisConvDelphiConversionTook, [s]), '', -1);
    ShowEndingMessage(Result);
  end;
end;

function TConvertDelphiProjPack.ConvertSub: TModalResult;
begin
  // Project / package instance.
  Result:=CreateInstance;
  if Result<>mrOK then exit;
  // create main source file (.lpr for project) (only copy, no conversion)
  Result:=CreateMainSourceFile; // Create project's LPR file.
  if Result<>mrOK then exit;
  // read config files (they often contain clues about paths, switches and defines)
  Result:=ReadDelphiConfigFiles;
  if Result<>mrOK then exit;

  RemoveNonExistingFiles(false);
  CleanUpCompilerOptionsSearchPaths(CompOpts);
  // LCL dependency should be added automatically later for GUI applications
  AddPackageDependency('LCL');            // but in some special cases it is not.
  // ToDo: make an option to add NoGUI to Project.CompilerOptions.LCLWidgetType.
  if fProjPack is TProject then
    PkgBoss.OpenProjectDependencies(fProjPack as TProject, true);
  CustomDefinesChanged;

  SetCompilerModeForDefineTempl(CustomDefines);
  try
    if Result<>mrOK then exit;
    // get all options from the .dpr or .dpk file
    Result:=ExtractOptionsFromDelphiSource;
    if Result<>mrOK then exit;
    Result:=FindAllUnits;              // find all files and save the project.
    if Result<>mrOK then exit;
    Result:=ConvertMainSourceFile;     // Convert project's LPR file.
    if Result<>mrOK then exit;
    Result:=ConvertAllUnits;           // convert all files.
  finally
    UnsetCompilerModeForDefineTempl(CustomDefines);
  end;
end;

function TConvertDelphiProjPack.ConvertAllFormFiles(ConverterList: TObjectList): TModalResult;
// Call Convert.ConvertFormFile for all units.
var
  Converter: TDelphiUnit;
  i: Integer;
begin
  if not fSettings.SameDfmFile then begin
    IDEMessagesWindow.AddMsg('', '', -1);
    IDEMessagesWindow.AddMsg(lisConvDelphiRepairingFormFiles, '', -1);
  end;
  Application.ProcessMessages;
  Screen.Cursor:=crHourGlass;
  try
    for i:=0 to ConverterList.Count-1 do begin
      Converter:=TDelphiUnit(ConverterList[i]); // Converter created in cycle1.
      Result:=Converter.ConvertFormFile;
      Result:=Converter.CheckFailed(Result);
      if Result<>mrOK then exit;
      // Finally save and maybe close the file.
      Result:=SaveAndMaybeClose(Converter.fLazUnitFilename);
      if Result<>mrOK then exit;
    end;
  finally
    Screen.Cursor:=crDefault;
  end;
  Result:=mrOK;
end;

function TConvertDelphiProjPack.ReadDelphiConfigFiles: TModalResult;
var
  FN, s: String;
begin
  Result:=mrOk;
  FN:=MainName;
  if FN<>'' then begin
    // read .dof file
    s:=FindDiskFileCaseInsensitive(ChangeFileExt(FN,'.dof'));
    if s<>'' then begin
      Result:=ExtractOptionsFromDOF(s);
      if Result<>mrOk then exit;
    end;
    // read .cfg file
    s:=FindDiskFileCaseInsensitive(ChangeFileExt(FN,'.cfg'));
    if s<>'' then
      Result:=ExtractOptionsFromCFG(s);
  end;
end;

function TConvertDelphiProjPack.ExtractOptionsFromDOF(const DOFFilename: string): TModalResult;
// parse .dof file and put options into Project/LazPackage
var
  IniFile: TIniFile;

  function ReadDirectory(const Section, Ident: string): string;
  begin
    Result:=IniFile.ReadString(Section,Ident,'');
    Result:=ExpandDelphiFilename(Result,fProjPack as TProject);
  end;

  function ReadSearchPath(const Section, Ident: string): string;
  var
    SearchPath: String;
  begin
    SearchPath:=IniFile.ReadString(Section,Ident,'');
    Result:=ExpandDelphiSearchPath(SearchPath,fProjPack as TProject);
  end;

  procedure AddPackDep(const DelphiPkgName, DelphiPkgNames, LazarusPkgName: string);
  begin
    if DelphiPkgName='' then exit;
    if System.Pos(';'+lowercase(DelphiPkgName)+';',
                  ';'+lowercase(DelphiPkgNames)+';')>0 then begin
      AddPackageDependency(LazarusPkgName);
      IDEMessagesWindow.AddMsg(
          Format(lisConvDelphiAddedPackageDependency, [LazarusPkgName]), '', -1);
    end;
  end;

  procedure ReadDelphiPackages;
  var
    DelphiPackages: String;
    Pkgs: TStrings;
    i: Integer;
  begin
    DelphiPackages:=IniFile.ReadString('Directories','Packages','');
    Pkgs:=SplitString(DelphiPackages,';');
    if Pkgs=nil then exit;
    try
      for i:=0 to Pkgs.Count-1 do
        AddPackDep(Pkgs[i],'rtl,dbrtl','FCL');
    finally
      Pkgs.Free;
    end;
  end;

  procedure AddSearchPath(const SearchPath: string);
  begin
    CompOpts.IncludePath:=MergeSearchPaths(CompOpts.IncludePath,SearchPath);
    CompOpts.Libraries:=MergeSearchPaths(CompOpts.Libraries,SearchPath);
    CompOpts.OtherUnitFiles:=MergeSearchPaths(CompOpts.OtherUnitFiles,SearchPath);
    CompOpts.ObjectPath:=MergeSearchPaths(CompOpts.ObjectPath,SearchPath);
    CompOpts.DebugPath:=MergeSearchPaths(CompOpts.DebugPath,SearchPath);
  end;

var
  OutputDir: String;
  SearchPath: String;
  DebugSourceDirs: String;
begin
  try
    IniFile:=TIniFile.Create(UTF8ToSys(DOFFilename));
    try
      // output directory
      if fProjPack is TProject then begin
        OutputDir:=ReadDirectory('Directories','OutputDir');
        if (OutputDir<>'') then
          CompOpts.UnitOutputDirectory:=OutputDir;
      end;

      // search path
      SearchPath:=ReadSearchPath('Directories','SearchPath');
      if (SearchPath<>'') then
        AddSearchPath(SearchPath);

      // debug source dirs
      DebugSourceDirs:=ReadSearchPath('Directories','DebugSourceDirs');
      if DebugSourceDirs<>'' then
        CompOpts.DebugPath:=MergeSearchPaths(CompOpts.DebugPath,DebugSourceDirs);

      // packages
      ReadDelphiPackages;

      if fProjPack is TProject then begin
        if IniFile.ReadString('Linker','ConsoleApp','')='0' then
          // does not need a windows console
          (fProjPack as TProject).LazCompilerOptions.Win32GraphicApp:=true;
      end;
    finally
      IniFile.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ExtractOptionsFromDOF failed reading "'+DOFFilename+'" '+E.Message);
    end;
  end;
  Result:=mrOk;
end;

function TConvertDelphiProjPack.ExtractOptionsFromCFG(const CFGFilename: string): TModalResult;
var
  sl: TStringList;
  i: Integer;
  Line, s: string;
  c: char;
begin
  try
    sl:=TStringList.Create;
    try
      sl.LoadFromFile(UTF8ToSys(CFGFilename));
      for i:=0 to sl.Count-1 do begin
        Line:=sl[i];
        if Line='' then continue;
        if (Line[1]<>'-') or (length(Line)<2) then continue;
        c:=Line[2];
        if (c='U') or (c='I') then begin
          s:=ExpandDelphiSearchPath(copy(Line,4,length(Line)-4),fProjPack as TProject);
          if s<>'' then
            case c of
              'U': CompOpts.OtherUnitFiles:=MergeSearchPaths(CompOpts.OtherUnitFiles,s);
              'I': CompOpts.IncludePath:=MergeSearchPaths(CompOpts.IncludePath,s);
            end;
        end
      end;
    finally
      sl.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ExtractOptionsFromDOF failed reading "'+CFGFilename+'" '+E.Message);
    end;
  end;
  Result:=mrOk;
end;

procedure TConvertDelphiProjPack.SetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
begin
  if DefTempl.FindChildByName(SettingDelphiModeTemplName)<>nil then exit;
  DefTempl.ReplaceChild(CreateDefinesForFPCMode(SettingDelphiModeTemplName,cmDELPHI));
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TConvertDelphiProjPack.UnsetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
begin
  if DefTempl.FindChildByName(SettingDelphiModeTemplName)=nil then exit;
  DefTempl.DeleteChild(SettingDelphiModeTemplName);
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TConvertDelphiProjPack.CleanUpCompilerOptionsSearchPaths(Options: TBaseCompilerOptions);
var
  BasePath, s: String;

  function CleanProjectSearchPath(const SearchPath: string): string;
  begin
    Result:=RemoveNonExistingPaths(SearchPath,BasePath);
    Result:=MinimizeSearchPath(Result);
  end;

begin
  BasePath:=Options.BaseDirectory;
  Options.OtherUnitFiles:=CleanProjectSearchPath(Options.OtherUnitFiles);
  Options.IncludePath:=CleanProjectSearchPath(Options.IncludePath);
  Options.Libraries:=CleanProjectSearchPath(Options.Libraries);
  Options.ObjectPath:=CleanProjectSearchPath(Options.ObjectPath);
  Options.SrcPath:=CleanProjectSearchPath(Options.SrcPath);
  if fSettings.DelphiDefine then begin
    s:='-dDelphi7 -dVer150 -dCompiler6_Up';
    Options.CustomOptions:=s;
    IDEMessagesWindow.AddMsg(Format(lisConvDelphiAddedCustomOptionDefines, [s]), '', -1);
  end;
end;

function TConvertDelphiProjPack.DoMissingUnits(AUsedUnitsTool: TUsedUnitsTool): integer;
// Locate unit names from earlier cached list or from packages.
// Return the number of units still missing.

  procedure DoMissingSub(AUsedUnits: TUsedUnits);
  var
    mUnit, sUnitPath, RealFileName, RealUnitName: string;
    i: Integer;
  begin
    for i:= AUsedUnits.MissingUnits.Count-1 downto 0 do begin
      mUnit:=AUsedUnits.MissingUnits[i];
      sUnitPath:=GetCachedUnitPath(mUnit);
      if sUnitPath<>'' then begin
        // Found from cached paths: add unit path to project's settings.
        with CompOpts do begin
          OtherUnitFiles:=MergeSearchPaths(OtherUnitFiles,sUnitPath);
          IncludePath:=MergeSearchPaths(IncludePath,sUnitPath);
        end;
        // Rename a unit with different casing if needed.
        RealFileName:=fCachedRealFileNames[UpperCase(mUnit)];
        RealUnitName:=ExtractFileNameOnly(RealFileName);
        if (RealUnitName<>'') and (RealUnitName<>mUnit) then
          AUsedUnits.UnitsToFixCase[mUnit]:=RealUnitName;
        // Will be added later to project.
        AddToProjectLater(sUnitPath+RealFileName);
        AUsedUnits.MissingUnits.Delete(i);      // No more missing, delete from list.
      end
      else if CheckPackageDep(mUnit) then
        AUsedUnits.MissingUnits.Delete(i);
    end;
  end;

begin
  DoMissingSub(AUsedUnitsTool.MainUsedUnits);
  DoMissingSub(AUsedUnitsTool.ImplUsedUnits);
  Result:=AUsedUnitsTool.MissingUnitCount;
end;

function TConvertDelphiProjPack.AddToProjectLater(AFileName: string): Boolean;
var
  x: Integer;
begin
  Result:=not fUnitsToAddToProject.Find(AFileName,x);
  if Result then
    fUnitsToAddToProject.Add(AFileName);
end;

function TConvertDelphiProjPack.CheckPackageDep(AUnitName: string): Boolean;
// Check if the given unit can be found in existing packages. Add a dependency if found.
// This is called only if the unit is reported as missing.
var
  Pack: TPkgFile;
  Dep: TPkgDependency;
  s: String;
begin
  Result:=False;
  Pack:=PackageGraph.FindUnitInAllPackages(AUnitName, True);
  if Assigned(Pack) then begin
    // Found from package: add package to project dependencies and open it.
    s:=Pack.LazPackage.Name;
    if s='LCLBase' then
      s:='LCL';
    Dep:=FindDependencyByName(s);
    if not Assigned(Dep) then begin
      AddPackageDependency(s);
      IDEMessagesWindow.AddMsg(Format(lisConvDelphiAddedPackageDependency, [s]), '', -1);
      Dep:=FindDependencyByName(s);
      if Assigned(Dep) then
        PackageGraph.OpenDependency(Dep,false);
      Result:=True;
    end;
  end else begin;
    // ToDo: Install the required package automatically from a repository...
  end;
end;

function TConvertDelphiProjPack.TryAddPackageDep(AUnitName, ADefaultPkgName: string): Boolean;
var
  Dep: TPkgDependency;
begin
  Result:=False;
  if ADefaultPkgName<>'' then begin
    Dep:=FindDependencyByName(ADefaultPkgName);
    if not Assigned(Dep) then begin
      Result:=CheckPackageDep(AUnitName); // Add dependency based on unit name (default is ignored)
      if not Result then
        // Package was not found. Add a message about a package that must be installed.
        IDEMessagesWindow.AddMsg(Format(lisConvDelphiPackageRequired, [ADefaultPkgName]), '', -1);
    end;
  end;
end;

function TConvertDelphiProjPack.CreateMainSourceFile: TModalResult;
begin
  Result:=mrOK; // Do nothing. Overridden in project.
end;

function TConvertDelphiProjPack.ConvertMainSourceFile: TModalResult;
begin
  Result:=mrOK; // Do nothing. Overridden in project.
end;

function TConvertDelphiProjPack.SaveAndMaybeClose(aFilename: string): TModalResult;
begin
  Result:=mrOK; // Do nothing. Overridden in project.
end;


{ TConvertDelphiProject }

constructor TConvertDelphiProject.Create(const aProjectFilename: string);
begin
  inherited Create(aProjectFilename, lisConvDelphiConvertDelphiProject);
  fLazPSuffix:='.lpi';
  fDelphiPSuffix:='.dpr';
  fMainUnitConverter:=nil;
end;

destructor TConvertDelphiProject.Destroy;
begin
  fMainUnitConverter.Free;
  inherited Destroy;
end;

function TConvertDelphiProject.CreateInstance: TModalResult;
// Open or create a project. If .lpi file does not exist, create it.
var
  Desc: TConvertedDelphiProjectDescriptor;
begin
  Result:=mrOk;
  if FileExistsUTF8(fLazPFilename) then begin
    // there is already a lazarus project -> open it, if not already open
    if (Project1=nil) or (CompareFilenames(Project1.ProjectInfoFile,fLazPFilename)<>0) then
      Result:=LazarusIDE.DoOpenProjectFile(fLazPFilename,[]);
  end else begin
    // create a new lazarus project
    Desc:=TConvertedDelphiProjectDescriptor.Create;
    try
      Result:=LazarusIDE.DoNewProject(Desc);
    finally
      Desc.Free;
    end;
    if Assigned(Project1) then
      Project1.ProjectInfoFile:=fLazPFilename;
  end;
  LazProject:=Project1;
  if Result<>mrOk then exit;
  // save to disk (this makes sure, all editor changes are saved too)
  LazProject.SkipCheckLCLInterfaces:=True; // Don't add Interfaces unit automatically.
  Result:=LazarusIDE.DoSaveProject([]);
end;

function TConvertDelphiProject.CreateMainSourceFile: TModalResult;
// if .lpr does not exists, copy the .dpr file to the .lpr
// adds the .lpr as main unit to the project, if not already done
var
  MainUnitInfo: TUnitInfo;
  ConvTool: TConvDelphiCodeTool;
begin
  // Converter for main LPR file.
  fMainUnitConverter:=TDelphiUnit.Create(Self,fOrigFilename,[]);
  if fSettings.SupportDelphi then
    fMainUnitConverter.LazFileExt:=ExtractFileExt(fOrigFilename)
  else
    fMainUnitConverter.LazFileExt:='.lpr';
  Result:=fMainUnitConverter.CopyAndLoadFile;
  if Result<>mrOk then exit;
  if LazProject.MainUnitInfo=nil then begin
    // add .lpr file to project as main unit
    MainUnitInfo:=TUnitInfo.Create(fMainUnitConverter.fPascalBuffer);
    MainUnitInfo.DefaultSyntaxHighlighter:=
                    ExtensionToLazSyntaxHighlighter(fMainUnitConverter.LazFileExt);
    MainUnitInfo.IsPartOfProject:=true;
    LazProject.AddFile(MainUnitInfo,false);
    LazProject.MainFileID:=0;
  end else begin
    // replace main unit in project
    LazProject.MainUnitInfo.Source:=fMainUnitConverter.fPascalBuffer;
  end;
  // Scan LPR file for directives. Sets fIsConsoleApp flag.
  ConvTool:=TConvDelphiCodeTool.Create(fMainUnitConverter.fCTLink);
  try
    fIsConsoleApp:=ConvTool.FindApptypeConsole;
  finally
    ConvTool.Free;
  end;
  CompOpts.Win32GraphicApp := not fIsConsoleApp;
  with fMainUnitConverter do begin
    fUsedUnitsTool.IsMainFile:=True;
    fUsedUnitsTool.IsConsoleApp:=fIsConsoleApp;
    Result:=LazarusIDE.DoOpenEditorFile(fLazUnitFilename,0,0,[ofQuiet]);
    if Result<>mrOK then exit;
  end;
  Result:=mrOk;
end;

function TConvertDelphiProject.ConvertMainSourceFile: TModalResult;
begin
  // Loading was done earlier. Now just convert.
  Result:=fMainUnitConverter.ConvertUnitFile;
  if Result<>mrOk then exit;
  Result:=fMainUnitConverter.ConvertFormFile;
end;

function TConvertDelphiProject.AddUnit(AFileName: string;
                                       out OutUnitInfo: TUnitInfo): TModalResult;
// add new unit to project
var
  CurUnitInfo: TUnitInfo;
  RP, PureUnitName: String;
begin
  Result:=mrOK;
  OutUnitInfo:=nil;
  if not FilenameIsAbsolute(AFileName) then
    AFileName:=AppendPathDelim(LazProject.ProjectDirectory)+AFileName;
  AFileName:=TrimFilename(AFileName);
  if not FileExistsUTF8(AFileName) then
    exit(mrNo);
  // Create relative search path for project settings.
  RP:=ExtractFilePath(CreateRelativePath(AFileName, LazProject.ProjectDirectory));
  if (RP<>'') and (fUnitSearchPaths.IndexOf(RP)<0) then
    fUnitSearchPaths.Add(RP);
  PureUnitName:=ExtractFileNameOnly(AFileName);
  if fSettings.OmitProjUnits.Contains(PureUnitName) then
  begin
    fMainUnitConverter.fUsedUnitsTool.Remove(PureUnitName);
    IDEMessagesWindow.AddMsg(Format(lisConvDelphiProjOmittedUnit,[PureUnitName]), '', -1);
    TryAddPackageDep(PureUnitName, fSettings.OmitProjUnits[PureUnitName]);
  end
  else begin
    // Check unitname and create UnitInfo.
    CurUnitInfo:=LazProject.UnitInfoWithFilename(AFileName);
    if CurUnitInfo=nil then begin
      if FilenameIsPascalUnit(AFileName) then begin
        CurUnitInfo:=LazProject.UnitWithUnitname(PureUnitName);
        if CurUnitInfo<>nil then begin
          Result:=QuestionDlg(lisConvDelphiUnitnameExistsTwice,
            Format(lisConvDelphiThereAreTwoUnitsWithTheSameUnitname,
                   [LineEnding, CurUnitInfo.Filename, LineEnding, AFileName, LineEnding]),
            mtWarning, [mrYes,lisConvDelphiRemoveFirst,mrNo,lisConvDelphiRemoveSecond,
                        mrIgnore,lisConvDelphiKeepBoth,mrAbort], 0);
          case Result of
            mrYes: CurUnitInfo.IsPartOfProject:=false;
            mrNo:  exit(mrNo);
            mrIgnore: ;
            else begin
              fErrorMsg:='User selected to end conversion with file '+fOrigFilename;
              exit(mrAbort);
            end;
          end;
        end;
      end;
      CurUnitInfo:=TUnitInfo.Create(nil);
      CurUnitInfo.Filename:=AFileName;
      LazProject.AddFile(CurUnitInfo,false);
    end;
    CurUnitInfo.IsPartOfProject:=true;
    OutUnitInfo:=CurUnitInfo;
  end;
end;

function TConvertDelphiProject.CheckUnitForConversion(aFileName: string): Boolean;
var
  UnitInfo: TUnitInfo;
begin
  // Units in project directory but not part of the project are not reported
  //  as missing and would not be converted. Now add them to the project.
  if ExtractFilePath(aFileName)=LazProject.ProjectDirectory then begin
    UnitInfo:=LazProject.UnitInfoWithFilename(aFileName);
    Result:=Assigned(UnitInfo);
    if not Result then
      AddToProjectLater(aFileName);  // Will be added later to project.
  end;
end;

function TConvertDelphiProject.FindAllUnits: TModalResult;
var
  FoundUnits, MisUnits, NormalUnits: TStrings;
  i: Integer;
  CurFilename: string;
  AllPath: string;
  p: LongInt;
  ui: TUnitInfo;
begin
  Screen.Cursor:=crHourGlass;
  FoundUnits:=nil;
  MisUnits:=nil;
  NormalUnits:=nil;
  try
    IDEMessagesWindow.AddMsg('', '', -1);
    IDEMessagesWindow.AddMsg(lisConvDelphiFindAllUnitFiles, '', -1);
    Application.ProcessMessages;
    if not CodeToolBoss.FindDelphiProjectUnits(fMainUnitConverter.fPascalBuffer,
                                         FoundUnits, MisUnits, NormalUnits) then
    begin
      LazarusIDE.DoJumpToCodeToolBossError;
      fErrorMsg:='Problems when trying to find all units from project file '+fOrigFilename;
      exit(mrCancel);
    end;
    try        // Add all units to the project
      for i:=0 to FoundUnits.Count-1 do begin
        CurFilename:=FoundUnits[i];
        p:=System.Pos(' in ',CurFilename);
        if p>0 then
          CurFilename:=copy(CurFilename,p+4,length(CurFilename));
        if CurFilename='' then continue;
        Result:=AddUnit(SwitchPathDelims(CurFilename, True), ui);
        if Result=mrAbort then
          exit;
      end;
    finally
      AllPath:=fUnitSearchPaths.DelimitedText;
      // Set unit and include paths for project
      with LazProject.CompilerOptions do begin
        OtherUnitFiles:=MergeSearchPaths(OtherUnitFiles,AllPath);
        IncludePath:=MergeSearchPaths(IncludePath,AllPath);
      end;
      // Clear caches
      LazProject.DefineTemplates.SourceDirectoriesChanged;
    end;
    // Save project
    Result:=LazarusIDE.DoSaveProject([sfQuietUnitCheck]);
    if Result<>mrOk then exit;
  finally
    FoundUnits.Free;
    MisUnits.Free;
    NormalUnits.Free;
    Screen.Cursor:=crDefault;
  end;
  Result:=mrOk;
end;

function TConvertDelphiProject.ConvertAllUnits: TModalResult;
var
  ConvUnits: TObjectList;       // List of ConvertDelphiUnits.

  function ConvertOne(AUnitInfo: TUnitInfo): TModalResult;
  var
    Converter: TDelphiUnit;
  begin
    Converter:=TDelphiUnit.Create(Self, AUnitInfo.Filename,[]);
    try
      Converter.fUnitInfo:=AUnitInfo;
      ConvUnits.Add(Converter);
      Result:=Converter.CopyAndLoadFile;
      if Result<>mrOK then exit;
      Result:=Converter.CheckFailed(Result);
      if Result<>mrOK then exit;
      Result:=Converter.ConvertUnitFile;
    except
      DebugLn(['TConvertDelphiProject.ConvertAllUnits: Exception happened while converting ',
               Converter.fLazUnitFilename]);
      ConvUnits.Remove(Converter);
      raise;
    end;
  end;

var
  CurUnitInfo: TUnitInfo;
  i: Integer;
begin
  Result:=mrOk;
  ConvUnits:=TObjectList.Create;
  try
  try
  try
    // convert all units and fix .lfm files
    IDEMessagesWindow.AddMsg('', '', -1);
    IDEMessagesWindow.AddMsg(lisConvDelphiConvertingProjPackUnits, '', -1);
    Application.ProcessMessages;
    for i:=0 to LazProject.UnitCount-1 do begin
      CurUnitInfo:=LazProject.Units[i];
      // Main LPR file was converted earlier.
      if CurUnitInfo.IsPartOfProject and (CurUnitInfo<>LazProject.MainUnitInfo) then begin
        Result:=ConvertOne(CurUnitInfo);
        if Result=mrIgnore then Continue;
        if Result=mrAbort then Exit;
      end;
    end;
    // During conversion there were more units added to be converted.
    IDEMessagesWindow.AddMsg('', '', -1);
    IDEMessagesWindow.AddMsg(lisConvDelphiConvertingFoundUnits, '', -1);
    Application.ProcessMessages;
    for i:=0 to fUnitsToAddToProject.Count-1 do begin
      Result:=AddUnit(fUnitsToAddToProject[i], CurUnitInfo);
      if Result=mrNo then Continue;
      if Result=mrAbort then Exit;
      if Assigned(CurUnitInfo) then begin
        Result:=ConvertOne(CurUnitInfo);
        if Result=mrIgnore then Continue;
        if Result=mrAbort then Exit;
      end;
    end;
    if Result=mrOK then begin
      if fUseThreads then begin
        Result:=fMainUnitConverter.fUsedUnitsTool.AddThreadSupport;
        if Result<>mrOK then exit;
      end;
    end;
  except
    IDEMessagesWindow.AddMsg('', '', -1);
    IDEMessagesWindow.AddMsg('- '+lisConvDelphiExceptionDuringConversion, '', -1);
    DebugLn('- '+lisConvDelphiExceptionDuringConversion);
    raise;
  end;
  finally
    if Result=mrOk then begin
      // Try to convert form files also in case of an exception.
      // Unit name replacements etc. are implemented there.
      Result:=ConvertAllFormFiles(ConvUnits);
      // Finally save project once more
      Result:=LazarusIDE.DoSaveProject([sfQuietUnitCheck]);
    end;
  end;
  finally
    ConvUnits.Free;  // Owns and frees converter objects.
  end;
end;

function TConvertDelphiProject.ExtractOptionsFromDelphiSource: TModalResult;
begin
  // TODO: remove compiler directives and put them into project/package
  if fDelphiPFilename<>'' then begin
  end;
  Result:=mrOk;
end;

// The following funcs are needed only because Project and LazPackage
//  don't inherit from the same class.

function TConvertDelphiProject.GetLazProject: TProject;
begin
  Result:=fProjPack as TProject;
end;

procedure TConvertDelphiProject.SetLazProject(const AValue: TProject);
begin
  fProjPack:=AValue;
end;

function TConvertDelphiProject.GetCompOpts: TBaseCompilerOptions;
begin
  Result:=(fProjPack as TProject).CompilerOptions;
end;

function TConvertDelphiProject.GetCustomDefines: TDefineTemplate;
begin
  Result:=(fProjPack as TProject).DefineTemplates.CustomDefines;
end;

procedure TConvertDelphiProject.CustomDefinesChanged;
begin
  (fProjPack as TProject).DefineTemplates.CustomDefinesChanged;
end;

function TConvertDelphiProject.GetMainName: string;
begin
  Result:='';
  if Assigned(LazProject.MainUnitInfo) then
    Result:=(fProjPack as TProject).MainUnitInfo.Filename;
end;

function TConvertDelphiProject.SaveAndMaybeClose(Filename: string): TModalResult;
var
  UnitIndex: Integer;
  AnUnitInfo: TUnitInfo;
begin
  Result:=mrOk;
  if Filename='' then exit;
  UnitIndex:=LazProject.IndexOfFilename(Filename, [pfsfOnlyEditorFiles]);
  if UnitIndex<0 then exit;
  AnUnitInfo:=LazProject.Units[UnitIndex];
  if AnUnitInfo.OpenEditorInfoCount=0 then exit;
  Result:=LazarusIDE.DoSaveEditorFile(AnUnitInfo.OpenEditorInfo[0].EditorComponent,
                                      [sfCheckAmbiguousFiles,sfQuietUnitCheck]);
  if not fSettings.KeepFileOpen then
    Result:=LazarusIDE.DoCloseEditorFile(AnUnitInfo.OpenEditorInfo[0].EditorComponent,
                                         [cfQuiet]);
end;

procedure TConvertDelphiProject.AddPackageDependency(const PackageName: string);
begin
  (fProjPack as TProject).AddPackageDependency(PackageName);
end;

function TConvertDelphiProject.FindDependencyByName(const PackageName: string): TPkgDependency;
begin
  Result:=(fProjPack as TProject).FindDependencyByName(PackageName);
end;

procedure TConvertDelphiProject.RemoveNonExistingFiles(RemoveFromUsesSection: boolean);
begin
  (fProjPack as TProject).RemoveNonExistingFiles(RemoveFromUsesSection);
end;


{ TConvertDelphiPackage }

constructor TConvertDelphiPackage.Create(const aPackageFilename: string);
begin
  inherited Create(aPackageFilename, lisConvDelphiConvertDelphiPackage);
  fLazPSuffix:='.lpk';
  fDelphiPSuffix:='.dpk';
end;

destructor TConvertDelphiPackage.Destroy;
begin
  inherited Destroy;
end;

function TConvertDelphiPackage.CreateInstance: TModalResult;
// open new package. If .lpk does not exist, create it
var
  PkgName: String;
  CurEditor: TPackageEditorForm;
begin
  LazPackage:=nil;
  if FileExistsUTF8(fLazPFilename) then begin
    // there is already a lazarus package file -> open the package editor
    Result:=PackageEditingInterface.DoOpenPackageFile(fLazPFilename,[pofAddToRecent],true);
    if Result<>mrOk then exit;
  end;
  // search package in graph
  PkgName:=ExtractFileNameOnly(fLazPFilename);
  LazPackage:=PackageGraph.FindPackageWithName(PkgName,nil);
  if LazPackage<>nil then begin
    // there is already a package loaded with this name ...
    if CompareFilenames(LazPackage.Filename,fLazPFilename)<>0 then begin
      // ... but it is not the package file we want -> stop
      MessageDlg(lisConvDelphiPackageNameExists,
        Format(lisConvDelphiThereIsAlreadyAPackageWithTheNamePleaseCloseThisPa,
               [PkgName, LineEnding]), mtError, [mbAbort], 0);
      PackageEditingInterface.DoOpenPackageFile(LazPackage.Filename,[pofAddToRecent],true);
      fErrorMsg:='Stopped because there already is a package with the same name';
      exit(mrAbort);
    end else begin
      Result:=mrOk;
    end;
  end else begin
    // there is not yet a package with this name -> create a new package with LCL as dependency
    LazPackage:=PackageGraph.CreateNewPackage(PkgName);
    PackageGraph.AddDependencyToPackage(LazPackage,
                  PackageGraph.LCLPackage.CreateDependencyWithOwner(LazPackage));
    LazPackage.Filename:=fLazPFilename;
    LazPackage.CompilerOptions.SyntaxMode:='delphi';
    // open a package editor
    CurEditor:=PackageEditors.OpenEditor(LazPackage);
    CurEditor.Show;
    // save .lpk file
    PackageEditors.SavePackage(LazPackage,false);
    Result:=mrOk;
  end;
end;

function TConvertDelphiPackage.FindAllUnits: TModalResult;
var
  FoundInUnits, MissingInUnits, NormalUnits: TStrings;
  i: Integer;
  NewSearchPath, AllPath, UselessPath: String;
  CurFilename: string;
  p: LongInt;
  OffendingUnit: TPkgFile;
  PkgFile: TPkgFile;
begin
  Result:=LoadDPKFile;
  if Result<>mrOk then exit;

  FoundInUnits:=nil;
  MissingInUnits:=nil;
  NormalUnits:=nil;
  try
    if not CodeToolBoss.FindDelphiPackageUnits(fDpkCode,FoundInUnits,
                                               MissingInUnits, NormalUnits) then
    begin
      LazarusIDE.DoJumpToCodeToolBossError;
      fErrorMsg:='Problems when trying to find all units from package file '+fOrigFilename;
      exit(mrCancel);
    end;
    try
      // Add all units to the package
      for i:=0 to FoundInUnits.Count-1 do begin
        CurFilename:=FoundInUnits[i];
        p:=System.Pos(' in ',CurFilename);
        if p>0 then
          CurFilename:=copy(CurFilename,p+4,length(CurFilename));
        if CurFilename='' then continue;
        CurFilename:=SwitchPathDelims(CurFilename, True);
        if not FilenameIsAbsolute(CurFilename) then
          CurFilename:=AppendPathDelim(LazPackage.Directory)+CurFilename;
        CurFilename:=TrimFilename(CurFilename);
        if not FileExistsUTF8(CurFilename) then
          continue;
        PkgFile:=LazPackage.FindPkgFile(CurFilename,true,false);
        if PkgFile=nil then begin
          if FilenameIsPascalUnit(CurFilename) then begin
            // Check unitname
            OffendingUnit:=LazPackage.FindUnit(ExtractFileNameOnly(CurFilename));
            if OffendingUnit<>nil then begin
              Result:=QuestionDlg(lisConvDelphiUnitnameExistsTwice,
                Format(lisConvDelphiThereAreTwoUnitsWithTheSameUnitname,
                       [LineEnding, OffendingUnit.Filename, LineEnding, CurFilename, LineEnding]),
                mtWarning, [mrNo, lisConvDelphiRemoveSecond, mrAbort], 0);
              case Result of
                mrNo:  continue;
                mrIgnore: ;
                else begin
                  fErrorMsg:='User selected to end conversion because a unitname exists twice';
                  exit(mrAbort);
                end;
              end;
            end;
          end;
          // Add new unit to package
          LazPackage.AddFile(CurFilename,ExtractFileNameOnly(CurFilename),
                           pftUnit,[pffAddToPkgUsesSection],cpNormal);
        end;
      end;
    finally
      AllPath:=LazPackage.SourceDirectories.CreateSearchPathFromAllFiles;
      UselessPath:='.;'+VirtualDirectory+';'+VirtualTempDir+';'+LazPackage.Directory;
      // Set unit paths to find all project units
      NewSearchPath:=MergeSearchPaths(LazPackage.CompilerOptions.OtherUnitFiles,AllPath);
      NewSearchPath:=RemoveSearchPaths(NewSearchPath,UselessPath);
      LazPackage.CompilerOptions.OtherUnitFiles:=MinimizeSearchPath(
               RemoveNonExistingPaths(NewSearchPath,LazPackage.Directory));
      // Set include path
      NewSearchPath:=MergeSearchPaths(LazPackage.CompilerOptions.IncludePath,AllPath);
      NewSearchPath:=RemoveSearchPaths(NewSearchPath,UselessPath);
      LazPackage.CompilerOptions.IncludePath:=MinimizeSearchPath(
               RemoveNonExistingPaths(NewSearchPath,LazPackage.Directory));
      // Clear caches
      LazPackage.DefineTemplates.SourceDirectoriesChanged;
      CodeToolBoss.DefineTree.ClearCache;
    end;
    // Save package
    Result:=PackageEditors.SavePackage(LazPackage,false);
    if Result<>mrOk then exit;
  finally
    FoundInUnits.Free;
    MissingInUnits.Free;
    NormalUnits.Free;
  end;
  Result:=mrOk;
end;

function TConvertDelphiPackage.ConvertAllUnits: TModalResult;
var
  i: Integer;
  PkgFile: TPkgFile;
  Converter: TDelphiUnit;
  ConvUnits: TObjectList;       // List of ConvertDelphiUnits.
begin
  Result:=mrOk;
  ConvUnits:=TObjectList.create;
  try
    // Convert all units and fix .lfm files
    IDEMessagesWindow.AddMsg('', '', -1);
    IDEMessagesWindow.AddMsg(lisConvDelphiConvertingProjPackUnits, '', -1);
    Application.ProcessMessages;
    for i:=0 to LazPackage.FileCount-1 do begin
      PkgFile:=LazPackage.Files[i];
      Converter:=TDelphiUnit.Create(Self, PkgFile.Filename,[]);
      ConvUnits.Add(Converter);
      Result:=Converter.CopyAndLoadFile;
      if Result<>mrOK then exit;
      //Result:=Converter.CheckFailed(Result);
      //if Result<>mrOK then Break;
      Result:=Converter.ConvertUnitFile;
      if Result<>mrOK then exit;
      Result:=Converter.CheckFailed(Result);
      if Result<>mrOK then Break;
    end;
    if Result=mrOK then
      Result:=ConvertAllFormFiles(ConvUnits);
    // Finally save the package one more time
    Result:=PackageEditors.SavePackage(LazPackage,false);
    if Result<>mrOk then exit;
  finally
    ConvUnits.Free;  // Owns and frees converter objects.
  end;
end;

function TConvertDelphiPackage.LoadDPKFile: TModalResult;
var
  DPKFilename: String;
begin
  DPKFilename:=ChangeFileExt(LazPackage.Filename,'.dpk');
  DPKFilename:=CodeToolBoss.DirectoryCachePool.FindDiskFilename(DPKFilename);
  if not FileExistsCached(DPKFilename) then begin
    Result:=MessageDlg(lisFileNotFound,
      Format(lisConvDelphiDelphiPackageMainSourceDpkFileNotFoundForPackage,
             [LineEnding, LazPackage.Filename]), mtError, [mbAbort], 0);
    exit;
  end;
  Result:=LoadCodeBuffer(fDpkCode,DPKFilename,[],true);
end;

function TConvertDelphiPackage.ExtractOptionsFromDelphiSource: TModalResult;
begin
  // TODO: use fDelphiPFilename and LazPackage to get options.
  if fDelphiPFilename<>'' then begin
  end;
  Result:=mrOk;
end;

// The following funcs are needed only because Project and LazPackage
//  don't inherit from the same class.

function TConvertDelphiPackage.GetLazPackage: TLazPackage;
begin
  Result:=fProjPack as TLazPackage;
end;

procedure TConvertDelphiPackage.SetLazPackage(const AValue: TLazPackage);
begin
  fProjPack:=AValue;
end;

function TConvertDelphiPackage.GetCompOpts: TBaseCompilerOptions;
begin
  Result:=(fProjPack as TLazPackage).CompilerOptions;
end;

function TConvertDelphiPackage.GetCustomDefines: TDefineTemplate;
begin
  Result:=(fProjPack as TLazPackage).DefineTemplates.CustomDefines;
end;

procedure TConvertDelphiPackage.CustomDefinesChanged;
begin
  (fProjPack as TLazPackage).DefineTemplates.CustomDefinesChanged;
end;

function TConvertDelphiPackage.GetMainName: string;
begin
  Result:=(fProjPack as TLazPackage).Filename;
end;

procedure TConvertDelphiPackage.AddPackageDependency(const PackageName: string);
begin
  (fProjPack as TLazPackage).AddPackageDependency(PackageName);
end;

function TConvertDelphiPackage.FindDependencyByName(const PackageName: string): TPkgDependency;
begin
  Result:=(fProjPack as TLazPackage).FindDependencyByName(PackageName);
end;

procedure TConvertDelphiPackage.RemoveNonExistingFiles(RemoveFromUsesSection: boolean);
begin
  (fProjPack as TLazPackage).RemoveNonExistingFiles;
end;


{ TConvertedDelphiProjectDescriptor }

function TConvertedDelphiProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
begin
  Result:=inherited InitProject(AProject);
  AProject.LazCompilerOptions.SyntaxMode:='delphi';
end;

end.

