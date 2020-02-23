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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner / Juha Manninen

  Abstract:
    Convert Delphi projects/packages to lazarus projects/packages.
    This was refactored and cleaned from code in unit DelphiProject2Laz.
    Now it is object oriented and easier to maintain / improve.
}
unit ConvertDelphi;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Classes, SysUtils, contnrs, IniFiles,
  // LCL
  LCLProc, Forms, Controls, Dialogs,
  // CodeTools
  CodeToolManager, DefineTemplates, CodeCache, LinkScanner, FileProcs,
  // LazUtils
  LConvEncoding, FileUtil, LazFileUtils, LazUTF8, LazUTF8Classes, LazStringUtils,
  AvgLvlTree,
  // IDEIntf
  ComponentReg, IDEDialogs, LazIDEIntf, PackageIntf, ProjectIntf,
  IDEExternToolIntf, IDEOptEditorIntf,
  // IDE
  IDEProcs, DialogProcs, CompilerOptions, ProjPackCommon, Project, ProjectDescriptors,
  PackageDefs, PackageSystem, PackageEditor, BasePkgManager, LazarusIDEStrConsts,
  // Converter
  ConverterTypes, ConvertSettings, ConvCodeTool, MissingUnits, MissingPropertiesDlg,
  UsedUnits;

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
    // Converter for the unit, project or package this unit belongs to.
    // There is always an owner converter.
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
    function ReduceMissingUnits: TModalResult;
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

  // Base class for all converters. Takes care of error handling etc.
  // TConvertDelphiUnit ja TConvertDelphiProjPack inherit from this,
  //  wrapping either one unit or whole project / package conversion.
  TConvertDelphiPBase = class
  private
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
    function DoMissingUnits({%H-}AUsedUnitsTool: TUsedUnitsTool): integer; virtual;
    function GetCachedUnitPath(const AUnitName: string): string;
  protected
    function EndConvert(AStatus: TModalResult): Boolean;
  public
    constructor Create(const ADescription: string);
    constructor Create(const AFilename, ADescription: string);
    destructor Destroy; override;
  public
    property ErrorMsg: string read fErrorMsg;
  end;

  { TConvertDelphiUnit }

  // Delphi unit conversion.
  TConvertDelphiUnit = class(TConvertDelphiPBase)
  private
    fDirTemplate: TDefineTemplate;
  public
    constructor Create(aFileNames: TStrings);
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
    fProjPack: iProjPack;
    fLazPMainFilename: string;         // .lpi or .lpk file name
    fDelphiPFilename: string;          // .dpr or .dpk file name
    fLazPMainSuffix: string;           // '.lpi' or '.lpk' suffix
    fLazPSuffix: string;   // '.lpr' or empty. '.lpk' is for the XML main package
    fDelphiPSuffix: string;            // '.dpr' or '.dpk' suffix
    // Main unit with resource code
    fMainUnitConverter: TDelphiUnit;
    // Unit search path for project settings.
    fUnitSearchPaths: TStringList;
    // Units that are found and will be added to project or package and converted.
    fUnitsToAddToProject: TStringList;
    fFilesToDelete: TStringList;
    fUseThreads: boolean;              // The project/package uses TThread.
    function ConvertSub: TModalResult;
    procedure CleanUpCompilerOptionsSearchPaths(Options: TBaseCompilerOptions);
    procedure SetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
    procedure UnsetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
    function ConvertAllFormFiles(ConverterList: TObjectList): TModalResult;
    function ReadDelphiConfigFiles: TModalResult;
    function ExtractOptionsFromDOF(const DOFFilename: string): TModalResult;
    function ExtractOptionsFromCFG(const CFGFilename: string): TModalResult;
    procedure MissingUnitsSub(AUsedUnits: TUsedUnits);
    function DoMissingUnits(AUsedUnitsTool: TUsedUnitsTool): integer; override;
    function AddToProjectLater(AFileName: string): Boolean;
    function MaybeDeleteFiles: TModalResult;
    function CheckUnitForConversion(aFileName: string): Boolean;
    function CheckPackageDep(AUnitName: string): Boolean;
    function TryAddPackageDep(AUnitName, ADefaultPkgName: string): Boolean;
  protected
    function CreateInstance: TModalResult; virtual; abstract;
    function CreateMainSourceFile: TModalResult; virtual; abstract;
    function FindAllUnits: TModalResult; virtual; abstract;
    function ConvertAllUnits: TModalResult; virtual; abstract;
    function ExtractOptionsFromDelphiSource: TModalResult; virtual; abstract;
    // Abstract base for the fake Project / Package virtual methods.
    function GetMainName: string; virtual; abstract;
    function SaveAndMaybeClose({%H-}aFilename: string): TModalResult; virtual;
    function ContainsFile(aFileName: string): Boolean; virtual; abstract;
    function FindDependencyByName(const PackageName: string): TPkgDependency; virtual; abstract;
  public
    constructor Create(const AFilename, ADescription: string);
    destructor Destroy; override;
    function Convert: TModalResult;
  public
    property MainName: string read GetMainName;
  end;


  { TConvertDelphiProject }

  // Delphi project conversion.
  TConvertDelphiProject = class(TConvertDelphiProjPack)
  private
    function AddUnit(AFileName: string; out OutUnitInfo: TUnitInfo): TModalResult;
    function GetLazProject: TProject;
    procedure SetLazProject(const AValue: TProject);
  protected
    function CreateInstance: TModalResult; override;
    function CreateMainSourceFile: TModalResult; override;
    function FindAllUnits: TModalResult; override;
    function ConvertAllUnits: TModalResult; override;
    function ExtractOptionsFromDelphiSource: TModalResult; override;
    // Fake Project virtual methods.
    function GetMainName: string; override;
    function SaveAndMaybeClose(Filename: string): TModalResult; override;
    function ContainsFile(aFileName: string): Boolean; override;
    function FindDependencyByName(const PackageName: string): TPkgDependency; override;
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
    function AddUnit(AFileName: string): TModalResult;
    function GetLazPackage: TLazPackage;
    procedure SetLazPackage(const AValue: TLazPackage);
  protected
    function CreateInstance: TModalResult; override;
    function CreateMainSourceFile: TModalResult; override;
    function FindAllUnits: TModalResult; override;
    function ConvertAllUnits: TModalResult; override;
    function ExtractOptionsFromDelphiSource: TModalResult; override;
    // Fake Package virtual methods.
    function GetMainName: string; override;
    function ContainsFile(aFileName: string): Boolean; override;
    function FindDependencyByName(const PackageName: string): TPkgDependency; override;
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
  public
    procedure InitPackage(APackage: TLazPackage); override;    // ToDo
  end;
}

implementation

function ConvertDelphiAbsoluteToRelativeFile(const Filename: string;
                                      AProjPack: TConvertDelphiProjPack): string;
// often projects use paths near to their project directory. For example:
//   A project /somewhere/MyProjects/project1.dpr
// and a path C:\Delphi\MyProj\folder can mean, that the relative path is 'folder'
var
  ProjectDir: String;
  ShortProjectDir: String;
  p: LongInt;
begin
  Result:='';       // Default: ignore absolute paths
  ProjectDir:=ExtractFilePath(AProjPack.fLazPMainFilename);
  ShortProjectDir:=PathDelim+ExtractFileName(ChompPathDelim(ProjectDir))+PathDelim;
  p:=System.Pos(ShortProjectDir,Filename);
  if (p>0) then
    Result:=copy(Filename,p+length(ShortProjectDir),length(Filename));
end;

function ExpandDelphiFilename(const Filename: string; AProjPack: TConvertDelphiProjPack): string;
var
  p: LongInt;
begin
  Result:=Filename;
  if Result='' then exit;
  Result:=TrimFilename(GetForcedPathDelims(Result));
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
    Result:=ConvertDelphiAbsoluteToRelativeFile(Result, AProjPack);
  end;
  // change PathDelim
  Result:=TrimFilename(GetForcedPathDelims(Result));
end;

function ExpandDelphiSearchPath(const SearchPath: string;
                                AProjPack: TConvertDelphiProjPack): string;
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
      Paths[i]:=ExpandDelphiFilename(Paths[i], AProjPack);
    // remove doubles
    for i:=Paths.Count-1 downto 0 do begin
      CurPath:=Paths[i];
      if (CurPath='') then
        Paths.Delete(i)
      else begin
        j:=i-1;
        while (j>=0) and (CompareText(CurPath, Paths[i])<>0) do
          dec(j);
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
  RelPath:=CreateRelativePath(FileName, fConverter.fSettings.MainPath);
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
  FreeOnTerminate:=False; // Will be set to True before starting;
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
  const AFilename: string; aFlags: TConvertUnitFlags);
begin
  inherited Create;
  fOwnerConverter:=AOwnerConverter;
  fOrigUnitFilename:=AFilename;
  fFlags:=AFlags;
  fLazFileExt:='';
  fUnitInfo:=nil;
  if not LazarusIDE.BeginCodeTools then
    fOwnerConverter.fSettings.AddLogLine(mluFatal,
      lisConvDelphiBeginCodeToolsFailed, fLazUnitFilename);
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
var
  CodeOk, CodeFixed: Boolean;
begin
  // Convert unit in place. File must be writable.
  Result:=CheckFileIsWritable(fOrigUnitFilename,[mbAbort]);
  if Result<>mrOK then exit;
  // close Delphi unit file in editor.
  Result:=LazarusIDE.DoCloseEditorFile(fOrigUnitFilename,[cfSaveFirst]);
  if Result<>mrOK then exit;
  // Copy/rename fLazUnitFilename based on fOrigUnitFilename.
  Assert(Assigned(fOwnerConverter), 'TDelphiUnit.CopyAndLoadFile: fOwnerConverter not assigned.');
  Result:=fOwnerConverter.fSettings.RenameDelphiToLazFile(fOrigUnitFilename,
                  fLazFileExt, fLazUnitFilename, cdtlufRenameLowercase in fFlags);
  if Result<>mrOK then exit;
  fPascalBuffer:=nil;                         // Read the code in.
  Result:=LoadCodeBuffer(fPascalBuffer,fLazUnitFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk],true);
  if Result<>mrOK then exit;
  // Change encoding to UTF-8
  if fPascalBuffer.DiskEncoding<>EncodingUTF8 then begin
    fOwnerConverter.fSettings.AddLogLine(mluNote,
      Format(lisConvDelphiChangedEncodingToUTF8, [fPascalBuffer.DiskEncoding]),
      fLazUnitFilename);
    fPascalBuffer.DiskEncoding:=EncodingUTF8; // Takes effect when buffer is saved.
  end;
  // Create a shared link for codetools.
  Assert(fCTLink=Nil, 'fCTLink should be Nil in CopyAndLoadFile');
  fCTLink:=TCodeToolLink.Create(fPascalBuffer);
  fCTLink.Settings:=fOwnerConverter.fSettings;
  fCTLink.AskAboutError:=fOwnerConverter is TConvertDelphiProjPack;
  // Fix include file names.
  Result:=FixIncludeFiles;
  if Result<>mrOK then exit;
  CodeFixed:=False;
  repeat
    CodeOk:=True;
    try
      // Create a tool for missing units.
      fUsedUnitsTool:=TUsedUnitsTool.Create(fCTLink, fOrigUnitFilename);
    except
      // If CodeTool's BuildTree raised exception, try dummy replacement for
      //  some known problematic syntax, currently only OleVariant members.
      if CodeFixed then
        Raise               // There was a second exception -> we are doomed!
      else begin
        if not fCTLink.DummyReplacements then
          Raise;
        CodeOk := False;
        CodeFixed := True;  // Try replacements only once
      end;
    end;
  until CodeOk;
  if fOwnerConverter is TConvertDelphiProjPack then
    with TConvertDelphiProjPack(fOwnerConverter) do begin
      fUsedUnitsTool.OnCheckPackageDependency:=@CheckPackageDep;
      fUsedUnitsTool.IsConsoleApp:=fIsConsoleApp;
      fUsedUnitsTool.OnCheckUnitForConversion:=@CheckUnitForConversion;
    end;
end;

function TDelphiUnit.FixLfmFilenameAndLoad(ADfmFilename: string): TModalResult;
var
  LfmFilename: string;     // Lazarus .LFM file name.
  DFMConverter: TDFMConverter;
  TempLFMBuffer: TCodeBuffer;
begin
  Result:=mrOK;
  fLFMBuffer:=nil;
  if ADfmFilename<>'' then
  begin
    Result:=LazarusIDE.DoCloseEditorFile(ADfmFilename,[cfSaveFirst]);
    if Result<>mrOK then exit;
    // Save before using .dfm.
    Result:=fOwnerConverter.fSettings.MaybeBackupFile(ADfmFilename);
    if Result<>mrOK then exit;
  end;
  if fOwnerConverter.fSettings.SameDfmFile then
    LfmFilename:=ADfmFilename
  else begin
    // Create a form file name based on the unit file name.
    with fOwnerConverter.fSettings do
      LfmFilename:=DelphiToLazFilename(fOrigUnitFilename, '.lfm',
                                       cdtlufRenameLowercase in fFlags);
    if ADfmFilename<>'' then
    begin
      if FileExistsUTF8(LfmFilename) then
        if (FileAgeUTF8(LfmFilename)<FileAgeUTF8(ADfmFilename)) then
          DeleteFileUTF8(LfmFilename); // .lfm is older than .dfm -> remove .lfm
      if not FileExistsUTF8(LfmFilename) then
      begin
        // TODO: update project
        if fOwnerConverter.fSettings.SupportDelphi then
          Result:=CopyFileWithErrorDialogs(ADfmFilename, LfmFilename, [mbAbort])
        else
          Result:=RenameFileWithErrorDialogs(ADfmFilename, LfmFilename, [mbAbort]);
        if Result<>mrOK then exit;
      end;
    end;
  end;
  // Convert Unit .dfm file to .lfm file (without context type checking)
  if FileExistsUTF8(LfmFilename) then
  begin
    DFMConverter:=TDFMConverter.Create;
    try
      DFMConverter.Settings:=fOwnerConverter.fSettings;
      Result:=DFMConverter.ConvertDfmToLfm(LfmFilename);
      if Result<>mrOK then exit;
    finally
      DFMConverter.Free;
    end;
    // Read pascal unit code in.
    Result:=LoadCodeBuffer(TempLFMBuffer,LfmFilename,
                           [lbfCheckIfText,lbfUpdateFromDisk],true);
    // Change encoding to UTF-8
    if TempLFMBuffer.DiskEncoding<>EncodingUTF8 then
    begin
      fOwnerConverter.fSettings.AddLogLine(mluNote,
        Format(lisConvDelphiChangedEncodingToUTF8, [TempLFMBuffer.DiskEncoding]),
        fLazUnitFilename);
      TempLFMBuffer.DiskEncoding:=EncodingUTF8;
      TempLFMBuffer.Save;
    end;
    // Read form file code in.
    if not fOwnerConverter.fSettings.SameDfmFile then
      Result:=LoadCodeBuffer(fLFMBuffer,LfmFilename,
                             [lbfCheckIfText,lbfUpdateFromDisk],true);
  end;
end;

function TDelphiUnit.ReduceMissingUnits: TModalResult;
// Find or comment out some / all of missing units.
begin
  Result:=mrOK;
  // Comment out automatically units that were commented in other files.
  fUsedUnitsTool.MainUsedUnits.CommentAutomatic(fOwnerConverter.fAllCommentedUnits);
  fUsedUnitsTool.ImplUsedUnits.CommentAutomatic(fOwnerConverter.fAllCommentedUnits);
  // Remove omitted units from MissingUnits.
  fUsedUnitsTool.MainUsedUnits.OmitUnits;
  fUsedUnitsTool.ImplUsedUnits.OmitUnits;
  // Try to find from subdirectories scanned earlier.
  if fOwnerConverter.DoMissingUnits(fUsedUnitsTool)=0 then exit;
  if fUsedUnitsTool.MissingUnitCount=0 then exit;
  // Interactive dialog for searching unit.
  Result:=AskUnitPathFromUser;
end;

function TDelphiUnit.ConvertUnitFile: TModalResult;
var
  DfmFilename: string;     // Delphi .DFM file name.
  ConvTool: TConvDelphiCodeTool;
begin
  // Get DFM file name and close it in editor.
  DfmFilename:=GetDfmFileName;
  Result:=FixLfmFilenameAndLoad(DfmFilename);
  if Result<>mrOK then exit;
  if fOwnerConverter.fSettings.UnitsReplaceMode<>rlDisabled then begin
    // Find and prepare the missing units. Don't replace yet.
    Result:=fUsedUnitsTool.Prepare;
    if Result<>mrOK then exit;
    if fUsedUnitsTool.MissingUnitCount>0 then begin
      Result:=ReduceMissingUnits;
      if Result<>mrOK then exit;
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
    if Result<>mrOK then exit;
  finally
    ConvTool.Free;
  end;
  // First pass to add, remove, fix and comment out units in uses sections.
  Result:=fUsedUnitsTool.ConvertUsed;
end;

function TDelphiUnit.ConvertFormFile: TModalResult;
var
  LfmFixer: TLFMFixer;
begin
  // Fix the LFM file and the pascal unit, updates fPascalBuffer and fLFMBuffer.
  if fLFMBuffer<>nil then begin
    LfmFixer:=TLFMFixer.Create(fCTLink,fLFMBuffer);
    try
      LfmFixer.Settings:=fOwnerConverter.fSettings;
      LfmFixer.UsedUnitsTool:=fUsedUnitsTool;
      LfmFixer.RootMustBeClassInUnit:=true;
      LfmFixer.RootMustBeClassInIntf:=true;
      LfmFixer.ObjectsMustExist:=true;
      if LfmFixer.ConvertAndRepair<>mrOK then begin
        LazarusIDE.DoJumpToCompilerMessage(true);
        fOwnerConverter.fErrorMsg:=Format(lisConvProblemsRepairingFormFile,
                                          [ChangeFileExt(fOrigUnitFilename, '.lfm')]);
        exit(mrAbort);
      end;
    finally
      LfmFixer.Free;
    end;
    // save LFM file
    Result:=SaveCodeBufferToFile(fLFMBuffer,fLFMBuffer.Filename);
    if Result<>mrOK then exit;
  end;
  // Second pass to add, remove, fix and comment out units in uses sections.
  // More changes to uses section can happen during form file conversion.
  Result:=fUsedUnitsTool.ConvertUsed;
  if Result<>mrOK then exit;
  Result:=mrOK;
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
      // mrOK means: Comment out.
      mrOK:
        MoveMissingToComment(fOwnerConverter.fAllCommentedUnits);
      // mrYes means: Search for unit path.
      mrYes: begin
        UnitDirDialog:=TSelectDirectoryDialog.Create(nil);
        try
          UnitDirDialog.InitialDir:=fOwnerConverter.fPrevSelectedPath;
          UnitDirDialog.Title:=lisConvDelphiAllSubDirsScanned;
          if UnitDirDialog.Execute then
          begin
            fOwnerConverter.fPrevSelectedPath:=UnitDirDialog.FileName;
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
      // mrIgnore means: Skip this unit. The missing units list is already cleared.
      mrIgnore:
        Result:=mrOK;
      // Abort the whole conversion.
      mrAbort:
        fOwnerConverter.fErrorMsg:=Format(lisConvUserSelectedToEndConversion,
                                          [fOrigUnitFilename]);
    end;
  until not TryAgain;
end;

function TDelphiUnit.FixIncludeFiles: TModalResult;
// fix include filenames
var
  FoundIncludeFiles: TStrings;
  MissingIncludeFilesCodeXYPos: TFPList;
  CodePos: PCodeXYPosition;
  Msg, s: string;
  i: Integer;
  OldChange: Boolean;
begin
  Result:=mrOK;
  FoundIncludeFiles:=Nil;
  MissingIncludeFilesCodeXYPos:=Nil;
  OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
  LazarusIDE.OpenEditorsOnCodeToolChange:=False;
  with fCTLink, fOwnerConverter do
  try
    if CodeTool.DirectoryCache=nil then exit;
    if CodeTool.FixIncludeFilenames(Code,SrcCache,FoundIncludeFiles,MissingIncludeFilesCodeXYPos)
    then begin
      if Assigned(FoundIncludeFiles) then begin
        // List the include files in log.
        Msg:=lisConvRepairingIncludeFiles;
        for i:=0 to FoundIncludeFiles.Count-1 do begin
          fSettings.MaybeBackupFile(FoundIncludeFiles[i]);
          s:=CreateRelativePath(FoundIncludeFiles[i], fSettings.MainPath);
          if i>0 then
            Msg:=Msg+'; ';
          Msg:=Msg+s;
        end;
        fSettings.AddLogLine(mluNote, Msg, fLazUnitFilename);
      end;
    end
    else begin
      if MissingIncludeFilesCodeXYPos<>nil then begin
        for i:=0 to MissingIncludeFilesCodeXYPos.Count-1 do begin
          CodePos:=PCodeXYPosition(MissingIncludeFilesCodeXYPos[i]);
          Msg:=Format(lisConvDelphiMissingIncludeFile,
                 [CodePos^.Code.Filename,IntToStr(CodePos^.y),IntToStr(CodePos^.x)]);
          fSettings.AddLogLine(mluError, Msg, fLazUnitFilename);
        end;
      end;
      fErrorMsg:=Format(lisConvProblemsFixingIncludeFile, [fOrigUnitFilename]);
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
        Format(lisConvDelphiFailedToConvertUnit, [fOrigUnitFilename]),
        mtWarning, [mrIgnore, lisIgnoreAndContinue, mrAbort], 0);
    case Result  of
      mrIgnore : Result:=mrOK;
      mrAbort : fOwnerConverter.fErrorMsg:=Format(lisConvUserSelectedToEndConversion,
                                                  [fOrigUnitFilename]);
    end;
  end;
end;


{ TConvertDelphiPBase }

constructor TConvertDelphiPBase.Create(const ADescription: string);
begin
  inherited Create;
  fSettings:=TConvertSettings.Create(ADescription);
  fIsConsoleApp:=False;                   // Default = GUI app.
end;

constructor TConvertDelphiPBase.Create(const AFilename, ADescription: string);
begin
  Create(ADescription);
  fSettings.MainFilenames.Add(AFilename);
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

function TConvertDelphiPBase.EndConvert(AStatus: TModalResult): Boolean;
begin
  // Show ending message
  if AStatus=mrOK then
    fSettings.AddLogLine(mluImportant, lisConvDelphiConversionReady)
  else begin
    if fErrorMsg<>'' then
      fSettings.AddLogLine(mluError, Format(lisConvDelphiError,[fErrorMsg]))
    else if CodeToolBoss.ErrorMessage<>'' then
      fSettings.AddLogLine(mluError, Format(lisConvDelphiError,[CodeToolBoss.ErrorMessage]));
    fSettings.AddLogLine(mluFatal, lisConvDelphiConversionAborted);
  end;
  // Save log messages to file.
  Result:=fSettings.SaveLog;
end;

{ TConvertDelphiUnit }

constructor TConvertDelphiUnit.Create(aFileNames: TStrings);
var
  MacroName, s: String;
  i: Integer;
begin
  inherited Create(lisConvDelphiConvertDelphiUnit);
  // Add the list of files if they exist.
  for i:=0 to aFileNames.Count-1 do begin
    s:=CleanAndExpandFilename(aFileNames[i]);
    if FileExistsUTF8(s) then
      fSettings.MainFilenames.Add(s);
  end;
  fPrevSelectedPath:=fSettings.MainPath;
  // use a template for compiler mode delphi for a single directory
  fDirTemplate:=TDefineTemplate.Create(SettingDelphiModeTemplName,
    'Mode Delphi for single unit conversion', '', fSettings.MainPath, da_Directory);
  MacroName:=CompilerModeVars[cmDELPHI];
  s:='Define'+MacroName;
  fDirTemplate.AddChild(TDefineTemplate.Create(s, s, MacroName, '1', da_Define));
  CodeToolBoss.DefineTree.Add(fDirTemplate); // add directory template to tree
end;

destructor TConvertDelphiUnit.Destroy;
begin
  CodeToolBoss.DefineTree.RemoveDefineTemplate(fDirTemplate);
  inherited Destroy;
end;

function TConvertDelphiUnit.Convert: TModalResult;
var
  DelphiUnit: TDelphiUnit;
  i: Integer;
begin
  Result:=fSettings.RunForm(Nil);
  if Result=mrOK then begin
    try
    try
      fSettings.ClearLog;
      for i:=0 to fSettings.MainFilenames.Count-1 do begin
        Application.ProcessMessages;
        if i>0 then
          fSettings.AddLogLine(mluImportant, '');
        DelphiUnit:=TDelphiUnit.Create(Self, fSettings.MainFilenames[i], []);
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
        finally
          DelphiUnit.Free;
        end;
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
      EndConvert(Result);
    end;
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
  fFilesToDelete:=TStringList.Create;
  fFilesToDelete.Sorted:=True;
  fMainUnitConverter:=nil;
end;

destructor TConvertDelphiProjPack.Destroy;
begin
  fMainUnitConverter.Free;
  fFilesToDelete.Free;
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
  if CompareFileExt(fSettings.MainFilename,'.dproj',false)=0 then begin
    fErrorMsg := lisConvDprojFileNotSupportedYet;
    Exit(mrCancel);
  end;
  // Start scanning unit files one level above project path. The GUI will appear
  // without delay but then we must wait for the thread before continuing.
  CacheUnitsThread:=TCacheUnitsThread.Create(Self,
                        ResolveDots(fSettings.MainPath+'..'+DirectorySeparator));
  try
  try
    Result:=fSettings.RunForm(CacheUnitsThread); // Get settings from user.
    if Result=mrOK then begin
      StartTime:=Now;
      fSettings.ClearLog;
      // create/open lazarus project or package file
      fLazPMainFilename:=fSettings.DelphiToLazFilename(fSettings.MainFilename,
                                                       fLazPMainSuffix, false);
      // Find Delphi project / package file name
      if CompareFileExt(fSettings.MainFilename,fDelphiPSuffix,false)=0 then
        fDelphiPFilename:=fSettings.MainFilename
      else
        fDelphiPFilename:=ChangeFileExt(fSettings.MainFilename,fDelphiPSuffix);
      if not FileExistsUTF8(fDelphiPFilename) then
        fDelphiPFilename:=FindDiskFileCaseInsensitive(fSettings.MainFilename);
// ? fDelphiPFilename:=CodeToolBoss.DirectoryCachePool.FindDiskFilename(fSettings.MainFilename);

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
    CacheUnitsThread.Free;
    EndTime:=Now;
    s:=FormatDateTime('hh:nn:ss', EndTime-StartTime);
    if (Result<>mrAbort) and (s<>'00:00:00') then
      fSettings.AddLogLine(mluProgress, Format(lisConvDelphiConversionTook,[s]));
    EndConvert(Result);
  end;
end;

function TConvertDelphiProjPack.ConvertSub: TModalResult;
begin
  // Project / package instance.
  Result:=CreateInstance;
  if Result<>mrOK then exit;
  // Create main source file (.lpr/.lpk) (only copy, no conversion)
  fMainUnitConverter:=TDelphiUnit.Create(Self, fSettings.MainFilename,[]);
  if fSettings.SupportDelphi then
    fMainUnitConverter.LazFileExt:=ExtractFileExt(fSettings.MainFilename)
  else
    fMainUnitConverter.LazFileExt:=fLazPSuffix; // '.lpr' or ''
  Result:=fMainUnitConverter.CopyAndLoadFile;
  if Result<>mrOK then exit;
  fMainUnitConverter.fUsedUnitsTool.IsMainFile:=True;
  Result:=CreateMainSourceFile; // More actions for the main source file.
  if Result<>mrOK then exit;
  Application.ProcessMessages;
  // read config files (they often contain clues about paths, switches and defines)
  Result:=ReadDelphiConfigFiles;
  if Result<>mrOK then exit;
  fProjPack.RemoveNonExistingFiles(false);
  CleanUpCompilerOptionsSearchPaths(fProjPack.BaseCompilerOptions);
  // LCL dependency should be added automatically later for GUI applications
  fProjPack.AddPackageDependency('LCL'); // but in some special cases it is not.
  // ToDo: make an option to add NoGUI to Project.CompilerOptions.LCLWidgetType.
  if fProjPack is TProject then
    PkgBoss.OpenProjectDependencies(fProjPack as TProject, true);
  fProjPack.DefineTemplates.CustomDefinesChanged;
  SetCompilerModeForDefineTempl(fProjPack.DefineTemplates.CustomDefines);
  try
    if Result<>mrOK then exit;
    // get all options from the .dpr or .dpk file
    Result:=ExtractOptionsFromDelphiSource;
    if Result<>mrOK then exit;
    Result:=FindAllUnits;              // find all files and save the project.
    if Result<>mrOK then exit;
    Application.ProcessMessages;
    // Convert .lpr/.lpk file. Main source file was loaded earlier. Now just convert.
    Result:=fMainUnitConverter.ConvertUnitFile;
    if Result<>mrOK then exit;
    Application.ProcessMessages;
    Result:=fMainUnitConverter.ConvertFormFile;
    if Result<>mrOK then exit;
    Application.ProcessMessages;
    Result:=ConvertAllUnits;           // convert all files.
    if Result<>mrOK then exit;
  finally
    UnsetCompilerModeForDefineTempl(fProjPack.DefineTemplates.CustomDefines);
  end;
  Result:=MaybeDeleteFiles;     // Delete files having same name with a LCL unit.
end;

function TConvertDelphiProjPack.ConvertAllFormFiles(ConverterList: TObjectList): TModalResult;
// Call Convert.ConvertFormFile for all units.
var
  Converter: TDelphiUnit;
  i: Integer;
begin
  if not fSettings.SameDfmFile then begin
    fSettings.AddLogLine(mluImportant, '');
    fSettings.AddLogLine(mluImportant, lisConvDelphiRepairingFormFiles);
    DebugLn('');
    DebugLn('TConvertDelphiProjPack.ConvertAllFormFiles: '+lisConvDelphiRepairingFormFiles);
  end;
  Screen.BeginWaitCursor;
  try
    for i:=0 to ConverterList.Count-1 do begin
      Converter:=TDelphiUnit(ConverterList[i]); // Converter created in cycle1.
      Application.ProcessMessages;
      Result:=Converter.ConvertFormFile;
      Result:=Converter.CheckFailed(Result);
      if Result<>mrOK then exit;
      // Finally save and maybe close the file.
      Result:=SaveAndMaybeClose(Converter.fLazUnitFilename);
      if Result<>mrOK then exit;
    end;
  finally
    Screen.EndWaitCursor;
  end;
  Result:=mrOK;
end;

function TConvertDelphiProjPack.ReadDelphiConfigFiles: TModalResult;
var
  FN, s: String;
begin
  Result:=mrOK;
  FN:=MainName;
  if FN<>'' then begin
    // read .dof file
    s:=FindDiskFileCaseInsensitive(ChangeFileExt(FN,'.dof'));
    if s<>'' then begin
      Result:=ExtractOptionsFromDOF(s);
      if Result<>mrOK then exit;
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
    Result:=ExpandDelphiFilename(Result, Self);
  end;

  function ReadSearchPath(const Section, Ident: string): string;
  var
    SearchPath: String;
  begin
    SearchPath:=IniFile.ReadString(Section,Ident,'');
    Result:=ExpandDelphiSearchPath(SearchPath, Self);
  end;

  procedure AddPackDep(const DelphiPkgName, LowerDelphiPkgNames, LazarusPkgName: string);
  begin
    if DelphiPkgName='' then exit;
    if Pos(';'+lowercase(DelphiPkgName)+';', ';'+LowerDelphiPkgNames+';')>0 then
    begin
      fProjPack.AddPackageDependency(LazarusPkgName);
      fSettings.AddLogLine(mluNote,
        Format(lisConvDelphiAddedPackageDependency,[LazarusPkgName]),
        fLazPMainFilename);
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
    fProjPack.BaseCompilerOptions.MergeToIncludePaths(SearchPath);
    fProjPack.BaseCompilerOptions.MergeToLibraryPaths(SearchPath);
    fProjPack.BaseCompilerOptions.MergeToUnitPaths(SearchPath);
    fProjPack.BaseCompilerOptions.MergeToObjectPath(SearchPath);
    fProjPack.BaseCompilerOptions.MergeToDebugPath(SearchPath);
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
          fProjPack.BaseCompilerOptions.UnitOutputDirectory:=OutputDir;
      end;

      // search path
      SearchPath:=ReadSearchPath('Directories','SearchPath');
      if (SearchPath<>'') then
        AddSearchPath(SearchPath);

      // debug source dirs
      DebugSourceDirs:=ReadSearchPath('Directories','DebugSourceDirs');
      if DebugSourceDirs<>'' then
        fProjPack.BaseCompilerOptions.MergeToDebugPath(DebugSourceDirs);

      // packages
      ReadDelphiPackages;

      if fProjPack is TProject then begin
        if IniFile.ReadString('Linker','ConsoleApp','')='0' then
          // does not need a windows console
          fProjPack.BaseCompilerOptions.Win32GraphicApp:=true;
      end;
    finally
      IniFile.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ExtractOptionsFromDOF failed reading "'+DOFFilename+'" '+E.Message);
    end;
  end;
  Result:=mrOK;
end;

function TConvertDelphiProjPack.ExtractOptionsFromCFG(const CFGFilename: string): TModalResult;
var
  sl: TStringListUTF8;
  i: Integer;
  Line, s: string;
  c: char;
begin
  try
    sl:=TStringListUTF8.Create;
    try
      sl.LoadFromFile(CFGFilename);
      for i:=0 to sl.Count-1 do begin
        Line:=sl[i];
        if Line='' then continue;
        if (Line[1]<>'-') or (length(Line)<2) then continue;
        c:=Line[2];
        if (c='U') or (c='I') then begin
          s:=ExpandDelphiSearchPath(copy(Line,4,length(Line)-4), Self);
          if s<>'' then
            case c of
              'U': fProjPack.BaseCompilerOptions.MergeToUnitPaths(s);
              'I': fProjPack.BaseCompilerOptions.MergeToIncludePaths(s);
            end;
        end
      end;
    finally
      sl.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('ExtractOptionsFromCFG failed reading "'+CFGFilename+'" '+E.Message);
    end;
  end;
  Result:=mrOK;
end;

procedure TConvertDelphiProjPack.SetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
begin
  if DefTempl.FindChildByName(SettingDelphiModeTemplName)<>nil then exit;
  DefTempl.ReplaceChild(CreateDefinesForFPCMode(SettingDelphiModeTemplName, cmDELPHI));
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
  if Options.UnitOutputDirectory='' then
    Options.UnitOutputDirectory:='lib'+PathDelim+'$(TargetCPU)-$(TargetOS)';
  Options.TargetFilename:=''; // This may have a wrong value from default compiler options.

  if fSettings.DelphiDefine then begin
    // "Borland" and "Ver150" are defined by Delphi7.
    // "Delphi7" and "Compiler6_Up" are defined by Jedi library based on other settings.
    //  They are needed because Jedi.inc undefines "Borland" when "FPC" is defined. Nuts.
    // PUREPASCAL is defined by some code to not use x86 assembly code.
    s:='-dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL';
    Options.CustomOptions:=s;
    fSettings.AddLogLine(mluNote, Format(lisConvDelphiAddedCustomOptionDefines,[s]),
      fLazPMainFilename);
  end;
end;

procedure TConvertDelphiProjPack.MissingUnitsSub(AUsedUnits: TUsedUnits);
var
  mUnit, sUnitPath, RealFileName, RealUnitName: string;
  i: Integer;
begin
  for i:= AUsedUnits.MissingUnits.Count-1 downto 0 do begin
    mUnit:=AUsedUnits.MissingUnits[i];
    sUnitPath:=GetCachedUnitPath(mUnit);
    if sUnitPath<>'' then begin
      fProjPack.BaseCompilerOptions.MergeToUnitPaths(sUnitPath);
      fProjPack.BaseCompilerOptions.MergeToIncludePaths(sUnitPath);
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

function TConvertDelphiProjPack.DoMissingUnits(AUsedUnitsTool: TUsedUnitsTool): integer;
// Locate unit names from earlier cached list or from packages.
// Return the number of units still missing.
begin
  MissingUnitsSub(AUsedUnitsTool.MainUsedUnits);
  MissingUnitsSub(AUsedUnitsTool.ImplUsedUnits);
  Result:=AUsedUnitsTool.MissingUnitCount;
end;

function TConvertDelphiProjPack.AddToProjectLater(AFileName: string): Boolean;
var
  x: Integer;
begin
  Result:=not fUnitsToAddToProject.Find(AFileName,x);
  if Result then           // Add the file later to project if not already done.
    fUnitsToAddToProject.Add(AFileName);
end;

function TConvertDelphiProjPack.MaybeDeleteFiles: TModalResult;
// Maybe delete files that are already in LCL. They are potentially copied from VCL.
var
  s: String;
  i: Integer;
begin
  for i := fFilesToDelete.Count-1 downto 0 do begin
    s:=fFilesToDelete[i];
    // Ask confirmation from user.
    if IDEMessageDialog(lisConvDelphiUnitnameExistsInLCL,
                  Format(lisConvDelphiUnitWithNameExistsInLCL,[ExtractFileNameOnly(s),s]),
                  mtConfirmation, mbYesNo) = mrYes
    then begin
      // Delete from file system because compiler would find it otherwise.
      if not DeleteFileUTF8(s) then
        exit(mrCancel);
      //fFilesToDelete.Delete(i);
      fSettings.AddLogLine(mluNote, Format(lisConvDeletedFile,[s]));
    end;
  end;
  Result:=mrOK;
end;

function TConvertDelphiProjPack.CheckUnitForConversion(aFileName: string): Boolean;
// Units in project directory but not part of the project are not reported
//  as missing and would not be converted. Now add them to the project/package.
var
  UnitN: String;
  x: Integer;
begin
  if ExtractFilePath(aFileName)=fSettings.MainPath then begin
    Result:=not ContainsFile(aFileName);     // Process only files in projct dir.
    if Result then begin                     // Not in project.
      UnitN:=ExtractFileNameOnly(AFileName);
      if Assigned(PackageGraph.LCLBasePackage.FindUnit(UnitN))
      or Assigned(PackageGraph.LazUtilsPackage.FindUnit(UnitN)) then begin
        if not fFilesToDelete.Find(aFileName, x) then
          fFilesToDelete.Add(AFileName); // Found also in the package, delete later.
      end
      else
        AddToProjectLater(aFileName);    // Add to project later.
    end;
  end
  else
    Result:=False;
end;

function TConvertDelphiProjPack.CheckPackageDep(AUnitName: string): Boolean;
// Check if the given unit can be found in existing packages. Add a dependency if found.
// This is called only if the unit is reported as missing.
// Returns True if a dependency was really added.
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
      fProjPack.AddPackageDependency(s);
      fSettings.AddLogLine(mluNote, Format(lisConvDelphiAddedPackageDependency,[s]),
                           fLazPMainFilename);
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
      // Add dependency based on unit name (default is ignored)
      Result:=CheckPackageDep(AUnitName);
      if not Result then
        // Package was not found. Add a message about a package that must be installed.
        fSettings.AddLogLine(mluWarning,
                        Format(lisConvDelphiPackageRequired, [ADefaultPkgName]));
    end;
  end;
end;

function TConvertDelphiProjPack.SaveAndMaybeClose(aFilename: string): TModalResult;
begin
  Result:=mrOK; // Do nothing. Overridden in project.
end;


{ TConvertDelphiProject }

constructor TConvertDelphiProject.Create(const aProjectFilename: string);
begin
  inherited Create(aProjectFilename, lisConvDelphiConvertDelphiProject);
  fLazPMainSuffix:='.lpi';
  fLazPSuffix:='.lpr';
  fDelphiPSuffix:='.dpr';
end;

destructor TConvertDelphiProject.Destroy;
begin
  inherited Destroy;
end;

function TConvertDelphiProject.CreateInstance: TModalResult;
// Open or create a project. If .lpi file does not exist, create it.
var
  Desc: TConvertedDelphiProjectDescriptor;
begin
  Result:=mrOK;
  if FileExistsUTF8(fLazPMainFilename) then begin
    // there is already a lazarus project -> open it, if not already open
    if (Project1=nil) or (CompareFilenames(Project1.ProjectInfoFile,fLazPMainFilename)<>0) then
      Result:=LazarusIDE.DoOpenProjectFile(fLazPMainFilename,[]);
  end else begin
    // create a new lazarus project
    Desc:=TConvertedDelphiProjectDescriptor.Create;
    try
      Result:=LazarusIDE.DoNewProject(Desc);
    finally
      Desc.Free;
    end;
    if Assigned(Project1) then
      Project1.ProjectInfoFile:=fLazPMainFilename;
  end;
  fProjPack:=Project1;
  if Result<>mrOK then exit;
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
  if LazProject.MainUnitInfo=nil then begin
    // add .lpr file to project as main unit
    MainUnitInfo:=TUnitInfo.Create(fMainUnitConverter.fPascalBuffer);
    Assert(Assigned(IDEEditorOptions), 'TConvertDelphiProject.CreateMainSourceFile: IDEEditorOptions is Nil.');
    MainUnitInfo.DefaultSyntaxHighlighter:=
      IDEEditorOptions.ExtensionToLazSyntaxHighlighter(fMainUnitConverter.LazFileExt);
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
  fProjPack.BaseCompilerOptions.Win32GraphicApp := not fIsConsoleApp;
  fMainUnitConverter.fUsedUnitsTool.IsConsoleApp:=fIsConsoleApp;
  Result:=LazarusIDE.DoOpenEditorFile(fMainUnitConverter.fLazUnitFilename,0,0,[ofQuiet]);
  if Result<>mrOK then exit;
  Result:=mrOK;
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
    AFileName:=AppendPathDelim(LazProject.Directory)+AFileName;
  AFileName:=TrimFilename(AFileName);
  if not FileExistsUTF8(AFileName) then
    exit(mrNo);
  // Create relative search path for project settings.
  RP:=ExtractFilePath(CreateRelativePath(AFileName, LazProject.Directory));
  if (RP<>'') and (fUnitSearchPaths.IndexOf(RP)<0) then
    fUnitSearchPaths.Add(RP);
  PureUnitName:=ExtractFileNameOnly(AFileName);
  if fSettings.OmitProjUnits.Contains(PureUnitName) then
  begin
    fMainUnitConverter.fUsedUnitsTool.Remove(PureUnitName);
    fSettings.AddLogLine(mluNote, Format(lisConvDelphiProjOmittedUnit,[PureUnitName]),
                         fLazPMainFilename);
    TryAddPackageDep(PureUnitName, fSettings.OmitProjUnits[PureUnitName]);
  end
  else begin
    // Check unitname and create UnitInfo.
    CurUnitInfo:=LazProject.UnitInfoWithFilename(AFileName);
    if CurUnitInfo=nil then begin
      if FilenameIsPascalUnit(AFileName) then begin
        CurUnitInfo:=LazProject.UnitWithUnitname(PureUnitName);
        Assert(CurUnitInfo=nil,
          Format('TConvertDelphiProject.AddUnit: Unitname %s exists twice',[PureUnitName]));
      end;
      CurUnitInfo:=TUnitInfo.Create(nil);
      CurUnitInfo.Filename:=AFileName;
      LazProject.AddFile(CurUnitInfo,false);
    end;
    CurUnitInfo.IsPartOfProject:=true;
    OutUnitInfo:=CurUnitInfo;
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
  Screen.BeginWaitCursor;
  FoundUnits:=nil;
  MisUnits:=nil;
  NormalUnits:=nil;
  try
    if not CodeToolBoss.FindDelphiProjectUnits(fMainUnitConverter.fPascalBuffer,
                                         FoundUnits, MisUnits, NormalUnits) then
    begin
      LazarusIDE.DoJumpToCodeToolBossError;
      fErrorMsg:=Format(lisConvProblemsFindingAllUnits, [fSettings.MainFilename]);
      exit(mrCancel);
    end;
    try        // Add all units to the project
      fSettings.AddLogLine(mluProgress, lisConvDelphiFoundAllUnitFiles);
      DebugLn('TConvertDelphiProject.FindAllUnits: '+lisConvDelphiFoundAllUnitFiles);
      for i:=0 to FoundUnits.Count-1 do begin
        CurFilename:=FoundUnits[i];
        p:=Pos(' in ',CurFilename);
        if p>0 then
          delete(CurFilename,1,p+3);
        if CurFilename='' then continue;
        Result:=AddUnit(SwitchPathDelims(CurFilename, True), ui);
        if Result=mrAbort then
          exit;
      end;
      for i:=0 to NormalUnits.Count-1 do begin
        CurFilename:=NormalUnits[i];
        AllPath:=FindDiskFileCaseInsensitive(fSettings.MainPath+CurFilename+'.pas');
        if AllPath<>'' then begin
          Result:=AddUnit(AllPath, ui);
          if Result=mrAbort then
            exit;
        end;
      end;
    finally
      AllPath:=fUnitSearchPaths.DelimitedText;
      LazProject.CompilerOptions.MergeToUnitPaths(AllPath);
      LazProject.CompilerOptions.MergeToIncludePaths(AllPath);
      // Clear caches
      LazProject.DefineTemplates.SourceDirectoriesChanged;
    end;
    // Save project
    Result:=LazarusIDE.DoSaveProject([sfQuietUnitCheck]);
    if Result<>mrOK then exit;
  finally
    FoundUnits.Free;
    MisUnits.Free;
    NormalUnits.Free;
    Screen.EndWaitCursor;
  end;
  Result:=mrOK;
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
  Result:=mrOK;
  ConvUnits:=TObjectList.Create;
  try
  try
  try
    // convert all units and fix .lfm files
    fSettings.AddLogLine(mluImportant, '');
    fSettings.AddLogLine(mluImportant, lisConvDelphiConvertingProjPackUnits);
    for i:=0 to LazProject.UnitCount-1 do begin
      CurUnitInfo:=LazProject.Units[i];
      Application.ProcessMessages;
      // Main LPR file was converted earlier.
      if CurUnitInfo.IsPartOfProject and (CurUnitInfo<>LazProject.MainUnitInfo) then begin
        Result:=ConvertOne(CurUnitInfo);
        if Result=mrIgnore then Continue;
        if Result=mrAbort then Exit;
      end;
    end;
    // During conversion there were more units added to be converted.
    if fUnitsToAddToProject.Count > 0 then begin
      fSettings.AddLogLine(mluImportant, '');
      fSettings.AddLogLine(mluImportant, lisConvDelphiConvertingFoundUnits);
    end;
    for i:=0 to fUnitsToAddToProject.Count-1 do begin
      Application.ProcessMessages;
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
    fSettings.AddLogLine(mluImportant, '');
    fSettings.AddLogLine(mluError, lisConvDelphiExceptionDuringConversion);
    DebugLn(lisConvDelphiExceptionDuringConversion);
    raise;
  end;
  finally
    if Result=mrOK then begin
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
  Result:=mrOK;
end;

function TConvertDelphiProject.GetLazProject: TProject;
begin
  Result:=fProjPack as TProject;
end;

procedure TConvertDelphiProject.SetLazProject(const AValue: TProject);
begin
  fProjPack:=AValue;
end;

function TConvertDelphiProject.ContainsFile(aFileName: string): Boolean;
begin
  Result:=Assigned(LazProject.UnitInfoWithFilename(aFileName));
end;

function TConvertDelphiProject.FindDependencyByName(const PackageName: string): TPkgDependency;
begin
  Result:=LazProject.FindDependencyByName(PackageName);
end;

function TConvertDelphiProject.GetMainName: string;
begin
  Result:='';
  if Assigned(LazProject.MainUnitInfo) then
    Result:=LazProject.MainUnitInfo.Filename;
end;

function TConvertDelphiProject.SaveAndMaybeClose(Filename: string): TModalResult;
var
  UnitIndex: Integer;
  AnUnitInfo: TUnitInfo;
begin
  Result:=mrOK;
  if Filename='' then exit;
  UnitIndex:=LazProject.IndexOfFilename(Filename, [pfsfOnlyEditorFiles]);
  if UnitIndex<0 then exit;
  AnUnitInfo:=LazProject.Units[UnitIndex];
  if AnUnitInfo.OpenEditorInfoCount=0 then exit;
  Result:=LazarusIDE.DoSaveEditorFile(AnUnitInfo.OpenEditorInfo[0].EditorComponent,
                                      [sfCheckAmbiguousFiles,sfQuietUnitCheck]);
  // The main form has UnitIndex = 1. Don't close it.
  if (UnitIndex<>1) and not fSettings.KeepFileOpen then
    Result:=LazarusIDE.DoCloseEditorFile(AnUnitInfo.OpenEditorInfo[0].EditorComponent,
                                         [cfQuiet]);
end;


{ TConvertDelphiPackage }

constructor TConvertDelphiPackage.Create(const aPackageFilename: string);
begin
  inherited Create(aPackageFilename, lisConvDelphiConvertDelphiPackage);
  fLazPMainSuffix:='.lpk'; // Main XML package file, not compatible with Delphi
  fLazPSuffix:='';         // '.lpk' is reserved to the main XML file
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
begin
  fProjPack:=nil;
  if FileExistsUTF8(fLazPMainFilename) then begin
    // there is already a lazarus package file -> open the package editor
    Result:=PackageEditingInterface.DoOpenPackageFile(fLazPMainFilename,[pofAddToRecent],true);
    if Result<>mrOK then exit;
  end;
  // search package in graph
  PkgName:=ExtractFileNameOnly(fLazPMainFilename);
  fProjPack:=PackageGraph.FindPackageWithName(PkgName,nil);
  if fProjPack<>nil then begin
    // there is already a package loaded with this name ...
    if CompareFilenames(LazPackage.Filename,fLazPMainFilename)<>0 then begin
      // ... but it is not the package file we want -> stop
      MessageDlg(lisConvDelphiPackageNameExists,
        Format(lisConvDelphiThereIsAlreadyAPackageWithTheNamePleaseCloseThisPa,
               [PkgName, LineEnding]), mtError, [mbAbort], 0);
      PackageEditingInterface.DoOpenPackageFile(LazPackage.Filename,[pofAddToRecent],true);
      fErrorMsg:=lisConvStoppedBecauseThereIsPackage;
      exit(mrAbort);
    end else begin
      Result:=mrOK;
    end;
  end else begin
    // there is not yet a package with this name -> create a new package with LCL as dependency
    fProjPack:=PackageGraph.CreateNewPackage(PkgName);
    PackageGraph.AddDependencyToPackage(LazPackage,
                  PackageGraph.LCLPackage.CreateDependencyWithOwner(LazPackage));
    LazPackage.Filename:=fLazPMainFilename;
    fProjPack.BaseCompilerOptions.SyntaxMode:='delphi';
    // open a package editor
    PackageEditors.OpenEditor(LazPackage,true);
    // save .lpk file
    PackageEditors.SavePackage(LazPackage,false);
    Result:=mrOK;
  end;
end;

function TConvertDelphiPackage.CreateMainSourceFile: TModalResult;
begin
  Result:=mrOK;
end;

function TConvertDelphiPackage.AddUnit(AFileName: string): TModalResult;
var
  PkgFile, OffendingUnit: TPkgFile;
  CodeBuffer: TCodeBuffer;
  Flags: TPkgFileFlags;
  HasRegisterProc: boolean;
  PureUnitName: String;
begin
  Result:=mrOK;
  if not FilenameIsAbsolute(AFileName) then
    AFileName:=AppendPathDelim(LazPackage.Directory)+AFileName;
  AFileName:=TrimFilename(AFileName);
  if not FileExistsUTF8(AFileName) then
    exit(mrNo);
  PkgFile:=LazPackage.FindPkgFile(AFileName,true,false);
  if PkgFile=nil then begin
    PureUnitName:=ExtractFileNameOnly(AFileName);
    if FilenameIsPascalUnit(AFileName) then begin
      // Check unitname
      OffendingUnit:=LazPackage.FindUnit(PureUnitName);
      Assert(OffendingUnit=nil,
        Format('TConvertDelphiPackage.AddUnit: Unitname %s exists twice',[PureUnitName]));
    end;
    Flags:=[pffAddToPkgUsesSection];
    // Check if the unit has a Register procedure.
    // ToDo: Optimize. The source is read again during unit conversion.
    CodeBuffer:=CodeToolBoss.LoadFile(AFilename, true, false);
    if CodeBuffer<>nil then
      if CodeToolBoss.HasInterfaceRegisterProc(CodeBuffer, HasRegisterProc) then
        if HasRegisterProc then begin
          Include(Flags, pffHasRegisterProc);
          fSettings.AddLogLine(mluNote, Format(lisConvAddingFlagForRegister,[PureUnitName]),
                               fLazPMainFilename);
        end;
    // Add new unit to package
    LazPackage.AddFile(AFileName, PureUnitName, pftUnit, Flags, cpNormal);
  end;
end;

function TConvertDelphiPackage.FindAllUnits: TModalResult;
var
  FoundUnits, MisUnits, NormalUnits: TStrings;
  i: Integer;
  NewSearchPath, AllPath, UselessPath: String;
  CurFilename: string;
  p: LongInt;
begin
  FoundUnits:=nil;
  MisUnits:=nil;
  NormalUnits:=nil;
  try
    if not CodeToolBoss.FindDelphiPackageUnits(fMainUnitConverter.fPascalBuffer,
                                         FoundUnits, MisUnits, NormalUnits) then
    begin
      LazarusIDE.DoJumpToCodeToolBossError;
      fErrorMsg:=Format(lisConvProblemsFindingAllUnits, [fSettings.MainFilename]);
      exit(mrCancel);
    end;
    try
      fSettings.AddLogLine(mluProgress, lisConvDelphiFoundAllUnitFiles);
      DebugLn('TConvertDelphiPackage.FindAllUnits: '+lisConvDelphiFoundAllUnitFiles);
      // Add all units to the package
      for i:=0 to FoundUnits.Count-1 do begin
        CurFilename:=FoundUnits[i];
        p:=System.Pos(' in ',CurFilename);
        if p>0 then
          delete(CurFilename,1,p+3);
        if CurFilename='' then continue;
        Result:=AddUnit(SwitchPathDelims(CurFilename, True));
        if Result=mrAbort then
          exit;
      end;
      for i:=0 to NormalUnits.Count-1 do begin
        CurFilename:=NormalUnits[i];
        AllPath:=FindDiskFileCaseInsensitive(fSettings.MainPath+CurFilename+'.pas');
        if AllPath<>'' then begin
          Result:=AddUnit(AllPath);
          if Result=mrAbort then
            exit;
        end;
      end;
    finally
      AllPath:=fProjPack.SourceDirectories.CreateSearchPathFromAllFiles;
      UselessPath:='.;'+VirtualDirectory+';'+VirtualTempDir+';'+LazPackage.Directory;
      // Set unit paths to find all project units
      NewSearchPath:=MergeSearchPaths(fProjPack.BaseCompilerOptions.OtherUnitFiles,AllPath);
      NewSearchPath:=RemoveSearchPaths(NewSearchPath,UselessPath);
      fProjPack.BaseCompilerOptions.OtherUnitFiles:=MinimizeSearchPath(
               RemoveNonExistingPaths(NewSearchPath,LazPackage.Directory));
      // Set include path
      NewSearchPath:=MergeSearchPaths(fProjPack.BaseCompilerOptions.IncludePath,AllPath);
      NewSearchPath:=RemoveSearchPaths(NewSearchPath,UselessPath);
      fProjPack.BaseCompilerOptions.IncludePath:=MinimizeSearchPath(
               RemoveNonExistingPaths(NewSearchPath,LazPackage.Directory));
      // Clear caches
      fProjPack.DefineTemplates.SourceDirectoriesChanged;
      CodeToolBoss.DefineTree.ClearCache;
    end;
    // Save package
    Result:=PackageEditors.SavePackage(LazPackage,false);
    if Result<>mrOK then exit;
  finally
    FoundUnits.Free;
    MisUnits.Free;
    NormalUnits.Free;
  end;
  Result:=mrOK;
end;

function TConvertDelphiPackage.ConvertAllUnits: TModalResult;
var
  ConvUnits: TObjectList;       // List of ConvertDelphiUnits.

  function ConvertOne(APkgFile: TPkgFile): TModalResult;
  var
    Converter: TDelphiUnit;
  begin
    Converter:=TDelphiUnit.Create(Self, APkgFile.Filename,[]);
    try
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
  i: Integer;
  PkgFile: TPkgFile;
begin
  Result:=mrOK;
  ConvUnits:=TObjectList.Create;
  try
  try
  try
    // Convert all units and fix .lfm files
    fSettings.AddLogLine(mluImportant, '');
    fSettings.AddLogLine(mluImportant, lisConvDelphiConvertingProjPackUnits);
    for i:=0 to LazPackage.FileCount-1 do begin
      PkgFile:=LazPackage.Files[i];
      Application.ProcessMessages;
      ConvertOne(PkgFile);
      if Result=mrIgnore then Continue;
      if Result=mrAbort then Exit;
    end;
    // During conversion there were more units added to be converted.
    if fUnitsToAddToProject.Count > 0 then begin
      fSettings.AddLogLine(mluImportant, '');
      fSettings.AddLogLine(mluImportant, lisConvDelphiConvertingFoundUnits);
    end;
{ ToDo: add more units
    for i:=0 to fUnitsToAddToProject.Count-1 do begin
      Application.ProcessMessages;
      Result:=AddUnit(fUnitsToAddToProject[i]);
      if Result=mrNo then Continue;
      if Result=mrAbort then Exit;
      if Assigned(CurUnitInfo) then begin
        Result:=ConvertOne(CurUnitInfo);
        if Result=mrIgnore then Continue;
        if Result=mrAbort then Exit;
      end;
    end;
}
  except
    fSettings.AddLogLine(mluImportant, '');
    fSettings.AddLogLine(mluError, lisConvDelphiExceptionDuringConversion);
    DebugLn(lisConvDelphiExceptionDuringConversion);
    raise;
  end;
  finally
    if Result=mrOK then begin
      // Try to convert form files also in case of an exception.
      // Unit name replacements etc. are implemented there.
      Result:=ConvertAllFormFiles(ConvUnits);
      // Finally save the package once more
      Result:=PackageEditors.SavePackage(LazPackage,false);
    end;
  end;
  finally
    ConvUnits.Free;  // Owns and frees converter objects.
  end;
end;

function TConvertDelphiPackage.ExtractOptionsFromDelphiSource: TModalResult;
begin
  // TODO: use fDelphiPFilename and LazPackage to get options.
  if fDelphiPFilename<>'' then begin
  end;
  Result:=mrOK;
end;

function TConvertDelphiPackage.GetLazPackage: TLazPackage;
begin
  Result:=fProjPack as TLazPackage;
end;

procedure TConvertDelphiPackage.SetLazPackage(const AValue: TLazPackage);
begin
  fProjPack:=AValue;
end;

function TConvertDelphiPackage.GetMainName: string;
begin
  Result:=LazPackage.Filename;
end;

function TConvertDelphiPackage.ContainsFile(aFileName: string): Boolean;
begin
  Result:=Assigned(LazPackage.FindPkgFile(aFileName, True, False));
end;

function TConvertDelphiPackage.FindDependencyByName(const PackageName: string): TPkgDependency;
begin
  Result:=LazPackage.FindDependencyByName(PackageName);
end;


{ TConvertedDelphiProjectDescriptor }

function TConvertedDelphiProjectDescriptor.InitProject(AProject: TLazProject): TModalResult;
begin
  Result:=inherited InitProject(AProject);
  AProject.LazCompilerOptions.SyntaxMode:='delphi';
end;

end.

