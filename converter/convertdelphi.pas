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
    A goal is to remove LCL dependency. It still comes through packages
      IdeProject and IdePackager + some IDE units.
}
unit ConvertDelphi;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, IniFiles, System.UITypes,
  // CodeTools
  CodeToolManager, DefineTemplates, CodeCache, LinkScanner, FileProcs,
  // LazUtils
  LConvEncoding, FileUtil, LazFileUtils, LazUTF8, LazStringUtils, LazLoggerBase,
  AvgLvlTree,
  // BuildIntf
  PackageIntf, ProjectIntf, ComponentReg, IDEExternToolIntf,
  // IdeConfig
  DialogProcs, SearchPathProcs, ParsedCompilerOpts, CompilerOptions, ProjPackCommon,
  // IdeProject
  Project,
  // IdePackager
  PackageDefs, BasePkgManager, PackageSystem,
  // IDE
  LazarusIDEStrConsts,
  // Converter
  ConvertBase, ConverterTypes, ConvCodeTool, ConvMissingProp, UsedUnits;

const
  SettingDelphiModeTemplName = 'Setting Delphi Mode';

type
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
    // Unit's info, for projects only.
    fUnitInfo: TUnitInfo;
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
  protected
  public
    constructor Create(AOwnerConverter: TConvertDelphiPBase; const AFilename: string;
                       aFlags: TConvertUnitFlags);
    destructor Destroy; override;
  public
    property LazFileExt: string read fLazFileExt write fLazFileExt;
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
    // Either Project or EditablePackage. Typecasted to right types in property getter.
    fProjPack: iProjPack;
    fLazPInfoFilename: string;         // .lpi or .lpk file name
    fDelphiPFilename: string;          // .dpr or .dpk file name
    fLazPInfoSuffix: string;           // '.lpi' or '.lpk' suffix
    fLazPSuffix: string;   // '.lpr' or empty. '.lpk' is for the XML main package
    fDelphiPSuffix: string;            // '.dpr' or '.dpk' suffix
    // Main unit with resource code
    fMainUnitConverter: TDelphiUnit;
    // Unit search path for project settings.
    fUnitSearchPaths: TStringListUTF8Fast;
    // Units that are found and will be added to project or package and converted.
    fUnitsToAddToProject: TStringListUTF8Fast;
    fFilesToDelete: TStringListUTF8Fast;
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
    function AddToProjectLater(const AFileName: string): Boolean;
    function MaybeDeleteFiles: TModalResult;
    function CheckUnitForConversion(aFileName: string): Boolean;
    procedure AddDependency(APackName: String);
    function CheckPackageDep(AUnitName: string): Boolean;
    function TryAddPackageDep(const AUnitName, ADefaultPkgName: string): Boolean;
  protected
    function CreateInstance: TModalResult; virtual; abstract;
    function CreateMainSourceFile: TModalResult; virtual; abstract;
    function FindAllUnits: TModalResult; virtual; abstract;
    function ConvertAllUnits: TModalResult; virtual; abstract;
    function ExtractOptionsFromDelphiSource: TModalResult; virtual; abstract;
    // Abstract base for the fake Project / Package virtual methods.
    function GetMainName: string; virtual; abstract;
    function SaveAndMaybeClose(const {%H-}aFilename: string): TModalResult; virtual;
    function ContainsFile(const aFileName: string): Boolean; virtual; abstract;
    function FindDependencyByName(const PackageName: string): TPkgDependency; virtual; abstract;
    function FirstDependency: TPkgDependency; virtual; abstract;
  public
    constructor Create(const AFilename, ADescription: string);
    destructor Destroy; override;
    function DoMissingUnits(AUsedUnitsTool: TUsedUnitsToolBase): integer; override;
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
    function SaveAndMaybeClose(const Filename: string): TModalResult; override;
    function ContainsFile(const aFileName: string): Boolean; override;
    function FindDependencyByName(const PackageName: string): TPkgDependency; override;
    function FirstDependency: TPkgDependency; override;
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
    function ContainsFile(const aFileName: string): Boolean; override;
    function FindDependencyByName(const PackageName: string): TPkgDependency; override;
    function FirstDependency: TPkgDependency; override;
  public
    constructor Create(const aPackageFilename: string);
    destructor Destroy; override;
  public
    property LazPackage: TLazPackage read GetLazPackage write SetLazPackage;
  end;

var
  UnitInfoClass: TUnitInfoClass;


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
  ProjectDir:=ExtractFilePath(AProjPack.fLazPInfoFilename);
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
  if not fOwnerConverter.Settings.BeginCodeTools then
    fOwnerConverter.Settings.AddLogLine(mluFatal,
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
  if FileExistsUTF8(Result) then exit;
  Result:=ChangeFileExt(fOrigUnitFilename,'.fmx');
  if FileExistsUTF8(Result) then exit;
  Result:=ChangeFileExt(fOrigUnitFilename,'.FMX');
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
  Assert(Assigned(fOwnerConverter), 'TDelphiUnit.CopyAndLoadFile: fOwnerConverter not assigned.');
  Result:=fOwnerConverter.Settings.CloseEditorFile(fOrigUnitFilename);
  if Result<>mrOK then exit;
  // Copy/rename fLazUnitFilename based on fOrigUnitFilename.
  Result:=fOwnerConverter.Settings.RenameDelphiToLazFile(fOrigUnitFilename,
                  fLazFileExt, fLazUnitFilename, cdtlufRenameLowercase in fFlags);
  if Result<>mrOK then exit;
  fPascalBuffer:=nil;                         // Read the code in.
  Result:=LoadCodeBuffer(fPascalBuffer,fLazUnitFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk],true);
  if Result<>mrOK then exit;
  // Change encoding to UTF-8
  if (fPascalBuffer.DiskEncoding<>EncodingUTF8)
  and (fPascalBuffer.DiskEncoding<>EncodingUTF8BOM) then
  begin
    fOwnerConverter.Settings.AddLogLine(mluNote,
      Format(lisConvDelphiChangedEncodingToUTF8, [fPascalBuffer.DiskEncoding]),
      fLazUnitFilename);
    fPascalBuffer.DiskEncoding:=EncodingUTF8; // Takes effect when buffer is saved.
  end;
  // Create a shared link for codetools.
  Assert(fCTLink=Nil, 'fCTLink should be Nil in CopyAndLoadFile');
  fCTLink:=TCodeToolLink.Create(fPascalBuffer);
  fCTLink.Settings:=fOwnerConverter.Settings;
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
      fUsedUnitsTool.IsConsoleApp:=IsConsoleApp;
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
    Result:=fOwnerConverter.Settings.CloseEditorFile(ADfmFilename);
    if Result<>mrOK then exit;
    // Save before using .dfm.
    Result:=fOwnerConverter.Settings.MaybeBackupFile(ADfmFilename);
    if Result<>mrOK then exit;
  end;
  if fOwnerConverter.Settings.SameDfmFile then
    LfmFilename:=ADfmFilename
  else begin
    // Create a form file name based on the unit file name.
    with fOwnerConverter.Settings do
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
        if fOwnerConverter.Settings.SupportDelphi then
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
      DFMConverter.Settings:=fOwnerConverter.Settings;
      Result:=DFMConverter.ConvertDfmToLfm(LfmFilename);
      if Result<>mrOK then exit;
    finally
      DFMConverter.Free;
    end;
    // Read pascal unit code in.
    Result:=LoadCodeBuffer(TempLFMBuffer,LfmFilename,
                           [lbfCheckIfText,lbfUpdateFromDisk],true);
    // Change encoding to UTF-8
    if (TempLFMBuffer.DiskEncoding<>EncodingUTF8)
    and (TempLFMBuffer.DiskEncoding<>EncodingUTF8BOM) then
    begin
      fOwnerConverter.Settings.AddLogLine(mluNote,
        Format(lisConvDelphiChangedEncodingToUTF8, [TempLFMBuffer.DiskEncoding]),
        fLazUnitFilename);
      TempLFMBuffer.DiskEncoding:=EncodingUTF8;
      TempLFMBuffer.Save;
    end;
    // Read form file code in.
    if not fOwnerConverter.Settings.SameDfmFile then
      Result:=LoadCodeBuffer(fLFMBuffer,LfmFilename,
                             [lbfCheckIfText,lbfUpdateFromDisk],true);
  end;
end;

function TDelphiUnit.ReduceMissingUnits: TModalResult;
// Find or comment out some / all of missing units.
begin
  Result:=mrOK;
  // Comment out automatically units that were commented in other files.
  fUsedUnitsTool.MainUsedUnits.CommentAutomatic(fOwnerConverter.AllCommentedUnits);
  fUsedUnitsTool.ImplUsedUnits.CommentAutomatic(fOwnerConverter.AllCommentedUnits);
  // Remove omitted units from MissingUnits.
  fUsedUnitsTool.MainUsedUnits.OmitUnits;
  fUsedUnitsTool.ImplUsedUnits.OmitUnits;
  // Try to find from subdirectories scanned earlier.
  if fOwnerConverter.DoMissingUnits(fUsedUnitsTool)=0 then exit;
  if fUsedUnitsTool.MissingUnitCount=0 then exit;
  // Interactive dialog for searching unit.
  Result:=fOwnerConverter.Settings.AskUnitPath(fOwnerConverter, fUsedUnitsTool,
      ExtractFileName(fLazUnitFilename), fOwnerConverter.Settings.SupportDelphi);
  if Result=mrAbort then
    fOwnerConverter.ErrorMsg:=Format(lisConvUserSelectedToEndConversion,[fOrigUnitFilename]);
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
  if fOwnerConverter.Settings.UnitsReplaceMode<>rlDisabled then begin
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
    ConvTool.IsConsoleApp:=fOwnerConverter.IsConsoleApp;
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
      LfmFixer.Settings:=fOwnerConverter.Settings;
      LfmFixer.UsedUnitsTool:=fUsedUnitsTool;
      LfmFixer.RootMustBeClassInUnit:=true;
      LfmFixer.RootMustBeClassInIntf:=true;
      LfmFixer.ObjectsMustExist:=true;
      if LfmFixer.ConvertAndRepair<>mrOK then begin
        fOwnerConverter.Settings.JumpToMessage;
        fOwnerConverter.ErrorMsg:=Format(lisConvProblemsRepairingFormFile,
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

function TDelphiUnit.FixIncludeFiles: TModalResult;
// fix include filenames
var
  FoundIncludeFiles: TStrings;
  MissingIncludeFilesCodeXYPos: TFPList;
  CodePos: PCodeXYPosition;
  Msg, s: string;
  i: Integer;
  //OldChange: Boolean;
begin
  Result:=mrOK;
  FoundIncludeFiles:=Nil;
  MissingIncludeFilesCodeXYPos:=Nil;
  //OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;  !!!
  //LazarusIDE.OpenEditorsOnCodeToolChange:=False;
  with fCTLink, fOwnerConverter do
  try
    if CodeTool.DirectoryCache=nil then exit;
    if CodeTool.FixIncludeFilenames(Code,SrcCache,FoundIncludeFiles,MissingIncludeFilesCodeXYPos)
    then begin
      if Assigned(FoundIncludeFiles) then begin
        // List the include files in log.
        Msg:=lisConvRepairingIncludeFiles;
        for i:=0 to FoundIncludeFiles.Count-1 do begin
          Settings.MaybeBackupFile(FoundIncludeFiles[i]);
          s:=CreateRelativePath(FoundIncludeFiles[i], Settings.MainPath);
          if i>0 then
            Msg:=Msg+'; ';
          Msg:=Msg+s;
        end;
        Settings.AddLogLine(mluNote, Msg, fLazUnitFilename);
      end;
    end
    else begin
      if MissingIncludeFilesCodeXYPos<>nil then begin
        for i:=0 to MissingIncludeFilesCodeXYPos.Count-1 do begin
          CodePos:=PCodeXYPosition(MissingIncludeFilesCodeXYPos[i]);
          Msg:=Format(lisConvDelphiMissingIncludeFile,
                 [CodePos^.Code.Filename,IntToStr(CodePos^.y),IntToStr(CodePos^.x)]);
          Settings.AddLogLine(mluError, Msg, fLazUnitFilename);
        end;
      end;
      ErrorMsg:=Format(lisConvProblemsFixingIncludeFile, [fOrigUnitFilename]);
      Result:=mrCancel;
    end;
  finally
    FoundIncludeFiles.Free;
    CodeCache.FreeListOfPCodeXYPosition(MissingIncludeFilesCodeXYPos);
    //LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
  end;
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
      Settings.MainFilenames.Add(s);
  end;
  PrevSelectedPath:=Settings.MainPath;
  // use a template for compiler mode delphi for a single directory
  fDirTemplate:=TDefineTemplate.Create(SettingDelphiModeTemplName,
    'Mode Delphi for single unit conversion', '', Settings.MainPath, da_Directory);
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
  Result:=Settings.RunSettingsForm(Nil);
  if Result=mrOK then begin
    try
    try
      Settings.ClearLog;
      for i:=0 to Settings.MainFilenames.Count-1 do begin
        Settings.ProcessMessages;
        if i>0 then
          Settings.AddLogLine(mluImportant, '');
        DelphiUnit:=TDelphiUnit.Create(Self, Settings.MainFilenames[i], []);
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
          Result:=Settings.SaveEditorFile(fLazUnitFilename);
        finally
          DelphiUnit.Free;
        end;
      end;
    except
      on E: Exception do begin
        ErrorMsg:=E.Message;
        Result:=mrAbort;
      end;
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
  fUnitSearchPaths:=TStringListUTF8Fast.Create;
  fUnitSearchPaths.Delimiter:=';';
  fUnitSearchPaths.StrictDelimiter:=True;
  AllCommentedUnits:=TStringListUTF8Fast.Create;
  AllCommentedUnits.Sorted:=True;
  CachedUnitNames:=TStringToStringTree.Create(False);
  CachedRealFileNames:=TStringToStringTree.Create(True);
  fUnitsToAddToProject:=TStringListUTF8Fast.Create;
  fUnitsToAddToProject.Sorted:=True;
  fFilesToDelete:=TStringListUTF8Fast.Create;
  fFilesToDelete.Sorted:=True;
  fMainUnitConverter:=nil;
end;

destructor TConvertDelphiProjPack.Destroy;
begin
  fMainUnitConverter.Free;
  fFilesToDelete.Free;
  fUnitsToAddToProject.Free;
  CachedRealFileNames.Free;
  CachedUnitNames.Free;
  AllCommentedUnits.Free;
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
  if FilenameExtIs(Settings.MainFilename,'dproj') then begin
    ErrorMsg := lisConvDprojFileNotSupportedYet;
    Exit(mrCancel);
  end;
  // Start scanning unit files one level above project path. The GUI will appear
  // without delay but then we must wait for the thread before continuing.
  CacheUnitsThread:=TCacheUnitsThread.Create(Self,
                          ResolveDots(Settings.MainPath+'..'+DirectorySeparator));
  try
  try
    Result:=Settings.RunSettingsForm(CacheUnitsThread); // Get settings from user.
    if Result=mrOK then begin
      StartTime:=Now;
      Settings.ClearLog;
      // create/open lazarus project or package file
      fLazPInfoFilename:=Settings.DelphiToLazFilename(Settings.MainFilename,
                                                      fLazPInfoSuffix, false);
      // Find Delphi project / package file name
      if FilenameExtIs(Settings.MainFilename,fDelphiPSuffix) then
        fDelphiPFilename:=Settings.MainFilename
      else
        fDelphiPFilename:=ChangeFileExt(Settings.MainFilename,fDelphiPSuffix);
      if not FileExistsUTF8(fDelphiPFilename) then
        fDelphiPFilename:=FindDiskFileCaseInsensitive(Settings.MainFilename);
// ? fDelphiPFilename:=CodeToolBoss.DirectoryCachePool.FindDiskFilename(fSettings.MainFilename);

      // Actual conversion.
      Result:=ConvertSub;
    end;
  except
    on E: Exception do begin
      ErrorMsg:=E.Message;
      Result:=mrAbort;
    end;
  end;
  finally
    CacheUnitsThread.Free;
    EndTime:=Now;
    s:=FormatDateTime('hh:nn:ss', EndTime-StartTime);
    if (Result<>mrAbort) and (s<>'00:00:00') then
      Settings.AddLogLine(mluProgress, Format(lisConvDelphiConversionTook,[s]));
    EndConvert(Result);
  end;
end;

function TConvertDelphiProjPack.ConvertSub: TModalResult;
begin
  // Project / package instance.
  Result:=CreateInstance;
  if Result<>mrOK then exit;
  // Create main source file (.lpr/.lpk) (only copy, no conversion)
  fMainUnitConverter:=TDelphiUnit.Create(Self, Settings.MainFilename,[]);
  if Settings.SupportDelphi then
    fMainUnitConverter.LazFileExt:=ExtractFileExt(Settings.MainFilename)
  else
    fMainUnitConverter.LazFileExt:=fLazPSuffix; // '.lpr' or ''
  Result:=fMainUnitConverter.CopyAndLoadFile;
  if Result<>mrOK then exit;
  fMainUnitConverter.fUsedUnitsTool.IsMainFile:=True;
  Result:=CreateMainSourceFile; // More actions for the main source file.
  if Result<>mrOK then exit;
  Settings.ProcessMessages;
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
    Settings.ProcessMessages;
    // Convert .lpr/.lpk file. Main source file was loaded earlier. Now just convert.
    Result:=fMainUnitConverter.ConvertUnitFile;
    if Result<>mrOK then exit;
    Settings.ProcessMessages;
    Result:=fMainUnitConverter.ConvertFormFile;
    if Result<>mrOK then exit;
    Settings.ProcessMessages;
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
  if not Settings.SameDfmFile then begin
    Settings.AddLogLine(mluImportant, '');
    Settings.AddLogLine(mluImportant, lisConvDelphiRepairingFormFiles);
    DebugLn('');
    DebugLn('TConvertDelphiProjPack.ConvertAllFormFiles: '+lisConvDelphiRepairingFormFiles);
  end;
  Settings.BeginWaitCursor;
  try
    for i:=0 to ConverterList.Count-1 do begin
      Converter:=TDelphiUnit(ConverterList[i]); // Converter created in cycle1.
      Settings.ProcessMessages;
      Result:=Converter.ConvertFormFile;
      Result:=Settings.CheckFailed(Result, Converter.fOrigUnitFilename);
      if Result<>mrOK then exit;
      // Finally save and maybe close the file.
      Result:=SaveAndMaybeClose(Converter.fLazUnitFilename);
      if Result<>mrOK then exit;
    end;
  finally
    Settings.EndWaitCursor;
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
      Settings.AddLogLine(mluNote,
        Format(lisConvDelphiAddedPackageDependency,[LazarusPkgName]),
        fLazPInfoFilename);
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
  sl: TStringList;
  i: Integer;
  Line, s: string;
  c: char;
begin
  try
    sl:=TStringList.Create;
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

  if Settings.DelphiDefine then begin
    // "Borland" and "Ver150" are defined by Delphi7.
    // "Delphi7" and "Compiler6_Up" are defined by Jedi library based on other settings.
    //  They are needed because Jedi.inc undefines "Borland" when "FPC" is defined. Nuts.
    // PUREPASCAL is defined by some code to not use x86 assembly code.
    s:='-dBorland -dVer150 -dDelphi7 -dCompiler6_Up -dPUREPASCAL';
    Options.CustomOptions:=s;
    Settings.AddLogLine(mluNote, Format(lisConvDelphiAddedCustomOptionDefines,[s]),
      fLazPInfoFilename);
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
      RealFileName:=CachedRealFileNames[UpperCase(mUnit)];
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

function TConvertDelphiProjPack.DoMissingUnits(AUsedUnitsTool: TUsedUnitsToolBase): integer;
// Locate unit names from earlier cached list or from packages.
// Return the number of units still missing.
begin
  MissingUnitsSub(TUsedUnitsTool(AUsedUnitsTool).MainUsedUnits);
  MissingUnitsSub(TUsedUnitsTool(AUsedUnitsTool).ImplUsedUnits);
  Result:=TUsedUnitsTool(AUsedUnitsTool).MissingUnitCount;
end;

function TConvertDelphiProjPack.AddToProjectLater(const AFileName: string): Boolean;
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
  for i:=fFilesToDelete.Count-1 downto 0 do begin
    s:=fFilesToDelete[i];
    // Ask confirmation from user.
    if Settings.MsgDialog(lisConvDelphiUnitnameExistsInLCL,
         Format(lisConvDelphiUnitWithNameExistsInLCL,[ExtractFileNameOnly(s),s]),
         mtConfirmation, mbYesNo) = mrYes
    then begin
      // Delete from file system because compiler would find it otherwise.
      if not DeleteFileUTF8(s) then
        exit(mrCancel);
      Settings.AddLogLine(mluNote, Format(lisConvDeletedFile,[s]));
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
  if ExtractFilePath(aFileName)=Settings.MainPath then begin
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

procedure TConvertDelphiProjPack.AddDependency(APackName: String);
// A unit was found from a package. Add the package as a dependency and open it.
var
  Dep: TPkgDependency;
begin
  if APackName='LCLBase' then
    APackName:='LCL';
  Dep:=FindDependencyByName(APackName);
  if Assigned(Dep) then Exit;               // Already added.
  fProjPack.AddPackageDependency(APackName);
  Settings.AddLogLine(mluNote, Format(lisConvDelphiAddedPackageDependency,[APackName]),
                      fLazPInfoFilename);
  Dep:=FindDependencyByName(APackName);
  if Assigned(Dep) then
    PackageGraph.OpenDependency(Dep,false);
end;

function TConvertDelphiProjPack.CheckPackageDep(AUnitName: string): Boolean;
// Check if the given unit can be found in existing packages. Add a dependency if found.
// This is called only if the unit is reported as missing.
// Returns True if the unit was found.
var
  RegComp: TRegisteredComponent;
  PackFile: TPkgFile;
  Package: TLazPackage;
  i, Cnt: Integer;
  SearchPath: String;
  Files: TFilenameToStringTree;
  FileItem: PStringToStringItem;
begin
  Result:=False;
  PackFile:=PackageGraph.FindUnitInAllPackages(AUnitName, True);
  if Assigned(PackFile) then begin
    AddDependency(PackFile.LazPackage.Name);
    Exit(True);
  end;
  // Do heuristics. Units of some registered components are not included in
  //  their package file. Try to find 'T'+UnitName. Helps with Indy package.
  RegComp := IDEComponentPalette.FindRegComponent('T'+AUnitName);
  if RegComp is TPkgComponent then begin
    PackFile:=TPkgComponent(RegComp).PkgFile;
    Assert(Assigned(PackFile), 'TConvertDelphiProjPack.CheckPackageDep: PackFile=Nil.');
    AddDependency(PackFile.LazPackage.Name);
    Exit(True);
  end;
  // Try to find the unit from all open packages by their search path.
  // Again needed when the unit is not included in a package file.
  Cnt:=PackageGraph.Count;
  Files:=TFilenameToStringTree.Create(false);
  try
    for i:=0 to Cnt-1 do begin
      Package:=PackageGraph.Packages[i];
      if Package.IsVirtual then
        Continue;   // Skip unsaved package.
      if PackageGraph.LazarusBasePackages.IndexOf(Package)>=0 then
        Continue;   // Skip base packages.
      SearchPath:=Package.CompilerOptions.GetParsedPath(pcosUnitPath,icoNone,false)
         +';'+Package.SourceDirectories.CreateSearchPathFromAllFiles
         +';'+Package.Directory;
      SearchPath:=TrimSearchPath(SearchPath,'',true);
      Files.Clear;
      CollectFilesInSearchPath(SearchPath,Files);
      for FileItem in Files do
        if FilenameIsPascalUnit(FileItem^.Name)
            and (CompareFilenameOnly(PChar(Pointer(FileItem^.Name)),Length(FileItem^.Name),
                                    PChar(Pointer(AUnitName)),Length(AUnitName))=0) then
        begin
          AddDependency(Package.Name);
          Exit(True);
        end;
    end;
  finally
    Files.Free;
  end;
  // ToDo: Install the required package automatically from a repository...
end;

function TConvertDelphiProjPack.TryAddPackageDep(const AUnitName,
  ADefaultPkgName: string): Boolean;
var
  Dep: TPkgDependency;
begin
  Result:=False;
  if ADefaultPkgName='' then Exit;
  Dep:=FindDependencyByName(ADefaultPkgName);
  if Assigned(Dep) then Exit;  // Already added.
  // Add dependency based on unit name (default is ignored).
  Result:=CheckPackageDep(AUnitName);
  if Result then Exit;
  // Package was not found. Add a message about a package that must be installed.
  Settings.AddLogLine(mluWarning,
                      Format(lisConvDelphiPackageRequired, [ADefaultPkgName]));
end;

function TConvertDelphiProjPack.SaveAndMaybeClose(const aFilename: string): TModalResult;
begin
  Result:=mrOK; // Do nothing. Overridden in project.
end;


{ TConvertDelphiProject }

constructor TConvertDelphiProject.Create(const aProjectFilename: string);
begin
  inherited Create(aProjectFilename, lisConvDelphiConvertDelphiProject);
  fLazPInfoSuffix:='.lpi';
  fLazPSuffix:='.lpr';
  fDelphiPSuffix:='.dpr';
end;

destructor TConvertDelphiProject.Destroy;
begin
  inherited Destroy;
end;

function TConvertDelphiProject.CreateInstance: TModalResult;
// Open or create a project. If .lpi file does not exist, create it.
begin
  Result:=mrCancel;
  if FileExistsUTF8(fLazPInfoFilename)
  and ((Project1=nil) or (CompareFilenames(Project1.ProjectInfoFile,fLazPInfoFilename)<>0)) then
    // there is already a lazarus project file. Delete it.
    DeleteFile(fLazPInfoFilename);
      //Result:=LazarusIDE.DoOpenProjectFile(fLazPInfoFilename,[]);
  //end else begin
  fProjPack:=Settings.NewProject(fLazPInfoFilename);
  //end;
  // save to disk (this makes sure, all editor changes are saved too)
  LazProject.SkipCheckLCLInterfaces:=True; // Don't add Interfaces unit automatically.
  Result:=Settings.SaveProject(False);
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
    MainUnitInfo:=UnitInfoClass.Create(fMainUnitConverter.fPascalBuffer);
    MainUnitInfo.IsPartOfProject:=true;
    LazProject.AddFile(MainUnitInfo,false);
    LazProject.MainFileID:=0;
  end else begin
    // replace main unit in project
    LazProject.MainUnitInfo.Source:=fMainUnitConverter.fPascalBuffer;
  end;
  // Scan LPR file for directives. Sets IsConsoleApp flag.
  ConvTool:=TConvDelphiCodeTool.Create(fMainUnitConverter.fCTLink);
  try
    IsConsoleApp:=ConvTool.FindApptypeConsole;
  finally
    ConvTool.Free;
  end;
  fProjPack.BaseCompilerOptions.Win32GraphicApp := not IsConsoleApp;
  fMainUnitConverter.fUsedUnitsTool.IsConsoleApp:=IsConsoleApp;
  Result:=Settings.OpenEditorFile(fMainUnitConverter.fLazUnitFilename);
  if Result<>mrOK then exit;
  Result:=mrOK;
end;

function TConvertDelphiProject.AddUnit(AFileName: string;
                                       out OutUnitInfo: TUnitInfo): TModalResult;
// add new unit to project
var
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
  if Settings.OmitProjUnits.Contains(PureUnitName) then
  begin
    fMainUnitConverter.fUsedUnitsTool.Remove(PureUnitName);
    Settings.AddLogLine(mluNote, Format(lisConvDelphiProjOmittedUnit,[PureUnitName]),
                         fLazPInfoFilename);
    TryAddPackageDep(PureUnitName, Settings.OmitProjUnits[PureUnitName]);
  end
  else begin
    // Check unitname and create UnitInfo.
    OutUnitInfo:=LazProject.UnitInfoWithFilename(AFileName);
    if OutUnitInfo=nil then begin
      if FilenameHasPascalExt(AFileName) then begin
        OutUnitInfo:=LazProject.UnitWithUnitname(PureUnitName);
        Assert(OutUnitInfo=nil,
          Format('TConvertDelphiProject.AddUnit: Unitname %s exists twice',[PureUnitName]));
      end;
      OutUnitInfo:=UnitInfoClass.Create(nil);
      OutUnitInfo.Filename:=AFileName;
      LazProject.AddFile(OutUnitInfo,false);
    end;
    OutUnitInfo.IsPartOfProject:=true;
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
  Settings.BeginWaitCursor;
  FoundUnits:=nil;
  MisUnits:=nil;
  NormalUnits:=nil;
  try
    if not CodeToolBoss.FindDelphiProjectUnits(fMainUnitConverter.fPascalBuffer,
                                         FoundUnits, MisUnits, NormalUnits) then
    begin
      Settings.JumpToCodeToolBossError;
      ErrorMsg:=Format(lisConvProblemsFindingAllUnits, [Settings.MainFilename]);
      exit(mrCancel);
    end;
    try        // Add all units to the project
      Settings.AddLogLine(mluProgress, lisConvDelphiFoundAllUnitFiles);
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
        AllPath:=FindDiskFileCaseInsensitive(Settings.MainPath+CurFilename+'.pas');
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
    Result:=Settings.SaveProject(True);
    if Result<>mrOK then exit;
  finally
    FoundUnits.Free;
    MisUnits.Free;
    NormalUnits.Free;
    Settings.EndWaitCursor;
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
    Settings.AddLogLine(mluNote, Format(lisConvDelphiConvertingUnit,[AUnitInfo.Filename]), AUnitInfo.Filename);
    Converter:=TDelphiUnit.Create(Self, AUnitInfo.Filename,[]);
    try
      Converter.fUnitInfo:=AUnitInfo;
      ConvUnits.Add(Converter);
      Result:=Converter.CopyAndLoadFile;
      if Result<>mrOK then exit;
      Result:=Settings.CheckFailed(Result, Converter.fOrigUnitFilename);
      if Result<>mrOK then exit;
      Result:=Converter.ConvertUnitFile;
    except
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
    Settings.AddLogLine(mluImportant, '');
    Settings.AddLogLine(mluImportant, lisConvDelphiConvertingProjPackUnits);
    for i:=0 to LazProject.UnitCount-1 do begin
      CurUnitInfo:=LazProject.Units[i];
      Settings.ProcessMessages;
      // Main LPR file was converted earlier.
      if CurUnitInfo.IsPartOfProject and (CurUnitInfo<>LazProject.MainUnitInfo) then begin
        Result:=ConvertOne(CurUnitInfo);
        if Result=mrIgnore then Continue;
        if Result=mrAbort then Exit;
      end;
    end;
    // During conversion there were more units added to be converted.
    if fUnitsToAddToProject.Count > 0 then begin
      Settings.AddLogLine(mluImportant, '');
      Settings.AddLogLine(mluImportant, lisConvDelphiConvertingFoundUnits);
    end;
    for i:=0 to fUnitsToAddToProject.Count-1 do begin
      Settings.ProcessMessages;
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
    on E: Exception do begin
      DebugLn(E.Message);
      Result:=mrAbort;
      raise;
    end;
  end;
  finally
    if Result=mrOK then begin
      // Try to convert form files also in case of an exception.
      // Unit name replacements etc. are implemented there.
      Result:=ConvertAllFormFiles(ConvUnits);
      // Finally save project once more
      if Result=mrOK then
        Result:=Settings.SaveProject(True);
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

function TConvertDelphiProject.ContainsFile(const aFileName: string): Boolean;
begin
  Result:=Assigned(LazProject.UnitInfoWithFilename(aFileName));
end;

function TConvertDelphiProject.FindDependencyByName(const PackageName: string): TPkgDependency;
begin
  Result:=LazProject.FindDependencyByName(PackageName);
end;

function TConvertDelphiProject.FirstDependency: TPkgDependency;
begin
  Result:=LazProject.FirstRequiredDependency;
end;

function TConvertDelphiProject.GetMainName: string;
begin
  Result:='';
  if Assigned(LazProject.MainUnitInfo) then
    Result:=LazProject.MainUnitInfo.Filename;
end;

function TConvertDelphiProject.SaveAndMaybeClose(const Filename: string): TModalResult;
var
  UnitIndex: Integer;
  AUnitInfo: TUnitInfo;
begin
  Result:=mrOK;
  if Filename='' then exit;
  UnitIndex:=LazProject.IndexOfFilename(Filename, [pfsfOnlyEditorFiles]);
  if UnitIndex<0 then exit;
  AUnitInfo:=LazProject.Units[UnitIndex];
  if not AUnitInfo.HasOpenEditors then exit;
  Result:=Settings.SaveEditorFile(AUnitInfo, True);
  // The main form has UnitIndex = 1. Don't close it.
  if (UnitIndex<>1) and not Settings.KeepFileOpen then
    Result:=Settings.CloseEditorFile(AUnitInfo, True);
end;


{ TConvertDelphiPackage }

constructor TConvertDelphiPackage.Create(const aPackageFilename: string);
begin
  inherited Create(aPackageFilename, lisConvDelphiConvertDelphiPackage);
  fLazPInfoSuffix:='.lpk'; // Main XML package file, not compatible with Delphi
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
  if FileExistsUTF8(fLazPInfoFilename) then begin
    // there is already a lazarus package file -> open the package editor
    Result:=PackageEditingInterface.DoOpenPackageFile(fLazPInfoFilename,[pofAddToRecent],true);
    if Result<>mrOK then exit;
  end;
  // search package in graph
  PkgName:=ExtractFileNameOnly(fLazPInfoFilename);
  fProjPack:=PackageGraph.FindPackageWithName(PkgName,nil);
  if fProjPack<>nil then begin
    // there is already a package loaded with this name ...
    if CompareFilenames(LazPackage.Filename,fLazPInfoFilename)<>0 then begin
      // ... but it is not the package file we want -> stop
      Settings.AddLogLine(mluFatal, Format(lisConvDelphiThereIsAlreadyAPackage,[PkgName]));
      //MessageDlg(lisConvDelphiPackageNameExists,
      //  Format(lisConvDelphiThereIsAlreadyAPackageWithTheNamePleaseCloseThisPa,
      //         [PkgName, LineEnding]), mtError, [mbAbort], 0);
      PackageEditingInterface.DoOpenPackageFile(LazPackage.Filename,[pofAddToRecent],true);
      ErrorMsg:=lisConvStoppedBecauseThereIsPackage;
      exit(mrAbort);
    end else begin
      Result:=mrOK;
    end;
  end else begin
    // there is not yet a package with this name -> create a new package with LCL as dependency
    fProjPack:=PackageGraph.CreateNewPackage(PkgName);
    PackageGraph.AddDependencyToPackage(LazPackage,
                  PackageGraph.LCLPackage.CreateDependencyWithOwner(LazPackage));
    LazPackage.Filename:=fLazPInfoFilename;
    fProjPack.BaseCompilerOptions.SyntaxMode:='delphi';
    PkgBoss.DoOpenPackage(LazPackage,[],False);  // open a package editor
    Result:=PkgBoss.DoSavePackage(LazPackage,[]) // save .lpk file
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
    if FilenameHasPascalExt(AFileName) then begin
      // Check unitname
      OffendingUnit:=LazPackage.FindUnit(PureUnitName);
      Assert(OffendingUnit=nil,
        Format('TConvertDelphiPackage.AddUnit: Unitname %s exists twice',[PureUnitName]));
    end;
    Flags:=[pffAddToPkgUsesSection];
    // Check if the unit has a Register procedure.
    // ToDo: Optimize. The source is read again during unit conversion.
    CodeBuffer:=CodeToolBoss.LoadFile(AFilename, true, false);
    if (CodeBuffer<>nil) and CodeToolBoss.HasInterfaceRegisterProc(CodeBuffer) then
    begin
      Include(Flags, pffHasRegisterProc);
      Settings.AddLogLine(mluNote, Format(lisConvAddingFlagForRegister,[PureUnitName]),
                          fLazPInfoFilename);
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
      Settings.JumpToCodeToolBossError;
      ErrorMsg:=Format(lisConvProblemsFindingAllUnits, [Settings.MainFilename]);
      exit(mrCancel);
    end;
    try
      Settings.AddLogLine(mluProgress, lisConvDelphiFoundAllUnitFiles);
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
        AllPath:=FindDiskFileCaseInsensitive(Settings.MainPath+CurFilename+'.pas');
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
    Result:=PkgBoss.DoSavePackage(LazPackage,[])  // save .lpk file
  finally
    FoundUnits.Free;
    MisUnits.Free;
    NormalUnits.Free;
  end;
end;

function TConvertDelphiPackage.ConvertAllUnits: TModalResult;
var
  ConvUnits: TObjectList;       // List of ConvertDelphiUnits.

  function ConvertOne(APkgFile: TPkgFile): TModalResult;
  var
    Converter: TDelphiUnit;
  begin
    Settings.AddLogLine(mluNote, Format(lisConvDelphiConvertingUnit,[APkgFile.Filename]), APkgFile.Filename);
    Converter:=TDelphiUnit.Create(Self, APkgFile.Filename,[]);
    try
      ConvUnits.Add(Converter);
      Result:=Converter.CopyAndLoadFile;
      if Result<>mrOK then exit;
      Result:=Settings.CheckFailed(Result, Converter.fOrigUnitFilename);
      if Result<>mrOK then exit;
      Result:=Converter.ConvertUnitFile;
    except
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
    Settings.AddLogLine(mluImportant, '');
    Settings.AddLogLine(mluImportant, lisConvDelphiConvertingProjPackUnits);
    for i:=0 to LazPackage.FileCount-1 do begin
      PkgFile:=LazPackage.Files[i];
      Settings.ProcessMessages;
      ConvertOne(PkgFile);
      if Result=mrIgnore then Continue;
      if Result=mrAbort then Exit;
    end;
    // During conversion there were more units added to be converted.
    if fUnitsToAddToProject.Count > 0 then begin
      Settings.AddLogLine(mluImportant, '');
      Settings.AddLogLine(mluImportant, lisConvDelphiConvertingFoundUnits);
    end;
{ ToDo: add more units
    for i:=0 to fUnitsToAddToProject.Count-1 do begin
      Settings.ProcessMessages;
      Result:=AddUnit(fUnitsToAddToProject[i]);
      if Result in [mrNo, mrCancel] then Continue;
      if Result=mrAbort then Exit;
      if Assigned(CurUnitInfo) then begin
        Result:=ConvertOne(CurUnitInfo);
        if Result=mrIgnore then Continue;
        if Result=mrAbort then Exit;
      end;
    end;
}
  except
    on E: Exception do begin
      DebugLn(E.Message);
      Result:=mrAbort;
      raise;
    end;
  end;
  finally
    if Result=mrOK then begin
      // Try to convert form files also in case of an exception.
      // Unit name replacements etc. are implemented there.
      Result:=ConvertAllFormFiles(ConvUnits);
      // Finally save the package once more
      if Result=mrOK then
        Result:=PkgBoss.DoSavePackage(LazPackage,[])  // save .lpk file
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

function TConvertDelphiPackage.ContainsFile(const aFileName: string): Boolean;
begin
  Result:=Assigned(LazPackage.FindPkgFile(aFileName, True, False));
end;

function TConvertDelphiPackage.FindDependencyByName(const PackageName: string): TPkgDependency;
begin
  Result:=LazPackage.FindDependencyByName(PackageName);
end;

function TConvertDelphiPackage.FirstDependency: TPkgDependency;
begin
  Result:=LazPackage.FirstRequiredDependency;
end;


end.

