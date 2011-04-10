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
  Classes, SysUtils, LCLProc, Forms, Controls, Dialogs, LResources,
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
  ConvertSettings, ConvCodeTool, MissingUnits, MissingPropertiesDlg, UsedUnits,
  ReplaceNamesUnit;

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
    fConverter: TConvertDelphiPBase;
    fPath: string;
  protected
    procedure Execute; override;
  public
    constructor Create(aConverter: TConvertDelphiPBase; aBasePath: string);
    destructor Destroy; override;
  end;

  { TConvertDelphiUnit }

  TConvertDelphiUnit = class
  private
    // Converter for the project or package this unit belongs to.
    // Nil if converting single unit.
    fOwnerConverter: TConvertDelphiPBase;
    // Original pascal unit's file name, .pas
    fOrigUnitFilename: string;
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
    fSettings: TConvertSettings;
    function GetDfmFileName: string;
    function CopyAndLoadFile: TModalResult;
    function FixLfmFilename(ADfmFilename: string): TModalResult;
    function ConvertUnitFile: TModalResult;
    function ConvertFormFile: TModalResult;
    function AskUnitPathFromUser: TModalResult;
    function FixIncludeFiles: TModalResult;
  protected
  public
    constructor Create(AOwnerConverter: TConvertDelphiPBase; const AFilename: string;
                       aFlags: TConvertUnitFlags);
    destructor Destroy; override;
    // This should be used only when converting a single unit.
    function Convert: TModalResult;
    function CheckFailed(PrevResult: TModalResult): TModalResult;
  public
    property LazFileExt: string read fLazFileExt write fLazFileExt;
  end;

  // Base class for Delphi project and package conversion.

  // Code would be cleaner if TProject and TLazPackage inherited from same class.
  // Now they can't share much code.

  { TConvertDelphiPBase }

  TConvertDelphiPBase = class
  private
    // Either Project or LazPackage. Typecasted to right types in property getter.
    fProjPack: TObject;
    // Original project's or package's file name, .lpi .lpk .dpr .dpk
    fOrigPFilename: string;
    fLazPFilename: string;             // .lpi or .lpk file name
    fDelphiPFilename: string;          // .dpr or .dpk file name
    fLazPSuffix: string;               // '.lpi' or '.lpk'
    fDelphiPSuffix: string;            // '.dpr' or '.dpk'
    fIsConsoleApp: Boolean;
    // Unit search path for project settings.
    fUnitSearchPaths: TStringList;
    // Units found in user defined paths.
    fCachedUnitNames: TStringToStringTree;
    // Map of case incorrect unit name -> real unit name.
    fCachedRealFileNames: TStringToStringTree;
    // The user selected path when searching missing units.
    fPrevSelectedPath: string;
    // Missing units that are commented automatically in all units.
    fAllCommentedUnits: TStringList;
    // Units that are found and will be added to project and converted.
    fUnitsToAddToProject: TStringList;
    fSettings: TConvertSettings;
    function ConvertSub: TModalResult;
    procedure CleanUpCompilerOptionsSearchPaths(Options: TBaseCompilerOptions);
    procedure SetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
    procedure UnsetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
    function ConvertAllFormFiles(ConverterList: TObjectList): TModalResult;
    function ReadDelphiConfigFiles: TModalResult;
    function ExtractOptionsFromDOF(const DOFFilename: string): TModalResult;
    function ExtractOptionsFromCFG(const CFGFilename: string): TModalResult;
    function DoMissingUnits(AUsedUnitsTool: TUsedUnitsTool): integer;
    procedure CacheUnitsInPath(const APath, ABasePath: string);
    procedure CacheUnitsInPath(const APath: string);
    function GetCachedUnitPath(const AUnitName: string): string;
  protected
    function CreateInstance: TModalResult; virtual; abstract;
    function CreateMainSourceFile: TModalResult; virtual;
    function ScanMainSourceFile: TModalResult; virtual;
    function ConvertMainSourceFile: TModalResult; virtual;
    function FindAllUnits: TModalResult; virtual; abstract;
    function ConvertAllUnits: TModalResult; virtual; abstract;
    function ExtractOptionsFromDelphiSource: TModalResult; virtual; abstract;
    // The following protected funcs are needed only because Project and LazPackage
    //  don't inherit from the same class.
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

  // Delphi project conversion.

  { TConvertDelphiProject }

  TConvertDelphiProject = class(TConvertDelphiPBase)
  private
    // Resource code
    fMainUnitConverter: TConvertDelphiUnit;
    function AddUnit(AUnitName: string; out OutUnitInfo: TUnitInfo): TModalResult;
    function GetLazProject: TProject;
    procedure SetLazProject(const AValue: TProject);
  protected
    function CreateInstance: TModalResult; override;
    function CreateMainSourceFile: TModalResult; override;
    function ScanMainSourceFile: TModalResult; override;
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

  // Delphi package conversion.

  { TConvertDelphiPackage }

  TConvertDelphiPackage = class(TConvertDelphiPBase)
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

  function CheckDelphiFileExt(const Filename: string): TModalResult;
  function CheckFilenameForLCLPaths(const Filename: string): TModalResult;

  // projects
  function CheckDelphiProjectExt(const Filename: string): TModalResult;

  // file names / search paths
  function ConvertDelphiAbsoluteToRelativeFile(const Filename: string;
                                               AProject: TProject): string;
  function ExpandDelphiFilename(const Filename: string; AProject: TProject): string;
  function ExpandDelphiSearchPath(const SearchPath: string;
                                  AProject: TProject): string;


implementation

function CheckDelphiFileExt(const Filename: string): TModalResult;
begin
  Result:=mrOk;
  if CompareFileExt(Filename,'.pas',false)<>0 then
    Result:=QuestionDlg(lisNotADelphiUnit,
        Format(lisTheFileIsNotADelphiUnit, ['"', Filename, '"']),
        mtError, [mrCancel,lisConvDelphiSkipThisFile,mbAbort,lisInfoBuildMakeAbort], 0);
end;

function CheckFilenameForLCLPaths(const Filename: string): TModalResult;
// check if the unitpath of the directory of filename contains the path to the LCL
var
  Directory: String;
  UnitPath: String;
  LazarusSrcDir: string;
  LCLPath: String;
  NextStartPos: Integer;
begin
  // get directory of filename
  Directory:=ExtractFilePath(Filename);
  // get unitpath definition of directory
  UnitPath:=CodeToolBoss.GetUnitPathForDirectory(Directory);
  // get lazarus source directory
  LazarusSrcDir:=CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'LazarusDir'];
  // create base path to LCL compiled units <LazarusSrcDir>/lcl/units/
  LCLPath:=TrimFilename(LazarusSrcDir+SetDirSeparators('/lcl/units/'));
  NextStartPos:=1;
  if GetNextUsedDirectoryInSearchPath(UnitPath,LCLPath,NextStartPos)='' then
  begin
    LCLPath:=LCLPath+'$(TargetCPU)-$(TargetOS)';
    Result:=QuestionDlg(lisLCLUnitPathMissing,
      Format(lisTheCurrentUnitPathForTheFileIsThePathToTheLCLUnits,
            [sLineBreak,'"',Filename,'"',sLineBreak,'"',UnitPath,'"',sLineBreak,
             sLineBreak,'"',LCLPath,'"',sLineBreak,sLineBreak,sLineBreak]),
      mtError, [mrOK, lisContinue, mrAbort, lisInfoBuildMakeAbort], 0);
    exit;
  end;
  Result:=mrOk;
end;

function CheckDelphiProjectExt(const Filename: string): TModalResult;
begin
  if CompareFileExt(Filename,'.dpr',false)<>0 then begin
    Result:=QuestionDlg(lisNotADelphiProject,
      Format(lisTheFileIsNotADelphiProjectDpr, ['"', Filename, '"']),
      mtError, [mrCancel, lisConvDelphiSkipThisStep, mbAbort], 0);
    exit;
  end;
  Result:=mrOk;
end;

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


{ TCacheUnitsThread }

constructor TCacheUnitsThread.Create(aConverter: TConvertDelphiPBase; aBasePath: string);
begin
  inherited Create(True);
  fConverter:=aConverter;
  fPath:=TrimFilename(aBasePath+'../'); // Will scan one level up from base path.
end;

destructor TCacheUnitsThread.Destroy;
begin
  inherited Destroy;
end;

procedure TCacheUnitsThread.Execute;
// This assumes that cache is not used while updating it.
// The main GUI thread must wait for this thread before starting conversion.
begin
  fConverter.CacheUnitsInPath(fPath);  // Scan for unit files.
end;

{ TConvertDelphiUnit }

constructor TConvertDelphiUnit.Create(AOwnerConverter: TConvertDelphiPBase;
                            const AFilename: string; AFlags: TConvertUnitFlags);
begin
  fOwnerConverter:=AOwnerConverter;
  fOrigUnitFilename:=AFilename;
  fFlags:=AFlags;
  fLazFileExt:='';
  if fOwnerConverter=nil then begin
    fSettings:=TConvertSettings.Create(lisConvDelphiConvertDelphiUnit);
    fSettings.MainFilename:=fOrigUnitFilename;
  end
  else
    fSettings:=fOwnerConverter.fSettings;
  fUnitInfo:=nil;
  if not LazarusIDE.BeginCodeTools then
    IDEMessagesWindow.AddMsg(lisConvDelphiBeginCodeToolsFailed, '', -1);
  fCTLink:=Nil;                     // Will be created later.
end;

destructor TConvertDelphiUnit.Destroy;
begin
  fCTLink.Free;
  if fOwnerConverter=nil then
    fSettings.Free;
  inherited Destroy;
end;

function TConvertDelphiUnit.Convert: TModalResult;
begin
  IDEMessagesWindow.Clear;
  Result:=fSettings.RunForm;
  if Result=mrOK then begin
    Result:=CopyAndLoadFile;
    if Result=mrOK then begin
      Result:=ConvertUnitFile;
      if Result=mrOk then begin
        Result:=SaveCodeBufferToFile(fPascalBuffer,fLazUnitFilename);
        if Result=mrOk then begin
          Result:=LazarusIDE.DoOpenEditorFile(fLazUnitFilename,0,0,
                                              [ofAddToRecent,ofQuiet]);
          if Result=mrOk then begin
            Result:=ConvertFormFile;
          end;
        end;
      end;
    end;
  end;
  if Result=mrOk then
    IDEMessagesWindow.AddMsg(lisConvDelphiReady, '', -1)
  else
    IDEMessagesWindow.AddMsg(lisConvDelphiAborted, '', -1)
end;

function TConvertDelphiUnit.GetDfmFileName: string;
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

function TConvertDelphiUnit.CopyAndLoadFile: TModalResult;
begin
  // Convert in place. File must be writable.
  Result:=CheckFileIsWritable(fOrigUnitFilename,[mbAbort]);
  if Result<>mrOk then exit;
  // close Delphi unit file in editor.
  Result:=LazarusIDE.DoCloseEditorFile(fOrigUnitFilename,[cfSaveFirst]);
  if Result<>mrOk then exit;
  // Copy/rename fLazUnitFilename based on fOrigUnitFilename.
  Result:=fSettings.RenameDelphiToLazFile(fOrigUnitFilename, fLazFileExt,
                              fLazUnitFilename, cdtlufRenameLowercase in fFlags);
  if Result<>mrOK then exit;
  // Read the code in.
  fPascalBuffer:=nil;
  Result:=LoadCodeBuffer(fPascalBuffer,fLazUnitFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk],true);
  // Create a shared link for codetools.
  Assert(fCTLink=Nil, 'fCTLink should be Nil in CopyAndLoadFile');
  fCTLink:=TCodeToolLink.Create(fPascalBuffer);
  fCTLink.Settings:=fSettings;
end;

function TConvertDelphiUnit.FixLfmFilename(ADfmFilename: string): TModalResult;
var
  LfmFilename: string;     // Lazarus .LFM file name.
begin
  Result:=mrOK;
  fLFMBuffer:=nil;
  if ADfmFilename<>'' then begin
    Result:=LazarusIDE.DoCloseEditorFile(ADfmFilename,[cfSaveFirst]);
    if Result<>mrOk then exit;
  end;
  if fSettings.Target=ctLazarusDelphiSameDfm then
    LfmFilename:=ADfmFilename
  else begin
    // Create a form file name based on the unit file name.
    LfmFilename:=fSettings.DelphiToLazFilename(fOrigUnitFilename, '.lfm',
                                               cdtlufRenameLowercase in fFlags);
    if ADfmFilename<>'' then begin
      if FileExistsUTF8(LfmFilename) then
        if (FileAgeUTF8(LfmFilename)<FileAgeUTF8(ADfmFilename)) then
          DeleteFileUTF8(LfmFilename); // .lfm is older than .dfm -> remove .lfm
      if not FileExistsUTF8(LfmFilename) then begin
        // TODO: update project
        if fSettings.Target=ctLazarusDelphi then
          Result:=CopyFileWithErrorDialogs(ADfmFilename,LfmFilename,[mbAbort])
        else
          Result:=fSettings.RenameFile(ADfmFilename,LfmFilename);
        if Result<>mrOK then exit;
      end;
    end;
  end;
  // convert .dfm file to .lfm file (without context type checking)
  if FileExistsUTF8(LfmFilename) then begin
    Result:=ConvertDfmToLfm(LfmFilename);
    if Result<>mrOk then exit;
    // Read form file code in.
    if fSettings.Target<>ctLazarusDelphiSameDfm then begin
      Result:=LoadCodeBuffer(fLFMBuffer,LfmFilename,
                             [lbfCheckIfText,lbfUpdateFromDisk],true);
    end;
  end;
end;

function TConvertDelphiUnit.ConvertUnitFile: TModalResult;

  function ReduceMissingUnits: TModalResult;
  // Find or comment out some / all of missing units.
  begin
    Result:=mrOK;
    if Assigned(fOwnerConverter) then begin
      // Try to find from subdirectories scanned earlier.
      if fOwnerConverter.DoMissingUnits(fUsedUnitsTool)=0 then exit;
      // Comment out automatically units that were commented in other files.
      fUsedUnitsTool.MainUsedUnits.CommentAutomatic(fOwnerConverter.fAllCommentedUnits);
      fUsedUnitsTool.ImplUsedUnits.CommentAutomatic(fOwnerConverter.fAllCommentedUnits);
      if fUsedUnitsTool.MissingUnitCount=0 then exit;
    end;
    // Interactive dialog for searching unit.
    Result:=AskUnitPathFromUser;
  end;

var
  DfmFilename: string;     // Delphi .DFM file name.
  ConvTool: TConvDelphiCodeTool;
begin
  IDEMessagesWindow.AddMsg(Format(lisConvDelphiConvertingUnitFile,
                                  [fOrigUnitFilename]), '', -1);
  Application.ProcessMessages;
  // Get DFM file name and close it in editor.
  DfmFilename:=GetDfmFileName;
  Result:=FixLfmFilename(DfmFilename);
  if Result<>mrOk then exit;
  // Check LCL path for single files. They are correct when converting projects.
{  if not Assigned(fOwnerConverter) then begin
    Result:=CheckFilenameForLCLPaths(fLazUnitFilename);
    if Result<>mrOk then exit;
  end;  }
  // Fix include file names.
  Result:=FixIncludeFiles;
  if Result<>mrOk then exit;
  fCTLink.IsConsoleApp:=False;
  fCTLink.IsMainFile:=False;
  fCTLink.AskAboutError:=False;
  if Assigned(fOwnerConverter) then begin
    fCTLink.IsConsoleApp:= fOwnerConverter.fIsConsoleApp;
    fCTLink.IsMainFile:=fOwnerConverter.MainName=fLazUnitFilename;
    fCTLink.AskAboutError:=True;
  end;
  // Take care of missing units in uses sections.
  fUsedUnitsTool:=Nil;
  try
    if fSettings.UnitsReplaceMode<>rlDisabled then begin
      fUsedUnitsTool:=TUsedUnitsTool.Create(fCTLink, fOrigUnitFilename);
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
      ConvTool.LowerCaseRes:=FileExistsUTF8(ChangeFileExt(fLazUnitFilename, '.res'));
      ConvTool.HasFormFile:=DfmFilename<>'';
      Result:=ConvTool.Convert;
      if Result<>mrOk then exit;
    finally
      ConvTool.Free;
    end;
    // Fix or comment missing units, show error messages.
    if fSettings.UnitsReplaceMode<>rlDisabled then
      Result:=fUsedUnitsTool.Convert;
  finally
    FreeAndNil(fUsedUnitsTool);
  end;
end;

function TConvertDelphiUnit.ConvertFormFile: TModalResult;
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
      LfmFixer.Settings:=fSettings;
      LfmFixer.RootMustBeClassInUnit:=true;
      LfmFixer.RootMustBeClassInIntf:=true;
      LfmFixer.ObjectsMustExists:=true;
      if LfmFixer.Repair<>mrOk then begin
        LazarusIDE.DoJumpToCompilerMessage(-1,true);
        exit(mrAbort);
      end;
    finally
      LfmFixer.Free;
    end;
    // save LFM file
    Result:=SaveCodeBufferToFile(fLFMBuffer,fLFMBuffer.Filename);
    if Result<>mrOk then exit;
  end;
  Result:=mrOk;
end;

function TConvertDelphiUnit.AskUnitPathFromUser: TModalResult;
// Ask the user what to do with missing units.
var
  TryAgain: Boolean;
  UnitDirDialog: TSelectDirectoryDialog;
begin
  with fUsedUnitsTool do
  repeat
    TryAgain:=False;
    Result:=AskMissingUnits(MainUsedUnits.MissingUnits, ImplUsedUnits.MissingUnits,
                            ExtractFileName(fLazUnitFilename),
                    fSettings.Target in [ctLazarusDelphi, ctLazarusDelphiSameDfm]);
    case Result of
      // mrOK means: comment out.
      mrOK: begin
        if Assigned(fOwnerConverter) then
          MoveMissingToComment(fOwnerConverter.fAllCommentedUnits)
        else
          MoveMissingToComment(Nil);
      end;
      // mrYes means: Search for unit path.
      mrYes: begin
        UnitDirDialog:=TSelectDirectoryDialog.Create(nil);
        try
          UnitDirDialog.InitialDir:=fOwnerConverter.fPrevSelectedPath;
          UnitDirDialog.Title:=
            lisConvDelphiAllSubDirectoriesWillBeScannedForUnitFiles;
          if UnitDirDialog.Execute then begin
            if Assigned(fOwnerConverter) then begin
              fOwnerConverter.fPrevSelectedPath:=ExtractFilePath(UnitDirDialog.Filename);
              // Add the new path to project if missing units are found.
              fOwnerConverter.CacheUnitsInPath(UnitDirDialog.Filename);
              TryAgain:=fOwnerConverter.DoMissingUnits(fUsedUnitsTool)>0;
            end;
          end
          else
            TryAgain:=true;  // User canceled. Stay with the same unit.
        finally
          UnitDirDialog.Free;
        end;
        Result:=mrOK;        // Caller will check for Result<>mrOK
      end;
      // User wants to abort.
      mrAbort: Exit;
    end;
  until not TryAgain;
end;

function TConvertDelphiUnit.FixIncludeFiles: TModalResult;
// fix include filenames
var
  MissingIncludeFilesCodeXYPos: TFPList;
  CodePos: PCodeXYPosition;
  Msg: string;
  i: Integer;
  OldChange: Boolean;
begin
  Result:=mrOk;
  OldChange:=LazarusIDE.OpenEditorsOnCodeToolChange;
  LazarusIDE.OpenEditorsOnCodeToolChange:=False;
  try
    if not CodeToolBoss.FixIncludeFilenames(fPascalBuffer,true,MissingIncludeFilesCodeXYPos)
    then begin
      if MissingIncludeFilesCodeXYPos<>nil then begin
        for i:=0 to MissingIncludeFilesCodeXYPos.Count-1 do begin
          CodePos:=PCodeXYPosition(MissingIncludeFilesCodeXYPos[i]);
          Msg:=Format(lisConvDelphiMissingIncludeFile,
                 [CodePos^.Code.Filename,IntToStr(CodePos^.y),IntToStr(CodePos^.x)]);
          IDEMessagesWindow.AddMsg(Msg, '', -1);
        end;
      end;
      IDEMessagesWindow.AddMsg(Format(lisConvDelphiError,
                                      [CodeToolBoss.ErrorMessage]), '', -1);
      Application.ProcessMessages;
      Result:=mrCancel;
      exit;
    end;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(MissingIncludeFilesCodeXYPos);
    LazarusIDE.OpenEditorsOnCodeToolChange:=OldChange;
  end;
end;

function TConvertDelphiUnit.CheckFailed(PrevResult: TModalResult): TModalResult;
begin
  Result:=PrevResult;
  if Result=mrCancel then begin
    Result:=QuestionDlg(lisConvDelphiFailedConvertingUnit,
        Format(lisConvDelphiFailedToConvertUnit, [#13,fOrigUnitFilename, #13]),
        mtWarning, [mrIgnore, lisIgnoreAndContinue, mrAbort], 0);
    if Result=mrIgnore then
      Result:=mrOK;
  end;
end;


{ TConvertDelphiPBase }

constructor TConvertDelphiPBase.Create(const AFilename, ADescription: string);
begin
  fOrigPFilename:=AFilename;
  fIsConsoleApp:=False;                      // Default = GUI app.
  fUnitSearchPaths:=TStringList.Create;
  fUnitSearchPaths.Delimiter:=';';
  fUnitSearchPaths.StrictDelimiter:=True;
  fCachedUnitNames:=TStringToStringTree.Create(false);
  fCachedRealFileNames:=TStringToStringTree.Create(true);
  fSettings:=TConvertSettings.Create(ADescription);
  fSettings.MainFilename:=fOrigPFilename;
  fAllCommentedUnits:=TStringList.Create;
  fAllCommentedUnits.Sorted:=true;
  fUnitsToAddToProject:=TStringList.Create;
  fPrevSelectedPath:=fSettings.MainPath;
end;

destructor TConvertDelphiPBase.Destroy;
begin
  fUnitsToAddToProject.Free;
  fAllCommentedUnits.Free;
  fSettings.Free;
  fCachedRealFileNames.Free;
  fCachedUnitNames.Free;
  fUnitSearchPaths.Free;
  inherited Destroy;
end;

function TConvertDelphiPBase.Convert: TModalResult;
// Creates or updates a lazarus project (.lpi+.lpr) or package.
var
  // The initial unit name cache is done in a thread so that GUI shows at once.
  CacheUnitsThread: TCacheUnitsThread;
begin
  IDEMessagesWindow.Clear;
  // Start scanning unit files one level above project path. The GUI will appear
  // without delay but then we must wait for the thread before continuing.
  CacheUnitsThread:=TCacheUnitsThread.Create(Self, fSettings.MainPath);
  try
    CacheUnitsThread.Resume;
    Result:=fSettings.RunForm;      // Get settings from user.
    Screen.Cursor:=crHourGlass;
    try
      CacheUnitsThread.WaitFor;     // Make sure the thread has finished.
    finally
      Screen.Cursor:=crDefault;
    end;
    if Result=mrOK then begin
      // create/open lazarus project or package file
      fLazPFilename:=fSettings.DelphiToLazFilename(fOrigPFilename, fLazPSuffix, false);

      // Find Delphi project / package file name
      if CompareFileExt(fOrigPFilename,fDelphiPSuffix,false)=0 then
        fDelphiPFilename:=fOrigPFilename
      else
        fDelphiPFilename:=ChangeFileExt(fOrigPFilename,fDelphiPSuffix);
      if not FileExistsUTF8(fDelphiPFilename) then
        fDelphiPFilename:=FindDiskFileCaseInsensitive(fOrigPFilename);
// ? fDelphiPFilename:=CodeToolBoss.DirectoryCachePool.FindDiskFilename(fDelphiPFilename);

      // Actual conversion.
      Result:=ConvertSub;
    end;
    if Result=mrOk then
      IDEMessagesWindow.AddMsg(lisConvDelphiConversionReady, '', -1)
    else
      IDEMessagesWindow.AddMsg(lisConvDelphiConversionAborted, '', -1)
  finally
    CacheUnitsThread.Free;
  end;
end;

function TConvertDelphiPBase.ConvertSub: TModalResult;
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
  // Scan LPR file for directives. Sets fIsConsoleApp flag.
  Result:=ScanMainSourceFile;
  if Result<>mrOK then exit;
  // LCL dependency is added automatically later for GUI applications.
//  AddPackageDependency('LCL');
  // ToDo: make an option to add NoGUI to Project.CompilerOptions.LCLWidgetType.
  if fProjPack is TProject then
    PkgBoss.AddDefaultDependencies(fProjPack as TProject);
  CustomDefinesChanged;

  SetCompilerModeForDefineTempl(CustomDefines);
  try
    Result:=ConvertMainSourceFile;     // Convert project's LPR file.
    if Result<>mrOK then exit;
    // get all options from the .dpr or .dpk file
    Result:=ExtractOptionsFromDelphiSource;
    if Result<>mrOK then exit;
    Result:=FindAllUnits;              // find all files and save the project.
    if Result<>mrOK then exit;
    Result:=ConvertAllUnits;           // convert all files.
  finally
    UnsetCompilerModeForDefineTempl(CustomDefines);
  end;
end;

function TConvertDelphiPBase.ConvertAllFormFiles(ConverterList: TObjectList): TModalResult;
// Call Convert.ConvertFormFile for all units.
var
  Converter: TConvertDelphiUnit;
  i: Integer;
begin
  IDEMessagesWindow.AddMsg(lisConvDelphiRepairingFormFiles, '', -1);
  Application.ProcessMessages;
  Screen.Cursor:=crHourGlass;
  try
    for i:=0 to ConverterList.Count-1 do begin
      Converter:=TConvertDelphiUnit(ConverterList[i]); // Converter created in cycle1.
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

function TConvertDelphiPBase.ReadDelphiConfigFiles: TModalResult;
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

function TConvertDelphiPBase.ExtractOptionsFromDOF(const DOFFilename: string): TModalResult;
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
                  ';'+lowercase(DelphiPkgNames)+';')>0 then
      AddPackageDependency(LazarusPkgName);
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

function TConvertDelphiPBase.ExtractOptionsFromCFG(const CFGFilename: string): TModalResult;
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

procedure TConvertDelphiPBase.SetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
begin
  if DefTempl.FindChildByName(SettingDelphiModeTemplName)<>nil then exit;
  DefTempl.ReplaceChild(CreateDefinesForFPCMode(SettingDelphiModeTemplName,cmDELPHI));
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TConvertDelphiPBase.UnsetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
begin
  if DefTempl.FindChildByName(SettingDelphiModeTemplName)=nil then exit;
  DefTempl.DeleteChild(SettingDelphiModeTemplName);
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TConvertDelphiPBase.CleanUpCompilerOptionsSearchPaths(Options: TBaseCompilerOptions);
var
  BasePath: String;

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
end;

function TConvertDelphiPBase.DoMissingUnits(AUsedUnitsTool: TUsedUnitsTool): integer;
// Locate unit names from earlier cached list or from packages.
// Return the number of units still missing.

  procedure DoMissingSub(AUsedUnits: TUsedUnits);
  var
    Pack: TPkgFile;
    Dep: TPkgDependency;
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
          AUsedUnits.UnitsToRename[mUnit]:=RealUnitName;
        // Will be added later to project.
        fUnitsToAddToProject.Add(sUnitPath+RealFileName);
        AUsedUnits.MissingUnits.Delete(i);      // No more missing, delete from list.
      end
      else begin
        Pack:=PackageGraph.FindUnitInAllPackages(mUnit, True);
        if Assigned(Pack) then begin
          // Found from package: add package to project dependencies and open it.
          AddPackageDependency(Pack.LazPackage.Name);
          Dep:=FindDependencyByName(Pack.LazPackage.Name);
          if Assigned(Dep) then
            PackageGraph.OpenDependency(Dep,false);
          AUsedUnits.MissingUnits.Delete(i);
        end;
      end;
    end;
  end;

begin
  DoMissingSub(AUsedUnitsTool.MainUsedUnits);
  DoMissingSub(AUsedUnitsTool.ImplUsedUnits);
  Result:=AUsedUnitsTool.MissingUnitCount;
end;

procedure TConvertDelphiPBase.CacheUnitsInPath(const APath, ABasePath: string);
// Search all pascal units in APath and store them in fCachedUnitNames
//  with a path relative to ABasePath.
var
  PasFileList: TStringList;
  i: Integer;
  PasFile, RelPath, SubPath, sUnitName, FileName: String;
begin
  PasFileList:=FindAllFiles(APath,'*.pas',true);
  try
    for i:=0 to PasFileList.Count-1 do begin
      PasFile:=PasFileList[i];
      RelPath:=FileUtil.CreateRelativePath(PasFile, ABasePath);
      SubPath:=ExtractFilePath(RelPath);
      FileName:=ExtractFileName(RelPath);
      sUnitName:=ExtractFileNameOnly(FileName);
      if (SubPath<>'') and (sUnitName<>'') then begin
        // Map path by unit name.
        fCachedUnitNames[sUnitName]:=SubPath;
        // Map real unit name by uppercase unit name.
        fCachedRealFileNames[UpperCase(sUnitName)]:=FileName;
      end;
    end;
  finally
    PasFileList.Free;
  end;
end;

procedure TConvertDelphiPBase.CacheUnitsInPath(const APath: string);
// Same as above but uses fSettings.MainPath as base path.
begin
  CacheUnitsInPath(APath, fSettings.MainPath);
end;

function TConvertDelphiPBase.GetCachedUnitPath(const AUnitName: string): string;
begin
  Result:=fCachedUnitNames[AUnitName];
end;

function TConvertDelphiPBase.CreateMainSourceFile: TModalResult;
begin
  Result:=mrOK; // Do nothing. Overridden in project.
end;

function TConvertDelphiPBase.ScanMainSourceFile: TModalResult;
begin
  Result:=mrOK; // Do nothing. Overridden in project.
end;

function TConvertDelphiPBase.ConvertMainSourceFile: TModalResult;
begin
  Result:=mrOK; // Do nothing. Overridden in project.
end;

function TConvertDelphiPBase.SaveAndMaybeClose(aFilename: string): TModalResult;
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
  if Assigned(fMainUnitConverter) then
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
begin
  // Converter for main LPR file.
  fMainUnitConverter:=TConvertDelphiUnit.Create(Self,fOrigPFilename,[]);
  if fSettings.Target in [ctLazarusDelphi, ctLazarusDelphiSameDfm] then
    fMainUnitConverter.LazFileExt:=ExtractFileExt(fOrigPFilename)
  else
    fMainUnitConverter.LazFileExt:='.lpr';
  fMainUnitConverter.CopyAndLoadFile;
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
  Result:=LazarusIDE.DoOpenEditorFile(fMainUnitConverter.fLazUnitFilename,0,0,[ofQuiet]);
  if Result<>mrOK then exit;
  Result:=mrOk;
end;

function TConvertDelphiProject.ScanMainSourceFile: TModalResult;
var
  CTLink: TCodeToolLink;
  ConvTool: TConvDelphiCodeTool;
begin
  Result:=mrOK;
  CTLink:=TCodeToolLink.Create(fMainUnitConverter.fPascalBuffer);
  ConvTool:=TConvDelphiCodeTool.Create(CTLink);
  try
    fIsConsoleApp:=ConvTool.FindApptypeConsole;
  finally
    ConvTool.Free;
    CTLink.Free;
  end;
end;

function TConvertDelphiProject.ConvertMainSourceFile: TModalResult;
begin
  // Loading was done earlier. Now just convert.
  Result:=fMainUnitConverter.ConvertUnitFile;
  if Result<>mrOk then exit;
  Result:=fMainUnitConverter.ConvertFormFile;
end;

function TConvertDelphiProject.AddUnit(AUnitName: string;
                                       out OutUnitInfo: TUnitInfo): TModalResult;
// add new unit to project
var
  CurUnitInfo: TUnitInfo;
  RP: String;
begin
  Result:=mrOK;
  OutUnitInfo:=nil;
  if not FilenameIsAbsolute(AUnitName) then
    AUnitName:=AppendPathDelim(LazProject.ProjectDirectory)+AUnitName;
  AUnitName:=TrimFilename(AUnitName);
  if not FileExistsUTF8(AUnitName) then
    exit(mrNo);
  // Create relative search path for project settings.
  RP:=ExtractFilePath(CreateRelativePath(AUnitName, LazProject.ProjectDirectory));
  if (RP<>'') and (fUnitSearchPaths.IndexOf(RP)<0) then
    fUnitSearchPaths.Add(RP);
  // Check unitname and create UnitInfo.
  CurUnitInfo:=LazProject.UnitInfoWithFilename(AUnitName);
  if CurUnitInfo=nil then begin
    if FilenameIsPascalUnit(AUnitName) then begin
      CurUnitInfo:=LazProject.UnitWithUnitname(ExtractFileNameOnly(AUnitName));
      if CurUnitInfo<>nil then begin
        Result:=QuestionDlg(lisConvDelphiUnitnameExistsTwice,
          Format(lisConvDelphiThereAreTwoUnitsWithTheSameUnitname,
                 [#13, CurUnitInfo.Filename, #13, AUnitName, #13]),
          mtWarning, [mrYes, lisConvDelphiRemoveFirst, mrNo,
            lisConvDelphiRemoveSecond,
                     mrIgnore, lisConvDelphiKeepBoth, mrAbort], 0);
        case Result of
          mrYes: CurUnitInfo.IsPartOfProject:=false;
          mrNo:  exit(mrNo);
          mrIgnore: ;
        else
          exit(mrAbort);
        end;
      end;
    end;
    CurUnitInfo:=TUnitInfo.Create(nil);
    CurUnitInfo.Filename:=AUnitName;
    LazProject.AddFile(CurUnitInfo,false);
  end;
  CurUnitInfo.IsPartOfProject:=true;
  OutUnitInfo:=CurUnitInfo;
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
    IDEMessagesWindow.AddMsg(lisConvDelphiFindAllUnitFiles, '', -1);
    Application.ProcessMessages;
    if not CodeToolBoss.FindDelphiProjectUnits(fMainUnitConverter.fPascalBuffer,
                                         FoundUnits, MisUnits, NormalUnits) then
    begin
      LazarusIDE.DoJumpToCodeToolBossError;
      Result:=mrCancel;
      exit;
    end;
    try
      // add all units to the project
      for i:=0 to FoundUnits.Count-1 do begin
        CurFilename:=FoundUnits[i];
        p:=System.Pos(' in ',CurFilename);
        if p>0 then
          CurFilename:=copy(CurFilename,p+4,length(CurFilename));
        if CurFilename='' then continue;
        Result:=AddUnit(SwitchPathDelims(CurFilename, True), ui);
        if Result=mrAbort then exit;
      end;
    finally
      AllPath:=fUnitSearchPaths.DelimitedText;
      // set unit paths to find all project units
      LazProject.CompilerOptions.OtherUnitFiles:=
          MergeSearchPaths(LazProject.CompilerOptions.OtherUnitFiles,AllPath);
      // set include path
      LazProject.CompilerOptions.IncludePath:=
          MergeSearchPaths(LazProject.CompilerOptions.IncludePath,AllPath);
      // clear caches
      LazProject.DefineTemplates.SourceDirectoriesChanged;
    end;
    // save project
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
    Converter: TConvertDelphiUnit;
  begin
    Converter:=TConvertDelphiUnit.Create(Self, AUnitInfo.Filename,[]);
    Converter.fUnitInfo:=AUnitInfo;
    ConvUnits.Add(Converter);
    Result:=Converter.CopyAndLoadFile;
    Result:=Converter.CheckFailed(Result);
    if Result<>mrOK then exit;
    Result:=Converter.ConvertUnitFile;
  end;

var
  CurUnitInfo: TUnitInfo;
  i: Integer;
begin
  Result:=mrOk;
  ConvUnits:=TObjectList.create;
  try
    // convert all units and fix .lfm files
    IDEMessagesWindow.AddMsg(lisConvDelphiConvertingUnitFiles, '', -1);
    Application.ProcessMessages;
    for i:=0 to LazProject.UnitCount-1 do begin
      CurUnitInfo:=LazProject.Units[i];
      // Main LPR file was converted earlier.
      if CurUnitInfo.IsPartOfProject and (CurUnitInfo<>LazProject.MainUnitInfo) then begin
        Result:=ConvertOne(CurUnitInfo);
        if Result<>mrOK then Break;
      end;
    end;
    // During conversion there were more units added to be converted.
    for i:=0 to fUnitsToAddToProject.Count-1 do begin
      Result:=AddUnit(fUnitsToAddToProject[i], CurUnitInfo);
      if Result=mrNo then continue;
      if Result=mrAbort then Break;
      Result:=ConvertOne(CurUnitInfo);
      if Result<>mrOK then Break;
    end;
    if Result=mrOK then
      Result:=ConvertAllFormFiles(ConvUnits);
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
    Result:=PackageEditingInterface.DoOpenPackageFile(fLazPFilename,
                                                      [pofAddToRecent],true);
    if Result<>mrOk then exit;
  end;
  // search package in graph
  PkgName:=ExtractFileNameOnly(fLazPFilename);
  LazPackage:=PackageGraph.FindAPackageWithName(PkgName,nil);
  if LazPackage<>nil then begin
    // there is already a package loaded with this name ...
    if CompareFilenames(LazPackage.Filename,fLazPFilename)<>0 then begin
      // ... but it is not the package file we want -> stop
      MessageDlg(lisConvDelphiPackageNameExists,
        Format(lisConvDelphiThereIsAlreadyAPackageWithTheNamePleaseCloseThisPa,
               [PkgName, #13]), mtError, [mbAbort], 0);
      PackageEditingInterface.DoOpenPackageFile(LazPackage.Filename,
                                                        [pofAddToRecent],true);
      Result:=mrAbort;
      exit;
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

function TConvertDelphiPackage.ConvertAllUnits: TModalResult;
var
  i: Integer;
  PkgFile: TPkgFile;
  Converter: TConvertDelphiUnit;
  ConvUnits: TObjectList;       // List of ConvertDelphiUnits.
begin
  Result:=mrOk;
  ConvUnits:=TObjectList.create;
  try
    // convert all units and fix .lfm files
    IDEMessagesWindow.AddMsg(lisConvDelphiConvertingUnitFiles, '', -1);
    Application.ProcessMessages;
    for i:=0 to LazPackage.FileCount-1 do begin
      PkgFile:=LazPackage.Files[i];
      Converter:=TConvertDelphiUnit.Create(Self, PkgFile.Filename,[]);
      ConvUnits.Add(Converter);
      Result:=Converter.CopyAndLoadFile;
      Result:=Converter.CheckFailed(Result);
      if Result<>mrOK then Break;
      Result:=Converter.ConvertUnitFile;
      Result:=Converter.CheckFailed(Result);
      if Result<>mrOK then Break;
    end;
    if Result=mrOK then
      Result:=ConvertAllFormFiles(ConvUnits);
  finally
    ConvUnits.Free;  // Owns and frees converter objects.
  end;
end;

function TConvertDelphiPackage.FindAllUnits: TModalResult;
var
  FoundInUnits, MissingInUnits, NormalUnits: TStrings;
  NotFoundUnits: String;
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
      Result:=mrCancel;
      exit;
    end;
    // warn about missing units
    if (MissingInUnits<>nil) and (MissingInUnits.Count>0) then begin
      NotFoundUnits:=MissingInUnits.Text;
      Result:=QuestionDlg(lisConvDelphiUnitsNotFound,
        Format(lisConvDelphiSomeUnitsOfTheDelphiPackageAreMissing,
               [#13, NotFoundUnits]), mtWarning, [mrIgnore, mrAbort], 0);
      if Result<>mrIgnore then exit;
    end;

    try
      // add all units to the package
      for i:=0 to FoundInUnits.Count-1 do begin
        CurFilename:=FoundInUnits[i];
        p:=System.Pos(' in ',CurFilename);
        if p>0 then
          CurFilename:=copy(CurFilename,p+4,length(CurFilename));
        if CurFilename='' then continue;
        if not FilenameIsAbsolute(CurFilename) then
          CurFilename:=AppendPathDelim(LazPackage.Directory)+CurFilename;
        CurFilename:=TrimFilename(CurFilename);
        if not FileExistsUTF8(CurFilename) then begin
          continue;
        end;
        PkgFile:=LazPackage.FindPkgFile(CurFilename,true,false);
        if PkgFile=nil then begin
          if FilenameIsPascalUnit(CurFilename) then begin
            // check unitname
            OffendingUnit:=LazPackage.FindUnit(ExtractFileNameOnly(CurFilename));
            if OffendingUnit<>nil then begin
              Result:=QuestionDlg(lisConvDelphiUnitnameExistsTwice,
                Format(lisConvDelphiThereAreTwoUnitsWithTheSameUnitname,
                       [#13, OffendingUnit.Filename, #13, CurFilename, #13]),
                mtWarning, [mrNo, lisConvDelphiRemoveSecond, mrAbort], 0);
              case Result of
              mrNo:  continue;
              mrIgnore: ;
              else
                Result:=mrAbort;
                exit;
              end;
            end;
          end;

          // add new unit to package
          LazPackage.AddFile(CurFilename,ExtractFileNameOnly(CurFilename),
                           pftUnit,[pffAddToPkgUsesSection],cpNormal);
        end;
      end;

    finally
      AllPath:=LazPackage.SourceDirectories.CreateSearchPathFromAllFiles;
      UselessPath:='.;'+VirtualDirectory+';'+VirtualTempDir+';'+LazPackage.Directory;
      // set unit paths to find all project units
      NewSearchPath:=MergeSearchPaths(LazPackage.CompilerOptions.OtherUnitFiles,AllPath);
      NewSearchPath:=RemoveSearchPaths(NewSearchPath,UselessPath);
      LazPackage.CompilerOptions.OtherUnitFiles:=MinimizeSearchPath(
               RemoveNonExistingPaths(NewSearchPath,LazPackage.Directory));
      // set include path
      NewSearchPath:=MergeSearchPaths(LazPackage.CompilerOptions.IncludePath,AllPath);
      NewSearchPath:=RemoveSearchPaths(NewSearchPath,UselessPath);
      LazPackage.CompilerOptions.IncludePath:=MinimizeSearchPath(
               RemoveNonExistingPaths(NewSearchPath,LazPackage.Directory));
      // clear caches
      LazPackage.DefineTemplates.SourceDirectoriesChanged;
      CodeToolBoss.DefineTree.ClearCache;
    end;

    // save package
    Result:=PackageEditors.SavePackage(LazPackage,false);
    if Result<>mrOk then exit;
  finally
    FoundInUnits.Free;
    MissingInUnits.Free;
    NormalUnits.Free;
  end;
  Result:=mrOk;
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
             [#13, LazPackage.Filename]), mtError, [mbAbort], 0);
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

