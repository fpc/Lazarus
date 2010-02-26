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
  FileUtil, contnrs, IniFiles,
  // codetools
  CodeToolManager, DefineTemplates, CodeAtom, CodeCache, LinkScanner,
  FileProcs, CodeToolsStructs,
  // IDEIntf
  ComponentReg, IDEMsgIntf, MainIntf, LazIDEIntf, PackageIntf, ProjectIntf,
  // IDE
  IDEProcs, MissingUnits, Project, DialogProcs, CheckLFMDlg,
  EditorOptions, CompilerOptions, PackageDefs, PackageSystem,
  PackageEditor, BasePkgManager, LazarusIDEStrConsts,
  ConvertSettings, ConvCodeTool;

const
  SettingDelphiModeTemplName = 'Setting Delphi Mode';

type

  TConvertUnitFlag = (
    cdtlufRenameLowercase, // rename the unit lowercase
    cdtlufCanAbort   // show 'Cancel all' button in error messages using mrAbort
  );
  TConvertUnitFlags = set of TConvertUnitFlag;

  TConvertDelphiPBase = class;

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
    fUnitCode, fLfmCode: TCodeBuffer;
    fFlags: TConvertUnitFlags;
    // Units not found in project dir or packages.
    fMissingUnits: TStrings;
    // Units to remove.
    fUnitsToRemove: TStringList;
    // Units to rename. Map of unit name -> real unit name.
    fUnitsToRename: TStringToStringTree;
    // Units to add.
    fUnitsToAdd: TStringList;
    // Units collected to be commented later.
    fUnitsToComment: TStringList;

    fSettings: TConvertSettings;
    function GetDfmFileName: string;
    function CopyAndLoadFile: TModalResult;
    function ConvertUnitFile: TModalResult;
    function ConvertFormFile: TModalResult;
    function ConvertDfmToLfm(const LfmFilename: string): TModalResult;
    function MissingUnitToMsg(MissingUnit: string): string;
    function CommentAutomatically: integer;
    function AskUnitPathFromUser: TModalResult;
    function FixIncludeFiles: TModalResult;
    function FixMissingUnits: TModalResult;
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
    // Units found in user defined paths.
    fCachedUnitNames: TStringToStringTree;
    // Map of case incorrect unit name -> real unit name.
    fCachedRealUnitNames: TStringToStringTree;
    // Missing units that are commented automatically in all units.
    fAllMissingUnits: TStringList;
    fSettings: TConvertSettings;
    function ConvertSub: TModalResult;
    procedure CleanUpCompilerOptionsSearchPaths(Options: TBaseCompilerOptions);
    procedure SetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
    procedure UnsetCompilerModeForDefineTempl(DefTempl: TDefineTemplate);
    function ConvertAllFormFiles(ConverterList: TObjectList): TModalResult;
    function ReadDelphiConfigFiles: TModalResult;
    function ExtractOptionsFromDOF(const DOFFilename: string): TModalResult;
    function ExtractOptionsFromCFG(const CFGFilename: string): TModalResult;
    function DoMissingUnits(MissingUnits: TStrings): integer;
    function DoCaseErrorUnits(MissingUnits: TStrings;
                              UnitsToRename: TStringToStringTree): integer;
    procedure CacheUnitsInPath(const APath, ABasePath: string);
    procedure CacheUnitsInPath(const APath: string);
    function GetCachedUnitPath(const AUnitName: string): string;
    function NeedsRenameUnit(const AUnitName: string;
                             out RealUnitName: string): Boolean;
  protected
    function CreateInstance: TModalResult; virtual; abstract;
    function CreateMainSourceFile: TModalResult; virtual;
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
    procedure AddPackageDependency(const PackageName: string); virtual; abstract;
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
    procedure AddPackageDependency(const PackageName: string); override;
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
    procedure RemoveNonExistingFiles(RemoveFromUsesSection: boolean); override;
  public
    constructor Create(const aPackageFilename: string);
    destructor Destroy; override;
  public
    property LazPackage: TLazPackage read GetLazPackage write SetLazPackage;
  end;


  // Some global functions from delphiunit2laz are not (yet) converted to class methods.

  function CheckDelphiFileExt(const Filename: string): TModalResult;
  function CheckFilenameForLCLPaths(const Filename: string): TModalResult;

  // dfm/lfm (ConvertDFMFileToLFMFile is used in main unit, too)
  function ConvertDFMToLFMFilename(const DFMFilename: string; KeepCase: boolean): string;
  function ConvertDFMFileToLFMFile(const DFMFilename: string): TModalResult;

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
  if CompareFileExt(Filename,'.pas',false)<>0 then begin
    Result:=QuestionDlg(lisNotADelphiUnit,
      Format(lisTheFileIsNotADelphiUnit, ['"', Filename, '"']),
      mtError,[mrCancel,'Skip this file',mbAbort,'Abort'],0);
    exit;
  end;
  Result:=mrOk;
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
      Format(lisTheCurrentUnitPathForTheFileIsThePathToTheLCLUnits, [#13, '"',
        Filename, '"', #13, '"', UnitPath, '"', #13, #13, '"', LCLPath, '"',
        #13, #13, #13]),
      mtError,[mrCancel,'Skip this step',mrAbort,'Abort'],0);
    exit;
  end;
  Result:=mrOk;
end;

function ConvertDFMToLFMFilename(const DFMFilename: string;
  KeepCase: boolean): string;
begin
  if DFMFilename<>'' then begin
    // platform and fpc independent unitnames are lowercase, so are the lfm files
    Result:=lowercase(ExtractFilenameOnly(DFMFilename));
    if KeepCase then
      Result:=ExtractFilenameOnly(DFMFilename);
    Result:=ExtractFilePath(DFMFilename)+Result+'.lfm';
  end else
    Result:='';
end;

function ConvertDFMFileToLFMFile(const DFMFilename: string): TModalResult;
var
  DFMStream, LFMStream: TMemoryStream;
  LFMFilename: string;
begin
  Result:=mrOk;
  DFMStream:=TMemoryStream.Create;
  LFMStream:=TMemoryStream.Create;
  try
    try
      DFMStream.LoadFromFile(UTF8ToSys(DFMFilename));
    except
      on E: Exception do begin
        Result:=QuestionDlg(lisCodeToolsDefsReadError, Format(
          lisUnableToReadFileError, ['"', DFMFilename, '"', #13, E.Message]),
          mtError,[mrIgnore,mrAbort],0);
        exit;
      end;
    end;
    try
      FormDataToText(DFMStream,LFMStream);
    except
      on E: Exception do begin
        Result:=QuestionDlg(lisFormatError,
          Format(lisUnableToConvertFileError, ['"', DFMFilename, '"', #13,
            E.Message]),
          mtError,[mrIgnore,mrAbort],0);
        exit;
      end;
    end;
    // converting dfm file, without renaming unit -> keep case
    LFMFilename:=ConvertDFMToLFMFilename(DFMFilename,true);
    try
      LFMStream.SaveToFile(UTF8ToSys(LFMFilename));
    except
      on E: Exception do begin
        Result:=MessageDlg(lisCodeToolsDefsWriteError,
          Format(lisUnableToWriteFileError, ['"', LFMFilename, '"', #13,
            E.Message]),
          mtError,[mbIgnore,mbAbort],0);
        exit;
      end;
    end;
  finally
    LFMSTream.Free;
    DFMStream.Free;
  end;
end;

function CheckDelphiProjectExt(const Filename: string): TModalResult;
begin
  if CompareFileExt(Filename,'.dpr',false)<>0 then begin
    Result:=QuestionDlg(lisNotADelphiProject,
      Format(lisTheFileIsNotADelphiProjectDpr, ['"', Filename, '"']),
      mtError,[mrCancel,'Skipt this step',mbAbort],0);
    exit;
  end;
  Result:=mrOk;
end;

function ConvertDelphiAbsoluteToRelativeFile(const Filename: string;
  AProject: TProject): string;
var
  ProjectDir: String;
  ShortProjectDir: String;
  p: LongInt;
begin
  // often projects use paths near to their project directory
  // For example:
  //   A project /somewhere/MyProjects/project1.dpr
  // and a path C:\Delphi\MyProject\folder
  // can mean, that the relative path is 'folder'

  ProjectDir:=AProject.ProjectDirectory;
  ShortProjectDir:=PathDelim+ExtractFileName(ChompPathDelim(ProjectDir))+PathDelim;
  p:=System.Pos(ShortProjectDir,Filename);
  if (p>0) then begin
    Result:=copy(Filename,p+length(ShortProjectDir),length(Filename));
    exit;
  end;

  // ignore all other absolute paths
  Result:='';
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
    // path macros are not supported
    // -> ignore
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


{ TConvertDelphiUnit }

constructor TConvertDelphiUnit.Create(AOwnerConverter: TConvertDelphiPBase;
                            const AFilename: string; AFlags: TConvertUnitFlags);
begin
  fOwnerConverter:=AOwnerConverter;
  fOrigUnitFilename:=AFilename;
  fFlags:=AFlags;
  fLazFileExt:='';
  if fOwnerConverter=nil then begin
    fSettings:=TConvertSettings.Create('Convert Delphi Unit');
    fSettings.MainFilename:=fOrigUnitFilename;
  end
  else
    fSettings:=fOwnerConverter.fSettings;
  fUnitInfo:=nil;
  if not LazarusIDE.BeginCodeTools then
    IDEMessagesWindow.AddMsg('BeginCodeTools failed!','',-1);
end;

destructor TConvertDelphiUnit.Destroy;
begin
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
      if Result=mrOk then
        Result:=ConvertFormFile;
    end;
  end;
  if Result=mrOk then
    IDEMessagesWindow.AddMsg('Ready.','',-1)
  else
    IDEMessagesWindow.AddMsg('Aborted.','',-1)
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
  Result:=fSettings.RenameDelphiToLazFile(fOrigUnitFilename,fLazFileExt,
                              fLazUnitFilename,cdtlufRenameLowercase in fFlags);
  if Result<>mrOK then exit;

  // Read the code in.
  fUnitCode:=nil;
  Result:=LoadCodeBuffer(fUnitCode,fLazUnitFilename,
                         [lbfCheckIfText,lbfUpdateFromDisk],true);
end;

function TConvertDelphiUnit.ConvertUnitFile: TModalResult;
var
  DfmFilename: string;     // Delphi .DFM file name.
  LfmFilename: string;     // Lazarus .LFM file name.
  ConvTool: TConvDelphiCodeTool;
begin
  fUnitsToRemove:=TStringList.Create;
  fUnitsToRename:=TStringToStringTree.Create(false);
  fUnitsToAdd:=TStringList.Create;
  fUnitsToComment:=TStringList.Create;
  ConvTool:=TConvDelphiCodeTool.Create(fUnitCode);
  try
    fLfmCode:=nil;
    // Get DFM file name and close it in editor.
    DfmFilename:=GetDfmFileName;
    if DfmFilename<>'' then begin
      Result:=LazarusIDE.DoCloseEditorFile(DfmFilename,[cfSaveFirst]);
      if Result<>mrOk then exit;
    end;
    if fSettings.FormFileRename then begin
      // rename files (.pas,.dfm) lowercase. TODO: rename files in project
      LfmFilename:=fSettings.DelphiToLazFilename(fOrigUnitFilename, '.lfm',
                                               cdtlufRenameLowercase in fFlags);
      if DfmFilename<>'' then begin
        if FileExistsUTF8(LfmFilename) then
          if (FileAgeUTF8(LfmFilename)<FileAgeUTF8(DfmFilename)) then
            DeleteFileUTF8(LfmFilename); // .lfm is older than .dfm -> remove .lfm
        if not FileExistsUTF8(LfmFilename) then begin
          // TODO: update project
          Result:=fSettings.RenameFile(DfmFilename,LfmFilename);
          if Result<>mrOK then exit;
        end;
      end;
    end
    else
      LfmFilename:=DfmFilename;
    // convert .dfm file to .lfm file (without context type checking)
    if FileExistsUTF8(LfmFilename) then begin
      IDEMessagesWindow.AddMsg('Converting DFM to LFM file '+LfmFilename,'',-1);
      Result:=ConvertDfmToLfm(LfmFilename);
      if Result<>mrOk then exit;
      // Read form file code in.
      Result:=LoadCodeBuffer(fLfmCode,LfmFilename,
                             [lbfCheckIfText,lbfUpdateFromDisk],true);
      if Result<>mrOk then exit;
    end;

    // check LCL path only for projects/packages.
    if Assigned(fOwnerConverter) then begin
      Result:=CheckFilenameForLCLPaths(fLazUnitFilename);
      if Result<>mrOk then exit;
    end;

    // Fix include file names.
    Result:=FixIncludeFiles;
    if Result<>mrOk then exit;
    // Fix or comment missing units, show error messages.
    Result:=FixMissingUnits;
    if Result<>mrOk then exit;

    // Do the actual code conversion.
    ConvTool.Ask:=Assigned(fOwnerConverter);
    ConvTool.LowerCaseRes:=FileExistsUTF8(ChangeFileExt(fLazUnitFilename, '.res'));
    ConvTool.HasFormFile:=DfmFilename<>'';
    ConvTool.FormFileRename:=fSettings.FormFileRename and (DfmFilename<>'');
    ConvTool.Target:=fSettings.Target;
    ConvTool.UnitsToRemove:=fUnitsToRemove;
    ConvTool.UnitsToRename:=fUnitsToRename;
    ConvTool.UnitsToAdd:=fUnitsToAdd;
    ConvTool.UnitsToComment:=fUnitsToComment;
    Result:=ConvTool.Convert;
  finally
    ConvTool.Free;
    fUnitsToComment.Free;
    fUnitsToAdd.Free;
    fUnitsToRename.Free;
    fUnitsToRemove.Free;
  end;
end;

function TConvertDelphiUnit.ConvertFormFile: TModalResult;
begin
  // check the LFM file and the pascal unit, updates fUnitCode and fLfmCode.
  if fLfmCode<>nil then begin
    if RepairLFMBuffer(fUnitCode,fLfmCode,@IDEMessagesWindow.AddMsg,true,true)<>mrOk
    then begin
      LazarusIDE.DoJumpToCompilerMessage(-1,true);
      exit(mrAbort);
    end;
    // save LFM file
    Result:=SaveCodeBufferToFile(fLfmCode,fLfmCode.Filename);
    if Result<>mrOk then exit;
  end;
  Result:=mrOk;
end;

function TConvertDelphiUnit.ConvertDfmToLfm(const LfmFilename: string): TModalResult;
var
  DFMStream, LFMStream: TMemoryStream;
begin
  Result:=mrOk;
  DFMStream:=TMemoryStream.Create;
  LFMStream:=TMemoryStream.Create;
  try
    // Note: LFM file is copied from DFM file earlier.
    try
      DFMStream.LoadFromFile(UTF8ToSys(LfmFilename));
    except
      on E: Exception do begin
        Result:=QuestionDlg(lisCodeToolsDefsReadError, Format(
          lisUnableToReadFileError, ['"', LfmFilename, '"', #13, E.Message]),
          mtError,[mrIgnore,mrAbort],0);
        if Result=mrIgnore then // The caller will continue like nothing happened.
          Result:=mrOk;
        exit;
      end;
    end;
    try
      FormDataToText(DFMStream,LFMStream);
    except
      on E: Exception do begin
        Result:=QuestionDlg(lisFormatError,
          Format(lisUnableToConvertFileError, ['"',LfmFilename,'"',#13,E.Message]),
          mtError,[mrIgnore,mrAbort],0);
        if Result=mrIgnore then
          Result:=mrOk;
        exit;
      end;
    end;
    // converting dfm file...
    try
      LFMStream.SaveToFile(UTF8ToSys(LfmFilename));
    except
      on E: Exception do begin
        Result:=MessageDlg(lisCodeToolsDefsWriteError,
          Format(lisUnableToWriteFileError, ['"',LfmFilename,'"',#13,E.Message]),
          mtError,[mbIgnore,mbAbort],0);
        if Result=mrIgnore then
          Result:=mrOk;
        exit;
      end;
    end;
  finally
    LFMSTream.Free;
    DFMStream.Free;
  end;
end;

function TConvertDelphiUnit.MissingUnitToMsg(MissingUnit: string): string;
var
  p: Integer;
  NamePos, InPos: Integer;
  Line, Col: Integer;
  ShortFilename: string;
begin
  ShortFilename:=ExtractFileName(fUnitCode.Filename);
  // cut 'in' extension
  p:=System.Pos(' ',MissingUnit);
  if p>0 then
    MissingUnit:=copy(MissingUnit,1,p-1);
  Line:=1;
  Col:=1;
  if CodeToolBoss.FindUnitInAllUsesSections(fUnitCode,MissingUnit,NamePos,InPos)
  then begin
    if InPos=0 then ;
    fUnitCode.AbsoluteToLineCol(NamePos,Line,Col);
  end;
  Result:=ShortFilename+'('+IntToStr(Line)+','+IntToStr(Col)+') Error: '
          +'Can''t find unit '+MissingUnit;
end;

function TConvertDelphiUnit.CommentAutomatically: integer;
// Comment automatically unit names that were commented in other files.
// Return the number of missing units still left.
var
  i, x: Integer;
  s: string;
begin
  for i:=fMissingUnits.Count-1 downto 0 do begin
    s:=fMissingUnits[i];
    if fOwnerConverter.fAllMissingUnits.Find(s, x) then begin
      fUnitsToComment.Append(s);
      fMissingUnits.Delete(i);
    end;
  end;
  Result:=fMissingUnits.Count;
end;

function TConvertDelphiUnit.AskUnitPathFromUser: TModalResult;
var
  TryAgain: Boolean;
  UnitDirDialog: TSelectDirectoryDialog;
  PrevMiss: LongInt;
begin
  // ask user what to do
  repeat
    TryAgain:=False;
    Result:=AskMissingUnits(fMissingUnits, ExtractFileName(fLazUnitFilename));
    case Result of
      // mrOK means: comment out.
      mrOK: begin
        // These units will be commented automatically in this project/package.
        if Assigned(fOwnerConverter) then
          fOwnerConverter.fAllMissingUnits.AddStrings(fMissingUnits);
        fUnitsToComment.AddStrings(fMissingUnits);
      end;
      // mrYes means: Search for unit path.
      mrYes: begin
        UnitDirDialog:=TSelectDirectoryDialog.Create(nil);
        try
          UnitDirDialog.InitialDir:=fSettings.MainPath;
          UnitDirDialog.Title:='All sub-directories will be scanned for unit files';
          if UnitDirDialog.Execute and Assigned(fOwnerConverter) then begin
            PrevMiss:=fMissingUnits.Count;
            // Add the new path to project if missing units are found.
            fOwnerConverter.CacheUnitsInPath(UnitDirDialog.Filename);
            TryAgain:=fOwnerConverter.DoMissingUnits(fMissingUnits)>0;
            if TryAgain then
              TryAgain:=fOwnerConverter.DoCaseErrorUnits(fMissingUnits, fUnitsToRename)>0;
            if TryAgain and (PrevMiss<>fMissingUnits.Count) then
              ShowMessage('Some units were found but not all.');
          end;
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
begin
  Result:=mrOk;
  MissingIncludeFilesCodeXYPos:=nil;
  try
    if not CodeToolBoss.FixIncludeFilenames(fUnitCode,true,MissingIncludeFilesCodeXYPos)
    then begin
      if MissingIncludeFilesCodeXYPos<>nil then begin
        for i:=0 to MissingIncludeFilesCodeXYPos.Count-1 do begin
          CodePos:=PCodeXYPosition(MissingIncludeFilesCodeXYPos[i]);
          Msg:=CodePos^.Code.Filename
               +'('+IntToStr(CodePos^.y)+','+IntToStr(CodePos^.x)+')'
               +' missing include file';
          IDEMessagesWindow.AddMsg(Msg, '', -1);
        end;
      end;
      IDEMessagesWindow.AddMsg('Error="'+CodeToolBoss.ErrorMessage+'"', '', -1);
      exit;
    end;
  finally
    CodeToolBoss.FreeListOfPCodeXYPosition(MissingIncludeFilesCodeXYPos);
  end;
end;

function TConvertDelphiUnit.FixMissingUnits: TModalResult;
var
  CTResult: Boolean;
  i: Integer;
begin
  Result:=mrOk;
  fMissingUnits:=nil; // Will be created in CodeToolBoss.FindMissingUnits.
  try
    // find missing units
    CTResult:=CodeToolBoss.FindMissingUnits(fUnitCode,fMissingUnits,true);
    if not CTResult then begin
      IDEMessagesWindow.AddMsg('Error="'+CodeToolBoss.ErrorMessage+'"','',-1);
      exit;
    end;
    // no missing units -> good
    if (fMissingUnits=nil) or (fMissingUnits.Count=0) then exit;

    // Remove Windows unit from missing list. It will be changed later.
    for i:=fMissingUnits.Count-1 downto 0 do begin
      if UpperCase(fMissingUnits[i])='WINDOWS' then begin
        fMissingUnits.Delete(i);
        break;
      end;
    end;
    if fMissingUnits.Count=0 then exit;

    if Assigned(fOwnerConverter) then begin
      // Try to find from subdirectories scanned earlier.
      if fOwnerConverter.DoMissingUnits(fMissingUnits)=0 then exit;
      // Comment out automatically units that were commented in other files.
      if CommentAutomatically=0 then exit;
    end;

    // Interactive dialog for searching unit.
    Result:=AskUnitPathFromUser;
    if Result<>mrOK then exit;

    // add error messages, so the user can click on them
    for i:=0 to fMissingUnits.Count-1 do
      IDEMessagesWindow.AddMsg(MissingUnitToMsg(fMissingUnits[i]),'',-1);
  finally
    fMissingUnits.Free;
  end;
end;

function TConvertDelphiUnit.CheckFailed(PrevResult: TModalResult): TModalResult;
begin
  Result:=PrevResult;
  if Result=mrCancel then begin
    Result:=QuestionDlg('Failed converting unit',
                        'Failed to convert unit'+#13+fOrigUnitFilename+#13,
                        mtWarning,[mrIgnore,'Ignore and continue',mrAbort],0);
    if Result=mrIgnore then
      Result:=mrOK;
  end;
end;


{ TConvertDelphiPBase }

constructor TConvertDelphiPBase.Create(const AFilename, ADescription: string);
begin
  fOrigPFilename:=AFilename;
  fCachedUnitNames:=TStringToStringTree.Create(true);
  fCachedRealUnitNames:=TStringToStringTree.Create(true);
  fSettings:=TConvertSettings.Create('Convert Delphi '+ADescription);
  fSettings.MainFilename:=fOrigPFilename;
  fAllMissingUnits:=TStringList.Create;
  fAllMissingUnits.Sorted:=true;
  // Scan unit files a level above project path. Used later for missing units.
  CacheUnitsInPath(TrimFilename(fSettings.MainPath+'../'));
end;

destructor TConvertDelphiPBase.Destroy;
begin
  fAllMissingUnits.Free;
  fCachedRealUnitNames.Free;
  fCachedUnitNames.Free;
  inherited Destroy;
end;

// Creates or updates a lazarus project (.lpi+.lpr) or package.
function TConvertDelphiPBase.Convert: TModalResult;
begin
  IDEMessagesWindow.Clear;
  // Get settings from user.
  Result:=fSettings.RunForm;
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
    IDEMessagesWindow.AddMsg('Conversion Ready.','',-1)
  else
    IDEMessagesWindow.AddMsg('Conversion Aborted.','',-1)
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

  // load required packages
  AddPackageDependency('LCL');// Nearly all Delphi projects require it
  if fProjPack is TProject then
    PkgBoss.AddDefaultDependencies(fProjPack as TProject);
  CustomDefinesChanged;

  SetCompilerModeForDefineTempl(CustomDefines);
  try
    Result:=ConvertMainSourceFile; // Convert project's LPR file.
    if Result<>mrOK then exit;
    // get all options from the .dpr or .dpk file
    Result:=ExtractOptionsFromDelphiSource;
    if Result<>mrOK then exit;
    Result:=FindAllUnits;      // find all files
    if Result<>mrOK then exit;
    Result:=ConvertAllUnits; // convert all files
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
  for i:=0 to ConverterList.Count-1 do begin
    Converter:=TConvertDelphiUnit(ConverterList[i]); // Converter created in cycle1.
    Result:=Converter.ConvertFormFile;
    Result:=Converter.CheckFailed(Result);
    if Result<>mrOK then exit;
    // Loading the form makes it appear in forms list. (Is there a better way?)
    //  fUnitInfo is set for projects only.
    if Assigned(Converter.fUnitInfo) then
      LazarusIDE.GetDesignerWithProjectFile(TUnitInfo(Converter.fUnitInfo), true);
    // Close file after processing.
    Result:=LazarusIDE.DoCloseEditorFile(Converter.fLazUnitFilename,[cfSaveFirst,cfQuiet]);
    if Result<>mrOK then exit;
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

function TConvertDelphiPBase.DoMissingUnits(MissingUnits: TStrings): integer;
// Locate unit names in earlier cached list and remove them from MissingUnits.
// Return the number of units still missing.
var
  i: Integer;
  sUnitPath: string;
begin
  for i:=MissingUnits.Count-1 downto 0 do begin
    sUnitPath:=GetCachedUnitPath(MissingUnits[i]);
    if sUnitPath<>'' then begin
      // Found: add unit path to project's settings.
      with CompOpts do
        OtherUnitFiles:=MergeSearchPaths(OtherUnitFiles,sUnitPath);
      // No more missing, delete from list.
      MissingUnits.Delete(i);
    end;
  end;
  Result:=MissingUnits.Count;
end;

function TConvertDelphiPBase.DoCaseErrorUnits(MissingUnits: TStrings;
                                   UnitsToRename: TStringToStringTree): integer;
// Locate existing unit names with different case add them to UnitsToRename.
// Return the number of units still missing.
var
  i: Integer;
  sUnitPath, mUnit, RealUnitName: string;
begin
  for i:=MissingUnits.Count-1 downto 0 do begin
    mUnit:=MissingUnits[i];
    sUnitPath:=GetCachedUnitPath(mUnit);
    Assert(sUnitPath='', 'sUnitPath should be empty');
    if NeedsRenameUnit(mUnit, RealUnitName) then begin
      // Add to rename unit list, delete from missing unit list.
      UnitsToRename[mUnit]:=RealUnitName;
      MissingUnits.Delete(i);
    end;
  end;
  Result:=MissingUnits.Count;
end;

procedure TConvertDelphiPBase.CacheUnitsInPath(const APath, ABasePath: string);
// Search all pascal units in APath and store them in fCachedUnitNames
//  with a path relative to ABasePath.
var
  PasFileList: TStringList;
  i: Integer;
  PasFile, RelPath, SubPath, sUnitName: String;
begin
  PasFileList:=FindAllFiles(APath,'*.pas',true);
  for i:=0 to PasFileList.Count-1 do begin
    PasFile:=PasFileList[i];
    RelPath:=FileUtil.CreateRelativePath(PasFile, ABasePath);
    SubPath:=ExtractFilePath(RelPath);
    sUnitName:=ExtractFileNameOnly(RelPath);
    if (SubPath<>'') and (sUnitName<>'') then begin
      // Map path by unit name.
      fCachedUnitNames[sUnitName]:=SubPath;
      // Map real unit name by uppercase unit name.
      fCachedRealUnitNames[UpperCase(sUnitName)]:=sUnitName;
    end;
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

function TConvertDelphiPBase.NeedsRenameUnit(const AUnitName: string;
                                             out RealUnitName: string): Boolean;
begin
  RealUnitName:=fCachedRealUnitNames[UpperCase(AUnitName)];
  Result := (RealUnitName<>'') and (RealUnitName<>AUnitName);
end;

function TConvertDelphiPBase.CreateMainSourceFile: TModalResult;
begin
  Result:=mrOK; // Do nothing. Overridden in project.
end;

function TConvertDelphiPBase.ConvertMainSourceFile: TModalResult;
begin
  Result:=mrOK; // Do nothing. Overridden in project.
end;


{ TConvertDelphiProject }

constructor TConvertDelphiProject.Create(const aProjectFilename: string);
begin
  inherited Create(aProjectFilename, 'Project');
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
// If .lpi does not exist, create it
// open new project
begin
  LazProject:=Project1;
  if FileExistsUTF8(fLazPFilename) then begin
    // there is already a lazarus project -> open it, if not already open
    if (LazProject=nil) or
       (CompareFilenames(LazProject.ProjectInfoFile,fLazPFilename)<>0) then
    begin
      Result:=LazarusIDE.DoOpenProjectFile(fLazPFilename,[]);
      LazProject:=Project1;
      if Result<>mrOk then exit;
    end;
  end else begin
    // create a new lazarus project
    Result:=LazarusIDE.DoNewProject(ProjectDescriptorEmptyProject);
    LazProject:=Project1;
    if Result<>mrOk then exit;
    LazProject.ProjectInfoFile:=fLazPFilename;
  end;
  // save to disk (this makes sure, all editor changes are saved too)
  Result:=LazarusIDE.DoSaveProject([]);
end;

function TConvertDelphiProject.CreateMainSourceFile: TModalResult;
// if .lpr does not exists, copy the .dpr file to the .lpr
// adds the .lpr as main unit to the project, if not already done
const
  LprExt='.lpr';
var
  MainUnitInfo: TUnitInfo;
begin
  // Converter for main LPR file.
  fMainUnitConverter:=TConvertDelphiUnit.Create(Self,fOrigPFilename,[]);
  fMainUnitConverter.LazFileExt:=LprExt;
  fMainUnitConverter.CopyAndLoadFile;
  if LazProject.MainUnitInfo=nil then begin
    // add .lpr file to project as main unit
    MainUnitInfo:=TUnitInfo.Create(fMainUnitConverter.fUnitCode);
    MainUnitInfo.SyntaxHighlighter:=ExtensionToLazSyntaxHighlighter(LprExt);
    MainUnitInfo.IsPartOfProject:=true;
    LazProject.AddFile(MainUnitInfo,false);
    LazProject.MainFileID:=0;
  end else begin
    // replace main unit in project
    LazProject.MainUnitInfo.Source:=fMainUnitConverter.fUnitCode;
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

function TConvertDelphiProject.FindAllUnits: TModalResult;
var
  FoundInUnits, MissingInUnits, NormalUnits: TStrings;
  NotFoundUnits: String;
  i: Integer;
  CurUnitInfo: TUnitInfo;
  CurFilename: string;
  NewSearchPath, AllPath, UselessPath: String;
  p: LongInt;
  OffendingUnit: TUnitInfo;
begin
  FoundInUnits:=nil;
  MissingInUnits:=nil;
  NormalUnits:=nil;
  try
    if not CodeToolBoss.FindDelphiProjectUnits(fMainUnitConverter.fUnitCode,
      FoundInUnits, MissingInUnits, NormalUnits) then
    begin
      LazarusIDE.DoJumpToCodeToolBossError;
      Result:=mrCancel;
      exit;
    end;
    // warn about missing units
    if (MissingInUnits<>nil) and (MissingInUnits.Count>0) then begin
      NotFoundUnits:=MissingInUnits.Text;
      Result:=QuestionDlg('Units not found',
        'Some units of the delphi project are missing:'#13
        +NotFoundUnits,mtWarning,[mrIgnore,mrAbort],0);
      if Result<>mrIgnore then exit;
    end;

    try
      // add all units to the project
      for i:=0 to FoundInUnits.Count-1 do begin
        CurFilename:=FoundInUnits[i];
        p:=System.Pos(' in ',CurFilename);
        if p>0 then
          CurFilename:=copy(CurFilename,p+4,length(CurFilename));
        if CurFilename='' then continue;
        if not FilenameIsAbsolute(CurFilename) then
          CurFilename:=AppendPathDelim(LazProject.ProjectDirectory)+CurFilename;
        CurFilename:=TrimFilename(CurFilename);
        if not FileExistsUTF8(CurFilename) then
          continue;
        CurUnitInfo:=LazProject.UnitInfoWithFilename(CurFilename);
        if CurUnitInfo<>nil then begin
          CurUnitInfo.IsPartOfProject:=true;
        end else begin
          if FilenameIsPascalUnit(CurFilename) then begin
            // check unitname
            OffendingUnit:=LazProject.UnitWithUnitname(
                                                ExtractFileNameOnly(CurFilename));
            if OffendingUnit<>nil then begin
              Result:=QuestionDlg('Unitname exists twice',
                'There are two units with the same unitname:'#13
                +OffendingUnit.Filename+#13
                +CurFilename+#13,
                mtWarning,[mrYes,'Remove first',mrNo,'Remove second',
                           mrIgnore,'Keep both',mrAbort],0);
              case Result of
              mrYes: OffendingUnit.IsPartOfProject:=false;
              mrNo:  continue;
              mrIgnore: ;
              else
                Result:=mrAbort;
                exit;
              end;
            end;
          end;

          // add new unit to project
          CurUnitInfo:=TUnitInfo.Create(nil);
          CurUnitInfo.Filename:=CurFilename;
          CurUnitInfo.IsPartOfProject:=true;
          LazProject.AddFile(CurUnitInfo,false);
        end;
      end;

    finally
      // set unit paths to find all project units
      AllPath:=LazProject.SourceDirectories.CreateSearchPathFromAllFiles;
      UselessPath:='.;'+VirtualDirectory+';'+VirtualTempDir+';'+LazProject.ProjectDirectory;
      NewSearchPath:=MergeSearchPaths(LazProject.CompilerOptions.OtherUnitFiles,AllPath);
      NewSearchPath:=RemoveSearchPaths(NewSearchPath,UselessPath);
      LazProject.CompilerOptions.OtherUnitFiles:=MinimizeSearchPath(
               RemoveNonExistingPaths(NewSearchPath,LazProject.ProjectDirectory));
      // set include path
      NewSearchPath:=MergeSearchPaths(LazProject.CompilerOptions.IncludePath,AllPath);
      NewSearchPath:=RemoveSearchPaths(NewSearchPath,UselessPath);
      LazProject.CompilerOptions.IncludePath:=MinimizeSearchPath(
               RemoveNonExistingPaths(NewSearchPath,LazProject.ProjectDirectory));
      // clear caches
      LazProject.DefineTemplates.SourceDirectoriesChanged;
    end;

    // save project
    Result:=LazarusIDE.DoSaveProject([sfQuietUnitCheck]);
    if Result<>mrOk then exit;

  finally
    FoundInUnits.Free;
    MissingInUnits.Free;
    NormalUnits.Free;
  end;

  Result:=mrOk;
end;

function TConvertDelphiProject.ConvertAllUnits: TModalResult;
var
  i: Integer;
  CurUnitInfo: TUnitInfo;
  Converter: TConvertDelphiUnit;
  ConvUnits: TObjectList;       // List of ConvertDelphiUnits.
begin
  Result:=mrOk;
  ConvUnits:=TObjectList.create;
  try
    // convert all units and fix .lfm files
    for i:=0 to LazProject.UnitCount-1 do begin
      CurUnitInfo:=LazProject.Units[i];
      // Main LPR file was converted earlier.
      if CurUnitInfo.IsPartOfProject and (CurUnitInfo<>LazProject.MainUnitInfo) then
      begin
        Converter:=TConvertDelphiUnit.Create(Self, CurUnitInfo.Filename, []);
        Converter.fUnitInfo:=CurUnitInfo;
        ConvUnits.Add(Converter);
        Result:=Converter.CopyAndLoadFile;
        Result:=Converter.CheckFailed(Result);
        if Result<>mrOK then Break;
        Result:=Converter.ConvertUnitFile;
//        Result:=Converter.CheckFailed(Result);
        if Result<>mrOK then Break;
      end;
    end;
    if Result=mrOK then
      Result:=ConvertAllFormFiles(ConvUnits);
  finally
    ConvUnits.Free;  // Owns and frees converter objects.
  end;
end;

function TConvertDelphiProject.ExtractOptionsFromDelphiSource: TModalResult;
begin
  // TODO remove compiler directives and put them into project/package
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

procedure TConvertDelphiProject.AddPackageDependency(const PackageName: string);
begin
  (fProjPack as TProject).AddPackageDependency(PackageName);
end;

procedure TConvertDelphiProject.RemoveNonExistingFiles(RemoveFromUsesSection: boolean);
begin
  (fProjPack as TProject).RemoveNonExistingFiles(RemoveFromUsesSection);
end;


{ TConvertDelphiPackage }

constructor TConvertDelphiPackage.Create(const aPackageFilename: string);
begin
  inherited Create(aPackageFilename, 'Package');
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
    // there is already a lazarus package file
    // open the package editor
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
      MessageDlg('Package name exists',
        'There is already a package with the name "'+PkgName+'"'#13
        +'Please close this package first.',mtError,[mbAbort],0);
      PackageEditingInterface.DoOpenPackageFile(LazPackage.Filename,
                                                        [pofAddToRecent],true);
      Result:=mrAbort;
      exit;
    end else begin
      Result:=mrOk;
    end;
  end else begin
    // there is not yet a package with this name
    // -> create a new package with LCL as dependency
    LazPackage:=PackageGraph.CreateNewPackage(PkgName);
    PackageGraph.AddDependencyToPackage(LazPackage,
                  PackageGraph.LCLPackage.CreateDependencyWithOwner(LazPackage));
    LazPackage.Filename:=fLazPFilename;

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
    for i:=0 to LazPackage.FileCount-1 do begin
      PkgFile:=LazPackage.Files[i];
      Converter:=TConvertDelphiUnit.Create(Self, PkgFile.Filename, []);
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
      Result:=QuestionDlg('Units not found',
        'Some units of the delphi package are missing:'#13
        +NotFoundUnits,mtWarning,[mrIgnore,mrAbort],0);
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
              Result:=QuestionDlg('Unitname exists twice',
                'There are two units with the same unitname:'#13
                +OffendingUnit.Filename+#13
                +CurFilename+#13,
                mtWarning,[mrNo,'Remove second',mrAbort],0);
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
      // set unit paths to find all project units
      AllPath:=LazPackage.SourceDirectories.CreateSearchPathFromAllFiles;
      UselessPath:='.;'+VirtualDirectory+';'+VirtualTempDir+';'+LazPackage.Directory;
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
    Result:=MessageDlg('File not found',
      'Delphi package main source (.dpk) file not found for package'#13
      +LazPackage.Filename,mtError,[mbAbort],0);
    exit;
  end;
  Result:=LoadCodeBuffer(fDpkCode,DPKFilename,[],true);
end;

function TConvertDelphiPackage.ExtractOptionsFromDelphiSource: TModalResult;
begin
  // TODO, use fDelphiPFilename and LazPackage to get options.
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

procedure TConvertDelphiPackage.RemoveNonExistingFiles(RemoveFromUsesSection: boolean);
begin
  (fProjPack as TLazPackage).RemoveNonExistingFiles;
end;


end.

