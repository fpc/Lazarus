{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Interface to the general IDE functions.
}
unit LazIDEIntf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ProjectIntf, SrcEditorIntf;

type
  // open file flags
  TOpenFlag = (
    ofProjectLoading,// this open is part of opening a whole project
    ofOnlyIfExists,  // do not auto create non existing files
    ofRevert,        // reload file if already open
    ofQuiet,         // less messages
    ofAddToRecent,   // add file to recent files
    ofRegularFile,   // open as regular file (e.g. do not open projects/packages)
    ofVirtualFile,   // open the virtual file
    ofConvertMacros, // replace macros in filename
    ofUseCache,      // do not update file from disk
    ofMultiOpen,     // set during loading multiple files
    ofDoNotLoadResource,// do not open form, datamodule, ... (overriding default)
    ofDoLoadResource,// do open form, datamodule, ... (overriding default)
    ofAddToProject   // add file to project (if exists)
    );
  TOpenFlags = set of TOpenFlag;

  // new file flags
  TNewFlag = (
    nfIsPartOfProject, // force IsPartOfProject,
                       //   default is to use a heuristic
    nfIsNotPartOfProject,// forbid IsPartOfProject
    nfOpenInEditor,    // open in editor
    nfSave,            // save file instantly
    nfAddToRecent,     // add file to recent files
    nfQuiet,           // less messages
    nfConvertMacros,   // replace macros in filename
    nfBeautifySrc,     // beautify custom source
    nfCreateDefaultSrc // create initial source based on the type
    );
  TNewFlags = set of TNewFlag;

  // save file flags
  TSaveFlag = (
    sfSaveAs,
    sfSaveToTestDir,
    sfProjectSaving,
    sfCheckAmbiguousFiles,
    sfSaveNonProjectFiles
    );
  TSaveFlags = set of TSaveFlag;
  
  // close file flags
  TCloseFlag = (
    cfSaveFirst, // check if modified and save
    cfQuiet,
    cfProjectClosing
    );
  TCloseFlags = set of TCloseFlag;

  // build project flags
  TProjectBuildFlag = (
    pbfCleanCompile,  // append -B to the compiler options
    pbfDoNotCompileDependencies,
    pbfDoNotCompileProject,
    pbfCompileDependenciesClean,
    pbfOnlyIfNeeded,
    pbfDoNotSaveEditorFiles,
    pbfCreateMakefile
    );
  TProjectBuildFlags = set of TProjectBuildFlag;
  
  // new filename flags
  TSearchIDEFileFlag = (
    siffDoNotCheckAllPackages, // do not search filename in loaded packages
    siffCheckAllProjects, // do not search filename in loaded projects
    siffCaseSensitive,  // check case sensitive
    siffDoNotCheckOpenFiles,  // do not search in files opened in source editor
    siffIgnoreExtension  // compare only filename, ignore file extension
    );
  TSearchIDEFileFlags = set of TSearchIDEFileFlag;

  // find source flags
  TFindSourceFlag = (
    fsfSearchForProject,
    fsfUseIncludePaths,
    fsfUseDebugPath
    );
  TFindSourceFlags = set of TFindSourceFlag;

  { TLazIDEInterface }

  TLazIDEInterface = class(TComponent)
  protected
    function GetActiveProject: TLazProject; virtual; abstract;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    
    function FindUnitFile(const AFilename: string): string; virtual; abstract;
    function FindSourceFile(const AFilename, BaseDirectory: string;
                            Flags: TFindSourceFlags): string; virtual; abstract;

    function DoNewEditorFile(NewFileDescriptor: TProjectFileDescriptor;
        NewFilename: string; const NewSource: string;
        NewFlags: TNewFlags): TModalResult;
    function DoNewFile(NewFileDescriptor: TProjectFileDescriptor;
        var NewFilename: string; const NewSource: string;
        NewFlags: TNewFlags; NewOwner: TObject): TModalResult; virtual; abstract;
    function DoCloseEditorFile(PageIndex:integer;
        Flags: TCloseFlags):TModalResult; virtual; abstract;
    function DoCloseEditorFile(const Filename: string;
        Flags: TCloseFlags): TModalResult; virtual; abstract;
    function DoOpenEditorFile(AFileName:string; PageIndex: integer;
        Flags: TOpenFlags): TModalResult; virtual; abstract;
    function DoOpenFileAndJumpToIdentifier(const AFilename, AnIdentifier: string;
        PageIndex: integer; Flags: TOpenFlags): TModalResult; virtual; abstract;
    function DoOpenFileAndJumpToPos(const AFilename: string;
        const CursorPosition: TPoint; TopLine: integer;
        PageIndex: integer; Flags: TOpenFlags): TModalResult; virtual; abstract;

    function DoNewProject(ProjectDesc: TProjectDescriptor): TModalResult; virtual; abstract;
    function DoSaveProject(Flags: TSaveFlags): TModalResult; virtual; abstract;
    function DoCloseProject: TModalResult; virtual; abstract;
    function DoOpenProjectFile(AFileName: string;
                               Flags: TOpenFlags): TModalResult; virtual; abstract;
    function DoPublishProject(Flags: TSaveFlags;
                              ShowDialog: boolean): TModalResult; virtual; abstract;

    function GetPrimaryConfigPath: String; virtual; abstract;
    function GetSecondaryConfigPath: String; virtual; abstract;
    procedure CopySecondaryConfigFile(const AFilename: String); virtual; abstract;

    function CreateNewUniqueFilename(const Prefix, Ext: string;
                          NewOwner: TObject; Flags: TSearchIDEFileFlags;
                          TryWithoutNumber: boolean): string; virtual; abstract;
       
    function SubstituteMakros(var s: string): boolean; virtual; abstract;

    function BeginCodeTools: boolean; virtual; abstract;
    procedure DoJumpToCodeToolBossError; virtual; abstract;
    procedure SaveSourceEditorChangesToCodeCache(PageIndex: integer); virtual; abstract;
    
    function ShowProgress(const SomeText: string;
                          Step, MaxStep: integer): boolean; virtual; abstract; // False if canceled by user
    function DoJumpToCompilerMessage(Index:integer;
      FocusEditor: boolean): boolean; virtual; abstract;
    procedure DoJumpToNextError(DirectionDown: boolean); virtual; abstract;
    procedure DoShowMessagesView; virtual; abstract;

    function GetDesignerWithProjectFile(AFile: TLazProjectFile;
                              LoadForm: boolean): TIDesigner; virtual; abstract;
    function GetProjectFileWithRootComponent(AComponent: TComponent): TLazProjectFile; virtual; abstract;
    function GetProjectFileWithDesigner(ADesigner: TIDesigner): TLazProjectFile; virtual; abstract;
  public
    property ActiveProject: TLazProject read GetActiveProject;
  end;
  
var
  LazarusIDE: TLazIDEInterface; // will be set by the IDE

implementation

{ TLazIDEInterface }

constructor TLazIDEInterface.Create(TheOwner: TComponent);
begin
  LazarusIDE:=Self;
  inherited Create(TheOwner);
end;

destructor TLazIDEInterface.Destroy;
begin
  inherited Destroy;
  LazarusIDE:=nil;
end;

function TLazIDEInterface.DoNewEditorFile(
  NewFileDescriptor: TProjectFileDescriptor; NewFilename: string;
  const NewSource: string; NewFlags: TNewFlags): TModalResult;
begin
  Result:=DoNewFile(NewFileDescriptor,NewFilename,NewSource,NewFlags,nil);
end;

end.

