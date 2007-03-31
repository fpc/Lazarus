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
  Classes, SysUtils, LCLProc, Forms, Controls, Dialogs, PropEdits, LazHelpHTML,
  IDEExternToolIntf, ProjectIntf, SrcEditorIntf;

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
    ofLoadHiddenResource,// load component hidden
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
    sfSaveNonProjectFiles,
    sfDoNotSaveVirtualFiles
    );
  TSaveFlags = set of TSaveFlag;
  
  // close file flags
  TCloseFlag = (
    cfSaveFirst, // check if modified and save
    cfQuiet,
    cfProjectClosing,
    cfCloseDependencies,
    cfSaveDependencies // set cfSaveFirst to closing the dependencies
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
    pbfSkipLinking,
    pbfSkipAssembler,
    pbfSkipTools,
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
  
  TModalResultFunction = function(Sender: TObject): TModalResult of object;

  TLazarusIDEHandlerType = (
    lihtOnSavingAll, // called before IDE saves everything
    lihtOnSavedAll   // called after IDE saved everything
    );
    
  { TLazIDEInterface }

  TLazIDEInterface = class(TComponent)
  private
    FLazarusIDEHandlers: array[TLazarusIDEHandlerType] of TMethodList;
    FMainBarSubTitle: string;
    procedure AddHandler(HandlerType: TLazarusIDEHandlerType;
                         const AMethod: TMethod; AsLast: boolean = false);
    procedure RemoveHandler(HandlerType: TLazarusIDEHandlerType;
                            const AMethod: TMethod);
  protected
    function GetActiveProject: TLazProject; virtual; abstract;
    procedure DoCallNotifyHandler(HandlerType: TLazarusIDEHandlerType);
    function DoCallModalFunctionHandler(HandlerType: TLazarusIDEHandlerType
                                        ): TModalResult;
    procedure SetMainBarSubTitle(const AValue: string); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    
    // the main window with the IDE menu
    function GetMainBar: TComponent; virtual; abstract;
    property MainBarSubTitle: string read FMainBarSubTitle write SetMainBarSubTitle;

    // find file
    function FindUnitFile(const AFilename: string): string; virtual; abstract;
    function FindSourceFile(const AFilename, BaseDirectory: string;
                            Flags: TFindSourceFlags): string; virtual; abstract;

    // file
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
    function DoRevertEditorFile(const Filename: string): TModalResult; virtual; abstract;

    // project
    property ActiveProject: TLazProject read GetActiveProject;
    function DoNewProject(ProjectDesc: TProjectDescriptor): TModalResult; virtual; abstract;
    function DoSaveProject(Flags: TSaveFlags): TModalResult; virtual; abstract;
    function DoCloseProject: TModalResult; virtual; abstract;
    function DoOpenProjectFile(AFileName: string;
                               Flags: TOpenFlags): TModalResult; virtual; abstract;
    function DoPublishProject(Flags: TSaveFlags;
                              ShowDialog: boolean): TModalResult; virtual; abstract;
    function DoBuildProject(const AReason: TCompileReason;
                            Flags: TProjectBuildFlags): TModalResult; virtual; abstract;

    // configs
    function GetPrimaryConfigPath: String; virtual; abstract;
    function GetSecondaryConfigPath: String; virtual; abstract;
    procedure CopySecondaryConfigFile(const AFilename: String); virtual; abstract;

    // filenames, paths
    function CreateNewUniqueFilename(const Prefix, Ext: string;
                          NewOwner: TObject; Flags: TSearchIDEFileFlags;
                          TryWithoutNumber: boolean): string; virtual; abstract;
    function GetTestBuildDirectory: string; virtual; abstract;

    // codetools
    function BeginCodeTools: boolean; virtual; abstract;
    procedure DoJumpToCodeToolBossError; virtual; abstract;
    procedure SaveSourceEditorChangesToCodeCache(PageIndex: integer); virtual; abstract;

    // progress and error messages
    function ShowProgress(const SomeText: string;
                          Step, MaxStep: integer): boolean; virtual; abstract; // False if canceled by user
    function DoJumpToCompilerMessage(Index:integer;
      FocusEditor: boolean): boolean; virtual; abstract;
    procedure DoJumpToNextError(DirectionDown: boolean); virtual; abstract;
    procedure DoShowMessagesView; virtual; abstract;
    function DoCheckFilesOnDisk(Instantaneous: boolean = false): TModalResult; virtual; abstract;

    // designer
    function GetDesignerWithProjectFile(AFile: TLazProjectFile;
                              LoadForm: boolean): TIDesigner; virtual; abstract;
    function GetProjectFileWithRootComponent(AComponent: TComponent): TLazProjectFile; virtual; abstract;
    function GetProjectFileWithDesigner(ADesigner: TIDesigner): TLazProjectFile; virtual; abstract;
    
    // events
    procedure RemoveAllHandlersOfObject(AnObject: TObject);
    procedure AddHandlerOnSavingAll(const OnSaveAllEvent: TModalResultFunction;
                                    AsLast: boolean = false);
    procedure RemoveHandlerOnSavingAll(const OnSaveAllEvent: TModalResultFunction);
    procedure AddHandlerOnSavedAll(const OnSaveAllEvent: TModalResultFunction;
                                   AsLast: boolean = false);
    procedure RemoveHandlerOnSavedAll(const OnSaveAllEvent: TModalResultFunction);
  end;
  
var
  LazarusIDE: TLazIDEInterface = nil; // will be set by the IDE

implementation

{ TLazIDEInterface }

procedure TLazIDEInterface.AddHandler(HandlerType: TLazarusIDEHandlerType;
  const AMethod: TMethod; AsLast: boolean);
begin
  if FLazarusIDEHandlers[HandlerType]=nil then
    FLazarusIDEHandlers[HandlerType]:=TMethodList.Create;
  FLazarusIDEHandlers[HandlerType].Add(AMethod);
end;

procedure TLazIDEInterface.RemoveHandler(HandlerType: TLazarusIDEHandlerType;
  const AMethod: TMethod);
begin
  FLazarusIDEHandlers[HandlerType].Remove(AMethod);
end;

procedure TLazIDEInterface.SetMainBarSubTitle(const AValue: string);
begin
  if FMainBarSubTitle=AValue then exit;
  FMainBarSubTitle:=AValue;
end;

procedure TLazIDEInterface.DoCallNotifyHandler(
  HandlerType: TLazarusIDEHandlerType);
begin
  FLazarusIDEHandlers[HandlerType].CallNotifyEvents(Self);
end;

function TLazIDEInterface.DoCallModalFunctionHandler(
  HandlerType: TLazarusIDEHandlerType): TModalResult;
var
  i: Integer;
  CurResult: TModalResult;
begin
  Result:=mrOk;
  i:=FLazarusIDEHandlers[HandlerType].Count;
  while FLazarusIDEHandlers[HandlerType].NextDownIndex(i) do begin
    CurResult:=TModalResultFunction(FLazarusIDEHandlers[HandlerType][i])(Self);
    if CurResult=mrAbort then exit(mrAbort);
    if CurResult<>mrOk then Result:=mrCancel;
  end;
end;

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

procedure TLazIDEInterface.RemoveAllHandlersOfObject(AnObject: TObject);
var
  HandlerType: TLazarusIDEHandlerType;
begin
  for HandlerType:=Low(TLazarusIDEHandlerType) to High(TLazarusIDEHandlerType) do
    FLazarusIDEHandlers[HandlerType].RemoveAllMethodsOfObject(AnObject);
end;

procedure TLazIDEInterface.AddHandlerOnSavingAll(
  const OnSaveAllEvent: TModalResultFunction; AsLast: boolean);
begin
  AddHandler(lihtOnSavingAll,TMethod(OnSaveAllEvent));
end;

procedure TLazIDEInterface.RemoveHandlerOnSavingAll(
  const OnSaveAllEvent: TModalResultFunction);
begin
  RemoveHandler(lihtOnSavingAll,TMethod(OnSaveAllEvent));
end;

procedure TLazIDEInterface.AddHandlerOnSavedAll(
  const OnSaveAllEvent: TModalResultFunction; AsLast: boolean);
begin
  AddHandler(lihtOnSavedAll,TMethod(OnSaveAllEvent));
end;

procedure TLazIDEInterface.RemoveHandlerOnSavedAll(
  const OnSaveAllEvent: TModalResultFunction);
begin
  RemoveHandler(lihtOnSavedAll,TMethod(OnSaveAllEvent));
end;

initialization
  RegisterPropertyEditor(TypeInfo(AnsiString),
    THTMLBrowserHelpViewer,'BrowserPath',TFileNamePropertyEditor);

end.

