{  $Id$  }
{
 /***************************************************************************
                  project.pp  -  project utility class file
                  -----------------------------------------
          TProject is responsible for managing a complete project.


              Initial Revision  : Sun Mar 28 23:15:32 CST 1999


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
unit Project;

{$mode objfpc}{$H+}

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, FPCAdds, LCLIntf, LCLType, Laz_XMLCfg, LazConf,
  CompilerOptions, FileCtrl, CodeToolManager, CodeCache, Forms, Controls,
  EditorOptions, Dialogs, IDEProcs, RunParamsOpts, ProjectDefs, EditDefineTree,
  DefineTemplates, PackageDefs;

type
  TUnitInfo = class;
  TProject = class;

  TOnFileBackup = function(const FileToBackup:string; 
                           IsPartOfProject:boolean):TModalResult of object;
  TOnUnitNameChange = procedure(AnUnitInfo: TUnitInfo;
       const OldUnitName, NewUnitName: string;
       CheckIfAllowed: boolean;
       var Allowed: boolean) of object;
  TOnLoadProjectInfo = procedure(TheProject: TProject;
                                 XMLConfig: TXMLConfig) of object;
  TOnSaveProjectInfo = procedure(TheProject: TProject;
                                 XMLConfig: TXMLConfig) of object;
                                 
  TUnitInfoList = (
    uilPartOfProject,
    uilWithEditorIndex,
    uilWithComponent,
    uilLoaded,
    uilAutoRevertLocked
    );

  //---------------------------------------------------------------------------
  TUnitInfo = class(TObject)
  private
    fAutoRevertLockCount: integer;
    fBookmarks: TFileBookmarks;
    FBuildFileIfActive: boolean;
    fComponent: TComponent;
    fComponentName: string; { classname is always T<ComponentName>
         this attribute contains the component name,
         even if the unit is not loaded,
         or the designer form is not created.
         A component can be a TForm or a TDataModule }
    fComponentResourceName: string;
    FComponentLastBinStreamSize: TStreamSeekType;
    FComponentLastLFMStreamSize: TStreamSeekType;
    FComponentLastLRSStreamSize: TStreamSeekType;
    fCursorPos: TPoint;
    fCustomHighlighter: boolean; // do not change highlighter on file extension change
    fEditorIndex: integer;
    fFileName: string;
    fFileReadOnly: Boolean;
    fHasResources: boolean; // source has resource file
    FIgnoreFileDateOnDiskValid: boolean;
    FIgnoreFileDateOnDisk: longint;
    fIsPartOfProject: boolean;
    fLoaded: Boolean;  // loaded in the source editor
    fModified: boolean;
    fNext, fPrev: array[TUnitInfoList] of TUnitInfo;
    fOnFileBackup: TOnFileBackup;
    fOnLoadSaveFilename: TOnLoadSaveFilename;
    fOnUnitNameChange: TOnUnitNameChange;
    FProject: TProject;
    FResourceFilename: string;
    FRunFileIfActive: boolean;
    fSource: TCodeBuffer;
    fSyntaxHighlighter: TLazSyntaxHighlighter;
    fTopLine: integer;
    fUnitName: String;
    fUsageCount: extended;
    fUserReadOnly:  Boolean;

    function GetFileName: string;
    function GetHasResources:boolean;
    function GetNextAutoRevertLockedUnit: TUnitInfo;
    function GetNextLoadedUnit: TUnitInfo;
    function GetNextPartOfProject: TUnitInfo;
    function GetNextUnitWithComponent: TUnitInfo;
    function GetNextUnitWithEditorIndex: TUnitInfo;
    function GetPrevAutoRevertLockedUnit: TUnitInfo;
    function GetPrevLoadedUnit: TUnitInfo;
    function GetPrevPartOfProject: TUnitInfo;
    function GetPrevUnitWithComponent: TUnitInfo;
    function GetPrevUnitWithEditorIndex: TUnitInfo;
    procedure SetBuildFileIfActive(const AValue: boolean);
    procedure SetEditorIndex(const AValue: integer);
    procedure SetFileReadOnly(const AValue: Boolean);
    procedure SetComponent(const AValue: TComponent);
    procedure SetIsPartOfProject(const AValue: boolean);
    procedure SetLoaded(const AValue: Boolean);
    procedure SetProject(const AValue: TProject);
    procedure SetRunFileIfActive(const AValue: boolean);
    procedure SetSource(ABuffer: TCodeBuffer);
    procedure SetUnitName(const NewUnitName:string);
    procedure SetUserReadOnly(const NewValue: boolean);
  protected
    procedure UpdateList(ListType: TUnitInfoList; Add: boolean);
  public
    constructor Create(ACodeBuffer: TCodeBuffer);
    destructor Destroy; override;

    function ChangedOnDisk(CompareOnlyLoadSaveTime: boolean): boolean;
    function IsAutoRevertLocked: boolean;
    function IsMainUnit: boolean;
    function IsVirtual: boolean;
    function NeedsSaveToDisk: boolean;
    function ReadOnly: boolean;
    function ReadUnitSource(ReadUnitName,Revert:boolean): TModalResult;
    function ShortFilename: string;
    function WriteUnitSource: TModalResult;
    function WriteUnitSourceToFile(const AFileName: string): TModalResult;
    procedure Clear;
    procedure CreateStartCode(NewUnitType: TNewUnitType;
                              const NewUnitName: string);
    procedure DecreaseAutoRevertLock;
    procedure IgnoreCurrentFileDateOnDisk;
    procedure IncreaseAutoRevertLock;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure ReadUnitNameFromSource(TryCache: boolean);
    function CreateUnitName: string;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure UpdateUsageCount(Min, IfBelowThis, IncIfBelow: extended);
    procedure UpdateUsageCount(TheUsage: TUnitUsage; Factor: extended);

    { Properties }
  public
    // Unit lists
    property NextUnitWithEditorIndex: TUnitInfo read GetNextUnitWithEditorIndex;
    property PrevUnitWithEditorIndex: TUnitInfo read GetPrevUnitWithEditorIndex;
    property NextUnitWithComponent: TUnitInfo read GetNextUnitWithComponent;
    property PrevUnitWithComponent: TUnitInfo read GetPrevUnitWithComponent;
    property NextLoadedUnit: TUnitInfo read GetNextLoadedUnit;
    property PrevLoadedUnit: TUnitInfo read GetPrevLoadedUnit;
    property NextAutoRevertLockedUnit: TUnitInfo read GetNextAutoRevertLockedUnit;
    property PrevAutoRevertLockedUnit: TUnitInfo read GetPrevAutoRevertLockedUnit;
    property NextPartOfProject: TUnitInfo read GetNextPartOfProject;
    property PrevPartOfProject: TUnitInfo read GetPrevPartOfProject;
  public
    property Bookmarks: TFileBookmarks read FBookmarks write FBookmarks;
    property BuildFileIfActive: boolean read FBuildFileIfActive write SetBuildFileIfActive;
    property Component: TComponent read fComponent write SetComponent;
    property ComponentName: string read fComponentName write fComponentName;
    property ComponentResourceName: string read fComponentResourceName
                                           write fComponentResourceName;
    property ComponentLastBinStreamSize: TStreamSeekType
             read FComponentLastBinStreamSize write FComponentLastBinStreamSize;
    property ComponentLastLRSStreamSize: TStreamSeekType
             read FComponentLastLRSStreamSize write FComponentLastLRSStreamSize;
    property ComponentLastLFMStreamSize: TStreamSeekType
             read FComponentLastLFMStreamSize write FComponentLastLFMStreamSize;
    property CursorPos: TPoint read fCursorPos write fCursorPos;
    property CustomHighlighter: boolean
                               read fCustomHighlighter write fCustomHighlighter;
    property EditorIndex:integer read fEditorIndex write SetEditorIndex;
    property Filename: String read GetFilename;
    property FileReadOnly: Boolean read fFileReadOnly write SetFileReadOnly;
    property HasResources: boolean read GetHasResources write fHasResources;
    property IsPartOfProject: boolean
                                 read fIsPartOfProject write SetIsPartOfProject;
    property Loaded: Boolean read fLoaded write SetLoaded;
    property Modified: boolean read fModified write fModified;
    property OnFileBackup: TOnFileBackup read fOnFileBackup write fOnFileBackup;
    property OnLoadSaveFilename: TOnLoadSaveFilename
                             read fOnLoadSaveFilename write fOnLoadSaveFilename;
    property OnUnitNameChange: TOnUnitNameChange
                                 read fOnUnitNameChange write fOnUnitNameChange;
    property Project: TProject read FProject write SetProject;
    property ResourceFileName: string
                                 read FResourceFilename write FResourceFilename;
    property RunFileIfActive: boolean read FRunFileIfActive write SetRunFileIfActive;
    property Source: TCodeBuffer read fSource write SetSource;
    property SyntaxHighlighter: TLazSyntaxHighlighter
                               read fSyntaxHighlighter write fSyntaxHighlighter;
    property TopLine: integer read fTopLine write fTopLine;
    property UnitName: String read fUnitName write SetUnitName;
    property UserReadOnly: Boolean read fUserReadOnly write SetUserReadOnly;
  end;


  //---------------------------------------------------------------------------

  { TProjectCompilerOptions }

  TProjectCompilerOptions = class(TCompilerOptions)
  private
    FGlobals: TGlobalCompilerOptions;
    FOwnerProject: TProject;
  protected
    procedure SetTargetCPU(const AValue: string); override;
    procedure SetTargetOS(const AValue: string); override;
    procedure Assign(CompOpts: TBaseCompilerOptions); override;
    procedure UpdateGlobals; virtual;
  public
    constructor Create(TheProject: TProject);
    destructor Destroy; override;
    function GetOwnerName: string; override;
    function GetDefaultMainSourceFileName: string; override;
    procedure GetInheritedCompilerOptions(var OptionsList: TList); override;
  public
    property OwnerProject: TProject read FOwnerProject;
    property Globals: TGlobalCompilerOptions read FGlobals;
  end;
  
  
  { TProjectDefineTemplates }

  TProjectDefineTemplatesFlag = (
    ptfFlagsChanged
    );
  TProjectDefineTemplatesFlags = set of TProjectDefineTemplatesFlag;

  TProjectDefineTemplates = class
  private
    FFlags: TProjectDefineTemplatesFlags;
    FMain: TDefineTemplate;
    FProjectDir: TDefineTemplate;
    FProject: TProject;
    FUpdateLock: integer;
    procedure UpdateMain;
  public
    constructor Create(OwnerProject: TProject);
    destructor Destroy; override;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure CompilerFlagsChanged;
    procedure AllChanged;
    procedure UpdateGlobalValues;
  public
    property Owner: TProject read FProject;
    property Main: TDefineTemplate read FMain;
  end;


  { TProject }
  
  TProjectType =   // for a description see ProjectTypeDescriptions below
    (ptApplication, ptProgram, ptCustomProgram, ptCGIApplication);
     
  TProjectFlag = (
    pfSaveClosedUnits,     // save info about closed files (not part of project)
    pfSaveOnlyProjectUnits // save no info about foreign files
    );
  TProjectFlags = set of TProjectFlag;
  
  TProjectFileSearchFlag = (
    pfsfResolveFileLinks,
    pfsfOnlyEditorFiles,
    pfsfOnlyVirtualFiles,
    pfsfOnlyProjectFiles
    );
  TProjectFileSearchFlags = set of TProjectFileSearchFlag;

  TEndUpdateProjectEvent =
    procedure(Sender: TObject; ProjectChanged: boolean) of object;
    
  TProject = class(TObject)
  private
    fActiveEditorIndexAtStart: integer;
    FAutoCreateForms: boolean;
    FAutoOpenDesignerFormsDisabled: boolean;
    fBookmarks: TProjectBookmarkList;
    fChanged: boolean;
    fCompilerOptions: TProjectCompilerOptions;
    FDefineTemplates: TProjectDefineTemplates;

    FFirstRemovedDependency: TPkgDependency;
    FFirstRequiredDependency: TPkgDependency;
    fFirst: array[TUnitInfoList] of TUnitInfo;

    fDestroying: boolean;
    FFlags: TProjectFlags;
    fIconPath: String;
    fJumpHistory: TProjectJumpHistory;
    fLastReadLPIFileDate: TDateTime;
    fLastReadLPIFilename: string;
    fMainUnitID: Integer;  // only for ptApplication, ptProgram, ptCGIApplication
    fModified: boolean;
    FOnBeginUpdate: TNotifyEvent;
    FOnEndUpdate: TEndUpdateProjectEvent;
    fOnFileBackup: TOnFileBackup;
    FOnLoadProjectInfo: TOnLoadProjectInfo;
    FOnSaveProjectInfo: TOnSaveProjectInfo;
    fPathDelimChanged: boolean;
    fProjectDirectory: string;
    fProjectInfoFile: String;  // the lpi filename
    fProjectType: TProjectType;
    fPublishOptions: TPublishProjectOptions;
    fRunParameterOptions: TRunParamsOptions;
    fTargetFileExt: String;
    fTitle: String;
    fUnitList: TList;  // list of _all_ units (TUnitInfo)
    FUpdateLock: integer;
    xmlconfig: TXMLConfig;
    function GetFirstAutoRevertLockedUnit: TUnitInfo;
    function GetFirstLoadedUnit: TUnitInfo;
    function GetFirstPartOfProject: TUnitInfo;
    function GetFirstUnitWithComponent: TUnitInfo;
    function GetFirstUnitWithEditorIndex: TUnitInfo;
    function GetMainFilename: String;
    function GetMainUnitInfo: TUnitInfo;
    function GetProjectInfoFile: string;
    function GetTargetFilename: string;
    function GetUnits(Index: integer): TUnitInfo;
    function JumpHistoryCheckPosition(
                                APosition:TProjectJumpHistoryPosition): boolean;
    function OnUnitFileBackup(const Filename: string;
                              IsPartOfProject:boolean): TModalResult;
    procedure OnLoadSaveFilename(var AFilename: string; Load: boolean);
    procedure OnUnitNameChange(AnUnitInfo: TUnitInfo;
                               const OldUnitName, NewUnitName: string;
                               CheckIfAllowed: boolean; var Allowed: boolean);
    procedure SetAutoOpenDesignerFormsDisabled(const AValue: boolean);
    procedure SetFlags(const AValue: TProjectFlags);
    procedure SetMainUnitID(const AValue: Integer);
    procedure SetModified(const AValue: boolean);
    procedure SetProjectInfoFile(const NewFilename: string);
    procedure SetTargetFilename(const NewTargetFilename: string);
    procedure SetUnits(Index:integer; AUnitInfo: TUnitInfo);
    procedure UpdateProjectDirectory;
  protected
    // special unit lists
    procedure AddToList(AnUnitInfo: TUnitInfo; ListType: TUnitInfoList);
    procedure RemoveFromList(AnUnitInfo: TUnitInfo; ListType: TUnitInfoList);

    procedure AddToOrRemoveFromAutoRevertLockedList(AnUnitInfo: TUnitInfo);
    procedure AddToOrRemoveFromEditorWithIndexList(AnUnitInfo: TUnitInfo);
    procedure AddToOrRemoveFromComponentList(AnUnitInfo: TUnitInfo);
    procedure AddToOrRemoveFromLoadedList(AnUnitInfo: TUnitInfo);
    procedure AddToOrRemoveFromPartOfProjectList(AnUnitInfo: TUnitInfo);
  public
    constructor Create(TheProjectType: TProjectType);
    destructor Destroy; override;
    procedure Clear;
    procedure BeginUpdate(Change: boolean);
    procedure EndUpdate;

    // load/save
    function IsVirtual: boolean;
    function SomethingModified: boolean;
    procedure MainSourceFilenameChanged;
    procedure GetUnitsChangedOnDisk(var AnUnitList: TList);
    function ReadProject(const LPIFilename: string): TModalResult;
    function WriteProject(ProjectWriteFlags: TProjectWriteFlags;
                          const OverrideProjectInfoFile: string): TModalResult;
                          
    // title
    function GetDefaultTitle: string;
    function TitleIsDefault: boolean;

    // units
    function UnitCount:integer;
    function NewUniqueUnitName(NewUnitType:TNewUnitType): string;
    function NewUniqueComponentName(NewUnitType:TNewUnitType): string;
    procedure AddUnit(AnUnit: TUnitInfo; AddToProjectFile: boolean);
    procedure RemoveUnit(Index: integer);
    
    // search
    function IndexOf(AUnitInfo: TUnitInfo): integer;
    function IndexOfUnitWithName(const AnUnitName: string;
                      OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo): integer;
    function IndexOfUnitWithComponent(AComponent: TComponent;
                      OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo): integer;
    function IndexOfUnitWithComponentName(const AComponentName: string;
                      OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo): integer;
    function IndexOfFilename(const AFilename: string): integer;
    function IndexOfFilename(const AFilename: string;
                             SearchFlags: TProjectFileSearchFlags): integer;
    function ProjectUnitWithFilename(const AFilename: string): TUnitInfo;
    function ProjectUnitWithUnitname(const AnUnitName: string): TUnitInfo;
    function UnitWithEditorIndex(Index:integer): TUnitInfo;
    Function UnitWithComponent(AComponent: TComponent): TUnitInfo;
    function UnitInfoWithFilename(const AFilename: string): TUnitInfo;
    function UnitInfoWithFilename(const AFilename: string;
                    SearchFlags: TProjectFileSearchFlags): TUnitInfo;
    function UnitWithUnitname(const AnUnitname: string): TUnitInfo;

    // units in editor
    procedure CloseEditorIndex(EditorIndex:integer);
    procedure InsertEditorIndex(EditorIndex:integer);
    procedure MoveEditorIndex(OldEditorIndex, NewEditorIndex: integer);

    // Application.CreateForm statements
    function AddCreateFormToProjectFile(const AClassName, AName:string):boolean;
    function RemoveCreateFormFromProjectFile(const AClassName,
                                                         AName: string):boolean;
    function FormIsCreatedInProjectFile(const AClassname, AName:string):boolean;
    
    // uses section
    function UnitIsUsed(const ShortUnitName:string):boolean;
    
    // resources
    function GetMainResourceFilename(AnUnitInfo: TUnitInfo): string;
    function GetResourceFile(AnUnitInfo: TUnitInfo; Index:integer):TCodeBuffer;

    // filenames and fileinfo
    function RemoveProjectPathFromFilename(const AFilename: string): string;
    function FileIsInProjectDir(const AFilename: string): boolean;
    procedure GetVirtualDefines(DefTree: TDefineTree; DirDef: TDirectoryDefines);
    function SearchFile(const Filename,SearchPaths,InitialDir:string):string;
    procedure ShortenFilename(var AFilename: string);
    procedure LongenFilename(var AFilename: string);

    // bookmarks
    procedure SetBookmark(AnUnitInfo: TUnitInfo; X,Y,ID: integer);
    procedure MergeBookmarks(AnUnitInfo: TUnitInfo);
    
    // dependencies
    function FindDependencyByName(const PackageName: string): TPkgDependency;
    function RequiredDepByIndex(Index: integer): TPkgDependency;
    function RemovedDepByIndex(Index: integer): TPkgDependency;
    procedure AddRequiredDependency(Dependency: TPkgDependency);
    procedure RemoveRequiredDependency(Dependency: TPkgDependency);
    procedure DeleteRequiredDependency(Dependency: TPkgDependency);
    procedure DeleteRemovedDependency(Dependency: TPkgDependency);
    procedure RemoveRemovedDependency(Dependency: TPkgDependency);
    procedure ReaddRemovedDependency(Dependency: TPkgDependency);
    procedure MoveRequiredDependencyUp(Dependency: TPkgDependency);
    procedure MoveRequiredDependencyDown(Dependency: TPkgDependency);
    function Requires(APackage: TLazPackage): boolean;
    procedure GetAllRequiredPackages(var List: TList);
  public
    property ActiveEditorIndexAtStart: integer read fActiveEditorIndexAtStart
                                               write fActiveEditorIndexAtStart;
    property AutoCreateForms: boolean
                                   read FAutoCreateForms write FAutoCreateForms;
    property AutoOpenDesignerFormsDisabled: boolean read FAutoOpenDesignerFormsDisabled write SetAutoOpenDesignerFormsDisabled;
    property Bookmarks: TProjectBookmarkList read fBookmarks write fBookmarks;
    property CompilerOptions: TProjectCompilerOptions
                                   read fCompilerOptions write fCompilerOptions;
    property DefineTemplates: TProjectDefineTemplates read FDefineTemplates;
    property Destroying: boolean read fDestroying;
    property FirstAutoRevertLockedUnit: TUnitInfo read GetFirstAutoRevertLockedUnit;
    property FirstLoadedUnit: TUnitInfo read GetFirstLoadedUnit;
    property FirstPartOfProject: TUnitInfo read GetFirstPartOfProject;
    property FirstRemovedDependency: TPkgDependency
                                                   read FFirstRemovedDependency;
    property FirstRequiredDependency: TPkgDependency
                                                  read FFirstRequiredDependency;
    property FirstUnitWithEditorIndex: TUnitInfo read GetFirstUnitWithEditorIndex;
    property FirstUnitWithComponent: TUnitInfo read GetFirstUnitWithComponent;
    property Flags: TProjectFlags read FFlags write SetFlags;
    property IconPath: String read fIconPath write fIconPath;
    property JumpHistory: TProjectJumpHistory
                                           read fJumpHistory write fJumpHistory;
    property MainFilename: String read GetMainFilename;
    property MainUnitID: Integer read fMainUnitID write SetMainUnitID;
    property MainUnitInfo: TUnitInfo read GetMainUnitInfo;
    property Modified: boolean read fModified write SetModified;
    property OnBeginUpdate: TNotifyEvent read FOnBeginUpdate write FOnBeginUpdate;
    property OnEndUpdate: TEndUpdateProjectEvent read FOnEndUpdate write FOnEndUpdate;
    property OnFileBackup: TOnFileBackup read fOnFileBackup write fOnFileBackup;
    property OnSaveProjectInfo: TOnSaveProjectInfo read FOnSaveProjectInfo
                                                   write FOnSaveProjectInfo;
    property OnLoadProjectInfo: TOnLoadProjectInfo read FOnLoadProjectInfo
                                                   write FOnLoadProjectInfo;
    property ProjectDirectory: string read fProjectDirectory;
    property ProjectInfoFile: string
                               read GetProjectInfoFile write SetProjectInfoFile;
    property ProjectType: TProjectType read fProjectType write fProjectType;
    property PublishOptions: TPublishProjectOptions
                                     read fPublishOptions write fPublishOptions;
    property RunParameterOptions: TRunParamsOptions read fRunParameterOptions;
    property TargetFileExt: String read fTargetFileExt write fTargetFileExt;
    property TargetFilename: string
                                 read GetTargetFilename write SetTargetFilename;
    property Title: String read fTitle write fTitle;
    property Units[Index: integer]:TUnitInfo read GetUnits write SetUnits;
    property UpdateLock: integer read FUpdateLock;
  end;

const
  ResourceFileExt = '.lrs';

  ProjectTypeNames : array[TProjectType] of string = (
      'Application', 'Program', 'Custom program', 'CGI Application'
    );

  ProjectTypeDescriptions : array[TProjectType] of string = (
      // ptApplication
      'Application'#13
      +'A graphical lcl/freepascal program. The program file is '
      +'automatically maintained by lazarus.'#13

      // ptProgram
      ,'Program:'#13
      +'A freepascal program. The program file is automatically '
      +'maintained by lazarus.'

      // ptCustomProgram
      ,'Custom program:'#13
      +'A freepascal program.'

      // ptCGIApplication
      ,'CGI Application'#13
      +'A cgi freepascal program. The program file is '
      +'automatically maintained by lazarus.'#13
    );

  ProjectDefaultExt : array[TProjectType] of string = (
      '.lpr','.pas','.pas', 'pas'
    );
    
  UnitTypeDefaultExt: array[TNewUnitType] of string = (
      '.pas', '.pas', '.pas', '.pas', '.pas', '.txt', '.pas'
    );

  DefaultProjectFlags = [pfSaveClosedUnits];
  ProjectFlagNames : array[TProjectFlag] of string = (
      'SaveClosedFiles', 'SaveOnlyProjectUnits'
    );

function ProjectTypeNameToType(const s:string): TProjectType;
function ProjectFlagsToStr(Flags: TProjectFlags): string;


implementation

const
  ProjectInfoFileVersion = 3;

function ProjectFlagsToStr(Flags: TProjectFlags): string;
var f: TProjectFlag;
begin
  Result:='';
  for f:=Low(TProjectFlag) to High(TProjectFlag) do begin
    if f in Flags then begin
      if Result='' then Result:=Result+',';
      Result:=Result+ProjectFlagNames[f];
    end;
  end;
end;

function ProjectTypeNameToType(const s:string): TProjectType;
begin
  for Result:=Low(TProjectType) to High(TProjectType) do
    if (lowercase(ProjectTypeNames[Result])=lowercase(s)) then exit;
  Result:=ptCustomProgram;
end;

{------------------------------------------------------------------------------
                              TUnitInfo Class
 ------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
  TUnitInfo Constructor
 ------------------------------------------------------------------------------}
constructor TUnitInfo.Create(ACodeBuffer: TCodeBuffer);
begin
  inherited Create;
  Assert(False, 'Project Unit Info Class Created');
  FBookmarks:=TFileBookmarks.Create;
  Clear;
  Source := ACodeBuffer;
  if Source=nil then
    FFileName:='';
end;

{------------------------------------------------------------------------------
  TUnitInfo Destructor
 ------------------------------------------------------------------------------}
destructor TUnitInfo.Destroy;
begin
  Source:=nil;
  FreeAndNil(FBookmarks);
  Project:=nil;
  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TUnitInfo WriteUnitSource
 ------------------------------------------------------------------------------}
function TUnitInfo.WriteUnitSource: TModalResult;
var
  ACaption:string;
  AText:string;
begin
  if fSource=nil then begin
    Result:=mrOk;
    exit;
  end;
  if Assigned(fOnFileBackup) then begin
    Result:=fOnFileBackup(fFilename,IsPartOfProject);
    if Result=mrAbort then exit;
  end;
  repeat
    if not fSource.Save then begin
      ACaption:='Write error';
      AText:='Unable to write file "'+Filename+'"!';
      Result:=Application.MessageBox(PChar(AText),PChar(ACaption)
         ,MB_ABORTRETRYIGNORE);
      if Result=mrAbort then exit;
      if Result=mrIgnore then Result:=mrOk;
    end else begin
      Result:=mrOk;
      FIgnoreFileDateOnDiskValid:=true;
    end;
  until Result<>mrRetry;
  Result:=mrOk;
end;

function TUnitInfo.WriteUnitSourceToFile(const AFileName: string): TModalResult;
var
  ACaption:string;
  AText:string;
begin
  if fSource=nil then begin
    Result:=mrOk;
    exit;
  end;
  if Assigned(fOnFileBackup) then begin
    Result:=fOnFileBackup(fFilename,false);
    if Result=mrAbort then exit;
  end;
  repeat
    if not fSource.SaveToFile(AFileName) then begin
      ACaption:='Write error';
      AText:='Unable to write file "'+AFilename+'"!';
      Result:=Application.MessageBox(PChar(AText),PChar(ACaption)
         ,MB_ABORTRETRYIGNORE);
      if Result=mrAbort then exit;
      if Result=mrIgnore then Result:=mrOk;
    end else
      Result:=mrOk;
  until Result<>mrRetry;
  Result:=mrOk;
end;

{------------------------------------------------------------------------------
  TUnitInfo ReadUnitSource
 ------------------------------------------------------------------------------}
function TUnitInfo.ReadUnitSource(ReadUnitName,Revert:boolean): TModalResult;
var 
  ACaption:string;
  AText:string;
  NewSource: TCodeBuffer;
begin
  repeat
    NewSource:=CodeToolBoss.LoadFile(fFilename,true,Revert);
    if NewSource=nil then begin
      ACaption:='Read error';
      AText:='Unable to read file "'+fFilename+'"!';
      Result:=Application.MessageBox(PChar(AText),PChar(ACaption)
         ,MB_ABORTRETRYIGNORE);
      if Result in [mrAbort,mrIgnore] then
        exit;
    end else begin
      Source:=NewSource;
      FIgnoreFileDateOnDiskValid:=true;
      Result:=mrOk;
    end;
  until Result<>mrRetry;
  if ReadUnitName then 
    fUnitName:=CodeToolBoss.GetSourceName(fSource,false);
  Result:=mrOk;
end;

procedure TUnitInfo.ReadUnitNameFromSource(TryCache: boolean);
var
  NewUnitName: String;
begin
  if TryCache then begin
    NewUnitName:=CodeToolBoss.GetCachedSourceName(Source);
    if NewUnitName<>'' then begin
      fUnitName:=NewUnitName;
      exit;
    end;
  end;
  fUnitName:=CodeToolBoss.GetSourceName(fSource,false);
end;

function TUnitInfo.CreateUnitName: string;
begin
  Result:=UnitName;
  if (Result='') and FilenameIsPascalUnit(Filename) then
    Result:=ExtractFilenameOnly(Filename);
end;

{------------------------------------------------------------------------------
  TUnitInfo Clear
 ------------------------------------------------------------------------------}
procedure TUnitInfo.Clear;
begin
  FBookmarks.Clear;
  FBuildFileIfActive:=false;
  fComponent := nil;
  fComponentName := '';
  fComponentResourceName := '';
  fCursorPos.X := -1;
  fCursorPos.Y := -1;
  fCustomHighlighter := false;
  fEditorIndex := -1;
  fFilename := '';
  fFileReadOnly := false;
  fHasResources := false;
  FIgnoreFileDateOnDiskValid:=false;
  fIsPartOfProject := false;
  fModified := false;
  FRunFileIfActive:=false;
  fSyntaxHighlighter := lshText;
  fTopLine := -1;
  fUnitName := '';
  fUsageCount:=-1;
  fUserReadOnly := false;
  if fSource<>nil then fSource.Clear;
  Loaded := false;
end;


{------------------------------------------------------------------------------
  TUnitInfo SaveToXMLConfig
 ------------------------------------------------------------------------------}
procedure TUnitInfo.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
var AFilename:string;
begin
  XMLConfig.SetDeleteValue(Path+'CursorPos/X',fCursorPos.X,-1);
  XMLConfig.SetDeleteValue(Path+'CursorPos/Y',fCursorPos.Y,-1);
  XMLConfig.SetDeleteValue(Path+'EditorIndex/Value',fEditorIndex,-1);
  AFilename:=Filename;
  if Assigned(fOnLoadSaveFilename) then
    fOnLoadSaveFilename(AFilename,false);
  XMLConfig.SetValue(Path+'Filename/Value',AFilename);
  XMLConfig.SetDeleteValue(Path+'BuildFileIfActive/Value',
                           FBuildFileIfActive,false);
  XMLConfig.SetDeleteValue(Path+'RunFileIfActive/Value',
                           FRunFileIfActive,false);
  XMLConfig.SetDeleteValue(Path+'ComponentName/Value',fComponentName,'');
  XMLConfig.SetDeleteValue(Path+'HasResources/Value',fHasResources,false);
  XMLConfig.SetDeleteValue(Path+'IsPartOfProject/Value',fIsPartOfProject,false);
  XMLConfig.SetDeleteValue(Path+'Loaded/Value',fLoaded,false);
  XMLConfig.SetDeleteValue(Path+'ReadOnly/Value',fUserReadOnly,false);
  AFilename:=FResourceFilename;
  if Assigned(fOnLoadSaveFilename) then
    fOnLoadSaveFilename(AFilename,false);
  XMLConfig.SetDeleteValue(Path+'ResourceFilename/Value',AFilename,'');
  XMLConfig.SetDeleteValue(Path+'SyntaxHighlighter/Value',
     LazSyntaxHighlighterNames[fSyntaxHighlighter],
     LazSyntaxHighlighterNames[lshFreePascal]);
  XMLConfig.SetDeleteValue(Path+'TopLine/Value',fTopLine,-1);
  XMLConfig.SetDeleteValue(Path+'UnitName/Value',fUnitName,'');
  XMLConfig.SetDeleteValue(Path+'UsageCount/Value',RoundToInt(fUsageCount),-1);
  FBookmarks.SaveToXMLConfig(XMLConfig,Path+'Bookmarks/');
end;

{------------------------------------------------------------------------------
  TUnitInfo LoadFromXMLConfig
 ------------------------------------------------------------------------------}
procedure TUnitInfo.LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
var AFilename: string;
begin
  CursorPos.X:=XMLConfig.GetValue(Path+'CursorPos/X',-1);
  CursorPos.Y:=XMLConfig.GetValue(Path+'CursorPos/Y',-1);
  EditorIndex:=XMLConfig.GetValue(Path+'EditorIndex/Value',-1);
  AFilename:=XMLConfig.GetValue(Path+'Filename/Value','');
  if Assigned(fOnLoadSaveFilename) then
    fOnLoadSaveFilename(AFilename,true);
  fFilename:=AFilename;
  FBuildFileIfActive:=XMLConfig.GetValue(Path+'BuildFileIfActive/Value',
                                           false);
  FRunFileIfActive:=XMLConfig.GetValue(Path+'RunFileIfActive/Value',false);
  fComponentName:=XMLConfig.GetValue(Path+'ComponentName/Value','');
  if fComponentName='' then
    fComponentName:=XMLConfig.GetValue(Path+'FormName/Value','');
  HasResources:=XMLConfig.GetValue(Path+'HasResources/Value',false);
  IsPartOfProject:=XMLConfig.GetValue(Path+'IsPartOfProject/Value',false);
  Loaded:=XMLConfig.GetValue(Path+'Loaded/Value',false);
  fUserReadOnly:=XMLConfig.GetValue(Path+'ReadOnly/Value',false);
  AFilename:=XMLConfig.GetValue(Path+'ResourceFilename/Value','');
  if (AFilename<>'') and Assigned(fOnLoadSaveFilename) then
    fOnLoadSaveFilename(AFilename,true);
  FResourceFilename:=AFilename;
  if (FResourceFilename<>'')
  and (FResourceFilename[length(FResourceFilename)]=PathDelim) then
    FResourceFilename:='';
  fSyntaxHighlighter:=StrToLazSyntaxHighlighter(XMLConfig.GetValue(
       Path+'SyntaxHighlighter/Value',''));
  fTopLine:=XMLConfig.GetValue(Path+'TopLine/Value',-1);
  UnitName:=XMLConfig.GetValue(Path+'UnitName/Value','');
  fUsageCount:=XMLConfig.GetValue(Path+'UsageCount/Value',-1);
  if fUsageCount<1 then begin
    UpdateUsageCount(uuIsLoaded,1);
    if IsPartOfProject then
      UpdateUsageCount(uuIsPartOfProject,1);
  end;
  FBookmarks.LoadFromXMLConfig(XMLConfig,Path+'Bookmarks/');
end;

procedure TUnitInfo.SetUnitName(const NewUnitName:string);
var Allowed:boolean;
begin
  if (fUnitName<>NewUnitName) and (NewUnitName<>'') then begin
    Allowed:=true;
    if Assigned(fOnUnitNameChange) then
      fOnUnitNameChange(Self,fUnitName,NewUnitName,false,Allowed);
    // (ignore Allowed)
    if (fSource<>nil) then begin
      CodeToolBoss.RenameSource(fSource,NewUnitName);
    end;
    fUnitName:=NewUnitName;
    if Project<>nil then Project.Modified:=true;
  end;
end;

procedure TUnitInfo.UpdateList(ListType: TUnitInfoList; Add: boolean);
begin
  if Project<>nil then begin
    if Add then
      Project.AddToList(Self,ListType)
    else
      Project.RemoveFromList(Self,ListType);
  end else begin
    fNext[ListType]:=nil;
    fPrev[ListType]:=nil;
  end;
end;

function TUnitInfo.GetFileName: string;
begin
  if fSource<>nil then Result:=fSource.Filename
  else Result:=fFileName;
end;

function TUnitInfo.IsVirtual: boolean;
begin
  if fSource<>nil then
    Result:=fSource.IsVirtual
  else
    Result:=(fFileName<>ExpandFileName(fFileName));
end;

function TUnitInfo.IsMainUnit: boolean;
begin
  Result:=(Project<>nil) and (Project.MainUnitInfo=Self);
end;

procedure TUnitInfo.IncreaseAutoRevertLock;
begin
  inc(fAutoRevertLockCount);
  if (fAutoRevertLockCount=1) then begin
    // activate lock
    if (Source<>nil) then
      Source.LockAutoDiskRevert;
    if Project<>nil then
      Project.AddToOrRemoveFromAutoRevertLockedList(Self);
  end;
end;

procedure TUnitInfo.DecreaseAutoRevertLock;
begin
  dec(fAutoRevertLockCount);
  if (fAutoRevertLockCount=0) then begin
    // deactivate lock
    if (Source<>nil) then
      Source.LockAutoDiskRevert;
    if Project<>nil then
      Project.AddToOrRemoveFromAutoRevertLockedList(Self);
  end;
end;

function TUnitInfo.IsAutoRevertLocked: boolean;
begin
  Result:=fAutoRevertLockCount>0;
end;

function TUnitInfo.ChangedOnDisk(CompareOnlyLoadSaveTime: boolean): boolean;
begin
  Result:=(Source<>nil) and (Source.FileOnDiskHasChanged);
  if Result
  and (not CompareOnlyLoadSaveTime)
  and FIgnoreFileDateOnDiskValid
  and (FIgnoreFileDateOnDisk=Source.FileDateOnDisk) then
    Result:=false;
  if (not IsVirtual) and FileExists(Filename) then
    FileReadOnly:=not FileIsWritable(Filename)
  else
    FileReadOnly:=false;
end;

procedure TUnitInfo.IgnoreCurrentFileDateOnDisk;
begin
  if Source<>nil then begin
    FIgnoreFileDateOnDiskValid:=true;
    FIgnoreFileDateOnDisk:=Source.FileDateOnDisk;
  end
end;

function TUnitInfo.ShortFilename: string;
begin
  if Project<>nil then begin
    Result:=Project.RemoveProjectPathFromFilename(Filename);
  end else begin
    Result:=Filename;
  end;
end;

function TUnitInfo.NeedsSaveToDisk: boolean;
begin
  Result:=IsVirtual or Modified or ChangedOnDisk(true)
          or (not FileExists(Filename));
end;

procedure TUnitInfo.UpdateUsageCount(Min, IfBelowThis, IncIfBelow: extended);
begin
  if fUsageCount<IfBelowThis then fUsageCount:=fUsageCount+IncIfBelow;
  if fUsageCount<Min then fUsageCount:=Min;
end;

procedure TUnitInfo.UpdateUsageCount(TheUsage: TUnitUsage; Factor: extended);
begin
  case TheUsage of
  uuIsPartOfProject: UpdateUsageCount(20,200,2*Factor);
  uuIsLoaded:        UpdateUsageCount(10,100,1*Factor);
  uuIsModified:      UpdateUsageCount(10,0,0);
  uuNotUsed:         fUsageCount:=fUsageCount-(Factor/5);
  end;
end;

function TUnitInfo.ReadOnly: boolean;
begin
  Result:=UserReadOnly or FileReadOnly;
end;

procedure TUnitInfo.SetSource(ABuffer: TCodeBuffer);
begin
  if fSource=ABuffer then exit;
  if (fSource<>nil) and IsAutoRevertLocked then
    fSource.UnlockAutoDiskRevert;
  fSource:=ABuffer;
  FIgnoreFileDateOnDiskValid:=false;
  if (fSource<>nil) then begin
    if IsAutoRevertLocked then
      fSource.LockAutoDiskRevert;
    fFileName:=fSource.FileName;
    if (fProject<>nil) and (fProject.MainUnitInfo=Self) then
      fProject.MainSourceFilenameChanged;
  end;
end;

procedure TUnitInfo.SetUserReadOnly(const NewValue: boolean);
begin
  fUserReadOnly:=NewValue;
  if fSource<>nil then
    fSource.ReadOnly:=ReadOnly;
end;

procedure TUnitInfo.CreateStartCode(NewUnitType: TNewUnitType;
  const NewUnitName: string);
var AResourceFilename :string;
  NewSource, LE: string;
  
  function Beautified(const s: string): string;
  begin
    Result:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.
                  BeautifyStatement(s,0);
  end;
  
begin
  if fSource=nil then exit;
  NewSource:='';
  LE:=LineEnding;
  if NewUnitType in [nuForm,nuUnit,nuDataModule,nuCGIDataModule] then begin
    fUnitName:=NewUnitName;
    AResourceFilename:=fUnitName+ResourceFileExt;
    NewSource:=Beautified(
       'unit '+fUnitName+';'+LE
      +LE
      +'{$mode objfpc}{$H+}'+LE
      +LE
      +'interface'+LE
      +LE
      +'uses'+LE);
    case NewUnitType of

     nuUnit:
      begin
        NewSource:=NewSource+Beautified(
          '  Classes, SysUtils;'+LE
          +LE
          +'implementation'+LE);
      end;

     nuForm, nuDataModule, nuCGIDataModule:
      begin
        if NewUnitType=nuForm then
          NewSource:=NewSource+Beautified(
            '  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs;'+LE
            +LE
            +'type'+LE)
        else if NewUnitType=nuDataModule then
          NewSource:=NewSource+Beautified(
            '  Classes, SysUtils, LResources, Forms, Controls, Dialogs;'+LE
            +LE
            +'type'+LE)
        else if NewUnitType=nuCGIDataModule then
          NewSource:=NewSource+Beautified(
            '  Classes, SysUtils, LResources, cgiModules;'+LE
            +LE
            +'type'+LE);
        if NewUnitType=nuForm then
          NewSource:=NewSource+Beautified(
            +'  T'+fComponentName+' = class(TForm)'+LE)
        else if NewUnitType=nuDataModule then
          NewSource:=NewSource+Beautified(
            +'  T'+fComponentName+' = class(TDataModule)'+LE)
        else if NewUnitType=nuCGIDataModule then
          NewSource:=NewSource+Beautified(
            +'  T'+fComponentName+' = class(TCGIDataModule)'+LE);
        NewSource:=NewSource+Beautified(
          '  private'+LE);
        NewSource:=NewSource
          +'    { private declarations }'+LE;
        NewSource:=NewSource+Beautified(
          +'  public'+LE);
        NewSource:=NewSource
          +'    { public declarations }'+LE;
        NewSource:=NewSource+Beautified(
          +'  end;'+LE
          +LE
          +'var'+LE
          +'  '+fComponentName+': T'+fComponentName+';'+LE
          +LE
          +'implementation'+LE
          +LE
          +'initialization'+LE);
        NewSource:=NewSource
          +'  {$I '+AResourceFilename+'}'+LE;
      end;

    end;
    NewSource:=NewSource+Beautified(
      +LE
      +'end.'+LE
      +LE);

  end else if NewUnitType in [nuCustomProgram] then begin
    NewSource:=NewSource+Beautified(
      +'program CustomProgram;'+LE
      +LE
      +'{$mode objfpc}{$H+}'+LE
      +LE
      +'uses'+LE
      +'  Classes, SysUtils;'+LE
      +LE
      +'begin'+LE
      +'end.'+LE
      +LE);
  end;
  fSource.Source:=NewSource;
  fModified:=true;
end;

function TUnitInfo.GetHasResources:boolean;
begin
  Result:=fHasResources or (ComponentName<>'');
end;

function TUnitInfo.GetNextAutoRevertLockedUnit: TUnitInfo;
begin
  Result:=fNext[uilAutoRevertLocked];
end;

function TUnitInfo.GetNextLoadedUnit: TUnitInfo;
begin
  Result:=fNext[uilLoaded];
end;

function TUnitInfo.GetNextPartOfProject: TUnitInfo;
begin
  Result:=fNext[uilPartOfProject];
end;

function TUnitInfo.GetNextUnitWithComponent: TUnitInfo;
begin
  Result:=fNext[uilWithComponent];
end;

function TUnitInfo.GetNextUnitWithEditorIndex: TUnitInfo;
begin
  Result:=fNext[uilWithEditorIndex];
end;

function TUnitInfo.GetPrevAutoRevertLockedUnit: TUnitInfo;
begin
  Result:=fPrev[uilAutoRevertLocked];
end;

function TUnitInfo.GetPrevLoadedUnit: TUnitInfo;
begin
  Result:=fPrev[uilLoaded];
end;

function TUnitInfo.GetPrevPartOfProject: TUnitInfo;
begin
  Result:=fPrev[uilPartOfProject];
end;

function TUnitInfo.GetPrevUnitWithComponent: TUnitInfo;
begin
  Result:=fPrev[uilWithComponent];
end;

function TUnitInfo.GetPrevUnitWithEditorIndex: TUnitInfo;
begin
  Result:=fPrev[uilWithEditorIndex];
end;

procedure TUnitInfo.SetBuildFileIfActive(const AValue: boolean);
begin
  if FBuildFileIfActive=AValue then exit;
  FBuildFileIfActive:=AValue;
  Modified:=true;
end;

procedure TUnitInfo.SetEditorIndex(const AValue: integer);
begin
  if fEditorIndex=AValue then exit;
  fEditorIndex:=AValue;
  UpdateList(uilWithEditorIndex,fEditorIndex>=0);
end;

procedure TUnitInfo.SetFileReadOnly(const AValue: Boolean);
begin
  if fFileReadOnly=AValue then exit;
  fFileReadOnly:=AValue;
  if fSource<>nil then
    fSource.ReadOnly:=ReadOnly;
end;

procedure TUnitInfo.SetComponent(const AValue: TComponent);
begin
  if fComponent=AValue then exit;
  fComponent:=AValue;
  UpdateList(uilWithComponent,fComponent<>nil);
end;

procedure TUnitInfo.SetIsPartOfProject(const AValue: boolean);
begin
  if fIsPartOfProject=AValue then exit;
  if Project<>nil then Project.BeginUpdate(true);
  fIsPartOfProject:=AValue;
  UpdateList(uilPartOfProject,fIsPartOfProject);
  if fIsPartOfProject then UpdateUsageCount(uuIsPartOfProject,0);
  if Project<>nil then Project.EndUpdate;
end;

{-------------------------------------------------------------------------------
  procedure TUnitInfo.SetLoaded(const AValue: Boolean);

  Loaded is a flag, that is set, when a unit has finished loading into the
  editor. It is saved to the project info file and a loaded unit will be
  reloaded, when the project is opened.
-------------------------------------------------------------------------------}
procedure TUnitInfo.SetLoaded(const AValue: Boolean);
begin
  if fLoaded=AValue then exit;
  fLoaded:=AValue;
  if fLoaded then begin
    IncreaseAutoRevertLock;
    UpdateUsageCount(uuIsLoaded,0);
  end else begin
    DecreaseAutoRevertLock;
  end;
end;

procedure TUnitInfo.SetProject(const AValue: TProject);
var
  ListType: TUnitInfoList;
begin
  if FProject=AValue then exit;
  if FProject<>nil then begin
    for ListType:=Low(TUnitInfoList) to High(TUnitInfoList) do
      Project.RemoveFromList(Self,ListType);
  end;
  FProject:=AValue;
  if FProject<>nil then begin
    if EditorIndex>=0 then Project.AddToList(Self,uilWithEditorIndex);
    if Component<>nil then Project.AddToList(Self,uilWithComponent);
    if Loaded then Project.AddToList(Self,uilLoaded);
    if IsAutoRevertLocked then Project.AddToList(Self,uilAutoRevertLocked);
    if IsPartOfProject then Project.AddToList(Self,uilPartOfProject);
  end;
end;

procedure TUnitInfo.SetRunFileIfActive(const AValue: boolean);
begin
  if FRunFileIfActive=AValue then exit;
  FRunFileIfActive:=AValue;
  Modified:=true;
end;


{------------------------------------------------------------------------------
                              TProject Class
 ------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
  TProject Constructor
 ------------------------------------------------------------------------------}
constructor TProject.Create(TheProjectType: TProjectType);
var PrgUnitInfo: TUnitInfo;
  NewSource: TStringList;
  NewPrgBuf: TCodeBuffer;
begin
  inherited Create;

  Assert(False, 'Trace:Project Class Created');
  xmlconfig := nil;

  fProjectType:=TheProjectType;

  fActiveEditorIndexAtStart := -1;
  FAutoCreateForms := true;
  fBookmarks := TProjectBookmarkList.Create;
  fCompilerOptions := TProjectCompilerOptions.Create(Self);
  FDefineTemplates:=TProjectDefineTemplates.Create(Self);
  FFlags:=DefaultProjectFlags;
  fIconPath := '';
  fJumpHistory:=TProjectJumpHistory.Create;
  fJumpHistory.OnCheckPosition:=@JumpHistoryCheckPosition;
  fJumpHistory.OnLoadSaveFilename:=@OnLoadSaveFilename;
  fMainUnitID := -1;
  fModified := false;
  fProjectInfoFile := '';
  UpdateProjectDirectory;
  fPublishOptions:=TPublishProjectOptions.Create(Self);
  fRunParameterOptions:=TRunParamsOptions.Create;
  fTargetFileExt := GetDefaultExecutableExt;
  fTitle := '';
  fUnitList := TList.Create;  // list of TUnitInfo

  // create program source
  NewSource:=TStringList.Create;
  case fProjectType of
   ptProgram, ptApplication, ptCustomProgram, ptCGIApplication:
    begin
      NewPrgBuf:=CodeToolBoss.CreateFile(
        'project1'+ProjectDefaultExt[fProjectType]);
      PrgUnitInfo:=TUnitInfo.Create(NewPrgBuf);
      PrgUnitInfo.IsPartOfProject:=true;
      PrgUnitInfo.SyntaxHighlighter:=
        ExtensionToLazSyntaxHighlighter(ProjectDefaultExt[fProjectType]);
      AddUnit(PrgUnitInfo,false);
      MainUnitID:=0;
      with NewSource do begin
        Add('program Project1;');
        Add('');
        Add('{$mode objfpc}{$H+}');
        // This results in crashing programs, when stdout is not open
        //if fProjectType in [ptApplication] then
        //  Add('{$AppType Gui} // for win32 applications');
        Add('');
        Add('uses');
        case fProjectType of
          ptProgram, ptCustomProgram:
            Add('  Classes;');
          ptApplication:
            begin
              Add('  Interfaces,');
              Add('  Forms;');
            end;
          ptCGIApplication:
            Add('  cgiModules;');
        else
          Add('  { add your units here };');
        end;
        Add('');
        Add('begin');
        case fProjectType of
         ptApplication:
          begin
            Add('  Application.Initialize;');
            Add('  Application.Run;');
         end;
        end;
        Add('end.');
        Add('');
      end;
      MainUnitInfo.Source.Assign(NewSource);
    end;
  end;
  NewSource.Free;
end;

{------------------------------------------------------------------------------
  TProject Destructor
 ------------------------------------------------------------------------------}
destructor TProject.Destroy;
begin
  fDestroying:=true;
  Clear;
  FreeThenNil(fBookmarks);
  FreeThenNil(xmlconfig);
  FreeThenNil(fUnitList);
  FreeThenNil(fJumpHistory);
  FreeThenNil(fPublishOptions);
  FreeThenNil(fRunParameterOptions);
  FreeThenNil(fCompilerOptions);
  FreeThenNil(FDefineTemplates);

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TProject WriteProject
 ------------------------------------------------------------------------------}
function TProject.WriteProject(ProjectWriteFlags: TProjectWriteFlags;
  const OverrideProjectInfoFile: string): TModalResult;
var
  confPath: String;
  Path: String;

  procedure SaveFlags;
  var f: TProjectFlag;
  begin
    for f:=Low(TProjectFlag) to High(TProjectFlag) do begin
      xmlconfig.SetDeleteValue(Path+'General/Flags/'
            +ProjectFlagNames[f]+'/Value', f in Flags,f in DefaultProjectFlags);
    end;
  end;
  
  procedure UpdateUsageCounts;
  var
    UnitUsageCount: extended;
    DiffTime: TDateTime;
    i: Integer;
  begin
    UnitUsageCount:=0;
    if CompareFileNames(confPath,fLastReadLPIFilename)=0 then begin
      DiffTime:=Now-fLastReadLPIFileDate;
      if DiffTime>0 then begin
        UnitUsageCount:= DiffTime*24; // one step every hour
      end;
      fLastReadLPIFileDate:=Now;
    end;
    for i:=0 to UnitCount-1 do begin
      if Units[i].IsPartOfProject then
        Units[i].UpdateUsageCount(uuIsPartOfProject,UnitUsageCount)
      else if Units[i].Loaded then
        Units[i].UpdateUsageCount(uuIsLoaded,UnitUsageCount)
      else
        Units[i].UpdateUsageCount(uuNotUsed,UnitUsageCount);
    end;
  end;
  
  function UnitMustBeSaved(i: integer): boolean;
  begin
    Result:=false;
    if not Units[i].IsPartOfProject then begin
      if (pfSaveOnlyProjectUnits in Flags) then exit;
      if (pwfSaveOnlyProjectUnits in ProjectWriteFlags) then exit;
      if (not Units[i].Loaded) then begin
        if (not (pfSaveClosedUnits in Flags)) then exit;
        if (pwfDontSaveClosedUnits in ProjectWriteFlags) then exit;
        if Units[i].fUsageCount<=0 then exit;
      end;
    end;
    Result:=true;
  end;
  
  procedure SaveUnits;
  var i, SaveUnitCount: integer;
  begin
    SaveUnitCount:=0;
    for i:=0 to UnitCount-1 do begin
      if UnitMustBeSaved(i) then begin
        Units[i].SaveToXMLConfig(
          xmlconfig,Path+'Units/Unit'+IntToStr(SaveUnitCount)+'/');
        inc(SaveUnitCount);
      end;
    end;
    xmlconfig.SetDeleteValue(Path+'Units/Count',SaveUnitCount,0);
  end;


begin
  Result := mrCancel;

  if OverrideProjectInfoFile<>'' then
    confPath := OverrideProjectInfoFile
  else
    confPath := ProjectInfoFile;
  if Assigned(fOnFileBackup) then begin
    Result:=fOnFileBackup(confPath,true);
    if Result=mrAbort then exit;
  end;
  confPath:=SetDirSeparators(confPath);
  
  UpdateUsageCounts;

  repeat
    try
      xmlconfig := TXMLConfig.CreateClean(confPath);
    except
      on E: Exception do begin
        writeln('ERROR: ',E.Message);
        MessageDlg('Write error',
          'Unable to write the project info file'#13
          +'"'+ProjectInfoFile+'".'#13
          +'Error: '+E.Message
          ,mtError,[mbOk],0);
        Result:=mrCancel;
        exit;
      end;
    end;

    try
      Path:='ProjectOptions/';
      xmlconfig.SetValue(Path+'Version/Value',ProjectInfoFileVersion);
      xmlconfig.SetDeleteValue(Path+'General/ProjectType/Value',
          ProjectTypeNames[ProjectType],'');
      SaveFlags;
      xmlconfig.SetDeleteValue(Path+'General/MainUnit/Value', MainUnitID,-1);
      xmlconfig.SetDeleteValue(Path+'General/ActiveEditorIndexAtStart/Value'
          ,ActiveEditorIndexAtStart,-1);
      xmlconfig.SetDeleteValue(Path+'General/AutoCreateForms/Value'
          ,AutoCreateForms,true);
      xmlconfig.SetDeleteValue(Path+'General/IconPath/Value',
           IconPath,'');
      xmlconfig.SetValue(Path+'General/TargetFileExt/Value'
          ,TargetFileExt);
      xmlconfig.SetDeleteValue(Path+'General/Title/Value', Title,'');
      if not (pfSaveOnlyProjectUnits in Flags) then begin
        fJumpHistory.DeleteInvalidPositions;
        fJumpHistory.SaveToXMLConfig(xmlconfig,Path);
      end;

      SaveUnits;

      // Save the compiler options
      CompilerOptions.SaveToXMLConfig(XMLConfig,'CompilerOptions/');
      
      // save the Publish Options
      PublishOptions.SaveToXMLConfig(xmlconfig,Path+'PublishOptions/');

      // save the Run Parameter Options
      RunParameterOptions.Save(xmlconfig,Path);
      
      // save dependencies
      SavePkgDependencyList(XMLConfig,Path+'RequiredPackages/',
        FFirstRequiredDependency,pdlRequires);
        
      if Assigned(OnSaveProjectInfo) then OnSaveProjectInfo(Self,XMLConfig);

      xmlconfig.Flush;
      Modified:=false;
      Result:=mrOk;
    except
      on E: Exception do begin
        Result:=MessageDlg('Write error','Unable to write to file "'+confPath+'".',
          mtError,[mbRetry,mbAbort],0);
      end;
    end;
    xmlconfig.Free;
    xmlconfig:=nil;
  until Result<>mrRetry;
end;

function TProject.GetDefaultTitle: string;
begin
  Result:=ExtractFilenameOnly(ProjectInfoFile);
end;

function TProject.TitleIsDefault: boolean;
begin
  Result:=(Title='') or (Title=GetDefaultTitle);
end;

{------------------------------------------------------------------------------
  TProject ReadProject
 ------------------------------------------------------------------------------}
function TProject.ReadProject(const LPIFilename: string): TModalResult;

var
  NewUnitInfo: TUnitInfo;
  NewUnitCount,i: integer;
  FileVersion: Integer;
  OldSrcPath: String;
  Path: String;

  procedure LoadCompilerOptions;
  var
    CompOptsPath: String;
  begin
    CompOptsPath:='CompilerOptions/';
    if FileVersion<3 then begin
      // due to an old bug, the XML path can be 'CompilerOptions/' or ''
      if XMLConfig.GetValue('SearchPaths/CompilerPath/Value','')<>'' then
        CompOptsPath:=''
      else if XMLConfig.GetValue(
        'CompilerOptions/SearchPaths/CompilerPath/Value','')<>''
      then
        CompOptsPath:='CompilerOptions/';
    end;
    CompilerOptions.LoadFromXMLConfig(xmlconfig,CompOptsPath);
    if FileVersion<2 then CompilerOptions.SrcPath:=OldSrcPath;
  end;

  procedure LoadFlags;
  var f: TProjectFlag;
  begin
    FFlags:=[];
    for f:=Low(TProjectFlag) to High(TProjectFlag) do begin
      if xmlconfig.GetValue(Path+'General/Flags/'
            +ProjectFlagNames[f]+'/Value', f in DefaultProjectFlags)
      then
        Include(FFlags,f)
      else
        Exclude(FFlags,f);
    end;
  end;

begin
  Result := mrCancel;
  BeginUpdate(true);
  try
    Clear;

    ProjectInfoFile:=LPIFilename;
    try
      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject A reading lpi');{$ENDIF}
      xmlconfig := TXMLConfig.Create(ProjectInfoFile);
      fLastReadLPIFilename:=ProjectInfoFile;
      fLastReadLPIFileDate:=Now;
      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject B done lpi');{$ENDIF}
    except
      MessageDlg('Unable to read the project info file'#13'"'+ProjectInfoFile+'".'
          ,mtError,[mbOk],0);
      Result:=mrCancel;
      exit;
    end;

    try
      Path:='ProjectOptions/';
      fPathDelimChanged:=
        XMLConfig.GetValue(Path+'PathDelim/Value', '/')<>PathDelim;

      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject C reading values');{$ENDIF}
      FileVersion:= XMLConfig.GetValue(Path+'Version/Value',0);
      ProjectType := ProjectTypeNameToType(xmlconfig.GetValue(
         Path+'General/ProjectType/Value', ''));
      LoadFlags;
      MainUnitID := xmlconfig.GetValue(Path+'General/MainUnit/Value', -1);
      ActiveEditorIndexAtStart := xmlconfig.GetValue(
         Path+'General/ActiveEditorIndexAtStart/Value', -1);
      AutoCreateForms := xmlconfig.GetValue(
         Path+'General/AutoCreateForms/Value', true);
      IconPath := xmlconfig.GetValue(Path+'General/IconPath/Value', './');
      TargetFileExt := xmlconfig.GetValue(
         Path+'General/TargetFileExt/Value', GetDefaultExecutableExt);
      Title := xmlconfig.GetValue(Path+'General/Title/Value', '');
      fJumpHistory.LoadFromXMLConfig(xmlconfig,Path+'');
      if FileVersion<2 then
        OldSrcPath := xmlconfig.GetValue(Path+'General/SrcPath/Value','');

      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject D reading units');{$ENDIF}
      NewUnitCount:=xmlconfig.GetValue(Path+'Units/Count',0);
      for i := 0 to NewUnitCount - 1 do begin
        NewUnitInfo:=TUnitInfo.Create(nil);
        AddUnit(NewUnitInfo,false);
        NewUnitInfo.LoadFromXMLConfig(
           xmlconfig,Path+'Units/Unit'+IntToStr(i)+'/');
      end;

      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject E reading comp sets');{$ENDIF}
      // Load the compiler options
      LoadCompilerOptions;

      // load the Publish Options
      PublishOptions.LoadFromXMLConfig(xmlconfig,
                            Path+'PublishOptions/',fPathDelimChanged);

      // load the Run Parameter Options
      RunParameterOptions.Load(xmlconfig,Path,fPathDelimChanged);

      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject update ct boss');{$ENDIF}
      CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'ProjPath']:=
            ProjectDirectory;
      CodeToolBoss.DefineTree.ClearCache;
      
      // load the dependencies
      LoadPkgDependencyList(XMLConfig,Path+'RequiredPackages/',
        FFirstRequiredDependency,pdlRequires,Self,true);

      if Assigned(OnLoadProjectInfo) then OnLoadProjectInfo(Self,XMLConfig);

    finally
      {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject freeing xml');{$ENDIF}
      fPathDelimChanged:=false;
      xmlconfig.Free;
      xmlconfig:=nil;
    end;
  finally
    EndUpdate;
  end;

  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject END');{$ENDIF}
  Result := mrOk;
end;

{------------------------------------------------------------------------------
  TProject AddUnit
 ------------------------------------------------------------------------------}
procedure TProject.AddUnit(AnUnit: TUnitInfo; AddToProjectFile:boolean);
var
  ShortUnitName:string;
  NewIndex: integer;
begin
  if (AnUnit = nil) then exit;
  BeginUpdate(true);
  NewIndex:=UnitCount;
  fUnitList.Add(AnUnit);
  AnUnit.Project:=Self;
  AnUnit.OnFileBackup:=@OnUnitFileBackup;
  AnUnit.OnLoadSaveFilename:=@OnLoadSaveFilename;
  AnUnit.OnUnitNameChange:=@OnUnitNameChange;

  // check if this is the new Main Unit
  if MainUnitID=NewIndex then
    MainUnitInfo.IncreaseAutoRevertLock;

  if AddToProjectFile and (MainUnitID>=0) and (MainUnitID<>NewIndex) then begin
    // add unit to uses section
    ShortUnitName:=AnUnit.UnitName;
    if (ShortUnitName<>'') and (not UnitIsUsed(ShortUnitName)) then
      CodeToolBoss.AddUnitToMainUsesSection(MainUnitInfo.Source,
        ShortUnitName,'');
  end;
  EndUpdate;
  Modified:=true;
end;

{------------------------------------------------------------------------------
  TProject RemoveUnit
 ------------------------------------------------------------------------------}
procedure TProject.RemoveUnit(Index: integer);
var
  OldUnitInfo: TUnitInfo;
begin
  if (Index<0) or (Index>=UnitCount) then begin
    raise Exception.Create('ERROR: TProject.RemoveUnit index out of bounds');
  end;
  if (Index=MainUnitID) then begin
    raise Exception.Create('ERROR: TProject.RemoveUnit index = MainUnit');
  end;
  
  BeginUpdate(true);
  OldUnitInfo:=Units[Index];
  Modified:=true;

  if (MainUnitID>=0) then begin
    // remove unit from uses section and from createforms in program file
    if (OldUnitInfo.IsPartOfProject) then begin
      if (OldUnitInfo.UnitName<>'') then
        CodeToolBoss.RemoveUnitFromAllUsesSections(Units[MainUnitID].Source,
          OldUnitInfo.UnitName);
      if (OldUnitInfo.ComponentName<>'') then
        CodeToolBoss.RemoveCreateFormStatement(Units[MainUnitID].Source,
          OldUnitInfo.ComponentName);
    end;
  end;

  // delete bookmarks of this unit
  if OldUnitInfo.EditorIndex>=0 then
    Bookmarks.DeleteAllWithEditorIndex(OldUnitInfo.EditorIndex);

  // adjust MainUnit
  if MainUnitID>=Index then dec(fMainUnitID);

  // delete unitinfo instance
  OldUnitInfo.Free;
  fUnitList.Delete(Index);
  EndUpdate;
end;

{------------------------------------------------------------------------------
  TProject Clear
 ------------------------------------------------------------------------------}
procedure TProject.Clear;
var i:integer;
begin
  BeginUpdate(true);

  FreeThenNil(xmlconfig);

  // break and free removed dependencies
  while FFirstRemovedDependency<>nil do
    DeleteRemovedDependency(FFirstRemovedDependency);
  // break and free required dependencies
  while FFirstRequiredDependency<>nil do
    DeleteRequiredDependency(FFirstRequiredDependency);

  // delete files
  for i:=0 to UnitCount-1 do Units[i].Free;
  fUnitList.Clear;
  
  fRunParameterOptions.Clear;

  fActiveEditorIndexAtStart := -1;
  FAutoOpenDesignerFormsDisabled := false;
  fBookmarks.Clear;
  fCompilerOptions.Clear;
  FDefineTemplates.Clear;
  fIconPath := '';
  fJumpHistory.Clear;
  fMainUnitID := -1;
  fModified := false;
  fProjectInfoFile := '';
  UpdateProjectDirectory;
  fPublishOptions.Clear;
  fTargetFileExt := GetDefaultExecutableExt;
  fTitle := '';
  EndUpdate;
end;

procedure TProject.BeginUpdate(Change: boolean);
begin
  inc(FUpdateLock);
  FDefineTemplates.BeginUpdate;
  if FUpdateLock=1 then begin
    fChanged:=Change;
    if Assigned(OnBeginUpdate) then OnBeginUpdate(Self);
  end else
    fChanged:=fChanged or Change;
end;

procedure TProject.EndUpdate;
begin
  if FUpdateLock<=0 then RaiseException('TProject.EndUpdate');
  dec(FUpdateLock);
  FDefineTemplates.EndUpdate;
  if FUpdateLock=0 then begin
    if Assigned(OnEndUpdate) then OnEndUpdate(Self,fChanged);
  end;
end;

function TProject.GetUnits(Index:integer):TUnitInfo;
begin
  Result:=TUnitInfo(fUnitList[Index]);
end;

procedure TProject.SetFlags(const AValue: TProjectFlags);
begin
  FFlags:=AValue;
end;

procedure TProject.SetMainUnitID(const AValue: Integer);
begin
  if fMainUnitID=AValue then exit;
  if (fMainUnitID>=0) and (fMainUnitID<UnitCount) then begin
    MainUnitInfo.DecreaseAutoRevertLock;
  end;
  fMainUnitID:=AValue;
  if (fMainUnitID>=0) and (fMainUnitID<UnitCount) then begin
    MainUnitInfo.IncreaseAutoRevertLock;
  end;
end;

procedure TProject.SetModified(const AValue: boolean);
begin
  if AValue=Modified then exit;
  fModified:=AValue;
  if not fModified then PublishOptions.Modified:=false;
end;

procedure TProject.SetUnits(Index:integer; AUnitInfo: TUnitInfo);
begin
  if AUnitInfo<>TUnitInfo(fUnitList[Index]) then begin
    fUnitList[Index]:=AUnitInfo;
    Modified:=true;
    if AUnitInfo<>nil then AUnitInfo.Project:=Self;
  end;
end;

function TProject.UnitCount:integer;
begin
  Result:=fUnitList.Count;
end;

function TProject.NewUniqueUnitName(NewUnitType:TNewUnitType):string;

  function ExpandedUnitname(const AnUnitName:string):string;
  begin
    Result:=uppercase(ExtractFileNameOnly(AnUnitName));
  end;

  function UnitNameExists(const AnUnitName:string):boolean;
  var i:integer;
    ExpName:string;
  begin
    Result:=true;
    ExpName:=ExpandedUnitName(AnUnitName);
    if ExtractFileNameOnly(fProjectInfoFile)=Expname then exit;
    for i:=0 to UnitCount-1 do
      if (Units[i].IsPartOfProject) 
      and (ExpandedUnitName(Units[i].FileName)=ExpName) then
        exit;
    Result:=false;
  end;

// NewUniqueUnitName(NewUnitType:TNewUnitType)
var u:integer;
  Prefix: string;
begin
  u:=1;
  case NewUnitType of
    nuForm,nuUnit,nuDataModule,nuCGIDataModule: Prefix:='unit';
  else Prefix:='text'
  end;
  while (UnitNameExists(Prefix+IntToStr(u))) do inc(u);
  Result:=Prefix+IntToStr(u);
end;

function TProject.NewUniqueComponentName(NewUnitType:TNewUnitType):string;

  function FormComponentExists(const AComponentName: string): boolean;
  var i: integer;
  begin
    Result:=true;
    for i:=0 to UnitCount-1 do begin
      if (Units[i].Component<>nil) then begin
        if AnsiCompareText(Units[i].Component.Name,AComponentName)=0 then exit;
        if AnsiCompareText(Units[i].Component.ClassName,'T'+AComponentName)=0
        then exit;
      end else if (Units[i].ComponentName<>'')
      and ((Units[i].IsPartOfProject) or (Units[i].Loaded)) then begin
        if AnsiCompareText(Units[i].ComponentName,AComponentName)=0 then exit;
      end;
    end;
    Result:=false;
  end;

// NewUniqueComponentName(NewUnitType:TNewUnitType)
var i: integer;
  Prefix: string;
begin
  i:=1;
  case NewUnitType of
    nuForm, nuUnit: Prefix:='Form';
    nuDataModule:   Prefix:='DataModule';
    nuCGIDataModule:   Prefix:='CGIDataModule';
  else
    Prefix:='form';
  end;
  while (FormComponentExists(Prefix+IntToStr(i))) do inc(i);
  Result:=Prefix+IntToStr(i);
end;

function TProject.AddCreateFormToProjectFile(
  const AClassName, AName: string):boolean;
begin
  Result:=CodeToolBoss.AddCreateFormStatement(MainUnitInfo.Source,
    AClassName,AName);
  if Result then Modified:=true;
end;

function TProject.RemoveCreateFormFromProjectFile(
  const AClassName,AName:string):boolean;
begin
  Result:=CodeToolBoss.RemoveCreateFormStatement(MainUnitInfo.Source,
              AName);
  if Result then Modified:=true;
end;

function TProject.FormIsCreatedInProjectFile(
  const AClassname,AName:string):boolean;
var p: integer;
begin
  Result:=(CodeToolBoss.FindCreateFormStatement(MainUnitInfo.Source,
               1,AClassName,AName,p)=0);
end;

function TProject.IndexOfUnitWithName(const AnUnitName:string; 
  OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo):integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if ((OnlyProjectUnits and Units[Result].IsPartOfProject)
    or (not OnlyProjectUnits))
    and (IgnoreUnit<>Units[Result]) then begin
      if (AnsiCompareText(Units[Result].UnitName,AnUnitName)=0) then
        exit;
    end;
    dec(Result);
  end;
end;

function TProject.IndexOfUnitWithComponent(AComponent: TComponent;
  OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo):integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if (OnlyProjectUnits and Units[Result].IsPartOfProject) 
    or (not OnlyProjectUnits)
    and (IgnoreUnit<>Units[Result]) then begin
      if Units[Result].Component=AComponent then
        exit;
    end;
    dec(Result);
  end;
end;

function TProject.IndexOfUnitWithComponentName(const AComponentName: string;
  OnlyProjectUnits: boolean; IgnoreUnit: TUnitInfo): integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if ((OnlyProjectUnits and Units[Result].IsPartOfProject)
    or (not OnlyProjectUnits))
    and (IgnoreUnit<>Units[Result]) then begin
      if (AnsiCompareText(Units[Result].ComponentName,AComponentName)=0)
      or ((Units[Result].Component<>nil)
        and (AnsiCompareText(Units[Result].Component.Name,AComponentName)=0))
      then
        exit;
    end;
    dec(Result);
  end;
end;

function TProject.UnitWithEditorIndex(Index:integer):TUnitInfo;
begin
  Result:=fFirst[uilWithEditorIndex];
  while (Result<>nil) and (Result.EditorIndex<>Index) do begin
    Result:=Result.fNext[uilWithEditorIndex];
  end;
end;

function TProject.UnitIsUsed(const ShortUnitName:string):boolean;
var NamePos, InPos: integer;
begin
  Result:=CodeToolBoss.FindUnitInAllUsesSections(MainUnitInfo.Source,
              ShortUnitName,NamePos,InPos);
end;

function TProject.GetResourceFile(AnUnitInfo: TUnitInfo;
  Index:integer): TCodeBuffer;
var i, LinkIndex: integer;
begin
  LinkIndex:=-1;
  i:=0;
  Result:=nil;
  while (i<Index) do begin
    inc(i);
    Result:=CodeToolBoss.FindNextResourceFile(AnUnitInfo.Source,LinkIndex);
  end;
end;

function TProject.SearchFile(
  const Filename,SearchPaths,InitialDir:string):string;
var StartPos,EndPos:integer;
  CurPath: string;
  OldDir: string;
begin
  OldDir:=GetCurrentDir;
  SetCurrentDir(ExtractFilePath(InitialDir));
  try
    StartPos:=1;
    while StartPos<=length(SearchPaths) do begin
      EndPos:=Startpos;
      while (EndPos<=length(SearchPaths)) and (SearchPaths[EndPos]<>';') do 
        inc(EndPos);
      CurPath:=copy(SearchPaths,Startpos,EndPos-StartPos);
      if CurPath<>'' then begin
        if CurPath[length(CurPath)]<>PathDelim then
          CurPath:=CurPath+PathDelim;
        Result:=CurPath+Filename;
        if FileExists(Result) then exit;
      end;
      StartPos:=EndPos+1;
    end;
  finally
    SetCurrentDir(OldDir);
  end;
  Result:='';
end;

procedure TProject.ShortenFilename(var AFilename: string);
begin
  OnLoadSaveFilename(AFilename,false);
end;

procedure TProject.LongenFilename(var AFilename: string);
begin
  OnLoadSaveFilename(AFilename,true);
end;

function TProject.GetMainResourceFilename(AnUnitInfo: TUnitInfo):string;
var CodeBuf: TCodeBuffer;
begin
  CodeBuf:=GetResourceFile(AnUnitInfo,1);
  if CodeBuf=nil then begin
    if AnUnitInfo.Filename='' then exit;
    Result:=ChangeFileExt(AnUnitInfo.Filename,ResourceFileExt);
    exit;
  end else
    Result:=CodeBuf.Filename;
end;

function TProject.IsVirtual: boolean;
begin
  Result:=(MainUnitID>=0) and MainUnitInfo.IsVirtual;
end;

function TProject.IndexOf(AUnitInfo: TUnitInfo):integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) and (Units[Result]<>AUnitInfo) do dec(Result);
end;

procedure TProject.CloseEditorIndex(EditorIndex:integer);
var i:integer;
  AnUnitInfo, NextUnitInfo: TUnitInfo;
begin
  AnUnitInfo:=fFirst[uilWithEditorIndex];
  while AnUnitInfo<>nil do begin
    NextUnitInfo:=AnUnitInfo.fNext[uilWithEditorIndex];
    if AnUnitInfo.EditorIndex=EditorIndex then
      AnUnitInfo.EditorIndex:=-1
    else if AnUnitInfo.EditorIndex>EditorIndex then
      AnUnitInfo.EditorIndex:=AnUnitInfo.EditorIndex-1;
    AnUnitInfo:=NextUnitInfo;
  end;
  i:=Bookmarks.Count-1;
  while (i>=0) do begin
    if (Bookmarks[i].EditorIndex=EditorIndex) then
      Bookmarks.Delete(i)
    else
      Bookmarks[i].EditorIndex:=Bookmarks[i].EditorIndex-1;
    dec(i);
  end;
  Modified:=true;
end;

procedure TProject.InsertEditorIndex(EditorIndex:integer);

  function MoveIndex(OldIndex: integer): integer;
  begin
    Result:=OldIndex;
    if OldIndex>=EditorIndex then
      inc(Result);
  end;

var i:integer;
  AnUnitInfo: TUnitInfo;
begin
  // move all editor index of units:
  AnUnitInfo:=fFirst[uilWithEditorIndex];
  while AnUnitInfo<>nil do begin
    AnUnitInfo.EditorIndex:=MoveIndex(AnUnitInfo.EditorIndex);
    AnUnitInfo:=AnUnitInfo.fNext[uilWithEditorIndex];
  end;
  // move bookmarks
  i:=Bookmarks.Count-1;
  while (i>=0) do begin
    Bookmarks[i].EditorIndex:=MoveIndex(Bookmarks[i].EditorIndex);
    dec(i);
  end;
  Modified:=true;
end;

procedure TProject.MoveEditorIndex(OldEditorIndex, NewEditorIndex: integer);

  function MoveIndex(OldIndex: integer): integer;
  begin
    Result:=OldIndex;
    if OldIndex=OldEditorIndex then
      // this is the moving index
      Result:=NewEditorIndex
    else if OldIndex>OldEditorIndex then begin
      // right of OldPageIndex ...
      if OldIndex<=NewEditorIndex then
        // .. and left of NewEditorIndex
        // -> move left
        Dec(Result);
    end else begin
      // left of OldPageIndex ...
      if OldIndex>=NewEditorIndex then
        // .. and right of NewEditorIndex
        // -> move right
        Inc(Result);
    end;
  end;

var
  i:integer;
  AnUnitInfo: TUnitInfo;
begin
  if OldEditorIndex=NewEditorIndex then exit;
  // move all editor index of units:
  AnUnitInfo:=fFirst[uilWithEditorIndex];
  while AnUnitInfo<>nil do begin
    AnUnitInfo.EditorIndex:=MoveIndex(AnUnitInfo.EditorIndex);
    AnUnitInfo:=AnUnitInfo.fNext[uilWithEditorIndex];
  end;
  // move bookmarks
  i:=Bookmarks.Count-1;
  while (i>=0) do begin
    Bookmarks[i].EditorIndex:=MoveIndex(Bookmarks[i].EditorIndex);
    dec(i);
  end;
  Modified:=true;
end;

procedure TProject.AddToOrRemoveFromEditorWithIndexList(AnUnitInfo: TUnitInfo);
begin
  if AnUnitInfo.EditorIndex<0 then begin
    RemoveFromList(AnUnitInfo,uilWithEditorIndex);
  end else begin
    AddToList(AnUnitInfo,uilWithEditorIndex);
  end;
end;

procedure TProject.AddToOrRemoveFromComponentList(AnUnitInfo: TUnitInfo);
begin
  if AnUnitInfo.Component=nil then begin
    RemoveFromList(AnUnitInfo,uilWithComponent);
  end else begin
    AddToList(AnUnitInfo,uilWithComponent);
  end;
end;

procedure TProject.AddToOrRemoveFromLoadedList(AnUnitInfo: TUnitInfo);
begin
  if not AnUnitInfo.Loaded then begin
    RemoveFromList(AnUnitInfo,uilLoaded);
  end else begin
    AddToList(AnUnitInfo,uilLoaded);
  end;
end;

procedure TProject.AddToOrRemoveFromAutoRevertLockedList(AnUnitInfo: TUnitInfo);
begin
  if not AnUnitInfo.IsAutoRevertLocked then begin
    RemoveFromList(AnUnitInfo,uilAutoRevertLocked);
  end else begin
    AddToList(AnUnitInfo,uilAutoRevertLocked);
  end;
end;

procedure TProject.AddToOrRemoveFromPartOfProjectList(AnUnitInfo: TUnitInfo);
begin
  if not AnUnitInfo.IsPartOfProject then begin
    RemoveFromList(AnUnitInfo,uilPartOfProject);
  end else begin
    AddToList(AnUnitInfo,uilPartOfProject);
  end;
end;

function TProject.GetTargetFilename: string;
begin
  Result:=fCompilerOptions.TargetFilename;
end;

procedure TProject.SetTargetFilename(const NewTargetFilename: string);
begin
  fCompilerOptions.TargetFilename:=NewTargetFilename;
end;

function TProject.GetMainFilename: String;
begin
  if MainUnitID>=0 then Result:=MainUnitInfo.Filename
  else Result:='';
end;

function TProject.GetFirstPartOfProject: TUnitInfo;
begin
  Result:=FFirst[uilPartOfProject];
end;

function TProject.GetFirstLoadedUnit: TUnitInfo;
begin
  Result:=fFirst[uilLoaded];
end;

function TProject.GetFirstAutoRevertLockedUnit: TUnitInfo;
begin
  Result:=fFirst[uilAutoRevertLocked];
end;

function TProject.GetFirstUnitWithComponent: TUnitInfo;
begin
  Result:=fFirst[uilWithComponent];
end;

function TProject.GetFirstUnitWithEditorIndex: TUnitInfo;
begin
  Result:=fFirst[uilWithEditorIndex];
end;

function TProject.GetMainUnitInfo: TUnitInfo;
begin
  if (MainUnitID>=0) and (MainUnitID<UnitCount) then
    Result:=Units[MainUnitID]
  else
    Result:=nil;
end;

function TProject.GetProjectInfoFile:string;
begin
  Result:=fProjectInfoFile;
end;

procedure TProject.SetProjectInfoFile(const NewFilename:string);
var
  NewProjectInfoFile: String;
begin
  NewProjectInfoFile:=TrimFilename(NewFilename);
  if NewProjectInfoFile='' then exit;
  DoDirSeparators(NewProjectInfoFile);
  if (AnsiCompareText(fTitle,ExtractFileNameOnly(fProjectInfoFile))=0)
  or (fProjectInfoFile='') or (fTitle='') then begin
    fTitle:=ExtractFileNameOnly(NewProjectInfoFile);
  end;
  fProjectInfoFile:=NewProjectInfoFile;
  UpdateProjectDirectory;
  Modified:=true;
end;

function TProject.OnUnitFileBackup(const Filename:string;
  IsPartOfProject:boolean):TModalResult;
begin
  if Assigned(fOnFileBackup) then
    Result:=fOnFileBackup(Filename,IsPartOfProject)
  else
    Result:=mrOk;
end;

procedure TProject.OnLoadSaveFilename(var AFilename: string; Load:boolean);
var ProjectPath:string;
begin
  if AFileName='' then exit;
  ProjectPath:=ProjectDirectory;
  if ProjectPath='' then ProjectPath:=GetCurrentDir;
  if fPathDelimChanged then
    DoDirSeparators(AFilename);
  AFilename:=TrimFilename(AFilename);
  if Load then begin
    // make filename absolute
    if (AFilename<>'') and (not FilenameIsAbsolute(AFilename)) then
      AFilename:=TrimFilename(ProjectPath+AFilename);
  end else begin
    // try making filename relative to project file
    if (AFilename<>'') and FilenameIsAbsolute(AFilename)
    and FileIsInPath(AFilename,ProjectPath) then
      AFilename:=CreateRelativePath(AFilename,ProjectPath);
  end;
end;

function TProject.RemoveProjectPathFromFilename(
  const AFilename: string): string;
var ProjectPath:string;
begin
  ProjectPath:=ProjectDirectory;
  if ProjectPath='' then ProjectPath:=GetCurrentDir;
  Result:=AFilename;
  DoDirSeparators(Result);
  // try making filename relative to project file
  if FilenameIsAbsolute(Result)
  and (CompareFileNames(copy(Result,1,length(ProjectPath)),ProjectPath)=0)
  then
    Result:=copy(Result,length(ProjectPath)+1,
         length(Result)-length(ProjectPath));
end;

function TProject.FileIsInProjectDir(const AFilename: string): boolean;
var ProjectDir, FilePath: string;
begin
  if FilenameIsAbsolute(AFilename) then begin
    if (not IsVirtual) then begin
      ProjectDir:=ProjectDirectory;
      FilePath:=LeftStr(AFilename,length(ProjectDir));
      Result:=(CompareFileNames(ProjectDir,FilePath)=0);
    end else
      Result:=false;
  end else
    Result:=true;
end;

procedure TProject.GetVirtualDefines(DefTree: TDefineTree;
  DirDef: TDirectoryDefines);
  
  procedure ExtendPath(const AVariable, APath: string);
  var
    TempValue: string;
  begin
    if APath<>'' then begin
      DefTree.ReadValue(DirDef,APath+';','',TempValue);
      DirDef.Values.Prepend(AVariable,TempValue);
    end;
  end;
  
begin
  if (not IsVirtual) then exit;
  ExtendPath(UnitPathMacroName,CompilerOptions.OtherUnitFiles);
  ExtendPath(IncludePathMacroName,CompilerOptions.IncludeFiles);
  ExtendPath(SrcPathMacroName,CompilerOptions.SrcPath);
end;

procedure TProject.GetUnitsChangedOnDisk(var AnUnitList: TList);
var
  AnUnitInfo: TUnitInfo;
begin
  AnUnitList:=nil;
  AnUnitInfo:=fFirst[uilAutoRevertLocked];
  while (AnUnitInfo<>nil) do begin
    if AnUnitInfo.ChangedOnDisk(false) then begin
      if AnUnitList=nil then
        AnUnitList:=TList.Create;
      AnUnitList.Add(AnUnitInfo);
    end;
    AnUnitInfo:=AnUnitInfo.fNext[uilAutoRevertLocked];
  end;
end;

procedure TProject.SetBookmark(AnUnitInfo: TUnitInfo; X, Y, ID: integer);
begin
  if AnUnitInfo.EditorIndex>=0 then
    Bookmarks.Add(X,Y,AnUnitInfo.EditorIndex,ID);
  AnUnitInfo.Bookmarks.Add(X,Y,ID);
end;

procedure TProject.MergeBookmarks(AnUnitInfo: TUnitInfo);
// merge the bookmarks of the unit with the bookmarks in the source editor
var
  i: Integer;
  UnitMark: TFileBookmark;
  ProjectMark: TProjectBookmark;
begin
  if AnUnitInfo.EditorIndex<0 then exit;
  for i:=0 to AnUnitInfo.Bookmarks.Count-1 do begin
    UnitMark:=AnUnitInfo.Bookmarks[i];
    ProjectMark:=Bookmarks.BookmarkWithIndex(UnitMark.ID);
    // merge the bookmark into the currently existing bookmarks, if the ID is
    // free
    //writeln('TProject.MergeBookmarks ',AnUnitInfo.Filename,' Y=',UnitMark.Y);
    if (ProjectMark=nil) then
      Bookmarks.Add(UnitMark.X,UnitMark.Y,AnUnitInfo.EditorIndex,UnitMark.ID);
  end;
end;

function TProject.FindDependencyByName(const PackageName: string
  ): TPkgDependency;
begin
  Result:=FindDependencyByNameInList(FFirstRequiredDependency,pdlRequires,
                                     PackageName);
end;

function TProject.RequiredDepByIndex(Index: integer): TPkgDependency;
begin
  Result:=GetDependencyWithIndex(FFirstRequiredDependency,pdlRequires,Index);
end;

function TProject.RemovedDepByIndex(Index: integer): TPkgDependency;
begin
  Result:=GetDependencyWithIndex(FFirstRemovedDependency,pdlRequires,Index);
end;

procedure TProject.AddRequiredDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  Dependency.AddToList(FFirstRequiredDependency,pdlRequires);
  Dependency.Owner:=Self;
  Dependency.HoldPackage:=true;
  FDefineTemplates.CompilerFlagsChanged;
  Modified:=true;
  EndUpdate;
end;

procedure TProject.RemoveRequiredDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  Dependency.RemoveFromList(FFirstRequiredDependency,pdlRequires);
  Dependency.RequiredPackage:=nil;
  Dependency.AddToList(FFirstRemovedDependency,pdlRequires);
  Dependency.Removed:=true;
  FDefineTemplates.CompilerFlagsChanged;
  Modified:=true;
  EndUpdate;
end;

procedure TProject.DeleteRequiredDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  Dependency.RequiredPackage:=nil;
  Dependency.RemoveFromList(FFirstRequiredDependency,pdlRequires);
  Dependency.Free;
  FDefineTemplates.CompilerFlagsChanged;
  EndUpdate;
end;

procedure TProject.DeleteRemovedDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  Dependency.RequiredPackage:=nil;
  Dependency.RemoveFromList(FFirstRemovedDependency,pdlRequires);
  Dependency.Free;
  EndUpdate;
end;

procedure TProject.RemoveRemovedDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  Dependency.RemoveFromList(FFirstRemovedDependency,pdlRequires);
  Dependency.Removed:=false;
  EndUpdate;
end;

procedure TProject.ReaddRemovedDependency(Dependency: TPkgDependency);
begin
  BeginUpdate(true);
  RemoveRemovedDependency(Dependency);
  AddRequiredDependency(Dependency);
  EndUpdate;
end;

procedure TProject.MoveRequiredDependencyUp(Dependency: TPkgDependency);
begin
  if Dependency.PrevRequiresDependency=nil then exit;
  BeginUpdate(true);
  Dependency.MoveUpInList(FFirstRequiredDependency,pdlRequires);
  FDefineTemplates.CompilerFlagsChanged;
  EndUpdate;
end;

procedure TProject.MoveRequiredDependencyDown(Dependency: TPkgDependency);
begin
  if Dependency.NextRequiresDependency=nil then exit;
  BeginUpdate(true);
  Dependency.MoveDownInList(FFirstRequiredDependency,pdlRequires);
  FDefineTemplates.CompilerFlagsChanged;
  EndUpdate;
end;

function TProject.Requires(APackage: TLazPackage): boolean;
begin
  Result:=FindCompatibleDependencyInList(FFirstRequiredDependency,pdlRequires,
                  APackage)<>nil;
end;

procedure TProject.GetAllRequiredPackages(var List: TList);
begin
  if Assigned(OnGetAllRequiredPackages) then
    OnGetAllRequiredPackages(FirstRequiredDependency,List);
end;

procedure TProject.OnUnitNameChange(AnUnitInfo: TUnitInfo; 
  const OldUnitName, NewUnitName: string;  CheckIfAllowed: boolean;
  var Allowed: boolean);
var i:integer;
begin
  if AnUnitInfo.IsPartOfProject then begin
    if CheckIfAllowed then begin
      // check if no other project unit has this name
      for i:=0 to UnitCount-1 do begin
        if (Units[i].IsPartOfProject)
        and (Units[i]<>AnUnitInfo) and (Units[i].UnitName<>'')
        and (lowercase(Units[i].UnitName)=lowercase(NewUnitName)) then begin
          Allowed:=false;
          exit;
        end;
      end;
    end;
    if (OldUnitName<>'')
    and (ProjectType in [ptProgram, ptApplication, ptCGIApplication]) then
    begin
      // rename unit in program uses section
      CodeToolBoss.RenameUsedUnit(MainUnitInfo.Source
        ,OldUnitName,NewUnitName,'');
    end;
  end;
end;

procedure TProject.SetAutoOpenDesignerFormsDisabled(const AValue: boolean);
begin
  if FAutoOpenDesignerFormsDisabled=AValue then exit;
  FAutoOpenDesignerFormsDisabled:=AValue;
end;

function TProject.JumpHistoryCheckPosition(
  APosition:TProjectJumpHistoryPosition): boolean;
var i: integer;
begin
  i:=IndexOfFilename(APosition.Filename);
  Result:=(i>=0) and (Units[i].EditorIndex>=0);
end;

function TProject.SomethingModified: boolean;
var i: integer;
begin
  Result:=Modified;
  for i:=0 to UnitCount-1 do Result:=Result or Units[i].Modified;
  Result:=Result or CompilerOptions.Modified;
end;

procedure TProject.MainSourceFilenameChanged;
begin

end;

Function TProject.UnitWithComponent(AComponent: TComponent) : TUnitInfo;
begin
  Result:=fFirst[uilWithComponent];
  while (Result<>nil) and (Result.Component<>AComponent) do
    Result:=Result.fNext[uilWithComponent];
end;

function TProject.UnitInfoWithFilename(const AFilename: string): TUnitInfo;
var
  i: Integer;
begin
  i:=IndexOfFilename(AFilename);
  if i>=0 then
    Result:=Units[i]
  else
    Result:=nil;
end;

function TProject.UnitInfoWithFilename(const AFilename: string;
  SearchFlags: TProjectFileSearchFlags): TUnitInfo;
var
  i: Integer;
begin
  i:=IndexOfFilename(AFilename,SearchFlags);
  if i>=0 then
    Result:=Units[i]
  else
    Result:=nil;
end;

function TProject.UnitWithUnitname(const AnUnitname: string): TUnitInfo;
var
  i: Integer;
begin
  i:=IndexOfUnitWithName(AnUnitName,true,nil);
  if i>=0 then
    Result:=Units[i]
  else
    Result:=nil;
end;

function TProject.IndexOfFilename(const AFilename: string): integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if CompareFilenames(AFilename,Units[Result].Filename)=0 then exit;
    dec(Result);
  end;
end;

function TProject.IndexOfFilename(const AFilename: string;
  SearchFlags: TProjectFileSearchFlags): integer;
var
  BaseFilename: String;
  CurBaseFilename: String;
begin
  BaseFilename:=AFilename;
  if pfsfResolveFileLinks in SearchFlags then
    BaseFilename:=ReadAllLinks(AFilename,false);
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if (pfsfOnlyEditorFiles in SearchFlags)
    and (Units[Result].EditorIndex<0) then begin
      dec(Result);
      continue;
    end;
    if (pfsfOnlyVirtualFiles in SearchFlags)
    and (not Units[Result].IsVirtual) then begin
      dec(Result);
      continue;
    end;
    if (pfsfOnlyProjectFiles in SearchFlags)
    and (not Units[Result].IsPartOfProject) then begin
      dec(Result);
      continue;
    end;
    CurBaseFilename:=Units[Result].Filename;
    if pfsfResolveFileLinks in SearchFlags then
      CurBaseFilename:=ReadAllLinks(CurBaseFilename,false);
    if CompareFilenames(BaseFilename,CurBaseFilename)=0 then exit;
    dec(Result);
  end;
end;

function TProject.ProjectUnitWithFilename(const AFilename: string): TUnitInfo;
begin
  Result:=fFirst[uilPartOfProject];
  while Result<>nil do begin
    if CompareFileNames(AFilename,Result.Filename)=0 then exit;
    Result:=Result.fNext[uilPartOfProject];
  end;
end;

function TProject.ProjectUnitWithUnitname(const AnUnitName: string): TUnitInfo;
begin
  Result:=fFirst[uilPartOfProject];
  while Result<>nil do begin
    if AnsiCompareText(AnUnitName,Result.UnitName)=0 then exit;
    Result:=Result.fNext[uilPartOfProject];
  end;
end;

procedure TProject.UpdateProjectDirectory;
begin
  fProjectDirectory:=ExtractFilePath(fProjectInfoFile);
  CompilerOptions.BaseDirectory:=fProjectDirectory;
end;

procedure TProject.AddToList(AnUnitInfo: TUnitInfo; ListType: TUnitInfoList);
begin
  // add to list if AnUnitInfo is not in list
  if (fFirst[ListType]<>AnUnitInfo)
  and (AnUnitInfo.fNext[ListType]=nil)
  and (AnUnitInfo.fPrev[ListType]=nil) then begin
    AnUnitInfo.fNext[ListType]:=fFirst[ListType];
    AnUnitInfo.fPrev[ListType]:=nil;
    fFirst[ListType]:=AnUnitInfo;
    if AnUnitInfo.fNext[ListType]<>nil then
      AnUnitInfo.fNext[ListType].fPrev[ListType]:=AnUnitInfo;
  end;
end;

procedure TProject.RemoveFromList(AnUnitInfo: TUnitInfo;
  ListType: TUnitInfoList);
begin
  // remove from list if AnUnitInfo is in list
  if fFirst[ListType]=AnUnitInfo then
    fFirst[ListType]:=AnUnitInfo.fNext[ListType];
  if AnUnitInfo.fNext[ListType]<>nil then
    AnUnitInfo.fNext[ListType].fPrev[ListType]:=
      AnUnitInfo.fPrev[ListType];
  if AnUnitInfo.fPrev[ListType]<>nil then
    AnUnitInfo.fPrev[ListType].fNext[ListType]:=
      AnUnitInfo.fNext[ListType];
  AnUnitInfo.fNext[ListType]:=nil;
  AnUnitInfo.fPrev[ListType]:=nil;
end;


{ TProjectCompilerOptions }

procedure TProjectCompilerOptions.SetTargetCPU(const AValue: string);
begin
  inherited SetTargetCPU(AValue);
  FGlobals.TargetCPU:=TargetCPU;
end;

procedure TProjectCompilerOptions.SetTargetOS(const AValue: string);
begin
  inherited SetTargetOS(AValue);
  FGlobals.TargetOS:=TargetOS;
end;

procedure TProjectCompilerOptions.Assign(CompOpts: TBaseCompilerOptions);
begin
  inherited Assign(CompOpts);
  UpdateGlobals;
end;

procedure TProjectCompilerOptions.UpdateGlobals;
begin
  FGlobals.TargetCPU:=TargetCPU;
  FGlobals.TargetOS:=TargetOS;
end;

constructor TProjectCompilerOptions.Create(TheProject: TProject);
begin
  FGlobals:=TGlobalCompilerOptions.Create;
  inherited Create(TheProject);
  UpdateGlobals;
  fOwnerProject:=TheProject;
end;

destructor TProjectCompilerOptions.Destroy;
begin
  inherited Destroy;
  FGlobals.Free;
end;

function TProjectCompilerOptions.GetOwnerName: string;
begin
  Result:=OwnerProject.Title;
  if Result='' then Result:=ExtractFilename(OwnerProject.ProjectInfoFile);
end;

function TProjectCompilerOptions.GetDefaultMainSourceFileName: string;
var
  MainUnitInfo: TUnitInfo;
begin
  MainUnitInfo:=FOwnerProject.MainUNitInfo;
  if (MainUnitInfo<>nil) then
    Result:=ExtractFileName(MainUnitInfo.Filename);
  if Result='' then
    Result:=inherited GetDefaultMainSourceFileName;
end;

procedure TProjectCompilerOptions.GetInheritedCompilerOptions(
  var OptionsList: TList);
var
  PkgList: TList;
begin
  PkgList:=nil;
  OwnerProject.GetAllRequiredPackages(PkgList);
  OptionsList:=GetUsageOptionsList(PkgList);
  PkgList.Free;
end;

{ TProjectDefineTemplates }

constructor TProjectDefineTemplates.Create(OwnerProject: TProject);
begin
  inherited Create;
  FProject:=OwnerProject;
end;

destructor TProjectDefineTemplates.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TProjectDefineTemplates.Clear;
begin
  if FMain<>nil then begin
    if CodeToolBoss<>nil then
      CodeToolBoss.DefineTree.RemoveDefineTemplate(FMain);
    FMain:=nil;
    FProjectDir:=nil;
    FFlags:=FFlags+[ptfFlagsChanged];
  end;
end;

procedure TProjectDefineTemplates.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TProjectDefineTemplates.EndUpdate;
begin
  if FUpdateLock=0 then RaiseException('TProjectDefineTemplates.EndUpdate');
  dec(FUpdateLock);
  if FUpdateLock=0 then begin
    if ptfFlagsChanged in FFlags then CompilerFlagsChanged;
  end;
end;

procedure TProjectDefineTemplates.UpdateMain;
begin
  // update the package block define template (the container for all other
  // define templates of the package)
  if (FMain=nil) and (not Owner.Destroying) then begin
    // create the main project template
    FMain:=CreateProjectTemplate(FProjectDir);
    FMain.SetDefineOwner(Owner,false);
    FMain.SetFlags([dtfAutoGenerated],[],false);
  end;
  // ClearCache is here unnessary, because it is only a block
end;

procedure TProjectDefineTemplates.CompilerFlagsChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,ptfFlagsChanged);
    exit;
  end;
  Exclude(FFlags,ptfFlagsChanged);
  if Owner.Destroying then exit;
  if FMain=nil then UpdateMain;

  if (FProjectDir=nil) then exit;
  UpdateCompilerOptionsTemplates(FProjectDir,Owner.CompilerOptions,true,true);
end;

procedure TProjectDefineTemplates.AllChanged;
begin
  CompilerFlagsChanged;
  UpdateGlobalValues;
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TProjectDefineTemplates.UpdateGlobalValues;
var
  NewProjectDir: String;
  Changed: Boolean;
begin
  Changed:=false;
  Changed:=Changed or CodeToolBoss.SetGlobalValue(
                           ExternalMacroStart+'LCLWidgetType',
                           Owner.CompilerOptions.GetEffectiveLCLWidgetType);
  if Owner.IsVirtual then
    NewProjectDir:=VirtualDirectory
  else
    NewProjectDir:=Owner.ProjectDirectory;
  Changed:=Changed or CodeToolBoss.SetGlobalValue(
                                   ExternalMacroStart+'ProjPath',NewProjectDir);
  if Changed then
    IncreaseCompilerParseStamp;
end;

end.

{
  $Log$
  Revision 1.152  2004/03/20 12:55:48  mattias
  implemented adding Application.Title:= statements

  Revision 1.151  2004/03/17 11:28:35  mattias
  fixed setting project LCLWidgetSet in defines

  Revision 1.150  2004/03/15 15:56:24  mattias
  fixed package ID string to ID conversion

  Revision 1.149  2004/02/22 15:39:43  mattias
  fixed error handling on saving lpi file

  Revision 1.148  2004/02/17 22:17:39  mattias
  accelerated conversion from data to lrs

  Revision 1.147  2004/01/17 13:29:04  mattias
  using now fpc constant LineEnding   from Vincent

  Revision 1.146  2004/01/03 20:19:22  mattias
  fixed reopening virtual files

  Revision 1.145  2003/12/26 09:37:19  mattias
  added TProject.Destroying

  Revision 1.144  2003/12/25 14:17:06  mattias
  fixed many range check warnings

  Revision 1.143  2003/12/23 18:51:40  mattias
  fixed updating Define caches, when project dependencies changes

  Revision 1.142  2003/12/20 01:20:52  mattias
  splitted output directories for cross compilation

  Revision 1.141  2003/11/25 08:59:01  mattias
  fixed a few more black colors

  Revision 1.140  2003/11/22 23:56:33  mattias
  fixed win32 intf menu height from Wojciech

  Revision 1.139  2003/10/15 18:01:10  mattias
  implemented extract proc, check lfm and convert delphi unit

  Revision 1.138  2003/10/11 08:33:22  mattias
  added catalan

  Revision 1.137  2003/09/18 09:21:02  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.136  2003/09/17 08:43:17  mattias
  fixed loading old project compiler options

  Revision 1.135  2003/09/10 12:13:48  mattias
  implemented Import and Export of compiler options

  Revision 1.134  2003/08/20 15:06:57  mattias
  implemented Build+Run File

  Revision 1.133  2003/08/15 14:28:48  mattias
  clean up win32 ifdefs

  Revision 1.132  2003/07/14 09:03:39  mattias
  deactivated FCL TDataModule

  Revision 1.131  2003/07/08 17:30:19  mattias
  fixed changing widget set and TStringGrid exceptions on ColCount=0

  Revision 1.130  2003/06/25 17:22:47  mattias
  added automatic linux-windows file conversions

  Revision 1.129  2003/06/19 09:26:58  mattias
  fixed changing unitname during update

  Revision 1.128  2003/06/08 11:05:45  mattias
  implemented filename case check before adding to project

  Revision 1.127  2003/06/03 16:12:14  mattias
  fixed loading bookmarks for editor index 0

  Revision 1.126  2003/06/01 21:09:09  mattias
  implemented datamodules

  Revision 1.125  2003/06/01 11:23:01  mattias
  splittet designer form and lookup root

  Revision 1.124  2003/05/31 10:07:33  mattias
  changed projects forms into components

  Revision 1.123  2003/05/30 16:25:47  mattias
  started datamodule

  Revision 1.122  2003/05/26 21:03:27  mattias
  added README, describing how to create a gtk2 lcl application

  Revision 1.121  2003/05/23 14:12:51  mattias
  implemented restoring breakpoints

  Revision 1.120  2003/05/21 16:19:12  mattias
  implemented saving breakpoints and watches

  Revision 1.119  2003/05/12 14:47:45  mattias
  reduced output

  Revision 1.118  2003/05/12 13:11:34  mattias
  implemented publish package

  Revision 1.117  2003/05/02 10:28:59  mattias
  improved file checking

  Revision 1.116  2003/04/29 19:00:41  mattias
  added package gtkopengl

  Revision 1.115  2003/04/29 09:31:10  mattias
  changed macro name ProjectDir to ProjPath

  Revision 1.114  2003/04/24 16:44:28  mattias
  implemented define templates for projects with packages

  Revision 1.113  2003/04/21 16:21:28  mattias
  implemented default package for custom IDE components

  Revision 1.112  2003/04/20 23:10:03  mattias
  implemented inherited project compiler options

  Revision 1.111  2003/04/20 20:32:40  mattias
  implemented removing, re-adding, updating project dependencies

  Revision 1.110  2003/04/20 09:52:07  mattias
  implemented saving loading project dependencies

  Revision 1.109  2003/04/20 07:36:29  mattias
  fixed loading form name

  Revision 1.108  2003/04/18 15:32:51  mattias
  implemented file reference list

  Revision 1.107  2003/04/16 13:48:10  mattias
  implemented creating compiler option string for packages

  Revision 1.106  2003/04/15 17:58:28  mattias
  implemented inherited Compiler Options View

  Revision 1.105  2003/04/15 08:54:27  mattias
  fixed TMemo.WordWrap

  Revision 1.104  2003/04/14 18:03:47  mattias
  implemented inherited compiler options

  Revision 1.103  2003/04/13 13:45:04  mattias
  implemented broken dependencies dialog

  Revision 1.102  2003/04/07 23:49:03  mattias
  implemented adding units to packages

  Revision 1.101  2003/03/29 21:41:19  mattias
  fixed path delimiters for environment directories

  Revision 1.100  2003/03/29 17:20:04  mattias
  added TMemoScrollBar

  Revision 1.99  2003/03/25 17:11:16  mattias
  set Project.AutoCreateForms default to true

  Revision 1.98  2003/03/11 09:57:51  mattias
  implemented ProjectOpt: AutoCreateNewForms, added designer Show Options

  Revision 1.97  2003/03/08 21:51:57  mattias
  make resource string dialog nearly complete

  Revision 1.96  2003/03/07 13:32:40  mattias
  fixed checking readonly for non existing files

  Revision 1.95  2003/03/07 11:41:21  mattias
  fixed readonly check and added script to quick create lazarus snapshot

  Revision 1.94  2003/02/28 19:10:25  mattias
  added new ... dialog

  Revision 1.93  2003/02/28 15:38:00  mattias
  bookmarks are now saved also for closed files and merged when possible

  Revision 1.92  2003/02/28 10:14:28  mattias
  started package system (packager)

  Revision 1.91  2003/02/26 12:44:52  mattias
  readonly flag is now only saved if user set

  Revision 1.90  2003/01/15 09:08:08  mattias
  fixed search paths for virtual projects

  Revision 1.89  2003/01/02 04:33:55  mattias
  implemented incremental find and unit usage counts

  Revision 1.88  2002/12/28 13:26:36  mattias
  reduced lpi size

  Revision 1.87  2002/12/28 12:42:38  mattias
  focus fixes, reduced lpi size

  Revision 1.86  2002/11/16 13:56:20  mattias
  project now notices, if compiler options changed

  Revision 1.85  2002/10/30 22:28:49  lazarus
  MG: fixed used virtual files and IsPartOfProject Bug

  Revision 1.84  2002/10/26 15:15:43  lazarus
  MG: broke LCL<->interface circles

  Revision 1.83  2002/10/13 09:35:37  lazarus
  MG: added publish project

  Revision 1.82  2002/10/04 21:31:56  lazarus
  MG: added some component rename checks

  Revision 1.81  2002/10/02 16:16:39  lazarus
  MG: accelerated unitdependencies

  Revision 1.80  2002/09/30 23:45:58  lazarus
  MG: added part of project list to project

  Revision 1.79  2002/09/30 23:41:00  lazarus
  MG: added part of project list to project

  Revision 1.78  2002/09/30 11:01:43  lazarus
  MG: accelerated xmlwriter

  Revision 1.77  2002/09/20 13:11:11  lazarus
  MG: fixed TPanel and Frame3D

  Revision 1.76  2002/09/20 11:40:07  lazarus
  MG: added Move Page Left/Right for sourcenotebook

  Revision 1.75  2002/09/13 16:58:26  lazarus
  MG: removed the 1x1 bitmap from TBitBtn

  Revision 1.74  2002/09/05 19:03:36  lazarus
  MG: improved handling of ambigious source files

  Revision 1.73  2002/08/23 07:05:15  lazarus
  MG: started form renaming

  Revision 1.72  2002/08/21 07:16:59  lazarus
  MG: reduced mem leak of clipping stuff, still not fixed

  Revision 1.71  2002/08/07 09:55:28  lazarus
  MG: codecompletion now checks for filebreaks, savefile now checks for filedate

  Revision 1.70  2002/08/01 14:10:30  lazarus
  MG: started file access monitoring for loaded files

  Revision 1.69  2002/08/01 08:03:03  lazarus
  MG: accelerated searches in project

  Revision 1.68  2002/07/31 06:52:17  lazarus
  MG: started File Access Monitoring for hidden files

  Revision 1.67  2002/07/30 06:24:04  lazarus
  MG: added a faster version of TXMLConfig

  Revision 1.66  2002/07/06 06:37:06  lazarus
  MG: added Revert

  Revision 1.65  2002/05/16 13:00:57  lazarus
  MG: fixed changing syntax highlighter on save as

  Revision 1.64  2002/05/10 06:57:45  lazarus
  MG: updated licenses

  Revision 1.63  2002/04/28 14:10:29  lazarus
  MG: fixes for saving resource files

  Revision 1.62  2002/04/21 13:24:06  lazarus
  MG: small updates and fixes

  Revision 1.61  2002/04/15 12:45:57  lazarus
  MG: added save projectunit flags

  Revision 1.60  2002/04/05 16:34:16  lazarus
  MG: fixed autocreate form editing in project opts

  Revision 1.59  2002/04/04 17:21:18  lazarus
  MG: fixed outputfilter for linker errors

  Revision 1.58  2002/04/02 17:18:25  lazarus
  MG: fixed save project as, renaming source name

  Revision 1.57  2002/03/28 00:11:06  lazarus
  MG: removed unused

  Revision 1.56  2002/03/25 16:48:26  lazarus
  MG: clean ups for main.pp, many minor fixes

  Revision 1.55  2002/03/25 07:29:23  lazarus
  MG: added TOpen/SaveFlags and splittet some methods

  Revision 1.54  2002/03/22 12:36:45  lazarus
  MG: many fixes, to make it short: events

  Revision 1.53  2002/03/21 23:59:59  lazarus
  MG: code creation options applied to new unit source

  Revision 1.52  2002/03/21 22:44:08  lazarus
  MG: fixes for save-as and form streaming exceptions

  Revision 1.51  2002/03/05 08:14:59  lazarus
  MG: updates for codetools defines editor

  Revision 1.50  2002/02/25 23:18:54  lazarus
  MG: jump history will now try to save relative filenames

  Revision 1.49  2002/02/08 21:08:00  lazarus
  MG: saving of virtual project files will now save the whole project

  Revision 1.48  2002/02/03 00:23:55  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.47  2001/12/19 22:09:14  lazarus
  MG: added GUID and alias parsing, added DoJumpToCodeToolBossError

  Revision 1.46  2001/12/16 22:24:54  lazarus
  MG: changes for new compiler 20011216

  Revision 1.45  2001/12/13 23:09:58  lazarus
  MG: enhanced code caching, fixed CursorToCleanPos and beautify statement

  Revision 1.44  2001/12/02 11:03:36  lazarus
  MG: added default pascal file extension option

  Revision 1.43  2001/12/01 22:17:26  lazarus
  MG: added jump-history

  Revision 1.42  2001/11/17 09:48:56  lazarus
  MG: changing project filename will now also change the title

  Revision 1.41  2001/11/15 13:49:50  lazarus
  MG: fixed open non existing file and unitname in save project as

  Revision 1.40  2001/11/14 17:46:57  lazarus
  Changes to make toggling between form and unit work.
  Added BringWindowToTop
  Shane

  Revision 1.39  2001/11/06 22:20:30  lazarus
  MG: started breakpoint and watch frontend

  Revision 1.38  2001/11/06 16:42:23  lazarus
  MG: added facade for find in files

  Revision 1.36  2001/11/06 15:47:32  lazarus
  MG: added build all

  Revision 1.35  2001/11/06 12:20:33  lazarus
  MG: added Run Parameter Options - not enabled yet

  Revision 1.34  2001/11/05 18:18:18  lazarus
  added popupmenu+arrows to notebooks, added target filename

  Revision 1.33  2001/11/03 08:37:35  lazarus
  MG: fixed errorline showing, resource adding and published var editing and added make cleanall

  Revision 1.32  2001/10/23 09:13:52  lazarus
  MG: fixed TestProject

  Revision 1.31  2001/10/18 13:01:31  lazarus
  MG: fixed speedbuttons numglyphs>1 and started IDE debugging

  Revision 1.30  2001/10/15 13:11:27  lazarus
  MG: added complete code

  Revision 1.27  2001/10/09 09:46:50  lazarus
  MG: added codetools, fixed synedit unindent, fixed MCatureHandle

  Revision 1.26  2001/07/08 22:33:56  lazarus
  MG: added rapid testing project

  Revision 1.25  2001/06/27 21:43:23  lazarus
  MG: added project bookmark support

  Revision 1.24  2001/06/04 09:32:17  lazarus
  MG: fixed bugs and cleaned up messages

  Revision 1.23  2001/05/27 11:52:00  lazarus
  MG: added --primary-config-path=<filename> cmd line option

  Revision 1.20  2001/04/04 13:55:35  lazarus
  MG: finished TComponentPropertyEditor, added OnModified to oi, cfe and designer

  Revision 1.19  2001/04/04 12:20:34  lazarus
  MG: added  add to/remove from project, small bugfixes

  Revision 1.18  2001/03/29 12:38:59  lazarus
  MG: new environment opts, ptApplication bugfixes

  Revision 1.17  2001/03/26 14:52:30  lazarus
  MG: TSourceLog + compiling bugfixes

  Revision 1.16  2001/03/19 14:00:47  lazarus
  MG: fixed many unreleased DC and GDIObj bugs

  Revision 1.15  2001/03/09 17:54:45  lazarus

  Fixed error in Windows section of OnLoadSaveFilename - missing ')'

  Revision 1.14  2001/03/09 11:38:20  lazarus
  auto load last project

  Revision 1.10  2001/03/03 11:06:15  lazarus
  added project support, codetools

  Revision 1.8  2001/02/22 17:04:57  lazarus
  added environment options + killed ide unit circles

  Revision 1.7  2001/02/08 06:08:13  lazarus
  Began adding code to save project to the output directory. Added TODO
  comments and cleaned up some of the code.                            CAW

  Revision 1.6  2001/01/31 13:03:33  lazarus
  Commitng source with new editor.
  Shane

  Revision 1.5  2001/01/31 06:28:41  lazarus
  Removed global unit.
  Renamed TProjectUnitInfo to TUnitInfo.
  Added Source property to both TUnitInfo and TProject to hold source code
    for units and project.
  Added functions to load and save units to TUnitInfo.
  Added code to save and load units when a project is saved and loaded.  CAW

  Revision 1.4  2001/01/29 05:42:41  lazarus
  Created new TProjectUnitInfo class.
  Created new TProject class. Saves to XML config file.
  Moved compiler options to write to the project file.            CAW

  Revision 1.3  2001/01/04 20:33:53  lazarus
  Moved lresources.
  Moved CreateLFM to Main.pp
  Changed Form1 and TFOrm1 to MainIDE and TMainIDE
  Shane

  Revision 1.2  2000/12/19 18:43:13  lazarus
  Removed IDEEDITOR.  This causes the PROJECT class to not function.
  Saving projects no longer works.

  I added TSourceNotebook and TSourceEditor.  They do all the work for saving/closing/opening units.  Somethings work but they are in early development.
  Shane

  Revision 1.1  2000/07/13 10:27:48  michael
  + Initial import

}
