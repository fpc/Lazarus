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
  Classes, SysUtils, LCLLinux, LCLType, Laz_XMLCfg, LazConf, CompilerOptions,
  FileCtrl, CodeToolManager, CodeCache, Forms, Controls, EditorOptions, Dialogs,
  IDEProcs, RunParamsOpts, ProjectDefs, EditDefineTree, DefineTemplates;

type
  TUnitInfo = class;
  TProject = class;

  TOnFileBackup = function(const FileToBackup:string; 
                           IsPartOfProject:boolean):TModalResult of object;
  TOnUnitNameChange = procedure(AnUnitInfo: TUnitInfo;
       const OldUnitName, NewUnitName: string;
       CheckIfAllowed: boolean;
       var Allowed: boolean) of object;

  //---------------------------------------------------------------------------
  TNewUnitType = (
     nuEmpty,   // no code
     nuUnit,    // unit
     nuForm,    // unit with form
     nuCustomProgram  // program
   );

      
  TUnitInfo = class(TObject)
  private
    { Variables }
    fAutoRevertLockCount: integer;
    fBreakpoints: TProjectBreakPointList;
    fCursorPos: TPoint;
    fCustomHighlighter: boolean; // do not change highlighter on file extension change
    fEditorIndex: integer;
    fFileName: string;
    fForm: TComponent;
    fFormName: string; { classname is always T<FormName>
         this attribute contains the formname even if the unit is not loaded }
    fFormResourceName: string;
    fHasResources: boolean; // source has resource file
    FIgnoreFileDateOnDiskValid: boolean;
    FIgnoreFileDateOnDisk: longint;
    fIsPartOfProject: boolean;
    fLoaded: Boolean;  // loaded in the source editor
    fModified: boolean;
    fOnFileBackup: TOnFileBackup;
    fOnLoadSaveFilename: TOnLoadSaveFilename;
    fOnUnitNameChange: TOnUnitNameChange;
    FProject: TProject;
    fReadOnly:  Boolean;
    FResourceFilename: string;
    fSource: TCodeBuffer;
    fSyntaxHighlighter: TLazSyntaxHighlighter;
    fTopLine: integer;
    fUnitName: String;

    function GetFileName: string;
    function GetHasResources:boolean;
    procedure SetEditorIndex(const AValue: integer);
    procedure SetForm(const AValue: TComponent);
    procedure SetLoaded(const AValue: Boolean);
    procedure SetProject(const AValue: TProject);
    procedure SetReadOnly(const NewValue: boolean);
    procedure SetSource(ABuffer: TCodeBuffer);
    procedure SetUnitName(const NewUnitName:string);
  protected
    fNextUnitWithEditorIndex: TUnitInfo;
    fPrevUnitWithEditorIndex: TUnitInfo;
    fNextUnitWithForm: TUnitInfo;
    fPrevUnitWithForm: TUnitInfo;
    fNextLoadedUnit: TUnitInfo;
    fPrevLoadedUnit: TUnitInfo;
    fNextAutoRevertLockedUnit: TUnitInfo;
    fPrevAutoRevertLockedUnit: TUnitInfo;
    procedure UpdateEditorIndexList;
    procedure UpdateFormList;
    procedure UpdateLoadedList;
    procedure UpdateAutoRevertLockedList;
  public
    constructor Create(ACodeBuffer: TCodeBuffer);
    destructor Destroy; override;

    function ReadUnitSource(ReadUnitName:boolean): TModalResult;
    procedure ReadUnitNameFromSource;
    function WriteUnitSource: TModalResult;
    function WriteUnitSourceToFile(const AFileName: string): TModalResult;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure Clear;
    procedure CreateStartCode(NewUnitType: TNewUnitType;
         const NewUnitName: string);
    function IsVirtual: boolean;
    function IsMainUnit: boolean;
    procedure IncreaseAutoRevertLock;
    procedure DecreaseAutoRevertLock;
    function IsAutoRevertLocked: boolean;
    function ChangedOnDisk(CompareOnlyLoadSaveTime: boolean): boolean;
    procedure IgnoreCurrentFileDateOnDisk;
    function ShortFilename: string;
    function NeedsSaveToDisk: boolean;

    { Properties }
  public
    property NextUnitWithEditorIndex: TUnitInfo read fNextUnitWithEditorIndex;
    property PrevUnitWithEditorIndex: TUnitInfo read fPrevUnitWithEditorIndex;
    property NextUnitWithForm: TUnitInfo read fNextUnitWithForm;
    property PrevUnitWithForm: TUnitInfo read fPrevUnitWithForm;
    property NextLoadedUnit: TUnitInfo read fNextLoadedUnit;
    property PrevLoadedUnit: TUnitInfo read fPrevLoadedUnit;
    property NextAutoRevertLockedUnit: TUnitInfo read fNextAutoRevertLockedUnit;
    property PrevAutoRevertLockedUnit: TUnitInfo read fPrevAutoRevertLockedUnit;
  public
    property Breakpoints: TProjectBreakPointList
        read fBreakpoints write fBreakpoints;
    property CursorPos: TPoint read fCursorPos write fCursorPos;
    property CustomHighlighter: boolean
          read fCustomHighlighter write fCustomHighlighter;
    property EditorIndex:integer read fEditorIndex write SetEditorIndex;
    property Filename: String read GetFilename;
    property Form: TComponent read fForm write SetForm;
    property FormName: string read fFormName write fFormName;
    property FormResourceName: string
          read fFormResourceName write fFormResourceName;
    property HasResources: boolean read GetHasResources write fHasResources;
    property IsPartOfProject: boolean
          read fIsPartOfProject write fIsPartOfProject;
    property Loaded: Boolean read fLoaded write SetLoaded;
    property Modified: boolean read fModified write fModified;
    property OnFileBackup: TOnFileBackup read fOnFileBackup write fOnFileBackup;
    property OnLoadSaveFilename: TOnLoadSaveFilename
          read fOnLoadSaveFilename write fOnLoadSaveFilename;
    property OnUnitNameChange: TOnUnitNameChange
          read fOnUnitNameChange write fOnUnitNameChange;
    property Project: TProject read FProject write SetProject;
    property ReadOnly: Boolean read fReadOnly write SetReadOnly;
    property ResourceFileName: string
          read FResourceFilename write FResourceFilename;
    property Source: TCodeBuffer read fSource write SetSource;
    property SyntaxHighlighter: TLazSyntaxHighlighter
          read fSyntaxHighlighter write fSyntaxHighlighter;
    property TopLine: integer read fTopLine write fTopLine;
    property UnitName: String read fUnitName write SetUnitName;
  end;


  //---------------------------------------------------------------------------
  TProjectType =   // for a description see ProjectTypeDescriptions below
     (ptApplication, ptProgram, ptCustomProgram); 
  TProjectFlag = (pfSaveClosedUnits, pfSaveOnlyProjectUnits);
  TProjectFlags = set of TProjectFlag;

  TProject = class(TObject)
  private
    FFlags: TProjectFlags;
    xmlconfig: TXMLConfig;

    { Variables }
    fActiveEditorIndexAtStart: integer;
    fBookmarks: TProjectBookmarkList;
    fCompilerOptions: TCompilerOptions;
    fFirstAutoRevertLockedUnit: TUnitInfo; // units with IsAutoRevertLocked=true
    fFirstLoadedUnit: TUnitInfo; // units with Loaded=true
    fFirstUnitWithEditorIndex: TUnitInfo;// units with EditorIndex>=0
    fFirstUnitWithForm: TUnitInfo; // units with Form<>nil
    fIconPath: String;
    fJumpHistory: TProjectJumpHistory;
    fMainUnit: Integer;  // only for ptApplication
    fModified: boolean;
    fOnFileBackup: TOnFileBackup;
    fOutputDirectory: String;
    fProjectInfoFile: String;  // the lpi filename
    fProjectType: TProjectType;
    fSrcPath: string; // source path addition for units in ProjectDir
    fTargetFileExt: String;
    fTitle: String;
    fUnitList: TList;  // list of TUnitInfo
    fUnitOutputDirectory: String;
    fRunParameterOptions: TRunParamsOptions;
    
    function GetMainFilename: String;
    function GetMainUnitInfo: TUnitInfo;
    function GetProjectInfoFile: string;
    function GetTargetFilename: string;
    function GetUnits(Index: integer): TUnitInfo;
    procedure SetFlags(const AValue: TProjectFlags);
    procedure SetMainUnit(const AValue: Integer);
    procedure SetUnits(Index:integer; AUnitInfo: TUnitInfo);
    procedure SetProjectInfoFile(const NewFilename: string);
    procedure SetTargetFilename(const NewTargetFilename: string);
    procedure OnLoadSaveFilename(var AFilename: string; Load: boolean);
    function OnUnitFileBackup(const Filename: string;
                              IsPartOfProject:boolean): TModalResult;
    procedure OnUnitNameChange(AnUnitInfo: TUnitInfo; 
       const OldUnitName, NewUnitName: string;  CheckIfAllowed: boolean;
       var Allowed: boolean);
    function JumpHistoryCheckPosition(
       APosition:TProjectJumpHistoryPosition): boolean;
    procedure SetSrcPath(const NewSrcPath: string);
  protected
    procedure AddToEditorWithIndexList(AnUnitInfo: TUnitInfo);
    procedure RemoveFromEditorWithIndexList(AnUnitInfo: TUnitInfo);
    procedure AddToFormList(AnUnitInfo: TUnitInfo);
    procedure RemoveFromFormList(AnUnitInfo: TUnitInfo);
    procedure AddToLoadedList(AnUnitInfo: TUnitInfo);
    procedure RemoveFromLoadedList(AnUnitInfo: TUnitInfo);
    procedure AddToAutoRevertLockedList(AnUnitInfo: TUnitInfo);
    procedure RemoveFromAutoRevertLockedList(AnUnitInfo: TUnitInfo);
  public
    constructor Create(TheProjectType: TProjectType);
    destructor Destroy; override;

    function ReadProject(const LPIFilename: string): TModalResult;
    function WriteProject: TModalResult;

    property Units[Index: integer]:TUnitInfo read GetUnits write SetUnits;
    function UnitCount:integer;
    function NewUniqueUnitName(NewUnitType:TNewUnitType): string;
    function NewUniqueFormName(NewUnitType:TNewUnitType): string;
    procedure AddUnit(AUnit: TUnitInfo; AddToProjectFile: boolean);
    procedure RemoveUnit(Index: integer);
    function IndexOf(AUnitInfo: TUnitInfo): integer;
    function IndexOfUnitWithName(const AnUnitName: string;
       OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo): integer;
    function IndexOfUnitWithForm(AForm: TComponent;
       OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo): integer;
    function IndexOfUnitWithFormName(const AFormName: string;
       OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo): integer;
    function IndexOfFilename(const AFilename: string): integer;
    function UnitWithEditorIndex(Index:integer): TUnitInfo;
    Function UnitWithForm(AForm: TComponent): TUnitInfo;
    
    procedure CloseEditorIndex(EditorIndex:integer);
    procedure InsertEditorIndex(EditorIndex:integer);
    procedure MoveEditorIndex(OldEditorIndex, NewEditorIndex: integer);
    procedure AddToOrRemoveFromEditorWithIndexList(AnUnitInfo: TUnitInfo);
    procedure AddToOrRemoveFromFormList(AnUnitInfo: TUnitInfo);
    procedure AddToOrRemoveFromLoadedList(AnUnitInfo: TUnitInfo);
    procedure AddToOrRemoveFromAutoRevertLockedList(AnUnitInfo: TUnitInfo);

    procedure Clear;
    function SomethingModified: boolean;
    function AddCreateFormToProjectFile(const AClassName,AName:string):boolean;
    function RemoveCreateFormFromProjectFile(const AClassName,AName:string):boolean;
    function FormIsCreatedInProjectFile(const AClassname,AName:string):boolean;
    function UnitIsUsed(const ShortUnitName:string):boolean;
    function GetResourceFile(AnUnitInfo: TUnitInfo; Index:integer):TCodeBuffer;
    function SearchFile(const Filename,SearchPaths,InitialDir:string):string;
    function GetMainResourceFilename(AnUnitInfo: TUnitInfo): string;
    function IsVirtual: boolean;
    function RemoveProjectPathFromFilename(const AFilename: string): string;
    function ProjectDirectory: string;
    function FileIsInProjectDir(const AFilename: string): boolean;
    
    procedure GetUnitsChangedOnDisk(var AnUnitList: TList);

    property ActiveEditorIndexAtStart: integer 
       read fActiveEditorIndexAtStart write fActiveEditorIndexAtStart;
    property Bookmarks: TProjectBookmarkList read fBookmarks write fBookmarks;
    property CompilerOptions: TCompilerOptions 
       read fCompilerOptions write fCompilerOptions;
    property FirstAutoRevertLockedUnit: TUnitInfo read fFirstAutoRevertLockedUnit;
    property FirstLoadedUnit: TUnitInfo read fFirstLoadedUnit;
    property FirstUnitWithEditorIndex: TUnitInfo read fFirstUnitWithEditorIndex;
    property FirstUnitWithForm: TUnitInfo read fFirstUnitWithForm;

    property Flags: TProjectFlags read FFlags write SetFlags;
    property IconPath: String read fIconPath write fIconPath;
    property JumpHistory: TProjectJumpHistory
       read fJumpHistory write fJumpHistory;
    property MainUnit: Integer //this is the unit index of the program file
       read fMainUnit write SetMainUnit;
    property MainUnitInfo: TUnitInfo read GetMainUnitInfo;
    property Modified: boolean read fModified write fModified;
    property OnFileBackup: TOnFileBackup read fOnFileBackup write fOnFileBackup;
    property OutputDirectory: String read fOutputDirectory write fOutputDirectory;
    property MainFilename: String read GetMainFilename;
    property ProjectInfoFile: string
       read GetProjectInfoFile write SetProjectInfoFile;
    property ProjectType: TProjectType read fProjectType write fProjectType;
    property RunParameterOptions: TRunParamsOptions read fRunParameterOptions;
    property SrcPath: string read fSrcPath write fSrcPath;
    property TargetFileExt: String read fTargetFileExt write fTargetFileExt;
    property TargetFilename: string read GetTargetFilename write SetTargetFilename;
    property Title: String read fTitle write fTitle;
    property UnitOutputDirectory: String
       read fUnitOutputDirectory write fUnitOutputDirectory;
  end;

const
  ResourceFileExt = '.lrs';

  ProjectTypeNames : array[TProjectType] of string = (
      'Application', 'Program', 'Custom program'
    );

  ProjectTypeDescriptions : array[TProjectType] of string = (
      // ptApplication
      'Application'#13
      +'A graphical lcl/freepascal program. The program file is '
      +'automatically maintained by lazarus.'#13
      +#13
      +#13
      +'WARNING:'#13
      +'Form editing is under development and should not be used.'

      // ptProgram
      ,'Program:'#13
      +'A freepascal program. The program file is automatically '
      +'maintained by lazarus.'

      // ptCustomProgram
      ,'Custom program:'#13
      +'A freepascal program.'
    );

  ProjectDefaultExt : array[TProjectType] of string = (
      '.lpr','.pas','.pas'
    );
    
  UnitTypeDefaultExt: array[TNewUnitType] of string = (
      '.pas', '.pas', '.pas', '.pas'
    );

  DefaultTargetFileExt : string = {$IFDEF win32}'.exe'{$ELSE}''{$ENDIF};
  
  DefaultProjectFlags = [pfSaveClosedUnits];
  ProjectFlagNames : array[TProjectFlag] of string = (
      'SaveClosedFiles', 'SaveOnlyProjectUnits'
    );

function ProjectTypeNameToType(const s:string): TProjectType;
function ProjectFlagsToStr(Flags: TProjectFlags): string;

implementation

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
  fBreakPoints:=TProjectBreakPointList.Create;
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
  fBreakPoints.Free;
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
function TUnitInfo.ReadUnitSource(ReadUnitName:boolean): TModalResult;
var 
  ACaption:string;
  AText:string;
  NewSource: TCodeBuffer;
begin
  repeat
    NewSource:=CodeToolBoss.LoadFile(fFilename,true,false);
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

procedure TUnitInfo.ReadUnitNameFromSource;
begin
  fUnitName:=CodeToolBoss.GetSourceName(fSource,false);
end;

{------------------------------------------------------------------------------
  TUnitInfo Clear
 ------------------------------------------------------------------------------}
procedure TUnitInfo.Clear;
begin
  fBreakPoints.Clear;
  fCursorPos.X := -1;
  fCursorPos.Y := -1;
  fCustomHighlighter := false;
  fEditorIndex := -1;
  fFilename := '';
  fForm := nil;
  fFormName := '';
  fFormResourceName := '';
  fHasResources := false;
  FIgnoreFileDateOnDiskValid:=false;
  fIsPartOfProject := false;
  Loaded := false;
  fModified := false;
  fReadOnly := false;
  if fSource<>nil then fSource.Clear;
  fSyntaxHighlighter := lshText;
  fTopLine := -1;
  fUnitName := '';
end;


{------------------------------------------------------------------------------
  TUnitInfo SaveToXMLConfig
 ------------------------------------------------------------------------------}
procedure TUnitInfo.SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
var AFilename:string;
begin
  XMLConfig.SetValue(Path+'CursorPos/X',fCursorPos.X);
  XMLConfig.SetValue(Path+'CursorPos/Y',fCursorPos.Y);
  XMLConfig.SetValue(Path+'EditorIndex/Value',fEditorIndex);
  AFilename:=Filename;
  if Assigned(fOnLoadSaveFilename) then
    fOnLoadSaveFilename(AFilename,false);
  XMLConfig.SetValue(Path+'Filename/Value',AFilename);
  XMLConfig.SetValue(Path+'FormName/Value',fFormName);
  XMLConfig.SetValue(Path+'HasResources/Value',fHasResources);
  XMLConfig.SetValue(Path+'IsPartOfProject/Value',fIsPartOfProject);
  XMLConfig.SetValue(Path+'Loaded/Value',fLoaded);
  XMLConfig.SetValue(Path+'ReadOnly/Value',fReadOnly);
  AFilename:=FResourceFilename;
  if Assigned(fOnLoadSaveFilename) then
    fOnLoadSaveFilename(AFilename,false);
  XMLConfig.SetValue(Path+'ResourceFilename/Value',AFilename);
  XMLConfig.SetValue(Path+'SyntaxHighlighter/Value'
     ,LazSyntaxHighlighterNames[fSyntaxHighlighter]);
  XMLConfig.SetValue(Path+'TopLine/Value',fTopLine);
  XMLConfig.SetValue(Path+'UnitName/Value',fUnitName);
  fBreakpoints.SaveToXMLConfig(XMLConfig,Path);
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
  fFormName:=XMLConfig.GetValue(Path+'FormName/Value','');
  HasResources:=XMLConfig.GetValue(Path+'HasResources/Value',false);
  fIsPartOfProject:=XMLConfig.GetValue(Path+'IsPartOfProject/Value',false);
  Loaded:=XMLConfig.GetValue(Path+'Loaded/Value',false);
  fReadOnly:=XMLConfig.GetValue(Path+'ReadOnly/Value',false);
  AFilename:=XMLConfig.GetValue(Path+'ResourceFilename/Value','');
  if Assigned(fOnLoadSaveFilename) then
    fOnLoadSaveFilename(AFilename,true);
  FResourceFilename:=AFilename;
  if (FResourceFilename<>'')
  and (FResourceFilename[length(FResourceFilename)]=PathDelim) then
    FResourceFilename:='';
  fSyntaxHighlighter:=StrToLazSyntaxHighlighter(XMLConfig.GetValue(
       Path+'SyntaxHighlighter/Value',''));
  fTopLine:=XMLConfig.GetValue(Path+'TopLine/Value',-1);
  UnitName:=XMLConfig.GetValue(Path+'UnitName/Value','');
  fBreakpoints.LoadFromXMLConfig(XMLConfig,Path);
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
    fModified:=true;
  end;
end;

procedure TUnitInfo.UpdateEditorIndexList;
begin
  if Project<>nil then begin
    Project.AddToOrRemoveFromEditorWithIndexList(Self);
  end else begin
    fNextUnitWithEditorIndex:=nil;
    fPrevUnitWithEditorIndex:=nil;
  end;
end;

procedure TUnitInfo.UpdateFormList;
begin
  if Project<>nil then begin
    Project.AddToOrRemoveFromFormList(Self);
  end else begin
    fNextUnitWithForm:=nil;
    fPrevUnitWithForm:=nil;
  end;
end;

procedure TUnitInfo.UpdateLoadedList;
begin
  if Project<>nil then begin
    Project.AddToOrRemoveFromLoadedList(Self);
  end else begin
    fNextLoadedUnit:=nil;
    fPrevLoadedUnit:=nil;
  end;
end;

procedure TUnitInfo.UpdateAutoRevertLockedList;
begin
  if Project<>nil then begin
    Project.AddToOrRemoveFromAutoRevertLockedList(Self);
  end else begin
    fNextAutoRevertLockedUnit:=nil;
    fPrevAutoRevertLockedUnit:=nil;
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
  end;
end;

procedure TUnitInfo.SetReadOnly(const NewValue: boolean);
begin
  fReadOnly:=NewValue;
  if fSource<>nil then
    fSource.ReadOnly:=fReadOnly;
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
  LE:=EndOfLine;
  if NewUnitType in [nuForm,nuUnit] then begin
    fUnitName:=NewUnitName;
    AResourceFilename:=fUnitName+ResourceFileExt;
    NewSource:=Beautified(
       'unit '+fUnitName+';'+LE
      +LE
      +'{$mode objfpc}{$H+}'+LE
      ++LE
      +'interface'+LE
      ++LE
      +'uses'+LE);
    case NewUnitType of
     nuUnit:
      begin
        NewSource:=NewSource+Beautified(
          '  Classes, SysUtils;'+LE
          +LE
          +'implementation'+LE);
      end;
     nuForm:
      begin
        NewSource:=NewSource+Beautified(
          '  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources;'+LE
          +LE
          +'type'+LE
          +'  T'+fFormName+' = class(TForm)'+LE
          +'  private'+LE);
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
          +'  '+fFormName+': T'+fFormName+';'+LE
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
  Result:=fHasResources or (FormName<>'');
end;

procedure TUnitInfo.SetEditorIndex(const AValue: integer);
begin
  if fEditorIndex=AValue then exit;
  fEditorIndex:=AValue;
  UpdateEditorIndexList;
end;

procedure TUnitInfo.SetForm(const AValue: TComponent);
begin
  if fForm=AValue then exit;
  fForm:=AValue;
  UpdateFormList;
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
  if fLoaded then
    IncreaseAutoRevertLock
  else
    DecreaseAutoRevertLock;
end;

procedure TUnitInfo.SetProject(const AValue: TProject);
begin
  if FProject=AValue then exit;
  if AValue=nil then begin
    Project.RemoveFromEditorWithIndexList(Self);
    Project.RemoveFromFormList(Self);
    Project.RemoveFromLoadedList(Self);
    Project.RemoveFromAutoRevertLockedList(Self);
  end;
  FProject:=AValue;
  UpdateEditorIndexList;
  UpdateFormList;
  UpdateLoadedList;
  UpdateAutoRevertLockedList;
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
  fBookmarks := TProjectBookmarkList.Create;
  fCompilerOptions := TCompilerOptions.Create;
  FFlags:=DefaultProjectFlags;
  fIconPath := '';
  fJumpHistory:=TProjectJumpHistory.Create;
  fJumpHistory.OnCheckPosition:=@JumpHistoryCheckPosition;
  fJumpHistory.OnLoadSaveFilename:=@OnLoadSaveFilename;
  fMainUnit := -1;
  fModified := false;
  fOutputDirectory := '.';
  fProjectInfoFile := '';
  fRunParameterOptions:=TRunParamsOptions.Create;
  fSrcPath := '';
  fTargetFileExt := DefaultTargetFileExt;
  fTitle := '';
  fUnitList := TList.Create;  // list of TUnitInfo
  fUnitOutputDirectory := '.';

  // create program source
  NewSource:=TStringList.Create;
  case fProjectType of
   ptProgram, ptApplication, ptCustomProgram:
    begin
      NewPrgBuf:=CodeToolBoss.CreateFile(
        'project1'+ProjectDefaultExt[fProjectType]);
      PrgUnitInfo:=TUnitInfo.Create(NewPrgBuf);
      PrgUnitInfo.IsPartOfProject:=true;
      PrgUnitInfo.SyntaxHighlighter:=
        ExtensionToLazSyntaxHighlighter(ProjectDefaultExt[fProjectType]);
      AddUnit(PrgUnitInfo,false);
      MainUnit:=0;
      with NewSource do begin
        Add('program Project1;');
        Add('');
        Add('{$mode objfpc}{$H+}');
        Add('');
        Add('uses');
        case fProjectType of
          ptProgram, ptCustomProgram:  Add('  Classes;');
          ptApplication:  Add('  Forms;');
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
  Clear;
  fBookmarks.Free;
  if (xmlconfig <> nil) then xmlconfig.Free;
  fUnitList.Free;
  fJumpHistory.Free;
  fRunParameterOptions.Free;
  fCompilerOptions.Free;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TProject WriteProject
 ------------------------------------------------------------------------------}
function TProject.WriteProject: TModalResult;

  procedure SaveFlags;
  var f: TProjectFlag;
  begin
    for f:=Low(TProjectFlag) to High(TProjectFlag) do begin
      xmlconfig.SetValue('ProjectOptions/General/Flags/'
            +ProjectFlagNames[f]+'/Value', f in Flags);
    end;
  end;
  
  function UnitMustBeSaved(i: integer): boolean;
  begin
    Result:=false;
    if (pfSaveOnlyProjectUnits in Flags) and (not Units[i].IsPartOfProject) then
      exit;
    if (not (pfSaveClosedUnits in Flags)) and (not Units[i].IsPartOfProject)
    and (not Units[i].Loaded) then exit;
    Result:=true;
  end;
  
  procedure SaveUnits;
  var i, SaveUnitCount: integer;
  begin
    SaveUnitCount:=0;
    for i:=0 to UnitCount-1 do begin
      if UnitMustBeSaved(i) then begin
        Units[i].SaveToXMLConfig(
          xmlconfig,'ProjectOptions/Units/Unit'+IntToStr(SaveUnitCount)+'/');
        inc(SaveUnitCount);
      end;
    end;
    xmlconfig.SetValue('ProjectOptions/Units/Count',SaveUnitCount);
  end;


var
  confPath: String;
  AText, ACaption: string;
begin
  Result := mrCancel;

  confPath := ProjectInfoFile;
  if Assigned(fOnFileBackup) then begin
    Result:=fOnFileBackup(confPath,true);
    if Result=mrAbort then exit;
  end;
  xmlconfig := TXMLConfig.Create(SetDirSeparators(confPath));

  try
    repeat
      try
        xmlconfig.SetValue('ProjectOptions/General/ProjectType/Value',
            ProjectTypeNames[ProjectType]);
        SaveFlags;
        xmlconfig.SetValue('ProjectOptions/General/MainUnit/Value', MainUnit);
        xmlconfig.SetValue('ProjectOptions/General/ActiveEditorIndexAtStart/Value'
            ,ActiveEditorIndexAtStart);
        xmlconfig.SetValue('ProjectOptions/General/IconPath/Value', IconPath);
        xmlconfig.SetValue('ProjectOptions/General/TargetFileExt/Value'
            ,TargetFileExt);
        xmlconfig.SetValue('ProjectOptions/General/Title/Value', Title);
        xmlconfig.SetValue('ProjectOptions/General/OutputDirectory/Value'
            ,OutputDirectory);
        xmlconfig.SetValue('ProjectOptions/General/UnitOutputDirectory/Value'
            ,UnitOutputDirectory);
        fBookmarks.SaveToXMLConfig(xmlconfig,'ProjectOptions/');
        fJumpHistory.DeleteInvalidPositions;
        fJumpHistory.SaveToXMLConfig(xmlconfig,'ProjectOptions/');
        xmlconfig.SetValue('ProjectOptions/General/SrcPath/Value',fSrcPath);

        SaveUnits;

        // Save the compiler options
        CompilerOptions.XMLConfigFile := xmlconfig;
        CompilerOptions.ProjectFile := confPath;
        CompilerOptions.SaveCompilerOptions(true);
        
        // save the Run Parameter Options
        RunParameterOptions.Save(xmlconfig,'ProjectOptions/');

        xmlconfig.Flush;
        Modified:=false;
      except
        ACaption:='Write error';
        AText:='Unable to write to file "'+confPath+'".';
        Result:=Application.MessageBox(PChar(ACaption),PChar(AText),MB_ABORTRETRYIGNORE);
        if Result=mrIgnore then Result:=mrOk;
        if Result=mrAbort then exit;
      end;
    until Result<>mrRetry;
  finally
    xmlconfig.Free;
    xmlconfig:=nil;
  end;
  Result := mrOk;
end;

{------------------------------------------------------------------------------
  TProject ReadProject
 ------------------------------------------------------------------------------}
function TProject.ReadProject(const LPIFilename: string): TModalResult;

  procedure LoadFlags;
  var f: TProjectFlag;
  begin
    FFlags:=[];
    for f:=Low(TProjectFlag) to High(TProjectFlag) do begin
      if xmlconfig.GetValue('ProjectOptions/General/Flags/'
            +ProjectFlagNames[f]+'/Value', f in DefaultProjectFlags)
      then
        Include(FFlags,f)
      else
        Exclude(FFlags,f);
    end;
  end;
  
var
  NewUnitInfo: TUnitInfo;
  NewUnitCount,i: integer;
begin
  Result := mrCancel;
  Clear;

  ProjectInfoFile:=LPIFilename;
  try
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject A reading lpi');{$ENDIF}
    xmlconfig := TXMLConfig.Create(ProjectInfoFile);
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject B done lpi');{$ENDIF}
  except
    MessageDlg('Unable to read the project info file'#13'"'+ProjectInfoFile+'".'
        ,mtError,[mbOk],0);
    Result:=mrCancel;
    exit;
  end;

  try
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject C reading values');{$ENDIF}
    ProjectType := ProjectTypeNameToType(xmlconfig.GetValue(
       'ProjectOptions/General/ProjectType/Value', ''));
    LoadFlags;
    MainUnit := xmlconfig.GetValue('ProjectOptions/General/MainUnit/Value', -1);
    ActiveEditorIndexAtStart := xmlconfig.GetValue(
       'ProjectOptions/General/ActiveEditorIndexAtStart/Value', -1);
    IconPath := xmlconfig.GetValue('ProjectOptions/General/IconPath/Value', './');
    TargetFileExt := xmlconfig.GetValue(
       'ProjectOptions/General/TargetFileExt/Value', DefaultTargetFileExt);
    Title := xmlconfig.GetValue('ProjectOptions/General/Title/Value', '');
    OutputDirectory := xmlconfig.GetValue(
       'ProjectOptions/General/OutputDirectory/Value', '.');
    UnitOutputDirectory := xmlconfig.GetValue(
       'ProjectOptions/General/UnitOutputDirectory/Value', '.');
    fBookmarks.LoadFromXMLConfig(xmlconfig,'ProjectOptions/');
    fJumpHistory.LoadFromXMLConfig(xmlconfig,'ProjectOptions/');
    FSrcPath := xmlconfig.GetValue('ProjectOptions/General/SrcPath/Value','');

    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject D reading units');{$ENDIF}
    NewUnitCount:=xmlconfig.GetValue('ProjectOptions/Units/Count',0);
    for i := 0 to NewUnitCount - 1 do begin
      NewUnitInfo:=TUnitInfo.Create(nil);
      AddUnit(NewUnitInfo,false);
      NewUnitInfo.LoadFromXMLConfig(
         xmlconfig,'ProjectOptions/Units/Unit'+IntToStr(i)+'/');
    end;

    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject E reading comp sets');{$ENDIF}
    // Load the compiler options
    CompilerOptions.XMLConfigFile := xmlconfig;
    CompilerOptions.ProjectFile := MainFilename;
    CompilerOptions.LoadCompilerOptions(true);
    CreateProjectDefineTemplate(CompilerOptions,FSrcPath);

    // load the Run Parameter Options
    RunParameterOptions.Load(xmlconfig,'ProjectOptions/');
    
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject update ct boss');{$ENDIF}
    CodeToolBoss.GlobalValues.Variables[ExternalMacroStart+'ProjectDir']:=
          ProjectDirectory;
    CodeToolBoss.DefineTree.ClearCache;
  finally
    {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject freeing xml');{$ENDIF}
    xmlconfig.Free;
    xmlconfig:=nil;
  end;

  {$IFDEF IDE_MEM_CHECK}CheckHeapWrtMemCnt('TProject.ReadProject END');{$ENDIF}
  Result := mrOk;
end;

{------------------------------------------------------------------------------
  TProject AddUnit
 ------------------------------------------------------------------------------}
procedure TProject.AddUnit(AUnit: TUnitInfo; AddToProjectFile:boolean);
var
  ShortUnitName:string;
  NewIndex: integer;
begin
  if (AUnit = nil) then exit;
  NewIndex:=UnitCount;
  fUnitList.Add(AUnit);
  AUnit.Project:=Self;
  AUnit.OnFileBackup:=@OnUnitFileBackup;
  AUnit.OnLoadSaveFilename:=@OnLoadSaveFilename;
  AUnit.OnUnitNameChange:=@OnUnitNameChange;
  
  // check if this is the new Main Unit
  if MainUnit=NewIndex then
    MainUnitInfo.IncreaseAutoRevertLock;

  if AddToProjectFile and (MainUnit>=0) and (MainUnit<>NewIndex) then begin
    // add unit to uses section
    ShortUnitName:=AUnit.UnitName;
    if (ShortUnitName<>'') and (not UnitIsUsed(ShortUnitName)) then
      CodeToolBoss.AddUnitToMainUsesSection(MainUnitInfo.Source,
        ShortUnitName,'');
  end;
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
  if (Index=MainUnit) then begin
    raise Exception.Create('ERROR: TProject.RemoveUnit index = MainUnit');
  end;
  OldUnitInfo:=Units[Index];
  Modified:=true;

  if (MainUnit>=0) then begin
    // remove unit from uses section and from createforms in program file
    if (OldUnitInfo.IsPartOfProject) then begin
      if (OldUnitInfo.UnitName<>'') then
        CodeToolBoss.RemoveUnitFromAllUsesSections(Units[MainUnit].Source,
          OldUnitInfo.UnitName);
      if (OldUnitInfo.FormName<>'') then
        CodeToolBoss.RemoveCreateFormStatement(Units[MainUnit].Source,
          OldUnitInfo.FormName);
    end;
  end;

  // delete bookmarks of this unit
  if OldUnitInfo.EditorIndex>=0 then
    Bookmarks.DeleteAllWithEditorIndex(OldUnitInfo.EditorIndex);

  // adjust MainUnit
  if MainUnit>=Index then dec(fMainUnit);

  // delete unitinfo instance
  OldUnitInfo.Free;
  fUnitList.Delete(Index);
end;

{------------------------------------------------------------------------------
  TProject Clear
 ------------------------------------------------------------------------------}
procedure TProject.Clear;
var i:integer;
begin
  if xmlconfig<>nil then xmlconfig.Free;
  xmlconfig:=nil;

  for i:=0 to UnitCount-1 do Units[i].Free;
  fUnitList.Clear;
  
  fRunParameterOptions.Clear;

  fActiveEditorIndexAtStart := -1;
  fBookmarks.Clear;
  fCompilerOptions.Clear;
  fIconPath := '';
  fJumpHistory.Clear;
  fMainUnit := -1;
  fModified := false;
  fOutputDirectory := '.';
  fProjectInfoFile := '';
  fSrcPath := '';
  fTargetFileExt := {$IFDEF win32}'.exe'{$ELSE}''{$ENDIF};
  fTitle := '';
  fUnitOutputDirectory := '.';
end;

function TProject.GetUnits(Index:integer):TUnitInfo;
begin
  Result:=TUnitInfo(fUnitList[Index]);
end;

procedure TProject.SetFlags(const AValue: TProjectFlags);
begin
  FFlags:=AValue;
end;

procedure TProject.SetMainUnit(const AValue: Integer);
begin
  if fMainUnit=AValue then exit;
  if (fMainUnit>=0) and (fMainUnit<UnitCount) then begin
    MainUnitInfo.DecreaseAutoRevertLock;
  end;
  fMainUnit:=AValue;
  if (fMainUnit>=0) and (fMainUnit<UnitCount) then begin
    MainUnitInfo.IncreaseAutoRevertLock;
  end;
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
    nuForm,nuUnit: Prefix:='unit';
  else Prefix:='text'
  end;
  while (UnitNameExists(Prefix+IntToStr(u))) do inc(u);
  Result:=Prefix+IntToStr(u);
end;

function TProject.NewUniqueFormName(NewUnitType:TNewUnitType):string;

  function FormNameExists(const AFormName: string): boolean;
  var i: integer;
  begin
    Result:=true;
    for i:=0 to UnitCount-1 do begin
      if (Units[i].Form<>nil) then begin
        if AnsiCompareText(Units[i].Form.Name,AFormName)=0 then exit;
        if AnsiCompareText(Units[i].Form.ClassName,'T'+AFormName)=0 then exit;
      end else if (Units[i].FormName<>'')
      and ((Units[i].IsPartOfProject) or (Units[i].Loaded)) then begin
        if AnsiCompareText(Units[i].FormName,AFormName)=0 then exit;
      end;
    end;
    Result:=false;
  end;

// NewUniqueFormName(NewUnitType:TNewUnitType)
var i: integer;
  Prefix: string;
begin
  i:=1;
  case NewUnitType of
    nuForm, nuUnit: Prefix:='Form'
  else
    Prefix:='form';
  end;
  while (FormNameExists(Prefix+IntToStr(i))) do inc(i);
  Result:=Prefix+IntToStr(i);
end;

function TProject.AddCreateFormToProjectFile(
  const AClassName,AName:string):boolean;
begin
  Result:=CodeToolBoss.AddCreateFormStatement(Units[MainUnit].Source,
    AClassName,AName);
  if Result then Modified:=true;
end;

function TProject.RemoveCreateFormFromProjectFile(
  const AClassName,AName:string):boolean;
begin
  Result:=CodeToolBoss.RemoveCreateFormStatement(Units[MainUnit].Source,
              AName);
  if Result then Modified:=true;
end;

function TProject.FormIsCreatedInProjectFile(
  const AClassname,AName:string):boolean;
var p: integer;
begin
  Result:=(CodeToolBoss.FindCreateFormStatement(Units[MainUnit].Source,
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

function TProject.IndexOfUnitWithForm(AForm: TComponent; 
  OnlyProjectUnits:boolean; IgnoreUnit: TUnitInfo):integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if (OnlyProjectUnits and Units[Result].IsPartOfProject) 
    or (not OnlyProjectUnits)
    and (IgnoreUnit<>Units[Result]) then begin
      if Units[Result].Form=AForm then
        exit;
    end;
    dec(Result);
  end;
end;

function TProject.IndexOfUnitWithFormName(const AFormName: string;
  OnlyProjectUnits: boolean; IgnoreUnit: TUnitInfo): integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if (OnlyProjectUnits and Units[Result].IsPartOfProject)
    or (not OnlyProjectUnits)
    and (IgnoreUnit<>Units[Result]) then begin
      if (AnsiCompareText(Units[Result].FormName,AFormName)=0)
      or ((Units[Result].Form<>nil)
        and (AnsiCompareText(Units[Result].Form.Name,AFormName)=0))
      then
        exit;
    end;
    dec(Result);
  end;
end;

function TProject.UnitWithEditorIndex(Index:integer):TUnitInfo;
begin
  Result:=fFirstUnitWithEditorIndex;
  while (Result<>nil) and (Result.EditorIndex<>Index) do begin
    Result:=Result.fNextUnitWithEditorIndex;
  end;
end;

function TProject.UnitIsUsed(const ShortUnitName:string):boolean;
var NamePos, InPos: integer;
begin
  Result:=CodeToolBoss.FindUnitInAllUsesSections(Units[MainUnit].Source,
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
  Result:=(MainUnit>=0) and Units[MainUnit].IsVirtual;
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
  AnUnitInfo:=fFirstUnitWithEditorIndex;
  while AnUnitInfo<>nil do begin
    NextUnitInfo:=AnUnitInfo.fNextUnitWithEditorIndex;
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
  AnUnitInfo:=fFirstUnitWithEditorIndex;
  while AnUnitInfo<>nil do begin
    AnUnitInfo.EditorIndex:=MoveIndex(AnUnitInfo.EditorIndex);
    AnUnitInfo:=AnUnitInfo.fNextUnitWithEditorIndex;
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
  AnUnitInfo:=fFirstUnitWithEditorIndex;
  while AnUnitInfo<>nil do begin
    AnUnitInfo.EditorIndex:=MoveIndex(AnUnitInfo.EditorIndex);
    AnUnitInfo:=AnUnitInfo.fNextUnitWithEditorIndex;
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
    RemoveFromEditorWithIndexList(AnUnitInfo);
  end else begin
    AddToEditorWithIndexList(AnUnitInfo);
  end;
end;

procedure TProject.AddToOrRemoveFromFormList(AnUnitInfo: TUnitInfo);
begin
  if AnUnitInfo.Form=nil then begin
    RemoveFromFormList(AnUnitInfo);
  end else begin
    AddToFormList(AnUnitInfo);
  end;
end;

procedure TProject.AddToOrRemoveFromLoadedList(AnUnitInfo: TUnitInfo);
begin
  if not AnUnitInfo.Loaded then begin
    RemoveFromLoadedList(AnUnitInfo);
  end else begin
    AddToLoadedList(AnUnitInfo);
  end;
end;

procedure TProject.AddToOrRemoveFromAutoRevertLockedList(AnUnitInfo: TUnitInfo);
begin
  if not AnUnitInfo.IsAutoRevertLocked then begin
    RemoveFromAutoRevertLockedList(AnUnitInfo);
  end else begin
    AddToAutoRevertLockedList(AnUnitInfo);
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
  if MainUnit>=0 then Result:=Units[MainUnit].Filename
  else Result:='';
end;

function TProject.GetMainUnitInfo: TUnitInfo;
begin
  if (MainUnit>=0) and (MainUnit<UnitCount) then
    Result:=Units[MainUnit]
  else
    Result:=nil;
end;

function TProject.GetProjectInfoFile:string;
begin
  Result:=fProjectInfoFile;
end;

procedure TProject.SetProjectInfoFile(const NewFilename:string);
begin
  if NewFilename='' then exit;
  if (AnsiCompareText(fTitle,ExtractFileNameOnly(fProjectInfoFile))=0)
  or (fProjectInfoFile='') or (fTitle='') then
    fTitle:=ExtractFileNameOnly(NewFilename);
  fProjectInfoFile:=NewFilename;

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

procedure TProject.OnLoadSaveFilename(var AFilename:string; Load:boolean);
var ProjectPath:string;
begin
  ProjectPath:=ProjectDirectory;
  if ProjectPath='' then ProjectPath:=GetCurrentDir;
  DoDirSeparators(AFilename);
  AFilename:=TrimFilename(AFilename);
  if Load then begin
    // make filename absolute
    if (AFilename<>'') and (not FilenameIsAbsolute(AFilename)) then
      AFilename:=ProjectPath+AFilename;
  end else begin
    // try making filename relative to project file
    if (AFilename<>'') and FilenameIsAbsolute(AFilename)
    and (CompareFileNames(copy(AFilename,1,length(ProjectPath)),ProjectPath)=0)
    then
      AFilename:=copy(AFilename,length(ProjectPath)+1,
           length(AFilename)-length(ProjectPath));
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

function TProject.ProjectDirectory: string;
begin
  Result:=ExtractFilePath(ProjectInfoFile);
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

procedure TProject.GetUnitsChangedOnDisk(var AnUnitList: TList);
var
  AnUnitInfo: TUnitInfo;
begin
  AnUnitList:=nil;
  AnUnitInfo:=fFirstAutoRevertLockedUnit;
  while (AnUnitInfo<>nil) do begin
    if AnUnitInfo.ChangedOnDisk(false) then begin
      if AnUnitList=nil then
        AnUnitList:=TList.Create;
      AnUnitList.Add(AnUnitInfo);
    end;
    AnUnitInfo:=AnUnitInfo.fNextAutoRevertLockedUnit;
  end;
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
    if (OldUnitName<>'') and (ProjectType in [ptProgram, ptApplication]) then
    begin
      // rename unit in program uses section
      CodeToolBoss.RenameUsedUnit(Units[MainUnit].Source
        ,OldUnitName,NewUnitName,'');
    end;
  end;
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
end;

Function TProject.UnitWithForm(AForm : TComponent) : TUnitInfo;
begin
  Result:=fFirstUnitWithForm;
  while (Result<>nil) and (Result.Form<>AForm) do
    Result:=Result.fNextUnitWithForm;
end;

function TProject.IndexOfFilename(const AFilename: string): integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if CompareFilenames(AFilename,Units[Result].Filename)=0 then exit;
    dec(Result);
  end;
end;

procedure TProject.SetSrcPath(const NewSrcPath: string);
begin
  if FSrcPath=NewSrcPath then exit;
  fSrcPath:=NewSrcPath;
end;

procedure TProject.AddToEditorWithIndexList(AnUnitInfo: TUnitInfo);
begin
  // add to list if AnUnitInfo is not in list
  if (fFirstUnitWithEditorIndex<>AnUnitInfo)
  and (AnUnitInfo.fNextUnitWithEditorIndex=nil)
  and (AnUnitInfo.fPrevUnitWithEditorIndex=nil) then begin
    AnUnitInfo.fNextUnitWithEditorIndex:=fFirstUnitWithEditorIndex;
    AnUnitInfo.fPrevUnitWithEditorIndex:=nil;
    fFirstUnitWithEditorIndex:=AnUnitInfo;
    if AnUnitInfo.fNextUnitWithEditorIndex<>nil then
      AnUnitInfo.fNextUnitWithEditorIndex.fPrevUnitWithEditorIndex:=AnUnitInfo;
  end;
end;

procedure TProject.RemoveFromEditorWithIndexList(AnUnitInfo: TUnitInfo);
begin
  // remove from list if AnUnitInfo is in list
  if fFirstUnitWithEditorIndex=AnUnitInfo then
    fFirstUnitWithEditorIndex:=AnUnitInfo.fNextUnitWithEditorIndex;
  if AnUnitInfo.fNextUnitWithEditorIndex<>nil then
    AnUnitInfo.fNextUnitWithEditorIndex.fPrevUnitWithEditorIndex:=
      AnUnitInfo.fPrevUnitWithEditorIndex;
  if AnUnitInfo.fPrevUnitWithEditorIndex<>nil then
    AnUnitInfo.fPrevUnitWithEditorIndex.fNextUnitWithEditorIndex:=
      AnUnitInfo.fNextUnitWithEditorIndex;
  AnUnitInfo.fNextUnitWithEditorIndex:=nil;
  AnUnitInfo.fPrevUnitWithEditorIndex:=nil;
end;

procedure TProject.AddToFormList(AnUnitInfo: TUnitInfo);
begin
  // add to list if AnUnitInfo is not in list
  if (fFirstUnitWithForm<>AnUnitInfo)
  and (AnUnitInfo.fNextUnitWithForm=nil)
  and (AnUnitInfo.fPrevUnitWithForm=nil) then begin
    AnUnitInfo.fNextUnitWithForm:=fFirstUnitWithForm;
    AnUnitInfo.fPrevUnitWithForm:=nil;
    fFirstUnitWithForm:=AnUnitInfo;
    if AnUnitInfo.fNextUnitWithForm<>nil then
      AnUnitInfo.fNextUnitWithForm.fPrevUnitWithForm:=AnUnitInfo;
  end;
end;

procedure TProject.RemoveFromFormList(AnUnitInfo: TUnitInfo);
begin
  // remove from list if AnUnitInfo is in list
  if fFirstUnitWithForm=AnUnitInfo then
    fFirstUnitWithForm:=AnUnitInfo.fNextUnitWithForm;
  if AnUnitInfo.fNextUnitWithForm<>nil then
    AnUnitInfo.fNextUnitWithForm.fPrevUnitWithForm:=
      AnUnitInfo.fPrevUnitWithForm;
  if AnUnitInfo.fPrevUnitWithForm<>nil then
    AnUnitInfo.fPrevUnitWithForm.fNextUnitWithForm:=
      AnUnitInfo.fNextUnitWithForm;
  AnUnitInfo.fNextUnitWithForm:=nil;
  AnUnitInfo.fPrevUnitWithForm:=nil;
end;

procedure TProject.AddToLoadedList(AnUnitInfo: TUnitInfo);
begin
  // add to list if AnUnitInfo is not in list
  if (fFirstLoadedUnit<>AnUnitInfo)
  and (AnUnitInfo.fNextLoadedUnit=nil)
  and (AnUnitInfo.fPrevLoadedUnit=nil) then begin
    AnUnitInfo.fNextLoadedUnit:=fFirstLoadedUnit;
    AnUnitInfo.fPrevLoadedUnit:=nil;
    fFirstLoadedUnit:=AnUnitInfo;
    if AnUnitInfo.fNextLoadedUnit<>nil then
      AnUnitInfo.fNextLoadedUnit.fPrevLoadedUnit:=AnUnitInfo;
  end;
end;

procedure TProject.RemoveFromLoadedList(AnUnitInfo: TUnitInfo);
begin
  // remove from list if AnUnitInfo is in list
  if fFirstLoadedUnit=AnUnitInfo then
    fFirstLoadedUnit:=AnUnitInfo.fNextUnitWithForm;
  if AnUnitInfo.fNextLoadedUnit<>nil then
    AnUnitInfo.fNextLoadedUnit.fPrevLoadedUnit:=
      AnUnitInfo.fPrevLoadedUnit;
  if AnUnitInfo.fPrevLoadedUnit<>nil then
    AnUnitInfo.fPrevLoadedUnit.fNextLoadedUnit:=
      AnUnitInfo.fNextLoadedUnit;
  AnUnitInfo.fNextLoadedUnit:=nil;
  AnUnitInfo.fPrevLoadedUnit:=nil;
end;

procedure TProject.AddToAutoRevertLockedList(AnUnitInfo: TUnitInfo);
begin
  // add to list if AnUnitInfo is not in list
  if (fFirstAutoRevertLockedUnit<>AnUnitInfo)
  and (AnUnitInfo.fNextAutoRevertLockedUnit=nil)
  and (AnUnitInfo.fPrevAutoRevertLockedUnit=nil) then begin
    AnUnitInfo.fNextAutoRevertLockedUnit:=fFirstAutoRevertLockedUnit;
    AnUnitInfo.fPrevAutoRevertLockedUnit:=nil;
    fFirstAutoRevertLockedUnit:=AnUnitInfo;
    if AnUnitInfo.fNextAutoRevertLockedUnit<>nil then
      AnUnitInfo.fNextAutoRevertLockedUnit.fPrevAutoRevertLockedUnit:=AnUnitInfo;
  end;
end;

procedure TProject.RemoveFromAutoRevertLockedList(AnUnitInfo: TUnitInfo);
begin
  // remove from list if AnUnitInfo is in list
  if fFirstAutoRevertLockedUnit=AnUnitInfo then
    fFirstAutoRevertLockedUnit:=AnUnitInfo.fNextAutoRevertLockedUnit;
  if AnUnitInfo.fNextAutoRevertLockedUnit<>nil then
    AnUnitInfo.fNextAutoRevertLockedUnit.fPrevAutoRevertLockedUnit:=
      AnUnitInfo.fPrevAutoRevertLockedUnit;
  if AnUnitInfo.fPrevAutoRevertLockedUnit<>nil then
    AnUnitInfo.fPrevAutoRevertLockedUnit.fNextAutoRevertLockedUnit:=
      AnUnitInfo.fNextAutoRevertLockedUnit;
  AnUnitInfo.fNextAutoRevertLockedUnit:=nil;
  AnUnitInfo.fPrevAutoRevertLockedUnit:=nil;
end;


end.



{
  $Log$
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
