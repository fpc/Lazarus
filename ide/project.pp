{
 /***************************************************************************
                  project.pp  -  project utility class file
                  -----------------------------------------
          TProject is responsible for managing a complete project.


              Initial Revision  : Sun Mar 28 23:15:32 CST 1999


 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 ***************************************************************************/

}
unit project;

{$mode objfpc}{$H+}

{$ifdef Trace}
  {$ASSERTIONS ON}
{$endif}

interface

uses
  Classes, SysUtils, LCLLinux, XMLCfg, LazConf, CompilerOptions, FileCtrl,
  CodeToolManager, CodeCache, Forms, Controls, EditorOptions, Dialogs, IDEProcs;

type
  //---------------------------------------------------------------------------
  TProjectBookmark = class
  private
    fCursorPos: TPoint;
    fEditorIndex: integer;
    fID: integer;
  public
    constructor Create;
    constructor Create(X,Y, AnEditorIndex, AnID: integer);
    property CursorPos: TPoint read fCursorPos write fCursorPos;
    property EditorIndex: integer read fEditorIndex write fEditorIndex;
    property ID:integer read fID write fID;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;

  TProjectBookmarkList = class
  private
    FBookmarks:TList;  // list of TProjectBookmark
    function GetBookmarks(Index:integer):TProjectBookmark;
    procedure SetBookmarks(Index:integer;  ABookmark: TProjectBookmark);
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index:integer]:TProjectBookmark
       read GetBookmarks write SetBookmarks; default;
    function Count:integer;
    procedure Delete(Index:integer);
    procedure Clear;
    function Add(ABookmark: TProjectBookmark):integer;
    procedure DeleteAllWithEditorIndex(EditorIndex:integer);
    function IndexOfID(ID:integer):integer;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;


  //---------------------------------------------------------------------------
  TProjectBreakPoint = class
  private
    fLineNumber: integer;
    // ToDo: conditions, active/non active ...
  public
    property LineNumber:integer read fLineNumber write fLineNumber;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
 end;

  TProjectBreakPointList = class
  private
    FBreakPoints:TList;  // list of TProjectBreakPoint
    function GetBreakPoints(Index:integer):TProjectBreakPoint;
    procedure SetBreakPoints(Index:integer;  ABreakPoint: TProjectBreakPoint);
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Index:integer]:TProjectBreakPoint 
       read GetBreakPoints write SetBreakPoints; default;
    function Count:integer;
    procedure Delete(Index:integer);
    procedure Clear;
    function Add(ABreakPoint: TProjectBreakPoint):integer;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;


  //---------------------------------------------------------------------------
  TUnitInfo = class;


  TOnFileBackup = function(const FileToBackup:string; 
                           IsPartOfProject:boolean):TModalResult of object;
  TOnLoadSaveFilename = procedure(var Filename:string; Load:boolean) of object;
  TOnUnitNameChange = procedure(AnUnitInfo: TUnitInfo; 
       const OldUnitName, NewUnitName: string;  var Allowed: boolean) of object;

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
    fBreakpoints: TProjectBreakPointList;
    fCursorPos: TPoint;
    fEditorIndex: integer;
    fFileName: string;
    fForm: TComponent;
    fFormName: string; // classname is always T<fFormName>
        // this attribute contains the formname even if the unit is not loaded
    fHasResources: boolean;
    fIsPartOfProject: boolean;
    fLoaded:  Boolean;  // loaded in the source editor
    fModified: boolean;
    fOnFileBackup: TOnFileBackup;
    fOnLoadSaveFilename: TOnLoadSaveFilename;
    fOnUnitNameChange: TOnUnitNameChange;
    fReadOnly:  Boolean;
    fSource: TCodeBuffer;
    fSyntaxHighlighter: TLazSyntaxHighlighter;
    fTopLine: integer;
    fUnitName: String;

    function GetHasResources:boolean;
    procedure SetUnitName(const NewUnitName:string);
    function GetFileName: string;
    procedure SetReadOnly(const NewValue: boolean);
    procedure SetSource(ABuffer: TCodeBuffer);
  public
    constructor Create(ACodeBuffer: TCodeBuffer);
    destructor Destroy; override;

    function ReadUnitSource(ReadUnitName:boolean): TModalResult;
    procedure ReadUnitNameFromSource;
    function WriteUnitSource: TModalResult;
    function WriteUnitSourceToFile(const AFileName: string): TModalResult;
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure Clear;
    procedure CreateStartCode(NewUnitType: TNewUnitType;
         const NewUnitName: string);

    { Properties }
    property Breakpoints: TProjectBreakPointList
        read fBreakpoints write fBreakpoints;
    property CursorPos: TPoint read fCursorPos write fCursorPos;
    property EditorIndex:integer read fEditorIndex write fEditorIndex;
    property Filename: String read GetFilename;
    property Form: TComponent read fForm write fForm;
    property FormName: string read fFormName write fFormName;
    property HasResources: boolean read GetHasResources write fHasResources;
    property IsPartOfProject:boolean 
        read fIsPartOfProject write fIsPartOfProject;
    property Loaded: Boolean read fLoaded write fLoaded;
    property Modified: boolean read fModified write fModified;
    property OnFileBackup: TOnFileBackup read fOnFileBackup write fOnFileBackup;
    property OnLoadSaveFilename: TOnLoadSaveFilename
        read fOnLoadSaveFilename write fOnLoadSaveFilename;
    property OnUnitNameChange: TOnUnitNameChange
        read fOnUnitNameChange write fOnUnitNameChange;
    property ReadOnly: Boolean read fReadOnly write SetReadOnly;
    property Source: TCodeBuffer read fSource write SetSource;
    property SyntaxHighlighter: TLazSyntaxHighlighter
        read fSyntaxHighlighter write fSyntaxHighlighter;
    property TopLine: integer read fTopLine write fTopLine;
    property UnitName: String read fUnitName write SetUnitName;
    function IsVirtual: boolean;
  end;


  //---------------------------------------------------------------------------
  TProjectType =   // for a description see ProjectTypeDescriptions
     (ptApplication, ptProgram, ptCustomProgram); 

  TProject = class(TObject)
  private
    xmlcfg: TXMLConfig;

    { Variables }
    fActiveEditorIndexAtStart: integer;
    fBookmarks: TProjectBookmarkList;
    fCompilerOptions: TCompilerOptions;
    fIconPath: String;
    fMainUnit: Integer;  // only for ptApplication
    fModified: boolean;
    fOnFileBackup: TOnFileBackup;
    fOutputDirectory: String;
    fProjectFile: String;  // the lpi filename
    fProjectType: TProjectType;
    fTargetFileExt: String;
    fTitle: String;
    fUnitList: TList;  // list of TUnitInfo
    fUnitOutputDirectory: String;

    function GetUnits(Index:integer):TUnitInfo;
    procedure SetUnits(Index:integer; AUnitInfo: TUnitInfo);
    procedure SetProjectFile(const NewProjectFilename: string);
    function OnUnitFileBackup(const Filename:string;
                              IsPartOfProject:boolean):TModalResult;
    function GetProjectInfoFile:string;
    procedure SetProjectInfoFile(const NewFilename:string);
    procedure OnLoadSaveFilename(var AFilename:string; Load:boolean);
    procedure OnUnitNameChange(AnUnitInfo: TUnitInfo; 
       const OldUnitName, NewUnitName: string;  var Allowed: boolean);
  public
    constructor Create(TheProjectType: TProjectType);
    destructor Destroy; override;

    function ReadProject(LPIFilename: string): TModalResult;
    function WriteProject: TModalResult;

    property Units[Index: integer]:TUnitInfo read GetUnits write SetUnits;
    function UnitCount:integer;
    function NewUniqueUnitName(NewUnitType:TNewUnitType):string;
    function NewUniqueFormName(NewUnitType:TNewUnitType):string;
    procedure AddUnit(AUnit: TUnitInfo; AddToProjectFile:boolean);
    procedure RemoveUnit(Index:integer);
    function IndexOf(AUnitInfo: TUnitInfo):integer;
    function IndexOfUnitWithName(const AnUnitName:string;
       OnlyProjectUnits:boolean):integer;
    function IndexOfUnitWithForm(AForm: TComponent;
       OnlyProjectUnits:boolean):integer;

    function UnitWithEditorIndex(Index:integer):TUnitInfo;
    procedure CloseEditorIndex(EditorIndex:integer);
    procedure InsertEditorIndex(EditorIndex:integer);
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

    property ActiveEditorIndexAtStart: integer 
       read fActiveEditorIndexAtStart write fActiveEditorIndexAtStart;
    property Bookmarks: TProjectBookmarkList read fBookmarks write fBookmarks;
    property CompilerOptions: TCompilerOptions 
       read fCompilerOptions write fCompilerOptions;
    property IconPath: String read fIconPath write fIconPath;
    property MainUnit: Integer //this is the unit index of the program file
       read fMainUnit write fMainUnit;
    property Modified: boolean read fModified write fModified;
    property OnFileBackup: TOnFileBackup read fOnFileBackup write fOnFileBackup;
    property OutputDirectory: String read fOutputDirectory write fOutputDirectory;
    property ProjectFile: String read fProjectFile write SetProjectFile;
    property ProjectInfoFile: string
       read GetProjectInfoFile write SetProjectInfoFile;
    property ProjectType: TProjectType read fProjectType write fProjectType;
    property TargetFileExt: String read fTargetFileExt write fTargetFileExt;
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
      +'automatically maintained by lazarus.'

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

function ProjectTypeNameToType(const s:string): TProjectType;


implementation


function ProjectTypeNameToType(const s:string): TProjectType;
begin
  for Result:=Low(TProjectType) to High(TProjectType) do
    if (lowercase(ProjectTypeNames[Result])=lowercase(s)) then exit;
  Result:=ptCustomProgram;
end;

{ TProjectBookmark }

constructor TProjectBookmark.Create;
begin
  inherited Create;
end;

constructor TProjectBookmark.Create(X,Y, AnEditorIndex, AnID: integer);
begin
  inherited Create;
  fCursorPos.X:=X;
  fCursorPos.Y:=Y;
  fEditorIndex:=AnEditorIndex;
  fID:=AnID;
end;

procedure TProjectBookmark.SaveToXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
begin
  XMLConfig.SetValue(Path+'ID',ID);
  XMLConfig.SetValue(Path+'CursorPosX',CursorPos.X);
  XMLConfig.SetValue(Path+'CursorPosY',CursorPos.Y);
  XMLConfig.SetValue(Path+'EditorIndex',EditorIndex);
end;

procedure TProjectBookmark.LoadFromXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
begin
  ID:=XMLConfig.GetValue(Path+'ID',-1);
  CursorPos.X:=XMLConfig.GetValue(Path+'CursorPosX',0);
  CursorPos.Y:=XMLConfig.GetValue(Path+'CursorPosY',0);
  EditorIndex:=XMLConfig.GetValue(Path+'EditorIndex',-1);
end;


{ TProjectBookmarkList }

constructor TProjectBookmarkList.Create;
begin
  inherited Create;
  fBookmarks:=TList.Create;
end;

destructor TProjectBookmarkList.Destroy;
begin
  Clear;
  fBookmarks.Free;
  inherited Destroy;
end;

procedure TProjectBookmarkList.Clear;
var a:integer;
begin
  for a:=0 to fBookmarks.Count-1 do Items[a].Free;
  fBookmarks.Clear;
end;

function TProjectBookmarkList.Count:integer;
begin
  Result:=fBookmarks.Count;
end;

function TProjectBookmarkList.GetBookmarks(Index:integer):TProjectBookmark;
begin
  Result:=TProjectBookmark(fBookmarks[Index]);
end;

procedure TProjectBookmarkList.SetBookmarks(Index:integer;  
  ABookmark: TProjectBookmark);
begin
  fBookmarks[Index]:=ABookmark;
end;

function TProjectBookmarkList.IndexOfID(ID:integer):integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Items[Result].ID<>ID) do dec(Result);
end;

procedure TProjectBookmarkList.Delete(Index:integer);
begin
  Items[Index].Free;
  fBookmarks.Delete(Index);
end;

procedure TProjectBookmarkList.DeleteAllWithEditorIndex(
  EditorIndex:integer);
var i:integer;
begin
  i:=Count-1;
  while (i>=0) do begin
    if Items[i].EditorIndex=EditorIndex then Delete(i);
    dec(i);
  end;
end;

function TProjectBookmarkList.Add(ABookmark: TProjectBookmark):integer;
begin
  Result:=fBookmarks.Add(ABookmark);
end;

procedure TProjectBookmarkList.SaveToXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
var a:integer;
begin
  XMLConfig.SetValue(Path+'Bookmarks/Count',Count);
  for a:=0 to Count-1 do
    Items[a].SaveToXMLConfig(XMLConfig,Path+'Bookmarks/Mark'+IntToStr(a)+'/');
end;

procedure TProjectBookmarkList.LoadFromXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
var a,NewCount:integer;
  NewBookmark:TProjectBookmark;
begin
  Clear;
  NewCount:=XMLConfig.GetValue(Path+'Bookmarks/Count',0);
  for a:=0 to NewCount-1 do begin
    NewBookmark:=TProjectBookmark.Create;
    Add(NewBookmark);
    NewBookmark.LoadFromXMLConfig(XMLConfig,Path+'Bookmarks/Mark'+IntToStr(a)+'/');
  end;
end;


{ TProjectBreakPoint }

procedure TProjectBreakPoint.SaveToXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
begin
  XMLConfig.SetValue(Path+'LineNumber',LineNumber);
end;

procedure TProjectBreakPoint.LoadFromXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
begin
  LineNumber:=XMLConfig.GetValue(Path+'LineNumber',-1);
end;


{ TProjectBreakPointList }

constructor TProjectBreakPointList.Create;
begin
  inherited Create;
  fBreakPoints:=TList.Create;
end;

destructor TProjectBreakPointList.Destroy;
begin
  Clear;
  fBreakPoints.Free;
  inherited Destroy;
end;

procedure TProjectBreakPointList.Clear;
var a:integer;
begin
  for a:=0 to fBreakPoints.Count-1 do Items[a].Free;
  fBreakPoints.Clear;
end;

function TProjectBreakPointList.Count:integer;
begin
  Result:=fBreakPoints.Count;
end;

function TProjectBreakPointList.GetBreakPoints(Index:integer):TProjectBreakPoint;
begin
  Result:=TProjectBreakPoint(fBreakPoints[Index]);
end;

procedure TProjectBreakPointList.SetBreakPoints(Index:integer;
  ABreakPoint: TProjectBreakPoint);
begin
  fBreakPoints[Index]:=ABreakPoint;
end;

procedure TProjectBreakPointList.Delete(Index:integer);
begin
  Items[Index].Free;
  fBreakPoints.Delete(Index);
end;

function TProjectBreakPointList.Add(ABreakPoint: TProjectBreakPoint):integer;
begin
  Result:=fBreakPoints.Add(ABreakPoint);
end;

procedure TProjectBreakPointList.SaveToXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
var a:integer;
begin
  XMLConfig.SetValue(Path+'BreakPoints/Count',Count);
  for a:=0 to Count-1 do
    Items[a].SaveToXMLConfig(XMLConfig,Path+'BreakPoints/Point'+IntToStr(a)+'/');
end;

procedure TProjectBreakPointList.LoadFromXMLConfig(XMLConfig: TXMLConfig; 
  const Path: string);
var a,NewCount:integer;
  NewBreakPoint:TProjectBreakPoint;
begin
  Clear;
  NewCount:=XMLConfig.GetValue(Path+'BreakPoints/Count',0);
  for a:=0 to NewCount-1 do begin
    NewBreakPoint:=TProjectBreakPoint.Create;
    Add(NewBreakPoint);
    NewBreakPoint.LoadFromXMLConfig(XMLConfig
      ,Path+'BreakPoints/Point'+IntToStr(a)+'/');
  end;
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
  fSource := ACodeBuffer;
  if fSource<>nil then fFileName:=fSource.Filename else FFileName:='';
end;

{------------------------------------------------------------------------------
  TUnitInfo Destructor
 ------------------------------------------------------------------------------}
destructor TUnitInfo.Destroy;
begin
  fBreakPoints.Free;
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
    end else
      Result:=mrOk;
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
    NewSource:=CodeToolBoss.LoadFile(fFilename);
    if NewSource=nil then begin
      ACaption:='Read error';
      AText:='Unable to read file "'+fFilename+'"!';
      Result:=Application.MessageBox(PChar(AText),PChar(ACaption)
         ,MB_ABORTRETRYIGNORE);
      if Result in [mrAbort,mrIgnore] then
        exit;
    end else begin
      fSource:=NewSource;
      Result:=mrOk;
    end;
  until Result<>mrRetry;
  if ReadUnitName then 
    fUnitName:=CodeToolBoss.GetSourceName(fSource);
  Result:=mrOk;
end;

procedure TUnitInfo.ReadUnitNameFromSource;
begin
  fUnitName:=CodeToolBoss.GetSourceName(fSource);
end;

{------------------------------------------------------------------------------
  TUnitInfo Clear
 ------------------------------------------------------------------------------}
procedure TUnitInfo.Clear;
begin
  fBreakPoints.Clear;
  fCursorPos.X := -1;
  fCursorPos.Y := -1;
  fEditorIndex := -1;
  fFilename := '';
  fForm := nil;
  fFormName := '';
  fHasResources := false;
  fIsPartOfProject := false;
  fLoaded := false;
  fModified := false;
  fReadOnly := false;
  if fSource<>nil then fSource.Clear;
  fSyntaxHighlighter := lshFreePascal;
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
  XMLConfig.SetValue(Path+'SyntaxHighlighter/Value'
     ,LazSyntaxHighlighterNames[fSyntaxHighlighter]);
  XMLConfig.SetValue(Path+'TopLine/Value',fTopLine);
  XMLConfig.SetValue(Path+'UnitName/Value',fUnitName);
  fBreakpoints.SaveToXMLConfig(XMLConfig,Path);
end;

{------------------------------------------------------------------------------
  TUnitInfo LoadFromXMLConfig
 ------------------------------------------------------------------------------}
procedure TUnitInfo.LoadFromXMLConfig(XMLConfig: TXMLConfig;const Path: string);
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
  fLoaded:=XMLConfig.GetValue(Path+'Loaded/Value',false);
  fReadOnly:=XMLConfig.GetValue(Path+'ReadOnly/Value',false);
  fSyntaxHighlighter:=StrToLazSyntaxHighlighter(XMLConfig.GetValue(
       Path+'SyntaxHighlighter/Value',''));
  fTopLine:=XMLConfig.GetValue(Path+'TopLine/Value',-1);
  UnitName:=XMLConfig.GetValue(Path+'UnitName/Value','');
  fBreakpoints.LoadFromXMLConfig(XMLConfig,Path);
end;

procedure TUnitInfo.SetUnitName(const NewUnitName:string);
var Allowed:boolean;
begin
  if fUnitName<>NewUnitName then begin
    if NewUnitName<>'' then begin
      Allowed:=true;
      if Assigned(fOnUnitNameChange) then
        fOnUnitNameChange(Self,fUnitName,NewUnitName,Allowed);
      if not Allowed then exit;
      if fSource<>nil then begin
        CodeToolBoss.RenameSource(fSource,NewUnitName);
        CodeToolBoss.RenameMainInclude(fSource,NewUnitName+'.lrs',true);
      end;
      fUnitName:=NewUnitName;
      fModified:=true;
    end;
  end;
end;

function TUnitInfo.GetFileName: string;
begin
  if fSource<>nil then Result:=fSource.Filename
  else Result:=fFileName;
end;

function TUnitInfo.IsVirtual: boolean;
begin
  if fSource<>nil then Result:=fSource.IsVirtual
  else Result:=(fFileName<>ExpandFileName(fFileName));
end;

procedure TUnitInfo.SetSource(ABuffer: TCodeBuffer);
begin
  if fSource=ABuffer then exit;
  fSource:=ABuffer;
  fFileName:=ABuffer.FileName;
end;

procedure TUnitInfo.SetReadOnly(const NewValue: boolean);
begin
  fReadOnly:=NewValue;
  if fSource<>nil then fSource.ReadOnly:=fReadOnly;
end;

procedure TUnitInfo.CreateStartCode(NewUnitType: TNewUnitType;
  const NewUnitName: string);
var ResourceFilename:string;
  NewSource: TStringList;
begin
  if fSource=nil then exit;
  NewSource:=TStringList.Create;
  if NewUnitType in [nuForm,nuUnit] then with NewSource do begin
    fUnitName:=NewUnitName;
    ResourceFilename:=fUnitName+ResourceFileExt;
    Add('unit '+fUnitName+';');
    Add('');
    Add('{$mode objfpc}{$H+}');
    Add('');
    Add('interface');
    Add('');
    Add('uses');
    case NewUnitType of
     nuUnit:
      begin
        Add('  Classes, SysUtils;');
        Add('');
        Add('implementation');
      end;
     nuForm:
      begin
        Add('  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LResources;');
        Add('');
        Add('type');
        Add('  T'+fFormName+' = class(TForm)');
        Add('  private');
        Add('    { private declarations }');
        Add('  public');
        Add('    { public declarations }');
        Add('  end;');
        Add('');
        Add('var');
        Add('  '+fFormName+': T'+fFormName+';');
        Add('');
        Add('implementation');
        Add('');
        Add('initialization');
        Add('  {$I '+ResourceFilename+'}');
      end;
    end;
    Add('');
    Add('end.');
    Add('');
  end else if NewUnitType in [nuCustomProgram] then with NewSource do begin
    Add('program CustomProgram;');
    Add('');
    Add('{$mode objfpc}{$H+}');
    Add('');
    Add('uses');
    Add('  Classes, SysUtils;');
    Add('');
    Add('begin');
    Add('end.');
    Add('');
  end;
  fSource.Assign(NewSource);
  NewSource.Free;
  fModified:=true;
end;

function TUnitInfo.GetHasResources:boolean;
begin
  Result:=fHasResources or (FormName<>'');
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
  XMLCfg := nil;

  fProjectType:=TheProjectType;

  fActiveEditorIndexAtStart := -1;
  fBookmarks := TProjectBookmarkList.Create;
  fCompilerOptions := TCompilerOptions.Create;
  fIconPath := '';
  fMainUnit := -1;
  fModified := false;
  fOutputDirectory := '.';
  fProjectFile := '';
  fTargetFileExt := DefaultTargetFileExt;
  fTitle := '';
  fUnitList := TList.Create;  // list of TUnitInfo
  fUnitOutputDirectory := '.';

  // create program source
  NewSource:=TStringList.Create;
  case fProjectType of
   ptProgram, ptApplication, ptCustomProgram:
    begin
      NewPrgBuf:=CodeToolBoss.CreateFile('project1.pas');
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
      Units[MainUnit].Source.Assign(NewSource);
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
  if (XMLCfg <> nil) then XMLCfg.Free;
  fUnitList.Free;
  fCompilerOptions.Free;

  inherited Destroy;
end;

{------------------------------------------------------------------------------
  TProject WriteProject
 ------------------------------------------------------------------------------}
function TProject.WriteProject: TModalResult;
var
  confPath: String;
  i: Integer;
  AText, ACaption: string;
begin
  Result := mrCancel;

  confPath := ChangeFileExt(ProjectFile,'.lpi');
  if Assigned(fOnFileBackup) then begin
    Result:=fOnFileBackup(confPath,true);
    if Result=mrAbort then exit;
  end;
  xmlcfg := TXMLConfig.Create(SetDirSeparators(confPath));

  try
    repeat
      try
        xmlcfg.SetValue('ProjectOptions/General/ProjectType/Value',
            ProjectTypeNames[ProjectType]);
        xmlcfg.SetValue('ProjectOptions/General/MainUnit/Value', MainUnit);
        xmlcfg.SetValue('ProjectOptions/General/ActiveEditorIndexAtStart/Value'
            ,ActiveEditorIndexAtStart);
        xmlcfg.SetValue('ProjectOptions/General/IconPath/Value', IconPath);
        xmlcfg.SetValue('ProjectOptions/General/TargetFileExt/Value'
            ,TargetFileExt);
        xmlcfg.SetValue('ProjectOptions/General/Title/Value', Title);
        xmlcfg.SetValue('ProjectOptions/General/OutputDirectory/Value'
            ,OutputDirectory);
        xmlcfg.SetValue('ProjectOptions/General/UnitOutputDirectory/Value'
            ,UnitOutputDirectory);
        fBookmarks.SaveToXMLConfig(xmlcfg,'ProjectOptions/');

        // Set options for each Unit
        xmlcfg.SetValue('ProjectOptions/Units/Count',UnitCount);
        for i := 0 to UnitCount - 1 do begin
          Units[i].SaveToXMLConfig(
            xmlcfg,'ProjectOptions/Units/Unit'+IntToStr(i)+'/');
        end;

        // Save the compiler options
        CompilerOptions.XMLConfigFile := xmlcfg;
        CompilerOptions.ProjectFile := confPath;
        CompilerOptions.SaveCompilerOptions(true);

        xmlcfg.Flush;
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
    xmlcfg.Free;
    xmlcfg:=nil;
  end;
  Result := mrOk;
end;

{------------------------------------------------------------------------------
  TProject ReadProject
 ------------------------------------------------------------------------------}
function TProject.ReadProject(LPIFilename: string): TModalResult;
var
  NewUnitInfo: TUnitInfo;
  NewUnitCount,i: integer;
begin
  Result := mrCancel;
  Clear;

  ProjectInfoFile:=LPIFilename;
  try
    xmlcfg := TXMLConfig.Create(ProjectInfoFile);
  except
    MessageDlg('Unable to read the project info file "'+ProjectInfoFile+'".'
        ,mtError,[mbOk],0);
    Result:=mrCancel;
    exit;
  end;

  try
    ProjectType := ProjectTypeNameToType(xmlcfg.GetValue(
       'ProjectOptions/General/ProjectType/Value', ''));
    MainUnit := xmlcfg.GetValue('ProjectOptions/General/MainUnit/Value', -1);
    ActiveEditorIndexAtStart := xmlcfg.GetValue(
       'ProjectOptions/General/ActiveEditorIndexAtStart/Value', -1);
    IconPath := xmlcfg.GetValue('ProjectOptions/General/IconPath/Value', './');
    TargetFileExt := xmlcfg.GetValue(
       'ProjectOptions/General/TargetFileExt/Value', DefaultTargetFileExt);
    Title := xmlcfg.GetValue('ProjectOptions/General/Title/Value', '');
    OutputDirectory := xmlcfg.GetValue(
       'ProjectOptions/General/OutputDirectory/Value', '.');
    UnitOutputDirectory := xmlcfg.GetValue(
       'ProjectOptions/General/UnitOutputDirectory/Value', '.');
    fBookmarks.LoadFromXMLConfig(xmlcfg,'ProjectOptions/');

    NewUnitCount:=xmlcfg.GetValue('ProjectOptions/Units/Count',0);
    for i := 0 to NewUnitCount - 1 do begin
      NewUnitInfo:=TUnitInfo.Create(nil);
      AddUnit(NewUnitInfo,false);
      NewUnitInfo.LoadFromXMLConfig(
         xmlcfg,'ProjectOptions/Units/Unit'+IntToStr(i)+'/');
    end;

    // Load the compiler options
    CompilerOptions.XMLConfigFile := xmlcfg;
    CompilerOptions.ProjectFile := ProjectFile;
    CompilerOptions.LoadCompilerOptions(true);

  finally
    xmlcfg.Free;
    xmlcfg:=nil;
  end;

  Result := mrOk;
end;

{------------------------------------------------------------------------------
  TProject AddUnit
 ------------------------------------------------------------------------------}
procedure TProject.AddUnit(AUnit: TUnitInfo; AddToProjectFile:boolean);
var ShortUnitName:string;
begin
  if (AUnit = nil) then exit;
  fUnitList.Add(AUnit);
  AUnit.OnFileBackup:=@OnUnitFileBackup;
  AUnit.OnLoadSaveFilename:=@OnLoadSaveFilename;
  AUnit.OnUnitNameChange:=@OnUnitNameChange;

  if AddToProjectFile and (MainUnit>=0) then begin
    // add unit to uses section
    ShortUnitName:=CodeToolBoss.GetSourceName(AUnit.Source);
    if (ShortUnitName<>'') and (not UnitIsUsed(ShortUnitName)) then
      CodeToolBoss.AddUnitToMainUsesSection(Units[MainUnit].Source,
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
    writeln('ERROR: TProject.RemoveUnit index out of bounds');
    Halt;
  end;
  if (Index=MainUnit) then begin
    writeln('ERROR: TProject.RemoveUnit index = MainUnit');
    Halt;
  end;
  OldUnitInfo:=Units[Index];
  Modified:=true;

  if MainUnit>=0 then begin
    // remove unit from uses section and from createforms in program file
    if OldUnitInfo.UnitName<>'' then
      CodeToolBoss.RemoveUnitFromAllUsesSections(Units[MainUnit].Source,
        OldUnitInfo.UnitName);
    if (OldUnitInfo.FormName<>'') then
      CodeToolBoss.RemoveCreateFormStatement(Units[MainUnit].Source,
        OldUnitInfo.FormName);
  end;

  // delete bookmarks on this unit
  if OldUnitInfo.EditorIndex>=0 then
    Bookmarks.DeleteAllWithEditorIndex(OldUnitInfo.EditorIndex);

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
  if XMLCfg<>nil then XMLCfg.Free;
  XMLCfg:=nil;

  for i:=0 to UnitCount-1 do Units[i].Free;
  fUnitList.Clear;

  fActiveEditorIndexAtStart := -1;
  fBookmarks.Clear;
  fCompilerOptions.Clear;
  fIconPath := '';
  fMainUnit := -1;
  fModified := false;
  fOutputDirectory := '.';
  fProjectFile := '';
  fTargetFileExt := {$IFDEF win32}'.exe'{$ELSE}''{$ENDIF};
  fTitle := '';
  fUnitOutputDirectory := '.';
end;

function TProject.GetUnits(Index:integer):TUnitInfo;
begin
  Result:=TUnitInfo(fUnitList[Index]);
end;

procedure TProject.SetUnits(Index:integer; AUnitInfo: TUnitInfo);
begin
  fUnitList[Index]:=AUnitInfo;
  Modified:=true;
end;

function TProject.UnitCount:integer;
begin
  Result:=fUnitList.Count;
end;

function TProject.NewUniqueUnitName(NewUnitType:TNewUnitType):string;

  function ExpandedUnitname(const AnUnitName:string):string;
  var Ext:string;
  begin
    Result:=uppercase(ExtractFileName(AnUnitName));
    Ext:=ExtractFileExt(Result);
    Result:=copy(Result,1,length(Result)-length(Ext));
  end;

  function UnitNameExists(const AnUnitName:string):boolean;
  var i:integer;
    ExpName:string;
  begin
    Result:=true;
    ExpName:=ExpandedUnitName(AnUnitName);
    if ExpandedUnitname(fProjectFile)=Expname then exit;
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
// NewUniqueFormName(NewUnitType:TNewUnitType)

  function FormNameExists(const AFormName: string): boolean;
  var i: integer;
  begin
    Result:=true;
    for i:=0 to UnitCount-1 do begin
      if Units[i].Form<>nil then begin
        if AnsiCompareText(Units[i].Form.Name,AFormName)=0 then exit;
        if AnsiCompareText(Units[i].Form.ClassName,'T'+AFormName)=0 then exit;
      end else if Units[i].FormName<>'' then begin
        if AnsiCompareText(Units[i].FormName,AFormName)=0 then exit;
      end;
    end;
    Result:=false;
  end;

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
  OnlyProjectUnits:boolean):integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if (OnlyProjectUnits and Units[Result].IsPartOfProject) 
    or (not OnlyProjectUnits) then begin
      if (AnsiCompareText(Units[Result].UnitName,AnUnitName)=0) then
        exit;
    end;
    dec(Result);
  end;
end;

function TProject.IndexOfUnitWithForm(AForm: TComponent; 
  OnlyProjectUnits:boolean):integer;
begin
  Result:=UnitCount-1;
  while (Result>=0) do begin
    if (OnlyProjectUnits and Units[Result].IsPartOfProject) 
    or (not OnlyProjectUnits) then begin
      if Units[Result].Form=AForm then
        exit;
    end;
    dec(Result);
  end;
end;

function TProject.UnitWithEditorIndex(Index:integer):TUnitInfo;
var i:integer;
begin
  i:=UnitCount-1;
  while (i>=0) and (Units[i].EditorIndex<>Index) do dec(i);
  if i>=0 then 
    Result:=Units[i]
  else
    Result:=nil;
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
        if CurPath[length(CurPath)]<>OSDirSeparator then
          CurPath:=CurPath+OSDirSeparator;
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
begin
  for i:=0 to UnitCount-1 do begin
    if Units[i].EditorIndex=EditorIndex then 
      Units[i].EditorIndex:=-1
    else if Units[i].EditorIndex>EditorIndex then 
      Units[i].EditorIndex:=Units[i].EditorIndex-1;
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
var i:integer;
begin
  for i:=0 to UnitCount-1 do begin
    if Units[i].EditorIndex>=EditorIndex then 
      Units[i].EditorIndex:=Units[i].EditorIndex+1;
  end;
  i:=Bookmarks.Count-1;
  while (i>=0) do begin
    if (Bookmarks[i].EditorIndex>=EditorIndex) then
      Bookmarks[i].EditorIndex:=Bookmarks[i].EditorIndex+1;
    dec(i);
  end;
  Modified:=true;
end;

procedure TProject.SetProjectFile(const NewProjectFilename: string);
begin
  fProjectFile:=NewProjectFilename;
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

function TProject.GetProjectInfoFile:string;
begin
  Result:=fProjectFile;
  if Result<>'' then Result:=ChangeFileExt(Result,'.lpi');
end;

procedure TProject.SetProjectInfoFile(const NewFilename:string);
begin
  if NewFilename='' then exit;
  ProjectFile:=ChangeFileExt(NewFilename,ProjectDefaultExt[ProjectType]);
end;

procedure TProject.OnLoadSaveFilename(var AFilename:string; Load:boolean);
var ProjectPath:string;
begin
  ProjectPath:=ExtractFilePath(ProjectFile);
  if ProjectPath='' then ProjectPath:=GetCurrentDir;
  DoDirSeparators(AFilename);
  if Load then begin
    // make filename absolute
    if not FilenameIsAbsolute(AFilename) then
      AFilename:=ProjectPath+AFilename;
  end else begin
    // try making filename relative to project file
    if FilenameIsAbsolute(AFilename) 
    and (copy(AFilename,1,length(ProjectPath))=ProjectPath) then
      AFilename:=copy(AFilename,length(ProjectPath)+1,
           length(AFilename)-length(ProjectPath));
  end;
end;

procedure TProject.OnUnitNameChange(AnUnitInfo: TUnitInfo; 
  const OldUnitName, NewUnitName: string;  var Allowed: boolean);
var i:integer;
begin
  if AnUnitInfo.IsPartOfProject then begin
    // check if no other project unit has this name
    for i:=0 to UnitCount-1 do
      if (Units[i].IsPartOfProject)
      and (Units[i]<>AnUnitInfo) and (Units[i].UnitName<>'') 
      and (lowercase(Units[i].UnitName)=lowercase(NewUnitName)) then begin
        Allowed:=false;
        exit;
      end;
    if (OldUnitName<>'') and (ProjectType in [ptProgram, ptApplication]) then
    begin
      // rename unit in program uses section
writeln('TProject.OnUnitNameChange A');
      CodeToolBoss.RenameUsedUnit(Units[MainUnit].Source
        ,OldUnitName,NewUnitName,'');
    end;
  end;
end;

function TProject.SomethingModified: boolean;
var i: integer;
begin
  Result:=Modified;
  for i:=0 to UnitCount-1 do Result:=Result or Units[i].Modified;
end;

end.



{
  $Log$
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
