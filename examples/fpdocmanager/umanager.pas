unit uManager;
(* Manager object for FPDoc GUI, by DoDi
Holds configuration and packages.

Packages (shall) contain extended descriptions for:
- default OSTarget (FPCDocs: Unix/Linux)
- inputs: by OSTarget
- directories: project(file), InputDir, DescrDir[by language?]
- FPCVersion, LazVersion: variations of inputs
- Skeleton and Output options, depending on DocType/Level and Format.
Units can be described in multiple XML docs, so that it's possible to
have specific parts depending on Laz/FPC version, OSTarget, Language, Widgetset.

Since the package (collection item) class is hardwired in the Packages collection,
a variable PackageClass has been introduced in FPDocProj.
*)

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles,
  umakeskel, fpdocproj, dw_HTML;

type

  { TDocPackage }

(* TDocPackage describes a package documentation project.
*)
  TDocPackage = class(TComponent) // (TFPDocPackage)
  private
    FDescrDir: string;
    FDocPkg: TFPDocPackage;
    FIncludePath: string;
    FInputDir: string;
    FLazPkg: string;
    FName: string;
    FProjectFile: string;
    FRequires: TStrings;
    FUnitPath: string;
    FUnits: TStrings;
    procedure SetDescrDir(AValue: string);
    procedure SetDocPkg(AValue: TFPDocPackage);
    procedure SetIncludePath(AValue: string);
    procedure SetInputDir(AValue: string);
    procedure SetLazPkg(AValue: string);
    procedure SetName(AValue: string);
    procedure SetProjectFile(AValue: string);
    procedure SetRequires(AValue: TStrings);
    procedure SetUnitPath(AValue: string);
    procedure SetUnits(AValue: TStrings);
  protected
    FPackage: TFPDocPackage;
    procedure ReadConfig(cf: TIniFile);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  CreateProject: TFPDocPackage;
    procedure UpdateConfig(cf: TIniFile; APkg: TFPDocPackage);
    property Name: string read FName write SetName;
    property DocPkg: TFPDocPackage read FDocPkg write SetDocPkg;
    property ProjectFile: string read FProjectFile write SetProjectFile; //xml?
  //from LazPkg
    property LazPkg: string read FLazPkg write SetLazPkg; //LPK name?
    property DescrDir: string read FDescrDir write SetDescrDir;
    property InputDir: string read FInputDir write SetInputDir;
    property Units: TStrings read FUnits write SetUnits;
    property Requires: TStrings read FRequires write SetRequires; //only string?
    property IncludePath: string read FIncludePath write SetIncludePath; //-Fi
    property UnitPath: string read FUnitPath write SetUnitPath; //-Fu
    //property DefOS: string; - variations!
  end;

  { TFPDocManager }

(* Holds configuration, projects and packages.
  Projects[] effectively represents packages.
*)
  TFPDocManager = class(TFPDocMaker)
  private
    FDirty: boolean;
    FFPDocDir: string;
    FLazarusDir: string;
    FModified: boolean;
    FOnChange: TNotifyEvent;
    FProjects: TStrings;
    FRootDir: string;
    UpdateCount: integer;
    procedure SetFPDocDir(AValue: string);
    procedure SetLazarusDir(AValue: string);
    procedure SetOnChange(AValue: TNotifyEvent);
    procedure SetRootDir(AValue: string);
  protected
    InputList, DescrList: TStringList;
    procedure Changed;
    function  BeginTest(APkg: TDocPackage): boolean;
    procedure EndTest;
  public
    Config: TIniFile;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    function  LoadConfig(const ADir: string; Force: boolean = False): boolean;
    function  SaveConfig: boolean;
    function  AddProject(const APkg, AFile: string; UpdateCfg: boolean): boolean; //from config
  {$IFDEF v0}
    function  CreateProject(const AFileName: string; APackage: TFPDocPackage): boolean;
  {$ELSE}
    //function CreateProject(const AFileName: string; APackage: TFPDocPackage): boolean;
  {$ENDIF}
    function AddPackage(AName: string): TDocPackage;
    function ImportLpk(const AName: string): TDocPackage;
  //actions
    //function MakeDoc(APkg: TDocPackage; AUnit: string): boolean; configure???
    function  TestRun(APkg: TDocPackage; AUnit: string): boolean;
    function  Update(APkg: TDocPackage; const AUnit: string): boolean;
    procedure CleanXML(const FileName: string);
  public //published?
    property FpcDocDir: string read FFPDocDir write SetFPDocDir;
    property LazarusDir: string read FLazarusDir write SetLazarusDir;
    property RootDir: string read FRootDir write SetRootDir;
    //property Packages: TStrings read FPackages;
    property Projects: TStrings read FProjects;
    property Dirty: boolean read FDirty; //inifile
    property Modified: boolean read FModified; //app
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

var
  Manager: TFPDocManager = nil; //init by application

implementation

uses
  uLpk;

const
  ConfigName = 'docmgr.ini';
  SecProjects = 'projects';

{ TDocPackage }

procedure TDocPackage.SetDescrDir(AValue: string);
begin
  if FDescrDir=AValue then Exit;
  FDescrDir:=AValue;
end;

(* Init from package
*)
procedure TDocPackage.SetDocPkg(AValue: TFPDocPackage);
var
  s: string;
begin
  if FDocPkg=AValue then Exit;
  FDocPkg:=AValue;
//init using Manager
  Manager.Package := DocPkg;
  FDescrDir := Manager.DescrDir;
  FInputDir := Manager.InputDir;
end;

procedure TDocPackage.SetIncludePath(AValue: string);
begin
  if FIncludePath=AValue then Exit;
  FIncludePath:=AValue;
end;

procedure TDocPackage.SetInputDir(AValue: string);
begin
  if FInputDir=AValue then Exit;
  FInputDir:=AValue;
end;

procedure TDocPackage.SetLazPkg(AValue: string);
begin
  if FLazPkg=AValue then Exit;
  FLazPkg:=AValue;
  //todo: import
end;

procedure TDocPackage.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure TDocPackage.SetProjectFile(AValue: string);
begin
  if FProjectFile=AValue then Exit;
  FProjectFile:=AValue;
  //todo: import?
  //FDocPkg; - must be created IN Manager
end;

procedure TDocPackage.SetRequires(AValue: TStrings);
begin
  if FRequires=AValue then Exit;
  FRequires:=AValue;
end;

procedure TDocPackage.SetUnitPath(AValue: string);
begin
  if FUnitPath=AValue then Exit;
  FUnitPath:=AValue;
end;

procedure TDocPackage.SetUnits(AValue: TStrings);
begin
  if FUnits=AValue then Exit;
  FUnits:=AValue;
end;

procedure TDocPackage.ReadConfig(cf: TIniFile);
begin
  //FProjectFile := cf.ReadString(Name, 'projectfile', '');
  FDescrDir := cf.ReadString(Name, 'descrdir', '');
  FInputDir := cf.ReadString(Name, 'inputdir', '');
//more?
end;

constructor TDocPackage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUnits := TStringList.Create;
  FRequires := TStringList.Create;
end;

destructor TDocPackage.Destroy;
begin
  FreeAndNil(FUnits);
  FreeAndNil(FRequires);
  inherited Destroy;
end;

(* Create new(?) package and project.
*)
function TDocPackage.CreateProject: TFPDocPackage;
var
  s, imp: string;
  pkg: TFPDocPackage;
  i: integer;
begin
//get the package
  Result := DocPkg;
  if Result <> nil then
    exit; //for now, implement update later
//create new pkg
  Result := Manager.Packages.FindPackage(Name);
  if Result = nil then begin
  //create pkg
    Manager.Package := nil; //!!!nothing selected!!!
    Manager.ParseFPDocOption('--package=' + Name); //selects or creates the pkg
    pkg := Manager.SelectedPackage;
  //add Inputs
    if pkg.Inputs.Count = 0 then begin //else pkg exists already?
      //if Units.Count = 0 then ???; --input-dir?
      //todo: common options? OS options?
      for i := 0 to Units.Count - 1 do begin
        pkg.Inputs.Add(Units.ValueFromIndex[i]);
      end;
    end;
  //add Descriptions
    if pkg.Descriptions.Count = 0 then begin
      if DescrDir <> '' then begin
      //first check for existing directory
        if not DirectoryExists(DescrDir) then begin
          MkDir(DescrDir); //exclude \?
        end else begin
          Manager.ParseFPDocOption('--descr-dir=' + DescrDir);
        end;
      end;
    end;
  //add Imports
    if (pkg.Imports.Count = 0) then begin
      for i := 0 to Requires.Count - 1 do begin
        s := Requires[i];
        imp := Manager.RootDir + s + '.xct,../' + s + '/';
        Manager.ParseFPDocOption('--import=' + imp);
      end;
    end;
  //add options
    pkg.Output := Manager.RootDir + Name;
    pkg.ContentFile := Manager.RootDir + Name + '.xct';
  //upate result when reached this
    Result := pkg;
  end;
  DocPkg := Result;
//create project file?
  if (ProjectFile = '') and (LazPkg <> '') then begin
    s := ChangeFileExt(LazPkg, '.xml');
    if Manager.CreateProject(s, DocPkg) then
      ProjectFile := s;
    //let manager register?
  end;
end;

(* Initialize the package, write global config (+local?)
*)
procedure TDocPackage.UpdateConfig(cf: TIniFile; APkg: TFPDocPackage);
begin
  //to come?
end;

{ TFPDocManager }

constructor TFPDocManager.Create(AOwner: TComponent);
begin
  //PackageClass:=TDocPackage; //extended package class
  FProjects := TStringList.Create;
  InputList := TStringList.Create;
  DescrList := TStringList.Create;
  inherited Create(AOwner);
end;

destructor TFPDocManager.Destroy;
begin
  FreeAndNil(Config); //save?
  FreeAndNil(FProjects);
  FreeAndNil(InputList);
  FreeAndNil(DescrList);
  inherited Destroy;
end;

procedure TFPDocManager.SetFPDocDir(AValue: string);
begin
  if FFPDocDir=AValue then Exit;
  FFPDocDir:=AValue;
  FDirty := True;
end;

procedure TFPDocManager.SetLazarusDir(AValue: string);
begin
  if FLazarusDir=AValue then Exit;
  FLazarusDir:=AValue;
  FDirty := True;
end;

procedure TFPDocManager.SetOnChange(AValue: TNotifyEvent);
begin
  if FOnChange=AValue then Exit;
  FOnChange:=AValue;
end;

(* Try load config from new dir - this may fail on the first run.
*)
procedure TFPDocManager.SetRootDir(AValue: string);
var
  s: string;
begin
  s := IncludeTrailingPathDelimiter(AValue);
  if FRootDir=s then Exit; //prevent recursion
  FRootDir:=s;
//load config?
  //LoadConfig(s, False);
  FDirty := True;
end;

procedure TFPDocManager.Changed;
begin
  if not Modified or (UpdateCount > 0) then
    exit; //should not be called directly
  FModified := False;
  if Assigned(OnChange) then
    FOnChange(self);
end;

procedure TFPDocManager.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure TFPDocManager.EndUpdate;
begin
  dec(UpdateCount);
  if UpdateCount <= 0 then begin
    UpdateCount := 0;
    if Modified then
      Changed;
  end;
end;

(* Try load config.
Init RootDir (only when config found?)
*)
function TFPDocManager.LoadConfig(const ADir: string; Force: boolean): boolean;
var
  s, pf, cf: string;
  i: integer;
  pkg: TDocPackage;
begin
  s := IncludeTrailingPathDelimiter(ADir);
  cf := s + ConfigName;
  Result := FileExists(cf);
  if not Result and not Force then
    exit;
  RootDir:=s; //recurse if RootDir changed
//sanity check: only one config file!
  if assigned(Config) then begin
    if (Config.FileName = cf) then
      exit(false) //nothing new?
    else
      Config.Free;
    //clear packages???
  end;
  Config := TIniFile.Create(cf);
  FDirty := True; //to be saved
  if not Result then
    exit; //nothing to read
//read directories
  FFPDocDir := Config.ReadString('dirs', 'fpc', '');
//read packages
  Config.ReadSectionValues(SecProjects, Projects); //<prj>=<file>
//read detailed package information - possibly multiple packages per project!
  BeginUpdate;  //turn of app notification!
  for i := 0 to Projects.Count - 1 do begin
  //read package config (=project file name?)
    s := Projects.Names[i];
    pf := Projects.ValueFromIndex[i];
    if pf <> '' then begin
      AddProject(s, pf, False); //add and load project file, don't update config!
      FModified := True; //force app notification
    end;
  end;
//more? (preferences?)
//done, nothing modified
  EndUpdate;
  FDirty := False;
end;

function TFPDocManager.SaveConfig: boolean;
begin
  if Dirty then begin
    Config.UpdateFile;
    FDirty := False;
  end;
  Result := not Dirty;
end;

(* Load FPDoc (XML) project file.
  Not Dirty when loaded from INI!
*)
function TFPDocManager.AddProject(const APkg, AFile: string; UpdateCfg: boolean): boolean;
var
  i, j, ipkg: integer;
  pn, s: string;
  pkg: TDocPackage;
  fpkg: TFPDocPackage;
begin
{$IFDEF v0}
//load the project
  LoadProjectFile(AFile); //override for init project/packages?
//update contained packages
  for i := 0 to Packages.Count - 1 do begin
    pkg := Packages[i] as TDocPackage;
    if pkg.ProjectFile = '' then
      pkg.ProjectFile := AFile;
  end;
  Config.WriteString(SecProjects, APkg, AFile); //unique entry!?
  FModified := true;
  FDirty:=true;
{$ELSE}
//load the project
  LoadProjectFile(AFile); //override for init project/packages?
//update contained packages
  for i := 0 to Packages.Count - 1 do begin
    Package := Packages[i];
    pkg := AddPackage(Package.Name);
    if pkg.ProjectFile = '' then begin
      pkg.ProjectFile := AFile;
      pkg.DocPkg := Package;
    //init Units
      for j := 0 to Package.Inputs.Count - 1 do begin
        s := UnitName(Package.Inputs, j);
        pkg.Units.Add(s + '=' + Package.Inputs[j]);
        //strip OS -Fi?
      end;
    //init imports
      for j := 0 to Package.Imports.Count - 1 do begin
        //s := Package.Imports[j];
        s := ImportName(j);
        //package reference?
        pkg.Requires.AddObject(s, AddPackage(s));
      end;
      if UpdateCfg then begin //update config?
        Config.WriteString(SecProjects, pkg.Name, AFile);
        FModified := true;
        FDirty:=true;
      end;
    end;
  end;
{$ENDIF}
//notify app?
  Changed;
end;

{$IFDEF v0}
(* Creates an FPDoc project file, adds the project to Projects[]
*)
function TFPDocManager.CreateProject(const AFileName: string;
  APackage: TFPDocPackage): boolean;
var
  APkg: TDocPackage absolute APackage;
begin
  Result:=inherited CreateProject(AFileName, APackage);
  if not Result then
    exit;
  if APkg.ProjectFile = AFileName then
    exit; //already done
  APkg.ProjectFile := AFileName;
  Config.WriteString(SecProjects, APackage.Name, AFileName); //unique entry!?
  FDirty:=true;
  FModified:=true;
  Changed;
end;
{$ELSE}
{$ENDIF}

(* Return the named package, create if not found.
  Rename: GetPackage?
*)
function TFPDocManager.AddPackage(AName: string): TDocPackage;
var
  i: integer;
begin
  AName := LowerCase(AName);
{$IFDEF v0}
  Result := Packages.FindPackage(AName) as TDocPackage;
  if assigned(Result) then
    exit;
//create new package
  Result := Packages.Add as TDocPackage;
  Result.Name := AName;
  fDirty := True;
  fModified := True;
  Changed;
{$ELSE}
  i := FProjects.IndexOfName(AName);
  if i < 0 then
    i := FProjects.Add(AName); //create new entry
  Result := FProjects.Objects[i] as TDocPackage;
  if Result = nil then begin
    Result := TDocPackage.Create(self);
    Result.Name := AName;
    FProjects.Objects[i] := Result; //add object
  end;
{$ENDIF}
end;

function TFPDocManager.ImportLpk(const AName: string): TDocPackage;
begin
  BeginUpdate;
//import the LPK file into
  Result := uLpk.ImportLpk(AName);
  if Result = nil then
    DoLog('Import failed on ' + AName)
  else begin
    if Result.CreateProject <> nil then begin  //create new(?) package project
      Config.WriteString(SecProjects, Result.Name, Result.ProjectFile);
      FDirty := True;
    end;
    FModified := True; //always?
    Changed;
  end;
  EndUpdate;
end;

function TFPDocManager.BeginTest(APkg: TDocPackage): boolean;
var
  pf, dir: string;
begin
  if not assigned(APkg) then
    exit(False);
  pf := APkg.ProjectFile;
  if pf = '' then
    exit(False);
  Package := APkg.DocPkg;
  dir := ExtractFileDir(pf);
  SetCurrentDir(dir);
  ParseFPDocOption('--project='+APkg.ProjectFile);
//okay, so far
  Result := True;
end;

procedure TFPDocManager.EndTest;
begin
  SetCurrentDir(ExtractFileDir(RootDir));
end;

function TFPDocManager.TestRun(APkg: TDocPackage; AUnit: string): boolean;
var
  pf, dir: string;
begin
(* more detailed error handling?
  Must CD to the project file directory!?
*)
  Result := BeginTest(APkg);
  if not Result then
    exit;
  ParseFPDocOption('--format=html');
  ParseFPDocOption('-n');
{$IFDEF v0}
  if AUnit <> '' then begin
    InputList.Clear;
    InputList.Add(Maker.UnitSpec(AUnit));
    DescrList.Clear;
    DescrList.Add(APkg.DescrDir + AUnit+'.xml');
    CreateUnitDocumentation(APkg, InputList, DescrList, True);
  end else begin
    CreateDocumentation(APkg,True);
  end;
{$ELSE}
  CreateUnitDocumentation(APkg.DocPkg, AUnit, True);
{$ENDIF}
  EndTest;
end;

(* MakeSkel functionality - create skeleton or update file
*)
function TFPDocManager.Update(APkg: TDocPackage; const AUnit: string): boolean;

  function DocumentUnit(const AUnit: string): boolean;
  var
    OutName, msg: string;
  begin
    InputList.Clear;
    InputList.Add(UnitSpec(AUnit));
    DescrList.Clear;
    OutName := DescrDir + AUnit + '.xml';
    Options.UpdateMode := FileExists(OutName);
    if Options.UpdateMode then begin
      DescrList.Add(APkg.DescrDir + AUnit + '.xml');
      OutName:=RootDir + 'upd.' + AUnit + '.xml';
      DoLog('Update ' + OutName);
    end else begin
      DoLog('Create ' + OutName);
    end;
    msg := DocumentPackage(APkg.Name, OutName, InputList, DescrList);
    Result := msg = '';
    if not Result then
      DoLog(msg) //+unit?
    else if Options.UpdateMode then begin
      CleanXML(OutName);
    end;
  end;

var
  i: integer;
  u: string;
begin
  Result := BeginTest(APkg);
  if not Result then
    exit;
  if AUnit <> '' then begin
    Result := DocumentUnit(AUnit);
  end else begin
    for i := 0 to Package.Inputs.Count - 1 do begin
      u := UnitName(Package.Inputs, i);
      DocumentUnit(u);
    end;
  end;
  EndTest;
end;

(* Kill file if no "<element" found
*)
procedure TFPDocManager.CleanXML(const FileName: string);
var
  f: TextFile;
  s: string;
begin
  AssignFile(f, FileName);
  Reset(f);
  try
    while not EOF(f) do begin
      ReadLn(f, s);
      if Pos('<element ', s) > 0 then
        exit; //file not empty
    end;
  finally
    CloseFile(f);
  end;
//nothing found, delete the file
  if DeleteFile(FileName) then
    DoLog('File ' + FileName + ' has no elements. Deleted.')
  else
    DoLog('File ' + FileName + ' has no elements. Delete failed.');
end;

end.

