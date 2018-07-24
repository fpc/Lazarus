unit ProjPackCommon;

{$mode objfpc}{$H+}
{.$INTERFACES CORBA}   // No reference counting for IProjPack. (does not work)

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazTracer,
  // Codetools
  DefineTemplates, CodeToolManager,
  // IDE
  CompilerOptions, FileReferenceList;

type

  TProjPackDefineTemplates = class;

  { IProjPack }

  IProjPack = interface
    function GetIDAsString: string;
    function GetIDAsWord: string;
    function GetBaseCompilerOptions: TBaseCompilerOptions;
    function GetDefineTemplates: TProjPackDefineTemplates;
    function GetSourceDirectories: TFileReferenceList;
    function GetModified: boolean;
    //function GetOutputDirectory: string;  // Proj/Pack have different params.
    function NeedsDefineTemplates: boolean;
    procedure AddPackageDependency(const PackageName: string);
    //function FindDependencyByName(const PackageName: string): TPkgDependency;
    function RemoveNonExistingFiles(RemoveFromUsesSection: boolean = true): boolean;
    procedure SetModified(const AValue: boolean);

    property IDAsString: string read GetIDAsString;
    property IDAsWord: string read GetIDAsWord;
    property BaseCompilerOptions: TBaseCompilerOptions read GetBaseCompilerOptions;
    property DefineTemplates: TProjPackDefineTemplates read GetDefineTemplates;
    property SourceDirectories: TFileReferenceList read GetSourceDirectories;
  end;

  { TProjPackDefineTemplates }

  TProjPackDefineTemplatesFlag = (
    ptfIsPackageTemplate,
    ptfFlagsChanged,
    ptfIDChanged,
    ptfSourceDirsChanged,
    ptfOutputDirChanged,
    ptfCustomDefinesChanged
    );
  TProjPackDefineTemplatesFlags = set of TProjPackDefineTemplatesFlag;

  TProjPackDefineTemplates = class
  private
    FOwner: IProjPack;
  protected
    FActive: boolean;
    FSrcDirectories: TDefineTemplate;
    FSrcDirIf: TDefineTemplate;
    FFlags: TProjPackDefineTemplatesFlags;
    FMain: TDefineTemplate;
    FOutputDir: TDefineTemplate;
    FOutPutSrcPath: TDefineTemplate;
    FUpdateLock: integer;
    fLastSourceDirectories: TStringList;
    fLastOutputDirSrcPathIDAsString: string;
    fLastSourceDirsIDAsString: string;
    fLastSourceDirStamp: integer;
    FLastCustomOptions: string;
    fLastUnitPath: string;
    procedure SetActive(const AValue: boolean);
    procedure UpdateMain; virtual; abstract;
    procedure UpdateSrcDirIfDef; virtual; abstract;
    procedure UpdateSourceDirectories; virtual; abstract;
    procedure UpdateOutputDirectory; virtual; abstract;
    procedure UpdateDefinesForCustomDefines; virtual; abstract;
    procedure ClearFlags; virtual; abstract;
  public
    constructor Create(AOwner: IProjPack);
    destructor Destroy; override;
    procedure Clear;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure AllChanged; virtual; abstract;
    procedure IDChanged;
    procedure SourceDirectoriesChanged;// a source directory was added/deleted
    procedure CustomDefinesChanged;    // the defines of the source dirs changed
    procedure OutputDirectoryChanged;// the path or the defines of the output dir changed
  public
    property Owner: IProjPack read FOwner;
    property Main: TDefineTemplate read FMain;
    property SrcDirectories: TDefineTemplate read FSrcDirectories;
    property OutputDir: TDefineTemplate read FOutputDir;
    property OutPutSrcPath: TDefineTemplate read FOutPutSrcPath;
    property CustomDefines: TDefineTemplate read FSrcDirIf;
    property Active: boolean read FActive write SetActive;
  end;


implementation

constructor TProjPackDefineTemplates.Create(AOwner: IProjPack);
begin
  inherited Create;
  FOwner:=AOwner;
end;

destructor TProjPackDefineTemplates.Destroy;
begin
  Clear;
  fLastSourceDirectories.Free;
  inherited Destroy;
end;

procedure TProjPackDefineTemplates.Clear;
begin
  if FMain<>nil then begin
    if (CodeToolBoss<>nil) then
      CodeToolBoss.DefineTree.RemoveDefineTemplate(FMain);
    FMain:=nil;
    FSrcDirIf:=nil;
    FSrcDirectories:=nil;
    FOutputDir:=nil;
    FOutPutSrcPath:=nil;
    fLastOutputDirSrcPathIDAsString:='';
    FLastCustomOptions:='';
    fLastUnitPath:='';
    fLastSourceDirsIDAsString:='';
    if fLastSourceDirectories<>nil then
      fLastSourceDirectories.Clear;
    ClearFlags;
  end;
end;

procedure TProjPackDefineTemplates.BeginUpdate;
begin
  inc(FUpdateLock);
end;

procedure TProjPackDefineTemplates.EndUpdate;
begin
  if FUpdateLock=0 then RaiseGDBException('TProjPackDefineTemplates.EndUpdate');
  dec(FUpdateLock);
  if FUpdateLock=0 then begin
    if ptfIsPackageTemplate in FFlags then begin
      if ptfIDChanged in FFlags then IDChanged;
    end;
    if ptfFlagsChanged in FFlags then CustomDefinesChanged;
    if ptfSourceDirsChanged in FFlags then SourceDirectoriesChanged;
    if ptfOutputDirChanged in FFlags then OutputDirectoryChanged;
    if ptfCustomDefinesChanged in FFlags then CustomDefinesChanged;
  end;
end;

procedure TProjPackDefineTemplates.SetActive(const AValue: boolean);
begin
  if FActive=AValue then exit;
  FActive:=AValue;
  if FActive then
    AllChanged
  else
    Clear;
end;

procedure TProjPackDefineTemplates.IDChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,ptfIDChanged);
    exit;
  end;
  Exclude(FFlags,ptfIDChanged);
  UpdateMain;
  UpdateOutputDirectory;
  UpdateSourceDirectories;
  UpdateDefinesForCustomDefines;
end;

procedure TProjPackDefineTemplates.SourceDirectoriesChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,ptfSourceDirsChanged);
    exit;
  end;
  Exclude(FFlags,ptfSourceDirsChanged);
  UpdateSourceDirectories;
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TProjPackDefineTemplates.OutputDirectoryChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,ptfOutputDirChanged);
    exit;
  end;
  Exclude(FFlags,ptfOutputDirChanged);
  UpdateOutputDirectory;
  CodeToolBoss.DefineTree.ClearCache;
end;

procedure TProjPackDefineTemplates.CustomDefinesChanged;
begin
  if FUpdateLock>0 then begin
    Include(FFlags,ptfCustomDefinesChanged);
    exit;
  end;
  Exclude(FFlags,ptfCustomDefinesChanged);
  UpdateDefinesForCustomDefines; // maybe custom defines changed
  if ptfIsPackageTemplate in FFlags then
    CodeToolBoss.DefineTree.ClearCache;
end;


end.

