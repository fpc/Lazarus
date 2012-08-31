program aarreupdatelist;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, LazFileUtils, LazLogger, Laz2_XMLCfg, CustApp, contnrs;

type

  TLazPackageType = (
    lptRunTime,         // RunTime packages can't register anything in the IDE.
                        // They can be used by designtime packages.
    lptDesignTime,      // DesignTime packages can register anything in the IDE
                        // and should not be compiled into projects.
                        // The IDE calls the 'register' procedures of each unit.
    lptRunAndDesignTime,// RunAndDesignTime packages can do anything.
    lptRunTimeOnly      // as lptRunTime, but they can not be used in the IDE
    );

  { TPkgVersion }

  TPkgVersion = class
  private
    FBuild: integer;
    FMajor: integer;
    FMinor: integer;
    FRelease: integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(XML: TXMLConfig; Path: string);
    procedure Save(XML: TXMLConfig; Path: string);
    function AsString: string;
    property Major: integer read FMajor write FMajor;
    property Minor: integer read FMinor write FMinor;
    property Release: integer read FRelease write FRelease;
    property Build: integer read FBuild write FBuild;
  end;

  { TPkgDependency }

  TPkgDependency = class
  private
    FDefaultFilename: string;
    FMaxVersion: TPkgVersion;
    FMaxVersionValid: boolean;
    FMinVersion: TPkgVersion;
    FMinVersionValid: boolean;
    FName: string;
    FPreferDefaultFilename: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Load(XML: TXMLConfig; Path: string);
    procedure Save(XML: TXMLConfig; Path: string);
    property Name: string read FName write FName;
    property MinVersion: TPkgVersion read FMinVersion;
    property MinVersionValid: boolean read FMinVersionValid write FMinVersionValid;
    property MaxVersion: TPkgVersion read FMaxVersion;
    property MaxVersionValid: boolean read FMaxVersionValid write FMaxVersionValid;
    property DefaultFilename: string read FDefaultFilename write FDefaultFilename;
    property PreferDefaultFilename: boolean read FPreferDefaultFilename write FPreferDefaultFilename;
  end;

  { TLPackage }

  TLPackage = class
  private
    FAuthor: String;
    FDescription: String;
    FLicense: String;
    FLPKFilename: string;
    FName: String;
    FPackageType: TLazPackageType;
    FRequiredDeps: TObjectList;
    FVersion: TPkgVersion;
  public
    constructor Create(aLPKFilename: string);
    destructor Destroy; override;
    procedure Load;
    property LPKFilename: string read FLPKFilename;
    property Version: TPkgVersion read FVersion;
    property Name: String read FName write FName;
    property PackageType: TLazPackageType read FPackageType write FPackageType;
    property Author: String read FAuthor write FAuthor;
    property Description: String read FDescription write FDescription;
    property License: String read FLicense write FLicense;
    property RequiredDeps: TObjectList read FRequiredDeps;
  end;

  { TAarreUpdateList }

  TAarreUpdateList = class(TCustomApplication)
  private
    FDirectory: string;
    FOutputFile: string;
    FQuiet: boolean;
    FVerbose: boolean;
    procedure SetDirectory(AValue: string);
    procedure SetOutputFile(AValue: string);
  protected
    procedure DoRun; override;
    procedure ScanDirectory(Dir: string);
    procedure AddLPK(LPKFilename: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;

    property Verbose: boolean read FVerbose write FVerbose;
    property Quiet: boolean read FQuiet write FQuiet;
    property Directory: string read FDirectory write SetDirectory;
    property OutputFile: string read FOutputFile write SetOutputFile;
  end;

const
  LazPackageTypeIdents: array[TLazPackageType] of string = (
    'RunTime', 'DesignTime', 'RunAndDesignTime', 'RunTimeOnly');

function LazPackageTypeIdentToType(const s: string): TLazPackageType;
begin
  for Result:=Low(TLazPackageType) to High(TLazPackageType) do
    if SysUtils.CompareText(s,LazPackageTypeIdents[Result])=0 then exit;
  Result:=lptRunTime;
end;

{ TPkgDependency }

constructor TPkgDependency.Create;
begin
  FMinVersion:=TPkgVersion.Create;
  FMaxVersion:=TPkgVersion.Create;
end;

destructor TPkgDependency.Destroy;
begin
  FreeAndNil(FMinVersion);
  FreeAndNil(FMaxVersion);
  inherited Destroy;
end;

procedure TPkgDependency.Load(XML: TXMLConfig; Path: string);
begin
  MaxVersion.Load(XML,Path+'MaxVersion');
  MaxVersionValid:=XML.GetValue(Path+'MaxVersion/Valid',false);
  MinVersion.Load(XML,Path+'MinVersion');
  MinVersionValid:=XML.GetValue(Path+'MinVersion/Valid',false);
  DefaultFilename:=XML.GetValue(Path+'DefaultFilename/Value','');
  PreferDefaultFilename:=XML.GetValue(Path+'DefaultFilename/Prefer',false);
end;

procedure TPkgDependency.Save(XML: TXMLConfig; Path: string);
begin
  MaxVersion.Save(XML,Path+'MaxVersion');
  XML.SetDeleteValue(Path+'MaxVersion/Valid',MaxVersionValid,false);
  MinVersion.Save(XML,Path+'MinVersion');
  XML.SetDeleteValue(Path+'MinVersion/Valid',MinVersionValid,false);
  XML.SetDeleteValue(Path+'DefaultFilename/Value',FDefaultFilename,'');
  XML.SetDeleteValue(Path+'DefaultFilename/Prefer',FPreferDefaultFilename,false);
end;

{ TPkgVersion }

constructor TPkgVersion.Create;
begin

end;

destructor TPkgVersion.Destroy;
begin
  inherited Destroy;
end;

procedure TPkgVersion.Load(XML: TXMLConfig; Path: string);
begin
  Major:=XML.GetValue(Path+'Major',0);
  Minor:=XML.GetValue(Path+'Minor',0);
  Release:=XML.GetValue(Path+'Release',0);
  Build:=XML.GetValue(Path+'Build',0);
end;

procedure TPkgVersion.Save(XML: TXMLConfig; Path: string);
begin
  XML.SetDeleteValue(Path+'Major',Major,0);
  XML.SetDeleteValue(Path+'Minor',Minor,0);
  XML.SetDeleteValue(Path+'Release',Release,0);
  XML.SetDeleteValue(Path+'Build',Build,0);
end;

function TPkgVersion.AsString: string;
begin
  Result:=IntToStr(Major)+'.'+IntToStr(Minor)+'.'+IntToStr(Release)+'.'+IntToStr(Build);
end;

{ TLPK }

constructor TLPackage.Create(aLPKFilename: string);
begin
  fLPKFilename:=aLPKFilename;
  FVersion:=TPkgVersion.Create;
  FRequiredDeps:=TObjectList.Create;
end;

destructor TLPackage.Destroy;
begin
  FreeAndNil(FVersion);
  FreeAndNil(FRequiredDeps);
  inherited Destroy;
end;

procedure TLPackage.Load;
var
  xml: TXMLConfig;
  Path: String;
  FileVersion: Integer;
  i: Integer;
  NewCount: Integer;
  PkgDependency: TPkgDependency;
begin
  xml:=TXMLConfig.Create(LPKFilename);
  try
    Path:='Package/';
    FileVersion:=xml.GetValue(Path+'Version',0);
    if FileVersion=0 then
      raise Exception.Create('no file version');
    Name:=xml.GetValue(Path+'Name/Value','');
    PackageType:=LazPackageTypeIdentToType(xml.GetValue(Path+'Type/Value',
                                            LazPackageTypeIdents[lptRunTime]));
    Author:=xml.GetValue(Path+'Author/Value','');
    Description:=xml.GetValue(Path+'Description/Value','');
    License:=xml.GetValue(Path+'License/Value','');
    Version.Load(xml,Path+'Version/');

    NewCount:=xml.GetValue(Path+'RequiredPkgs/Count',0);
    RequiredDeps.Clear;
    for i:=0 to NewCount-1 do begin
      PkgDependency:=TPkgDependency.Create;
      PkgDependency.Load(xml,Path+'RequiredPkgs/Item'+IntToStr(i+1)+'/');
      RequiredDeps.Add(PkgDependency);
    end;

    debugln(['TLPackage.Load Name="',Name,'"',
      ' Type=',LazPackageTypeIdents[PackageType],
      ' Author="',Author,'"',
      ' Description="',Description,'"',
      ' License="',License,'"',
      ' Version="',Version.AsString,'"'
      ]);
  finally
    xml.Free;
  end;
end;

{ TAarreUpdateList }

procedure TAarreUpdateList.SetDirectory(AValue: string);
begin
  if FDirectory=AValue then Exit;
  FDirectory:=AValue;
end;

procedure TAarreUpdateList.SetOutputFile(AValue: string);
begin
  if FOutputFile=AValue then Exit;
  FOutputFile:=AValue;
end;

procedure TAarreUpdateList.DoRun;

  procedure E(ErrMsg: string);
  begin
    writeln('ERROR: ',ErrMsg);
    Halt;
  end;

var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('hvqd:o:','help verbose quiet directory: output:');
  if ErrorMsg<>'' then
    E(ErrorMsg);

  // parse parameters
  if HasOption('h','help') then begin
    WriteHelp;
    Exit;
  end;

  Quiet:=HasOption('q','quiet');
  Verbose:=HasOption('v','verbose');
  if HasOption('o','output') then
    OutputFile:=GetOptionValue('o','output');
  if (OutputFile='') then
    E('output file is empty');
  OutputFile:=TrimAndExpandFilename(OutputFile);
  if not DirectoryExistsUTF8(ExtractFilePath(OutputFile)) then
    E('missing output directory: '+ExtractFilePath(OutputFile));
  if DirectoryExistsUTF8(OutputFile) then
    E('output file is directory: '+OutputFile);
  if HasOption('d','directory') then
    Directory:=GetOptionValue('d','directory');
  if Directory='' then
    E('directory is empty');
  Directory:=TrimAndExpandFilename(Directory);
  if not DirectoryExistsUTF8(Directory) then
    E('directory not found: '+Directory);

  if not Quiet then begin
    debugln(['scanning directory "',Directory,'" ...']);
  end;

  ScanDirectory(Directory);

  // stop program loop
  Terminate;
end;

procedure TAarreUpdateList.ScanDirectory(Dir: string);
var
  fileinfo: TSearchRec;
  Filename: TFilename;
begin
  Dir:=AppendPathDelim(Dir);
  if FindFirstUTF8(Dir+AllFilesMask,faAnyFile,fileinfo)=0 then begin
    repeat
      Filename:=fileinfo.Name;
      if (Filename='') or (Filename='.') or (Filename='..')
      or (Filename='.svn') or (Filename='CVS') or (Filename='.git') then
        continue;
      if faDirectory and fileinfo.Attr>0 then begin
        ScanDirectory(Dir+Filename);
      end else begin
        if ExtractFileExt(Filename)='.lpk' then
          AddLPK(Dir+Filename);
      end;
    until FindNextUTF8(fileinfo)<>0;
  end;
  FindCloseUTF8(fileinfo);
end;

procedure TAarreUpdateList.AddLPK(LPKFilename: string);
var
  Pkg: TLPackage;
begin
  DebugLn(['TAarreUpdateList.AddLPK ',LPKFilename]);

  Pkg:=TLPackage.Create(LPKFilename);
  try
    Pkg.Load;
  except
    on E: Exception do begin
      debugln(['ERROR: while reading "'+LPKFilename+'": '+E.Message]);
    end;
  end;
end;

constructor TAarreUpdateList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  Verbose:=false;
  Quiet:=false;
  Directory:='.';
  OutputFile:='packages.gz';
end;

destructor TAarreUpdateList.Destroy;
begin
  inherited Destroy;
end;

procedure TAarreUpdateList.WriteHelp;
begin
  writeln('Usage: ',ExeName,' -h');
  writeln;
  writeln('-h : write this help and exit');
  writeln('-v : be more verbose');
  writeln('-q : be quiet');
  writeln('-d <repository>  : The directory to scan. Default: ',Directory);
  writeln('-o <pkglistfile.gz> : The file to create. Default: ',OutputFile);
  Terminate;
end;

var
  Application: TAarreUpdateList;
begin
  Application:=TAarreUpdateList.Create(nil);
  Application.Title:='Aaree update list';
  Application.Run;
  Application.Free;
end.

