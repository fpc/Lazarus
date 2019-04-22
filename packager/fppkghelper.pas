unit FppkgHelper;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  {$IF FPC_FULLVERSION>30100}
  pkgFppkg,
  fprepos,
  LazarusIDEStrConsts,
  {$ENDIF}
  LazLogger,
  LazFileCache,
  FileUtil,
  LazFileUtils;

type

  TFppkgPackageVariantArray = array of TStringArray;

  TFppkgPropConfigured = (fpcUnknown, fpcYes, fpcNo);

  { TFppkgHelper }

  TFppkgHelper = class
  private
    {$IF FPC_FULLVERSION>30100}
    FFPpkg: TpkgFPpkg;
    {$ENDIF}
    FIsProperlyConfigured: TFppkgPropConfigured;
    FConfStatusMessage: string;
    FOverrideConfigurationFilename: string;
    function HasFPCPackagesOnly(const PackageName: string): Boolean;
    procedure InitializeFppkg;
    procedure SetOverrideConfigurationFilename(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    class function Instance: TFppkgHelper;
    function HasPackage(const PackageName: string): Boolean;
    procedure ListPackages(AList: TStringList);
    function GetPackageUnitPath(const PackageName: string): string;
    function IsProperlyConfigured(out Message: string): Boolean;
    function GetCompilerFilename: string;
    function GetConfigurationFileName: string;
    function GetCompilerConfigurationFileName: string;
    // Temporary solution, because fpc 3.2.0 does not has support for package-variants
    // in TFPPackage
    function GetPackageVariantArray(const PackageName: string): TFppkgPackageVariantArray;
    procedure ReInitialize;
    property OverrideConfigurationFilename: string read FOverrideConfigurationFilename write SetOverrideConfigurationFilename;
  end;

implementation

var
  GFppkgHelper: TFppkgHelper = nil;

{ TFppkgHelper }

procedure TFppkgHelper.InitializeFppkg;
{$IF NOT (FPC_FULLVERSION>30100)}
begin
end;
{$ELSE}
var
  FPpkg: TpkgFPpkg;
begin
  FPpkg := TpkgFPpkg.Create(nil);
  try
    try
      FPpkg.InitializeGlobalOptions(FOverrideConfigurationFilename);
      FPpkg.InitializeCompilerOptions;

      FPpkg.CompilerOptions.CheckCompilerValues;
      FPpkg.FpmakeCompilerOptions.CheckCompilerValues;

      FPpkg.LoadLocalAvailableMirrors;

      FPpkg.ScanPackages;

      FFPpkg := FPpkg;
      FPpkg := nil;
    except
      on E: Exception do
        debugln(['InitializeFppkg failed: '+E.Message]);
    end;
  finally
    FPpkg.Free;
  end;
end;
{$ENDIF FPC_FULLVERSION>30100}

constructor TFppkgHelper.Create;
begin
  InitializeFppkg;
end;

destructor TFppkgHelper.Destroy;
begin
{$IF FPC_FULLVERSION>30100}
  FFPpkg.Free;
{$ENDIF FPC_FULLVERSION>30100}
  inherited Destroy;
end;

class function TFppkgHelper.Instance: TFppkgHelper;
begin
  if not Assigned(GFppkgHelper) then
    GFppkgHelper := TFppkgHelper.Create;
  Result := GFppkgHelper;
end;

function TFppkgHelper.HasPackage(const PackageName: string): Boolean;
{$IF FPC_FULLVERSION>30100}
var
  Msg: string;
{$ENDIF}
begin
{$IF NOT (FPC_FULLVERSION>30100)}
  Result := HasFPCPackagesOnly(PackageName);
{$ELSE }
  if IsProperlyConfigured(Msg) then
    begin
    Result :=
      Assigned(FFPpkg.FindPackage(PackageName,pkgpkInstalled)) or
      Assigned(FFPpkg.FindPackage(PackageName,pkgpkAvailable)) or
      Assigned(FFPpkg.FindPackage(PackageName,pkgpkBoth));

    if not Result then
      begin
      // rescan and try again
      FFppkg.LoadLocalAvailableMirrors;
      FFppkg.ScanPackages;

      Result :=
        Assigned(FFPpkg.FindPackage(PackageName,pkgpkInstalled)) or
        Assigned(FFPpkg.FindPackage(PackageName,pkgpkAvailable)) or
        Assigned(FFPpkg.FindPackage(PackageName,pkgpkBoth));
      end;
    end
  else
    Result := HasFPCPackagesOnly(PackageName);
{$ENDIF FPC_FULLVERSION>30100}
end;

procedure TFppkgHelper.ListPackages(AList: TStringList);
{$IF FPC_FULLVERSION>30100}
var
  I, J: Integer;
  Repository: TFPRepository;
{$ENDIF FPC_FULLVERSION>30100}
begin
  AList.Clear;
{$IF FPC_FULLVERSION>30100}
  if not Assigned(FFPpkg) then
    Exit;
  for I := 0 to FFPpkg.RepositoryList.Count -1 do
    begin
    Repository := FFPpkg.RepositoryList.Items[I] as TFPRepository;
    for J := 0 to Repository.PackageCount -1 do
      begin
      AList.AddObject(Repository.Packages[J].Name, Repository.Packages[J]);
      end;
    end;
{$ENDIF FPC_FULLVERSION>30100}
end;

function TFppkgHelper.GetPackageUnitPath(const PackageName: string): string;
{$IF FPC_FULLVERSION>30100}
var
  FppkgPackage: TFPPackage;
{$IF not (FPC_FULLVERSION>30300)}
  PackageVariantsArray: TFppkgPackageVariantArray;
{$ENDIF}
  i: Integer;
{$ENDIF FPC_FULLVERSION>30100}
begin
{$IF NOT (FPC_FULLVERSION>30100)}
  if PackageName='' then ;
  Result := '';
{$ELSE}
  if not Assigned(FFPpkg) then
    begin
    Result := '';
    Exit;
    end;
  FppkgPackage := FFPpkg.FindPackage(PackageName, pkgpkInstalled);
  if Assigned(FppkgPackage) then
    begin
    Result := FppkgPackage.PackagesStructure.GetUnitDirectory(FppkgPackage);

    {$IF FPC_FULLVERSION>30300}
    for i := 0 to FppkgPackage.PackageVariants.Count -1 do
      begin
      Result := ConcatPaths([Result, FppkgPackage.PackageVariants.Items[i].Options[0]]);
      end;
    {$ELSE}
    PackageVariantsArray := GetPackageVariantArray(PackageName);
    for i := 0 to High(PackageVariantsArray) do
      begin
      Result := ConcatPaths([Result, PackageVariantsArray[i][1]]);
      end;
    {$ENDIF FPC_FULLVERSION>30300}
    end
  else
    begin
    // The package has not been installed, so there is no unit-path yet.
    // ToDo: if this leads to problems, we could 'guess' the repository it will
    // be installed into, and use the corresponding packagestructure.
    Result := '';
    end;
{$ENDIF FPC_FULLVERSION>30100}
end;

function TFppkgHelper.GetPackageVariantArray(const PackageName: string): TFppkgPackageVariantArray;
{$IF FPC_FULLVERSION>30100}
var
  FppkgPackage: TFPPackage;
  UnitConfigFile: TStringList;
  PackageVariantStr, PackageVariant, UnitConfigFilename: String;
  PackageVariantOptions: TStringArray;
  i: Integer;
{$ENDIF FPC_FULLVERSION>30100}
begin
  {$IF FPC_FULLVERSION>30100}
  Result := [];
  {$ELSE}
  SetLength(Result, 0);
  if PackageName='' then ;
  {$ENDIF FPC_FULLVERSION>30100}

  {$IF FPC_FULLVERSION>30100}
  if not Assigned(FFPpkg) then
    begin
    Result := [];
    Exit;
    end;

  FppkgPackage := FFPpkg.FindPackage(PackageName, pkgpkInstalled);
  if Assigned(FppkgPackage) then
    begin
    UnitConfigFilename := FppkgPackage.PackagesStructure.GetConfigFileForPackage(FppkgPackage);
    if FileExists(UnitConfigFilename) then
      begin
      UnitConfigFile := TStringList.Create;
      try
        UnitConfigFile.LoadFromFile(UnitConfigFilename);
        i := 1;
        repeat
        PackageVariantStr := UnitConfigFile.Values['PackageVariant_'+IntToStr(i)];
        if PackageVariantStr<>'' then
          begin
          PackageVariant := Copy(PackageVariantStr, 1, pos(':', PackageVariantStr) -1);
          if RightStr(PackageVariant, 1) = '*' then
            PackageVariant := Copy(PackageVariant, 1, Length(PackageVariant) -1);
          PackageVariantOptions := Copy(PackageVariantStr, pos(':', PackageVariantStr) +1).Split(',');
          Insert(PackageVariant, PackageVariantOptions, -1);
          Insert(PackageVariantOptions, Result, 100);
          end;
        inc(i);
        until PackageVariantStr='';
      finally
        UnitConfigFile.Free;
      end;
      end
    end
  {$ENDIF FPC_FULLVERSION>30100}
end;

function TFppkgHelper.IsProperlyConfigured(out Message: string): Boolean;
{$IF FPC_FULLVERSION>30100}
var
  CompilerFilename: string;
{$ENDIF FPC_FULLVERSION>30100}
begin
  Message := '';
  {$IF FPC_FULLVERSION>30100}
  if Assigned(FFPpkg) and (FIsProperlyConfigured=fpcUnknown) then
    begin
    FIsProperlyConfigured := fpcYes;
    FConfStatusMessage := '';

    if not HasPackage('rtl') then
      begin
      FIsProperlyConfigured := fpcNo;
      FConfStatusMessage := lisFppkgRtlNotFound;
      end
    else
      begin
      CompilerFilename := FFPpkg.CompilerOptions.Compiler;
      if Pos(PathDelim, CompilerFilename) > 0 then
        begin
        if not FileExistsCached(CompilerFilename) then
          begin
          FIsProperlyConfigured := fpcNo;
          FConfStatusMessage := Format(lisFppkgCompilerNotExists, [CompilerFilename]);
          end
        else if not FileIsExecutableCached(CompilerFilename) then
          begin
          FIsProperlyConfigured := fpcNo;
          FConfStatusMessage := Format(lisFppkgCompilerNotExecutable, [CompilerFilename]);
          end;
        end
      else
        begin
        CompilerFilename := ExeSearch(CompilerFilename);
        if CompilerFilename = '' then
          begin
          FIsProperlyConfigured := fpcNo;
          FConfStatusMessage := Format(lisFppkgCompilerNotFound, [FFPpkg.CompilerOptions.Compiler]);
          end
        else if not FileIsExecutableCached(CompilerFilename) then
          begin
          FIsProperlyConfigured := fpcNo;
          FConfStatusMessage := Format(lisFppkgCompilerNotExecutable, [CompilerFilename]);
          end;
        end
      end;
    end;
  result := FIsProperlyConfigured=fpcYes;
  {$ELSE}
  result := True;
  {$ENDIF FPC_FULLVERSION>30100}
  Message := FConfStatusMessage;
end;

function TFppkgHelper.HasFPCPackagesOnly(const PackageName: string): Boolean;
const
  FpcPackages: array[0..120] of String = (
    // All packages of fpc-trunk from 20181231
    'rtl',
    'rtl-generics',
    'fcl-res',
    'fpindexer',
    'lua',
    'regexpr',
    'fcl-db',
    'cdrom',
    'paszlib',
    'libgc',
    'libtar',
    'fcl-report',
    'libcups',
    'sqlite',
    'libsee',
    'newt',
    'sdl',
    'gnome1',
    'ldap',
    'openssl',
    'libpng',
    'graph',
    'bzip2',
    'fcl-extra',
    'dbus',
    'symbolic',
    'rtl-objpas',
    'mad',
    'httpd24',
    'fcl-process',
    'fcl-sound',
    'gdbint',
    'rtl-unicode',
    'gtk1',
    'fcl-net',
    'utils-lexyacc',
    'mysql',
    'ptc',
    'libvlc',
    'fcl-image',
    'webidl',
    'fcl-base',
    'oggvorbis',
    'a52',
    'fcl-pdf',
    'opencl',
    'pthreads',
    'libgd',
    'tcl',
    'xforms',
    'iconvenc',
    'dts',
    'gmp',
    'httpd22',
    'jni',
    'syslog',
    'pasjpeg',
    'users',
    'postgres',
    'rtl-extra',
    'pxlib',
    'fv',
    'ncurses',
    'zlib',
    'fastcgi',
    'aspell',
    'rtl-console',
    'googleapi',
    'fpgtk',
    'bfd',
    'libusb',
    'unzip',
    'libenet',
    'x11',
    'libcurl',
    'utils-pas2js',
    'chm',
    'numlib',
    'fcl-registry',
    'libxml2',
    'fcl-web',
    'imlib',
    'fpmkunit',
    'libmicrohttpd',
    'pcap',
    'utmp',
    'odbc',
    'fcl-xml',
    'fcl-fpcunit',
    'ibase',
    'fcl-passrc',
    'cairo',
    'ide',
    'fppkg',
    'gtk2',
    'fcl-async',
    'pastojs',
    'hermes',
    'ggi',
    'openal',
    'opengl',
    'zorba',
    'hash',
    'fcl-json',
    'gdbm',
    'oracle',
    'fftw',
    'uuid',
    'libfontconfig',
    'modplug',
    'rsvg',
    'fcl-sdo',
    'fcl-js',
    'proj4',
    'dblib',
    'svgalib',
    'opengles',
    'libffi',
    'odata',
    'fcl-stl',
    'imagemagick'
  );
var
  i: Integer;
begin
  for i := 0 to High(FpcPackages) do
    begin
    if SameText(PackageName, FpcPackages[i]) then
      begin
      Result := True;
      Exit;
      end;
    end;
  Result := False;
end;

function TFppkgHelper.GetCompilerFilename: string;
begin
  Result := '';
  {$IF FPC_FULLVERSION>30100}
  if Assigned(FFPpkg) then
    begin
    Result := FFPpkg.CompilerOptions.Compiler;
    end;
  {$ENDIF}
end;

procedure TFppkgHelper.ReInitialize;
begin
  FIsProperlyConfigured := fpcUnknown;
  {$IF FPC_FULLVERSION>30100}
  FFPpkg.Free;
  {$ENDIF}
  InitializeFppkg;
end;

function TFppkgHelper.GetCompilerConfigurationFileName: string;
{$IF FPC_FULLVERSION>30100}
var
  FPpkg: TpkgFPpkg;
{$ENDIF}
begin
  Result := '';
  {$IF FPC_FULLVERSION>30100}
  if Assigned(FFPpkg) then
    Result:=ConcatPaths([FFPpkg.Options.GlobalSection.CompilerConfigDir, FFPpkg.Options.CommandLineSection.CompilerConfig])
  else
    begin
    FPpkg := TpkgFPpkg.Create(nil);
    try
      try
        FPpkg.InitializeGlobalOptions(FOverrideConfigurationFilename);
        Result:=ConcatPaths([FPpkg.Options.GlobalSection.CompilerConfigDir, FPpkg.Options.CommandLineSection.CompilerConfig])
      except
        on E: Exception do
          debugln(['Fppkg initialize global options failed: '+E.Message]);
      end;
    finally
      FPpkg.Free;
    end;
    end
  {$ENDIF}
end;

function TFppkgHelper.GetConfigurationFileName: string;
begin
  Result := '';
  {$IF FPC_FULLVERSION>30200}
  if Assigned(FFPpkg) then
    Result:=FFPpkg.ConfigurationFilename;
  {$ENDIF}
end;

procedure TFppkgHelper.SetOverrideConfigurationFilename(AValue: string);
begin
  if FOverrideConfigurationFilename = AValue then Exit;
  FOverrideConfigurationFilename := AValue;
  ReInitialize;
end;

finalization
  GFppkgHelper.Free;
end.
