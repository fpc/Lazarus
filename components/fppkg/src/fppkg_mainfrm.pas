{ Main form for the lazarus package manager

  Copyright (C) 2011 Darius Blaszyk

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit fppkg_mainfrm;

{$mode objfpc}{$H+}

{$IFDEF VER2_4}{$ERROR This package requires at least fpc 2.5.1}{$ENDIF}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  Menus, CheckLst, Dialogs, fppkg_const,
  fppkg_optionsfrm, fppkg_details, fppkg_lpk,
  //IDE interface
  {$IFDEF LazarusIDEPackage}
    IDEIntf, PackageIntf, IDECommands,
  {$ENDIF}
  // Repository handler objects
  fprepos,
  pkgmessages, pkgglobals, pkgoptions, pkgrepos, laz_pkgrepos,
  // Package Handler components
  pkghandler, laz_pkghandler, laz_pkgcommands, pkgcommands,
  //downloader
  pkgfphttp, pkglnet;

type
  TFppkgConfigOptions = record
    ConfigFile: string;
  end;

  { TFppkgForm }

  TFppkgForm = class(TForm)
    CategoriesLabel: TLabel;
    CategoryCheckListBox: TCheckListBox;
    FilterPanel: TPanel;
    MenuItem5: TMenuItem;
    miCleanMessages: TMenuItem;
    OutputMemo: TMemo;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    miShowDetails: TMenuItem;
    miSeparator: TMenuItem;
    PackageListView: TListView;
    PackageSupportImages: TImageList;
    MainMenu: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    miExit: TMenuItem;
    miSelect: TMenuItem;
    miUnselect: TMenuItem;
    miFile: TMenuItem;
    PackagePopupMenu: TPopupMenu;
    Panel: TPanel;
    SearchEdit: TEdit;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    SupportCheckGroup: TCheckGroup;
    ToolbarImages: TImageList;
    SearchLabel: TLabel;
    SearchPanel: TPanel;
    SearchButton: TSpeedButton;
    ToolBar: TToolBar;
    BuildButton: TToolButton;
    CompileButton: TToolButton;
    FixBrokenButton: TToolButton;
    UpdateButton: TToolButton;
    InstallButton: TToolButton;
    CleanButton: TToolButton;
    ArchiveButton: TToolButton;
    DownloadButton: TToolButton;
    VertSplitter: TSplitter;
    procedure ArchiveButtonClick(Sender: TObject);
    procedure BuildButtonClick(Sender: TObject);
    procedure CategoryCheckListBoxClickCheck(Sender: TObject);
    procedure CleanButtonClick(Sender: TObject);
    procedure CompileButtonClick(Sender: TObject);
    procedure DownloadButtonClick(Sender: TObject);
    procedure FixBrokenButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InstallButtonClick(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure miClearMemoClick(Sender: TObject);
    procedure miCleanMessagesClick(Sender: TObject);
    procedure PackageListViewDblClick(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miSelectClick(Sender: TObject);
    procedure miUnselectClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchEditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SupportCheckGroupItemClick(Sender: TObject; Index: integer);
    procedure UpdateButtonClick(Sender: TObject);
  private
    { private declarations }
    SearchPhrases: TStrings;

    function FindSearchPhrase(pkg: TLazPackageData): boolean;
    function FindCategory(pkg: TLazPackageData): boolean;
    function FindSupport(pkg: TLazPackageData): boolean;
    procedure GetSelectedPackages(var s: TStrings);

    procedure MaybeCreateLocalDirs;
    procedure DoRun(cfg: TFppkgConfigOptions; ParaAction: string; ParaPackages: TStrings);
    procedure LoadGlobalDefaults(cfg: TFppkgConfigOptions);
    procedure LoadCompilerDefaults;

    procedure UpdatePackageListView;
    procedure ListPackages;
  public
    { public declarations }
  end;

var
  FppkgForm: TFppkgForm;
  FppkgCfg: TFppkgConfigOptions;

implementation

{$R *.lfm}

uses
  Masks, fppkg_aboutfrm;

procedure LazLog(Level: TLogLevel; const Msg: string);
var
  Prefix : string;
begin
  if not(Level in LogLevels) then
    exit;
  Prefix:='';
  case Level of
    vlWarning :
      Prefix:=SWarning;
    vlError :
      Prefix:=SError;
{    vlInfo :
      Prefix:='I: ';
    vlCommands :
      Prefix:='C: ';
    vlDebug :
      Prefix:='D: '; }
  end;

  if Assigned(FppkgForm) then
    FppkgForm.OutputMemo.Lines.Add(DateTimeToStr(Now) + ' ' + Prefix + ' ' + Msg);
end;

procedure LazError(const Msg: String);
begin
  ShowMessage(Msg);
end;

{ TFppkgForm }

procedure TFppkgForm.FixBrokenButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;
  DoRun(FppkgCfg, 'laz_fixbroken', s);
  ListPackages;
  UpdatePackageListView;
  s.Free;
end;

procedure TFppkgForm.CleanButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;

  GetSelectedPackages(s);

  if s.Count = 0 then
    Error(SErrNoPackageSpecified)
  else
    DoRun(FppkgCfg, 'laz_clean', s);

  s.Free;
end;

procedure TFppkgForm.CompileButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;

  GetSelectedPackages(s);

  if s.Count = 0 then
    Error(SErrNoPackageSpecified)
  else
    DoRun(FppkgCfg, 'laz_compile', s);

  s.Free;
end;

procedure TFppkgForm.DownloadButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;

  GetSelectedPackages(s);

  if s.Count = 0 then
    Error(SErrNoPackageSpecified)
  else
    DoRun(FppkgCfg, 'laz_download', s);

  s.Free;
end;

procedure TFppkgForm.ArchiveButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;

  GetSelectedPackages(s);

  if s.Count = 0 then
    Error(SErrNoPackageSpecified)
  else
    DoRun(FppkgCfg, 'laz_archive', s);

  s.Free;
end;

procedure TFppkgForm.BuildButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;

  GetSelectedPackages(s);

  if s.Count = 0 then
    Error(SErrNoPackageSpecified)
  else
    DoRun(FppkgCfg, 'laz_build', s);

  s.Free;
end;

procedure TFppkgForm.CategoryCheckListBoxClickCheck(Sender: TObject);
begin
  UpdatePackageListView;
end;

procedure TFppkgForm.FormCreate(Sender: TObject);
begin
  //setup log callback function
  LogHandler := @LazLog;

  //setup error callback function
  ErrorHandler := @LazError;

  Caption := rsFreePascalPackageManagerForLazarus;

  SupportCheckGroup.Checked[0] := True;
  SupportCheckGroup.Checked[1] := True;
  SupportCheckGroup.Checked[2] := True;

  SearchPhrases := TStringList.Create;
  SearchPhrases.Delimiter := ' ';

  ListPackages;

  UpdatePackageListView;
end;

procedure TFppkgForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(AvailableRepository);
  SearchPhrases.Free;
end;

procedure TFppkgForm.InstallButtonClick(Sender: TObject);
var
  s: TStrings;
  i: integer;
  LPKFile: string;
  {$IFDEF LazarusIDEPackage}
  RebuildLazarus: boolean;
  PkgFlags: TPkgInstallInIDEFlags;
  {$ENDIF}
begin
  s := TStringList.Create;

  GetSelectedPackages(s);

  if s.Count = 0 then
    Error(SErrNoPackageSpecified)
  else
  begin
    DoRun(FppkgCfg, 'laz_install', s);
    ListPackages;
    UpdatePackageListView;

    ////handle the installation of packages and the rebuilding of the IDE
    //{$IFDEF LazarusIDEPackage}
    //  RebuildLazarus := False;
    //
    //  //now install the packages in the IDE
    //  for i := 0 to s.Count - 1 do
    //  begin
    //    //retrieve each package file location
    //    LPKFile := LPKFindPackage(s[i]);
    //
    //    if LPKFile <> '' then
    //    begin
    //      PkgFlags := [piiifQuiet];
    //
    //      //make sure to determine if the IDE needs to be rebuilt
    //      if LPKStatus(LPKFile) in [lpDesigntime, lpBoth] then
    //      begin
    //        RebuildLazarus := True;
    //        PkgFlags := PkgFlags + [piiifRebuildIDE];
    //      end;
    //
    //      //add LPK file to IDE
    //      {$note what's the modal result doing here?}
    //      //PackageEditingInterface.InstallPackages(, PkgFlags);
    //    end;
    //  end;
    //
    //  //one or more designtime or design/runtime packages were installed
    //  if RebuildLazarus then
    //    ExecuteIDECommand(Self, ecBuildLazarus);
    //{$ENDIF}
  end;

  s.Free;
end;

procedure TFppkgForm.MenuItem4Click(Sender: TObject);
begin
  if not Assigned(FppkgAboutForm) then
    FppkgAboutForm := TFppkgAboutForm.Create(Self);

  FppkgAboutForm.ShowModal;
  FreeAndNil(FppkgAboutForm);
end;

procedure TFppkgForm.miClearMemoClick(Sender: TObject);
begin
  OutputMemo.Clear;
end;

procedure TFppkgForm.miCleanMessagesClick(Sender: TObject);
begin
  OutputMemo.Clear;
end;

procedure TFppkgForm.PackageListViewDblClick(Sender: TObject);
begin
  //only for selected items show details
  if not Assigned(PackageListView.Selected) then
    exit;

  if not Assigned(PkgDetailsForm) then
    PkgDetailsForm := TPkgDetailsForm.Create(Self);

  PkgDetailsForm.PackageName := PackageListView.Selected.Caption;
  PkgDetailsForm.ShowModal;

  FreeAndNil(PkgDetailsForm);
end;

procedure TFppkgForm.MenuItem2Click(Sender: TObject);
begin
  if not Assigned(OptionsForm) then
    OptionsForm := TOptionsForm.Create(Self);

  OptionsForm.ShowModal;

  //to be sure setup the view again
  UpdatePackageListView;
end;

procedure TFppkgForm.miExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFppkgForm.miSelectClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to PackageListView.Items.Count - 1 do
    if PackageListView.Items[i].Selected then
      PackageListView.Items[i].Checked := True;
end;

procedure TFppkgForm.miUnselectClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to PackageListView.Items.Count - 1 do
    if PackageListView.Items[i].Selected then
      PackageListView.Items[i].Checked := False;
end;

procedure TFppkgForm.SearchButtonClick(Sender: TObject);
begin
  SearchPhrases.DelimitedText := SearchEdit.Text;
  UpdatePackageListView;
end;

procedure TFppkgForm.SearchEditKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Key = 13 then
  begin
    SearchPhrases.DelimitedText := SearchEdit.Text;
    UpdatePackageListView;
  end;
end;

procedure TFppkgForm.SupportCheckGroupItemClick(Sender: TObject; Index: integer);
begin
  UpdatePackageListView;
end;

procedure TFppkgForm.UpdateButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;
  DoRun(FppkgCfg, 'laz_update', s);
  UpdatePackageListView;
  s.Free;
end;

procedure TFppkgForm.MaybeCreateLocalDirs;
begin
  ForceDirectories(GlobalOptions.BuildDir);
  ForceDirectories(GlobalOptions.ArchivesDir);
  ForceDirectories(GlobalOptions.CompilerConfigDir);
end;

function TFppkgForm.FindSearchPhrase(pkg: TLazPackageData): boolean;
var
  i: integer;
  searchmask: string;
begin
  Result := False;

  if SearchPhrases.Count = 0 then
    Result := True;

  for i := 0 to SearchPhrases.Count - 1 do
  begin
    searchmask := LowerCase('*' + SearchPhrases[i] + '*');

    if MatchesMask(LowerCase(pkg.Description), searchmask) or
      MatchesMask(LowerCase(pkg.Category), searchmask) or
      MatchesMask(LowerCase(pkg.Keywords), searchmask) or
      MatchesMask(LowerCase(pkg.Name), searchmask) then
    begin
      Result := True;
      exit;
    end;
  end;
end;

function TFppkgForm.FindCategory(pkg: TLazPackageData): boolean;
var
  i: integer;
  searchmask: string;
begin
  Result := False;

  for i := 0 to CategoryCheckListBox.Count - 1 do
  begin
    if CategoryCheckListBox.Checked[i] then
    begin
      //determine the searchmask
      if CategoryCheckListBox.Items[i] = 'All' then
        searchmask := '*'
      else
      if CategoryCheckListBox.Items[i] = 'Unknown' then
        searchmask := ''
      else
        searchmask := CategoryCheckListBox.Items[i];

      if MatchesMask(pkg.Category, searchmask) then
      begin
        Result := True;
        exit;
      end;
    end;
  end;
end;

function TFppkgForm.FindSupport(pkg: TLazPackageData): boolean;
begin
  Result := False;

  //FPC
  Result := Result or (SupportCheckGroup.Checked[0] and (pkg.Category = 'FPC'));

  //Lazarus
  Result := Result or (SupportCheckGroup.Checked[1] and (pkg.Category = 'Lazarus'));

  //Rest
  Result := Result or (SupportCheckGroup.Checked[2] and
    ((pkg.Category <> 'FPC') and (pkg.Category <> 'Lazarus')));
end;

procedure TFppkgForm.GetSelectedPackages(var s: TStrings);
var
  i: integer;
begin
  for i := 0 to PackageListView.Items.Count - 1 do
    if PackageListView.Items[i].Checked then
      s.Add(PackageListView.Items[i].Caption);
end;

procedure TFppkgForm.LoadGlobalDefaults(cfg: TFppkgConfigOptions);
var
  cfgfile: string;
  GeneratedConfig, UseGlobalConfig: boolean;
begin
  GeneratedConfig := False;
  UseGlobalConfig := False;
  // First try config file from command line
  if cfg.ConfigFile <> '' then
  begin
    cfgfile := cfg.ConfigFile;
    if not FileExists(cfgfile) then
      Error(SErrNoSuchFile, [cfgfile]);
  end
  else
  begin
    // Now try if a local config-file exists
    cfgfile := GetAppConfigFile(False, False);
    if not FileExists(cfgfile) then
    begin
      // If not, try to find a global configuration file
      cfgfile := GetAppConfigFile(True, False);
      if FileExists(cfgfile) then
        UseGlobalConfig := True
      else
      begin
        // Create a new configuration file
        if not IsSuperUser then // Make a local, not global, configuration file
          cfgfile := GetAppConfigFile(False, False);
        ForceDirectories(ExtractFilePath(cfgfile));
        //make sure the downloader is FPC native
        //GlobalOptions.Downloader := 'FPC';
        GlobalOptions.SaveGlobalToFile(cfgfile);
        GeneratedConfig := True;
      end;
    end;
  end;
  // Load file or create new default configuration
  if not GeneratedConfig then
  begin
    GlobalOptions.LoadGlobalFromFile(cfgfile);
    //make sure the downloader is FPC native
    //GlobalOptions.Downloader := 'FPC';
    if GlobalOptions.SaveInifileChanges and (not UseGlobalConfig or IsSuperUser) then
      GlobalOptions.SaveGlobalToFile(cfgfile);
  end;
  GlobalOptions.CompilerConfig := GlobalOptions.DefaultCompilerConfig;
  // Tracing of what we've done above, need to be done after the verbosity is set
  if GeneratedConfig then
    pkgglobals.Log(vlDebug, SLogGeneratingGlobalConfig, [cfgfile])
  else
    pkgglobals.Log(vlDebug, SLogLoadingGlobalConfig, [cfgfile]);
  // Log configuration
  GlobalOptions.LogValues(vlDebug);
end;

procedure TFppkgForm.LoadCompilerDefaults;
var
  S: string;
begin
  // Load default compiler config
  S := GlobalOptions.CompilerConfigDir + GlobalOptions.CompilerConfig;
  CompilerOptions.UpdateLocalRepositoryOption;
  if FileExists(S) then
  begin
    pkgglobals.Log(vlDebug, SLogLoadingCompilerConfig, [S]);
    CompilerOptions.LoadCompilerFromFile(S);
  end
  else
  begin
    // Generate a default configuration if it doesn't exists
    if GlobalOptions.CompilerConfig = 'default' then
    begin
      pkgglobals.Log(vlDebug, SLogGeneratingCompilerConfig, [S]);
      CompilerOptions.InitCompilerDefaults;
      CompilerOptions.SaveCompilerToFile(S);
      if CompilerOptions.SaveInifileChanges then
        CompilerOptions.SaveCompilerToFile(S);
    end
    else
      Error(SErrMissingCompilerConfig, [S]);
  end;
  // Log compiler configuration
  CompilerOptions.LogValues(vlDebug, '');
  // Load FPMake compiler config, this is normally the same config as above
  S := GlobalOptions.CompilerConfigDir + GlobalOptions.FPMakeCompilerConfig;
  FPMakeCompilerOptions.UpdateLocalRepositoryOption;
  if FileExists(S) then
  begin
    pkgglobals.Log(vlDebug, SLogLoadingFPMakeCompilerConfig, [S]);
    FPMakeCompilerOptions.LoadCompilerFromFile(S);
    if FPMakeCompilerOptions.SaveInifileChanges then
      FPMakeCompilerOptions.SaveCompilerToFile(S);
  end
  else
    Error(SErrMissingCompilerConfig, [S]);
  // Log compiler configuration
  FPMakeCompilerOptions.LogValues(vlDebug, 'fpmake-building ');
end;

procedure TFppkgForm.DoRun(cfg: TFppkgConfigOptions; ParaAction: string;
  ParaPackages: TStrings);
var
  ActionPackage: TFPPackage;
  OldCurrDir: string;
  i: integer;
  SL: TStringList;
begin
  OldCurrDir := GetCurrentDir;
  try
    LoadGlobalDefaults(cfg);
    //ProcessCommandLine(true);

    // Scan is special, it doesn't need a valid local setup
    if (ParaAction = 'laz_scan') then
    begin
      RebuildRemoteRepository;
      ListRemoteRepository;
      SaveRemoteRepository;
      exit;
    end;

    MaybeCreateLocalDirs;
    if not GlobalOptions.SkipConfigurationFiles then
      LoadCompilerDefaults
    else
    begin
      FPMakeCompilerOptions.InitCompilerDefaults;
      CompilerOptions.InitCompilerDefaults;
    end;

    // The command-line is parsed for the second time, to make it possible
    // to override the values in the compiler-configuration file. (like prefix)
    //ProcessCommandLine(false);

    // If CompilerVersion, CompilerOS or CompilerCPU is still empty, use the
    // compiler-executable to get them
    FPMakeCompilerOptions.CheckCompilerValues;
    CompilerOptions.CheckCompilerValues;

    LoadLocalAvailableMirrors;

    // Load local repository, update first if this is a new installation
    // errors will only be reported as warning. The user can be bootstrapping
    // and do an update later
    if not FileExists(GlobalOptions.LocalPackagesFile) then
    begin
      try
        laz_pkghandler.Laz_ExecuteAction('', 'laz_update');
      except
        on E: Exception do
          pkgglobals.Log(vlWarning, E.Message);
      end;
    end;
    LoadLocalAvailableRepository;
    FindInstalledPackages(FPMakeCompilerOptions, True);
    CheckFPMakeDependencies;
    // We only need to reload the status when we use a different
    // configuration for compiling fpmake
    if GlobalOptions.CompilerConfig <> GlobalOptions.FPMakeCompilerConfig then
      FindInstalledPackages(CompilerOptions, True);

    // Check for broken dependencies
    if not GlobalOptions.AllowBroken and
      (((ParaAction = 'laz_fixbroken') and (ParaPackages.Count > 0)) or
      (ParaAction = 'laz_compile') or (ParaAction = 'laz_build') or
      (ParaAction = 'laz_install') or (ParaAction = 'laz_archive')) then
    begin
      pkgglobals.Log(vlDebug, SLogCheckBrokenDependenvies);
      SL := TStringList.Create;
      if FindBrokenPackages(SL) then
        Error(SErrBrokenPackagesFound);
      FreeAndNil(SL);
    end;

    if ParaPackages.Count = 0 then
    begin
      ActionPackage := AvailableRepository.AddPackage(CurrentDirPackageName);
      laz_pkghandler.Laz_ExecuteAction(CurrentDirPackageName, ParaAction);
    end
    else
    begin
      // Process packages
      for i := 0 to ParaPackages.Count - 1 do
      begin
        if sametext(ExtractFileExt(ParaPackages[i]), '.zip') and
          FileExists(ParaPackages[i]) then
        begin
          ActionPackage := AvailableRepository.AddPackage(CmdLinePackageName);
          ActionPackage.LocalFileName := ExpandFileName(ParaPackages[i]);
          laz_pkghandler.Laz_ExecuteAction(CmdLinePackageName, ParaAction);
        end
        else
        begin
          pkgglobals.Log(vlDebug, SLogCommandLineAction,['[' + ParaPackages[i] + ']', ParaAction]);
          laz_pkghandler.Laz_ExecuteAction(ParaPackages[i], ParaAction);
        end;
      end;
    end;

    // Recompile all packages dependent on this package
    if (ParaAction = 'install') and not GlobalOptions.SkipFixBrokenAfterInstall then
      laz_pkghandler.Laz_ExecuteAction('', 'fixbroken');

  except
    On E: Exception do
    begin
      Error(SErrException);
      Error(E.Message);
      exit;
    end;
  end;
  SetCurrentDir(OldCurrDir);
end;

function PkgColumnValue(AName: string; pkg: TLazPackageData): string;
begin
  if AName = 'Name' then
    Result := pkg.Name;
  if AName = 'Installed' then
    Result := pkg.InstalledVersion;
  if AName = 'Available' then
    Result := pkg.AvialableVersion;
  if AName = 'Description' then
    Result := pkg.Description;
  if AName = 'State' then
    Result := pkg.State;
  if AName = 'Keywords' then
    Result := pkg.Keywords;
  if AName = 'Category' then
    Result := pkg.Category;
  if AName = 'Support' then
    Result := pkg.Support;
  if AName = 'Author' then
    Result := pkg.Author;
  if AName = 'License' then
    Result := pkg.License;
  if AName = 'HomepageURL' then
    Result := pkg.HomepageURL;
  if AName = 'DownloadURL' then
    Result := pkg.DownloadURL;
  if AName = 'FileName' then
    Result := pkg.FileName;
  if AName = 'Email' then
    Result := pkg.Email;
  if AName = 'OS' then
    Result := pkg.OS;
  if AName = 'CPU' then
    Result := pkg.CPU;
end;

procedure TFppkgForm.UpdatePackageListView;
var
  i, c: integer;
  li: TListItem;
  pkg: TLazPackageData;
  col: TListColumn;
  f: boolean;
begin
  //setup the package listview
  PackageListView.BeginUpdate;

  //setup columns
  PackageListView.Columns.Clear;
  for c := 0 to LazPkgOptions.PkgColumnCount - 1 do
    if LazPkgOptions.PkgColumns[c].Visible then
    begin
      col := PackageListView.Columns.Add;
      col.Caption := LazPkgOptions.PkgColumns[c].Name;
      col.AutoSize := True;
    end;

  PackageListView.Clear;

  for i := 0 to Laz_Packages.Count - 1 do
  begin
    pkg := Laz_Packages.PkgData[i];

    if FindSearchPhrase(pkg) and FindCategory(pkg) and FindSupport(pkg) then
    begin
      li := PackageListView.Items.Add;

      f := False;
      for c := 0 to LazPkgOptions.PkgColumnCount - 1 do
      begin
        if LazPkgOptions.PkgColumns[c].Visible then
          if not f then
          begin
            li.Caption := PkgColumnValue(LazPkgOptions.PkgColumns[c].Name, pkg);
            f := True;
          end
          else
            li.SubItems.Add(PkgColumnValue(LazPkgOptions.PkgColumns[c].Name, pkg));
      end;

      //add images to supported packages
      if LowerCase(pkg.Support) = 'fpc' then
        li.ImageIndex := FPC_SUPPORTED
      else
      if LowerCase(pkg.Support) = 'lazarus' then
        li.ImageIndex := LAZARUS_SUPPORTED
      else
        li.ImageIndex := COMMUNITY_SUPPORTED;
    end;
  end;

  PackageListView.EndUpdate;
end;

procedure TFppkgForm.ListPackages;
var
  s: TStringList;
  i: integer;
  pkg: TLazPackageData;
  cat: string;
begin
  //update the package list
  s := TStringList.Create;
  DoRun(FppkgCfg, 'laz_list', s);
  s.Free;

  //setup the categories listview
  CategoryCheckListBox.Clear;
  CategoryCheckListBox.Items.Add('All');
  for i := 0 to Laz_Packages.Count - 1 do
  begin
    pkg := Laz_Packages.PkgData[i];

    if pkg.Category = '' then
      cat := 'Unknown'
    else
      cat := pkg.Category;

    if CategoryCheckListBox.Items.IndexOf(cat) = -1 then
      CategoryCheckListBox.Items.Add(cat);
  end;

  //check all the items
  for i := 0 to CategoryCheckListBox.Count - 1 do
    CategoryCheckListBox.Checked[i] := True;
end;

end.

