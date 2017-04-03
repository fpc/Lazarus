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
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit fppkg_mainfrm;

{$mode objfpc}{$H+}

{$ifndef ver3}
{$error This packagemanager only works with fpc 3.1.1 or higher.}
{$endif}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, Buttons,
  Menus, CheckLst, Dialogs, fppkg_const,
  fppkg_optionsfrm, fppkg_details,
  pkgFppkg,
  //IDE interface
  {$IFDEF LazarusIDEPackage}
    PackageIntf, IDECommands, contnrs, fppkg_lpk,
  {$ENDIF}
  // Repository handler objects
  fprepos,
  pkgmessages, pkgglobals, pkgoptions, pkgrepos, laz_pkgrepos,
  // Package Handler components
  pkghandler, pkgcommands,
  //downloader
  pkgfphttp;

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
    procedure PanelClick(Sender: TObject);
    procedure SearchButtonClick(Sender: TObject);
    procedure SearchEditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure SupportCheckGroupItemClick(Sender: TObject; Index: integer);
    procedure UpdateButtonClick(Sender: TObject);
  private
    SearchPhrases: TStrings;

    FErrors: TStrings;
    FFPpkg: TpkgFPpkg;
    FLazPackages: TLazPackages;

    function PkgColumnValue(AName: string; pkg: TLazPackage): string;

    function FindSearchPhrase(pkg: TLazPackageData): boolean;
    function FindCategory(pkg: TLazPackageData): boolean;
    function FindSupport(pkg: TLazPackageData): boolean;
    procedure GetSelectedPackages(var s: TStrings);

    procedure MaybeCreateLocalDirs;
    procedure DoRun(cfg: TFppkgConfigOptions; ParaAction: string; ParaPackages: TStrings; Description: string);

    procedure UpdatePackageListView;
    procedure ListPackages;

    procedure RescanPackages;
    procedure SetupColumns;

    procedure ShowError(const Description, Error: String);
  public
    procedure OnError(const Msg: String);
  end;

var
  FppkgForm: TFppkgForm;
  FppkgCfg: TFppkgConfigOptions;

implementation

{$R *.lfm}

uses
  Masks, fppkg_aboutfrm;

resourcestring
  SErrActionFailed    = 'Failed to %s: ' + sLineBreak + sLineBreak + '%s';
  SMsgActionSucceeded = '%s succeeded.';
  SActFixBroken       = 'fix broken packages';
  SActCleanPackages   = 'clean package(s)';
  SActCompilePackages = 'compile packages';
  SActDownloadPackages= 'download packages';
  SActArchivePackages = 'create archive(s) for package(s)';
  SActBuildPackages   = 'build package(s)';
  SActInstallPackages = 'install package(s)';
  SActUpdate          = 'update repository';
  SActInitializeFppkg = 'initialize fppkg';


procedure LazLog(Level: TLogLevel; const Msg: string);
var
  Prefix : string;
begin
  if not(Level in LogLevels) then
    exit;
  Prefix:='';
  case Level of
    {$IF FPC_FULLVERSION > 20602}
    llWarning :
      Prefix:=SWarning;
    llError :
      Prefix:=SError;
    {$ELSE}
    vlWarning :
      Prefix:=SWarning;
    vlError :
      Prefix:=SError;
    {$ENDIF}
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
  if Assigned(FppkgForm) then
    FppkgForm.OnError(Msg)
  else
    ShowMessage(Msg);
end;

{ TFppkgForm }

procedure TFppkgForm.FixBrokenButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;
  DoRun(FppkgCfg, 'fixbroken', s, SActFixBroken);
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
    ShowError(SActCleanPackages, SErrNoPackageSpecified)
  else
    DoRun(FppkgCfg, 'clean', S, SActCleanPackages);

  s.Free;
end;

procedure TFppkgForm.CompileButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;

  GetSelectedPackages(s);

  if s.Count = 0 then
    ShowError(SActCompilePackages, SErrNoPackageSpecified)
  else
    DoRun(FppkgCfg, 'compile', s, SActCompilePackages);

  s.Free;
end;

procedure TFppkgForm.DownloadButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;

  GetSelectedPackages(s);

  if s.Count = 0 then
    ShowError(SActDownloadPackages, SErrNoPackageSpecified)
  else
    DoRun(FppkgCfg, 'download', s, SActDownloadPackages);

  s.Free;
end;

procedure TFppkgForm.ArchiveButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;

  GetSelectedPackages(s);

  if s.Count = 0 then
    ShowError(SActArchivePackages, SErrNoPackageSpecified)
  else
    DoRun(FppkgCfg, 'archive', s, SActArchivePackages);

  s.Free;
end;

procedure TFppkgForm.BuildButtonClick(Sender: TObject);
var
  s: TStrings;
begin
  s := TStringList.Create;

  GetSelectedPackages(s);

  if s.Count = 0 then
    ShowError(SActBuildPackages, SErrNoPackageSpecified)
  else
    DoRun(FppkgCfg, 'build', s, SActBuildPackages);

  s.Free;
end;

procedure TFppkgForm.CategoryCheckListBoxClickCheck(Sender: TObject);
begin
  UpdatePackageListView;
end;

procedure TFppkgForm.FormCreate(Sender: TObject);
var
  i: Integer;
  StoredGetVendorName: TGetVendorNameEvent;
  StoredGetApplicationName: TGetAppNameEvent;
begin
  //setup log callback function
  LogLevels := AllLogLevels;
  LogHandler := @LazLog;

  //setup error callback function
  ErrorHandler := @LazError;

  FFPpkg := TpkgFPpkg.Create(Self);

  FFPpkg.InitializeGlobalOptions('');

  FFPpkg.Options.GlobalSection.Downloader := 'FPC';

  SetLength(FPMKUnitDeps,FPMKUnitDepDefaultCount);
  for i := 0 to FPMKUnitDepDefaultCount-1 do
    FPMKUnitDeps[i]:=FPMKUnitDepsDefaults[i];

  FFPpkg.InitializeCompilerOptions;

  FFPpkg.CompilerOptions.InitCompilerDefaults;
  FFPpkg.FpmakeCompilerOptions.InitCompilerDefaults;
  FFPpkg.CompilerOptions.CheckCompilerValues;
  FFPpkg.FpmakeCompilerOptions.CheckCompilerValues;

  FFPpkg.LoadLocalAvailableMirrors;

  Caption := rsFreePascalPackageManagerForLazarus;

  SupportCheckGroup.Checked[0] := True;
  SupportCheckGroup.Checked[1] := True;
  SupportCheckGroup.Checked[2] := True;

  SearchPhrases := TStringList.Create;
  SearchPhrases.Delimiter := ' ';

  FLazPackages := TLazPackages.Create(Self);
  FLazPackages.PackageManager := FFPpkg;

  SetupColumns;

  RescanPackages;

  FErrors := TStringList.Create;
end;

procedure TFppkgForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FErrors);
  SearchPhrases.Free;
end;

procedure TFppkgForm.InstallButtonClick(Sender: TObject);
var
  s: TStrings;
  {$IFDEF LazarusIDEPackage}
(*
    P: TLazFPPackage;
    RebuildLazarus: boolean;
    PkgFlags: TPkgInstallInIDEFlags;
    APackage: TIDEPackage;
    InstPackages: TObjectList;
    i, j, k: integer;
    LPKFile: string;
*)
  {$ENDIF}
begin
  s := TStringList.Create;

  GetSelectedPackages(s);

  if s.Count = 0 then
    ShowError(SActInstallPackages, SErrNoPackageSpecified)
  else
  begin
    DoRun(FppkgCfg, 'install', s, SActInstallPackages);
    ListPackages;
    UpdatePackageListView;

    {$IFDEF LazarusIDEPackage}
(*
    RebuildLazarus := False;
    InstPackages:=TObjectList.create;
    try
      PkgFlags := [piiifQuiet];
      for i:=0 to s.Count-1 do
      begin
        P := Repository.FindPackage(s.Strings[i]) as TLazFPPackage;
        if P.HasLazarusPackageFiles then
          for j := 0 to p.LazarusPackageFiles.Count-1 do
          begin
            LPKFile := P.LazarusPackageFiles.Strings[j];

            //make sure to determine if the IDE needs to be rebuilt
            if LPKStatus(LPKFile) in [lpDesigntime, lpBoth] then
            begin
              RebuildLazarus := True;
              PkgFlags := PkgFlags + [piiifRebuildIDE];
            end;

            //add LPK file to IDE
            {$note what's the modal result doing here?}

            PackageEditingInterface.DoOpenPackageFile(LPKFile,[pofRevert,pofDoNotOpenEditor],true);
            APackage := nil;
            for k := 0 to PackageEditingInterface.GetPackageCount-1 do
              if PackageEditingInterface.GetPackages(k).Filename = LPKFile then
                begin
                  APackage := PackageEditingInterface.GetPackages(k);
                  break;
                end;
            if not assigned(APackage) then
              raise exception.create('Failed to find just installed package. Something went wrong.');
            InstPackages.Add(APackage);
          end;
      end;

      if InstPackages.Count>0 then
        PackageEditingInterface.InstallPackages(InstPackages,PkgFlags);
    finally
      InstPackages.Free;
    end;
    if RebuildLazarus then
      ExecuteIDECommand(Self, ecBuildLazarus);
*)
    {$ENDIF}
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

procedure TFppkgForm.PanelClick(Sender: TObject);
begin

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
  DoRun(FppkgCfg, 'update', s, SActUpdate);
  UpdatePackageListView;
  s.Free;
end;

procedure TFppkgForm.MaybeCreateLocalDirs;
begin
  ForceDirectories(FFPpkg.Options.GlobalSection.BuildDir);
  ForceDirectories(FFPpkg.Options.GlobalSection.ArchivesDir);
  ForceDirectories(FFPpkg.Options.GlobalSection.CompilerConfigDir);
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

procedure TFppkgForm.DoRun(cfg: TFppkgConfigOptions; ParaAction: string; ParaPackages: TStrings;
  Description: string);
var
  OldCurrDir: string;
  i: integer;
  s: string;
begin
  pkghandler.ClearExecutedAction;
  FErrors.Clear;

  OldCurrDir := GetCurrentDir;
  try
    if ParaPackages.Count = 0 then
    begin
      pkghandler.ExecuteAction(CurrentDirPackageName, ParaAction, FFPpkg);
    end
    else
    begin
      // Process packages
      for i := 0 to ParaPackages.Count - 1 do
      begin
        pkgglobals.Log({$IF FPC_FULLVERSION > 20602}llDebug{$ELSE}vlDebug{$ENDIF}, SLogCommandLineAction,['[' + ParaPackages[i] + ']', ParaAction]);
        pkghandler.ExecuteAction(ParaPackages[i], ParaAction, FFPpkg);
      end;
    end;

    s := Format(SMsgActionSucceeded, [Description]);
    s[1] := upCase(s[1]);
    if FErrors.Count=0 then
      ShowMessage(s);
  except
    On E: Exception do
    begin
      Error(SErrException + LineEnding + E.Message);
    end;
  end;
  SetCurrentDir(OldCurrDir);
  if FErrors.Count>0 then
    ShowError(Description, FErrors[0]);
  RescanPackages;
end;

function TFppkgForm.PkgColumnValue(AName: string; pkg: TLazPackage): string;
begin
  case AName of
    'Name'    : Result := pkg.Name;
    'State'   : Result := SLazPackageInstallStateString[pkg.State];
    'Version' : Result := pkg.Version;
    'Info'    : Result := pkg.GetInfo(FFPpkg);
    'Description' : Result := pkg.Description;
  end;
end;

procedure TFppkgForm.UpdatePackageListView;
var
  i, c: integer;
  li: TListItem;
  pkg: TLazPackage;
  f: boolean;
begin
  //setup the package listview
  PackageListView.BeginUpdate;

  PackageListView.Clear;

  for i := 0 to FLazPackages.Count - 1 do
  begin
    pkg := FLazPackages.PkgData[i];

    //if FindSearchPhrase(pkg) and FindCategory(pkg) and FindSupport(pkg) then
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
{      if LowerCase(pkg.Support) = 'fpc' then
        li.ImageIndex := FPC_SUPPORTED
      else
      if LowerCase(pkg.Support) = 'lazarus' then
        li.ImageIndex := LAZARUS_SUPPORTED
      else
        li.ImageIndex := COMMUNITY_SUPPORTED;
}    end;
  end;

  PackageListView.EndUpdate;
end;

procedure TFppkgForm.ListPackages;
var
  i, RepoIndex: integer;
  cat: string;
  Repository: TFPRepository;
  Package: TFPPackage;
  pkg: TLazPackage;
begin
  //update the package list
  FLazPackages.Clear;

  for RepoIndex := 0 to FFPpkg.RepositoryList.Count -1 do
  begin
    Repository := FFPpkg.RepositoryList.Items[RepoIndex] as TFPRepository;
    for i := 0 to Repository.PackageCount -1 do
    begin
      Package := Repository.Packages[i];
      FLazPackages.AddFPPackage(Package);
    end;
  end;

{  s := TStringList.Create;
  DoRun(FppkgCfg, 'laz_list', s);
  s.Free;
  end;   }

  //setup the categories listview
  CategoryCheckListBox.Clear;
  CategoryCheckListBox.Items.Add('All');
  for i := 0 to FLazPackages.Count - 1 do
  begin
    pkg := FLazPackages.PkgData[i];

//    if pkg.Category = '' then
      cat := 'Unknown';
//    else
//      cat := pkg.Category;

    if CategoryCheckListBox.Items.IndexOf(cat) = -1 then
      CategoryCheckListBox.Items.Add(cat);
  end;

  //check all the items
  for i := 0 to CategoryCheckListBox.Count - 1 do
    CategoryCheckListBox.Checked[i] := True;
end;

procedure TFppkgForm.RescanPackages;
begin
  FFPpkg.ScanAvailablePackages;
  FFPpkg.ScanPackages;
  ListPackages;
  UpdatePackageListView;
end;

procedure TFppkgForm.SetupColumns;
var
  c: Integer;
  col: TListColumn;
begin
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
  PackageListView.EndUpdate;
end;

procedure TFppkgForm.ShowError(const Description, Error: String);
begin
  ShowMessage(Format(SErrActionFailed, [Description, Error]))
end;

procedure TFppkgForm.OnError(const Msg: String);
begin
  // Cache all errors and show the them after a command has been finished
  // completely. This because most problems lead to multiple errors, which is
  // annoying in a GUI-environment
  if Assigned(FErrors) then
    FErrors.Add(Msg)
  else
    ShowError(SActInitializeFppkg, Msg);
end;

end.

