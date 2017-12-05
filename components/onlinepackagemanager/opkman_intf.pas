{
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

 Author: Balázs Székely
 Abstract:
   This unit allows OPM to interact with the Lazarus package system}

unit opkman_intf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Dialogs, Controls, contnrs, fpjson,
  // IdeIntf
  LazIDEIntf, PackageIntf, PackageLinkIntf, PackageDependencyIntf,
  // OPM
  opkman_timer, opkman_downloader, opkman_serializablepackages;

type

  { TOPMInterfaceEx }

  TOPMInterfaceEx = class(TOPMInterface)
  private
    FPackagesToInstall: TObjectList;
    FPackageDependecies: TObjectList;
    FPackageLinks: TObjectList;
    FWaitForIDE: TThreadTimer;
    procedure DoWaitForIDE(Sender: TObject);
    procedure DoUpdatePackageLinks(Sender: TObject);
    procedure InitOPM;
    procedure SynchronizePackages;
    procedure AddToInstallList(const AName, AURL: String);
    function IsInLinkList(const AName, AURL: String): Boolean;
    function ResolveDependencies(AParentForm: TForm): TModalResult;
    function CanInstallPackages(APkgLinks: TList; AParentForm: TForm): TModalResult;
  public
    constructor Create;
    destructor Destroy; override;
  public
    function InstallPackages(APkgLinks: TList; AParentForm: TForm;
      const AResolveDependencies: Boolean = True): TModalResult; override;
  end;

implementation

uses opkman_common, opkman_options, opkman_const;

{ TOPMInterfaceEx }

constructor TOPMInterfaceEx.Create;
begin
  FPackageLinks := TObjectList.Create(False);
  FPackagesToInstall := TObjectList.Create(False);
  FPackageDependecies := TObjectList.Create(False);
  FWaitForIDE := TThreadTimer.Create;
  FWaitForIDE.Interval := 100;
  FWaitForIDE.OnTimer := @DoWaitForIDE;
  FWaitForIDE.StartTimer;
end;

destructor TOPMInterfaceEx.Destroy;
begin
  FPackageLinks.Clear;
  FPackageLinks.Free;
  FPackagesToInstall.Clear;
  FPackagesToInstall.Free;
  FPackageDependecies.Clear;
  FPackageDependecies.Free;
  PackageDownloader.Free;
  SerializablePackages.Free;
  Options.Free;
  InstallPackageList.Free;
  inherited Destroy;
end;

procedure TOPMInterfaceEx.DoWaitForIDE(Sender: TObject);
begin
  if Assigned(LazarusIDE) and Assigned(PackageEditingInterface) then
  begin
    InitOPM;
    FWaitForIDE.StopTimer;
    FWaitForIDE.Terminate;
  end;
end;

procedure TOPMInterfaceEx.InitOPM;
begin
  InitLocalRepository;
  Options := TOptions.Create(LocalRepositoryConfigFile);
  SerializablePackages := TSerializablePackages.Create;
  SerializablePackages.OnUpdatePackageLinks := @DoUpdatePackageLinks;
  PackageDownloader := TPackageDownloader.Create(Options.RemoteRepository[Options.ActiveRepositoryIndex]);
  InstallPackageList := TObjectList.Create(True);
  PackageDownloader.DownloadJSON(Options.ConTimeOut*1000);
end;

procedure TOPMInterfaceEx.DoUpdatePackageLinks(Sender: TObject);
begin
  SynchronizePackages;
end;

function TOPMInterfaceEx.IsInLinkList(const AName, AURL: String): Boolean;
var
  I: Integer;
  PackageLink: TPackageLink;
begin
  Result := False;
  for I := 0 to FPackageLinks.Count - 1 do
  begin
    PackageLink := TPackageLink(FPackageLinks.Items[I]);
    if (UpperCase(PackageLink.Name) = UpperCase(AName)) and (UpperCase(PackageLink.LPKUrl) = UpperCase(AURL)) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TOPMInterfaceEx.SynchronizePackages;
var
  I, J: Integer;
  MetaPackage: TMetaPackage;
  LazPackage: TLazarusPackage;
  PackageLink: TPackageLink;
  FileName, Name, URL: String;
begin
  PkgLinks.ClearOnlineLinks;
  FPackageLinks.Clear;
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    MetaPackage := SerializablePackages.Items[I];
    for J := 0 to MetaPackage.LazarusPackages.Count - 1 do
    begin
      LazPackage := TLazarusPackage(MetaPackage.LazarusPackages.Items[J]);
      FileName := Options.LocalRepositoryPackages + MetaPackage.PackageBaseDir + LazPackage.PackageRelativePath + LazPackage.Name;
      Name := StringReplace(LazPackage.Name, '.lpk', '', [rfReplaceAll, rfIgnoreCase]);
      URL := Options.RemoteRepository[Options.ActiveRepositoryIndex] + MetaPackage.RepositoryFileName;
      if not IsInLinkList(Name, URL) then
      begin
        PackageLink := PkgLinks.AddOnlineLink(FileName, Name, URL);
        if PackageLink <> nil then
        begin
          PackageLink.Version.Assign(LazPackage.Version);
          PackageLink.LPKFileDate := MetaPackage.RepositoryDate;
          FPackageLinks.Add(PackageLink);
        end;
      end;
    end;
  end;
end;

procedure TOPMInterfaceEx.AddToInstallList(const AName, AURL: String);
var
  I, J: Integer;
  MetaPackage: TMetaPackage;
  LazPackage: TLazarusPackage;
begin
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    MetaPackage := SerializablePackages.Items[I];
    for J := 0 to MetaPackage.LazarusPackages.Count - 1 do
    begin
      LazPackage := TLazarusPackage(MetaPackage.LazarusPackages.Items[J]);
      if (UpperCase(LazPackage.Name) = UpperCase(AName)) and
         (UpperCase(Options.RemoteRepository[Options.ActiveRepositoryIndex] + MetaPackage.RepositoryFileName) = UpperCase(AURL)) then
      begin
        MetaPackage.Checked := True;
        LazPackage.Checked := True;
        FPackagesToInstall.Add(LazPackage);
        Break;
      end;
    end;
  end;
end;

function TOPMInterfaceEx.ResolveDependencies(AParentForm: TForm): TModalResult;
var
  I, J: Integer;
  PackageList: TObjectList;
  PkgFileName: String;
  DependencyPkg: TLazarusPackage;
  Msg: String;
begin
  Result := mrNone;
  FPackageDependecies.Clear;
  for I := 0 to FPackagesToInstall.Count - 1 do
  begin
    PackageList := TObjectList.Create(True);
    try
      SerializablePackages.GetPackageDependencies(TLazarusPackage(FPackagesToInstall.Items[I]).Name, PackageList, True, True);
      for J := 0 to PackageList.Count - 1 do
      begin
        PkgFileName := TPackageDependency(PackageList.Items[J]).PkgFileName + '.lpk';
        DependencyPkg := SerializablePackages.FindLazarusPackage(PkgFileName);
        if DependencyPkg <> nil then
        begin
          if (not DependencyPkg.Checked) and
              ((SerializablePackages.IsDependencyOk(TPackageDependency(PackageList.Items[I]), DependencyPkg)) and
               ((not (DependencyPkg.PackageState = psInstalled)) or ((DependencyPkg.PackageState = psInstalled) and (not (SerializablePackages.IsInstalledVersionOk(TPackageDependency(PackageList.Items[I]), DependencyPkg.InstalledFileVersion)))))) then
          begin
            if (Result = mrNone) or (Result = mrYes) then
              begin
                Msg := Format(rsMainFrm_rsPackageDependency0, [TLazarusPackage(FPackagesToInstall.Items[I]).Name, DependencyPkg.Name]);
                Result := MessageDlgEx(Msg, mtConfirmation, [mbYes, mbYesToAll, mbNo, mbNoToAll, mbCancel], AParentForm);
                if Result in [mrNo, mrNoToAll] then
                  MessageDlgEx(rsMainFrm_rsPackageDependency1, mtInformation, [mbOk], AParentForm);
                if (Result = mrNoToAll) or (Result = mrCancel) then
                  Exit;
              end;
              if Result in [mrYes, mrYesToAll] then
              begin
                DependencyPkg.Checked := True;
                FPackageDependecies.Add(DependencyPkg);
              end;
          end;
        end;
      end;
    finally
      PackageList.Free;
    end;
  end;
end;

function TOPMInterfaceEx.CanInstallPackages(APkgLinks: TList;
  AParentForm: TForm): TModalResult;
var
  PkgListStr: String;
  I: Integer;
  PackageLink: TPackageLink;
begin
  Result := mrOK;
  PkgListStr := '';
  for I := 0 to APkgLinks.Count - 1 do
  begin
    PackageLink := TPackageLink(APkgLinks.Items[I]);
    if PkgListStr = '' then
      PkgListStr := '"' + PackageLink.Name + '"'
    else
      PkgListStr := PkgListStr + ', "' + PackageLink.Name + '"';
  end;
  if Trim(PkgListStr) <> '' then
    if MessageDlgEx(rsOPMInterfaceConf + ' ' + PkgListStr + ' ?', mtConfirmation, [mbYes, mbNo], AParentForm) <> mrYes then
      Result := mrCancel;
end;

function TOPMInterfaceEx.InstallPackages(APkgLinks: TList; AParentForm: TForm;
  const AResolveDependencies: Boolean = True): TModalResult;
var
  I: Integer;
begin
  Result := CanInstallPackages(APkgLinks, AParentForm);
  if Result = mrCancel then
    Exit;

  FPackagesToInstall.Clear;
  for I := 0 to APkgLinks.Count - 1 do
    AddToInstallList(TPackageLink(APkgLinks.Items[I]).Name + '.lpk', TPackageLink(APkgLinks.Items[I]).LPKUrl);

  if AResolveDependencies then
  begin
    if ResolveDependencies(AParentForm) = mrCancel then
      Exit;
    for I := 0 to FPackageDependecies.Count - 1 do
      FPackagesToInstall.Insert(0, FPackageDependecies.Items[I]);
  end;

  MessageDlgEx('Not yet implemented!', mtInformation, [mbOk], AParentForm);
 { for I := 0 to FPackagesToInstall.Count - 1 do
    MessageDlgEx(TLazarusPackage(FPackagesToInstall.Items[I]).Name + sLineBreak +
                TLazarusPackage(FPackagesToInstall.Items[I]).Author, mtInformation, [mbOk], AParentForm);}
  Result := mrOk;
end;

end.

