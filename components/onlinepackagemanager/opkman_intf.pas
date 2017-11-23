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
  Classes, SysUtils, contnrs, fpjson,
  // IdeIntf
  LazIDEIntf, PackageIntf, PackageLinkIntf, PackageDependencyIntf,
  // OPM
  opkman_timer, opkman_downloader, opkman_serializablepackages;

type

  { TOPMInterfaceEx }

  TOPMInterfaceEx = class(TOPMInterface)
  private
    FOPMPackageLinks: TList;
    FWaitForIDE: TThreadTimer;
    procedure DoWaitForIDE(Sender: TObject);
    procedure DoUpdatePackageLinks(Sender: TObject);
    procedure InitOPM;
    procedure SynchronizePackages;
    function IsInList(const AName, AURL: String): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public

  end;

implementation

uses opkman_common, opkman_options;

{ TOPMInterfaceEx }

constructor TOPMInterfaceEx.Create;
begin
  FOPMPackageLinks := TList.Create;
  FWaitForIDE := TThreadTimer.Create;
  FWaitForIDE.Interval := 100;
  FWaitForIDE.OnTimer := @DoWaitForIDE;
  FWaitForIDE.StartTimer;
end;

destructor TOPMInterfaceEx.Destroy;
begin
  FOPMPackageLinks.Clear;
  FOPMPackageLinks.Free;
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

function TOPMInterfaceEx.IsInList(const AName, AURL: String): Boolean;
var
  I: Integer;
  PackageLink: TPackageLink;
begin
  Result := False;
  for I := 0 to FOPMPackageLinks.Count - 1 do
  begin
    PackageLink := TPackageLink(FOPMPackageLinks.Items[I]);
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
  for I := 0 to SerializablePackages.Count - 1 do
  begin
    MetaPackage := SerializablePackages.Items[I];
    for J := 0 to MetaPackage.LazarusPackages.Count - 1 do
    begin
      LazPackage := TLazarusPackage(MetaPackage.LazarusPackages.Items[J]);
      FileName := Options.LocalRepositoryPackages + MetaPackage.PackageBaseDir + LazPackage.PackageRelativePath + LazPackage.Name;
      Name := StringReplace(LazPackage.Name, '.lpk', '', [rfReplaceAll, rfIgnoreCase]);
      URL := Options.RemoteRepository[Options.ActiveRepositoryIndex] + MetaPackage.RepositoryFileName;
      if not IsInList(Name, URL) then
      begin
        PackageLink := PkgLinks.AddOnlineLink(FileName, Name, URL);
        if PackageLink <> nil then
        begin
          PackageLink.Version.Assign(LazPackage.Version);
          PackageLink.LPKFileDate := MetaPackage.RepositoryDate;
          FOPMPackageLinks.Add(PackageLink);
        end;
      end;
    end;
  end;
end;


end.

