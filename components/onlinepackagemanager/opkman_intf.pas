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
  opkman_timer, opkman_downloader;

type

  { TOPMInterfaceEx }

  TOPMInterfaceEx = class(TOPMInterface)
  private
    FWaitForIDE: TThreadTimer;
    procedure DoWaitForIDE(Sender: TObject);
    procedure InitOPM;
  public
    constructor Create;
    destructor Destroy; override;
  public
  end;

implementation

uses opkman_serializablepackages, opkman_common, opkman_options;

{ TOPMMain }

constructor TOPMInterfaceEx.Create;
begin
  FWaitForIDE := TThreadTimer.Create;
  FWaitForIDE.Interval := 100;
  FWaitForIDE.OnTimer := @DoWaitForIDE;
  FWaitForIDE.StartTimer;
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
  PackageDownloader := TPackageDownloader.Create(Options.RemoteRepository[Options.ActiveRepositoryIndex]);
  InstallPackageList := TObjectList.Create(True);
  PackageDownloader.DownloadJSON(Options.ConTimeOut*1000);
end;

destructor TOPMInterfaceEx.Destroy;
begin
  PackageDownloader.Free;
  SerializablePackages.Free;
  Options.Free;
  InstallPackageList.Free;
  inherited Destroy;
end;

end.

