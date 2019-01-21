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
  Implementation of the package installer class.
Note: Compiling packages(Linux-GTK2) is only possible from the main thread so TPackageInstaller
is no longer a TThread(2016.11.04).
}
unit opkman_installer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs,
  // LCL
  Controls, LCLVersion,
  // IdeIntf
  PackageIntf,
  // OpkMan
  opkman_serializablepackages, opkman_const, opkman_common;

type
  TInstallStatus = (isSuccess, isPartiallyFailed, isFailed);
  TInstallMessage = (imOpenPackage, imOpenPackageSuccess, imOpenPackageError,
                     imCompilePackage, imCompilePackageSuccess, imCompilePackageError,
                     imInstallPackage, imInstallPackageSuccess, imInstallPackageError,
                     imDependencyError, imPackageCompleted);

  { TPackageInstaller }
  TOnPackageInstallProgress = procedure(Sender: TObject; ACnt, ATotCnt: Integer; APackgaName: String; AInstallMessage: TInstallMessage) of object;
  TOnPackageInstallError = procedure(Sender: TObject; APackageName, AErrMsg: String) of object;
  TOnPackageInstallCompleted = procedure(Sender: TObject; ANeedToRebuild: Boolean; AInstallStatus: TInstallStatus) of object;

  TPackageInstaller = class
  private
    FNeedToBreak: Boolean;
    FNeedToRebuild: Boolean;
    FCnt: Integer;
    FTotCnt: Integer;
    FStarted: Boolean;
    FInstallStatus: TInstallStatus;
    FPackageList: TList;
    FToInstall: TStringList;
    FFileName: String;
    FUnresolvedFileName: String;
    FOnPackageInstallProgress: TOnPackageInstallProgress;
    FOnPackageInstallError: TOnPackageInstallError;
    FOnPackageInstallCompleted: TOnPackageInstallCompleted;
    function OpenPackage(const AFileName: String): TIDEPackage;
    function IsPackageInTheList(const AFileName: String): Boolean;
    function HasUnresolvedDependency(AName: String): Boolean;
    function CompilePackage(const AIDEPackage: TIDEPackage; ALazarusPkg: TLazarusPackage): Integer;
    function InstallPackage: Boolean;
    procedure OrderPackagesByDependecy;
    procedure PrepareInstallList;
    procedure DoOnPackageInstallProgress(const AInstallMessage: TInstallMessage; ALazarusPkg: TLazarusPackage);
    procedure DoOnPackageInstallError(const AInstallMessage: TInstallMessage; ALazarusPkg: TLazarusPackage);
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;
    procedure StartInstall;
    procedure StopInstall;
  published
    property NeedToBreak: Boolean read FNeedToBreak write FNeedToBreak;
    property OnPackageInstallProgress: TOnPackageInstallProgress read FOnPackageInstallProgress write FOnPackageInstallProgress;
    property OnPackageInstallError: TOnPackageInstallError read FOnPackageInstallError write FOnPackageInstallError;
    property OnPackageInstallCompleted: TOnPackageInstallCompleted read FOnPackageInstallCompleted write FOnPackageInstallCompleted;
  end;

var
  PackageInstaller: TPackageInstaller = nil;

implementation

{ TPackageInstaller }

constructor TPackageInstaller.Create;
begin
  FToInstall := TStringList.Create;
  FPackageList := TList.Create;
end;

destructor TPackageInstaller.Destroy;
begin
  FToInstall.Free;
  FPackageList.Free;
  inherited Destroy;
end;

function TPackageInstaller.OpenPackage(const AFileName: String): TIDEPackage;
var
  I: Integer;
begin
  Result := nil;
  if PackageEditingInterface.DoOpenPackageFile(AFileName, [pofRevert, pofDoNotOpenEditor], True) = mrOk then
  begin
    for I := 0 to PackageEditingInterface.GetPackageCount - 1 do
    begin
      if UpperCase(PackageEditingInterface.GetPackages(I).Filename) = UpperCase(AFileName) then
      begin
        Result := PackageEditingInterface.GetPackages(I);
        Break;
      end;
    end;
  end;
end;

function TPackageInstaller.IsPackageInTheList(const AFileName: String): Boolean;
var
  P, I: Integer;
  PkgFileName: String;
begin
  Result := False;
  for I := 0 to FToInstall.Count - 1 do
  begin
    P := Pos('  ', FToInstall.Strings[I]);
    if P <> 0 then
      PkgFileName := Copy(FToInstall.Strings[I], 1, P - 1)
    else
      PkgFileName := FToInstall.Strings[I];
    if UpperCase(PkgFileName) = UpperCase(AFileName) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TPackageInstaller.HasUnresolvedDependency(AName: String): Boolean;
var
  P: Integer;
begin
  Result := False;
  P := Pos('  ', AName);
  if P > 0 then
  begin
    FFileName := Copy(AName, 1, P - 1);
    Delete(AName, 1, P);
    FUnresolvedFileName := Trim(AName);
    Result := True;
  end
  else
    FFileName := AName;
end;

function TPackageInstaller.CompilePackage(const AIDEPackage: TIDEPackage;
  ALazarusPkg: TLazarusPackage): Integer;
begin
  Result := -1;
  DoOnPackageInstallProgress(imCompilePackage, ALazarusPkg);
  Result := PackageEditingInterface.DoCompilePackage(AIDEPackage, [pcfCleanCompile, pcfDoNotSaveEditorFiles], False);
end;

function TPackageInstaller.InstallPackage: Boolean;
var
  P: Integer;
  PackageName: String;
  NewPackageID: TLazPackageID;
begin
  Result := False;
  P := Pos('.lpk', FFileName);
  if P <> 0 then
  begin
    PackageName := Copy(FFileName, 1, P - 1);
    NewPackageID := TLazPackageID.Create;
    if NewPackageID.StringToID(PackageName) then
    begin
      InstallPackageList.Add(NewPackageID);
      Result := True;
    end;
  end;
end;

procedure TPackageInstaller.DoOnPackageInstallProgress(const AInstallMessage: TInstallMessage;
  ALazarusPkg: TLazarusPackage);
begin
  if AInstallMessage = imPackageCompleted then
     ALazarusPkg.PackageStates := ALazarusPkg.PackageStates + [psInstalled];
  if Assigned(FOnPackageInstallProgress) then
    FOnPackageInstallProgress(Self, FCnt, FTotCnt, FFileName, AInstallMessage);
  if AInstallMessage <> imPackageCompleted then
    Sleep(50);
end;

procedure TPackageInstaller.DoOnPackageInstallError(const AInstallMessage: TInstallMessage;
  ALazarusPkg: TLazarusPackage);
var
  ErrMsg: String;
begin
  case AInstallMessage of
    imOpenPackageError:
      ErrMsg := rsProgressFrm_Error7;
    imCompilePackageError:
      ErrMsg := rsProgressFrm_Error8;
    imInstallPackageError:
      ErrMsg := rsProgressFrm_Error9;
    imDependencyError:
      ErrMsg := Format(rsProgressFrm_Error4, [FUnresolvedFileName]);
    end;
  ALazarusPkg.PackageStates := ALazarusPkg.PackageStates - [psInstalled];
  ALazarusPkg.PackageStates := ALazarusPkg.PackageStates + [psError];
  if Assigned(FOnPackageInstallError) then
    FOnPackageInstallError(Self, FFileName, ErrMsg);
  Sleep(50);
end;

procedure TPackageInstaller.Execute;
var
  I: Integer;
  IDEPackage: TIDEPackage;
  PkgInstallInIDEFlags: TPkgInstallInIDEFlags;
  ErrCnt: Integer;
  LazarusPkg: TLazarusPackage;
  CanGo: Boolean;
  CompRes: Integer;
begin
  ErrCnt := 0;
  FCnt := 0;
  FInstallStatus := isFailed;
  for I := 0 to FToInstall.Count - 1 do
  begin
    if NeedToBreak then
      Break;
    CanGo := not HasUnresolvedDependency(FToInstall.Strings[I]);
    LazarusPkg := SerializablePackages.FindLazarusPackage(FFileName);
    if CanGo then
    begin
      Inc(FCnt);
      if not FNeedToRebuild then
        FNeedToRebuild := LazarusPkg.PackageType in [lptRunAndDesignTime, lptDesigntime];
      DoOnPackageInstallProgress(imOpenPackage, LazarusPkg);
      IDEPackage := OpenPackage(LazarusPkg.PackageAbsolutePath);
      if IDEPackage <> nil then
      begin
        DoOnPackageInstallProgress(imOpenPackageSuccess, LazarusPkg);
        CompRes := CompilePackage(IDEPAckage, LazarusPkg);
        case CompRes of
          -1, 1:
            begin
              if CompRes = 1 then
              begin
                DoOnPackageInstallProgress(imCompilePackageSuccess, LazarusPkg);
                if PackageAction = paUpdate then
                  if LazarusPkg.ForceNotify then
                    if LazarusPkg.InternalVersion > LazarusPkg.InternalVersionOld then
                      LazarusPkg.InternalVersionOld := LazarusPkg.InternalVersion;
              end;
              if LazarusPkg.PackageType in [lptRunAndDesignTime, lptDesigntime] then
              begin
                DoOnPackageInstallProgress(imInstallPackage, LazarusPkg);
                if InstallPackage then
                  DoOnPackageInstallProgress(imInstallpackageSuccess, LazarusPkg)
                else
                begin
                  Inc(ErrCnt);
                  DoOnPackageInstallError(imInstallPackageError, LazarusPkg);
                end;
              end;
            end;
          else
            begin
              Inc(ErrCnt);
              DoOnPackageInstallError(imCompilePackageError, LazarusPkg);
            end;
        end;
      end
      else
      begin
        Inc(ErrCnt);
        DoOnPackageInstallError(imOpenPackageError, LazarusPkg);
      end;
      DoOnPackageInstallProgress(imPackageCompleted, LazarusPkg);
    end
    else
    begin
      Inc(FCnt);
      DoOnPackageInstallProgress(imOpenPackage, LazarusPkg);
      DoOnPackageInstallProgress(imOpenPackageSuccess, LazarusPkg);
      Inc(ErrCnt);
      DoOnPackageInstallError(imDependencyError, LazarusPkg);
      DoOnPackageInstallProgress(imPackageCompleted, LazarusPkg);
    end;
  end;
  if InstallPackageList.Count > 0 then
  begin
    PkgInstallInIDEFlags := [piiifQuiet];
    if PackageEditingInterface.InstallPackages(InstallPackageList, PkgInstallInIDEFlags) = mrOk then
    begin
      if not FNeedToBreak then
      begin
        if ErrCnt = 0 then
          FInstallStatus := isSuccess
        else
          FInstallStatus := isPartiallyFailed;
      end;
    end;
  end;
  if Assigned(FOnPackageInstallCompleted) then
    FOnPackageInstallCompleted(Self, FNeedToRebuild, FInstallStatus);
end;

procedure TPackageInstaller.OrderPackagesByDependecy;
var
  I, J, K: Integer;
  SPos, EPos: Integer;
  PackageDependency: TPackageDependency;
  PackageDependecyList: TObjectList;
  LazarusPkg, DependecyPackage: TLazarusPackage;
  CanGo: Boolean;
begin
  PackageDependecyList := TObjectList.Create(True);
  try
    FPackageList.Clear;
    for I := 0 to SerializablePackages.Count - 1 do
    begin
      for J := 0 to SerializablePackages.Items[I].LazarusPackages.Count - 1 do
      begin
        LazarusPkg := TLazarusPackage(SerializablePackages.Items[I].LazarusPackages.Items[J]);
        if LazarusPkg.IsInstallable then
          FPackageList.Add(LazarusPkg);
      end;
    end;
    repeat
      CanGo := True;
      for I := FPackageList.Count - 1 downto 1 do
      begin
        if not CanGo then
          Break;
        for J := I - 1 downto 0 do
        begin
          LazarusPkg := TLazarusPackage(FPackageList.Items[J]);
          PackageDependecyList.Clear;
          SerializablePackages.GetPackageDependencies(LazarusPkg.Name, PackageDependecyList, True, True);
          if PackageDependecyList.Count > 0 then
          begin
            for K := 0 to PackageDependecyList.Count - 1 do
            begin
              PackageDependency := TPackageDependency(PackageDependecyList.Items[K]);
              DependecyPackage := SerializablePackages.FindLazarusPackage(PackageDependency.PkgFileName + '.lpk');
              if DependecyPackage <> nil then
              begin
                if UpperCase(DependecyPackage.Name) = UpperCase(TLazarusPackage(FPackageList.Items[I]).Name) then
                begin
                  CanGo := False;
                  SPos := I;
                  EPos := J;
                  Break;
                end;
              end;
            end;
          end;
        end;
      end;
      if CanGo = False then
      begin
        LazarusPkg := TLazarusPackage(FPackageList.Items[SPos]);
        FPackageList.Delete(SPos);
        FPackageList.Insert(EPos, LazarusPkg);
      end;
    until CanGo;
  finally
    PackageDependecyList.Free;
  end;
end;

procedure TPackageInstaller.PrepareInstallList;
var
  I, J: Integer;
  PackageDependency: TPackageDependency;
  PackageDependecyList: TObjectList;
  LazarusPkg, DependecyPackage: TLazarusPackage;
  DependencyFound: Boolean;
begin
  PackageDependecyList := TObjectList.Create(True);
  try
    for I := 0 to FPackageList.Count - 1 do
    begin
      LazarusPkg := TLazarusPackage(FPackageList.Items[I]);
      if LazarusPkg.IsInstallable then
      begin
        PackageDependecyList.Clear;
        SerializablePackages.GetPackageDependencies(LazarusPkg.Name, PackageDependecyList, True, True);
        if PackageDependecyList.Count > 0 then
        begin
          DependencyFound := True;
          for J := 0 to PackageDependecyList.Count - 1 do
          begin
            PackageDependency := TPackageDependency(PackageDependecyList.Items[J]);
            DependecyPackage := SerializablePackages.FindLazarusPackage(PackageDependency.PkgFileName + '.lpk');
            if DependecyPackage <> nil then
            begin
              if not ((DependecyPackage.PackageState = psInstalled)
              and (SerializablePackages.IsInstalledVersionOk(PackageDependency, DependecyPackage.VersionAsString))) then
              begin
                if ((DependecyPackage.IsInstallable)
                and (SerializablePackages.IsDependencyOk(PackageDependency, DependecyPackage))) then
                begin
                  if not IsPackageInTheList(DependecyPackage.Name) then
                    FToInstall.Add(DependecyPackage.Name);
                end
                else
                begin
                  DependencyFound := False;
                  Break;
                end;
              end;
            end;
         end;
         if (not DependencyFound) then
         begin
           if (not IsPackageInTheList(LazarusPkg.Name)) then
             FToInstall.Add(LazarusPkg.Name + '  ' + DependecyPackage.Name)
         end
       end;
       if (not IsPackageInTheList(LazarusPkg.Name)) then
          FToInstall.Add(LazarusPkg.Name);
      end;
    end;
  finally
    PackageDependecyList.Free;
  end
end;

procedure TPackageInstaller.StartInstall;
begin
  if FStarted then
    Exit;
  FStarted := True;
  FTotCnt := 0;
  OrderPackagesByDependecy;
  PrepareInstallList;
  FTotCnt := FToInstall.Count;
  Execute;
end;

procedure TPackageInstaller.StopInstall;
begin
  FNeedToBreak := True;
  FStarted := False;
end;

end.

