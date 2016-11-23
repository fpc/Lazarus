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
*   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
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
  Classes, SysUtils, contnrs, Controls, Forms, PackageIntf, LCLVersion,
  opkman_serializablepackages;


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
    FToInstall: TStringList;
    FPackageFileName: String;
    FUnresolvedPackageFileName: String;
    FOnPackageInstallProgress: TOnPackageInstallProgress;
    FOnPackageInstallError: TOnPackageInstallError;
    FOnPackageInstallCompleted: TOnPackageInstallCompleted;
    function OpenPackage(const APackageFileName: String): TIDEPackage;
    function IsPackageInTheList(const APackageFileName: String): Boolean;
    function HasUnresolvedDependency(AName: String): Boolean;
    function CompilePackage(const AIDEPackage: TIDEPackage; APackageFile: TPackageFile): Integer;
    function InstallPackage: Boolean;
    procedure DoOnPackageInstallProgress(const AInstallMessage: TInstallMessage; APackageFile: TPackageFile);
    procedure DoOnPackageInstallError(const AInstallMessage: TInstallMessage; APackageFile: TPackageFile);
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

uses opkman_const, opkman_common;
{ TPackageInstaller }

constructor TPackageInstaller.Create;
begin
  FToInstall := TStringList.Create;
end;

destructor TPackageInstaller.Destroy;
begin
  FToInstall.Free;
  inherited Destroy;
end;

function TPackageInstaller.OpenPackage(const APackageFileName: String): TIDEPackage;
var
  I: Integer;
begin
  Result := nil;
  if PackageEditingInterface.DoOpenPackageFile(APackageFileName, [pofRevert, pofDoNotOpenEditor], True) = mrOk then
  begin
    for I := 0 to PackageEditingInterface.GetPackageCount - 1 do
    begin
      if UpperCase(PackageEditingInterface.GetPackages(I).Filename) = UpperCase(APackageFileName) then
      begin
        Result := PackageEditingInterface.GetPackages(I);
        Break;
      end;
    end;
  end;
end;

function TPackageInstaller.IsPackageInTheList(const APackageFileName: String): Boolean;
var
  P, I: Integer;
  PackageFileName: String;
begin
  Result := False;
  for I := 0 to FToInstall.Count - 1 do
  begin
    P := Pos('  ', FToInstall.Strings[I]);
    if P <> 0 then
      PackageFileName := Copy(FToInstall.Strings[I], 1, P - 1)
    else
      PackageFileName := FToInstall.Strings[I];
    if UpperCase(PackageFileName) = UpperCase(APackageFileName) then
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
    FPackageFileName := Copy(AName, 1, P - 1);
    Delete(AName, 1, P);
    FUnresolvedPackageFileName := Trim(AName);
    Result := True;
  end
  else
    FPackageFileName := AName;
end;

function TPackageInstaller.CompilePackage(const AIDEPackage: TIDEPackage;
  APackageFile: TPackageFile): Integer;
begin
  Result := -1;
  {$if declared(lcl_version)}
   {$if (lcl_major > 0) and (lcl_minor > 6)}
     //DoCompilePackage function is only available with Laz 1.7 +
     DoOnPackageInstallProgress(imCompilePackage, APackageFile);
     Result := PackageEditingInterface.DoCompilePackage(AIDEPackage, [pcfCleanCompile, pcfDoNotSaveEditorFiles], False);
   {$endif}
  {$endif}
end;

function TPackageInstaller.InstallPackage: Boolean;
var
  P: Integer;
  PackageName: String;
  NewPackageID: TLazPackageID;
begin
  Result := False;
  P := Pos('.lpk', FPackageFileName);
  if P <> 0 then
  begin
    PackageName := Copy(FPackageFileName, 1, P - 1);
    NewPackageID := TLazPackageID.Create;
    if NewPackageID.StringToID(PackageName) then
    begin
      InstallPackageList.Add(NewPackageID);
      Result := True;
    end;
  end;
end;

procedure TPackageInstaller.DoOnPackageInstallProgress(const AInstallMessage: TInstallMessage;
  APackageFile: TPackageFile);
begin
  if AInstallMessage = imPackageCompleted then
     APackageFile.PackageStates := APackageFile.PackageStates + [psInstalled];
  if Assigned(FOnPackageInstallProgress) then
    FOnPackageInstallProgress(Self, FCnt, FTotCnt, FPackageFileName, AInstallMessage);
  if AInstallMessage <> imPackageCompleted then
    Sleep(1000);
end;

procedure TPackageInstaller.DoOnPackageInstallError(const AInstallMessage: TInstallMessage;
  APackageFile: TPackageFile);
var
  ErrMsg: String;
begin
  case AInstallMessage of
    imOpenPackageError:
      ErrMsg := rsInstallErrorOpenPackage;
    imCompilePackageError:
      ErrMsg := rsInstallErrorCompilePackge;
    imInstallPackageError:
      ErrMsg := rsInstallErrorInstallPackage;
    imDependencyError:
      ErrMsg := rsInstallErrorDependency0 + ' "' + FUnresolvedPackageFileName + '" ' + rsInstallErrorDependency1;
    end;
  APackageFile.PackageStates := APackageFile.PackageStates - [psInstalled];
  APackageFile.PackageStates := APackageFile.PackageStates + [psError];
  if Assigned(FOnPackageInstallError) then
    FOnPackageInstallError(Self, FPackageFileName, ErrMsg);
  Sleep(1000);
end;

procedure TPackageInstaller.Execute;
var
  I: Integer;
  IDEPackage: TIDEPackage;
  PkgInstallInIDEFlags: TPkgInstallInIDEFlags;
  ErrCnt: Integer;
  PackageFile: TPackageFile;
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
    PackageFile := SerializablePackages.FindPackageFile(FPackageFileName);
    if CanGo then
    begin
      Inc(FCnt);
      if not FNeedToRebuild then
        FNeedToRebuild := PackageFile.PackageType in [ptRunAndDesignTime, ptDesigntime];
      DoOnPackageInstallProgress(imOpenPackage, PackageFile);
      IDEPackage := OpenPackage(PackageFile.PackageAbsolutePath);
      if IDEPackage <> nil then
      begin
        DoOnPackageInstallProgress(imOpenPackageSuccess, PackageFile);
        CompRes := CompilePackage(IDEPAckage, PackageFile);
        case CompRes of
          -1, 1:
            begin
              if CompRes = 1 then
                DoOnPackageInstallProgress(imCompilePackageSuccess, PackageFile);
              if PackageFile.PackageType in [ptRunAndDesignTime, ptDesigntime] then
              begin
                DoOnPackageInstallProgress(imInstallPackage, PackageFile);
                if InstallPackage then
                  DoOnPackageInstallProgress(imInstallpackageSuccess, PackageFile)
                else
                begin
                  Inc(ErrCnt);
                  DoOnPackageInstallError(imInstallPackageError, PackageFile);
                end;
              end;
            end;
          else
            begin
              Inc(ErrCnt);
              DoOnPackageInstallError(imCompilePackageError, PackageFile);
            end;
        end;
      end
      else
      begin
        Inc(ErrCnt);
        DoOnPackageInstallError(imOpenPackageError, PackageFile);
      end;
      DoOnPackageInstallProgress(imPackageCompleted, PackageFile);
    end
    else
    begin
      Inc(FCnt);
      DoOnPackageInstallProgress(imOpenPackage, PackageFile);
      DoOnPackageInstallProgress(imOpenPackageSuccess, PackageFile);
      Inc(ErrCnt);
      DoOnPackageInstallError(imDependencyError, PackageFile);
      DoOnPackageInstallProgress(imPackageCompleted, PackageFile);
    end;
  end;
  if (InstallPackageList.Count > 0) and (not FNeedToBreak) then
  begin
    PkgInstallInIDEFlags := [piiifQuiet];
    if PackageEditingInterface.InstallPackages(InstallPackageList, PkgInstallInIDEFlags) = mrOk then
    begin
      if ErrCnt = 0 then
        FInstallStatus := isSuccess
      else
        FInstallStatus := isPartiallyFailed;
    end;
  end;
  if Assigned(FOnPackageInstallCompleted) then
    FOnPackageInstallCompleted(Self, FNeedToRebuild, FInstallStatus);
end;

procedure TPackageInstaller.StartInstall;
var
  I, J, K: Integer;
  PackageList: TObjectList;
  PackageFile, DependecyPackage: TPackageFile;
  PackageDependency: TPackageDependency;
  DependencyFound: Boolean;
begin
  if FStarted then
    Exit;
  FStarted := True;
  FTotCnt := 0;
  PackageList := TObjectList.Create(True);
  try
    for I := 0 to SerializablePackages.Count - 1 do
    begin
      for J := 0 to SerializablePackages.Items[I].PackageFiles.Count - 1 do
      begin
        PackageFile := TPackageFile(SerializablePackages.Items[I].PackageFiles.Items[J]);
        if PackageFile.IsInstallable then
        begin
          SerializablePackages.GetPackageDependencies(PackageFile.Name, PackageList, True, True);
          if PackageList.Count > 0 then
          begin
            DependencyFound := True;
            for K := 0 to PackageList.Count - 1 do
            begin
              PackageDependency := TPackageDependency(PackageList.Items[K]);
              DependecyPackage := SerializablePackages.FindPackageFile(TPackageDependency(PackageList.Items[k]).PackageFileName + '.lpk');
              if DependecyPackage <> nil then
              begin
                if not ((DependecyPackage.PackageState = psInstalled) and (SerializablePackages.IsInstalledVersionOk(PackageDependency, DependecyPackage.VersionAsString))) then
                begin
                  if ((DependecyPackage.IsInstallable) and (SerializablePackages.IsDependencyOk(PackageDependency, DependecyPackage))) then
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
             if (not IsPackageInTheList(PackageFile.Name)) then
               FToInstall.Add(PackageFile.Name + '  ' + DependecyPackage.Name)
           end
         end;
         if (not IsPackageInTheList(PackageFile.Name)) then
            FToInstall.Add(PackageFile.Name);
        end;
      end;
    end;
  finally
    PackageList.Free;
  end;
  FTotCnt := FToInstall.Count;
  Execute;
end;

procedure TPackageInstaller.StopInstall;
begin
  FNeedToBreak := True;
  FStarted := False;
end;

end.

