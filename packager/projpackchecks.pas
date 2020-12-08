unit ProjPackChecks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, Forms, Dialogs,
  // LazUtils
  FileUtil, LazFileUtils, UITypes,
  // Codetools
  CodeToolManager, CodeCache,
  // BuildIntf
  PackageIntf, PackageDependencyIntf,
  // IDEIntf
  ComponentReg, IDEDialogs,
  // IDE
  LazarusIDEStrConsts, IDEDefs, Project, PackageSystem, PackageDefs, ProjPackCommon;

type

  TProjPackFileCheck = class
  protected
    class function UnitNameOk(const AFilename, AUnitFilename: string): TModalResult;
  public
  end;

  { TPkgFileCheck }

  TPkgFileCheck = class(TProjPackFileCheck)
  private
    class function NormalizeFN(LazPackage: TLazPackage; var AFilename: string): TModalResult;
    class function FileExistsOk(LazPackage: TLazPackage; const AFilename: string): TModalResult;
    class function PartOfProjectOk(const AFilename: string;
      OnGetIDEFileInfo: TGetIDEFileStateEvent): TModalResult;
    class function UniqueUnitOk(LazPackage: TLazPackage;
      const AUnitFilename: string): TModalResult;
  public
    class function ReadOnlyOk(LazPackage: TLazPackage): TModalResult;
    class function AddingUnit(LazPackage: TLazPackage; const AFilename: string;
      OnGetIDEFileInfo: TGetIDEFileStateEvent): TModalResult;
    class function ReAddingUnit(LazPackage: TLazPackage;
      FileTyp: TPkgFileType; const AFilename: string;
      OnGetIDEFileInfo: TGetIDEFileStateEvent): TModalResult;
    class function AddingDependency(LazPackage: TLazPackage;
      NewDependency: TPkgDependency; WarnIfAlreadyThere: boolean): TModalResult;
  end;

  { TPrjFileCheck }

  TPrjFileCheck = class(TProjPackFileCheck)
  private
  public
    class function AddingFile(AProject: TProject; const AFilename: string): TModalResult;
    class function AddingDependency(AProject: TProject; NewDependency: TPkgDependency): TModalResult;
  end;

// Project or Package using the common interface
function CheckAddingDependency(AProjPack: IProjPack; ADependency: TPkgDependency): boolean;


implementation

// Package or Project:

function CheckAddingDependency(AProjPack: IProjPack; ADependency: TPkgDependency): boolean;
// ToDo: Try to combine CheckAddingPackageDependency and CheckAddingProjectDependency
//  somehow to use IProjPack param.
begin
  //Assert((AProjPack is TLazPackage) or (AProjPack is TProject),
  //       'CheckAddingDependency: AProjPack is neither a project nor a package.');
  if AProjPack is TLazPackage then
    Result := TPkgFileCheck.AddingDependency(AProjPack as TLazPackage, ADependency, True) = mrOK
  else
    Result := TPrjFileCheck.AddingDependency(AProjPack as TProject, ADependency) = mrOK;
end;

{ TProjPackFileCheck }

class function TProjPackFileCheck.UnitNameOk(const AFilename, AUnitFilename: string): TModalResult;
// This is called only for Pascal units.
var
  Unit_Name: string;
  CodeBuffer: TCodeBuffer;
begin
  Result:=mrCancel;
  // valid unitname
  Unit_Name:='';
  CodeBuffer:=CodeToolBoss.LoadFile(AFilename,true,false);
  if CodeBuffer<>nil then
    Unit_Name:=CodeToolBoss.GetSourceName(CodeBuffer,false);
  Assert(Unit_Name<>'', 'TProjPackFileCheck.UnitNameOk: Unit_Name is empty.');
  if CompareText(Unit_Name, AUnitFilename)<>0 then
    if IDEMessageDialog(lisA2PInvalidUnitName,
      Format(lisA2PTheUnitNameAndFilenameDiffer,[Unit_Name,LineEnding,AUnitFilename]),
      mtError,[mbIgnore,mbCancel]) <> mrIgnore
    then exit;

  if not IsValidUnitName(AUnitFilename) then
  begin
    IDEMessageDialog(lisA2PFileNotUnit, Format(lisA2PisNotAValidUnitName,[AUnitFilename]),
                     mtWarning,[mbCancel]);
    exit;
  end;
  // Pascal extension
  Assert(FilenameIsPascalUnit(AFilename), 'TPkgFileCheck.UnitNameOk: Wrong extension.');
  Result:=mrOK;
end;

{ TPkgFileCheck }

class function TPkgFileCheck.NormalizeFN(LazPackage: TLazPackage;
  var AFilename: string): TModalResult;
begin
  Result:=mrCancel;
  // normalize filename
  if FilenameIsAbsolute(AFilename) then exit;
  if LazPackage.HasDirectory then
    AFilename:=TrimFilename(LazPackage.Directory+AFilename)
  else begin
    IDEMessageDialog(lisA2PInvalidFilename,
      Format(lisA2PTheFilenameIsAmbiguousPleaseSpecifiyAFilename,[AFilename,LineEnding]),
      mtError,[mbCancel]);
    exit;
  end;
  Result:=mrOK;
end;

class function TPkgFileCheck.FileExistsOk(LazPackage: TLazPackage;
  const AFilename: string): TModalResult;
var
  PkgFile: TPkgFile;
  Msg: String;
begin
  Result:=mrCancel;
  // check if file exists
  if not FileExistsUTF8(AFilename) then
  begin
    IDEMessageDialog(lisFileNotFound, Format(lisPkgMangFileNotFound,[AFilename]),
                     mtError, [mbCancel]);
    exit;
  end;
  // check if file already exists in package
  PkgFile:=LazPackage.FindPkgFile(AFilename,true,false);
  if PkgFile<>nil then
  begin
    Msg:=Format(lisA2PFileAlreadyExistsInThePackage, [AFilename]);
    if PkgFile.Filename<>AFilename then
      Msg:=Msg+LineEnding+Format(lisA2PExistingFile2, [PkgFile.Filename]);
    IDEMessageDialog(lisA2PFileAlreadyExists, Msg, mtError, [mbCancel]);
    exit;
  end;
  Result:=mrOK;
end;

class function TPkgFileCheck.PartOfProjectOk(const AFilename: string;
  OnGetIDEFileInfo: TGetIDEFileStateEvent): TModalResult;
var
  IDEFileFlags: TIDEFileStateFlags;
begin
  Assert(Assigned(OnGetIDEFileInfo), 'TPkgFileCheck.PartOfProjectOk: OnGetIDEFileInfo=Nil.');
  IDEFileFlags:=[];
  OnGetIDEFileInfo(nil,AFilename,[ifsPartOfProject],IDEFileFlags);
  if ifsPartOfProject in IDEFileFlags then
  begin
    IDEMessageDialog(lisA2PFileIsUsed,
      Format(lisA2PTheFileIsPartOfTheCurrentProjectItIsABadIdea,[AFilename,LineEnding]),
      mtError,[mbCancel]);
    exit(mrCancel);
  end;
  Result:=mrOK;
end;

class function TPkgFileCheck.UniqueUnitOk(LazPackage: TLazPackage;
  const AUnitFilename: string): TModalResult;
// This is called only for Pascal units.
var
  PkgFile: TPkgFile;
begin
  Result:=mrCancel;
  // check if unitname already exists in package
  PkgFile:=LazPackage.FindUnit(AUnitFilename,true);
  if PkgFile<>nil then
  begin
    // a unit with this name already exists in this package => warn
    if IDEMessageDialog(lisA2PUnitnameAlreadyExists,
              Format(lisA2PTheUnitnameAlreadyExistsInThisPackage,[AUnitFilename]),
              mtError,[mbCancel,mbIgnore]) <> mrIgnore then
    exit;
  end
  else begin
    PkgFile:=PackageGraph.FindUnit(LazPackage,AUnitFilename,true,true);
    if (PkgFile<>nil) and (PkgFile.LazPackage<>LazPackage) then
    begin
      // there is already a unit with this name in another package => warn
      if IDEMessageDialog(lisA2PUnitnameAlreadyExists,
                Format(lisA2PTheUnitnameAlreadyExistsInThePackage,
                       [AUnitFilename, LineEnding, PkgFile.LazPackage.IDAsString]),
                mtWarning,[mbCancel,mbIgnore]) <> mrIgnore then
        exit;
    end;
  end;
  // check if unitname is a componentclass
  if IDEComponentPalette.FindRegComponent(AUnitFilename)<>nil then
  begin
    if IDEMessageDialog(lisA2PAmbiguousUnitName,
        Format(lisA2PTheUnitNameIsTheSameAsAnRegisteredComponent,[AUnitFilename,LineEnding]),
        mtWarning,[mbCancel,mbIgnore]) <> mrIgnore then
    exit;
  end;
  Result:=mrOK;
end;

class function TPkgFileCheck.ReadOnlyOk(LazPackage: TLazPackage): TModalResult;
begin
  // check if package is readonly
  if LazPackage.ReadOnly then
  begin
    IDEMessageDialog(lisAF2PPackageIsReadOnly,
      Format(lisAF2PThePackageIsReadOnly, [LazPackage.IDAsString]),
      mtError,[mbCancel]);
    exit(mrCancel);
  end;
  Result:=mrOK;
end;

class function TPkgFileCheck.AddingUnit(LazPackage: TLazPackage;
  const AFilename: string; OnGetIDEFileInfo: TGetIDEFileStateEvent): TModalResult;
var
  NewFileType: TPkgFileType;
  UnitFilename: String;
begin
  Assert(FilenameIsAbsolute(AFilename), 'TPkgFileCheck.AddingUnit: Not absolute Filename.');
  // file exists
  Result:=FileExistsOk(LazPackage, AFilename);
  if Result<>mrOK then exit;
  // file is part of project
  Result:=PartOfProjectOk(AFilename, OnGetIDEFileInfo);
  if Result<>mrOK then exit;

  NewFileType:=FileNameToPkgFileType(AFilename);
  if NewFileType<>pftUnit then
    exit(mrOK);                         // Further checks only for Pascal units.
  UnitFilename:=ExtractFileNameOnly(AFilename);
  // unitname
  Result:=UnitNameOk(AFilename, UnitFilename);
  if Result<>mrOK then exit;
  // unit is unique
  Result:=UniqueUnitOk(LazPackage, UnitFilename);
  if Result<>mrOK then exit;
  Result:=mrOK;  // ok
end;

class function TPkgFileCheck.ReAddingUnit(LazPackage: TLazPackage;
  FileTyp: TPkgFileType; const AFilename: string;
  OnGetIDEFileInfo: TGetIDEFileStateEvent): TModalResult;
var
  UnitFilename: String;
begin
  Assert(FilenameIsAbsolute(AFilename), 'TPkgFileCheck.ReAddingUnit: Not absolute Filename.');
  // file exists
  Result:=FileExistsOk(LazPackage, AFilename);
  if Result<>mrOK then exit;
  // file is part of project
  Result:=PartOfProjectOk(AFilename, OnGetIDEFileInfo);
  if Result<>mrOK then exit;
  if not (FileTyp in [pftUnit, pftMainUnit, pftVirtualUnit]) then
    exit(mrOK);                         // Further checks only for Pascal units.
  UnitFilename:=ExtractFileNameOnly(AFilename);
  // unitname
  Result:=UnitNameOk(AFilename, UnitFilename);
  if Result<>mrOK then exit;
  // unit is unique
  Result:=UniqueUnitOk(LazPackage, UnitFilename);
  if Result<>mrOK then exit;
  Result:=mrOK;  // ok
end;

class function TPkgFileCheck.AddingDependency(LazPackage: TLazPackage;
  NewDependency: TPkgDependency; WarnIfAlreadyThere: boolean): TModalResult;
// Returns mrOk=can be added, mrCancel=do not add, mrIgnore=already there
var
  NewPkgName, s: String;
  RequiredPackage, ProvidingAPackage: TLazPackage;
  ConflictDependency: TPkgDependency;
  PathList: TFPList;
begin
  Result:=mrCancel;
  DebugLn(['CheckAddingPackageDependency: ', LazPackage.Name]);
  NewPkgName:=NewDependency.PackageName;

  // check Max-Min version
  if (pdfMinVersion in NewDependency.Flags)
  and (pdfMaxVersion in NewDependency.Flags)
  and (NewDependency.MaxVersion.Compare(NewDependency.MinVersion)<0) then
  begin
    IDEMessageDialog(lisProjAddInvalidMinMaxVersion,
      lisA2PTheMaximumVersionIsLowerThanTheMinimimVersion,
      mtError,[mbCancel]);
    exit(mrCancel);
  end;

  // package name is checked earlier
  Assert(IsValidPkgName(NewPkgName), 'CheckAddingPackageDependency: '+NewPkgName+' is not valid.');

  // check if package is already required
  if (CompareText(NewPkgName,LazPackage.Name)=0)
  or (PackageGraph.FindDependencyRecursively(
        LazPackage.FirstRequiredDependency,NewPkgName)<>nil)
  then begin
    if WarnIfAlreadyThere then
      IDEMessageDialog(lisProjAddDependencyAlreadyExists,
        Format(lisA2PThePackageHasAlreadyADependencyForThe, [NewPkgName]),
        mtError,[mbCancel]);
    exit(mrIgnore);
  end;

  // check if required lpk exists
  if not PackageGraph.DependencyExists(NewDependency,fpfSearchAllExisting)
  then begin
    IDEMessageDialog(lisProjAddPackageNotFound,
      Format(lisA2PNoPackageFoundForDependencyPleaseChooseAnExisting,
             [NewDependency.AsString, LineEnding]),
      mtError,[mbCancel]);
    exit(mrCancel);
  end;

  RequiredPackage:=PackageGraph.FindPackageWithName(NewPkgName,nil);
  if RequiredPackage<>nil then
  begin
    // check if there is a dependency, that requires another version
    ConflictDependency:=PackageGraph.FindConflictRecursively(
                            LazPackage.FirstRequiredDependency, RequiredPackage);
    if ConflictDependency<>nil then
    begin
      DebugLn(['CheckAddingPackageDependency ',LazPackage.Name,' requiring ',RequiredPackage.IDAsString,' conflicts with ',ConflictDependency.AsString]);
      IDEMessageDialog(lisVersionMismatch,
        Format(lisUnableToAddTheDependencyBecauseThePackageHasAlread,
          [RequiredPackage.IDAsString, LazPackage.Name, ConflictDependency.AsString]),
        mtError,[mbCancel]);
      exit(mrCancel);
    end;

    // check if there is a cycle
    PathList:=PackageGraph.FindPath(RequiredPackage,nil,LazPackage.Name);
    if PathList<>nil then
      try
        s:=PackagePathToStr(PathList);
        IDEMessageDialog(lisCircularDependencyDetected,
              Format(lisUnableToAddTheDependencyBecauseThisWouldCreateA,
                     [RequiredPackage.IDAsString, s]),
              mtError,[mbCancel]);
        exit(mrCancel);
      finally
        PathList.Free;
      end;
  end;

  ProvidingAPackage:=PackageGraph.FindPackageProvidingName(
                                   LazPackage.FirstRequiredDependency,NewPkgName);
  if ProvidingAPackage<>nil then
  begin
    // package is already provided by another package
    if WarnIfAlreadyThere then
      IDEMessageDialog(lisProjAddDependencyAlreadyExists,
        Format(lisUnableToAddTheDependencyBecauseThePackageHasAlread, [
          RequiredPackage.IDAsString, LazPackage.Name, ProvidingAPackage.Name]),
        mtError,[mbCancel]);
    exit(mrIgnore);
  end;

  Result:=mrOk;
end;

{ TPrjFileCheck }

class function TPrjFileCheck.AddingFile(AProject: TProject; const AFilename: string): TModalResult;
// Returns mrOk=can be added, mrCancel=do not add, mrIgnore=already there
var
  NewFile: TUnitInfo;
  UnitFileName: String;
  ConflictFile: TUnitInfo;
begin
  Result:=mrCancel;
  // check if file is already part of project
  NewFile:=AProject.UnitInfoWithFilename(AFilename);
  if (NewFile<>nil) and NewFile.IsPartOfProject then
    exit(mrIgnore);
  // check unit name
  if FilenameIsPascalUnit(AFilename) then
  begin
    UnitFilename:=ExtractFileNameOnly(AFilename);
    Result:=UnitNameOk(AFilename, UnitFilename);
    if Result<>mrOK then exit;
    // check if unitname already exists in project
    ConflictFile:=AProject.UnitWithUnitname(UnitFileName);
    if ConflictFile<>nil then begin
      if IDEMessageDialog(lisProjAddUnitNameAlreadyExists,
            Format(lisProjAddTheUnitNameAlreadyExistsInTheProject,
                   [UnitFileName, LineEnding, ConflictFile.Filename]),
            mtWarning, [mbCancel, mbIgnore]) <> mrIgnore
      then exit;
    end;
  end;
  Result:=mrOk;
end;

class function TPrjFileCheck.AddingDependency(AProject: TProject;
  NewDependency: TPkgDependency): TModalResult;
var
  NewPkgName: String;
begin
  NewPkgName:=NewDependency.PackageName;
  // check Max-Min version
  if (pdfMinVersion in NewDependency.Flags)
  and (pdfMaxVersion in NewDependency.Flags)
  and (NewDependency.MaxVersion.Compare(NewDependency.MinVersion)<0) then
  begin
    IDEMessageDialog(lisProjAddInvalidMinMaxVersion,
      lisProjAddTheMaximumVersionIsLowerThanTheMinimimVersion,
      mtError,[mbCancel]);
    exit(mrCancel);
  end;
  // package name is checked earlier
  Assert(IsValidPkgName(NewPkgName), 'CheckAddingProjectDependency: ' + NewPkgName + ' is not valid.');
  // check if package is already required
  if AProject.FindDependencyByName(NewPkgName)<>nil then begin
    IDEMessageDialog(lisProjAddDependencyAlreadyExists,
      Format(lisProjAddTheProjectHasAlreadyADependency, [NewPkgName]),
      mtError,[mbCancel]);
    exit(mrCancel);
  end;
  // check if required package exists
  if not PackageGraph.DependencyExists(NewDependency,fpfSearchAllExisting)
  then begin
    IDEMessageDialog(lisProjAddPackageNotFound,
      Format(lisProjAddTheDependencyWasNotFound,[NewDependency.AsString, LineEnding]),
      mtError,[mbCancel]);
    exit(mrCancel);
  end;
  Result:=mrOK;
end;

end.

