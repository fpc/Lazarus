unit ProjPackChecks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  LCLProc, Forms, Controls, Dialogs,
  // LazUtils
  FileUtil, LazFileUtils,
  // IDEIntf
  PackageDependencyIntf, ComponentReg, IDEDialogs,
  // IDE
  LazarusIDEStrConsts, IDEDefs, Project, PackageSystem, PackageDefs, ProjPackCommon;

// Packages:
type
  TAddToPkgType = (
    d2ptUnit,
    d2ptVirtualUnit,
    d2ptNewComponent,
    d2ptFile
    );

function CheckAddingPackageUnit(LazPackage: TLazPackage;
  AddFileType: TAddToPkgType; OnGetIDEFileInfo: TGetIDEFileStateEvent;
  var AFilename: string): boolean;

function CheckAddingPackageDependency(LazPackage: TLazPackage;
  NewDependency: TPkgDependency; Quiet, WarnIfAlreadyThere: boolean
  ): TModalResult;// mrOk=can be added, mrCancel=do not add, mrIgnore=already there

// Projects:

function CheckAddingProjectFile(AProject: TProject; NewFiles: TStrings;
  var NewFilename: string): TModalResult;

function CheckAddingProjectDependency(AProject: TProject;
  NewDependency: TPkgDependency): boolean;

// Project or Package using the common interface

function CheckAddingDependency(AProjPack: IProjPack; ADependency: TPkgDependency): boolean;


implementation

// Packages:

function CheckAddingPackageUnit(LazPackage: TLazPackage;
  AddFileType: TAddToPkgType; OnGetIDEFileInfo: TGetIDEFileStateEvent;
  var AFilename: string): boolean;
var
  AnUnitName: String;
  PkgFile: TPkgFile;
  Msg: String;
  IDEFileFlags: TIDEFileStateFlags;
begin
  Result:=false;
  PkgFile:=Nil;

  // check if package is readonly
  if LazPackage.ReadOnly then begin
    IDEMessageDialog(lisAF2PPackageIsReadOnly,
      Format(lisAF2PThePackageIsReadOnly, [LazPackage.IDAsString]),
      mtError,[mbCancel]);
    exit;
  end;

  // normalize filename
  AFilename:=TrimFilename(AFilename);
  if (AddFileType<>d2ptVirtualUnit) and (not FilenameIsAbsolute(AFilename)) then
  begin
    if LazPackage.HasDirectory then
      AFilename:=LazPackage.Directory+AFilename
    else begin
      IDEMessageDialog(lisA2PInvalidFilename,
        Format(lisA2PTheFilenameIsAmbiguousPleaseSpecifiyAFilename,[AFilename,LineEnding]),
        mtError,[mbCancel]);
      exit;
    end;
  end;

  // check if file exists
  if (FilenameIsAbsolute(AFilename)) then begin
    if (not FileExistsUTF8(AFilename)) then begin
      if AddFileType=d2ptUnit then begin
        IDEMessageDialog(lisFileNotFound,
          Format(lisPkgMangFileNotFound, [AFilename]),
          mtError, [mbCancel]);
        exit;
      end;
    end;
  end;

  // check if file already exists in package
  if FilenameIsAbsolute(AFilename) then begin
    PkgFile:=LazPackage.FindPkgFile(AFilename,true,false);
    if PkgFile<>nil then begin
      Msg:=Format(lisA2PFileAlreadyExistsInThePackage, [AFilename]);
      if PkgFile.Filename<>AFilename then
        Msg:=Msg+LineEnding+Format(lisA2PExistingFile2, [PkgFile.Filename]);
      IDEMessageDialog(lisA2PFileAlreadyExists, Msg, mtError, [mbCancel]);
      exit;
    end;
  end;

  // check if file is part of project
  if FilenameIsAbsolute(AFilename) then begin
    if Assigned(OnGetIDEFileInfo) then begin
      IDEFileFlags:=[];
      OnGetIDEFileInfo(nil,AFilename,[ifsPartOfProject],IDEFileFlags);
      if (ifsPartOfProject in IDEFileFlags) then begin
        IDEMessageDialog(lisA2PFileIsUsed,
          Format(lisA2PTheFileIsPartOfTheCurrentProjectItIsABadIdea,[AFilename,LineEnding]),
          mtError,[mbCancel]);
        exit;
      end;
    end;
  end;

  // check file extension
  if AddFileType in [d2ptUnit,d2ptNewComponent,d2ptVirtualUnit] then begin
    if not FilenameIsPascalUnit(AFilename) then begin
      IDEMessageDialog(lisA2PFileNotUnit,
        lisA2PPascalUnitsMustHaveTheExtensionPPOrPas,
        mtWarning,[mbCancel]);
      exit;
    end;
  end;

  // check unitname
  if AddFileType in [d2ptUnit,d2ptNewComponent,d2ptVirtualUnit] then begin
    AnUnitName:=ExtractFileNameOnly(AFilename);
    if not IsValidUnitName(AnUnitName) then begin
      IDEMessageDialog(lisA2PFileNotUnit,
        Format(lisA2PisNotAValidUnitName, [AnUnitName]),
        mtWarning,[mbCancel]);
      exit;
    end;

    // check if unitname already exists in package
    PkgFile:=LazPackage.FindUnit(AnUnitName,true,PkgFile);
    if PkgFile<>nil then begin
      // a unit with this name already exists in this package => warn
      if IDEMessageDialog(lisA2PUnitnameAlreadyExists,
        Format(lisA2PTheUnitnameAlreadyExistsInThisPackage, [AnUnitName]),
        mtError,[mbCancel,mbIgnore])<>mrIgnore then exit;
    end else begin
      PkgFile:=PackageGraph.FindUnit(LazPackage,AnUnitName,true,true);
      if (PkgFile<>nil) and (PkgFile.LazPackage<>LazPackage) then begin
        // there is already a unit with this name in another package => warn
        if IDEMessageDialog(lisA2PUnitnameAlreadyExists,
          Format(lisA2PTheUnitnameAlreadyExistsInThePackage,
                 [AnUnitName, LineEnding, PkgFile.LazPackage.IDAsString]),
          mtWarning,[mbCancel,mbIgnore])<>mrIgnore then exit;
      end;
    end;

    // check if unitname is a componentclass
    if IDEComponentPalette.FindComponent(AnUnitName)<>nil then begin
      if IDEMessageDialog(lisA2PAmbiguousUnitName,
        Format(lisA2PTheUnitNameIsTheSameAsAnRegisteredComponent,
               [AnUnitName, LineEnding]),
        mtWarning,[mbCancel,mbIgnore])<>mrIgnore
      then
        exit;
    end;
  end;

  // ok
  Result:=true;
end;

function CheckAddingPackageDependency(LazPackage: TLazPackage;
  NewDependency: TPkgDependency; Quiet, WarnIfAlreadyThere: boolean): TModalResult;
var
  NewPkgName: String;
  RequiredPackage: TLazPackage;
  ProvidingAPackage: TLazPackage;
  ConflictDependency: TPkgDependency;
  PathList: TFPList;
  s: String;
begin
  Result:=mrCancel;
  DebugLn(['CheckAddingPackageDependency: ', LazPackage.Name]);
  NewPkgName:=NewDependency.PackageName;

  // check Max-Min version
  if (pdfMinVersion in NewDependency.Flags)
  and (pdfMaxVersion in NewDependency.Flags)
  and (NewDependency.MaxVersion.Compare(NewDependency.MinVersion)<0) then
  begin
    if not Quiet then
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
    if not Quiet then
      IDEMessageDialog(lisProjAddPackageNotFound,
        Format(lisA2PNoPackageFoundForDependencyPleaseChooseAnExisting,
               [NewDependency.AsString, LineEnding]),
        mtError,[mbCancel]);
    exit(mrCancel);
  end;

  RequiredPackage:=PackageGraph.FindPackageWithName(NewPkgName,nil);
  if RequiredPackage<>nil then begin
    // check if there is a dependency, that requires another version
    ConflictDependency:=PackageGraph.FindConflictRecursively(
      LazPackage.FirstRequiredDependency,RequiredPackage);
    if ConflictDependency<>nil then begin
      DebugLn(['CheckAddingPackageDependency ',LazPackage.Name,' requiring ',RequiredPackage.IDAsString,' conflicts with ',ConflictDependency.AsString]);
      if not Quiet then
        IDEMessageDialog(lisVersionMismatch,
          Format(lisUnableToAddTheDependencyBecauseThePackageHasAlread, [
            RequiredPackage.IDAsString, LazPackage.Name, ConflictDependency.
            AsString]),
          mtError,[mbCancel]);
      exit(mrCancel);
    end;

    // check if there is a cycle
    PathList:=PackageGraph.FindPath(RequiredPackage,nil,LazPackage.Name);
    if PathList<>nil then begin
      try
        s:=PackagePathToStr(PathList);
        DebugLn(['CheckAddingPackageDependency ',LazPackage.Name,' requiring ',RequiredPackage.IDAsString,' creates cycles with ',s]);
        if not Quiet then
          IDEMessageDialog(lisCircularDependencyDetected,
            Format(lisUnableToAddTheDependencyBecauseThisWouldCreateA, [
              RequiredPackage.IDAsString, s]),
            mtError,[mbCancel]);
        exit(mrCancel);
      finally
        PathList.Free;
      end;
    end;
  end;

  ProvidingAPackage:=PackageGraph.FindPackageProvidingName(
    LazPackage.FirstRequiredDependency,NewPkgName);
  if ProvidingAPackage<>nil then
  begin
    // package is already provided by another package
    DebugLn(['CheckAddingPackageDependency ',LazPackage.Name,' requiring ',NewPkgName,', but is already provided by ',ProvidingAPackage.IDAsString]);
    if WarnIfAlreadyThere then
      IDEMessageDialog(lisProjAddDependencyAlreadyExists,
        Format(lisUnableToAddTheDependencyBecauseThePackageHasAlread, [
          RequiredPackage.IDAsString, LazPackage.Name, ProvidingAPackage.Name]),
        mtError,[mbCancel]);
    exit(mrIgnore);
  end;

  Result:=mrOk;
end;

// Project:

function CheckAddingProjectFile(AProject: TProject; NewFiles: TStrings;
  var NewFilename: string): TModalResult;
var
  ConflictFile: TUnitInfo;
  OtherUnitName: String;
  OtherFile: string;
  j: Integer;
  NewFile: TUnitInfo;
  NewUnitName: String;
begin
  Result:=mrCancel;
  // expand filename
  if not FilenameIsAbsolute(NewFilename) then
    NewFilename:=TrimFilename(AProject.Directory+PathDelim+NewFilename);
  // check if file is already part of project
  NewFile:=AProject.UnitInfoWithFilename(NewFilename);
  if (NewFile<>nil) and NewFile.IsPartOfProject then begin
    Result:=mrIgnore;
    exit;
  end;
  // check unit name
  if FilenameIsPascalUnit(NewFilename) then begin
    // check unitname is valid pascal identifier
    NewUnitName:=ExtractFileNameOnly(NewFilename);
    if (NewUnitName='') or not (IsValidUnitName(NewUnitName)) then begin
      IDEMessageDialog(lisProjAddInvalidPascalUnitName,
        Format(lisProjAddTheUnitNameIsNotAValidPascalIdentifier, [NewUnitName]),
        mtWarning, [mbIgnore, mbCancel]);
      exit;
    end;
    // check if unitname already exists in project
    ConflictFile:=AProject.UnitWithUnitname(NewUnitName);
    if ConflictFile<>nil then begin
      IDEMessageDialog(lisProjAddUnitNameAlreadyExists,
        Format(lisProjAddTheUnitNameAlreadyExistsInTheProject,
               [NewUnitName, LineEnding, ConflictFile.Filename]),
        mtWarning, [mbCancel, mbIgnore]);
      exit;
    end;
    // check if unitname already exists in selection
    for j:=0 to NewFiles.Count-1 do begin
      OtherFile:=NewFiles[j];
      if FilenameIsPascalUnit(OtherFile) then begin
        OtherUnitName:=ExtractFileNameOnly(OtherFile);
        if CompareText(OtherUnitName, NewUnitName)=0 then begin
          IDEMessageDialog(lisProjAddUnitNameAlreadyExists,
            Format(lisProjAddTheUnitNameAlreadyExistsInTheSelection,
                   [NewUnitName, LineEnding, OtherFile]),
            mtWarning, [mbCancel]);
          exit;
        end;
      end;
    end;
  end;
  Result:=mrOk;
end;

function CheckAddingProjectDependency(AProject: TProject;
  NewDependency: TPkgDependency): boolean;
var
  NewPkgName: String;
begin
  Result:=false;

  NewPkgName:=NewDependency.PackageName;

  // check Max-Min version
  if (pdfMinVersion in NewDependency.Flags)
  and (pdfMaxVersion in NewDependency.Flags)
  and (NewDependency.MaxVersion.Compare(NewDependency.MinVersion)<0) then
  begin
    IDEMessageDialog(lisProjAddInvalidMinMaxVersion,
      lisProjAddTheMaximumVersionIsLowerThanTheMinimimVersion,
      mtError,[mbCancel]);
    exit;
  end;

  // package name is checked earlier
  Assert(IsValidPkgName(NewPkgName), 'CheckAddingProjectDependency: ' + NewPkgName + ' is not valid.');

  // check if package is already required
  if AProject.FindDependencyByName(NewPkgName)<>nil then begin
    IDEMessageDialog(lisProjAddDependencyAlreadyExists,
      Format(lisProjAddTheProjectHasAlreadyADependency, [NewPkgName]),
      mtError,[mbCancel]);
    exit;
  end;

  // check if required package exists
  if not PackageGraph.DependencyExists(NewDependency,fpfSearchAllExisting)
  then begin
    IDEMessageDialog(lisProjAddPackageNotFound,
      Format(lisProjAddTheDependencyWasNotFound,[NewDependency.AsString, LineEnding]),
      mtError,[mbCancel]);
    exit;
  end;

  Result:=true;
end;

// Package or Project:

function CheckAddingDependency(AProjPack: IProjPack; ADependency: TPkgDependency): boolean;
// ToDo: Try to combine CheckAddingPackageDependency and CheckAddingProjectDependency
//  somehow to use IProjPack param.
begin
  Assert((AProjPack is TLazPackage) or (AProjPack is TProject),
         'CheckAddingDependency: AProjPack is neither a project nor a package.');
  if AProjPack is TLazPackage then
    Result := CheckAddingPackageDependency(AProjPack as TLazPackage, ADependency, False, True) = mrOK
  else
    Result := CheckAddingProjectDependency(AProjPack as TProject, ADependency)
end;

end.

