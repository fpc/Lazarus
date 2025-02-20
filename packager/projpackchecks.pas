unit ProjPackChecks;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, System.UITypes,
  // LCL
  Forms, Dialogs,
  // LazUtils
  FileUtil, LazLoggerBase, LazFileUtils,
  // Codetools
  CodeToolManager, CodeCache, BasicCodeTools,
  // BuildIntf
  PackageIntf, PackageDependencyIntf, ComponentReg,
  // IDEIntf
  IDEDialogs,
  // IdeConfig
  ProjPackCommon,
  // IDE
  LazarusIDEStrConsts, IDEDefs, Project, PackageSystem, PackageDefs;

type

  { TYesToAllList }

  TYesToAllList = class(TStringList)
  public
    WarnFileNotFound: boolean;
    WarnAlreadyAdded: boolean;
    function Inc(Name: string): integer;
  end;

  { TProjPackFileCheck }

  TProjPackFileCheck = class
  protected
    class function UnitNameOk(const AFilename, AnUnitName: string): TModalResult;
  public
  end;

  { TPkgFileCheck }

  TPkgFileCheck = class(TProjPackFileCheck)
  private
    class function NormalizeFN(LazPackage: TLazPackage; var AFilename: string): TModalResult;
    class function FileExistsOk(LazPackage: TLazPackage; const AFilename: string; YesToAll: TYesToAllList): TModalResult;
    class function PartOfProjectOk(const AFilename: string;
      OnGetIDEFileInfo: TGetIDEFileStateEvent; YesToAll: TYesToAllList): TModalResult;
    class function UniqueUnitOk(LazPackage: TLazPackage;
      const AnUnitName: string; YesToAll: TYesToAllList): TModalResult;
  public
    class function ReadOnlyOk(LazPackage: TLazPackage): TModalResult;
    class function AddingFile(LazPackage: TLazPackage; const AFilename: string;
      OnGetIDEFileInfo: TGetIDEFileStateEvent; YesToAll: TYesToAllList): TModalResult; // ok=success, cancel=fail, abort=fail and stop multi add
    class function ReAddingFile(LazPackage: TLazPackage;
      FileTyp: TPkgFileType; const AFilename: string;
      OnGetIDEFileInfo: TGetIDEFileStateEvent; YesToAll: TYesToAllList): TModalResult;
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

{ TYesToAllList }

function TYesToAllList.Inc(Name: string): integer;
begin
  Result:=StrToIntDef(Values[Name],0)+1;
  Values[Name]:=IntToStr(Result);
end;

{ TProjPackFileCheck }

class function TProjPackFileCheck.UnitNameOk(const AFilename, AnUnitName: string): TModalResult;
// This is called only for Pascal units.
var
  SrcUnitName: string;
  CodeBuffer: TCodeBuffer;
begin
  Result:=mrCancel;
  // check valid unitname
  if not IsValidUnitName(AnUnitName) then
  begin
    IDEMessageDialog(lisA2PFileNotUnit, Format(lisA2PisNotAValidUnitName,[AnUnitName]),
                     mtWarning,[mbCancel]);
    exit;
  end;

  // check file and source match
  SrcUnitName:='';
  CodeBuffer:=CodeToolBoss.LoadFile(AFilename,true,false);
  if CodeBuffer<>nil then begin
    SrcUnitName:=CodeToolBoss.GetSourceName(CodeBuffer,false);
    // SrcUnitName can be empty if Codetools had problems parsing the source.
    if (SrcUnitName<>'')
    and (CompareText(RemoveAmpersands(SrcUnitName), AnUnitName)<>0)
    and (IDEMessageDialog(lisA2PInvalidUnitName,
                          Format(lisA2PTheUnitNameAndFilenameDiffer,
                                 [SrcUnitName,LineEnding,AnUnitName]),
                          mtError,[mbIgnore,mbCancel]) <> mrIgnore) then exit;
  end;

  // Pascal extension
  Assert(FilenameHasPascalExt(AFilename), 'TPkgFileCheck.UnitNameOk: Wrong extension.');
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
  const AFilename: string; YesToAll: TYesToAllList): TModalResult;
var
  PkgFile: TPkgFile;
  Msg: String;
begin
  Result:=mrCancel;
  // check if file exists
  if not FileExistsUTF8(AFilename) then
  begin
    if not YesToAll.WarnFileNotFound then exit(mrIgnore);
    if YesToAll.Inc('FileNotFound')<4 then
      IDEMessageDialog(lisFileNotFound, Format(lisPkgMangFileNotFound,[AFilename]),
                     mtError, [mbCancel]);
    exit;
  end;
  // check if file already exists in package
  PkgFile:=LazPackage.FindPkgFile(AFilename,true,false);
  if PkgFile<>nil then
  begin
    if not YesToAll.WarnAlreadyAdded then exit(mrIgnore);
    if YesToAll.Inc('FileAlreadyInPackage')>2 then exit;
    Msg:=Format(lisA2PFileAlreadyExistsInThePackage, [AFilename]);
    if PkgFile.Filename<>AFilename then
      Msg:=Msg+LineEnding+Format(lisA2PExistingFile2, [PkgFile.Filename]);
    IDEMessageDialog(lisA2PFileAlreadyExists, Msg, mtError, [mbCancel]);
    exit;
  end;
  Result:=mrOK;
end;

class function TPkgFileCheck.PartOfProjectOk(const AFilename: string;
  OnGetIDEFileInfo: TGetIDEFileStateEvent; YesToAll: TYesToAllList
  ): TModalResult;
var
  IDEFileFlags: TIDEFileStateFlags;
begin
  Assert(Assigned(OnGetIDEFileInfo), 'TPkgFileCheck.PartOfProjectOk: OnGetIDEFileInfo=Nil.');
  IDEFileFlags:=[];
  OnGetIDEFileInfo(nil,AFilename,[ifsPartOfProject],IDEFileFlags);
  if ifsPartOfProject in IDEFileFlags then
  begin
    if YesToAll.Inc('MixingProjectAndPackage')>1 then exit;
    IDEMessageDialog(lisA2PFileIsUsed,
      Format(lisA2PTheFileIsPartOfTheCurrentProjectItIsABadIdea,[AFilename,LineEnding]),
      mtError,[mbCancel]);
    exit(mrCancel);
  end;
  Result:=mrOK;
end;

class function TPkgFileCheck.UniqueUnitOk(LazPackage: TLazPackage;
  const AnUnitName: string; YesToAll: TYesToAllList): TModalResult;
// This is called only for Pascal units.
var
  PkgFile: TPkgFile;
  i: Integer;
begin
  Result:=mrCancel;
  // check if unitname already exists in package
  PkgFile:=LazPackage.FindUnit(AnUnitName,true);
  if PkgFile<>nil then
  begin
    // a unit with this name already exists in this package => warn
    i:=YesToAll.Inc('UnitnameExistsInPkg');
    if i=1 then
    begin
      if IDEMessageDialog(lisA2PUnitnameAlreadyExists,
              Format(lisA2PTheUnitnameAlreadyExistsInThisPackage,[AnUnitName]),
              mtError,[mbCancel,mbIgnore]) <> mrIgnore then
        exit;
    end else if i<100000 then begin
      if IDEMessageDialog(lisA2PUnitnameAlreadyExists,
              Format(lisA2PTheUnitnameAlreadyExistsInThisPackage,[AnUnitName]),
              mtError,[mbCancel,mbYesToAll]) = mrYesToAll then
        YesToAll.Values['UnitnameExistsInPkg']:='100000'
      else
        exit;
    end;
  end
  else begin
    PkgFile:=PackageGraph.FindUnit(LazPackage,AnUnitName,true,true);
    if (PkgFile<>nil) and (PkgFile.LazPackage<>LazPackage) then
    begin
      // there is already a unit with this name in another package => warn
      i:=YesToAll.Inc('UnitnameExistsInOtherPkg');
      if i=1 then
      begin
        if IDEMessageDialog(lisA2PUnitnameAlreadyExists,
                Format(lisA2PTheUnitnameAlreadyExistsInThePackage,
                       [AnUnitName, LineEnding, PkgFile.LazPackage.IDAsString]),
                mtWarning,[mbCancel,mbIgnore]) <> mrIgnore then
          exit;
      end else if i<100000 then begin
        if IDEMessageDialog(lisA2PUnitnameAlreadyExists,
                Format(lisA2PTheUnitnameAlreadyExistsInThePackage,
                       [AnUnitName, LineEnding, PkgFile.LazPackage.IDAsString]),
                mtWarning,[mbCancel,mbYesToAll]) = mrYesToAll then
          YesToAll.Values['UnitnameExistsInOtherPkg']:='100000'
        else
          exit;
      end;
    end;
  end;
  // check if unitname is a componentclass
  if IDEComponentPalette.FindRegComponent(AnUnitName)<>nil then
  begin
    i:=YesToAll.Inc('UnitnameIsCompName');
    if i=1 then
    begin
      if IDEMessageDialog(lisA2PAmbiguousUnitName,
          Format(lisA2PTheUnitNameIsTheSameAsAnRegisteredComponent,[AnUnitName,LineEnding]),
          mtWarning,[mbCancel,mbIgnore]) <> mrIgnore then
        exit;
    end else if i<100000 then begin
      if IDEMessageDialog(lisA2PAmbiguousUnitName,
          Format(lisA2PTheUnitNameIsTheSameAsAnRegisteredComponent,[AnUnitName,LineEnding]),
          mtWarning,[mbCancel,mbYesToAll]) = mrYesToAll then
        YesToAll.Values['UnitnameIsCompName']:='100000'
      else
        exit;
    end;
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
    exit(mrAbort);
  end;
  Result:=mrOK;
end;

class function TPkgFileCheck.AddingFile(LazPackage: TLazPackage;
  const AFilename: string; OnGetIDEFileInfo: TGetIDEFileStateEvent;
  YesToAll: TYesToAllList): TModalResult;
var
  NewFileType: TPkgFileType;
  AnUnitName: String;
begin
  Assert(FilenameIsAbsolute(AFilename), 'TPkgFileCheck.AddingUnit: Not absolute Filename.');
  // file exists
  Result:=FileExistsOk(LazPackage, AFilename, YesToAll);
  if Result<>mrOK then exit;
  // file is part of project
  Result:=PartOfProjectOk(AFilename, OnGetIDEFileInfo, YesToAll);
  if Result<>mrOK then exit;

  NewFileType:=FileNameToPkgFileType(AFilename);
  if NewFileType<>pftUnit then
    exit(mrOK);                         // Further checks only for Pascal units.
  AnUnitName:=ExtractFileNameOnly(AFilename);
  // unitname
  Result:=UnitNameOk(AFilename, AnUnitName);
  if Result<>mrOK then exit;
  // unit is unique
  Result:=UniqueUnitOk(LazPackage, AnUnitName, YesToAll);
  if Result<>mrOK then exit;
  Result:=mrOK;  // ok
end;

class function TPkgFileCheck.ReAddingFile(LazPackage: TLazPackage;
  FileTyp: TPkgFileType; const AFilename: string;
  OnGetIDEFileInfo: TGetIDEFileStateEvent; YesToAll: TYesToAllList
  ): TModalResult;
var
  AnUnitName: String;
begin
  Assert(FilenameIsAbsolute(AFilename), 'TPkgFileCheck.ReAddingUnit: Not absolute Filename.');
  // file exists
  Result:=FileExistsOk(LazPackage, AFilename, YesToAll);
  if Result<>mrOK then exit;
  // file is part of project
  Result:=PartOfProjectOk(AFilename, OnGetIDEFileInfo, YesToAll);
  if Result<>mrOK then exit;
  if not (FileTyp in [pftUnit, pftMainUnit, pftVirtualUnit]) then
    exit(mrOK);                         // Further checks only for Pascal units.
  AnUnitName:=ExtractFileNameOnly(AFilename);
  // unitname
  Result:=UnitNameOk(AFilename, AnUnitName);
  if Result<>mrOK then exit;
  // unit is unique
  Result:=UniqueUnitOk(LazPackage, AnUnitName, YesToAll);
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
  if FilenameHasPascalExt(AFilename) then
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

