{  $Id$  }
{
 /***************************************************************************
                            packagesystem.pas
                            -----------------


 ***************************************************************************/

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

  Author: Mattias Gaertner

  Abstract:
    The package registration.
}
unit PackageSystem;

{$mode objfpc}{$H+}

interface

{off $DEFINE IDE_MEM_CHECK}

{$DEFINE StopOnRegError}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, AVL_Tree, FileCtrl, Forms, Controls, Dialogs,
  LazarusIDEStrConsts, IDEProcs, PackageLinks, PackageDefs, LazarusPackageIntf,
  ComponentReg, RegisterLCL, RegisterFCL;
  
type
  TFindPackageFlag = (
    fpfSearchInInstalledPckgs,
    fpfSearchInAutoInstallPckgs,
    fpfSearchInPckgsWithEditor,
    fpfSearchInPkgLinks,
    fpfIgnoreVersion
    );
  TFindPackageFlags = set of TFindPackageFlag;
  
const
  fpfSearchPackageEverywhere =
    [fpfSearchInInstalledPckgs,fpfSearchInAutoInstallPckgs,
     fpfSearchInPckgsWithEditor,fpfSearchInPkgLinks];

type
  TPkgAddedEvent = procedure(Pkg: TLazPackage) of object;

  TLazPackageGraph = class
  private
    FAbortRegistration: boolean;
    FErrorMsg: string;
    FFCLPackage: TLazPackage;
    FLCLPackage: TLazPackage;
    FOnAddPackage: TPkgAddedEvent;
    FOnChangePackageName: TPkgChangeNameEvent;
    FRegistrationFile: TPkgFile;
    FRegistrationPackage: TLazPackage;
    FRegistrationUnitName: string;
    FTree: TAVLTree; // sorted tree of TLazPackage
    FItems: TList;   // unsorted list of TLazPackage
    function GetPackages(Index: integer): TLazPackage;
    procedure SetAbortRegistration(const AValue: boolean);
    procedure SetRegistrationPackage(const AValue: TLazPackage);
    function CreateFCLPackage: TLazPackage;
    function CreateLCLPackage: TLazPackage;
    procedure PackageChangedName(Pkg: TLazPackage; const OldName: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    function FindLowestPkgNodeByName(const PkgName: string): TAVLTreeNode;
    function FindNextSameName(ANode: TAVLTreeNode): TAVLTreeNode;
    function FindNodeOfDependency(Dependency: TPkgDependency;
                                  Flags: TFindPackageFlags): TAVLTreeNode;
    function FindOpenPackage(Dependency: TPkgDependency;
                             Flags: TFindPackageFlags): TLazPackage;
    function FindAPackageWithName(const PkgName: string;
                                  IgnorePackage: TLazPackage): TLazPackage;
    function FindPackageWithID(PkgID: TLazPackageID): TLazPackage;
    function FindUnit(StartPackage: TLazPackage; const TheUnitName: string;
                      WithRequiredPackages, IgnoreDeleted: boolean): TPkgFile;
    function FindUnitInAllPackages(const TheUnitName: string;
                                   IgnoreDeleted: boolean): TPkgFile;
    function FindFileInAllPackages(const TheFilename: string;
                                ResolveLinks, IgnoreDeleted: boolean): TPkgFile;
    function FindPackageWithFilename(const TheFilename: string;
                                     ResolveLinks: boolean): TLazPackage;
    function CreateUniqueUnitName(const Prefix: string): string;
    function PackageNameExists(const PkgName: string;
                               IgnorePackage: TLazPackage): boolean;
    function CreateUniquePkgName(const Prefix: string;
                                 IgnorePackage: TLazPackage): string;
    function NewPackage(const Prefix: string): TLazPackage;
    procedure ConsistencyCheck;
    procedure RegisterUnitHandler(const TheUnitName: string;
                                  RegisterProc: TRegisterProc);
    procedure RegisterComponentsHandler(const Page: string;
                                    ComponentClasses: array of TComponentClass);
    procedure RegistrationError(const Msg: string);
    procedure AddPackage(APackage: TLazPackage);
    procedure AddStaticBasePackages;
    procedure RegisterStaticPackages;
    function OpenDependency(Dependency: TPkgDependency;
                            var APackage: TLazPackage): TLoadPackageResult;
    procedure IterateComponentClasses(APackage: TLazPackage;
                               Event: TIterateComponentClassesEvent;
                               WithUsedPackages, WithRequiredPackages: boolean);
    procedure IterateAllComponentClasses(Event: TIterateComponentClassesEvent);
    procedure IteratePackages(Flags: TFindPackageFlags;
                              Event: TIteratePackagesEvent);
    procedure IteratePackagesSorted(Flags: TFindPackageFlags;
                                    Event: TIteratePackagesEvent);
  public
    property Packages[Index: integer]: TLazPackage read GetPackages; default;
    property RegistrationPackage: TLazPackage read FRegistrationPackage
                                              write SetRegistrationPackage;
    property RegistrationUnitName: string read FRegistrationUnitName;
    property RegistrationFile: TPkgFile read FRegistrationFile;
    property ErrorMsg: string read FErrorMsg write FErrorMsg;
    property AbortRegistration: boolean read FAbortRegistration
                                        write SetAbortRegistration;
    property FCLPackage: TLazPackage read FFCLPackage;
    property LCLPackage: TLazPackage read FLCLPackage;
    property OnChangePackageName: TPkgChangeNameEvent read FOnChangePackageName
                                                     write FOnChangePackageName;
    property OnAddPackage: TPkgAddedEvent read FOnAddPackage write FOnAddPackage;
  end;
  
var
  PackageGraph: TLazPackageGraph;

implementation

procedure RegisterComponentsGlobalHandler(const Page: string;
  ComponentClasses: array of TComponentClass);
begin
  PackageGraph.RegisterComponentsHandler(Page,ComponentClasses);
end;

procedure RegisterNoIconGlobalHandler(
  ComponentClasses: array of TComponentClass);
begin
  PackageGraph.RegisterComponentsHandler('',ComponentClasses);
end;

{ TLazPackageGraph }

procedure TLazPackageGraph.PackageChangedName(Pkg: TLazPackage;
  const OldName: string);
begin
  if Assigned(OnChangePackageName) then OnChangePackageName(Pkg,OldName);
end;

function TLazPackageGraph.GetPackages(Index: integer): TLazPackage;
begin
  Result:=TLazPackage(FItems[Index]);
end;

procedure TLazPackageGraph.SetAbortRegistration(const AValue: boolean);
begin
  if FAbortRegistration=AValue then exit;
  FAbortRegistration:=AValue;
end;

procedure TLazPackageGraph.SetRegistrationPackage(const AValue: TLazPackage);
begin
  if FRegistrationPackage=AValue then exit;
  FRegistrationPackage:=AValue;
  AbortRegistration:=false;
  LazarusPackageIntf.RegisterUnit:=@RegisterUnitHandler;
  RegisterComponentsProc:=@RegisterComponentsGlobalHandler;
  RegisterNoIconProc:=@RegisterNoIconGlobalHandler;
end;

constructor TLazPackageGraph.Create;
begin
  FTree:=TAVLTree.Create(@CompareLazPackageID);
  FItems:=TList.Create;
end;

destructor TLazPackageGraph.Destroy;
begin
  if LazarusPackageIntf.RegisterUnit=@RegisterUnitHandler then
    LazarusPackageIntf.RegisterUnit:=nil;
  if RegisterComponentsProc=@RegisterComponentsGlobalHandler then
    RegisterComponentsProc:=nil;
  if RegisterNoIconProc=@RegisterNoIconGlobalHandler then
    RegisterNoIconProc:=nil;
  Clear;
  FItems.Free;
  FTree.Free;
  inherited Destroy;
end;

procedure TLazPackageGraph.Clear;
var
  i: Integer;
  CurPkg: TLazPackage;
begin
  for i:=FItems.Count-1 downto 0 do begin
    CurPkg:=Packages[i];
    FItems.Delete(i);
    FTree.Remove(CurPkg);
    CurPkg.Free;
  end;
end;

function TLazPackageGraph.Count: integer;
begin
  Result:=FItems.Count;
end;

function TLazPackageGraph.FindLowestPkgNodeByName(const PkgName: string
  ): TAVLTreeNode;
var
  PriorNode: TAVLTreeNode;
begin
  Result:=nil;
  if PkgName='' then exit;
  Result:=FTree.FindKey(PChar(PkgName),@CompareNameWithPackageID);
  while Result<>nil do begin
    PriorNode:=FTree.FindPrecessor(Result);
    if (PriorNode=nil)
    or (AnsiCompareText(PkgName,TLazPackage(PriorNode.Data).Name)<>0) then
      break;
    Result:=PriorNode;
  end;
end;

function TLazPackageGraph.FindNextSameName(ANode: TAVLTreeNode): TAVLTreeNode;
var
  NextNode: TAVLTreeNode;
begin
  Result:=nil;
  if ANode=nil then exit;
  NextNode:=FTree.FindSuccessor(ANode);
  if (NextNode=nil)
  or (AnsiCompareText(TLazPackage(ANode.Data).Name,
                      TLazPackage(NextNode.Data).Name)<>0)
  then exit;
  Result:=NextNode;
end;

function TLazPackageGraph.FindNodeOfDependency(Dependency: TPkgDependency;
  Flags: TFindPackageFlags): TAVLTreeNode;
var
  CurPkg: TLazPackage;
begin
  // search in all packages with the same name
  Result:=FindLowestPkgNodeByName(Dependency.PackageName);
  while Result<>nil do begin
    CurPkg:=TLazPackage(Result.Data);
    // check version
    if (not (fpfIgnoreVersion in Flags))
    and (not Dependency.IsCompatible(CurPkg)) then begin
      Result:=FindNextSameName(Result);
      continue;
    end;
    // check installed packages
    if (fpfSearchInInstalledPckgs in Flags)
    and (CurPkg.Installed<>pitNope) then exit;
    // check autoinstall packages
    if (fpfSearchInAutoInstallPckgs in Flags)
    and (CurPkg.AutoInstall<>pitNope) then exit;
    // check packages with opened editor
    if (fpfSearchInPckgsWithEditor in Flags) and (CurPkg.Editor<>nil) then exit;
    // search next package node with same name
    Result:=FindNextSameName(Result);
  end;
end;

function TLazPackageGraph.FindOpenPackage(Dependency: TPkgDependency;
  Flags: TFindPackageFlags): TLazPackage;
var
  ANode: TAVLTreeNode;
begin
  ANode:=FindNodeOfDependency(Dependency,Flags);
  if ANode<>nil then
    Result:=TLazPackage(ANode.Data)
  else
    Result:=nil;
end;

function TLazPackageGraph.FindAPackageWithName(const PkgName: string;
  IgnorePackage: TLazPackage): TLazPackage;
var
  ANode: TAVLTreeNode;
begin
  Result:=nil;
  ANode:=FindLowestPkgNodeByName(PkgName);
  if ANode<>nil then begin
    Result:=TLazPackage(ANode.Data);
    if Result=IgnorePackage then begin
      Result:=nil;
      ANode:=FindNextSameName(ANode);
      if ANode<>nil then
        Result:=TLazPackage(ANode.Data);
    end;
  end;
end;

function TLazPackageGraph.FindPackageWithID(PkgID: TLazPackageID): TLazPackage;
var
  ANode: TAVLTreeNode;
begin
  ANode:=FTree.Find(PkgID);
  if ANode<>nil then
    Result:=TLazPackage(ANode.Data)
  else
    Result:=nil;
end;

function TLazPackageGraph.FindUnit(StartPackage: TLazPackage;
  const TheUnitName: string;
  WithRequiredPackages, IgnoreDeleted: boolean): TPkgFile;
var
  ADependency: TPkgDependency;
  ARequiredPackage: TLazPackage;
begin
  Result:=StartPackage.FindUnit(TheUnitName,IgnoreDeleted);
  if Result<>nil then exit;
  // search also in all required packages
  if WithRequiredPackages then begin
    ADependency:=StartPackage.FirstRequiredDependency;
    while ADependency<>nil do begin
      ARequiredPackage:=FindOpenPackage(ADependency,[fpfSearchInInstalledPckgs]);
      if ARequiredPackage<>nil then begin
        Result:=ARequiredPackage.FindUnit(TheUnitName,IgnoreDeleted);
        if Result<>nil then exit;
      end;
      ADependency:=ADependency.NextRequiresDependency;
    end;
  end;
end;

function TLazPackageGraph.FindUnitInAllPackages(
  const TheUnitName: string; IgnoreDeleted: boolean): TPkgFile;
var
  Cnt: Integer;
  i: Integer;
begin
  Cnt:=Count;
  for i:=0 to Cnt-1 do begin
    Result:=FindUnit(Packages[i],TheUnitName,false,IgnoreDeleted);
    if Result<>nil then exit;
  end;
  Result:=nil;
end;

function TLazPackageGraph.FindFileInAllPackages(const TheFilename: string;
  ResolveLinks, IgnoreDeleted: boolean): TPkgFile;
var
  Cnt: Integer;
  i: Integer;
begin
  Cnt:=Count;
  for i:=0 to Cnt-1 do begin
    Result:=Packages[i].FindPkgFile(TheFilename,ResolveLinks,IgnoreDeleted);
    if Result<>nil then exit;
  end;
  Result:=nil;
end;

function TLazPackageGraph.FindPackageWithFilename(const TheFilename: string;
  ResolveLinks: boolean): TLazPackage;
var
  Cnt: Integer;
  i: Integer;
  AFilename: string;
begin
  Cnt:=Count;
  AFilename:=TheFilename;
  if ResolveLinks then begin
    AFilename:=ReadAllLinks(TheFilename,false);
    if AFilename='' then AFilename:=TheFilename;
  end;
  for i:=0 to Cnt-1 do begin
    Result:=Packages[i];
    if Result.IsVirtual then continue;
    if ResolveLinks then begin
      if CompareFilenames(TheFilename,Result.GetResolvedFilename)=0 then
        exit;
    end else begin
      if CompareFilenames(TheFilename,Result.Filename)=0 then
        exit;
    end;
  end;
  Result:=nil;
end;

function TLazPackageGraph.CreateUniqueUnitName(const Prefix: string): string;
var
  i: Integer;
begin
  if FindUnitInAllPackages(Prefix,false)=nil then
    Result:=Prefix
  else begin
    i:=1;
    repeat
      Result:=Prefix+IntToStr(i);
    until FindUnitInAllPackages(Result,false)=nil;
  end;
end;

function TLazPackageGraph.PackageNameExists(const PkgName: string;
  IgnorePackage: TLazPackage): boolean;
var
  ANode: TAVLTreeNode;
begin
  Result:=false;
  if PkgName<>'' then begin
    ANode:=FindLowestPkgNodeByName(PkgName);
    if (ANode<>nil) and (IgnorePackage=TLazPackage(ANode.Data)) then
      ANode:=FindNextSameName(ANode);
    Result:=ANode<>nil;
  end;
end;

function TLazPackageGraph.CreateUniquePkgName(const Prefix: string;
  IgnorePackage: TLazPackage): string;
var
  i: Integer;
begin
  // try Prefix alone
  if not PackageNameExists(Prefix,IgnorePackage) then begin
    Result:=Prefix;
  end else begin
    // try Prefix + number
    i:=1;
    while PackageNameExists(Prefix+IntToStr(i),IgnorePackage) do inc(i);
    Result:=Prefix+IntToStr(i);
  end;
end;

function TLazPackageGraph.NewPackage(const Prefix: string): TLazPackage;
begin
  Result:=TLazPackage.Create;
  Result.Name:=CreateUniquePkgName('NewPackage',nil);
  FItems.Add(Result);
  FTree.Add(Result);
end;

procedure TLazPackageGraph.ConsistencyCheck;
begin
  CheckList(FItems,true,true,true);
end;

procedure TLazPackageGraph.RegisterUnitHandler(const TheUnitName: string;
  RegisterProc: TRegisterProc);
begin
  if AbortRegistration then exit;

  ErrorMsg:='';
  FRegistrationFile:=nil;
  FRegistrationUnitName:='';

  // check package
  if FRegistrationPackage=nil then begin
    RegistrationError('');
    exit;
  end;
  try
    // check unitname
    FRegistrationUnitName:=TheUnitName;
    if not IsValidIdent(FRegistrationUnitName) then begin
      RegistrationError('Invalid Unitname: '+FRegistrationUnitName);
      exit;
    end;
    // check unit file
    FRegistrationFile:=FRegistrationPackage.FindUnit(FRegistrationUnitName,true);
    if FRegistrationFile=nil then begin
      FRegistrationFile:=
        FRegistrationPackage.FindUnit(FRegistrationUnitName,false);
      if FRegistrationFile=nil then begin
        RegistrationError('Unit not found: "'+FRegistrationUnitName+'"');
      end else begin
        RegistrationError(
          'Unit "'+FRegistrationUnitName+'" was deleted from package');
      end;
      exit;
    end;
    // check registration procedure
    if RegisterProc=nil then begin
      RegistrationError('Register procedure is nil');
      exit;
    end;
    {$IFNDEF StopOnRegError}
    try
    {$ENDIF}
      // call the registration procedure
      RegisterProc();
    {$IFNDEF StopOnRegError}
    except
      on E: Exception do begin
        RegistrationError(E.Message);
      end;
    end;
    {$ENDIF}
    // clean up
  finally
    FRegistrationUnitName:='';
    FRegistrationFile:=nil;
  end;
end;

procedure TLazPackageGraph.RegisterComponentsHandler(const Page: string;
  ComponentClasses: array of TComponentClass);
var
  i: integer;
  CurComponent: TComponentClass;
  NewPkgComponent: TPkgComponent;
  CurClassname: string;
begin
  {$IFDEF IDE_MEM_CHECK}
  CheckHeap('TLazPackageGraph.RegisterComponentsHandler Page='+Page);
  {$ENDIF}
  if AbortRegistration or (Low(ComponentClasses)>High(ComponentClasses)) then
    exit;

  ErrorMsg:='';

  // check package
  if FRegistrationPackage=nil then begin
    RegistrationError('');
    exit;
  end;
  // check unit file
  if FRegistrationFile=nil then begin
    RegistrationError('Can not register components without unit');
    exit;
  end;
  // register components
  for i:=Low(ComponentClasses) to High(ComponentClasses) do begin
    CurComponent:=ComponentClasses[i];
    if (CurComponent=nil) then continue;
    {$IFNDEF StopOnRegError}
    try
    {$ENDIF}
      CurClassname:=CurComponent.Classname;
      if not IsValidIdent(CurClassname) then begin
        RegistrationError('Invalid component class');
        continue;
      end;
    {$IFNDEF StopOnRegError}
    except
      on E: Exception do begin
        RegistrationError(E.Message);
        continue;
      end;
    end;
    {$ENDIF}
    if IDEComponentPalette.FindComponent(CurClassname)<>nil then begin
      RegistrationError(
        'Component Class "'+CurComponent.ClassName+'" already defined');
    end;
    if AbortRegistration then exit;
    NewPkgComponent:=
      FRegistrationPackage.AddComponent(FRegistrationFile,Page,CurComponent);
    IDEComponentPalette.AddComponent(NewPkgComponent);
  end;
end;

procedure TLazPackageGraph.RegistrationError(const Msg: string);
var
  DlgResult: Integer;
begin
  // create nice and useful error message

  // current registration package
  if FRegistrationPackage=nil then begin
    ErrorMsg:='RegisterUnit was called, but no package is registering.';
  end else begin
    ErrorMsg:='Package: "'+FRegistrationPackage.NameAndVersion+'"';
    // current unitname
    if FRegistrationUnitName<>'' then
      ErrorMsg:=ErrorMsg+#13+'Unit Name: "'+FRegistrationUnitName+'"';
    // current file
    if FRegistrationFile<>nil then
      ErrorMsg:=ErrorMsg+#13+'File Name: "'+FRegistrationFile.Filename+'"';
  end;
  // append message
  if Msg<>'' then
    ErrorMsg:=ErrorMsg+#13#13+Msg;
  // tell user
  DlgResult:=MessageDlg('Registration Error',
                        ErrorMsg,mtError,[mbIgnore,mbAbort],0);
  if DlgResult=mrAbort then
    AbortRegistration:=true;
end;

function TLazPackageGraph.CreateFCLPackage: TLazPackage;
begin
  Result:=TLazPackage.Create;
  with Result do begin
    AutoCreated:=true;
    Name:='FCL';
    Title:='FreePascal Component Library';
    Filename:='$(FPCSrcDir)/fcl/';
    Version.SetValues(1,0,1,1);
    Author:='FPC team';
    AutoInstall:=pitStatic;
    AutoUpdate:=false;
    Description:='The FCL provides the base classes for object pascal.';
    PackageType:=lptDesignTime;
    Installed:=pitStatic;

    // add files
    AddFile('inc/process.pp','Process',pftUnit,[pffHasRegisterProc],cpBase);
    AddFile('db/db.pp','DB',pftUnit,[pffHasRegisterProc],cpBase);

    Modified:=false;
  end;
end;

function TLazPackageGraph.CreateLCLPackage: TLazPackage;
begin
  Result:=TLazPackage.Create;
  with Result do begin
    AutoCreated:=true;
    Name:='LCL';
    Title:='Lazarus Component Library';
    Filename:='$(LazarusDir)/lcl/';
    Version.SetValues(1,0,1,1);
    Author:='Lazarus';
    AutoInstall:=pitStatic;
    AutoUpdate:=false;
    Description:='The LCL contains all base components for form editing.';
    PackageType:=lptDesignTime;
    Installed:=pitStatic;

    // add files
    AddFile('menus.pp','Menus',pftUnit,[pffHasRegisterProc],cpLCL);
    AddFile('buttons.pp','Buttons',pftUnit,[pffHasRegisterProc],cpLCL);
    AddFile('stdctrls.pp','StdCtrls',pftUnit,[pffHasRegisterProc],cpLCL);
    AddFile('extctrls.pp','ExtCtrls',pftUnit,[pffHasRegisterProc],cpLCL);
    AddFile('comctrls.pp','ComCtrls',pftUnit,[pffHasRegisterProc],cpLCL);
    AddFile('forms.pp','Forms',pftUnit,[pffHasRegisterProc],cpLCL);
    AddFile('grids.pas','Grids',pftUnit,[pffHasRegisterProc],cpLCL);
    AddFile('controls.pp','Controls',pftUnit,[pffHasRegisterProc],cpLCL);
    AddFile('dialogs.pp','Dialogs',pftUnit,[pffHasRegisterProc],cpLCL);
    AddFile('spin.pp','Spin',pftUnit,[pffHasRegisterProc],cpLCL);
    AddFile('arrow.pp','Arrow',pftUnit,[pffHasRegisterProc],cpLCL);
    AddFile('calendar.pp','Calendar',pftUnit,[pffHasRegisterProc],cpLCL);
    
    // add requirements
    AddRequiredDependency(FCLPackage.CreateDependencyForThisPkg);
    
    Modified:=false;
  end;
end;

procedure TLazPackageGraph.AddPackage(APackage: TLazPackage);
var
  RequiredPackage: TLazPackage;
  Dependency: TPkgDependency;
begin
  FTree.Add(APackage);
  FItems.Add(APackage);
  APackage.OnChangeName:=@PackageChangedName;
  
  // open all dependencies
  Dependency:=APackage.FirstRequiredDependency;
  while Dependency<>nil do begin
    OpenDependency(Dependency,RequiredPackage);
    Dependency:=Dependency.NextRequiresDependency;
  end;
  
  // update all dependencies
  
  
  if Assigned(OnAddPackage) then OnAddPackage(APackage);
end;

procedure TLazPackageGraph.AddStaticBasePackages;
begin
  // FCL
  FFCLPackage:=CreateFCLPackage;
  AddPackage(FFCLPackage);
  // LCL
  FLCLPackage:=CreateLCLPackage;
  AddPackage(FLCLPackage);
end;

procedure TLazPackageGraph.RegisterStaticPackages;
begin
  // FCL
  RegistrationPackage:=FCLPackage;
  RegisterFCL.Register;
  FCLPackage.Registered:=true;
  
  // LCL
  RegistrationPackage:=LCLPackage;
  RegisterLCL.Register;
  LCLPackage.Registered:=true;

  // clean up
  RegistrationPackage:=nil;
end;

function TLazPackageGraph.OpenDependency(Dependency: TPkgDependency;
  var APackage: TLazPackage): TLoadPackageResult;
var
  ANode: TAVLTreeNode;
  PkgLink: TPackageLink;
begin
  if Dependency.LoadPackageResult=lprUndefined then begin
    // search in opened packages
    ANode:=FindNodeOfDependency(Dependency,fpfSearchPackageEverywhere);
    if (APackage=nil) then begin
      // package not yet open
      PkgLinks.UpdateAll;
      PkgLink:=PkgLinks.FindLinkWithDependency(Dependency);
      if PkgLink<>nil then begin

        // ToDo

      end;
    end;
    // save result
    if ANode<>nil then begin
      Dependency.RequiredPackage:=TLazPackage(ANode.Data);
      Dependency.LoadPackageResult:=lprSuccess;
    end else begin
      Dependency.RequiredPackage:=nil;
      Dependency.LoadPackageResult:=lprNotFound;
    end;
  end;
  APackage:=Dependency.RequiredPackage;
  Result:=Dependency.LoadPackageResult;
end;

procedure TLazPackageGraph.IterateComponentClasses(APackage: TLazPackage;
  Event: TIterateComponentClassesEvent; WithUsedPackages,
  WithRequiredPackages: boolean);
var
  ARequiredPackage: TLazPackage;
  ADependency: TPkgDependency;
begin
  APackage.IterateComponentClasses(Event,WithUsedPackages);
  // iterate through all required packages
  if WithRequiredPackages then begin
    ADependency:=APackage.FirstRequiredDependency;
    while ADependency<>nil do begin
      ARequiredPackage:=FindOpenPackage(ADependency,[fpfSearchInInstalledPckgs]);
      if ARequiredPackage<>nil then begin
        ARequiredPackage.IterateComponentClasses(Event,false);
      end;
      ADependency:=ADependency.NextRequiresDependency;
    end;
  end;
end;

procedure TLazPackageGraph.IterateAllComponentClasses(
  Event: TIterateComponentClassesEvent);
var
  Cnt: Integer;
  i: Integer;
begin
  Cnt:=Count;
  for i:=0 to Cnt-1 do
    IterateComponentClasses(Packages[i],Event,false,false);
end;

procedure TLazPackageGraph.IteratePackages(Flags: TFindPackageFlags;
  Event: TIteratePackagesEvent);
var
  CurPkg: TLazPackage;
  i: Integer;
begin
  // iterate opened packages
  for i:=0 to FItems.Count-1 do begin
    CurPkg:=Packages[i];
    // check installed packages
    if ((fpfSearchInInstalledPckgs in Flags) and (CurPkg.Installed<>pitNope))
    // check autoinstall packages
    or ((fpfSearchInAutoInstallPckgs in Flags) and (CurPkg.AutoInstall<>pitNope))
    // check packages with opened editor
    or ((fpfSearchInPckgsWithEditor in Flags) and (CurPkg.Editor<>nil))
    then
      Event(CurPkg);
  end;
  // iterate in package links
  if (fpfSearchInPkgLinks in Flags) then begin
    PkgLinks.IteratePackages(Event);
  end;
end;

procedure TLazPackageGraph.IteratePackagesSorted(Flags: TFindPackageFlags;
  Event: TIteratePackagesEvent);
var
  ANode: TAVLTreeNode;
  CurPkg: TLazPackage;
begin
  ANode:=FTree.FindLowest;
  while ANode<>nil do begin
    CurPkg:=TLazPackage(ANode.Data);
    // check installed packages
    if ((fpfSearchInInstalledPckgs in Flags) and (CurPkg.Installed<>pitNope))
    // check autoinstall packages
    or ((fpfSearchInAutoInstallPckgs in Flags) and (CurPkg.AutoInstall<>pitNope))
    // check packages with opened editor
    or ((fpfSearchInPckgsWithEditor in Flags) and (CurPkg.Editor<>nil))
    then
      Event(CurPkg);
    ANode:=FTree.FindSuccessor(ANode);
  end;
end;

initialization
  PackageGraph:=nil;

end.

