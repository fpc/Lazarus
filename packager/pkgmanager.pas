{  $Id$  }
{
 /***************************************************************************
                            pkgmanager.pas
                            --------------


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
    TPkgManager is the class for the global PkgBoss variable, which controls
    the whole package system in the IDE.
}
unit PkgManager;

{$mode objfpc}{$H+}

interface

{$I ide.inc}

uses
{$IFDEF IDE_MEM_CHECK}
  MemCheck,
{$ENDIF}
  Classes, SysUtils, LCLProc, Forms, Controls, FileCtrl,
  Dialogs, Menus, CodeToolManager, CodeCache, Laz_XMLCfg, AVL_Tree,
  LazarusIDEStrConsts, KeyMapping, EnvironmentOpts, IDEProcs, ProjectDefs,
  InputHistory, IDEDefs, UComponentManMain, Project, ComponentReg,
  PackageEditor, AddToPackageDlg, PackageDefs, PackageLinks, PackageSystem,
  OpenInstalledPkgDlg, PkgGraphExporer, BrokenDependenciesDlg,
  BasePkgManager, MainBar;

type
  TPkgManager = class(TBasePkgManager)
    procedure MainIDEitmPkgOpenPackageFileClick(Sender: TObject);
    procedure MainIDEitmPkgPkgGraphClick(Sender: TObject);
    function OnPackageEditorCreateFile(Sender: TObject;
                                   const Params: TAddToPkgResult): TModalResult;
    procedure OnPackageEditorFreeEditor(APackage: TLazPackage);
    procedure OnPackageEditorGetUnitRegisterInfo(Sender: TObject;
                              const AFilename: string; var TheUnitName: string;
                              var HasRegisterProc: boolean);
    function OnPackageEditorOpenPackage(Sender: TObject; APackage: TLazPackage
                                        ): TModalResult;
    function OnPackageEditorSavePackage(Sender: TObject; APackage: TLazPackage;
                                        SaveAs: boolean): TModalResult;
    procedure PackageGraphChangePackageName(APackage: TLazPackage;
                                            const OldName: string);
    procedure PackageGraphDeletePackage(APackage: TLazPackage);
    procedure PackageGraphDependencyModified(ADependency: TPkgDependency);
    function PackageGraphExplorerOpenPackage(Sender: TObject;
                                           APackage: TLazPackage): TModalResult;
    procedure PkgManagerAddPackage(Pkg: TLazPackage);
    procedure mnuConfigCustomCompsClicked(Sender: TObject);
    procedure mnuPkgOpenPackageClicked(Sender: TObject);
    procedure mnuOpenRecentPackageClicked(Sender: TObject);
    procedure OnApplicationIdle(Sender: TObject);
  private
    function DoShowSavePackageAsDialog(APackage: TLazPackage): TModalResult;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ConnectMainBarEvents; override;
    procedure ConnectSourceNotebookEvents; override;
    procedure SetupMainBarShortCuts; override;
    procedure SetRecentPackagesMenu; override;
    procedure AddFileToRecentPackages(const Filename: string);

    procedure LoadInstalledPackages; override;
    function AddPackageToGraph(APackage: TLazPackage): TModalResult;

    function ShowConfigureCustomComponents: TModalResult; override;
    function DoNewPackage: TModalResult; override;
    function DoShowOpenInstalledPckDlg: TModalResult; override;
    function DoOpenPackage(APackage: TLazPackage): TModalResult; override;
    function DoOpenPackageFile(AFilename: string;
                         Flags: TPkgOpenFlags): TModalResult; override;
    function DoSavePackage(APackage: TLazPackage;
                           Flags: TPkgSaveFlags): TModalResult; override;
    function DoSaveAllPackages(Flags: TPkgSaveFlags): TModalResult; override;
    function DoShowPackageGraph: TModalResult;
    function DoClosePackageEditor(APackage: TLazPackage): TModalResult; override;
    function DoCloseAllPackageEditors: TModalResult; override;
  end;

implementation

{ TPkgManager }

procedure TPkgManager.MainIDEitmPkgOpenPackageFileClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
  I: Integer;
  OpenFlags: TPkgOpenFlags;
begin
  OpenDialog:=TOpenDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisOpenPackageFile;
    OpenDialog.Options:=OpenDialog.Options+[ofAllowMultiSelect];
    if OpenDialog.Execute and (OpenDialog.Files.Count>0) then begin
      OpenFlags:=[pofAddToRecent];
      For I := 0 to OpenDialog.Files.Count-1 do
        Begin
          AFilename:=CleanAndExpandFilename(OpenDialog.Files.Strings[i]);
          if DoOpenPackageFile(AFilename,OpenFlags)=mrAbort then begin
            break;
          end;
        end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TPkgManager.MainIDEitmPkgPkgGraphClick(Sender: TObject);
begin
  DoShowPackageGraph;
end;

function TPkgManager.OnPackageEditorCreateFile(Sender: TObject;
  const Params: TAddToPkgResult): TModalResult;
var
  LE: String;
  UsesLine: String;
  NewSource: String;
begin
  Result:=mrCancel;
  // create sourcecode
  LE:=EndOfLine;
  UsesLine:='Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs';
  if System.Pos(Params.UsedUnitname,UsesLine)<1 then
    UsesLine:=UsesLine+', '+Params.UsedUnitname;
  NewSource:=
     'unit '+Params.UnitName+';'+LE
    +LE
    +'{$mode objfpc}{$H+}'+LE
    +LE
    +'interface'+LE
    +LE
    +'uses'+LE
    +'  '+UsesLine+';'+LE
    +LE
    +'type'+LE
    +'  '+Params.ClassName+' = class('+Params.AncestorType+')'+LE
    +'  private'+LE
    +'    { Private declarations }'+LE
    +'  protected'+LE
    +'    { Protected declarations }'+LE
    +'  public'+LE
    +'    { Public declarations }'+LE
    +'  published'+LE
    +'    { Published declarations }'+LE
    +'  end;'+LE
    +LE
    +'procedure Register;'+LE
    +LE
    +'implementation'+LE
    +LE
    +'procedure Register;'+LE
    +'begin'+LE
    +'  RegisterComponents('''+Params.PageName+''',['+Params.ClassName+']);'+LE
    +'end;'+LE
    +LE
    +'end.'+LE;

  Result:=MainIDE.DoNewEditorFile(nuUnit,Params.UnitFilename,NewSource,
                    [nfOpenInEditor,nfIsNotPartOfProject,nfSave,nfAddToRecent]);
end;

procedure TPkgManager.OnPackageEditorFreeEditor(APackage: TLazPackage);
begin
  APackage.Editor:=nil;
  PackageGraph.ClosePackage(APackage);
end;

procedure TPkgManager.OnPackageEditorGetUnitRegisterInfo(Sender: TObject;
  const AFilename: string; var TheUnitName: string; var HasRegisterProc: boolean
  );
var
  ExpFilename: String;
  CodeBuffer: TCodeBuffer;
begin
  ExpFilename:=CleanAndExpandFilename(AFilename);
  // create default values
  TheUnitName:='';
  HasRegisterProc:=false;
  MainIDE.SaveSourceEditorChangesToCodeCache(-1);
  CodeBuffer:=CodeToolBoss.LoadFile(ExpFilename,true,false);
  if CodeBuffer<>nil then begin
    TheUnitName:=CodeToolBoss.GetSourceName(CodeBuffer,false);
    CodeToolBoss.HasInterfaceRegisterProc(CodeBuffer,HasRegisterProc);
  end;
  if TheUnitName='' then
    TheUnitName:=ExtractFileNameOnly(ExpFilename);
end;

function TPkgManager.OnPackageEditorOpenPackage(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  Result:=DoOpenPackage(APackage);
end;

function TPkgManager.OnPackageEditorSavePackage(Sender: TObject;
  APackage: TLazPackage; SaveAs: boolean): TModalResult;
begin
  if SaveAs then
    Result:=DoSavePackage(APackage,[psfSaveAs])
  else
    Result:=DoSavePackage(APackage,[]);
end;

procedure TPkgManager.PackageGraphChangePackageName(APackage: TLazPackage;
  const OldName: string);
begin
  if PackageGraphExplorer<>nil then
    PackageGraphExplorer.UpdatePackageName(APackage,OldName);
end;

procedure TPkgManager.PackageGraphDeletePackage(APackage: TLazPackage);
begin
  if APackage.Editor<>nil then begin
    APackage.Editor.Hide;
    APackage.Editor.Free;
  end;
end;

procedure TPkgManager.PackageGraphDependencyModified(ADependency: TPkgDependency
  );
var
  DepOwner: TObject;
begin
  DepOwner:=ADependency.Owner;
  if DepOwner is TLazPackage then
    TLazPackage(DepOwner).Modified:=true
  else if DepOwner is TProject then
    TProject(DepOwner).Modified:=true;
end;

function TPkgManager.PackageGraphExplorerOpenPackage(Sender: TObject;
  APackage: TLazPackage): TModalResult;
begin
  Result:=DoOpenPackage(APackage);
end;

procedure TPkgManager.PkgManagerAddPackage(Pkg: TLazPackage);
begin
  if PackageGraphExplorer<>nil then
    PackageGraphExplorer.UpdatePackageAdded(Pkg);
end;

procedure TPkgManager.mnuConfigCustomCompsClicked(Sender: TObject);
begin
  ShowConfigureCustomComponents;
end;

procedure TPkgManager.mnuPkgOpenPackageClicked(Sender: TObject);
begin
  DoShowOpenInstalledPckDlg;
end;

procedure TPkgManager.mnuOpenRecentPackageClicked(Sender: TObject);

  procedure UpdateEnvironment;
  begin
    SetRecentPackagesMenu;
    MainIDE.SaveEnvironment;
  end;

var
  AFilename: string;
begin
  AFileName:=ExpandFilename(TMenuItem(Sender).Caption);
  if DoOpenPackageFile(AFilename,[pofAddToRecent])=mrOk then begin
    UpdateEnvironment;
  end else begin
    // open failed
    if not FileExists(AFilename) then begin
      // file does not exist -> delete it from recent file list
      RemoveFromRecentList(AFilename,EnvironmentOptions.RecentPackageFiles);
      UpdateEnvironment;
    end;
  end;
end;

procedure TPkgManager.OnApplicationIdle(Sender: TObject);
begin
  PackageGraph.CloseUnneededPackages;
end;

function TPkgManager.DoShowSavePackageAsDialog(
  APackage: TLazPackage): TModalResult;
var
  OldPkgFilename: String;
  SaveDialog: TSaveDialog;
  NewFileName: String;
  NewPkgName: String;
  ConflictPkg: TLazPackage;
  PkgFile: TPkgFile;
  LowerFilename: String;
  BrokenDependencies: TList;
  RenameDependencies: Boolean;
begin
  OldPkgFilename:=APackage.Filename;

  SaveDialog:=TSaveDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.Title:='Save Package '+APackage.IDAsString+' (*.lpk)';
    if APackage.HasDirectory then
      SaveDialog.InitialDir:=APackage.Directory;

    // build a nice package filename suggestion
    NewFileName:=APackage.Name+'.lpk';
    SaveDialog.FileName:=NewFileName;

    repeat
      Result:=mrCancel;

      if not SaveDialog.Execute then begin
        // user cancels
        Result:=mrCancel;
        exit;
      end;
      NewFileName:=CleanAndExpandFilename(SaveDialog.Filename);
      NewPkgName:=ExtractFileNameOnly(NewFilename);
      
      // check file extension
      if ExtractFileExt(NewFilename)='' then begin
        // append extension
        NewFileName:=NewFileName+'.lpk';
      end else if ExtractFileExt(NewFilename)<>'.lpk' then begin
        Result:=MessageDlg('Invalid package file extension',
          'Packages must have the extension .lpk',
          mtInformation,[mbRetry,mbAbort],0);
        if Result=mrAbort then exit;
        continue; // try again
      end;

      // check filename
      if (NewPkgName='') or (not IsValidIdent(NewPkgName)) then begin
        Result:=MessageDlg('Invalid package name',
          'The package name "'+NewPkgName+'" is not a valid package name'#13
          +'Please choose another name (e.g. package1.lpk)',
          mtInformation,[mbRetry,mbAbort],0);
        if Result=mrAbort then exit;
        continue; // try again
      end;

      // apply naming conventions
      if lowercase(NewPkgName)<>NewPkgName then begin
        LowerFilename:=ExtractFilePath(NewFilename)
                      +lowercase(ExtractFileName(NewFilename));
        if EnvironmentOptions.PascalFileAskLowerCase then begin
          if MessageDlg('Rename File lowercase?',
            'Should the file renamed lowercase to'#13
            +'"'+LowerFilename+'"?',
            mtConfirmation,[mbYes,mbNo],0)=mrYes
          then
            NewFileName:=LowerFilename;
        end else begin
          if EnvironmentOptions.PascalFileAutoLowerCase then
            NewFileName:=LowerFilename;
        end;
      end;

      // check package name conflict
      ConflictPkg:=PackageGraph.FindAPackageWithName(NewPkgName,APackage);
      if ConflictPkg<>nil then begin
        Result:=MessageDlg('Package name already exists',
          'There is already another package with the name "'+NewPkgName+'".'#13
          +'Conflict package: "'+ConflictPkg.IDAsString+'"'#13
          +'File: "'+ConflictPkg.Filename+'"',
          mtInformation,[mbRetry,mbAbort,mbIgnore],0);
        if Result=mrAbort then exit;
        if Result<>mrIgnore then continue; // try again
      end;
      
      // check file name conflict with project
      if Project1.ProjectUnitWithFilename(NewFilename)<>nil then begin
        Result:=MessageDlg('Filename is used by project',
          'The file name "'+NewFilename+'" is part of the current project.'#13
          +'Projects and Packages should not share files.',
          mtInformation,[mbRetry,mbAbort],0);
        if Result=mrAbort then exit;
        continue; // try again
      end;
      
      // check file name conflicts with other packages
      PkgFile:=PackageGraph.FindFileInAllPackages(NewFilename,true,true);
      if PkgFile<>nil then begin
        Result:=MessageDlg('Filename is used by other package',
          'The file name "'+NewFilename+'" is used by'#13
          +'the package "'+PkgFile.LazPackage.IDAsString+'"'#13
          +'in file "'+PkgFile.LazPackage.Filename+'".',
          mtWarning,[mbRetry,mbAbort],0);
        if Result=mrAbort then exit;
        continue; // try again
      end;
      
      // check for broken dependencies
      BrokenDependencies:=PackageGraph.GetBrokenDependenciesWhenChangingPkgID(
        APackage,NewPkgName,APackage.Version);
      RenameDependencies:=false;
      try
        if BrokenDependencies.Count>0 then begin
          Result:=ShowBrokenDependencies(BrokenDependencies,
                                         DefaultBrokenDepButtons);
          if Result=mrAbort then exit;
          if Result=mrRetry then continue;
          if Result=mrYes then RenameDependencies:=true;
        end;
      finally
        BrokenDependencies.Free;
      end;
      
      // check existing file
      if (CompareFilenames(NewFileName,OldPkgFilename)<>0)
      and FileExists(NewFileName) then begin
        Result:=MessageDlg('Replace File',
          'Replace existing file "'+NewFilename+'"?',
          mtConfirmation,[mbOk,mbCancel],0);
        if Result<>mrOk then exit;
      end;
      
      // check if new file is read/writable
      Result:=MainIDE.DoCheckCreatingFile(NewFileName,true);
      if Result=mrAbort then exit;

    until Result<>mrRetry;
  finally
    InputHistories.StoreFileDialogSettings(SaveDialog);
    SaveDialog.Free;
  end;
  
  // set filename
  APackage.Filename:=NewFilename;
  
  // rename package
  PackageGraph.ChangePackageID(APackage,NewPkgName,APackage.Version,
                               RenameDependencies);

  // clean up old package file to reduce ambigiousities
  if FileExists(OldPkgFilename)
  and (CompareFilenames(OldPkgFilename,NewFilename)<>0) then begin
    if MessageDlg('Delete Old Package File?',
      'Delete old package file "'+OldPkgFilename+'"?',
      mtConfirmation,[mbOk,mbCancel],0)=mrOk
    then begin
      if DeleteFile(OldPkgFilename) then begin
        RemoveFromRecentList(OldPkgFilename,
                             EnvironmentOptions.RecentPackageFiles);
      end else begin
        MessageDlg('Delete failed',
          'Unable to delete file "'+OldPkgFilename+'".',mtError,[mbOk],0);
      end;
    end;
  end;

  // success
  Result:=mrOk;
end;

constructor TPkgManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IDEComponentPalette:=TIDEComponentPalette.Create;
  
  PkgLinks:=TPackageLinks.Create;

  PackageGraph:=TLazPackageGraph.Create;
  PackageGraph.OnChangePackageName:=@PackageGraphChangePackageName;
  PackageGraph.OnAddPackage:=@PkgManagerAddPackage;
  PackageGraph.OnDeletePackage:=@PackageGraphDeletePackage;
  PackageGraph.OnDependencyModified:=@PackageGraphDependencyModified;
  
  PackageEditors:=TPackageEditors.Create;
  PackageEditors.OnOpenFile:=@MainIDE.DoOpenMacroFile;
  PackageEditors.OnOpenPackage:=@OnPackageEditorOpenPackage;
  PackageEditors.OnCreateNewFile:=@OnPackageEditorCreateFile;
  PackageEditors.OnGetIDEFileInfo:=@MainIDE.GetIDEFileState;
  PackageEditors.OnGetUnitRegisterInfo:=@OnPackageEditorGetUnitRegisterInfo;
  PackageEditors.OnFreeEditor:=@OnPackageEditorFreeEditor;
  PackageEditors.OnSavePackage:=@OnPackageEditorSavePackage;

  Application.AddOnIdleHandler(@OnApplicationIdle);
end;

destructor TPkgManager.Destroy;
begin
  FreeThenNil(PackageGraphExplorer);
  FreeThenNil(PackageEditors);
  FreeThenNil(PackageGraph);
  FreeThenNil(PkgLinks);
  FreeThenNil(IDEComponentPalette);
  FreeThenNil(PackageDependencies);
  inherited Destroy;
end;

procedure TPkgManager.ConnectMainBarEvents;
begin
  with MainIDE do begin
    itmCompsConfigCustomComps.OnClick :=@mnuConfigCustomCompsClicked;
    itmPkgOpenPackage.OnClick :=@mnuPkgOpenPackageClicked;
    itmPkgOpenPackageFile.OnClick:=@MainIDEitmPkgOpenPackageFileClick;
    itmPkgPkgGraph.OnClick:=@MainIDEitmPkgPkgGraphClick;
  end;
  
  SetRecentPackagesMenu;
end;

procedure TPkgManager.ConnectSourceNotebookEvents;
begin

end;

procedure TPkgManager.SetupMainBarShortCuts;
begin

end;

procedure TPkgManager.SetRecentPackagesMenu;
begin
  MainIDE.SetRecentSubMenu(MainIDE.itmPkgOpenRecent,
            EnvironmentOptions.RecentPackageFiles,@mnuOpenRecentPackageClicked);
end;

procedure TPkgManager.AddFileToRecentPackages(const Filename: string);
begin
  AddToRecentList(Filename,EnvironmentOptions.RecentPackageFiles,
                  EnvironmentOptions.MaxRecentPackageFiles);
  SetRecentPackagesMenu;
  MainIDE.SaveEnvironment;
end;

procedure TPkgManager.LoadInstalledPackages;
begin
  // base packages
  PackageGraph.AddStaticBasePackages;
  
  PackageGraph.RegisterStaticPackages;
  // custom packages
  // ToDo
end;

function TPkgManager.AddPackageToGraph(APackage: TLazPackage
  ): TModalResult;
var
  ConflictPkg: TLazPackage;
begin
  // check Package Name
  if (APackage.Name='') or (not IsValidIdent(APackage.Name)) then begin
    Result:=MessageDlg('Invalid Package Name',
      'The package name "'+APackage.Name+'" of'#13
      +'the file "'+APackage.Filename+'" is invalid.',
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;

  // check if Package with same name is already loaded
  ConflictPkg:=PackageGraph.FindAPackageWithName(APackage.Name,nil);
  if ConflictPkg<>nil then begin
    Result:=MessageDlg('Package Name already loaded',
      'There is already a package with the name "'+APackage.Name+'" loaded'#13
      +'from file "'+ConflictPkg.Filename+'".',
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;
  
  // add to graph
  PackageGraph.AddPackage(APackage);

  Result:=mrOk;
end;

function TPkgManager.ShowConfigureCustomComponents: TModalResult;
begin
  Result:=ShowConfigureCustomComponentDlg(EnvironmentOptions.LazarusDirectory);
end;

function TPkgManager.DoNewPackage: TModalResult;
var
  NewPackage: TLazPackage;
  CurEditor: TPackageEditorForm;
begin
  // create a new package with standard dependencies
  NewPackage:=PackageGraph.NewPackage('NewPackage');
  NewPackage.AddRequiredDependency(
    PackageGraph.FCLPackage.CreateDependencyForThisPkg);
  NewPackage.Modified:=false;

  // open a package editor
  CurEditor:=PackageEditors.OpenEditor(NewPackage);
  CurEditor.Show;
  Result:=mrOk;
end;

function TPkgManager.DoShowOpenInstalledPckDlg: TModalResult;
var
  APackage: TLazPackage;
begin
  Result:=ShowOpenInstalledPkgDlg(APackage);
  if (Result<>mrOk) then exit;
  Result:=DoOpenPackage(APackage);
end;

function TPkgManager.DoOpenPackage(APackage: TLazPackage): TModalResult;
var
  CurEditor: TPackageEditorForm;
begin
  // open a package editor
  CurEditor:=PackageEditors.OpenEditor(APackage);
  CurEditor.ShowOnTop;
  Result:=mrOk;
end;

function TPkgManager.DoOpenPackageFile(AFilename: string; Flags: TPkgOpenFlags
  ): TModalResult;
var
  APackage: TLazPackage;
  XMLConfig: TXMLConfig;
begin
  AFilename:=CleanAndExpandFilename(AFilename);

  // check if package is already loaded
  APackage:=PackageGraph.FindPackageWithFilename(AFilename,true);
  if APackage=nil then begin
    // package not yet loaded
    
    if not FileExists(AFilename) then begin
      MessageDlg('File not found',
        'File "'+AFilename+'" not found.',
        mtError,[mbCancel],0);
      RemoveFromRecentList(AFilename,EnvironmentOptions.RecentPackageFiles);
      SetRecentPackagesMenu;
      Result:=mrCancel;
      exit;
    end;

    // create a new package
    Result:=mrCancel;
    APackage:=TLazPackage.Create;
    try
      // load the package file
      try
        XMLConfig:=TXMLConfig.Create(AFilename);
        try
          APackage.Filename:=AFilename;
          APackage.LoadFromXMLConfig(XMLConfig,'Package/');
        finally
          XMLConfig.Free;
        end;
      except
        on E: Exception do begin
          Result:=MessageDlg('Error Reading Package',
            'Unable to read package file "'+APackage.Filename+'".',
            mtError,[mbAbort,mbCancel],0);
          exit;
        end;
      end;
      APackage.Modified:=false;
      
      Result:=AddPackageToGraph(APackage);
    finally
      if Result<>mrOk then APackage.Free;
    end;
  end;
  
  Result:=DoOpenPackage(APackage);
end;

function TPkgManager.DoSavePackage(APackage: TLazPackage;
  Flags: TPkgSaveFlags): TModalResult;
var
  XMLConfig: TXMLConfig;
begin
  // do not save during compilation
  if not (MainIDE.ToolStatus in [itNone,itDebugger]) then begin
    Result:=mrAbort;
    exit;
  end;
  
  if APackage.IsVirtual then Include(Flags,psfSaveAs);

  // check if package needs saving
  if (not (psfSaveAs in Flags))
  and (not APackage.ReadOnly) and (not APackage.Modified)
  and FileExists(APackage.Filename) then begin
    Result:=mrOk;
    exit;
  end;

  // ask user if package should be saved
  if pfAskBeforeSaving in Flags then begin
    Result:=MessageDlg('Save package?',
               'Package "'+APackage.IDAsString+'" changed. Save?',
               mtConfirmation,[mbYes,mbNo,mbAbort],0);
    if (Result=mrNo) then Result:=mrIgnore;
    if Result<>mrOk then exit;
  end;

  // save editor files to codetools
  MainIDE.SaveSourceEditorChangesToCodeCache(-1);

  // save package
  if (psfSaveAs in Flags) then begin
    Result:=DoShowSavePackageAsDialog(APackage);
    if Result<>mrOk then exit;
  end;
  
  // backup old file
  Result:=MainIDE.DoBackupFile(APackage.Filename,false);
  if Result=mrAbort then exit;
  
  Result:=MainIDE.DoDeleteAmbigiousFiles(APackage.Filename);
  if Result=mrAbort then exit;

  // save
  try
    ClearFile(APackage.Filename,true);
    XMLConfig:=TXMLConfig.Create(APackage.Filename);
    try
      XMLConfig.Clear;
      APackage.SaveToXMLConfig(XMLConfig,'Package/');
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      Result:=MessageDlg('Error Writing Package',
        'Unable to write package "'+APackage.IDAsString+'"'#13
        +'to file "'+APackage.Filename+'".'#13
        +'Error: '+E.Message,
        mtError,[mbAbort,mbCancel],0);
      exit;
    end;
  end;

  // success
  APackage.Modified:=false;
  // add to recent
  if (psfSaveAs in Flags) then begin
    AddFileToRecentPackages(APackage.Filename);
  end;

  Result:=mrOk;
end;

function TPkgManager.DoShowPackageGraph: TModalResult;
begin
  if PackageGraphExplorer=nil then begin
    PackageGraphExplorer:=TPkgGraphExplorer.Create(Application);
    PackageGraphExplorer.OnOpenPackage:=@PackageGraphExplorerOpenPackage;
  end;
  PackageGraphExplorer.ShowOnTop;
  Result:=mrOk;
end;

function TPkgManager.DoCloseAllPackageEditors: TModalResult;
var
  APackage: TLazPackage;
begin
  while PackageEditors.Count>0 do begin
    APackage:=PackageEditors.Editors[PackageEditors.Count-1].LazPackage;
    Result:=DoClosePackageEditor(APackage);
    if Result<>mrOk then exit;
  end;
  Result:=mrOk;
end;

function TPkgManager.DoClosePackageEditor(APackage: TLazPackage): TModalResult;
begin
  if APackage.Editor<>nil then begin
    APackage.Editor.Free;
  end;
  Result:=mrOk;
end;

function TPkgManager.DoSaveAllPackages(Flags: TPkgSaveFlags): TModalResult;
var
  AllSaved: Boolean;
  i: Integer;
  CurPackage: TLazPackage;
begin
  try
    repeat
      AllSaved:=true;
      i:=0;
      while i<PackageGraph.Count do begin
        CurPackage:=PackageGraph[i];
        if CurPackage.Modified and (not CurPackage.ReadOnly)
        and (not (lpfSkipSaving in CurPackage.Flags)) then begin
          Result:=DoSavePackage(CurPackage,Flags);
          if Result=mrIgnore then
            CurPackage.Flags:=CurPackage.Flags+[lpfSkipSaving];
          if Result<>mrOk then exit;
          AllSaved:=false;
        end;
        inc(i);
      end;
    until AllSaved;
  finally
    // clear all lpfSkipSaving flags
    for i:=0 to PackageGraph.Count-1 do begin
      CurPackage:=PackageGraph[i];
      CurPackage.Flags:=CurPackage.Flags-[lpfSkipSaving];
    end;
  end;
  Result:=mrOk;
end;

end.

