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
  InputHistory, IDEDefs, Project, ComponentReg, UComponentManMain,
  PackageEditor, AddToPackageDlg, PackageDefs, PackageLinks, PackageSystem,
  OpenInstalledPkgDlg, PkgGraphExplorer, BrokenDependenciesDlg, CompilerOptions,
  ExtToolDialog, ExtToolEditDlg, EditDefineTree, DefineTemplates,
  ProjectInspector, ComponentPalette, UnitEditor, AddFileToAPackageDlg,
  BasePkgManager, MainBar;

type
  TPkgManager = class(TBasePkgManager)
    // events
    function OnPackageEditorCompilePackage(Sender: TObject;
      APackage: TLazPackage; CompileAll: boolean): TModalResult;
    function OnPackageEditorCreateFile(Sender: TObject;
                                   const Params: TAddToPkgResult): TModalResult;
    function OnPackageEditorOpenPackage(Sender: TObject; APackage: TLazPackage
                                        ): TModalResult;
    function OnPackageEditorSavePackage(Sender: TObject; APackage: TLazPackage;
                                        SaveAs: boolean): TModalResult;
    function PackageGraphExplorerOpenPackage(Sender: TObject;
                                           APackage: TLazPackage): TModalResult;
    procedure MainIDEitmPkgOpenPackageFileClick(Sender: TObject);
    procedure MainIDEitmPkgPkgGraphClick(Sender: TObject);
    procedure MainIDEitmPkgAddCurUnitToPkgClick(Sender: TObject);
    procedure mnuConfigCustomCompsClicked(Sender: TObject);
    procedure mnuOpenRecentPackageClicked(Sender: TObject);
    procedure mnuPkgOpenPackageClicked(Sender: TObject);
    procedure OnApplicationIdle(Sender: TObject);
    procedure OnPackageEditorFreeEditor(APackage: TLazPackage);
    procedure OnPackageEditorGetUnitRegisterInfo(Sender: TObject;
                              const AFilename: string; var TheUnitName: string;
                              var HasRegisterProc: boolean);
    procedure PackageGraphAddPackage(Pkg: TLazPackage);
    procedure PackageGraphBeginUpdate(Sender: TObject);
    procedure PackageGraphChangePackageName(APackage: TLazPackage;
                                            const OldName: string);
    procedure PackageGraphDeletePackage(APackage: TLazPackage);
    procedure PackageGraphDependencyModified(ADependency: TPkgDependency);
    procedure PackageGraphEndUpdate(Sender: TObject; GraphChanged: boolean);
    procedure IDEComponentPaletteEndUpdate(Sender: TObject;
      PaletteChanged: boolean);
  private
    // helper functions
    function DoShowSavePackageAsDialog(APackage: TLazPackage): TModalResult;
    function CompileRequiredPackages(APackage: TLazPackage;
                                 FirstDependency: TPkgDependency): TModalResult;
    function CheckPackageGraphForCompilation(APackage: TLazPackage;
                                 FirstDependency: TPkgDependency): TModalResult;
    function DoPreparePackageOutputDirectory(APackage: TLazPackage): TModalResult;
    function DoSavePackageCompiledState(APackage: TLazPackage;
                  const CompilerFilename, CompilerParams: string): TModalResult;
    function DoLoadPackageCompiledState(APackage: TLazPackage;
                                        IgnoreErrors: boolean): TModalResult;
    function CheckIfPackageNeedsCompilation(APackage: TLazPackage;
              var NewCompilerFilename, NewCompilerParams: string): TModalResult;
    function MacroFunctionPkgSrcPath(Data: Pointer): boolean;
    function MacroFunctionPkgUnitPath(Data: Pointer): boolean;
    function MacroFunctionPkgIncPath(Data: Pointer): boolean;
    function DoGetUnitRegisterInfo(const AFilename: string;
                          var TheUnitName: string; var HasRegisterProc: boolean;
                          IgnoreErrors: boolean): TModalResult;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ConnectMainBarEvents; override;
    procedure ConnectSourceNotebookEvents; override;
    procedure SetupMainBarShortCuts; override;
    procedure SetRecentPackagesMenu; override;
    procedure AddFileToRecentPackages(const Filename: string);
    procedure SaveSettings; override;
    
    function GetDefaultSaveDirectoryForFile(const Filename: string): string; override;

    procedure LoadInstalledPackages; override;
    procedure UpdateVisibleComponentPalette; override;
    
    function AddPackageToGraph(APackage: TLazPackage): TModalResult;
    function OpenProjectDependencies(AProject: TProject): TModalResult; override;
    procedure AddDefaultDependencies(AProject: TProject); override;
    procedure AddProjectDependency(AProject: TProject; APackage: TLazPackage); override;
    procedure AddProjectRegCompDependency(AProject: TProject;
                          ARegisteredComponent: TRegisteredComponent); override;
    procedure AddProjectLCLDependency(AProject: TProject); override;

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
    procedure DoShowPackageGraphPathList(PathList: TList); override;
    function DoCompileProjectDependencies(AProject: TProject;
                               Flags: TPkgCompileFlags): TModalResult; override;
    function DoCompilePackage(APackage: TLazPackage;
                              Flags: TPkgCompileFlags): TModalResult; override;
    function DoSavePackageMainSource(APackage: TLazPackage;
                              Flags: TPkgCompileFlags): TModalResult; override;
    function OnRenameFile(const OldFilename,
                          NewFilename: string): TModalResult; override;
    function DoAddActiveUnitToAPackage: TModalResult;
                          

    function OnProjectInspectorOpen(Sender: TObject): boolean; override;
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

procedure TPkgManager.IDEComponentPaletteEndUpdate(Sender: TObject;
  PaletteChanged: boolean);
begin
  UpdateVisibleComponentPalette;
end;

procedure TPkgManager.MainIDEitmPkgAddCurUnitToPkgClick(Sender: TObject);
begin
  DoAddActiveUnitToAPackage;
end;

function TPkgManager.OnPackageEditorCompilePackage(Sender: TObject;
  APackage: TLazPackage; CompileAll: boolean): TModalResult;
var
  Flags: TPkgCompileFlags;
begin
  Flags:=[];
  if CompileAll then Include(Flags,pcfCleanCompile);
  Result:=DoCompilePackage(APackage,Flags);
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
begin
  DoGetUnitRegisterInfo(AFilename,TheUnitName,HasRegisterProc,true);
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

procedure TPkgManager.PackageGraphBeginUpdate(Sender: TObject);
begin
  if PackageGraphExplorer<>nil then PackageGraphExplorer.BeginUpdate;
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

procedure TPkgManager.PackageGraphAddPackage(Pkg: TLazPackage);
begin
  if FileExists(Pkg.FileName) then PkgLinks.AddUserLink(Pkg);
  if PackageGraphExplorer<>nil then
    PackageGraphExplorer.UpdatePackageAdded(Pkg);
end;

procedure TPkgManager.PackageGraphEndUpdate(Sender: TObject;
  GraphChanged: boolean);
begin
  if GraphChanged then IncreaseCompilerGraphStamp;
  if PackageGraphExplorer<>nil then begin
    if GraphChanged then PackageGraphExplorer.UpdateAll;
    PackageGraphExplorer.EndUpdate;
  end;
  if GraphChanged then begin
    if PackageEditors<>nil then
      PackageEditors.UpdateAllEditors;
    if ProjInspector<>nil then
      ProjInspector.UpdateItems;
  end;
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
  if (Screen.ActiveCustomForm<>nil)
  and (fsModal in Screen.ActiveCustomForm.FormState) then exit;
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

function TPkgManager.CompileRequiredPackages(APackage: TLazPackage;
  FirstDependency: TPkgDependency): TModalResult;
var
  AutoPackages: TList;
  i: Integer;
begin
  writeln('TPkgManager.CompileRequiredPackages A ');
  AutoPackages:=PackageGraph.GetAutoCompilationOrder(APackage,FirstDependency);
  if AutoPackages<>nil then begin
    try
      i:=0;
      while i<AutoPackages.Count do begin
        Result:=DoCompilePackage(TLazPackage(AutoPackages[i]),
                      [pcfDoNotCompileDependencies,pcfOnlyIfNeeded,
                       pcfAutomatic]);
        if Result<>mrOk then exit;
        inc(i);
      end;
    finally
      AutoPackages.Free;
    end;
  end;
  writeln('TPkgManager.CompileRequiredPackages END ');
  Result:=mrOk;
end;

function TPkgManager.CheckPackageGraphForCompilation(APackage: TLazPackage;
  FirstDependency: TPkgDependency): TModalResult;
var
  PathList: TList;
begin
  writeln('TPkgManager.CheckPackageGraphForCompilation A');
  // check for unsaved packages
  PathList:=PackageGraph.FindUnsavedDependencyPath(APackage,FirstDependency);
  if PathList<>nil then begin
    DoShowPackageGraphPathList(PathList);
    Result:=MessageDlg('Unsaved package',
      'There is an unsaved package in the required packages. See package graph.',
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;

  // check for broken dependencies
  PathList:=PackageGraph.FindBrokenDependencyPath(APackage,FirstDependency);
  if PathList<>nil then begin
    DoShowPackageGraphPathList(PathList);
    Result:=MessageDlg('Broken dependency',
      'A required packages was not found. See package graph.',
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;

  // check for circle dependencies
  PathList:=PackageGraph.FindCircleDependencyPath(APackage,FirstDependency);
  if PathList<>nil then begin
    DoShowPackageGraphPathList(PathList);
    Result:=MessageDlg('Circle in package dependencies',
      'There is a circle in the required packages. See package graph.',
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;

  writeln('TPkgManager.CheckPackageGraphForCompilation END');
  Result:=mrOk;
end;

function TPkgManager.DoSavePackageCompiledState(APackage: TLazPackage;
  const CompilerFilename, CompilerParams: string): TModalResult;
var
  XMLConfig: TXMLConfig;
  StateFile: String;
  CompilerFileDate: Integer;
begin
  StateFile:=APackage.GetStateFilename;
  try
    CompilerFileDate:=FileAge(CompilerFilename);
    ClearFile(StateFile,true);
    XMLConfig:=TXMLConfig.Create(StateFile);
    try
      XMLConfig.SetValue('Compiler/Value',CompilerFilename);
      XMLConfig.SetValue('Compiler/Date',CompilerFileDate);
      XMLConfig.SetValue('Params/Value',CompilerParams);
      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
    APackage.LastCompilerFilename:=CompilerFilename;
    APackage.LastCompilerFileDate:=CompilerFileDate;
    APackage.LastCompilerParams:=CompilerParams;
    APackage.StateFileDate:=FileAge(StateFile);
    APackage.Flags:=APackage.Flags+[lpfStateFileLoaded];
  except
    on E: Exception do begin
      Result:=MessageDlg('Error writing file',
        'Unable to write state file "'+StateFile+'"'#13
        +'of package '+APackage.IDAsString+'.'#13
        +'Error: '+E.Message,
        mtError,[mbAbort,mbCancel],0);
      exit;
    end;
  end;

  Result:=MainIDE.DoDeleteAmbigiousFiles(StateFile);
  if Result<>mrOk then exit;
end;

function TPkgManager.DoLoadPackageCompiledState(APackage: TLazPackage;
  IgnoreErrors: boolean): TModalResult;
var
  XMLConfig: TXMLConfig;
  StateFile: String;
  StateFileAge: Integer;
begin
  StateFile:=APackage.GetStateFilename;
  if not FileExists(StateFile) then begin
    writeln('TPkgManager.DoLoadPackageCompiledState Statefile not found: ',StateFile);
    APackage.Flags:=APackage.Flags-[lpfStateFileLoaded];
    Result:=mrOk;
    exit;
  end;

  // read the state file
  StateFileAge:=FileAge(StateFile);
  if (not (lpfStateFileLoaded in APackage.Flags))
  or (APackage.StateFileDate<>StateFileAge) then begin
    APackage.Flags:=APackage.Flags-[lpfStateFileLoaded];
    try
      XMLConfig:=TXMLConfig.Create(StateFile);
      try
        APackage.LastCompilerFilename:=
          XMLConfig.GetValue('Compiler/Value','');
        APackage.LastCompilerFileDate:=
          XMLConfig.GetValue('Compiler/Date',0);
        APackage.LastCompilerParams:=
          XMLConfig.GetValue('Params/Value','');
      finally
        XMLConfig.Free;
      end;
      APackage.StateFileDate:=StateFileAge;
    except
      on E: Exception do begin
        if IgnoreErrors then begin
          Result:=mrOk;
        end else begin
          Result:=MessageDlg('Error reading file',
            'Unable to read state file "'+StateFile+'"'#13
            +'of package '+APackage.IDAsString+'.'#13
            +'Error: '+E.Message,
            mtError,[mbCancel,mbAbort],0);
        end;
        exit;
      end;
    end;
    APackage.Flags:=APackage.Flags+[lpfStateFileLoaded];
  end;
  
  Result:=mrOk;
end;

function TPkgManager.DoPreparePackageOutputDirectory(APackage: TLazPackage
  ): TModalResult;
var
  OutputDir: String;
  StateFile: String;
begin
  OutputDir:=APackage.GetOutputDirectory;
  StateFile:=APackage.GetStateFilename;

  // create the output directory
  if not ForceDirectory(OutputDir) then begin
    Result:=MessageDlg('Unable to create directory',
      'Unable to create output directory "'+OutputDir+'"'#13
      +'for package '+APackage.IDAsString+'.',
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;

  // delete old Compile State file
  if FileExists(StateFile) and not DeleteFile(StateFile) then begin
    Result:=MessageDlg('Unable to delete file',
      'Unable to delete old state file "'+StateFile+'"'#13
      +'for package '+APackage.IDAsString+'.',
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;
  APackage.Flags:=APackage.Flags-[lpfStateFileLoaded];
  
  Result:=mrOk;
end;

function TPkgManager.CheckIfPackageNeedsCompilation(APackage: TLazPackage;
  var NewCompilerFilename, NewCompilerParams: string): TModalResult;
var
  OutputDir: String;
  SrcFilename: String;
  StateFilename: String;
  StateFileAge: Integer;
  i: Integer;
  CurFile: TPkgFile;
  Dependency: TPkgDependency;
  RequiredPackage: TLazPackage;
  OtherStateFile: String;
begin
  Result:=mrYes;
  writeln('TPkgManager.CheckIfPackageNeedsCompilation A ',APackage.IDAsString);

  // calculate compiler filename and parameters
  OutputDir:=APackage.GetOutputDirectory;
  SrcFilename:=OutputDir+APackage.GetCompileSourceFilename;
  NewCompilerFilename:=APackage.GetCompilerFilename;
  NewCompilerParams:=APackage.CompilerOptions.MakeOptionsString(
                               APackage.CompilerOptions.DefaultMakeOptionsFlags)
                 +' '+CreateRelativePath(SrcFilename,APackage.Directory);

  // check state file
  StateFilename:=APackage.GetStateFilename;
  Result:=DoLoadPackageCompiledState(APackage,false);
  if Result<>mrOk then exit;
  if not (lpfStateFileLoaded in APackage.Flags) then begin
    writeln('TPkgManager.CheckIfPackageNeedsCompilation  No state file for ',APackage.IDAsString);
    Result:=mrYes;
    exit;
  end;

  StateFileAge:=FileAge(StateFilename);

  // check all required packages
  Dependency:=APackage.FirstRequiredDependency;
  while Dependency<>nil do begin
    if (Dependency.LoadPackageResult=lprSuccess) then begin
      RequiredPackage:=Dependency.RequiredPackage;
      // check compile state file of required package
      if not RequiredPackage.AutoCreated then begin
        Result:=DoLoadPackageCompiledState(RequiredPackage,false);
        if Result<>mrOk then exit;
        Result:=mrYes;
        if not (lpfStateFileLoaded in RequiredPackage.Flags) then begin
          writeln('TPkgManager.CheckIfPackageNeedsCompilation  No state file for ',RequiredPackage.IDAsString);
          exit;
        end;
        if StateFileAge<RequiredPackage.StateFileDate then begin
          writeln('TPkgManager.CheckIfPackageNeedsCompilation  Required ',
            RequiredPackage.IDAsString,' State file is newer than ',
            'State file ',APackage.IDAsString);
          exit;
        end;
      end;
      // check output state file of required package
      if RequiredPackage.OutputStateFile<>'' then begin
        OtherStateFile:=RequiredPackage.OutputStateFile;
        MainIDE.MacroList.SubstituteStr(OtherStateFile);
        if FileExists(OtherStateFile)
        and (FileAge(OtherStateFile)>StateFileAge) then
          exit;
      end;
    end;
    Dependency:=Dependency.NextRequiresDependency;
  end;
  
  Result:=mrYes;

  // check main source file
  if not FileExists(SrcFilename) or (StateFileAge<FileAge(SrcFilename)) then
  begin
    writeln('TPkgManager.CheckIfPackageNeedsCompilation  SrcFile outdated ',APackage.IDAsString);
    exit;
  end;

  // check compiler and params
  if NewCompilerFilename<>APackage.LastCompilerFilename then begin
    writeln('TPkgManager.CheckIfPackageNeedsCompilation  Compiler filename changed for ',APackage.IDAsString);
    writeln('  Old="',APackage.LastCompilerFilename,'"');
    writeln('  Now="',NewCompilerFilename,'"');
    exit;
  end;
  if not FileExists(NewCompilerFilename) then begin
    writeln('TPkgManager.CheckIfPackageNeedsCompilation  Compiler filename not found for ',APackage.IDAsString);
    writeln('  File="',NewCompilerFilename,'"');
    exit;
  end;
  if FileAge(NewCompilerFilename)<>APackage.LastCompilerFileDate then begin
    writeln('TPkgManager.CheckIfPackageNeedsCompilation  Compiler file changed for ',APackage.IDAsString);
    writeln('  File="',NewCompilerFilename,'"');
    exit;
  end;
  if NewCompilerParams<>APackage.LastCompilerParams then begin
    writeln('TPkgManager.CheckIfPackageNeedsCompilation  Compiler params changed for ',APackage.IDAsString);
    writeln('  Old="',APackage.LastCompilerParams,'"');
    writeln('  Now="',NewCompilerParams,'"');
    exit;
  end;
  
  // check package files
  if StateFileAge<FileAge(APackage.Filename) then begin
    writeln('TPkgManager.CheckIfPackageNeedsCompilation  StateFile older than lpk ',APackage.IDAsString);
    exit;
  end;
  for i:=0 to APackage.FileCount-1 do begin
    CurFile:=APackage.Files[i];
    if FileExists(CurFile.Filename)
    and (StateFileAge<FileAge(CurFile.Filename)) then begin
      writeln('TPkgManager.CheckIfPackageNeedsCompilation  Src has changed ',APackage.IDAsString,' ',CurFile.Filename);
      exit;
    end;
  end;

writeln('TPkgManager.CheckIfPackageNeedsCompilation END ',APackage.IDAsString);
  Result:=mrNo;
end;

function TPkgManager.MacroFunctionPkgSrcPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  PkgID: TLazPackageID;
  APackage: TLazPackage;
begin
  FuncData:=PReadFunctionData(Data);
  PkgID:=TLazPackageID.Create;
  Result:=false;
  if PkgID.StringToID(FuncData^.Param) then begin
    APackage:=PackageGraph.FindPackageWithID(PkgID);
    if APackage<>nil then begin
      FuncData^.Result:=APackage.SourceDirectories.CreateSearchPathFromAllFiles;
      Result:=true;
    end;
  end;
  PkgID.Free;
end;

function TPkgManager.MacroFunctionPkgUnitPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  PkgID: TLazPackageID;
  APackage: TLazPackage;
begin
  FuncData:=PReadFunctionData(Data);
  PkgID:=TLazPackageID.Create;
  Result:=false;
  if PkgID.StringToID(FuncData^.Param) then begin
    APackage:=PackageGraph.FindPackageWithID(PkgID);
    if APackage<>nil then begin
      FuncData^.Result:=APackage.GetUnitPath(false);
      Result:=true;
    end;
  end;
  PkgID.Free;
end;

function TPkgManager.MacroFunctionPkgIncPath(Data: Pointer): boolean;
var
  FuncData: PReadFunctionData;
  PkgID: TLazPackageID;
  APackage: TLazPackage;
begin
  FuncData:=PReadFunctionData(Data);
  PkgID:=TLazPackageID.Create;
  Result:=false;
  if PkgID.StringToID(FuncData^.Param) then begin
    APackage:=PackageGraph.FindPackageWithID(PkgID);
    if APackage<>nil then begin
      FuncData^.Result:=APackage.GetIncludePath(false);
      Result:=true;
    end;
  end;
  PkgID.Free;
end;

function TPkgManager.DoGetUnitRegisterInfo(const AFilename: string;
  var TheUnitName: string; var HasRegisterProc: boolean; IgnoreErrors: boolean
  ): TModalResult;
  
  function ErrorsHandled: boolean;
  begin
    if (CodeToolBoss.ErrorMessage='') or IgnoreErrors then exit;
    MainIDE.DoJumpToCodeToolBossError;
    Result:=false;
  end;
  
var
  ExpFilename: String;
  CodeBuffer: TCodeBuffer;
begin
  Result:=mrCancel;
  ExpFilename:=CleanAndExpandFilename(AFilename);
  // create default values
  TheUnitName:='';
  HasRegisterProc:=false;
  MainIDE.SaveSourceEditorChangesToCodeCache(-1);
  CodeBuffer:=CodeToolBoss.LoadFile(ExpFilename,true,false);
  if CodeBuffer<>nil then begin
    TheUnitName:=CodeToolBoss.GetSourceName(CodeBuffer,false);
    if not ErrorsHandled then exit;
    CodeToolBoss.HasInterfaceRegisterProc(CodeBuffer,HasRegisterProc);
    if not ErrorsHandled then exit;
  end;
  if TheUnitName='' then
    TheUnitName:=ExtractFileNameOnly(ExpFilename);
  Result:=mrOk;
end;

constructor TPkgManager.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  IDEComponentPalette:=TComponentPalette.Create;
  IDEComponentPalette.OnEndUpdate:=@IDEComponentPaletteEndUpdate;
  
  PkgLinks:=TPackageLinks.Create;
  PkgLinks.UpdateAll;

  PackageGraph:=TLazPackageGraph.Create;
  PackageGraph.OnChangePackageName:=@PackageGraphChangePackageName;
  PackageGraph.OnAddPackage:=@PackageGraphAddPackage;
  PackageGraph.OnDeletePackage:=@PackageGraphDeletePackage;
  PackageGraph.OnDependencyModified:=@PackageGraphDependencyModified;
  PackageGraph.OnBeginUpdate:=@PackageGraphBeginUpdate;
  PackageGraph.OnEndUpdate:=@PackageGraphEndUpdate;

  PackageEditors:=TPackageEditors.Create;
  PackageEditors.OnOpenFile:=@MainIDE.DoOpenMacroFile;
  PackageEditors.OnOpenPackage:=@OnPackageEditorOpenPackage;
  PackageEditors.OnCreateNewFile:=@OnPackageEditorCreateFile;
  PackageEditors.OnGetIDEFileInfo:=@MainIDE.GetIDEFileState;
  PackageEditors.OnGetUnitRegisterInfo:=@OnPackageEditorGetUnitRegisterInfo;
  PackageEditors.OnFreeEditor:=@OnPackageEditorFreeEditor;
  PackageEditors.OnSavePackage:=@OnPackageEditorSavePackage;
  PackageEditors.OnCompilePackage:=@OnPackageEditorCompilePackage;
  
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PKGSRCPATH',nil,@MacroFunctionPkgSrcPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PKGUNITPATH',nil,@MacroFunctionPkgUnitPath);
  CodeToolBoss.DefineTree.MacroFunctions.AddExtended(
    'PKGINCPATH',nil,@MacroFunctionPkgIncPath);

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
    itmPkgOpenPackage.OnClick :=@mnuPkgOpenPackageClicked;
    itmPkgOpenPackageFile.OnClick:=@MainIDEitmPkgOpenPackageFileClick;
    itmPkgAddCurUnitToPkg.OnClick:=@MainIDEitmPkgAddCurUnitToPkgClick;
    itmPkgPkgGraph.OnClick:=@MainIDEitmPkgPkgGraphClick;
    itmCompsConfigCustomComps.OnClick :=@mnuConfigCustomCompsClicked;
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

procedure TPkgManager.SaveSettings;
begin
  PackageEditors.SaveLayouts;
end;

function TPkgManager.GetDefaultSaveDirectoryForFile(const Filename: string
  ): string;
var
  APackage: TLazPackage;
  PkgFile: TPkgFile;
begin
  Result:='';
  PkgFile:=PackageGraph.FindFileInAllPackages(Filename,false,true);
  if PkgFile=nil then exit;
  APackage:=PkgFile.LazPackage;
  if APackage.AutoCreated or (not APackage.HasDirectory) then exit;
  Result:=APackage.Directory;
end;

procedure TPkgManager.LoadInstalledPackages;
begin
  IDEComponentPalette.BeginUpdate(true);

  // static packages
  PackageGraph.AddStaticBasePackages;
  PackageGraph.RegisterStaticPackages;
  
  // dynamic packages
  // ToDo

  IDEComponentPalette.EndUpdate;
end;

procedure TPkgManager.UpdateVisibleComponentPalette;
begin
  {$IFDEF EnablePkgs}
  TComponentPalette(IDEComponentPalette).NoteBook:=MainIDE.ComponentNotebook;
  TComponentPalette(IDEComponentPalette).UpdateNoteBookButtons;
  {$ENDIF}
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
    if not PackageGraph.PackageCanBeReplaced(ConflictPkg,APackage) then begin
      Result:=MessageDlg('Package Name already loaded',
        'There is already a package with the name "'+APackage.Name+'" loaded'#13
        +'from file "'+ConflictPkg.Filename+'".'#13
        +'See Components -> Package Graph.',
        mtError,[mbCancel,mbAbort],0);
      exit;
    end;
    
    // replace package
    PackageGraph.ReplacePackage(ConflictPkg,APackage);
  end else begin
    // add to graph
    PackageGraph.AddPackage(APackage);
  end;

  // save package file links
  PkgLinks.SaveUserLinks;

  Result:=mrOk;
end;

function TPkgManager.OpenProjectDependencies(AProject: TProject): TModalResult;
begin
  PackageGraph.OpenRequiredDependencyList(AProject.FirstRequiredDependency);
  Result:=mrOk;
end;

procedure TPkgManager.AddDefaultDependencies(AProject: TProject);
var
  ds: char;
begin
  case AProject.ProjectType of
  
  ptApplication:
    begin
      // add lcl pp/pas dirs to source search path
      ds:=PathDelim;
      AProject.CompilerOptions.SrcPath:=
        '$(LazarusDir)'+ds+'lcl'
       +';'+
        '$(LazarusDir)'+ds+'lcl'+ds+'interfaces'+ds+'$(LCLWidgetType)';
      {$IFDEF EnablePkgs}
      AddProjectLCLDependency(AProject);
      {$ELSE}
      // add lcl ppu dirs to unit search path
      Project1.CompilerOptions.OtherUnitFiles:=
        '$(LazarusDir)'+ds+'lcl'+ds+'units'
       +';'+
        '$(LazarusDir)'+ds+'lcl'+ds+'units'+ds+'$(LCLWidgetType)';
      {$ENDIF}
    end;

  end;
  OpenProjectDependencies(AProject);
end;

procedure TPkgManager.AddProjectDependency(AProject: TProject;
  APackage: TLazPackage);
var
  NewDependency: TPkgDependency;
  RequiredPackage: TLazPackage;
begin
  {$IFNDEF EnablePkgs}
  exit;
  {$ENDIF}
  // check if the dependency is already there
  if FindDependencyByNameInList(AProject.FirstRequiredDependency,pdlRequires,
    APackage.Name)<>nil
  then
    exit;
  // add a dependency for the package to the project
  NewDependency:=APackage.CreateDependencyForThisPkg;
  AProject.AddRequiredDependency(NewDependency);
  PackageGraph.OpenDependency(NewDependency,RequiredPackage);
end;

procedure TPkgManager.AddProjectRegCompDependency(AProject: TProject;
  ARegisteredComponent: TRegisteredComponent);
var
  PkgFile: TPkgFile;
begin
  {$IFNDEF EnablePkgs}
  exit;
  {$ENDIF}
  if not (ARegisteredComponent is TPkgComponent) then exit;
  PkgFile:=TPkgComponent(ARegisteredComponent).PkgFile;
  if (PkgFile=nil) or (PkgFile.LazPackage=nil) then exit;
  AddProjectDependency(AProject,PkgFile.LazPackage);
end;

procedure TPkgManager.AddProjectLCLDependency(AProject: TProject);
begin
  AddProjectDependency(AProject,PackageGraph.LCLPackage);
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
  NewPackage:=PackageGraph.CreateNewPackage('NewPackage');
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
  AlternativePkgName: String;
begin
  AFilename:=CleanAndExpandFilename(AFilename);
  
  // check file extension
  if CompareFileExt(AFilename,'.lpk',false)<>0 then begin
    Result:=MessageDlg('Invalid file extension',
      'The file "'+AFilename+'" is not a lazarus package.',
      mtError,[mbCancel,mbAbort],0);
    RemoveFromRecentList(AFilename,EnvironmentOptions.RecentPackageFiles);
    SetRecentPackagesMenu;
    exit;
  end;
  
  // check filename
  AlternativePkgName:=ExtractFileNameOnly(AFilename);
  if (AlternativePkgName='') or (not IsValidIdent(AlternativePkgName)) then
  begin
    Result:=MessageDlg('Invalid package filename',
      'The package file name "'+AlternativePkgName+'" in'#13
      +'"'+AFilename+'" is not a valid lazarus package name.',
      mtError,[mbCancel,mbAbort],0);
    RemoveFromRecentList(AFilename,EnvironmentOptions.RecentPackageFiles);
    SetRecentPackagesMenu;
    exit;
  end;

  // add to recent packages
  if pofAddToRecent in Flags then begin
    AddToRecentList(AFilename,EnvironmentOptions.RecentPackageFiles,
                    EnvironmentOptions.MaxRecentPackageFiles);
    SetRecentPackagesMenu;
  end;

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

      // newly loaded is not modified
      APackage.Modified:=false;

      // check if package name and file name correspond
      if (AnsiCompareText(AlternativePkgName,APackage.Name)<>0) then begin
        Result:=MessageDlg('Filename differs from Packagename',
          'The filename "'+ExtractFileName(AFilename)+'" does not correspond '
          +'to the package name "'+APackage.Name+'" in the file.'#13
          +'Change package name to "'+AlternativePkgName+'"?',
          mtConfirmation,[mbYes,mbCancel,mbAbort],0);
        if Result<>mrYes then exit;
        APackage.Name:=AlternativePkgName;
      end;
      
      // integrate it into the graph
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
  if psfAskBeforeSaving in Flags then begin
    Result:=MessageDlg('Save package?',
               'Package "'+APackage.IDAsString+'" changed. Save?',
               mtConfirmation,[mbYes,mbNo,mbAbort],0);
    if (Result=mrNo) then Result:=mrIgnore;
    if Result<>mrYes then exit;
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

  // delete ambigious files
  Result:=MainIDE.DoDeleteAmbigiousFiles(APackage.Filename);
  if Result=mrAbort then exit;

  // save
  try
    ClearFile(APackage.Filename,true);
    XMLConfig:=TXMLConfig.Create(APackage.Filename);
    try
      XMLConfig.Clear;
      APackage.SaveToXMLConfig(XMLConfig,'Package/');
      PkgLinks.AddUserLink(APackage);
      PkgLinks.SaveUserLinks;
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

procedure TPkgManager.DoShowPackageGraphPathList(PathList: TList);
begin
  if DoShowPackageGraph<>mrOk then exit;
  PackageGraphExplorer.ShowPath(PathList);
end;

function TPkgManager.DoCompileProjectDependencies(AProject: TProject;
  Flags: TPkgCompileFlags): TModalResult;
begin
  // check graph for circles and broken dependencies
  if not (pcfDoNotCompileDependencies in Flags) then begin
    Result:=CheckPackageGraphForCompilation(nil,
                                            AProject.FirstRequiredDependency);
    if Result<>mrOk then exit;
  end;
  
  // save all open files
  if not (pcfAutomatic in Flags) then begin
    Result:=MainIDE.DoSaveForBuild;
    if Result<>mrOk then exit;
  end;

  PackageGraph.BeginUpdate(false);
  try
    // automatically compile required packages
    if not (pcfDoNotCompileDependencies in Flags) then begin
      Result:=CompileRequiredPackages(nil,AProject.FirstRequiredDependency);
      if Result<>mrOk then exit;
    end;
  finally
    PackageGraph.EndUpdate;
  end;
  
  Result:=mrOk;
end;

function TPkgManager.DoCompilePackage(APackage: TLazPackage;
  Flags: TPkgCompileFlags): TModalResult;
var
  PkgCompileTool: TExternalToolOptions;
  CompilerFilename: String;
  CompilerParams: String;
  EffektiveCompilerParams: String;
begin
  Result:=mrCancel;
  
  writeln('TPkgManager.DoCompilePackage A ',APackage.IDAsString,' Flags=',PkgCompileFlagsToString(Flags));
  
  if APackage.AutoCreated then exit;

  // check graph for circles and broken dependencies
  if not (pcfDoNotCompileDependencies in Flags) then begin
    Result:=CheckPackageGraphForCompilation(APackage,nil);
    if Result<>mrOk then exit;
  end;
  
  // save all open files
  if not (pcfAutomatic in Flags) then begin
    Result:=MainIDE.DoSaveForBuild;
    if Result<>mrOk then exit;
  end;

  PackageGraph.BeginUpdate(false);
  try
    // automatically compile required packages
    if not (pcfDoNotCompileDependencies in Flags) then begin
      Result:=CompileRequiredPackages(APackage,nil);
      if Result<>mrOk then exit;
    end;

    // check if compilation is neccessary
    if ([pcfOnlyIfNeeded,pcfCleanCompile]*Flags<>[]) then begin
      Result:=CheckIfPackageNeedsCompilation(APackage,
                                             CompilerFilename,CompilerParams);
      if Result=mrNo then begin
        Result:=mrOk;
        exit;
      end;
      if Result<>mrYes then exit;
    end;
    
    // auto increase version
    // ToDo

    Result:=DoPreparePackageOutputDirectory(APackage);
    if Result<>mrOk then exit;

    // create package main source file
    Result:=DoSavePackageMainSource(APackage,Flags);
    if Result<>mrOk then exit;

    // create external tool to run the compiler
    writeln('TPkgManager.DoCompilePackage Compiler="',CompilerFilename,'"');
    writeln('TPkgManager.DoCompilePackage Params="',CompilerParams,'"');
    writeln('TPkgManager.DoCompilePackage WorkingDir="',APackage.Directory,'"');

    // check compiler filename
    try
      CheckIfFileIsExecutable(CompilerFilename);
    except
      on e: Exception do begin
        Result:=MessageDlg('invalid Compiler filename',
          'The compiler file for package '+APackage.IDAsString+' is not a valid executable:'#13
          +E.Message,
          mtError,[mbCancel,mbAbort],0);
        exit;
      end;
    end;
    
    // change compiler parameters for compiling clean
    EffektiveCompilerParams:=CompilerParams;
    if pcfCleanCompile in Flags then begin
      if EffektiveCompilerParams<>'' then
        EffektiveCompilerParams:='-B '+EffektiveCompilerParams
      else
        EffektiveCompilerParams:='-B';
    end;

    PkgCompileTool:=TExternalToolOptions.Create;
    try
      PkgCompileTool.Title:='Compiling package '+APackage.IDAsString;
      PkgCompileTool.ScanOutputForFPCMessages:=true;
      PkgCompileTool.ScanOutputForMakeMessages:=true;
      PkgCompileTool.WorkingDirectory:=APackage.Directory;
      PkgCompileTool.Filename:=CompilerFilename;
      PkgCompileTool.CmdLineParams:=EffektiveCompilerParams;

      // clear old errors
      SourceNotebook.ClearErrorLines;

      // compile package
      Result:=EnvironmentOptions.ExternalTools.Run(PkgCompileTool,MainIDE.MacroList);
      if Result<>mrOk then exit;
      // compilation succeded -> write state file
      Result:=DoSavePackageCompiledState(APackage,
                                         CompilerFilename,CompilerParams);
      if Result<>mrOk then exit;
    finally
      // clean up
      PkgCompileTool.Free;

      if not (pcfAutomatic in Flags) then begin
        // check for changed files on disk
        MainIDE.DoCheckFilesOnDisk;
      end;
    end;
  finally
    PackageGraph.EndUpdate;
  end;
  Result:=mrOk;
end;

function TPkgManager.DoSavePackageMainSource(APackage: TLazPackage;
  Flags: TPkgCompileFlags): TModalResult;
var
  SrcFilename: String;
  UsedUnits: String;
  Src: String;
  i: Integer;
  e: String;
  CurFile: TPkgFile;
  CodeBuffer: TCodeBuffer;
  CurUnitName: String;
  RegistrationCode: String;
  fs: TFileStream;
  HeaderSrc: String;
  OutputDir: String;
begin
  writeln('TPkgManager.DoSavePackageMainSource A');
  // check if package is ready for saving
  OutputDir:=APackage.GetOutputDirectory;
  if not DirectoryExists(OutputDir) then begin
    Result:=MessageDlg('Directory not found',
      'Package "'+APackage.IDAsString+'" has no valid output directory:'#13
      +'"'+OutputDir+'"',
      mtError,[mbCancel,mbAbort],0);
    exit;
  end;

  SrcFilename:=OutputDir+APackage.GetCompileSourceFilename;

  // backup old file
  Result:=MainIDE.DoBackupFile(SrcFilename,false);
  if Result=mrAbort then exit;

  // delete ambigious files
  Result:=MainIDE.DoDeleteAmbigiousFiles(SrcFilename);
  if Result=mrAbort then exit;

  // collect unitnames
  e:=EndOfLine;
  UsedUnits:='';
  RegistrationCode:='';
  for i:=0 to APackage.FileCount-1 do begin
    CurFile:=APackage.Files[i];
    // update unitname
    if FilenameIsPascalUnit(CurFile.Filename)
    and (CurFile.FileType=pftUnit) then begin
      CodeBuffer:=CodeToolBoss.LoadFile(CurFile.Filename,false,false);
      if CodeBuffer<>nil then begin
        // if the unit is edited, the unitname is probably already cached
        CurUnitName:=CodeToolBoss.GetCachedSourceName(CodeBuffer);
        // if not then parse it
        if AnsiCompareText(CurUnitName,CurFile.UnitName)<>0 then
          CurUnitName:=CodeToolBoss.GetSourceName(CodeBuffer,false);
        // if it makes sense, update unitname
        if AnsiCompareText(CurUnitName,CurFile.UnitName)=0 then
          CurFile.UnitName:=CurUnitName;
      end;
      CurUnitName:=CurFile.UnitName;
      if (CurUnitName<>'') and IsValidIdent(CurUnitName) then begin
        if UsedUnits<>'' then
          UsedUnits:=UsedUnits+', ';
        UsedUnits:=UsedUnits+CurUnitName;
        if CurFile.HasRegisterProc then begin
          RegistrationCode:=RegistrationCode+
            '  RegisterUnit('''+CurUnitName+''',@'+CurUnitName+'.Register);'+e;
        end;
      end;
    end;
  end;


  // create source
  HeaderSrc:=
       '{ This file was automatically created by Lazarus. Do not edit!'+e
      +'  This source is only used to compile and install'+e
      +'  the package '+APackage.IDAsString+'.'+e
      +'}'+e
      +e;
  Src:='unit '+APackage.Name+';'+e
      +e
      +'interface'+e
      +e
      +'uses'+e
      +'  '+UsedUnits+', LazarusPackageIntf;'+e
      +e
      +'implementation'+e
      +e
      +'procedure Register;'+e
      +'begin'+e
      +RegistrationCode
      +'end;'+e
      +e
      +'initialization'
      +'  RegisterPackage('''+APackage.Name+''',@Register)'
      +e
      +'end.'+e;
  Src:=CodeToolBoss.SourceChangeCache.BeautifyCodeOptions.
                  BeautifyStatement(Src,0);
  Src:=HeaderSrc+Src;

  // save source
  try
    fs:=TFileStream.Create(SrcFilename,fmCreate);
    try
      fs.Write(Src[1],length(Src));
    finally
      fs.Free;
    end;
  except
    on E: Exception do begin
      Result:=MessageDlg('Error writing file',
        'Unable to write package main source file'#13
        +'"'+SrcFilename+'".',
        mtError,[mbCancel,mbAbort],0);
      exit;
    end;
  end;

  Result:=mrOk;
end;

function TPkgManager.OnRenameFile(const OldFilename, NewFilename: string
  ): TModalResult;
var
  OldPackage: TLazPackage;
  OldPkgFile: TPkgFile;
  NewPkgFile: TPkgFile;
begin
  Result:=mrOk;
  if (OldFilename=NewFilename) or (not FilenameIsPascalUnit(NewFilename)) then
    exit;
  OldPkgFile:=PackageGraph.FindFileInAllPackages(OldFilename,false,true);
  if (OldPkgFile=nil) or (OldPkgFile.LazPackage.ReadOnly) then
    exit;
  OldPackage:=OldPkgFile.LazPackage;
  NewPkgFile:=PackageGraph.FindFileInAllPackages(NewFilename,false,true);
  if (NewPkgFile<>nil) and (OldPackage<>NewPkgFile.LazPackage) then exit;

  Result:=MessageDlg('Rename file in package?',
    'The package '+OldPackage.IDAsString+' owns the file'#13
    +'"'+OldFilename+'".'#13
    +'Should the file be renamed in the package as well?',
    mtConfirmation,[mbYes,mbNo,mbAbort],0);
  if Result=mrNo then begin
    Result:=mrOk;
    exit;
  end;
  if Result<>mrYes then exit;
  
  OldPkgFile.Filename:=NewFilename;
  if OldPackage.Editor<>nil then OldPackage.Editor.UpdateAll;
  OldPackage.Modified:=true;

  Result:=mrOk;
end;

function TPkgManager.DoAddActiveUnitToAPackage: TModalResult;
var
  ActiveSourceEditor: TSourceEditor;
  ActiveUnitInfo: TUnitInfo;
  PkgFile: TPkgFile;
  Filename: String;
  TheUnitName: String;
  HasRegisterProc: Boolean;
begin
  MainIDE.GetCurrentUnit(ActiveSourceEditor,ActiveUnitInfo);
  if ActiveSourceEditor=nil then exit;
  
  Filename:=ActiveUnitInfo.Filename;
  
  // check if filename is absolute
  if ActiveUnitInfo.IsVirtual or (not FileExists(Filename)) then begin
    Result:=MessageDlg('File not saved',
      'Please save the file before adding it to a package.',
      mtWarning,[mbCancel],0);
    exit;
  end;
  
  // check if file is part of project
  if ActiveUnitInfo.IsPartOfProject then begin
    Result:=MessageDlg('File is in Project',
      'Warning: The file "'+Filename+'"'#13
      +'belongs to the current project.'
      ,mtWarning,[mbIgnore,mbCancel,mbAbort],0);
    if Result<>mrIgnore then exit;
  end;
  
  // check if file is already in a package
  PkgFile:=PackageGraph.FindFileInAllPackages(Filename,false,true);
  if PkgFile<>nil then begin
    Result:=MessageDlg('File is already in package',
      'The file "'+Filename+'"'#13
      +'is already in the package '+PkgFile.LazPackage.IDAsString+'.',
      mtWarning,[mbIgnore,mbCancel,mbAbort],0);
    if Result<>mrIgnore then exit;
  end;
  
  TheUnitName:='';
  HasRegisterProc:=false;
  if FilenameIsPascalUnit(Filename) then begin
    Result:=DoGetUnitRegisterInfo(Filename,TheUnitName,HasRegisterProc,false);
    if Result<>mrOk then exit;
  end;
  
  Result:=ShowAddFileToAPackageDlg(Filename,TheUnitName,HasRegisterProc);
end;

function TPkgManager.OnProjectInspectorOpen(Sender: TObject): boolean;
var
  Dependency: TPkgDependency;
  RequiredPackage: TLazPackage;
begin
  Result:=false;
  if (Sender=nil) or (not (Sender is TProjectInspectorForm)) then exit;
  Dependency:=TProjectInspectorForm(Sender).GetSelectedDependency;
  if Dependency=nil then exit;
  // user has selected a dependency -> open package
  Result:=true;
  if PackageGraph.OpenDependency(Dependency,RequiredPackage)<>lprSuccess then
    exit;
  DoOpenPackage(RequiredPackage);
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
          if Result=mrIgnore then begin
            CurPackage.Flags:=CurPackage.Flags+[lpfSkipSaving];
            Result:=mrOk;
          end;
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

