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
  Classes, SysUtils, LCLProc, Forms, Controls, FileCtrl, Dialogs,
  CodeToolManager, CodeCache, Laz_XMLCfg,
  KeyMapping, EnvironmentOpts, IDEProcs, ProjectDefs, InputHistory,
  IDEDefs, UComponentManMain, PackageEditor, AddToPackageDlg, PackageDefs,
  PackageLinks, PackageSystem, ComponentReg, OpenInstalledPkgDlg,
  BasePkgManager, MainBar;

type
  TPkgManager = class(TBasePkgManager)
    function OnPackageEditorCreateFile(Sender: TObject;
      const Params: TAddToPkgResult): TModalResult;
    procedure OnPackageEditorGetUnitRegisterInfo(Sender: TObject;
      const AFilename: string; var TheUnitName: string;
      var HasRegisterProc: boolean);
    function OnPackageEditorOpenPackage(Sender: TObject; APackage: TLazPackage
      ): TModalResult;
    procedure OnPackageEditorSavePackage(Sender: TObject);
    procedure mnuConfigCustomCompsClicked(Sender: TObject);
    procedure mnuOpenInstalledPckClicked(Sender: TObject);
  private
    function DoShowSavePackageAsDialog(APackage: TLazPackage): TModalResult;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure ConnectMainBarEvents; override;
    procedure ConnectSourceNotebookEvents; override;
    procedure SetupMainBarShortCuts; override;

    procedure LoadInstalledPackages; override;

    function ShowConfigureCustomComponents: TModalResult; override;
    function DoNewPackage: TModalResult; override;
    function DoShowOpenInstalledPckDlg: TModalResult; override;
    function DoOpenPackage(APackage: TLazPackage): TModalResult; override;
    function DoSavePackage(APackage: TLazPackage;
                           Flags: TPkgSaveFlags): TModalResult; override;
  end;

implementation

{ TPkgManager }

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

procedure TPkgManager.OnPackageEditorSavePackage(Sender: TObject);
begin
  if Sender is TLazPackage then
    DoSavePackage(TLazPackage(Sender),[]);
end;

procedure TPkgManager.mnuConfigCustomCompsClicked(Sender: TObject);
begin
  ShowConfigureCustomComponents;
end;

procedure TPkgManager.mnuOpenInstalledPckClicked(Sender: TObject);
begin
  DoShowOpenInstalledPckDlg;
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
      if EnvironmentOptions.PascalFileAutoLowerCase then
        NewFileName:=ExtractFilePath(NewFilename)
                    +lowercase(ExtractFileName(NewFilename));

      // check package name conflicts
      ConflictPkg:=PackageGraph.FindAPackageWithName(NewPkgName,APackage);
      if ConflictPkg<>nil then begin
        Result:=MessageDlg('Package name already exists',
          'The package name "'+NewPkgName+'" already exists.'#13
          +'Conflict package: "'+ConflictPkg.IDAsString+'"'#13
          +'File: "'+ConflictPkg.Filename+'"'#13
          +#13
          +'It is strongly recommended to choose another name.',
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
      
      // check file name conflict with other packages
      PkgFile:=PackageGraph.FindFileInAllPackages(NewFilename,true);
      if PkgFile<>nil then begin
        Result:=MessageDlg('Filename is used by other package',
          'The file name "'+NewFilename+'" is used by'#13
          +'the package "'+PkgFile.LazPackage.IDAsString+'"'#13
          +'in file "'+PkgFile.LazPackage.Filename+'".',
          mtInformation,[mbRetry,mbAbort],0);
        if Result=mrAbort then exit;
        continue; // try again
      end;

    until Result<>mrRetry;
  finally
    InputHistories.StoreFileDialogSettings(SaveDialog);
    SaveDialog.Free;
  end;
  
  // set filename
  APackage.Filename:=NewFilename;
  
  // rename package
  if AnsiCompareText(NewPkgName,APackage.Name)=0 then begin
    // just change in case
    APackage.Name:=NewPkgName;
  end else begin
    // name change -> update package graph
    APackage.Name:=NewPkgName;
    // ToDo: update package graph
  end;
  
  // clean up old package file to reduce ambigiousities
  if FileExists(OldPkgFilename)
  and (CompareFilenames(OldPkgFilename,NewFilename)<>0) then begin
    if MessageDlg('Delete Old Package File?',
      'Delete old package file "'+OldPkgFilename+'"?',
      mtConfirmation,[mbOk,mbCancel],0)=mrOk
    then begin
      if not DeleteFile(OldPkgFilename) then begin
        MessageDlg('Delete failed',
          'Deleting of file "'+OldPkgFilename+'"'
             +' failed.',mtError,[mbOk],0);
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
  
  PackageEditors:=TPackageEditors.Create;
  PackageEditors.OnOpenFile:=@MainIDE.DoOpenMacroFile;
  PackageEditors.OnOpenPackage:=@OnPackageEditorOpenPackage;
  PackageEditors.OnCreateNewFile:=@OnPackageEditorCreateFile;
  PackageEditors.OnGetIDEFileInfo:=@MainIDE.GetIDEFileState;
  PackageEditors.OnGetUnitRegisterInfo:=@OnPackageEditorGetUnitRegisterInfo;
  PackageEditors.OnSavePackage:=@OnPackageEditorSavePackage;
end;

destructor TPkgManager.Destroy;
begin
  FreeThenNil(PackageEditors);
  FreeThenNil(PackageGraph);
  FreeThenNil(PkgLinks);
  FreeThenNil(IDEComponentPalette);
  inherited Destroy;
end;

procedure TPkgManager.ConnectMainBarEvents;
begin
  with MainIDE do begin
    itmCompsConfigCustomComps.OnClick :=@mnuConfigCustomCompsClicked;
    itmOpenInstalledPkg.OnClick :=@mnuOpenInstalledPckClicked;
  end;
end;

procedure TPkgManager.ConnectSourceNotebookEvents;
begin

end;

procedure TPkgManager.SetupMainBarShortCuts;
begin

end;

procedure TPkgManager.LoadInstalledPackages;
begin
  // base packages
  PackageGraph.AddStaticBasePackages;
  
  PackageGraph.RegisterStaticPackages;
  // custom packages
  // ToDo
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
  CurEditor.Show;
  Result:=mrOk;
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
  MainIDE.SaveSourceEditorChangesToCodeCache(-1);

  if APackage.IsVirtual then Include(Flags,psfSaveAs);

  // check if package needs saving
  if (not (psfSaveAs in Flags)) and (not APackage.ReadOnly)
  and (not APackage.Modified)
  and FileExists(APackage.Filename) then begin
    Result:=mrOk;
    exit;
  end;

  // save package
  if (psfSaveAs in Flags) then begin
    Result:=DoShowSavePackageAsDialog(APackage);
    if Result<>mrOk then exit;
  end;
  
  // save
  Result:=mrCancel;
  try
    XMLConfig:=TXMLConfig.Create(APackage.Filename);
    APackage.SaveToXMLConfig(XMLConfig,'Package/');
    XMLConfig.Flush;
    XMLConfig.Free;
  except
    on E: Exception do begin
      Result:=MessageDlg('Error Writing Package',
        'Unable to write package "'+APackage.IDAsString+'"'#13
        +'to file "'+APackage.Filename+'".',
        mtError,[mbAbort,mbCancel],0);
      exit;
    end;
  end;
  
  // success
  APackage.Modified:=false;
  Result:=mrOk;
end;

end.

