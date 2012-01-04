{  $Id$  }
{
 /***************************************************************************
                            addtopackagedlg.pas
                            -------------------


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
    TAddToPackageDlg is the form for adding files to an open package.
}
unit AddToPackageDlg;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LCLProc, LCLType, Forms, Controls, Buttons, ExtDlgs,
  StdCtrls, ExtCtrls, Dialogs, FileUtil, ComCtrls, AVL_Tree,
  // IDEIntf
  NewItemIntf, ProjectIntf, PackageIntf, FormEditingIntf, IDEWindowIntf,
  IDEDialogs,
  // IDE
  LazarusIDEStrConsts, InputHistory, CodeToolManager, IDEDefs,
  IDEProcs, EnvironmentOpts, PackageSystem, PackageDefs, ComponentReg,
  AddDirToPkgDlg;
  
type
  TAddToPkgType = (
    d2ptUnit,
    d2ptVirtualUnit,
    d2ptNewComponent,
    d2ptRequiredPkg,
    d2ptFile,
    d2ptFiles,
    d2ptNewFile
    );
    
  { TAddToPkgResult }

  TAddToPkgResult = class
  public
    Pkg: TLazPackage;
    AddType: TAddToPkgType;
    Dependency: TPkgDependency;
    UnitFilename: string;
    Unit_Name: string;
    AncestorType: string;
    NewClassName: string;
    PageName: string;
    FileType: TPkgFileType;
    PkgFileFlags: TPkgFileFlags;
    UsedUnitname: string;
    IconFile: string;
    AutoAddLFMFile: boolean;
    AutoAddLRSFile: boolean;
    NewItem: TNewIDEItemTemplate;
    Next: TAddToPkgResult;
    procedure Clear;
    destructor Destroy; override;
  end;
  
  TOnGetUnitRegisterInfo = procedure(Sender: TObject; const AFilename: string;
    var TheUnitName: string; var HasRegisterProc: boolean) of object;

  { TAddToPackageDlg }

  TAddToPackageDlg = class(TForm)
    CancelDependButton: TBitBtn;
    CancelNewComponentButton: TBitBtn;
    // notebook
    NoteBook: TPageControl;
    NewFilePage: TTabSheet;
    NewComponentPage: TTabSheet;
    NewRequirementPage: TTabSheet;
    AddFilesPage: TTabSheet;
    // new file page
    NewFileTreeView: TTreeView;
    NewFileDescriptionGroupBox: TGroupBox;
    NewFileHelpLabel: TLabel;
    NewFileCancelButton: TBitBtn;
    NewFileOkButton: TBitBtn;
    // new component page
    NewCompBtnPanel: TPanel;
    NewComponentButton: TBitBtn;
    AncestorTypeLabel: TLabel;
    AncestorComboBox: TComboBox;
    AncestorShowAllCheckBox: TCheckBox;
    ClassNameLabel: TLabel;
    ClassNameEdit: TEdit;
    PalettePageLabel: TLabel;
    PalettePageCombobox: TCombobox;
    ComponentIconLabel: TLabel;
    ComponentIconSpeedButton: TSpeedButton;
    ComponentUnitFileLabel: TLabel;
    ComponentUnitFileEdit: TEdit;
    ComponentUnitFileBrowseButton: TButton;
    ComponentUnitFileShortenButton: TButton;
    ComponentUnitNameLabel: TLabel;
    ComponentUnitNameEdit: TEdit;
    // new required package
    NewDepBtnPanel: TPanel;
    NewDependButton: TBitBtn;
    NewDepPanel: TPanel;
    DependPkgNameLabel: TLabel;
    DependPkgNameComboBox: TComboBox;
    DependMinVersionLabel: TLabel;
    DependMinVersionEdit: TEdit;
    DependMaxVersionLabel: TLabel;
    DependMaxVersionEdit: TEdit;
    // add files page
    FilesAddButton: TButton;
    FilesDirButton: TButton;
    FilesDeleteButton: TBitBtn;
    FilesShortenButton: TBitBtn;
    FilesListView: TListView;
    FilesBrowseButton: TButton;
    NewFileBtnPanel: TPanel;
    AddFilesBtnPanel: TPanel;
    procedure AddToPackageDlgClose(Sender: TObject;
                                   var CloseAction: TCloseAction);
    procedure AddToPackageDlgKeyDown(Sender: TObject; var Key: Word;
                                     Shift: TShiftState);
    procedure AncestorComboBoxCloseUp(Sender: TObject);
    procedure AncestorShowAllCheckBoxClick(Sender: TObject);
    procedure CancelAddFileButtonClick(Sender: TObject);
    procedure CancelAddUnitButtonClick(Sender: TObject);
    procedure CancelNewComponentButtonClick(Sender: TObject);
    procedure ClassNameEditChange(Sender: TObject);
    procedure ComponentIconSpeedButtonClick(Sender: TObject);
    procedure ComponentUnitFileBrowseButtonClick(Sender: TObject);
    procedure ComponentUnitFileShortenButtonClick(Sender: TObject);
    procedure FilesAddButtonClick(Sender: TObject);
    procedure FilesBrowseButtonClick(Sender: TObject);
    procedure FilesDeleteButtonClick(Sender: TObject);
    procedure FilesDirButtonClick(Sender: TObject);
    procedure FilesShortenButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewComponentButtonClick(Sender: TObject);
    procedure NewComponentPageResize(Sender: TObject);
    procedure NewDependButtonClick(Sender: TObject);
    procedure NewFileOkButtonClick(Sender: TObject);
    procedure NewFilePageResize(Sender: TObject);
    procedure NewFileTreeViewClick(Sender: TObject);
    procedure NewFileTreeViewDblClick(Sender: TObject);
    procedure NewFileTreeViewSelectionChanged(Sender: TObject);
  private
    fLastNewComponentAncestorType: string;
    fLastNewComponentClassName: string;
    FLazPackage: TLazPackage;
    FOnGetIDEFileInfo: TGetIDEFileStateEvent;
    FOnGetUnitRegisterInfo: TOnGetUnitRegisterInfo;
    fPkgComponents: TAVLTree;// tree of TPkgComponent
    fPackages: TAVLTree;// tree of  TLazPackage or TPackageLink
    FComponentIconFilename: string;
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure SetupComponents;
    procedure SetupNewFilePage;
    procedure SetupNewComponentPage;
    procedure SetupAddDependencyPage;
    procedure SetupAddFilesPage;
    procedure OnIterateComponentClasses(PkgComponent: TPkgComponent);
    procedure OnIteratePackages(APackageID: TLazPackageID);
    procedure AutoCompleteNewComponent;
    procedure AutoCompleteNewComponentUnitName;
    function SwitchRelativeAbsoluteFilename(const Filename: string): string;
    procedure FillNewFileTreeView;
    function FindFileInFilesList(AFilename: string): Integer;
    procedure LoadComponentIcon(AFilename: string);
  public
    Params: TAddToPkgResult;
    procedure UpdateAvailableAncestorTypes;
    procedure UpdateAvailablePageNames;
    procedure UpdateAvailableDependencyNames;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
    property OnGetIDEFileInfo: TGetIDEFileStateEvent read FOnGetIDEFileInfo
                                                     write FOnGetIDEFileInfo;
    property OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo
                       read FOnGetUnitRegisterInfo write FOnGetUnitRegisterInfo;
  end;
  
function ShowAddToPackageDlg(Pkg: TLazPackage; var Params: TAddToPkgResult;
  OnGetIDEFileInfo: TGetIDEFileStateEvent;
  OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo): TModalResult;
function CheckAddingUnitFilename(LazPackage: TLazPackage;
  AddFileType: TAddToPkgType; OnGetIDEFileInfo: TGetIDEFileStateEvent;
  var AFilename: string): boolean;
function CheckAddingDependency(LazPackage: TLazPackage;
  NewDependency: TPkgDependency; Quiet, WarnIfAlreadyThere: boolean
  ): TModalResult;// mrOk=can be added, mrCancel=do not add, mrIgnore=already there
function CheckAddingPkgDependency(LazPackage: TLazPackage;
  RequiredPkg: TLazPackage; Quiet, WarnIfAlreadyThere: boolean): TModalResult;


implementation

{$R *.lfm}

function ShowAddToPackageDlg(Pkg: TLazPackage; var Params: TAddToPkgResult;
  OnGetIDEFileInfo: TGetIDEFileStateEvent;
  OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo): TModalResult;
var
  AddDlg: TAddToPackageDlg;
begin
  AddDlg:=TAddToPackageDlg.Create(nil);
  AddDlg.OnGetIDEFileInfo:=OnGetIDEFileInfo;
  AddDlg.OnGetUnitRegisterInfo:=OnGetUnitRegisterInfo;
  AddDlg.LazPackage:=Pkg;
  Result:=AddDlg.ShowModal;
  if Result=mrOk then begin
    Params:=AddDlg.Params;
    AddDlg.Params:=nil;
  end;
  AddDlg.Free;
end;

function CheckAddingUnitFilename(LazPackage: TLazPackage;
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
        Format(lisA2PTheFilenameIsAmbiguousPleaseSpecifiyAFilename,
               ['"', AFilename, '"', #13]),
        mtError,[mbCancel]);
      exit;
    end;
  end;

  // check if file exists
  if (FilenameIsAbsolute(AFilename)) then begin
    if (not FileExistsUTF8(AFilename)) then begin
      if AddFileType=d2ptUnit then begin
        IDEMessageDialog(lisFileNotFound,
          Format(lisPkgMangFileNotFound, ['"', AFilename, '"']),
          mtError, [mbCancel]);
        exit;
      end;
    end;
  end;

  // check if file already exists in package
  if FilenameIsAbsolute(AFilename) then begin
    PkgFile:=LazPackage.FindPkgFile(AFilename,true,false);
    if PkgFile<>nil then begin
      Msg:=Format(lisA2PFileAlreadyExistsInThePackage, ['"',AFilename,'"']);
      if PkgFile.Filename<>AFilename then
        Msg:=Msg+LineEnding+Format(lisA2PExistingFile2, ['"',PkgFile.Filename,'"']);
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
          Format(lisA2PTheFileIsPartOfTheCurrentProjectItIsABadIdea,
                 ['"', AFilename, '"', #13]),
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
        Format(lisA2PisNotAValidUnitName, ['"', AnUnitName, '"']),
        mtWarning,[mbCancel]);
      exit;
    end;

    // check if unitname already exists in package
    PkgFile:=LazPackage.FindUnit(AnUnitName,true,PkgFile);
    if PkgFile<>nil then begin
      // a unit with this name already exists in this package => warn
      if IDEMessageDialog(lisA2PUnitnameAlreadyExists,
        Format(lisA2PTheUnitnameAlreadyExistsInThisPackage, ['"',AnUnitName,'"']),
        mtError,[mbCancel,mbIgnore])<>mrIgnore then exit;
    end else begin
      PkgFile:=PackageGraph.FindUnit(LazPackage,AnUnitName,true,true);
      if (PkgFile<>nil) and (PkgFile.LazPackage<>LazPackage) then begin
        // there is already a unit with this name in another package => warn
        if IDEMessageDialog(lisA2PUnitnameAlreadyExists,
          Format(lisA2PTheUnitnameAlreadyExistsInThePackage,
                 ['"', AnUnitName, '"', #13, PkgFile.LazPackage.IDAsString]),
          mtWarning,[mbCancel,mbIgnore])<>mrIgnore then exit;
      end;
    end;

    // check if unitname is a componentclass
    if IDEComponentPalette.FindComponent(AnUnitName)<>nil then begin
      if IDEMessageDialog(lisA2PAmbiguousUnitName,
        Format(lisA2PTheUnitNameIsTheSameAsAnRegisteredComponent,
               ['"', AnUnitName, '"', #13]),
        mtWarning,[mbCancel,mbIgnore])<>mrIgnore
      then
        exit;
    end;
  end;

  // ok
  Result:=true;
end;

function CheckAddingDependency(LazPackage: TLazPackage;
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

  // check packagename
  if (NewPkgName='') or (not IsValidUnitName(NewPkgName)) then begin
    if not Quiet then
      IDEMessageDialog(lisProjAddInvalidPackagename,
        Format(lisA2PThePackageNameIsInvalidPleaseChooseAnExisting, ['"', NewPkgName, '"', #13]),
        mtError,[mbCancel]);
    exit(mrCancel);
  end;

  // check if package is already required
  if (CompareText(NewPkgName,LazPackage.Name)=0)
  or (LazPackage.FindDependencyByName(NewPkgName)<>nil) then begin
    if WarnIfAlreadyThere then
      IDEMessageDialog(lisProjAddDependencyAlreadyExists,
        Format(lisA2PThePackageHasAlreadyADependencyForThe, ['"', NewPkgName, '"']),
        mtError,[mbCancel]);
    exit(mrIgnore);
  end;

  // check if required lpk exists
  if not PackageGraph.DependencyExists(NewDependency,fpfSearchAllExisting)
  then begin
    if not Quiet then
      IDEMessageDialog(lisProjAddPackageNotFound,
        Format(lisA2PNoPackageFoundForDependencyPleaseChooseAnExisting,
               ['"', NewDependency.AsString, '"', #13]),
        mtError,[mbCancel]);
    exit(mrCancel);
  end;

  RequiredPackage:=PackageGraph.FindPackageWithName(NewPkgName,nil);
  if RequiredPackage<>nil then begin
    // check if there is a dependency, that requires another version
    ConflictDependency:=PackageGraph.FindConflictRecursively(
      LazPackage.FirstRequiredDependency,RequiredPackage);
    if ConflictDependency<>nil then begin
      DebugLn(['CheckAddingDependency ',LazPackage.Name,' requiring ',RequiredPackage.IDAsString,' conflicts with ',ConflictDependency.AsString]);
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
        DebugLn(['CheckAddingDependency ',LazPackage.Name,' requiring ',RequiredPackage.IDAsString,' creates cycles with ',s]);
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
    DebugLn(['CheckAddingDependency ',LazPackage.Name,' requiring ',NewPkgName,', but is already provided by ',ProvidingAPackage.IDAsString]);
    if WarnIfAlreadyThere then
      IDEMessageDialog(lisProjAddDependencyAlreadyExists,
        Format(lisUnableToAddTheDependencyBecauseThePackageHasAlread2, [
          RequiredPackage.IDAsString, LazPackage.Name, ProvidingAPackage.Name]),
        mtError,[mbCancel]);
    exit(mrIgnore);
  end;

  Result:=mrOk;
end;

function CheckAddingPkgDependency(LazPackage: TLazPackage;
  RequiredPkg: TLazPackage; Quiet, WarnIfAlreadyThere: boolean): TModalResult;
var
  NewDependency: TPkgDependency;
begin
  NewDependency:=TPkgDependency.Create;
  try
    NewDependency.PackageName:=RequiredPkg.Name;
    Result:=CheckAddingDependency(LazPackage,NewDependency,Quiet,WarnIfAlreadyThere);
  finally
    NewDependency.Free;
  end;
end;

{ TAddToPackageDlg }

procedure TAddToPackageDlg.AddToPackageDlgClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddToPackageDlg.AddToPackageDlgKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key=VK_ESCAPE) and (Shift=[]) then
    ModalResult:=mrCancel;
end;

procedure TAddToPackageDlg.AncestorComboBoxCloseUp(Sender: TObject);
begin
  if fLastNewComponentAncestorType<>AncestorComboBox.Text then
    AutoCompleteNewComponent;
end;

procedure TAddToPackageDlg.AncestorShowAllCheckBoxClick(Sender: TObject);
begin
  UpdateAvailableAncestorTypes;
end;

procedure TAddToPackageDlg.CancelAddFileButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TAddToPackageDlg.CancelAddUnitButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TAddToPackageDlg.CancelNewComponentButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TAddToPackageDlg.ClassNameEditChange(Sender: TObject);
begin
  AutoCompleteNewComponentUnitName;
end;

procedure TAddToPackageDlg.ComponentIconSpeedButtonClick(Sender: TObject);
var
  Dlg: TOpenPictureDialog;
begin
  Dlg:=TOpenPictureDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(Dlg);
    Dlg.InitialDir:=LazPackage.GetFileDialogInitialDir(ExtractFilePath(ComponentUnitFileEdit.Text));
    Dlg.Title := 'Choose a component icon 24x24';
    Dlg.Options := Dlg.Options+[ofPathMustExist];
    Dlg.Filter:='PNG|*.png'
              +'|Bitmap, bmp|*.bmp'
              +'|Pixmap, xpm|*.xpm'
              +'|'+lisAllFiles+'|'+GetAllFilesMask;
    if Dlg.Execute then begin
      LoadComponentIcon(Dlg.FileName);
    end;
    InputHistories.StoreFileDialogSettings(Dlg);
  finally
    Dlg.Free;
  end;
end;

procedure TAddToPackageDlg.ComponentUnitFileBrowseButtonClick(Sender: TObject);
var
  SaveDialog: TSaveDialog;
  AFilename: string;
begin
  SaveDialog:=TSaveDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(SaveDialog);
    SaveDialog.InitialDir :=
      LazPackage.GetFileDialogInitialDir(SaveDialog.InitialDir);
    SaveDialog.Title := lisMenuSaveAs;
    SaveDialog.Options := SaveDialog.Options+[ofPathMustExist];
    if SaveDialog.Execute then begin
      AFilename := CleanAndExpandFilename(SaveDialog.Filename);
      if FilenameIsPascalUnit(AFilename) then begin
        LazPackage.ShortenFilename(AFilename,true);
        ComponentUnitFileEdit.Text := AFilename;
      end else begin
        IDEMessageDialog(lisA2PInvalidFile,
         lisA2PAPascalUnitMustHaveTheExtensionPPOrPas,
         mtError,[mbCancel]);
      end;
    end;
    InputHistories.StoreFileDialogSettings(SaveDialog);
  finally
    SaveDialog.Free;
  end;
end;

procedure TAddToPackageDlg.ComponentUnitFileShortenButtonClick(Sender: TObject);
begin
  if ''=ComponentUnitFileEdit.Text then exit;
  ComponentUnitFileEdit.Text:=
    SwitchRelativeAbsoluteFilename(ComponentUnitFileEdit.Text);
end;

procedure TAddToPackageDlg.FilesAddButtonClick(Sender: TObject);
var
  i: Integer;
  Filename: String;
  NewFileType: TPkgFileType;
  HasRegisterProc: boolean;
  CurParams, LastParams: TAddToPkgResult;
  ok: Boolean;
begin
  ok:=false;
  try
    LastParams:=nil;
    i:=0;
    while i<FilesListView.Items.Count do begin
      Filename:=FilesListView.Items[i].Caption;
      LazPackage.LongenFilename(Filename);

      // skip directories
      if DirPathExists(Filename) then begin
        FilesListView.Items.Delete(i);
        continue;
      end;
      
      // skip not existing files
      if (not FileExistsUTF8(Filename)) then begin
        if QuestionDlg(lisFileNotFound,
          Format(lisPkgMangFileNotFound, ['"', Filename, '"']),
          mtError,[mrIgnore,mrCancel],0)<>mrIgnore
        then
          exit;
        FilesListView.Items.Delete(i);
        continue;
      end;

      NewFileType:=FileNameToPkgFileType(Filename);

      if LazPackage.FindPkgFile(Filename,true,false)<>nil then begin
        // file already in package
        FilesListView.Items.Delete(i);
        continue;
      end;

      if LastParams<>nil then begin
        LastParams.Next:=TAddToPkgResult.Create;
        CurParams:=LastParams.Next;
      end else
        CurParams:=Params;

      CurParams.Clear;
      CurParams.AddType:=d2ptFile;
      CurParams.UnitFilename:=Filename;
      CurParams.FileType:=NewFileType;

      if NewFileType=pftUnit then begin
        CurParams.AddType:=d2ptUnit;
        Include(CurParams.PkgFileFlags,pffAddToPkgUsesSection);

        // check filename
        if not CheckAddingUnitFilename(LazPackage,CurParams.AddType,
          OnGetIDEFileInfo,CurParams.UnitFilename)
        then begin
          FilesListView.Items.Delete(i);
          exit;
        end;

        CurParams.AutoAddLFMFile:=true;
        CurParams.AutoAddLRSFile:=true;
        if Assigned(OnGetUnitRegisterInfo) then begin
          OnGetUnitRegisterInfo(Self,Filename,CurParams.Unit_Name,HasRegisterProc);
          if HasRegisterProc then
            Include(CurParams.PkgFileFlags,pffHasRegisterProc);
        end;

        // check unitname
        if CompareText(CurParams.Unit_Name,
          ExtractFileNameOnly(CurParams.UnitFilename))<>0
        then begin
          if IDEMessageDialog(lisA2PInvalidUnitName,
              Format(lisA2PTheUnitNameAndFilenameDiffer, ['"',
                CurParams.Unit_Name, '"', #13, '"', CurParams.UnitFilename, '"']),
            mtError,[mbIgnore,mbCancel])<>mrIgnore
          then begin
            FilesListView.Items.Delete(i);
            exit;
          end;
        end;
      end;
      LastParams:=CurParams;
      inc(i);
    end;
    FilesAddButton.Enabled:=FilesListView.Items.Count>0;
    ok:=LastParams<>nil;
  finally
    if not ok then Params.Clear;
  end;
  if LastParams=nil then begin
    exit;
  end;
  ModalResult:=mrOk;
end;

procedure TAddToPackageDlg.FilesBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
  i: Integer;
  NewListItem: TListItem;
  NewPgkFileType: TPkgFileType;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.InitialDir:=
      LazPackage.GetFileDialogInitialDir(OpenDialog.InitialDir);
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options
                          +[ofFileMustExist,ofPathMustExist,ofAllowMultiSelect];
    OpenDialog.Filter:=dlgAllFiles+' ('+GetAllFilesMask+')|'+GetAllFilesMask
                 +'|'+lisLazarusUnit+' (*.pas;*.pp)|*.pas;*.pp'
                 +'|'+lisLazarusProject+' (*.lpi)|*.lpi'
                 +'|'+lisLazarusForm+' (*.lfm;*.dfm)|*.lfm;*.dfm'
                 +'|'+lisLazarusPackage+' (*.lpk)|*.lpk'
                 +'|'+lisLazarusProjectSource+' (*.lpr)|*.lpr';
    if OpenDialog.Execute then begin
      for i:=0 to OpenDialog.Files.Count-1 do begin
        AFilename:=CleanAndExpandFilename(OpenDialog.Files[i]);
        if FileExistsUTF8(AFilename) then begin
          if FindFileInFilesList(AFilename)<0 then begin
            LazPackage.ShortenFilename(AFilename,true);
            NewListItem:=FilesListView.Items.Add;
            NewListItem.Caption:=AFilename;
            NewPgkFileType:=FileNameToPkgFileType(AFilename);
            NewListItem.SubItems.Add(GetPkgFileTypeLocalizedName(NewPgkFileType));
          end;
        end;
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
    FilesAddButton.Enabled:=FilesListView.Items.Count>0;
  finally
    OpenDialog.Free;
  end;
end;

procedure TAddToPackageDlg.FilesDeleteButtonClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=FilesListView.Items.Count-1 downto 0 do
    if FilesListView.Items[i].Selected then
      FilesListView.Items.Delete(i);
  FilesAddButton.Enabled:=FilesListView.Items.Count>0;
end;

procedure TAddToPackageDlg.FilesDirButtonClick(Sender: TObject);
var
  i: Integer;
  Files: TStrings;
  AFilename: string;
  NewListItem: TListItem;
  NewPgkFileType: TPkgFileType;
begin
  Files:=nil;
  try
    if ShowAddDirToPkgDialog(LazPackage,Files)<>mrOk then exit;
    for i:=0 to Files.Count-1 do begin
      AFilename:=Files[i];
      if FindFileInFilesList(AFilename)<0 then begin
        LazPackage.ShortenFilename(AFilename,true);
        NewListItem:=FilesListView.Items.Add;
        NewListItem.Caption:=AFilename;
        NewPgkFileType:=FileNameToPkgFileType(AFilename);
        NewListItem.SubItems.Add(GetPkgFileTypeLocalizedName(NewPgkFileType));
      end;
    end;
    FilesAddButton.Enabled:=FilesListView.Items.Count>0;
  finally
    Files.Free;
  end;
end;

procedure TAddToPackageDlg.FilesShortenButtonClick(Sender: TObject);
var
  SwitchToAbsolute: Boolean;
  i: Integer;
  Filename: String;
begin
  if FilesListView.Items.Count=0 then exit;
  if (not LazPackage.HasDirectory)
  or (not FilenameIsAbsolute(LazPackage.Directory)) then exit;
  SwitchToAbsolute:=not FilenameIsAbsolute(FilesListView.Items[0].Caption);
  for i:=0 to FilesListView.Items.Count-1 do begin
    Filename:=FilesListView.Items[i].Caption;
    if SwitchToAbsolute then
      Filename:=CreateAbsoluteSearchPath(Filename,LazPackage.Directory)
    else
      Filename:=CreateRelativePath(Filename,LazPackage.Directory);
    FilesListView.Items[i].Caption:=Filename;
  end;
end;

procedure TAddToPackageDlg.FormCreate(Sender: TObject);
begin
  Caption:=lisA2PAddToPackage;
  fPkgComponents:=TAVLTree.Create(@CompareIDEComponentByClassName);
  fPackages:=TAVLTree.Create(@CompareLazPackageID);
  Params:=TAddToPkgResult.Create;
  IDEDialogLayoutList.ApplyLayout(Self,500,300);
  SetupComponents;
end;

procedure TAddToPackageDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fPkgComponents);
  FreeAndNil(fPackages);
  FreeAndNil(Params);
end;

procedure TAddToPackageDlg.NewComponentButtonClick(Sender: TObject);
var
  PkgFile: TPkgFile;
  PkgComponent: TPkgComponent;
  ARequiredPackage: TLazPackage;
begin
  Params.Clear;
  Params.AddType:=d2ptNewComponent;
  Params.FileType:=pftUnit;
  Params.PkgFileFlags:=[pffHasRegisterProc,pffAddToPkgUsesSection];
  Params.AncestorType:=AncestorComboBox.Text;
  Params.NewClassName:=ClassNameEdit.Text;
  Params.PageName:=PalettePageCombobox.Text;
  Params.Unit_Name:=ComponentUnitNameEdit.Text;
  Params.UnitFilename:=ComponentUnitFileEdit.Text;
  Params.UsedUnitname:='';
  Params.IconFile:=FComponentIconFilename;

  // check Ancestor Type
  if not IsValidIdent(Params.AncestorType) then begin
    IDEMessageDialog(lisA2PInvalidAncestorType,
      Format(lisA2PTheAncestorTypeIsNotAValidPascalIdentifier, ['"',
        Params.AncestorType, '"']),
      mtError,[mbCancel]);
    exit;
  end;

  // check pagename
  if length(Params.PageName)>100 then begin
    IDEMessageDialog(lisA2PPageNameTooLong,
      Format(lisA2PThePageNameIsTooLongMax100Chars, ['"', Params.PageName, '"']
        ),
      mtError,[mbCancel]);
    exit;
  end;

  // check unitname - filename redundancy
  if AnsiCompareText(Params.Unit_name,ExtractFileNameOnly(Params.UnitFilename))<>0
  then begin
    IDEMessageDialog(lisA2PUnitNameInvalid,
      Format(lisA2PTheUnitNameDoesNotCorrespondToTheFilename, ['"',
        Params.Unit_Name, '"']),
      mtError,[mbCancel]);
    exit;
  end;

  // check classname
  if not IsValidIdent(Params.NewClassName) then begin
    IDEMessageDialog(lisA2PInvalidClassName,
      Format(lisA2PTheClassNameIsNotAValidPascalIdentifier, ['"',
        Params.NewClassName, '"']),
      mtError,[mbCancel]);
    exit;
  end;

  // check classname<>ancestortype
  if AnsiCompareText(Params.NewClassName,Params.AncestorType)=0 then begin
    IDEMessageDialog(lisA2PInvalidCycle,
      Format(lisA2PTheClassNameAndAncestorTypeAreTheSame, ['"',
        Params.NewClassName, '"', '"', Params.AncestorType, '"']),
      mtError,[mbCancel]);
    exit;
  end;

  // check ancestor type is not unitname
  PkgFile:=PackageGraph.FindUnit(LazPackage,Params.AncestorType,true,true);
  if PkgFile<>nil then begin
    if IDEMessageDialog(lisA2PAmbiguousAncestorType,
      Format(lisA2PTheAncestorTypeHasTheSameNameAsTheUnit, ['"',
        Params.AncestorType, '"', #13, '"', PkgFile.Filename, '"']),
      mtError,[mbCancel,mbIgnore])<>mrIgnore
    then
      exit;
  end;

  // check classname does not interfere with an existing unitname
  PkgFile:=PackageGraph.FindUnit(LazPackage,Params.NewClassName,true,true);
  if PkgFile<>nil then begin
    if IDEMessageDialog(lisA2PAmbiguousClassName,
      Format(lisA2PTheClassNameHasTheSameNameAsTheUnit, ['"',
        Params.AncestorType, '"', #13, '"', PkgFile.Filename, '"']),
      mtError,[mbCancel,mbIgnore])<>mrIgnore
    then
      exit;
  end;

  // check if classname already exists
  PkgComponent:=
    TPkgComponent(IDEComponentPalette.FindComponent(Params.NewClassname));
  if PkgComponent<>nil then begin
    if IDEMessageDialog(lisA2PClassNameAlreadyExists,
      Format(lisA2PTheClassNameExistsAlreadyInPackageFile, ['"',
        Params.NewClassName, '"', #13, PkgComponent.PkgFile.LazPackage.IDAsString,
        #13, '"', PkgComponent.PkgFile.Filename, '"']),
      mtError,[mbCancel,mbIgnore])<>mrIgnore
    then
      exit;
  end;

  // check filename
  if not CheckAddingUnitFilename(LazPackage,Params.AddType,
    OnGetIDEFileInfo,Params.UnitFilename) then exit;

  // create dependency if needed
  PkgComponent:=
    TPkgComponent(IDEComponentPalette.FindComponent(Params.AncestorType));
  if PkgComponent<>nil then begin
    Params.UsedUnitname:=PkgComponent.GetUnitName;
    ARequiredPackage:=PkgComponent.PkgFile.LazPackage;
    ARequiredPackage:=TLazPackage(
           PackageEditingInterface.RedirectPackageDependency(ARequiredPackage));

    if (LazPackage<>ARequiredPackage)
    and (not LazPackage.Requires(PkgComponent.PkgFile.LazPackage))
    then
      Params.Dependency:=ARequiredPackage.CreateDependencyWithOwner(nil);
  end;

  ModalResult:=mrOk;
end;

procedure TAddToPackageDlg.NewComponentPageResize(Sender: TObject);
var
  x: Integer;
begin
  x:=0;
  x:=Max(x,AncestorTypeLabel.Left+AncestorTypeLabel.Width);
  x:=Max(x,ClassNameLabel.Left+ClassNameLabel.Width);
  x:=Max(x,PalettePageLabel.Left+PalettePageLabel.Width);
  x:=Max(x,ComponentUnitFileLabel.Left+ComponentUnitFileLabel.Width);
  x:=Max(x,ComponentUnitNameLabel.Left+ComponentUnitNameLabel.Width);
  x:=Max(x,ComponentIconLabel.Left+ComponentIconLabel.Width);
  AncestorComboBox.Left:=x+6;
end;

procedure TAddToPackageDlg.NewDependButtonClick(Sender: TObject);
var
  NewDependency: TPkgDependency;
begin
  Params.Clear;
  Params.AddType:=d2ptRequiredPkg;
  
  NewDependency:=TPkgDependency.Create;
  try
    // check minimum version
    if DependMinVersionEdit.Text<>'' then begin
      if not NewDependency.MinVersion.ReadString(DependMinVersionEdit.Text) then
      begin
        IDEMessageDialog(lisProjAddInvalidVersion,
          Format(lisA2PTheMinimumVersionIsInvalidPleaseUseTheFormatMajor, ['"',
            DependMinVersionEdit.Text, '"', #13, #13]),
          mtError,[mbCancel]);
        exit;
      end;
      NewDependency.Flags:=NewDependency.Flags+[pdfMinVersion];
    end;
    // check maximum version
    if DependMaxVersionEdit.Text<>'' then begin
      if not NewDependency.MaxVersion.ReadString(DependMaxVersionEdit.Text) then
      begin
        IDEMessageDialog(lisProjAddInvalidVersion,
          Format(lisA2PTheMaximumVersionIsInvalidPleaseUseTheFormatMajor, ['"',
            DependMaxVersionEdit.Text, '"', #13, #13]),
          mtError,[mbCancel]);
        exit;
      end;
      NewDependency.Flags:=NewDependency.Flags+[pdfMaxVersion];
    end;
    
    NewDependency.PackageName:=DependPkgNameComboBox.Text;
    if CheckAddingDependency(LazPackage,NewDependency,false,true)<>mrOk then exit;

    // ok
    Params.Dependency:=NewDependency;
    NewDependency:=nil;

    ModalResult:=mrOk;
  finally
    NewDependency.Free;
  end;
end;

procedure TAddToPackageDlg.NewFileOkButtonClick(Sender: TObject);
var
  ANode: TTreeNode;
begin
  ANode:=NewFileTreeView.Selected;
  if (ANode=nil) or (ANode.Data=nil)
  or (not (TObject(ANode.Data) is TNewItemProjectFile))
  then begin
    IDEMessageDialog(lisNewDlgNoItemSelected,
      lisNewDlgPleaseSelectAnItemFirst, mtInformation, [mbOk]);
    exit;
  end;

  Params.Clear;
  Params.AddType:=d2ptNewFile;
  Params.NewItem:=TNewIDEItemTemplate(ANode.Data);

  ModalResult:=mrOk;
end;

procedure TAddToPackageDlg.NewFilePageResize(Sender: TObject);
begin
  NewFileTreeView.Width:=NewFilePage.ClientWidth div 2;
end;

procedure TAddToPackageDlg.NewFileTreeViewClick(Sender: TObject);
var
  Desc: String;
  ANode: TTreeNode;
begin
  ANode:=NewFileTreeView.Selected;
  Desc:='';
  if (ANode<>nil) and (ANode.Data<>nil) then begin
    if TObject(ANode.Data) is TNewIDEItemTemplate then
      Desc:=TNewIDEItemTemplate(ANode.Data).Description;
  end;
  NewFileHelpLabel.Caption:=Desc;
end;

procedure TAddToPackageDlg.NewFileTreeViewDblClick(Sender: TObject);
begin
  NewFileOkButtonClick(Self);
end;

procedure TAddToPackageDlg.NewFileTreeViewSelectionChanged(Sender: TObject);
begin
  NewFileOkButton.Enabled:=(NewFileTreeView.Selected<>nil)
            and (TObject(NewFileTreeView.Selected.Data) is TNewIDEItemTemplate);
end;

procedure TAddToPackageDlg.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
  Params.Pkg:=FLazPackage;
  UpdateAvailableAncestorTypes;
  UpdateAvailablePageNames;
  UpdateAvailableDependencyNames;
end;

procedure TAddToPackageDlg.SetupComponents;
begin
  NewFilePage.Caption:=lisA2PNewFile;
  NewComponentPage.Caption:=lisA2PNewComponent;
  NewRequirementPage.Caption:=lisProjAddNewRequirement;
  AddFilesPage.Caption:=lisA2PAddFiles;
  NoteBook.PageIndex:=0;

  SetupNewFilePage;
  SetupNewComponentPage;
  SetupAddDependencyPage;
  SetupAddFilesPage;
end;

procedure TAddToPackageDlg.SetupNewFilePage;
begin
  with NewFileDescriptionGroupBox do begin
    Caption:=lisCodeHelpDescrTag;
  end;

  with NewFileHelpLabel do begin
    Caption:='';
  end;
  
  with NewFileOkButton do begin
    Caption:=lisA2PCreateNewFile;
  end;
  
  with NewFileCancelButton do begin
    Caption:=dlgCancel;
  end;
  
  FillNewFileTreeView;
end;

procedure TAddToPackageDlg.SetupNewComponentPage;
begin
  with AncestorTypeLabel do begin
    Caption:=lisA2PAncestorType;
  end;

  with AncestorComboBox do begin
    Text:='';
  end;

  with AncestorShowAllCheckBox do begin
    Text:=lisA2PShowAll;
  end;

  with ClassNameLabel do begin
    Caption:=lisA2PNewClassName;
  end;

  with ClassNameEdit do begin
    Text:='';
  end;

  with PalettePageLabel do begin
    Caption:=lisA2PPalettePage;
  end;

  with PalettePageCombobox do begin
    Text:='';
  end;

  with ComponentUnitFileLabel do begin
    Caption:=lisA2PUnitFileName2;
  end;

  with ComponentUnitFileEdit do begin
    Text:='';
  end;

  with ComponentUnitFileBrowseButton do begin
    Caption:='...';
    ShowHint:=true;
    Hint:=lisA2PSaveFileDialog;
  end;

  with ComponentUnitFileShortenButton do begin
    Caption:='<>';
    ShowHint:=true;
    Hint:=lisA2PShortenOrExpandFilename;
  end;

  with ComponentUnitNameLabel do begin
    Caption:=lisA2PUnitName;
  end;

  with ComponentUnitNameEdit do begin
    Text:='';
  end;

  ComponentIconLabel.Caption:='Icon (maximum 24x24)';
  ComponentIconSpeedButton.Width:=ComponentPaletteBtnWidth;
  ComponentIconSpeedButton.Height:=ComponentPaletteBtnHeight;

  with NewComponentButton do begin
    Caption:=lisLazBuildOk;
  end;

  with CancelNewComponentButton do begin
    Caption:=dlgCancel;
  end;
end;

procedure TAddToPackageDlg.SetupAddDependencyPage;
begin
  with DependPkgNameLabel do begin
    Caption:=lisProjAddPackageName;
  end;

  with DependPkgNameComboBox do begin
    Text:='';
  end;

  with DependMinVersionLabel do begin
    Caption:=lisProjAddMinimumVersionOptional;
  end;

  with DependMinVersionEdit do begin
    Text:='';
  end;

  with DependMaxVersionLabel do begin
    Caption:=lisProjAddMaximumVersionOptional;
  end;

  with DependMaxVersionEdit do begin
    Text:='';
  end;

  with NewDependButton do begin
    Caption:=lisLazBuildOk;
  end;

  with CancelDependButton do begin
    Caption:=dlgCancel;
  end;
end;

procedure TAddToPackageDlg.SetupAddFilesPage;
var
  CurColumn: TListColumn;
begin
  with FilesListView do begin
    CurColumn:=Columns[0];
    CurColumn.Width:=200;
    CurColumn.Caption:=lisA2PFilename2;
    CurColumn:=Columns[1];
    CurColumn.Caption:=dlgEnvType;
  end;
  
  FilesBrowseButton.Caption:=lisPathEditBrowse;
  FilesDirButton.Caption:=lisAddDirectory;

  with FilesShortenButton do begin
    Caption:=lisA2PSwitchPaths;
    ShowHint:=true;
    Hint:=lisToggleShowingFilenamesWithFullPathOrWithRelativePa;
  end;

  with FilesDeleteButton do begin
    Caption:=dlgEdDelete;
    ShowHint:=true;
    Hint:=lisDeleteSelectedFiles;
  end;
  FilesAddButton.Caption:=lisA2PAddFilesToPackage;
end;

procedure TAddToPackageDlg.OnIterateComponentClasses(PkgComponent: TPkgComponent);
begin
  if fPkgComponents.Find(PkgComponent)=nil then
    fPkgComponents.Add(PkgComponent);
end;

procedure TAddToPackageDlg.OnIteratePackages(APackageID: TLazPackageID);
begin
  if (APackageID<>LazPackage) and (fPackages.Find(APackageID)=nil) then begin
    //DebugLn(['TAddToPackageDlg.OnIteratePackages ',APackageID.IDAsString,' ',DbgSName(APackageID)]);
    fPackages.Add(APackageID);
  end;
end;

procedure TAddToPackageDlg.AutoCompleteNewComponent;
var
  PkgComponent: TPkgComponent;
begin
  fLastNewComponentAncestorType:=AncestorComboBox.Text;
  if not IsValidIdent(fLastNewComponentAncestorType) then exit;
  PkgComponent:=TPkgComponent(
    IDEComponentPalette.FindComponent(fLastNewComponentAncestorType));

  // create unique classname
  if (not IsValidIdent(ClassNameEdit.Text)) or (ClassNameEdit.Text='') then
    ClassNameEdit.Text:=IDEComponentPalette.CreateNewClassName(
                                                 fLastNewComponentAncestorType);
  // choose the same page name
  if (PalettePageCombobox.Text='') and (PkgComponent<>nil) then
    PalettePageCombobox.Text:=PkgComponent.Page.PageName;
  // filename
  AutoCompleteNewComponentUnitName;
end;

procedure TAddToPackageDlg.AutoCompleteNewComponentUnitName;
var
  CurClassName: String;
  NewUnitName: String;
  NewFileName: String;
begin
  // check if update needed
  CurClassName:=ClassNameEdit.Text;
  if fLastNewComponentClassName=CurClassName then exit;
  fLastNewComponentClassName:=CurClassName;

  // check classname
  if not IsValidIdent(CurClassName) then exit;

  // create unitname
  NewUnitName:=CurClassName;
  if NewUnitName[1]='T' then
    NewUnitName:=copy(NewUnitName,2,length(NewUnitName)-1);
  NewUnitName:=PackageGraph.CreateUniqueUnitName(NewUnitName);
  ComponentUnitNameEdit.Text:=NewUnitName;

  // create filename
  NewFileName:=NewUnitName;

  if EnvironmentOptions.CharcaseFileAction in [ccfaAsk, ccfaAutoRename] then
    NewFileName:=lowercase(NewFileName);

  // append pascal file extension
  NewFileName:=NewFileName
       +EnvironmentOpts.PascalExtension[EnvironmentOptions.PascalFileExtension];
  // prepend path
  if LazPackage.HasDirectory then
    NewFileName:=LazPackage.Directory+NewFileName;
  ComponentUnitFileEdit.Text:=NewFileName;
end;

function TAddToPackageDlg.SwitchRelativeAbsoluteFilename(const Filename: string
  ): string;
begin
  Result:=Filename;
  if (not LazPackage.HasDirectory)
  or (not FilenameIsAbsolute(LazPackage.Directory)) then exit;
  if FilenameIsAbsolute(Filename) then
    Result:=TrimFilename(CreateRelativePath(Filename,LazPackage.Directory))
  else
    Result:=TrimFilename(CreateAbsoluteSearchPath(Filename,LazPackage.Directory));
end;

procedure TAddToPackageDlg.FillNewFileTreeView;
var
  NewParentNode: TTreeNode;
  Category: TNewIDEItemCategory;
  TemplateID: Integer;
  Template: TNewIDEItemTemplate;
begin
  NewFileTreeView.BeginUpdate;
  NewFileTreeView.Items.Clear;
  Category:=NewIDEItems.FindByName(FileDescGroupName);
  NewParentNode:=NewFileTreeView.Items.AddObject(nil,Category.LocalizedName,
                                                 Category);
  for TemplateID:=0 to Category.Count-1 do begin
    Template:=Category[TemplateID];
    if Template.VisibleInNewDialog and (Template is TNewItemProjectFile) then
      NewFileTreeView.Items.AddChildObject(NewParentNode,Template.LocalizedName,
                                           Template);
  end;
  NewParentNode.Expand(true);
  NewFileTreeView.EndUpdate;
end;

function TAddToPackageDlg.FindFileInFilesList(AFilename: string): Integer;
var
  i: Integer;
  Item: TListItem;
  OtherFilename: String;
begin
  if not FilenameIsAbsolute(AFilename) then
    LazPackage.LongenFilename(AFilename);
  for i:=0 to FilesListView.Items.Count-1 do begin
    Item:=FilesListView.Items[i];
    OtherFilename:=Item.Caption;
    if not FilenameIsAbsolute(OtherFilename) then
      LazPackage.LongenFilename(OtherFilename);
    if CompareFilenames(AFilename,OtherFilename)=0 then begin
      Result:=i;
      exit;
    end;
  end;
  Result:=-1;
end;

procedure TAddToPackageDlg.LoadComponentIcon(AFilename: string);
var
  ShortFilename: String;
  Image: TImage;
begin
  try
    Image:=TImage.Create(nil);
    try
      Image.Picture.LoadFromFile(AFilename);
      ComponentIconSpeedButton.Glyph.Assign(Image.Picture.Graphic);
      ShortFilename:=AFilename;
      LazPackage.ShortenFilename(ShortFilename,true);
      ComponentIconSpeedButton.Hint:=
        ShortFilename+' ('+IntToStr(ComponentIconSpeedButton.Glyph.Width)
                         +'x'+IntToStr(ComponentIconSpeedButton.Glyph.Height)+')';
      FComponentIconFilename:=AFilename;
    finally
      Image.Free;
    end;
  except
    on E: Exception do begin
      IDEMessageDialog(lisCCOErrorCaption,
        Format(lisErrorLoadingFile2, [AFilename, #13, E.Message]), mtError, [
          mbCancel]);
      ComponentIconSpeedButton.Glyph.Clear;
      FComponentIconFilename:='';
      ComponentIconSpeedButton.Hint:=lisNoneClickToChooseOne;
    end;
  end;
end;

procedure TAddToPackageDlg.UpdateAvailableAncestorTypes;
var
  ANode: TAVLTreeNode;
  sl: TStringList;
  OldAncestorType: String;
begin
  // get all available registered components
  fPkgComponents.Clear;
  if AncestorShowAllCheckBox.Checked then begin
    PackageGraph.IterateAllComponentClasses(@OnIterateComponentClasses);
  end else begin
    PackageGraph.IterateComponentClasses(LazPackage,@OnIterateComponentClasses,
                                         true,true);
  end;
  // put them into a list
  sl:=TStringList.Create;
  ANode:=fPkgComponents.FindLowest;
  while ANode<>nil do begin
    sl.Add(TPkgComponent(ANode.Data).ComponentClass.ClassName);
    ANode:=fPkgComponents.FindSuccessor(ANode);
  end;
  // add at least TComponent
  sl.Add('TComponent');
  sl.Sort;
  
  // put them into the combobox
  OldAncestorType:=AncestorComboBox.Text;
  AncestorComboBox.Items.Assign(sl);
  AncestorComboBox.Text:=OldAncestorType;
  sl.Free;
end;

procedure TAddToPackageDlg.UpdateAvailablePageNames;
var
  i: Integer;
  APageName: String;
  sl: TStringList;
begin
  // get all current pagenames (excluding the hidden page)
  sl:=TStringList.Create;
  for i:=0 to IDEComponentPalette.Count-1 do begin
    APageName:=IDEComponentPalette[i].PageName;
    if APageName<>'' then
      sl.Add(APageName);
  end;
  sl.Sort;
  PalettePageCombobox.Items.Assign(sl);
  sl.Free;
end;

procedure TAddToPackageDlg.UpdateAvailableDependencyNames;
var
  ANode: TAVLTreeNode;
  sl: TStringList;
  PkgName: String;
  Pkg: TLazPackage;
begin
  fPackages.Clear;
  PackageGraph.IteratePackages(fpfSearchAllExisting,@OnIteratePackages);
  sl:=TStringList.Create;
  ANode:=fPackages.FindLowest;
  while ANode<>nil do begin
    Pkg:=TLazPackage(ANode.Data);
    PkgName:=Pkg.Name;
    if (sl.IndexOf(PkgName)<0) then
      sl.Add(PkgName);
    ANode:=fPackages.FindSuccessor(ANode);
  end;
  DependPkgNameComboBox.Items.Assign(sl);
  sl.Free;
end;

{ TAddToPkgResult }

procedure TAddToPkgResult.Clear;
begin
  AddType:=d2ptUnit;
  Dependency:=nil;
  UnitFilename:='';
  Unit_Name:='';
  AncestorType:='';
  NewClassName:='';
  PageName:='';
  FileType:=pftUnit;
  PkgFileFlags:=[];
  UsedUnitname:='';
  AutoAddLFMFile:=false;
  AutoAddLRSFile:=false;
  FreeThenNil(Next);
end;

destructor TAddToPkgResult.Destroy;
begin
  FreeThenNil(Next);
  inherited Destroy;
end;

end.

