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
  Classes, SysUtils, LResources, Forms, Controls, Buttons, StdCtrls, ExtCtrls,
  Dialogs, FileUtil, ComCtrls, LazarusIDEStrConsts, IDEOptionDefs, InputHistory,
  AVL_Tree, CodeToolManager, IDEDefs, IDEProcs, EnvironmentOpts, PackageSystem,
  PackageDefs, ComponentReg;
  
type
  TAddToPkgType = (
    d2ptUnit,
    d2ptVirtualUnit,
    d2ptNewComponent,
    d2ptRequiredPkg,
    d2ptFile,
    d2ptFiles
    );
    
  TAddToPkgResult = class
    AddType: TAddToPkgType;
    Dependency: TPkgDependency;
    UnitFilename: string;
    UnitName: string;
    AncestorType: string;
    NewClassName: string;
    PageName: string;
    FileType: TPkgFileType;
    PkgFileFlags: TPkgFileFlags;
    UsedUnitname: string;
    AutoAddLFMFile: boolean;
    AutoAddLRSFile: boolean;
    Next: TAddToPkgResult;
    procedure Clear;
    destructor Destroy; override;
  end;
  
  TOnGetUnitRegisterInfo = procedure(Sender: TObject; const AFilename: string;
    var TheUnitName: string; var HasRegisterProc: boolean) of object;

  TAddToPackageDlg = class(TForm)
    // notebook
    NoteBook: TNoteBook;
    AddUnitPage: TPage;
    AddFilePage: TPage;
    AddFilesPage: TPage;
    NewComponentPage: TPage;
    NewDependPage: TPage;
    // add unit page
    AddUnitFilenameLabel: TLabel;
    AddUnitFilenameEdit: TEdit;
    AddUnitFileBrowseButton: TButton;
    AddUnitFileShortenButton: TButton;
    AddUnitSrcNameLabel: TLabel;
    AddUnitSrcNameEdit: TEdit;
    AddUnitHasRegisterCheckBox: TCheckBox;
    AddUnitIsVirtualCheckBox: TCheckBox;
    AddSecondaryFilesCheckBox: TCheckBox;
    AddUnitUpdateButton: TButton;
    AddUnitButton: TButton;
    CancelAddUnitButton: TButton;
    // new component page
    AncestorTypeLabel: TLabel;
    AncestorComboBox: TComboBox;
    AncestorShowAllCheckBox: TCheckBox;
    ClassNameLabel: TLabel;
    ClassNameEdit: TEdit;
    PalettePageLabel: TLabel;
    PalettePageCombobox: TCombobox;
    ComponentUnitFileLabel: TLabel;
    ComponentUnitFileEdit: TEdit;
    ComponentUnitFileBrowseButton: TButton;
    ComponentUnitFileShortenButton: TButton;
    ComponentUnitNameLabel: TLabel;
    ComponentUnitNameEdit: TEdit;
    NewComponentButton: TButton;
    CancelNewComponentButton: TButton;
    // new required package
    DependPkgNameLabel: TLabel;
    DependPkgNameComboBox: TComboBox;
    DependMinVersionLabel: TLabel;
    DependMinVersionEdit: TEdit;
    DependMaxVersionLabel: TLabel;
    DependMaxVersionEdit: TEdit;
    NewDependButton: TButton;
    CancelDependButton: TButton;
    // add file page
    AddFilenameLabel: TLabel;
    AddFilenameEdit: TEdit;
    AddFileBrowseButton: TButton;
    AddFileShortenButton: TButton;
    AddFileTypeRadioGroup: TRadioGroup;
    AddFileButton: TButton;
    CancelAddFileButton: TButton;
    // add files page
    FilesListView: TListView;
    FilesBrowseButton: TButton;
    FilesShortenButton: TButton;
    FilesDeleteButton: TButton;
    FilesAddButton: TButton;
    procedure AddFileBrowseButtonClick(Sender: TObject);
    procedure AddFileButtonClick(Sender: TObject);
    procedure AddFilePageResize(Sender: TObject);
    procedure AddFileShortenButtonClick(Sender: TObject);
    procedure AddFilesPageResize(Sender: TObject);
    procedure AddToPackageDlgClose(Sender: TObject;
                                   var CloseAction: TCloseAction);
    procedure AddUnitButtonClick(Sender: TObject);
    procedure AddUnitFileBrowseButtonClick(Sender: TObject);
    procedure AddUnitFileShortenButtonClick(Sender: TObject);
    procedure AddUnitIsVirtualCheckBoxClick(Sender: TObject);
    procedure AddUnitPageResize(Sender: TObject);
    procedure AddUnitUpdateButtonClick(Sender: TObject);
    procedure AncestorComboBoxCloseUp(Sender: TObject);
    procedure AncestorShowAllCheckBoxClick(Sender: TObject);
    procedure CancelAddFileButtonClick(Sender: TObject);
    procedure CancelAddUnitButtonClick(Sender: TObject);
    procedure CancelNewComponentButtonClick(Sender: TObject);
    procedure ClassNameEditChange(Sender: TObject);
    procedure ComponentUnitFileBrowseButtonClick(Sender: TObject);
    procedure ComponentUnitFileShortenButtonClick(Sender: TObject);
    procedure FilesAddButtonClick(Sender: TObject);
    procedure FilesBrowseButtonClick(Sender: TObject);
    procedure FilesDeleteButtonClick(Sender: TObject);
    procedure FilesShortenButtonClick(Sender: TObject);
    procedure NewComponentButtonClick(Sender: TObject);
    procedure NewComponentPageResize(Sender: TObject);
    procedure NewDependButtonClick(Sender: TObject);
    procedure NewDependPageResize(Sender: TObject);
  private
    fLastNewComponentAncestorType: string;
    fLastNewComponentClassName: string;
    FLazPackage: TLazPackage;
    FOnGetIDEFileInfo: TGetIDEFileStateEvent;
    FOnGetUnitRegisterInfo: TOnGetUnitRegisterInfo;
    fPkgComponents: TAVLTree;// tree of TPkgComponent
    fPackages: TAVLTree;// tree of  TLazPackage or TPackageLink
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure SetupComponents;
    procedure SetupAddUnitPage;
    procedure SetupNewComponentPage;
    procedure SetupAddDependencyPage;
    procedure SetupAddFilePage;
    procedure SetupAddFilesPage;
    procedure OnIterateComponentClasses(PkgComponent: TPkgComponent);
    procedure OnIteratePackages(APackageID: TLazPackageID);
    procedure AutoCompleteNewComponent;
    procedure AutoCompleteNewComponentUnitName;
    procedure UpdateAddUnitInfo;
    procedure UpdateAddFileInfo;
    function FileNameToPkgFileType(const AFilename: string): TPkgFileType;
    function SwitchRelativeAbsoluteFilename(const Filename: string): string;
  public
    Params: TAddToPkgResult;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
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
  NewDependency: TPkgDependency): boolean;


implementation


function ShowAddToPackageDlg(Pkg: TLazPackage; var Params: TAddToPkgResult;
  OnGetIDEFileInfo: TGetIDEFileStateEvent;
  OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo): TModalResult;
var
  AddDlg: TAddToPackageDlg;
begin
  AddDlg:=TAddToPackageDlg.Create(Application);
  AddDlg.LazPackage:=Pkg;
  AddDlg.OnGetIDEFileInfo:=OnGetIDEFileInfo;
  AddDlg.OnGetUnitRegisterInfo:=OnGetUnitRegisterInfo;
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

  // normalize filename
  AFilename:=TrimFilename(AFilename);
  if (AddFileType<>d2ptVirtualUnit) and (not FilenameIsAbsolute(AFilename)) then
  begin
    if LazPackage.HasDirectory then
      AFilename:=LazPackage.Directory+AFilename
    else begin
      MessageDlg(lisA2PInvalidFilename,
        Format(lisA2PTheFilenameIsAmbigiousPleaseSpecifiyAFilename, ['"',
          AFilename, '"', #13]),
        mtError,[mbCancel],0);
      exit;
    end;
  end;

  // check if file exists
  if (FilenameIsAbsolute(AFilename)) then begin
    if (not FileExists(AFilename)) then begin
      if AddFileType=d2ptUnit then begin
        MessageDlg(lisFileNotFound,
          Format(lisPkgMangFileNotFound, ['"', AFilename, '"']), mtError, [
            mbCancel], 0);
        exit;
      end;
    end;
  end;

  // check file extension
  if AddFileType in [d2ptUnit,d2ptNewComponent,d2ptVirtualUnit] then begin
    if not FilenameIsPascalUnit(AFilename) then begin
      MessageDlg(lisA2PFileNotUnit,
        lisA2PPascalUnitsMustHaveTheExtensionPPOrPas,
        mtWarning,[mbCancel],0);
      exit;
    end;
  end;

  // check unitname
  if AddFileType in [d2ptUnit,d2ptNewComponent,d2ptVirtualUnit] then begin
    AnUnitName:=ExtractFileNameOnly(AFilename);
    if not IsValidIdent(AnUnitName) then begin
      MessageDlg(lisA2PFileNotUnit,
        Format(lisA2PisNotAValidUnitName, ['"', AnUnitName, '"']),
        mtWarning,[mbCancel],0);
      exit;
    end;

    // check if unitname already exists in package
    PkgFile:=PackageGraph.FindUnit(LazPackage,AnUnitName,true,true);
    if PkgFile<>nil then begin
      if PkgFile.LazPackage=LazPackage then begin
        MessageDlg(lisA2PUnitnameAlreadyExists,
          Format(lisA2PTheUnitnameAlreadyExistsInThisPackage, ['"', AnUnitName,
            '"']),
          mtError,[mbCancel],0);
        exit;
      end else begin
        if MessageDlg(lisA2PUnitnameAlreadyExists,
          Format(lisA2PTheUnitnameAlreadyExistsInThePackage, ['"', AnUnitName,
            '"', #13, PkgFile.LazPackage.IDAsString]),
          mtWarning,[mbCancel,mbIgnore],0)<>mrIgnore then exit;
      end;
    end;

    // check if unitname is a componentclass
    if IDEComponentPalette.FindComponent(AnUnitName)<>nil then begin
      if MessageDlg(lisA2PAmbigiousUnitName,
        Format(lisA2PTheUnitNameIsTheSameAsAnRegisteredComponent, ['"',
          AnUnitName, '"', #13]),
        mtWarning,[mbCancel,mbIgnore],0)<>mrIgnore
      then
        exit;
    end;
  end else begin
    AnUnitName:='';
  end;

  // check if file already exists in package
  if FilenameIsAbsolute(AFilename) then begin
    PkgFile:=LazPackage.FindPkgFile(AFilename,true,true);
    if PkgFile<>nil then begin
      Msg:=Format(lisA2PFileAlreadyExistsInTheProject, ['"', AFilename, '"']);
      if PkgFile.Filename<>AFilename then
        Msg:=Format(lisA2PExistingFile, [#13, '"', PkgFile.Filename, '"']);
      MessageDlg(lisA2PFileAlreadyExists, Msg, mtError, [mbCancel], 0);
      exit;
    end;
  end;

  // check if file is part of project
  if FilenameIsAbsolute(AFilename) then begin
    if Assigned(OnGetIDEFileInfo) then begin
      IDEFileFlags:=[];
      OnGetIDEFileInfo(nil,AFilename,[ifsPartOfProject],
                       IDEFileFlags);
      if (ifsPartOfProject in IDEFileFlags) then begin
        MessageDlg(lisA2PFileIsUsed,
          Format(lisA2PTheFileIsPartOfTheCurrentProjectItIsABadIdea, ['"',
            AFilename, '"', #13]),
          mtError,[mbCancel],0);
        exit;
      end;
    end;
  end;

  // ok
  Result:=true;
end;

function CheckAddingDependency(LazPackage: TLazPackage;
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
    MessageDlg(lisProjAddInvalidMinMaxVersion,
      lisA2PTheMaximumVersionIsLowerThanTheMinimimVersion,
      mtError,[mbCancel],0);
    exit;
  end;

  // check packagename
  if (NewPkgName='') or (not IsValidIdent(NewPkgName)) then begin
    MessageDlg(lisProjAddInvalidPackagename,
      Format(lisA2PThePackageNameIsInvalidPleaseChooseAnExisting, ['"',
        NewPkgName, '"', #13]),
      mtError,[mbCancel],0);
    exit;
  end;

  // check if package is already required
  if LazPackage.FindDependencyByName(NewPkgName)<>nil then begin
    MessageDlg(lisProjAddDependencyAlreadyExists,
      Format(lisA2PThePackageHasAlreadyADependencyForThe, ['"', NewPkgName, '"']
        ),
      mtError,[mbCancel],0);
    exit;
  end;

  // check if required package exists
  if not PackageGraph.DependencyExists(NewDependency,fpfSearchAllExisting)
  then begin
    MessageDlg(lisProjAddPackageNotFound,
      Format(lisA2PNoPackageFoundForDependencyPleaseChooseAnExisting, ['"',
        NewDependency.AsString, '"', #13]),
      mtError,[mbCancel],0);
    exit;
  end;
  
  Result:=true;
end;


{ TAddToPackageDlg }

procedure TAddToPackageDlg.AddUnitButtonClick(Sender: TObject);
begin
  Params.Clear;
  if not AddUnitIsVirtualCheckBox.Checked then begin
    // normal unit
    Params.AddType:=d2ptUnit;

    Params.UnitFilename:=AddUnitFilenameEdit.Text;
    Params.UnitName:=AddUnitSrcNameEdit.Text;
    Params.FileType:=pftUnit;
    Params.PkgFileFlags:=[];
    Params.AutoAddLFMFile:=AddSecondaryFilesCheckBox.Checked;
    Params.AutoAddLRSFile:=AddSecondaryFilesCheckBox.Checked;
    if AddUnitHasRegisterCheckBox.Checked then
      Include(Params.PkgFileFlags,pffHasRegisterProc);
  end else begin
    // virtual unit
    Params.AddType:=d2ptVirtualUnit;

    Params.UnitFilename:=ExtractFilename(AddUnitFilenameEdit.Text);
    Params.UnitName:=AddUnitSrcNameEdit.Text;
    Params.FileType:=pftVirtualUnit;
    Params.PkgFileFlags:=[];
    Params.AutoAddLFMFile:=false;
    Params.AutoAddLRSFile:=false;
  end;
  LazPackage.LongenFilename(Params.UnitFilename);

  // check filename
  if not CheckAddingUnitFilename(LazPackage,Params.AddType,
    OnGetIDEFileInfo,Params.UnitFilename) then exit;
    
  // check unitname
  if AnsiCompareText(Params.UnitName,ExtractFileNameOnly(Params.UnitFilename))<>0
  then begin
    MessageDlg(lisA2PInvalidUnitName,
      Format(lisA2PTheUnitNameAndFilenameDiffer, ['"', Params.UnitName, '"']),
      mtError,[mbCancel],0);
    exit;
  end;

  // add it ...
  ModalResult:=mrOk;
end;

procedure TAddToPackageDlg.AddToPackageDlgClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddToPackageDlg.AddFilePageResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
begin
  x:=5;
  y:=5;
  with AddFilenameLabel do
    SetBounds(x,y+2,100,Height);
  inc(x,AddFilenameLabel.Width+5);

  w:=AddFilenameEdit.Height;
  with AddFilenameEdit do
    SetBounds(x,y,Parent.ClientWidth-x-2-w-2-w-5,Height);
  inc(x,AddFilenameEdit.Width+2);

  with AddFileBrowseButton do
    SetBounds(x,y,w,w);
  inc(x,w+2);
  with AddFileShortenButton do
    SetBounds(x,y,w,w);
  x:=5;
  y:=AddFilenameEdit.Top+AddFilenameEdit.Height+5;

  with AddFileTypeRadioGroup do begin
    SetBounds(x,y,Parent.ClientWidth-2*x,140);
    inc(y,Height+20);
  end;

  with AddFileButton do
    SetBounds(x,y,80,Height);
  inc(x,AddFileButton.Width+10);

  with CancelAddFileButton do
    SetBounds(x,y,80,Height);
end;

procedure TAddToPackageDlg.AddFileShortenButtonClick(Sender: TObject);
begin
  if lisA2PchooseAnExistingFile=AddFilenameEdit.Text then exit;
  AddFilenameEdit.Text:=
    SwitchRelativeAbsoluteFilename(AddFilenameEdit.Text);
end;

procedure TAddToPackageDlg.AddFilesPageResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
begin
  with FilesListView do
    SetBounds(0,0,Parent.ClientWidth,Parent.ClientHeight-45);

  x:=5;
  y:=FilesListView.Height+10;
  w:=100;
  h:=25;
  
  with FilesBrowseButton do
    SetBounds(x,y,w,h);
  inc(x,w+3);

  with FilesShortenButton do
    SetBounds(x,y,w,h);
  inc(x,w+3);

  with FilesDeleteButton do
    SetBounds(x,y,w,h);
  inc(x,w+3);

  w:=FilesAddButton.Parent.ClientWidth-x-5;
  if w<20 then w:=20;
  with FilesAddButton do
    SetBounds(x,y,w,h);
end;

procedure TAddToPackageDlg.AddFileBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.InitialDir:=
      LazPackage.GetFileDialogInitialDir(OpenDialog.InitialDir);
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist,ofPathMustExist];
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      if FileExists(AFilename) then begin
        LazPackage.ShortenFilename(AFilename,true);
        AddFilenameEdit.Text:=AFilename;
        UpdateAddFileInfo;
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TAddToPackageDlg.AddFileButtonClick(Sender: TObject);
var
  i: Integer;
  CurPFT: TPkgFileType;
  Filename: String;
begin
  Filename:=AddFilenameEdit.Text;
  if Filename='' then exit;
  LazPackage.LongenFilename(Filename);

  Params.Clear;
  Params.AddType:=d2ptUnit;
  Params.UnitFilename:=Filename;
  Params.FileType:=pftText;
  Params.UnitName:='';
  Params.PkgFileFlags:=[];

  if not FileExists(Params.UnitFilename) then begin
    MessageDlg(lisFileNotFound,
      Format(lisPkgMangFileNotFound, ['"', Params.UnitFilename, '"']),
      mtError,[mbCancel],0);
    exit;
  end;
  if LazPackage.FindPkgFile(Params.UnitFilename,true,true)<>nil then begin
    MessageDlg(lisA2PFileAlreadyInPackage,
      Format(lisA2PTheFileIsAlreadyInThePackage, ['"', Params.UnitFilename, '"']
        ),
      mtError,[mbCancel],0);
    exit;
  end;
  
  i:=0;
  for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
    if CurPFT=pftVirtualUnit then continue;
    if i=AddFileTypeRadioGroup.ItemIndex then begin
      Params.FileType:=CurPFT;
    end;
    inc(i);
  end;

  ModalResult:=mrOk;
end;

procedure TAddToPackageDlg.AddUnitFileBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.InitialDir:=
      LazPackage.GetFileDialogInitialDir(OpenDialog.InitialDir);
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist,ofPathMustExist];
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      if FileExists(AFilename) then begin
        LazPackage.ShortenFilename(AFilename,true);
        AddUnitFilenameEdit.Text:=AFilename;
        UpdateAddUnitInfo;
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TAddToPackageDlg.AddUnitFileShortenButtonClick(Sender: TObject);
begin
  if lisA2PchooseAnExistingFile=AddUnitFilenameEdit.Text then exit;
  AddUnitFilenameEdit.Text:=
    SwitchRelativeAbsoluteFilename(AddUnitFilenameEdit.Text);
end;

procedure TAddToPackageDlg.AddUnitIsVirtualCheckBoxClick(Sender: TObject);
var
  VirtualFile: Boolean;
begin
  VirtualFile:=AddUnitIsVirtualCheckBox.Checked;
  AddUnitFileBrowseButton.Enabled:=not VirtualFile;
  AddUnitFileShortenButton.Enabled:=not VirtualFile;
  AddUnitUpdateButton.Enabled:=not VirtualFile;
  AddSecondaryFilesCheckBox.Enabled:=not VirtualFile;
end;

procedure TAddToPackageDlg.AddUnitPageResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
begin
  x:=5;
  y:=5;
  with AddUnitFilenameLabel do
    SetBounds(x,y+2,100,Height);
  inc(x,AddUnitFilenameLabel.Width+5);

  w:=AddUnitFilenameEdit.Height;
  with AddUnitFilenameEdit do
    SetBounds(x,y,Parent.ClientWidth-x-2*w-2-2-5,Height);
  inc(x,AddUnitFilenameEdit.Width+2);

  with AddUnitFileBrowseButton do
    SetBounds(x,y,w,w);
  inc(x,w+2);
  with AddUnitFileShortenButton do
    SetBounds(x,y,w,w);

  x:=5;
  y:=AddUnitFilenameEdit.Top+AddUnitFilenameEdit.Height+5;

  with AddUnitSrcNameLabel do
    SetBounds(x,y+2,100,Height);
  inc(x,AddUnitSrcNameLabel.Width+5);

  with AddUnitSrcNameEdit do
    SetBounds(x,y,100,Height);
  inc(y,AddUnitSrcNameEdit.Height+5);
  x:=5;

  with AddUnitHasRegisterCheckBox do
    SetBounds(x,y,Parent.ClientWidth-2*x,Height);
  inc(y,AddUnitHasRegisterCheckBox.Height+5);

  with AddUnitIsVirtualCheckBox do
    SetBounds(x,y,Parent.ClientWidth-2*x,Height);
  inc(y,AddUnitIsVirtualCheckBox.Height+5);

  with AddSecondaryFilesCheckBox do
    SetBounds(x,y,Parent.ClientWidth-2*x,Height);
  inc(y,AddSecondaryFilesCheckBox.Height+5);

  with AddUnitUpdateButton do
    SetBounds(x,y,300,Height);
  inc(y,AddUnitUpdateButton.Height+25);

  with AddUnitButton do
    SetBounds(x,y,80,Height);
  inc(x,AddUnitButton.Width+10);

  with CancelAddUnitButton do
    SetBounds(x,y,80,Height);
end;

procedure TAddToPackageDlg.AddUnitUpdateButtonClick(Sender: TObject);
begin
  UpdateAddUnitInfo;
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

procedure TAddToPackageDlg.ComponentUnitFileBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.InitialDir:=
      LazPackage.GetFileDialogInitialDir(OpenDialog.InitialDir);
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      if FilenameIsPascalUnit(AFilename) then begin
        LazPackage.ShortenFilename(AFilename,true);
        ComponentUnitFileEdit.Text:=AFilename;
      end else begin
        MessageDlg(lisA2PInvalidFile,
         lisA2PAPascalUnitMustHaveTheExtensionPPOrPas,
         mtError,[mbCancel],0);
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
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
  LastParams: TAddToPkgResult;
  ok: Boolean;
  CurParams: TAddToPkgResult;
begin
  ok:=true;
  try
    LastParams:=nil;
    for i:=0 to FilesListView.Items.Count-1 do begin
      Filename:=FilesListView.Items[i].Caption;
      LazPackage.LongenFilename(Filename);
      NewFileType:=FileNameToPkgFileType(Filename);

      if not FileExists(Filename) then begin
        MessageDlg(lisFileNotFound,
          Format(lisPkgMangFileNotFound, ['"', Filename, '"']),
          mtError,[mbCancel],0);
        exit;
      end;
      if LazPackage.FindPkgFile(Filename,true,true)<>nil then begin
        MessageDlg(lisA2PFileAlreadyInPackage,
          Format(lisA2PTheFileIsAlreadyInThePackage, ['"', Filename, '"']
            ),
          mtError,[mbCancel],0);
        exit;
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

        // check filename
        if not CheckAddingUnitFilename(LazPackage,CurParams.AddType,
          OnGetIDEFileInfo,CurParams.UnitFilename) then exit;

        CurParams.AutoAddLFMFile:=true;
        CurParams.AutoAddLRSFile:=true;
        if Assigned(OnGetUnitRegisterInfo) then begin
          OnGetUnitRegisterInfo(Self,Filename,CurParams.UnitName,HasRegisterProc);
          if HasRegisterProc then
            Include(CurParams.PkgFileFlags,pffHasRegisterProc);
        end;

        // check unitname
        if AnsiCompareText(CurParams.UnitName,
          ExtractFileNameOnly(CurParams.UnitFilename))<>0
        then begin
          MessageDlg(lisA2PInvalidUnitName,
            Format(lisA2PTheUnitNameAndFilenameDiffer, ['"', CurParams.UnitName, '"']),
            mtError,[mbCancel],0);
          exit;
        end;
      end;
      LastParams:=CurParams;
    end;
  finally
    if not ok then Params.Clear;
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
  OpenDialog:=TOpenDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.InitialDir:=
      LazPackage.GetFileDialogInitialDir(OpenDialog.InitialDir);
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options
                          +[ofFileMustExist,ofPathMustExist,ofAllowMultiSelect];
    if OpenDialog.Execute then begin
      for i:=0 to OpenDialog.Files.COunt-1 do begin
        AFilename:=CleanAndExpandFilename(OpenDialog.Files[i]);
        if FileExists(AFilename) then begin
          LazPackage.ShortenFilename(AFilename,true);
          NewListItem:=FilesListView.Items.Add;
          NewListItem.Caption:=AFilename;
          NewPgkFileType:=FileNameToPkgFileType(AFilename);
          NewListItem.SubItems.Add(GetPkgFileTypeLocalizedName(NewPgkFileType));
        end;
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
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
      Filename:=CreateAbsolutePath(Filename,LazPackage.Directory)
    else
      Filename:=CreateRelativePath(Filename,LazPackage.Directory);
    FilesListView.Items[i].Caption:=Filename;
  end;
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
  Params.PkgFileFlags:=[pffHasRegisterProc];
  Params.AncestorType:=AncestorComboBox.Text;
  Params.NewClassName:=ClassNameEdit.Text;
  Params.PageName:=PalettePageCombobox.Text;
  Params.UnitName:=ComponentUnitNameEdit.Text;
  Params.UnitFilename:=ComponentUnitFileEdit.Text;
  Params.UsedUnitname:='';

  // check Ancestor Type
  if not IsValidIdent(Params.AncestorType) then begin
    MessageDlg(lisA2PInvalidAncestorType,
      Format(lisA2PTheAncestorTypeIsNotAValidPascalIdentifier, ['"',
        Params.AncestorType, '"']),
      mtError,[mbCancel],0);
    exit;
  end;

  // check pagename
  if length(Params.PageName)>100 then begin
    MessageDlg(lisA2PPageNameTooLong,
      Format(lisA2PThePageNameIsTooLongMax100Chars, ['"', Params.PageName, '"']
        ),
      mtError,[mbCancel],0);
    exit;
  end;

  // check unitname - filename redundancy
  if AnsiCompareText(Params.Unitname,ExtractFileNameOnly(Params.UnitFilename))<>0
  then begin
    MessageDlg(lisA2PUnitNameInvalid,
      Format(lisA2PTheUnitNameDoesNotCorrespondToTheFilename, ['"',
        Params.UnitName, '"']),
      mtError,[mbCancel],0);
    exit;
  end;

  // check classname
  if not IsValidIdent(Params.NewClassName) then begin
    MessageDlg(lisA2PInvalidClassName,
      Format(lisA2PTheClassNameIsNotAValidPascalIdentifier, ['"',
        Params.NewClassName, '"']),
      mtError,[mbCancel],0);
    exit;
  end;

  // check classname<>ancestortype
  if AnsiCompareText(Params.NewClassName,Params.AncestorType)=0 then begin
    MessageDlg(lisA2PInvalidCircle,
      Format(lisA2PTheClassNameAndAncestorTypeAreTheSame, ['"',
        Params.NewClassName, '"', '"', Params.AncestorType, '"']),
      mtError,[mbCancel],0);
    exit;
  end;

  // check ancestor type is not unitname
  PkgFile:=PackageGraph.FindUnit(LazPackage,Params.AncestorType,true,true);
  if PkgFile<>nil then begin
    if MessageDlg(lisA2PAmbigiousAncestorType,
      Format(lisA2PTheAncestorTypeHasTheSameNameAsTheUnit, ['"',
        Params.AncestorType, '"', #13, '"', PkgFile.Filename, '"']),
      mtError,[mbCancel,mbIgnore],0)<>mrIgnore
    then
      exit;
  end;

  // check classname does not interfere with an existing unitname
  PkgFile:=PackageGraph.FindUnit(LazPackage,Params.NewClassName,true,true);
  if PkgFile<>nil then begin
    if MessageDlg(lisA2PAmbigiousClassName,
      Format(lisA2PTheClassNameHasTheSameNameAsTheUnit, ['"',
        Params.AncestorType, '"', #13, '"', PkgFile.Filename, '"']),
      mtError,[mbCancel,mbIgnore],0)<>mrIgnore
    then
      exit;
  end;

  // check if classname already exists
  PkgComponent:=
    TPkgComponent(IDEComponentPalette.FindComponent(Params.NewClassname));
  if PkgComponent<>nil then begin
    if MessageDlg(lisA2PClassNameAlreadyExists,
      Format(lisA2PTheClassNameExistsAlreadyInPackageFile, ['"',
        Params.NewClassName, '"', #13, PkgComponent.PkgFile.LazPackage.IDAsString,
        #13, '"', PkgComponent.PkgFile.Filename, '"']),
      mtError,[mbCancel,mbIgnore],0)<>mrIgnore
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
    if (LazPackage<>ARequiredPackage)
    and (not LazPackage.Requires(PkgComponent.PkgFile.LazPackage))
    then
      Params.Dependency:=ARequiredPackage.CreateDependencyForThisPkg(nil);
  end;

  ModalResult:=mrOk;
end;

procedure TAddToPackageDlg.NewComponentPageResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
begin
  x:=5;
  y:=5;
  
  with AncestorTypeLabel do
    SetBounds(x,y+2,100,Height);
  inc(x,AncestorTypeLabel.Width+5);

  with AncestorComboBox do
    SetBounds(x,y,200,Height);
  inc(x,AncestorComboBox.Width+5);
    
  with AncestorShowAllCheckBox do
    SetBounds(x,y,100,Height);
  x:=5;
  inc(y,AncestorComboBox.Height+5);

  with ClassNameLabel do
    SetBounds(x,y+2,100,Height);
  inc(x,ClassNameLabel.Width+5);

  with ClassNameEdit do
    SetBounds(x,y,200,Height);
  x:=5;
  inc(y,ClassNameEdit.Height+5);

  with PalettePageLabel do
    SetBounds(x,y+2,100,Height);
  inc(x,PalettePageLabel.Width+5);

  with PalettePageCombobox do
    SetBounds(x,y,200,Height);
  x:=5;
  inc(y,PalettePageCombobox.Height+5);

  with ComponentUnitFileLabel do
    SetBounds(x,y+2,100,Height);
  inc(x,ComponentUnitFileLabel.Width+5);

  w:=ComponentUnitFileEdit.Height;
  with ComponentUnitFileEdit do
    SetBounds(x,y,Parent.ClientWidth-x-2-w-2-w-5,Height);
  inc(x,ComponentUnitFileEdit.Width+2);

  with ComponentUnitFileBrowseButton do
    SetBounds(x,y,w,w);
  inc(x,w+2);
  with ComponentUnitFileShortenButton do
    SetBounds(x,y,w,w);
  x:=5;
  inc(y,ComponentUnitFileEdit.Height+5);

  with ComponentUnitNameLabel do
    SetBounds(x,y+2,100,Height);
  inc(x,ComponentUnitNameLabel.Width+5);

  with ComponentUnitNameEdit do
    SetBounds(x,y,100,Height);
  inc(y,ComponentUnitNameEdit.Height+15);
  x:=5;

  with NewComponentButton do
    SetBounds(x,y,80,Height);
  inc(x,NewComponentButton.Width+10);

  with CancelNewComponentButton do
    SetBounds(x,y,80,Height);
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
        MessageDlg(lisProjAddInvalidVersion,
          Format(lisA2PTheMinimumVersionIsInvalidPleaseUseTheFormatMajor, ['"',
            DependMinVersionEdit.Text, '"', #13, #13]),
          mtError,[mbCancel],0);
        exit;
      end;
      NewDependency.Flags:=NewDependency.Flags+[pdfMinVersion];
    end;
    // check maximum version
    if DependMaxVersionEdit.Text<>'' then begin
      if not NewDependency.MaxVersion.ReadString(DependMaxVersionEdit.Text) then
      begin
        MessageDlg(lisProjAddInvalidVersion,
          Format(lisA2PTheMaximumVersionIsInvalidPleaseUseTheFormatMajor, ['"',
            DependMaxVersionEdit.Text, '"', #13, #13]),
          mtError,[mbCancel],0);
        exit;
      end;
      NewDependency.Flags:=NewDependency.Flags+[pdfMaxVersion];
    end;
    
    NewDependency.PackageName:=DependPkgNameComboBox.Text;
    if not CheckAddingDependency(LazPackage,NewDependency) then exit;

    // ok
    Params.Dependency:=NewDependency;
    NewDependency:=nil;

    ModalResult:=mrOk;
  finally
    NewDependency.Free;
  end;
end;

procedure TAddToPackageDlg.NewDependPageResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
begin
  x:=5;
  y:=5;
  
  with DependPkgNameLabel do
    SetBounds(x,y+3,110,Height);

  with DependPkgNameComboBox do
    SetBounds(x+DependPkgNameLabel.Width+5,y,150,Height);
  inc(y,DependPkgNameComboBox.Height+5);

  with DependMinVersionLabel do
    SetBounds(x,y+3,170,Height);

  with DependMinVersionEdit do
    SetBounds(x+DependMinVersionLabel.Width+5,y,100,Height);
  inc(y,DependMinVersionEdit.Height+5);

  with DependMaxVersionLabel do
    SetBounds(x,y+3,DependMinVersionLabel.Width,Height);

  with DependMaxVersionEdit do
    SetBounds(x+DependMaxVersionLabel.Width+5,y,
              DependMinVersionEdit.Width,Height);
  inc(y,DependMaxVersionEdit.Height+20);

  with NewDependButton do
    SetBounds(x,y,80,Height);

  with CancelDependButton do
    SetBounds(x+NewDependButton.Width+10,y,80,Height);
end;

procedure TAddToPackageDlg.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
  UpdateAvailableAncestorTypes;
  UpdateAvailablePageNames;
  UpdateAvailableDependencyNames;
end;

procedure TAddToPackageDlg.SetupComponents;
begin
  NoteBook:=TNoteBook.Create(Self);
  with NoteBook do begin
    Name:='NoteBook';
    Parent:=Self;
    Pages.Add(lisA2PAddUnit);
    AddUnitPage:=Page[0];
    Pages.Add(lisA2PNewComponent);
    NewComponentPage:=Page[1];
    Pages.Add(lisProjAddNewRequirement);
    NewDependPage:=Page[2];
    Pages.Add(lisA2PAddFile);
    AddFilePage:=Page[3];
    Pages.Add(lisA2PAddFiles);
    AddFilesPage:=Page[4];
    PageIndex:=0;
    Align:=alClient;
  end;
  
  AddUnitPage.OnResize:=@AddUnitPageResize;
  NewComponentPage.OnResize:=@NewComponentPageResize;
  NewDependPage.OnResize:=@NewDependPageResize;
  AddFilePage.OnResize:=@AddFilePageResize;
  AddFilesPage.OnResize:=@AddFilesPageResize;

  SetupAddUnitPage;
  SetupNewComponentPage;
  SetupAddDependencyPage;
  SetupAddFilePage;
  SetupAddFilesPage;
end;

procedure TAddToPackageDlg.SetupAddUnitPage;
begin
  AddUnitFilenameLabel:=TLabel.Create(Self);
  with AddUnitFilenameLabel do begin
    Name:='AddUnitFilenameLabel';
    Parent:=AddUnitPage;
    Caption:=lisA2PUnitFileName;
  end;

  AddUnitFilenameEdit:=TEdit.Create(Self);
  with AddUnitFilenameEdit do begin
    Name:='AddUnitFilenameEdit';
    Parent:=AddUnitPage;
    Text:=lisA2PchooseAnExistingFile;
  end;

  AddUnitFileBrowseButton:=TButton.Create(Self);
  with AddUnitFileBrowseButton do begin
    Name:='AddUnitFileBrowseButton';
    Parent:=AddUnitPage;
    Caption:='...';
    OnClick:=@AddUnitFileBrowseButtonClick;
  end;

  AddUnitFileShortenButton:=TButton.Create(Self);
  with AddUnitFileShortenButton do begin
    Name:='AddUnitFileShortenButton';
    Parent:=AddUnitPage;
    Caption:='<>';
    OnClick:=@AddUnitFileShortenButtonClick;
  end;

  AddUnitSrcNameLabel:=TLabel.Create(Self);
  with AddUnitSrcNameLabel do begin
    Name:='AddUnitSrcNameLabel';
    Parent:=AddUnitPage;
    Caption:=lisAF2PUnitName;
  end;

  AddUnitSrcNameEdit:=TEdit.Create(Self);
  with AddUnitSrcNameEdit do begin
    Name:='AddUnitSrcNameEdit';
    Parent:=AddUnitPage;
    Text:='';
  end;

  AddUnitHasRegisterCheckBox:=TCheckBox.Create(Self);
  with AddUnitHasRegisterCheckBox do begin
    Name:='AddUnitHasRegisterCheckBox';
    Parent:=AddUnitPage;
    Caption:=lisAF2PHasRegisterProcedure;
  end;

  AddUnitIsVirtualCheckBox:=TCheckBox.Create(Self);
  with AddUnitIsVirtualCheckBox do begin
    Name:='AddUnitIsVirtualCheckBox';
    Parent:=AddUnitPage;
    Caption:=lisAF2PIsVirtualUnit;
    OnClick:=@AddUnitIsVirtualCheckBoxClick;
  end;

  AddSecondaryFilesCheckBox:=TCheckBox.Create(Self);
  with AddSecondaryFilesCheckBox do begin
    Name:='AddSecondaryFilesCheckBox';
    Parent:=AddUnitPage;
    Caption:=lisA2PAddLFMLRSFilesIfTheyExist;
    Checked:=true;
  end;

  AddUnitUpdateButton:=TButton.Create(Self);
  with AddUnitUpdateButton do begin
    Name:='AddUnitUpdateButton';
    Parent:=AddUnitPage;
    Caption:=lisA2PUpdateUnitNameAndHasRegisterProcedure;
    OnClick:=@AddUnitUpdateButtonClick;
  end;

  AddUnitButton:=TButton.Create(Self);
  with AddUnitButton do begin
    Name:='AddUnitButton';
    Parent:=AddUnitPage;
    Caption:=lisLazBuildOk;
    OnClick:=@AddUnitButtonClick;
  end;

  CancelAddUnitButton:=TButton.Create(Self);
  with CancelAddUnitButton do begin
    Name:='CancelAddUnitButton';
    Parent:=AddUnitPage;
    Caption:=dlgCancel;
    OnClick:=@CancelAddUnitButtonClick;
  end;
end;

procedure TAddToPackageDlg.SetupNewComponentPage;
begin
  AncestorTypeLabel:=TLabel.Create(Self);
  with AncestorTypeLabel do begin
    Name:='AncestorTypeLabel';
    Parent:=NewComponentPage;
    Caption:=lisA2PAncestorType;
  end;

  AncestorComboBox:=TComboBox.Create(Self);
  with AncestorComboBox do begin
    Name:='AncestorComboBox';
    Parent:=NewComponentPage;
    Text:='';
    OnCloseUp:=@AncestorComboBoxCloseUp;
  end;

  AncestorShowAllCheckBox:=TCheckBox.Create(Self);
  with AncestorShowAllCheckBox do begin
    Name:='AncestorShowAllCheckBox';
    Parent:=NewComponentPage;
    Text:=lisA2PShowAll;
    Checked:=true;
    OnClick:=@AncestorShowAllCheckBoxClick;
  end;

  ClassNameLabel:=TLabel.Create(Self);
  with ClassNameLabel do begin
    Name:='ClassNameLabel';
    Parent:=NewComponentPage;
    Caption:=lisA2PNewClassName;
  end;

  ClassNameEdit:=TEdit.Create(Self);
  with ClassNameEdit do begin
    Name:='ClassNameEdit';
    Parent:=NewComponentPage;
    Text:='';
    OnChange:=@ClassNameEditChange;
  end;

  PalettePageLabel:=TLabel.Create(Self);
  with PalettePageLabel do begin
    Name:='PalettePageLabel';
    Parent:=NewComponentPage;
    Caption:=lisA2PPalettePage;
  end;

  PalettePageCombobox:=TCombobox.Create(Self);
  with PalettePageCombobox do begin
    Name:='PalettePageCombobox';
    Parent:=NewComponentPage;
    Text:='';
  end;

  ComponentUnitFileLabel:=TLabel.Create(Self);
  with ComponentUnitFileLabel do begin
    Name:='ComponentUnitFileLabel';
    Parent:=NewComponentPage;
    Caption:=lisA2PUnitFileName2;
  end;

  ComponentUnitFileEdit:=TEdit.Create(Self);
  with ComponentUnitFileEdit do begin
    Name:='ComponentUnitFileEdit';
    Parent:=NewComponentPage;
    Text:='';
  end;

  ComponentUnitFileBrowseButton:=TButton.Create(Self);
  with ComponentUnitFileBrowseButton do begin
    Name:='ComponentUnitFileBrowseButton';
    Parent:=NewComponentPage;
    Caption:='...';
    OnClick:=@ComponentUnitFileBrowseButtonClick;
  end;

  ComponentUnitFileShortenButton:=TButton.Create(Self);
  with ComponentUnitFileShortenButton do begin
    Name:='ComponentUnitFileShortenButton';
    Parent:=NewComponentPage;
    Caption:='<>';
    OnClick:=@ComponentUnitFileShortenButtonClick;
  end;

  ComponentUnitNameLabel:=TLabel.Create(Self);
  with ComponentUnitNameLabel do begin
    Name:='ComponentUnitNameLabel';
    Parent:=NewComponentPage;
    Caption:=lisA2PUnitName;
  end;

  ComponentUnitNameEdit:=TEdit.Create(Self);
  with ComponentUnitNameEdit do begin
    Name:='ComponentUnitNameEdit';
    Parent:=NewComponentPage;
    Text:='';
  end;

  NewComponentButton:=TButton.Create(Self);
  with NewComponentButton do begin
    Name:='NewComponentButton';
    Parent:=NewComponentPage;
    Caption:=lisLazBuildOk;
    OnClick:=@NewComponentButtonClick;
  end;

  CancelNewComponentButton:=TButton.Create(Self);
  with CancelNewComponentButton do begin
    Name:='CancelNewComponentButton';
    Parent:=NewComponentPage;
    Caption:=dlgCancel;
    OnClick:=@CancelNewComponentButtonClick;
  end;
end;

procedure TAddToPackageDlg.SetupAddDependencyPage;
begin
  DependPkgNameLabel:=TLabel.Create(Self);
  with DependPkgNameLabel do begin
    Name:='DependPkgNameLabel';
    Parent:=NewDependPage;
    Caption:=lisProjAddPackageName;
  end;

  DependPkgNameComboBox:=TComboBox.Create(Self);
  with DependPkgNameComboBox do begin
    Name:='DependPkgNameComboBox';
    Parent:=NewDependPage;
    Text:='';
  end;

  DependMinVersionLabel:=TLabel.Create(Self);
  with DependMinVersionLabel do begin
    Name:='DependMinVersionLabel';
    Parent:=NewDependPage;
    Caption:=lisProjAddMinimumVersionOptional;
  end;

  DependMinVersionEdit:=TEdit.Create(Self);
  with DependMinVersionEdit do begin
    Name:='DependMinVersionEdit';
    Parent:=NewDependPage;
    Text:='';
  end;

  DependMaxVersionLabel:=TLabel.Create(Self);
  with DependMaxVersionLabel do begin
    Name:='DependMaxVersionLabel';
    Parent:=NewDependPage;
    Caption:=lisProjAddMaximumVersionOptional;
  end;

  DependMaxVersionEdit:=TEdit.Create(Self);
  with DependMaxVersionEdit do begin
    Name:='DependMaxVersionEdit';
    Parent:=NewDependPage;
    Text:='';
  end;

  NewDependButton:=TButton.Create(Self);
  with NewDependButton do begin
    Name:='NewDependButton';
    Parent:=NewDependPage;
    Caption:=lisLazBuildOk;
    OnClick:=@NewDependButtonClick;
  end;

  CancelDependButton:=TButton.Create(Self);
  with CancelDependButton do begin
    Name:='CancelDependButton';
    Parent:=NewDependPage;
    Caption:=dlgCancel;
    ModalResult:=mrCancel;
  end;
end;

procedure TAddToPackageDlg.SetupAddFilePage;
var
  pft: TPkgFileType;
begin
  AddFilenameLabel:=TLabel.Create(Self);
  with AddFilenameLabel do begin
    Name:='AddFilenameLabel';
    Parent:=AddFilePage;
    Caption:=lisA2PFileName;
  end;

  AddFilenameEdit:=TEdit.Create(Self);
  with AddFilenameEdit do begin
    Name:='AddFilenameEdit';
    Parent:=AddFilePage;
    Text:=lisA2PchooseAnExistingFile;
  end;

  AddFileBrowseButton:=TButton.Create(Self);
  with AddFileBrowseButton do begin
    Name:='AddFileBrowseButton';
    Parent:=AddFilePage;
    Caption:='...';
    OnClick:=@AddFileBrowseButtonClick;
  end;

  AddFileShortenButton:=TButton.Create(Self);
  with AddFileShortenButton do begin
    Name:='AddFileShortenButton';
    Parent:=AddFilePage;
    Caption:='<>';
    OnClick:=@AddFileShortenButtonClick;
  end;

  AddFileTypeRadioGroup:=TRadioGroup.Create(Self);
  with AddFileTypeRadioGroup do begin
    Name:='AddFileTypeRadioGroup';
    Parent:=AddFilePage;
    Caption:=lisAF2PFileType;
    with Items do begin
      BeginUpdate;
      for pft:=Low(TPkgFileType) to High(TPkgFileType) do begin
        if pft=pftVirtualUnit then continue;
        Add(GetPkgFileTypeLocalizedName(pft));
      end;
      EndUpdate;
    end;
  end;

  AddFileButton:=TButton.Create(Self);
  with AddFileButton do begin
    Name:='AddFileButton';
    Parent:=AddFilePage;
    Caption:=lisLazBuildOk;
    OnClick:=@AddFileButtonClick;
  end;

  CancelAddFileButton:=TButton.Create(Self);
  with CancelAddFileButton do begin
    Name:='CancelAddFileButton';
    Parent:=AddFilePage;
    Caption:=dlgCancel;
    OnClick:=@CancelAddFileButtonClick;
  end;
end;

procedure TAddToPackageDlg.SetupAddFilesPage;
var
  CurColumn: TListColumn;
begin
  FilesListView:=TListView.Create(Self);
  with FilesListView do begin
    Name:='FilesListView';
    Parent:=AddFilesPage;
    MultiSelect:=true;
    ViewStyle:=vsReport;
    CurColumn:=Columns.Add;
    CurColumn.Width:=200;
    CurColumn.Caption:=lisA2PFilename2;
    CurColumn:=Columns.Add;
    CurColumn.Caption:=dlgEnvType;
  end;
  
  FilesBrowseButton:=TButton.Create(Self);
  with FilesBrowseButton do begin
    Name:='FilesBrowseButton';
    Parent:=AddFilesPage;
    Caption:=lisPathEditBrowse;
    OnClick:=@FilesBrowseButtonClick;
  end;
  
  FilesShortenButton:=TButton.Create(Self);
  with FilesShortenButton do begin
    Name:='FilesShortenButton';
    Parent:=AddFilesPage;
    Caption:=lisA2PSwitchPaths;
    OnClick:=@FilesShortenButtonClick;
  end;

  FilesDeleteButton:=TButton.Create(Self);
  with FilesDeleteButton do begin
    Name:='FilesDeleteButton';
    Parent:=AddFilesPage;
    Caption:=dlgEdDelete;
    OnClick:=@FilesDeleteButtonClick;
  end;
  
  FilesAddButton:=TButton.Create(Self);
  with FilesAddButton do begin
    Name:='FilesAddButton';
    Parent:=AddFilesPage;
    Caption:=lisA2PAddFilesToPackage;
    OnClick:=@FilesAddButtonClick;
  end;
end;

procedure TAddToPackageDlg.OnIterateComponentClasses(PkgComponent: TPkgComponent
  );
begin
  if fPkgComponents.Find(PkgComponent)=nil then
    fPkgComponents.Add(PkgComponent);
end;

procedure TAddToPackageDlg.OnIteratePackages(APackageID: TLazPackageID);
begin
  if (APackageID<>LazPackage) and (fPackages.Find(APackageID)=nil) then
    fPackages.Add(APackageID);
end;

procedure TAddToPackageDlg.AutoCompleteNewComponent;
var
  PkgComponent: TPkgComponent;
begin
  fLastNewComponentAncestorType:=AncestorComboBox.Text;
  if not IsValidIdent(fLastNewComponentAncestorType) then exit;
  PkgComponent:=TPkgComponent(
    IDEComponentPalette.FindComponent(fLastNewComponentAncestorType));
  if PkgComponent=nil then exit;
  
  // create unique classname
  ClassNameEdit.Text:=IDEComponentPalette.CreateNewClassName(
                                                 fLastNewComponentAncestorType);
  // choose the same page name
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
  if EnvironmentOptions.PascalFileAutoLowerCase
  or EnvironmentOptions.PascalFileAskLowerCase then
    NewFileName:=lowercase(NewFileName);
  // append pascal file extension
  NewFileName:=NewFileName+
       +EnvironmentOpts.PascalExtension[EnvironmentOptions.PascalFileExtension];
  // prepend path
  if LazPackage.HasDirectory then
    NewFileName:=LazPackage.Directory+NewFileName;
  ComponentUnitFileEdit.Text:=NewFileName;
end;

procedure TAddToPackageDlg.UpdateAddUnitInfo;
var
  AnUnitName: string;
  HasRegisterProc: boolean;
  Filename: String;
begin
  if Assigned(OnGetUnitRegisterInfo) and not AddUnitIsVirtualCheckBox.Checked
  then begin
    Filename:=AddUnitFilenameEdit.Text;
    LazPackage.LongenFilename(Filename);
    OnGetUnitRegisterInfo(Self,Filename,AnUnitName,HasRegisterProc);
    AddUnitSrcNameEdit.Text:=AnUnitName;
    AddUnitHasRegisterCheckBox.Checked:=HasRegisterProc;
  end;
end;

procedure TAddToPackageDlg.UpdateAddFileInfo;
var
  CurFilename: String;
  NewPFT: TPkgFileType;
  CurPFT: TPkgFileType;
  i: Integer;
begin
  CurFilename:=AddFilenameEdit.Text;
  NewPFT:=FileNameToPkgFileType(CurFilename);
  if NewPFT=pftVirtualUnit then NewPFT:=pftUnit;
  i:=0;
  for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
    if CurPFT=pftVirtualUnit then continue;
    if CurPFT=NewPFT then break;
    inc(i);
  end;
  AddFileTypeRadioGroup.ItemIndex:=i;
end;

function TAddToPackageDlg.FileNameToPkgFileType(const AFilename: string
  ): TPkgFileType;
begin
  if CompareFileExt(AFilename,'.lfm',true)=0 then
    Result:=pftLFM
  else if CompareFileExt(AFilename,'.lrs',true)=0 then
    Result:=pftLRS
  else if CompareFileExt(AFilename,'.inc',true)=0 then
    Result:=pftInclude
  else if CompareFileExt(AFilename,'.pas',true)=0 then
    Result:=pftUnit
  else if CompareFileExt(AFilename,'.pp',true)=0 then
    Result:=pftUnit
  else if FileIsText(AFilename) then
    Result:=pftText
  else
    Result:=pftBinary;
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
    Result:=TrimFilename(CreateAbsolutePath(Filename,LazPackage.Directory));
end;

constructor TAddToPackageDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Name:='AddToPackageDlg';
  Caption:=lisA2PAddToPackage;
  fPkgComponents:=TAVLTree.Create(@CompareIDEComponentByClassName);
  fPackages:=TAVLTree.Create(@CompareLazPackageID);
  Params:=TAddToPkgResult.Create;
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,500,300);
  SetupComponents;
  OnClose:=@AddToPackageDlgClose;
end;

destructor TAddToPackageDlg.Destroy;
begin
  FreeAndNil(fPkgComponents);
  FreeAndNil(fPackages);
  FreeAndNil(Params);
  inherited Destroy;
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
  // put them into the combobox
  sl:=TStringList.Create;
  ANode:=fPkgComponents.FindLowest;
  while ANode<>nil do begin
    sl.Add(TPkgComponent(ANode.Data).ComponentClass.ClassName);
    ANode:=fPkgComponents.FindSuccessor(ANode);
  end;
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
  UnitName:='';
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

