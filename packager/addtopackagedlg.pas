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
  Dialogs, LazarusIDEStrConsts, IDEOptionDefs, InputHistory, FileCtrl, AVL_Tree,
  CodeToolManager, IDEDefs, IDEProcs, EnvironmentOpts, PackageSystem,
  PackageDefs, ComponentReg;
  
type
  TAddToPkgType = (
    d2ptUnit,
    d2ptNewComponent,
    d2ptRequiredPkg
    );
    
  TAddToPkgResult = record
    AddType: TAddToPkgType;
    Dependency: TPkgDependency;
    UnitFilename: string;
    UnitName: string;
    AncestorType: string;
    ClassName: string;
    PageName: string;
    FileType: TPkgFileType;
    PkgFileFlags: TPkgFileFlags;
    UsedUnitname: string;
  end;

  TAddToPackageDlg = class(TForm)
    // notebook
    NoteBook: TNoteBook;
    AddUnitPage: TPage;
    NewComponentPage: TPage;
    NewDependPage: TPage;
    // add unit page
    AddUnitFilenameLabel: TLabel;
    AddUnitFilenameEdit: TEdit;
    AddUnitFileBrowseButton: TButton;
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
    ComponentUnitFileButton: TButton;
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
    procedure AddToPackageDlgResize(Sender: TObject);
    procedure AddUnitButtonClick(Sender: TObject);
    procedure AddUnitFileBrowseButtonClick(Sender: TObject);
    procedure AddUnitPageResize(Sender: TObject);
    procedure AncestorComboBoxCloseUp(Sender: TObject);
    procedure AncestorShowAllCheckBoxClick(Sender: TObject);
    procedure CancelAddUnitButtonClick(Sender: TObject);
    procedure CancelNewComponentButtonClick(Sender: TObject);
    procedure ClassNameEditChange(Sender: TObject);
    procedure ComponentUnitFileButtonClick(Sender: TObject);
    procedure NewComponentButtonClick(Sender: TObject);
    procedure NewComponentPageResize(Sender: TObject);
    procedure NewDependButtonClick(Sender: TObject);
    procedure NewDependPageResize(Sender: TObject);
  private
    fLastNewComponentAncestorType: string;
    fLastNewComponentClassName: string;
    FLazPackage: TLazPackage;
    FOnGetIDEFileInfo: TGetIDEFileStateEvent;
    fPkgComponents: TAVLTree;// tree of TPkgComponent
    fPackages: TAVLTree;// tree of  TLazPackage or TPackageLink
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure SetupComponents;
    procedure OnIterateComponentClasses(PkgComponent: TPkgComponent);
    procedure OnIteratePackages(APackageID: TLazPackageID);
    procedure AutoCompleteNewComponent;
    procedure AutoCompleteNewComponentUnitName;
    function CheckUnitFilename(AddFileType: TAddToPkgType;
      var AFilename: string): boolean;
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
  end;
  
function ShowAddToPackageDlg(Pkg: TLazPackage; var Params: TAddToPkgResult;
  OnGetIDEFileInfo: TGetIDEFileStateEvent): TModalResult;


implementation


function ShowAddToPackageDlg(Pkg: TLazPackage; var Params: TAddToPkgResult;
  OnGetIDEFileInfo: TGetIDEFileStateEvent): TModalResult;
var
  AddDlg: TAddToPackageDlg;
begin
  AddDlg:=TAddToPackageDlg.Create(Application);
  AddDlg.LazPackage:=Pkg;
  AddDlg.OnGetIDEFileInfo:=OnGetIDEFileInfo;
  Result:=AddDlg.ShowModal;
  if Result=mrOk then
    Params:=AddDlg.Params;
  IDEDialogLayoutList.SaveLayout(AddDlg);
  AddDlg.Free;
end;

{ TAddToPackageDlg }

procedure TAddToPackageDlg.AddToPackageDlgResize(Sender: TObject);
begin

end;

procedure TAddToPackageDlg.AddUnitButtonClick(Sender: TObject);
begin
  Params.AddType:=d2ptUnit;

  Params.UnitFilename:=AddUnitFilenameEdit.Text;
  if not CheckUnitFilename(Params.AddType,Params.UnitFilename) then exit;
  Params.UnitName:=ExtractFileNameOnly(Params.UnitFilename);
  Params.FileType:=pftUnit;
  Params.PkgFileFlags:=[];

  // add it ...
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
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options+[ofFileMustExist,ofPathMustExist];
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      if FileExists(AFilename) then begin
        LazPackage.ShortenFilename(AFilename);
        AddUnitFilenameEdit.Text:=AFilename;
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TAddToPackageDlg.AddUnitPageResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
begin
  x:=5;
  y:=5;
  with AddUnitFilenameLabel do
    SetBounds(x,y+2,100,Height);
  inc(x,AddUnitFilenameLabel.Width+5);

  with AddUnitFilenameEdit do
    SetBounds(x,y,Parent.ClientWidth-x-30,Height);
  inc(x,AddUnitFilenameEdit.Width+2);

  with AddUnitFileBrowseButton do
    SetBounds(x,y,AddUnitFilenameEdit.Height,AddUnitFilenameEdit.Height);
  x:=5;
  y:=AddUnitFilenameEdit.Top+AddUnitFilenameEdit.Height+15;

  with AddUnitButton do
    SetBounds(x,y,80,Height);
  inc(x,AddUnitButton.Width+10);

  with CancelAddUnitButton do
    SetBounds(x,y,80,Height);
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

procedure TAddToPackageDlg.ComponentUnitFileButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  AFilename: string;
begin
  OpenDialog:=TOpenDialog.Create(Application);
  try
    InputHistories.ApplyFileDialogSettings(OpenDialog);
    OpenDialog.Title:=lisOpenFile;
    OpenDialog.Options:=OpenDialog.Options+[ofPathMustExist];
    if OpenDialog.Execute then begin
      AFilename:=CleanAndExpandFilename(OpenDialog.Filename);
      if FilenameIsPascalUnit(AFilename) then begin
        LazPackage.ShortenFilename(AFilename);
        ComponentUnitFileEdit.Text:=AFilename;
      end else begin
        MessageDlg('Invalid file',
         'A pascal unit must have the extension .pp or .pas',
         mtError,[mbCancel],0);
      end;
    end;
    InputHistories.StoreFileDialogSettings(OpenDialog);
  finally
    OpenDialog.Free;
  end;
end;

procedure TAddToPackageDlg.NewComponentButtonClick(Sender: TObject);
var
  PkgFile: TPkgFile;
  PkgComponent: TPkgComponent;
  ARequiredPackage: TLazPackage;
begin
  Params.AddType:=d2ptNewComponent;
  Params.FileType:=pftUnit;
  Params.PkgFileFlags:=[pffHasRegisterProc];
  Params.AncestorType:=AncestorComboBox.Text;
  Params.ClassName:=ClassNameEdit.Text;
  Params.PageName:=PalettePageCombobox.Text;
  Params.UnitName:=ComponentUnitNameEdit.Text;
  Params.UnitFilename:=ComponentUnitFileEdit.Text;
  Params.UsedUnitname:='';

  // check Ancestor Type
  if not IsValidIdent(Params.AncestorType) then begin
    MessageDlg('Invalid Ancestor Type',
      'The ancestor type "'+Params.AncestorType+'"'
      +' is not a valid pascal identifier.',
      mtError,[mbCancel],0);
    exit;
  end;

  // check pagename
  if length(Params.PageName)>100 then begin
    MessageDlg('Page Name too long',
      'The page name "'+Params.PageName+'" is too long (max 100 chars).',
      mtError,[mbCancel],0);
    exit;
  end;

  // check unitname - filename redundancy
  if AnsiCompareText(Params.Unitname,ExtractFileNameOnly(Params.UnitFilename))<>0
  then begin
    MessageDlg('Unit Name Invalid',
      'The unit name "'+Params.UnitName+'" does not correspond to the filename.',
      mtError,[mbCancel],0);
    exit;
  end;

  // check classname
  if not IsValidIdent(Params.ClassName) then begin
    MessageDlg('Invalid Class Name',
      'The class name "'+Params.ClassName+'" is not a valid pascal identifier.',
      mtError,[mbCancel],0);
    exit;
  end;

  // check classname<>ancestortype
  if AnsiCompareText(Params.ClassName,Params.AncestorType)=0 then begin
    MessageDlg('Invalid Circle',
      'The class name "'+Params.ClassName+'" and ancestor type "'
      +Params.AncestorType+'" are the same.',
      mtError,[mbCancel],0);
    exit;
  end;

  // check ancestor type is not unitname
  PkgFile:=PackageGraph.FindUnit(LazPackage,Params.AncestorType,true);
  if PkgFile<>nil then begin
    if MessageDlg('Ambigious Ancestor Type',
      'The ancestor type "'+Params.AncestorType+'" has the same name as'#13
      +'the unit "'+PkgFile.Filename+'".',
      mtError,[mbCancel,mbIgnore],0)<>mrIgnore
    then
      exit;
  end;
  
  // check classname does not interfere with an existing unitname
  PkgFile:=PackageGraph.FindUnit(LazPackage,Params.ClassName,true);
  if PkgFile<>nil then begin
    if MessageDlg('Ambigious Class Name',
      'The class name "'+Params.AncestorType+'" has the same name as'#13
      +'the unit "'+PkgFile.Filename+'".',
      mtError,[mbCancel,mbIgnore],0)<>mrIgnore
    then
      exit;
  end;

  // check if classname already exists
  PkgComponent:=
    TPkgComponent(IDEComponentPalette.FindComponent(Params.Classname));
  if PkgComponent<>nil then begin
    if MessageDlg('Class Name already exists',
      'The class name "'+Params.ClassName+'" exists already in'#13
      +'Package '+PkgComponent.PkgFile.LazPackage.IDAsString+#13
      +'File: "'+PkgComponent.PkgFile.Filename+'"',
      mtError,[mbCancel,mbIgnore],0)<>mrIgnore
    then
      exit;
  end;

  // check filename
  if not CheckUnitFilename(Params.AddType,Params.UnitFilename) then exit;

  // create dependency if needed
  PkgComponent:=
    TPkgComponent(IDEComponentPalette.FindComponent(Params.AncestorType));
  if PkgComponent<>nil then begin
    Params.UsedUnitname:=PkgComponent.GetUnitName;
    ARequiredPackage:=PkgComponent.PkgFile.LazPackage;
    if (LazPackage<>ARequiredPackage)
    and (not LazPackage.Requires(PkgComponent.PkgFile.LazPackage))
    then
      Params.Dependency:=ARequiredPackage.CreateDependencyForThisPkg;
  end;

  ModalResult:=mrOk;
end;

procedure TAddToPackageDlg.NewComponentPageResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
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

  with ComponentUnitFileEdit do
    SetBounds(x,y,Parent.ClientWidth-x-Height-5,Height);
  inc(x,ComponentUnitFileEdit.Width+2);

  with ComponentUnitFileButton do
    SetBounds(x,y,ComponentUnitFileEdit.Height,ComponentUnitFileEdit.Height);
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
  NewPckName: String;
begin
  NewDependency:=TPkgDependency.Create;
  try
    // check minimum version
    if DependMinVersionEdit.Text<>'' then begin
      if not NewDependency.MinVersion.ReadString(DependMinVersionEdit.Text) then
      begin
        MessageDlg('Invalid version',
          'The Minimum Version "'+DependMinVersionEdit.Text+'" is invalid.'#13
          +'Please use the format major.minor.build.release'#13
          +'For exmaple: 1.0.20.10',
          mtError,[mbCancel],0);
        exit;
      end;
      NewDependency.Flags:=NewDependency.Flags+[pdfMinVersion];
    end;
    // check maximum version
    if DependMaxVersionEdit.Text<>'' then begin
      if not NewDependency.MaxVersion.ReadString(DependMaxVersionEdit.Text) then
      begin
        MessageDlg('Invalid version',
          'The Maximum Version "'+DependMaxVersionEdit.Text+'" is invalid.'#13
          +'Please use the format major.minor.build.release'#13
          +'For exmaple: 1.0.20.10',
          mtError,[mbCancel],0);
        exit;
      end;
      NewDependency.Flags:=NewDependency.Flags+[pdfMaxVersion];
    end;
    // check packagename
    NewPckName:=DependPkgNameComboBox.Text;
    if not IsValidIdent(NewPckName) then begin
      MessageDlg('Invalid packagename',
        'The packagename "'+NewPckName+'" is invalid.'#13
        +'Plase choose an existing package.',
        mtError,[mbCancel],0);
      exit;
    end;
    NewDependency.PackageName:=NewPckName;
    if PackageGraph.FindWithDependency(NewDependency,fpfSearchPackageEverywhere)
      =nil then
    begin
      MessageDlg('Package not found',
        'The packagename "'+DependPkgNameComboBox.Text+'" was not found.'#13
        +'Please choose an existing package.',
        mtError,[mbCancel],0);
      exit;
    end;
    
    // ok
    Params.Dependency:=NewDependency;
    NewDependency:=nil;
    Params.AddType:=d2ptRequiredPkg;

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
    Pages.Add('Add Unit');
    AddUnitPage:=Page[0];
    Pages.Add('New Component');
    NewComponentPage:=Page[1];
    Pages.Add('New Requirement');
    NewDependPage:=Page[2];
    PageIndex:=0;
    Align:=alClient;
  end;
  
  // add unit
  
  AddUnitPage.OnResize:=@AddUnitPageResize;
  NewComponentPage.OnResize:=@NewComponentPageResize;
  NewDependPage.OnResize:=@NewDependPageResize;

  AddUnitFilenameLabel:=TLabel.Create(Self);
  with AddUnitFilenameLabel do begin
    Name:='AddUnitFilenameLabel';
    Parent:=AddUnitPage;
    Caption:='Unit file name:';
  end;
  
  AddUnitFilenameEdit:=TEdit.Create(Self);
  with AddUnitFilenameEdit do begin
    Name:='AddUnitFilenameEdit';
    Parent:=AddUnitPage;
    Text:='<choose an existing file>';
  end;

  AddUnitFileBrowseButton:=TButton.Create(Self);
  with AddUnitFileBrowseButton do begin
    Name:='AddUnitFileBrowseButton';
    Parent:=AddUnitPage;
    Caption:='...';
    OnClick:=@AddUnitFileBrowseButtonClick;
  end;

  AddUnitButton:=TButton.Create(Self);
  with AddUnitButton do begin
    Name:='AddUnitButton';
    Parent:=AddUnitPage;
    Caption:='Ok';
    OnClick:=@AddUnitButtonClick;
  end;

  CancelAddUnitButton:=TButton.Create(Self);
  with CancelAddUnitButton do begin
    Name:='CancelAddUnitButton';
    Parent:=AddUnitPage;
    Caption:='Cancel';
    OnClick:=@CancelAddUnitButtonClick;
  end;
  

  // add new component unit

  AncestorTypeLabel:=TLabel.Create(Self);
  with AncestorTypeLabel do begin
    Name:='AncestorTypeLabel';
    Parent:=NewComponentPage;
    Caption:='Ancestor Type';
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
    Text:='Show all';
    Checked:=true;
    OnClick:=@AncestorShowAllCheckBoxClick;
  end;

  ClassNameLabel:=TLabel.Create(Self);
  with ClassNameLabel do begin
    Name:='ClassNameLabel';
    Parent:=NewComponentPage;
    Caption:='New class name:';
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
    Caption:='Palette Page:';
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
    Caption:='Unit File Name:';
  end;

  ComponentUnitFileEdit:=TEdit.Create(Self);
  with ComponentUnitFileEdit do begin
    Name:='ComponentUnitFileEdit';
    Parent:=NewComponentPage;
    Text:='';
  end;

  ComponentUnitFileButton:=TButton.Create(Self);
  with ComponentUnitFileButton do begin
    Name:='ComponentUnitFileButton';
    Parent:=NewComponentPage;
    Caption:='...';
    OnClick:=@ComponentUnitFileButtonClick;
  end;

  ComponentUnitNameLabel:=TLabel.Create(Self);
  with ComponentUnitNameLabel do begin
    Name:='ComponentUnitNameLabel';
    Parent:=NewComponentPage;
    Caption:='Unit Name:';
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
    Caption:='Ok';
    OnClick:=@NewComponentButtonClick;
  end;

  CancelNewComponentButton:=TButton.Create(Self);
  with CancelNewComponentButton do begin
    Name:='CancelNewComponentButton';
    Parent:=NewComponentPage;
    Caption:='Cancel';
    OnClick:=@CancelNewComponentButtonClick;
  end;


  // add require
  
  DependPkgNameLabel:=TLabel.Create(Self);
  with DependPkgNameLabel do begin
    Name:='DependPkgNameLabel';
    Parent:=NewDependPage;
    Caption:='Package Name:';
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
    Caption:='Minimum Version (optional):';
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
    Caption:='Maximum Version (optional):';
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
    Caption:='Ok';
    OnClick:=@NewDependButtonClick;
  end;

  CancelDependButton:=TButton.Create(Self);
  with CancelDependButton do begin
    Name:='CancelDependButton';
    Parent:=NewDependPage;
    Caption:='Cancel';
    ModalResult:=mrCancel;
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

function TAddToPackageDlg.CheckUnitFilename(AddFileType: TAddToPkgType;
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
  if (not FilenameIsAbsolute(AFilename)) then begin
    if LazPackage.HasDirectory then
      AFilename:=LazPackage.Directory+AFilename
    else begin
      MessageDlg('Invalid filename',
        'The filename "'+AFilename+'" is ambigious.'#13
        +'Please specifiy a filename with full path.',
        mtError,[mbCancel],0);
      exit;
    end;
  end;
  
  // check if file exists
  if not FileExists(AFilename) then begin
    if AddFileType=d2ptUnit then begin
      MessageDlg('File not found',
        'File "'+AFilename+'" not found.',mtError,[mbCancel],0);
      exit;
    end;
  end;
  
  // check file extension
  if not FilenameIsPascalUnit(AFilename) then begin
    MessageDlg('File not unit',
      'Pascal units must have the extension .pp or .pas',
      mtWarning,[mbCancel],0);
    exit;
  end;
  
  // check unitname
  AnUnitName:=ExtractFileNameOnly(AFilename);
  if not IsValidIdent(AnUnitName) then begin
    MessageDlg('File not unit',
      +'"'+AnUnitName+'" is not a valid unit name.',
      mtWarning,[mbCancel],0);
    exit;
  end;
  
  // check if unitname already exists in package
  PkgFile:=PackageGraph.FindUnit(LazPackage,AnUnitName,true);
  if PkgFile<>nil then begin
    if PkgFile.LazPackage=LazPackage then begin
      MessageDlg('Unitname already exists',
        'The unitname "'+AnUnitName+'" already exists in this package.',
        mtError,[mbCancel],0);
      exit;
    end else begin
      if MessageDlg('Unitname already exists',
        'The unitname "'+AnUnitName+'" already exists in the package:'#13
        +PkgFile.LazPackage.IDAsString,
        mtWarning,[mbCancel,mbIgnore],0)<>mrIgnore then exit;
    end;
  end;
  
  // check if unitname is a componentclass
  if IDEComponentPalette.FindComponent(AnUnitName)<>nil then begin
    if MessageDlg('Ambigious Unit Name',
      'The unit name "'+AnUnitName+'" is the same as an registered component.'#13
      +'Using this can cause strange error messages.',
      mtWarning,[mbCancel,mbIgnore],0)<>mrIgnore
    then
      exit;
  end;
  
  // check if file already exists in package
  PkgFile:=LazPackage.FindPkgFile(AFilename,true);
  if PkgFile<>nil then begin
    Msg:='File "'+AFilename+'" already exists in the project.';
    if PkgFile.Filename<>AFilename then
      Msg:=#13+'Existing file: "'+PkgFile.Filename+'"';
    MessageDlg('File already exists',Msg,mtError,[mbCancel],0);
    exit;
  end;
  
  // check if file is part of project or marked readonly
  if Assigned(OnGetIDEFileInfo) then begin
    IDEFileFlags:=[];
    OnGetIDEFileInfo(Self,AFilename,[ifsPartOfProject,ifsReadOnly],
                     IDEFileFlags);
    if (ifsPartOfProject in IDEFileFlags) then begin
      MessageDlg('File is used',
        'The file "'+AFilename+'" is part of the current project.'#13
        +'It is a bad idea to share files between projects and packages.',
        mtError,[mbCancel],0);
      exit;
    end;
    if (ifsReadOnly in IDEFileFlags) then begin
      MessageDlg('File is readonly',
        'The file "'+AFilename+'" is marked as readonly.',
        mtError,[mbCancel],0);
      exit;
    end;
  end;

  // ok
  Result:=true;
end;

constructor TAddToPackageDlg.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fPkgComponents:=TAVLTree.Create(@CompareIDEComponentByClassName);
  fPackages:=TAVLTree.Create(@CompareLazPackageID);
  Position:=poScreenCenter;
  IDEDialogLayoutList.ApplyLayout(Self,500,300);
  SetupComponents;
  OnResize:=@AddToPackageDlgResize;
end;

destructor TAddToPackageDlg.Destroy;
begin
  FreeAndNil(fPkgComponents);
  FreeAndNil(fPackages);
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
begin
  fPackages.Clear;
  PackageGraph.IteratePackages(fpfSearchPackageEverywhere,@OnIteratePackages);
  sl:=TStringList.Create;
  ANode:=fPackages.FindLowest;
  while ANode<>nil do begin
    sl.Add(TLazPackageID(ANode.Data).Name);
    ANode:=fPackages.FindSuccessor(ANode);
  end;
  DependPkgNameComboBox.Items.Assign(sl);
  sl.Free;
end;

end.

