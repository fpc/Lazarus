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
  IDEProcs, EnvironmentOpts, PackageSystem, PackageDefs, ComponentReg;
  
type
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
    ComponentUnitLabel: TLabel;
    ComponentUnitEdit: TEdit;
    ComponentUnitButton: TButton;
    NewComponentButton: TButton;
    CancelNewComponentButton: TButton;
    // new require/conflict
    DependPkgNameLabel: TLabel;
    DependPkgNameComboBox: TComboBox;
    DependTypeRadioGroup: TRadioGroup;
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
    procedure ComponentUnitButtonClick(Sender: TObject);
    procedure NewComponentButtonClick(Sender: TObject);
    procedure NewComponentPageResize(Sender: TObject);
    procedure NewDependPageResize(Sender: TObject);
  private
    fLastNewComponentAncestorType: string;
    fLastNewComponentClassName: string;
    FLazPackage: TLazPackage;
    fPkgComponents: TAVLTree;// tree of TPkgComponent
    fPackages: TAVLTree;// tree of  TLazPackage or TPackageLink
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure SetupComponents;
    procedure OnIterateComponentClasses(PkgComponent: TPkgComponent);
    procedure OnIteratePackages(APackageID: TLazPackageID);
    procedure AutoCompleteNewComponent;
    procedure AutoCompleteNewComponentUnitName;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateAvailableAncestorTypes;
    procedure UpdateAvailablePageNames;
    procedure UpdateAvailableDependencyNames;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
  end;
  
function ShowAddToPackageDlg(Pkg: TLazPackage): TModalResult;


implementation


function ShowAddToPackageDlg(Pkg: TLazPackage): TModalResult;
var
  AddDlg: TAddToPackageDlg;
begin
  AddDlg:=TAddToPackageDlg.Create(Application);
  AddDlg.LazPackage:=Pkg;
  Result:=AddDlg.ShowModal;
  IDEDialogLayoutList.SaveLayout(AddDlg);
  AddDlg.Free;
end;

{ TAddToPackageDlg }

procedure TAddToPackageDlg.AddToPackageDlgResize(Sender: TObject);
begin

end;

procedure TAddToPackageDlg.AddUnitButtonClick(Sender: TObject);
var
  AFilename: String;
  AnUnitName: String;
  PkgFile: TPkgFile;
  Msg: String;
begin
  // normalize filename
  AFilename:=CleanAndExpandFilename(AddUnitFilenameEdit.Text);
  // check if file exists
  if not FileExists(AFilename) then begin
    MessageDlg('File not found',
      'File "'+AFilename+'" not found.',mtError,[mbCancel],0);
    exit;
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
  // check if file already exists in package
  PkgFile:=LazPackage.FindPkgFile(AFilename,true);
  if PkgFile<>nil then begin
    Msg:='File "'+AFilename+'" already exists in the project.';
    if PkgFile.Filename<>AFilename then
      Msg:=#13+'Existing file: "'+PkgFile.Filename+'"';
    MessageDlg('File already exists',Msg,mtError,[mbCancel],0);
    exit;
  end;
  
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

procedure TAddToPackageDlg.ComponentUnitButtonClick(Sender: TObject);
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
        ComponentUnitEdit.Text:=AFilename;
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
begin
  // ToDo
  ShowMessage('Not implemented yet');
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

  with ComponentUnitLabel do
    SetBounds(x,y+2,100,Height);
  inc(x,ComponentUnitLabel.Width+5);

  with ComponentUnitEdit do
    SetBounds(x,y,Parent.ClientWidth-x-Height-5,Height);
  inc(x,ComponentUnitEdit.Width+2);

  with ComponentUnitButton do
    SetBounds(x,y,ComponentUnitEdit.Height,ComponentUnitEdit.Height);
  x:=5;
  inc(y,ComponentUnitEdit.Height+15);

  with NewComponentButton do
    SetBounds(x,y,80,Height);
  inc(x,NewComponentButton.Width+10);

  with CancelNewComponentButton do
    SetBounds(x,y,80,Height);
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

  with DependTypeRadioGroup do
    SetBounds(x,y,200,50);
  inc(y,DependTypeRadioGroup.Height+10);

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
    Pages.Add('New Require, Conflict');
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

  ComponentUnitLabel:=TLabel.Create(Self);
  with ComponentUnitLabel do begin
    Name:='ComponentUnitLabel';
    Parent:=NewComponentPage;
    Caption:='Unit File Name:';
  end;

  ComponentUnitEdit:=TEdit.Create(Self);
  with ComponentUnitEdit do begin
    Name:='ComponentUnitEdit';
    Parent:=NewComponentPage;
    Text:='';
  end;

  ComponentUnitButton:=TButton.Create(Self);
  with ComponentUnitButton do begin
    Name:='ComponentUnitButton';
    Parent:=NewComponentPage;
    Caption:='...';
    OnClick:=@ComponentUnitButtonClick;
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


  // add require, conflict
  
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
  
  DependTypeRadioGroup:=TRadioGroup.Create(Self);
  with DependTypeRadioGroup do begin
    Name:='DependTypeRadioGroup';
    Parent:=NewDependPage;
    Items.Add('Require');
    Items.Add('Conflict');
    ItemIndex:=0;
    Columns:=2;
    Caption:='Type';
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
  CurClassName:=ClassNameEdit.Text;
  if fLastNewComponentClassName=CurClassName then exit;
  fLastNewComponentClassName:=CurClassName;
  if not IsValidIdent(CurClassName) then exit;
  // create unitname
  NewUnitName:=CurClassName;
  if NewUnitName[1]='T' then
    NewUnitName:=copy(NewUnitName,2,length(NewUnitName)-1);
  NewUnitName:=PackageGraph.CreateUniqueUnitName(NewUnitName);
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
  ComponentUnitEdit.Text:=NewFileName;
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
    sl.Add(TLazPackageID(ANode.Data).IDAsString);
    ANode:=fPackages.FindSuccessor(ANode);
  end;
  DependPkgNameComboBox.Items.Assign(sl);
  sl.Free;
end;

end.

