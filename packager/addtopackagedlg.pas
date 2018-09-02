{
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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  Math, Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  LCLProc, LCLType, Forms, Controls, Buttons, ExtDlgs, StdCtrls, ExtCtrls,
  Dialogs, ComCtrls, ButtonPanel,
  // LazUtils
  FileUtil, LazFileUtils,
  // IDEIntf
  NewItemIntf, PackageIntf, FormEditingIntf, IDEWindowIntf, ComponentReg,
  IDEDialogs, IDEImagesIntf,
  // IDE
  LazarusIDEStrConsts, InputHistory, IDEDefs, EnvironmentOpts,
  PackageSystem, PackageDefs, ProjPackChecks;
  
type

  { TAddToPkgResult }

  TAddToPkgResult = class
  public
    Pkg: TLazPackage;
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
    out TheUnitName: string; out HasRegisterProc: boolean) of object;

  { TAddToPackageDlg }

  TAddToPackageDlg = class(TForm)
    AncestorComboBox: TComboBox;
    AncestorShowAllCheckBox: TCheckBox;
    AncestorTypeLabel: TLabel;
    ButtonPanel1: TButtonPanel;
    ClassNameEdit: TEdit;
    ClassNameLabel: TLabel;
    ComponentIconBitBtn: TBitBtn;
    ComponentIconLabel: TLabel;
    ComponentUnitFileBrowseButton: TButton;
    ComponentUnitFileEdit: TEdit;
    ComponentUnitFileLabel: TLabel;
    ComponentUnitFileShortenButton: TButton;
    ComponentUnitNameEdit: TEdit;
    ComponentUnitNameLabel: TLabel;
    LabelIconInfo: TLabel;
    PalettePageCombobox: TComboBox;
    PalettePageLabel: TLabel;
    procedure AddToPackageDlgClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure AncestorComboBoxChange(Sender: TObject);
    procedure AncestorComboBoxCloseUp(Sender: TObject);
    procedure AncestorShowAllCheckBoxClick(Sender: TObject);
    procedure ClassNameEditChange(Sender: TObject);
    procedure ComponentIconBitBtnClick(Sender: TObject);
    procedure ComponentUnitFileBrowseButtonClick(Sender: TObject);
    procedure ComponentUnitFileShortenButtonClick(Sender: TObject);
    procedure ComponentUnitNameEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewComponentButtonClick(Sender: TObject);
  private
    fLastNewComponentAncestorType: string;
    fLastNewComponentClassName: string;
    FLazPackage: TLazPackage;
    FOnGetIDEFileInfo: TGetIDEFileStateEvent;
    FOnGetUnitRegisterInfo: TOnGetUnitRegisterInfo;
    fPkgComponents: TAVLTree;// tree of TPkgComponent
    fPackages: TAVLTree;// tree of  TLazPackage or TPackageLink
    FComponentIconFilename: string;
    fParams: TAddToPkgResult;
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure OnIterateComponentClasses(PkgComponent: TPkgComponent);
    function CheckNewCompOk: Boolean;
    procedure AutoCompleteNewComponent;
    procedure AutoCompleteNewComponentUnitName;
    function SwitchRelativeAbsoluteFilename(const Filename: string): string;
    procedure LoadComponentIcon(AFilename: string);
  public
    procedure UpdateAvailableAncestorTypes;
    procedure UpdateAvailablePageNames;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
    property OnGetIDEFileInfo: TGetIDEFileStateEvent read FOnGetIDEFileInfo
                                                     write FOnGetIDEFileInfo;
    property OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo
                       read FOnGetUnitRegisterInfo write FOnGetUnitRegisterInfo;
  end;
  
function ShowAddToPackageDlg(Pkg: TLazPackage; out Params: TAddToPkgResult;
  OnGetIDEFileInfo: TGetIDEFileStateEvent;
  OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo): TModalResult;


implementation

{$R *.lfm}

function ShowAddToPackageDlg(Pkg: TLazPackage; out Params: TAddToPkgResult;
  OnGetIDEFileInfo: TGetIDEFileStateEvent;
  OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo): TModalResult;
var
  AddDlg: TAddToPackageDlg;
begin
  Params:=nil;
  AddDlg:=TAddToPackageDlg.Create(nil);
  try
    AddDlg.OnGetIDEFileInfo:=OnGetIDEFileInfo;
    AddDlg.OnGetUnitRegisterInfo:=OnGetUnitRegisterInfo;
    AddDlg.LazPackage:=Pkg;
    Result:=AddDlg.ShowModal;
    if Result=mrOk then begin
      Params:=AddDlg.fParams;
      AddDlg.fParams:=nil;
    end;
  finally
    AddDlg.Free;
  end;
end;

{ TAddToPackageDlg }

procedure TAddToPackageDlg.FormCreate(Sender: TObject);
begin
  Caption:=lisMenuNewComponent;
  fPkgComponents:=TAVLTree.Create(@CompareIDEComponentByClassName);
  fPackages:=TAVLTree.Create(@CompareLazPackageID);
  fParams:=TAddToPkgResult.Create;
  IDEDialogLayoutList.ApplyLayout(Self,500,260);
  // Setup Components
  ButtonPanel1.OkButton.Caption:=lisA2PCreateNewComp;
  ButtonPanel1.OkButton.OnClick:=@NewComponentButtonClick;
  CheckNewCompOk;
  AncestorTypeLabel.Caption:=lisA2PAncestorType;
  AncestorComboBox.Text:='';
  AncestorShowAllCheckBox.Caption:=lisA2PShowAll;
  ClassNameLabel.Caption:=lisA2PNewClassName;
  ClassNameEdit.Text:='';
  PalettePageLabel.Caption:=lisA2PPalettePage;
  PalettePageCombobox.Text:='';
  ComponentUnitFileLabel.Caption:=lisA2PUnitFileName2;
  ComponentUnitFileEdit.Text:='';
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
  ComponentUnitNameLabel.Caption:=lisA2PUnitName;
  ComponentUnitNameEdit.Text:='';
  ComponentIconLabel.Caption:=lisA2PIconAndSize;
  ComponentIconBitBtn.Width:=ComponentPaletteBtnWidth;
  ComponentIconBitBtn.Height:=ComponentPaletteBtnHeight;
end;

procedure TAddToPackageDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fPkgComponents);
  FreeAndNil(fPackages);
  FreeAndNil(fParams);
end;

procedure TAddToPackageDlg.AddToPackageDlgClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  IDEDialogLayoutList.SaveLayout(Self);
end;

procedure TAddToPackageDlg.AncestorComboBoxChange(Sender: TObject);
begin
  CheckNewCompOk;
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

procedure TAddToPackageDlg.ClassNameEditChange(Sender: TObject);
begin
  AutoCompleteNewComponentUnitName;
  CheckNewCompOk;
end;

procedure TAddToPackageDlg.ComponentIconBitBtnClick(Sender: TObject);
var
  Dlg: TOpenPictureDialog;
begin
  Dlg:=TOpenPictureDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(Dlg);
    Dlg.InitialDir:=LazPackage.GetFileDialogInitialDir(ExtractFilePath(ComponentUnitFileEdit.Text));
    Dlg.Title:=lisTitleOpenComponentIcon24x24;
    Dlg.Options:=Dlg.Options+[ofPathMustExist];
    Dlg.Filter:=Format('%s|*.png|%s|*.bmp|%s|*.xpm|%s|%s',
      [dlgFilterImagesPng,
       dlgFilterImagesBitmap,
       dlgFilterImagesPixmap,
       dlgFilterAll, GetAllFilesMask]);

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
    SaveDialog.InitialDir := LazPackage.GetFileDialogInitialDir(SaveDialog.InitialDir);
    SaveDialog.Title := lisSaveAs;
    SaveDialog.Options := SaveDialog.Options+[ofPathMustExist];
    SaveDialog.Filter := Format('%s|*.pas;*.pp', [dlgFilterPascalFile]);
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
  ComponentUnitFileEdit.Text:=SwitchRelativeAbsoluteFilename(ComponentUnitFileEdit.Text);
end;

procedure TAddToPackageDlg.ComponentUnitNameEditChange(Sender: TObject);
begin
  CheckNewCompOk;
end;

procedure TAddToPackageDlg.NewComponentButtonClick(Sender: TObject);
var
  PkgFile: TPkgFile;
  PkgComponent: TPkgComponent;
  ARequiredPackage: TLazPackage;
begin
  fParams.Clear;
  fParams.FileType:=pftUnit;
  fParams.PkgFileFlags:=[pffHasRegisterProc,pffAddToPkgUsesSection];
  fParams.AncestorType:=AncestorComboBox.Text;
  fParams.NewClassName:=ClassNameEdit.Text;
  fParams.PageName:=PalettePageCombobox.Text;
  fParams.Unit_Name:=ComponentUnitNameEdit.Text;
  fParams.UnitFilename:=ComponentUnitFileEdit.Text;
  fParams.UsedUnitname:='';
  fParams.IconFile:=FComponentIconFilename;

  // check Ancestor Type
  if not IsValidIdent(fParams.AncestorType) then begin
    IDEMessageDialog(lisA2PInvalidAncestorType,
      Format(lisA2PTheAncestorTypeIsNotAValidPascalIdentifier, [fParams.AncestorType]),
      mtError,[mbCancel]);
    exit;
  end;

  // check pagename
  if length(fParams.PageName)>100 then begin
    IDEMessageDialog(lisA2PPageNameTooLong,
      Format(lisA2PThePageNameIsTooLongMax100Chars, [fParams.PageName]),
      mtError,[mbCancel]);
    exit;
  end;

  // check unitname - filename redundancy
  if CompareText(fParams.Unit_name,ExtractFileNameOnly(fParams.UnitFilename))<>0
  then begin
    IDEMessageDialog(lisA2PUnitNameInvalid,
      Format(lisA2PTheUnitNameDoesNotCorrespondToTheFilename, [fParams.Unit_Name]),
      mtError,[mbCancel]);
    exit;
  end;

  // check classname
  if not IsValidIdent(fParams.NewClassName) then begin
    IDEMessageDialog(lisA2PInvalidClassName,
      Format(lisA2PTheClassNameIsNotAValidPascalIdentifier, [fParams.NewClassName]),
      mtError,[mbCancel]);
    exit;
  end;

  // check classname<>ancestortype
  if CompareText(fParams.NewClassName,fParams.AncestorType)=0 then begin
    IDEMessageDialog(lisA2PInvalidCircularDependency,
      Format(lisA2PTheClassNameAndAncestorTypeAreTheSame,[fParams.NewClassName,fParams.AncestorType]),
      mtError,[mbCancel]);
    exit;
  end;

  // check ancestor type is not unitname
  PkgFile:=PackageGraph.FindUnit(LazPackage,fParams.AncestorType,true,true);
  if PkgFile<>nil then begin
    if IDEMessageDialog(lisA2PAmbiguousAncestorType,
      Format(lisA2PTheAncestorTypeHasTheSameNameAsTheUnit,
             [fParams.AncestorType, LineEnding, PkgFile.Filename]),
      mtError,[mbCancel,mbIgnore])<>mrIgnore
    then
      exit;
  end;

  // check classname does not interfere with an existing unitname
  PkgFile:=PackageGraph.FindUnit(LazPackage,fParams.NewClassName,true,true);
  if PkgFile<>nil then begin
    if IDEMessageDialog(lisA2PAmbiguousClassName,
      Format(lisA2PTheClassNameHasTheSameNameAsTheUnit,
             [fParams.AncestorType, LineEnding, PkgFile.Filename]),
      mtError,[mbCancel,mbIgnore])<>mrIgnore
    then
      exit;
  end;

  // check if classname already exists
  PkgComponent:=TPkgComponent(IDEComponentPalette.FindComponent(fParams.NewClassname));
  if PkgComponent<>nil then begin
    if IDEMessageDialog(lisA2PClassNameAlreadyExists,
      Format(lisA2PTheClassNameExistsAlreadyInPackageFile, [fParams.NewClassName, LineEnding,
        PkgComponent.PkgFile.LazPackage.IDAsString, LineEnding, PkgComponent.PkgFile.Filename]),
      mtError,[mbCancel,mbIgnore])<>mrIgnore
    then
      exit;
  end;

  // check filename
  if not CheckAddingPackageUnit(LazPackage, d2ptNewComponent,
    OnGetIDEFileInfo, fParams.UnitFilename) then exit;

  // create dependency if needed
  PkgComponent:=TPkgComponent(IDEComponentPalette.FindComponent(fParams.AncestorType));
  if PkgComponent<>nil then begin
    fParams.UsedUnitname:=PkgComponent.GetUnitName;
    ARequiredPackage:=PkgComponent.PkgFile.LazPackage;
    ARequiredPackage:=TLazPackage(PackageEditingInterface.RedirectPackageDependency(ARequiredPackage));
    if (LazPackage<>ARequiredPackage)
    and (not LazPackage.Requires(PkgComponent.PkgFile.LazPackage))
    then
      fParams.Dependency:=ARequiredPackage.CreateDependencyWithOwner(nil);
  end;
  ModalResult:=mrOk;
end;

procedure TAddToPackageDlg.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
  fParams.Pkg:=FLazPackage;
  UpdateAvailableAncestorTypes;
  UpdateAvailablePageNames;
end;

function TAddToPackageDlg.CheckNewCompOk: Boolean;
begin
  Result:=(AncestorComboBox.Text<>'') and (ClassNameEdit.Text<>'') and (ComponentUnitNameEdit.Text<>'');
  ButtonPanel1.OKButton.Enabled:=Result;
end;

procedure TAddToPackageDlg.OnIterateComponentClasses(PkgComponent: TPkgComponent);
begin
  if fPkgComponents.Find(PkgComponent)=nil then
    fPkgComponents.Add(PkgComponent);
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
  if not IsValidIdent(ClassNameEdit.Text) then
    ClassNameEdit.Text:=IDEComponentPalette.CreateNewClassName(
                                                 fLastNewComponentAncestorType);
  // choose the same page name
  if (PalettePageCombobox.Text='')
  and (PkgComponent<>nil) and (PkgComponent.RealPage<>nil) then
    PalettePageCombobox.Text:=PkgComponent.RealPage.PageName;
  // filename
  AutoCompleteNewComponentUnitName;
  ButtonPanel1.OkButton.Enabled:=True;
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

function TAddToPackageDlg.SwitchRelativeAbsoluteFilename(const Filename: string): string;
begin
  Result:=Filename;
  if (not LazPackage.HasDirectory)
  or (not FilenameIsAbsolute(LazPackage.Directory)) then exit;
  if FilenameIsAbsolute(Filename) then
    Result:=TrimFilename(CreateRelativePath(Filename,LazPackage.Directory))
  else
    Result:=TrimFilename(CreateAbsoluteSearchPath(Filename,LazPackage.Directory));
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
      ComponentIconBitBtn.Glyph.Assign(Image.Picture.Graphic);
      ShortFilename:=AFilename;
      LazPackage.ShortenFilename(ShortFilename,true);
      LabelIconInfo.Caption:= Format('%s (%dx%d)',
        [ShortFilename, ComponentIconBitBtn.Glyph.Width, ComponentIconBitBtn.Glyph.Height]);
      FComponentIconFilename:=AFilename;
    finally
      Image.Free;
    end;
  except
    on E: Exception do begin
      IDEMessageDialog(lisCCOErrorCaption,
        Format(lisErrorLoadingFile2,[AFilename]) + LineEnding + E.Message,
        mtError, [mbCancel]);
      ComponentIconBitBtn.Glyph.Clear;
      FComponentIconFilename:='';
      LabelIconInfo.Caption:=lisNoneClickToChooseOne;
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
  for i:=0 to IDEComponentPalette.Pages.Count-1 do begin
    APageName:=IDEComponentPalette.Pages[i].PageName;
    if APageName<>'' then
      sl.Add(APageName);
  end;
  sl.Sort;
  PalettePageCombobox.Items.Assign(sl);
  sl.Free;
end;

{ TAddToPkgResult }

procedure TAddToPkgResult.Clear;
begin
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

