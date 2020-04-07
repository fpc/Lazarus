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
  Classes, SysUtils, Laz_AVL_Tree,
  // LCL
  LCLProc, LCLType, Forms, Controls, Buttons, ExtDlgs, StdCtrls, ExtCtrls,
  Dialogs, ComCtrls, ButtonPanel,
  // LazUtils
  FileUtil, LazFileUtils,
  // IDEIntf
  NewItemIntf, PackageIntf, FormEditingIntf, IDEWindowIntf, ComponentReg,
  IDEDialogs,
  // IDE
  LazarusIDEStrConsts, InputHistory, EnvironmentOpts,
  PackageSystem, PackageDefs, ProjPackChecks;
  
type

  { TAddToPkgResult }

  TAddToPkgResult = class
  public
    Pkg: TLazPackage;
    UnitFilename: string;
    Unit_Name: string;
    AncestorType: string;
    NewClassName: string;
    PageName: string;
    FileType: TPkgFileType;
    PkgFileFlags: TPkgFileFlags;
    UsedUnitname: string;
    IconNormFile: string;
    Icon150File: string;
    Icon200File: string;
    AutoAddLFMFile: boolean;
    AutoAddLRSFile: boolean;
    NewItem: TNewIDEItemTemplate;
    Next: TAddToPkgResult;
    procedure Clear;
    destructor Destroy; override;
  end;
  
  TOnGetUnitRegisterInfo = procedure(Sender: TObject; const AFilename: string;
    out TheUnitName: string; out HasRegisterProc: boolean) of object;

  { TIconGuiStuff }

  TIconGuiStuff = class
  // Join icon related GUI controls together. Streamlines the code.
  private
    Btn: TBitBtn;
    InfoLabel: TLabel;
    Title: string;
    FileName: string;
  public
    constructor Create(aBtn: TBitBtn; aInfoLabel: TLabel; aTitle: string);
    procedure LoadIcon(aLazPackage: TLazPackage; aFileName: string);
  end;

  { TAddToPackageDlg }

  TAddToPackageDlg = class(TForm)
    AncestorComboBox: TComboBox;
    AncestorShowAllCheckBox: TCheckBox;
    AncestorTypeLabel: TLabel;
    ButtonPanel1: TButtonPanel;
    ClassNameEdit: TEdit;
    ClassNameLabel: TLabel;
    Icon200Label: TLabel;
    IconNormBitBtn: TBitBtn;
    Icon150BitBtn: TBitBtn;
    Icon150InfoLabel: TLabel;
    Icon200InfoLabel: TLabel;
    IconNormLabel: TLabel;
    UnitFilenameExistsLabel: TLabel;
    UnitDirectoryBrowseButton: TButton;
    UnitDirectoryEdit: TEdit;
    UnitDirectoryLabel: TLabel;
    UnitDirectoryShortenButton: TButton;
    UnitNameEdit: TEdit;
    UnitNameLabel: TLabel;
    Icon200BitBtn: TBitBtn;
    Icon150Label: TLabel;
    IconNormInfoLabel: TLabel;
    UnitFilenameLabel: TLabel;
    PalettePageCombobox: TComboBox;
    PalettePageLabel: TLabel;
    procedure AddToPackageDlgClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure AncestorComboBoxChange(Sender: TObject);
    procedure AncestorComboBoxCloseUp(Sender: TObject);
    procedure AncestorShowAllCheckBoxClick(Sender: TObject);
    procedure ClassNameEditChange(Sender: TObject);
    procedure IconBitBtnClick(Sender: TObject);
    procedure UnitDirectoryBrowseButtonClick(Sender: TObject);
    procedure UnitDirectoryShortenButtonClick(Sender: TObject);
    procedure UnitDirectoryEditChange(Sender: TObject);
    procedure UnitNameEditChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewComponentButtonClick(Sender: TObject);
  private
    fLastNewAncestorType: string;
    fLastNewClassName: string;
    FLazPackage: TLazPackage;
    fPkgComponents: TAVLTree;// tree of TPkgComponent
    fPackages: TAVLTree;// tree of  TLazPackage or TPackageLink
    fParams: TAddToPkgResult;
    fIconDlg: TOpenPictureDialog;
    fIconNormGUI: TIconGuiStuff;
    fIcon150GUI: TIconGuiStuff;
    fIcon200GUI: TIconGuiStuff;
    function GenerateUnitFileName: string;
    function RelatedIconFile(aSuffix: string): string;
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure OnIterateComponentClasses(PkgComponent: TPkgComponent);
    function CheckNewCompOk: Boolean;
    procedure AutoCompleteNewComponent;
    procedure AutoCompleteUnitName;
    procedure UpdateUnitFilename;
  public
    procedure UpdateAvailableAncestorTypes;
    procedure UpdateAvailablePageNames;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
  end;
  
function ShowAddToPackageDlg(Pkg: TLazPackage; out Params: TAddToPkgResult): TModalResult;


implementation

{$R *.lfm}

function ShowAddToPackageDlg(Pkg: TLazPackage; out Params: TAddToPkgResult): TModalResult;
var
  AddDlg: TAddToPackageDlg;
begin
  Params:=nil;
  AddDlg:=TAddToPackageDlg.Create(nil);
  try
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

{ TIconGuiStuff }

constructor TIconGuiStuff.Create(aBtn: TBitBtn; aInfoLabel: TLabel; aTitle: string);
begin
  Btn:=aBtn;
  InfoLabel:=aInfoLabel;
  Title:=aTitle;
  InfoLabel.Caption:='';
end;

procedure TIconGuiStuff.LoadIcon(aLazPackage: TLazPackage; aFileName: string);
var
  ShortFN: String;
  Image: TImage;
  W, H: Integer;
begin
  Filename:=aFileName;
  try
    Image:=TImage.Create(nil);
    try
      Image.Picture.LoadFromFile(Filename);
      W:=Image.Picture.Graphic.Width+6;
      H:=Image.Picture.Graphic.Height+6;
      if W > 32 then
        Btn.Width:=W;
      if H > 32 then
        Btn.Height:=H;
      Btn.Glyph.Assign(Image.Picture.Graphic);
      ShortFN:=Filename;
      aLazPackage.ShortenFilename(ShortFN,true);
      InfoLabel.Caption:=Format('%s (%dx%d)',[ShortFN, Btn.Glyph.Width, Btn.Glyph.Height]);
    finally
      Image.Free;
    end;
  except
    on E: Exception do begin
      IDEMessageDialog(lisCCOErrorCaption,
        Format(lisErrorLoadingFile2,[FileName]) + LineEnding + E.Message,
        mtError, [mbCancel]);
      Btn.Glyph.Clear;
      InfoLabel.Caption:=lisNoneClickToChooseOne;
      FileName:='';
    end;
  end;
end;

{ TAddToPackageDlg }

procedure TAddToPackageDlg.FormCreate(Sender: TObject);
begin
  Caption:=lisMenuNewComponent;
  fPkgComponents:=TAVLTree.Create(@CompareIDEComponentByClassName);
  fPackages:=TAVLTree.Create(@CompareLazPackageID);
  fParams:=TAddToPkgResult.Create;
  IDEDialogLayoutList.ApplyLayout(Self,700,400);
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
  UnitDirectoryLabel.Caption:=lisA2PDirectoryForUnitFile;
  UnitDirectoryEdit.Text:='';
  UnitFilenameLabel.Caption:='';
  UnitFilenameExistsLabel.Caption:='';
  with UnitDirectoryBrowseButton do begin
    Caption:='...';
    ShowHint:=true;
    Hint:=lisChooseDirectory;
  end;
  with UnitDirectoryShortenButton do begin
    Caption:='<>';
    ShowHint:=true;
    Hint:=lisA2PShortenOrExpandFilename;
  end;
  UnitNameLabel.Caption:=lisA2PUnitName;
  UnitNameEdit.Text:='';
  IconNormLabel.Caption:=lisA2PIcon24x24;
  Icon150Label.Caption:=lisA2PIcon36x36;
  Icon200Label.Caption:=lisA2PIcon48x48;
  fIconDlg:=TOpenPictureDialog.Create(nil);
  // Helper objects to join icon related GUI controls together
  fIconNormGUI:=TIconGuiStuff.Create(IconNormBitBtn, IconNormInfoLabel, lisA2PIcon24x24);
  fIcon150GUI:=TIconGuiStuff.Create(Icon150BitBtn, Icon150InfoLabel, lisA2PIcon36x36);
  fIcon200GUI:=TIconGuiStuff.Create(Icon200BitBtn, Icon200InfoLabel, lisA2PIcon48x48);
  AncestorComboBox.DropDownCount:=EnvironmentOptions.DropDownCount;
  PalettePageCombobox.DropDownCount:=EnvironmentOptions.DropDownCount;
end;

procedure TAddToPackageDlg.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fIcon200GUI);
  FreeAndNil(fIcon150GUI);
  FreeAndNil(fIconNormGUI);
  FreeAndNil(fIconDlg);
  FreeAndNil(fParams);
  FreeAndNil(fPackages);
  FreeAndNil(fPkgComponents);
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
  if fLastNewAncestorType<>AncestorComboBox.Text then
    AutoCompleteNewComponent;
end;

procedure TAddToPackageDlg.AncestorShowAllCheckBoxClick(Sender: TObject);
begin
  UpdateAvailableAncestorTypes;
end;

procedure TAddToPackageDlg.ClassNameEditChange(Sender: TObject);
begin
  AutoCompleteUnitName;
  CheckNewCompOk;
end;

function TAddToPackageDlg.RelatedIconFile(aSuffix: string): string;
var
  Ext: String;
begin
  Ext := ExtractFileExt(fIconDlg.FileName);
  Result := ExtractFileNameWithoutExt(fIconDlg.FileName)+ASuffix+Ext;
end;

procedure TAddToPackageDlg.IconBitBtnClick(Sender: TObject);
var
  Btn: TBitBtn;
  IconGUI: TIconGuiStuff;
  OtherIconFile: string;
begin
  Btn:=Sender as TBitBtn;
  if Btn = IconNormBitBtn then
    IconGUI:=fIconNormGUI
  else if Btn = Icon150BitBtn then
    IconGUI:=fIcon150GUI
  else if Btn = Icon200BitBtn then
    IconGUI:=fIcon200GUI;
  if fIconDlg.InitialDir='' then
    fIconDlg.InitialDir:=LazPackage.Directory;
  fIconDlg.Title:=IconGUI.Title;
  fIconDlg.Options:=fIconDlg.Options+[ofPathMustExist];
  fIconDlg.Filter:=Format('%s|*.png|%s|*.bmp|%s|*.xpm|%s|%s',
    [dlgFilterImagesPng,
     dlgFilterImagesBitmap,
     dlgFilterImagesPixmap,
     dlgFilterAll, GetAllFilesMask]);

  if fIconDlg.Execute then begin
    IconGUI.LoadIcon(LazPackage, fIconDlg.FileName);
    // Load high resolution icons automatically if found.
    if Btn = IconNormBitBtn then begin
      // 150%
      OtherIconFile:=RelatedIconFile('_150');
      if FileExists(OtherIconFile) then
        fIcon150GUI.LoadIcon(LazPackage, OtherIconFile);
      // 200%
      OtherIconFile:=RelatedIconFile('_200');
      if FileExists(OtherIconFile) then
        fIcon200GUI.LoadIcon(LazPackage, OtherIconFile);
    end;
  end;
end;

procedure TAddToPackageDlg.UnitDirectoryBrowseButtonClick(Sender: TObject);
var
  DirDialog: TSelectDirectoryDialog;
begin
  DirDialog:=TSelectDirectoryDialog.Create(nil);
  try
    InputHistories.ApplyFileDialogSettings(DirDialog);
    DirDialog.InitialDir:=LazPackage.Directory;
    DirDialog.Title:=lisA2PDirectoryForUnitFile;
    //DirDialog.Options:=DirDialog.Options+[ofPathMustExist];
    //DirDialog.Filter:=Format('%s|*.pas;*.pp', [dlgFilterPascalFile]);
    if DirDialog.Execute then begin
      UnitDirectoryEdit.Text:=DirDialog.Filename;
      UpdateUnitFilename;
    end;
    InputHistories.StoreFileDialogSettings(DirDialog);
  finally
    DirDialog.Free;
  end;
end;

procedure TAddToPackageDlg.UnitDirectoryShortenButtonClick(Sender: TObject);
var
  S: string;
begin
  Assert(LazPackage.HasDirectory and FilenameIsAbsolute(LazPackage.Directory),
         'Unexpected package directory');
  S:=UnitDirectoryEdit.Text;
  if (S='') then
    S:='.';
  // Toggle between absolute and relative paths.
  if FilenameIsAbsolute(S) then
    UnitDirectoryEdit.Text:=CreateRelativePath(S,LazPackage.Directory,True)
  else
    UnitDirectoryEdit.Text:=CreateAbsolutePath(S,LazPackage.Directory);
  UpdateUnitFilename;
end;

procedure TAddToPackageDlg.UnitDirectoryEditChange(Sender: TObject);
begin
  UpdateUnitFilename;
  if UnitDirectoryEdit.Text<>'' then
    fIconDlg.InitialDir:=UnitDirectoryEdit.Text;
end;

procedure TAddToPackageDlg.UnitNameEditChange(Sender: TObject);
begin
  CheckNewCompOk;
  UpdateUnitFilename;
end;

procedure TAddToPackageDlg.NewComponentButtonClick(Sender: TObject);
var
  PkgFile: TPkgFile;
  PkgComponent: TPkgComponent;
  ARequiredPackage: TLazPackage;
  NewDependency: TPkgDependency;
  ThePath: String;
begin
  fParams.Clear;
  fParams.FileType:=pftUnit;
  fParams.PkgFileFlags:=[pffHasRegisterProc,pffAddToPkgUsesSection];
  fParams.AncestorType:=AncestorComboBox.Text;
  fParams.NewClassName:=ClassNameEdit.Text;
  fParams.PageName:=PalettePageCombobox.Text;
  fParams.Unit_Name:=UnitNameEdit.Text;
  fParams.UsedUnitname:='';
  fParams.IconNormFile:=fIconNormGUI.Filename;
  fParams.Icon150File:=fIcon150GUI.Filename;
  fParams.Icon200File:=fIcon200GUI.Filename;

  // prepend path to unit filename
  ThePath:=UnitDirectoryEdit.Text;
  if ThePath='' then
    ThePath:='.';
  ThePath:=CreateAbsolutePath(ThePath,LazPackage.Directory);
  if not DirectoryExists(ThePath) then
    if not ForceDirectories(ThePath) then
      raise Exception.Create('NewComponentButtonClick: Cannot create directory '+ThePath);
  fParams.UnitFilename:=AppendPathDelim(ThePath)+GenerateUnitFileName;

  // check if package is readonly
  if LazPackage.ReadOnly then begin
    IDEMessageDialog(lisAF2PPackageIsReadOnly,
      Format(lisAF2PThePackageIsReadOnly, [LazPackage.IDAsString]),
      mtError,[mbCancel]);
    exit;
  end;
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
  // check if unitname is a componentclass
  if IDEComponentPalette.FindComponent(fParams.Unit_Name)<>nil then begin
    if IDEMessageDialog(lisA2PAmbiguousUnitName,
      Format(lisA2PTheUnitNameIsTheSameAsAnRegisteredComponent,[fParams.Unit_Name,LineEnding]),
      mtWarning,[mbCancel,mbIgnore])<>mrIgnore
    then
      exit;
  end;

  // create dependency if needed
  PkgComponent:=TPkgComponent(IDEComponentPalette.FindComponent(fParams.AncestorType));
  if PkgComponent<>nil then begin
    fParams.UsedUnitname:=PkgComponent.GetUnitName;
    ARequiredPackage:=PkgComponent.PkgFile.LazPackage;
    ARequiredPackage:=TLazPackage(PackageEditingInterface.RedirectPackageDependency(ARequiredPackage));
    NewDependency:=TPkgDependency.Create;
    try
      NewDependency.DependencyType:=pdtLazarus;
      NewDependency.PackageName:=ARequiredPackage.Name;
      if CheckAddingPackageDependency(LazPackage,NewDependency,false,false)=mrOK then
        PackageGraph.AddDependencyToPackage(LazPackage, NewDependency);
    finally
      NewDependency.Free;
    end;
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
  Result:=(AncestorComboBox.Text<>'') and (ClassNameEdit.Text<>'') and (UnitNameEdit.Text<>'');
  ButtonPanel1.OKButton.Enabled:=Result;
end;

procedure TAddToPackageDlg.OnIterateComponentClasses(PkgComponent: TPkgComponent);
begin
  if fPkgComponents.Find(PkgComponent)=nil then
    fPkgComponents.Add(PkgComponent);
end;

function TAddToPackageDlg.GenerateUnitFileName: string;
begin
  Result:=UnitNameEdit.Text;
  if Result='' then Exit;
  if EnvironmentOptions.CharcaseFileAction in [ccfaAsk, ccfaAutoRename] then
    Result:=LowerCase(Result);
  // append pascal file extension
  Result:=Result+PascalExtension[EnvironmentOptions.PascalFileExtension];
end;

procedure TAddToPackageDlg.AutoCompleteNewComponent;
var
  PkgComponent: TPkgComponent;
begin
  fLastNewAncestorType:=AncestorComboBox.Text;
  if not IsValidIdent(fLastNewAncestorType) then exit;
  PkgComponent:=TPkgComponent(IDEComponentPalette.FindComponent(fLastNewAncestorType));
  // create unique classname
  ClassNameEdit.Text:=IDEComponentPalette.CreateNewClassName(fLastNewAncestorType);
  // choose the same page name
  if (PkgComponent<>nil) and (PkgComponent.RealPage<>nil) then
    PalettePageCombobox.Text:=PkgComponent.RealPage.PageName;
  // filename
  AutoCompleteUnitName;
  ButtonPanel1.OkButton.Enabled:=True;
end;

procedure TAddToPackageDlg.AutoCompleteUnitName;
var
  CurClassName: String;
  NewUnitName: String;
begin
  // check if update needed
  CurClassName:=ClassNameEdit.Text;
  if fLastNewClassName=CurClassName then exit;
  fLastNewClassName:=CurClassName;
  // check classname
  if not IsValidIdent(CurClassName) then exit;
  // create unitname
  NewUnitName:=CurClassName;
  if NewUnitName[1]='T' then
    NewUnitName:=copy(NewUnitName,2,length(NewUnitName)-1);
  NewUnitName:=PackageGraph.CreateUniqueUnitName(NewUnitName);
  UnitNameEdit.Text:=NewUnitName;
  // default directory
  UnitDirectoryEdit.Text:=LazPackage.Directory;
end;

procedure TAddToPackageDlg.UpdateUnitFilename;
begin
  UnitFilenameLabel.Caption:=AppendPathDelim(UnitDirectoryEdit.Text)+GenerateUnitFileName;
  if FileExists(UnitFilenameLabel.Caption) then
    UnitFilenameExistsLabel.Caption:=lisA2PFileAlreadyExists
  else
    UnitFilenameExistsLabel.Caption:='';
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

