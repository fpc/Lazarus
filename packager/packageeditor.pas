{  $Id$  }
{
 /***************************************************************************
                            packageeditor.pas
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
    TPackageEditorForm is the form of a package editor.
}
unit PackageEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ExtCtrls, ComCtrls, Buttons,
  LResources, Graphics, LCLType, Menus, Dialogs, Laz_XMLCfg, AVL_Tree, IDEProcs,
  LazConf, LazarusIDEStrConsts, IDEOptionDefs, IDEDefs, CompilerOptions,
  ComponentReg, PackageDefs, PkgOptionsDlg, AddToPackageDlg, PackageSystem;
  
type
  TOnOpenFile =
    function(Sender: TObject; const Filename: string): TModalResult of object;
  TOnOpenPackage =
    function(Sender: TObject; APackage: TLazPackage): TModalResult of object;
  TOnSavePackage =
    function(Sender: TObject; APackage: TLazPackage;
             SaveAs: boolean): TModalResult of object;
  TOnCompilePackage =
    function(Sender: TObject; APackage: TLazPackage;
             CompileAll: boolean): TModalResult of object;
  TOnCreateNewPkgFile =
    function(Sender: TObject;
             const Params: TAddToPkgResult): TModalResult  of object;
  TOnFreePkgEditor = procedure(APackage: TLazPackage) of object;


  { TPackageEditorLayout }

  TPackageEditorLayout = class
  public
    Filename: string;
    Rectangle: TRect;
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromXMLConfig(XMLConfig: TXMLConfig; const Path: string);
    procedure SaveToXMLConfig(XMLConfig: TXMLConfig; const Path: string);
  end;
  

  { TPackageEditorForm }

  TPackageEditorForm = class(TBasePackageEditor)
    // buttons
    SaveBitBtn: TBitBtn;
    CompileBitBtn: TBitBtn;
    AddBitBtn: TBitBtn;
    RemoveBitBtn: TBitBtn;
    InstallBitBtn: TBitBtn;
    OptionsBitBtn: TBitBtn;
    CompilerOptionsBitBtn: TBitBtn;
    // items
    FilesTreeView: TTreeView;
    // properties
    FilePropsGroupBox: TGroupBox;
    // file properties
    CallRegisterProcCheckBox: TCheckBox;
    RegisteredPluginsGroupBox: TGroupBox;
    RegisteredListBox: TListBox;
    // dependency properties
    UseMinVersionCheckBox: TCheckBox;
    MinVersionEdit: TEdit;
    UseMaxVersionCheckBox: TCheckBox;
    MaxVersionEdit: TEdit;
    ApplyDependencyButton: TButton;
    // statusbar
    StatusBar: TStatusBar;
    // hidden components
    ImageList: TImageList;
    FilesPopupMenu: TPopupMenu;
    procedure AddBitBtnClick(Sender: TObject);
    procedure ApplyDependencyButtonClick(Sender: TObject);
    procedure CallRegisterProcCheckBoxClick(Sender: TObject);
    procedure ChangeFileTypeMenuItemClick(Sender: TObject);
    procedure CompileAllClick(Sender: TObject);
    procedure CompileBitBtnClick(Sender: TObject);
    procedure CompilerOptionsBitBtnClick(Sender: TObject);
    procedure FilePropsGroupBoxResize(Sender: TObject);
    procedure FilesPopupMenuPopup(Sender: TObject);
    procedure FilesTreeViewDblClick(Sender: TObject);
    procedure FilesTreeViewSelectionChanged(Sender: TObject);
    procedure InstallBitBtnClick(Sender: TObject);
    procedure MaxVersionEditChange(Sender: TObject);
    procedure MinVersionEditChange(Sender: TObject);
    procedure MoveDependencyUpClick(Sender: TObject);
    procedure MoveDependencyDownClick(Sender: TObject);
    procedure OpenFileMenuItemClick(Sender: TObject);
    procedure OptionsBitBtnClick(Sender: TObject);
    procedure PackageEditorFormClose(Sender: TObject; var Action: TCloseAction);
    procedure PackageEditorFormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure PackageEditorFormResize(Sender: TObject);
    procedure ReAddMenuItemClick(Sender: TObject);
    procedure RegisteredListBoxDrawItem(Control: TWinControl; Index: Integer;
                                        ARect: TRect; State: TOwnerDrawState);
    procedure RemoveBitBtnClick(Sender: TObject);
    procedure SaveBitBtnClick(Sender: TObject);
    procedure SaveAsClick(Sender: TObject);
    procedure UseMaxVersionCheckBoxClick(Sender: TObject);
    procedure UseMinVersionCheckBoxClick(Sender: TObject);
  private
    FLazPackage: TLazPackage;
    FilesNode: TTreeNode;
    RequiredPackagesNode: TTreeNode;
    RemovedFilesNode: TTreeNode;
    RemovedRequiredNode: TTreeNode;
    FPlugins: TStringList;
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure SetupComponents;
    procedure UpdateAll; override;
    procedure UpdateTitle;
    procedure UpdateButtons;
    procedure UpdateFiles;
    procedure UpdateRequiredPkgs;
    procedure UpdateSelectedFile;
    procedure UpdateApplyDependencyButton;
    procedure UpdateStatusBar;
    function GetCurrentDependency(var Removed: boolean): TPkgDependency;
    function GetCurrentFile(var Removed: boolean): TPkgFile;
    function StoreCurrentTreeSelection: TStringList;
    procedure ApplyTreeSelection(ASelection: TStringList; FreeList: boolean);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoSave(SaveAs: boolean);
    procedure DoCompile(CompileAll: boolean);
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
  end;
  
  
  { TPackageEditors }
  
  TPackageEditors = class
  private
    FItems: TList; // list of TPackageEditorForm
    fLayouts: TAVLTree;// tree of TPackageEditorLayout sorted for filename
    FOnCompilePackage: TOnCompilePackage;
    FOnCreateNewFile: TOnCreateNewPkgFile;
    FOnFreeEditor: TOnFreePkgEditor;
    FOnGetIDEFileInfo: TGetIDEFileStateEvent;
    FOnGetUnitRegisterInfo: TOnGetUnitRegisterInfo;
    FOnOpenFile: TOnOpenFile;
    FOnOpenPackage: TOnOpenPackage;
    FOnSavePackage: TOnSavePackage;
    function GetEditors(Index: integer): TPackageEditorForm;
    procedure ApplyLayout(AnEditor: TPackageEditorForm);
    procedure SaveLayout(AnEditor: TPackageEditorForm);
    procedure LoadLayouts;
    function GetLayoutConfigFilename: string;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    procedure Clear;
    procedure SaveLayouts;
    procedure Remove(Editor: TPackageEditorForm);
    function IndexOfPackage(Pkg: TLazPackage): integer;
    function FindEditor(Pkg: TLazPackage): TPackageEditorForm;
    function OpenEditor(Pkg: TLazPackage): TPackageEditorForm;
    function OpenFile(Sender: TObject; const Filename: string): TModalResult;
    function OpenDependency(Sender: TObject;
                            Dependency: TPkgDependency): TModalResult;
    procedure DoFreeEditor(Pkg: TLazPackage);
    function CreateNewFile(Sender: TObject;
                           const Params: TAddToPkgResult): TModalResult;
    function SavePackage(APackage: TLazPackage; SaveAs: boolean): TModalResult;
    function CompilePackage(APackage: TLazPackage;
                            CompileAll: boolean): TModalResult;
    procedure UpdateAllEditors;
  public
    property Editors[Index: integer]: TPackageEditorForm read GetEditors;
    property OnCreateNewFile: TOnCreateNewPkgFile read FOnCreateNewFile
                                                  write FOnCreateNewFile;
    property OnOpenFile: TOnOpenFile read FOnOpenFile write FOnOpenFile;
    property OnOpenPackage: TOnOpenPackage read FOnOpenPackage write FOnOpenPackage;
    property OnGetIDEFileInfo: TGetIDEFileStateEvent read FOnGetIDEFileInfo
                                                     write FOnGetIDEFileInfo;
    property OnGetUnitRegisterInfo: TOnGetUnitRegisterInfo
                       read FOnGetUnitRegisterInfo write FOnGetUnitRegisterInfo;
    property OnFreeEditor: TOnFreePkgEditor read FOnFreeEditor write FOnFreeEditor;
    property OnSavePackage: TOnSavePackage read FOnSavePackage write FOnSavePackage;
    property OnCompilePackage: TOnCompilePackage read FOnCompilePackage
                                                 write FOnCompilePackage;
  end;
  
var
  PackageEditors: TPackageEditors;


implementation

uses Math;

var
  ImageIndexFiles: integer;
  ImageIndexRemovedFiles: integer;
  ImageIndexRequired: integer;
  ImageIndexRemovedRequired: integer;
  ImageIndexUnit: integer;
  ImageIndexRegisterUnit: integer;
  ImageIndexLFM: integer;
  ImageIndexLRS: integer;
  ImageIndexInclude: integer;
  ImageIndexText: integer;
  ImageIndexBinary: integer;
  ImageIndexConflict: integer;

function CompareLayouts(Data1, Data2: Pointer): integer;
var
  Layout1: TPackageEditorLayout;
  Layout2: TPackageEditorLayout;
begin
  Layout1:=TPackageEditorLayout(Data1);
  Layout2:=TPackageEditorLayout(Data2);
  Result:=CompareFilenames(Layout1.Filename,Layout2.Filename);
end;

function CompareFilenameWithLayout(Key, Data: Pointer): integer;
var
  Filename: String;
  Layout: TPackageEditorLayout;
begin
  Filename:=String(Key);
  Layout:=TPackageEditorLayout(Data);
  Result:=CompareFilenames(Filename,Layout.Filename);
end;

{ TPackageEditorForm }

procedure TPackageEditorForm.PackageEditorFormResize(Sender: TObject);
var
  x: Integer;
  y: Integer;
  w: Integer;
  h: Integer;
begin
  x:=0;
  y:=0;
  w:=ClientWidth div 7;
  h:=25;
  SaveBitBtn.SetBounds(x,y,w-2,h);
  inc(x,w);

  CompileBitBtn.SetBounds(x,y,w-2,h);
  inc(x,w);

  AddBitBtn.SetBounds(x,y,w-2,h);
  inc(x,w);

  RemoveBitBtn.SetBounds(x,y,w-2,h);
  inc(x,w);

  InstallBitBtn.SetBounds(x,y,w-2,h);
  inc(x,w);

  OptionsBitBtn.SetBounds(x,y,w-2,h);
  inc(x,w);

  CompilerOptionsBitBtn.SetBounds(x,y,ClientWidth-x,h);
  
  x:=0;
  inc(y,h+3);
  w:=ClientWidth;
  h:=Max(10,ClientHeight-y-123-StatusBar.Height);
  FilesTreeView.SetBounds(x,y,w,h);
  
  inc(y,h+3);
  h:=120;
  FilePropsGroupBox.SetBounds(x,y,w,h);
end;

procedure TPackageEditorForm.ReAddMenuItemClick(Sender: TObject);
var
  PkgFile: TPkgFile;
  AFilename: String;
  Dependency: TPkgDependency;
  Removed: boolean;
begin
  PkgFile:=GetCurrentFile(Removed);
  if (PkgFile<>nil) then begin
    if Removed then begin
      // re-add file
      AFilename:=PkgFile.Filename;
      if PkgFile.FIleType=pftUnit then begin
        if not CheckAddingUnitFilename(LazPackage,d2ptUnit,
          PackageEditors.OnGetIDEFileInfo,AFilename) then exit;
      end else begin
        if not CheckAddingUnitFilename(LazPackage,d2ptFile,
          PackageEditors.OnGetIDEFileInfo,AFilename) then exit;
      end;
      PkgFile.Filename:=AFilename;
      LazPackage.UnremovePkgFile(PkgFile);
      UpdateAll;
    end;
  end else begin
    Dependency:=GetCurrentDependency(Removed);
    if (Dependency<>nil) and (Removed) then begin
      // re-add dependency
      if not CheckAddingDependency(LazPackage,Dependency) then exit;
      LazPackage.RemoveRemovedDependency(Dependency);
      PackageGraph.AddDependencyToPackage(LazPackage,Dependency);
    end;
  end;
end;

procedure TPackageEditorForm.FilesPopupMenuPopup(Sender: TObject);
var
  ItemCnt: Integer;
  CurDependency: TPkgDependency;
  Removed: boolean;
  CurFile: TPkgFile;

  function AddPopupMenuItem(const ACaption: string; AnEvent: TNotifyEvent;
    EnabledFlag: boolean): TMenuItem;
  begin
    if FilesPopupMenu.Items.Count<=ItemCnt then begin
      Result:=TMenuItem.Create(Self);
      FilesPopupMenu.Items.Add(Result);
    end else begin
      Result:=FilesPopupMenu.Items[ItemCnt];
      while Result.Count>0 do Result.Delete(Result.Count-1);
    end;
    Result.Caption:=ACaption;
    Result.OnClick:=AnEvent;
    Result.Enabled:=EnabledFlag;
    inc(ItemCnt);
  end;
  
  procedure AddFileTypeMenuItem;
  var
    FileTypeMenuItem: TMenuItem;
    CurPFT: TPkgFileType;
    NewMenuItem: TMenuItem;
  begin
    FileTypeMenuItem:=AddPopupMenuItem('File Type',nil,true);
    for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
      NewMenuItem:=TMenuItem.Create(Self);
      NewMenuItem.Caption:=GetPkgFileTypeLocalizedName(CurPFT);
      NewMenuItem.OnClick:=@ChangeFileTypeMenuItemClick;
      NewMenuItem.Enabled:=(CurPFT<>pftUnit)
                           or FilenameIsPascalUnit(CurFile.Filename);
      FileTypeMenuItem.Add(NewMenuItem);
    end;
  end;
  
begin
  ItemCnt:=0;
  CurDependency:=GetCurrentDependency(Removed);
  if CurDependency=nil then
    CurFile:=GetCurrentFile(Removed)
  else
    CurFile:=nil;

  if CurFile<>nil then begin
    if not Removed then begin
      AddPopupMenuItem('Open file',@OpenFileMenuItemClick,true);
      AddPopupMenuItem('Remove file',@RemoveBitBtnClick,
                       RemoveBitBtn.Enabled);
      AddFileTypeMenuItem;
    end else begin
      AddPopupMenuItem('Open file',@OpenFileMenuItemClick,true);
      AddPopupMenuItem('Re-Add file',@ReAddMenuItemClick,
                       AddBitBtn.Enabled);
    end;
  end;

  if CurDependency<>nil then begin
    if (not Removed) then begin
      AddPopupMenuItem('Open package',@OpenFileMenuItemClick,true);
      AddPopupMenuItem('Remove dependency',@RemoveBitBtnClick,
                       RemoveBitBtn.Enabled);
      AddPopupMenuItem('Move dependency up',@MoveDependencyUpClick,
                       (CurDependency.PrevRequiresDependency<>nil)
                       and (not LazPackage.ReadOnly));
      AddPopupMenuItem('Move dependency down',@MoveDependencyDownClick,
                       (CurDependency.NextRequiresDependency<>nil)
                       and (not LazPackage.ReadOnly));
    end else begin
      AddPopupMenuItem('Open package',@OpenFileMenuItemClick,true);
      AddPopupMenuItem('Re-Add dependency',@ReAddMenuItemClick,
                       AddBitBtn.Enabled);
    end;
  end;
  
  if ItemCnt>0 then
    AddPopupMenuItem('-',nil,true);

  AddPopupMenuItem('Save',@SaveBitBtnClick,SaveBitBtn.Enabled);
  AddPopupMenuItem('Save As',@SaveAsClick,not LazPackage.AutoCreated);
  AddPopupMenuItem('Compile',@CompileBitBtnClick,CompileBitBtn.Enabled);
  AddPopupMenuItem('Compile All',@CompileAllClick,CompileBitBtn.Enabled);
  AddPopupMenuItem('Add',@AddBitBtnClick,AddBitBtn.Enabled);
  AddPopupMenuItem('Remove',@RemoveBitBtnClick,RemoveBitBtn.Enabled);
  AddPopupMenuItem('Install',@InstallBitBtnClick,InstallBitBtn.Enabled);
  AddPopupMenuItem('General Options',@OptionsBitBtnClick,OptionsBitBtn.Enabled);
  AddPopupMenuItem('Compiler Options',@CompilerOptionsBitBtnClick,CompilerOptionsBitBtn.Enabled);

  while FilesPopupMenu.Items.Count>ItemCnt do
    FilesPopupMenu.Items.Delete(FilesPopupMenu.Items.Count-1);
end;

procedure TPackageEditorForm.FilesTreeViewDblClick(Sender: TObject);
begin
  OpenFileMenuItemClick(Self);
end;

procedure TPackageEditorForm.FilesTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateSelectedFile;
  UpdateButtons;
end;

procedure TPackageEditorForm.InstallBitBtnClick(Sender: TObject);
begin

end;

procedure TPackageEditorForm.MaxVersionEditChange(Sender: TObject);
begin
  UpdateApplyDependencyButton;
end;

procedure TPackageEditorForm.MinVersionEditChange(Sender: TObject);
begin
  UpdateApplyDependencyButton;
end;

procedure TPackageEditorForm.MoveDependencyUpClick(Sender: TObject);
var
  CurDependency: TPkgDependency;
  Removed: boolean;
  OldSelection: TStringList;
begin
  CurDependency:=GetCurrentDependency(Removed);
  if (CurDependency=nil) or Removed then exit;
  FilesTreeView.BeginUpdate;
  OldSelection:=StoreCurrentTreeSelection;
  PackageGraph.MoveRequiredDependencyUp(CurDependency);
  ApplyTreeSelection(OldSelection,true);
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.MoveDependencyDownClick(Sender: TObject);
var
  CurDependency: TPkgDependency;
  Removed: boolean;
  OldSelection: TStringList;
begin
  CurDependency:=GetCurrentDependency(Removed);
  if (CurDependency=nil) or Removed then exit;
  FilesTreeView.BeginUpdate;
  OldSelection:=StoreCurrentTreeSelection;
  PackageGraph.MoveRequiredDependencyDown(CurDependency);
  ApplyTreeSelection(OldSelection,true);
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.OpenFileMenuItemClick(Sender: TObject);
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
  CurFile: TPkgFile;
  CurDependency: TPkgDependency;
begin
  CurNode:=FilesTreeView.Selected;
  if CurNode=nil then exit;
  NodeIndex:=CurNode.Index;
  if CurNode.Parent<>nil then begin
    if CurNode.Parent=FilesNode then begin
      CurFile:=LazPackage.Files[NodeIndex];
      PackageEditors.OpenFile(Self,CurFile.Filename);
    end else if CurNode.Parent=RequiredPackagesNode then begin
      CurDependency:=LazPackage.RequiredDepByIndex(NodeIndex);
      PackageEditors.OpenDependency(Self,CurDependency);
    end else if CurNode.Parent=RemovedFilesNode then begin
      CurFile:=LazPackage.RemovedFiles[NodeIndex];
      PackageEditors.OpenFile(Self,CurFile.Filename);
    end else if CurNode.Parent=RemovedRequiredNode then begin
      CurDependency:=LazPackage.RemovedDepByIndex(NodeIndex);
      PackageEditors.OpenDependency(Self,CurDependency);
    end;
  end;
end;

procedure TPackageEditorForm.OptionsBitBtnClick(Sender: TObject);
begin
  ShowPackageOptionsDlg(LazPackage);
  UpdateButtons;
  UpdateTitle;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.PackageEditorFormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if LazPackage=nil then exit;
  PackageEditors.SaveLayout(Self);
end;

procedure TPackageEditorForm.PackageEditorFormCloseQuery(Sender: TObject;
  var CanClose: boolean);
var
  MsgResult: Integer;
begin
  if (LazPackage=nil) or (lpfDestroying in LazPackage.Flags)
  or (LazPackage.ReadOnly) or (not LazPackage.Modified) then exit;

  MsgResult:=MessageDlg('Save Changes?',
    'Package "'+LazPackage.IDAsString+'" has changed.'#13
    +'Save package?',
    mtConfirmation,[mbYes,mbNo,mbAbort],0);
  if MsgResult=mrYes then begin
    MsgResult:=PackageEditors.SavePackage(LazPackage,false);
  end;
  if MsgResult=mrAbort then CanClose:=false;
end;

procedure TPackageEditorForm.RegisteredListBoxDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  CurComponent: TPkgComponent;
  CurStr: string;
  CurObject: TObject;
  TxtH: Integer;
  CurIcon: TBitmap;
  IconWidth: Integer;
  IconHeight: Integer;
  CurRect: TRect;
begin
  if LazPackage=nil then exit;
  if (Index<0) or (Index>=FPlugins.Count) then exit;
  CurObject:=FPlugins.Objects[Index];
  if CurObject is TPkgComponent then begin
    // draw registered component
    CurComponent:=TPkgComponent(CurObject);
    with RegisteredListBox.Canvas do begin
      CurStr:=CurComponent.ComponentClass.ClassName
              +', Page: '+CurComponent.Page.PageName;
      TxtH:=TextHeight(CurStr);
      CurRect:=ARect;
      inc(CurRect.Left,25);
      FillRect(CurRect);
      Brush.Color:=clLtGray;
      CurRect:=ARect;
      CurRect.Right:=ARect.Left+25;
      FillRect(CurRect);
      CurIcon:=CurComponent.Icon;
      if CurIcon<>nil then begin
        IconWidth:=CurIcon.Width;
        IconHeight:=CurIcon.Height;
        Draw(ARect.Left+(25-IconWidth) div 2,
             ARect.Top+(ARect.Bottom-ARect.Top-IconHeight) div 2,
             CurIcon);
      end;
      TextOut(ARect.Left+25,
              ARect.Top+(ARect.Bottom-ARect.Top-TxtH) div 2,
              CurStr);
    end;
  end;
end;

procedure TPackageEditorForm.RemoveBitBtnClick(Sender: TObject);
var
  ANode: TTreeNode;
  NodeIndex: Integer;
  CurFile: TPkgFile;
  CurDependency: TPkgDependency;
begin
  ANode:=FilesTreeView.Selected;
  if (ANode=nil) or LazPackage.ReadOnly then begin
    UpdateButtons;
    exit;
  end;
  NodeIndex:=ANode.Index;
  if ANode.Parent=FilesNode then begin
    // get current package file
    CurFile:=LazPackage.Files[NodeIndex];
    if CurFile<>nil then begin
      // confirm deletion
      if MessageDlg('Remove file?',
        'Remove file "'+CurFile.Filename+'"'#13
        +'from package "'+LazPackage.IDAsString+'"?',
        mtConfirmation,[mbYes,mbNo],0)=mrNo
      then
        exit;
      LazPackage.RemoveFile(CurFile);
    end;
    UpdateAll;
  end else if ANode.Parent=RequiredPackagesNode then begin
    // get current dependency
    CurDependency:=LazPackage.RequiredDepByIndex(NodeIndex);
    if CurDependency<>nil then begin
      // confirm deletion
      if MessageDlg('Remove Dependency?',
        'Remove dependency "'+CurDependency.AsString+'"'#13
        +'from package "'+LazPackage.IDAsString+'"?',
        mtConfirmation,[mbYes,mbNo],0)=mrNo
      then
        exit;
      PackageGraph.RemoveDependencyFromPackage(LazPackage,CurDependency,true);
    end;
  end;
end;

procedure TPackageEditorForm.SaveBitBtnClick(Sender: TObject);
begin
  DoSave(false);
end;

procedure TPackageEditorForm.SaveAsClick(Sender: TObject);
begin
  DoSave(true);
end;

procedure TPackageEditorForm.UseMaxVersionCheckBoxClick(Sender: TObject);
begin
  MaxVersionEdit.Enabled:=UseMaxVersionCheckBox.Checked;
  UpdateApplyDependencyButton;
end;

procedure TPackageEditorForm.UseMinVersionCheckBoxClick(Sender: TObject);
begin
  MinVersionEdit.Enabled:=UseMinVersionCheckBox.Checked;
  UpdateApplyDependencyButton;
end;

procedure TPackageEditorForm.FilePropsGroupBoxResize(Sender: TObject);
var
  y: Integer;
  x: Integer;
begin
  // components for files
  with CallRegisterProcCheckBox do
    SetBounds(3,0,Parent.ClientWidth,Height);

  y:=CallRegisterProcCheckBox.Top+CallRegisterProcCheckBox.Height+3;
  with RegisteredPluginsGroupBox do
    SetBounds(0,y,Parent.ClientWidth,Parent.ClientHeight-y);
    
  // components for dependencies
  x:=5;
  y:=3;
  with UseMinVersionCheckBox do
    SetBounds(x,y,150,MinVersionEdit.Height);
  inc(x,UseMinVersionCheckBox.Width+5);

  with MinVersionEdit do
    SetBounds(x,y,120,Height);
    
  x:=5;
  inc(y,MinVersionEdit.Height+5);
  with UseMaxVersionCheckBox do
    SetBounds(x,y,UseMinVersionCheckBox.Width,MaxVersionEdit.Height);
  inc(x,UseMaxVersionCheckBox.Width+5);

  with MaxVersionEdit do
    SetBounds(x,y,MinVersionEdit.Width,Height);
  inc(y,MaxVersionEdit.Height+10);

  x:=5;
  with ApplyDependencyButton do
    SetBounds(x,y,150,Height);
end;

procedure TPackageEditorForm.AddBitBtnClick(Sender: TObject);
var
  AddParams: TAddToPkgResult;
  NewFilename: String;
begin
  if LazPackage.ReadOnly then begin
    UpdateButtons;
    exit;
  end;
  
  if ShowAddToPackageDlg(LazPackage,AddParams,PackageEditors.OnGetIDEFileInfo,
    PackageEditors.OnGetUnitRegisterInfo)
    <>mrOk
  then
    exit;

  PackageGraph.BeginUpdate(false);
  case AddParams.AddType of

  d2ptUnit:
    begin
      // add unit file
      with AddParams do
        LazPackage.AddFile(UnitFilename,UnitName,FileType,PkgFileFlags,cpNormal);
      // add lfm file
      if AddParams.AutoAddLFMFile then begin
        NewFilename:=ChangeFileExt(AddParams.UnitFilename,'.lfm');
        if FileExists(NewFilename)
        and (LazPackage.FindPkgFile(NewFilename,false,true)=nil) then
          LazPackage.AddFile(NewFilename,'',pftLFM,[],cpNormal);
      end;
      // add lrs file
      if AddParams.AutoAddLRSFile then begin
        NewFilename:=ChangeFileExt(AddParams.UnitFilename,'.lrs');
        if FileExists(NewFilename)
        and (LazPackage.FindPkgFile(NewFilename,false,true)=nil) then
          LazPackage.AddFile(NewFilename,'',pftLRS,[],cpNormal);
      end;
      UpdateAll;
    end;

  d2ptNewComponent:
    begin
      // add file
      with AddParams do
        LazPackage.AddFile(UnitFilename,UnitName,FileType,PkgFileFlags,cpNormal);
      // add dependency
      if AddParams.Dependency<>nil then begin
        PackageGraph.AddDependencyToPackage(LazPackage,AddParams.Dependency);
      end;
      // open file in editor
      PackageEditors.CreateNewFile(Self,AddParams);
      UpdateAll;
    end;

  d2ptRequiredPkg:
    begin
      // add dependency
      PackageGraph.AddDependencyToPackage(LazPackage,AddParams.Dependency);
    end;

  d2ptFile:
    begin
      // add file
      with AddParams do
        LazPackage.AddFile(UnitFilename,UnitName,FileType,PkgFileFlags,cpNormal);
      UpdateAll;
    end;
  
  end;
  LazPackage.Modified:=true;
  PackageGraph.EndUpdate;
end;

procedure TPackageEditorForm.ApplyDependencyButtonClick(Sender: TObject);
var
  CurDependency: TPkgDependency;
  Removed: boolean;
  NewDependency: TPkgDependency;
begin
  CurDependency:=GetCurrentDependency(Removed);
  if (CurDependency=nil) or Removed then exit;

  NewDependency:=TPkgDependency.Create;
  try
    NewDependency.Assign(CurDependency);

    // read minimum version
    if UseMinVersionCheckBox.Checked then begin
      NewDependency.Flags:=NewDependency.Flags+[pdfMinVersion];
      if not NewDependency.MinVersion.ReadString(MinVersionEdit.Text) then begin
        MessageDlg('Invalid minimum version',
          'The minimum version "'+MinVersionEdit.Text+'" '
          +'is not a valid package version.'#13
          +'(good example 1.2.3.4)',
          mtError,[mbCancel],0);
        exit;
      end;
    end else begin
      NewDependency.Flags:=NewDependency.Flags-[pdfMinVersion];
    end;

    // read maximum version
    if UseMaxVersionCheckBox.Checked then begin
      NewDependency.Flags:=NewDependency.Flags+[pdfMaxVersion];
      if not NewDependency.MaxVersion.ReadString(MaxVersionEdit.Text) then begin
        MessageDlg('Invalid maximum version',
          'The maximum version "'+MaxVersionEdit.Text+'" '
          +'is not a valid package version.'#13
          +'(good example 1.2.3.4)',
          mtError,[mbCancel],0);
        exit;
      end;
    end else begin
      NewDependency.Flags:=NewDependency.Flags-[pdfMaxVersion];
    end;

    PackageGraph.ChangeDependency(CurDependency,NewDependency);
  finally
    NewDependency.Free;
  end;
end;

procedure TPackageEditorForm.CallRegisterProcCheckBoxClick(Sender: TObject);
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
  CurFile: TPkgFile;
begin
  if LazPackage=nil then exit;
  CurNode:=FilesTreeView.Selected;
  if (CurNode=nil) then exit;
  if (CurNode.Parent=FilesNode) then begin
    NodeIndex:=CurNode.Index;
    if NodeIndex>=LazPackage.FileCount then exit;
    CurFile:=LazPackage.Files[NodeIndex];
    CurFile.HasRegisterProc:=CallRegisterProcCheckBox.Checked;
    LazPackage.Modified:=true;
    UpdateAll;
  end;
  if (RemovedFilesNode<>nil) and (CurNode.Parent=RemovedFilesNode) then begin
    NodeIndex:=CurNode.Index;
    if NodeIndex>=LazPackage.RemovedFilesCount then exit;
    CurFile:=LazPackage.RemovedFiles[NodeIndex];
    CurFile.HasRegisterProc:=CallRegisterProcCheckBox.Checked;
    UpdateAll;
  end;
end;

procedure TPackageEditorForm.ChangeFileTypeMenuItemClick(Sender: TObject);
var
  i: Integer;
  CurItem: TMenuItem;
  CurPFT: TPkgFileType;
  Removed: boolean;
  CurFile: TPkgFile;
begin
  CurItem:=TMenuItem(Sender);
  i:=CurItem.Parent.IndexOf(CurItem);
  if i<0 then exit;
  CurFile:=GetCurrentFile(Removed);
  if CurFile=nil then exit;
  for CurPFT:=Low(TPkgFileType) to High(TPkgFileType) do begin
    if CurItem.Caption=GetPkgFileTypeLocalizedName(CurPFT) then begin
      if (not FilenameIsPascalUnit(CurFIle.Filename))
      and (CurPFT=pftUnit) then exit;
      CurFile.FileType:=CurPFT;
      UpdateAll;
      exit;
    end;
  end;
end;

procedure TPackageEditorForm.CompileAllClick(Sender: TObject);
begin
  DoCompile(true);
end;

procedure TPackageEditorForm.CompileBitBtnClick(Sender: TObject);
begin
  DoCompile(false);
end;

procedure TPackageEditorForm.CompilerOptionsBitBtnClick(Sender: TObject);
var
  CompilerOptsDlg: TfrmCompilerOptions;
begin
  CompilerOptsDlg:=TfrmCompilerOptions.Create(Self);
  CompilerOptsDlg.CompilerOpts:=LazPackage.CompilerOptions;
  with CompilerOptsDlg do begin
    GetCompilerOptions;
    Caption:='Compiler Options for Package '+LazPackage.IDAsString;
    ReadOnly:=LazPackage.ReadOnly;
    ShowModal;
    Free;
  end;
  UpdateButtons;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.SetLazPackage(const AValue: TLazPackage);
begin
  if FLazPackage=AValue then exit;
  if FLazPackage<>nil then FLazPackage.Editor:=nil;
  FLazPackage:=AValue;
  if FLazPackage=nil then exit;
  FLazPackage.Editor:=Self;
  PackageEditors.ApplyLayout(Self);
  // update components
  UpdateAll;
  // show files
  FilesNode.Expanded:=true;
end;

procedure TPackageEditorForm.SetupComponents;

  procedure AddResImg(const ResName: string);
  var Pixmap: TPixmap;
  begin
    Pixmap:=TPixmap.Create;
    Pixmap.TransparentColor:=clWhite;
    Pixmap.LoadFromLazarusResource(ResName);
    ImageList.Add(Pixmap,nil)
  end;
  
  procedure LoadBitBtnGlyph(ABitBtn: TBitBtn; const ResName: string);
  var Pixmap: TPixmap;
  begin
    Pixmap:=TPixmap.Create;
    Pixmap.TransparentColor:=clWhite;
    Pixmap.LoadFromLazarusResource(ResName);
    ABitBtn.Glyph:=Pixmap;
  end;

begin
  ImageList:=TImageList.Create(Self);
  with ImageList do begin
    Width:=16;
    Height:=16;
    Name:='ImageList';
    ImageIndexFiles:=Count;
    AddResImg('pkg_files');
    ImageIndexRemovedFiles:=Count;
    AddResImg('pkg_removedfiles');
    ImageIndexRequired:=Count;
    AddResImg('pkg_required');
    ImageIndexRemovedRequired:=Count;
    AddResImg('pkg_removedrequired');
    ImageIndexUnit:=Count;
    AddResImg('pkg_unit');
    ImageIndexRegisterUnit:=Count;
    AddResImg('pkg_registerunit');
    ImageIndexLFM:=Count;
    AddResImg('pkg_lfm');
    ImageIndexLRS:=Count;
    AddResImg('pkg_lrs');
    ImageIndexInclude:=Count;
    AddResImg('pkg_include');
    ImageIndexText:=Count;
    AddResImg('pkg_text');
    ImageIndexBinary:=Count;
    AddResImg('pkg_binary');
    ImageIndexConflict:=Count;
    AddResImg('pkg_conflict');
  end;
  
  SaveBitBtn:=TBitBtn.Create(Self);
  with SaveBitBtn do begin
    Name:='SaveBitBtn';
    Parent:=Self;
    Caption:='Save';
    OnClick:=@SaveBitBtnClick;
    Hint:='Save package';
    ShowHint:=true;
  end;

  CompileBitBtn:=TBitBtn.Create(Self);
  with CompileBitBtn do begin
    Name:='CompileBitBtn';
    Parent:=Self;
    Caption:='Compile';
    OnClick:=@CompileBitBtnClick;
    Hint:='Compile package';
    ShowHint:=true;
  end;
  
  AddBitBtn:=TBitBtn.Create(Self);
  with AddBitBtn do begin
    Name:='AddBitBtn';
    Parent:=Self;
    Caption:='Add';
    OnClick:=@AddBitBtnClick;
    Hint:='Add an item';
    ShowHint:=true;
  end;

  RemoveBitBtn:=TBitBtn.Create(Self);
  with RemoveBitBtn do begin
    Name:='RemoveBitBtn';
    Parent:=Self;
    Caption:='Remove';
    OnClick:=@RemoveBitBtnClick;
    Hint:='Remove selected item';
    ShowHint:=true;
  end;

  InstallBitBtn:=TBitBtn.Create(Self);
  with InstallBitBtn do begin
    Name:='InstallBitBtn';
    Parent:=Self;
    Caption:='Install';
    OnClick:=@InstallBitBtnClick;
    Hint:='Install package in the IDE';
    ShowHint:=true;
  end;

  OptionsBitBtn:=TBitBtn.Create(Self);
  with OptionsBitBtn do begin
    Name:='OptionsBitBtn';
    Parent:=Self;
    Caption:='Options';
    OnClick:=@OptionsBitBtnClick;
    Hint:='Edit General Options';
    ShowHint:=true;
  end;

  CompilerOptionsBitBtn:=TBitBtn.Create(Self);
  with CompilerOptionsBitBtn do begin
    Name:='CompilerOptionsBitBtn';
    Parent:=Self;
    Caption:='Comp. Opts.';
    OnClick:=@CompilerOptionsBitBtnClick;
    Hint:='Edit Options to compile package';
    ShowHint:=true;
  end;

  FilesPopupMenu:=TPopupMenu.Create(Self);
  with FilesPopupMenu do begin
    OnPopup:=@FilesPopupMenuPopup;
  end;

  FilesTreeView:=TTreeView.Create(Self);
  with FilesTreeView do begin
    Name:='FilesTreeView';
    Parent:=Self;
    BeginUpdate;
    Images:=ImageList;
    FilesNode:=Items.Add(nil,'Files');
    FilesNode.ImageIndex:=ImageIndexFiles;
    FilesNode.SelectedIndex:=FilesNode.ImageIndex;
    RequiredPackagesNode:=Items.Add(nil,'Required Packages');
    RequiredPackagesNode.ImageIndex:=ImageIndexRequired;
    RequiredPackagesNode.SelectedIndex:=RequiredPackagesNode.ImageIndex;
    EndUpdate;
    PopupMenu:=FilesPopupMenu;
    OnSelectionChanged:=@FilesTreeViewSelectionChanged;
    Options:=Options+[tvoRightClickSelect];
    OnDblClick:=@FilesTreeViewDblClick;
  end;

  FilePropsGroupBox:=TGroupBox.Create(Self);
  with FilePropsGroupBox do begin
    Name:='FilePropsGroupBox';
    Parent:=Self;
    Caption:='File Properties';
    OnResize:=@FilePropsGroupBoxResize;
  end;

  CallRegisterProcCheckBox:=TCheckBox.Create(Self);
  with CallRegisterProcCheckBox do begin
    Name:='CallRegisterProcCheckBox';
    Parent:=FilePropsGroupBox;
    Caption:='Register unit';
    UseOnChange:=true;
    OnClick:=@CallRegisterProcCheckBoxClick;
    Hint:='Call "Register" procedure of selected unit';
    ShowHint:=true;
  end;

  RegisteredPluginsGroupBox:=TGroupBox.Create(Self);
  with RegisteredPluginsGroupBox do begin
    Name:='RegisteredPluginsGroupBox';
    Parent:=FilePropsGroupBox;
    Caption:='Registered plugins';
  end;

  RegisteredListBox:=TListBox.Create(Self);
  with RegisteredListBox do begin
    Name:='RegisteredListBox';
    Parent:=RegisteredPluginsGroupBox;
    Align:=alClient;
    ItemHeight:=23;
    OnDrawItem:=@RegisteredListBoxDrawItem;
  end;
  
  UseMinVersionCheckBox:=TCheckBox.Create(Self);
  with UseMinVersionCheckBox do begin
    Name:='UseMinVersionCheckBox';
    Parent:=FilePropsGroupBox;
    Caption:='Minimum Version:';
    UseOnChange:=true;
    OnClick:=@UseMinVersionCheckBoxClick;
  end;
  
  MinVersionEdit:=TEdit.Create(Self);
  with MinVersionEdit do begin
    Name:='MinVersionEdit';
    Parent:=FilePropsGroupBox;
    Text:='';
    OnChange:=@MinVersionEditChange;
  end;

  UseMaxVersionCheckBox:=TCheckBox.Create(Self);
  with UseMaxVersionCheckBox do begin
    Name:='UseMaxVersionCheckBox';
    Parent:=FilePropsGroupBox;
    Caption:='Maximum Version:';
    UseOnChange:=true;
    OnClick:=@UseMaxVersionCheckBoxClick;
  end;

  MaxVersionEdit:=TEdit.Create(Self);
  with MaxVersionEdit do begin
    Name:='MaxVersionEdit';
    Parent:=FilePropsGroupBox;
    Text:='';
    OnChange:=@MaxVersionEditChange;
  end;
  
  ApplyDependencyButton:=TButton.Create(Self);
  with ApplyDependencyButton do begin
    Name:='ApplyDependencyButton';
    Parent:=FilePropsGroupBox;
    Caption:='Apply changes';
    OnClick:=@ApplyDependencyButtonClick;
  end;

  StatusBar:=TStatusBar.Create(Self);
  with StatusBar do begin
    Name:='StatusBar';
    Parent:=Self;
    Align:=alBottom;
  end;
end;

procedure TPackageEditorForm.UpdateAll;
begin
  if LazPackage=nil then exit;
  FilesTreeView.BeginUpdate;
  UpdateTitle;
  UpdateButtons;
  UpdateFiles;
  UpdateRequiredPkgs;
  UpdateSelectedFile;
  UpdateStatusBar;
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.UpdateTitle;
var
  NewCaption: String;
begin
  if LazPackage=nil then exit;
  NewCaption:='Package '+FLazPackage.Name;
  if LazPackage.Modified then
    NewCaption:=NewCaption+'*';
  Caption:=NewCaption;
end;

procedure TPackageEditorForm.UpdateButtons;
begin
  if LazPackage=nil then exit;
  SaveBitBtn.Enabled:=(not LazPackage.ReadOnly)
                              and (LazPackage.IsVirtual or LazPackage.Modified);
  CompileBitBtn.Enabled:=(not LazPackage.IsVirtual);
  AddBitBtn.Enabled:=not LazPackage.ReadOnly;
  RemoveBitBtn.Enabled:=(not LazPackage.ReadOnly)
     and (FilesTreeView.Selected<>nil)
     and ((FilesTreeView.Selected.Parent=FilesNode)
           or (FilesTreeView.Selected.Parent=RequiredPackagesNode));
  InstallBitBtn.Enabled:=(not LazPackage.AutoCreated);
  OptionsBitBtn.Enabled:=true;
  CompilerOptionsBitBtn.Enabled:=true;
end;

procedure TPackageEditorForm.UpdateFiles;

  procedure SetImageIndex(ANode: TTreeNode; PkgFile: TPkgFile);
  begin
    case PkgFile.FileType of
    pftUnit:
      if PkgFile.HasRegisterProc then
        ANode.ImageIndex:=ImageIndexRegisterUnit
      else
        ANode.ImageIndex:=ImageIndexUnit;
    pftLFM: ANode.ImageIndex:=ImageIndexLFM;
    pftLRS: ANode.ImageIndex:=ImageIndexLRS;
    pftInclude: ANode.ImageIndex:=ImageIndexInclude;
    pftText: ANode.ImageIndex:=ImageIndexText;
    pftBinary: ANode.ImageIndex:=ImageIndexBinary;
    else
      ANode.ImageIndex:=-1;
    end;
    ANode.SelectedIndex:=ANode.ImageIndex;
  end;

var
  Cnt: Integer;
  i: Integer;
  CurFile: TPkgFile;
  CurNode: TTreeNode;
  NextNode: TTreeNode;
begin
  if LazPackage=nil then exit;
  FilesTreeView.BeginUpdate;
  
  // files
  CurNode:=FilesNode.GetFirstChild;
  Cnt:=LazPackage.FileCount;
  for i:=0 to Cnt-1 do begin
    if CurNode=nil then
      CurNode:=FilesTreeView.Items.AddChild(FilesNode,'');
    CurFile:=LazPackage.Files[i];
    CurNode.Text:=CurFile.GetShortFilename;
    SetImageIndex(CurNode,CurFile);
    CurNode:=CurNode.GetNextSibling;
  end;
  while CurNode<>nil do begin
    NextNode:=CurNode.GetNextSibling;
    CurNode.Free;
    CurNode:=NextNode;
  end;
  FilesNode.Expanded:=true;
  
  // removed files
  if LazPackage.RemovedFilesCount>0 then begin
    if RemovedFilesNode=nil then begin
      RemovedFilesNode:=
        FilesTreeView.Items.Add(RequiredPackagesNode,
                'Removed Files (these entries are not saved to the lpk file)');
      RemovedFilesNode.ImageIndex:=ImageIndexRemovedFiles;
      RemovedFilesNode.SelectedIndex:=RemovedFilesNode.ImageIndex;
    end;
    CurNode:=RemovedFilesNode.GetFirstChild;
    Cnt:=LazPackage.RemovedFilesCount;
    for i:=0 to Cnt-1 do begin
      if CurNode=nil then
        CurNode:=FilesTreeView.Items.AddChild(RemovedFilesNode,'');
      CurFile:=LazPackage.RemovedFiles[i];
      CurNode.Text:=CurFile.GetShortFilename;
      SetImageIndex(CurNode,CurFile);
      CurNode:=CurNode.GetNextSibling;
    end;
    while CurNode<>nil do begin
      NextNode:=CurNode.GetNextSibling;
      CurNode.Free;
      CurNode:=NextNode;
    end;
    RemovedFilesNode.Expanded:=true;
  end else begin
    FreeAndNil(RemovedFilesNode);
  end;
  
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.UpdateRequiredPkgs;
var
  CurNode: TTreeNode;
  CurDependency: TPkgDependency;
  NextNode: TTreeNode;
begin
  if LazPackage=nil then exit;
  FilesTreeView.BeginUpdate;
  
  // required packages
  CurNode:=RequiredPackagesNode.GetFirstChild;
  CurDependency:=LazPackage.FirstRequiredDependency;
  while CurDependency<>nil do begin
    if CurNode=nil then
      CurNode:=FilesTreeView.Items.AddChild(RequiredPackagesNode,'');
    CurNode.Text:=CurDependency.AsString;
    if CurDependency.LoadPackageResult=lprSuccess then
      CurNode.ImageIndex:=ImageIndexRequired
    else
      CurNode.ImageIndex:=ImageIndexConflict;
    CurNode.SelectedIndex:=CurNode.ImageIndex;
    CurNode:=CurNode.GetNextSibling;
    CurDependency:=CurDependency.NextRequiresDependency;
  end;
  while CurNode<>nil do begin
    NextNode:=CurNode.GetNextSibling;
    CurNode.Free;
    CurNode:=NextNode;
  end;
  RequiredPackagesNode.Expanded:=true;
  
  // removed required packages
  CurDependency:=LazPackage.FirstRemovedDependency;
  if CurDependency<>nil then begin
    if RemovedRequiredNode=nil then begin
      RemovedRequiredNode:=
        FilesTreeView.Items.Add(nil,
          'Removed required packages (these entries are not saved to the lpk file)');
      RemovedRequiredNode.ImageIndex:=ImageIndexRemovedRequired;
      RemovedRequiredNode.SelectedIndex:=RemovedRequiredNode.ImageIndex;
    end;
    CurNode:=RemovedRequiredNode.GetFirstChild;
    while CurDependency<>nil do begin
      if CurNode=nil then
        CurNode:=FilesTreeView.Items.AddChild(RemovedRequiredNode,'');
      CurNode.Text:=CurDependency.AsString;
      CurNode.ImageIndex:=RemovedRequiredNode.ImageIndex;
      CurNode.SelectedIndex:=CurNode.ImageIndex;
      CurNode:=CurNode.GetNextSibling;
      CurDependency:=CurDependency.NextRequiresDependency;
    end;
    while CurNode<>nil do begin
      NextNode:=CurNode.GetNextSibling;
      CurNode.Free;
      CurNode:=NextNode;
    end;
    RemovedRequiredNode.Expanded:=true;
  end else begin
    FreeAndNil(RemovedRequiredNode);
  end;

  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.UpdateSelectedFile;
var
  CurFile: TPkgFile;
  i: Integer;
  CurComponent: TPkgComponent;
  CurLine: string;
  CurListIndex: Integer;
  RegCompCnt: Integer;
  Dependency: TPkgDependency;
  Removed: boolean;
begin
  if LazPackage=nil then exit;
  FPlugins.Clear;
  CurFile:=GetCurrentFile(Removed);
  if CurFile=nil then
    Dependency:=GetCurrentDependency(Removed)
  else
    Dependency:=nil;

  // make components visible
  UseMinVersionCheckBox.Visible:=Dependency<>nil;
  MinVersionEdit.Visible:=Dependency<>nil;
  UseMaxVersionCheckBox.Visible:=Dependency<>nil;
  MaxVersionEdit.Visible:=Dependency<>nil;
  ApplyDependencyButton.Visible:=Dependency<>nil;
  
  CallRegisterProcCheckBox.Visible:=CurFile<>nil;
  RegisteredPluginsGroupBox.Visible:=CurFile<>nil;

  if CurFile<>nil then begin
    FilePropsGroupBox.Enabled:=true;
    FilePropsGroupBox.Caption:='File Properties';
    // set Register Unit checkbox
    CallRegisterProcCheckBox.Enabled:=(not LazPackage.ReadOnly)
                                      and (CurFile.FileType=pftUnit);
    CallRegisterProcCheckBox.Checked:=pffHasRegisterProc in CurFile.Flags;
    // fetch all registered plugins
    CurListIndex:=0;
    RegCompCnt:=CurFile.ComponentCount;
    for i:=0 to RegCompCnt-1 do begin
      CurComponent:=CurFile.Components[i];
      CurLine:=CurComponent.ComponentClass.ClassName;
      FPlugins.AddObject(CurLine,CurComponent);
      inc(CurListIndex);
    end;
    // put them in the RegisteredListBox
    RegisteredListBox.Items.Assign(FPlugins);
  end else if Dependency<>nil then begin
    FilePropsGroupBox.Enabled:=not Removed;
    FilePropsGroupBox.Caption:='Dependency Properties';
    UseMinVersionCheckBox.Checked:=pdfMinVersion in Dependency.Flags;
    MinVersionEdit.Text:=Dependency.MinVersion.AsString;
    MinVersionEdit.Enabled:=pdfMinVersion in Dependency.Flags;
    UseMaxVersionCheckBox.Checked:=pdfMaxVersion in Dependency.Flags;
    MaxVersionEdit.Text:=Dependency.MaxVersion.AsString;
    MaxVersionEdit.Enabled:=pdfMaxVersion in Dependency.Flags;
    UpdateApplyDependencyButton;
  end else begin
    FilePropsGroupBox.Enabled:=false;
  end;
end;

procedure TPackageEditorForm.UpdateApplyDependencyButton;
var
  DepencyChanged: Boolean;
  CurDependency: TPkgDependency;
  AVersion: TPkgVersion;
  Removed: boolean;
begin
  if LazPackage=nil then exit;
  DepencyChanged:=false;
  CurDependency:=GetCurrentDependency(Removed);
  if (CurDependency<>nil) then begin
    // check min version
    if UseMinVersionCheckBox.Checked
    <>(pdfMinVersion in CurDependency.Flags)
    then begin
      DepencyChanged:=true;
    end;
    if UseMinVersionCheckBox.Checked then begin
      AVersion:=TPkgVersion.Create;
      if AVersion.ReadString(MinVersionEdit.Text)
      and (AVersion.Compare(CurDependency.MinVersion)<>0) then begin
        DepencyChanged:=true;
      end;
      AVersion.Free;
    end;
    // check max version
    if UseMaxVersionCheckBox.Checked
    <>(pdfMaxVersion in CurDependency.Flags)
    then begin
      DepencyChanged:=true;
    end;
    if UseMaxVersionCheckBox.Checked then begin
      AVersion:=TPkgVersion.Create;
      if AVersion.ReadString(MaxVersionEdit.Text)
      and (AVersion.Compare(CurDependency.MaxVersion)<>0) then begin
        DepencyChanged:=true;
      end;
      AVersion.Free;
    end;
  end;
  ApplyDependencyButton.Enabled:=DepencyChanged;
end;

procedure TPackageEditorForm.UpdateStatusBar;
var
  StatusText: String;
begin
  if LazPackage=nil then exit;
  if LazPackage.IsVirtual and (not LazPackage.ReadOnly) then begin
    StatusText:='package '+LazPackage.Name+' not saved';
  end else begin
    StatusText:=LazPackage.Filename;
  end;
  if LazPackage.ReadOnly then
    StatusText:='Read Only: '+StatusText;
  if LazPackage.Modified then
    StatusText:='Modified: '+StatusText;
  StatusBar.SimpleText:=StatusText;
end;

function TPackageEditorForm.GetCurrentDependency(var Removed: boolean
  ): TPkgDependency;
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
begin
  Result:=nil;
  CurNode:=FilesTreeView.Selected;
  if (CurNode<>nil) and (CurNode.Parent<>nil) then begin
    NodeIndex:=CurNode.Index;
    if CurNode.Parent=RequiredPackagesNode then begin
      Result:=LazPackage.RequiredDepByIndex(NodeIndex);
      Removed:=false;
    end else if CurNode.Parent=RemovedRequiredNode then begin
      Result:=LazPackage.RemovedDepByIndex(NodeIndex);
      Removed:=true;
    end;
  end;
end;

function TPackageEditorForm.GetCurrentFile(var Removed: boolean): TPkgFile;
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
begin
  Result:=nil;
  CurNode:=FilesTreeView.Selected;
  if (CurNode<>nil) and (CurNode.Parent<>nil) then begin
    NodeIndex:=CurNode.Index;
    if CurNode.Parent=FilesNode then begin
      Result:=LazPackage.Files[NodeIndex];
      Removed:=false;
    end else if CurNode.Parent=RemovedFilesNode then begin
      Result:=LazPackage.RemovedFiles[NodeIndex];
      Removed:=true;
    end;
  end;
end;

function TPackageEditorForm.StoreCurrentTreeSelection: TStringList;
var
  ANode: TTreeNode;
begin
  Result:=TStringList.Create;
  ANode:=FilesTreeView.Selected;
  while ANode<>nil do begin
    Result.Insert(0,ANode.Text);
    ANode:=ANode.Parent;
  end;
end;

procedure TPackageEditorForm.ApplyTreeSelection(ASelection: TStringList;
  FreeList: boolean);
var
  ANode: TTreeNode;
  CurText: string;
begin
  ANode:=nil;
  while ASelection.Count>0 do begin
    CurText:=ASelection[0];
    if ANode=nil then
      ANode:=FilesTreeView.Items.GetFirstNode
    else
      ANode:=ANode.GetFirstChild;
    while ANode.Text<>CurText do ANode:=ANode.GetNextSibling;
    if ANode=nil then break;
    ASelection.Delete(0);
  end;
  if ANode<>nil then FilesTreeView.Selected:=ANode;
  if FreeList then ASelection.Free;
end;

procedure TPackageEditorForm.DoSave(SaveAs: boolean);
begin
  PackageEditors.SavePackage(LazPackage,SaveAs);
  UpdateButtons;
  UpdateTitle;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.DoCompile(CompileAll: boolean);
begin
  PackageEditors.CompilePackage(LazPackage,CompileAll);
  UpdateButtons;
  UpdateTitle;
  UpdateStatusBar;
end;

constructor TPackageEditorForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPlugins:=TStringList.Create;
  SetupComponents;
  OnResize:=@PackageEditorFormResize;
  OnCloseQuery:=@PackageEditorFormCloseQuery;
  OnClose:=@PackageEditorFormClose;
end;

destructor TPackageEditorForm.Destroy;
begin
  PackageEditors.DoFreeEditor(LazPackage);
  FreeAndNil(FPlugins);
  inherited Destroy;
end;

{ TPackageEditors }

function TPackageEditors.GetEditors(Index: integer): TPackageEditorForm;
begin
  Result:=TPackageEditorForm(FItems[Index]);
end;

procedure TPackageEditors.ApplyLayout(AnEditor: TPackageEditorForm);
var
  PkgFilename: String;
  ANode: TAVLTreeNode;
  ARect: TRect;
begin
  if fLayouts=nil then LoadLayouts;
  PkgFilename:=AnEditor.LazPackage.Filename;
  ANode:=fLayouts.FindKey(Pointer(PkgFilename),@CompareFilenameWithLayout);
  // find a nice position for the editor
  if ANode<>nil then
    ARect:=TPackageEditorLayout(ANode.Data).Rectangle
  else
    ARect:=Rect(0,0,0,0);
  if (ARect.Bottom<ARect.Top+50) or (ARect.Right<ARect.Left+50)
  or (ARect.Bottom>Screen.Height) or (ARect.Right>Screen.Width) then
    ARect:=CreateNiceWindowPosition(500,400);
  AnEditor.SetBounds(ARect.Left,ARect.Top,
                     ARect.Right-ARect.Left,ARect.Bottom-ARect.Top);
end;

procedure TPackageEditors.SaveLayout(AnEditor: TPackageEditorForm);
var
  PkgFilename: String;
  ANode: TAVLTreeNode;
  CurLayout: TPackageEditorLayout;
begin
  if fLayouts=nil then exit;
  PkgFilename:=AnEditor.LazPackage.Filename;
  ANode:=fLayouts.FindKey(Pointer(PkgFilename),@CompareFilenameWithLayout);
  if ANode<>nil then begin
    CurLayout:=TPackageEditorLayout(ANode.Data);
    fLayouts.Remove(CurLayout);
  end else begin
    CurLayout:=TPackageEditorLayout.Create;
  end;
  CurLayout.Filename:=PkgFilename;
  with AnEditor do
    CurLayout.Rectangle:=Bounds(Left,Top,Width,Height);
  fLayouts.Add(CurLayout);
end;

procedure TPackageEditors.LoadLayouts;
var
  Filename: String;
  Path: String;
  XMLConfig: TXMLConfig;
  LayoutCount: Integer;
  NewLayout: TPackageEditorLayout;
  i: Integer;
begin
  if fLayouts=nil then fLayouts:=TAVLTree.Create(@CompareLayouts);
  fLayouts.FreeAndClear;
  Filename:=GetLayoutConfigFilename;
  if not FileExists(Filename) then exit;
  try
    XMLConfig:=TXMLConfig.Create(Filename);
  except
    writeln('ERROR: unable to open package editor layouts "',Filename,'"');
    exit;
  end;
  try
    try
      Path:='PackageEditorLayouts/';
      LayoutCount:=XMLConfig.GetValue(Path+'Count/Value',0);
      for i:=1 to LayoutCount do begin
        NewLayout:=TPackageEditorLayout.Create;
        NewLayout.LoadFromXMLConfig(XMLConfig,Path+'Layout'+IntToStr(i));
        if (NewLayout.Filename='') or (fLayouts.Find(NewLayout)<>nil) then
          NewLayout.Free
        else
          fLayouts.Add(NewLayout);
      end;
    finally
      XMLConfig.Free;
    end;
  except
    on E: Exception do begin
      writeln('ERROR: unable read miscellaneous options from "',Filename,'": ',E.Message);
    end;
  end;
end;

function TPackageEditors.GetLayoutConfigFilename: string;
begin
  Result:=SetDirSeparators(GetPrimaryConfigPath+'/packageeditorlayouts.xml');
end;

constructor TPackageEditors.Create;
begin
  FItems:=TList.Create;
end;

destructor TPackageEditors.Destroy;
begin
  Clear;
  FItems.Free;
  if fLayouts<>nil then begin
    fLayouts.FreeAndClear;
    fLayouts.Free;
  end;
  inherited Destroy;
end;

function TPackageEditors.Count: integer;
begin
  Result:=FItems.Count;
end;

procedure TPackageEditors.Clear;
begin
  FItems.Clear;
end;

procedure TPackageEditors.SaveLayouts;
var
  Filename: String;
  XMLConfig: TXMLConfig;
  Path: String;
  LayoutCount: Integer;
  ANode: TAVLTreeNode;
  CurLayout: TPackageEditorLayout;
begin
  if fLayouts=nil then exit;
  Filename:=GetLayoutConfigFilename;
  try
    ClearFile(Filename,true);
    XMLConfig:=TXMLConfig.Create(Filename);
  except
    on E: Exception do begin
      writeln('ERROR: unable to open miscellaneous options "',Filename,'": ',E.Message);
      exit;
    end;
  end;
  try
    try
      Path:='PackageEditorLayouts/';
      LayoutCount:=0;
      ANode:=fLayouts.FindLowest;
      while ANode<>nil do begin
        inc(LayoutCount);
        CurLayout:=TPackageEditorLayout(ANode.Data);
        CurLayout.SaveToXMLConfig(XMLConfig,Path+'Layout'+IntToStr(LayoutCount));
        ANode:=fLayouts.FindSuccessor(ANode);
      end;
      XMLConfig.SetDeleteValue(Path+'Count/Value',LayoutCount,0);

      XMLConfig.Flush;
    finally
      XMLConfig.Free;
    end;
  except
    writeln('ERROR: unable read miscellaneous options from "',Filename,'"');
  end;
end;

procedure TPackageEditors.Remove(Editor: TPackageEditorForm);
begin
  FItems.Remove(Editor);
end;

function TPackageEditors.IndexOfPackage(Pkg: TLazPackage): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (Editors[Result].LazPackage<>Pkg) do dec(Result);
end;

function TPackageEditors.FindEditor(Pkg: TLazPackage): TPackageEditorForm;
var
  i: Integer;
begin
  i:=IndexOfPackage(Pkg);
  if i>=0 then
    Result:=Editors[i]
  else
    Result:=nil;
end;

function TPackageEditors.OpenEditor(Pkg: TLazPackage): TPackageEditorForm;
begin
  Result:=FindEditor(Pkg);
  if Result=nil then begin
    Result:=TPackageEditorForm.Create(Application);
    Result.LazPackage:=Pkg;
    FItems.Add(Result);
  end;
end;

function TPackageEditors.OpenFile(Sender: TObject; const Filename: string
  ): TModalResult;
begin
  if Assigned(OnOpenFile) then
    Result:=OnOpenFile(Sender,Filename)
  else
    Result:=mrCancel;
end;

function TPackageEditors.OpenDependency(Sender: TObject;
  Dependency: TPkgDependency): TModalResult;
var
  APackage: TLazPackage;
begin
  Result:=mrCancel;
  if PackageGraph.OpenDependency(Dependency,APackage)=lprSuccess then
  begin
    if Assigned(OnOpenPackage) then Result:=OnOpenPackage(Sender,APackage);
  end;
end;

procedure TPackageEditors.DoFreeEditor(Pkg: TLazPackage);
begin
  FItems.Remove(Pkg.Editor);
  if Assigned(OnFreeEditor) then OnFreeEditor(Pkg);
end;

function TPackageEditors.CreateNewFile(Sender: TObject;
  const Params: TAddToPkgResult): TModalResult;
begin
  Result:=mrCancel;
  if Assigned(OnCreateNewFile) then
    Result:=OnCreateNewFile(Sender,Params);
end;

function TPackageEditors.SavePackage(APackage: TLazPackage;
  SaveAs: boolean): TModalResult;
begin
  if Assigned(OnSavePackage) then
    Result:=OnSavePackage(Self,APackage,SaveAs);
end;

function TPackageEditors.CompilePackage(APackage: TLazPackage;
  CompileAll: boolean): TModalResult;
begin
  if Assigned(OnCompilePackage) then
    Result:=OnCompilePackage(Self,APackage,CompileAll);
end;

procedure TPackageEditors.UpdateAllEditors;
var
  i: Integer;
begin
  for i:=0 to Count-1 do Editors[i].UpdateAll;
end;

{ TPackageEditorLayout }

constructor TPackageEditorLayout.Create;
begin

end;

destructor TPackageEditorLayout.Destroy;
begin
  inherited Destroy;
end;

procedure TPackageEditorLayout.LoadFromXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  Filename:=XMLConfig.GetValue(Path+'Filename/Value','');
  LoadRect(XMLConfig,Path+'Rect/',Rectangle);
end;

procedure TPackageEditorLayout.SaveToXMLConfig(XMLConfig: TXMLConfig;
  const Path: string);
begin
  XMLConfig.SetDeleteValue(Path+'Filename/Value',Filename,'');
  SaveRect(XMLConfig,Path+'Rect/',Rectangle);
end;

initialization
  PackageEditors:=nil;

end.

