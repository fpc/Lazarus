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
  LResources, Graphics, LCLType, Menus, Dialogs, LazarusIDEStrConsts,
  IDEOptionDefs, IDEDefs, ComponentReg, PackageDefs, AddToPackageDlg,
  PackageSystem;
  
type
  TOnOpenFile =
    function(Sender: TObject; const Filename: string): TModalResult of Object;
  TOnOpenPackage =
    function(Sender: TObject; APackage: TLazPackage): TModalResult of Object;
  TOnCreateNewPkgFile =
    function(Sender: TObject;
             const Params: TAddToPkgResult): TModalResult  of object;


  { TPackageEditorForm }

  TPackageEditorForm = class(TBasePackageEditor)
    SaveBitBtn: TBitBtn;
    CompileBitBtn: TBitBtn;
    AddBitBtn: TBitBtn;
    RemoveBitBtn: TBitBtn;
    InstallBitBtn: TBitBtn;
    OptionsBitBtn: TBitBtn;
    FilesTreeView: TTreeView;
    FilePropsGroupBox: TGroupBox;
    CallRegisterProcCheckBox: TCheckBox;
    RegisteredPluginsGroupBox: TGroupBox;
    RegisteredListBox: TListBox;
    StatusBar: TStatusBar;
    ImageList: TImageList;
    FilesPopupMenu: TPopupMenu;
    procedure AddBitBtnClick(Sender: TObject);
    procedure FilePropsGroupBoxResize(Sender: TObject);
    procedure FilesPopupMenuPopup(Sender: TObject);
    procedure FilesTreeViewSelectionChanged(Sender: TObject);
    procedure OpenFileMenuItemClick(Sender: TObject);
    procedure PackageEditorFormResize(Sender: TObject);
    procedure RegisteredListBoxDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure RemoveBitBtnClick(Sender: TObject);
    procedure SaveBitBtnClick(Sender: TObject);
  private
    FLazPackage: TLazPackage;
    FilesNode: TTreeNode;
    RequiredPackagesNode: TTreeNode;
    FPlugins: TStringList;
    procedure SetLazPackage(const AValue: TLazPackage);
    procedure SetupComponents;
    procedure UpdateAll;
    procedure UpdateTitle;
    procedure UpdateButtons;
    procedure UpdateFiles;
    procedure UpdateRequiredPkgs;
    procedure UpdateSelectedFile;
    procedure UpdateStatusBar;
    procedure DoSave;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  public
    property LazPackage: TLazPackage read FLazPackage write SetLazPackage;
  end;
  
  
  { TPackageEditors }
  
  TPackageEditors = class
  private
    FItems: TList; // list of TPackageEditorForm
    FOnCreateNewFile: TOnCreateNewPkgFile;
    FOnGetIDEFileInfo: TGetIDEFileStateEvent;
    FOnGetUnitRegisterInfo: TOnGetUnitRegisterInfo;
    FOnOpenFile: TOnOpenFile;
    FOnOpenPackage: TOnOpenPackage;
    FOnSavePackage: TNotifyEvent;
    function GetEditors(Index: integer): TPackageEditorForm;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: integer;
    procedure Clear;
    procedure Remove(Editor: TPackageEditorForm);
    function IndexOfPackage(Pkg: TLazPackage): integer;
    function FindEditor(Pkg: TLazPackage): TPackageEditorForm;
    function OpenEditor(Pkg: TLazPackage): TPackageEditorForm;
    function OpenFile(Sender: TObject; const Filename: string): TModalResult;
    function OpenDependency(Sender: TObject;
                            Dependency: TPkgDependency): TModalResult;
    function CreateNewFile(Sender: TObject;
                           const Params: TAddToPkgResult): TModalResult;
    procedure SavePackage(APackage: TLazPackage);
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
    property OnSavePackage: TNotifyEvent read FOnSavePackage write FOnSavePackage;
  end;
  
var
  PackageEditors: TPackageEditors;


implementation


uses Math;

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
  w:=75;
  h:=25;
  SaveBitBtn.SetBounds(x,y,w,h);
  inc(x,w+2);

  CompileBitBtn.SetBounds(x,y,w,h);
  inc(x,w+2);

  AddBitBtn.SetBounds(x,y,w,h);
  inc(x,w+2);

  RemoveBitBtn.SetBounds(x,y,w,h);
  inc(x,w+2);

  InstallBitBtn.SetBounds(x,y,w,h);
  inc(x,w+2);

  OptionsBitBtn.SetBounds(x,y,w,h);

  x:=0;
  inc(y,h+3);
  w:=ClientWidth;
  h:=Max(10,ClientHeight-y-123-StatusBar.Height);
  FilesTreeView.SetBounds(x,y,w,h);
  
  inc(y,h+3);
  h:=120;
  FilePropsGroupBox.SetBounds(x,y,w,h);
end;

procedure TPackageEditorForm.FilesPopupMenuPopup(Sender: TObject);
var
  CurNode: TTreeNode;
  ItemCnt: Integer;
  
  procedure AddPopupMenuItem(const ACaption: string; AnEvent: TNotifyEvent);
  var
    CurMenuItem: TMenuItem;
  begin
    if FilesPopupMenu.Items.Count<=ItemCnt then begin
      CurMenuItem:=TMenuItem.Create(Self);
      FilesPopupMenu.Items.Add(CurMenuItem);
    end else
      CurMenuItem:=FilesPopupMenu.Items[FilesPopupMenu.Items.Count-1];
    CurMenuItem.Caption:=ACaption;
    CurMenuItem.OnClick:=AnEvent;
    inc(ItemCnt);
  end;

begin
  CurNode:=FilesTreeView.Selected;
  ItemCnt:=0;
  if CurNode<>nil then begin
    if CurNode.Parent=FilesNode then begin
      AddPopupMenuItem('Open file',@OpenFileMenuItemClick);
    end else if (CurNode.Parent=RequiredPackagesNode) then begin
      AddPopupMenuItem('Open package',@OpenFileMenuItemClick);
    end;
  end else begin

  end;
  while FilesPopupMenu.Items.Count>ItemCnt do
    FilesPopupMenu.Items.Delete(FilesPopupMenu.Items.Count-1);
end;

procedure TPackageEditorForm.FilesTreeViewSelectionChanged(Sender: TObject);
begin
  UpdateSelectedFile;
  UpdateButtons;
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
  if CurNode.Parent=FilesNode then begin
    CurFile:=LazPackage.Files[NodeIndex];
    PackageEditors.OpenFile(Self,CurFile.Filename);
  end else if CurNode.Parent=RequiredPackagesNode then begin
    CurDependency:=LazPackage.RequiredPkgs[NodeIndex];
    PackageEditors.OpenDependency(Self,CurDependency);
  end;
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
    if CurFile=nil then exit;
    if MessageDlg('Remove file?',
      'Remove file "'+CurFile.Filename+'"'#13
      +'from package "'+LazPackage.IDAsString+'"?',
      mtConfirmation,[mbYes,mbNo],0)=mrNo
    then
      exit;
      
    // ToDo
    
  end else if ANode.Parent=RequiredPackagesNode then begin

  end;
end;

procedure TPackageEditorForm.SaveBitBtnClick(Sender: TObject);
begin
  DoSave;
end;

procedure TPackageEditorForm.FilePropsGroupBoxResize(Sender: TObject);
var
  y: Integer;
begin
  with CallRegisterProcCheckBox do
    SetBounds(3,0,Parent.ClientWidth,Height);

  y:=CallRegisterProcCheckBox.Top+CallRegisterProcCheckBox.Height+3;
  with RegisteredPluginsGroupBox do begin
    SetBounds(0,y,Parent.ClientWidth,Parent.ClientHeight-y);
  end;
end;

procedure TPackageEditorForm.AddBitBtnClick(Sender: TObject);
var
  AddParams: TAddToPkgResult;
begin
  if ShowAddToPackageDlg(LazPackage,AddParams,PackageEditors.OnGetIDEFileInfo,
    PackageEditors.OnGetUnitRegisterInfo)
    <>mrOk
  then
    exit;

  case AddParams.AddType of
  d2ptUnit:
    begin
      // add file
      with AddParams do
        LazPackage.AddFile(UnitFilename,UnitName,FileType,PkgFileFlags,cpNormal);
      UpdateFiles;
    end;

  d2ptNewComponent:
    begin
      // add file
      with AddParams do
        LazPackage.AddFile(UnitFilename,UnitName,FileType,PkgFileFlags,cpNormal);
      UpdateFiles;
      // add dependency
      if AddParams.Dependency<>nil then begin
        LazPackage.AddRequiredDependency(AddParams.Dependency);
        UpdateRequiredPkgs;
      end;
      // open file in editor
      PackageEditors.CreateNewFile(Self,AddParams);
    end;

  d2ptRequiredPkg:
    begin
      // add dependency
      LazPackage.AddRequiredDependency(AddParams.Dependency);
      UpdateRequiredPkgs;
    end;
    
  end;
  
  UpdateSelectedFile;
  UpdateStatusBar;
end;

procedure TPackageEditorForm.SetLazPackage(const AValue: TLazPackage);
var
  ARect: TRect;
begin
  if FLazPackage=AValue then exit;
  FLazPackage:=AValue;
  FLazPackage.Editor:=Self;
  // find a nice position for the editor
  ARect:=FLazPackage.EditorRect;
  if (ARect.Bottom<ARect.Top+50) or (ARect.Right<ARect.Left+50) then
    ARect:=CreateNiceWindowPosition(500,400);
  SetBounds(ARect.Left,ARect.Top,
            ARect.Right-ARect.Left,ARect.Bottom-ARect.Top);
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
    AddResImg('pkg_files');
    AddResImg('pkg_required');
    AddResImg('pkg_conflict');
    AddResImg('pkg_unit');
    AddResImg('pkg_text');
    AddResImg('pkg_binary');
  end;
  
  SaveBitBtn:=TBitBtn.Create(Self);
  with SaveBitBtn do begin
    Name:='SaveBitBtn';
    Parent:=Self;
    Caption:='Save';
    OnClick:=@SaveBitBtnClick;
  end;

  CompileBitBtn:=TBitBtn.Create(Self);
  with CompileBitBtn do begin
    Name:='CompileBitBtn';
    Parent:=Self;
    Caption:='Compile';
  end;
  
  AddBitBtn:=TBitBtn.Create(Self);
  with AddBitBtn do begin
    Name:='AddBitBtn';
    Parent:=Self;
    Caption:='Add';
    OnClick:=@AddBitBtnClick;
  end;

  RemoveBitBtn:=TBitBtn.Create(Self);
  with RemoveBitBtn do begin
    Name:='RemoveBitBtn';
    Parent:=Self;
    Caption:='Remove';
    OnClick:=@RemoveBitBtnClick;
  end;

  InstallBitBtn:=TBitBtn.Create(Self);
  with InstallBitBtn do begin
    Name:='InstallBitBtn';
    Parent:=Self;
    Caption:='Install';
  end;

  OptionsBitBtn:=TBitBtn.Create(Self);
  with OptionsBitBtn do begin
    Name:='OptionsBitBtn';
    Parent:=Self;
    Caption:='Options';
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
    FilesNode.ImageIndex:=0;
    FilesNode.SelectedIndex:=FilesNode.ImageIndex;
    RequiredPackagesNode:=Items.Add(nil,'Required Packages');
    RequiredPackagesNode.ImageIndex:=1;
    RequiredPackagesNode.SelectedIndex:=RequiredPackagesNode.ImageIndex;
    EndUpdate;
    PopupMenu:=FilesPopupMenu;
    OnSelectionChanged:=@FilesTreeViewSelectionChanged;
    Options:=Options+[tvoRightClickSelect];
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

  StatusBar:=TStatusBar.Create(Self);
  with StatusBar do begin
    Name:='StatusBar';
    Parent:=Self;
    Align:=alBottom;
  end;
end;

procedure TPackageEditorForm.UpdateAll;
begin
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
begin
  Caption:='Package '+FLazPackage.Name;
end;

procedure TPackageEditorForm.UpdateButtons;
begin
  SaveBitBtn.Enabled:=(not LazPackage.ReadOnly)
                              and (LazPackage.IsVirtual or LazPackage.Modified);
  CompileBitBtn.Enabled:=(not LazPackage.IsVirtual);
  AddBitBtn.Enabled:=not LazPackage.ReadOnly;
  RemoveBitBtn.Enabled:=(not LazPackage.ReadOnly)
     and (FilesTreeView.Selected<>nil) and (FilesTreeView.Selected.Parent<>nil);
  InstallBitBtn.Enabled:=(not LazPackage.IsVirtual);
  OptionsBitBtn.Enabled:=true;
end;

procedure TPackageEditorForm.UpdateFiles;
var
  Cnt: Integer;
  i: Integer;
  CurFile: TPkgFile;
  CurNode: TTreeNode;
  NextNode: TTreeNode;
begin
  Cnt:=LazPackage.FileCount;
  FilesTreeView.BeginUpdate;
  CurNode:=FilesNode.GetFirstChild;
  for i:=0 to Cnt-1 do begin
    if CurNode=nil then
      CurNode:=FilesTreeView.Items.AddChild(FilesNode,'');
    CurFile:=LazPackage.Files[i];
    CurNode.Text:=CurFile.GetShortFilename;
    case CurFile.FileType of
    pftUnit: CurNode.ImageIndex:=3;
    pftText: CurNode.ImageIndex:=4;
    pftBinary: CurNode.ImageIndex:=5;
    else
      CurNode.ImageIndex:=-1;
    end;
    CurNode.SelectedIndex:=CurNode.ImageIndex;
    CurNode:=CurNode.GetNextSibling;
  end;
  while CurNode<>nil do begin
    NextNode:=CurNode.GetNextSibling;
    CurNode.Free;
    CurNode:=NextNode;
  end;
  FilesNode.Expanded:=true;
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.UpdateRequiredPkgs;
var
  Cnt: Integer;
  CurNode: TTreeNode;
  i: Integer;
  CurDependency: TPkgDependency;
  NextNode: TTreeNode;
begin
  Cnt:=LazPackage.RequiredPkgCount;
  FilesTreeView.BeginUpdate;
  CurNode:=RequiredPackagesNode.GetFirstChild;
  for i:=0 to Cnt-1 do begin
    if CurNode=nil then
      CurNode:=FilesTreeView.Items.AddChild(RequiredPackagesNode,'');
    CurDependency:=LazPackage.RequiredPkgs[i];
    CurNode.Text:=CurDependency.AsString;
    CurNode.ImageIndex:=RequiredPackagesNode.ImageIndex;
    CurNode.SelectedIndex:=CurNode.ImageIndex;
    CurNode:=CurNode.GetNextSibling;
  end;
  while CurNode<>nil do begin
    NextNode:=CurNode.GetNextSibling;
    CurNode.Free;
    CurNode:=NextNode;
  end;
  RequiredPackagesNode.Expanded:=true;
  FilesTreeView.EndUpdate;
end;

procedure TPackageEditorForm.UpdateSelectedFile;
var
  CurNode: TTreeNode;
  NodeIndex: Integer;
  CurFile: TPkgFile;
  i: Integer;
  CurComponent: TPkgComponent;
  CurLine: string;
  CurListIndex: Integer;
  RegCompCnt: Integer;
begin
  CurNode:=FilesTreeView.Selected;
  FilePropsGroupBox.Enabled:=(CurNode<>nil) and (CurNode.Parent=FilesNode);
  FPlugins.Clear;
  if CurNode<>nil then begin
    CallRegisterProcCheckBox.Enabled:=not LazPackage.ReadOnly;
    NodeIndex:=CurNode.Index;
    if CurNode.Parent=FilesNode then begin
      // get current package file
      CurFile:=LazPackage.Files[NodeIndex];
      // set Register Unit checkbox
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
    end else begin
      CallRegisterProcCheckBox.Checked:=false;
      RegisteredListBox.Items.Clear;
    end;
  end;
end;

procedure TPackageEditorForm.UpdateStatusBar;
var
  StatusText: String;
begin
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

procedure TPackageEditorForm.DoSave;
begin
  PackageEditors.SavePackage(LazPackage);
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
end;

destructor TPackageEditorForm.Destroy;
begin
  FreeAndNil(FPlugins);
  inherited Destroy;
end;

{ TPackageEditors }

function TPackageEditors.GetEditors(Index: integer): TPackageEditorForm;
begin
  Result:=TPackageEditorForm(FItems[Index]);
end;

constructor TPackageEditors.Create;
begin
  FItems:=TList.Create;
end;

destructor TPackageEditors.Destroy;
begin
  Clear;
  FItems.Free;
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
  if PackageGraph.OpenDependency(Dependency,
    fpfSearchPackageEverywhere,APackage)=lprSuccess then
  begin
    if Assigned(OnOpenPackage) then Result:=OnOpenPackage(Sender,APackage);
  end;
end;

function TPackageEditors.CreateNewFile(Sender: TObject;
  const Params: TAddToPkgResult): TModalResult;
begin
  Result:=mrCancel;
  if Assigned(OnCreateNewFile) then
    Result:=OnCreateNewFile(Sender,Params);
end;

procedure TPackageEditors.SavePackage(APackage: TLazPackage);
begin
  if Assigned(OnSavePackage) then OnSavePackage(APackage);
end;

initialization
  PackageEditors:=nil;

end.

