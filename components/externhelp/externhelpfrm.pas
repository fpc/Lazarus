{ Extern help options frame for Lazarus IDE.

  Copyright (C) 2010  Mattias Gaertner  mattias@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit ExternHelpFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  FileUtil, LazFileUtils, LazConfigStorage,
  // LCL
  LCLProc, LResources, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons, StdCtrls,
  ExtCtrls, ButtonPanel, HelpIntfs,
  // IdeIntf
  LazHelpIntf, PackageIntf, MacroIntf, IDEOptionsIntf, IDEOptEditorIntf,
  LazIDEIntf, BaseIDEIntf, IDEDialogs, IDEImagesIntf, SrcEditorIntf;

const
  ExternHelpConfigVersion = 1;
var
  ExternHelpOptionID: integer = 2000;
  ExternHelpOptionGeneralID: integer = 1000;

resourcestring
  ehrsGroupTitle = 'Extern help';
  ehrsName = 'Name';
  ehrsUnitFileOrUnitDirectory = 'Unit file or unit directory';
  ehrsURL = 'URL';
  ehrsHelp = 'Help';
  ehrsAddNewItem = 'Add new item';
  ehrsDeleteItem = 'Delete item';
  ehrsBrowseForPath = 'Browse for path';
  ehrsGeneral = 'General';
  ehrsBrowse = 'Browse ...';
  ehrsMacrofy = 'Use macros';
  ehrsReplaceCommonDirectoriesWithMacros = 'Replace common directories with '
    +'macros';
  ehrsEditorFile = 'Editor file ...';
  ehrsStoreThisURLIn = 'Store this URL in';
  ehrsIncludeSubDirectories = 'Include sub directories';
  ehrsSelectAFileFromTheSourceEditor = 'Select a file from the source editor';
  ehrsChooseAPascalUnit = 'Choose a pascal unit';
  ehrsSelectFile = 'Select file';
  ehrsDirectoryNotFound = 'Directory not found: %s';
  ehrsFileNotFound = 'File not found: %s';
  ehrsWarning = 'Warning';
  ehrsExternal = 'External';
  ehrsMySettings = 'My settings (default)';

type

  { TExternHelpFileSelector }

  TExternHelpFileSelector = class(TForm)
  published
    FileListBox: TListBox;
    ButtonPanel1: TButtonPanel;
    procedure ButtonPanel1OKButtonClick(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TExternHelpItem }

  TExternHelpItem = class
  private
    FChangeStep: integer;
    fChilds: TFPList;
    FFilename: string;
    FName: string;
    FStoreIn: string;
    FURL: string;
    FWithSubDirectories: boolean;
    function GetChildCount: integer;
    function GetChildren(Index: integer): TExternHelpItem;
    procedure SetFilename(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetStoreIn(const AValue: string);
    procedure SetURL(const AValue: string);
    procedure SetWithSubDirectories(const AValue: boolean);
  public
    Parent: TExternHelpItem;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddChild(Item: TExternHelpItem);
    procedure MoveChild(FromPos, ToPos: integer);
    procedure RemoveChild(Index: integer);
    procedure DeleteChild(Index: integer);
    procedure DeleteChild(Child: TExternHelpItem);
    function IndexOf(Child: TExternHelpItem): integer;
    function IsEqual(Item: TExternHelpItem; WithName: boolean): boolean;
    function IsDirectory: boolean;
    procedure Assign(Src: TExternHelpItem; WithName: boolean);
    procedure IncreaseChangeStep; virtual;
    property Name: string read FName write SetName;
    property Filename: string read FFilename write SetFilename;
    property WithSubDirectories: boolean read FWithSubDirectories write SetWithSubDirectories;
    property URL: string read FURL write SetURL;
    property StoreIn: string read FStoreIn write SetStoreIn;
    property ChildCount: integer read GetChildCount;
    property Children[Index: integer]: TExternHelpItem read GetChildren;
    property ChangeStep: integer read FChangeStep;
  end;

  TExternHelpOptions = class;

  { TExternHelpRootItem }

  TExternHelpRootItem = class(TExternHelpItem)
  public
    Owner: TExternHelpOptions;
    procedure IncreaseChangeStep; override;
  end;

  { TExternalHelpDatabase }

  TExternalHelpDatabase = class(THelpDatabase)
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function ShowHelp(Query: THelpQuery; BaseNode, NewNode: THelpNode;
                      QueryItem: THelpQueryItem;
                      var ErrMsg: string): TShowHelpResult; override;
  end;

  { TExternHelpOptions }

  TExternHelpOptions = class(TAbstractIDEEnvironmentOptions)
  private
    FChangeStep: integer;
    FFilename: string;
    FHelpDB: TExternalHelpDatabase;
    FLastSavedChangeStep: integer;
    procedure SetFilename(const AValue: string);
    procedure PkgFileLoaded(Sender: TObject);
    procedure LoadNode(Config: TConfigStorage; Path: string; Node: TExternHelpItem);
    procedure SaveNode(Config: TConfigStorage; Path: string; Node: TExternHelpItem);
  public
    RootItem: TExternHelpRootItem;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure ClearItemsStoredInUserSettings;
    procedure ClearItemsStoredInPackages(Parent: TExternHelpItem;
                                         const Name: string = '*');
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    function Load(Config: TConfigStorage; KeepPackageOpts: boolean): TModalResult; virtual;
    function Save(Config: TConfigStorage): TModalResult; virtual;
    procedure LoadOptionsFromPackage(Pkg: TIDEPackage; Parent: TExternHelpItem);
    procedure LoadOptionsFromPackages;
    procedure SaveOptionsToPackage(Pkg: TIDEPackage);
    procedure SaveOptionsToPackages;
    function LoadFromFile(Filename: string; KeepPackageOpts: boolean): TModalResult; virtual;
    function SaveToFile(Filename: string): TModalResult; virtual;
    function Load(KeepPackageOpts: boolean): TModalResult; virtual;
    function Save: TModalResult; virtual;
    function GetFullFilename: string;
    function IsEqual(Src: TExternHelpOptions): boolean;
    procedure Assign(Src: TExternHelpOptions); reintroduce;
    procedure IncreaseChangeStep;
    procedure UpdateHelpDB;
    property Filename: string read FFilename write SetFilename;
    property ChangeStep: integer read FChangeStep;
    property LastSavedChangeStep: integer read FLastSavedChangeStep;
    property HelpDB: TExternalHelpDatabase read FHelpDB;
  end;

type

  { TExternHelpGeneralOptsFrame }

  TExternHelpGeneralOptsFrame = class(TAbstractIDEOptionsEditor)
    AddSpeedButton: TSpeedButton;
    SelEditorFileButton: TButton;
    WithSubDirsCheckBox: TCheckBox;
    FileUseMacrosButton: TButton;
    DeleteSpeedButton: TSpeedButton;
    FileBrowseButton: TButton;
    FilenameEdit: TEdit;
    FilenameLabel: TLabel;
    HelpBitBtn: TBitBtn;
    ItemsTreeView: TTreeView;
    NameEdit: TEdit;
    NameLabel: TLabel;
    Splitter1: TSplitter;
    StoreComboBox: TComboBox;
    StoreLabel: TLabel;
    URLLabel: TLabel;
    URLMemo: TMemo;
    procedure AddSpeedButtonClick(Sender: TObject);
    procedure DeleteSpeedButtonClick(Sender: TObject);
    procedure FileBrowseButtonClick(Sender: TObject);
    procedure FileUseMacrosButtonClick(Sender: TObject);
    procedure FilenameEditChange(Sender: TObject);
    procedure FilenameEditEditingDone(Sender: TObject);
    procedure ItemsTreeViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ItemsTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure ItemsTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure ItemsTreeViewEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ItemsTreeViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ItemsTreeViewSelectionChanged(Sender: TObject);
    procedure ItemsTreeViewStartDrag(Sender: TObject;
      var DragObject: TDragObject);
    procedure NameEditChange(Sender: TObject);
    procedure NameEditEditingDone(Sender: TObject);
    procedure SelEditorFileButtonClick(Sender: TObject);
    procedure StoreComboBoxEditingDone(Sender: TObject);
    procedure URLMemoEditingDone(Sender: TObject);
    procedure WithSubDirsCheckBoxEditingDone(Sender: TObject);
  private
    FOptions: TExternHelpOptions;
    FDragNode: TTreeNode;
    FMySettingsCaption: string;
    procedure FillItemsTreeView;
    procedure NameChanged(TVNode: TTreeNode; var NewName: string;
      UpdateTree, UpdateEdit: boolean);
    procedure StoreInChanged(TVNode: TTreeNode; var NewStoreIn: string;
      UpdateTree, UpdateEdit: boolean);
    procedure SelectionChanged;
    function FindTVNode(NodeText: string): TTreeNode;
    function CreateUniqueName(Prefix: string): string;
    procedure FillStoreInCombobox;
    function Macrofy(Filename: string): string;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function GetTitle: String; override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    property Options: TExternHelpOptions read FOptions;
  end;

var
  ExternHelpOptions: TExternHelpOptions = nil;

procedure Register;

implementation

{$R *.lfm}

procedure Register;
begin
  ExternHelpOptions:=TExternHelpOptions.Create;
  ExternHelpOptionID:=GroupHelp;
  ExternHelpOptionGeneralID:=RegisterIDEOptionsEditor(ExternHelpOptionID,
      TExternHelpGeneralOptsFrame,ExternHelpOptionGeneralID)^.Index;
  try
    ExternHelpOptions.Load(false);
    ExternHelpOptions.LoadOptionsFromPackages;
  except
    on E: Exception do begin
      DebugLn(['Error reading externhelp options ',ExternHelpOptions.Filename,': ',E.Message]);
    end;
  end;
  ExternHelpOptions.UpdateHelpDB;
end;

{ TExternHelpOptions }

procedure TExternHelpOptions.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

procedure TExternHelpOptions.PkgFileLoaded(Sender: TObject);
begin
  if Sender is TIDEPackage then
    LoadOptionsFromPackage(TIDEPackage(Sender),RootItem);
end;

procedure TExternHelpOptions.LoadNode(Config: TConfigStorage; Path: string;
  Node: TExternHelpItem);
var
  i, NewCount: Integer;
  NewItem: TExternHelpItem;
begin
  Node.Name:=Config.GetValue(Path+'Name','');
  Node.Filename:=Config.GetValue(Path+'Filename/Value','');
  Node.WithSubDirectories:=Config.GetValue(Path+'WithSubDirectories/Value',false);
  Node.URL:=Config.GetValue(Path+'URL/Value','');
  NewCount:=Config.GetValue(Path+'ChildCount',0);
  for i:=1 to NewCount do begin
    NewItem:=TExternHelpItem.Create;
    NewItem.StoreIn:=Node.StoreIn;
    Node.AddChild(NewItem);
    LoadNode(Config,Path+'Item'+IntToStr(i)+'/',NewItem);
  end;
end;

procedure TExternHelpOptions.SaveNode(Config: TConfigStorage; Path: string;
  Node: TExternHelpItem);
var
  i: Integer;
begin
  Config.SetDeleteValue(Path+'Name',Node.Name,'');
  Config.SetDeleteValue(Path+'Filename/Value',Node.Filename,'');
  Config.SetDeleteValue(Path+'WithSubDirectories/Value',Node.WithSubDirectories,false);
  Config.SetDeleteValue(Path+'URL/Value',Node.URL,'');
  Config.SetDeleteValue(Path+'ChildCount',Node.ChildCount,0);
  for i:=1 to Node.ChildCount do
    SaveNode(Config,Path+'Item'+IntToStr(i)+'/',Node.Children[i-1]);
end;

constructor TExternHelpOptions.Create;
var
  OldHelpDB: THelpDatabase;
begin
  RootItem:=TExternHelpRootItem.Create;
  RootItem.Owner:=Self;
  Filename:='externhelp.xml';
  OldHelpDB:=HelpDatabases.FindDatabase('External help');
  if OldHelpDB is TExternalHelpDatabase then
    FHelpDB:=TExternalHelpDatabase(OldHelpDB)
  else
    FHelpDB:=TExternalHelpDatabase(HelpDatabases.CreateHelpDatabase('External help',
                                               TExternalHelpDatabase,true));
  PackageEditingInterface.AddHandlerOnPackageFileLoaded(@PkgFileLoaded);
end;

destructor TExternHelpOptions.Destroy;
begin
  FreeAndNil(RootItem);
  // FHelpDB is freed by the IDE
  inherited Destroy;
end;

procedure TExternHelpOptions.Clear;
begin
  RootItem.Clear;
end;

procedure TExternHelpOptions.ClearItemsStoredInUserSettings;
var
  i: Integer;
  Item: TExternHelpItem;
begin
  if RootItem<>nil then begin
    for i:=RootItem.ChildCount-1 downto 0 do begin
      Item:=RootItem.Children[i];
      if (Item.StoreIn='') then
        Item.Free;
    end;
  end;
end;

procedure TExternHelpOptions.ClearItemsStoredInPackages(Parent: TExternHelpItem;
  const Name: string);
var
  i: Integer;
  Item: TExternHelpItem;
begin
  if Parent<>nil then begin
    for i:=Parent.ChildCount-1 downto 0 do begin
      Item:=Parent.Children[i];
      if (Item.StoreIn<>'')
      and (Name='*') or (SysUtils.CompareText(Item.StoreIn,Name)=0) then
        Item.Free;
    end;
  end;
end;

procedure TExternHelpOptions.LoadOptionsFromPackage(Pkg: TIDEPackage;
  Parent: TExternHelpItem);
var
  Cnt: integer;
  i: Integer;
  Path: String;
  NewItem: TExternHelpItem;
begin
  if Pkg.Name='' then exit;
  ClearItemsStoredInPackages(Parent,Pkg.Name);
  Path:='ExternHelp/';
  Cnt:=Pkg.CustomOptions.GetValue(Path+'Count',0);
  //DebugLn(['TExternHelpOptions.LoadOptionsFromPackage ',Pkg.Name,' Cnt=',Cnt]);
  for i:=1 to Cnt do begin
    NewItem:=TExternHelpItem.Create;
    Parent.AddChild(NewItem);
    NewItem.StoreIn:=Pkg.Name;
    LoadNode(Pkg.CustomOptions,Path+'Item'+IntToStr(i)+'/',NewItem);
  end;
end;

procedure TExternHelpOptions.LoadOptionsFromPackages;
var
  i: Integer;
begin
  if PackageEditingInterface=nil then exit;
  ClearItemsStoredInPackages(RootItem,'*');
  for i:=0 to PackageEditingInterface.GetPackageCount-1 do
    LoadOptionsFromPackage(PackageEditingInterface.GetPackages(i),RootItem);
end;

procedure TExternHelpOptions.SaveOptionsToPackage(Pkg: TIDEPackage);
var
  Cnt: Integer;
  Path: String;
  Item: TExternHelpItem;
  i: Integer;
  TmpRoot: TExternHelpItem;
  Changed: Boolean;
begin
  if Pkg.Name='' then exit;
  //DebugLn(['TExternHelpOptions.SaveOptionsToPackage START ',Pkg.Name]);

  // check if something changed
  TmpRoot:=TExternHelpItem.Create;
  try
    LoadOptionsFromPackage(Pkg,TmpRoot);
    Changed:=false;
    Cnt:=0;
    for i:=0 to RootItem.ChildCount-1 do begin
      Item:=RootItem.Children[i];
      if SysUtils.CompareText(Item.StoreIn,Pkg.Name)<>0 then continue;
      if (Cnt=TmpRoot.ChildCount)
      or (not TmpRoot.Children[Cnt].IsEqual(Item,true)) then begin
        Changed:=true;
        break;
      end;
      inc(Cnt);
    end;
    if TmpRoot.ChildCount>Cnt then Changed:=true;
    if not Changed then exit;
  finally
    TmpRoot.Free;
  end;
  DebugLn(['TExternHelpOptions.SaveOptionsToPackage CHANGED: ',Pkg.Name]);

  // save to package and mark it modified
  Path:='ExternHelp';
  Pkg.CustomOptions.DeletePath(Path);
  Path:=Path+'/';
  Cnt:=0;
  for i:=0 to RootItem.ChildCount-1 do begin
    Item:=RootItem.Children[i];
    //DebugLn(['TExternHelpOptions.SaveOptionsToPackage ',Item.Name,' StoreIN=',Item.StoreIn,' ',SysUtils.CompareText(Item.StoreIn,Pkg.Name)=0]);
    if SysUtils.CompareText(Item.StoreIn,Pkg.Name)<>0 then continue;
    inc(Cnt);
    SaveNode(Pkg.CustomOptions,Path+'Item'+IntToStr(Cnt)+'/',Item);
  end;
  Pkg.CustomOptions.SetDeleteValue(Path+'Count',Cnt,0);
  Pkg.Modified:=True;
  //DebugLn(['TExternHelpOptions.SaveOptionsToPackage MODIFIED: ',Pkg.Name]);
end;

procedure TExternHelpOptions.SaveOptionsToPackages;
var
  i: Integer;
  Pkg: TIDEPackage;
begin
  if PackageEditingInterface=nil then exit;
  for i:=0 to PackageEditingInterface.GetPackageCount-1 do begin
    Pkg:=PackageEditingInterface.GetPackages(i);
    if Pkg.ReadOnly then continue;
    SaveOptionsToPackage(Pkg);
  end;
end;

class function TExternHelpOptions.GetGroupCaption: string;
begin
  Result:=ehrsGroupTitle;
end;

class function TExternHelpOptions.GetInstance: TAbstractIDEOptions;
begin
  Result:=ExternHelpOptions;
end;

function TExternHelpOptions.Load(Config: TConfigStorage;
  KeepPackageOpts: boolean): TModalResult;
var
  Path: String;
begin
  Result:=mrOk;
  if KeepPackageOpts then
    ClearItemsStoredInUserSettings
  else
    Clear;
  Path:='ExternHelp/';
  LoadNode(Config,Path+'Items/',RootItem);
end;

function TExternHelpOptions.Save(Config: TConfigStorage): TModalResult;
var
  Path: String;
  Item: TExternHelpItem;
  i: Integer;
  Cnt: Integer;
begin
  Result:=mrOk;
  Path:='ExternHelp/';
  Cnt:=0;
  for i:=1 to RootItem.ChildCount do begin
    Item:=RootItem.Children[i-1];
    if Item.StoreIn<>'' then continue;
    inc(Cnt);
    SaveNode(Config,Path+'Items/Item'+IntToStr(Cnt)+'/',Item);
  end;
  Config.SetDeleteValue(Path+'Items/ChildCount',Cnt,0);
end;

function TExternHelpOptions.LoadFromFile(Filename: string;
  KeepPackageOpts: boolean): TModalResult;
var
  Config: TConfigStorage;
begin
  Config:=GetIDEConfigStorage(Filename,true);
  try
    Result:=Load(Config,KeepPackageOpts);
  finally
    Config.Free;
  end;
end;

function TExternHelpOptions.SaveToFile(Filename: string): TModalResult;
var
  Config: TConfigStorage;
begin
  Config:=GetIDEConfigStorage(Filename,false);
  try
    Result:=Save(Config);
    //DebugLn(['TExternHelpOptions.SaveToFile ',Filename,' ',Config.GetFilename]);
    Config.WriteToDisk;
    //DebugLn(['TExternHelpOptions.SaveToFile exists=',FileExistsUTF8(GetFullFilename)]);
  finally
    Config.Free;
  end;
end;

function TExternHelpOptions.Load(KeepPackageOpts: boolean): TModalResult;
begin
  Result:=LoadFromFile(Filename,KeepPackageOpts);
  FLastSavedChangeStep:=ChangeStep;
end;

function TExternHelpOptions.Save: TModalResult;
var
  FullFilename: String;
begin
  FullFilename:=GetFullFilename;
  if FileExistsUTF8(FullFilename)
  and (FLastSavedChangeStep=ChangeStep) then
    Result:=mrOK;
  Result:=SaveToFile(Filename);
  FLastSavedChangeStep:=ChangeStep;
end;

function TExternHelpOptions.GetFullFilename: string;
begin
  Result:=Filename;
  if FilenameIsAbsolute(Result) then exit;
  Result:=AppendPathDelim(LazarusIDE.GetPrimaryConfigPath)+Result;
end;

function TExternHelpOptions.IsEqual(Src: TExternHelpOptions): boolean;
begin
  Result:=RootItem.IsEqual(Src.RootItem,false);
end;

procedure TExternHelpOptions.Assign(Src: TExternHelpOptions);
begin
  RootItem.Assign(Src.RootItem,false);
end;

procedure TExternHelpOptions.IncreaseChangeStep;
begin
  if FChangeStep=high(FChangeStep) then
    FChangeStep:=low(FChangeStep)
  else
    inc(FChangeStep);
end;

procedure TExternHelpOptions.UpdateHelpDB;

  procedure RegisterItem(Item: TExternHelpItem);
  var
    i: Integer;
    HelpNode: THelpNode;
    ItemFilename: String;
    SrcFilter: THelpDBISourceFile;
  begin
    if (Item.Filename<>'') and (Item.URL<>'') then begin
      ItemFilename:=GetForcedPathDelims(Item.Filename);
      // create a help node for this topic
      HelpNode:=THelpNode.CreateURL(HelpDB,Item.Name,Item.URL);
      // create a filter for the source file(s)
      if Item.IsDirectory then
        SrcFilter:=THelpDBISourceDirectory.Create(HelpNode,
                              ItemFilename,'*.pp;*.pas',Item.WithSubDirectories)
      else
        SrcFilter:=THelpDBISourceFile.Create(HelpNode,ItemFilename);
      HelpDB.RegisterItem(SrcFilter);
    end;

    for i:=0 to Item.ChildCount-1 do
      RegisterItem(Item.Children[i]);
  end;

begin
  HelpDB.UnregisterAllItems;
  RegisterItem(RootItem);
end;

{ TExternHelpGeneralOptsFrame }

procedure TExternHelpGeneralOptsFrame.ItemsTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  NameChanged(Node,S,false,true);
end;

procedure TExternHelpGeneralOptsFrame.FilenameEditEditingDone(Sender: TObject);
var
  s: String;
  TVNode: TTreeNode;
  Item: TExternHelpItem;
  Msg: String;
  Filename: String;
begin
  TVNode:=ItemsTreeView.Selected;
  if (TVNode=nil) or (not (TObject(TVNode.Data) is TExternHelpItem)) then exit;
  Item:=TExternHelpItem(TVNode.Data);
  s:=FilenameEdit.Text;
  s:=TrimFilename(s);
  if s<>Item.Filename then begin
    Filename:=s;
    if s='' then begin
      // ok, allow simple deactivate
    end else begin
      ForcePathDelims(Filename);
      IDEMacros.SubstituteMacros(Filename);
      Msg:='';
      if (Filename<>'') and (Filename[length(Filename)]=PathDelim) then begin
        if not DirPathExists(Filename) then
          Msg:=Format(ehrsDirectoryNotFound, [Filename]);
      end else begin
        if not FileExistsUTF8(Filename) then
          Msg:=Format(ehrsFileNotFound, [Filename]);
      end;
      if Msg<>'' then begin
        MessageDlg(ehrsWarning, Msg, mtWarning, [mbIgnore], 0);
      end;
    end;
    Item.Filename:=s;
    if not Item.IsDirectory then
      Item.WithSubDirectories:=false;
    WithSubDirsCheckBox.Enabled:=Item.IsDirectory;
  end;
end;

procedure TExternHelpGeneralOptsFrame.ItemsTreeViewDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  TVNode: TTreeNode;
  InsertType: TTreeViewInsertMarkType;
begin
  if State=dsDragEnter then ;
  if (Source<>ItemsTreeView) or (FDragNode=nil) then begin
    Accept:=false;
    exit;
  end;
  ItemsTreeView.GetInsertMarkAt(X,Y,TVNode,InsertType);
  if (TVNode<>nil) then
    if (TVNode=FDragNode) or TVNode.HasAsParent(FDragNode)
    or FDragNode.HasAsParent(TVNode) then begin
      Accept:=false;
      exit;
    end;
end;

procedure TExternHelpGeneralOptsFrame.AddSpeedButtonClick(Sender: TObject);
var
  SelTVNode: TTreeNode;
  Item: TExternHelpItem;
  TVNode: TTreeNode;
  SelItem: TExternHelpItem;
begin
  SelTVNode:=ItemsTreeView.Selected;
  Item:=TExternHelpItem.Create;
  Item.Name:=CreateUniqueName('Item');
  if (SelTVNode<>nil) and (TObject(SelTVNode.Data) is TExternHelpItem) then
  begin
    // init with values of selected node
    SelItem:=TExternHelpItem(SelTVNode.Data);
    Item.Filename:=SelItem.Filename;
    Item.URL:=SelItem.URL;
    Item.StoreIn:=SelItem.StoreIn;
    SelItem.Parent.AddChild(Item);
  end else
    Options.RootItem.AddChild(Item);
  TVNode:=ItemsTreeView.Items.AddObject(SelTVNode,Item.Name,Item);
  ItemsTreeView.Selected:=TVNode;
end;

procedure TExternHelpGeneralOptsFrame.DeleteSpeedButtonClick(Sender: TObject);
var
  SelTVNode: TTreeNode;
  Item: TExternHelpItem;
begin
  SelTVNode:=ItemsTreeView.Selected;
  if (SelTVNode=nil) or (not (TObject(SelTVNode.Data) is TExternHelpItem)) then exit;
  Item:=TExternHelpItem(SelTVNode.Data);
  // select next
  if SelTVNode.GetNext<>nil then
    ItemsTreeView.Selected:=SelTVNode.GetNext
  else
    ItemsTreeView.Selected:=SelTVNode.GetPrev;
  // delete in treeview
  SelTVNode.Free;
  // delete in Options
  Item.Free;
end;

procedure TExternHelpGeneralOptsFrame.FileBrowseButtonClick(Sender: TObject);
var
  OpenDialog: TOpenDialog;
  Filename: String;
  SelTVNode: TTreeNode;
  Item: TExternHelpItem;
begin
  OpenDialog:=TOpenDialog.Create(nil);
  try
    InitIDEFileDialog(OpenDialog);
    OpenDialog.Title:=ehrsChooseAPascalUnit;
    if not OpenDialog.Execute then exit;
    Filename:=Macrofy(TrimFilename(OpenDialog.FileName));
    FilenameEdit.Text:=Filename;
    SelTVNode:=ItemsTreeView.Selected;
    if (SelTVNode<>nil) and (TObject(SelTVNode.Data) is TExternHelpItem) then
    begin
      Item:=TExternHelpItem(SelTVNode.Data);
      Item.Filename:=Filename;
    end;
  finally
    OpenDialog.Free;
  end;
end;

procedure TExternHelpGeneralOptsFrame.FileUseMacrosButtonClick(Sender: TObject);
var
  Filename: String;
begin
  Filename:=TrimFilename(FilenameEdit.Text);
  FilenameEdit.Text:=Macrofy(Filename);
end;

procedure TExternHelpGeneralOptsFrame.FilenameEditChange(Sender: TObject);
var
  s: String;
  TVNode: TTreeNode;
begin
  TVNode:=ItemsTreeView.Selected;
  if (TVNode=nil) or (not (TObject(TVNode.Data) is TExternHelpItem)) then exit;
  s:=FilenameEdit.Text;
  s:=TrimFilename(s);
  WithSubDirsCheckBox.Enabled:=(s<>'') and (s[length(s)] in ['/','\']);
end;

procedure TExternHelpGeneralOptsFrame.ItemsTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin
  if AllowEdit or (Node=nil) then ;
end;

procedure TExternHelpGeneralOptsFrame.ItemsTreeViewEndDrag(Sender,
  Target: TObject; X, Y: Integer);
begin
  FDragNode:=nil;
  if (Target=nil) or (X=0) or (Y=0) then ;
  ItemsTreeView.Options:=ItemsTreeView.Options-[tvoAutoInsertMark];
end;

procedure TExternHelpGeneralOptsFrame.ItemsTreeViewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  TVNode: TTreeNode;
begin
  if not (ssLeft in Shift) then begin
    // no left mouse button => stop showing insert mark
    //if tvoAutoInsertMark in ItemsTreeView.Options then DebugLn(['TExternHelpGeneralOptsFrame.ItemsTreeViewMouseMove no left']);
    ItemsTreeView.Options:=ItemsTreeView.Options-[tvoAutoInsertMark];
    FDragNode:=nil;
  end else if (not ItemsTreeView.Dragging) then begin
    // left mouse button is presses and not yet dragging => start dragging
    TVNode:=ItemsTreeView.GetNodeAt(X,Y);
    if (TVNode<>nil) then begin
      // a node to drag => start dragging
      FDragNode:=TVNode;
      ItemsTreeView.BeginDrag(true);
      ItemsTreeView.Options:=ItemsTreeView.Options-[tvoAutoInsertMark];
      //DebugLn(['TExternHelpGeneralOptsFrame.ItemsTreeViewMouseMove START']);
    end else begin
      // no node to drag
      ItemsTreeView.Options:=ItemsTreeView.Options-[tvoAutoInsertMark];
      FDragNode:=nil;
      //DebugLn(['TExternHelpGeneralOptsFrame.ItemsTreeViewMouseMove no node']);
    end;
  end;
end;

procedure TExternHelpGeneralOptsFrame.ItemsTreeViewSelectionChanged(
  Sender: TObject);
begin
  SelectionChanged;
end;

procedure TExternHelpGeneralOptsFrame.ItemsTreeViewStartDrag(Sender: TObject;
  var DragObject: TDragObject);
begin
  if DragObject=nil then ;
  ItemsTreeView.Options:=ItemsTreeView.Options+[tvoAutoInsertMark];
end;

procedure TExternHelpGeneralOptsFrame.NameEditChange(Sender: TObject);
var
  S: String;
begin
  S:=NameEdit.Text;
  NameChanged(ItemsTreeView.Selected,S,true,false);
end;

procedure TExternHelpGeneralOptsFrame.NameEditEditingDone(Sender: TObject);
var
  S: String;
begin
  S:=NameEdit.Text;
  NameChanged(ItemsTreeView.Selected,S,true,true);
end;

procedure TExternHelpGeneralOptsFrame.SelEditorFileButtonClick(Sender: TObject);
var
  Selector: TExternHelpFileSelector;
  i: Integer;
  Filename: String;
  Files: TStringList;
  j: Integer;
  Filename2: String;
  SelTVNode: TTreeNode;
  Item: TExternHelpItem;
begin
  if SourceEditorManagerIntf=nil then exit;
  Files:=TStringList.Create;
  Selector:=TExternHelpFileSelector.Create(GetParentForm(Self));
  try
    // collect open editor files
    for i:=0 to SourceEditorManagerIntf.SourceEditorCount-1 do begin
      Filename:=SourceEditorManagerIntf.SourceEditors[i].FileName;
      j:=Files.Count-1;
      while (j>=0) and (CompareFilenames(Files[j],Filename)<>0) do dec(j);
      if j>=0 then continue;
      Files.Add(Filename);
    end;
    // shorten file names
    for i:=0 to Files.Count-1 do begin
      Filename:=ExtractFileName(Files[i]);
      j:=Files.Count-1;
      while (j>=0)
      and ((i=j) or (CompareFilenames(ExtractFileName(Files[j]),Filename)<>0)) do
        dec(j);
      if (j<0) then begin
        // short file is unique => use short file
        Files[i]:=Filename;
      end;
    end;
    Selector.FileListBox.Items:=Files;
    Selector.Position:=poOwnerFormCenter;
    Selector.Caption:=ehrsSelectFile;
    if Files.Count>0 then
      Selector.FileListBox.ItemIndex:=0;
    if (Selector.ShowModal=mrOK) then begin
      i:=Selector.FileListBox.ItemIndex;
      if i>=0 then begin
        Filename2:=Selector.FileListBox.Items[i];
        for i:=0 to SourceEditorManagerIntf.SourceEditorCount-1 do begin
          Filename:=SourceEditorManagerIntf.SourceEditors[i].FileName;
          if (CompareFilenames(Filename2,Filename)=0)
          or (CompareFilenames(Filename2,ExtractFileName(Filename))=0) then
          begin
            Filename:=Macrofy(FileName);
            FilenameEdit.Text:=Filename;
            SelTVNode:=ItemsTreeView.Selected;
            if (SelTVNode<>nil) and (TObject(SelTVNode.Data) is TExternHelpItem)
            then begin
              Item:=TExternHelpItem(SelTVNode.Data);
              Item.Filename:=Filename;
            end;
            exit;
          end;
        end;
      end;
    end;
  finally
    Selector.Free;
    Files.Free;
  end;
end;

procedure TExternHelpGeneralOptsFrame.StoreComboBoxEditingDone(Sender: TObject);
var
  S: String;
begin
  S:=StoreComboBox.Text;
  StoreInChanged(ItemsTreeView.Selected,S,true,true);
end;

procedure TExternHelpGeneralOptsFrame.URLMemoEditingDone(Sender: TObject);
var
  s: String;
  TVNode: TTreeNode;
  Item: TExternHelpItem;
begin
  URLMemo.Lines.Delimiter:=' ';
  s:=URLMemo.Lines.DelimitedText;
  TVNode:=ItemsTreeView.Selected;
  if (TVNode=nil) or (not (TObject(TVNode.Data) is TExternHelpItem)) then exit;
  Item:=TExternHelpItem(TVNode.Data);
  s:=Trim(s);
  if s<>Item.URL then begin
    Item.URL:=s;
  end;
end;

procedure TExternHelpGeneralOptsFrame.WithSubDirsCheckBoxEditingDone(
  Sender: TObject);
var
  TVNode: TTreeNode;
  Item: TExternHelpItem;
begin
  TVNode:=ItemsTreeView.Selected;
  if (TVNode=nil) or (not (TObject(TVNode.Data) is TExternHelpItem)) then exit;
  Item:=TExternHelpItem(TVNode.Data);
  Item.WithSubDirectories:=WithSubDirsCheckBox.Checked;
end;

procedure TExternHelpGeneralOptsFrame.FillItemsTreeView;

  procedure Add(ParentItem: TExternHelpItem; ParentTVNode: TTreeNode);
  var
    i: Integer;
    Item: TExternHelpItem;
    TVNode: TTreeNode;
  begin
    for i:=0 to ParentItem.ChildCount-1 do begin
      Item:=ParentItem.Children[i];
      TVNode:=ItemsTreeView.Items.AddChildObject(ParentTVNode,Item.Name,Item);
      Add(Item,TVNode);
      TVNode.Expanded:=true;
    end;
  end;

begin
  ItemsTreeView.BeginUpdate;
  ItemsTreeView.Items.Clear;
  Add(Options.RootItem,nil);
  ItemsTreeView.EndUpdate;
end;

procedure TExternHelpGeneralOptsFrame.NameChanged(TVNode: TTreeNode;
  var NewName: string; UpdateTree, UpdateEdit: boolean);
var
  Item: TExternHelpItem;
begin
  NewName:=Trim(NewName);
  if (TVNode<>nil) and (TObject(TVNode.Data) is TExternHelpItem) then begin
    Item:=TExternHelpItem(TVNode.Data);
    Item.Name:=NewName;
    if UpdateTree then
      TVNode.Text:=NewName;
    if UpdateEdit then
      NameEdit.Text:=NewName;
  end;
end;

procedure TExternHelpGeneralOptsFrame.StoreInChanged(TVNode: TTreeNode; var
  NewStoreIn: string; UpdateTree, UpdateEdit: boolean);
var
  Item: TExternHelpItem;
begin
  NewStoreIn:=Trim(NewStoreIn);
  if NewStoreIn=FMySettingsCaption then
    NewStoreIn:='';
  if (TVNode<>nil) and (TObject(TVNode.Data) is TExternHelpItem) then begin
    Item:=TExternHelpItem(TVNode.Data);
    Item.StoreIn:=NewStoreIn;
    if UpdateTree then
      ;
    if UpdateEdit then
      StoreComboBox.Text:=NewStoreIn;
  end;
end;

procedure TExternHelpGeneralOptsFrame.SelectionChanged;
var
  TVNode: TTreeNode;
  Item: TExternHelpItem;
  s: String;
  ItemFilename: String;
begin
  TVNode:=ItemsTreeView.Selected;
  Item:=nil;
  if (TVNode<>nil) and (TObject(TVNode.Data) is TExternHelpItem) then
    Item:=TExternHelpItem(TVNode.Data);
  DisableAlign;
  try
    if Item<>nil then begin
      NameEdit.Enabled:=true;
      NameEdit.Text:=Item.Name;
      FilenameEdit.Enabled:=true;
      ItemFilename:=GetForcedPathDelims(Item.Filename);
      FilenameEdit.Text:=ItemFilename;
      WithSubDirsCheckBox.Enabled:=Item.IsDirectory;
      WithSubDirsCheckBox.Checked:=Item.WithSubDirectories;
      URLMemo.Enabled:=true;
      URLMemo.Lines.Text:=Item.URL;
      StoreComboBox.Enabled:=Item.Parent=Options.RootItem;
      s:=Item.StoreIn;
      if s='' then s:=FMySettingsCaption;
      StoreComboBox.Text:=s;
    end else begin
      NameEdit.Enabled:=false;
      NameEdit.Text:='';
      FilenameEdit.Enabled:=false;
      FilenameEdit.Text:='';
      WithSubDirsCheckBox.Enabled:=false;
      WithSubDirsCheckBox.Checked:=false;
      URLMemo.Enabled:=false;
      URLMemo.Lines.Text:='';
      StoreComboBox.Enabled:=false;
      StoreComboBox.Text:='';
    end;
  finally
    EnableAlign;
  end;
end;

function TExternHelpGeneralOptsFrame.FindTVNode(NodeText: string): TTreeNode;
begin
  Result:=ItemsTreeView.Items.GetFirstNode;
  while (Result<>nil) and (SysUtils.CompareText(Result.Text,NodeText)<>0) do
    Result:=Result.GetNext;
end;

function TExternHelpGeneralOptsFrame.CreateUniqueName(Prefix: string): string;
var
  i: Integer;
begin
  i:=0;
  repeat
    inc(i);
    Result:=Prefix+IntToStr(i);
  until FindTVNode(Result)=nil;
end;

procedure TExternHelpGeneralOptsFrame.FillStoreInCombobox;
var
  sl: TStringList;
  i: Integer;
begin
  sl:=TStringList.Create;
  try
    for i:=0 to PackageEditingInterface.GetPackageCount-1 do
      sl.Add(PackageEditingInterface.GetPackages(i).Name);
    sl.Sort;
    sl.Insert(0, Trim(FMySettingsCaption));
    StoreComboBox.Items.Assign(sl);
  finally
    sl.Free;
  end;
end;

function TExternHelpGeneralOptsFrame.Macrofy(Filename: string): string;

  procedure CheckPath(PathMacro: string; var s: string);
  var
    Value: String;
  begin
    Value:=PathMacro;
    if (not IDEMacros.SubstituteMacros(Value)) or (Value='')
    or (Value[1]='$') then
      exit;
    Value:=ChompPathDelim(Value);
    if (CompareFilenames(copy(s,1,length(Value)),Value)=0)
    and ((length(s)<=length(Value)) or (s[length(Value)+1]=PathDelim)) then
    begin
      // filename is a file in the macro path
      s:=PathMacro+copy(s,length(Value)+1,length(s));
      exit;
    end;
  end;

begin
  Result:=Filename;
  CheckPath('$(FPCSrcDir)',Result);
  CheckPath('$(LazarusDir)',Result);
end;

constructor TExternHelpGeneralOptsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FOptions:=TExternHelpOptions.Create;
  IDEImages.AssignImage(AddSpeedButton, 'laz_add');
  IDEImages.AssignImage(DeleteSpeedButton, 'laz_delete');
end;

destructor TExternHelpGeneralOptsFrame.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

function TExternHelpGeneralOptsFrame.GetTitle: String;
begin
  Result:=ehrsExternal;
end;

procedure TExternHelpGeneralOptsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is TAbstractIDEHelpOptions) then exit;
  Options.Assign(ExternHelpOptions);
  FillStoreInCombobox;
  FillItemsTreeView;
  ItemsTreeView.Selected:=ItemsTreeView.Items.GetFirstNode;
  SelectionChanged;
end;

procedure TExternHelpGeneralOptsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  if ADialog=nil then ;
  NameLabel.Caption:=ehrsName;
  FilenameLabel.Caption:=ehrsUnitFileOrUnitDirectory;
  URLLabel.Caption:=ehrsURL;
  HelpBitBtn.Caption:=ehrsHelp;
  AddSpeedButton.Hint:=ehrsAddNewItem;
  DeleteSpeedButton.Hint:=ehrsDeleteItem;
  FileBrowseButton.Caption:=ehrsBrowse;
  FileBrowseButton.Hint:=ehrsBrowseForPath;
  FileUseMacrosButton.Caption:=ehrsMacrofy;
  FileUseMacrosButton.Hint:=ehrsReplaceCommonDirectoriesWithMacros;
  SelEditorFileButton.Caption:=ehrsEditorFile;
  SelEditorFileButton.Hint:=ehrsSelectAFileFromTheSourceEditor;
  WithSubDirsCheckBox.Caption:=ehrsIncludeSubDirectories;
  StoreLabel.Caption:=ehrsStoreThisURLIn;
  FMySettingsCaption:=ehrsMySettings;
end;

class function TExternHelpGeneralOptsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TAbstractIDEHelpOptions;
end;

procedure TExternHelpGeneralOptsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  if not (AOptions is TAbstractIDEHelpOptions) then exit;
  if ExternHelpOptions.IsEqual(Options) then exit;
  ExternHelpOptions.Assign(Options);
  try
    ExternHelpOptions.Save;
  except
    on E: Exception do begin
      DebugLn(['TExternHelpGeneralOptsFrame.WriteSettings unable to write file ',ExternHelpOptions.Filename,': ',E.Message]);
    end;
  end;
  ExternHelpOptions.SaveOptionsToPackages;
  ExternHelpOptions.UpdateHelpDB;
end;

{ TExternHelpItem }

function TExternHelpItem.GetChildCount: integer;
begin
  Result:=fChilds.Count;
end;

function TExternHelpItem.GetChildren(Index: integer): TExternHelpItem;
begin
  Result:=TExternHelpItem(fChilds[Index]);
end;

procedure TExternHelpItem.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
  IncreaseChangeStep;
end;

procedure TExternHelpItem.SetName(const AValue: string);
begin
  if FName=AValue then exit;
  FName:=AValue;
  IncreaseChangeStep;
end;

procedure TExternHelpItem.SetStoreIn(const AValue: string);
begin
  if FStoreIn=AValue then exit;
  FStoreIn:=AValue;
  IncreaseChangeStep;
end;

procedure TExternHelpItem.SetURL(const AValue: string);
begin
  if FURL=AValue then exit;
  FURL:=AValue;
  IncreaseChangeStep;
end;

procedure TExternHelpItem.SetWithSubDirectories(const AValue: boolean);
begin
  if FWithSubDirectories=AValue then exit;
  FWithSubDirectories:=AValue;
  IncreaseChangeStep;
end;

constructor TExternHelpItem.Create;
begin
  fChilds:=TFPList.Create;
end;

destructor TExternHelpItem.Destroy;
begin
  if Parent<>nil then
    Parent.RemoveChild(Parent.IndexOf(Self));
  Clear;
  FreeAndNil(fChilds);
  inherited Destroy;
end;

procedure TExternHelpItem.Clear;
var
  i: Integer;
  Child: TExternHelpItem;
begin
  if (ChildCount=0) and (URL='') and (Filename='') and (StoreIn='') then exit;
  for i:=fChilds.Count-1 downto 0 do begin
    Child:=Children[i];
    Child.Parent:=nil;
    Child.Free;
  end;
  fChilds.Clear;
  fURL:='';
  FFilename:='';
  FWithSubDirectories:=false;
  FStoreIn:='';
  IncreaseChangeStep;
end;

procedure TExternHelpItem.AddChild(Item: TExternHelpItem);
begin
  Item.Parent:=Self;
  fChilds.Add(Item);
  IncreaseChangeStep;
end;

procedure TExternHelpItem.MoveChild(FromPos, ToPos: integer);
begin
  if FromPos=ToPos then exit;
  fChilds.Move(FromPos,ToPos);
  IncreaseChangeStep;
end;

procedure TExternHelpItem.RemoveChild(Index: integer);
begin
  Children[Index].Parent:=nil;
  fChilds.Delete(Index);
  IncreaseChangeStep;
end;

procedure TExternHelpItem.DeleteChild(Index: integer);
begin
  Children[Index].Free;
end;

procedure TExternHelpItem.DeleteChild(Child: TExternHelpItem);
begin
  Child.Free;
end;

function TExternHelpItem.IndexOf(Child: TExternHelpItem): integer;
begin
  Result:=fChilds.IndexOf(Child);
end;

function TExternHelpItem.IsEqual(Item: TExternHelpItem; WithName: boolean
  ): boolean;
var
  i: Integer;
begin
  Result:=((not WithName) or (Name=Item.Name))
    and (Filename=Item.Filename)
    and (WithSubDirectories=Item.WithSubDirectories)
    and (URL=Item.URL)
    and (StoreIn=Item.StoreIn)
    and (ChildCount=Item.ChildCount);
  if Result then begin
    for i:=0 to ChildCount-1 do
      if not (Children[i].IsEqual(Item.Children[i],true)) then exit(false);
  end;
end;

function TExternHelpItem.IsDirectory: boolean;
begin
  Result:=(Filename<>'') and (Filename[length(Filename)] in ['/','\']);
end;

procedure TExternHelpItem.Assign(Src: TExternHelpItem; WithName: boolean);
var
  i: Integer;
  Item: TExternHelpItem;
begin
  if WithName then Name:=Src.Name;
  Filename:=Src.Filename;
  WithSubDirectories:=Src.WithSubDirectories;
  URL:=Src.URL;
  StoreIn:=Src.StoreIn;
  for i:=0 to Src.ChildCount-1 do begin
    if ChildCount<=i then begin
      Item:=TExternHelpItem.Create;
      AddChild(Item);
      IncreaseChangeStep;
    end else begin
      Item:=Children[i];
    end;
    Item.Assign(Src.Children[i],true);
  end;
  while ChildCount>Src.ChildCount do begin
    DeleteChild(ChildCount-1);
    IncreaseChangeStep;
  end;
end;

procedure TExternHelpItem.IncreaseChangeStep;
begin
  if Parent<>nil then Parent.IncreaseChangeStep;
  if FChangeStep=High(FChangeStep) then
    FChangeStep:=low(FChangeStep)
  else
    inc(FChangeStep);
end;

{ TExternHelpRootItem }

procedure TExternHelpRootItem.IncreaseChangeStep;
begin
  inherited IncreaseChangeStep;
  Owner.IncreaseChangeStep;
end;

{ TExternalHelpDatabase }

constructor TExternalHelpDatabase.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TExternalHelpDatabase.Destroy;
begin
  inherited Destroy;
end;

function TExternalHelpDatabase.ShowHelp(Query: THelpQuery; BaseNode,
  NewNode: THelpNode; QueryItem: THelpQueryItem; var ErrMsg: string
  ): TShowHelpResult;
var
  ContextList: TPascalHelpContextList;
  Identifier: String;
  AUnitName: String;
  i: Integer;
  Context: String;
  p: LongInt;
  URL: String;
  ShowNode: THelpNode;
  Viewer: THelpViewer;
begin
  //DebugLn(['TExternalHelpDatabase.ShowHelp ',DbgSName(Query)]);
  if (Query is THelpQueryPascalContexts)
  and (QueryItem is TPascalHelpContextList) then begin
    // a pascal context query
    ContextList:=TPascalHelpContextList(QueryItem);
    if (ContextList.Count>0) and (ContextList.List[0].Descriptor=pihcFilename)
    then begin
      // extract unit filename
      AUnitName:=lowercase(ExtractFileNameOnly(ContextList.List[0].Context));
      DebugLn('TExternalHelpDatabase.ShowHelp A Unitname=',AUnitname,' NewNode.HelpType=',dbgs(ord(NewNode.HelpType)),' NewNode.Title=',NewNode.Title,' NewNode.URL=',NewNode.URL);
      if AUnitName<>'' then begin

        // extract identifier
        Identifier:='';
        for i:=0 to ContextList.Count-1 do begin
          Context:=ContextList.List[i].Context;
          case ContextList.List[i].Descriptor of

          pihcProperty,pihcVariable,pihcType,pihcConst:
            begin
              Identifier:=Context;
              break;
            end;

          pihcProcedure:
            begin
              // chomp parameters  ToDo: overloaded procs
              p:=System.Pos('(',Context);
              if p>0 then
                Context:=copy(Context,1,p-1);
              Identifier:=Context;
              break;
            end;

          end;
        end;

        if Identifier<>'' then begin
          DebugLn(['TExternalHelpDatabase.ShowHelp Identifier=',Identifier]);
          // replace special macros (Identifier)
          URL:=NewNode.URL;
          repeat
            p:=System.Pos('$(identifier)',lowercase(URL));
            if p<1 then break;
            URL:=copy(URL,1,p-1)+Identifier
                               +copy(URL,p+length('$(identifier)'),length(URL));
          until false;

          // replace global macros
          if (IDEMacros<>nil) then
            IDEMacros.SubstituteMacros(URL);

          DebugLn(['TExternalHelpDatabase.ShowHelp URL=',URL]);

          // find HTML viewer
          Result:=FindViewer('text/html',ErrMsg,Viewer);
          if Result<>shrSuccess then exit;

          // call viewer
          ShowNode:=nil;
          try
            ShowNode:=THelpNode.CreateURL(Self,NewNode.Title,URL);
            Result:=Viewer.ShowNode(ShowNode,ErrMsg);
          finally
            ShowNode.Free;
          end;
          exit;
        end;
      end;
    end;
  end;

  // otherwise use default
  Result:=inherited ShowHelp(Query, BaseNode, NewNode, QueryItem, ErrMsg);
end;

{ TExternHelpFileSelector }

procedure TExternHelpFileSelector.ButtonPanel1OKButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

constructor TExternHelpFileSelector.Create(TheOwner: TComponent);
begin
  inherited CreateNew(TheOwner);

  FileListBox:=TListBox.Create(Self);
  with FileListBox do begin
    Name:='FileListBox';
    Align:=alClient;
    BorderSpacing.Around:=6;
    Parent:=Self;
  end;

  ButtonPanel1:=TButtonPanel.Create(Self);
  with ButtonPanel1 do begin
    Name:='ButtonPanel1';
    Align:=alBottom;
    ShowBevel:=false;
    ShowButtons:=[pbOK,pbCancel];
    OKButton.OnClick:=@ButtonPanel1OKButtonClick;
    OKButton.ModalResult:=mrNone;
    Parent:=Self;
  end;
end;

finalization
  FreeAndNil(ExternHelpOptions);

end.

