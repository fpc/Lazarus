unit ExternHelpFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, LazConfigStorage, ComCtrls, Buttons, StdCtrls, ExtCtrls,
  IDEOptionsIntf, LazIDEIntf, BaseIDEIntf;

var
  ExternHelpOptionID: integer = 2000;
  ExternHelpOptionGeneralID: integer = 100;

resourcestring
  EHGroupTitle = 'Extern help';
  ehrsName = 'Name';
  ehrsUnitFileOrUnitDirectory = 'Unit file or unit directory';
  ehrsURL = 'URL';
  ehrsHelp = 'Help';
  ehrsAddNewItem = 'Add new item';
  ehrsDeleteItem = 'Delete item';
  ehrsBrowseForPath = 'Browse for path';
  ehrsGeneral = 'General';

type

  { TExternHelpItem }

  TExternHelpItem = class
  private
    FChangeStep: integer;
    fChilds: TFPList;
    FFilename: string;
    FName: string;
    FStoreIn: string;
    FURL: string;
    function GetChildCount: integer;
    function GetChilds(Index: integer): TExternHelpItem;
    procedure SetFilename(const AValue: string);
    procedure SetName(const AValue: string);
    procedure SetStoreIn(const AValue: string);
    procedure SetURL(const AValue: string);
  public
    Parent: TExternHelpItem;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure AddChild(Item: TExternHelpItem);
    procedure MoveChild(FromPos, ToPos: integer);
    procedure RemoveChild(Index: integer);
    procedure DeleteChild(Index: integer);
    function IndexOf(Child: TExternHelpItem): integer;
    function IsEqual(Item: TExternHelpItem; WithName: boolean): boolean;
    procedure Assign(Src: TExternHelpItem; WithName: boolean);
    procedure IncreaseChangeStep; virtual;
    property Name: string read FName write SetName;
    property Filename: string read FFilename write SetFilename;
    property URL: string read FURL write SetURL;
    property StoreIn: string read FStoreIn write SetStoreIn;
    property ChildCount: integer read GetChildCount;
    property Childs[Index: integer]: TExternHelpItem read GetChilds;
    property ChangeStep: integer read FChangeStep;
  end;

  TExternHelpOptions = class;

  { TExternHelpRootItem }

  TExternHelpRootItem = class(TExternHelpItem)
  public
    Owner: TExternHelpOptions;
    procedure IncreaseChangeStep; override;
  end;

  { TExternHelpOptions }

  TExternHelpOptions = class(TAbstractIDEEnvironmentOptions)
  private
    FChangeStep: integer;
    FFilename: string;
    FLastSavedChangeStep: integer;
    procedure SetFilename(const AValue: string);
  public
    RootItem: TExternHelpRootItem;
    constructor Create;
    destructor Destroy; override;
    class function GetGroupCaption: string; override;
    class function GetInstance: TAbstractIDEOptions; override;
    function Load(Config: TConfigStorage): TModalResult; virtual;
    function Save(Config: TConfigStorage): TModalResult; virtual;
    function LoadFromFile(Filename: string): TModalResult; virtual;
    function SaveToFile(Filename: string): TModalResult; virtual;
    function Load: TModalResult; virtual;
    function Save: TModalResult; virtual;
    function GetFullFilename: string;
    function IsEqual(Src: TExternHelpOptions): boolean;
    procedure Assign(Src: TExternHelpOptions);
    procedure IncreaseChangeStep;
    property Filename: string read FFilename write SetFilename;
    property ChangeStep: integer read FChangeStep;
    property LastSavedChangeStep: integer read FLastSavedChangeStep;
  end;

type

  { TExternHelpGeneralOptsFrame }

  TExternHelpGeneralOptsFrame = class(TAbstractIDEOptionsEditor)
    AddSpeedButton: TSpeedButton;
    DeleteSpeedButton: TSpeedButton;
    FileBrowseSpeedButton: TSpeedButton;
    FilenameEdit: TEdit;
    FilenameLabel: TLabel;
    HelpBitBtn: TBitBtn;
    ItemsTreeView: TTreeView;
    NameEdit: TEdit;
    NameLabel: TLabel;
    Splitter1: TSplitter;
    StoreComboBox: TComboBox;
    StoreLabel: TLabel;
    URLEdit: TEdit;
    URLLabel: TLabel;
    procedure ItemsTreeViewEdited(Sender: TObject; Node: TTreeNode;
      var S: string);
    procedure ItemsTreeViewEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure ItemsTreeViewSelectionChanged(Sender: TObject);
    procedure NameEditChange(Sender: TObject);
    procedure NameEditEditingDone(Sender: TObject);
  private
    FOptions: TExternHelpOptions;
    procedure FillItemsTreeView;
    procedure NameChanged(TVNode: TTreeNode; var NewName: string;
      UpdateTree, UpdateEdit: boolean);
    procedure SelectionChanged;
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
  ExternHelpOptionID:=RegisterIDEOptionsGroup(ExternHelpOptionID,TExternHelpOptions)^.Index;
  ExternHelpOptionGeneralID:=RegisterIDEOptionsEditor(ExternHelpOptionID,
      TExternHelpGeneralOptsFrame,ExternHelpOptionGeneralID)^.Index;
end;

{ TExternHelpOptions }

procedure TExternHelpOptions.SetFilename(const AValue: string);
begin
  if FFilename=AValue then exit;
  FFilename:=AValue;
end;

constructor TExternHelpOptions.Create;
begin
  RootItem:=TExternHelpRootItem.Create;
  RootItem.Owner:=Self;
end;

destructor TExternHelpOptions.Destroy;
begin
  FreeAndNil(RootItem);
  inherited Destroy;
end;

class function TExternHelpOptions.GetGroupCaption: string;
begin
  Result:=EHGroupTitle;
end;

class function TExternHelpOptions.GetInstance: TAbstractIDEOptions;
begin
  Result:=ExternHelpOptions;
end;

function TExternHelpOptions.Load(Config: TConfigStorage): TModalResult;
begin
  Result:=mrOk;
end;

function TExternHelpOptions.Save(Config: TConfigStorage): TModalResult;
begin
  Result:=mrOk;
end;

function TExternHelpOptions.LoadFromFile(Filename: string): TModalResult;
var
  Config: TConfigStorage;
begin
  Config:=GetIDEConfigStorage(Filename,true);
  try
    Result:=Load(Config);
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
  finally
    Config.Free;
  end;
end;

function TExternHelpOptions.Load: TModalResult;
begin
  Result:=LoadFromFile(Filename);
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

{ TExternHelpGeneralOptsFrame }

procedure TExternHelpGeneralOptsFrame.ItemsTreeViewEdited(Sender: TObject;
  Node: TTreeNode; var S: string);
begin
  NameChanged(Node,S,false,true);
end;

procedure TExternHelpGeneralOptsFrame.ItemsTreeViewEditing(Sender: TObject;
  Node: TTreeNode; var AllowEdit: Boolean);
begin

end;

procedure TExternHelpGeneralOptsFrame.ItemsTreeViewSelectionChanged(
  Sender: TObject);
begin
  SelectionChanged;
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

procedure TExternHelpGeneralOptsFrame.FillItemsTreeView;

  procedure Add(ParentItem: TExternHelpItem; ParentTVNode: TTreeNode);
  var
    i: Integer;
    Item: TExternHelpItem;
    TVNode: TTreeNode;
  begin
    for i:=0 to ParentItem.ChildCount-1 do begin
      Item:=ParentItem.Childs[i];
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
  if TObject(TVNode.Data) is TExternHelpItem then begin
    Item:=TExternHelpItem(TVNode.Data);
    Item.Name:=NewName;
    if UpdateTree then
      TVNode.Text:=NewName;
    if UpdateEdit then
      NameEdit.Text:=NewName;
  end;
end;

procedure TExternHelpGeneralOptsFrame.SelectionChanged;
var
  TVNode: TTreeNode;
  Item: TExternHelpItem;
begin
  TVNode:=ItemsTreeView.Selected;
  Item:=nil;
  if (TVNode<>nil) and (TObject(TVNode.Data) is TExternHelpItem) then
    Item:=TExternHelpItem(TVNode.Data);
  DisableAlign;
  if Item<>nil then begin
    NameEdit.Enabled:=true;
    NameEdit.Text:=Item.Name;
    FilenameEdit.Enabled:=true;
    FilenameEdit.Text:=Item.Filename;
    URLEdit.Enabled:=true;
    URLEdit.Text:=Item.URL;
    StoreComboBox.Enabled:=true;
    StoreComboBox.Text:=Item.StoreIn;
  end else begin
    NameEdit.Enabled:=false;
    NameEdit.Text:='';
    FilenameEdit.Enabled:=false;
    FilenameEdit.Text:='';
    URLEdit.Enabled:=false;
    URLEdit.Text:='';
    StoreComboBox.Enabled:=false;
    StoreComboBox.Text:='';
  end;
end;

constructor TExternHelpGeneralOptsFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FOptions:=TExternHelpOptions.Create;
end;

destructor TExternHelpGeneralOptsFrame.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

function TExternHelpGeneralOptsFrame.GetTitle: String;
begin
  Result:=ehrsGeneral;
end;

procedure TExternHelpGeneralOptsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
var
  Opts: TExternHelpOptions;
begin
  if AOptions is TExternHelpOptions then begin
    Opts:=TExternHelpOptions(AOptions);
    Options.Assign(Opts);
    FillItemsTreeView;
    ItemsTreeView.Selected:=ItemsTreeView.Items.GetFirstNode;
  end;
end;

procedure TExternHelpGeneralOptsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  NameLabel.Caption:=ehrsName;
  FilenameLabel.Caption:=ehrsUnitFileOrUnitDirectory;
  URLLabel.Caption:=ehrsURL;
  HelpBitBtn.Caption:=ehrsHelp;
  AddSpeedButton.Hint:=ehrsAddNewItem;
  DeleteSpeedButton.Hint:=ehrsDeleteItem;
  FileBrowseSpeedButton.Hint:=ehrsBrowseForPath;
end;

class function TExternHelpGeneralOptsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=TExternHelpOptions;
end;

procedure TExternHelpGeneralOptsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
var
  Opts: TExternHelpOptions;
begin
  if AOptions is TExternHelpOptions then begin
    Opts:=TExternHelpOptions(AOptions);
    Opts.Assign(Options);
  end;
end;

{ TExternHelpItem }

function TExternHelpItem.GetChildCount: integer;
begin
  Result:=fChilds.Count;
end;

function TExternHelpItem.GetChilds(Index: integer): TExternHelpItem;
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

constructor TExternHelpItem.Create;
begin
  fChilds:=TFPList.Create;
end;

destructor TExternHelpItem.Destroy;
begin
  Clear;
  FreeAndNil(fChilds);
  inherited Destroy;
end;

procedure TExternHelpItem.Clear;
var
  i: Integer;
begin
  if (ChildCount=0) and (URL='') and (Filename='') and (StoreIn='') then exit;
  for i:=fChilds.Count-1 downto 0 do TObject(fChilds).Free;
  fChilds.Clear;
  fURL:='';
  FFilename:='';
  FStoreIn:='';
  IncreaseChangeStep;
end;

procedure TExternHelpItem.AddChild(Item: TExternHelpItem);
begin
  Item.Parent:=Self;
  fChilds.Add(Item);
end;

procedure TExternHelpItem.MoveChild(FromPos, ToPos: integer);
begin
  fChilds.Move(FromPos,ToPos);
end;

procedure TExternHelpItem.RemoveChild(Index: integer);
begin
  fChilds.Delete(Index);
end;

procedure TExternHelpItem.DeleteChild(Index: integer);
begin
  TObject(fChilds[Index]).Free;
  fChilds.Delete(Index);
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
    and (URL=Item.URL)
    and (StoreIn=Item.StoreIn)
    and (ChildCount=Item.ChildCount);
  if Result then begin
    for i:=0 to ChildCount-1 do
      if not (Childs[i].IsEqual(Item.Childs[i],true)) then exit(false);
  end;
end;

procedure TExternHelpItem.Assign(Src: TExternHelpItem; WithName: boolean);
var
  i: Integer;
  Item: TExternHelpItem;
begin
  if WithName then Name:=Src.Name;
  Filename:=Src.Filename;
  URL:=Src.URL;
  StoreIn:=Src.StoreIn;
  for i:=0 to Src.ChildCount-1 do begin
    if ChildCount<=i then begin
      Item:=TExternHelpItem.Create;
      AddChild(Item);
    end else begin
      Item:=Childs[i];
    end;
    Item.Assign(Src.Childs[i],true);
  end;
  while ChildCount>Src.ChildCount do DeleteChild(ChildCount-1);
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

finalization
  FreeAndNil(ExternHelpOptions);

end.

