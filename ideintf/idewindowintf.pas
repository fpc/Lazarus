{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner
  
  Abstract:
    Provides general classes and methods to access and handle IDE dialogs and
    windows.
}
unit IDEWindowIntf;

{$mode objfpc}{$H+}

interface

uses
  Math, types, Classes, SysUtils, LCLProc, LazConfigStorage, Forms, Controls;

  //----------------------------------------------------------------------------
  // layout settings of modal forms (dialogs) in the IDE
type

  TIDEDialogLayoutList = class;

  { TIDEDialogLayout - for modal forms
    For non modal forms see TIDEWindowCreator below }

  TIDEDialogLayout = class
  private
    FList: TIDEDialogLayoutList;
    FModified: boolean;
    FName: string;
    FWidth: integer;
    FHeight: integer;
    procedure SetList(const AValue: TIDEDialogLayoutList);
    procedure SetModified(const AValue: boolean);
    procedure SetWidth(const AValue: integer);
    procedure SetHeight(const AValue: integer);
  public
    constructor Create(const TheName: string; TheList: TIDEDialogLayoutList);
    function SizeValid: boolean;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
    property Name: string read FName;
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
    property List: TIDEDialogLayoutList read FList write SetList;
    property Modified: boolean read FModified write SetModified;
  end;
  TIDEDialogLayoutClass = class of TIDEDialogLayout;

  { TIDEDialogLayoutList - for modal forms }

  TIDEDialogLayoutList = class
  private
    FItemClass: TIDEDialogLayoutClass;
    FItems: TList;
    FModified: boolean;
    function GetItems(Index: integer): TIDEDialogLayout;
  protected
    procedure SetModified(const AValue: boolean); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ApplyLayout(ADialog: TControl;
                          DefaultWidth, DefaultHeight: integer;
                          UseAsMin: boolean = true);
    procedure ApplyLayout(ADialog: TControl);
    procedure SaveLayout(ADialog: TControl);
    procedure Clear;
    function Count: integer;
    function Find(const DialogName: string;
                  CreateIfNotExists: boolean): TIDEDialogLayout;
    function Find(ADialog: TObject;
                  CreateIfNotExists: boolean): TIDEDialogLayout;
    function IndexOf(const DialogName: string): integer;
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
    property Items[Index: integer]: TIDEDialogLayout read GetItems;
    property Modified: boolean read FModified write SetModified;
    property ItemClass: TIDEDialogLayoutClass read FItemClass write FItemClass;
  end;
  
  { TIDEDialogLayoutStorage }

  TIDEDialogLayoutStorage = class(TComponent)
  protected
    procedure OnCreateForm(Sender: TObject);
    procedure OnCloseForm(Sender: TObject; var CloseAction: TCloseAction);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  IDEDialogLayoutList: TIDEDialogLayoutList = nil;// set by the IDE

type
  { TSimpleWindowLayout stores information about the position, min/maximized state
    and similar things for an IDE window or dialog, like the source editor,
    the object inspector, the main bar or the message view.
  }
  TIDEWindowPlacement = (
    iwpUseWindowManagerSetting, // leave window position, where window manager
                                //   creates the window
    iwpDefault,                 // set window to the default position
    iwpRestoreWindowGeometry,   // save window geometry at end and restore it
                                //   at start
    iwpCustomPosition,          // set window to custom position
    iwpRestoreWindowSize        // save window size at end and restore it
                                //   at start
    );
  TIDEWindowPlacements = set of TIDEWindowPlacement;
  TIDEWindowState = (iwsNormal, iwsMaximized, iwsMinimized, iwsHidden);
  TIDEWindowStates = set of TIDEWindowState;

  { TSimpleWindowLayout }

  TSimpleWindowLayout = class(TComponent)
  private
    FApplied: boolean;
    FFormCaption: string;
    FVisible: boolean;
    fWindowPlacement: TIDEWindowPlacement;
    fLeft: integer;
    fTop: integer;
    fWidth: integer;
    fHeight: integer;
    fWindowState: TIDEWindowState;
    fForm: TCustomForm;
    fFormID: string;
    fDefaultWindowPlacement: TIDEWindowPlacement;
    function GetFormCaption: string;
    function GetFormID: string;
    procedure SetForm(const AValue: TCustomForm);
    procedure OnFormClose(Sender: TObject; var CloseAction: TCloseAction);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AFormID: string); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    procedure GetCurrentPosition;
    procedure Assign(Layout: TSimpleWindowLayout); reintroduce;
    procedure ReadCurrentCoordinates;
    procedure ReadCurrentState;
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
    function CustomCoordinatesAreValid: boolean;
    procedure CloseForm;
  public
    property FormID: string read GetFormID write FFormID;
    function FormBaseID(out SubIndex: Integer): String; // split FormID into name+number
    property FormCaption: string read GetFormCaption;
    property WindowPlacement: TIDEWindowPlacement read fWindowPlacement write FWindowPlacement;
    property DefaultWindowPlacement: TIDEWindowPlacement
      read fDefaultWindowPlacement write fDefaultWindowPlacement;
    property Left: integer read fLeft write FLeft;
    property Top: integer read fTop write FTop;
    property Width: integer read fWidth write FWidth;
    property Height: integer read fHeight write FHeight;
    property WindowState: TIDEWindowState read fWindowState write FWindowState;
    property Form: TCustomForm read fForm write SetForm;
    property Visible: boolean read FVisible write FVisible;
    property Applied: boolean read FApplied write FApplied;
  end;

  { TSimpleWindowLayoutList }

  TSimpleWindowLayoutList = class
  private
    fItems: TFPList;
    function GetItems(Index: Integer): TSimpleWindowLayout;
    procedure OnScreenRemoveForm(Sender: TObject; AForm: TCustomForm);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure ApplyAndShow(Sender: TObject; AForm: TCustomForm;
                           BringToFront: boolean);
    procedure StoreWindowPositions;
    procedure Assign(SrcList: TSimpleWindowLayoutList);
    function Add(ALayout: TSimpleWindowLayout): integer;
    function CreateWindowLayout(const TheFormID: string): TSimpleWindowLayout;
    function CreateWindowLayout(const TheForm: TCustomForm): TSimpleWindowLayout;
    function IndexOf(const FormID: string): integer;
    function ItemByForm(AForm: TCustomForm): TSimpleWindowLayout;
    function ItemByFormID(const FormID: string): TSimpleWindowLayout;
    function ItemByFormCaption(const aFormCaption: string): TSimpleWindowLayout;
    procedure CloseForm(AForm: TCustomForm);
    procedure LoadFromConfig(Config: TConfigStorage; const Path: string);
    procedure SaveToConfig(Config: TConfigStorage; const Path: string);
  public
    function Count: integer;
    property Items[Index: Integer]: TSimpleWindowLayout read GetItems; default;
  end;

const
  IDEWindowPlacementNames: array[TIDEWindowPlacement] of string = (
      'UseWindowManagerSetting',
      'Default',
      'RestoreWindowGeometry',
      'CustomPosition',
      'RestoreWindowSize'
    );
  IDEWindowStateNames: array[TIDEWindowState] of string = (
      'Normal', 'Maximized', 'Minimized', 'Hidden'
    );

function StrToIDEWindowPlacement(const s: string): TIDEWindowPlacement;
function StrToIDEWindowState(const s: string): TIDEWindowState;

type
  TIWCState = (
    iwcsHidden,
    iwcsIconified,
    iwcsNormal,
    iwcsDocked
    );

  TCreateIDEWindowMethod = procedure(Sender: TObject; aFormName: string;
                var AForm: TCustomForm; DoDisableAutoSizing: boolean) of object;
  TCreateIDEWindowProc = procedure(Sender: TObject; aFormName: string;
                var AForm: TCustomForm; DoDisableAutoSizing: boolean);
  TGetDefaultIDEWindowLayoutEvent = procedure(Sender: TObject; aFormName: string;
   out aBounds: TRect; out DockSibling: string; out DockAlign: TAlign) of object;
  TShowIDEWindowEvent = procedure(Sender: TObject; AForm: TCustomForm;
                                  BringToFront: boolean) of object;

  { TIDEWindowCreator
    Every dockable window of the IDE needs a TIDEWindowCreator. }

  TIDEWindowCreator = class
  private
    FCreateFormMethod: TCreateIDEWindowMethod;
    FCreateFormProc: TCreateIDEWindowProc;
    FDockAlign: TAlign;
    FDockSibling: string;
    FFormName: string;
    FBottom: string;
    FLeft: string;
    FMulti: boolean;
    FOnGetLayout: TGetDefaultIDEWindowLayoutEvent;
    FState: TIWCState;
    FTop: string;
    FRight: string;
    procedure SetBottom(const AValue: string);
    procedure SetLeft(const AValue: string);
    procedure SetTop(const AValue: string);
    procedure SetRight(const AValue: string);
  public
    constructor Create(aFormName: string); overload;
    constructor Create(aFormName: string;
                       CreateFormProc: TCreateIDEWindowProc;
                       CreateFormMethod: TCreateIDEWindowMethod;
                       aLeft, aTop, aRight, aBottom: string;
                       aDockSibling : string = '';
                       aDockAlign: TAlign = alNone;
                       aMulti: boolean = false;
               GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent = nil); overload;
    property FormName: string read FFormName; // prefix for all forms
    property Multi: boolean read FMulti; // there can be more than one of this form, e.g. the source editors and the package editors
    property OnCreateFormMethod: TCreateIDEWindowMethod read FCreateFormMethod write FCreateFormMethod;
    property OnCreateFormProc: TCreateIDEWindowProc read FCreateFormProc write FCreateFormProc;
    function NameFits(const AName: string): boolean;

    // hints for the layout system / dock master. It may ignore them completely.
    property State: TIWCState read FState write FState;
    property Left: string read FLeft write SetLeft; // '12' for 12 pixel, '10%' for 10 percent of screen.width
    property Top: string read FTop write SetTop; // '12' for 12 pixel, '10%' for 10 percent of screen.height
    property Right: string read FRight write SetRight; // '12' for 12 pixel, '+12' for a Width of 12 pixel, '-12' for Screen.Width-12, '10%' for 10 percent of screen.width
    property Bottom: string read FBottom write SetBottom; // '12' for 12 pixel, '+12' for a Height of 12 pixel, '10%' for 10 percent of screen.height
    property DockSibling: string read FDockSibling write FDockSibling; // another form name
    property DockAlign: TAlign read FDockAlign write FDockAlign;
    property OnGetLayout: TGetDefaultIDEWindowLayoutEvent read FOnGetLayout
                                                          write FOnGetLayout;
    procedure CheckBoundValue(s: string);
    procedure GetDefaultBounds(AForm: TCustomForm; out DefBounds: TRect);
  end;

  { TIDEWindowCreatorList }

  TIDEWindowCreatorList = class
  private
    fItems: TFPList; // list of TIDEWindowCreator
    FScreenMaxSizeForDefaults: TPoint;
    FSimpleLayoutStorage: TSimpleWindowLayoutList;
    function GetItems(Index: integer): TIDEWindowCreator;
    procedure ErrorIfFormExists(FormName: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    property Items[Index: integer]: TIDEWindowCreator read GetItems; default;
    function Add(aLayout: TIDEWindowCreator): integer; overload;
    function Add(aFormName: string): TIDEWindowCreator; overload;
    function Add(aFormName: string;
                 CreateFormProc: TCreateIDEWindowProc;
                 CreateFormMethod: TCreateIDEWindowMethod;
                 // you can pass some hints to the layout system how to place the form by default
                 // Note: the IDE dockmaster may completely ignore these values
                 aLeft, aTop, aRight, aBottom: string;
                 aDockSibling : string = '';
                 aDockAlign: TAlign = alNone;
                 aMulti: boolean = false; // if true, then there can be more than one instance, the aFormName is the shared prefix
                 GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent = nil
                 ): TIDEWindowCreator; overload;
    procedure Delete(Index: integer);
    function IndexOfName(FormName: string): integer;
    function FindWithName(FormName: string): TIDEWindowCreator;
    function GetForm(aFormName: string; AutoCreate: boolean;
                               DisableAutoSizing: boolean = false): TCustomForm;
    procedure ShowForm(AForm: TCustomForm; BringToFront: boolean); overload;
    function ShowForm(AFormName: string; BringToFront: boolean): TCustomForm; overload;
    procedure CreateForm(var AForm; AFormClass: TCustomFormClass;
                         DoDisableAutoSizing: boolean; TheOwner: TComponent); // utility function to create a form with delayed autosizing

    property SimpleLayoutStorage: TSimpleWindowLayoutList read FSimpleLayoutStorage;
    procedure RestoreSimpleLayout;

    property ScreenMaxSizeForDefaults: TPoint read FScreenMaxSizeForDefaults
                                              write FScreenMaxSizeForDefaults; // on big screens: do not span the whole screen
    function GetScreenrectForDefaults: TRect;
  end;

var
  IDEWindowCreators: TIDEWindowCreatorList = nil; // set by the IDE

type

  TDockSides = set of TAlign;

  { TIDEDockMaster }

  TIDEDockMaster = class
  protected
    FHideSimpleLayoutOptions: boolean;
  public
    procedure MakeIDEWindowDockable(AControl: TWinControl); virtual; abstract; // make AControl dockable, it can be docked and other dockable windows can be docked to it, this does not make it visible
    procedure MakeIDEWindowDockSite(AForm: TCustomForm; ASides: TDockSides = [alBottom]); virtual; abstract; // make AForm a dock site, AForm can not be docked, its Parent must be kept nil, this does not make it visible
    procedure ShowForm(AForm: TCustomForm; BringToFront: boolean); virtual; abstract; // make a form visible, set BringToFront=true if form should be shown on active screen and on front of other windows, normally this focus the form
    function AddableInWindowMenu(AForm: TCustomForm): boolean; virtual;
    procedure CloseAll; virtual; // close all forms, called after IDE has saved all and shuts down
    property HideSimpleLayoutOptions: boolean read FHideSimpleLayoutOptions;
  end;

var
  IDEDockMaster: TIDEDockMaster = nil; // can be set by a package

procedure MakeIDEWindowDockable(AControl: TWinControl);
procedure MakeIDEWindowDockSite(AForm: TCustomForm);
procedure CloseAllForms;

procedure Register;

implementation

function StrToIDEWindowPlacement(const s: string): TIDEWindowPlacement;
begin
  for Result:=Low(TIDEWindowPlacement) to High(TIDEWindowPlacement) do
    if AnsiCompareText(s,IDEWindowPlacementNames[Result])=0 then exit;
  Result:=iwpDefault;
end;

function StrToIDEWindowState(const s: string): TIDEWindowState;
begin
  for Result:=Low(TIDEWindowState) to High(TIDEWindowState) do
    if AnsiCompareText(s,IDEWindowStateNames[Result])=0 then exit;
  Result:=iwsNormal;
end;

procedure MakeIDEWindowDockable(AControl: TWinControl);
begin
  if Assigned(IDEDockMaster) then
    IDEDockMaster.MakeIDEWindowDockable(AControl);
end;

procedure MakeIDEWindowDockSite(AForm: TCustomForm);
begin
  if Assigned(IDEDockMaster) then
    IDEDockMaster.MakeIDEWindowDockSite(AForm);
end;

procedure CloseAllForms;
var
  i: Integer;
  AForm: TCustomForm;
  AControl: TWinControl;
begin
  // hide all forms
  i:=Screen.CustomFormCount-1;
  while i>=0 do begin
    AForm:=GetParentForm(Screen.CustomForms[i]);
    AForm.Hide;
    dec(i);
    if i>=Screen.CustomFormCount then i:=Screen.CustomFormCount-1;
  end;

  // close all forms except the MainForm
  i:=Screen.CustomFormCount-1;
  while i>=0 do begin
    AForm:=Screen.CustomForms[i];
    if (AForm<>Application.MainForm) and not AForm.IsParentOf(Application.MainForm)
    then begin
      AControl:=AForm;
      while (AControl.Parent<>nil)
      and (AControl.Parent<>Application.MainForm) do begin
        AControl:=AControl.Parent;
        if AControl is TCustomForm then AForm:=TCustomForm(AControl);
      end;
      AForm.Close;
    end;
    dec(i);
    if i>=Screen.CustomFormCount then i:=Screen.CustomFormCount-1;
  end;
end;

procedure Register;
begin
  RegisterComponents('Misc',[TIDEDialogLayoutStorage]);
end;

{ TIDEDialogLayout }

procedure TIDEDialogLayout.SetList(const AValue: TIDEDialogLayoutList);
begin
  if FList=AValue then exit;
  FList:=AValue;
  if (List<>nil) and Modified then List.Modified:=true;
end;

procedure TIDEDialogLayout.SetModified(const AValue: boolean);
begin
  FModified:=AValue;
  if FModified and (FList<>nil) then FList.Modified:=true;
end;

procedure TIDEDialogLayout.SetWidth(const AValue: integer);
begin
  if FWidth=AValue then exit;
  FWidth:=AValue;
  Modified:=true;
end;

procedure TIDEDialogLayout.SetHeight(const AValue: integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
  Modified:=true;
end;

constructor TIDEDialogLayout.Create(const TheName: string;
  TheList: TIDEDialogLayoutList);
begin
  FName:=TheName;
  FList:=TheList;
end;

function TIDEDialogLayout.SizeValid: boolean;
begin
  Result:=(Width>10) and (Height>10);
end;

procedure TIDEDialogLayout.LoadFromConfig(Config: TConfigStorage;
  const Path: string);
begin
  FName:=Config.GetValue(Path+'Name/Value','');
  FWidth:=Config.GetValue(Path+'Size/Width',0);
  FHeight:=Config.GetValue(Path+'Size/Height',0);
  Modified:=false;
end;

procedure TIDEDialogLayout.SaveToConfig(Config: TConfigStorage;
  const Path: string);
begin
  Config.SetValue(Path+'Name/Value',Name);
  Config.SetValue(Path+'Size/Width',Width);
  Config.SetValue(Path+'Size/Height',Height);
  Modified:=false;
end;

{ TIDEDialogLayoutList }

function TIDEDialogLayoutList.GetItems(Index: integer): TIDEDialogLayout;
begin
  Result:=TIDEDialogLayout(FItems[Index]);
end;

procedure TIDEDialogLayoutList.SetModified(const AValue: boolean);
begin
  if FModified=AValue then exit;
  FModified:=AValue;
end;

constructor TIDEDialogLayoutList.Create;
begin
  inherited Create;
  FItems:=TList.Create;
  FItemClass:=TIDEDialogLayout;
end;

destructor TIDEDialogLayoutList.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TIDEDialogLayoutList.ApplyLayout(ADialog: TControl;
  DefaultWidth, DefaultHeight: integer; UseAsMin: boolean);
var
  ALayout: TIDEDialogLayout;
  NewWidth, NewHeight: integer;
begin
  if (ADialog=nil) or (Self=nil) then exit;
  ALayout:=Find(ADialog,true);
  //debugln(['TIDEDialogLayoutList.ApplyLayout ',ALayout.Name,' ',ALayout.SizeValid,' ',ALayout.Width,',',ALayout.Height]);
  if ALayout.SizeValid then begin
    NewWidth:=ALayout.Width;
    NewHeight:=ALayout.Height;
  end else begin
    NewWidth:=DefaultWidth;
    NewHeight:=DefaultHeight;
  end;
  if UseAsMin then begin
    if NewWidth<DefaultWidth then NewWidth:=DefaultWidth;
    if NewHeight<DefaultHeight then NewHeight:=DefaultHeight;
  end;
  ADialog.SetBounds(ADialog.Left,ADialog.Top,NewWidth,NewHeight);
end;

procedure TIDEDialogLayoutList.ApplyLayout(ADialog: TControl);
begin
  ApplyLayout(ADialog,ADialog.Width,ADialog.Height);
end;

procedure TIDEDialogLayoutList.SaveLayout(ADialog: TControl);
var
  ALayout: TIDEDialogLayout;
begin
  if (ADialog=nil) or (Self=nil) then exit;
  ALayout:=Find(ADialog,true);
  ALayout.Width:=ADialog.Width;
  ALayout.Height:=ADialog.Height;
end;

procedure TIDEDialogLayoutList.Clear;
var i: integer;
begin
  for i:=0 to FItems.Count-1 do
    Items[i].Free;
  FItems.Clear;
end;

function TIDEDialogLayoutList.Count: integer;
begin
  Result:=FItems.Count;
end;

function TIDEDialogLayoutList.Find(const DialogName: string;
  CreateIfNotExists: boolean): TIDEDialogLayout;
var i: integer;
begin
  i:=IndexOf(DialogName);
  if (i<0) then begin
    if CreateIfNotExists then begin
      Result:=FItemClass.Create(DialogName,Self);
      FItems.Add(Result);
    end else begin
      Result:=nil;
    end;
  end else begin
    Result:=Items[i];
  end;
end;

function TIDEDialogLayoutList.Find(ADialog: TObject; CreateIfNotExists: boolean
  ): TIDEDialogLayout;
begin
  if ADialog<>nil then begin
    Result:=Find(ADialog.ClassName,CreateIfNotExists);
  end else begin
    Result:=nil;
  end;
end;

function TIDEDialogLayoutList.IndexOf(const DialogName: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (CompareText(DialogName,Items[Result].Name)<>0) do
    dec(Result);
end;

procedure TIDEDialogLayoutList.LoadFromConfig(Config: TConfigStorage; const Path: string);
var
  NewCount, i: integer;
  NewDialogLayout: TIDEDialogLayout;
begin
  Clear;
  NewCount:=Config.GetValue(Path+'Count',0);
  for i:=0 to NewCount-1 do begin
    NewDialogLayout:=FItemClass.Create('',Self);
    FItems.Add(NewDialogLayout);
    NewDialogLayout.LoadFromConfig(Config,Path+'Dialog'+IntToStr(i+1)+'/');
  end;
  Modified:=false;
end;

procedure TIDEDialogLayoutList.SaveToConfig(Config: TConfigStorage; const Path: string);
var i: integer;
begin
  Config.SetDeleteValue(Path+'Count',Count,0);
  for i:=0 to Count-1 do
    Items[i].SaveToConfig(Config,Path+'Dialog'+IntToStr(i+1)+'/');
  Modified:=false;
end;

{ TIDEDialogLayoutStorage }

procedure TIDEDialogLayoutStorage.OnCreateForm(Sender: TObject);
begin
  if Sender=nil then ;
  IDEDialogLayoutList.ApplyLayout(Sender as TControl);
end;

procedure TIDEDialogLayoutStorage.OnCloseForm(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if Sender=nil then ;
  IDEDialogLayoutList.SaveLayout(Sender as TControl);
end;

constructor TIDEDialogLayoutStorage.Create(TheOwner: TComponent);
var
  Form: TCustomForm;
begin
  inherited Create(TheOwner);
  if Owner is TCustomForm then begin
    Form:=TCustomForm(Owner);
    Form.AddHandlerCreate(@OnCreateForm);
    Form.AddHandlerClose(@OnCloseForm);
  end;
end;

destructor TIDEDialogLayoutStorage.Destroy;
var
  Form: TCustomForm;
begin
  if Owner is TCustomForm then begin
    Form:=TCustomForm(Owner);
    Form.RemoveAllHandlersOfObject(Self);
  end;
  inherited Destroy;
end;

{ TSimpleWindowLayout }

constructor TSimpleWindowLayout.Create(AFormID: string);
begin
  inherited Create(nil);
  FormID:=AFormID;
  fDefaultWindowPlacement:=iwpRestoreWindowGeometry;
  Clear;
end;

destructor TSimpleWindowLayout.Destroy;
begin
  Form:=nil;
  inherited Destroy;
end;

procedure TSimpleWindowLayout.LoadFromConfig(Config: TConfigStorage; const Path: string);
var
  P: string;
begin
  // set all values to default
  Clear;
  // read settings
  // build path
  P:=FormID;
  if P='' then exit;
  P:=Path+P+'/';
  FFormCaption := Config.GetValue(P+'Caption/Value', fFormID);
  // placement
  fWindowPlacement:=StrToIDEWindowPlacement(Config.GetValue(
    P+'WindowPlacement/Value',IDEWindowPlacementNames[fWindowPlacement]));
  // custom position
  fLeft:=Config.GetValue(P+'CustomPosition/Left',fLeft);
  fTop:=Config.GetValue(P+'CustomPosition/Top',fTop);
  fWidth:=Config.GetValue(P+'CustomPosition/Width',fWidth);
  fHeight:=Config.GetValue(P+'CustomPosition/Height',fHeight);
  // state
  fWindowState:=StrToIDEWindowState(Config.GetValue(
    P+'WindowState/Value',IDEWindowStateNames[fWindowState]));
  FVisible:=Config.GetValue(P+'Visible/Value',false);
  //debugln(['TSimpleWindowLayout.LoadFromConfig ',FormID,' ',Left,',',Top,',',Width,',',Height]);
end;

procedure TSimpleWindowLayout.SaveToConfig(Config: TConfigStorage;
  const Path: string);
var
  P: string;
begin
  // build path
  P:=FormID;
  if P='' then exit;
  P:=Path+P+'/';
  Config.SetDeleteValue(P+'Caption/Value',FFormCaption,'');
  // placement
  Config.SetDeleteValue(P+'WindowPlacement/Value',
    IDEWindowPlacementNames[fWindowPlacement],
    IDEWindowPlacementNames[iwpRestoreWindowSize]);
  // custom position
  Config.SetDeleteValue(P+'CustomPosition/Left',fLeft,0);
  Config.SetDeleteValue(P+'CustomPosition/Top',fTop,0);
  Config.SetDeleteValue(P+'CustomPosition/Width',fWidth,0);
  Config.SetDeleteValue(P+'CustomPosition/Height',fHeight,0);
  // state
  Config.SetValue(P+'WindowState/Value',IDEWindowStateNames[fWindowState]);
  Config.SetDeleteValue(P+'Visible/Value',FVisible,false);
end;

procedure TSimpleWindowLayout.OnFormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  GetCurrentPosition;
end;

procedure TSimpleWindowLayout.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if fForm=AComponent then begin
      fForm:=nil;
      Applied:=false;
    end;
  end;
end;

function TSimpleWindowLayout.CustomCoordinatesAreValid: boolean;
begin
  Result:=(Width>0) and (Height>0); // and (Left>10-Width) and (Top>10-Height);
end;

procedure TSimpleWindowLayout.CloseForm;
begin
  GetCurrentPosition;
  Form:=nil;
end;

function TSimpleWindowLayout.FormBaseID(out SubIndex: Integer): String;
var
  i: Integer;
begin
  Result := FormID;
  SubIndex := -1;
  i := length(Result);
  while (i > 0) and (Result[i] in ['0'..'9']) do
    dec(i);
  if (i < 1) or (i = length(Result)) then
    exit;
  SubIndex := StrToInt(copy(Result, i+1, length(Result)));
  Result := copy(Result, 1, i);
end;

procedure TSimpleWindowLayout.SetForm(const AValue: TCustomForm);
begin
  if fForm=AValue then exit;
  if (Form<>nil) then begin
    RemoveFreeNotification(Form);
    Form.RemoveHandlerClose(@OnFormClose);
  end;
  fForm:=AValue;
  if (Form<>nil) then begin
    fFormID := Form.Name;
    FFormCaption := Form.Caption;
    FreeNotification(Form);
    Applied:=false;
  end;
end;

function TSimpleWindowLayout.GetFormID: string;
begin
  if Form=nil then
    Result:=fFormID
  else
    Result:=Form.Name;
end;

function TSimpleWindowLayout.GetFormCaption: string;
begin
  if Form<>nil then
    FFormCaption:=Form.Caption
  else if FFormCaption='' then
    FFormCaption:=FormID;
  Result:=FFormCaption;
end;

procedure TSimpleWindowLayout.Clear;
begin
  //debugln(['TSimpleWindowLayout.Clear ',FormID]);
  fWindowPlacement:=fDefaultWindowPlacement;
  fLeft:=0;
  fTop:=0;
  fWidth:=0;
  fHeight:=0;
  fWindowState:=iwsNormal;
end;

procedure TSimpleWindowLayout.ReadCurrentCoordinates;
var
  p: TPoint;
begin
  if (Form<>nil) and (Form.WindowState=wsNormal) then begin
    if Form.Parent<>nil then
      p:=Form.ClientOrigin
    else
      p:=Point(0,0);
    Left:=Form.Left+p.X;
    Top:=Form.Top+p.Y;
    Width:=Form.Width;
    Height:=Form.Height;
  end;
end;

procedure TSimpleWindowLayout.ReadCurrentState;
begin
  Visible:=(Form<>nil) and Form.IsVisible;
  if Form<>nil then begin
    case Form.WindowState of
    wsMinimized: fWindowState:=iwsMinimized;
    wsMaximized: fWindowState:=iwsMaximized;
    else
      fWindowState:=iwsNormal;
    end;
  end;
end;

procedure TSimpleWindowLayout.Assign(Layout: TSimpleWindowLayout);
begin
  Clear;
  FApplied:=Layout.Applied;
  Form:=Layout.Form;
  fWindowPlacement:=Layout.fWindowPlacement;
  fLeft:=Layout.fLeft;
  fTop:=Layout.fTop;
  fWidth:=Layout.fWidth;
  fHeight:=Layout.fHeight;
  fWindowState:=Layout.fWindowState;
  fFormID:=Layout.fFormID;
  fDefaultWindowPlacement:=Layout.fDefaultWindowPlacement;
end;

procedure TSimpleWindowLayout.GetCurrentPosition;
begin
  //debugln('TSimpleWindowLayout.GetCurrentPosition ',DbgSName(Self),' ',FormID,' ',IDEWindowPlacementNames[WindowPlacement]);
  case WindowPlacement of
  iwpRestoreWindowGeometry, iwpRestoreWindowSize:
    ReadCurrentCoordinates;
  end;
  ReadCurrentState;
  //debugln('TSimpleWindowLayout.GetCurrentPosition ',DbgSName(Self),' ',FormID,' Width=',dbgs(Width));
end;

{ TSimpleWindowLayoutList }

procedure TSimpleWindowLayoutList.Clear;
var i: integer;
begin
  for i:=0 to Count-1 do Items[i].Free;
  fItems.Clear;
end;

procedure TSimpleWindowLayoutList.Delete(Index: Integer);
begin
  Items[Index].Free;
  fItems.Delete(Index);
end;

function TSimpleWindowLayoutList.GetItems(Index: Integer): TSimpleWindowLayout;
begin
  Result:=TSimpleWindowLayout(fItems[Index]);
end;

procedure TSimpleWindowLayoutList.OnScreenRemoveForm(Sender: TObject;
  AForm: TCustomForm);
begin
  CloseForm(AForm);
end;

constructor TSimpleWindowLayoutList.Create;
begin
  fItems:=TFPList.Create;
  Screen.AddHandlerRemoveForm(@OnScreenRemoveForm);
end;

destructor TSimpleWindowLayoutList.Destroy;
begin
  Screen.RemoveHandlerRemoveForm(@OnScreenRemoveForm);
  Clear;
  FreeAndNil(fItems);
  inherited Destroy;
end;

function TSimpleWindowLayoutList.IndexOf(const FormID: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and (FormID<>Items[Result].GetFormID) do dec(Result);
end;

procedure TSimpleWindowLayoutList.LoadFromConfig(Config: TConfigStorage; const Path: string);
// do not clear, just add/replace the values from the config
var
  i: integer;
  ID: String;
begin
  // create new windows
  i := Config.GetValue(Path+'Desktop/FormIdCount', 0);
  //debugln(['TSimpleWindowLayoutList.LoadFromConfig ',i]);
  while i > 0 do begin
    ID := Config.GetValue(Path+'Desktop/FormIdList/a'+IntToStr(i), '');
    //debugln(['TSimpleWindowLayoutList.LoadFromConfig ',i,' ',ID]);
    if (ID <> '') and (IDEWindowCreators.SimpleLayoutStorage.ItemByFormID(ID) = nil) then
      CreateWindowLayout(ID);
    dec(i);
  end;

  for i:=0 to Count-1 do
    Items[i].LoadFromConfig(Config,Path);
end;

procedure TSimpleWindowLayoutList.SaveToConfig(Config: TConfigStorage;
  const Path: string);
var i: integer;
begin
  Config.SetDeleteValue(Path+'Desktop/FormIdCount',Count,0);
  //debugln(['TSimpleWindowLayoutList.SaveToConfig ',Count]);
  for i:=0 to Count-1 do begin
    Config.SetDeleteValue(Path+'Desktop/FormIdList/a'+IntToStr(i+1),Items[i].FormID,'');
    Items[i].SaveToConfig(Config,Path);
  end;
end;

function TSimpleWindowLayoutList.Count: integer;
begin
  Result:=fItems.Count;
end;

function TSimpleWindowLayoutList.ItemByForm(AForm: TCustomForm): TSimpleWindowLayout;
var i: integer;
begin
  i:=Count-1;
  while (i>=0) do begin
    Result:=Items[i];
    if Result.Form=AForm then exit;
    dec(i);
  end;
  Result:=nil;
end;

function TSimpleWindowLayoutList.ItemByFormID(const FormID: string
  ): TSimpleWindowLayout;
var i: integer;
begin
  i:=IndexOf(FormID);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TSimpleWindowLayoutList.ItemByFormCaption(const aFormCaption: string
  ): TSimpleWindowLayout;
var i: integer;
begin
  i := Count - 1;
  while i >= 0 do begin
    Result := Items[i];
    if Result.FormCaption = aFormCaption then
      exit;
    dec(i);
  end;
  Result:=nil;
end;

procedure TSimpleWindowLayoutList.CloseForm(AForm: TCustomForm);
var
  ALayout: TSimpleWindowLayout;
begin
  ALayout:=ItemByForm(AForm);
  if ALayout<>nil then
    ALayout.CloseForm;
end;

procedure TSimpleWindowLayoutList.ApplyAndShow(Sender: TObject;
  AForm: TCustomForm; BringToFront: boolean);
var
  ALayout: TSimpleWindowLayout;
  NewBounds: TRect;
  Creator: TIDEWindowCreator;
  DockSiblingName: string;
  DockAlign: TAlign;
  DockSibling: TCustomForm;
  DockSiblingBounds: TRect;
  Offset: TPoint;
  i: Integer;
begin
  {$IFDEF VerboseIDEDocking}
  debugln(['TSimpleWindowLayoutList.ApplyAndShow Form=',DbgSName(AForm),' ',BringToFront,
           '  DesktopWidth=', Screen.DesktopWidth, '  DesktopHeight=', Screen.DesktopHeight]);
  {$ENDIF}
  try
    ALayout:=ItemByFormID(AForm.Name);
    if ALayout<>nil then
    begin
      ALayout.Form:=AForm;
      if ALayout.Applied then exit;
      ALayout.Applied:=true;
      {$IFDEF VerboseIDEDocking}
      debugln(['TSimpleWindowLayoutList.ApplyAndShow restore ',
              ALayout.FormID,' ',IDEWindowPlacementNames[ALayout.WindowPlacement],
              ' Valid=',ALayout.CustomCoordinatesAreValid,' ',ALayout.Left,',',
              ALayout.Top,',',ALayout.Width,',',ALayout.Height]);
      {$ENDIF}

      case ALayout.WindowPlacement of
      iwpCustomPosition,iwpRestoreWindowGeometry:
        begin
          //DebugLn(['TMainIDE.OnApplyWindowLayout ',IDEWindowStateNames[ALayout.WindowState]]);
          case ALayout.WindowState of
          iwsMinimized: AForm.WindowState:=wsMinimized;
          iwsMaximized: AForm.WindowState:=wsMaximized;
          end;

          if (ALayout.CustomCoordinatesAreValid) then begin
            // explicit position
            NewBounds:=Bounds(ALayout.Left,ALayout.Top,ALayout.Width,ALayout.Height);
            // set minimum size
            if NewBounds.Right-NewBounds.Left<60 then
              NewBounds.Right:=NewBounds.Left+60;
            if NewBounds.Bottom-NewBounds.Top<60 then
              NewBounds.Bottom:=NewBounds.Top+60;

            // Move to visible area :
            // window is out at left side of screen
            if NewBounds.Right<Screen.DesktopLeft+60 then
              OffsetRect(NewBounds,Screen.DesktopLeft+60-NewBounds.Right,0);

            // window is out above the screen
            if NewBounds.Bottom<Screen.DesktopTop+60 then
              OffsetRect(NewBounds,0,Screen.DesktopTop+60-NewBounds.Bottom);

            // window is out at right side of screen, i = right edge of screen - 60
            i:=Screen.DesktopWidth+Screen.DesktopLeft-60;
            if NewBounds.Left > i then begin
              NewBounds.Left := i;
              NewBounds.Right := NewBounds.Right + i - NewBounds.Left;
            end;

            // window is out below the screen, i = bottom edge of screen - 60
            i:=Screen.DesktopHeight+Screen.DesktopTop-60;
            if NewBounds.Top > i then begin
              NewBounds.Top := i;
              NewBounds.Bottom := NewBounds.Bottom + i - NewBounds.Top;
            end;

            // set bounds (do not use SetRestoredBounds - that flickers with the current LCL implementation)
            AForm.SetBounds(NewBounds.Left,NewBounds.Top,
                            NewBounds.Right-NewBounds.Left,
                            NewBounds.Bottom-NewBounds.Top);
            exit;
          end;

          if ALayout.WindowState in [iwsMinimized, iwsMaximized] then
            exit;
        end;

      iwpUseWindowManagerSetting:
        begin
          exit;
        end;
      end;
    end;

    {$IFDEF VerboseIDEDocking}
    debugln(['TSimpleWindowLayoutList.ApplyAndShow no stored layout found, layout registered=',
            ALayout<>nil,' AForm=',DbgSName(AForm)]);
    {$ENDIF}

    // no layout found => use default
    Creator:=IDEWindowCreators.FindWithName(AForm.Name);
    if Creator<>nil then
    begin
      if Creator.OnGetLayout<>nil then
        Creator.OnGetLayout(Self,AForm.Name,NewBounds,DockSiblingName,DockAlign)
      else begin
        Creator.GetDefaultBounds(AForm,NewBounds);
        DockSiblingName:=Creator.DockSibling;
        DockAlign:=Creator.DockAlign;
      end;
      {$IFDEF VerboseIDEDocking}
      debugln(['TSimpleWindowLayoutList.ApplyAndShow creator found for ',DbgSName(AForm),': Left=',Creator.Left,' Top=',Creator.Top,' Right=',Creator.Right,' Bottom=',Creator.Bottom,' Creator.DockSibling=',Creator.DockSibling,' Creator.DockAlign=',dbgs(Creator.DockAlign),' NewBounds=',dbgs(NewBounds),' DockSibling=',DockSiblingName,' DockAlign=',dbgs(DockAlign)]);
      {$ENDIF}
      if DockSiblingName<>'' then
      begin
        DockSibling:=Screen.FindForm(DockSiblingName);
        if DockSibling<>nil then
        begin
          DockSiblingBounds:=DockSibling.BoundsRect;
          if DockSibling.Parent<>nil then
          begin
            Offset:=DockSibling.ClientToScreen(Point(0,0));
            OffsetRect(DockSiblingBounds,Offset.X,Offset.Y);
          end;
          case DockAlign of
          alLeft:
            begin
              NewBounds.Top:=DockSiblingBounds.Top;
              NewBounds.Bottom:=DockSiblingBounds.Bottom;
              OffsetRect(NewBounds,DockSiblingBounds.Left-6-NewBounds.Right,0);
            end;
          alRight:
            begin
              NewBounds.Top:=DockSiblingBounds.Top;
              NewBounds.Bottom:=DockSiblingBounds.Bottom;
              OffsetRect(NewBounds,DockSiblingBounds.Right+6-NewBounds.Left,0);
            end;
          alTop:
            begin
              NewBounds.Left:=DockSiblingBounds.Left;
              NewBounds.Right:=DockSiblingBounds.Right;
              OffsetRect(NewBounds,0,DockSiblingBounds.Top-25-NewBounds.Bottom);
            end;
          alBottom:
            begin
              NewBounds.Left:=DockSiblingBounds.Left;
              NewBounds.Right:=DockSiblingBounds.Right;
              OffsetRect(NewBounds,0,DockSiblingBounds.Bottom+25-NewBounds.Top);
            end;
          alClient:
            NewBounds:=DockSibling.BoundsRect;
          end;
        end;
      end;
      {$IFDEF VerboseIDEDocking}
      debugln(['TSimpleWindowLayoutList.ApplyAndShow ',DbgSName(AForm),' NewBounds=',dbgs(NewBounds)]);
      {$ENDIF}
      NewBounds.Left:=Min(10000,Max(-10000,NewBounds.Left));
      NewBounds.Top:=Min(10000,Max(-10000,NewBounds.Top));
      NewBounds.Right:=Max(NewBounds.Left+100,NewBounds.Right);
      NewBounds.Bottom:=Max(NewBounds.Top+100,NewBounds.Bottom);
      AForm.BoundsRect:=NewBounds;
    end;
  finally
    if (AForm.WindowState in [wsNormal,wsMaximized]) and BringToFront then
      AForm.ShowOnTop
    else
    begin
      AForm.Visible:=true;
      if BringToFront and (AForm.WindowState in [wsMinimized]) then
        AForm.WindowState := wsNormal;
    end;
  end;
end;

procedure TSimpleWindowLayoutList.StoreWindowPositions;
var i: integer;
begin
  for i:=0 to Count-1 do
    Items[i].GetCurrentPosition;
end;

procedure TSimpleWindowLayoutList.Assign(SrcList: TSimpleWindowLayoutList);
var i: integer;
  NewLayout: TSimpleWindowLayout;
begin
  Clear;
  if SrcList=nil then exit;
  for i:=0 to SrcList.Count-1 do begin
    NewLayout:=TSimpleWindowLayout.Create(SrcList[i].FormID);
    NewLayout.Assign(SrcList[i]);
    Add(NewLayout);
  end;
end;

function TSimpleWindowLayoutList.Add(ALayout: TSimpleWindowLayout): integer;
begin
  Result:=fItems.Add(ALayout);
end;

function TSimpleWindowLayoutList.CreateWindowLayout(const TheFormID: string
  ): TSimpleWindowLayout;
begin
  if TheFormID='' then
    raise Exception.Create('TEnvironmentOptions.CreateWindowLayout TheFormID empty');
  if ItemByFormID(TheFormID)<>nil then
    raise Exception.Create('TEnvironmentOptions.CreateWindowLayout TheFormID exists');
  Result:=TSimpleWindowLayout.Create(TheFormID);
  Add(Result);
end;

function TSimpleWindowLayoutList.CreateWindowLayout(const TheForm: TCustomForm
  ): TSimpleWindowLayout;
begin
  Result:=CreateWindowLayout(TheForm.Name);
  Result.Form:=TheForm;
end;

{ TIDEWindowCreator }

procedure TIDEWindowCreator.SetBottom(const AValue: string);
begin
  CheckBoundValue(AValue);
  if FBottom=AValue then exit;
  FBottom:=AValue;
end;

procedure TIDEWindowCreator.SetLeft(const AValue: string);
begin
  CheckBoundValue(AValue);
  if FLeft=AValue then exit;
  FLeft:=AValue;
end;

procedure TIDEWindowCreator.SetTop(const AValue: string);
begin
  CheckBoundValue(AValue);
  if FTop=AValue then exit;
  FTop:=AValue;
end;

procedure TIDEWindowCreator.SetRight(const AValue: string);
begin
  CheckBoundValue(AValue);
  if FRight=AValue then exit;
  FRight:=AValue;
end;

procedure TIDEWindowCreator.CheckBoundValue(s: string);
var
  p: Integer;
begin
  if s='' then exit;
  p:=1;
  if (p<=length(s)) and (s[p] in ['+','-']) then inc(p);
  while (p<=length(s)) and (s[p] in ['0'..'9']) do inc(p);
  if p<=1 then
    raise Exception.Create('TIDEWindowDefaultLayout.CheckBoundValue: expected number, but '+s+' found');
  // check for percent
  if (p<=length(s)) and (s[p]='%') then inc(p);
  if p<=length(s) then
    raise Exception.Create('TIDEWindowDefaultLayout.CheckBoundValue: expected number, but '+s+' found');
end;

procedure TIDEWindowCreator.GetDefaultBounds(AForm: TCustomForm; out
  DefBounds: TRect);
var
  aRight: LongInt;
  aBottom: LongInt;
  ScreenR: TRect;
  ScreenW: Integer;
  ScreenH: Integer;
begin
  ScreenR:=IDEWindowCreators.GetScreenrectForDefaults;
  ScreenW:=ScreenR.Right-ScreenR.Left;
  ScreenH:=ScreenR.Bottom-ScreenR.Top;

  // left
  if Left='' then
    DefBounds.Left:=AForm.Left
  else if Left[length(Left)]='%' then
    DefBounds.Left:=ScreenR.Left+ScreenW*StrToIntDef(copy(Left,1,length(Left)-1),0) div 100
  else
    DefBounds.Left:=ScreenR.Left+StrToIntDef(Left,0);
  // top
  if Top='' then
    DefBounds.Top:=AForm.Top
  else if Top[length(Top)]='%' then
    DefBounds.Top:=ScreenR.Top+ScreenH*StrToIntDef(copy(Top,1,length(Top)-1),0) div 100
  else
    DefBounds.Top:=ScreenR.Top+StrToIntDef(Top,0);
  // right
  if Right='' then
    aRight:=DefBounds.Left+AForm.Width
  else begin
    // 300 = fixed at 300,
    // +300 = Left+300
    // 30% = fixed at 30% on screen
    // +30% = Left+30% of screen
    // -300 = fixed 300 from right border of screen
    // -30% = fixed 30% from right border of screen
    if Right[length(Right)]='%' then
      aRight:=ScreenW*StrToIntDef(copy(Right,1,length(Right)-1),0) div 100
    else
      aRight:=StrToIntDef(Right,0);
    if aRight<0 then
      aRight:=ScreenR.Right+aRight // relative to right of screen
    else if (Right<>'') and (Right[1]='+') then
      inc(aRight,DefBounds.Left) // relative to Left
    else
      inc(aRight,ScreenR.Left); // relative to left of screen
  end;
  DefBounds.Right:=aRight;
  // bottom
  if Bottom='' then
    aBottom:=DefBounds.Top+AForm.Height
  else begin
    // 300 = fixed at 300,
    // +300 = Top+300
    // 30% = fixed at 30% on screen
    // +30% = Top+30% of screen
    // -300 = fixed 300 from bottom border of screen
    // -30% = fixed 30% from bottom border of screen
    if Bottom[length(Bottom)]='%' then
      aBottom:=ScreenH*StrToIntDef(copy(Bottom,1,length(Bottom)-1),0) div 100
    else
      aBottom:=StrToIntDef(Bottom,0);
    if aBottom<0 then
      aBottom:=ScreenR.Bottom+aBottom // relative to bottom of screen
    else if (Bottom<>'') and (Bottom[1]='+') then
      inc(aBottom,DefBounds.Top) // relative to Top
    else
      inc(aBottom,ScreenR.Top); // relative to top of screen
  end;
  DefBounds.Bottom:=aBottom;
end;

constructor TIDEWindowCreator.Create(aFormName: string);
begin
  FFormName:=aFormName;
end;

constructor TIDEWindowCreator.Create(aFormName: string;
  CreateFormProc: TCreateIDEWindowProc;
  CreateFormMethod: TCreateIDEWindowMethod;
  aLeft, aTop, aRight, aBottom: string; aDockSibling: string; aDockAlign: TAlign; aMulti: boolean;
  GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent);
begin
  Create(aFormName);
  FMulti:=aMulti;
  Left:=aLeft;
  Top:=aTop;
  Right:=aRight;
  Bottom:=aBottom;
  DockSibling:=aDockSibling;
  DockAlign:=aDockAlign;
  OnCreateFormMethod:=CreateFormMethod;
  OnCreateFormProc:=CreateFormProc;
  OnGetLayout:=GetLayoutEvent;
end;

function TIDEWindowCreator.NameFits(const AName: string): boolean;
begin
  Result:=CompareText(copy(AName,1,Length(FormName)),FormName)=0;
end;

{ TIDEWindowCreatorList }

function TIDEWindowCreatorList.GetItems(Index: integer
  ): TIDEWindowCreator;
begin
  Result:=TIDEWindowCreator(fItems[Index]);
end;

procedure TIDEWindowCreatorList.ErrorIfFormExists(FormName: string);
begin
  if IndexOfName(FormName)>=0 then
    raise Exception.Create('TIDEWindowDefaultLayoutList.Add: form name '+FormName+' already exists');
end;

constructor TIDEWindowCreatorList.Create;
begin
  fItems:=TFPList.Create;
  FSimpleLayoutStorage:=TSimpleWindowLayoutList.Create;
  FScreenMaxSizeForDefaults:=Point(1200,900);
end;

destructor TIDEWindowCreatorList.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
  FreeAndNil(FSimpleLayoutStorage);
  inherited Destroy;
end;

procedure TIDEWindowCreatorList.Clear;
var
  i: Integer;
begin
  for i:=0 to fItems.Count-1 do
    TObject(fItems[i]).Free;
end;

function TIDEWindowCreatorList.Count: integer;
begin
  Result:=fItems.Count;
end;

function TIDEWindowCreatorList.Add(aLayout: TIDEWindowCreator
  ): integer;
begin
  ErrorIfFormExists(aLayout.FormName);
  Result:=fItems.Add(aLayout);
end;

function TIDEWindowCreatorList.Add(aFormName: string
  ): TIDEWindowCreator;
begin
  ErrorIfFormExists(aFormName);
  Result:=TIDEWindowCreator.Create(aFormName);
  Add(Result);
end;

function TIDEWindowCreatorList.Add(aFormName: string;
  CreateFormProc: TCreateIDEWindowProc;
  CreateFormMethod: TCreateIDEWindowMethod;
  aLeft, aTop, aRight, aBottom: string;
  aDockSibling: string; aDockAlign: TAlign; aMulti: boolean;
  GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent): TIDEWindowCreator;
begin
  ErrorIfFormExists(aFormName);
  Result:=TIDEWindowCreator.Create(aFormName,CreateFormProc,CreateFormMethod,
       aLeft,aTop,aRight,aBottom,aDockSibling,aDockAlign,aMulti,GetLayoutEvent);
  Add(Result);
end;

procedure TIDEWindowCreatorList.Delete(Index: integer);
begin
  TObject(fItems[Index]).Free;
  fItems.Delete(Index);
end;

function TIDEWindowCreatorList.IndexOfName(FormName: string): integer;
begin
  Result:=Count-1;
  while (Result>=0) and not Items[Result].NameFits(FormName) do
    dec(Result);
end;

function TIDEWindowCreatorList.FindWithName(FormName: string): TIDEWindowCreator;
var
  i: LongInt;
begin
  i:=IndexOfName(FormName);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TIDEWindowCreatorList.GetForm(aFormName: string; AutoCreate: boolean;
  DisableAutoSizing: boolean): TCustomForm;
var
  Item: TIDEWindowCreator;
begin
  Result:=Screen.FindForm(aFormName);
  if Result<>nil then begin
    if DisableAutoSizing then
      Result.DisableAutoSizing;
    exit;
  end;
  if AutoCreate then begin
    Item:=FindWithName(aFormName);
    if Item=nil then begin
      debugln(['TIDEWindowCreatorList.GetForm no creator for ',aFormName]);
      exit;
    end;
    if Assigned(Item.OnCreateFormProc) then begin
      Item.OnCreateFormProc(Self,aFormName,Result,DisableAutoSizing);
    end else if Assigned(Item.OnCreateFormMethod) then begin
      Item.OnCreateFormMethod(Self,aFormName,Result,DisableAutoSizing);
    end else begin
      debugln(['TIDEWindowCreatorList.GetForm no OnCreateForm for ',aFormName]);
      exit;
    end;
    if Result=nil then begin
      debugln(['TIDEWindowCreatorList.GetForm create failed for ',aFormName]);
      exit;
    end;
  end;
end;

procedure TIDEWindowCreatorList.ShowForm(AForm: TCustomForm; BringToFront: boolean);
var
  Layout: TSimpleWindowLayout;
begin
  if (AForm.Name='') or (not IsValidIdent(AForm.Name)) then
    raise Exception.Create('TIDEWindowCreatorList.ShowForm invalid form name '+AForm.Name);

  // auto create a layput storage for every shown form
  Layout:=SimpleLayoutStorage.ItemByFormID(AForm.Name);
  if Layout=nil then begin
    if not (csDesigning in AForm.ComponentState) then
      SimpleLayoutStorage.CreateWindowLayout(AForm);
  end
  else
    Layout.Form:=AForm;

  if (IDEDockMaster<>nil) and (not (csDesigning in AForm.ComponentState))
  and (FindWithName(AForm.Name)<>nil) then
    // show dockable if it has a creator and is not a designer form
    IDEDockMaster.ShowForm(AForm,BringToFront)
  else
    SimpleLayoutStorage.ApplyAndShow(Self,AForm,BringToFront);
end;

function TIDEWindowCreatorList.ShowForm(AFormName: string; BringToFront: boolean
  ): TCustomForm;
begin
  Result:=GetForm(AFormName,true,false);
  if Result<>nil then
    ShowForm(Result,BringToFront);
end;

procedure TIDEWindowCreatorList.CreateForm(var AForm;
  AFormClass: TCustomFormClass; DoDisableAutoSizing: boolean;
  TheOwner: TComponent);
begin
  if TCustomForm(AForm)=nil then begin
    TCustomForm(AForm):=TCustomForm(AFormClass.NewInstance);
    TCustomForm(AForm).DisableAutoSizing;
    TCustomForm(AForm).Create(TheOwner);
    if not DoDisableAutoSizing then
      TCustomForm(AForm).EnableAutoSizing;
  end else if DoDisableAutoSizing then
    TCustomForm(AForm).DisableAutoSizing;
end;

procedure TIDEWindowCreatorList.RestoreSimpleLayout;
var
  i: Integer;
  ALayout: TSimpleWindowLayout;
  AForm: TCustomForm;
begin
  for i:=0 to SimpleLayoutStorage.Count-1 do begin
    ALayout:=SimpleLayoutStorage[i];
    if not ALayout.Visible then continue;
    AForm:=GetForm(ALayout.FormID,true);
    if AForm=nil then continue;
    ShowForm(AForm,false);
  end;
end;

function TIDEWindowCreatorList.GetScreenrectForDefaults: TRect;
begin
  Result:=Screen.WorkAreaRect;
  if (Result.Right-Result.Left<10)
  or (Result.Bottom-Result.Top<10) then begin
    // screen not recognized
    Result:=Rect(0,0,Max(Screen.Width,600),Max(Screen.Height,400));
  end;
  Result.Right:=Min(Result.Right,Result.Left+IDEWindowCreators.ScreenMaxSizeForDefaults.X);
  Result.Bottom:=Min(Result.Bottom,Result.Top+IDEWindowCreators.ScreenMaxSizeForDefaults.Y);
end;

{ TIDEDockMaster }

function TIDEDockMaster.AddableInWindowMenu(AForm: TCustomForm): boolean;
begin
  Result:=true;
end;

procedure TIDEDockMaster.CloseAll;
begin
  CloseAllForms;
end;

initialization
  IDEWindowCreators:=TIDEWindowCreatorList.Create;
finalization
  FreeAndNil(IDEWindowCreators);

end.

