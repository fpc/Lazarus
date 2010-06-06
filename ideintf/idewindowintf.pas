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
  Classes, SysUtils, LCLProc, LazConfigStorage, Forms, Controls;

  //----------------------------------------------------------------------------
  // layout settings of modal forms (dialogs) in the IDE
type

  TIDEDialogLayoutList = class;

  { TIDEDialogLayout - for modal forms
    For non modal forms see TIDEWindowCreator below }

  TIDEDialogLayout = class
  private
    FHeight: integer;
    FList: TIDEDialogLayoutList;
    FModified: boolean;
    FName: string;
    FWidth: integer;
    procedure SetHeight(const AValue: integer);
    procedure SetList(const AValue: TIDEDialogLayoutList);
    procedure SetModified(const AValue: boolean);
    procedure SetWidth(const AValue: integer);
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
  TIWCState = (
    iwcsHidden,
    iwcsIconified,
    iwcsNormal,
    iwcsDocked
    );

  TCreateIDEWindowEvent = procedure(Sender: TObject; aFormName: string;
                                    var AForm: TCustomForm) of object;
  TGetDefaultIDEWindowLayoutEvent = procedure(Sender: TObject; aFormName: string;
   out aBounds: TRect; out DockSibling: string; out DockAlign: TAlign) of object;
  TShowIDEWindowEvent = procedure(Sender: TObject; AForm: TCustomForm;
                                  BringToFront: boolean) of object;

  { TIDEWindowCreator }

  TIDEWindowCreator = class
  private
    FCreateForm: TCreateIDEWindowEvent;
    FDockAlign: TAlign;
    FDockSibling: string;
    FFormName: string;
    FHeight: string;
    FLeft: string;
    FMulti: boolean;
    FOnGetLayout: TGetDefaultIDEWindowLayoutEvent;
    FState: TIWCState;
    FTop: string;
    FWidth: string;
    procedure SetHeight(const AValue: string);
    procedure SetLeft(const AValue: string);
    procedure SetTop(const AValue: string);
    procedure SetWidth(const AValue: string);
  public
    constructor Create(aFormName: string); overload;
    constructor Create(aFormName: string;
                       CreateFormEvent: TCreateIDEWindowEvent;
                       aLeft, aTop, aWidth, aHeight: string;
                       aDockSibling : string = '';
                       aDockAlign: TAlign = alNone;
                       aMulti: boolean = false;
               GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent = nil); overload;
    property FormName: string read FFormName; // prefix for all forms
    property Multi: boolean read FMulti; // there can be more than one of this form, e.g. the source editors and the package editors
    property State: TIWCState read FState write FState;
    property Left: string read FLeft write SetLeft; // '12' for 12 pixel, '10%' for 10 percent of screen.width
    property Top: string read FTop write SetTop; // '12' for 12 pixel, '10%' for 10 percent of screen.height
    property Width: string read FWidth write SetWidth; // '12' for 12 pixel, '10%' for 10 percent of screen.width
    property Height: string read FHeight write SetHeight; // '12' for 12 pixel, '10%' for 10 percent of screen.height
    property DockSibling: string read FDockSibling write FDockSibling; // another form name
    property DockAlign: TAlign read FDockAlign write FDockAlign;
    property OnCreateForm: TCreateIDEWindowEvent read FCreateForm write FCreateForm;
    property OnGetLayout: TGetDefaultIDEWindowLayoutEvent read FOnGetLayout
                                                          write FOnGetLayout;
    procedure CheckBoundValue(s: string);
    procedure GetDefaultBounds(AForm: TCustomForm; out DefBounds: TRect);
  end;

  { TIDEWindowCreatorList }

  TIDEWindowCreatorList = class
  private
    fItems: TFPList; // list of TIDEWindowCreator
    FOnShowForm: TShowIDEWindowEvent;
    function GetItems(Index: integer): TIDEWindowCreator;
    procedure ErrorIfFormExists(FormName: string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Count: integer;
    property Items[Index: integer]: TIDEWindowCreator read GetItems;
    function Add(aLayout: TIDEWindowCreator): integer; overload;
    function Add(aFormName: string): TIDEWindowCreator; overload;
    function Add(aFormName: string;
                 CreateFormEvent: TCreateIDEWindowEvent;
                 aLeft, aTop, aWidth, aHeight: string;
                 aDockSibling : string = '';
                 aDockAlign: TAlign = alNone;
                 aMulti: boolean = false;
                 GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent = nil
                 ): TIDEWindowCreator; overload;
    procedure Delete(Index: integer);
    function IndexOfName(FormName: string): integer;
    function FindWithName(FormName: string): TIDEWindowCreator;
    function GetForm(aFormName: string; AutoCreate: boolean): TCustomForm;
    procedure ShowForm(AForm: TCustomForm; BringToFront: boolean);
    property OnShowForm: TShowIDEWindowEvent read FOnShowForm write FOnShowForm;
  end;

var
  IDEWindowCreators: TIDEWindowCreatorList = nil; // set by the IDE

type

  { TIDEDockMaster }

  TIDEDockMaster = class
  public
    // ToDo: save/restore layout
    procedure MakeIDEWindowDockable(AControl: TWinControl); virtual; abstract;
    procedure MakeIDEWindowDockSite(AForm: TCustomForm); virtual; abstract;
    procedure LoadDefaultLayout; virtual; abstract; // called before opening the first project
    procedure ShowForm(AForm: TCustomForm; BringToFront: boolean); virtual; abstract;
  end;

var
  IDEDockMaster: TIDEDockMaster = nil; // can be set by a package

procedure MakeIDEWindowDockable(AControl: TWinControl);
procedure MakeIDEWindowDockSite(AForm: TCustomForm);

procedure Register;

implementation

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

procedure Register;
begin
  RegisterComponents('Misc',[TIDEDialogLayoutStorage]);
end;

{ TIDEDialogLayout }

procedure TIDEDialogLayout.SetHeight(const AValue: integer);
begin
  if FHeight=AValue then exit;
  FHeight:=AValue;
  Modified:=true;
end;

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

procedure TIDEDialogLayoutList.LoadFromConfig(Config: TConfigStorage;
  const Path: string);
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

procedure TIDEDialogLayoutList.SaveToConfig(Config: TConfigStorage;
  const Path: string);
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

{ TIDEWindowCreator }

procedure TIDEWindowCreator.SetHeight(const AValue: string);
begin
  CheckBoundValue(AValue);
  if FHeight=AValue then exit;
  FHeight:=AValue;
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

procedure TIDEWindowCreator.SetWidth(const AValue: string);
begin
  CheckBoundValue(AValue);
  if FWidth=AValue then exit;
  FWidth:=AValue;
end;

procedure TIDEWindowCreator.CheckBoundValue(s: string);
var
  p: Integer;
begin
  if s='' then exit;
  p:=1;
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
  aWidth: LongInt;
  aHeight: LongInt;
begin
  // left
  if Left='' then
    DefBounds.Left:=AForm.Left
  else if Left[length(Left)]='%' then
    DefBounds.Left:=Screen.Width*StrToIntDef(copy(Left,1,length(Left)-1),0) div 100
  else
    DefBounds.Left:=StrToIntDef(Left,0);
  // top
  if Top='' then
    DefBounds.Top:=AForm.Top
  else if Top[length(Top)]='%' then
    DefBounds.Top:=Screen.Height*StrToIntDef(copy(Top,1,length(Top)-1),0) div 100
  else
    DefBounds.Top:=StrToIntDef(Top,0);
  // width
  if Width='' then
    aWidth:=AForm.Width
  else if Width[length(Width)]='%' then
    aWidth:=Screen.Width*StrToIntDef(copy(Width,1,length(Width)-1),0) div 100
  else
    aWidth:=StrToIntDef(Width,0);
  DefBounds.Right:=DefBounds.Left+aWidth;
  // height
  if Height='' then
    aHeight:=AForm.Height
  else if Height[length(Height)]='%' then
    aHeight:=Screen.Height*StrToIntDef(copy(Height,1,length(Height)-1),0) div 100
  else
    aHeight:=StrToIntDef(Height,0);
  DefBounds.Bottom:=DefBounds.Top+aHeight;
end;

constructor TIDEWindowCreator.Create(aFormName: string);
begin
  FFormName:=aFormName;
end;

constructor TIDEWindowCreator.Create(aFormName: string;
  CreateFormEvent: TCreateIDEWindowEvent; aLeft, aTop, aWidth, aHeight: string;
  aDockSibling: string; aDockAlign: TAlign;
  aMulti: boolean; GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent);
begin
  Create(aFormName);
  FMulti:=aMulti;
  Left:=aLeft;
  Top:=aTop;
  Width:=aWidth;
  Height:=aHeight;
  DockSibling:=aDockSibling;
  DockAlign:=aDockAlign;
  OnCreateForm:=CreateFormEvent;
  OnGetLayout:=GetLayoutEvent;
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
end;

destructor TIDEWindowCreatorList.Destroy;
begin
  Clear;
  FreeAndNil(fItems);
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
  CreateFormEvent: TCreateIDEWindowEvent; aLeft, aTop, aWidth, aHeight: string;
  aDockSibling: string; aDockAlign: TAlign;
  aMulti: boolean;
  GetLayoutEvent: TGetDefaultIDEWindowLayoutEvent): TIDEWindowCreator;
begin
  ErrorIfFormExists(aFormName);
  Result:=TIDEWindowCreator.Create(aFormName,CreateFormEvent,
       aLeft,aTop,aWidth,aHeight,aDockSibling,aDockAlign,aMulti,GetLayoutEvent);
  Add(Result);
end;

procedure TIDEWindowCreatorList.Delete(Index: integer);
begin
  TObject(fItems[Index]).Free;
  fItems.Delete(Index);
end;

function TIDEWindowCreatorList.IndexOfName(FormName: string): integer;
var
  Item: TIDEWindowCreator;
begin
  Result:=Count-1;
  while (Result>=0) do begin
    Item:=Items[Result];
    if (SysUtils.CompareText(copy(FormName,1,length(Item.FormName)),Item.FormName)=0)
    then
      exit;
    dec(Result);
  end;
end;

function TIDEWindowCreatorList.FindWithName(FormName: string
  ): TIDEWindowCreator;
var
  i: LongInt;
begin
  i:=IndexOfName(FormName);
  if i>=0 then
    Result:=Items[i]
  else
    Result:=nil;
end;

function TIDEWindowCreatorList.GetForm(aFormName: string; AutoCreate: boolean
  ): TCustomForm;
var
  Item: TIDEWindowCreator;
begin
  Result:=Screen.FindForm(aFormName);
  if Result<>nil then exit;
  if AutoCreate then begin
    Item:=FindWithName(aFormName);
    if Item=nil then begin
      debugln(['TIDEWindowCreatorList.GetForm no creator for ',aFormName]);
      exit;
    end;
    if Item.OnCreateForm=nil then begin
      debugln(['TIDEWindowCreatorList.GetForm no OnCreateForm for ',aFormName]);
      exit;
    end;
    Item.OnCreateForm(Self,aFormName,Result);
    if Result=nil then begin
      debugln(['TIDEWindowCreatorList.GetForm create failed for ',aFormName]);
      exit;
    end;
  end;
end;

procedure TIDEWindowCreatorList.ShowForm(AForm: TCustomForm;
  BringToFront: boolean);
begin
  if AForm.IsVisible then
  begin
    if BringToFront then
      AForm.ShowOnTop;
    exit;
  end;
  if IDEDockMaster<>nil then
    IDEDockMaster.ShowForm(AForm,BringToFront)
  else if Assigned(OnShowForm) then
    OnShowForm(Self,AForm,BringToFront)
  else if BringToFront then
    AForm.ShowOnTop
  else
    AForm.Show;
end;

initialization
  IDEWindowCreators:=TIDEWindowCreatorList.Create;
finalization
  FreeAndNil(IDEWindowCreators);

end.

