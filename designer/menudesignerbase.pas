unit MenuDesignerBase;

{$mode objfpc}{$H+}

interface

uses
  // FCL + LCL
  Classes, SysUtils,
  Controls, Forms, Menus, LCLProc,
  // IdeIntf
  PropEdits,
  // IDE
  MenuShortcuts, MenuTemplates;

type

  { TShadowItemBase }

  TShadowItemBase = class(TCustomControl)
  private
  protected
    FRealItem: TMenuItem;
  public
    constructor Create(AOwner: TComponent; aRealItem: TMenuItem); reintroduce;
    destructor Destroy; override;
  public
    property RealItem: TMenuItem read FRealItem write FRealItem;
  end;

  { TShadowBoxBase }

  TShadowBoxBase = class(TCustomControl)
  private
    function GetHasRadioItems: boolean;
    function GetRadioGroupsString: string;
  protected
    FLevel: integer;
    FLastRIValue: boolean;
    FParentMenuItem: TMenuItem;
    FShadowList: TFPList;
  public
    constructor Create(AOwner: TComponent; aParentItem: TMenuItem); reintroduce;
    destructor Destroy; override;
  public
    property Level: integer read FLevel;
    property LastRIValue: boolean read FLastRIValue write FLastRIValue;
    property ParentMenuItem: TMenuItem read FParentMenuItem;
    property ShadowList: TFPList read FShadowList;
    property HasRadioItems: boolean read GetHasRadioItems;
    property RadioGroupsString: string read GetRadioGroupsString;
  end;

  { TShadowMenuBase }

  TShadowMenuBase = class(TScrollBox)
  private
  protected
    FSelectedMenuItem: TMenuItem;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetSelectedMenuItem(aMI: TMenuItem;
      viaDesigner, prevWasDeleted: boolean); virtual; abstract;
  public
    property SelectedMenuItem: TMenuItem read FSelectedMenuItem write FSelectedMenuItem;
  end;

  { TMenuDesignerBase }

  TMenuDesignerBase = class
  private
  protected
    FShadowMenu: TShadowMenuBase;
    FShortcuts: TMenuShortcuts;
    FTemplatesSaved: boolean;
    FSavedTemplatesCount: integer;
    FTotalMenuItemsCount: integer;
    FVariableGlyphsInMenuBar: boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateShadowMenu(aMenu: TMenu; aSelect: TMenuItem;
      aWidth, aHeight: integer); virtual; abstract;
    procedure FreeShadowMenu;
    procedure UpdateTemplatesCount;
  public
    property ShadowMenu: TShadowMenuBase read FShadowMenu write FShadowMenu;
    property Shortcuts: TMenuShortcuts read FShortcuts;
    property TemplatesSaved: boolean read FTemplatesSaved;
    property TotalMenuItemsCount: integer read FTotalMenuItemsCount
                                         write FTotalMenuItemsCount;
    property VariableGlyphsInMenuBar: boolean read FVariableGlyphsInMenuBar
                                             write FVariableGlyphsInMenuBar;
    property SavedTemplatesCount: integer read FSavedTemplatesCount;
  end;


implementation

{ TShadowItemBase }

constructor TShadowItemBase.Create(AOwner: TComponent; aRealItem: TMenuItem);
begin
  inherited Create(AOwner);
  FRealItem:=aRealItem;
end;

destructor TShadowItemBase.Destroy;
begin
  inherited Destroy;
end;

{ TShadowBoxBase }

constructor TShadowBoxBase.Create(AOwner: TComponent; aParentItem: TMenuItem);
begin
  inherited Create(AOwner);
  Assert(aParentItem<>nil,'TShadowBox.CreateWithParentBox: aParentItem parameter is nil');
  FParentMenuItem:=aParentItem;
  FShadowList:=TFPList.Create;
end;

destructor TShadowBoxBase.Destroy;
begin
  FreeAndNil(FShadowList);
  inherited Destroy;
end;

function TShadowBoxBase.GetHasRadioItems: boolean;
var
  p: pointer;
  si: TShadowItemBase absolute p;
begin
  for p in FShadowList do
    if si.RealItem.RadioItem then
      Exit(True);
  Result:=False;
end;

function TShadowBoxBase.GetRadioGroupsString: string;
var
  rgSet: set of byte = [];
  g: byte;
  p: pointer;
  si: TShadowItemBase absolute p;
  mi: TMenuItem;
begin
  Result:='';
  for p in FShadowList do begin
    mi:=si.RealItem;
    if mi.RadioItem then begin
      g:=mi.GroupIndex;
      if not (g in rgSet) then begin
        Include(rgSet, g);
        AppendStr(Result, IntToStr(g) + ', ');
      end;
    end;
  end;
  Delete(Result, Pred(Length(Result)), 2);
end;

{ TShadowMenuBase }

constructor TShadowMenuBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TShadowMenuBase.Destroy;
begin
  inherited Destroy;
end;

{ TMenuDesignerBase }

constructor TMenuDesignerBase.Create;
begin
  FShortcuts:=TMenuShortcuts.Create;
  FShortcuts.Initialize;
  FTemplatesSaved:=SavedTemplatesExist;
end;

destructor TMenuDesignerBase.Destroy;
begin
  if (FShadowMenu <> nil) then begin
    FShadowMenu.Parent:=nil;
    FreeAndNil(FShadowMenu);
  end;
  FreeAndNil(FShortcuts);
  inherited Destroy;
end;

procedure TMenuDesignerBase.FreeShadowMenu;
begin
  if FShadowMenu <> nil then
    FShadowMenu.SelectedMenuItem:=nil;
  if Assigned(GlobalDesignHook) then
    GlobalDesignHook.RemoveAllHandlersForObject(FShadowMenu);
  FreeAndNil(FShadowMenu);
end;

procedure TMenuDesignerBase.UpdateTemplatesCount;
begin
  FTemplatesSaved:=SavedTemplatesExist;
  DebugLn('SavedTemplatesExist is %s',[booltostr(FTemplatesSaved)]);
  FSavedTemplatesCount:=GetSavedTemplatesCount;
end;

end.

