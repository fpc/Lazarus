unit MenuDesignerBase;

{$mode objfpc}{$H+}

interface

uses
  // FCL + LCL
  Classes, SysUtils, fgl,
  Controls, Forms, Menus, Graphics, LCLProc,
  // IdeIntf
  FormEditingIntf, ComponentEditors,
  // IDE
  MenuShortcuts, MenuTemplates;

type

  TShadowItemDisplayState = (dsNormal, dsSelected, dsDisabled);
  TByteArray = Array of Byte;

  { TShadowItemBase }

  TShadowItemBase = class(TCustomControl)
  private
  protected
    FRealItem: TMenuItem;
    FState: TShadowItemDisplayState;
  public
    constructor Create(AOwner: TComponent; aRealItem: TMenuItem); reintroduce;
    destructor Destroy; override;
    function GetHeight: integer;
    function GetWidth: integer; virtual; abstract;
    procedure ShowDisabled;
    procedure ShowNormal;
    procedure ShowSelected;
  public
    property RealItem: TMenuItem read FRealItem write FRealItem;
  end;

  TShadowItemList = specialize TFPGList<TShadowItemBase>;

  { TShadowBoxBase }

  TShadowBoxBase = class(TCustomControl)
  private
    function GetRadioGroupValues: TByteArray;
  protected
    FLevel: integer;
    FLastRIValue: boolean;
    FParentBox: TShadowBoxBase;
    FParentMenuItem: TMenuItem;
    FShadowList: TShadowItemList;
    function GetIsMainMenu: boolean; virtual; abstract;
    function GetIsMenuBar: boolean; virtual; abstract;
  public
    constructor Create(AOwner: TComponent; aParentItem: TMenuItem); reintroduce;
    destructor Destroy; override;
  public
    function GetInnerDims: TPoint;
    property IsMainMenu: boolean read GetIsMainMenu;
    property IsMenuBar: boolean read GetIsMenuBar;
    property Level: integer read FLevel;
    property LastRIValue: boolean read FLastRIValue write FLastRIValue;
    property ParentMenuItem: TMenuItem read FParentMenuItem;
    property ParentBox: TShadowBoxBase read FParentBox;
    property ShadowList: TShadowItemList read FShadowList;
    property RadioGroupValues: TByteArray read GetRadioGroupValues;
  end;

  TShadowBoxList = specialize TFPGList<TShadowBoxBase>;

  { TShadowMenuBase }

  TShadowMenuBase = class(TScrollBox)
  private
  protected
    FEditorDesigner: TComponentEditorDesigner;
    FLookupRoot: TComponent;
    FMainCanvas: TCanvas;
    FMenu: TMenu;
    FSelectedMenuItem: TMenuItem;
    FBoxList: TShadowBoxList;
    function GetStringWidth(const aText: string; isBold: boolean): integer;
  public
    constructor Create(AOwner: TComponent; aMenu: TMenu); reintroduce;
    destructor Destroy; override;
    procedure RefreshFakes; virtual; abstract;
    procedure SetSelectedMenuItem(aMI: TMenuItem;
      viaDesigner, prevWasDeleted: boolean); virtual; abstract;
    procedure UpdateBoxLocationsAndSizes; virtual; abstract;
    function GetParentBoxForMenuItem(aMI: TMenuItem): TShadowBoxBase;
    function GetShadowForMenuItem(aMI: TMenuItem): TShadowItemBase;
    function IsMainMenu: boolean;
  public
    property EditorDesigner: TComponentEditorDesigner read FEditorDesigner;
    property LookupRoot: TComponent read FLookupRoot;
    property SelectedMenuItem: TMenuItem read FSelectedMenuItem write FSelectedMenuItem;
    property BoxList: TShadowBoxList read FBoxList;
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

function TShadowItemBase.GetHeight: integer;
begin
  if FRealItem.IsInMenuBar then
    Result:=MenuBar_Height
  else if FRealItem.IsLine then
    Result:=Separator_Height
  else
    Result:=DropDown_Height;
end;

procedure TShadowItemBase.ShowDisabled;
begin
  if (FState <> dsDisabled) then begin
    FState:=dsDisabled;
    Invalidate;
  end;
end;

procedure TShadowItemBase.ShowNormal;
begin
  if (FState <> dsNormal) then begin
    FState:=dsNormal;
    Invalidate;
  end;
end;

procedure TShadowItemBase.ShowSelected;
begin
  if (FState <> dsSelected) then begin
    FState:=dsSelected;
    Invalidate;
  end;
end;

{ TShadowBoxBase }

constructor TShadowBoxBase.Create(AOwner: TComponent; aParentItem: TMenuItem);
begin
  inherited Create(AOwner);
  Assert(aParentItem<>nil,'TShadowBox.CreateWithParentBox: aParentItem parameter is nil');
  FParentMenuItem:=aParentItem;
  FShadowList:=TShadowItemList.Create;
end;

destructor TShadowBoxBase.Destroy;
begin
  FreeAndNil(FShadowList);
  inherited Destroy;
end;

function TShadowBoxBase.GetRadioGroupValues: TByteArray;
var
  rgSet: set of byte = [];
  g: byte;
  si: TShadowItemBase;
  mi: TMenuItem;
begin
  SetLength(Result, 0);
  for si in FShadowList do
  begin
    mi:=si.RealItem;
    if mi.RadioItem then begin
      g:=mi.GroupIndex;
      if not (g in rgSet) then begin
        Include(rgSet, g);
        SetLength(Result, Length(Result)+1);
        Result[Length(Result)-1] := g;
      end;
    end;
  end;
end;

function TShadowBoxBase.GetInnerDims: TPoint;
var
  si: TShadowItemBase;
  w: integer;
begin
  FillChar(Result{%H-}, SizeOf(Result), 0);
  for si in FShadowList do begin
    Inc(Result.y, si.GetHeight);
    w:=si.GetWidth;
    if (Result.x < w) then
      Result.x:=w;
  end;
end;

{ TShadowMenuBase }

constructor TShadowMenuBase.Create(AOwner: TComponent; aMenu: TMenu);
begin
  inherited Create(AOwner);
  FMenu := aMenu;
  FEditorDesigner := FindRootDesigner(FMenu) as TComponentEditorDesigner;
  FLookupRoot := FEditorDesigner.LookupRoot;
  FBoxList := TShadowBoxList.Create;
end;

destructor TShadowMenuBase.Destroy;
begin
  FEditorDesigner:=nil;
  FreeAndNil(FBoxList);
  inherited Destroy;
end;

function TShadowMenuBase.GetStringWidth(const aText: string; isBold: boolean): integer;
begin
  if isBold then
    FMainCanvas.Font.Style:=[fsBold]
  else
    FMainCanvas.Font.Style:=[];
  Result:=FMainCanvas.TextWidth(aText);
end;

function TShadowMenuBase.GetParentBoxForMenuItem(aMI: TMenuItem): TShadowBoxBase;
var
  sb: TShadowBoxBase;
  si: TShadowItemBase;
begin
  for sb in FBoxList do
    for si in sb.ShadowList do
      if si.RealItem = aMI then
        Exit(sb);
  Result:=nil;
end;

function TShadowMenuBase.GetShadowForMenuItem(aMI: TMenuItem): TShadowItemBase;
var
  sb: TShadowBoxBase;
  si: TShadowItemBase;
begin
  for sb in FBoxList do
    for si in sb.ShadowList do
      if si.RealItem = aMI then
        Exit(si);
  Result:=nil;
end;

function TShadowMenuBase.IsMainMenu: boolean;
begin
  Result := FMenu is TMainMenu;
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
  FreeShadowMenu;
  FreeAndNil(FShortcuts);
  inherited Destroy;
end;

procedure TMenuDesignerBase.FreeShadowMenu;
begin
  if FShadowMenu=nil then exit;
  FShadowMenu.Parent:=nil;
  Application.ReleaseComponent(FShadowMenu);
  FShadowMenu:=nil;
end;

procedure TMenuDesignerBase.UpdateTemplatesCount;
begin
  FTemplatesSaved:=SavedTemplatesExist;
  DebugLn('Hint: (lazarus) [TMenuDesignerBase.UpdateTemplatesCount] SavedTemplatesExist is %s',[booltostr(FTemplatesSaved)]);
  FSavedTemplatesCount:=GetSavedTemplatesCount;
end;

end.

