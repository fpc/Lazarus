unit MenuDesignerBase;

{$mode objfpc}{$H+}

interface

uses
  // FCL + LCL
  Classes, SysUtils,
  Controls, Forms, Menus, LCLProc,
  // IdeIntf
  FormEditingIntf, ComponentEditors, PropEdits,
  // IDE
  MenuShortcuts, MenuTemplates;

type

  TByteArray = Array of Byte;

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
    function GetRadioGroupValues: TByteArray;
  protected
    FLevel: integer;
    FLastRIValue: boolean;
    FParentMenuItem: TMenuItem;
    FShadowList: TFPList;
    function GetShadowCount: integer;
  public
    constructor Create(AOwner: TComponent; aParentItem: TMenuItem); reintroduce;
    destructor Destroy; override;
  public
    property Level: integer read FLevel;
    property LastRIValue: boolean read FLastRIValue write FLastRIValue;
    property ParentMenuItem: TMenuItem read FParentMenuItem;
    property ShadowList: TFPList read FShadowList;
    property ShadowCount: integer read GetShadowCount;
    property RadioGroupValues: TByteArray read GetRadioGroupValues;
  end;

  { TShadowMenuBase }

  TShadowMenuBase = class(TScrollBox)
  private
  protected
    FEditorDesigner: TComponentEditorDesigner;
    FLookupRoot: TComponent;
    FMenu: TMenu;
    FSelectedMenuItem: TMenuItem;
    FBoxList: TFPList;
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
    property BoxList: TFPList read FBoxList;
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

function TShadowBoxBase.GetRadioGroupValues: TByteArray;
var
  rgSet: set of byte = [];
  g: byte;
  p: pointer;
  si: TShadowItemBase absolute p;
  mi: TMenuItem;
begin
  SetLength(Result, 0);
  for p in FShadowList do
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

function TShadowBoxBase.GetShadowCount: integer;
begin
  Result:=FShadowList.Count;
end;

{ TShadowMenuBase }

constructor TShadowMenuBase.Create(AOwner: TComponent; aMenu: TMenu);
begin
  inherited Create(AOwner);
  FMenu := aMenu;
  FEditorDesigner := FindRootDesigner(FMenu) as TComponentEditorDesigner;
  FLookupRoot := FEditorDesigner.LookupRoot;
  FBoxList := TFPList.Create;
end;

destructor TShadowMenuBase.Destroy;
begin
  FEditorDesigner:=nil;
  FreeAndNil(FBoxList);
  inherited Destroy;
end;

function TShadowMenuBase.GetParentBoxForMenuItem(aMI: TMenuItem): TShadowBoxBase;
var
  p: pointer;
  sb: TShadowBoxBase absolute p;
  ps: pointer;
  si: TShadowItemBase absolute ps;
begin
  for p in FBoxList do
    for ps in sb.ShadowList do
      if (si.RealItem = aMI) then
        Exit(sb);
  Result:=nil;
end;

function TShadowMenuBase.GetShadowForMenuItem(aMI: TMenuItem): TShadowItemBase;
var
  p: pointer;
  sb: TShadowBoxBase absolute p;
  ps: pointer;
  si: TShadowItemBase absolute ps;
begin
  for p in FBoxList do
    for ps in sb.ShadowList do
      if (si.RealItem = aMI) then
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
  FreeAndNil(FShadowMenu);
end;

procedure TMenuDesignerBase.UpdateTemplatesCount;
begin
  FTemplatesSaved:=SavedTemplatesExist;
  DebugLn('SavedTemplatesExist is %s',[booltostr(FTemplatesSaved)]);
  FSavedTemplatesCount:=GetSavedTemplatesCount;
end;

end.

