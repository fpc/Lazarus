unit ExtendedTabControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, ComCtrls, Controls, Toolwin, ImgList, Graphics, Menus, Forms,
  LazLoggerBase;

type

  TTabControlToolBarSide = (tsNone, tsLeft, tsRight);

  { TExtendedTabToolbar }

  TExtendedTabToolbar = class(TToolBar)
  protected
    FVertical: Boolean;
    function IsVertical: Boolean; override;
    procedure Loaded; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure GetChildren({%H-}Proc: TGetChildProc; {%H-}Root: TComponent); override;
  end;

  { TExtendedTabToolButton }

  TExtendedTabToolButton = class(TToolButton)
  protected
    procedure Loaded; override;
  end;

  TCustomExtendedTabControl = class;

  { TToolbarWrapper }

  TToolbarWrapper = class(TPersistent)
  private
    FOwner: TCustomExtendedTabControl;
    function ToolBar: TToolBar;
    function GetButton(Index: Integer): TToolButton;
    function GetButtonCount: Integer;
    function GetButtonList: TList;
    function GetEdgeInner: TEdgeStyle;
    function GetEdgeOuter: TEdgeStyle;
    function GetEnabled: Boolean;
    function GetFEdgeBorders: TEdgeBorders;
    function GetFlat: Boolean;
    function GetFont: TFont;
    function GetHotImages: TCustomImageList;
    function GetImages: TCustomImageList;
    function GetIndent: Integer;
    function GetList: Boolean;
    function GetOnClick: TNotifyEvent;
    function GetOnContextPopup: TContextPopupEvent;
    function GetOnDblClick: TNotifyEvent;
    function GetPopupMenu: TPopupMenu;
    function GetRowCount: Integer;
    function GetShowCaptions: Boolean;
    function GetShowHint: Boolean;
    function GetWrapable: Boolean;
    procedure SetEdgeBorders(AValue: TEdgeBorders);
    procedure SetEdgeInner(AValue: TEdgeStyle);
    procedure SetEdgeOuter(AValue: TEdgeStyle);
    procedure SetEnabled(AValue: Boolean);
    procedure SetFlat(AValue: Boolean);
    procedure SetFont(AValue: TFont);
    procedure SetHotImages(AValue: TCustomImageList);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetIndent(AValue: Integer);
    procedure SetList(AValue: Boolean);
    procedure SetOnClick(AValue: TNotifyEvent);
    procedure SetOnContextPopup(AValue: TContextPopupEvent);
    procedure SetOnDblClick(AValue: TNotifyEvent);
    procedure SetPopupMenu(AValue: TPopupMenu);
    procedure SetShowCaptions(AValue: Boolean);
    procedure SetShowHint(AValue: Boolean);
    procedure SetWrapable(AValue: Boolean);
  public
    constructor Create(AnAdvTabControl: TCustomExtendedTabControl);
    procedure AcceptButton(AToolButton: TExtendedTabToolButton);
    property ButtonCount: Integer read GetButtonCount;
    property Buttons[Index: Integer]: TToolButton read GetButton;
    property ButtonList: TList read GetButtonList;
    property RowCount: Integer read GetRowCount;
  published
    property EdgeBorders: TEdgeBorders   read GetFEdgeBorders   write SetEdgeBorders  default [ebLeft, ebTop, ebRight, ebBottom];
    property EdgeInner: TEdgeStyle       read GetEdgeInner      write SetEdgeInner    default esRaised;
    property EdgeOuter: TEdgeStyle       read GetEdgeOuter      write SetEdgeOuter    default esLowered;
    property Enabled: Boolean            read GetEnabled        write SetEnabled      default True;
    property Flat: Boolean               read GetFlat           write SetFlat         default True;
    property Font: TFont                 read GetFont           write SetFont;
    property HotImages: TCustomImageList read GetHotImages      write SetHotImages;
    property Images: TCustomImageList    read GetImages         write SetImages;
    property Indent: Integer             read GetIndent         write SetIndent       default 1;
    property List: Boolean               read GetList           write SetList         default False;
    property PopupMenu: TPopupMenu       read GetPopupMenu      write SetPopupMenu;
    property ShowCaptions: Boolean       read GetShowCaptions   write SetShowCaptions default False;
    property ShowHint: Boolean           read GetShowHint       write SetShowHint;
    property Wrapable: Boolean           read GetWrapable       write SetWrapable     default True;
    property OnClick: TNotifyEvent       read GetOnClick        write SetOnClick;
    property OnContextPopup: TContextPopupEvent read GetOnContextPopup write SetOnContextPopup;
    property OnDblClick: TNotifyEvent    read GetOnDblClick     write SetOnDblClick;
  end;


  TCustomExtendedTabSheet = class;

  { TExtendedInternalCustomPage }

  TExtendedInternalCustomPage = class(TCustomPage)
  protected
  public
    RealPage: TCustomPage; // optional TCustomExtendedTabSheet
    destructor Destroy; override;
  end;

  { TExtendedInternalTabControl
    Internal TabControl that displays the tabs
  }

  TExtendedInternalTabControl = class(TNoteBookStringsTabControl)
  protected
    function GetPageClass: TCustomPageClass; override; // TExtendedInternalCustomPage
  end;

  { TExtendedTabControlNoteBookStrings
    List of Tabs (and pages)
    TCustomExtendedTabControl.Tabs
  }

  TExtendedTabControlNoteBookStrings = class(TTabControlNoteBookStrings)
  private
    FShowToolBar: TTabControlToolBarSide;
    FToolBar: TExtendedTabToolbar;
    procedure SetShowToolBar(AValue: TTabControlToolBarSide);
    procedure ToolbarResized(Sender: TObject);
  protected
    function GetInternalTabControllClass: TNoteBookStringsTabControlClass; override; // TExtendedInternalTabControl
    procedure SetToolbar(AToolBar: TExtendedTabToolbar);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
    procedure NBPageChanged(Sender: TObject); override;
  public
    constructor Create(TheTabControl: TTabControl); override;
    destructor Destroy; override;
    function IndexOfPage(const APage: TCustomExtendedTabSheet): Integer;
    procedure Delete(Index: Integer); override;
    procedure TabControlBoundsChange; override;
    property ShowToolBar: TTabControlToolBarSide read FShowToolBar write SetShowToolBar;
    property ToolBar: TExtendedTabToolbar read FToolBar;
  end;


  { TCustomExtendedTabSheet }

  TCustomExtendedTabSheet = class(TCustomPage)
  private
    FInternalTabPage: TExtendedInternalCustomPage;
    procedure SetInternalTabPage(AValue: TExtendedInternalCustomPage);
  protected
    property InternalTabPage: TExtendedInternalCustomPage read FInternalTabPage write SetInternalTabPage;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    //property TabControl: TCustomExtendedTabControl read GetTabControl write SetTabControl;
    //property TabIndex: Integer read GetTabIndex;
  end;

  TExtendedTabSheet = class(TCustomExtendedTabSheet)
  published
    property BorderWidth;
    property BiDiMode;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Enabled;
    property Font;
    //property Height stored False;
    property ImageIndex;
    //property Left stored False;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnHide;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnShow;
    property OnStartDrag;
    property PageIndex stored False;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabVisible default True;
    //property Top stored False;
    //property Width stored False;
  end;

  { TCustomExtendedTabControl }

  TCustomExtendedTabControl = class(TTabControl)
  private
    FToolBarWrapper: TToolbarWrapper;
    function GetActiveTabSheet: TExtendedTabSheet;
    function GetShowToolBar: TTabControlToolBarSide;
    function GetTabSheet(Index: Integer): TExtendedTabSheet;
    procedure SetActivePageComponent(AValue: TCustomPage);
    procedure SetActiveTabSheet(AValue: TExtendedTabSheet);
    procedure SetShowToolBar(AValue: TTabControlToolBarSide);
  protected
    function AdvTabs: TExtendedTabControlNoteBookStrings;
    function CreateTabNoteBookStrings: TTabControlNoteBookStrings; override;

    //procedure AddRemovePageHandle({%H-}APage: TCustomPage); override; // prevent calling widgetset
    //procedure UpdateTabProperties; override;
    procedure InsertPage(APage: TCustomPage; Index: Integer); override;
    procedure RemovePage(Index: Integer); override;
    //PageRemoved // visible = false
    //DoSendPageIndex // prevent

    function  GetChildOwner: TComponent; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    function GetPageClass: TCustomPageClass; override;
    function GetListClass: TNBBasePagesClass; override;
    property ActivePageComponent: TCustomPage write SetActivePageComponent;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property ActivePage: TExtendedTabSheet read GetActiveTabSheet write SetActiveTabSheet; experimental; platform;
    property Pages[Index: Integer]: TExtendedTabSheet read GetTabSheet; experimental; platform;
    property ShowToolBar: TTabControlToolBarSide read GetShowToolBar write SetShowToolBar;
    property ToolBar: TToolbarWrapper read FToolBarWrapper;
    property OnChange; // Only pages // TODO: activate for tabs
  end;

  { TExtendedTabControl }

  TExtendedTabControl = class(TCustomExtendedTabControl)
  published
    property ShowToolBar;
    property ToolBar;
  end;

implementation

{ TExtendedInternalCustomPage }

destructor TExtendedInternalCustomPage.Destroy;
begin
  if RealPage <> nil then
    TCustomExtendedTabSheet(RealPage).InternalTabPage := nil;
  inherited Destroy;
end;

{ TCustomExtendedTabSheet }

procedure TCustomExtendedTabSheet.SetInternalTabPage(AValue: TExtendedInternalCustomPage);
begin
  if FInternalTabPage = AValue then Exit;
  if FInternalTabPage <> nil then
    FInternalTabPage.RealPage := nil;
  FInternalTabPage := AValue;
  if FInternalTabPage <> nil then
    FInternalTabPage.RealPage := Self;
end;

constructor TCustomExtendedTabSheet.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  BorderSpacing.Around := 1;
end;

destructor TCustomExtendedTabSheet.Destroy;
begin
  SetParent(nil);
  InternalTabPage := nil;
  inherited Destroy;
end;

{ TExtendedInternalTabControl }

function TExtendedInternalTabControl.GetPageClass: TCustomPageClass;
begin
  Result := TExtendedInternalCustomPage;
end;

{ TExtendedTabToolButton }

procedure TExtendedTabToolButton.Loaded;
begin
  inherited Loaded;
  if Parent is TExtendedTabControl then
    Parent := TExtendedTabControl(Parent).AdvTabs.ToolBar;
end;

{ TExtendedTabToolbar }

function TExtendedTabToolbar.IsVertical: Boolean;
begin
  Result := FVertical;
end;

procedure TExtendedTabToolbar.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  // toolbuttons are streamed by tabcontrol. that way the designer can see them
end;

procedure TExtendedTabToolbar.Loaded;
begin
  inherited Loaded;
  if Parent is TExtendedTabControl then
    TExtendedTabControl(Parent).AdvTabs.SetToolbar(Self);
end;

constructor TExtendedTabToolbar.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  //ControlStyle := ControlStyle + [csDesignFixedBounds];
  ControlStyle := ControlStyle + [csNoDesignSelectable];
  Align := alCustom;
  AutoSize := True;
end;

{ TToolbarWrapper }

function TToolbarWrapper.GetEdgeInner: TEdgeStyle;
begin
  Result := ToolBar.EdgeInner;
end;

function TToolbarWrapper.ToolBar: TToolBar;
begin
  Result := FOwner.AdvTabs.ToolBar;
end;

function TToolbarWrapper.GetButton(Index: Integer): TToolButton;
begin
  Result := ToolBar.Buttons[Index];
end;

function TToolbarWrapper.GetButtonCount: Integer;
begin
  Result := ToolBar.ButtonCount;
end;

function TToolbarWrapper.GetButtonList: TList;
begin
  Result := ToolBar.ButtonList;
end;

function TToolbarWrapper.GetEdgeOuter: TEdgeStyle;
begin
  Result := ToolBar.EdgeOuter;
end;

function TToolbarWrapper.GetEnabled: Boolean;
begin
  Result := ToolBar.Enabled;
end;

function TToolbarWrapper.GetFEdgeBorders: TEdgeBorders;
begin
  Result := ToolBar.EdgeBorders;
end;

function TToolbarWrapper.GetFlat: Boolean;
begin
  Result := ToolBar.Flat;
end;

function TToolbarWrapper.GetFont: TFont;
begin
  Result := ToolBar.Font;
end;

function TToolbarWrapper.GetHotImages: TCustomImageList;
begin
  Result := ToolBar.HotImages;
end;

function TToolbarWrapper.GetImages: TCustomImageList;
begin
  Result := ToolBar.Images;
end;

function TToolbarWrapper.GetIndent: Integer;
begin
  Result := ToolBar.Indent;
end;

function TToolbarWrapper.GetList: Boolean;
begin
  Result := ToolBar.List;
end;

function TToolbarWrapper.GetOnClick: TNotifyEvent;
begin
  Result := ToolBar.OnClick;
end;

function TToolbarWrapper.GetOnContextPopup: TContextPopupEvent;
begin
  Result := ToolBar.OnContextPopup;
end;

function TToolbarWrapper.GetOnDblClick: TNotifyEvent;
begin
  Result := ToolBar.OnDblClick;
end;

function TToolbarWrapper.GetPopupMenu: TPopupMenu;
begin
  Result := ToolBar.PopupMenu;
end;

function TToolbarWrapper.GetRowCount: Integer;
begin
  Result := ToolBar.RowCount;
end;

function TToolbarWrapper.GetShowCaptions: Boolean;
begin
  Result := ToolBar.ShowCaptions;
end;

function TToolbarWrapper.GetShowHint: Boolean;
begin
  Result := ToolBar.ShowHint;
end;

function TToolbarWrapper.GetWrapable: Boolean;
begin
  Result := ToolBar.Wrapable;
end;

procedure TToolbarWrapper.SetEdgeBorders(AValue: TEdgeBorders);
begin
  ToolBar.EdgeBorders := AValue;
end;

procedure TToolbarWrapper.SetEdgeInner(AValue: TEdgeStyle);
begin
  ToolBar.EdgeInner := AValue;
end;

procedure TToolbarWrapper.SetEdgeOuter(AValue: TEdgeStyle);
begin
  ToolBar.EdgeOuter := AValue;
end;

procedure TToolbarWrapper.SetEnabled(AValue: Boolean);
begin
  ToolBar.Enabled := AValue;
end;

procedure TToolbarWrapper.SetFlat(AValue: Boolean);
begin
  ToolBar.Flat := AValue;
end;

procedure TToolbarWrapper.SetFont(AValue: TFont);
begin
  ToolBar.Font := AValue;
end;

procedure TToolbarWrapper.SetHotImages(AValue: TCustomImageList);
begin
  ToolBar.HotImages := AValue;
end;

procedure TToolbarWrapper.SetImages(AValue: TCustomImageList);
begin
  ToolBar.Images := AValue;
end;

procedure TToolbarWrapper.SetIndent(AValue: Integer);
begin
  ToolBar.Indent := AValue;
end;

procedure TToolbarWrapper.SetList(AValue: Boolean);
begin
  ToolBar.List := AValue;
end;

procedure TToolbarWrapper.SetOnClick(AValue: TNotifyEvent);
begin
  ToolBar.OnClick := AValue;
end;

procedure TToolbarWrapper.SetOnContextPopup(AValue: TContextPopupEvent);
begin
  ToolBar.OnContextPopup := AValue;
end;

procedure TToolbarWrapper.SetOnDblClick(AValue: TNotifyEvent);
begin
  ToolBar.OnDblClick := AValue;
end;

procedure TToolbarWrapper.SetPopupMenu(AValue: TPopupMenu);
begin
  ToolBar.PopupMenu := AValue;
end;

procedure TToolbarWrapper.SetShowCaptions(AValue: Boolean);
begin
  ToolBar.ShowCaptions := AValue;
end;

procedure TToolbarWrapper.SetShowHint(AValue: Boolean);
begin
  ToolBar.ShowHint := AValue;
end;

procedure TToolbarWrapper.SetWrapable(AValue: Boolean);
begin
  ToolBar.Wrapable := AValue;
end;

constructor TToolbarWrapper.Create(AnAdvTabControl: TCustomExtendedTabControl);
begin
  inherited Create;
  FOwner := AnAdvTabControl;
  ToolBar;
end;

procedure TToolbarWrapper.AcceptButton(AToolButton: TExtendedTabToolButton);
begin
  AToolButton.Parent := FOwner.AdvTabs.ToolBar;
end;

{ TExtendedTabControlNoteBookStrings }

procedure TExtendedTabControlNoteBookStrings.ToolbarResized(Sender: TObject);
begin
  TabControlBoundsChange;
end;

function TExtendedTabControlNoteBookStrings.GetInternalTabControllClass: TNoteBookStringsTabControlClass;
begin
  Result := TExtendedInternalTabControl;
end;

procedure TExtendedTabControlNoteBookStrings.SetToolbar(AToolBar: TExtendedTabToolbar);
begin
  if FToolBar <> nil then begin
    FToolBar.RemoveFreeNotification(TabControl);
    FreeAndNil(FToolBar);
  end;

  FToolBar := AToolBar;

  if FToolBar <> nil then
    FToolBar.FreeNotification(TabControl);
end;

procedure TExtendedTabControlNoteBookStrings.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  if (Operation = opRemove) and (AComponent = FToolBar) then
    FToolBar := nil;
end;

procedure TExtendedTabControlNoteBookStrings.NBPageChanged(Sender: TObject);
var
  p: TCustomPage;
begin
  p := TExtendedInternalCustomPage(NoteBook.Page[NoteBook.PageIndex]).RealPage;
  TExtendedTabControl(TabControl).ActivePageComponent := p;
end;


constructor TExtendedTabControlNoteBookStrings.Create(TheTabControl: TTabControl);
begin
  FToolBar := TExtendedTabToolbar.Create(TheTabControl.Owner);
  FToolBar.FreeNotification(TheTabControl);
  FToolBar.Parent := TheTabControl;
  FToolBar.OnResize := @ToolbarResized;

  inherited Create(TheTabControl);
end;

destructor TExtendedTabControlNoteBookStrings.Destroy;
begin
  inherited Destroy;
  SetToolbar(nil);
end;

function TExtendedTabControlNoteBookStrings.IndexOfPage(const APage: TCustomExtendedTabSheet): Integer;
begin
  Result := Count - 1;
  while (Result >= 0) and (TExtendedInternalCustomPage(Objects[Result]).RealPage <> APage) do
    dec(Result);
end;

procedure TExtendedTabControlNoteBookStrings.Delete(Index: Integer);
var
  p: TCustomPage;
begin
  p := TExtendedInternalCustomPage(Objects[Index]).RealPage;
  inherited Delete(Index);
  if p <> nil then begin
    p.Parent := nil;
    Application.ReleaseComponent(p);
  end;
end;

procedure TExtendedTabControlNoteBookStrings.SetShowToolBar(AValue: TTabControlToolBarSide);
begin
  if FShowToolBar = AValue then Exit;
  FShowToolBar := AValue;
  TabControlBoundsChange;
end;

procedure TExtendedTabControlNoteBookStrings.TabControlBoundsChange;
var
  NewTop, NewHeight, NewLeft, NewWidth, TBOffs: LongInt;
begin
  inherited TabControlBoundsChange;

  NoteBook.TabPosition:=TabControl.TabPosition;
  FToolBar.Visible := ShowToolBar <> tsNone;
  if FToolBar.Visible then
    FToolBar.ControlStyle := FToolBar.ControlStyle-[csNoDesignVisible]
  else
    FToolBar.ControlStyle := FToolBar.ControlStyle+[csNoDesignVisible];
  FToolBar.FVertical := TabPosition in [tpLeft,tpRight];

  case TabControl.TabPosition of
  tpTop,tpBottom:
    begin
      NewHeight:=TabControl.TabHeight;
      if NewHeight<=0 then
        NewHeight:=NoteBook.GetMinimumTabHeight;
      NewHeight:=min(TabControl.ClientHeight,NewHeight);

      if FToolBar.Visible then begin
        FToolBar.Constraints.MaxHeight := Max(0, NewHeight - 2);
        FToolBar.Constraints.MinHeight := Max(0, NewHeight - 2);
        FToolBar.Constraints.MaxWidth  := 0;
        FToolBar.Constraints.MinWidth  := 0;
      end;

      if TabControl.TabPosition=tpTop then begin
        NewTop := 0;
        TBOffs := 0;
      end
      else begin
        NewTop := TabControl.ClientHeight - NewHeight;
        TBOffs := 2;
      end;

      NewLeft := 0;
      NewWidth := TabControl.ClientWidth;

      case ShowToolBar of
        tsLeft: begin
            NewLeft := FToolBar.Width;
            NewWidth := Max(TabControl.ClientWidth div 2, NewWidth - FToolBar.Width);
            FToolBar.SetBounds(0, NewTop+TBOffs, FToolBar.Width, NewHeight - 2 + TBOffs);
          end;
        tsRight: begin
            NewWidth := Max(TabControl.ClientWidth div 2, TabControl.ClientWidth - FToolBar.Width);
            FToolBar.SetBounds(NewWidth, NewTop+TBOffs, FToolBar.Width, NewHeight - 2 + TBOffs);
          end;
      end;
      NoteBook.SetBounds(NewLeft, NewTop, NewWidth ,NewHeight);
    end;

  tpLeft,tpRight:
    begin
      NewWidth := TabControl.TabWidth;
      if NewWidth<=0 then
        NewWidth:=NoteBook.GetMinimumTabWidth;
      NewWidth:=Min(TabControl.ClientWidth,NewWidth);

      if FToolBar.Visible then begin
        FToolBar.Constraints.MaxHeight := 0;
        FToolBar.Constraints.MinHeight := 0;
        FToolBar.Constraints.MaxWidth  := Max(0, NewWidth-2);
        FToolBar.Constraints.MinWidth  := Max(0, NewWidth-2);
      end;

      if TabControl.TabPosition=tpLeft then begin
        NewLeft := 0;
        TBOffs := 0;
      end
      else begin
        NewLeft := TabControl.ClientWidth - NewWidth;
        TBOffs := 2;
      end;

      NewTop := 0;
      NewHeight := TabControl.ClientHeight;

      case ShowToolBar of
        tsLeft: begin
            NewTop := Max(TabControl.ClientHeight div 2, FToolBar.Height);
            NewHeight := NewHeight - FToolBar.Height;
            FToolBar.SetBounds(NewLeft+TBOffs, 0, NewWidth - 2 + TBOffs, FToolBar.Height);
          end;
        tsRight: begin
            NewHeight := Max(TabControl.ClientHeight div 2, TabControl.ClientWidth - FToolBar.Height);
            FToolBar.SetBounds(NewLeft+TBOffs, NewHeight, NewWidth - 2 + TBOffs, FToolBar.Height);
          end;
      end;

      NoteBook.SetBounds(NewLeft, NewTop, NewWidth ,NewHeight);

    end;
  end;

  TabControl.Invalidate;
end;

{ TExtendedTabControl }

function TCustomExtendedTabControl.GetShowToolBar: TTabControlToolBarSide;
begin
  Result := AdvTabs.ShowToolBar;
end;

function TCustomExtendedTabControl.GetTabSheet(Index: Integer): TExtendedTabSheet;
begin
  Result:=TExtendedTabSheet(inherited Page[Index]);
end;

function TCustomExtendedTabControl.GetActiveTabSheet: TExtendedTabSheet;
begin
  Result:=TExtendedTabSheet(inherited ActivePageComponent);
end;

procedure TCustomExtendedTabControl.SetActivePageComponent(AValue: TCustomPage);
var
  i: Integer;
begin
  inherited ActivePageComponent := AValue;
  for i := 0 to PageCount - 1 do
    Page[i].Visible := Page[i] = AValue;
end;

procedure TCustomExtendedTabControl.SetActiveTabSheet(AValue: TExtendedTabSheet);
begin
  ActivePageComponent := AValue;
end;

procedure TCustomExtendedTabControl.SetShowToolBar(AValue: TTabControlToolBarSide);
begin
  AdvTabs.ShowToolBar := AValue;
end;

function TCustomExtendedTabControl.AdvTabs: TExtendedTabControlNoteBookStrings;
begin
  Result := TExtendedTabControlNoteBookStrings(Tabs);
end;

function TCustomExtendedTabControl.CreateTabNoteBookStrings: TTabControlNoteBookStrings;
begin
  Result := TExtendedTabControlNoteBookStrings.Create(Self);
end;

procedure TCustomExtendedTabControl.InsertPage(APage: TCustomPage; Index: Integer);
begin
  inherited InsertPage(APage, Index);
  if Index >= Tabs.Count then
    Index := Tabs.Add(APage.Caption)
  else
    Tabs.Insert(Index, APage.Caption);
  TCustomExtendedTabSheet(APage).InternalTabPage := TExtendedInternalCustomPage(Tabs.Objects[Index]);
end;

procedure TCustomExtendedTabControl.RemovePage(Index: Integer);
var
  i: Integer;
  ThePage: TCustomExtendedTabSheet;
begin
  ThePage := TCustomExtendedTabSheet(CustomPage(Index));
  if ThePage.InternalTabPage <> nil then begin
    i := AdvTabs.IndexOfPage(ThePage);
    ThePage.InternalTabPage := nil;
    if i >= 0 then
      Tabs.Delete(i);
  end;
  inherited RemovePage(Index);
end;

function TCustomExtendedTabControl.GetChildOwner: TComponent;
begin
  Result := inherited GetChildOwner;
  //Result := Self;
end;

procedure TCustomExtendedTabControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I : Integer;
  Control : TControl;
begin
  inherited GetChildren(Proc, Root);

  //if Owner = Root then
  //  Proc(AdvTabs.ToolBar);


  if (Owner <> Root) or (AdvTabs = nil) or (AdvTabs.ToolBar = nil) then
    exit;
  for I := 0 to AdvTabs.ToolBar.ControlCount-1 do
  begin
    Control := AdvTabs.ToolBar.Controls[i];
    Proc(Control);
  end;
end;

procedure TCustomExtendedTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AdvTabs <> nil then
    AdvTabs.Notification(AComponent, Operation);
end;

function TCustomExtendedTabControl.GetPageClass: TCustomPageClass;
begin
  Result := TExtendedTabSheet;
end;

function TCustomExtendedTabControl.GetListClass: TNBBasePagesClass;
begin
  Result := TNBPages;
end;

constructor TCustomExtendedTabControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FToolBarWrapper := TToolbarWrapper.Create(Self);
end;

destructor TCustomExtendedTabControl.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FToolBarWrapper);
end;

end.

