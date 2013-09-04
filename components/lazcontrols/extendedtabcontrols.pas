unit ExtendedTabControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, ComCtrls, Controls, Toolwin, ImgList, Graphics, Menus,
  LazLoggerBase;

type

  TTabControlToolBarSide = (tsNone, tsLeft, tsRight);

  { TAdvancedToolbar }

  TAdvancedToolbar = class(TToolBar)
  protected
    FVertical: Boolean;
    function IsVertical: Boolean; override;
    procedure GetChildren({%H-}Proc: TGetChildProc; {%H-}Root: TComponent); override;
    procedure Loaded; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TAdvancedToolButton }

  TAdvancedToolButton = class(TToolButton)
  protected
    procedure Loaded; override;
  end;

  { TAdvancedTabControlNoteBookStrings }

  TAdvancedTabControlNoteBookStrings = class(TTabControlNoteBookStrings)
  private
    FShowToolBar: TTabControlToolBarSide;
    FToolBar: TAdvancedToolbar;
    procedure SetShowToolBar(AValue: TTabControlToolBarSide);
    procedure ToolbarResized(Sender: TObject);
  public
    constructor Create(TheTabControl: TTabControl); override;
    destructor Destroy; override;
    procedure TabControlBoundsChange; override;
    property ShowToolBar: TTabControlToolBarSide read FShowToolBar write SetShowToolBar;
    property ToolBar: TAdvancedToolbar read FToolBar;
  end;

  TCustomAdvancedTabControl = class;
  // Expose only selected properties

  { TToolbarWrapper }

  TToolbarWrapper = class(TPersistent)
  private
    FOwner: TCustomAdvancedTabControl;
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
    constructor Create(AnAdvTabControl: TCustomAdvancedTabControl);
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

  { TCustomAdvancedTabControl }

  TCustomAdvancedTabControl = class(TTabControl)
  private
    FToolBarWrapper: TToolbarWrapper;
    function GetShowToolBar: TTabControlToolBarSide;
    procedure SetShowToolBar(AValue: TTabControlToolBarSide);
  protected
    function AdvTabs: TAdvancedTabControlNoteBookStrings;
    function CreateTabNoteBookStrings: TTabControlNoteBookStrings; override;
    function  GetChildOwner: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RequestNotification;
    procedure UnRequestNotification;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ShowToolBar: TTabControlToolBarSide read GetShowToolBar write SetShowToolBar;
    property ToolBar: TToolbarWrapper read FToolBarWrapper;
  end;

  { TAdvancedTabControl }

  TAdvancedTabControl = class(TCustomAdvancedTabControl)
  published
    property ShowToolBar;
    property ToolBar;
  end;

implementation

{ TAdvancedToolButton }

procedure TAdvancedToolButton.Loaded;
begin
  inherited Loaded;
  if Parent is TAdvancedTabControl then
    Parent := TAdvancedTabControl(Parent).AdvTabs.ToolBar;
end;

{ TAdvancedToolbar }

function TAdvancedToolbar.IsVertical: Boolean;
begin
  Result := FVertical;
end;

procedure TAdvancedToolbar.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
  // toolbuttons are streamed by tabcontrol. that way the designer can see them
end;

procedure TAdvancedToolbar.Loaded;
begin
  inherited Loaded;
  if Parent is TAdvancedTabControl then begin
    TAdvancedTabControl(Parent).UnRequestNotification;
    FreeAndNil(TAdvancedTabControl(Parent).AdvTabs.FToolBar);
    TAdvancedTabControl(Parent).AdvTabs.FToolBar := Self;
    TAdvancedTabControl(Parent).RequestNotification;
  end;
end;

constructor TAdvancedToolbar.Create(TheOwner: TComponent);
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

constructor TToolbarWrapper.Create(AnAdvTabControl: TCustomAdvancedTabControl);
begin
  inherited Create;
  FOwner := AnAdvTabControl;
  ToolBar;
end;

{ TAdvancedTabControlNoteBookStrings }

procedure TAdvancedTabControlNoteBookStrings.ToolbarResized(Sender: TObject);
begin
  TabControlBoundsChange;
end;


constructor TAdvancedTabControlNoteBookStrings.Create(TheTabControl: TTabControl);
begin
  FToolBar := TAdvancedToolbar.Create(TheTabControl.Owner);
  FToolBar.Parent := TheTabControl;
  FToolBar.OnResize := @ToolbarResized;

  inherited Create(TheTabControl);
end;

destructor TAdvancedTabControlNoteBookStrings.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FToolBar);
end;

procedure TAdvancedTabControlNoteBookStrings.SetShowToolBar(AValue: TTabControlToolBarSide);
begin
  if FShowToolBar = AValue then Exit;
  FShowToolBar := AValue;
  TabControlBoundsChange;
end;

procedure TAdvancedTabControlNoteBookStrings.TabControlBoundsChange;
var
  NewTop, NewHeight, NewLeft, NewWidth, TBOffs: LongInt;
begin
  inherited TabControlBoundsChange;

  NoteBook.TabPosition:=TabControl.TabPosition;
  FToolBar.Visible := ShowToolBar <> tsNone;
  FToolBar.AutoSize := FToolBar.Visible;
  if not FToolBar.Visible then
    FToolBar.SetBounds(0,0,0,0);
  FToolBar.FVertical := TabPosition in [tpLeft,tpRight];

  case TabControl.TabPosition of
  tpTop,tpBottom:
    begin
      NewHeight:=TabHeight;
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
      NewWidth := TabWidth;
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

{ TAdvancedTabControl }

function TCustomAdvancedTabControl.GetShowToolBar: TTabControlToolBarSide;
begin
  Result := AdvTabs.ShowToolBar;
end;

procedure TCustomAdvancedTabControl.SetShowToolBar(AValue: TTabControlToolBarSide);
begin
  AdvTabs.ShowToolBar := AValue;
end;

function TCustomAdvancedTabControl.AdvTabs: TAdvancedTabControlNoteBookStrings;
begin
  Result := TAdvancedTabControlNoteBookStrings(Tabs);
end;

function TCustomAdvancedTabControl.CreateTabNoteBookStrings: TTabControlNoteBookStrings;
begin
  Result := TAdvancedTabControlNoteBookStrings.Create(Self);
  TAdvancedTabControlNoteBookStrings(Result).ToolBar.FreeNotification(Self);
end;

function TCustomAdvancedTabControl.GetChildOwner: TComponent;
begin
  Result := inherited GetChildOwner;
  //Result := Self;
end;

procedure TCustomAdvancedTabControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
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

procedure TCustomAdvancedTabControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AdvTabs <> nil) and (AComponent = AdvTabs.ToolBar) then
    AdvTabs.FToolBar := nil;
end;

procedure TCustomAdvancedTabControl.RequestNotification;
begin
  if AdvTabs.ToolBar<> nil then
    AdvTabs.ToolBar.FreeNotification(Self);
end;

procedure TCustomAdvancedTabControl.UnRequestNotification;
begin
  if AdvTabs.ToolBar<> nil then
    AdvTabs.ToolBar.RemoveFreeNotification(Self);
end;

constructor TCustomAdvancedTabControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FToolBarWrapper := TToolbarWrapper.Create(Self);
end;

destructor TCustomAdvancedTabControl.Destroy;
begin
  UnRequestNotification;
  inherited Destroy;
  FreeAndNil(FToolBarWrapper);
end;

end.

