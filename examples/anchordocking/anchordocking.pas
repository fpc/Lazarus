{ Unit implementing anchor docking.

  Copyright (C) 2010 Mattias Gaertner mattias@freepascal.org

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
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  Features:
    - dnd docking
    - preview rectangle while drag over
    - inside and outside docking
    - header with close button and hints
    - using stock item for close button glyph
    - auto header caption from content
    - hide header caption for floating form
    - auto site for headers to safe space (configurable)
    - bidimode for headers
    - page docking
    - pagecontrols uses TPageControl for native look&feel
    - page control is automatically removed if only one page left
    - scaling on resize (configurable)
    - auto insert splitters between controls (size configurable)
    - keep size when docking
    - header is automatically hidden when docked into page
    - save complete layout
    - restore layout:
       - close unneeded windows,
       - automatic clean up if windows are missing,
       - reusing existing docksites to minimize flickering
    - popup menu
       - close site
       - lock/unlock
       - header auto, left, top, right, bottom
       - undock (needed if no place to undock on screen)
       - merge (for example after moving a dock page into a layout)
       - enlarge side to left, top, right, bottom
       - move page left, right, leftmost, rightmost
       - close page
       - tab position (default, left, top, right, bottom)
       - options
    - dock site: MakeDockSite for forms, that should be able to dock other sites,
       but should not be docked themselves. Their Parent is always nil.
    - design time package for IDE
    - dnd move page index
    - dnd move page to another pagecontrol
    - on close button: save a restore layout

  ToDo:
    - keep custom dock site content visible
    - restore custom dock site splitter without resizing content, only resize docked site
    - undock on hide
    - option to show/hide dock headers of children
    - popup menu
       - shrink side left, top, right, bottom
    - simple way to make forms dockable at designtime without any code
    - on show again: restore layout
    - close button for pages
}
unit AnchorDocking;

{$mode objfpc}{$H+}

interface

uses
  Math, Classes, SysUtils, LResources, types, LCLType, LCLIntf, LCLProc,
  Controls, Forms, ExtCtrls, ComCtrls, Graphics, Themes, Menus, Buttons,
  LazConfigStorage, AnchorDockStr, AnchorDockStorage;

type
  TAnchorDockHostSite = class;

  { TAnchorDockCloseButton
    Close button used in TAnchorDockHeader, uses the close button glyph of the
    theme shrinked to a small size. The glyph is shared by all close buttons. }

  TAnchorDockCloseButton = class(TCustomSpeedButton)
  protected
    procedure GetCloseGlyph; // increase reference count
    procedure ReleaseCloseGlyph; // decrease reference count
    function GetGlyphSize({%H-}Drawing: boolean; PaintRect: TRect): TSize; override;
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect;
            const AOffset: TPoint; AState: TButtonState; ATransparent: Boolean;
            BiDiFlags: Longint): TRect; override;
    procedure CalculatePreferredSize(var PreferredWidth,
           PreferredHeight: integer; WithThemeSpace: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TAnchorDockHeader
    The panel of a TAnchorDockHostSite containing the close button and the
    caption when the form is docked. The header can be shown at any of the four
    sides, shows a hint for long captions, starts dragging and shows the popup
    menu of the dockmaster.
    Hiding and aligning is done by its Parent a TAnchorDockHostSite }

  TAnchorDockHeader = class(TCustomPanel)
  private
    FCloseButton: TCustomSpeedButton;
    FHeaderPosition: TADLHeaderPosition;
    procedure CloseButtonClick(Sender: TObject);
    procedure HeaderPositionItemClick(Sender: TObject);
    procedure UndockButtonClick(Sender: TObject);
    procedure MergeButtonClick(Sender: TObject);
    procedure EnlargeSideClick(Sender: TObject);
    procedure SetHeaderPosition(const AValue: TADLHeaderPosition);
  protected
    procedure Paint; override;
    procedure CalculatePreferredSize(var PreferredWidth,
          PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,
             Y: Integer); override;
    procedure UpdateHeaderControls;
    procedure SetAlign(Value: TAlign); override;
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
    procedure PopupMenuPopup(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    property CloseButton: TCustomSpeedButton read FCloseButton;
    property HeaderPosition: TADLHeaderPosition read FHeaderPosition write SetHeaderPosition;
    property BevelOuter default bvNone;
  end;
  TAnchorDockHeaderClass = class of TAnchorDockHeader;

  { TAnchorDockSplitter
    A TSplitter used on a TAnchorDockHostSite with SiteType=adhstLayout.
    It can store DockBounds, used by its parent to scale. Scaling works by
    moving the splitters. All other controls are fully anchored to these
    splitters or their parent. }

  TAnchorDockSplitter = class(TCustomSplitter)
  private
    FDockBounds: TRect;
    FDockParentClientSize: TSize;
    FDockRestoreBounds: TRect;
  protected
    procedure SetResizeAnchor(const AValue: TAnchorKind); override;
  public
    constructor Create(TheOwner: TComponent); override;
    property DockBounds: TRect read FDockBounds write FDockBounds;
    property DockParentClientSize: TSize read FDockParentClientSize write FDockParentClientSize;
    procedure UpdateDockBounds;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: integer); override; // any normal movement sets the DockBounds
    procedure SetBoundsKeepDockBounds(ALeft, ATop, AWidth, AHeight: integer); // movement for scaling keeps the DockBounds
    function SideAnchoredControlCount(Side: TAnchorKind): integer;
    procedure SaveLayout(LayoutNode: TAnchorDockLayoutTreeNode);
    function HasOnlyOneSibling(Side: TAnchorKind; MinPos, MaxPos: integer): TControl;
    property DockRestoreBounds: TRect read FDockRestoreBounds write FDockRestoreBounds;
  end;
  TAnchorDockSplitterClass = class of TAnchorDockSplitter;

  { TAnchorDockPage
    A page of a TAnchorDockPageControl. }

  TAnchorDockPage = class(TCustomPage)
  public
    procedure UpdateDockCaption(Exclude: TControl = nil); override;
    procedure InsertControl(AControl: TControl; Index: integer); override;
    procedure RemoveControl(AControl: TControl); override;
    function GetSite: TAnchorDockHostSite;
  end;
  TAnchorDockPageClass = class of TAnchorDockPage;

  { TAnchorDockPageControl
    Used for page docking.
    The parent is always a TAnchorDockHostSite with SiteType=adhstPages.
    Its children are all TAnchorDockPage.
    It shows the DockMaster popup menu and starts dragging. }

  TAnchorDockPageControl = class(TCustomTabControl)
  private
    function GetDockPages(Index: integer): TAnchorDockPage;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y: Integer); override;
    procedure PopupMenuPopup(Sender: TObject); virtual;
    procedure CloseButtonClick(Sender: TObject); virtual;
    procedure MoveLeftButtonClick(Sender: TObject); virtual;
    procedure MoveLeftMostButtonClick(Sender: TObject); virtual;
    procedure MoveRightButtonClick(Sender: TObject); virtual;
    procedure MoveRightMostButtonClick(Sender: TObject); virtual;
    procedure TabPositionClick(Sender: TObject); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure UpdateDockCaption(Exclude: TControl = nil); override;
    property DockPages[Index: integer]: TAnchorDockPage read GetDockPages;
    procedure RemoveControl(AControl: TControl); override;
    function GetActiveSite: TAnchorDockHostSite;
  end;
  TAnchorDockPageControlClass = class of TAnchorDockPageControl;

  { TAnchorDockHostSite
    This form is the dockhostsite for all controls.
    When docked together they build a tree structure with the docked controls
    as leaf nodes.
    A TAnchorDockHostSite has four modes:

    }

  TAnchorDockHostSiteType = (
    adhstNone,  // fresh created, no control docked
    adhstOneControl, // a control and the "Header" (TAnchorDockHeader)
    adhstLayout, // several controls/TAnchorDockHostSite separated by TAnchorDockSplitters
    adhstPages  // the "Pages" (TAnchorDockPageControl) with several pages
    );

  TAnchorDockHostSite = class(TCustomForm)
  private
    FDockRestoreBounds: TRect;
    FHeader: TAnchorDockHeader;
    FHeaderSide: TAnchorKind;
    FPages: TAnchorDockPageControl;
    FSiteType: TAnchorDockHostSiteType;
    FBoundSplitter: TAnchorDockSplitter;
    fUpdateLayout: integer;
    procedure SetHeaderSide(const AValue: TAnchorKind);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
                           override;
    function DoDockClientMsg(DragDockObject: TDragDockObject;
                             aPosition: TPoint): boolean; override;
    function ExecuteDock(NewControl, DropOnControl: TControl; DockAlign: TAlign): boolean; virtual;
    function DockFirstControl(NewControl: TControl): boolean; virtual;
    function DockSecondControl(NewControl: TControl; DockAlign: TAlign;
                               Inside: boolean): boolean; virtual;
    function DockAnotherControl(Sibling, NewControl: TControl; DockAlign: TAlign;
                                Inside: boolean): boolean; virtual;
    procedure CreatePages; virtual;
    function DockSecondPage(NewControl: TControl): boolean; virtual;
    function DockAnotherPage(NewControl: TControl; InFrontOf: TControl): boolean; virtual;
    procedure AddCleanControl(AControl: TControl; TheAlign: TAlign = alNone);
    procedure RemoveControlFromLayout(AControl: TControl);
    procedure RemoveSpiralSplitter(AControl: TControl);
    procedure ClearChildControlAnchorSides(AControl: TControl);
    procedure Simplify;
    procedure SimplifyPages;
    procedure SimplifyOneControl;
    function GetOneControl: TControl;
    function GetSiteCount: integer;
    function IsOneSiteLayout(out Site: TAnchorDockHostSite): boolean;
    function IsTwoSiteLayout(out Site1, Site2: TAnchorDockHostSite): boolean;
    function GetUniqueSplitterName: string;
    function MakeSite(AControl: TControl): TAnchorDockHostSite;
    procedure MoveAllControls(dx, dy: integer);
    procedure AlignControls(AControl: TControl; var ARect: TRect); override;
    function CheckIfOneControlHidden: boolean;
    procedure DoDock(NewDockSite: TWinControl; var ARect: TRect); override;
    procedure SetParent(NewParent: TWinControl); override;
    function HeaderNeedsShowing: boolean;
    procedure DoClose(var CloseAction: TCloseAction); override;
    function CanUndock: boolean;
    procedure Undock;
    function CanMerge: boolean;
    procedure Merge;
    function EnlargeSide(Side: TAnchorKind;
                         OnlyCheckIfPossible: boolean): boolean;
    function EnlargeSideResizeTwoSplitters(ShrinkSplitterSide,
                         EnlargeSpitterSide: TAnchorKind;
                         OnlyCheckIfPossible: boolean): boolean;
    function EnlargeSideRotateSplitter(Side: TAnchorKind;
                         OnlyCheckIfPossible: boolean): boolean;
    procedure CreateBoundSplitter;
    procedure PositionBoundSplitter;
  public
    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;
    function CloseQuery: boolean; override;
    function CloseSite: boolean; virtual;
    procedure RemoveControl(AControl: TControl); override;
    procedure InsertControl(AControl: TControl; Index: integer); override;
    procedure GetSiteInfo(Client: TControl; var InfluenceRect: TRect;
                          MousePos: TPoint; var CanDock: Boolean); override;
    function GetPageArea: TRect;
    procedure ChangeBounds(ALeft, ATop, AWidth, AHeight: integer;
                           KeepBase: boolean); override;
    procedure UpdateDockCaption(Exclude: TControl = nil); override;
    procedure UpdateHeaderAlign;
    procedure UpdateHeaderShowing;
    procedure BeginUpdateLayout;
    procedure EndUpdateLayout;
    function UpdatingLayout: boolean;

    // save/restore layout
    procedure SaveLayout(LayoutTree: TAnchorDockLayoutTree;
                         LayoutNode: TAnchorDockLayoutTreeNode);
    property DockRestoreBounds: TRect read FDockRestoreBounds write FDockRestoreBounds;

    property HeaderSide: TAnchorKind read FHeaderSide write SetHeaderSide;
    property Header: TAnchorDockHeader read FHeader;
    property Pages: TAnchorDockPageControl read FPages;
    property SiteType: TAnchorDockHostSiteType read FSiteType;
    property BoundSplitter: TAnchorDockSplitter read FBoundSplitter;
  end;
  TAnchorDockHostSiteClass = class of TAnchorDockHostSite;

  TADMResizePolicy = (
    admrpNone,
    admrpChild  // resize child
    );

  { TAnchorDockManager
    A TDockManager is the LCL connector to catch various docking events for a
    TControl. Every TAnchorDockHostSite and every custom dock site gets one
    TAnchorDockManager. The LCL frees it automatically when the Site is freed. }

  TAnchorDockManager = class(TDockManager)
  private
    FDockableSites: TAnchors;
    FDockSite: TAnchorDockHostSite;
    FInsideDockingAllowed: boolean;
    FPreferredSiteSizeAsSiteMinimum: boolean;
    FResizePolicy: TADMResizePolicy;
    FStoredConstraints: TRect;
    FSite: TWinControl;
    FSiteClientRect: TRect;
    procedure SetPreferredSiteSizeAsSiteMinimum(const AValue: boolean);
  public
    constructor Create(ADockSite: TWinControl); override;
    procedure GetControlBounds(Control: TControl; out AControlBounds: TRect);
      override;
    procedure InsertControl(Control: TControl; InsertAt: TAlign;
      DropCtl: TControl); override; overload;
    procedure InsertControl(ADockObject: TDragDockObject); override; overload;
    procedure LoadFromStream(Stream: TStream); override;
    procedure PositionDockRect(Client, DropCtl: TControl; DropAlign: TAlign;
      var DockRect: TRect); override; overload;
    procedure RemoveControl(Control: TControl); override;
    procedure ResetBounds(Force: Boolean); override;
    procedure SaveToStream(Stream: TStream); override;
    function GetDockEdge(ADockObject: TDragDockObject): boolean; override;
    procedure RestoreSite(SplitterPos: integer);
    procedure StoreConstraints;
    function GetSitePreferredClientSize: TPoint;

    property Site: TWinControl read FSite; // the associated TControl (a TAnchorDockHostSite or a custom dock site)
    property DockSite: TAnchorDockHostSite read FDockSite; // if Site is a TAnchorDockHostSite, this is it
    property DockableSites: TAnchors read FDockableSites write FDockableSites; // at which sides can be docked.
    property InsideDockingAllowed: boolean read FInsideDockingAllowed write FInsideDockingAllowed; // if true allow to put a site into the custom dock site
    function GetChildSite: TAnchorDockHostSite; // get first child TAnchorDockHostSite
    property ResizePolicy: TADMResizePolicy read FResizePolicy write FResizePolicy;
    property StoredConstraints: TRect read FStoredConstraints write FStoredConstraints;
    function StoredConstraintsValid: boolean;
    property PreferredSiteSizeAsSiteMinimum: boolean read FPreferredSiteSizeAsSiteMinimum write SetPreferredSiteSizeAsSiteMinimum;
  end;
  TAnchorDockManagerClass = class of TAnchorDockManager;

  TAnchorDockMaster = class;

  { TAnchorDockMaster
    The central instance that connects all sites and manages all global
    settings. Its global variable is the DockMaster.
    Applications only need to talk to the DockMaster. }

  TADCreateControlEvent = procedure(Sender: TObject; aName: string;
                var AControl: TControl; DoDisableAutoSizing: boolean) of object;
  TADShowDockMasterOptionsEvent = function(aDockMaster: TAnchorDockMaster): TModalResult;

  TAnchorDockMaster = class(TComponent)
  private
    FAllowDragging: boolean;
    FControls: TFPList;
    FDockOutsideMargin: integer;
    FDockParentMargin: integer;
    FDragTreshold: integer;
    FHeaderAlignLeft: integer;
    FHeaderAlignTop: integer;
    FHeaderButtonSize: integer;
    FHeaderClass: TAnchorDockHeaderClass;
    FHeaderHint: string;
    FIdleConnected: Boolean;
    FManagerClass: TAnchorDockManagerClass;
    FOnCreateControl: TADCreateControlEvent;
    FOnShowOptions: TADShowDockMasterOptionsEvent;
    FPageAreaInPercent: integer;
    FPageClass: TAnchorDockPageClass;
    FPageControlClass: TAnchorDockPageControlClass;
    FQueueSimplify: Boolean;
    FRestoreLayouts: TAnchorDockRestoreLayouts;
    FRestoring: boolean;
    FScaleOnResize: boolean;
    FShowHeaderCaption: boolean;
    FHideHeaderCaptionFloatingControl: boolean;
    FSiteClass: TAnchorDockHostSiteClass;
    FSplitterClass: TAnchorDockSplitterClass;
    FSplitterWidth: integer;
    fNeedSimplify: TFPList; // list of TControl
    fNeedFree: TFPList; // list of TControl
    fSimplifying: boolean;
    fUpdateCount: integer;
    fDisabledAutosizing: TFPList; // list of TControl
    fTreeNameToDocker: TADNameToControl;
    fPopupMenu: TPopupMenu;
    function GetControls(Index: integer): TControl;
    function CloseUnneededControls(Tree: TAnchorDockLayoutTree): boolean;
    function CreateNeededControls(Tree: TAnchorDockLayoutTree;
                DisableAutoSizing: boolean; ControlNames: TStrings): boolean;
    procedure MapTreeToControls(Tree: TAnchorDockLayoutTree);
    function RestoreLayout(Tree: TAnchorDockLayoutTree; Scale: boolean): boolean;
    procedure EnableAllAutoSizing;
    procedure ClearLayoutProperties(AControl: TControl);
    procedure PopupMenuPopup(Sender: TObject);
    procedure ChangeLockButtonClick(Sender: TObject);
    procedure OptionsClick(Sender: TObject);
    procedure SetIdleConnected(const AValue: Boolean);
    procedure SetQueueSimplify(const AValue: Boolean);
    procedure SetRestoring(const AValue: boolean);
  protected
    fCloseBtnReferenceCount: integer;
    fCloseBtnBitmap: TBitmap;
    function DoCreateControl(aName: string; DisableAutoSizing: boolean): TControl;
    procedure AutoSizeAllHeaders(EnableAutoSizing: boolean);
    procedure CreateCloseButtonBitmap; virtual;
    procedure DisableControlAutoSizing(AControl: TControl);
    procedure FreeCloseButtonBitmap; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation);
          override;
    procedure SetHeaderAlignLeft(const AValue: integer);
    procedure SetHeaderAlignTop(const AValue: integer);
    procedure SetHeaderButtonSize(const AValue: integer);
    procedure SetShowHeaderCaption(const AValue: boolean);
    procedure SetHideHeaderCaptionFloatingControl(const AValue: boolean);
    procedure SetSplitterWidth(const AValue: integer);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure AsyncSimplify({%H-}Data: PtrInt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ControlCount: integer;
    property Controls[Index: integer]: TControl read GetControls;
    function IndexOfControl(const aName: string): integer;
    function FindControl(const aName: string): TControl;
    function IsSite(AControl: TControl): boolean;
    function IsAnchorSite(AControl: TControl): boolean;
    function IsCustomSite(AControl: TControl): boolean;
    function GetSite(AControl: TControl): TCustomForm;
    function GetAnchorSite(AControl: TControl): TAnchorDockHostSite;
    function GetControl(Site: TControl): TControl;
    function IsFloating(AControl: TControl): Boolean;
    function GetPopupMenu: TPopupMenu;
    function AddPopupMenuItem(AName, ACaption: string;
                    const OnClickEvent: TNotifyEvent; AParent: TMenuItem = nil): TMenuItem; virtual;
    function AddRemovePopupMenuItem(Add: boolean; AName, ACaption: string;
                    const OnClickEvent: TNotifyEvent; AParent: TMenuItem = nil): TMenuItem; virtual;

    // show / make a control dockable
    procedure MakeDockable(AControl: TControl; Show: boolean = true;
                           BringToFront: boolean = false;
                           AddDockHeader: boolean = true);
    procedure MakeDockSite(AForm: TCustomForm; Sites: TAnchors;
                           ResizePolicy: TADMResizePolicy;
                           AllowInside: boolean = false);
    procedure MakeVisible(AControl: TControl; SwitchPages: boolean);
    function ShowControl(ControlName: string; BringToFront: boolean = false
                         ): TControl;
    procedure CloseAll;

    // save/restore layouts
    procedure SaveLayoutToConfig(Config: TConfigStorage);
    procedure SaveMainLayoutToTree(LayoutTree: TAnchorDockLayoutTree);
    procedure SaveSiteLayoutToTree(AForm: TCustomForm;
                                   LayoutTree: TAnchorDockLayoutTree);
    function CreateRestoreLayout(AControl: TControl): TAnchorDockRestoreLayout;
    function ConfigIsEmpty(Config: TConfigStorage): boolean;
    function LoadLayoutFromConfig(Config: TConfigStorage; Scale: Boolean): boolean;
    property RestoreLayouts: TAnchorDockRestoreLayouts read FRestoreLayouts;
    property Restoring: boolean read FRestoring write SetRestoring;
    property IdleConnected: Boolean read FIdleConnected write SetIdleConnected;
    procedure LoadSettingsFromConfig(Config: TConfigStorage);
    procedure SaveSettingsToConfig(Config: TConfigStorage);

    // manual docking
    procedure ManualFloat(AControl: TControl);
    procedure ManualDock(SrcSite: TAnchorDockHostSite; TargetSite: TCustomForm;
                         Align: TAlign; TargetControl: TControl = nil);
    function ManualEnlarge(Site: TAnchorDockHostSite; Side: TAnchorKind;
                         OnlyCheckIfPossible: boolean): boolean;

    // simplification/garbage collection
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure NeedSimplify(AControl: TControl);
    procedure NeedFree(AControl: TControl);
    procedure SimplifyPendingLayouts;
    function AutoFreedIfControlIsRemoved(AControl, RemovedControl: TControl): boolean;
    function CreateSite(NamePrefix: string = '';
                        DisableAutoSizing: boolean = true): TAnchorDockHostSite;
    function CreateSplitter(NamePrefix: string = ''): TAnchorDockSplitter;
    property QueueSimplify: Boolean read FQueueSimplify write SetQueueSimplify;

    // options
    property OnShowOptions: TADShowDockMasterOptionsEvent read FOnShowOptions write FOnShowOptions;
    property DragTreshold: integer read FDragTreshold write FDragTreshold default 4;
    property DockOutsideMargin: integer read FDockOutsideMargin write FDockOutsideMargin default 10; // max distance for outside mouse snapping
    property DockParentMargin: integer read FDockParentMargin write FDockParentMargin default 10; // max distance for snap to parent
    property PageAreaInPercent: integer read FPageAreaInPercent write FPageAreaInPercent default 40; // size of inner mosue snapping area for page docking
    property HeaderAlignTop: integer read FHeaderAlignTop write SetHeaderAlignTop default 80; // move header to top, when (width/height)*100<=HeaderAlignTop
    property HeaderAlignLeft: integer read FHeaderAlignLeft write SetHeaderAlignLeft default 120; // move header to left, when (width/height)*100>=HeaderAlignLeft
    property HeaderHint: string read FHeaderHint write FHeaderHint;
    property SplitterWidth: integer read FSplitterWidth write SetSplitterWidth default 4;
    property ScaleOnResize: boolean read FScaleOnResize write FScaleOnResize default true; // scale children when resizing a site
    property ShowHeaderCaption: boolean read FShowHeaderCaption write SetShowHeaderCaption default true; // set to false to disable showing header captions
    property HideHeaderCaptionFloatingControl: boolean read FHideHeaderCaptionFloatingControl
                          write SetHideHeaderCaptionFloatingControl default true;
    property OnCreateControl: TADCreateControlEvent read FOnCreateControl write FOnCreateControl;
    property AllowDragging: boolean read FAllowDragging write FAllowDragging default true;
    property HeaderButtonSize: integer read FHeaderButtonSize write SetHeaderButtonSize default 10;

    // for descendants
    property SplitterClass: TAnchorDockSplitterClass read FSplitterClass write FSplitterClass;
    property SiteClass: TAnchorDockHostSiteClass read FSiteClass write FSiteClass;
    property ManagerClass: TAnchorDockManagerClass read FManagerClass write FManagerClass;
    property HeaderClass: TAnchorDockHeaderClass read FHeaderClass write FHeaderClass;
    property PageControlClass: TAnchorDockPageControlClass read FPageControlClass write FPageControlClass;
    property PageClass: TAnchorDockPageClass read FPageClass write FPageClass;
  end;

var
  DockMaster: TAnchorDockMaster = nil;

function dbgs(SiteType: TAnchorDockHostSiteType): string; overload;

procedure CopyAnchorBounds(Source, Target: TControl);
procedure AnchorAndChangeBounds(AControl: TControl; Side: TAnchorKind;
                                Target: TControl);
function ControlsLeftTopOnScreen(AControl: TControl): TPoint;

type
  TAnchorControlsRect = array[TAnchorKind] of TControl;

function GetDockSplitter(Control: TControl; Side: TAnchorKind;
                         out Splitter: TAnchorDockSplitter): boolean;
function GetDockSplitterOrParent(Control: TControl; Side: TAnchorKind;
                                 out AnchorControl: TControl): boolean;
function CountAnchoredControls(Control: TControl; Side: TAnchorKind
                               ): Integer;
function NeighbourCanBeShrinked(EnlargeControl, Neighbour: TControl;
                                Side: TAnchorKind): boolean;
function ControlIsAnchoredIndirectly(StartControl: TControl; Side: TAnchorKind;
                                     DestControl: TControl): boolean;
procedure GetAnchorControlsRect(Control: TControl;
                                out ARect: TAnchorControlsRect);
function GetEnclosingControlRect(ControlList: TFPlist;
                                 out ARect: TAnchorControlsRect): boolean;
function GetEnclosedControls(const ARect: TAnchorControlsRect): TFPList;

implementation

function dbgs(SiteType: TAnchorDockHostSiteType): string; overload;
begin
  case SiteType of
  adhstNone: Result:='None';
  adhstOneControl: Result:='OneControl';
  adhstLayout: Result:='Layout';
  adhstPages: Result:='Pages';
  else Result:='?';
  end;
end;

procedure CopyAnchorBounds(Source, Target: TControl);
var
  a: TAnchorKind;
begin
  Target.DisableAutoSizing;
  Target.BoundsRect:=Source.BoundsRect;
  Target.Anchors:=Source.Anchors;
  Target.Align:=Source.Align;
  for a:=low(TAnchorKind) to high(TAnchorKind) do
    Target.AnchorSide[a].Assign(Source.AnchorSide[a]);
  Target.EnableAutoSizing;
end;

procedure AnchorAndChangeBounds(AControl: TControl; Side: TAnchorKind;
  Target: TControl);
begin
  if Target=AControl.Parent then begin
    AControl.AnchorParallel(Side,0,Target);
    case Side of
    akTop: AControl.Top:=0;
    akLeft: AControl.Left:=0;
    akRight: AControl.Width:=AControl.Parent.ClientWidth-AControl.Left;
    akBottom: AControl.Height:=AControl.Parent.ClientHeight-AControl.Top;
    end;
  end else begin
    AControl.AnchorToNeighbour(Side,0,Target);
    case Side of
    akTop: AControl.Top:=Target.Top+Target.Height;
    akLeft: AControl.Left:=Target.Left+Target.Width;
    akRight: AControl.Width:=Target.Left-AControl.Width;
    akBottom: AControl.Height:=Target.Top-AControl.Height;
    end;
  end;
end;

function ControlsLeftTopOnScreen(AControl: TControl): TPoint;
begin
  if AControl.Parent<>nil then begin
    Result:=AControl.Parent.ClientOrigin;
    inc(Result.X,AControl.Left);
    inc(Result.Y,AControl.Top);
  end else begin
    Result:=AControl.Parent.ClientOrigin;
  end;
end;

function GetDockSplitter(Control: TControl; Side: TAnchorKind; out
  Splitter: TAnchorDockSplitter): boolean;
begin
  Result:=false;
  Splitter:=nil;
  if not (Side in Control.Anchors) then exit;
  Splitter:=TAnchorDockSplitter(Control.AnchorSide[Side].Control);
  if not (Splitter is TAnchorDockSplitter) then begin
    Splitter:=nil;
    exit;
  end;
  if Splitter.Parent<>Control.Parent then exit;
  Result:=true;
end;

function GetDockSplitterOrParent(Control: TControl; Side: TAnchorKind; out
  AnchorControl: TControl): boolean;
begin
  Result:=false;
  AnchorControl:=nil;
  if not (Side in Control.Anchors) then exit;
  AnchorControl:=Control.AnchorSide[Side].Control;
  if (AnchorControl is TAnchorDockSplitter)
  and (AnchorControl.Parent=Control.Parent)
  then
    Result:=true
  else if AnchorControl=Control.Parent then
    Result:=true;
end;

function CountAnchoredControls(Control: TControl; Side: TAnchorKind): Integer;
{ return the number of siblings, that are anchored on Side of Control
  For example: if Side=akLeft it will return the number of controls, which
  right side is anchored to the left of Control }
var
  i: Integer;
  Neighbour: TControl;
begin
  Result:=0;
  for i:=0 to Control.AnchoredControlCount-1 do begin
    Neighbour:=Control.AnchoredControls[i];
    if (OppositeAnchor[Side] in Neighbour.Anchors)
    and (Neighbour.AnchorSide[OppositeAnchor[Side]].Control=Control) then
      inc(Result);
  end;
end;

function NeighbourCanBeShrinked(EnlargeControl, Neighbour: TControl;
  Side: TAnchorKind): boolean;
{ returns true if Neighbour can be shrinked on the opposite side of Side
}
const
  MinControlSize = 20;
var
  Splitter: TAnchorDockSplitter;
begin
  Result:=false;
  if not GetDockSplitter(EnlargeControl,OppositeAnchor[Side],Splitter) then
    exit;
  case Side of
  akLeft: // check if left side of Neighbour can be moved
    Result:=Neighbour.Left+Neighbour.Width
        >EnlargeControl.Left+EnlargeControl.Width+Splitter.Width+MinControlSize;
  akRight: // check if right side of Neighbour can be moved
    Result:=Neighbour.Left+MinControlSize+Splitter.Width<EnlargeControl.Left;
  akTop: // check if top side of Neighbour can be moved
    Result:=Neighbour.Top+Neighbour.Height
       >EnlargeControl.Top+EnlargeControl.Height+Splitter.Height+MinControlSize;
  akBottom: // check if bottom side of Neighbour can be moved
    Result:=Neighbour.Top+MinControlSize+Splitter.Height<EnlargeControl.Top;
  end;
end;

function ControlIsAnchoredIndirectly(StartControl: TControl; Side: TAnchorKind;
  DestControl: TControl): boolean;
{ true if there is an Anchor way from StartControl to DestControl over Side.
  For example:

    +-+|+-+
    |A|||B|
    +-+|+-+

  A is akLeft to B.
  B is akRight to A.
  The splitter is akLeft to B.
  The splitter is akRight to A.
  All other are false.
}
var
  Checked: array of Boolean;
  Parent: TWinControl;

  function Check(ControlIndex: integer): boolean;
  var
    AControl: TControl;
    SideControl: TControl;
    i: Integer;
  begin
    if Checked[ControlIndex] then
      exit(false);
    Checked[ControlIndex]:=true;
    AControl:=Parent.Controls[ControlIndex];
    if AControl=DestControl then exit(true);

    if (Side in AControl.Anchors) then begin
      SideControl:=AControl.AnchorSide[Side].Control;
      if (SideControl<>nil) and Check(Parent.GetControlIndex(SideControl)) then
        exit(true);
    end;
    for i:=0 to AControl.AnchoredControlCount-1 do begin
      if Checked[i] then continue;
      SideControl:=AControl.AnchoredControls[i];
      if OppositeAnchor[Side] in SideControl.Anchors then begin
        if (SideControl.AnchorSide[OppositeAnchor[Side]].Control=AControl)
        and Check(i) then
          exit(true);
      end;
    end;
    Result:=false;
  end;

var
  i: Integer;
begin
  if (StartControl=nil) or (DestControl=nil)
  or (StartControl.Parent=nil)
  or (StartControl.Parent<>DestControl.Parent)
  or (StartControl=DestControl) then
    exit(false);
  Parent:=StartControl.Parent;
  SetLength(Checked,Parent.ControlCount);
  for i:=0 to length(Checked)-1 do Checked[i]:=false;
  Result:=Check(Parent.GetControlIndex(StartControl));
end;

procedure GetAnchorControlsRect(Control: TControl; out
  ARect: TAnchorControlsRect);
var
  a: TAnchorKind;
begin
  for a:=Low(TAnchorKind) to High(TAnchorKind) do
    ARect[a]:=Control.AnchorSide[a].Control;
end;

function GetEnclosingControlRect(ControlList: TFPlist; out
  ARect: TAnchorControlsRect): boolean;
{ ARect will be the minimum TAnchorControlsRect around the controls in the list
  returns true, if there is such a TAnchorControlsRect.

  The controls in ARect will either be the Parent or a TLazDockSplitter
}
var
  Parent: TWinControl;

  function ControlIsValidAnchor(Control: TControl; Side: TAnchorKind): boolean;
  var
    i: Integer;
  begin
    Result:=false;
    if (Control=ARect[Side]) then exit(true);// this allows Parent at the beginning

    if not (Control is TAnchorDockSplitter) then
      exit;// not a splitter
    if (TAnchorDockSplitter(Control).ResizeAnchor in [akLeft,akRight])
      <>(Side in [akLeft,akRight]) then
        exit;// wrong alignment
    if ControlList.IndexOf(Control)>=0 then
      exit;// is an inner control
    if ControlIsAnchoredIndirectly(Control,Side,ARect[Side]) then
      exit; // this anchor would be worse than the current maximum
    for i:=0 to ControlList.Count-1 do begin
      if not ControlIsAnchoredIndirectly(Control,Side,TControl(ControlList[i]))
      then begin
        // this anchor is not above (below, ...) the inner controls
        exit;
      end;
    end;
    Result:=true;
  end;

var
  TopIndex: Integer;
  TopControl: TControl;
  RightIndex: Integer;
  RightControl: TControl;
  BottomIndex: Integer;
  BottomControl: TControl;
  LeftIndex: Integer;
  LeftControl: TControl;
  Candidates: TFPList;
  i: Integer;
  a: TAnchorKind;
begin
  Result:=false;
  if (ControlList=nil) or (ControlList.Count=0) then exit;

  // get Parent
  Parent:=TControl(ControlList[0]).Parent;
  if Parent=nil then exit;
  for i:=0 to ControlList.Count-1 do
    if TControl(ControlList[i]).Parent<>Parent then exit;

  // set the default rect: the Parent
  Result:=true;
  for a:=Low(TAnchorKind) to High(TAnchorKind) do
    ARect[a]:=Parent;

  // find all possible Candidates
  Candidates:=TFPList.Create;
  try
    Candidates.Add(Parent);
    for i:=0 to Parent.ControlCount-1 do
      if Parent.Controls[i] is TAnchorDockSplitter then
        Candidates.Add(Parent.Controls[i]);

    // now check every possible rectangle
    // Note: four loops seems to be dog slow, but the checks
    //       avoid most possibilities early
    for TopIndex:=0 to Candidates.Count-1 do begin
      TopControl:=TControl(Candidates[TopIndex]);
      if not ControlIsValidAnchor(TopControl,akTop) then continue;

      for RightIndex:=0 to Candidates.Count-1 do begin
        RightControl:=TControl(Candidates[RightIndex]);
        if (TopControl.AnchorSide[akRight].Control<>RightControl)
        and (RightControl.AnchorSide[akTop].Control<>TopControl) then
          continue; // not touching / not a corner
        if not ControlIsValidAnchor(RightControl,akRight) then continue;

        for BottomIndex:=0 to Candidates.Count-1 do begin
          BottomControl:=TControl(Candidates[BottomIndex]);
          if (RightControl.AnchorSide[akBottom].Control<>BottomControl)
          and (BottomControl.AnchorSide[akRight].Control<>RightControl) then
            continue; // not touching / not a corner
          if not ControlIsValidAnchor(BottomControl,akBottom) then continue;

          for LeftIndex:=0 to Candidates.Count-1 do begin
            LeftControl:=TControl(Candidates[LeftIndex]);
            if (BottomControl.AnchorSide[akLeft].Control<>LeftControl)
            and (LeftControl.AnchorSide[akBottom].Control<>BottomControl) then
              continue; // not touching / not a corner
            if (TopControl.AnchorSide[akLeft].Control<>LeftControl)
            and (LeftControl.AnchorSide[akTop].Control<>LeftControl) then
              continue; // not touching / not a corner
            if not ControlIsValidAnchor(LeftControl,akLeft) then continue;

            // found a better rectangle
            ARect[akLeft]  :=LeftControl;
            ARect[akRight] :=RightControl;
            ARect[akTop]   :=TopControl;
            ARect[akBottom]:=BottomControl;
          end;
        end;
      end;
    end;
  finally
    Candidates.Free;
  end;
end;

function GetEnclosedControls(const ARect: TAnchorControlsRect): TFPList;
{ return a list of all controls bounded by the anchors in ARect }
var
  Parent: TWinControl;

  procedure Fill(AControl: TControl);
  var
    a: TAnchorKind;
    SideControl: TControl;
    i: Integer;
  begin
    if AControl=nil then exit;
    if AControl=Parent then exit;// do not add Parent
    for a:=Low(TAnchorKind) to High(TAnchorKind) do
      if ARect[a]=AControl then exit;// do not add boundary

    if Result.IndexOf(AControl)>=0 then exit;// already added
    Result.Add(AControl);

    for a:=Low(TAnchorKind) to High(TAnchorKind) do
      Fill(AControl.AnchorSide[a].Control);
    for i:=0 to Parent.ControlCount-1 do begin
      SideControl:=Parent.Controls[i];
      for a:=Low(TAnchorKind) to High(TAnchorKind) do
        if SideControl.AnchorSide[a].Control=AControl then
          Fill(SideControl);
    end;
  end;

var
  i: Integer;
  AControl: TControl;
  LeftTopControl: TControl;
begin
  Result:=TFPList.Create;

  // find the Parent
  if (ARect[akLeft]=ARect[akRight]) and (ARect[akLeft] is TWinControl) then
    Parent:=TWinControl(ARect[akLeft])
  else
    Parent:=ARect[akLeft].Parent;

  // find the left, top most control
  for i:=0 to Parent.ControlCount-1 do begin
    AControl:=Parent.Controls[i];
    if (AControl.AnchorSide[akLeft].Control=ARect[akLeft])
    and (AControl.AnchorSide[akTop].Control=ARect[akTop]) then begin
      LeftTopControl:=AControl;
      break;
    end;
  end;
  if Result.Count=0 then exit;

  // use flood fill to find the rest
  Fill(LeftTopControl);
end;

{ TAnchorDockMaster }

function TAnchorDockMaster.GetControls(Index: integer): TControl;
begin
  Result:=TControl(FControls[Index]);
end;

procedure TAnchorDockMaster.SetHeaderAlignLeft(const AValue: integer);
begin
  if FHeaderAlignLeft=AValue then exit;
  FHeaderAlignLeft:=AValue;
  FHeaderAlignTop:=Min(FHeaderAlignLeft-1,FHeaderAlignTop);
end;

procedure TAnchorDockMaster.SetHeaderAlignTop(const AValue: integer);
begin
  if FHeaderAlignTop=AValue then exit;
  FHeaderAlignTop:=AValue;
  FHeaderAlignLeft:=Max(FHeaderAlignTop+1,FHeaderAlignLeft);
end;

function TAnchorDockMaster.CloseUnneededControls(Tree: TAnchorDockLayoutTree
  ): Boolean;
var
  i: Integer;
  AControl: TControl;
begin
  i:=ControlCount-1;
  while i>=0 do begin
    AControl:=Controls[i];
    if GetParentForm(AControl).IsVisible
    and (Tree.Root.FindChildNode(AControl.Name,true)=nil)
    and (Application.MainForm<>AControl) then begin
      DisableControlAutoSizing(AControl);
      // AControl is currently on a visible site, but not in the Tree
      // => close site
      debugln(['TAnchorDockMaster.CloseUnneededControls Control=',DbgSName(AControl),' Site=',AControl.HostDockSite.Name]);
      if AControl.HostDockSite is TAnchorDockHostSite then begin
        if not TAnchorDockHostSite(AControl.HostDockSite).CloseSite then begin
          if FControls.IndexOf(AControl)<0 then
            AControl:=nil;
          debugln(['TAnchorDockMaster.CloseUnneededControls CloseSite failed Control=',DbgSName(AControl)]);
          exit(false);
        end;
      end;
      if FControls.IndexOf(AControl)>=0 then begin
        // the control is still there
        if AControl.HostDockSite<>nil then begin
          AControl.HostDockSite.Visible:=false;
          AControl.HostDockSite.Parent:=nil;
        end else begin
          AControl.Visible:=False;
          AControl.Parent:=nil;
        end;
      end;
    end;
    i:=Min(i,ControlCount)-1;
  end;
  Result:=true;
end;

function TAnchorDockMaster.CreateNeededControls(Tree: TAnchorDockLayoutTree;
  DisableAutoSizing: boolean; ControlNames: TStrings): boolean;

  procedure CreateControlsForNode(Node: TAnchorDockLayoutTreeNode);
  var
    i: Integer;
    AControl: TControl;
  begin
    if (Node.NodeType in [adltnControl,adltnCustomSite])
    and (Node.Name<>'') then begin
      AControl:=FindControl(Node.Name);
      if AControl<>nil then begin
        //debugln(['CreateControlsForNode ',Node.Name,' already exists']);
        DisableControlAutoSizing(AControl);
      end else begin
        //debugln(['CreateControlsForNode ',Node.Name,' needs creation']);
        AControl:=DoCreateControl(Node.Name,DisableAutoSizing);
        if AControl<>nil then begin
          //debugln(['CreateControlsForNode ',AControl.Name,' created']);
          if fDisabledAutosizing.IndexOf(AControl)<0 then
            fDisabledAutosizing.Add(AControl);
          if Node.NodeType=adltnControl then
            MakeDockable(AControl,false)
          else if not IsCustomSite(AControl) then
            raise EAnchorDockLayoutError.Create('not a docksite: '+DbgSName(AControl));
        end else begin
          debugln(['CreateControlsForNode ',Node.Name,' failed to create']);
        end;
      end;
      if AControl<>nil then
        ControlNames.Add(AControl.Name);
    end;
    for i:=0 to Node.Count-1 do
      CreateControlsForNode(Node[i]);
  end;

begin
  Result:=false;
  CreateControlsForNode(Tree.Root);
  Result:=true;
end;

procedure TAnchorDockMaster.MapTreeToControls(Tree: TAnchorDockLayoutTree);

  procedure MapHostDockSites(Node: TAnchorDockLayoutTreeNode);
  // map in TreeNameToDocker each control name to its HostDockSite or custom dock site
  var
    i: Integer;
    AControl: TControl;
  begin
    if Node.IsSplitter then exit;
    if (Node.NodeType=adltnControl) then begin
      AControl:=FindControl(Node.Name);
      if (AControl<>nil) and (AControl.HostDockSite is TAnchorDockHostSite) then
        fTreeNameToDocker[Node.Name]:=AControl.HostDockSite;
      // ignore kids
      exit;
    end;
    if (Node.NodeType=adltnCustomSite) then begin
      AControl:=FindControl(Node.Name);
      if IsCustomSite(AControl) then
        fTreeNameToDocker[Node.Name]:=AControl;
    end;
    for i:=0 to Node.Count-1 do
      MapHostDockSites(Node[i]); // recursive
  end;

  procedure MapTopLevelSites(Node: TAnchorDockLayoutTreeNode);
  // map in TreeNameToDocker each RootWindow node name to a site whith a
  // corresponding control
  // For example: if there is control on a complex site (SiteA), and the control
  //    has a node in the Tree, then the root node of the tree node is mapped to
  //    the SiteA. This way the corresponding root forms are kept which reduces
  //    flickering.

    function FindMappedControl(ChildNode: TAnchorDockLayoutTreeNode
      ): TCustomForm;
    var
      i: Integer;
    begin
      if ChildNode.NodeType in [adltnControl,adltnCustomSite] then
        Result:=TCustomForm(fTreeNameToDocker[ChildNode.Name])
      else
        for i:=0 to ChildNode.Count-1 do begin
          Result:=FindMappedControl(ChildNode[i]); // search recursive
          if Result<>nil then exit;
        end;
    end;

  var
    i: Integer;
    RootSite: TCustomForm;
    Site: TCustomForm;
  begin
    if Node.IsSplitter then exit;
    if Node.IsRootWindow then begin
      if Node.Name='' then exit;
      if Node.NodeType=adltnControl then exit;
      // Node is a complex site
      if fTreeNameToDocker[Node.Name]<>nil then exit;
      // and not yet mapped to a site
      Site:=FindMappedControl(Node);
      if Site=nil then exit;
      // and there is sub node mapped to a site (anchor or custom)
      RootSite:=GetParentForm(Site);
      if not (RootSite is TAnchorDockHostSite) then exit;
      // and the mapped site has a root site
      if fTreeNameToDocker.ControlToName(RootSite)<>'' then exit;
      // and the root site is not yet mapped
      // => map the root node to the root site
      fTreeNameToDocker[Node.Name]:=RootSite;
    end else
      for i:=0 to Node.Count-1 do
        MapTopLevelSites(Node[i]); // recursive
  end;

  procedure MapBottomUp(Node: TAnchorDockLayoutTreeNode);
  { map the other nodes to existing sites
    The heuristic works like this:
      if a child node was mapped to a site and the site has a parent site then
      map this node to this parent site.
  }
  var
    i: Integer;
    BestSite: TControl;
  begin
    if Node.IsSplitter then exit;
    BestSite:=fTreeNameToDocker[Node.Name];
    for i:=0 to Node.Count-1 do begin
      MapBottomUp(Node[i]); // recursive
      if BestSite=nil then
        BestSite:=fTreeNameToDocker[Node[i].Name];
    end;
    if (fTreeNameToDocker[Node.Name]=nil) and (BestSite<>nil) then begin
      // search the parent site of a child site
      repeat
        BestSite:=BestSite.Parent;
        if BestSite is TAnchorDockHostSite then begin
          if fTreeNameToDocker.ControlToName(BestSite)='' then
            fTreeNameToDocker[Node.Name]:=BestSite;
          break;
        end;
      until (BestSite=nil);
    end;
  end;

  procedure MapSplitters(Node: TAnchorDockLayoutTreeNode);
  { map the splitter nodes to existing splitters
    The heuristic works like this:
      If a node is mapped to a site and the node is at Side anchored to a
      splitter node and the site is anchored at Side to a splitter then
      map the the splitter node to the splitter.
  }
  var
    i: Integer;
    Side: TAnchorKind;
    Site: TControl;
    SplitterNode: TAnchorDockLayoutTreeNode;
    Splitter: TControl;
  begin
    if Node.IsSplitter then exit;
    for i:=0 to Node.Count-1 do
      MapSplitters(Node[i]); // recursive

    if Node.Parent=nil then exit;
    // node is a child node
    Site:=fTreeNameToDocker[Node.Name];
    if Site=nil then exit;
    // node is mapped to a site
    // check each side
    for Side:=Low(TAnchorKind) to high(TAnchorKind) do begin
      if Node.Anchors[Side]='' then continue;
      SplitterNode:=Node.Parent.FindChildNode(Node.Anchors[Side],false);
      if (SplitterNode=nil) then continue;
      // this side of node is anchored to a splitter node
      if fTreeNameToDocker[SplitterNode.Name]<>nil then continue;
      // the splitter node is not yet mapped
      Splitter:=Site.AnchorSide[Side].Control;
      if (not (Splitter is TAnchorDockSplitter))
      or (Splitter.Parent<>Site.Parent) then continue;
      // there is an unmapped splitter anchored to the Site
      // => map the splitter to the splitter node
      fTreeNameToDocker[Splitter.Name]:=Splitter;
    end;
  end;

begin
  MapHostDockSites(Tree.Root);
  MapTopLevelSites(Tree.Root);
  MapBottomUp(Tree.Root);
  MapSplitters(Tree.Root);
end;

function TAnchorDockMaster.RestoreLayout(Tree: TAnchorDockLayoutTree;
  Scale: boolean): boolean;
var
  WorkArea: TRect;

  function SrcRectValid(const r: TRect): boolean;
  begin
    Result:=(r.Left<r.Right) and (r.Top<r.Bottom);
  end;

  function ScaleX(p: integer; const SrcRect: TRect): integer;
  begin
    Result:=p;
    if SrcRectValid(SrcRect) and SrcRectValid(WorkArea) then
      Result:=((p-SrcRect.Left)*(WorkArea.Right-WorkArea.Left))
                div (SrcRect.Right-SrcRect.Left)
              +WorkArea.Left;
  end;

  function ScaleY(p: integer; const SrcRect: TRect): integer;
  begin
    Result:=p;
    if SrcRectValid(SrcRect) and SrcRectValid(WorkArea) then
      Result:=((p-SrcRect.Top)*(WorkArea.Bottom-WorkArea.Top))
                   div (SrcRect.Bottom-SrcRect.Top)
              +WorkArea.Top;
  end;

  procedure SetupSite(Site: TCustomForm;
    Node: TAnchorDockLayoutTreeNode; Parent: TWinControl;
    const SrcRect: TRect);
  var
    aManager: TAnchorDockManager;
    NewBounds: TRect;
  begin
    if Parent=nil then begin
      WorkArea:=Site.Monitor.WorkareaRect;
      {$IFDEF VerboseAnchorDockRestore}
      debugln(['TAnchorDockMaster.RestoreLayout.SetupSite WorkArea=',dbgs(WorkArea)]);
      {$ENDIF}
    end;
    if IsCustomSite(Site) then begin
      aManager:=TAnchorDockManager(Site.DockManager);
      if Node.Count>0 then begin
        // this custom dock site gets a child => store and clear constraints
        aManager.StoreConstraints;
      end;
    end;
    Site.Constraints.MaxWidth:=0;
    Site.Constraints.MaxHeight:=0;
    NewBounds:=Node.BoundsRect;
    if Parent=nil then begin
      NewBounds:=Rect(ScaleX(NewBounds.Left,SrcRect),ScaleY(NewBounds.Top,SrcRect),
                      ScaleX(NewBounds.Right,SrcRect),ScaleY(NewBounds.Bottom,SrcRect));
      {$IFDEF VerboseAnchorDockRestore}
      debugln(['TAnchorDockMaster.RestoreLayout.SetupSite scale Site=',DbgSName(Site),' OldWorkArea=',dbgs(SrcRect),' CurWorkArea=',dbgs(WorkArea),' OldBounds=',dbgs(Node.BoundsRect),' NewBounds=',dbgs(NewBounds)]);
      {$ENDIF}
    end;
    Site.BoundsRect:=NewBounds;
    Site.Visible:=true;
    Site.Parent:=Parent;
    if IsCustomSite(Parent) then begin
      aManager:=TAnchorDockManager(Parent.DockManager);
      Site.Align:=Node.Align;
      {$IFDEF VerboseAnchorDockRestore}
      debugln(['TAnchorDockMaster.RestoreLayout.SetupSite custom Site=',DbgSName(Site),' Site.Bounds=',dbgs(Site.BoundsRect),' BoundSplitterPos=',Node.BoundSplitterPos]);
      {$ENDIF}
      aManager.RestoreSite(Node.BoundSplitterPos);
      Site.HostDockSite:=Parent;
    end;
    if Site is TAnchorDockHostSite then begin
      TAnchorDockHostSite(Site).Header.HeaderPosition:=Node.HeaderPosition;
      TAnchorDockHostSite(Site).DockRestoreBounds:=NewBounds;
    end;
    if Parent=nil then begin
      Site.WindowState:=Node.WindowState;
      if (Node.Monitor>=0) and (Node.Monitor<Screen.MonitorCount) then
      begin
        // ToDo: move to monitor
      end;
    end;
  end;

  function Restore(Node: TAnchorDockLayoutTreeNode; Parent: TWinControl;
    SrcRect: TRect): TControl;
  var
    AControl: TControl;
    Site: TAnchorDockHostSite;
    Splitter: TAnchorDockSplitter;
    i: Integer;
    Side: TAnchorKind;
    AnchorControl: TControl;
    ChildNode: TAnchorDockLayoutTreeNode;
    NewBounds: TRect;
  begin
    Result:=nil;
    if Scale and SrcRectValid(Node.WorkAreaRect) then
      SrcRect:=Node.WorkAreaRect;
    {$IFDEF VerboseAnchorDockRestore}
    debugln(['TAnchorDockMaster.RestoreLayout.Restore ',Node.Name,' ',dbgs(Node.NodeType),' Bounds=',dbgs(Node.BoundsRect),' Parent=',DbgSName(Parent),' ']);
    {$ENDIF}
    if Node.NodeType=adltnControl then begin
      // restore control
      // the control was already created
      // => dock it
      AControl:=FindControl(Node.Name);
      if AControl=nil then begin
        debugln(['TAnchorDockMaster.RestoreLayout.Restore can not find control ',Node.Name]);
        exit;
      end;
      DisableControlAutoSizing(AControl);
      if AControl.HostDockSite=nil then
        MakeDockable(AControl,false)
      else
        ClearLayoutProperties(AControl);
      Site:=AControl.HostDockSite as TAnchorDockHostSite;
      {$IFDEF VerboseAnchorDockRestore}
      debugln(['TAnchorDockMaster.RestoreLayout.Restore Control Node.Name=',Node.Name,' Control=',DbgSName(AControl),' Site=',DbgSName(Site)]);
      {$ENDIF}
      AControl.Visible:=true;
      SetupSite(Site,Node,Parent,SrcRect);
      Result:=Site;
    end else if Node.NodeType=adltnCustomSite then begin
      // restore custom dock site
      // the control was already created
      // => position it
      AControl:=FindControl(Node.Name);
      if AControl=nil then begin
        debugln(['TAnchorDockMaster.RestoreLayout.Restore WARNING: can not find control ',Node.Name]);
        exit;
      end;
      if not IsCustomSite(AControl) then begin
        debugln(['TAnchorDockMaster.RestoreLayout.Restore WARNING: ',Node.Name,' is not a custom dock site ',DbgSName(AControl)]);
        exit;
      end;
      DisableControlAutoSizing(AControl);
      SetupSite(TCustomForm(AControl),Node,nil,SrcRect);
      Result:=AControl;
      // restore docked site
      if Node.Count>0 then begin
        Restore(Node[0],TCustomForm(AControl),SrcRect);
      end;
    end else if Node.IsSplitter then begin
      // restore splitter
      Splitter:=TAnchorDockSplitter(fTreeNameToDocker[Node.Name]);
      if Splitter=nil then begin
        Splitter:=CreateSplitter;
        fTreeNameToDocker[Node.Name]:=Splitter;
      end;
      {$IFDEF VerboseAnchorDockRestore}
      debugln(['TAnchorDockMaster.RestoreLayout.Restore Splitter Node.Name=',Node.Name,' ',dbgs(Node.NodeType),' Splitter=',DbgSName(Splitter)]);
      {$ENDIF}
      Splitter.Parent:=Parent;
      NewBounds:=Node.BoundsRect;
      if SrcRectValid(SrcRect) then
        NewBounds:=Bounds(ScaleX(NewBounds.Left,SrcRect),ScaleX(NewBounds.Top,SrcRect),
          NewBounds.Right-NewBounds.Left,NewBounds.Bottom-NewBounds.Top);
      Splitter.DockRestoreBounds:=NewBounds;
      Splitter.BoundsRect:=NewBounds;
      if Node.NodeType=adltnSplitterVertical then
        Splitter.ResizeAnchor:=akLeft
      else
        Splitter.ResizeAnchor:=akTop;
      Result:=Splitter;
    end else if Node.NodeType=adltnLayout then begin
      // restore layout
      Site:=TAnchorDockHostSite(fTreeNameToDocker[Node.Name]);
      if Site=nil then begin
        Site:=CreateSite;
        fDisabledAutosizing.Add(Site);
        fTreeNameToDocker[Node.Name]:=Site;
      end;
      {$IFDEF VerboseAnchorDockRestore}
      debugln(['TAnchorDockMaster.RestoreLayout.Restore Layout Node.Name=',Node.Name,' ChildCount=',Node.Count]);
      {$ENDIF}
      Site.BeginUpdateLayout;
      try
        SetupSite(Site,Node,Parent,SrcRect);
        Site.FSiteType:=adhstLayout;
        Site.Header.Parent:=nil;
        // create children
        for i:=0 to Node.Count-1 do
          Restore(Node[i],Site,SrcRect);
        // anchor children
        for i:=0 to Node.Count-1 do begin
          ChildNode:=Node[i];
          AControl:=fTreeNameToDocker[ChildNode.Name];
          {$IFDEF VerboseAnchorDockRestore}
          debugln(['  Restore layout child anchors Site=',DbgSName(Site),' ChildNode.Name=',ChildNode.Name,' Control=',DbgSName(AControl)]);
          {$ENDIF}
          if AControl=nil then continue;
          for Side:=Low(TAnchorKind) to high(TAnchorKind) do begin
            if ((ChildNode.NodeType=adltnSplitterHorizontal)
                and (Side in [akTop,akBottom]))
            or ((ChildNode.NodeType=adltnSplitterVertical)
                and (Side in [akLeft,akRight]))
            then continue;
            AnchorControl:=nil;
            if ChildNode.Anchors[Side]<>'' then
              AnchorControl:=fTreeNameToDocker[ChildNode.Anchors[Side]];
            if AnchorControl<>nil then
              AControl.AnchorToNeighbour(Side,0,AnchorControl)
            else
              AControl.AnchorParallel(Side,0,Site);
          end;
        end;
      finally
        Site.EndUpdateLayout;
      end;
      Result:=Site;
    end else if Node.NodeType=adltnPages then begin
      // restore pages
      Site:=TAnchorDockHostSite(fTreeNameToDocker[Node.Name]);
      if Site=nil then begin
        Site:=CreateSite;
        fDisabledAutosizing.Add(Site);
        fTreeNameToDocker[Node.Name]:=Site;
      end;
      Site.BeginUpdateLayout;
      try
        SetupSite(Site,Node,Parent,SrcRect);
        Site.FSiteType:=adhstPages;
        Site.Header.Parent:=nil;
        Site.CreatePages;
        for i:=0 to Node.Count-1 do begin
          Site.Pages.Pages.Add(Node[i].Name);
          AControl:=Restore(Node[i],Site.Pages.Page[i],SrcRect);
          if AControl=nil then continue;
          AControl.Align:=alClient;
          for Side:=Low(TAnchorKind) to high(TAnchorKind) do
            AControl.AnchorSide[Side].Control:=nil;
        end;
      finally
        Site.EndUpdateLayout;
      end;
      Result:=Site;
    end else begin
      // create children
      for i:=0 to Node.Count-1 do
        Restore(Node[i],Parent,SrcRect);
    end;
  end;

begin
  Result:=true;
  WorkArea:=Rect(0,0,0,0);
  Restore(Tree.Root,nil,Rect(0,0,0,0));
  Restoring:=true;
end;

function TAnchorDockMaster.DoCreateControl(aName: string;
  DisableAutoSizing: boolean): TControl;
begin
  Result:=nil;
  OnCreateControl(Self,aName,Result,DisableAutoSizing);
  if Result=nil then
    debugln(['TAnchorDockMaster.DoCreateControl WARNING: control not found: "',aName,'"']);
  if (Result<>nil) and (Result.Name<>aName) then
    raise Exception.Create('TAnchorDockMaster.DoCreateControl'+Format(
      adrsRequestedButCreated, [aName, Result.Name]));
end;

procedure TAnchorDockMaster.DisableControlAutoSizing(AControl: TControl);
begin
  if fDisabledAutosizing.IndexOf(AControl)>=0 then exit;
  //debugln(['TAnchorDockMaster.DisableControlAutoSizing ',DbgSName(AControl)]);
  fDisabledAutosizing.Add(AControl);
  AControl.FreeNotification(Self);
  AControl.DisableAutoSizing;
end;

procedure TAnchorDockMaster.EnableAllAutoSizing;
var
  i: Integer;
  AControl: TControl;
begin
  i:=fDisabledAutosizing.Count-1;
  while (i>=0) do begin
    //debugln(['TAnchorDockMaster.EnableAllAutoSizing ',DbgSName(TControl(fDisabledAutosizing[i]))]);
    AControl:=TControl(fDisabledAutosizing[i]);
    fDisabledAutosizing.Delete(i);
    AControl.EnableAutoSizing;
    i:=Min(i,fDisabledAutosizing.Count)-1;
  end;
end;

procedure TAnchorDockMaster.ClearLayoutProperties(AControl: TControl);
var
  a: TAnchorKind;
begin
  AControl.AutoSize:=false;
  AControl.Align:=alClient;
  AControl.BorderSpacing.Around:=0;
  AControl.BorderSpacing.Left:=0;
  AControl.BorderSpacing.Top:=0;
  AControl.BorderSpacing.Right:=0;
  AControl.BorderSpacing.Bottom:=0;
  AControl.BorderSpacing.InnerBorder:=0;
  for a:=Low(TAnchorKind) to High(TAnchorKind) do
    AControl.AnchorSide[a].Control:=nil;
end;

procedure TAnchorDockMaster.PopupMenuPopup(Sender: TObject);
var
  Popup: TPopupMenu;
  ChangeLockItem: TMenuItem;
begin
  if not (Sender is TPopupMenu) then exit;
  Popup:=TPopupMenu(Sender);
  Popup.Items.Clear;

  // top popup menu item can be clicked by accident, so use something simple:
  // lock/unlock
  ChangeLockItem:=AddPopupMenuItem('ChangeLockMenuItem', adrsLocked,
                                    @ChangeLockButtonClick);
  ChangeLockItem.Checked:=not AllowDragging;
  ChangeLockItem.ShowAlwaysCheckable:=true;

  if Popup.PopupComponent is TAnchorDockHeader then
    TAnchorDockHeader(Popup.PopupComponent).PopupMenuPopup(Sender)
  else if Popup.PopupComponent is TAnchorDockPageControl then
    TAnchorDockPageControl(Popup.PopupComponent).PopupMenuPopup(Sender);

  if Assigned(OnShowOptions) then
    AddPopupMenuItem('OptionsMenuItem', adrsDockingOptions, @OptionsClick);
end;

procedure TAnchorDockMaster.SetHideHeaderCaptionFloatingControl(
  const AValue: boolean);
var
  Site: TAnchorDockHostSite;
  i: Integer;
begin
  if AValue=HideHeaderCaptionFloatingControl then exit;
  fHideHeaderCaptionFloatingControl:=AValue;
  for i:=0 to ComponentCount-1 do begin
    Site:=TAnchorDockHostSite(Components[i]);
    if not (Site is TAnchorDockHostSite) then continue;
    Site.UpdateDockCaption;
  end;
end;

procedure TAnchorDockMaster.SetSplitterWidth(const AValue: integer);
var
  i: Integer;
  Splitter: TAnchorDockSplitter;
begin
  if (AValue<1) or (AValue=SplitterWidth) then exit;
  FSplitterWidth:=AValue;
  for i:=0 to ComponentCount-1 do begin
    Splitter:=TAnchorDockSplitter(Components[i]);
    if not (Splitter is TAnchorDockSplitter) then continue;
    if Splitter.ResizeAnchor in [akLeft,akRight] then
      Splitter.Width:=SplitterWidth
    else
      Splitter.Height:=SplitterWidth;
  end;
end;

procedure TAnchorDockMaster.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if Done then ;
  IdleConnected:=false;
  Restoring:=false;
end;

procedure TAnchorDockMaster.AsyncSimplify(Data: PtrInt);
begin
  FQueueSimplify:=false;
  SimplifyPendingLayouts;
end;

procedure TAnchorDockMaster.ChangeLockButtonClick(Sender: TObject);
begin
  AllowDragging:=not AllowDragging;
end;

procedure TAnchorDockMaster.OptionsClick(Sender: TObject);
begin
  if Assigned(OnShowOptions) then OnShowOptions(Self);
end;

procedure TAnchorDockMaster.SetIdleConnected(const AValue: Boolean);
begin
  if FIdleConnected=AValue then exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle,true)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TAnchorDockMaster.SetQueueSimplify(const AValue: Boolean);
begin
  if FQueueSimplify=AValue then exit;
  FQueueSimplify:=AValue;
  if FQueueSimplify then
    Application.QueueAsyncCall(@AsyncSimplify,0)
  else
    Application.RemoveAsyncCalls(Self);
end;

procedure TAnchorDockMaster.SetRestoring(const AValue: boolean);
var
  AComponent: TComponent;
  i: Integer;
begin
  if FRestoring=AValue then exit;
  FRestoring:=AValue;
  if FRestoring then begin
    IdleConnected:=true;
  end else begin
    for i:=0 to ComponentCount-1 do begin
      AComponent:=Components[i];
      if AComponent is TAnchorDockHostSite then
        TAnchorDockHostSite(AComponent).DockRestoreBounds:=Rect(0,0,0,0)
      else if AComponent is TAnchorDockSplitter then
        TAnchorDockSplitter(AComponent).DockRestoreBounds:=Rect(0,0,0,0)
    end;
  end;
end;

procedure TAnchorDockMaster.SetHeaderButtonSize(const AValue: integer);
begin
  if FHeaderButtonSize=AValue then exit;
  FHeaderButtonSize:=Max(1,AValue);
  FreeCloseButtonBitmap;
  AutoSizeAllHeaders(true);
end;

procedure TAnchorDockMaster.SetShowHeaderCaption(const AValue: boolean);
var
  i: Integer;
  Site: TAnchorDockHostSite;
begin
  if FShowHeaderCaption=AValue then exit;
  FShowHeaderCaption:=AValue;
  for i:=0 to ComponentCount-1 do begin
    Site:=TAnchorDockHostSite(Components[i]);
    if not (Site is TAnchorDockHostSite) then continue;
    Site.UpdateDockCaption;
  end;
end;

procedure TAnchorDockMaster.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  AControl: TControl;
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if AComponent is TControl then begin
      AControl:=TControl(AComponent);
      FControls.Remove(AControl);
      fNeedSimplify.Remove(AControl);
      fNeedFree.Remove(AControl);
      fDisabledAutosizing.Remove(AControl);
      if fTreeNameToDocker<>nil then fTreeNameToDocker.RemoveControl(AControl);
    end;
  end;
end;

procedure TAnchorDockMaster.FreeCloseButtonBitmap;
begin
  fCloseBtnBitmap.Free;
end;

procedure TAnchorDockMaster.AutoSizeAllHeaders(EnableAutoSizing: boolean);
var
  i: Integer;
  Site: TAnchorDockHostSite;
begin
  for i:=0 to ComponentCount-1 do begin
    Site:=TAnchorDockHostSite(Components[i]);
    if not (Site is TAnchorDockHostSite) then continue;
    if (Site.Header<>nil) and (Site.Header.Parent<>nil) then begin
      Site.Header.InvalidatePreferredSize;
      DisableControlAutoSizing(Site);
    end;
  end;
  if EnableAutoSizing then
    EnableAllAutoSizing;
end;

procedure TAnchorDockMaster.CreateCloseButtonBitmap;
var
  BitmapHandle,MaskHandle: HBITMAP;
  OrigBitmap: TCustomBitmap;
begin
  if fCloseBtnBitmap<>nil then exit;

  if ThemeServices.GetStockImage(idButtonClose,BitmapHandle,MaskHandle) then begin
    OrigBitmap:=TBitmap.Create;
    OrigBitmap.Handle:=BitmapHandle;
    if MaskHandle<>0 then
      OrigBitmap.MaskHandle:=MaskHandle;
  end
  else
    OrigBitmap := GetDefaultButtonIcon(idButtonClose);
  DockMaster.fCloseBtnBitmap:=TBitmap.Create;
  with DockMaster.fCloseBtnBitmap do begin
    SetSize(HeaderButtonSize,HeaderButtonSize);
    Canvas.Brush.Color:=clWhite;
    Canvas.FillRect(Rect(0,0,Width,Height));
    Canvas.StretchDraw(Rect(0,0,Width,Height),OrigBitmap);
    Transparent:=true;
    TransparentColor:=clWhite;
  end;
  OrigBitmap.Free;
end;

constructor TAnchorDockMaster.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControls:=TFPList.Create;
  FAllowDragging:=true;
  FHeaderButtonSize:=10;
  FDragTreshold:=4;
  FDockOutsideMargin:=10;
  FDockParentMargin:=10;
  FPageAreaInPercent:=40;
  FHeaderAlignTop:=80;
  HeaderAlignLeft:=120;
  FHeaderHint:=adrsDragAndDockC;
  FShowHeaderCaption:=true;
  FHideHeaderCaptionFloatingControl:=true;
  FSplitterWidth:=4;
  FScaleOnResize:=true;
  fNeedSimplify:=TFPList.Create;
  fNeedFree:=TFPList.Create;
  fDisabledAutosizing:=TFPList.Create;
  FSplitterClass:=TAnchorDockSplitter;
  FSiteClass:=TAnchorDockHostSite;
  FManagerClass:=TAnchorDockManager;
  FHeaderClass:=TAnchorDockHeader;
  FPageControlClass:=TAnchorDockPageControl;
  FPageClass:=TAnchorDockPage;
  FRestoreLayouts:=TAnchorDockRestoreLayouts.Create;
end;

destructor TAnchorDockMaster.Destroy;
var
  AControl: TControl;
  i: Integer;
begin
  QueueSimplify:=false;
  FreeAndNil(FRestoreLayouts);
  FreeAndNil(fPopupMenu);
  FreeAndNil(fTreeNameToDocker);
  if FControls.Count>0 then begin
    while ControlCount>0 do begin
      AControl:=Controls[ControlCount-1];
      debugln(['TAnchorDockMaster.Destroy: still in list: ',DbgSName(AControl),' Caption="',AControl.Caption,'"']);
      AControl.Free;
    end;
  end;
  FreeAndNil(fNeedSimplify);
  FreeAndNil(FControls);
  FreeAndNil(fNeedFree);
  FreeAndNil(fDisabledAutosizing);
  fCloseBtnReferenceCount:=-1;
  FreeAndNil(fCloseBtnBitmap);
  for i:=0 to ComponentCount-1 do begin
    debugln(['TAnchorDockMaster.Destroy ',i,'/',ComponentCount,' ',DbgSName(Components[i])]);
  end;
  inherited Destroy;
end;

function TAnchorDockMaster.ControlCount: integer;
begin
  Result:=FControls.Count;
end;

function TAnchorDockMaster.IndexOfControl(const aName: string): integer;
begin
  Result:=ControlCount-1;
  while (Result>=0) and (Controls[Result].Name<>aName) do dec(Result);
end;

function TAnchorDockMaster.FindControl(const aName: string): TControl;
var
  i: LongInt;
begin
  i:=IndexOfControl(aName);
  if i>=0 then
    Result:=Controls[i]
  else
    Result:=nil;
end;

function TAnchorDockMaster.IsSite(AControl: TControl): boolean;
begin
  Result:=(AControl is TAnchorDockHostSite) or IsCustomSite(AControl);
end;

function TAnchorDockMaster.IsAnchorSite(AControl: TControl): boolean;
begin
  Result:=AControl is TAnchorDockHostSite;
end;

function TAnchorDockMaster.IsCustomSite(AControl: TControl): boolean;
begin
  Result:=(AControl is TCustomForm) // also checks for nil
      and (AControl.Parent=nil)
      and (TCustomForm(AControl).DockManager is TAnchorDockManager);
end;

function TAnchorDockMaster.GetSite(AControl: TControl): TCustomForm;
begin
  Result:=nil;
  if AControl=nil then
    exit
  else if IsCustomSite(AControl) then
    Result:=TCustomForm(AControl)
  else if AControl is TAnchorDockHostSite then
    Result:=TAnchorDockHostSite(AControl)
  else if (AControl.HostDockSite is TAnchorDockHostSite) then
    Result:=TAnchorDockHostSite(AControl.HostDockSite);
end;

function TAnchorDockMaster.GetAnchorSite(AControl: TControl): TAnchorDockHostSite;
begin
  Result:=nil;
  if AControl=nil then
    Result:=nil
  else if AControl is TAnchorDockHostSite then
    Result:=TAnchorDockHostSite(AControl)
  else if (AControl.HostDockSite is TAnchorDockHostSite) then
    Result:=TAnchorDockHostSite(AControl.HostDockSite);
end;

function TAnchorDockMaster.GetControl(Site: TControl): TControl;
var
  AnchorSite: TAnchorDockHostSite;
begin
  Result:=nil;
  if IsCustomSite(Site) then
    Result:=Site
  else if Site is TAnchorDockHostSite then begin
    AnchorSite:=TAnchorDockHostSite(Site);
    if AnchorSite.SiteType=adhstOneControl then
      Result:=AnchorSite.GetOneControl;
  end else if (Site<>nil) and (Site.HostDockSite is TAnchorDockHostSite)
  and (TAnchorDockHostSite(Site.HostDockSite).SiteType=adhstOneControl) then
    Result:=Site;
end;

function TAnchorDockMaster.IsFloating(AControl: TControl): Boolean;
begin
  if AControl is TAnchorDockHostSite then begin
    Result:=(TAnchorDockHostSite(AControl).SiteType=adhstOneControl)
            and (AControl.Parent=nil);
  end else if (AControl.HostDockSite is TAnchorDockHostSite) then begin
    Result:=(TAnchorDockHostSite(AControl.HostDockSite).SiteType=adhstOneControl)
        and (AControl.HostDockSite.Parent=nil);
  end else
    Result:=AControl.Parent=nil;
end;

function TAnchorDockMaster.GetPopupMenu: TPopupMenu;
begin
  if fPopupMenu=nil then begin
    fPopupMenu:=TPopupMenu.Create(Self);
    fPopupMenu.OnPopup:=@PopupMenuPopup;
  end;
  Result:=fPopupMenu;
end;

function TAnchorDockMaster.AddPopupMenuItem(AName, ACaption: string;
  const OnClickEvent: TNotifyEvent; AParent: TMenuItem): TMenuItem;
begin
  Result:=TMenuItem(fPopupMenu.FindComponent(AName));
  if Result=nil then begin
    Result:=TMenuItem.Create(fPopupMenu);
    Result.Name:=AName;
    if AParent=nil then
      fPopupMenu.Items.Add(Result)
    else
      AParent.Add(Result);
  end;
  Result.Caption:=ACaption;
  Result.OnClick:=OnClickEvent;
end;

function TAnchorDockMaster.AddRemovePopupMenuItem(Add: boolean; AName,
  ACaption: string; const OnClickEvent: TNotifyEvent; AParent: TMenuItem
  ): TMenuItem;
begin
  if Add then
    Result:=AddPopupMenuItem(AName,ACaption,OnClickEvent,AParent)
  else begin
    Result:=TMenuItem(fPopupMenu.FindComponent(AName));
    if Result<>nil then
      FreeAndNil(Result);
  end;
end;

procedure TAnchorDockMaster.MakeDockable(AControl: TControl; Show: boolean;
  BringToFront: boolean; AddDockHeader: boolean);
var
  Site: TAnchorDockHostSite;
begin
  if AControl.Name='' then
    raise Exception.Create('TAnchorDockMaster.MakeDockable '+
      adrsMissingControlName);
  if (AControl is TCustomForm) and (fsModal in TCustomForm(AControl).FormState)
  then
    raise Exception.Create('TAnchorDockMaster.MakeDockable '+
      adrsModalFormsCanNotBeMadeDockable);
  if IsCustomSite(AControl) then
    raise Exception.Create('TAnchorDockMaster.MakeDockable '+
      adrsControlIsAlreadyADocksite);
  Site:=nil;
  AControl.DisableAutoSizing;
  try
    if AControl is TAnchorDockHostSite then begin
      // already a site
      Site:=TAnchorDockHostSite(AControl);
    end else if AControl.Parent=nil then begin

      if FControls.IndexOf(AControl)<0 then begin
        FControls.Add(AControl);
        AControl.FreeNotification(Self);
      end;

      // create docksite
      Site:=CreateSite;
      try
        try
          Site.BoundsRect:=AControl.BoundsRect;
          ClearLayoutProperties(AControl);
          // dock
          AControl.ManualDock(Site);
          AControl.Visible:=true;
          if not AddDockHeader then
            Site.Header.Parent:=nil;
        except
          FreeAndNil(Site);
          raise;
        end;
      finally
        if Site<>nil then
          Site.EnableAutoSizing;
      end;
    end else if AControl.Parent is TAnchorDockHostSite then begin
      // AControl is already docked => show site
      Site:=TAnchorDockHostSite(AControl.Parent);
      AControl.Visible:=true;
    end else begin
      raise Exception.Create('TAnchorDockMaster.MakeDockable '+Format(
        adrsNotSupportedHasParent, [DbgSName(AControl), DbgSName(AControl)]));
    end;
    if (Site<>nil) and Show then
      MakeVisible(Site,BringToFront);
  finally
    AControl.EnableAutoSizing;
  end;
  // BringToFront
  if BringToFront and (Site<>nil) then begin
    GetParentForm(Site).BringToFront;
    Site.SetFocus;
  end;
end;

procedure TAnchorDockMaster.MakeDockSite(AForm: TCustomForm; Sites: TAnchors;
  ResizePolicy: TADMResizePolicy; AllowInside: boolean);
var
  AManager: TAnchorDockManager;
begin
  if AForm.Name='' then
    raise Exception.Create('TAnchorDockMaster.MakeDockable '+
      adrsMissingControlName);
  if AForm.DockManager<>nil then
    raise Exception.Create('TAnchorDockMaster.MakeDockSite DockManager<>nil');
  if AForm.Parent<>nil then
    raise Exception.Create('TAnchorDockMaster.MakeDockSite Parent='+DbgSName(AForm.Parent));
  if fsModal in AForm.FormState then
    raise Exception.Create('TAnchorDockMaster.MakeDockSite '+
      adrsModalFormsCanNotBeMadeDockable);
  if Sites=[] then
    raise Exception.Create('TAnchorDockMaster.MakeDockSite Sites=[]');
  AForm.DisableAutoSizing;
  try
    if FControls.IndexOf(AForm)<0 then begin
      FControls.Add(AForm);
      AForm.FreeNotification(Self);
    end;
    AManager:=ManagerClass.Create(AForm);
    AManager.DockableSites:=Sites;
    AManager.InsideDockingAllowed:=AllowInside;
    AManager.ResizePolicy:=ResizePolicy;
    AForm.DockManager:=AManager;
    AForm.UseDockManager:=true;
    AForm.DockSite:=true;
  finally
    AForm.EnableAutoSizing;
  end;
end;

procedure TAnchorDockMaster.MakeVisible(AControl: TControl; SwitchPages: boolean);
begin
  while AControl<>nil do begin
    AControl.Visible:=true;
    if SwitchPages and (AControl is TAnchorDockPage) then
      TAnchorDockPageControl(AControl.Parent).PageIndex:=
        TAnchorDockPage(AControl).PageIndex;
    AControl:=AControl.Parent;
  end;
end;

function TAnchorDockMaster.ShowControl(ControlName: string;
  BringToFront: boolean): TControl;
begin
  Result:=DoCreateControl(ControlName,false);
  if Result=nil then exit;
  MakeDockable(Result,true,BringToFront);
end;

procedure TAnchorDockMaster.CloseAll;
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
    i:=Min(i,Screen.CustomFormCount)-1;
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
    i:=Min(i,Screen.CustomFormCount)-1;
  end;
end;

procedure TAnchorDockMaster.SaveMainLayoutToTree(LayoutTree: TAnchorDockLayoutTree);
var
  i: Integer;
  AControl: TControl;
  Site: TAnchorDockHostSite;
  SavedSites: TFPList;
  LayoutNode: TAnchorDockLayoutTreeNode;
  AForm: TCustomForm;
  VisibleControls: TStringList;
begin
  SavedSites:=TFPList.Create;
  VisibleControls:=TStringList.Create;
  try
    for i:=0 to ControlCount-1 do begin
      AControl:=Controls[i];
      if not AControl.IsVisible then continue;
      VisibleControls.Add(AControl.Name);
      AForm:=GetParentForm(AControl);
      if AForm=nil then continue;
      if SavedSites.IndexOf(AForm)>=0 then continue;
      SavedSites.Add(AForm);
      debugln(['TAnchorDockMaster.SaveMainLayoutToTree AForm=',DbgSName(AForm)]);
      DebugWriteChildAnchors(AForm,true,true);
      if (AForm is TAnchorDockHostSite) then begin
        Site:=TAnchorDockHostSite(AForm);
        LayoutNode:=LayoutTree.NewNode(LayoutTree.Root);
        Site.SaveLayout(LayoutTree,LayoutNode);
      end else if IsCustomSite(AForm) then begin
        // custom dock site
        LayoutNode:=LayoutTree.NewNode(LayoutTree.Root);
        LayoutNode.NodeType:=adltnCustomSite;
        LayoutNode.Assign(AForm);
        // can have one normal dock site
        Site:=TAnchorDockManager(AForm.DockManager).GetChildSite;
        if Site<>nil then begin
          LayoutNode:=LayoutTree.NewNode(LayoutNode);
          Site.SaveLayout(LayoutTree,LayoutNode);
          {if Site.BoundSplitter<>nil then begin
            LayoutNode:=LayoutTree.NewNode(LayoutNode);
            Site.BoundSplitter.SaveLayout(LayoutNode);
          end;}
        end;
      end else
        raise EAnchorDockLayoutError.Create('invalid root control for save: '+DbgSName(AControl));
    end;
    // remove invisible controls
    LayoutTree.Root.Simplify(VisibleControls);
  finally
    VisibleControls.Free;
    SavedSites.Free;
  end;
end;

procedure TAnchorDockMaster.SaveSiteLayoutToTree(AForm: TCustomForm;
  LayoutTree: TAnchorDockLayoutTree);
var
  LayoutNode: TAnchorDockLayoutTreeNode;
  Site: TAnchorDockHostSite;
begin
  if (AForm is TAnchorDockHostSite) then begin
    Site:=TAnchorDockHostSite(AForm);
    Site.SaveLayout(LayoutTree,LayoutTree.Root);
  end else if IsCustomSite(AForm) then begin
    LayoutTree.Root.NodeType:=adltnCustomSite;
    LayoutTree.Root.Assign(AForm);
    // can have one normal dock site
    Site:=TAnchorDockManager(AForm.DockManager).GetChildSite;
    if Site<>nil then begin
      LayoutNode:=LayoutTree.NewNode(LayoutTree.Root);
      Site.SaveLayout(LayoutTree,LayoutNode);
    end;
  end else
    raise EAnchorDockLayoutError.Create('invalid root control for save: '+DbgSName(AForm));
end;

function TAnchorDockMaster.CreateRestoreLayout(AControl: TControl
  ): TAnchorDockRestoreLayout;
{ Create a restore layout for AControl and its child controls.
  It contains the whole parent structure so that the restore knows where to
  put AControl.
}

  procedure AddControlNames(SubControl: TControl;
    RestoreLayout: TAnchorDockRestoreLayout);
  var
    i: Integer;
  begin
    if (FControls.IndexOf(SubControl)>=0)
    and not RestoreLayout.HasControlName(SubControl.Name) then
      RestoreLayout.ControlNames.Add(SubControl.Name);
    if SubControl is TWinControl then
      for i:=0 to TWinControl(SubControl).ControlCount-1 do
        AddControlNames(TWinControl(SubControl).Controls[i],RestoreLayout);
  end;

var
  AForm: TCustomForm;
begin
  if not IsSite(AControl) then
    raise Exception.Create('TAnchorDockMaster.CreateRestoreLayout: not a site '+DbgSName(AControl));
  AForm:=GetParentForm(AControl);
  Result:=TAnchorDockRestoreLayout.Create(TAnchorDockLayoutTree.Create);
  SaveSiteLayoutToTree(AForm,Result.Layout);
  AddControlNames(AControl,Result);
end;

procedure TAnchorDockMaster.SaveLayoutToConfig(Config: TConfigStorage);
var
  Tree: TAnchorDockLayoutTree;
begin
  Tree:=TAnchorDockLayoutTree.Create;
  try
    Config.AppendBasePath('MainConfig/');
    SaveMainLayoutToTree(Tree);
    Tree.SaveToConfig(Config);
    Config.UndoAppendBasePath;
    Config.AppendBasePath('Restores/');
    RestoreLayouts.SaveToConfig(Config);
    Config.UndoAppendBasePath;
    WriteDebugLayout('TAnchorDockMaster.SaveLayoutToConfig ',Tree.Root);
    //DebugWriteChildAnchors(Tree.Root);
  finally
    Tree.Free;
  end;
end;

function TAnchorDockMaster.ConfigIsEmpty(Config: TConfigStorage): boolean;
begin
  Result:=Config.GetValue('MainConfig/Nodes/ChildCount',0)=0;
end;

function TAnchorDockMaster.LoadLayoutFromConfig(Config: TConfigStorage;
  Scale: Boolean): boolean;
var
  Tree: TAnchorDockLayoutTree;
  ControlNames: TStringList;
begin
  Result:=false;
  ControlNames:=TStringList.Create;
  fTreeNameToDocker:=TADNameToControl.Create;
  Tree:=TAnchorDockLayoutTree.Create;
  try
    // load tree
    Config.AppendBasePath('MainConfig/');
    Tree.LoadFromConfig(Config);
    Config.UndoAppendBasePath;
    Config.AppendBasePath('Restores/');
    RestoreLayouts.LoadFromConfig(Config);
    Config.UndoAppendBasePath;

    {$IFDEF VerboseAnchorDockRestore}
    WriteDebugLayout('TAnchorDockMaster.LoadLayoutFromConfig ',Tree.Root);
    DebugWriteChildAnchors(Tree.Root);
    {$ENDIF}

    // close all unneeded forms/controls
    if not CloseUnneededControls(Tree) then exit;

    BeginUpdate;
    try
      // create all needed forms/controls
      if not CreateNeededControls(Tree,true,ControlNames) then exit;

      // simplify layouts
      ControlNames.Sort;
      {$IFDEF VerboseAnchorDockRestore}
      debugln(['TAnchorDockMaster.LoadLayoutFromConfig controls: ']);
      debugln(ControlNames.Text);
      {$ENDIF}
      Tree.Root.Simplify(ControlNames);

      // reuse existing sites to reduce flickering
      MapTreeToControls(Tree);
      {$IFDEF VerboseAnchorDockRestore}
      fTreeNameToDocker.WriteDebugReport('TAnchorDockMaster.LoadLayoutFromConfig Map');
      {$ENDIF}

      // create sites
      RestoreLayout(Tree,Scale);
    finally
      EndUpdate;
    end;
  finally
    // clean up
    FreeAndNil(fTreeNameToDocker);
    ControlNames.Free;
    Tree.Free;
    // commit (this can raise an exception)
    EnableAllAutoSizing;
  end;
  {$IFDEF VerboseAnchorDockRestore}
  DebugWriteChildAnchors(Application.MainForm,true,false);
  {$ENDIF}
  Result:=true;
end;

procedure TAnchorDockMaster.LoadSettingsFromConfig(Config: TConfigStorage);
begin
  Config.AppendBasePath('Settings/');
  DragTreshold:=Config.GetValue('DragThreshold',4);
  DockOutsideMargin:=Config.GetValue('DockOutsideMargin',10);
  DockParentMargin:=Config.GetValue('DockParentMargin',10);
  PageAreaInPercent:=Config.GetValue('PageAreaInPercent',40);
  HeaderAlignTop:=Config.GetValue('HeaderAlignTop',80);
  HeaderAlignLeft:=Config.GetValue('HeaderAlignLeft',120);
  SplitterWidth:=Config.GetValue('SplitterWidth',4);
  ScaleOnResize:=Config.GetValue('ScaleOnResize',true);
  ShowHeaderCaption:=Config.GetValue('ShowHeaderCaption',true);
  HideHeaderCaptionFloatingControl:=Config.GetValue('HideHeaderCaptionFloatingControl',true);
  AllowDragging:=Config.GetValue('AllowDragging',true);
  HeaderButtonSize:=Config.GetValue('HeaderButtonSize',10);

  //property HeaderHint: string read FHeaderHint write FHeaderHint;

  Config.UndoAppendBasePath;
end;

procedure TAnchorDockMaster.SaveSettingsToConfig(Config: TConfigStorage);
begin
  Config.AppendBasePath('Settings/');
  Config.SetDeleteValue('DragThreshold',DragTreshold,4);
  Config.SetDeleteValue('DockOutsideMargin',DockOutsideMargin,10);
  Config.SetDeleteValue('DockParentMargin',DockParentMargin,10);
  Config.SetDeleteValue('PageAreaInPercent',PageAreaInPercent,40);
  Config.SetDeleteValue('HeaderAlignTop',HeaderAlignTop,80);
  Config.SetDeleteValue('HeaderAlignLeft',HeaderAlignLeft,120);
  Config.SetDeleteValue('SplitterWidth',SplitterWidth,4);
  Config.SetDeleteValue('ScaleOnResize',ScaleOnResize,true);
  Config.SetDeleteValue('ShowHeaderCaption',ShowHeaderCaption,true);
  Config.SetDeleteValue('HideHeaderCaptionFloatingControl',HideHeaderCaptionFloatingControl,true);
  Config.SetDeleteValue('AllowDragging',AllowDragging,true);
  Config.SetDeleteValue('HeaderButtonSize',HeaderButtonSize,10);

  //property HeaderHint: string read FHeaderHint write FHeaderHint;

  Config.UndoAppendBasePath;
end;

procedure TAnchorDockMaster.ManualFloat(AControl: TControl);
var
  Site: TAnchorDockHostSite;
begin
  Site:=GetAnchorSite(AControl);
  if Site=nil then exit;
  Site.Undock;
end;

procedure TAnchorDockMaster.ManualDock(SrcSite: TAnchorDockHostSite;
  TargetSite: TCustomForm; Align: TAlign; TargetControl: TControl);
var
  Site: TAnchorDockHostSite;
  aManager: TAnchorDockManager;
  DockObject: TDragDockObject;
begin
  debugln(['TAnchorDockMaster.ManualDock SrcSite=',DbgSName(SrcSite),' TargetSite=',DbgSName(TargetSite),' Align=',dbgs(Align),' TargetControl=',DbgSName(TargetControl)]);
  if SrcSite=TargetSite then exit;
  if SrcSite.IsParentOf(TargetSite) then
    raise Exception.Create('TAnchorDockMaster.ManualDock SrcSite.IsParentOf(TargetSite)');
  if TargetSite.IsParentOf(SrcSite) then
    raise Exception.Create('TAnchorDockMaster.ManualDock TargetSite.IsParentOf(SrcSite)');

  if IsCustomSite(TargetSite) then begin
    aManager:=TAnchorDockManager(TargetSite.DockManager);
    Site:=aManager.GetChildSite;
    if Site=nil then begin
      // dock as first site into custom dock site
      debugln(['TAnchorDockMaster.ManualDock dock as first site into custom dock site: SrcSite=',DbgSName(SrcSite),' TargetSite=',DbgSName(TargetSite),' Align=',dbgs(Align)]);
      BeginUpdate;
      try
        DockObject := TDragDockObject.Create(SrcSite);
        try
          DockObject.DropAlign:=Align;
          DockObject.DockRect:=SrcSite.BoundsRect;
          aManager.InsertControl(DockObject);
        finally
          DockObject.Free;
        end;
      finally
        EndUpdate;
      end;
      exit;
    end;
    // else: dock into child site of custom dock site
  end else begin
    // dock to or into TargetSite
    if not (TargetSite is TAnchorDockHostSite) then
      raise Exception.Create('TAnchorDockMaster.ManualDock invalid TargetSite');
    Site:=TAnchorDockHostSite(TargetSite);
  end;
  if AutoFreedIfControlIsRemoved(Site,SrcSite) then
    raise Exception.Create('TAnchorDockMaster.ManualDock TargetSite depends on SrcSite');
  BeginUpdate;
  try
    Site.ExecuteDock(SrcSite,TargetControl,Align);
  finally
    EndUpdate;
  end;
end;

function TAnchorDockMaster.ManualEnlarge(Site: TAnchorDockHostSite;
  Side: TAnchorKind; OnlyCheckIfPossible: boolean): boolean;
begin
  Result:=(Site<>nil) and Site.EnlargeSide(Side,OnlyCheckIfPossible);
end;

procedure TAnchorDockMaster.BeginUpdate;
begin
  inc(fUpdateCount);
end;

procedure TAnchorDockMaster.EndUpdate;
begin
  if fUpdateCount<=0 then
    RaiseGDBException('');
  dec(fUpdateCount);
  if fUpdateCount=0 then
    SimplifyPendingLayouts;
end;

procedure TAnchorDockMaster.NeedSimplify(AControl: TControl);
begin
  if Self=nil then exit;
  if csDestroying in ComponentState then exit;
  if csDestroying in AControl.ComponentState then exit;
  if fNeedSimplify=nil then exit;
  if fNeedSimplify.IndexOf(AControl)>=0 then exit;
  if not ((AControl is TAnchorDockHostSite)
          or (AControl is TAnchorDockPage))
  then
    exit;
  if Application.Terminated then exit;
  //debugln(['TAnchorDockMaster.NeedSimplify ',DbgSName(AControl),' Caption="',AControl.Caption,'"']);
  fNeedSimplify.Add(AControl);
  AControl.FreeNotification(Self);
  QueueSimplify:=true;
end;

procedure TAnchorDockMaster.NeedFree(AControl: TControl);
begin
  //debugln(['TAnchorDockMaster.NeedFree ',DbgSName(AControl),' ',csDestroying in AControl.ComponentState]);
  if fNeedFree.IndexOf(AControl)>=0 then exit;
  if csDestroying in AControl.ComponentState then exit;
  fNeedFree.Add(AControl);
  AControl.DisableAutoSizing;
  AControl.Parent:=nil;
  AControl.Visible:=false;
end;

procedure TAnchorDockMaster.SimplifyPendingLayouts;
var
  AControl: TControl;
  Changed: Boolean;
  i: Integer;
begin
  if fSimplifying or (fUpdateCount>0) then exit;
  fSimplifying:=true;
  try
    // simplify layout (do not free controls in this step, only mark them)
    repeat
      Changed:=false;
      i:=fNeedSimplify.Count-1;
      while i>=0 do begin
        AControl:=TControl(fNeedSimplify[i]);
        if (csDestroying in AControl.ComponentState)
        or (fNeedFree.IndexOf(AControl)>=0) then begin
          fNeedSimplify.Delete(i);
          Changed:=true;
        end else if (AControl is TAnchorDockHostSite) then begin
          //debugln(['TAnchorDockMaster.SimplifyPendingLayouts ',DbgSName(AControl),' ',dbgs(TAnchorDockHostSite(AControl).SiteType),' UpdatingLayout=',TAnchorDockHostSite(AControl).UpdatingLayout]);
          if not TAnchorDockHostSite(AControl).UpdatingLayout then begin
            fNeedSimplify.Delete(i);
            Changed:=true;
            if TAnchorDockHostSite(AControl).SiteType=adhstNone then
            begin
              //debugln(['TAnchorDockMaster.SimplifyPendingLayouts free empty site: ',dbgs(pointer(AControl)),' Caption="',AControl.Caption,'"']);
              NeedFree(AControl);
            end else begin
              TAnchorDockHostSite(AControl).Simplify;
            end;
          end;
        end else if AControl is TAnchorDockPage then begin
          fNeedSimplify.Delete(i);
          Changed:=true;
          NeedFree(AControl);
        end else
          RaiseGDBException('TAnchorDockMaster.SimplifyPendingLayouts inconsistency');
        i:=Min(fNeedSimplify.Count,i)-1;
      end;
    until not Changed;

    // free unneeded controls
    while fNeedFree.Count>0 do
      if csDestroying in TControl(fNeedFree[0]).ComponentState then
        fNeedFree.Delete(0)
      else
        TControl(fNeedFree[0]).Free;

  finally
    fSimplifying:=false;
  end;
end;

function TAnchorDockMaster.AutoFreedIfControlIsRemoved(AControl,
  RemovedControl: TControl): boolean;
{ returns true if the simplification algorithm will automatically free
     AControl when RemovedControl is removed
  Some sites are dummy sites that were autocreated. They will be auto freed
  if not needed anymore.
  1. A TAnchorDockPage has a TAnchorDockHostSite as child. If the child is freed
     the page will be freed.
  2. When a TAnchorDockPageControl has only one page left the content is moved
     up and the pagecontrol and page will be freed.
  3. When a layout site has only one child site left, the content is moved up
     and the child site will be freed.
  4. When the control of a OneControl site is freed the site will be freed.
}
var
  ParentSite: TAnchorDockHostSite;
  Page: TAnchorDockPage;
  PageControl: TAnchorDockPageControl;
  OtherPage: TAnchorDockPage;
  Site, Site1, Site2: TAnchorDockHostSite;
begin
  Result:=false;
  if (RemovedControl=nil) or (AControl=nil) then exit;
  while RemovedControl<>nil do begin
    if RemovedControl=AControl then exit(true);
    if RemovedControl is TAnchorDockPage then begin
      // a page will be removed
      Page:=TAnchorDockPage(RemovedControl);
      if not (Page.Parent is TAnchorDockPageControl) then exit;
      PageControl:=TAnchorDockPageControl(Page.Parent);
      if PageControl.PageCount>2 then exit;
      if PageControl.PageCount=2 then begin
        // this pagecontrol will be replaced by the content of the other page
        if PageControl=AControl then exit(true);
        if PageControl.Page[0]=Page then
          OtherPage:=PageControl.DockPages[1]
        else
          OtherPage:=PageControl.DockPages[0];
        // the other page will be removed (its content will be moved up)
        if OtherPage=AControl then exit(true);
        if (OtherPage.ControlCount>0) then begin
          if (OtherPage.Controls[0] is TAnchorDockHostSite)
          and (OtherPage.Controls[0]=RemovedControl) then
            exit(true); // the site of the other page will be removed (its content moved up)
        end;
        exit;
      end;
      // the last page of the pagecontrol is freed => the pagecontrol will be removed too
    end else if RemovedControl is TAnchorDockPageControl then begin
      // the pagecontrol will be removed
      if not (RemovedControl.Parent is TAnchorDockHostSite) then exit;
      // a pagecontrol is always the only child of a site
      // => the site will be removed too
    end else if RemovedControl is TAnchorDockHostSite then begin
      // a site will be removed
      Site:=TAnchorDockHostSite(RemovedControl);
      if Site.Parent is TAnchorDockPage then begin
        // a page has only one site
        // => the page will be removed too
      end else if Site.Parent is TAnchorDockHostSite then begin
        ParentSite:=TAnchorDockHostSite(Site.Parent);
        if (ParentSite.SiteType=adhstOneControl)
        or ParentSite.IsOneSiteLayout(Site) then begin
          // the control of a OneControl site is removed => the ParentSite is freed too
        end else if ParentSite.SiteType=adhstLayout then begin
          if ParentSite.IsTwoSiteLayout(Site1,Site2) then begin
            // when there are two sites and one of them is removed
            // the content of the other will be moved up and then both sites are
            // removed
            if (Site1=AControl) or (Site2=AControl) then
              exit(true);
          end;
          exit; // removing only site will not free the layout
        end else begin
          raise Exception.Create('TAnchorDockMaster.AutoFreedIfControlIsRemoved ParentSiteType='+dbgs(ParentSite.SiteType)+' ChildSiteType='+dbgs(Site.SiteType));
        end;
      end else
        exit; // other classes will never be auto freed
    end else begin
      // control is not a site => check if control is in a OneControl site
      if not (RemovedControl.Parent is TAnchorDockHostSite) then exit;
      ParentSite:=TAnchorDockHostSite(RemovedControl.Parent);
      if (ParentSite.SiteType<>adhstOneControl) then exit;
      if ParentSite.GetOneControl<>RemovedControl then exit;
      // the control of a OneControl site is removed => the site is freed too
    end;
    RemovedControl:=RemovedControl.Parent;
  end;
end;

function TAnchorDockMaster.CreateSite(NamePrefix: string;
  DisableAutoSizing: boolean): TAnchorDockHostSite;
var
  i: Integer;
  NewName: String;
begin
  Result:=TAnchorDockHostSite(SiteClass.NewInstance);
  Result.DisableAutoSizing;
  Result.CreateNew(Self,1);
  i:=0;
  repeat
    inc(i);
    NewName:=NamePrefix+AnchorDockSiteName+IntToStr(i);
  until (Screen.FindForm(NewName)=nil) and (FindComponent(NewName)=nil);
  Result.Name:=NewName;
  if not DisableAutoSizing then
    Result.EnableAutoSizing;
end;

function TAnchorDockMaster.CreateSplitter(NamePrefix: string
  ): TAnchorDockSplitter;
var
  i: Integer;
  NewName: String;
begin
  Result:=SplitterClass.Create(Self);
  i:=0;
  repeat
    inc(i);
    NewName:=NamePrefix+AnchorDockSplitterName+IntToStr(i);
  until FindComponent(NewName)=nil;
  Result.Name:=NewName;
end;

{ TAnchorDockHostSite }

procedure TAnchorDockHostSite.SetHeaderSide(const AValue: TAnchorKind);
begin
  if FHeaderSide=AValue then exit;
  FHeaderSide:=AValue;
end;

procedure TAnchorDockHostSite.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if AComponent=Pages then FPages:=nil;
    if AComponent=Header then FHeader:=nil;
    if AComponent=BoundSplitter then FBoundSplitter:=nil;
  end;
end;

function TAnchorDockHostSite.DoDockClientMsg(DragDockObject: TDragDockObject;
  aPosition: TPoint): boolean;
begin
  if aPosition.X=0 then ;
  Result:=ExecuteDock(DragDockObject.Control,DragDockObject.DropOnControl,
                      DragDockObject.DropAlign);
end;

function TAnchorDockHostSite.ExecuteDock(NewControl, DropOnControl: TControl;
  DockAlign: TAlign): boolean;
begin
  if UpdatingLayout then exit;
  //debugln(['TAnchorDockHostSite.ExecuteDock Self="',Caption,'"  Control=',DbgSName(NewControl),' DropOnControl=',DbgSName(DropOnControl),' Align=',dbgs(DockAlign)]);

  DisableAutoSizing;
  try
    BeginUpdateLayout;
    try
      DockMaster.SimplifyPendingLayouts;
      NewControl.DisableAutoSizing;

      if (NewControl.Parent=Self) and (SiteType=adhstLayout) then begin
        // change of layout, one child is docked to the outer side
        RemoveControlFromLayout(NewControl);
      end else if (NewControl.Parent=Parent) and (Parent is TAnchorDockHostSite)
      and (TAnchorDockHostSite(Parent).SiteType=adhstLayout) then begin
        // change of layout, one sibling is moved
        TAnchorDockHostSite(Parent).RemoveControlFromLayout(NewControl);
      end;

      if SiteType=adhstNone then begin
        // make a control dockable by docking it into a TAnchorDockHostSite;
        Result:=DockFirstControl(NewControl);
      end else if DockAlign=alClient then begin
        // page docking
        if SiteType=adhstOneControl then begin
          if Parent is TAnchorDockPage then begin
            // add as sibling page
            Result:=(Parent.Parent.Parent as TAnchorDockHostSite).DockAnotherPage(NewControl,nil);
          end else
            // create pages
            Result:=DockSecondPage(NewControl);
        end else if SiteType=adhstPages then
          // add as sibling page
          Result:=DockAnotherPage(NewControl,DropOnControl);
      end else if DockAlign in [alLeft,alTop,alRight,alBottom] then
      begin
        // anchor docking
        if SiteType=adhstOneControl then begin
          if Parent is TAnchorDockHostSite then begin
            // add site as sibling
            Result:=TAnchorDockHostSite(Parent).DockAnotherControl(Self,NewControl,
                      DockAlign,DropOnControl<>nil);
          end else
            // create layout
            Result:=DockSecondControl(NewControl,DockAlign,DropOnControl<>nil);
        end else if SiteType=adhstLayout then
          // add site as sibling
          Result:=DockAnotherControl(nil,NewControl,DockAlign,DropOnControl<>nil);
      end;

      NewControl.EnableAutoSizing;
    finally
      EndUpdateLayout;
    end;
  finally
    EnableAutoSizing;
  end;
end;

function TAnchorDockHostSite.DockFirstControl(NewControl: TControl): boolean;
var
  DestRect: TRect;
begin
  if SiteType<>adhstNone then
    RaiseGDBException('TAnchorDockHostSite.DockFirstControl inconsistency');
  // create adhstOneControl
  DestRect := ClientRect;
  NewControl.Dock(Self, DestRect);
  FSiteType:=adhstOneControl;
  if NewControl is TCustomForm then begin
    Icon.Assign(TCustomForm(NewControl).Icon);
  end;
  Result:=true;
end;

function TAnchorDockHostSite.DockSecondControl(NewControl: TControl;
  DockAlign: TAlign; Inside: boolean): boolean;
{ Convert a adhstOneControl into a adhstLayout by docking NewControl
  at a side (DockAlign).
  If Inside=true this DockSite is not expanded and both controls share the old space.
  If Inside=false this DockSite is expanded.
}
var
  OldSite: TAnchorDockHostSite;
  OldControl: TControl;
begin
  Result:=true;
  debugln(['TAnchorDockHostSite.DockSecondControl Self="',Caption,'" AControl=',DbgSName(NewControl),' Align=',dbgs(DockAlign),' Inside=',Inside]);
  if SiteType<>adhstOneControl then
    RaiseGDBException('TAnchorDockHostSite.DockSecondControl inconsistency: not adhstOneControl');
  if not (DockAlign in [alLeft,alTop,alRight,alBottom]) then
    RaiseGDBException('TAnchorDockHostSite.DockSecondControl inconsistency: DockAlign='+dbgs(DockAlign));

  FSiteType:=adhstLayout;

  // remove header (keep it for later use)
  Header.Parent:=nil;

  // put the OldControl into a site of its own (OldSite) and dock OldSite
  OldControl:=GetOneControl;
  OldSite:=MakeSite(OldControl);
  AddCleanControl(OldSite);
  OldSite.AnchorClient(0);
  // the LCL will compute the bounds later after EnableAutoSizing
  // but the bounds are needed now => set them manually
  OldSite.BoundsRect:=Rect(0,0,ClientWidth,ClientHeight);

  Result:=DockAnotherControl(OldSite,NewControl,DockAlign,Inside);

  debugln(['TAnchorDockHostSite.DockSecondControl END Self="',Caption,'" AControl=',DbgSName(NewControl),' Align=',dbgs(DockAlign),' Inside=',Inside]);
end;

function TAnchorDockHostSite.DockAnotherControl(Sibling, NewControl: TControl;
  DockAlign: TAlign; Inside: boolean): boolean;
var
  Splitter: TAnchorDockSplitter;
  a: TAnchorKind;
  NewSite: TAnchorDockHostSite;
  NewBounds: TRect;
  MainAnchor: TAnchorKind;
  i: Integer;
  NewSiblingWidth: Integer;
  NewSiblingHeight: Integer;
  NewSize: LongInt;
  BoundsIncreased: Boolean;
  NewParentBounds: TRect;
begin
  Result:=false;
  if SiteType<>adhstLayout then
    RaiseGDBException('TAnchorDockHostSite.DockAnotherControl inconsistency');
  if not (DockAlign in [alLeft,alTop,alRight,alBottom]) then
    RaiseGDBException('TAnchorDockHostSite.DockAnotherControl inconsistency');

  // add a splitter
  Splitter:=DockMaster.CreateSplitter;
  if DockAlign in [alLeft,alRight] then begin
    Splitter.ResizeAnchor:=akLeft;
    Splitter.Width:=DockMaster.SplitterWidth;
  end else begin
    Splitter.ResizeAnchor:=akTop;
    Splitter.Height:=DockMaster.SplitterWidth;
  end;
  Splitter.Parent:=Self;

  // dock the NewControl
  NewSite:=MakeSite(NewControl);
  AddCleanControl(NewSite);

  BoundsIncreased:=false;
  if (not Inside) then begin
    if (Parent=nil) then begin
      // expand Self
      NewBounds:=BoundsRect;
      case DockAlign of
      alLeft:
        begin
          dec(NewBounds.Left,NewSite.Width+Splitter.Width);
          MoveAllControls(NewSite.Width+Splitter.Width,0);
        end;
      alRight:
        inc(NewBounds.Right,NewSite.Width+Splitter.Width);
      alTop:
        begin
          dec(NewBounds.Top,NewSite.Height+Splitter.Height);
          MoveAllControls(0,NewSite.Height+Splitter.Height);
        end;
      alBottom:
        inc(NewBounds.Bottom,NewSite.Height+Splitter.Height);
      end;
      BoundsRect:=NewBounds;
      BoundsIncreased:=true;
    end else if DockMaster.IsCustomSite(Parent) then begin
      // Parent is a custom docksite
      // => expand Self and Parent
      // expand Parent (the custom docksite)
      NewParentBounds:=Parent.BoundsRect;
      NewBounds:=BoundsRect;
      case DockAlign of
      alLeft:
        begin
          i:=NewSite.Width+Splitter.Width;
          dec(NewParentBounds.Left,i);
          dec(NewBounds.Left,i);
          MoveAllControls(i,0);
        end;
      alRight:
        begin
          i:=NewSite.Width+Splitter.Width;
          inc(NewBounds.Right,i);
          inc(NewParentBounds.Right,i);
        end;
      alTop:
        begin
          i:=NewSite.Height+Splitter.Height;
          dec(NewBounds.Top,i);
          dec(NewParentBounds.Top,i);
          MoveAllControls(0,i);
        end;
      alBottom:
        begin
          i:=NewSite.Height+Splitter.Height;
          inc(NewParentBounds.Bottom,i);
          inc(NewBounds.Bottom,i);
        end;
      end;
      Parent.BoundsRect:=NewParentBounds;
      BoundsRect:=NewBounds;
      BoundsIncreased:=true;
      TAnchorDockManager(Parent.DockManager).FSiteClientRect:=Parent.ClientRect;
    end;
    debugln(['TAnchorDockHostSite.DockAnotherControl AFTER ENLARGE ',Caption]);
    //DebugWriteChildAnchors(Self,true,true);
  end;

  // anchors
  MainAnchor:=MainAlignAnchor[DockAlign];
  if Inside and (Sibling<>nil) then begin
    { Example: insert right of Sibling
                    #                                  #
        ################          ########################
            -------+#                -------+#+-------+#
            Sibling|#     ----->     Sibling|#|NewSite|#
            -------+#                -------+#+-------+#
        ################          ########################
                    #                                  #
     }
    for a:=low(TAnchorKind) to high(TAnchorKind) do begin
      if a in AnchorAlign[DockAlign] then begin
        NewSite.AnchorSide[a].Assign(Sibling.AnchorSide[a]);
      end else begin
        NewSite.AnchorToNeighbour(a,0,Splitter);
      end;
    end;
    Sibling.AnchorToNeighbour(MainAnchor,0,Splitter);

    if DockAlign in [alLeft,alRight] then begin
      Splitter.AnchorSide[akTop].Assign(Sibling.AnchorSide[akTop]);
      Splitter.AnchorSide[akBottom].Assign(Sibling.AnchorSide[akBottom]);
      // resize and move
      // the NewSite gets at maximum half the space
      // Many bounds are later set by the LCL anchoring. When docking several
      // controls at once the bounds are needed earlier.
      NewSize:=Max(1,Min(NewSite.Width,Sibling.Width div 2));
      NewBounds:=Rect(0,0,NewSize,Sibling.Height);
      NewSiblingWidth:=Max(1,Sibling.Width-NewSize-Splitter.Width);
      if DockAlign=alLeft then begin
        // alLeft: NewControl, Splitter, Sibling
        Splitter.SetBounds(Sibling.Left+NewSize,Sibling.Top,
                           Splitter.Width,Sibling.Height);
        OffsetRect(NewBounds,Sibling.Left,Sibling.Top);
        Sibling.SetBounds(Splitter.Left+Splitter.Width,Sibling.Top,
                          NewSiblingWidth,Sibling.Height);
      end else begin
        // alRight: Sibling, Splitter, NewControl
        Sibling.Width:=NewSiblingWidth;
        Splitter.SetBounds(Sibling.Left+Sibling.Width,Sibling.Top,
                           Splitter.Width,Sibling.Height);
        OffsetRect(NewBounds,Splitter.Left+Splitter.Width,Sibling.Top);
      end;
      NewSite.BoundsRect:=NewBounds;
    end else begin
      Splitter.AnchorSide[akLeft].Assign(Sibling.AnchorSide[akLeft]);
      Splitter.AnchorSide[akRight].Assign(Sibling.AnchorSide[akRight]);
      // resize and move
      // the NewSite gets at maximum half the space
      // Many bounds are later set by the LCL anchoring. When docking several
      // controls at once the bounds are needed earlier.
      NewSize:=Max(1,Min(NewSite.Height,Sibling.Height div 2));
      NewSiblingHeight:=Max(1,Sibling.Height-NewSize-Splitter.Height);
      if DockAlign=alTop then begin
        // alTop: NewControl, Splitter, Sibling
        Splitter.SetBounds(Sibling.Left,Sibling.Top+NewSize,
                           Sibling.Width,Splitter.Height);
        NewSite.SetBounds(Sibling.Left,Sibling.Top,Sibling.Width,NewSize);
        Sibling.SetBounds(Sibling.Left,Splitter.Top+Splitter.Height,
                          Sibling.Width,NewSiblingHeight);
      end else begin
        // alBottom: Sibling, Splitter, NewControl
        Sibling.Height:=NewSiblingHeight;
        Splitter.SetBounds(Sibling.Left,Sibling.Top+Sibling.Height,
                           Sibling.Width,Splitter.Height);
        NewSite.SetBounds(Sibling.Left,Splitter.Top+Splitter.Height,
                          Sibling.Width,NewSize);
      end;
    end;
  end else begin
    { Example: insert right of all siblings
        ##########         #######################
        --------+#         --------+#+----------+#
        SiblingA|#         SiblingA|#|          |#
        --------+#         --------+#|          |#
        ##########  -----> ##########|NewControl|#
        --------+#         --------+#|          |#
        SiblingB|#         SiblingB|#|          |#
        --------+#         --------+#+----------+#
        ##########         #######################
    }
    if DockAlign in [alLeft,alRight] then
      NewSize:=NewSite.Width
    else
      NewSize:=NewSite.Height;
    for i:=0 to ControlCount-1 do begin
      Sibling:=Controls[i];
      if Sibling.AnchorSide[MainAnchor].Control=Self then begin
        // this Sibling is anchored to the docked site
        // anchor it to the splitter
        Sibling.AnchorToNeighbour(MainAnchor,0,Splitter);
        if not BoundsIncreased then begin
          // the NewSite gets at most half the space
          if DockAlign in [alLeft,alRight] then
            NewSize:=Min(NewSize,Sibling.Width div 2)
          else
            NewSize:=Min(NewSize,Sibling.Height div 2);
        end;
      end;
    end;
    NewSize:=Max(1,NewSize);

    // anchor Splitter and NewSite
    a:=ClockwiseAnchor[MainAnchor];
    Splitter.AnchorParallel(a,0,Self);
    Splitter.AnchorParallel(OppositeAnchor[a],0,Self);
    NewSite.AnchorParallel(a,0,Self);
    NewSite.AnchorParallel(OppositeAnchor[a],0,Self);
    NewSite.AnchorParallel(MainAnchor,0,Self);
    NewSite.AnchorToNeighbour(OppositeAnchor[MainAnchor],0,Splitter);

    // Many bounds are later set by the LCL anchoring. When docking several
    // controls at once the bounds are needed earlier.
    if DockAlign in [alLeft,alRight] then begin
      if DockAlign=alLeft then begin
        // alLeft: NewSite, Splitter, other siblings
        Splitter.SetBounds(NewSize,0,Splitter.Width,ClientHeight);
        NewSite.SetBounds(0,0,NewSize,ClientHeight);
      end else begin
        // alRight: other siblings, Splitter, NewSite
        NewSite.SetBounds(ClientWidth-NewSize,0,NewSize,ClientHeight);
        Splitter.SetBounds(NewSite.Left-Splitter.Width,0,Splitter.Width,ClientHeight);
      end;
    end else begin
      if DockAlign=alTop then begin
        // alTop: NewSite, Splitter, other siblings
        Splitter.SetBounds(0,NewSize,ClientWidth,Splitter.Height);
        NewSite.SetBounds(0,0,ClientWidth,NewSize);
      end else begin
        // alBottom: other siblings, Splitter, NewSite
        NewSite.SetBounds(0,ClientHeight-NewSize,ClientWidth,NewSize);
        Splitter.SetBounds(0,NewSite.Top-Splitter.Height,ClientWidth,Splitter.Height);
      end;
    end;
    // shrink siblings
    for i:=0 to ControlCount-1 do begin
      Sibling:=Controls[i];
      if Sibling.AnchorSide[MainAnchor].Control=Splitter then begin
        NewBounds:=Sibling.BoundsRect;
        case DockAlign of
        alLeft: NewBounds.Left:=Splitter.Left+Splitter.Width;
        alRight: NewBounds.Right:=Splitter.Left;
        alTop: NewBounds.Top:=Splitter.Top+Splitter.Height;
        alBottom: NewBounds.Bottom:=Splitter.Top;
        end;
        NewBounds.Right:=Max(NewBounds.Left+1,NewBounds.Right);
        NewBounds.Bottom:=Max(NewBounds.Top+1,NewBounds.Bottom);
        Sibling.BoundsRect:=NewBounds;
      end;
    end;
  end;

  //debugln(['TAnchorDockHostSite.DockAnotherControl ',DbgSName(Self)]);
  //DebugWriteChildAnchors(Self,true,true);
  Result:=true;
end;

procedure TAnchorDockHostSite.CreatePages;
begin
  FPages:=DockMaster.PageControlClass.Create(nil); // do not own it, pages can be moved to another site
  FPages.FreeNotification(Self);
  FPages.Parent:=Self;
  FPages.Align:=alClient;
end;

function TAnchorDockHostSite.DockSecondPage(NewControl: TControl): boolean;
var
  OldControl: TControl;
  OldSite: TAnchorDockHostSite;
begin
  debugln(['TAnchorDockHostSite.DockSecondPage Self="',Caption,'" AControl=',DbgSName(NewControl)]);
  if SiteType<>adhstOneControl then
    RaiseGDBException('TAnchorDockHostSite.DockSecondPage inconsistency');

  FSiteType:=adhstPages;
  CreatePages;

  // remove header (keep it for later use)
  debugln(['TAnchorDockHostSite.DockSecondPage Self="',Caption,'" removing header ...']);
  Header.Parent:=nil;

  // put the OldControl into a page of its own
  debugln(['TAnchorDockHostSite.DockSecondPage Self="',Caption,'" move oldcontrol to site of its own ...']);
  OldControl:=GetOneControl;
  OldSite:=MakeSite(OldControl);
  OldSite.HostDockSite:=nil;
  debugln(['TAnchorDockHostSite.DockSecondPage Self="',Caption,'" adding oldcontrol site ...']);
  FPages.Pages.Add(OldSite.Caption);
  OldSite.Parent:=FPages.Page[0];
  OldSite.Align:=alClient;
  OldSite.Visible:=true;

  Result:=DockAnotherPage(NewControl,nil);
end;

function TAnchorDockHostSite.DockAnotherPage(NewControl: TControl;
  InFrontOf: TControl): boolean;
var
  NewSite: TAnchorDockHostSite;
  NewIndex: LongInt;
begin
  debugln(['TAnchorDockHostSite.DockAnotherPage Self="',Caption,'" make new control (',DbgSName(NewControl),') dockable ...']);
  if SiteType<>adhstPages then
    RaiseGDBException('TAnchorDockHostSite.DockAnotherPage inconsistency');

  NewSite:=MakeSite(NewControl);
  //debugln(['TAnchorDockHostSite.DockAnotherPage Self="',Caption,'" adding newcontrol site ...']);
  NewIndex:=FPages.PageCount;
  if (InFrontOf is TAnchorDockPage)
  and (InFrontOf.Parent=Pages) then
    NewIndex:=TAnchorDockPage(InFrontOf).PageIndex;
  Pages.Pages.Insert(NewIndex,NewSite.Caption);
  //debugln(['TAnchorDockHostSite.DockAnotherPage ',DbgSName(FPages.Page[1])]);
  NewSite.Parent:=FPages.Page[NewIndex];
  NewSite.Align:=alClient;
  NewSite.Visible:=true;
  FPages.PageIndex:=NewIndex;

  Result:=true;
end;

procedure TAnchorDockHostSite.AddCleanControl(AControl: TControl;
  TheAlign: TAlign);
var
  a: TAnchorKind;
begin
  AControl.Parent:=Self;
  AControl.Align:=TheAlign;
  AControl.Anchors:=[akLeft,akTop,akRight,akBottom];
  for a:=Low(TAnchorKind) to high(TAnchorKind) do
    AControl.AnchorSide[a].Control:=nil;
  AControl.Visible:=true;
end;

procedure TAnchorDockHostSite.RemoveControlFromLayout(AControl: TControl);

  procedure RemoveControlBoundSplitter(Splitter: TAnchorDockSplitter;
    Side: TAnchorKind);
  var
    i: Integer;
    Sibling: TControl;
    NewBounds: TRect;
  begin
    //debugln(['RemoveControlBoundSplitter START ',DbgSName(Splitter)]);
    { Example: Side=akRight
                          #             #
        #####################     #########
           ---+S+--------+#         ---+#
           ---+S|AControl|#   --->  ---+#
           ---+S+--------+#         ---+#
        #####################     #########
    }
    for i:=Splitter.AnchoredControlCount-1 downto 0 do begin
      Sibling:=Splitter.AnchoredControls[i];
      if Sibling.AnchorSide[Side].Control=Splitter then begin
        // anchor Sibling to next
        Sibling.AnchorSide[Side].Assign(AControl.AnchorSide[Side]);
        // enlarge Sibling
        NewBounds:=Sibling.BoundsRect;
        case Side of
        akTop: NewBounds.Top:=AControl.Top;
        akLeft: NewBounds.Left:=AControl.Left;
        akRight: NewBounds.Right:=AControl.Left+AControl.Width;
        akBottom: NewBounds.Bottom:=AControl.Top+AControl.Height;
        end;
        Sibling.BoundsRect:=NewBounds;
      end;
    end;
    //debugln(['RemoveControlBoundSplitter ',DbgSName(Splitter)]);
    Splitter.Free;

    ClearChildControlAnchorSides(AControl);
    //DebugWriteChildAnchors(GetParentForm(Self),true,true);
  end;

  procedure ConvertToOneControlType(OnlySiteLeft: TAnchorDockHostSite);
  var
    a: TAnchorKind;
    NewBounds: TRect;
    p: TPoint;
    i: Integer;
    Sibling: TControl;
    NewParentBounds: TRect;
  begin
    BeginUpdateLayout;
    try
      // remove splitter
      for i:=ControlCount-1 downto 0 do begin
        Sibling:=Controls[i];
        if Sibling is TAnchorDockSplitter then
          Sibling.Free
        else if Sibling is TAnchorDockHostSite then
          for a:=low(TAnchorKind) to high(TAnchorKind) do
            Sibling.AnchorSide[a].Control:=nil;
      end;
      if (Parent=nil) then begin
        // shrink this site
        NewBounds:=OnlySiteLeft.BoundsRect;
        p:=ClientOrigin;
        OffsetRect(NewBounds,p.x,p.y);
        BoundsRect:=NewBounds;
      end else if DockMaster.IsCustomSite(Parent) then begin
        // parent is a custom dock site
        // shrink this site and the parent
        NewParentBounds:=Parent.BoundsRect;
        case Align of
        alTop:
          begin
            inc(NewParentBounds.Top,Height-OnlySiteLeft.Height);
            Width:=Parent.ClientWidth;
            Height:=OnlySiteLeft.Height;
          end;
        alBottom:
          begin
            dec(NewParentBounds.Bottom,Height-OnlySiteLeft.Height);
            Width:=Parent.ClientWidth;
            Height:=OnlySiteLeft.Height;
          end;
        alLeft:
          begin
            inc(NewParentBounds.Left,Width-OnlySiteLeft.Width);
            Width:=OnlySiteLeft.Width;
            Height:=Parent.ClientHeight;
          end;
        alRight:
          begin
            dec(NewParentBounds.Right,Width-OnlySiteLeft.Width);
            Width:=OnlySiteLeft.Width;
            Height:=Parent.ClientHeight;
          end;
        end;
        Parent.BoundsRect:=NewParentBounds;
      end;

      // change type
      FSiteType:=adhstOneControl;
      OnlySiteLeft.Align:=alClient;
      Header.Parent:=Self;
      UpdateHeaderAlign;

      //debugln(['TAnchorDockHostSite.RemoveControlFromLayout.ConvertToOneControlType AFTER CONVERT "',Caption,'" to onecontrol OnlySiteLeft="',OnlySiteLeft.Caption,'"']);
      //DebugWriteChildAnchors(GetParentForm(Self),true,true);

      DockMaster.NeedSimplify(Self);
    finally
      EndUpdateLayout;
    end;
  end;

var
  Side: TAnchorKind;
  Splitter: TAnchorDockSplitter;
  OnlySiteLeft: TAnchorDockHostSite;
  Sibling: TControl;
  SplitterCount: Integer;
begin
  debugln(['TAnchorDockHostSite.RemoveControlFromLayout Self="',Caption,'" AControl=',DbgSName(AControl),'="',AControl.Caption,'"']);
  if SiteType<>adhstLayout then
    RaiseGDBException('TAnchorDockHostSite.RemoveControlFromLayout inconsistency');

  if IsOneSiteLayout(OnlySiteLeft) then begin
    ClearChildControlAnchorSides(AControl);
    ConvertToOneControlType(OnlySiteLeft);
    exit;
  end;

  // remove a splitter and fill the gap
  SplitterCount:=0;
  for Side:=Low(TAnchorKind) to high(TAnchorKind) do begin
    Sibling:=AControl.AnchorSide[OppositeAnchor[Side]].Control;
    if Sibling is TAnchorDockSplitter then begin
      inc(SplitterCount);
      Splitter:=TAnchorDockSplitter(Sibling);
      if Splitter.SideAnchoredControlCount(Side)=1 then begin
        // Splitter is only used by AControl at Side
        RemoveControlBoundSplitter(Splitter,Side);
        exit;
      end;
    end;
  end;

  if SplitterCount=4 then begin
    RemoveSpiralSplitter(AControl);
    exit;
  end;

  ClearChildControlAnchorSides(AControl);
end;

procedure TAnchorDockHostSite.RemoveSpiralSplitter(AControl: TControl);
{ Merge two splitters and delete one of them.
  Prefer the pair with shortest distance between.

  For example:
                   3            3
     111111111111113            3
        2+--------+3            3
        2|AControl|3  --->  111111111
        2+--------+3            2
        24444444444444          2
        2                       2
   Everything anchored to 4 is now anchored to 1.
   And right side of 1 is now anchored to where the right side of 4 was anchored.
}
var
  Splitters: array[TAnchorKind] of TAnchorDockSplitter;
  Side: TAnchorKind;
  Keep: TAnchorKind;
  DeleteSplitter: TAnchorDockSplitter;
  i: Integer;
  Sibling: TControl;
  NextSide: TAnchorKind;
  NewBounds: TRect;
begin
  for Side:=low(TAnchorKind) to high(TAnchorKind) do
    Splitters[Side]:=AControl.AnchorSide[Side].Control as TAnchorDockSplitter;
  // Prefer the pair with shortest distance between
  if (Splitters[akRight].Left-Splitters[akLeft].Left)
    <(Splitters[akBottom].Top-Splitters[akTop].Top)
  then
    Keep:=akLeft
  else
    Keep:=akTop;
  DeleteSplitter:=Splitters[OppositeAnchor[Keep]];
  // transfer anchors from the deleting splitter to the kept splitter
  for i:=0 to ControlCount-1 do begin
    Sibling:=Controls[i];
    for Side:=low(TAnchorKind) to high(TAnchorKind) do begin
      if Sibling.AnchorSide[Side].Control=DeleteSplitter then
        Sibling.AnchorSide[Side].Control:=Splitters[Keep];
    end;
  end;
  // longen kept splitter
  NextSide:=ClockwiseAnchor[Keep];
  if Splitters[Keep].AnchorSide[NextSide].Control<>Splitters[NextSide] then
    NextSide:=OppositeAnchor[NextSide];
  Splitters[Keep].AnchorSide[NextSide].Control:=
                                    DeleteSplitter.AnchorSide[NextSide].Control;
  case NextSide of
  akTop: Splitters[Keep].Top:=DeleteSplitter.Top;
  akLeft: Splitters[Keep].Left:=DeleteSplitter.Left;
  akRight: Splitters[Keep].Width:=DeleteSplitter.Left+DeleteSplitter.Width-Splitters[Keep].Left;
  akBottom: Splitters[Keep].Height:=DeleteSplitter.Top+DeleteSplitter.Height-Splitters[Keep].Top;
  end;

  // move splitter to the middle
  if Keep=akLeft then
    Splitters[Keep].Left:=(Splitters[Keep].Left+DeleteSplitter.Left) div 2
  else
    Splitters[Keep].Top:=(Splitters[Keep].Top+DeleteSplitter.Top) div 2;
  // adjust all anchored controls
  for i:=0 to ControlCount-1 do begin
    Sibling:=Controls[i];
    for Side:=low(TAnchorKind) to high(TAnchorKind) do begin
      if Sibling.AnchorSide[Side].Control=Splitters[Keep] then begin
        NewBounds:=Sibling.BoundsRect;
        case Side of
        akTop: NewBounds.Top:=Splitters[Keep].Top+Splitters[Keep].Height;
        akLeft: NewBounds.Left:=Splitters[Keep].Left+Splitters[Keep].Width;
        akRight: NewBounds.Right:=Splitters[Keep].Left;
        akBottom: NewBounds.Bottom:=Splitters[Keep].Top;
        end;
        Sibling.BoundsRect:=NewBounds;
      end;
    end;
  end;

  // delete the splitter
  DeleteSplitter.Free;

  ClearChildControlAnchorSides(AControl);
end;

procedure TAnchorDockHostSite.ClearChildControlAnchorSides(AControl: TControl);
var
  Side: TAnchorKind;
  Sibling: TControl;
begin
  for Side:=Low(TAnchorKind) to high(TAnchorKind) do begin
    Sibling:=AControl.AnchorSide[Side].Control;
    if (Sibling=nil) then continue;
    if (Sibling.Parent=Self) then
      AControl.AnchorSide[Side].Control:=nil;
  end;
end;

procedure TAnchorDockHostSite.Simplify;
var
  AControl: TControl;
begin
  if (Pages<>nil) and (Pages.PageCount=1) then
    SimplifyPages
  else if (SiteType=adhstOneControl) then begin
    AControl:=GetOneControl;
    debugln(['TAnchorDockHostSite.Simplify ',DbgSName(Self),' ',DbgSName(AControl)]);
    if AControl is TAnchorDockHostSite then
      SimplifyOneControl
    else if (AControl=nil) or (csDestroying in AControl.ComponentState) then
      DockMaster.NeedFree(Self);
  end;
end;

procedure TAnchorDockHostSite.SimplifyPages;
var
  Page: TAnchorDockPage;
  Site: TAnchorDockHostSite;
begin
  if Pages=nil then exit;
  if Pages.PageCount=1 then begin
    debugln(['TAnchorDockHostSite.SimplifyPages "',Caption,'" PageCount=1']);
    DisableAutoSizing;
    BeginUpdateLayout;
    try
      // move the content of the Page to the place where Pages is
      Page:=Pages.DockPages[0];
      Site:=Page.GetSite;
      Site.Parent:=Self;
      if Site<>nil then
        CopyAnchorBounds(Pages,Site);
      if SiteType=adhstPages then
        FSiteType:=adhstOneControl;
      // free Pages
      FreeAndNil(FPages);
      if SiteType=adhstOneControl then
        SimplifyOneControl;
    finally
      EndUpdateLayout;
      EnableAutoSizing;
    end;
    //debugln(['TAnchorDockHostSite.SimplifyPages END Self="',Caption,'"']);
    //DebugWriteChildAnchors(GetParentForm(Self),true,true);
  end else if Pages.PageCount=0 then begin
    //debugln(['TAnchorDockHostSite.SimplifyPages "',Caption,'" PageCount=0 Self=',dbgs(Pointer(Self))]);
    FSiteType:=adhstNone;
    FreeAndNil(FPages);
    DockMaster.NeedSimplify(Self);
  end;
end;

procedure TAnchorDockHostSite.SimplifyOneControl;
var
  Site: TAnchorDockHostSite;
  i: Integer;
  Child: TControl;
  a: TAnchorKind;
begin
  if SiteType<>adhstOneControl then exit;
  if not IsOneSiteLayout(Site) then exit;
  debugln(['TAnchorDockHostSite.SimplifyOneControl Self="',Caption,'" Site="',Site.Caption,'"']);
  DisableAutoSizing;
  BeginUpdateLayout;
  try
    // move the content of Site up and free Site
    // Note: it is not possible to do it the other way round, because moving a
    // form to screen changes the z order and focus
    FSiteType:=Site.SiteType;

    // header
    Header.Align:=Site.Header.Align;
    Header.Caption:=Site.Header.Caption;
    UpdateHeaderShowing;
    Caption:=Site.Caption;

    Site.BeginUpdateLayout;
    // move controls from Site to Self
    i:=Site.ControlCount-1;
    while i>=0 do begin
      Child:=Site.Controls[i];
      if Child.Owner<>Site then begin
        //debugln(['TAnchorDockHostSite.SimplifyOneControl Self="',Caption,'" Child=',DbgSName(Child),'="',Child.Caption,'"']);
        Child.Parent:=Self;
        if Child=Site.Pages then begin
          FPages:=Site.Pages;
          Site.FPages:=nil;
        end;
        if Child.HostDockSite=Site then
          Child.HostDockSite:=Self;
        for a:=low(TAnchorKind) to high(TAnchorKind) do begin
          if Child.AnchorSide[a].Control=Site then
            Child.AnchorSide[a].Control:=Self;
        end;
      end;
      i:=Min(i,Site.ControlCount)-1;
    end;
    Site.EndUpdateLayout;

    // delete Site
    Site.FSiteType:=adhstNone;
    DockMaster.NeedFree(Site);
  finally
    EndUpdateLayout;
    EnableAutoSizing;
  end;

  //debugln(['TAnchorDockHostSite.SimplifyOneControl END Self="',Caption,'"']);
  //DebugWriteChildAnchors(GetParentForm(Self),true,true);
end;

function TAnchorDockHostSite.GetOneControl: TControl;
var
  i: Integer;
begin
  for i:=0 to ControlCount-1 do begin
    Result:=Controls[i];
    if Result.Owner<>Self then exit;
  end;
  Result:=nil;
end;

function TAnchorDockHostSite.GetSiteCount: integer;
var
  i: Integer;
  Child: TControl;
begin
  Result:=0;
  for i:=0 to ControlCount-1 do begin
    Child:=Controls[i];
    if not (Child is TAnchorDockHostSite) then continue;
    if not Child.IsVisible then continue;
    inc(Result);
  end;
end;

function TAnchorDockHostSite.IsOneSiteLayout(out Site: TAnchorDockHostSite
  ): boolean;
var
  i: Integer;
  Child: TControl;
begin
  Site:=nil;
  for i:=0 to ControlCount-1 do begin
    Child:=Controls[i];
    if not (Child is TAnchorDockHostSite) then continue;
    if not Child.IsVisible then continue;
    if Site<>nil then exit(false);
    Site:=TAnchorDockHostSite(Child);
  end;
  Result:=Site<>nil;
end;

function TAnchorDockHostSite.IsTwoSiteLayout(out Site1,
  Site2: TAnchorDockHostSite): boolean;
var
  i: Integer;
  Child: TControl;
begin
  Site1:=nil;
  Site2:=nil;
  for i:=0 to ControlCount-1 do begin
    Child:=Controls[i];
    if not (Child is TAnchorDockHostSite) then continue;
    if not Child.IsVisible then continue;
    if Site1=nil then
      Site1:=TAnchorDockHostSite(Child)
    else if Site2=nil then
      Site2:=TAnchorDockHostSite(Child)
    else
      exit(false);
  end;
  Result:=Site2<>nil;
end;

function TAnchorDockHostSite.GetUniqueSplitterName: string;
var
  i: Integer;
begin
  i:=0;
  repeat
    inc(i);
    Result:=AnchorDockSplitterName+IntToStr(i);
  until FindComponent(Result)=nil;
end;

function TAnchorDockHostSite.MakeSite(AControl: TControl): TAnchorDockHostSite;
begin
  if AControl is TAnchorDockHostSite then
    Result:=TAnchorDockHostSite(AControl)
  else begin
    Result:=DockMaster.CreateSite;
    try
      AControl.ManualDock(Result,nil,alClient);
    finally
      Result.EnableAutoSizing;
    end;
  end;
end;

procedure TAnchorDockHostSite.MoveAllControls(dx, dy: integer);
// move all children, except the sides that are anchored to parent left,top
var
  i: Integer;
  Child: TControl;
  NewBounds: TRect;
begin
  for i:=0 to ControlCount-1 do begin
    Child:=Controls[i];
    NewBounds:=Child.BoundsRect;
    OffsetRect(NewBounds,dx,dy);
    if Child.AnchorSideLeft.Control=Self then
      NewBounds.Left:=0;
    if Child.AnchorSideTop.Control=Self then
      NewBounds.Top:=0;
    Child.BoundsRect:=NewBounds;
  end;
end;

procedure TAnchorDockHostSite.AlignControls(AControl: TControl; var ARect: TRect);
var
  i: Integer;
  Child: TControl;
  Splitter: TAnchorDockSplitter;
begin
  inherited AlignControls(AControl, ARect);
  if csDestroying in ComponentState then exit;

  if DockMaster.ScaleOnResize and (not UpdatingLayout)
  and (not DockMaster.Restoring) then begin
    // scale splitters
    for i:=0 to ControlCount-1 do begin
      Child:=Controls[i];
      if not Child.IsVisible then continue;
      if Child is TAnchorDockSplitter then begin
        Splitter:=TAnchorDockSplitter(Child);
        //debugln(['TAnchorDockHostSite.AlignControls ',Caption,' ',DbgSName(Splitter),' OldBounds=',dbgs(Splitter.BoundsRect),' BaseBounds=',dbgs(Splitter.DockBounds),' BaseParentSize=',dbgs(Splitter.DockParentClientSize),' ParentSize=',ClientWidth,'x',ClientHeight]);
        if Splitter.ResizeAnchor in [akLeft,akRight] then begin
          if Splitter.DockParentClientSize.cx>0 then
            Splitter.SetBoundsKeepDockBounds(
              (Splitter.DockBounds.Left*ClientWidth) div Splitter.DockParentClientSize.cx,
              Splitter.Top,Splitter.Width,Splitter.Height);
        end else begin
          if Splitter.DockParentClientSize.cy>0 then
            Splitter.SetBoundsKeepDockBounds(Splitter.Left,
              (Splitter.DockBounds.Top*ClientHeight) div Splitter.DockParentClientSize.cy,
              Splitter.Width,Splitter.Height);
        end;
        //debugln(['TAnchorDockHostSite.AlignControls ',Caption,' ',DbgSName(Child),' NewBounds=',dbgs(Child.BoundsRect)]);
      end;
    end;
  end;
end;

function TAnchorDockHostSite.CheckIfOneControlHidden: boolean;
var
  Child: TControl;
begin
  Result:=false;
  //debugln(['TAnchorDockHostSite.CheckIfOneControlHidden ',DbgSName(Self),' UpdatingLayout=',UpdatingLayout,' Visible=',Visible,' Parent=',DbgSName(Parent),' csDestroying=',csDestroying in ComponentState,' SiteType=',dbgs(SiteType)]);
  if UpdatingLayout or (not Visible)
  or (csDestroying in ComponentState)
  or (SiteType<>adhstOneControl)
  then
    exit;
  Child:=GetOneControl;
  if (Child=nil) then exit;
  if Child.IsControlVisible then exit;

  // docked child was hidden/closed
  Result:=true;
  // => undock
  BeginUpdateLayout;
  DisableAutoSizing;
  debugln(['TAnchorDockHostSite.CheckIfOneControlHidden ',DbgSName(Self),' UpdatingLayout=',UpdatingLayout,' Visible=',Visible,' Parent=',DbgSName(Parent),' csDestroying=',csDestroying in ComponentState,' SiteType=',dbgs(SiteType),' Child=',DbgSName(Child),' Child.csDestroying=',csDestroying in Child.ComponentState]);
  Visible:=false;
  Parent:=nil;
  EnableAutoSizing;
  EndUpdateLayout;
  if (not (Child is TCustomForm)) or (csDestroying in Child.ComponentState) then
    Release;
end;

procedure TAnchorDockHostSite.DoDock(NewDockSite: TWinControl; var ARect: TRect);
begin
  inherited DoDock(NewDockSite, ARect);
  DockMaster.SimplifyPendingLayouts;
end;

procedure TAnchorDockHostSite.SetParent(NewParent: TWinControl);
var
  OldCaption: string;
  OldParent: TWinControl;
begin
  OldParent:=Parent;
  if NewParent=OldParent then exit;
  inherited SetParent(NewParent);
  OldCaption:=Caption;
  UpdateDockCaption;
  if OldCaption<>Caption then begin
    // UpdateDockCaption has not updated parents => do it now
    if Parent is TAnchorDockHostSite then
      TAnchorDockHostSite(Parent).UpdateDockCaption;
    if Parent is TAnchorDockPage then
      TAnchorDockPage(Parent).UpdateDockCaption;
  end;
  UpdateHeaderShowing;

  if (BoundSplitter<>nil) and (BoundSplitter.Parent<>Parent) then begin
    //debugln(['TAnchorDockHostSite.SetParent freeing splitter: ',DbgSName(BoundSplitter)]);
    FreeAndNil(FBoundSplitter);
  end;
  if Parent=nil then
    BorderStyle:=bsSizeable
  else
    BorderStyle:=bsNone;
end;

function TAnchorDockHostSite.HeaderNeedsShowing: boolean;
begin
  Result:=(SiteType<>adhstLayout) and (not (Parent is TAnchorDockPage));
end;

procedure TAnchorDockHostSite.DoClose(var CloseAction: TCloseAction);
begin
  inherited DoClose(CloseAction);
end;

function TAnchorDockHostSite.CanUndock: boolean;
begin
  Result:=Parent<>nil;
end;

procedure TAnchorDockHostSite.Undock;
var
  p: TPoint;
begin
  if Parent=nil then exit;
  DisableAutoSizing;
  p:=ClientOrigin;
  Parent:=nil;
  SetBounds(Left+p.x,Top+p.y,Width,Height);
  EnableAutoSizing;
end;

function TAnchorDockHostSite.CanMerge: boolean;
begin
  Result:=(SiteType=adhstLayout)
      and (Parent is TAnchorDockHostSite)
      and (TAnchorDockHostSite(Parent).SiteType=adhstLayout);
end;

procedure TAnchorDockHostSite.Merge;
{ Move all child controls to parent and delete this site
}
var
  ParentSite: TAnchorDockHostSite;
  i: Integer;
  Child: TControl;
  Side: TAnchorKind;
begin
  ParentSite:=Parent as TAnchorDockHostSite;
  if (SiteType<>adhstLayout) or (ParentSite.SiteType<>adhstLayout) then
    RaiseGDBException('');
  ParentSite.BeginUpdateLayout;
  DisableAutoSizing;
  try
    i:=0;
    while i<ControlCount-1 do begin
      Child:=Controls[i];
      if Child.Owner=Self then
        inc(i)
      else begin
        Child.Parent:=ParentSite;
        Child.SetBounds(Child.Left+Left,Child.Top+Top,Child.Width,Child.Height);
        for Side:=Low(TAnchorKind) to High(TAnchorKind) do begin
          if Child.AnchorSide[Side].Control=Self then
            Child.AnchorSide[Side].Assign(AnchorSide[Side]);
        end;
      end;
    end;
    Parent:=nil;
    DockMaster.NeedFree(Self);
  finally
    ParentSite.EndUpdateLayout;
  end;
end;

function TAnchorDockHostSite.EnlargeSide(Side: TAnchorKind;
  OnlyCheckIfPossible: boolean): boolean;
{
 Shrink one splitter, enlarge the other splitter.

     |#|         |#         |#|         |#
     |#| Control |#         |#|         |#
   --+#+---------+#   --> --+#| Control |#
   ===============#       ===#|         |#
   --------------+#       --+#|         |#
       A         |#        A|#|         |#
   --------------+#       --+#+---------+#
   ==================     ===================

 Move one neighbor splitter, enlarge Control, resize one splitter, rotate the other splitter.

     |#|         |#|          |#|         |#|
     |#| Control |#|          |#|         |#|
   --+#+---------+#+--  --> --+#| Control |#+--
   ===================      ===#|         |#===
   --------+#+--------      --+#|         |#+--
           |#|   B            |#|         |#|B
           |#+--------        |#|         |#+--
       A   |#=========       A|#|         |#===
           |#+--------        |#|         |#+--
           |#|   C            |#|         |#|C
   --------+#+--------      --+#+---------+#+--
   ===================      ===================
}
begin
  Result:=true;
  if EnlargeSideResizeTwoSplitters(Side,ClockwiseAnchor[Side],
                                   OnlyCheckIfPossible) then exit;
  if EnlargeSideResizeTwoSplitters(Side,OppositeAnchor[ClockwiseAnchor[Side]],
                                   OnlyCheckIfPossible) then exit;
  if EnlargeSideRotateSplitter(Side,OnlyCheckIfPossible) then exit;
  Result:=false;
end;

function TAnchorDockHostSite.EnlargeSideResizeTwoSplitters(ShrinkSplitterSide,
  EnlargeSpitterSide: TAnchorKind; OnlyCheckIfPossible: boolean): boolean;
{ Shrink one neighbor control, enlarge Self. Two splitters are resized.

  For example: ShrinkSplitterSide=akBottom, EnlargeSpitterSide=akLeft

    |#|        |#         |#|        |#
    |#|  Self  |#         |#|        |#
  --+#+--------+#   --> --+#|  Self  |#
  ==============#       ===#|        |#
  -------------+#       --+#|        |#
      A        |#        A|#|        |#
  -------------+#       --+#+--------+#
  =================     ==================



}
var
  ParentSite: TAnchorDockHostSite;
  ShrinkSplitter: TAnchorDockSplitter;
  EnlargeSplitter: TAnchorDockSplitter;
  KeptSide: TAnchorKind;
  KeptAnchorControl: TControl;
  Sibling: TControl;
  ShrinkControl: TControl;
  i: Integer;
begin
  Result:=false;
  if not (Parent is TAnchorDockHostSite) then exit;
  ParentSite:=TAnchorDockHostSite(Parent);
  if not OnlyCheckIfPossible then begin
    ParentSite.BeginUpdateLayout;
    ParentSite.DisableAutoSizing;
  end;
  try
    // check ShrinkSplitter
    ShrinkSplitter:=TAnchorDockSplitter(AnchorSide[ShrinkSplitterSide].Control);
    if not (ShrinkSplitter is TAnchorDockSplitter) then exit;
    // check if EnlargeSpitterSide is a neighbor ShrinkSplitterSide
    if (EnlargeSpitterSide<>ClockwiseAnchor[ShrinkSplitterSide])
    and (EnlargeSpitterSide<>OppositeAnchor[ClockwiseAnchor[ShrinkSplitterSide]]) then
      exit;
    // check EnlargeSpitter
    EnlargeSplitter:=TAnchorDockSplitter(AnchorSide[EnlargeSpitterSide].Control);
    if not (EnlargeSplitter is TAnchorDockSplitter) then exit;
    // check if KeptSide is anchored to a splitter or parent
    KeptSide:=OppositeAnchor[EnlargeSpitterSide];
    KeptAnchorControl:=AnchorSide[KeptSide].Control;
    if not ((KeptAnchorControl=ParentSite)
            or (KeptAnchorControl is TAnchorDockSplitter)) then exit;
    // check if ShrinkSplitter is anchored/stops at KeptAnchorControl
    if ShrinkSplitter.AnchorSide[KeptSide].Control<>KeptAnchorControl then exit;

    // check if there is a control to shrink
    ShrinkControl:=nil;
    for i:=0 to ShrinkSplitter.AnchoredControlCount-1 do begin
      Sibling:=ShrinkSplitter.AnchoredControls[i];
      if (Sibling.AnchorSide[OppositeAnchor[ShrinkSplitterSide]].Control=ShrinkSplitter)
      and (Sibling.AnchorSide[KeptSide].Control=KeptAnchorControl) then begin
        ShrinkControl:=Sibling;
        break;
      end;
    end;
    if ShrinkControl=nil then exit;

    if OnlyCheckIfPossible then begin
      // check if ShrinkControl is large enough for shrinking
      case EnlargeSpitterSide of
      akTop: if ShrinkControl.Top>=EnlargeSplitter.Top then exit;
      akLeft: if ShrinkControl.Left>=EnlargeSplitter.Left then exit;
      akRight: if ShrinkControl.Left+ShrinkControl.Width
                      <=EnlargeSplitter.Left+EnlargeSplitter.Width then exit;
      akBottom: if ShrinkControl.Top+ShrinkControl.Height
                      <=EnlargeSplitter.Top+EnlargeSplitter.Height then exit;
      end;
    end else begin
      // do it
      // enlarge the EnlargeSplitter and Self
      AnchorAndChangeBounds(EnlargeSplitter,ShrinkSplitterSide,
                          ShrinkControl.AnchorSide[ShrinkSplitterSide].Control);
      AnchorAndChangeBounds(Self,ShrinkSplitterSide,
                          ShrinkControl.AnchorSide[ShrinkSplitterSide].Control);
      // shrink the ShrinkSplitter and ShrinkControl
      AnchorAndChangeBounds(ShrinkSplitter,KeptSide,EnlargeSplitter);
      AnchorAndChangeBounds(ShrinkControl,KeptSide,EnlargeSplitter);
    end;

  finally
    if not OnlyCheckIfPossible then begin
      ParentSite.EnableAutoSizing;
      ParentSite.EndUpdateLayout;
    end;
  end;
  Result:=true;
end;

function TAnchorDockHostSite.EnlargeSideRotateSplitter(Side: TAnchorKind;
  OnlyCheckIfPossible: boolean): boolean;
{ Shrink splitter at Side, enlarge both neighbor splitters,
  rotate the splitter behind, enlarge Control,
  shrink controls at rotate splitter

     |#|         |#|          |#|         |#|
     |#| Control |#|          |#|         |#|
   --+#+---------+#+--  --> --+#| Control |#+--
   ===================      ===#|         |#===
   --------+#+--------      --+#|         |#+--
           |#|   B            |#|         |#|B
           |#+--------        |#|         |#+--
       A   |#=========       A|#|         |#===
           |#+--------        |#|         |#+--
           |#|   C            |#|         |#|C
   --------+#+--------      --+#+---------+#+--
   ===================      ===================
}
var
  Splitter: TAnchorDockSplitter;
  CWSide: TAnchorKind;
  CWSplitter: TAnchorDockSplitter;
  CCWSide: TAnchorKind;
  i: Integer;
  Sibling: TControl;
  BehindSide: TAnchorKind;
  RotateSplitter: TAnchorDockSplitter;
  CCWSplitter: TAnchorDockSplitter;
begin
  Result:=false;
  // check if there is a splitter at Side
  Splitter:=TAnchorDockSplitter(AnchorSide[Side].Control);
  if not (Splitter is TAnchorDockSplitter) then exit;
  // check if there is a splitter at clockwise Side
  CWSide:=ClockwiseAnchor[Side];
  CWSplitter:=TAnchorDockSplitter(AnchorSide[CWSide].Control);
  if not (CWSplitter is TAnchorDockSplitter) then exit;
  // check if there is a splitter at counter clockwise Side
  CCWSide:=OppositeAnchor[CWSide];
  CCWSplitter:=TAnchorDockSplitter(AnchorSide[CCWSide].Control);
  if not (CCWSplitter is TAnchorDockSplitter) then exit;
  // check if neighbor splitters end at Splitter
  if CWSplitter.AnchorSide[Side].Control<>Splitter then exit;
  if CCWSplitter.AnchorSide[Side].Control<>Splitter then exit;
  // find the rotate splitter behind Splitter
  BehindSide:=OppositeAnchor[Side];
  RotateSplitter:=nil;
  for i:=0 to Splitter.AnchoredControlCount-1 do begin
    Sibling:=Splitter.AnchoredControls[i];
    if Sibling.AnchorSide[BehindSide].Control<>Splitter then continue;
    if not (Sibling is TAnchorDockSplitter) then continue;
    if Side in [akLeft,akRight] then begin
      if Sibling.Top<Top-DockMaster.SplitterWidth then continue;
      if Sibling.Top>Top+Height then continue;
    end else begin
      if Sibling.Left<Left-DockMaster.SplitterWidth then continue;
      if Sibling.Left>Left+Width then continue;
    end;
    if RotateSplitter=nil then
      RotateSplitter:=TAnchorDockSplitter(Sibling)
    else
      // there are multiple splitters behind
      exit;
  end;
  if RotateSplitter=nil then exit;
  // check that all siblings at RotateSplitter are large enough to shrink
  for i:=0 to RotateSplitter.AnchoredControlCount-1 do begin
    Sibling:=RotateSplitter.AnchoredControls[i];
    if Side in [akLeft,akRight] then begin
      if (Sibling.Top>Top-DockMaster.SplitterWidth)
      and (Sibling.Top+Sibling.Height<Top+Height+DockMaster.SplitterWidth) then
        exit;
    end else begin
      if (Sibling.Left>Left-DockMaster.SplitterWidth)
      and (Sibling.Left+Sibling.Width<Left+Width+DockMaster.SplitterWidth) then
        exit;
    end;
  end;
  Result:=true;
  if OnlyCheckIfPossible then exit;

  //debugln(['TAnchorDockHostSite.EnlargeSideRotateSplitter BEFORE Self=',DbgSName(Self),'=',dbgs(BoundsRect),' Side=',dbgs(Side),' CWSide=',dbgs(CWSide),' CWSplitter=',CWSplitter.Name,'=',dbgs(CWSplitter.BoundsRect),' CCWSide=',dbgs(CCWSide),' CCWSplitter=',CCWSplitter.Name,'=',dbgs(CCWSplitter.BoundsRect),' Behind=',dbgs(BehindSide),'=',RotateSplitter.Name,'=',dbgs(RotateSplitter.BoundsRect)]);

  DisableAutoSizing;
  try
    // enlarge the two neighbor splitters
    AnchorAndChangeBounds(CWSplitter,Side,RotateSplitter.AnchorSide[Side].Control);
    AnchorAndChangeBounds(CCWSplitter,Side,RotateSplitter.AnchorSide[Side].Control);
    // enlarge control
    AnchorAndChangeBounds(Self,Side,RotateSplitter.AnchorSide[Side].Control);
    // shrink the neighbors and anchor them to the enlarge splitters
    for i:=0 to Parent.ControlCount-1 do begin
      Sibling:=Parent.Controls[i];
      if Sibling.AnchorSide[CWSide].Control=RotateSplitter then
        AnchorAndChangeBounds(Sibling,CWSide,CCWSplitter)
      else if Sibling.AnchorSide[CCWSide].Control=RotateSplitter then
        AnchorAndChangeBounds(Sibling,CCWSide,CWSplitter);
    end;
    // rotate the RotateSplitter
    RotateSplitter.AnchorSide[Side].Control:=nil;
    RotateSplitter.AnchorSide[BehindSide].Control:=nil;
    RotateSplitter.ResizeAnchor:=Side;
    AnchorAndChangeBounds(RotateSplitter,CCWSide,Splitter.AnchorSide[CCWSide].Control);
    AnchorAndChangeBounds(RotateSplitter,CWSide,CCWSplitter);
    if Side in [akLeft,akRight] then begin
      RotateSplitter.Left:=Splitter.Left;
      RotateSplitter.Width:=DockMaster.SplitterWidth;
    end else begin
      RotateSplitter.Top:=Splitter.Top;
      RotateSplitter.Height:=DockMaster.SplitterWidth;
    end;
    // shrink Splitter
    AnchorAndChangeBounds(Splitter,CCWSide,CWSplitter);
    // anchor some siblings of Splitter to RotateSplitter
    for i:=0 to Parent.ControlCount-1 do begin
      Sibling:=Parent.Controls[i];
      case Side of
      akLeft: if Sibling.Top<Top then continue;
      akRight: if Sibling.Top>Top then continue;
      akTop: if Sibling.Left>Left then continue;
      akBottom: if Sibling.Left<Left then continue;
      end;
      if Sibling.AnchorSide[BehindSide].Control=Splitter then
        Sibling.AnchorSide[BehindSide].Control:=RotateSplitter
      else if Sibling.AnchorSide[Side].Control=Splitter then
        Sibling.AnchorSide[Side].Control:=RotateSplitter;
    end;
    //debugln(['TAnchorDockHostSite.EnlargeSideRotateSplitter AFTER Self=',DbgSName(Self),'=',dbgs(BoundsRect),' Side=',dbgs(Side),' CWSide=',dbgs(CWSide),' CWSplitter=',CWSplitter.Name,'=',dbgs(CWSplitter.BoundsRect),' CCWSide=',dbgs(CCWSide),' CCWSplitter=',CCWSplitter.Name,'=',dbgs(CCWSplitter.BoundsRect),' Behind=',dbgs(BehindSide),'=',RotateSplitter.Name,'=',dbgs(RotateSplitter.BoundsRect)]);
  finally
    EnableAutoSizing;
  end;
end;

procedure TAnchorDockHostSite.CreateBoundSplitter;
begin
  if BoundSplitter<>nil then exit;
  FBoundSplitter:=DockMaster.CreateSplitter;
  BoundSplitter.FreeNotification(Self);
  BoundSplitter.Align:=Align;
  BoundSplitter.Parent:=Parent;
end;

procedure TAnchorDockHostSite.PositionBoundSplitter;
begin
  case Align of
  alTop: BoundSplitter.SetBounds(0,Height,Parent.ClientWidth,BoundSplitter.Height);
  alBottom: BoundSplitter.SetBounds(0,Parent.ClientHeight-Height-BoundSplitter.Height,
                                Parent.ClientWidth,BoundSplitter.Height);
  alLeft: BoundSplitter.SetBounds(Width,0,BoundSplitter.Width,Parent.ClientHeight);
  alRight: BoundSplitter.SetBounds(Parent.ClientWidth-Width-BoundSplitter.Width,0
                              ,BoundSplitter.Width,Parent.ClientHeight);
  end;
end;

function TAnchorDockHostSite.CloseQuery: boolean;

  function Check(AControl: TWinControl): boolean;
  var
    i: Integer;
    Child: TControl;
  begin
    for i:=0 to AControl.ControlCount-1 do begin
      Child:=AControl.Controls[i];
      if Child is TWinControl then begin
        if Child is TCustomForm then begin
          if not TCustomForm(Child).CloseQuery then exit(false);
        end else begin
          if not Check(TWinControl(Child)) then exit(false);
        end;
      end;
    end;
    Result:=true;
  end;

begin
  Result:=Check(Self);
end;

function TAnchorDockHostSite.CloseSite: Boolean;
var
  AControl: TControl;
  AForm: TCustomForm;
  IsMainForm: Boolean;
  CloseAction: TCloseAction;
begin
  Result:=CloseQuery;
  if not Result then exit;

  debugln(['TAnchorDockHostSite.CloseSite ',DbgSName(Self),' SiteType=',dbgs(SiteType)]);
  case SiteType of
  adhstNone:
    begin
      Release;
      exit;
    end;
  adhstOneControl:
    begin
      DisableAutoSizing;
      AControl:=GetOneControl;
      if AControl is TCustomForm then begin
        AForm:=TCustomForm(AControl);
        IsMainForm := (Application.MainForm = AForm)
                      or (AForm.IsParentOf(Application.MainForm));
        if IsMainForm then
          CloseAction := caFree
        else
          CloseAction := caHide;
        // ToDo: TCustomForm(AControl).DoClose(CloseAction);
        case CloseAction of
        caHide: Hide;
        caMinimize: WindowState := wsMinimized;
        caFree:
          begin
            // if form is MainForm, then terminate the application
            // the owner of the MainForm is the application,
            // so the Application will take care of free-ing the form
            // and Release is not necessary
            if IsMainForm then
              Application.Terminate
            else begin
              Release;
              AForm.Release;
              exit;
            end;
          end;
        end;
      end else begin
        AControl.Visible:=false;
        Release;
        exit;
      end;
      Visible:=false;
      Parent:=nil;
      EnableAutoSizing;
    end;
  end;
end;

procedure TAnchorDockHostSite.RemoveControl(AControl: TControl);
begin
  //debugln(['TAnchorDockHostSite.RemoveControl ',DbgSName(Self),'=',Caption,' ',DbgSName(AControl)]);
  DisableAutoSizing{$IFDEF DebugDisableAutoSizing}('TAnchorDockHostSite.RemoveControl'){$ENDIF};
  inherited RemoveControl(AControl);
  if not (csDestroying in ComponentState) then begin
    if (not ((AControl is TAnchorDockHeader)
             or (AControl is TAnchorDockSplitter)))
    then begin
      //debugln(['TAnchorDockHostSite.RemoveControl START ',Caption,' ',dbgs(SiteType),' ',DbgSName(AControl),' UpdatingLayout=',UpdatingLayout]);
      if (SiteType=adhstLayout) then
        RemoveControlFromLayout(AControl)
      else
        DockMaster.NeedSimplify(Self);
      UpdateDockCaption;
      //debugln(['TAnchorDockHostSite.RemoveControl END ',Caption,' ',dbgs(SiteType),' ',DbgSName(AControl)]);
    end;
  end;
  EnableAutoSizing{$IFDEF DebugDisableAutoSizing}('TAnchorDockHostSite.RemoveControl'){$ENDIF};
end;

procedure TAnchorDockHostSite.InsertControl(AControl: TControl; Index: integer);
begin
  DisableAutoSizing;
  try
    inherited InsertControl(AControl, Index);
    if not ((AControl is TAnchorDockSplitter)
            or (AControl is TAnchorDockHeader))
    then
      UpdateDockCaption;
  finally
    EnableAutoSizing;
  end;
end;

procedure TAnchorDockHostSite.UpdateDockCaption(Exclude: TControl);
var
  i: Integer;
  Child: TControl;
  NewCaption, OldCaption: String;
begin
  if csDestroying in ComponentState then exit;
  NewCaption:='';
  for i:=0 to ControlCount-1 do begin
    Child:=Controls[i];
    if Child=Exclude then continue;
    if (Child.HostDockSite=Self) or (Child is TAnchorDockHostSite)
    or (Child is TAnchorDockPageControl) then begin
      if NewCaption<>'' then
        NewCaption:=NewCaption+',';
      NewCaption:=NewCaption+Child.Caption;
    end;
  end;
  OldCaption:=Caption;
  Caption:=NewCaption;
  if ((Parent=nil) and DockMaster.HideHeaderCaptionFloatingControl)
  or (not DockMaster.ShowHeaderCaption) then
    Header.Caption:=''
  else
    Header.Caption:=Caption;
  if OldCaption<>Caption then begin
    //debugln(['TAnchorDockHostSite.UpdateDockCaption Caption="',Caption,'" NewCaption="',NewCaption,'" HasParent=',Parent<>nil]);
    if Parent is TAnchorDockHostSite then
      TAnchorDockHostSite(Parent).UpdateDockCaption;
    if Parent is TAnchorDockPage then
      TAnchorDockPage(Parent).UpdateDockCaption;
  end;

  // do not show close button for mainform
  Header.CloseButton.Visible:=not IsParentOf(Application.MainForm);
end;

procedure TAnchorDockHostSite.GetSiteInfo(Client: TControl;
  var InfluenceRect: TRect; MousePos: TPoint; var CanDock: Boolean);
var
  ADockMargin: LongInt;
begin
  GetWindowRect(Handle, InfluenceRect);

  if (Parent=nil) or DockMaster.IsCustomSite(Parent) then begin
    // allow docking outside => enlarge margins
    ADockMargin:=DockMaster.DockOutsideMargin;
    //debugln(['TAnchorDockHostSite.GetSiteInfo ',DbgSName(Self),' allow outside ADockMargin=',ADockMargin,' ',dbgs(InfluenceRect)]);
    InfluenceRect.Left := InfluenceRect.Left-ADockMargin;
    InfluenceRect.Top := InfluenceRect.Top-ADockMargin;
    InfluenceRect.Right := InfluenceRect.Right+ADockMargin;
    InfluenceRect.Bottom := InfluenceRect.Bottom+ADockMargin;
  end else if Parent is TAnchorDockHostSite then begin
    // do not cover parent site => shrink margins
    ADockMargin:=DockMaster.DockParentMargin;
    ADockMargin:=Min(ADockMargin,Min(ClientWidth,ClientHeight) div 10);
    ADockMargin:=Max(0,ADockMargin);
    //debugln(['TAnchorDockHostSite.GetSiteInfo ',DbgSName(Self),' do not cover parent ADockMargin=',ADockMargin,' ',dbgs(InfluenceRect)]);
    InfluenceRect.Left := InfluenceRect.Left+ADockMargin;
    InfluenceRect.Top := InfluenceRect.Top+ADockMargin;
    InfluenceRect.Right := InfluenceRect.Right-ADockMargin;
    InfluenceRect.Bottom := InfluenceRect.Bottom-ADockMargin;
  end;

  CanDock:=(Client is TAnchorDockHostSite)
           and not DockMaster.AutoFreedIfControlIsRemoved(Self,Client);
  //debugln(['TAnchorDockHostSite.GetSiteInfo ',DbgSName(Self),' ',dbgs(BoundsRect),' ',Caption,' CanDock=',CanDock,' PtIn=',PtInRect(InfluenceRect,MousePos)]);

  if Assigned(OnGetSiteInfo) then
    OnGetSiteInfo(Self, Client, InfluenceRect, MousePos, CanDock);
end;

function TAnchorDockHostSite.GetPageArea: TRect;
begin
  Result:=Rect(0,0,Width*DockMaster.PageAreaInPercent div 100,
               Height*DockMaster.PageAreaInPercent div 100);
  OffsetRect(Result,(Width*(100-DockMaster.PageAreaInPercent)) div 200,
                    (Height*(100-DockMaster.PageAreaInPercent)) div 200);
end;

procedure TAnchorDockHostSite.ChangeBounds(ALeft, ATop, AWidth,
  AHeight: integer; KeepBase: boolean);
begin
  inherited ChangeBounds(ALeft, ATop, AWidth, AHeight, KeepBase);
  if Header<>nil then UpdateHeaderAlign;
end;

procedure TAnchorDockHostSite.UpdateHeaderAlign;
begin
  if Header=nil then exit;
  case Header.HeaderPosition of
  adlhpAuto:
    if Header.Align in [alLeft,alRight] then begin
      if (ClientHeight>0)
      and ((ClientWidth*100 div ClientHeight)<=DockMaster.HeaderAlignTop) then
        Header.Align:=alTop;
    end else begin
      if (ClientHeight>0)
      and ((ClientWidth*100 div ClientHeight)>=DockMaster.HeaderAlignLeft) then
      begin
        if Application.BidiMode=bdRightToLeft then
          Header.Align:=alRight
        else
          Header.Align:=alLeft;
      end;
    end;
  adlhpLeft: Header.Align:=alLeft;
  adlhpTop: Header.Align:=alTop;
  adlhpRight: Header.Align:=alRight;
  adlhpBottom: Header.Align:=alBottom;
  end;
end;

procedure TAnchorDockHostSite.UpdateHeaderShowing;
begin
  if Header=nil then exit;
  if HeaderNeedsShowing then
    Header.Parent:=Self
  else
    Header.Parent:=nil;
end;

procedure TAnchorDockHostSite.BeginUpdateLayout;
begin
  inc(fUpdateLayout);
  if fUpdateLayout=1 then DockMaster.BeginUpdate;
end;

procedure TAnchorDockHostSite.EndUpdateLayout;
begin
  if fUpdateLayout=0 then RaiseGDBException('TAnchorDockHostSite.EndUpdateLayout');
  dec(fUpdateLayout);
  if fUpdateLayout=0 then
    DockMaster.EndUpdate;
end;

function TAnchorDockHostSite.UpdatingLayout: boolean;
begin
  Result:=(fUpdateLayout>0) or (csDestroying in ComponentState);
end;

procedure TAnchorDockHostSite.SaveLayout(
  LayoutTree: TAnchorDockLayoutTree; LayoutNode: TAnchorDockLayoutTreeNode);
var
  i: Integer;
  Site: TAnchorDockHostSite;
  ChildNode: TAnchorDockLayoutTreeNode;
  Child: TControl;
  Splitter: TAnchorDockSplitter;
  OneControl: TControl;
begin
  if SiteType=adhstOneControl then
    OneControl:=GetOneControl
  else
    OneControl:=nil;
  if (SiteType=adhstOneControl) and (OneControl<>nil)
  and (not (OneControl is TAnchorDockHostSite)) then begin
    LayoutNode.NodeType:=adltnControl;
    LayoutNode.Assign(Self);
    LayoutNode.Name:=OneControl.Name;
    LayoutNode.HeaderPosition:=Header.HeaderPosition;
  end else if (SiteType in [adhstLayout,adhstOneControl]) then begin
    LayoutNode.NodeType:=adltnLayout;
    for i:=0 to ControlCount-1 do begin
      Child:=Controls[i];
      if Child.Owner=Self then continue;
      if (Child is TAnchorDockHostSite) then begin
        Site:=TAnchorDockHostSite(Child);
        ChildNode:=LayoutTree.NewNode(LayoutNode);
        Site.SaveLayout(LayoutTree,ChildNode);
      end else if (Child is TAnchorDockSplitter) then begin
        Splitter:=TAnchorDockSplitter(Child);
        ChildNode:=LayoutTree.NewNode(LayoutNode);
        Splitter.SaveLayout(ChildNode);
      end;
    end;
    LayoutNode.Assign(Self);
    LayoutNode.HeaderPosition:=Header.HeaderPosition;
  end else if SiteType=adhstPages then begin
    LayoutNode.NodeType:=adltnPages;
    for i:=0 to Pages.PageCount-1 do begin
      Site:=Pages.DockPages[i].GetSite;
      if Site<>nil then begin
        ChildNode:=LayoutTree.NewNode(LayoutNode);
        Site.SaveLayout(LayoutTree,ChildNode);
      end;
    end;
    LayoutNode.Assign(Self);
    LayoutNode.HeaderPosition:=Header.HeaderPosition;
  end else
    LayoutNode.NodeType:=adltnNone;
  if BoundSplitter<>nil then begin
    if Align in [alLeft,alRight] then
      LayoutNode.BoundSplitterPos:=BoundSplitter.Left
    else
      LayoutNode.BoundSplitterPos:=BoundSplitter.Top;
  end;
end;

constructor TAnchorDockHostSite.CreateNew(AOwner: TComponent; Num: Integer);
begin
  inherited CreateNew(AOwner,Num);
  Visible:=false;
  FHeaderSide:=akTop;
  FHeader:=DockMaster.HeaderClass.Create(Self);
  FHeader.Align:=alTop;
  FHeader.Parent:=Self;
  FSiteType:=adhstNone;
  UpdateHeaderAlign;
  DragKind:=dkDock;
  DockManager:=DockMaster.ManagerClass.Create(Self);
  UseDockManager:=true;
  DragManager.RegisterDockSite(Self,true);
end;

destructor TAnchorDockHostSite.Destroy;
//var i: Integer;
begin
  //debugln(['TAnchorDockHostSite.Destroy ',DbgSName(Self),' Caption="',Caption,'" Self=',dbgs(Pointer(Self)),' ComponentCount=',ComponentCount,' ControlCount=',ControlCount]);
  {for i:=0 to ComponentCount-1 do
    debugln(['TAnchorDockHostSite.Destroy Component ',i,'/',ComponentCount,' ',DbgSName(Components[i])]);
  for i:=0 to ControlCount-1 do
    debugln(['TAnchorDockHostSite.Destroy Control ',i,'/',ControlCount,' ',DbgSName(Controls[i])]);}
  FreeAndNil(FPages);
  inherited Destroy;
end;

{ TAnchorDockHeader }

procedure TAnchorDockHeader.PopupMenuPopup(Sender: TObject);
var
  HeaderPosItem: TMenuItem;
  ParentSite: TAnchorDockHostSite;
  Side: TAnchorKind;
  SideCaptions: array[TAnchorKind] of string;
  Item: TMenuItem;
  ContainsMainForm: boolean;
  s: String;
begin
  ParentSite:=TAnchorDockHostSite(Parent);
  SideCaptions[akLeft]:=adrsLeft;
  SideCaptions[akTop]:=adrsTop;
  SideCaptions[akRight]:=adrsRight;
  SideCaptions[akBottom]:=adrsBottom;

  // undock, merge
  DockMaster.AddRemovePopupMenuItem(ParentSite.CanUndock,'UndockMenuItem',
                                    adrsUndock,@UndockButtonClick);
  DockMaster.AddRemovePopupMenuItem(ParentSite.CanMerge,'MergeMenuItem',
                                    adrsMerge, @MergeButtonClick);

  // header position
  HeaderPosItem:=DockMaster.AddPopupMenuItem('HeaderPosMenuItem',
                                             adrsHeaderPosition, nil);
  Item:=DockMaster.AddPopupMenuItem('HeaderPosAutoMenuItem', adrsAutomatically,
                   @HeaderPositionItemClick, HeaderPosItem);
  if Item<>nil then begin
    Item.Tag:=ord(adlhpAuto);
    Item.Checked:=HeaderPosition=TADLHeaderPosition(Item.Tag);
  end;
  for Side:=Low(TAnchorKind) to High(TAnchorKind) do begin
    Item:=DockMaster.AddPopupMenuItem('HeaderPos'+DbgS(Side)+'MenuItem',
                     SideCaptions[Side], @HeaderPositionItemClick,
                     HeaderPosItem);
    if Item=nil then continue;
    Item.Tag:=ord(Side)+1;
    Item.Checked:=HeaderPosition=TADLHeaderPosition(Item.Tag);
  end;

  // enlarge
  for Side:=Low(TAnchorKind) to High(TAnchorKind) do begin
    Item:=DockMaster.AddRemovePopupMenuItem(ParentSite.EnlargeSide(Side,true),
      'Enlarge'+DbgS(Side)+'MenuItem', Format(adrsEnlargeSide, [
        SideCaptions[Side]]),@EnlargeSideClick);
    if Item<>nil then Item.Tag:=ord(Side);
  end;

  // close
  ContainsMainForm:=ParentSite.IsParentOf(Application.MainForm);
  if ContainsMainForm then
    s:=Format(adrsQuit, [Application.Title])
  else
    s:=adrsClose;
  DockMaster.AddRemovePopupMenuItem(CloseButton.Visible,'CloseMenuItem',s,
                                    @CloseButtonClick);
end;

procedure TAnchorDockHeader.CloseButtonClick(Sender: TObject);
begin
  if Parent is TAnchorDockHostSite then begin
    DockMaster.RestoreLayouts.Add(DockMaster.CreateRestoreLayout(Parent),true);
    TAnchorDockHostSite(Parent).CloseSite;
  end;
end;

procedure TAnchorDockHeader.HeaderPositionItemClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  if not (Sender is TMenuItem) then exit;
  Item:=TMenuItem(Sender);
  HeaderPosition:=TADLHeaderPosition(Item.Tag);
end;

procedure TAnchorDockHeader.UndockButtonClick(Sender: TObject);
begin
  TAnchorDockHostSite(Parent).Undock;
end;

procedure TAnchorDockHeader.MergeButtonClick(Sender: TObject);
begin
  TAnchorDockHostSite(Parent).Merge;
end;

procedure TAnchorDockHeader.EnlargeSideClick(Sender: TObject);
var
  Side: TAnchorKind;
begin
  if not (Sender is TMenuItem) then exit;
  Side:=TAnchorKind(TMenuItem(Sender).Tag);
  TAnchorDockHostSite(Parent).EnlargeSide(Side,false);
end;

procedure TAnchorDockHeader.SetHeaderPosition(const AValue: TADLHeaderPosition);
begin
  if FHeaderPosition=AValue then exit;
  FHeaderPosition:=AValue;
  if Parent is TAnchorDockHostSite then
    TAnchorDockHostSite(Parent).UpdateHeaderAlign;
end;

procedure TAnchorDockHeader.Paint;

  procedure DrawGrabber(r: TRect);
  begin
    Canvas.Frame3d(r,2,bvLowered);
    Canvas.Frame3d(r,4,bvRaised);
  end;

var
  r: TRect;
  TxtH: longint;
  TxtW: longint;
  dx,dy: Integer;
begin
  r:=ClientRect;
  Canvas.Frame3d(r,1,bvRaised);
  Canvas.FillRect(r);
  if CloseButton.IsControlVisible and (CloseButton.Parent=Self) then begin
    if Align in [alLeft,alRight] then
      r.Top:=CloseButton.Top+CloseButton.Height+1
    else
      r.Right:=CloseButton.Left-1;
  end;

  // caption
  if Caption<>'' then begin
    Canvas.Brush.Color:=clNone;
    Canvas.Brush.Style:=bsClear;
    TxtH:=Canvas.TextHeight('ABCMgq');
    TxtW:=Canvas.TextWidth(Caption);
    if Align in [alLeft,alRight] then begin
      // vertical
      dx:=Max(0,(r.Right-r.Left-TxtH) div 2);
      dy:=Max(0,(r.Bottom-r.Top-TxtW) div 2);
      Canvas.Font.Orientation:=900;
      Canvas.TextOut(r.Left+dx,r.Bottom-dy,Caption);
      DrawGrabber(Rect(r.Left,r.Top,r.Right,r.Bottom-dy-TxtW-1));
      DrawGrabber(Rect(r.Left,r.Bottom-dy+1,r.Right,r.Bottom));
    end else begin
      // horizontal
      dx:=Max(0,(r.Right-r.Left-TxtW) div 2);
      dy:=Max(0,(r.Bottom-r.Top-TxtH) div 2);
      Canvas.Font.Orientation:=0;
      Canvas.TextOut(r.Left+dx,r.Top+dy,Caption);
      DrawGrabber(Rect(r.Left,r.Top,r.Left+dx-1,r.Bottom));
      DrawGrabber(Rect(r.Left+dx+TxtW+2,r.Top,r.Right,r.Bottom));
    end;
  end else
    DrawGrabber(r);
end;

procedure TAnchorDockHeader.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
const
  TestTxt = 'ABCXYZ123gqj';
var
  DC: HDC;
  R: TRect;
  OldFont: HGDIOBJ;
  Flags: cardinal;
  NeededHeight: Integer;
begin
  inherited CalculatePreferredSize(PreferredWidth,PreferredHeight,WithThemeSpace);
  if Caption<>'' then begin
    DC := GetDC(Parent.Handle);
    try
      R := Rect(0, 0, 10000, 10000);
      OldFont := SelectObject(DC, HGDIOBJ(Font.Reference.Handle));
      Flags := DT_CALCRECT or DT_EXPANDTABS or DT_SINGLELINE or DT_NOPREFIX;

      DrawText(DC, PChar(TestTxt), Length(TestTxt), R, Flags);
      SelectObject(DC, OldFont);
      NeededHeight := R.Bottom - R.Top + BevelWidth*2;
    finally
      ReleaseDC(Parent.Handle, DC);
    end;
    if Align in [alLeft,alRight] then begin
      PreferredWidth:=Max(NeededHeight,PreferredWidth);
    end else begin
      PreferredHeight:=Max(NeededHeight,PreferredHeight);
    end;
  end;
end;

procedure TAnchorDockHeader.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if (Button=mbLeft) and DockMaster.AllowDragging then
    DragManager.DragStart(Parent,false,DockMaster.DragTreshold);
end;

procedure TAnchorDockHeader.UpdateHeaderControls;
begin
  if Align in [alLeft,alRight] then begin
    if CloseButton<>nil then
      CloseButton.Align:=alTop;
  end else begin
    if CloseButton<>nil then
      CloseButton.Align:=alRight;
  end;
  //debugln(['TAnchorDockHeader.UpdateHeaderControls ',dbgs(Align),' ',dbgs(CloseButton.Align)]);
end;

procedure TAnchorDockHeader.SetAlign(Value: TAlign);
begin
  if Value=Align then exit;
  DisableAutoSizing;
  inherited SetAlign(Value);
  UpdateHeaderControls;
  EnableAutoSizing;
end;

procedure TAnchorDockHeader.DoOnShowHint(HintInfo: PHintInfo);
var
  s: String;
  p: LongInt;
  c: String;
begin
  s:=DockMaster.HeaderHint;
  p:=Pos('%c',s);
  if p>0 then begin
    if Parent<>nil then
      c:=Parent.Caption
    else
      c:='';
    s:=copy(s,1,p-1)+c+copy(s,p+2,length(s));
  end;
  //debugln(['TAnchorDockHeader.DoOnShowHint "',s,'" "',DockMaster.HeaderHint,'"']);
  HintInfo^.HintStr:=s;
  inherited DoOnShowHint(HintInfo);
end;

constructor TAnchorDockHeader.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FHeaderPosition:=adlhpAuto;
  FCloseButton:=TAnchorDockCloseButton.Create(Self);
  BevelOuter:=bvNone;
  BorderWidth:=0;
  with FCloseButton do begin
    Name:='CloseButton';
    Parent:=Self;
    Flat:=true;
    ShowHint:=true;
    Hint:=adrsClose;
    OnClick:=@CloseButtonClick;
    AutoSize:=true;
  end;
  Align:=alTop;
  AutoSize:=true;
  ShowHint:=true;
  PopupMenu:=DockMaster.GetPopupMenu;
end;

{ TAnchorDockCloseButton }

constructor TAnchorDockCloseButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GetCloseGlyph;
  Glyph:=DockMaster.fCloseBtnBitmap;
end;

destructor TAnchorDockCloseButton.Destroy;
begin
  ReleaseCloseGlyph;
  inherited Destroy;
end;

procedure TAnchorDockCloseButton.GetCloseGlyph;
begin
  inc(DockMaster.fCloseBtnReferenceCount);
  if DockMaster.fCloseBtnBitmap=nil then
    DockMaster.CreateCloseButtonBitmap;
end;

procedure TAnchorDockCloseButton.ReleaseCloseGlyph;
begin
  if DockMaster=nil then exit;
  dec(DockMaster.fCloseBtnReferenceCount);
  if DockMaster.fCloseBtnReferenceCount=0 then
    DockMaster.FreeCloseButtonBitmap;
end;

function TAnchorDockCloseButton.GetGlyphSize(Drawing: boolean; PaintRect: TRect
  ): TSize;
begin
  if PaintRect.Left=0 then ;
  Result.cx:=DockMaster.fCloseBtnBitmap.Width;
  Result.cy:=DockMaster.fCloseBtnBitmap.Height;
end;

function TAnchorDockCloseButton.DrawGlyph(ACanvas: TCanvas;
  const AClient: TRect; const AOffset: TPoint; AState: TButtonState;
  ATransparent: Boolean; BiDiFlags: Longint): TRect;
var
  closeBmp: TBitmap;
begin
  if BiDiFlags=0 then ;
  closeBmp := DockMaster.fCloseBtnBitmap;
  closeBmp.Transparent := ATransparent;
  if ATransparent then
    closeBmp.TransparentMode := tmAuto;
  if AState=bsDisabled then ;
  Result:=Rect(0,0,closeBmp.Width,closeBmp.Height);
  OffsetRect(Result,AClient.Left+AOffset.X,AClient.Top+AOffset.Y);
  ACanvas.Draw(Result.Left,Result.Top,closeBmp);
end;

procedure TAnchorDockCloseButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  if WithThemeSpace then ;
  PreferredWidth:=DockMaster.fCloseBtnBitmap.Width+4;
  PreferredHeight:=DockMaster.fCloseBtnBitmap.Height+4;
end;

{ TAnchorDockManager }

procedure TAnchorDockManager.SetPreferredSiteSizeAsSiteMinimum(
  const AValue: boolean);
begin
  if FPreferredSiteSizeAsSiteMinimum=AValue then exit;
  FPreferredSiteSizeAsSiteMinimum:=AValue;
  if DockSite=nil then
    Site.AdjustSize;
end;

constructor TAnchorDockManager.Create(ADockSite: TWinControl);
begin
  inherited Create(ADockSite);
  FSite:=ADockSite;
  FDockableSites:=[akLeft,akTop,akBottom,akRight];
  FInsideDockingAllowed:=true;
  FPreferredSiteSizeAsSiteMinimum:=true;
  if (ADockSite is TAnchorDockHostSite) then
    FDockSite:=TAnchorDockHostSite(ADockSite);
end;

procedure TAnchorDockManager.GetControlBounds(Control: TControl; out
  AControlBounds: TRect);
begin
  if Control=nil then ;
  AControlBounds:=Rect(0,0,0,0);
  //debugln(['TAnchorDockManager.GetControlBounds DockSite="',DockSite.Caption,'" Control=',DbgSName(Control)]);
end;

procedure TAnchorDockManager.InsertControl(Control: TControl; InsertAt: TAlign;
  DropCtl: TControl);
begin
  if Control=nil then;
  if InsertAt=alNone then ;
  if DropCtl=nil then ;
end;

procedure TAnchorDockManager.InsertControl(ADockObject: TDragDockObject);
var
  NewSiteBounds: TRect;
  NewChildBounds: TRect;
  Child: TControl;
  ChildSite: TAnchorDockHostSite;
  SplitterWidth: Integer;
begin
  if DockSite<>nil then begin
    // handled by TAnchorDockHostSite
    //debugln(['TAnchorDockManager.InsertControl DockSite="',DockSite.Caption,'" Control=',DbgSName(ADockObject.Control),' InsertAt=',dbgs(ADockObject.DropAlign)])
  end else begin
    debugln(['TAnchorDockManager.InsertControl DockSite=nil Site="',DbgSName(Site),'" Control=',DbgSName(ADockObject.Control),' InsertAt=',dbgs(ADockObject.DropAlign),' Site.Bounds=',dbgs(Site.BoundsRect),' Control.Client=',dbgs(ADockObject.Control.ClientRect),' Parent=',DbgSName(ADockObject.Control.Parent)]);
    Site.DisableAutoSizing;
    try
      // align dragged Control
      Child:=ADockObject.Control;
      Child.Parent:=Site;
      Child.Align:=ADockObject.DropAlign;
      Child.Width:=ADockObject.DockRect.Right-ADockObject.DockRect.Left;
      Child.Height:=ADockObject.DockRect.Bottom-ADockObject.DockRect.Top;

      SplitterWidth:=0;
      ChildSite:=nil;
      if Child is TAnchorDockHostSite then begin
        ChildSite:=TAnchorDockHostSite(Child);
        ChildSite.CreateBoundSplitter;
        SplitterWidth:=DockMaster.SplitterWidth;
      end;

      // resize Site
      NewSiteBounds:=Site.BoundsRect;
      case ADockObject.DropAlign of
      alLeft: dec(NewSiteBounds.Left,Child.ClientWidth+SplitterWidth);
      alRight: dec(NewSiteBounds.Right,Child.ClientWidth+SplitterWidth);
      alTop: dec(NewSiteBounds.Top,Child.ClientHeight+SplitterWidth);
      alBottom: inc(NewSiteBounds.Bottom,Child.ClientHeight+SplitterWidth);
      end;
      if not StoredConstraintsValid then
        StoreConstraints;
      if ADockObject.DropAlign in [alLeft,alRight] then
        Site.Constraints.MaxWidth:=0
      else
        Site.Constraints.MaxHeight:=0;
      Site.BoundsRect:=NewSiteBounds;
      //debugln(['TAnchorDockManager.InsertControl Site.BoundsRect=',dbgs(Site.BoundsRect),' NewSiteBounds=',dbgs(NewSiteBounds),' Child.ClientRect=',dbgs(Child.ClientRect)]);
      FSiteClientRect:=Site.ClientRect;

      // resize child
      NewChildBounds:=Child.BoundsRect;
      case ADockObject.DropAlign of
      alTop: NewChildBounds:=Bounds(0,0,Site.ClientWidth,Child.ClientHeight);
      alBottom: NewChildBounds:=Bounds(0,Site.ClientHeight-Child.ClientHeight,
                                       Site.ClientWidth,Child.ClientHeight);
      alLeft: NewChildBounds:=Bounds(0,0,Child.ClientWidth,Site.ClientHeight);
      alRight: NewChildBounds:=Bounds(Site.ClientWidth-Child.ClientWidth,0,
                                      Child.ClientWidth,Site.ClientHeight);
      end;
      Child.BoundsRect:=NewChildBounds;

      if ChildSite<>nil then
        ChildSite.PositionBoundSplitter;

      // only allow to dock one control
      DragManager.RegisterDockSite(Site,false);
      debugln(['TAnchorDockManager.InsertControl AFTER Site="',DbgSName(Site),'" Control=',DbgSName(ADockObject.Control),' InsertAt=',dbgs(ADockObject.DropAlign),' Site.Bounds=',dbgs(Site.BoundsRect),' Control.ClientRect=',dbgs(ADockObject.Control.ClientRect)]);

    finally
      Site.EnableAutoSizing;
    end;
  end;
end;

procedure TAnchorDockManager.LoadFromStream(Stream: TStream);
begin
  debugln(['TAnchorDockManager.LoadFromStream not implemented Site="',DbgSName(Site),'"']);
  if Stream=nil then ;
end;

procedure TAnchorDockManager.PositionDockRect(Client, DropCtl: TControl;
  DropAlign: TAlign; var DockRect: TRect);
{ Client = dragged source site (a TAnchorDockHostSite)
  DropCtl is target control (the DockSite, DockSite.Pages or one of the pages)
  DropAlign: where on Client DropCtl should be placed
  DockRect: the estimated new bounds of DropCtl
}
var
  Offset: TPoint;
  Inside: Boolean;
begin
  if (DropAlign=alClient) and (DockSite<>nil) and (DockSite.Pages<>nil) then begin
    // dock into pages
    if DropCtl=DockSite.Pages then begin
      // dock as last page
      DockRect:=DockSite.Pages.TabRect(DockSite.Pages.PageCount-1);
      case DockSite.Pages.TabPosition of
      tpTop,tpBottom: DockRect.Left:=(DockRect.Left+DockRect.Right) div 2;
      tpLeft,tpRight: DockRect.Top:=(DockRect.Top+DockRect.Bottom) div 2;
      end;
      Offset:=DockSite.Pages.ClientOrigin;
      OffsetRect(DockRect,Offset.X,Offset.Y);
      exit;
    end else if DropCtl is TAnchorDockPage then begin
      // dock in front of page
      DockRect:=DockSite.Pages.TabRect(TAnchorDockPage(DropCtl).PageIndex);
      case DockSite.Pages.TabPosition of
      tpTop,tpBottom: DockRect.Right:=(DockRect.Left+DockRect.Right) div 2;
      tpLeft,tpRight: DockRect.Bottom:=(DockRect.Top+DockRect.Bottom) div 2;
      end;
      Offset:=DockSite.Pages.ClientOrigin;
      OffsetRect(DockRect,Offset.X,Offset.Y);
      exit;
    end;
  end;

  Inside:=(DropCtl=Site);
  if (not Inside) and (Site.Parent<>nil) then begin
    if (Site.Parent is TAnchorDockHostSite)
    or (not (Site.Parent.DockManager is TAnchorDockManager))
    or (Site.Parent.Parent<>nil) then
      Inside:=true;
  end;
  case DropAlign of
  alLeft:
    if Inside then
      DockRect:=Rect(0,0,Min(Client.Width,Site.ClientWidth div 2),Site.ClientHeight)
    else
      DockRect:=Rect(-Client.Width,0,0,Site.ClientHeight);
  alRight:
    if Inside then begin
      DockRect:=Rect(0,0,Min(Client.Width,Site.Width div 2),Site.ClientHeight);
      OffsetRect(DockRect,Site.ClientWidth-DockRect.Right,0);
    end else
      DockRect:=Bounds(Site.ClientWidth,0,Client.Width,Site.ClientHeight);
  alTop:
    if Inside then
      DockRect:=Rect(0,0,Site.ClientWidth,Min(Client.Height,Site.ClientHeight div 2))
    else
      DockRect:=Rect(0,-Client.Height,Site.ClientWidth,0);
  alBottom:
    if Inside then begin
      DockRect:=Rect(0,0,Site.ClientWidth,Min(Client.Height,Site.ClientHeight div 2));
      OffsetRect(DockRect,0,Site.ClientHeight-DockRect.Bottom);
    end else
      DockRect:=Bounds(0,Site.ClientHeight,Site.ClientWidth,Client.Height);
  alClient:
    begin
      // paged docking => show center
      if DockSite<>nil then
        DockRect:=DockSite.GetPageArea;
    end;
  else
    exit; // use default
  end;
  Offset:=Site.ClientOrigin;
  OffsetRect(DockRect,Offset.X,Offset.Y);
end;

procedure TAnchorDockManager.RemoveControl(Control: TControl);
var
  NewBounds: TRect;
  ChildSite: TAnchorDockHostSite;
  SplitterWidth: Integer;
begin
  if DockSite<>nil then
    debugln(['TAnchorDockManager.RemoveControl DockSite="',DockSite.Caption,'" Control=',DbgSName(Control)])
  else begin
    debugln(['TAnchorDockManager.RemoveControl Site="',DbgSName(Site),'" Control=',DbgSName(Control)]);
    if Control is TAnchorDockHostSite then begin
      SplitterWidth:=0;
      if Control is TAnchorDockHostSite then begin
        ChildSite:=TAnchorDockHostSite(Control);
        if ChildSite.BoundSplitter<>nil then
          SplitterWidth:=DockMaster.SplitterWidth;
      end;

      // shrink Site
      NewBounds:=Site.BoundsRect;
      case Control.Align of
      alTop: inc(NewBounds.Top,Control.Height+SplitterWidth);
      alBottom: dec(NewBounds.Bottom,Control.Height+SplitterWidth);
      alLeft: inc(NewBounds.Left,Control.Width+SplitterWidth);
      alRight: dec(NewBounds.Right,Control.Width+SplitterWidth);
      end;
      if StoredConstraintsValid then begin
        // restore constraints
        with Site.Constraints do begin
          MinWidth:=FStoredConstraints.Left;
          MinHeight:=FStoredConstraints.Top;
          MaxWidth:=FStoredConstraints.Right;
          MaxHeight:=FStoredConstraints.Bottom;
        end;
        FStoredConstraints:=Rect(0,0,0,0);
      end;
      Site.BoundsRect:=NewBounds;
      debugln(['TAnchorDockManager.RemoveControl Site=',DbgSName(Site),' ',dbgs(Site.BoundsRect)]);

      // Site can dock a control again
      DragManager.RegisterDockSite(Site,true);
    end;
  end;
end;

procedure TAnchorDockManager.ResetBounds(Force: Boolean);
var
  OldSiteClientRect: TRect;
  WidthDiff: Integer;
  HeightDiff: Integer;
  ClientRectChanged: Boolean;

  procedure AlignChilds;
  var
    i: Integer;
    b: TRect;
    AControl: TControl;
    ChildMaxSize: TPoint;
    SiteMinSize: TPoint;
    Child: TAnchorDockHostSite;
  begin
    if ClientRectChanged and DockMaster.Restoring then begin
      // ClientRect changed => restore bounds
      for i:=0 to Site.ControlCount-1 do begin
        AControl:=Site.Controls[i];
        b:=Rect(0,0,0,0);
        if AControl is TAnchorDockHostSite then
          b:=TAnchorDockHostSite(AControl).DockRestoreBounds
        else if AControl is TAnchorDockSplitter then
          b:=TAnchorDockSplitter(AControl).DockRestoreBounds;
        if (b.Right<=b.Left) or (b.Bottom<=b.Top) then
          b:=AControl.BoundsRect;
        {$IFDEF VerboseAnchorDockRestore}
        debugln(['TAnchorDockManager.ResetBounds RESTORE ',DbgSName(AControl),' Cur=',dbgs(AControl.BoundsRect),' Restore=',dbgs(b)]);
        {$ENDIF}
        if AControl is TAnchorDockSplitter then begin
          // fit splitter into clientarea
          if AControl.AnchorSide[akLeft].Control=nil then
            b.Left:=Max(0,Min(b.Left,Site.ClientWidth-10));
          if AControl.AnchorSide[akTop].Control=nil then
            b.Top:=Max(0,Min(b.Top,Site.ClientHeight-10));
          if TAnchorDockSplitter(AControl).ResizeAnchor in [akLeft,akRight] then
          begin
            b.Right:=b.Left+DockMaster.SplitterWidth;
            b.Bottom:=Max(1,Min(b.Bottom,Site.ClientHeight-b.Top));
          end
          else begin
            b.Right:=Max(1,Min(b.Right,Site.ClientWidth-b.Left));
            b.Bottom:=b.Top+DockMaster.SplitterWidth;
          end;
        end;

        AControl.BoundsRect:=b;
        if AControl is TAnchorDockSplitter then
          TAnchorDockSplitter(AControl).UpdateDockBounds;
      end;
      exit;
    end;

    if DockSite<>nil then exit;
    Child:=GetChildSite;
    if Child=nil then exit;

    {$IFDEF VerboseAnchorDockRestore}
    debugln(['TAnchorDockManager.ResetBounds ',DbgSName(Site),' ',dbgs(Child.BaseBounds),' ',WidthDiff,',',HeightDiff]);
    {$ENDIF}
    ChildMaxSize:=Point(Site.ClientWidth-DockMaster.SplitterWidth,
                        Site.ClientHeight-DockMaster.SplitterWidth);
    if PreferredSiteSizeAsSiteMinimum then begin
      SiteMinSize:=GetSitePreferredClientSize;
      if Child.Align in [alLeft,alRight] then begin
        ChildMaxSize.X:=Max(0,(ChildMaxSize.X-SiteMinSize.X));
      end else begin
        ChildMaxSize.Y:=Max(0,(ChildMaxSize.Y-SiteMinSize.Y));
      end;
      debugln(['TAnchorDockManager.ResetBounds ChildMaxSize=',dbgs(ChildMaxSize),' SiteMinSize=',dbgs(SiteMinSize),' Site.Client=',dbgs(Site.ClientRect)]);
    end;

    case ResizePolicy of
    admrpChild:
      begin
        if Child.Align in [alLeft,alRight] then
          Child.Width:=Max(1,Min(ChildMaxSize.X,Child.Width+WidthDiff))
        else begin
          i:=Max(1,Min(ChildMaxSize.Y,Child.Height+HeightDiff));
          {$IFDEF VerboseAnchorDockRestore}
          debugln(['TAnchorDockManager.ResetBounds Child=',DbgSName(Child),' OldHeight=',Child.Height,' NewHeight=',i]);
          {$ENDIF}
          Child.Height:=i;
        end;
      end;
    end;
  end;

begin
  if Force then ;

  //debugln(['TAnchorDockManager.ResetBounds Site="',Site.Caption,'" Force=',Force,' ',dbgs(Site.ClientRect)]);
  OldSiteClientRect:=FSiteClientRect;
  FSiteClientRect:=Site.ClientRect;
  WidthDiff:=FSiteClientRect.Right-OldSiteClientRect.Right;
  HeightDiff:=FSiteClientRect.Bottom-OldSiteClientRect.Bottom;
  ClientRectChanged:=(WidthDiff<>0) or (HeightDiff<>0);
  if ClientRectChanged or PreferredSiteSizeAsSiteMinimum then
    AlignChilds;
end;

procedure TAnchorDockManager.SaveToStream(Stream: TStream);
begin
  if Stream=nil then ;
  debugln(['TAnchorDockManager.SaveToStream not implemented Site="',DbgSName(Site),'"']);
end;

function TAnchorDockManager.GetDockEdge(ADockObject: TDragDockObject): boolean;
var
  BestDistance: Integer;

  procedure FindMinDistance(CurAlign: TAlign; CurDistance: integer);
  begin
    if CurDistance<0 then
      CurDistance:=-CurDistance;
    if CurDistance>=BestDistance then exit;
    ADockObject.DropAlign:=CurAlign;
    BestDistance:=CurDistance;
  end;

var
  p: TPoint;
  LastTabRect: TRect;
  TabIndex: longint;
begin
  //debugln(['TAnchorDockManager.GetDockEdge ',DbgSName(Site),' ',DbgSName(DockSite),' DockableSites=',dbgs(DockableSites)]);
  if DockableSites=[] then begin
    ADockObject.DropAlign:=alNone;
    exit(false);
  end;

  p:=Site.ScreenToClient(ADockObject.DragPos);
  //debugln(['TAnchorDockManager.GetDockEdge ',dbgs(p),' ',dbgs(Site.BoundsRect),' ',DbgSName(Site)]);
  if (DockSite<>nil) and (DockSite.Pages<>nil) then begin
    // page docking
    ADockObject.DropAlign:=alClient;
    p:=DockSite.Pages.ScreenToClient(ADockObject.DragPos);
    LastTabRect:=DockSite.Pages.TabRect(DockSite.Pages.PageCount-1);
    if (p.Y>=LastTabRect.Top) and (p.y<LastTabRect.Bottom) then begin
      // specific tab
      if p.X>=LastTabRect.Right then begin
        // insert as last
        ADockObject.DropOnControl:=DockSite.Pages;
      end else begin
        TabIndex:=DockSite.Pages.TabIndexAtClientPos(p);
        if TabIndex>=0 then begin
          // insert in front of an existing
          ADockObject.DropOnControl:=DockSite.Pages.Page[TabIndex];
        end;
      end;
    end;
  end else if (DockSite<>nil) and PtInRect(DockSite.GetPageArea,p) then begin
    // page docking
    ADockObject.DropAlign:=alClient;
  end else begin

    // check side
    BestDistance:=High(Integer);
    if akLeft in DockableSites then FindMinDistance(alLeft,p.X);
    if akRight in DockableSites then FindMinDistance(alRight,Site.ClientWidth-p.X);
    if akTop in DockableSites then FindMinDistance(alTop,p.Y);
    if akBottom in DockableSites then FindMinDistance(alBottom,Site.ClientHeight-p.Y);

    // check inside
    if InsideDockingAllowed
    and ( ((ADockObject.DropAlign=alLeft) and (p.X>=0))
       or ((ADockObject.DropAlign=alTop) and (p.Y>=0))
       or ((ADockObject.DropAlign=alRight) and (p.X<Site.ClientWidth))
       or ((ADockObject.DropAlign=alBottom) and (p.Y<Site.ClientHeight)) )
    then
      ADockObject.DropOnControl:=Site
    else
      ADockObject.DropOnControl:=nil;
  end;
  //debugln(['TAnchorDockManager.GetDockEdge ADockObject.DropAlign=',dbgs(ADockObject.DropAlign),' DropOnControl=',DbgSName(ADockObject.DropOnControl)]);
  Result:=true;
end;

procedure TAnchorDockManager.RestoreSite(SplitterPos: integer);
var
  ChildSite: TAnchorDockHostSite;
begin
  FSiteClientRect:=Site.ClientRect;
  if DockSite<>nil then exit;
  ChildSite:=GetChildSite;
  {$IFDEF VerboseAnchorDockRestore}
  debugln(['TAnchorDockManager.RestoreSite START ',DbgSName(Site),' ChildSite=',DbgSName(ChildSite)]);
  {$ENDIF}
  if ChildSite<>nil then begin
    ChildSite.CreateBoundSplitter;
    ChildSite.PositionBoundSplitter;
    if ChildSite.Align in [alLeft,alRight] then
      ChildSite.BoundSplitter.Left:=SplitterPos
    else
      ChildSite.BoundSplitter.Top:=SplitterPos;
    case ChildSite.Align of
    alTop: ChildSite.Height:=ChildSite.BoundSplitter.Top;
    alBottom: ChildSite.Height:=Site.ClientHeight
                  -(ChildSite.BoundSplitter.Top+ChildSite.BoundSplitter.Height);
    alLeft: ChildSite.Width:=ChildSite.BoundSplitter.Left;
    alRight: ChildSite.Width:=Site.ClientWidth
                  -(ChildSite.BoundSplitter.Left+ChildSite.BoundSplitter.Width);
    end;
    // only allow to dock one control
    DragManager.RegisterDockSite(Site,false);
    {$IFDEF VerboseAnchorDockRestore}
    debugln(['TAnchorDockManager.RestoreSite ',DbgSName(Site),' ChildSite=',DbgSName(ChildSite),' Site.Bounds=',dbgs(Site.BoundsRect),' Site.Client=',dbgs(Site.ClientRect),' ChildSite.Bounds=',dbgs(ChildSite.BoundsRect),' Splitter.Bounds=',dbgs(ChildSite.BoundSplitter.BoundsRect)]);
    {$ENDIF}
  end;
end;

procedure TAnchorDockManager.StoreConstraints;
begin
  with Site.Constraints do
    FStoredConstraints:=Rect(MinWidth,MinHeight,MaxWidth,MaxHeight);
end;

function TAnchorDockManager.GetSitePreferredClientSize: TPoint;
{ Compute the preferred inner size of Site without the ChildSite and without
  the splitter
}
var
  ChildSite: TAnchorDockHostSite;
  ChildSitePrefSize: TPoint;
  SplitterSize: TPoint;
begin
  Result:=Point(0,0);
  Site.GetPreferredSize(Result.X,Result.Y);
  ChildSite:=GetChildSite;
  if ChildSite<>nil then begin
    // subtract the ChildSite and the splitter
    SplitterSize:=Point(0,0);
    if ChildSite.BoundSplitter<>nil then
      ChildSite.BoundSplitter.GetPreferredSize(SplitterSize.X,SplitterSize.Y);
    ChildSitePrefSize:=Point(ChildSite.Width,ChildSite.Height);
    debugln(['TAnchorDockManager.GetSitePreferredClientSize Total=',dbgs(Result),' Child=',dbgs(ChildSitePrefSize),' Splitter=',dbgs(SplitterSize)]);
    if ChildSite.Align in [alLeft,alRight] then begin
      Result.X:=Max(0,Result.X-ChildSitePrefSize.X-SplitterSize.X);
    end else begin
      Result.Y:=Max(0,Result.Y-ChildSitePrefSize.Y-SplitterSize.Y);
    end;
  end;
end;

function TAnchorDockManager.GetChildSite: TAnchorDockHostSite;
var
  i: Integer;
begin
  for i:=0 to Site.ControlCount-1 do
    if Site.Controls[i] is TAnchorDockHostSite then begin
      Result:=TAnchorDockHostSite(Site.Controls[i]);
      exit;
    end;
  Result:=nil;
end;

function TAnchorDockManager.StoredConstraintsValid: boolean;
begin
  with FStoredConstraints do
    Result:=(Left<>0) or (Top<>0) or (Right<>0) or (Bottom<>0);
end;

{ TAnchorDockSplitter }

procedure TAnchorDockSplitter.SetResizeAnchor(const AValue: TAnchorKind);
begin
  inherited SetResizeAnchor(AValue);

  case ResizeAnchor of
  akLeft: Anchors:=AnchorAlign[alLeft];
  akTop: Anchors:=AnchorAlign[alTop];
  akRight: Anchors:=AnchorAlign[alRight];
  akBottom: Anchors:=AnchorAlign[alBottom];
  end;
  //debugln(['TAnchorDockSplitter.SetResizeAnchor ',DbgSName(Self),' ResizeAnchor=',dbgs(ResizeAnchor),' Align=',dbgs(Align),' Anchors=',dbgs(Anchors)]);
end;

procedure TAnchorDockSplitter.UpdateDockBounds;
begin
  FDockBounds:=BoundsRect;
  if Parent<>nil then begin
    FDockParentClientSize.cx:=Parent.ClientWidth;
    FDockParentClientSize.cy:=Parent.ClientHeight;
  end else begin
    FDockParentClientSize.cx:=0;
    FDockParentClientSize.cy:=0;
  end;
end;

procedure TAnchorDockSplitter.SetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  DisableAutoSizing;
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  UpdateDockBounds;
  EnableAutoSizing;
end;

procedure TAnchorDockSplitter.SetBoundsKeepDockBounds(ALeft, ATop, AWidth,
  AHeight: integer);
begin
  inherited SetBounds(ALeft,ATop,AWidth,AHeight);
end;

function TAnchorDockSplitter.SideAnchoredControlCount(Side: TAnchorKind
  ): integer;
var
  Sibling: TControl;
  i: Integer;
begin
  Result:=0;
  for i:=0 to AnchoredControlCount-1 do begin
    Sibling:=AnchoredControls[i];
    if Sibling.AnchorSide[OppositeAnchor[Side]].Control=Self then
      inc(Result);
  end;
end;

procedure TAnchorDockSplitter.SaveLayout(
  LayoutNode: TAnchorDockLayoutTreeNode);
begin
  if ResizeAnchor in [akLeft,akRight] then
    LayoutNode.NodeType:=adltnSplitterVertical
  else
    LayoutNode.NodeType:=adltnSplitterHorizontal;
  LayoutNode.Assign(Self);
end;

function TAnchorDockSplitter.HasOnlyOneSibling(Side: TAnchorKind; MinPos,
  MaxPos: integer): TControl;
var
  i: Integer;
  AControl: TControl;
begin
  Result:=nil;
  for i:=0 to AnchoredControlCount-1 do begin
    AControl:=AnchoredControls[i];
    if AControl.AnchorSide[OppositeAnchor[Side]].Control<>Self then continue;
    // AControl is anchored at Side to this splitter
    if (Side in [akLeft,akRight]) then begin
      if (AControl.Left>MaxPos) or (AControl.Left+AControl.Width<MinPos) then
        continue;
    end else begin
      if (AControl.Top>MaxPos) or (AControl.Top+AControl.Height<MinPos) then
        continue;
    end;
    // AControl is in range
    if Result=nil then
      Result:=AControl
    else begin
      // there is more than one control
      Result:=nil;
      exit;
    end;
  end;
end;

constructor TAnchorDockSplitter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Align:=alNone;
  ResizeAnchor:=akLeft;
  // make sure the splitter never vanish
  Constraints.MinWidth:=2;
  Constraints.MinHeight:=2;
end;

{ TAnchorDockPageControl }

function TAnchorDockPageControl.GetDockPages(Index: integer): TAnchorDockPage;
begin
  Result:=TAnchorDockPage(Page[Index]);
end;

procedure TAnchorDockPageControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ATabIndex: LongInt;
  APage: TCustomPage;
  Site: TAnchorDockHostSite;
begin
  inherited MouseDown(Button, Shift, X, Y);
  ATabIndex := TabIndexAtClientPos(Point(X,Y));
  if (Button = mbLeft) and DockMaster.AllowDragging and (ATabIndex >= 0) then
  begin
    APage:=Page[ATabIndex];
    if (APage.ControlCount>0) and (APage.Controls[0] is TAnchorDockHostSite) then
    begin
      Site:=TAnchorDockHostSite(APage.Controls[0]);
      DragManager.DragStart(Site,false,DockMaster.DragTreshold);
    end;
  end;
end;

procedure TAnchorDockPageControl.PopupMenuPopup(Sender: TObject);
var
  ContainsMainForm: Boolean;
  s: String;
  TabPositionSection: TMenuItem;
  Item: TMenuItem;
  tp: TTabPosition;
begin
  // movement
  if PageIndex>0 then
    DockMaster.AddPopupMenuItem('MoveLeftMenuItem', adrsMovePageLeft,
                                              @MoveLeftButtonClick);
  if PageIndex>1 then
    DockMaster.AddPopupMenuItem('MoveLeftMostMenuItem', adrsMovePageLeftmost,
                                              @MoveLeftMostButtonClick);

  if PageIndex<PageCount-1 then
    DockMaster.AddPopupMenuItem('MoveRightMenuItem', adrsMovePageRight,
                                              @MoveRightButtonClick);
  if PageIndex<PageCount-2 then
    DockMaster.AddPopupMenuItem('MoveRightMostMenuItem', adrsMovePageRightmost,
                                              @MoveRightMostButtonClick);

  // tab position
  TabPositionSection:=DockMaster.AddPopupMenuItem('TabPositionMenuItem',
                                                  adrsTabPosition,nil);
  for tp:=Low(TTabPosition) to high(TTabPosition) do begin
    case tp of
    tpTop: s:=adrsTop;
    tpBottom: s:=adrsBottom;
    tpLeft: s:=adrsLeft;
    tpRight: s:=adrsRight;
    end;
    Item:=DockMaster.AddPopupMenuItem('TabPos'+ADLTabPostionNames[tp]+'MenuItem',
                              s,@TabPositionClick,TabPositionSection);
    Item.ShowAlwaysCheckable:=true;
    Item.Checked:=TabPosition=tp;
    Item.Tag:=ord(tp);
  end;

  // close
  ContainsMainForm:=IsParentOf(Application.MainForm);
  if ContainsMainForm then
    s:=Format(adrsQuit, [Application.Title])
  else
    s:=adrsClose;
  DockMaster.AddPopupMenuItem('CloseMenuItem',s,@CloseButtonClick);
end;

procedure TAnchorDockPageControl.CloseButtonClick(Sender: TObject);
var
  Site: TAnchorDockHostSite;
begin
  Site:=GetActiveSite;
  if Site=nil then exit;
  DockMaster.RestoreLayouts.Add(DockMaster.CreateRestoreLayout(Site),true);
  Site.CloseSite;
  DockMaster.SimplifyPendingLayouts;
end;

procedure TAnchorDockPageControl.MoveLeftButtonClick(Sender: TObject);
begin
  if PageIndex>0 then
    Page[PageIndex].PageIndex:=Page[PageIndex].PageIndex-1;
end;

procedure TAnchorDockPageControl.MoveLeftMostButtonClick(Sender: TObject);
begin
  if PageIndex>0 then
    Page[PageIndex].PageIndex:=0;
end;

procedure TAnchorDockPageControl.MoveRightButtonClick(Sender: TObject);
begin
  if PageIndex<PageCount-1 then
    Page[PageIndex].PageIndex:=Page[PageIndex].PageIndex+1;
end;

procedure TAnchorDockPageControl.MoveRightMostButtonClick(Sender: TObject);
begin
  if PageIndex<PageCount-1 then
    Page[PageIndex].PageIndex:=PageCount-1;
end;

procedure TAnchorDockPageControl.TabPositionClick(Sender: TObject);
var
  Item: TMenuItem;
begin
  if not (Sender is TMenuItem) then exit;
  Item:=TMenuItem(Sender);
  TabPosition:=TTabPosition(Item.Tag);
end;

procedure TAnchorDockPageControl.UpdateDockCaption(Exclude: TControl);
begin
  if Exclude=nil then ;
end;

procedure TAnchorDockPageControl.RemoveControl(AControl: TControl);
begin
  inherited RemoveControl(AControl);
  if (not (csDestroying in ComponentState)) then begin
    if (PageCount<=1) and (Parent is TAnchorDockHostSite) then
      DockMaster.NeedSimplify(Parent);
  end;
end;

function TAnchorDockPageControl.GetActiveSite: TAnchorDockHostSite;
var
  CurPage: TCustomPage;
  CurDockPage: TAnchorDockPage;
begin
  Result:=nil;
  CurPage:=ActivePageComponent;
  if not (CurPage is TAnchorDockPage) then exit;
  CurDockPage:=TAnchorDockPage(CurPage);
  Result:=CurDockPage.GetSite;
end;

constructor TAnchorDockPageControl.Create(TheOwner: TComponent);
begin
  PageClass:=DockMaster.PageClass;
  inherited Create(TheOwner);
  PopupMenu:=DockMaster.GetPopupMenu;
end;

{ TAnchorDockPage }

procedure TAnchorDockPage.UpdateDockCaption(Exclude: TControl);
var
  i: Integer;
  Child: TControl;
  NewCaption: String;
begin
  NewCaption:='';
  for i:=0 to ControlCount-1 do begin
    Child:=Controls[i];
    if Child=Exclude then continue;
    if not (Child is TAnchorDockHostSite) then continue;
    if NewCaption<>'' then
      NewCaption:=NewCaption+',';
    NewCaption:=NewCaption+Child.Caption;
  end;
  //debugln(['TAnchorDockPage.UpdateDockCaption ',Caption,' ',NewCaption]);
  if Caption=NewCaption then exit;
  Caption:=NewCaption;
  if Parent is TAnchorDockPageControl then
    TAnchorDockPageControl(Parent).UpdateDockCaption;
end;

procedure TAnchorDockPage.InsertControl(AControl: TControl; Index: integer);
begin
  inherited InsertControl(AControl, Index);
  //debugln(['TAnchorDockPage.InsertControl ',DbgSName(AControl)]);
  if AControl is TAnchorDockHostSite then begin
    if TAnchorDockHostSite(AControl).Header<>nil then
      TAnchorDockHostSite(AControl).Header.Parent:=nil;
    UpdateDockCaption;
  end;
end;

procedure TAnchorDockPage.RemoveControl(AControl: TControl);
begin
  inherited RemoveControl(AControl);
  if (GetSite=nil) and (not (csDestroying in ComponentState))
  and (Parent<>nil) and (not (csDestroying in Parent.ComponentState)) then
    DockMaster.NeedSimplify(Self);
end;

function TAnchorDockPage.GetSite: TAnchorDockHostSite;
begin
  Result:=nil;
  if ControlCount=0 then exit;
  if not (Controls[0] is TAnchorDockHostSite) then exit;
  Result:=TAnchorDockHostSite(Controls[0]);
end;

initialization
  DockMaster:=TAnchorDockMaster.Create(nil);

finalization
  FreeAndNil(DockMaster);

end.

