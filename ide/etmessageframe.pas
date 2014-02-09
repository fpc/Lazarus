{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Frame for messages - output lines for (compiler) messages.
}
unit etMessageFrame;

{$mode objfpc}{$H+}

{$I ide.inc}

interface

uses
  Math, strutils, Classes, SysUtils, UTF8Process, FileProcs, LazFileCache,
  LazUTF8Classes, LazFileUtils, LazUTF8, AvgLvlTree, SynEdit,
  LResources, Forms, Buttons, ExtCtrls, Controls, LMessages, LCLType, Graphics,
  LCLIntf, Themes, ImgList, GraphType, Menus, Clipbrd, Dialogs, StdCtrls,
  IDEExternToolIntf, IDEImagesIntf, MenuIntf, etSrcEditMarks, etQuickFixes;

const
  CustomViewCaption = '------------------------------';
type
  TLMsgViewFilter = class;

  { TLMVFilterHideMsgType - read/write by main, read by worker thread }

  TLMVFilterHideMsgType = class
  private
    FFilter: TLMsgViewFilter;
    FIndex: integer;
    FMsgID: integer;
    FSubTool: string;
    procedure SetMsgID(AValue: integer);
    procedure SetSubTool(AValue: string);
    procedure Changed;
    procedure InternalAssign(Src: TLMVFilterHideMsgType);
  public
    constructor Create(aFilter: TLMsgViewFilter);
    function IsEqual(Src: TLMVFilterHideMsgType): boolean;
    procedure Assign(Src: TLMVFilterHideMsgType);
    property Filter: TLMsgViewFilter read FFilter;
    property SubTool: string read FSubTool write SetSubTool;
    property MsgID: integer read FMsgID write SetMsgID;
    property Index: integer read FIndex;
  end;

  { TLMsgViewFilter - read/write by main thread, read by worker thread }

  TLMsgViewFilter = class
  private
    FCaption: string;
    FHideNotesWithoutPos: boolean;
    FMinUrgency: TMessageLineUrgency;
    FOnChanged: TNotifyEvent;
    fHideMsgTypes: array of TLMVFilterHideMsgType; // sorted for SubTool, MsgID
    function GetHideMsgTypes(Index: integer): TLMVFilterHideMsgType; inline;
    procedure SetCaption(AValue: string);
    procedure SetHideNotesWithoutPos(AValue: boolean);
    procedure SetMinUrgency(AValue: TMessageLineUrgency);
    procedure Changed;
    procedure UpdateHideMsgTypeIndex(Item: TLMVFilterHideMsgType);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure SetToFitsAll;
    function IsEqual(Src: TLMsgViewFilter): boolean; // does not check Caption
    procedure Assign(Src: TLMsgViewFilter); // does not copy Caption
    function LineFits(Line: TMessageLine): boolean; virtual;
    property Caption: string read FCaption write SetCaption;
    property MinUrgency: TMessageLineUrgency read FMinUrgency write SetMinUrgency;
    property HideNotesWithoutPos: boolean read FHideNotesWithoutPos write SetHideNotesWithoutPos;
    function HideMsgTypeCount: integer; inline;
    property HideMsgTypes[Index: integer]: TLMVFilterHideMsgType read GetHideMsgTypes;
    function AddHideMsgType(SubTool: string; MsgID: integer): TLMVFilterHideMsgType;
    procedure DeleteHideMsgType(Index: integer);
    procedure ClearHideMsgTypes;
    function IndexOfHideMsgType(Line: TMessageLine): integer;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    procedure ConsistencyCheck;
  end;


const
  MCDefaultBackground = clWindow;
  MCDefaultHeaderBackgroundRunning = TColor($F0F080);
  MCDefaultHeaderBackgroundSuccess = TColor($A0F0A0);
  MCDefaultHeaderBackgroundFailed = TColor($A0A0F0);
  MCDefaultAutoHeaderBackground = TColor($FFC0A0);

type
  TMessagesCtrl = class;

  TLMVToolState = (
    lmvtsRunning,
    lmvtsSuccess,
    lmvtsFailed
    );
  TLMVToolStates = set of TLMVToolState;

  { TLMsgWndView }

  TLMsgWndView = class(TExtToolView)
  private
    FControl: TMessagesCtrl;
    FFilter: TLMsgViewFilter;
    FPendingChanges: TETMultiSrcChanges;
    FToolState: TLMVToolState;
    procedure SetFilter(AValue: TLMsgViewFilter);
    procedure OnMarksFixed(ListOfTMessageLine: TFPList); // (main thread) called after mlfFixed was added to these messages
    procedure SetToolState(AValue: TLMVToolState);
  protected
    FAsyncQueued: boolean;
    FPaintStamp: int64;
    fPaintTop: integer; // only valid if FPaintStamp=Control.FPaintStamp
    fPaintBottom: integer; // only valid if FPaintStamp=Control.FPaintStamp
    procedure CallOnChangedInMainThread({%H-}Data: PtrInt); // (main thread)
    procedure FetchAllPending; override; // (main thread)
    procedure QueueAsyncOnChanged; override; // (worker thread)
    procedure RemoveAsyncOnChanged; override; // (worker thread)
    procedure ToolExited; override; // (main thread)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function LineFits(Line: TMessageLine): boolean; override;
    property Control: TMessagesCtrl read FControl;
    function HasContent: boolean;
    function GetShownLineCount(WithHeader, WithProgressLine: boolean): integer;
    procedure RebuildLines; // (main thread)
    function ApplySrcChanges(Changes: TETSrcChanges): boolean; // true if something changed
    property ToolState: TLMVToolState read FToolState write SetToolState;
  public
    // requires Enter/LeaveCriticalSection, write only via main thread
    property Filter: TLMsgViewFilter read FFilter write SetFilter;
    property PendingChanges: TETMultiSrcChanges read FPendingChanges;// src changes for messages adding to view
  end;

  { TMsgCtrlUrgencyStyle }

  TMsgCtrlUrgencyStyle = class
  private
    FColor: TColor;
    FControl: TMessagesCtrl;
    FImageIndex: integer;
    FTranslated: string;
    FUrgency: TMessageLineUrgency;
    procedure SetColor(AValue: TColor);
    procedure SetImageIndex(AValue: integer);
    procedure SetTranslated(AValue: string);
    procedure Changed;
  public
    constructor Create(AControl: TMessagesCtrl; TheUrgency: TMessageLineUrgency);
    function Equals(Obj: TObject): boolean; override;
    procedure Assign(Src: TMsgCtrlUrgencyStyle);
    procedure SetValues(TheTranslated: string; TheImageIndex: integer = -1; TheColor: TColor = clDefault);
    property Control: TMessagesCtrl read FControl;
    property Urgency: TMessageLineUrgency read FUrgency;
    property Translated: string read FTranslated write SetTranslated;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Color: TColor read FColor write SetColor default clDefault;
  end;

  TOnOpenMessageLine = function(Sender: TObject; Msg: TMessageLine): boolean of object;

  TMsgCtrlState = (
    mcsFocused
    );
  TMsgCtrlStates = set of TMsgCtrlState;

  TMsgCtrlOption = (
    mcoSingleClickOpensFile, // otherwise double click
    mcoShowStats, // show numbers of errors, warnings and hints in view header line
    mcoShowTranslated, // show translation (e.g. messages from German message file)
    mcoShowMessageID,  // show message ID
    mcoAutoOpenFirstError, // when all views stopped, open first error
    mcoShowMsgIcons
    );
  TMsgCtrlOptions = set of TMsgCtrlOption;
const
  MCDefaultOptions = [mcoShowStats,mcoShowTranslated,mcoAutoOpenFirstError,mcoShowMsgIcons];

type

  TMsgCtrlFileNameStyle = (
    mcfsShort,   // = ExtractFilename
    mcfsRelative, // = CreateRelativePath
    mcfsFull
    );
  TMsgCtrlFileNameStyles = set of TMsgCtrlFileNameStyle;

  { TMessagesCtrl }

  TMessagesCtrl = class(TCustomControl)
  private
    FActiveFilter: TLMsgViewFilter;
    FAutoScrollToNewMessage: boolean;
    FBackgroundColor: TColor;
    FFilenameStyle: TMsgCtrlFileNameStyle;
    FFilters: TFPList; // list of TLMsgViewFilter
    FHeaderBackground: array[TLMVToolState] of TColor;
    FIdleConnected: boolean;
    FImageChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FItemHeight: integer;
    FOnAllViewsStopped: TNotifyEvent;
    FOnOpenMessage: TOnOpenMessageLine;
    FOptions: TMsgCtrlOptions;
    FScrollLeft: integer;
    FScrollTop: integer;
    fScrollTopMax: integer;
    FSearchText: string;
    FSelectedLine: integer;
    FSelectedView: TLMsgWndView;
    FSourceMarks: TETMarks;
    fUpdateLock: integer;
    FUpdateTimer: TTimer;
    fSomeViewsRunning: boolean;
    fUrgencyStyles: array[TMessageLineUrgency] of TMsgCtrlUrgencyStyle;
    FAutoHeaderBackground: TColor;
    procedure CreateSourceMark(MsgLine: TMessageLine; aSynEdit: TSynEdit);
    procedure CreateSourceMarks(View: TLMsgWndView; StartLineNumber: Integer);
    function GetFilters(Index: integer): TLMsgViewFilter;
    function GetHeaderBackground(aToolState: TLMVToolState): TColor;
    function GetSelectedLine: integer;
    function GetUrgencyStyles(Urgency: TMessageLineUrgency
      ): TMsgCtrlUrgencyStyle;
    function GetViews(Index: integer): TLMsgWndView;
    procedure OnViewChanged(Sender: TObject); // (main thread)
    procedure MsgUpdateTimerTimer(Sender: TObject);
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetFilenameStyle(AValue: TMsgCtrlFileNameStyle);
    procedure SetActiveFilter(AValue: TLMsgViewFilter);
    procedure SetHeaderBackground(aToolState: TLMVToolState; AValue: TColor);
    procedure SetIdleConnected(AValue: boolean);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetItemHeight(AValue: integer);
    procedure SetOptions(NewOptions: TMsgCtrlOptions);
    procedure SetScrollLeft(AValue: integer);
    procedure SetScrollTop(AValue: integer);
    procedure SetSearchText(AValue: string);
    procedure SetSelectedLine(AValue: integer);
    procedure SetSelectedView(AValue: TLMsgWndView);
    procedure SetSourceMarks(AValue: TETMarks);
    procedure SetUrgencyStyles(Urgency: TMessageLineUrgency;
      AValue: TMsgCtrlUrgencyStyle);
    procedure SetAutoHeaderBackground(AValue: TColor);
    procedure WMHScroll(var Msg: TLMScroll); message LM_HSCROLL;
    procedure WMVScroll(var Msg: TLMScroll); message LM_VSCROLL;
    procedure WMMouseWheel(var Message: TLMMouseEvent); message LM_MOUSEWHEEL;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    function GetMaxSelectedLine: integer;
    procedure ImageListChange(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure OnFilterChanged(Sender: TObject);
  protected
    FViews: TFPList;// list of TMessagesViewMap
    FStates: TMsgCtrlStates;
    FPaintStamp: int64;
    fLastSearchStartView: TLMsgWndView;
    fLastSearchStartLine: integer;
    fLastLoSearchText: string; // lower case search text
    procedure FetchNewMessages;
    procedure FetchNewMessages(View: TLMsgWndView);
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
    procedure Paint; override;
    procedure UpdateScrollBar(InvalidateScrollMax: boolean);
    procedure CreateWnd; override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer
      ); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function GetDefaultFilterCaption: string;
    procedure DoOnShowHint(HintInfo: PHintInfo); override;
    procedure DoAllViewsStopped;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure EraseBackground({%H-}DC: HDC); override;

    // views
    function ViewCount: integer; inline;
    property Views[Index: integer]: TLMsgWndView read GetViews;
    function IndexOfView(View: TLMsgWndView): integer;
    procedure ClearViews; // deletes/frees all views
    procedure RemoveView(View: TLMsgWndView); // remove without free
    function GetView(aCaption: string; CreateIfNotExist: boolean): TLMsgWndView;
    function GetLineAt(Y: integer; out View: TLMsgWndView; out Line: integer): boolean;
    function GetLineText(Line: TMessageLine): string;
    function GetHeaderText(View: TLMsgWndView): string;
    function FindUnfinishedView: TLMsgWndView; // running or waiting for run

    // filter
    property ActiveFilter: TLMsgViewFilter read FActiveFilter write SetActiveFilter;
    function FilterCount: integer; inline;
    property Filters[Index: integer]: TLMsgViewFilter read GetFilters;
    function GetFilter(aCaption: string; CreateIfNotExist: boolean): TLMsgViewFilter;
    procedure DeleteFilter(Index: integer);
    procedure ClearFilters;

    // select, search
    function HasSelection: boolean;
    procedure Select(View: TLMsgWndView; LineNumber: integer; DoScroll, FullyVisible: boolean);
    function SearchNext(StartView: TLMsgWndView; StartLine: integer;
      SkipStart, Downwards: boolean;
      out View: TLMsgWndView; out LineNumber: integer): boolean;
    procedure Select(Msg: TMessageLine; DoScroll: boolean);
    function SelectNextOccurence(Downwards: boolean): boolean;
    function SelectNextShown(Offset: integer): boolean;
    function SelectLast(DoScroll, FullyVisible: boolean): boolean;
    function SelectFirst(DoScroll, FullyVisible: boolean): boolean;
    function GetSelectedMsg: TMessageLine;
    function SearchNextUrgent(StartView: TLMsgWndView; StartLine: integer;
      SkipStart, Downwards: boolean;
      aMinUrgency: TMessageLineUrgency; WithSrcPos: boolean;
      out View: TLMsgWndView; out LineNumber: integer): boolean;
    function SelectFirstUrgentMessage(aMinUrgency: TMessageLineUrgency;
      WithSrcPos: boolean): boolean;
    function SelectNextUrgentMessage(aMinUrgency: TMessageLineUrgency;
      WithSrcPos: boolean; Downwards: boolean): boolean;

    // scroll
    function IsLineVisible(View: TLMsgWndView; LineNumber: integer): boolean;
    function IsLastLineVisible(View: TLMsgWndView): boolean;
    procedure ScrollToLine(View: TLMsgWndView; LineNumber: integer; FullyVisible: boolean);
    function GetLineTop(View: TLMsgWndView; LineNumber: integer; Scrolled: boolean): integer;
    property ScrollLeft: integer read FScrollLeft write SetScrollLeft;
    property ScrollTop: integer read FScrollTop write SetScrollTop;
    function ScrollLeftMax: integer;
    function ScrollTopMax: integer;
    procedure StoreSelectedAsSearchStart;
    property AutoScrollToNewMessage: boolean read FAutoScrollToNewMessage write FAutoScrollToNewMessage;

    // file
    function OpenSelection: boolean;
    procedure CreateMarksForFile(aSynEdit: TSynEdit; aFilename: string; DeleteOld: boolean);
    function ApplySrcChanges(Changes: TETSrcChanges): boolean; // true if something changed
  public
    // properties
    property ItemHeight: integer read FItemHeight write SetItemHeight;
    property Color default clWindow;
    property SelectedView: TLMsgWndView read FSelectedView write SetSelectedView;
    property SelectedLine: integer read GetSelectedLine write SetSelectedLine; // -1=header line
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default MCDefaultBackground;
    property HeaderBackground[aToolState: TLMVToolState]: TColor read GetHeaderBackground write SetHeaderBackground;
    property AutoHeaderBackground: TColor read FAutoHeaderBackground write SetAutoHeaderBackground default MCDefaultAutoHeaderBackground;
    property Images: TCustomImageList read FImages write SetImages;
    property UrgencyStyles[Urgency: TMessageLineUrgency]: TMsgCtrlUrgencyStyle read GetUrgencyStyles write SetUrgencyStyles;
    property SearchText: string read FSearchText write SetSearchText;
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
    property Options: TMsgCtrlOptions read FOptions write SetOptions default MCDefaultOptions;
    property FilenameStyle: TMsgCtrlFileNameStyle read FFilenameStyle write SetFilenameStyle;
    property SourceMarks: TETMarks read FSourceMarks write SetSourceMarks;
    property ShowHint default true;
    property OnAllViewsStopped: TNotifyEvent read FOnAllViewsStopped write FOnAllViewsStopped;
    property OnOpenMessage: TOnOpenMessageLine read FOnOpenMessage write FOnOpenMessage;
  end;

  { TMessagesFrame }

  TMessagesFrame = class(TFrame)
    HideSearchSpeedButton: TSpeedButton;
    MsgCtrlPopupMenu: TPopupMenu;
    SearchEdit: TEdit;
    SearchNextSpeedButton: TSpeedButton;
    SearchPanel: TPanel;
    SearchPrevSpeedButton: TSpeedButton;
    ShowIDMenuItem: TMenuItem;
    UnhideMsgTypeSeparatorMenuItem: TMenuItem;
    procedure AboutToolMenuItemClick(Sender: TObject);
    procedure AddFilterMenuItemClick(Sender: TObject);
    procedure ClearHideMsgTypesMenuItemClick(Sender: TObject);
    procedure CopyAllMenuItemClick(Sender: TObject);
    procedure CopyFilenameMenuItemClick(Sender: TObject);
    procedure CopyMsgMenuItemClick(Sender: TObject);
    procedure CopyShownMenuItemClick(Sender: TObject);
    procedure FileStyleMenuItemClick(Sender: TObject);
    procedure FindMenuItemClick(Sender: TObject);
    procedure HideHintsWithoutPosMenuItemClick(Sender: TObject);
    procedure HideMsgOfTypeMenuItemClick(Sender: TObject);
    procedure HideUrgencyMenuItemClick(Sender: TObject);
    procedure HideSearchSpeedButtonClick(Sender: TObject);
    procedure MsgCtrlPopupMenuClose(Sender: TObject);
    procedure MsgCtrlPopupMenuPopup(Sender: TObject);
    procedure OnSwitchFilterClick(Sender: TObject);
    procedure SaveAllToFileMenuItemClick(Sender: TObject);
    procedure SaveShownToFileMenuItemClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure SearchEditEnter(Sender: TObject);
    procedure SearchEditExit(Sender: TObject);
    procedure SearchNextSpeedButtonClick(Sender: TObject);
    procedure SearchPrevSpeedButtonClick(Sender: TObject);
    procedure ShowIDMenuItemClick(Sender: TObject);
    procedure TranslateMenuItemClick(Sender: TObject);
    procedure UnhideMsgTypeClick(Sender: TObject);
  private
    function AllMessagesAsString(const OnlyShown: boolean): String;
    function GetViews(Index: integer): TLMsgWndView;
    procedure SaveClicked(OnlyShown: boolean);
    procedure CopyAllClicked(OnlyShown: boolean);
    procedure CopyMsgToClipboard(OnlyFilename: boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    MessagesCtrl: TMessagesCtrl;
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    // Views
    function ViewCount: integer;
    property Views[Index: integer]: TLMsgWndView read GetViews;
    function GetView(aCaption: string; CreateIfNotExist: boolean): TLMsgWndView;
    function FindUnfinishedView: TLMsgWndView;
    procedure DeleteView(View: TLMsgWndView); // free view
    function IndexOfView(View: TLMsgWndView): integer;
    procedure ClearViews; // deletes/frees all views

    // source marks
    procedure CreateMarksForFile(aSynEdit: TSynEdit; aFilename: string;
      DeleteOld: boolean);
    procedure ApplySrcChangeds(Changes: TETSrcChanges);

    // message lines
    procedure SelectMsgLine(Msg: TMessageLine; DoScroll: boolean);
    function SelectFirstUrgentMessage(aMinUrgency: TMessageLineUrgency;
      WithSrcPos: boolean): boolean;
    function SelectNextUrgentMessage(aMinUrgency: TMessageLineUrgency;
      WithSrcPos, Downwards: boolean): boolean;
    procedure ClearCustomMessages(const ViewCaption: string='');
    function AddCustomMessage(TheUrgency: TMessageLineUrgency; Msg: string;
      aFilename: string = ''; LineNumber: integer = 0; Column: integer = 0;
      const ViewCaption: string = CustomViewCaption): TMessageLine;

    // misc
    function GetDefaultSearchText: string;
  end;

const
  MessagesMenuRootName = 'Messages';
var
  MsgFindMenuItem: TIDEMenuCommand;
  MsgQuickFixMenuSection: TIDEMenuSection;
  MsgClearMenuItem: TIDEMenuCommand;
  MsgHideMsgOfTypeMenuItem: TIDEMenuCommand;
  MsgUnhideMsgTypeMenuSection: TIDEMenuSection;
    MsgClearHideMsgTypesMenuItem: TIDEMenuCommand;
  MsgHideBelowMenuSection: TIDEMenuSection;
    MsgHideWarningsMenuItem: TIDEMenuCommand;
    MsgHideNotesMenuItem: TIDEMenuCommand;
    MsgHideHintsMenuItem: TIDEMenuCommand;
    MsgHideVerboseMenuItem: TIDEMenuCommand;
    MsgHideDebugMenuItem: TIDEMenuCommand;
    MsgHideNoneMenuItem: TIDEMenuCommand;
  MsgHideHintsWithoutPosMenuItem: TIDEMenuCommand;
  MsgSwitchFilterMenuSection: TIDEMenuSection;
    MsgFiltersMenuSection: TIDEMenuSection;
    MsgAddFilterMenuItem: TIDEMenuCommand;
  MsgCopyMenuSection: TIDEMenuSection;
    MsgCopyFilenameMenuItem: TIDEMenuCommand;
    MsgCopyMsgMenuItem: TIDEMenuCommand;
    MsgCopyShownMenuItem: TIDEMenuCommand;
    MsgCopyAllMenuItem: TIDEMenuCommand;
  MsgSaveToFileMenuSection: TIDEMenuSection;
    MsgSaveAllToFileMenuItem: TIDEMenuCommand;
    MsgSaveShownToFileMenuItem: TIDEMenuCommand;
  MsgFilenameStyleMenuSection: TIDEMenuSection;
    MsgFileStyleShortMenuItem: TIDEMenuCommand;
    MsgFileStyleRelativeMenuItem: TIDEMenuCommand;
    MsgFileStyleFullMenuItem: TIDEMenuCommand;
  MsgTranslateMenuItem: TIDEMenuCommand;
  MsgShowIDMenuItem: TIDEMenuCommand;
  MsgAboutToolMenuItem: TIDEMenuCommand;

procedure RegisterStandardMessagesViewMenuItems;

function CompareHideMsgType(HideMsgType1, HideMsgType2: Pointer): integer;
function CompareLineAndHideMsgType(MessageLine1, HideMsgType1: Pointer): integer;

implementation

procedure RegisterStandardMessagesViewMenuItems;
var
  Path: String;
  Parent: TIDEMenuSection;
begin
  MessagesMenuRoot := RegisterIDEMenuRoot(MessagesMenuRootName);
  Path := MessagesMenuRoot.Name;
  MsgFindMenuItem := RegisterIDEMenuCommand(Path, 'Find', 'Find ...');
  MsgQuickFixMenuSection := RegisterIDEMenuSection(Path, 'Quick Fix');
  MsgClearMenuItem := RegisterIDEMenuCommand(Path, 'Clear', lisClear);
  MsgHideMsgOfTypeMenuItem:=RegisterIDEMenuCommand(Path,'HideMsgOfType','');
  MsgUnhideMsgTypeMenuSection:=RegisterIDEMenuCommand(Parent,'UnhideMsgType');
    Parent:=MsgUnhideMsgTypeMenuSection;
    Parent.ChildsAsSubMenu:=true;
    Parent.Caption:='Unhide message type';
    MsgUnhideMsgOfTypeMenuSection:=RegisterIDEMenuSection(Parent,'UnhideMsgOfTypeSection');
    MsgClearHideMsgTypesMenuItem:=RegisterIDEMenuCommand(Parent,'Unhide all','Unhide all');
  MsgHideBelowMenuSection:=RegisterIDEMenuSection(Path,'Hide Below Section');
    Parent:=MsgHideBelowMenuSection;
    Parent.ChildsAsSubMenu:=true;
    Parent.Caption:='Hide non urgent messages ...';
    MsgHideWarningsMenuItem:=RegisterIDEMenuCommand(Parent,'Hide warnings','Hide warnings');
    MsgHideNotesMenuItem:=RegisterIDEMenuCommand(Parent,'Hide notes','Hide notes');
    MsgHideHintsMenuItem:=RegisterIDEMenuCommand(Parent,'Hide hints','Hide hints');
    MsgHideVerboseMenuItem:=RegisterIDEMenuCommand(Parent,'Hide verbose messages','Hide verbose messages');
    MsgHideDebugMenuItem:=RegisterIDEMenuCommand(Parent,'Hide debug messages','Hide debug messages');
    MsgHideNoneMenuItem:=RegisterIDEMenuCommand(Parent,'Hide none, do not hide by urgency','Hide none, do not hide by urgency');
  MsgHideHintsWithoutPosMenuItem:=RegisterIDEMenuCommand(Path, 'Hide hints without source position', 'Hide hints without source position');
  MsgSwitchFilterMenuSection:=RegisterIDEMenuSection(Path,'Switch Filter Section');
    Parent:=MsgSwitchFilterMenuSection;
    Parent.ChildsAsSubMenu:=true;
    Parent.Caption:='Switch Filter Settings ...';
    MsgFiltersMenuSection:=RegisterIDEMenuSection(Parent,'Filters');
    MsgAddFilterMenuItem:=RegisterIDEMenuCommand(Parent,'Add Filter','Add filter ...');
  MsgCopyMenuSection:=RegisterIDEMenuSection(Path,'Copy');
    Parent:=MsgCopyMenuSection;
    Parent.ChildsAsSubMenu:=true;
    Parent.Caption:='Copy ...';
    MsgCopyFilenameMenuItem:=RegisterIDEMenuCommand(Parent,'Filename','Copy file name to clipboard');
    MsgCopyMsgMenuItem := RegisterIDEMenuCommand(Parent, 'Selected',lisCopySelectedMessagesToClipboard);
    MsgCopyShownMenuItem: := RegisterIDEMenuCommand(Parent, 'Shown', 'Copy shown messages to clipboard');
    MsgCopyAllMenuItem:=RegisterIDEMenuCommand(Parent,'All'.'Copy all/original messages to clipboard');
  MsgSaveToFileMenuSection:=RegisterIDEMenuSection(Path,'Save');
    Parent:=MsgSaveToFileMenuSection;
    Parent.ChildsAsSubMenu:=true;
    Parent.Caption:='Save ...';
    MsgSaveShownToFileMenuItem:=RegisterIDEMenuCommand(Parent,'Save shown messages to file ...','Shown');
    MsgSaveAllToFileMenuItem:=RegisterIDEMenuCommand(Parent,'Save all/original messages to file ...','All');
  MsgFilenameStyleMenuSection:=RegisterIDEMenuSection(Path,'Filename Styles');
    Parent:=MsgFilenameStyleMenuSection;
    Parent.ChildsAsSubMenu:=true;
    Parent.Caption:='Filename Style ...';
    MsgFileStyleShortMenuItem:=RegisterIDEMenuCommand(Parent,'Short','Short, no path');
    MsgFileStyleRelativeMenuItem:=RegisterIDEMenuCommand(Parent,'Relative','Relative');
    MsgFileStyleFullMenuItem:=RegisterIDEMenuCommand(Parent,'Full','Full');
  MsgTranslateMenuItem:=RegisterIDEMenuCommand(Path, 'Translate', 'Translate');
  MsgShowIDMenuItem:=RegisterIDEMenuCommand(Path, 'ShowID', 'Show message type ID');
  MsgAboutToolMenuItem:=RegisterIDEMenuCommand(Path, 'About', 'About Tool');
end;

function CompareHideMsgType(HideMsgType1, HideMsgType2: Pointer): integer;
var
  Item1: TLMVFilterHideMsgType absolute HideMsgType1;
  Item2: TLMVFilterHideMsgType absolute HideMsgType2;
begin
  Result:=SysUtils.CompareText(Item1.SubTool,Item2.SubTool);
  if Result<>0 then exit;
  if Item1.MsgID<Item2.MsgID then
    exit(-1)
  else if Item1.MsgID>Item2.MsgID then
    exit(1);
  Result:=0;
end;

function CompareLineAndHideMsgType(MessageLine1, HideMsgType1: Pointer
  ): integer;
var
  Line: TMessageLine absolute MessageLine1;
  Item: TLMVFilterHideMsgType absolute HideMsgType1;
begin
  Result:=SysUtils.CompareText(Line.SubTool,Item.SubTool);
  if Result<>0 then exit;
  if Line.MsgID<Item.MsgID then
    exit(-1)
  else if Line.MsgID>Item.MsgID then
    exit(1);
  Result:=0;
end;

{$R *.lfm}

{ TLMVFilterHideMsgType }

procedure TLMVFilterHideMsgType.SetMsgID(AValue: integer);
begin
  if FMsgID=AValue then Exit;
  FMsgID:=AValue;
  Changed;
end;

procedure TLMVFilterHideMsgType.SetSubTool(AValue: string);
begin
  if FSubTool=AValue then Exit;
  FSubTool:=AValue;
  Changed;
end;

procedure TLMVFilterHideMsgType.Changed;
begin
  Filter.UpdateHideMsgTypeIndex(Self);
  Filter.Changed;
end;

procedure TLMVFilterHideMsgType.InternalAssign(Src: TLMVFilterHideMsgType);
begin
  fSubTool:=Src.SubTool;
  fMsgID:=Src.MsgID;
end;

constructor TLMVFilterHideMsgType.Create(aFilter: TLMsgViewFilter);
begin
  FFilter:=aFilter;
end;

function TLMVFilterHideMsgType.IsEqual(Src: TLMVFilterHideMsgType): boolean;
begin
  if Self=Src then exit(true);
  Result:=(SubTool=Src.SubTool)
      and (MsgID=Src.MsgID);
end;

procedure TLMVFilterHideMsgType.Assign(Src: TLMVFilterHideMsgType);
begin
  if IsEqual(Src) then exit;
  InternalAssign(Src);
  Changed;
end;

{ TLMsgViewFilter }

// inline
function TLMsgViewFilter.HideMsgTypeCount: integer;
begin
  Result:=length(fHideMsgTypes);
end;

// inline
function TLMsgViewFilter.GetHideMsgTypes(Index: integer): TLMVFilterHideMsgType;
begin
  Result:=fHideMsgTypes[Index];
end;

procedure TLMsgViewFilter.SetCaption(AValue: string);
begin
  AValue:=UTF8Trim(AValue,[]);
  if FCaption=AValue then Exit;
  FCaption:=AValue;
end;

procedure TLMsgViewFilter.SetMinUrgency(AValue: TMessageLineUrgency);
begin
  if FMinUrgency=AValue then Exit;
  FMinUrgency:=AValue;
  Changed;
end;

procedure TLMsgViewFilter.SetHideNotesWithoutPos(AValue: boolean);
begin
  if FHideNotesWithoutPos=AValue then Exit;
  FHideNotesWithoutPos:=AValue;
  Changed;
end;

procedure TLMsgViewFilter.Changed;
begin
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TLMsgViewFilter.UpdateHideMsgTypeIndex(Item: TLMVFilterHideMsgType);
var
  OldIndex: Integer;
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
  StartIndex: Integer;
  EndIndex: Integer;
  NewIndex: Integer;
begin
  if HideMsgTypeCount=1 then exit;
  OldIndex:=Item.FIndex;
  if (OldIndex>0) and (CompareHideMsgType(Item,fHideMsgTypes[OldIndex-1])<0)
  then begin
    StartIndex:=0;
    EndIndex:=OldIndex-1;
  end else if (OldIndex<HideMsgTypeCount-1)
  and (CompareHideMsgType(Item,fHideMsgTypes[OldIndex+1])>0) then begin
    StartIndex:=OldIndex+1;
    EndIndex:=HideMsgTypeCount-1;
  end else
    exit;

  l:=StartIndex;
  r:=EndIndex;
  m:=0;
  cmp:=0;
  while l<=r do begin
    m:=(l+r) div 2;
    cmp:=CompareHideMsgType(Item,fHideMsgTypes[m]);
    if cmp<0 then
      r:=m-1
    else if cmp>0 then
      l:=m+1
    else
      break;
  end;
  if cmp<=0 then
    NewIndex:=m
  else
    NewIndex:=m+1;
  if OldIndex<NewIndex then begin
    system.Move(fHideMsgTypes[OldIndex+1],fHideMsgTypes[OldIndex],
      SizeOf(TLMVFilterHideMsgType)*(NewIndex-OldIndex));
  end else if OldIndex>NewIndex then begin
    system.Move(fHideMsgTypes[NewIndex],fHideMsgTypes[NewIndex+1],
      SizeOf(TLMVFilterHideMsgType)*(OldIndex-NewIndex));
  end else
    exit;
  fHideMsgTypes[NewIndex]:=Item;

  {$IFDEF CheckExtTools}
  ConsistencyCheck;
  {$ENDIF}
end;

constructor TLMsgViewFilter.Create;
begin
  FMinUrgency:=mluHint;
  FHideNotesWithoutPos:=true;
end;

destructor TLMsgViewFilter.Destroy;
begin
  ClearHideMsgTypes;
  inherited Destroy;
end;

procedure TLMsgViewFilter.Clear;
begin
  MinUrgency:=mluHint;
  HideNotesWithoutPos:=true;
  ClearHideMsgTypes;
end;

procedure TLMsgViewFilter.SetToFitsAll;
begin
  MinUrgency:=mluNone;
  HideNotesWithoutPos:=false;
  ClearHideMsgTypes;
end;

function TLMsgViewFilter.IsEqual(Src: TLMsgViewFilter): boolean;
var
  i: Integer;
begin
  Result:=false;
  if Self=Src then exit(true);
  if (MinUrgency<>Src.MinUrgency)
  or (HideNotesWithoutPos<>Src.HideNotesWithoutPos)
  or (HideMsgTypeCount<>Src.HideMsgTypeCount)
  then exit;
  for i:=0 to HideMsgTypeCount-1 do
    if not HideMsgTypes[i].IsEqual(Src.HideMsgTypes[i]) then exit;
  Result:=true;
end;

procedure TLMsgViewFilter.Assign(Src: TLMsgViewFilter);
var
  NewCnt: Integer;
  OldCnt: Integer;
  i: Integer;
begin
  if IsEqual(Src) then exit;
  fMinUrgency:=Src.MinUrgency;
  FHideNotesWithoutPos:=Src.HideNotesWithoutPos;

  // hide msg type
  NewCnt:=Src.HideMsgTypeCount;
  OldCnt:=HideMsgTypeCount;
  for i:=NewCnt to OldCnt-1 do
    FreeAndNil(fHideMsgTypes[i]);
  SetLength(fHideMsgTypes,NewCnt);
  for i:=0 to NewCnt-1 do begin
    if fHideMsgTypes[i]=nil then
      fHideMsgTypes[i]:=TLMVFilterHideMsgType.Create(Self);
    fHideMsgTypes[i].InternalAssign(Src.HideMsgTypes[i]);
  end;

  Changed;
end;

function TLMsgViewFilter.LineFits(Line: TMessageLine): boolean;
begin
  Result:=false;

  if ord(Line.Urgency)<ord(MinUrgency) then exit;

  if [mlfHiddenByIDEDirective,mlfFixed]*Line.Flags<>[] then exit;

  if HideNotesWithoutPos and (Line.Urgency<=mluNote)
  and ((Line.Filename='') or (Line.Line<1)) then exit;

  if IndexOfHideMsgType(Line)>=0 then exit;

  Result:=true;
end;

function TLMsgViewFilter.AddHideMsgType(SubTool: string;
  MsgID: integer): TLMVFilterHideMsgType;
var
  i: Integer;
begin
  i:=length(fHideMsgTypes);
  SetLength(fHideMsgTypes,i+1);
  Result:=TLMVFilterHideMsgType.Create(Self);
  fHideMsgTypes[i]:=Result;
  Result.FSubTool:=SubTool;
  Result.FMsgID:=MsgID;
  UpdateHideMsgTypeIndex(Result);
  Changed;
end;

procedure TLMsgViewFilter.DeleteHideMsgType(Index: integer);
begin
  if (Index<0) or (Index>=HideMsgTypeCount) then
    raise Exception.Create('');
  fHideMsgTypes[Index].Free;
  if Index<HideMsgTypeCount-1 then
    system.Move(fHideMsgTypes[Index+1],fHideMsgTypes[Index],
      SizeOf(TLMVFilterHideMsgType)*(HideMsgTypeCount-Index-1));
  SetLength(fHideMsgTypes,length(fHideMsgTypes)-1);
  Changed;
end;

procedure TLMsgViewFilter.ClearHideMsgTypes;
var
  i: Integer;
begin
  if HideMsgTypeCount=0 then exit;
  for i:=0 to HideMsgTypeCount-1 do
    fHideMsgTypes[i].Free;
  SetLength(fHideMsgTypes,0);
  Changed;
end;

function TLMsgViewFilter.IndexOfHideMsgType(Line: TMessageLine): integer;
var
  l: Integer;
  r: Integer;
  m: Integer;
  cmp: Integer;
begin
  l:=0;
  r:=HideMsgTypeCount-1;
  while l<=r do begin
    m:=(l+r) div 2;
    cmp:=CompareLineAndHideMsgType(Line,fHideMsgTypes[m]);
    if cmp<0 then
      r:=m-1
    else if cmp>0 then
      l:=m+1
    else
      exit(m);
  end;
  Result:=-1;
end;

procedure TLMsgViewFilter.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    raise Exception.Create(Msg);
  end;

var
  i: Integer;
begin
  for i:=0 to HideMsgTypeCount-2 do begin
    if CompareHideMsgType(fHideMsgTypes[i],fHideMsgTypes[i+1])>0 then
      E(IntToStr(i));
  end;
end;

{ TLMsgWndView }

procedure TLMsgWndView.OnMarksFixed(ListOfTMessageLine: TFPList);
var
  i: Integer;
  ViewLine: TMessageLine;
  j: Integer;
  WorkerMsg: TMessageLine;
begin
  //debugln(['TLMsgWndView.OnMarksFixed START ',ListOfTMessageLine.Count]);
  // apply marks to WorkerMessages
  if Tool<>nil then begin
    Tool.EnterCriticalSection;
    try
      for i:=0 to ListOfTMessageLine.Count-1 do begin
        ViewLine:=TMessageLine(ListOfTMessageLine[i]);
        j:=Tool.WorkerMessages.IndexOfOutputIndex(ViewLine.OutputIndex);
        if j<0 then continue;
        WorkerMsg:=Tool.WorkerMessages[j];
        WorkerMsg.Flags:=ViewLine.Flags;
        //debugln(['TLMsgWndView.OnMarksFixed j=',j,' ',dbgs(WorkerMsg.Flags),' ',dbgs(Pointer(WorkerMsg)),' WorkerMsg.OutputIndex=',WorkerMsg.OutputIndex,' ViewLine.OutputIndex=',ViewLine.OutputIndex]);
      end;
    finally
      Tool.LeaveCriticalSection;
    end;
  end;

  // delete messages from view
  for i:=ListOfTMessageLine.Count-1 downto 0 do begin
    ViewLine:=TMessageLine(ListOfTMessageLine[i]);
    Lines.Delete(ViewLine);
  end;
  ListOfTMessageLine.Clear;

  // update control
  Control.UpdateScrollBar(true);
  Control.Invalidate;
end;

procedure TLMsgWndView.SetToolState(AValue: TLMVToolState);
begin
  if FToolState=AValue then Exit;
  FToolState:=AValue;
  Control.Invalidate;
end;

procedure TLMsgWndView.SetFilter(AValue: TLMsgViewFilter);
begin
  FFilter.Assign(AValue);
end;

procedure TLMsgWndView.CallOnChangedInMainThread(Data: PtrInt);
begin
  FAsyncQueued:=false;
  if csDestroying in ComponentState then exit;
  if Assigned(OnChanged) then
    OnChanged(Self);
end;

procedure TLMsgWndView.FetchAllPending;
var
  OldLineCount: Integer;
  i: Integer;
  OldUpdateSortedSrcPos: Boolean;
  MsgLine: TMessageLine;
  Line: Integer;
  Col: Integer;
begin
  OldLineCount:=Lines.Count;
  inherited FetchAllPending;
  if OldLineCount=Lines.Count then exit;

  // apply pending src changes
  OldUpdateSortedSrcPos:=Lines.UpdateSortedSrcPos;
  Lines.UpdateSortedSrcPos:=false;
  try
    for i:=OldLineCount to Lines.Count-1 do begin
      MsgLine:=Lines[i];
      //debugln(['TLMsgWndView.FetchAllPending ',i,' ',MsgLine.Msg]);
      Line:=MsgLine.Line;
      Col:=MsgLine.Column;
      FPendingChanges.AdaptCaret(MsgLine.GetFullFilename,Line,Col,
        mlfLeftToken in MsgLine.Flags);
      MsgLine.SetSourcePosition(MsgLine.Filename,Line,Col);
    end;
  finally
    Lines.UpdateSortedSrcPos:=OldUpdateSortedSrcPos;
  end;
end;

procedure TLMsgWndView.QueueAsyncOnChanged;
begin
  if FAsyncQueued then exit;
  FAsyncQueued:=true;
  Application.QueueAsyncCall(@CallOnChangedInMainThread,0);
end;

procedure TLMsgWndView.RemoveAsyncOnChanged;
begin
  if not FAsyncQueued then exit;
  FAsyncQueued:=false;
  Application.RemoveAsyncCalls(Self);
end;

procedure TLMsgWndView.ToolExited;
var
  ErrCount: Integer;
  u: TMessageLineUrgency;
  MsgLine: TMessageLine;
  i: Integer;
  StartLine: Integer;
  sl: TStringList;
begin
  inherited ToolExited;
  if Tool.Terminated then begin
    ToolState:=lmvtsFailed;
  end else if (ExitStatus<>0) then begin
    // tool stopped with errors
    ErrCount:=0;
    EnterCriticalSection;
    try
      for u:=mluError to high(TMessageLineUrgency) do
        inc(ErrCount,Lines.UrgencyCounts[u]+PendingLines.UrgencyCounts[u]);
    finally
      LeaveCriticalSection;
    end;
    if ErrCount=0 then begin
      // parser did not add an error message
      // => add an error message
      // add the last 3 lines of output with fatal urgency
      Tool.EnterCriticalSection; // Note: always lock Tool before View
      try
        EnterCriticalSection;
        try
          StartLine:=Max(0,Tool.WorkerOutput.Count-100);
          if PendingLines.Count>0 then
            StartLine:=Max(StartLine,PendingLines[PendingLines.Count-1].OutputIndex+1);
          if Lines.Count>0 then
            StartLine:=Max(StartLine,Lines[Lines.Count-1].OutputIndex+1);
          for i:=StartLine to Tool.WorkerOutput.Count-1 do
          begin
            MsgLine:=PendingLines.CreateLine(-1);
            MsgLine.Msg:=Tool.WorkerOutput[i];
            MsgLine.Urgency:=mluPanic;
            PendingLines.Add(MsgLine);
          end;
          MsgLine:=PendingLines.CreateLine(-1);
          MsgLine.Urgency:=mluPanic;
          MsgLine.Msg:='tool stopped with exit code '+IntToStr(ExitStatus)+'. Use context menu to get more information.';
          PendingLines.Add(MsgLine);
        finally
          LeaveCriticalSection;
        end;
      finally
        Tool.LeaveCriticalSection;
      end;
    end;
    ToolState:=lmvtsFailed;
  end else if Tool.ErrorMessage<>'' then begin
    // error executing the tool
    EnterCriticalSection;
    try
      sl:=TStringList.Create;
      try
        sl.Text:=Tool.ErrorMessage;
        for i:=0 to sl.Count-1 do begin
          if sl[i]='' then continue;
          MsgLine:=PendingLines.CreateLine(-1);
          MsgLine.Urgency:=mluPanic;
          MsgLine.Msg:='internal error: '+sl[i];
          PendingLines.Add(MsgLine);
        end;
      finally
        sl.Free;
      end;
    finally
      LeaveCriticalSection;
    end;
    ToolState:=lmvtsFailed;
  end else
    ToolState:=lmvtsSuccess;
end;

constructor TLMsgWndView.Create(AOwner: TComponent);
begin
  fMessageLineClass:=TLMsgViewLine;
  inherited Create(AOwner);
  Lines.OnMarksFixed:=@OnMarksFixed;
  FFilter:=TLMsgViewFilter.Create;
  fPendingChanges:=TETMultiSrcChanges.Create;
end;

destructor TLMsgWndView.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FPendingChanges);
  FreeAndNil(FFilter);
end;

function TLMsgWndView.LineFits(Line: TMessageLine): boolean;
begin
  if FFilter<>nil then
    Result:=FFilter.LineFits(Line)
  else
    Result:=inherited LineFits(Line);
end;

function TLMsgWndView.HasContent: boolean;
begin
  Result:=GetShownLineCount(true,true)>0;
end;

function TLMsgWndView.GetShownLineCount(WithHeader, WithProgressLine: boolean
  ): integer;
begin
  Result:=Lines.Count;
  // the header is only shown if there SummaryMsg<>'' or ProgressLine.Msg<>'' or Lines.Count>0
  if ProgressLine.Msg<>'' then begin
    if WithHeader then
      inc(Result);
    if WithProgressLine then
      inc(Result);
  end else if SummaryMsg<>'' then begin
    if WithHeader then
      inc(Result);
  end else if (Result>0) and WithHeader then
    inc(Result);
end;

procedure TLMsgWndView.RebuildLines;
// called by main thread
var
  i: Integer;
  SrcMsg: TMessageLine;
  NewProgressLine: TMessageLine;
  NewMsg: TMessageLine;
  Line: Integer;
  Col: Integer;
begin
  if Tool=nil then exit;
  Tool.EnterCriticalSection; // lock Tool before View
  try
    EnterCriticalSection;
    try
      ClearLines;
      NewProgressLine:=nil;
      for i:=0 to Tool.WorkerMessages.Count-1 do begin
        SrcMsg:=Tool.WorkerMessages[i];
        //if Pos('"db"',SrcMsg.Msg)>0 then
         // debugln(['TLMsgWndView.RebuildLines i=',i,' Msg="',SrcMsg.Msg,'" Fits=',LineFits(SrcMsg),' ',dbgs(SrcMsg.Flags),' ',SrcMsg.OutputIndex]);
        if LineFits(SrcMsg) then begin
          NewProgressLine:=nil;
        end else begin
          NewProgressLine:=SrcMsg;
          continue;
        end;
        NewMsg:=Lines.CreateLine(-1);
        NewMsg.Assign(SrcMsg);
        // adapt line,col due to src changes
        Line:=NewMsg.Line;
        Col:=NewMsg.Column;
        FPendingChanges.AdaptCaret(NewMsg.GetFullFilename,Line,Col,
                                   mlfLeftToken in NewMsg.Flags);
        NewMsg.SetSourcePosition(NewMsg.Filename,Line,Col);
        //debugln(['TLMsgWndView.RebuildLines NewMsg=',Lines.Count,'="',NewMsg.Msg,'"']);
        Lines.Add(NewMsg);
      end;
      FLastWorkerMessageCount:=Tool.WorkerMessages.Count-1;
      if (NewProgressLine<>nil) and Running then begin
        ProgressLine.Assign(NewProgressLine);
      end
      else if ProgressLine.Msg<>'' then begin
        ProgressLine.Clear;
      end;
    finally
      LeaveCriticalSection;
    end;
  finally
    Tool.LeaveCriticalSection;
  end;
end;

function TLMsgWndView.ApplySrcChanges(Changes: TETSrcChanges): boolean;

  function ApplyChanges(CurChanges: TETSrcChanges;
    CurLines: TMessageLines): boolean;
  var
    FromY: integer;
    MaxY: integer;
    LineDiffBehindMaxY: integer;
    ToY: Integer;
    MsgLine: TMessageLine;
    Line: Integer;
    Col: Integer;
    OldUpdateSortedSrcPos: Boolean;
  begin
    Result:=false;
    if CurChanges.First=nil then exit;
    CurChanges.GetRange(FromY,MaxY,LineDiffBehindMaxY);
    if LineDiffBehindMaxY=0 then
      ToY:=MaxY
    else
      ToY:=High(Integer);
    OldUpdateSortedSrcPos:=Lines.UpdateSortedSrcPos;
    CurLines.UpdateSortedSrcPos:=false;
    try
      {if CurLines=Lines then begin
        debugln(['ApplyChanges MinY=',FromY,' MaxY=',MaxY,' LineDiffBehindMaxY=',LineDiffBehindMaxY]);
        CurChanges.WriteDebugReport('Changes:');
      end;}
      for MsgLine in CurLines.EnumerateFile(CurChanges.Filename,FromY,ToY)
      do begin
        Line:=MsgLine.Line;
        Col:=MsgLine.Column;
        if Line>MaxY then
          inc(Line,LineDiffBehindMaxY)
        else
          CurChanges.AdaptCaret(Line,Col,mlfLeftToken in MsgLine.Flags);
        //if CurLines=Lines then
        //  debugln(['ApplyChanges ',MsgLine.Msg,' Old=',MsgLine.Line,',',MsgLine.Column,' New=',Line,',',Col]);

        if (Line=MsgLine.Line) and (MsgLine.Column=Col) then continue;
        MsgLine.SetSourcePosition(MsgLine.Filename,Line,Col);
        Result:=true;
      end;
    finally
      CurLines.UpdateSortedSrcPos:=OldUpdateSortedSrcPos;
    end;
  end;

var
  Queue: TETSrcChanges;
  Change: TETSrcChange;
  Node: TAvgLvlTreeNode;
  aFilename: String;
begin
  Result:=false;
  //debugln(['TLMsgWndView.ApplySrcChanges START ',Changes.Filename,' ',Changes.First<>nil]);
  // check if there are changes
  if Changes.First=nil then exit;
  aFilename:=Changes.Filename;
  if aFilename='' then exit;

  // update visible lines
  Result:=ApplyChanges(Changes,Lines);

  // update pending lines
  if Tool<>nil then begin
    Tool.EnterCriticalSection; // lock Tool before View
    try
      EnterCriticalSection;
      try
        Queue:=PendingChanges.GetChanges(aFilename,true);
        Change:=Changes.First;
        while Change<>nil do begin
          Queue.Add(Change.Action,Change.FromPos,Change.ToPos);
          Change:=Change.Next;
        end;
        if not Running then begin
          // apply all pending changes to Tool.WorkerMessages
          Node:=PendingChanges.AllChanges.FindLowest;
          while Node<>nil do begin
            ApplyChanges(TETSrcChanges(Node.Data),Tool.WorkerMessages);
            Node:=Node.Successor;
          end;
          PendingChanges.Clear;
        end;
      finally
        LeaveCriticalSection;
      end;
    finally
      Tool.LeaveCriticalSection;
    end;
  end;
end;

{ TMsgCtrlUrgencyStyle }

procedure TMsgCtrlUrgencyStyle.SetColor(AValue: TColor);
begin
  if FColor=AValue then Exit;
  FColor:=AValue;
  Changed;
end;

procedure TMsgCtrlUrgencyStyle.SetImageIndex(AValue: integer);
begin
  if FImageIndex=AValue then Exit;
  FImageIndex:=AValue;
  Changed;
end;

procedure TMsgCtrlUrgencyStyle.SetTranslated(AValue: string);
begin
  if FTranslated=AValue then Exit;
  FTranslated:=AValue;
  Changed;
end;

procedure TMsgCtrlUrgencyStyle.Changed;
begin
  Control.Invalidate;
end;

constructor TMsgCtrlUrgencyStyle.Create(AControl: TMessagesCtrl;
  TheUrgency: TMessageLineUrgency);
begin
  FControl:=AControl;
  fUrgency:=TheUrgency;
  FImageIndex:=-1;
  FColor:=clDefault;
end;

function TMsgCtrlUrgencyStyle.Equals(Obj: TObject): boolean;
var
  Src: TMsgCtrlUrgencyStyle;
begin
  if Obj is TMsgCtrlUrgencyStyle then begin
    Src:=TMsgCtrlUrgencyStyle(Obj);
    Result:=(ImageIndex=Src.ImageIndex)
        and (Color=Src.Color)
        and (Translated=Src.Translated);
  end else
    Result:=inherited Equals(Obj);
end;

procedure TMsgCtrlUrgencyStyle.Assign(Src: TMsgCtrlUrgencyStyle);
begin
  if Equals(Src) then exit;
  fImageIndex:=Src.ImageIndex;
  fColor:=Src.Color;
  fTranslated:=Src.Translated;
  Changed;
end;

procedure TMsgCtrlUrgencyStyle.SetValues(TheTranslated: string;
  TheImageIndex: integer; TheColor: TColor);
begin
  Translated:=TheTranslated;
  ImageIndex:=TheImageIndex;
  Color:=TheColor;
end;

{ TMessagesCtrl }

// inline
function TMessagesCtrl.ViewCount: integer;
begin
  Result:=FViews.Count;
end;

// inline
function TMessagesCtrl.FilterCount: integer;
begin
  Result:=FFilters.Count;
end;

function TMessagesCtrl.GetViews(Index: integer): TLMsgWndView;

  procedure RaiseOutOfBounds;
  begin
    raise Exception.Create('TMessagesCtrl.GetViews '+IntToStr(Index)+' out of bounds '+IntToStr(ViewCount));
  end;

begin
  if (Index<0) or (Index>=ViewCount) then
    RaiseOutOfBounds;
  Result:=TLMsgWndView(FViews[Index]);
end;

procedure TMessagesCtrl.OnViewChanged(Sender: TObject);
var
  AllViewsStopped: Boolean;
  i: Integer;
begin
  for i:=0 to ViewCount-1 do begin
    if Views[i].Running then begin
      // the views may change many times
      // reduce the update of the control to a few per second by using a timer
      fSomeViewsRunning:=true;
      FUpdateTimer.Enabled:=true;
      exit;
    end;
  end;
  AllViewsStopped:=fSomeViewsRunning;
  fSomeViewsRunning:=false;
  // no views running => update immediately
  FetchNewMessages;

  if AllViewsStopped then
    DoAllViewsStopped;
end;

procedure TMessagesCtrl.FetchNewMessages;
var
  i: Integer;
begin
  if csDestroying in ComponentState then exit;
  BeginUpdate;
  try
    for i:=0 to ViewCount-1 do
      FetchNewMessages(Views[i]);
  finally
    EndUpdate;
  end;
  UpdateScrollBar(true);
end;

procedure TMessagesCtrl.FetchNewMessages(View: TLMsgWndView);
var
  OldLineCount: Integer;
  LastLineWasVisible: Boolean;
begin
  if csDestroying in ComponentState then exit;
  if IndexOfView(View)<0 then exit;

  LastLineWasVisible:=IsLastLineVisible(View);
  //debugln(['TMessagesCtrl.FetchNewMessages START ScrollTop=',ScrollTop,' ScrollTopMax=',ScrollTopMax,' Last=',View.GetShownLineCount(false,true),' LineTop=',GetLineTop(View,View.GetShownLineCount(false,true),true),' IsLastLineVisible=',IsLastLineVisible(View)]);

  OldLineCount:=View.Lines.Count;
  if (not View.Running) and LastLineWasVisible then
    AutoScrollToNewMessage:=true; // this view stoped running -> let other views take over the focus
  if not View.ApplyPending then exit;
  CreateSourceMarks(View,OldLineCount);
  UpdateScrollBar(true);
  Invalidate;

  if LastLineWasVisible or AutoScrollToNewMessage then begin
    // scroll to last line
    AutoScrollToNewMessage:=false; // avoid switching back and forth between two running Views
    ScrollToLine(View,View.GetShownLineCount(false,true),true);
    //debugln(['TMessagesCtrl.FetchNewMessages END ScrollTop=',ScrollTop,' ScrollTopMax=',ScrollTopMax,' Last=',View.GetShownLineCount(false,true),' LineTop=',GetLineTop(View,View.GetShownLineCount(false,true),true),' IsLastLineVisible=',IsLastLineVisible(View)]);
  end;
end;

procedure TMessagesCtrl.MsgUpdateTimerTimer(Sender: TObject);
begin
  FUpdateTimer.Enabled:=false;
  FetchNewMessages;
end;

procedure TMessagesCtrl.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor=AValue then Exit;
  FBackgroundColor:=AValue;
  Invalidate;
end;

procedure TMessagesCtrl.SetFilenameStyle(AValue: TMsgCtrlFileNameStyle);
begin
  if FFilenameStyle=AValue then Exit;
  FFilenameStyle:=AValue;
  Invalidate;
end;

procedure TMessagesCtrl.SetActiveFilter(AValue: TLMsgViewFilter);
var
  i: Integer;
begin
  if (AValue=nil) or (ActiveFilter=AValue) then exit;
  i:=FFilters.IndexOf(AValue);
  if i<0 then begin
    if FActiveFilter.IsEqual(AValue) then exit;
    FActiveFilter.Assign(AValue);
  end else
    FActiveFilter:=AValue;
  IdleConnected:=true;
end;

procedure TMessagesCtrl.SetHeaderBackground(aToolState: TLMVToolState;
  AValue: TColor);
begin
  if FHeaderBackground[aToolState]=AValue then exit;
  FHeaderBackground[aToolState]:=AValue;
  Invalidate;
end;

procedure TMessagesCtrl.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TMessagesCtrl.SetImages(AValue: TCustomImageList);
begin
  if FImages=AValue then Exit;
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages:=AValue;
  if Images <> nil then begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
    if ItemHeight<Images.Height+2 then
      ItemHeight:=Images.Height+2;
  end;
  Invalidate;
end;

procedure TMessagesCtrl.SetItemHeight(AValue: integer);
begin
  FItemHeight:=Max(0,FItemHeight);
  if FItemHeight=AValue then Exit;
  FItemHeight:=AValue;
  UpdateScrollBar(true);
  Invalidate;
end;

procedure TMessagesCtrl.SetOptions(NewOptions: TMsgCtrlOptions);
var
  ChangedOptions: TMsgCtrlOptions;
begin
  if FOptions=NewOptions then Exit;
  ChangedOptions:=(FOptions-NewOptions)+(NewOptions-FOptions);
  FOptions:=NewOptions;
  if [mcoShowStats,mcoShowTranslated,mcoShowMessageID,mcoShowMsgIcons]*ChangedOptions<>[] then
    Invalidate;
end;

procedure TMessagesCtrl.SetScrollLeft(AValue: integer);
begin
  AValue:=Max(0,Min(AValue,ScrollLeftMax));
  if FScrollLeft=AValue then Exit;
  FScrollLeft:=AValue;
  UpdateScrollBar(false);
  Invalidate;
end;

procedure TMessagesCtrl.SetScrollTop(AValue: integer);
begin
  AValue:=Max(0,Min(AValue,ScrollTopMax));
  if FScrollTop=AValue then Exit;
  FScrollTop:=AValue;
  UpdateScrollBar(false);
  Invalidate;
end;

procedure TMessagesCtrl.SetSearchText(AValue: string);
begin
  if FSearchText=AValue then Exit;
  FSearchText:=AValue;
  IdleConnected:=true;
end;

procedure TMessagesCtrl.SetSelectedLine(AValue: integer);
begin
  if AValue<-1 then AValue:=-1;
  if FSelectedLine=AValue then Exit;
  AValue:=Min(AValue,GetMaxSelectedLine);
  if FSelectedLine=AValue then Exit;
  FSelectedLine:=AValue;
  Invalidate;
end;

procedure TMessagesCtrl.SetSelectedView(AValue: TLMsgWndView);
begin
  if FSelectedView=AValue then Exit;
  FSelectedView:=AValue;
  Invalidate;
end;

procedure TMessagesCtrl.SetSourceMarks(AValue: TETMarks);
begin
  if FSourceMarks=AValue then Exit;
  FSourceMarks:=AValue;
  if SourceMarks<>nil then
    FreeNotification(SourceMarks);
end;

procedure TMessagesCtrl.SetUrgencyStyles(Urgency: TMessageLineUrgency;
  AValue: TMsgCtrlUrgencyStyle);
begin
  fUrgencyStyles[Urgency].Assign(AValue);
end;

procedure TMessagesCtrl.SetAutoHeaderBackground(AValue: TColor);
begin
  if FAutoHeaderBackground=AValue then Exit;
  FAutoHeaderBackground:=AValue;
  Invalidate;
end;

procedure TMessagesCtrl.WMHScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the line
    SB_TOP: ScrollLeft := 1;
    SB_BOTTOM: ScrollLeft := ScrollLeftMax;
      // Scrolls one char left / right
    SB_LINEDOWN: ScrollLeft := ScrollLeft + 1;
    SB_LINEUP: ScrollLeft := ScrollLeft - 1;
      // Scrolls one page of chars left / right
    SB_PAGEDOWN: ScrollLeft := ScrollLeft + (ClientWidth div 2);
    SB_PAGEUP: ScrollLeft := ScrollLeft - (ClientHeight div 2);
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrollLeft := Msg.Pos;
  end;
end;

procedure TMessagesCtrl.WMVScroll(var Msg: TLMScroll);
begin
  case Msg.ScrollCode of
      // Scrolls to start / end of the text
    SB_TOP:        ScrollTop := 0;
    SB_BOTTOM:     ScrollTop := ScrollTopMax;
      // Scrolls one line up / down
    SB_LINEDOWN:   ScrollTop := ScrollTop + ItemHeight div 2;
    SB_LINEUP:     ScrollTop := ScrollTop - ItemHeight div 2;
      // Scrolls one page of lines up / down
    SB_PAGEDOWN:   ScrollTop := ScrollTop + ClientHeight - ItemHeight;
    SB_PAGEUP:     ScrollTop := ScrollTop - ClientHeight + ItemHeight;
      // Scrolls to the current scroll bar position
    SB_THUMBPOSITION,
    SB_THUMBTRACK: ScrollTop := Msg.Pos;
      // Ends scrolling
    SB_ENDSCROLL:  SetCaptureControl(nil); // release scrollbar capture
  end;
end;

procedure TMessagesCtrl.WMMouseWheel(var Message: TLMMouseEvent);
begin
  if Mouse.WheelScrollLines=-1 then
  begin
    // -1 : scroll by page
    ScrollTop := ScrollTop -
              (Message.WheelDelta * (ClientHeight - ItemHeight)) div 120;
  end else begin
    // scrolling one line -> scroll half an item, see SB_LINEDOWN and SB_LINEUP
    // handler in WMVScroll
    ScrollTop := ScrollTop -
        (Message.WheelDelta * Mouse.WheelScrollLines*ItemHeight) div 240;
  end;
  Message.Result := 1;
end;

procedure TMessagesCtrl.WMSetFocus(var Message: TLMSetFocus);
begin
  Invalidate;
  inherited;
end;

procedure TMessagesCtrl.WMKillFocus(var Message: TLMKillFocus);
begin
  Invalidate;
  inherited;
end;

function TMessagesCtrl.GetMaxSelectedLine: integer;
var
  View: TLMsgWndView;
begin
  View:=SelectedView;
  if View<>nil then
    Result:=View.GetShownLineCount(false,true)-1
  else
    Result:=-1;
end;

procedure TMessagesCtrl.ImageListChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TMessagesCtrl.OnIdle(Sender: TObject; var Done: Boolean);
var
  View: TLMsgWndView;
  LineNumber: integer;
  i: Integer;
begin
  //debugln(['TMessagesCtrl.OnIdle fLastLoSearchText=',fLastLoSearchText,' ',UTF8LowerCase(fSearchText)]);
  for i:=0 to ViewCount-1 do begin
    View:=Views[i];
    if not View.Filter.IsEqual(ActiveFilter) then begin
      View.EnterCriticalSection;
      try
        View.Filter:=ActiveFilter;
      finally
        View.LeaveCriticalSection;
      end;
      View.RebuildLines;
      CreateSourceMarks(View,0);
      UpdateScrollBar(true);
      Invalidate;
    end;
  end;

  if fLastLoSearchText<>UTF8LowerCase(fSearchText) then begin
    fLastLoSearchText:=UTF8LowerCase(FSearchText);
    if SearchNext(fLastSearchStartView,fLastSearchStartLine,false,true,
      View,LineNumber)
    then begin
      //debugln(['TMessagesCtrl.OnIdle search text found ',LineNumber]);
      Select(View,LineNumber,true,true);
    end else begin
      //debugln(['TMessagesCtrl.OnIdle search text not found']);
    end;
    Invalidate;
  end;
  IdleConnected:=false;
end;

procedure TMessagesCtrl.OnFilterChanged(Sender: TObject);
begin
  IdleConnected:=true;
end;

function TMessagesCtrl.GetSelectedLine: integer;
var
  View: TLMsgWndView;
begin
  View:=SelectedView;
  if View<>nil then begin
    Result:=FSelectedLine;
    if Result>=0 then
      Result:=Min(FSelectedLine,GetMaxSelectedLine);
  end else begin
    Result:=-1;
  end;
  FSelectedLine:=Result;
end;

procedure TMessagesCtrl.CreateSourceMarks(View: TLMsgWndView;
  StartLineNumber: Integer);
var
  i: Integer;
begin
  if SourceMarks=nil then exit;
  for i:=StartLineNumber to View.Lines.Count-1 do
    CreateSourceMark(View.Lines[i],nil);
end;

function TMessagesCtrl.GetFilters(Index: integer): TLMsgViewFilter;

  procedure RaiseOutOfBounds;
  begin
    raise Exception.Create('TMessagesCtrl.GetFilters '+IntToStr(Index)+' out of bounds '+IntToStr(FilterCount));
  end;

begin
  if (Index<0) or (Index>=FilterCount) then
    RaiseOutOfBounds;
  Result:=TLMsgViewFilter(fFilters[Index]);
end;

function TMessagesCtrl.GetHeaderBackground(aToolState: TLMVToolState): TColor;
begin
  Result:=FHeaderBackground[aToolState];
end;

procedure TMessagesCtrl.CreateSourceMark(MsgLine: TMessageLine;
  aSynEdit: TSynEdit);
var
  SourceMark: TETMark;
begin
  if TLMsgViewLine(MsgLine).Mark<>nil then exit;
  if ord(MsgLine.Urgency)<ord(mluHint) then exit;
  SourceMark:=SourceMarks.CreateMark(MsgLine,aSynEdit);
  if SourceMark=nil then exit;
  TLMsgViewLine(MsgLine).Mark:=SourceMark;
end;

function TMessagesCtrl.GetUrgencyStyles(Urgency: TMessageLineUrgency
  ): TMsgCtrlUrgencyStyle;
begin
  Result:=fUrgencyStyles[Urgency];
end;

procedure TMessagesCtrl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if (AComponent is TLMsgWndView) and (FViews.IndexOf(AComponent)>=0) then begin
      if fLastSearchStartView=AComponent then
        fLastSearchStartView:=nil;
      if SelectedView=AComponent then
        FSelectedView:=nil;
      RemoveView(TLMsgWndView(AComponent));
    end
    else if AComponent=Images then
      Images:=nil
    else if AComponent=SourceMarks then
      SourceMarks:=nil;
  end;
end;

procedure TMessagesCtrl.Paint;
var
  LoSearchText: string;

  procedure DrawText(ARect: TRect; aTxt: string; IsSelected: boolean;
    TxtColor: TColor);
  var
    Details: TThemedElementDetails;
    TextRect: TRect;
    p: SizeInt;
    LoTxt: String;
    aLeft: Integer;
    aRight: Integer;
    LastP: Integer;
  begin
    Canvas.Font.Color:=Font.Color;
    TextRect:=ARect;
    TextRect.Right:=TextRect.Left+Canvas.TextWidth(aTxt)+2;
    if IsSelected then begin
      if mcsFocused in FStates then
        Details := ThemeServices.GetElementDetails(ttItemSelected)
      else
        Details := ThemeServices.GetElementDetails(ttItemSelectedNotFocus);
      ThemeServices.DrawElement(Canvas.Handle, Details, TextRect, nil);
      TxtColor:=clDefault;
    end else
      Details := ThemeServices.GetElementDetails(ttItemNormal);
    if LoSearchText<>'' then begin
      LoTxt:=UTF8LowerCase(aTxt);
      p:=1;
      LastP:=1;
      while p<=length(LoTxt) do begin
        p:=PosEx(LoSearchText,LoTxt,LastP);
        if p<1 then break;
        Canvas.Brush.Color:=clHighlight;
        aLeft:=TextRect.Left+Canvas.TextWidth(copy(ATxt,1,p-1));
        aRight:=aLeft+Canvas.TextWidth(copy(ATxt,p,length(LoSearchText)));
        Canvas.FillRect(aLeft,TextRect.Top+1,aRight,TextRect.Bottom-1);
        LastP:=p+length(LoSearchText);
      end;
      Canvas.Brush.Color:=BackgroundColor;
    end;
    if TxtColor=clDefault then
      ThemeServices.DrawText(Canvas, Details, ATxt, TextRect,
        DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX, 0)
    else begin
      p:=(TextRect.Top+TextRect.Bottom-Canvas.TextHeight('Mg')) div 2;
      Canvas.Font.Color:=TxtColor;
      Canvas.TextOut(TextRect.Left+2,p,ATxt);
    end;
  end;

var
  i: Integer;
  View: TLMsgWndView;
  y: Integer;
  j: Integer;
  Line: TMessageLine;
  Indent: Integer;
  NodeRect: TRect;
  ImgIndex: LongInt;
  IsSelected: Boolean;
  FirstLineIsNotSelectedMessage: Boolean;
  SecondLineIsNotSelectedMessage: Boolean;
begin
  if Focused then
    Include(FStates,mcsFocused)
  else
    Exclude(FStates,mcsFocused);
  //debugln(['TMessagesCtrl.Paint ',Focused,' CanFocus=',CanFocus,' TabStop=',TabStop]);
  LUIncreaseChangeStamp64(FPaintStamp);

  // paint background
  Canvas.Brush.Color:=BackgroundColor;
  Canvas.FillRect(0,0,ClientWidth,ClientHeight);

  Indent:=BorderWidth+2;
  LoSearchText:=fLastLoSearchText;

  // paint from top to bottom
  y:=-ScrollTop;
  for i:=0 to ViewCount-1 do begin
    if y>ClientHeight then break;
    View:=Views[i];
    if not View.HasContent then continue;
    View.FPaintStamp:=FPaintStamp;
    View.fPaintTop:=y;

    // draw header
    if (y+ItemHeight>0) and (y<ClientHeight) then begin
      // header text
      NodeRect:=Rect(0,y,ClientWidth,y+ItemHeight);
      Canvas.Brush.Color:=HeaderBackground[View.ToolState];
      Canvas.FillRect(NodeRect);
      Canvas.Pen.Style:=psDash;
      Canvas.Line(NodeRect.Left,NodeRect.Top,NodeRect.Right,NodeRect.Top);
      Canvas.Pen.Style:=psSolid;
      DrawText(NodeRect,GetHeaderText(View),
        (fSelectedView=View) and (FSelectedLine=-1),clDefault);
      Canvas.Brush.Color:=BackgroundColor;
    end;
    inc(y,ItemHeight);

    // draw lines
    j:=0;
    if y<0 then begin
      j:=Min((-y) div ItemHeight,View.Lines.Count);
      inc(y,j*ItemHeight);
    end;
    FirstLineIsNotSelectedMessage:=false;
    SecondLineIsNotSelectedMessage:=false;
    while (j<View.Lines.Count) and (y<ClientHeight) do begin
      Line:=View.Lines[j];
      NodeRect:=Rect(Indent,y,ClientWidth,y+ItemHeight);
      IsSelected:=(fSelectedView=View) and (FSelectedLine=j);
      if not IsSelected then begin
        if (y>-ItemHeight) and (y<=0) then
          FirstLineIsNotSelectedMessage:=true
        else if (y>0) and (y<=ItemHeight) then
          SecondLineIsNotSelectedMessage:=true;
      end;
      ImgIndex:=fUrgencyStyles[Line.Urgency].ImageIndex;
      if (Images<>nil) and (mcoShowMsgIcons in Options)
      and (ImgIndex>=0) and (ImgIndex<Images.Count) then begin
        Images.Draw(Canvas,
          NodeRect.Left + 1, (NodeRect.Top + NodeRect.Bottom - Images.Height) div 2,
          ImgIndex, gdeNormal);
        inc(NodeRect.Left,Images.Width+2);
      end;
      // message text
      DrawText(NodeRect,GetLineText(Line),IsSelected,
               UrgencyStyles[Line.Urgency].Color);
      inc(y,ItemHeight);
      inc(j);
    end;
    if FirstLineIsNotSelectedMessage and SecondLineIsNotSelectedMessage then begin
      // the first two lines are normal messages, not selected
      // => paint view header hint
      NodeRect:=Rect(0,0,ClientWidth,ItemHeight);
      Canvas.Brush.Color:=AutoHeaderBackground;
      Canvas.FillRect(NodeRect);
      Canvas.Pen.Style:=psDash;
      Canvas.Line(NodeRect.Left,NodeRect.Bottom,NodeRect.Right,NodeRect.Bottom);
      Canvas.Pen.Style:=psSolid;
      DrawText(NodeRect,GetHeaderText(View),false,clDefault);
      Canvas.Brush.Color:=BackgroundColor;
    end;
    inc(y,ItemHeight*(View.Lines.Count-j));

    // draw progress line
    if View.ProgressLine.Msg<>'' then begin
      if (y+ItemHeight>0) and (y<ClientHeight) then begin
        // progress text
        NodeRect:=Rect(Indent,y,ClientWidth,y+ItemHeight);
        DrawText(NodeRect,View.ProgressLine.Msg,
          (fSelectedView=View) and (FSelectedLine=View.Lines.Count),
          UrgencyStyles[View.ProgressLine.Urgency].Color);
      end;
      inc(y,ItemHeight);
    end;

    View.fPaintBottom:=y;
  end;

  // call OnPaint
  inherited Paint;
end;

procedure TMessagesCtrl.UpdateScrollBar(InvalidateScrollMax: boolean);
var
  ScrollInfo: TScrollInfo;
begin
  if InvalidateScrollMax then begin
    fScrollTopMax:=-1;
  end;
  if not HandleAllocated then exit;

  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_ALL or SIF_DISABLENOSCROLL;
  ScrollInfo.nMin := 0;
  ScrollInfo.nTrackPos := 0;
  ScrollInfo.nMax := ScrollTopMax+ClientHeight-1;
  if ClientHeight < 2 then
    ScrollInfo.nPage := 1
  else
    ScrollInfo.nPage := ClientHeight-1;
  if ScrollTop > ScrollTopMax then
    ScrollTop := ScrollTopMax;
  ScrollInfo.nPos := ScrollTop;
  //debugln(['TMessagesCtrl.UpdateScrollBar ScrollTop=',ScrollTop,' ScrollTopMax=',ScrollTopMax]);
  ShowScrollBar(Handle, SB_VERT, True);
  SetScrollInfo(Handle, SB_VERT, ScrollInfo, false);
end;

procedure TMessagesCtrl.CreateWnd;
begin
  inherited CreateWnd;
  ItemHeight:=Canvas.TextHeight('Mg')+2;
  UpdateScrollBar(false);
end;

procedure TMessagesCtrl.DoSetBounds(ALeft, ATop, AWidth, AHeight: integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  fScrollTopMax:=-1;
end;

procedure TMessagesCtrl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  View: TLMsgWndView;
  LineNumber: integer;
begin
  if not Focused and CanFocus then
    SetFocus;
  inherited MouseDown(Button, Shift, X, Y);
  if Button=mbLeft then begin
    if GetLineAt(Y,View,LineNumber) then begin
      SelectedView:=View;
      SelectedLine:=LineNumber;
      StoreSelectedAsSearchStart;

      if ((ssDouble in Shift) and (not (mcoSingleClickOpensFile in FOptions)))
      or ((mcoSingleClickOpensFile in FOptions) and ([ssDouble,ssTriple,ssQuad]*Shift=[]))
      then
        OpenSelection;
    end else begin

    end;
  end;
end;

procedure TMessagesCtrl.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);

  case Key of

  VK_DOWN:
    begin
      SelectNextShown(+1);
      Key:=VK_UNKNOWN;
    end;

  VK_UP:
    begin
      SelectNextShown(-1);
      Key:=VK_UNKNOWN;
    end;

  VK_HOME:
    begin
      SelectFirst(true,true);
      Key:=VK_UNKNOWN;
    end;

  VK_END:
    begin
      SelectLast(true,true);
      Key:=VK_UNKNOWN;
    end;

  VK_PRIOR: // Page Up
    begin
      SelectNextShown(-1-Max(0,ClientHeight div ItemHeight));
      Key:=VK_UNKNOWN;
    end;

  VK_NEXT: // Page Down
    begin
      SelectNextShown(1+Max(0,ClientHeight div ItemHeight));
      Key:=VK_UNKNOWN;
    end;
  end;
end;

function TMessagesCtrl.GetDefaultFilterCaption: string;
begin
  Result:='Default';
end;

procedure TMessagesCtrl.DoOnShowHint(HintInfo: PHintInfo);
var
  View: TLMsgWndView;
  Line: integer;
  MsgLine: TMessageLine;
  s: String;
begin
  if GetLineAt(HintInfo^.CursorPos.Y,View,Line) then begin
    MsgLine:=nil;
    if Line<0 then
      s:=GetHeaderText(View)
    else if Line<View.Lines.Count then begin
      MsgLine:=View.Lines[Line];
    end else begin
      MsgLine:=View.ProgressLine;
    end;
    if MsgLine<>nil then begin
      s:=GetLineText(MsgLine);
      s+=LineEnding+LineEnding;
      s+=ExternalToolList.GetMsgHint(MsgLine.SubTool,MsgLine.MsgID);
    end;
    HintInfo^.HintStr:=s;
    HintInfo^.ReshowTimeout:=500;
    HintInfo^.HideTimeout:=5000;
  end;
  inherited DoOnShowHint(HintInfo);
end;

procedure TMessagesCtrl.DoAllViewsStopped;
begin
  if Assigned(OnAllViewsStopped) then
    OnAllViewsStopped(Self);
  if mcoAutoOpenFirstError in Options then begin
    if SelectFirstUrgentMessage(mluError,true) then
      OpenSelection;
  end;
end;

function TMessagesCtrl.SearchNext(StartView: TLMsgWndView; StartLine: integer;
  SkipStart, Downwards: boolean; out View: TLMsgWndView; out LineNumber: integer
  ): boolean;
var
  CurView: TLMsgWndView;
  CurLine: Integer;
  CurViewLineCnt: integer;
  Txt: String;

  function Next: boolean;
  var
    i: Integer;
  begin
    if Downwards then begin
      inc(CurLine);
      if CurLine>=CurViewLineCnt then begin
        i:=IndexOfView(CurView);
        repeat
          inc(i);
          if i>=ViewCount then exit(false);
          CurView:=Views[i];
        until CurView.HasContent;
        CurLine:=-1;
        CurViewLineCnt:=CurView.GetShownLineCount(true,true);
      end;
    end else begin
      dec(CurLine);
      if CurLine<-1 then begin
        i:=IndexOfView(CurView);
        repeat
          dec(i);
          if i<0 then exit(false);
          CurView:=Views[i];
        until CurView.HasContent;
        CurViewLineCnt:=CurView.GetShownLineCount(true,true);
        CurLine:=CurViewLineCnt-1;
      end;
    end;
    Result:=true;
  end;

begin
  Result:=false;
  View:=nil;
  LineNumber:=-1;
  if ViewCount=0 then exit;
  if StartView=nil then begin
    // use default start
    if Downwards then begin
      StartView:=Views[0];
      StartLine:=-1;
    end else begin
      StartView:=Views[ViewCount-1];
      StartLine:=StartView.GetShownLineCount(true,true);
    end;
  end;
  CurView:=StartView;
  CurLine:=StartLine;
  CurViewLineCnt:=CurView.GetShownLineCount(true,true);
  // skip invalid line numbers
  if CurLine<-1 then begin
    SkipStart:=false;
    if Downwards then
      CurLine:=-1
    else if not Next then
      exit;
  end else if CurLine>=CurViewLineCnt then begin
    SkipStart:=false;
    if Downwards then begin
      if not Next then exit;
    end else
      CurLine:=CurViewLineCnt-1;
  end;
  // skip invalid views
  if not CurView.HasContent then begin
    SkipStart:=false;
    if not Next then exit;
  end;
  // skip start
  if SkipStart then
    if not Next then exit;
  // search
  repeat
    if CurLine<0 then
      Txt:=GetHeaderText(CurView)
    else if CurLine<CurView.Lines.Count then
      Txt:=GetLineText(CurView.Lines[CurLine])
    else
      Txt:=CurView.ProgressLine.Msg;
    Txt:=UTF8LowerCase(Txt);
    if Pos(fLastLoSearchText,Txt)>0 then begin
      View:=CurView;
      LineNumber:=CurLine;
      exit(true);
    end;
  until not Next;
end;

procedure TMessagesCtrl.Select(Msg: TMessageLine; DoScroll: boolean);
begin
  BeginUpdate;
  if (Msg=nil) or (Msg.Lines=nil) or (not (Msg.Lines.Owner is TLMsgWndView))
  then begin
    SelectedView:=nil;
    SelectedLine:=-1;
  end else begin
    SelectedView:=TLMsgWndView(Msg.Lines.Owner);
    SelectedLine:=Msg.Index;
    if DoScroll then
      ScrollToLine(SelectedView,SelectedLine,true);
  end;
  EndUpdate;
end;

function TMessagesCtrl.SelectNextOccurence(Downwards: boolean): boolean;
var
  View: TLMsgWndView;
  LineNumber: integer;
begin
  StoreSelectedAsSearchStart;
  Result:=SearchNext(SelectedView,SelectedLine,true,Downwards,View,LineNumber);
  if not Result then exit;
  Select(View,LineNumber,true,true);
end;

function TMessagesCtrl.SelectNextShown(Offset: integer): boolean;
// returns true if selection changed
var
  View: TLMsgWndView;
  Line: Integer;
  i: Integer;
begin
  Result:=false;
  {$IFDEF VerboseMsgCtrlSelectNextShown}
  debugln(['TMessagesCtrl.SelectNextShown START']);
  {$ENDIF}
  while Offset<>0 do begin
    {$IFDEF VerboseMsgCtrlSelectNextShown}
    debugln(['TMessagesCtrl.SelectNextShown LOOP Offset=',Offset,' ViewIndex=',IndexOfView(SelectedView),' Line=',SelectedLine]);
    {$ENDIF}
    if SelectedView=nil then begin
      if Offset>0 then begin
        SelectFirst(true,true);
        dec(Offset);
      end else begin
        SelectLast(true,true);
        Inc(Offset);
      end;
      Result:=true;
    end else begin
      View:=SelectedView;
      Line:=SelectedLine;
      if Offset>0 then begin
        {$IFDEF VerboseMsgCtrlSelectNextShown}
        debugln(['TMessagesCtrl.SelectNextShown NEXT View.GetShownLineCount(false,true)=',View.GetShownLineCount(false,true),' ',' ViewIndex=',IndexOfView(View),' Line=',Line]);
        {$ENDIF}
        inc(Line,Offset);
        if Line<View.GetShownLineCount(false,true) then
          Offset:=0
        else begin
          // next view
          Offset:=Line-View.GetShownLineCount(false,true);
          i:=IndexOfView(View);
          {$IFDEF VerboseMsgCtrlSelectNextShown}
          debugln(['TMessagesCtrl.SelectNextShown Line=',Line,' Offset=',Offset,' ViewIndex=',i]);
          {$ENDIF}
          repeat
            inc(i);
            if i>=ViewCount then begin
              {$IFDEF VerboseMsgCtrlSelectNextShown}
              debugln(['TMessagesCtrl.SelectNextShown can not go further down']);
              {$ENDIF}
              exit;
            end;
            View:=Views[i];
          until View.HasContent;
          Line:=-1;
        end;
      end else begin
        inc(Line,Offset);
        if Line>=-1 then
          Offset:=0
        else begin
          // previous view
          Offset:=Line+2;
          i:=IndexOfView(View);
          repeat
            dec(i);
            if i<0 then begin
              {$IFDEF VerboseMsgCtrlSelectNextShown}
              debugln(['TMessagesCtrl.SelectNextShown can not go further up']);
              {$ENDIF}
              exit;
            end;
            View:=Views[i];
          until View.HasContent;
          Line:=View.GetShownLineCount(true,true)-1;
        end;
      end;
      {$IFDEF VerboseMsgCtrlSelectNextShown}
      debugln(['TMessagesCtrl.SelectNextShown SELECT Offset=',Offset,' ViewIndex=',IndexOfView(View),' Line=',Line]);
      {$ENDIF}
      Select(View,Line,true,true);
      Result:=true;
    end;
  end;
  {$IFDEF VerboseMsgCtrlSelectNextShown}
  debugln(['TMessagesCtrl.SelectNextShown END ViewIndex=',IndexOfView(SelectedView),' Line=',SelectedLine]);
  {$ENDIF}
end;

function TMessagesCtrl.SelectLast(DoScroll, FullyVisible: boolean): boolean;
var
  i: Integer;
begin
  i:=ViewCount-1;
  while (i>=0) do begin
    if Views[i].HasContent then begin
      Select(Views[i],Views[i].GetShownLineCount(true,true)-1,DoScroll,FullyVisible);
      exit(true);
    end;
    dec(i);
  end;
  Result:=false;
end;

function TMessagesCtrl.SelectFirst(DoScroll, FullyVisible: boolean): boolean;
var
  i: Integer;
begin
  i:=0;
  while (i<ViewCount) do begin
    if Views[i].HasContent then begin
      Select(Views[i],-1,DoScroll,FullyVisible);
      exit(true);
    end;
    inc(i);
  end;
  Result:=false;
end;

function TMessagesCtrl.GetSelectedMsg: TMessageLine;
var
  View: TLMsgWndView;
  Line: Integer;
begin
  Result:=nil;
  View:=SelectedView;
  if View=nil then exit;
  Line:=SelectedLine;
  if (Line<0) then exit;
  if Line<View.Lines.Count then
    Result:=View.Lines[Line]
  else if (Line=View.Lines.Count) and (View.ProgressLine.Msg<>'') then
    Result:=View.ProgressLine;
end;

function TMessagesCtrl.SearchNextUrgent(StartView: TLMsgWndView;
  StartLine: integer; SkipStart, Downwards: boolean;
  aMinUrgency: TMessageLineUrgency; WithSrcPos: boolean; out
  View: TLMsgWndView; out LineNumber: integer): boolean;
var
  CurView: TLMsgWndView;
  CurLine: Integer;
  CurViewLineCnt: integer;
  MsgLine: TMessageLine;

  function Next: boolean;
  var
    i: Integer;
  begin
    if Downwards then begin
      inc(CurLine);
      if CurLine>=CurViewLineCnt then begin
        i:=IndexOfView(CurView);
        repeat
          inc(i);
          if i>=ViewCount then exit(false);
          CurView:=Views[i];
        until CurView.HasContent;
        CurLine:=-1;
        CurViewLineCnt:=CurView.GetShownLineCount(true,true);
      end;
    end else begin
      dec(CurLine);
      if CurLine<-1 then begin
        i:=IndexOfView(CurView);
        repeat
          dec(i);
          if i<0 then exit(false);
          CurView:=Views[i];
        until CurView.HasContent;
        CurViewLineCnt:=CurView.GetShownLineCount(true,true);
        CurLine:=CurViewLineCnt-1;
      end;
    end;
    Result:=true;
  end;

begin
  Result:=false;
  View:=nil;
  LineNumber:=-1;
  if ViewCount=0 then exit;
  if StartView=nil then begin
    // use default start
    if Downwards then begin
      StartView:=Views[0];
      StartLine:=-1;
    end else begin
      StartView:=Views[ViewCount-1];
      StartLine:=StartView.GetShownLineCount(true,true);
    end;
  end;
  CurView:=StartView;
  CurLine:=StartLine;
  CurViewLineCnt:=CurView.GetShownLineCount(true,true);
  // skip invalid line numbers
  if CurLine<-1 then begin
    SkipStart:=false;
    if Downwards then
      CurLine:=-1
    else if not Next then
      exit;
  end else if CurLine>=CurViewLineCnt then begin
    SkipStart:=false;
    if Downwards then begin
      if not Next then exit;
    end else
      CurLine:=CurViewLineCnt-1;
  end;
  // skip invalid views
  if not CurView.HasContent then begin
    SkipStart:=false;
    if not Next then exit;
  end;
  // skip start
  if SkipStart then
    if not Next then exit;
  // search
  repeat
    if (CurLine>=0) and (CurLine<CurView.Lines.Count) then begin
      MsgLine:=CurView.Lines[CurLine];
      if MsgLine.Urgency>=aMinUrgency then begin
        if (not WithSrcPos) or MsgLine.HasSourcePosition then begin
          View:=CurView;
          LineNumber:=CurLine;
          exit(true);
        end;
      end;
    end;
  until not Next;
end;

function TMessagesCtrl.SelectFirstUrgentMessage(
  aMinUrgency: TMessageLineUrgency; WithSrcPos: boolean): boolean;
var
  View: TLMsgWndView;
  LineNumber: integer;
begin
  Result:=false;
  if ViewCount=0 then exit;
  if not SearchNextUrgent(nil,0,false,true,aMinUrgency,WithSrcPos,View,LineNumber)
  then exit;
  Select(View,LineNumber,true,true);
  Result:=true;
end;

function TMessagesCtrl.SelectNextUrgentMessage(
  aMinUrgency: TMessageLineUrgency; WithSrcPos: boolean; Downwards: boolean
  ): boolean;
var
  View: TLMsgWndView;
  LineNumber: integer;
begin
  Result:=false;
  if not SearchNextUrgent(SelectedView,SelectedLine,true,Downwards,
    aMinUrgency,WithSrcPos,View,LineNumber)
  then exit;
  Select(View,LineNumber,true,true);
  Result:=true;
end;

function TMessagesCtrl.IsLineVisible(View: TLMsgWndView; LineNumber: integer
  ): boolean;
var
  y: Integer;
begin
  Result:=false;
  if View=nil then exit;
  y:=GetLineTop(View,LineNumber,true);
  if (y+ItemHeight>0) and (y<ClientHeight) then
    Result:=true;
end;

function TMessagesCtrl.IsLastLineVisible(View: TLMsgWndView): boolean;
var
  LineNumber: Integer;
begin
  LineNumber:=View.GetShownLineCount(false,true)-1;
  Result:=IsLineVisible(View,LineNumber);
end;

function TMessagesCtrl.GetLineText(Line: TMessageLine): string;
begin
  // 'filename(line,column) '
  case FilenameStyle of
  mcfsShort: Result:=Line.GetShortFilename;
  mcfsRelative: Result:=Line.GetRelativeFilename
  else Result:=Line.GetFullFilename;
  end;
  if Line.Line>0 then begin
    Result+='('+IntToStr(Line.Line)+','+IntToStr(Line.Column)+')';
  end;
  if Result<>'' then
    Result+=' ';

  // 'error: '
  if (mcoShowTranslated in Options)
  and (fUrgencyStyles[Line.Urgency].Translated<>'') then
    Result+=fUrgencyStyles[Line.Urgency].Translated
  else
    Result+=MessageLineUrgencyNames[Line.Urgency];
  Result+=': ';

  // message id
  if (mcoShowMessageID in Options) and (Line.MsgID<>0) then
   Result+='('+IntToStr(Line.MsgID)+') ';

  // message
  if (mcoShowTranslated in Options) and (Line.TranslatedMsg<>'') then
    Result+=Line.TranslatedMsg
  else
    Result+=Line.Msg;
end;

function TMessagesCtrl.GetHeaderText(View: TLMsgWndView): string;

  function GetStats(Lines: TMessageLines): string;
  var
    ErrCnt: Integer;
    WarnCnt: Integer;
    HintCnt: Integer;
    c: TMessageLineUrgency;
  begin
    Result:='';
    ErrCnt:=0;
    WarnCnt:=0;
    HintCnt:=0;
    for c:=Low(Lines.UrgencyCounts) to high(Lines.UrgencyCounts) do begin
      //debugln(['GetStats cat=',dbgs(c),' count=',Lines.UrgencyCounts[c]]);
      if c>=mluError then
        inc(ErrCnt,Lines.UrgencyCounts[c])
      else if c=mluWarning then
        inc(WarnCnt,Lines.UrgencyCounts[c])
      else if c in [mluHint,mluNote] then
        inc(HintCnt,Lines.UrgencyCounts[c]);
    end;
    if ErrCnt>0 then
      Result+=', Errors:'+IntToStr(ErrCnt);
    if WarnCnt>0 then
      Result+=', Warnings:'+IntToStr(WarnCnt);
    if HintCnt>0 then
      Result+=', Hints:'+IntToStr(HintCnt);
  end;

begin
  Result:=View.Caption;
  if View.SummaryMsg<>'' then
    Result+=' '+View.SummaryMsg;
  if mcoShowStats in Options then begin
    Result+=GetStats(View.Lines);
  end;
end;

function TMessagesCtrl.FindUnfinishedView: TLMsgWndView;
var
  i: Integer;
begin
  for i:=0 to ViewCount-1 do begin
    Result:=Views[i];
    //debugln(['TMessagesCtrl.FindUnfinishedView ',i,' ',ViewCount,' caption="',Result.Caption,'" Result.Tool=',dbgsname(Result.Tool)]);
    if not Result.HasFinished then exit;
  end;
  Result:=nil;
end;

function TMessagesCtrl.GetFilter(aCaption: string; CreateIfNotExist: boolean
  ): TLMsgViewFilter;
var
  i: Integer;
begin
  for i:=0 to FilterCount-1 do begin
    Result:=Filters[i];
    if SysUtils.CompareText(Result.Caption,aCaption)=0 then exit;
  end;
  if not CreateIfNotExist then
    exit(nil);
  Result:=TLMsgViewFilter.Create;
  Result.Caption:=aCaption;
  Result.OnChanged:=@OnFilterChanged;
  FFilters.Add(Result);
end;

procedure TMessagesCtrl.DeleteFilter(Index: integer);
var
  CurFilter: TLMsgViewFilter;
begin
  CurFilter:=Filters[Index];
  if (CurFilter=ActiveFilter) then begin
    if FilterCount=1 then begin
      CurFilter.Clear;
      exit;
    end;
    if Index>0 then
      ActiveFilter:=Filters[0]
    else
      ActiveFilter:=Filters[1];
  end;
  FFilters.Delete(Index);
  CurFilter.Free;
end;

procedure TMessagesCtrl.ClearFilters;
var
  i: Integer;
begin
  for i:=FilterCount-1 downto 0 do begin
    if Filters[i]=ActiveFilter then continue;
    DeleteFilter(i);
  end;
  ActiveFilter.Clear;
end;

procedure TMessagesCtrl.Select(View: TLMsgWndView; LineNumber: integer;
  DoScroll, FullyVisible: boolean);
begin
  BeginUpdate;
  SelectedView:=View;
  SelectedLine:=LineNumber;
  if DoScroll then
    ScrollToLine(SelectedView,SelectedLine,FullyVisible);
  EndUpdate;
end;

procedure TMessagesCtrl.ScrollToLine(View: TLMsgWndView; LineNumber: integer;
  FullyVisible: boolean);
var
  y: Integer;
  MinScrollTop: integer;
  MaxScrollTop: Integer;
begin
  y:=GetLineTop(View,LineNumber,false);
  if FullyVisible then begin
    MinScrollTop:=Max(0,y+ItemHeight-ClientHeight);
    MaxScrollTop:=y;
  end else begin
    MinScrollTop:=Max(0,y-1-ClientHeight);
    MaxScrollTop:=y+ItemHeight-1;
  end;
  //debugln(['TMessagesCtrl.ScrollToLine ',LineNumber,' y=',y,' Min=',MinScrollTop,' Max=',MaxScrollTop]);
  y:=Max(Min(ScrollTop,MaxScrollTop),MinScrollTop);
  //debugln(['TMessagesCtrl.ScrollToLine y=',y,' ScrollTopMax=',ScrollTopMax]);
  ScrollTop:=y;
end;

function TMessagesCtrl.GetLineTop(View: TLMsgWndView; LineNumber: integer;
  Scrolled: boolean): integer;
var
  i: Integer;
  CurView: TLMsgWndView;
begin
  Result:=0;
  if View=nil then exit;
  for i:=0 to ViewCount-1 do begin
    CurView:=Views[i];
    if CurView=View then break;
    inc(Result,ItemHeight*CurView.GetShownLineCount(true,true));
  end;
  if LineNumber<0 then begin
    // header
  end else if LineNumber<View.Lines.Count then begin
    // normal messages
    inc(Result,(LineNumber+1)*ItemHeight);
  end else begin
    // last line
    inc(Result,(View.Lines.Count+1)*ItemHeight);
  end;
  if Scrolled then
    dec(Result,ScrollTop);
end;

constructor TMessagesCtrl.Create(AOwner: TComponent);
var
  u: TMessageLineUrgency;
begin
  inherited Create(AOwner);
  ControlStyle:=ControlStyle-[csCaptureMouse]+[csReflector];
  FOptions:=MCDefaultOptions;
  FFilters:=TFPList.Create;
  FActiveFilter:=TLMsgViewFilter.Create;
  FActiveFilter.Caption:=GetDefaultFilterCaption;
  FActiveFilter.OnChanged:=@OnFilterChanged;
  FFilters.Add(FActiveFilter);
  FViews:=TFPList.Create;
  FUpdateTimer:=TTimer.Create(Self);
  FUpdateTimer.Name:='MsgUpdateTimer';
  FUpdateTimer.Interval:=200;
  FUpdateTimer.OnTimer:=@MsgUpdateTimerTimer;
  FItemHeight:=20;
  FSelectedView:=nil;
  FSelectedLine:=-1;
  BorderWidth:=0;
  fBackgroundColor:=MCDefaultBackground;
  FHeaderBackground[lmvtsRunning]:=MCDefaultHeaderBackgroundRunning;
  FHeaderBackground[lmvtsSuccess]:=MCDefaultHeaderBackgroundSuccess;
  FHeaderBackground[lmvtsFailed]:=MCDefaultHeaderBackgroundFailed;
  FAutoHeaderBackground:=MCDefaultAutoHeaderBackground;
  TabStop := True;
  ParentColor := False;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
  for u:=Low(TMessageLineUrgency) to high(TMessageLineUrgency) do
    fUrgencyStyles[u]:=TMsgCtrlUrgencyStyle.Create(Self,u);
  ShowHint:=true;
end;

destructor TMessagesCtrl.Destroy;
var
  u: TMessageLineUrgency;
  i: Integer;
begin
  IdleConnected:=false;
  Images:=nil;
  ClearViews;

  FActiveFilter:=nil;
  for i:=0 to FFilters.Count-1 do
    TObject(FFilters[i]).Free;
  FreeAndNil(FFilters);

  FreeAndNil(FViews);
  FreeAndNil(FUpdateTimer);
  FreeAndNil(FImageChangeLink);
  for u:=Low(TMessageLineUrgency) to high(TMessageLineUrgency) do
    FreeAndNil(fUrgencyStyles[u]);
  inherited Destroy;
end;

procedure TMessagesCtrl.BeginUpdate;
begin
  inc(fUpdateLock);
end;

procedure TMessagesCtrl.EndUpdate;
begin
  if fUpdateLock=0 then
    raise Exception.Create('');
  dec(fUpdateLock);
end;

procedure TMessagesCtrl.EraseBackground(DC: HDC);
begin
  // everything is painted, so erasing the background is not needed
end;

function TMessagesCtrl.IndexOfView(View: TLMsgWndView): integer;
begin
  Result:=FViews.IndexOf(View);
end;

procedure TMessagesCtrl.ClearViews;
begin
  while ViewCount>0 do
    Views[0].Free;
end;

procedure TMessagesCtrl.RemoveView(View: TLMsgWndView);
begin
  if FViews.IndexOf(View)<0 then exit;
  FViews.Remove(View);
  View.FControl:=nil;
  View.OnChanged:=nil;
  if fLastSearchStartView=View then
    fLastSearchStartView:=nil;
  if SelectedView=View then
    SelectedView:=nil;
  UpdateScrollBar(true);
  Invalidate;
end;

function TMessagesCtrl.GetView(aCaption: string; CreateIfNotExist: boolean
  ): TLMsgWndView;
var
  i: Integer;
begin
  for i:=0 to ViewCount-1 do begin
    Result:=Views[i];
    if UTF8CompareStr(aCaption,Result.Caption)=0 then exit;
  end;
  if not CreateIfNotExist then
    exit(nil);
  Result:=TLMsgWndView.Create(Self);
  Result.FControl:=Self;
  Result.Caption:=aCaption;
  Result.Filter.Assign(ActiveFilter);
  FViews.Add(Result);
  FreeNotification(Result);
  Result.OnChanged:=@OnViewChanged;
end;

function TMessagesCtrl.GetLineAt(Y: integer; out View: TLMsgWndView; out
  Line: integer): boolean;
var
  i: Integer;
begin
  for i:=0 to ViewCount-1 do begin
    View:=Views[i];
    if View.FPaintStamp<>FPaintStamp then continue;
    if (View.fPaintTop>Y) or (View.fPaintBottom<Y) then continue;
    Line:=((Y-View.fPaintTop) div ItemHeight)-1;
    Result:=true;
    exit;
  end;
  View:=nil;
  Line:=-1;
  Result:=false;
end;

function TMessagesCtrl.ScrollLeftMax: integer;
begin
  Result:=0;
end;

function TMessagesCtrl.ScrollTopMax: integer;
var
  i: Integer;
  View: TLMsgWndView;
begin
  if fScrollTopMax<0 then begin
    fScrollTopMax:=0;
    for i:=0 to ViewCount-1 do begin
      View:=Views[i];
      inc(fScrollTopMax,View.GetShownLineCount(true,true)*ItemHeight);
    end;
    fScrollTopMax:=Max(0,fScrollTopMax-ClientHeight);
  end;
  Result:=fScrollTopMax;
end;

procedure TMessagesCtrl.StoreSelectedAsSearchStart;
begin
  fLastLoSearchText:=UTF8LowerCase(FSearchText);
  fLastSearchStartView:=FSelectedView;
  fLastSearchStartLine:=FSelectedLine;
end;

function TMessagesCtrl.OpenSelection: boolean;
var
  Msg: TMessageLine;
begin
  Result:=false;
  if not Assigned(OnOpenMessage) then exit;
  Msg:=GetSelectedMsg;
  if Msg=nil then exit;
  Result:=OnOpenMessage(Self,Msg);
end;

procedure TMessagesCtrl.CreateMarksForFile(aSynEdit: TSynEdit;
  aFilename: string; DeleteOld: boolean);
var
  i: Integer;
  Lines: TMessageLines;
  Line: TMessageLine;
begin
  if DeleteOld then
    SourceMarks.RemoveMarks(aSynEdit);
  for i:=0 to ViewCount-1 do begin
    Lines:=Views[i].Lines;
    for Line in Lines.EnumerateFile(aFilename,0,High(Integer)) do begin
      //debugln(['TMessagesCtrl.CreateMarksForFile ',GetLineText(Line)]);
      CreateSourceMark(Line,aSynEdit);
    end;
  end;
end;

function TMessagesCtrl.ApplySrcChanges(Changes: TETSrcChanges): boolean;
var
  i: Integer;
begin
  Result:=false;
  for i:=0 to ViewCount-1 do
    if Views[i].ApplySrcChanges(Changes) then
      Result:=true;
  if Result then
    Invalidate;
end;

function TMessagesCtrl.HasSelection: boolean;
var
  View: TLMsgWndView;
begin
  Result:=false;
  View:=SelectedView;
  if View=nil then exit;
  Result:=SelectedLine<View.GetShownLineCount(false,true);
end;

{ TMessagesFrame }

procedure TMessagesFrame.MsgCtrlPopupMenuPopup(Sender: TObject);

  procedure UpdateUnhideItems;
  var
    ExampleMsg: String;
    s: String;
    HideItem: TLMVFilterHideMsgType;
    Item: TMenuItem;
    i: Integer;
  begin
    // create one menuitem per filter item
    UnhideMsgTypeMenuItem.Visible:=MessagesCtrl.ActiveFilter.HideMsgTypeCount>0;
    for i:=0 to MessagesCtrl.ActiveFilter.HideMsgTypeCount-1 do begin
      Item:=UnhideMsgTypeMenuItem.Items[i];
      if Item=UnhideMsgTypeSeparatorMenuItem then begin
        Item:=TMenuItem.Create(UnhideMsgTypeMenuItem);
        Item.Tag:=i;
        Item.OnClick:=@UnhideMsgTypeClick;
        UnhideMsgTypeMenuItem.Insert(i,Item);
      end;
      HideItem:=MessagesCtrl.ActiveFilter.HideMsgTypes[i];
      s:=HideItem.SubTool;
      if HideItem.MsgID<>0 then
        s+='('+IntToStr(HideItem.MsgID)+')';
      ExampleMsg:=ExternalToolList.GetMsgExample(HideItem.SubTool,HideItem.MsgID);
      if ExampleMsg<>'' then
        s+=' '+ExampleMsg;
      Item.Caption:=s;
    end;
    // delete old menu items
    i:=MessagesCtrl.ActiveFilter.HideMsgTypeCount;
    while UnhideMsgTypeSeparatorMenuItem.MenuIndex>i do
      UnhideMsgTypeMenuItem.Delete(i);
  end;

  procedure UpdateFilterItems;
  var
    i: Integer;
    Filter: TLMsgViewFilter;
    Item: TMenuItem;
  begin
    for i:=0 to MessagesCtrl.FilterCount-1 do begin
      Filter:=MessagesCtrl.Filters[i];
      if SwitchFilterMenuItem.Items[i]=FilterSeparatorMenuItem then begin
        Item:=TMenuItem.Create(SwitchFilterMenuItem);
        Item.OnClick:=@OnSwitchFilterClick;
        SwitchFilterMenuItem.Insert(i,Item);
      end else
        Item:=SwitchFilterMenuItem.Items[i];
      Item.Caption:=Filter.Caption;
      Item.Checked:=Filter=MessagesCtrl.ActiveFilter;
    end;
    // delete old menu items
    i:=MessagesCtrl.FilterCount;
    while FilterSeparatorMenuItem.MenuIndex>i do
      SwitchFilterMenuItem.Delete(i);
  end;

  procedure UpdateQuickFixes(CurLine: TMessageLine);
  begin
    // delete old
    QuickFixesMenuItem.Clear;
    // create items
    if CurLine<>nil then begin
      IDEQuickFixes.SetMsgLines(CurLine);
      IDEQuickFixes.OnPopupMenu(QuickFixesMenuItem);
    end;
    QuickFixesMenuItem.Visible:=QuickFixesMenuItem.Count>0;
  end;

var
  HasText: Boolean;
  View: TLMsgWndView;
  HasFilename: Boolean;
  LineNumber: Integer;
  Line: TMessageLine;
  i: Integer;
  HasViewContent: Boolean;
  Running: Boolean;
  MsgType: String;
  CanHideMsgType: Boolean;
  MinUrgency: TMessageLineUrgency;
begin
  HasText:=false;
  HasFilename:=false;
  MsgType:='';
  CanHideMsgType:=false;
  Line:=nil;
  HasViewContent:=false;
  Running:=false;

  // check all
  for i:=0 to MessagesCtrl.ViewCount-1 do begin
    View:=MessagesCtrl.Views[i];
    if View.HasContent then
      HasViewContent:=true;
    if View.Running then
      Running:=true;
  end;

  // check selection
  View:=MessagesCtrl.SelectedView;
  if View<>nil then begin
    LineNumber:=MessagesCtrl.SelectedLine;
    if LineNumber>=0 then begin
      Line:=View.Lines[LineNumber];
      HasFilename:=Line.Filename<>'';
      HasText:=Line.Msg<>'';
      if (Line.SubTool<>'') and (Line.MsgID<>0) then begin
        MsgType:=Line.SubTool+' '+IntToStr(Line.MsgID);
        CanHideMsgType:=ord(Line.Urgency)<ord(mluError);
      end;
    end;

    InfoSeparatorMenuItem.Visible:=true;
    MsgAboutToolMenuItem.Caption:='About '+View.Caption;
    MsgAboutToolMenuItem.Visible:=true;
  end else begin
    InfoSeparatorMenuItem.Visible:=false;
    MsgAboutToolMenuItem.Visible:=false;
  end;

  if CanHideMsgType then begin
    HideMsgOfTypeMenuItem.Caption:='Hide all messages of type '+MsgType;
    HideMsgOfTypeMenuItem.Visible:=true;
  end else begin
    HideMsgOfTypeMenuItem.Visible:=false;
  end;
  MsgHideHintsWithoutPosMenuItem.Checked:=MessagesCtrl.ActiveFilter.HideNotesWithoutPos;

  MsgCopyMsgMenuItem.Enabled:=HasText;
  MsgCopyFilenameMenuItem.Enabled:=HasFilename;
  MsgCopyAllMenuItem.Enabled:=not Running;
  MsgCopyShownMenuItem.Enabled:=HasViewContent;
  MsgSaveAllToFileMenuItem.Enabled:=not Running;
  MsgSaveShownToFileMenuItem.Enabled:=HasViewContent;

  MinUrgency:=MessagesCtrl.ActiveFilter.MinUrgency;
  MsgHideWarningsMenuItem.Checked:=MinUrgency in [mluError..mluPanic];
  MsgHideNotesMenuItem.Checked:=MinUrgency in [mluWarning,mluImportant];
  MsgHideHintsMenuItem.Checked:=MinUrgency=mluNote;
  MsgHideVerboseMenuItem.Checked:=MinUrgency=mluHint;
  MsgHideDebugMenuItem.Checked:=MinUrgency in [mluProgress..mluVerbose];
  MsgHideNoneMenuItem.Checked:=MinUrgency=mluNone;

  MsgFileStyleShortMenuItem.Checked:=MessagesCtrl.FilenameStyle=mcfsShort;
  MsgFileStyleRelativeMenuItem.Checked:=MessagesCtrl.FilenameStyle=mcfsRelative;
  MsgFileStyleFullMenuItem.Checked:=MessagesCtrl.FilenameStyle=mcfsFull;

  UpdateUnhideItems;
  UpdateFilterItems;

  UpdateQuickFixes(Line);
end;

procedure TMessagesFrame.OnSwitchFilterClick(Sender: TObject);
var
  Item: TMenuItem;
  Filter: TLMsgViewFilter;
begin
  Item:=Sender as TMenuItem;
  Filter:=MessagesCtrl.GetFilter(Item.Caption,false);
  if Filter=nil then exit;
  MessagesCtrl.ActiveFilter:=Filter;
end;

procedure TMessagesFrame.SaveAllToFileMenuItemClick(Sender: TObject);
begin
  SaveClicked(false);
end;

procedure TMessagesFrame.SaveShownToFileMenuItemClick(Sender: TObject);
begin
  SaveClicked(true);
end;

procedure TMessagesFrame.SearchEditChange(Sender: TObject);
var
  s: TCaption;
begin
  s:=SearchEdit.Text;
  if s=GetDefaultSearchText then
    s:='';
  MessagesCtrl.SearchText:=s;
end;

procedure TMessagesFrame.SearchEditEnter(Sender: TObject);
begin
  if SearchEdit.Text=GetDefaultSearchText then
    SearchEdit.Text:='';
end;

procedure TMessagesFrame.SearchEditExit(Sender: TObject);
begin
  if SearchEdit.Text='' then
    SearchEdit.Text:=GetDefaultSearchText;
end;

procedure TMessagesFrame.SearchNextSpeedButtonClick(Sender: TObject);
begin
  MessagesCtrl.SelectNextOccurence(true);
end;

procedure TMessagesFrame.SearchPrevSpeedButtonClick(Sender: TObject);
begin
  MessagesCtrl.SelectNextOccurence(false);
end;

procedure TMessagesFrame.ShowIDMenuItemClick(Sender: TObject);
begin
  if ShowIDMenuItem.Checked then
    MessagesCtrl.Options:=MessagesCtrl.Options+[mcoShowMessageID]
  else
    MessagesCtrl.Options:=MessagesCtrl.Options-[mcoShowMessageID];
end;

procedure TMessagesFrame.TranslateMenuItemClick(Sender: TObject);
begin
  if MsgTranslateMenuItem.Checked then
    MessagesCtrl.Options:=MessagesCtrl.Options+[mcoShowTranslated]
  else
    MessagesCtrl.Options:=MessagesCtrl.Options-[mcoShowTranslated];
end;

procedure TMessagesFrame.UnhideMsgTypeClick(Sender: TObject);
var
  i: PtrInt;
begin
  i:=TMenuItem(Sender).Tag;
  if i<MessagesCtrl.ActiveFilter.HideMsgTypeCount then
    MessagesCtrl.ActiveFilter.DeleteHideMsgType(i);
end;

function TMessagesFrame.AllMessagesAsString(const OnlyShown: boolean): String;
var
  s: String;
  Tool: TAbstractExternalTool;
  View: TLMsgWndView;
  j: Integer;
  i: Integer;
begin
  s:='';
  for i:=0 to MessagesCtrl.ViewCount-1 do begin
    View:=MessagesCtrl.Views[i];
    if OnlyShown or (View.Tool=nil) then begin
      // save shown messages
      if not View.HasContent then continue;
      s+=MessagesCtrl.GetHeaderText(View)+LineEnding;
      for j:=0 to View.Lines.Count-1 do
        s+=MessagesCtrl.GetLineText(View.Lines[j])+LineEnding;
    end else begin
      // save raw data
      if View.Running then continue;
      Tool:=View.Tool;
      Tool.EnterCriticalSection;
      try
        for j:=0 to Tool.WorkerOutput.Count-1 do
          s+=Tool.WorkerOutput[j]+LineEnding;
      finally
        Tool.LeaveCriticalSection;
      end;
    end;
  end;
  Result:=s;
end;

procedure TMessagesFrame.CopyMsgMenuItemClick(Sender: TObject);
begin
  CopyMsgToClipboard(false);
end;

procedure TMessagesFrame.CopyShownMenuItemClick(Sender: TObject);
begin
  CopyAllClicked(true);
end;

procedure TMessagesFrame.FileStyleMenuItemClick(Sender: TObject);
begin
  if Sender=MsgFileStyleShortMenuItem then
    MessagesCtrl.FilenameStyle:=mcfsShort
  else if Sender=MsgFileStyleRelativeMenuItem then
    MessagesCtrl.FilenameStyle:=mcfsRelative
  else if Sender=MsgFileStyleFullMenuItem then
    MessagesCtrl.FilenameStyle:=mcfsFull;
end;

procedure TMessagesFrame.FindMenuItemClick(Sender: TObject);
begin
  MessagesCtrl.StoreSelectedAsSearchStart;
  SearchPanel.Visible:=true;
  SearchEdit.SetFocus;
end;

procedure TMessagesFrame.HideHintsWithoutPosMenuItemClick(Sender: TObject);
begin
  MessagesCtrl.ActiveFilter.HideNotesWithoutPos:=MsgHideHintsWithoutPosMenuItem.Checked;
end;

procedure TMessagesFrame.HideMsgOfTypeMenuItemClick(Sender: TObject);
var
  Line: TMessageLine;
begin
  Line:=MessagesCtrl.GetSelectedMsg;
  if (Line=nil) or (ord(Line.Urgency)>=ord(mluError)) then exit;
  //debugln(['TMessagesFrame.HideMsgOfTypeMenuItemClick SubTool=',Line.SubTool,' MsgID=',Line.MsgID]);
  MessagesCtrl.ActiveFilter.AddHideMsgType(Line.SubTool,Line.MsgID);
end;

procedure TMessagesFrame.HideUrgencyMenuItemClick(Sender: TObject);
var
  MinUrgency: TMessageLineUrgency;
begin
  if Sender=MsgHideWarningsMenuItem then
    MinUrgency:=mluError
  else if Sender=MsgHideNotesMenuItem then
    MinUrgency:=mluWarning
  else if Sender=MsgHideHintsMenuItem then
    MinUrgency:=mluNote
  else if Sender=MsgHideVerboseMenuItem then
    MinUrgency:=mluHint
  else if Sender=MsgHideDebugMenuItem then
    MinUrgency:=mluVerbose3
  else if Sender=MsgHideNoneMenuItem then
    MinUrgency:=mluNone;
  MessagesCtrl.ActiveFilter.MinUrgency:=MinUrgency;
end;

procedure TMessagesFrame.HideSearchSpeedButtonClick(Sender: TObject);
begin
  SearchPanel.Visible:=false;
  MessagesCtrl.SearchText:='';
  SearchEdit.Text:=GetDefaultSearchText;
end;

procedure TMessagesFrame.MsgCtrlPopupMenuClose(Sender: TObject);
begin
  IDEQuickFixes.ClearLines;
end;

procedure TMessagesFrame.CopyFilenameMenuItemClick(Sender: TObject);
begin
  CopyMsgToClipboard(true);
end;

procedure TMessagesFrame.CopyAllMenuItemClick(Sender: TObject);
begin
  CopyAllClicked(false);
end;

procedure TMessagesFrame.AboutToolMenuItemClick(Sender: TObject);
var
  View: TLMsgWndView;
  Form: TForm;
  s: String;
  Tool: TAbstractExternalTool;
  Proc: TProcessUTF8;
  Memo: TMemo;
begin
  View:=MessagesCtrl.SelectedView;
  if View=nil then exit;

  s:=View.Caption+LineEnding;
  s+=LineEnding;
  Tool:=View.Tool;
  if Tool<>nil then begin
    Proc:=Tool.Process;
    if Proc<>nil then begin
      if Proc.Executable<>'' then
        s+='Executable: '+LineEnding+Proc.Executable+LineEnding+LineEnding;
      if Proc.CurrentDirectory<>'' then
        s+='CurrentDirectory: '+LineEnding+Proc.CurrentDirectory+LineEnding+LineEnding;
      if Proc.Desktop<>'' then
        s+='Desktop: '+Proc.Desktop+LineEnding;
      s+='Parameters:'+LineEnding;
      s+=Proc.Parameters.Text+LineEnding;
      s+='ProcessID:'+LineEnding+IntToStr(Proc.ProcessID)+LineEnding+LineEnding;
      if Tool.Terminated then
        s+='Terminated'+LineEnding+LineEnding
      else
        s+='Exit Status:'+LineEnding+IntToStr(Proc.ExitStatus)+LineEnding+LineEnding;
    end;
    if Tool.ErrorMessage<>'' then
      s+='Error: '+Tool.ErrorMessage+LineEnding+LineEnding;
  end;

  Form:=TForm.CreateNew(Self);
  try
    with Form do begin
      Name:='AboutExtToolDlg';
      Position:=poMainFormCenter;
      Caption:='About '+View.Caption;
    end;

    Memo:=TMemo.Create(Form);
    with Memo do begin
      Name:='Memo';
      Lines.Text:=s;
      Align:=alClient;
      WordWrap:=false;
      ScrollBars:=ssBoth;
      Parent:=Form;
    end;
    Form.ShowModal;
  finally
    Form.Free;
  end;
end;

procedure TMessagesFrame.AddFilterMenuItemClick(Sender: TObject);
var
  aCaption: String;
  i: Integer;
  NewFilter: TLMsgViewFilter;
begin
  aCaption:='Filter';
  i:=1;
  while MessagesCtrl.GetFilter(aCaption+IntToStr(i),false)<>nil do
    inc(i);
  aCaption:=UTF8Trim(InputBox('Create Filter','Name:',aCaption),[]);
  if aCaption='' then exit;
  if MessagesCtrl.GetFilter(aCaption,false)<>nil then begin
    // ToDo: use IDEMessageDlg
    MessageDlg('Filter already exists','A filter with the name "'+aCaption+'" already exists.',mtError,[mbCancel],'');
    exit;
  end;
  NewFilter:=MessagesCtrl.GetFilter(aCaption,true);
  NewFilter.Assign(MessagesCtrl.ActiveFilter);
  MessagesCtrl.ActiveFilter:=NewFilter;
end;

procedure TMessagesFrame.ClearHideMsgTypesMenuItemClick(Sender: TObject);
begin
  MessagesCtrl.ActiveFilter.ClearHideMsgTypes;
end;

function TMessagesFrame.GetViews(Index: integer): TLMsgWndView;
begin
  Result:=MessagesCtrl.Views[Index];
end;

procedure TMessagesFrame.SaveClicked(OnlyShown: boolean);
var
  Dlg: TSaveDialog;
  s: String;
  Filename: String;
  fs: TFileStreamUTF8;
begin
  Dlg:=TSaveDialog.Create(nil);
  try
    // ToDo: initfiledialog
    Dlg.Title:='Save messages';
    Dlg.FileName:='messages.txt';
    Dlg.Options:=Dlg.Options+[ofPathMustExist,ofCreatePrompt];
    if not Dlg.Execute then exit;
    Filename:=TrimAndExpandFilename(Dlg.FileName);
    if DirPathExistsCached(Filename) then exit;

    s:=AllMessagesAsString(OnlyShown);

    try
      fs:=TFileStreamUTF8.Create(Filename,fmCreate);
      try
        if s<>'' then
          fs.Write(s[1],length(s));
      finally
        fs.Free;
      end;
    except
      on E: Exception do begin
        // ToDo: idemessagedlg
        MessageDlg('Write Error','Unable to write file "'+Filename+'".',mtError,[mbCancel],0);
      end;
    end;

  finally
    Dlg.Free;
  end;
end;

procedure TMessagesFrame.CopyAllClicked(OnlyShown: boolean);
var
  s: String;
  Msg: String;
begin
  s:=AllMessagesAsString(OnlyShown);
  if length(s)>1000000 then begin
    if length(s)<10000 then
      Msg:=IntToStr(length(s))+' byte'
    else if Length(s)<10000000 then
      Msg:=IntToStr(length(s) div 1000)+' KB'
    else
      Msg:=IntToStr(length(s) div 1000)+' MB';
    // ToDo: replace with IDEMessageDlg
    if MessageDlg('Warning','This will put a lot of text ('+Msg+') on the clipboard.'#13'Proceed?',
      mtConfirmation,[mbYes,mbNo],0)<>mrYes then exit;
  end;
  Clipboard.AsText:=s;
end;

procedure TMessagesFrame.CopyMsgToClipboard(OnlyFilename: boolean);
var
  View: TLMsgWndView;
  LineNumber: Integer;
  Txt: String;
  Line: TMessageLine;
begin
  View:=MessagesCtrl.SelectedView;
  if View=nil then exit;
  LineNumber:=MessagesCtrl.SelectedLine;
  if LineNumber<0 then begin
    // header
    if OnlyFilename then exit;
    Txt:=MessagesCtrl.GetHeaderText(View);
  end else if LineNumber<View.Lines.Count then begin
    // normal messages
    Line:=View.Lines[LineNumber];
    if OnlyFilename then
      Txt:=Line.Filename
    else
      Txt:=MessagesCtrl.GetLineText(Line);
  end else if LineNumber=View.Lines.Count then begin
    // progress line
    Line:=View.ProgressLine;
    if OnlyFilename then
      Txt:=Line.Filename
    else
      Txt:=MessagesCtrl.GetLineText(Line);
  end else
    exit;
  Clipboard.AsText:=Txt;
end;

procedure TMessagesFrame.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then begin
    if AComponent=MessagesCtrl then
      MessagesCtrl:=nil;
  end;
end;

constructor TMessagesFrame.Create(TheOwner: TComponent);
var
  ImgIDInfo: Integer;
  ImgIDHint: Integer;
  ImgIDNote: Integer;
  ImgIDWarning: Integer;
  ImgIDError: Integer;
  ImgIDFatal: Integer;
begin
  inherited Create(TheOwner);

  MessagesCtrl:=TMessagesCtrl.Create(Self);
  ImgIDInfo:=IDEImages.LoadImage(12, 'state12x12_information');
  ImgIDHint:=IDEImages.LoadImage(12, 'state12x12_hint');
  ImgIDNote:=IDEImages.LoadImage(12, 'state12x12_note');
  ImgIDWarning:=IDEImages.LoadImage(12, 'state12x12_warning');
  ImgIDError:=IDEImages.LoadImage(12, 'state12x12_error');
  ImgIDFatal:=IDEImages.LoadImage(12, 'state12x12_fatal');
  with MessagesCtrl do begin
    Name:='MessagesCtrl';
    Align:=alClient;
    Parent:=Self;

    UrgencyStyles[mluNone].SetValues('?',ImgIDInfo,clDefault);
    UrgencyStyles[mluProgress].SetValues('Fortschritt',ImgIDInfo,clDefault);
    UrgencyStyles[mluDebug].SetValues('Debug',ImgIDInfo,clDefault);
    UrgencyStyles[mluVerbose3].SetValues('ExtAusfuehrlich',ImgIDInfo,clDefault);
    UrgencyStyles[mluVerbose2].SetValues('SehrAusfuerhlich',ImgIDInfo,clDefault);
    UrgencyStyles[mluVerbose].SetValues('Ausfuerhlich',ImgIDInfo,clDefault);
    UrgencyStyles[mluHint].SetValues('Hinweis',ImgIDHint,clDefault);
    UrgencyStyles[mluNote].SetValues('Note',ImgIDNote,clDefault);
    UrgencyStyles[mluWarning].SetValues('Warnung',ImgIDWarning,clDefault);
    UrgencyStyles[mluImportant].SetValues('Sonstiges',ImgIDInfo,clDefault);
    UrgencyStyles[mluError].SetValues('Fehler',ImgIDError,clDefault);
    UrgencyStyles[mluFatal].SetValues('Fatal',ImgIDFatal,clDefault);
    UrgencyStyles[mluPanic].SetValues('Panik',ImgIDFatal,clDefault);
    Images:=IDEImages.Images_12;
    PopupMenu:=MsgCtrlPopupMenu;
  end;

  // search
  SearchPanel.Visible:=false; // by default the search is hidden
  HideSearchSpeedButton.Hint:='Hide Search';
  HideSearchSpeedButton.LoadGlyphFromLazarusResource('debugger_power_grey');
  SearchEdit.Text:=GetDefaultSearchText;
  SearchNextSpeedButton.Hint:='Find the next occurence of the phrase';
  SearchNextSpeedButton.LoadGlyphFromLazarusResource('callstack_bottom');
  SearchPrevSpeedButton.Hint:='Find the previous occurence of the phrase';
  SearchPrevSpeedButton.LoadGlyphFromLazarusResource('callstack_top');
end;

destructor TMessagesFrame.Destroy;
begin
  MessagesCtrl.BeginUpdate;
  ClearViews;
  inherited Destroy;
end;

function TMessagesFrame.ViewCount: integer;
begin
  Result:=MessagesCtrl.ViewCount;
end;

function TMessagesFrame.GetView(aCaption: string; CreateIfNotExist: boolean
  ): TLMsgWndView;
begin
  Result:=MessagesCtrl.GetView(aCaption,CreateIfNotExist);
end;

function TMessagesFrame.FindUnfinishedView: TLMsgWndView;
begin
  Result:=MessagesCtrl.FindUnfinishedView;
end;

procedure TMessagesFrame.DeleteView(View: TLMsgWndView);
begin
  View.Free;
end;

function TMessagesFrame.IndexOfView(View: TLMsgWndView): integer;
begin
  Result:=MessagesCtrl.IndexOfView(View);
end;

procedure TMessagesFrame.ClearViews;
begin
  MessagesCtrl.ClearViews;
end;

procedure TMessagesFrame.CreateMarksForFile(aSynEdit: TSynEdit;
  aFilename: string; DeleteOld: boolean);
begin
  MessagesCtrl.CreateMarksForFile(aSynEdit,aFilename,DeleteOld);
end;

procedure TMessagesFrame.ApplySrcChangeds(Changes: TETSrcChanges);
begin
  MessagesCtrl.ApplySrcChanges(Changes);
end;

procedure TMessagesFrame.SelectMsgLine(Msg: TMessageLine; DoScroll: boolean);
begin
  MessagesCtrl.Select(Msg,DoScroll);
end;

function TMessagesFrame.GetDefaultSearchText: string;
begin
  Result:='(Search)';
end;

function TMessagesFrame.SelectFirstUrgentMessage(
  aMinUrgency: TMessageLineUrgency; WithSrcPos: boolean): boolean;
begin
  Result:=MessagesCtrl.SelectFirstUrgentMessage(aMinUrgency,WithSrcPos);
end;

function TMessagesFrame.SelectNextUrgentMessage(
  aMinUrgency: TMessageLineUrgency; WithSrcPos, Downwards: boolean): boolean;
begin
  Result:=MessagesCtrl.SelectNextUrgentMessage(aMinUrgency,WithSrcPos,Downwards);
end;

procedure TMessagesFrame.ClearCustomMessages(const ViewCaption: string);
var
  View: TLMsgWndView;
begin
  View:=GetView(ViewCaption,false);
  if (View=nil) or (View.Lines.Count=0) then exit;
  View.Lines.Clear;
  MessagesCtrl.UpdateScrollBar(true);
  MessagesCtrl.Invalidate;
end;

function TMessagesFrame.AddCustomMessage(TheUrgency: TMessageLineUrgency;
  Msg: string; aFilename: string; LineNumber: integer; Column: integer;
  const ViewCaption: string): TMessageLine;
var
  View: TLMsgWndView;
begin
  Result:=nil;
  View:=GetView(ViewCaption,true);
  Result:=View.Lines.CreateLine(-1);
  Result.Msg:=Msg;
  Result.Urgency:=TheUrgency;
  Result.SetSourcePosition(aFilename,LineNumber,Column);
  View.Lines.Add(Result);
  MessagesCtrl.UpdateScrollBar(true);
  MessagesCtrl.Invalidate;
end;

end.

