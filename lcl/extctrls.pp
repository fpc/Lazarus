{  $Id$  }
{
 /***************************************************************************
                               extctrls.pp
                               -----------
                             Component Library Extended Controls
                   Initial Revision  : Sat Jul 26 12:04:35 PDT 1999

 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

{
@abstract(Just a try to provide the same objects as the Delphi extctrls unit)
@author(TCustomNotebook, TNotebook - Curtis White <cwhite@aracnet.com>)
@author(TTimer - Stefan Hille (stoppok@osibisa.ms.sub.org))
@created(26 Jul 1999)
@lastmod(28 Jul 1999)

Extctrls contains only few class defintions at the moment and is very
incomplete.
}

unit ExtCtrls;

{$mode objfpc}
{$H+}

interface

{$ifdef Trace}
{$ASSERTIONS ON}
{$endif}

uses
  SysUtils, Classes, Controls, stdCtrls, vclGlobals, lMessages, GraphType,
  Graphics, LCLLinux;

type
  { workaround problem with fcl }
  TAbstractReader = TReader;
  
  { TTabPosition - Move to TTabbedNotebook when it is created }
  TTabPosition = (tpTop, tpBottom, tpLeft, tpRight);

  { TPage }
  {
    @abstract(Pages for Notebooks and TabbedNotebooks.)
    Introduced and (currently) maintained by Curtis White
  }


  TPage = class(TWinControl)
  private
    FImageIndex: integer;
    procedure SetImageIndex(const AValue: integer);
  protected
    procedure ReadState(Reader: TAbstractReader); override;
    //procedure Paint; override;
    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
  public
    procedure AddControl; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AdjustClientRect(var ARect: TRect); override;
  published
    property Caption;
    //property Height;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    // property TabOrder;     This property needs to be created in TWinControl
    property Visible;
    //property Width;
  end;

  TCustomNotebook = class;

  { TNBPages }
  {
    @abstract(Notebook page access class to provide access to notebook pages.)
    Introduced and (currently) maintained by Curtis White
  }
  TNBPages = class(TStrings)
  private
    fPageList: TList;
    fNotebook: TCustomNotebook;
  protected
    function Get(Index: Integer): String; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: String); override;
  public
    constructor Create(thePageList: TList; theNotebook: TCustomNotebook);
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Insert(Index: Integer; const S: String); override;
    procedure InsertPage(Index:integer; APage: TPage);

    procedure Move(CurIndex, NewIndex: Integer); override;
  end;

  { TCustomNotebook }
  {
    @abstract(Base class for TNotebook and TTabbedNotebook.)
    Introduced by Curtis White
  }
  TNoteBookOption = (nboShowCloseButtons);
  TNoteBookOptions = set of TNoteBookOption;
  
  TCustomNotebook = class(TCustomControl)
  private
    fAccess: TStrings; // TNBPages
    FImages: TImageList;
    FOnCloseTabClicked: TNotifyEvent;
    FOptions: TNoteBookOptions;
    fPageIndex: Integer;
    fPageList: TList;  // List of TPage
    //fMultiLine: boolean;
    fOnPageChanged: TNotifyEvent;
    fShowTabs: Boolean;
    fTabPosition: TTabPosition;

    Procedure CNNotify(var Message : TLMNotify); message CN_NOTIFY;
    function GetActivePage: String;
    function GetPage(aIndex: Integer): TPage;
    function GetPageCount : integer;
    function GetPageIndex: Integer;
    //function InternalSetMultiLine(Value: boolean): boolean;
    procedure SetActivePage(const Value: String);
    procedure SetImages(const AValue: TImageList);
    procedure SetOptions(const AValue: TNoteBookOptions);
    //procedure SetMultiLine(Value: boolean);
    procedure SetPageIndex(Value: Integer);
    procedure SetPages(Value: TStrings);
    procedure SetShowTabs(Value: Boolean);
    procedure SetTabPosition(tabPos: TTabPosition);
  protected
    procedure CreateParams(var Params: TCreateParams);override;
    procedure CreateWnd; override;
    procedure Change; virtual;
    function GetChildOwner: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ReadState(Reader: TAbstractReader); override;
    procedure ShowControl(AControl: TControl); override;
    procedure UpdateTabProperties; virtual;

    property ActivePage: String read GetActivePage write SetActivePage;
    //property MultiLine: boolean read fMultiLine write SetMultiLine default false;
    property Page[Index: Integer]: TPage read GetPage;
    property PageCount : integer read GetPageCount;
    property PageIndex: Integer read GetPageIndex write SetPageIndex default 0;
    property PageList: TList read fPageList;
    property Pages: TStrings read fAccess write SetPages;
    property OnPageChanged: TNotifyEvent read fOnPageChanged write fOnPageChanged;
    property ShowTabs: Boolean read fShowTabs write SetShowTabs;
    property TabPosition: TTabPosition read fTabPosition write SetTabPosition;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoCloseTabClicked(APage: TPage); virtual;
    property Images: TImageList read FImages write SetImages;
    property Name;
    property OnCloseTabClicked: TNotifyEvent
      read FOnCloseTabClicked write FOnCloseTabClicked;
    property Options: TNoteBookOptions read FOptions write SetOptions;
  end;

  { TNotebook }
  {
    @abstract(A Delphi style TNotebook.)
    Introduced and (currently) maintained by Curtis White
  }
  TNotebook = class(TCustomNotebook)
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Page;
    property PageCount;
    property Pages;
  published
    property ActivePage;
    property Images;
    property OnCloseTabClicked;
    //property MultiLine;
    property OnPageChanged;
    property Options;
    property PageIndex;
    property PageList;
    property ShowTabs;
  end;


  {
    @abstract(A free running timer.)
    Introduced and (currently) maintained by Stefan Hille (stoppok@osibisa.ms.sub.org)
  }
  TTimer = class (TComponent)
  private
    FInterval     : Cardinal;
    FTimerID      : integer;
    FOnTimer      : TNotifyEvent;
    FEnabled      : Boolean;
    procedure UpdateTimer;
    procedure SetEnabled(Value: Boolean);
    procedure SetInterval(Value: Cardinal);
    procedure SetOnTimer(Value: TNotifyEvent);
    procedure KillTimer;
  protected
    procedure Timer (var msg); message LM_Timer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Interval: Cardinal read FInterval write SetInterval default 1000;
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;

  TPaintBox = class(TGraphicControl)
  private
    FOnPaint: TNotifyEvent;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  published
    property Align;
//    property Anchors;
    property Color;
//    property Constraints;
//    property DragCursor;
//    property DragKind;
//    property DragMode;
//    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
//    property ParentShowHint;
//    property PopupMenu;
//    property ShowHint;
    property Visible;
    property OnClick;
//    property OnDblClick;
//    property OnDragDrop;
//    property OnDragOver;
//    property OnEndDock;
//    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
//    property OnStartDock;
//    property OnStartDrag;
  end;
  
  TImage = class(TGraphicControl)
  private
    FPicture: TPicture;
    procedure SetPicture(const AValue: TPicture);
    Procedure PictureChanged(SEnder : TObject);
  protected
  public
    constructor Create(AOwner: TComponent); override;
  published
    Property Align;
    property Picture : TPicture read FPicture write SetPicture;
    property Visible;
    property OnCLick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


  { TBevel }
  
  TBevelStyle = (bsLowered, bsRaised);
  TBevelShape=(bsBox, bsFrame, bsTopLine, bsBottomLine, bsLeftLine, bsRightLine);
  
  TBevel = Class(TGraphicControl)
  private
    FStyle:TBevelStyle;
    FShape:TBevelShape;
    Function GetStyle:TBevelStyle;
    Procedure SetStyle(aStyle:TBevelStyle);
    Function GetShape:TBevelShape;
    Procedure SetShape(aShape:TBevelShape);
  Protected
    Procedure Paint; Override;
  Public
    Constructor Create(AOwner:TComponent); override;
    Destructor Destroy; override;
    Procedure Invalidate; override;
  Published
    Property Height;
    Property Left;
    Property Name;
    Property Shape:TBevelShape Read GetShape Write SetShape Default bsBox;
    Property Top;
    Property Style:TBevelStyle Read GetStyle Write SetStyle Default bsLowered;
    Property Visible;
    Property Width;
  End;

  {
    @abstract(Base class for TRadioGroup.)
    (currently) maintained by Stefan Hille (stoppok@osibisa.ms.sub.org)
  }
  TCustomRadioGroup = class(TCustomGroupBox)
  private
    FButtonList : TList; // list of TRadioButton
    FCreatingWnd: boolean;
    FItems      : TStrings;
    FItemIndex  : integer;
    FColumns    : integer;
    FReading    : boolean;
    FOnClick    : TNotifyEvent;
    procedure ItemsChanged (Sender : TObject);
    procedure Clicked(Sender : TObject); virtual;
    procedure DoPositionButtons;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    function CanModify : boolean; virtual;
    procedure CreateWnd; override;
  protected
    procedure ReadState(Reader: TReader); override;
    procedure SetItem (value : TStrings);
    procedure SetColumns (value : integer);
    procedure SetItemIndex (value : integer);
    function GetItemIndex : integer;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;
    property ItemIndex : integer read GetItemIndex write SetItemIndex default -1;
    property Items : TStrings read FItems write SetItem;
    property Columns : integer read FColumns write SetColumns default 1;
    property OnClick : TNotifyEvent read FOnClick write FOnClick;
  end;

  {
    @abstract(Group of radiobuttons.)
    (currently) maintained by Stefan Hille (stoppok@osibisa.ms.sub.org)
  }
  TRadioGroup = class(TCustomRadioGroup)
  public
    constructor Create (AOwner : TComponent); override;
  published
    property Align;
    property Caption;
    property Enabled;
    property ItemIndex;
    property Items;
    property Columns;
    property Visible;
    property OnClick;
  end;

  TPanelBevel = TBevelCut;
  TBevelWidth = 1..Maxint;
  TBorderWidth = 0..Maxint;

  TCustomPanel = class(TCustomControl)
  private
    FBevelInner, FBevelOuter : TPanelBevel;
    FBevelWidth : TBevelWidth;
    FBorderWidth : TBorderWidth;
    FBorderStyle : TControlBorderStyle;
    FAlignment : TAlignment;
    FCaption : TCaption;
    procedure SetAlignment(const Value : TAlignment);
    procedure SetBevelInner(const Value: TPanelBevel);
    procedure SetBevelOuter(const Value: TPanelBevel);
    procedure SetBevelWidth(const Value: TBevelWidth);
    procedure SetBorderWidth(const Value: TBorderWidth);
    procedure SetBorderStyle(const Value: TControlBorderStyle);
  protected
    procedure AdjustClientRect(var Rect: TRect); override;
    function GetText: TCaption; override;
    procedure SetText(const Value: TCaption); override;
    procedure Paint; override;
    property Alignment: TAlignment read FAlignment write SetAlignment default taCenter;
    property BevelInner: TPanelBevel read FBevelInner write SetBevelInner default bvNone;
    property BevelOuter: TPanelBevel read FBevelOuter write SetBevelOuter default bvRaised;
    property BevelWidth: TBevelWidth read FBevelWidth write SetBevelWidth default 1;
    property BorderWidth: TBorderWidth read FBorderWidth write SetBorderWidth default 0;
    property BorderStyle: TControlBorderStyle read FBorderStyle write SetBorderStyle default bsNone;
    property Color default clBtnFace;
    property Caption read GetText write SetText;
    property ParentColor default True;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Invalidate; override;
  end;

  TPanel = class(TCustomPanel)
  published
    property Align default alNone;
    property Alignment;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color default clBackground;
    property DragMode;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default False;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;
    
    

const
  TCN_First = 0-550;
  TCN_SELCHANGE = TCN_FIRST - 1;

implementation

uses Interfaces;

{$I page.inc}
{$I customnotebook.inc}
{$I notebook.inc}
{$I timer.inc}
{$I paintbox.inc}
{$I customradiogroup.inc}
{$I custompanel.inc}
{$I radiogroup.inc}
{$I bevel.inc}
{$I image.inc}


end.

 {
  $Log$
  Revision 1.27  2002/08/28 11:41:53  lazarus
  MG: activated environment opts in debugger

  Revision 1.26  2002/07/27 15:38:01  lazarus
  MG: fixed search forward

  Revision 1.25  2002/06/08 17:16:02  lazarus
  MG: added close buttons and images to TNoteBook and close buttons to source editor

  Revision 1.24  2002/05/13 14:47:00  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.23  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.22  2002/04/22 13:07:45  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.21  2002/03/25 17:59:19  lazarus
  GTK Cleanup
  Shane

  Revision 1.20  2002/03/14 23:25:51  lazarus
  MG: fixed TBevel.Create and TListView.Destroy

  Revision 1.19  2002/03/13 22:48:16  lazarus
  Constraints implementation (first cut) and sizig - moving system rework to
  better match Delphi/Kylix way of doing things (the existing implementation
  worked by acident IMHO :-)

  Revision 1.18  2002/02/24 20:51:23  lazarus
  Improved TSpeedButton (Glyph, Spacing, Margin, drawing)
  Added PageCount to TNotebook
  Optimized component selection buttons a bit.

  Revision 1.17  2002/02/03 00:24:00  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.16  2002/01/01 15:50:13  lazarus
  MG: fixed initial component aligning

  Revision 1.15  2001/12/21 18:16:59  lazarus
  Added TImage class
  Shane

  Revision 1.14  2001/11/05 18:18:19  lazarus
  added popupmenu+arrows to notebooks, added target filename

  Revision 1.12  2001/06/26 21:44:32  lazarus
  MG: reduced paint messages

  Revision 1.11  2001/06/12 18:31:01  lazarus
  MG: small bugfixes

  Revision 1.10  2001/04/17 21:39:17  lazarus
  + added working OnClick support for TCustomRadiogroup, stoppok

  Revision 1.9  2001/04/06 22:28:09  lazarus
  * TTimer uses winapi interface now instead of sendmessage interface, stoppok

  Revision 1.8  2001/03/15 14:42:20  lazarus
  MG: customradiogroup is now streamable

  Revision 1.7  2001/01/12 18:27:31  lazarus
  Streaming additions by MAttias
  Shane

  Revision 1.6  2001/01/09 21:06:06  lazarus
  Started taking KeyDown messages in TDesigner
  Shane

  Revision 1.5  2001/01/09 18:23:20  lazarus
  Worked on moving controls.  It's just not working with the X and Y coord's I'm getting.
  Shane

  Revision 1.4  2001/01/05 18:56:23  lazarus
  Minor changes

  Revision 1.3  2001/01/04 20:33:53  lazarus
  Moved lresources.
  Moved CreateLFM to Main.pp
  Changed Form1 and TFOrm1 to MainIDE and TMainIDE
  Shane

  Revision 1.2  2000/12/29 15:04:07  lazarus
  Added more images to the resource.
  Shane

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.25  2000/06/29 21:06:14  lazarus
  reintroduced TAbstractReader=Treader hack, stoppok

  Revision 1.24  2000/06/28 13:11:37  lazarus
  Fixed TNotebook so it gets page change events.  Shane

  Revision 1.23  2000/05/08 23:59:52  lazarus
  Updated my email address in the documentation to the current one. Also
  removed email references in comments that were not @author comments to
  fix problems with the documentation produced by pasdoc.           CAW

  Revision 1.22  2000/02/26 23:31:50  lazarus
  MWE:
    Fixed notebook crash on insert
    Fixed loadfont problem for win32 (tleast now a fontname is required)

  Revision 1.21  2000/01/10 19:09:18  lazarus
  MWE:
    Removed temp hack TAbstractReader=TReader. It is now defined

  Revision 1.20  2000/01/10 00:07:12  lazarus
  MWE:
    Added more scrollbar support for TWinControl
    Most signals for TWinContorl are jet connected to the wrong widget
      (now scrolling window, should be fixed)
    Added some cvs entries

  Revision 1.19  2000/01/07 21:14:13  lazarus
  Added code for getwindowlong and setwindowlong.
  Shane

  Revision 1.18  2000/01/06 01:10:36  lazarus
  Stoppok:
     - changed ReadState to match current definition in fcl
       (affects TPage & TCustomNotebook)
     - added callback FItems.OnChanging to TCustomRadiogroup

  Revision 1.17  2000/01/02 00:22:54  lazarus
  stoppok:
    - introduced TBevel
    - enhanced TCustomRadioGroup

  Revision 1.16  1999/12/31 02:20:57  lazarus
    Initial implementation of TCustomRadioGroup / TRadioGroup
      stoppok

  Revision 1.15  1999/11/01 01:28:29  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.14  1999/10/22 21:01:50  lazarus

        Removed calls to InterfaceObjects except for controls.pp. Commented
        out any gtk depend lines of code.     MAH

  Revision 1.13  1999/10/19 19:16:51  lazarus
  renamed stdcontrols.pp stdctrls.pp
  Shane

  Revision 1.12  1999/10/04 23:36:25  lazarus
  Moved PageList and Page property to public to allow access to them.   CAW

  Revision 1.11  1999/09/30 21:59:01  lazarus
  MWE: Fixed TNoteBook problems
       Modifications: A few
       - Removed some debug messages
       + Added some others
       * changed fixed widged of TPage. Code is still broken.
       + TWinControls are also added to the Controls collection
       + Added TControl.Controls[] property

  Revision 1.10  1999/09/22 19:09:17  lazarus
  Added some trace info for the TNotebook problem.

  Revision 1.9  1999/09/21 23:46:54  lazarus
  *** empty log message ***

  Revision 1.8  1999/09/16 21:14:27  lazarus
    Some cleanups to the timer class. (moved some comments to timer.inc,
    added more comments and changed TTimer.Timer from function to procedure)
      Stoppok

  Revision 1.7  1999/09/13 03:27:10  lazarus
  Fixed a bug in the PageIndex property of TCustomNotebook where
  it was not tracking notebook pages if the user selected them
  with the mouse in a TTabbedNotebook.                               caw

  Revision 1.6  1999/08/26 23:36:02  peter
    + paintbox
    + generic keydefinitions and gtk conversion
    * gtk state -> shiftstate conversion

  Revision 1.5  1999/08/04 05:21:11  lazarus
  Created TCustomNotebook to allow both TNotebook and TTabbedNotebook to
  inherit from a common object. Made TNotebook work like Delphi TNotebook.

  Revision 1.3  1999/07/31 06:39:22  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
