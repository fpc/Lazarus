{ 
 /*************************************************************************** 
                               StdCtrls.pp
                               -----------
 
                   Initial Revision  : Tue Oct 19 CST 1999 
 
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
@author()
@created()
@lastmod()

Detailed description of the Unit.
}

unit StdCtrls;

{$mode objfpc}{$H+}

interface


uses
  VCLGlobals, Classes, SysUtils, Graphics, GraphType, LMessages, Controls,
  Forms;


type

  { TScrollBar }

  TScrollStyle = (ssNone, ssHorizontal, ssVertical, ssBoth,
    ssAutoHorizontal, ssAutoVertical, ssAutoBoth);

  TScrollCode = (scLineUp, scLineDown, scPageUp, scPageDown, scPosition,
                 scTrack, scTop, scBottom, scEndScroll);

  TScrollEvent = procedure(Sender: TObject; ScrollCode: TScrollCode;
                           var ScrollPos: Integer) of object;

  TScrollBar = class(TWinControl)
  private
    FKind: TScrollBarKind;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FPageSize: Integer;
    FRTLFactor: Integer;
    FSmallChange: TScrollBarInc;
    FLargeChange: TScrollBarInc;
    FOnChange: TNotifyEvent;
    FOnScroll: TScrollEvent;
    procedure DoScroll(var Message: TLMScroll);
    function NotRightToLeft: Boolean;
    procedure SetKind(Value: TScrollBarKind);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure CNHScroll(var Message: TLMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Message: TLMVScroll); message CN_VSCROLL;
    procedure CNCtlColorScrollBar(var Message: TLMessage); message CN_CTLCOLORSCROLLBAR;
    procedure WMEraseBkgnd(var Message: TLMEraseBkgnd); message LM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure Change; dynamic;
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetParams(APosition, AMin, AMax: Integer);
  published
    property Align;
    property Anchors;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Kind: TScrollBarKind read FKind write SetKind default sbHorizontal;
    property LargeChange: TScrollBarInc read FLargeChange write FLargeChange default 1;
    property Max: Integer read FMax write SetMax default 100;
    property Min: Integer read FMin write SetMin default 0;
    property PageSize: Integer read FPageSize write SetPageSize;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property Position: Integer read FPosition write SetPosition default 0;
    property ShowHint;
    property SmallChange: TScrollBarInc read FSmallChange write FSmallChange default 1;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
    property OnStartDrag;
  end;


  { TCustomGroupBox }

  TCustomGroupBox = class (TWinControl) {class(TCustomControl) }
  protected
  public
    constructor Create(AOwner : TComponent); Override;
  end;
  
  
  { TGroupBox }
                                                                                                   
  TGroupBox = class(TCustomGroupBox)
  published
    property Align;
    property Anchors;
    property Caption;
    property Visible;
  end;
  
  
  { TCustomComboBox }

  TComboBoxStyle = (csDropDown, csSimple, csDropDownList, csOwnerDrawFixed,
                    csOwnerDrawVariable);
  TCustomDrawItemState = (
    cdiSelected, cdiGrayed, cdiDisabled, cdiChecked,
    cdiFocused, cdiDefault, cdiHotLight, cdiInactive, cdiNoAccel,
    cdiNoFocusRect, cdiComboBoxEdit);
  
  TCustomDrawItemEvent = procedure(Control: TWinControl; Index: Integer;
    Rect: TRect; State: TCustomDrawItemState) of object;
  TMeasureItemEvent = procedure(Control: TWinControl; Index: Integer;
    var Height: Integer) of object;

  TCustomComboBox = class(TWinControl)
  private
    FAutoDropDown: Boolean;
    FDropDownCount: Integer;
    FItemHeight: integer;
    FItemIndex: integer;
    FItems: TStrings;
    fMaxLength: integer;
    FOnChange : TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FOnDrawItem: TCustomDrawItemEvent;
    FOnDropDown: TNotifyEvent;
    FOnMeasureItem: TMeasureItemEvent;
    FOnSelect: TNotifyEvent;
    FSelLength: integer;
    FSelStart: integer;
    FSorted : boolean;
    FStyle : TComboBoxStyle;
    procedure SetItems(Value : TStrings);
    procedure CNDrawItems(var Message : TLMDrawItems); message CN_DrawItem;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TCustomDrawItemState); virtual;
    procedure DoChange(var msg); message LM_CHANGED;
    procedure Change; dynamic;
    procedure Loaded; override;
    procedure Select; dynamic;
    procedure DropDown; dynamic;
    procedure CloseUp; dynamic;

    function GetItemCount: Integer; //override;
    function GetItemHeight: Integer; virtual;
    function GetSelLength : integer; virtual;
    function GetSelStart : integer; virtual;
    function GetSelText : string; virtual;
    function GetItemIndex : integer; virtual;
    function GetMaxLength : integer; virtual;
    procedure InitializeWnd; override;
    function SelectItem(const AnItem: String): Boolean;
    procedure SetDropDownCount(const AValue: Integer); virtual;
    procedure SetItemHeight(const AValue: Integer); virtual;
    procedure SetItemIndex(Val : integer); virtual;
    procedure SetMaxLength(Val : integer); virtual;
    procedure SetSelLength(Val : integer); virtual;
    procedure SetSelStart(Val : integer); virtual;
    procedure SetSelText(const Val : string); virtual;
    procedure SetSorted(Val : boolean); virtual;
    procedure SetStyle(Val : TComboBoxStyle); virtual;

    property DropDownCount: Integer read
      FDropDownCount write SetDropDownCount default 8;
    property Items: TStrings read FItems write SetItems;
    property ItemHeight: Integer read GetItemHeight write SetItemHeight;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property MaxLength: integer read GetMaxLength write SetMaxLength default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnDrawItem: TCustomDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnMeasureItem: TMeasureItemEvent
      read FOnMeasureItem write FOnMeasureItem;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
    property Sorted: boolean read FSorted write SetSorted;
    property Style: TComboBoxStyle read FStyle write SetStyle;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure AddItem(const Item: String; AObject: TObject); //override;
    procedure Clear; //override;
    procedure ClearSelection; //override;
    procedure MeasureItem(Index: Integer; var TheHeight: Integer); virtual;
    procedure SelectAll;
    property AutoDropDown: Boolean
      read FAutoDropDown write FAutoDropDown default False;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelText: String read GetSelText write SetSelText;
  end;
  
  
  { TComboBox }

  TComboBox = class(TCustomComboBox)
  public
    property ItemIndex;
  published
    property Anchors;
    property Enabled;
    property Items;
    property MaxLength;
    property Sorted;
    property Style;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
  end;
    

  { TCustomListBox }
    
  TListBoxStyle = (lbStandard, lbOwnerDrawFixed, lbOwnerDrawVariable);

  TCustomListBox = class(TWinControl)
  private
    FBorderStyle : TBorderStyle;
    FExtendedSelect, FMultiSelect : boolean;
    FItems : TStrings;
    FSorted : boolean;
    FStyle : TListBoxStyle;
    procedure UpdateSelectionMode;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
    function GetItemIndex : integer; virtual;
    function GetSelCount : integer;
    function GetSelected(Index : integer) : boolean;
    procedure SetBorderStyle(Val : TBorderStyle); virtual;
    procedure SetExtendedSelect(Val : boolean); virtual;
    procedure SetItemIndex(Val : integer); virtual;
    procedure SetItems(Value : TStrings); virtual;
    procedure SetMultiSelect(Val : boolean); virtual;
    procedure SetSelected(Index : integer; Val : boolean);
    procedure SetSorted(Val : boolean); virtual;
    procedure SetStyle(Val : TListBoxStyle); virtual;
    property BorderStyle : TBorderStyle read FBorderStyle write SetBorderStyle;
    property ExtendedSelect : boolean read FExtendedSelect write SetExtendedSelect;
    property Sorted : boolean read FSorted write SetSorted;
    property Style : TListBoxStyle read FStyle write SetStyle;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    property ItemIndex : integer read GetItemIndex write SetItemIndex;
    property Items : TStrings read FItems write SetItems;
    property MultiSelect : boolean read FMultiSelect write SetMultiSelect;
    property SelCount : integer read GetSelCount;
    property Selected[Index : integer] : boolean read GetSelected write SetSelected;
  end;
    
  TListBox = class(TCustomListBox)
  public
    property ItemIndex;
  published
    property Align;
    property Anchors;
    property BorderStyle;
    property ExtendedSelect;
    property Items;
    property MultiSelect;
    property OnClick;
    property OnDblClick;
    property OnEnter;
    property OnExit;
    property OnKeyPress;
    property OnKeyDown;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseUp;
    property OnResize;
    property Sorted;
    property Style;
    property Visible;
  end;    

  TEditCharCase = (ecNormal, ecUppercase, ecLowerCase);
  TEchoMode = (emNormal, emNone, emPassword);

  TCustomEdit = class(TWinControl)
  private
    FCharCase : TEditCharCase;
    FEchoMode : TEchoMode;
    FMaxLength : Integer;
    FModified : Boolean;
    FReadOnly : Boolean;
    FOnChange : TNotifyEvent;
    FSelLength : integer;
    FSelStart : integer;
    function GetModified : Boolean;
    procedure SetCharCase(Value : TEditCharCase);
    procedure SetMaxLength(Value : Integer);
    procedure SetModified(Value : Boolean);
    procedure SetReadOnly(Value : Boolean);
  protected
    procedure CMTextChanged(Var Message : TLMessage); message CM_TextChanged;
    procedure Change; dynamic;
    function GetSelLength : integer; virtual;
    function GetSelStart : integer; virtual;
    function GetSelText : string; virtual;
    procedure InitializeWnd; override;
    procedure SetEchoMode(Val : TEchoMode); virtual;
    procedure SetSelLength(Val : integer); virtual;
    procedure SetSelStart(Val : integer); virtual;
    procedure SetSelText(const Val : string); virtual;

    property OnChange : TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SelectAll;
    property CharCase : TEditCharCase read FCharCase write SetCharCase default ecNormal;
    property EchoMode : TEchoMode read FEchoMode write SetEchoMode default emNormal;
    property MaxLength : Integer read FMaxLength write SetMaxLength default 0;
    property ReadOnly : Boolean read FReadOnly write SetReadOnly default false;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelText: String read GetSelText write SetSelText;
    property Modified : Boolean read GetModified write SetModified;
    property Text;
  published
    property TabStop default true;
  end;

  TCustomMemo = class(TCustomEdit)
  private
    FFont : TFont;
    FLines: TStrings;
    FScrollBars: TScrollStyle;
    FWordWrap: Boolean;
  protected
    procedure SetLines(Value : TStrings);
    procedure SetWordWrap(const Value : boolean);
    procedure SetScrollBars(const Value : TScrollStyle);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Append(const Value : String);
    property Lines: TStrings read FLines write SetLines;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
    property Font : TFont read FFont write FFont;
  end;

  TEdit = class(TCustomEdit)
  published
    property Anchors;
    property CharCase;
    property DragMode;
    property EchoMode;
    property MaxLength;
    property PopupMenu;
    property ReadOnly;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnEnter;
    property OnExit;
  end;

  TMemo = class(TCustomMemo)
  published
    property Align;
    property Anchors;
    property Color;
    property Font;
    property Lines;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property Tabstop;
    property Visible;
    property WordWrap;
    property OnChange;
    property OnEnter;
    property OnExit;
  end;

  { TCustomLabel }

  TCustomLabel = class(TWinControl)
  private
    FAlignment : TAlignment;
    FWordWrap : Boolean;
    FLayout : TTextLayout;
    FFocusControl : TWinControl;
    FShowAccelChar : boolean;
    procedure SetAlignment(Value : TAlignment);
    procedure SetLayout(Value : TTextLayout);
    procedure SetWordWrap(Value : Boolean);
    procedure WMActivate(var Message: TLMActivate); message LM_ACTIVATE;
  protected
    function GetLabelText: String ; virtual;
    procedure Notification(AComponent : TComponent; Operation : TOperation); override;
    procedure SetFocusControl(Val : TWinControl); virtual;
    procedure SetShowAccelChar(Val : boolean); virtual;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property FocusControl : TWinControl read FFocusControl write SetFocusControl;
    property Layout: TTextLayout read FLayout write SetLayout default tlBottom;
    property ShowAccelChar : boolean read FShowAccelChar write SetShowAccelChar default true;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default false;
  public
    constructor Create(AOwner : TComponent); override;
  end;


  { TLabel }

  TLabel = class(TCustomLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property Caption;
    property Color;
    property FocusControl;
    property Font;
    property Visible;
    property Layout;
    property ShowAccelChar;
    property WordWrap;
  end;


  { TButtonControl }

  TButtonControl = class(TWinControl)
  private
    FClicksDisabled: Boolean;
    function IsCheckedStored: boolean;
  protected
    function GetChecked: Boolean; virtual;
    procedure SetChecked(Value: Boolean); virtual;
    property Checked: Boolean read GetChecked write SetChecked stored IsCheckedStored default False;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  { TCHECKBOX }

  // ToDo: delete TLeftRight when in classesh.inc
  TLeftRight = taLeftJustify..taRightJustify;

  TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);

  TCustomCheckBox = class(TButtonControl)
  private
    // FAlignment: TLeftRight;
    FAllowGrayed: Boolean;
    FState: TCheckBoxState;
    procedure SetState(Value: TCheckBoxState);
    function GetState : TCheckBoxState;
  protected
    procedure InitializeWnd; override;
    procedure Toggle; virtual;
    function GetChecked: Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure ApplyChanges; virtual;
    property AllowGrayed: Boolean read FAllowGrayed write FAllowGrayed;
    property State: TCheckBoxState read GetState write SetState;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  TCheckBox = class(TCustomCheckBox)
  protected
    procedure DoAutoSize; Override;
    procedure SetText(const Value: TCaption); Override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AutoSize;
    property AllowGrayed;
    property Anchors;
    property Caption;
    property Checked;
    property State;
    property Visible;
    property Enabled;
    property OnEnter;
    property OnExit;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  TToggleBox = class(TCustomCheckBox)
  private
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AllowGrayed;
    property Anchors;
    property Caption;
    property Checked;
    property State;
    property Visible;
    property Enabled;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

  {TRadioButton}
   
  TRadioButton = class(TCustomCheckBox)
  protected
    procedure DoAutoSize; override;
    procedure SetText(const Value: TCaption); override;
  public
    constructor Create (AOwner: TComponent); override;
  published
    property Anchors;
    property AutoSize;
    property AllowGrayed;
    property Caption;
    property Checked;
    property State;
    property Visible;
    property Enabled;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Hint;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

Function DeleteAmpersands(var Str : String) : Longint;

implementation

uses LCLLinux, LCLType, Interfaces;


type
   TSelection = record
      Startpos, EndPos: Integer;
   end;

   TMemoStrings = class(TStrings)
   private
      FMemo: TCustomMemo;
   protected
      function Get(Index : Integer): String; override;
      function GetCount: Integer; override;
   public
      constructor Create(AMemo: TCustomMemo);
      procedure Clear; override;
      procedure Delete(index : Integer); override;
      procedure Insert(index: Integer; const S: String); override;
   end;

 { TComboBoxStrings = class(TStrings)
  private
    ComboBox: TCustomComboBox;
  protected
    function Get(Index: Integer): string; override;
    function GetCount: Integer; override;
    function GetObject(Index: Integer): TObject; override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
    procedure SetUpdateState(Updating: Boolean); override;
  public
    function Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    function IndexOf(const S: string): Integer; override;
    procedure Insert(Index: Integer; const S: string); override;
  end;
    }

const
  SScrollBarRange = 'ScrollBar property out of range';

Function DeleteAmpersands(var Str : String) : Longint;
var
  I : Integer;
  Tmp : String;
begin
  I := 1;
  Result := -1;
  SetLength(Tmp,0);
  While I <= Length(Str) do
    Case Str[I] of
      '&' :
         If I + 1 <= Length(Str) then begin
           If Str[I+1] = '&' then begin
             Inc(I,2);
             Tmp := Tmp + '&';
           end
           else begin
             If Result  < 0 then
               Result := Length(Tmp) + 1;
             Inc(I,1);
           end;
         end
         else
           Inc(I,1);
      else begin
        Tmp := Tmp + Str[I];
        Inc(I,1);
      end;
    end;
  SetLength(Str,0);
  Str := Tmp;
end;

{$I customgroupbox.inc}
{$I customcombobox.inc}                                                                                            
{$I customlistbox.inc}
{$I custommemo.inc}
{$I customedit.inc}
{$I customlabel.inc}
{$I customcheckbox.inc}

{$I scrollbar.inc} 
{$I memo.inc}
{$I memostrings.inc}

{$I edit.inc}
{$I buttoncontrol.inc}

{$I checkbox.inc}
{$I radiobutton.inc}
{$I togglebox.inc}

end.

{ =============================================================================

  $Log$
  Revision 1.45  2002/09/18 17:07:24  lazarus
  MG: added patch from Andrew

  Revision 1.44  2002/09/09 07:26:42  lazarus
  MG: started TCollectionPropertyEditor

  Revision 1.43  2002/09/08 19:09:55  lazarus
  Fixed and simplified TRadioButton

  Revision 1.42  2002/09/07 12:14:50  lazarus
  EchoMode for TCustomEdit. emNone not implemented for GTK+, falls back to emPassword
  behaviour.

  Revision 1.41  2002/09/05 10:12:06  lazarus

  New dialog for multiline caption of TCustomLabel.
  Prettified TStrings property editor.
  Memo now has automatic scrollbars (not fully working), WordWrap and Scrollbars property
  Removed saving of old combo text (it broke things and is not needed). Cleanups.

  Revision 1.40  2002/09/03 11:32:49  lazarus

  Added shortcut keys to labels
  Support for alphabetically sorting the properties
  Standardize message and add shortcuts ala Kylix
  Published BorderStyle, unpublished BorderWidth
  ShowAccelChar and FocusControl
  ShowAccelChar and FocusControl for TLabel, escaped ampersands now work.

  Revision 1.39  2002/09/03 08:07:19  lazarus
  MG: image support, TScrollBox, and many other things from Andrew

  Revision 1.38  2002/08/30 06:46:03  lazarus

  Use comboboxes. Use history. Prettify the dialog. Preselect text on show.
  Make the findreplace a dialog. Thus removing resiying code (handled by Anchors now anyway).
  Make Anchors work again and publish them for various controls.
  SelStart and Co. for TEdit, SelectAll procedure for TComboBox and TEdit.
  Clean up and fix some bugs for TComboBox, plus selection stuff.

  Revision 1.37  2002/08/27 18:45:13  lazarus
  MG: propedits text improvements from Andrew, uncapturing, improved comobobox

  Revision 1.36  2002/08/27 14:33:37  lazarus
  MG: fixed designer component deletion

  Revision 1.35  2002/08/25 13:31:35  lazarus
  MG: replaced C-style operators

  Revision 1.34  2002/08/24 06:51:22  lazarus
  MG: from Andrew: style list fixes, autosize for radio/checkbtns

  Revision 1.33  2002/08/19 20:34:47  lazarus
  MG: improved Clipping, TextOut, Polygon functions

  Revision 1.32  2002/08/17 15:45:32  lazarus
  MG: removed ClientRectBugfix defines

  Revision 1.31  2002/07/23 07:40:51  lazarus
  MG: fixed get widget position for inherited gdkwindows

  Revision 1.30  2002/05/20 14:19:03  lazarus
  MG: activated the clientrect bugfixes

  Revision 1.29  2002/05/13 14:47:00  lazarus
  MG: fixed client rectangles, TRadioGroup, RecreateWnd

  Revision 1.28  2002/05/10 06:05:50  lazarus
  MG: changed license to LGPL

  Revision 1.27  2002/05/09 12:41:28  lazarus
  MG: further clientrect bugfixes

  Revision 1.26  2002/04/22 13:07:45  lazarus
  MG: fixed AdjustClientRect of TGroupBox

  Revision 1.25  2002/04/21 06:53:54  lazarus
  MG: fixed save lrs to test dir

  Revision 1.24  2002/04/18 08:09:03  lazarus
  MG: added include comments

  Revision 1.23  2002/04/18 07:53:08  lazarus
  MG: fixed find declaration of forward def class

  Revision 1.22  2002/03/25 17:59:19  lazarus
  GTK Cleanup
  Shane

  Revision 1.21  2002/02/20 23:33:24  lazarus
  MWE:
    + Published OnClick for TMenuItem
    + Published PopupMenu property for TEdit and TMemo (Doesn't work yet)
    * Fixed debugger running twice
    + Added Debugger output form
    * Enabled breakpoints

  Revision 1.20  2002/02/03 00:24:01  lazarus
  TPanel implemented.
  Basic graphic primitives split into GraphType package, so that we can
  reference it from interface (GTK, Win32) units.
  New Frame3d canvas method that uses native (themed) drawing (GTK only).
  New overloaded Canvas.TextRect method.
  LCLLinux and Graphics was split, so a bunch of files had to be modified.

  Revision 1.19  2002/01/09 22:49:25  lazarus
  MWE: Converted to Unix fileformat

  Revision 1.18  2002/01/09 22:47:29  lazarus
  MWE: published OnClick for checkbox family

  Revision 1.17  2001/12/07 20:12:15  lazarus
  Added a watch dialog.
  Shane

  Revision 1.16  2001/10/19 14:27:43  lazarus
  MG: fixed customradiogroup OnClick + ItemIndex

  Revision 1.15  2001/06/14 14:57:58  lazarus
  MG: small bugfixes and less notes

  Revision 1.14  2001/03/27 21:12:53  lazarus
  MWE:
    + Turned on longstrings
    + modified memotest to add lines

  Revision 1.13  2001/02/02 14:23:38  lazarus
  Start of code completion code.
  Shane

  Revision 1.12  2001/02/01 16:45:19  lazarus
  Started the code completion.
  Shane

  Revision 1.11  2001/01/28 21:06:07  lazarus
  Changes for TComboBox events KeyPress Focus.
  Shane

  Revision 1.10  2001/01/11 20:16:47  lazarus
  Added some TImageList code.
  Added a bookmark resource with 10 resource images.
  Removed some of the IFDEF's in mwCustomEdit around the inherited code.
  Shane

  Revision 1.8  2001/01/05 17:44:37  lazarus
  ViewUnits1, ViewForms1 and MessageDlg are all loaded from their resources and all controls are auto-created on them.
  There are still a few problems with some controls so I haven't converted all forms.
  Shane

  Revision 1.7  2001/01/04 15:09:05  lazarus
  Tested TCustomEdit.Readonly, MaxLength and CharCase.
  Shane

  Revision 1.6  2001/01/04 13:52:00  lazarus
  Minor changes to TEdit.
  Not tested.
  Shane

  Revision 1.5  2000/12/29 15:04:07  lazarus
  Added more images to the resource.
  Shane

  Revision 1.4  2000/12/01 15:50:39  lazarus
  changed the TCOmponentInterface SetPropByName.  It works for a few properties, but not all.
  Shane

  Revision 1.3  2000/11/29 21:22:35  lazarus
  New Object Inspector code
  Shane

  Revision 1.2  2000/07/16 12:45:01  lazarus
  Added procedure ListBox.Clear (changes by chris, added by stoppok)

  Revision 1.1  2000/07/13 10:28:24  michael
  + Initial import

  Revision 1.28  2000/07/09 20:41:20  lazarus
  Added Attachsignals method to custombobobox, stoppok

  Revision 1.27  2000/06/29 21:07:08  lazarus
  some more published properties for combobox, stoppok

  Revision 1.26  2000/06/24 21:30:19  lazarus
  *** empty log message ***

  Revision 1.25  2000/06/16 13:33:21  lazarus
  Created a new method for adding controls to the toolbar to be dropped onto the form!
  Shane

  Revision 1.24  2000/05/30 22:28:41  lazarus
  MWE:
    Applied patches from Vincent Snijders:
    + Added GetWindowRect
    * Fixed horz label alignment
    + Added vert label alignment

  Revision 1.23  2000/05/08 12:54:19  lazarus
  Removed some writeln's
  Added alignment for the TLabel.  Isn't working quite right.
  Added the shell code for WindowFromPoint and GetParent.
  Added FindLCLWindow
  Shane

  Revision 1.22  2000/04/18 20:06:39  lazarus
  Added some functions to Compiler.pp

  Revision 1.21  2000/04/13 21:25:16  lazarus
  MWE:
    ~ Added some docu and did some cleanup.
  Hans-Joachim Ott <hjott@compuserve.com>:
    * TMemo.Lines works now.
    + TMemo has now a property Scrollbar.
    = TControl.GetTextBuf revised :-)
    + Implementation for CListBox columns added
    * Bug in TGtkCListStringList.Assign corrected.

  Revision 1.20  2000/04/10 14:03:06  lazarus
  Added SetProp and GetProp winapi calls.
  Added ONChange to the TEdit's published property list.
  Shane

  Revision 1.19  2000/03/30 21:57:45  lazarus
  MWE:
    + Added some general functions to Get/Set the Main/Fixed/CoreChild
      widget
    + Started with graphic scalig/depth stuff. This is way from finished

  Hans-Joachim Ott <hjott@compuserve.com>:
    + Added some improvements for TMEMO

  Revision 1.18  2000/03/30 18:07:54  lazarus
  Added some drag and drop code
  Added code to change the unit name when it's saved as a different name.  Not perfect yet because if you are in a comment it fails.

  Shane

  Revision 1.17  2000/02/28 19:16:04  lazarus
  Added code to the FILE CLOSE to check if the file was modified.  HAven't gotten the application.messagebox working yet though.  It won't stay visible.
  Shane

  Revision 1.16  2000/02/24 21:15:30  lazarus
  Added TCustomForm.GetClientRect and RequestAlign to try and get the controls to align correctly when a MENU is present.  Not Complete yet.

  Fixed the bug in TEdit that caused it not to update it's text property.  I will have to
  look at TMemo to see if anything there was affected.

  Added SetRect to WinAPI calls
  Added AdjustWindowRectEx to WINAPI calls.
  Shane

  Revision 1.15  2000/02/22 22:19:50  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.14  2000/02/22 21:51:40  lazarus
  MWE: Removed some double (or triple) event declarations.
       The latest compiler doesn't like it

  Revision 1.13  2000/02/21 17:38:04  lazarus
  Added modalresult to TCustomForm
  Added a View Units dialog box
  Added a View Forms dialog box
  Added a New Unit menu selection
  Added a New Form menu selection
  Shane

  Revision 1.12  2000/02/18 19:38:53  lazarus
  Implemented TCustomForm.Position
  Better implemented border styles. Still needs some tweaks.
  Changed TComboBox and TListBox to work again, at least partially.
  Minor cleanups.

  Revision 1.11  2000/01/04 19:16:09  lazarus
  Stoppok:
     - new messages LM_GETVALUE, LM_SETVALUE, LM_SETPROPERTIES
     - changed trackbar, progressbar, checkbox to use above messages
     - some more published properties for above components
       (all properties derived from TWinControl)
     - new functions SetValue, GetValue, SetProperties in gtk-interface

  Revision 1.10  1999/12/30 19:04:13  lazarus
   - Made TRadiobutton work again
   - Some more cleanups to checkbox code
           stoppok

  Revision 1.9  1999/12/30 10:38:59  lazarus

    Some changes to Checkbox code.
      stoppok

  Revision 1.8  1999/12/29 01:30:02  lazarus

    Made groupbox working again.
      stoppok

  Revision 1.7  1999/12/18 18:27:32  lazarus
  MWE:
    Rearranged some events to get a LM_SIZE, LM_MOVE and LM_WINDOWPOSCHANGED
    Initialized the TextMetricstruct to zeros to clear unset values
    Get mwEdit to show more than one line
    Fixed some errors in earlier commits

  Revision 1.6  1999/12/07 01:19:26  lazarus
  MWE:
    Removed some double events
    Changed location of SetCallBack
    Added call to remove signals
    Restructured somethings
    Started to add default handlers in TWinControl
    Made some parts of TControl and TWinControl more delphi compatible
    ... and lots more ...

  Revision 1.5  1999/11/01 01:28:30  lazarus
  MWE: Implemented HandleNeeded/CreateHandle/CreateWND
       Now controls are created on demand. A call to CreateComponent shouldn't
       be needed. It is now part of CreateWnd

  Revision 1.4  1999/10/27 17:27:07  lazarus
  Added alot of changes and TODO: statements
  shane

  Revision 1.3  1999/10/25 17:38:52  lazarus
  More stuff added for compatability.  Most stuff added was put in the windows.pp file.  CONST scroll bar messages and such.  2 functions were also added to that unit that needs to be completed.
  Shane

  Revision 1.2  1999/10/22 21:01:51  lazarus

        Removed calls to InterfaceObjects except for controls.pp. Commented
        out any gtk depend lines of code.     MAH

  Revision 1.1  1999/10/19 19:16:51  lazarus
  renamed stdcontrols.pp stdctrls.pp
  Shane

  Revision 1.9  1999/08/21 13:57:41  lazarus
  Implemented TListBox.BorderStyle. The listbox is scrollable now.

  Revision 1.8  1999/08/14 10:05:56  lazarus
  Added TListBox ItemIndex property. Made ItemIndex public for TComboBox and TListBox.

  Revision 1.7  1999/08/11 20:41:34  lazarus

  Minor changes and additions made.  Lazarus may not compile due to these changes

  Revision 1.6  1999/08/07 17:59:23  lazarus

        buttons.pp   the DoLeave and DoEnter were connected to the wrong
                     event.

        The rest were modified to use the new SendMessage function.   MAH

 }


