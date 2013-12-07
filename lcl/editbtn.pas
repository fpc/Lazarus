{
 /***************************************************************************
                               editbtn.pas
                               -----------
                Component Library Extended dialogs Controls


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit EditBtn;

{$mode objfpc}{$H+}

{$IFDEF NewEditButton}{$I lcl_defines.inc}{$ENDIF}

interface

uses
  Classes, SysUtils, LCLProc, LResources, LCLStrConsts, LCLType, LMessages,
  Graphics, Controls, Forms, FileUtil, Dialogs, StdCtrls, Buttons, Calendar,
  ExtDlgs, CalendarPopup, MaskEdit, Menus;


const
  NullDate: TDateTime = 0;

type
{$IFDEF NewEditButton}

  TEBMaskEdit = class(TCustomMaskEdit)
  protected
    { this is only because of the property ButtonOnlyWhenFocused }
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
  end;

  { TCustomEditButton }

  TCustomEditButton = class(TWinControl)
  private
    FButton: TSpeedButton;
    FButtonNeedsFocus: Boolean;
    FDirectInput: Boolean;
    FEdit: TEBMaskEdit;
    FOnButtonClick: TNotifyEvent;
    FReadOnly: Boolean;
    function GetAlignment: TAlignment;
    function GetAutoSelect: Boolean;
    function GetButtonHint: TTranslateString;
    function GetButtonWidth: Integer;
    function GetCharCase: TEditCharCase;
    function GetEditDragCursor: TCursor;
    function GetEditDragMode: TDragMode;
    function GetEchoMode: TEchoMode;
    function GetEditColor: TColor;
    function GetEditMask: string;
    function GetEditPopupMenu: TPopupMenu;
    function GetEditText: string;
    function GetFlat: Boolean;
    function GetGlyph: TBitmap;
    function GetMaxLength: Integer;
    function GetNumGlyphs: Integer;
    function GetOnChange: TNotifyEvent;
    function GetOnClick: TNotifyEvent;
    function GetOnDblClick: TNotifyEvent;
    function GetOnDragDrop: TDragDropEvent;
    function GetOnDragOver: TDragOverEvent;
    function GetOnEditingDone: TNotifyEvent;
    function GetOnEndDrag: TEndDragEvent;
    function GetOnEnter: TNotifyEvent;
    function GetOnKeyDown: TKeyEvent;
    function GetOnKeyPress: TKeyPressEvent;
    function GetOnKeyUp: TKeyEvent;
    function GetOnMouseDown: TMouseEvent;
    function GetOnMouseMove: TMouseMoveEvent;
    function GetOnMouseUp: TMouseEvent;
    function GetOnStartDrag: TStartDragEvent;
    function GetOnUTF8KeyPress: TUTF8KeyPressEvent;
    function GetPasswordChar: Char;
    function GetTabStop: Boolean;
    function IsCustomGlyph: Boolean;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetAutoSelect(AValue: Boolean);
    procedure SetButtonHint(AValue: TTranslateString);
    procedure SetButtonNeedsFocus(AValue: Boolean);
    procedure SetButtonWidth(AValue: Integer);
    procedure SetCharCase(AValue: TEditCharCase);
    procedure SetDirectInput(AValue: Boolean);
    procedure SetEchoMode(AValue: TEchoMode);
    procedure SetEditDragCursor(AValue: TCursor);
    procedure SetEditColor(AValue: TColor);
    procedure SetEditDragMode(AValue: TDragMode);
    procedure SetEditMask(AValue: string);
    procedure SetEditBtnPopupMenu(AValue: TPopupMenu);
    procedure SetEditText(AValue: string);
    procedure SetFlat(AValue: Boolean);
    procedure SetGlyph(AValue: TBitmap);
    procedure SetMaxLength(AValue: Integer);
    procedure SetNumGlyphs(AValue: Integer);
    procedure SetOnChange(const AValue: TNotifyEvent);
    procedure SetOnClick(AValue: TNotifyEvent);
    procedure SetOnDblClick(AValue: TNotifyEvent);
    procedure SetOnDragDrop(AValue: TDragDropEvent);
    procedure SetOnDragOver(AValue: TDragOverEvent);
    procedure SetOnEditingDone(AValue: TNotifyEvent);
    procedure SetOnEndDrag(AValue: TEndDragEvent);
    procedure SetOnEnter(AValue: TNotifyEvent);
    procedure SetOnKeyDown(AValue: TKeyEvent);
    procedure SetOnKeyPress(AValue: TKeyPressEvent);
    procedure SetOnKeyUp(AValue: TKeyEvent);
    procedure SetOnMouseDown(AValue: TMouseEvent);
    procedure SetOnMouseMove(AValue: TMouseMoveEvent);
    procedure SetOnMouseUp(AValue: TMouseEvent);
    procedure SetOnStartDrag(AValue: TStartDragEvent);
    procedure SetOnUTF8KeyPress(AValue: TUTF8KeyPressEvent);
    procedure SetPasswordChar(AValue: Char);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetTabStop(AValue: Boolean);
  protected
    procedure AnchorEditAndButton; virtual;
    function CalcButtonVisible: Boolean; virtual;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer;
      WithThemeSpace: Boolean); override;
    procedure Change; virtual;
    procedure CheckButtonVisible; virtual;
    procedure CMEnter(var Message: TLMessage); message CM_ENTER;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    function ColorIsStored: Boolean; override;
    procedure DoButtonClick(Sender: TObject); virtual;
    function GetBorderStyle: TBorderStyle;
    function GetDefaultGlyph: TBitmap; virtual;
    function GetDefaultGlyphName: string; virtual;
    procedure Loaded; override;
    procedure RealSetText(const AValue: TCaption); override;
    procedure SetBiDiMode(AValue: TBiDiMode); override;
    procedure SetBorderStyle(NewStyle: TBorderStyle); override;
    property DirectInput: Boolean read FDirectInput write SetDirectInput default True;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
  protected
    // edit
    property Edit:TEBMaskEdit read FEdit;
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect default True;
    property AutoSize default True;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
    property Color: TColor read GetEditColor write SetEditColor stored ColorIsStored
               default {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
    property DragCursor: TCursor read GetEditDragCursor write SetEditDragCursor;
    property DragMode: TDragMode read GetEditDragMode write SetEditDragMode;
    property EchoMode: TEchoMode read GetEchoMode write SetEchoMode default emNormal;
    property EditMask: string read GetEditMask write SetEditMask;
    property MaxLength: Integer read GetMaxLength write SetMaxLength default 0;
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
    property OnClick: TNotifyEvent read GetOnClick write SetOnClick;
    property OnDblClick: TNotifyEvent read GetOnDblClick write SetOnDblClick;
    property OnDragDrop: TDragDropEvent read GetOnDragDrop write SetOnDragDrop;
    property OnDragOver: TDragOverEvent read GetOnDragOver write SetOnDragOver;
    property OnEditingDone: TNotifyEvent read GetOnEditingDone write SetOnEditingDone;
    property OnEndDrag: TEndDragEvent read GetOnEndDrag write SetOnEndDrag;
    property OnEnter: TNotifyEvent read GetOnEnter write SetOnEnter;
    property OnKeyDown: TKeyEvent read GetOnKeyDown write SetOnKeyDown;
    property OnKeyPress: TKeyPressEvent read GetOnKeyPress write SetOnKeyPress;
    property OnKeyUp: TKeyEvent read GetOnKeyUp write SetOnKeyUp;
    property OnMouseDown: TMouseEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read GetOnMouseMove write SetOnMouseMove;
    property OnMouseUp: TMouseEvent read GetOnMouseUp write SetOnMouseUp;
    property OnStartDrag: TStartDragEvent read GetOnStartDrag write SetOnStartDrag;
    property OnUTF8KeyPress: TUTF8KeyPressEvent read GetOnUTF8KeyPress write SetOnUTF8KeyPress;
    property PasswordChar: Char read GetPasswordChar write SetPasswordChar default #0;
  protected
    // button
    property Button: TSpeedButton read FButton;
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;
    property ButtonOnlyWhenFocused: Boolean read FButtonNeedsFocus write SetButtonNeedsFocus default False;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustomGlyph;
    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs;
    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property BorderStyle read GetBorderStyle write SetBorderStyle default bsSingle;
    property ParentColor default False;
    property PopupMenu: TPopupMenu read GetEditPopupMenu write SetEditBtnPopupMenu;
    property TabStop read GetTabStop write SetTabStop default True;
    property Text: string read GetEditText write SetEditText;
  end;

{$ELSE}

  { TCustomEditButton }

  TCustomEditButton = class(TCustomMaskEdit)
  private
    FButton: TSpeedButton;
    FButtonNeedsFocus: Boolean;
    FDirectInput: Boolean;
    FIsReadOnly: boolean;
    FOnButtonClick : TNotifyEvent;
    function GetButtonHint: TTranslateString;
    function GetButtonWidth: Integer;
    function GetDirectInput: Boolean;
    function GetFlat: Boolean;
    procedure SetButtonHint(const AValue: TTranslateString);
    procedure SetButtonNeedsFocus(const AValue: Boolean);
    procedure SetButtonWidth(const AValue: Integer);
    procedure SetDirectInput(const AValue: Boolean);
    procedure SetFlat(const AValue: Boolean);
    procedure SetGlyph(Pic: TBitmap);
    function GetGlyph : TBitmap;
    procedure SetNumGlyphs(ANumber: Integer);
    function GetNumGlyphs:Integer;
    function GetMinHeight: Integer;
    procedure WMSetFocus(var Message: TLMSetFocus); message LM_SETFOCUS;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    function IsCustomGlyph : Boolean;
  protected
    procedure CheckButtonVisible;
    function CalcButtonVisible: boolean; virtual;
    function CalcButtonEnabled: Boolean; virtual;
    function GetReadOnly: Boolean; override;
    function GetDefaultGlyph: TBitmap; virtual;
    function GetDefaultGlyphName: String; virtual;
    procedure SetParent(AParent: TWinControl); override;
    procedure SetReadOnly(AValue: Boolean); override;
    procedure DoPositionButton; virtual;
    procedure DoButtonClick (Sender: TObject); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    // New properties.
    property ButtonWidth : Integer read GetButtonWidth write SetButtonWidth;
    property DirectInput : Boolean read GetDirectInput write SetDirectInput default True;
    property Glyph : TBitmap read GetGlyph write SetGlyph stored IsCustomGlyph;
    property NumGlyphs : Integer read GetNumGlyphs write SetNumGlyphs;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property Button: TSpeedButton read FButton;
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Flat: Boolean read GetFlat write SetFlat default False;
    property ButtonOnlyWhenFocused: Boolean read FButtonNeedsFocus write SetButtonNeedsFocus default False;
  end;

{$ENDIF}
  
  { TEditButton }

  TEditButton = class(TCustomEditButton)
  Public
    property Button;
  published
    property AutoSize;
    property AutoSelect;
    property Align;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property ButtonHint;
    property CharCase;
    property Color;
    property DirectInput;
    property DragCursor;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
    property MaxLength;
    property NumGlyphs;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
  end;

  // Called when an item is filtered. Returns true if the item passes the filter.
  // Done=False means the data should also be filtered by its title string.
  // Done=True means no other filtering is needed.
  TFilterItemEvent = function (Item: TObject; out Done: Boolean): Boolean of object;

  // Can be used only for items that have a checkbox. Returns true if checked.
  TCheckItemEvent = function (Item: TObject): Boolean of object;

  { TCustomControlFilterEdit }

  // An abstract base class for edit controls which filter data in
  // visual controls like TListView and TTreeView.
  TCustomControlFilterEdit = class(TCustomEditButton)
  private
    fFilter: string;
    fIdleConnected: Boolean;
    fSortData: Boolean;             // Data needs to be sorted.
    fUseFormActivate: Boolean;
    fIsFirstSetFormActivate: Boolean;
    fJustActivated: Boolean;
    fParentForm: TForm;
    fOnAfterFilter: TNotifyEvent;
    procedure SetFilter(const AValue: string);
    procedure SetIdleConnected(const AValue: Boolean);
    procedure OnIdle(Sender: TObject; var Done: Boolean);
    procedure SetUseFormActivate(AValue: Boolean);
    procedure FormActivate(Sender: TObject); // Connects to owning form.
    procedure FormDeactivate(Sender: TObject);
  protected
    fNeedUpdate: Boolean;
    fIsFirstUpdate: Boolean;
    fSelectedPart: TObject;         // Select this node on next update
    fOnFilterItem: TFilterItemEvent;
    fOnCheckItem: TCheckItemEvent;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Change; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoButtonClick (Sender: TObject); override;
    procedure SortAndFilter; virtual; abstract;
    procedure ApplyFilter(Immediately: Boolean = False);
    procedure ApplyFilterCore; virtual; abstract;
    procedure MoveNext; virtual; abstract;
    procedure MovePrev; virtual; abstract;
    function ReturnPressed: Boolean; virtual; abstract;
    function GetDefaultGlyphName: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateFilter;
    function ForceFilter(AFilter : String = '') : String;
    procedure StoreSelection; virtual; abstract;
    procedure RestoreSelection; virtual; abstract;
  public
    property Filter: string read fFilter write SetFilter;
    property IdleConnected: Boolean read fIdleConnected write SetIdleConnected;
    property SortData: Boolean read fSortData write fSortData;
    property SelectedPart: TObject read fSelectedPart write fSelectedPart;
  published
    property OnAfterFilter: TNotifyEvent read fOnAfterFilter write fOnAfterFilter;
    property OnFilterItem: TFilterItemEvent read fOnFilterItem write fOnFilterItem;
    property OnCheckItem: TCheckItemEvent read fOnCheckItem write fOnCheckItem;
    property UseFormActivate: Boolean read fUseFormActivate write SetUseFormActivate default False;
    // TEditButton properties.
    property ButtonWidth;
    property DirectInput;
    property ButtonOnlyWhenFocused;
    property NumGlyphs;
    property Flat;
    // Other properties
    property Align;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property AutoSize;
    property AutoSelect;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;

  { TFileNameEdit }

  TAcceptFileNameEvent = procedure (Sender : TObject; Var Value : String) of Object;
  TDialogKind = (dkOpen,dkSave,dkPictureOpen,dkPictureSave);
  
  TFileNameEdit = class(TCustomEditButton)
  private
    FDialogOptions: TOpenOptions;
    FFileName : String;
    FDialogFiles : TStrings;
    FDialogKind: TDialogKind;
    FDialogTitle: String;
    FFilter: String;
    FFilterIndex: Integer;
    FDefaultExt: String;
    FHideDirectories: Boolean;
    FInitialDir: String;
    FOnAcceptFileName: TAcceptFileNameEvent;
    FOnFolderChange: TNotifyEvent;
    FFileNameChangeLock: Integer;
    procedure SetFileName(const AValue: String);
  protected
    function GetDefaultGlyph: TBitmap; override;
    function GetDefaultGlyphName: String; override;
    function CreateDialog(AKind: TDialogKind): TCommonDialog; virtual;
    procedure SaveDialogResult(AKind: TDialogKind; D: TCommonDialog); virtual;
    procedure DoButtonClick (Sender: TObject); override;
    procedure RunDialog; virtual;
    procedure TextChanged; override;
    procedure DoFolderChange(Sender:TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property DialogFiles: TStrings read FDialogFiles;
  published
    // TFileName properties.
    property FileName: String read FFileName write SetFileName;
    property InitialDir: String read FInitialDir write FInitialDir;
    property OnAcceptFileName: TAcceptFileNameEvent read FOnAcceptFileName write FOnAcceptFileName;
    property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
    property DialogKind: TDialogKind read FDialogKind write FDialogKind default dkOpen;
    property DialogTitle: String read FDialogTitle write FDialogTitle;
    property DialogOptions: TOpenOptions read FDialogOptions write FDialogOptions default DefaultOpenDialogOptions;
    property Filter: String read FFilter write FFilter;
    property FilterIndex: Integer read FFilterIndex write FFIlterIndex;
    property DefaultExt: String read FDefaultExt write FDefaultExt;
    property HideDirectories: Boolean read FHideDirectories write FHideDirectories;
    // TEditButton properties.
    property ButtonWidth;
    property DirectInput;
    property ButtonOnlyWhenFocused;
    // property Glyph;
    property NumGlyphs;
    property Flat;
    // Other properties
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property AutoSize;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;
  
  
  { TDirectoryEdit }
  
  TDirectoryEdit = class(TCustomEditButton)
  private
    FDialogTitle: String;
    FRootDir: String;
    FOnAcceptDir: TAcceptFileNameEvent;
    FShowHidden: Boolean;
    function GetDirectory: String;
    procedure SetDirectory(const AValue: String);
  protected
    function GetDefaultGlyph: TBitmap; override;
    function GetDefaultGlyphName: String; override;
    function CreateDialog: TCommonDialog; virtual;
    function GetDialogResult(D : TCommonDialog) : String; virtual;
    procedure DoButtonClick (Sender: TObject); override;
    procedure RunDialog; virtual;
  public
  published
    // TDirectory properties.
    property Directory: String read GetDirectory write SetDirectory;
    property RootDir: String read FRootDir write FRootDir;
    property OnAcceptDirectory: TAcceptFileNameEvent read FOnAcceptDir write FonAcceptDir;
    property DialogTitle: String read FDialogTitle write FDialogTitle;
    property ShowHidden: Boolean read FShowHidden write FShowHidden;
    // TEditButton properties.
    property ButtonWidth;
    property DirectInput;
    property ButtonOnlyWhenFocused;
    // property Glyph;
    property NumGlyphs;
    property Flat;
    // Other properties
    property Align;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;
  
  
  { TDateEdit }

  TAcceptDateEvent = procedure (Sender : TObject; var ADate : TDateTime;
    var AcceptDate: Boolean) of object;
  TCustomDateEvent = procedure (Sender : TObject; var ADate : string) of object;
  TDateOrder = (doNone,doMDY,doDMY,doYMd);

  { TDateEdit }

  TDateEdit = class(TCustomEditButton)
  private
    FDateOrder: TDateOrder;
    FDefaultToday: Boolean;
    FDialogTitle: TCaption;
    FDisplaySettings: TDisplaySettings;
    FDroppedDown: Boolean;
    FOnAcceptDate: TAcceptDateEvent;
    FOnCustomDate: TCustomDateEvent;
    FOKCaption: TCaption;
    FCancelCaption: TCaption;
    FDateFormat: string;
    function GetDate: TDateTime;
    function IsStoreTitle: boolean;
    procedure SetDate(Value: TDateTime);
    procedure CalendarPopupReturnDate(Sender: TObject; const ADate: TDateTime);
    procedure CalendarPopupShowHide(Sender: TObject);
    procedure SetDateOrder(const AValue: TDateOrder);
  protected
    function GetDefaultGlyph: TBitmap; override;
    function GetDefaultGlyphName: String; override;
    procedure DoButtonClick(Sender: TObject); override;
    procedure DblClick; override;
    procedure SetDateMask; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure DateFormatChanged; virtual;
    function GetDateFormat: string;
    property Date: TDateTime read GetDate write SetDate;
    property Button;
    property DroppedDown: Boolean read FDroppedDown;
  published
    property DialogTitle: TCaption read FDialogTitle write FDialogTitle stored IsStoreTitle;
    property CalendarDisplaySettings: TDisplaySettings read FDisplaySettings write FDisplaySettings;
    property OnAcceptDate: TAcceptDateEvent read FOnAcceptDAte write FOnAcceptDate;
    property OnCustomDate: TCustomDateEvent read FOnCustomDate write FOnCustomDate;
    property OKCaption: TCaption read FOKCaption write FOKCaption;
    property CancelCaption: TCaption read FCancelCaption write FCancelCaption;
    property ReadOnly;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday default False;
    Property DateOrder : TDateOrder Read FDateOrder Write SetDateOrder;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DirectInput;
    property Glyph;
    property NumGlyphs;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property MaxLength;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnUTF8KeyPress;
    property ParentBidiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
  end;

  
  { TCalcEdit }
  
  TAcceptValueEvent = procedure(Sender: TObject; var AValue: Double; var Accept: Boolean) of object;
  TCalcEdit = class(TCustomEditButton)
  private
    FDialogTitle: String;
    FLayout: TCalculatorLayout;
    FOnAcceptValue: TAcceptValueEvent;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    procedure SetAsFloat(const AValue: Double);
    procedure SetAsInteger(const AValue: Integer);
    function TitleStored: boolean;
  protected
    FCalcDialog : TForm;
    function GetDefaultGlyph: TBitmap; override;
    function GetDefaultGlyphName: String; override;
    procedure DoButtonClick (Sender: TObject); override;
    procedure RunDialog; virtual;
  public
    constructor Create(AOwner: TComponent); override;
  published
    // CalcEdit properties
    property CalculatorLayout : TCalculatorLayout read FLayout write Flayout;
    property AsFloat : Double read GetAsFloat write SetAsFloat;
    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property OnAcceptValue : TAcceptValueEvent read FOnAcceptValue write FOnAcceptValue;
    property DialogTitle : String read FDialogTitle write FDialogTitle stored TitleStored;
    // TEditButton properties.
    property ButtonWidth;
    property DirectInput;
    property ButtonOnlyWhenFocused;
    // property Glyph;
    property NumGlyphs;
    property Flat;
    // Other properties
    property Align;
    property Anchors;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property AutoSize;
    property AutoSelect;
    property Color;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
  end;


var
  FileOpenGlyph: TBitmap;
  DateGlyph: TBitmap;
  CalcGlyph: TBitmap;

const
  ResBtnListFilter = 'btnfiltercancel';
  ResBtnFileOpen   = 'btnselfile';
  ResBtnSelDir     = 'btnseldir';
  ResBtnCalendar   = 'btncalendar';
  ResBtnCalculator = 'btncalculator';

procedure Register;

implementation

{$IFDEF NewEditButton}

{ TEBMaskEdit }

procedure TEBMaskEdit.WMKillFocus(var Message: TLMKillFocus);
begin
  with Parent as TCustomEditButton do CheckButtonVisible;
  inherited;
end;

procedure TEBMaskEdit.WMSetFocus(var Message: TLMSetFocus);
begin
  with Parent as TCustomEditButton do CheckButtonVisible;
  inherited;
end;

{ TCustomEditButton }

procedure TCustomEditButton.DoButtonClick(Sender: TObject);
begin
  if (not ReadOnly) and assigned(FOnButtonClick) then FOnButtonClick(self);
end;

function TCustomEditButton.GetAlignment: TAlignment;
begin
  Result:=FEdit.Alignment;
end;

function TCustomEditButton.ColorIsStored: Boolean;
begin
  Result:= inherited ColorIsStored;     ///FEdit.IsColorStored
end;

function TCustomEditButton.GetAutoSelect: Boolean;
begin
  Result:=Edit.AutoSelect;
end;

function TCustomEditButton.GetBorderStyle: TBorderStyle;
begin
  Result:=FEdit.BorderStyle;
end;

function TCustomEditButton.GetButtonHint: TTranslateString;
begin
  Result:=Button.Hint;
end;

function TCustomEditButton.GetButtonWidth: Integer;
begin
  Result:=Button.Width;
end;

function TCustomEditButton.GetCharCase: TEditCharCase;
begin
  Result:=Edit.CharCase;
end;

function TCustomEditButton.GetDefaultGlyph: TBitmap;
begin
  Result:=nil;
end;

function TCustomEditButton.GetDefaultGlyphName: string;
begin
  Result:='';
end;

function TCustomEditButton.GetEchoMode: TEchoMode;
begin
  Result:=Edit.EchoMode;
end;

function TCustomEditButton.GetEditColor: TColor;
begin
  Result:=Edit.Color;
end;

function TCustomEditButton.GetEditDragCursor: TCursor;
begin
  Result:=Edit.DragCursor;
end;

function TCustomEditButton.GetEditDragMode: TDragMode;
begin
  Result:=Edit.DragMode;
end;

function TCustomEditButton.GetEditMask: string;
begin
  with Edit do Result:=EditMask;
end;

function TCustomEditButton.GetEditPopupMenu: TPopupMenu;
begin
  Result:=Edit.PopupMenu;
end;

function TCustomEditButton.GetEditText: string;
begin
  Result:=Edit.Text;
end;

function TCustomEditButton.GetFlat: Boolean;
begin
  Result:=Button.Flat;
end;

function TCustomEditButton.GetGlyph: TBitmap;
begin
  Result:=Button.Glyph;
end;

function TCustomEditButton.GetMaxLength: Integer;
begin
  Result:=Edit.MaxLength;
end;

function TCustomEditButton.GetNumGlyphs: Integer;
begin
  Result:=Button.NumGlyphs;
end;

function TCustomEditButton.GetOnChange: TNotifyEvent;
begin
  Result:=Edit.OnChange;
end;

function TCustomEditButton.GetOnClick: TNotifyEvent;
begin
  Result:=Edit.OnClick;
end;

function TCustomEditButton.GetOnDblClick: TNotifyEvent;
begin
  Result:=Edit.OnDblClick;
end;

function TCustomEditButton.GetOnDragDrop: TDragDropEvent;
begin
  Result:=Edit.OnDragDrop;
end;

function TCustomEditButton.GetOnDragOver: TDragOverEvent;
begin
  Result:=Edit.OnDragOver;
end;

function TCustomEditButton.GetOnEditingDone: TNotifyEvent;
begin
  Result:=Edit.OnEditingDone;
end;

function TCustomEditButton.GetOnEndDrag: TEndDragEvent;
begin
  Result:=Edit.OnEndDrag;
end;

function TCustomEditButton.GetOnEnter: TNotifyEvent;
begin
  Result:=Edit.OnEnter;
end;

function TCustomEditButton.GetOnKeyDown: TKeyEvent;
begin
  Result:=Edit.OnKeyDown;
end;

function TCustomEditButton.GetOnKeyPress: TKeyPressEvent;
begin
  Result:=Edit.OnKeyPress;
end;

function TCustomEditButton.GetOnKeyUp: TKeyEvent;
begin
  Result:=Edit.OnKeyUp;
end;

function TCustomEditButton.GetOnMouseDown: TMouseEvent;
begin
  Result:=Edit.OnMouseDown;
end;

function TCustomEditButton.GetOnMouseMove: TMouseMoveEvent;
begin
  Result:=Edit.OnMouseMove;
end;

function TCustomEditButton.GetOnMouseUp: TMouseEvent;
begin
  Result:=Edit.OnMouseUp;
end;

function TCustomEditButton.GetOnStartDrag: TStartDragEvent;
begin
  Result:=Edit.OnStartDrag;
end;

function TCustomEditButton.GetOnUTF8KeyPress: TUTF8KeyPressEvent;
begin
  Result:=Edit.OnUTF8KeyPress;
end;

function TCustomEditButton.GetPasswordChar: Char;
begin
  Result:=Edit.PasswordChar;
end;

function TCustomEditButton.GetTabStop: Boolean;
begin
  Result:=FEdit.TabStop;
end;

function TCustomEditButton.IsCustomGlyph: Boolean;

  function _LoadRes: TBitmap;
  var
    ResName: String;
    C : TCustomBitmap;
  begin
    ResName := GetDefaultGlyphName;
    if ResName = '' then
      Exit(nil);
    Result := TBitmap.Create;
    try
      try
        C := CreateBitmapFromLazarusResource(ResName);
        Result.Assign(C); // the "Equals" did not work with ClassType different
        // maybe it should compare the "RawImage" because it is independent of ClassType
      finally
        C.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  end;

var
  B, GlypRes, GlypActual: TBitmap;
begin
  GlypActual := nil;
  GlypRes := nil;
  try
    B := GetDefaultGlyph;
    if B = nil then                // if Default Glyph is nil, use the resource
    begin
      GlypRes := _LoadRes;
      B := GlypRes;
    end;
    if B = nil then
      Result := Glyph <> nil
    else if Glyph = nil then
      Result := True
    else
    begin
      GlypActual := TBitmap.Create; // the "Equals" did not work with ClassType different.
      GlypActual.Assign(Glyph);
      Result := not GlypActual.Equals(B);
    end;
  finally
    GlypRes.Free;
    GlypActual.Free;
  end;
end;

procedure TCustomEditButton.SetAlignment(AValue: TAlignment);
begin
  Edit.Alignment:=AValue;
end;

procedure TCustomEditButton.SetAutoSelect(AValue: Boolean);
begin
  Edit.AutoSelect:=AValue;
end;

procedure TCustomEditButton.SetBiDiMode(AValue: TBiDiMode);
begin
  if BiDiMode=AValue then exit;
  inherited SetBiDiMode(AValue);
  DisableAutoSizing;
  try
    AnchorEditAndButton;
  finally
    EnableAutoSizing;
  end;
end;

procedure TCustomEditButton.SetBorderStyle(NewStyle: TBorderStyle);
begin
  Edit.BorderStyle:=NewStyle;
end;

procedure TCustomEditButton.SetButtonHint(AValue: TTranslateString);
begin
  Button.Hint:=AValue;
end;

procedure TCustomEditButton.SetButtonNeedsFocus(AValue: Boolean);
begin
  if FButtonNeedsFocus=AValue then exit;
  FButtonNeedsFocus:=AValue;
  AnchorEditAndButton;
  CheckButtonVisible;
end;

procedure TCustomEditButton.SetButtonWidth(AValue: Integer);
begin
  Button.Width:=AValue;
end;

procedure TCustomEditButton.SetCharCase(AValue: TEditCharCase);
begin
  Edit.CharCase:=AValue;
end;

procedure TCustomEditButton.SetDirectInput(AValue: Boolean);
begin
  FDirectInput:=AValue;
  if not ReadOnly then Edit.ReadOnly:= not AValue;
end;

procedure TCustomEditButton.SetEditDragCursor(AValue: TCursor);
begin
  Edit.DragCursor:=AValue;
end;

procedure TCustomEditButton.SetEchoMode(AValue: TEchoMode);
begin
  Edit.EchoMode:=AValue;
end;

procedure TCustomEditButton.SetEditColor(AValue: TColor);
begin
  Edit.Color:=AValue;
end;

procedure TCustomEditButton.SetEditDragMode(AValue: TDragMode);
begin
  Edit.DragMode:=AValue;
end;

procedure TCustomEditButton.SetEditMask(AValue: string);
begin
  with Edit do EditMask:=AValue;
end;

procedure TCustomEditButton.SetEditBtnPopupMenu(AValue: TPopupMenu);
begin
  Edit.PopupMenu:=AValue;
  Button.PopupMenu:=AValue;
end;

procedure TCustomEditButton.SetEditText(AValue: string);
begin
  Edit.Text:=AValue;
end;

procedure TCustomEditButton.SetFlat(AValue: Boolean);
begin
  Button.Flat:=AValue;
end;

procedure TCustomEditButton.SetGlyph(AValue: TBitmap);
begin
  Button.Glyph:=AValue;
end;

procedure TCustomEditButton.SetMaxLength(AValue: Integer);
begin
  Edit.MaxLength:=AValue;
end;

procedure TCustomEditButton.SetNumGlyphs(AValue: Integer);
begin
  Button.NumGlyphs:=AValue;
end;

procedure TCustomEditButton.SetOnChange(const AValue: TNotifyEvent);
begin
  Edit.OnChange:=AValue;
end;

procedure TCustomEditButton.SetOnClick(AValue: TNotifyEvent);
begin
  Edit.OnClick:=AValue;
end;

procedure TCustomEditButton.SetOnDblClick(AValue: TNotifyEvent);
begin
  Edit.OnDblClick:=AValue;
end;

procedure TCustomEditButton.SetOnDragDrop(AValue: TDragDropEvent);
begin
  Edit.OnDragDrop:=AValue;
end;

procedure TCustomEditButton.SetOnDragOver(AValue: TDragOverEvent);
begin
  Edit.OnDragOver:=AValue;
end;

procedure TCustomEditButton.SetOnEditingDone(AValue: TNotifyEvent);
begin
  Edit.OnEditingDone:=AValue;
end;

procedure TCustomEditButton.SetOnEndDrag(AValue: TEndDragEvent);
begin
  Edit.OnEndDrag:=AValue;
end;

procedure TCustomEditButton.SetOnEnter(AValue: TNotifyEvent);
begin
  Edit.OnEnter:=AValue;
end;

procedure TCustomEditButton.SetOnKeyDown(AValue: TKeyEvent);
begin
  Edit.OnKeyDown:=AValue;
end;

procedure TCustomEditButton.SetOnKeyPress(AValue: TKeyPressEvent);
begin
  Edit.OnKeyPress:=AValue;
end;

procedure TCustomEditButton.SetOnKeyUp(AValue: TKeyEvent);
begin
  Edit.OnKeyUp:=AValue;
end;

procedure TCustomEditButton.SetOnMouseDown(AValue: TMouseEvent);
begin
  Edit.OnMouseDown:=AValue;
end;

procedure TCustomEditButton.SetOnMouseMove(AValue: TMouseMoveEvent);
begin
  Edit.OnMouseMove:=AValue;
end;

procedure TCustomEditButton.SetOnMouseUp(AValue: TMouseEvent);
begin
  Edit.OnMouseUp:=AValue;
end;

procedure TCustomEditButton.SetOnStartDrag(AValue: TStartDragEvent);
begin
  Edit.OnStartDrag:=AValue;
end;

procedure TCustomEditButton.SetOnUTF8KeyPress(AValue: TUTF8KeyPressEvent);
begin
  Edit.OnUTF8KeyPress:=AValue;
end;

procedure TCustomEditButton.SetPasswordChar(AValue: Char);
begin
  Edit.PasswordChar:=AValue;
end;

procedure TCustomEditButton.SetReadOnly(AValue: Boolean);
begin
  FReadOnly:=AValue;
  if AValue or DirectInput then Edit.ReadOnly:=AValue;
  Button.Enabled:=not AValue;
end;

procedure TCustomEditButton.SetTabStop(AValue: Boolean);
begin
  FEdit.TabStop:=AValue;
end;

procedure TCustomEditButton.AnchorEditAndButton;
begin
  DisableAutoSizing;
  try
    Button.Anchors:=[];
    if IsRightToLeft then begin
      // button + edit
      Button.AnchorParallel(akLeft, 0, Self);
      Edit.AnchorAsAlign(alRight, 0);
      Edit.AnchorToNeighbour(akLeft, 0, Button);
    end else begin
      // edit + button
      Button.AnchorParallel(akRight, 0, Self);
      Edit.AnchorAsAlign(alLeft, 0);
      Edit.AnchorToNeighbour(akRight, 0, Button);
    end;
    Button.AnchorParallel(akTop, 0, Edit);
    Button.AnchorParallel(akBottom, 0, Edit);
  finally
    EnableAutoSizing;
  end;
end;

function TCustomEditButton.CalcButtonVisible: Boolean;
begin
  Result:=(csDesigning in ComponentState) or
          (Visible and (FEdit.Focused or not FButtonNeedsFocus));
end;

procedure TCustomEditButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);
  PreferredWidth:=0;
end;

procedure TCustomEditButton.CheckButtonVisible;
begin
  if assigned(FButton) then FButton.Visible:=CalcButtonVisible;
end;

procedure TCustomEditButton.Change;
begin
  with FEdit do Change;
end;

procedure TCustomEditButton.CMEnter(var Message: TLMessage);
begin
  FEdit.SetFocus;
end;

procedure TCustomEditButton.CMParentColorChanged(var Message: TLMessage);
begin
  inherited CMParentColorChanged(Message);
  FEdit.ParentColor:=ParentColor;
end;

procedure TCustomEditButton.Loaded;
begin
  inherited Loaded;
  CheckButtonVisible;
end;

procedure TCustomEditButton.RealSetText(const AValue: TCaption);
begin
  inherited RealSetText(AValue);
  SetEditText(AValue);
end;

constructor TCustomEditButton.Create(TheOwner: TComponent);
var aGlyph: TBitmap;
begin
  inherited Create(TheOwner);
  FCompStyle:=csNone;

  ControlStyle:=ControlStyle
               + [csAcceptsControls, csNoFocus, csNoStdEvents, csOwnedChildrenNotSelectable]
               - [csClickEvents, csDoubleClicks, csOpaque, csSetCaption];

  AutoSize:=True;
  Caption:='';
  FDirectInput:=True;
  ParentColor:=False;

  FEdit:=TEBMaskEdit.Create(self);
  Edit.ControlStyle := FEdit.ControlStyle + [csNoDesignSelectable] - [csSetCaption];
  Edit.Parent:=self;

  BorderStyle:=bsSingle;

  FButton:=TSpeedButton.Create(self);
  Button.OnClick:=@DoButtonClick;
  Button.Cursor := crArrow;
  Button.ControlStyle := Button.ControlStyle + [csNoDesignSelectable];
  aGlyph := GetDefaultGlyph;
  if aGlyph = nil then
    Button.LoadGlyphFromLazarusResource(GetDefaultGlyphName)
  else
    Button.Glyph := aGlyph;
  Button.Parent:=self;

  CheckButtonVisible;
  SetInitialBounds(0, 0, Edit.Width+Button.Width, Edit.Height);
  AnchorEditAndButton;
end;

destructor TCustomEditButton.Destroy;
begin
  FreeAndNil(FButton);
  FreeAndNil(FEdit);
  inherited Destroy;
end;

{$ELSE}

{ TCustomEditButton }

constructor TCustomEditButton.Create(AOwner: TComponent);
var
  B: TBitmap;
begin
  inherited Create(AOwner);
  FDirectInput := True;
  FButton := TSpeedButton.Create(Self);
  FButton.Width := Self.Height;
  FButton.Height := Self.Height;
  FButton.FreeNotification(Self);
  CheckButtonVisible;
  FButton.OnClick := @DoButtonClick;
  FButton.Cursor := crArrow;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable];
  B := GetDefaultGlyph;
  if B = nil
  then FButton.LoadGlyphFromLazarusResource(GetDefaultGlyphName)
  else FButton.Glyph := B;
  ControlStyle := ControlStyle - [csSetCaption];
end;

destructor TCustomEditButton.Destroy;
begin
  FreeAndNil(FButton);
  inherited Destroy;
end;

procedure TCustomEditButton.SetGlyph(Pic: TBitmap);
Begin
  FButton.Glyph:=Pic;
end;

function TCustomEditButton.GetButtonWidth: Integer;
begin
  Result:=FButton.Width;
end;

function TCustomEditButton.GetDefaultGlyph: TBitmap;
begin
  Result := nil;
end;

function TCustomEditButton.GetDefaultGlyphName: String;
begin
  Result := '';
end;

function TCustomEditButton.GetButtonHint: TTranslateString;
begin
  Result:=FButton.Hint;
end;

function TCustomEditButton.GetDirectInput: Boolean;
begin
  Result := FDirectInput;
end;

function TCustomEditButton.GetFlat: Boolean;
begin
  if Assigned(FButton) then
    Result := FButton.Flat
  else
    Result := False;
end;

function TCustomEditButton.CalcButtonVisible: boolean;
begin
  Result := (csdesigning in ComponentState) or
            (Visible and (Focused or not FButtonNeedsFocus));
end;

procedure TCustomEditButton.CheckButtonVisible;
begin
  If Assigned(FButton) then
    FButton.Visible:=CalcButtonVisible;
end;

procedure TCustomEditButton.SetButtonHint(const AValue: TTranslateString);
begin
  FButton.Hint:=AValue;
end;

procedure TCustomEditButton.SetButtonNeedsFocus(const AValue: Boolean);
begin
  if FButtonNeedsFocus<>AValue then
  begin
    FButtonNeedsFocus:=AValue;
    CheckButtonVisible;
  end;
end;

procedure TCustomEditButton.SetButtonWidth(const AValue: Integer);
begin
  FButton.Width:=AValue;
end;

procedure TCustomEditButton.SetDirectInput(const AValue: Boolean);
begin
  FDirectInput := AValue;
  inherited SetReadOnly((not FDirectInput) or (FIsReadOnly))
end;

procedure TCustomEditButton.SetFlat(const AValue: Boolean);
begin
  if Assigned(FButton) then
    FButton.Flat:=AValue;
end;

function TCustomEditButton.GetGlyph : TBitmap;
begin
  Result:=FButton.Glyph;
end;

procedure TCustomEditButton.SetNumGlyphs(ANumber: Integer);
begin
  FButton.NumGlyphs:=ANumber;
end;

function TCustomEditButton.GetNumGlyphs:Integer;
begin
  Result:=FButton.NumGlyphs;
end;

procedure TCustomEditButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FButton) and (Operation = opRemove) then
    FButton := nil;
end;

procedure TCustomEditButton.CMVisibleChanged(var Msg: TLMessage);
begin
  inherited CMVisibleChanged(Msg);
  CheckButtonVisible;
end;

procedure TCustomEditButton.CMEnabledChanged(var Msg: TLMessage);
begin
  inherited CMEnabledChanged(Msg);
  if (FButton<>nil) then
    FButton.Enabled:=CalcButtonEnabled;
end;

procedure TCustomEditButton.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited;
  DoPositionButton;
end;

function TCustomEditButton.GetMinHeight: Integer;
begin
  Result:=23;
end;

procedure TCustomEditButton.DoButtonClick (Sender: TObject);
begin
  if not ReadOnly then
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self);
end;

procedure TCustomEditButton.Loaded;
begin
  inherited Loaded;
  DoPositionButton;
  CheckButtonVisible;
end;

procedure TCustomEditButton.WMKillFocus(var Message: TLMKillFocus);
begin
  CheckButtonVisible;
  inherited;
end;

function TCustomEditButton.IsCustomGlyph : Boolean;

  function _LoadRes: TBitmap;
  var
    ResName: String;
    C : TCustomBitmap;
  begin
    ResName := GetDefaultGlyphName;
    if ResName = '' then
      Exit(nil);
    Result := TBitmap.Create;
    try
      try
        C := CreateBitmapFromLazarusResource(ResName);
        Result.Assign(C); // the "Equals" did not work with ClassType different
        // maybe it should compare the "RawImage" because it is independent of ClassType
      finally
        C.Free;
      end;
    except
      Result.Free;
      raise;
    end;
  end;

var
  B, GlypRes, GlypActual: TBitmap;
begin
  GlypActual := nil;
  GlypRes := nil;
  try
    B := GetDefaultGlyph;
    if B = nil then                // if Default Glyph is nil, use the resource
    begin
      GlypRes := _LoadRes;
      B := GlypRes;
    end;
    if B = nil then
      Result := Glyph <> nil
    else if Glyph = nil then
      Result := True
    else
    begin
      GlypActual := TBitmap.Create; // the "Equals" did not work with ClassType different.
      GlypActual.Assign(Glyph);
      Result := not GlypActual.Equals(B);
    end;
  finally
    GlypRes.Free;
    GlypActual.Free;
  end;
end;

function TCustomEditButton.GetReadOnly: Boolean;
begin
  Result := FIsReadOnly;
end;

procedure TCustomEditButton.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FButton <> nil then
  begin
    DoPositionButton;
    CheckButtonVisible;
  end;
end;

function TCustomEditButton.CalcButtonEnabled: Boolean;
begin
  Result := not FIsReadOnly and Enabled;
end;

procedure TCustomEditButton.SetReadOnly(AValue: Boolean);
begin
  FIsReadOnly := AValue;
  if Assigned(FButton) then
    FButton.Enabled := CalcButtonEnabled;
  inherited SetReadOnly(FIsReadOnly or (not DirectInput));
end;

procedure TCustomEditButton.DoPositionButton;
begin
  if FButton = nil then exit;
  FButton.Parent := Parent;
  if BiDiMode = bdLeftToRight then
    FButton.AnchorToCompanion(akLeft,0,Self)
  else
    FButton.AnchorToCompanion(akRight,0,Self);
end;

procedure TCustomEditButton.WMSetFocus(var Message: TLMSetFocus);
begin
  CheckButtonVisible;
  inherited;
end;

{$ENDIF}

{ TCustomControlFilterEdit }

constructor TCustomControlFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  CharCase:=ecLowerCase;
  Button.Enabled:=False;
  fIsFirstUpdate:=True;
  fIsFirstSetFormActivate:=True;
end;

destructor TCustomControlFilterEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomControlFilterEdit.OnIdle(Sender: TObject; var Done: Boolean);
begin
  if fNeedUpdate then
    ApplyFilter(true);
  IdleConnected:=false;
  if Assigned(fOnAfterFilter) then
    fOnAfterFilter(Self);
end;

procedure TCustomControlFilterEdit.SetUseFormActivate(AValue: Boolean);
var
  c: TWinControl;
begin
  if fUseFormActivate=AValue then Exit;
  fUseFormActivate:=AValue;
  c:=Parent;
  // Find the parent form
  while Assigned(c) and not (c is TForm) do
    c:=c.Parent;
  // Found: set or remove Activate and Deactivate handlers
  if c is TForm then begin
    fParentForm:=TForm(c);
    if AValue then begin          // Set handlers
      if fIsFirstSetFormActivate then begin
        if Assigned(fParentForm.OnActivate) or Assigned(fParentForm.OnDeactivate) then
          raise Exception.Create('TCustomControlFilterEdit.SetUseFormActivate:'+
                                 ' OnActivate handler already set in parent form');
        fIsFirstSetFormActivate:=False;
      end;
      fParentForm.OnActivate:=@FormActivate;
      fParentForm.OnDeactivate:=@FormDeactivate;
    end
    else begin                    // Remove handlers
      fParentForm.OnActivate:=nil;
      fParentForm.OnDeactivate:=nil;
    end;
  end
  else
    raise Exception.Create('TCustomControlFilterEdit.SetUseFormActivate: This control'+
              ' has no TForm in the parent chain. You should disable UseFormActivate.');
end;

procedure TCustomControlFilterEdit.FormActivate(Sender: TObject);
begin
  fJustActivated:=fParentForm.ActiveControl=Self;
  if fParentForm.ActiveControl=Self then
    Filter:=Text;
end;

procedure TCustomControlFilterEdit.FormDeactivate(Sender: TObject);
begin
  fJustActivated:=False;
end;

procedure TCustomControlFilterEdit.SetFilter(const AValue: string);
var
  NewValue: String;
begin
  if AValue=rsFilter then
    NewValue:=''
  else
    NewValue:=AValue;
  Button.Enabled:=NewValue<>'';
  if (NewValue='') and not (Focused or fJustActivated) then begin
    Text:=rsFilter;
    Font.Color:=clBtnShadow;
  end
  else begin
    Text:=NewValue;
    Font.Color:=clDefault;
  end;
  if fFilter=NewValue then exit;
  fFilter:=NewValue;
  ApplyFilter;
end;

procedure TCustomControlFilterEdit.SetIdleConnected(const AValue: Boolean);
begin
  if fIdleConnected=AValue then exit;
  fIdleConnected:=AValue;
  if fIdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TCustomControlFilterEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  Handled: Boolean;
begin
  Handled:=False;
  if Shift = [] then
    case Key of
      VK_UP:     begin MovePrev; Handled:=True; end;
      VK_DOWN:   begin MoveNext; Handled:=True; end;
      VK_RETURN: Handled:=ReturnPressed;
    end;
  if Handled then
    Key:=VK_UNKNOWN
  else
    inherited KeyDown(Key, Shift);
end;

procedure TCustomControlFilterEdit.Change;
begin
  Filter:=Text;
  inherited;
end;

procedure TCustomControlFilterEdit.DoEnter;
begin
//  inherited;
  fJustActivated:=False;
  if Text=rsFilter then
    Text:='';
end;

procedure TCustomControlFilterEdit.DoExit;
begin
  fJustActivated:=False;
  Filter:=Text;
//  inherited;
end;

procedure TCustomControlFilterEdit.DoButtonClick(Sender: TObject);
begin
  fJustActivated:=False;
  Filter:='';
end;

procedure TCustomControlFilterEdit.ApplyFilter(Immediately: Boolean);
begin
  if Immediately then begin
    fNeedUpdate := False;
    SortAndFilter;
    if (fSelectedPart=Nil) and not fIsFirstUpdate then
      StoreSelection;      // At first round the selection is from caller
    fIsFirstUpdate:=False;

    ApplyFilterCore;       // The actual filtering implemented by inherited class.

    fSelectedPart:=Nil;
    RestoreSelection;
  end
  else begin
    if [csDestroying,csDesigning]*ComponentState=[] then
      InvalidateFilter;
  end;
end;

procedure TCustomControlFilterEdit.InvalidateFilter;
begin
  fNeedUpdate:=true;
  IdleConnected:=true;
end;

function TCustomControlFilterEdit.ForceFilter(AFilter: String): String;
begin
  Result := FFilter;
  FFilter := AFilter;
  ApplyFilter(True);
end;

function TCustomControlFilterEdit.GetDefaultGlyphName: String;
begin
  Result := ResBtnListFilter;
end;

{ TFileNameEdit }

constructor TFileNameEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDialogFiles := TStringList.Create;
  FDialogKind := dkOpen;
  FDialogOptions := DefaultOpenDialogOptions;
end;

destructor TFileNameEdit.Destroy;
begin
  FreeAndNil(FDialogFiles);
  inherited Destroy;
end;

procedure TFileNameEdit.SetFileName(const AValue: String);
begin
  if FFileNameChangeLock > 0 then
    Exit;
  FFileName := AValue;
  Inc(FFileNameChangeLock);
  try
    if FHideDirectories then
      inherited RealSetText(ExtractFileName(AValue))
    else
      inherited RealSetText(AValue)
  finally
    Dec(FFileNameChangeLock);
  end;
end;

function TFileNameEdit.CreateDialog(AKind: TDialogKind): TCommonDialog;
var
  O: TOpenDialog;
  S: TSaveDialog;
  Dir: String;
begin
  case AKind of
    dkOpen, dkPictureOpen:
    begin
      O := TOpenDialog.Create(Self);
      Result := O;
    end;
    dkSave, dkPictureSave:
    begin
      S:=TSaveDialog.Create(Self);
      S.DefaultExt := FDefaultExt;
      Result := S;
    end;
  end;
  if Result is TOpenDialog then
  begin
    O:=TOpenDialog(Result);
    Dir:=ExtractFilePath(Filename);
    if (Dir<>'') and DirPathExists(Dir) then
      // setting a FileName with path disables InitialDir
      O.FileName := FileName
    else begin
      // do not use path, so that InitialDir works
      O.FileName := ExtractFileName(Filename);
    end;
    O.Options := DialogOptions;
    O.Filter := Filter;
    O.FilterIndex := FilterIndex;
    O.InitialDir := CleanAndExpandDirectory(InitialDir);
  end;
  // Set some common things.
  Result.Title := DialogTitle;
end;

procedure TFileNameEdit.SaveDialogResult(AKind: TDialogKind; D: TCommonDialog);
var
  FN: String;
begin
  case AKind of
    dkOpen, dkPictureOpen :
    begin
      FilterIndex := TOpenDialog(D).FilterIndex;
      FN := TOpenDialog(D).FileName;
      if (FN <> '') then
      begin
        if Assigned(OnAcceptFileName) then
          OnAcceptFileName(Self, FN);
      end;
      if (FN <> '') then
      begin
        // set FDialogFiles first since assigning of FileName trigger events
        FDialogFiles.Text := TOpenDialog(D).Files.Text;
        FileName := FN;
      end;
    end;
    dkSave, dkPictureSave :
    begin
      FileName := TSaveDialog(D).FileName;
      FilterIndex := TSaveDialog(D).FilterIndex;
      FDialogFiles.Clear;
    end;
  end;
end;

procedure TFileNameEdit.DoButtonClick(Sender: TObject);
begin
  inherited DoButtonClick(Sender);
  RunDialog;
end;

function TFileNameEdit.GetDefaultGlyph: TBitmap;
begin
  Result := FileOpenGlyph;
end;

function TFileNameEdit.GetDefaultGlyphName: String;
begin
  Result := ResBtnFileOpen;
end;

procedure TFileNameEdit.RunDialog;
var
  D: TCommonDialog;
begin
  D := CreateDialog(DialogKind);
  try
    if D.Execute then
      SaveDialogResult(DialogKind, D);
  finally
    D.Free;
  end
end;

procedure TFileNameEdit.TextChanged;
begin
  if FFileNameChangeLock <= 0 then
  begin
    Inc(FFileNameChangeLock);
    try
      if FHideDirectories and (ExtractFilePath(Text) = '') then
        FFileName := ExtractFilePath(FFileName) + Text
      else
        FFileName := Text;
    finally
      Dec(FFileNameChangeLock);
    end;
  end;
  inherited TextChanged; //do this _after_ we have updated FFileName
end;

procedure TFileNameEdit.DoFolderChange(Sender: TObject);
begin
  if Assigned(FOnFolderChange) then
    FOnFolderChange(Self);
end;

{ TDirectoryEdit }

procedure TDirectoryEdit.SetDirectory(const AValue: String);
begin
  if (Text<>AValue) then
    Text:=AValue;
end;

function TDirectoryEdit.CreateDialog: TCommonDialog;
begin
  Result:=TSelectDirectoryDialog.Create(Self);
  if DirPathExists(Directory) then
  begin
    TSelectDirectoryDialog(Result).InitialDir:=Directory;
    TSelectDirectoryDialog(Result).FileName:='';
  end
  else
  begin
    TSelectDirectoryDialog(Result).InitialDir:=RootDir;
    TSelectDirectoryDialog(Result).FileName:=Directory;
  end;
  // Set some common things.
  Result.Title := DialogTitle;
end;

function TDirectoryEdit.GetDialogResult(D: TCommonDialog) : String;
begin
  Result:=TSelectDirectoryDialog(D).FileName;
end;


procedure TDirectoryEdit.DoButtonClick(Sender: TObject);
begin
  inherited DoButtonClick(Sender);
  RunDialog;
end;

function TDirectoryEdit.GetDefaultGlyph: TBitmap;
begin
  Result := FileOpenGlyph;
end;

function TDirectoryEdit.GetDefaultGlyphName: String;
begin
  Result := ResBtnSelDir;
end;

procedure TDirectoryEdit.RunDialog;
var
  D: String;
  Dlg: TCommonDialog;
  B: Boolean;
begin
  Dlg:=CreateDialog;
  try
    B:=Dlg.Execute;
    if B then
      D:=GetDialogResult(Dlg);
  finally
    Dlg.Free;
  end;
  if B then
  begin
    if Assigned(FOnAcceptDir) then
    begin
      FOnAcceptdir(Self,D);
      if (D<>'') then
        Directory:=D;
    end
    else
      Directory:=D;
  end;
end;

function TDirectoryEdit.GetDirectory: String;
begin
  Result:=Text;
end;

{ TDateEdit }

function StrToDateDef(cDate: String; dDefault: TDateTime): TDateTime;
begin
  try
    Result := StrToDate(cDate)
  except
    Result := dDefault;
  end;
end;

constructor TDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDefaultToday := False;
  FDisplaySettings := [dsShowHeadings, dsShowDayNames];
  DialogTitle := rsPickDate;
  OKCaption := 'OK';
  CancelCaption := 'Cancel';
  DateFormatChanged;
end;

procedure TDateEdit.DateFormatChanged;
begin
  FDateFormat := DefaultFormatSettings.ShortDateFormat;
end;

function TDateEdit.GetDateFormat: string;
begin
  Result := FDateFormat;
end;

function TDateEdit.GetDefaultGlyph: TBitmap;
begin
  Result := DateGlyph;
end;

function TDateEdit.GetDefaultGlyphName: String;
begin
  Result := ResBtnCalendar;
end;

procedure TDateEdit.DoButtonClick(Sender: TObject);//or onClick
var
  PopupOrigin: TPoint;
  ADate: TDateTime;
begin
  inherited DoButtonClick(Sender);

  PopupOrigin := ControlToScreen(Point(0, Height));
  ADate := GetDate;
  if ADate = NullDate then
    ADate := SysUtils.Date;
  ShowCalendarPopup(PopupOrigin, ADate, CalendarDisplaySettings,
                    @CalendarPopupReturnDate, @CalendarPopupShowHide)
end;

procedure TDateEdit.DblClick;
begin
  inherited DblClick;
  if not ReadOnly then
    DoButtonClick(nil);
end;

procedure TDateEdit.SetDateMask;

Var
  S : String;
  D : TDateTime;
begin
  Case DateOrder of
    doNone :
       begin
       S:=''; // no mask
       FDateFormat:='';
       end;
    doDMY,
    doMDY  :
      begin
      S:='99/99/9999;1;_';
      if DateOrder=doMDY then
        FDateFormat:='mm/dd/yyyy'
      else
        FDateFormat:='dd/mm/yyyy';
      end;
    doYMD  :
      begin
      S:='9999/99/99;1;_';
      FDateFormat:='yyyy/mm/dd';
      end;
  end;
  D:=GetDate;
  EditMask:=S;
  SetDate(D);
end;

Function ParseDate(S : String; Order : TDateOrder; Def: TDateTime) : TDateTime;

Var
  P,N1,N2,N3 : Integer;
  B : Boolean;

begin
  Result:=Def;
  P:=Pos(DefaultFormatSettings.DateSeparator,S);
  If (P=0) then
    Exit;
  N1:=StrToIntDef(Copy(S,1,P-1),-1);
  If (N1=-1) then Exit;
  Delete(S,1,P);
  P:=Pos(DefaultFormatSettings.DateSeparator,S);
  If (P=0) then
    Exit;
  N2:=StrToIntDef(Copy(S,1,P-1),-1);
  If (N1=0) then Exit;
  Delete(S,1,P);
  N3:=StrToIntDef(S,-1);
  If (N3=-1) then
    exit;
  Case Order of
    doYMD : B:=TryEncodeDate(N1,N2,N3,Result);
    doMDY : B:=TryEncodeDate(N3,N1,N2,Result);
    doDMY : B:=TryEncodeDate(N3,N2,N1,Result);
  end;
  If not B then // Not sure if TryEncodeDate touches Result.
    Result:=Def;
end;

function TDateEdit.GetDate: TDateTime;
var
  ADate: string;
begin
  if FDefaultToday then
    Result := SysUtils.Date
  else
    Result := NullDate;
  ADate := Trim(Text);
  if ADate <> '' then
  begin
    if Assigned(FOnCustomDate) then
      FOnCustomDate(Self, ADate);
    if (DateOrder = doNone) then
      Result := StrToDateDef(ADate, Result)
    else
      Result := ParseDate(ADate,DateOrder,Result)
  end;
end;

function TDateEdit.IsStoreTitle: boolean;
begin
  Result := DialogTitle <> rsPickDate;
end;

procedure TDateEdit.SetDate(Value: TDateTime);
begin
  if {not IsValidDate(Value) or }(Value = NullDate) then
  begin
    if DefaultToday then
      Value := SysUtils.Date
    else
      Value := NullDate;
  end;
  if Value = NullDate then
    Text := ''
  else
  begin
    if (FDateFormat = '') then
      Text := DateToStr(Value)
    else
      Text := FormatDateTime(FDateFormat, Value)
  end;
end;

procedure TDateEdit.CalendarPopupReturnDate(Sender: TObject;
  const ADate: TDateTime);
var
  B: Boolean;
  D: TDateTime;
begin
  try
    B := True;
    D := ADate;
    if Assigned(FOnAcceptDate) then
      FOnAcceptDate(Self, D, B);
    if B then
      Self.Date := D;
  except
    on E:Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TDateEdit.CalendarPopupShowHide(Sender: TObject);
begin
  FDroppedDown := (Sender as TForm).Visible;
end;

procedure TDateEdit.SetDateOrder(const AValue: TDateOrder);
begin
  if FDateOrder=AValue then exit;
  FDateOrder:=AValue;
  SetDateMask;
end;

{ TCalcEdit }

function TCalcEdit.GetAsFloat: Double;
begin
  Result := StrToFloatDef(Trim(Text), 0.0);
end;

function TCalcEdit.GetAsInteger: Integer;
begin
  Result:=StrToIntDef(Text,0);
end;

function TCalcEdit.GetDefaultGlyph: TBitmap;
begin
  Result := CalcGlyph;
end;

function TCalcEdit.GetDefaultGlyphName: String;
begin
  Result := ResBtnCalculator;
end;

procedure TCalcEdit.SetAsFloat(const AValue: Double);
begin
  Text:=FloatToStr(AValue);
end;

procedure TCalcEdit.SetAsInteger(const AValue: Integer);
begin
  Text:=IntToStr(AValue);
end;

function TCalcEdit.TitleStored: boolean;
begin
  Result:=FDialogTitle<>rsCalculator;
end;

procedure TCalcEdit.DoButtonClick(Sender: TObject);
begin
  inherited DoButtonClick(Sender);
  RunDialog;
end;

procedure TCalcEdit.RunDialog;
var
  D : Double;
  B : Boolean;
begin
  D:=AsFloat;
  with CreateCalculatorForm(Self,FLayout,0) do
    try
      Caption:=DialogTitle;
      Value:=D;
      if (ShowModal=mrOK) then
      begin
        D:=Value;
        B:=True;
        If Assigned(FOnAcceptValue) then
          FOnAcceptValue(Self,D,B);
        if B then
          AsFloat:=D;
      end;
    finally
      Free;
    end;
end;

constructor TCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FdialogTitle:=rsCalculator;
end;



procedure Register;
begin
  RegisterComponents('Misc', [TEditButton,TFileNameEdit,TDirectoryEdit,
                              TDateEdit,TCalcEdit]);
end;

initialization
{$i lcl_edbtnimg.lrs}

end.
