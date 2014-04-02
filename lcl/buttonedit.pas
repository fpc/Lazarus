{

 TButtonEdit Component
 (Relacement for TEditButton component)

 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

 Original Author of TCustomButtonEdit: Derit Agustin

}
unit ButtonEdit;

{$mode objfpc}{$H+}

{$I lcl_defines.inc}

interface

uses
  Classes, SysUtils, LCLProc, LResources, LCLStrConsts, Types, LCLType, LMessages,
  Graphics, Controls, Forms, FileUtil, Dialogs, StdCtrls, Buttons, Calendar,
  ExtDlgs, CalendarPopup, MaskEdit, Menus;

const
  NullDate: TDateTime = 0;

type

  
  TButtonAlign = (BaLeft, BaRight);

  { TBEEdit }

  TBeEdit = class(TCustomMaskedit)
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
  end;

  { TCustomButtonEdit }

  TCustomButtonEdit = class(TCustomControl)
  private
    FButton: TSpeedButton;
    FButtonAlign: TButtonAlign;
    FButtonOnlyWhenFocused: Boolean;
    FDirectInput: Boolean;
    FEdit: TBeEdit;
    FFlat: Boolean;
    //Forwarded events from FButton
    FOnButtonClick: TNotifyEvent;
    //Forwarded events from FEdit
    FOnEditClick: TNotifyEvent;
    FOnEditChange: TNotifyEvent;
    FOnEditDblClick: TNotifyEvent;
    FOnEditDragDrop: TDragDropEvent;
    FOnEditDragOver: TDragOverEvent;
    FOnEditEditingDone: TNotifyEvent;
    FOnEditEndDrag: TEndDragEvent;
    FOnEditExit: TNotifyEvent;
    FOnEditKeyDown: TKeyEvent;
    FOnEditKeyPress: TKeyPressEvent;
    FOnEditEnter: TNotifyEvent;
    FOnEditKeyUp: TKeyEvent;
    FOnEditMouseDown: TMouseEvent;
    FOnEditMouseUp: TMouseEvent;
    FOnEditMouseEnter: TNotifyEvent;
    FOnEditMouseLeave: TNotifyEvent;
    FOnEditMouseMove: TMouseMoveEvent;
    FOnEditStartDrag: TStartDragEvent;
    FOnEditUtf8KeyPress: TUtf8KeyPressEvent;

    function GetAlignment: TAlignment;
    function GetAutoSelect: Boolean;
    function GetBtnCaption: TCaption;
    function GetButtonHint: TTranslateString;
    function GetButtonWidth: Integer;
    function GetCharCase: TEditCharCase;
    function GetColor: TColor;
    function GetDirectInput: Boolean;
    function GetEchoMode: TEchoMode;
    function GetEditMask: String;
    function GetGlyph: TBitmap;
    function GetHideSelection: Boolean;
    function GetMaxLength: Integer;
    function GetNumbersOnly: Boolean;
    function GetNumGlyps: Integer;
    function GetPasswordChar: char;
    function GetReadOnly: Boolean;
    function GetText: TCaption;
    function IsCustomGlyph : Boolean;

    procedure DoEditClick(Sender: TObject);
    procedure DoEditDragDrop(Sender, Source: TObject; X,Y: Integer);
    procedure DoEditDragOver(Sender, Source: TObject; X,Y: Integer; State: TDragState; var Accept: Boolean);
    procedure DoEditEditingDone(Sender: TObject);
    procedure DoEditEndDrag(Sender, Target: TObject; X,Y: Integer);
    procedure DoEditKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure DoEditKeyPress(Sender: TObject; var Key: char);
    procedure DoEditKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure DoEditMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoEditMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DoEditMouseEnter(Sender: TObject);
    procedure DoEditMouseLeave(Sender: TObject);
    procedure DoEditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoEditUtf8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure DoEditStartDrag(Sender: TObject; var DragObject: TDragObject);

    procedure SetAlignment(AValue: TAlignment);
    procedure SetAutoSelect(AValue: Boolean);
    procedure SetButtonAlign(AValue: TButtonAlign);
    procedure SetBtnCaption(AValue: TCaption);
    procedure SetButtonHint(AValue: TTranslateString);
    procedure SetButtonOnlyWhenFocused(AValue: Boolean);
    procedure SetButtonWidth(AValue: Integer);
    procedure SetCharCase(AValue: TEditCharCase);
    procedure SetDirectInput(AValue: Boolean);
    procedure SetEchoMode(AValue: TEchoMode);
    procedure SetEditMask(AValue: String);
    procedure SetFlat(AValue: Boolean);
    procedure SetGlyph(AValue: TBitmap);
    procedure SetHideSelection(AValue: Boolean);
    procedure SetMaxLength(AValue: Integer);
    procedure SetNumbersOnly(AValue: Boolean);
    procedure SetNumGlyphs(AValue: Integer);
    procedure SetPasswordChar(AValue: char);
    procedure SetPopupMenu(AValue: TPopupMenu);
    procedure SetReadOnly(AValue: Boolean);
    procedure SetText(AValue: TCaption);
  protected
    class function GetControlClassDefaultSize: TSize; override;
    function CalcButtonVisible: Boolean; virtual;
    function GetDefaultGlyph: TBitmap; virtual;
    function GetDefaultGlyphName: String; virtual;
    function GetPopupMenu: TPopupMenu; override;
    procedure CheckButtonVisible;

    procedure DoButtonClick(Sender: TObject);  virtual;

    procedure DoEditDblClick(Sender: TObject); virtual;
    procedure DoEditEnter(Sender: TObject); virtual;
    procedure DoEditExit(Sender: TObject); virtual;
    procedure DoEditTextChange(Sender: TObject); virtual;


    procedure Loaded; override;
    procedure SetAutoSize(AValue: Boolean); override;
    procedure SetColor(AValue: TColor); override;
    procedure SetCursor(AValue: TCursor); override;

    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect default True;
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    property Button: TSpeedButton read FButton;
    property ButtonAlign: TButtonAlign read FButtonAlign write SetButtonAlign default BaRight;
    property ButtonCaption: TCaption read GetBtnCaption write SetBtnCaption;
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;
    property ButtonOnlyWhenFocused: Boolean read FButtonOnlyWhenFocused write SetButtonOnlyWhenFocused default False;
    property ButtonWidth: Integer read GetButtonWidth write SetButtonWidth;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
    property Color: TColor read GetColor write SetColor stored True default {$ifdef UseCLDefault}clDefault{$else}clWindow{$endif};
    property DirectInput : Boolean read GetDirectInput write SetDirectInput default True;
    property EchoMode: TEchoMode read GetEchoMode write SetEchoMode default emNormal;
    property EditMask: String read GetEditMask write SetEditMask;
    property Flat: Boolean read FFlat write SetFlat default False;
    property Glyph: TBitmap read GetGlyph write SetGlyph stored IsCustomGlyph;
    property HideSelection: Boolean read GetHideSelection write SetHideSelection default False;
    property MaxLength: Integer read GetMaxLength write SetMaxLength;
    property NumbersOnly: Boolean read GetNumbersOnly write SetNumbersOnly default False;
    property NumGlyphs: Integer read GetNumGlyps write SetNumGlyphs;
    property PasswordChar: char read GetPasswordChar write SetPasswordChar;
    property PopupMenu: TPopupMenu read GetPopupMenu write SetPopupMenu;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;

    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property OnChange: TNotifyEvent read FOnEditChange write FOnEditChange;
    property OnClick: TNotifyEvent read FOnEditClick write FOnEditClick;
    property OnDblClick: TNotifyEvent read FOnEditDblClick write FOnEditDblClick;
    property OnDragDrop: TDragDropEvent read FOnEditDragDrop write FOnEditDragDrop;
    property OnDragOver: TDragOverEvent read FOnEditDragOver write FOnEditDragOver;
    property OnEditingDone: TNotifyEvent read FOnEditEditingDone write FOnEditEditingDone;
    property OnEndDrag: TEndDragEvent read FOnEditEndDrag write FOnEditEndDrag;
    property OnEnter: TNotifyEvent read FOnEditEnter write FOnEditEnter;
    property OnExit: TNotifyEvent read FOnEditExit write FOnEditExit;
    property OnMouseDown: TMouseEvent read FOnEditMouseDown write FOnEditMouseDown;
    property OnKeyPress: TKeyPressEvent read FOnEditKeyPress write FOnEditKeyPress;
    property OnKeyUp: TKeyEvent read FOnEditKeyUp write FOnEditKeyUp;
    property OnMouseEnter: TNotifyEvent read FOnEditMouseEnter write FOnEditMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnEditMouseLeave write FOnEditMouseLeave;
    property OnMouseMove: TMouseMoveEvent read FOnEditMouseMove write FOnEditMouseMove;
    property OnMouseUp: TMouseEvent read FOnEditMouseUp write FOnEditMouseUp;
    property OnStartDrag: TStartDragEvent read FOnEditStartDrag write FOnEditStartDrag;
    property OnUtf8KeyPress: TUtf8KeyPressEvent read FOnEditUtf8KeyPress write FOnEditUtf8KeyPress;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Text: TCaption read GetText write SetText;

  end;

 { TButtonEdit }

  TButtonEdit = class(TCustomButtonEdit)
  public
    property Button;
  published
    property NumbersOnly;
    property Action;
    property AutoSelect;
    property AutoSize default False;
    property Align;
    property Alignment;
    property Anchors;
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle default bsNone;
    property ButtonAlign;
    property ButtonCaption;
    property ButtonHint;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property CharCase;
    property Color;
    property Constraints;
    property Cursor;
    property DirectInput;
    property EchoMode;
    property Enabled;
    property Flat;
    property Font;
    property Glyph;
//    property HideSelection;
    property Hint;
    property MaxLength;
    property NumGlyphs;
    property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnContextPopup;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
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
  TCustomControlFilterEdit = class(TCustomButtonEdit)
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
    procedure DoEditTextChange(Sender: TObject); override;
    procedure DoEditEnter(Sender: TObject); override;
    procedure DoEditExit(Sender: TObject); override;
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
    property Text;
  end;

  { TFileNameEdit }

  TAcceptFileNameEvent = procedure (Sender : TObject; Var Value : String) of Object;
  TDialogKind = (dkOpen,dkSave,dkPictureOpen,dkPictureSave);

  TFileNameEdit = class(TCustomButtonEdit)
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
    property Text;
  end;


  { TDirectoryEdit }

  TDirectoryEdit = class(TCustomButtonEdit)
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
    property Text;
  end;


  { TDateEdit }

  TAcceptDateEvent = procedure (Sender : TObject; var ADate : TDateTime;
    var AcceptDate: Boolean) of object;
  TCustomDateEvent = procedure (Sender : TObject; var ADate : string) of object;
  TDateOrder = (doNone,doMDY,doDMY,doYMd);

  { TDateEdit }

  TDateEdit = class(TCustomButtonEdit)
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
    procedure DoEditDblClick(Sender: TObject); override;
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
    property Text;
  end;


  { TCalcEdit }

  TAcceptValueEvent = procedure(Sender: TObject; var AValue: Double; var Accept: Boolean) of object;
  TCalcEdit = class(TCustomButtonEdit)
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
    property Text;
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

{$IFDEF USEBUTTONEDIT}
{$R lcl_edbtnimg.res}
{$ENDIF}

{ TBEEdit }

procedure TBeEdit.DoEnter;
begin
  if (Owner is TCustomButtonEdit) then TCustomButtonEdit(Owner).CheckButtonVisible;
  inherited DoEnter;
end;

procedure TBeEdit.DoExit;
begin
  if (Owner is TCustomButtonEdit) then TCustomButtonEdit(Owner).CheckButtonVisible;
  inherited DoExit;
end;

{ TCustomButtonEdit }

procedure TCustomButtonEdit.DoButtonClick(Sender: TObject);
begin
  if ReadOnly then
    Exit;
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);
end;

procedure TCustomButtonEdit.DoEditTextChange(Sender: TObject);
begin
  if Assigned(FOnEditChange) then FOnEditChange(Self);
end;

procedure TCustomButtonEdit.DoEditClick(Sender: TObject);
begin
  if Assigned(FOnEditClick) then FOnEditClick(Self);
end;

procedure TCustomButtonEdit.DoEditDblClick(Sender: TObject);
begin
  if Assigned(FOnEditDblClick) then FOnEditDblClick(Self);
end;

procedure TCustomButtonEdit.DoEditDragDrop(Sender, Source: TObject; X, Y: Integer);
begin
  if Assigned(FOnEditDragDrop) then FOnEditDragDrop(Self, Source, X, Y);
end;

procedure TCustomButtonEdit.DoEditDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  if Assigned(FOnEditDragOver) then FOnEditDragOver(Self, Source, X, Y, State, Accept);
end;

procedure TCustomButtonEdit.DoEditEditingDone(Sender: TObject);
begin
  if Assigned(FOnEditEditingDone) then FOnEditEditingDone(Self);
end;

procedure TCustomButtonEdit.DoEditEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Assigned(FOnEditEndDrag) then FOnEditEndDrag(Self, Target, X, Y);
end;

procedure TCustomButtonEdit.DoEditEnter(Sender: TObject);
begin
  if Assigned(FOnEditEnter) then FOnEditEnter(Self);
end;

procedure TCustomButtonEdit.DoEditExit(Sender: TObject);
begin
  if Assigned(FOnEditExit) then FOnEditExit(Self);
end;

procedure TCustomButtonEdit.DoEditKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Assigned(FOnEditKeyDown) then FOnEditKeyDown(Self, Key, Shift);
end;

procedure TCustomButtonEdit.DoEditKeyPress(Sender: TObject; var Key: char);
begin
  if Assigned(FOnEditKeyPress) then FOnEditKeyPress(Self, Key);
end;

procedure TCustomButtonEdit.DoEditKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if Assigned(FOnEditKeyUp) then FOnEditKeyUp(Self, Key, Shift);
end;

procedure TCustomButtonEdit.DoEditMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnEditMouseDown) then FOnEditMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCustomButtonEdit.DoEditMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnEditMouseUp) then FOnEditMouseUp(Self, Button, Shift, X, Y);
end;

procedure TCustomButtonEdit.DoEditMouseEnter(Sender: TObject);
begin
  if Assigned(FOnEditMouseEnter) then FOnEditMouseEnter(Self);
end;

procedure TCustomButtonEdit.DoEditMouseLeave(Sender: TObject);
begin
  if Assigned(FOnEditMouseLeave) then FOnEditMouseLeave(Self);
end;

procedure TCustomButtonEdit.DoEditMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnEditMouseMove) then FOnEditMouseMove(Self, Shift, X, Y);
end;

procedure TCustomButtonEdit.DoEditUtf8KeyPress(Sender: TObject;
  var UTF8Key: TUTF8Char);
begin
  if Assigned(FOnEditUtf8KeyPress) then FOnEditUtf8KeyPress(Self, Utf8Key);
end;

procedure TCustomButtonEdit.DoEditStartDrag(Sender: TObject; var DragObject: TDragObject);
begin
  if Assigned(FOnEditStartDrag) then FOnEditStartDrag(Self, DragObject);
end;

function TCustomButtonEdit.GetButtonWidth: Integer;
begin
  Result := FButton.Width;
end;

function TCustomButtonEdit.GetPopupMenu: TPopupMenu;
begin
  Result := FEdit.PopupMenu;
end;

function TCustomButtonEdit.GetReadOnly: Boolean;
begin
  Result := FEdit.ReadOnly;
end;

function TCustomButtonEdit.GetText: TCaption;
begin
  Result := FEdit.Text;
end;

function TCustomButtonEdit.IsCustomGlyph: Boolean;

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
        C := TPortableNetworkGraphic.Create;
        C.LoadFromResourceName(hInstance, ResName);
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

function TCustomButtonEdit.GetAlignment: TAlignment;
begin
  Result := FEdit.Alignment;
end;


function TCustomButtonEdit.GetAutoSelect: Boolean;
begin
  Result := FEdit.AutoSelect;
end;

function TCustomButtonEdit.GetButtonHint: TTranslateString;
begin
  Result := FButton.Hint;
end;

function TCustomButtonEdit.GetBtnCaption: TCaption;
begin
  Result := FButton.Caption;
end;

procedure TCustomButtonEdit.SetButtonAlign(AValue: TButtonAlign);
begin
  if FButtonAlign = AValue then
    exit;
  FButtonAlign := AValue;
  case FButtonAlign of
    BaRight:
      FButton.Align := alRight;
    BaLeft:
      FButton.Align := alLeft;
  end;
end;

procedure TCustomButtonEdit.SetButtonHint(AValue: TTranslateString);
begin
  FButton.Hint := AValue;
end;

procedure TCustomButtonEdit.SetButtonOnlyWhenFocused(AValue: Boolean);
begin
  if FButtonOnlyWhenFocused <> AValue then
  begin
    FButtonOnlyWhenFocused := AValue;
    CheckButtonVisible;
  end;
end;

procedure TCustomButtonEdit.SetButtonWidth(AValue: Integer);
begin
  FButton.Width := AValue;
end;

function TCustomButtonEdit.GetCharCase: TEditCharCase;
begin
  Result := FEdit.CharCase;
end;

function TCustomButtonEdit.GetDirectInput: Boolean;
begin
  Result := FDirectInput;
end;

function TCustomButtonEdit.GetEchoMode: TEchoMode;
begin
  Result := FEdit.EchoMode;
end;

function TCustomButtonEdit.GetEditMask: String;
begin
  Result := FEdit.EditMask
end;

function TCustomButtonEdit.GetColor: TColor;
begin
  Result := FEdit.Color;
end;


function TCustomButtonEdit.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

function TCustomButtonEdit.GetHideSelection: Boolean;
begin
  Result := FEdit.HideSelection;
end;


function TCustomButtonEdit.GetMaxLength: Integer;
begin
  Result := FEdit.MaxLength;
end;

function TCustomButtonEdit.GetNumbersOnly: Boolean;
begin
  Result := FEdit.NumbersOnly;
end;

function TCustomButtonEdit.GetNumGlyps: Integer;
begin
  Result := FButton.NumGlyphs;
end;

function TCustomButtonEdit.GetPasswordChar: char;
begin
  Result := FEdit.PasswordChar;
end;

procedure TCustomButtonEdit.SetAlignment(AValue: TAlignment);
begin
  FEdit.Alignment := AValue;
end;

procedure TCustomButtonEdit.SetAutoSelect(AValue: Boolean);
begin
  FEdit.AutoSelect := AValue;
end;

procedure TCustomButtonEdit.SetAutoSize(AValue: Boolean);
begin
  if AutoSize = AValue then
    Exit;
  inherited SetAutosize(AValue);
  //FButton.AutoSize := AValue;
  FEdit.AutoSize := AValue;
end;

procedure TCustomButtonEdit.SetBtnCaption(AValue: TCaption);
begin
  FButton.Caption := AValue;
end;

class function TCustomButtonEdit.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 100;
  Result.CY := 23;
end;

function TCustomButtonEdit.GetDefaultGlyph: TBitmap;
begin
  Result := nil;
end;

function TCustomButtonEdit.GetDefaultGlyphName: String;
begin
  Result := '';
end;


procedure TCustomButtonEdit.SetCharCase(AValue: TEditCharCase);
begin
  FEdit.CharCase := AValue;
end;

procedure TCustomButtonEdit.SetDirectInput(AValue: Boolean);
begin
  FDirectInput := AValue;
  FEdit.ReadOnly := ((not FDirectInput) or (FEdit.ReadOnly));
end;

procedure TCustomButtonEdit.SetEchoMode(AValue: TEchoMode);
begin
  FEdit.EchoMode := AValue;
end;

procedure TCustomButtonEdit.SetEditMask(AValue: String);
begin
  FEdit.EditMask := AValue;
end;

procedure TCustomButtonEdit.SetColor(AValue: TColor);
begin
  FEdit.Color := AValue;
end;

procedure TCustomButtonEdit.SetCursor(AValue: TCursor);
begin
  if Cursor = AValue then
    Exit;
  inherited SetCursor(AValue);
  FButton.Cursor := AValue;
  FEdit.Cursor := AValue;
end;

procedure TCustomButtonEdit.SetFlat(AValue: Boolean);
begin
  if FFlat = AValue then
    Exit;
  FFlat := AValue;
  FButton.Flat := AValue;
end;

procedure TCustomButtonEdit.SetHideSelection(AValue: Boolean);
begin
  FEdit.HideSelection := AValue;
end;

procedure TCustomButtonEdit.SetMaxLength(AValue: Integer);
begin
  FEdit.MaxLength := AValue;
end;

procedure TCustomButtonEdit.SetNumbersOnly(AValue: Boolean);
begin
  FEdit.NumbersOnly := AValue;
end;

procedure TCustomButtonEdit.SetNumGlyphs(AValue: Integer);
begin
  FButton.NumGlyphs := AValue;
end;

procedure TCustomButtonEdit.SetPasswordChar(AValue: char);
begin
  FEdit.PasswordChar := AValue;
end;


procedure TCustomButtonEdit.SetPopupMenu(AValue: TPopupMenu);
begin
  FEdit.PopupMenu := AValue;
end;

procedure TCustomButtonEdit.SetText(AValue: TCaption);
begin
  FEdit.Text := AValue;
end;


function TCustomButtonEdit.CalcButtonVisible: Boolean;
begin
  Result := (csdesigning in ComponentState) or
            (Visible and (FEdit.Focused or not FButtonOnlyWhenFocused));
end;

procedure TCustomButtonEdit.CheckButtonVisible;
begin
  If Assigned(FButton) then
    FButton.Visible := CalcButtonVisible;
end;

procedure TCustomButtonEdit.Loaded;
begin
  inherited Loaded;
  CheckButtonVisible;
end;


procedure TCustomButtonEdit.SetGlyph(AValue: TBitmap);
begin
  FButton.Glyph := AValue;
  Invalidate;
end;


procedure TCustomButtonEdit.SetReadOnly(AValue: Boolean);
begin
  FEdit.ReadOnly := AValue;
end;


constructor TCustomButtonEdit.Create(AOwner: TComponent);
var
  B: TBitmap;
begin
  FButton := TSpeedButton.Create(Self);
  FEdit := TBeEdit.Create(Self);
  inherited Create(AOwner);
  BorderStyle := bsNone;
  FButtonAlign := BaRight;
  FButtonOnlyWhenFocused := False;
  FDirectInput := False;

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);


  with FButton do
  begin
    Align := alRight;
    OnClick := @DoButtonClick;
    Parent := Self;
  end;
  B := GetDefaultGlyph;
  if B = nil
  then
   FButton.LoadGlyphFromResourceName(hInstance, GetDefaultGlyphName)
  else
    FButton.Glyph := B;

  with FEdit do
  begin
    Align := alClient;
    ParentColor := False;
    ParentFont := True;

    AutoSelect := True;
    Alignment := taLeftJustify;
    ReadOnly := False;

    OnChange := @DoEditTextChange;
    OnClick := @DoEditClick;
    OnDblClick := @DoEditDblClick;
    OnDragDrop := @DoEditDragDrop;
    OnDragOver := @DoEditDragOver;
    OnEndDrag := @DoEditEndDrag;
    OnExit := @DoEditExit;
    OnEnter := @DoEditEnter;
    OnKeyDown := @DoEditKeyDown;
    OnKeyPress := @DoEditKeyPress;
    OnKeyUp := @DoEditKeyUp;
    OnMouseDown := @DoEditMouseDown;
    OnMouseUp := @DoEditMouseUp;
    OnMouseEnter := @DoEditEnter;
    OnMouseLeave := @DoEditMouseLeave;
    OnMouseMove := @DoEditMouseMove;
    OnStartDrag := @DoEditStartDrag;
    OnUtf8KeyPress := @DoEditUtf8KeyPress;

    Parent := Self;
  end;
end;

destructor TCustomButtonEdit.Destroy;
begin
  inherited Destroy;
end;             

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

procedure TCustomControlFilterEdit.DoEditTextChange(Sender: TObject);
begin
  Filter:=Text;
  inherited;
end;

procedure TCustomControlFilterEdit.DoEditEnter(Sender: TObject);
begin
//  inherited;
  fJustActivated:=False;
  if Text=rsFilter then
    Text:='';
end;

procedure TCustomControlFilterEdit.DoEditExit(Sender: TObject);
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

procedure TDateEdit.DoEditDblClick(Sender: TObject);
begin
  inherited DoEditDblClick(Sender);
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
  {$IFDEF USEBUTTONEDIT}
  RegisterComponents('Misc', [TButtonEdit,TFileNameEdit,TDirectoryEdit,
                              TDateEdit,TCalcEdit]);
  {$ENDIF}
end;

end.
