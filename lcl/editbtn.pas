{
 /***************************************************************************
                               editbtn.pas
                               -----------
                Component Library Extended dialogs Controls


 ***************************************************************************/

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit EditBtn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, LCLStrConsts, LCLType, LMessages,
  Graphics, Controls, Forms, FileUtil, Dialogs, StdCtrls, Buttons, Calendar,
  ExtDlgs, CalendarPopup, MaskEdit;


const
  NullDate: TDateTime = 0;

type
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
    // New properties.
    property ButtonWidth : Integer read GetButtonWidth write SetButtonWidth;
    property DirectInput : Boolean read GetDirectInput write SetDirectInput default True;
    property Glyph : TBitmap read GetGlyph write SetGlyph;
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
    procedure OnIdle(Sender: TObject; var Done: Boolean);
  private
    fFilter: string;
    fIdleConnected: Boolean;
    fSortData: Boolean;             // Data needs to be sorted.
    procedure SetFilter(const AValue: string);
    procedure SetIdleConnected(const AValue: Boolean);
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
    function GetDefaultGlyphName: String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InvalidateFilter;
    procedure StoreSelection; virtual; abstract;
    procedure RestoreSelection; virtual; abstract;
  public
    property Filter: string read fFilter write SetFilter;
    property IdleConnected: Boolean read fIdleConnected write SetIdleConnected;
    property SortData: Boolean read fSortData write fSortData;
    property SelectedPart: TObject read fSelectedPart write fSelectedPart;
  published
    property OnFilterItem: TFilterItemEvent read fOnFilterItem write fOnFilterItem;
    property OnCheckItem: TCheckItemEvent read fOnCheckItem write fOnCheckItem;
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
    property DialogOptions: TOpenOptions read FDialogOptions write FDialogOptions;
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
    property Color;
    property Constraints;
    property CharCase;
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
  
  TAcceptValueEvent = procedure(Sender: TObject; var AValue: Double; var Action: Boolean) of object;
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


{ TCustomControlFilterEdit }

constructor TCustomControlFilterEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Button.Enabled:=False;
  fIsFirstUpdate := True;
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
end;

procedure TCustomControlFilterEdit.SetFilter(const AValue: string);
var
  NewValue: String;
begin
  if AValue=lisCEFilter then
    NewValue:=''
  else
    NewValue:=LowerCase(AValue);
  Button.Enabled:=NewValue<>'';
  if (NewValue='') and not Focused then begin
    Text:=lisCEFilter;
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
begin
  if (Key in [VK_UP, VK_DOWN]) and (Shift = []) then
  begin
    case Key of
      VK_UP:   MovePrev;
      VK_DOWN: MoveNext;
    end;
    Key:=0;
  end
  else inherited KeyDown(Key, Shift);
end;

procedure TCustomControlFilterEdit.Change;
begin
  Filter:=Text;
  inherited;
end;

procedure TCustomControlFilterEdit.DoEnter;
begin
  if Text=lisCEFilter then
    Text:='';
  inherited;
end;

procedure TCustomControlFilterEdit.DoExit;
begin
  Filter:=Text;
  inherited;
end;

procedure TCustomControlFilterEdit.DoButtonClick(Sender: TObject);
begin
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
    if csDestroying in ComponentState then exit;
    InvalidateFilter;
  end;
end;

procedure TCustomControlFilterEdit.InvalidateFilter;
begin
  fNeedUpdate:=true;
  IdleConnected:=true;
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
begin
  case AKind of
    dkopen, dkPictureOpen:
    begin
      O := TOpenDialog.Create(Self);
      O.FileName := FileName;
      O.Options := DialogOptions;
      O.InitialDir := InitialDir;
      O.Filter := Filter;
      O.FilterIndex := FilterIndex;
      Result := O;
    end;
    dkSave, dkPictureSave:
    begin
      S:=TSaveDialog.Create(Self);
      S.DefaultExt := FDefaultExt;
      S.Filter := Filter;
      S.FilterIndex := FilterIndex;
      Result := S;
    end;
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
