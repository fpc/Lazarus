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
 *  See the file COPYING.modifiedLGPL, included in this distribution,        *
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
  ExtDlgs, CalendarPopup;

const
  NullDate: TDateTime = 0;

type
  { TCustomEditButton }

  TCustomEditButton = class(TCustomEdit)
  private
    FButton: TSpeedButton;
    FButtonNeedsFocus: Boolean;
    FOnButtonClick : TNotifyEvent;
    function GetButtonHint: TTranslateString;
    function GetButtonWidth: Integer;
    function GetDirectInput: Boolean;
    function GetFlat: Boolean;
    Procedure CheckButtonVisible;
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
    procedure SetParent(AParent: TWinControl); override;
    procedure DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure DoPositionButton; virtual;
    procedure DoButtonClick (Sender: TObject); virtual;
    Procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Msg: TLMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TLMessage); message CM_ENABLEDCHANGED;
    // New properties.
    property ButtonWidth : Integer read GetButtonWidth write SetButtonWidth;
    property DirectInput : Boolean read GetDirectInput write SetDirectInput stored False Default True;
    property Glyph : TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs : Integer read GetNumGlyphs write SetNumGlyphs;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property Button: TSpeedButton read FButton;
    property ButtonHint: TTranslateString read GetButtonHint write SetButtonHint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Flat : Boolean read GetFlat write SetFlat;
    property ButtonOnlyWhenFocused : Boolean read FButtonNeedsFocus write SetButtonNeedsFocus;
  end;
  
  
  { TEditButton }
  
  TEditButton = Class(TCustomEditButton)
  Public
    property Button;
  published
    property AutoSize;
    property AutoSelect;
    property Align;
    property Anchors;
    property BorderSpacing;
    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property ButtonHint;
    property CharCase;
    property Color;
    property Ctl3D;
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
    property ParentColor;
    property ParentCtl3D;
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


  { TFileNameEdit }

  TAcceptFileNameEvent = Procedure (Sender : TObject; Var Value : String) of Object;
  TDialogKind = (dkOpen,dkSave,dkPictureOpen,dkPictureSave);
  
  TFileNameEdit = Class(TCustomEditButton)
  private
    FDialogOptions: TOpenOptions;
    FFileName : String;
    FDialogFiles : TStrings;
    FDialogKind: TDialogKind;
    FDialogTitle: String;
    FFilter: String;
    FFilterIndex: Integer;
    FInitialDir: String;
    FOnAcceptFN: TAcceptFileNameEvent;
    function GetFileName: String;
    procedure SetFileName(const AValue: String);
  protected
    Function CreateDialog(AKind : TDialogKind) : TCommonDialog; virtual;
    Procedure SaveDialogResult(AKind : TDialogKind; D : TCommonDialog); virtual;
    Function CreateFileOpenBitmap : TBitmap; virtual;
    procedure DoButtonClick (Sender: TObject); override;
    Procedure RunDialog; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    property DialogFiles : TStrings read FDialogFiles;
  published
    // TFileName properties.
    property FileName : String read GetFileName write SetFileName;
    property InitialDir : String read FInitialDir write FInitialDir;
    property OnAcceptFileName : TAcceptFileNameEvent read FOnAcceptFN write FonAcceptFN;
    property DialogKind : TDialogKind read FDialogKind write FDialogKind;
    property DialogTitle : String read FDialogTitle write FDialogTitle;
    property DialogOptions : TOpenOptions read FDialogOptions write FDialogOptions;
    property Filter : String read FFilter write FFilter;
    property FilterIndex : Integer read FFilterIndex write FFIlterIndex;
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
    property AutoSelect;
    property BorderSpacing;
    property AutoSize;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
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
  end;
  
  
  { TDirectoryEdit }
  
  TDirectoryEdit = Class(TCustomEditButton)
  private
    FDialogTitle: String;
    FRootDir: String;
    FOnAcceptDir: TAcceptFileNameEvent;
    FShowHidden: Boolean;
    function GetDirectory: String;
    procedure SetDirectory(const AValue: String);
  protected
    Function CreateDialog : TCommonDialog; virtual;
    Function GetDialogResult(D : TCommonDialog) : String; virtual;
    Function CreateDirectoryBitmap : TBitmap; virtual;
    procedure DoButtonClick (Sender: TObject); override;
    Procedure RunDialog; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  published
    // TDirectory properties.
    property Directory : String read GetDirectory write SetDirectory;
    property RootDir : String read FRootDir write FRootDir;
    property OnAcceptDirectory : TAcceptFileNameEvent read FOnAcceptDir write FonAcceptDir;
    property DialogTitle : String read FDialogTitle write FDialogTitle;
    property ShowHidden : Boolean read FShowHidden write FShowHidden;
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
    property BorderSpacing;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
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
  end;
  
  
  { TDateEdit }

  TAcceptDateEvent = Procedure (Sender : TObject; Var ADate : TDateTime;
    Var AcceptDate: Boolean) of Object;
  TCustomDateEvent = procedure (Sender : TObject; var ADate : string) of object;

  { TDateEdit }

  TDateEdit = class(TCustomEditButton)
  private
    FDefaultToday: Boolean;
    FDialogTitle: TCaption;
    FDisplaySettings: TDisplaySettings;
    FOnAcceptDate: TAcceptDateEvent;
    FOnCustomDate: TCustomDateEvent;
    FOKCaption: TCaption;
    FCancelCaption: TCaption;
    FDateFormat:string;
    function GetDate: TDateTime;
    function IsStoreTitle: boolean;
    procedure SetDate(Value: TDateTime);
    procedure CalendarPopupReturnDate(Sender: TObject; Const ADate: TDateTime);
  protected
    procedure DoButtonClick (Sender: TObject); override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DateFormatChanged; virtual;
    function GetDateFormat: string;
    property Date: TDateTime Read GetDate Write SetDate;
    property Button;
  published
    property DialogTitle:TCaption read FDialogTitle write FDialogTitle Stored IsStoreTitle;
    property CalendarDisplaySettings : TDisplaySettings read FDisplaySettings write FDisplaySettings;
    property OnAcceptDate : TAcceptDateEvent read FOnAcceptDAte write FOnAcceptDate;
    property OnCustomDate : TCustomDateEvent read FOnCustomDate write FOnCustomDate;
    property OKCaption:TCaption read FOKCaption write FOKCaption;
    property CancelCaption:TCaption read FCancelCaption write FCancelCaption;
    property ReadOnly default true;
    property DefaultToday: Boolean read FDefaultToday write FDefaultToday
      default False;

    property ButtonOnlyWhenFocused;
    property ButtonWidth;
    property Action;
    property Align;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BorderSpacing;
    property Color;
    property Constraints;
    property CharCase;
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
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
  end;

  
  { TCalcEdit }
  
  TAcceptValueEvent = procedure(Sender: TObject; var AValue: Double; var Action: Boolean) of Object;
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
    Function CreateCalcBitmap : TBitmap; virtual;
    procedure DoButtonClick (Sender: TObject); override;
    Procedure RunDialog; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  published
    // CalcEdit properties
    property CalculatorLayout : TCalculatorLayout read FLayout write Flayout;
    property AsFloat : Double read GetAsFloat write SetAsFloat;
    property AsInteger : Integer read GetAsInteger write SetAsInteger;
    property OnAcceptValue : TAcceptValueEvent read FOnAcceptValue write FOnAcceptValue;
    property DialogTitle : String read FDialogTitle write FDialogTitle Stored TitleStored;
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
    property BorderSpacing;
    property AutoSize;
    property AutoSelect;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
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
  end;

var
  FileOpenGlyph: TBitmap;
  DateGlyph: TBitmap;
  CalcGlyph: TBitmap;

const
  ResBtnFileOpen   = 'btnselfile';
  ResBtnSelDir     = 'btnseldir';
  ResBtnCalendar   = 'btncalendar';
  ResBtnCalculator = 'btncalculator';

procedure Register;

implementation

function GlyphFromBitmapOrResource(B: TBitmap;
  const ResourceName: String): TBitmap;
begin
  if Assigned(B) then
  begin
    Result := TBitmap.Create;
    Result.Assign(B);
  end
  else
    Result := LoadBitmapFromLazarusResource(ResourceName);
end;

function CreateFileOpenGlyph : TBitmap;
begin
  Result := GlyphFromBitmapOrResource(FileOpenGlyph,ResBtnFileOpen);
end;

function CreateDirectoryGlyph : TBitmap;
begin
  Result := GlyphFromBitmapOrResource(FileOpenGlyph,ResBtnSelDir);
end;

function CreateDateGlyph : TBitmap;
begin
  Result := GlyphFromBitmapOrResource(DateGlyph,ResBtnCalendar);
end;

function CreateCalcGlyph : TBitmap;
begin
  Result := GlyphFromBitmapOrResource(CalcGlyph,ResBtnCalculator);
end;


{ TEditBtn }

constructor TCustomEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FButton := TSpeedButton.Create(Self);
  FButton.Width := Self.Height;
  FButton.Height := Self.Height;
  FButton.FreeNotification(Self);
  CheckButtonVisible;
  FButton.OnClick := @DoButtonClick;
  FButton.Cursor := crArrow;
  FButton.ControlStyle := FButton.ControlStyle + [csNoDesignSelectable];
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

function TCustomEditButton.GetButtonHint: TTranslateString;
begin
  Result:=FButton.Hint;
end;

function TCustomEditButton.GetDirectInput: Boolean;
begin
  Result := not ReadOnly;
end;

function TCustomEditButton.GetFlat: Boolean;
begin
  If Assigned(FButton) then
    Result:=FButton.Flat
  else
    Result:=False;
end;

procedure TCustomEditButton.CheckButtonVisible;
begin
  If Assigned(FButton) then
    FButton.Visible:=(csdesigning in ComponentState) or
                     (Visible and (Focused or not FButtonNeedsFocus));
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
  ReadOnly := not AValue;
end;

procedure TCustomEditButton.SetFlat(const AValue: Boolean);
begin
  If Assigned(FButton) then
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

  if FButton<>nil then
    FButton.Enabled:=Enabled;
end;

function TCustomEditButton.GetMinHeight: Integer;
begin
  Result:=23;
end;

procedure TCustomEditButton.DoButtonClick (Sender: TObject);
begin
  If not ReadOnly then
    if Assigned(FOnButtonClick) then
      FOnButtonClick(Self);
end;

procedure TCustomEditButton.Loaded;
begin
  inherited Loaded;
  CheckButtonVisible;
end;

procedure TCustomEditButton.WMKillFocus(var Message: TLMKillFocus);
begin
  if FButtonNeedsFocus then
    FButton.Visible:=False;
  inherited;
end;

procedure TCustomEditButton.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if FButton <> nil then 
  begin
    FButton.Parent := Parent;
    CheckButtonVisible;
  end;
end;

procedure TCustomEditButton.DoSetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited DoSetBounds(ALeft, ATop, AWidth, AHeight);
  DoPositionButton;
end;

procedure TCustomEditButton.DoPositionButton;
begin
  if FButton <> nil then
    FButton.SetBounds(Left+Width, Top, FButton.Width, Height);
end;

procedure TCustomEditButton.WMSetFocus(var Message: TLMSetFocus);

begin
  FButton.Visible:=True;
  inherited;
end;

{ TFileNameEdit }

constructor TFileNameEdit.Create(AOwner: TComponent);
var
  ABitmap: TBitmap;
begin
  inherited Create(AOwner);
  FDialogFiles:=TStringList.Create;
  ABitmap:=CreateFileOpenBitmap;
  Glyph:=ABitmap;
  ABitmap.Free;
end;

destructor TFileNameEdit.Destroy;
begin
  FDialogFiles.Free;
  inherited Destroy;
end;

procedure TFileNameEdit.SetFileName(const AValue: String);
begin
  FFileNAme:=AValue;
  Text:=AValue;
end;

function TFileNameEdit.GetFileName: String;
begin
  Result:=Text;
end;

function TFileNameEdit.CreateDialog(AKind: TDialogKind): TCommonDialog;
var
  O: TOpenDialog;
  S: TSaveDialog;
begin
  Case AKind of
    dkopen, dkPictureOpen:
    begin
      O:=TOpenDialog.Create(Self);
      O.FileName:=FileName;
      O.Options:=DialogOptions;
      O.InitialDir:=InitialDir;
      O.Filter:=Filter;
      O.FilterIndex:=FilterIndex;
      Result:=O;
    end;
    dkSave, dkPictureSave:
    begin
      S:=TSaveDialog.Create(Self);
      S.Filter:=Filter;
      S.FilterIndex:=FilterIndex;
      Result:=S;
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
    dkOpen,dkPictureOpen :
    begin
      FN:=TOpenDialog(D).FileName;
      if (FN<>'') then
      begin
        if Assigned(FOnAcceptFN) then
          FOnAcceptFN(Self,Fn);
      end;
      if (FN<>'') then
      begin
        FileName:=FN;
        FDialogFiles.Text:=TOpenDialog(D).Files.Text;
      end;
    end;
    dkSave,dkPictureSave :
    begin
      FileName:=TSaveDialog(D).FileName;
      FDialogFiles.Clear;
    end;
  end;
end;

function TFileNameEdit.CreateFileOpenBitmap: TBitmap;
begin
  Result:=CreateFileOpenGlyph;
end;

procedure TFileNameEdit.DoButtonClick(Sender: TObject);
begin
  inherited DoButtonClick(Sender);
  RunDialog;
end;

procedure TFileNameEdit.RunDialog;
var
  D : TCommonDialog;
begin
  D:=CreateDialog(DialogKind);
  try
    if D.Execute then
      SaveDialogResult(DialogKind,D);
  finally
    D.Free;
  end
end;

{ TDirectoryEdit }

constructor TDirectoryEdit.Create(AOwner: TComponent);
var
  ABitmap: TBitmap;
begin
  inherited Create(AOwner);
  ABitmap:=CreateDirectoryBitmap;
  Glyph:=ABitmap;
  ABitmap.Free;
end;

destructor TDirectoryEdit.Destroy;
begin
  inherited Destroy;
end;

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
end;

function TDirectoryEdit.GetDialogResult(D: TCommonDialog) : String;
begin
  Result:=TSelectDirectoryDialog(D).FileName;
end;


function TDirectoryEdit.CreateDirectoryBitmap: TBitmap;
begin
  Result:=CreateDirectoryGlyph;
end;

procedure TDirectoryEdit.DoButtonClick(Sender: TObject);
begin
  inherited DoButtonClick(Sender);
  RunDialog;
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
var
  ABitmap: TBitmap;
begin
  inherited Create(AOwner);
  FDisplaySettings:=[dsShowHeadings, dsShowDayNames];
  ReadOnly:=true;
  DialogTitle:=rsPickDate;
  OKCaption:='OK';
  CancelCaption:='Cancel';
  ABitmap:=CreateDateGlyph;
  Button.Glyph:=ABitmap;
  ABitmap.Free;
  Button.OnClick:= @DoButtonClick;
  DateFormatChanged;
end;

destructor TDateEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TDateEdit.DateFormatChanged;
begin
  FDateFormat := LongDateFormat;
end;

function TDateEdit.GetDateFormat: string;
begin
  Result := FDateFormat;
end;

procedure TDateEdit.DoButtonClick(Sender:TObject);//or onClick
var
  PopupOrigin:TPoint;
begin
  inherited DoButtonClick(Sender);

  PopupOrigin:=ControlToScreen(Point(0, Height));
  ShowCalendarPopup(PopupOrigin, Date, @CalendarPopupReturnDate);
end;

procedure TDateEdit.DblClick;
begin
  inherited DblClick;
  DoButtonClick(nil);
end;

function TDateEdit.GetDate: TDateTime;
var
  ADate: string;
begin
  if FDefaultToday then Result := SysUtils.Date
  else Result := NullDate;
  ADate := Trim(Text);
  if ADate<>'' then
  begin
    if Assigned(FOnCustomDate) then
      FOnCustomDate(Self, ADate);
    Result := StrToDateDef(ADate, Result);
  end;
end;

function TDateEdit.IsStoreTitle: boolean;
begin
  Result:=DialogTitle<>rsPickDate;
end;

procedure TDateEdit.SetDate(Value:TDateTime);
var
  D: TDateTime;
begin
  if {not IsValidDate(Value) or }(Value = NullDate) then
  begin
    if DefaultToday then Value := SysUtils.Date
    else Value := NullDate;
  end;
  D := Self.Date;
  if Value = NullDate then
    Text := ''
  else
    Text := DateToStr(Value);
  if D <> Date then
    Change;
end;

procedure TDateEdit.CalendarPopupReturnDate(Sender: TObject;
  const ADate: TDateTime);
var
  B:Boolean;
  D:TDateTime;
begin
  try
    B:=true;
    D:=ADate;
    if Assigned(FOnAcceptDate) then
      FOnAcceptDate(Self, D, B);
   if B then
      Self.Date:=D;
  except
    on E:Exception do
      MessageDlg(E.Message, mtError, [mbOK], 0);
  end;
end;

{ TCalcEdit }

function TCalcEdit.GetAsFloat: Double;
begin
  try
    Result:=StrToDouble(Trim(Text));
  except
    Result:=0.0;
  end;
end;

function TCalcEdit.GetAsInteger: Integer;
begin
  Result:=StrToIntDef(Text,0);
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

function TCalcEdit.CreateCalcBitmap: TBitmap;
begin
  Result:=CreateCalcGlyph;
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
var
  ABitmap: TBitmap;
begin
  inherited Create(AOwner);
  ABitmap:=CreateCalcBitmap;
  Glyph:=ABitmap;
  ABitmap.Free;
  FdialogTitle:=rsCalculator;
end;

destructor TCalcEdit.Destroy;
begin
  inherited Destroy;
end;

procedure Register;
begin
  RegisterComponents('Misc', [TEditButton,TFileNameEdit,TDirectoryEdit,
                              TDateEdit,TCalcEdit]);
end;

initialization
{$i lcl_edbtnimg.lrs}

end.
