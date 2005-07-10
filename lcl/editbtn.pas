{
 /***************************************************************************
                               edirbtn.pas
                               -----------
                Component Library Extended dialogs Controls


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
unit EditBtn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, LCLStrConsts, LCLType, LMessages,
  Graphics, Controls, Forms, FileUtil, Dialogs, StdCtrls, Buttons, Calendar,
  ExtDlgs;

type
  { TCustomEditButton }

  TCustomEditButton = class(TEdit)
  private
    FButton: TSpeedButton;
    FButtonNeedsFocus: Boolean;
    FDirectInput : Boolean;
    FOnButtonClick : TNotifyEvent;
    function GetButtonWidth: Integer;
    function GetFlat: Boolean;
    Procedure CheckButtonVisible;
    procedure SetButtonNeedsFocus(const AValue: Boolean);
    procedure SetButtonWidth(const AValue: Integer);
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
    Property ButtonWidth : Integer Read GetButtonWidth write SetButtonWidth;
    property DirectInput : Boolean read FDirectInput write FDirectInput Default True;
    property Glyph : TBitmap read GetGlyph write SetGlyph;
    property NumGlyphs : Integer read GetNumGlyphs write SetNumGlyphs;
    property OnButtonClick : TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property Button: TSpeedButton read FButton;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Property Flat : Boolean Read GetFlat Write SetFlat;
    Property ButtonOnlyWhenFocused : Boolean Read FButtonNeedsFocus Write SetButtonNeedsFocus;
  end;
  
  
  { TEditButton }
  
  TEditButton = Class(TCustomEditButton)
  Public
    Property Button;
  published
    property AutoSize;
    property Align;
    property Anchors;
    property BorderSpacing;
    Property ButtonOnlyWhenFocused;
    Property ButtonWidth;
    property Color;
    property Ctl3D;
    Property DirectInput;
    property DragCursor;
    property DragMode;
    property Enabled;
    Property Flat;
    property Font;
    property Glyph;
    property MaxLength;
    property NumGlyphs;
    Property OnButtonClick;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
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
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
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
  Protected
    Function CreateDialog(AKind : TDialogKind) : TCommonDialog; Virtual;
    Procedure SaveDialogResult(AKind : TDialogKind; D : TCommonDialog); Virtual;
    Function CreateFileOpenBitmap : TBitmap; Virtual;
    procedure DoButtonClick (Sender: TObject); override;
    Procedure RunDialog; Virtual;
  Public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Property DialogFiles : TStrings Read FDialogFiles;
  Published
    // TFileName properties.
    Property FileName : String Read GetFileName Write SetFileName;
    Property InitialDir : String Read FInitialDir Write FInitialDir;
    Property OnAcceptFileName : TAcceptFileNameEvent Read FOnAcceptFN Write FonAcceptFN;
    Property DialogKind : TDialogKind Read FDialogKind Write FDialogKind;
    Property DialogTitle : String Read FDialogTitle Write FDialogTitle;
    Property DialogOptions : TOpenOptions Read FDialogOptions Write FDialogOptions;
    Property Filter : String Read FFilter Write FFilter;
    Property FilterIndex : Integer Read FFilterIndex Write FFIlterIndex;
    // TEditButton properties.
    Property ButtonWidth;
    Property DirectInput;
    Property ButtonOnlyWhenFocused;
    // property Glyph;
    property NumGlyphs;
    Property Flat;
    // Other properties
    property Align;
    property Anchors;
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
  Protected
    Function CreateDialog : TCommonDialog; Virtual;
    Function GetDialogResult(D : TCommonDialog) : String; Virtual;
    Function CreateDirectoryBitmap : TBitmap; Virtual;
    procedure DoButtonClick (Sender: TObject); override;
    Procedure RunDialog; Virtual;
  Public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  Published
    // TDirectory properties.
    Property Directory : String Read GetDirectory Write SetDirectory;
    Property RootDir : String Read FRootDir Write FRootDir;
    Property OnAcceptDirectory : TAcceptFileNameEvent Read FOnAcceptDir Write FonAcceptDir;
    Property DialogTitle : String Read FDialogTitle Write FDialogTitle;
    Property ShowHidden : Boolean Read FShowHidden Write FShowHidden;
    // TEditButton properties.
    Property ButtonWidth;
    Property DirectInput;
    Property ButtonOnlyWhenFocused;
    // property Glyph;
    property NumGlyphs;
    Property Flat;
    // Other properties
    property Align;
    property Anchors;
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

  { TDateEdit }

  TDateEdit = class(TEditButton)
  private
    FDialogTitle: TCaption;
    FDisplaySettings: TDisplaySettings;
    FOnAcceptDate: TAcceptDateEvent;

    FOKCaption: TCaption;
    FCancelCaption: TCaption;
    FDate: TDateTime;
    function IsStoreTitle: boolean;
    procedure SetDate(const Value: TDateTime);
  protected
    procedure DoButtonClick (Sender: TObject); override;
    procedure Change; override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DateFormatChanged; virtual;
  published
    property DialogTitle:TCaption Read FDialogTitle Write FDialogTitle Stored IsStoreTitle;
    Property CalendarDisplaySettings : TDisplaySettings Read FDisplaySettings Write FDisplaySettings;
    Property OnAcceptDate : TAcceptDateEvent Read FOnAcceptDAte Write FOnAcceptDate;
    property OKCaption:TCaption Read FOKCaption Write FOKCaption;
    property CancelCaption:TCaption Read FCancelCaption Write FCancelCaption;
    property Date: TDateTime Read FDate Write SetDate;
    property ReadOnly default true;
  end;

  
  { TCalcEdit }
  
  TAcceptValueEvent = Procedure (Sender : TObject; Var AValue : Double; Var Action : Boolean) of Object;
  TCalcEdit = Class(TCustomEditButton)
  private
    FDialogTitle: String;
    FLayout: TCalculatorLayout;
    FOnAcceptValue: TAcceptValueEvent;
    function GetAsFloat: Double;
    function GetAsInteger: Integer;
    procedure SetAsFloat(const AValue: Double);
    procedure SetAsInteger(const AValue: Integer);
    function TitleStored: boolean;
  Protected
    FCalcDialog : TForm;
    Function CreateCalcBitmap : TBitmap; Virtual;
    procedure DoButtonClick (Sender: TObject); override;
    Procedure RunDialog; Virtual;
  Public
    constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
  Published
    // CalcEdit properties
    Property CalculatorLayout : TCalculatorLayout Read FLayout Write Flayout;
    Property AsFloat : Double Read GetAsFloat Write SetAsFloat;
    Property AsInteger : Integer Read GetAsInteger Write SetAsInteger;
    Property OnAcceptValue : TAcceptValueEvent Read FOnAcceptValue Write FOnAcceptValue;
    Property DialogTitle : String Read FDialogTitle Write FDialogTitle Stored TitleStored;
    // TEditButton properties.
    Property ButtonWidth;
    Property DirectInput;
    Property ButtonOnlyWhenFocused;
    // property Glyph;
    property NumGlyphs;
    Property Flat;
    // Other properties
    property Align;
    property Anchors;
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

Var
  FileOpenGlyph : TBitmap;
  DateGlyph : TBitmap;
  CalcGlyph : TBitmap;

Const
  ResBtnFileOpen   = 'btnselfile';
  ResBtnSelDir     = 'btnseldir';
  ResBtnCalendar   = 'btncalendar';
  ResBtnCalculator = 'btncalculator';

procedure Register;

implementation

Function GlyphFromBitmapOrResource(B : TBitmap;
  const ResourceName : String) : TBitmap;
begin
  Result:=TBitmap.Create;
  If Assigned(B) then
    Result.Assign(B)
  else
    Result.LoadFromLazarusResource(ResourceName);
end;

Function CreateFileOpenGlyph : TBitmap;

begin
  Result:=GlyphFromBitmapOrResource(FileOpenGlyph,ResBtnFileOpen);
end;

Function CreateDirectoryGlyph : TBitmap;
begin
  Result:=GlyphFromBitmapOrResource(FileOpenGlyph,ResBtnSelDir);
end;

Function CreateDateGlyph : TBitmap;

begin
  Result:=GlyphFromBitmapOrResource(DateGlyph,ResBtnCalendar);
end;

Function CreateCalcGlyph : TBitmap;

begin
  Result:=GlyphFromBitmapOrResource(CalcGlyph,ResBtnCalculator);
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
  ControlStyle := ControlStyle - [csSetCaption];
  FDirectInput := True;
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

procedure TCustomEditButton.SetButtonNeedsFocus(const AValue: Boolean);
begin
  If FButtonNeedsFocus<>AValue then
    begin
    FButtonNeedsFocus:=AValue;
    CheckButtonVisible;
    end;
end;

procedure TCustomEditButton.SetButtonWidth(const AValue: Integer);
begin
  FButton.Width:=AValue;
end;

procedure TCustomEditButton.SetFlat(const AValue: Boolean);
begin
  If Assigned(FButton) then
    FButton.Flat:=AValue;
end;

function TCustomEditButton.GetGlyph : TBitmap;
Begin
  Result:=FButton.Glyph;
end;

procedure TCustomEditButton.SetNumGlyphs(ANumber: Integer);
Begin
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
  If Not ReadOnly then
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
  If FButtonNeedsFocus then
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
begin
  inherited Create(AOwner);
  FDialogFiles:=TStringList.Create;
  Glyph:=CreateFileOpenBitmap;
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

Var
  O : TOpenDialog;
  S : TSaveDialog;
  
begin
  Case AKind of
    dkopen,dkPictureOpen :
      begin
      O:=TOpenDialog.Create(Self);
      O.FileName:=FileName;
      O.Options:=DialogOptions;
      O.InitialDir:=InitialDir;
      O.Filter:=Filter;
      O.FilterIndex:=FilterIndex;
      Result:=O;
      end;
    dkSave,dkPictureSave :
      begin
      S:=TSaveDialog.Create(Self);
      S.Filter:=Filter;
      S.FilterIndex:=FilterIndex;
      Result:=S;
      end;
  end;
  // Set some common things.
  Result.Title:=DialogTitle;
end;

procedure TFileNameEdit.SaveDialogResult(AKind: TDialogKind; D: TCommonDialog);

Var
  FN : String;

begin
  Case AKind of
    dkOpen,dkPictureOpen :
      begin
      FN:=TOpenDialog(D).FileName;
      If (FN<>'') then
        begin
        If Assigned(FOnAcceptFN) then
          FOnAcceptFN(Self,Fn);
        end;
      If (FN<>'') then
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

Var
  D : TCommonDialog;

begin
  D:=CreateDialog(DialogKind);
  Try
    If D.Execute then
      SaveDialogResult(DialogKind,D);
  Finally
    D.Free;
  end
end;

{ TDirectoryEdit }

constructor TDirectoryEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Glyph:=CreateDirectoryBitmap;
end;

destructor TDirectoryEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TDirectoryEdit.SetDirectory(const AValue: String);
begin
  if (Text<>AValue) then
    begin
    Text:=AValue;
    end;
end;

function TDirectoryEdit.CreateDialog: TCommonDialog;
begin
  Result:=TSelectDirectoryDialog.Create(Self);
  if DirPathExists(Directory) then begin
    TSelectDirectoryDialog(Result).InitialDir:=Directory;
    TSelectDirectoryDialog(Result).FileName:='';
  end else begin
    TSelectDirectoryDialog(Result).InitialDir:=RootDir;
    TSelectDirectoryDialog(Result).FileName:=Directory;
  end;
end;

Function TDirectoryEdit.GetDialogResult(D: TCommonDialog) : String;
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

Var
  D : String;
  Dlg : TCommonDialog;
  B : Boolean;
  
begin
  Dlg:=CreateDialog;
  Try
    B:=Dlg.Execute;
    If B then
      D:=GetDialogResult(Dlg);
  Finally
    Dlg.Free;
  end;
  If B then
    begin
    If Assigned(FOnAcceptDir) then
      begin
      FOnAcceptdir(Self,D);
      If (D<>'') then
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

constructor TDateEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDate:=Now;
  Date:=trunc(FDate);
  Text:=DateToStr(Date);
  FDisplaySettings:=[dsShowHeadings, dsShowDayNames];
  DialogTitle:='Select a date';
  OKCaption:='OK';
  CancelCaption:='Cancel';
  ReadOnly:=true;
  Color:=clBtnFace;
  Button.Glyph:=CreateDateGlyph;//.LoadFromLazarusResource('DateEditGlyph');  //GlyphFromBitmapOrResource(DateGlyph,ResBtnCalendar)
  Button.OnClick:= @DoButtonClick;
//  OnChange:=@Change;
//  OnDblClick:=@DoButtonClick;
end;

destructor TDateEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TDateEdit.DateFormatChanged;
begin
  Text:=DateToStr(FDate);
end;

procedure TDateEdit.DoButtonClick(Sender:TObject);//or onClick
var CD:TCalendarDialog;
    B:Boolean;
    D:TDateTime;
begin
  inherited DoButtonClick(Sender);

  CD:=TCalendarDialog.Create(Self);
  with CD do begin
    Date:=Self.Date;
    CancelCaption:=Self.CancelCaption;
    OKCaption:=Self.OKCaption;
    DialogTitle:=Self.DialogTitle;
    DisplaySettings:=Self.CalendarDisplaySettings;
  end;
  try
    if CD.Execute then begin
      FDate:=CD.Date;
      Self.Date:=FDate;
      D:=FDate;
      B:=true;
      If Assigned(FOnAcceptDate) then
        FOnAcceptDate(Self,D,B);
      Text:=DateToStr(FDate);
    end;
  except
    raise Exception.Create('Errore in calendar dialog di dateedit');
  end;
  FreeAndNil(CD);
end;

procedure TDateEdit.Change;//(Sender:TObject);
var Datetmp: TDate;
begin
  inherited Change;//(Sender);
  //debugln('TDateEdit.Change Text="',Text,'"');
  try
    Datetmp:=StrToDate(Text);
    // if this worked, then adjust it to current format
    Date:=Datetmp;
  except
    // invalid date: the user is probably currently editing
  end;
end;

procedure TDateEdit.DblClick;
begin
  inherited DblClick;
  DoButtonClick(nil);
end;


function TDateEdit.IsStoreTitle: boolean;
begin
  Result:=DialogTitle<>rsPickDate;
end;

procedure TDateEdit.SetDate(const Value:TDateTime);
begin
  if FDate=Value then exit;
  FDate:=Value;
  Text:=DateToStr(FDate);
end;



{ TCalcEdit }

function TCalcEdit.GetAsFloat: Double;
begin
  Try
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

Var
  D : Double;
  B : Boolean;
  
begin
  D:=AsFloat;
  With CreateCalculatorForm(Self,FLayout,0) do
    Try
      Caption:=DialogTitle;
      Value:=D;
      If (ShowModal=mrOK) then
        begin
        D:=Value;
        B:=True;
        If Assigned(FOnAcceptValue) then
          FOnAcceptValue(Self,D,B);
        If B then
          AsFloat:=D;
        end;
    Finally
      Free;
    end;
end;

constructor TCalcEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Glyph:=CreateCalcBitmap;
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

Initialization

{$i edbtnimg.lrs}

Finalization

end.
