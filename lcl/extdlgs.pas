{
 /***************************************************************************
                               extdlgs.pas
                               -----------
                Component Library Extended dialogs Controls


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit ExtDlgs;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, LCLProc, LResources, LCLType, LCLStrConsts,
  InterfaceBase, LCLPlatformDef,
  FileUtil, LazFileUtils, Controls, Dialogs, GraphType, Graphics, ExtCtrls,
  StdCtrls, Forms, Calendar, Buttons, Masks, CalcForm;

type

  { TPreviewFileControl }

  TPreviewFileDialog = class;

  TPreviewFileControl = class(TWinControl)
  private
    FPreviewFileDialog: TPreviewFileDialog;
  protected
    class procedure WSRegisterClass; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure SetPreviewFileDialog(const AValue: TPreviewFileDialog);
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(TheOwner: TComponent); override;
    property PreviewFileDialog: TPreviewFileDialog read FPreviewFileDialog
                                                   write SetPreviewFileDialog;
  end;

  { TPreviewFileDialog }

  TPreviewFileDialog = class(TOpenDialog)
  private
    FPreviewFileControl: TPreviewFileControl;
  protected
    class procedure WSRegisterClass; override;
    procedure CreatePreviewControl; virtual;
    procedure InitPreviewControl; virtual;
    function DoExecute: boolean; override;
  public
    constructor Create(TheOwner: TComponent); override;
    property PreviewFileControl: TPreviewFileControl read FPreviewFileControl;
  end;

  { TOpenPictureDialog }

  TOpenPictureDialog = class(TPreviewFileDialog)
  private
    FDefaultFilter: string;
    FImageCtrl: TImage;
    FPictureGroupBox: TGroupBox;
    FPreviewFilename: string;
  protected
    class procedure WSRegisterClass; override;
    function  IsFilterStored: Boolean; virtual;
    property ImageCtrl: TImage read FImageCtrl;
    property PictureGroupBox: TGroupBox read FPictureGroupBox;
    procedure InitPreviewControl; override;
    procedure ClearPreview; virtual;
    procedure UpdatePreview; virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure DoClose; override;
    procedure DoSelectionChange; override;
    procedure DoShow; override;
    function GetFilterExt: String;
    property DefaultFilter: string read FDefaultFilter;
  published
    property Filter stored IsFilterStored;
  end;

  { TSavePictureDialog }

  TSavePictureDialog = class(TOpenPictureDialog)
  protected
    class procedure WSRegisterClass; override;
    function DefaultTitle: string; override;
  public
    constructor Create(TheOwner: TComponent); override;
  end;

  { TExtCommonDialog }

  // A common base class for custom drawn dialogs (Calculator and Calendar).
  TExtCommonDialog = class(TCommonDialog)
  private
    FDialogPosition: TPosition;
    FLeft: Integer;
    FTop: Integer;
    FDlgForm: TCustomForm;
  protected
    function GetLeft: Integer; virtual;
    function GetHeight: Integer; override;
    function GetTop: Integer; virtual;
    function GetWidth: Integer; override;
    procedure SetLeft(AValue: Integer); virtual;
    procedure SetTop(AValue: Integer); virtual;
    property DlgForm: TCustomForm read FDlgForm write FDlgForm;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
  published
    property DialogPosition: TPosition read FDialogPosition write FDialogPosition default poMainFormCenter;
  end;

  { TCalculatorDialog }

  TCalculatorDialog = class(TExtCommonDialog)
  private
    FLayout: TCalculatorLayout;
    FValue: Double;
    FMemory: Double;
    FPrecision: Byte;
    FBeepOnError: Boolean;
    FOnChange: TNotifyEvent;
    FOnCalcKey: TKeyPressEvent;
    FOnDisplayChange: TNotifyEvent;
    FDialogScale: integer;
    FColorBtnDigits,
    FColorBtnOthers,
    FColorBtnMemory,
    FColorBtnOk,
    FColorBtnCancel,
    FColorBtnClear,
    FColorDisplayText,
    FColorDisplayBack: TColor;
    function GetDisplay: Double;
    procedure SetDialogScale(AValue: integer);
  protected
    class procedure WSRegisterClass; override;
    procedure OnDialogClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OnDialogShow(Sender: TObject);
    procedure OnDialogCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure Change; virtual;
    procedure CalcKey(var Key: char); virtual;
    function DefaultTitle: string; override;
    procedure DisplayChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    property CalcDisplay: Double read GetDisplay;
    property Memory: Double read FMemory;
  published
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property CalculatorLayout: TCalculatorLayout read FLayout write FLayout default clNormal;
    property Precision: Byte read FPrecision write FPrecision default CalcDefPrecision;
    property Title;
    property Value: Double read FValue write FValue;
    property OnCalcKey: TKeyPressEvent read FOnCalcKey write FOnCalcKey;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDisplayChange: TNotifyEvent read FOnDisplayChange write FOnDisplayChange;
    property DialogScale: integer read FDialogScale write SetDialogScale default 100;
    property ColorBtnDigits: TColor read FColorBtnDigits write FColorBtnDigits;
    property ColorBtnMemory: TColor read FColorBtnMemory write FColorBtnMemory;
    property ColorBtnOk: TColor read FColorBtnOk write FColorBtnOk;
    property ColorBtnCancel: TColor read FColorBtnCancel write FColorBtnCancel;
    property ColorBtnClear: TColor read FColorBtnClear write FColorBtnClear;
    property ColorBtnOthers: TColor read FColorBtnOthers write FColorBtnOthers;
    property ColorDisplayText: TColor read FColorDisplayText write FColorDisplayText;
    property ColorDisplayBack: TColor read FColorDisplayBack write FColorDisplayBack;
  end;

  { TCalendarDialog }

  TCalendarDialog = class(TExtCommonDialog)
  private
    FDate: TDateTime;
    FDayChanged: TNotifyEvent;
    FDisplaySettings: TDisplaySettings;
    FMonthChanged: TNotifyEvent;
    FYearChanged: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOKCaption: TCaption;
    FCancelCaption: TCaption;
    FCalendar: TCalendar;
    okButton: TButton;
    cancelButton: TButton;
    panel: TPanel;
    procedure OnDialogClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OnDialogCloseQuery(Sender : TObject; var CanClose : boolean);
    procedure OnDialogShow(Sender: TObject);
    procedure OnCalendarDayChanged(Sender: TObject);
    procedure OnCalendarMonthChanged(Sender: TObject);
    procedure OnCalendarYearChanged(Sender: TObject);
    procedure OnCalendarChange(Sender: TObject);
  protected
    class procedure WSRegisterClass; override;
    procedure GetNewDate(Sender:TObject);//or onClick
    procedure CalendarDblClick(Sender: TObject);
    function DefaultTitle: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
    property Left: Integer read GetLeft write SetLeft;
    property Top: Integer read GetTop write SetTop;
  published
    property Date: TDateTime read FDate write FDate;
    property DisplaySettings: TDisplaySettings read FDisplaySettings write FDisplaySettings default DefaultDisplaySettings;
    property OnDayChanged: TNotifyEvent read FDayChanged write FDayChanged;
    property OnMonthChanged: TNotifyEvent read FMonthChanged write FMonthChanged;
    property OnYearChanged: TNotifyEvent read FYearChanged write FYearChanged;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OKCaption: TCaption read FOKCaption write FOKCaption;
    property CancelCaption: TCaption read FCancelCaption write FCancelCaption;
  end;

procedure Register;

implementation

//no need as buttons don't have glyphs now
//{$R lcl_calc_images.res}

uses
  WSExtDlgs, Math;

procedure Register;
begin
  RegisterComponents('Dialogs',[TOpenPictureDialog,TSavePictureDialog,
                                TCalendarDialog,TCalculatorDialog]);
end;

{ TPreviewFileControl }

class procedure TPreviewFileControl.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterPreviewFileControl;
end;

procedure TPreviewFileControl.SetPreviewFileDialog(
  const AValue: TPreviewFileDialog);
begin
  if FPreviewFileDialog=AValue then exit;
  FPreviewFileDialog:=AValue;
end;

procedure TPreviewFileControl.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Params.WndParent = 0 then
    Params.Style := Params.Style and not WS_CHILD;
end;

class function TPreviewFileControl.GetControlClassDefaultSize: TSize;
begin
  Result.CX := 200;
  Result.CY := 200;
end;

constructor TPreviewFileControl.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle:=csPreviewFileControl;
  SetInitialBounds(0, 0, GetControlClassDefaultSize.CX, GetControlClassDefaultSize.CY);
end;

{ TPreviewFileDialog }

class procedure TPreviewFileDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterPreviewFileDialog;
end;

procedure TPreviewFileDialog.CreatePreviewControl;
begin
  if FPreviewFileControl<>nil then exit;
  FPreviewFileControl:=TPreviewFileControl.Create(Self);
  FPreviewFileControl.PreviewFileDialog:=Self;
  InitPreviewControl;
end;

procedure TPreviewFileDialog.InitPreviewControl;
begin
  FPreviewFileControl.Name:='PreviewFileControl';
end;

function TPreviewFileDialog.DoExecute: boolean;
begin
  CreatePreviewControl;
  Result:=inherited DoExecute;
end;

constructor TPreviewFileDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FCompStyle:=csPreviewFileDialog;
end;

{ TOpenPictureDialog }

class procedure TOpenPictureDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterOpenPictureDialog;
end;

function TOpenPictureDialog.IsFilterStored: Boolean;
begin
  Result := (Filter<>FDefaultFilter);
end;

procedure TOpenPictureDialog.DoClose;
begin
  ClearPreview;
  inherited DoClose;
end;

procedure TOpenPictureDialog.DoSelectionChange;
begin
  UpdatePreview;
  inherited DoSelectionChange;
end;

procedure TOpenPictureDialog.DoShow;
begin
  ClearPreview;
  inherited DoShow;
end;

procedure TOpenPictureDialog.InitPreviewControl;
begin
  inherited InitPreviewControl;
  FPictureGroupBox.Parent:=PreviewFileControl;
end;

procedure TOpenPictureDialog.ClearPreview;
begin
  FPictureGroupBox.Caption:='None';
  FImageCtrl.Picture:=nil;
end;

procedure TOpenPictureDialog.UpdatePreview;
var
  CurFilename: String;
  FileIsValid: boolean;
begin
  CurFilename := FileName;
  if CurFilename = FPreviewFilename then exit;

  FPreviewFilename := CurFilename;
  FileIsValid := FileExistsUTF8(FPreviewFilename)
                 and (not DirPathExists(FPreviewFilename))
                 and FileIsReadable(FPreviewFilename);
  if FileIsValid then
    try
      FImageCtrl.Picture.LoadFromFile(FPreviewFilename);
      FPictureGroupBox.Caption := Format('(%dx%d)',
        [FImageCtrl.Picture.Width, FImageCtrl.Picture.Height]);
    except
      FileIsValid := False;
    end;
  if not FileIsValid then
    ClearPreview;
end;

constructor TOpenPictureDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDefaultFilter := GraphicFilter(TGraphic)+'|'+
                       Format(rsAllFiles,[GetAllFilesMask, GetAllFilesMask,'']);
  Filter:=FDefaultFilter;

  FPictureGroupBox:=TGroupBox.Create(Self);
  with FPictureGroupBox do begin
    Name:='FPictureGroupBox';
    Align:=alClient;
  end;

  FImageCtrl:=TImage.Create(Self);
  with FImageCtrl do begin
    Name:='FImageCtrl';
    Parent:=FPictureGroupBox;
    Align:=alClient;
    Center:=true;
    Proportional:=true;
  end;
end;

function TOpenPictureDialog.GetFilterExt: String;
var
  ParsedFilter: TParseStringList;
begin
  Result := '';
  
  ParsedFilter := TParseStringList.Create(Filter, '|');
  try
    if (FilterIndex > 0) and (FilterIndex * 2 <= ParsedFilter.Count) then
    begin
      Result := AnsiLowerCase(ParsedFilter[FilterIndex * 2 - 1]);
      // remove *.*
      if (Result <> '') and (Result[1] = '*') then Delete(Result, 1, 1);
      if (Result <> '') and (Result[1] = '.') then Delete(Result, 1, 1);
      if (Result <> '') and (Result[1] = '*') then Delete(Result, 1, 1);
      // remove all after ;
      if Pos(';', Result) > 0 then Delete(Result, Pos(';', Result), MaxInt);
    end;
    
    if Result = '' then Result := DefaultExt;
  finally
    ParsedFilter.Free;
  end;
end;

{ TSavePictureDialog }

class procedure TSavePictureDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterSavePictureDialog;
end;

function TSavePictureDialog.DefaultTitle: string;
begin
  Result := rsfdFileSaveAs;
end;

constructor TSavePictureDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fCompStyle:=csSaveFileDialog;
end;

{ ---------------------------------------------------------------------
  Auxiliary
  ---------------------------------------------------------------------}

procedure SetDefaultFont(AFont: TFont; Layout: TCalculatorLayout);

begin
  with AFont do
  begin
    Color := clWindowText;
    Name := 'MS Sans Serif';
    Size := 8;
    Style := [fsBold];
  end;
end;


{ TExtCommonDialog }

function TExtCommonDialog.GetLeft: Integer;
begin
  if Assigned(FDlgForm) then FLeft := FDlgForm.Left;
  Result := FLeft;
end;

function TExtCommonDialog.GetHeight: Integer;
begin
  if Assigned(DlgForm) then
    Result := DlgForm.Height
  else
    Result := inherited GetHeight;
end;

function TExtCommonDialog.GetTop: Integer;
begin
  if Assigned(FDlgForm) then FTop := FDlgForm.Top;
  Result := FTop;
end;

function TExtCommonDialog.GetWidth: Integer;
begin
  if Assigned(DlgForm) then
    Result := DlgForm.Width
  else
    Result := inherited GetWidth;
end;

procedure TExtCommonDialog.SetLeft(AValue: Integer);
begin
  if Assigned(FDlgForm) then FDlgForm.Left := AValue;
  FLeft := AValue;
end;

procedure TExtCommonDialog.SetTop(AValue: Integer);
begin
  if Assigned(FDlgForm) then FDlgForm.Top := AValue;
  FTop := AValue;
end;

constructor TExtCommonDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDialogPosition := poMainFormCenter;     // Set the initial location on screen.
end;

destructor TExtCommonDialog.Destroy;
begin
  inherited Destroy;
end;


{ TCalculatorDialog }

constructor TCalculatorDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrecision:=CalcDefPrecision;
  FBeepOnError:=True;
  FDialogScale:=100;
  FLayout:=clNormal;

  FColorBtnDigits:=cColorBtnDigits;
  FColorBtnOthers:=cColorBtnOthers;
  FColorBtnMemory:=cColorBtnMemory;
  FColorBtnOk:=cColorBtnOk;
  FColorBtnCancel:=cColorBtnCancel;
  FColorBtnClear:=cColorBtnClear;
  FColorDisplayText:=cColorDisplayText;
  FColorDisplayBack:=cColorDisplayBack;
end;

destructor TCalculatorDialog.Destroy;
begin
  FOnChange:=nil;
  FOnDisplayChange:=nil;
  inherited Destroy;
end;

class procedure TCalculatorDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCalculatorDialog;
end;

procedure TCalculatorDialog.OnDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  DoClose;
end;

procedure TCalculatorDialog.OnDialogShow(Sender: TObject);
begin
  DoShow;
end;

procedure TCalculatorDialog.OnDialogCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  UserChoice := DlgForm.ModalResult;
  DoCanClose(CanClose);
end;

function TCalculatorDialog.GetDisplay: Double;
begin
  if Assigned(DlgForm) then
    Result:=TCalculatorForm(DlgForm).CalcPanel.DisplayValue
  else Result:=FValue;
end;

procedure TCalculatorDialog.SetDialogScale(AValue: integer);
const
  cMinSize = 80;
  cMaxSize = 400;
begin
  if FDialogScale=AValue then Exit;
  FDialogScale:=Max(cMinSize, Min(cMaxSize, AValue));
end;

procedure TCalculatorDialog.CalcKey(var Key: char);
begin
  if Assigned(FOnCalcKey) then FOnCalcKey(Self, Key);
end;

function TCalculatorDialog.DefaultTitle: string;
begin
  Result := rsCalculator;
end;

procedure TCalculatorDialog.DisplayChange;
begin
  if Assigned(FOnDisplayChange) then FOnDisplayChange(Self);
end;


procedure TCalculatorDialog.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TCalculatorDialog.Execute: Boolean;
var
  CPanel: TCalculatorPanel;
begin
  cColorBtnDigits:=FColorBtnDigits;
  cColorBtnOthers:=FColorBtnOthers;
  cColorBtnMemory:=FColorBtnMemory;
  cColorBtnOk:=FColorBtnOk;
  cColorBtnCancel:=FColorBtnCancel;
  cColorBtnClear:=FColorBtnClear;
  cColorDisplayText:=FColorDisplayText;
  cColorDisplayBack:=FColorDisplayBack;

  DlgForm:=CreateCalculatorForm(Application, FLayout, HelpContext);
  try
    ResetShowCloseFlags;
    (DlgForm as TCalculatorForm).OnCalcKey:= @Self.CalcKey;
    (DlgForm as TCalculatorForm).OnDisplayChange:= @Self.DisplayChange;
    (DlgForm as TCalculatorForm).OnShow := @Self.OnDialogShow;
    (DlgForm as TCalculatorForm).OnClose := @Self.OnDialogClose;
    (DlgForm as TCalculatorForm).OnCloseQuery :=@Self.OnDialogCloseQuery;

    if FDialogScale<>100 then
      DlgForm.ScaleBy(FDialogScale,100);
    if (csDesigning in ComponentState) then
      DlgForm.Position:=poScreenCenter
    else
      DlgForm.Position:=DialogPosition;
    if (DlgForm.Position=poDesigned) then begin
      DlgForm.Left:=FLeft;
      DlgForm.Top:=FTop;
    end else begin
      FLeft:=DlgForm.Left;
      FTop:=DlgForm.Top;
    end;
    CPanel:=TCalculatorForm(DlgForm).CalcPanel;

    DlgForm.Caption:=Title;
    CPanel.Memory:=FMemory;
    CPanel.UpdateMemoryLabel;
    If Precision>2 then
      CPanel.Precision:=Precision
    else
      CPanel.Precision:=2;
    CPanel.BeepOnError:=BeepOnError;
    if FValue <> 0 then begin
      CPanel.DisplayValue:=FValue;
      CPanel.Status:=csFirst;
      CPanel.OperatorChar:='=';
    end;
    Result := (DlgForm.ShowModal = mrOk);
    FLeft := DlgForm.Left;
    FTop := DlgForm.Top;
    //update private fields FHeight and FWidth of ancestor
    SetHeight(DlgForm.Height);
    SetWidth(DlgForm.Width);
    if Result then begin
      FMemory:=CPanel.Memory;
      if CPanel.DisplayValue <> FValue then begin
        FValue:=CPanel.DisplayValue;
        Change;
      end;
    end;
  finally
    DlgForm.Free;
    DlgForm:=nil;
  end;
end;


{ ---------------------------------------------------------------------
  TCalendarDialog
  ---------------------------------------------------------------------}

{ TCalendarDialog }

constructor TCalendarDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DisplaySettings := DefaultDisplaySettings;
  Date := trunc(Now);
  OKCaption := rsMbOK;
  CancelCaption := rsMbCancel;
end;

procedure TCalendarDialog.GetNewDate(Sender:TObject);//or onClick
begin
  Date:=FCalendar.DateTime;
end;

procedure TCalendarDialog.CalendarDblClick(Sender: TObject);
var
  CalendarForm: TForm;
  P: TPoint;
  htRes: TCalendarPart;
begin
  P := FCalendar.ScreenToClient(Mouse.CursorPos);
  //if FCalendar.HitTest(P) in [cpNoWhere, cpDate] then
  htRes := FCalendar.HitTest(P);
  if {(htRes = cpNoWhere) or }((htRes = cpDate) and (FCalendar.GetCalendarView = cvMonth)) then
  begin
    GetNewDate(Sender);
    CalendarForm:=TForm(TComponent(Sender).Owner);
    // close the calendar dialog
    CalendarForm.ModalResult:=mrOk;
  end;
end;

function TCalendarDialog.DefaultTitle: string;
begin
  Result := rsPickDate;
end;

procedure TCalendarDialog.OnDialogClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  //if Assigned(OnClose) then OnClose(Self);
  DoClose;
end;

procedure TCalendarDialog.OnDialogCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  //if Assigned(OnCanClose) then OnCanClose(Sender, CanClose);
  if DlgForm.ModalResult = mrOK then
    UserChoice := mrOk
  else
    UserChoice := mrCancel;
  DoCanClose(CanClose);
end;

procedure TCalendarDialog.OnDialogShow(Sender: TObject);
var
  frm: TForm;
  NBtnSize, NSpace, NCalSize: integer;
const
  cSpace = 16; // space between 2 buttons
begin
  // Calendar form size for Cocoa cannot be fixed on WS level
  // see issue 35336
  if WidgetSet.LCLPlatform = lpCocoa then begin
    frm := TForm(Sender);

    okButton.Constraints.MinWidth := 0;
    okButton.Constraints.MaxWidth := 0;
    cancelButton.Constraints.MinWidth := 0;
    cancelButton.Constraints.MaxWidth := 0;
    okButton.AutoSize := true;
    cancelButton.AutoSize := true;
    FCalendar.AutoSize := true;

    NBtnSize := Max(okButton.Width, cancelButton.Width);
    NCalSize := FCalendar.Width;
    NSpace := NBtnSize * 2 + cSpace;
    NSpace := Max(NCalSize, NSpace);

    frm.AutoSize := false;
    okButton.AutoSize := false;
    cancelButton.AutoSize := false;

    frm.ClientWidth := NSpace;
    panel.Anchors := [];
    FCalendar.Align := alNone;
    FCalendar.Left := (NSpace-NCalSize) div 2;
    okButton.Width := NBtnSize;
    cancelButton.Width := NBtnSize;
    cancelButton.Left := 0;
    okButton.Left := frm.ClientWidth - okButton.Width;
  end;

  DoShow;
end;

procedure TCalendarDialog.OnCalendarDayChanged(Sender: TObject);
begin
  GetNewDate(Self);
  if Assigned(FDayChanged) then FDayChanged(Self);
end;

procedure TCalendarDialog.OnCalendarMonthChanged(Sender: TObject);
begin
  GetNewDate(Self);
  if Assigned(FMonthChanged) then FMonthChanged(Self);
end;

procedure TCalendarDialog.OnCalendarYearChanged(Sender: TObject);
begin
  GetNewDate(Self);
  if Assigned(FYearChanged) then FYearChanged(Self);
end;

procedure TCalendarDialog.OnCalendarChange(Sender: TObject);
begin
  //Date already updated in OnCalendarXXXChanged
  if Assigned(FOnChange) then FOnChange(Self);
end;


class procedure TCalendarDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCalendarDialog;
end;

function TCalendarDialog.Execute:boolean;
const
  dw=8;
  bbs=2;
begin
  DlgForm:=TForm.CreateNew(Application, 0);
  try
    ResetShowCloseFlags;
    DlgForm.DisableAlign;
    DlgForm.Caption:=Title;
    if (csDesigning in ComponentState) then
      DlgForm.Position:=poScreenCenter
    else
      DlgForm.Position:=DialogPosition;
    if (DlgForm.Position=poDesigned) then begin
      DlgForm.Left:=FLeft;
      DlgForm.Top:=FTop;
    end else begin
      FLeft:=DlgForm.Left;
      FTop:=DlgForm.Top;
    end;
    DlgForm.BorderStyle:=bsDialog;
    DlgForm.AutoScroll:=false;
    DlgForm.AutoSize:=true;
    DlgForm.OnShow := @OnDialogShow;
    DlgForm.OnClose:=@OnDialogClose;
    DlgForm.OnCloseQuery:=@OnDialogCloseQuery;

    FCalendar:=TCalendar.Create(DlgForm);
    with FCalendar do begin
      Parent:=DlgForm;
      Align:=alTop;
      DateTime:=Self.Date;
      TabStop:=True;
      DisplaySettings:=Self.DisplaySettings;
      OnDayChanged:=@Self.OnCalendarDayChanged;
      OnMonthChanged:=@Self.OnCalendarMonthChanged;
      OnYearChanged:=@Self.OnCalendarYearChanged;
      OnChange:=@Self.OnCalendarChange;
      OnDblClick:=@CalendarDblClick;
    end;

    panel:=TPanel.Create(DlgForm);
    with panel do begin
      Parent:=DlgForm;
      Caption:='';
      Height:=32;
      AnchorToCompanion(akTop, 0, FCalendar);
      BevelOuter:=bvLowered;
    end;

    okButton:=TButton.Create(DlgForm);
    with okButton do begin
      Parent:=panel;
      Caption:=OKCaption;
      Constraints.MinWidth:=75;
      Constraints.MaxWidth:=FCalendar.Width div 2 - bbs;
      Width:=DlgForm.Canvas.TextWidth(OKCaption)+2*dw;
      ModalResult:=mrOK;
      OnClick:=@GetNewDate;
      //Align:=alRight;
      Anchors := [akTop,akRight];
      BorderSpacing.Right:=bbs;
      AnchorSide[akRight].Side:=asrRight;
      AnchorSide[akRight].Control:=panel;
      AnchorVerticalCenterTo(panel);
      Default:=True;
    end;

    cancelButton:=TButton.Create(DlgForm);
    with cancelButton do begin
      Parent:=panel;
      Caption:=CancelCaption;
      Constraints.MinWidth:=75;
      Constraints.MaxWidth:=FCalendar.Width div 2;
      Width:=DlgForm.Canvas.TextWidth(CancelCaption)+2*dw;;
      ModalResult:=mrCancel;
      //Align:=alLeft;
      BorderSpacing.Left:=bbs;
      Anchors:=[akLeft,akTop];
      AnchorSide[akLeft].Side:=asrLeft;
      AnchorSide[akLeft].Control:=panel;
      AnchorVerticalCenterTo(panel);
      Cancel:=True;
    end;
    DlgForm.ClientWidth := FCalendar.Width;
    DlgForm.ClientHeight := panel.Top+panel.Height;

    DlgForm.EnableAlign;
    Result:=DlgForm.ShowModal=mrOK;
    FLeft:=DlgForm.Left;
    FTop:=DlgForm.Top;
    //update private fields FHeight and FWidth of ancestor
    SetHeight(DlgForm.Height);
    SetWidth(DlgForm.Width);
  finally
    DlgForm.Free;
    DlgForm := nil;
  end;
end;


end.
