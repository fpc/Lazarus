{
 /***************************************************************************
                               extdlgs.pas
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
unit ExtDlgs;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, SysUtils, LCLProc, LResources, LCLType, LCLStrConsts, FileUtil,
  Controls, Dialogs, GraphType, Graphics, ExtCtrls, StdCtrls, Forms,
  Calendar, Buttons, Masks;

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
  public
    function Execute: boolean; override;
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
  public
    constructor Create(TheOwner: TComponent); override;
  end;

{ ---------------------------------------------------------------------
  Calculator Dialog
  ---------------------------------------------------------------------}

const
  DefCalcPrecision = 15;

type
  TCalcState = (csFirst, csValid, csError);
  TCalculatorLayout = (clNormal, clSimple);
  TCalculatorForm = class;

{ TCalculatorDialog }

  TCalculatorDialog = class(TCommonDialog)
  private
    FLayout: TCalculatorLayout;
    FValue: Double;
    FMemory: Double;
    FTitle: String;
    FPrecision: Byte;
    FBeepOnError: Boolean;
    FHelpContext: THelpContext;
    FCalc: TCalculatorForm;
    FOnChange: TNotifyEvent;
    FOnCalcKey: TKeyPressEvent;
    FOnDisplayChange: TNotifyEvent;
    function GetDisplay: Double;
    function GetTitle: string;
    procedure SetTitle(const AValue: string);
    function TitleStored: Boolean;
  protected
    class procedure WSRegisterClass; override;
    procedure Change; virtual;
    procedure CalcKey(var Key: char); virtual;
    procedure DisplayChange; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; override;
    property CalcDisplay: Double read GetDisplay;
    property Memory: Double read FMemory;
  published
    property BeepOnError: Boolean read FBeepOnError write FBeepOnError default True;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property CalculatorLayout : TCalculatorLayout Read FLayout Write Flayout;
    property Precision: Byte read FPrecision write FPrecision default DefCalcPrecision;
    property Title: string read GetTitle write SetTitle stored TitleStored;
    property Value: Double read FValue write FValue;
    property OnCalcKey: TKeyPressEvent read FOnCalcKey write FOnCalcKey;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDisplayChange: TNotifyEvent read FOnDisplayChange write FOnDisplayChange;
  end;

{ TCalculatorForm }

  TCalculatorForm = class(TForm)
  private
    FMainPanel: TPanel;
    FCalcPanel: TPanel;
    FDisplayPanel: TPanel;
    FDisplayLabel: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure CopyItemClick(Sender: TObject);
    function GetValue: Double;
    procedure PasteItemClick(Sender: TObject);
    procedure SetValue(const AValue: Double);
  protected
    class procedure WSRegisterClass; override;
    procedure OkClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure CalcKey(Sender: TObject; var Key: char);
    procedure DisplayChange(Sender: TObject);
    procedure InitForm(ALayout : TCalculatorLayout); virtual;
    property MainPanel: TPanel read FMainPanel;
    property CalcPanel: TPanel read FCalcPanel;
    property DisplayPanel: TPanel read FDisplayPanel;
    property DisplayLabel: TLabel read FDisplayLabel;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateLayout(AOwner: TComponent;ALayout : TCalculatorLayout);
    property Value : Double read GetValue write SetValue;
  end;

function CreateCalculatorForm(AOwner: TComponent; ALayout : TCalculatorLayout; AHelpContext: THelpContext): TCalculatorForm;

{ ---------------------------------------------------------------------
  Date Dialog
  ---------------------------------------------------------------------}


Type
  { TCalendarDialog }

  TCalendarDialog = class(TCommonDialog)
  private
    FDate: TDateTime;
    FDayChanged: TNotifyEvent;
    FDialogPosition: TPosition;
    FDisplaySettings: TDisplaySettings;
    FHelpContext: THelpContext;
    FMonthChanged: TNotifyEvent;
    FYearChanged: TNotifyEvent;
    FDialogTitle:TCaption;
    FOKCaption:TCaption;
    FCancelCaption:TCaption;
    FCalendar:TCalendar;
    function IsTitleStored: Boolean;
  protected
    class procedure WSRegisterClass; override;
    procedure GetNewDate(Sender:TObject);//or onClick
    procedure CalendarDblClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: Boolean; override;
  published
    property Date: TDateTime read FDate write FDate;
    property OnDayChanged: TNotifyEvent read FDayChanged write FDayChanged;
    property DisplaySettings: TDisplaySettings read FDisplaySettings write FDisplaySettings default DefaultDisplaySettings;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property OnMonthChanged: TNotifyEvent read FMonthChanged write FMonthChanged;
    property OnYearChanged: TNotifyEvent read FYearChanged write FYearChanged;
    property DialogPosition: TPosition read FDialogPosition write FDialogPosition default poMainFormCenter;
    property DialogTitle:TCaption read FDialogTitle write FDialogTitle stored IsTitleStored;
    property OKCaption:TCaption read FOKCaption write FOKCaption;
    property CancelCaption:TCaption read FCancelCaption write FCancelCaption;
  end;

procedure Register;

implementation
uses WSExtDlgs;

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
  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, CX, CY);
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

function TPreviewFileDialog.Execute: boolean;
begin
  CreatePreviewControl;
  Result:=inherited Execute;
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

constructor TSavePictureDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Title:=rsfdFileSaveAs;
end;

type
  TCalcBtnKind =
   (cbNone, cbNum0, cbNum1, cbNum2, cbNum3, cbNum4, cbNum5, cbNum6,
    cbNum7, cbNum8, cbNum9, cbSgn, cbDcm, cbDiv, cbMul, cbSub,
    cbAdd, cbSqr, cbPcnt, cbRev, cbEql, cbBck, cbClr, cbMP,
    cbMS, cbMR, cbMC, cbOk, cbCancel);

const
  BtnPos: array[TCalculatorLayout, TCalcBtnKind] of TPoint =
  (((X: -1; Y: -1), (X: 47; Y: 104), (X: 47; Y: 80), (X: 85; Y: 80),
    (X: 123; Y: 80), (X: 47; Y: 56), (X: 85; Y: 56), (X: 123; Y: 56),
    (X: 47; Y: 32), (X: 85; Y: 32), (X: 123; Y: 32), (X: 85; Y: 104),
    (X: 123; Y: 104), (X: 161; Y: 32), (X: 161; Y: 56), (X: 161; Y: 80),
    (X: 161; Y: 104), (X: 199; Y: 32), (X: 199; Y: 56), (X: 199; Y: 80),
    (X: 199; Y: 104), (X: 145; Y: 6), (X: 191; Y: 6), (X: 5; Y: 104),
    (X: 5; Y: 80), (X: 5; Y: 56), (X: 5; Y: 32),
    (X: 47; Y: 6), (X: 85; Y: 6)),
   ((X: -1; Y: -1), (X: 6; Y: 75), (X: 6; Y: 52), (X: 29; Y: 52),
    (X: 52; Y: 52), (X: 6; Y: 29), (X: 29; Y: 29), (X: 52; Y: 29),
    (X: 6; Y: 6), (X: 29; Y: 6), (X: 52; Y: 6), (X: 52; Y: 75),
    (X: 29; Y: 75), (X: 75; Y: 6), (X: 75; Y: 29), (X: 75; Y: 52),
    (X: 75; Y: 75), (X: -1; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1),
    (X: 52; Y: 98), (X: 29; Y: 98), (X: 6; Y: 98), (X: -1; Y: -1),
    (X: -1; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1),
    (X: -1; Y: -1), (X: -1; Y: -1)));
  ResultKeys = [#13, '=', '%'];

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

function CreateCalculatorForm(AOwner: TComponent; ALayout : TCalculatorLayout; AHelpContext: THelpContext): TCalculatorForm;
begin
  Result:=TCalculatorForm.Create(AOwner);
  with Result do
    try
      HelpContext:=AHelpContext;
      if Screen.PixelsPerInch <> 96 then
      begin { scale to screen res }
        SetDefaultFont(Font, ALayout);
        Left:=(Screen.Width div 2) - (Width div 2);
        Top:=(Screen.Height div 2) - (Height div 2);
      end;
    except
      Free;
      raise;
    end;
end;

{ ---------------------------------------------------------------------
  Calculator Dialog
  ---------------------------------------------------------------------}


{ TCalcButton }

type
  TCalcButton = class(TCustomSpeedButton)
  private
    FKind: TCalcBtnKind;
  public
    constructor CreateKind(AOwner: TComponent; AKind: TCalcBtnKind);
    property Kind: TCalcBtnKind read FKind;
    property ParentFont;
  end;

constructor TCalcButton.CreateKind(AOwner: TComponent; AKind: TCalcBtnKind);
begin
  inherited Create(AOwner);
  FKind:=AKind;
  if FKind in [cbNum0..cbClr] then
    Tag:=Ord(Kind) - 1
  else
    Tag:=-1;
end;

function CreateCalcBtn(AParent: TWinControl; AKind: TCalcBtnKind;
  AOnClick: TNotifyEvent; ALayout: TCalculatorLayout): TCalcButton;
const
  BtnSizes: array[TCalculatorLayout,1..2] of Integer =
    ((36,22),(21,21));
  BtnCaptions: array[cbSgn..cbMC] of String =
   ('±', ',', '/', '*', '-', '+', 'sqrt', '%', '1/x', '=', '<-', 'C',
    'MP','MS','MR','MC');
begin
  Result:=TCalcButton.CreateKind(AParent, AKind);
  with Result do
    try
      if Kind in [cbNum0..cbNum9] then
        Caption:=IntToStr(Tag)
      else if Kind = cbDcm then
        Caption:=DefaultFormatSettings.DecimalSeparator
      else if Kind in [cbSgn..cbMC] then
        Caption:=BtnCaptions[Kind];
      Left:=BtnPos[ALayout, Kind].X;
      Top:=BtnPos[ALayout, Kind].Y;
      Width:=BtnSizes[ALayout,1];
      Height:=BtnSizes[ALayout,2];
      OnClick:=AOnClick;
      ParentFont:=True;
      Parent:=AParent;
    except
      Free;
      raise;
    end;
end;

{ TCalculatorPanel }

type
  TCalculatorPanel = class(TPanel)
  private
    FText: string;
    FStatus: TCalcState;
    FOperator: Char;
    FOperand: Double;
    FMemory: Double;
    FPrecision: Byte;
    FBeepOnError: Boolean;
    FMemoryPanel: TPanel;
    FMemoryLabel: TLabel;
    FOnError: TNotifyEvent;
    FOnOk: TNotifyEvent;
    FOnCancel: TNotifyEvent;
    FOnResult: TNotifyEvent;
    FOnTextChange: TNotifyEvent;
    FOnCalcKey: TKeyPressEvent;
    FOnDisplayChange: TNotifyEvent;
    FControl: TControl;
    procedure SetCalcText(const Value: string);
    procedure CheckFirst;
    procedure CalcKey(Key: char);
    procedure Clear;
    procedure Error;
    procedure SetDisplay(R: Double);
    function GetDisplay: Double;
    procedure UpdateMemoryLabel;
    function FindButton(Key: Char): TCustomSpeedButton;
    procedure BtnClick(Sender: TObject);
  protected
    procedure ErrorBeep;
    procedure TextChange; virtual;
    class procedure WSRegisterClass; override;
  public
    constructor CreateLayout(AOwner: TComponent; ALayout: TCalculatorLayout);
    procedure CalcKeyPress(Sender: TObject; var Key: char);
    procedure Copy;
    procedure Paste;
    function WorkingPrecision : Integer;
    property DisplayValue: Double read GetDisplay write SetDisplay;
    property Text: string read FText;
    property OnOkClick: TNotifyEvent read FOnOk write FOnOk;
    property OnCancelClick: TNotifyEvent read FOnCancel write FOnCancel;
    property OnResultClick: TNotifyEvent read FOnResult write FOnResult;
    property OnError: TNotifyEvent read FOnError write FOnError;
    property OnTextChange: TNotifyEvent read FOnTextChange write FOnTextChange;
    property OnCalcKey: TKeyPressEvent read FOnCalcKey write FOnCalcKey;
    property OnDisplayChange: TNotifyEvent read FOnDisplayChange write FOnDisplayChange;
    property Color default clBtnFace;
  end;

constructor TCalculatorPanel.CreateLayout(AOwner: TComponent; ALayout: TCalculatorLayout);
const
  PanelSizes: array[TCalculatorLayout,1..2] of Integer =
    ((129,140),(124,98));
  BtnGlyphs: array[TCalculatorLayout,cbSgn..cbCancel] of String =
   (('btncalcpmin','','','btncalcmul','btncalcmin','btncalcplus', '',
     '','','','','','','','','', 'btncalcok', 'btncalccancel'),
    ('btncalcpmin','','','btncalcmul','btncalcmin','btncalcplus', '',
     '','','','','','','','','', 'btncalcok', 'btncalccancel')
   );
var
  I: TCalcBtnKind;
  Bitmap: TCustomBitmap;
begin
  inherited Create(AOwner);
  ParentColor:=False;
  Color:=clBtnFace;
  Height:=PanelSizes[ALayout,1];
  Width:=PanelSizes[ALayout,2];
  SetDefaultFont(Font, ALayout);
  ParentFont:=False;
  BevelOuter:=bvNone;
  BevelInner:=bvNone;
  ParentColor:=True;
  for I:=cbNum0 to cbCancel do
  begin
    if BtnPos[ALayout, I].X > 0 then
      with CreateCalcBtn(Self, I, @BtnClick, ALayout) do
      begin
        if ALayout = clNormal then
        begin
          if (Kind in [cbBck, cbClr]) then
            Width:=44;
          if (Kind in [cbSgn..cbCancel]) then
            if (BtnGlyphs[ALayout,Kind]<>'') then
            begin
              Caption:='';
              Bitmap := CreateBitmapFromLazarusResource(BtnGlyphs[ALayout,Kind]);
              Glyph.Assign(Bitmap);
              Bitmap.Free;
            end;
        end
        else
        begin
          if Kind in [cbEql] then Width:=44;
        end;
      end;
  end;
  if ALayout = clNormal then
  begin
    { Memory panel }
    FMemoryPanel:=TPanel.Create(Self);
    with FMemoryPanel do
    begin
      SetBounds(6, 7, 34, 20);
      BevelInner:=bvLowered;
      BevelOuter:=bvNone;
      ParentColor:=True;
      Parent:=Self;
    end;
    FMemoryLabel:=TLabel.Create(Self);
    with FMemoryLabel do
    begin
      SetBounds(3, 3, 26, 14);
      Alignment:=taCenter;
      AutoSize:=False;
      Parent:=FMemoryPanel;
      Font.Style:=[];
    end;
  end;
  FText:='0';
  FMemory:=0.0;
  FPrecision:=DefCalcPrecision;
  FBeepOnError:=True;
end;

procedure TCalculatorPanel.SetCalcText(const Value: string);
begin
  if FText <> Value then
    begin
    FText:=Value;
    TextChange;
    end;
end;

procedure TCalculatorPanel.TextChange;
begin
  if Assigned(FControl) then
    TLabel(FControl).Caption:=FText;
  if Assigned(FOnTextChange) then
    FOnTextChange(Self);
end;

class procedure TCalculatorPanel.WSRegisterClass;
begin
  inherited WSRegisterClass;
  if RegisterCalculatorPanel then
  begin
    {$i lcl_calc_images.lrs}
  end;
end;

procedure TCalculatorPanel.ErrorBeep;

begin
 if FBeepOnError then
   // MessageBeep(0);
end;

procedure TCalculatorPanel.Error;
begin
  FStatus:=csError;
  SetCalcText(rsError);
  ErrorBeep;
  if Assigned(FOnError) then
    FOnError(Self);
end;

procedure TCalculatorPanel.SetDisplay(R: Double);
var
  S: string;
begin
  S:=FloatToStrF(R, ffGeneral, WorkingPrecision, 0);
  if FText <> S then
    begin
    SetCalcText(S);
    if Assigned(FOnDisplayChange) then
      FOnDisplayChange(Self);
    end;
end;

function TCalculatorPanel.GetDisplay: Double;
begin
  if (FStatus=csError) then
    Result:=0.0
  else
    Result:=StrToDouble(Trim(FText));
end;

procedure TCalculatorPanel.CheckFirst;
begin
  if (FStatus=csFirst) then
    begin
    FStatus:=csValid;
    SetCalcText('0');
    end;
end;

procedure TCalculatorPanel.UpdateMemoryLabel;
begin
  if (FMemoryLabel<>nil) then
    if (FMemory<>0.0) then
      FMemoryLabel.Caption:='M'
    else
      FMemoryLabel.Caption:='';
end;

function TCalculatorPanel.WorkingPrecision : Integer;

begin
  Result:=2;
  If FPrecision>2 then
    Result:=FPrecision;
end;


procedure TCalculatorPanel.CalcKey(Key: char);
var
  R: Double;
begin
{$IFDEF GTK1}
  Key:=UpCase(Key);
{$ENDIF GTK1}
  if (FStatus = csError) and (Key <> 'C') then
    Key:=#0;
  if Assigned(FOnCalcKey) then
    FOnCalcKey(Self, Key);
  if Key in [DefaultFormatSettings.DecimalSeparator, '.', ','] then
    begin
    CheckFirst;
    if Pos(DefaultFormatSettings.DecimalSeparator, FText) = 0 then
      SetCalcText(FText + DefaultFormatSettings.DecimalSeparator);
    end
  else
    case Key of
      'R':
        if (FStatus in [csValid, csFirst]) then
          begin
          FStatus:=csFirst;
          if GetDisplay = 0 then
            Error
          else
            SetDisplay(1.0 / GetDisplay);
          end;
      'Q':
        if FStatus in [csValid, csFirst] then
          begin
          FStatus:=csFirst;
          if GetDisplay < 0 then
            Error
          else
            SetDisplay(Sqrt(GetDisplay));
          end;
      '0'..'9':
        begin
        CheckFirst;
        if (FText='0') then
          SetCalcText('');
        if (Pos('E', FText)=0) then
          begin
          if (Length(FText) < WorkingPrecision + Ord(Boolean(Pos('-', FText)))) then
            SetCalcText(FText + Key)
          else
            ErrorBeep;
          end;
        end;
      #8:
        begin
        CheckFirst;
        if ((Length(FText)=1) or ((Length(FText)=2) and (FText[1]='-'))) then
          SetCalcText('0')
        else
          SetCalcText(System.Copy(FText,1,Length(FText)-1));
        end;
      '_':
        SetDisplay(-GetDisplay);
      '+', '-', '*', '/', '=', '%', #13:
        begin
        if (FStatus=csValid) then
          begin
          FStatus:=csFirst;
          R:=GetDisplay;
          if (Key='%') then
            case FOperator of
              '+', '-': R:=(FOperand*R)/100.0;
              '*', '/': R:=R/100.0;
            end;
          case FOperator of
            '+': SetDisplay(FOperand+R);
            '-': SetDisplay(FOperand-R);
            '*': SetDisplay(FOperand*R);
            '/': if R = 0 then
                   Error
                 else
                   SetDisplay(FOperand / R);
          end;
        end;
        FOperator:=Key;
        FOperand:=GetDisplay;
        if (Key in ResultKeys) and Assigned(FOnResult) then
          FOnResult(Self);
        end;
      #27, 'C':
        Clear;
      ^C:
        Copy;
      ^V:
        Paste;
    end;
end;

procedure TCalculatorPanel.Clear;
begin
  FStatus:=csFirst;
  SetDisplay(0.0);
  FOperator:='=';
end;

procedure TCalculatorPanel.CalcKeyPress(Sender: TObject; var Key: char);

var
  Btn: TCustomSpeedButton;

begin
  Btn:=FindButton(Key);
  if Assigned(Btn) then
    Btn.Click
  else
    CalcKey(Key);
end;

function TCalculatorPanel.FindButton(Key: Char): TCustomSpeedButton;
const
  ButtonChars = '0123456789_./*-+Q%R='#8'C';
var
  I: Integer;
  BtnTag: Longint;
begin
  if Key in [DefaultFormatSettings.DecimalSeparator, '.', ','] then
    Key:='.'
  else if Key = #13 then
    Key:='='
  else if Key = #27 then
    Key:='C';
  Result:=nil;
  BtnTag:=Pos(UpCase(Key), ButtonChars) - 1;
  if (BtnTag>=0) then
    begin
    I:=0;
    While (Result=Nil) and (I<ControlCount) do
      begin
      if Controls[I] is TCustomSpeedButton then
        If BtnTag=TCustomSpeedButton(Controls[I]).Tag then
          Result:=TCustomSpeedButton(Controls[I]);
      Inc(I);
      end;
    end;
end;

procedure TCalculatorPanel.BtnClick(Sender: TObject);
begin
  case TCalcButton(Sender).Kind of
    cbNum0..cbNum9: CalcKey(Char(TComponent(Sender).Tag + Ord('0')));
    cbSgn: CalcKey('_');
    cbDcm: CalcKey(DefaultFormatSettings.DecimalSeparator);
    cbDiv: CalcKey('/');
    cbMul: CalcKey('*');
    cbSub: CalcKey('-');
    cbAdd: CalcKey('+');
    cbSqr: CalcKey('Q');
    cbPcnt: CalcKey('%');
    cbRev: CalcKey('R');
    cbEql: CalcKey('=');
    cbBck: CalcKey(#8);
    cbClr: CalcKey('C');
    cbMP:
      if (FStatus in [csValid, csFirst]) then
        begin
        FStatus:=csFirst;
        FMemory:=FMemory + GetDisplay;
        UpdateMemoryLabel;
        end;
    cbMS:
      if FStatus in [csValid, csFirst] then
        begin
        FStatus:=csFirst;
        FMemory:=GetDisplay;
        UpdateMemoryLabel;
        end;
    cbMR:
      if (FStatus in [csValid, csFirst]) then
        begin
        FStatus:=csFirst;
        CheckFirst;
        SetDisplay(FMemory);
        end;
    cbMC:
        begin
        FMemory:=0.0;
        UpdateMemoryLabel;
        end;
    cbOk:
        begin
        if FStatus <> csError then
          begin
          DisplayValue:=DisplayValue; { to raise exception on error }
          if Assigned(FOnOk) then
            FOnOk(Self);
          end
        else
          ErrorBeep;
        end;
    cbCancel:
        if Assigned(FOnCancel) then
          FOnCancel(Self);
  end;
end;

procedure TCalculatorPanel.Copy;
begin
  // Clipboard.AsText:=FText;
end;

procedure TCalculatorPanel.Paste;
begin
{  if Clipboard.HasFormat(CF_TEXT) then
    try
      SetDisplay(StrToFloat(Trim(ReplaceStr(Clipboard.AsText,
        CurrencyString, ''))));
    except
      SetCalcText('0');
    end;
}
end;

{ TCalculatorDialog }

constructor TCalculatorDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTitle:=rsCalculator;
  FPrecision:=DefCalcPrecision;
  FBeepOnError:=True;
end;

destructor TCalculatorDialog.Destroy;
begin
  FOnChange:=nil;
  FOnDisplayChange:=nil;
  inherited Destroy;
end;

function TCalculatorDialog.GetTitle: string;
begin
  Result:=FTitle;
end;

procedure TCalculatorDialog.SetTitle(const AValue: string);
begin
  FTitle:=AValue;
end;

function TCalculatorDialog.TitleStored: Boolean;
begin
  Result:=Title <> rsCalculator;
end;

class procedure TCalculatorDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCalculatorDialog;
end;

function TCalculatorDialog.GetDisplay: Double;
begin
  if Assigned(FCalc) then
    Result:=TCalculatorPanel(FCalc.FCalcPanel).GetDisplay
  else Result:=FValue;
end;

procedure TCalculatorDialog.CalcKey(var Key: char);
begin
  if Assigned(FOnCalcKey) then FOnCalcKey(Self, Key);
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
begin
  FCalc:=CreateCalculatorForm(Self, FLayout, HelpContext);
  with FCalc do
  try
    Caption:=Self.Title;
    TCalculatorPanel(FCalcPanel).FMemory:=Self.FMemory;
    TCalculatorPanel(FCalcPanel).UpdateMemoryLabel;
    If Self.Precision>2 then
      TCalculatorPanel(FCalcPanel).FPrecision:=Self.Precision
    else
      TCalculatorPanel(FCalcPanel).FPrecision:=2;
    TCalculatorPanel(FCalcPanel).FBeepOnError:=Self.BeepOnError;
    if Self.FValue <> 0 then begin
      TCalculatorPanel(FCalcPanel).DisplayValue:=Self.FValue;
      TCalculatorPanel(FCalcPanel).FStatus:=csFirst;
      TCalculatorPanel(FCalcPanel).FOperator:='=';
    end;
    Result:=(ShowModal = mrOk);
    if Result then begin
      Self.FMemory:=TCalculatorPanel(FCalcPanel).FMemory;
      if (TCalculatorPanel(FCalcPanel).DisplayValue <> Self.FValue) then begin
        Self.FValue:=TCalculatorPanel(FCalcPanel).DisplayValue;
        Change;
      end;
    end;
  finally
    Free;
    FCalc:=nil;
  end;
end;

{ TCalculatorForm }

constructor TCalculatorForm.Create(AOwner: TComponent);

begin
  BeginFormUpdate;
  inherited CreateNew(AOwner, 0);
  InitForm(clNormal);
  EndFormUpdate;
end;

constructor TCalculatorForm.CreateLayout(AOwner: TComponent;ALayout : TCalculatorLayout);

begin
  BeginFormUpdate;
  inherited CreateNew(AOwner, 0);
  InitForm(ALayout);
  EndFormUpdate;
end;


procedure TCalculatorForm.InitForm(ALayout : TCalculatorLayout);
begin
  BorderStyle:=bsDialog;
  Caption:=rsCalculator;
  ClientHeight:=159;
  ClientWidth:=242;
  SetDefaultFont(Font, ALayout);
  KeyPreview:=True;
  PixelsPerInch:=96;
  Position:=poScreenCenter;
  OnKeyPress:=@FormKeyPress;
  { MainPanel }
  FMainPanel:=TPanel.Create(Self);
  with FMainPanel do
  begin
    Align:=alClient;
    Parent:=Self;
    BevelOuter:=bvLowered;
    ParentColor:=True;
  end;
  { DisplayPanel }
  FDisplayPanel:=TPanel.Create(Self);
  with FDisplayPanel do
  begin
    SetBounds(6, 6, 230, 23);
    Parent:=FMainPanel;
    BevelOuter:=bvLowered;
    Color:=clWhite;
    Font:=Self.Font;
  end;
  FDisplayLabel:=TLabel.Create(Self);
  with FDisplayLabel do
  begin
    AutoSize:=False;
    Alignment:=taRightJustify;
    SetBounds(5, 2, 217, 15);
    Parent:=FDisplayPanel;
    Caption:='0';
    Font.Color:=clBlack;
  end;
  { CalcPanel }
  FCalcPanel:=TCalculatorPanel.CreateLayout(Self, ALayout);
  with TCalculatorPanel(FCalcPanel) do
  begin
    Align:=alBottom;
    Top:=17;
    Anchors:=[akLeft,akRight,AkBottom];
    Parent:=FMainPanel;
    OnOkClick:=@Self.OkClick;
    OnCancelClick:=@Self.CancelClick;
    OnCalcKey:=@Self.CalcKey;
    OnDisplayChange:=@Self.DisplayChange;
    FControl:=FDisplayLabel;
  end;
end;


procedure TCalculatorForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  TCalculatorPanel(FCalcPanel).CalcKeyPress(Sender, Key);
end;

procedure TCalculatorForm.CopyItemClick(Sender: TObject);
begin
  TCalculatorPanel(FCalcPanel).Copy;
end;

function TCalculatorForm.GetValue: Double;
begin
  Result:=TCalculatorPanel(FCalcPanel).DisplayValue
end;

procedure TCalculatorForm.PasteItemClick(Sender: TObject);
begin
  TCalculatorPanel(FCalcPanel).Paste;
end;

procedure TCalculatorForm.SetValue(const AValue: Double);
begin
  TCalculatorPanel(FCalcPanel).DisplayValue:=AValue;
end;

class procedure TCalculatorForm.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCalculatorForm;
end;

procedure TCalculatorForm.OkClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TCalculatorForm.CancelClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

procedure TCalculatorForm.CalcKey(Sender: TObject; var Key: char);
begin
  if (Owner <> nil) and (Owner is TCalculatorDialog) then
    TCalculatorDialog(Owner).CalcKey(Key);
end;

procedure TCalculatorForm.DisplayChange(Sender: TObject);
begin
  if (Owner <> nil) and (Owner is TCalculatorDialog) then
    TCalculatorDialog(Owner).DisplayChange;
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
  DialogPosition := poMainFormCenter;
  DialogTitle := rsPickDate;
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
begin
  P := FCalendar.ScreenToClient(Mouse.CursorPos);
  if FCalendar.HitTest(P) in [cpNoWhere, cpDate] then
  begin
    GetNewDate(Sender);
    CalendarForm:=TForm(TComponent(Sender).Owner);
    // close the calendar dialog
    CalendarForm.ModalResult:=mrOk;
  end;
end;

function TCalendarDialog.IsTitleStored: Boolean;
begin
  Result:=DialogTitle<>rsPickDate;//controllare
end;

class procedure TCalendarDialog.WSRegisterClass;
begin
  inherited WSRegisterClass;
  RegisterCalendarDialog;
end;

function TCalendarDialog.Execute:boolean;
const dw=8;
var DF:TForm;
    okButton,cancelButton:TButton;
    panel:TPanel;
begin
  DF:=TForm(TForm.NewInstance);
  DF.DisableAlign;
  DF.Create(Self.Owner); // Self.Owner, so that poOwnerFormCenter works
  DF.Caption:=DialogTitle;
  DF.Position:=DialogPosition;
  DF.BorderStyle:=bsDialog;
  DF.AutoScroll:=false;
  DF.AutoSize:=true;

  FCalendar:=TCalendar.Create(DF);
  with FCalendar do begin
    Parent:=DF;
    Align:=alTop;
    DateTime:=Self.Date;
    DisplaySettings:=Self.DisplaySettings;
    OnDayChanged:=Self.OnDayChanged;
    OnMonthChanged:=Self.OnMonthChanged;
    OnYearChanged:=Self.OnYearChanged;
    OnDblClick:=@CalendarDblClick;
  end;

  panel:=TPanel.Create(DF);
  with panel do begin
    Parent:=DF;
    Caption:='';
    Height:=32;
    AnchorToCompanion(akTop, 0, FCalendar);
    BevelOuter:=bvLowered;
  end;

  okButton:=TButton.Create(DF);
  with okButton do begin
    Parent:=panel;
    Caption:=OKCaption;
    Constraints.MinWidth:=75;
    Constraints.MaxWidth:=FCalendar.Width div 2;
    Width:=DF.Canvas.TextWidth(OKCaption)+2*dw;
    ModalResult:=mrOK;
    OnClick:=@GetNewDate;
    Align:=alRight;
    Default:=True;
  end;

  cancelButton:=TButton.Create(DF);
  with cancelButton do begin
    Parent:=panel;
    Caption:=CancelCaption;
    Constraints.MinWidth:=75;
    Constraints.MaxWidth:=FCalendar.Width div 2;
    Width:=DF.Canvas.TextWidth(CancelCaption)+2*dw;;
    ModalResult:=mrCancel;
    Align:=alLeft;
    Cancel:=True;
  end;
  DF.ClientWidth := FCalendar.Width;
  DF.ClientHeight := panel.Top+panel.Height;

  DF.EnableAlign;
  Result:=DF.ShowModal=mrOK;
  FreeAndNil(DF);
end;

end.
