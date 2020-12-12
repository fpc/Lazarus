{
 /***************************************************************************
                               calcform.pas
                               ------------
                              Calculator form


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit CalcForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  LazUtilities,
  // LCL
  Forms, Controls, Graphics, StdCtrls, ExtCtrls, Buttons, Menus, Clipbrd,
  WSExtDlgs, LCLStrConsts;

const
  CalcDefPrecision = 15;

type
  TCalculatorCalcKeyEvent = procedure (var key: char) of object;
  TCalculatorDispChangeEvent = procedure of object;
  TCalculatorState = (csFirst, csValid, csError);

  TCalculatorLayout = (clNormal, clSimple);

{ TCalculatorForm }

type
  TCalculatorPanel = class(TPanel)
  private
    FText: string;
    FStatus: TCalculatorState;
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

    procedure UpdateMemoryLabel;
    property DisplayValue: Double read GetDisplay write SetDisplay;
    property Memory: Double read FMemory write FMemory;
    property Precision: byte read FPrecision write FPrecision;
    property BeepOnError: boolean read FBeepOnError write FBeepOnError;
    property Status: TCalculatorState read FStatus write FStatus;
    property OperatorChar: char read FOperator write FOperator;
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

  TCalculatorForm = class(TForm)
  private
    FMainPanel: TPanel;
    FCalcPanel: TCalculatorPanel;
    FDisplayPanel: TPanel;
    FDisplayLabel: TLabel;
    FOnCalcKey: TCalculatorCalcKeyEvent;
    FOnDisplayChange: TCalculatorDispChangeEvent;
    FMenu: TPopupMenu;
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
    procedure InitForm(ALayout: TCalculatorLayout); virtual;
  public
    constructor Create(AOwner: TComponent; ALayout: TCalculatorLayout); reintroduce;
    property Value: Double read GetValue write SetValue;

    property MainPanel: TPanel read FMainPanel;
    property CalcPanel: TCalculatorPanel read FCalcPanel;
    property DisplayPanel: TPanel read FDisplayPanel;
    property DisplayLabel: TLabel read FDisplayLabel;

    property OnCalcKey: TCalculatorCalcKeyEvent read FOnCalcKey write FOnCalcKey;
    property OnDisplayChange: TCalculatorDispChangeEvent read FOnDisplayChange write FOnDisplayChange;
  end;

function CreateCalculatorForm(AOwner: TComponent;
  ALayout: TCalculatorLayout;
  AHelpContext: THelpContext): TCalculatorForm;

var
  cColorBtnDigits: TColor = clblack;
  cColorBtnMemory: TColor = clnavy;
  cColorBtnClear: TColor = clred;
  cColorBtnOk: TColor = clgreen;
  cColorBtnCancel: TColor = clmaroon;
  cColorBtnOthers: TColor = clblack;
  cColorDisplayText: TColor = clblack;
  cColorDisplayBack: TColor = clwhite;

  cCalculatorFontName: string = 'MS Sans Serif';
  cCalculatorFontSize: integer = 8;
  cCalculatorFontStyle: TFontStyles = [fsBold];


implementation

type
  TCalcBtnKind =
   (cbNone, cbNum0, cbNum1, cbNum2, cbNum3, cbNum4, cbNum5, cbNum6,
    cbNum7, cbNum8, cbNum9, cbSgn, cbDcm, cbDiv, cbMul, cbSub,
    cbAdd, cbSqrt, cbSquare, cbPcnt, cbRev, cbEql, cbBck, cbClr, cbMP,
    cbMS, cbMR, cbMC, cbOk, cbCancel);

{$IFDEF Windows}
// Windows: use BitBtn to enable font color on button
{$define CalcBitButtons}
{$ENDIF}

type

  { TCalcButton }

  TCalcButton = class(
    {$IFDEF CalcBitButtons} TCustomBitBtn {$ELSE} TCustomSpeedButton {$ENDIF}
    )
  private
    FKind: TCalcBtnKind;
  public
    constructor CreateKind(AOwner: TComponent; AKind: TCalcBtnKind);
    property Kind: TCalcBtnKind read FKind;
    property ParentFont;
  end;


const
  CalcFormWidth: array[TCalculatorLayout] of integer = (278, 170);
  CalcFormHeight: array[TCalculatorLayout] of integer = (160, 150);
  CalcBtnEqualWidthNormal = 74;
  CalcBtnClearWidthNormal = 50;
  CalcBtnEqualWidthSimple = 54;
  CalcResultKeys = [#13, '=', '%'];

const
  CalcPanelSizes: array[TCalculatorLayout, 1..2] of Integer =
    ((129,140), (124,98));
  CalcBtnSizes: array[TCalculatorLayout, 1..2] of Integer =
    ((36,22), (25,21));
  CalcBtnCaptions: array[cbSgn..cbCancel] of String =
   ('±', ',', '/', '*', '-', '+', '√ ', 'x²', '%', '1/x', '=', '«', 'C',
    'MP','MS','MR','MC', 'ok', 'x');
  CalcBtnPos: array[TCalculatorLayout, TCalcBtnKind] of TPoint =
  ((
    //normal layout
    (X: -1; Y: -1), (X: 47; Y: 104), (X: 47; Y: 80), (X: 85; Y: 80),
    (X: 123; Y: 80), (X: 47; Y: 56), (X: 85; Y: 56), (X: 123; Y: 56),
    (X: 47; Y: 32), (X: 85; Y: 32), (X: 123; Y: 32), (X: 85; Y: 104),
    (X: 123; Y: 104), (X: 161; Y: 32), (X: 161; Y: 56), (X: 161; Y: 80),
    (X: 161; Y: 104),
    (X: 199; Y: 32), //sqrt
    (X: 199+38; Y: 32), //sqr
    (X: 199; Y: 56), //%
    (X: 199; Y: 80), //1/x
    (X: 199; Y: 104), //=
    (X: 170; Y: 6), //back
    (X: 223; Y: 6), //clear
    (X: 5; Y: 104), (X: 5; Y: 80), (X: 5; Y: 56), (X: 5; Y: 32), //mem
    (X: 47; Y: 6), //ok
    (X: 100; Y: 6) //cancel
    ),
    (
    //simple layout
    (X: -1; Y: -1), (X: 6; Y: 75), (X: 6; Y: 52), (X: 34; Y: 52),
    (X: 62; Y: 52), (X: 6; Y: 29), (X: 34; Y: 29), (X: 62; Y: 29),
    (X: 6; Y: 6), (X: 34; Y: 6), (X: 62; Y: 6), (X: 62; Y: 75),
    (X: 34; Y: 75), (X: 90; Y: 6), (X: 90; Y: 29), (X: 90; Y: 52),
    (X: 90; Y: 75), (X: -1; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1),
    (X: 62; Y: 98), (X: 34; Y: 98), (X: 6; Y: 98), (X: -1; Y: -1),
    (X: -1; Y: -1), (X: -1; Y: -1), (X: -1; Y: -1),
    (X: 140; Y: 6), (X: 140; Y: 29)
    ));

{ funcs }

procedure SetDefaultFont(AFont: TFont; Layout: TCalculatorLayout);
begin
  //AFont.Color:=cCalculatorFontColor; //all controls now have their custom colors
  AFont.Name:=cCalculatorFontName;
  AFont.Size:=cCalculatorFontSize;
  AFont.Style:=cCalculatorFontStyle;
end;

function CreateCalculatorForm(AOwner: TComponent; ALayout: TCalculatorLayout; AHelpContext: THelpContext): TCalculatorForm;
begin
  Result:=TCalculatorForm.Create(AOwner, ALayout);
  with Result do
    try
      HelpContext:=AHelpContext;
      if Screen.PixelsPerInch <> 96 then
      begin { scale to screen res }
        SetDefaultFont(Font, ALayout);
        Left:=(Screen.Width - Width) div 2;
        Top:=(Screen.Height - Height) div 2;
      end;
    except
      Free;
      raise;
    end;
end;

function CreateCalcBtn(AParent: TWinControl; AKind: TCalcBtnKind;
  AOnClick: TNotifyEvent; ALayout: TCalculatorLayout): TCalcButton;
begin
  Result:=TCalcButton.CreateKind(AParent, AKind);
  with Result do
    try
      if Kind in [cbNum0..cbNum9] then
        Caption:=IntToStr(Tag)
      else if Kind = cbDcm then
        Caption:=DefaultFormatSettings.DecimalSeparator
      else if Kind in [cbSgn..cbCancel] then
        Caption:=CalcBtnCaptions[Kind];

      Left:=CalcBtnPos[ALayout, Kind].X;
      Top:=CalcBtnPos[ALayout, Kind].Y;
      Width:=CalcBtnSizes[ALayout,1];
      Height:=CalcBtnSizes[ALayout,2];
      OnClick:=AOnClick;
      ParentFont:=True;
      Parent:=AParent;

      case Result.Kind of
        cbMC, cbMR, cbMS, cbMP: Result.Font.Color:= cColorBtnMemory;
        cbOk: Result.Font.Color:= cColorBtnOk;
        cbCancel: Result.Font.Color:= cColorBtnCancel;
        cbClr: Result.Font.Color:= cColorBtnClear;
        cbNum0..cbNum9: Result.Font.Color:= cColorBtnDigits;
        else Result.Font.Color:= cColorBtnOthers;
      end;
    except
      Free;
      raise;
    end;
end;


{ TCalculatorPanel }

constructor TCalculatorPanel.CreateLayout(AOwner: TComponent; ALayout: TCalculatorLayout);
var
  I: TCalcBtnKind;
  Bitmap: TCustomBitmap;
begin
  inherited Create(AOwner);
  ParentColor:=False;
  Color:=clBtnFace;
  Height:=CalcPanelSizes[ALayout,1];
  Width:=CalcPanelSizes[ALayout,2];
  SetDefaultFont(Font, ALayout);
  ParentFont:=False;
  BevelOuter:=bvNone;
  BevelInner:=bvNone;
  ParentColor:=True;
  for I:=cbNum0 to cbCancel do
  begin
    if CalcBtnPos[ALayout, I].X > 0 then
      with CreateCalcBtn(Self, I, @BtnClick, ALayout) do
      begin
        if ALayout=clNormal then
        begin
          if (Kind in [cbBck, cbClr, cbOk, cbCancel]) then
            Width:=CalcBtnClearWidthNormal;
          if (Kind in [cbPcnt, cbRev, cbEql]) then
            Width:=CalcBtnEqualWidthNormal;
        end
        else
        begin
          if Kind in [cbEql] then Width:=CalcBtnEqualWidthSimple;
        end;
      end;
  end;
  if ALayout=clNormal then
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
      Font.Color:=cColorDisplayText;
      Font.Style:=[];
    end;
  end;
  FText:='0';
  FMemory:=0.0;
  FPrecision:=CalcDefPrecision;
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
  RegisterCalculatorPanel;
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
      'S':
        if FStatus in [csValid, csFirst] then
          begin
          FStatus:=csFirst;
          SetDisplay(Sqr(GetDisplay));
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
        if (Key in CalcResultKeys) and Assigned(FOnResult) then
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
  ButtonChars = '0123456789_./*-+QS%R='#8'C';
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
    cbSqrt: CalcKey('Q');
    cbSquare: CalcKey('S');
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
  Clipboard.AsText:=FText;
end;

procedure TCalculatorPanel.Paste;
var
  S: string;
begin
  if Clipboard.HasFormat(CF_TEXT) then
  begin
    S:=Clipboard.AsText;
    S:=Trim(StringReplace(S, DefaultFormatSettings.CurrencyString, '', []));
    SetDisplay(StrToFloatDef(S, 0.0));
  end;
end;

{ TCalculatorForm }

constructor TCalculatorForm.Create(AOwner: TComponent; ALayout: TCalculatorLayout);
begin
  BeginFormUpdate;
  inherited CreateNew(AOwner, 0);
  InitForm(ALayout);
  EndFormUpdate;
end;

procedure TCalculatorForm.InitForm(ALayout: TCalculatorLayout);
var
  mi: TMenuItem;
begin
  BorderStyle:=bsDialog;
  Caption:=rsCalculator;
  ClientHeight:=CalcFormHeight[ALayout];
  ClientWidth:=CalcFormWidth[ALayout];
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
    Height:=23;
    Align:=alTop;
    BorderSpacing.Around:=7;
    Parent:=FMainPanel;
    BevelOuter:=bvLowered;
    Color:=cColorDisplayBack;
    Font:=Self.Font;
  end;
  FDisplayLabel:=TLabel.Create(Self);
  with FDisplayLabel do
  begin
    AutoSize:=False;
    Alignment:=taRightJustify;
    Align:=alClient;
    BorderSpacing.Around:=2;
    Parent:=FDisplayPanel;
    Caption:='0';
    Font.Color:=cColorDisplayText;
  end;
  { CalcPanel }
  FCalcPanel:=TCalculatorPanel.CreateLayout(Self, ALayout);
  with FCalcPanel do
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
  { Menu }
  FMenu:=TPopupMenu.Create(Self);
  DisplayPanel.PopupMenu:=FMenu;
  mi:=TMenuItem.Create(Self);
  mi.Caption:=rsDoCopy;
  mi.OnClick:=@CopyItemClick;
  FMenu.Items.Add(mi);
  mi:=TMenuItem.Create(Self);
  mi.Caption:=rsDoPaste;
  mi.OnClick:=@PasteItemClick;
  FMenu.Items.Add(mi);
end;


procedure TCalculatorForm.FormKeyPress(Sender: TObject; var Key: char);
begin
  FCalcPanel.CalcKeyPress(Sender, Key);
end;

procedure TCalculatorForm.CopyItemClick(Sender: TObject);
begin
  FCalcPanel.Copy;
end;

function TCalculatorForm.GetValue: Double;
begin
  Result:=FCalcPanel.DisplayValue
end;

procedure TCalculatorForm.PasteItemClick(Sender: TObject);
begin
  FCalcPanel.Paste;
end;

procedure TCalculatorForm.SetValue(const AValue: Double);
begin
  FCalcPanel.DisplayValue:=AValue;
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
  if Assigned(FOnCalcKey) then
    FOnCalcKey(Key);
end;

procedure TCalculatorForm.DisplayChange(Sender: TObject);
begin
  if Assigned(FOnDisplayChange) then
    FOnDisplayChange();
end;

{ TCalcButton }

constructor TCalcButton.CreateKind(AOwner: TComponent; AKind: TCalcBtnKind);
begin
  inherited Create(AOwner);
  {$IFDEF CalcBitButtons}
  TabStop:=false;
  {$ENDIF}
  FKind:=AKind;
  if FKind in [cbNum0..cbClr] then
    Tag:=Ord(Kind) - 1
  else
    Tag:=-1;
end;

end.

