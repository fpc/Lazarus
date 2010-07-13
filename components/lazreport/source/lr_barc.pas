{*******************************************}
{                                           }
{            FastReport v2.3                }
{         Barcode Add-in object             }
{                                           }
{  Copyright (c) 1998-99 by Tzyganenko A.   }
{                                           }

//  Barcode Component
//  Version 1.3
//  Copyright 1998-99 Andreas Schmidt and friends

//  Freeware

//  for use with Delphi 2/3/4


//  this component is for private use only!
//  i am not responsible for wrong barcodes
//  Code128C not implemented

//  bug-reports, enhancements:
//  mailto:shmia@bizerba.de or
//  a_j_schmidt@rocketmail.com

{  Fr_BarC:     Guilbaud Olivier            }
{               golivier@worldnet.fr        }
{  Ported to FR2.3: Alexander Tzyganenko    }
{                                           }
{*******************************************}

unit LR_BarC;

{$I lr_vers.inc}

interface

uses
  Classes, SysUtils, LResources,
  Graphics, Controls, Forms, Dialogs,Buttons,
  StdCtrls, Menus, Barcode,
  
  LCLType,LR_Class, ExtCtrls, ButtonPanel;


{.$DEFINE BC_1_25} //For Barcode version 1.25 actually in debug
type
  {$IFDEF BC_1_25}
  TBarCode=Class(TAsBarCode);
  {$ENDIF}
  
  { TfrBarCodeObject }

  TfrBarCodeObject = class(TComponent)  // fake component
  public
    constructor Create(aOwner : TComponent); override;
  end;

  TfrBarCode = packed record
    cCheckSum : Boolean;
    cShowText : Boolean;
    cCadr     : Boolean;
    cBarType  : TBarcodeType;
    cModul    : Integer;
    cRatio    : Double;
    cAngle    : Double;
  end;

  { TfrBarCodeView }

  TfrBarCodeView = class(TfrView)
  private
    BarC: TBarCode;
    
    function GetBarType: TBarcodeType;
    function GetCheckSum: Boolean;
    function GetShowText: Boolean;
    function GetZoom: Double;
    procedure SetBarType(const AValue: TBarcodeType);
    procedure SetCheckSum(const AValue: Boolean);
    procedure SetShowText(const AValue: Boolean);
    procedure SetZoom(const AValue: Double);
  public
    Param: TfrBarCode;
    
    constructor Create; override;
    destructor Destroy; override;
    procedure Assign(From: TfrView); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure Draw(aCanvas: TCanvas); override;
    procedure Print(Stream: TStream); override;
    procedure DefinePopupMenu(Popup: TPopupMenu); override;
    function  GenerateBitmap: TBitmap;
    
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property CheckSum : Boolean read GetCheckSum write SetCheckSum;
    property BarType : TBarcodeType read GetBarType write SetBarType;
    property ShowText : Boolean read GetShowText write SetShowText;
    property Zoom : Double read GetZoom write SetZoom;
    property Memo;
    property Frames;
    property FrameColor;
    property FrameStyle;
    property FrameWidth;
  end;

  { TfrBarCodeForm }

  TfrBarCodeForm = class(TfrObjEditorForm)
    ButtonPanel1: TButtonPanel;
    edZoom: TEdit;
    labZoom: TLabel;
    M1: TEdit;
    Label1: TLabel;
    cbType: TComboBox;
    Label2: TLabel;
    Image1: TImage;
    Panel1: TPanel;
    DBBtn: TSpeedButton;
    VarBtn: TSpeedButton;
    GroupBox1: TGroupBox;
    ckCheckSum: TCheckBox;
    ckViewText: TCheckBox;
    GroupBox2: TGroupBox;
    RB1: TRadioButton;
    RB2: TRadioButton;
    RB3: TRadioButton;
    RB4: TRadioButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure VarBtnClick(Sender: TObject);
    procedure DBBtnClick(Sender: TObject);
    procedure bOkClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure edZoomKeyPress(Sender: TObject; var Key: char);
  public
    procedure ShowEditor(t: TfrView); override;
  end;


implementation

{$R *.lfm}

uses LR_Var, LR_Flds, LR_Const, LR_Utils;


var
  frBarCodeForm: TfrBarCodeForm;

const
   cbDefaultText ='12345678';
   bcNames: array[bcCode_2_5_interleaved..{$IFNDEF BC_1_25}bcCodeEAN13{$ELSE}bcCodeEAN128C{$ENDIF}, 0..1] of string =
     (('2_5_interleaved', 'N'),
      ('2_5_industrial', 'N'),
      ('2_5_matrix', 'N'),
      ('Code39', 'A'),
      ('Code39 Extended', 'A'),
      ('Code128A', 'A'),
      ('Code128B', 'A'),
      ('Code128C', 'N'),
      ('Code93', 'A'),
      ('Code93 Extended', 'A'),
      ('MSI', 'N'),
      ('PostNet', 'N'),
      ('Codebar', 'A'),
      ('EAN8', 'N'),
      ('EAN13', 'N')
      {$IFDEF BC_1_25}
      ,
      ('UPC A','N'),
      ('UPC E0','N'),
      ('UPC E1','N'),
      ('UPC SUPP 2','N'),
      ('UPC SUPP 5','N'),
      ('EAN128A','A'),
      ('EAN128B','A'),
      ('EAN128C','N')
      {$ENDIF}
      );


{$HINTS OFF}
function isNumeric(St: String): Boolean;
var
  R: Double;
  E: Integer;
begin
  Val(St, R, E);
  Result := (E = 0);
end;
{$HINTS ON}

function TfrBarCodeView.GetBarType: TBarcodeType;
begin
  Result:=Param.cBarType;
end;

function TfrBarCodeView.GetCheckSum: Boolean;
begin
  Result:=Param.cCheckSum;
end;

function TfrBarCodeView.GetShowText: Boolean;
begin
  Result:=Param.cShowText;
end;

function TfrBarCodeView.GetZoom: Double;
begin
  Result:=Param.cRatio;
end;

procedure TfrBarCodeView.SetBarType(const AValue: TBarcodeType);
begin
  Param.cBarType:=aValue;
  invalidate;
end;

procedure TfrBarCodeView.SetCheckSum(const AValue: Boolean);
begin
  Param.cCheckSum:=aValue;
  invalidate;
end;

procedure TfrBarCodeView.SetShowText(const AValue: Boolean);
begin
  Param.cShowText:=aValue;
  invalidate;
end;

procedure TfrBarCodeView.SetZoom(const AValue: Double);
begin
  Param.cRatio:=aValue;
  invalidate;
end;

constructor TfrBarCodeView.Create;
begin
  inherited Create;

  BarC := TBarCode.Create(nil);
  Param.cCheckSum := True;
  Param.cShowText := True;
  Param.cCadr     := False;
  Param.cBarType  := bcCode39;
  Param.cModul    := 2;
  Param.cRatio    := 1;
  Param.cAngle    := 0;
  Memo.Add(cbDefaultText);
  Typ := gtAddIn;
  BaseName := 'Bar';
end;

destructor TfrBarCodeView.Destroy;
begin
  BarC.Free;
  inherited Destroy;
end;

procedure TfrBarCodeView.Assign(From:TfrView);
begin
  inherited Assign(From);
  Param := (From as TfrBarCodeView).Param;
end;

procedure TfrBarCodeView.LoadFromStream(Stream:TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(Param, SizeOf(Param));
end;

procedure TfrBarCodeView.SaveToStream(Stream:TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(Param, SizeOf(Param));
end;

function TfrBarCodeView.GenerateBitmap: TBitmap;
var
  Txt: String;
  hg: Integer;
  h, oldh: HFont;
  newdx,newdy : Integer;
begin
  Memo1.Assign(Memo);

  if (Memo1.Count>0) and (Memo1[0][1]<>'[') then
    Txt := Memo1.Strings[0]
  else
    Txt := cbDefaultText;

  BarC.Typ := Param.cBarType;
  BarC.Angle := Param.cAngle;
  BarC.Ratio := 2; //Param.cRatio;
  BarC.Modul := 1; //Param.cModul;
  BarC.Checksum := Param.cCheckSum;
  {$IFDEF BC_1_25}
  BarC.ShowTextPosition:=stpBottomCenter;
  BarC.ShowText := bcoNone;

  if FillColor=clNone then
    BarC.Color:=clWhite
  else
    BarC.Color:=FillColor;

  {$ELSE}
  BarC.ShowText:=False;
  {$ENDIF}

  if bcNames[Param.cBarType, 1] = 'A' then
    BarC.Text := Txt
  else
    begin
      if IsNumeric(Txt) then
        BarC.Text := Txt
      else
        BarC.Text := cbDefaultText;
    end;


  newdx:=dx;
  newdy:=dy;
  if (Param.cAngle = 90) or (Param.cAngle = 270) then
  begin
    dy := BarC.Width;
    newdy:=Round(dy*Param.cRatio);
  end
  else
  begin
    dx := BarC.Width;
    newdx:=Round(dx*Param.cRatio);
  end;

  if Trim(BarC.Text)='0' then Exit;

  if (Param.cAngle = 90) or (Param.cAngle = 270) then
    if Param.cShowText then
      hg := dx - 14
    else
      hg := dx
  else
    if Param.cShowText then
      hg := dy - 14
    else
      hg := dy;

  BarC.Left:=0;
  BarC.Top :=0;
  BarC.Height:=hg;

  if (BarC.Typ=bcCodePostNet) and (Param.cAngle=0) then
  begin
    BarC.Top:=hg;
    BarC.Height:=-hg;
  end;

  if Param.cAngle = 180 then
    BarC.Top:=dy-hg
  else
  if Param.cAngle = 270 then
    BarC.Left:=dx-hg;

  Result:=TBitMap.Create;

  Result.Width:=dx;
  Result.Height:=dy;
  Result.Canvas.Brush.Style:=bsSolid;
  Result.Canvas.Brush.Color:=clWhite;
  Result.Canvas.FillRect(Rect(0,0,Dx,Dy));

  BarC.DrawBarcode(Result.Canvas);

  if Param.cShowText then
  begin
    with Result.Canvas do
    begin
      Font.Color := clBlack;
      Font.Name := 'Courier New';
      Font.Height := -12;
      Font.Style := [];

      if Param.cAngle = 0 then
      begin
        Brush.Color:=clWhite;
        Brush.Style:=bsSolid;
        FillRect(Rect(0,dy-12,dx,dy));
        TextOut((dx - TextWidth(Txt)) div 2, dy - 12, Txt);
      end
      else
        if Param.cAngle = 90 then
        begin
          Brush.Color:=clWhite;
          Brush.Style:=bsSolid;
          FillRect(Rect(dx - 12,0,dx,dy));

          TextOut(dx - 12, dy - (dy - TextWidth(Txt)) div 2, Txt);
        end
        else
          if Param.cAngle = 180 then
          begin
            Brush.Color:=clWhite;
            Brush.Style:=bsSolid;
            FillRect(Rect(0,0,dx,12));

            TextOut((dx - TextWidth(Txt)) div 2, 1, Txt);
          end
          else
          begin
            Brush.Color:=clWhite;
            Brush.Style:=bsSolid;
            FillRect(Rect(0,0,12,dy));

            //here text it's write in barcode :o( TextOut(12, (dy - TextWidth(Txt)) div 2, Txt);
          end;
    end;
  end;

  dx:=newdx;
  dy:=newdy;
end;

procedure TfrBarCodeView.Draw(aCanvas:TCanvas);
var
  Bmp : TBitMap;
begin
  BeginDraw(aCanvas);

  Bmp := GenerateBitmap;
  try
    CalcGaps;
    ShowBackground;
    aCanvas.StretchDraw(DRect,Bmp);
  finally
    Bmp.Free;
  end;
  
  ShowFrame;
  RestoreCoord;
end;

procedure TfrBarCodeView.Print(Stream: TStream);
begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1, Self);
  frInterpretator.DoScript(Script);
  if not Visible then Exit;

  if Memo1.Count > 0 then
    if (Length(Memo1[0]) > 0) and (Memo1[0][1] = '[') then
      Memo1[0] := frParser.Calc(Memo1[0]);
  Stream.Write(Typ, 1);
  frWriteString(Stream, ClassName);
  SaveToStream(Stream);
end;

procedure TfrBarCodeView.DefinePopupMenu(Popup: TPopupMenu);
begin
  // no specific items in popup menu
end;

procedure TfrBarCodeView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  
  RestoreProperty('BarType',XML.GetValue(Path+'BarCode/BarType',''));
  RestoreProperty('ShowText',XML.GetValue(Path+'BarCode/ShowText',''));
  RestoreProperty('CheckSum',XML.GetValue(Path+'BarCode/CheckSum',''));
  RestoreProperty('Zoom',XML.GetValue(Path+'BarCode/Zoom','1'));
end;

procedure TfrBarCodeView.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  
  XML.SetValue(Path+'BarCode/BarType', GetSaveProperty('BarType'));
  XML.SetValue(Path+'BarCode/ShowText', GetSaveProperty('ShowText'));
  XML.SetValue(Path+'BarCode/CheckSum', GetSaveProperty('CheckSum'));
  XML.SetValue(Path+'BarCode/Zoom', GetSaveProperty('Zoom'));
end;


//--------------------------------------------------------------------------
procedure TfrBarCodeForm.FormCreate(Sender: TObject);
var
  i: TBarcodeType;
begin
  CbType.Items.Clear;
  for i := bcCode_2_5_interleaved to {$IFNDEF BC_1_25}bcCodeEAN13{$ELSE}bcCodeEAN128C{$ENDIF} do
    cbType.Items.Add(bcNames[i, 0]);
  cbType.ItemIndex := 0;

  Caption := sBarCodeFormTitle;
  Label1.Caption := sBarCodeFormCode;
  Label2.Caption := sBarCodeFormType;
  GroupBox1.Caption := sBarCodeFormOpts;
  ckCheckSum.Caption := sBarCodeFormChksum;
  ckViewText.Caption := sBarCodeFormReadable;
  DBBtn.Hint := sBarCodeFormDbFld;
  VarBtn.Hint := sBarCodeFormVar;
  GroupBox2.Caption := sBarCodeFormRotate;
  labZoom.Caption:=sBarCodeZoom;
end;

procedure TfrBarCodeForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if ModalResult = mrOk then
    bOkClick(nil);
end;

procedure TfrBarCodeForm.FormActivate(Sender: TObject);
begin
  M1.SetFocus;
end;

procedure TfrBarCodeForm.edZoomKeyPress(Sender: TObject; var Key: char);
begin
  If (Key>#31) and not (Key in ['0'..'9']) then
    Key:=#0;
end;

procedure TfrBarCodeForm.ShowEditor(t:TfrView);
begin
  if t.Memo.Count > 0 then
    M1.Text := t.Memo.Strings[0];
  with t as TfrBarCodeView do
  begin
    cbType.ItemIndex   := ord(Param.cBarType);
    ckCheckSum.checked := Param.cCheckSum;
    ckViewText.Checked := Param.cShowText;
    if Param.cAngle = 0 then
      RB1.Checked := True
    else if Param.cAngle = 90 then
      RB2.Checked := True
    else if Param.cAngle = 180 then
      RB3.Checked := True
    else
      RB4.Checked := True;
    edZoom.Text:=SysUtils.Format('%.0f',[Param.cRatio]);

    if ShowModal = mrOk then
    begin
      Memo.Clear;
      Memo.Add(M1.Text);
      Param.cCheckSum  := ckCheckSum.Checked;
      Param.cShowText  := ckViewText.Checked;
      Param.cBarType   := TBarcodeType(cbType.ItemIndex);
      Param.cRatio     := StrToFloatDef(edZoom.Text,1);
      if Param.cRatio<1 then
        Param.cRatio:=1;
        
      if RB1.Checked then
        Param.cAngle := 0
      else if RB2.Checked then
        Param.cAngle := 90
      else if RB3.Checked then
        Param.cAngle := 180
      else
        Param.cAngle := 270;
    end;
  end;
end;

procedure TfrBarCodeForm.VarBtnClick(Sender: TObject);
begin
  frVarForm := TfrVarForm.Create(nil);
  with frVarForm do
  if ShowModal = mrOk then
    if SelectedItem <> '' then
      M1.Text := '[' + SelectedItem + ']';
  frVarForm.Free;
  M1.SetFocus;
end;

procedure TfrBarCodeForm.DBBtnClick(Sender: TObject);
begin
  frFieldsForm := TfrFieldsForm.Create(nil);
  with frFieldsForm do
  if ShowModal = mrOk then
    if DBField <> '' then
      M1.Text := '[' + DBField + ']';
  frFieldsForm.Free;
  M1.SetFocus;
end;

procedure TfrBarCodeForm.bOkClick(Sender: TObject);
var
  bc: TBarCode;
  Bmp: TBitmap;
begin
  bc := TBarCode.Create(nil);
  bc.Text := M1.Text;
  bc.CheckSum  := ckCheckSum.Checked;
  bc.Typ := TBarcodeType(cbType.ItemIndex);
  Bmp := TBitmap.Create;
  Bmp.Width := 16; Bmp.Height := 16;
  try
   Bmp.Canvas.Brush.Style:=bsSolid;
   Bmp.Canvas.Brush.Color:=clWhite;
   Bmp.Canvas.FillRect(Rect(0,0,Bmp.Width,Bmp.Height));

    bc.DrawBarcode(Bmp.Canvas);
  except
    MessageDlg(sBarcodeError,mtError,[mbOk],0);
    ModalResult := 0;
  end;
  Bmp.Free;
end;


{ TfrBarCodeObject }

constructor TfrBarCodeObject.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  
  if not assigned(frBarCodeForm) {and not (csDesigning in ComponentState)} then
  begin
    frBarCodeForm := TfrBarCodeForm.Create(nil);
    frRegisterObject(TfrBarCodeView, frBarCodeForm.Image1.Picture.Bitmap,
                           sInsBarcode, frBarCodeForm);
  end;
end;

initialization

  frBarCodeForm:=nil;
finalization
  if Assigned(frBarCodeForm) then
    frBarCodeForm.Free;

end.
