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
  Classes, SysUtils,
  LCLPlatformDef, InterfaceBase, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, Menus, Barcode, ExtCtrls, ButtonPanel,
  LCLType, LR_Class;


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

  { TfrCustomBarCodeView }

  TfrCustomBarCodeView = class(TfrView)
  private
    BarC: TBarCode;
    FText: string;
    
    function GetAngle: Double;
    function GetBarType: TBarcodeType;
    function GetCheckSum: Boolean;
    function GetShowText: Boolean;
    function GetZoom: Double;
    procedure SetAngle(AValue: Double);
    procedure SetBarType(const AValue: TBarcodeType);
    procedure SetCheckSum(const AValue: Boolean);
    procedure SetShowText(const AValue: Boolean);
    procedure SetZoom(const AValue: Double);
    function CreateBarcode: TBitmap;
    function CreateLabelFont(aCanvas: TCanvas): TFont;
    procedure DrawLabel(aCanvas: TCanvas; R: TRect);
  public
    Param: TfrBarCode;
    
    constructor Create(AOwnerPage:TfrPage);override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GenerateBitmap: TBitmap; virtual;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure Draw(aCanvas: TCanvas); override;
    procedure Print(Stream: TStream); override;
    procedure DefinePopupMenu({%H-}Popup: TPopupMenu); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;

    property CheckSum : Boolean read GetCheckSum write SetCheckSum;
    property BarType : TBarcodeType read GetBarType write SetBarType;
    property ShowText : Boolean read GetShowText write SetShowText;
    property Zoom : Double read GetZoom write SetZoom;
    property Angle: Double read GetAngle write SetAngle;
  end;

  TfrBarcodeView = class(TfrCustomBarcodeView)
  published
    property CheckSum;
    property BarType;
    property ShowText;
    property Zoom;
    property Angle;
    property Memo;
    property Frames;
    property FrameColor;
    property FrameStyle;
    property FrameWidth;
    property Restrictions;
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
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
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

   defaultFontSize = 10;

{$PUSH}
{$HINTS OFF}
{$NOTES OFF}
function isNumeric(St: String): Boolean;
var
  {%H-}R: Double;
  E: Integer;
begin
  Val(St, R, E);
  Result := (E = 0);
end;
{$POP}

function TfrCustomBarCodeView.GetBarType: TBarcodeType;
begin
  Result:=Param.cBarType;
end;

function TfrCustomBarCodeView.GetAngle: Double;
begin
  Result := Param.cAngle;
end;

function TfrCustomBarCodeView.GetCheckSum: Boolean;
begin
  Result:=Param.cCheckSum;
end;

function TfrCustomBarCodeView.GetShowText: Boolean;
begin
  Result:=Param.cShowText;
end;

function TfrCustomBarCodeView.GetZoom: Double;
begin
  Result:=Param.cRatio;
end;

procedure TfrCustomBarCodeView.SetAngle(AValue: Double);
begin
  if (Param.cAngle<>AValue) and
     ((AValue=0.0) or (AValue=90.0) or (AValue=180.0) or (AValue=270.0)) then
  begin
    BeforeChange;
    Param.cAngle:=AValue;
    AfterChange;
  end;
end;

procedure TfrCustomBarCodeView.SetBarType(const AValue: TBarcodeType);
begin
  if Param.cBarType<>AValue then
  begin
    BeforeChange;
    Param.cBarType:=aValue;
    AfterChange;
  end;
end;

procedure TfrCustomBarCodeView.SetCheckSum(const AValue: Boolean);
begin
  if Param.cCheckSum<>AValue then
  begin
    BeforeChange;
    Param.cCheckSum:=aValue;
    AfterChange;
  end;
end;

procedure TfrCustomBarCodeView.SetShowText(const AValue: Boolean);
begin
  if Param.cShowText<>AValue then
  begin
    BeforeChange;
    Param.cShowText:=aValue;
    AfterChange;
  end;
end;

procedure TfrCustomBarCodeView.SetZoom(const AValue: Double);
begin
  if (Param.cRatio<>AValue) and
     ((AValue>=1.0)and(Avalue<=4.0)) then
  begin
    BeforeChange;
    Param.cRatio:=aValue;
    AfterChange;
  end;
end;

function TfrCustomBarCodeView.CreateBarcode: TBitmap;
begin

  Result := nil;
  if Trim(Memo.Text) = '' then
    Exit;

  {Assign Barcode text}
  Memo1.Assign(Memo);

  if (Memo1.Text <> '') and (pos('[',Memo1.Strings[0])=0) and
    ((bcNames[Param.cBarType, 1] = 'A') or IsNumeric(Memo1.Strings[0]) or
      Barc.BarcodeTypeChecked(Param.cBarType) )  then
  begin
      BarC.Text := Memo1.Strings[0];
      BarC.Checksum := Param.cCheckSum;
  end
  else
  begin
    BarC.Text := cbDefaultText;
    BarC.Checksum := true;
  end;

  if Trim(BarC.Text)='0' then Exit;

  {Barcode Properties}
  BarC.Left:= 0;
  BarC.Top := 0;
  BarC.Typ := Param.cBarType;
  BarC.Angle := Param.cAngle;
  BarC.Ratio := 2; // param.cRatio <>2 renders some codes unreadable
  BarC.Modul := 1; // param.cModul
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


  {Barcode width is determined by type of barcode and text. Update
   object dimensions to suit barcode}

  if (Param.cAngle = 90) or (Param.cAngle = 270) then
    dy := BarC.Width
  else
    dx := BarC.Width;


  if (Param.cAngle = 90) or (Param.cAngle = 270) then
       BarC.Height := dx
  else
       BarC.Height := dy;

  if (BarC.Typ=bcCodePostNet) and (Param.cAngle=0) then
  begin
    BarC.Top:=BarC.Height;
    BarC.Height:=-BarC.Height;
  end;

  if  Param.cAngle = 90 then
    begin
      BarC.Top:= Round(Height);
      BarC.Left:=0;
    end
  else
  if  Param.cAngle = 180 then
    begin
      BarC.Top:= dy;
      BarC.Left:= dx;
    end
  else
  if  Param.cAngle = 270 then
    begin
      BarC.Top:= 0;
      BarC.Left:= dx;
    end;

  Result:=TBitMap.Create;

  Result.Width:=dx;
  Result.Height:=dy;
  Result.Canvas.Brush.Style:=bsSolid;
  Result.Canvas.Brush.Color:=clWhite;
  Result.Canvas.FillRect(Rect(0,0,dx,dy));

  try
    BarC.DrawBarcode(Result.Canvas);
    if BarC.Checksum then
      FText := BarC.CodeText
    else
      FText := BarC.Text;
  except on E: Exception do
    FText := E.Message
  end;


end;

function TfrCustomBarCodeView.CreateLabelFont(aCanvas: TCanvas) :TFont;
begin
  with aCanvas do
  begin
    Result := TFont.Create;
    Result.Assign(aCanvas.Font);
    Result.Color := clBlack;
    Result.Name := 'Arial';
    Result.Style := [];
    Result.Size := -defaultFontSize;

    if Param.cAngle = 90 then
      Result.Orientation := 900
    else
    if Param.cAngle = 180 then
      Result.Orientation := 1800
    else
    if Param.cAngle = 270 then
      Result.Orientation := 2700
    else
      Result.Orientation := 0;
  end;

end;


procedure TfrCustomBarCodeView.DrawLabel(aCanvas: TCanvas; R: TRect);
var fs: integer;
begin
  if Param.cShowText then
  begin
    with aCanvas do
    begin
      fs := Font.Height;

      if Param.cAngle = 0 then
      begin
        Brush.Color:=clWhite;
        Brush.Style:=bsSolid;
        FillRect(Rect(R.Left,R.Top + dy-fs ,R.Right, R.Bottom));
        TextOut(R.Left + (dx - TextWidth(FText)) div 2, R.Top + dy - fs, FText);
      end
      else
        if Param.cAngle = 90 then
        begin
          Brush.Color:=clWhite;
          Brush.Style:=bsSolid;
          FillRect(Rect(R.Left + dx - fs,R.Top,R.Right, R.Bottom));
          Font.Orientation := 900;

          TextOut(R.Right - fs,R.Bottom - (dy - TextWidth(FText)) div 2, FText)
        end
        else
          if Param.cAngle = 180 then
          begin
            Brush.Color:=clWhite;
            Brush.Style:=bsSolid;
            FillRect(Rect(R.Left,R.Top,R.Right,R.Top + fs));
            Font.Orientation := 1800;
            TextOut(R.left + (dx + TextWidth(FText)) div 2,  R.Top + fs, FText);
          end
          else
          begin
            Brush.Color:=clWhite;
            Brush.Style:=bsSolid;
            Font.Orientation := 2700;
            FillRect(Rect(R.Left,R.Top,R.Left + fs,R.Bottom));
            if (WidgetSet.LCLPlatform = lpGtk2) and IsPrinting then
            TextOut(R.Left + fs, R.Top + (dy -TextWidth(FText)) div 2, FText)
          end;
    end;
  end;

end;

constructor TfrCustomBarCodeView.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);

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

destructor TfrCustomBarCodeView.Destroy;
begin
  BarC.Free;
  inherited Destroy;
end;

procedure TfrCustomBarCodeView.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TfrCustomBarCodeView then
    Param := TfrCustomBarCodeView(Source).Param;
end;

function TfrCustomBarCodeView.GenerateBitmap: TBitmap;
var
  R: TRect;
  barcodeFont: TFont;
  oldFont: TFont;
begin
  Result := CreateBarcode;
  R := Rect(0,0, Result.Width,Result.Height);
  barcodeFont := CreateLabelFont(Result.Canvas);
  try
    oldFont := Result.Canvas.Font;
    Result.Canvas.Font := barcodeFont;
    DrawLabel(Result.Canvas,r)
  finally
      Result.Canvas.Font := oldFont;
      barcodeFont.Free
  end;
end;

procedure TfrCustomBarCodeView.LoadFromStream(Stream:TStream);
begin
  inherited LoadFromStream(Stream);
  Stream.Read(Param, SizeOf(Param));
end;

procedure TfrCustomBarCodeView.SaveToStream(Stream:TStream);
begin
  inherited SaveToStream(Stream);
  Stream.Write(Param, SizeOf(Param));
end;

procedure TfrCustomBarCodeView.Draw(aCanvas:TCanvas);
var
  Bmp : TBitMap;
  R: TRect;
  fh: integer;
  barcodeFont: TFont;
  oldFont: TFont;
begin
  BeginDraw(aCanvas);

  Bmp := CreateBarcode;
  if Bmp <> nil then
  try
    // dx/dy is calculated in CreateBarCode using as base
    // barcdode type, angle, zoom and data
    if (Param.cAngle = 90) or (Param.cAngle = 270) then
      dy := round(dy * param.cRatio)
    else
      dx := round(dx * Param.cRatio);
    CalcGaps;
    ShowBackground;
    if Param.cShowText then
    begin
      barcodeFont := CreateLabelFont(aCanvas);
      try
        oldFont := aCanvas.Font;
        aCanvas.Font := barcodeFont;
        if not IsPrinting then
        begin
          if (Param.cAngle = 90) or (Param.cAngle = 270) then
            ACanvas.Font.Height := -Round(ACanvas.Font.Size * ACanvas.Font.PixelsPerInch / 72 * ScaleX)
          else
            ACanvas.Font.Height := -Round(ACanvas.Font.Size * ACanvas.Font.PixelsPerInch / 72 * ScaleY);
          fh := Round(aCanvas.Font.Height);
        end
        else
          fh := aCanvas.Font.Height;

        if (Param.cAngle = 90)  then
          R := Rect(DRect.Left,DRect.Top,
                    DRect.Right - fh,
                    DRect.Bottom)
        else
        if (Param.cAngle = 180)  then
          R := Rect(DRect.Left,DRect.Top + fh,
                    DRect.Right ,
                    DRect.Bottom)
        else
        if (Param.cAngle = 270)  then
          R := Rect(DRect.Left + fh,
                  DRect.Top,
                  DRect.Right,
                  DRect.Bottom)
        else
          R := Rect(DRect.Left,DRect.Top,
                  DRect.Right ,
                  DRect.Bottom -  fh);
      aCanvas.StretchDraw(R,Bmp);
      DrawLabel(aCanvas, DRect);
      finally
        aCanvas.Font := oldFont;
        barcodeFont.Free
      end;
    end
    else
      aCanvas.StretchDraw(DRect,Bmp);

    ShowFrame;

  finally
    Bmp.Free;
    RestoreCoord;
  end;
end;

procedure TfrCustomBarCodeView.Print(Stream: TStream);
begin
  BeginDraw(Canvas);
  Memo1.Assign(Memo);
  CurReport.InternalOnEnterRect(Memo1, Self);
  frInterpretator.DoScript(Script);
  if not Visible then Exit;

  if Memo1.Count > 0 then
    if (Length(Memo1[0]) > 0) and (Pos('[',Memo1[0])<>0) then
      Memo1[0] := frParser.Calc(Memo1[0]);
  Stream.Write(Typ, 1);
  frWriteString(Stream, ClassName);
  SaveToStream(Stream);
end;

procedure TfrCustomBarCodeView.DefinePopupMenu(Popup: TPopupMenu);
begin
  // no specific items in popup menu
end;

procedure TfrCustomBarCodeView.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  
  RestoreProperty('BarType',XML.GetValue(Path+'BarCode/BarType',''));
  RestoreProperty('ShowText',XML.GetValue(Path+'BarCode/ShowText',''));
  RestoreProperty('CheckSum',XML.GetValue(Path+'BarCode/CheckSum',''));
  RestoreProperty('Zoom',XML.GetValue(Path+'BarCode/Zoom','1'));
  RestoreProperty('Angle',XML.GetValue(Path+'BarCode/Angle','0'));
end;

procedure TfrCustomBarCodeView.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  
  XML.SetValue(Path+'BarCode/BarType', GetSaveProperty('BarType'));
  XML.SetValue(Path+'BarCode/ShowText', GetSaveProperty('ShowText'));
  XML.SetValue(Path+'BarCode/CheckSum', GetSaveProperty('CheckSum'));
  XML.SetValue(Path+'BarCode/Zoom', GetSaveProperty('Zoom'));
  XML.SetValue(Path+'BarCode/Angle', GetSaveProperty('Angle'));
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
  If (Key>#31) and not (Key in ['0'..'9','.']) then {AJW}
    Key:=#0;
end;

procedure TfrBarCodeForm.ShowEditor(t:TfrView);
var
  tmp:Double;
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
    edZoom.Text:=SysUtils.Format('%.1f',[Param.cRatio]);

    if ShowModal = mrOk then
    begin
      Memo.Clear;
      Memo.Add(M1.Text);
      CheckSum  := ckCheckSum.Checked;
      ShowText  := ckViewText.Checked;
      BarType   := TBarcodeType(cbType.ItemIndex);
      tmp := StrToFloatDef(edZoom.Text,1);
      if tmp<1.0 then
        tmp:=1.0;
      Zoom := tmp;
        
      if RB1.Checked then
        Angle := 0
      else if RB2.Checked then
        Angle := 90
      else if RB3.Checked then
        Angle := 180
      else
        Angle := 270;
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
  Bmp := nil;
  bc := TBarCode.Create(nil);
  try
    if Pos('[',M1.Text) <> 0 then
    begin
      bc.Text := cbDefaultText;
      bc.checksum := true
    end
    else
    begin
      bc.Text := M1.Text;
      bc.CheckSum  := ckCheckSum.Checked;
    end;
    bc.Ratio := StrToFloatDef(edZoom.Text,1);
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
  finally
    Bmp.Free;
    bc.Free;
  end;
end;

procedure InitializeBarcAddin;
begin
  if not assigned(frBarCodeForm) {and not (csDesigning in ComponentState)} then
  begin
    frBarCodeForm := TfrBarCodeForm.Create(nil);
    frSetAddinEditor(TfrBarcodeView, frBarcodeForm);
    frSetAddinIcon(TfrBarcodeView, frBarCodeForm.Image1.Picture.Bitmap);
    frSetAddinHint(TfrBarcodeView, sInsBarcode);
  end;
end;

{ TfrBarCodeObject }

constructor TfrBarCodeObject.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  InitializeBarcAddin;
end;

initialization
  frBarcodeForm := nil;
  frRegisterObject(TfrBarCodeView, nil, '', nil, @InitializeBarcAddin);

finalization
  if Assigned(frBarCodeForm) then
    frBarCodeForm.Free;

end.
