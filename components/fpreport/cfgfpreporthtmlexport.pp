{
    This file is part of the Free Component Library.
    Copyright (c) 2016 Michael Van Canneyt, member of the Free Pascal development team

    Configure FPReport to fpimage export dialog to be used in LCL preview.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit cfgfpreporthtmlexport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, EditBtn,
  StdCtrls, ButtonPanel, ExtCtrls, Spin, ComCtrls, fpreport, fpreporthtmlutil, fpreporthtmlexport;

type
  { TConfigHTMLExportForm }
//  hnoFirstLast,hnoAlwaysFirstLast,hnoPageNo,hnoImage,hnoSkipStyling,hnoUsePageNOfM,hnoPageNoEdit
  TConfigHTMLExportForm = class(TForm)
    BPExport: TButtonPanel;
    CBBottomNavigator: TCheckBox;
    CBDPI: TComboBox;
    CBFirstlast: TCheckBox;
    CBFrame: TCheckBox;
    CBLeftNavigator: TCheckBox;
    CBRightNavigator: TCheckBox;
    CBTopNavigator: TCheckBox;
    CBAdvancedOptions: TCheckBox;
    CBInlineImage: TCheckBox;
    CBIMGTag: TCheckBox;
    CBFixedPositioning: TCheckBox;
    CBTOCPage: TCheckBox;
    CBMemoAsIS: TCheckBox;
    CBExternalJS: TCheckBox;
    CBPageEdit: TCheckBox;
    CBPageNofM: TCheckBox;
    CBalwaysFirstLast: TCheckBox;
    CBPageNo: TCheckBox;
    CBSkipStyling: TCheckBox;
    CBNavigatorImages: TCheckBox;
    CBTOCSkipStyling: TCheckBox;
    CBNavigatorInActiveColor: TColorButton;
    CBNavigatorActiveColor: TColorButton;
    Edit1: TEdit;
    Edit2: TEdit;
    EOddPageStyle: TEdit;
    EEvenPageStyle: TEdit;
    ESeparator: TEdit;
    FEBaseFileName: TFileNameEdit;
    FETOCCSSFileName: TFileNameEdit;
    FEFrameTOCHtml: TFileNameEdit;
    FEFrameTOCCSS: TFileNameEdit;
    FETOCFileName: TFileNameEdit;
    Label1: TLabel;
    LNavInactive: TLabel;
    LNavActive: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    LNavHeight: TLabel;
    Label7: TLabel;
    LNavWidth: TLabel;
    Label9: TLabel;
    LTOCStyling: TLabel;
    LTOCPageFileName: TLabel;
    Label3: TLabel;
    LCBDPI: TLabel;
    LESeparator: TLabel;
    LFEPDF: TLabel;
    LSEDigits: TLabel;
    LTOCPageFileName1: TLabel;
    PCOptions: TPageControl;
    RGTocZone: TRadioGroup;
    RGStyle: TRadioGroup;
    SEDigits: TSpinEdit;
    Navigation: TTabSheet;
    SENavigatorFixedHeight: TSpinEdit;
    SENavigatorFixedMargin: TSpinEdit;
    SETOCZoneWidth: TSpinEdit;
    SENavigatorFixedWidth: TSpinEdit;
    TabSheet1: TTabSheet;
    TSNavigator: TTabSheet;
    TTocPage: TTabSheet;
    TSFrame: TTabSheet;
    TSAdvanced: TTabSheet;
    TSOptions: TTabSheet;
    procedure CBAdvancedOptionsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure GetImageFilter;
    procedure LocalizeForm;
    { private declarations }
  public
    { public declarations TFPReportExportfpImage } 
    Procedure ConfigToForm(AExporter: TFPReportExportHTML);
    Procedure FormToConfig(AExporter: TFPReportExportHTML);
  end;

  { THTMLConfigObj }

  THTMLConfigObj = Class
  public
    Procedure RegisterHandler;
    procedure DoConfig(Sender: TObject; AExporter: TFPReportExporter; var Cancelled: Boolean);
  end;

var
  ConfigHTMLExportForm: TConfigHTMLExportForm;

Procedure RegisterHTMLExportConfig;

implementation

uses fpimage, fppdf;

{$R *.lfm}

Var
  Cfg : THTMLConfigObj;

Resourcestring
  SFiles = 'Files';

Procedure RegisterHTMLExportConfig;

begin
  FreeAndNil(Cfg);
  Cfg:=THTMLConfigObj.Create;
  Cfg.RegisterHandler;
end;

{ TConfigHTMLExportForm }

procedure TConfigHTMLExportForm.LocalizeForm;


begin
  GetImageFilter;
end;

procedure TConfigHTMLExportForm.GetImageFilter;

Var
  I : Integer;
  S,TN : String;

begin
  S:='';
  with ImageHandlers do
    For I:=0 to Count-1 do
      begin
      TN:=TypeNames[I];
      if (S<>'') then
        S:=S+'|';

      S:=S+TN+' '+SFiles+'|*'+{$IFDEF VER2_6_4}DefaultExtention[TN]{$else}DefaultExtension[TN]{$ENDIF};
      end;
  FEBaseFileName.Filter:=S;
end;

procedure TConfigHTMLExportForm.FormCreate(Sender: TObject);
begin
  LocalizeForm;
  CBAdvancedOptionsChange(Self);
end;

procedure TConfigHTMLExportForm.CBAdvancedOptionsChange(Sender: TObject);
begin
  TSAdvanced.TabVisible:=CBAdvancedOptions.Checked;
  TSFrame.TabVisible:=CBAdvancedOptions.Checked;
  TTocPage.TabVisible:=CBAdvancedOptions.Checked;
  TSNavigator.TabVisible:=CBAdvancedOptions.Checked;
end;

procedure TConfigHTMLExportForm.ConfigToForm(AExporter: TFPReportExportHTML);

  procedure setcb(CB : TCheckBox; O : THTMLExportOption);

  begin
    CB.Checked:=O in AExporter.Options;
  end;

  procedure setcbn(CB : TCheckBox; O : THTMLNavigatorOption);

  begin
    CB.Checked:=O in AExporter.PageNavigator.Options;
  end;

  procedure setcbp(CB : TCheckBox; O : TNavigatorPosition);

  begin
    CB.Checked:=O in AExporter.PageNavigator.Positions;
  end;

  Function RelFileName(FN : String) : String;

  begin
    Result:='';
    if (FN<>'') and (FEBaseFileName.FileName<>'') then
      Result:=ExtractRelativepath(ExtractFilePath(ExpandFileName(FEBaseFileName.FileName)),
                                  ExpandFileName(FN));
  end;

  Procedure Col(CB : TColorButton; Col : TFPReportColor);

  Var
    S : TFPColor;

  begin
    if Col=clNone then
      CB.Color:=graphics.clNone
    else
      begin
      S:=ColorToRGBTriple(Col);
      CB.Color:=RGBToColor(S.red,S.Green,S.Blue);
      end;
  end;

begin
  SetCB(CBInlineImage,heoInlineImage);
  SetCB(CBFixedPositioning,heoFixedPositioning);
  SetCB(CBIMGTag,heoUseIMGtag);
  SetCB(CBTOCPage, heoTOCPage);
  SetCB(CBFrame,heoTOCPageFrame);
  SetCB(CBMemoAsIS,heoMemoAsIs);
  SetCB(CBExternalJS,heoExternalJS);

  SetCBN(CBFirstlast,hnoFirstLast);
  SetCBN(CBAlwaysFirstLast,hnoAlwaysFirstLast);
  SetCBN(CBPageNo,hnoPageNo);
  SetCBN(CBNavigatorImages,hnoImage);
  SetCBN(CBSkipStyling,hnoSkipStyling);
  SetCBN(CBPageNofM,hnoUsePageNOfM);
  SetCBN(CBPageEdit,hnoPageNoEdit);

  SetCBP(CBTopNavigator,npTop);
  SetCBP(CBBottomNavigator,npBottom);
  SetCBP(CBLeftNavigator,npLeft);
  SetCBP(CBRightNavigator,npRight);

  RGStyle.ItemIndex:=Ord(AExporter.StyleEmbedding);

  if (AExporter.BaseFileName<>'') then
    FEBaseFileName.FileName:=ExpandFileName(AExporter.BaseFileName);

  FEFrameTOCHtml.FileName:=AExporter.FramePage.FileName;
  FEFrameTOCCSS.FileName:=AExporter.FramePage.CSSFileName;
  RGTocZone.Itemindex:=Ord(AExporter.FramePage.TOCZonePosition);
  SETOCZoneWidth.Value:=AExporter.FramePage.TOCZoneSize;

  FETOCFileName.FileName:=RelFileName(AExporter.TOCPage.FileName);
  FETOCCSSFileName.FileName:=RelFileName(AExporter.TOCPage.CSSFileName);
  CBTOCSkipStyling.Checked := AExporter.TOCPage.SkipStyling;
  EEvenPageStyle.Text      := AExporter.TOCPage.EvenPageStyle;
  EOddPageStyle.Text       := AExporter.TOCPage.OddPageStyle;

  SENavigatorFixedHeight.Value:=AExporter.PageNavigator.FixedHeight;
  SENavigatorFixedWidth.Value:=AExporter.PageNavigator.FixedWidth;
  SENavigatorFixedMargin.Value:=AExporter.PageNavigator.FixedMargin;
  Col(CBNavigatorActiveColor,AExporter.PageNavigator.ActiveBGColor);
  Col(CBNavigatorInActiveColor,AExporter.PageNavigator.InActiveBGColor);

  CBDPI.Text:=IntToStr(AExporter.DPI);
end;

procedure TConfigHTMLExportForm.FormToConfig(AExporter: TFPReportExportHTML);

Var
  Sep : String;


  procedure setcb(CB : TCheckBox; O : THTMLExportOption);

  begin
    if CB.Checked then
      AExporter.Options:=AExporter.Options+[o]
    else
      AExporter.Options:=AExporter.Options-[o];
  end;

  procedure setcbn(CB : TCheckBox; O : THTMLNavigatorOption);

  begin
    if CB.Checked then
      AExporter.PageNavigator.Options:=AExporter.PageNavigator.Options+[O]
    else
      AExporter.PageNavigator.Options:=AExporter.PageNavigator.Options-[O]
  end;

  procedure setcbp(CB : TCheckBox; O : TNavigatorPosition);

  begin
    if CB.Checked then
      AExporter.PageNavigator.Positions:=AExporter.PageNavigator.Positions+[O]
    else
      AExporter.PageNavigator.Positions:=AExporter.PageNavigator.Positions-[O];
  end;

  Function RelFileName(FN : String) : String;

  begin
    Result:=ExtractRelativepath(ExtractFilePath(ExpandFileName(FEBaseFileName.FileName)),
                                ExpandFileName(FN));
  end;

  Function Col(CB : TColorButton) : TFPReportColor;

  begin
    if CB.Color<>Graphics.clNone then
      Result:=ColorToRGB(CB.Color)
    else
      Result:=clNone;
  end;

begin
  SetCB(CBInlineImage,heoInlineImage);
  SetCB(CBFixedPositioning,heoFixedPositioning);
  SetCB(CBIMGTag,heoUseIMGtag);
  SetCB(CBTOCPage, heoTOCPage);
  SetCB(CBFrame,heoTOCPageFrame);
  SetCB(CBMemoAsIS,heoMemoAsIs);
  SetCB(CBExternalJS,heoExternalJS);

  SetCBN(CBFirstlast,hnoFirstLast);
  SetCBN(CBAlwaysFirstLast,hnoAlwaysFirstLast);
  SetCBN(CBPageNo,hnoPageNo);
  SetCBN(CBNavigatorImages,hnoImage);
  SetCBN(CBSkipStyling,hnoSkipStyling);
  SetCBN(CBPageNofM,hnoUsePageNOfM);
  SetCBN(CBPageEdit,hnoPageNoEdit);

  SetCBP(CBTopNavigator,npTop);
  SetCBP(CBBottomNavigator,npBottom);
  SetCBP(CBLeftNavigator,npLeft);
  SetCBP(CBRightNavigator,npRight);

  AExporter.FramePage.FileName := RelFileName(FEFrameTOCHtml.FileName);
  AExporter.FramePage.CSSFileName := RelFileName(FEFrameTOCCSS.FileName);
  AExporter.FramePage.TOCZonePosition := TTOCPosition(RGTocZone.Itemindex);
  AExporter.FramePage.TOCZoneSize := SETOCZoneWidth.Value;

  AExporter.TOCPage.FileName:=RelFileName(FETOCFileName.FileName);
  AExporter.TOCPage.CSSFileName:=RelFileName(FETOCCSSFileName.FileName);
  AExporter.TOCPage.SkipStyling:=CBTOCSkipStyling.Checked;
  AExporter.TOCPage.EvenPageStyle:=EEvenPageStyle.Text;
  AExporter.TOCPage.OddPageStyle:=EOddPageStyle.Text;

  AExporter.PageNavigator.FixedHeight := SENavigatorFixedHeight.Value;
  AExporter.PageNavigator.FixedWidth  := SENavigatorFixedWidth.Value;
  AExporter.PageNavigator.FixedMargin := SENavigatorFixedMargin.Value;
  AExporter.PageNavigator.ActiveBGColor:=Col(CBNavigatorActiveColor);
  AExporter.PageNavigator.InActiveBGColor:=Col(CBNavigatorInActiveColor);

  if RGStyle.ItemIndex<>-1 then
    AExporter.StyleEmbedding:=TStyleEmbedding(RGStyle.ItemIndex);
  AExporter.BaseFileName:=FEBaseFileName.FileName;
  Sep:=StringReplace(ESeparator.Caption,'%','%%',[]);
  if SEDigits.Value=1 then
    AExporter.SequenceFormat:=Sep+'%d'
  else
    AExporter.SequenceFormat:=Sep+'%.'+IntToStr(SEDigits.Value)+'d';
end;


{ TFPImageConfigObj }

procedure THTMLConfigObj.RegisterHandler;
begin
  if ReportExportManager.FindExporter(TFPReportExportHTML.Name)<>Nil then
    ReportExportManager.RegisterConfigHandler(TFPReportExportHTML.Name,@DoConfig);
end;

procedure THTMLConfigObj.DoConfig(Sender: TObject; AExporter: TFPReportExporter;
  var Cancelled: Boolean);
begin
  Cancelled:=True;
  With TConfigHTMLExportForm.Create(Application) do
    try
      ConfigToForm(AExporter as TFPReportExportHTML);
      Cancelled:=ShowModal<>mrOK;
      if not Cancelled then
        FormToConfig(AExporter as TFPReportExportHTML);
    finally
      Free;
    end;
end;

initialization
  RegisterHTMLExportConfig;
finalization
  FreeAndNil(Cfg);
end.

