(*
                           udlgpropertiesprinter.pp
                                ------------
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

 Author : Olivier Guilbaud (OG)

 Abstract:
   Printer properties dialog.
   This code makes it possible to modify the properties of a printer.
   
 history
   oct  01 2003 OG - Creation
   sept 07 2004 OG - Use new Printer Media property
   sept 29 2004 OG - Modify for use new CUPSPrinters unit
   mar  08 2005 OG - Modifications for Printer4Lazarus pakage
   
*)
unit uDlgPropertiesPrinter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls,Buttons,Printers, OsPrinters in '../osprinters.pas';

type

  { Tdlgpropertiesprinter }

  Tdlgpropertiesprinter = class(TForm)
    btnCancel1: TBUTTON;
    btnOk: TBUTTON;
    cbPaperSize: TCOMBOBOX;
    cbPaperType: TCOMBOBOX;
    cbPaperSrc: TCOMBOBOX;
    cbBanStart: TCOMBOBOX;
    cbBanEnd: TCOMBOBOX;
    gbOrientation: TGROUPBOX;
    gbDuplex: TGROUPBOX;
    gbBanners: TGROUPBOX;
    gbPagesSheet: TGROUPBOX;
    imgDuplex: TIMAGE;
    imgPageSheet: TIMAGE;
    imgOrientation: TIMAGE;
    labBanStart: TLABEL;
    labBanEnd: TLABEL;
    labPaperSrc: TLABEL;
    labPaperType: TLABEL;
    labPaperSize: TLABEL;
    PagesProperties: TNOTEBOOK;
    pgGeneral: TPAGE;
    pgMarge: TPAGE;
    Panel1: TPANEL;
    rbSheet1: TRADIOBUTTON;
    rbSheet2: TRADIOBUTTON;
    rbSheet4: TRADIOBUTTON;
    rbDuplexShort: TRADIOBUTTON;
    rbDuplexLong: TRADIOBUTTON;
    rbDuplexNone: TRADIOBUTTON;
    rbrev_portrait: TRADIOBUTTON;
    rbrev_Landscape: TRADIOBUTTON;
    rbLandscape: TRADIOBUTTON;
    rbPortrait: TRADIOBUTTON;
    procedure FormDestroy(Sender: TObject);
    procedure rbPortraitCLICK(Sender: TObject);
    procedure cbPaperSizeKEYPRESS(Sender: TObject; var Key: Char);
    procedure dlgpropertiesprinterCREATE(Sender: TObject);
    procedure dlgpropertiesprinterSHOW(Sender: TObject);
  private
    { private declarations }
    fPaperSizeOptions,FMediaTypeOptions,FInputSlotOptions: TStringList;
    
    procedure RefreshInfos;

    procedure InitCombo(aCombo: TCombobox; aKeyWord,aDefault : string; OptList: TStrings);
    
    function number_up_supported: string;
  public
    { public declarations }
    procedure InitProperties;
  end;

var
  dlgpropertiesprinter: Tdlgpropertiesprinter;

implementation

Type
  THackCUPSPrinter=Class(TCUPSPrinter);

procedure Tdlgpropertiesprinter.dlgpropertiesprinterSHOW(Sender: TObject);
begin
  if Sender=nil then ;
  RefreshInfos;
end;

procedure Tdlgpropertiesprinter.cbPaperSizeKEYPRESS(Sender: TObject;
  var Key: Char);
begin
  if Sender=nil then ;
  Key:=#0;
end;

function Tdlgpropertiesprinter.number_up_supported: string;
Var Lst : TStringList;
begin
  Lst:=TStringList.Create;
  try
    THackCUPSPrinter(Printer).GetEnumAttributeString('number-up-supported',Lst);
    Result:=Lst.CommaText;
  finally
    Lst.Free;
  end;
end;

//Initialization
procedure Tdlgpropertiesprinter.dlgpropertiesprinterCREATE(Sender: TObject);
Var Lst : TStringList;
    i   : Integer;
    pOr : TPrinterOrientation;
    St  : String;
begin
  if Sender=nil then ;
  
  FPaperSizeOptions := TStringList.Create;
  FMediaTypeOptions := TStringlist.Create;
  FInputSlotOptions := TStringList.Create;

  InitCombo(cbPaperSize,'PageSize',Printer.PaperSize.PaperName, FPaperSizeOptions);
  InitCombo(cbPaperType,'MediaType','Plain Paper',FMediaTypeOptions);
  InitCombo(cbPaperSrc ,'InputSlot','Auto Sheet Feeder',FInputSlotOptions);

  Lst:=TStringList.Create;
  try
    i:=THackCUPSPrinter(Printer).EnumPPDChoice(Lst,'Duplex');
    gbDuplex.Tag:=i;
    gbDuplex.Enabled:=(Lst.Count>0);
  finally
    Lst.Free;
  end;

  //Enum banners
  Lst:=TStringList.Create;
  try
    THackCUPSPrinter(Printer).GetEnumAttributeString('job-sheets-supported',Lst);
    gbBanners.Enabled:=(Lst.Count>0);
    If Lst.Count>0 then
    begin
      cbBanStart.Items.Assign(Lst);
      cbBanEnd.Items.Assign(Lst);
      St:=THackCUPSPrinter(Printer).cupsGetOption('job-sheets');
      if St<>'' then
      begin
        if Pos(',',St)=0 then
        begin
          cbBanStart.ItemIndex:=cbBanStart.Items.IndexOf(St);
          cbBanEnd.ItemIndex  :=cbBanEnd.Items.IndexOf('none');
        end
        else
        begin
          cbBanStart.ItemIndex:=cbBanStart.Items.IndexOf(Copy(St,1,Pos(',',St)-1));
          cbBanEnd.ItemIndex  :=cbBanEnd.Items.IndexOf(Copy(St,1+Pos(',',St),MaxInt));
        end;
      end
      else
      begin
        THackCUPSPrinter(Printer).GetEnumAttributeString('job-sheets-default',Lst);
        if Lst.Count>0 then cbBanStart.Items.IndexOf(Lst.Strings[0]);
        if Lst.Count>1 then cbBanEnd.Items.IndexOf(Lst.Strings[1]);
      end;
    end;
  finally
    Lst.Free;
  end;

  //Initialization of paper orientation
  pOr:=Printer.Orientation;
  rbPortrait.Checked      :=(pOr=poPortrait);
  rbLandscape.Checked     :=(pOr=poLandscape);
  rbRev_Portrait.Checked  :=(pOr=poReversePortrait);
  rbRev_Landscape.Checked :=(pOr=poReverseLandscape);

  //Initialization of Number pages per sheet
  gbPagesSheet.Enabled:=(number_up_supported<>'');
  if gbPagesSheet.Enabled then
  begin
    St:=THackCUPSPrinter(Printer).cupsGetOption('number-up');
    i:=StrToIntDef(St,THackCUPSPrinter(Printer).GetAttributeInteger('number-up-default',0));
    rbSheet2.Checked:=(i=2);
    rbSheet4.Checked:=(i=4);
    rbSheet1.Checked:=((i=1) or (i>4));
  end;
end;

procedure Tdlgpropertiesprinter.rbPortraitCLICK(Sender: TObject);
begin
  if Sender=nil then ;
  RefreshInfos;
end;

procedure Tdlgpropertiesprinter.FormDestroy(Sender: TObject);
begin
  if Sender=nil then ;
  FPaperSizeOptions.Free;
  FMediaTypeOptions.Free;
  FInputSlotOptions.Free;
end;

procedure Tdlgpropertiesprinter.RefreshInfos;
var St  : string;
begin
  //image orientation
  St:='portrait';
  if rbLandscape.Checked then St:='landscape';
  if rbrev_Landscape.Checked then St:='rev_landscape';
  if rbrev_portrait.Checked then St:='rev_portrait';
  imgOrientation.Picture.PixMap.TransparentColor:=clNone;
  imgOrientation.Picture.PixMap.LoadFromLazarusResource(St);
  imgOrientation.Picture.PixMap.Transparent:=True;

  //image "Pages per sheet"
  St:='pagesheet_1';
  if rbSheet2.Checked then St:='pagesheet_2';
  if rbSheet4.Checked then St:='pagesheet_4';
  imgPageSheet.Picture.PixMap.TransparentColor:=clNone;
  imgPageSheet.Picture.PixMap.LoadFromLazarusResource(St);
  imgPageSheet.Picture.PixMap.Transparent:=True;
  
  //Images "Duplex"
  St:='dupl_none';
  if rbDuplexLong.Checked then St:='dupl_long';
  if rbDuplexShort.Checked then St:='dupl_short';
  imgDuplex.Picture.PixMap.TransparentColor:=clNone;
  imgDuplex.Picture.PixMap.LoadFromLazarusResource(St);
  imgDuplex.Picture.PixMap.Transparent:=True;
end;

//Initialization of an combobox with ppd options
procedure Tdlgpropertiesprinter.InitCombo(aCombo: TCombobox; aKeyWord,aDefault: string; OptList: TStrings);
Var
  i : Integer;
begin
  If Assigned(aCombo) then
  begin
    //Save the default choice or marked choice
    aCombo.Tag:=THackCUPSPrinter(Printer).EnumPPDChoice(aCombo.Items,aKeyWord,OptList);
    aCombo.Enabled:=(aCombo.Items.Count>0);
    //i:=aCombo.Items.IndexOf(aDefault);
    i := OptList.IndexOf(aDefault);
    if i=-1 then i:=aCombo.Tag;
    if i>-1 then
      aCombo.ItemIndex:=i;
  end;
end;

//Set the options for the selected printer
procedure Tdlgpropertiesprinter.InitProperties;
Var St : string;
begin
  THackCUPSPrinter(Printer).FreeOptions;
  THackCUPSPrinter(Printer).SetOptionsOfPrinter;

  //PageSize
  if (cbPaperSize.Items.Count>0) and (cbPaperSize.ItemIndex<>cbPaperSize.Tag) then
  begin
    //THackCUPSPrinter(Printer).cupsAddOption('PageSize',cbPaperSize.Text);
    THackCUPSPrinter(Printer).cupsAddOption('PageSize',
      fPaperSizeOptions[cbPaperSize.ItemIndex]);
  end;

  //MediaType
  if (cbPaperType.Items.Count>0) and (cbPaperType.ItemIndex<>cbPaperType.Tag) then
    THackCUPSPrinter(Printer).cupsAddOption('MediaType',
      fMediaTypeOptions[cbPaperType.ItemIndex]);

  //InputSlot
  if (cbPaperSrc.Items.Count>0) and (cbPaperSrc.ItemIndex<>cbPaperSrc.Tag) then
    THackCUPSPrinter(Printer).cupsAddOption('InputSlot',
      FInputSlotOptions[cbPaperSrc.ItemIndex]);
      

  //Duplex
  if gbDuplex.Enabled then
  begin
    if rbDuplexNone.Checked then
     THackCUPSPrinter(Printer).cupsAddOption('Duplex','None');
    if rbDuplexLong.Checked then
     THackCUPSPrinter(Printer).cupsAddOption('Duplex','DuplexNoTumble');
    if rbDuplexShort.Checked then
     THackCUPSPrinter(Printer).cupsAddOption('Duplex','DuplexTumble');
  end;
  
  //Orientation
  if gbOrientation.Enabled then
  begin
    St:='';
    if rbPortrait.Checked      then St:='3';
    if rbLandscape.Checked     then St:='4';
    if rbrev_Landscape.Checked then St:='5';
    if rbrev_portrait.Checked  then St:='6';
    if (St<>'') then
      THackCUPSPrinter(Printer).cupsAddOption('orientation-requested',St);
  end;
  
  //Page per sheet
  if gbPagesSheet.Enabled then
  begin
    St:='0';
    if rbSheet1.Checked then St:='1';
    if rbSheet2.Checked then St:='2';
    if rbSheet4.Checked then St:='4';
    if THackCUPSPrinter(Printer).GetAttributeInteger('number-up-default',0)<>StrToInt(St) then
      THackCUPSPrinter(Printer).cupsAddOption('number-up',St);
  end;
  
  //Banners
  if gbBanners.Enabled then
  begin
    St:=cbBanStart.Text+','+cbBanEnd.Text;
    THackCUPSPrinter(Printer).cupsAddOption('job-sheets',St);
  end;
end;

initialization
  {$I udlgpropertiesprinter.lrs}
  {$I printerprop.lrs}
end.
