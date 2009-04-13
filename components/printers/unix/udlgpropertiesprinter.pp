(*
                           udlgpropertiesprinter.pp
                                ------------
 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
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
  StdCtrls,Buttons,Printers, CupsLCL, OsPrinters in '../osprinters.pas', LCLProc,
  CupsDyn;

type

  { Tdlgpropertiesprinter }

  Tdlgpropertiesprinter = class(TForm)
    btnCancel1: TBUTTON;
    btnOk: TBUTTON;
    cbPaperSize: TCOMBOBOX;
    cbResolution: TComboBox;
    cbPaperType: TCOMBOBOX;
    cbPaperSrc: TCOMBOBOX;
    cbBanStart: TCOMBOBOX;
    cbBanEnd: TCOMBOBOX;
    gbOrientation: TGROUPBOX;
    gbBanners: TGROUPBOX;
    gbPagesSheet: TGROUPBOX;
    imgPageSheet: TIMAGE;
    imgOrientation: TIMAGE;
    labBanStart: TLABEL;
    labBanEnd: TLABEL;
    labPaperSrc: TLABEL;
    labResolution: TLabel;
    labPaperType: TLABEL;
    labPaperSize: TLABEL;
    pgAdvanced: TPage;
    Notebook1: TNotebook;
    pgGeneral: TPAGE;
    pgMargins: TPage;
    Panel1: TPANEL;
    rbSheet1: TRADIOBUTTON;
    rbSheet2: TRADIOBUTTON;
    rbSheet4: TRADIOBUTTON;
    rbrev_portrait: TRADIOBUTTON;
    rbrev_Landscape: TRADIOBUTTON;
    rbLandscape: TRADIOBUTTON;
    rbPortrait: TRADIOBUTTON;
    sb: TScrollBox;
    procedure FormDestroy(Sender: TObject);
    procedure rbPortraitCLICK(Sender: TObject);
    procedure cbPaperSizeKEYPRESS(Sender: TObject; var Key: Char);
    procedure dlgpropertiesprinterCREATE(Sender: TObject);
    procedure dlgpropertiesprinterSHOW(Sender: TObject);
  private
    { private declarations }
    procedure RefreshInfos;

    function number_up_supported: string;
    procedure SetupOptions;
    procedure SetupAdvancedOptions;

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

procedure Tdlgpropertiesprinter.SetupOptions;
var
  Lst : TStringList;
  i   : Integer;
  pOr : TPrinterOrientation;
  St  : String;
begin

  {$IFDEF DebugCUPS}
  THackCUPSPrinter(Printer).DebugOptions;
  {$ENDIF}

  SetupCupsCombo(cbPaperSize, nil, 'PageSize');
  SetupCupsCombo(cbPaperType, nil, 'MediaType');
  if not cbPaperType.Enabled then begin
    cbPaperType.Items.Add('Not Available');
    cbPaperType.ItemIndex:=0;
  end;
  SetupCupsCombo(cbPaperSrc, nil, 'InputSlot');
  SetupCupsCombo(cbResolution, nil, 'Resolution');
  st := THackCUPSPrinter(Printer).GetResolutionOption;
  if not cbResolution.Enabled then begin
    cbResolution.Items.Add(st);
    cbResolution.ItemIndex:=0;
  end;

  //Enum banners
  Lst:=TStringList.Create;
  try
    THackCUPSPrinter(Printer).GetEnumAttributeString('job-sheets-supported',Lst);
    gbBanners.Enabled:=(Lst.Count>0);
    if gbBanners.Enabled then
    begin

      if Lst.IndexOf('none')<0 then
        Lst.Insert(0, 'none');

      cbBanStart.Items.Assign(Lst);
      cbBanEnd.Items.Assign(Lst);

      Lst.Clear;
      St := THackCUPSPrinter(Printer).cupsGetOption('job-sheets');
      if St='' then
        THackCUPSPrinter(Printer).GetEnumAttributeString('job-sheets-default',Lst)
      else
        Lst.CommaText := St;

      gbBanners.Enabled:= (Lst.Count=2);
      if gbBanners.Enabled then
      begin
        cbBanStart.ItemIndex:= cbBanStart.Items.IndexOf(Lst[0]);
        cbBanStart.Tag:=cbBanStart.ItemIndex;
        cbBanEnd.ItemIndex:=cbBanEnd.Items.IndexOf(Lst[1]);
        cbBanEnd.Tag:=cbBanEnd.ItemIndex;
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
  gbOrientation.Tag := ord(pOr);

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

  SetupAdvancedOptions;
end;

// TODO: do this in a frame to make printer properties dialog
procedure Tdlgpropertiesprinter.SetupAdvancedOptions;
var
  Group: pppd_group_t;
  Option: pppd_option_t;
  c,g,k,y: Integer;
  lab: TLabel;
  Bevel: TBevel;
  Combo: TCombobox;

  function CheckOption: boolean;
  begin
    // TODO: handle PPD_UI_PICKMANY options with checkgroup
    // TODO: not tested PPD_UI_BOOLEAN, ppd spec recommeds checkbox
    result :=
      (Option^.ui <> PPD_UI_PICKMANY) and
      (strcomp('PageSize', Option^.keyword)<>0) and
      (strcomp('PageRegion', Option^.keyword)<>0) and
      (strcomp('MediaType', Option^.keyword)<>0) and
      (strcomp('Resolution', Option^.keyword)<>0) and
      (strcomp('InputSlot', Option^.keyword)<>0);
  end;

begin
  if (THackCUPSPrinter(Printer).CupsPPD=nil) or
     (THackCUPSPrinter(Printer).CupsPPD^.num_groups=0)
  then
    exit;

  y := C_SPACE-C_GROUPSPACE;

  g := 0;
  Group := THackCUPSPrinter(Printer).CupsPPD^.groups;
  while (Group<>nil) and (g<THackCUPSPrinter(Printer).CupsPPD^.num_groups)  do
  begin

    // count options per group
    c := 0;
    k := 0;
    Option := Group^.options;
    while (Option<>nil) and (c<Group^.num_options) do
    begin
      if CheckOption then
        inc(K);
      Inc(Option);
      Inc(c);
    end;

    if k>0 then
    begin
      // add group's caption
      Inc(Y, C_GROUPSPACE);

      // todo: use exclusively anchor options to do layout
      lab := TLabel.Create(Self);
      lab.Font.Style:=lab.font.style + [fsBold];
      lab.Top := Y;
      lab.Caption := group^.text;
      lab.BorderSpacing.Around:=C_SPACE;
      lab.AnchorSideLeft.Control := sb;
      lab.Parent := sb;

      Bevel := TBevel.Create(Self);
      Bevel.Shape := bsTopLine;
      Bevel.Top:= y + lab.Height div 2;
      Bevel.Height:= C_SPACE div 2;
      Bevel.BorderSpacing.Around := C_SPACE;
      Bevel.AnchorSideLeft.Control := lab;
      Bevel.AnchorSideLeft.Side := asrBottom;
      Bevel.AnchorSideRight.Control := sb;
      Bevel.AnchorSideRight.Side := asrBottom;
      Bevel.anchors := [akLeft, akTop, akRight];
      Bevel.Parent := sb;

      inc(y, Lab.Height);

      // add options
      c := 0;
      Option := Group^.options;
      while (Option<>nil) and (c<Group^.num_options) do
      begin

        if CheckOption then
        begin
          y := y + C_SPACE;

          lab := TLabel.Create(Self);
          lab.Layout:=tlCenter;
          lab.AutoSize := false;
          lab.Top := Y;
          lab.Width:=220;
          lab.BorderSpacing.Around:=C_SPACE;
          lab.AnchorSideLeft.Control := sb;
          lab.Caption := Option^.text;
          lab.Parent := sb;

          combo := TCombobox.Create(self);
          combo.Style:= csDropDownList;
          combo.Top:= y + lab.Height div 2;
          combo.BorderSpacing.Around := C_SPACE;
          combo.AnchorSideLeft.Control := lab;
          combo.AnchorSideLeft.Side := asrBottom;
          combo.AnchorSideRight.Control := sb;
          combo.AnchorSideRight.Side := asrBottom;
          combo.anchors := [akLeft, akTop, akRight];
          combo.Parent := sb;
          lab.Height:= combo.height;

          SetupCupsCombo(Combo, Option);

          inc(y, combo.height + C_SPACE );
        end;

        Inc(Option);
        Inc(C);
      end; // option

    end; // valid group

    inc(group);
    inc(g);
  end; // group

end;

//Initialization
procedure Tdlgpropertiesprinter.dlgpropertiesprinterCREATE(Sender: TObject);
begin
  if Sender=nil then ;
  Notebook1.PageIndex:=0;
  SetupOptions;
end;

procedure Tdlgpropertiesprinter.rbPortraitCLICK(Sender: TObject);
begin
  if Sender=nil then ;
  RefreshInfos;
end;

procedure Tdlgpropertiesprinter.FormDestroy(Sender: TObject);
begin
  if Sender=nil then ;
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
end;

//Set the options for the selected printer
procedure Tdlgpropertiesprinter.InitProperties;
var
  St: string;
  i: Integer;

  function GetSelOrientation: TPrinterOrientation;
  begin
    if rbLandscape.Checked then
      result := poLandscape
    else
    if rbRev_Portrait.Checked then
      result := poReversePortrait
    else
    if rbRev_Landscape.Checked then
      result := poReverseLandscape
    else
      result := poPortrait;
  end;

begin

  {$IFDEF DebugCUPS}
  THackCupsPrinter(Printer).DebugOptions;
  {$ENDIF}

  CheckCupsComboChanges(cbResolution);
  CheckCupsComboChanges(cbPaperSize);
  CheckCupsComboChanges(cbPaperType);
  CheckCupsComboChanges(cbPaperSrc);

  //Orientation
  if gbOrientation.Enabled and (gbOrientation.Tag<>Ord(GetSelOrientation)) then
    Printer.Orientation := GetSelOrientation;
  
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
  if gbBanners.Enabled and
    (
      (cbBanStart.Tag<>cbBanStart.ItemIndex) or
      (cbBanEnd.Tag<>cbBanEnd.ItemIndex)
    ) then
  begin
    St:=cbBanStart.Text+','+cbBanEnd.Text;
    THackCUPSPrinter(Printer).cupsAddOption('job-sheets',St);
  end;

  // check advanced options
  for i := 0 to sb.ControlCount-1 do
    if sb.Controls[i] is TCombobox then
      CheckCupsComboChanges(TCombobox(sb.Controls[i]));

  {$IFDEF DebugCUPS}
  THackCupsPrinter(Printer).DebugOptions;
  {$ENDIF}
end;

initialization
  {$I udlgpropertiesprinter.lrs}
  {$I printerprop.lrs}
end.
