
{*****************************************}
{                                         }
{             FastReport v2.3             }
{            Document options             }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Dopt;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls,
  LR_Const, ButtonPanel, ComCtrls;

type

  { TfrDocOptForm }

  TfrDocOptForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CB1: TCheckBox;
    CB2: TCheckBox;
    edComments: TMemo;
    edAutor: TEdit;
    edtMaj: TEdit;
    edtMinor: TEdit;
    edtRelease: TEdit;
    edtBuild: TEdit;
    edKeyWords: TEdit;
    edSubject: TEdit;
    edTitle: TEdit;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    ImageList1: TImageList;
    labComments: TLabel;
    labPrinter: TLabel;
    labMaj: TLabel;
    labMinor: TLabel;
    labRelease: TLabel;
    labBuild: TLabel;
    labKeyWords: TLabel;
    labSubject: TLabel;
    labAutor: TLabel;
    labTitle: TLabel;
    ListBox1: TListBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frDocOptForm: TfrDocOptForm;

implementation

{$R *.lfm}

uses LR_Prntr, math;

procedure TfrDocOptForm.FormActivate(Sender: TObject);
begin
  OnActivate:=nil;
  ListBox1.Items.Assign(Prn.Printers);
  ListBox1.ItemIndex := Prn.PrinterIndex;
  ListBox1.ItemHeight:=Max(ListBox1.Canvas.TextHeight('Wg')+8, 20);
end;

procedure TfrDocOptForm.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex:=0;
  Caption := sDocOptFormOpt;
  labPrinter.Caption := sDocOptFormPrinter;
  CB1.Caption := sDocOptFormSelect;
  GroupBox2.Caption := sDocOptFormOther;
  CB2.Caption := sDocOptForm2Pass;
  labTitle.Caption:=sDocOptFormTitle;
  labSubject.Caption:=sDocOptFormSubject;
  labKeywords.Caption:=sDocOptFormKeyWords;
  labComments.Caption:=sDocOptFormComments;
  labAutor.Caption:=sDocAutor;
  //Page control
  TabSheet1.Caption:=sDesOptionsFormOpt;
  TabSheet2.Caption:=sDocOptFormOther;

  //Version group box
  GroupBox3.Caption:=sDocVersion;
  labMaj.Caption:=sDocMajor;
  labMinor.Caption:=sDocMinor;
  labRelease.Caption:=sDocRelease;
  labBuild.Caption:=sDocBuild;
end;

procedure TfrDocOptForm.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  ImgRect:TRect;
begin
  ListBox1.Canvas.FillRect(ARect);       { clear the rectangle }
  if ListBox1.Items.Count > Index then
  begin
    ImageList1.Draw(ListBox1.Canvas, ARect.Left + 2, ARect.Top + 2, 0, True);
    ListBox1.Canvas.TextOut(ARect.Left + 24, (ARect.Top + ARect.Bottom  - ListBox1.Canvas.TextHeight('Wg')) div 2, ListBox1.Items[Index]);  { display the text }
  end;
end;

end.

