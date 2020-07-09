{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    TFPREport image editor form.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmfpreportimageedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls, StdCtrls, ExtDlgs, fpreport;

type

  { TReportImageEditorForm }

  TReportImageEditorForm = class(TForm)
    BPImage: TButtonPanel;
    BLoad: TButton;
    BSave: TButton;
    CBStretched: TCheckBox;
    CBFieldName: TComboBox;
    IImage: TImage;
    ODPimage: TOpenPictureDialog;
    RBFixedImage: TRadioButton;
    RBFieldName: TRadioButton;
    SDPImage: TSavePictureDialog;
    procedure BLoadClick(Sender: TObject);
    procedure BSaveClick(Sender: TObject);
    procedure CBStretchedChange(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure RBFixedImageChange(Sender: TObject);
  private
    FReportImage: TFPReportCustomImage;
    procedure CheckEnableds;
    procedure CheckStretchedImage;
    procedure SetFields(R: TFPCustomReport);
    procedure SetReportImage(AValue: TFPReportCustomImage);
  public
    Procedure ImageToForm;
    Procedure FormToImage;
    Property ReportImage : TFPReportCustomImage Read FReportImage Write SetReportImage;
  end;


  { TDefaultReportImageEditor }

  TDefaultReportImageEditor = Class(TFPReportElementEditor)
    Class function DefaultClass: TFPReportElementClass; override;
    Function Execute : Boolean; override;
  end;

var
  ReportImageEditorForm: TReportImageEditorForm;

implementation

uses FPWritePNG,FPReadPNG;


{$R *.lfm}

{ TDefaultReportImageEditor }

class function TDefaultReportImageEditor.DefaultClass: TFPReportElementClass;
begin
  Result:=TFPReportImage;
end;

function TDefaultReportImageEditor.Execute: Boolean;
Var
  F : TReportImageEditorForm;

begin
  F:=TReportImageEditorForm.Create(Self);
  try
    F.ReportImage:=Self.Element as TFPReportCustomImage;
    Result:=F.ShowModal=mrOK;
  finally
    F.Free;
  end;
end;

{ TReportImageEditorForm }

procedure TReportImageEditorForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);


begin
  CanClose:=(ModalResult<>mrOK);
  if Not CanClose then
    begin
    CanClose:=(RBFieldName.Checked and (CBFieldName.ItemIndex<>-1))
              or (RBFixedImage.Checked and (IImage.Width<>0) and (IImage.Height<>0));
    end;
  if CanClose then
    FormToImage;
end;

procedure TReportImageEditorForm.BLoadClick(Sender: TObject);
begin
  with ODPIMage do
    if Execute then
      IImage.Picture.LoadFromFile(FileName);
end;

procedure TReportImageEditorForm.BSaveClick(Sender: TObject);
begin
  with SDPIMage do
    if Execute then
      IImage.Picture.SaveToFile(FileName);
end;

procedure TReportImageEditorForm.CBStretchedChange(Sender: TObject);
begin
  CheckStretchedImage;
end;

procedure TReportImageEditorForm.RBFixedImageChange(Sender: TObject);
begin
  CheckEnableds;
end;

procedure TReportImageEditorForm.CheckStretchedImage;

begin
  IImage.Stretch:=CBStretched.Checked;
end;

procedure TReportImageEditorForm.CheckEnableds;

Var
  isField : Boolean;

begin
  isField:=RBFieldName.Checked;
  CBFieldName.Enabled:=isField;
  BLoad.Enabled:=not isField;
  BSave.Enabled:=not isField;
end;

procedure TReportImageEditorForm.SetFields(R : TFPCustomReport);

Var
  I,J : Integer;
  L,S : TStrings;
  N : String;


begin
  L:=CBFieldName.Items;
  S:=TStringList.Create;
  try
    L.BeginUpdate;
    For I:=0 to TFPReport(R).ReportData.Count-1 do
      begin
      N:=TFPReport(R).ReportData[i].Data.Name;
      TFPReport(R).ReportData[i].Data.GetFieldList(S);
      For J:=0 to S.Count-1 do
        L.Add(N+'.'+S[J]);
      end;
  finally
    L.EndUpdate;
    S.Free;
  end;
end;

procedure TReportImageEditorForm.SetReportImage(AValue: TFPReportCustomImage);


begin
  if FReportImage=AValue then Exit;
  FReportImage:=AValue;
  ImageToForm;
end;

procedure TReportImageEditorForm.ImageToForm;
Var
  RI : TFPReportImage;

begin
  RI:=TFPReportImage(FReportImage);
  SetFields(FReportImage.Report);
  RBFieldName.Checked:=RI.FieldName<>'';
  if RBFieldName.Checked then
    CBFieldName.ItemIndex:=CBFieldName.Items.IndexOf(RI.FieldName)
  else
    begin
    RBFixedImage.Checked:=true;
    If RI.ImageID<>-1 then
      begin
      IImage.Picture.Assign(RI.Image);
      end;
    end;
  CBStretched.Checked:=RI.Stretched;
  CheckEnableds;
  CheckStretchedImage;
end;

procedure TReportImageEditorForm.FormToImage;

Var
  RI : TFPReportImage;
  S : TMemoryStream;

begin
  RI:=TFPReportImage(ReportImage);
  RI.Stretched:=CBStretched.Checked;
  if RBFieldName.Checked then
    RI.FieldName:=CBFieldName.Text
  else
    begin
    S:=TMemoryStream.Create;
    try
      //
      IImage.Picture.SaveToStreamWithFileExt(S,'.png');
      S.Position:=0;
      RI.LoadFromStream(S,TFPReaderPNG);
    finally
      S.Free;
    end;
    end;
end;

Initialization
  TDefaultReportImageEditor.RegisterEditor;
end.

