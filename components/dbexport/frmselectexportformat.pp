{
 **********************************************************************
  This file is part of the Free Pascal run time library.

  See the file COPYING.FPC, included in this distribution,
  for details about the license.
 **********************************************************************

 Select export format from available formats.

 Copyright (c) 2007 by Michael Van Canneyt, member of the Free Pascal development team

}
unit frmSelectExportFormat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ButtonPanel, fpdbexport, sdb_consts;

type

  { TSelectExportFormatForm }

  TSelectExportFormatForm = class(TForm)
    BPButtons: TButtonPanel;
    RGFormats: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    function GetSelected: TExportFormatItem;
    procedure SetSelected(const AValue: TExportFormatItem);
  private
    { private declarations }
  public
    { public declarations }
    Procedure FillFormats;
    Property SelectedFormat : TExportFormatItem Read GetSelected Write SetSelected;
  end; 

var
  SelectExportFormatForm: TSelectExportFormatForm;

implementation

{$R *.lfm}

{ TSelectExportFormatForm }

procedure TSelectExportFormatForm.FormShow(Sender: TObject);
begin
  FillFormats;
end;

procedure TSelectExportFormatForm.FormCreate(Sender: TObject);
begin
  //
  Caption:= sdb_Selectdataexportformat;
  RGFormats.Caption:=sdb_Availableexportformats;
  //
end;

function TSelectExportFormatForm.GetSelected: TExportFormatItem;
begin
  With RGFormats do
    If (ItemIndex=-1) then
      Result:=Nil
    else
      Result:=Items.Objects[ItemIndex] as TExportFormatItem;
end;

procedure TSelectExportFormatForm.SetSelected(const AValue: TExportFormatItem);
begin
  With RGFormats do
    begin
    If (Items.Count=0) then
      FillFormats;
    If (AValue=Nil) then
      ItemIndex:=-1
    else
      ItemIndex:=Items.IndexOfObject(AValue);
    end;
end;

procedure TSelectExportFormatForm.FillFormats;

Var
  F : TExportFormats;
  I : Integer;
  FI : TExportFormatItem;
  
begin
  F:=ExportFormats;
  For I:=0 to F.Count-1 do
    begin
    FI:=F[i];
    RGFormats.Items.AddObject(FI.Description,FI);
    end;
  If RGFormats.Items.Count>0 then
    RGFormats.ItemIndex:=0;
end;

end.

