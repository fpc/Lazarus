{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2007 by Michael Van Canneyt, member of the
    Free Pascal development team

    fpDBExport basic configuration dialog.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmBaseConfigExport;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ButtonPanel, EditBtn, CheckLst, ComCtrls, RTTIGrids, fpdbexport,
  Buttons, ActnList, sdb_consts;

type
  { TBaseConfigExportForm }
  TBaseConfigExportForm = class(TForm)
    AUp: TAction;
    ADown: TAction;
    ALFields: TActionList;
    CLBFields: TCheckListBox;
    EFileName: TFileNameEdit;
    LEFileName: TLabel;
    Label2: TLabel;
    LCLBFields: TLabel;
    PExportFieldList: TPanel;
    PUPDown: TPanel;
    PFieldProps: TPanel;
    PFieldsTop: TPanel;
    PCFields: TPageControl;
    PFileName: TPanel;
    PButtons: TButtonPanel;
    GFormatting: TTIPropertyGrid;
    SBup: TSpeedButton;
    SBDown: TSpeedButton;
    TSFields: TTabSheet;
    TSFormatting: TTabSheet;
    SplitterFields: TSplitter;
    GFieldProps: TTIPropertyGrid;
    FExporter: TCustomDatasetExporter;
    procedure ADownExecute(Sender: TObject);
    procedure ADownUpdate(Sender: TObject);
    procedure AUpExecute(Sender: TObject);
    procedure AUpUpdate(Sender: TObject);
    procedure CLBFieldsClick(Sender: TObject);
    procedure CLBFieldsClickCheck(Sender: TObject);
    procedure CLBFieldsItemClick(Sender: TObject; Index: integer);
    procedure CLBFieldsKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SetExporter(const AValue: TCustomDatasetExporter);
    Procedure OnOKClick(Sender : TObject);
    procedure ExporterToForm;
  private
    { private declarations }
    FFieldMap : TExportFields;
    FFormatting : TCustomExportFormatSettings;
    procedure FormToExporter;
    procedure MoveFieldDown;
    function MoveFieldUp: Boolean;
    procedure SelectField(F: TExportFieldItem);
    procedure ShowSelectedField;
  public
    { public declarations }
    Property Exporter : TCustomDatasetExporter Read FExporter Write SetExporter;
  end; 

var
  BaseConfigExportForm: TBaseConfigExportForm;

Function ShowBaseExportConfig (AExporter : TCustomDatasetExporter) : Boolean;

Procedure RegisterBaseExportConfigForm;

implementation

uses typinfo,lcltype;

Function ShowBaseExportConfig (AExporter : TCustomDatasetExporter) : Boolean;

begin
  With TBaseConfigExportForm.Create(Application) do
    Try
      Exporter:=AExporter;
      Result:=(ShowModal=mrOK);
    Finally
      Free;
    end;
end;

Type

  { TShowBaseConfigDialog }

  TShowBaseConfigDialog = Class(TObject)
  Public
    Function ShowConfig (AExporter : TCustomDatasetExporter) : Boolean;
  end;

{ TShowBaseConfigDialog }

function TShowBaseConfigDialog.ShowConfig(AExporter: TCustomDatasetExporter
  ): Boolean;
  
begin
  Result:=ShowBaseExportConfig(AExporter);
end;

Var
  DLG : TShowBaseConfigDialog;

Procedure RegisterBaseExportConfigForm;

Var
  EF : TExportFormats;
  I : Integer;

begin
  EF:=ExportFormats;
  For I:=0 to EF.Count-1 do
    begin
    If Not assigned(EF[i].OnConfigureDialog) then
      begin
      If DLG=Nil then
        DLG:=TShowBaseConfigDialog.Create;
      EF[i].OnConfigureDialog:=@DLG.ShowConfig;
      end;
    end;
end;

{ TBaseConfigExportForm }

procedure TBaseConfigExportForm.SetExporter(const AValue: TCustomDatasetExporter
  );
begin
  if (FExporter=AValue) then
    exit;
  FExporter:=AValue;
  If Assigned(FExporter) then
    ExporterToForm;
end;

procedure TBaseConfigExportForm.OnOKClick(Sender: TObject);
begin
  FormToExporter;
end;

procedure TBaseConfigExportForm.CLBFieldsItemClick(Sender: TObject;
  Index: integer);
begin
  CLBFields.ItemIndex:=Index;
  ShowSelectedField;
  With CLBFields do
    If (ItemIndex<>-1) then
      begin
      FFieldMap[ItemIndex].Enabled:=Checked[ItemIndex];
      GFieldProps.PropertyEditorHook.RefreshPropertyValues;
      end;
end;

procedure TBaseConfigExportForm.CLBFieldsKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Shift=[ssShift] then
    begin
    If (Key=VK_UP)  then
      MoveFieldUp
    else if (Key=VK_DOWN) then
      MoveFieldDown
    end;
end;

procedure TBaseConfigExportForm.CLBFieldsClick(Sender: TObject);
begin
  ShowSelectedField;
end;

procedure TBaseConfigExportForm.ShowSelectedField;

begin
  If (CLBFields.ItemIndex=-1) then
    SelectField(Nil)
  else
    SelectField(FFieldMap[CLBFields.ItemIndex]);
end;

procedure TBaseConfigExportForm.AUpExecute(Sender: TObject);
begin
  MoveFieldUp;
end;

Function TBaseConfigExportForm.MoveFieldUp : Boolean;

begin
  Result:=false;
  With CLBFields do
    If (ItemIndex>0) then
      begin
      Items.Exchange(ItemIndex,ItemIndex-1);
      FFieldMap.Items[ItemIndex].Index:=ItemIndex-1;
      ItemIndex:=ItemIndex-1;
      end;
end;

procedure TBaseConfigExportForm.ADownExecute(Sender: TObject);
begin
  MoveFieldDown;
end;

procedure TBaseConfigExportForm.MoveFieldDown;

begin
  With CLBFields do
    If (ItemIndex<Items.Count-1) then
      begin
      Items.Exchange(ItemIndex,ItemIndex+1);
      FFieldMap.Items[ItemIndex].Index:=ItemIndex+1;
      ItemIndex:=ItemIndex+1;
      end;
end;

procedure TBaseConfigExportForm.ADownUpdate(Sender: TObject);
begin
  With CLBFields do
    (Sender as Taction).Enabled:=(Itemindex<Items.Count-1);
end;

procedure TBaseConfigExportForm.AUpUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=(CLBFields.Itemindex>0)
end;

procedure TBaseConfigExportForm.CLBFieldsClickCheck(Sender: TObject);
begin
end;

procedure TBaseConfigExportForm.FormCreate(Sender: TObject);
begin
  //
  Caption:= sdb_Configuredataexport;
  LEFileName.Caption:= sdb_Filename;
  TSFields.Caption:= sdb_Fields;
  TSFormatting.Caption:= sdb_Formatting;
  LCLBFields.Caption:= sdb_Selectfieldstoexport;
  Label2.Caption:= sdb_Propertiesforselectedfield;
  SBup.Hint:= sdb_Moveselectedfieldup;
  SBDown.Hint:= sdb_Moveselectedfielddown;
  //
  PButtons.OKButton.OnClick:=@OnOKClick;
end;

procedure TBaseConfigExportForm.SelectField(F : TExportFieldItem);

begin
  GFieldProps.TIObject:=F;
  GFieldProps.Enabled:=(F<>Nil);
end;


procedure TBaseConfigExportForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FFieldMap);
  FreeAndNil(FFormatting);
end;

procedure TBaseConfigExportForm.ExporterToForm;

Var
  B : Boolean;
  EF : TExportFieldItem;
  I,J : Integer;
  FS : TCustomExportFormatSettings;
  
begin
  B:=Exporter is TCustomFileExporter;
  B:=B or (FindPropInfo(Exporter,'FileName')<>Nil);
  PFileName.Visible:=B;
  If B then
    begin
    if Exporter is TCustomFileExporter then
      EFileName.FileName:=TCustomFileExporter(Exporter).FileName
    else
      EFileName.FileName:=GetStrProp(Exporter,'FileName');
    EFileName.Filter:=ExportFormats.ConstructFilter(Exporter);
    end;
  { The following construct means that only explicitly added
    can be configured, or all fields. }
  FreeAndNil(FFieldMap);
  FFieldMap:=TExportFields.Create(Exporter.ExportFields.ItemClass);
  If (Exporter.ExportFields.Count=0) then
    Exporter.BuildDefaultFieldMap(FFieldMap)
  else
    For I:=0 to Exporter.ExportFields.Count-1 do
      FFieldMap.Add.Assign(Exporter.ExportFields[i]);
  For I:=0 to FFieldMap.Count-1 do
    begin
    EF:=FFieldMap[i];
    J:=CLBFields.Items.AddObject(EF.FieldName,EF);
    CLBFields.Checked[J]:=EF.Enabled;
    end;
  If (CLBFields.Items.Count>0) then
    begin
    CLBFields.ItemIndex:=0;
    SelectField(FFieldMap[0])
    end
  else
    begin
    CLBFields.ItemIndex:=-1;
    SelectField(Nil);
    end;
  B:=FindPropInfo(Exporter,'FormatSettings')<>Nil;
  TSFormatting.TabVisible:=B;
  If B then
    begin
    FS:=TCustomExportFormatSettings(GetObjectProp(Exporter,'FormatSettings'));
    FFormatting:=TCustomExportFormatSettingsClass(FS.ClassType).Create(False);
    FFormatting.Assign(FS);
    GFormatting.TIObject:=FFormatting;
    end
  else
    FreeAndNil(FFormatting);
end;

procedure TBaseConfigExportForm.FormToExporter;

Var
  I : Integer;
  FS : TCustomExportFormatSettings;

begin
  If PFileName.Visible then
    if (Exporter is TCustomFileExporter) then
      TCustomFileExporter(Exporter).FileName:=EFileName.FileName
    else
      SetStrProp(Exporter,'FileName',EFileName.FileName);
  If (Exporter.ExportFields.Count=0) then
    begin
    For I:=0 to FFieldMap.Count-1 do
      Exporter.ExportFields.Add.Assign(FFieldMap[i]);
    end
  else
    For I:=0 to FFieldMap.Count-1 do
      Exporter.ExportFields[I].Assign(FFieldMap[i]);
  If Assigned(FFormatting) then
    begin
    FS:=TCustomExportFormatSettings(GetObjectProp(Exporter,'FormatSettings'));
    FS.Assign(FFormatting);
    end;
end;


initialization
  {$I frmbaseconfigexport.lrs}

Finalization
  FreeAndNil(DLG);
end.

