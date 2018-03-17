{
    This file is part of the FPReport report designer.
    Copyright (c) 2017 by the Free Pascal development team

    Factory class pattern for the various forms that make up the designer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit reportdesignbaseforms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpreport, forms, db, fpreportdesignobjectlist,
  fpreportdata, fpreportdesignreportdata;

Type
  { TReportEditorForm }

  TBaseReportEditorForm = Class(TForm)
  private
    FReport: TFPCustomReport;
  Protected
    procedure SetReport(AValue: TFPCustomReport); virtual;
  Public
    Property Report : TFPCustomReport Read FReport Write SetReport;
  end;
  TBaseReportEditorFormClass = class of TBaseReportEditorForm;


  TBaseReportResizeForm = Class(TBaseReportEditorForm)
  Protected
    function GetH: TSizeAdjust; virtual; abstract;
    function GetHS: TFPReportUnits; virtual; abstract;
    function GetV: TSizeAdjust; virtual; abstract;
    function GetVS: TFPReportUnits; virtual; abstract;
    procedure SetH(AValue: TSizeAdjust); virtual; abstract;
    procedure SetHS(AValue: TFPReportUnits);virtual; abstract;
    procedure SetV(AValue: TSizeAdjust);virtual; abstract;
    procedure SetVS(AValue: TFPReportUnits);virtual; abstract;
  public
    Property Horizontal : TSizeAdjust Read GetH Write SetH;
    Property Vertical : TSizeAdjust Read GetV Write SetV;
    Property HorizontalSize : TFPReportUnits Read GetHS Write SetHS;
    Property VerticalSize : TFPReportUnits Read GetVS Write SetVS;
  end;
  TBaseReportResizeFormClass = Class of TBaseReportResizeForm;

  TBaseReportAlignForm = Class(TBaseReportEditorForm)
  Protected
    function GetH: THAlignAction; virtual; abstract;
    function GetV: TVAlignAction; virtual; abstract;
    procedure SetH(AValue: THAlignAction); virtual; abstract;
    procedure SetV(AValue: TVAlignAction); virtual; abstract;
  public
    Property Horizontal : THAlignAction Read GetH Write SetH;
    Property Vertical : TVAlignAction Read GetV Write SetV;
  end;
  TBaseReportAlignFormClass = class of TBaseReportAlignForm;

  { TReportDataForm }

  { TBaseReportDataForm }

  TBaseReportDataForm = Class(TBaseReportEditorForm)
  private
    FData: TFPReportDataDefinitions;
  Protected
    procedure SetData(AValue: TFPReportDataDefinitions); virtual;
  public
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    Property Data : TFPReportDataDefinitions Read FData Write SetData;
  end;
  TBaseReportDataFormClass = Class of TBaseReportDataForm;

  { TBaseReportDataPreviewForm }

  TBaseReportDataPreviewForm = class (TBaseReportEditorForm)
  Private
    FDataset: TDataset;
  Protected
    procedure SetDataSet(AValue: TDataset); virtual;
  public
    Property PreviewDataset : TDataset Read FDataset Write SetDataSet;
  end;
  TBaseReportDataPreviewFormClass = class of TBaseReportDataPreviewForm;

  { TBaseReportVariablesForm }

  TBaseReportVariablesForm = class(TBaseReportEditorForm)
  private
    FVariables: TFPReportVariables;
  Protected
    procedure SetVariables(AValue: TFPReportVariables); virtual;
    function CreateVariables: TFPReportVariables; virtual;
  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Property Variables : TFPReportVariables Read FVariables Write SetVariables;
  end;
  TBaseReportVariablesFormClass = Class of TBaseReportVariablesForm;

Var
  ReportDataFormClass : TBaseReportDataFormClass;
  ReportResizeFormClass : TBaseReportResizeFormClass;
  ReportAlignFormClass : TBaseReportAlignFormClass;
  ReportDataPreviewClass : TBaseReportDataPreviewFormClass;
  ReportVariablesFormClass : TBaseReportVariablesFormClass;
  ReportPropertiesFormClass : TBaseReportEditorFormClass;

implementation

{ TBaseReportVariablesForm }

procedure TBaseReportVariablesForm.SetVariables(AValue: TFPReportVariables);
begin
  if FVariables=AValue then Exit;
  FVariables.Assign(AValue);
end;

function TBaseReportVariablesForm.CreateVariables: TFPReportVariables;
begin
  Result:=TFPReportVariables.Create(Nil,TFPReportVariable);
end;

constructor TBaseReportVariablesForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVariables:=CreateVariables;
end;

destructor TBaseReportVariablesForm.Destroy;
begin
  FreeAndNil(FVariables);
  inherited Destroy;
end;

{ TBaseReportDataPreviewForm }

procedure TBaseReportDataPreviewForm.SetDataSet(AValue: TDataset);
begin
  FDataset:=AValue;
end;

{ TReportDataForm }

procedure TBaseReportDataForm.SetData(AValue: TFPReportDataDefinitions);
begin
  if FData=AValue then Exit;
  FData.Assign(AValue);
end;

constructor TBaseReportDataForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FData:=TFPReportDataDefinitions.Create(TFPReportDataDefinitionItem);
end;

destructor TBaseReportDataForm.Destroy;
begin
  FreeAndNil(FData);
  inherited Destroy;
end;

{ TReportEditorForm }

procedure TBaseReportEditorForm.SetReport(AValue: TFPCustomReport);
begin
  if FReport=AValue then Exit;
  FReport:=AValue;
end;

end.

