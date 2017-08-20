{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Report DBF data configuration frame.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fradbfdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbf, FileUtil, Forms, Controls, EditBtn, StdCtrls, fpjson, designreportdata, db;

type
  TFrame = TReportDataConfigFrame;

  { TDBFReportDataFrame }

  TDBFReportDataFrame = class(TFrame)
    FEData: TFileNameEdit;
    LFEData: TLabel;
  private
  public
    Procedure GetConfig(aConfig : TJSONObject); override;
    Procedure SetConfig(aConfig : TJSONObject); override;
    Function SaveNotOKMessage : string; override;
  end;

  { TDBFReportDataHandler }

  TDBFReportDataHandler = Class(TDesignReportDataHandler)
    Function CreateDataset(AOwner : TComponent; AConfig : TJSONObject) : TDataset; override;
    Function CreateConfigFrame(AOwner : TComponent) : TReportDataConfigFrame; override;
    Class Function CheckConfig(AConfig: TJSONObject): String; override;
    Class Function DataType : String; override;
    Class Function DataTypeDescription : String; override;
  end;

implementation

{$R *.lfm}
Resourcestring
  SErrNeedFileName = 'Need a DBF file name';
  SFileNameDoesNotExist = 'Filename does not exist: "%s"';

{ TDBFReportDataFrame }

Const
  keyFileName = 'filename';

procedure TDBFReportDataFrame.GetConfig(aConfig: TJSONObject);
begin
  aConfig.Strings[keyFileName]:=FEData.FileName;
end;

procedure TDBFReportDataFrame.SetConfig(aConfig: TJSONObject);
begin
  FEData.FileName:=aConfig.Get(keyFileName,'');
end;

function TDBFReportDataFrame.SaveNotOKMessage: string;
begin
  Result:='';
  if FEData.FileName='' then
    Result:=SErrNeedFileName
  else if not FileExists(FEData.FileName) then
    Result:=Format(SFileNameDoesNotExist,[FEData.FileName]);
end;

{ TMyDBFDataset }

function TDBFReportDataHandler.CreateDataset(AOwner: TComponent; AConfig: TJSONObject): TDataset;

Var
  C : TDBF;

begin
  C:=TDBF.Create(AOWner);
  C.TableName:=AConfig.Get(KeyFileName,'');
  C.ReadOnly:=True;
  Result:=C;
end;

function TDBFReportDataHandler.CreateConfigFrame(AOwner: TComponent): TReportDataConfigFrame;
begin
  Result:=TDBFReportDataFrame.Create(AOWner);
end;

class function TDBFReportDataHandler.CheckConfig(AConfig: TJSONObject): String;

Var
  FN : UTF8String;

begin
  Result:='';
  FN:=AConfig.Get(KeyFileName,'');
  if FN='' then
    Result:=SErrNeedFileName
  else if not FileExists(FN) then
    Result:=Format(SFileNameDoesNotExist,[FN]);
end;

class function TDBFReportDataHandler.DataType: String;
begin
  Result:='DBF'
end;

class function TDBFReportDataHandler.DataTypeDescription: String;
begin
  Result:='DBase data file';
end;

initialization
  TDBFReportDataHandler.RegisterHandler;
end.

