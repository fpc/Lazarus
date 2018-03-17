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
unit frafpreportdbfdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dbf, FileUtil, Forms, Controls, EditBtn, StdCtrls, fpjson, fpreportdesignreportdata, db;

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


implementation

uses fpreportdatadbf;

{$R *.lfm}


{ TDBFReportDataFrame }

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

initialization
  TDBFReportDataHandler.RegisterConfigClass(TDBFReportDataFrame);
end.

