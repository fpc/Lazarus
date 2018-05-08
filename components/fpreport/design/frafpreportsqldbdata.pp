{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Frame to configure a SQL report data loop.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frafpreportsqldbdata;


interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, EditBtn, StdCtrls, Buttons, ActnList, SynEdit, SynHighlighterSQL,
  fpreportdesignreportdata, fpjson, sqldb, fpreportdatasqldb, dialogs;

type
  TFrame = TReportDataConfigFrame;

  { TSQLReportDataConfigFrame }

  TSQLReportDataConfigFrame = class(TFrame)
    ATest: TAction;
    ALJSON: TActionList;
    AConstruct: TAction;
    EConnection: TEditButton;
    ILJSON: TImageList;
    Label1: TLabel;
    LSQl: TLabel;
    SpeedButton1: TSpeedButton;
    SESQL: TSynEdit;
    SynSQLSyn1: TSynSQLSyn;
    procedure ATestExecute(Sender: TObject);
    procedure EConnectionButtonClick(Sender: TObject);
  private
    FConnectionData: TJSONObject;

  public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure GetConfig(aConfig : TJSONObject); override;
    Procedure SetConfig(aConfig : TJSONObject); override;
    Function SaveNotOKMessage : string; override;
    Property ConnectionData : TJSONObject Read FConnectionData;
  end;

  { TSQLDBReportDataHandler }


implementation

uses  frmfpreportdataconnectioneditor;



{$R *.lfm}

{ TSQLReportDataConfigFrame }

procedure TSQLReportDataConfigFrame.EConnectionButtonClick(Sender: TObject);

Var
  F : TReportConnectionEditorForm;

begin
  F:=TReportConnectionEditorForm.Create(Self);
  try
    F.Params:=FConnectionData;
    if F.ShowModal=mrOK then
      begin
      FreeAndNil(FConnectionData);
      FConnectionData:=F.Params.Clone as TJSONObject;
      EConnection.Text:=FConnectionData.AsJSON;
      end;
  finally
    F.Free;
  end;
end;

procedure TSQLReportDataConfigFrame.ATestExecute(Sender: TObject);

Var
  S : String;

begin
  S:=TFPReportConnector.TestConnection(FConnectionData);
  if (S<>'') then
    MessageDlg(SErrConnectionNotOK,S,mtError,[mbOK],0)
  else
    MessageDlg(SSuccess,SConnectionSuccesful,mtInformation,[mbOK],0)
end;

constructor TSQLReportDataConfigFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FConnectionData:=TJSONObject.Create;
end;

destructor TSQLReportDataConfigFrame.Destroy;
begin
  FreeAndNil(FConnectionData);
  inherited Destroy;
end;

procedure TSQLReportDataConfigFrame.GetConfig(aConfig: TJSONObject);

begin
  aConfig.Objects[keyConnection]:=FConnectionData.CLone as TJSONObject;
  aConfig.strings[keySQL]:=SESQL.Text;
end;

procedure TSQLReportDataConfigFrame.SetConfig(aConfig: TJSONObject);

Var
  O : TJSONObject;
begin
  O:=aConfig.Get(keyConnection,TJSONObject(Nil));
  if Assigned(O) then
    begin
    FreeAndNil(FConnectionData);
    FConnectionData:=O.Clone as TJSONObject;
    EConnection.Text:=FConnectionData.asJSON;
    end;
  SESQL.Text:=aConfig.get(keySQL,'');
end;

function TSQLReportDataConfigFrame.SaveNotOKMessage: string;
begin
  if (FConnectionData.Count=0) then
    Result:=SErrNoConnectionData
  else if Trim(SESQL.Text)='' then
    Result:=SErrNoSQL
  else
    Result:=TFPReportConnector.TestConnection(FConnectionData);
end;

initialization
  TSQLDBReportDataHandler.RegisterConfigClass(TSQLReportDataConfigFrame);
end.

