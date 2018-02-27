{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Form to edit a SQLDB data connection, used in data management.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmfpreportdataconnectioneditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel, Buttons, sqldb, fpjson;

type

  { TReportConnectionEditorForm }

  TReportConnectionEditorForm = class(TForm)
    BPConnection: TButtonPanel;
    CBType: TComboBox;
    ECharset: TEdit;
    ERole: TEdit;
    EUserName: TEdit;
    EPassword: TEdit;
    EHostName: TEdit;
    EDatabaseName: TEdit;
    Label2: TLabel;
    LEUserName: TLabel;
    LCBType: TLabel;
    LEHostName: TLabel;
    LEDatabaseName: TLabel;
    LEPassword: TLabel;
    LERole: TLabel;
    LMParams: TLabel;
    MParams: TMemo;
    SBTest: TSpeedButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SBTestClick(Sender: TObject);
  private
    FParams: TJSONObject;
    procedure FormToParams;
    procedure ParamsToForm;
    procedure Setparams(AValue: TJSONObject);
  public
    Property Params : TJSONObject Read FParams Write Setparams;
  end;

var
  ReportConnectionEditorForm: TReportConnectionEditorForm;

Resourcestring
  SConnectionSuccesful = 'Connection to the database was succesfully made.';
  SErrConnectionNotOK = 'Error connecting to the database';
  SSuccess = 'Succesfully connected.';


implementation

uses strutils, reportdesigndatasql;


{$R *.lfm}

{ TReportConnectionEditorForm }

procedure TReportConnectionEditorForm.FormCreate(Sender: TObject);
begin
  FParams:=TJSONObject.Create;
  GetConnectionList(CBType.Items);
end;

procedure TReportConnectionEditorForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);

Var
  S : String;

begin
  if ModalResult=mrOK then
    begin
    FormToParams;
    S:=TFPReportConnector.TestConnection(FParams);
    if (S<>'') then
      MessageDlg(SErrConnectionNotOK,S,mtError,[mbOK],0);
    end;
  CanClose:=(S='');
end;

procedure TReportConnectionEditorForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FParams);
end;

procedure TReportConnectionEditorForm.SBTestClick(Sender: TObject);

Var
  C : TFPReportConnector;
  S : String;

begin
  FormToParams;
  S:=TFPReportConnector.TestConnection(FParams);
  if (S<>'') then
    MessageDlg(SErrConnectionNotOK,S,mtError,[mbOK],0)
  else
    MessageDlg(SSuccess,SConnectionSuccesful,mtInformation,[mbOK],0);
end;

procedure TReportConnectionEditorForm.Setparams(AValue: TJSONObject);
begin
  if FParams=AValue then Exit;
  FreeAndNil(FParams);
  FParams:=AValue.Clone as TJSONObject;
  ParamsToForm;
end;

procedure TReportConnectionEditorForm.FormToParams;

  Procedure CB(Ed : TCombobox; aName : string);

  begin
    if (Ed.Text='') then
      FParams.Delete(aName)
    else
     FParams.Strings[aName]:=Ed.Text
  end;

  Procedure E(Ed : TEdit; aName : string);

  begin
    if (Ed.Text='') then
      FParams.Delete(aName)
    else
      if Ed.EchoMode=emPassword then
        FParams.Strings[aName]:=XorEncode(keyHash,Ed.Text)
      else
        FParams.Strings[aName]:=Ed.Text;
  end;

  Procedure M(Ed : TMemo; aName : string);

  Var
    I : Integer;
    A : TJSONArray;

  begin
    A:=FParams.get(aName,TJSONArray(Nil));
    if (A=Nil) then
      begin
      A:=TJSONArray.Create;
      FParams.Add(aName,A);
      end
    else
      A.Clear;
    if (Ed.Lines.Count>0) then
      For I:=0 to Ed.lines.Count-1 do
        if Ed.Lines[i]<>'' then
          A.Add(Ed.Lines[i]);
    if A.Count=0 then
      FParams.Delete(aName);
  end;

begin
  CB(CBtype,keyType);
  E(EHostName,keyHostName);
  E(EDatabaseName,keyDatabaseName);
  E(EUserName,keyUserName);
  E(EPassword,keyPassword);
  E(ERole,keyRole);
  E(ECharset,keyCharset);
  M(MParams,keyParams);
end;

procedure TReportConnectionEditorForm.ParamsToForm;

  Procedure E(Ed : TEdit; aName : string);

  Var
    S : String;

  begin
    S:=FParams.Get(aName,'');
    if Ed.EchoMode=emPassword then
      Ed.Text:=XorDecode(keyHash,S)
    else
      Ed.Text:=S;
  end;

  Procedure M(Ed : TMemo; aName : string);

  Var
    I : Integer;
    A : TJSONArray;

  begin
    Ed.Clear;
    A:=FParams.get(aName,TJSONArray(Nil));
    if Assigned(A) then
      For I:=0 to A.Count-1 do
        if A.Strings[i]<>'' then
          Ed.lines.Add(A.Strings[i]);
  end;

begin
  CBType.ItemIndex:=CBType.Items.IndexOf(FParams.Get(keyType,''));
  E(EHostName,keyHostName);
  E(EDatabaseName,keyDatabaseName);
  E(EUserName,keyUserName);
  E(EPassword,keyPassword);
  E(ERole,keyRole);
  E(ECharset,keyCharset);
  M(MParams,keyParams);
end;

end.

