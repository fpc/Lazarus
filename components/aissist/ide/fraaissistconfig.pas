{ Copyright (C) 2024

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael Van Canneyt

  Abstract: AI Assistant configuration frame
}
unit fraAIssistConfig;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, llm.Client,
  IDEOptionsIntf, IDEOptEditorIntf, LazNumEdit;

type

  { TAIAssistentConfigFrame }

  TAIAssistentConfigFrame = class(TAbstractIDEOptionsEditor)
    btnRefresh: TButton;
    CBProtocol: TComboBox;
    cbModel: TComboBox;
    edtAPIKey: TEdit;
    edtURL: TEdit;
    Label1: TLabel;
    lblProtocol: TLabel;
    lblModel: TLabel;
    edtMaxResponseLength: TLazIntegerEdit;
    lblMaxLength: TLabel;
    lblURL: TLabel;
    procedure CBProtocolChange(Sender: TObject);
    procedure HandleRefreshClick(Sender: TObject);
  private
    FBusy : Boolean;
    FClient : TLLMClient;
    procedure CheckURL;
    function ExtractModelID(const S: String): string;
    procedure GetModelNames;
    procedure HandleModels(Sender: TObject; aModels: TGetModelsResult);
  public
    function GetTitle: String; override;
    procedure Setup({%H-}ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings({%H-}AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings({%H-}AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;

  end;

implementation

uses StrAIssist, AIssistController;

{$R *.lfm}

{ TAIAssistentConfigFrame }

procedure TAIAssistentConfigFrame.HandleModels(Sender: TObject; aModels: TGetModelsResult);

var
  aModel : TModelData;
  Idx,I : Integer;
  lItem : string;
begin
  Writeln('Handling models');
  FBusy:=False;
  Idx:=-1;
  With cbModel.Items do
    begin
    BeginUpdate;
    Clear;
    For aModel in aModels.Value do
      begin
      lItem:='['+aModel.ID+'] '+aModel.Name;
      Writeln('Adding ',lItem);
      I:=Add(lItem);
      if SameText(aModel.ID,AIController.Settings.DefaultModel) then
        Idx:=I;
      end;
    EndUpdate;
    end;
  if CBModel.Text='' then
    cbModel.ItemIndex:=Idx;
end;

function TAIAssistentConfigFrame.GetTitle: String;
begin
  Result:=SConfigTitle;
end;

procedure TAIAssistentConfigFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  if ADialog<>Nil then ; // Silence compiler warning
  TLLMClient.GetProtocolList(CBProtocol.Items);
end;

procedure TAIAssistentConfigFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  CBProtocol.ItemIndex:=CBProtocol.Items.IndexOf(AIController.Settings.Protocol);
  EdtURL.Text:=AIController.Settings.BaseURL;
  edtAPIKey.Text:=AIController.Settings.AuthorizationKey;
  CheckURL;
  cbModel.Text:=AIController.Settings.DefaultModel;
  edtMaxResponseLength.Value:=AIController.Settings.DefaultMaxLength;
  if AIController.Configured then
    GetModelNames;
end;

procedure TAIAssistentConfigFrame.HandleRefreshClick(Sender: TObject);
begin
  GetModelNames;
end;

procedure TAIAssistentConfigFrame.CBProtocolChange(Sender: TObject);

begin
  CheckURL;
end;

procedure TAIAssistentConfigFrame.CheckURL;

var
  lClass : TLLMProtocolClass;

begin
  if edtURL.Text<>'' then
    exit;
  lClass:=TLLMClient.FindProtocolClass(CBProtocol.Text);
  if lClass<>Nil then
    edtURL.Text:=lClass.DefaultURL;
end;

procedure TAIAssistentConfigFrame.GetModelNames;
begin
  if FBusy then exit;
  if not Assigned(FClient) then
    FClient:=TLLMClient.Create(Self);
  FClient.Settings.Protocol:=cbProtocol.Text;
  FClient.Settings.BaseURL:=edtURL.Text;
  FClient.Settings.AuthorizationKey:=edtAPIKey.Text;
  FClient.SynchronizeCallBacks:=True;
  FBusy:=True;
  FClient.GetModels(@HandleModels);
end;

function TAIAssistentConfigFrame.ExtractModelID(const S : String) : string;

var
  P1,P2 : Integer;

begin
  P1:=Pos('[',S);
  P2:=Pos(']',S);
  if (P1>0) and (P2>P1) then
    Result:=Copy(S,P1+1,P2-P1-1)
  else
    Result:=S;
end;

procedure TAIAssistentConfigFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  AIController.Settings.Protocol:=cbProtocol.Text;
  AIController.Settings.BaseURL := EdtURL.Text;
  AIController.Settings.AuthorizationKey := edtAPIKey.Text;
  AIController.Settings.DefaultModel := ExtractModelID(cbModel.Text);
  AIController.Settings.DefaultMaxLength := edtMaxResponseLength.Value;
  AIController.SaveConfig;
end;

class function TAIAssistentConfigFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result:=IDEEditorGroups.GetByIndex(GroupEnvironment)^.GroupClass;
end;

end.

