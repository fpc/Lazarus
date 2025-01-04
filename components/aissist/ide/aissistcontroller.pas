{ Copyright (C) 2024

 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Michael Van Canneyt

  Abstract: AI Assistant controller
}
unit AIssistController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AIClient, JanAIProtocol,  SrcEditorIntf, IDEOptionsIntf, IDEOptEditorIntf, BaseIDEIntf;

Type
  TAIState = (csDisconnected,csConnected,csWaiting,csAIThinking);

  { TAIssistController }
  TAIssistController = Class(TComponent)
  private
    FConfigFrame: TAbstractIDEOptionsEditorClass;
    FSettings: TAIServerSettings;
    class var _Instance: TAIssistController;
  Public
    class constructor Init;
    class destructor done;
    Class property Instance : TAIssistController Read _Instance;
  Public
    constructor create(aOwner : TComponent); override;
    Destructor destroy; override;
    procedure HandleShowConfig(Sender : TObject);
    function ShowConfig: Boolean;
    Procedure LoadConfig;
    Procedure SaveConfig;
    Function CreateAIClient : TAIClient;
    Function ExplainCurrentSelection (aEdit : TSourceEditorInterface): Boolean;
    Function Configured : Boolean;
    Property Settings : TAIServerSettings Read FSettings;
    Property ConfigFrame : TAbstractIDEOptionsEditorClass Read FConfigFrame Write FConfigFrame;
  end;

Function AIController : TAIssistController;

implementation

uses LazIDEintf, StrAIssist, LazConfigStorage, forms, frmaixplain;

const
  DefaultMaxLength = 2048;

function AIController: TAIssistController;
begin
  Result:=TAIssistController.Instance;
end;

{ TAIssistController }

class constructor TAIssistController.Init;
begin
  _Instance:=TAIssistController.Create(nil);
end;

class destructor TAIssistController.done;
begin
  FreeAndNil(_Instance);
end;

constructor TAIssistController.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  FSettings:=TAIServerSettings.Create;
  FSettings.Protocol:=TJanAIServerProtocol.protocolname;
end;

destructor TAIssistController.destroy;
begin
  FreeAndNil(FSettings);
  inherited destroy;
end;

procedure TAIssistController.HandleShowConfig(Sender: TObject);
begin
  ShowConfig;
end;

function TAIssistController.ShowConfig : Boolean;
begin
  Result:=LazarusIDE.DoOpenIDEOptions(ConfigFrame);
end;

procedure TAIssistController.LoadConfig;
var
  Storage : TConfigStorage;
begin
  Storage:=GetIDEConfigStorage(SConfigFile, True);
  with Storage do
    try
      Settings.Protocol := GetValue(KeyProtocol,Settings.Protocol);
      Settings.BaseURL := GetValue(KeyServerURL,'');
      Settings.DefaultModel:= GetValue(KeyDefaultModel,'');
      Settings.DefaultMaxLength:= GetValue(KeyDefaultMaxLength,DefaultMaxLength);
    finally
      Free;
    end;
end;

procedure TAIssistController.SaveConfig;
var
  Storage : TConfigStorage;
begin
  Storage:=GetIDEConfigStorage(SConfigFile, True);
  with Storage do
    try
      SetDeleteValue(KeyServerURL,Settings.BaseURL,'');
      SetDeleteValue(KeyProtocol,Settings.Protocol,TJanAIServerProtocol.protocolname);
      SetDeleteValue(KeyDefaultModel,Settings.DefaultModel,'');
      SetDeleteValue(KeyDefaultMaxLength,Settings.DefaultMaxLength, DefaultMaxLength);
    finally
      Free;
    end;
end;

function TAIssistController.CreateAIClient: TAIClient;
begin
  Result:=Nil;
  If Not Configured then exit;
  Result:=TAIClient.Create(Self);
  Result.Settings:=Self.Settings;
end;

function TAIssistController.ExplainCurrentSelection(aEdit : TSourceEditorInterface): Boolean;

var
  frm : TAIxplainForm;
  Clnt : TAIClient;
  lPos,Caret : TPoint;

begin
  // todo: show messages
  if Not Assigned(aEdit) then exit(False);
  if not Configured then exit(False);

  frm:=TAIxplainForm.Create(Application);
  Caret:=aEdit.CursorScreenXY;
  lPos:=aEdit.EditorControl.ClientToScreen(aEdit.ScreenToPixelPosition(Caret));
  Frm.Top:=lPos.Y;
  Frm.Left:=lPos.X;
  Clnt:=CreateAIClient;
  Frm.Explain(aEdit,Clnt);
  frm.Show;
end;

function TAIssistController.Configured: Boolean;
begin
  Result:=(Settings.BaseURL<>'');
  Result:=Result and ((Settings.Protocol<>'') and (TAIClient.FindProtocolClass(Settings.Protocol)<>Nil));
  Result:=Result and (Settings.DefaultModel<>'');
  Result:=Result and (Settings.DefaultMaxLength>500);
end;

end.

