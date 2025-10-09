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
  Classes, SysUtils, Math, Forms, Dialogs,
  // ide
  SrcEditorIntf, IDEOptEditorIntf, BaseIDEIntf, LazIDEintf, IDEHelpIntf, IDEDialogs,
  LazConfigStorage, LazLoggerBase, CodeTree, CodeToolManager, PascalParserTool,
  PascalReaderTool,
  // aissist
  LLM.Client, LLM.ollama, llm.chatgpt, llm.claude, llm.gemini,
  // laz_aissist
  StrAIssist, FrmAixplain, FrmAIssistFPDocEdit;

Type
  TAIState = (csDisconnected,csConnected,csWaiting,csAIThinking);

  { TAIssistController }

  TAIssistController = Class(TComponent)
  private
    FConfigFrame: TAbstractIDEOptionsEditorClass;
    FSettings: TLLMServerSettings;
    class var _Instance: TAIssistController;
  Public
    class constructor Init;
    class destructor done;
    Class property Instance : TAIssistController Read _Instance;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    procedure HandleShowConfig(Sender : TObject);
    Function ShowConfig: Boolean;
    Procedure LoadConfig;
    Procedure SaveConfig;
    Function CreateLLMClient : TLLMClient;
    Function ExplainCurrentSelection (aEdit : TSourceEditorInterface): Boolean;
    Procedure FPDocEditorInsertTextClick(var Params: TFPDocEditorTxtBtnParams);
    Function Configured : Boolean;
    Property Settings : TLLMServerSettings Read FSettings;
    Property ConfigFrame : TAbstractIDEOptionsEditorClass Read FConfigFrame Write FConfigFrame;
  end;

Function AIController : TAIssistController;

implementation

uses strutils;

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

constructor TAIssistController.Create(aOwner: TComponent);
begin
  inherited create(aOwner);
  FSettings:=TLLMServerSettings.Create;
  FSettings.Protocol:=TOLLamaProtocol.ProtocolName;
end;

destructor TAIssistController.Destroy;
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
  S : String;
begin
  Storage:=GetIDEConfigStorage(SConfigFile, True);
  with Storage do
    try
      Settings.Protocol := GetValue(KeyProtocol,Settings.Protocol);
      Settings.BaseURL := GetValue(KeyServerURL,'');
      Settings.DefaultModel:= GetValue(KeyDefaultModel,'');
      Settings.DefaultMaxLength:= GetValue(KeyDefaultMaxLength,DefaultMaxLength);
      // Trivial Decode
      S:=GetValue(KeyAuthorizationKey,'');
      if S<>'' then
        Settings.AuthorizationKey:=XorDecode('FPC',S)
      else
        Settings.AuthorizationKey:='';
    finally
      Free;
    end;
end;

procedure TAIssistController.SaveConfig;
var
  Storage : TConfigStorage;
  S : String;
begin
  Storage:=GetIDEConfigStorage(SConfigFile, True);
  with Storage do
    try
      SetDeleteValue(KeyServerURL,Settings.BaseURL,'');
      SetDeleteValue(KeyProtocol,Settings.Protocol,TOLLamaProtocol.ProtocolName);
      SetDeleteValue(KeyDefaultModel,Settings.DefaultModel,'');
      SetDeleteValue(KeyDefaultMaxLength,Settings.DefaultMaxLength, DefaultMaxLength);
      // trivial encode
      if Settings.AuthorizationKey<>'' then
        S:=XorEncode('FPC',Settings.AuthorizationKey)
      else
        S:='';
      SetDeleteValue(KeyAuthorizationKey,S,'');
    finally
      Free;
    end;
end;

function TAIssistController.CreateLLMClient: TLLMClient;
begin
  Result:=Nil;
  If Not Configured then exit;
  Result:=TLLMClient.Create(Self);
  Result.Settings:=Self.Settings;
end;

function TAIssistController.ExplainCurrentSelection(aEdit : TSourceEditorInterface): Boolean;

var
  frm : TAIxplainForm;
  Clnt : TLLMClient;
  lPos,Caret : TPoint;

begin
  // todo: show messages
  if Not Assigned(aEdit) then exit(False);
  if not Configured then exit(False);

  frm:=TAIxplainForm.Create(LazarusIDE.OwningComponent);
  Caret:=aEdit.CursorScreenXY;
  lPos:=aEdit.EditorControl.ClientToScreen(aEdit.ScreenToPixelPosition(Caret));
  Frm.SetBounds(lPos.X,lPos.Y,Frm.Width,Frm.Height);
  Clnt:=CreateLLMClient;
  Frm.Explain(aEdit,Clnt);
  frm.Show;
end;

procedure TAIssistController.FPDocEditorInsertTextClick(var Params: TFPDocEditorTxtBtnParams);

  procedure ShowNotAtAProc;
  begin
    IDEMessageDialog('Unsupported','Source cursor is not at a function declaration.',mtInformation,[mbOk]);
  end;

var
  Tool: TCodeTool;
  Node, ProcNode: TCodeTreeNode;
  Src, NewTxt: String;
  aForm: TAIssistFPDocEditDlg;
  aClient: TLLMClient;
begin
  Params.Success:=false;
  Tool:=TCodeTool(Params.CodeTool);
  Node:=TCodeTreeNode(Params.CodeNode);

  DebugLn(['TAIssistController.FPDocEditorInsertTextClick START ',Params.Filename,'(',Params.Line,',',Params.Col,') Part=',ord(Params.Part)]);
  if Params.Selection>'' then
    DebugLn(['  Selection="',LeftStr(Params.Selection,100),'{...}',copy(Params.Selection,Max(101,length(Params.Selection)-99),100),'"']);
  if (Tool=nil) or (Node=nil) then
  begin
    debugln(['Error: TAIssistController.FPDocEditorInsertTextClick Node=nil']);
    ShowNotAtAProc;
    exit;
  end;
  //debugln(['TAIssistController.FPDocEditorInsertTextClick Node: Start=',Tool.CleanPosToStr(Node.StartPos),' [',NodePathAsString(Node),'] {',LeftStr(Tool.ExtractNode(Node,[]),200),'}']);

  // find proc body
  ProcNode:=Node;
  while (ProcNode<>nil) and (ProcNode.Desc<>ctnProcedure) do
    ProcNode:=ProcNode.Parent;
  if ProcNode=nil then
  begin
    debugln(['Error: TAIssistController.FPDocEditorInsertTextClick Node=',NodePathAsString(Node)]);
    ShowNotAtAProc;
    exit;
  end;
  Node:=Tool.FindCorrespondingProcNode(ProcNode,[phpInUpperCase],cpsDown);
  if Node<>nil then
    ProcNode:=Node;

  Src:=copy(Tool.Src,ProcNode.StartPos,ProcNode.EndPos-ProcNode.StartPos);
  debugln(['TAIssistController.FPDocEditorInsertTextClick ProcNode: Start=',Tool.CleanPosToStr(ProcNode.StartPos),' [',NodePathAsString(ProcNode),'] Src={',Src,'}']);

  aForm:=TAissistFPDocEditDlg.Create(nil);
  try
    aClient:=CreateLLMClient;
    if aForm.Describe(aClient,Src,NewTxt) then
    begin
      Params.Selection:=NewTxt;
      Params.Success:=true;
    end;
  finally
    aForm.Free;
  end;
end;

function TAIssistController.Configured: Boolean;
begin
  Result:=(Settings.BaseURL<>'');
  Result:=Result and ((Settings.Protocol<>'') and (TLLMClient.FindProtocolClass(Settings.Protocol)<>Nil));
  Result:=Result and (Settings.DefaultModel<>'');
  Result:=Result and (Settings.DefaultMaxLength>500);
end;

end.

