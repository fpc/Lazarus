unit reglazllm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebclient, fphttpwebclient, llm.Client, FormEditingIntf, PropEdits;

type

  { TProtocolEditor }

  TProtocolEditor = class(TStringPropertyEditor)
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
    procedure SetValue(const NewValue: ansistring); override;
  private
    procedure AddUnitToFormUses(const aUnitName: string);
  end;

procedure register;

implementation

uses
  LResources, llm.claude,llm.chatgpt,llm.gemini,llm.ollama,
  IDEMsgIntf,
  LazIDEIntf, IDEExternToolIntf, ComponentEditors, ProjectIntf, CodeCache, CodeToolManager;

resourcestring
  SErrFailedToAddUnit = 'Failed to add unit %s to uses clause: %s';

procedure register;
begin
  if DefaultWebClientClass=Nil then
    DefaultWebClientClass:=TFPHTTPWebClient;
  RegisterComponents('AI',[TLLMClient]);
  RegisterPropertyEditor(TypeInfo(String),TLLMServerSettings,'Protocol',TProtocolEditor);
end;

{ TProtocolEditor }

function TProtocolEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paValueList, paRevertable];
end;

procedure TProtocolEditor.AddUnitToFormUses(const aUnitName : string);
var
  lLookupRoot: TPersistent;
  lRoot: TComponent absolute lLookupRoot;
  lDesigner: TComponentEditorDesigner;
  lFile: TLazProjectFile;
  Code: TCodeBuffer;
  lMsg : String;
begin
  lLookupRoot:=PropertyHook.LookupRoot;
  if not (lLookupRoot is TComponent) then
    exit;
  lDesigner := TComponentEditorDesigner(FormEditingHook.GetDesignerByComponent(lRoot));
  if lDesigner=nil then
    exit;
  // get unit. Requires a recent lazarus.
  lFile:=lDesigner.ProjectFile;
  if lFile=nil then
    exit;
  Code:=CodeToolBoss.FindFile(lFile.Filename);
  // save changes in source editor to codetools
  LazarusIDE.SaveSourceEditorChangesToCodeCache(Nil); // -1: commit all source editors
  // add unit to interface uses
  if CodeToolBoss.AddUnitToMainUsesSectionIfNeeded(Code, aUnitName, '') then
    begin
    lMsg:=CodeToolBoss.ErrorMessage;
    if lMsg<>'' then
      begin
      lMsg:=Format(SErrFailedToAddUNit,[aUnitName,lMsg]);
      AddIDEMessage(mluError,lMsg,lFile.Filename);
      end;
    end;
end;

procedure TProtocolEditor.SetValue(const NewValue: ansistring);
var
  C : TLLMProtocolClass;
begin
  inherited SetValue(NewValue);
  C:=TCustomLLMClient.FindProtocolClass(NewValue);
  if assigned(C) then
    AddUnitToFormUses(C.UnitName);
end;

procedure TProtocolEditor.GetValues(Proc: TGetStrProc);
var
  L : TstringList;
  S : String;
begin
  L:=TstringList.Create;
  try
    TCustomLLMClient.GetProtocolList(L);
    L.Sort;
    for S in L do
      Proc(S);
  finally
    L.Free;
  end;
end;

initialization
  {$i llmclient}
end.

