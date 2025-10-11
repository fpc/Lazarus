unit reglazllm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpwebclient, fphttpwebclient, llm.Client, PropEdits;

type

  { TProtocolEditor }

  TProtocolEditor = class(TStringPropertyEditor)
    procedure GetValues(Proc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure register;

implementation

uses LResources, llm.claude,llm.chatgpt,llm.gemini,llm.ollama;

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

