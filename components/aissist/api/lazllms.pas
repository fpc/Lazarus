{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazllms;

{$warn 5023 off : no warning about unused units}
interface

uses
 claude.Dto, claude.Serializer, claude.Service.Impl, claude.Service.Intf, 
 chatgpt.Dto, chatgpt.Serializer, chatgpt.Service.Impl, chatgpt.Service.Intf, 
 ollama.Dto, ollama.Serializer, ollama.Service.Impl, ollama.Service.Intf, 
 Gemini.Dto, Gemini.Persister, Gemini.Service.Impl, Gemini.Service.Intf, 
 LLM.Client, llm.ollama, llm.claude, llm.chatgpt, llm.gemini, reglazllm, 
 perplexity.Serializer, perplexity.Service.Impl, perplexity.Service.Intf, 
 perplexity.Dto, llm.perplexity, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('reglazllm', @reglazllm.Register);
end;

initialization
  RegisterPackage('lazllms', @Register);
end.
