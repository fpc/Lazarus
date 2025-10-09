{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2024 by Michael Van Canneyt

    AI Server - JAN AI server protocol implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit JanAIProtocol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, fpwebclient, AIClient, JanAI_V1;

type

  { TJanAIServerProtocol }

  TJanAIServerProtocol = Class(TAIProtocol)
  public
    procedure AuthenticateRequest(const aRequest: TWebClientRequest; const aKey : String); override;
    function CreatePromptRequest(const aPrompt: string; aMaxResponseLength: Cardinal): TJSONData; override;
    function ResponseToPromptResponses(aResponse: TJSONData; out Responses: TPromptResponseArray): boolean; override;
    function ResponseToModels(aResponse: TJSONData; out Models: TModelDataArray): boolean; override;
    function GetAIURL(aURL: TAIURL): String; override;
    class function ProtocolName : string; override;
    class function DefaultURL : String; override;
  end;

implementation

{ TJanAIServerProtocol }

class function TJanAIServerProtocol.ProtocolName: string;
begin
  Result:='JanAI';
end;

class function TJanAIServerProtocol.DefaultURL: String;
begin
  Result:='http://localhost:1337/v1/';
end;

function TJanAIServerProtocol.ResponseToModels(aResponse: TJSONData; out Models: TModelDataArray): boolean;

var
  Response : TModelsListResponse;
  Item : TDataItem;
  Idx : Integer;


begin
  Models:=[];
  Response:=TModelsListResponse.CreateFromJSON(aResponse);
  try
    SetLength(Models,Length(Response.data));
    Idx:=0;
    For Item in Response.data do
      begin
      Models[Idx].Name:=Item.Name;
      Models[Idx].ID:=Item.id;
      Inc(Idx);
      end;
    Result:=True;
  finally
    Response.Free;
  end;
end;

function TJanAIServerProtocol.GetAIURL(aURL: TAIURL): String;
begin
  case aURL of
    auListModels : Result:='models';
    auPrompt : Result:='chat/completions';
  end;
  Result:=BaseURL+Result;
end;

procedure TJanAIServerProtocol.AuthenticateRequest(const aRequest: TWebClientRequest; const aKey: String);
begin
  inherited AuthenticateRequest(aRequest, aKey);
  Writeln('Adding key',aKey);
  if aKey<>'' then
    aRequest.Headers.Add('Authorization: Bearer '+aKey);
end;

function TJanAIServerProtocol.CreatePromptRequest(const aPrompt: string; aMaxResponseLength: Cardinal): TJSONData;

var
  Prompt : TCompletionRequest;
  Msgs : TMessages;
  Item : TMessageItem;

begin
  Prompt:=TCompletionRequest.CreateFromJSON(Nil);
  try
    Msgs:=[];
    SetLength(Msgs,1);
    Item:=TMessageItem.Create;
    Item.content:=aPrompt;
    Item.Role:='user';
    Msgs[0]:=Item;
    Prompt.messages:=Msgs;
    Prompt.max_tokens:=aMaxResponseLength;
    Prompt.model:=Self.Model;
    Result:=Prompt.SaveToJSON;
  finally
    Prompt.Free;
  end;
end;

function TJanAIServerProtocol.ResponseToPromptResponses(aResponse: TJSONData; out Responses: TPromptResponseArray): boolean;

var
  Resp : TCompletionsResponse;
  Item : TchoicesItem;
  Idx : Integer;

begin
  Responses:=[];
  Resp:=TCompletionsResponse.CreateFromJSON(aResponse);
  SetLength(Responses,Length(Resp.choices));
  Idx:=0;
  For Item in Resp.Choices do
    Responses[Idx].Response:=Item.message.content;
  Result:=True;
end;

initialization
  RegisterAIProtocol(TJANAIServerProtocol);
end.

