{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2024 by Michael Van Canneyt

    JAN AI server API implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit JanAI_V1;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, fpJSON;

Type

  { -----------------------------------------------------------------------
    TMessageItem
    -----------------------------------------------------------------------}

  TMessageItem = class(TObject)
  Private
    Fcontent : String;
    Frole : String;
  Public
    Constructor CreateFromJSON(AJSON : TJSONData); virtual;
    Procedure LoadFromJSON(AJSON : TJSONData); virtual;
    Function SaveToJSON : TJSONObject; overload;
    Procedure SaveToJSON(AJSON : TJSONObject); overload; virtual;
    Property content : String Read Fcontent Write Fcontent;
    Property role : String Read Frole Write Frole;
  end;

  Tmessages = Array of TMessageItem;

Procedure ClearArray(var anArray : Tmessages); overload;
Function CreateTmessages(AJSON : TJSONData) : Tmessages;
Procedure SaveTmessagesToJSON(AnArray : Tmessages; AJSONArray : TJSONArray); overload;
Function SaveTmessagesToJSON(AnArray : Tmessages) : TJSONArray; overload;


Type
  Tstop = Array of String;

Function CreateTstop(AJSON : TJSONData) : Tstop;
Procedure SaveTstopToJSON(AnArray : Tstop; AJSONArray : TJSONArray); overload;
Function SaveTstopToJSON(AnArray : Tstop) : TJSONArray; overload;


Type


  { -----------------------------------------------------------------------
    TCompletionRequest
    -----------------------------------------------------------------------}

  TCompletionRequest = class(TObject)
  Private
    Fmessages : Tmessages;
    Fmodel : String;
    Fstream : Boolean;
    Fmax_tokens : Integer;
    Fstop : Tstop;
    Ffrequency_penalty : Integer;
    Fpresence_penalty : Integer;
    Ftemperature : Double;
    Ftop_p : Double;
  Public
    Destructor Destroy; override;
    Constructor CreateFromJSON(AJSON : TJSONData); virtual;
    Procedure LoadFromJSON(AJSON : TJSONData); virtual;
    Function SaveToJSON : TJSONObject; overload;
    Procedure SaveToJSON(AJSON : TJSONObject); overload; virtual;
    Property messages : Tmessages Read Fmessages Write Fmessages;
    Property model : String Read Fmodel Write Fmodel;
    Property stream : Boolean Read Fstream Write Fstream;
    Property max_tokens : Integer Read Fmax_tokens Write Fmax_tokens;
    Property stop : Tstop Read Fstop Write Fstop;
    Property frequency_penalty : Integer Read Ffrequency_penalty Write Ffrequency_penalty;
    Property presence_penalty : Integer Read Fpresence_penalty Write Fpresence_penalty;
    Property temperature : Double Read Ftemperature Write Ftemperature;
    Property top_p : Double Read Ftop_p Write Ftop_p;
  end;

  { -----------------------------------------------------------------------
    TchoicesItemmessage
    -----------------------------------------------------------------------}

  TchoicesItemmessage = class(TObject)
  Private
    Fcontent : String;
    Frole : String;
  Public
    Constructor CreateFromJSON(AJSON : TJSONData); virtual;
    Procedure LoadFromJSON(AJSON : TJSONData); virtual;
    Function SaveToJSON : TJSONObject; overload;
    Procedure SaveToJSON(AJSON : TJSONObject); overload; virtual;
    Property content : String Read Fcontent Write Fcontent;
    Property role : String Read Frole Write Frole;
  end;


  { -----------------------------------------------------------------------
    TchoicesItem
    -----------------------------------------------------------------------}

  TchoicesItem = class(TObject)
  Private
    Ffinish_reason : string;
    Findex : Integer;
    Fmessage : TchoicesItemmessage;
  Public
    Destructor Destroy; override;
    Constructor CreateFromJSON(AJSON : TJSONData); virtual;
    Procedure LoadFromJSON(AJSON : TJSONData); virtual;
    Function SaveToJSON : TJSONObject; overload;
    Procedure SaveToJSON(AJSON : TJSONObject); overload; virtual;
    Property finish_reason : string Read Ffinish_reason Write Ffinish_reason;
    Property index : Integer Read Findex Write Findex;
    Property message : TchoicesItemmessage Read Fmessage Write Fmessage;
  end;

  Tchoices = Array of TchoicesItem;

Procedure ClearArray(var anArray : Tchoices); overload;
Function CreateTchoices(AJSON : TJSONData) : Tchoices;
Procedure SaveTchoicesToJSON(AnArray : Tchoices; AJSONArray : TJSONArray); overload;
Function SaveTchoicesToJSON(AnArray : Tchoices) : TJSONArray; overload;


Type


  { -----------------------------------------------------------------------
    Tusage
    -----------------------------------------------------------------------}

  Tusage = class(TObject)
  Private
    Fcompletion_tokens : Integer;
    Fprompt_tokens : Integer;
    Ftotal_tokens : Integer;
  Public
    Constructor CreateFromJSON(AJSON : TJSONData); virtual;
    Procedure LoadFromJSON(AJSON : TJSONData); virtual;
    Function SaveToJSON : TJSONObject; overload;
    Procedure SaveToJSON(AJSON : TJSONObject); overload; virtual;
    Property completion_tokens : Integer Read Fcompletion_tokens Write Fcompletion_tokens;
    Property prompt_tokens : Integer Read Fprompt_tokens Write Fprompt_tokens;
    Property total_tokens : Integer Read Ftotal_tokens Write Ftotal_tokens;
  end;


  { -----------------------------------------------------------------------
    TCompletionsResponse
    -----------------------------------------------------------------------}

  TCompletionsResponse = class(TObject)
  Private
    Fchoices : Tchoices;
    Fcreated : Integer;
    Fid : String;
    Fmodel : String;
    F_object : String;
    Fsystem_fingerprint : String;
    Fusage : Tusage;
  Public
    Destructor Destroy; override;
    Constructor CreateFromJSON(AJSON : TJSONData); virtual;
    Procedure LoadFromJSON(AJSON : TJSONData); virtual;
    Function SaveToJSON : TJSONObject; overload;
    Procedure SaveToJSON(AJSON : TJSONObject); overload; virtual;
    Property choices : Tchoices Read Fchoices Write Fchoices;
    Property created : Integer Read Fcreated Write Fcreated;
    Property id : String Read Fid Write Fid;
    Property model : String Read Fmodel Write Fmodel;
    Property _object : String Read F_object Write F_object;
    Property system_fingerprint : String Read Fsystem_fingerprint Write Fsystem_fingerprint;
    Property usage : Tusage Read Fusage Write Fusage;
  end;



  { -----------------------------------------------------------------------
    TdataItemsettings
    -----------------------------------------------------------------------}

  TdataItemsettings = class(TObject)
  Private
    Fctx_len : Integer;
    Fprompt_template : String;
  Public
    Constructor CreateFromJSON(AJSON : TJSONData); virtual;
    Procedure LoadFromJSON(AJSON : TJSONData); virtual;
    Property ctx_len : Integer Read Fctx_len Write Fctx_len;
    Property prompt_template : String Read Fprompt_template Write Fprompt_template;
  end;

  TdataItemparametersstop = Array of String;

Function CreateTdataItemparametersstop(AJSON : TJSONData) : TdataItemparametersstop;


Type


  { -----------------------------------------------------------------------
    TdataItemparameters
    -----------------------------------------------------------------------}

  TdataItemparameters = class(TObject)
  Private
    Ftemperature : Double;
    Ftop_p : Double;
    Fstream : Boolean;
    Fmax_tokens : Integer;
    Fstop : TdataItemparametersstop;
    Ffrequency_penalty : Integer;
    Fpresence_penalty : Integer;
  Public
    Constructor CreateFromJSON(AJSON : TJSONData); virtual;
    Procedure LoadFromJSON(AJSON : TJSONData); virtual;
    Property temperature : Double Read Ftemperature Write Ftemperature;
    Property top_p : Double Read Ftop_p Write Ftop_p;
    Property stream : Boolean Read Fstream Write Fstream;
    Property max_tokens : Integer Read Fmax_tokens Write Fmax_tokens;
    Property stop : TdataItemparametersstop Read Fstop Write Fstop;
    Property frequency_penalty : Integer Read Ffrequency_penalty Write Ffrequency_penalty;
    Property presence_penalty : Integer Read Fpresence_penalty Write Fpresence_penalty;
  end;


  { -----------------------------------------------------------------------
    TDataItem
    -----------------------------------------------------------------------}

  TDataItem = class(TObject)
  Private
    Fsource_url : String;
    Fid : String;
    F_object : String;
    Fname : String;
    Fversion : String;
    Fdescription : String;
    Fformat : String;
    Fsettings : TDataItemsettings;
    Fparameters : TDataItemparameters;
    Fmetadata : String;
    Fengine : String;
  Public
    Destructor Destroy; override;
    Constructor CreateFromJSON(AJSON : TJSONData); virtual;
    Procedure LoadFromJSON(AJSON : TJSONData); virtual;
    Property source_url : String Read Fsource_url Write Fsource_url;
    Property id : String Read Fid Write Fid;
    Property _object : String Read F_object Write F_object;
    Property name : String Read Fname Write Fname;
    Property version : String Read Fversion Write Fversion;
    Property description : String Read Fdescription Write Fdescription;
    Property format : String Read Fformat Write Fformat;
    Property settings : TDataItemsettings Read Fsettings Write Fsettings;
    Property parameters : TDataItemparameters Read Fparameters Write Fparameters;
    Property metadata : String Read Fmetadata Write Fmetadata;
    Property engine : String Read Fengine Write Fengine;
  end;

  Tdata = Array of TDataItem;

Procedure ClearArray(var anArray : Tdata); overload;
Function CreateTdata(AJSON : TJSONData) : Tdata;


Type


  { -----------------------------------------------------------------------
    TModelsListResponse
    -----------------------------------------------------------------------}

  TModelsListResponse = class(TObject)
  Private
    F_object : String;
    Fdata : Tdata;
  Public
    Destructor Destroy; override;
    Constructor CreateFromJSON(AJSON : TJSONData); virtual;
    Procedure LoadFromJSON(AJSON : TJSONData); virtual;
    Property _object : String Read F_object Write F_object;
    Property data : Tdata Read Fdata Write Fdata;
  end;



implementation


{ -----------------------------------------------------------------------
  TMessageItem
  -----------------------------------------------------------------------}


Constructor TMessageItem.CreateFromJSON(AJSON : TJSONData);

begin
  Create();
  LoadFromJSON(AJSON);
end;

Procedure TMessageItem.LoadFromJSON(AJSON : TJSONData);

var
  E : TJSONEnum;

begin
  for E in AJSON do
    begin
    case E.Key of
    'content':
      content:=E.Value.AsString;
    'role':
      role:=E.Value.AsString;
    end;
    end;
end;
Function  TMessageItem.SaveToJSON : TJSONObject;
begin
  Result:=TJSONObject.Create;
  Try
    SaveToJSON(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;


Procedure TMessageItem.SaveToJSON(AJSON : TJSONObject);

begin
  AJSON.Add('content',content);
  AJSON.Add('role',role);
end;


Procedure ClearArray(Var anArray : Tmessages);

var
  I : integer;

begin
  For I:=0 to Length(anArray)-1 do
    FreeAndNil(anArray[I]);
  SetLength(anArray,0);
End;


Function CreateTmessages(AJSON : TJSONData) : Tmessages;

var
  I : integer;

begin
  Result:=[];
  SetLength(Result,AJSON.Count);
  For I:=0 to AJSON.Count-1 do
    Result[i]:=TMessageItem.CreateFromJSON(AJSON.Items[i]);
End;


Function SaveTmessagesToJSON(AnArray : Tmessages) : TJSONArray;
begin
  Result:=TJSONArray.Create;
  Try
    SaveTmessagesToJSON(AnArray,Result);
  Except
    FreeAndNil(Result);
    Raise;
  end;
end;


Procedure SaveTmessagesToJSON(AnArray : Tmessages; AJSONArray : TJSONArray);

var
  I : integer;

begin
  For I:=0 to Length(AnArray)-1 do
    AJSONArray.Add(AnArray[i].SaveToJSON);
end;




Function CreateTstop(AJSON : TJSONData) : Tstop;

var
  I : integer;

begin
  Result:=[];
  SetLength(Result,AJSON.Count);
  For I:=0 to AJSON.Count-1 do
    Result[i]:=AJSON.Items[i].AsString;
End;


Function SaveTstopToJSON(AnArray : Tstop) : TJSONArray;
begin
  Result:=TJSONArray.Create;
  Try
    SaveTstopToJSON(AnArray,Result);
  Except
    FreeAndNil(Result);
    Raise;
  end;
end;


Procedure SaveTstopToJSON(AnArray : Tstop; AJSONArray : TJSONArray);

var
  I : integer;

begin
  For I:=0 to Length(AnArray)-1 do
    AJSONArray.Add(AnArray[i]);
end;



{ -----------------------------------------------------------------------
  TCompletionRequest
  -----------------------------------------------------------------------}

Destructor TCompletionRequest.Destroy;

begin
  ClearArray(Fmessages);

  inherited;
end;


Constructor TCompletionRequest.CreateFromJSON(AJSON : TJSONData);

begin
  Create();
  if assigned(aJSON) then
    LoadFromJSON(AJSON);
end;

Procedure TCompletionRequest.LoadFromJSON(AJSON : TJSONData);

var
  E : TJSONEnum;

begin
  for E in AJSON do
    begin
    case E.Key of
    'messages':
      messages:=CreateTmessages(E.Value);
    'model':
      model:=E.Value.AsString;
    'stream':
      stream:=E.Value.AsBoolean;
    'max_tokens':
      max_tokens:=E.Value.AsInteger;
    'stop':
      stop:=CreateTstop(E.Value);
    'frequency_penalty':
      frequency_penalty:=E.Value.AsInteger;
    'presence_penalty':
      presence_penalty:=E.Value.AsInteger;
    'temperature':
      temperature:=E.Value.AsFloat;
    'top_p':
      top_p:=E.Value.AsFloat;
    end;
    end;
end;
Function  TCompletionRequest.SaveToJSON : TJSONObject;
begin
  Result:=TJSONObject.Create;
  Try
    SaveToJSON(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;


Procedure TCompletionRequest.SaveToJSON(AJSON : TJSONObject);

begin
  AJSON.Add('messages',SaveTmessagesToJSON(messages));
  AJSON.Add('model',model);
  AJSON.Add('stream',stream);
  AJSON.Add('max_tokens',max_tokens);
  AJSON.Add('stop',SaveTstopToJSON(stop));
  AJSON.Add('frequency_penalty',frequency_penalty);
  AJSON.Add('presence_penalty',presence_penalty);
  AJSON.Add('temperature',temperature);
  AJSON.Add('top_p',top_p);
end;

{ -----------------------------------------------------------------------
  TchoicesItemmessage
  -----------------------------------------------------------------------}


Constructor TchoicesItemmessage.CreateFromJSON(AJSON : TJSONData);

begin
  Create();
  LoadFromJSON(AJSON);
end;

Procedure TchoicesItemmessage.LoadFromJSON(AJSON : TJSONData);

var
  E : TJSONEnum;

begin
  for E in AJSON do
    begin
    case E.Key of
    'content':
      content:=E.Value.AsString;
    'role':
      role:=E.Value.AsString;
    end;
    end;
end;
Function  TchoicesItemmessage.SaveToJSON : TJSONObject;
begin
  Result:=TJSONObject.Create;
  Try
    SaveToJSON(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;


Procedure TchoicesItemmessage.SaveToJSON(AJSON : TJSONObject);

begin
  AJSON.Add('content',content);
  AJSON.Add('role',role);
end;


{ -----------------------------------------------------------------------
  TchoicesItem
  -----------------------------------------------------------------------}

Destructor TchoicesItem.Destroy;

begin
  FreeAndNil(Fmessage);
  inherited;
end;


Constructor TchoicesItem.CreateFromJSON(AJSON : TJSONData);

begin
  Create();
  LoadFromJSON(AJSON);
end;

Procedure TchoicesItem.LoadFromJSON(AJSON : TJSONData);

var
  E : TJSONEnum;

begin
  for E in AJSON do
    begin
    if E.Value.JSONType=jtNull then
      continue;
    case E.Key of
    'finish_reason':
      finish_reason:=E.Value.AsString;
    'index':
      index:=E.Value.AsInteger;
    'message':
      message:=TchoicesItemmessage.CreateFromJSON(E.Value);
    end;
    end;
end;
Function  TchoicesItem.SaveToJSON : TJSONObject;
begin
  Result:=TJSONObject.Create;
  Try
    SaveToJSON(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;


Procedure TchoicesItem.SaveToJSON(AJSON : TJSONObject);

begin
  AJSON.Add('finish_reason',finish_reason);
  AJSON.Add('index',index);
  If Assigned(message) then
    AJSON.Add('message',message.SaveToJSON);
end;


Procedure ClearArray(Var anArray : Tchoices);

var
  I : integer;

begin
  For I:=0 to Length(anArray)-1 do
    FreeAndNil(anArray[I]);
  SetLength(anArray,0);
End;


Function CreateTchoices(AJSON : TJSONData) : Tchoices;

var
  I : integer;

begin
  Result:=[];
  SetLength(Result,AJSON.Count);
  For I:=0 to AJSON.Count-1 do
    Result[i]:=TchoicesItem.CreateFromJSON(AJSON.Items[i]);
End;


Function SaveTchoicesToJSON(AnArray : Tchoices) : TJSONArray;
begin
  Result:=TJSONArray.Create;
  Try
    SaveTchoicesToJSON(AnArray,Result);
  Except
    FreeAndNil(Result);
    Raise;
  end;
end;


Procedure SaveTchoicesToJSON(AnArray : Tchoices; AJSONArray : TJSONArray);

var
  I : integer;

begin
  For I:=0 to Length(AnArray)-1 do
    AJSONArray.Add(AnArray[i].SaveToJSON);
end;




{ -----------------------------------------------------------------------
  Tusage
  -----------------------------------------------------------------------}


Constructor Tusage.CreateFromJSON(AJSON : TJSONData);

begin
  Create();
  LoadFromJSON(AJSON);
end;

Procedure Tusage.LoadFromJSON(AJSON : TJSONData);

var
  E : TJSONEnum;

begin
  for E in AJSON do
    begin
    case E.Key of
    'completion_tokens':
      completion_tokens:=E.Value.AsInteger;
    'prompt_tokens':
      prompt_tokens:=E.Value.AsInteger;
    'total_tokens':
      total_tokens:=E.Value.AsInteger;
    end;
    end;
end;
Function  Tusage.SaveToJSON : TJSONObject;
begin
  Result:=TJSONObject.Create;
  Try
    SaveToJSON(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;


Procedure Tusage.SaveToJSON(AJSON : TJSONObject);

begin
  AJSON.Add('completion_tokens',completion_tokens);
  AJSON.Add('prompt_tokens',prompt_tokens);
  AJSON.Add('total_tokens',total_tokens);
end;


{ -----------------------------------------------------------------------
  TCompletionsResponse
  -----------------------------------------------------------------------}

Destructor TCompletionsResponse.Destroy;

begin
  ClearArray(Fchoices);
  FreeAndNil(Fusage);
  inherited;
end;


Constructor TCompletionsResponse.CreateFromJSON(AJSON : TJSONData);

begin
  Create();
  LoadFromJSON(AJSON);
end;

Procedure TCompletionsResponse.LoadFromJSON(AJSON : TJSONData);

var
  E : TJSONEnum;

begin
  for E in AJSON do
    begin
    case E.Key of
    'choices':
      choices:=CreateTchoices(E.Value);
    'created':
      created:=E.Value.AsInteger;
    'id':
      id:=E.Value.AsString;
    'model':
      model:=E.Value.AsString;
    'object':
      _object:=E.Value.AsString;
    'system_fingerprint':
      system_fingerprint:=E.Value.AsString;
    'usage':
      usage:=Tusage.CreateFromJSON(E.Value);
    end;
    end;
end;

Function  TCompletionsResponse.SaveToJSON : TJSONObject;
begin
  Result:=TJSONObject.Create;
  Try
    SaveToJSON(Result);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;


Procedure TCompletionsResponse.SaveToJSON(AJSON : TJSONObject);

begin
  AJSON.Add('choices',SaveTchoicesToJSON(choices));
  AJSON.Add('created',created);
  AJSON.Add('id',id);
  AJSON.Add('model',model);
  AJSON.Add('object',_object);
  AJSON.Add('system_fingerprint',system_fingerprint);
  If Assigned(usage) then
    AJSON.Add('usage',usage.SaveToJSON);
end;




{ -----------------------------------------------------------------------
  TDataItemsettings
  -----------------------------------------------------------------------}


Constructor TDataItemsettings.CreateFromJSON(AJSON : TJSONData);

begin
  Create();
  LoadFromJSON(AJSON);
end;

Procedure TDataItemsettings.LoadFromJSON(AJSON : TJSONData);

var
  E : TJSONEnum;

begin
  for E in AJSON do
    begin
    case E.Key of
    'ctx_len':
      ctx_len:=E.Value.AsInteger;
    'prompt_template':
      prompt_template:=E.Value.AsString;
    end;
    end;
end;




Function CreateTDataItemparametersstop(AJSON : TJSONData) : TDataItemparametersstop;

var
  I : integer;

begin
  Result:=[];
  SetLength(Result,AJSON.Count);
  For I:=0 to AJSON.Count-1 do
    Result[i]:=AJSON.Items[i].AsString;
End;



{ -----------------------------------------------------------------------
  TDataItemparameters
  -----------------------------------------------------------------------}


Constructor TDataItemparameters.CreateFromJSON(AJSON : TJSONData);

begin
  Create();
  LoadFromJSON(AJSON);
end;

Procedure TDataItemparameters.LoadFromJSON(AJSON : TJSONData);

var
  E : TJSONEnum;

begin
  for E in AJSON do
    begin
    case E.Key of
    'temperature':
      temperature:=E.Value.AsFloat;
    'top_p':
      top_p:=E.Value.AsFloat;
    'stream':
      stream:=E.Value.AsBoolean;
    'max_tokens':
      max_tokens:=E.Value.AsInteger;
    'stop':
      stop:=CreateTDataItemparametersstop(E.Value);
    'frequency_penalty':
      frequency_penalty:=E.Value.AsInteger;
    'presence_penalty':
      presence_penalty:=E.Value.AsInteger;
    end;
    end;
end;


{ -----------------------------------------------------------------------
  TDataItem
  -----------------------------------------------------------------------}

Destructor TDataItem.Destroy;

begin
  FreeAndNil(Fsettings);
  FreeAndNil(Fparameters);
  inherited;
end;


Constructor TDataItem.CreateFromJSON(AJSON : TJSONData);

begin
  Create();
  LoadFromJSON(AJSON);
end;

Procedure TDataItem.LoadFromJSON(AJSON : TJSONData);

var
  E : TJSONEnum;

begin
  for E in AJSON do
    begin
    case E.Key of
    'source_url':
      source_url:=E.Value.AsString;
    'id':
      id:=E.Value.AsString;
    'object':
      _object:=E.Value.AsString;
    'name':
      name:=E.Value.AsString;
    'version':
      version:=E.Value.AsString;
    'description':
      description:=E.Value.AsString;
    'format':
      format:=E.Value.AsString;
    'settings':
      settings:=TDataItemsettings.CreateFromJSON(E.Value);
    'parameters':
      parameters:=TDataItemparameters.CreateFromJSON(E.Value);
    'metadata':
      metadata:=E.Value.AsJSON;
    'engine':
      engine:=E.Value.AsString;
    end;
    end;
end;


Procedure ClearArray(Var anArray : Tdata);

var
  I : integer;

begin
  For I:=0 to Length(anArray)-1 do
    FreeAndNil(anArray[I]);
  SetLength(anArray,0);
End;


Function CreateTdata(AJSON : TJSONData) : Tdata;

var
  I : integer;

begin
  Result:=[];
  SetLength(Result,AJSON.Count);
  For I:=0 to AJSON.Count-1 do
    Result[i]:=TDataItem.CreateFromJSON(AJSON.Items[i]);
End;



{ -----------------------------------------------------------------------
  TModelsListResponse
  -----------------------------------------------------------------------}

Destructor TModelsListResponse.Destroy;

begin
  ClearArray(Fdata);
  inherited;
end;


Constructor TModelsListResponse.CreateFromJSON(AJSON : TJSONData);

begin
  Create();
  LoadFromJSON(AJSON);
end;

Procedure TModelsListResponse.LoadFromJSON(AJSON : TJSONData);

var
  E : TJSONEnum;

begin
  for E in AJSON do
    begin
    case E.Key of
    'object':
      _object:=E.Value.AsString;
    'data':
      data:=CreateTdata(E.Value);
    end;
    end;
end;

end.
