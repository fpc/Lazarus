{
 /***************************************************************************
                              ideinstances.pas
                              ----------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Ondrej Pokorny

  Abstract:
    This unit handles one/multiple Lazarus IDE instances.

}
unit IDEInstances;

{$mode objfpc}{$H+}

interface

uses
  sysutils, Interfaces, Classes, Controls, Forms, Dialogs, ExtCtrls,
  LCLProc, LCLIntf, LCLType, AdvancedIPC,
  LazFileUtils, LazUTF8, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite,
  LazarusIDEStrConsts, IDECmdLine;

type
  TStartNewInstanceResult = (ofrStartNewInstance, ofrDoNotStart, ofrModalError, ofrForceSingleInstanceModalError, ofrNotResponding);
  TStartNewInstanceEvent = procedure(const aFiles: TStrings;
    var outResult: TStartNewInstanceResult) of object;

  TMessageParam = record
    Name: string;
    Value: string;
  end;
  TMessageParams = array of TMessageParam;

  TUniqueServer = class(TIPCServer)
  public
    procedure StartUnique(const aServerPrefix: string);
  end;

  TMainServer = class(TUniqueServer)
  private
    FStartNewInstanceEvent: TStartNewInstanceEvent;
    FTimer: TTimer;
    FMsgStream: TMemoryStream;

    procedure DoStartNewInstance(const aMsgID: Integer; const aInParams: TMessageParams);

    procedure SimpleResponse(const aResponseToMsgID: Integer;
      const aResponseType: string; const aParams: array of TMessageParam);

    procedure DoCheckMessages;
    procedure CheckMessagesOnTimer(Sender: TObject);

    procedure StartListening(const aStartNewInstanceEvent: TStartNewInstanceEvent);
    procedure StopListening;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TResponseClient = class(TIPCClient)
  public
    function AllowStartNewInstance(
      const aFiles: TStrings; var outModalErrorMessage,
      outModalErrorForceUniqueMessage, outNotRespondingErrorMessage: string;
      var outHandleBringToFront: HWND): TStartNewInstanceResult;
  end;

  TIDEInstances = class(TComponent)
  private
    FMainServer: TMainServer;//running IDE
    FStartIDE: Boolean;// = True;
    FForceNewInstance: Boolean;
    FAllowOpenLastProject: Boolean;// = True;
    FFilesToOpen: TStrings;

    class procedure AddFilesToParams(const aFiles: TStrings;
      var ioParams: TMessageParams); static;
    class procedure AddFilesFromParams(const aParams: TMessageParams;
      const aFiles: TStrings); static;
    class procedure BuildMessage(const aMessageType: string;
      const aParams: array of TMessageParam; const aStream: TStream); static;
    class function MessageParam(const aName, aValue: string): TMessageParam; static;
    class function ParseMessage(const aStream: TStream; out outMessageType: string;
      out outParams: TMessageParams): Boolean; static;
    class function GetMessageParam(const aParams: array of TMessageParam;
      const aParamName: string): string; static;

    function CheckParamsForForceNewInstanceOpt: Boolean;

    procedure CollectFiles(out
      outFilesWereSentToCollectingServer: Boolean);

    function AllowStartNewInstance(const aFiles: TStrings;
      var outModalErrorMessage, outModalErrorForceUniqueMessage, outNotRespondingErrorMessage: string;
      var outHandleBringToFront: HWND): TStartNewInstanceResult;
    procedure InitIDEInstances;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure PerformCheck;//call PerformCheck after Application.Initialize - it can open dialogs!

    procedure StartServer;
    procedure StopServer;
    procedure StartListening(const aStartNewInstanceEvent: TStartNewInstanceEvent);
    procedure StopListening;

    function StartIDE: Boolean;//can the IDE be started?
    function AllowOpenLastProject: Boolean;//if a secondary IDE is starting, do NOT reopen last project!
    function FilesToOpen: TStrings;
  end;

function LazIDEInstances: TIDEInstances;

implementation

const
  SERVERPREFIX_MAIN = 'LazarusMain';
  SERVERNAME_COLLECT = 'LazarusCollect';
  MESSAGETYPE_XML = 2;
  ELEMENT_ROOT = 'ideinstances';
  ATTR_VALUE = 'value';
  ATTR_MESSAGE_TYPE = 'msgtype';
  MESSAGE_STARTNEWINSTANCE = 'startnewinstance';
  RESPONSE_OPENFILES = 'openfilesResponse';
  TIMEOUT_OPENFILES = 1000;
  MESSAGE_COLLECTFILES = 'collectfiles';
  TIMEOUT_COLLECTFILES = 100;
  PARAM_FILE = 'file';
  PARAM_RESULT = 'result';
  PARAM_HANDLEBRINGTOFRONT = 'handlebringtofront';
  PARAM_MODALERRORMESSAGE = 'modalerrormessage';
  PARAM_FORCEUNIQUEMODALERRORMESSAGE = 'forceuniquemodalerrormessage';
  PARAM_NOTRESPONDINGERRORMESSAGE = 'notrespondingerrormessage';

var
  FLazIDEInstances: TIDEInstances;

function LazIDEInstances: TIDEInstances;
begin
  Result := FLazIDEInstances;
end;

{ TIDEInstances }

class function TIDEInstances.MessageParam(const aName, aValue: string): TMessageParam;
begin
  Result.Name := aName;
  Result.Value := aValue;
end;

function TIDEInstances.StartIDE: Boolean;
begin
  Result := FStartIDE;
end;

function TIDEInstances.AllowOpenLastProject: Boolean;
begin
  Result := FAllowOpenLastProject;
end;

function TIDEInstances.FilesToOpen: TStrings;
begin
  if not Assigned(FFilesToOpen) then
    FFilesToOpen := TStringList.Create;
  Result := FFilesToOpen;
end;

procedure TIDEInstances.StartListening(const aStartNewInstanceEvent: TStartNewInstanceEvent);
begin
  Assert(Assigned(FMainServer));

  FMainServer.StartListening(aStartNewInstanceEvent);
end;

procedure TIDEInstances.StartServer;
begin
  Assert(FMainServer = nil);

  FMainServer := TMainServer.Create(Self);
  FMainServer.StartUnique(SERVERPREFIX_MAIN);
end;

procedure TIDEInstances.StopListening;
begin
  FMainServer.StopListening;
end;

procedure TIDEInstances.StopServer;
begin
  FreeAndNil(FMainServer);
end;

class procedure TIDEInstances.AddFilesFromParams(const aParams: TMessageParams;
  const aFiles: TStrings);
var
  I: Integer;
begin
  //do not clear aFiles
  for I := Low(aParams) to High(aParams) do
    if aParams[I].Name = PARAM_FILE then
      aFiles.Add(aParams[I].Value);
end;

class procedure TIDEInstances.AddFilesToParams(const aFiles: TStrings;
  var ioParams: TMessageParams);
var
  xStartIndex: Integer;
  I: Integer;
begin
  xStartIndex := Length(ioParams);
  SetLength(ioParams, xStartIndex+aFiles.Count);
  for I := 0 to aFiles.Count-1 do
    ioParams[xStartIndex+I] := MessageParam(PARAM_FILE, aFiles[I]);
end;

class function TIDEInstances.GetMessageParam(
  const aParams: array of TMessageParam; const aParamName: string): string;
var
  I: Integer;
begin
  for I := Low(aParams) to High(aParams) do
  if aParams[I].Name = aParamName then
    Exit(aParams[I].Value);

  Result := '';//not found
end;

class procedure TIDEInstances.BuildMessage(const aMessageType: string;
  const aParams: array of TMessageParam; const aStream: TStream);
var
  xDOM: TXMLDocument;
  xRoot: TDOMElement;
  xParam: TDOMElement;
  I: Integer;
begin
  xDOM := TXMLDocument.Create;
  try
    xRoot := xDOM.CreateElement(ELEMENT_ROOT);
    xRoot.AttribStrings[ATTR_MESSAGE_TYPE] := aMessageType;
    xDOM.AppendChild(xRoot);

    for I := Low(aParams) to High(aParams) do
    begin
      xParam := xDOM.CreateElement(aParams[I].Name);
      xRoot.AppendChild(xParam);
      xParam.AttribStrings[ATTR_VALUE] := aParams[I].Value;
    end;

    WriteXMLFile(xDOM, aStream);
  finally
    xDOM.Free;
  end;
end;

class function TIDEInstances.ParseMessage(const aStream: TStream; out
  outMessageType: string; out outParams: TMessageParams): Boolean;
var
  xDOM: TXMLDocument;
  xChildList: TDOMNodeList;
  I, J: Integer;
begin
  Result := False;

  outMessageType := '';
  SetLength(outParams, 0);
  try
    ReadXMLFile(xDOM, aStream, []);
  except
    on EXMLReadError do
      Exit;//eat XML exceptions
  end;
  try
    if (xDOM = nil) or (xDOM.DocumentElement = nil) or (xDOM.DocumentElement.NodeName <> ELEMENT_ROOT) then
      Exit;

    outMessageType := xDOM.DocumentElement.AttribStrings[ATTR_MESSAGE_TYPE];

    xChildList := xDOM.DocumentElement.ChildNodes;
    SetLength(outParams, xChildList.Count);
    J := 0;
    for I := 0 to xChildList.Count-1 do
    if xChildList[I] is TDOMElement then
    begin
      outParams[J].Name := xChildList[I].NodeName;
      outParams[J].Value := TDOMElement(xChildList[I]).AttribStrings[ATTR_VALUE];
      Inc(J);
    end;
    SetLength(outParams, J);
    Result := True;
  finally
    xDOM.Free;
  end;
end;

function TIDEInstances.AllowStartNewInstance(const aFiles: TStrings;
  var outModalErrorMessage, outModalErrorForceUniqueMessage,
  outNotRespondingErrorMessage: string; var outHandleBringToFront: HWND
  ): TStartNewInstanceResult;
var
  xStartClient: TResponseClient;
  I: Integer;
  xServerIDs: TStringList;
begin
  Result := ofrStartNewInstance;
  xStartClient := TResponseClient.Create(nil);
  xServerIDs := TStringList.Create;
  try
    xStartClient.FindRunningServers(SERVERPREFIX_MAIN, xServerIDs);//check for multiple instances
    xServerIDs.Sort;

    for I := xServerIDs.Count-1 downto 0 do//last started is first to choose
    begin
      xStartClient.ServerID := xServerIDs[I];
      if xStartClient.ServerRunning then
      begin
        //there are open Lazarus instances, do not reopen last project!
        FAllowOpenLastProject := False;
        Result := xStartClient.AllowStartNewInstance(aFiles, outModalErrorMessage,
          outModalErrorForceUniqueMessage, outNotRespondingErrorMessage, outHandleBringToFront);
        if not(Result in [ofrModalError, ofrForceSingleInstanceModalError, ofrNotResponding]) then
          Exit;//handle only one running Lazarus IDE
      end;
    end;
  finally
    xStartClient.Free;
    xServerIDs.Free;
  end;
end;

function TIDEInstances.CheckParamsForForceNewInstanceOpt: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamsAndCfgCount do
    if ParamIsOption(i, ForceNewInstanceOpt) then//ignore the settings and start new Lazarus IDE instance
      Result := True;
end;

procedure TIDEInstances.PerformCheck;
var
  xResult: TStartNewInstanceResult;
  xModalErrorMessage: string = '';
  xModalErrorForceUniqueMessage: string = '';
  xNotRespondingErrorMessage: string = '';
  xHandleBringToFront: HWND = 0;
begin
  if not FStartIDE then//InitIDEInstances->CollectOtherOpeningFiles decided not to start the IDE
    Exit;

  if not FForceNewInstance then
    xResult := AllowStartNewInstance(FilesToOpen, xModalErrorMessage, xModalErrorForceUniqueMessage, xNotRespondingErrorMessage, xHandleBringToFront)
  else
    xResult := ofrStartNewInstance;

  if xModalErrorMessage = '' then
    xModalErrorMessage := dlgRunningInstanceModalError;
  if xNotRespondingErrorMessage = '' then
    xNotRespondingErrorMessage := dlgRunningInstanceNotRespondingError;
  if xModalErrorForceUniqueMessage = '' then
    xModalErrorForceUniqueMessage := dlgForceUniqueInstanceModalError;

  FStartIDE := (xResult = ofrStartNewInstance);
  case xResult of
    ofrModalError:
      FStartIDE := MessageDlg(lisLazarusIDE, Format(xModalErrorMessage, [FilesToOpen.Text]), mtWarning, mbYesNo, 0, mbYes) = mrYes;
    ofrNotResponding:
      MessageDlg(lisLazarusIDE, xNotRespondingErrorMessage, mtError, [mbOK], 0);
    ofrForceSingleInstanceModalError:
      MessageDlg(lisLazarusIDE, xModalErrorForceUniqueMessage, mtError, [mbOK], 0);
  end;

  {$IFDEF MSWINDOWS}
  if not FStartIDE and (xHandleBringToFront <> 0) then
  begin
    try
      SetForegroundWindow(xHandleBringToFront);//SetForegroundWindow works (on Windows) only if the calling process is the foreground process, therefore it must be here!
    except
      //eat all widget exceptions
    end;
  end;
  {$ENDIF}
end;

constructor TIDEInstances.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FStartIDE := True;
  FAllowOpenLastProject := True;
end;

destructor TIDEInstances.Destroy;
begin
  StopServer;
  FreeAndNil(FMainServer);
  FreeAndNil(FFilesToOpen);

  inherited Destroy;
end;

procedure TIDEInstances.CollectFiles(out
  outFilesWereSentToCollectingServer: Boolean);

var
  xThisClientMessageId: Integer;

  procedure _SendToServer;
  var
    xClient: TIPCClient;
    xOutParams: TMessageParams;
    xStream: TMemoryStream;
  begin
    xClient := TIPCClient.Create(nil);
    try
      xClient.ServerID := SERVERNAME_COLLECT;

      SetLength(xOutParams, 0);
      AddFilesToParams(FilesToOpen, xOutParams);

      xStream := TMemoryStream.Create;
      try
        BuildMessage(MESSAGE_COLLECTFILES, xOutParams, xStream);
        xStream.Position := 0;
        xThisClientMessageId := xClient.PostRequest(MESSAGETYPE_XML, xStream);
      finally
        xStream.Free;
      end;
    finally
      xClient.Free;
    end;
  end;

  procedure _WaitForFiles;
  var
    xLastCount, xNewCount: Integer;
    xServer: TIPCServer;
  begin
    xServer := TIPCServer.Create(nil);
    try
      xServer.ServerID := SERVERNAME_COLLECT;
      //do not start server here
      xLastCount := -1;
      xNewCount := xServer.GetPendingRequestCount;
      while xLastCount <> xNewCount do
      begin
        xLastCount := xNewCount;
        Sleep(TIMEOUT_COLLECTFILES);
        xNewCount := xServer.GetPendingRequestCount;
      end;
    finally
      xServer.Free;
    end;
  end;

  function _ReceiveAsServer: Boolean;
  var
    xServer: TIPCServer;
    xInParams: TMessageParams;
    xStream: TMemoryStream;
    xMsgType: Integer;
    xMessageType: string;
  begin
    xStream := TMemoryStream.Create;
    xServer := TIPCServer.Create(nil);
    try
      xServer.ServerID := SERVERNAME_COLLECT;
      //files have to be handled only by one instance!
      Result := xServer.FindHighestPendingRequestId = xThisClientMessageId;
      if Result then
      begin
        //we are the highest client, handle the files
        xServer.StartServer(False);
      end else
      begin
        //we are not the highest client, maybe there are pending files, check that
        {$IFNDEF MSWINDOWS}
        //this code is not slowing up IDE start because if there was highest client found (the normal way), we close anyway
        Randomize;  //random sleep in order to prevent double file locks on unix
        Sleep(Random( PtrInt(($3F+GetCurrentThreadId) and $3F) ));
        {$ENDIF}
        if not (xServer.StartServer(False) and (xServer.GetPendingRequestCount > 0)) then
          Exit;//server is already running or there are no pending message -> close
        Result := True;//no one handled handled the files, do it by myself
      end;

      FilesToOpen.Clear;
      while xServer.PeekRequest(xStream, xMsgType{%H-}) do
      if xMsgType = MESSAGETYPE_XML then
      begin
        if ParseMessage(xStream, xMessageType, xInParams) and
           (xMessageType = MESSAGE_COLLECTFILES)
        then
          AddFilesFromParams(xInParams, FilesToOpen);
      end;
    finally
      xStream.Free;
      xServer.Free;
    end;
  end;
begin
  //if you select more files in explorer and open them, they are not opened in one process but one process is started per file
  // -> collect them

  //first send messages to queue (there is no server, no problem, it will collect the messages when it is created)
  _SendToServer;

  //now wait until we have everything
  _WaitForFiles;

  //now send them to one instance
  outFilesWereSentToCollectingServer := not _ReceiveAsServer;
end;

procedure TIDEInstances.InitIDEInstances;
var
  xFilesWereSentToCollectingServer: Boolean;
  I: Integer;
begin
  FForceNewInstance := CheckParamsForForceNewInstanceOpt;

  //get cmd line filenames
  FFilesToOpen := ExtractCmdLineFilenames;
  for I := 0 to FilesToOpen.Count-1 do
    FilesToOpen[I] := CleanAndExpandFilename(FilesToOpen[I]);

  if FilesToOpen.Count > 0 then//if there are file in the cmd, check for multiple starting instances
  begin
    CollectFiles(xFilesWereSentToCollectingServer);
    if xFilesWereSentToCollectingServer then
    begin
      FilesToOpen.Clear;
      FStartIDE := False;
    end;
  end;
end;

{ TUniqueServer }

procedure TUniqueServer.StartUnique(const aServerPrefix: string);
var
  I: Integer;
begin
  if Active then
    StopServer;

  I := 0;
  while not Active do
  begin
    Inc(I);
    if I < 10 then
      ServerID := aServerPrefix+'0'+IntToStr(I)
    else
      ServerID := aServerPrefix+IntToStr(I);
    StartServer;
  end;
end;

{ TResponseClient }

function TResponseClient.AllowStartNewInstance(const aFiles: TStrings;
  var outModalErrorMessage, outModalErrorForceUniqueMessage,
  outNotRespondingErrorMessage: string; var outHandleBringToFront: HWND
  ): TStartNewInstanceResult;
var
  xStream: TMemoryStream;
  xMsgType: Integer;
  xResponseType: string;
  xOutParams, xInParams: TMessageParams;
begin
  Result := ofrStartNewInstance;
  xStream := TMemoryStream.Create;
  try
    //ask to show prompt
    xStream.Clear;
    SetLength(xOutParams, 0);
    TIDEInstances.AddFilesToParams(aFiles, xOutParams);
    TIDEInstances.BuildMessage(MESSAGE_STARTNEWINSTANCE, xOutParams, xStream);
    xStream.Position := 0;
    Self.PostRequest(MESSAGETYPE_XML, xStream);
    xStream.Clear;
    if PeekResponse(xStream, xMsgType{%H-}, TIMEOUT_OPENFILES) and
       (xMsgType = MESSAGETYPE_XML) then
    begin
      xStream.Position := 0;
      if TIDEInstances.ParseMessage(xStream, xResponseType, xInParams) and
         (xResponseType = RESPONSE_OPENFILES) then
      begin
        Result := TStartNewInstanceResult(StrToIntDef(TIDEInstances.GetMessageParam(xInParams, PARAM_RESULT), 0));
        outModalErrorMessage := TIDEInstances.GetMessageParam(xInParams, PARAM_MODALERRORMESSAGE);
        outModalErrorForceUniqueMessage := TIDEInstances.GetMessageParam(xInParams, PARAM_FORCEUNIQUEMODALERRORMESSAGE);
        outNotRespondingErrorMessage := TIDEInstances.GetMessageParam(xInParams, PARAM_NOTRESPONDINGERRORMESSAGE);
        outHandleBringToFront := StrToInt64Def(TIDEInstances.GetMessageParam(xInParams, PARAM_HANDLEBRINGTOFRONT), 0);
      end;
    end else//no response, the IDE is modal and cannot accept messages
    begin
      DeleteRequest;
      Result := ofrNotResponding;
    end;
  finally
    xStream.Free;
  end;
end;

{ TMainServer }

procedure TMainServer.CheckMessagesOnTimer(Sender: TObject);
begin
  DoCheckMessages;
end;

constructor TMainServer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FMsgStream := TMemoryStream.Create;
end;

destructor TMainServer.Destroy;
begin
  FMsgStream.Free;
  StopListening;

  inherited Destroy;
end;

procedure TMainServer.DoStartNewInstance(const aMsgID: Integer;
  const aInParams: TMessageParams);
var
  xResult: TStartNewInstanceResult;
  xFiles: TStrings;
  xParams: TMessageParams;
begin
  xResult := ofrStartNewInstance;
  if Assigned(FStartNewInstanceEvent) then
  begin
    xFiles := TStringList.Create;
    try
      TIDEInstances.AddFilesFromParams(aInParams, xFiles);
      FStartNewInstanceEvent(xFiles, xResult);
    finally
      xFiles.Free;
    end;
  end;

  SetLength(xParams, 5);
  xParams[0] := TIDEInstances.MessageParam(PARAM_RESULT, IntToStr(Ord(xResult)));
  xParams[1] := TIDEInstances.MessageParam(PARAM_HANDLEBRINGTOFRONT, IntToStr(Application.MainFormHandle));
  xParams[2] := TIDEInstances.MessageParam(PARAM_MODALERRORMESSAGE, dlgRunningInstanceModalError);
  xParams[3] := TIDEInstances.MessageParam(PARAM_FORCEUNIQUEMODALERRORMESSAGE, dlgForceUniqueInstanceModalError);
  xParams[4] := TIDEInstances.MessageParam(PARAM_NOTRESPONDINGERRORMESSAGE, dlgRunningInstanceNotRespondingError);
  SimpleResponse(aMsgID, RESPONSE_OPENFILES, xParams);
end;

procedure TMainServer.SimpleResponse(const aResponseToMsgID: Integer; const
  aResponseType: string; const aParams: array of TMessageParam);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    TIDEInstances.BuildMessage(aResponseType, aParams, xStream);
    xStream.Position := 0;
    PostResponse(aResponseToMsgID, MESSAGETYPE_XML, xStream);
  finally
    xStream.Free;
  end;
end;

procedure TMainServer.StartListening(const aStartNewInstanceEvent: TStartNewInstanceEvent);
begin
  Assert((FTimer = nil) and Assigned(aStartNewInstanceEvent));

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := @CheckMessagesOnTimer;
  FTimer.Interval := 50;
  FTimer.Enabled := True;

  FStartNewInstanceEvent := aStartNewInstanceEvent;
end;

procedure TMainServer.StopListening;
begin
  FreeAndNil(FTimer);

  FStartNewInstanceEvent := nil;
end;

procedure TMainServer.DoCheckMessages;
var
  xMessageType: string;
  xParams: TMessageParams;
  xMsgID, xMsgType: Integer;
begin
  if Active then
  begin
    if PeekRequest(FMsgStream, xMsgID{%H-}, xMsgType{%H-}) and
       (xMsgType = MESSAGETYPE_XML) and
       (TIDEInstances.ParseMessage(FMsgStream, xMessageType, xParams)) and
       (xMessageType = MESSAGE_STARTNEWINSTANCE)
    then
      DoStartNewInstance(xMsgID, xParams);
  end;
end;

initialization
  FLazIDEInstances := TIDEInstances.Create(nil);
  FLazIDEInstances.InitIDEInstances;

finalization
  FreeAndNil(FLazIDEInstances);

end.
{
 /***************************************************************************
                              ideinstances.pas
                              ----------------

 ***************************************************************************/

 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Author: Ondrej Pokorny

  Abstract:
    This unit handles one/multiple Lazarus IDE instances.

}
unit IDEInstances;

{$mode objfpc}{$H+}

interface

uses
  sysutils, Interfaces, Classes, Controls, Forms, Dialogs, ExtCtrls,
  LCLProc, LCLIntf, LCLType, AdvancedIPC,
  LazFileUtils, LazUTF8, Laz2_DOM, laz2_XMLRead, laz2_XMLWrite,
  LazarusIDEStrConsts, IDECmdLine;

type
  TIDEStartWithFilesAction = (isaStartNewInstance, isaOpenFilesInRunningInstance, isaPrompt, isaModalError);
  TChooseActionEvent = procedure(
    const aFiles: TStrings;
    var Result: TIDEStartWithFilesAction;
    var outPromptMessage: string;
    var outOpenNewInstanceMessage: string) of object;//we want to receive translated outPromptMessage and outOpenNewInstanceMessage -> do not read them directly from LazarusIDEStrConsts here
  TOpenFilesResult = (ofrStartNewInstance, ofrDoNotStart, ofrModalError);
  TOpenFilesEvent = procedure(const aFiles: TStrings;
    var outResult: TOpenFilesResult; var outHandleBringToFront: HWND) of object;

  TMessageParam = record
    Name: string;
    Value: string;
  end;
  TMessageParams = array of TMessageParam;

  TUniqueServer = class(TIPCServer)
  public
    procedure StartUnique(const aServerPrefix: string);
  end;

  TMainServer = class(TUniqueServer)
  private
    FOpenFilesEvent: TOpenFilesEvent;
    FChooseActionEvent: TChooseActionEvent;
    FTimer: TTimer;
    FMsgStream: TMemoryStream;

    procedure DoChooseActionEvent(const aMsgID: string; const aInParams: TMessageParams);
    procedure DoOpenFiles(const aMsgID: string; const aInParams: TMessageParams);

    procedure SimpleResponse(
      const aResponseToMsgID, aResponseType: string;
      const aParams: array of TMessageParam);

    procedure DoCheckMessages;
    procedure CheckMessagesOnTimer(Sender: TObject);

  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TResponseClient = class(TIPCClient)
  public
    function SendFilesToRunningInstance(
      const aFiles: TStrings; var outOpenNewInstanceMessage: string;
      var outHandleBringToFront: HWND): TOpenFilesResult;
  end;

  TIDEInstances = class
  private
    FMainServer: TMainServer;//running IDE
    FStartIDE: Boolean;// = True;
    FForceNewInstance: Boolean;
    FAllowOpenLastProject: Boolean;// = True;
    FFilesToOpen: TStrings;

    class procedure AddFilesToParams(const aFiles: TStrings;
      var ioParams: TMessageParams); static;
    class procedure AddFilesFromParams(const aInParams: TMessageParams;
      const aFiles: TStrings); static;
    class procedure BuildMessage(const aMessageType: string;
      const aParams: array of TMessageParam; const aStream: TStream); static;
    class function MessageParam(const aName, aValue: string): TMessageParam; static;
    class function ParseMessage(const aStream: TStream; out outMessageType: string;
      out outParams: TMessageParams): Boolean; static;
    class function GetMessageParam(const aParams: array of TMessageParam;
      const aParamName: string): string; static;

    function CheckParamsForForceNewInstanceOpt: Boolean;

    procedure CollectFiles(out
      outFilesWereSentToCollectingServer: Boolean);

    function SendFilesToRunningInstance(const aFiles: TStrings;
      var outOpenNewInstanceMessage: string; var outHandleBringToFront: HWND): TOpenFilesResult;
    procedure InitIDEInstances;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure PerformCheck;//call PerformCheck after Application.Initialize - it can open dialogs!

    procedure StartListening(const aOpenFilesEvent: TOpenFilesEvent;
      const aChooseActionEvent: TChooseActionEvent);
    procedure StopListening;
    function StartIDE: Boolean;//can the IDE be started?
    function AllowOpenLastProject: Boolean;//if a secondary IDE is starting, do NOT reopen last project!
    function FilesToOpen: TStrings;
    procedure DeleteCollectMessageFiles;
  end;

function LazIDEInstances: TIDEInstances;

implementation

const
  SERVERPREFIX_MAIN = 'LazarusMain';
  SERVERNAME_COLLECT = 'LazarusCollect';
  MESSAGETYPE_XML = 2;
  ELEMENT_ROOT = 'ideinstances';
  ATTR_VALUE = 'value';
  ATTR_MESSAGE_TYPE = 'msgtype';
  MESSAGE_CHOOSEACTION = 'chooseaction';
  RESPONSE_SHOWPROMPT = 'showprompt';
  MESSAGE_OPENFILES = 'openfiles';
  MESSAGE_COLLECTFILES = 'collectfiles';
  RESPONSE_STARTNEWINSTANCE = 'startnewinstance';
  PARAM_FILE = 'file';
  PARAM_RESULT = 'result';
  PARAM_HANDLEBRINGTOFRONT = 'handlebringtofront';
  PARAM_PROMPTMESSAGE = 'promptmessage';
  PARAM_OPENNEWINSTANCEMESSAGE = 'opennewinstancemessage';
  MAX_CHECK_INSTANCES = 10;//check maximum count of continuously started instances
  TIMEOUT_COLLECTFILES = 100;
  TIMEOUT_SHOWPROMPT = 500;//first timeout is 1/2 second, we should get an answer fast
  TIMEOUT_STARTNEWINSTANCE = 3000;//we know that Lazarus is already running and responding so we get an answer, the UnHide process could take some time, let's wait for it max 3 seconds

var
  FLazIDEInstances: TIDEInstances;

function LazIDEInstances: TIDEInstances;
begin
  Result := FLazIDEInstances;
end;

{ TIDEInstances }

class function TIDEInstances.MessageParam(const aName, aValue: string): TMessageParam;
begin
  Result.Name := aName;
  Result.Value := aValue;
end;

function TIDEInstances.StartIDE: Boolean;
begin
  Result := FStartIDE;
end;

function TIDEInstances.AllowOpenLastProject: Boolean;
begin
  Result := FAllowOpenLastProject;
end;

function TIDEInstances.FilesToOpen: TStrings;
begin
  if not Assigned(FFilesToOpen) then
    FFilesToOpen := TStringList.Create;
  Result := FFilesToOpen;
end;

procedure TIDEInstances.StartListening(const aOpenFilesEvent: TOpenFilesEvent;
  const aChooseActionEvent: TChooseActionEvent);
begin
  Assert(Assigned(aOpenFilesEvent) and Assigned(aChooseActionEvent));

  if not Assigned(FMainServer) then
  begin
    FMainServer := TMainServer.Create;
    FMainServer.StartUnique(SERVERPREFIX_MAIN);
  end;
  FMainServer.FOpenFilesEvent := aOpenFilesEvent;
  FMainServer.FChooseActionEvent := aChooseActionEvent;

  DeleteCollectMessageFiles;
end;

procedure TIDEInstances.StopListening;
begin
  FreeAndNil(FMainServer);
end;

class procedure TIDEInstances.AddFilesFromParams(
  const aInParams: TMessageParams; const aFiles: TStrings);
var
  I: Integer;
begin
  //do not clear aFiles
  for I := Low(aInParams) to High(aInParams) do
    if aInParams[I].Name = PARAM_FILE then
      aFiles.Add(aInParams[I].Value);
end;

class procedure TIDEInstances.AddFilesToParams(const aFiles: TStrings;
  var ioParams: TMessageParams);
var
  xStartIndex: Integer;
  I: Integer;
begin
  xStartIndex := Length(ioParams);
  SetLength(ioParams, xStartIndex+aFiles.Count);
  for I := 0 to aFiles.Count-1 do
    ioParams[xStartIndex+I] := MessageParam(PARAM_FILE, aFiles[I]);
end;

class function TIDEInstances.GetMessageParam(
  const aParams: array of TMessageParam; const aParamName: string): string;
var
  I: Integer;
begin
  for I := 0 to Length(aParams) do
  if aParams[I].Name = aParamName then
    Exit(aParams[I].Value);

  Result := '';//not found
end;

class procedure TIDEInstances.BuildMessage(const aMessageType: string;
  const aParams: array of TMessageParam; const aStream: TStream);
var
  xDOM: TXMLDocument;
  xRoot: TDOMElement;
  xParam: TDOMElement;
  I: Integer;
begin
  xDOM := TXMLDocument.Create;
  try
    xRoot := xDOM.CreateElement(ELEMENT_ROOT);
    xRoot.AttribStrings[ATTR_MESSAGE_TYPE] := aMessageType;
    xDOM.AppendChild(xRoot);

    for I := Low(aParams) to High(aParams) do
    begin
      xParam := xDOM.CreateElement(aParams[I].Name);
      xRoot.AppendChild(xParam);
      xParam.AttribStrings[ATTR_VALUE] := aParams[I].Value;
    end;

    WriteXMLFile(xDOM, aStream);
  finally
    xDOM.Free;
  end;
end;

class function TIDEInstances.ParseMessage(const aStream: TStream; out
  outMessageType: string; out outParams: TMessageParams): Boolean;
var
  xDOM: TXMLDocument;
  xChildList: TDOMNodeList;
  I, J: Integer;
begin
  Result := False;

  outMessageType := '';
  SetLength(outParams, 0);
  try
    ReadXMLFile(xDOM, aStream, []);
  except
    on EXMLReadError do
      Exit;//eat XML exceptions
  end;
  try
    if (xDOM = nil) or (xDOM.DocumentElement = nil) or (xDOM.DocumentElement.NodeName <> ELEMENT_ROOT) then
      Exit;

    outMessageType := xDOM.DocumentElement.AttribStrings[ATTR_MESSAGE_TYPE];

    xChildList := xDOM.DocumentElement.ChildNodes;
    SetLength(outParams, xChildList.Count);
    J := 0;
    for I := 0 to xChildList.Count-1 do
    if xChildList[I] is TDOMElement then
    begin
      outParams[J].Name := xChildList[I].NodeName;
      outParams[J].Value := TDOMElement(xChildList[I]).AttribStrings[ATTR_VALUE];
      Inc(J);
    end;
    SetLength(outParams, J);
    Result := True;
  finally
    xDOM.Free;
  end;
end;

function TIDEInstances.SendFilesToRunningInstance(const aFiles: TStrings;
  var outOpenNewInstanceMessage: string; var outHandleBringToFront: HWND
  ): TOpenFilesResult;
var
  xStartClient: TResponseClient;
  I: Integer;
begin
  Result := ofrStartNewInstance;
  xStartClient := TResponseClient.Create;
  try
    for I := 1 to MAX_CHECK_INSTANCES do//check for multiple instances
    begin
      xStartClient.ServerName := SERVERPREFIX_MAIN+IntToStr(I);
      if xStartClient.ServerRunning then
      begin
        //there are open Lazarus instances, do not reopen last project!
        FAllowOpenLastProject := False;
        Result := xStartClient.SendFilesToRunningInstance(aFiles, outOpenNewInstanceMessage, outHandleBringToFront);
        if Result <> ofrModalError then//if the current IDE is modal, try another one
          Exit;//handle only one running Lazarus IDE
      end;
    end;
  finally
    xStartClient.Free;
  end;
end;

function TIDEInstances.CheckParamsForForceNewInstanceOpt: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to ParamsAndCfgCount do
    if ParamIsOption(i, ForceNewInstanceOpt) then//ignore the settings and start new Lazarus IDE instance
      Result := True;
end;

procedure TIDEInstances.DeleteCollectMessageFiles;
var
  xServer: TIPCServer;
begin
  xServer := TIPCServer.Create;
  try
    xServer.ServerName := SERVERNAME_COLLECT;
    xServer.DeletePendingRequests;
  finally
    xServer.Free;
  end;
end;

procedure TIDEInstances.PerformCheck;
var
  xResult: TOpenFilesResult;
  xOpenNewInstanceMessage: string = '';
  xHandleBringToFront: HWND = 0;
begin
  if not FStartIDE then//InitIDEInstances->CollectOtherOpeningFiles decided not to start the IDE
    Exit;

  if not FForceNewInstance and (FilesToOpen.Count > 0) then
    xResult := SendFilesToRunningInstance(FilesToOpen, xOpenNewInstanceMessage, xHandleBringToFront)
  else
    xResult := ofrStartNewInstance;

  if xOpenNewInstanceMessage = '' then
    xOpenNewInstanceMessage := dlgRunningInstanceModalOpenNew;
  FStartIDE :=
    (xResult = ofrStartNewInstance) or
      ((xResult = ofrModalError) and
       (MessageDlg(lisLazarusIDE, Format(xOpenNewInstanceMessage, [FilesToOpen.Text]), mtWarning, mbYesNo, 0, mbYes) = mrYes));//user decided to open in new ide

  {$IFDEF MSWINDOWS}
  if not FStartIDE and (xHandleBringToFront <> 0) then
  begin
    try
      SetForegroundWindow(xHandleBringToFront);//SetForegroundWindow works (on Windows) only if the calling process is the foreground process, therefore it must be here!
    except
      //eat all widget exceptions
    end;
  end;
  {$ENDIF}
end;

constructor TIDEInstances.Create;
begin
  inherited Create;

  FStartIDE := True;
  FAllowOpenLastProject := True;
end;

destructor TIDEInstances.Destroy;
begin
  FreeAndNil(FMainServer);
  FreeAndNil(FFilesToOpen);

  inherited Destroy;
end;

procedure TIDEInstances.CollectFiles(out
  outFilesWereSentToCollectingServer: Boolean);

var
  xThisClientMessageId: string;

  procedure _SendToServer;
  var
    xClient: TIPCClient;
    xOutParams: TMessageParams;
    xStream: TMemoryStream;
  begin
    xClient := TIPCClient.Create;
    try
      xClient.ServerName := SERVERNAME_COLLECT;

      SetLength(xOutParams, 0);
      AddFilesToParams(FilesToOpen, xOutParams);

      xStream := TMemoryStream.Create;
      try
        BuildMessage(MESSAGE_COLLECTFILES, xOutParams, xStream);
        xStream.Position := 0;
        xClient.PostRequest(MESSAGETYPE_XML, xStream, xThisClientMessageId);
      finally
        xStream.Free;
      end;
    finally
      xClient.Free;
    end;
  end;

  procedure _WaitForFiles;
  var
    xLastCount, xNewCount: Integer;
    xServer: TIPCServer;
  begin
    xServer := TIPCServer.Create;
    try
      xServer.ServerName := SERVERNAME_COLLECT;
      //do not start server here
      xLastCount := -1;
      xNewCount := xServer.GetPendingRequestCount;
      while xLastCount <> xNewCount do
      begin
        xLastCount := xNewCount;
        Sleep(TIMEOUT_COLLECTFILES);
        xNewCount := xServer.GetPendingRequestCount;
      end;
    finally
      xServer.Free;
    end;
  end;

  function _ReceiveAsServer: Boolean;
  var
    xServer: TIPCServer;
    xInParams: TMessageParams;
    xStream: TMemoryStream;
    xMsgType: Integer;
    xMessageType: string;
  begin
    xStream := TMemoryStream.Create;
    xServer := TIPCServer.Create;
    try
      xServer.ServerName := SERVERNAME_COLLECT;
      //files have to be handled only by one instance!
      Result := xServer.FindHighestPendingRequestId = xThisClientMessageId;
      if Result then
      begin
        //we are the highest client, handle the files
        xServer.StartServer(False);
      end else
      begin
        //we are not the highest client, maybe there are pending files, check that
        {$IFNDEF MSWINDOWS}
        //this code is not slowing up IDE start because if there was highest client found (the normal way), we close anyway
        Randomize;
        Sleep(50+Random(50));//random sleep in order to prevent double file locks on linux
        {$ENDIF}
        if not (xServer.StartServer(False) and (xServer.GetPendingRequestCount > 0)) then
          Exit;//server is already running or there are no pending message -> close
        Result := True;//no one handled handled the files, do it by myself
      end;

      FilesToOpen.Clear;
      while xServer.PeekRequest(xStream, xMsgType{%H-}) do
      if xMsgType = MESSAGETYPE_XML then
      begin
        if ParseMessage(xStream, xMessageType, xInParams) and
           (xMessageType = MESSAGE_COLLECTFILES)
        then
          AddFilesFromParams(xInParams, FilesToOpen);
      end;
    finally
      xStream.Free;
      xServer.Free;
    end;
  end;
begin
  //if you select more files in explorer and open them, they are not opened in one process but one process is started per file
  // -> collect them

  //first send messages to queue (there is no server, no problem, it will collect the messages when it is created)
  _SendToServer;

  //now wait until we have everything
  _WaitForFiles;

  //now send them to one instance
  outFilesWereSentToCollectingServer := not _ReceiveAsServer;
end;

procedure TIDEInstances.InitIDEInstances;
var
  xFilesWereSentToCollectingServer: Boolean;
begin
  FForceNewInstance := CheckParamsForForceNewInstanceOpt;

  //get cmd line filenames
  FFilesToOpen := ExtractCmdLineFilenames;

  if FilesToOpen.Count > 0 then//if there are file in the cmd, check for multiple starting instances
  begin
    CollectFiles(xFilesWereSentToCollectingServer);
    if xFilesWereSentToCollectingServer then
    begin
      FilesToOpen.Clear;
      FStartIDE := False;
    end;
  end;
end;

{ TUniqueServer }

procedure TUniqueServer.StartUnique(const aServerPrefix: string);
var
  I: Integer;
begin
  if Active then
    StopServer;

  I := 0;
  while not Active do
  begin
    Inc(I);
    ServerName := aServerPrefix+IntToStr(I);
    StartServer;
  end;
end;

{ TResponseClient }

function TResponseClient.SendFilesToRunningInstance(const aFiles: TStrings;
  var outOpenNewInstanceMessage: string; var outHandleBringToFront: HWND
  ): TOpenFilesResult;
var
  xStream: TMemoryStream;
  xMsgType: Integer;
  xResponseType: string;
  xOutParams, xInParams: TMessageParams;
  xChooseAction: TIDEStartWithFilesAction;
  xPromptMessage: string;
begin
  Result := ofrStartNewInstance;
  xStream := TMemoryStream.Create;
  try
    //ask to show prompt
    xChooseAction := isaStartNewInstance;
    SetLength(xOutParams, 0);
    TIDEInstances.AddFilesToParams(aFiles, xOutParams);
    TIDEInstances.BuildMessage(MESSAGE_CHOOSEACTION, xOutParams, xStream);
    xStream.Position := 0;
    PostRequest(MESSAGETYPE_XML, xStream);
    xStream.Clear;
    if PeekResponse(xStream, xMsgType{%H-}, TIMEOUT_SHOWPROMPT) and
       (xMsgType = MESSAGETYPE_XML) then
    begin
      xStream.Position := 0;
      if TIDEInstances.ParseMessage(xStream, xResponseType, xInParams) and
         (xResponseType = RESPONSE_SHOWPROMPT) then
      begin
        xChooseAction := TIDEStartWithFilesAction(StrToIntDef(TIDEInstances.GetMessageParam(xInParams, PARAM_RESULT), 0));
        outOpenNewInstanceMessage := TIDEInstances.GetMessageParam(xInParams, PARAM_OPENNEWINSTANCEMESSAGE);
        xPromptMessage := TIDEInstances.GetMessageParam(xInParams, PARAM_PROMPTMESSAGE);
        if xChooseAction = isaModalError then
          Exit(ofrModalError);
      end;
    end else//no response, the IDE is modal and cannot accept messages
    begin
      DeleteRequest;
      Exit(ofrModalError);
    end;

    case xChooseAction of
      isaPrompt:
      begin
        if xPromptMessage = '' then
          xPromptMessage := dlgOpenInRunningInstance;
        case MessageDlg(lisLazarusIDE, Format(xPromptMessage, [aFiles.Text]), mtConfirmation, mbYesNo, 0, mbYes) of
          mrYes: begin end;//user hit "yes" -> proceed
          mrNo: Exit(ofrStartNewInstance);//user hit "no" -> open new instance
        else//cancel/close -> do nothing, do not open IDE
          Exit(ofrDoNotStart);
        end;
      end;
      isaStartNewInstance://settings is startnewide -> open new instance
        Exit(ofrStartNewInstance);
    end;

    //open files in new instance
    xStream.Clear;
    TIDEInstances.BuildMessage(MESSAGE_OPENFILES, xOutParams, xStream);
    xStream.Position := 0;
    Self.PostRequest(MESSAGETYPE_XML, xStream);
    xStream.Clear;
    if PeekResponse(xStream, xMsgType, TIMEOUT_STARTNEWINSTANCE) and
       (xMsgType = MESSAGETYPE_XML) then
    begin
      xStream.Position := 0;
      if TIDEInstances.ParseMessage(xStream, xResponseType, xInParams) and
         (xResponseType = RESPONSE_STARTNEWINSTANCE) then
      begin
        Result := TOpenFilesResult(StrToIntDef(TIDEInstances.GetMessageParam(xInParams, PARAM_RESULT), 0));
        outHandleBringToFront := StrToInt64Def(TIDEInstances.GetMessageParam(xInParams, PARAM_HANDLEBRINGTOFRONT), 0);
      end;
    end else//no response, the IDE is modal and cannot accept messages
    begin
      DeleteRequest;
      Exit(ofrModalError);
    end;
  finally
    xStream.Free;
  end;
end;

{ TMainServer }

procedure TMainServer.CheckMessagesOnTimer(Sender: TObject);
begin
  DoCheckMessages;
end;

constructor TMainServer.Create;
begin
  inherited Create;

  FMsgStream := TMemoryStream.Create;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := @CheckMessagesOnTimer;
  FTimer.Interval := 50;
  FTimer.Enabled := True;
end;

destructor TMainServer.Destroy;
begin
  FMsgStream.Free;
  FTimer.Free;//must free manually before inherited Destroy

  inherited Destroy;
end;

procedure TMainServer.DoChooseActionEvent(const aMsgID: string;
  const aInParams: TMessageParams);
var
  xResult: TIDEStartWithFilesAction;
  xParams: TMessageParams;
  xPromptMessage, xOpenNewInstanceMessage: string;
  xFiles: TStringList;
begin
  xResult := isaStartNewInstance;
  xPromptMessage := '';
  xOpenNewInstanceMessage := '';
  if Assigned(FChooseActionEvent) then
  begin
    xFiles := TStringList.Create;
    try
      TIDEInstances.AddFilesFromParams(aInParams, xFiles);
      FChooseActionEvent(xFiles, xResult, xPromptMessage, xOpenNewInstanceMessage);
    finally
      xFiles.Free;
    end;
  end;

  SetLength(xParams, 3);
  xParams[0] := TIDEInstances.MessageParam(PARAM_RESULT, IntToStr(Ord(xResult)));
  xParams[1] := TIDEInstances.MessageParam(PARAM_PROMPTMESSAGE, xPromptMessage);
  xParams[2] := TIDEInstances.MessageParam(PARAM_OPENNEWINSTANCEMESSAGE, xOpenNewInstanceMessage);
  SimpleResponse(aMsgID, RESPONSE_SHOWPROMPT, xParams);
end;

procedure TMainServer.DoOpenFiles(const aMsgID: string;
  const aInParams: TMessageParams);
var
  xResult: TOpenFilesResult;
  xHandleBringToFront: HWND;
  xFiles: TStrings;
  xParams: TMessageParams;
begin
  xResult := ofrStartNewInstance;
  xHandleBringToFront := 0;
  if Assigned(FOpenFilesEvent) then
  begin
    xFiles := TStringList.Create;
    try
      TIDEInstances.AddFilesFromParams(aInParams, xFiles);
      FOpenFilesEvent(xFiles, xResult, xHandleBringToFront);
    finally
      xFiles.Free;
    end;
  end;

  SetLength(xParams, 2);
  xParams[0] := TIDEInstances.MessageParam(PARAM_RESULT, IntToStr(Ord(xResult)));
  xParams[1] := TIDEInstances.MessageParam(PARAM_HANDLEBRINGTOFRONT, IntToStr(xHandleBringToFront));
  SimpleResponse(aMsgID, RESPONSE_STARTNEWINSTANCE, xParams);
end;

procedure TMainServer.SimpleResponse(const aResponseToMsgID,
  aResponseType: string; const aParams: array of TMessageParam);
var
  xStream: TMemoryStream;
begin
  xStream := TMemoryStream.Create;
  try
    TIDEInstances.BuildMessage(aResponseType, aParams, xStream);
    xStream.Position := 0;
    PostResponse(aResponseToMsgID, MESSAGETYPE_XML, xStream);
  finally
    xStream.Free;
  end;
end;

procedure TMainServer.DoCheckMessages;
var
  xMessageType: string;
  xParams: TMessageParams;
  xMsgID: string;
  xMsgType: Integer;
begin
  if Active then
  begin
    if PeekRequest(FMsgStream, xMsgID{%H-}, xMsgType{%H-}) and
       (xMsgType = MESSAGETYPE_XML) then
    begin
      if TIDEInstances.ParseMessage(FMsgStream, xMessageType, xParams) and
         (xMessageType = MESSAGE_CHOOSEACTION)
      then
        DoChooseActionEvent(xMsgID, xParams)
      else
      if xMessageType = MESSAGE_OPENFILES then
        DoOpenFiles(xMsgID, xParams);
    end;
  end;
end;

initialization
  FLazIDEInstances := TIDEInstances.Create;
  FLazIDEInstances.InitIDEInstances;

finalization
  FreeAndNil(FLazIDEInstances);

end.
