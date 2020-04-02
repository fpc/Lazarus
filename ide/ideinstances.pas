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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
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
  LCLProc, LCLIntf, LCLType, LazFileUtils, LazUTF8, laz2_XMLRead, laz2_XMLWrite,
  Laz2_DOM, LazarusIDEStrConsts, IDECmdLine, crc,
  {$IF (FPC_FULLVERSION >= 30101)}
  AdvancedIPC
  {$ELSE}
  LazAdvancedIPC
  {$ENDIF}
  ;

type
  TStartNewInstanceResult = (ofrStartNewInstance, ofrDoNotStart, ofrModalError,
                             ofrForceSingleInstanceModalError, ofrNotResponding);
  TStartNewInstanceEvent = procedure(const aFiles: TStrings;
    var outResult: TStartNewInstanceResult; var outSourceWindowHandle: HWND) of object;
  TGetCurrentProjectEvent = procedure(var outProjectFileName: string) of object;

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
    FGetCurrentProjectEvent: TGetCurrentProjectEvent;
    FTimer: TTimer;
    FMsgStream: TMemoryStream;

    procedure DoStartNewInstance(const aMsgID: Integer; const aInParams: TMessageParams);
    procedure DoGetCurrentProject(const aMsgID: Integer; const {%H-}aInParams: TMessageParams);

    procedure SimpleResponse(const aResponseToMsgID: Integer;
      const aResponseType: string; const aParams: array of TMessageParam);

    procedure DoCheckMessages;
    procedure CheckMessagesOnTimer(Sender: TObject);

    procedure StartListening(const aStartNewInstanceEvent: TStartNewInstanceEvent;
      const aGetCurrentProjectEvent: TGetCurrentProjectEvent);
    procedure StopListening;

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TResponseClient = class(TIPCClient)
  public
    function GetCurrentProjectFileName: string;
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
    procedure StartListening(const aStartNewInstanceEvent: TStartNewInstanceEvent;
      const aGetCurrentProjectEvent: TGetCurrentProjectEvent);
    procedure StopListening;

    function StartIDE: Boolean;//can the IDE be started?
    function ProjectIsOpenInAnotherInstance(aProjectFileName: string): Boolean;
    function FilesToOpen: TStrings;
  end;

function LazIDEInstances: TIDEInstances;

implementation

const
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
  MESSAGE_GETOPENEDPROJECT = 'getopenedproject';
  RESPONSE_GETOPENEDPROJECT = 'getopenedprojectResponse';
  TIMEOUT_GETOPENEDPROJECT = 100;
var
  FLazIDEInstances: TIDEInstances;
  FServerPrefix: string;

function LazIDEInstances: TIDEInstances;
begin
  Result := FLazIDEInstances;
end;

function LazServerPrefix: string;
// allow for multiple users on lazarus host system - encode to prevent illegal chars
begin
  if FServerPrefix = '' then
  begin
    // Calculate the user specific instance prefix only once.
    FServerPrefix := GetEnvironmentVariable('USER');    // current user
    // encode to cover illegal chars ('-' etc)
    FServerPrefix := IntToStr( crc32(0, pbyte(FServerPrefix), Length(FServerPrefix)) )
                     + '_LazarusMain';
  end;
  Result := FServerPrefix;
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

function TIDEInstances.ProjectIsOpenInAnotherInstance(aProjectFileName: string
  ): Boolean;
var
  xStartClient: TResponseClient;
  I: Integer;
  xServerIDs, xOpenedProjectFiles: TStringList;
  xProjFileName: string;
begin
  aProjectFileName := ExtractFilePath(aProjectFileName)+ExtractFileNameOnly(aProjectFileName);

  xStartClient := nil;
  xServerIDs := nil;
  xOpenedProjectFiles := nil;
  try
    xStartClient := TResponseClient.Create(nil);
    xServerIDs := TStringList.Create;
    xOpenedProjectFiles := TStringList.Create;

    xStartClient.FindRunningServers(LazServerPrefix, xServerIDs);

    for I := 0 to xServerIDs.Count-1 do
    begin
      if FMainServer.ServerID = xServerIDs[I] then
        continue; // ignore current instance
      xStartClient.ServerID := xServerIDs[I];
      xProjFileName := xStartClient.GetCurrentProjectFileName;
      if (xProjFileName='') then
        continue;
      xProjFileName := ExtractFilePath(xProjFileName)+ExtractFileNameOnly(xProjFileName);
      if CompareFilenames(xProjFileName, aProjectFileName)=0 then
        Exit(True);
    end;
  finally
    xStartClient.Free;
    xServerIDs.Free;
    xOpenedProjectFiles.Free;
  end;

  Result := False;
end;

function TIDEInstances.FilesToOpen: TStrings;
begin
  if not Assigned(FFilesToOpen) then
    FFilesToOpen := TStringList.Create;
  Result := FFilesToOpen;
end;

procedure TIDEInstances.StartListening(
  const aStartNewInstanceEvent: TStartNewInstanceEvent;
  const aGetCurrentProjectEvent: TGetCurrentProjectEvent);
begin
  Assert(Assigned(FMainServer));

  FMainServer.StartListening(aStartNewInstanceEvent, aGetCurrentProjectEvent);
end;

procedure TIDEInstances.StartServer;
begin
  Assert(FMainServer = nil);

  FMainServer := TMainServer.Create(Self);
  FMainServer.StartUnique(LazServerPrefix);
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
  try                                      //check for multiple instances
    xStartClient.FindRunningServers(LazServerPrefix, xServerIDs);
    xServerIDs.Sort;

    for I := xServerIDs.Count-1 downto 0 do//last started is first to choose
    begin
      xStartClient.ServerID := xServerIDs[I];
      if xStartClient.ServerRunning then
      begin
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
        Sleep((PtrInt(Random($3F)) + {%H-}PtrInt(GetCurrentThreadId)) and $3F);
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
  Tmp: String;
begin
  if Active then
    StopServer;

  I := 0;
  while not Active do
  begin
    Inc(I);
    ServerID := aServerPrefix+Format('%.2d',[I]);
    // FileName is composed of TempDir and ServerID. Make sure TempDir exists.
    Tmp := GetTempDir(Global);        // Use TIPCBase.Global property also here.
    if not DirectoryExists(Tmp) then
      ForceDirectories(Tmp);
    StartServer;  // This uses the FileName in TempDir.
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
    end else//no response
    begin
      DeleteRequest;
      Result := ofrNotResponding;
    end;
  finally
    xStream.Free;
  end;
end;

function TResponseClient.GetCurrentProjectFileName: string;
var
  xStream: TMemoryStream;
  xMsgType: Integer;
  xResponseType: string;
  xOutParams, xInParams: TMessageParams;
begin
  Result := '';
  xStream := TMemoryStream.Create;
  try
    xStream.Clear;
    SetLength(xOutParams, 0);
    TIDEInstances.BuildMessage(MESSAGE_GETOPENEDPROJECT, xOutParams, xStream);
    xStream.Position := 0;
    Self.PostRequest(MESSAGETYPE_XML, xStream);
    xStream.Clear;
    if PeekResponse(xStream, xMsgType{%H-}, TIMEOUT_GETOPENEDPROJECT) and
       (xMsgType = MESSAGETYPE_XML) then
    begin
      xStream.Position := 0;
      if TIDEInstances.ParseMessage(xStream, xResponseType, xInParams) and
         (xResponseType = RESPONSE_GETOPENEDPROJECT) then
      begin
        Result := TIDEInstances.GetMessageParam(xInParams, PARAM_RESULT);
      end;
    end else//no response
    begin
      DeleteRequest;
      Result := '';
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
  xSourceWindowHandle: HWND = 0;
begin
  xResult := ofrStartNewInstance;
  if Assigned(FStartNewInstanceEvent) then
  begin
    xFiles := TStringList.Create;
    try
      TIDEInstances.AddFilesFromParams(aInParams, xFiles);
      FStartNewInstanceEvent(xFiles, xResult, xSourceWindowHandle);
    finally
      xFiles.Free;
    end;
  end;

  SetLength(xParams, 5);
  xParams[0] := TIDEInstances.MessageParam(PARAM_RESULT, IntToStr(Ord(xResult)));
  xParams[1] := TIDEInstances.MessageParam(PARAM_HANDLEBRINGTOFRONT, IntToStr(xSourceWindowHandle)); // do not use Application.MainFormHandle here - it steals focus from active source editor
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

procedure TMainServer.StartListening(
  const aStartNewInstanceEvent: TStartNewInstanceEvent;
  const aGetCurrentProjectEvent: TGetCurrentProjectEvent);
begin
  Assert((FTimer = nil) and Assigned(aStartNewInstanceEvent) and Assigned(aGetCurrentProjectEvent));

  FTimer := TTimer.Create(nil);
  FTimer.OnTimer := @CheckMessagesOnTimer;
  FTimer.Interval := 50;
  FTimer.Enabled := True;

  FStartNewInstanceEvent := aStartNewInstanceEvent;
  FGetCurrentProjectEvent := aGetCurrentProjectEvent;
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
    while
       PeekRequest(FMsgStream, xMsgID{%H-}, xMsgType{%H-}) and
       (xMsgType = MESSAGETYPE_XML) and
       (TIDEInstances.ParseMessage(FMsgStream, xMessageType, xParams))
    do
      case xMessageType of
        MESSAGE_STARTNEWINSTANCE: DoStartNewInstance(xMsgID, xParams);
        MESSAGE_GETOPENEDPROJECT: DoGetCurrentProject(xMsgID, xParams);
      end;
  end;
end;

procedure TMainServer.DoGetCurrentProject(const aMsgID: Integer;
  const aInParams: TMessageParams);
var
  xResult: string;
  xParams: TMessageParams;
begin
  xResult := '';
  if Assigned(FStartNewInstanceEvent) then
    FGetCurrentProjectEvent(xResult);

  SetLength(xParams, 1);
  xParams[0] := TIDEInstances.MessageParam(PARAM_RESULT, xResult);
  SimpleResponse(aMsgID, RESPONSE_GETOPENEDPROJECT, xParams);
end;

initialization
  FLazIDEInstances := TIDEInstances.Create(nil);
  FLazIDEInstances.InitIDEInstances;

finalization
  FreeAndNil(FLazIDEInstances);

end.
