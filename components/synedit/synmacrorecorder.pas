{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynMacroRecorder.pas, released 2001-10-17.

Author of this file is Flávio Etrusco.
Portions created by Flávio Etrusco are Copyright 2001 Flávio Etrusco.
All Rights Reserved.

Contributors to the SynEdit project are listed in the Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id$

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Known Issues:
-------------------------------------------------------------------------------}

unit SynMacroRecorder;

{$I synedit.inc}

interface

uses
  Classes, SysUtils, FileUtil, LazUTF8Classes, Types, LCLType, Menus,
  SynEdit, SynEditKeyCmds, SynEditPlugins;

resourcestring
  sCannotRecord = 'Cannot record macro when recording';
  sCannotPlay = 'Cannot playback macro when recording';
  sCannotPause = 'Can only pause when recording';
  sCannotResume = 'Can only resume when paused';

type
  TSynMacroState = (msStopped, msRecording, msPlaying, msPaused); // msPaused = paused recording
  TSynMacroCommand = (mcRecord, mcPlayback);
  TSynEventParamType = (ptString, ptInteger);

  TSynMacroEvent = class;

  TSynMacroEventWriter = class(TObject)
  public
    procedure WriteEventCommand(const ACmd: TSynEditorCommand); virtual; abstract;
    procedure WriteEventParam(const AParam: string); virtual; abstract;
    procedure WriteEventParam(const AParam: integer); virtual; abstract;
  end;

  { TSynMacroEventReader }

  TSynMacroEventReader = class(TObject)
  protected
    function GetParamAsInt(Index: Integer): Integer; virtual; abstract;
    function GetParamAsString(Index: Integer): String; virtual; abstract;
    function GetParamType(Index: Integer): TSynEventParamType; virtual; abstract;
  public
    function  EventCommand: TSynEditorCommand; virtual; abstract;
    function  ParamCount: Integer; virtual; abstract;
    property  ParamType[Index: Integer]: TSynEventParamType read GetParamType;
    property  ParamAsString[Index: Integer]: String read GetParamAsString;
    property  ParamAsInt[Index: Integer]: Integer read GetParamAsInt;
  end;

  { TSynMacroEvent }

  TSynMacroEvent = class(TObject)
  protected
    fRepeatCount: Integer;
    function GetAsString : string; virtual; abstract;
    procedure InitEventParameters(aStr : string); virtual; abstract;
  public
    constructor Create; virtual;
    procedure Initialize(aCmd: TSynEditorCommand;
      const aChar: TUTF8Char;
      aData: Pointer);  virtual; abstract;
    procedure Assign(AnEvent: TSynMacroEvent); virtual;
    { the CommandID must not be read inside LoadFromStream/SaveToStream. It's read by the
    MacroRecorder component to decide which MacroEvent class to instanciate }
    procedure LoadFromStream(aStream: TStream); virtual; abstract;
    procedure SaveToStream(aStream: TStream); virtual; abstract;
    procedure LoadFromReader(aReader: TSynMacroEventReader); virtual; abstract;
    procedure SaveToWriter(aWriter: TSynMacroEventWriter); virtual; abstract;
    procedure Playback(aEditor: TCustomSynEdit); virtual; abstract;
    property AsString : string read GetAsString;
    property RepeatCount : Integer read fRepeatCount write fRepeatCount;
  end;

  TSynMacroEventClass = class of TSynMacroEvent;

  { TSynIgnoredEvent }

  TSynIgnoredEvent = class(TSynMacroEvent)
  protected
    function GetAsString : string; override;
    procedure InitEventParameters(aStr : string); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand;
      const aChar: TUTF8Char;
      aData: Pointer); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure LoadFromReader(aReader: TSynMacroEventReader); override;
    procedure SaveToWriter(aWriter: TSynMacroEventWriter); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  end;

  TSynSkippedEvent = class(TSynIgnoredEvent) // Do not queue
  end;

  { TSynBasicEvent }

  TSynBasicEvent = class(TSynMacroEvent)
  protected
    fCommand: TSynEditorCommand;
    function GetAsString : string; override;
    procedure InitEventParameters(aStr : string); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand;
      const aChar: TUTF8Char;
      aData: Pointer); override;
    procedure Assign(AnEvent: TSynMacroEvent); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure LoadFromReader(aReader: TSynMacroEventReader); override;
    procedure SaveToWriter(aWriter: TSynMacroEventWriter); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  public
    property Command: TSynEditorCommand read fCommand write fCommand;
  end;

  { TSynCharEvent }

  TSynCharEvent = class(TSynMacroEvent)
  protected
    fKey: TUTF8Char;
    function GetAsString : string; override;
    procedure InitEventParameters(aStr : string); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand;
      const aChar: TUTF8Char;
      aData: Pointer); override;
    procedure Assign(AnEvent: TSynMacroEvent); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure LoadFromReader(aReader: TSynMacroEventReader); override;
    procedure SaveToWriter(aWriter: TSynMacroEventWriter); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  public
    property Key: TUTF8Char read fKey write fKey;
  end;

  { TSynStringEvent }

  TSynStringEvent = class(TSynMacroEvent)
  protected
    fString: string;
    function GetAsString : string; override;
    procedure InitEventParameters(aStr : string); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand;
      const aChar: TUTF8Char;
      aData: Pointer); override;
    procedure Assign(AnEvent: TSynMacroEvent); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure LoadFromReader(aReader: TSynMacroEventReader); override;
    procedure SaveToWriter(aWriter: TSynMacroEventWriter); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  public
    property Value: string read fString write fString;
  end;

  { TSynPositionEvent }

  TSynPositionEvent = class(TSynBasicEvent)
  protected
    fPosition: TPoint;
    function GetAsString : string; override;
    procedure InitEventParameters(aStr : string); override;
  public
    procedure Initialize(aCmd: TSynEditorCommand;
      const aChar: TUTF8Char;
      aData: Pointer); override;
    procedure Assign(AnEvent: TSynMacroEvent); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure LoadFromReader(aReader: TSynMacroEventReader); override;
    procedure SaveToWriter(aWriter: TSynMacroEventWriter); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  public
    property Position: TPoint read fPosition write fPosition;
  end;

  { TSynDataEvent }

  TSynDataEvent = class(TSynBasicEvent)
  protected
    fData: Pointer;
  public
    procedure Initialize(aCmd: TSynEditorCommand;
      const aChar: TUTF8Char;
      aData: Pointer); override;
    procedure Assign(AnEvent: TSynMacroEvent); override;
    procedure LoadFromStream(aStream: TStream); override;
    procedure SaveToStream(aStream: TStream); override;
    procedure Playback(aEditor: TCustomSynEdit); override;
  end;

  TCustomSynMacroRecorder = class;

  TSynUserCommandEvent = procedure (aSender: TCustomSynMacroRecorder;
    aCmd: TSynEditorCommand; var aEvent: TSynMacroEvent) of object;

  { TCustomSynMacroRecorder
  OnStateChange:
    occurs right after start playing, recording, pausing or stopping
  SaveMarkerPos:
    if true, Bookmark position is recorded in the macro. Otherwise, the Bookmark
    is created in the position the Caret is at the time of playback.
  }

  TCustomSynMacroRecorder = class(TAbstractSynHookerPlugin)
  private
    fCommandIDs: array [TSynMacroCommand] of TSynEditorCommand;
    fCommandIDUserSet: array [TSynMacroCommand] of Boolean;
    fShortCuts: array [TSynMacroCommand] of TShortCut;
    fOnStateChange: TNotifyEvent;
    fOnUserCommand: TSynUserCommandEvent;
    fMacroName: string;
    fSaveMarkerPos: boolean;
    FStartPlayBack: boolean;
    FStopRequested: Boolean;
    function GetCommandIDs(Index: integer): TSynEditorCommand;
    function GetEvent(aIndex: integer): TSynMacroEvent;
    function GetEventCount: integer;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    procedure SetCommandIDs(AIndex: Integer; AValue: TSynEditorCommand);
    procedure HookEditorCmd(ACmd: TSynMacroCommand; AnOldShortCut: TShortCut = 0);
    procedure UnHookEditorCmd(ACmd: TSynMacroCommand);
  protected
    fCurrentEditor: TCustomSynEdit;
    fState: TSynMacroState;
    fEvents: TList;
    procedure SetMacroName(AValue: string); virtual;
    procedure SetShortCut(const Index: Integer; const Value: TShortCut);
    function GetIsEmpty: boolean;
    procedure StateChanged;
    procedure DoEditorAdded(aEditor: TCustomSynEdit); override;
    procedure DoEditorRemoving(aEditor: TCustomSynEdit); override;
    procedure OnCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand;
      var aChar: TUTF8Char;
      Data: pointer; HandlerData: pointer); override;
    // hcfInit for recording, so we get the unmodified command
    procedure OnPreCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand;
      var aChar: TUTF8Char;
      Data: pointer; HandlerData: pointer);
    // hcfFinish for playback, so there are no locks.
    // Locks interfere with selection (locked caret, no callback to selection)
    // and undo, does not work insid an UndoBlock
    procedure OnFinishCommand(Sender: TObject; AfterProcessing: boolean;
      var Handled: boolean; var Command: TSynEditorCommand;
      var aChar: TUTF8Char;
      Data: pointer; HandlerData: pointer);
  protected
    function CreateMacroEvent(aCmd: TSynEditorCommand): TSynMacroEvent; virtual;
    property RecordCommandID: TSynEditorCommand index ord(mcRecord) read GetCommandIDs write SetCommandIDs;
    property PlaybackCommandID: TSynEditorCommand index ord(mcPlayback) read GetCommandIDs write SetCommandIDs;
    function GetShortCuts(Cmd: integer): TShortCut;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure Error(const aMsg: String);
    procedure RecordMacro(aEditor: TCustomSynEdit);
    procedure PlaybackMacro(aEditor: TCustomSynEdit);
    procedure Stop;
    procedure Pause;
    procedure Resume;
    procedure AssignEventsFrom(AMacroRecorder: TCustomSynMacroRecorder);
    property IsEmpty: boolean read GetIsEmpty;
    property State: TSynMacroState read fState;
    procedure Clear;
    procedure AddEvent(aCmd: TSynEditorCommand; aChar: char; aData: pointer);
    procedure InsertEvent(aIndex: integer; aCmd: TSynEditorCommand; aChar: char;
      aData: pointer);
    procedure AddCustomEvent(aEvent: TSynMacroEvent);
    procedure InsertCustomEvent(aIndex: integer; aEvent: TSynMacroEvent);
    procedure DeleteEvent(aIndex: integer);
    procedure LoadFromStream(aSrc: TStream);
    procedure LoadFromStreamEx(aSrc: TStream; aClear: boolean);
    procedure SaveToStream(aDest: TStream);
    procedure LoadFromFile(aFilename : string);
    procedure SaveToFile(aFilename : string);
    property EventCount: integer read GetEventCount;
    property Events[aIndex: integer]: TSynMacroEvent read GetEvent;
    property CurrentEditor: TCustomSynEdit read fCurrentEditor; // while recording
    property RecordShortCut: TShortCut index Ord(mcRecord)
      read GetShortCuts write SetShortCut;
    property PlaybackShortCut: TShortCut index Ord(mcPlayback)
      read GetShortCuts write SetShortCut;
    property SaveMarkerPos: boolean read fSaveMarkerPos
      write fSaveMarkerPos default False;
    property AsString : string read GetAsString write SetAsString;
    property MacroName : string read fMacroName write SetMacroName;
    property OnStateChange: TNotifyEvent read fOnStateChange write fOnStateChange;
    property OnUserCommand: TSynUserCommandEvent read fOnUserCommand
      write fOnUserCommand;
  end;

  TSynMacroRecorder = class(TCustomSynMacroRecorder)
  public
    property RecordCommandID;
    property PlaybackCommandID;
  published
    property SaveMarkerPos;
    property RecordShortCut;
    property PlaybackShortCut;
    property OnStateChange;
    property OnUserCommand;
    property Editor;
  end;

implementation

{ TSynIgnoredEvent }

function TSynIgnoredEvent.GetAsString: string;
begin
  Result := '';
end;

procedure TSynIgnoredEvent.InitEventParameters(aStr: string);
begin
  //
end;

procedure TSynIgnoredEvent.Initialize(aCmd: TSynEditorCommand; const aChar: TUTF8Char;
  aData: Pointer);
begin
  //
end;

procedure TSynIgnoredEvent.LoadFromStream(aStream: TStream);
begin
  //
end;

procedure TSynIgnoredEvent.SaveToStream(aStream: TStream);
begin
  //
end;

procedure TSynIgnoredEvent.LoadFromReader(aReader: TSynMacroEventReader);
begin
  //
end;

procedure TSynIgnoredEvent.SaveToWriter(aWriter: TSynMacroEventWriter);
begin
  aWriter.WriteEventCommand(ecNone);
end;

procedure TSynIgnoredEvent.Playback(aEditor: TCustomSynEdit);
begin
  //
end;

{ TSynDataEvent }

procedure TSynDataEvent.Initialize(aCmd: TSynEditorCommand; const aChar: TUTF8Char;
  aData: Pointer);
begin
  fCommand := aCmd;
  Assert( aChar = #0 );
  fData := aData; // Todo: must be copied...
end;

procedure TSynDataEvent.Assign(AnEvent: TSynMacroEvent);
begin
  inherited Assign(AnEvent);
  // Todo: COPY data.
end;

procedure TSynDataEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read( fData, SizeOf(fData) );
end;

procedure TSynDataEvent.Playback(aEditor: TCustomSynEdit);
begin
  aEditor.CommandProcessor( Command, #0, fData );
end;

procedure TSynDataEvent.SaveToStream(aStream: TStream);
begin
  inherited;
  aStream.Write( fData, SizeOf(fData) );
end;

{ TCustomSynMacroRecorder }

procedure TCustomSynMacroRecorder.AddCustomEvent(aEvent: TSynMacroEvent);
begin
  InsertCustomEvent( EventCount, aEvent );
end;

procedure TCustomSynMacroRecorder.AddEvent(aCmd: TSynEditorCommand;
  aChar: char; aData: pointer);
begin
  InsertEvent( EventCount, aCmd, aChar, aData );
end;

procedure TCustomSynMacroRecorder.Clear;
var
  I: Integer;
  Obj: TObject;
begin
  if Assigned(fEvents) then
  begin
    for I := fEvents.Count-1 downto 0 do
    begin
      Obj := TObject(fEvents[I]);
      fEvents.Delete(I);
      Obj.Free;
    end;
    FreeAndNil( fEvents );
  end;
end;

constructor TCustomSynMacroRecorder.Create(aOwner: TComponent);
begin
  inherited;
  fMacroName := 'unnamed';
  fCommandIDs[mcRecord] := ecNone;
  fCommandIDs[mcPlayback] := ecNone;
  fShortCuts[mcRecord] := Menus.ShortCut( Ord('R'), [ssCtrl, ssShift] );
  fShortCuts[mcPlayback] := Menus.ShortCut( Ord('P'), [ssCtrl, ssShift] );
end;

function TCustomSynMacroRecorder.CreateMacroEvent(aCmd: TSynEditorCommand): TSynMacroEvent;

  function WantDefaultEvent(var aEvent: TSynMacroEvent): boolean;
  begin
    if Assigned( OnUserCommand ) then
      OnUserCommand( Self, aCmd, aEvent );
    Result := aEvent = nil;
  end;

begin
  case aCmd of
    ecNone:
      Result := TSynIgnoredEvent.Create;
    ecGotoXY, ecSelGotoXY, ecSetMarker0..ecSetMarker9:
      begin
        Result := TSynPositionEvent.Create;
        TSynPositionEvent(Result).Command := aCmd;
      end;
    ecChar:
      Result := TSynCharEvent.Create;
    ecString:
      Result := TSynStringEvent.Create;
    else begin
      Result := nil;
      if (aCmd < ecUserFirst) or WantDefaultEvent( Result ) then
      begin
        Result := TSynBasicEvent.Create;
        TSynBasicEvent(Result).Command := aCmd;
      end;
    end;
  end;
end;

function TCustomSynMacroRecorder.GetShortCuts(Cmd: integer): TShortCut;
begin
  Result:=fShortCuts[TSynMacroCommand(Cmd)];
end;

procedure TCustomSynMacroRecorder.DeleteEvent(aIndex: integer);
var
  iObj: Pointer;
begin
  iObj := fEvents[ aIndex ];
  fEvents.Delete( aIndex );
  TObject( iObj ).Free;
end;

destructor TCustomSynMacroRecorder.Destroy;
begin
  Clear;
  SetCommandIDs(ord(mcRecord), ecNone);
  SetCommandIDs(ord(mcPlayback), ecNone);
  inherited;
end;

procedure TCustomSynMacroRecorder.DoEditorAdded(aEditor: TCustomSynEdit);
begin
  if (RecordCommandID <> ecNone) and (RecordShortCut <> 0) then
    HookEditor( aEditor, RecordCommandID, 0, RecordShortCut, []);
  if (PlaybackCommandID <> ecNone) and (PlaybackShortCut <> 0) then
    HookEditor( aEditor, PlaybackCommandID, 0, PlaybackShortCut, []);

  aEditor.RegisterCommandHandler( @OnCommand, Self, [hcfPreExec]);
  aEditor.RegisterCommandHandler( @OnPreCommand, Self, [hcfInit]);
  aEditor.RegisterCommandHandler( @OnFinishCommand, Self, [hcfFinish]);
end;

procedure TCustomSynMacroRecorder.DoEditorRemoving(aEditor: TCustomSynEdit);
begin
  if RecordCommandID <> ecNone then
    UnHookEditor( aEditor, RecordCommandID, RecordShortCut );
  if PlaybackCommandID <> ecNone then
    UnHookEditor( aEditor, PlaybackCommandID, PlaybackShortCut );

  aEditor.UnregisterCommandHandler( @OnCommand);
  aEditor.UnregisterCommandHandler( @OnPreCommand);
  aEditor.UnregisterCommandHandler( @OnFinishCommand);
end;

procedure TCustomSynMacroRecorder.Error(const aMsg: String);
begin
  raise Exception.Create(aMsg);
end;

function TCustomSynMacroRecorder.GetEvent(aIndex: integer): TSynMacroEvent;
begin
  Result := TSynMacroEvent( fEvents[aIndex] );
end;

function TCustomSynMacroRecorder.GetCommandIDs(Index: integer
  ): TSynEditorCommand;
begin
  Result:=fCommandIDs[TSynMacroCommand(Index)];
  if Result = ecNone then begin
    Result := AllocatePluginKeyRange(1);
    fCommandIDs[TSynMacroCommand(Index)] := Result;
    fCommandIDUserSet[TSynMacroCommand(Index)] := False;
  end;
end;

function TCustomSynMacroRecorder.GetEventCount: integer;
begin
  if fEvents = nil then
    Result := 0
  else
    Result := fEvents.Count;
end;

function TCustomSynMacroRecorder.GetIsEmpty: boolean;
begin
  Result := (fEvents = nil) or (fEvents.Count = 0);
end;

procedure TCustomSynMacroRecorder.InsertCustomEvent(aIndex: integer;
  aEvent: TSynMacroEvent);
begin
  if fEvents = nil then
    fEvents := TList.Create;
  fEvents.Insert( aIndex, aEvent );
end;

procedure TCustomSynMacroRecorder.InsertEvent(aIndex: integer;
  aCmd: TSynEditorCommand; aChar: char; aData: pointer);
var
  iEvent: TSynMacroEvent;
begin
  iEvent := CreateMacroEvent( aCmd );
  if iEvent is TSynSkippedEvent then begin
    iEvent.Free;
    exit;
  end;
  try
    iEvent.Initialize( aCmd, aChar, aData );
    InsertCustomEvent( aIndex, iEvent );
  except
    iEvent.Free;
    raise;
  end;
end;

procedure TCustomSynMacroRecorder.LoadFromStream(aSrc: TStream);
begin
  LoadFromStreamEx( aSrc, True );
end;

procedure TCustomSynMacroRecorder.LoadFromStreamEx(aSrc: TStream;
  aClear: boolean);
var
  iCommand : TSynEditorCommand;
  iEvent: TSynMacroEvent;
  cnt, i : Integer;
begin
  Stop;
  if aClear then
    Clear;
  fEvents := TList.Create;
  cnt := 0;
  aSrc.Read(cnt, sizeof(cnt));
  i := 0;
  fEvents.Capacity := aSrc.Size div SizeOf( TSynEditorCommand );
  while (aSrc.Position < aSrc.Size) and (i < cnt) do
  begin
    iCommand := 0;
    aSrc.Read( iCommand, SizeOf(TSynEditorCommand) );
    iEvent := CreateMacroEvent( iCommand );
    if iEvent is TSynSkippedEvent then begin
      iEvent.Free;
    end
    else begin
      iEvent.Initialize( iCommand, #0, nil );
      iEvent.LoadFromStream( aSrc );
      fEvents.Add( iEvent );
    end;
    Inc(i);
  end;
end;

procedure TCustomSynMacroRecorder.OnCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var aChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
begin
  FStartPlayBack := False;
  case State of
    msStopped:
      if Command = RecordCommandID then
      begin
        RecordMacro( TCustomSynEdit( Sender ) );
        Handled := True;
      end
      else if Command = PlaybackCommandID then
      begin
        FStartPlayBack := True;
        Handled := True;
      end;
    msPlaying:
      ;
    msPaused:
      if Command = PlaybackCommandID then
      begin
        Resume;
        Handled := True;
      end;
    msRecording:
      if Command = PlaybackCommandID then
      begin
        Pause;
        Handled := True;
      end
      else if Command = RecordCommandID then
      begin
        Stop;
        Handled := True;
      end;
  end;
end;

procedure TCustomSynMacroRecorder.OnPreCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var aChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
var
  iEvent: TSynMacroEvent;
begin
  if (Command = PlaybackCommandID) or (Command = RecordCommandID) then
    exit;

  if (Sender = fCurrentEditor) and (State = msRecording) and (Command <> ecNone) then
  begin
    iEvent := CreateMacroEvent( Command );
    if iEvent is TSynSkippedEvent then begin
      iEvent.Free;
      exit;
    end;
    iEvent.Initialize( Command, aChar, Data );
    fEvents.Add( iEvent );
    if SaveMarkerPos and (Command >= ecSetMarker0) and
      (Command <= ecSetMarker9) and (Data = nil) then
    begin
      TSynPositionEvent(iEvent).Position := fCurrentEditor.CaretXY;
    end;
  end;
end;

procedure TCustomSynMacroRecorder.OnFinishCommand(Sender: TObject; AfterProcessing: boolean;
  var Handled: boolean; var Command: TSynEditorCommand; var aChar: TUTF8Char; Data: pointer;
  HandlerData: pointer);
begin
  if not FStartPlayBack then exit;
  FStartPlayBack := False;
  PlaybackMacro( TCustomSynEdit( Sender ) );
end;

procedure TCustomSynMacroRecorder.Pause;
begin
  if State <> msRecording then
    Error( sCannotPause );
  fState := msPaused;
  StateChanged;
end;

procedure TCustomSynMacroRecorder.PlaybackMacro(aEditor: TCustomSynEdit);
var
  cEvent: integer;
begin
  if State <> msStopped then
    Error( sCannotPlay );
  FStopRequested := False;
  fState := msPlaying;
  try
    StateChanged;
    for cEvent := 0 to EventCount -1 do begin
      Events[ cEvent ].Playback( aEditor );
      if FStopRequested then break;
    end;
  finally
    fState := msStopped;
    FStopRequested := False;
    StateChanged;
  end;
end;

procedure TCustomSynMacroRecorder.RecordMacro(aEditor: TCustomSynEdit);
begin
  if fState <> msStopped then
    Error( sCannotRecord );
  Clear;
  fEvents := TList.Create;
  fEvents.Capacity := 512;
  fState := msRecording;
  fCurrentEditor := aEditor;
  StateChanged;
end;

procedure TCustomSynMacroRecorder.Resume;
begin
  if fState <> msPaused then
    Error( sCannotResume );
  fState := msRecording;
  StateChanged;
end;

procedure TCustomSynMacroRecorder.AssignEventsFrom(AMacroRecorder: TCustomSynMacroRecorder);
var
  i: Integer;
  NewEvent: TSynMacroEvent;
begin
  Clear;
  if (AMacroRecorder = nil) or (AMacroRecorder.fEvents = nil) then exit;

  fEvents := TList.Create;
  for i := 0 to AMacroRecorder.fEvents.Count -1 do begin
    NewEvent := TSynMacroEventClass(TSynMacroEvent(AMacroRecorder.fEvents[i]).ClassType).Create;
    NewEvent.Assign(TSynMacroEvent(AMacroRecorder.fEvents[i]));
    fEvents.add(NewEvent);
  end;
end;

procedure TCustomSynMacroRecorder.SaveToStream(aDest: TStream);
var
  cEvent, eCnt : integer;
begin
  eCnt := EventCount;
  aDest.Write(eCnt, sizeof(eCnt));
  for cEvent := 0 to eCnt -1 do
    Events[ cEvent ].SaveToStream( aDest );
end;

procedure TCustomSynMacroRecorder.SetShortCut(const Index: Integer;
  const Value: TShortCut);
begin
  if fShortCuts[TSynMacroCommand(Index)] <> Value then
  begin
    if Value <> 0 then
      HookEditorCmd(TSynMacroCommand(Index), fShortCuts[TSynMacroCommand(Index)])
    else
      UnHookEditorCmd(TSynMacroCommand(Index));
    fShortCuts[TSynMacroCommand(Index)] := Value;
  end;
end;

procedure TCustomSynMacroRecorder.StateChanged;
begin
  if Assigned( OnStateChange ) then
    OnStateChange( Self );
end;

procedure TCustomSynMacroRecorder.Stop;
begin
  if fState = msStopped then
    Exit;
  if fState = msPlaying then begin
    FStopRequested := True;
    Exit;
  end;
  fState := msStopped;
  fCurrentEditor := nil;
  if fEvents.Count = 0 then
    FreeAndNil( fEvents );
  StateChanged;
end;

function TCustomSynMacroRecorder.GetAsString: string;
var
  i : integer;
  eStr : string;
begin
  Result := 'macro ' + MacroName + #13#10 + 'begin' + #13#10;
  if Assigned(fEvents) then
  begin
    for i := 0 to fEvents.Count -1 do
    begin
      eStr := Events[i].AsString;
      if eStr <> '' then
        Result := Result + '  '  + eStr + #13#10;
    end;
  end;
  Result := Result + 'end';
end;

procedure TCustomSynMacroRecorder.SetAsString(const Value: string);
var
  i, p, Cmd : Integer;
  S : TStrings;
  cmdStr : string;
  iEvent: TSynMacroEvent;
begin
  Stop;
  Clear;
  fEvents := TList.Create;
  // process file line by line and create events
  S := TStringList.Create;
  try
    S.Text := Value;
    for i := 0 to S.Count - 1 do
    begin
      cmdStr := Trim(S[i]);
      p := Pos(' ', cmdStr);
      if p = 0 then p := Length(cmdStr)+1;
      Cmd := ecNone;
      if IdentToEditorCommand(Copy(cmdStr, 1, p-1), Longint(Cmd)) then  // D2 needs type-cast
      begin
        Delete(cmdStr, 1, p);
        iEvent := CreateMacroEvent(TSynEditorCommand(Cmd));
        if iEvent is TSynSkippedEvent then begin
          iEvent.Free;
        end
        else begin
          try
            fEvents.Add(iEvent);
            iEvent.InitEventParameters(cmdStr);
          except
            iEvent.Free;
          end;
        end;
      end;
    end;
  finally
    S.Free;
  end;
end;

procedure TCustomSynMacroRecorder.SetCommandIDs(AIndex: Integer; AValue: TSynEditorCommand);
begin
  if (fCommandIDs[TSynMacroCommand(AIndex)] = AValue) and
     (fCommandIDUserSet[TSynMacroCommand(AIndex)] or (AValue = ecNone))
  then
    exit;

  UnHookEditorCmd(TSynMacroCommand(AIndex));

  //if (fCommandIDs[TSynMacroCommand(Index)] <> ecNone) and
  //   (not fCommandIDUserSet[TSynMacroCommand(Index)])
  //then
  //  ReleasePluginCommand(fCommandIDs[TSynMacroCommand(Index)]);

  fCommandIDs[TSynMacroCommand(AIndex)] := AValue;
  fCommandIDUserSet[TSynMacroCommand(AIndex)] := AValue <> ecNone;

  HookEditorCmd(TSynMacroCommand(AIndex));
end;

procedure TCustomSynMacroRecorder.HookEditorCmd(ACmd: TSynMacroCommand;
  AnOldShortCut: TShortCut);
var
  cEditor: Integer;
  c: TSynEditorCommand;
begin
  c := GetCommandIDs(ord(ACmd));
  if (c = ecNone) then
    exit;

  if fShortCuts[ACmd] = 0 then begin
    UnHookEditorCmd(ACmd);
    exit;
  end;

  for cEditor := 0 to EditorCount -1 do
    HookEditor(Editors[cEditor], c, AnOldShortCut, fShortCuts[ACmd], []);
end;

procedure TCustomSynMacroRecorder.SetMacroName(AValue: string);
begin
  if fMacroName = AValue then Exit;
  fMacroName := AValue;
end;

procedure TCustomSynMacroRecorder.UnHookEditorCmd(ACmd: TSynMacroCommand);
var
  c: TSynEditorCommand;
  cEditor: Integer;
begin
  c := GetCommandIDs(ord(ACmd));
  if (c = ecNone) then
    exit;

  for cEditor := 0 to EditorCount -1 do
    UnHookEditor(Editors[cEditor], c, fShortCuts[ACmd]);
end;

procedure TCustomSynMacroRecorder.LoadFromFile(aFilename: string);
var
  F : TFileStream;
begin
  F := TFileStreamUTF8.Create(aFilename, fmOpenRead);
  try
    LoadFromStream(F);
    MacroName := ChangeFileExt(ExtractFileName(aFilename), '');
  finally
    F.Free;
  end;
end;

procedure TCustomSynMacroRecorder.SaveToFile(aFilename: string);
var
  F : TFileStream;
begin
  F := TFileStreamUTF8.Create(aFilename, fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

{ TSynBasicEvent }

function TSynBasicEvent.GetAsString: string;
begin
  Result := '';
  EditorCommandToIdent(Command, Result);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynBasicEvent.InitEventParameters(aStr: string);
begin
  // basic events have no parameters but can contain an optional repeat count
  RepeatCount := StrToIntDef(Trim(aStr), 1);
end;

procedure TSynBasicEvent.Initialize(aCmd: TSynEditorCommand; const aChar: TUTF8Char;
  aData: Pointer);
begin
  Command := aCmd;
{$IFDEF SYN_DEVELOPMENT_CHECKS}
  if (aChar <> #0) or (aData <> nil) then
    raise Exception.Create('TSynBasicEvent cannot handle Char <> #0 or Data <> nil');
{$ENDIF}
end;

procedure TSynBasicEvent.Assign(AnEvent: TSynMacroEvent);
begin
  inherited Assign(AnEvent);
  if AnEvent is TSynBasicEvent then
    fCommand := TSynBasicEvent(AnEvent).fCommand;
end;

procedure TSynBasicEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read( fRepeatCount, SizeOf(fRepeatCount) );
end;

procedure TSynBasicEvent.Playback(aEditor: TCustomSynEdit);
var
  i : Integer;
begin
  for i := 1 to RepeatCount do
    aEditor.CommandProcessor( Command, #0, nil );
end;

procedure TSynBasicEvent.SaveToStream(aStream: TStream);
begin
  aStream.Write( Command, SizeOf(TSynEditorCommand) );
  aStream.Write( RepeatCount, SizeOf(RepeatCount) );
end;

procedure TSynBasicEvent.LoadFromReader(aReader: TSynMacroEventReader);
begin
  fCommand := aReader.EventCommand;
  fRepeatCount := 1;
  if aReader.ParamCount > 0 then
    fRepeatCount := aReader.GetParamAsInt(0);
end;

procedure TSynBasicEvent.SaveToWriter(aWriter: TSynMacroEventWriter);
begin
  aWriter.WriteEventCommand(fCommand);
  if fRepeatCount <> 1 then
    aWriter.WriteEventParam(fRepeatCount);
end;

{ TSynCharEvent }

function TSynCharEvent.GetAsString: string;
begin
  Result := '';
  EditorCommandToIdent(ecChar, Result);
  Result := Result + ' ' + Key;
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynCharEvent.InitEventParameters(aStr: string);
begin
  // aStr should be a Key value one character in length
  // with an optional repeat count whitespace separated
  if Length(aStr) >= 1 then
    Key := aStr[1]
  else
    Key := ' ';
  Delete(aStr, 1, 1); // if possible delete the first character
  RepeatCount := StrToIntDef(Trim(aStr), 1);
end;

procedure TSynCharEvent.Initialize(aCmd: TSynEditorCommand; const aChar: TUTF8Char;
  aData: Pointer);
begin
  Key := aChar;
  Assert( aData = nil );
end;

procedure TSynCharEvent.Assign(AnEvent: TSynMacroEvent);
begin
  inherited Assign(AnEvent);
  if AnEvent is TSynCharEvent then
    fKey := TSynCharEvent(AnEvent).fKey;
end;

procedure TSynCharEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read( fKey, SizeOf(Key) );
  aStream.Read( fRepeatCount, SizeOf(fRepeatCount) );
end;

procedure TSynCharEvent.Playback(aEditor: TCustomSynEdit);
var
  i : Integer;
begin
  for i := 1 to RepeatCount do
    aEditor.CommandProcessor( ecChar, Key, nil );
end;

procedure TSynCharEvent.SaveToStream(aStream: TStream);
const
  iCharCommand: TSynEditorCommand = ecChar;
begin
  aStream.Write( iCharCommand, SizeOf(TSynEditorCommand) );
  aStream.Write( Key, SizeOf(Key) );
  aStream.Write( RepeatCount, SizeOf(RepeatCount) );
end;

procedure TSynCharEvent.LoadFromReader(aReader: TSynMacroEventReader);
begin
  fKey := aReader.GetParamAsString(0);
  fRepeatCount := 1;
  if aReader.ParamCount > 1 then
    fRepeatCount := aReader.GetParamAsInt(1);
end;

procedure TSynCharEvent.SaveToWriter(aWriter: TSynMacroEventWriter);
begin
  aWriter.WriteEventCommand(ecChar);
  aWriter.WriteEventParam(fKey);
  if fRepeatCount <> 1 then
    aWriter.WriteEventParam(fRepeatCount);
end;

{ TSynPositionEvent }

function TSynPositionEvent.GetAsString: string;
begin
  Result := inherited GetAsString;
  // add position data here
  Result := Result + Format(' (%d, %d)', [Position.x, Position.y]);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynPositionEvent.InitEventParameters(aStr: string);
var
  i, o, c, x, y : Integer;
  valStr : string;
begin
  inherited;
  // aStr should be (x, y) with optional repeat count whitespace separated
  aStr := Trim(aStr);
  i := Pos(',', aStr);
  o := Pos('(', aStr);
  c := Pos(')', aStr);
  if (not ((i = 0) or (o = 0) or (c = 0))) and
     ((i > o) and (i < c)) then
  begin
    valStr := Copy(aStr, o+1, i-o-1);
    x := StrToIntDef(valStr, 1);
    Delete(aStr, 1, i);
    aStr := Trim(aStr);
    c := Pos(')', aStr);
    valStr := Copy(aStr, 1, c-1);
    y := StrToIntDef(valStr, 1);
    Position := Point(x, y);
    Delete(aStr, 1, c);
    aStr := Trim(aStr);
    RepeatCount := StrToIntDef(aStr, 1);
  end;
end;

procedure TSynPositionEvent.Initialize(aCmd: TSynEditorCommand; const aChar: TUTF8Char;
  aData: Pointer);
begin
  inherited;
  if aData <> nil then
    Position := PPoint( aData )^
  else
    Position := Point( 0, 0 );
end;

procedure TSynPositionEvent.Assign(AnEvent: TSynMacroEvent);
begin
  inherited Assign(AnEvent);
  if AnEvent is TSynPositionEvent then
    fPosition := TSynPositionEvent(AnEvent).fPosition;
end;

procedure TSynPositionEvent.LoadFromStream(aStream: TStream);
begin
  aStream.Read( fPosition, SizeOf(Position) );
end;

procedure TSynPositionEvent.Playback(aEditor: TCustomSynEdit);
begin
  if (Position.x <> 0) or (Position.y <> 0) then
    aEditor.CommandProcessor( Command, #0, @Position )
  else
    aEditor.CommandProcessor( Command, #0, nil );
end;

procedure TSynPositionEvent.SaveToStream(aStream: TStream);
begin
  inherited;
  aStream.Write( Position, SizeOf(Position) );
end;

procedure TSynPositionEvent.LoadFromReader(aReader: TSynMacroEventReader);
begin
  fCommand := aReader.EventCommand;
  fPosition.x := aReader.GetParamAsInt(0);
  fPosition.y := aReader.GetParamAsInt(1);
  fRepeatCount := 1;
  if aReader.ParamCount > 2 then
    fRepeatCount := aReader.GetParamAsInt(2);
end;

procedure TSynPositionEvent.SaveToWriter(aWriter: TSynMacroEventWriter);
begin
  aWriter.WriteEventCommand(fCommand);
  aWriter.WriteEventParam(fPosition.x);
  aWriter.WriteEventParam(fPosition.y);
  if fRepeatCount <> 1 then
    aWriter.WriteEventParam(fRepeatCount);
end;

{ TSynStringEvent }

function QuotedStr(const S: string; QuoteChar: Char): string;
var
  i: Integer;
begin
  Result := S;
  for i := Length(Result) downto 1 do
    if Result[i] = QuoteChar then
      Insert(QuoteChar, Result, i);
  Result := QuoteChar + Result + QuoteChar;
end;

function TSynStringEvent.GetAsString: string;
begin
  Result := '';
  EditorCommandToIdent(ecString, Result);
  Result := Result + ' ' + AnsiQuotedStr(Value, #39);
  if RepeatCount > 1 then
    Result := Result + ' ' + IntToStr(RepeatCount);
end;

procedure TSynStringEvent.InitEventParameters(aStr: string);
var
  o, c : Integer;
  valStr : string;
begin
  // aStr = 'test' with optional whitespace separated repeat count
  o := Pos('''', aStr);
  c := LastDelimiter('''', aStr);
  valStr := Copy(aStr, o+1, c-o-1);
  Value := StringReplace(valStr, '''''', '''', [rfReplaceAll]);
  Delete(aStr, 1, c);
  RepeatCount := StrToIntDef(Trim(aStr), 1);
end;

procedure TSynStringEvent.Initialize(aCmd: TSynEditorCommand; const aChar: TUTF8Char;
  aData: Pointer);
begin
  Value := String(aData);
end;

procedure TSynStringEvent.Assign(AnEvent: TSynMacroEvent);
begin
  inherited Assign(AnEvent);
  if AnEvent is TSynStringEvent then
    fString := TSynStringEvent(AnEvent).fString;
end;

procedure TSynStringEvent.LoadFromStream(aStream: TStream);
var
  l : Integer;
  Buff : PChar;
begin
  l:=0;
  aStream.Read(l, SizeOf(l));
  GetMem(Buff, l);
  try
    FillChar(Buff^,l,0);
    aStream.Read(Buff^, l);
    fString := Buff;
  finally
    FreeMem(Buff);
  end;
  aStream.Read( fRepeatCount, SizeOf(fRepeatCount) );
end;

procedure TSynStringEvent.Playback(aEditor: TCustomSynEdit);
var
  i, j : Integer;
begin
  for j := 1 to RepeatCount do
  begin
//    aEditor.CommandProcessor( ecString, #0, Pointer(Value) );
    // SynEdit doesn't actually support the ecString command so we convert
    // it into ecChar commands
    for i := 1 to Length(Value) do
      aEditor.CommandProcessor(ecChar, Value[i], nil);
  end;
end;

procedure TSynStringEvent.SaveToStream(aStream: TStream);
const
  StrCommand: TSynEditorCommand = ecString;
var
  l : Integer;
  Buff : PChar;
begin
  aStream.Write(StrCommand, SizeOf(StrCommand));
  l := Length(Value) + 1;
  aStream.Write(l, sizeof(l));
  GetMem(Buff, l);
  try
    FillChar(Buff^,l,0);
    StrPCopy(Buff, Value);
    aStream.Write(Buff^, l);
  finally
    FreeMem(Buff);
  end;
  aStream.Write( RepeatCount, SizeOf(RepeatCount) );
end;

procedure TSynStringEvent.LoadFromReader(aReader: TSynMacroEventReader);
begin
  fString := aReader.GetParamAsString(0);
  fRepeatCount := 1;
  if aReader.ParamCount > 1 then
    fRepeatCount := aReader.GetParamAsInt(1);
end;

procedure TSynStringEvent.SaveToWriter(aWriter: TSynMacroEventWriter);
begin
  aWriter.WriteEventCommand(ecString);
  aWriter.WriteEventParam(fString);
  if fRepeatCount <> 1 then
    aWriter.WriteEventParam(fRepeatCount);
end;

{ TSynMacroEvent }

constructor TSynMacroEvent.Create;
begin
  inherited Create;
  fRepeatCount := 1;
end;

procedure TSynMacroEvent.Assign(AnEvent: TSynMacroEvent);
begin
  fRepeatCount := AnEvent.fRepeatCount;
end;

end.
