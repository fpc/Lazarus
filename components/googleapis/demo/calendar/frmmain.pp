unit frmmain;

{$mode objfpc}{$H+}

// Define USESYNAPSE if you want to force use of synapse
{ $DEFINE USESYNAPSE}

// For version 2.6.4, synapse is the only option.
{$IFDEF VER2_6}
{$DEFINE USESYNAPSE}
{$ENDIF}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, synautil, IniFiles, googlebase, googleservice, googleclient,
  googlecalendar, opensslsockets;

type

  { TMainForm }
  TAccessTokenState = (acsWaiting,acsOK,acsCancel);

  TMainForm = class(TForm)
    BCancel: TButton;
    BSetAccess: TButton;
    BFetchCalendars: TButton;
    BFetchEvents: TButton;
    EAccessCode: TEdit;
    GBAccess: TGroupBox;
    LEvents: TLabel;
    LEAccess: TLabel;
    LBCalendars: TListBox;
    SGEvents: TStringGrid;
    procedure BCancelClick(Sender: TObject);
    procedure BFetchEventsClick(Sender: TObject);
    procedure BSetAccessClick(Sender: TObject);
    procedure BFetchCalendarsClick(Sender: TObject);
    Procedure DoUserConsent(Const AURL : String; Out AAuthCode : String) ;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LBCalendarsSelectionChange(Sender: TObject; User: boolean);
  private
    { private declarations }
    FAccessState : TAccessTokenState;
    FClient : TGoogleClient;

    FCalendarAPI: TCalendarAPI;
    calendarList: TCalendarList;
    FCurrentCalendar : TCalendarListEntry;
    events : TEvents;
    procedure LoadAuthConfig;
    procedure SaveRefreshToken;
  public
    { public declarations }
  end;

var
  MainForm: TMainForm;

implementation


uses
  {$ifdef windows}windows,{$endif}
  jsonparser, // needed
  fpoauth2,
  lclintf,
{$IFDEF USESYNAPSE}
  ssl_openssl,
  synapsewebclient
{$ELSE}
  fphttpwebclient
{$ENDIF}
  ;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  // Register calendar resources.
  TCalendarAPI.RegisterAPIResources;
  // Set up google client.
  FClient:=TGoogleClient.Create(Self);
{$IFDEF USESYNAPSE}
  FClient.WebClient:=TSynapseWebClient.Create(Self);
{$ELSE}
  FClient.WebClient:=TFPHTTPWebClient.Create(Self);
{$ENDIF}
  FClient.WebClient.RequestSigner:=FClient.AuthHandler;
  FClient.WebClient.LogFile:='requests.log';
  FClient.AuthHandler.WebClient:=FClient.WebClient;
  FClient.AuthHandler.Config.AccessType:=atOffLine;
  // We want to enter a code.
  FClient.OnUserConsent:=@DoUserConsent;
  // Create a calendar API and connect it to the client.
  FCalendarAPI:=TCalendarAPI.Create(Self);
  FCalendarAPI.GoogleClient:=FClient;
  // Load configuration
  LoadAuthConfig;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(CalendarList);
  FreeAndNil(Events);
end;

procedure TMainForm.LBCalendarsSelectionChange(Sender: TObject; User: boolean);
begin
  BFetchEvents.Enabled:=User and (LBCalendars.ItemIndex<>-1);
  if BFetchEvents.Enabled then
    begin
    FCurrentCalendar:=LBCalendars.Items.Objects[LBCalendars.ItemIndex] as TCalendarListEntry;
    if (FCurrentCalendar.Summary<>'') then
      LEvents.Caption:='Events for calendar : '+FCurrentCalendar.Summary
    else
      LEvents.Caption:='Events for calendar : '+FCurrentCalendar.ID;
    end
  else
    begin
    LEvents.Caption:='Events for calendar : <select a calendar>';
    SGEvents.RowCount:=1;
    FCurrentCalendar:=Nil;
    end;

end;

procedure TMainForm.LoadAuthConfig;

Var
  ini:TIniFile;

begin
  ini:=TIniFile.Create('google.ini');
  try
    With FClient.AuthHandler.Config,Ini do
      begin
      // Registered application needs calendar scope
      ClientID:=ReadString('Credentials','ClientID','');
      ClientSecret:=ReadString('Credentials','ClientSecret','');
      AuthScope:=ReadString('Credentials','Scope',
                            'https://www.googleapis.com/auth/calendar');
      // We are offline.
      RedirectUri:='urn:ietf:wg:oauth:2.0:oob';
      end;
    With FClient.AuthHandler.Session,Ini do
      begin
      // Session data
      RefreshToken:=ReadString('Session','RefreshToken','');
      AccessToken:=ReadString('Session','AccesToken','');
      AuthTokenType:=ReadString('Session','TokenType','');
      AuthExpires:=ReadDateTime('Session','AuthExpires',0);
      AuthExpiryPeriod:=ReadInteger('Session','AuthPeriod',0);
      end;
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.SaveRefreshToken;

Var
  ini:TIniFile;

begin
  // We save the refresh token for later use.
  With FClient.AuthHandler.Session do
  if RefreshToken<>'' then
    begin
    ini:=TIniFile.Create('google.ini');
    try
      With ini do
        begin
        WriteString('Session','RefreshToken',RefreshToken);
        WriteString('Session','AccessToken',AccessToken);
        WriteString('Session','TokenType',AuthTokenType);
        WriteDateTime('Session','AuthExpires',AuthExpires);
        WriteInteger('Session','AuthPeriod',AuthExpiryPeriod);
        end;
    finally
      Ini.Free;
    end;
    end;
end;

procedure TMainForm.BFetchCalendarsClick(Sender: TObject);

var
  Entry: TCalendarListEntry;
  Resource : TCalendarListResource;
  EN : String;
  i:integer;

begin
  LBCalendars.Items.Clear;
  FreeAndNil(CalendarList);
  Resource:=Nil;
  try
    Resource:=FCalendarAPI.CreateCalendarListResource;
    CalendarList:=Resource.list('');
    SaveRefreshToken;
    I:=0;
    if assigned(calendarList) then
      for Entry in calendarList.items do
        begin
        Inc(i);
        EN:=Entry.Summary;
        if EN='' then
          EN:=Entry.id+' ('+Entry.description+')';
        LBCalendars.Items.AddObject(IntToStr(i)+': '+EN,Entry);
        end;
     BFetchEvents.Enabled:=LBCalendars.Items.Count>0;
  finally
    FreeAndNil(Resource);
  end;
end;

procedure TMainForm.BSetAccessClick(Sender: TObject);
begin
  FAccessState:=acsOK;
  GBAccess.Visible:=False;
end;

procedure TMainForm.BCancelClick(Sender: TObject);
begin
  FAccessState:=acsCancel;
  GBAccess.Visible:=False;
end;

procedure TMainForm.BFetchEventsClick(Sender: TObject);
var
  Entry: TEvent;
  EN : String;
  i:integer;
  pageToken: String;
  SD, ED, TS: String; //Startdate, Enddate, Timestamp

  function prepareDate(EVDate: TEventDateTime):String;
  begin
     if EVDate.date<>0 then
       prepareDate:=DateToStr(EVDate.date)
     else if EVDate.dateTime<>0 then
       prepareDate:=DateTimeToStr(EVDate.datetime)
     else
       prepareDate:='(unspecified)';
  end;

  function prepareTS(EVDate: TEventDateTime):String;
  begin
     if EVDate.date<>0 then
       prepareTS:=Format('%.*d',[12,TimeStampToMSecs(DateTimeToTimeStamp(EVDate.date)) div 1000])
     else if EVDate.dateTime<>0 then
       prepareTS:=Format('%.*d',[12,TimeStampToMSecs(DateTimeToTimeStamp(EVDate.dateTime)) div 1000])
     else
       prepareTS:='(unspecified)';
  end;


begin
  if LBCalendars.ItemIndex<0 then
     Exit;
  BFetchEvents.Enabled:=False;
  MainForm.Cursor:=crHourGlass;

  SGEvents.RowCount:=1;
  FreeAndNil(Events);
  SaveRefreshToken;
  I:=0;
  pageToken := '';
  repeat
     SD:='(unspecified)';
     ED:='(unspecified)';
     TS:='(unspecified)';
     Events:=FCalendarAPI.EventsResource.list(FCurrentCalendar.id,'pageToken='+pageToken);
     if assigned(Events) then begin
       pageToken:=Events.nextPageToken;
       for Entry in Events.items do
        begin
        Inc(i);
        if Assigned(Entry.start) then begin
          SD:=prepareDate(Entry.start);
          TS:=prepareTS(Entry.start);
        end;

        if Assigned(Entry._end) then
          ED:=prepareDate(Entry._end);


        SGEvents.InsertRowWithValues(SGEvents.RowCount,[IntToStr(i),SD,ED,Entry.Summary,TS]);
      end;
      Application.ProcessMessages;
    end;
  until (not assigned(Events)) or (pageToken='');
  SGEvents.SortColRow(True,4);
  MainForm.Cursor:=crDefault;
  BFetchEvents.Enabled:=True;
end;

Procedure TMainForm.DoUserConsent(Const AURL: String; Out AAuthCode: String);

begin
  GBAccess.Visible:=True;
  EAccessCode.Text:='<enter code here>';
  FAccessState:=acsWaiting;
  OpenUrl(AURL);
  While (FAccessState=acsWaiting) do
    Application.ProcessMessages;
  if FAccessState=acsOK then
    AAuthCode:=EAccessCode.Text;
  GBAccess.Visible:=False;
end;

end.

