unit BaseContentProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Controls,
  // LazUtils
  Laz2_XMLCfg, LazLoggerBase;
  
type

  { TBaseContentProvider }

  TBaseContentProviderClass = Class of TBaseContentProvider;
  TBaseContentProvider = class(TObject)
  private
    FOnTitleChange: TNotifyEvent;
    FOnContentComplete: TNotifyEvent;
    FParent: TWinControl;
    FTitle: String;
    FConfig: TXMLConfig;
    FUpdateCount: Integer;
  protected
    fImageList: TImageList;
    function GetTitle: String; virtual;
    procedure SetTitle(const AValue: String); virtual;
    function isUpdate: Boolean;
    function isUpdateLast: Boolean;
  public
    function CanGoBack: Boolean; virtual; abstract;
    function CanGoForward: Boolean; virtual; abstract;
    function GetHistory: TStrings; virtual; abstract;
    function LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean; virtual; abstract;
    function HasLoadedData(const {%H-}AURL: String): Boolean; virtual;
    procedure GoHome; virtual; abstract;
    procedure GoBack; virtual; abstract;
    procedure GoForward; virtual; abstract;
    procedure ActivateProvider; virtual;
    procedure ActivateTOCControl; virtual; abstract;
    procedure ActivateIndexControl; virtual; abstract;
    procedure ActivateSearchControl; virtual; abstract;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;
    procedure LoadPreferences(ACfg: TXMLConfig); virtual;
    procedure SavePreferences({%H-}ACfg: TXMLConfig); virtual;
    class function GetProperContentProvider(const AURL: String): TBaseContentProviderClass; virtual; abstract;
    constructor Create(AParent: TWinControl; AImageList: TImageList; AUpdateCount: Integer); virtual;
    destructor Destroy; override;
    property Parent: TWinControl read fParent;
    property Title: String read GetTitle write SetTitle;
    property OnTitleChange: TNotifyEvent read FOnTitleChange write FOnTitleChange;
    property OnContentComplete: TNotifyEvent read FOnContentComplete write FOnContentComplete;
  end;

  function GetUriPrefix( const AUri: String ):String;
  function GetUrlFilePath ( const AUri: String ) : String;
  function GetURIURL( const AURI: String): String;
  function GetURIFileName( const AURI: String): String;
  function GetUrlFile( const AUrl:String): String;
  function GetUrlWoContext( const AUrl:String): String;

  // returns false if the protocol has already been registered
  function RegisterContentProviderClass(const Protocol: String; ContentProvider: TBaseContentProviderClass): Boolean;
  // example: RegisterContentProvider('file://', TChmContentProvider);
  
  function GetContentProvider(const Protocol: String): TBaseContentProviderClass;
  function GetContentProviderList: TStringList;

implementation

var
  ContentProviders: TStringList;

function GetUriPrefix ( const AUri: String ) : String;
var
  xPos: Integer;
begin
  Assert(AUri = Trim(AUri), 'GetUriPrefix: AUri should be trimmed.');
  Result := AUri;
  xPos := Pos('://', AUri);
  if xPos > 0 Then
    SetLength(Result, xPos+2);  // Include '://' in result.
end;

function GetUriPrefixLen ( const AUri: String ) : integer;
var
  xPos: Integer;
begin
  xPos := Pos('://', AUri);
  if xPos > 0 Then
    Result := xPos+2
  else
    Result := Length(AUri);
end;

function GetUrlFilePath ( const AUri: String ) : String;
var
  xPos: Integer;
begin
  Result := Copy(AUri, GetUriPrefixLen(AUri)+1, Length(AUri));
  xPos := Pos('://', Result);
  if xPos > 0 then
    Result := Copy(Result, 1, xPos-1);
  xPos := Pos('?', Result);
  if xPos > 0 then
    SetLength(Result, xPos-1);  // Leave parameters out.
end;

function GetURIFileName(Const AURI: String): String;
var
  FileStart,
  FileEnd: Integer;
begin
  FileStart := Pos(':', AURI)+1;
  FileEnd := Pos('::', AURI);
  Result := Copy(AURI, FileStart, FileEnd-FileStart);
end;

function GetUrlFile(const AUrl: String): String;
var
  xPos: Integer;
begin
  Result := Copy(AUrl, GetUriPrefixLen(AUrl), Length(AUrl));
  xPos := Pos('://', Result);
  if xPos > 0 then
    Result := Copy(Result, xPos+3, Length(Result))
  else
    Result:= '';
end;

function GetUrlWoContext(const AUrl: String): String;
var
  xPos: Integer;
begin
  Result := AUrl;
  xPos := Pos('?', Result);
  if xPos > 0 then
    SetLength(Result, xPos-1);
  xPos := Pos('#', Result);
  if xPos > 0 then
    SetLength(Result, xPos-1);
end;

function GetURIURL(Const AURI: String): String;
var
  URLStart: Integer;
begin
  URLStart := Pos('::', AURI) + 2;
  Result := Copy(AURI, URLStart, Length(AURI));
end;

function RegisterContentProviderClass(const Protocol: String;
  ContentProvider: TBaseContentProviderClass): Boolean;
begin
  Result := False;
  if GetContentProviderList.IndexOf(Protocol) > -1 then exit;
  GetContentProviderList.AddObject(Protocol, TObject(ContentProvider));
  Result := true;
end;

function GetContentProvider(const Protocol: String): TBaseContentProviderClass;
var
  Ind: Integer;
begin
  Result := nil;
  Ind := GetContentProviderList.IndexOf(Protocol);
  if Ind = -1 then Exit;
  Result := TBaseContentProviderClass(GetContentProviderList.Objects[Ind]);
end;

function GetContentProviderList: TStringList;
begin
  if ContentProviders = nil then // Singleton
    ContentProviders := TStringList.Create;
  Result := ContentProviders;
end;

{ TBaseContentProvider }

function TBaseContentProvider.GetTitle: String;
begin
  Result := FTitle;
end;

procedure TBaseContentProvider.SetTitle(const AValue: String);
begin
  FTitle := AValue;
  if Assigned(FOnTitleChange) then
    FOnTitleChange(Self);
end;

function TBaseContentProvider.isUpdate: Boolean;
begin
  Result := FUpdateCount <> 0;
end;

function TBaseContentProvider.isUpdateLast: Boolean;
begin
  Result := FUpdateCount <= 1;
end;

function TBaseContentProvider.HasLoadedData ( const AURL: String ) : Boolean;
begin
  Result:= false;
end;

procedure TBaseContentProvider.ActivateProvider;
begin
  //
end;

procedure TBaseContentProvider.BeginUpdate;
begin
  Inc(FUpdateCount);
  {$IFDEF UPDATE_CNT}
  DebugLn('BeginUpdate() Cnt: ', IntToStr(FUpdateCount));
  {$ENDIF}
end;

procedure TBaseContentProvider.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount < 0 then
    FUpdateCount:=0;
  {$IFDEF UPDATE_CNT}
  DebugLn('EndUpdate() Cnt: ', IntToStr(FUpdateCount));
  {$ENDIF}
end;

procedure TBaseContentProvider.LoadPreferences(ACfg: TXMLConfig);
begin
  FConfig := ACfg;
end;

procedure TBaseContentProvider.SavePreferences(ACfg: TXMLConfig);
begin

end;

constructor TBaseContentProvider.Create(AParent: TWinControl;
    AImageList: TImageList; AUpdateCount: Integer);
begin
  FParent:= AParent;
  FImageList:= AImageList;
  FUpdateCount:= AUpdateCount;
end;

destructor TBaseContentProvider.Destroy;
begin
  SavePreferences(FConfig);
  inherited Destroy;
end;

initialization

finalization

  ContentProviders.Free;

end.

