unit BaseContentProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, XMLCfg;
  
type

  { TBaseContentProvider }

  TBaseContentProviderClass = Class of TBaseContentProvider;
  TBaseContentProvider = class(TObject)
  private
    FOnTitleChange: TNotifyEvent;
    fParent: TWinControl;
    FTitle: String;
    FConfig: TXMLConfig;
  protected
    fImageList: TImageList;
    function GetTitle: String; virtual;
    procedure SetTitle(const AValue: String); virtual;
  public
    function CanGoBack: Boolean; virtual; abstract;
    function CanGoForward: Boolean; virtual; abstract;
    function GetHistory: TStrings; virtual; abstract;
    function LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean; virtual; abstract;
    procedure GoHome; virtual; abstract;
    procedure GoBack; virtual; abstract;
    procedure GoForward; virtual; abstract;
    procedure LoadPreferences(ACfg: TXMLConfig); virtual;
    procedure SavePreferences(ACfg: TXMLConfig); virtual;
    class function GetProperContentProvider(const AURL: String): TBaseContentProviderClass; virtual; abstract;
    constructor Create(AParent: TWinControl; AImageList: TImageList); virtual;
    destructor Destroy; override;
    property Parent: TWinControl read fParent;
    property Title: String read GetTitle write SetTitle;
    property OnTitleChange: TNotifyEvent read FOnTitleChange write FOnTitleChange;
  end;
  



  // returns false if the protocol has already been registered
  function RegisterContentProvider(const Protocol: String; ContentProvider: TBaseContentProviderClass): Boolean;
  // example: RegisterContentProvider('chm://', TChmContentProvider);
  
  function GetContentProvider(const Protocol: String): TBaseContentProviderClass;

  // Result must be freed by caller
  function GetContentProviderList: TStringList;

implementation

var
  ContentProviders: TStringList;

function RegisterContentProvider(const Protocol: String;
  ContentProvider: TBaseContentProviderClass): Boolean;
begin
  Result := False;
  if ContentProviders.IndexOf(Protocol) > -1 then exit;
  ContentProviders.AddObject(Protocol, TObject(ContentProvider));
end;

function GetContentProvider(const Protocol: String): TBaseContentProviderClass;
var
  fIndex: Integer;
begin
  Result := nil;
  fIndex := ContentProviders.IndexOf(Protocol);
  if fIndex = -1 then Exit;
  
  Result := TBaseContentProviderClass(ContentProviders.Objects[fIndex]);
end;

function GetContentProviderList: TStringList;
begin
  Result := TStringList.Create;
  Result.AddStrings(ContentProviders);
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

procedure TBaseContentProvider.LoadPreferences(ACfg: TXMLConfig);
begin
  FConfig := ACfg;
end;

procedure TBaseContentProvider.SavePreferences(ACfg: TXMLConfig);
begin

end;

constructor TBaseContentProvider.Create(AParent: TWinControl; AImageList: TImageList);
begin
  fParent:= AParent;
  fImageList:= AImageList;
end;

destructor TBaseContentProvider.Destroy;
begin
  SavePreferences(FConfig);
  inherited Destroy;
end;

initialization
  ContentProviders := TStringList.Create;

finalization

  ContentProviders.Free;

end.

