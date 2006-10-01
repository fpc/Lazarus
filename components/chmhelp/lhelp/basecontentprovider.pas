unit BaseContentProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;
  
type

  { TBaseContentProvider }

  TBaseContentProviderClass = Class of TBaseContentProvider;
  TBaseContentProvider = class(TObject)
  private
    fParent: TWinControl;
  public
    function CanGoBack: Boolean; virtual; abstract;
    function CanGoForward: Boolean; virtual; abstract;
    function GetHistory: TStrings; virtual; abstract;
    function LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean; virtual; abstract;
    procedure GoHome; virtual; abstract;
    procedure GoBack; virtual; abstract;
    procedure GoForward; virtual; abstract;
    class function GetProperContentProvider(const AURL: String): TBaseContentProviderClass; virtual; abstract;
    constructor Create(AParent: TWinControl); virtual;
    property Parent: TWinControl read fParent;
  end;
  



  // returns false if the protocol has already been registered
  function RegisterContentProvider(const Protocol: String; ContentProvider: TBaseContentProviderClass): Boolean;
  // example: RegisterContentProvider('chm://', TChmContentProvider);
  
  function GetContentProvider(const Protocol: String): TBaseContentProviderClass;
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



{ TBaseContentProvider }

constructor TBaseContentProvider.Create(AParent: TWinControl);
begin
  fParent:= AParent;
end;

initialization
  ContentProviders := TStringList.Create;

finalization

  ContentProviders.Free;

end.

