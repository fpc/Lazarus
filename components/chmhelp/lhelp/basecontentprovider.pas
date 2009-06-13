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
  protected
    fImageList: TImageList;
  public
    function CanGoBack: Boolean; virtual; abstract;
    function CanGoForward: Boolean; virtual; abstract;
    function GetHistory: TStrings; virtual; abstract;
    function LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean; virtual; abstract;
    procedure GoHome; virtual; abstract;
    procedure GoBack; virtual; abstract;
    procedure GoForward; virtual; abstract;
    class function GetProperContentProvider(const AURL: String): TBaseContentProviderClass; virtual; abstract;
    constructor Create(AParent: TWinControl; AImageList: TImageList); virtual;
    property Parent: TWinControl read fParent;
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

constructor TBaseContentProvider.Create(AParent: TWinControl; AImageList: TImageList);
begin
  fParent:= AParent;
  fImageList:= AImageList;
end;

initialization
  ContentProviders := TStringList.Create;

finalization

  ContentProviders.Free;

end.

