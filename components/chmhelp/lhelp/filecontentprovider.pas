unit filecontentprovider;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, BaseContentProvider;
  
type

  { TFileContentProvider }
  TFileContentProviderClass = Class of TFileContentProvider;

  TFileContentProvider = class(TBaseContentProvider)
  private

  public
    function CanGoBack: Boolean; override;
    function CanGoForward: Boolean; override;
    function GetHistory: TStrings; override;
    function LoadURL(const {%H-}AURL: String; const {%H-}AContext: THelpContext=-1): Boolean; override;
    procedure GoHome; override;
    procedure GoBack; override;
    procedure GoForward; override;
    class function GetProperContentProvider(const AURL: String): TBaseContentProviderClass; override;
    class function GetRegisteredFileTypes(): TStringList;

    constructor Create(AParent: TWinControl; AImageList: TImageList; AUpdateCount: Integer); override;
  end;

  function RegisterFileType(const AFileType: String; ContentProvider: TBaseContentProviderClass): Boolean;

implementation

var
  FileContentProviders: TStringList;

function RegisteredFileTypes( ) : TStringList;
begin
  if FileContentProviders = nil Then // Singleton
    FileContentProviders := TStringList.Create;
  Result := FileContentProviders;
end;

function RegisterFileType(const AFileType: String;
  ContentProvider: TBaseContentProviderClass): Boolean;
begin
  Result := False;
  if RegisteredFileTypes.IndexOf(AFileType) > -1 then Exit;
  RegisteredFileTypes.AddObject(AFileType, TObject(ContentProvider));
end;

function GetRegisteredFileType (
  const AProviderClass: TBaseContentProviderClass ) : String;
var
  fIndex: Integer;
begin
  Result := '';
  fIndex := RegisteredFileTypes.IndexOfObject(TObject(AProviderClass));
  if fIndex = -1 then Exit;
  Result := RegisteredFileTypes[fIndex];
end;

{ TFileContentProvider }

function TFileContentProvider.CanGoBack: Boolean;
begin
  Result := False;
end;

function TFileContentProvider.CanGoForward: Boolean;
begin
  Result := False;
end;

function TFileContentProvider.GetHistory: TStrings;
begin
  Result:= nil;
end;

function TFileContentProvider.LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean;
begin
  Result := False;
end;

procedure TFileContentProvider.GoHome;
begin
end;

procedure TFileContentProvider.GoBack;
begin
end;

procedure TFileContentProvider.GoForward;
begin
end;

class function TFileContentProvider.GetProperContentProvider(const AURL: String
  ): TBaseContentProviderClass;
var
  fIndex: Integer;
  fExt: String;
begin
  Result := nil;
  fExt := ExtractFileExt(GetUrlFilePath(AURL));

  //WriteLn(fExt);
  fIndex := RegisteredFileTypes.IndexOf(fExt);
  if fIndex = -1 then exit;
  Result := TBaseContentProviderClass(RegisteredFileTypes.Objects[fIndex]);
end;

class function TFileContentProvider.GetRegisteredFileTypes ( ) : TStringList;
begin
  Result:=RegisteredFileTypes();
end;

constructor TFileContentProvider.Create(AParent: TWinControl;
    AImageList: TImageList; AUpdateCount: Integer);
begin
  inherited Create(AParent, AImageList, AUpdateCount);
end;

initialization

  RegisterContentProviderClass('file://', TFileContentProvider);
  
finalization

 FileContentProviders.Free;

end.

