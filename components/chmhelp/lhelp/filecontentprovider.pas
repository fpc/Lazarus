unit filecontentprovider;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, BaseContentProvider;
  
type

  { TFileContentProvider }

  TFileContentProvider = class(TBaseContentProvider)
  private

  public
    function CanGoBack: Boolean; override;
    function CanGoForward: Boolean; override;
    function GetHistory: TStrings; override;
    function LoadURL(const AURL: String; const AContext: THelpContext=-1): Boolean; override;
    procedure GoHome; override;
    procedure GoBack; override;
    procedure GoForward; override;
    class function GetProperContentProvider(const AURL: String): TBaseContentProviderClass; override;

    constructor Create(AParent: TWinControl; AImageList: TImageList); override;
  end;
  function RegisterFileType(const FileType: String; ContentProvider: TBaseContentProviderClass): Boolean;
  
implementation

var
  FileContentProviders: TStringList;

function RegisterFileType(const FileType: String;
  ContentProvider: TBaseContentProviderClass): Boolean;
begin
  Result := False;
  if FileContentProviders.IndexOf(FileType) > -1 then exit;
  FileContentProviders.AddObject(FileType, TObject(ContentProvider));
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
  fFile: String;
  fPos: Integer;
begin
  Result := nil;
  fFile := Copy(AUrl,8, Length(AURL));
  fPos := Pos('://', fFile);
  if fPos > 0 then begin
    fFile := Copy(fFIle, 1, fPos-1);

  end;
  fExt := ExtractFileExt(fFile);

  //WriteLn(fExt);
  fIndex := FileContentProviders.IndexOf(fExt);
  if fIndex = -1 then exit;
  Result := TBaseContentProviderClass(FileContentProviders.Objects[fIndex]);
end;

constructor TFileContentProvider.Create(AParent: TWinControl; AImageList: TImageList);
begin
  inherited Create(AParent, AImageList);
end;

initialization

  FileContentProviders := TStringList.Create;
  RegisterContentProvider('file://', TFileContentProvider);
  
finalization

 FileContentProviders.Free;

end.

