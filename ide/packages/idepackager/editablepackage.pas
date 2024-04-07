unit EditablePackage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms,
  // IDE
  PackageDefs;

type
  TBasePackageEditor = class;

  { TEditablePackage }

  TEditablePackage = class(TLazPackage)
  private
    FPackageEditor: TBasePackageEditor;
    procedure SetPackageEditor(const AValue: TBasePackageEditor);
  protected
    procedure SetModified(const AValue: boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property Editor: TBasePackageEditor read FPackageEditor write SetPackageEditor;
  end;

  { TBasePackageEditor }

  TBasePackageEditor = class(TForm)
  protected
    function GetLazPackage: TEditablePackage; virtual;
    procedure SetLazPackage(const AValue: TEditablePackage); virtual; abstract;
  public
    function CanCloseEditor: TModalResult; virtual; abstract;
    procedure UpdateAll(Immediately: boolean = false); virtual; abstract;
    property LazPackage: TEditablePackage read GetLazPackage write SetLazPackage;
  end;


implementation

{ TEditablePackage }

constructor TEditablePackage.Create;
begin
  inherited Create;
end;

destructor TEditablePackage.Destroy;
begin
  inherited Destroy;
end;

procedure TEditablePackage.SetPackageEditor(const AValue: TBasePackageEditor);
begin
  if FPackageEditor=AValue then exit;
  FPackageEditor:=AValue;
end;

procedure TEditablePackage.SetModified(const AValue: boolean);
begin
  inherited SetModified(AValue);
  if Modified and (Editor<>nil) then
    Editor.UpdateAll(false);
end;

{ TBasePackageEditor }

function TBasePackageEditor.GetLazPackage: TEditablePackage;
begin
  Result:=nil;
  raise Exception.Create('TBasePackageEditor.GetLazPackage should not be called.');
end;


end.

