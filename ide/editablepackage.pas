unit EditablePackage;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms,
  // IdePackager
  PackageDefs;

type
  TBasePackageEditor = class;

  { TEditablePackage }

  TEditablePackage = class(TLazPackage)
  private
    FPackageEditor: TBasePackageEditor;
    procedure SetPackageEditor(const AValue: TBasePackageEditor);
  protected
    function GetHasEditor: boolean; override;
    procedure SetModified(const AValue: boolean); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure PushEditor; override;
    procedure PopEditor; override;
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

var
  EditorMem: TBasePackageEditor;

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

function TEditablePackage.GetHasEditor: boolean;
begin
  Result:=Assigned(Editor);
end;

procedure TEditablePackage.SetModified(const AValue: boolean);
begin
  inherited SetModified(AValue);
  if Modified and (Editor<>nil) then
    Editor.UpdateAll(false);
end;

// PushEditor and PopEditor use a global variable as a poor man's "stack".
// Pushing and popping package instances are not the same.
// Called at least from TLazPackageGraph.ReplacePackage.
procedure TEditablePackage.PushEditor;
begin
  EditorMem:=Editor;
  if Assigned(EditorMem) then
    EditorMem.LazPackage:=nil;
end;

procedure TEditablePackage.PopEditor;
begin
  if Assigned(EditorMem) then
    EditorMem.LazPackage:=Self;
end;

{ TBasePackageEditor }

function TBasePackageEditor.GetLazPackage: TEditablePackage;
begin
  Result:=nil;
  raise Exception.Create('TBasePackageEditor.GetLazPackage should not be called.');
end;


end.

