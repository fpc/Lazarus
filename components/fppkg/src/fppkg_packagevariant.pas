unit fppkg_packagevariant;

{$mode objfpc}{$H+}

interface

uses
  fgl,
  Classes,
  SysUtils,
  Generics.Collections,
  LazConfigStorage;

type


  { TFppkgNamedItem }

  TFppkgNamedItem = class
  private
    FName: string;
  public
    constructor Create; virtual;
    procedure Save(Config: TConfigStorage); virtual;
    procedure Load(Config: TConfigStorage); virtual;
    property Name: string read FName write FName;
  end;

  { TFppkgNamedItemList }

  generic TFppkgNamedItemList<T: TFppkgNamedItem> = class(specialize TObjectList<T>)
  public
    function FindItemByName(AName: string): T;
    procedure Load(Config: TConfigStorage);
    procedure Save(Config: TConfigStorage);
  end;

  { TFppkgPackageVariantItem }

  TFppkgPackageVariantItem = class(TFppkgNamedItem)
  private
    FCompilerOptions: TStrings;
    FPackageFiles: TStrings;
  public
    constructor Create(); override;
    destructor Destroy; override;
    procedure Save(Config: TConfigStorage); override;
    procedure Load(Config: TConfigStorage); override;
    property CompilerOptions: TStrings read FCompilerOptions;
    property PackageFiles: TStrings read FPackageFiles;
  end;
  TFppkgPackageVariantItemList = specialize TFppkgNamedItemList<TFppkgPackageVariantItem>;

  { TFppkgPackageVariant }

  TFppkgPackageVariant = class(TFppkgNamedItem)
  private
    FItems: TFppkgPackageVariantItemList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Save(Config: TConfigStorage); override;
    procedure Load(Config: TConfigStorage); override;

    property Items: TFppkgPackageVariantItemList read FItems write FItems;
  end;

  TFppkgPackageVariantList = specialize TFppkgNamedItemList<TFppkgPackageVariant>;

implementation

{ TFppkgNamedItem }

constructor TFppkgNamedItem.Create;
begin
  //
end;

procedure TFppkgNamedItem.Save(Config: TConfigStorage);
begin
  Config.SetValue('name', Name);
end;

procedure TFppkgNamedItem.Load(Config: TConfigStorage);
begin
  Name := Config.GetValue('name', '');
end;

{ TFppkgNamedItemList }

function TFppkgNamedItemList.FindItemByName(AName: string): T;
var
  i: Integer;
begin
  Result := T(Nil);
  for i := 0 to Count -1 do
    begin
    if SameText(Items[I].Name, AName) then
      begin
      Result := Items[I];
      Break;
      end;
    end;
end;

procedure TFppkgNamedItemList.Load(Config: TConfigStorage);
var
  Cnt: Integer;
  i: Integer;
  Item: T;
begin
  Config.AppendBasePath('Items');
  try
    Cnt := Config.GetValue('Count',0);
    for i:=0 to Cnt-1 do
      begin
      Config.AppendBasePath('Item'+IntToStr(i+1)+'/');
      try
        Item := T.Create;
        Add(Item);
        Item.Load(Config);
      finally
        Config.UndoAppendBasePath;
      end;
      end;
  finally
    Config.UndoAppendBasePath;
  end;
end;

procedure TFppkgNamedItemList.Save(Config: TConfigStorage);
var
  i: Integer;
begin
  if Count > 0 then
    begin
    Config.AppendBasePath('Items');
    try
      for i:=Count to Config.GetValue('Count', -1) -1 do
        begin
        Config.DeletePath('Item'+IntToStr(i+1));
        end;

      Config.SetDeleteValue('Count', Count, 0);
      for i:=0 to Count-1 do
        begin
        Config.AppendBasePath('Item'+IntToStr(i+1)+'/');
        try
          Items[i].Save(Config);
        finally
          Config.UndoAppendBasePath;
        end;
        end;
    finally
      Config.UndoAppendBasePath;
    end;
    end
  else
    begin
    Config.DeletePath('Items');
    end;
end;

{ TFppkgPackageVariantItem }

constructor TFppkgPackageVariantItem.Create();
begin
  inherited Create();
  FCompilerOptions := TStringList.Create;
  FPackageFiles := TStringList.Create;
end;

destructor TFppkgPackageVariantItem.Destroy;
begin
  FCompilerOptions.Free;
  FPackageFiles.Free;
  inherited Destroy;
end;

procedure TFppkgPackageVariantItem.Save(Config: TConfigStorage);
begin
  inherited;
  Config.SetValue('CompilerOptions', CompilerOptions);
  Config.SetValue('PackageFiles', PackageFiles);
end;

procedure TFppkgPackageVariantItem.Load(Config: TConfigStorage);
begin
  inherited;
  Config.GetValue('CompilerOptions', CompilerOptions);
  Config.GetValue('PackageFiles', PackageFiles);
end;


{ TFppkgPackageVariant }

constructor TFppkgPackageVariant.Create;
begin
  FItems := TFppkgPackageVariantItemList.Create(True);
end;

destructor TFppkgPackageVariant.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TFppkgPackageVariant.Save(Config: TConfigStorage);
begin
  Inherited;
  FItems.Save(Config);
end;

procedure TFppkgPackageVariant.Load(Config: TConfigStorage);
begin
  inherited;
  FItems.Load(Config);
end;

end.

