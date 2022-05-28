unit stub.jsondataset;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils,  fpjsondataset;

Type
  { TCustomLocalJSONDataset }
  TStorageKind = (skLocal,skSession);
  TStorageOption = (soAutoLoad,soAutoSave);
  TStorageOptions = Set of TStorageOption;


  TCustomLocalJSONDataset = class(TJSONDataset)
  private
    FDataElement: String;
    FStorageKey: String;
    FStorageKind: TStorageKind;
    FStorageOptions: TStorageOptions;
    function GetStorageKey: String;
    function IsKeyStored: Boolean;
  Public
    Property DataElement : String Read FDataElement Write FDataElement;
    Property StorageKey : String Read GetStorageKey Write FStorageKey stored IsKeyStored;
    Property StorageKind : TStorageKind Read FStorageKind Write FStorageKind;
    Property Options : TStorageOptions Read FStorageOptions Write FStorageOptions;
  end;

  TLocalJSONDataset = Class(TCustomLocalJSONDataset)
  Published
    Property DataElement;
    Property Options;
    Property StorageKind;
    Property StorageKey;
  end;


implementation


{ TCustomLocalJSONDataset }


function TCustomLocalJSONDataset.GetStorageKey: String;
begin
  Result:=FStorageKey;
  if Result='' then
    Result:=Name;
end;

function TCustomLocalJSONDataset.IsKeyStored: Boolean;
begin
  Result:=FStorageKey<>Name;
end;


end.

