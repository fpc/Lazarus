unit TestConfigMemStorage;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  fpcunit,
  testregistry,
  XMLPropStorage,
  LazConfigStorage;

type
  TTestConfigMemStorage1 = class(TTestCase)
  published
    procedure TestDefaultValues;
  end;

implementation

procedure TTestConfigMemStorage1.TestDefaultValues;
var
  Storage: TConfigMemStorage;
  LazConfig: TXMLConfigStorage;
begin
  LazConfig := TXMLConfigStorage.Create('', False);
  try
    Storage := TConfigMemStorage.Create('', False);
    try
      CheckEquals(True, Storage.GetValue('Key/DefTrue1', True));
      CheckEquals(True, Storage.GetValue('Key/DefTrue2', True));
      CheckEquals(False, Storage.GetValue('Key/DefFalse1', False));
      CheckEquals(False, Storage.GetValue('Key/DefFalse2', False));

      CheckEquals('NonEmpty', Storage.GetValue('Key/DefNonEmpty1', 'NonEmpty'));
      CheckEquals('NonEmpty', Storage.GetValue('Key/DefNonEmpty2', 'NonEmpty'));
      CheckEquals('NonEmpty', Storage.GetValue('Key/DefNonEmpty3', 'NonEmpty'));

      Storage.SetDeleteValue('Key/DefTrue1', True, True);
      Storage.SetDeleteValue('Key/DefTrue2', False, True);
      Storage.SetDeleteValue('Key/DefFalse1', True, False);
      Storage.SetDeleteValue('Key/DefFalse2', False, False);

      Storage.SetDeleteValue('Key/DefNonEmpty1', 'NonEmpty', 'NonEmpty');
      Storage.SetDeleteValue('Key/DefNonEmpty2', '', 'NonEmpty');
      Storage.SetDeleteValue('Key/DefNonEmpty3', 'Val', 'NonEmpty');

      CheckEquals(True, Storage.GetValue('Key/DefTrue1', True));
      CheckEquals(False, Storage.GetValue('Key/DefTrue2', True));
      CheckEquals(True, Storage.GetValue('Key/DefFalse1', False));
      CheckEquals(False, Storage.GetValue('Key/DefFalse2', False));

      CheckEquals('NonEmpty', Storage.GetValue('Key/DefNonEmpty1', 'NonEmpty'));
      CheckEquals('', Storage.GetValue('Key/DefNonEmpty2', 'NonEmpty'));
      CheckEquals('Val', Storage.GetValue('Key/DefNonEmpty3', 'NonEmpty'));

      Storage.SaveToConfig(LazConfig, 'CustomOptions');
    finally
      Storage.Free;
    end;

    Storage := TConfigMemStorage.Create('', False);
    try
      Storage.LoadFromConfig(LazConfig, 'CustomOptions');

      CheckEquals(True, Storage.GetValue('Key/DefTrue1', True));
      CheckEquals(False, Storage.GetValue('Key/DefTrue2', True));
      CheckEquals(True, Storage.GetValue('Key/DefFalse1', False));
      CheckEquals(False, Storage.GetValue('Key/DefFalse2', False));

      CheckEquals('NonEmpty', Storage.GetValue('Key/DefNonEmpty1', 'NonEmpty'));
      CheckEquals('', Storage.GetValue('Key/DefNonEmpty2', 'NonEmpty'));
      CheckEquals('Val', Storage.GetValue('Key/DefNonEmpty3', 'NonEmpty'));
    finally
      Storage.Free;
    end;
  finally
    LazConfig.Free;
  end;
end;

initialization
  RegisterTest(TTestConfigMemStorage1);
end.

