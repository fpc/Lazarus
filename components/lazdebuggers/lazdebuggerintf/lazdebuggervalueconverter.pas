unit LazDebuggerValueConverter;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fgl;

type
  TLazDbgValueConverterIntf = interface;
  TLazDbgValueConvertRegistryEntry = class;
  TLazDbgValueConvertRegistryEntryClass = class of TLazDbgValueConvertRegistryEntry;

  TLazDbgValueConverterSettingsFrameIntf = interface
    procedure ReadFrom(AConvertor: TLazDbgValueConverterIntf);
    function WriteTo(AConvertor: TLazDbgValueConverterIntf): Boolean;

    function GetFrame: TObject;  // TFrame
    procedure Free;
  end;

  TLazDbgValueConverterIntf = interface
    procedure AddReference;
    procedure ReleaseReference;
    function CreateCopy: TLazDbgValueConverterIntf;

    function GetObject: TObject;
    function GetRegistryEntry: TLazDbgValueConvertRegistryEntryClass;
    function GetSettingsFrame: TLazDbgValueConverterSettingsFrameIntf;
  end;

  TLazDbgValueConvertSelectorIntf = interface
    procedure AddFreeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeNotification(ANotification: TNotifyEvent);
    function GetConverter: TLazDbgValueConverterIntf;

    function AllowedTypeNames: TStrings;
  end;

  { TLazDbgValueConvertSelectorListIntf }

  TLazDbgValueConvertSelectorListIntf = interface
    function Count: Integer;
    function Get(Index: Integer): TLazDbgValueConvertSelectorIntf;
    property Items[Index: Integer]: TLazDbgValueConvertSelectorIntf read Get; default;

    procedure Lock;
    procedure Unlock;

    //function CreateCopy: TLazDbgValueConvertSelectorListIntf;
    //procedure Assign(ASource: TLazDbgValueConvertSelectorListIntf);
    //procedure Free;
  end;

  { TLazDbgValueConvertRegistryEntry }

  TLazDbgValueConvertRegistryEntry = class
  public
    class function CreateValueConvertorIntf: TLazDbgValueConverterIntf; virtual; abstract;
    class function GetName: String; virtual; abstract;
    class function GetConvertorClass: TClass; virtual; abstract;
    class function GetDebuggerClass: TClass; virtual; abstract; //  class of TDebuggerIntf
  end;

  { TLazDbgValueConvertRegistry }

  TLazDbgValueConvertRegistry = class(specialize TFPGList<TLazDbgValueConvertRegistryEntryClass>)
    function FindByConvertorClassName(AName: String): TLazDbgValueConvertRegistryEntryClass;
    function IndexOfConvertorClass(AClass: TClass): integer;
  end;

function ValueConverterRegistry: TLazDbgValueConvertRegistry;

var
  ValueConverterConfigList: TLazDbgValueConvertSelectorListIntf;

implementation
var
  TheValueConverterRegistry: TLazDbgValueConvertRegistry;

function ValueConverterRegistry: TLazDbgValueConvertRegistry;
begin
  if TheValueConverterRegistry = nil then
    TheValueConverterRegistry := TLazDbgValueConvertRegistry.Create;
  Result := TheValueConverterRegistry;
end;

{ TLazDbgValueConvertRegistry }

function TLazDbgValueConvertRegistry.FindByConvertorClassName(AName: String
  ): TLazDbgValueConvertRegistryEntryClass;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count -1 do
    if Items[i].GetConvertorClass.ClassName = AName then
      exit(Items[i]);
end;

function TLazDbgValueConvertRegistry.IndexOfConvertorClass(AClass: TClass
  ): integer;
begin
  Result := Count - 1;
  while Result >= 0 do begin
    if Items[Result].GetConvertorClass = AClass then
      exit;
    dec(Result);
  end;
end;

finalization
  FreeAndNil(TheValueConverterRegistry);

end.

