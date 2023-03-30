unit LazDebuggerValueConverter;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils, fgl;

type
  ILazDbgValueConverterIntf = interface;
  TLazDbgValueConvertRegistryEntry = class;
  TLazDbgValueConvertRegistryEntryClass = class of TLazDbgValueConvertRegistryEntry;

  ILazDbgValueConverterSettingsFrameIntf = interface
    ['{0A2F548B-0621-4E2C-A94A-8DE5C3DF81B8}']
    procedure ReadFrom(AConvertor: ILazDbgValueConverterIntf);
    function WriteTo(AConvertor: ILazDbgValueConverterIntf): Boolean;

    function GetFrame: TObject;  // TFrame
    procedure Free;
  end;

  ILazDbgValueConverterIntf = interface
    ['{FC054A2E-17D3-4616-9F68-20E1169C29E8}']
    procedure AddReference;
    procedure ReleaseReference;
    function CreateCopy: ILazDbgValueConverterIntf;

    function GetObject: TObject;
    function GetRegistryEntry: TLazDbgValueConvertRegistryEntryClass;
    function GetSettingsFrame: ILazDbgValueConverterSettingsFrameIntf;
  end;

  ILazDbgValueConvertSelectorIntf = interface
    ['{BEF1676E-0404-4F82-80BD-ABA4522A505E}']
    procedure AddFreeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeNotification(ANotification: TNotifyEvent);
    function GetConverter: ILazDbgValueConverterIntf;

    function AllowedTypeNames: TStrings;
  end;

  { ILazDbgValueConvertSelectorListIntf }

  ILazDbgValueConvertSelectorListIntf = interface
    ['{7004204B-14ED-4282-A150-81AF71D133E3}']
    function Count: Integer;
    function Get(Index: Integer): ILazDbgValueConvertSelectorIntf;
    property Items[Index: Integer]: ILazDbgValueConvertSelectorIntf read Get; default;

    procedure Lock;
    procedure Unlock;

    //function CreateCopy: ILazDbgValueConvertSelectorListIntf;
    //procedure Assign(ASource: ILazDbgValueConvertSelectorListIntf);
    //procedure Free;
  end;

  { TLazDbgValueConvertRegistryEntry }

  TLazDbgValueConvertRegistryEntry = class
  public
    class function CreateValueConvertorIntf: ILazDbgValueConverterIntf; virtual; abstract;
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
  ValueConverterConfigList: ILazDbgValueConvertSelectorListIntf;

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

