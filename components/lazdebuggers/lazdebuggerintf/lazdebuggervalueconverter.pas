unit LazDebuggerValueConverter;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils;

type
  TLazDbgValueConverterIntf = interface;

  TLazDbgValueConverterSettingsFrameIntf = interface
    procedure ReadFrom(AConvertor: TLazDbgValueConverterIntf);
    function WriteTo(AConvertor: TLazDbgValueConverterIntf): Boolean;

    function GetFrame: TObject;  // TFrame
    procedure Free;
  end;

  TLazDbgValueConverterIntf = interface
    procedure AddReference;
    procedure ReleaseReference;
    function GetObject: TObject;
    function GetSettingsFrame: TLazDbgValueConverterSettingsFrameIntf;
  end;

  TLazDbgValueConvertSelectorIntf = interface
    procedure AddFreeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeNotification(ANotification: TNotifyEvent);
    function GetConverter: TLazDbgValueConverterIntf;
    function GetBackendSpecificObject: TObject; deprecated;
  end;


implementation

end.

