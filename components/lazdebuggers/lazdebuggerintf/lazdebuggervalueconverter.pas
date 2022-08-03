unit LazDebuggerValueConverter;

{$mode objfpc}{$H+}
{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils;

type

  TLazDbgValueConverterIntf = interface
    procedure AddReference;
    procedure ReleaseReference;
    function GetObject: TObject;
  end;

  TLazDbgValueConvertSelectorIntf = interface
    procedure AddFreeNotification(ANotification: TNotifyEvent);
    procedure RemoveFreeNotification(ANotification: TNotifyEvent);
    function GetBackendSpecificObject: TObject;
  end;


implementation

end.

