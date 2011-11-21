unit objctest1; 

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils;

type
  NSSomeObject = objcclass(NSObject)
    procedure method_(params: Integer); message 'method:';
    class procedure classmethod_(para: char); override; // "message 'classmethod:'" not required, compiler will get this from the parent class
  end;

type
  ObjCClassName1 =  objcclass
  private
  end;

  ObjCClassName2 =  objcclass external name 'ExternalClassName' (ObjCSuperClassName, ProtocolName)
  private
  end;

  ObjCClassName3 =  objcclass external (ObjCSuperClassName)
  private
  end;

  ObjCClassName4 =  objcclass external
  private
  end;

  ObjCClassName5 =  objcclass (ObjCSuperClassName)
  private
  end;

  NSAlertDelegateProtocol = objcprotocol external name 'NSAlertDelegate'
  optional
    function alertShowHelp(alert: NSAlert): Boolean; message 'alertShowHelp:';
  end;

implementation

end.

