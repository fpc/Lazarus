{
 *****************************************************************************
 *                         CustomDrawnLazDeviceAPIS.pas                      *
 *                                ----------                                 * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit CustomDrawnWSLazDeviceAPIS;

{$mode objfpc}{$H+}

{$I customdrawndefines.inc}

interface

uses
  // RTL
  Types, Math,
  {$ifdef CD_Android}
  jni,
  {$endif}
  // LCL
  LazDeviceAPIs,
  // Widgetset
  customdrawnint, WSLCLClasses, WSLazDeviceAPIs;

type
  { TWSLazDeviceAPIS }
  
  { TCDWSLazDeviceAPIs }

  TCDWSLazDeviceAPIs = class(TWSLazDeviceAPIs)
  public
    class procedure RequestPositionInfo(AMethod: TLazPositionMethod); override;
    class procedure SendMessage(AMsg: TLazDeviceMessage); override;
    class procedure StartReadingAccelerometerData(); override;
    class procedure StopReadingAccelerometerData(); override;
  end;

implementation

{ TCDWSLazDeviceAPIs }

{$if defined(CD_Windows) or defined(CD_Cocoa) or defined(CD_X11)}
class procedure TCDWSLazDeviceAPIs.RequestPositionInfo(
  AMethod: TLazPositionMethod);
begin

end;

class procedure TCDWSLazDeviceAPIs.SendMessage(AMsg: TLazDeviceMessage);
begin

end;

class procedure TCDWSLazDeviceAPIs.StartReadingAccelerometerData;
begin

end;

class procedure TCDWSLazDeviceAPIs.StopReadingAccelerometerData;
begin

end;
{$endif}

{$ifdef CD_Android}
class procedure TCDWSLazDeviceAPIs.RequestPositionInfo(
  AMethod: TLazPositionMethod);
var
  lPositionMethod: jint;
begin
  // Prepare the input
  case AMethod of
    pmGPS: lPositionMethod := 1;
    pmNetwork: lPositionMethod := 2;
  else
    Exit;
  end;
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclkind, lPositionMethod);
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoRequestPositionInfo);
end;

class procedure TCDWSLazDeviceAPIs.SendMessage(AMsg: TLazDeviceMessage);
var
  lJavaString: jstring;
  lStr: String;
begin
  // Prepare the input
  // String fields
  lStr := AMsg.Body;
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(lStr));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcltext, lJavaString);
  lStr := AMsg.destinationAddress.Text;
  lJavaString :=javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(lStr));
  javaEnvRef^^.SetObjectField(javaEnvRef, javaActivityObject, JavaField_lcldestination, lJavaString);
  // Message type
  javaEnvRef^^.SetIntField(javaEnvRef, javaActivityObject, JavaField_lclkind, 1);
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoSendMessage);
end;

class procedure TCDWSLazDeviceAPIs.StartReadingAccelerometerData;
begin
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoStartReadingAccelerometer);
end;

class procedure TCDWSLazDeviceAPIs.StopReadingAccelerometerData;
begin
  // Call the method
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaActivityObject, javaMethod_LCLDoStopReadingAccelerometer);
end;
{$endif}

end.
