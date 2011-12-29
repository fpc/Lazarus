{
  Lazarus Component Library
  
  This is a unit for holding all hardware APIs which do not involve the screen
  nor input events. These are things like accelerometer, SMS, GPS, etc, which
  are typical of mobile devices such as smartphones

  Author: Felipe Monteiro de Carvalho 2011

  License: The same modified LGPL as the rest of the LCL
}
unit lazdeviceapis;

{$mode delphi}

interface

uses
  SysUtils, Classes,
  LCLIntf, LCLType;

type

  // TLazAccelerometer

  { The accelerometer geometry is the following:

    Consider the device with the screen facing the user:
       ^ Y axis
       |
    ________
    |      |
    |      | --> X axis
    |      |
    |[][][]|

    The Z axis goes from the device screen in the direction of the user facing it.
  }

  TLazAccelerometer = class
  private
    FOnSensorChanged: TNotifyEvent;
    FReadingStarted: Boolean;
  public
    // These fields store the last data read, to get fresh data use UpdateAccelerometerData;
    xaxis, yaxis, zaxis: Double; // in m/s^2
    procedure StartReadingAccelerometerData();
    procedure StopReadingAccelerometerData();
    property OnSensorChanged: TNotifyEvent read FOnSensorChanged write FOnSensorChanged;
  end;

  // TLazMessaging

  TLazMessagingStatus = (
    // Message sending status
    mssSentSuccessfully, mssSendingGeneralError, mssRadioOff, mssNoService,
    // Message receiving status (by the destination)
    mssReceivedSuccessfully, mssReceivingGeneralError
    );

  TOnMessagingStatus = procedure (AMessage: TLazDeviceMessage;
    AStatus: TLazMessagingStatus) of object;

  { TLazMessaging }

  TLazMessaging = class
  private
    FOnMessagingStatus: TOnMessagingStatus;
    FMessages: TFPList; // of TLazDeviceMessage
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SendMessage(AMsg: TLazDeviceMessage); // Attempt to send the specified message.
    function CreateMessage: TLazDeviceMessage;
    procedure FreeMessage(AMessage: TLazDeviceMessage);
    // Called asynchronously when there is a message sending status
    property OnMessagingStatus: TOnMessagingStatus read FOnMessagingStatus write FOnMessagingStatus;
  end;

  // TLazPositionInfo

  TLazPositionInfo = class
  private
    FOnPositionRetrieved: TNotifyEvent;
  public
    IsPositionDataAvailable: Boolean; // Indicates if position info was read in the life of this program
    // These fields hold the last position information read
    accuracy: Double; // The horizontal accuracy of the position in meters
    altitude: Double; // Altitude in meters using the World Geodetic System 1984 (WGS84) datum.
    altitudeAccuracy: Double; // The vertical accuracy of the position in meters, or null if not available.
    latitude: Double; // Latitude in degrees using the World Geodetic System 1984 (WGS84) datum.
    longitude: Double; // Longitude in degrees using the World Geodetic System 1984 (WGS84) datum.
    speed: Double; // In meters / second
    timeStamp: TDateTime; // The time when the location was established.
    procedure RequestPositionInfo(AMethod: TLazPositionMethod);
    // Called asynchronously when the position is read
    property OnPositionRetrieved: TNotifyEvent read FOnPositionRetrieved write FOnPositionRetrieved;
  end;

var
  Accelerometer: TLazAccelerometer;
  Messaging: TLazMessaging;
  PositionInfo: TLazPositionInfo;

implementation

{ TLazAccelerometer }

procedure TLazAccelerometer.StartReadingAccelerometerData;
begin
  if FReadingStarted then Exit;
  LCLIntf.LazDeviceAPIs_StartReadingAccelerometerData();
  FReadingStarted := True;
end;

procedure TLazAccelerometer.StopReadingAccelerometerData;
begin
  if not FReadingStarted then Exit;
  LCLIntf.LazDeviceAPIs_StopReadingAccelerometerData();
  FReadingStarted := False;
end;

{ TLazPositionInfo }

procedure TLazPositionInfo.RequestPositionInfo(AMethod: TLazPositionMethod);
begin
  LCLIntf.LazDeviceAPIs_RequestPositionInfo(AMethod);
end;

{ TLazMessaging }

constructor TLazMessaging.Create;
begin
  FMessages := TFPList.Create;
end;

destructor TLazMessaging.Destroy;
var
  i: Integer;
begin
  // Free all messages
  for i := 0 to FMessages.Count-1 do
    TLazDeviceMessage(FMessages.Items[i]).Free;
  FMessages.Free;
  inherited Destroy;
end;

procedure TLazMessaging.SendMessage(AMsg: TLazDeviceMessage);
begin
  LCLIntf.LazDeviceAPIs_SendMessage(AMsg);
end;

function TLazMessaging.CreateMessage: TLazDeviceMessage;
begin
  Result := TLazDeviceMessage.Create;
  FMessages.Add(Result);
end;

procedure TLazMessaging.FreeMessage(AMessage: TLazDeviceMessage);
begin
  FMessages.Remove(AMessage);
  AMessage.Free;
end;

initialization
  Accelerometer := TLazAccelerometer.Create;
  Messaging := TLazMessaging.Create;
  PositionInfo := TLazPositionInfo.Create;
finalization
  Accelerometer.Free;
  Messaging.Free;
  PositionInfo.Free;
end.
