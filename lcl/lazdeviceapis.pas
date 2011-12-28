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
  public
    // These fields store the last data read, to get fresh data use UpdateAccelerometerData;
    xaxis, yaxis, zaxis: Double; // in m/s^2
    procedure UpdateAccelerometerData();
  end;

  // TLazMessaging

  TLazDeviceMessageKind = (dmkSMS, dmkMMS, dmkEMail);

  { TLazDeviceMessage }

  TLazDeviceMessage = class
  public
    Kind: TLazDeviceMessageKind;
    // The commends indicate in which message kind each field is available
    // in this order:                  SMS     MMS     EMail
    //bccAddress	N	N	Y
    Body: string;                    //	Y	Y	Y
    {callbackNumber	Y	N	N
    ccAddress	N	N	Y}
    destinationAddress: TstringList; //	Y	Y	Y
    {isRead	Y	Y	Y
    messageId	Y	Y	Y
    messagePriority	Y	Y	Y
    messageType	Y	Y	Y
    sourceAddress	Y	Y	Y
    Subject	N	Y	Y
    Time	Y	Y	Y
    validityPeriodHours	Y	N	N}
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TLazMessageSendingStatus = (mssSuccess, mssGeneralError);

  TOnMessageSendingFinished = procedure (AMessage: TLazDeviceMessage;
    ASendingStatus: TLazMessageSendingStatus; AErrorMsg: string) of object;

  { TLazMessaging }

  TLazMessaging = class
  private
    FTOnMessageSendingFinished: TOnMessageSendingFinished;
  public
    // Attempt to send the specified message.
    procedure sendMessage(AMsg: TLazDeviceMessage);
    // Called asynchronously when the message sending is finished
    property OnMessageSendingFinished: TOnMessageSendingFinished read FTOnMessageSendingFinished
      write FTOnMessageSendingFinished;
  end;

  // TLazPositionInfo

  TLazPositionMethod = (pmCellID, pmGPS, pmAGPS);

  TOnPositionRetrieved = procedure of object;

  TLazPositionInfo = class
  private
    FOnPositionRetrieved: TOnPositionRetrieved;
  public
    IsPositionDataAvailable: Boolean; // Indicates if position info was read in the life of this program
    // These fields hold the last position information read
    accuracy: Double; // The horizontal accuracy of the position in meters
    altitude: Double; // Altitude in meters using the World Geodetic System 1984 (WGS84) datum.
    altitudeAccuracy: Double; // The vertical accuracy of the position in meters, or null if not available.
    cellID: Double; // This is the id of the cell.
    latitude: Double; // Latitude in degrees using the World Geodetic System 1984 (WGS84) datum.
    longitude: Double; // Longitude in degrees using the World Geodetic System 1984 (WGS84) datum.
    timeStamp: TDateTime; // The time when the location was established.
    procedure RequestPositionInfo(AMethod: TLazPositionMethod);
    // Called asynchronously when the position is read
    property OnPositionRetrieved: TOnPositionRetrieved read FOnPositionRetrieved write FOnPositionRetrieved;
  end;

var
  Accelerometer: TLazAccelerometer;
  Messaging: TLazMessaging;
  PositionInfo: TLazPositionInfo;

implementation

{ TLazAccelerometer }

procedure TLazAccelerometer.UpdateAccelerometerData;
begin
  //LCLIntf.UpdateAccelerometerData(Self);
end;

{ TLazPositionInfo }

procedure TLazPositionInfo.RequestPositionInfo(AMethod: TLazPositionMethod);
begin

end;

{ TLazDeviceMessage }

constructor TLazDeviceMessage.Create;
begin
  inherited Create;
  destinationAddress := TStringList.Create;
end;

destructor TLazDeviceMessage.Destroy;
begin
  destinationAddress.Free;
  inherited Destroy;
end;

{ TLazMessaging }

procedure TLazMessaging.sendMessage(AMsg: TLazDeviceMessage);
begin
  //LCLIntf.LazMessaging_SendMessage(Self, AMsg);
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
