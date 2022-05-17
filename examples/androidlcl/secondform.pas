unit secondform;

{$mode objfpc}{$H+}

{$ifdef Linux}{$ifdef CPUARM}
  {$define Android}
{$endif}{$endif}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, LCLType, LCLProc, lazdeviceapis;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    btnStartAccel: TButton;
    btnStopAccel: TButton;
    btnGetPos: TButton;
    btnSendSMS: TButton;
    Button2: TButton;
    Button3: TButton;
    textDest: TEdit;
    textBody: TEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    labelSensorData: TLabel;
    procedure btnSendSMSClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnStartAccelClick(Sender: TObject);
    procedure btnStopAccelClick(Sender: TObject);
    procedure btnGetPosClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure textDestExit(Sender: TObject);
    procedure textDestKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure textDestKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure textDestUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { private declarations }
  public
    { public declarations }
    procedure HandleAccelerometerChanged(Sender: TObject);
    procedure HandlePositionRetrieved(Sender: TObject);
    procedure HandleMessagingStatus(AMessage: TLazDeviceMessage;
      AStatus: TLazMessagingStatus);
  end; 

var
  Form2: TForm2; 

implementation

uses
  {$ifdef LCLCustomDrawn}
  customdrawnint,
  {$endif}
  TypInfo, mainform;

{$R *.lfm}

{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
begin
  Hide;
end;

procedure TForm2.btnSendSMSClick(Sender: TObject);
var
  lMessage: TLazDeviceMessage;
begin
  Messaging.OnMessagingStatus := @HandleMessagingStatus;
  lMessage := Messaging.CreateMessage();
  lMessage.Body := textBody.Text;
  lMessage.destinationAddress.Text := textDest.Text;
  DebugLn('[TForm2.btnSendSMSClick] dest='+textDest.Text);
  Messaging.SendMessage(lMessage);
end;

procedure TForm2.btnStartAccelClick(Sender: TObject);
begin
  Accelerometer.OnSensorChanged := @HandleAccelerometerChanged;
  Accelerometer.StartReadingAccelerometerData();
end;

procedure TForm2.btnStopAccelClick(Sender: TObject);
begin
  Accelerometer.StopReadingAccelerometerData();
end;

procedure TForm2.btnGetPosClick(Sender: TObject);
begin
  PositionInfo.RequestPositionInfo(pmGPS);
  PositionInfo.OnPositionRetrieved := @HandlePositionRetrieved;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
  lStr: string;
begin
  lStr := Form1.LoadHTMLPageViaJNI('http://magnifier.sourceforge.net/');
end;

procedure TForm2.Button3Click(Sender: TObject);
begin
  {$ifdef Android}
  CDWidgetSet.ShowListViewDialog('Dialog Title',
    ['Title1', 'Title2', 'Title3', 'Title4', 'Title5', 'Title6'],
    ['Descr1', 'Descr2', 'Descr3', 'Descr4', 'Descr5', 'Descr6']);
  {$endif}
end;

procedure TForm2.textDestExit(Sender: TObject);
begin
  DebugLn('[Edit1Exit]');
end;

procedure TForm2.textDestKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  DebugLn('[Edit1KeyDown] Key=' + DbgsVKCode(Key));
end;

procedure TForm2.textDestKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  DebugLn('[Edit1KeyUp] Key=' + DbgsVKCode(Key));
end;

procedure TForm2.textDestUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  DebugLn('[Edit1UTF8KeyPress] Char=' + UTF8Key);
end;

procedure TForm2.HandleAccelerometerChanged(Sender: TObject);
begin
  labelSensorData.Caption := Format('X=%f Y=%f Z=%f', [Accelerometer.xaxis,
    Accelerometer.yaxis, Accelerometer.zaxis]);
  DebugLn(labelSensorData.Caption);
end;

procedure TForm2.HandlePositionRetrieved(Sender: TObject);
begin
  labelSensorData.Caption := Format('latitude=%f longitude=%f',
    [PositionInfo.latitude, PositionInfo.longitude]);
  DebugLn(labelSensorData.Caption);
end;

procedure TForm2.HandleMessagingStatus(AMessage: TLazDeviceMessage;
  AStatus: TLazMessagingStatus);
begin
  labelSensorData.Caption := '[HandleMessagingStatus] ' +
    GetEnumName(TypeInfo(TLazMessagingStatus), integer(AStatus));
  DebugLn(labelSensorData.Caption);
end;

end.

