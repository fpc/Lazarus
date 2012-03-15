unit ftplister;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, dateutils,
  lnet, lftp;

type

  { TFTPLister }

  TFTPLister = class
  private
    FFtp: TLFTPClient;
    FInternalStatus: TLFTPStatus;
    FDirList: string;
    procedure FtpError(const msg: string; aSocket: TLSocket);
    procedure FtpConnect(Sender: TLSocket);
    procedure FtpControl(Sender: TLSocket);
    procedure FtpReceive(Sender: TLSocket);
    procedure FtpSuccess(Sender: TLSocket; const aStatus: TLFTPStatus);
    procedure FtpFailure(Sender: TLSocket; const aStatus: TLFTPStatus);
  public
    constructor Create;
    destructor Destroy; override;
    function GetList(const server, dir: string): TStrings;
  end;

  { TFtpFile }

  TFtpFile = class
  private
    FFileDate: TDateTime;
    FFileName: string;
  public
    constructor Create(AListLine: string);
    property FileName : string read FFileName;
    property FileDate : TDateTime read FFileDate;
  end;

implementation

const
  MonthNames : array[1..12] of string[3] =
  ('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
   'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC');

constructor TFtpFile.Create(AListLine: string);
{ example line
lrwxrwxrwx    1 0        0               7 Jun 07 19:12 fpc -> pub/fpc
-rw-r--r--    1 1003     100      52533164 Jul 07  2006 lazarus-arm-wince-20060707.7z
}
var
  datestr: string;
  month:integer;
  year: string;
  timestr: string;
  startpos: integer;
  
  procedure ParseMonth(Name: string);
  begin
    Name := uppercase(Name);
    //writeln('Month: ', Name);
    month := 1;
    while (Name<>MonthNames[month]) and (month<high(MonthNames)) do
      inc(month);
  end;

begin
  startpos := 55;
  while (AListLine[startpos]<>' ') do
    inc(startpos);
  FFileName := copy(AListLine, startpos+1, length(AListLine)-startpos);
  writeln(FFileName);
  ParseMonth(copy(AListLine, 44, 3));
  TimeStr := copy(AListLine, 51, 5);
  if pos(':', timestr)=0 then begin
    Year := Trim(timestr);
    timestr := '';
  end
  else
    Year := IntToStr(YearOf(Now));;
  datestr := copy(AListLine, 48, 2) + '-' + IntToStr(month) + '-'+  Year;
  ShortDateFormat := 'DD-MM-YYYY';
  DateSeparator:= '-';
  if length(timestr)>0 then
    datestr := datestr + ' ' + timestr;
  FFileDate := StrToDateTime(datestr);
  if FFileDate > IncDay(Now) then
  begin
    // datum ligt in de toekomst, dan een jaar terug
    Year := IntToStr(YearOf(Now)-1);
    datestr := copy(AListLine, 48, 2) + '-' + IntToStr(month) + '-'+  Year + ' ' + timestr;
    FFileDate := StrToDateTime(datestr);
  end;
end;

{ TFPTLister }

procedure TFTPLister.FtpReceive(Sender: TLSocket);
var
  s: string;
begin
  //Write('D:');
  s := FFtp.GetDataMessage;
  if FInternalStatus = fsCWD then
    FDirList := FDirList + s;
  //Writeln(s);
end;

procedure TFTPLister.FtpSuccess(Sender: TLSocket; const aStatus: TLFTPStatus);
begin
  FInternalStatus := aStatus;
  //Writeln('Success on status: ' + FTPStatusToStr(aStatus));
end;

procedure TFTPLister.FtpFailure(Sender: TLSocket; const aStatus: TLFTPStatus);
begin
  //Writeln('Failure on status: ' + FTPStatusToStr(aStatus));
end;

procedure TFTPLister.FtpError(const msg: string; aSocket: TLSocket);
begin
  Writeln('Error: ', msg);
  RunError(99);
end;

procedure TFTPLister.FtpConnect(Sender: TLSocket);
begin
  //writeln('Connected');
end;

procedure TFTPLister.FtpControl(Sender: TLSocket);
var
  s: string;
begin
  if FFtp.GetMessage(s) > 0 then begin
    //writeln('C: ', s);
  end;
end;

constructor TFTPLister.Create;
begin
  FInternalStatus := fsNone;

  FFtp := TLFTPClient.Create(nil);

  FFtp.StatusSet := [fsType, fsCon, fsCWD, fsList]; // let's watch for these only, these will call OnSuccess or OnFailure
  FFtp.PipeLine := False; // this means CLIENT pipeline emulation
  FFtp.Timeout := 100;
  FFtp.OnConnect := @FtpConnect;
  FFtp.OnError := @FtpError;
  FFtp.OnReceive := @FtpReceive;
  FFtp.OnControl := @FtpControl;
  FFtp.OnSuccess := @FtpSuccess; // reports when "watched" status succesfuly finishes
  FFtp.OnFailure := @FtpFailure; // --||-- fails
end;

destructor TFTPLister.Destroy;
begin
  FFtp.Free;

  inherited Destroy;
end;

function TFTPLister.GetList(const server, dir: string): TStrings;
begin
  Result := TStringList.Create;
  FFtp.Connect(server);

  repeat
    FFtp.CallAction;
  until FFtp.Connected;

  { until here it's same }

  { Let's send all the commands at once (beware shitty servers, set pipeline to false for them) }
  FFtp.Authenticate('anonymous', '');
  FFtp.Binary:=True;
  FFtp.ChangeDirectory(dir);
  FFtp.List();

  repeat
    FFtp.CallAction;
  until FInternalStatus = fsList; // we increate the status per successful operation

  Result.Text := FDirList;
  FFtp.Disconnect;
end;

end.

