unit mainform; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLProc, Arrow, StdCtrls, ComCtrls, LCLType, LCLIntf, InterfaceBase,
  lazdeviceapis, Menus, ExtDlgs, customdrawncontrols, ClipBrd;

type
  TSubControl = class;

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    btnShowInfo: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    ProgressBar1: TProgressBar;
    TrackBar1: TTrackBar;
    procedure Arrow1Click(Sender: TObject);
    procedure Arrow1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Arrow1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
    procedure Arrow1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnShowInfoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure Button1KeyPress(Sender: TObject; var Key: char);
    procedure Button1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Button1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    SubControl: TSubControl;
    ClickCounter: Integer;
    procedure HandleMessageDialogFinished(Sender: TObject; AResult: Integer);
    procedure SocketProc;
    procedure SocketProc2;
  end; 

  { TSubControl }

  TSubControl = class(TCustomControl)
  public
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure Paint; override;
  end;

var
  Form1: TForm1; 

implementation

uses secondform,
  ctypes, android_sockets,
  jni, customdrawnint;

{ TSubControl }

procedure TSubControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('TSubControl.Mouse Down X=%d Y=%d', [X, Y]));
end;

procedure TSubControl.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  DebugLn(Format('TSubControl.Mouse Move X=%d Y=%d', [X, Y]));
end;

procedure TSubControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('TSubControl.Mouse Up X=%d Y=%d', [X, Y]));
end;

procedure TSubControl.MouseEnter;
begin
  DebugLn('TSubControl.Mouse Enter');
end;

procedure TSubControl.MouseLeave;
begin
  DebugLn('TSubControl.Mouse Leave');
end;

procedure TSubControl.Paint;
begin
  Canvas.Brush.Color := clBlue;
  Canvas.Rectangle(0, 0, Width, Height);
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormClick(Sender: TObject);
begin
  DebugLn(Format('Form click #%d', [ClickCounter]));
  Inc(ClickCounter);
//  Invalidate;
end;

procedure TForm1.Arrow1Click(Sender: TObject);
begin
  Caption := 'Clicked Arrow';
  DebugLn('Clicked Arrow');
end;

procedure TForm1.Arrow1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Down X=%d Y=%d', [X, Y]));
end;

procedure TForm1.Arrow1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Move X=%d Y=%d', [X, Y]));
end;

procedure TForm1.Arrow1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Up X=%d Y=%d', [X, Y]));
end;

procedure TForm1.btnShowInfoClick(Sender: TObject);
//var
//  i: Integer;
begin
  //for i := 0 to Screen.Fonts.Count - 1 do
  //  DebugLn(Screen.Fonts.Strings[i]);
  DebugLn('Device.Manufacturer='+Device.Manufacturer);
  DebugLn('Device.Model='+Device.Model);
  Device.Vibrate(2000);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  DebugLn('Button1Click');
  ProgressBar1.Position := ProgressBar1.Position + 10;
  DebugLn('Cliboard.AsText='+ClipBoard.AsText);
  SocketProc2();
  //ClipBoard.AsText:='Button1Clicked';
//  OpenDocument('/mnt/sdcard/dcim/100MEDIA/IMAG0008.jpg');
//  OpenDocument('/mnt/sdcard/emaillog.txt');
//  OpenURL('http://www.google.com');
//  Self.AutoAdjustLayout(lapAutoAdjustWithoutHorizontalScrolling, 96, 150, 220, 600);
end;

procedure TForm1.Button1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  DebugLn('[TForm1.Button1KeyDown] '+ LCLProc.DbgsVKCode(Key));
//  Caption := 'KeyDown ' + LCLProc.DbgsVKCode(Key);
end;

procedure TForm1.Button1KeyPress(Sender: TObject; var Key: char);
begin
  DebugLn('KeyPress: ' + Key);
end;

procedure TForm1.Button1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  DebugLn('[TForm1.Button1KeyUp] '+ LCLProc.DbgsVKCode(Key));
//  Caption := 'KeyUp ' + LCLProc.DbgsVKCode(Key);
end;

procedure TForm1.Button1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  DebugLn('UTF8KeyPress: ' + UTF8Key);
  Caption := UTF8Key;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Application.OnMessageDialogFinished := @HandleMessageDialogFinished;
  DebugLn('Button2Click A');
//  LCLIntf.MessageBox(0, 'Text', 'Title', MB_ABORTRETRYIGNORE);
  Application.MessageBox('Text', 'Title', MB_ABORTRETRYIGNORE);
  DebugLn('Button2Click B');
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Form2.Show;
  DebugLn('Button3Click');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SubControl := TSubControl.Create(Self);
  SubControl.Left := 40;
  SubControl.Top := 160;
  SubControl.Width := 50;
  SubControl.Height := 50;
  SubControl.Parent := Self;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('MouseMove x=%d y=%d', [x, y]));
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  lPoints: array[0..2] of TPoint;
begin
  Canvas.Brush.Color := clRed;
  lPoints[0] := Point(67,57);
  lPoints[1] := Point(11,29);
  lPoints[2] := Point(67,1);
  Canvas.Polygon(lPoints);

{  Canvas.Brush.Color := clRed;
  Canvas.Rectangle(10, 10, 100, 100);
  Canvas.Brush.Color := clGreen;
  Canvas.Rectangle(100, 100, 200, 200);
  Canvas.Brush.Color := clBlue;
  Canvas.Rectangle(200, 200, 300, 300);}
end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  DebugLn('[TForm1.MenuItem1Click]');
end;

procedure TForm1.HandleMessageDialogFinished(Sender: TObject; AResult: Integer);
begin
  DebugLn(Format('[TForm1.HandleMessageDialogFinished] AResult=%d', [AResult]));
end;

procedure TForm1.SocketProc;
const
  RCVBUFSIZE = 64;
var
  sock: cint;                      { Socket descriptor }
  echoServAddr: Tsockaddr_in;      { Echo server address }
  echoServPort: cushort;           { Echo server port }
  servIP: PChar;                   { Server IP address (dotted quad) }
  echoString: PChar;               { String to send to echo server }
  echoBuffer: array[0..RCVBUFSIZE-1] of Char;     { Buffer for echo string }
  echoStringLen: cuint;            { Length of string to echo }
  bytesRcvd, totalBytesRcvd: cint; { Bytes read in single recv() and total bytes read }
begin
  servIP := '123.123.123.123';
  echoString := 'echo string';

  echoServPort := 7; // 7 is the well-known port for the echo service

  // Create a reliable, stream socket using TCP
  sock := socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  DebugLn(Format('[SocketProc] sock=%d', [sock]));
//  if sock < 0 then
//    DieWithError("socket() failed");

  // Construct the server address structure */
(*      memset(&echoServAddr, 0, sizeof(echoServAddr));     /* Zero out structure */
      echoServAddr.sin_family      = AF_INET;             /* Internet address family */
      echoServAddr.sin_addr.s_addr = inet_addr(servIP);   /* Server IP address */
      echoServAddr.sin_port        = htons(echoServPort); /* Server port */

      /* Establish the connection to the echo server */
      if (connect(sock, (struct sockaddr * ) &echoServAddr, sizeof(echoServAddr)) < 0)
          DieWithError("connect() failed");

      echoStringLen = strlen(echoString);          /* Determine input length */

      /* Send the string to the server */
      if (send(sock, echoString, echoStringLen, 0) != echoStringLen)
          DieWithError("send() sent a different number of bytes than expected");

      /* Receive the same string back from the server */
      totalBytesRcvd = 0;
      printf("Received: ");                /* Setup to print the echoed string */
      while (totalBytesRcvd < echoStringLen)
      {
          /* Receive up to the buffer size (minus 1 to leave space for
             a null terminator) bytes from the sender */
          if ((bytesRcvd = recv(sock, echoBuffer, RCVBUFSIZE - 1, 0)) <= 0)
              DieWithError("recv() failed or connection closed prematurely");
          totalBytesRcvd += bytesRcvd;   /* Keep tally of total bytes */
          echoBuffer[bytesRcvd] = '\0';  /* Terminate the string! */
          printf("%s", echoBuffer);      /* Print the echo buffer */
      }

      printf("\n");    /* Print a final linefeed */

      close(sock);
      exit(0);
  }//-- end main --//*)
end;

procedure TForm1.SocketProc2;
var
  javaClass_DefaultHttpClient, javaClass_HttpGet,
    javaClass_URI, javaClass_InputStreamReader, javaClass_BufferedReader: jclass;
  javaMethod_DefaultHttpClient_new,
    javaMethod_HttpGet_new, javaMethod_HttpGet_setURI,
    javaMethod_URI_new, javaMethod_InputStreamReader_new, javaMethod_BufferedReader_new: jmethod;
  javaHttpClient, javaRequest, javaURI,
    javaResponse, javaStreamReader, javaBufferedReader: jobject;
  javaString: jstring;
  lParams: array[0..2] of JValue;
begin
  // First call FindClass for all required classes
  javaClass_DefaultHttpClient := javaEnvRef^^.FindClass(
    javaEnvRef,
    'org/apache/http/impl/client/DefaultHttpClient');
  javaClass_HttpGet := javaEnvRef^^.FindClass(javaEnvRef,
    'org/apache/http/client/methods/HttpGet');
  javaClass_URI := javaEnvRef^^.FindClass(javaEnvRef,
    'android/net/URI/');
  javaClass_InputStreamReader := javaEnvRef^^.FindClass(javaEnvRef,
    'java/io/InputStreamReader');
  javaClass_BufferedReader := javaEnvRef^^.FindClass(javaEnvRef,
    'java/io/BufferedReader');

  // Create a new DefaultHttpClient instance
  // HttpClient javaHttpClient = new DefaultHttpClient();
  javaMethod_DefaultHttpClient_new :=
    javaEnvRef^^.GetMethodID(javaEnvRef,
    javaClass_DefaultHttpClient, '<init>', '()V');
  javaHttpClient := javaEnvRef^^.NewObject(javaEnvRef,
    javaClass_DefaultHttpClient,
    javaMethod_DefaultHttpClient_new);

  // Create a new instence for the HTTP request object
  // HttpGet javaRequest = new HttpGet();
  javaMethod_HttpGet_new := javaEnvRef^^.GetMethodID(
    javaEnvRef, javaClass_DefaultHttpClient, '<init>', '()V');
  javaRequest := javaEnvRef^^.NewObject(javaEnvRef,
    javaClass_DefaultHttpClient,
    javaMethod_DefaultHttpClient_new);

  // Add a URI for the request object
  // URI myURI = new URI("http://w3mentor.com/");
  javaMethod_URI_new := javaEnvRef^^.GetMethodID(
    javaEnvRef, javaClass_URI, '<init>',
    '(Ljava/lang/String;)V');
  lParams[0].l := javaEnvRef^^.NewStringUTF(javaEnvRef, 'http://w3mentor.com/');
  javaURI := javaEnvRef^^.NewObjectA(javaEnvRef,
    javaClass_URI, javaMethod_URI_new, @lParams[0]);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lParams[0].l);
  // javaRequest.setURI(myURI);
  javaMethod_HttpGet_setURI := javaEnvRef^^.GetMethodID(
    javaEnvRef, javaClass_URI, 'setURI',
    '(Landroid/net/URI/;)V');
  javaEnvRef^^.CallVoidMethodA(javaEnvRef, javaRequest,
    javaMethod_HttpGet_setURI, @lParams[0]);

  // Execute the HTTP request and obtain a HTTP response
  // HttpResponse response = javaHttpClient.execute(javaRequest);
  javaMethod_HttpClient_execute := javaEnvRef^^.GetMethodID(
    javaEnvRef, javaClass_URI, 'execute',
    '(Lorg/apache/http/client/methods/HttpUriRequest;)Lorg/apache/http/HttpResponse;');
  lParams[0].l := javaRequest;
  javaResponse := javaEnvRef^^.CallObjectMethodA(javaEnvRef, javaHttpClient,
    javaMethod_HttpClient_execute, @lParams[0]);

  // javaContent = response.getEntity().getContent();
  // .....
  // javaStreamReader = new InputStreamReader(javaContent)
  javaMethod_InputStreamReader_new := javaEnvRef^^.GetMethodID(
    javaEnvRef, javaClass_URI, '<init>',
    '(Ljava/io/InputStream)V');
  lParams[0].l := javaRequest;
  javaStreamReader := javaEnvRef^^.NewObjectA(javaEnvRef,
    javaClass_InputStreamReader,
    javaMethod_InputStreamReader_new, @lParams[0]);
  // javaBufferedReader = new BufferedReader(javaStreamReader);
  javaMethod_BufferedReader_new := javaEnvRef^^.GetMethodID(
    javaEnvRef, javaClass_URI, '<init>',
    '(Ljava/io/InputStream)V');
  lParams[0].l := javaStreamReader;
  javaBufferedReader := javaEnvRef^^.NewObjectA(javaEnvRef,
    javaClass_BufferedReader,
    javaMethod_BufferedReader_new, @lParams[0]);

  // String line = "";
  // while ((line = javaBufferedReader.readLine()) != null) {
  // }
  // javaBufferedReader.close();
end;

end.

