unit mainform; 

{$mode objfpc}{$H+}

{$ifdef Linux}{$ifdef CPUARM}
  {$define Android}
{$endif}{$endif}
{.$define TEST_SQLITE}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  LCLProc, Arrow, StdCtrls, ComCtrls, LCLType, LCLIntf, InterfaceBase,
  lazdeviceapis, Menus, ExtDlgs, customdrawncontrols, ClipBrd;

type
  TSubControl = class;

  { Tform1 }

  Tform1 = class(TForm)
    btnProgress: TButton;
    btnMsgBox: TButton;
    btnOpenForm: TButton;
    btnShowInfo: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
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
    procedure btnProgressClick(Sender: TObject);
    procedure btnProgressKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure btnProgressKeyPress(Sender: TObject; var Key: char);
    procedure btnProgressKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnProgressUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure btnMsgBoxClick(Sender: TObject);
    procedure btnOpenFormClick(Sender: TObject);
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
    function LoadHTMLPageViaJNI(AURL: string): string;
    procedure MyOnListViewDialogResult(ASelectedItem: Integer);
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
  form1: Tform1;

implementation

uses secondform, sqliteform,
  ctypes //android_sockets,
  {$ifdef Android}
  ,jni,customdrawnint
  {$endif}
  ;

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

{ Tform1 }

procedure Tform1.FormClick(Sender: TObject);
begin
  DebugLn(Format('Form click #%d', [ClickCounter]));
  Inc(ClickCounter);
//  Invalidate;
end;

procedure Tform1.Arrow1Click(Sender: TObject);
begin
  Caption := 'Clicked Arrow';
  DebugLn('Clicked Arrow');
end;

procedure Tform1.Arrow1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Down X=%d Y=%d', [X, Y]));
end;

procedure Tform1.Arrow1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Move X=%d Y=%d', [X, Y]));
end;

procedure Tform1.Arrow1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DebugLn(Format('Arrow Mouse Up X=%d Y=%d', [X, Y]));
end;

procedure Tform1.btnShowInfoClick(Sender: TObject);
//var
//  i: Integer;
begin
  //for i := 0 to Screen.Fonts.Count - 1 do
  //  DebugLn(Screen.Fonts.Strings[i]);
  DebugLn('Device.Manufacturer='+Device.Manufacturer);
  DebugLn('Device.Model='+Device.Model);
  Device.Vibrate(2000);
end;

procedure Tform1.btnProgressClick(Sender: TObject);
var
  sqliteDLL : Pointer;
begin
(*  sqliteDLL:=DlOpen('/system/lib/libsqlite.so',RTLD_LAZY);
  DebugLn(IntToHex(PtrUInt(sqliteDLL), 8));
  sqliteDLL:=DlOpen('/data/data/com.pascal.lcltest/lib/libsqlite.so',RTLD_LAZY);*)
  btnProgress.Caption := IntToHex(PtrUInt(sqliteDLL), 8);
  DebugLn('Button1Click');
  ProgressBar1.Position := ProgressBar1.Position + 10;
  DebugLn('Cliboard.AsText='+ClipBoard.AsText);
  //ClipBoard.AsText:='Button1Clicked';
//  OpenDocument('/mnt/sdcard/dcim/100MEDIA/IMAG0008.jpg');
//  OpenDocument('/mnt/sdcard/emaillog.txt');
//  OpenURL('http://www.google.com');
//  Self.AutoAdjustLayout(lapAutoAdjustWithoutHorizontalScrolling, 96, 150, 220, 600);
end;

procedure Tform1.btnProgressKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  DebugLn('[TForm1.Button1KeyDown] '+ LCLProc.DbgsVKCode(Key));
//  Caption := 'KeyDown ' + LCLProc.DbgsVKCode(Key);
end;

procedure Tform1.btnProgressKeyPress(Sender: TObject; var Key: char);
begin
  DebugLn('KeyPress: ' + Key);
end;

procedure Tform1.btnProgressKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  DebugLn('[TForm1.Button1KeyUp] '+ LCLProc.DbgsVKCode(Key));
//  Caption := 'KeyUp ' + LCLProc.DbgsVKCode(Key);
end;

procedure Tform1.btnProgressUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  DebugLn('UTF8KeyPress: ' + UTF8Key);
  Caption := UTF8Key;
end;

procedure Tform1.btnMsgBoxClick(Sender: TObject);
begin
  Application.OnMessageDialogFinished := @HandleMessageDialogFinished;
  DebugLn('Button2Click A');
//  LCLIntf.MessageBox(0, 'Text', 'Title', MB_ABORTRETRYIGNORE);
  Application.MessageBox('Text', 'Title', MB_ABORTRETRYIGNORE);
  DebugLn('Button2Click B');
end;

procedure Tform1.btnOpenFormClick(Sender: TObject);
begin
  {$ifdef TEST_SQLITE}formsqlite.Show;
  {$else}
  Form2.Show;
  {$ENDIF}

  DebugLn('Button3Click');
end;

procedure Tform1.FormCreate(Sender: TObject);
begin
  SubControl := TSubControl.Create(Self);
  SubControl.Left := 40;
  SubControl.Top := 160;
  SubControl.Width := 50;
  SubControl.Height := 50;
  SubControl.Parent := Self;

  OnListViewDialogResult := @MyOnListViewDialogResult;
end;

procedure Tform1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  DebugLn(Format('MouseMove x=%d y=%d', [x, y]));
end;

procedure Tform1.FormPaint(Sender: TObject);
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

procedure Tform1.MenuItem1Click(Sender: TObject);
begin
  DebugLn('[TForm1.MenuItem1Click]');
end;

procedure Tform1.HandleMessageDialogFinished(Sender: TObject; AResult: Integer);
begin
  DebugLn(Format('[TForm1.HandleMessageDialogFinished] AResult=%d', [AResult]));
end;

procedure Tform1.SocketProc;
const
  RCVBUFSIZE = 64;
(*var
  sock: cint;                      { Socket descriptor }
  echoServAddr: Tsockaddr_in;      { Echo server address }
  echoServPort: cushort;           { Echo server port }
  servIP: PChar;                   { Server IP address (dotted quad) }
  echoString: PChar;               { String to send to echo server }
  echoBuffer: array[0..RCVBUFSIZE-1] of Char;     { Buffer for echo string }
  echoStringLen: cuint;            { Length of string to echo }
  bytesRcvd, totalBytesRcvd: cint; { Bytes read in single recv() and total bytes read }*)
begin
(*  servIP := '123.123.123.123';
  echoString := 'echo string';

  echoServPort := 7; // 7 is the well-known port for the echo service

  // Create a reliable, stream socket using TCP
  sock := socket(PF_INET, SOCK_STREAM, IPPROTO_TCP);
  DebugLn(Format('[SocketProc] sock=%d', [sock]));
//  if sock < 0 then
//    DieWithError("socket() failed");

  // Construct the server address structure */*)
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

function Tform1.LoadHTMLPageViaJNI(AURL: string): string;
{$ifdef Android}
var
  javaClass_DefaultHttpClient, javaClass_AbstractHttpClient, javaClass_HttpGet,
    javaClass_URI, javaClass_HttpResponse, javaClass_HttpEntity,
    javaClass_InputStreamReader, javaClass_BufferedReader: jclass;
  javaMethod_DefaultHttpClient_new, javaMethod_DefaultHttpClient_execute,
    javaMethod_HttpGet_new, javaMethod_HttpGet_setURI,
    javaMethod_URI_new,
    javaMethod_HttpResponse_getEntity,
    javaMethod_HttpEntity_getContent,
    javaMethod_InputStreamReader_new,
    javaMethod_BufferedReader_new, javaMethod_BufferedReader_readLine,
    javaMethod_BufferedReader_close: jmethodid;
  javaHttpClient, javaRequest, javaURI,
    javaResponse, javaEntity, javaContent,
    javaStreamReader, javaBufferedReader: jobject;
  javaString: jstring;
  lParams: array[0..2] of JValue;
  lNativeString: PChar;
{$endif}
begin
{$ifdef Android}
  DebugLn(':>LoadHTMLPageViaJNI');
  // First call FindClass for all required classes
  javaClass_DefaultHttpClient := javaEnvRef^^.FindClass(
    javaEnvRef,
    'org/apache/http/impl/client/DefaultHttpClient');
  javaClass_AbstractHttpClient := javaEnvRef^^.FindClass(
    javaEnvRef,
    'org/apache/http/impl/client/AbstractHttpClient');
  javaClass_HttpGet := javaEnvRef^^.FindClass(javaEnvRef,
    'org/apache/http/client/methods/HttpGet');
  javaClass_URI := javaEnvRef^^.FindClass(javaEnvRef,
    'java/net/URI');
  javaClass_HttpResponse := javaEnvRef^^.FindClass(
    javaEnvRef, 'org/apache/http/HttpResponse');
  javaClass_HttpEntity := javaEnvRef^^.FindClass(
    javaEnvRef, 'org/apache/http/HttpEntity');
  javaClass_InputStreamReader := javaEnvRef^^.FindClass(
    javaEnvRef, 'java/io/InputStreamReader');
  javaClass_BufferedReader := javaEnvRef^^.FindClass(
    javaEnvRef, 'java/io/BufferedReader');

  // Now all Method IDs
  DebugLn(':LoadHTMLPageViaJNI 1');
  javaMethod_DefaultHttpClient_new := javaEnvRef^^.GetMethodID(javaEnvRef, javaClass_DefaultHttpClient, '<init>', '()V');
  javaMethod_DefaultHttpClient_execute := javaEnvRef^^.GetMethodID(javaEnvRef, javaClass_AbstractHttpClient, 'execute',
    '(Lorg/apache/http/client/methods/HttpUriRequest;)Lorg/apache/http/HttpResponse;');
  javaMethod_HttpGet_new := javaEnvRef^^.GetMethodID(javaEnvRef, javaClass_HttpGet, '<init>', '()V');
  javaMethod_HttpGet_setURI := javaEnvRef^^.GetMethodID(javaEnvRef, javaClass_HttpGet, 'setURI', '(Ljava/net/URI;)V');
  javaMethod_URI_new := javaEnvRef^^.GetMethodID(javaEnvRef, javaClass_URI, '<init>', '(Ljava/lang/String;)V');
  javaMethod_HttpResponse_getEntity := javaEnvRef^^.GetMethodID(javaEnvRef, javaClass_HttpResponse, 'getEntity', '()Lorg/apache/http/HttpEntity;');
  javaMethod_HttpEntity_getContent := javaEnvRef^^.GetMethodID(javaEnvRef, javaClass_HttpEntity, 'getContent', '()Ljava/io/InputStream;');
  javaMethod_InputStreamReader_new := javaEnvRef^^.GetMethodID(javaEnvRef, javaClass_InputStreamReader, '<init>', '(Ljava/io/InputStream;)V');
  javaMethod_BufferedReader_new := javaEnvRef^^.GetMethodID(javaEnvRef, javaClass_BufferedReader, '<init>', '(Ljava/io/Reader;)V');
  javaMethod_BufferedReader_readLine := javaEnvRef^^.GetMethodID(javaEnvRef, javaClass_BufferedReader, 'readLine', '()Ljava/lang/String;');
  javaMethod_BufferedReader_close := javaEnvRef^^.GetMethodID(javaEnvRef, javaClass_BufferedReader, 'close', '()V');

  // Create a new DefaultHttpClient instance
  // HttpClient javaHttpClient = new DefaultHttpClient();
  javaHttpClient := javaEnvRef^^.NewObject(javaEnvRef,
    javaClass_DefaultHttpClient,
    javaMethod_DefaultHttpClient_new);

  DebugLn(':LoadHTMLPageViaJNI 3 javaHttpClient='+IntToHex(PtrInt(javaHttpClient), 8));
  // Create a new instance for the HTTP request object
  // HttpGet javaRequest = new HttpGet();
  javaRequest := javaEnvRef^^.NewObject(javaEnvRef,
    javaClass_HttpGet,
    javaMethod_HttpGet_new);

  DebugLn(':LoadHTMLPageViaJNI 4 javaRequest='+IntToHex(PtrInt(javaRequest), 8));
  // Add a URI for the request object
  // URI javaURI = new URI("http://w3mentor.com/");
  lParams[0].l := javaEnvRef^^.NewStringUTF(javaEnvRef, PChar(AURL));
  javaURI := javaEnvRef^^.NewObjectA(javaEnvRef,
    javaClass_URI, javaMethod_URI_new, @lParams[0]);
  javaEnvRef^^.DeleteLocalRef(javaEnvRef, lParams[0].l);
  // javaRequest.setURI(javaURI);
  lParams[0].l := javaURI;
  javaEnvRef^^.CallVoidMethodA(javaEnvRef, javaRequest,
    javaMethod_HttpGet_setURI, @lParams[0]);

  DebugLn(':LoadHTMLPageViaJNI 5');
  // Execute the HTTP request and obtain a HTTP response
  // HttpResponse response = javaHttpClient.execute(javaRequest);
  lParams[0].l := javaRequest;
  javaResponse := javaEnvRef^^.CallObjectMethodA(javaEnvRef, javaHttpClient,
    javaMethod_DefaultHttpClient_execute, @lParams[0]);

  DebugLn(':LoadHTMLPageViaJNI 6 javaResponse='+IntToHex(PtrInt(javaResponse), 8));
  // HttpEntity javaEntity = response.getEntity();
  javaEntity := javaEnvRef^^.CallObjectMethod(javaEnvRef, javaResponse,
    javaMethod_HttpResponse_getEntity);
  DebugLn(':LoadHTMLPageViaJNI 6.1');
  // InputStream javaContent = javaEntity.getContent();
  javaContent := javaEnvRef^^.CallObjectMethod(javaEnvRef, javaEntity,
    javaMethod_HttpEntity_getContent);
  DebugLn(':LoadHTMLPageViaJNI 6.2');
  // javaStreamReader = new InputStreamReader(javaContent)
  lParams[0].l := javaContent;
  javaStreamReader := javaEnvRef^^.NewObjectA(javaEnvRef,
    javaClass_InputStreamReader,
    javaMethod_InputStreamReader_new, @lParams[0]);
  // javaBufferedReader = new BufferedReader(javaStreamReader);
  DebugLn(':LoadHTMLPageViaJNI 6.3 javaStreamReader='+IntToHex(PtrInt(javaStreamReader), 8));
  lParams[0].l := javaStreamReader;
  DebugLn(':LoadHTMLPageViaJNI 6.4');
  javaBufferedReader := javaEnvRef^^.NewObjectA(javaEnvRef,
    javaClass_BufferedReader,
    javaMethod_BufferedReader_new, @lParams[0]);

  // String line = "";
  // while ((line = javaBufferedReader.readLine()) != null)
  while True do
  begin
    javaString := javaEnvRef^^.CallObjectMethod(javaEnvRef,
      javaBufferedReader,
      javaMethod_BufferedReader_readLine);
    if javaString = nil then Break;
    lNativeString := javaEnvRef^^.GetStringUTFChars(
      javaEnvRef, JavaString, nil);
    DebugLn(lNativeString);
    Result := lNativeString;
    javaEnvRef^^.ReleaseStringUTFChars(javaEnvRef,
      JavaString, lNativeString);
    javaEnvRef^^.DeleteLocalRef(javaEnvRef, javaString);
  end;

  // javaBufferedReader.close();
  javaEnvRef^^.CallVoidMethod(javaEnvRef, javaBufferedReader,
    javaMethod_BufferedReader_close);
{$endif}
end;

procedure Tform1.MyOnListViewDialogResult(ASelectedItem: Integer);
begin
  DebugLn(Format('[MyOnListViewDialogResult] ASelectedItem=%d', [ASelectedItem]));
end;

end.

