unit MainUnit;

{$mode objfpc}{$H+}

{$define UsePreview}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  {$ifdef UsePreview}
  OsPrinters,
  {$endif}
  FileUtil, IpHtml, ExtCtrls, StdCtrls;

type
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
  end;
//  TIpSimpleHtmlDataProvider = class(TIpAbstractHtmlDataProvider)
//  end;

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    IpHtmlPanel1: TIpHtmlPanel;
    OpenDialog1: TOpenDialog;
    OpenHTMLFileButton1: TButton;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure IpHtmlPanel1HotClick(Sender: TObject);
    procedure MainFormCreate(Sender: TObject);
    procedure OpenHTMLFileButtonClick(Sender: TObject);
  public
    procedure OpenHTMLFile(const Filename: string);
  end; 

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.MainFormCreate(Sender: TObject);
begin
  OpenHTMLFile('index.html');
end;

procedure TMainForm.OpenHTMLFileButtonClick(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    OpenHtmlFile(OpenDialog1.FileName);
  end;
end;

procedure TMainForm.IpHtmlPanel1HotClick(Sender: TObject);
var
  NodeA: TIpHtmlNodeA;
  NewFilename: String;
begin
  if IpHtmlPanel1.HotNode is TIpHtmlNodeA then begin
    NodeA:=TIpHtmlNodeA(IpHtmlPanel1.HotNode);
    NewFilename:=NodeA.HRef;
    OpenHTMLFile(NewFilename);
  end;
end;

procedure TMainForm.HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
var
  PicCreated: boolean;
begin
  try
    if FileExistsUTF8(URL) then begin
      PicCreated := False;
      if Picture=nil then begin
        Picture:=TPicture.Create;
        PicCreated := True;
      end;
      Picture.LoadFromFile(URL);
    end;
  except
    if PicCreated then
      Picture.Free;
    Picture := nil;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  {$ifdef UsePreview}
  IpHtmlPanel1.PrintPreview;
  {$else}
  ShowMessage(
    'In order to use this feature, please read instructions'#13+
    'contained in the readme file included in this project'#13+
    'directory');
  {$endif}
end;

procedure TMainForm.OpenHTMLFile(const Filename: string);
var
  fs: TFileStream;
  NewHTML: TSimpleIpHtml;
begin
  try
    fs:=TFileStream.Create(UTF8ToSys(Filename),fmOpenRead);
    try
      NewHTML:=TSimpleIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
      NewHTML.OnGetImageX:=@HTMLGetImageX;
      NewHTML.LoadFromStream(fs);
    finally
      fs.Free;
    end;
    IpHtmlPanel1.SetHtml(NewHTML);
  except
    on E: Exception do begin
      MessageDlg('Unable to open HTML file',
        'HTML File: '+Filename+#13
        +'Error: '+E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

initialization
  {$I defaultimage.lrs}

end.

