unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  //LazJpeg,
  IpHtml;

type
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
  end;
//  TIpSimpleHtmlDataProvider = class(TIpAbstractHtmlDataProvider)
//  end;

  TMainForm = class(TForm)
    IpHtmlPanel1: TIpHtmlPanel;
    OpenDialog1: TOpenDialog;
    OpenHTMLFileButton: TButton;
    procedure HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure IpHtmlPanel1HotClick(Sender: TObject);
    procedure MainFormCreate(Sender: TObject);
    procedure MainFormDestroy(Sender: TObject);
    procedure OpenHTMLFileButtonClick(Sender: TObject);
  public
    FDefaultImage: TBitmap;
    procedure OpenHTMLFile(const Filename: string);
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.MainFormCreate(Sender: TObject);
begin

  FDefaultImage := TBitmap.Create;
  FDefaultImage.LoadFromFile('imagebroken.xpm');

  OpenHTMLFile('index.html');
end;

procedure TMainForm.MainFormDestroy(Sender: TObject);
begin
  FDefaultImage.Free;
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
begin
  try
    if Picture=nil then
      Picture:=TPicture.Create;
    Picture.LoadFromFile(URL);
  except
    Picture.Assign(FDefaultImage);
    {
    on E: Exception do begin
      MessageDlg('Unable to open image file',
        'Image file: '+URL+#13
        +'Error: '+E.Message,mtError,[mbCancel],0);
    end;
    }
  end;
end;

procedure TMainForm.OpenHTMLFile(const Filename: string);
var
  fs: TFileStream;
  NewHTML: TSimpleIpHtml;
begin
  try
    fs:=TFileStream.Create(Filename,fmOpenRead);
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
  {$I mainunit.lrs}

end.

