unit htmlwithcssfrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  IPHtml, Ipfilebroker, IpMsg;

type

  { TMyIpHtmlDataProvider }

  TMyIpHtmlDataProvider = class(TIpHtmlDataProvider)
  protected
    function DoGetStream(const URL: string): TStream; override;
  public

  end;

  { TForm1 }

  TForm1 = class(TForm)
    function DataProvider1CanHandle(Sender: TObject; const URL: string
      ): Boolean;
    procedure DataProvider1CheckURL(Sender: TObject; const URL: string;
      var Available: Boolean; var ContentType: string);
    procedure DataProvider1GetHtml(Sender: TObject; const URL: string;
      const PostData: TIpFormDataEntity; var Stream: TStream);
    procedure DataProvider1GetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure DataProvider1Leave(Sender: TIpHtml);
    procedure DataProvider1ReportReference(Sender: TObject; const URL: string);
    procedure FormCreate(Sender: TObject);
  private
    procedure ShowHTML(Src: string);
  public
    IpHtmlPanel1: TIpHtmlPanel;
    DataProvider1: TMyIpHtmlDataProvider;
  end;

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TMyIpHtmlDataProvider }

function TMyIpHtmlDataProvider.DoGetStream(const URL: string): TStream;
var
  ms: TMemoryStream;
begin
  Result:=nil;
  debugln(['TMyIpHtmlDataProvider.DoGetStream ',URL]);

  if URL='fpdoc.css' then begin
    debugln(['TMyIpHtmlDataProvider.DoGetStream ',FileExists(URL)]);
    ms:=TMemoryStream.Create;
    try
      ms.LoadFromFile(URL);
      ms.Position:=0;
    except
      ms.Free;
    end;
    Result:=ms;
  end;
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  DataProvider1:=TMyIpHtmlDataProvider.Create(Self);
  with DataProvider1 do begin
    Name:='DataProvider1';
    OnCanHandle:=@DataProvider1CanHandle;
    OnGetHtml:=@DataProvider1GetHtml;
    OnGetImage:=@DataProvider1GetImage;
    OnLeave:=@DataProvider1Leave;
    OnCheckURL:=@DataProvider1CheckURL;
    OnReportReference:=@DataProvider1ReportReference;
  end;
  IpHtmlPanel1:=TIpHtmlPanel.Create(Self);
  with IpHtmlPanel1 do begin
    Name:='IpHtmlPanel1';
    Parent:=Self;
    Align:=alClient;
    DefaultFontSize:=10;
    DataProvider:=DataProvider1;
  end;

  ShowHTML(
     '<HTML>'
    +'<head>'
    +'<link rel="stylesheet" href="fpdoc.css" type="text/css">'
    +'</head>'
    +'<BODY>'
    +'<h1>TComponent.Name</h1>'
    +'<p>Name of the component.</p>'
    +'<h2>Declaration</h2>'
    +'<p>Source position: classesh.inc line 1678</p>'
    +'<table cellpadding="0" cellspacing="0">'
    +'<tr>'
    +'<td><p><tt><span class="code"> <span class="kw">published</span>'
    +'  <span class="kw">property </span>'
    +'<a href="../classes/tcomponent.html">TComponent</a>'
    +'<span class="sym">.</span>Name<span class="sym">: </span>'
    +'<a href="../classes/tcomponentname.html">TComponentName</a>'
    +'<br>&nbsp;&nbsp;<span class="kw">read </span>FName<br>&nbsp;&nbsp;'
    +'<span class="kw">write </span>SetName<br>&nbsp;&nbsp;'
    +'<span class="kw">stored </span>False<span class="sym">;</span></span></tt></p></td>'
    +'</tr>'
    +'</table>'
    +'<h2>Description</h2>'
    +'<p><var>Name</var> is the name of the component. This name should be a valid identifier,'
    +' i.e. must start with a letter or underscore, and can contain only letters, numbers and'
    +' the underscore character. When attempting to set the name of a component, the name'
    +' will be checked for validity. Furthermore, when a component is owned by another component,'
    +' the name must be either empty or must be unique among the child component names. </p>'
    +'<p>By "letters", 7-bit letters are meant. </p>'
    +'<h2>Errors</h2>'
    +'<p>Attempting to set the name to an invalid value will result in an exception being raised. </p>'
    +'<h2>See also</h2>'
    +'<table cellpadding="0" cellspacing="0">'
    +'<tr>'
    +'<td valign="top"><p><a href="../classes/tcomponent.validaterename.html">ValidateRename</a></p></td>'
    +'<td><p>&nbsp;&nbsp;</p></td>'
    +'<td><p class="cmt">Called when a name change must be validated</p></td>'
    +'</tr>'
    +'<tr>'
    +'<td valign="top"><p><a href="../classes/tcomponent.owner.html">Owner</a></p></td>'
    +'<td><p>&nbsp;&nbsp;</p></td>'
    +'<td><p class="cmt">Owner of this component.</p></td>'
    +'</tr>'
    +'</table>'
    +'</BODY></HTML>');
end;

function TForm1.DataProvider1CanHandle(Sender: TObject; const URL: string
  ): Boolean;
begin
  debugln(['TForm1.DataProvider1CanHandle ',URL]);
  Result:=false;
end;

procedure TForm1.DataProvider1CheckURL(Sender: TObject; const URL: string;
  var Available: Boolean; var ContentType: string);
begin
  debugln(['TForm1.DataProvider1CheckURL ',URL]);
  Available:=false;
  ContentType:='';
end;

procedure TForm1.DataProvider1GetHtml(Sender: TObject; const URL: string;
  const PostData: TIpFormDataEntity; var Stream: TStream);
begin
  debugln(['TForm1.DataProvider1GetHtml ',URL]);
  Stream:=nil;
end;

procedure TForm1.DataProvider1GetImage(Sender: TIpHtmlNode; const URL: string;
  var Picture: TPicture);
begin
  debugln(['TForm1.DataProvider1GetImage ',URL]);
  Picture:=nil;
end;

procedure TForm1.DataProvider1Leave(Sender: TIpHtml);
begin

end;

procedure TForm1.DataProvider1ReportReference(Sender: TObject; const URL: string
  );
begin
  debugln(['TForm1.DataProvider1ReportReference ',URL]);
end;

procedure TForm1.ShowHTML(Src: string);
var
  ss: TStringStream;
  NewHTML: TIpHtml;
begin
  ss := TStringStream.Create(Src);
  try
    NewHTML := TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
    debugln(['TForm1.ShowHTML BEFORE SETHTML']);
    IpHtmlPanel1.SetHtml(NewHTML);
    debugln(['TForm1.ShowHTML BEFORE LOADFROMSTREAM']);
    NewHTML.LoadFromStream(ss);
    //if Anchor <> '' then IpHtmlPanel1.MakeAnchorVisible(Anchor);
  finally
    ss.Free;
  end;
end;

end.

