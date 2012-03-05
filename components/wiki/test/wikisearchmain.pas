unit WikiSearchMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazLogger, LazUTF8, LazFileUtils, IpHtml,
  Ipfilebroker, IpMsg, CodeToolManager, CodeCache, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, WikiHelpManager;

type

  { TWikiIpHtmlDataProvider }

  TWikiIpHtmlDataProvider = class(TIpHtmlDataProvider)
  private
  protected
    function DoGetStream(const URL: string): TStream; override;
  public
  end;

  { TWikiSearchDemoForm }

  TWikiSearchDemoForm = class(TForm)
    LanguagesEdit: TEdit;
    LanguagesLabel: TLabel;
    PageIpHtmlPanel: TIpHtmlPanel;
    ProgressLabel: TLabel;
    MainGroupBox: TGroupBox;
    ResultsIpHtmlPanel: TIpHtmlPanel;
    SearchEdit: TEdit;
    SearchLabel: TLabel;
    Splitter1: TSplitter;
    Timer1: TTimer;
    function DataProviderCanHandle(Sender: TObject; const URL: string): Boolean;
    procedure DataProviderCheckURL(Sender: TObject; const URL: string;
      var Available: Boolean; var ContentType: string);
    procedure DataProviderGetHtml(Sender: TObject; const URL: string;
      const PostData: TIpFormDataEntity; var Stream: TStream);
    procedure DataProviderGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure DataProviderLeave(Sender: TIpHtml);
    procedure DataProviderReportReference(Sender: TObject; const URL: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LanguagesEditChange(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure ResultsIpHtmlPanelDocumentOpen(Sender: TObject);
    procedure ResultsIpHtmlPanelHotClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WikiHelpScanned(Sender: TObject);
    procedure WikiHelpSearched(Sender: TObject);
  private
    fLastSearchText: string;
    fLastLanguages: string;
    FIdleConnected: boolean;
    FURLDataProvider: TWikiIpHtmlDataProvider;
    procedure SearchParamsChanged;
    procedure SetIdleConnected(AValue: boolean);
    procedure UpdateProgress;
    procedure LoadHTML(Target: TIpHtmlPanel; HTML: string);
  public
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

var
  WikiSearchDemoForm: TWikiSearchDemoForm;

implementation

{ TWikiIpHtmlDataProvider }

function TWikiIpHtmlDataProvider.DoGetStream(const URL: string): TStream;
begin
  Result:=nil;
  if URL='' then exit;
  if URL=WikiHelp.ResultsCSSURL then begin
    debugln(['TWikiIpHtmlDataProvider.DoGetStream loading css ']);
    Result:=TMemoryStream.Create;
    if WikiHelp.ResultsCSS<>'' then
      Result.Write(WikiHelp.ResultsCSS[1],length(WikiHelp.ResultsCSS));
    Result.Position:=0;
    exit;
  end;
  debugln(['TWikiIpHtmlDataProvider.DoGetStream ',URL]);
end;

{$R *.lfm}

{ TWikiSearchDemoForm }

procedure TWikiSearchDemoForm.FormCreate(Sender: TObject);
var
  Code: TCodeBuffer;
begin
  Caption:='Search Wiki (Proof of concept)';
  SearchLabel.Caption:='Search:';
  SearchEdit.Text:='Documentation';
  SearchEdit.Hint:='Type one or more words separated by space, use " for phrases with spaces';
  LanguagesLabel.Caption:='Languages:';
  LanguagesEdit.Text:='';
  LanguagesEdit.Hint:='Empty for only original/untranslated pages, "de" to include german pages, "-,de" for german pages only';
  MainGroupBox.Caption:='Result:';

  FURLDataProvider:=TWikiIpHtmlDataProvider.Create(Self);
  ResultsIpHtmlPanel.DataProvider:=FURLDataProvider;
  PageIpHtmlPanel.DataProvider:=FURLDataProvider;
  FURLDataProvider.OnCanHandle:=@DataProviderCanHandle;
  FURLDataProvider.OnGetHtml:=@DataProviderGetHtml;
  FURLDataProvider.OnGetImage:=@DataProviderGetImage;
  FURLDataProvider.OnLeave:=@DataProviderLeave;
  FURLDataProvider.OnCheckURL:=@DataProviderCheckURL;
  FURLDataProvider.OnReportReference:=@DataProviderReportReference;

  WikiHelp:=TWikiHelp.Create(nil);
  WikiHelp.XMLDirectory:=SetDirSeparators('../wikixml');
  WikiHelp.ImagesDirectory:=SetDirSeparators('../images');
  WikiHelp.Converter.OutputDir:='';
  WikiHelp.Converter.CSSFilename:='wiki.css';
  WikiHelp.OnScanned:=@WikiHelpScanned;
  WikiHelp.OnSearched:=@WikiHelpSearched;
  WikiHelp.ResultsCSSURL:='wiki.css';
  Code:=CodeToolBoss.LoadFile(TrimAndExpandFilename(SetDirSeparators('../html/wiki.css')),true,false);
  if Code<>nil then
    WikiHelp.ResultsCSS:=Code.Source;

  LoadHTML(ResultsIpHtmlPanel,'');
  LoadHTML(PageIpHtmlPanel,'');

  WikiHelp.StartLoading;
  UpdateProgress;
  Timer1.Enabled:=true;
end;

function TWikiSearchDemoForm.DataProviderCanHandle(Sender: TObject;
  const URL: string): Boolean;
begin
  debugln(['TWikiSearchDemoForm.DataProviderCanHandle URL=',URL]);
  Result:=false;
end;

procedure TWikiSearchDemoForm.DataProviderCheckURL(Sender: TObject;
  const URL: string; var Available: Boolean; var ContentType: string);
begin
  debugln(['TWikiSearchDemoForm.DataProviderCheckURL URL=',URL]);
  Available:=false;
  ContentType:='';
end;

procedure TWikiSearchDemoForm.DataProviderGetHtml(Sender: TObject;
  const URL: string; const PostData: TIpFormDataEntity; var Stream: TStream);
begin
  debugln(['TWikiSearchDemoForm.DataProviderGetHtml URL=',URL]);
  Stream:=nil;
end;

procedure TWikiSearchDemoForm.DataProviderGetImage(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
begin
  debugln(['TWikiSearchDemoForm.DataProviderGetImage URL=',URL]);
end;

procedure TWikiSearchDemoForm.DataProviderLeave(Sender: TIpHtml);
begin
  //debugln(['TWikiSearchDemoForm.DataProviderLeave ']);
end;

procedure TWikiSearchDemoForm.DataProviderReportReference(Sender: TObject;
  const URL: string);
begin
  if URL='' then exit;
  //debugln(['TWikiSearchDemoForm.DataProviderReportReference ',URL]);
end;

procedure TWikiSearchDemoForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(WikiHelp);
end;

procedure TWikiSearchDemoForm.LanguagesEditChange(Sender: TObject);
begin
  IdleConnected:=true;
end;

procedure TWikiSearchDemoForm.OnIdle(Sender: TObject; var Done: Boolean);
begin
  SearchParamsChanged;
  IdleConnected:=false;
end;

procedure TWikiSearchDemoForm.ResultsIpHtmlPanelDocumentOpen(Sender: TObject);
begin

end;

procedure TWikiSearchDemoForm.ResultsIpHtmlPanelHotClick(Sender: TObject);
var
  HotNode: TIpHtmlNode;
  HRef: String;
  Panel: TIpHtmlPanel;
begin
  Panel:=Sender as TIpHtmlPanel;
  HotNode:=Panel.HotNode;
  if HotNode is TIpHtmlNodeA then begin
    HRef := TIpHtmlNodeA(HotNode).HRef;
    //Target := TIpHtmlNodeA(HotNode).Target;
  end else begin
    HRef := TIpHtmlNodeAREA(HotNode).HRef;
    //Target := TIpHtmlNodeAREA(HotNode).Target;
  end;
  debugln(['TWikiSearchDemoForm.ResultsIpHtmlPanelHotClick href=',href]);
end;

procedure TWikiSearchDemoForm.SearchEditChange(Sender: TObject);
begin
  IdleConnected:=true;
end;

procedure TWikiSearchDemoForm.Timer1Timer(Sender: TObject);
begin
  ProgressLabel.Caption:=WikiHelp.GetProgressCaption;
  //debugln(['TWikiSearchDemoForm.Timer1Timer ',ProgressLabel.Caption]);
  Timer1.Enabled:=WikiHelp.Busy;
end;

procedure TWikiSearchDemoForm.WikiHelpScanned(Sender: TObject);
begin
  UpdateProgress;
end;

procedure TWikiSearchDemoForm.WikiHelpSearched(Sender: TObject);
var
  HTML: String;
begin
  UpdateProgress;

  HTML:=WikiHelp.ResultsHTML;
  if HTML='' then begin
    HTML:='<html><body><h1>Search Results</h1></body></html>';
  end;
  LoadHTML(ResultsIpHtmlPanel,HTML);
end;

procedure TWikiSearchDemoForm.SearchParamsChanged;
var
  NewSearchText: String;
  NewLanguages: String;
begin
  NewSearchText:=UTF8Trim(SearchEdit.Text);
  NewLanguages:=UTF8Trim(LanguagesEdit.Text);
  if (NewSearchText=fLastSearchText) and (NewLanguages=fLastLanguages) then
    exit;
  WikiHelp.Search(NewSearchText,NewLanguages);
  Timer1.Enabled:=true;
end;

procedure TWikiSearchDemoForm.SetIdleConnected(AValue: boolean);
begin
  if FIdleConnected=AValue then Exit;
  FIdleConnected:=AValue;
  if IdleConnected then
    Application.AddOnIdleHandler(@OnIdle)
  else
    Application.RemoveOnIdleHandler(@OnIdle);
end;

procedure TWikiSearchDemoForm.UpdateProgress;
begin
  ProgressLabel.Caption:=WikiHelp.GetProgressCaption;
  Timer1.Enabled:=WikiHelp.Busy;
end;

procedure TWikiSearchDemoForm.LoadHTML(Target: TIpHtmlPanel; HTML: string);
var
  ms: TMemoryStream;
  NewHTML: TIpHtml;
begin
  if HTML='' then
    HTML:='<html><body></body></html>';
  ms:=TMemoryStream.Create;
  try
    try
      ms.Write(HTML[1],length(HTML));
      ms.Position:=0;
      NewHTML:=TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel
      //NewHTML.OnGetImageX:=@HTMLGetImageX;
      Target.SetHtml(NewHTML);
      NewHTML.LoadFromStream(ms);
    except
      on E: Exception do begin
        debugln(['TWikiSearchDemoForm.LoadHTML ',E.Message]);
      end;
    end;
  finally
    ms.Free;
  end;
end;

end.

