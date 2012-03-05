unit WikiSearchMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LazLogger, LazUTF8, IpHtml, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ExtCtrls, WikiHelpManager;

type

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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LanguagesEditChange(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    procedure SearchEditChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure WikiHelpScanned(Sender: TObject);
    procedure WikiHelpSearched(Sender: TObject);
  private
    fLastSearchText: string;
    fLastLanguages: string;
    FIdleConnected: boolean;
    procedure SearchParamsChanged;
    procedure SetIdleConnected(AValue: boolean);
    procedure UpdateProgress;
  public
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

var
  WikiSearchDemoForm: TWikiSearchDemoForm;

implementation

{$R *.lfm}

{ TWikiSearchDemoForm }

procedure TWikiSearchDemoForm.FormCreate(Sender: TObject);
begin
  Caption:='Search Wiki (Proof of concept)';
  SearchLabel.Caption:='Search:';
  SearchEdit.Text:='Documentation';
  SearchEdit.Hint:='Type one or more words separated by space, use " for phrases with spaces';
  LanguagesLabel.Caption:='Languages:';
  LanguagesEdit.Text:='';
  LanguagesEdit.Hint:='Empty for only original/untranslated pages, "de" to include german pages, "-,de" for german pages only';
  MainGroupBox.Caption:='Result:';

  WikiHelp:=TWikiHelp.Create(nil);
  WikiHelp.XMLDirectory:=SetDirSeparators('../wikixml');
  WikiHelp.ImagesDirectory:=SetDirSeparators('../images');
  WikiHelp.Converter.OutputDir:='';
  WikiHelp.Converter.CSSFilename:='wiki.css';
  WikiHelp.OnScanned:=@WikiHelpScanned;
  WikiHelp.OnSearched:=@WikiHelpSearched;

  WikiHelp.StartLoading;
  UpdateProgress;
  Timer1.Enabled:=true;
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

procedure TWikiSearchDemoForm.SearchEditChange(Sender: TObject);
begin
  IdleConnected:=true;
end;

procedure TWikiSearchDemoForm.Timer1Timer(Sender: TObject);
begin
  ProgressLabel.Caption:=WikiHelp.GetProgressCaption;
  debugln(['TWikiSearchDemoForm.Timer1Timer ',ProgressLabel.Caption]);
  Timer1.Enabled:=WikiHelp.Busy;
end;

procedure TWikiSearchDemoForm.WikiHelpScanned(Sender: TObject);
begin
  UpdateProgress;
end;

procedure TWikiSearchDemoForm.WikiHelpSearched(Sender: TObject);
begin
  UpdateProgress;
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

end.

