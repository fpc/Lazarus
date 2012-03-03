unit WikiSearchMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, IpHtml, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, WikiHelpManager;

type

  { TWikiSearchDemoForm }

  TWikiSearchDemoForm = class(TForm)
    PageGroupBox: TGroupBox;
    PageIpHtmlPanel: TIpHtmlPanel;
    ResultsGroupBox: TGroupBox;
    ResultsIpHtmlPanel: TIpHtmlPanel;
    SearchEdit: TEdit;
    SearchLabel: TLabel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
  private
    procedure SearchParamsChanged;
  public
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
  SearchEdit.Text:='';
  ResultsGroupBox.Caption:='Result:';
  PageGroupBox.Caption:='Page:';

  WikiHelp:=TWikiHelp.Create(nil);
  WikiHelp.XMLDirectory:=SetDirSeparators('../wikixml');
  WikiHelp.ImagesDirectory:=SetDirSeparators('../images');
  WikiHelp.Converter.OutputDir:='';
  WikiHelp.Converter.CSSFilename:='wiki.css';

  WikiHelp.StartLoading;
end;

procedure TWikiSearchDemoForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(WikiHelp);
end;

procedure TWikiSearchDemoForm.SearchEditChange(Sender: TObject);
begin
  SearchParamsChanged;
end;

procedure TWikiSearchDemoForm.SearchParamsChanged;
begin

end;

end.

