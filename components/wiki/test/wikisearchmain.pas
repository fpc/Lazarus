{ Browse and search form for offline wiki

  Copyright (C) 2012  Mattias Gaertner  mattias@freepascal.org

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.

}
unit WikiSearchMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math, FileUtil, LazLogger, LazUTF8, LazFileUtils, laz2_DOM,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls,
  IpHtml, Ipfilebroker, IpMsg,
  CodeToolManager, CodeCache,
  WikiHelpManager, WikiSearchOptions;

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
    HideSearchButton: TButton;
    OptionsButton: TButton;
    PagePanel: TPanel;
    PageIpHtmlPanel: TIpHtmlPanel;
    PageToolBar: TToolBar;
    ProgressLabel: TLabel;
    ResultsIpHtmlPanel: TIpHtmlPanel;
    SearchEdit: TEdit;
    SearchPanel: TPanel;
    SearchLabel: TLabel;
    Splitter1: TSplitter;
    Timer1: TTimer;
    ShowSearchToolButton: TToolButton;
    function DataProviderCanHandle(Sender: TObject; const {%H-}URL: string): Boolean;
    procedure DataProviderGetImage(Sender: TIpHtmlNode; const URL: string;
      var Picture: TPicture);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure HideSearchButtonClick(Sender: TObject);
    procedure LanguagesEditChange(Sender: TObject);
    procedure IpHtmlPanelHotClick(Sender: TObject);
    procedure OptionsButtonClick(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure ShowSearchToolButtonClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    fLastSearchText: string;
    fLastLanguages: string;
    FIdleConnected: boolean;
    FURLDataProvider: TWikiIpHtmlDataProvider;
    procedure SearchParamsChanged;
    procedure SetIdleConnected(AValue: boolean);
    procedure UpdateProgress;
    procedure LoadHTML(Target: TIpHtmlPanel; HTML: string); overload;
    procedure LoadHTML(Target: TIpHtmlPanel; aStream: TStream); overload;
    procedure ShowOptions;
    procedure WikiHelpScanned(Sender: TObject);
    procedure WikiHelpSearched(Sender: TObject);
    procedure OnIdle(Sender: TObject; var {%H-}Done: Boolean);
    function GetLanguages: string;
  public
    property IdleConnected: boolean read FIdleConnected write SetIdleConnected;
  end;

var
  WikiSearchDemoForm: TWikiSearchDemoForm = nil;

implementation

{ TWikiIpHtmlDataProvider }

function TWikiIpHtmlDataProvider.DoGetStream(const URL: string): TStream;
begin
  Result:=nil;
  if URL='' then exit;
  if URL=WikiHelp.ResultsCSSURL then begin
    //debugln(['TWikiIpHtmlDataProvider.DoGetStream loading css ']);
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

  // search panel
  SearchLabel.Caption:='Search:';
  SearchEdit.Text:='Documentation';
  SearchEdit.Hint:='Type one or more words separated by space, use " for phrases with spaces';
  HideSearchButton.Caption:='Hide';
  OptionsButton.Caption:='Options';

  // page panel
  ShowSearchToolButton.Caption:='Search';

  FURLDataProvider:=TWikiIpHtmlDataProvider.Create(nil);
  ResultsIpHtmlPanel.DataProvider:=FURLDataProvider;
  PageIpHtmlPanel.DataProvider:=FURLDataProvider;
  FURLDataProvider.OnCanHandle:=@DataProviderCanHandle;
  FURLDataProvider.OnGetImage:=@DataProviderGetImage;

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
  //debugln(['TWikiSearchDemoForm.DataProviderCanHandle URL=',URL]);
  Result:=false;
end;

procedure TWikiSearchDemoForm.DataProviderGetImage(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  Filename: String;
  PicCreated: Boolean;
begin
  //debugln(['TWikiSearchDemoForm.DataProviderGetImage URL=',URL]);
  Filename:=WikiHelp.ImagesDirectory+URL;
  if not FileExistsUTF8(Filename) then begin
    debugln(['TWikiSearchDemoForm.DataProviderGetImage image not found "',Filename,'"']);
    exit;
  end;
  PicCreated := False;
  try
    if Picture=nil then begin
      Picture:=TPicture.Create;
      PicCreated := True;
    end;
    Picture.LoadFromFile(Filename);
  except
    if PicCreated then
      Picture.Free;
    Picture := nil;
  end;
end;

procedure TWikiSearchDemoForm.FormDestroy(Sender: TObject);
begin
  // free pages before dataprovider
  FreeAndNil(ResultsIpHtmlPanel);
  FreeAndNil(PageIpHtmlPanel);
  FreeAndNil(FURLDataProvider);
  FreeAndNil(WikiHelp);
end;

procedure TWikiSearchDemoForm.HideSearchButtonClick(Sender: TObject);
begin
  DisableAutoSizing;
  try
    SearchPanel.Hide;
    Splitter1.Hide;
    ShowSearchToolButton.Visible:=true;
  finally
    EnableAutoSizing;
  end;
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

function TWikiSearchDemoForm.GetLanguages: string;
begin
  if WikiSearchOptsWnd<>nil then
    Result:=WikiSearchOptsWnd.Languages
  else
    Result:='';
end;

procedure TWikiSearchDemoForm.IpHtmlPanelHotClick(Sender: TObject);
var
  HotNode: TIpHtmlNode;
  HRef: String;
  Panel: TIpHtmlPanel;
  ms: TMemoryStream;
  Src: String;
  DocumentName: String;
  p: SizeInt;
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
  // open page in PageIpHtmlPanel
  ms:=TMemoryStream.Create;
  try
    try
      DocumentName:=HRef;
      p:=Pos('#',DocumentName);
      if p>0 then begin
        DocumentName:=LeftStr(DocumentName,p-1);
        // ToDo: anchor
      end;
      WikiHelp.SavePageToStream(DocumentName,ms);
      ms.Position:=0;
      LoadHTML(PageIpHtmlPanel,ms);
    except
      on E: Exception do begin
        Src:='<html><body>Error: '+EncodeLesserAndGreaterThan(E.Message)+'</body></html>';
        LoadHTML(PageIpHtmlPanel,Src);
      end;
    end;
  finally
    ms.Free;
  end;
end;

procedure TWikiSearchDemoForm.OptionsButtonClick(Sender: TObject);
begin
  ShowOptions;
end;

procedure TWikiSearchDemoForm.SearchEditChange(Sender: TObject);
begin
  IdleConnected:=true;
end;

procedure TWikiSearchDemoForm.ShowSearchToolButtonClick(Sender: TObject);
begin
  DisableAutoSizing;
  try
    Splitter1.Show;
    SearchPanel.Show;
    SearchPanel.Width:=Max(SearchPanel.Width,30);
    Splitter1.Left:=Max(Splitter1.Left,SearchPanel.Width);
    ShowSearchToolButton.Visible:=false;
  finally
    EnableAutoSizing;
  end;
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
  if WikiSearchOptsWnd<>nil then
    WikiSearchOptsWnd.UpdateAvailableLanguages;
end;

procedure TWikiSearchDemoForm.WikiHelpSearched(Sender: TObject);
var
  HTML: String;
begin
  UpdateProgress;

  HTML:=WikiHelp.ResultsHTML;
  if HTML='' then exit;
  LoadHTML(ResultsIpHtmlPanel,HTML);
end;

procedure TWikiSearchDemoForm.SearchParamsChanged;
var
  NewSearchText: String;
  NewLanguages: String;
begin
  NewSearchText:=UTF8Trim(SearchEdit.Text);
  NewLanguages:=GetLanguages;
  if (NewSearchText=fLastSearchText) and (NewLanguages=fLastLanguages) then
    exit;
  fLastSearchText:=NewSearchText;
  fLastLanguages:=NewLanguages;
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
begin
  if HTML='' then
    HTML:='<html><body></body></html>';
  ms:=TMemoryStream.Create;
  try
    try
      ms.Write(HTML[1],length(HTML));
      ms.Position:=0;
      LoadHTML(Target,ms);
    except
      on E: Exception do begin
        debugln(['TWikiSearchDemoForm.LoadHTML ',E.Message]);
      end;
    end;
  finally
    ms.Free;
  end;
end;

procedure TWikiSearchDemoForm.LoadHTML(Target: TIpHtmlPanel; aStream: TStream);
var
  NewHTML: TIpHtml;
begin
  try
    NewHTML:=TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel
    //NewHTML.OnGetImageX:=@HTMLGetImageX;
    Target.SetHtml(NewHTML);
    NewHTML.LoadFromStream(aStream);
  except
    on E: Exception do begin
      debugln(['TWikiSearchDemoForm.LoadHTML ',E.Message]);
    end;
  end;
end;

procedure TWikiSearchDemoForm.ShowOptions;
begin
  if WikiSearchOptsWnd=nil then
    WikiSearchOptsWnd:=TWikiSearchOptsWnd.Create(Self);
  WikiSearchOptsWnd.UpdateAvailableLanguages;
  WikiSearchOptsWnd.ShowOnTop;
end;

end.

