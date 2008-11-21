{
 *****************************************************************************
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,        *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************

  Author: Mattias Gaertner

  Abstract:
    Installs a HTML control in the IDE using TIpHtmlPanel.
}
unit IPIDEHTMLControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, Graphics, Controls, Dialogs,
  IpHtml, IDEHelpIntf, LazHelpIntf;

type
  TSimpleIpHtml = class(TIpHtml)
  public
    property OnGetImageX;
  end;

  { TIPLazHtmlControl }

  TIPLazHtmlControl = class(TIpHtmlPanel,TIDEHTMLControlIntf)
  private
    FProvider: TAbstractIDEHTMLProvider;
    FURL: string;
    procedure SetProvider(const AValue: TAbstractIDEHTMLProvider);
    procedure HTMLGetImageX(Sender: TIpHtmlNode; const URL: string;
                            var Picture: TPicture);
  public
    constructor Create(AOwner: TComponent); override;
    function GetURL: string;
    procedure SetURL(const AValue: string);
    property Provider: TAbstractIDEHTMLProvider read FProvider write SetProvider;
    procedure SetHTMLContent(Stream: TStream);
    procedure GetPreferredControlSize(out AWidth, AHeight: integer);
  end;

function IPCreateLazIDEHTMLControl(Owner: TComponent;
  var Provider: TAbstractIDEHTMLProvider): TControl;

procedure Register;

implementation

procedure Register;
begin
  CreateIDEHTMLControl:=@IPCreateLazIDEHTMLControl;
end;

function IPCreateLazIDEHTMLControl(Owner: TComponent;
  var Provider: TAbstractIDEHTMLProvider): TControl;
var
  HTMLControl: TIPLazHtmlControl;
begin
  HTMLControl:=TIPLazHtmlControl.Create(Owner);
  Result:=HTMLControl;
  if Provider=nil then
    Provider:=CreateIDEHTMLProvider(HTMLControl);
  Provider.ControlIntf:=HTMLControl;
  HTMLControl.Provider:=Provider;
end;

{ TIPLazHtmlControl }

procedure TIPLazHtmlControl.SetProvider(const AValue: TAbstractIDEHTMLProvider
  );
begin
  if FProvider=AValue then exit;
  FProvider:=AValue;
end;

procedure TIPLazHtmlControl.HTMLGetImageX(Sender: TIpHtmlNode;
  const URL: string; var Picture: TPicture);
var
  URLType: string;
  URLPath: string;
  URLParams: string;
  Filename: String;
  Ext: String;
  Stream: TStream;
  NewURL: String;
begin
  //DebugLn(['TIPLazHtmlControl.HTMLGetImageX URL=',URL]);
  if Provider=nil then exit;
  NewURL:=Provider.BuildURL(Provider.BaseURL,URL);
  //DebugLn(['TIPLazHtmlControl.HTMLGetImageX NewURL=',NewURL,' Provider.BaseURL=',Provider.BaseURL,' URL=',URL]);

  Picture:=nil;
  Stream:=nil;
  try
    try
      SplitURL(NewURL,URLType,URLPath,URLParams);
      if URLPath='' then
        URLPath:=NewURL;
      Filename:=URLPathToFilename(URLPath);
      Ext:=ExtractFileExt(Filename);
      //DebugLn(['TIPLazHtmlControl.HTMLGetImageX URLPath=',URLPath,' Filename=',Filename,' Ext=',Ext]);
      Picture:=TPicture.Create;
      // quick check if file format is supported
      Picture.FindGraphicClassWithFileExt(Ext);
      // get stream
      Stream:=Provider.GetStream(NewURL);
      // load picture
      Picture.LoadFromStreamWithFileExt(Stream,Ext);
    finally
      if Stream<>nil then
        Provider.ReleaseStream(NewURL);
    end;
  except
    on E: Exception do begin
      FreeAndNil(Picture);
      DebugLn(['TIPLazHtmlControl.HTMLGetImageX ERROR: ',E.Message]);
    end;
  end;
end;

constructor TIPLazHtmlControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DefaultFontSize := 8;
  MarginHeight := 0;
  MarginWidth := 0; 
end;

function TIPLazHtmlControl.GetURL: string;
begin
  Result:=FURL;
end;

procedure TIPLazHtmlControl.SetURL(const AValue: string);
var
  Stream: TStream;
  NewHTML: TSimpleIpHtml;
  NewURL: String;
  ok: Boolean;
begin
  if Provider=nil then raise Exception.Create('TIPLazHtmlControl.SetURL missing Provider');
  if FURL=AValue then exit;
  NewURL:=Provider.BuildURL(Provider.BaseURL,AValue);
  if FURL=NewURL then exit;
  FURL:=NewURL;
  try
    Stream:=Provider.GetStream(FURL);
    ok:=false;
    NewHTML:=nil;
    try
      NewHTML:=TSimpleIpHtml.Create; // Beware: Will be freed automatically TIpHtmlPanel
      NewHTML.OnGetImageX:=@HTMLGetImageX;
      NewHTML.LoadFromStream(Stream);
      ok:=true;
    finally
      if not ok then NewHTML.Free;
      Provider.ReleaseStream(FURL);
    end;
    SetHtml(NewHTML);
  except
    on E: Exception do begin
      MessageDlg('Unable to open HTML file',
        'HTML File: '+FURL+#13
        +'Error: '+E.Message,mtError,[mbCancel],0);
    end;
  end;
end;

procedure TIPLazHtmlControl.SetHTMLContent(Stream: TStream);
var
  ok: Boolean;
  NewHTML: TSimpleIpHtml;
begin
  ok:=false;
  NewHTML:=nil;
  try
    NewHTML:=TSimpleIpHtml.Create; // Beware: Will be freed automatically by TIpHtmlPanel
    NewHTML.OnGetImageX:=@HTMLGetImageX;
    NewHTML.LoadFromStream(Stream);
    ok:=true;
  finally
    if not ok then NewHTML.Free;
  end;
  SetHtml(NewHTML);
end;

procedure TIPLazHtmlControl.GetPreferredControlSize(out AWidth, AHeight: integer);
begin
  with GetContentSize do
  begin
    AWidth := cx;
    AHeight := cy;
  end;
end;

end.

