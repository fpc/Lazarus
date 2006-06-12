{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

  Abstract:
    This example implements a help viewer using the turbo power
    ipro browser component.
    
    procedure RegisterHelpViewer;
    
    need to be called to register this viewer. The sample calls it in the
    OnCreate of the main form.
}

unit HtmlHelp2Viewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  LCLProc, IpHtml, Buttons, helpintfs, lazhelpintf, ComCtrls, ipfilebroker,
  iputils;

type

  { THelpViewerForm }
  
  THelpViewerForm = class(TForm)
    IHP: TIpHtmlPanel;
    DataProvider: TIpFileDataProvider;
    Panel1: TPanel;
    IndexButton: TSpeedButton;
    BackButton: TSpeedButton;
    ForwardButton: TSpeedButton;
    StatusBar1: TStatusBar;
    procedure BackButtonClick(Sender: TObject);
    procedure ForwardButtonClick(Sender: TObject);
    procedure IHPDocumentOpen(Sender: TObject);
    procedure IHPHotChange(Sender: TObject);
    procedure IndexButtonClick(Sender: TObject);
  private
  public
    { public declarations }
    procedure showURL(URL : String);
  end; 

var
  HelpViewerForm: THelpViewerForm;

procedure RegisterHelpViewer;

implementation

type

  { THTMLHelpViewer }

  THTMLHelpViewer = class(THelpViewer)
  private
  public
    constructor Create(TheOwner: TComponent); override;
    function ShowNode(Node: THelpNode; var ErrMsg: string): TShowHelpResult; override;
  published
    property AutoRegister;
  end;

{ THTMLHelpViewer }

constructor THTMLHelpViewer.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  AddSupportedMimeType('text/html');
end;

function THTMLHelpViewer.ShowNode(Node: THelpNode; var ErrMsg: string): TShowHelpResult;
begin
  DebugLn (Format('THTMLHelpViewer.ShowNode: URL:"%s" ID:"%s" Context:"%d"',[Node.URL,Node.ID,Node.Context]));
  HelpViewerForm.ShowURL(Node.URL);
  result := shrSuccess;  // we should return a "better" result ;-)
end;

var Help_Viewer : THTMLHelpViewer = nil;

procedure RegisterHelpViewer;
begin
  if Help_Viewer = nil then                      // if not already done
  begin
    Help_Viewer := THTMLHelpViewer.Create(nil);  // create the viewer and
    Help_Viewer.RegisterSelf;                    // register it in the help system
  end;
end;

{ THelpViewerForm }

procedure THelpViewerForm.IndexButtonClick(Sender: TObject);
begin
  ShowHelpOrErrorForKeyword('','HTML/index.html');  // HTML is case sensitive
end;

// Show URL of a link in Status Bar
procedure THelpViewerForm.IHPHotChange(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := IHP.HotURL;
end;

procedure THelpViewerForm.BackButtonClick(Sender: TObject);
begin
  IHP.GoBack;
end;

procedure THelpViewerForm.ForwardButtonClick(Sender: TObject);
begin
  IHP.GoForward;
end;

procedure THelpViewerForm.IHPDocumentOpen(Sender: TObject);
begin
  BackButton.Enabled := IHP.canGoBack;
  ForwardButton.Enabled := IHP.canGoForward;
end;

procedure THelpViewerForm.showURL(URL : String);
begin
  Show;
  URL := expandLocalHtmlFileName (URL);
  IHP.OpenURL(URL);
  BringToFront;  // needed if already open and another help is shown
end;


initialization
  {$I htmlhelp2viewer.lrs}

end.

