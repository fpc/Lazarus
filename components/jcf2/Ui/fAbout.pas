{(*}
(*------------------------------------------------------------------------------
 Delphi Code formatter source code 

The Original Code is fAbout.pas, released April 2000.
The Initial Developer of the Original Code is Anthony Steele.
Portions created by Anthony Steele are Copyright (C) 1999-2008 Anthony Steele.
All Rights Reserved.
Contributor(s): Anthony Steele.

The contents of this file are subject to the Mozilla Public License Version 1.1
(the "License"). you may not use this file except in compliance with the License.
You may obtain a copy of the License at http://www.mozilla.org/NPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied.
See the License for the specific language governing rights and limitations 
under the License.

Alternatively, the contents of this file may be used under the terms of
the GNU General Public License Version 2 or later (the "GPL") 
See http://www.gnu.org/licenses/gpl.html
------------------------------------------------------------------------------*)
{*)}

unit fAbout;

{$I JcfGlobal.inc}

interface

uses
  { delphi }
  Classes, Forms, Graphics, Controls, StdCtrls,
  Buttons, ExtCtrls, SysUtils;

type
  TfrmAboutBox = class(TForm)
    bbOK:          TBitBtn;
    pnlClient:     TPanel;
    imgOpenSource: TImage;
    mWarning:      TMemo;
    mWhat:         TMemo;
    lblMPL:        TLabel;
    hlHomePage:    TLabel;

    procedure FormCreate(Sender: TObject);
    procedure imgOpenSourceClick(Sender: TObject);
    procedure lblMPLClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure hlHomePageClick(Sender: TObject);
    procedure lblMPLMouseEnter(Sender: TObject);
    procedure lblMPLMouseLeave(Sender: TObject);
  private
  public
  end;

implementation

{$ifndef FPC}
  {$R *.dfm}
{$else}
  {$R *.lfm}
{$endif}

uses
  { delphi }
  {$ifndef fpc}
    Windows, ShellAPI, URLMon,
  {$else}
    LCLIntf,
  {$endif}
  { local }
  JcfVersionConsts, JcfHelp, JcfFontSetFunctions, JcfStringUtils, jcfuiconsts;

{$ifdef fpc}
procedure ShowURL(const ps: string);
begin
  // do it silent
  OpenURL(ps);
end;
{$else}
procedure ShowURL(const ps: string);
var
  lws: WideString;
begin
  lws := ps;
  HLinkNavigateString(nil, pWideChar(lws));
end;
{$endif}

procedure TfrmAboutBox.imgOpenSourceClick(Sender: TObject);
begin
  ShowURL('http://www.delphi-jedi.org');
end;

procedure TfrmAboutBox.lblMPLClick(Sender: TObject);
begin
  ShowURL('http://www.mozilla.org/MPL');
end;

procedure TfrmAboutBox.FormCreate(Sender: TObject);
var
  ls: string;
begin
  inherited;

  SetObjectFontToSystemFont(Self);

  Caption := lisAboutAboutJEDICodeFormat;
  mWhat.Text := Format(lisAboutVersion, [NativeLineBreak, NativeLineBreak,
    NativeLineBreak, NativeLineBreak, NativeLineBreak]);
  mWarning.Text := lisAboutThisProgramIsStillUnderDevelopment;
  lblMPL.Caption := lisAboutThisProgramIsOpenSource;

  // show the version from the program constant
  ls := mWhat.Text;
  StrReplace(ls, '$VERSION$', PROGRAM_VERSION);
  StrReplace(ls, '$DATE$', PROGRAM_DATE);
  mWhat.Text := string(ls);

  hlHomePage.Caption := Format(lisAboutFindMoreInformationOnTheWebAt,
    [PROGRAM_HOME_PAGE]);
end;

procedure TfrmAboutBox.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
{$ifndef fpc}
  if Key = VK_F1 then
    try
      Application.HelpContext(HELP_MAIN);
    except
      if FileExists(Application.HelpFile) then
        ShellExecute(Handle, 'open', PChar(Application.HelpFile), nil, nil, SW_SHOWNORMAL);
    end;
{$endif}
end;

procedure TfrmAboutBox.hlHomePageClick(Sender: TObject);
begin
  ShowURL(PROGRAM_HOME_PAGE);
end;

procedure TfrmAboutBox.lblMPLMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Color := clBlue;
  TLabel(Sender).Font.Style := [fsUnderline];
end;

procedure TfrmAboutBox.lblMPLMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Color := clWindowText;
  TLabel(Sender).Font.Style := [];
end;

end.
