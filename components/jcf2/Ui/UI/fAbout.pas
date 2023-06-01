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
  Classes, SysUtils,
  // LCL
  Forms, Graphics, StdCtrls, ExtCtrls, ButtonPanel, LCLIntf, LCLType;

type

  { TfrmAboutBox }

  TfrmAboutBox = class(TForm)
    ButtonPanel1: TButtonPanel;
    pnlClient:     TPanel;
    imgOpenSource: TImage;
    mWarning:      TMemo;
    mWhat:         TMemo;
    lblMPL:        TLabel;
    hlHomePage:    TLabel;

    procedure FormCreate(Sender: TObject);
    procedure imgOpenSourceClick(Sender: TObject);
    procedure lblMPLClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var {%H-}Key: word; {%H-}Shift: TShiftState);
    procedure hlHomePageClick(Sender: TObject);
    procedure lblMPLMouseEnter(Sender: TObject);
    procedure lblMPLMouseLeave(Sender: TObject);
  private
  public
  end;

implementation

{$R *.lfm}

uses
  { local }
  JcfHelp, JcfVersionConsts, JcfStringUtils, JcfUIConsts;

procedure ShowURL(const ps: string);
begin
  // do it silently
  OpenURL(ps);
end;

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

  //LCL already has 'default' font
  //SetObjectFontToSystemFont(Self);

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
  if Key = VK_F1 then
    try
      Application.HelpContext(HELP_MAIN);
    except
      if FileExists(Application.HelpFile) then
        OpenDocument(Application.HelpFile);
    end;
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
