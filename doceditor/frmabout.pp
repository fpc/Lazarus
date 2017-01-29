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
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

  Author: Michael Van Canneyt
}
unit FrmAbout;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, StdCtrls, ButtonPanel, LazFileUtils;

const
  LicenseFile = 'COPYING.GPL.txt';

type

  { TAboutForm }

  TAboutForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    LThisApplication: TLabel;
    LCopyRight1: TLabel;
    LCopyRight2: TLabel;
    MCopyRight: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    procedure LoadLicense;
  public
    { public declarations }
  end; 

var
  AboutForm: TAboutForm;

implementation
uses LazDEMsg;

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  Caption:=SAboutFormCaption;
  LThisApplication.Caption:=sLazDocEditor;
  LCopyRight1.Caption:=sCopyRight1;
  LCopyRight2.Caption:=sCopyRight2;
  LoadLicense;
end;

procedure TAboutForm.LoadLicense;
var
  GPLFileName:string;
begin
  GPLFileName:=AppendPathDelim(AppendPathDelim(ExtractFileDir(ParamStr(0))) + '..')+LicenseFile;
  if FileExists(GPLFileName) then
    MCopyRight.Lines.LoadFromFile(GPLFileName);
end;

end.

