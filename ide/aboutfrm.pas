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
}
unit AboutFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FPCAdds, Forms, Controls, Graphics, Dialogs, LResources,
  StdCtrls, Buttons, LazConf, LazarusIDEStrConsts, ExtCtrls, EnvironmentOpts,
  Clipbrd, FileUtil, Menus;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CloseButton: TBitBtn;
    BuildDateLabel: TLABEL;
    AboutMemo: TMEMO;
    Image1: TImage;
    LogoImage: TImage;
    LogoPage: TPage;
    miVerToClipboard: TMenuItem;
    PlatformLabel: TLabel;
    PopupMenu1: TPopupMenu;
    VersionLabel: TLABEL;
    ContributorsMemo:TMemo;
    AcknowledgementsMemo:TMemo;
    RevisionLabel: TLabel;
    Notebook1:TNotebook;
    AboutPage:TPage;
    ContributorsPage:TPage;
    AcknowledgementsPage:TPage;
    procedure AboutFormCreate(Sender:TObject);
    procedure miVerToClipboardClick(Sender: TObject);
  private
    procedure LoadContributors;
    procedure LoadAcknowledgements;
  public
    constructor Create(TheOwner: TComponent); override;
  end;


function ShowAboutForm: TModalResult;

const
  LazarusVersionStr= {$I version.inc};
var
  LazarusRevisionStr: string;
  
function GetLazarusVersionString : string;

implementation


function ShowAboutForm: TModalResult;
var
  AboutForm: TAboutForm;
begin
  AboutForm:=TAboutForm.Create(nil);
  Result:=AboutForm.ShowModal;
  AboutForm.Free;
end;

function GetLazarusVersionString: string;
begin
  Result:=format(lisLazarusVersionString,[LazarusVersionStr]);
end;

{ TAboutForm }

constructor TAboutForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

procedure TAboutForm.AboutFormCreate(Sender:TObject);
const
  DoubleLineEnding = LineEnding + LineEnding;

  {The compiler generated date string is always of the form y/m/d.
   This function gives it a string respresentation according to the
   shortdateformat}
  function GetLocalizedBuildDate(): string;
  var
    BuildDate: string;
    SlashPos1, SlashPos2: integer;
    Date: TDateTime;
  begin
    BuildDate := {$I %date%};
    SlashPos1 := Pos('/',BuildDate);
    SlashPos2 := SlashPos1 +
      Pos('/', Copy(BuildDate, SlashPos1+1, Length(BuildDate)-SlashPos1));
    Date := EncodeDate(StrToWord(Copy(BuildDate,1,SlashPos1-1)),
      StrToWord(Copy(BuildDate,SlashPos1+1,SlashPos2-SlashPos1-1)),
      StrToWord(Copy(BuildDate,SlashPos2+1,Length(BuildDate)-SlashPos2)));
    Result := FormatDateTime('yyyy-mm-dd', Date);
  end;

var
  FBitmap: TBitmap;
begin
  Notebook1.PageIndex:=0;
  FBitmap := LoadBitmapFromLazarusResource('splash_logo');
  Image1.Picture.Graphic:=FBitmap;
  LogoImage.Picture.Graphic:=FBitmap;
  FBitmap.Free;
  Caption:=lisAboutLazarus;
  VersionLabel.Caption := lisVersion+' #: '+ GetLazarusVersionString;
  RevisionLabel.Caption := lisSVNRevision+LazarusRevisionStr;
  BuildDateLabel.Caption := lisDate+': '+GetLocalizedBuildDate;
  PlatformLabel.Caption:=GetDefaultTargetCPU+'-'+GetDefaultTargetOS
                         +'-'+LCLPlatformDisplayNames[GetDefaultLCLWidgetType];

  AboutPage.Caption:=lisMenuTemplateAbout;
  ContributorsPage.Caption:=lisContributors;
  AcknowledgementsPage.Caption:=lisAcknowledgements;
  LogoPage.Caption:=lisLogo;
  miVerToClipboard.Caption := lisVerToClipboard;
  
  Constraints.MinWidth:= 600;
  Constraints.MinHeight:= 300;

  AboutMemo.Lines.Text:=
    Format(lisAboutLazarusMsg,[DoubleLineEnding,DoubleLineEnding,DoubleLineEnding])
    +DoubleLineEnding
    +'Official: http://sourceforge.net/projects/lazarus/'+LineEnding
    +'Tutorials: http://lazarus-ccr.sourceforge.net'+LineEnding;
  LoadContributors;
  LoadAcknowledgements;
  CloseButton.Caption:=lisClose;
end;

procedure TAboutForm.miVerToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := 'v' + LazarusVersionStr + ' r' + LazarusRevisionStr +
      ' ' + PlatformLabel.Caption;
end;

procedure TAboutForm.LoadContributors;
var
  ContributorsFileName: string;
begin
  ContributorsFileName:=
    AppendPathDelim(EnvironmentOptions.LazarusDirectory)
    +'docs'+PathDelim+'Contributors.txt';
  //writeln('TAboutForm.LoadContributors ',FileExists(ContributorsFileName),' ',ContributorsFileName);
  if FileExists(ContributorsFileName) then
    ContributorsMemo.Lines.LoadFromFile(ContributorsFileName)
  else
    ContributorsMemo.Text:=lisAboutNoContributors;
end;

procedure TAboutForm.LoadAcknowledgements;
var
  AcknowledgementsFileName: string;
begin
  AcknowledgementsFileName:=
    AppendPathDelim(EnvironmentOptions.LazarusDirectory)
    +'docs'+PathDelim+'acknowledgements.txt';
  if FileExists(AcknowledgementsFileName) then
    AcknowledgementsMemo.Lines.LoadFromFile(AcknowledgementsFileName)
  else
    AcknowledgementsMemo.Text:=lisAboutNoContributors;
end;

initialization
  {$I aboutfrm.lrs}
  {$I lazarus_about_logo.lrs}

end.

