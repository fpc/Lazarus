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
  Clipbrd, FileUtil, Menus, HelpIntfs;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    CloseButton: TBitBtn;
    BuildDateLabel: TLABEL;
    AboutMemo: TMEMO;
    DocumentationLabel: TLabel;
    DocumentationURLLabel: TLabel;
    Image1: TImage;
    FPCVersionLabel: TLabel;
    LogoImage: TImage;
    LogoPage: TPage;
    miVerToClipboard: TMenuItem;
    AcknowledgementsPaintBox: TPaintBox;
    ContributorsPaintBox: TPaintBox;
    OfficialLabel: TLabel;
    OfficialURLLabel: TLabel;
    PlatformLabel: TLabel;
    PopupMenu1: TPopupMenu;
    Timer: TTimer;
    VersionLabel: TLABEL;
    RevisionLabel: TLabel;
    Notebook:TNotebook;
    AboutPage:TPage;
    ContributorsPage:TPage;
    AcknowledgementsPage:TPage;
    procedure AboutFormCreate(Sender:TObject);
    procedure miVerToClipboardClick(Sender: TObject);
    procedure URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure URLLabelMouseEnter(Sender: TObject);
    procedure URLLabelMouseLeave(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FAcknowledgements: TStrings;
    FBuffer: TBitmap;
    FContributors: TStrings;
    FEnd: integer;
    FLineHeight: integer;
    FNumLines: integer;
    FOffset: integer;
    FStart: integer;
    FStepSize: integer;
    procedure ResetScrollText;
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

begin
  Notebook.PageIndex:=0;
  Image1.Picture.LoadFromLazarusResource('splash_logo');
  LogoImage.Picture := Image1.Picture;
  Caption:=lisAboutLazarus;
  VersionLabel.Caption := lisVersion+' #: '+ GetLazarusVersionString;
  RevisionLabel.Caption := lisSVNRevision+LazarusRevisionStr;
  BuildDateLabel.Caption := lisDate+': '+GetLocalizedBuildDate;
  FPCVersionLabel.Caption:= lisFPCVersion+' '+{$I %FPCVERSION%};
  PlatformLabel.Caption:=GetDefaultTargetCPU+'-'+GetDefaultTargetOS
                         +'-'+LCLPlatformDisplayNames[GetDefaultLCLWidgetType];

  AboutPage.Caption:=lisMenuTemplateAbout;
  ContributorsPage.Caption:=lisContributors;
  AcknowledgementsPage.Caption:=lisAcknowledgements;
  LogoPage.Caption:=lisLogo;
  miVerToClipboard.Caption := lisVerToClipboard;
  
  FBuffer := TBitmap.Create;
  FBuffer.Width := ContributorsPaintBox.Width;
  FBuffer.Height := ContributorsPaintBox.Height;
  FLineHeight := FBuffer.Canvas.TextHeight('X');
  FNumLines := FBuffer.Height div FLineHeight;

  FOffset := FBuffer.Height;
  FStart := 0;
  FStepSize := 1;

  Constraints.MinWidth:= 600;
  Constraints.MinHeight:= 300;

  AboutMemo.Lines.Text:=
    Format(lisAboutLazarusMsg,[DoubleLineEnding,DoubleLineEnding,DoubleLineEnding]);

  OfficialLabel.Caption := 'Official:';
  OfficialURLLabel.Caption := 'http://lazarus.freepascal.org';
  DocumentationLabel.Caption := 'Documentation:';
  DocumentationURLLabel.Caption := 'http://wiki.lazarus.freepascal.org/index.php/Main_Page';

  LoadContributors;
  LoadAcknowledgements;
  CloseButton.Caption:=lisClose;
end;

procedure TAboutForm.miVerToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := 'v' + LazarusVersionStr + ' r' + LazarusRevisionStr +
      ' ' + PlatformLabel.Caption;
end;

procedure TAboutForm.URLLabelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  err: string;
begin
  if HelpIntfs.ShowHelp(TLabel(Sender).Caption, 'Lazarus', 'text/html', err) <> shrSuccess then
    ShowMessage(err);
end;

procedure TAboutForm.URLLabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Cursor := crDefault;
end;

procedure TAboutForm. URLLabelMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderLine];
  TLabel(Sender).Cursor := crHandPoint;
end;

procedure TAboutForm.TimerTimer(Sender: TObject);

  procedure DrawScrollText(ACanvas: TCanvas; AText: TStrings);
  var
    w: integer;
    s: string;
    i: integer;
  begin
    Dec(FOffset, FStepSize);

    if FOffSet < 0 then
      FStart := -FOffset div FLineHeight
    else
      FStart := 0;

    FEnd := FStart + FNumLines + 1;
    if FEnd > AText.Count - 1 then
      FEnd := AText.Count - 1;

    FBuffer.Canvas.FillRect(Rect(0, 0, FBuffer.Width, FBuffer.Height));

    for i := FEnd downto FStart do
    begin
      s := Trim(AText[i]);

      //reset buffer font
      FBuffer.Canvas.Font.Style := [];
      FBuffer.Canvas.Font.Color := clBlack;

      //skip empty lines
      if Length(s) > 0 then
      begin
        //check for bold makeup token
        if s[1] = '#' then
        begin
          s := copy(s, 2, Length(s) - 1);
          FBuffer.Canvas.Font.Style := [fsBold];
        end
        else
        begin
          //check for url
          if Pos('http://', s) > 0 then
            FBuffer.Canvas.Font.Color := clBlue;
        end;

        w := FBuffer.Canvas.TextWidth(s);
        FBuffer.Canvas.TextOut((FBuffer.Width - w) div 2, FOffset + i * FLineHeight, s);
      end;
    end;

    //start showing the list from the start
    if FStart > AText.Count - 1 then
      FOffset := FBuffer.Height;

    ACanvas.Draw(0,0,FBuffer);
  end;

begin
  if NoteBook.ActivePage = lisContributors then
  begin
    DrawScrollText(ContributorsPaintBox.Canvas, FContributors);
    exit;
  end;
  if NoteBook.ActivePage = lisAcknowledgements then
  begin
    DrawScrollText(AcknowledgementsPaintBox.Canvas, FAcknowledgements);
    exit;
  end;

  ResetScrollText;
end;

procedure TAboutForm.ResetScrollText;
begin
  if Assigned(FBuffer) then
  begin
    FBuffer.Width := ContributorsPaintBox.Width;
    FBuffer.Height := ContributorsPaintBox.Height;
    FNumLines := FBuffer.Height div FLineHeight;
    FOffset := FBuffer.Height;
  end;
end;

procedure TAboutForm.LoadContributors;
var
  ContributorsFileName: string;
begin
  ContributorsFileName:=
    AppendPathDelim(EnvironmentOptions.LazarusDirectory)
    +'docs'+PathDelim+'Contributors.txt';
  //writeln('TAboutForm.LoadContributors ',FileExistsUTF8(ContributorsFileName),' ',ContributorsFileName);
  FContributors := TStringList.Create;
  if FileExistsUTF8(ContributorsFileName) then
    FContributors.LoadFromFile(UTF8ToSys(ContributorsFileName))
  else
    FContributors.Text:=lisAboutNoContributors;
end;

procedure TAboutForm.LoadAcknowledgements;
var
  AcknowledgementsFileName: string;
begin
  AcknowledgementsFileName:=
    AppendPathDelim(EnvironmentOptions.LazarusDirectory)
    +'docs'+PathDelim+'acknowledgements.txt';
  FAcknowledgements := TStringList.Create;
  if FileExistsUTF8(AcknowledgementsFileName) then
    FAcknowledgements.LoadFromFile(UTF8ToSys(AcknowledgementsFileName))
  else
    FAcknowledgements.Text:=lisAboutNoContributors;
end;

initialization
  {$I aboutfrm.lrs}
  {$I lazarus_about_logo.lrs}

end.

