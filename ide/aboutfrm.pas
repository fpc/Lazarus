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
  LCLProc, StdCtrls, Buttons, LazConf, LazarusIDEStrConsts, ExtCtrls, ComCtrls,
  EnvironmentOpts, Clipbrd, FileUtil, Menus, LCLIntf;

type

  { TScrollingText }

  TScrollingText = class(TGraphicControl)
  private
    FActive: boolean;
    FActiveLine: integer;   //the line over which the mouse hovers
    FBuffer: TBitmap;
    FEndLine: integer;
    FLineHeight: integer;
    FLines: TStrings;
    FNumLines: integer;
    FOffset: integer;
    FStartLine: integer;
    FStepSize: integer;
    FTimer: TTimer;
    function ActiveLineIsURL: boolean;
    procedure DoTimer(Sender: TObject);
    procedure SetActive(const AValue: boolean);
    procedure Init;
    procedure DrawScrollingText(Sender: TObject);
  protected
    procedure DoOnChangeBounds; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Active: boolean read FActive write SetActive;
    property Lines: TStrings read FLines write FLines;
  end;

  { TAboutForm }

  TAboutForm = class(TForm)
    CloseButton: TBitBtn;
    BuildDateLabel: TLABEL;
    AboutMemo: TMEMO;
    DocumentationLabel: TLabel;
    DocumentationURLLabel: TLabel;
    FPCVersionLabel: TLabel;
    LogoImage: TImage;
    miVerToClipboard: TMenuItem;
    OfficialLabel: TLabel;
    OfficialURLLabel: TLabel;
    VersionPage: TTabSheet;
    ButtonPanel: TPanel;
    PlatformLabel: TLabel;
    PopupMenu1: TPopupMenu;
    VersionLabel: TLABEL;
    RevisionLabel: TLabel;
    Notebook: TPageControl;
    AboutPage: TTabSheet;
    ContributorsPage: TTabSheet;
    AcknowledgementsPage:TTabSheet;
    procedure AboutFormCreate(Sender:TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure miVerToClipboardClick(Sender: TObject);
    procedure NotebookPageChanged(Sender: TObject);
    procedure URLLabelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure URLLabelMouseEnter(Sender: TObject);
    procedure URLLabelMouseLeave(Sender: TObject);
  private
    Acknowledgements: TScrollingText;
    Contributors: TScrollingText;
    procedure LoadContributors;
    procedure LoadAcknowledgements;
  public
  end;

function ShowAboutForm: TModalResult;

const
  LazarusVersionStr= {$I version.inc};
var
  LazarusRevisionStr: string;
  
function GetLazarusVersionString : string;

implementation

{$R *.lfm}

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
  Result:=LazarusVersionStr;
end;

{ TAboutForm }

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
  //Image1.Picture.LoadFromLazarusResource('splash_logo');
  LogoImage.Picture.LoadFromLazarusResource('splash_logo');
  Caption:=lisAboutLazarus;
  VersionLabel.Caption := lisVersion+' #: '+ GetLazarusVersionString;
  RevisionLabel.Caption := lisSVNRevision+LazarusRevisionStr;
  BuildDateLabel.Caption := lisDate+': '+GetLocalizedBuildDate;
  FPCVersionLabel.Caption:= lisFPCVersion+{$I %FPCVERSION%};
  PlatformLabel.Caption:=GetDefaultTargetCPU+'-'+GetDefaultTargetOS
                         +'-'+LCLPlatformDisplayNames[GetDefaultLCLWidgetType];

  VersionPage.Caption:=lisVersion;
  AboutPage.Caption:=lisMenuTemplateAbout;
  ContributorsPage.Caption:=lisContributors;
  ContributorsPage.DoubleBuffered := True;
  AcknowledgementsPage.Caption:=lisAcknowledgements;
  AcknowledgementsPage.DoubleBuffered := True;
  miVerToClipboard.Caption := lisVerToClipboard;

  VersionLabel.Font.Color:= clWhite;

  Constraints.MinWidth:= 460;
  Constraints.MinHeight:= 380;
  Width:= 460;
  Height:= 380;

  AboutMemo.Lines.Text:=
    Format(lisAboutLazarusMsg,[DoubleLineEnding,DoubleLineEnding,DoubleLineEnding]);

  OfficialLabel.Caption := lisAboutOfficial;
  OfficialURLLabel.Caption := 'http://lazarus.freepascal.org';
  DocumentationLabel.Caption := lisAboutDocumentation;
  DocumentationURLLabel.Caption := 'http://wiki.lazarus.freepascal.org';

  LoadContributors;
  LoadAcknowledgements;
  CloseButton.Caption:=lisClose;
end;

procedure TAboutForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  Acknowledgements.Active := False;
  Contributors.Active     := False;
end;

procedure TAboutForm.miVerToClipboardClick(Sender: TObject);
begin
  Clipboard.AsText := 'Lazarus ' + LazarusVersionStr + ' r' + LazarusRevisionStr +
    ' FPC ' + {$I %FPCVERSION%} + ' ' + PlatformLabel.Caption;
end;

procedure TAboutForm.NotebookPageChanged(Sender: TObject);
begin
  if Assigned(Contributors) then
    Contributors.Active:=NoteBook.ActivePage = ContributorsPage;
  if Assigned(Acknowledgements) then
    Acknowledgements.Active:=NoteBook.ActivePage = AcknowledgementsPage;
end;

procedure TAboutForm.URLLabelMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TAboutForm.URLLabelMouseLeave(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [];
  TLabel(Sender).Font.Color := clBlue;
  TLabel(Sender).Cursor := crDefault;
end;

procedure TAboutForm. URLLabelMouseEnter(Sender: TObject);
begin
  TLabel(Sender).Font.Style := [fsUnderLine];
  TLabel(Sender).Font.Color := clRed;
  TLabel(Sender).Cursor := crHandPoint;
end;

procedure TAboutForm.LoadContributors;
var
  ContributorsFileName: string;
begin
  ContributorsPage.ControlStyle := ContributorsPage.ControlStyle - [csOpaque];
  Contributors := TScrollingText.Create(ContributorsPage);
  Contributors.Name:='Contributors';
  Contributors.Parent := ContributorsPage;
  Contributors.Align:=alClient;

  ContributorsFileName:=
    AppendPathDelim(EnvironmentOptions.LazarusDirectory)
    +'docs'+PathDelim+'Contributors.txt';
  //writeln('TAboutForm.LoadContributors ',FileExistsUTF8(ContributorsFileName),' ',ContributorsFileName);

  if FileExistsUTF8(ContributorsFileName) then
    Contributors.Lines.LoadFromFile(UTF8ToSys(ContributorsFileName))
  else
    Contributors.Lines.Text:=lisAboutNoContributors;
end;

procedure TAboutForm.LoadAcknowledgements;
var
  AcknowledgementsFileName: string;
begin
  Acknowledgements := TScrollingText.Create(AcknowledgementsPage);
  Acknowledgements.Name:='Acknowledgements';
  Acknowledgements.Parent := AcknowledgementsPage;
  Acknowledgements.Align:=alClient;

  AcknowledgementsFileName:=
    AppendPathDelim(EnvironmentOptions.LazarusDirectory)
    +'docs'+PathDelim+'acknowledgements.txt';

  if FileExistsUTF8(AcknowledgementsFileName) then
    Acknowledgements.Lines.LoadFromFile(UTF8ToSys(AcknowledgementsFileName))
  else
    Acknowledgements.Lines.Text:=lisAboutNoContributors;
end;

{ TScrollingText }

procedure TScrollingText.SetActive(const AValue: boolean);
begin
  FActive := AValue;
  if FActive then
    Init;
  FTimer.Enabled:=Active;
end;

procedure TScrollingText.Init;
begin
  FBuffer.Width := Width;
  FBuffer.Height := Height;
  FLineHeight := FBuffer.Canvas.TextHeight('X');
  FNumLines := FBuffer.Height div FLineHeight;

  if FOffset = -1 then
    FOffset := FBuffer.Height;

  with FBuffer.Canvas do
  begin
    Brush.Color := clWhite;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
  end;
end;

procedure TScrollingText.DrawScrollingText(Sender: TObject);
begin
  if Active then
    Canvas.Draw(0,0,FBuffer);
end;

procedure TScrollingText.DoTimer(Sender: TObject);
var
  w: integer;
  s: string;
  i: integer;
begin
  if not Active then
    Exit;

  Dec(FOffset, FStepSize);

  if FOffSet < 0 then
    FStartLine := -FOffset div FLineHeight
  else
    FStartLine := 0;

  FEndLine := FStartLine + FNumLines + 1;
  if FEndLine > FLines.Count - 1 then
    FEndLine := FLines.Count - 1;

  FBuffer.Canvas.FillRect(Rect(0, 0, FBuffer.Width, FBuffer.Height));

  for i := FEndLine downto FStartLine do
  begin
    s := Trim(FLines[i]);

    //reset buffer font
    FBuffer.Canvas.Font.Style := [];
    FBuffer.Canvas.Font.Color := clBlack;

    //skip empty lines
    if Length(s) > 0 then
    begin
      //check for bold format token
      if s[1] = '#' then
      begin
        s := copy(s, 2, Length(s) - 1);
        FBuffer.Canvas.Font.Style := [fsBold];
      end
      else
      begin
        //check for url
        if Pos('http://', s) = 1 then
        begin
          if i = FActiveLine then
          begin
            FBuffer.Canvas.Font.Style := [fsUnderline];
            FBuffer.Canvas.Font.Color := clRed;
          end
          else
            FBuffer.Canvas.Font.Color := clBlue;
         end;
      end;

      w := FBuffer.Canvas.TextWidth(s);
      FBuffer.Canvas.TextOut((FBuffer.Width - w) div 2, FOffset + i * FLineHeight, s);
    end;
  end;

  //start showing the list from the start
  if FStartLine > FLines.Count - 1 then
    FOffset := FBuffer.Height;
  Invalidate;
end;

function TScrollingText.ActiveLineIsURL: boolean;
begin
  if (FActiveLine > 0) and (FActiveLine < FLines.Count) then
    Result := Pos('http://', FLines[FActiveLine]) = 1
  else
    Result := False;
end;

procedure TScrollingText.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;

  Init;
end;

procedure TScrollingText.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if ActiveLineIsURL then
    OpenURL(FLines[FActiveLine]);
end;

procedure TScrollingText.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  //calculate what line is clicked from the mouse position
  FActiveLine := (Y - FOffset) div FLineHeight;

  Cursor := crDefault;

  if (FActiveLine >= 0) and (FActiveLine < FLines.Count) and ActiveLineIsURL then
    Cursor := crHandPoint;
end;

constructor TScrollingText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];

  OnPaint := @DrawScrollingText;
  FLines := TStringList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer:=@DoTimer;
  FTimer.Interval:=30;
  FBuffer := TBitmap.Create;

  FStepSize := 1;
  FStartLine := 0;
  FOffset := -1;
end;

destructor TScrollingText.Destroy;
begin
  FLines.Free;
  FTimer.Free;
  FBuffer.Free;
  inherited Destroy;
end;

initialization
  {$I lazarus_about_logo.lrs}

end.

