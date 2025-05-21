unit chmAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons;

type

  { TAboutForm }

  TAboutForm = class(TForm)
    Bevel1: TBevel;
    CloseBtn: TBitBtn;
    AppImage: TImage;
    InfoOperatingSystem: TLabel;
    LblLazarus: TLabel;
    LblCreatedWith: TLabel;
    LblFreePascal: TLabel;
    LblAnd: TLabel;
    LblTargetCPU: TLabel;
    InfoTargetCPU: TLabel;
    InfoTargetOS: TLabel;
    LblTargetPlatform: TLabel;
    LblTargetOS: TLabel;
    InfoTargetPlatform: TLabel;
    LblVersion: TLabel;
    LblOperatingSystem: TLabel;
    lblTitle: TLabel;
    InfoVersion: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure URLClick(Sender: TObject);
    procedure URLMouseEnter(Sender: TObject);
    procedure URLMouseLeave(Sender: TObject);
  private
    procedure UpdateLanguage;

  public

  end;

var
  AboutForm: TAboutForm;

implementation

{$R *.lfm}

uses
  LCLIntf, LCLPlatformDef, Types, InterfaceBase,
  {$IFDEF MSWINDOWS}
  win32Proc,
  {$ENDIF}
  FileInfo,
  CHMStrConsts;

// https://forum.lazarus.freepascal.org/index.php/topic,15390.msg82563.html#msg82563
function GetOSVersion: String;
begin
  Result := 'Unknown';

 {$IFDEF MSWINDOWS}
  case WindowsVersion of
    wv95: Result := '95';
    wvNT4: Result := 'NT v.4';
    wv98: Result := '98';
    wvMe: Result := 'ME';
    wv2000: Result := '2000';
    wvXP: Result := 'XP';
    wvServer2003: Result := 'Server 2003';
    wvVista: Result := 'Vista';
    wv7: Result := '7';
    wv8: Result := '8';
    wv8_1: Result := '8.1';
    wv10: Result := '10';
    wv11: Result := '11';
    else Result:= '';
  end;
  Result := 'Windows ' + Result;
 {$ENDIF}

 {$IFDEF UNIX}
  Result := 'Unix ';
 {$ENDIF}

  {$IFDEF LCLcarbon}
  Result := 'Mac OS X';
 {$ENDIF}

 {$IFDEF LCLcocoa}
  Result := 'macOS';
 {$ENDIF}

 {$IFDEF Linux}
  Result := 'Linux';
 {$ENDIF}
end;

function GetVersionStr: String;
var
  ver: TProgramVersion;
begin
  ver := Default(TProgramVersion);
  GetProgramVersion(ver);
  Result := Format('v%d.%d.%d', [ver.Major, ver.Minor, ver.Revision]);
end;

{ TAboutForm }

procedure TAboutForm.FormCreate(Sender: TObject);
begin
  UpdateLanguage;

  with AppImage do
  begin
    Picture.Assign(Application.Icon);
    Picture.Icon.Current := Picture.Icon.GetBestIndexForSize(Size(Width, Height));
  end;

  InfoVersion.Caption := GetVersionStr;
  InfoOperatingSystem.Caption := GetOSVersion;
  InfoTargetCPU.Caption := LowerCase({$I %FPCTARGETCPU%});
  InfoTargetOS.Caption := LowerCase({$I %FPCTARGETOS%});
  InfoTargetPlatform.Caption := LCLPlatformDisplayNames[GetDefaultLCLWidgetType];
end;

procedure TAboutForm.URLClick(Sender: TObject);
begin
  OpenURL(TControl(Sender).Hint);
end;

procedure TAboutForm.URLMouseEnter(Sender: TObject);
begin
  TControl(Sender).Font.Style := [fsUnderline];
end;

procedure TAboutForm.URLMouseLeave(Sender: TObject);
begin
  TControl(Sender).Font.Style := [];
end;

procedure TAboutForm.UpdateLanguage;
begin
  Caption := rsAboutCaption;
  CloseBtn.Caption := rsClose;
  LblVersion.Caption := rsVersion;
  LblCreatedWith.Caption := rsCreatedWith;
  LblAnd.Caption := rsAnd;
  LblOperatingSystem.Caption := rsOperatingSystem;
  LblTargetCPU.Caption := rsTargetCPU;
  LblTargetOS.Caption := rsTargetOS;
  LblTargetPlatform.Caption := rsTargetPlatform;
end;

end.

