unit lrEmailAppFreeSoft;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lrEmailExportFilter;
type
  { TEmailAppThunderbird }

  TEmailAppThunderbird = class(TEmailApp)
  protected
    class function AppFileName:string;override;
    function MakeParams:string;override;
  public
    class function AppName:string;override;
  end;

  { TEmailAppEvolution }

  TEmailAppEvolution = class(TEmailApp)
  protected
    class function AppFileName:string;override;
    function MakeParams:string;override;
  public
    class function AppName:string;override;
  end;

  { TEmailAppKMail }

  TEmailAppKMail = class(TEmailApp)
  protected
    class function AppFileName:string;override;
    function MakeParams:string;override;
  public
    class function AppName:string;override;
  end;

implementation
uses
  {$IFDEF WINDOWS}
  registry,
  {$ENDIF}
  UTF8Process;

{ TEmailAppKMail }

class function TEmailAppKMail.AppFileName: string;
begin
  {$IFDEF LINUX}
  Result:=FindFilenameOfCmd('kmail');
  {$ELSE}
  {$IFDEF WINDOWS}
  Result:='';
  {$ENDIF}
  {$ENDIF}
end;

function TEmailAppKMail.MakeParams: string;
begin
  Result:='--composer '+
     '--subject '+FFilter.MessageSubject+' '+
     '--attach file://'+FFilter.EmailAttachFileName+' ';

  if Trim(FFilter.MessageBody.Text)<>'' then
    Result:=Result + '--body "'+Trim(FFilter.MessageBody.Text)+'" ';
  Result:=Result + FFilter.Email;

{
  Usage: kmail [Qt-options] [KDE-options] [options] [address|URL]

  KDE Email Client

  Generic options:
    --help                    Show help about options
    --help-qt                 Show Qt specific options
    --help-kde                Show KDE specific options
    --help-all                Show all options
    --author                  Show author information
    -v, --version             Show version information
    --license                 Show license information
    --                        End of options

  Options:
    -s, --subject <subject>   Set subject of message
    -c, --cc <address>        Send CC: to 'address'
    -b, --bcc <address>       Send BCC: to 'address'
    -h, --replyTo <address>   Set replyTo to 'address'
    --header <header_name:header_value> Add 'header' to message. This can be repeated
    --msg <file>              Read message body from 'file'
    --body <text>             Set body of message
    --attach <url>            Add an attachment to the mail. This can be repeated
    --check                   Only check for new mail
    --composer                Only open composer window
    --view <url>              View the given message file

  Arguments:
    address|URL               Send message to 'address' or attach the file the 'URL' points to
}
end;


class function TEmailAppKMail.AppName: string;
begin
  Result:='KDE Mail App (KMail)';
end;

{ TEmailAppEvolution }

class function TEmailAppEvolution.AppFileName: string;
begin
  Result:=FindFilenameOfCmd('evolution');
end;

function TEmailAppEvolution.MakeParams: string;
begin
  Result:='"mailto:' + FFilter.Email +
     '?subject='+FFilter.MessageSubject;
  if Trim(FFilter.MessageBody.Text)<>'' then
    Result:=Result + '&body='+Trim(FFilter.MessageBody.Text);
  Result:=Result + '&attach=file://'+FFilter.EmailAttachFileName+'"';
end;

class function TEmailAppEvolution.AppName: string;
begin
  Result:='Gnome Evolution';
end;

{ TEmailAppThunderbird }

class function TEmailAppThunderbird.AppFileName: string;
{$IFDEF WINDOWS}
var
  R:TRegistry;
  S: String;
{$ENDIF}
begin
  {$IFDEF LINUX}
  Result:=FindFilenameOfCmd('thunderbird');
  {$ELSE}
  {$IFDEF WINDOWS}
  R:=TRegistry.Create;
  R.Access     := KEY_READ;
  R.RootKey    := HKEY_LOCAL_MACHINE;
  S:='';
  Result:='';
  try
    if R.OpenKeyReadOnly('SOFTWARE\Mozilla\Mozilla Thunderbird') then
      S:=R.ReadString('CurrentVersion');
    R.CloseKey;
    if S<>'' then
    begin
      S:='SOFTWARE\Mozilla\Mozilla Thunderbird\'+S+'\Main';
      if R.OpenKeyReadOnly(S) then
        Result:=R.ReadString('PathToExe');
      R.CloseKey;
    end;
  finally
    R.Free;
  end;
  {$ENDIF}
  {$ENDIF}
end;

function TEmailAppThunderbird.MakeParams: string;
begin
  Result:='-compose "to='+FFilter.Email+','+
     'subject='+FFilter.MessageSubject+','+
     'attachment='+FFilter.EmailAttachFileName+','+
     'body='''+Trim(FFilter.MessageBody.Text)+'''"';
  {
    Each message option follows the syntax field=value, for example:

        to=foo@nowhere.net
        subject=cool page
        attachment=www.mozilla.org
        attachment='file:///c:/test.txt'
        body=check this page

    Multiple message options are separated by comma (,), for example: "to=foo@nowhere.net,subject=cool page" . Comma separators must not follow or precede spaces ( ). To assign multiple values to a field, enclose the values in single quotes ('), for example: "to='foo@nowhere.net,foo@foo.de',subject=cool page" .
  }
end;

class function TEmailAppThunderbird.AppName: string;
begin
  Result:='Mozilla Thunderbird'
end;

initialization
  RegisterEmailApp(TEmailAppThunderbird);
  RegisterEmailApp(TEmailAppEvolution);
  RegisterEmailApp(TEmailAppKMail);
end.

