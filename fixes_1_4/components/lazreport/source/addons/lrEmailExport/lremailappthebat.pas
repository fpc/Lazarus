unit lrEmailAppTheBat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lrEmailExportFilter;

type

  { TEmailAppTheBat }

  TEmailAppTheBat = class(TEmailApp)
  protected
    class function AppFileName:string;override;
    function MakeParams:string;override;
  public
    class function AppName:string;override;
  end;

implementation
uses registry, LCLProc, LazUTF8;

{ TEmailAppTheBat }

class function TEmailAppTheBat.AppFileName: string;
{$IFDEF WINDOWS}
var
  R:TRegistry;
{$ENDIF}
begin
  {$IFDEF WINDOWS}
  R:=TRegistry.Create;
  R.Access     := KEY_READ;
  R.RootKey    := HKEY_CURRENT_USER;
  Result:='';
  try
    if R.OpenKeyReadOnly('Software\RIT\The Bat!') then
      Result:=R.ReadString('EXE path');
    R.CloseKey;
  finally
    R.Free;
  end;
  {$ELSE}
  ..
  {$ENDIF}
end;

function TEmailAppTheBat.MakeParams: string;
var
  FBodyTxt, S:string;
  F:TFileStream;
begin
{
  Result:='"mailto:' + FFilter.Email +
     '?subject='+FFilter.MessageSubject;
  if Trim(FFilter.MessageBody.Text)<>'' then
    Result:=Result + '&body='+Trim(FFilter.MessageBody.Text);
  Result:=Result + '&attach='+FFilter.EmailAttachFileName+'"';
}

{
Автоматизированное создание сообщения - команда /MAIL

Команда /MAIL используется для автоматизированного создания сообщения из шаблона, текстового файла и/или набора присоединенных файлов на определенный адрес. Эта команда чрезвычайно полезна для приложений, которые требуют посылки сообщений электронной почты без вмешательства пользователя, в соответствии со стандартами сети Интернет.

Синтаксис команды /MAIL:

/MAIL:[parameter1[;parameter2[;parameter3 [...]]]

Возможные параметры:

USER=значение или U=значение.
  Значение - название ящика, из которого должно быть отправлено сообщение.
  Если параметр FOLDER не определен, будет отправлено сообщение из папки Inbox данного ящика.
PASSWORD=значение или P=значение.
  Значение - пароль. Используется в том случае, если ящик защищен паролем.
FOLDER=значение или F=значение.
  Значение - путь к папке в ящике, из которой следует оправить сообщение. Если путь не включает имя ящика,
  The Bat! будет просматривать все ящики в поисках папки с таким именем;
  использоваться будет первая найденная папка.
  Если заданная папка не найдена, будет использоваться папка Inbox указанного ящика.
TEMPLATE=значение или T=значение.
  Значение - путь к файлу, содержащему шаблон, который должен использоваться для создания сообщения.
  По умолчанию это - шаблон используемой папки или ящика.

TO=значение.
  Значение определяет первичного адресата сообщения. Вы можете добавлять дополнительных адресатов с помощью макрокоманд шаблона %TO, %CC, %BCC.
SUBJECT=значение или S=значение.
  Значение определяет тему сообщения. Также возможно определить тему сообщения в шаблоне, используя макрос %SUBJECT.
TEXT=значение или CONTENTS=значение или C=значение.
  Значение - путь к текстовому файлу, который содержит текст сообщения.
  Также возможно включить текстовый файл в сообщение, используя в шаблоне макрос %PUT.
ATTACH=значение или FILE=значение или A=значение.
  Значение - путь к файлу, который должен быть присоединен к сообщению. Также возможно использовать макрос %ATTACHFILE в шаблоне.

ПРИМЕЧАНИЯ:
  Для разделения параметров используйте точку с запятой (символ ";"). Не используйте пробелы между параметрами,
  когда используете команду /MAIL в командной строке, потому что отделенный пробелом параметр будет обрабатываться
  как следующая команда и не будет понят программой должным образом.

  Если значение содержит пробелы, поместите его в кавычки. Если значение содержит кавычки,
  Вы должны использовать апострофы (символ " ' ").
}

  if Trim(FFilter.MessageBody.Text)<>'' then
  begin
    FBodyTxt:=GetTempDir(false)+'tmp_EmailAppTheBat_body.txt';
    F:=TFileStream.Create(FBodyTxt, fmCreate);
    try
      S:=UTF8ToSys(FFilter.MessageBody.Text);
      F.Write(S[1], Length(S));
    finally
      F.Free;
    end;
  end
  else
    FBodyTxt:='';

  Result:= '/MAIL:'+ //[parameter1[;parameter2[;parameter3 [...]]]
          'TO='+FFilter.Email+';'+
          'SUBJECT='''+FFilter.MessageSubject+''';';
  if FBodyTxt<>'' then
    Result:=Result+'CONTENTS='''+SysToUTF8(FBodyTxt)+''';';
  Result:=Result+'ATTACH='+FFilter.EmailAttachFileName;

end;

class function TEmailAppTheBat.AppName: string;
begin
  Result:='The Bat!';
end;

initialization
  RegisterEmailApp(TEmailAppTheBat);
end.

