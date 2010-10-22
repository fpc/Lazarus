unit MainUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, LResources, Forms, Controls, Graphics, Dialogs,
  FileUtil, UTF8Process, StdCtrls, Process, ExtCtrls;

type
  { TForm1 }

  TForm1 = class(TForm)
    OpenLazarusButton: TButton;
    BrowserRadioGroup: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure OpenLazarusButtonClick(Sender: TObject);
  private
  public
    procedure OpenURL(const URL: string);
    procedure GetBrowser(out BrowserName, BrowserFilename, StartScriptFilename: string);
  end;

var
  Form1: TForm1; 

implementation

function SearchExecutable(const ShortFilename: string; var Filename: string
  ): boolean;
begin
  Filename:=SearchFileInPath(ShortFilename,'',
                      GetEnvironmentVariableUTF8('PATH'),PathSeparator,[]);
  Result:=Filename<>'';
end;

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
end;

procedure TForm1.OpenLazarusButtonClick(Sender: TObject);
begin
  OpenURL('http://www.lazarus.freepascal.org/');
end;

procedure TForm1.OpenURL(const URL: string);
var
  TheProcess: TProcessUTF8;
  BrowserFilename: string;
  StartScriptFilename: string;
  BrowserName: string;
begin
  GetBrowser(BrowserName,BrowserFilename,StartScriptFilename);
  if BrowserFilename='' then begin
    DebugLn('TForm1.OpenURL unable to find browser "',BrowserName,'"');
    MessageDlg('Invalid browser',
           'Unable to find browser executable "'+BrowserName+'"',
           mtError,[mbCancel],0);
    exit;
  end;
  
  DebugLn('TForm1.OpenURL StartScriptFilename=',StartScriptFilename);
  if not FileExistsUTF8(StartScriptFilename) then begin
    DebugLn('TForm1.OpenURL unable to find program "',StartScriptFilename,'"');
    MessageDlg('Invalid browser',
           'Unable to find browser "'+StartScriptFilename+'"',
           mtError,[mbCancel],0);
    exit;
  end;
  if not FileIsExecutable(StartScriptFilename) then begin
    DebugLn('TForm1.OpenURL browserfile is not executable "',StartScriptFilename,'"');
    MessageDlg('Invalid browser',
           'Browserfilename "'+StartScriptFilename+'" is not executable',
           mtError,[mbCancel],0);
    exit;
  end;

  
  TheProcess:=TProcessUTF8.Create(nil);
  try
    TheProcess.Options:= [poUsePipes, poNoConsole, poStdErrToOutput];
    TheProcess.ShowWindow := swoNone;
    TheProcess.CommandLine:=StartScriptFilename+' '+URL;
    try
      TheProcess.Execute;
      TheProcess.WaitOnExit;
      if TheProcess.ExitStatus<>0 then begin
        MessageDlg('Error',
          'Error executing browser script '+StartScriptFilename+#13
          +'Error code: '+IntToStr(TheProcess.ExitStatus),
          mtError,[mbCancel],0);
      end;
    finally
      TheProcess.Free;
    end;
  except
    on E: Exception do begin
      DebugLn('TForm1.OpenURL ERROR: ',E.Message);
    end;
  end;
end;

procedure TForm1.GetBrowser(out BrowserName, BrowserFilename,
  StartScriptFilename: string);
var
  i: LongInt;
begin
  i:=BrowserRadioGroup.ItemIndex;
  if i<0 then i:=4;
  BrowserName:=BrowserRadioGroup.Items[i];
  StartScriptFilename:=ExtractFilePath(Application.ExeName)+'OpenURLIn'+BrowserName+'.sh';
  if not SearchExecutable(lowercase(BrowserName),BrowserFilename) then
    BrowserFilename:='';
end;


initialization
  {$I mainunit.lrs}

end.

