unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, RichMemo, richmemoml;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Panel1: TPanel;
    RichMemo1: TRichMemo;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    procedure TagFormat(Sender: TObject;  var atagName: string; tagattr: TStrings;
      var afont: TFontParams; var txt: string; var tagCloses: Boolean );
    procedure EntReplace(Sender: TObject; var txt: string; tagsStack: TStrings);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  RichMemo1.Clear;
  Parse(Memo1.Text, RichMemo1, @TagFormat, @EntReplace);
end;

procedure TForm1.TagFormat(Sender: TObject; var atagName: string; tagattr: TStrings;
  var afont: TFontParams; var txt: string; var tagCloses: Boolean);
begin
  if atagName='b' then
    Include(afont.Style, fsBold)
  else if atagName='i' then
    Include(afont.Style, fsItalic)
  else if atagName='s' then
    Include(afont.Style, fsStrikeOut)
  else if atagName='u' then
    Include(afont.Style, fsUnderline)
  else if atagName='h1' then begin
    Include(afont.Style, fsBold);
    afont.Size:=afont.Size*2;
  end else if atagName='h2' then begin
    Include(afont.Style, fsBold);
    afont.Size:=round(afont.Size*1.5)
  end else if atagName='pre' then begin
    afont.Name:='Courier New'
  end;
end;

procedure TForm1.EntReplace(Sender: TObject; var txt: string;
  tagsStack: TStrings);
begin
  txt:=StringReplace(txt, '&lt;', '<', [rfReplaceAll, rfIgnoreCase]);
  txt:=StringReplace(txt, '&gt;', '>', [rfReplaceAll, rfIgnoreCase]);
  txt:=StringReplace(txt, '&quot;', '"', [rfReplaceAll, rfIgnoreCase]);
end;

end.

