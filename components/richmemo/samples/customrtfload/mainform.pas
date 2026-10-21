unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, Spin, ExtCtrls, RichMemo, RichMemoUtils, RichMemoRTF;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    btnRTFtoMemo: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    RichMemo1: TRichMemo;
    SaveDialog1: TSaveDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure btnRTFtoMemoClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    Memo1.Lines.LoadFromFile( OpenDialog1.FileName  );
    btnRTFtoMemoClick(nil);
  end;
end;

procedure TForm1.btnRTFtoMemoClick(Sender: TObject);
begin
  RichMemo1.Rtf:=Memo1.Text;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo2.Text:=RichMemo1.Rtf;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Memo2.Lines.SaveToFile(SaveDialog1.FileName);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RegisterRTFLoader;
  RegisterRTFSaver;
end;

end.

