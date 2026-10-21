unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls, Spin, RichMemo, RichMemoUtils;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    ComboBox1: TComboBox;
    OpenDialog1: TOpenDialog;
    RichMemo1: TRichMemo;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RichMemo1Click(Sender: TObject);
    procedure RichMemo1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    LoadRTFFile( RichMemo1, OpenDialog1.FileName );
  end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
var
  t : string;
  prc : Integer;
begin
  t:=StringReplace(ComboBox1.Text, '%', '', [rfReplaceAll]);
  prc := StrToIntDef( t , 100);
  RichMemo1.ZoomFactor:=prc/100;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

end;

procedure TForm1.RichMemo1Click(Sender: TObject);
begin

end;

procedure TForm1.RichMemo1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

end.

