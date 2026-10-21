unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RichMemo, RichMemoUtils, LazUTF8;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    chkCaseSensitive: TCheckBox;
    chkWholeWord: TCheckBox;
    chkBackward: TCheckBox;
    Edit1: TEdit;
    OpenDialog1: TOpenDialog;
    RichMemo1: TRichMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  if OpenDialog1.Execute then
    LoadRTFFile( RichMemo1, OpenDialog1.FileName );
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  s : Integer;
  l : Integer;
  st: Integer;
  opt: TSearchOptions;
begin
  st:=RichMemo1.SelStart;
  l:=RichMemo1.SelLength;

  opt:=[];
  if chkCaseSensitive.Checked then include(opt, soMatchCase);
  if chkWholeWord.Checked then include(opt, soWholeWord);
  if chkBackward.Checked then include(opt, soBackward);

  Caption:='Searching...';
  s:=RichMemo1.Search(Edit1.Text, RichMemo1.SelStart, RichMemo1.GetTextLen, opt);
  if (s>=0) then begin
    if (st=s) and (l=UTF8Length(Edit1.Text)) then
      s:=RichMemo1.Search(Edit1.Text, RichMemo1.SelStart+1, RichMemo1.GetTextLen, opt);
  end;

  if (s>=0) then begin
    Caption:='Found!';
    RichMemo1.SelStart:=s;
    RichMemo1.SetSelLengthFor(Edit1.text);
  end else begin
    Caption:='Not found!';
    RichMemo1.SelStart:=MaxInt;
    RichMemo1.SelLength:=0;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  RichMemo1.SelStart:=0;
  RichMemo1.SelLength:=0;
  Caption:='Ready to Search!';
end;

end.

