unit BitBtnForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, ExtCtrls,
  StdCtrls;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    Button1: TBitBtn;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    RadioButton7: TRadioButton;
    RadioButton8: TRadioButton;
    procedure FormCreate(Sender: TObject);
    procedure RadioButton1Change(Sender: TObject);
    procedure RadioButton2Change(Sender: TObject);
    procedure RadioButton3Change(Sender: TObject);
    procedure RadioButton4Change(Sender: TObject);
    procedure RadioButton5Change(Sender: TObject);
    procedure RadioButton6Change(Sender: TObject);
    procedure RadioButton7Change(Sender: TObject);
    procedure RadioButton8Change(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  RadioButton1Change(nil);
  RadioButton5Change(nil);
end;

procedure TForm1.RadioButton1Change(Sender: TObject);
begin
  Button1.Kind := bkClose;
  Label1.Caption := 'bkClose';
end;

procedure TForm1.RadioButton2Change(Sender: TObject);
begin
  Button1.Kind := bkOK;
  Label1.Caption := 'bkOK';
end;

procedure TForm1.RadioButton3Change(Sender: TObject);
begin
  Button1.Kind := bkCancel;
  Label1.Caption := 'bkCancel';
end;

procedure TForm1.RadioButton4Change(Sender: TObject);
begin
  Button1.Kind := bkHelp;
  Label1.Caption := 'bkHelp';
end;

procedure TForm1.RadioButton5Change(Sender: TObject);
begin
  Button1.Layout := blGlyphLeft;
  Label2.Caption := 'blGlyphLeft';
end;

procedure TForm1.RadioButton6Change(Sender: TObject);
begin
  Button1.Layout := blGlyphTop;
  Label2.Caption := 'blGlyphTop';
end;

procedure TForm1.RadioButton7Change(Sender: TObject);
begin
  Button1.Layout := blGlyphRight;
  Label2.Caption := 'blGlyphRight';
end;

procedure TForm1.RadioButton8Change(Sender: TObject);
begin
  Button1.Layout := blGlyphBottom;
  Label2.Caption := 'blGlyphBottom';
end;

end.

