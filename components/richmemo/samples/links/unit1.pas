unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  RichMemo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    RichMemo1: TRichMemo;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RichMemo1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
  public
    { public declarations }
    procedure OnLinkAction(Sender: TObject; AAction: TLinkAction;
      const AMouseInfo: TLinkMouseInfo; StartChar, LenChars: Integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  RichMemo1.SetLink(RichMemo1.SelStart, RichMemo1.SelLength, true);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  RichMemo1.SetLink(RichMemo1.SelStart, RichMemo1.SelLength, false);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  RichMemo1.OnLinkAction:=@OnLinkAction;
end;

procedure TForm1.RichMemo1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
end;

procedure TForm1.OnLinkAction(Sender: TObject; AAction: TLinkAction;
  const AMouseInfo: TLinkMouseInfo; StartChar, LenChars: Integer);
begin
  if AMouseInfo.button = mbLeft then begin
    ShowMessage( 'Link Text: "'+RichMemo1.GetText(StartChar, LenChars)+'"' );
  end;
end;

end.

