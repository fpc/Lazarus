unit secondform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, LCLType, LCLProc;

type

  { TForm2 }

  TForm2 = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Edit1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form2: TForm2; 

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.Button1Click(Sender: TObject);
begin
  Hide;
end;

procedure TForm2.Edit1Exit(Sender: TObject);
begin
  DebugLn('[Edit1Exit]');
end;

procedure TForm2.Edit1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  DebugLn('[Edit1KeyDown] Key=' + DbgsVKCode(Key));
end;

procedure TForm2.Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  DebugLn('[Edit1KeyUp] Key=' + DbgsVKCode(Key));
end;

procedure TForm2.Edit1UTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
begin
  DebugLn('[Edit1UTF8KeyPress] Char=' + UTF8Key);
end;

end.

