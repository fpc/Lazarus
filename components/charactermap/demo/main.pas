unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, Controls, Graphics, Dialogs, StdCtrls,
  CharacterMapFrm;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    procedure CharacterMapHelpHandler(Sender: TObject);
    procedure InsertCharacterHandler(const C: TUTF8Char);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.Button1Click(Sender: TObject);
begin
  if CharacterMapForm = nil then
  begin
    CharacterMapForm := TCharacterMapForm.Create(Application);
    CharacterMapForm.OnInsertCharacter := @InsertCharacterHandler;
    CharacterMapForm.OnShowHelp := @CharacterMapHelpHandler;
  end;
  CharacterMapForm.Show;
end;

procedure TMainForm.CharacterMapHelpHandler(Sender: TObject);
begin
  ShowMessage('Clicking on a character cell inserts the associated character into the editor text.');
end;

procedure TMainForm.InsertCharacterHandler(const C: TUTF8Char);
var
  txt: String;
  p: Integer;
begin
  p := Memo1.SelStart;
  txt := Memo1.Lines.Text;
  if Memo1.SelLength > 0 then
    Delete(txt, p+1, Memo1.SelLength);
  Insert(C, txt, p+1);
  Memo1.Lines.Text := txt;
  Memo1.SelStart := p + Length(C);
  Memo1.SelLength := 0;
end;

end.

