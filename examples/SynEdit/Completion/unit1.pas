unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, SynEdit,
  SynCompletion;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure Memo1Change(Sender: TObject);
  private
    { private declarations }
    SynCompletion: TSynCompletion;
    procedure DoExecute(Sender: TObject);
    procedure DoSearchPosition(var APosition: integer);
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Memo1Change(Sender: TObject);
begin
  SynAutoComplete1.AutoCompleteList := Memo1.Lines;
end;

procedure TForm1.DoExecute(Sender: TObject);
  procedure Add(s: String);
  begin
    if pos(lowercase(SynCompletion.CurrentString), lowercase(s)) = 1 then
      SynCompletion.ItemList.Add(s);
  end;
begin
  SynCompletion.ItemList.Clear;

  Add('Personal Computer');
  Add('Personal');
  Add('Computer');
  Add('Police Constable');
  Add('Police');
  Add('Constable');
end;

procedure TForm1.DoSearchPosition(var APosition: integer);
begin
  DoExecute(nil);
  APosition := 0;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1Change(nil);
  SynCompletion := TSynCompletion.Create(Form1);
  SynCompletion.Editor := SynEdit1;
  SynCompletion.CaseSensitive := False;
  SynCompletion.OnExecute := @DoExecute;
  SynCompletion.OnSearchPosition := @DoSearchPosition;
end;

end.

