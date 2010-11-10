unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, SynEdit,
  SynCompletion;

type

  { TForm1 }

  TForm1 = class(TForm)
    chkSizeDrag: TCheckBox;
    chkSearch: TCheckBox;
    chkExec: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    procedure chkExecChange(Sender: TObject);
    procedure chkSearchChange(Sender: TObject);
    procedure chkSizeDragChange(Sender: TObject);
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
  if chkExec.Checked then begin
    Add('Personal Computer');
    Add('Personal');
    Add('Computer');
    Add('Police Constable');
    Add('Police');
    Add('Constable');
  end else begin
    SynCompletion.ItemList.Add('Personal Computer');
    SynCompletion.ItemList.Add('Personal');
    SynCompletion.ItemList.Add('Computer');
    SynCompletion.ItemList.Add('Police Constable');
    SynCompletion.ItemList.Add('Police');
    SynCompletion.ItemList.Add('Constable');
  end;
end;

procedure TForm1.DoSearchPosition(var APosition: integer);
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
  if SynCompletion.ItemList.Count > 0 then
    APosition := 0
  else
    APosition := -1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1Change(nil);
  SynCompletion := TSynCompletion.Create(Form1);
  SynCompletion.Editor := SynEdit1;
  SynCompletion.CaseSensitive := False;
  SynCompletion.OnExecute := @DoExecute;
  SynCompletion.OnSearchPosition := @DoSearchPosition;
  SynCompletion.ShowSizeDrag := True;
  SynCompletion.DoubleClickSelects := True;
end;

procedure TForm1.chkExecChange(Sender: TObject);
begin
  SynEdit1.SetFocus;
end;

procedure TForm1.chkSearchChange(Sender: TObject);
begin
  if chkSearch.Checked then
    SynCompletion.OnSearchPosition := @DoSearchPosition
  else
    SynCompletion.OnSearchPosition := nil;
  SynEdit1.SetFocus;
end;

procedure TForm1.chkSizeDragChange(Sender: TObject);
begin
  SynCompletion.ShowSizeDrag := chkSizeDrag.Checked;
end;

end.

