unit LR_EditVariables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel, LR_Intrp;

type

  { TlrEditVariablesForm }

  TlrEditVariablesForm = class(TForm)
    BitBtn1: TBitBtn;
    ButtonPanel1: TButtonPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    Memo2: TMemo;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
  private
    EditItem:integer;
    ValList:TStringList;
  public
    procedure LoadParamList(AVars:TfrVariables);
    procedure SaveParamList(AVars:TfrVariables);
  end;

var
  lrEditVariablesForm: TlrEditVariablesForm;

implementation
uses lr_expres;

{$R *.lfm}

{ TlrEditVariablesForm }

procedure TlrEditVariablesForm.ListBox1Click(Sender: TObject);
begin
  if (ListBox1.Items.Count>0) and (ListBox1.ItemIndex > -1) and (ListBox1.ItemIndex<ListBox1.Items.Count) then
  begin
    if EditItem>-1 then
      ValList.Values[ListBox1.Items[EditItem]]:=Memo2.Text;
    EditItem:=ListBox1.ItemIndex;
    Memo2.Text:=ValList.Values[ListBox1.Items[EditItem]];
  end;
end;

procedure TlrEditVariablesForm.FormCreate(Sender: TObject);
begin
  ValList:=TStringList.Create;
  Memo2.Text:='';
end;

procedure TlrEditVariablesForm.BitBtn1Click(Sender: TObject);
var
  EF:TlrExpresionEditorForm;
begin
  EF:=TlrExpresionEditorForm.Create(Application);
  if EF.ShowModal = mrOk then
    Memo2.Text:=EF.ResultExpresion;
  EF.Free;
end;

procedure TlrEditVariablesForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(ValList);
end;

procedure TlrEditVariablesForm.LoadParamList(AVars: TfrVariables);
var
  i:integer;
begin
  ListBox1.Items.Clear;
  for i:=0 to AVars.Count - 1 do
  begin
    ListBox1.Items.Add(AVars.Name[i]);
    ValList.Values[AVars.Name[i]]:=AVars.Value[i];
  end;
  EditItem:=-1;
  Memo2.Enabled:=ListBox1.Items.Count>0;
  ListBox1.Enabled:=ListBox1.Items.Count>0;
  if ListBox1.Items.Count>0 then
  begin
    ListBox1.ItemIndex:=0;
    ListBox1Click(nil);
  end;
end;

procedure TlrEditVariablesForm.SaveParamList(AVars: TfrVariables);
var
  i:integer;
begin
  ListBox1Click(nil);
  AVars.Clear;
  for i:=0 to ListBox1.Items.Count - 1 do
    AVars[ListBox1.Items[i]]:=ValList.Values[ListBox1.Items[i]];
end;

end.


