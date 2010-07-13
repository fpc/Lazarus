unit lr_expres;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ExtCtrls, ButtonPanel, SynEdit;

type

  { TlrExpresionEditorForm }

  TlrExpresionEditorForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    ButtonPanel1: TButtonPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Panel1: TPanel;
    Memo1: TSynEdit;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure BitBtn3Click(Sender: TObject);
    procedure Button13Click(Sender: TObject);
  private
    procedure AddWord(S:string);
  public
    function ResultExpresion:string;
  end; 

implementation

{$R *.lfm}

uses LR_Var, LR_Flds, lr_funct_editor_unit, lr_funct_editor_unit1, LR_Class;

{ TlrExpresionEditorForm }

procedure TlrExpresionEditorForm.Button13Click(Sender: TObject);
begin
  AddWord((Sender as TButton).Caption);
end;

procedure TlrExpresionEditorForm.BitBtn2Click(Sender: TObject);
begin
  frVarForm := TfrVarForm.Create(nil);
  try
    with frVarForm do
    if ShowModal = mrOk then
    begin
      if SelectedItem <> '' then
        AddWord('[' + SelectedItem + ']');
    end;
  finally
    frVarForm.Free;
  end;
end;

procedure TlrExpresionEditorForm.BitBtn1Click(Sender: TObject);
var
  LR_FunctEditorForm: TLR_FunctEditorForm;
  FD:TfrFunctionDescription;
  LR_FunctEditor1Form: TLR_FunctEditor1Form;
begin
  FD:=nil;
  LR_FunctEditorForm:=TLR_FunctEditorForm.Create(Application);
  try
    if LR_FunctEditorForm.ShowModal = mrOk then
      FD:=LR_FunctEditorForm.CurentFunctionDescription;
  finally
    LR_FunctEditorForm.Free;
  end;
  if Assigned(FD) then
  begin
    LR_FunctEditor1Form:=TLR_FunctEditor1Form.Create(Application);
    try
      LR_FunctEditor1Form.SetFunctionDescription(FD);
      if LR_FunctEditor1Form.ShowModal = mrOk then
        AddWord(LR_FunctEditor1Form.ResultText);
    finally
      LR_FunctEditor1Form.Free;
    end;
  end;
end;

procedure TlrExpresionEditorForm.BitBtn3Click(Sender: TObject);
begin
  frFieldsForm := TfrFieldsForm.Create(nil);
  try
    with frFieldsForm do
    begin
      if ShowModal = mrOk then
      begin
        if DBField <> '' then
          AddWord('[' + DBField + ']');
      end;
    end;
  finally
    frFieldsForm.Free;
  end;
end;

procedure TlrExpresionEditorForm.AddWord(S: string);
begin
  if Memo1.Lines.Count = 0 then
    Memo1.Lines.Add(S)
  else
  begin
    Memo1.Lines[Memo1.Lines.Count-1]:=Memo1.Lines[Memo1.Lines.Count-1] + S;
  end;
  Memo1.CaretY:=Memo1.Lines.Count-1;
  Memo1.CaretX:=Length(Memo1.Lines[Memo1.Lines.Count-1])+1;
  Memo1.SetFocus;
end;

function TlrExpresionEditorForm.ResultExpresion: string;
begin
  Result:=Trim(Memo1.Text);
end;

end.

