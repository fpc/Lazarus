unit lr_funct_editor_unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, LR_Class, EditBtn, Buttons;

type

  { TLR_FunctEditor1Form }

  TLR_FunctEditor1Form = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure BitBtn5Click(Sender: TObject);
  private
    FParCount:integer;
    FD:TfrFunctionDescription;
  public
    procedure SetFunctionDescription(AFD:TfrFunctionDescription);
    function ResultText:string;
  end; 

implementation

{$R *.lfm}

uses lr_expres, lr_utils;

{ TLR_FunctEditor1Form }


procedure TLR_FunctEditor1Form.BitBtn5Click(Sender: TObject);
var
  EF:TlrExpresionEditorForm;
begin
  EF:=TlrExpresionEditorForm.Create(Application);
  try
    if EF.ShowModal = mrOk then
    case (Sender as TComponent).Tag of
      1:Edit1.Text:=EF.ResultExpresion;
      2:Edit2.Text:=EF.ResultExpresion;
      3:Edit3.Text:=EF.ResultExpresion;
    end;
  finally
    EF.Free;
  end;
end;


procedure TLR_FunctEditor1Form.SetFunctionDescription(AFD: TfrFunctionDescription
  );
var
  S, S1:string;
  i:integer;
begin

  // TODO: context sensitive inpunts, for example for
  //       bandname use the list of available bands.

  FD:=AFD;
  S:=FD.funDescription;
  S1:=Copy(S, 1, Pos('/', S)-1);

  FParCount:=0;
  for i:=1 to Length(S1) do
    if S1[i]='<' then
      Inc(FParCount);

  Label1.Caption:=S1;
  Delete(S, 1, Pos('/', S));
  Label2.Caption:=S;

  Label3.Enabled:=FParCount>0;
  Edit1.Enabled:=FParCount>0;
  BitBtn3.Enabled:=FParCount>0;
  
  Label4.Enabled:=FParCount>1;
  Edit2.Enabled:=FParCount>1;
  BitBtn4.Enabled:=FParCount>0;

  Label5.Enabled:=FParCount>2;
  Edit3.Enabled:=FParCount>2;
  BitBtn5.Enabled:=FParCount>0;

end;

function TLR_FunctEditor1Form.ResultText: string;
begin
  Result:='';
  if FParCount>0 then
   Result:=Result + '[' + lrGetUnBrackedStr(Edit1.Text) + ']';

  if FParCount>1 then
   Result:=Result + ', [' + lrGetUnBrackedStr(Edit2.Text) + ']';

  if FParCount>2 then
   Result:=Result + ', [' + lrGetUnBrackedStr(Edit3.Text) + ']';
   
  if FParCount>0 then
    Result:='('+Result+')';
  Result:=FD.funName + Result;
end;

end.

