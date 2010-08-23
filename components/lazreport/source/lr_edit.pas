
{*****************************************}
{                                         }
{             FastReport v2.3             }
{               Memo editor               }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Edit;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls,ClipBrd,ExtCtrls,

  LCLType,LCLIntf,LCLProc,
  
  LR_Class, LR_Insp, lr_propedit, SynEdit, SynHighlighterPas;

type

  { TfrEditorForm }

  TfrEditorForm = class(TPropEditor)
    Button6: TButton;
    CB1: TCheckBox;
    CB2: TCheckBox;
    CB3: TCheckBox;
    Label1: TLabel;
    M1: TMemo;
    Panel2: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Panel3: TPanel;
    ScriptPanel: TPanel;
    Label2: TLabel;
    MemoPanel: TPanel;
    M2: TSynEdit;
    Splitter: TSplitter;
    SynPasSyn1: TSynPasSyn;
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure M1KeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure Button4Click(Sender: TObject);
    procedure M1Enter(Sender: TObject);
    procedure CB1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure CB2Click(Sender: TObject);
    procedure CB3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
    FActiveMemo: TMemo;
    //** procedure WMGetMinMaxInfo(var Msg: TLMGetMinMaxInfo); message LM_GETMINMAXINFO;
  public
    { Public declarations }
    function ShowEditor: TModalResult; override;
  end;


implementation

{$R *.lfm}

uses LR_Desgn, LR_Fmted, LR_Var, LR_Flds, LR_Const, lr_expres;

function TfrEditorForm.ShowEditor: TModalResult;
begin
  Result := mrCancel;
  if Assigned(View) then
    Result := inherited ShowEditor;
end;

procedure TfrEditorForm.FormShow(Sender: TObject);
begin
  {$IFDEF DebugLR}
  DebugLn('TfrEditorForm.FormShow INIT HandleAllocated=', dbgs(HandleAllocated));
  {$ENDIF}
  CB1Click(nil);
  CB2Click(nil);
  CB3Click(nil);
  M1.Lines.Text:=View.Memo.Text;
  if not M1.HandleAllocated then
    M1.SelStart:=0;
  M1.SetFocus;
  FActiveMemo := M1;
  CB1.Checked:=(View.Script.Count>0);
  M2.Lines.Text:=View.Script.Text;
  Button5.Visible := (View is TfrMemoView);
  M1.Font.Charset := frCharset;
  M2.Font.Charset := frCharset;
  {$IFDEF DebugLR}
  DebugLn('TfrEditorForm.FormShow END');
  {$ENDIF}
end;

procedure TfrEditorForm.FormHide(Sender: TObject);
begin
  if ModalResult = mrOk then
  begin
    frDesigner.BeforeChange;
    M1.WordWrap := False;
    View.Memo.Text := M1.Text;
    View.Script.Text := M2.Text;
  end;
end;

//**
{
procedure TfrEditorForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  with Msg.MinMaxInfo^ do
  begin
    ptMinTrackSize.x := Button2.Left + Button2.Width + 4 + 8;
    ptMinTrackSize.y := 200;
  end;
end;
}

procedure TfrEditorForm.Button3Click(Sender: TObject);
begin
  frVarForm := TfrVarForm.Create(Application);
  try
    with frVarForm do
    if ShowModal = mrOk then
    begin
      if SelectedItem <> '' then
      begin
        ClipBoard.Clear;
        ClipBoard.AsText := '[' + SelectedItem + ']';
        FActiveMemo.PasteFromClipboard;
      end;
    end;
  finally
    frVarForm.Free;
  end;
  FActiveMemo.SetFocus;
end;

procedure TfrEditorForm.Button6Click(Sender: TObject);
var
  lrExpresionEditorForm: TlrExpresionEditorForm;
begin
  lrExpresionEditorForm:=TlrExpresionEditorForm.Create(Application);
  try
    if lrExpresionEditorForm.ShowModal = mrOk then
    begin
      ClipBoard.Clear;
      ClipBoard.AsText := '[' + lrExpresionEditorForm.ResultExpresion + ']';
      FActiveMemo.PasteFromClipboard;
    end
  finally
    lrExpresionEditorForm.Free;
  end;
end;

procedure TfrEditorForm.M1KeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = vk_Insert) and (Shift = []) then Button3Click(Self);
  if Key = vk_Escape then ModalResult := mrCancel;
end;

procedure TfrEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Chr(Key) = 'F') and (ssCtrl in Shift) and Button5.Visible then
  begin
    Button5Click(nil);
    Key := 0;
  end;
  if (Key = vk_Return) and (ssCtrl in Shift) then
  begin
    ModalResult := mrOk;
    Key := 0;
  end;
end;

procedure TfrEditorForm.Button4Click(Sender: TObject);
begin
  frFieldsForm := TfrFieldsForm.Create(Application);
  try
    with frFieldsForm do
    begin
      if ShowModal = mrOk then
      begin
        if DBField <> '' then
        begin
          ClipBoard.Clear;
          ClipBoard.AsText := '[' + DBField + ']';
          FActiveMemo.PasteFromClipboard;
        end;
      end;
    end;
  finally
    frFieldsForm.Free;
  end;
  FActiveMemo.SetFocus;
end;

procedure TfrEditorForm.M1Enter(Sender: TObject);
begin
  FActiveMemo := Sender as TMemo;
end;

procedure TfrEditorForm.CB1Click(Sender: TObject);
begin
  ScriptPanel.Visible := CB1.Checked;
  Splitter.Visible:= CB1.Checked;
  if Splitter.Visible then
    Splitter.Top:=MemoPanel.Height+1;
end;

procedure TfrEditorForm.CB2Click(Sender: TObject);
begin
{  if CB2.Checked then
    M1.Font.Size := 12
  else
    M1.Font.Size := 10;
  M2.Font.Size := M1.Font.Size;
}
end;

procedure TfrEditorForm.CB3Click(Sender: TObject);
begin
  M1.WordWrap := CB3.Checked;
end;

procedure TfrEditorForm.FormCreate(Sender: TObject);
begin
  Caption := sEditorFormCapt;
  Label1.Caption := sEditorFormMemo;
  CB1.Caption := sEditorFormScript;
  CB2.Caption := sEditorFormBig;
  CB3.Caption := sEditorFormWord;
  Label2.Caption := sEditorFormScr;
  Button3.Caption := sEditorFormVar;
  Button4.Caption := sEditorFormField;
  Button5.Caption := sEditorFormFormat;
  Button6.Caption := sEditorFormFunction;
  Button1.Caption := sOk;
  Button2.Caption := sCancel;
end;

procedure TfrEditorForm.Button5Click(Sender: TObject);
var
  t: TfrMemoView;
begin
  t := TfrMemoView(View);
  frFmtForm := TfrFmtForm.Create(nil);
  with frFmtForm do
  begin
    EdFormat := t.Format;
    EdFormatStr:=t.FormatStr;
    if ShowModal = mrOk then
    begin
      frDesigner.BeforeChange;
      t.Format := EdFormat;
      t.FormatStr := EdFormatStr;
    end;
  end;
  frFmtForm.Free;
end;

procedure TfrEditorForm.FormResize(Sender: TObject);
begin
  //**
  {
  ptMinTrackSize.x := Button2.Left + Button2.Width + 4 + 8;
  ptMinTrackSize.y := 200;
  }
end;

end.

