
{*****************************************}
{                                         }
{             FastReport v2.3             }
{        'Values' property editor         }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_Ev_ed;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls,ExtCtrls, ButtonPanel, LR_Class;

type

  { TfrEvForm }

  TfrEvForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    VarCombo: TComboBox;
    VarList: TListBox;
    ValCombo: TComboBox;
    ValList: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    Button3: TButton;
    SB1: TSpeedButton;
    SB2: TSpeedButton;
    Bevel1: TBevel;
    procedure VarComboClick(Sender: TObject);
    procedure ValComboClick(Sender: TObject);
    procedure VarListClick(Sender: TObject);
    procedure ValListClick(Sender: TObject);
    procedure Edit1Exit(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure SB1Click(Sender: TObject);
    procedure SB2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    function CurVar: String;
    function CurVal: String;
    function CurDataSet: String;
    procedure GetFields(Value: String);
    procedure GetSpecValues;
    procedure GetFRVariables;
    procedure FillVarCombo;
    procedure FillValCombo;
    procedure ShowVarValue(Value: String);
    procedure SetValTo(Value: String);
    procedure CheckForExpr;
    procedure PostVal;
  public
    { Public declarations }
    Doc: TfrReport;
    Str: TMemoryStream;
    Sl: TStringList;
    procedure Init;
    procedure RefreshVarList(Memo: TStrings);
    procedure CancelChanges;
  end;


function ShowEvEditor(Component: TfrReport): Boolean;

implementation

{$R *.lfm}

uses LR_Vared, LR_Const, LR_Utils, LR_DBRel, DB;

var
  SMemo: TStringList;
  VarClipbd: TMemoryStream;

function ShowEvEditor(Component: TfrReport): Boolean;
begin
  Result := False;
  with TfrEvForm.Create(nil) do
  try
    Doc := Component;
    Str := TMemoryStream.Create;
    Sl  := TStringList.Create;
    
    Doc.Values.WriteBinaryData(Str);
    Doc.Values.Items.Sorted := False;
    Sl.Assign(Doc.Variables);
    Init;
    SB2.Enabled:=(VarClipbd.Size<>0);
    if ShowModal = mrOk then
      Result := True
    else
      CancelChanges;
  finally
    Str.Free;
    Sl.Free;
    Free;
  end
end;

procedure TfrEvForm.Button3Click(Sender: TObject);
begin
  with TfrVaredForm.Create(nil) do
  try
    Doc := Self.Doc;
    if ShowModal = mrOk then
      RefreshVarList(Memo1.Lines);
  finally
    Free;
  end
end;

procedure TfrEvForm.Init;
begin
  FillVarCombo;
  FillValCombo;
  if VarCombo.Items.Count>0 then
    VarCombo.ItemIndex := 0;
  if ValCombo.Items.Count>0 then
    ValCombo.ItemIndex := 0;
  VarComboClick(nil);
  ValComboClick(nil);
  CheckForExpr;
end;

procedure TfrEvForm.RefreshVarList(Memo: TStrings);
var
  i, j, n: Integer;
  L      : TStringList;
begin
  L := TStringList.Create;
  try
    Doc.Variables.Assign(Memo);
    with Doc.Values do
    begin
      for i := Items.Count-1 downto 0 do
        if Doc.FindVariable(Items[i]) = -1 then
        begin
          Objects[i].Free;
          Items.Delete(i);
        end;
    end;
    
    Doc.GetCategoryList(L);
    n := L.Count;
    for i := 0 to n-1 do
    begin
      Doc.GetVarList(i, L);
      for j := 0 to L.Count-1 do
        with Doc.Values do
          if FindVariable(L[j]) = nil then
            Items[AddValue] := L[j];
    end;
    FillVarCombo;
    if VarCombo.Items.Count>0 then
      VarCombo.ItemIndex:=0;
    VarComboClick(nil);
  Finally
    L.Free;
  end;
end;

procedure TfrEvForm.CancelChanges;
begin
  Str.Position := 0;
  Doc.Values.ReadBinaryData(Str);
  Doc.Variables.Assign(Sl);
end;

function TfrEvForm.CurVar: String;
begin
  Result := '';
  if VarList.ItemIndex <> -1 then
    Result := VarList.Items[VarList.ItemIndex];
end;

function TfrEvForm.CurVal: String;
begin
  Result := '';
  if ValList.ItemIndex <> -1 then
    Result := ValList.Items[ValList.ItemIndex];
end;

function TfrEvForm.CurDataSet: String;
begin
  Result := '';
  if ValCombo.ItemIndex <> -1 then
    Result := ValCombo.Items[ValCombo.ItemIndex];
end;

procedure TfrEvForm.FillVarCombo;
begin
  VarCombo.Enabled:=False;
  Doc.GetCategoryList(VarCombo.Items);
  VarCombo.Enabled:=(VarCombo.Items.Count>0);
end;

procedure TfrEvForm.FillValCombo;
var  Lst: TStringList;
begin
  Lst := TStringList.Create;
  try
    ValCombo.Enabled:=False;

    frGetComponents(Doc.Owner, TDataSet, Lst, nil);
    Lst.Sort;
    Lst.Add(sSpecVal);
    Lst.Add(sFRVariables);
    ValCombo.Items.Assign(Lst);
    
    ValCombo.Enabled:=(ValCombo.Items.Count>0);
  finally
    Lst.Free;
  end;
end;

procedure TfrEvForm.VarComboClick(Sender: TObject);
begin
  Doc.GetVarList(VarCombo.ItemIndex, VarList.Items);
end;

procedure TfrEvForm.ValComboClick(Sender: TObject);
begin
  if CurDataSet = sFRVariables then
    GetFRVariables
  else if CurDataSet <> sSpecVal then
         GetFields(CurDataSet)
       else
         GetSpecValues;
end;

procedure TfrEvForm.VarListClick(Sender: TObject);
begin
  ShowVarValue(CurVar);
end;

procedure TfrEvForm.GetFields(Value: String);
var
  DataSet: TfrTDataSet;
begin
  ValList.Items.Clear;
  CurReport := Doc;
  DataSet := frGetDataSet(Value);
  if DataSet <> nil then
  try
    frGetFieldNames(DataSet, ValList.Items);
  except
  end;
  ValList.Items.Insert(0, sNotAssigned);
end;

procedure TfrEvForm.GetSpecValues;
var
  i: Integer;
begin
  with ValList.Items do
  begin
    Clear;
    Add(sNotAssigned);
    for i := 0 to frSpecCount - 1 do
      Add(frSpecArr[i]);
  end;
end;

procedure TfrEvForm.GetFRVariables;
var
  i: Integer;
begin
  with ValList.Items do
  begin
    Clear;
    Add(sNotAssigned);
    for i := 0 to frVariables.Count - 1 do
      Add(frVariables.Name[i]);
  end;
end;

procedure TfrEvForm.ShowVarValue(Value: String);
begin
  if Value='' then Exit;
  with Doc.Values.FindVariable(Value) do
    case Typ of
      vtNotAssigned:
        SetValTo(CurDataSet + '.' + sNotAssigned);
      vtDBField:
        SetValTo(DataSet + '.' + Field);
      vtFRVar:
        SetValTo(sFRVariables + '.' + Field);
      vtOther:
        begin
          SetValTo(sSpecVal + '.' + frSpecArr[OtherKind]);
          if OtherKind = 1 then
            Edit1.Text := Field;
        end;
    end;
end;

procedure TfrEvForm.SetValTo(Value: String);
var
  s1, s2, s3: String;
  i, j: Integer;
begin
  s1 := Copy(Value, 1, Pos('.', Value) - 1);
  s2 := Copy(Value, Pos('.', Value) + 1, 255);
  if Pos('.', s2) <> 0 then
  begin
    s3 := Copy(s2, Pos('.', s2) + 1, 255);
    s2 := Copy(s2, 1, Pos('.', s2) - 1);
    if AnsiCompareText(s1, Doc.Owner.Name) = 0 then
      s1 := s2 else
      s1 := s1 + '.' + s2;
    s2 := s3;
  end;

  with ValCombo do
  begin
    for i := 0 to Items.Count-1 do
    begin
      if Items[i] = s1 then
      begin
        if ItemIndex <> i then
        begin
          ItemIndex := i;
          ValComboClick(nil);
        end;
        with ValList do
        for j := 0 to Items.Count-1 do
          if Items[j] = s2 then
          begin
            ItemIndex := j;
            break;
          end;
        break;
      end;
    end;
  end;
  
  CheckForExpr;
end;

procedure TfrEvForm.ValListClick(Sender: TObject);
begin
  if VarList.ItemIndex < 0 then Exit;
  CheckForExpr;
end;

procedure TfrEvForm.CheckForExpr;
begin
  Edit1.Enabled := (CurDataSet = sSpecVal) and (CurVal = frSpecArr[1]);
  Label3.Enabled := Edit1.Enabled;
  if not Edit1.Enabled then
  begin
    Edit1.Text := '';
    Edit1.Color:= clBtnFace;
  end
  else
    Edit1.Color:= clWindow;
end;

procedure TfrEvForm.Edit1Exit(Sender: TObject);
begin
  PostVal;
end;

procedure TfrEvForm.PostVal;
var
  Val: TfrValue;
  i: Integer;
  s: String;
begin
  Val := Doc.Values.FindVariable(CurVar);
  if Val <> nil then
  with Val do
  begin
    if CurVal = sNotAssigned then
      Typ := vtNotAssigned
    else if CurDataSet = sSpecVal then
    begin
      Typ := vtOther;
      s := CurVal;
      for i := 0 to frSpecCount - 1 do
        if s = frSpecArr[i] then
        begin
          OtherKind := i;
          if i = 1 then // SExpr
            Field := Edit1.Text;
          break;
        end;
    end
    else if CurDataSet = sFRVariables then
    begin
      Typ := vtFRVar;
      Field := CurVal;
    end
    else
    begin
      Typ := vtDBField;
      DataSet := CurDataSet;
      Field := CurVal;
      OtherKind := 0;
    end;
  end;
end;

procedure TfrEvForm.SB1Click(Sender: TObject);
begin
  VarClipbd.Position := 0;
  Doc.Values.WriteBinaryData(VarClipbd);
  SMemo.Assign(Doc.Variables);
  frWriteMemo(VarClipbd, SMemo);
  SB2.Enabled := True;
end;

procedure TfrEvForm.SB2Click(Sender: TObject);
begin
  VarClipbd.Position := 0;
  Doc.Values.ReadBinaryData(VarClipbd);
  frReadMemo(VarClipbd, SMemo);
  Doc.Variables.Assign(SMemo);
  Init;
end;

procedure TfrEvForm.Button1Click(Sender: TObject);
begin
  PostVal;
end;

procedure TfrEvForm.FormCreate(Sender: TObject);
begin
  Caption := sEvFormCapt;
  Label1.Caption := sEvFormVar;
  Label2.Caption := sEvFormValue;
  Label3.Caption := sEvFormExp;
  SB1.Hint := sEvFormCopy;
  SB2.Hint := sEvFormPaste;
  Button3.Caption := sEvFormVars;

  Button3.Parent:=ButtonPanel1;
  SB1.Parent:=ButtonPanel1;
  SB2.Parent:=ButtonPanel1;
  Button3.AnchorSide[akLeft].Control:=ButtonPanel1.HelpButton;
  Button3.AnchorSide[akTop].Control:=ButtonPanel1.HelpButton;
  Button3.AnchorSide[akBottom].Control:=ButtonPanel1.HelpButton;

  SB1.AnchorSide[akTop].Control:=ButtonPanel1.HelpButton;
  SB1.AnchorSide[akBottom].Control:=ButtonPanel1.HelpButton;
  SB2.AnchorSide[akTop].Control:=ButtonPanel1.HelpButton;
  SB2.AnchorSide[akBottom].Control:=ButtonPanel1.HelpButton;
end;


initialization

  SMemo := TStringList.Create;
  VarClipbd := TMemoryStream.Create;

finalization
  SMemo.Free;
  VarClipbd.Free;
end.

