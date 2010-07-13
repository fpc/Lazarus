
{*****************************************}
{                                         }
{             FastReport v2.3             }
{     Select Band datasource dialog       }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_VBnd;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources,
  Forms, Controls, Graphics, Dialogs,
  Buttons, StdCtrls, ButtonPanel, Spin,Variants,

  LR_Class, LR_Intrp;

type

  { TfrVBandEditorForm }

  TfrVBandEditorForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    edtRecCount: TSpinEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    CB1: TComboBox;
    LB1: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure CB1Click(Sender: TObject);
    procedure LB1Click(Sender: TObject);
    procedure CB1Exit(Sender: TObject);
  private
    { Private declarations }
    Band: TfrBandView;
    List: TfrVariables;
    procedure FillCombo;
  public
    { Public declarations }
    procedure ShowEditor(t: TfrView);
  end;

var
  frVBandEditorForm: TfrVBandEditorForm;

implementation

{$R *.lfm}

uses LR_DSet, LR_Const, LR_Utils;

procedure TfrVBandEditorForm.ShowEditor(t: TfrView);
var
  i, j, n: Integer;
  s: String;
  t1: TfrView;
  b: Boolean;
begin
  Band := t as TfrBandView;
  List := TfrVariables.Create;
  try
    s := Band.Dataset;
    b := False;
    if Pos(';', s) = 0 then
      b := True;
    with frDesigner.Page do
    begin
      for i := 0 to Objects.Count - 1 do
      begin
        t1 := TfrView(Objects[i]);
        if (t1.Typ = gtBand) and not (TfrBandView(t1).BandType in
          [btReportTitle..btPageFooter, btOverlay, btCrossHeader..btCrossFooter]) then
        begin
          LB1.Items.Add(t1.Name + ': ' + frBandNames[TfrBandView(t1).BandType]);
          n := Pos(AnsiUpperCase(t1.Name) + '=', AnsiUpperCase(s));
          if n <> 0 then
          begin
            n := n + Length(t1.Name) + 1;
            j := n;
            while s[j] <> ';' do
              Inc(j);
            List[t1.Name] := Copy(s, n, j - n);
          end
          else
            if b then
              List[t1.Name] := s
            else
              List[t1.Name] := '0';
        end;
      end;
    end;
    
    if LB1.Items.Count = 0 then
      Exit;

    FillCombo;
    LB1.ItemIndex := 0;
    LB1Click(nil);

    if ShowModal = mrOk then
    begin
      CB1Exit(nil);
      frDesigner.BeforeChange;
      s := '';
      for i := 0 to List.Count - 1 do
      begin
        s := s + List.Name[i] + '=' + VarToStr(List.Value[i]) + ';';
      end;
      Band.DataSet := s;
    end;
  finally
    List.Free;
  end;
end;

procedure TfrVBandEditorForm.FillCombo;
begin
  frGetComponents(CurReport.Owner, TfrDataset, CB1.Items, nil);
  TStringList(CB1.Items).Sorted := False;
  TStringList(CB1.Items).Sort;
  CB1.Items.Insert(0, sVirtualDataset);
  CB1.Items.Insert(0, sNotAssigned);
end;

procedure TfrVBandEditorForm.FormCreate(Sender: TObject);
begin
  Caption := sVBandEditorFormCapt;
  GroupBox1.Caption := sVBandEditorFormBnd;
  GroupBox2.Caption := sVBandEditorFormDataSource;
  Label1.Caption := sVBandEditorFormRecordCount;
end;

procedure TfrVBandEditorForm.CB1Click(Sender: TObject);
begin
  frEnableControls([Label1, edtRecCount], CB1.ItemIndex = 1);
end;

procedure TfrVBandEditorForm.LB1Click(Sender: TObject);
var
  i: Integer;
  s: String;
begin
  s := LB1.Items[LB1.ItemIndex];
  s := Copy(s, 1, Pos(':', s) - 1);
  s := List[s];
  if (s <> '') and (s[1] in ['1'..'9']) then
  begin
    i := 1;
    edtRecCount.Text := s;
  end
  else
  begin
    i := CB1.Items.IndexOf(s);
    if i = -1 then
      i := CB1.Items.IndexOf(sNotAssigned);
  end;
  CB1.ItemIndex := i;
  CB1Click(nil);
end;

procedure TfrVBandEditorForm.CB1Exit(Sender: TObject);
var
  s: String;
begin
  s := LB1.Items[LB1.ItemIndex];
  s := Copy(s, 1, Pos(':', s) - 1);
  if CB1.ItemIndex = 1 then
    List[s] := edtRecCount.Text
  else
    List[s] := CB1.Items[CB1.ItemIndex];
end;

end.

