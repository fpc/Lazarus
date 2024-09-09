{
 *****************************************************************************
  This file is part of a Lazarus Package, IconFinder.

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.
 *****************************************************************************

 Editor for keyword search phrases.
}

unit IconKeywordFilterEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ButtonPanel, Spin,
  IconFinderStrConsts;

type

  { TKeywordFilterEditorForm }

  TKeywordFilterEditorForm = class(TForm)
    Bevel1: TBevel;
    btnClear: TBitBtn;
    btnAND: TBitBtn;
    btnNOT: TBitBtn;
    btnOR: TBitBtn;
    BitBtn3: TBitBtn;
    btnNew: TBitBtn;
    btnEdit: TBitBtn;
    btnAdd: TBitBtn;
    ButtonPanel: TButtonPanel;
    edFilter: TEdit;
    FilterPanel: TPanel;
    lblKeywords: TLabel;
    lblFilter: TLabel;
    lblColumns: TLabel;
    lbKeywords: TListBox;
    KeywordsPanel: TPanel;
    OperationPanel: TPanel;
    RightPanel: TPanel;
    seColumns: TSpinEdit;
    procedure btnClearClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnANDClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnNOTClick(Sender: TObject);
    procedure btnORClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lbKeywordsDblClick(Sender: TObject);
    procedure seColumnsChange(Sender: TObject);
  private
    FActivated: Boolean;
    function GetFilter: String;
    procedure SetFilter(AValue: String);
    procedure SetKeywords(AValue: TStrings);
    procedure UpdateLayout;

  public
    procedure UpdateLanguage;
    property Filter: String read GetFilter write SetFilter;
    property Keywords: TStrings write SetKeywords;

  end;

var
  KeywordFilterEditorForm: TKeywordFilterEditorForm;

implementation

{$R *.lfm}

function EndSpace(s: String): String;
begin
  if (s <> '') and (s[Length(s)] <> ' ') then
    Result := s + ' '
  else
    Result := s;
end;

{ TKeywordFilterEditorForm }

procedure TKeywordFilterEditorForm.btnAddClick(Sender: TObject);
var
  keyword: String;
begin
  if lbKeywords.ItemIndex >-1 then
  begin
    keyword := lbKeywords.Items[lbKeywords.ItemIndex];
    edFilter.Text := EndSpace(edFilter.Text) + keyword;
  end;
end;

procedure TKeywordFilterEditorForm.btnClearClick(Sender: TObject);
begin
  edFilter.Text := '';
end;

procedure TKeywordFilterEditorForm.btnANDClick(Sender: TObject);
begin
  edFilter.Text := EndSpace(edFilter.Text) + 'AND';
end;

procedure TKeywordFilterEditorForm.btnNewClick(Sender: TObject);
var
  idx: Integer;
  keyword: String;
  list: TStringList;
begin
  keyword := '';
  if InputQuery(RSKeywordEditor_NewKeyword, RSKeywordEditor_Keyword, keyword) then
  begin
    idx := lbKeywords.Items.IndexOf(keyword);
    if idx > -1 then
    begin
      MessageDlg(RSKeywordEditor_KeywordExists, mtInformation, [mbOK], 0);
      lbKeywords.ItemIndex := idx;
    end else
    begin
      list := TStringList.Create;
      try
        list.Assign(lbKeywords.Items);
        list.Sorted := true;
        idx := list.Add(keyword);
        lbKeywords.Items.Assign(list);
      finally
        list.Free;
      end;
    end;
    lbKeywords.ItemIndex := idx;
  end;
end;

procedure TKeywordFilterEditorForm.btnNOTClick(Sender: TObject);
begin
  edFilter.Text := EndSpace(edFilter.Text) + 'NOT';
end;

procedure TKeywordFilterEditorForm.btnORClick(Sender: TObject);
begin
  edFilter.Text := EndSpace(edFilter.Text) + 'OR';
end;

procedure TKeywordFilterEditorForm.FormActivate(Sender: TObject);
begin
  if not FActivated then
  begin
    FActivated := true;
    UpdateLayout;
  end;
end;

procedure TKeywordFilterEditorForm.FormCreate(Sender: TObject);
begin
  UpdateLanguage;
end;

function TKeywordFilterEditorForm.GetFilter: String;
begin
  Result := edFilter.Text;
end;

procedure TKeywordFilterEditorForm.lbKeywordsDblClick(Sender: TObject);
begin
  btnAddClick(nil);
end;

procedure TKeywordFilterEditorForm.seColumnsChange(Sender: TObject);
begin
  lbKeywords.Columns := seColumns.Value;
end;

procedure TKeywordFilterEditorForm.SetFilter(AValue: String);
begin
  edFilter.Text := trim(AValue);
end;

procedure TKeywordFilterEditorForm.SetKeywords(AValue: TStrings);
begin
  lbKeywords.Items.Assign(AValue);
end;

procedure TKeywordFilterEditorForm.UpdateLanguage;
begin
  Caption := RSKeywordEditor_Caption;
  lblFilter.Caption := RSKeywordEditor_Filter;
  lblKeywords.Caption := RSKeywordEditor_Keywords;
  btnAdd.Caption := RSKeywordEditor_Add;
  btnClear.Caption := RSKeywordEditor_Clear;
  btnNew.Caption := RSKeywordEditor_New;
  btnEdit.Caption := RSKeywordEditor_Edit;
  lblColumns.Caption := RSKeywordeditor_Columns;
  UpdateLayout;
end;

procedure TKeywordFilterEditorForm.UpdateLayout;
begin
  Constraints.MinWidth :=
    OperationPanel.Width + OperationPanel.BorderSpacing.Left * 2;

  Constraints.MinHeight :=
    KeywordsPanel.Top + RightPanel.Top + seColumns.Top + seColumns.Height +
    ButtonPanel.Height + 2*ButtonPanel.BorderSpacing.Around;

  if Width < Constraints.MinWidth then   // Enforce constraints
    Width := 0;
  if Height < Constraints.MinHeight then
    Height := 0;
end;

end.

