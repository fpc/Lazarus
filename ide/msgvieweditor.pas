unit MsgViewEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls;

type
  { TMessageFilterRule }
  
  TMessageFilterAction = (
    mfaHide
    );
  
  TMessageFilterRule = class
  public
    property Expression: string;
    property SimpleSyntax: boolean;
    property Action: TMessageFilterAction;
  end;
  
  TMessageFilterRules = class(TList)
  private
    function GetItems(Index: integer): TMessageFilterRule;
    procedure SetItems(Index: integer; const AValue: TMessageFilterRule);
  public
    property Items[Index: integer]: TMessageFilterRule read GetItems write SetItems; default;
  end;

  { TMsgViewEditorDlg }

  TMsgViewEditorDlg = class(TForm)
    AddNewSetButton: TButton;
    ActiveFilterSetGroupBox: TGroupBox;
    RulesListView: TListView;
    OkButton: TButton;
    CancelButton: TButton;
    RenameSetButton: TButton;
    DeleteSetButton: TButton;
    FilterSetGroupBox: TGroupBox;
    FilterSetsListBox: TListBox;
    procedure AddNewSetButtonClick(Sender: TObject);
    procedure DeleteSetButtonClick(Sender: TObject);
    procedure MsgViewEditorDlgCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
    procedure RenameSetButtonClick(Sender: TObject);
  private
  public
  end;

var
  MsgViewEditorDlg: TMsgViewEditorDlg;

implementation

{ TMsgViewEditorDlg }

procedure TMsgViewEditorDlg.MsgViewEditorDlgCreate(Sender: TObject);
begin
  AddNewSetButton.Caption:='Add new set';
  ActiveFilterSetGroupBox.Caption:='Active Filter';
  OkButton.Caption:='Ok';
  CancelButton.Caption:='Cancel';
  RenameSetButton.Caption:='Rename';
  DeleteSetButton.Caption:='Delete';
  FilterSetGroupBox.Caption:='Filter Sets';
end;

procedure TMsgViewEditorDlg.DeleteSetButtonClick(Sender: TObject);
begin

end;

procedure TMsgViewEditorDlg.AddNewSetButtonClick(Sender: TObject);
begin

end;

procedure TMsgViewEditorDlg.OkButtonClick(Sender: TObject);
begin

end;

procedure TMsgViewEditorDlg.RenameSetButtonClick(Sender: TObject);
begin

end;

{ TMessageFilterRules }

function TMessageFilterRules.GetItems(Index: integer): TMessageFilterRule;
begin
  Result:=inherited Items[Index];
end;

procedure TMessageFilterRules.SetItems(Index: integer;
  const AValue: TMessageFilterRule);
begin
  inherited Items[Index]:=AValue;
end;

initialization
  {$I msgvieweditor.lrs}

end.

