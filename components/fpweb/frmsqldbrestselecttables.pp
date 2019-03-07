unit frmsqldbrestselecttables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, CheckLst;

type

  { TSQLDBRestSelectTablesForm }

  TSQLDBRestSelectTablesForm = class(TForm)
    BPSelectTables: TButtonPanel;
    CBSelectAllNone: TCheckBox;
    CLBTables: TCheckListBox;
    procedure CBSelectAllNoneChange(Sender: TObject);
  private
    procedure DoSelectAllNone(aSelect: Boolean);
    function GetTables: Tstrings;
    procedure SetTables(AValue: Tstrings);

  public
    Function GetSelectedTables(aList : Tstrings) : Integer;
    Property Tables : Tstrings Read GetTables Write SetTables;
  end;

var
  SQLDBRestSelectTablesForm: TSQLDBRestSelectTablesForm;

implementation

{$R *.lfm}

{ TSQLDBRestSelectTablesForm }

procedure TSQLDBRestSelectTablesForm.CBSelectAllNoneChange(Sender: TObject);
begin
  DoSelectAllNone(CBSelectAllNone.Checked);
end;

function TSQLDBRestSelectTablesForm.GetTables: Tstrings;
begin
  Result:=CLBTables.Items;
end;

procedure TSQLDBRestSelectTablesForm.DoSelectAllNone(aSelect : Boolean);

Var
  I : Integer;

begin
  With CLBTables do
    for I:=0 to Items.Count-1 do
      Checked[i]:=aSelect;
end;

procedure TSQLDBRestSelectTablesForm.SetTables(AValue: Tstrings);
begin
  CLBTables.Items.Assign(aValue);
  CBSelectAllNone.Checked:=True;
  DoSelectAllNone(True);
end;

function TSQLDBRestSelectTablesForm.GetSelectedTables(aList: Tstrings): Integer;

Var
  I : Integer;

begin
  aList.Clear;
  Result:=0;
  With CLBTables do
    for I:=0 to Items.Count-1 do
      if Checked[i] then
        begin
        aList.Add(Items[i]);
        Inc(Result);
        end;
end;

end.

