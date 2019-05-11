unit frmsqldbrestselectconn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls, sqldbrestschema, sqldbrestbridge;

type

  { TSelectRestConnectionForm }

  TSelectRestConnectionForm = class(TForm)
    BPSelect: TButtonPanel;
    CGFieldOPtions: TCheckGroup;
    CBAllTables: TCheckBox;
    RGConnection: TRadioGroup;
  private
    FShowOptions: Boolean;
    procedure SetShowOptions(AValue: Boolean);
  public
    Procedure Init(aList : TSQLDBRestConnectionList); overload;
    Procedure Init(aList : TStrings); overload;
    Property ShowOptions : Boolean Read FShowOptions Write SetShowOptions;
    Function SelectedConnection : TSQLDBRestConnection;
    Function SelectedOptions : TRestFieldOptions;
    Function AllTables : Boolean;
  end;

Function SelectRestConnection(aList : TSQLDBRestConnectionList) : TSQLDBRestConnection;
Function SelectRestConnection(aList : TStrings; out aOptions: TRestFieldOptions; Out aAllTables : Boolean) : TSQLDBRestConnection;
Function SelectRestConnection(aList : TSQLDBRestConnectionList; out aOptions: TRestFieldOptions; Out aAllTables : Boolean) : TSQLDBRestConnection;

var
  SelectRestConnectionForm: TSelectRestConnectionForm;

implementation

{$R *.lfm}

Function SelectRestConnection(aList : TStrings; out aOptions: TRestFieldOptions; Out aAllTables : Boolean) : TSQLDBRestConnection;
begin
  Result:=Nil;
  if Alist.Count<>0 then
    With TSelectRestConnectionForm.Create(Application) do
      try
        Init(aList);
        ShowOptions:=True;
        if ShowModal=mrOK then
          begin
          Result:=SelectedConnection;
          aOptions:=SelectedOptions;
          aAllTables:=AllTables;
          end;
      finally
        Free;
      end;
end;
Function DoSelectRestConnection(aList : TSQLDBRestConnectionList; withOpts : Boolean; out aOptions: TRestFieldOptions; Out aAllTables : Boolean) : TSQLDBRestConnection;

begin
  Result:=Nil;
  if Alist.Count<>0 then
    With TSelectRestConnectionForm.Create(Application) do
      try
        Init(aList);
        ShowOptions:=WithOpts;
        if ShowModal=mrOK then
          begin
          Result:=SelectedConnection;
          if WithOpts then
            aOptions:=SelectedOptions;
          aAllTables:=AllTables;
          end;
      finally
        Free;
      end;
end;

Function SelectRestConnection(aList : TSQLDBRestConnectionList; out aOptions: TRestFieldOptions; Out aAllTables : Boolean) : TSQLDBRestConnection;

begin
  Result:=DoSelectRestConnection(alIst,True,aOptions,aAllTables);
end;

Function SelectRestConnection(aList : TSQLDBRestConnectionList) : TSQLDBRestConnection;

Var
  aOptions: TRestFieldOptions;
  B : Boolean;

begin
  Result:=DoSelectRestConnection(alIst,False,aOptions,B);
end;


{ TSelectRestConnectionForm }

procedure TSelectRestConnectionForm.SetShowOptions(AValue: Boolean);
begin
  if FShowOptions=AValue then Exit;
  FShowOptions:=AValue;
  if Not FShowOptions then
    begin
    CGFieldOptions.Visible:=False;
    RGConnection.Width:=CGFieldOptions.Left+CGFieldOptions.Width;
    end;
end;

procedure TSelectRestConnectionForm.Init(aList: TSQLDBRestConnectionList);

Var
  I : integer;

begin
  For I:=0 to aList.Count-1 do
    RGConnection.Items.AddObject(aList[I].Name,aList[i]);
end;

procedure TSelectRestConnectionForm.Init(aList: TStrings);
begin
  RGConnection.Items.Assign(aList);
end;

function TSelectRestConnectionForm.SelectedConnection: TSQLDBRestConnection;
begin
  With RGConnection do
    if ItemIndex=-1 then
      Result:=Nil
    else
      Result:=TSQLDBRestConnection(Items.Objects[ItemIndex]);
end;

function TSelectRestConnectionForm.SelectedOptions: TRestFieldOptions;

  Procedure Add(I : integer; V : TRestFieldOption);
  begin
    if CGFieldOPtions.Checked[I] then
      Include(Result,V);
  end;

begin
  Result:=[];
  Add(0,foFilter);
  Add(1,foOrderBy);
  Add(2,foOrderByDesc);
  Add(3,foInInsert);
  Add(4,foInUpdate);
  Add(5,foRequired);
end;

function TSelectRestConnectionForm.AllTables: Boolean;
begin
  Result:=CBAllTables.Checked;
end;

end.

