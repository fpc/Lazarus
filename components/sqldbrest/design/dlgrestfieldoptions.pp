unit dlgrestfieldoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ButtonPanel, sqldbrestschema;

type

  { TRestFieldOptionsDialog }

  TRestFieldOptionsDialog = class(TForm)
    BPFieldOptions: TButtonPanel;
    CGFieldOptions: TCheckGroup;
  private
    function GetOptions: TRestFieldOptions;
    procedure Setoptions(AValue: TRestFieldOptions);
  public
    Property Options : TRestFieldOptions Read GetOptions Write Setoptions;
  end;

Function GetRestFieldOptions(var aOptions : TRestFieldOptions) : Boolean;

implementation

Function GetRestFieldOptions(var aOptions : TRestFieldOptions) : Boolean;

begin
  With TRestFieldOptionsDialog.Create(Application) do
    try
      Options:=aOptions;
      Result:=ShowModal=mrOK;
      if Result then
        aOptions:=Options;
    finally
      Free;
    end;
end;

{$R *.lfm}

function TRestFieldOptionsDialog.GetOptions: TRestFieldOptions;

  Procedure DoOption(O : TRestFieldOption);

  begin
    if CGFieldOPtions.Checked[Ord(O)] then
      Include(Result,O);
  end;

Var
  O : TRestFieldOption;

begin
  Result:=[];
  For O in TRestFieldOption do
    DoOption(O);
end;

procedure TRestFieldOptionsDialog.Setoptions(AValue: TRestFieldOptions);

  Procedure DoOption(O : TRestFieldOption);

  begin
    CGFieldOptions.Checked[Ord(O)]:=O in aValue;
  end;

Var
  O : TRestFieldOption;

begin
  For O in TRestFieldOption do
    DoOption(O);
end;

end.

