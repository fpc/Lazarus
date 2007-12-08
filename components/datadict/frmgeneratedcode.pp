unit frmgeneratedcode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  SynHighlighterPas, SynMemo, ExtCtrls, StdCtrls;

type

  { TCodeForm }

  TCodeForm = class(TForm)
    BClose: TButton;
    BSave: TButton;
    PButtons: TPanel;
    SDCode: TSaveDialog;
    SynFreePascalSyn1: TSynFreePascalSyn;
    MCode: TSynMemo;
    procedure BSaveClick(Sender: TObject);
    function GetCode: TStrings;
    procedure SetCode(const AValue: TStrings);
  private
    FUnitName: String;
    { private declarations }
  public
    { public declarations }
    Property Code : TStrings Read GetCode Write SetCode;
    Property UnitName : String Read FUnitName Write FUnitName;
  end; 

var
  CodeForm: TCodeForm;

implementation

{ TCodeForm }

function TCodeForm.GetCode: TStrings;
begin
  Result:=MCode.Lines;
end;

procedure TCodeForm.BSaveClick(Sender: TObject);
begin
  With SDCode do
    begin
    If (UnitName<>'') then
      SDCode.FileName:=UnitName+'.pp';
    If Execute then
      MCode.Lines.SaveToFile(FileName);
    end;
end;

procedure TCodeForm.SetCode(const AValue: TStrings);
begin
  MCode.Lines.Assign(AValue);
end;

initialization
  {$I frmgeneratedcode.lrs}

end.

