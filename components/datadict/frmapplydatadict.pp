unit frmapplydatadict;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TApplyDataDictionaryForm }

  TApplyDataDictionaryForm = class(TForm)
    BOK: TButton;
    MLog: TMemo;
    procedure BOKClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  ApplyDataDictionaryForm: TApplyDataDictionaryForm;

implementation

{ TApplyDataDictionaryForm }

procedure TApplyDataDictionaryForm.BOKClick(Sender: TObject);
begin
  Close;
end;

initialization
  {$I frmapplydatadict.lrs}

end.

