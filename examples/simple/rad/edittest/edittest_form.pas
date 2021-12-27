unit edittest_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  SynHighlighterPas, SynEdit;

type
  
  { TEditTestForm }

  TEditTestForm = class(TForm)
    Edit1: TEdit;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
  private

  public

  end;

var
  EditTestForm: TEditTestForm;

implementation

{$R *.lfm}

end.

