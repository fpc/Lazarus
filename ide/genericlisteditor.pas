unit GenericListEditor;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, ButtonPanel;

type

  { TGenericListEditForm }

  TGenericListEditForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
  end;

implementation

{$R *.lfm}

end.

