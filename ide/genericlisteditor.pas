unit GenericListEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, LazarusIDEStrConsts;

type

  { TGenericListEditForm }

  TGenericListEditForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    Memo1: TMemo;
  private

  public

  end; 

var
  GenericListEditForm: TGenericListEditForm;

implementation

{$R *.lfm}

end.

