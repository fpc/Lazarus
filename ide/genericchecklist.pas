unit GenericCheckList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  StdCtrls, CheckLst;

type

  { TGenericCheckListForm }

  TGenericCheckListForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckListBox1: TCheckListBox;
  private

  public

  end; 

var
  GenericCheckListForm: TGenericCheckListForm;

implementation

{$R *.lfm}

end.

