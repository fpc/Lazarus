unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ActnList, StdActns, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ActionList1: TActionList;
    Edit1: TEdit;
    EditCopy1: TEditCopy;
    EditCut1: TEditCut;
    EditDelete1: TEditDelete;
    EditPaste1: TEditPaste;
    EditSelectAll1: TEditSelectAll;
    EditUndo1: TEditUndo;
    Memo1: TMemo;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

initialization
  {$I unit1.lrs}

end.

