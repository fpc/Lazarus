unit testmenuintf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, Menus, MenuIntf, testglobals;

type

  { TTestMenuIntfForm }

  TTestMenuIntfForm = class(TForm)
    TestMainMenuIntf1: TMainMenu;
    TestPopupMenuIntf1: TPopupMenu;
  private
  public
  end;

  { TMenuIntfTest }

  TTestMenuIntf = class(TTestCase)
  public
  published
    procedure TestMainMenu;
  end;

var
  TestMenuIntfForm: TTestMenuIntfForm;

implementation

{$R *.lfm}


{ TMenuIntfTest }

procedure TTestMenuIntf.TestMainMenu;
begin

end;

initialization
  AddToIDEIntfTestSuite(TTestMenuIntf);

end.

