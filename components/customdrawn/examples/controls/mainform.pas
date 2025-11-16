unit MainForm;

{$mode objfpc}{$H+}

interface

uses     //        LazLogger,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, TypInfo,
  // other forms
  ToolBarTest, ButtonTest,
  // CD
  customdrawndrawers, customdrawn_common, customdrawn_mac, customdrawn_winxp;

type

  { TFormCDControlsTest }

  TFormCDControlsTest = class(TForm)
    Button1: TButton;
    Button2: TButton;
    comboDrawer: TComboBox;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormCDControlsTest: TFormCDControlsTest;

implementation

{$R *.lfm}

{ TFormCDControlsTest }

procedure TFormCDControlsTest.Button1Click(Sender: TObject);
begin
  FormToolBar.ShowModal();
end;

procedure TFormCDControlsTest.Button2Click(Sender: TObject);
begin
  FormButtons.ShowModal();
end;

procedure TFormCDControlsTest.FormCreate(Sender: TObject);
var
  lStyle: TCDDrawStyle;
  lStr: string;
begin
  for lStyle in TCDDrawStyle do
  begin
    lStr := GetEnumName(TypeInfo(TCDDrawStyle), integer(lStyle));
    comboDrawer.Items.Add(lStr);
  end;
  comboDrawer.ItemIndex := 1;
end;

end.

