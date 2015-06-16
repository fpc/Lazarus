unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, TypInfo,
  // other forms
  toolbartest,
  // CD
  customdrawndrawers, customdrawn_common, customdrawn_mac;

type

  { TformCDControlsTest }

  TformCDControlsTest = class(TForm)
    Button1: TButton;
    comboDrawer: TComboBox;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  formCDControlsTest: TformCDControlsTest;

implementation

{$R *.lfm}

{ TformCDControlsTest }

procedure TformCDControlsTest.Button1Click(Sender: TObject);
begin
  FormToolBar.ShowModal();
end;

procedure TformCDControlsTest.FormCreate(Sender: TObject);
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

