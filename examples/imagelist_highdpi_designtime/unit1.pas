unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ImgList,
  Menus, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    ListView1: TListView;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PageControl1: TPageControl;
    RadioGroup1: TRadioGroup;
    TabSheet1: TTabSheet;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    TreeView1: TTreeView;
    procedure ImageList1GetWidthForPPI(Sender: TCustomImageList; AImageWidth,
      APPI: Integer; var AResultWidth: Integer);
    procedure RadioGroup1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ImageList1GetWidthForPPI(Sender: TCustomImageList;
  AImageWidth, APPI: Integer; var AResultWidth: Integer);
begin
  // make sure that the originally available resolutions (22px and 32px)
  // are used if 20-24px or 30-36px are needed
  case AResultWidth of
    20..24: AResultWidth := 22;
    30..36: AResultWidth := 32;
  end;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  case Radiogroup1.ItemIndex of
    0: ListView1.ViewStyle := vsIcon;
    1: ListView1.ViewStyle := vsSmallIcon;
  end;
end;

end.

