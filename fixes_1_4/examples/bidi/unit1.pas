unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Dialogs, Forms, Controls, Graphics,
  StdCtrls, Buttons, Menus,
  ExtCtrls, PopupNotifier, ActnList, ComCtrls, Grids,
  ColorBox, CheckLst, DBGrids;

type

  { TForm1 }

  TForm1 = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckListBox1: TCheckListBox;
    ColorBox1: TColorBox;
    ColorListBox1: TColorListBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    DBGrid1: TDBGrid;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    FileMnu: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    NewMnu: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PageControl1: TPageControl;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    PopupNotifier1: TPopupNotifier;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioGroup1: TRadioGroup;
    ScrollBar1: TScrollBar;
    Shape2: TShape;
    SpeedButton1: TSpeedButton;
    StaticText1: TStaticText;
    StringGrid1: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TrayIcon1: TTrayIcon;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    procedure DefaultWidths;
  public
  end;

var
  Form1: TForm1; 

implementation

{$r *.lfm}

{ TForm1 }

procedure TForm1.Button3Click(Sender: TObject);
begin
  if BiDiMode <> bdLeftToRight then
    BiDiMode := bdLeftToRight
  else
    BiDiMode := bdRightToLeft;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  DefaultWidths;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  ParentBidiMode := True;
  if Application.BidiMode = bdRightToLeft then
    Application.BidiMode := bdLeftToRight
  else
    Application.BidiMode := bdRightToLeft;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
  if StringGrid1.TitleStyle >= tsNative then
    StringGrid1.TitleStyle := tsLazarus
  else
    StringGrid1.TitleStyle := succ(StringGrid1.TitleStyle);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TrayIcon1.BalloonHint := 'Balloon Hint?';
  TrayIcon1.Hint := 'Ok!';
  TrayIcon1.ShowBalloonHint;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FlipChildren(True);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  DefaultWidths;
end;

procedure TForm1.MenuItem20Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  PopupNotifier1.Text := 'Notify me?';
  PopupNotifier1.Show;
end;

procedure TForm1.DefaultWidths;
var
  i: Integer;
begin
  for i := 0 to StringGrid1.ColCount -1 do
    StringGrid1.ColWidths[i]:=50;
end;

end.

