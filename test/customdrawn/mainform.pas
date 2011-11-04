unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, customdrawncontrols, Forms, Controls, Graphics,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CDButton1: TCDButton;
    CDButton2: TCDButton;
    CDButton3: TCDButton;
    CDButton4: TCDButton;
    CDCheckBox1: TCDCheckBox;
    CDCheckBox2: TCDCheckBox;
    CDEdit1: TCDEdit;
    CDEdit2: TCDEdit;
    editWinXP: TCDEdit;
    CDGroupBox1: TCDGroupBox;
    CDGroupBox2: TCDGroupBox;
    CDPageControl1: TCDPageControl;
    CDPageControl2: TCDPageControl;
    CDTabSheet1: TCDTabSheet;
    CDTabSheet2: TCDTabSheet;
    CDTabSheet3: TCDTabSheet;
    CDTabSheet4: TCDTabSheet;
    CDTabSheet5: TCDTabSheet;
    CheckBox1: TCheckBox;
    comboControls: TComboBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    memoLog: TMemo;
    notebookControls: TNotebook;
    pageButtons: TPage;
    PageControl1: TPageControl;
    pageEdits: TPage;
    pageCheckboxes: TPage;
    pageGroupBoxes: TPage;
    pageTrackBars: TPage;
    pagePageControls: TPage;
    pageTabControls: TPage;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TrackBar1: TTrackBar;
    CDTrackBar1: TCDTrackBar;
    procedure comboControlsChange(Sender: TObject);
    procedure HandleClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses TypInfo, customdrawndrawers;

{$R *.lfm}

{ TForm1 }

procedure TForm1.comboControlsChange(Sender: TObject);
begin
  notebookControls.PageIndex := comboControls.ItemIndex;
//  Caption := GetEnumName(TypeInfo(TCDDrawStyle), Integer(editWinXP.DrawStyle));
end;

procedure TForm1.HandleClick(Sender: TObject);
begin
  memoLog.Lines.Add(Format('%s: %s OnClick', [TControl(Sender).Name, TControl(Sender).ClassName]));
end;

end.

