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
    CDEdit1: TCDEdit;
    CDGroupBox1: TCDGroupBox;
    CDGroupBox2: TCDGroupBox;
    CDPageControl1: TCDPageControl;
    CDTabSheet1: TCDTabSheet;
    CDTabSheet2: TCDTabSheet;
    CDTabSheet3: TCDTabSheet;
    CheckBox1: TCheckBox;
    comboControls: TComboBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    notebookControls: TNotebook;
    pageButtons: TPage;
    PageControl1: TPageControl;
    pageEdits: TPage;
    pageCheckboxes: TPage;
    pageGroupBoxes: TPage;
    pageTrackBars: TPage;
    pagePageControls: TPage;
    Page7: TPage;
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
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.comboControlsChange(Sender: TObject);
begin
  notebookControls.PageIndex := comboControls.ItemIndex;
end;

end.

