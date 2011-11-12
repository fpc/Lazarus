unit mainform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, customdrawncontrols, customdrawndrawers, Forms,
  Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CDButton1: TCDButton;
    CDButton2: TCDButton;
    CDButton3: TCDButton;
    CDButton4: TCDButton;
    CDCheckBox1: TCDCheckBox;
    CDCheckBox2: TCDCheckBox;
    CDCheckBox3: TCDCheckBox;
    CDCheckBox4: TCDCheckBox;
    CDCheckBox5: TCDCheckBox;
    CDCheckBox6: TCDCheckBox;
    CDEdit1: TCDEdit;
    CDEdit2: TCDEdit;
    CDTabControl1: TCDTabControl;
    sbCommon1: TCDScrollBar;
    sbCommon2: TCDScrollBar;
    TabControl1: TTabControl;
    trackScrollBarPageSize: TCDTrackBar;
    CheckBox2: TCheckBox;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    listviewCommon: TCDListView;
    progressCommon3: TCDProgressBar;
    progressCommon2: TCDProgressBar;
    progressCommon4: TCDProgressBar;
    progressNative2: TProgressBar;
    progressNative4: TProgressBar;
    progressCommon1: TCDProgressBar;
    CDRadioButton1: TCDRadioButton;
    CDRadioButton2: TCDRadioButton;
    CDRadioButton3: TCDRadioButton;
    CDStaticText1: TCDStaticText;
    CDTrackBar2: TCDTrackBar;
    sbNative1: TScrollBar;
    sbNative2: TScrollBar;
    trackChangeProgress: TCDTrackBar;
    CDTrackBar4: TCDTrackBar;
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
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListView1: TListView;
    Memo1: TMemo;
    memoLog: TMemo;
    notebookControls: TNotebook;
    pageToolBars: TPage;
    pageUpDowns: TPage;
    pageListViews: TPage;
    pageStatusBars: TPage;
    pageScrollBars: TPage;
    pageToggleBoxes: TPage;
    pageComboBoxes: TPage;
    pageStaticTexts: TPage;
    pageMenu: TPage;
    pagePopUp: TPage;
    pageEditMultiline: TPage;
    pageRadioButton: TPage;
    pagePanels: TPage;
    pageButtonGlyph: TPage;
    pageListBoxes: TPage;
    pageProgressBars: TPage;
    pageButtons: TPage;
    PageControl1: TPageControl;
    pageEdits: TPage;
    pageCheckboxes: TPage;
    pageGroupBoxes: TPage;
    pageTrackBars: TPage;
    pagePageControls: TPage;
    pageTabControls: TPage;
    Panel1: TPanel;
    progressNative1: TProgressBar;
    progressNative3: TProgressBar;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    ToggleBox1: TToggleBox;
    TrackBar1: TTrackBar;
    CDTrackBar1: TCDTrackBar;
    TrackBar2: TTrackBar;
    procedure comboControlsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HandleClick(Sender: TObject);
    procedure trackChangeProgressChange(Sender: TObject);
    procedure trackScrollBarPageSizeChange(Sender: TObject);
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

procedure TForm1.FormCreate(Sender: TObject);
var
  lItem: TCDListItems;
begin
  // We still dont have a property editor for this
  lItem := listviewCommon.Items.Add('First', -1, -1);
  lItem.Add('FirstSub1', -1, -1);
  lItem.Add('FirstSub2', -1, -1);
  lItem.Add('FirstSub3', -1, -1);
  lItem := listviewCommon.Items.Add('Second', -1, -1);
  lItem.Add('SecSub1', -1, -1);
  lItem := listviewCommon.Items.Add('Third', -1, -1);
  lItem.Add('3rdSub1', -1, -1);
  lItem := listviewCommon.Items.Add('Fourth', -1, -1);
  lItem.Add('4thSub1', -1, -1);
  lItem.Add('4thSub2', -1, -1);
end;

procedure TForm1.HandleClick(Sender: TObject);
begin
  memoLog.Lines.Add(Format('%s: %s OnClick', [TControl(Sender).Name, TControl(Sender).ClassName]));
end;

procedure TForm1.trackChangeProgressChange(Sender: TObject);
begin
  progressNative1.Position := trackChangeProgress.Position;
  progressNative2.Position := trackChangeProgress.Position;
  progressNative3.Position := trackChangeProgress.Position;
  progressNative4.Position := trackChangeProgress.Position;
  progressCommon1.Position := trackChangeProgress.Position;
  progressCommon2.Position := trackChangeProgress.Position;
  progressCommon3.Position := trackChangeProgress.Position;
  progressCommon4.Position := trackChangeProgress.Position;
end;

procedure TForm1.trackScrollBarPageSizeChange(Sender: TObject);
begin
  sbNative1.PageSize := trackScrollBarPageSize.Position;
  sbNative2.PageSize := trackScrollBarPageSize.Position;
  sbCommon1.PageSize := trackScrollBarPageSize.Position;
  sbCommon2.PageSize := trackScrollBarPageSize.Position;
end;

end.

