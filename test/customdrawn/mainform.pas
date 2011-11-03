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
    CDButton1: TCDButton;
    CDButton2: TCDButton;
    CDEdit1: TCDEdit;
    comboControls: TComboBox;
    Edit1: TEdit;
    Memo1: TMemo;
    notebookControls: TNotebook;
    pageButtons: TPage;
    pageEdits: TPage;
    pageCheckboxes: TPage;
    pageGroupBoxes: TPage;
    pageTrackBars: TPage;
    pagePageControls: TPage;
    Page7: TPage;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
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

