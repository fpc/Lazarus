unit Main;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes,
  Forms, Controls, ComCtrls, //ExtCtrls, StdCtrls, FileUtil,
  frmBubble, frmStacked, frmBoxWhisker, frmOHLC, frmField;

type

  { TMainForm }

  TMainForm = class(TForm)
    PageControl1: TPageControl;
    tsField: TTabSheet;
    tsOHLC: TTabSheet;
    tsWhiskers: TTabSheet;
    tsStacked: TTabSheet;
    tsBubble: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FBubbleFrame: TBubbleFrame;
    FStackedFrame: TStackedFrame;
    FBoxWhiskerFrame: TBoxWhiskerFrame;
    FOHLCFrame: TOHLCFrame;
    FFieldFrame: TFieldFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBubbleFrame := TBubbleFrame.Create(Self);
  FBubbleFrame.Parent := tsBubble;
  FBubbleFrame.Align := alClient;

  FStackedFrame := TStackedFrame.Create(Self);
  FStackedFrame.Parent := tsStacked;
  FStackedFrame.Align := alClient;

  FBoxWhiskerFrame := TBoxWhiskerFrame.Create(Self);
  FBoxWhiskerFrame.Parent := tsWhiskers;
  FBoxWhiskerFrame.Align := alClient;

  FOHLCFrame := TOHLCFrame.Create(Self);
  FOHLCFrame.Parent := tsOHLC;
  FOHLCFrame.Align := alClient;

  FFieldFrame := TFieldFrame.Create(Self);
  FFieldFrame.Parent := tsField;
  FFieldFrame.Align := alClient;
end;

end.

