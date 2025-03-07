unit main;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, Controls, ExtCtrls, ComCtrls,
  frmBasic, frmOwnerDraw;

type

  { TMainForm }

  TMainForm = class(TForm)
    PageControl: TPageControl;
    tsBasic: TTabSheet;
    tsOwnerDraw: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FBasicFrame: TBasicFrame;
    FOwnerDrawFrame: TOwnerDrawFrame;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FBasicFrame := TBasicFrame.Create(Self);
  FBasicFrame.Parent := tsBasic;
  FBasicFrame.Align := alClient;

  FOwnerDrawFrame := TOwnerDrawFrame.Create(self);
  FOwnerDrawFrame.Parent := tsOwnerDraw;
  FOwnerDrawFrame.Align := alClient;
end;

end.

