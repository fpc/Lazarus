unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ComCtrls,
  frmGeneral, frmErrorRange;

type

  { TForm1 }

  TForm1 = class(TForm)
    PageControl1: TPageControl;
    PgGeneral: TTabSheet;
    PgErrorRange: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FGeneralFrame: TGeneralFrame;
    FErrorRangeFrame: TErrorRangeFrame;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  FGeneralFrame := TGeneralFrame.Create(Self);
  FGeneralFrame.Parent := PgGeneral;
  FGeneralFrame.Align := alClient;

  FErrorRangeFrame := TErrorRangeFrame.Create(Self);
  FErrorRangeFrame.Parent := PgErrorRange;
  FErrorRangeFrame.Align := alClient;
end;

end.

