unit editbuttonunit1;

{$mode objfpc}{$H+}

interface

uses
  RTTIGrids, Forms, StdCtrls, ExtCtrls, EditBtn, GroupedCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    EditBtnGrpGroupBox: TGroupBox;
    EditBtnSatGroupBox: TGroupBox;
    EditButton1: TEditButton;
    GrpParentGroupBox: TGroupBox;
    SatParentGroupBox: TGroupBox;
    SatTIPropertyGrid: TTIPropertyGrid;
    Splitter1: TSplitter;
    GrpTIPropertyGrid1: TTIPropertyGrid;
    procedure FormCreate(Sender: TObject);
  private
  public
    GrpEditButton1: TGroupedEditButton;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  GrpEditButton1:=TGroupedEditButton.Create(Self);
  GrpEditButton1.Name:='GrpEditButton1';
  GrpEditButton1.Caption:='';
  GrpEditButton1.Left:=10;
  GrpEditButton1.Top:=10;
  GrpEditButton1.Parent:=GrpParentGroupBox;

  GrpTIPropertyGrid1.TIObject:=GrpEditButton1;
  SatTIPropertyGrid.TIObject:=EditButton1;
end;

end.

