unit IdeInspectKeyGrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LazUtils
  FileUtil,
  // LCL
  Forms, Controls, Graphics, Dialogs, ButtonPanel,
  // IdeIntf
  PropEdits;

type

  { TIdeInspectKeyGrabForm }

  TIdeInspectKeyGrabForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FKeyBox: TShortCutGrabBox;
  public
    property KeyBox: TShortCutGrabBox read FKeyBox;
  end;

implementation

{$R *.lfm}

{ TIdeInspectKeyGrabForm }

procedure TIdeInspectKeyGrabForm.FormCreate(Sender: TObject);
begin
  FKeyBox:=TShortCutGrabBox.Create(Self);
  with FKeyBox do begin
    Name:='FKeyBox';
    Align:=alClient;
    AutoSize:=true;
    BorderSpacing.Around:=6;
    Parent:=self;
  end;
  AutoSize := True;
end;

procedure TIdeInspectKeyGrabForm.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;

procedure TIdeInspectKeyGrabForm.CancelButtonClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.

