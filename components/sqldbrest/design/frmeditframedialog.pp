unit frmeditframedialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, sqldbschemaedittools;

type
  { TEditFrameForm }

  TEditFrameForm = class(TForm)
    BPFrame: TButtonPanel;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
  private
    FFrame: TBaseEditFrame;
    procedure SetEditFrame(AValue: TBaseEditFrame);
  public
    Property EditFrame : TBaseEditFrame Read FFrame Write SetEditFrame;
  end;

var
  EditFrameForm: TEditFrameForm;

implementation

{$R *.lfm}

{ TEditFrameForm }

procedure TEditFrameForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose:=True;
  if (ModalResult=mrOK) then
    If EditFrame.Modified then
      EditFrame.SaveData;
end;

procedure TEditFrameForm.FormShow(Sender: TObject);
begin
  if Assigned(FFrame) then
    Caption:=SEdit+' '+FFrame.FrameCaption;
end;

procedure TEditFrameForm.SetEditFrame(AValue: TBaseEditFrame);
begin
  if FFrame=AValue then Exit;
  FFrame:=AValue;
  Self.ClientWidth:=FFrame.Width;
  Self.ClientHeight:=FFrame.Height+BPFrame.Height+1;
  FFrame.Parent:=Self;
  FFrame.Align:=alClient
end;

end.

