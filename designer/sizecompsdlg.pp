{
  Author: Mattias Gaertner

  Abstract:
    Defines TSizeComponentsDialog.
}
unit SizeCompsDlg;

{$mode objfpc}{$H+}

interface

uses
  Classes, LCLLinux, Forms, Controls, Buttons, ExtCtrls, StdCtrls, LResources;

type
  TSizeComponentsDialog = class(TForm)
    WidthRadioGroup: TRadioGroup;
    WidthEdit: TEdit;
    HeightRadioGroup: TRadioGroup;
    HeightEdit: TEdit;
    OkButton: TButton;
    CancelButton: TButton;
    procedure OkButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);  override;
  end;

var SizeComponentsDialog: TSizeComponentsDialog;

function ShowSizeComponentsDialog: TModalResult;

implementation

function ShowSizeComponentsDialog: TModalResult;
begin
  if SizeComponentsDialog=nil then
    SizeComponentsDialog:=TSizeComponentsDialog.Create(Application);
  with SizeComponentsDialog do begin
    SetBounds((Screen.Width-365) div 2,(Screen.Height-175) div 2,355,165);
    WidthRadioGroup.ItemIndex:=0;
    HeightRadioGroup.ItemIndex:=0;
    Result:=ShowModal;
  end;
end;


{ TSizeComponentsDialog }

constructor TSizeComponentsDialog.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if LazarusResources.Find(Classname)=nil then begin
    SetBounds((Screen.Width-365) div 2,(Screen.Height-175) div 2,355,165);
    Caption:='Size';

    WidthRadioGroup:=TRadioGroup.Create(Self);
    with WidthRadioGroup do begin
      Name:='WidthRadioGroup';
      Parent:=Self;
      Left:=5;
      Top:=5;
      Width:=170;
      Height:=115;
      Caption:='Width:';
      with Items do begin
        BeginUpdate;
        Add('No change');
        Add('Shrink to smallest');
        Add('Grow to Largest');
        Add('Width:');
        EndUpdate;
      end;
      Show;
    end;

    WidthEdit:=TEdit.Create(Self);
    with WidthEdit do begin
      Name:='WidthEdit';
      Parent:=Self;
      Left:=87;
      Top:=90;
      Width:=60;
      Text:='';
      Show;
    end;

    HeightRadioGroup:=TRadioGroup.Create(Self);
    with HeightRadioGroup do begin
      Name:='HeightRadioGroup';
      Parent:=Self;
      Left:=180;
      Top:=5;
      Width:=170;
      Height:=115;
      Caption:='Height:';
      with Items do begin
        BeginUpdate;
        Add('No change');
        Add('Shrink to smallest');
        Add('Grow to Largest');
        Add('Height:');
        EndUpdate;
      end;
      Show;
    end;

    HeightEdit:=TEdit.Create(Self);
    with HeightEdit do begin
      Name:='HeightEdit';
      Parent:=Self;
      Left:=262;
      Top:=90;
      Width:=60;
      Text:='';
      Show;
    end;

    OkButton:=TButton.Create(Self);
    with OkButton do begin
      Name:='OkButton';
      Parent:=Self;
      Left:=145;
      Top:=WidthRadioGroup.Top+WidthRadioGroup.Height+10;
      Width:=75;
      Height:=25;
      Caption:='Ok';
      OnClick:=@OkButtonClick;
      Show;
    end;

    CancelButton:=TButton.Create(Self);
    with CancelButton do begin
      Name:='CancelButton';
      Parent:=Self;
      Left:=235;
      Top:=OkButton.Top;
      Width:=75;
      Height:=25;
      Caption:='Cancel';
      OnClick:=@CancelButtonClick;
      Show;
    end;
  end;
end;

procedure TSizeComponentsDialog.OkButtonClick(Sender: TObject);
begin
  ModalResult:=mrOk;
end;

procedure TSizeComponentsDialog.CancelButtonClick(Sender: TObject);
begin
  ModalResult:=mrCancel;
end;

initialization
  SizeComponentsDialog:=nil;

end.
