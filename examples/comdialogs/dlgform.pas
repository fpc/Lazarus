unit DlgForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type
  
  { TForm1 }

  TForm1 = class(TForm)
    dirButton: TButton;
    colorButton: TButton;
    fontButton: TButton;
    saveButton: TButton;
    openButton: TButton;
    closeButton: TButton;
    dirLabel: TLabel;
    fileLabel: TLabel;
    procedure ButtonClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonClick(Sender: TObject);
begin
  case TButton(Sender).Tag of
    1 : Close;
    2 : with TOpenDialog.Create(Self) do
        begin
          Filter := '*.pp';
          Options := Options + [ofAllowMultiSelect];
          if Execute then fileLabel.Caption := FileName;
          Free;
        end;
    3 : with TSaveDialog.Create(Self) do
        begin
          Filename := 'untitled.pp';
          if Execute then fileLabel.Caption := FileName;
          Free;
        end;
    4 : with TFontDialog.Create(Self) do
        begin
          Font.Assign(fontButton.Font);
          if Execute then fontButton.Font.Assign(Font);
          Free;
        end;
    5 : with TColorDialog.Create(Self) do
        begin
          Color := Self.Color;
          if Execute then Self.Color := Color;
          Free;
        end;
    6 : with TSelectDirectoryDialog.Create(Self) do
        begin
          if Execute then dirLabel.Caption := FileName;
          Free;
        end;
  end;
end;

end.

