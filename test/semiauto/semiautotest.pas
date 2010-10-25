unit semiautotest;

{$mode objfpc}{$H+}

interface

uses
  // RTL, FCL
  Classes, SysUtils, fpcunit,
  // LCL
  Interfaces, Forms, LCLType, StdCtrls, Controls,
  // runtests
  testglobals;

type

  { TSemiAutomaticTest }

  TSemiAutomaticTest = class(TTestCase)
  protected
    FDialog: TForm;
    FInstructions: TLabel;
    FPass: TButton;
    FFail: TButton;
  public
    constructor Create; override;
    // Simple Yes-No dialog
    function ShowResultDialog(const ATitle, AInstructions: string): Boolean;
    // A dialog for testing the Canvas drawing
    function GetCanvasDialog(var ADelta: TPoint): TForm; // ADelta is the position where the drawing should start
    function ShowCanvasDialog(const ATitle, AInstructions: string): Boolean;
  end;

implementation

{ TSemiAutomaticTest }

constructor TSemiAutomaticTest.Create;
begin
  inherited Create;

  FDialog := TForm.Create(Application);
  FDialog.Width := 240;
  FDialog.Height := 240;
  FDialog.Position := poScreenCenter;

  FInstructions := TLabel.Create(FDialog);
  FInstructions.Parent := FDialog;
  FInstructions.Top := 10;
  FInstructions.Left := 10;
  FInstructions.AutoSize := False;
  FInstructions.Width := FDialog.Width - 10;
  FInstructions.WordWrap := True;
  FInstructions.Height := 200;

  FPass := TButton.Create(FDialog);
  FPass.Parent := FDialog;
  FPass.Caption := 'PASS';
  FPass.AutoSize := True;
  FPass.Top := FDialog.Height - 35;
  FPass.Left := 50;
  FPass.ModalResult := mrYes;

  FFail := TButton.Create(FDialog);
  FFail.Parent := FDialog;
  FFail.Caption := 'FAIL';
  FFail.AutoSize := True;
  FFail.Top := FDialog.Height - 35;
  FFail.Left := 150;
  FFail.ModalResult := mrNo;
end;

function TSemiAutomaticTest.ShowResultDialog(const ATitle, AInstructions: string
  ): Boolean;
begin
  Result := Application.MessageBox(PChar(AInstructions), PChar(ATitle), MB_YESNO) = IDYES;
end;

function TSemiAutomaticTest.GetCanvasDialog(var ADelta: TPoint): TForm;
begin
  Result := FDialog;
  ADelta.X := 50;
  ADelta.Y := 50;
end;

function TSemiAutomaticTest.ShowCanvasDialog(const ATitle, AInstructions: string
  ): Boolean;
begin
  FInstructions.Caption := AInstructions;
  FDialog.Caption := ATitle;

  Result := FDialog.ShowModal() = mrYes;
end;

end.

