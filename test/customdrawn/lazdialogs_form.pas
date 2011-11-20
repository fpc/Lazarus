unit lazdialogs_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel;

type

  { TForm1 }

  TForm1 = class(TForm)
    buttonNativeOpen: TButton;
    buttonLazOpen: TButton;
    buttonNativeSave: TButton;
    buttonLazSave: TButton;
    buttonNativeSelectDir: TButton;
    buttonLazSelectDir: TButton;
    dialogNativeOpen: TOpenDialog;
    editInputInitialDir: TEdit;
    editOutputFileName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    dialogNativeSave: TSaveDialog;
    dialogNativeSelectDir: TSelectDirectoryDialog;
    procedure buttonLazSaveClick(Sender: TObject);
    procedure buttonNativeOpenClick(Sender: TObject);
    procedure buttonLazOpenClick(Sender: TObject);
    procedure buttonLazSelectDirClick(Sender: TObject);
    procedure buttonNativeSaveClick(Sender: TObject);
    procedure buttonNativeSelectDirClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses lazdialogs;

{$R *.lfm}

{ TForm1 }

procedure TForm1.buttonNativeOpenClick(Sender: TObject);
begin
  dialogNativeOpen.InitialDir := editInputInitialDir.Text;
  if dialogNativeOpen.Execute then
    editOutputFileName.Text := dialogNativeOpen.FileName;
end;

procedure TForm1.buttonLazSaveClick(Sender: TObject);
var
  lDialog: TLazSaveDialog;
begin
  lDialog := TLazSaveDialog.Create(nil);
  try
    lDialog.InitialDir := editInputInitialDir.Text;
    if lDialog.Execute then
      editOutputFileName.Text := lDialog.FileName;
  finally
    lDialog.Free;
  end;
end;

procedure TForm1.buttonLazOpenClick(Sender: TObject);
var
  lDialog: TLazOpenDialog;
begin
  lDialog := TLazOpenDialog.Create(nil);
  try
    lDialog.InitialDir := editInputInitialDir.Text;
    if lDialog.Execute then
      editOutputFileName.Text := lDialog.FileName;
  finally
    lDialog.Free;
  end;
end;

procedure TForm1.buttonLazSelectDirClick(Sender: TObject);
var
  lDialog: TLazSelectDirectoryDialog;
begin
  lDialog := TLazSelectDirectoryDialog.Create(nil);
  try
    lDialog.InitialDir := editInputInitialDir.Text;
    if lDialog.Execute then
      editOutputFileName.Text := lDialog.FileName;
  finally
    lDialog.Free;
  end;
end;

procedure TForm1.buttonNativeSaveClick(Sender: TObject);
begin
  dialogNativeSave.InitialDir := editInputInitialDir.Text;
  if dialogNativeSave.Execute then
    editOutputFileName.Text := dialogNativeSave.FileName;
end;

procedure TForm1.buttonNativeSelectDirClick(Sender: TObject);
begin
  dialogNativeSelectDir.InitialDir := editInputInitialDir.Text;
  if dialogNativeSelectDir.Execute then
    editOutputFileName.Text := dialogNativeSelectDir.FileName;
end;

end.

