unit DesktopManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ButtonPanel,
  LazarusIDEStrConsts, LCLProc, EnvironmentOpts;

type

  { TDesktopForm }

  TDesktopForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    DesktopComboBox: TComboBox;
    SaveBitBtn: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure DesktopComboBoxSelect(Sender: TObject);
    procedure SaveBitBtnClick(Sender: TObject);
  private

  public

  end;

function ShowDesktopManagerDlg: TModalResult;

//var DesktopForm: TDesktopForm;

implementation

function ShowDesktopManagerDlg: TModalResult;
var
  theForm: TDesktopForm;
begin
  Result:=mrCancel;
  theForm:=TDesktopForm.Create(Nil);
  try
    Result:=theForm.ShowModal;
    if Result=mrYes then begin
      debugln(['ShowDesktopManagerDlg: Selecting ', theForm.DesktopComboBox.Text]);
      EnvironmentOptions.Desktops.Select(theForm.DesktopComboBox.Text);
    end;
  finally
    theForm.Free;
  end;
end;

{$R *.lfm}

{ TDesktopForm }

procedure TDesktopForm.FormCreate(Sender: TObject);
var
  i, ActiveInd: Integer;
begin
  // Saved desktops
  ActiveInd := 0;
  with EnvironmentOptions do
    for i:=0 to Desktops.Count-1 do
    begin
      if Desktops[i] = Desktop then
        ActiveInd := i;
      DesktopComboBox.Items.Add(Desktops[i].Name);
    end;
  DesktopComboBox.ItemIndex := ActiveInd;
  // Save button
  SaveBitBtn.Caption := dlgSaveCurrentDesktop;
  SaveBitBtn.LoadGlyphFromResourceName(HInstance, 'laz_save');
end;

procedure TDesktopForm.DesktopComboBoxSelect(Sender: TObject);
begin
  debugln(['TDesktopForm.DesktopComboBoxSelect: ', DesktopComboBox.Text]);
end;

procedure TDesktopForm.SaveBitBtnClick(Sender: TObject);
var
  dsk: TDesktopOpt;
begin
  with EnvironmentOptions do
  begin
    dsk := Desktops.Find(DesktopComboBox.Text);
    if not Assigned(dsk) then
    begin
      debugln(['TDesktopForm.SaveBitBtnClick: Adding ', DesktopComboBox.Text]);
      dsk := TDesktopOpt.Create(DesktopComboBox.Text);
      Desktops.Add(dsk);
    end;
    debugln(['TDesktopForm.SaveBitBtnClick: Assign from ', Desktop.Name, ' to ', dsk.Name]);
    dsk.Assign(Desktop);
    Desktops.Select(DesktopComboBox.Text);
  end;
end;

end.

