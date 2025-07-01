unit GenericCheckList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Controls, StdCtrls, Dialogs, Buttons, ButtonPanel, CheckLst, LCLType,
  // IdeIntf
  IDEImagesIntf;

type
  TGenericCheckListForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckListBox1: TCheckListBox;
    InfoLabel: TLabel;
    procedure CheckListBox1ItemClick(Sender: TObject; {%H-}Index: integer);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    fActionBtn: TBitBtn;
    fDisallowNoneSelected: Boolean;
    procedure UpdateButtons;
  public
    property DisallowNoneSelected: Boolean read fDisallowNoneSelected write fDisallowNoneSelected;
    constructor Create(TheOwner: TComponent); override;
    constructor CreateWithActionButton(aCaption: TCaption; aResourceGlyphName: string = '');
  end;

implementation

{$R *.lfm}

constructor TGenericCheckListForm.CreateWithActionButton(aCaption: TCaption;
  aResourceGlyphName: string);
begin
  Create(nil);
  fActionBtn := TBitBtn.Create(ButtonPanel1);
  fActionBtn.Caption := aCaption;
  fActionBtn.ModalResult := mrYes; // ActionButton will return mrYes.
  fActionBtn.Align := alRight;
  fActionBtn.BorderSpacing.Left := 6;
  fActionBtn.BorderSpacing.Right := 6;
  if aResourceGlyphName <> '' then
    IDEImages.AssignImage(fActionBtn, aResourceGlyphName);
  fActionBtn.AutoSize := True;
  fActionBtn.Parent := ButtonPanel1;
end;

procedure TGenericCheckListForm.FormShow(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TGenericCheckListForm.CheckListBox1ItemClick(Sender: TObject; Index: integer);
begin
  UpdateButtons;
end;

procedure TGenericCheckListForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
  begin
    if ButtonPanel1.OKButton.IsEnabled then
    begin
      Key := 0;
      ModalResult := mrOK;
    end;
  end;
end;

procedure TGenericCheckListForm.UpdateButtons;
var
  i: Integer;
begin
  if Assigned(fActionBtn) or DisallowNoneSelected then
  begin
    for i := 0 to CheckListBox1.Count-1 do
      if CheckListBox1.Checked[i] then
      begin
        if Assigned(fActionBtn) then fActionBtn.Enabled := True;
        ButtonPanel1.OKButton.Enabled := True;
        Exit;
      end;
    if Assigned(fActionBtn) then fActionBtn.Enabled := False;
    if DisallowNoneSelected then ButtonPanel1.OKButton.Enabled := False;
  end;
end;

constructor TGenericCheckListForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  fDisallowNoneSelected := False;
end;

end.

