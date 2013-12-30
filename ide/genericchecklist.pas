{ ToDo: move to lazcontrols }
unit GenericCheckList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ButtonPanel,
  CheckLst, Buttons, StdCtrls;

type

  { TGenericCheckListForm }

  TGenericCheckListForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CheckListBox1: TCheckListBox;
    InfoLabel: TLabel;
    procedure CheckListBox1ItemClick(Sender: TObject; {%H-}Index: integer);
    procedure FormShow(Sender: TObject);
  private
    fActionBtn: TBitBtn;
    procedure UpdateButtons;
  public
    constructor Create(TheOwner: TComponent); override;
    constructor CreateWithActionButton(aCaption: TCaption; aResourceGlyphName: string = '');
    destructor Destroy; override;
  end;

var
  GenericCheckListForm: TGenericCheckListForm;

implementation

{$R *.lfm}

{ TGenericCheckListForm }

constructor TGenericCheckListForm.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  InfoLabel.Caption := '';
end;

constructor TGenericCheckListForm.CreateWithActionButton(aCaption: TCaption;
  aResourceGlyphName: string);
begin
  Create(Nil);
  fActionBtn := TBitBtn.Create(ButtonPanel1);
  fActionBtn.Caption := aCaption;
  fActionBtn.ModalResult := mrYes;             // ActionButton will return mrYes.
  fActionBtn.Align := alRight;
  fActionBtn.BorderSpacing.Left := 6;
  fActionBtn.BorderSpacing.Right := 6;
  if aResourceGlyphName <> '' then
    fActionBtn.LoadGlyphFromLazarusResource(aResourceGlyphName);
  fActionBtn.AutoSize := True;
  fActionBtn.Parent := ButtonPanel1;
end;

destructor TGenericCheckListForm.Destroy;
begin
  inherited Destroy;
end;

procedure TGenericCheckListForm.FormShow(Sender: TObject);
begin
  UpdateButtons;
end;

procedure TGenericCheckListForm.CheckListBox1ItemClick(Sender: TObject; Index: integer);
begin
  UpdateButtons;
end;

procedure TGenericCheckListForm.UpdateButtons;
var
  i: Integer;
begin
  if Assigned(fActionBtn) then
  begin
    for i := 0 to CheckListBox1.Count-1 do
      if CheckListBox1.Checked[i] then
      begin
        fActionBtn.Enabled := True;
        Exit;
      end;
    fActionBtn.Enabled := False;
  end;
end;

end.

