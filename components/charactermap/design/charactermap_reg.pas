unit charactermap_reg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Forms, SynEdit,
  MenuIntf, IDECommands, IDEHelpIntf, SrcEditorIntf,
  CharacterMapFrm;

type

  { TCharacterMapDialog }

  TCharacterMapDialog = class(TCharacterMapForm)
  private
    procedure HelpButtonClick(Sender: TObject);
    procedure InsertCharacter(const C: TUTF8Char);
  protected
    procedure Activate; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;


implementation

var
  CharacterMapDialog: TCharacterMapDialog = nil;

procedure ShowCharacterMapProc(ASender: TObject);
begin
  if CharacterMapDialog = nil then
    Application.CreateForm(TCharacterMapDialog, CharacterMapDialog);
  CharacterMapDialog.Show;
end;

{ TCharacterMapDialog }

constructor TCharacterMapDialog.Create(AOwner: TComponent);
begin
  inherited;
  OnInsertCharacter := @InsertCharacter;
end;

procedure TCharacterMapDialog.Activate;
begin
  OnShowHelp := @HelpButtonClick;
end;

procedure TCharacterMapDialog.HelpButtonClick(Sender: TObject);
begin
  LazarusHelp.ShowHelpForIDEControl(Self);
end;

procedure TCharacterMapDialog.InsertCharacter(const C: TUTF8Char);
var
  FActiveEdit: TSourceEditorInterface;
  synEdit: TSynEdit;
begin
  FActiveEdit := SourceEditorManagerIntf.ActiveSourceWindow.ActiveEditor;
  if Assigned(FActiveEdit) then
  begin
    if FActiveEdit.ReadOnly then Exit;
    if (FActiveEdit.EditorControl is TSynEdit) then
      TSynEdit(FActiveEdit.EditorControl).InsertTextAtCaret(C);
  end;
end;


{ Registration }

procedure Register;
const
  cmdInsertCharName = 'cmdInsertCharacter';
  mnuInsertCharName = 'mnuInsertCharacter';
var
  shortcut: TIDEShortCut;
  category: TIDECommandCategory;
  cmdSP: TIDECommand;
begin
  shortCut := CleanIDEShortCut;

  category := IDECommandList.FindIDECommand(ecMultiPaste).Category;
  cmdSP := RegisterIDECommand(
    category,
    cmdInsertCharName,       // Name of command
    lisInsertCharacter,      // Description of command
    shortcut,                // Shortcut
    nil,                     // OnExecuteMethod
    @ShowCharacterMapProc    // OnExecuteProc
  );
  RegisterIDEMenuCommand(
    itmEditInsertions,       // Parent
    mnuInsertCharName,       // Name
    lisInsertCharacter,      // Caption
    nil,                     // OnClickMethod
    @ShowCharacterMapProc,   // OnClickProc
    cmdSP,                   // Command
    'menu_charmap'           // ResourceName of the menu icon
  );
end;

end.

