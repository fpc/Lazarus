unit charactermap_reg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, LCLType, Forms, SynEdit, Laz2_XMLCfg,
  MenuIntf, IDECommands, SrcEditorIntf, EnvironmentOpts,
  CharacterMapFrm;

type

  { TCharacterMapDialog }

  TCharacterMapDialog = class(TCharacterMapForm)
  private
    FXMLCfg: TXMLConfig;
    procedure InsertCharacter(const C: TUTF8Char);
    procedure CloseQueryHandler(Sender: TObject; var CanClose: Boolean);
    procedure LoadConfig;
    procedure SaveConfig;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

procedure Register;


implementation

var
  CharacterMapDialog: TCharacterMapDialog = nil;

procedure ShowCharacterMapProc({%H-}ASender: TObject);
begin
  if CharacterMapDialog = nil then
    Application.CreateForm(TCharacterMapDialog, CharacterMapDialog);
  CharacterMapDialog.LoadConfig;
  CharacterMapDialog.Show;
end;

{ TCharacterMapDialog }

const
  Path = 'CharacterMap/';

constructor TCharacterMapDialog.Create(AOwner: TComponent);
begin
  inherited;
  OnInsertCharacter := @InsertCharacter;
  OnCloseQuery := @CloseQueryHandler;
  FXMLCfg := TXMLConfig.Create(ExtractFilePath(EnvironmentOptions.FileName) + 'charactermap.xml');
end;

destructor TCharacterMapDialog.Destroy;
begin
  FXMLCfg.Free;
  inherited;
end;

procedure TCharacterMapDialog.CloseQueryHandler(Sender: TObject;
  var CanClose: Boolean);
begin
  if CanClose then
    SaveConfig;
end;

procedure TCharacterMapDialog.InsertCharacter(const C: TUTF8Char);
var
  FActiveEdit: TSourceEditorInterface;
begin
  FActiveEdit := SourceEditorManagerIntf.ActiveSourceWindow.ActiveEditor;
  if Assigned(FActiveEdit) then
  begin
    if FActiveEdit.ReadOnly then Exit;
    if (FActiveEdit.EditorControl is TSynEdit) then
      TSynEdit(FActiveEdit.EditorControl).InsertTextAtCaret(C);
  end;
end;

procedure TCharacterMapDialog.LoadConfig;
var
  L, T, W, H: Integer;
  R: TRect;
begin
  L := FXMLCfg.GetValue(Path + 'Position/Left', Left);
  T := FXMLCfg.GetValue(Path + 'Position/Top', Top);
  W := FXMLCfg.GetValue(Path + 'Position/Width', Width);
  H := FXMLCfg.GetValue(Path + 'Position/Height', Height);
  R := Screen.WorkAreaRect;
  if W > R.Width then W := R.Width;
  if H > R.Height then H := R.Height;
  if L < R.Left then L := R.Left;
  if T < R.Top then T := R.Top;
  if L + W > R.Right then L := R.Right - W - GetSystemMetrics(SM_CXSIZEFRAME);
  if T + H > R.Bottom then T := R.Bottom - H - GetSystemMetrics(SM_CYCAPTION) - GetSystemMetrics(SM_CYSIZEFRAME);
  SetBounds(L, T, W, H);
  WindowState := TWindowState(FXMLCfg.GetValue(Path + 'Position/WindowState', 0));
  FontSize := FXMLCfg.GetValue(Path + 'FontSize', 12);
  ActivePage := TCharMapPage(FXMLCfg.GetValue(Path + 'ActivePage', Integer(ActivePage)));
  AlphaSort := FXMLCfg.GetValue(Path + 'SortedUnicodeRangeList', AlphaSort);

  DropDownCount := EnvironmentOptions.DropDownCount;
end;

procedure TCharacterMapDialog.SaveConfig;
begin
  FXMLCfg.SetValue(Path + 'Position/Top', RestoredTop);
  FXMLCfg.SetValue(Path + 'Position/Left', RestoredLeft);
  FXMLCfg.SetValue(Path + 'Position/Width', RestoredWidth);
  FXMLCfg.SetValue(Path + 'Position/Height', RestoredHeight);
  FXMLCfg.SetValue(Path + 'Position/WindowState', Integer(WindowState));
  FXMLCfg.SetValue(Path + 'FontSize', FontSize);
  FXMLCfg.SetValue(Path + 'ActivePage', ord(ActivePage));
  FXMLCfg.SetValue(Path + 'SortedUnicodeRangeList', AlphaSort);

  EnvironmentOptions.DropDownCount := DropDownCount;
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

