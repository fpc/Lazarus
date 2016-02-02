{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit sparta_FakeFormBackground;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  // BGRAButton,
  // BGRAImageButton,
  Forms, Controls, StdCtrls, ExtCtrls, Menus, sparta_DesignedForm,
  LCLType, LMessages, PropEdits, Graphics, sparta_InterfacesMDI;

type

  { TfrFakeFormBackground }

  TfrFakeFormBackground = class(TFrame, IDesignedFormBackground)
    bBackground: TButton;
    bTop: TButton;
    bFormCaption: TButton;
    bOther: TImage;
    bResize: TImage;
    bIcon: TImage;
    bSystem: TImage;
    bMaximalize: TImage;
    bMinimalize: TImage;
    bHelp: TImage;
    eFormCaption: TEdit;
    lRight: TLabel;
    miNone: TMenuItem;
    miSingle: TMenuItem;
    miSizeable: TMenuItem;
    miDialog: TMenuItem;
    miToolWindow: TMenuItem;
    miSizeToolWin: TMenuItem;
    miAddMinimize: TMenuItem;
    miAddSystemMenu: TMenuItem;
    miAddMaximize: TMenuItem;
    miAddHelp: TMenuItem;
    miLine: TMenuItem;
    miRemove: TMenuItem;
    pmFormStyle: TPopupMenu;
    pmBorderIcons: TPopupMenu;
    procedure bFormCaptionClick(Sender: TObject);
    procedure bIconClick(Sender: TObject);
    procedure bOtherClick(Sender: TObject);
    procedure bResizeClick(Sender: TObject);
    procedure eFormCaptionExit(Sender: TObject);
    procedure eFormCaptionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure miAddHelpClick(Sender: TObject);
    procedure miNoneClick(Sender: TObject);
    procedure miRemoveClick(Sender: TObject);
    procedure pmBorderIconsPopup(Sender: TObject);
  private
    FDesignedForm: IDesignedForm;
    FDesignedFakeForm: IDesignedFakeForm;

    function RootIsSelected: Boolean;

    function GetMargin(const AIndex: Integer): Integer;
    procedure OnUserInputHandler(Sender: TObject; Msg: Cardinal);
    procedure OnDesignRefreshPropertyValues;
  protected
    function GetParent: TWinControl; virtual;
    procedure SetParent(AParent: TWinControl); override;

    function GetDesignedForm: IDesignedForm;
    function GetResizeFrame: IResizeFrame;
    procedure SetResizeFrame(AValue: IResizeFrame);
  public
    { public declarations }
    constructor Create(const ADesignedForm: IDesignedForm; const ADesignedFakeForm: IDesignedFakeForm); virtual; reintroduce;
    destructor Destroy; override;

    procedure RefreshValues;

    procedure UpdateBorderIcons;
    procedure UpdateCaption;
  end;

implementation

{$R *.lfm}
{$R *.res}

uses
  sparta_MainIDE;

var
  Frames: TList;

{ TfrFakeFormBackground }

procedure TfrFakeFormBackground.miAddHelpClick(Sender: TObject);
begin
  if Sender = miAddHelp then
    FDesignedFakeForm.BorderIcons:=FDesignedFakeForm.BorderIcons + [biHelp]
  else if Sender = miAddMaximize then
    FDesignedFakeForm.BorderIcons:=FDesignedFakeForm.BorderIcons + [biMaximize]
  else if Sender = miAddMinimize then
    FDesignedFakeForm.BorderIcons:=FDesignedFakeForm.BorderIcons + [biMinimize]
  else if Sender = miAddSystemMenu then
    FDesignedFakeForm.BorderIcons:=FDesignedFakeForm.BorderIcons + [biSystemMenu];

  GlobalDesignHook.Modified(Self);
  if not RootIsSelected then
    RefreshValues
  else
    GlobalDesignHook.RefreshPropertyValues;
end;

procedure TfrFakeFormBackground.miNoneClick(Sender: TObject);
begin
  if Sender = miNone then
    FDesignedFakeForm.BorderStyle := bsNone
  else if Sender = miSingle then
    FDesignedFakeForm.BorderStyle := bsSingle
  else if Sender = miSizeable then
    FDesignedFakeForm.BorderStyle := bsSizeable
  else if Sender = miDialog then
    FDesignedFakeForm.BorderStyle := bsDialog
  else if Sender = miToolWindow then
    FDesignedFakeForm.BorderStyle := bsToolWindow
  else if Sender = miSizeToolWin then
    FDesignedFakeForm.BorderStyle := bsSizeToolWin
  ;

  GlobalDesignHook.Modified(Self);
  if not RootIsSelected then
    RefreshValues
  else
    GlobalDesignHook.RefreshPropertyValues;
end;

procedure TfrFakeFormBackground.miRemoveClick(Sender: TObject);
begin
  if pmBorderIcons.Tag = PtrInt(bHelp) then
    FDesignedFakeForm.BorderIcons:=FDesignedFakeForm.BorderIcons - [biHelp]
  else if pmBorderIcons.Tag = PtrInt(bMaximalize) then
    FDesignedFakeForm.BorderIcons:=FDesignedFakeForm.BorderIcons - [biMaximize]
  else if pmBorderIcons.Tag = PtrInt(bMinimalize) then
    FDesignedFakeForm.BorderIcons:=FDesignedFakeForm.BorderIcons - [biMinimize]
  else if pmBorderIcons.Tag = PtrInt(bSystem) then
    FDesignedFakeForm.BorderIcons:=FDesignedFakeForm.BorderIcons - [biSystemMenu];

  GlobalDesignHook.Modified(Self);
  if not RootIsSelected then
    RefreshValues
  else
    GlobalDesignHook.RefreshPropertyValues;
end;

procedure TfrFakeFormBackground.pmBorderIconsPopup(Sender: TObject);
begin
  miRemove.Visible := pmBorderIcons.Tag <> PtrInt(bOther);
  miLine.Visible := (pmBorderIcons.Tag <> PtrInt(bOther)) and (not bHelp.Visible or not bMinimalize.Visible or
    not bMaximalize.Visible or not bSystem.Visible);
  miAddHelp.Visible := not bHelp.Visible;
  miAddMinimize.Visible := not bMinimalize.Visible;
  miAddMaximize.Visible := not bMaximalize.Visible;
  miAddSystemMenu.Visible := not bSystem.Visible;
end;

function TfrFakeFormBackground.RootIsSelected: Boolean;
var
  LSelection: TPersistentSelectionList;
  i: integer;
begin
  Result := False;
  LSelection := TPersistentSelectionList.Create;
  GlobalDesignHook.GetSelection(LSelection);
  for i := 0 to LSelection.Count - 1 do
    if LSelection.Items[i] = FDesignedForm.Form then
    begin
      Result := True;
      Break;
    end;
  LSelection.Free;
end;

procedure TfrFakeFormBackground.bIconClick(Sender: TObject);
begin
  pmFormStyle.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
end;

procedure TfrFakeFormBackground.bFormCaptionClick(Sender: TObject);
begin
  eFormCaption.Visible := True;
  eFormCaption.SetFocus;
end;

procedure TfrFakeFormBackground.bOtherClick(Sender: TObject);
begin
  pmBorderIcons.Tag := PtrInt(Sender);
  pmBorderIcons.PopUp(Mouse.CursorPos.x, Mouse.CursorPos.y);
end;

procedure TfrFakeFormBackground.bResizeClick(Sender: TObject);
begin
  case FDesignedFakeForm.BorderStyle of
    bsSizeable: miNoneClick(miSingle);
    bsSingle: miNoneClick(miSizeable);
    bsToolWindow: miNoneClick(miSizeToolWin);
    bsSizeToolWin: miNoneClick(miToolWindow);
  end;
end;

procedure TfrFakeFormBackground.eFormCaptionExit(Sender: TObject);
begin
  bFormCaption.Caption := eFormCaption.Text;
  eFormCaption.Visible := False;
  FDesignedFakeForm.Caption := eFormCaption.Text;
  GlobalDesignHook.Modified(Self);
  GlobalDesignHook.RefreshPropertyValues;
end;

procedure TfrFakeFormBackground.eFormCaptionKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then
    eFormCaptionExit(eFormCaption);
end;

function TfrFakeFormBackground.GetMargin(const AIndex: Integer): Integer;
begin
  case AIndex of
    0: // left
      Result := 5;
    1: // Top
      Result := 30;
    2: // Right
      Result := 5;
    3: // Bottom
      Result := 5;
  end;
end;

procedure TfrFakeFormBackground.OnUserInputHandler(Sender: TObject; Msg: Cardinal);
var
  LCtrl: TControl;
  LIDE: IDesignedFormIDE;
begin
  LIDE := FDesignedForm as IDesignedFormIDE;
  if LIDE.LastActiveSourceWindow = nil then
    Exit;

  if FindModulePageControl(LIDE.LastActiveSourceWindow).PageIndex <> 1 then
    Exit;

  LCtrl := FindDragTarget(Mouse.CursorPos, True);
  if eFormCaption.Visible and (LCtrl <> eFormCaption)  then
    eFormCaptionExit(eFormCaption);
end;

procedure TfrFakeFormBackground.OnDesignRefreshPropertyValues;
begin
  if RootIsSelected then
    RefreshValues;
end;

function TfrFakeFormBackground.GetParent: TWinControl;
begin
  Result := inherited Parent;
end;

procedure TfrFakeFormBackground.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  UpdateBorderIcons;
  UpdateCaption;
end;

function TfrFakeFormBackground.GetDesignedForm: IDesignedForm;
begin
  Result := FDesignedForm as IDesignedForm;
end;

function TfrFakeFormBackground.GetResizeFrame: IResizeFrame;
begin
  Result := nil;
end;

procedure TfrFakeFormBackground.SetResizeFrame(AValue: IResizeFrame);
begin
end;

constructor TfrFakeFormBackground.Create(const ADesignedForm: IDesignedForm;
  const ADesignedFakeForm: IDesignedFakeForm);
begin
  inherited Create(nil);
  FDesignedForm := ADesignedForm;
  FDesignedFakeForm := ADesignedFakeForm;
  Frames.Add(Self);

  GlobalDesignHook.AddHandlerRefreshPropertyValues(@OnDesignRefreshPropertyValues);
  RefreshValues;
end;

destructor TfrFakeFormBackground.Destroy;
begin
  Pointer(FDesignedForm) := nil;
  Pointer(FDesignedFakeForm) := nil;
  Frames.Remove(Self);
  GlobalDesignHook.RemoveHandlerRefreshPropertyValues(@OnDesignRefreshPropertyValues);
  inherited Destroy;
end;

procedure TfrFakeFormBackground.RefreshValues;

  procedure SetBorderStyle({ABorderStyle: TBGRABorderStyle});
  begin
    {bBackground.BorderStyle.BottomLeft:=ABorderStyle;
    bBackground.BorderStyle.BottomRight:=ABorderStyle;
    bBackground.BorderStyle.TopLeft:=ABorderStyle;
    bBackground.BorderStyle.TopRight:=ABorderStyle; }
  end;

  procedure LoadPng(ABitmap: TCustomBitmap; AName: string);
  var
    LPng: TPortableNetworkGraphic;
  begin
    LPng := TPortableNetworkGraphic.Create;
    LPng.LoadFromResourceName(HINSTANCE, AName);
    ABitmap.Assign(LPng);
    LPng.Free;
  end;

  procedure SelectFormStyle(AMenuItem: TMenuItem);
  begin
    miNone.Checked:=False;
    miSingle.Checked:=False;
    miSizeable.Checked:=False;
    miDialog.Checked:=False;
    miToolWindow.Checked:=False;
    miSizeToolWin.Checked:=False;
    AMenuItem.Checked:=True;
  end;

begin
  UpdateBorderIcons;
  UpdateCaption;

  if FDesignedFakeForm.BorderStyle in [bsSizeable, bsSizeToolWin] then
    LoadPng(bResize.Picture.Bitmap, 'form_bg_resize')
  else
    LoadPng(bResize.Picture.Bitmap, 'form_bg_noresize');

  case FDesignedFakeForm.BorderStyle of
    bsToolWindow, bsSizeToolWin: SetBorderStyle({bsSquare});
    bsSingle, bsSizeable, bsNone: SetBorderStyle({bsRound});
    bsDialog: SetBorderStyle({bsBevel});
  end;

  case FDesignedFakeForm.BorderStyle of
    bsToolWindow: SelectFormStyle(miToolWindow);
    bsSizeToolWin: SelectFormStyle(miSizeToolWin);
    bsSingle: SelectFormStyle(miSingle);
    bsSizeable:SelectFormStyle(miSizeable);
    bsNone: SelectFormStyle(miNone);
    bsDialog: SelectFormStyle(miDialog);
  end;

  bTop.Visible := FDesignedFakeForm.BorderStyle = bsNone;
  bBackground.Visible := FDesignedFakeForm.BorderStyle <> bsNone;
end;

procedure TfrFakeFormBackground.UpdateBorderIcons;
begin
  if FDesignedFakeForm = nil then
    Exit;

  bOther.Visible := (FDesignedFakeForm.BorderIcons * [biSystemMenu, biMinimize, biMaximize, biHelp]) = [];
  bHelp.Visible := biHelp in FDesignedFakeForm.BorderIcons;
  bMinimalize.Visible := biMinimize in FDesignedFakeForm.BorderIcons;
  bMaximalize.Visible := biMaximize in FDesignedFakeForm.BorderIcons;
  bSystem.Visible := biSystemMenu in FDesignedFakeForm.BorderIcons;
end;

procedure TfrFakeFormBackground.UpdateCaption;
begin
  if FDesignedFakeForm = nil then
    Exit;

  bFormCaption.Caption := FDesignedFakeForm.Caption;
  eFormCaption.Caption := FDesignedFakeForm.Caption;
end;

type

  { OnUserInputHandler }

  TOnUserInputHandler = class
  public
    class procedure OnUserInputHandler(Sender: TObject; Msg: Cardinal);
  end;

{ OnUserInputHandler }

class procedure TOnUserInputHandler.OnUserInputHandler(Sender: TObject; Msg: Cardinal);
var
  p: pointer;
  frame: TfrFakeFormBackground absolute p;
begin
  case Msg of
    LM_LBUTTONDOWN, LM_RBUTTONDOWN, LM_MBUTTONDOWN, LM_XBUTTONDOWN:
      for p in Frames do
        if frame.Parent <> nil then // jesli robilismy popupparent framesy sie tworzyly i byl przypisywany zly caption
          frame.OnUserInputHandler(Sender, Msg);
  end;
end;

var
  OnUserInputHandler: TOnUserInputHandler;
initialization
  Frames := TList.Create;
  Application.AddOnUserInputHandler(@OnUserInputHandler.OnUserInputHandler);
finalization
  Application.RemoveOnUserInputHandler(@OnUserInputHandler.OnUserInputHandler);
  Frames.Free;
end.

