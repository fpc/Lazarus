{
 *****************************************************************************
  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************

  Author: Maciej Izak

  DaThoX 2004-2015
  FreeSparta.com
}

unit dtx_sparta_BasicRegister;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, SrcEditorIntf, LazIDEIntf, ComCtrls, Controls, Forms, IDEImagesIntf,
  Buttons, ExtCtrls, Graphics, IDEWindowIntf, sparta_MainIDE,
  PropEdits, PropEditUtils, FormEditingIntf, ComponentEditors, EditBtn, TypInfo,
  LCLIntf, LCLType, sparta_FakeForm, sparta_FakeNonControl, sparta_FakeFrame, sparta_ComponentPalette;

procedure Register;

implementation

procedure CreateSpartaPalette;
var
  tbStandard: TToolBar;
  pnlToolButtons, pnlSpeedButtons: TPanel;
  PalettePageSelectBtn, LayerSelectBtn: TSpeedButton;
begin
  exit; // TODO
  tbStandard := LazarusIDE.OwningComponent.FindComponent('tbStandard') as TToolBar;

  if IDEDockMaster <> nil then
  begin
    with TToolButton.Create(LazarusIDE.OwningComponent) do
    begin
      Style := tbsDivider;
      AutoSize := True;
      Parent := tbStandard;
    end;

    with TToolButton.Create(LazarusIDE.OwningComponent) do
    begin
      Name := 'HideComponentPageControlButton';
      Parent := tbStandard;
      Enabled := True;
      OnClick := spartaIDE.mnuHideHideComponentPageControlClicked;

      ImageIndex := IDEImages.LoadImage(16, 'SHOW_PALETTE_DOWN');
      Hint := 'Show components';
      ShowHint := true;
    end;
  end;

  pnlToolButtons := LazarusIDE.OwningComponent.FindComponent('pnlToolButtons') as TPanel;
  MainComponentsPalette := TComponentsPalette.Create(LazarusIDE.OwningComponent, pnlToolButtons, True);

  pnlSpeedButtons := LazarusIDE.OwningComponent.FindComponent('pnlSpeedButtons') as TPanel;
  pnlSpeedButtons.Width := 107;

  PalettePageSelectBtn := LazarusIDE.OwningComponent.FindComponent('PalettePageSelectBtn') as TSpeedButton;
  LayerSelectBtn := LazarusIDE.OwningComponent.FindComponent('LayerSelectBtn') as TSpeedButton;

  with LayerSelectBtn do
  begin
    AnchorSideRight.Control := pnlSpeedButtons;
    AnchorSideRight.Side := asrBottom;
    AnchorSideBottom.Control := pnlSpeedButtons;
    AnchorSideBottom.Side := asrBottom;
    Anchors := [akRight, akBottom];
  end;
  with PalettePageSelectBtn do
  begin
    AnchorSideLeft.Control := pnlSpeedButtons;
    AnchorSideRight.Side := asrBottom;
    AnchorSideBottom.Control := pnlSpeedButtons;
    AnchorSideBottom.Side := asrBottom;
    Anchors := [akLeft, akBottom];
  end;

  MainComponentsPaletteFilter := TEditButton.Create(LazarusIDE.OwningComponent);
  with MainComponentsPaletteFilter do
  begin
    Parent := pnlSpeedButtons;
    Name := 'eFilter';
    AnchorSideLeft.Control := pnlSpeedButtons;
    AnchorSideTop.Control := pnlSpeedButtons;
    AnchorSideTop.Side := asrTop;
    Left := 3;
    Height := 23;
    Top := 0;

    AnchorSideRight.Control := pnlSpeedButtons;
    AnchorSideRight.Side := asrBottom;
    Anchors := [akTop, akLeft, akRight];

    BorderSpacing.Left := 3;
    Button.LoadGlyphFromResourceName(HINSTANCE, 'MENU_CLOSE');
    OnButtonClick:=spartaIDE.eFilterClear;
    OnChange := spartaIDE.eFilterChange;
  end
end;

procedure Register;
begin
  FormEditingHook.StandardDesignerBaseClasses[DesignerBaseClassId_TForm] := TFakeForm;
  FormEditingHook.NonFormProxyDesignerForm[NonControlProxyDesignerFormId] := TFakeNonControl;
  FormEditingHook.NonFormProxyDesignerForm[FrameProxyDesignerFormId] := TFakeFrame;

  Screen.AddHandlerFormAdded(spartaIDE.Screen_FormAdded);
  Screen.AddHandlerRemoveForm(spartaIDE.Screen_FormDel);
{$IFDEF USE_POPUP_PARENT_DESIGNER}
  TCustomForm(LazarusIDE.GetMainBar).AddHandlerOnBeforeDestruction(spartaIDE.OnBeforeClose);
{$ENDIF}
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowCreate, spartaIDE.WindowCreate);
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowDestroy, spartaIDE.WindowDestroy);
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowShow, spartaIDE.WindowShow);
  SourceEditorManagerIntf.RegisterChangeEvent(semWindowHide, spartaIDE.WindowHide);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorActivate, spartaIDE.EditorActivated);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorDestroy, spartaIDE.EditorDestroyed);
  SourceEditorManagerIntf.RegisterChangeEvent(semEditorCreate, spartaIDE.EditorCreate);

  LazarusIDE.AddHandlerOnShowDesignerFormOfSource(spartaIDE.OnShowDesignerForm);
  LazarusIDE.AddHandlerOnShowSourceOfActiveDesignerForm(spartaIDE.OnShowSrcEditor);

  GlobalDesignHook.AddHandlerShowMethod(spartaIDE.OnShowMethod);
  GlobalDesignHook.AddHandlerRefreshPropertyValues(spartaIDE.OnDesignRefreshPropertyValues);


  IDETabMaster := TDTXTabMaster.Create;
  IDEComponentsMaster := TDTXComponentsMaster.Create;

  CreateSpartaPalette;
end;

finalization
  Screen.RemoveHandlerFormAdded(spartaIDE.Screen_FormAdded);
  Screen.RemoveHandlerRemoveForm(spartaIDE.Screen_FormDel);

  IDETabMaster.Free;
  IDEComponentsMaster.Free;
end.

