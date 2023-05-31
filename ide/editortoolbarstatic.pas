{
  Copyright (C) 2007 Graeme Geldenhuys (graemeg@gmail.com)
  Modified by Giuliano Colla and Juha Manninen

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}

unit EditorToolbarStatic;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, fgl,
  // LCL
  ComCtrls, Controls, LCLProc, Menus,
  // LazUtils
  LazConfigStorage, Laz2_XMLCfg,
  // IdeIntf
  BaseIDEIntf, IDEImagesIntf, SrcEditorIntf,
  // IdeConfig
  EditorToolBarOptions,
  // IDE
  LazarusIDEStrConsts, ToolbarConfig, EnvGuiOptions;

type

  TAllEditorToolbars = class;

  { TEditorToolbar }

  TEditorToolbar = class(TIDEToolbarBase)
  private
    FCollection: TAllEditorToolbars;
    FWindow: TSourceEditorWindowInterface;
    CfgItem: TMenuItem;
    procedure ClearToolbar;
  protected
    procedure PostCopyOptions; override;
  public
    constructor Create(AOwner: TComponent; ACollection: TAllEditorToolbars); overload;
    destructor Destroy; override;
    property OwnerWindow: TSourceEditorWindowInterface read FWindow;
  end;
  

  TEditorToolbarList = specialize TFPGList<TEditorToolbar>;

  { TAllEditorToolbars }

  TAllEditorToolbars = class
  private
    FToolBars: TEditorToolbarList;
    FConfigEvent: TNotifyEvent;
    procedure SourceWindowCreated(Sender: TObject);
    procedure SourceWindowDestroyed(Sender: TObject);
    procedure DoConfigureEditorToolbar(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReloadAll;
  end;

procedure CreateEditorToolBar(aConfigEvent: TNotifyEvent);

var
  uAllEditorToolbars: TAllEditorToolbars;

implementation

{ TEditorToolbar }

constructor TEditorToolbar.Create(AOwner: TComponent; ACollection: TAllEditorToolbars);
var
  xPM: TPopupMenu;
begin
  inherited Create(AOwner);
  Assert(not Assigned(FToolBar), 'TEditorToolbar.Create: FToolBar is assigned');
  FCollection := ACollection;
  FWindow := TSourceEditorWindowInterface(AOwner);

  // Toolbar must be created with Align = alTop, then initial positioning of buttons is correct.
  FToolBar := TToolbar.Create(FWindow);
  FToolBar.Parent   := FWindow;
  FToolBar.AutoSize := True;
  FToolBar.Align    := alTop;
  FToolBar.Flat     := True;
  FToolBar.Images   := IDEImages.Images_16;
  FToolBar.ShowHint := True;

  xPM := TPopupMenu.Create(FToolBar);
  xPM.Images := IDEImages.Images_16;
  CfgItem := TMenuItem.Create(xPM);
  xPM.Items.Add(CfgItem);
  CfgItem.Caption     := lisConfigureEditorToolbar;
  CfgItem.ImageIndex  := IDEImages.LoadImage('preferences');
  CfgItem.OnClick     := @FCollection.DoConfigureEditorToolbar;

  FToolBar.PopupMenu  := xPM;

  if FWindow.PixelsPerInch<>96 then
    FToolBar.AutoAdjustLayout(lapAutoAdjustForDPI, 96, FWindow.PixelsPerInch, 0, 0);
end;

destructor TEditorToolbar.Destroy;
begin
  uAllEditorToolbars.FToolBars.Remove(Self);
  inherited Destroy;
end;

procedure TEditorToolbar.PostCopyOptions;
begin
  case EnvironmentGuiOpts.Desktop.EditorToolBarOptions.Position of
    'Top': begin
      FToolBar.Align:= alTop;
      FToolBar.Height:= 26;
      end;
    'Bottom': begin
      FToolBar.Align:= alBottom;
      FToolBar.Height:= 26;
      end;
    'Left': begin
      FToolBar.Align:= alLeft;
      FToolBar.Width:= 26;
      end;
    'Right': begin
      FToolBar.Align:= alRight;
      FToolBar.Width:= 26;
      end;
  end;
end;

procedure TEditorToolbar.ClearToolbar;
var
  i: integer;
begin
  FToolBar.BeginUpdate;
  try
    for i := FToolBar.ButtonCount - 1 downto 0 do
      FToolBar.Buttons[i].Free
  finally
    FToolBar.EndUpdate;
  end;
end;

procedure CreateEditorToolBar(aConfigEvent: TNotifyEvent);
begin
  uAllEditorToolbars := TAllEditorToolbars.Create;
  uAllEditorToolbars.FConfigEvent := aConfigEvent;
end;

{ TAllEditorToolbars }

constructor TAllEditorToolbars.Create;
begin
  inherited;
  FToolBars := TEditorToolbarList.Create;
  if SourceEditorManagerIntf <> nil then
  begin
    SourceEditorManagerIntf.RegisterChangeEvent(semWindowCreate, @SourceWindowCreated);
    SourceEditorManagerIntf.RegisterChangeEvent(semWindowDestroy,@SourceWindowDestroyed);
  end;
end;

destructor TAllEditorToolbars.Destroy;
begin
  while FToolBars.Count > 0 do
    FToolBars[0].Free;
  FreeAndNil(FToolBars);
  inherited Destroy;
end;

procedure TAllEditorToolbars.SourceWindowCreated(Sender: TObject);
var
  ETB: TEditorToolbar;
  Opts: TEditorToolBarOptions;
begin
  ETB := TEditorToolbar.Create(Sender as TSourceEditorWindowInterface, Self);
  FToolBars.Add(ETB);
  Opts := EnvironmentGuiOpts.Desktop.EditorToolBarOptions;
  ETB.CopyFromOptions(Opts);
  ETB.FToolBar.Visible := Opts.Visible;
end;

procedure TAllEditorToolbars.SourceWindowDestroyed(Sender: TObject);
var
  i: integer;
  aBar: TEditorToolbar;
begin
  // Let's remove from our list the destroyed window and then destroy the ToolBar
  for i:= 0 to FToolBars.Count -1 do
  begin
    aBar := FToolBars[i];
    if aBar.OwnerWindow = TSourceEditorWindowInterface(Sender) then
    begin
      FToolBars.Remove(aBar);
      aBar.Free;
      exit;
    end;
  end;
end;

procedure TAllEditorToolbars.DoConfigureEditorToolbar(Sender: TObject);
begin
  if Assigned(FConfigEvent) then
    FConfigEvent(Sender);
end;

procedure TAllEditorToolbars.ReloadAll;
var
  aBar: TEditorToolbar;
  Opts: TEditorToolBarOptions;
  i: Integer;
begin
  for i := 0 to FToolBars.Count-1 do
  begin
    aBar := FToolBars[i];
    aBar.ClearToolbar;
    Opts := EnvironmentGuiOpts.Desktop.EditorToolBarOptions;
    aBar.CopyFromOptions(Opts);
    aBar.FToolBar.Visible := Opts.Visible;
  end;
end;


initialization
  ;

finalization
  uAllEditorToolbars.Free;

end.

