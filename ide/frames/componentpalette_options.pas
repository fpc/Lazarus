{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
}
unit componentpalette_options;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Graphics, Forms, StdCtrls, Dialogs,
  Buttons, ComCtrls, ExtCtrls, EnvironmentOpts, LazarusIDEStrConsts, IDEProcs,
  IDEOptionsIntf, ComponentReg, Controls, LCLProc, LCLType, PackageDefs;

type
  { TCompPaletteOptionsFrame }

  TCompPaletteOptionsFrame = class(TAbstractIDEOptionsEditor)
    AddPageButton: TBitBtn;
    ComponentsListView: TListView;
    CompMoveDownBtn: TSpeedButton;
    PageMoveDownBtn: TSpeedButton;
    CompMoveUpBtn: TSpeedButton;
    PageMoveUpBtn: TSpeedButton;
    PagesListBox: TListBox;
    ComponentsGroupBox: TGroupBox;
    PagesGroupBox: TGroupBox;
    RestoreButton: TBitBtn;
    Splitter1: TSplitter;
    procedure AddPageButtonClick(Sender: TObject);
    procedure ComponentsListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure ComponentsListViewClick(Sender: TObject);
    procedure ComponentsListViewCustomDraw(Sender: TCustomListView;
      const ARect: TRect; var DefaultDraw: Boolean);
    procedure ComponentsListViewCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ComponentsListViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure ComponentsListViewDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure ComponentsListViewItemChecked(Sender: TObject; Item: TListItem);
    procedure ComponentsListViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CompMoveDownBtnClick(Sender: TObject);
    procedure PageMoveDownBtnClick(Sender: TObject);
    procedure CompMoveUpBtnClick(Sender: TObject);
    procedure PageMoveUpBtnClick(Sender: TObject);
    procedure PagesListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure PagesListBoxDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure PagesListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure PagesListBoxSelectionChange(Sender: TObject; User: boolean);
    procedure RestoreButtonClick(Sender: TObject);
  private
    FLoaded: Boolean;
    procedure FillPages;
    procedure FillComponents(aPageName: string);
    procedure UpdateButtons;
    procedure UpdatePageMoveButtons(ListIndex: integer);
    procedure UpdateCompMoveButtons(ListIndex: integer);
  public
    function GetTitle: String; override;
    procedure Setup(ADialog: TAbstractOptionsEditorDialog); override;
    procedure ReadSettings(AOptions: TAbstractIDEOptions); override;
    procedure WriteSettings(AOptions: TAbstractIDEOptions); override;
    class function SupportedOptionsClass: TAbstractIDEOptionsClass; override;
  end;

implementation

{$R *.lfm}

const
  AllComponents = '<All>';

{ TCompPaletteOptionsFrame }

procedure TCompPaletteOptionsFrame.FillPages;
//Collect all available components (excluding hidden)
var
  APage: TBaseComponentPage;
  i: Integer;
begin
  if Assigned(IDEComponentPalette) then
  begin
    PagesListBox.Clear;
    PagesListBox.AddItem(AllComponents, Nil);
    for i := 0 to IDEComponentPalette.Count-1 do
    begin
      APage := IDEComponentPalette.Pages[i];
      if APage.Visible then
        PagesListBox.AddItem(APage.PageName, Nil);
    end;
    PagesListBox.ItemIndex := 0;     // Activate first
  end;
end;

procedure TCompPaletteOptionsFrame.FillComponents(aPageName: string);
var
  AComponent: TRegisteredComponent;
  APage: TBaseComponentPage;
  Item: TListItem;
  PageList: TStringList;
  PageCnt, CompCnt: Integer;
  RealPageName: String;
begin
  PageList := TStringList.Create;
  try
    if aPageName = AllComponents then
    begin                           // Skip the first entry for all components.
      for PageCnt := 1 to PagesListBox.Count-1 do
        PageList.Add(PagesListBox.Items[PageCnt]);
    end
    else
      PageList.Add(aPageName);
    ComponentsListView.Items.Clear;
    for PageCnt := 0 to PageList.Count-1 do
    begin
      RealPageName := PageList[PageCnt];
      APage := IDEComponentPalette.GetPage(RealPageName, False);
      if APage = Nil then Continue;
      for CompCnt := 0 to APage.Count-1 do
      begin
        AComponent := APage.Items[CompCnt];
        if AComponent.Visible and (AComponent.PageName<>'') then
        begin
          Item := ComponentsListView.Items.Add;
          Item.SubItems.Add(AComponent.ComponentClass.ClassName);
          Item.SubItems.Add(RealPageName);
          Item.SubItems.Add(AComponent.GetUnitName);
          Item.Data := AComponent;
        end;
      end;
    end;
  finally
    PageList.Free;
  end;
end;

function TCompPaletteOptionsFrame.GetTitle: String;
begin
  Result := lisMenuViewComponentPalette;
end;

procedure TCompPaletteOptionsFrame.Setup(ADialog: TAbstractOptionsEditorDialog);
begin
  PagesGroupBox.Caption := lisCmpPages;
  AddPageButton.Caption := lisBtnDlgAdd;
  RestoreButton.Caption := lisCmpRestoreDefaults;

  ComponentsGroupBox.Caption := lisCmpLstComponents;
  ComponentsListView.Column[1].Caption  := lisName;
  ComponentsListView.Column[2].Caption  := lisPage;
  ComponentsListView.Column[3].Caption  := lisPackage;
  // Arrow buttons for pages
  PageMoveUpBtn.LoadGlyphFromLazarusResource('arrow_up');
  PageMoveDownBtn.LoadGlyphFromLazarusResource('arrow_down');
  PageMoveUpBtn.Hint:=lisMoveSelectedUp;
  PageMoveDownBtn.Hint:=lisMoveSelectedDown;
  // Arrow buttons for components
  CompMoveUpBtn.LoadGlyphFromLazarusResource('arrow_up');
  CompMoveDownBtn.LoadGlyphFromLazarusResource('arrow_down');
  CompMoveUpBtn.Hint:=lisMoveSelectedUp;
  CompMoveDownBtn.Hint:=lisMoveSelectedDown;

  FillPages;
  UpdateButtons;
  UpdatePageMoveButtons(PagesListBox.ItemIndex);
  UpdateCompMoveButtons(ComponentsListView.ItemIndex);
  FLoaded := False;
end;

procedure TCompPaletteOptionsFrame.ReadSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    ;
  end;
  FLoaded := True;
end;

procedure TCompPaletteOptionsFrame.WriteSettings(AOptions: TAbstractIDEOptions);
begin
  with AOptions as TEnvironmentOptions do
  begin
    ;
  end;
end;

procedure TCompPaletteOptionsFrame.PagesListBoxSelectionChange(Sender: TObject; User: boolean);
var
  lb: TListBox;
begin
  //if not (FLoaded and User) then
  //  Exit;
  lb := Sender as TListBox;
  FillComponents(lb.Items[lb.ItemIndex]);
  UpdateButtons;
  UpdateCompMoveButtons(ComponentsListView.ItemIndex);
end;

procedure TCompPaletteOptionsFrame.AddPageButtonClick(Sender: TObject);
var
  s: String;
begin
  s := InputBox('New page', 'Page name', '');
  PagesListBox.AddItem(s, Nil);
end;

procedure TCompPaletteOptionsFrame.RestoreButtonClick(Sender: TObject);
begin
  ; // ToDo
end;

// Drag-drop PagesListBox

procedure TCompPaletteOptionsFrame.PagesListBoxDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  lb: TListBox;
  SrcView: TListView;
  DestPt: TPoint;
  Ind, DestInd: integer;
  Item: TListItem;
begin
  lb := Sender as TListBox;
  DestPt := Point(X, Y);
  DestInd := lb.ItemAtPos(DestPt, true);
  //SelfBox := Sender as TListBox;
  if Source is TListBox then
  begin
    Assert(Source = Sender, 'TCompPaletteOptionsFrame.PagesListBoxDragDrop: Source and Sender ListBoxes differ.');
    DebugLn(['TCompPaletteOptionsFrame.PagesListBoxDragDrop: DestInd=', DestInd,
             ', ItemIndex=', lb.ItemIndex]);
    if lb.ItemIndex < DestInd then
      Dec(DestInd);
    if (lb.ItemIndex > 0) and (DestInd > 0) and (lb.ItemIndex <> DestInd) then
    begin
      lb.Items.Move(lb.ItemIndex, DestInd);
      lb.ItemIndex := DestInd;
    end;
  end
  else if Source is TListView then
  begin
    SrcView := Source as TListView;
    Ind := 0;
    While Ind <= SrcView.Items.Count-1 do
    begin
      // Move possibly many selected items
      if SrcView.Items[Ind].Selected then
      begin
        Item := SrcView.Items[Ind];
        SrcView.Items.Delete(Ind);     // delete the item that is being dragged
        // ToDo: insert item into this page in a temporary data
        if Item=nil then ;
      end;
      inc(Ind);
    end;
  end;
end;

procedure TCompPaletteOptionsFrame.PagesListBoxDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  DestPt: TPoint;
  DestInd: integer;
  lb: TListBox;
begin
  lb := Sender as TListBox;
  DestPt := Point(X, Y);
  DestInd := lb.ItemAtPos(DestPt, true);
  Accept := ( (Source is TListBox) and (Source = Sender)
              and (lb.ItemIndex > 0) and (DestInd > 0)
            ) or (Source is TListView);
end;

// Drag-drop ComponentsListView

procedure TCompPaletteOptionsFrame.ComponentsListViewDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  lv: TListView;
  DestInd: Integer;
  SrcItem: TListItem;
begin
  lv := Sender as TListView;
  DestInd := lv.GetItemAt(X, Y).Index;
  SrcItem := lv.Selected;
  Assert(Source = Sender, 'TCompPaletteOptionsFrame.ComponentsListViewDragDrop: Source and Sender ListViews differ.');
  DebugLn(['TCompPaletteOptionsFrame.ComponentsListViewDragDrop: DestInd=', DestInd,
           ', ItemIndex=', SrcItem.Index]);
  if SrcItem.Index < DestInd then
    Dec(DestInd);
  if (SrcItem.Index > -1) and (DestInd > -1) and (SrcItem.Index <> DestInd) then
  begin
    lv.Selected := Nil;
    lv.items.Move(SrcItem.Index, DestInd);
    lv.Selected := lv.Items[DestInd];
    UpdateCompMoveButtons(DestInd);
  end;
end;

procedure TCompPaletteOptionsFrame.ComponentsListViewDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TListView) and (Source = Sender)
    and (PagesListBox.ItemIndex > 0);  // No dragging when <All> components is selected.
end;

procedure TCompPaletteOptionsFrame.ComponentsListViewChange(Sender: TObject;
  Item: TListItem; Change: TItemChange);
begin
  DebugLn(['TCompPaletteOptionsFrame.ComponentsListViewChange: Item=', Item.SubItems[0]]);
  UpdateCompMoveButtons(ComponentsListView.ItemIndex);
end;

procedure TCompPaletteOptionsFrame.ComponentsListViewClick(Sender: TObject);
begin
  DebugLn(['TCompPaletteOptionsFrame.ComponentsListViewClick: ']);
end;

procedure TCompPaletteOptionsFrame.ComponentsListViewItemChecked(Sender: TObject; Item: TListItem);
begin
  DebugLn(['TCompPaletteOptionsFrame.ComponentsListViewItemChecked: Item=', Item.SubItems[0]]);
end;

// Draw ComponentsListView

procedure TCompPaletteOptionsFrame.ComponentsListViewCustomDraw(Sender: TCustomListView;
  const ARect: TRect; var DefaultDraw: Boolean);
begin
  DebugLn(['TCompPaletteOptionsFrame.ComponentsListViewCustomDraw: DefaultDraw=', DefaultDraw]);
end;

procedure TCompPaletteOptionsFrame.ComponentsListViewCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  Comp: TRegisteredComponent;
  ARect: TRect;
  CurIcon: TCustomBitmap;
  IconWidth, IconHeight: Integer;
begin
  Comp := TRegisteredComponent(Item.Data);
  ARect := Item.DisplayRect(drIcon);
  //ARect.;
  CurIcon := nil;
  if Comp is TPkgComponent then
    CurIcon := TPkgComponent(Comp).Icon;
  if CurIcon<>nil then
  begin
    IconWidth := CurIcon.Width;
    IconHeight := CurIcon.Height;
    Sender.Canvas.Draw(ARect.Left+(25-IconWidth) div 2,
                       ARect.Top+(ARect.Bottom-ARect.Top-IconHeight) div 2, CurIcon);
    ARect.Left := ARect.Left + IconWidth + 2;
  end;
end;

// Page move up / down

procedure TCompPaletteOptionsFrame.PagesListBoxKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift ) and ((Key = VK_UP) or (Key = VK_DOWN)) then begin
    if Key = VK_UP then
      PageMoveUpBtnClick(nil)
    else
      PageMoveDownBtnClick(nil);
    Key:=VK_UNKNOWN;
  end;
end;

procedure TCompPaletteOptionsFrame.PageMoveUpBtnClick(Sender: TObject);
var
  i: Integer;
begin
  i := PagesListBox.ItemIndex;
  if i > 1 then
  begin
    PagesListBox.Items.Exchange(i, i-1);
    PagesListBox.ItemIndex := i-1;
    UpdatePageMoveButtons(i-1);
  end;
end;

procedure TCompPaletteOptionsFrame.PageMoveDownBtnClick(Sender: TObject);
var
  i: Integer;
begin
  i := PagesListBox.ItemIndex;
  if (i > 0) and (i < PagesListBox.Count-1) then
  begin
    PagesListBox.Items.Exchange(i, i+1);
    PagesListBox.ItemIndex := i+1;
    UpdatePageMoveButtons(i+1);
  end;
end;

// Component move up / down

procedure TCompPaletteOptionsFrame.ComponentsListViewKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (ssCtrl in Shift ) and ((Key = VK_UP) or (Key = VK_DOWN)) then begin
    if Key = VK_UP then
      CompMoveUpBtnClick(nil)
    else
      CompMoveDownBtnClick(nil);
    Key:=VK_UNKNOWN;
  end;
end;

procedure TCompPaletteOptionsFrame.CompMoveUpBtnClick(Sender: TObject);
var
  i: Integer;
begin
  i := ComponentsListView.ItemIndex;
  if i > 0 then
  begin
    ComponentsListView.Selected := Nil;
    ComponentsListView.Items.Exchange(i, i-1);
    ComponentsListView.Selected := ComponentsListView.Items[i-1];
    UpdateCompMoveButtons(i-1);
  end;
end;

procedure TCompPaletteOptionsFrame.CompMoveDownBtnClick(Sender: TObject);
var
  i: Integer;
begin
  i := ComponentsListView.ItemIndex;
  if (i > -1) and (i < ComponentsListView.Items.Count-1) then
  begin
    ComponentsListView.Selected := Nil;
    ComponentsListView.Items.Exchange(i, i+1);
    ComponentsListView.Selected := ComponentsListView.Items[i+1];
    UpdateCompMoveButtons(i+1);
  end;
end;

///

procedure TCompPaletteOptionsFrame.UpdateButtons;
begin
  RestoreButton.Visible := PagesListBox.ItemIndex = 0;
end;

procedure TCompPaletteOptionsFrame.UpdatePageMoveButtons(ListIndex: integer);
begin
  DebugLn(['TCompPaletteOptionsFrame.UpdatePageMoveButtons: Page index=', ListIndex]);
  if (ListIndex > 0) and (ListIndex < PagesListBox.Items.Count) then
  begin
    PageMoveUpBtn.Enabled := ListIndex > 1;
    PageMoveDownBtn.Enabled := ListIndex < PagesListBox.Items.Count-1;
  end
  else begin
    PageMoveUpBtn.Enabled := False;
    PageMoveDownBtn.Enabled := False;
  end;
end;

procedure TCompPaletteOptionsFrame.UpdateCompMoveButtons(ListIndex: integer);
begin
  DebugLn(['TCompPaletteOptionsFrame.UpdateCompMoveButtons: Component index=', ListIndex]);
  if (ListIndex > -1) and (ListIndex < ComponentsListView.Items.Count)
  and (PagesListBox.ItemIndex > 0) then  // No moving when <All> components is selected.
  begin
    CompMoveUpBtn.Enabled := ListIndex > 0;
    CompMoveDownBtn.Enabled := ListIndex < ComponentsListView.Items.Count-1;
  end
  else begin
    CompMoveUpBtn.Enabled := False;
    CompMoveDownBtn.Enabled := False;
  end;
end;

class function TCompPaletteOptionsFrame.SupportedOptionsClass: TAbstractIDEOptionsClass;
begin
  Result := TEnvironmentOptions;
end;

{$IFDEF EnableComponentPaletteOptions}
initialization
  RegisterIDEOptionsEditor(GroupEnvironment, TCompPaletteOptionsFrame, EnvOptionsCompPalette);
{$ENDIF}

end.

