{ $Id$ }
{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

 ******************************************************************************
 Implementation
 ******************************************************************************

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.LCL, included in this distribution,                 *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}

//##apiwiz##sps##   // Do not remove

procedure TWin32WidgetSet.DrawArrow(Arrow: TComponent; Canvas: TPersistent);
begin
  Windows.DrawFrameControl(TCanvas(Canvas).Handle, 
{$ifdef VER1_0}
  	Windows.RECT(TControl(Arrow).ClientRect),
{$else}  
  	TControl(Arrow).ClientRect,
{$endif}  
	DFC_MENU, DFCS_MENUARROW);
end;

{------------------------------------------------------------------------------
  Function: AlignmentCreateHandle
  Params: AAlignment:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.AlignmentCreateHandle(const AAlignment: TObject; const AParams: TCreateParams): THandle;
begin         
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ArrowCreateHandle
  Params: AArrow:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ArrowCreateHandle(const AArrow: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: BitBtnCreateHandle
  Params: ABitBtn:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.BitBtnCreateHandle(const ABitBtn: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ButtonCreateHandle
  Params: AButton:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ButtonCreateHandle(const AButton: TObject; const AParams: TCreateParams): THandle;
begin
  Result := CreateComponent(AButton);
end;

{------------------------------------------------------------------------------
  Function: CalendarCreateHandle
  Params: ACalendar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CalendarCreateHandle(const ACalendar: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: CheckboxCreateHandle
  Params: ACheckBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CheckboxCreateHandle(const ACheckBox: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: CheckListBoxCreateHandle
  Params: ACheckListBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CheckListBoxCreateHandle(const ACheckListBox: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: CListBoxCreateHandle
  Params: ACListBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.CListBoxCreateHandle(const ACListBox: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ColorDialogCreateHandle
  Params: AColorDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ColorDialogCreateHandle(const AColorDialog: TObject): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ComboBoxCreateHandle
  Params: AComboBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ComboBoxCreateHandle(const AComboBox: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: EditCreateHandle
  Params: AEdit:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.EditCreateHandle(const AEdit: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: FileDialogCreateHandle
  Params: AFileDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.FileDialogCreateHandle(const AFileDialog: TObject): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: FontDialogCreateHandle
  Params: AFontDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.FontDialogCreateHandle(const AFontDialog: TObject): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: FormCreateHandle
  Params: AForm:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.FormCreateHandle(const AForm: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: GetAcceleratorString
  Params: AVKey:
          AShiftState:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String;
begin
  //TODO: Implement
  Result := '';
end;

{------------------------------------------------------------------------------
  Function: GetControlConstraints
  Params: Constraints: TObject
  Returns: true on success

  Updates the constraints object (e.g. TSizeConstraints) with interface specific
  bounds.
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetControlConstraints(Constraints: TObject): boolean;
var
  SizeConstraints: TSizeConstraints;
  SizeRect: TRect;
  Height: Integer;
begin
  Result:=true;
  if Constraints is TSizeConstraints then begin
    SizeConstraints:=TSizeConstraints(Constraints);

    if (SizeConstraints.Control=nil) then exit;

    if (SizeConstraints.Control is TComboBox)
      and TWinControl(SizeConstraints.Control).HandleAllocated then begin
      // the height of a combobox is fixed
      Windows.GetWindowRect(TWinControl(SizeConstraints.Control).Handle, @SizeRect);
      Height := SizeRect.Bottom - SizeRect.Top;
      SizeConstraints.SetInterfaceConstraints(0, Height, 0, Height);
    end;

  end;
end;

{------------------------------------------------------------------------------
  Function: GetListBoxIndexAtY
  Params: ListBox:
          y:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer;
begin
  Result := -1;
  if ListBox is TCustomListBox then begin
    Result := Windows.SendMessage(TCustomListBox(ListBox).Handle, LB_ITEMFROMPOINT, 0, MakeLParam(0,y));
    if hi(Result)=0 then
      Result := lo(Result)
    else Result := -1;
  end;
end;

{------------------------------------------------------------------------------
  Function: GroupBoxCreateHandle
  Params: AGroupBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.GroupBoxCreateHandle(const AGroupBox: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: HintWindowCreateHandle
  Params: AHintWindow:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.HintWindowCreateHandle(const AHintWindow: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ImageCreateHandle
  Params: AImage:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ImageCreateHandle(const AImage: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: IntfCreateHandle
  Params: AObject:
          AParams:
  Returns:

  This is a temporary routine until all controls are created directly
 ------------------------------------------------------------------------------}
function TWin32WidgetSet.IntfCreateHandle(const AObject: TObject; const AParams: TCreateParams): THandle;
begin
  Result := CreateComponent(AObject);
end;

{------------------------------------------------------------------------------
  Function: LabelCreateHandle
  Params: ALabel:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.LabelCreateHandle(const ALabel: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ListBoxCreateHandle
  Params: AListBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ListBoxCreateHandle(const AListBox: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ListViewCreateHandle
  Params: AListView:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ListViewCreateHandle(const AListView: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: MainMenuCreateHandle
  Params: AMainMenu:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.MainMenuCreateHandle(const AMainMenu: TObject): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: MemoCreateHandle
  Params: AMemo:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.MemoCreateHandle(const AMemo: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: MenuItemCreateHandle
  Params: AMenuItem:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.MenuItemCreateHandle(const AMenuItem: TObject): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: NotebookCreateHandle
  Params: ANotebook:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.NotebookCreateHandle(const ANotebook: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: OpenFileDialogCreateHandle
  Params: AOpenFileDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.OpenFileDialogCreateHandle(const AOpenFileDialog: TObject): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: PageCreateHandle
  Params: APage:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.PageCreateHandle(const APage: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: PairSplitterCreateHandle
  Params: APairSplitter:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.PairSplitterCreateHandle(const APairSplitter: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: PairSplitterSideCreateHandle
  Params: APairSplitterSide:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.PairSplitterSideCreateHandle(const APairSplitterSide: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: PanelCreateHandle
  Params: APanel:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.PanelCreateHandle(const APanel: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: PopupMenuCreateHandle
  Params: APopupMenu:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.PopupMenuCreateHandle(const APopupMenu: TObject): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: PreviewFileControlCreateHandle
  Params: APreviewFileControl:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.PreviewFileControlCreateHandle(const APreviewFileControl: TObject): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: PreviewFileDialogCreateHandle
  Params: APreviewFileDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.PreviewFileDialogCreateHandle(const APreviewFileDialog: TObject): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ProgressBarCreateHandle
  Params: AProgressBar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ProgressBarCreateHandle(const AProgressBar: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: RadioButtonCreateHandle
  Params: ARadioButton:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.RadioButtonCreateHandle(const ARadioButton: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: SaveFileDialogCreateHandle
  Params: ASaveFileDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SaveFileDialogCreateHandle(const ASaveFileDialog: TObject): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ScrollBarCreateHandle
  Params: AScrollBar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ScrollBarCreateHandle(const AScrollBar: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ScrollBoxCreateHandle
  Params: AScrollBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ScrollBoxCreateHandle(const AScrollBox: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ScrolledWindowCreateHandle
  Params: AScrolledWindow:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ScrolledWindowCreateHandle(const AScrolledWindow: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: SelectDirectoryDialogCreateHandle
  Params: ASelectDirectoryDialog:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SelectDirectoryDialogCreateHandle(const ASelectDirectoryDialog: TObject): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: SpeedButtonCreateHandle
  Params: ASpeedButton:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SpeedButtonCreateHandle(const ASpeedButton: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: SpineditCreateHandle
  Params: ASpinEdit:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.SpineditCreateHandle(const ASpinEdit: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: StatusBarCreateHandle
  Params: AStatusBar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.StatusBarCreateHandle(const AStatusBar: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: MenuItemSetCheck
  Params:  BaseMenuItem
  Returns: Nothing

  Checks or unchecks the specified menu item.
 ------------------------------------------------------------------------------}
Function TWin32WidgetSet.MenuItemSetCheck(BaseMenuItem: TComponent): Boolean;
var
  CheckFlag: Integer;
  AMenuItem: TMenuItem;
Begin
  AMenuItem:=BaseMenuItem as TMenuItem;
  if AMenuItem.Checked then CheckFlag := MF_CHECKED
  else CheckFlag := MF_UNCHECKED;
  CheckFlag := CheckFlag or MF_BYCOMMAND;
  Result := Windows.CheckMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command, CheckFlag) <> DWORD($FFFFFFFF);
End;

{------------------------------------------------------------------------------
  Function: MenuItemSetEnable
  Params: BaseMenuItem:
  Returns:

  Enables, disables, or grays the specified menu item.
 ------------------------------------------------------------------------------}
Function TWin32WidgetSet.MenuItemSetEnable(BaseMenuItem: TComponent): Boolean;
Var
  EnableFlag: Integer;
  AMenuItem: TMenuItem;
Begin
  AMenuItem:=BaseMenuItem as TMenuItem;
  if AMenuItem.Enabled then EnableFlag := MF_ENABLED
  else EnableFlag := MF_GRAYED;
  EnableFlag := EnableFlag or MF_BYCOMMAND;
  Result := Boolean(Windows.EnableMenuItem(AMenuItem.Parent.Handle, AMenuItem.Command, EnableFlag));
End;

{------------------------------------------------------------------------------
  Procedure: StatusBarPanelUpdate
  Params: StatusBar:
          PanelIndex:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.StatusBarPanelUpdate(StatusBar: TObject; PanelIndex: integer);
var
  TheStatusBar: TStatusBar;
begin
  TheStatusBar := (StatusBar as TStatusBar);
  UpdateStatusBarPanelWidths(TheStatusBar);
  UpdateStatusBarPanel(TheStatusBar.Panels[PanelIndex]);
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarSetText
  Params: StatusBar:
          PanelIndex:
          Text:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.StatusBarSetText(StatusBar: TObject; PanelIndex: integer);
var
  TheStatusBar: TStatusBar;
begin
  TheStatusBar := StatusBar as TStatusBar;
  if TheStatusBar.SimplePanel then
    Windows.SendMessage(TheStatusBar.Handle, SB_SETTEXT, 255, LPARAM(PChar(TheStatusBar.SimpleText)))
  else
    UpdateStatusBarPanel(TheStatusBar.Panels[PanelIndex]);
end;

{------------------------------------------------------------------------------
  Procedure: StatusBarUpdate
  Params: StatusBar:
  Returns: Nothing

 ------------------------------------------------------------------------------}
procedure TWin32WidgetSet.StatusBarUpdate(StatusBar: TObject);
var
  TheStatusBar: TStatusBar;
  PanelIndex: integer;
begin
  TheStatusBar := StatusBar as TStatusBar;
  Windows.SendMessage(TheStatusBar.Handle, SB_SIMPLE, WPARAM(TheStatusBar.SimplePanel), 0);
  if TheStatusBar.SimplePanel then
    StatusBarSetText(StatusBar, 0)
  else begin
    UpdateStatusBarPanelWidths(TheStatusBar);
    for PanelIndex := 0 to TheStatusBar.Panels.Count-1 do
      UpdateStatusBarPanel(TheStatusBar.Panels[PanelIndex]);
  end;
end;

{------------------------------------------------------------------------------
  Function: ToggleBoxCreateHandle
  Params: AToggleBox:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ToggleBoxCreateHandle(const AToggleBox: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ToolbarCreateHandle
  Params: AToolbar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ToolbarCreateHandle(const AToolbar: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: ToolButtonCreateHandle
  Params: AToolButton:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.ToolButtonCreateHandle(const AToolButton: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: TrackBarCreateHandle
  Params: ATrackBar:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.TrackBarCreateHandle(const ATrackBar: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

{------------------------------------------------------------------------------
  Function: WincontrolCreateHandle
  Params: AFixed:
          AParams:
  Returns:

 ------------------------------------------------------------------------------}
function TWin32WidgetSet.WincontrolCreateHandle(const AFixed: TObject; const AParams: TCreateParams): THandle;
begin
  //TODO: Implement
  Result := 0;
end;

//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
  Revision 1.16  2004/03/05 01:04:21  marc
  * Renamed TWin32Object to TWin32WidgetSet

  Revision 1.15  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.14  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.12  2004/02/22 15:47:58  micha
  fp 1.0 compatiblity

  Revision 1.11  2004/02/20 19:52:18  micha
  fixed: tarrow crash in win32
  added: interface function DrawArrow to draw themed arrow

  Revision 1.10  2004/02/05 13:53:38  mattias
  fixed GetConstraints for win32 intf

  Revision 1.9  2004/02/02 16:56:43  micha
  implement GetControlConstraints for combobox

  Revision 1.8  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.7  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.6  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.5  2004/01/03 11:57:48  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.4  2003/12/29 14:22:22  micha
  fix a lot of range check errors win32

  Revision 1.3  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.2  2003/11/26 21:55:15  mattias
  fixed win32 TBaseMenuitem

  Revision 1.1  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32


}
