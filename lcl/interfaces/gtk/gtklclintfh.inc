{ $Id$ }

// included by gtkint.pp

{******************************************************************************
                 All GTK interface communication implementations.
                   Initial Revision  : Sun Nov 23 23:53:53 2003


  !! Keep alphabetical !!

  Support routines go to gtkproc.pp

 ******************************************************************************
 Interface
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
function AlignmentCreateHandle(const AAlignment: TObject; const AParams: TCreateParams): THandle; override;
function ArrowCreateHandle(const AArrow: TObject; const AParams: TCreateParams): THandle; override;

function BitBtnCreateHandle(const ABitBtn: TObject; const AParams: TCreateParams): THandle; override;

function CalendarCreateHandle(const ACalendar: TObject; const AParams: TCreateParams): THandle; override;
function CheckboxCreateHandle(const ACheckBox: TObject; const AParams: TCreateParams): THandle; override;
function CheckListBoxCreateHandle(const ACheckListBox: TObject; const AParams: TCreateParams): THandle; override;
function CListBoxCreateHandle(const ACListBox: TObject; const AParams: TCreateParams): THandle; override;
function ColorDialogCreateHandle(const AColorDialog: TObject): THandle; override;
function ComboBoxCreateHandle(const AComboBox: TObject; const AParams: TCreateParams): THandle; override;

function DrawSplitter(DC: HDC; const ARect: TRect; Horizontal: boolean): boolean; override;

function EditCreateHandle(const AEdit: TObject; const AParams: TCreateParams): THandle; override;

function FileDialogCreateHandle(const AFileDialog: TObject): THandle; override;
function FontDialogCreateHandle(const AFontDialog: TObject): THandle; override;
function FormCreateHandle(const AForm: TObject; const AParams: TCreateParams): THandle; override;
function GetAcceleratorString(const AVKey: Byte; const AShiftState: TShiftState): String; override;
function GetControlConstraints(Constraints: TObject): boolean; override;
function GetLCLOwnerObject(Handle: HWnd): TObject; override;
function GetListBoxIndexAtY(ListBox: TComponent; y: integer): integer; override;
function GetListBoxItemRect(ListBox: TComponent; Index: integer; var ARect: TRect): boolean; override;
{$ifdef gtk1}
function GroupBoxCreateHandle(const AGroupBox: TObject; const AParams: TCreateParams): THandle; override;

function HintWindowCreateHandle(const AHintWindow: TObject; const AParams: TCreateParams): THandle; override;

function ImageCreateHandle(const AImage: TObject; const AParams: TCreateParams): THandle; override;
function IntfCreateHandle(const AObject: TObject; const AParams: TCreateParams): THandle; override;

function LabelCreateHandle(const ALabel: TObject; const AParams: TCreateParams): THandle; override;
function ListBoxCreateHandle(const AListBox: TObject; const AParams: TCreateParams): THandle; override;
function ListViewCreateHandle(const AListView: TObject; const AParams: TCreateParams): THandle; override;

function MainMenuCreateHandle(const AMainMenu: TObject): THandle; override;
function MemoCreateHandle(const AMemo: TObject; const AParams: TCreateParams): THandle; override;
function MenuItemCreateHandle(const AMenuItem: TObject): THandle; override;
{$endif gtk1}
function MenuItemSetCheck(BaseMenuItem: TComponent): Boolean; override;
function MenuItemSetEnable(BaseMenuItem: TComponent): Boolean; override;

function NotebookCreateHandle(const ANotebook: TObject; const AParams: TCreateParams): THandle; override;

function OpenFileDialogCreateHandle(const AOpenFileDialog: TObject): THandle; override;

function PageCreateHandle(const APage: TObject; const AParams: TCreateParams): THandle; override;
function PairSplitterCreateHandle(const APairSplitter: TObject; const AParams: TCreateParams): THandle; override;
function PairSplitterSideCreateHandle(const APairSplitterSide: TObject; const AParams: TCreateParams): THandle; override;
function PanelCreateHandle(const APanel: TObject; const AParams: TCreateParams): THandle; override;
function PopupMenuCreateHandle(const APopupMenu: TObject): THandle; override;
function PreviewFileControlCreateHandle(const APreviewFileControl: TObject): THandle; override;
function PreviewFileDialogCreateHandle(const APreviewFileDialog: TObject): THandle; override;
function ProgressBarCreateHandle(const AProgressBar: TObject; const AParams: TCreateParams): THandle; override;

function RadioButtonCreateHandle(const ARadioButton: TObject; const AParams: TCreateParams): THandle; override;

function SaveFileDialogCreateHandle(const ASaveFileDialog: TObject): THandle; override;
function ScrollBarCreateHandle(const AScrollBar: TObject; const AParams: TCreateParams): THandle; override;
function ScrollBoxCreateHandle(const AScrollBox: TObject; const AParams: TCreateParams): THandle; override;
function ScrolledWindowCreateHandle(const AScrolledWindow: TObject; const AParams: TCreateParams): THandle; override;
function SelectDirectoryDialogCreateHandle(const ASelectDirectoryDialog: TObject): THandle; override;
function SpeedButtonCreateHandle(const ASpeedButton: TObject; const AParams: TCreateParams): THandle; override;
function SpineditCreateHandle(const ASpinEdit: TObject; const AParams: TCreateParams): THandle; override;
function StatusBarCreateHandle(const AStatusBar: TObject; const AParams: TCreateParams): THandle; override;
procedure StatusBarPanelUpdate(StatusBar: TObject; Index: integer); override;
procedure StatusBarSetText(StatusBar: TObject; PanelIndex: integer); override;
procedure StatusBarUpdate(StatusBar: TObject); override;

function ToggleBoxCreateHandle(const AToggleBox: TObject; const AParams: TCreateParams): THandle; override;

function ToolbarCreateHandle(const AToolbar: TObject; const AParams: TCreateParams): THandle; override;
function ToolButtonCreateHandle(const AToolButton: TObject; const AParams: TCreateParams): THandle; override;
function TrackBarCreateHandle(const ATrackBar: TObject; const AParams: TCreateParams): THandle; override;

function WincontrolCreateHandle(const AFixed: TObject; const AParams: TCreateParams): THandle; override;
//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log$
  Revision 1.15  2004/03/19 00:03:15  marc
  * Moved the implementation of (GTK)ButtonCreateHandle to the new
    (GTK)WSButton class

  Revision 1.14  2004/03/09 15:30:15  peter
    * fixed gtk2 compilation

  Revision 1.13  2004/02/27 00:42:41  marc
  * Interface CreateComponent splitup
  * Implemented CreateButtonHandle on GTK interface
    on win32 interface it still needs to be done
  * Changed ApiWizz to support multilines and more interfaces

  Revision 1.12  2004/02/17 00:32:25  mattias
  fixed TCustomImage.DoAutoSize fixing uninitialized vars

  Revision 1.11  2004/02/02 15:46:19  mattias
  implemented basic TSplitter, still many ToDos

  Revision 1.10  2004/02/02 12:44:45  mattias
  implemented interface constraints

  Revision 1.9  2004/01/12 08:36:34  micha
  statusbar interface dependent reimplementation (from vincent)

  Revision 1.8  2004/01/11 16:38:29  marc
  * renamed (Check|Enable)MenuItem to MenuItemSet(Check|Enable)
  + Started with accelerator nameing routines
  * precheckin for createwidget splitup

  Revision 1.7  2004/01/11 11:57:54  mattias
  implemented TCustomListBox.ItemRect for gtk1 intf

  Revision 1.6  2004/01/09 20:03:13  mattias
  implemented new statusbar methods in gtk intf

  Revision 1.5  2004/01/03 11:57:48  mattias
  applied implementation for LM_LB_GETINDEXAT  from Vincent

  Revision 1.4  2003/11/27 23:02:30  mattias
  removed menutype.pas

  Revision 1.3  2003/11/26 21:30:19  mattias
  reduced unit circles, fixed fpImage streaming

  Revision 1.2  2003/11/26 00:23:47  marc
  * implemented new LCL(check|enable)Menuitem functions
  * introduced the lclintf inc files to win32

  Revision 1.1  2003/11/24 11:03:07  marc
  * Splitted winapi*.inc into a winapi and a lcl interface communication part

}
