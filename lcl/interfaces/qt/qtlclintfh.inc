{ $Id$ }

// included by qtint.pp

{******************************************************************************
                  All QT interface communication implementations.
                   Initial Revision  : Sat Jan 17 19:00:00 2004


  !! Keep alphabetical !!

  Support routines go to qtproc.pp

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
function ButtonCreateHandle(const AButton: TObject; const AParams: TCreateParams): THandle; override;

function CalendarCreateHandle(const ACalendar: TObject; const AParams: TCreateParams): THandle; override;
function CheckboxCreateHandle(const ACheckBox: TObject; const AParams: TCreateParams): THandle; override;
function CheckListBoxCreateHandle(const ACheckListBox: TObject; const AParams: TCreateParams): THandle; override;
function CListBoxCreateHandle(const ACListBox: TObject; const AParams: TCreateParams): THandle; override;
function ColorDialogCreateHandle(const AColorDialog: TObject): THandle; override;
function ComboBoxCreateHandle(const AComboBox: TObject; const AParams: TCreateParams): THandle; override;

function EditCreateHandle(const AEdit: TObject; const AParams: TCreateParams): THandle; override;

function FileDialogCreateHandle(const AFileDialog: TObject): THandle; override;
function FontDialogCreateHandle(const AFontDialog: TObject): THandle; override;
function FormCreateHandle(const AForm: TObject; const AParams: TCreateParams): THandle; override;

function GroupBoxCreateHandle(const AGroupBox: TObject; const AParams: TCreateParams): THandle; override;

function HintWindowCreateHandle(const AHintWindow: TObject; const AParams: TCreateParams): THandle; override;

function ImageCreateHandle(const AImage: TObject; const AParams: TCreateParams): THandle; override;

function LabelCreateHandle(const ALabel: TObject; const AParams: TCreateParams): THandle; override;
function ListBoxCreateHandle(const AListBox: TObject; const AParams: TCreateParams): THandle; override;
function ListViewCreateHandle(const AListView: TObject; const AParams: TCreateParams): THandle; override;

function MainMenuCreateHandle(const AMainMenu: TObject): THandle; override;
function MemoCreateHandle(const AMemo: TObject; const AParams: TCreateParams): THandle; override;
function MenuItemCreateHandle(const AMenuItem: TObject): THandle; override;

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

function ToggleBoxCreateHandle(const AToggleBox: TObject; const AParams: TCreateParams): THandle; override;

function ToolbarCreateHandle(const AToolbar: TObject; const AParams: TCreateParams): THandle; override;
function ToolButtonCreateHandle(const AToolButton: TObject; const AParams: TCreateParams): THandle; override;
function TrackBarCreateHandle(const ATrackBar: TObject; const AParams: TCreateParams): THandle; override;

function WincontrolCreateHandle(const AFixed: TObject; const AParams: TCreateParams): THandle; override;
//##apiwiz##eps##   // Do not remove, no wizard declaration after this line

{ =============================================================================

  $Log $
}
