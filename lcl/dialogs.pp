{
 /***************************************************************************
                               dialogs.pp
                               ----------
                Component Library Standard dialogs Controls


 ***************************************************************************/

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

{
@author(TMyClass - Michael A. Hess <author@emailaddress.com>)                       
@author(TMyOtherClass - Other Author Name <otherauthor@emailaddress.com>)                       
@created()
@lastmod()

Detailed description of the Unit.
} 

unit Dialogs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, InterfaceBase, FileUtil, LCLStrConsts, LCLType,
  LMessages, LCLProc, Forms, Controls, GraphType, Graphics, Buttons, StdCtrls,
  LCLClasses;


type
  TMsgDlgType    = (mtWarning, mtError, mtInformation, mtConfirmation,
                    mtCustom);
  TMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
                    mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose);
  TMsgDlgButtons = set of TMsgDlgBtn;

   
const
  mbYesNoCancel = [mbYes, mbNo, mbCancel];
  mbOKCancel = [mbOK, mbCancel];
  mbAbortRetryIgnore = [mbAbort, mbRetry, mbIgnore];
  
  MsgDlgBtnToBitBtnKind: array[TMsgDlgBtn] of TBitBtnKind = (
    bkYes, bkNo, bkOK, bkCancel, bkAbort, bkRetry, bkIgnore,
    bkAll, bkNoToAll, bkYesToAll, bkHelp, bkClose
    );

  BitBtnKindToMsgDlgBtn: array[TBitBtnKind] of TMsgDlgBtn = (
    mbOk, mbOK, mbCancel, mbHelp, mbYes, mbNo,
    mbClose, mbAbort, mbRetry, mbIgnore, mbAll, mbNoToALl, mbYesToAll
    );

type

  { TCommonDialog }

  TCommonDialog = class(TLCLComponent)
  private
    FHandle : integer;
    FHeight: integer;
    FWidth: integer;
    FOnCanClose: TCloseQueryEvent;
    FOnShow, FOnClose : TNotifyEvent;
    FTitle : string;
    FUserChoice: integer;
    FHelpContext: THelpContext;
    procedure SetHandle(const AValue: integer);
    procedure SetHeight(const AValue: integer);
    procedure SetWidth(const AValue: integer);
  protected
    function DoExecute : boolean; virtual;
  public
    FCompStyle : LongInt;
    constructor Create(TheOwner: TComponent); override;
    function Execute: boolean; virtual;
    property Handle: integer read FHandle write SetHandle;
    property Title: string read FTitle write FTitle;
    property UserChoice: integer read FUserChoice write FUserChoice;
    procedure Close; virtual;
    procedure DoShow; virtual;
    procedure DoClose; virtual;
    function HandleAllocated: boolean;
  published
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCanClose: TCloseQueryEvent read FOnCanClose write FOnCanClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight write SetHeight;
  end;


  { TFileDialog }
  
  TFileDialog = class(TCommonDialog)
  private
    FDefaultExt: string;
    FFileName : String;
    FFiles: TStrings;
    FFilter: String;
    FFilterIndex: Integer;
    FHistoryList: TStrings;
    FInitialDir: string;
    FOldWorkingDir: string;
    FOnHelpClicked: TNotifyEvent;
    procedure SetDefaultExt(const AValue: string);
  protected
    function DoExecute: boolean; override;
    procedure SetFileName(const Value: String); virtual;
    procedure SetFilter(const Value: String); virtual;
    procedure SetHistoryList(const AValue: TStrings); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: boolean; override;
    property Files: TStrings read FFiles;
    property HistoryList: TStrings read FHistoryList write SetHistoryList;
  published
    property Title;
    property DefaultExt: string read FDefaultExt write SetDefaultExt;
    property FileName: String read FFileName write SetFileName;
    property Filter: String read FFilter write SetFilter;
    property FilterIndex: Integer read FFilterIndex write FFilterIndex default 1;
    property InitialDir: string read FInitialDir write FInitialDir;
    property OnHelpClicked: TNotifyEvent read FOnHelpClicked write FOnHelpClicked;
  end;


  { TOpenDialog }
  
  TOpenOption = (
    ofReadOnly,
    ofOverwritePrompt, // if selected file exists shows a message, that file
                       // will be overwritten
    ofHideReadOnly,    // hide read only file
    ofNoChangeDir,     // do not change current directory
    ofShowHelp,        // show a help button
    ofNoValidate,
    ofAllowMultiSelect,// allow multiselection
    ofExtensionDifferent,
    ofPathMustExist,   // shows an error message if selected path does not exist
    ofFileMustExist,   // shows an error message if selected file does not exist
    ofCreatePrompt,
    ofShareAware,
    ofNoReadOnlyReturn,// do not return filenames that are readonly
    ofNoTestFileCreate,
    ofNoNetworkButton,
    ofNoLongNames,
    ofOldStyleDialog,
    ofNoDereferenceLinks,// do not expand filenames
    ofEnableIncludeNotify,
    ofEnableSizing,    // dialog can be resized, e.g. via the mouse
    ofDontAddToRecent, // do not add the path to the history list
    ofForceShowHidden, // show hidden files
    ofViewDetail,      // details are OS and interface dependent
    ofAutoPreview      // details are OS and interface dependent
    );
  TOpenOptions = set of TOpenOption;
  
const
  DefaultOpenDialogOptions = [ofEnableSizing, ofViewDetail];
  
type
  
  TOpenDialog = class(TFileDialog)
  private
    FOnFolderChange: TNotifyEvent;
    FOnSelectionChange: TNotifyEvent;
    FOptions: TOpenOptions;
    FLastSelectionChangeFilename: string;
  protected
    procedure DereferenceLinks; virtual;
    function CheckFile(var AFilename: string): boolean; virtual;
    function CheckAllFiles: boolean; virtual;
    function DoExecute: boolean; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure DoFolderChange; virtual;
    procedure DoSelectionChange; virtual;
  published
    property Options: TOpenOptions read FOptions write FOptions
      default DefaultOpenDialogOptions;
    property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;


  { TSaveDialog }
  
  TSaveDialog = class(TOpenDialog)
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  
  { TSelectDirectoryDialog }
  
  TSelectDirectoryDialog = class(TOpenDialog)
  protected
    function CheckFile(var AFilename: string): boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;


  { TColorDialog }
  
  TColorDialog = class(TCommonDialog)
  private
    FColor: TColor;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Title;
    property Color: TColor read FColor write FColor;
  end;


  { TColorButton }

  TColorButton = class(TGraphicControl)
  private
    FBorderWidth: integer;
    FButtonColor: TColor;
    FColorDialog: TColorDialog;
    FOnColorChanged: TNotifyEvent;
    procedure SetBorderWidth(const AValue: integer);
  protected
    procedure Click; override;
    procedure Paint; override;
    procedure SetButtonColor(Value: TColor);
    procedure ShowColorDialog; virtual;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; Override;
  published
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderWidth: integer read FBorderWidth write SetBorderWidth;
    property ButtonColor:TColor read FButtonColor write SetButtonColor;
    property Hint;
    property OnChangeBounds;
    property OnColorChanged: TNotifyEvent read FOnColorChanged
                                          write FOnColorChanged;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;
  end;


  { TFontDialog }

  TFontDialogOption = (fdAnsiOnly, fdTrueTypeOnly, fdEffects,
    fdFixedPitchOnly, fdForceFontExist, fdNoFaceSel, fdNoOEMFonts,
    fdNoSimulations, fdNoSizeSel, fdNoStyleSel,  fdNoVectorFonts,
    fdShowHelp, fdWysiwyg, fdLimitSize, fdScalableOnly, fdApplyButton);
  TFontDialogOptions = set of TFontDialogOption;
  
  TFontDialog = class(TCommonDialog)
  private
    FFont: TFont;
    FMaxFontSize: Integer;
    FMinFontSize: Integer;
    FOnApplyClicked: TNotifyEvent;
    FOptions: TFontDialogOptions;
    FPreviewText: string;
    procedure SetFont(const AValue: TFont);
  public
    procedure ApplyClicked; virtual;
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
  published
    property Title;
    property Font: TFont read FFont write SetFont;
    property MinFontSize: Integer read FMinFontSize write FMinFontSize;
    property MaxFontSize: Integer read FMaxFontSize write FMaxFontSize;
    property Options: TFontDialogOptions
      read FOptions write FOptions default [fdEffects];
    property OnApplyClicked: TNotifyEvent
      read FOnApplyClicked write FOnApplyClicked;
    property PreviewText: string read FPreviewText write FPreviewText;
  end;
  
  
{ MessageDlg }

function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
function MessageDlgPos(const aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer;
function MessageDlgPosHelp(const aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
            const HelpFileName: string): Integer;
function QuestionDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: array of const; HelpCtx: Longint): TModalResult;
            
procedure ShowMessage(const aMsg: string);
procedure ShowMessageFmt(const aMsg: string; Params: array of const);
procedure ShowMessagePos(const aMsg: string; X, Y: Integer);

Function InputQuery(const ACaption, APrompt : String; MaskInput : Boolean; var Value : String) : Boolean;
Function InputQuery(const ACaption, APrompt : String; var Value : String) : Boolean;
Function InputBox(const ACaption, APrompt, ADefault : String) : String;
Function PasswordBox(const ACaption, APrompt : String) : String;
  
function SelectDirectory(const Caption, InitialDirectory: string;
  var Directory: string): boolean;
function SelectDirectory(const Caption, InitialDirectory: string;
  var Directory: string; ShowHidden: boolean): boolean;


procedure Register;

implementation

uses 
  Math, WSDialogs;

const
  //
  //TODO: all the constants below should be replaced in the future
  //      their only purpose is to overcome some current design flaws &
  //      missing features in the GTK libraries
  //
  cBitmapX  = 10;      // x-position for bitmap in messagedialog
  cBitmapY  = 10;      // y-position for bitmap in messagedialog
  cLabelSpacing = 10;   // distance between icon & label

procedure Register;
begin
  RegisterComponents('Dialogs',[TOpenDialog,TSaveDialog,TSelectDirectoryDialog,
                                TColorDialog,TFontDialog]);
  RegisterComponents('Misc',[TColorButton]);
end;

function ShowMessageBox(Text, Caption : PChar; Flags : Longint) : Integer;
var
  DlgType : TMsgDlgType;
  Buttons : TMsgDlgButtons;
begin
  //This uses TMessageBox class in MessageDialogs.inc
  if (Flags and MB_RETRYCANCEL) = MB_RETRYCANCEL then
    Buttons := [mbREtry, mbCancel]
  else
  if (Flags and MB_YESNO) = MB_YESNO then
    Buttons := [mbYes, mbNo]
  else
  if (Flags and MB_YESNOCANCEL) = MB_YESNOCANCEL then
    Buttons := [mbYes, mbNo, mbCancel]
  else
  if (Flags and MB_ABORTRETRYIGNORE) = MB_ABORTRETRYIGNORE then
    Buttons := [mbAbort, mbRetry, mbIgnore]
  else
  if (Flags and MB_OKCANCEL) = MB_OKCANCEL then
    Buttons := [mbOK,mbCancel]
  else
  if (Flags and MB_OK) = MB_OK then
    Buttons := [mbOK]
  else
    Buttons := [mbOK];


  if (Flags and MB_ICONQUESTION) = MB_ICONQUESTION then
    DlgTYpe := mtConfirmation
  else
  if (Flags and MB_ICONINFORMATION) = MB_ICONINFORMATION then
    DlgTYpe := mtInformation
  else
  if (Flags and MB_ICONERROR) = MB_ICONERROR then
    DlgTYpe := mtError
  else
  if (Flags and MB_ICONWARNING) = MB_ICONWARNING then
    DlgTYpe := mtWarning
  else
    DlgTYpe := mtCustom;

  Result := MessageDlg(Caption,Text,DlgType,Buttons,0);
end;


{$I colordialog.inc}
{$I commondialog.inc}
{$I filedialog.inc}
{$I fontdialog.inc}
{$I inputdialog.inc}
{$I messagedialogs.inc}
{$I promptdialog.inc}
{$I colorbutton.inc}

initialization
  Forms.MessageBoxFunction:=@ShowMessageBox;
  InterfaceBase.InputDialogFunction:=@ShowInputDialog;
  InterfaceBase.PromptDialogFunction:=@ShowPromptDialog;

finalization

end.

{ =============================================================================

  $Log$
  Revision 1.58  2005/01/27 19:03:51  mattias
  added QuestionDlg - a MessageDlg with custom buttons

  Revision 1.57  2004/12/27 19:40:59  mattias
  published BorderSpacing for many controls

  Revision 1.56  2004/12/21 11:14:29  mattias
  implemented im/export of install package list

  Revision 1.55  2004/12/13 21:30:21  mattias
  implemented TMultiPropertyLink

  Revision 1.54  2004/09/27 21:45:44  vincents
  splitted off unit FileUtil, it doesn't depend on other LCL units

  Revision 1.53  2004/09/14 21:30:37  vincents
  replaced writeln by DebugLn

  Revision 1.52  2004/09/13 13:13:46  micha
  convert LM_SHOWMODAL to interface methods

  Revision 1.51  2004/08/18 09:31:21  mattias
  removed obsolete unit vclglobals

  Revision 1.50  2004/08/01 14:35:26  micha
  publish Title property on dialogs for .dfm streaming

  Revision 1.49  2004/04/03 13:10:20  mattias
  fixed TCalendarDialog.DialogTitle

  Revision 1.48  2004/03/20 17:36:56  mattias
  added IDEIntf package and component editors for MemDS

  Revision 1.47  2004/03/17 00:34:37  marc
  * Interface reconstruction. Created skeleton units, classes and wscontrols

  Revision 1.46  2004/02/23 08:19:04  micha
  revert intf split

  Revision 1.44  2004/02/04 13:40:19  mattias
  ShortCutToText now deletes any modifier

  Revision 1.43  2004/02/04 00:21:40  mattias
  added SelectDirectory and TListBox.ItemVisible

  Revision 1.42  2004/01/24 16:25:35  mattias
  TSelectDirectoryDialog ofFileMustExist now checks for directory from Vincent

  Revision 1.41  2004/01/24 11:42:53  micha
  findinfiles uses tselectdirectorydialog; remove global SelectDirectory function (from vincent)

  Revision 1.40  2004/01/13 16:39:01  mattias
  changed consistency stops during var renaming to errors

  Revision 1.39  2003/12/23 20:40:43  mattias
  added TEditButton, TFileNameEdit, TDirectoryEdit, TDateEdit, TCalcEdit from Michael V.C.

  Revision 1.38  2003/10/15 20:33:36  ajgenius
  add csForm, start fixing Style matching for syscolors and fonts

  Revision 1.37  2003/09/18 09:21:03  mattias
  renamed LCLLinux to LCLIntf

  Revision 1.36  2003/09/04 10:51:30  mattias
  fixed default size of preview widget

  Revision 1.35  2003/09/02 21:32:56  mattias
  implemented TOpenPictureDialog

  Revision 1.34  2003/08/14 10:36:55  mattias
  added TSelectDirectoryDialog

  Revision 1.33  2003/08/01 09:44:52  mattias
  added SelectDirectory dialog

  Revision 1.32  2003/06/13 21:32:17  mattias
  moved TColorButton to misc palette

  Revision 1.31  2003/06/13 21:08:53  mattias
  moved TColorButton to dialogs.pp

  Revision 1.30  2003/04/13 13:45:04  mattias
  implemented broken dependencies dialog

  Revision 1.29  2003/04/08 09:04:07  mattias
  fixed registration for fpc 1.0.x

  Revision 1.28  2003/04/04 16:35:24  mattias
  started package registration

  Revision 1.27  2003/03/15 09:42:49  mattias
  fixed transient windows

  Revision 1.26  2002/10/25 15:27:02  lazarus
  AJ: Moved form contents creation to gtkproc for code
      reuse between GNOME and GTK, and to make GNOME MDI
      programming easier later on.

  Revision 1.25  2002/10/25 14:59:11  lazarus
  AJ: MessageDlgs -> PromptUser, better Cancel/Default order

  Revision 1.24  2002/10/25 10:06:34  lazarus
  MG: broke interfacebase uses circles

  Revision 1.23  2002/10/25 09:47:37  lazarus
  MG: added inputdialog.inc

  Revision 1.22  2002/10/24 10:37:04  lazarus
  MG: broke dialogs.pp <-> forms.pp circle

  Revision 1.21  2002/10/24 10:05:51  lazarus
  MG: broke graphics.pp <-> clipbrd.pp circle

  Revision 1.20  2002/10/11 16:00:39  lazarus
  AJ: made InputQuery Interface Dependant

  Revision 1.19  2002/10/10 13:29:08  lazarus
  AJ: added LoadStockPixmap routine & minor fixes to/for GNOMEInt

  Revision 1.18  2002/09/27 20:52:21  lazarus
  MWE: Applied patch from "Andrew Johnson" <aj_genius@hotmail.com>

  Here is the run down of what it includes -

   -Vasily Volchenko's Updated Russian Localizations

   -improvements to GTK Styles/SysColors
   -initial GTK Palette code - (untested, and for now useless)

   -Hint Windows and Modal dialogs now try to stay transient to
    the main program form, aka they stay on top of the main form
    and usually minimize/maximize with it.

   -fixes to Form BorderStyle code(tool windows needed a border)

   -fixes DrawFrameControl DFCS_BUTTONPUSH to match Win32 better
    when flat

   -fixes DrawFrameControl DFCS_BUTTONCHECK to match Win32 better
    and to match GTK theme better. It works most of the time now,
    but some themes, noteably Default, don't work.

   -fixes bug in Bitmap code which broke compiling in NoGDKPixbuf
    mode.

   -misc other cleanups/ fixes in gtk interface

   -speedbutton's should now draw correctly when flat in Win32

   -I have included an experimental new CheckBox(disabled by
    default) which has initial support for cbGrayed(Tri-State),
    and WordWrap, and misc other improvements. It is not done, it
    is mostly a quick hack to test DrawFrameControl
    DFCS_BUTTONCHECK, however it offers many improvements which
    can be seen in cbsCheck/cbsCrissCross (aka non-themed) state.

   -fixes Message Dialogs to more accurately determine
    button Spacing/Size, and Label Spacing/Size based on current
    System font.
   -fixes MessageDlgPos, & ShowMessagePos in Dialogs
   -adds InputQuery & InputBox to Dialogs

   -re-arranges & somewhat re-designs Control Tabbing, it now
    partially works - wrapping around doesn't work, and
    subcontrols(Panels & Children, etc) don't work. TabOrder now
    works to an extent. I am not sure what is wrong with my code,
    based on my other tests at least wrapping and TabOrder SHOULD
    work properly, but.. Anyone want to try and fix?

   -SynEdit(Code Editor) now changes mouse cursor to match
    position(aka over scrollbar/gutter vs over text edit)

   -adds a TRegion property to Graphics.pp, and Canvas. Once I
    figure out how to handle complex regions(aka polygons) data
    properly I will add Region functions to the canvas itself
    (SetClipRect, intersectClipRect etc.)

   -BitBtn now has a Stored flag on Glyph so it doesn't store to
    lfm/lrs if Glyph is Empty, or if Glyph is not bkCustom(aka
    bkOk, bkCancel, etc.) This should fix most crashes with older
    GDKPixbuf libs.

  Revision 1.17  2002/08/06 09:32:48  lazarus
  MG: moved TColor definition to graphtype.pp and registered TColor names

  Revision 1.16  2002/07/29 13:39:06  lazarus
  MG: removed ambigious TBitmap from LCLType and added Escape key to MessageDlgs

  Revision 1.15  2002/07/04 11:46:00  lazarus
  MG: moved resourcestring to lclstrconsts.pas

  Revision 1.14  2002/06/04 15:17:21  lazarus
  MG: improved TFont for XLFD font names

  Revision 1.13  2002/05/30 14:11:11  lazarus
  MG: added filters and history to TOpenDialog

  Revision 1.12  2002/05/29 21:44:37  lazarus
  MG: improved TCommon/File/OpenDialog, fixed TListView scrolling and broder

  Revision 1.11  2002/05/10 06:05:49  lazarus
  MG: changed license to LGPL

  Revision 1.10  2002/01/25 19:42:56  lazarus
  Keith: Improved events and common dialogs on Win32

  Revision 1.9  2001/12/11 14:36:41  lazarus
  MG: started multiselection for TOpenDialog

  Revision 1.8  2001/11/01 18:48:51  lazarus
  Changed Application.Messagebox to use TMessageBox class.
  Added icon images for mtError and mtConfirmation
  Shane

  Revision 1.7  2001/07/31 18:40:24  lazarus
  MG: added unit info, arrow xpms, and many changes from jens arm

  Revision 1.6  2001/06/06 12:30:41  lazarus
  MG: bugfixes

  Revision 1.5  2001/03/27 11:11:13  lazarus
  MG: fixed mouse msg, added filedialog initialdir

  Revision 1.4  2001/03/03 00:48:03  lazarus
  + added support for message dialogs
  stoppok

  Revision 1.3  2000/08/10 10:55:45  lazarus
  Changed TCustomDialog to TCommonDialog
  Shane

  Revision 1.2  2000/08/09 18:32:10  lazarus
  Added more code for the find function.
  Shane

  Revision 1.1  2000/07/13 10:28:23  michael
  + Initial import

  Revision 1.10  2000/02/28 19:16:04  lazarus
  Added code to the FILE CLOSE to check if the file was modified.  HAven't gotten the application.messagebox working yet though.  It won't stay visible.
  Shane

  Revision 1.9  2000/02/22 22:19:49  lazarus
  TCustomDialog is a descendant of TComponent.
  Initial cuts a form's proper Close behaviour.

  Revision 1.8  2000/02/22 17:32:49  lazarus
  Modified the ShowModal call.
  For TCustomForm is simply sets the visible to true now and adds fsModal to FFormState.  In gtkObject.inc FFormState is checked.  If it contains fsModal then either gtk_grab_add or gtk_grab_remove is called depending on the value of VISIBLE.

  The same goes for TCustomDialog (open, save, font, color).
  I moved the Execute out of the individual dialogs and moved it into TCustomDialog and made it virtual because FONT needs to set some stuff before calling the inherited execute.
  Shane

  Revision 1.7  1999/12/10 00:47:01  lazarus
  MWE:
    Fixed some samples
    Fixed Dialog parent is no longer needed
    Fixed (Win)Control Destruction
    Fixed MenuClick

  Revision 1.6  1999/09/15 02:14:44  lazarus
  *** empty log message ***

  Revision 1.5  1999/08/16 18:45:38  lazarus
  Added a TFont Dialog plus minor additions.

  Shane Aug 16th 1999  14:07 CST

  Revision 1.4  1999/07/31 06:39:21  lazarus

       Modified the IntSendMessage3 to include a data variable. It isn't used
       yet but will help in merging the Message2 and Message3 features.

       Adjusted TColor routines to match Delphi color format

       Added a TGdkColorToTColor routine in gtkproc.inc

       Finished the TColorDialog added to comDialog example.        MAH

 }
