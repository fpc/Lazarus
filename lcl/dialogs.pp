{
 /***************************************************************************
                               dialogs.pp
                               ----------
                Component Library Standard dialogs Controls


 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit Dialogs;

{$mode objfpc}{$H+}

interface

uses
  // RTL + FCL
  Types, typinfo, Classes, SysUtils,
  // LCL
  LMessages, LResources, LCLIntf, InterfaceBase, LCLStrConsts, LCLType,
  Forms, Controls, Themes, Graphics, Buttons, ButtonPanel, StdCtrls,
  ExtCtrls, LCLClasses, ClipBrd, Menus, LCLTaskDialog, DialogRes,
  // LazUtils
  GraphType, UITypes, FileUtil, LazFileUtils, LazStringUtils, LazLoggerBase;

type
  // Aliases for types in UITypes.
  TMsgDlgType    = UITypes.TMsgDlgType;
  TMsgDlgBtn     = UITypes.TMsgDlgBtn;
  TMsgDlgButtons = UITypes.TMsgDlgButtons;

const
  // Aliases for enum values in UITypes.
  mtWarning      = UITypes.TMsgDlgType.mtWarning;
  mtError        = UITypes.TMsgDlgType.mtError;
  mtInformation  = UITypes.TMsgDlgType.mtInformation;
  mtConfirmation = UITypes.TMsgDlgType.mtConfirmation;
  mtCustom       = UITypes.TMsgDlgType.mtCustom;

  mbYes      = UITypes.TMsgDlgBtn.mbYes;
  mbNo       = UITypes.TMsgDlgBtn.mbNo;
  mbOK       = UITypes.TMsgDlgBtn.mbOK;
  mbCancel   = UITypes.TMsgDlgBtn.mbCancel;
  mbAbort    = UITypes.TMsgDlgBtn.mbAbort;
  mbRetry    = UITypes.TMsgDlgBtn.mbRetry;
  mbIgnore   = UITypes.TMsgDlgBtn.mbIgnore;
  mbAll      = UITypes.TMsgDlgBtn.mbAll;
  mbNoToAll  = UITypes.TMsgDlgBtn.mbNoToAll;
  mbYesToAll = UITypes.TMsgDlgBtn.mbYesToAll;
  mbHelp     = UITypes.TMsgDlgBtn.mbHelp;
  mbClose    = UITypes.TMsgDlgBtn.mbClose;

  // Combinations of buttons.
  mbYesNoCancel = [mbYes, mbNo, mbCancel];
  mbYesNo = [mbYes, mbNo];
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

  TCDWSEventCapability = (cdecWSPerformsDoShow, cdecWSPerformsDoCanClose, cdecWSPerformsDoClose,
                          cdecWSNOCanCloseSupport);
  TCDWSEventCapabilities = set of TCDWSEventCapability;

  TDialogResultEvent = procedure(Sender: TObject; Success: Boolean) of object;

  TCommonDialog = class(TLCLComponent)
  private
    FAttachTo: TCustomForm;
    FHandle : THandle;
    FHeight: Integer;
    FOnDialogResult: TDialogResultEvent;
    FWidth: Integer;
    FOnCanClose: TCloseQueryEvent;
    FOnShow, FOnClose : TNotifyEvent;
    FTitle : string;
    FUserChoice: Integer;
    FHelpContext: THelpContext;
    FDoCanCloseCalled: Boolean;
    FDoShowCalled: Boolean;
    FDoCloseCalled: Boolean;
    FClosing: Boolean;
    FWSEventCapabilities :TCDWSEventCapabilities;
    procedure SetHandle(const AValue: THandle);
    function IsTitleStored: Boolean;
  protected
    class procedure WSRegisterClass; override;
    function DoExecute : Boolean; virtual;
    function DefaultTitle: string; virtual;
    function GetHeight: Integer; virtual;
    function GetWidth: Integer; virtual;
    procedure SetHeight(const AValue: Integer); virtual;
    procedure SetWidth(const AValue: Integer); virtual;
    procedure ResetShowCloseFlags;
    property AttachTo: TCustomForm read FAttachTo write FAttachTo; platform;
    property OnDialogResult:TDialogResultEvent read FOnDialogResult write FOnDialogResult; platform;
  public
    FCompStyle : LongInt;
    constructor Create(TheOwner: TComponent); override;
    function Execute: Boolean; virtual;
    property Handle: THandle read FHandle write SetHandle;
    property UserChoice: Integer read FUserChoice write FUserChoice;
    procedure Close; virtual;
    procedure DoShow; virtual;
    procedure DoCanClose(var CanClose: Boolean); virtual;
    procedure DoClose; virtual;
    function HandleAllocated: Boolean;
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
  published
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCanClose: TCloseQueryEvent read FOnCanClose write FOnCanClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property Title: TTranslateString read FTitle write FTitle stored IsTitleStored nodefault;
  end;


  { TFileDialog }
  
  TFileDialog = class(TCommonDialog)
  private
    FInternalFilterIndex: Integer;
    FDefaultExt: string;
    FFileName : string;
    FFiles: TStrings;
    FFilter: string;
    FFilterIndex: Integer;
    FHistoryList: TStrings;
    FInitialDir: string;
    FOnHelpClicked: TNotifyEvent;
    FOnTypeChange: TNotifyEvent;
    procedure SetDefaultExt(const AValue: string);
    procedure SetFilterIndex(const AValue: Integer);
  protected
    class procedure WSRegisterClass; override;
    function GetFilterIndex: Integer; virtual;
    procedure SetFileName(const Value: string); virtual;
    procedure SetFilter(const Value: string); virtual;
    procedure SetHistoryList(const AValue: TStrings); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoCanClose(var CanClose: Boolean); override;
    procedure DoTypeChange; virtual;
    property Files: TStrings read FFiles;
    property HistoryList: TStrings read FHistoryList write SetHistoryList;
    procedure IntfFileTypeChanged(NewFilterIndex: Integer);
    class function FindMaskInFilter(aFilter, aMask: string): Integer;
    class function ExtractAllFilterMasks(aFilter: string;
                                   SkipAllFilesMask: Boolean = true): string;
  published
    property Title;
    property DefaultExt: string read FDefaultExt write SetDefaultExt;
    property FileName: string read FFileName write SetFileName;
    property Filter: string read FFilter write SetFilter;
    property FilterIndex: Integer read GetFilterIndex write SetFilterIndex default 1;
    property InitialDir: string read FInitialDir write FInitialDir;
    property OnHelpClicked: TNotifyEvent read FOnHelpClicked write FOnHelpClicked;
    property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
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
    ofNoDereferenceLinks,// do not resolve links while dialog is shown (only on Windows, see OFN_NODEREFERENCELINKS)
    ofNoResolveLinks,  // do not resolve links after Execute
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
    class procedure WSRegisterClass; override;
    procedure ResolveLinks; virtual;
    procedure DereferenceLinks; virtual; deprecated 'override ResolveLinks instead' {Laz 1.9};
    function CheckFile(var AFilename: string): Boolean; virtual;
    function CheckFileMustExist(const AFileName: string): Boolean; virtual;
    function CheckAllFiles: Boolean; virtual;
    function DoExecute: Boolean; override;
    function DefaultTitle: string; override;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure DoCanClose(var CanClose: Boolean); override;
    procedure DoFolderChange; virtual;
    procedure DoSelectionChange; virtual;
    procedure IntfSetOption(const AOption: TOpenOption; const AValue: Boolean);
  published
    property Options: TOpenOptions read FOptions write FOptions default DefaultOpenDialogOptions;
    property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
  end;


  { TSaveDialog }
  
  TSaveDialog = class(TOpenDialog)
  protected
    class procedure WSRegisterClass; override;
    function DefaultTitle: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  
  
  { TSelectDirectoryDialog }
  
  TSelectDirectoryDialog = class(TOpenDialog)
  protected
    class procedure WSRegisterClass; override;
    function CheckFileMustExist(const AFilename: string): Boolean; override;
    function DefaultTitle: string; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  { TColorDialog }
  
  TColorDialog = class(TCommonDialog)
  private
    FColor: TColor;
    FCustomColors: TStrings;
    procedure SetCustomColors(const AValue: TStrings);
    procedure AddDefaultColor(const s: AnsiString);
  protected
    class procedure WSRegisterClass; override;
    function DefaultTitle: string; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Title;
    property Color: TColor read FColor write FColor;
    // entry looks like ColorA = FFFF00 ... ColorX = C0C0C0
    property CustomColors: TStrings read FCustomColors write SetCustomColors;
  end;


  { TColorButton }

  TColorButton = class(TCustomSpeedButton)
  private
    FBorderWidth: Integer;
    FButtonColorAutoSize: Boolean;
    FButtonColorSize: Integer;
    FButtonColor: TColor;
    FColorDialog: TColorDialog;
    FOnColorChanged: TNotifyEvent;
    FDisabledPattern: TBitmap;
    function IsButtonColorAutoSizeStored: Boolean;
    procedure SetBorderWidth(const AValue: Integer);
    procedure SetButtonColor(const AValue: TColor);
    procedure SetButtonColorAutoSize(const AValue: Boolean);
    procedure SetButtonColorSize(const AValue: Integer);
  protected
    class procedure WSRegisterClass; override;
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy;
      const AXProportion, AYProportion: Double); override;
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
      AState: TButtonState; ATransparent: Boolean; BiDiFlags: Longint): TRect; override;
    function GetDisabledPattern: TBitmap; virtual;
    function GetGlyphSize(Drawing: Boolean; PaintRect: TRect): TSize; override;
    class function GetControlClassDefaultSize: TSize; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ShowColorDialog; virtual;
  public
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; Override;
    procedure Click; override;
  published
    property Action;
    property Align;
    property Anchors;
    property AllowAllUp;
    property BorderSpacing;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth;
    property ButtonColorAutoSize: Boolean read FButtonColorAutoSize
                                          write SetButtonColorAutoSize
                                          stored IsButtonColorAutoSizeStored;
    property ButtonColorSize: Integer read FButtonColorSize write SetButtonColorSize;
    property ButtonColor: TColor read FButtonColor write SetButtonColor;
    property ColorDialog: TColorDialog read FColorDialog write FColorDialog;
    property Constraints;
    property Caption;
    property Color;
    property Down;
    property Enabled;
    property Flat;
    property Font;
    property GroupIndex;
    property Hint;
    property Layout;
    property Margin;
    property Spacing;
    property Transparent;
    property Visible;
    property OnClick;
    property OnColorChanged: TNotifyEvent read FOnColorChanged
                                          write FOnColorChanged;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnChangeBounds;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
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
  protected
    class procedure WSRegisterClass; override;
    function DefaultTitle: string; override;
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
  
  
{ TFindDialog }
  
  TFindOption = (frDown, frFindNext, frHideMatchCase, frHideWholeWord,
                 frHideUpDown, frMatchCase, frDisableMatchCase, frDisableUpDown,
                 frDisableWholeWord, frReplace, frReplaceAll, frWholeWord, frShowHelp,
                 frEntireScope, frHideEntireScope, frPromptOnReplace, frHidePromptOnReplace,
                 frButtonsAtBottom);
  TFindOptions = set of TFindOption;

  TFindDialog = class(TCommonDialog)
  private
    FFormLeft: Integer;
    FFormTop: Integer;
    function GetReplaceText: string;
    function GetFindText: string;
    function GetLeft: Integer;
    function GetPosition: TPoint;
    function GetTop: Integer;
    procedure SetFindText(const AValue: string);
    procedure SetLeft(const AValue: Integer);
    procedure SetOptions(AValue: TFindOptions);
    procedure SetPosition(const AValue: TPoint);
    procedure SetTop(const AValue: Integer);
    procedure SetReplaceText(const AValue: string);
  protected
    FFindForm: TForm;
    FOnReplace: TNotifyEvent;
    FOnFind: TNotifyEvent;
    FOptions: TFindOptions;
    FOnHelpClicked: TNotifyEvent;
    FReplaceText: string;
    FFindText: string;

    function DefaultTitle: string; override;

    procedure FindClick(Sender: TObject);
    procedure HelpClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);

    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure DoCloseForm(Sender: TObject; var CloseAction: TCloseAction);virtual;
    procedure DoShowForm(Sender: TObject);virtual;
    procedure Find; virtual;
    procedure Help; virtual;
    procedure Replace; virtual;
    function CreateForm:TForm;virtual;
    procedure SetFormValues;virtual;
    procedure GetFormValues; virtual;
    Procedure CalcPosition(aForm:Tform);
    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property OnReplace: TNotifyEvent read FOnReplace write FOnReplace;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseDialog;
    function Execute: Boolean; override;
    property Left: Integer read GetLeft write SetLeft;
    property Position: TPoint read GetPosition write SetPosition;
    property Top: Integer read GetTop write SetTop;
  published
    property FindText: string read GetFindText write SetFindText;
    property Options: TFindOptions read FOptions write SetOptions default [frDown];
    property OnFind: TNotifyEvent read FOnFind write FOnFind;
    property OnHelpClicked: TNotifyEvent read FOnHelpClicked write FOnHelpClicked;
  end;


{ TReplaceDialog }

  TReplaceDialog = class(TFindDialog)
  protected
    function DefaultTitle: string; override;
    procedure ReplaceClick(Sender: TObject);
    procedure ReplaceAllClick(Sender: TObject);
    function CreateForm: TForm; override;
    procedure SetFormValues; override;
    procedure GetFormValues; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property ReplaceText;
    property OnReplace;
  end;



{ TPrinterSetupDialog }

  TCustomPrinterSetupDialog = class(TCommonDialog)
  end;


{ TPrintDialog }

  TPrintRange = (prAllPages, prSelection, prPageNums, prCurrentPage);
  TPrintDialogOption = (poPrintToFile, poPageNums, poSelection, poWarning,
    poHelp, poDisablePrintToFile, poBeforeBeginDoc);
  TPrintDialogOptions = set of TPrintDialogOption;

  TCustomPrintDialog = class(TCommonDialog)
  private
    FFromPage: Integer;
    FToPage: Integer;
    FCollate: Boolean;
    FOptions: TPrintDialogOptions;
    FPrintToFile: Boolean;
    FPrintRange: TPrintRange;
    FMinPage: Integer;
    FMaxPage: Integer;
    FCopies: Integer;
  public
    constructor Create(TheOwner: TComponent); override;
  public
    property Collate: Boolean read FCollate write FCollate default False;
    property Copies: Integer read FCopies write FCopies default 1;
    property FromPage: Integer read FFromPage write FFromPage default 0;
    property MinPage: Integer read FMinPage write FMinPage default 0;
    property MaxPage: Integer read FMaxPage write FMaxPage default 0;
    property Options: TPrintDialogOptions read FOptions write FOptions default [];
    property PrintToFile: Boolean read FPrintToFile write FPrintToFile default False;
    property PrintRange: TPrintRange read FPrintRange write FPrintRange default prAllPages;
    property ToPage: Integer read FToPage write FToPage default 0;
  end;

{ TTaskDialog }

type
  TCustomTaskDialog = class;

  TTaskDialogFlag = (tfEnableHyperlinks, tfUseHiconMain,
    tfUseHiconFooter, tfAllowDialogCancellation,
    tfUseCommandLinks, tfUseCommandLinksNoIcon,
    tfExpandFooterArea, tfExpandedByDefault,
    tfVerificationFlagChecked, tfShowProgressBar,
    tfShowMarqueeProgressBar, tfCallbackTimer,
    tfPositionRelativeToWindow, tfRtlLayout,
    tfNoDefaultRadioButton, tfCanBeMinimized,
    tfForceNonNative, tfEmulateClassicStyle);
  TTaskDialogFlags = set of TTaskDialogFlag;

  TTaskDialogCommonButton = (tcbOk, tcbYes, tcbNo, tcbCancel, tcbRetry, tcbClose);
  TTaskDialogCommonButtons = set of TTaskDialogCommonButton;

  TTaskDlgClickEvent = procedure(Sender: TObject; AModalResult: TModalResult; var ACanClose: Boolean) of object;

  TTaskDialogIcon = (tdiNone, tdiWarning, tdiError, tdiInformation, tdiShield, tdiQuestion);

  TTaskDialogButtons = class;

  TTaskDialogBaseButtonItem = class(TCollectionItem)
  private
    FCaption: TTranslateString;
    FClient: TCustomTaskDialog;
    FModalResult: TModalResult;
    function GetDefault: Boolean;
    procedure SetCaption(const ACaption: TTranslateString);
    procedure SetDefault(const Value: Boolean);
  protected
    property Client: TCustomTaskDialog read FClient;
    function GetDisplayName: TTranslateString; override;
    function TaskButtonCollection: TTaskDialogButtons;
  public
    constructor Create(ACollection: TCollection); override;
    property ModalResult: TModalResult read FModalResult write FModalResult;
  published
    property Caption: TTranslateString read FCaption write SetCaption;
    property Default: Boolean read GetDefault write SetDefault default False;
  end;

  TTaskDialogButtonItem = class(TTaskDialogBaseButtonItem)
  public
    constructor Create(ACollection: TCollection); override;
  published
    property ModalResult;
  end;

  TTaskDialogRadioButtonItem = class(TTaskDialogBaseButtonItem)
  public
    constructor Create(ACollection: TCollection); override;
  end;

  TTaskDialogButtonsEnumerator = class
  private
    FIndex: Integer;
    FCollection: TTaskDialogButtons;
  public
    constructor Create(ACollection: TTaskDialogButtons);
    function GetCurrent: TTaskDialogBaseButtonItem;
    function MoveNext: Boolean;
    property Current: TTaskDialogBaseButtonItem read GetCurrent;
  end;

  TTaskDialogButtons = class(TOwnedCollection)
  private
    FDefaultButton: TTaskDialogBaseButtonItem;
    function GetItem(Index: Integer): TTaskDialogBaseButtonItem;
    procedure SetDefaultButton(const Value: TTaskDialogBaseButtonItem);
    procedure SetItem(Index: Integer; const Value: TTaskDialogBaseButtonItem);
  public
    function Add: TTaskDialogBaseButtonItem;
    function FindButton(AModalResult: TModalResult): TTaskDialogBaseButtonItem;
    function GetEnumerator: TTaskDialogButtonsEnumerator;
    property DefaultButton: TTaskDialogBaseButtonItem read FDefaultButton write SetDefaultButton;
    property Items[Index: Integer]: TTaskDialogBaseButtonItem read GetItem write SetItem; default;
  end;

  TCustomTaskDialog = class(TComponent)
  private
    FButton: TTaskDialogButtonItem;
    FButtons: TTaskDialogButtons;
    FCaption: TTranslateString;
    FCommonButtons: TTaskDialogCommonButtons;
    FDefaultButton: TTaskDialogCommonButton;
    FExpandButtonCaption: TTranslateString;
    FExpandedText: TTranslateString;
    FFlags: TTaskDialogFlags;
    FFooterIcon: TTaskDialogIcon;
    FFooterText: TTranslateString;
    FMainIcon: TTaskDialogIcon;
    FModalResult: TModalResult;
    FRadioButton: TTaskDialogRadioButtonItem;
    FRadioButtons: TTaskDialogButtons;
    FText: TTranslateString;
    FTitle: TTranslateString;
    FVerificationText: TTranslateString;
    FWidth: Integer;
    FOnButtonClicked: TTaskDlgClickEvent;
    procedure DoOnButtonClickedHandler(Sender: PTaskDialog; AButtonID: Integer;
      var ACanClose: Boolean);
    procedure SetButtons(const Value: TTaskDialogButtons);
    procedure SetRadioButtons(const Value: TTaskDialogButtons);
    function ButtonIDToModalResult(const AButtonID: Integer): TModalResult;
  protected
    function DoExecute(ParentWnd: HWND): Boolean; dynamic;
    procedure DoOnButtonClicked(AModalResult: Integer; var ACanClose: Boolean); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; overload; dynamic;
    function Execute(ParentWnd: HWND): Boolean; overload; dynamic;
    property Button: TTaskDialogButtonItem read FButton write FButton;
    property Buttons: TTaskDialogButtons read FButtons write SetButtons;
    property Caption: TTranslateString read FCaption write FCaption;
    property CommonButtons: TTaskDialogCommonButtons read FCommonButtons write FCommonButtons default [tcbOk, tcbCancel];
    property DefaultButton: TTaskDialogCommonButton read FDefaultButton write FDefaultButton default tcbOk;
    property ExpandButtonCaption: TTranslateString read FExpandButtonCaption write FExpandButtonCaption;
    property ExpandedText: TTranslateString read FExpandedText write FExpandedText;
    property Flags: TTaskDialogFlags read FFlags write FFlags default [tfAllowDialogCancellation];
    property FooterIcon: TTaskDialogIcon read FFooterIcon write FFooterIcon default tdiNone;
    property FooterText: TTranslateString read FFooterText write FFooterText;
    property MainIcon: TTaskDialogIcon read FMainIcon write FMainIcon default tdiInformation;
    property ModalResult: TModalResult read FModalResult write FModalResult;
    property RadioButton: TTaskDialogRadioButtonItem read FRadioButton;
    property RadioButtons: TTaskDialogButtons read FRadioButtons write SetRadioButtons;
    property Text: TTranslateString read FText write FText;
    property Title: TTranslateString read FTitle write FTitle;
    property VerificationText: TTranslateString read FVerificationText write FVerificationText;
    property Width: Integer read FWidth write FWidth default 0;
    property OnButtonClicked: TTaskDlgClickEvent read FOnButtonClicked write FOnButtonClicked;
  end;

  TTaskDialog = class(TCustomTaskDialog)
  published
    property Buttons;
    property Caption;
    property CommonButtons;
    property DefaultButton;
    property ExpandButtonCaption;
    property ExpandedText;
    property Flags;
    property FooterIcon;
    property FooterText;
    property MainIcon;
    property RadioButtons;
    property Text;
    property Title;
    property VerificationText;
    property Width;
    property OnButtonClicked;
  end;


var
  MinimumDialogButtonWidth: Integer = 75;
  MinimumDialogButtonHeight: Integer = 25;

{ MessageDlg }

function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult; overload;
function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint): TModalResult; overload;
function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): TModalResult; overload;
function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): TModalResult; overload;
function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; const HelpKeyword: string): TModalResult; overload;
function MessageDlgPos(const aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): TModalResult; overload;
function MessageDlgPosHelp(const aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
            const HelpFileName: string): TModalResult; overload;
function CreateMessageDialog(const aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons): TForm; overload;
function CreateMessageDialog(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons): TForm; overload;
function DefaultPromptDialog(const DialogCaption,
  DialogMessage: string;
  DialogType: longint; Buttons: PLongint;
  ButtonCount, DefaultIndex, EscapeResult: Longint;
  UseDefaultPos: Boolean;
  X, Y: Longint): Longint;// widgetset independent implementation, see PromptDialogFunction

function QuestionDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: array of const; HelpCtx: Longint): TModalResult; overload;
function QuestionDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: array of const; const HelpKeyword: string): TModalResult; overload;
function DefaultQuestionDialog(const aCaption, aMsg: string; DlgType: LongInt;
  Buttons: TDialogButtons; HelpCtx: Longint): LongInt;// widgetset independent implementation, see QuestionDialogFunction

procedure ShowMessage(const aMsg: string);
procedure ShowMessageFmt(const aMsg: string; Params: array of const);
procedure ShowMessagePos(const aMsg: string; X, Y: Integer);
function DefaultMessageBox(Text, Caption: PChar; Flags: Longint) : Integer;// widgetset independent implementation, see MessageBoxFunction

function InputBox(const ACaption, APrompt, ADefault : string) : string;
function PasswordBox(const ACaption, APrompt : string) : string;

function PromptForFileName(var AFileName: string; const AFilter: string = ''; const ADefaultExt: string = '';
                           const ATitle: string = ''; const AInitialDir: string = ''; AIsSaveDialog: Boolean = False): Boolean;


type
  TCustomCopyToClipboardDialog = class(TForm)
  protected
    procedure DoCreate; override;
  public
    function GetMessageText: string; virtual; abstract;
  end;

procedure RegisterDialogForCopyToClipboard(const ADlg: TCustomForm);
procedure DialogCopyToClipboard(Self, Sender: TObject; var Key: Word; Shift: TShiftState);

const
  cInputQueryEditSizePixels: Integer = 260; // Edit size in pixels
  cInputQueryEditSizePercents: Integer = 25; // Edit size in % of monitor width
  cInputQuerySpacingSize: Integer = 6;

type
  TSelectDirOpt = (sdAllowCreate, sdPerformCreate, sdPrompt);
  TSelectDirOpts = set of TSelectDirOpt;
  TInputCloseQueryEvent = procedure(Sender: TObject; const AValues: array of string;
    var ACanClose: Boolean) of object;

function SelectDirectory(const Caption, InitialDirectory: string;
  out Directory: string): Boolean;
function SelectDirectory(const Caption, InitialDirectory: string;
  out Directory: string; ShowHidden: Boolean; HelpCtx: Longint = 0): Boolean;
function SelectDirectory(out Directory: string;
  Options: TSelectDirOpts; HelpCtx: Longint): Boolean;

function InputQuery(const ACaption, APrompt : string; MaskInput : Boolean; var Value : string) : Boolean;
function InputQuery(const ACaption, APrompt : string; var Value : string) : Boolean;
function InputQuery(const ACaption: string; const APrompts: array of string;
  var AValues: array of string; ACloseEvent: TInputCloseQueryEvent = nil): Boolean;
function DefaultInputDialog(const InputCaption, InputPrompt : string;
  MaskInput : Boolean; var Value : string) : Boolean;// widgetset independent implementation, see InputDialogFunction

function InputCombo(const ACaption, APrompt: string; const AList: TStrings): Integer;
function InputCombo(const ACaption, APrompt: string; const AList : Array of string): Integer;
function InputComboEx(const ACaption, APrompt: string; const AList: TStrings; AllowCustomText: Boolean = False): string;
function InputComboEx(const ACaption, APrompt: string; const AList : Array of string; AllowCustomText: Boolean = False): string;

function ExtractColorIndexAndColor(const AColorList: TStrings; const AIndex: Integer;
  out ColorIndex: Integer; out ColorValue: TColor): Boolean;

// helper functions (search LCLType for idDiag)
function GetDialogCaption(idDiag: Integer): string;
function GetDialogIcon(idDiag: Integer): TCustomBitmap; deprecated 'Use DialogRes.DialogGlyphs';

function dbgs(Option: TOpenOption): string; overload;
function dbgs(Options: TOpenOptions): string; overload;

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

  DialogResult : Array[mrNone..mrLast] of Longint = (
    -1, idButtonOK, idButtonCancel, idButtonAbort, idButtonRetry,
    idButtonIgnore, idButtonYes,idButtonNo, idButtonAll, idButtonNoToAll,
    idButtonYesToAll,idButtonClose);


  DialogButtonKind : Array[idButtonOK..idButtonNoToAll] of TBitBtnKind = (
    bkOk, bkCancel, bkHelp, bkYes, bkNo, bkClose, bkAbort, bkRetry,
    bkIgnore, bkAll, bkYesToAll, bkNoToAll);

  DialogResName: array[idDialogWarning..idDialogShield] of string =
  (
{idDialogWarning} 'dialog_warning',
{idDialogError  } 'dialog_error',
{idDialogInfo   } 'dialog_information',
{idDialogConfirm} 'dialog_confirmation',
{idDialogShield } 'dialog_shield'
  );

type
  TBitBtnAccess = class(TBitBtn);

function dbgs(Option: TOpenOption): string;
begin
  Result:=GetEnumName(typeinfo(TOpenOption),ord(Option));
end;

function dbgs(Options: TOpenOptions): string;
var
  o: TOpenOption;
begin
  Result:='';
  for o in Options do
    Result:=Result+dbgs(o)+',';
  Result:='['+LeftStr(Result,length(Result)-1)+']';
end;

procedure Register;
begin
  RegisterComponents('Dialogs',[TOpenDialog,TSaveDialog,TSelectDirectoryDialog,
                                TColorDialog,TFontDialog,
                                TFindDialog,TReplaceDialog, TTaskDialog]);
  RegisterComponents('Misc',[TColorButton]);
end;

function DefaultMessageBox(Text, Caption: PChar; Flags: Longint) : Integer;
var
  DlgType : TMsgDlgType;
  Buttons : TMsgDlgButtons;
  CurBtn, DefButton: TMsgDlgBtn;
  DefButtonIndex: Integer;
begin
  //This uses TMessageBox class in MessageDialogs.inc
  if (Flags and MB_RETRYCANCEL) = MB_RETRYCANCEL then
    Buttons := [mbRetry, mbCancel]
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
  //else
  //if (Flags and MB_OK) = MB_OK then  <-- MB_OK = 0, the test would always be true.
  //  Buttons := [mbOK]
  else
    Buttons := [mbOK];

  if (Flags and MB_ICONINFORMATION) = MB_ICONINFORMATION then
    DlgTYpe := mtInformation
  else
  if (Flags and MB_ICONWARNING) = MB_ICONWARNING then
    DlgTYpe := mtWarning
  else
  if (Flags and MB_ICONQUESTION) = MB_ICONQUESTION then
    DlgTYpe := mtConfirmation
  else
  if (Flags and MB_ICONERROR) = MB_ICONERROR then
    DlgTYpe := mtError
  else
    DlgTYpe := mtCustom;

  if (Flags and MB_DEFBUTTON2) = MB_DEFBUTTON2 then
    DefButtonIndex := 2 else
  if (Flags and MB_DEFBUTTON3) = MB_DEFBUTTON3 then
    DefButtonIndex := 3 else
  if (Flags and MB_DEFBUTTON4) = MB_DEFBUTTON4 then
    DefButtonIndex := 4 else
    DefButtonIndex := 1;

  DefButton := Low(TMsgDlgBtn);
  for CurBtn := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
  begin
    DefButton := CurBtn;
    if CurBtn in Buttons then
      Dec(DefButtonIndex);
    if DefButtonIndex = 0 then
      break;
  end;
  Result := MessageDlg(Caption, Text, DlgType, Buttons, 0, DefButton);
end;

{** Return the localized or not title of dialog}
function GetDialogCaption(idDiag: Integer): string;
begin
  case idDiag of
    idDialogWarning : Result := rsMtWarning;
    idDialogError   : Result := rsMtError;
    idDialogInfo    : Result := rsMtInformation;
    idDialogConfirm : Result := rsMtConfirmation;
    idDialogShield  : Result := rsMtAuthentication;
  else
    Result := '?';
  end;
end;

function GetDialogIcon(idDiag: Integer): TCustomBitmap;
var
  BitmapHandle, MaskHandle: HBitmap;
begin
  if ThemeServices.GetStockImage(idDiag, BitmapHandle, MaskHandle) then
  begin
    Result := TBitmap.Create;
    Result.Handle := BitmapHandle;
    if MaskHandle <> 0 then
      Result.MaskHandle := MaskHandle;
  end
  else
  if (idDiag < Low(DialogResName)) or (idDiag > High(DialogResName)) then
    Result := nil
  else
  begin
    Result := TPortableNetworkGraphic.Create;
    Result.LoadFromResourceName(hInstance, DialogResName[idDiag]);
  end;
end;

{$I lclcolordialog.inc}
{$I commondialog.inc}
{$I filedialog.inc}
{$I finddialog.inc}
{$I replacedialog.inc}
{$I fontdialog.inc}
{$I inputdialog.inc}
{$I messagedialogs.inc}
{$I promptdialog.inc}
{$I colorbutton.inc}
{$I taskdialog.inc}

{ TCustomPrintDialog }

constructor TCustomPrintDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPrintRange:=prAllPages;
  FCopies:=1;
end;

{ TCustomCopyToClipboardDialog }

procedure TCustomCopyToClipboardDialog.DoCreate;
begin
  inherited DoCreate;

  RegisterDialogForCopyToClipboard(Self);
end;

initialization
  Forms.MessageBoxFunction := @DefaultMessageBox;
  InterfaceBase.InputDialogFunction := @DefaultInputDialog;
  InterfaceBase.PromptDialogFunction := @DefaultPromptDialog;
  InterfaceBase.QuestionDialogFunction := @DefaultQuestionDialog;

finalization
  InterfaceBase.InputDialogFunction := nil;
  InterfaceBase.QuestionDialogFunction := nil;

end.
