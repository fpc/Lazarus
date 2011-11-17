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
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit Dialogs;

{$mode objfpc}{$H+}

interface

uses
  Types, Classes, LResources, SysUtils, LCLIntf, InterfaceBase, FileUtil,
  LCLStrConsts, LCLType, LCLProc, Forms, Controls, Themes,
  GraphType, Graphics, Buttons, ButtonPanel, StdCtrls, ExtCtrls, LCLClasses;


type
  TMsgDlgType    = (mtWarning, mtError, mtInformation, mtConfirmation,
                    mtCustom);
  TMsgDlgBtn     = (mbYes, mbNo, mbOK, mbCancel, mbAbort, mbRetry, mbIgnore,
                    mbAll, mbNoToAll, mbYesToAll, mbHelp, mbClose);
  TMsgDlgButtons = set of TMsgDlgBtn;

   
const
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

  TCommonDialog = class(TLCLComponent)
  private
    FHandle : THandle;
    FHeight: integer;
    FWidth: integer;
    FOnCanClose: TCloseQueryEvent;
    FOnShow, FOnClose : TNotifyEvent;
    FTitle : string;
    FUserChoice: integer;
    FHelpContext: THelpContext;
    FCanCloseCalled: Boolean;
    FClosing: boolean;
    procedure SetHandle(const AValue: THandle);
    procedure SetHeight(const AValue: integer);
    procedure SetWidth(const AValue: integer);
    function IsTitleStored: boolean;
  protected
    class procedure WSRegisterClass; override;
    function DoExecute : boolean; virtual;
    function DefaultTitle: string; virtual;
  public
    FCompStyle : LongInt;
    constructor Create(TheOwner: TComponent); override;
    function Execute: boolean; virtual;
    property Handle: THandle read FHandle write SetHandle;
    property UserChoice: integer read FUserChoice write FUserChoice;
    procedure Close; virtual;
    procedure DoShow; virtual;
    procedure DoCanClose(var CanClose: Boolean); virtual;
    procedure DoClose; virtual;
    function HandleAllocated: boolean;
  published
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnCanClose: TCloseQueryEvent read FOnCanClose write FOnCanClose;
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property HelpContext: THelpContext read FHelpContext write FHelpContext default 0;
    property Width: integer read FWidth write SetWidth default 0;
    property Height: integer read FHeight write SetHeight default 0;
    property Title: TTranslateString read FTitle write FTitle stored IsTitleStored;
  end;


  { TFileDialog }
  
  TFileDialog = class(TCommonDialog)
  private
    FInternalFilterIndex: Integer;
    FDefaultExt: string;
    FFileName : String;
    FFiles: TStrings;
    FFilter: String;
    FFilterIndex: Integer;
    FHistoryList: TStrings;
    FInitialDir: string;
    FOnHelpClicked: TNotifyEvent;
    FOnTypeChange: TNotifyEvent;
    procedure SetDefaultExt(const AValue: string);
    procedure SetFilterIndex(const AValue: Integer);
  protected
    class procedure WSRegisterClass; override;
    function DoExecute: boolean; override;
    function GetFilterIndex: Integer; virtual;
    procedure SetFileName(const Value: String); virtual;
    procedure SetFilter(const Value: String); virtual;
    procedure SetHistoryList(const AValue: TStrings); virtual;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoTypeChange; virtual;
    function Execute: boolean; override;
    property Files: TStrings read FFiles;
    property HistoryList: TStrings read FHistoryList write SetHistoryList;
    procedure IntfFileTypeChanged(NewFilterIndex: Integer);
    class function FindMaskInFilter(aFilter, aMask: string): integer;
    class function ExtractAllFilterMasks(aFilter: string;
                                   SkipAllFilesMask: boolean = true): string;
  published
    property Title;
    property DefaultExt: string read FDefaultExt write SetDefaultExt;
    property FileName: String read FFileName write SetFileName;
    property Filter: String read FFilter write SetFilter;
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
    class procedure WSRegisterClass; override;
    procedure DereferenceLinks; virtual;
    function CheckFile(var AFilename: string): boolean; virtual;
    function CheckFileMustExist(const AFileName: string): boolean; virtual;
    function CheckAllFiles: boolean; virtual;
    function DoExecute: boolean; override;
    function DefaultTitle: string; override;
  public
    constructor Create(TheOwner: TComponent); override;
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
    function CheckFileMustExist(const AFilename: string): boolean; override;
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
    function IsButtonColorAutoSizeStored: boolean;
    procedure SetBorderWidth(const AValue: Integer);
    procedure SetButtonColor(const AValue: TColor);
    procedure SetButtonColorAutoSize(const AValue: Boolean);
    procedure SetButtonColorSize(const AValue: Integer);
  protected
    class procedure WSRegisterClass; override;
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
      AState: TButtonState; ATransparent: Boolean; BiDiFlags: Longint): TRect; override;
    function GetDisabledPattern: TBitmap; virtual;
    function GetGlyphSize(Drawing: boolean; PaintRect: TRect): TSize; override;
    class function GetControlClassDefaultSize: TSize; override;
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
    property OnMouseMove;
    property OnMouseUp;
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
                 frEntireScope, frHideEntireScope, frPromptOnReplace, frHidePromptOnReplace);
  TFindOptions = set of TFindOption;

  TFindDialog = class(TCommonDialog)
  private
    FFormLeft: integer;
    FFormTop: integer;
    function GetReplaceText: string;
    function GetFindText: string;
    function GetLeft: Integer;
    function GetPosition: TPoint;
    function GetTop: Integer;
    procedure SetFindText(const AValue: string);
    procedure SetLeft(const AValue: Integer);
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

    procedure UpdatePosition;
    procedure DoCloseForm(Sender: TObject; var CloseAction: TCloseAction);virtual;
    procedure Find; virtual;
    procedure Help; virtual;
    procedure Replace; virtual;
    function CreateForm:TForm;virtual;
    procedure SetFormValues;virtual;

    procedure GetFormValues; virtual;

    property ReplaceText: string read GetReplaceText write SetReplaceText;
    property OnReplace: TNotifyEvent read FOnReplace write FOnReplace;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CloseDialog;
    function Execute: Boolean;override;
    property Left: Integer read GetLeft write SetLeft;
    property Position: TPoint read GetPosition write SetPosition;
    property Top: Integer read GetTop write SetTop;
  published
    property FindText: string read GetFindText write SetFindText;
    property Options: TFindOptions read FOptions write FOptions default [frDown];
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
    poHelp, poDisablePrintToFile);
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

var
  MinimumDialogButtonWidth: integer = 75;
  MinimumDialogButtonHeight: integer = 25;

{ MessageDlg }

function MessageDlg(const aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint): Integer;
function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint; DefaultButton: TMsgDlgBtn): Integer;
function MessageDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; const HelpKeyword: string): Integer;
function MessageDlgPos(const aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer): Integer;
function MessageDlgPosHelp(const aMsg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons; HelpCtx: Longint; X, Y: Integer;
            const HelpFileName: string): Integer;
function CreateMessageDialog(const Msg: string; DlgType: TMsgDlgType;
            Buttons: TMsgDlgButtons): TForm;
function QuestionDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: array of const; HelpCtx: Longint): TModalResult;
function QuestionDlg(const aCaption, aMsg: string; DlgType: TMsgDlgType;
            Buttons: array of const; const HelpKeyword: string): TModalResult;

            
procedure ShowMessage(const aMsg: string);
procedure ShowMessageFmt(const aMsg: string; Params: array of const);
procedure ShowMessagePos(const aMsg: string; X, Y: Integer);

function InputQuery(const ACaption, APrompt : String; MaskInput : Boolean; var Value : String) : Boolean;
function InputQuery(const ACaption, APrompt : String; var Value : String) : Boolean;
function InputBox(const ACaption, APrompt, ADefault : String) : String;
function PasswordBox(const ACaption, APrompt : String) : String;
  
type
  TSelectDirOpt = (sdAllowCreate, sdPerformCreate, sdPrompt);
  TSelectDirOpts = set of TSelectDirOpt;

function SelectDirectory(const Caption, InitialDirectory: string;
  out Directory: string): boolean;
function SelectDirectory(const Caption, InitialDirectory: string;
  out Directory: string; ShowHidden: boolean; HelpCtx: Longint = 0): boolean;
function SelectDirectory(out Directory: string;
  Options: TSelectDirOpts; HelpCtx: Longint): Boolean;

function ExtractColorIndexAndColor(const AColorList: TStrings; const AIndex: Integer;
  out ColorIndex: Integer; out ColorValue: TColor): Boolean;

// helper functions (search LCLType for idDiag)
function GetDialogCaption(idDiag: Integer): String;
function GetDialogIcon(idDiag: Integer): TCustomBitmap;

procedure Register;

implementation

{$ifndef ver2_2}
{$R forms/finddlgunit.lfm}
{$R forms/replacedlgunit.lfm}
{$ENDIF}

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

  DialogResult : Array[mrNone..mrYesToAll] of Longint = (
    -1, idButtonOK, idButtonCancel, idButtonAbort, idButtonRetry,
    idButtonIgnore, idButtonYes,idButtonNo, idButtonAll, idButtonNoToAll,
    idButtonYesToAll);

  DialogButtonKind : Array[idButtonOK..idButtonNoToAll] of TBitBtnKind = (
    bkOk, bkCancel, bkHelp, bkYes, bkNo, bkClose, bkAbort, bkRetry,
    bkIgnore, bkAll, bkYesToAll, bkNoToAll);

  DialogResName: array[idDialogWarning..idDialogConfirm] of String =
  (
{idDialogWarning} 'dialog_warning',
{idDialogError  } 'dialog_error',
{idDialogInfo   } 'dialog_information',
{idDialogConfirm} 'dialog_confirmation'
  );

type
  TBitBtnAccess = class(TBitBtn);

procedure Register;
begin
  RegisterComponents('Dialogs',[TOpenDialog,TSaveDialog,TSelectDirectoryDialog,
                                TColorDialog,TFontDialog,
                                TFindDialog,TReplaceDialog]);
  RegisterComponents('Misc',[TColorButton]);
end;

function ShowMessageBox(Text, Caption: PChar; Flags: Longint) : Integer;
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
  else
  if (Flags and MB_OK) = MB_OK then
    Buttons := [mbOK]
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
function GetDialogCaption(idDiag: Integer): String;
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
    Result := CreateBitmapFromLazarusResource(DialogResName[idDiag]);
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

{ TCustomPrintDialog }

constructor TCustomPrintDialog.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FPrintRange:=prAllPages;
  FCopies:=1;
end;

initialization
  Forms.MessageBoxFunction := @ShowMessageBox;
  InterfaceBase.InputDialogFunction := @ShowInputDialog;
  InterfaceBase.PromptDialogFunction := @ShowPromptDialog;
  InterfaceBase.QuestionDialogFunction := @ShowQuestionDialog;
  {$I dialog_icons.lrs}

finalization
  InterfaceBase.InputDialogFunction := nil;
  InterfaceBase.QuestionDialogFunction := nil;

end.
