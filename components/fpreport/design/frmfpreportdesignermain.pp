{
    This file is part of the Free Component Library.
    Copyright (c) 2017 Michael Van Canneyt, member of the Free Pascal development team

    Main Report designer Form.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit frmfpreportdesignermain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, fpreportdata,
  Menus, ActnList, ComCtrls, ExtCtrls, IniPropStorage, fpreport, fpreportdesignctrl, contnrs,
  fraReportObjectInspector, fpreportdesignreportdata, frafpreportdata, mrumanager;

type
  // If you add something here, do not forget to add to AllReportDesignOptions.
  TFPReportDesignOption = (rdoManageData,      // Allow user to manage report data
                           rdoManageVariables, // Allow user to manage report variables
                           rdoAllowLoad,       // Allow user to load new reports (open)
                           rdoAllowSave,       // Allow user to save reports (open)
                           rdoAllowProperties, // Allow user to save
                           rdoAllowPageAdd,    // Allow user to add pages
                           rdoAllowNew,        // Allow user to start new report
                           rdoAllowPreview,    // Allow user to ask report preview
                           rdoAllowBands,      // Allow user to add/remove bands
                           rdoAllowFileDrop,   // Allow user to drop files on designer, so they will be loaded.
                           rdoAllowImport      // Allow import of other formats (needs rdoAllowLoad as well)
                           );
  TFPReportDesignOptions = set of TFPReportDesignOption;

  // What to do when selection contains a report page
  TPageCopyAction = (pcaNone,pcaAbort,pcaReplace,pcaAdd);
  TPageCopyActions = set of TPageCopyAction;

  // What to do when selection contains a band that cannot be correctly added to the current page.
  TBandCopyAction = (bcaNone,bcaAbort,bcaConvertToChild);
  TBandCopyActions = Set of TBandCopyAction;

  { TPageTabSheet }

  TPageTabSheet = class(TTabSheet)
  private
    FDesigner: TFPReportDesignerControl;
    FPage: TFPReportCustomPage;
  Public
    Property Page : TFPReportCustomPage Read FPage;
    Property Designer : TFPReportDesignerControl Read FDesigner;
  end;
  { TFPReportDesignerForm }

  TFPReportDesignerForm = class(TForm)
    AAddMemo: TAction;
    AAddShape: TAction;
    AAddCheckBox: TAction;
    AAddImage: TAction;
    AAddGroupHeader: TAction;
    AAddGroupFooter: TAction;
    AAddPageHeader: TAction;
    AAddPageFooter: TAction;
    AAddReportTitle: TAction;
    AAddReportSummary: TAction;
    AAddColumnHeader: TAction;
    AAddColumnFooter: TAction;
    AAddChildBand: TAction;
    AAddDataHeader: TAction;
    AAddDataFooter: TAction;
    AAddDataBand: TAction;
    AAddPage: TAction;
    AAlignleft: TAction;
    AAlignRight: TAction;
    AAlignHCenter: TAction;
    AAlignTop: TAction;
    AAlignVCenter: TAction;
    AAlignBottom: TAction;
    AAlign: TAction;
    ACopy: TAction;
    ABringToFront: TAction;
    AImportLazreport: TAction;
    ASendToBack: TAction;
    AEditElement: TAction;
    AFileOpenNewWindow: TAction;
    ANewNewWindow: TAction;
    ACut: TAction;
    APaste: TAction;
    AResizeBandToFit: TAction;
    AFileSaveAs: TAction;
    ARecent: TAction;
    AReportData: TAction;
    APreview: TAction;
    AReportVariables: TAction;
    AReportProperties: TAction;
    ADelete: TAction;
    AFrameAll: TAction;
    AFrameClear: TAction;
    AFrameRight: TAction;
    AFrameLeft: TAction;
    AFrameTop: TAction;
    AFrameBottom: TAction;
    AResize: TAction;
    AResizeVLargest: TAction;
    AResizeVSmallest: TAction;
    AResizeHLargest: TAction;
    AResizeHSmallest: TAction;
    ANew: TAction;
    AQuit: TAction;
    AFileOpen: TAction;
    AFileSave: TAction;
    ALReport: TActionList;
    ILReport: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MIImportLazReport: TMenuItem;
    MIBringToFront: TMenuItem;
    MISendToBack: TMenuItem;
    MIEditElement: TMenuItem;
    MenuItem5: TMenuItem;
    MINewNewWindow: TMenuItem;
    MIFileOpenNewWindow: TMenuItem;
    MICopy: TMenuItem;
    MICut: TMenuItem;
    MIPaste: TMenuItem;
    MISaveAs: TMenuItem;
    MIPreview: TMenuItem;
    MIframeBottom: TMenuItem;
    MIFrameTop: TMenuItem;
    MIFrameRight: TMenuItem;
    MIFrameLeft: TMenuItem;
    MIFrameClear: TMenuItem;
    MIFrameAll: TMenuItem;
    MFrames: TMenuItem;
    MIRecent: TMenuItem;
    MIAddPage: TMenuItem;
    ODImport: TOpenDialog;
    PMRecent: TPopupMenu;
    PSDesign: TIniPropStorage;
    MIAlign: TMenuItem;
    MIAddColumnHeader: TMenuItem;
    MIAddColumnFooter: TMenuItem;
    MIAddPageHeader: TMenuItem;
    MIAddPageFooter: TMenuItem;
    MIAddReportTitle: TMenuItem;
    MIAddReportSummary: TMenuItem;
    PMIAddDataBand: TMenuItem;
    PMIAddDataHeader: TMenuItem;
    PMIAddDataFooter: TMenuItem;
    PMIAddGroupHeader: TMenuItem;
    PMIAddGroupFooter: TMenuItem;
    PMIAddColumnHeader: TMenuItem;
    PMIColumnFooter: TMenuItem;
    PMIAddPageHeader: TMenuItem;
    PMIAddPageFooter: TMenuItem;
    PMIAddReportSummary: TMenuItem;
    PMIAddReportTitle: TMenuItem;
    PMIAddMemo: TMenuItem;
    PMIAddShape: TMenuItem;
    PMIAddCheckBox: TMenuItem;
    PMIAddImage: TMenuItem;
    MAlign: TMenuItem;
    MIAlignLeft: TMenuItem;
    MIAlignHCentered: TMenuItem;
    MIAlignRight: TMenuItem;
    MIAlignTop: TMenuItem;
    MIAlignVCenter: TMenuItem;
    MIAlignBottom: TMenuItem;
    MEdit: TMenuItem;
    MIReportProperties: TMenuItem;
    MIReportVariables: TMenuItem;
    MReport: TMenuItem;
    MIDelete: TMenuItem;
    MIResize: TMenuItem;
    MIResizeHLargest: TMenuItem;
    MIResizeHSmallest: TMenuItem;
    MIResizeVLargest: TMenuItem;
    MIResizeVSmallest: TMenuItem;
    MResize: TMenuItem;
    MIPMAddChild: TMenuItem;
    MIAddDataFooter: TMenuItem;
    MIAddDataHeader: TMenuItem;
    MIAddGroupHeader: TMenuItem;
    MIAddGroupFooter: TMenuItem;
    MIAddDataFooterBand: TMenuItem;
    MIAddChildBand: TMenuItem;
    MIBands: TMenuItem;
    MMReport: TMainMenu;
    MFile: TMenuItem;
    MAdd: TMenuItem;
    MIAddShape: TMenuItem;
    MISaveReport: TMenuItem;
    MIAddCheckbox: TMenuItem;
    MIAddImage: TMenuItem;
    MISep: TMenuItem;
    MIQuit: TMenuItem;
    miAddMemo: TMenuItem;
    miOpenReport: TMenuItem;
    FOI: TObjectInspectorFrame;
    ODReport: TOpenDialog;
    PCReport: TPageControl;
    PMAdd: TPopupMenu;
    PMAddBand: TPopupMenu;
    FReportData: TReportDataDisplay;
    SDReport: TSaveDialog;
    SObjectInspector: TSplitter;
    SBReport: TStatusBar;
    Splitter1: TSplitter;
    TBAdd: TToolButton;
    TBAddBand: TToolButton;
    ToolBar1: TToolBar;
    TBNEwReport: TToolButton;
    TBAddPage: TToolButton;
    TBPReview: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    TBAlignLeft: TToolButton;
    TBAlignHCenter: TToolButton;
    TBAlignRight: TToolButton;
    ToolButton16: TToolButton;
    TBAlignTop: TToolButton;
    TBAlignVCenter: TToolButton;
    TBAlignBottoms: TToolButton;
    TBOpenReport: TToolButton;
    TBResize: TToolButton;
    TBResizeHSmallest: TToolButton;
    TBAlign: TToolButton;
    TBSaveReport: TToolButton;
    TBResizeVLargest: TToolButton;
    TBResizeVSmallest: TToolButton;
    ToolButton24: TToolButton;
    TBFrameAll: TToolButton;
    TBFrameNone: TToolButton;
    TBFrameLeft: TToolButton;
    TBFrameRight: TToolButton;
    TBFrameTop: TToolButton;
    ToolButton3: TToolButton;
    TBFrameBottom: TToolButton;
    ToolButton4: TToolButton;
    TBAddMemo: TToolButton;
    TBAddCheckbox: TToolButton;
    TBAddShape: TToolButton;
    TBAddImage: TToolButton;
    TBExit: TToolButton;
    TBResizeHLargest: TToolButton;
    TSDesign: TTabSheet;
    procedure AAddCheckBoxExecute(Sender: TObject);
    procedure AAddElementUpdate(Sender: TObject);
    procedure AAddImageExecute(Sender: TObject);
    procedure AAddMemoExecute(Sender: TObject);
    procedure AAddPageExecute(Sender: TObject);
    procedure AAddBandExecute(Sender: TObject);
    procedure AAddBandUpdate(Sender: TObject);
    procedure AAddPageUpdate(Sender: TObject);
    procedure AAddShapeExecute(Sender: TObject);
    procedure AAlignExecute(Sender: TObject);
    procedure AAlignUpdate(Sender: TObject);
    procedure ABringToFrontExecute(Sender: TObject);
    procedure ACopyExecute(Sender: TObject);
    procedure ACopyUpdate(Sender: TObject);
    procedure ACutExecute(Sender: TObject);
    procedure ACutUpdate(Sender: TObject);
    procedure ADeleteExecute(Sender: TObject);
    procedure ADeleteUpdate(Sender: TObject);
    procedure AEditElementExecute(Sender: TObject);
    procedure AEditElementUpdate(Sender: TObject);
    procedure AFileOpenNewWindowExecute(Sender: TObject);
    procedure AFileSaveAsExecute(Sender: TObject);
    procedure AFileSaveAsUpdate(Sender: TObject);
    procedure AFileSaveUpdate(Sender: TObject);
    procedure AFrameExecute(Sender: TObject);
    procedure AFrameUpdate(Sender: TObject);
    procedure AImportLazreportExecute(Sender: TObject);
    procedure AImportLazreportUpdate(Sender: TObject);
    procedure ANewExecute(Sender: TObject);
    procedure ANewNewWindowExecute(Sender: TObject);
    procedure APasteExecute(Sender: TObject);
    procedure APasteUpdate(Sender: TObject);
    procedure APreviewExecute(Sender: TObject);
    procedure APreviewUpdate(Sender: TObject);
    procedure AReportDataExecute(Sender: TObject);
    procedure AReportDataUpdate(Sender: TObject);
    procedure AReportPropertiesExecute(Sender: TObject);
    procedure AReportPropertiesUpdate(Sender: TObject);
    procedure AReportVariablesExecute(Sender: TObject);
    procedure AReportVariablesUpdate(Sender: TObject);
    procedure AResizeBandToFitExecute(Sender: TObject);
    procedure AResizeBandToFitUpdate(Sender: TObject);
    procedure AResizeExecute(Sender: TObject);
    procedure ASendToBackExecute(Sender: TObject);
    procedure ASendToBackFrontUpdate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure PCReportChange(Sender: TObject);
    procedure VResizeAllow(Sender: TObject);
    procedure HResizeExecute(Sender: TObject);
    procedure AResizeUpdate(Sender: TObject);
    procedure HResizeAllow(Sender: TObject);
    procedure VAlignUpdate(Sender: TObject);
    procedure AFileSaveExecute(Sender: TObject);
    procedure AQuitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure HAlignExecute(Sender: TObject);
    procedure HAlignUpdate(Sender: TObject);
    procedure AFileOpenExecute(Sender: TObject);
    procedure VAlignExecute(Sender: TObject);
    procedure VResizeExecute(Sender: TObject);
  private
    FCustomStartIndex : Integer;
    FInitialFileName: String;
    FLoadModified : Boolean;
    FStopDesigning: Boolean;
    MRUMenuManager1: TMRUMenuManager;
    FAutoSaveOnClose: Boolean;
    FDesignOptions: TFPReportDesignOptions;
    FFileName: String;
    FModified: Boolean;
    FOnNewReport: TNotifyEvent;
    FOnOpenReport: TNotifyEvent;
    FOnSaveReport: TNotifyEvent;
    FReportDesignData : TDesignReportDataManager;
    FImportForm: TForm;
{$IFDEF USEDEMOREPORT}
    lReportData : TFPReportUserData;
    sl: TStringList;
{$ENDIF}
    FReport: TFPReport;
    FDataParent : TComponent;
{$IFDEF USEDEMOREPORT}
    procedure CreateDemoReport;
    procedure GetReportDataFirst(Sender: TObject);
    procedure GetReportDataValue(Sender: TObject; const AValueName: String; var AValue: Variant);
    procedure GetReportDataEOF(Sender: TObject; var IsEOF: Boolean);
    procedure GetReportDataNames(Sender: TObject; List: TStrings);
    procedure InitialiseData;
{$ENDIF}
    procedure AddCustomElementExecute(Sender: TObject);
    procedure AddCustomElements;
    procedure CheckLoadInitialFile;
    function CreateDesignPopupMenu(aOWner: TComponent): TPopupMenu;
    function CreateNewPage: TFPReportCustomPage;
    procedure DoImportLog(Sender: TOBject; const Msg: String);
    procedure DoPaste(Sender: TObject);
    procedure DoReportChangedByDesigner(Sender: TObject);
    procedure DoSelectionModifiedByOI(Sender: TObject);
    procedure DoStructureChange(Sender: TObject);
    procedure ExecutePaste(aControl: TFPReportDesignerControl);
    procedure GetCopyActions(aControl: TFPReportDesignerControl; L: TFPObjectList; out PCA: TPageCopyAction; out
      BCA: TBandCopyAction);
    function GetModified: boolean;
    procedure ActivateDesignerForElement(AElement: TFPReportElement);
    function GetPageCopyAction(aCount: Integer): TPageCopyAction;
    function GetBandCopyAction(aCount: Integer): TBandCopyAction;
    procedure ImportLazReport;
    procedure MaybeAddFirstPage;
    procedure OpenInNewWindow(aFileName: string);
    procedure PasteBand(aControl: TFPReportDesignerControl; aAction: TBandCopyAction; var aBand: TFPReportCustomBand);
    procedure PasteElement(aControl: TFPReportDesignerControl; aBand: TFPReportCustomBand; aElement: TFPReportElement);
    procedure PasteList(aControl: TFPReportDesignerControl; L: TFPObjectList);
    function PastePage(aAction: TPageCopyAction; aPage: TFPReportCustomPage): TFPReportDesignerControl;
    procedure ResetReport;
    procedure SetBandActionTags;
    procedure SetDesignOptions(AValue: TFPReportDesignOptions);
    procedure SetFileCaption(const AFileName: String);
    procedure SetModified(AValue: Boolean);
    procedure SetModifiedStatus;
    procedure SetPageCaption(ASheet: TTabSheet);
  Protected
    procedure MRUMenuManager1RecentFile(Sender: TObject; const AFileName: String);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ApplyDesignOptions; virtual;
    function AddPageDesign(APage: TFPReportCustomPage): TTabSheet;
    Function FindTabForReportPage(APage : TFPReportCustomPage) : TPageTabSheet;
    procedure DoSelectComponent(Sender: TObject; Selected: TComponent); virtual;
    procedure DoSelectionChanged(Sender: TObject); virtual;
    procedure DoStateChange(Sender: TObject); virtual;
    function CheckSaved(aAction: String): boolean;
    function CurrentDesigner: TFPReportDesignerControl;
    procedure ResetObjectInspector; virtual;
    procedure LoadDesignFromFile(const AFilename: string); virtual;
    procedure SaveDesignToFile(AFileName: string); virtual;
    procedure CreateReportData; virtual;
    procedure CreateReportDataSets(Errors : TStrings); virtual;
    procedure SetReport(AValue: TFPReport); virtual;
    procedure ShowReportData; virtual;
    Function DesignerCount : Integer;
    function PageDesigner(Aindex : Integer) : TFPReportDesignerControl;
    Property Modified : Boolean Read GetModified Write SetModified;
  public
  Public
    procedure ResetModified; virtual;
    procedure DesignReport; virtual;
    procedure StopDesigning; virtual;
    procedure PreviewReport; virtual;
    procedure LoadReportFromFile(const AFileName: String);
    function NewReport: Boolean; virtual;
    Function SaveReport : Boolean; virtual;
    function OpenReport: Boolean; virtual;
    function ValidateReport: Boolean;
    procedure DoElementCreated(Sender: TObject; AElement: TFPReportElement);
    Property Report : TFPReport Read FReport Write SetReport;
    Property FileName : String Read FFileName Write FFileName;
    Property ReportDesignData : TDesignReportDataManager Read FReportDesignData;
    Property DesignOptions : TFPReportDesignOptions Read FDesignOptions Write SetDesignOptions;
    // If these are set, they override the default handling. You must set the modified
    Property OnSaveReport : TNotifyEvent Read FOnSaveReport Write FOnSaveReport;
    Property OnNewReport : TNotifyEvent Read FOnNewReport Write FOnNewReport;
    Property OnOpenReport : TNotifyEvent Read FOnOpenReport Write FOnOpenReport;
    Property AutoSaveOnClose : Boolean Read FAutoSaveOnClose Write FAutoSaveOnClose;
    Property InitialFileName : String Read FInitialFileName Write FInitialFileName;
  end;
  TFPReportDesignerFormClass = Class of TFPReportDesignerForm;

Const
  AllReportDesignOptions = [rdoManageData,rdoManageVariables,rdoAllowLoad,rdoAllowSave,rdoAllowProperties,
                            rdoAllowPageAdd,rdoAllowNew, rdoAllowPreview, rdoAllowBands,rdoAllowFileDrop,rdoAllowImport];


implementation

uses
  reportdesignbaseforms,
  fpreportdesignobjectlist,
  fpreportformexport, // form export has it's own factory pattern.
  fpttf,
  fpreportstreamer,
  fpjson,
  fplazreport,
  Clipbrd,
  jsonparser;

{$R *.lfm}

ResourceString
  SCaption = 'FPReport Designer Demo';
  SModified = 'Report design modified.';
  SSaveChanges = 'The report is modified. Would you like to save the changes?';
  SClose = 'Discard changes';
  SSaveAnyway = 'Save the report';
  SAbort = 'Do not %s';
  SNewReport = 'create new report';
  SOpenReport = 'open other report';
  SCloseDesigner = 'close designer';
  SNoSelection = 'No selection';
  SStatusModified = 'Modified';
  SErrAccessingData = 'Error accessing data for report';
  SErrAccessingDataDetails = 'One or more report data sources failed to open:'+
                             sLineBreak+'%s'+sLineBreak+
                             'You will need to fix these errors before proceeding.';
  SErrInvalidReport = 'Invalid report design';
  SErrFixErrors = 'The report design contains %d errors:'+sLineBreak+'%s'+sLineBreak+
                  'You will need to fix these errors before proceeding.';
  SErrCannotCopyClipboardFormat = 'Clipboard contents cannot be pasted into report.';
  SCopyingPage = 'Pasting report page';
  SCopyingPageWhat = 'The selection contains a complete report page.'+sLineBreak+
                     'What do you want to do with this page?';
  SCopyingPages = 'Pasting multiple report pages';
  SCopyingPagesWhat = 'The selection contains multiple complete report pages.'+sLineBreak+
                     'What do you want to do with these pages?';
  SCopyingBand = 'Pasting conflicting band';
  SCopyingBandWhat = 'The selection contains a band which cannot be logically placed on the current page.'+sLineBreak+
                     'What do you want to do with this band?';
  SCopyingBands = 'Pasting multiple conflicting bands';
  SCopyingBandsWhat = 'The selection contains multiple bands which cannot be logically placed on the current page.'+sLineBreak+
                      'What do you want to do with these bands?';
  SNoCopy = 'Do not copy';
  SPageAdd = 'Add as new page';
  SPageReplace = 'Replace current page';
  SAbortCopy = 'Abort copy';
  sBandConvert = 'Convert to child band';
  sBandConverts = 'Convert to child bands';
  SErrNoBandToPaste = 'No band to paste elements on';

Const
  StateNames : Array[TDesignerState] of string = ('','Resetting',
                'Selecting','Rectangle select','Extending rectangle selection',
                'Adding element','Adding multiple elements',
                'Adding element','Adding multiple elements',
                'Moving','Resizing');

{ TFPReportDesignerForm }

procedure TFPReportDesignerForm.SetBandActionTags;

 Procedure S(A : TAction; T : TFPReportBandType);

 begin
   A.Tag:=Ord(T);
 end;

begin
  S(AAddGroupHeader,btGroupHeader);
  S(AAddGroupFooter,btGroupFooter);
  S(AAddPageHeader,btPageHeader);
  S(AAddPageFooter,btPageFooter);
  S(AAddReportTitle,btReportTitle);
  S(AAddReportSummary,btReportSummary);
  S(AAddColumnHeader,btColumnHeader);
  S(AAddColumnFooter,btColumnFooter);
  S(AAddChildBand,btChild);
  S(AAddDataHeader,btDataHeader);
  S(AAddDataFooter,btDataFooter);
  S(AAddDataBand,btDataband);
end;

procedure TFPReportDesignerForm.FormCreate(Sender: TObject);

var
  F : Text;
  i : Integer;

begin
  DesignOptions:=AllReportDesignOptions;
  if PaperManager.PaperCount=0 then
    PaperManager.RegisterStandardSizes;
  if gTTFontCache.SearchPath.Count=0 then
    begin
    {$IFDEF UNIX}
    gTTFontCache.SearchPath.Add(ExtractFilePath(ParamStr(0))+'../demos/fonts/');
    gTTFontCache.SearchPath.Add(GetUserDir + '.fonts/');
    gTTFontCache.SearchPath.Add('/usr/share/fonts/truetype/ubuntu-font-family/');
    gTTFontCache.SearchPath.Add('/usr/share/fonts/truetype/dejavu/');
    {$ENDIF}
    end;
  if (gTTFontCache.Count=0) then
    gTTFontCache.ReadStandardFonts;
  {$IFNDEF WINDOWS}
  AssignFile(F,'/tmp/fonts.txt');
  Rewrite(F);
  For I:=0 to gTTFontCache.Count-1 do
     Writeln(F,I,' ',gTTFontCache.Items[i].PostScriptName,' : ',gTTFontCache.Items[i].FamilyName,' : ',gTTFontCache.Items[i].HumanFriendlyName);
  CloseFile(F);
  {$ENDIF}
  FDataParent:=TComponent.Create(nil);
  FreeAndNil(TSDesign); // Remove design-time added page
  FReportDesignData:=TDesignReportDataManager.Create(Self);
  SetBandActionTags;
  FCustomStartIndex:=Madd.Count;
  AddCustomElements;
  // DEMO
{$IFDEF USEDEMOREPORT}
  CreateDemoReport;
  CreateReportData;
{$ENDIF}
// END OF DEMO
  SetFileCaption('');
  FOI.OnSelectElement:=@DoSelectComponent;
  FOI.OnModified:=@DoSelectionModifiedByOI;
  MRUMenuManager1 := TMRUMenuManager.Create(self);
  with MRUMenuManager1 do
    begin
    IniFileName := ChangeFileExt(ParamStr(0), '.ini');
    MenuItem := MIRecent;
    PopupMenu := PMRecent;
    MaxItemLength := 80;
    MenuCaptionMask := '(%d) %s';
    OnRecentFile := @MRUMenuManager1RecentFile;
    LoadRecentFilesFromIni;
    maxRecent := 15;
    end;
end;

Type
  { TAddElementAction }

  TAddElementAction  = Class(TAction)
  private
    FClass: TFPReportElementClass;
  published
    Property AClass : TFPReportElementClass Read FClass Write FClass;
  end;

procedure TFPReportDesignerForm.AddCustomElements;

  Function AllowClass(M : TFPReportClassMapping) : Boolean;

  begin
    Result:= not M.ReportElementClass.InheritsFrom(TFPReportCustomBand)
             and not M.ReportElementClass.InheritsFrom(TFPReportCustomPage);
    if Result then
      Result:=Not M.Standard;
  end;

Var
  I : Integer;
  M : TFPReportClassMapping;
  MI : TMenuItem;
  A : TAddElementAction;
  Img : TPortableNetworkGraphic;
  S : TMemoryStream;

begin
  for I:=0 to gElementFactory.MappingCount-1 do
    begin
    M:=gElementFactory.Mappings[i];
    if AllowClass(M) then
      begin
      A:=TAddElementAction.Create(Self);
      A.Caption:=M.MappingName;
      A.AClass:=M.ReportElementClass;
      A.OnUpdate:=@AAddElementUpdate;
      A.OnExecute:=@AddCustomElementExecute;
      A.Category:='Add';
      A.ActionList:=ALReport;
      if (Length(M.IconData)>0) then
        begin
        Img:=Nil;
        S:=TMemoryStream.Create;
        try
          S.WriteBuffer(M.IconData[0],Length(M.IConData));
          S.Position:=0;
          Img:=TPortableNetworkGraphic.Create;
          Img.LoadFromStream(S);
          A.ImageIndex:=ILReport.Add(Img,Nil);
        Finally
          S.Free;
          Img.Free;
        end;
        end;
      MI:=TMenuItem.Create(Self);
      MI.Action:=A;
      MAdd.Add(MI);
      end;
    end;
end;

procedure TFPReportDesignerForm.AddCustomElementExecute(Sender: TObject);

begin
  if (Sender is TAddElementAction) then
    CurrentDesigner.AddElement((Sender as TAddElementAction).AClass);
end;

procedure TFPReportDesignerForm.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  MRUMenuManager1.SaveRecentFilesToIni;
  if FAutoSaveOnClose then
    begin
    SaveReport;
    CanClose:=True;
    end
  else
    CanClose:=CheckSaved(SCloseDesigner);
end;

procedure TFPReportDesignerForm.FormDestroy(Sender: TObject);
begin
{$IFDEF USEDEMOREPORT}
  FreeAndNil(SL);
{$ENDIF}
  if Assigned(FReport) and (FReport.Owner=Self) then
    FreeAndNil(FReport);
  FreeAndNil(FDataParent);
  FreeAndNil(FReportDesignData);
end;

procedure TFPReportDesignerForm.FormDropFiles(Sender: TObject; const FileNames: array of String);

Var
  I : Integer;

begin
  if not (rdoAllowFileDrop in DesignOptions) then exit;
  if Length(FileNames)<1 then exit;
  if not CheckSaved(SOpenReport) then
    exit;
  StopDesigning;
  LoadReportFromFile(FileNames[0]);
  DesignReport;

  For I:=1 to Length(FileNames)-1 do
    OpenInNewWindow(Filenames[i]);
end;

procedure TFPReportDesignerForm.OpenInNewWindow(aFileName : string);

Var
  C : TFPReportDesignerFormClass;
  F : TFPReportDesignerForm;

begin
  C:=TFPReportDesignerFormClass(Self.Classtype);
  F:=C.Create(Application);
  F.InitialFileName:=aFileName;
  F.Show;
end;

procedure TFPReportDesignerForm.FormShow(Sender: TObject);

begin
  if rdoAllowFileDrop in DesignOptions then
    AllowDropFiles:=True;
  CheckLoadInitialFile;
  SBReport.Refresh;
end;

procedure TFPReportDesignerForm.CheckLoadInitialFile;

Var
  FN : String;

begin
  if (InitialFileName<>'') then
    begin
    FN:=InitialFileName;
    InitialFileName:='';
    if FileExists(FN) then
      begin
      LoadReportFromFile(FN);
      DesignReport;
      end;
    end;
end;

procedure TFPReportDesignerForm.DesignReport;

Var
  I : Integer;

begin
  MaybeAddFirstPage;
  Report.StartDesigning;
  For I:=0 to Report.PageCount-1 do
    AddPageDesign(Report.Pages[I]);
  ShowReportData;
  ResetObjectInspector;
  if FLoadModified then
    begin
    Modified:=True;
    FLoadModified:=false;
    end
  else
    Modified:=False;
end;

procedure TFPReportDesignerForm.CreateReportData;

  Procedure MaybeAddData(D : TFPReportData);

  begin
    if D=nil then exit;
    If FReport.ReportData.FindReportDataItem(D)=Nil then
      FReport.ReportData.AddReportData(D);
  end;

Var
  I,J : Integer;
  P : TFPReportCustomPage;

begin
  FReport.ReportData.Clear;
  For I:=0 to FReport.PageCount-1 do
    begin
    P:=FReport.Pages[I];
    MaybeAddData(P.Data);
    for J:=0 to P.BandCount-1 do
      if P.Bands[J] is TFPReportCustomDataBand then
        begin
        MaybeAddData(TFPReportCustomDataBand(P.Bands[J]).Data);
        end;
    end;
end;

procedure TFPReportDesignerForm.ShowReportData;

begin
  FReportData.Report:=FReport;
end;


procedure TFPReportDesignerForm.ActivateDesignerForElement(AElement : TFPReportElement);

Var
  TS : TPageTabSheet;

begin
  While (AElement<>Nil) and Not (AElement.InheritsFrom(TFPReportCustomPage)) do
    AElement:=AElement.Parent;
  if AElement=Nil then exit;
  TS:=FindTabForReportPage(AElement as TFPReportCustomPage);
  if Assigned(TS) then
    PCReport.ActivePage:=TS;
end;

function TFPReportDesignerForm.FindTabForReportPage(APage: TFPReportCustomPage
  ): TPageTabSheet;

Var
  TS : TTabSheet;
  I : Integer;
begin
  I:=0;
  Result:=Nil;
  While (Result=Nil) and (I<PCReport.PageCount) do
    begin
    TS:=PCReport.Pages[I];
    if (TS is TPageTabSheet) and (TPageTabSheet(TS).Page=APage) then
      Result:=TS As TPageTabSheet;
    Inc(I);
    end;
end;

procedure TFPReportDesignerForm.DoSelectComponent(Sender: TObject;
  Selected: TComponent);
begin
  if (Selected is TFPReport) then
    CurrentDesigner.Objects.ClearSelection
  else if Selected is TFPReportElement then
    begin
    ActivateDesignerForElement(Selected as TFPReportElement);
    CurrentDesigner.Objects.SelectElement(Selected as TFPReportElement)
    end
  else
    CurrentDesigner.Objects.ClearSelection
end;

procedure TFPReportDesignerForm.DoSelectionChanged(Sender: TObject);

Var
  S : String;
  D : TFPReportDesignerControl;

begin
  D:=Sender as TFPReportDesignerControl;
  if (D=CurrentDesigner) then
    FOI.UpdateSelection
  else
    FOI.SelectControls(D.Objects);
  if D.Objects.HaveSelection then
    S:=D.Objects.GetSelectionRect.AsString
  else
    S:=SNoSelection;
  SBReport.Panels[1].Text:=S;
end;

procedure TFPReportDesignerForm.DoStateChange(Sender: TObject);
begin
  If (Sender<>CurrentDesigner) then
    exit;
  SBreport.Panels[2].text:=StateNames[CurrentDesigner.DesignerState];
end;

Procedure TFPReportDesignerForm.SetPageCaption(ASheet : TTabSheet);

Var
  TS : TPageTabSheet;
  PageNo : Integer;

begin
  if Not (Asheet is TPageTabSheet) then
    exit;
  TS:=ASheet as TPageTabSheet;
  PageNo:=TS.TabIndex+1;
  if (TS.Page.Name<>'') then
    TS.Caption:=Format('Page %d (%s)',[PageNo,TS.Page.Name])
  else
    TS.Caption:=Format('Page %d',[PageNo]);
end;


function TFPReportDesignerForm.AddPageDesign(APage: TFPReportCustomPage): TTabSheet;

Var
  TS : TPageTabSheet;
  SB : TScrollBox;
  D : TFPReportDesignerControl;

begin
  TS:=TPageTabSheet.Create(Self);
  TS.FPage:=APage;
  TS.Parent:=PCReport;
  SetPageCaption(TS);
  D:=TFPReportDesignerControl.Create(Self);
  SB:=TScrollBox.Create(TS);
  SB.Parent:=TS;
  SB.Align:=alClient;
  TS.FDesigner:=D;
  D.Parent:=SB;
  //  FDesign.Align:=alClient;
  //  fdesign.SetBounds(0,0,ClientWidth,ClientHeight);
  D.Top:=0;
  D.Left:=0;
  D.Page:=APage;
  D.OnElementCreated:=@DoElementCreated;
  D.OnSelectionChanged:=@DoSelectionChanged;
  D.OnStateChange:=@DoStateChange;
  D.OnReportChanged:=@DoReportChangedByDesigner;
  D.Objects.OnStructureChange:=@DoStructureChange;
  D.OnPaste:=@DoPaste;
  D.Objects[0].Selected:=True;
  D.PopupMenu:=CreateDesignPopupMenu(D);
  Result:=TS;
end;

Function TFPReportDesignerForm.CreateDesignPopupMenu(aOWner : TComponent) : TPopupMenu;

Var
  PM : TPopupMenu;

  Function AddAction(A : TAction) : TMenuItem;

  begin
    Result:=TMenuItem.Create(PM);
    Result.Action:=A;
    PM.Items.Add(Result);
  end;

  Function AddSep : TMenuItem;

  begin
    Result:=TMenuItem.Create(PM);
    Result.Caption:='-';
    PM.Items.Add(Result);
  end;

begin
  PM:=TPopupMenu.Create(aOwner);
  AddAction(AEditElement);
  AddSep;
  AddAction(ACopy);
  AddAction(ACut);
  AddAction(APaste);
  AddSep;
  AddAction(ABringToFront);
  AddAction(ASendToBack);
  AddSep;
  AddAction(AResize);
  AddAction(AAlign);
  Result:=PM;
end;

procedure TFPReportDesignerForm.SetFileCaption(const AFileName: String);

Var
  S : String;

begin
  if AFileName='' then
    S:=SCaption+' [new file]'
  else
    S:=SCaption+' ['+AFileName+']';
  if Modified then
    S:='*'+S;
  Caption:=S;
end;

procedure TFPReportDesignerForm.SetModified(AValue: Boolean);

Var
  I : integer;

begin
  FModified:=AVAlue;
  if not Avalue then
    For I:=0 to DesignerCount-1 do
       PageDesigner(i).Objects.ResetModified;
  SetModifiedStatus;
end;

procedure TFPReportDesignerForm.SetModifiedStatus;

begin
  SetFileCaption(FileName);
  if GetModified then
    SBReport.Panels[0].Text:=SStatusModified
  else
    SBReport.Panels[0].Text:='';
end;

procedure TFPReportDesignerForm.MRUMenuManager1RecentFile(Sender: TObject;
  const AFileName: String);
begin
  if Not CheckSaved(SOpenReport) then
    Exit;
  if Assigned(OnOpenReport) then
    begin
    StopDesigning;
    OnOpenReport(Self)
    end
  else
    begin
      LoadDesignFromFile(AFileName);
      SetFileCaption(AFileName);
    end;
  DesignReport;
end;

procedure TFPReportDesignerForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) then
    begin
    if (AComponent=FReport) then
      begin
      FReport:=nil;
      if not (csDestroying in ComponentState) then
        StopDesigning;
      end;
    end;
end;

procedure TFPReportDesignerForm.SetReport(AValue: TFPReport);
begin
  if FReport=AValue then Exit;
  FReport:=AValue;
  DesignReport;
end;

function TFPReportDesignerForm.CurrentDesigner: TFPReportDesignerControl;

begin
  if PCReport.ActivePage<>Nil then
    Result:=PageDesigner(PCReport.ActivePageIndex)
  else
    Result:=Nil;
end;

function TFPReportDesignerForm.DesignerCount: Integer;
begin
  Result:=PCReport.PageCount;
end;

function TFPReportDesignerForm.PageDesigner(Aindex: Integer
  ): TFPReportDesignerControl;
begin
  if (AIndex>=0) and (AIndex<DesignerCount) then
    Result:=(PCReport.Pages[AIndex] as TPageTabSheet).Designer
  else
    Result:=Nil;
end;

procedure TFPReportDesignerForm.ResetModified;
begin
  Modified:=False;
end;

procedure TFPReportDesignerForm.AAddMemoExecute(Sender: TObject);
begin
  CurrentDesigner.AddElement(TFPReportMemo);
end;

Function TFPReportDesignerForm.CreateNewPage: TFPReportCustomPage;

begin
  Result:=gElementFactory.PageClass.Create(FReport);
  Result.PageSize.PaperName := 'A4';
  { page margins }
  Result.Margins.Left := 30;
  Result.Margins.Top := 20;
  Result.Margins.Right := 30;
  Result.Margins.Bottom := 20;
  Result.StartDesigning;
end;

procedure TFPReportDesignerForm.DoImportLog(Sender: TOBject; const Msg: String);
begin
  if not Assigned(FImportForm) and Assigned(ReportImportFormClass) then
     begin
     FImportForm:=ReportImportFormClass.Create(Self);
     FImportForm.Show;
     end;
  if Assigned(FImportForm) and (FImportForm is TBaseImportReportForm)  then
    TBaseImportReportForm(FImportForm).Log(Msg);
end;

Function TFPReportDesignerForm.GetPageCopyAction(aCount : Integer) : TPageCopyAction;

Var
  MR : TModalResult;

begin
  if aCount=1 then
    MR:=QuestionDlg(SCopyingPage,SCopyingPageWhat,mtWarning,[
         mrIgnore,SNoCopy,
         mrYes,SPageAdd,
         mrRetry,sPageReplace,
         mrAbort,sAbortCopy],'')
  else
    MR:=QuestionDlg(SCopyingPages,SCopyingPagesWhat,mtWarning,[
         mrIgnore,SNoCopy,
         mrYes,SPageAdd,
         mrAbort,sAbortCopy],'');
  case MR of
     mrIgnore : Result:=pcaNone;
     mrYes: Result:=pcaAdd;
     mrRetry : Result:=pcaReplace;
     mrAbort :Result:=pcaAbort;
   else
     Result := pcaAbort
   end;
end;

function TFPReportDesignerForm.GetBandCopyAction(aCount: Integer): TBandCopyAction;

Var
  MR : TModalResult;
  BC,C,M : String;

begin
  if aCount=1 then
    begin
    C:=SCopyingBand;
    M:=SCopyingBandWhat;
    BC:=sBandConvert;
    end
  else
    begin
    C:=SCopyingBands;
    M:=SCopyingBandsWhat;
    BC:=sBandConverts;
    end;
  MR:=QuestionDlg(C,M,mtWarning,[
       mrIgnore,SNoCopy,
       mrYes,BC,
       mrAbort,sAbortCopy],'');
  case MR of
     mrIgnore : Result:=bcaNone;
     mrYes: Result:=bcaConvertToChild;
     mrAbort :Result:=bcaAbort;
   else
     Result := bcaAbort
   end;
end;



procedure TFPReportDesignerForm.ExecutePaste(aControl : TFPReportDesignerControl);

Var
  L : TFPObjectList;
  S : TMemoryStream;

begin
  aControl.CheckClipBoardFormat;
  if Not ClipBoard.HasFormat(ClipBoardFormat) then
     Raise EReportError.Create(SErrCannotCopyClipboardFormat);
  L:=Nil;
  S:=Nil;
  try
    S:=TMemoryStream.Create;
    ClipBoard.GetFormat(ClipBoardFormat,S);
    S.Position:=0;
    L:=FReport.StreamToReportElements(S);
    PasteList(aControl,L);
  finally
    FreeAndNil(L);
    FreeAndNil(S);
  end;
end;

procedure TFPReportDesignerForm.GetCopyActions(aControl : TFPReportDesignerControl; L : TFPObjectList;
                                               Out PCA : TPageCopyAction;
                                               out  BCA : TBandCopyAction);

Var
  i,pCount : Integer;

begin
  PCA:=pcaNone;
  bca:=bcaNone;
  pCount:=0;
  for I:=0 to L.Count-1 do
    if L[i] is TFPReportCustomPage then
      inc(pCount);
  if pCount>0 then
    PCA:=GetPageCopyAction(pCount);
  if (PCA=pcaAbort) then
    exit;
  pCount:=0;
  for I:=0 to L.Count-1 do
    if L[i] is TFPReportCustomBand then
      if Not aControl.Page.CheckBandMultiplicity(TFPReportCustomBand(L[i])) then
        Inc(pCount);
  if pCount>0 then
    BCA:=GetBandCopyAction(pCount)
end;

procedure TFPReportDesignerForm.PasteList(aControl : TFPReportDesignerControl; L : TFPObjectList);

Var
  L2 : TFPList;
  i : Integer;
  PCA : TPageCopyAction;
  BCA : TBandCopyAction;
  E : TFPReportElement;
  B : TFPReportCustomBand;
  cControl : TFPReportDesignerControl;
  NeedReorder : Boolean;

begin
  GetCopyActions(aControl,l,PCA,BCA);
  if (PCA=pcaAbort) or (BCA=bcaAbort) then
    exit;
  cControl:=aControl;
  // First the pages (current page may be changed by this)
  For I:=0 to L.Count-1 do
    if L[I] is TFPReportCustomPage then
      begin
      E:=TFPReportCustomPage(L.Extract(L[i]));
      cControl:=PastePage(PCA,(E as TFPReportCustomPage));
      end;
  if L.Count=0 then
    exit;
  // .. and paste the rest.
  NeedReorder:=False;
  L2:=TFPList.Create; // List to contain pasted elements
  try
    B:=aControl.GetBandForPaste;
    While (L.Count>0) do
      begin
      E:=TFPReportElement(L.Extract(L[0]));
      if E is TFPReportCustomBand then
        begin
        PasteBand(cControl,BCA,TFPReportCustomBand(E));
        NeedReorder:=NeedReorder or Assigned(E);
        // If there was not a band, use the just pasted one, if there is one.
        if B=Nil then
          B:=aControl.GetBandForPaste;
        end
      else if (E is TFPReportElement) then
        PasteElement(cControl,B,E)
      else
        FreeAndNil(E);
      if Assigned(E) then
        L2.Add(E);
      end;
    // Set selection to pasted objects. Pages will not be selected by this
    cControl.Objects.ClearSelection;
    For I:=0 to L2.Count-1 do
      cControl.Objects.SelectElement(TFPReportElement(L2[i]));
    if NeedReorder then
      cControl.Objects.OrderBands(cControl.Canvas,CurrentDesigner.CurrentDPI);
    DoStructureChange(Self);
  finally
    FreeAndNil(L2);
  end;
end;


Function TFPReportDesignerForm.PastePage(aAction : TPageCopyAction; aPage : TFPReportCustomPage) : TFPReportDesignerControl;

Var
  Idx : Integer;
  oldPage : TFPReportCustomPage;

begin
  Idx:=CurrentDesigner.Page.PageIndex;
  FReport.AddPage(aPage);
  Case aAction of
  pcaAdd:
    begin
    Result:=TPageTabSheet(AddPageDesign(aPage)).Designer;
    end;
  pcaReplace:
    begin
    oldPage:=CurrentDesigner.Page;
    idx:=OldPage.PageIndex;
    CurrentDesigner.Page:=aPage;
    FReport.RemovePage(oldPage);
    aPage.PageIndex:=idx;
    end;
  end;
end;

Procedure TFPReportDesignerForm.PasteBand(aControl : TFPReportDesignerControl; aAction : TBandCopyAction; var aBand : TFPReportCustomBand);

Var
  C : TFPReportCustomChildBand;
  I : Integer;
  N : String;

begin
  if Not aControl.Page.CheckBandMultiplicity(aBand) then
    Case aAction of
      bcaNone : FreeAndNil(aBand);
      bcaConvertToChild :
        begin
        C:=TFPReportCustomChildBand(gElementFactory.CreateInstance('ChildBand',aControl.Page.Report));
        N:=aBand.Name;
        // Copy properties
        aBand.ChildBand:=Nil;
        C.Assign(aBand);
        // Copy elements
        For I:=aBand.ChildCount-1 downto 0 do
          aBand.Child[i].Parent:=C;
        // Replace
        FreeAndNil(aBand);
        aBand:=C;
        aBand.Name:=N;
        end;
    end;
  // Paste into page.
  if Assigned(aBand) then
    begin
    aBand.Parent:=aControl.Page;
    aControl.Objects.AddBand(aBand);
    end;
end;

Procedure TFPReportDesignerForm.PasteElement(aControl : TFPReportDesignerControl; aBand : TFPReportCustomBand; aElement : TFPReportElement);

Const
  xShift = 2.0;
  yShift = 2.0;

begin
  if (ABand=Nil) then
    Raise EReportError.Create(SErrNoBandToPaste);
  aElement.Parent:=aBand;
  aElement.Layout.Left:=aElement.Layout.Left+xShift;
  if aElement.Layout.Left>aBand.Layout.Width then
    begin
    aElement.Layout.Left:=aBand.Layout.Width-aElement.Layout.Width;
    if aElement.Layout.Left<0 then
      aElement.Layout.Left:=0;
    end;
  aElement.Layout.Top:=aElement.Layout.Top+yShift;
  if aElement.Layout.top>aBand.Layout.Height then
    begin
    aElement.Layout.top:=aBand.Layout.Height-aElement.Layout.Height;
    if aElement.Layout.top<0 then
      aElement.Layout.top:=0;
    end;
  aControl.Objects.AddElement(aElement);
end;

procedure TFPReportDesignerForm.DoPaste(Sender: TObject);
begin
  ExecutePaste(Sender as TFPReportDesignerControl);
end;

procedure TFPReportDesignerForm.AAddPageExecute(Sender: TObject);

Var
  P : TFPReportCustomPage;

begin
  P:=CreateNewPage;
  FReport.AddPage(P);
  P.Name:='Page'+IntToStr(FReport.PageCount);
  FOI.RefreshReportTree;
  PCReport.ActivePage:=AddPageDesign(P);
  Modified:=True;
end;

procedure TFPReportDesignerForm.AAddBandExecute(Sender: TObject);

Var
  T : Integer;

begin
  T:=(sender as Taction).Tag;
  if (T<0) or (T>Ord(High(TFPReportBandType))) then
    exit;
  if (CurrentDesigner=Nil) then
    Exit;
  CurrentDesigner.AddBand(gElementFactory.BandClasses[TFPReportBandType(T)]);
end;

procedure TFPReportDesignerForm.AAddBandUpdate(Sender: TObject);
Var
  T : Integer;
  TOK : Boolean;
begin

  T:=(sender as Taction).Tag;
  // Check valid tag
  TOK:=Assigned(CurrentDesigner) and Not ((T<0) or (T>Ord(High(TFPReportBandType)))) ;
  // need to improve this to check that the type of band is actually allowed.
  (Sender as TAction).Enabled:=(rdoAllowBands in DesignOptions) and TOK;
end;

procedure TFPReportDesignerForm.AAddPageUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(FReport);
end;

procedure TFPReportDesignerForm.AAddCheckBoxExecute(Sender: TObject);
begin
  CurrentDesigner.AddElement(TFPReportCheckbox);
end;

procedure TFPReportDesignerForm.AAddElementUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(Freport) and Assigned(CurrentDesigner)
end;

procedure TFPReportDesignerForm.AAddImageExecute(Sender: TObject);
begin
  CurrentDesigner.AddElement(TFPReportImage);
end;

procedure TFPReportDesignerForm.AAddShapeExecute(Sender: TObject);
begin
  CurrentDesigner.AddElement(TFPReportShape);
end;

procedure TFPReportDesignerForm.AAlignExecute(Sender: TObject);

Var
  F : TBaseReportAlignForm;

begin
  if (ReportAlignFormClass=nil) then exit;
  F:=ReportAlignFormClass.Create(Self);
  try
    F.Report:=Self.Report;
    if F.ShowModal=mrOK then
      CurrentDesigner.Objects.AlignSelection(F.Horizontal,F.Vertical);
  finally
    F.Free;
  end;
end;

procedure TFPReportDesignerForm.AAlignUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled:=Assigned(ReportAlignFormClass)
                               and Assigned(CurrentDesigner)
                               and CurrentDesigner.Objects.HaveSelection
end;

procedure TFPReportDesignerForm.ABringToFrontExecute(Sender: TObject);
begin
  CurrentDesigner.Objects.BringToFront;
end;

procedure TFPReportDesignerForm.ACopyExecute(Sender: TObject);
begin
  if Assigned(CurrentDesigner) then
    CurrentDesigner.CopySelectionToClipBoard;
end;

procedure TFPReportDesignerForm.ACopyUpdate(Sender: TObject);
begin
  (Sender As Taction).Enabled:=Assigned(CurrentDesigner) and CurrentDesigner.Objects.HaveSelection;
end;

procedure TFPReportDesignerForm.ACutExecute(Sender: TObject);
begin
  if Assigned(CurrentDesigner) then
    begin
    CurrentDesigner.CopySelectionToClipBoard;
    CurrentDesigner.Objects.DeleteSelection;
    end;
end;

procedure TFPReportDesignerForm.ACutUpdate(Sender: TObject);
begin
  (Sender As Taction).Enabled:=Assigned(CurrentDesigner) and CurrentDesigner.Objects.HaveSelection;
end;

procedure TFPReportDesignerForm.ADeleteExecute(Sender: TObject);

Var
  ODR : TObjectDeleteResult;

begin
  ODR:=CurrentDesigner.Objects.DeleteSelection;
  if (ODR=odrPage) then
    PCReport.ActivePage.Free;
  FOI.RefreshReportTree;
end;

procedure TFPReportDesignerForm.ADeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentDesigner) and CurrentDesigner.Objects.HaveSelection;
end;

procedure TFPReportDesignerForm.AEditElementExecute(Sender: TObject);
begin
  CurrentDesigner.ShowEditorForElement(CurrentDesigner.Objects.GetSelection[0].Element);
end;

procedure TFPReportDesignerForm.AEditElementUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(CurrentDesigner)
                               and (CurrentDesigner.Objects.SelectionCount=1)
                               and (CurrentDesigner.Objects.GetSelection[0].IsPlainElement);
end;

procedure TFPReportDesignerForm.AFileOpenNewWindowExecute(Sender: TObject);

Var
  O : TOpenOptions;
  I : integer;

begin
  With ODReport do
    try
      O:=Options;
      Include(O,ofAllowMultiSelect);
      Options:=O;
      If Execute then
        For I:=0 to ODReport.Files.Count-1 do
          OpenInNewWindow(ODReport.Files[i]);
    finally
      Exclude(O,ofAllowMultiSelect);
      Options:=O;
    end;
end;

procedure TFPReportDesignerForm.AFileSaveAsExecute(Sender: TObject);

Var
  FN : String;

begin
  FN:=FileName;
  FileName:='';
  if Not SaveReport then
    FileName:=FN;
end;

procedure TFPReportDesignerForm.AFileSaveAsUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(Report);
end;

procedure TFPReportDesignerForm.AFileSaveUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(Report) and Modified;
end;

procedure TFPReportDesignerForm.AFrameExecute(Sender: TObject);

Var
  A : TFrameAction;

begin
  A:=TFrameAction((Sender as Taction).Tag);
  CurrentDesigner.Objects.FrameSelection([A],A in [faNone,faAll]);
end;

procedure TFPReportDesignerForm.AFrameUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentDesigner) and CurrentDesigner.Objects.HaveSelection;
end;

procedure TFPReportDesignerForm.AImportLazreportExecute(Sender: TObject);
begin
  if CheckSaved('import lazreport') then
    ImportLazReport;
end;

procedure TFPReportDesignerForm.AImportLazreportUpdate(Sender: TObject);

Const
   Ops = [rdoAllowImport,rdoAllowLoad];

begin
  (Sender as TAction).Enabled:=(Ops * DesignOptions)=Ops;
end;

Procedure TFPReportDesignerForm.ImportLazReport;

Var
  FN,OFN : String;
  R : TFPLazReport;
  S : TFPReportJSONStreamer;
  J : TJSONStringType;

begin
  With ODImport do
    If Execute then
      FN:=FileName
    else
      exit;
  OFN:=ChangeFileExt(FN,'.json');
  R:=TFPLazReport.Create(Self);
  try
    // Reset.
    FImportForm:=Nil;
    R.OnLog:=@DoImportLog;
    R.LoadFromFile(FN);
    S:=TFPReportJSONStreamer.Create(Self);
    R.WriteElement(S,Nil);
    S.JSON.Add('DesignData',TJSONObject.Create);
    J:=S.JSON.FormatJSON( );
    With TFileStream.Create(OFN,fmCreate) do
      try
        WriteBuffer(J[1],Length(J));
      finally
        Free;
      end;
  finally
    R.Free;
  end;
  LoadReportFromFile(OFN);
  DesignReport;
end;

function TFPReportDesignerForm.GetModified: boolean;

Var
  i : Integer;

begin
  Result:=Assigned(FReport);
  If not Result then exit;
  Result:=FModified;
  if Result then exit;
  Result:=True;
  I:=0;
  While Result and (I<DesignerCount) do
    begin
    Result:=PageDesigner(I).Objects.Modified;
    Inc(I);
    end;
end;

procedure TFPReportDesignerForm.DoSelectionModifiedByOI(Sender: TObject);

Var
  Sel : TReportObjectArray;

begin
  if Assigned(CurrentDesigner) then
    begin
    if (FOI.ObjectList.SelectionCount=1) then
      begin
      Sel:=FOI.ObjectList.GetSelection;
      if (Sel[0].IsPage) and (Sel[0].AsPage=CurrentDesigner.Page) then
        begin
        CurrentDesigner.UpdatePageParams;
        CurrentDesigner.Reset;
        CurrentDesigner.Objects.SelectElement(CurrentDesigner.Page);
        SetPageCaption(PCReport.ActivePage);
        end
      else
        CurrentDesigner.Invalidate;
      end;
    end;
  Modified:=True;
end;

procedure TFPReportDesignerForm.DoStructureChange(Sender: TObject);
begin
  FOI.RefreshReportTree;
end;

procedure TFPReportDesignerForm.DoReportChangedByDesigner(Sender: TObject);
begin
  FOI.RefreshOI;
  Modified:=True;
end;

function TFPReportDesignerForm.SaveReport: Boolean;
begin
  if Assigned(FReport) and FReport.Prepared then
    FReport.ClearPreparedReport;
  Result:=Assigned(OnSaveReport);
  if result then
    OnSaveReport(Self)
  else
    begin
    Result:=(FileName<>'');
    if not Result then
      if SDReport.Execute then
        begin
        FileName:=SDReport.FileName;
        Result:=(FileName<>'');
        end;
    if Result then
      begin
      SaveDesignToFile(FileName);
      if Assigned(MRUMenuManager1) then
        begin
        MRUMenuManager1.AddToRecent(FileName);
        MRUMenuManager1.SaveRecentFilesToIni;
        end;
      end;
    end;
end;

function TFPReportDesignerForm.CheckSaved(aAction: String): boolean;

begin
  Result:=Not Modified;
  If Not Result then
    Case QuestionDlg(SModified,SSaveChanges,mtWarning,[mrIgnore,sClose,mrYes,sSaveAnyway,mrAbort,Format(SAbort,[aAction])],'') of
      mrIgnore : Result:=True;
      mrYes : Result:=SaveReport;
      mrAbort : Result:=False
    else
      Result:=False
    end;
end;

procedure TFPReportDesignerForm.ANewExecute(Sender: TObject);


begin
  if not CheckSaved (SNewReport) then
    exit;
  if NewReport then
    DesignReport;
end;

procedure TFPReportDesignerForm.ANewNewWindowExecute(Sender: TObject);

Var
  C : TFPReportDesignerFormClass;
  F : TFPReportDesignerForm;

begin
  C:=TFPReportDesignerFormClass(Self.Classtype);
  F:=C.Create(Application);
  if F.NewReport then
    begin
    F.DesignReport;
    F.Show;
    end;
end;

procedure TFPReportDesignerForm.APasteExecute(Sender: TObject);
begin
  DoPaste(CurrentDesigner);
end;

procedure TFPReportDesignerForm.APasteUpdate(Sender: TObject);
begin
  TFPReportDesignerControl.CheckClipBoardFormat;
  (Sender as TAction).Enabled:=ClipBoard.HasFormat(ClipBoardFormat);
end;

Procedure TFPReportDesignerForm.MaybeAddFirstPage;

Var
  P : TFPReportCustomPage;

begin
  if (FReport.PageCount=0) then
    begin
    p:=CreateNewPage;
    FReport.AddPage(P);
    P.Name:='Page'+IntToStr(FReport.PageCount);
    end;
end;

function TFPReportDesignerForm.NewReport: Boolean;


begin
  result:=Assigned(OnNewReport);
  if Result then
    OnNewReport(Self)
  else
    begin
    StopDesigning;
    ResetReport;
    MaybeAddFirstPage;
    Report.StartDesigning;
    FOI.RefreshReportTree;
    Result:=True;
    end;
  if Result then
    FFileName:='';
end;

procedure TFPReportDesignerForm.APreviewExecute(Sender: TObject);
begin
  PreviewReport;
end;

Function TFPReportDesignerForm.ValidateReport : Boolean;

Var
  errs : TStrings;

begin
  errs:=TStringList.Create;
  try
    Report.Validate(errs);
    Result:=Errs.Count=0;
    if Not Result then
      MessageDlg(SErrInvalidReport,Format(SErrFixErrors,[Errs.Count,Errs.Text]),mtError,[mbOK],'');
  finally
    errs.Free;
  end;
end;

procedure TFPReportDesignerForm.PreviewReport;

Var
  F : TFPreportPreviewExport;

begin
  if not ValidateReport then
    exit;
  FReportDesignData.StartRender;
  try
    FReport.RunReport;
    F:=TFPreportPreviewExport.Create(Self);
    FReport.RenderReport(F);
    FReport.ClearPreparedReport;
  finally
    FReportDesignData.EndRender;
  end;
end;

procedure TFPReportDesignerForm.APreviewUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(FReport) and (FReport.PageCount>0) and (FReport.Pages[0].BandCount>0);
end;

procedure TFPReportDesignerForm.AReportDataExecute(Sender: TObject);

Var
  F : TBaseReportDataForm;

begin
  if not Assigned(ReportDataFormClass) then
    exit;
  Self.Report.SaveDataToNames;
  F:=ReportDataFormClass.Create(Self);
  try

    F.Report:=Self.Report;
    F.Data:=FReportDesignData.DataDefinitions;
    if F.ShowModal=mrOK then
      begin
      FReportDesignData.RemoveFromReport(FReport);
      FReportDesignData.DataDefinitions:=F.Data;
      CreateReportDataSets(Nil);
      Modified:=True;
      end;
  finally
     F.Free;
  end;
end;

procedure TFPReportDesignerForm.AReportDataUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(FReport);
end;

procedure TFPReportDesignerForm.CreateReportDataSets(Errors: TStrings);


begin
  if (rdoManageData in DesignOptions) then
    FReportDesignData.ApplyToReport(FReport,Errors);
  FReport.RestoreDataFromNames;
  FReportData.Report:=FReport;
  FReportData.RefreshData;
end;

procedure TFPReportDesignerForm.AReportPropertiesExecute(Sender: TObject);

Var
  F : TBaseReportEditorForm;

begin
  if ReportPropertiesFormClass=Nil then exit;
  F:=ReportPropertiesFormClass.Create(Self);
  try
    F.Report:=FReport;
    If (F.ShowModal=mrOK) then
      Modified:=True;
  finally
     F.Free;
  end;
end;

procedure TFPReportDesignerForm.AReportPropertiesUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(Report) and (rdoAllowProperties in DesignOptions);
end;

procedure TFPReportDesignerForm.AReportVariablesExecute(Sender: TObject);

Var
  F : TBaseReportVariablesForm;


begin
  if ReportVariablesFormClass=nil then
    exit;
  F:=ReportVariablesFormClass.Create(Self);
  try
    F.Report:=Self.Report;
    F.Variables:=FReport.Variables;
    if (F.ShowModal=mrOK) then
      begin
      Modified:=True;
      FReport.Variables:=F.Variables;
      FReportData.RefreshVariables;
      end;
  finally
     F.Free;
  end;
end;

procedure TFPReportDesignerForm.AReportVariablesUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=Assigned(Report) and (rdoManageVariables in DesignOptions);
end;

procedure TFPReportDesignerForm.AResizeBandToFitExecute(Sender: TObject);
begin
  If Assigned(CurrentDesigner) then
   CurrentDesigner.Objects.AdjustSelectedBandsToContent;
end;

procedure TFPReportDesignerForm.AResizeBandToFitUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentDesigner) and (CurrentDesigner.Objects.HaveSelection);
end;

procedure TFPReportDesignerForm.AResizeExecute(Sender: TObject);

Var
  F : TBaseReportResizeForm;

begin
  if ReportResizeFormClass=nil then
    exit;
  F:=ReportResizeFormClass.Create(Self);
  try
    F.Report:=Self.Report;
    if F.ShowModal=mrOK then
      CurrentDesigner.Objects.ResizeSelection(F.Vertical,F.VerticalSize,F.Horizontal,F.HorizontalSize);
  finally
    F.Free;
  end;
end;

procedure TFPReportDesignerForm.ASendToBackExecute(Sender: TObject);
begin
  CurrentDesigner.Objects.SendToBack;
end;

procedure TFPReportDesignerForm.ASendToBackFrontUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentDesigner)
                               and (CurrentDesigner.Objects.SelectionCount=1)
                               and (CurrentDesigner.Objects.GetSelection[0].IsPlainElement);
end;

procedure TFPReportDesignerForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if Not (fsModal in FFormState) then
    CloseAction:=caFree;

end;


procedure TFPReportDesignerForm.PCReportChange(Sender: TObject);
begin
  if not FStopDesigning then
    ResetObjectInspector;
end;

procedure TFPReportDesignerForm.ResetObjectInspector;

begin
  FOI.Report:=FReport;
  if Assigned(CurrentDesigner) then
    FOI.SelectControls(CurrentDesigner.Objects)
  else
    FOI.SelectControls(Nil)
end;

procedure TFPReportDesignerForm.VResizeAllow(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentDesigner) and CurrentDesigner.Objects.IsMultiSelect;
end;

procedure TFPReportDesignerForm.HResizeExecute(Sender: TObject);
begin
  CurrentDesigner.Objects.ResizeSelection(saNone,0.0,TSizeAdjust((Sender as TACtion).Tag),0.0);
end;

procedure TFPReportDesignerForm.AResizeUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled:=Assigned(ReportResizeFormClass) and Assigned(CurrentDesigner) and (CurrentDesigner.Objects.Haveselection) ;
end;

procedure TFPReportDesignerForm.HResizeAllow(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(CurrentDesigner) and CurrentDesigner.Objects.IsMultiSelect;
end;

procedure TFPReportDesignerForm.VAlignUpdate(Sender: TObject);

Var
  A : TAction;
begin
  A:=(Sender as TAction);
  A.Enabled:=Assigned(CurrentDesigner) and CurrentDesigner.Objects.VerticalAlignOK(TVAlignAction(A.Tag));
end;

procedure TFPReportDesignerForm.AFileSaveExecute(Sender: TObject);
begin
  SaveReport;
end;

procedure TFPReportDesignerForm.AQuitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFPReportDesignerForm.SaveDesignToFile(AFileName : string);

Var
  WS: TFPReportJSONStreamer;
  S : UTF8String;
  FS : TFileStream;
  DD : TJSONObject;

begin
  FS:=Nil;
  ws := TFPReportJSONStreamer.Create(nil);
  try
    // Write report
    WS.JSON:=TJSONObject.Create;
    FReport.WriteElement(WS);
    if rdoManageData in DesignOptions then
      begin
      // Add design data
      DD:=TJSONObject.Create;
      WS.JSon.Add('DesignData',DD);
      FReportDesignData.SaveToJSON(DD);
      end;
    // Now save to file
    fs:=TFileStream.Create(AFilename, fmCreate);
    S:=WS.JSON.FormatJSON();
    fs.WriteBuffer(S[1],Length(S));
    // Housekeeping
    SetFileCaption(AFileName);
    Modified:=False;
  finally
    FreeAndNil(fs);
    FreeAndNil(ws);
  end;
end;

procedure TFPReportDesignerForm.FormKeyPress(Sender: TObject; var Key: char);
begin
end;

procedure TFPReportDesignerForm.HAlignExecute(Sender: TObject);
Var
  A : TAction;
begin
  A:=(Sender as TAction);
  CurrentDesigner.Objects.AlignSelection(THAlignAction(A.Tag),vaNone);
  CurrentDesigner.Invalidate;
end;

procedure TFPReportDesignerForm.HAlignUpdate(Sender: TObject);

Var
  A : TAction;
begin
  A:=(Sender as TAction);
  A.Enabled:=Assigned(CurrentDesigner) and CurrentDesigner.Objects.HorizontalAlignOK(THAlignAction(A.Tag));
end;

procedure TFPReportDesignerForm.AFileOpenExecute(Sender: TObject);
begin
  if Not CheckSaved(SOpenReport) then
    Exit;
  OpenReport;
end;

function TFPReportDesignerForm.OpenReport: Boolean;

begin
  FLoadModified:=False;
  Result:=Assigned(OnOpenReport);
  if Result then
    begin
    StopDesigning;
    OnOpenReport(Self)
    end
  else
    begin
    if (FileName<>'') then
      ODReport.FileName:=Self.FileName;
    Result:=ODReport.Execute;
    if Result then
      LoadReportFromFile(ODReport.FileName);
    end;
  If Result then
    DesignReport;
end;

Procedure TFPReportDesignerForm.LoadReportFromFile(Const AFileName : String);

begin
  LoadDesignFromFile(AFileName);
  SetFileCaption(AFileName);
  if Assigned(MRUMenuManager1) then
    begin
    MRUMenuManager1.AddToRecent(AFileName);
    MRUMenuManager1.SaveRecentFilesToIni;
    end;
end;

procedure TFPReportDesignerForm.StopDesigning;

Var
  I : integer;

begin
  I:=FReportDesignData.DataDefinitions.Count;
  FStopDesigning:=True;
  try
    if Assigned(FReport) then
      Report.EndDesigning;
    For I:=ComponentCount-1 downto 0 do
      if Components[I] is TFPReportDesignerControl then
         Components[I].Free;

    While PCReport.ControlCount>0 do
      PCReport.Controls[PCReport.ControlCount-1].Free;
    // Give LCL time to clean up.
    Application.ProcessMessages;
    FReportData.Report:=Nil;
    if (rdoManageData in DesignOptions) then
      begin
      if Assigned(FReport) then
        FReportDesignData.RemoveFromReport(FReport);
      FReportDesignData.DataDefinitions.Clear;
      end;
    FOI.Report:=Nil;
    FOI.SelectControls(Nil);
  Finally
    FStopDesigning:=False;
  end;
end;

procedure TFPReportDesignerForm.VAlignExecute(Sender: TObject);
Var
  A : TAction;
begin
  A:=(Sender as TAction);
  CurrentDesigner.Objects.AlignSelection(haNone,TVAlignAction(A.Tag));
end;

procedure TFPReportDesignerForm.VResizeExecute(Sender: TObject);
begin
  CurrentDesigner.Objects.ResizeSelection(TSizeAdjust((Sender as TACtion).Tag),0.0,saNone,0.0);
end;

procedure TFPReportDesignerForm.ResetReport;

begin
  if Assigned(FReport) then
    begin
    if (FReport.Owner=Self) then
      begin
      FreeAndNil(FReport);
      FReport := TFPReport.Create(Self);
      end
    else
      FReport.Clear(rdoManageData in DesignOptions);
    end
  else
    FReport := TFPReport.Create(Self);
end;

procedure TFPReportDesignerForm.LoadDesignFromFile(const AFilename: string);

var
  rs: TFPReportJSONStreamer;
  ms : TMemoryStream;
  uts : UTF8String;
  DD,lJSON: TJSONObject;
  Errs : TStrings;
  OldName : TComponentName;

begin
  if AFilename = '' then
    Exit;
  if not FileExists(AFilename) then
    raise Exception.CreateFmt('The file "%s" can not be found', [AFilename]);

  ms := TMemoryStream.Create();
  try
    ms.LoadFromFile(AFilename);
    ms.Position := 0;
    SetLength(uts,ms.Size);
    Move(ms.Memory^,uts[Low(uts)],Length(uts));
  finally
    FreeAndNil(ms);
  end;
  lJSON := TJSONObject(GetJSON(uts));
  StopDesigning;
  ResetReport;
  OldName:=FReport.Name;
  errs:=nil;
  rs := TFPReportJSONStreamer.Create(nil);
  rs.JSON := lJSON; // rs takes ownership of lJSON
  try
    if rdoManageData in DesignOptions then
      begin
      DD:=lJSON.Get('DesignData',TJSONObject(Nil));
      if Assigned(DD) then
        FReportDesignData.DataDefinitions.LoadFromJSON(DD);
      end;
    // We must do this before the report is loaded, so the pages/bands can find their data
    Errs:=TStringList.Create;
    CreateReportDataSets(Errs);
    FReport.ReadElement(rs);
    if (FReport.Owner<>Self) and (OldName<>'') then
      FReport.Name:=OldName;
    FFilename:=AFileName;
    if Assigned(errs) and (Errs.Count>0) then
      MessageDlg(SErrAccessingData,Format(SErrAccessingDataDetails,[Errs.Text]),mtWarning,[mbOK],'');
    FLoadModified:=rs.IsModified;
  finally
    FreeAndNil(rs);
    FreeAndNil(Errs);
  end;
end;

procedure TFPReportDesignerForm.SetDesignOptions(AValue: TFPReportDesignOptions);


begin
  if FDesignOptions=AValue then Exit;
  FDesignOptions:=AValue;
  ApplyDesignOptions;
end;

procedure TFPReportDesignerForm.ApplyDesignOptions;

  Procedure EnableAction(A : TAction; O : TFPReportDesignOption);

  begin
    A.Enabled:=(O in FDesignOptions);
    A.Visible:=(O in FDesignOptions);
  end;

  Procedure EnableActions(Ar : Array of TAction; O : TFPReportDesignOption);

  Var
    A : TAction;

  begin
    For A in Ar do
      EnableAction(A,O);
  end;

Var
  I : Integer;
  B : Boolean;

begin
  EnableAction(AReportData,rdoManageData);
  EnableAction(AReportVariables,rdoManageVariables);
  EnableAction(AFileOpen,rdoAllowLoad);
  EnableAction(AFileOpenNewWindow,rdoAllowLoad);
  EnableAction(AFileSave,rdoAllowSave);
  EnableAction(AReportProperties,rdoAllowProperties);
  EnableAction(AAddPage,rdoAllowPageAdd);
  EnableAction(ANew,rdoAllowNew);
  EnableAction(ANewNewWindow,rdoAllowNew);
  EnableAction(APreview,rdoAllowPreview);
  EnableActions([AAddChildBand,AAddGroupHeader,AAddGroupFooter,AAddPageHeader,
                 AAddPageFooter,AAddReportTitle,AAddReportSummary,AAddColumnHeader,
                 AAddColumnFooter,AAddDataHeader,AAddDataFooter,AAddDataBand],
                rdoAllowBands);
  MIBands.Visible:=(rdoAllowBands in FDesignoptions);
  I:=0;
  B:=False;
  While (not B) and (I<MReport.Count) do
    begin
    B:=MReport.Items[i].Visible;
    Inc(I);
    end;
  MReport.Visible:=B;

end;

procedure TFPReportDesignerForm.DoElementCreated(Sender: TObject;
  AElement: TFPReportElement);
begin
  AElement.StartDesigning;
  if AElement.Name='' then
    AElement.Name:=AElement.AllocateName;
  If AElement is TFPReportCustomMemo then
    begin
    if TFPReportMemo(AElement).Text='' then
      TFPReportMemo(AElement).Text:='New memo';
    end;
  FOI.RefreshReportTree;
end;

{$IFDEF USEDEMOREPORT}
procedure TFPReportDesignerForm.CreateDemoReport;

var
  p: TFPReportPage;
  TitleBand: TFPReportTitleBand;
  DataBand: TFPReportDataBand;
  Memo: TFPReportMemo;
  PageFooter: TFPReportPageFooterBand;
  SummaryBand: TFPReportSummaryBand;
begin
  lReportData := TFPReportUserData.Create(Self);
  lReportData.Name:='UserData';
  lReportData.OnGetValue := @GetReportDataValue;
  lReportData.OnGetEOF := @GetReportDataEOF;
  lReportData.OnFirst := @GetReportDataFirst;
  lReportData.OnGetNames := @GetReportDataNames;
  lReportData.InitFieldDefs;
  InitialiseData;

  FReport:=TFPReport.Create(Self);
  FReport.Author := 'Graeme Geldenhuys';
  FReport.Title := 'FPReport Demo 1 - Simple Listing';
  FReport.Variables.AddVariable('Var1').AsString:='Value1';
  FReport.Variables.AddVariable('Var2').AsString:='Value2';
    p := TFPReportPage.Create(FReport);
    p.Orientation := poPortrait;
    p.PageSize.PaperName := 'A4';
    { page margins }
    p.Margins.Left := 30;
    p.Margins.Top := 20;
    p.Margins.Right := 30;
    p.Margins.Bottom := 20;
    p.Data := lReportData;

    TitleBand := TFPReportTitleBand.Create(p);
    TitleBand.Layout.Height := 40;
    {$ifdef ColorBands}
    TitleBand.Frame.Shape := fsRectangle;
    TitleBand.Frame.BackgroundColor := clReportTitleSummary;
    {$endif}

    Memo := TFPReportMemo.Create(TitleBand);
    Memo.Layout.Left := 55;
    Memo.Layout.Top := 20;
    Memo.Layout.Width := 50;
    Memo.Layout.Height := 10;
    Memo.Font.Name := 'LiberationSans';
    Memo.Text := 'THE REPORT TITLE';

    With TFPReportShape.Create(TitleBand) do
      begin
      Layout.Left := 5;
      Layout.Top := 15;
      Layout.Width := 20;
      Layout.Height := 20;
      ShapeType:=stCircle;
      end;
    With TFPReportCheckbox.Create(TitleBand) do
      begin
      Layout.Left := 35;
      Layout.Top := 15;
      Layout.Width := 5;
      Layout.Height := 5;
      end;
    DataBand := TFPReportDataBand.Create(p);
    DataBand.Layout.Height := 10;
    {$ifdef ColorBands}
    DataBand.Frame.Shape := fsRectangle;
    DataBand.Frame.BackgroundColor := clDataBand;
    {$endif}

    Memo := TFPReportMemo.Create(DataBand);
    Memo.Layout.Left := 5;
    Memo.Layout.Top := 0;
    Memo.Layout.Width := 60;
    Memo.Layout.Height := 5;
    Memo.Font.Name := 'LiberationSans';
    Memo.Text := 'Hello world <[userdata.string]>.';

    PageFooter := TFPReportPageFooterBand.Create(p);
    PageFooter.Layout.Height := 40;
    {$ifdef ColorBands}
    PageFooter.Frame.Shape := fsRectangle;
    PageFooter.Frame.BackgroundColor := clPageHeaderFooter;
    {$endif}

    Memo := TFPReportMemo.Create(PageFooter);
    Memo.Layout.Left := 135;
    Memo.Layout.Top := 13;
    Memo.Layout.Width := 15;
    Memo.Layout.Height := 5;
    Memo.Options:=[moDisableWordWrap];
    Memo.Font.Name := 'LiberationSans';
    Memo.Text := 'Page [PageNo]';

    With TFPReportImage.Create(PageFooter) do
      begin
      Layout.Left := 5;
      Layout.Top := 2;
      Layout.Width := 35;
      Layout.Height := 35;
      Stretched:=True;
      LoadFromFile(ExtractFilePath(ParamStr(0))+'testimage.png');
      end;


    SummaryBand := TFPReportSummaryBand.Create(p);
    SummaryBand.Layout.Height := 40;
    {$ifdef ColorBands}
    SummaryBand.Frame.Shape := fsRectangle;
    SummaryBand.Frame.BackgroundColor := clReportTitleSummary;
    {$endif}

    Memo := TFPReportMemo.Create(SummaryBand);
    Memo.Layout.Left := 19;
    Memo.Layout.Top := 10;
    Memo.Layout.Width := 70;
    Memo.Layout.Height := 25;
    Memo.Font.Name := 'LiberationSans';
    Memo.StretchMode := smActualHeight;
    Memo.Text := 'This is some long text that should be wrapping inside the memo. It has a 10mm left margin, and a 7mm right margin. 0mm margin top and bottom.';
    Memo.TextAlignment.LeftMargin := 10;
    Memo.TextAlignment.RightMargin := 7;
    Memo.Frame.Shape := fsRectangle;
    Memo.Frame.BackgroundColor := clLtGray;
end;

procedure TFPReportDesignerForm.GetReportDataFirst(Sender: TObject);
begin

end;

procedure TFPReportDesignerForm.GetReportDataValue(Sender: TObject;
  const AValueName: String; var AValue: Variant);
begin
  if (AValueName = 'element') or (AValueName = 'string') then
  begin
    AValue := sl[lReportData.RecNo-1];
  end
  else
    AValue:=AValueName+IntToStr(lReportData.RecNo);
end;

procedure TFPReportDesignerForm.GetReportDataEOF(Sender: TObject;
  var IsEOF: Boolean);
begin
  if lReportData.RecNo > sl.Count then
    IsEOF := True
  else
    IsEOF := False;
end;

procedure TFPReportDesignerForm.GetReportDataNames(Sender: TObject;
  List: TStrings);
begin
  List.Add('element');
  List.Add('string');
  List.Add('FirstName');
  List.Add('LastName');
  List.Add('DateOfBirth');
  List.Add('Gender');
  List.Add('Email');
end;

procedure TFPReportDesignerForm.InitialiseData;
var
  i: integer;
begin
  sl := TStringList.Create;
  for i := 1 to 30 do
    sl.Add(Format('Item %d', [i]));
end;
{$ENDIF USEDEMOREPORT}

end.

