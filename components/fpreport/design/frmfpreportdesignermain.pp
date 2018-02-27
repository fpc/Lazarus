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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, ActnList, ComCtrls, ExtCtrls, IniPropStorage, Types, fpreport, fpreportdesignctrl,
  fraReportObjectInspector, fpreportdesignreportdata, frafpreportdata, fpreportdb, mrumanager;

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
                           rdoAllowBands      // Allow user to add/remove bands
                           );
  TFPReportDesignOptions = set of TFPReportDesignOption;

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
    procedure ADeleteExecute(Sender: TObject);
    procedure ADeleteUpdate(Sender: TObject);
    procedure AFrameExecute(Sender: TObject);
    procedure AFrameUpdate(Sender: TObject);
    procedure ANewExecute(Sender: TObject);
    procedure APreviewExecute(Sender: TObject);
    procedure APreviewUpdate(Sender: TObject);
    procedure AReportDataExecute(Sender: TObject);
    procedure AReportDataUpdate(Sender: TObject);
    procedure AReportPropertiesExecute(Sender: TObject);
    procedure AReportPropertiesUpdate(Sender: TObject);
    procedure AReportVariablesExecute(Sender: TObject);
    procedure AReportVariablesUpdate(Sender: TObject);
    procedure AResizeExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormDestroy(Sender: TObject);
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
    MRUMenuManager1: TMRUMenuManager;
    FAutoSaveOnClose: Boolean;
    FDesignOptions: TFPReportDesignOptions;
    FFileName: String;
    FModified: Boolean;
    FOnNewReport: TNotifyEvent;
    FOnOpenReport: TNotifyEvent;
    FOnSaveReport: TNotifyEvent;
    FReportDesignData : TDesignReportDataCollection;
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
    procedure DoSelectionModifiedByOI(Sender: TObject);
    function GetModified: boolean;
    procedure ActivateDesignerForElement(AElement: TFPReportElement);
    procedure SetBandActionTags;
    procedure SetDesignOptions(AValue: TFPReportDesignOptions);
    procedure SetFileCaption(const AFileName: String);
    procedure SetModified(AValue: Boolean);
  Protected
    procedure MRUMenuManager1RecentFile(Sender: TObject; const AFileName: String);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ApplyDesignOptions; virtual;
    function AddPageDesign(aPageNo: Integer; APage: TFPReportCustomPage): TTabSheet;
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
    procedure CreateReportDataSets; virtual;
    procedure SetReport(AValue: TFPReport); virtual;
    procedure ShowReportData; virtual;
    Function DesignerCount : Integer;
    function PageDesigner(Aindex : Integer) : TFPReportDesignerControl;
    Property Modified : Boolean Read GetModified Write SetModified;
  public
    procedure DesignReport; virtual;
    procedure StopDesigning; virtual;
    procedure PreviewReport; virtual;
    function NewReport: Boolean; virtual;
    Function SaveReport : Boolean; virtual;
    function OpenReport: Boolean; virtual;
    procedure DoElementCreated(Sender: TObject; AElement: TFPReportElement);
    Property Report : TFPReport Read FReport Write SetReport;
    Property FileName : String Read FFileName Write FFileName;
    Property ReportDesignData : TDesignReportDataCollection Read FReportDesignData;
    Property DesignOptions : TFPReportDesignOptions Read FDesignOptions Write SetDesignOptions;
    // If these are set, they override the default handling.
    Property OnSaveReport : TNotifyEvent Read FOnSaveReport Write FOnSaveReport;
    Property OnNewReport : TNotifyEvent Read FOnNewReport Write FOnNewReport;
    Property OnOpenReport : TNotifyEvent Read FOnOpenReport Write FOnOpenReport;
    Property AutoSaveOnClose : Boolean Read FAutoSaveOnClose Write FAutoSaveOnClose;
  end;

Const
  AllReportDesignOptions = [rdoManageData,rdoManageVariables,rdoAllowLoad,rdoAllowSave,rdoAllowProperties,
                            rdoAllowPageAdd,rdoAllowNew, rdoAllowPreview, rdoAllowBands];


implementation

uses
  reportdesignbaseforms,
  fpreportdesignobjectlist,
  fpreportformexport, // form export has it's own factory pattern.
  fpttf,
  fpreportstreamer,
  fpjson,
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
  FDataParent:=TComponent.Create(nil);
  FreeAndNil(TSDesign); // Remove design-time added page
  FReportDesignData:=TDesignReportDataCollection.Create(TDesignReportData);
  SetBandActionTags;
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
  with MRUMenuManager1 do begin
    maxRecent := 5;
    IniFileName := ChangeFileExt(ParamStr(0), '.ini');
    MenuItem := MIRecent;
    PopupMenu := PMRecent;
    MaxItemLength := 80;
    MenuCaptionMask := '(%d) %s';
    OnRecentFile := @MRUMenuManager1RecentFile;
    LoadRecentFilesFromIni;
  end;
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

procedure TFPReportDesignerForm.FormShow(Sender: TObject);
begin
  if Assigned(Report) then
    DesignReport;
  SBReport.Visible:=False;
  SBReport.Visible:=True;
end;

procedure TFPReportDesignerForm.DesignReport;

Var
  I : Integer;

begin
  For I:=0 to Report.PageCount-1 do
    AddPageDesign(I+1,Report.Pages[I]);
  ShowReportData;
  ResetObjectInspector;
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
  if D.Objects.SelectionCount>0 then
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

function TFPReportDesignerForm.AddPageDesign(aPageNo: Integer;
  APage: TFPReportCustomPage): TTabSheet;

Var
  TS : TPageTabSheet;
  D : TFPReportDesignerControl;

begin
  TS:=TPageTabSheet.Create(Self);
  TS.FPage:=APage;
  TS.Parent:=PCReport;
  //    TS.AutoScroll:=True;
  TS.Caption:=Format('Page %d',[aPageNo]);
  D:=TFPReportDesignerControl.Create(Self);
  TS.FDesigner:=D;
  D.Parent:=TS;
  //  FDesign.Align:=alClient;
  //  fdesign.SetBounds(0,0,ClientWidth,ClientHeight);
  D.Top:=0;
  D.Left:=0;
  D.Page:=APage;
  D.OnElementCreated:=@DoElementCreated;
  D.OnSelectionChanged:=@DoSelectionChanged;
  D.OnStateChange:=@DoStateChange;
  D.Objects[0].Selected:=True;
  Result:=TS;
end;

procedure TFPReportDesignerForm.SetFileCaption(const AFileName: String);

begin
  if AFileName='' then
    Caption:=SCaption+' [new file]'
  else
    Caption:=SCaption+' ['+AFileName+']'
end;

procedure TFPReportDesignerForm.SetModified(AValue: Boolean);

Var
  I : integer;

begin
  FModified:=AVAlue;
  if not Avalue then
    For I:=0 to DesignerCount-1 do
       PageDesigner(i).Objects.Modified:=False;
end;

procedure TFPReportDesignerForm.MRUMenuManager1RecentFile(Sender: TObject;
  const AFileName: String);
begin
  if Assigned(OnOpenReport) then
    begin
    StopDesigning;
    OnOpenReport(Self)
    end
  else
    begin
      StopDesigning;
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

procedure TFPReportDesignerForm.AAddMemoExecute(Sender: TObject);
begin
  CurrentDesigner.AddElement(TFPReportMemo);
end;

procedure TFPReportDesignerForm.AAddPageExecute(Sender: TObject);

Var
  P : TFPReportCustomPage;

begin
  P:=gBandFactory.PageClass.Create(FReport);
  P.PageSize.PaperName := 'A4';
  { page margins }
  P.Margins.Left := 30;
  P.Margins.Top := 20;
  P.Margins.Right := 30;
  P.Margins.Bottom := 20;
  FReport.AddPage(P);
  FOI.RefreshReportTree;
  PCReport.ActivePage:=AddPageDesign(FReport.PageCount,P);
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
  CurrentDesigner.AddBand(gBandFactory.BandClasses[TFPReportBandType(T)]);
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
  (Sender as TAction).Enabled:=Assigned(CurrentDesigner) and CurrentDesigner.Objects.IsMultiSelect;
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
  (Sender as TAction).Enabled:=Assigned(CurrentDesigner) and (CurrentDesigner.Objects.SelectionCount>0);
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
  (Sender as TAction).Enabled:=Assigned(CurrentDesigner) and (CurrentDesigner.Objects.SelectionCount>0);
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
begin
  if Assigned(CurrentDesigner) then
    begin
    if (FOI.ObjectList.Count=1) and
       (FOI.ObjectList.Elements[0]=CurrentDesigner.Page) then
      begin
      CurrentDesigner.UpdatePageParams;
      CurrentDesigner.Reset;
      CurrentDesigner.Objects.SelectElement(CurrentDesigner.Page)
      end
    else
      CurrentDesigner.Invalidate;
    end;
end;

function TFPReportDesignerForm.SaveReport: Boolean;
begin
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
        MRUMenuManager1.AddToRecent(FileName);
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

function TFPReportDesignerForm.NewReport: Boolean;

Var
  P : TFPReportCustomPage;

begin
  result:=Assigned(OnNewReport);
  if Result then
    OnNewReport(Self)
  else
    begin
    StopDesigning;
    FreeAndNil(FReport);
    Report:=TFPReport.Create(Self);
    p:=gBandFactory.PageClass.Create(FReport);
    p.PageSize.PaperName := 'A4';
    p.Margins.Left := 20;
    p.Margins.Top := 20;
    p.Margins.Right := 20;
    p.Margins.Bottom := 20;
    FReport.AddPage(P);
    FOI.RefreshReportTree;
    Result:=True
    end;
end;

procedure TFPReportDesignerForm.APreviewExecute(Sender: TObject);
begin
  PreviewReport;
end;

procedure TFPReportDesignerForm.PreviewReport;

Var
  F : TFPreportPreviewExport;

begin
  FReport.RunReport;
  F:=TFPreportPreviewExport.Create(Self);
  FReport.RenderReport(F);
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
  F:=ReportDataFormClass.Create(Self);
  try
    F.Report:=Self.Report;
    F.Data:=FReportDesignData;
    if F.ShowModal=mrOK then
      begin
      FReportDesignData.Assign(F.Data);
      CreateReportDataSets;
      FModified:=True;
      end;
  finally
     F.Free;
  end;
end;

procedure TFPReportDesignerForm.AReportDataUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled:=Assigned(FReport);
end;

procedure TFPReportDesignerForm.CreateReportDataSets;

Var
  I : Integer;
  ReportD : TFPReportDataItem;
  DesignD : TDesignReportData;
  DatasetD : TFPReportDatasetData;

begin
  While FDataParent.ComponentCount>0 do
    FDataParent.Components[FDataParent.ComponentCount-1].Free;
  FReport.SaveDataToNames;
  FReport.ReportData.Clear;
  For I:=0 to FReportDesignData.Count-1 do
    begin
    DesignD:=FReportDesignData[i];
    DatasetD:=TFPReportDatasetData.Create(FDataParent);
    DatasetD.Dataset:=DesignD.CreateDataSet(DatasetD);
    DatasetD.InitFieldDefs;
    DatasetD.Name:=DesignD.Name;
    DatasetD.Dataset.Name:=DesignD.Name;
    ReportD:=FReport.ReportData.AddReportData(DatasetD);
    end;
  FReport.RestoreDataFromNames;
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
      FModified:=True;
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
  S : String;

begin
  if ReportVariablesFormClass=nil then
    exit;
  S:=ReportVariablesFormClass.ClassName;
  F:=ReportVariablesFormClass.Create(Self);

  try
    F.Report:=Self.Report;
    F.Variables:=FReport.Variables;
    if (F.ShowModal=mrOK) then
      begin
      FModified:=True;
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
      CurrentDesigner.Objects.ResizeSelection(F.Horizontal,F.HorizontalSize,F.Vertical,F.VerticalSize);
  finally
    F.Free;
  end;
end;


procedure TFPReportDesignerForm.PCReportChange(Sender: TObject);
begin
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
  CurrentDesigner.Objects.ResizeSelection(TSizeAdjust((Sender as TACtion).Tag),0.0,saNone,0.0);
end;

procedure TFPReportDesignerForm.AResizeUpdate(Sender: TObject);

begin
  (Sender as TAction).Enabled:=Assigned(ReportResizeFormClass) and Assigned(CurrentDesigner) and CurrentDesigner.Objects.IsMultiSelect;
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
  S : String;
  FS : TFileStream;
  DD : TJSONObject;

begin
  FS:=Nil;
  ws := TFPReportJSONStreamer.Create(nil);
  try
    // Write report
    WS.JSON:=TJSONObject.Create;
    FReport.WriteElement(WS);
    // Add design data
    DD:=TJSONObject.Create;
    WS.JSon.Add('DesignData',DD);
    FReportDesignData.SaveToJSON(DD);
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
      begin
      StopDesigning;
      LoadDesignFromFile(ODReport.FileName);
      SetFileCaption(ODReport.FileName);
      if Assigned(MRUMenuManager1) then
        MRUMenuManager1.AddToRecent(ODReport.FileName);
      end;
    end;
  If Result then
    DesignReport;
end;

procedure TFPReportDesignerForm.StopDesigning;

Var
  I : integer;

begin
  For I:=ComponentCount-1 downto 0 do
    if Components[I] is TFPReportDesignerControl then
       Components[I].Free;
  While PCReport.ControlCount>0 do
    PCReport.Controls[PCReport.ControlCount-1].Free;
  // Give LCL time to clean up.
  Application.ProcessMessages;
  FReportData.Report:=Nil;
  FReportDesignData.Clear;
  FOI.Report:=Nil;
  FOI.SelectControls(Nil);
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
  CurrentDesigner.Objects.ResizeSelection(saNone,0.0,TSizeAdjust((Sender as TACtion).Tag),0.0);
end;

procedure TFPReportDesignerForm.LoadDesignFromFile(const AFilename: string);
var
  rs: TFPReportJSONStreamer;
  fs: TFileStream;
  DD,lJSON: TJSONObject;
begin
  if AFilename = '' then
    Exit;
  if not FileExists(AFilename) then
    raise Exception.CreateFmt('The file "%s" can not be found', [AFilename]);

  fs := TFileStream.Create(AFilename, fmOpenRead or fmShareDenyNone);
  try
    lJSON := TJSONObject(GetJSON(fs));
  finally
    FreeAndNil(fs);
  end;
  StopDesigning;
  FreeAndNil(FReport);
  FReport := TFPReport.Create(Self);

  rs := TFPReportJSONStreamer.Create(nil);
  rs.JSON := lJSON; // rs takes ownership of lJSON
  try
    DD:=lJSON.Get('DesignData',TJSONObject(Nil));
    if Assigned(DD) then
      FReportDesignData.LoadFromJSON(DD);
    // We must do this before the report is loaded, so the pages/bands can find their data
    CreateReportDataSets;
    FReport.ReadElement(rs);
    FFilename:=AFileName;
  finally
    FreeAndNil(rs);
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
  EnableAction(AFileSave,rdoAllowSave);
  EnableAction(AReportProperties,rdoAllowProperties);
  EnableAction(AAddPage,rdoAllowPageAdd);
  EnableAction(ANew,rdoAllowNew);
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
  If AElement is TFPReportCustomMemo then
    begin
    TFPReportMemo(AElement).Font.Name := 'LiberationSans';
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

