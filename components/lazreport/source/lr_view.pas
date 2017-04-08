
{*****************************************}
{                                         }
{             FastReport v2.3             }
{             Report preview              }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

unit LR_View;

interface

{$I LR_Vers.inc}

uses
  Classes, SysUtils, LResources, LMessages, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Buttons, StdCtrls, Menus, GraphType, LCLType, LCLIntf, LazUTF8,
  LR_Const, PrintersDlgs;

type
  TfrPreviewForm = class;
  TfrPreviewZoom = (pzDefault, pzPageWidth, pzOnePage, pzTwoPages);
  TfrPreviewButton = (pbZoom, pbLoad, pbSave, pbPrint, pbFind, pbHelp, pbExit);
  TfrPreviewButtons = set of TfrPreviewButton;

  { TfrPreview }

  TfrPreview = class(TPanel)
  private
    FWindow: TfrPreviewForm;
    FScrollBars: TScrollStyle;
    function GetOnScrollPage: TNotifyEvent;
    function GetPage: Integer;
    procedure SetOnScrollPage(AValue: TNotifyEvent);
    procedure SetPage(Value: Integer);
    function GetZoom: Double;
    procedure SetZoom(Value: Double);
    function GetAllPages: Integer;
    procedure SetScrollBars(Value: TScrollStyle);
  protected
    procedure DoOnChangeBounds; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect(Doc: Pointer);
    procedure Clear;
    procedure OnePage;
    procedure TwoPages;
    procedure PageWidth;
    procedure First;
    procedure Next;
    procedure Prev;
    procedure Last;
    procedure SaveToFile;
    procedure LoadFromFile;
    function Print: boolean;
    procedure Edit;
    procedure Find;

    procedure SmallScrollUp;
    procedure SmallScrollDown;
    procedure SmallScrollLeft;
    procedure SmallScrollRight;
    procedure SmallScrollNext;
    procedure SmallScrollPrior;

    function ExportTo(AFileName: string): boolean;
    property AllPages: Integer read GetAllPages;
    property Page: Integer read GetPage write SetPage;
    property Zoom: Double read GetZoom write SetZoom;
  published
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars;
    property OnScrollPage:TNotifyEvent read GetOnScrollPage write SetOnScrollPage;
  end;

  { TfrPBox }

  TfrPBox = class(TPanel)
  private
    FCurView:TObject;
  public
    Preview: TfrPreviewForm;
    procedure WMEraseBackground(var {%H-}Message: TLMEraseBkgnd); message LM_ERASEBKGND;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure DblClick; override;

    property OnMouseWheelDown;
    property OnMouseWheelUp;
  end;

  TfrScaleMode = (mdNone, mdPageWidth, mdOnePage, mdTwoPages);

  { TfrPreviewForm }

  TfrPreviewForm = class(TForm)
    FindBtn: TBitBtn;
    BtZoomOut: TBitBtn;
    BtZoomIn: TBitBtn;
    frTBSeparator1: TPanel;
    frTBSeparator2: TPanel;
    frTBSeparator3: TPanel;
    frTBSeparator4: TPanel;
    LbPanel: TPanel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PanTop: TPanel;
    PgDown: TSpeedButton;
    PgUp: TSpeedButton;
    PopupMenu1: TPopupMenu;
    prnDialog: TPrintDialog;
    ProcMenu: TPopupMenu;
    N2001: TMenuItem;
    N1501: TMenuItem;
    N1001: TMenuItem;
    N751: TMenuItem;
    N501: TMenuItem;
    N251: TMenuItem;
    N101: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    N4: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    PreviewPanel: TPanel;
    ScrollBox1: TScrollBox;
    RPanel: TPanel;
    BtPgFirst: TSpeedButton;
    BtPgLast: TSpeedButton;
    SpeedButton1: TSpeedButton;
    VScrollBar: TScrollBar;
    BPanel: TPanel;
    HScrollBar: TScrollBar;
    Panel1: TPanel;
    ZoomBtn: TBitBtn;
    LoadBtn: TBitBtn;
    SaveBtn: TBitBtn;
    PrintBtn: TBitBtn;
    ExitBtn: TBitBtn;
    procedure BtZoomInClick(Sender: TObject);
    procedure BtZoomOutClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure BtPgFirstClick(Sender: TObject);
    procedure BtPgLastClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure VScrollBarChange(Sender: TObject);
    procedure HScrollBarChange(Sender: TObject);
    procedure PgUpClick(Sender: TObject);
    procedure PgDownClick(Sender: TObject);
    procedure ZoomBtnClick(Sender: TObject);
    procedure N3Click(Sender: TObject);
    procedure ExitBtnClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure LoadBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FindBtnClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure EditBtnClick(Sender: TObject);
    procedure DelPageBtnClick(Sender: TObject);
    procedure NewPageBtnClick(Sender: TObject);
    procedure HelpBtnClick(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; {%H-}Button: TMouseButton;
      {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormActivate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
  private
    { Private declarations }
    Doc: Pointer;
    EMFPages: Pointer;
    PBox: TfrPBox;
    CurPage: Integer;
    ofx, ofy, OldV, OldH: Integer;
    per: Double;
    mode: TfrScaleMode;
    PaintAllowed: Boolean;

    SearchFindStr: String;
    SearchCaseSensitive: Boolean;
    SearchDirecion:integer;
    SearchLastFoundPage: Integer;
    SearchLastFoundObject: Integer;

    HF: String;
    
    FOnScrollPage:TNotifyEvent;
    procedure ShowPageNum;
    procedure SetToCurPage;
//    procedure WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo); message WM_GETMINMAXINFO;
    procedure RedrawAll;
    procedure LoadFromFile(const aName: String);
    procedure SaveToFile(const aName: String);
//    procedure FindInEMF(emf: TMetafile);
    function FindInEMFPages:boolean;
    procedure FindText;
    procedure SetGrayedButtons(Value: Boolean);
    procedure Connect(ADoc: Pointer);
    procedure ConnectBack;
    procedure ScrollbarDelta(const VertDelta,HorzDelta: Integer);
    procedure MouseWheelDown(Sender: TObject; Shift: TShiftState;
      {%H-}MousePos: TPoint; var Handled: Boolean);
    procedure MouseWheelUp(Sender: TObject; Shift: TShiftState;
      {%H-}MousePos: TPoint; var Handled: Boolean);
    function ExportToWithFilterIndex(AFilterIndex:Integer; const AFileName: string): boolean;
    function Print: boolean;
    procedure CreateExportFilterItems;
    procedure ExportFilterItemExecClick(Sender: TObject);
  public
    { Public declarations }
    procedure Show_Modal(ADoc: Pointer);
  end;


implementation
uses LR_Class, LR_Prntr, LR_Srch, LR_PrDlg, Printers, strutils, lr_PreviewToolsAbstract;

{$R *.lfm}


type
  THackControl = class(TWinControl)
  end;

var
  LastScale     : Double = 1;
  LastScaleMode : TfrScaleMode = mdNone;
{----------------------------------------------------------------------------}
constructor TfrPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FWindow := TfrPreviewForm.Create(nil);
  self.BevelInner := bvNone;
  self.BevelOuter := bvLowered;
  self.ScrollBars := ssBoth;
end;

destructor TfrPreview.Destroy;
begin
  FWindow.Free;
  inherited Destroy;
end;

procedure TfrPreview.Connect(Doc: Pointer);
begin
  FWindow.PreviewPanel.Parent := Self;
  FWindow.Connect(Doc);
  Page := 1;
  FWindow.RedrawAll;
end;

procedure TfrPreview.Clear;
begin
  FWindow.PreviewPanel.Parent := nil;
end;

function TfrPreview.GetPage: Integer;
begin
  Result := FWindow.CurPage;
end;

function TfrPreview.GetOnScrollPage: TNotifyEvent;
begin
  Result:=FWindow.FOnScrollPage;
end;

procedure TfrPreview.SetOnScrollPage(AValue: TNotifyEvent);
begin
  FWindow.FOnScrollPage:=AValue;
end;

procedure TfrPreview.SetPage(Value: Integer);
begin
  if (Value < 1) or (Value > AllPages) then Exit;
  FWindow.CurPage := Value;
  FWindow.SetToCurPage;
end;

function TfrPreview.GetZoom: Double;
begin
  Result := FWindow.Per * 100;
end;

procedure TfrPreview.SetZoom(Value: Double);
begin
  FWindow.Per := Value / 100;
  FWindow.Mode := mdNone;
  FWindow.FormResize(nil);
  FWindow.PBox.Paint;
end;

function TfrPreview.GetAllPages: Integer;
begin
  Result := 0;
  if TfrEMFPages(FWindow.EMFPages) <> nil then
    Result := TfrEMFPages(FWindow.EMFPages).Count;
end;

procedure TfrPreview.SetScrollBars(Value: TScrollStyle);
begin
  FScrollBars := Value;
  FWindow.RPanel.Visible := (Value = ssBoth) or (Value = ssVertical);
  FWindow.BPanel.Visible := (Value = ssBoth) or (Value = ssHorizontal);
end;

procedure TfrPreview.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  if FWindow<>nil then
    FWindow.FormResize(nil);
end;

procedure TfrPreview.OnePage;
begin
  FWindow.Mode := mdOnePage;
  FWindow.FormResize(nil);
  FWindow.PBox.Paint;
end;

procedure TfrPreview.TwoPages;
begin
  FWindow.Mode := mdTwoPages;
  FWindow.FormResize(nil);
  FWindow.PBox.Paint;
end;

procedure TfrPreview.PageWidth;
begin
  FWindow.Mode := mdPageWidth;
  FWindow.FormResize(nil);
  FWindow.PBox.Paint;
end;

procedure TfrPreview.First;
begin
  Page := 1;
end;

procedure TfrPreview.Next;
begin
  Page := Page + 1;
end;

procedure TfrPreview.Prev;
begin
  Page := Page - 1;
end;

procedure TfrPreview.Last;
begin
  Page := AllPages;
end;

procedure TfrPreview.SaveToFile;
begin
  FWindow.SaveBtnClick(nil);
end;

procedure TfrPreview.LoadFromFile;
begin
  FWindow.LoadBtnClick(nil);
end;

function TfrPreview.Print: boolean;
begin
  result := FWindow.Print;
end;

procedure TfrPreview.Edit;
begin
  FWindow.EditBtnClick(nil);
end;

procedure TfrPreview.Find;
begin
  FWindow.FindBtnClick(nil);
end;

procedure TfrPreview.SmallScrollUp;
begin
  if FWindow.VScrollBar.Enabled then FWindow.VScrollBar.SetFocus;
  FWindow.ScrollBarDelta(-FWindow.VScrollBar.SmallChange, 0)
end;

procedure TfrPreview.SmallScrollDown;
begin
  if FWindow.VScrollBar.Enabled then FWindow.VScrollBar.SetFocus;
  FWindow.ScrollBarDelta(FWindow.VScrollBar.SmallChange, 0)
end;

procedure TfrPreview.SmallScrollLeft;
begin
  if FWindow.HScrollBar.Enabled then FWindow.HScrollBar.SetFocus;
  FWindow.ScrollBarDelta(-FWindow.HScrollBar.SmallChange, 0)
end;

procedure TfrPreview.SmallScrollRight;
begin
  if FWindow.HScrollBar.Enabled then FWindow.HScrollBar.SetFocus;
  FWindow.ScrollBarDelta(FWindow.HScrollBar.SmallChange, 0)
end;

procedure TfrPreview.SmallScrollNext;
begin
  if FWindow.VScrollBar.Enabled then FWindow.VScrollBar.SetFocus;
  FWindow.ScrollBarDelta(FWindow.VScrollBar.LargeChange, 0)
end;

procedure TfrPreview.SmallScrollPrior;
begin
  if FWindow.VScrollBar.Enabled then FWindow.VScrollBar.SetFocus;
  FWindow.ScrollBarDelta(-FWindow.VScrollBar.LargeChange, 0)
end;

function TfrPreview.ExportTo(AFileName: string): boolean;
var
  i: Integer;
  AExt: string;
begin
  result := false;
  AExt := ExtractFileExt(AFileName);
  for i:=0 to ExportFilters.Count - 1 do
    if SameText(AExt, ExtractFileExt(ExportFilters[i].FilterExt)) then
    begin
      FWindow.ExportToWithFilterIndex(i, AFileName);
      result := true;
      break;
    end;
end;

{----------------------------------------------------------------------------}
procedure TfrPBox.WMEraseBackground(var Message: TLMEraseBkgnd);
begin
end;

procedure TfrPBox.Paint;
var
  i: Integer;
  r, r1: TRect;
  Pages: TfrEMFPages;
  h: HRGN;
begin
  if not Preview.PaintAllowed then Exit;
  if Preview.EMFPages = nil then
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.FillRect(ClientRect);
    Exit;
  end;
  Pages := TfrEMFPages(Preview.EMFPages);
  h := CreateRectRgn(0, 0, Width, Height);
  GetClipRgn(Canvas.Handle, h);

  for i := 0 to Pages.Count - 1 do            // drawing window background
  begin
    r := Pages[i]^.r;
    OffsetRect(r, Preview.ofx, Preview.ofy);
    if (r.Top > 2000) or (r.Bottom < 0) then
      Pages[i]^.Visible := False else
      Pages[i]^.Visible := RectVisible(Canvas.Handle, r);
    if Pages[i]^.Visible then
      ExcludeClipRect(Canvas.Handle, r.Left + 1, r.Top + 1, r.Right - 1, r.Bottom - 1);
  end;

  with Canvas do
  begin
    Brush.Color := clGray;
    FillRect(Rect(0, 0, Width, Height));
    Pen.Color := clBlack;
    Pen.Width := 1;
    Pen.Mode := pmCopy;
    Pen.Style := psSolid;
    Brush.Color := clWhite;
  end;

  SelectClipRgn(Canvas.Handle, h);
  for i := 0 to Pages.Count - 1 do            // drawing page background
    if Pages[i]^.Visible then
    begin
      r := Pages[i]^.r;
      OffsetRect(r, Preview.ofx, Preview.ofy);
      Canvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);
      Canvas.Polyline([Point(r.Left + 1, r.Bottom),
                       Point(r.Right, r.Bottom),
                       Point(r.Right, r.Top + 1)]);
    end;

  for i := 0 to Pages.Count - 1 do           // drawing page content
  begin
    if Pages[i]^.Visible then
    begin
      r := Pages[i]^.r;
      OffsetRect(r, Preview.ofx, Preview.ofy);
      if Pages[i]^.pgMargins then
        Pages.Draw(i, Canvas, r)
      else
      begin
        with Preview, Pages[i]^.PrnInfo do
        begin
          r1.Left := Round(Ofx * per);
          r1.Top := Round(Ofy * per);
          r1.Right := r1.Left + Round(Pw * per);
          r1.Bottom := r1.Top + Round(Ph * per);
          Inc(r1.Left, r.Left); Inc(r1.Right, r.Left);
          Inc(r1.Top, r.Top); Inc(r1.Bottom, r.Top);
        end;
        Pages.Draw(i, Canvas, r1);
      end;
    end
    else
      Pages.Draw(i, Canvas, Rect(0, 0, 0, 0)); // remove it from cache
  end;
  DeleteObject(h);
end;

procedure TfrPBox.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i, k, PP: Integer;
  pt: TPoint;
  AInfo:string;
begin
  if Preview.EMFPages = nil then Exit;
  with Preview do
  if Button = mbLeft then
  begin
    Pt:=Point(X - Preview.ofx, Y - Preview.ofy);
    for i := 0 to TfrEMFPages(EMFPages).Count - 1 do
      if PtInRect(TfrEMFPages(EMFPages)[i]^.r, Pt) then
      begin
        if TfrEMFPages(EMFPages).DoMouseClick(i, Point(Round((pt.X - TfrEMFPages(EMFPages)[i]^.r.Left) / per), Round((pt.Y - TfrEMFPages(EMFPages)[i]^.r.Top) / per)), AInfo) then
        begin
          K:=Pos ('@', AInfo);
          if (K > 0) then
          begin
            PP:=StrToIntDef(Copy(AInfo, K+1, 255), -1);
            if (PP>0) and (K<TfrEMFPages(EMFPages).Count) then
            begin
              CurPage := PP;
              SetToCurPage;
              CurPage := PP;
              ShowPageNum;
            end;
          end;
          Exit;
        end;

        CurPage := i + 1;
        SetToCurPage;
        CurPage := i + 1;
        ShowPageNum;
        break;
      end;
  end
  else
  if Button = mbRight then
  begin
    pt := Self.ClientToScreen(Point(X, Y));
    if frDesigner <> nil then
    begin
      N4.Visible := True;
      N5.Visible := True;
      N6.Visible := True;
      N7.Visible := True;
    end;
    if THackControl(Preview.PreviewPanel.Parent).PopupMenu = nil then
      ProcMenu.Popup(pt.x, pt.y) else
      THackControl(Preview.PreviewPanel.Parent).PopupMenu.Popup(pt.x, pt.y);
  end;
end;

type
  THackView = class(TfrMemoView);

procedure TfrPBox.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  E:TfrEMFPages;
  i:integer;
  P:TPoint;
  C:TCursor;
  S:string;
  V:TfrView;
begin
  if not Assigned(Preview.EMFPages) then Exit;
  E:=TfrEMFPages(Preview.EMFPages);
  P:=Point(X - Preview.ofx, Y - Preview.ofy);
  for i := 0 to E.Count - 1 do
    if PtInRect(E[i]^.R, P) then
    begin
      V:=E.DoMouseMove(i, Point(Round((P.X - E[i]^.R.Left) / Preview.per), Round((P.Y - E[i]^.r.Top) / Preview.per)), C, S);
      if Assigned(V) then
        Cursor:=C
      else
        Cursor:=crDefault;

      if FCurView <> V then
      begin
        if Assigned(FCurView) and (FCurView is TfrMemoView) then
        begin
          THackView(FCurView).DoMouseLeave;
//          THackView(FCurView).Invalidate;
        end;
        FCurView:=V;
        if Assigned(FCurView) and (FCurView is TfrMemoView) then
        begin
          THackView(FCurView).DoMouseEnter;
//          THackView(FCurView).Invalidate;
        end;
//        Invalidate;
      end;
      Break;
    end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TfrPBox.DblClick;
begin
  if Preview.EMFPages = nil then Exit;
  with Preview do
    if N5.Visible then EditBtnClick(nil);
  FCurView:=nil;
end;


{----------------------------------------------------------------------------}
procedure TfrPreviewForm.FormCreate(Sender: TObject);
begin
  PBox := TfrPBox.Create(Self);
  with PBox do
  begin
    Parent := ScrollBox1;
    Align := alClient;
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Color := clGray;
    Preview := Self;
    Tag := 207;
    OnMouseWheelDown := @MouseWheelDown;
    OnMouseWheelUp   := @MouseWheelUp;
  end;

  N1.Caption := sPreviewFormPW;
  N2.Caption := sPreviewFormWhole;
  N3.Caption := sPreviewForm2Pg;
  N5.Caption := sPreviewFormEdit;
  N6.Caption := sPreviewFormAdd;
  N7.Caption := sPreviewFormDel;

  ZoomBtn.Hint := sPreviewFormScale;
  LoadBtn.Hint := sPreviewFormOpen;
  SaveBtn.Hint := sPreviewFormSave;
  PrintBtn.Hint := sPreviewFormPrint;
  ExitBtn.Hint := sPreviewFormClose;
  FindBtn.Hint := sPreviewFormFind;

  // TODO:  ADD hints to new buttons
  CreateExportFilterItems;
end;

procedure TfrPreviewForm.FormDestroy(Sender: TObject);
begin
  if EMFPages <> nil then
    TfrEMFPages(EMFPages).Free;
  PBox.Free;
end;

procedure TfrPreviewForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TfrPreviewForm.FormActivate(Sender: TObject);
begin
  Application.HelpFile := 'FRuser.hlp';
end;

procedure TfrPreviewForm.FormDeactivate(Sender: TObject);
begin
  Application.HelpFile := HF;
end;

procedure TfrPreviewForm.Show_Modal(ADoc: Pointer);
var
  GrayedButtons: Boolean;
begin
  Connect(ADoc);

  if not (csDesigning in TfrReport(Doc).ComponentState) then
  begin
    FindBtn.Visible := pbFind in TfrReport(Doc).PreviewButtons;
    ZoomBtn.Visible := pbZoom in TfrReport(Doc).PreviewButtons;
    SaveBtn.Visible := (pbSave in TfrReport(Doc).PreviewButtons) and not ((ExportFilters.Count = 0) and (roHideDefaultFilter in TfrReport(Doc).Options));
    LoadBtn.Visible := pbLoad in TfrReport(Doc).PreviewButtons;
    PrintBtn.Visible := pbPrint in TfrReport(Doc).PreviewButtons;
    ExitBtn.Visible := pbExit in TfrReport(Doc).PreviewButtons;
    if not ZoomBtn.Visible then
      frTBSeparator1.Hide;
  end;

  PrintBtn.Enabled := Printer.Printers.Count > 0;
  if frDesigner = nil then
  begin
    N4.Visible := False;
    N5.Visible := False;
    N6.Visible := False;
    N7.Visible := False;
  end;

  case TfrReport(Doc).InitialZoom of
    pzPageWidth: LastScaleMode := mdPageWidth;
    pzOnePage: LastScaleMode := mdOnePage;
    pzTwoPages:  LastScaleMode := mdTwoPages;
  end;


  RedrawAll;
  HScrollBar.Position := 0;
  VScrollBar.Position := 0;

  GrayedButtons := TfrReport(Doc).GrayedButtons;
  (*
  //TODO: designtime options are not saved so no restore,
  //      see lr_desgn.pas:TfrDesignerForm.SaveState;
  {$IFDEF MSWINDOWS}
  if frDesigner <> nil then
  begin
    Ini := TRegIniFile.Create('Software\FastReport\' + Application.Title);
    GrayedButtons := Ini.ReadBool('Form\' + frDesigner.Name, 'GrayButtons', False);
    Ini.Free;
  end;
  {$ENDIF}
  *)
  SetGrayedButtons(GrayedButtons);

  HF := Application.HelpFile;
  {$IFDEF DebugLR}
  DebugLn('TfrReport(Doc).ModalPreview=',BoolToStr(TfrReport(Doc).ModalPreview));
  {$ENDIF}
  if TfrReport(Doc).ModalPreview then
  begin
    Visible:=False;
    Enabled:=True;
    ShowModal;
  end
  else Show;
end;

function TfrPreviewForm.Print: boolean;
var
  Pages: String;
  ind: Integer;

  function RebuildReport: boolean;
  begin
    result := true;
    if TfrReport(Doc).CanRebuild then
    begin
      if TfrReport(Doc).ChangePrinter(ind, Printer.PrinterIndex) then
      begin
        TfrEMFPages(EMFPages).Free;
        EMFPages := nil;
        TfrReport(Doc).PrepareReport;
        Connect(Doc);
      end
      else
        result := false;
    end;
  end;

  procedure PrintReport(NumCopies: Integer);
  begin
    ConnectBack;
    TfrReport(Doc).PrintPreparedReport(Pages, NumCopies);
    Connect(Doc);
    RedrawAll;
  end;

begin
  result := false;
  if (EMFPages = nil) or (Printer.Printers.Count = 0) then Exit;
  ind := Printer.PrinterIndex;
  {$IFDEF PRINTDIALOG_NATIVE_PRINTDIALOG}
  if TfrReport(Doc).DefaultCopies<1 then
    prnDialog.Copies := 1
  else
    prnDialog.Copies:= TfrReport(Doc).DefaultCopies;
  prnDialog.MaxPage := TfrEMFPages(EMFPages).Count;
  prnDialog.MinPage:=1;
  prnDialog.FromPage := 1;
  prnDialog.ToPage := prnDialog.MaxPage;
  if prnDialog.Execute then begin
    if not RebuildReport then
      exit;
    Pages := format('%d-%d',[prnDialog.FromPage,prnDialog.ToPage]);
    PrintReport(prnDialog.Copies);
  end;
  {$ELSE}
  frPrintForm := TfrPrintForm.Create(nil);
  frPrintForm.E1.Value:=TfrReport(Doc).DefaultCopies;
  frPrintForm.cbCollate.Checked:=TfrReport(Doc).DefaultCollate;
//  with frPrintForm do
//  begin
    if frPrintForm.ShowModal = mrOk then
    begin
      if TfrReport(Doc).RebuildPrinter and ((Printer.PrinterIndex <> ind) or Prn.UseVirtualPrinter) then
      begin
        if not RebuildReport then
          exit;
      end;

      if frPrintForm.RB1.Checked then
        Pages := ''
      else
        if frPrintForm.RB2.Checked then
           Pages := IntToStr(CurPage)
        else
           Pages := frPrintForm.E2.Text;

      TfrReport(Doc).DefaultCollate:=frPrintForm.cbCollate.Checked;
      PrintReport(frPrintForm.E1.Value);
    end;
    frPrintForm.Free;
//  end;
  {$ENDIF}
  result := true;
end;

procedure TfrPreviewForm.CreateExportFilterItems;
var
  M: TMenuItem;
  i: Integer;
  B:TbitMap;
begin
  if lrExportFilters.Count>0 then
  begin
    PopupMenu1.Items.Clear;
    for i:=0 to lrExportFilters.Count-1 do
    begin
      M:=TMenuItem.Create(Self);
      M.Tag:=I;
      M.Caption:=TlrPreviewToolsAbstract(lrExportFilters[i]).Caption;
      M.OnClick:=@ExportFilterItemExecClick;

      B := TbitMap.Create;
      B.LoadFromResourceName(HInstance, TlrPreviewToolsAbstract(lrExportFilters[i]).ClassName);
      M.Bitmap.Assign(B);
      B.Free;

      PopupMenu1.Items.Add(M);
    end;
  end
  else
    SpeedButton1.Visible:=false;
end;

procedure TfrPreviewForm.ExportFilterItemExecClick(Sender: TObject);
var
  i:integer;
begin
  i:=TMenuItem(Sender).Tag;
  ConnectBack;
  TlrPreviewToolsAbstract(lrExportFilters[i]).Execute(TfrReport(Doc));
  Connect(Doc);
  Invalidate;
end;

function TfrPreviewForm.ExportToWithFilterIndex(AFilterIndex: Integer;
  const AFileName: string):boolean;
var
  S, S1:string;
  i: SizeInt;
begin
  if (AFilterIndex<0) or (AFilterIndex>=ExportFilters.Count) then
    raise exception.Create(sExportFilterIndexError);
  ConnectBack;

  S:=Trim(AFileName);
  if (S <> '') and (ExtractFileExt(S) = '') and (S[Length(S)]<>'.') then
  begin
    S1:=ExportFilters[AFilterIndex].FilterExt;
    i:=Pos('.', S1);
    if i>0 then
      Delete(S1, 1, i-1);
    S:=S+S1;
  end;

  TfrReport(Doc).ExportTo(ExportFilters[AFilterIndex].ClassRef, S);
  Connect(Doc);
  Result:=true;
end;

procedure TfrPreviewForm.Connect(ADoc: Pointer);
begin
  Doc := ADoc;
  if EMFPages <> nil then
    TfrEMFPages(EMFPages).Free;
  EMFPages := TfrReport(Doc).EMFPages;
  TfrReport(Doc).EMFPages := TfrEMFPages.Create(TfrReport(Doc));
end;

procedure TfrPreviewForm.ConnectBack;
begin
  TfrReport(Doc).EMFPages.Free;
  TfrReport(Doc).EMFPages := TfrEMFPages(EMFPages);
  EMFPages := nil;
end;

procedure TfrPreviewForm.ScrollbarDelta(const VertDelta, HorzDelta: Integer);
begin
  if VertDelta<>0 then
    VScrollBar.Position:=VScrollBar.Position + VertDelta;
  if HorzDelta<>0 then
    HScrollBar.Position:=HScrollBar.Position + HorzDelta;
end;

procedure TfrPreviewForm.MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in shift then
    BtZoomoutClick(sender)
  else
  if ssShift in Shift then
    ScrollbarDelta(VScrollbar.SmallChange, 0)
  else
    ScrollBarDelta(VScrollbar.LargeChange, 0);
  Handled := True;
end;

procedure TfrPreviewForm.MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if ssCtrl in shift then
    BtZoomInClick(sender)
  else
  if ssShift in Shift then
    ScrollbarDelta(-VScrollbar.SmallChange, 0)
  else
    ScrollBarDelta(-VScrollbar.LargeChange, 0);
  Handled := True;
end;

{procedure TfrPreviewForm.WMGetMinMaxInfo(var Msg: TWMGetMinMaxInfo);
begin
  with Msg.MinMaxInfo^ do
  begin
    ptMaxSize.x := Screen.Width;
    ptMaxSize.y := Screen.Height;
    ptMaxPosition.x := 0;
    ptMaxPosition.y := 0;
  end;
end;
}
procedure TfrPreviewForm.SetGrayedButtons(Value: Boolean);
var
  i: Integer;
  c: TControl;
begin
  for i := 0 to PanTop.ControlCount - 1 do
  begin
    c := PanTop.Controls[i];
    if c is TBitBtn then
      TBitBtn(c).Enabled := Value; //** GrayedInactive := Value;
  end;
end;

procedure TfrPreviewForm.RedrawAll;
var
  i: Integer;
begin
  if EMFPages = nil then Exit;
  per := LastScale;
  mode := LastScaleMode;
  if mode = mdPageWidth then
    N1.Checked := True
  else if mode = mdOnePage then
    N2.Checked := True
  else if mode = mdTwoPages then
    N3.Checked := True
  else
    for i := 0 to ProcMenu.Items.Count - 1 do
      if ProcMenu.Items[i].Tag = per * 100 then
        ProcMenu.Items[i].Checked := True;

  CurPage := 1;
  ShowPageNum;
  ofx := 0; ofy := 0; OldH := 0; OldV := 0;
  HScrollBar.Position := 0;
  VScrollBar.Position := 0;
  FormResize(nil);
  for i := 0 to TfrEMFPages(EMFPages).Count - 1 do
  begin
    TfrEMFPages(EMFPages)[i]^.Visible := False;
    TfrEMFPages(EMFPages).Draw(i, Canvas, Rect(0, 0, 0, 0));
  end;
  PBox.Repaint;
end;

procedure TfrPreviewForm.FormResize(Sender: TObject);
var
  i, j, y, d, nx, dwx, dwy, maxx, maxy, maxdy, curx: Integer;
  Pages: TfrEMFPages;
begin
  if EMFPages = nil then Exit;
  Pages := TfrEMFPages(EMFPages);
  PaintAllowed := False;
  with Pages[CurPage - 1]^.PrnInfo do
  begin
    dwx := Pgw; dwy := Pgh;
  end;
  case mode of
    mdNone:;
    mdPageWidth: per := (PBox.Width - 20) / dwx;
    mdOnePage: per := (PBox.Height - 20) / dwy;
    mdTwoPages: per := (PBox.Width - 30) / (2 * dwx);
  end;
  ZoomBtn.Caption := IntToStr(Round(per * 100)) + '%';
  nx := 0; maxx := 10; j := 0;
  for i := 0 to Pages.Count - 1 do
  begin
    d := maxx + 10 + Round(Pages[i]^.PrnInfo.Pgw * per);
    if d > PBox.Width then
    begin
      if nx < j then nx := j;
      j := 0;
      maxx := 10;
    end
    else
    begin
      maxx := d;
      Inc(j);
      if i = Pages.Count - 1 then
        if nx < j then nx := j;
    end;
  end;
  if nx = 0 then nx := 1;
  if mode = mdOnePage then nx := 1;
  if mode = mdTwoPages then nx := 2;
  y := 10;
  i := 0;
  maxx := 0; maxy := 0;
  while i < Pages.Count do
  begin
    j := 0; maxdy := 0; curx := 10;
    while (j < nx) and (i + j < Pages.Count) do
    begin
      dwx := Round(Pages[i + j]^.PrnInfo.Pgw * per);
      dwy := Round(Pages[i + j]^.PrnInfo.Pgh * per);
      if (nx = 1) and (dwx < PBox.Width) then
      begin
        d := (PBox.Width - dwx) div 2;
        Pages[i + j]^.r := Rect(d, y, d + dwx, y + dwy);
      end
      else
        Pages[i + j]^.r := Rect(curx, y, curx + dwx, y + dwy);
      if maxx < Pages[i + j]^.r.Right then
        maxx := Pages[i + j]^.r.Right;
      if maxy < Pages[i + j]^.r.Bottom then
        maxy := Pages[i + j]^.r.Bottom;
      Inc(j);
      if maxdy < dwy then maxdy := dwy;
      Inc(curx, dwx + 10);
    end;
    Inc(y, maxdy + 10);
    Inc(i, nx);
  end;

  // REMOVE: scrolls size hacks
  //VScrollBar.Height := RPanel.Height - PgUp.height - PgDown.height;
  //if RPanel.Visible then
  //  HScrollbar.Width := BPanel.Width - HScrollbar.Left - RPanel.Width;

  if maxx < 0 then maxx := 0 else Inc(maxx, 10);
  if maxy < 0 then maxy := 0 else Inc(maxy, 10);
  
  HScrollBar.Max := maxx;
  VScrollBar.Max := maxy;
  VScrollBar.PageSize := Scrollbox1.ClientHeight;
  Hscrollbar.PageSize := Scrollbox1.ClientWidth;
  HScrollBar.Enabled := maxx <> 0;
  VScrollBar.Enabled := maxy <> 0;
  
  SetToCurPage;
  PaintAllowed := True;
  LastScale := per;
  LastScaleMode := mode;
end;

procedure TfrPreviewForm.BtZoomOutClick(Sender: TObject);
begin
  if EMFPages = nil then Exit;
  ofx := 0;
  if LastScale > 0.1 then
  begin
    mode := mdNone;
    per := (LastScale - 0.1);
    HScrollBar.Position := 0;
    FormResize(nil);
    PBox.Repaint;
  end;
end;

procedure TfrPreviewForm.BtZoomInClick(Sender: TObject);
begin
    if EMFPages = nil then Exit;
  ofx := 0;
  if LastScale < 100 then
  begin
    mode := mdNone;
    per := (LastScale + 0.1);
    HScrollBar.Position := 0;
    FormResize(nil);
    PBox.Repaint;
  end;
end;

procedure TfrPreviewForm.BtPgFirstClick(Sender: TObject);
begin
  if EMFPages = nil then Exit;
  if CurPage > 1 then
    CurPage := 1;
  ShowPageNum;
  SetToCurPage;
end;

procedure TfrPreviewForm.BtPgLastClick(Sender: TObject);
begin
  if EMFPages = nil then Exit;
  if CurPage < TfrEMFPages(EMFPages).Count then
    CurPage := TfrEMFPages(EMFPages).Count;
  ShowPageNum;
  SetToCurPage;
end;

procedure TfrPreviewForm.SpeedButton1Click(Sender: TObject);
var
  R:TPoint;
begin
  R:=ClientToScreen(Point(SpeedButton1.Left, SpeedButton1.Top + SpeedButton1.Height));
  PopupMenu1.PopUp(R.X, R.Y);
end;

procedure TfrPreviewForm.SetToCurPage;
begin
  if EMFPages = nil then Exit;
  if ofy <> TfrEMFPages(EMFPages)[CurPage - 1]^.r.Top - 10 then
    VScrollBar.Position := TfrEMFPages(EMFPages)[CurPage - 1]^.r.Top - 10;
    
  PBox.Invalidate;
end;

procedure TfrPreviewForm.ShowPageNum;
begin
  if EMFPages = nil then Exit;
  LbPanel.Caption := sPg + ' ' + IntToStr(CurPage) + '/' +
    IntToStr(TfrEMFPages(EMFPages).Count);

  if Assigned(FOnScrollPage) then
    FOnScrollPage(Self);
end;

procedure TfrPreviewForm.VScrollBarChange(Sender: TObject);
var
  {$IFDEF WIN32}
  r: TRect;
  pp: Integer;
  {$ENDIF}
  p: Integer;
  i: integer;
  Pages: TfrEMFPages;
begin
  if EMFPages = nil then Exit;
  p := VScrollBar.Position;
  ofy := -p;
  {$IFDEF WIN32}
  pp := OldV - p;
  OldV := p;
  r := Rect(0, 0, PBox.Width, PBox.Height);
  ScrollWindowEx(PBox.Handle, 0, pp, @r, @r, 0, nil, SW_INVALIDATE);
  UpdateWindow(Pbox.Handle);
  {$ELSE}
  PBox.Invalidate;
  {$ENDIF}
  Pages := TfrEMFPages(EMFPages);
  for i := 0 to Pages.Count-1 do
    if (Pages[i]^.r.Top < -ofy + 11) and
      (Pages[i]^.r.Bottom > -ofy + 11) then
    begin
      CurPage := i + 1;
      ShowPageNum;
      break;
    end;
end;

procedure TfrPreviewForm.HScrollBarChange(Sender: TObject);
var
  p: Integer;
{$IFDEF WIN32}
  pp: Integer;
  r: TRect;
{$ENDIF}
begin
  if EMFPages = nil then Exit;
  p := HScrollBar.Position;
  ofx := -p;
  {$IFDEF WIN32}
  pp := OldH - p;
  OldH := p;
  r := Rect(0, 0, PBox.Width, PBox.Height);
  ScrollWindowEx(PBox.Handle, pp, 0, @r, @r, 0, nil, SW_INVALIDATE);
  UpdateWindow(Pbox.Handle);
  {$ELSE}
  PBox.Invalidate;
  {$ENDIF}
end;

procedure TfrPreviewForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if EMFPages = nil then Exit;
  if Key in [vk_Up, vk_Down, vk_Prior, vk_Next] then
    if VScrollBar.Enabled then VScrollBar.SetFocus;
  if Key in [vk_Left, vk_Right] then
    if HScrollBar.Enabled then HScrollBar.SetFocus;
  if Key = vk_Up then
    ScrollBarDelta(-VScrollBar.SmallChange, 0)
  else if Key = vk_Down then
    ScrollBarDelta(VScrollBar.SmallChange, 0)
  else if Key = vk_Left then
    ScrollBarDelta(0, -HScrollBar.SmallChange)
  else if Key = vk_Right then
    ScrollBarDelta(0, HScrollBar.SmallChange)
  else if Key = vk_Prior then
    if ssCtrl in Shift then
      PgUpClick(nil) else
      ScrollBarDelta(-VScrollBar.LargeChange, 0)
  else if Key = vk_Next then
    if ssCtrl in Shift then
      PgDownClick(nil) else
      ScrollBarDelta(VScrollBar.LargeChange, 0)
  else if Key = vk_Space then
    ZoomBtnClick(nil)
  else if Key = vk_Escape then
    ExitBtnClick(nil)
  else if Key = vk_Home then
    if ssCtrl in Shift then
      VScrollBar.Position := 0 else
      Exit
  else if Key = vk_End then
    if ssCtrl in Shift then
    begin
      CurPage := TfrEMFPages(EMFPages).Count;
      SetToCurPage;
    end
    else Exit
  else if ssCtrl in Shift then
  begin
    if Chr(Key) = 'O' then LoadBtnClick(nil)
    else if Chr(Key) = 'S' then SaveBtnClick(nil)
    else if (Chr(Key) = 'P') and PrintBtn.Visible then PrintBtnClick(nil)
    else if Chr(Key) = 'F' then FindBtnClick(nil)
    else if (Chr(Key) = 'E') and N5.Visible then EditBtnClick(nil)
  end
  else if Key = vk_F3 then
  begin
    if SearchFindStr <> '' then
    begin
      if SearchLastFoundPage <> CurPage - 1 then
      begin
        SearchLastFoundPage := CurPage - 1;
        SearchLastFoundObject := 0;
      end;
      FindText;
    end;
  end
  else if (Key = vk_Delete) and N5.Visible then
    DelPageBtnClick(nil)
  else if (Key = vk_Insert) and N5.Visible then
    NewPageBtnClick(nil)
  else Exit;
  Key := 0;
end;

procedure TfrPreviewForm.PgUpClick(Sender: TObject);
begin
  if EMFPages = nil then Exit;
  if CurPage > 1 then Dec(CurPage);
  ShowPageNum;
  SetToCurPage;
end;

procedure TfrPreviewForm.PgDownClick(Sender: TObject);
begin
  if EMFPages = nil then Exit;
  if CurPage < TfrEMFPages(EMFPages).Count then
    Inc(CurPage);
  ShowPageNum;
  SetToCurPage;
end;

procedure TfrPreviewForm.ZoomBtnClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt := ClientToScreen(Point(ZoomBtn.Left, ZoomBtn.Top + ZoomBtn.Height + 2));
  N4.Visible := False;
  N5.Visible := False;
  N6.Visible := False;
  N7.Visible := False;
  ProcMenu.Popup(pt.x + 4, pt.y + 6);
end;

procedure TfrPreviewForm.N3Click(Sender: TObject);
begin
  if EMFPages = nil then Exit;
  ofx := 0;
  with Sender as TMenuItem do
  begin
    case Tag of
      1: mode := mdPageWidth;
      2: mode := mdOnePage;
      3: mode := mdTwoPages;
    else
      begin
        mode := mdNone;
        per := Tag / 100;
      end;
    end;
    Checked := True;
  end;
  HScrollBar.Position := 0;
  FormResize(nil);
  PBox.Repaint;
end;

procedure TfrPreviewForm.LoadBtnClick(Sender: TObject);
begin
  if EMFPages = nil then Exit;
  OpenDialog.Filter := sRepFile + ' (*.frp)|*.frp';
  with OpenDialog do
   if Execute then
     LoadFromFile(FileName);
end;

procedure TfrPreviewForm.SaveBtnClick(Sender: TObject);
var
  i, Index: Integer;
  FilterStr: string;
  FilterInfo: TExportFilterItem;
  FExtList:TStringList;
begin
  if EMFPages = nil then Exit;
  FExtList:=TStringList.Create;
  try
    Index := 1;
    if not (roHideDefaultFilter in TfrReport(Doc).Options) then
    begin
      FExtList.Add('*.frp');
      FilterStr := sRepFile + ' (*.frp)|*.frp';
    end else
      FilterStr := '';

    for i := 0 to ExportFilters.Count - 1 do
    begin
      FilterInfo := ExportFilters[i];
      if FilterInfo.Enabled then
      begin
        FExtList.AddObject(FilterInfo.FilterExt, TObject(PtrInt(i+1)));
        if FilterStr <> '' then
          FilterStr := FilterStr + '|';
        FilterStr := FilterStr + FilterInfo.FilterDesc + '|' + FilterInfo.FilterExt;
      end;
    end;

    SaveDialog.Filter := FilterStr;
    SaveDialog.FilterIndex := Index;
    if SaveDialog.Execute then
    begin
      Index := SaveDialog.FilterIndex - 1;
      if fExtList.Objects[Index]=nil then
        SaveToFile(SaveDialog.Filename) // using .frp
      else
      begin
        Index := PtrInt(fExtList.Objects[Index])-1;
        ExportToWithFilterIndex(Index, SaveDialog.FileName);
      end;
    end;

  finally
    FExtList.Free;
    ScrollBox1.Invalidate;
  end;
end;

procedure TfrPreviewForm.PrintBtnClick(Sender: TObject);
begin
  Print;
end;

procedure TfrPreviewForm.ExitBtnClick(Sender: TObject);
begin
  if Doc = nil then Exit;
  if TfrReport(Doc).ModalPreview then
    ModalResult := mrOk else
    Close;
end;

procedure TfrPreviewForm.LoadFromFile(const aName: String);
begin
  if Doc = nil then Exit;
  TfrEMFPages(EMFPages).Free;
  EMFPages := nil;
  TfrReport(Doc).LoadPreparedReport(aName);
  Connect(Doc);
  CurPage := 1;
  FormResize(nil);
  PaintAllowed := False;
  ShowPageNum;
  SetToCurPage;
  PaintAllowed := True;
  PBox.Repaint;
end;

procedure TfrPreviewForm.SaveToFile(const aName:String);
begin
  if Doc = nil then Exit;
  ConnectBack;
  TfrReport(Doc).SavePreparedReport(ChangeFileExt(aName, '.frp'));
  Connect(Doc);
end;

function TfrPreviewForm.FindInEMFPages: boolean;
var
  P:PfrPageInfo;
  V:TfrObject;
  i, j, SK:integer;
  Pages : TfrEMFPages;
  S:string;
begin
  Result:=false;
  if not Assigned(EMFPages) then exit;

  Pages := TfrEMFPages(EMFPages);
  Pages.ResetFindData;

  for i:=SearchLastFoundPage to Pages.Count - 1 do
  begin
    P:=Pages[i];

    if not Assigned(P^.Page) then
      Pages.ObjectsToPage(i);

    if i = SearchLastFoundPage then
      SK:=SearchLastFoundObject + 1
    else
      SK:=0;

    for j:=SK to P^.Page.Objects.Count - 1 do
    begin
      V:=TfrView(P^.Page.Objects[j]);
      if Assigned(V) and (V is TfrMemoView) then
      begin
        S:=TfrMemoView(V).Memo.Text;
        if not SearchCaseSensitive then
          S := UTF8UpperCase(S);

        if UTF8Pos(SearchFindStr, S)>0 then
        begin
          TfrMemoView(V).FindHighlight:=true;
          CurPage:=i + 1;

          SearchLastFoundPage:=i;
          SearchLastFoundObject:=j;

          ShowPageNum;
          SetToCurPage;
          Result:=true;
          exit;
        end;
      end;
    end;

  end;
end;

procedure TfrPreviewForm.FindText;
begin
  PaintAllowed := False;
  if not FindInEMFPages then
    ShowMessage(sFindTextNotFound);
  PaintAllowed := True;
end;

procedure TfrPreviewForm.FindBtnClick(Sender: TObject);
var
  SrchForm: TfrPreviewSearchForm;
begin
  if Doc = nil then Exit;

  SrchForm := TfrPreviewSearchForm.Create(nil);
  SrchForm.Edit1.Text:=SearchFindStr;
  SrchForm.GroupBox1.Checked[0]:=SearchCaseSensitive;
  SrchForm.GroupBox2.ItemIndex:=SearchDirecion;


  if SrchForm.ShowModal = mrOk then
  begin
    SearchFindStr := SrchForm.Edit1.Text;
    SearchCaseSensitive := SrchForm.GroupBox1.Checked[0];// CB1.Checked;
    SearchDirecion:=SrchForm.GroupBox2.ItemIndex;

    if not SearchCaseSensitive then
      SearchFindStr := UTF8UpperCase(SearchFindStr);
    if SrchForm.GroupBox2.ItemIndex = 0 {RB1.Checked} then
    begin
      SearchLastFoundPage := 0;
      SearchLastFoundObject := 0;
    end
    else
    if SearchLastFoundPage <> CurPage - 1 then
    begin
      SearchLastFoundPage := CurPage - 1;
      SearchLastFoundObject := 0;
    end;
    FindText;
  end;
  SrchForm.Free;
end;

procedure TfrPreviewForm.EditBtnClick(Sender: TObject);
var
  R: TfrReport;
begin
  if (Doc = nil) or not TfrReport(Doc).ModifyPrepared then Exit;
{  ConnectBack;
  TfrReport(Doc).EditPreparedReport(CurPage - 1);
  Connect(Doc);}

  R:=TfrReport.Create(nil);
  R.EMFPages.Free;
  R.EMFPages := TfrEMFPages(EMFPages);
  EMFPages := nil;
  R.EditPreparedReport(CurPage - 1);

  if EMFPages <> nil then
    TfrEMFPages(EMFPages).Free;

  EMFPages := R.EMFPages;
  R.EMFPages:=nil;
//  TfrReport(Doc).EMFPages := TfrEMFPages.Create(TfrReport(Doc));

  R.Free;
  RedrawAll;
end;

procedure TfrPreviewForm.DelPageBtnClick(Sender: TObject);
begin
  if Doc = nil then Exit;
  if TfrEMFPages(EMFPages).Count > 1 then
    if MessageBox(0, PChar(sRemovePg), PChar(sConfirm),
      mb_YesNo + mb_IconQuestion) = mrYes then
    begin
      TfrEMFPages(EMFPages).Delete(CurPage - 1);
      RedrawAll;
    end;
end;

procedure TfrPreviewForm.NewPageBtnClick(Sender: TObject);
begin
  if Doc = nil then Exit;
  TfrEMFPages(EMFPages).Insert(CurPage - 1, TfrReport(Doc).Pages[0]);
  RedrawAll;
end;

procedure TfrPreviewForm.HelpBtnClick(Sender: TObject);
begin
  Screen.Cursor := crHelp;
  SetCapture(Handle);
end;

procedure TfrPreviewForm.FormMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Screen.Cursor := crDefault;
end;

end.

