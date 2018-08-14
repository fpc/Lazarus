unit GraphStat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, Graphics, Dialogs,
  {$ifndef POCHECKERSTANDALONE}
  LazIDEIntf,
  {$else}
  Process, Utf8Process,
  {$endif}
  ExtCtrls, PoFamilies, PoFamilyLists, PoCheckerConsts, LCLProc, StdCtrls, ComCtrls, Menus,
  PoCheckerSettings, LCLIntf, Translations;


type

  { TGraphStatForm }

  TGraphStatForm = class(TForm)
    Separator1MenuItem: TMenuItem;
    RefreshCurrMenuItem: TMenuItem;
    RefreshAllMenuItem: TMenuItem;
    Separator2MenuItem: TMenuItem;
    POEditorMenuItem: TMenuItem;
    ExtEditorMenuItem: TMenuItem;
    IDEEditorMenuItem: TMenuItem;
    ContextPopupMenu: TPopupMenu;
    StatusLabel: TLabel;
    ListView: TListView;
    TranslatedLabel: TLabel;
    UnTranslatedLabel: TLabel;
    FuzzyLabel: TLabel;
    LegendPanel: TPanel;
    TranslatedShape: TShape;
    UnTranslatedShape: TShape;
    FuzzyShape: TShape;
    procedure ExtEditorMenuItemClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IDEEditorMenuItemClick(Sender: TObject);
    procedure ListViewMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X,
      Y: Integer);
    procedure ListViewMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure POEditorMenuItemClick(Sender: TObject);
    procedure RefreshAllMenuItemClick(Sender: TObject);
    procedure RefreshCurrMenuItemClick(Sender: TObject);
  private
    { private declarations }
    FPoFamilyList: TPoFamilyList;
    FPoFamilyStats: TPoFamilyStats;
    FImgList: TImageList;
    FOldHintHidePause: Integer;
    FSettings: TPoCheckerSettings;
    Fn: string;
    CurrentMasterPoFile: string;
    procedure LoadConfig;
    Procedure SaveConfig;
    function CreateBitmap(AStat: TStat): TBitmap;
    procedure AddToListView(AStat: TStat; ABmp: TBitmap);
    procedure DrawGraphs(Cnt: PtrInt);
    procedure MaybeOpenInLazIDE;
    procedure MaybeOpenInExternalEditor;
    procedure RefreshTranslations(All: boolean);
    procedure ConfigureContextPopUp(AdvancedMode: boolean);
  public
    { public declarations }
    property PoFamilyList: TPoFamilyList read FPoFamilyList write FPoFamilyList;
    property PoFamilyStats: TPoFamilyStats read FPoFamilyStats write FPoFamilyStats;
    property Settings: TPoCheckerSettings read FSettings write FSettings;
  end;

var
  GraphStatForm: TGraphStatForm;

implementation

{$R *.lfm}


{ TGraphStatForm }

const
  Radius = 40;   //when we have anti-aliasing we can reduce this
  BmpWH = 2 * Radius;
  clBackGround = clWhite;
  clTranslated = clGreen;
  clUnTranslated = clRed;
  clFuzzy = clFuchsia;

  {
  ListView
    @designtime
    AutoSort := False
    ReadOnly := True
    ScrollBars := ssAutoBoth
    ViewStyle := vsIcon

    @runtime
    Color := clBackGround
    LargeImages := FImgList;
    Align := alClient;

  }


procedure TGraphStatForm.FormShow(Sender: TObject);
begin
  FOldHintHidePause := Application.HintHidePause;
  Application.HintHidePause := 5000;
  LoadConfig;
  WindowState := Settings.GraphFormWindowState;
  Application.QueueAsyncCall(@DrawGraphs, FPoFamilyStats.Count);
end;

procedure TGraphStatForm.IDEEditorMenuItemClick(Sender: TObject);
begin
  MaybeOpenInLazIDE;
end;

procedure TGraphStatForm.ListViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  Item: TListItem;
  Index: Integer;
  AStat: TStat;
begin
  Item := Listview.GetItemAt(X, Y);
  if Assigned(Item) then
  begin
    Index := Item.Index;
    AStat := FPoFamilyStats.Items[Index];
    ListView.Hint := Format(sStatHint,[AStat.NrTranslated, AStat.PercTranslated,
                                       AStat.NrUnTranslated, AStat.PercUnTranslated,
                                       AStat.NrFuzzy, AStat.PercFuzzy, AStat.NrErrors]);
  end
  else
  begin
    ListView.Hint := '';
    Application.HideHint;
  end;

end;

procedure TGraphStatForm.ListViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  anItem: TListItem;
  anIndex: Integer;
  AStat: TStat;
  CurrScreenPoint: TPoint;
begin
  if Button = mbRight then
  begin
    anItem := Listview.GetItemAt(X, Y);
    if Assigned(anItem) then
    begin
      anIndex := anItem.Index;
      AStat := FPoFamilyStats.Items[anIndex];
      Fn := AStat.PoName;
      CurrentMasterPoFile := ExtractMasterNameFromChildName(AStat.PoName);
      ConfigureContextPopUp(ssShift in Shift);
      CurrScreenPoint := ListView.ClientToScreen(Point(X, Y));
      ContextPopupMenu.PopUp(CurrScreenPoint.X, CurrScreenPoint.Y);
    end;
  end;
end;

procedure TGraphStatForm.POEditorMenuItemClick(Sender: TObject);
begin
  if OpenDocument(Fn) = false then
    ShowMessage(Format(SOpenFail,[Fn]));
end;

procedure TGraphStatForm.RefreshAllMenuItemClick(Sender:
  TObject);
begin
  RefreshTranslations(true);
end;

procedure TGraphStatForm.RefreshCurrMenuItemClick(Sender: TObject);
begin
  RefreshTranslations(false);
end;

procedure TGraphStatForm.RefreshTranslations(All: boolean);
var
  i: integer;
  AbortUpdate: boolean;
begin
  //"All" parameter defines if we refresh current family only (false) or all families (true)
  StatusLabel.Visible := true;
  RefreshCurrMenuItem.Enabled := false;
  RefreshAllMenuItem.Enabled := false;
  try
    AbortUpdate := false;
    i := 0;
    while (i<PoFamilyList.Count) and (AbortUpdate=false) do
    begin
      try
        if All=true then
          StatusLabel.Caption := Format(sProcessingTranslationFamilyOf, [
            IntToStr(i + 1), IntToStr(PoFamilyList.Count)])
        else
          StatusLabel.Caption := sProcessingTranslationFamily;
        StatusLabel.Repaint;
        if All=true then
        begin
          UpdatePoFileTranslations(PoFamilyList.Items[i].MasterName);
          inc(i);
        end
        else
        begin
          UpdatePoFileTranslations(CurrentMasterPoFile);
          AbortUpdate := true;
        end;
      except
        on E: EPOFileError do
          if MessageDlg('POChecker', Format(
            sCannotWriteFileYouCanPressRetryToRefreshThisTransl, [LineEnding,
            E.ResFileName, LineEnding, LineEnding]), mtError, [mbRetry, mbAbort
            ], 0) = mrAbort then
            AbortUpdate := true;
      end;
    end;
  finally
    StatusLabel.Visible := false;
    RefreshCurrMenuItem.Enabled := true;
    RefreshAllMenuItem.Enabled := true;
  end;
end;

procedure TGraphStatForm.ConfigureContextPopUp(AdvancedMode: boolean);
begin
  //POEditorMenuItem is always shown, others only in advanced mode
  Separator1MenuItem.Visible := AdvancedMode;
  //ExtEditorMenuItem and IDEEditorMenuItem are invisible by default
  {$ifdef pocheckerstandalone}
  ExtEditorMenuItem.Visible := AdvancedMode;
  if Settings.ExternalEditorName <> '' then
    ExtEditorMenuItem.Enabled := true;
  {$else}
  IDEEditorMenuItem.Visible := AdvancedMode;
  {$endif}
  Separator2MenuItem.Visible := AdvancedMode;
  RefreshCurrMenuItem.Visible := AdvancedMode;
  RefreshAllMenuItem.Visible := AdvancedMode;
end;

procedure TGraphStatForm.LoadConfig;
var
  ARect: TRect;
begin
  if not Assigned(FSettings) then Exit;
  ARect := FSettings.GraphFormGeometry;
  //debugln('TGraphStatForm.LoadConfig: ARect = ',dbgs(ARect));
  if not IsDefaultRect(ARect) and IsValidRect(ARect) then
  begin
    ARect := FitToRect(ARect, Screen.WorkAreaRect);
    BoundsRect := ARect;
  end;
end;

procedure TGraphStatForm.SaveConfig;
begin
  //debugln('TGraphStatForm.SaveConfig: BoundsRect = ',dbgs(BoundsRect));
  if not Assigned(FSettings) then Exit;
  Settings.GraphFormWindowState := WindowState;
  if (WindowState = wsNormal) then
    Settings.GraphFormGeometry := BoundsRect
  else
    Settings.GraphFormGeometry := Rect(RestoredLeft, RestoredTop, RestoredLeft + RestoredWidth, RestoredTop + RestoredHeight);
end;

procedure TGraphStatForm.FormCreate(Sender: TObject);
begin
  Caption := sGrapStatFormCaption;
  POEditorMenuItem.Caption := sOpenFileInSystemPOEditor;
  ExtEditorMenuItem.Caption := sOpenFileInExternalEditor;
  IDEEditorMenuItem.Caption := sOpenFileInIDEEditor;
  RefreshCurrMenuItem.Caption := sRefreshCurrentTranslationFamily;
  RefreshAllMenuItem.Caption := sRefreshAllTranslationFamilies;
  TranslatedShape.Brush.Color := clTranslated;
  UnTranslatedShape.Brush.Color := clUntranslated;
  FuzzyShape.Brush.Color := clFuzzy;
  ListView.Color := clBackGround;
  ListView.Align := alClient;
end;

procedure TGraphStatForm.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  Application.HintHidePause := FOldHintHidePause;
end;

procedure TGraphStatForm.FormActivate(Sender: TObject);
begin
  //Doing this in TGraphStatForm.FormShow results in icons disappearing in Linux GTK2
  //DrawGraphs;
end;

procedure TGraphStatForm.ExtEditorMenuItemClick(Sender: TObject);
begin
  MaybeOpenInExternalEditor;
end;

procedure TGraphStatForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FImgList) then FImgList.Free;
  SaveConfig;
end;

function TGraphStatForm.CreateBitmap(AStat: TStat): TBitmap;
const
  FullCircle = 16 * 360;
  QMark = ' ? ';
var
  Bmp: TBitmap;
  Translated16Angle, UnTranslated16Angle, Fuzzy16Angle: Integer;
  PieRect: TRect;
  TextSize: TSize;
begin
  Bmp := TBitmap.Create;
  Bmp.SetSize(BmpWH,BmpWH);
  Bmp.Canvas.AntialiasingMode := amOn; // currently effective only with Qt
  PieRect := Rect(0,0,BmpWH, BmpWH);
  with Bmp do
  begin
    //Draw background
    Canvas.Brush.Color := clBackGround;
    Canvas.FillRect(PieRect);
    Canvas.Pen.Color := clBackGround;
    //All angles in 16th of a degree
    Translated16Angle     := Round(AStat.FracTranslated * FullCircle);
    UnTranslated16Angle   := Round(AStat.FracUntranslated * FullCircle);
    Fuzzy16Angle          := Round(AStat.FracFuzzy * FullCircle);

    if Translated16Angle > 0 then
    begin
      Canvas.Brush.Color:= clTranslated;
      if Translated16Angle = FullCircle then
        Canvas.Ellipse(PieRect)
      else
        Canvas.RadialPie(PieRect.Left,PieRect.Top,PieRect.Right,PieRect.Bottom,0,Translated16Angle);;
    end;

    if UnTranslated16Angle > 0 then
    begin
      Canvas.Brush.Color:= clUnTranslated;
      if UnTranslated16Angle = FullCircle then
        Canvas.Ellipse(PieRect)
      else
        Canvas.RadialPie(PieRect.Left,PieRect.Top,PieRect.Right,PieRect.Bottom,Translated16Angle,UnTranslated16Angle);;
    end;

    if Fuzzy16Angle > 0 then
    begin
      Canvas.Brush.Color:= clFuzzy;
      if Fuzzy16Angle = FullCircle then
        Canvas.Ellipse(PieRect)
      else
        Canvas.RadialPie(PieRect.Left,PieRect.Top,PieRect.Right,PieRect.Bottom,Translated16Angle+UnTranslated16Angle,Fuzzy16Angle);
    end;
    if AStat.NrErrors <> 0 then begin
      Bmp.Canvas.Font := Font;
      Canvas.Font.Size := BmpWH div 6;
      Canvas.Font.Style:= [fsBold];
      Canvas.Font.Color:= clRed;
      TextSize := Bmp.Canvas.TextExtent(QMark);
      Canvas.Brush.Color:= clBlack;
      Canvas.Rectangle(0,PieRect.Bottom-TextSize.cy-4,TextSize.cx+4,PieRect.Bottom);
      Canvas.Brush.Color:= clYellow;
      Canvas.TextOut(2,PieRect.Bottom-TextSize.cy-2,QMark);
    end;
  end;
  Result := Bmp;
end;

procedure TGraphStatForm.AddToListView(AStat: TStat; ABmp: TBitmap);
var
  ImgIndex: Integer;
  ListItem: TListItem;
begin
  ImgIndex := FImgList.AddMasked(ABmp, clBackGround);
  ListItem := ListView.Items.Add;
  ListItem.Caption := AStat.ShortPoName;
  ListItem.ImageIndex := ImgIndex;
end;

procedure TGraphStatForm.DrawGraphs(Cnt: PtrInt);
var
  Bmp: TBitmap;
  AStat: TStat;
  Index: Integer;
  Cur: TCursor;
begin
  if Assigned(FImgList) then FImgList.Free;
  FImgList := TImageList.CreateSize(BmpWH, BmpWH);
  ListView.Clear;
  ListView.LargeImages := FImgList;
  ListView.BeginUpdate;
  Cur := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    StatusLabel.Visible := True;
    for Index := 0 to FPoFamilyStats.Count - 1 do
    begin
      StatusLabel.Caption := Format(sCreatingIconXofY,[Index + 1, Cnt]);
      StatusLabel.Repaint;
      AStat := FPoFamilyStats.Items[Index];
      Bmp := CreateBitmap(AStat);
      AddToListView(AStat, Bmp);

      //if there are many icons to draw, occasionally update the ListView as visual feedback
      if (((Index + 1) mod 25) = 0) then
      begin
        ListView.EndUpdate;
        ListView.Repaint;
        ListView.BeginUpdate;
      end;

      Bmp.Free;
    end;
  finally
    ListView.EndUpdate;
    Screen.Cursor := Cur;
    StatusLabel.Visible := False;
  end;
end;

procedure TGraphStatForm.MaybeOpenInExternalEditor;
{$ifdef POCHECKERSTANDALONE}
var
  Proc: TProcessUtf8;
{$endif}
begin
  {$ifdef POCHECKERSTANDALONE}
  Proc := TProcessUtf8.Create(nil);
  try
    Proc.Options := [];
    Proc.Executable := Settings.ExternalEditorName;
    Proc.Parameters.Add(Fn);
    try
      Proc.Execute;
    except
      on E: EProcess do
      begin
        debugln('TGraphStatForm.ListViewMouseUp:');
        debugln('  Exception occurred of type ',E.ClassName);
        debugln('  Message: ',E.Message);
        ShowMessage(Format(SOpenFailExternal,[Fn,Settings.ExternalEditorName]));
      end;
    end;
  finally
    Proc.Free;
  end;
  {$endif}
end;

procedure TGraphStatForm.MaybeOpenInLazIDE;
{$ifndef POCHECKERSTANDALONE}
var
  mr: TModalResult;
  PageIndex,WindowIndex: Integer;
  OpenFlags: TOpenFlags;
{$endif}
begin
  {$ifndef POCHECKERSTANDALONE}
  PageIndex:= -1;
  WindowIndex:= -1;
  OpenFlags:= [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofConvertMacros];
  mr := LazarusIde.DoOpenEditorFile(Fn,PageIndex,WindowIndex,OpenFlags);
  if mr = mrOk then
    ModalResult:= mrOpenEditorFile //To let caller know what we want to do
  else
    ShowMessage(Format(SOpenFail,[Fn]));
  {$endif}
end;

end.

