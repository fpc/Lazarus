unit GraphStat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, FileUtil, Forms, Controls, Graphics, Dialogs,
  {$ifndef POCHECKERSTANDALONE} LazIDEIntf, {$endif}
  ExtCtrls, PoFamilies, PoCheckerConsts, LCLProc, StdCtrls, ComCtrls;


type

  { TGraphStatForm }

  TGraphStatForm = class(TForm)
    ListView: TListView;
    TranslatedLabel: TLabel;
    UnTranslatedLabel: TLabel;
    FuzzyLabel: TLabel;
    LegendPanel: TPanel;
    TranslatedShape: TShape;
    UnTranslatedShape: TShape;
    FuzzyShape: TShape;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure ListViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { private declarations }
    FPoFamilyStats: TPoFamilyStats;
    FImgList: TImageList;
    FOldHintHidePause: Integer;
    function CreateBitmap(AStat: TStat): TBitmap;
    procedure AddToListView(AStat: TStat; ABmp: TBitmap);
    procedure DrawGraphs;
  public
    { public declarations }
    property PoFamilyStats: TPoFamilyStats read FPoFamilyStats write FPoFamilyStats;
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
{$ifndef POCHECKERSTANDALONE}
var
  anItem: TListItem;
  anIndex: Integer;
  AStat: TStat;
  PageIndex,WindowIndex: Integer;
  OpenFlags: TOpenFlags;
  mr: TModalResult;
{$endif}
begin
  {$ifndef POCHECKERSTANDALONE}
  anItem := Listview.GetItemAt(X, Y);
  if Assigned(anItem) then begin
    anIndex := anItem.Index;
    AStat := FPoFamilyStats.Items[anIndex];
    PageIndex:= -1;
    WindowIndex:= -1;
    OpenFlags:= [ofOnlyIfExists,ofAddToRecent,ofRegularFile,ofConvertMacros];
    mr := LazarusIde.DoOpenEditorFile(AStat.PoName,PageIndex,WindowIndex,OpenFlags);
    if mr = mrOk then begin
      if MessageDlg('PoChecker',Format(sOpenFile,[AStat.PoName]),
         mtConfirmation,mbOKCancel,0) = mrOk then begin
           ModalResult:= mrOpenEditorFile; //To let caller know what we want to do
         end;
      end
    else ShowMessage(Format(SOpenFail,[AStat.PoName]));
  end;
  {$endif}
end;

procedure TGraphStatForm.FormCreate(Sender: TObject);
begin
  Caption := sGrapStatFormCaption;
  TranslatedLabel.Caption := sTranslated;
  UntranslatedLabel.Caption := sUnTranslated;
  FuzzyLabel.Caption := sFuzzy;
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
  DrawGraphs;
end;

procedure TGraphStatForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FImgList) then FImgList.Free;
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

procedure TGraphStatForm.DrawGraphs;
var
  Bmp: TBitmap;
  AStat: TStat;
  Index: Integer;
begin
  if Assigned(FImgList) then FImgList.Free;
  FImgList := TImageList.CreateSize(BmpWH, BmpWH);
  ListView.Clear;
  ListView.LargeImages := FImgList;
  ListView.BeginUpdate;
  try
    for Index := 0 to FPoFamilyStats.Count - 1 do
    begin
      AStat := FPoFamilyStats.Items[Index];
      Bmp := CreateBitmap(AStat);
      AddToListView(AStat, Bmp);
      Bmp.Free;
    end;
  finally
    ListView.EndUpdate;
  end;
end;

end.

