unit TestPageControl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, types, math, fpcunit, Interfaces, LCLType, LCLIntf, Forms, ComCtrls,
  Controls, StdCtrls, LMessages, LCLProc, WSComCtrls, testglobals,
  MouseAndKeyInput;

type

  { TTestLabel }

  TTestLabel=class(TLabel)
  public
    PaintCalled: Integer;
    procedure ResetCounts;
    function  DidPaint: Integer; // 0 no, 1 yes, -1 unknown
    procedure Paint; override;
  end;

  { TTestButton }

  TTestButton=class(TButton)
  public
    WMPaintCalled: Integer;
    procedure ResetCounts;
    function  DidPaint: Integer; // 0 no, 1 yes, -1 unknown

    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
  end;

  { TTestSheet }

  TTestSheet = class(TTabSheet)
  public
    WMPaintCalled: Integer;
    TestButton: TTestButton;
    TestLabel: TTestLabel;
    procedure ResetCounts;
    function  DidPaint: Integer; // 0 no, 1 yes, -1 unknown

    procedure WMPaint(var Msg: TLMPaint); message LM_PAINT;
  end;

  { TTestPage }

  TTestPage=class(TPageControl)
  protected
    function GetPageClass: TCustomPageClass; override;
  public
    function TestGetTabBarHeight: Integer;
    function TestGetTabBarWidth: Integer;
  end;

  TCreatePageFlag = (
    fResetCounts,
    fSkipIndex,      // do NOT set: TabSteet.PageIndex
    fChangeIndex,    // Set:        PageControl.ActivePageIndex := NewIndex
    FSkipChildren,   // No Buuton or label
    FSkifProcessMsgs
  );
  TCreatePageFlags = set of TCreatePageFlag;

  { TTestPageControl }

  TTestPageControl = class(TTestCase)
  private
    FForm : TForm;
    FPageControl: TTestPage;

    FOnChangeCalled, FOnChangingCalled: Integer;
    FOnChangesList: TStringList;

    procedure DoPageCtrlChanged(Sender: TObject);
    procedure DoPageCtrlChanging(Sender: TObject; var {%H-}AllowChange: Boolean);
  protected
    property  Form: TForm read FForm;
    property  PageControl: TTestPage read FPageControl;
  public
    procedure SetUp; override;
    procedure TearDown; override;
    procedure RecreateForm(WithPageCtrl: Boolean = True);
    procedure CreatePageControl;
    function  CreatePage(ACaption: String; AIndex: Integer; AFlags: TCreatePageFlags = []): TTestSheet;
    procedure ResetCounts;
    procedure ResetPaintCounts;

    procedure CheckPaint(AName: String; APaintedPage: TTestSheet);
  published
    procedure TestPageCreation;
    procedure TestMovePages;
    procedure TestTabAndClientRect;
    procedure TestSwitchTabByClick;
    procedure TestPageDestruction;
    //procedure TestCreationAndHiddenTabs;
    //procedure TestMovePagesAndHiddenTabs;
  end;

implementation

{ TTestButton }

procedure TTestButton.ResetCounts;
begin
  WMPaintCalled := 0;
end;

function TTestButton.DidPaint: Integer;
begin
  Result := WMPaintCalled;
  ResetCounts;
end;

procedure TTestButton.WMPaint(var Msg: TLMPaint);
begin
  inc(WMPaintCalled);
  inherited WMPaint(Msg);
end;

{ TTestLabel }

procedure TTestLabel.ResetCounts;
begin
  PaintCalled := 0;
end;

function TTestLabel.DidPaint: Integer;
begin
  Result := PaintCalled;
  ResetCounts;
end;

procedure TTestLabel.Paint;
begin
  inc(PaintCalled);
  inherited Paint;
end;

{ TTestSheet }

procedure TTestSheet.ResetCounts;
begin
  WMPaintCalled := 0;
  if TestButton <> nil then TestButton.ResetCounts;
  if TestLabel <> nil then TestLabel.ResetCounts;
end;

function TTestSheet.DidPaint: Integer;
begin
  try
    Result := WMPaintCalled;
    if (TestLabel <> nil) and (Result <> TestLabel.DidPaint) then exit(-1);
    {$IfNDef LCLQT}
    if (TestButton <> nil) and (Result <> TestButton.DidPaint) then exit(-1);
    {$EndIf}
  finally
    ResetCounts;
  end;
end;

procedure TTestSheet.WMPaint(var Msg: TLMPaint);
begin
  inc(WMPaintCalled);
  inherited WMPaint(Msg);
end;

{ TTestPage }

function TTestPage.GetPageClass: TCustomPageClass;
begin
  Result := TTestSheet;
end;

function TTestPage.TestGetTabBarHeight: Integer;
begin
  Result := TWSCustomTabControlClass(WidgetSetClass).GetNotebookMinTabHeight(Self);
end;

function TTestPage.TestGetTabBarWidth: Integer;
begin
  Result := TWSCustomTabControlClass(WidgetSetClass).GetNotebookMinTabWidth(Self);
end;

{ TTestPageControl }

procedure TTestPageControl.DoPageCtrlChanged(Sender: TObject);
begin
  inc(FOnChangeCalled);
  FOnChangesList.Add('Changed '+IntToStr(PageControl.ActivePageIndex));

  if PageControl.ActivePageIndex < 0
  then AssertTrue('OnChange, correct index=page', nil = PageControl.ActivePage)
  else AssertTrue('OnChange, correct index=page', PageControl.ActivePageIndex = PageControl.ActivePage.TabIndex);
end;

procedure TTestPageControl.DoPageCtrlChanging(Sender: TObject; var AllowChange: Boolean);
begin
  inc(FOnChangingCalled);
  FOnChangesList.Add('Changing '+IntToStr(PageControl.ActivePageIndex));

  if PageControl.ActivePageIndex < 0
  then AssertTrue('OnChanging, correct index=page', nil = PageControl.ActivePage)
  else AssertTrue('OnChanging, correct index=page', PageControl.ActivePageIndex = PageControl.ActivePage.TabIndex);
end;

procedure TTestPageControl.SetUp;
begin
  inherited SetUp;
  FOnChangesList := TStringList.Create;
  RecreateForm;
end;

procedure TTestPageControl.TearDown;
begin
  inherited TearDown;
  FreeAndNil(FForm);
  FreeAndNil(FOnChangesList);
end;

procedure TTestPageControl.RecreateForm(WithPageCtrl: Boolean);
begin
  FreeAndNil(FForm);

  FForm := TForm.CreateNew(nil);
  FForm.Top := Screen.DesktopTop + 1;
  FForm.Left := Screen.DesktopLeft + 1;
  FForm.Height := 200;
  FForm.Width := 500;
  FForm.Show;

  if WithPageCtrl then
    CreatePageControl;
  Application.ProcessMessages;
end;

procedure TTestPageControl.CreatePageControl;
begin
  FPageControl:= TTestPage.Create(Form);
  FPageControl.Parent := Form;
  FPageControl.Align := alClient;
  FPageControl.OnChange := @DoPageCtrlChanged;
  FPageControl.OnChanging := @DoPageCtrlChanging;
end;

function TTestPageControl.CreatePage(ACaption: String; AIndex: Integer;
  AFlags: TCreatePageFlags): TTestSheet;
begin
  if fResetCounts in AFlags then
    ResetCounts;

  Result := TTestSheet.Create(Form);
  Result.PageControl := PageControl;
  Result.Caption :=  ACaption;
  if not(fSkipIndex in AFlags) then
    Result.PageIndex := AIndex;

  if not(FSkipChildren in AFlags) then begin
    Result.TestButton := TTestButton.Create(Form);
    Result.TestButton.Parent := Result;
    Result.TestButton.Align := alTop;
    Result.TestButton.Caption := ACaption;

    Result.TestLabel := TTestLabel.Create(Form);
    Result.TestLabel.Parent := Result;
    Result.TestLabel.Align := alTop;
    Result.TestLabel.Caption := ACaption;
  end;

  if fChangeIndex in AFlags then
    PageControl.ActivePage := Result;

  if not(FSkifProcessMsgs in AFlags) then
    Application.ProcessMessages;
end;

procedure TTestPageControl.ResetCounts;
begin
  FOnChangeCalled := 0;
  FOnChangingCalled := 0;
  FOnChangesList.Clear;
end;

procedure TTestPageControl.ResetPaintCounts;
var
  i: Integer;
begin
  for i := 0 to PageControl.ControlCount - 1 do
    if (PageControl.Controls[i] is TTestSheet) then
      (PageControl.Controls[i] as TTestSheet).DidPaint;
end;

procedure TTestPageControl.CheckPaint(AName: String; APaintedPage: TTestSheet);
{$If (not defined(LCLGTK2)) and (not defined(LCLQT))}
var
  i: Integer;
{$ENDIF}
begin
  {$IfDef LCLGTK2}
  if APaintedPage <> nil then begin
    AssertTrue(AName + ' Paint was called', APaintedPage.DidPaint >= 1);
  end;
  {$Else}
  if APaintedPage <> nil then begin
    AssertEquals(AName + ' Paint was called', 1, APaintedPage.DidPaint);
  end;
  {$IfNDef LCLQT}
  for i := 0 to PageControl.ControlCount - 1 do
    if (PageControl.Controls[i] <> APaintedPage) and (PageControl.Controls[i] is TTestSheet) then
      AssertEquals(AName + 'NO paint for other page', 0, (PageControl.Controls[i] as TTestSheet).DidPaint);
  {$EndIf}
  {$EndIf}
end;

procedure TTestPageControl.TestPageCreation;
var
  s1, s2, s3, s4, s5, s6: TTestSheet;
  Name, Name2: String;
begin
  {%region  NON Setting PagControl.PageIndex}
    RecreateForm(True);
    Name := 'Pages, with children: ';

    s1 := CreatePage('p1', 0, [fResetCounts]);
    Name2 := 'Inserted 1st page at 0 (no index change forced): ';
    AssertEquals(Name+Name2+'PageIndex is 1',             0, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 0',       0, s1.TabIndex);
    AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    CheckPaint(Name+Name2+'paint', s1);

    s2 := CreatePage('p2', 1, [fResetCounts]);
    Name2 := 'Inserted 2nd page at 1 (no index change forced): ';
    AssertEquals(Name+Name2+'PageIndex remains 0',        0, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 0',       0, s1.TabIndex);
    AssertEquals(Name+Name2+'Page 2 TabIndex is 1',       1, s2.TabIndex);
    AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    CheckPaint(Name+Name2+'no (re-)paint', nil);


    s3 := CreatePage('p3', 2, [fResetCounts]);
    Name2 := 'Inserted 3rd page at 2 (no index change forced): ';
    AssertEquals(Name+Name2+'PageIndex remains 0',        0, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 0',       0, s1.TabIndex);
    AssertEquals(Name+Name2+'Page 2 TabIndex is 1',       1, s2.TabIndex);
    AssertEquals(Name+Name2+'Page 3 TabIndex is 2',       2, s3.TabIndex);
    AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    CheckPaint(Name+Name2+'no (re-)paint', nil);

    // insert in front
    s4 := CreatePage('p4', 0, [fResetCounts]);
    Name2 := 'Inserted 4th page at 0 (no index change forced): ';
    AssertEquals(Name+Name2+'PageIndex CHANGED to 1',     1, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 1',       1, s1.TabIndex);
    AssertEquals(Name+Name2+'Page 2 TabIndex is 2',       2, s2.TabIndex);
    AssertEquals(Name+Name2+'Page 3 TabIndex is 3',       3, s3.TabIndex);
    AssertEquals(Name+Name2+'Page 4 TabIndex is 0',       0, s4.TabIndex);
    // TODO: an event is triggered (maybe by gages move)
//AssertEquals(Name+Name2+'No OnChanged',               2, FOnChangeCalled); /// XXXXX 2 times
    AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    CheckPaint(Name+Name2+'no (re-)paint', nil);

    // insert in middle
    s5 := CreatePage('p5', 2, [fResetCounts]);
    Name2 := 'Inserted 5th page at 2 (no index change forced): ';
    AssertEquals(Name+Name2+'PageIndex remains 1',        1, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 1',       1, s1.TabIndex);
    AssertEquals(Name+Name2+'Page 2 TabIndex is 2',       3, s2.TabIndex);
    AssertEquals(Name+Name2+'Page 3 TabIndex is 3',       4, s3.TabIndex);
    AssertEquals(Name+Name2+'Page 4 TabIndex is 0',       0, s4.TabIndex);
    AssertEquals(Name+Name2+'Page 5 TabIndex is 0',       2, s5.TabIndex);
    // TODO: an event is triggered (maybe by gages move)
//AssertEquals(Name+Name2+'No OnChanged',               1, FOnChangeCalled);
    AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    CheckPaint(Name+Name2+'no (re-)paint', nil);

    // insert WITHOUT setting TabIndex
    s6 := CreatePage('p6', -1, [fSkipIndex, fResetCounts]);
    Name2 := 'Inserted 5th page at 2 (no index change forced): ';
    AssertEquals(Name+Name2+'PageIndex remains 1',        1, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 1',       1, s1.TabIndex);
    AssertEquals(Name+Name2+'Page 2 TabIndex is 2',       3, s2.TabIndex);
    AssertEquals(Name+Name2+'Page 3 TabIndex is 3',       4, s3.TabIndex);
    AssertEquals(Name+Name2+'Page 4 TabIndex is 0',       0, s4.TabIndex);
    AssertEquals(Name+Name2+'Page 5 TabIndex is 0',       2, s5.TabIndex);
    AssertEquals(Name+Name2+'Page 6 TabIndex is 5',       5, s6.TabIndex); // at end
    // TODO: an event is triggered (maybe by gages move)
    AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    CheckPaint(Name+Name2+'no (re-)paint', nil);

// TODO: change PageIndex to display new page.

  {%endregion }


  {%region  Setting PagControl.PageIndex}
    RecreateForm(True);
    Name := 'Pages, with children: ';

    s1 := CreatePage('p1', 0, [fChangeIndex, fResetCounts]);
    Name2 := 'Inserted 1st page at 0 (index change forced): ';
    AssertEquals(Name+Name2+'PageIndex is 1',             0, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 0',       0, s1.TabIndex);
    AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    CheckPaint(Name+Name2+'paint', s1);

    s2 := CreatePage('p2', 0, [fChangeIndex, fResetCounts]);
    Name2 := 'Inserted 2nd page at 0 (index change forced): ';
    AssertEquals(Name+Name2+'PageIndex set 0',            0, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 1',       1, s1.TabIndex);
    AssertEquals(Name+Name2+'Page 2 TabIndex is 0',       0, s2.TabIndex);
    //AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    CheckPaint(Name+Name2+'paint', s2);

    s3 := CreatePage('p3', 1, [fChangeIndex, fResetCounts]);
    Name2 := 'Inserted 3rd page at 1 (index change forced): ';
    AssertEquals(Name+Name2+'PageIndex set 1',            1, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 1',       2, s1.TabIndex);
    AssertEquals(Name+Name2+'Page 2 TabIndex is 0',       0, s2.TabIndex);
    AssertEquals(Name+Name2+'Page 3 TabIndex is 0',       1, s3.TabIndex);
    //AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    CheckPaint(Name+Name2+'paint', s3);

  {%endregion  Pages, WITHOUT children}


end;

procedure TTestPageControl.TestMovePages;
var
  s0, s1, s2, s3, s4, s5, s6: TTestSheet;
  Name, Name2: String;

  procedure InternalSetup(AIndex: Integer);
  begin
    RecreateForm(True);
    s0 := CreatePage('p0', 0, []);
    s1 := CreatePage('p1', 1, []);
    s2 := CreatePage('p2', 2, []);
    s3 := CreatePage('p3', 3, []);
    s4 := CreatePage('p4', 4, []);
    s5 := CreatePage('p5', 5, []);
    s6 := CreatePage('p6', 6, []);
    PageControl.ActivePageIndex := AIndex;
    Application.ProcessMessages;
    ResetCounts;
    ResetPaintCounts;

    Name := 'SelfTest';
    Name2 := '';
    AssertEquals(Name+Name2+'PageIndex',                  AIndex, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 0 TabIndex is 0',       0, s0.TabIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 1',       1, s1.TabIndex);
    AssertEquals(Name+Name2+'Page 2 TabIndex is 2',       2, s2.TabIndex);
    AssertEquals(Name+Name2+'Page 3 TabIndex is 3',       3, s3.TabIndex);
    AssertEquals(Name+Name2+'Page 4 TabIndex is 4',       4, s4.TabIndex);
    AssertEquals(Name+Name2+'Page 5 TabIndex is 5',       5, s5.TabIndex);
    AssertEquals(Name+Name2+'Page 6 TabIndex is 6',       6, s6.TabIndex);
  end;

begin
 // TODO: check events
 // TODO: temp select tabs before move / create handles
 // TODO: Move to last / move to first (range check tests

  Name := 'Move, where all affected tabs are right of the active: ';
  InternalSetup(0);
  Name2 := 'Move Tab idx=1 to 3 (Selected 0): ';
  s1.PageIndex := 3;
  AssertEquals(Name+Name2+'PageIndex',                  0, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       3, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       1, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);
  AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
  //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);

  InternalSetup(0);
  Name2 := 'Move Tab idx=2 to 3 (SWAP) (Selected 0): ';
  s2.PageIndex := 3;
  AssertEquals(Name+Name2+'PageIndex',                  0, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       1, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  InternalSetup(0);
  Name2 := 'Move Tab idx=3 to 1 (Selected 0): ';
  s3.PageIndex := 1;
  AssertEquals(Name+Name2+'PageIndex',                  0, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       2, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       1, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  InternalSetup(0);
  Name2 := 'Move Tab idx=3 to 2 (SWAP backward) (Selected 0): ';
  s3.PageIndex := 2;
  AssertEquals(Name+Name2+'PageIndex',                  0, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       1, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);



  Name := 'Move, where all affected tabs are LEFT of the active: ';
  InternalSetup(5);
  Name2 := 'Move Tab idx=1 to 3 (Selected 5): ';
  s1.PageIndex := 3;
  AssertEquals(Name+Name2+'PageIndex',                  5, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       3, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       1, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  InternalSetup(5);
  Name2 := 'Move Tab idx=2 to 3 (SWAP) (Selected 5): ';
  s2.PageIndex := 3;
  AssertEquals(Name+Name2+'PageIndex',                  5, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       1, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  InternalSetup(5);
  Name2 := 'Move Tab idx=3 to 1 (Selected 5): ';
  s3.PageIndex := 1;
  AssertEquals(Name+Name2+'PageIndex',                  5, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       2, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       1, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  InternalSetup(5);
  Name2 := 'Move Tab idx=3 to 2 (SWAP backward) (Selected 5): ';
  s3.PageIndex := 2;
  AssertEquals(Name+Name2+'PageIndex',                  5, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       1, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);



  Name := 'Move, where affected tabs are one to the LEFT, one to the RIGHT of the active: ';
  InternalSetup(2);
  Name2 := 'Move Tab idx=1 to 3 (Selected 2): ';
  s1.PageIndex := 3;
  AssertEquals(Name+Name2+'PageIndex',                  1, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       3, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       1, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  // Not possible to SWAP as single move, with active needed in the middle
  //Name2 := 'Move Tab idx=2 to 3 (SWAP) (Selected 0): ';

  InternalSetup(2);
  Name2 := 'Move Tab idx=3 to 1 (Selected 2): ';
  s3.PageIndex := 1;
  AssertEquals(Name+Name2+'PageIndex',                  3, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       2, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       1, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);



  Name := 'Move the active: ';
  InternalSetup(1);
  Name2 := 'Move Tab idx=1 to 3 (Selected 1): ';
  s1.PageIndex := 3;
  AssertEquals(Name+Name2+'PageIndex',                  3, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       3, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       1, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  InternalSetup(2);
  Name2 := 'Move Tab idx=2 to 3 (SWAP) (Selected 2): ';
  s2.PageIndex := 3;
  AssertEquals(Name+Name2+'PageIndex',                  3, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       1, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  InternalSetup(3);
  Name2 := 'Move Tab idx=3 to 1 (Selected 3): ';
  s3.PageIndex := 1;
  AssertEquals(Name+Name2+'PageIndex',                  1, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       2, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       1, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  InternalSetup(3);
  Name2 := 'Move Tab idx=3 to 2 (SWAP backward) (Selected 3): ';
  s3.PageIndex := 2;
  AssertEquals(Name+Name2+'PageIndex',                  2, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       1, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);




  Name := 'Move to position of the active: ';
  InternalSetup(3);
  Name2 := 'Move Tab idx=1 to 3 (Selected 3): ';
  s1.PageIndex := 3;
  AssertEquals(Name+Name2+'PageIndex',                  2, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       3, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       1, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  InternalSetup(3);
  Name2 := 'Move Tab idx=2 to 3 (SWAP) (Selected 3): ';
  s2.PageIndex := 3;
  AssertEquals(Name+Name2+'PageIndex',                  2, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       1, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  InternalSetup(1);
  Name2 := 'Move Tab idx=3 to 1 (Selected 1): ';
  s3.PageIndex := 1;
  AssertEquals(Name+Name2+'PageIndex',                  2, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       2, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       1, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);

  InternalSetup(2);
  Name2 := 'Move Tab idx=3 to 2 (SWAP backward) (Selected 2): ';
  s3.PageIndex := 2;
  AssertEquals(Name+Name2+'PageIndex',                  3, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'Page 0 PageIndex is 0',       0, s0.PageIndex);
  AssertEquals(Name+Name2+'Page 1 PageIndex is 1',       1, s1.PageIndex);
  AssertEquals(Name+Name2+'Page 2 PageIndex is 2',       3, s2.PageIndex);
  AssertEquals(Name+Name2+'Page 3 PageIndex is 3',       2, s3.PageIndex);
  AssertEquals(Name+Name2+'Page 4 PageIndex is 4',       4, s4.PageIndex);
  AssertEquals(Name+Name2+'Page 5 PageIndex is 5',       5, s5.PageIndex);
  AssertEquals(Name+Name2+'Page 6 PageIndex is 6',       6, s6.PageIndex);
  CheckPaint(Name+Name2+' no painting', nil);


end;

procedure TTestPageControl.TestTabAndClientRect;
var
  s0, s3: TTestSheet;
  PgRect, PgRectScr: TRect;
  TsRect, TsRectScr: TRect;
  T0Rect, T0RectScr: TRect;
  T1Rect, T1RectScr: TRect;
  T2Rect, T2RectScr: TRect;
  T2Recta, T2RectaScr: TRect;
  T3Rect, T3RectScr: TRect;
begin
  RecreateForm(True);
  s0 := CreatePage('abc', 0, []);
  CreatePage('a', 1, []);
  CreatePage('foo bar 123 ...', 2, []);
  PageControl.ActivePageIndex := 0;
  ResetCounts;
  ResetPaintCounts;
  Application.ProcessMessages;

  PgRect := PageControl.ClientRect;
  PgRectScr.TopLeft := PageControl.ClientToScreen(PgRect.TopLeft);
  PgRectScr.BottomRight := PageControl.ClientToScreen(PgRect.BottomRight);

  TsRect := s0.ClientRect;
  TsRectScr.TopLeft := s0.ClientToScreen(TsRect.TopLeft);
  TsRectScr.BottomRight := s0.ClientToScreen(TsRect.BottomRight);

  T0Rect := PageControl.TabRect(0);
  T1Rect := PageControl.TabRect(1);
  T2Rect := PageControl.TabRect(2);
  T0RectScr.TopLeft := PageControl.ClientToScreen(T0Rect.TopLeft);
  T1RectScr.TopLeft := PageControl.ClientToScreen(T1Rect.TopLeft);
  T2RectScr.TopLeft := PageControl.ClientToScreen(T2Rect.TopLeft);
  T0RectScr.BottomRight := PageControl.ClientToScreen(T0Rect.BottomRight);
  T1RectScr.BottomRight := PageControl.ClientToScreen(T1Rect.BottomRight);
  T2RectScr.BottomRight := PageControl.ClientToScreen(T2Rect.BottomRight);


//DebugLn([PageControl.TestGetTabBarHeight, ' /// ',
//  dbgs(PgRect),' / ',  dbgs(PgRectScr),' / ',
//  dbgs(TsRect),' / ',  dbgs(TsRectScr),' / ',
//  dbgs(T0Rect),' / ',  dbgs(T0RectScr),' / ',
//  dbgs(T1Rect),' / ',  dbgs(T1RectScr),' / ',
//  dbgs(T2Rect),' / ',  dbgs(T2RectScr),' / '    ]);
  // just approx sanity checks
  // comparing absolute values

  AssertTrue('Page in Container : Top',    TsRectScr.Top    >= PgRectScr.Top);
  AssertTrue('Page in Container : Left',   TsRectScr.Left   >= PgRectScr.Left);
  AssertTrue('Page in Container : Bottom', TsRectScr.Bottom <= PgRectScr.Bottom);
  AssertTrue('Page in Container : Right',  TsRectScr.Right  <= PgRectScr.Right);

  // Allow a tiny overlap, in case
  AssertTrue('Tab1 above Container:', T0RectScr.Bottom -5 <= TsRectScr.Top);
  AssertTrue('Tab2 above Container:', T1RectScr.Bottom -5 <= TsRectScr.Top);
  AssertTrue('Tab3 above Container:', T2RectScr.Bottom -5 <= TsRectScr.Top);

  AssertTrue('Tab1 at left side:', Abs(T0RectScr.Left - PgRectScr.Left) <= 15);

  AssertTrue('Tab1 left of tab2 a:', T0RectScr.Left < T1RectScr.Left);
  AssertTrue('Tab1 left of tab2 b:', Abs(T0RectScr.Right - T1RectScr.Left) <=
             min(20, (T0Rect.Right-T0Rect.Left) div 2 )
            );
  AssertTrue('Tab2 left of tab3 a:', T1RectScr.Left < T2RectScr.Left);
  AssertTrue('Tab2 left of tab3 b:', Abs(T1RectScr.Right - T2RectScr.Left) <=
             Min(20, (T1Rect.Right-T1Rect.Left) div 2 )
            );

  AssertTrue('TestGetTabBarHeight', Abs(PageControl.TestGetTabBarHeight-(T0Rect.Bottom-T0Rect.Top)) <= 20 );
  // Todo Width

  (*
   * Hidden Tabs and Rect
  *)

  s3 := CreatePage('p3', 2, []); // insert 2nd last

  T2Recta := PageControl.TabRect(3);
  T3Rect := PageControl.TabRect(2);
  T2RectaScr.TopLeft := PageControl.ClientToScreen(T2Recta.TopLeft);
  T3RectScr.TopLeft  := PageControl.ClientToScreen(T3Rect.TopLeft);
  T2RectaScr.BottomRight := PageControl.ClientToScreen(T2Recta.BottomRight);
  T3RectScr.BottomRight  := PageControl.ClientToScreen(T3Rect.BottomRight);

  AssertTrue('Tab4 at ex-tab3 left:', Abs(T3RectScr.Left - T2RectScr.Left) <= 5);
  AssertTrue('Tab3 went right:', T2RectaScr.Left > T2RectScr.Left);

  s3.TabVisible := False;
  Application.ProcessMessages;

  // TabRect takes an indek into VISBLE-only tabs
  T2Recta := PageControl.TabRect(2); // the old tab 2, since tab3 is hidden
  T2RectaScr.TopLeft := PageControl.ClientToScreen(T2Recta.TopLeft);
  T2RectaScr.BottomRight := PageControl.ClientToScreen(T2Recta.BottomRight);

  AssertEquals('old and new tab2', T2Recta.Top, T2Rect.Top);
  AssertEquals('old and new tab2', T2Recta.Left, T2Rect.Left);
  AssertEquals('old and new tab2', T2Recta.Bottom, T2Rect.Bottom);
  AssertEquals('old and new tab2', T2Recta.Right, T2Rect.Right);
end;

procedure TTestPageControl.TestSwitchTabByClick;
var
  s2: TTestSheet;
  Name, Name2: String;
  T0Rect, T0RectScr: TRect;
  T1Rect, T1RectScr: TRect;
  T2Rect, T2RectScr: TRect;
  T3Rect, T3RectScr: TRect;
begin
  RecreateForm(True);
  CreatePage('abc', 0, []);
  CreatePage('a', 1, []);
  s2 := CreatePage('foo 1', 2, []);
  CreatePage('p3', 3, []);

  T0Rect := PageControl.TabRect(0);
  T1Rect := PageControl.TabRect(1);
  T2Rect := PageControl.TabRect(2);
  T3Rect := PageControl.TabRect(3);
  T0RectScr:=Rect(0,0,0,0);
  T1RectScr:=Rect(0,0,0,0);
  T2RectScr:=Rect(0,0,0,0);
  T3RectScr:=Rect(0,0,0,0);
  T0RectScr.TopLeft := PageControl.ClientToScreen(T0Rect.TopLeft);
  T1RectScr.TopLeft := PageControl.ClientToScreen(T1Rect.TopLeft);
  T2RectScr.TopLeft := PageControl.ClientToScreen(T2Rect.TopLeft);
  T3RectScr.TopLeft  := PageControl.ClientToScreen(T3Rect.TopLeft);
  T0RectScr.BottomRight := PageControl.ClientToScreen(T0Rect.BottomRight);
  T1RectScr.BottomRight := PageControl.ClientToScreen(T1Rect.BottomRight);
  T2RectScr.BottomRight := PageControl.ClientToScreen(T2Rect.BottomRight);
  T3RectScr.BottomRight  := PageControl.ClientToScreen(T3Rect.BottomRight);

  if T0RectScr.Left<0 then ;
  if T1RectScr.Left<0 then ;
  if T2Rect.Left<0 then ;
  if T2RectScr.Left<0 then ;
  if T3RectScr.Left<0 then ;

  //DebugLn([  dbgs(T2Rect),' / ',  dbgs(T2RectScr),' / '    ]);

  ResetCounts;
  ResetPaintCounts;

  Application.BringToFront;
  Form.BringToFront;
  Application.ProcessMessages;

  MouseInput.Click(mbLeft, [], (T2RectScr.Right+T2RectScr.Left) div 2, (T2RectScr.Bottom+T2RectScr.Top) div 2);
  Application.ProcessMessages;
  Name:='';
  Name2:='';
  AssertEquals(Name+Name2+'PageIndex 2',             2, PageControl.ActivePageIndex);
  AssertEquals(Name+Name2+'OnChanging',              1, FOnChangingCalled);
  AssertEquals(Name+Name2+'OnChange',                1, FOnChangeCalled);
  AssertEquals(Name+Name2+'OnChangList cnt', FOnChangesList.Count, 2);
  AssertEquals(Name+Name2+'OnChangList 0', FOnChangesList[0], 'Changing 0');
  AssertEquals(Name+Name2+'OnChangList 1', FOnChangesList[1], 'Changed 2');
  CheckPaint(Name+Name2+'paint', s2);

end;

procedure TTestPageControl.TestPageDestruction;
var
  s0: TTestSheet;
  Name, Name2: String;
begin
// http://bugs.freepascal.org/view.php?id=24972
  RecreateForm(True);
  s0 := CreatePage('abc', 0, []);
  CreatePage('a', 1, []);
  CreatePage('foo 1', 2, []);
  CreatePage('p3', 3, []);
  CreatePage('p4', 4, []);

  PageControl.ActivePageIndex := 3;
  ResetCounts;
  ResetPaintCounts;
  Application.ProcessMessages;

  Name := 'Remove tab 0';
  s0.Free;

  Application.ProcessMessages;

  Name2:='';
  AssertEquals(Name+Name2+'PageIndex 2',             2, PageControl.ActivePageIndex);
  //AssertEquals(Name+Name2+'OnChanging',              0, FOnChangingCalled);
  //AssertEquals(Name+Name2+'OnChange',                0, FOnChangeCalled);
  CheckPaint(Name+Name2+'no paint', nil);

end;


initialization
  AddToLCLTestSuite(TTestPageControl);

end.

