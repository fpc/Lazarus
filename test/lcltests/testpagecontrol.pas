unit testpagecontrol;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit,
  Interfaces, LCLType, LCLIntf, Forms, ComCtrls, Controls, StdCtrls, LMessages,
  testglobals;

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
    procedure DoPageCtrlChanging(Sender: TObject; var AllowChange: Boolean);
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
  published
    procedure TestCreation;
    procedure TestMovePages;
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
    if (TestButton <> nil) and (Result <> TestButton.DidPaint) then exit(-1);
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

procedure TTestPageControl.TestCreation;
var
  s1, s2, s3, s4, s5, s6, s7: TTestSheet;
  Name, Name2: String;
begin
  {%region  Pages, with children}
    RecreateForm(True);
    Name := 'Pages, with children: ';

    s1 := CreatePage('p1', 0, [fResetCounts]);
    Name2 := 'Inserted 1st page at 0 (no index change forced): ';
    AssertEquals(Name+Name2+'PageIndex is 1',             0, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 0',       0, s1.TabIndex);
    AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    AssertEquals(Name+Name2+'Did Paint Page 1',           1, s1.DidPaint);

    s2 := CreatePage('p2', 1, [fResetCounts]);
    Name2 := 'Inserted 2nd page at 1 (no index change forced): ';
    AssertEquals(Name+Name2+'PageIndex remains 0',        0, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 0',       0, s1.TabIndex);
    AssertEquals(Name+Name2+'Page 2 TabIndex is 1',       1, s2.TabIndex);
    AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    AssertEquals(Name+Name2+'Did not RE-Paint Page 1',    0, s1.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 2',       0, s2.DidPaint);


    s3 := CreatePage('p3', 2, [fResetCounts]);
    Name2 := 'Inserted 3rd page at 2 (no index change forced): ';
    AssertEquals(Name+Name2+'PageIndex remains 0',        0, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 0',       0, s1.TabIndex);
    AssertEquals(Name+Name2+'Page 2 TabIndex is 1',       1, s2.TabIndex);
    AssertEquals(Name+Name2+'Page 3 TabIndex is 2',       2, s3.TabIndex);
    AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    AssertEquals(Name+Name2+'Did not RE-Paint Page 1',    0, s1.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 2',       0, s2.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 3',       0, s3.DidPaint);

    // insert in front
    s4 := CreatePage('p4', 0, [fResetCounts]);
    Name2 := 'Inserted 4th page at 0 (no index change forced): ';
    AssertEquals(Name+Name2+'PageIndex CHANGED to 1',     1, PageControl.ActivePageIndex);
    AssertEquals(Name+Name2+'Page 1 TabIndex is 1',       1, s1.TabIndex);
    AssertEquals(Name+Name2+'Page 2 TabIndex is 2',       2, s2.TabIndex);
    AssertEquals(Name+Name2+'Page 3 TabIndex is 3',       3, s3.TabIndex);
    AssertEquals(Name+Name2+'Page 4 TabIndex is 0',       0, s4.TabIndex);
    // TODO: an event is triggered (maybe by gages move)
// MOVEDF pages...
//AssertEquals(Name+Name2+'No OnChanging',              1, FOnChangingCalled);
//AssertEquals(Name+Name2+'No OnChanged',               2, FOnChangeCalled); /// XXXXX 2 times
    //AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    AssertEquals(Name+Name2+'Did not RE-Paint Page 1',    0, s1.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 2',       0, s2.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 3',       0, s3.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 4',       0, s4.DidPaint);

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
// MOVEDF pages...
//AssertEquals(Name+Name2+'No OnChanged',               1, FOnChangeCalled);
    AssertEquals(Name+Name2+'No OnChanging',              0, FOnChangingCalled);
    //AssertEquals(Name+Name2+'No OnChange',                0, FOnChangeCalled);
    AssertEquals(Name+Name2+'Did not RE-Paint Page 1',    0, s1.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 2',       0, s2.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 3',       0, s3.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 4',       0, s4.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 5',       0, s5.DidPaint);

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
    AssertEquals(Name+Name2+'Did not RE-Paint Page 1',    0, s1.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 2',       0, s2.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 3',       0, s3.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 4',       0, s4.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 5',       0, s5.DidPaint);
    AssertEquals(Name+Name2+'Did NOT Paint Page 6',       0, s6.DidPaint);

// TODO: change PageIndex to display new page.

  {%endregion  Pages, with children}


  {%region  Pages, WITHOUT children}
    //RecreateForm(True);
    //Name := 'Pages, WITHOUT children';
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
    ResetCounts;

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
 // TODO: check paint and events
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




end;


initialization
  AddToLCLTestSuite(TTestPageControl);

end.

