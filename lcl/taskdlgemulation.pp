unit TaskDlgEmulation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  LazUTF8,
  LCLType, LCLStrConsts, LCLIntf, InterfaceBase, ImgList, LCLProc,
  LResources, Menus, Graphics, Forms, Controls, StdCtrls, ExtCtrls, Buttons, Dialogs, DialogRes;


type

  TTaskDialogElement = (
    tdeContent, tdeExpandedInfo, tdeFooter, tdeMainInstruction,
    tdeEdit, tdeVerif);


  { TLCLTaskDialog }

  TLCLTaskDialog = class(TForm)
  private
    /// the Task Dialog structure which created the form
    FDlg: TTaskDialog;
    FVerifyChecked: Boolean;
    RadioButtonArray: array of TRadioButton;

    //CustomButtons, Radios: TStringList;
    DialogCaption, DlgTitle, DlgText,
    ExpandedButtonCaption, ExpandedText, FooterText,
    VerificationText: String;
    CommonButtons: TTaskDialogCommonButtons;

    Panel: TPanel;
    Image: TImage;
    /// the labels corresponding to the Task Dialog main elements
    Element: array[tdeContent..tdeMainInstruction] of TLabel;
    /// the Task Dialog selection list
    Combo: TComboBox;
    /// the Task Dialog optional query editor
    Edit: TEdit;
    /// the Task Dialog optional checkbox
    VerifyCheckBox: TCheckBox;

    procedure AddIcon(out IconBorder,X,Y: Integer; AParent: TWinControl);
    procedure AddPanel;
    procedure AddRadios(ARadioOffSet, AWidth, ARadioDef, AFontHeight: Integer; var X,Y: Integer; AParent: TWinControl);
    procedure AddCommandLinkButtons(var X, Y: Integer; AWidth, AButtonDef, AFontHeight: Integer; AParent: TWinControl);
    procedure AddButtonsAndCheckBox(var X,Y, XB: Integer; AWidth, AButtonDef: Integer; APArent: TWinControl);
    procedure AddFooter(var X, Y, XB: Integer; AFontHeight, AWidth: Integer; APArent: TWinControl);
    function AddLabel(const AText: string; BigFont: boolean; var X, Y: Integer; AFontHeight, AWidth: Integer; APArent: TWinControl): TLabel;
    procedure AddQueryCombo(var X,Y: Integer; AWidth: Integer; AParent: TWinControl);
    procedure AddQueryEdit(var X,Y: Integer; AWidth: Integer; AParent: TWinControl);

    procedure DoDialogConstructed;
    procedure DoDialogCreated;
    procedure DoDialogDestroyed;
    procedure VerifyClicked(Sender: TObject);

  protected
    procedure HandleEmulatedButtonClicked(Sender: TObject);
    procedure SetupControls;
  public
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;


    function Execute(AParentWnd: HWND; out ARadioRes: Integer): Integer;
  public
  end;


type
  TTaskDialogTranslate = function(const aString: string): string;
var
  TaskDialog_Translate: TTaskDialogTranslate;


function ExecuteLCLTaskDialog(const ADlg: TCustomTaskDialog; AParentWnd: HWND; out ARadioRes: Integer): Integer;


type
  TLCLTaskDialogIcon = (
    tiBlank, tiWarning, tiQuestion, tiError, tiInformation, tiNotUsed, tiShield);
  TLCLTaskDialogFooterIcon = (
    tfiBlank, tfiWarning, tfiQuestion, tfiError, tfiInformation, tfiShield);

function IconMessage(Icon: TLCLTaskDialogIcon): string;
function TF_DIALOGICON(const aIcon: TTaskDialogIcon): TLCLTaskDialogIcon;
function TF_FOOTERICON(const aIcon: TTaskDialogIcon): TLCLTaskDialogFooterIcon;



implementation

var
  LDefaultFont: TFont;

function DefaultFont: TFont;
begin
  if LDefaultFont<>nil then
    Exit(LDefaultFont);
  LDefaultFont := TFont.Create;
  LDefaultFont.Name := 'default';
  LDefaultFont.Style := [];
  LDefaultFont.Size := 10;
  Result := LDefaultFont;

  {$IFDEF WINDOWS}
  if Screen.Fonts.IndexOf('Calibri')>=0 then begin
    LDefaultFont.Size := 11;
    LDefaultFont.Name := 'Calibri';
  end else begin
    if Screen.Fonts.IndexOf('Tahoma')>=0 then
      LDefaultFont.Name := 'Tahoma'
    else
      LDefaultFont.Name := 'Arial';
  end;
  {$ENDIF}
end;


const
  LCL_IMAGES: array[TLCLTaskDialogIcon] of Integer = (
    0, idDialogWarning, idDialogConfirm, idDialogError, idDialogInfo, 0, idDialogShield);
  LCL_FOOTERIMAGES: array[TLCLTaskDialogFooterIcon] of Integer = (
    0, idDialogWarning, idDialogConfirm, idDialogError, idDialogInfo, idDialogShield);

const
  TD_BTNMOD: array[TTaskDialogCommonButton] of Integer = (
    mrOk, mrYes, mrNo, mrCancel, mrRetry, mrAbort);

function CR(const aText: string): string;
begin
  //disable this for now: what if a caption were to be 'Save to "c:\new_folder\new.work"'' ??
  //remove later
  Result := AText;
  {
  if pos('\n', aText) = 0 then
    Result := aText else
    Result := StringReplace(aText, '\n', #10, [rfReplaceAll]);
  }
end;

//if aText contains '\n'
//  return the part before '\n'
//  set aHint to part after '\n' and replace all remaining '\n' in aHint with a LineFeed character
function NoCR(const aText: string; out aHint: String): String;
var
  i: integer;
begin
  //disable this for now: what if a caption were to be 'Save to "c:\new_folder\new.work"'' ??
  //remove later
  Result := aText;
  aHint := '';
  {
  i := pos('\n',aText);
  if (i > 0) then
  begin
    aHint := CR(copy(Result,i+2,maxInt));
    SetLength(Result,i-1);
  end;
  }
end;


function TD_BTNS(button: TTaskDialogCommonButton): pointer;
begin
  case button of
    tcbOK:     Result := @rsMbOK;
    tcbYes:    Result := @rsMbYes;
    tcbNo:     Result := @rsMbNo;
    tcbCancel: Result := @rsMbCancel;
    tcbRetry:  Result := @rsMbRetry;
    tcbClose:  Result := @rsMbClose;
  end;
end;



function TF_DIALOGICON(const aIcon: TTaskDialogIcon): TLCLTaskDialogIcon;
begin
  case aIcon of
    tdiWarning: Result := tiWarning;
    tdiError: Result := tiError;
    tdiInformation: Result := tiInformation;
    tdiShield: Result := tiShield;
    tdiQuestion: Result := tiQuestion;
  else
    Result := tiBlank;
  end;
end;

function TF_FOOTERICON(const aIcon: TTaskDialogIcon): TLCLTaskDialogFooterIcon;
begin
  case aIcon of
    tdiWarning: Result := tfiWarning;
    tdiError: Result := tfiError;
    tdiInformation: Result := tfiInformation;
    tdiShield: Result := tfiShield;
    tdiQuestion: Result := tfiQuestion;
  else
    Result := tfiBlank;
  end;
end;


//Note: do we really need this??
//We already use resourcestrings that can be translated using
//translations unit
function TD_Trans(const aString: string): string;
begin
  if Assigned(TaskDialog_Translate) then
    Result := TaskDialog_Translate(aString)
  else
    Result := aString;
end;

function IconMessage(Icon: TLCLTaskDialogIcon): string;
begin
  case Icon of
    tiWarning:   Result := rsMtWarning;
    tiQuestion:  Result := rsMtConfirmation;
    tiError:     Result := rsMtError;
    tiInformation, tiShield: Result := rsMtInformation;
    else Result := '';
  end;
  Result := TD_Trans(Result);
end;


function ExecuteLCLTaskDialog(const ADlg: TCustomTaskDialog; AParentWnd: HWND; out ARadioRes: Integer): Integer;
var
  DlgForm: TLCLTaskDialog;
begin
  //debugln('ExecuteLCLTaskDialog');
  Result := -1;
  DlgForm := TLCLTaskDialog.CreateNew(ADlg);
  try
    Result := DlgForm.Execute(AParentWnd, ARadioRes);
  finally
    FreeAndNil(DlgForm);
  end;
end;

constructor TLCLTaskDialog.CreateNew(AOwner: TComponent; Num: Integer);
begin
  //debugln('TLCLTaskDialog.CreateNew: AOwner=',DbgSName(AOwner));
  inherited CreateNew(AOwner, Num);
  if (AOwner is TCustomTaskDialog) then
    FDlg := TTaskDialog(AOwner);
  RadioButtonArray := nil;
  KeyPreview := True;
  DoDialogCreated;
end;

destructor TLCLTaskDialog.Destroy;
begin
  DoDialogDestroyed;
  inherited Destroy;
end;

procedure TLCLTaskDialog.AfterConstruction;
begin
  inherited AfterConstruction;
  DoDialogConstructed;
end;

function TLCLTaskDialog.Execute(AParentWnd: HWND; out ARadioRes: Integer): Integer;
var
  mRes, I: Integer;
begin
  //debugln(['TLCLTaskDialog.Execute: Assigned(FDlg)=',Assigned(FDlg)]);
  if not Assigned(FDlg) then
    Exit(-1);
  SetupControls;

  //set form parent
  if (AParentWnd <> 0) then
    for I := 0 to Screen.CustomFormCount-1 do
      if Screen.CustomForms[I].Handle = AParentWnd then
      begin
        PopupParent := Screen.CustomForms[I];
        Break;
      end;
  if not Assigned(PopupParent) then
    PopupParent := Screen.ActiveCustomForm;
  if Assigned(PopupParent) then
    PopupMode := pmExplicit;

  Result := ShowModal;

  if Assigned(Combo) then
  begin
    FDlg.QueryItemIndex := Combo.ItemIndex;
    FDlg.QueryResult := Combo.Text;
  end
  else
  begin
    if Assigned(Edit) then
      FDlg.QueryResult := Edit.Text;
  end;

  if VerifyCheckBox<>nil then
  begin
    if VerifyCheckBox.Checked then
      FDlg.Flags := FDlg.Flags + [tfVerificationFlagChecked]
    else
      FDlg.Flags := FDlg.Flags - [tfVerificationFlagChecked]
  end;

  ARadioRes := 0;
  for i := 0 to high(RadioButtonArray) do
    if RadioButtonArray[i].Checked then
      ARadioRes := i+TaskDialogFirstRadioButtonIndex;
end;




procedure TLCLTaskDialog.AddIcon(out IconBorder,X,Y: Integer; AParent: TWinControl);
var
  aDialogIcon: TLCLTaskDialogIcon;
begin
  if (tfEmulateClassicStyle in FDlg.Flags) then
    IconBorder := 10
  else
    IconBorder := 24;

  aDialogIcon := TF_DIALOGICON(FDlg.MainIcon);
  if (LCL_IMAGES[aDialogIcon]<>0) then
  begin
    Image := TImage.Create(Self);
    Image.Parent := AParent;
    Image.Images := DialogGlyphs;
    Image.ImageIndex := DialogGlyphs.DialogIcon[LCL_IMAGES[aDialogIcon]];
    Image.SetBounds(IconBorder,IconBorder, 32, 32);
    Image.Stretch := True;
    Image.StretchOutEnabled := False;
    Image.Proportional := True;
    Image.Center := True;
    X := Image.Width+IconBorder*2;
    Y := Image.Top;
    if (tfEmulateClassicStyle in FDlg.Flags) then
      inc(Y, 8);
  end
  else
  begin
    Image := nil;
    if (not (tfEmulateClassicStyle in FDlg.Flags)) and (DlgTitle <> '') then
      IconBorder := IconBorder*2;
    X := IconBorder;
    Y := IconBorder;
  end;
end;

procedure TLCLTaskDialog.AddPanel;
begin
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alTop;
  Panel.BorderStyle := bsNone;
  Panel.BevelOuter := bvNone;
  if not (tfEmulateClassicStyle in FDlg.Flags) then
    Panel.Color := clWindow;
end;

procedure TLCLTaskDialog.AddRadios(ARadioOffSet, AWidth, ARadioDef, AFontHeight: Integer; var X,Y: Integer; AParent: TWinControl);
var
  i: Integer;
  aHint: String;
begin
  SetLength(RadioButtonArray,FDlg.RadioButtons.Count);
  for i := 0 to FDlg.RadioButtons.Count-1 do
  begin
    RadioButtonArray[i] := TRadioButton.Create(Self);
    with RadioButtonArray[i] do
    begin
      Parent := AParent;
      AutoSize := False;
      SetBounds(X+16,Y,aWidth-32-X, (6-AFontHeight) + ARadioOffset);
      Caption := NoCR(FDlg.RadioButtons[i].Caption, aHint); //LCL RadioButton doesn't support multiline captions
      if (aHint <> '') then begin
        ShowHint := True;
        Hint := aHint; // note shown as Hint
      end;
      inc(Y,Height + ARadioOffset);
      if not (tfNoDefaultRadioButton in FDlg.Flags) and ((i=0) or (i=aRadioDef)) then
        Checked := True;
    end;
  end;
  inc(Y,24);
end;

procedure TLCLTaskDialog.AddCommandLinkButtons(var X, Y: Integer; AWidth, AButtonDef, AFontHeight: Integer; AParent: TWinControl);
var
  i: Integer;
  CommandLink: TBitBtn;
  aHint: String;
begin
  inc(Y,8);
  for i := 0 to FDlg.Buttons.Count-1 do
  begin
    CommandLink := TBitBtn.Create(Self);
    with CommandLink do
    begin
      Parent := AParent;
      Font.Height := AFontHeight-3;
      if (tfEmulateClassicStyle in FDlg.Flags) then
        SetBounds(X,Y,aWidth-10-X,40) else
        SetBounds(X,Y,aWidth-16-X,40);
      Caption := FDlg.Buttons[i].Caption;
      Hint := FDlg.Buttons[i].CommandLinkHint;
      if (Hint <> '') then
        ShowHint := True;
      inc(Y,Height+2);
      ModalResult := i+TaskDialogFirstButtonIndex;
      OnClick := @HandleEmulatedButtonClicked;
      if ModalResult=aButtonDef then
        ActiveControl := CommandLink;
      if (tfEmulateClassicStyle in FDlg.Flags) then
      begin
        Font.Height := AFontHeight - 2;
        Font.Style := [fsBold]
      end;
      if (tfEmulateClassicStyle in FDlg.Flags) then
      begin
        Margin := 7;
        Spacing := 7;
      end
      else
      begin
        Margin := 24;
        Spacing := 10;
      end;
      if not (tfUseCommandLinksNoIcon in FDlg.Flags) then
      begin
        Images := LCLGlyphs;
        ImageIndex := LCLGlyphs.GetImageIndex('btn_arrowright');
        end;
      end;
    end;
  inc(Y,24);
end;

procedure TLCLTaskDialog.AddButtonsAndCheckBox(var X, Y, XB: Integer; AWidth, AButtonDef: Integer; APArent: TWinControl);
var
  CurrTabOrder, i: Integer;
  Btn: TTaskDialogCommonButton;

  function AddButton(const s: string; AModalResult, ACustomButtonIndex: integer): TButton;
  var
    WB: integer;
  begin
    WB := Canvas.TextWidth(s)+52;
    dec(XB,WB);
    if XB<X shr 1 then
    begin
      XB := aWidth-WB;
      inc(Y,32);
    end;
    Result := TButton.Create(Self);
    Result.Parent := AParent;
    if (tfEmulateClassicStyle in FDlg.Flags) then
      Result.SetBounds(XB,Y,WB-10,22)
    else
      Result.SetBounds(XB,Y,WB-12,28);
    Result.Caption := s;
    Result.ModalResult := AModalResult;
    Result.TabOrder := CurrTabOrder;
    Result.OnClick := @HandleEmulatedButtonClicked;

    if Assigned(FDlg.Buttons.DefaultButton) then
    begin
      if (ACustomButtonIndex >= 0) and
         (FDlg.ButtonIDToModalResult(TaskDialogFirstButtonIndex+ACustomButtonIndex) = AButtonDef) then
        Result.Default := True;
    end
    else
    begin
      case AModalResult of
        mrOk: begin
          Result.Default := True;
          if CommonButtons=[tcbOk] then
            Result.Cancel := True;
        end;
        mrCancel: Result.Cancel := True;
      end;//case
    end;//else

    if Assigned(FDlg.Buttons.DefaultButton) and Result.Default then
      ActiveControl := Result
    else
      if AModalResult=aButtonDef then
        ActiveControl := Result;
  end;
begin
  CurrTabOrder := Panel.TabOrder;
  inc(Y, 16);
  XB := aWidth;
  if not (tfUseCommandLinks in FDlg.Flags) then
    for i := FDlg.Buttons.Count-1 downto 0 do
      AddButton(FDlg.Buttons[i].Caption,i+TaskDialogFirstButtonIndex,i);
  for Btn := high(TTaskDialogCommonButton) downto low(TTaskDialogCommonButton) do
  begin
    if (Btn in CommonButtons) then
      AddButton(TD_Trans(LoadResString(TD_BTNS(Btn))), TD_BTNMOD[Btn],-1);
  end;
  if (VerificationText <> '') then
  begin
    VerifyCheckBox := TCheckBox.Create(Self);
    with VerifyCheckBox do
    begin
      Parent := AParent;
      if (X+16+Canvas.TextWidth(VerificationText) > XB) then begin
        inc(Y,32);
        XB := aWidth;
      end;
      SetBounds(X,Y,XB-X,24);
      Caption := VerificationText;
      Checked := FVerifyChecked;
      OnClick := @VerifyClicked;
    end;
  end;
  inc(Y,36);
end;

procedure TLCLTaskDialog.AddFooter(var X, Y, XB: Integer; AFontHeight, AWidth: Integer; APArent: TWinControl);
  procedure AddBevel;
  var
    BX: integer;
  begin
    with TBevel.Create(Self) do begin
      Parent := AParent;
      if (Image<>nil) and (Y<Image.Top+Image.Height) then
        BX := X else
        BX := 2;
      SetBounds(BX,Y,aWidth-BX-2,2);
    end;
    inc(Y,16);
  end;

begin
  if XB<>0 then
    AddBevel
  else
    inc(Y,16);
  if (LCL_FOOTERIMAGES[TF_FOOTERICON(FDlg.FooterIcon)]<>0) then
  begin
    Image := TImage.Create(Self);
    Image.Parent := AParent;
    Image.Images := DialogGlyphs;
    Image.ImageWidth := 16;
    Image.ImageIndex := DialogGlyphs.DialogIcon[LCL_FOOTERIMAGES[TF_FOOTERICON(FDlg.FooterIcon)]];
    Image.Stretch := True;
    Image.StretchOutEnabled := False;
    Image.Proportional := True;
    Image.Center := True;
    Image.SetBounds(24,Y,16,16);
    X := 40+Image.Width;
  end
  else
  begin
    X := 24;
  end;
  Element[tdeFooter] := AddLabel(FooterText, False, X, Y, AFontHeight, AWidth, AParent);
end;

function TLCLTaskDialog.AddLabel(const AText: string; BigFont: boolean; var X, Y: Integer; AFontHeight, AWidth: Integer; APArent: TWinControl): TLabel;
var
  R: TRect;
  W: integer;
begin
  if (AText = '') then
    Exit(nil);
  Result := TLabel.Create(Self);
  Result.Parent := AParent;
  Result.WordWrap := True;
  if BigFont then
  begin
    if (tfEmulateClassicStyle in FDlg.Flags) then
    begin
      Result.Font.Height := AFontHeight-2;
      Result.Font.Style := [fsBold]
    end
    else
    begin
      Result.Font.Height := AFontHeight-4;
      Result.Font.Color := clHighlight;
    end;
  end
  else
    Result.Font.Height := AFontHeight;
  Result.AutoSize := False;
  R.Left := 0;
  R.Top := 0;
  W := aWidth-X-8;
  R.Right := W;
  R.Bottom := Result.Height;
  LCLIntf.DrawText(Result.Canvas.Handle,PChar(AText),Length(AText),R,DT_CALCRECT or DT_WORDBREAK);//lazarus does not return box height on OSX (Lazarus bug), the height is stored in the rect in all cases, so we don't need to use the Result

  Result.SetBounds(X,Y,W,R.Bottom);
  Result.Caption := AText;
  inc(Y,R.Bottom+16);
end;

procedure TLCLTaskDialog.AddQueryCombo(var X, Y: Integer; AWidth: Integer; AParent: TWinControl);
begin
  Combo := TComboBox.Create(Self);
  with Combo do
  begin
    Items.Assign(FDlg.QueryChoices);
    SetBounds(X,Y,aWidth-32-X,22);
    if (tfQueryFixedChoices in FDlg.Flags) then
      Style := csDropDownList
    else
      Style := csDropDown;
    if (FDlg.QueryItemIndex >= 0) and (FDlg.QueryItemIndex < FDlg.QueryChoices.Count) then
      ItemIndex := FDlg.QueryItemIndex
    else
    begin
      if (tfQueryFixedChoices in FDlg.Flags) then
        ItemIndex := 0
      else
        ItemIndex := -1;
    end;
    if (tfQueryFocused in FDlg.Flags) then
      ActiveControl := Combo;
    Parent := AParent;
  end;
  inc(Y,42);
end;

procedure TLCLTaskDialog.AddQueryEdit(var X, Y: Integer; AWidth: Integer; AParent: TWinControl);
begin
  Edit := TEdit.Create(Self);
  with Edit do
  begin
    SetBounds(X,Y,aWidth-16-X,22);
    Text := FDlg.SimpleQuery;
    PasswordChar := FDlg.SimpleQueryPasswordChar;
    Parent := AParent;
  end;
  if (tfQueryFocused in FDlg.Flags) then
    ActiveControl := Edit;
  inc(Y,42);
end;

procedure TLCLTaskDialog.DoDialogConstructed;
begin
  if Assigned(FDlg.OnDialogConstructed) then
    FDlg.OnDialogDestroyed(FDlg);
end;

procedure TLCLTaskDialog.DoDialogCreated;
begin
  if Assigned(FDlg.OnDialogCreated) then
    FDlg.OnDialogCreated(FDlg);
end;

procedure TLCLTaskDialog.DoDialogDestroyed;
begin
  if Assigned(FDlg.OnDialogDestroyed) then
    FDlg.OnDialogDestroyed(FDlg);
end;

procedure TLCLTaskDialog.VerifyClicked(Sender: TObject);
begin
  if Assigned(FDlg.OnVerificationClicked) then
    FDlg.OnVerificationClicked(FDlg);
end;


procedure TLCLTaskDialog.HandleEmulatedButtonClicked(Sender: TObject);
var Btn: TButton absolute Sender;
    CanClose: Boolean;
begin
  if Assigned(FDlg) and Assigned(FDlg.OnButtonClicked) then
  begin
    CanClose := True;
    FDlg.OnButtonClicked(FDlg, FDlg.ButtonIDToModalResult(Btn.ModalResult),CanClose);
    if not CanClose then
      ModalResult := mrNone;
  end;
end;


procedure TLCLTaskDialog.SetupControls;
var
  aRadioDef, aButtonDef: TModalResult;
  B: TTaskDialogBaseButtonItem;
  ButtonID: Integer;
  ARadioOffset, FontHeight, aWidth, IconBorder, X, Y, i, XB: integer;
  CurrParent: TWinControl;
  aDialogIcon: TLCLTaskDialogIcon;
  CommandLink: TBitBtn;
  aHint: String;
  List: TStringListUTF8Fast;
  Btn: TTaskDialogCommonButton;
  CustomButtonsTextLength: Integer;
begin

  if FDlg.RadioButtons.DefaultButton<> nil then
    aRadioDef := FDlg.RadioButtons.DefaultButton.Index
  else
    aRadioDef := 0;
  if FDlg.Buttons.DefaultButton<>nil then
    aButtonDef := FDlg.Buttons.DefaultButton.ModalResult
  else
    aButtonDef := TD_BTNMOD[FDlg.DefaultButton];

  CustomButtonsTextLength := 0;
  for B in FDlg.Buttons do
    CustomButtonsTextLength := CustomButtonsTextLength + Length(B.Caption);


  DialogCaption := FDlg.Caption;
  DlgTitle := FDlg.Title;
  DlgText := FDlg.Text;
  ExpandedButtonCaption := FDlg.ExpandButtonCaption;
  ExpandedText := FDlg.ExpandedText;
  FooterText := FDlg.FooterText;
  VerificationText := FDlg.VerificationText;
  FVerifyChecked := (tfVerificationFlagChecked in FDlg.Flags);

  CommonButtons := FDlg.CommonButtons;

  if (CommonButtons=[]) and (FDlg.Buttons.Count=0) then
  begin
    CommonButtons := [tcbOk];
    if (aButtonDef = 0) then
      aButtonDef := mrOk;
  end;

  if (DialogCaption = '') then
    if (Application.MainForm = nil) then
      DialogCaption := Application.Title
    else
      DialogCaption := Application.MainForm.Caption;

  if (DlgTitle = '') then
    DlgTitle := IconMessage(TF_DIALOGICON(FDlg.MainIcon));

  PixelsPerInch := 96; // we are using 96 PPI in the code, scale it automatically at ShowModal
  Font.PixelsPerInch := 96;
  BorderStyle := bsDialog;
  if (tfAllowDialogCancellation in FDlg.Flags) then
    BorderIcons := [biSystemMenu]
  else
    BorderIcons := [];
  if (tfPositionRelativeToWindow in FDlg.Flags) then
    Position := poOwnerFormCenter
  else
    Position := poScreenCenter;

  if not (tfEmulateClassicStyle in FDlg.Flags) then
    Font := DefaultFont;

  FontHeight := Font.Height;
  if (FontHeight = 0) then
    FontHeight := Screen.SystemFont.Height;

  aWidth := FDlg.Width;
  if (aWidth <= 0) then
  begin
    aWidth := Canvas.TextWidth(DlgTitle);
    if (aWidth > 300) or (Canvas.TextWidth(DlgText) > 300) or
       (CustomButtonsTextLength > 40) then
      aWidth := 480 else
      aWidth := 420;
  end
  else
    if (aWidth < 120) then aWidth := 120;
  ClientWidth := aWidth;

  Height := TaskDialogFirstRadioButtonIndex;
  Caption := DialogCaption;

  // create a white panel for the main dialog part
  AddPanel;
  CurrParent := Panel;

  // handle main dialog icon
  AddIcon(IconBorder, X, Y, CurrParent);

  // add main texts (DlgTitle, DlgText, Information)
  Element[tdeMainInstruction] := AddLabel(DlgTitle, True, X, Y, FontHeight, aWidth, CurrParent);
  Element[tdeContent] := AddLabel(DlgText, False, X, Y, FontHeight, aWidth, CurrParent);
  if (ExpandedText <> '') then
    // no information collapse/expand yet: it's always expanded
    Element[tdeExpandedInfo] := AddLabel(ExpandedText, False, X, Y,  FontHeight, aWidth, CurrParent);


  // add radio CustomButtons
  if (FDlg.RadioButtons.Count > 0) then
  begin
    ARadioOffset := 1;
    AddRadios(ARadioOffSet, aWidth, aRadioDef, FontHeight, X, Y, CurrParent);
  end;

  // add command links CustomButtons
  if (tfUseCommandLinks in FDlg.Flags) and (FDlg.Buttons.Count<>0) then
    AddCommandLinkButtons(X, Y, aWidth, aButtonDef, FontHeight, CurrParent);


  // add query combobox list or query edit
  if (tfQuery in FDlg.Flags) and (FDlg.QueryChoices.Count > 0) then
    AddQueryCombo(X, Y, aWidth, CurrParent)
  else
  begin
    if (tfSimpleQuery in FDlg.Flags) and (FDlg.SimpleQuery <> '') then
      AddQueryEdit(X, Y, aWidth, CurrParent);
  end;

  // from now we won't add components to the white panel, but to the form
  Panel.Height := Y;
  CurrParent := Self;

  XB := 0;
  // add CustomButtons and verification checkbox
  if (CommonButtons <> []) or (VerificationText<>'') or
     ((FDlg.Buttons.Count<>0) and not (tfUseCommandLinks in FDlg.Flags)) then
  begin
    AddButtonsAndCheckBox(X, Y, XB, aWidth, aButtonDef, CurrParent);
  end;

  // add FooterText text with optional icon
  if (FooterText <> '') then
    AddFooter(X, Y, XB, FontHeight, aWidth, CurrParent);

   ClientHeight := Y;
end;

procedure TLCLTaskDialog.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (biSystemMenu in BorderIcons) then//is Alt+F4/Esc cancellation allowed?
  begin//yes -> cancel on ESC
    if Key = VK_ESCAPE then
      Close;
  end else
  begin//no -> block Alt+F4
    if (Key = VK_F4) and (ssAlt in Shift) then//IMPORTANT: native task dialog blocks Alt+F4 to close the dialog -> we have to block it as well
      Key := 0;
  end;

  inherited KeyDown(Key, Shift);
end;

finalization
  if assigned(LDefaultFont) then
    LDefaultFont.Free;


end.
