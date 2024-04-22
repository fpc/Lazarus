{
 /***************************************************************************
                            TaskDlgEmulation.pp
                            -----------------

 Implements TaskDialog Window on systems that do not support it natively
 This unit was originally a part of the freeware Synopse mORMot framework,
 licensed under a MPL/GPL/LGPL tri-license; version 1.19.
 It has been relicensed with permission from Arnaud Bouchez, the original
 author, and all contributors.

 The original name is SynTaskDialog.pas

 ***************************************************************************/

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}

unit TaskDlgEmulation;

{
  This unit tries to emulate the functionality of Windows Vista and higher TaskDialogIndirect.
  It also adds capabilities that TaskDialogIndirect does not have, currently:
    * Query via combobox
    * Query via single line edit, which supports masking the input for use eith e.g. passwords

  The emulated dialog does not aim to be visually (near) exactly the same as the Vista+ native dialog.

  This dialog is invoked by Dialogs.TTaskDialog.Execute on systems that do not support
  the native Vista+ dialog, and it is also used as a fallback in case the native
  Vista+ dialog fails (when passed invalid combination of arguments).
  The dialog therefore uses the Flags property of Dialogs.TTaskDialog, but not
  all of these flags are supported (yet) in the emulated dialog.
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  LazUTF8,
  LCLType, LCLStrConsts, LCLIntf, LMessages, InterfaceBase, ImgList, LCLProc, DateUtils, Math, ComCtrls,
  LResources, Menus, Graphics, Forms, Controls, StdCtrls, ExtCtrls, Buttons, Dialogs, DialogRes,
  LazLoggerBase;

type

  TTaskDialogElement = (
    tdeContent, tdeExpandedInfo, tdeFooter, tdeMainInstruction,
    tdeEdit, tdeVerif);


  { TLCLTaskDialog }

  TLCLTaskDialog = class(TForm)
  private
    const
      RadioIndent = 16;
      ComboBoxHeight = 22;
      QueryEditHeight = 22;
      LargeImageSize = 32;
      SmallImageSize = 16;
      CommandLinkButtonHeight = 40;
      RadioVSpacing = 16;
      LabelVSpacing = 16;
      CommandLinkButtonVSpacing = 2;
      BevelMargin = 2;
      BevelHeight = 2;
      ProgressBarHeight = 20;
      ProgressBarVSpacing = 16;
  private
    /// the Task Dialog structure which created the form
    FDlg: TTaskDialog;
    FVerifyChecked: Boolean;
    FExpanded: Boolean;
    CommandLinkButtonWidth: Integer;
    CommandLinkButtonMargin: Integer;
    CommandLinkButtonSpacing: Integer; //Height of TBitBtns
    ButtonHeight: Integer;  //Height of TButtons
    GlobalLeftMargin: Integer;
    ExpandHeightRequired: Integer;
    Timer: TTimer;
    TimerStartTime: TTime;
    RadioButtonArray: array of TRadioButton;

    //CustomButtons, Radios: TStringList;
    DialogCaption, DlgTitle, DlgText,
    ExpandButtonCaption, CollapseButtonCaption, ExpandedText, FooterText,
    VerificationText: String;
    CommonButtons: TTaskDialogCommonButtons;

    TopPanel: TPanel;
    MidPanel: TPanel;
    BottomPanel: TPanel;
    MainImage: TImage;
    FooterImage: TImage;
    ExpandedTextBevel: TBevel;
    /// the labels corresponding to the Task Dialog main elements
    Element: array[tdeContent..tdeMainInstruction] of TLabel;
    /// the Task Dialog query selection list
    QueryCombo: TComboBox;
    /// the Task Dialog optional query single line editor
    QueryEdit: TEdit;
    /// the Task Dialog optional checkbox
    VerifyCheckBox: TCheckBox;
    /// the Expand/Collapse button
    ExpandBtn: TButton;
    ///
    ProgressBar: TProgressBar;

    procedure GetDefaultButtons(out aButtonDef, aRadioDef: TModalResult);
    procedure InitCaptions;
    procedure InitGlobalDimensionsAndStyle(ACustomButtonsTextLength: Integer; out aWidth, aFontHeight: Integer);
    function GetGlobalLeftMargin: Integer;
    procedure AddMainIcon(out ALeft,ATop: Integer; AGlobalLeftMargin: Integer; AParent: TWinControl);
    procedure AddPanels;
    procedure AddProgressBar(const ALeft: Integer; var ATop: Integer; AWidth: Integer; AParent: TWinControl);
    procedure AddRadios(const ARadioOffSet, AWidth, ARadioDef, AFontHeight, ALeft: Integer; var ATop: Integer; AParent: TWinControl);
    procedure AddCommandLinkButtons(const ALeft: Integer; var ATop: Integer; AWidth, AButtonDef, AFontHeight: Integer; AParent: TWinControl);
    procedure AddButtons(const ALeft: Integer; var ATop, AButtonLeft: Integer; AWidth, AButtonDef: Integer; APArent: TWinControl);
    procedure AddCheckBox(const ALeft: Integer; var ATop, XB: Integer; AWidth: Integer; APArent: TWinControl);
    procedure AddExpandButton(const ALeft: Integer; var ATop, XB: Integer; AWidth: Integer; APArent: TWinControl);
    function AddBevel(var ATop: Integer; aWidth: Integer; AParent: TWinControl; Hidden: Boolean = False): TBevel;
    procedure AddFooter(var ALeft: Integer; var ATop: Integer; AFontHeight, AWidth: Integer; APArent: TWinControl);
    function AddLabel(const AText: string; BigFont: Boolean; const ALeft: Integer; var ATop: Integer; AFontHeight,
                      AWidth: Integer; APArent: TWinControl; Hidden: Boolean = False): TLabel;
    procedure AddQueryCombo(const ALeft: Integer; var ATop: Integer; AWidth: Integer; AParent: TWinControl);
    procedure AddQueryEdit(var X,Y: Integer; AWidth: Integer; AParent: TWinControl);
    procedure SetupTimer;
    procedure ResetTimer;
    procedure ExpandDialog;
    procedure CollapseDialog;

    function FindButtonByButtonID(ID: Integer): TCustomButton;
    function FindRadioButtonByButtonID(ID: Integer): TRadioButton;

    procedure DoDialogConstructed;
    procedure DoDialogCreated;
    procedure DoDialogDestroyed;
    procedure OnButtonClicked(Sender: TObject);
    procedure OnRadioButtonClick(Sender: TObject);
    procedure OnVerifyClicked(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure OnExpandButtonClicked(Sender: TObject);
    procedure DoOnHelp;

    procedure SetProgressBarType(var Msg: TLMessage); message TDM_SET_MARQUEE_PROGRESS_BAR;
    procedure SetProgressBarRange(var Msg: TLMessage); message TDM_SET_PROGRESS_BAR_RANGE;
    procedure SetProgressBarPos(var Msg: TLMessage); message TDM_SET_PROGRESS_BAR_POS;
    procedure ClickVerification(var Msg: TLMessage); message TDM_CLICK_VERIFICATION;
    procedure ClickButton(var Msg: TLMessage); message TDM_CLICK_BUTTON;
    procedure ClickRadioButton(var Msg: TLMessage); message TDM_CLICK_RADIO_BUTTON;
    procedure EnableButton(var Msg: TLMessage); message TDM_ENABLE_BUTTON;
    procedure EnableRadioButton(var Msg: TLMessage); message TDM_ENABLE_RADIO_BUTTON;
    procedure UpdateElementText(var Msg: TLMessage); message TDM_UPDATE_ELEMENT_TEXT;

  protected
    procedure SetupControls;
  public
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure DoShow; override;

    function Execute(AParentWnd: HWND; out ARadioRes: Integer): Integer;
  public
  end;



function ExecuteLCLTaskDialog(const ADlg: TCustomTaskDialog; AParentWnd: HWND; out ARadioRes: Integer): Integer;


type
  TLCLTaskDialogIcon = (
    tiBlank, tiWarning, tiQuestion, tiError, tiInformation, tiNotUsed, tiShield);
  TLCLTaskDialogFooterIcon = (
    tfiBlank, tfiWarning, tfiQuestion, tfiError, tfiInformation, tfiShield);

function IconMessage(Icon: TTaskDialogIcon): string;

implementation

type
  TTaskDialogAccess = class(TCustomTaskDialog)
  end;

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
  LCL_IMAGES: array[TTaskDialogIcon] of Integer = (
    0 {tdiNone}, idDialogWarning {tdiWarning}, idDialogError {tdiError}, idDialogInfo {tdiInformation},
    idDialogShield {tdiShield}, idDialogConfirm {tdiQuestion});

const
  TD_BTNMOD: array[TTaskDialogCommonButton] of Integer = (
    mrOk, mrYes, mrNo, mrCancel, mrRetry, mrAbort);


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



function IconMessage(Icon: TTaskDialogIcon): string;
begin
  case Icon of
    tdiWarning:   Result := rsMtWarning;
    tdiQuestion:  Result := rsMtConfirmation;
    tdiError:     Result := rsMtError;
    tdiInformation, tdiShield: Result := rsMtInformation;
    else Result := '';
  end;
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
  if (AOwner is TCustomTaskDialog) then
  begin
    FDlg := TTaskDialog(AOwner);
    if (csDesigning in FDlg.ComponentState) then
      AOwner:=nil; // do not inherit csDesigning, a normal taskdialog should be shown
  end;

  inherited CreateNew(AOwner, Num);
  RadioButtonArray := nil;
  FExpanded := False;
  CommandLinkButtonWidth := -1;
  KeyPreview := True;
  //DoDialogCreated;
end;

destructor TLCLTaskDialog.Destroy;
begin
  DoDialogDestroyed;
  inherited Destroy;
end;

procedure TLCLTaskDialog.AfterConstruction;
begin
  inherited AfterConstruction;
  //DoDialogConstructed;
end;

procedure TLCLTaskDialog.DoShow;
begin
  inherited DoShow;
  {
    If we call GetHandle in AfterConstrucion this triggers a CreateWnd, but later on
    (as a consequence of using CreateNew ??) the window gets destroyed and recreated
    with a different handle, so we cannot call FDlg.InternalSetDialogHandle in CreateNew
    or AfterConstruction.
    And since we want to have a valid FDlg.Handle in all OnDialogXXX events, we do it here.
  }
  DoDialogConstructed;
  DoDialogCreated;
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

  if Assigned(QueryCombo) then
  begin
    FDlg.QueryItemIndex := QueryCombo.ItemIndex;
    FDlg.QueryResult := QueryCombo.Text;
  end
  else
  begin
    if Assigned(QueryEdit) then
      FDlg.QueryResult := QueryEdit.Text;
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

procedure TLCLTaskDialog.GetDefaultButtons(out aButtonDef, aRadioDef: TModalResult);
begin
  if FDlg.RadioButtons.DefaultButton<> nil then
   aRadioDef := FDlg.RadioButtons.DefaultButton.Index
 else
   aRadioDef := 0;
 if FDlg.Buttons.DefaultButton<>nil then
   aButtonDef := FDlg.Buttons.DefaultButton.ModalResult
 else
   aButtonDef := TD_BTNMOD[FDlg.DefaultButton];

end;

procedure TLCLTaskDialog.InitCaptions;
begin
  DialogCaption := FDlg.Caption;
  DlgTitle := FDlg.Title;
  DlgText := FDlg.Text;
  ExpandButtonCaption := FDlg.ExpandButtonCaption;
  CollapseButtonCaption := FDlg.CollapseButtonCaption;
  ExpandedText := FDlg.ExpandedText;
  FooterText := FDlg.FooterText;
  VerificationText := FDlg.VerificationText;
 if (DialogCaption = '') then
   if (Application.MainForm = nil) then
     DialogCaption := Application.Title
   else
     DialogCaption := Application.MainForm.Caption;
 if (DlgTitle = '') then
   DlgTitle := IconMessage(FDlg.MainIcon);
end;

procedure TLCLTaskDialog.InitGlobalDimensionsAndStyle(ACustomButtonsTextLength: Integer; out aWidth, aFontHeight: Integer);
begin
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

  aFontHeight := Font.Height;
  if (aFontHeight = 0) then
    aFontHeight := Screen.SystemFont.Height;

  aWidth := FDlg.Width;
  if (aWidth <= 0) then
  begin
    aWidth := Canvas.TextWidth(DlgTitle);
    if (aWidth > 300) or (Canvas.TextWidth(DlgText) > 300) or
       (ACustomButtonsTextLength > 40) then
      aWidth := 480 else
      aWidth := 420;
  end
  else
    if (aWidth < 120) then aWidth := 120;
  ClientWidth := aWidth;

  if (tfEmulateClassicStyle in FDlg.Flags) then
  begin
    CommandLinkButtonMargin := 7;
    CommandLinkButtonSpacing := 7;
    ButtonHeight := 22;
  end
  else
  begin
    CommandLinkButtonMargin := 24;
    CommandLinkButtonSpacing := 10;
    ButtonHeight := 28;
  end;
  Height := 200;
  //debugln(['Font: Name=',Font.Name,', Size=',Font.Size,', Height=',Font.Height]);
end;

function TLCLTaskDialog.GetGlobalLeftMargin: Integer;
begin
  if (tfEmulateClassicStyle in FDlg.Flags) then
    Result := 10
  else
    Result := 16;
end;

procedure TLCLTaskDialog.AddMainIcon(out ALeft,ATop: Integer; AGlobalLeftMargin: Integer; AParent: TWinControl);
var
  aDialogIcon: TTaskDialogIcon;
begin
  MainImage := nil;
  if not (tfUseHIconMain in FDlg.Flags) then
  begin
    aDialogIcon := FDlg.MainIcon;
    if (LCL_IMAGES[aDialogIcon]<>0) then
    begin
      MainImage := TImage.Create(Self);
      MainImage.Parent := AParent;
      MainImage.Images := DialogGlyphs;
      MainImage.ImageIndex := DialogGlyphs.DialogIcon[LCL_IMAGES[aDialogIcon]];
    end;
  end
  else
  begin
    if Assigned(FDlg.CustomMainIcon) and not (FDlg.CustomMainIcon.Empty) then
    begin
      MainImage := TImage.Create(Self);
      MainImage.Parent := AParent;
      MainImage.Picture.Assign(FDlg.CustomMainIcon);
    end;
  end;
  if Assigned(MainImage) then
  begin
    MainImage.SetBounds(AGlobalLeftMargin, AGlobalLeftMargin, LargeImageSize, LargeImageSize);
    MainImage.Stretch := True;
    MainImage.StretchOutEnabled := False;
    MainImage.Proportional := True;
    MainImage.Center := True;
    ALeft := MainImage.Width+AGlobalLeftMargin*2;
    ATop := MainImage.Top;
    if (tfEmulateClassicStyle in FDlg.Flags) then
      inc(ATop, 8);
  end
  else
  begin
    ALeft := AGlobalLeftMargin;
    ATop := AGlobalLeftMargin;
  end;
end;

procedure TLCLTaskDialog.AddPanels;
begin
  {
    Create 3 different panels:
    - the top panel holds main icon, title, text and expanded text
    - the mid panel holds radiobuttons, commandlinkbuttons and query's
      (basically everything that comes after ExpandedText and needs to be on a "colored" panel)
    - the bottom panel has the rest of the controls
    The top and mid panel have a distinct color (unless tfEmulateClassicStyle is set)
    The reason for the 3 panel setup is that it makes it a lot easier to displace the controls
    when Expand or Collapse is invoked: just move/resize the appropriate panels, no need to
    iterate the individual controls on it.
  }
  TopPanel := TPanel.Create(Self);
  TopPanel.Parent := Self;
  TopPanel.Align := alTop;
  TopPanel.BorderStyle := bsNone;
  TopPanel.BevelOuter := bvNone;
  if not (tfEmulateClassicStyle in FDlg.Flags) then
    TopPanel.Color := clWindow;
  TopPanel.Name := 'TopPanel'; //for debugging purposes
  TopPanel.Caption := '';

  MidPanel := TPanel.Create(Self);
  MidPanel.Parent := Self;
  MidPanel.Top := TopPanel.Top + TopPanel.Height + 1;
  MidPanel.Align := alTop;
  MidPanel.BorderStyle := bsNone;
  MidPanel.BevelOuter := bvNone;
  MidPanel.Color := TopPanel.Color;
  MidPanel.Name := 'MidPanel'; //for debugging purposes
  MidPanel.Caption := '';

  BottomPanel := TPanel.Create(Self);
  BottomPanel.Parent := Self;
  BottomPanel.Top := MidPanel.Top + MidPanel.Height + 1;
  BottomPanel.Align := alCLient;
  BottomPanel.BorderStyle := bsNone;
  BottomPanel.BevelOuter := bvNone;
  BottomPanel.Name := 'BottomPanel'; //for debugging purposes
  BottomPanel.Caption := '';

end;

procedure TLCLTaskDialog.AddProgressBar(const ALeft: Integer; var ATop: Integer; AWidth: Integer; AParent: TWinControl);
begin
  Inc(ATop, ProgressBarVSpacing);
  ProgressBar := TProgressBar.Create(Self);
  if (tfShowMarqueeProgressBar in FDlg.Flags) then
    ProgressBar.Style := pbstMarquee
  else
  begin
    ProgressBar.Style := pbstNormal;
    ProgressBar.Min := FDlg.ProgressBar.Min;
    ProgressBar.Max := FDlg.ProgressBar.Max;
    ProgressBar.Position := FDlg.ProgressBar.Position;
  end;
  ProgressBar.SetBounds(ALeft, ATop, AWidth-ALeft-GlobalLeftMargin, ProgressBarHeight);
  Inc(ATop, ProgressBar.Height + ProgressBarVSpacing);
  ProgressBar.Parent := AParent;
end;

procedure TLCLTaskDialog.AddRadios(const ARadioOffSet, AWidth, ARadioDef, AFontHeight, ALeft: Integer; var ATop: Integer; AParent: TWinControl);
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
      Tag := FDlg.RadioButtons[i].Index + TaskDialogFirstRadioButtonIndex;
      AutoSize := False;
      SetBounds(ALeft+RadioIndent,ATop,aWidth-(2*RadioIndent)-ALeft, (6-AFontHeight) + ARadioOffset);
      Caption := FDlg.RadioButtons[i].Caption;
      inc(ATop,Height + ARadioOffset);
      if not (tfNoDefaultRadioButton in FDlg.Flags) and ((i=0) or (i=aRadioDef)) then
        Checked := True;
      OnClick := @OnRadioButtonClick;
    end;
  end;
  inc(ATop,24);
end;

procedure TLCLTaskDialog.AddCommandLinkButtons(const ALeft: Integer; var ATop: Integer; AWidth, AButtonDef, AFontHeight: Integer; AParent: TWinControl);
var
  i: Integer;
  CommandLink: TBitBtn;
  aHint: String;
begin
  inc(ATop,8);
  for i := 0 to FDlg.Buttons.Count-1 do
  begin
    CommandLink := TBitBtn.Create(Self);
    with CommandLink do
    begin
      Parent := AParent;
      Font.Height := AFontHeight-3;
      CommandLinkButtonWidth := aWidth - ALeft - GlobalLeftMargin;
      SetBounds(ALeft,ATop,CommandLinkButtonWidth,CommandLinkButtonHeight);
      Caption := FDlg.Buttons[i].Caption;
      Hint := FDlg.Buttons[i].CommandLinkHint;
      if (Hint <> '') then
        ShowHint := True;
      inc(ATop,Height+CommandLinkButtonVSpacing);
      ModalResult := i+TaskDialogFirstButtonIndex;
      OnClick := @OnButtonClicked;
      if ModalResult=aButtonDef then
        ActiveControl := CommandLink;
      if (tfEmulateClassicStyle in FDlg.Flags) then
      begin
        Font.Height := AFontHeight - 2;
        Font.Style := [fsBold]
      end;
      Margin := CommandLinkButtonMargin;
      Spacing := CommandLinkButtonSpacing;
      if not (tfUseCommandLinksNoIcon in FDlg.Flags) then
      begin
        Images := LCLGlyphs;
        ImageIndex := LCLGlyphs.GetImageIndex('btn_arrowright');
        end;
      end;
    end;
  inc(ATop,24);
end;

procedure TLCLTaskDialog.AddButtons(const ALeft: Integer; var ATop, AButtonLeft: Integer; AWidth, AButtonDef: Integer; APArent: TWinControl);
var
  CurrTabOrder, i: Integer;
  Btn: TTaskDialogCommonButton;

  function AddButton(const s: string; AModalResult, ACustomButtonIndex: integer): TButton;
  var
    WB: integer;
  begin
    WB := Canvas.TextWidth(s)+52;
    dec(AButtonLeft,WB);
    if AButtonLeft<ALeft {shr 1} then
    begin
      AButtonLeft := aWidth-WB;
      inc(ATop,32);
    end;
    Result := TButton.Create(Self);
    Result.Parent := AParent;
    Result.SetBounds(AButtonLeft,ATop,WB-10,ButtonHeight);
    Result.Caption := s;
    Result.ModalResult := AModalResult;
    Result.TabOrder := CurrTabOrder;
    Result.OnClick := @OnButtonClicked;

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
  //debugln(['TLCLTaskDialog.AddButtons: ALeft=',ALeft,', aWidth=',aWidth,', AParent=',DbgSName(AParent),', AParent.ClientWidth=',AParent.ClientWidth]);
  if MidPanel.ControlCount > 0 then
    CurrTabOrder := MidPanel.TabOrder
  else
    CurrTabOrder := TopPanel.TabOrder;
  inc(ATop, 16);
  AButtonLeft := aWidth;
  if not (tfUseCommandLinks in FDlg.Flags) then
    for i := FDlg.Buttons.Count-1 downto 0 do
      AddButton(FDlg.Buttons[i].Caption,i+TaskDialogFirstButtonIndex,i);
  for Btn := high(TTaskDialogCommonButton) downto low(TTaskDialogCommonButton) do
  begin
    if (Btn in CommonButtons) then
      AddButton(LoadResString(TD_BTNS(Btn)), TD_BTNMOD[Btn],-1);
  end;
end;

procedure TLCLTaskDialog.AddCheckBox(const ALeft: Integer; var ATop, XB: Integer; AWidth: Integer; APArent: TWinControl);
begin
  //debugln(['TLCLTaskDialog.AddCheckBox: ALeft=',ALeft]);
  VerifyCheckBox := TCheckBox.Create(Self);
  with VerifyCheckBox do
  begin
    Parent := AParent;
    if (ALeft+16+Canvas.TextWidth(VerificationText) > XB) then begin
      inc(ATop,32);
      XB := aWidth;
    end;
    SetBounds(ALeft,ATop,XB-ALeft,24);
    Caption := VerificationText;
    Checked := FVerifyChecked;
    OnClick := @OnVerifyClicked;
  end;
end;

procedure TLCLTaskDialog.AddExpandButton(const ALeft: Integer; var ATop, XB: Integer; AWidth: Integer; APArent: TWinControl);
var
  CurrTabOrder: TTabOrder;
  WB, AHeight: Integer;
begin
  if MidPanel.ControlCount > 0 then
    CurrTabOrder := MidPanel.TabOrder
  else
    CurrTabOrder := TopPanel.TabOrder;
  if (ExpandButtonCaption = '') then
  begin
    if (CollapseButtonCaption = '') then
    begin
      ExpandButtonCaption := rsShowDetails;
      CollapseButtonCaption := rsHideDetails;
    end
    else
      ExpandButtonCaption := CollapseButtonCaption;
  end;
  if (CollapseButtonCaption = '') then
    CollapseButtonCaption := ExpandButtonCaption;
  WB := Max(Canvas.TextWidth(ExpandButtonCaption), Canvas.TextWidth(CollapseButtonCaption)) +32;//52;
  if (ALeft+WB > XB) then
  begin
    inc(ATop,32);
    XB := aWidth;
  end;

  ExpandBtn := TButton.Create(Self);
  ExpandBtn.Parent := AParent;
  if (tfEmulateClassicStyle in FDlg.Flags) then
    AHeight := 22
  else
    AHeight := 28;
  ExpandBtn.SetBounds(ALeft,ATop,WB-12,AHeight);
  if not (tfExpandedByDefault in FDlg.Flags) then
    ExpandBtn.Caption := ExpandButtonCaption
  else
    ExpandBtn.Caption := CollapseButtonCaption;
  ExpandBtn.ModalResult := mrNone;
  ExpandBtn.TabOrder := CurrTabOrder;
  ExpandBtn.OnClick := @OnExpandButtonClicked;
  Inc(ATop, AHeight+8);
end;

function TLCLTaskDialog.AddBevel(var ATop: Integer; aWidth: Integer; AParent: TWinControl; Hidden: Boolean): TBevel;
begin
  Result := TBevel.Create(Self);
  with Result do begin
    Parent := AParent;
    //if (FooterImage<>nil) and (ATop<FooterImage.Top+FooterImage.Height) then
    //  BX := ALeft else
    //  BX := BevelMargin;
    SetBounds(BevelMargin,ATop,aWidth-2*BevelMargin,BevelHeight);
    if Hidden then
      Visible := False
    else
      Inc(ATop, BevelHeight);
  end;
end;

procedure TLCLTaskDialog.AddFooter(var ALeft: Integer; var ATop: Integer; AFontHeight, AWidth: Integer; APArent: TWinControl);
//ALeft must be adjusted by AddFooter if FooterIcon exists, so that we can left-align
//ExpandedText in the Footer area with the FooterText (in case of tfExpandFooterArea)
var
  aFooterIcon: TTaskDialogIcon;
begin
  //debugln(['AddFooterText: XB=',XB]);
  AddBevel(ATop, aWidth, AParent);
  inc(ATop,LabelVSPacing div 2);
  FooterImage := nil;
  if not (tfUseHIconFooter in FDlg.Flags) then
  begin
    aFooterIcon := FDlg.FooterIcon;
    if (LCL_IMAGES[aFooterIcon]<>0) then
    begin
      FooterImage := TImage.Create(Self);
      FooterImage.Parent := AParent;
      FooterImage.Images := DialogGlyphs;
      FooterImage.ImageWidth := SmallImageSize;
      FooterImage.ImageIndex := DialogGlyphs.DialogIcon[LCL_IMAGES[aFooterIcon]];
    end;
  end
  else
  begin
    if Assigned(FDlg.CustomFooterIcon) and not (FDlg.CustomFooterIcon.Empty) then
    begin
      FooterImage := TImage.Create(Self);
      FooterImage.Parent := AParent;
      FooterImage.ImageWidth := SmallImageSize;
      FooterImage.Picture.Assign(FDlg.CustomFooterIcon);
    end;
  end;
  if Assigned(FooterImage) then
  begin
    FooterImage.Stretch := True;
    FooterImage.StretchOutEnabled := False;
    FooterImage.Proportional := True;
    FooterImage.Center := True;
    FooterImage.SetBounds(GlobalLeftMargin,ATop,SmallImageSize,SmallImageSize);
    ALeft := GlobalLeftMargin + Aleft + FooterImage.Width;
  end;
  Element[tdeFooter] := AddLabel(FooterText, False, ALeft, ATop, AFontHeight, AWidth, AParent);
  Dec(ATop, LabelVSpacing div 2);
end;

function TLCLTaskDialog.AddLabel(const AText: string; BigFont: Boolean; const ALeft: Integer; var ATop: Integer; AFontHeight,
                                 AWidth: Integer; APArent: TWinControl; Hidden: Boolean = False): TLabel;
var
  R: TRect;
  W: integer;
begin
  //debugln(['TLCLTaskDialog.AddLabel A: AText=',AText,',X=',ALeft,', AParent=',DbgSName(AParent),', AParent.Width=',AParent.Width,', Self.Width=',Self.Width]);
  if (AText = '') then
    Exit(nil);
  Result := TLabel.Create(Self);
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
  W := aWidth-ALeft-GlobalLeftMargin;
  R.Right := W;
  R.Bottom := Result.Height;
  Result.Caption := AText;
  Result.Parent := AParent;
  LCLIntf.DrawText(Result.Canvas.Handle,PChar(AText),Length(AText),R,DT_CALCRECT or DT_WORDBREAK);
  Result.SetBounds(ALeft,ATop,W,R.Bottom);
  if not Hidden then
    inc(ATop,R.Bottom+LabelVSpacing)
  else
    Result.Visible := False;
  //else
  //  ExpandHeightRequired := R.Bottom+LabelVSpacing;
  //debugln(['TLCLTaskDialog.AddLabel End: X=',ALeft,', Result.Left=',Result.Left]);
end;

procedure TLCLTaskDialog.AddQueryCombo(const ALeft: Integer; var ATop: Integer; AWidth: Integer; AParent: TWinControl);
begin
  QueryCombo := TComboBox.Create(Self);
  with QueryCombo do
  begin
    Items.Assign(FDlg.QueryChoices);
    if (CommandLinkButtonWidth > 0) then
      SetBounds(ALeft,ATop,CommandLinkButtonWidth,ComboBoxHeight) //right align with the buttons
    else
      SetBounds(ALeft,ATop,aWidth-2*GlobalLeftMargin-ALeft,ComboBoxHeight);
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
    Parent := AParent;
  end;
  inc(ATop,42);
end;

procedure TLCLTaskDialog.AddQueryEdit(var X, Y: Integer; AWidth: Integer; AParent: TWinControl);
begin
  QueryEdit := TEdit.Create(Self);
  with QueryEdit do
  begin
    if (CommandLinkButtonWidth > 0) then
      SetBounds(X,Y,CommandLinkButtonWidth,QueryEditHeight) //right align with the buttons
    else
    SetBounds(X,Y,aWidth-16-X,22);
    Text := FDlg.SimpleQuery;
    PasswordChar := FDlg.SimpleQueryPasswordChar;
    Parent := AParent;
  end;
  inc(Y,42);
end;

procedure TLCLTaskDialog.OnExpandButtonClicked(Sender: TObject);
begin
  if not FExpanded then
    ExpandDialog
  else
    CollapseDialog;
  FExpanded := not FExpanded;
  {$PUSH}
  {$ObjectChecks OFF}
  {%H-}TTaskDialogAccess(FDlg).DoOnExpandButtonClicked(FExpanded);
  {$POP}
end;

procedure TLCLTaskDialog.OnTimer(Sender: TObject);
var
  AResetTimer: Boolean;
  MSecs: Cardinal;
  MSecs64: Int64;
begin
  MSecs64 := MilliSecondsBetween(Now, TimerStartTime);
  {$PUSH}{$R-}
  MSecs := MSecs64;
  {$POP}
  AResetTimer := False;
  {$PUSH}
  {$ObjectChecks OFF}
  {%H-}TTaskDialogAccess(FDlg).DoOnTimer(MSecs, AResetTimer);
  {$POP}
  if AResetTimer then
    ResetTimer;
end;

procedure TLCLTaskDialog.DoOnHelp;
begin
  {$PUSH}
  {$ObjectChecks OFF}
  {%H-}TTaskDialogAccess(FDlg).DoOnHelp;
  {$POP}
end;


procedure TLCLTaskDialog.OnRadioButtonClick(Sender: TObject);
var
  ButtonID: Integer;
begin
  ButtonID := (Sender as TRadioButton).Tag;
  {$PUSH}
  {$ObjectChecks OFF}
  {%H-}TTaskDialogAccess(FDlg).DoOnRadioButtonClicked(ButtonID);
  {$POP}
end;

procedure TLCLTaskDialog.SetupTimer;
begin
  Timer := TTimer.Create(Self);
  Timer.Interval := 200; //source: https://learn.microsoft.com/en-us/windows/win32/controls/tdn-timer
  Timer.OnTimer := @OnTimer;
  TimerStartTime := Now;
  Timer.Enabled := True;
end;

procedure TLCLTaskDialog.ResetTimer;
begin
  Timer.Enabled := False;
  TimerStartTime := Now;
  Timer.Enabled := True;
end;

procedure TLCLTaskDialog.ExpandDialog;
begin
  ExpandBtn.Caption := CollapseButtonCaption;
  if not (tfExpandFooterArea in FDlg.Flags) then
  begin
    Element[tdeExpandedInfo].Parent.Height := Element[tdeExpandedInfo].Parent.Height + ExpandHeightRequired;
    Height := Height + ExpandHeightRequired;
    Element[tdeExpandedInfo].Visible := True;
  end
  else
  begin
    Height := Height + ExpandHeightRequired;
    ExpandedTextBevel.Visible := True;
    Element[tdeExpandedInfo].Visible := True;
  end;
end;

procedure TLCLTaskDialog.CollapseDialog;
begin
  ExpandBtn.Caption := ExpandButtonCaption;
  if not (tfExpandFooterArea in FDlg.Flags) then
  begin
    Element[tdeExpandedInfo].Visible := False;
    Element[tdeExpandedInfo].Parent.Height := Element[tdeExpandedInfo].Parent.Height - ExpandHeightRequired;
    Height := Height - ExpandHeightRequired;
  end
  else
  begin
    ExpandedTextBevel.Visible := False;
    Element[tdeExpandedInfo].Visible := False;
    Height := Height - ExpandHeightRequired;
  end;
end;

function TLCLTaskDialog.FindButtonByButtonID(ID: Integer): TCustomButton;
var
  i: Integer;
  Btn: TCustomButton;
begin
  Result := nil;
  for i := 0 to ComponentCount -1 do
  begin
    if (Components[i] is TCustomButton) then
    begin
      Btn := TCustomButton(Components[i]);
      if (Btn.ModalResult = ID) then
      begin
        Result := Btn;
        Break;
      end;
    end;
  end;
end;

function TLCLTaskDialog.FindRadioButtonByButtonID(ID: Integer): TRadioButton;
var
  i: Integer;
begin
  Result := nil;
  for i := Low(RadioButtonArray) to High(RadioButtonArray) do
  begin
    if (RadioButtonArray[i].Tag = ID) then
    begin
      Result := RadioButtonArray[i];
      Break;
    end;
  end;
end;

procedure TLCLTaskDialog.DoDialogConstructed;
begin
  {$PUSH}
  {$ObjectChecks OFF}
  {%H-}TTaskDialogAccess(FDlg).InternalSetDialogHandle(Handle);
  {%H-}TTaskDialogAccess(FDlg).DoOnDialogConstructed;
  {$POP}
end;

procedure TLCLTaskDialog.DoDialogCreated;
begin
  {$PUSH}
  {$ObjectChecks OFF}
  {%H-}TTaskDialogAccess(FDlg).DoOnDialogCreated;
  {$POP}
end;

procedure TLCLTaskDialog.DoDialogDestroyed;
begin
  {$PUSH}
  {$ObjectChecks OFF}
  {%H-}TTaskDialogAccess(FDlg).DoOnDialogDestroyed;
  {$POP}
end;

procedure TLCLTaskDialog.OnVerifyClicked(Sender: TObject);
begin
  {$PUSH}
  {$ObjectChecks OFF}
  {%H-}TTaskDialogAccess(FDlg).DoOnverificationClicked(VerifyCheckBox.Checked);
  {$POP}
end;


procedure TLCLTaskDialog.OnButtonClicked(Sender: TObject);
var Btn: TButton absolute Sender;
    CanClose: Boolean;
begin
  CanClose := True;
  {$PUSH}
  {$ObjectChecks OFF}
  {%H-}TTaskDialogAccess(FDlg).DoOnButtonClicked(FDlg.ButtonIDToModalResult(Btn.ModalResult), CanClose);
  {$POP}
  if not CanClose then
    ModalResult := mrNone
end;


procedure TLCLTaskDialog.SetupControls;
var
  aRadioDef, aButtonDef: TModalResult;
  B: TTaskDialogBaseButtonItem;
  ButtonID: Integer;
  ARadioOffset, FontHeight, aWidth, ALeft {Left for controls aligned right to the icon, so on top 2 panels},
  ATop, i, XB: integer;
  CurrParent: TWinControl;
  aDialogIcon: TLCLTaskDialogIcon;
  CommandLink: TBitBtn;
  aHint: String;
  List: TStringListUTF8Fast;
  Btn: TTaskDialogCommonButton;
  CustomButtonsTextLength: Integer;
begin
  DisableAutoSizing;
  try
    GetDefaultButtons(aButtonDef, aRadioDef);

    CustomButtonsTextLength := 0;
    for B in FDlg.Buttons do
      CustomButtonsTextLength := CustomButtonsTextLength + Length(B.Caption);

    InitCaptions;
    FVerifyChecked := (tfVerificationFlagChecked in FDlg.Flags);

    CommonButtons := FDlg.CommonButtons;
    if (CommonButtons=[]) and (FDlg.Buttons.Count=0) then
    begin
      CommonButtons := [tcbOk];
      if (aButtonDef = 0) then
        aButtonDef := mrOk;
    end;

    InitGlobalDimensionsAndStyle(CustomButtonsTextLength, aWidth, FontHeight);

    Caption := DialogCaption;

    AddPanels;
    CurrParent := TopPanel;

    // handle main dialog icon
    GlobalLeftMargin := GetGlobalLeftMargin;
    AddMainIcon(ALeft, ATop, GlobalLeftMargin, CurrParent);
    //debugln('SetupControls');
    //debugln(['  GlobalLeftMargin=',GlobalLeftMargin]);
    //debugln(['  ALeft=',ALeft]);
    //debugln(['  ATop=',ATop]);

    // add main texts (DlgTitle, DlgText, Information)
    Element[tdeMainInstruction] := AddLabel(DlgTitle, True, ALeft, ATop, FontHeight, aWidth, CurrParent);
    Element[tdeContent] := AddLabel(DlgText, False, ALeft, ATop, FontHeight, aWidth, CurrParent);
    if (ExpandedText <> '')  and not (tfExpandFooterArea in FDlg.Flags) then
    begin
      Element[tdeExpandedInfo] := AddLabel(ExpandedText, False, ALeft, ATop,  FontHeight, aWidth, CurrParent, not (tfExpandedByDefault in Fdlg.Flags));
      ExpandHeightRequired := Element[tdeExpandedInfo].Height + LabelVSPacing;
      //debugln(['ExpandHeightRequired=',ExpandHeightRequired]);
    end;


    TopPanel.Height := ATop;
    CurrParent := MidPanel;
    ATop := 0;
    //Add ProgressBar
    if ([tfShowProgressBar,tfShowMarqueeProgressBar] * FDlg.Flags <> []) then
      AddProgressBar(ALeft, ATop, AWidth, CurrParent);

    // add radio CustomButtons
    if (FDlg.RadioButtons.Count > 0) then
    begin
      ARadioOffset := 1;
      AddRadios(ARadioOffSet, aWidth, aRadioDef, FontHeight, ALeft, ATop, CurrParent);
    end;

    // add command links CustomButtons
    if (tfUseCommandLinks in FDlg.Flags) and (FDlg.Buttons.Count<>0) then
      AddCommandLinkButtons(ALeft, ATop, aWidth, aButtonDef, FontHeight, CurrParent);


    // add query combobox list or QueryEdit
    if (tfQuery in FDlg.Flags) and (FDlg.QueryChoices.Count > 0) then
      AddQueryCombo(ALeft, ATop, aWidth, CurrParent)
    else
    begin
      if (tfSimpleQuery in FDlg.Flags) and (FDlg.SimpleQuery <> '') then
        AddQueryEdit(ALeft, ATop, aWidth, CurrParent);
    end;

    MidPanel.Height := ATop;
    if MidPanel.ControlCount = 0 then
      MidPanel.Visible := False;

    CurrParent := BottomPanel;
    ATop := 0;

    XB := 0;
    //ALeft := GlobalLeftMargin; //Left most margin of the form
    // add CustomButtons and verification checkbox
    if (CommonButtons <> []) or
       ((FDlg.Buttons.Count<>0) and not (tfUseCommandLinks in FDlg.Flags)) then
    begin
      AddButtons(GlobalLeftMargin, ATop, XB, aWidth, aButtonDef, CurrParent);
    end;

    //Add Expand button
    if (ExpandedText <> '') then
      AddExpandButton(GlobalLeftMargin, ATop, XB, aWidth, CurrParent);
    FExpanded := (ExpandedText <> '') and (tfExpandedByDefault in FDlg.Flags);

    if (VerificationText <> '') then
      AddCheckBox(GlobalLeftMargin, ATop, XB, aWidth, CurrParent);
    inc(ATop,36);



    // add FooterText text with optional icon
    if (FooterText <> '') then
    begin
      ALeft := GlobalLeftMargin;
      AddFooter(ALeft, ATop, FontHeight, aWidth, CurrParent);
    end;

    if (ExpandedText <> '') and (tfExpandFooterArea in FDlg.Flags) then
    begin
      ExpandedTextBevel := AddBevel(ATop, aWidth, CurrParent, not FExpanded);
      Inc(ATop, LabelVSpacing div 2);
      Element[tdeExpandedInfo] := AddLabel(ExpandedText, False, ALeft, ATop,  FontHeight, aWidth, CurrParent, not FExpanded);

      ExpandHeightRequired := Element[tdeExpandedInfo].Height + BevelHeight + (LabelVSPacing {div 2});
      //debugln(['ExpandHeightRequired=',ExpandHeightRequired]);
      //if not FExpanded then
      Dec(ATop, LabelVSpacing div 2);
    end;

    ClientHeight := TopPanel.Height + MidPanel.Height + ATop;

    if (tfCallBackTimer in FDlg.Flags) then
      SetupTimer;

    //AddButtons (which comes after adding query) may have set ActiveControl
    //so do this here and not in AddQueryCombo or AddQueryEdit
    if Assigned(QueryCombo) and (tfQueryFocused in FDlg.Flags) then
      ActiveControl := QueryCombo
    else
      if Assigned(QueryEdit) and (tfQueryFocused in FDlg.Flags) then
        ActiveControl := QueryEdit;

  finally
    EnableAutoSizing;
  end;
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
  if (Key = VK_F1) and (Shift = []) then
  begin
    Key := 0;
    DoOnHelp;
  end;
  inherited KeyDown(Key, Shift);
end;

{ ------------- message handling ------------}

procedure TLCLTaskDialog.SetProgressBarType(var Msg: TLMessage);
begin
  // wParam: TRUE turns on marquee mode, FALSE turns off marquee mode.
  // lParam: Must be zero.
  // The return value is ignored
  LazLoggerBase.debugln(['TLCLTaskDialog.SetProgressBarType']);
  LazLoggerBase.debugln(['  Msg.wParam=',Msg.wParam]);
  LazLoggerBase.debugln(['  Msg.lParam=',Msg.lParam]);
  //if both tfShowMarqueeProgressBar and tfShowProgressBar are set, user can switch ProgressBar.Style
  if Assigned(ProgressBar) and (Msg.lParam = 0) then
  begin
    if BOOL(Msg.wParam) then
    begin
      if (tfShowMarqueeProgressBar in FDlg.Flags) then
      begin
        LazLoggerBase.Debugln('TLCLTaskDialog.SetProgressBarType: set pbstMarquee');
        ProgressBar.Style := pbstMarquee;
      end;
    end
    else
    begin
      if (tfShowProgressBar in FDlg.Flags) then
      begin
        LazLoggerBase.Debugln('TLCLTaskDialog.SetProgressBarType: set pbstNormal');
        ProgressBar.Style := pbstNormal;
      end;
    end;
  end;
end;

procedure TLCLTaskDialog.SetProgressBarRange(var Msg: TLMessage);
var
  OldMin, OldMax: Integer;
begin
  // wParam: must be zero
  // lParam: The LOWORD specifies the minimum value. The HIWORD specifies the maximum value.
  // Returns the previous range if sucessfull, zero otherwise
  LazLoggerBase.debugln(['TLCLTaskDialog.SetProgressBarRange']);
  LazLoggerBase.debugln(['  Msg.wParam=',Msg.wParam]);
  LazLoggerBase.debugln(['  Msg.lParam LoWord=',LParamLoWord(Msg.lParam)]);
  LazLoggerBase.debugln(['  Msg.lParam HiWord=',LParamHiWord(Msg.lParam)]);
  if Assigned(ProgressBar) and (Msg.wParam = 0) then
  begin
    OldMin := ProgressBar.Min;
    OldMax := ProgressBar.Max;
    ProgressBar.Min := LParamLoWord(Msg.lParam);
    ProgressBar.Max := LParamHiWord(Msg.lParam);
    Msg.Result := MAKELPARAM(OldMin,OldMax);
  end
  else
    Msg.Result := 0;
end;

procedure TLCLTaskDialog.SetProgressBarPos(var Msg: TLMessage);
var
  OldPos: Integer;
begin
  // wParam: An int that specifies the new position.
  // lParam: Must be zero
  // Returns the previous position
  LazLoggerBase.debugln(['TLCLTaskDialog.SetProgressBarPos']);
  LazLoggerBase.debugln(['  Msg.wParam=',(Msg.wParam)]);
  LazLoggerBase.debugln(['  Msg.lParam=',(Msg.lParam)]);
  if Assigned(ProgressBar) and (Msg.lParam = 0) then
  begin
    OldPos := ProgressBar.Position;
    ProgressBar.Position := Msg.wParam;
    Msg.Result := OldPos;
  end
  else
    Msg.Result := 0;
end;

procedure TLCLTaskDialog.ClickVerification(var Msg: TLMessage);
begin
  // wParam: TRUE to set the state of the checkbox to be checked; FALSE to set it to be unchecked.
  // lParam  TRUE to set the keyboard focus to the checkbox; FALSE otherwise.
  // Return value is ignored
  LazLoggerBase.debugln('TLCLTaskDialog.ClickVerification');
  LazLoggerBase.debugln(['  Msg.wParam=',(Msg.wParam)]);
  LazLoggerBase.debugln(['  Msg.lParam=',(Msg.lParam)]);
  if Assigned(VerifyCheckBox) then
  begin
    VerifyCheckBox.Checked := BOOL(Msg.wParam);
    if BOOL(Msg.lParam) then
      if VerifyCheckBox.CanSetFocus then
        VerifyCheckBox.SetFocus;
  end;
end;

procedure TLCLTaskDialog.ClickButton(var Msg: TLMessage);
var
  i: Integer;
  Btn: TCustomButton;
  BitBtn: TBitBtn;
begin
  // wParam: An int value that specifies the ID of the button to be clicked.
  // lParam: Must be zero.
  // Return value is ignored
  LazLoggerBase.debugln('TLCLTaskDialog.ClickButton');
  LazLoggerBase.debugln(['  Msg.wParam=',(Msg.wParam)]);
  LazLoggerBase.debugln(['  Msg.lParam=',(Msg.lParam)]);

  if (Msg.lPARAM = 0) then
  begin
    Btn := FindButtonByButtonID(Msg.wParam);
    if Assigned(Btn) and Btn.Enabled then
      Btn.Click;
  end;
end;

procedure TLCLTaskDialog.ClickRadioButton(var Msg: TLMessage);
var
  i: Integer;
  RadioBtn: TRadioButton;
begin
  // wParam: An int value that specifies the ID of the radio button to be clicked.
  // lParam: Must be zero.
  // Return value is ignored
  LazLoggerBase.debugln('TLCLTaskDialog.ClickRadioButton');
  LazLoggerBase.debugln(['  Msg.wParam=',(Msg.wParam)]);
  LazLoggerBase.debugln(['  Msg.lParam=',(Msg.lParam)]);
  if (Msg.lParam = 0) and (FDlg.RadioButtons.Count > 0) then
  begin
    RadioBtn := FindRadioButtonByButtonID(Msg.wParam);
    if Assigned(RadioBtn) and (RadioBtn.Enabled) then
      RadioBtn.Checked := True;
  end;
end;

procedure TLCLTaskDialog.EnableButton(var Msg: TLMessage);
var
  Btn: TCustomButton;
begin
  // wParam: An int value that specifies the ID of the button to be enabled/disabled.
  // lParam: 0: disable the button, otherwise enable
  // Return value is ignored
  LazLoggerBase.debugln('TLCLTaskDialog.EnableButton');
  LazLoggerBase.debugln(['  Msg.wParam=',(Msg.wParam)]);
  LazLoggerBase.debugln(['  Msg.lParam=',(Msg.lParam)]);
  Btn := FindButtonByButtonID(Msg.wParam);
  if Assigned(Btn) then
    Btn.Enabled := (Msg.lParam <> 0);
end;

procedure TLCLTaskDialog.EnableRadioButton(var Msg: TLMessage);
var
  RadioBtn: TRadioButton;
begin
  // wParam: An int value that specifies the ID of the radio button to be enabled/disabled.
  // lParam: 0: disable the button, otherwise enable
  // Return value is ignored
  LazLoggerBase.debugln('TLCLTaskDialog.EnableRadioButton');
  LazLoggerBase.debugln(['  Msg.wParam=',(Msg.wParam)]); //ID of radiobutton
  LazLoggerBase.debugln(['  Msg.lParam=',(Msg.lParam)]); //must be 0
  RadioBtn := FindRadioButtonByButtonID(Msg.wParam);
  if Assigned(RadioBtn) then
    RadioBtn.Enabled := (Msg.lParam <> 0);
end;

procedure TLCLTaskDialog.UpdateElementText(var Msg: TLMessage);
var
  NewText: String;
  Lbl: TLabel;
  //ARect: TRect;
begin
  // wParam: Indicates the element to update.
  // lParam: Pointer to a Unicode string that contains the new text.
  // Return value is ignored.
  LazLoggerBase.debugln('TLCLTaskDialog.UpdateElementText');
  LazLoggerBase.debugln(['  Msg.wParam=',(Msg.wParam)]);
  LazLoggerBase.debugln(['  Msg.lParam=',(Msg.lParam)]);
  NewText := Utf16ToUtf8(Unicodestring(PWideChar(Msg.lParam)));
  LazLoggerBase.debugln('  NewText=',NewText);
  //It seems to be that the native Vista+ dialog does not adjust heights (properly),
  //if new label height is bigger, then e.g. FooterText may end up falling off the dialog...
  // ToDo: do this properly:
  // - calculate new BoundsRect, adjust height of panles and dialog and reserved height for expanding
  if (Msg.wParam in [TDE_CONTENT..TDE_MAIN_INSTRUCTION]) then
  begin
    case Msg.wParam of
      TDE_CONTENT: //FDlg.Text;
      begin
        Lbl := Element[tdeContent];
        //ARect := Lbl.BoundsRect;
        if Assigned(lbl) then Lbl.Caption := NewText;
      end;
      TDE_EXPANDED_INFORMATION: //FDlg.ExpandedText
      begin
        Lbl := Element[tdeExpandedInfo];
        if Assigned(lbl) then Lbl.Caption := NewText;
      end;
      TDE_FOOTER: //FDlg.FooterText
      begin
        Lbl := Element[tdeFooter];
        if Assigned(lbl) then Lbl.Caption := NewText;
      end;
      TDE_MAIN_INSTRUCTION: //FDlg.Title
      begin
        Lbl := Element[tdeMainInstruction];
        if Assigned(lbl) then Lbl.Caption := NewText;
      end;
    end;
  end;
end;

{ ------------- end message handling ------------}

finalization
  if assigned(LDefaultFont) then
    LDefaultFont.Free;


end.
