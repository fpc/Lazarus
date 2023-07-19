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
    FRadioRes: Integer;
    FVerifyCheck: Boolean;
    FVerifyChecked: Boolean;
    Rad: array of TRadioButton;

  protected
    procedure HandleEmulatedButtonClicked(Sender: TObject);
    procedure SetupControls;
  public
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;

    function Execute: Boolean;
  public
    /// the labels corresponding to the Task Dialog main elements
    Element: array[tdeContent..tdeMainInstruction] of TLabel;
    /// the Task Dialog selection list
    Combo: TComboBox;
    /// the Task Dialog optional query editor
    Edit: TEdit;
    /// the Task Dialog optional checkbox
    Verif: TCheckBox;

    property VerifyChecked: Boolean read FVerifyCheck write FVerifyChecked;
    property RadioRes: Integer read FRadioRes;
  end;


type
  TTaskDialogTranslate = function(const aString: string): string;
var
  TaskDialog_Translate: TTaskDialogTranslate;


function ExecuteLCLTaskDialog(const ADlg: TTaskDialog): Boolean;

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

type
  TLCLTaskDialogIcon = (
    tiBlank, tiWarning, tiQuestion, tiError, tiInformation, tiNotUsed, tiShield);
  TLCLTaskDialogFooterIcon = (
    tfiBlank, tfiWarning, tfiQuestion, tfiError, tfiInformation, tfiShield);

const
  LCL_IMAGES: array[TLCLTaskDialogIcon] of Integer = (
    0, idDialogWarning, idDialogConfirm, idDialogError, idDialogInfo, 0, idDialogShield);
  LCL_FOOTERIMAGES: array[TLCLTaskDialogFooterIcon] of Integer = (
    0, idDialogWarning, idDialogConfirm, idDialogError, idDialogInfo, idDialogShield);

const
  FirstButtonIndex = 100;
  FirstRadioButtonIndex = 200;
  TD_BTNMOD: array[TTaskDialogCommonButton] of Integer = (
    mrOk, mrYes, mrNo, mrCancel, mrRetry, mrAbort);


type
  /// internal type used for Unicode string storage
  WS = WideString;

function _WS(const aString: string): WS;
begin
  Result := UTF8Decode(aString);
end;

function CR(const aText: string): string;
begin
  if pos('\n', aText) = 0 then
    result := aText else
    result := StringReplace(aText, '\n', #10, [rfReplaceAll]);
end;


function TD_BTNS(button: TTaskDialogCommonButton): pointer;
begin
  case button of
    tcbOK:     result := @rsMbOK;
    tcbYes:    result := @rsMbYes;
    tcbNo:     result := @rsMbNo;
    tcbCancel: result := @rsMbCancel;
    tcbRetry:  result := @rsMbRetry;
    tcbClose:  result := @rsMbClose;
  end;
end;






{ -------------- }
function ExecuteLCLTaskDialog(const ADlg: TTaskDialog): Boolean;
var
  DlgForm: TLCLTaskDialog;
begin
  debugln('ExecuteLCLTaskDialog');
  Result := False;
  DlgForm := TLCLTaskDialog.CreateNew(ADlg);
  try
    Result := DlgForm.Execute;
  finally
    FreeAndNil(DlgForm);
  end;
end;

constructor TLCLTaskDialog.CreateNew(AOwner: TComponent; Num: Integer);
begin
  debugln('TLCLTaskDialog.CreateNew: AOwner=',DbgSName(AOwner));
  inherited CreateNew(AOwner, Num);
  if (AOwner is TCustomTaskDialog) then
    FDlg := TTaskDialog(AOwner);
  Rad := nil;
  KeyPreview := True;
end;

function TLCLTaskDialog.Execute: Boolean;
var
  mRes, I: Integer;
  aParent: HWND;
begin
  debugln(['TLCLTaskDialog.Execute: Assigned(FDlg)=',Assigned(FDlg)]);
  if not Assigned(FDlg) then
    Exit(False);
  SetupControls;

  aParent := FDlg.ParentWindow;
  //set form parent
  if (aParent <> 0) then
    for I := 0 to Screen.CustomFormCount-1 do
      if Screen.CustomForms[I].Handle = aParent then
      begin
        PopupParent := Screen.CustomForms[I];
        Break;
      end;
  if not Assigned(PopupParent) then
    PopupParent := Screen.ActiveCustomForm;
  if Assigned(PopupParent) then
    PopupMode := pmExplicit;



  mRes := ShowModal;
  Result := (mRes > 0);

//ToDo implement this
(*
  if Combo<>nil then begin
    SelectionRes := Combo.ItemIndex;
    Query := Dialog.Form.Combo.Text;
  end
  else
  if Dialog.Form.Edit<>nil then
    Query := Dialog.Form.Edit.Text;

*)

  if Verif<>nil then
    FVerifyChecked := Verif.Checked;
  FRadioRes := 0;
  for i := 0 to high(Rad) do
    if Rad[i].Checked then
      FRadioRes := i+FirstRadioButtonIndex;

end;

procedure TLCLTaskDialog.HandleEmulatedButtonClicked(Sender: TObject);
var Btn: TButton absolute Sender;
    CanClose: Boolean;
begin
  if Assigned(FDlg) and Assigned(FDlg.OnButtonClicked) then begin
    CanClose := true;
    FDlg.{Dialog.}OnButtonClicked(FDlg,Btn.ModalResult,CanClose);
    if not CanClose then
      ModalResult := mrNone;
  end;
end;



//const
//  LCL_IMAGES: array[TTaskDialogIcon] of Integer = (
//    0, idDialogWarning, idDialogConfirm, idDialogError, idDialogInfo, 0, idDialogShield);
  //LCL_FOOTERIMAGES: array[TTaskDialogFooterIcon] of Integer = (
  //  0, idDialogWarning, idDialogConfirm, idDialogError, idDialogInfo, idDialogShield);


function TD_Trans(const aString: string): string;
begin
  if Assigned(TaskDialog_Translate) then
    Result := TaskDialog_Translate(aString)
  else
    Result := aString;
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


function IconMessage(Icon: TLCLTaskDialogIcon): string;
begin
  case Icon of
    tiWarning:   result := rsMtWarning;
    tiQuestion:  result := rsMtConfirmation;
    tiError:     result := rsMtError;
    tiInformation, tiShield: result := rsMtInformation;
    else result := '';
  end;
  result := TD_Trans(result);
end;


procedure TLCLTaskDialog.SetupControls;
var
  //TaskDlg: LCLTaskDialog.TTaskDialog;
  aRadioDef, aButtonDef: TModalResult;
  B: TTaskDialogBaseButtonItem;
  ButtonID: Integer;
  Buttons, Radios, Title, Inst, Content,
  TaskDlgInfoCollapse, Info, Footer,
  Verify, Selection: TTranslateString;
  ARadioOffset, FontHeight, aWidth, IconBorder, X, Y, i, XB: integer;
  aCommonButtons: TTaskDialogCommonButtons;
  Panel: TPanel;
  Par: TWinControl;
  aDialogIcon: TLCLTaskDialogIcon;
  Image: TImage;
  CommandLink: TBitBtn;
  aHint: String;
  List: TStringListUTF8Fast;
  CurrTabOrder: TTabOrder;
  Btn: TTaskDialogCommonButton;


  function AddLabel(Text: string; BigFont: boolean): TLabel;
  var R: TRect;
      W: integer;
  begin
    if Text = '' then
      exit(nil);

    result := TLabel.Create(Self);
    result.Parent := Par;
    result.WordWrap := true;
    if BigFont then
    begin
      if (tfEmulateClassicStyle in FDlg.Flags) then
      begin
        result.Font.Height := FontHeight-2;
        result.Font.Style := [fsBold]
      end
      else
      begin
        result.Font.Height := FontHeight-4;
        result.Font.Color := clHighlight;
      end;
    end
    else
      result.Font.Height := FontHeight;
    result.AutoSize := false;
    R.Left := 0;
    R.Top := 0;
    W := aWidth-X-8;
    R.Right := W;
    R.Bottom := result.Height;
    LCLIntf.DrawText(result.Canvas.Handle,PChar(Text),Length(Text),R,DT_CALCRECT or DT_WORDBREAK);//lazarus does not return box height on OSX (Lazarus bug), the height is stored in the rect in all cases, so we don't need to use the result

    result.SetBounds(X,Y,W,R.Bottom);
    result.Caption := Text;
    inc(Y,R.Bottom+16);
  end;


  function NoCR(const aText: string): string;
  var i: integer;
  begin
    result := aText;
    aHint := '';
    i := pos('\n',result);
    if i>0 then begin
      aHint := CR(copy(result,i+2,maxInt));
      SetLength(result,i-1);
    end;
  end;

  function AddButton(const s: string; ModalResult: integer): TButton;
  var
    WB: integer;
  begin
    WB := Canvas.TextWidth(s)+52;
    dec(XB,WB);
    if XB<X shr 1 then begin
      XB := aWidth-WB;
      inc(Y,32);
    end;
    result := TButton.Create(Self);
    result.Parent := Par;
      if (tfEmulateClassicStyle in FDlg.Flags) then
        result.SetBounds(XB,Y,WB-10,22)
      else
        result.SetBounds(XB,Y,WB-12,28);
    result.Caption := s;
    result.ModalResult := ModalResult;
    result.TabOrder := CurrTabOrder;
    result.OnClick := @HandleEmulatedButtonClicked;
    case ModalResult of
      mrOk: begin
        result.Default := true;
        if aCommonButtons=[tcbOk] then
          result.Cancel := true;
      end;
      mrCancel: result.Cancel := true;
    end;
    if ModalResult=aButtonDef then
      ActiveControl := result;
  end;


  procedure AddBevel;
  var
    BX: integer;
  begin
    with TBevel.Create(Self) do begin
      Parent := Par;
      if (Image<>nil) and (Y<Image.Top+Image.Height) then
        BX := X else
        BX := 2;
      SetBounds(BX,Y,aWidth-BX-2,2);
    end;
    inc(Y,16);
  end;


begin

  if FDlg.RadioButtons.DefaultButton<> nil then
    aRadioDef := FDlg.RadioButtons.DefaultButton.Index
  else
    aRadioDef := 0;
  if FDlg.Buttons.DefaultButton<>nil then
    aButtonDef := FDlg.Buttons.DefaultButton.ModalResult
  else
    aButtonDef := TD_BTNMOD[FDlg.DefaultButton];

  Buttons := '';
  for B in FDlg.Buttons do
    Buttons := Buttons + B.Caption + #10;
  Radios := '';
  for B in FDlg.RadioButtons do
    Radios := Radios + B.Caption + #10;

  //ToDo
  //This field/parameter is currently not used in Dialogs.TTaskDialog and not passed so we cannot initialize it properly yet
  Selection := '';

  Title := FDlg.Caption;
  Inst := FDlg.Title;
  Content := FDlg.Text;
  TaskDlgInfoCollapse := FDlg.ExpandButtonCaption;
  Info := FDlg.ExpandedText;
  Footer := FDlg.FooterText;
  Verify := FDlg.VerificationText;
  FVeriFyChecked := (tfVerificationFlagChecked in FDlg.Flags);

  aCommonButtons := FDlg.CommonButtons;

  if (aCommonButtons=[]) and (Buttons='') then
  begin
    aCommonButtons := [tcbOk];
    if (aButtonDef = 0) then
      aButtonDef := mrOk;
  end;

  if (Title = '') then
    if (Application.MainForm = nil) then
      Title := Application.Title else
      Title := Application.MainForm.Caption;
  //
  if (Inst = '') then
    Inst := IconMessage(TF_DIALOGICON(FDlg.MainIcon));

  //Dialog.OnButtonClicked := aOnButtonClicked;


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

  aWidth := FDlg.FWidth;
  if (aWidth <= 0) then
  begin
    aWidth := Canvas.TextWidth(Inst);
    if (aWidth > 300) or (Canvas.TextWidth(Content) > 300) or
       (Length(Buttons) > 40) then
      aWidth := 480 else
      aWidth := 420;
  end
  else
    if (aWidth < 120) then aWidth := 120;
  ClientWidth := aWidth;

  Height := FirstRadioButtonIndex;
  Caption := Title;

  // create a white panel for the main dialog part
  Panel := TPanel.Create(Self);
  Panel.Parent := Self;
  Panel.Align := alTop;
  Panel.BorderStyle := bsNone;
  Panel.BevelOuter := bvNone;
  if not (tfEmulateClassicStyle in FDlg.Flags) then begin
    Panel.Color := clWindow;
  end;
  Par := Panel;

  // handle main dialog icon
  if (tfEmulateClassicStyle in FDlg.Flags) then
    IconBorder := 10
  else
    IconBorder := 24;

  aDialogIcon := TF_DIALOGICON(FDlg.MainIcon);
  if (LCL_IMAGES[aDialogIcon]<>0) then
  begin
    Image := TImage.Create(Self);
    Image.Parent := Par;
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
  end else
  begin
    Image := nil;
    if (not (tfEmulateClassicStyle in FDlg.Flags)) and (Inst <> '') then
      IconBorder := IconBorder*2;
    X := IconBorder;
    Y := IconBorder;
  end;

  // add main texts (Instruction, Content, Information)
  Element[tdeMainInstruction] := AddLabel(Inst, true);
  Element[tdeContent] := AddLabel(Content, false);
  if (Info <> '') then
    // no information collapse/expand yet: it's always expanded
    Element[tdeExpandedInfo] := AddLabel(Info, false);


  // add radio buttons
  if Radios<>'' then
  begin
    {$IFDEF MSWINDOWS}
    if WidgetSet.GetLCLCapability(lcNativeTaskDialog) = LCL_CAPABILITY_NO then
      ARadioOffset := 1
    else
      ARadioOffset := 0;
    {$ELSE}
    ARadioOffset := 1;
    {$ENDIF}
    with TStringList.Create do
    try
      Text := SysUtils.trim(Radios);
      SetLength(Rad,Count);
      for i := 0 to Count-1 do begin
        Rad[i] := TRadioButton.Create(Self);
        with Rad[i] do begin
          Parent := Par;
          AutoSize := False;
          SetBounds(X+16,Y,aWidth-32-X, (6-FontHeight) + ARadioOffset);
          Caption := NoCR(Strings[i]);
          if aHint<>'' then begin
            ShowHint := true;
            Hint := aHint; // note shown as Hint
          end;
          inc(Y,Height + ARadioOffset);
          if not (tfNoDefaultRadioButton in FDlg.Flags) and ((i=0) or (i=aRadioDef)) then
            Checked := true;
        end;
      end;
      inc(Y,24);
    finally
      Free;
    end;
  end;



  // add command links buttons
  if (tfUseCommandLinks in FDlg.Flags) and (Buttons<>'') then
    with TStringList.Create do
    try
      inc(Y,8);
      Text := SysUtils.trim(Buttons);
      for i := 0 to Count-1 do begin
        CommandLink := TBitBtn.Create(Self);
        with CommandLink do begin
          Parent := Par;
          Font.Height := FontHeight-3;
          if (tfEmulateClassicStyle in FDlg.Flags) then
            SetBounds(X,Y,aWidth-10-X,40) else
            SetBounds(X,Y,aWidth-16-X,40);
          Caption := NoCR(Strings[i]);
          if aHint<>'' then begin
            ShowHint := true;
            Hint := aHint; // note shown as Hint
          end;
          inc(Y,Height+2);
          ModalResult := i+FirstButtonIndex;
          OnClick := @HandleEmulatedButtonClicked;
          if ModalResult=aButtonDef then
            ActiveControl := CommandLink;
          if (tfEmulateClassicStyle in FDlg.Flags) then begin
            Font.Height := FontHeight - 2;
            Font.Style := [fsBold]
          end;
          if (tfEmulateClassicStyle in FDlg.Flags) then begin
            Margin := 7;
            Spacing := 7;
          end else begin
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
    finally
      Free;
    end;


(*

  This needs expanding of TTaskDialogFlags and a new field Content in TTaskDialog.
  Basically this code was never excuted from Dialogs.TTaskDialog

  // add selection list or query editor
  if (Selection <> '') then
  begin
    List := TStringListUTF8Fast.Create;
    try
      Combo := TComboBox.Create(self);
      with Combo do begin
        Parent := Par;
        SetBounds(X,Y,aWidth-32-X,22);
        if (tfQuery in FDlg.Flags) then
          Style := csDropDown
        else
          Style := csDropDownList;
        List.Text := trim(Selection);
        Items.Assign(List);
        ItemIndex := List.IndexOf(Query);
        if (ItemIndex=-1) and (Style=csDropDown) then
          Text := Query;
        if tdfQueryFieldFocused in aFlags then
          Dialog.Form.ActiveControl := Dialog.Form.Combo;
      end;
      inc(Y,42);
    finally
      List.Free;
    end;
  end
  else
    if tfQuery in aFlags then
    begin
      Dialog.Form.Edit := TEdit.Create(Dialog.Form);
      with Dialog.Form.Edit do
      begin
        Parent := Par;
        SetBounds(X,Y,aWidth-16-X,22);
        Text := Query;
        if tdfQueryMasked in aFlags then
          PasswordChar := '*';
      end;
      if tdfQueryFieldFocused in aFlags then
        ActiveControl := Form.Edit;
      inc(Y,42);
    end;

*)


  // from now we won't add components to the white panel, but to the form
  Panel.Height := Y;
  Par := Self;



  // add buttons and verification checkbox
  if (aCommonButtons <> []) or (Verify<>'') or
     ((Buttons<>'') and not (tfUseCommandLinks in FDlg.Flags)) then
  begin
    CurrTabOrder := Panel.TabOrder;
    inc(Y, 16);
    XB := aWidth;
    if not (tfUseCommandLinks in FDlg.Flags) then
      with TStringList.Create do
      try
        Text := SysUtils.trim(Buttons);
        for i := Count-1 downto 0 do
          AddButton(Strings[i],i+FirstButtonIndex);
      finally
        Free;
      end;
    for Btn := high(TTaskDialogCommonButton) downto low(TTaskDialogCommonButton) do
    begin
      if (Btn in aCommonButtons) then
        AddButton(TD_Trans(LoadResString(TD_BTNS(Btn))), TD_BTNMOD[Btn]);
    end;
    if Verify<>'' then
    begin
      Verif := TCheckBox.Create(Self);
      with Verif do
      begin
        Parent := Par;
        if X+16+Canvas.TextWidth(Verify)>XB then begin
          inc(Y,32);
          XB := aWidth;
        end;
        SetBounds(X,Y,XB-X,24);
        Caption := Verify;
        Checked := FVerifyChecked;
      end;
    end;
    inc(Y,36);
  end
  else
    XB := 0;



  // add footer text with optional icon
  if (Footer <> '') then
  begin
    if XB<>0 then
      AddBevel
    else
      inc(Y,16);
    if (LCL_FOOTERIMAGES[TF_FOOTERICON(FDlg.FooterIcon)]<>0) then
    begin
      Image := TImage.Create(Self);
      Image.Parent := Par;
      Image.Images := DialogGlyphs;
      Image.ImageWidth := 16;
      Image.ImageIndex := DialogGlyphs.DialogIcon[LCL_FOOTERIMAGES[TF_FOOTERICON(FDlg.FooterIcon)]];
      Image.Stretch := True;
      Image.StretchOutEnabled := False;
      Image.Proportional := True;
      Image.Center := True;
      Image.SetBounds(24,Y,16,16);
      X := 40+Image.Width;
    end else
    begin
      X := 24;
    end;
    Element[tdeFooter] := AddLabel(Footer, false);
  end;


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

