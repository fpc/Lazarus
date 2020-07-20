unit gtk3boxes;

interface

uses
  // LazUtils
  UITypes,
  // LCL
  LCLType, LCLStrConsts,LCLProc, InterfaceBase,
  LazGtk3, LazGLib2, LazGObject2, LazGdk3, gtk3objects;


type
  TBtnListfunction=function(ndx:integer):longint of object;

  { TGtk3DialogFactory }

  TGtk3DialogFactory = class
    btn_def: PGtkButton;
    DefaultNdx: Integer;
    fButtons: TDialogButtons;
    pButtons: PLongint;
    fCaption: string;
    fDialogType:longint;
    Dialog: PGtkDialog;
    DialogResult: Integer;
    constructor CreateAsk(const DialogCaption, DialogMessage: string;
       DialogType: LongInt; Buttons: TDialogButtons; HelpCtx: Longint);
    constructor CreatePrompt(const DialogCaption, DialogMessage: string;
       DialogType: LongInt; Buttons: PLongInt;
       ButtonCount: LongInt; DefaultIndex: LongInt; EscapeResult: LongInt);
    constructor CreateMsgBox(hWnd: HWND; lpText, lpCaption: PChar;
       uType: Cardinal);
    constructor CreateMsgBox1(hWnd: HWND; lpText, lpCaption: PChar;
       uType: Cardinal);
    class function tr(UseWidgetStr: boolean; const TranslatedStr, WidgetStr: String): string;
    destructor Destroy;override;
    procedure run;
    function btn_coll_info(ndx:integer):longint;
    function btn_ptr_info(ndx:integer):longint;
    procedure update_widget_list(const func:TBtnListFunction);
    procedure CreateButton(const ALabel : String; const AResponse: Integer;
      const AImageHint: Integer = -1);
    function lcl_result:integer;
    function btn_result:integer;
    class function ResponseID(const AnID: Integer): Integer;
    class function gtk_resp_to_lcl(const gtk_resp:integer):integer;
    class function gtk_resp_to_btn(const gtk_resp:integer):integer;
    class function MessageType(ADialogType:longint):TGtkMessageType;
  end;


implementation

// fake GTK button responses
const
  GTK_RESPONSE_LCL_ALL = -10;
  GTK_RESPONSE_LCL_YESTOALL = -3; // GTK_RESPONSE_ACCEPT;
  GTK_RESPONSE_LCL_RETRY = -12;
  GTK_RESPONSE_LCL_IGNORE = -13;
  GTK_RESPONSE_LCL_NOTOALL = -14;


{ callbacks }
function BoxClosed(Widget : PGtkWidget; {%H-}Event : PGdkEvent;
  data: gPointer) : GBoolean; cdecl;
var
  ModalResult : PtrUInt;
begin
  { We were requested by window manager to close so return EscapeResult}
  if PInteger(data)^ = 0 then
  begin
    ModalResult:= {%H-}PtrUInt(g_object_get_data(PGObject(Widget), 'modal_result'));
    { Don't allow to close if we don't have a default return value }
    Result:= (ModalResult = 0);
    if not Result then PInteger(data)^:= ModalResult
    else DebugLn('Do not close !!!');
  end else Result:= false;
end;

function ButtonClicked(Widget : PGtkWidget; data: gPointer) : GBoolean; cdecl;
begin
  PInteger(data)^ := {%H-}PtrUInt(g_object_get_data(PGObject(Widget), 'modal_result'));
  Result := False;
end;



class function TGtk3DialogFactory.ResponseID(const AnID: Integer): Integer;
begin
  case AnID of
    idButtonOK       : Result := GTK_RESPONSE_OK;
    idButtonCancel   : Result := GTK_RESPONSE_CANCEL;
    idButtonHelp     : Result := GTK_RESPONSE_HELP;
    idButtonYes      : Result := GTK_RESPONSE_YES;
    idButtonNo       : Result := GTK_RESPONSE_NO;
    idButtonClose    : Result := GTK_RESPONSE_CLOSE;
    idButtonAbort    : Result := GTK_RESPONSE_REJECT;
    idButtonRetry    : Result := GTK_RESPONSE_LCL_RETRY;
    idButtonIgnore   : Result := GTK_RESPONSE_LCL_IGNORE;
    idButtonAll      : Result := GTK_RESPONSE_LCL_ALL;
    idButtonNoToAll  : Result := GTK_RESPONSE_LCL_NOTOALL;
    idButtonYesToAll : Result := GTK_RESPONSE_LCL_YESTOALL;
  else
    Result:=AnID;
  end;
end;


class function TGtk3DialogFactory.gtk_resp_to_lcl(const gtk_resp:integer):integer;
begin
  case gtk_resp of
  -5{GTK_RESPONSE_OK}: Result:=  ID_OK;
  -6{GTK_RESPONSE_CANCEL}:  Result := id_Cancel;
  -7{GTK_RESPONSE_CLOSE}: Result:=id_Close;
  -8{GTK_RESPONSE_YES}: Result:=id_Yes;
  -9{GTK_RESPONSE_NO}: Result:=id_No;

  -1{GTK_RESPONSE_NONE}: Result:=0;
  -2{GTK_RESPONSE_REJECT}: Result:=id_abort;
  -3{GTK_RESPONSE_ACCEPT}: Result:=id_Yes;

  GTK_RESPONSE_LCL_RETRY: Result:=id_Retry;
  GTK_RESPONSE_LCL_IGNORE:  Result:=id_Ignore;
  GTK_RESPONSE_LCL_ALL:  Result:=idButtonAll;
  GTK_RESPONSE_LCL_NOTOALL: Result:=idButtonNoToAll;
  //GTK_RESPONSE_LCL_YESTOALL: Result:= idButtonYesToAll;
  else
    Result:=gtk_resp;
  end;
end;

class function TGtk3DialogFactory.gtk_resp_to_btn(const gtk_resp:integer):integer;
begin
  case gtk_resp of
  -5{GTK_RESPONSE_OK}: Result:=  idButtonOk;
  -6{GTK_RESPONSE_CANCEL}:  Result := idButtonCancel;
  -7{GTK_RESPONSE_CLOSE}: Result:=idButtonClose;
  -8{GTK_RESPONSE_YES}: Result:=idButtonYes;
  -9{GTK_RESPONSE_NO}: Result:=idButtonNo;

  -1{GTK_RESPONSE_NONE}: Result:=0;
  -2{GTK_RESPONSE_REJECT}: Result:=idButtonAbort;
  -3{GTK_RESPONSE_ACCEPT}: Result:=idButtonYesToAll;

  GTK_RESPONSE_LCL_RETRY: Result:=idButtonRetry;
  GTK_RESPONSE_LCL_IGNORE:  Result:=idButtonIgnore;
  GTK_RESPONSE_LCL_ALL:  Result:=idButtonAll;
  GTK_RESPONSE_LCL_NOTOALL: Result:=idButtonNoToAll;
  //GTK_RESPONSE_LCL_YESTOALL: Result:= idButtonYesToAll;
  else
    Result:=gtk_resp;
  end;
end;


procedure TGtk3DialogFactory.CreateButton(
    const ALabel : String;
    const AResponse: Integer;
    const AImageHint: Integer = -1);
var
  NewButton: PGtkWidget;
  //BitmapHandle, MaskHandle: HBitmap;
  // GDIObject: PGDIObject;
  //Pixbuf: PGdkPixbuf;
  // Mask: PGdkBitmap;
  //Img: PGtkWidget;
begin

  NewButton := gtk_dialog_add_button(Dialog,
    PgChar(ReplaceAmpersandsWithUnderscores(ALabel)), AResponse);
  gtk_button_set_use_underline(PGtkButton(NewButton), True);
  g_object_set_data(PGObject(NewButton), 'modal_result',
        {%H-}Pointer(PtrInt(AResponse)));
  (*
  if AImageHint >= 0 then
  begin
    if ThemeServices.GetStockImage(AImageHint, BitmapHandle, MaskHandle) then
    begin
      GDIObject := {%H-}PGDIObject(BitmapHandle);
      Mask := nil;
      Pixbuf := nil;
      if GDIObject^.GDIBitmapType = gbPixbuf then
        Pixbuf := GDIObject^.GDIPixbufObject
      else
        Mask := CreateGdkMaskBitmap(BitmapHandle, MaskHandle);

      Img := gtk_image_new;

      if Pixbuf <> nil then
        gtk_image_set_from_pixbuf(PGtkImage(Img), Pixbuf)
      else
        gtk_image_set_from_pixmap(PGtkImage(Img), GDIObject^.GDIPixmapObject.Image, Mask);
      gtk_button_set_image(PGtkButton(NewButton), Img);
      if Mask <> nil then
        g_object_unref(Mask);
      DeleteObject(BitmapHandle);
      DeleteObject(MaskHandle);
    end;
  end;
  *)
end;

function TGtk3DialogFactory.lcl_result: integer;
begin
  Result:=gtk_resp_to_lcl(DialogResult);
end;

function TGtk3DialogFactory.btn_result: integer;
begin
  Result:=gtk_resp_to_btn(DialogResult);
end;

class function TGtk3DialogFactory.MessageType(ADialogType:longint):TGtkMessageType;
begin
  case ADialogType of
    idDialogWarning: Result := GTK_MESSAGE_WARNING;
    idDialogError: Result := GTK_MESSAGE_ERROR;
    idDialogInfo : Result := GTK_MESSAGE_INFO;
    idDialogConfirm : Result := GTK_MESSAGE_QUESTION;
  else
    Result := GTK_MESSAGE_INFO;
  end;
end;


const
  ButtonResults : array[mrNone..mrYesToAll] of Longint = (
    -1, idButtonOK, idButtonCancel, idButtonAbort, idButtonRetry,
    idButtonIgnore, idButtonYes,idButtonNo, idButtonAll, idButtonNoToAll,
    idButtonYesToAll);

constructor TGtk3DialogFactory.CreateAsk(const DialogCaption,
       DialogMessage: string; DialogType: LongInt;
       Buttons: TDialogButtons; HelpCtx: Longint);
var
  GtkDialogType: TGtkMessageType;
  Btns: TGtkButtonsType;
  i, BtnIdx, BtnID: Integer;
  dbtn:TDialogButton;
  Title: String;
  BtnResult: LongInt;
begin
  DialogResult := mrNone;
  fDialogType := DialogType;
  GtkDialogType := MessageType(fDialogType); // map LCLINTF -> GTK
  fButtons:=Buttons;
  fCaption:=DialogCaption;

  Btns := GTK_BUTTONS_NONE;
  DefaultNdx := 0;
  for i := 0 to Buttons.Count - 1 do
  begin
    if Buttons[i].Default then
      DefaultNdx := i;

    if (DialogResult = mrNone) and
      (Buttons[i].ModalResult in [mrCancel, mrAbort, mrIgnore, mrNo, mrNoToAll])
    then
      DialogResult := Buttons[i].ModalResult;
  end;

  Dialog := gtk_message_dialog_new(nil, GTK_DIALOG_MODAL, GtkDialogType, Btns, nil , []);

  gtk_message_dialog_set_markup(PGtkMessageDialog(Dialog), PGChar(DialogMessage));

  g_signal_connect_data(Dialog, 'delete-event',
    TGCallback(@BoxClosed),
    @DialogResult, nil, 0);

  if Btns = GTK_BUTTONS_NONE then
  begin
    // gtk3 have reverted buttons eg. No, Yes
    for BtnIdx := Buttons.Count - 1 downto 0 do
    begin
      dbtn:=Buttons[BtnIdx];
      if (dbtn.ModalResult >= Low(ButtonResults)) and (dbtn.ModalResult <= High(ButtonResults)) then
      begin
        BtnID := ButtonResults[dbtn.ModalResult];
        case BtnID of
          idButtonOK       : CreateButton(dbtn.Caption, GTK_RESPONSE_OK, BtnID);
          idButtonCancel   : CreateButton(dbtn.Caption, GTK_RESPONSE_CANCEL, BtnID);
          idButtonHelp     : CreateButton(dbtn.Caption, GTK_RESPONSE_HELP, BtnID);
          idButtonYes      : CreateButton(dbtn.Caption, GTK_RESPONSE_YES, BtnID);
          idButtonNo       : CreateButton(dbtn.Caption, GTK_RESPONSE_NO, BtnID);
          idButtonClose    : CreateButton(dbtn.Caption, GTK_RESPONSE_CLOSE, BtnID);
          idButtonAbort    : CreateButton(dbtn.Caption, GTK_RESPONSE_REJECT, BtnID);
          idButtonRetry    : CreateButton(dbtn.Caption, GTK_RESPONSE_LCL_RETRY, BtnID);
          idButtonIgnore   : CreateButton(dbtn.Caption, GTK_RESPONSE_LCL_IGNORE, BtnID);
          idButtonAll      : CreateButton(dbtn.Caption, GTK_RESPONSE_LCL_ALL, BtnID);
          idButtonNoToAll  : CreateButton(dbtn.Caption, GTK_RESPONSE_LCL_NOTOALL, BtnID);
          idButtonYesToAll : CreateButton(dbtn.Caption, GTK_RESPONSE_LCL_YESTOALL, BtnID);
        end;
      end else
         CreateButton(dbtn.Caption, dbtn.ModalResult, 0);

    end;
  end;

  update_widget_list(@btn_coll_info);

end;


function TGtk3DialogFactory.btn_coll_info(ndx:integer):longint;
begin
  Result:=fButtons[ndx].ModalResult; // get modal result for button
end;

function TGtk3DialogFactory.btn_ptr_info(ndx:integer):longint;
begin
  Result:=pButtons[ndx];
end;

procedure TGtk3DialogFactory.update_widget_list(const func:TBtnListFunction);
var
  btn:PgtkButton;
  BtnIdx,BtnID,BtnRes:integer;
  MainList,ChildList: PGList;
begin
  MainList := gtk_container_get_children(PGtkContainer(Dialog^.get_action_area));
  ChildList:=g_list_last(MainList);
  BtnIdx:=0;
  btn_def:=nil;
  while Assigned(ChildList) do
  begin
    if (ChildList^.Data <> nil) then
    begin
      if g_type_check_instance_is_a(ChildList^.Data, gtk_button_get_type) then
      begin
        Btn := PGtkButton(ChildList^.Data);

        BtnRes:=func(BtnIdx); // process button

        if (BtnRes>=Low(ButtonResults)) and (BtnRes<=High(ButtonResults)) then
          BtnID := ButtonResults[BtnRes]
        else
          BtnID := BtnRes;

        if BtnID = idButtonCancel then
           g_object_set_data(PGObject(Dialog), 'modal_result', Pointer(idButtonCancel));

        g_signal_connect_data(Btn, 'clicked',
          TGCallback(@ButtonClicked), @DialogResult, nil, 0);

        if DefaultNdx = BtnIdx then
        begin
          gtk_dialog_set_default_response(Dialog, ResponseID(BtnID));
          btn_def:=btn;
          gtk_widget_grab_default(btn);
          gtk_widget_grab_focus(btn);
          gtk_widget_set_can_default(btn_def,TRUE);
          gtk_window_set_default(dialog, btn_def);
          g_object_set_data(PGObject(Dialog), 'modal_result',
            {%H-}Pointer(PtrInt(BtnID)));
        end;

        inc(BtnIdx);
      end;
    end;
    ChildList:=ChildList^.prev;
  end;
  if MainList <> nil then
    g_list_free(MainList);

end;

procedure TGtk3DialogFactory.run;
var
  Title:string;
begin
  if not Assigned(Dialog) then exit;

  if fCaption <> '' then
    Title:=fCaption
  else
  begin
    Title := '';
    case fDialogType of
      idDialogWarning: Title := rsMtWarning;
      idDialogError: Title := rsMtError;
      idDialogInfo : Title := rsMtInformation;
      idDialogConfirm : Title := rsMtConfirmation;
    end;
  end;

  gtk_window_set_title(PGtkWindow(Dialog), PGChar(Title));
  gtk_dialog_run(Dialog);
end;

class function TGtk3DialogFactory.tr(UseWidgetStr: boolean; const TranslatedStr, WidgetStr: String): string;
begin
  if UseWidgetStr then
    Result:=WidgetStr
  else
    Result:=TranslatedStr;
end;

destructor TGtk3DialogFactory.Destroy;
begin
  if Assigned(Dialog) then
  gtk_widget_destroy(Dialog);
end;


constructor TGtk3DialogFactory.CreatePrompt(const DialogCaption,
  DialogMessage: string; DialogType: LongInt; Buttons: PLongInt;
  ButtonCount: LongInt; DefaultIndex: LongInt; EscapeResult: LongInt);
var
  x,i:integer;
  GtkDialogType: TGtkMessageType;
  Btns: TGtkButtonsType;
  DefaultID: Integer;
begin
  DialogResult := EscapeResult;
  fDialogType := DialogType;
  GtkDialogType := MessageType(fDialogType); // map LCLINTF -> GTK
  pButtons:=Buttons;
  fCaption:=DialogCaption;

  Btns := GTK_BUTTONS_NONE;
  DefaultId := 0;
  for X := 0 to ButtonCount - 1 do
  begin
    if X = DefaultIndex then
      DefaultID := Buttons[X];
  end;

  Dialog := gtk_message_dialog_new(nil, GTK_DIALOG_MODAL, GtkDialogType, Btns, nil , []);

  gtk_message_dialog_set_markup(PGtkMessageDialog(Dialog), PGChar(DialogMessage));

  g_signal_connect_data(GPointer(Dialog), 'delete-event',
    TGCallback(@BoxClosed),
    @DialogResult, nil, 0);

  if Btns = GTK_BUTTONS_NONE then
  begin
    for i := ButtonCount-1 downto 0 do
    begin
      case Buttons[i] of
        idButtonOK       : CreateButton(tr(rsmbOK='&OK',rsmbOK, 'gtk-ok'), GTK_RESPONSE_OK);
        idButtonCancel   : CreateButton(tr(rsmbCancel='Cancel',rsmbCancel,'gtk-cancel'), GTK_RESPONSE_CANCEL);
        idButtonHelp     : CreateButton(tr(rsmbHelp='&Help',rsmbHelp,'gtk-help'), GTK_RESPONSE_HELP);
        idButtonYes      : CreateButton(tr(rsmbYes='&Yes',rsmbYes,'gtk-yes'), GTK_RESPONSE_YES);
        idButtonNo       : CreateButton(tr(rsmbNo='&No',rsmbNo,'gtk-no'), GTK_RESPONSE_NO);
        idButtonClose    : CreateButton(tr(rsmbClose='&Close',rsmbClose,'gtk-close'), GTK_RESPONSE_CLOSE);
        idButtonAbort    : CreateButton(rsMBAbort, GTK_RESPONSE_REJECT);
        idButtonRetry    : CreateButton(rsMBRetry, GTK_RESPONSE_LCL_RETRY);
        idButtonIgnore   : CreateButton(rsMBIgnore, GTK_RESPONSE_LCL_IGNORE);
        idButtonAll      : CreateButton(rsMbAll, GTK_RESPONSE_LCL_ALL);
        idButtonNoToAll  : CreateButton(rsMBNoToAll, GTK_RESPONSE_LCL_NOTOALL);
        idButtonYesToAll : CreateButton(rsMBYesToAll, GTK_RESPONSE_LCL_YESTOALL);
      end;
    end;
  end;
  update_widget_list(@btn_ptr_info);
end;


constructor TGtk3DialogFactory.CreateMsgBox1(hWnd: HWND; lpText, lpCaption: PChar;
  uType: Cardinal);
var
  ALabel : PGtkWidget;
  ButtonCount, DefButton : Integer;
(*  procedure CreateButton(const ALabel : PChar; const RetValue : integer);
  var AButton : PGtkWidget;
  begin
    AButton:= gtk_button_new_with_label(ALabel);
    Inc(ButtonCount);
    if ButtonCount = DefButton then begin
      gtk_window_set_focus(PGtkWindow(Dialog), AButton);
    end;
    { If there is the Cancel button, allow the dialog to close }
    if RetValue = IDCANCEL then begin
      g_object_set_data(PGObject(Dialog), 'modal_result', Pointer(IDCANCEL));
    end;
    g_object_set_data(AButton, 'modal_result',
                        {%H-}Pointer(PtrInt(RetValue)));
    g_signal_connect_data(AButton, 'clicked',
                     TGCallback(@MessageButtonClicked), GPointer(@ADialogResult), nil, 0);
    gtk_container_add (PGtkContainer(PGtkDialog(Dialog)^.get_action_area), AButton);
  end;*)

begin
  ButtonCount:= 0;

  fCaption:=lpCaption;
  { Determine which is the default button }
  DefButton:= ((uType and $00000300) shr 8) + 1;
  //DebugLn('Trace:Default button is ' + IntToStr(DefButton));

  DialogResult:= 0;
  Dialog := gtk_dialog_new;
  g_signal_connect_data(Dialog, 'delete-event', TGCallback(@BoxClosed), @DialogResult, nil, 0);
  gtk_window_set_default_size(PGtkWindow(Dialog), 100, 100);
  ALabel:= gtk_label_new(lpText);
  gtk_container_add (PGtkContainer(PGtkDialog(Dialog)^.get_content_area), ALabel);
  fDialogType:= (uType and $0000000F);
  if fDialogType = MB_OKCANCEL
  then begin
    CreateButton(PChar(rsMbOK), IDOK);
    CreateButton(PChar(rsMbCancel), IDCANCEL);
  end
  else begin
    if fDialogType = MB_ABORTRETRYIGNORE
    then begin
      CreateButton(PChar(rsMbAbort), IDABORT);
      CreateButton(PChar(rsMbRetry), IDRETRY);
      CreateButton(PChar(rsMbIgnore), IDIGNORE);
    end
    else begin
      if fDialogType = MB_YESNOCANCEL
      then begin
        CreateButton(PChar(rsMbYes), IDYES);
        CreateButton(PChar(rsMbNo), IDNO);
        CreateButton(PChar(rsMbCancel), IDCANCEL);
      end
      else begin
        if fDialogType = MB_YESNO
        then begin
          CreateButton(PChar(rsMbYes), IDYES);
          CreateButton(PChar(rsMbNo), IDNO);
        end
        else begin
          if fDialogType = MB_RETRYCANCEL
          then begin
            CreateButton(PChar(rsMbRetry), IDRETRY);
            CreateButton(PChar(rsMbCancel), IDCANCEL);
          end
          else begin
            { We have no buttons to show. Create the default of OK button }
            CreateButton(PChar(rsMbOK), IDOK);
          end;
        end;
      end;
    end;
  end;
  gtk_window_set_title(PGtkWindow(Dialog), lpCaption);
  gtk_window_set_position(PGtkWindow(Dialog), GTK_WIN_POS_CENTER);
  gtk_window_set_modal(PGtkWindow(Dialog), true);
  gtk_widget_show_all(Dialog);
end;

constructor TGtk3DialogFactory.CreateMsgBox(hWnd: HWND; lpText,
  lpCaption: PChar; uType: Cardinal);
var
  AType,AButtons,DefIndex:integer;
  btns:array of integer;

  procedure AddBtn(btn_res:integer);
  begin
    setlength(btns,length(btns)+1);
    btns[high(btns)]:=btn_res;
  end;

begin
  // icons
  case uType and $000000F0 of
//  MB_ICONEXCLAMATION:
  MB_ICONWARNING: AType:=idDialogWarning;
  //MB_ICONASTERISK:
  MB_ICONINFORMATION: AType:=idDialogInfo;
  MB_ICONQUESTION: Atype:=idDialogConfirm;
  // MB_ICONSTOP:
  // MB_ICONHAND:
  MB_ICONERROR: Atype:=idDialogError;
  end;
  // default button
  DefIndex:=(uType and $00000F00) shr 8;

  // buttons requested
  AButtons:= (uType and $0000000F);
  if AButtons = MB_OKCANCEL
  then begin
    AddBtn(idButtonOk);
    AddBtn(idButtonCancel);
  end
  else begin
    if AButtons = MB_ABORTRETRYIGNORE
    then begin
      AddBtn(idButtonAbort);
      AddBtn(idButtonRetry);
      AddBtn(idButtonIgnore);
    end
    else begin
      if AButtons = MB_YESNOCANCEL
      then begin
        AddBtn(idButtonYes);
        AddBtn(idButtonNo);
        AddBtn(idButtonCancel);
      end
      else begin
        if AButtons = MB_YESNO
        then begin
          AddBtn(idButtonYes);
          AddBtn(idButtonNo);
        end
        else begin
          if AButtons = MB_RETRYCANCEL
          then begin
            AddBtn(idButtonRetry);
            AddBtn(idButtonCancel);
          end
          else begin
            { We have no buttons to show. Create the default of OK button }
            AddBtn(idButtonOK);
          end;
        end;
      end;
    end;
  end;
  Self.CreatePrompt(lpCaption,lpText,AType,@btns[0],length(btns),DefIndex,0);
end;


end.

