{$ASSERTIONS ON}

Function CS_To_String(CompStyle: Integer): String;
Begin
 Case CompStyle of
  csAlignment      : Result := 'csAlignment';
  csButton         : Result := 'csButton';
  csCanvas         : Result := 'csCanvas';
  csCheckbox       : Result := 'csCheckbox';
  csComboBox       : Result := 'csComboBox';
  csListBox        : Result := 'csListBox';
  csEdit           : Result := 'csEdit';
  csFont           : Result := 'csFont';
  csForm           : Result := 'csForm';
  csMainForm       : Result := 'csMainForm';
  csFrame          : Result := 'csFrame';
  csMemo           : Result := 'csMemo';
  csMenuBar        : Result := 'csMenuBar';
  csMenuItem       : Result := 'csMenuItem';
  csNotebook       : Result := 'csNotebook';
  csFileDialog     : Result := 'csFileDialog';
  csColorDialog    : Result := 'csColorDialog';
  csRadioButton    : Result := 'csRadioButton';
  csScrolledWindow : Result := 'csScrolledWindow';
  csSpinEdit       : Result := 'csSpinEdit';
  csSTATUSBAR      : Result := 'csSTATUSBAR';
  csgtkTable       : Result := 'csgtkTable';
  csToggleBox      : Result := 'csToggleBox';
  csGroupBox       : Result := 'csGroupBox';
  csPage           : Result := 'csPage';
 end; {Case}
End;

Function LM_ToString(LM_Message: Integer): String;
Begin
 Case LM_Message - $1000 of // Workaround: LCL message numbers have changed (+$1000) since this was writted; TODO: Use constants to improve future compatibility (or simply remove this function altogether)
  -3011 : Result := 'LM_SCREENINIT';
      1 : Result := 'LM_Create';
      2 : Result := 'LM_SetLabel';
      3 : Result := 'LM_SetLeft';
      4 : Result := 'LM_SetTop';
      5 : Result := 'LM_SetWidth';
      6 : Result := 'LM_SetHeight';
      7 : Result := 'LM_AddChild';
      8 : Result := 'LM_Setsize';
      9 : Result := 'LM_GetLabel';
     10 : Result := 'LM_AssignEvent';
     11 : Result := 'LM_AssignSelf';
     12 : Result := 'LM_SetName';
     13 : Result := 'LM_RESIZECHILDREN';
     14 : Result := 'LM_ShowHide';
     15 : Result := 'LM_AddPAge';
     16 : Result := 'LM_GetLineCount';
     17 : Result := 'LM_SETTEXT';
     18 : Result := 'LM_GETTEXT';
     19 : Result := 'LM_CANVASCREATE';
     20 : Result := 'LM_DrawFIllRect';
     21 : Result := 'LM_DrawRect';
     22 : Result := 'LM_DrawLine';
     23 : Result := 'LM_DrawText';
     24 : Result := 'LM_FontSetSize';
     25 : Result := 'LM_FontGetSize';
     26 : Result := 'LM_ReDraw';
     27 : Result := 'LM_SetColor';
     28 : Result := 'LM_RemovePage';
     29 : Result := 'LM_ShowTabs';
     30 : Result := 'LM_SetTabPosition';
   $100 : Result := 'LM_USER';
   $101 : Result := 'LM_RESIZE';
   $102 : Result := 'LM_DESTROY';
   $103 : Result := 'LM_ACTIVATE';
   $104 : Result := 'LM_ACTIVATEITEM';
   $105 : Result := 'LM_CHANGED';
   $106 : Result := 'LM_FOCUS';
   $107 : Result := 'LM_CLICKED';
   $108 : Result := 'LM_PRESSED';
   $109 : Result := 'LM_RELEASED';
   $10A : Result := 'LM_MOVECURSOR';
   $10B : Result := 'LM_ENTER';
   $10C : Result := 'LM_LEAVE';
   $10D : Result := 'LM_SIZEALLOCATE';
   $10E : Result := 'LM_CHECKRESIZE';
   $10F : Result := 'LM_SHOW';
   $110 : Result := 'LM_INSERTTEXT';
   $111 : Result := 'LM_DELETETEXT';
   $112 : Result := 'LM_SETEDITABLE';
   $113 : Result := 'LM_MOVEWORD';
   $114 : Result := 'LM_MOVEPAGE';
   $115 : Result := 'LM_MOVETOROW';
   $116 : Result := 'LM_MOVETOCOLUMN';
   $117 : Result := 'LM_KILLCHAR';
   $118 : Result := 'LM_KILLWORD';
   $119 : Result := 'LM_KILLLINE';
   $11A : Result := 'LM_CUTTOCLIP';
   $11B : Result := 'LM_COPYTOCLIP';
   $11C : Result := 'LM_PASTEFROMCLIP';
   $11D : Result := 'LM_MOVERESIZE';
   $11E : Result := 'LM_EXPOSEEVENT';
   $11F : Result := 'LM_CONFIGUREEVENT';
   $120 : Result := 'LM_DRAW';
   $121 : Result := 'LM_SHOWMODAL';
   $122 : Result := 'LM_SETFILTER';
   $123 : Result := 'LM_SETFILENAME';
   $124 : Result := 'LM_OK_CLICKED';
   $125 : Result := 'LM_CANCEL_CLICKED';
   $126 : Result := 'LM_KEYDOWN';
   $127 : Result := 'LM_KEYUP';
   $128 : Result := 'LM_TIMER';
   $129 : Result := 'LM_MOUSEBTNPRESS';
   $12A : Result := 'LM_MOUSEBTNRELEASE';
   $12B : Result := 'LM_GETITEMS';
   $12C : Result := 'LM_GETITEMINDEX';
   $12D : Result := 'LM_SETITEMINDEX';
   $12E : Result := 'LM_SETITEMINDEX';
   $12F : Result := 'LM_SETSELTEXT';
   $130 : Result := 'LM_GETSELSTART';
   $131 : Result := 'LM_SETSELSTART';
   $132 : Result := 'LM_GETSELLEN';
   $133 : Result := 'LM_SETSELLEN';
   $134 : Result := 'LM_MOUSEWHEEL';
   $135 : Result := 'LM_GETLIMITTEXT';
   $136 : Result := 'LM_SETLIMITTEXT';
   $137 : Result := 'LM_SORT';
   $138 : Result := 'LM_GETSELCOUNT';
   $139 : Result := 'LM_GETSEL';
   $13A : Result := 'LM_SETSEL';
   $13B : Result := 'LM_SETSELMODE';
   $163 : Result := 'LM_UNKNOWN';
 Else
   Result := 'Unknown LM_Message = $' + IntToHex(LM_Message, 4);
 End; {Case}
End;

Function WM_ToString(WM_Message: Integer): String;
Begin
 Case WM_Message of
  $0000 : Result := 'WM_NULL';
  $0001 : Result := 'WM_CREATE';
  $0002 : Result := 'WM_DESTROY';
  $0003 : Result := 'WM_MOVE';
  $0005 : Result := 'WM_SIZE';
  $0006 : Result := 'WM_ACTIVATE';
  $0007 : Result := 'WM_SETFOCUS';
  $0008 : Result := 'WM_KILLFOCUS';
  $000A : Result := 'WM_ENABLE';
  $000B : Result := 'WM_SETREDRAW';
  $000C : Result := 'WM_SETTEXT';
  $000D : Result := 'WM_GETTEXT';
  $000E : Result := 'WM_GETTEXTLENGTH';
  $000F : Result := 'WM_PAINT';
  $0010 : Result := 'WM_CLOSE';
  $0011 : Result := 'WM_QUERYENDSESSION';
  $0012 : Result := 'WM_QUIT';
  $0013 : Result := 'WM_QUERYOPEN';
  $0014 : Result := 'WM_ERASEBKGND';
  $0015 : Result := 'WM_SYSCOLORCHANGE';
  $0016 : Result := 'WM_ENDSESSION';
  $0017 : Result := 'WM_SYSTEMERROR';
  $0018 : Result := 'WM_SHOWWINDOW';
  $0019 : Result := 'WM_CTLCOLOR';
  $001A : Result := 'WM_WININICHANGE or WM_SETTINGCHANGE';
  $001B : Result := 'WM_DEVMODECHANGE';
  $001C : Result := 'WM_ACTIVATEAPP';
  $001D : Result := 'WM_FONTCHANGE';
  $001E : Result := 'WM_TIMECHANGE';
  $001F : Result := 'WM_CANCELMODE';
  $0020 : Result := 'WM_SETCURSOR';
  $0021 : Result := 'WM_MOUSEACTIVATE';
  $0022 : Result := 'WM_CHILDACTIVATE';
  $0023 : Result := 'WM_QUEUESYNC';
  $0024 : Result := 'WM_GETMINMAXINFO';
  $0026 : Result := 'WM_PAINTICON';
  $0027 : Result := 'WM_ICONERASEBKGND';
  $0028 : Result := 'WM_NEXTDLGCTL';
  $002A : Result := 'WM_SPOOLERSTATUS';
  $002B : Result := 'WM_DRAWITEM';
  $002C : Result := 'WM_MEASUREITEM';
  $002D : Result := 'WM_DELETEITEM';
  $002E : Result := 'WM_VKEYTOITEM';
  $002F : Result := 'WM_CHARTOITEM';
  $0030 : Result := 'WM_SETFONT';
  $0031 : Result := 'WM_GETFONT';
  $0032 : Result := 'WM_SETHOTKEY';
  $0033 : Result := 'WM_GETHOTKEY';
  $0037 : Result := 'WM_QUERYDRAGICON';
  $0039 : Result := 'WM_COMPAREITEM';
  $003D : Result := 'WM_GETOBJECT';
  $0041 : Result := 'WM_COMPACTING';
  $0044 : Result := 'WM_COMMNOTIFY { obsolete in Win32}';
  $0046 : Result := 'WM_WINDOWPOSCHANGING';
  $0047 : Result := 'WM_WINDOWPOSCHANGED';
  $0048 : Result := 'WM_POWER';
  $004A : Result := 'WM_COPYDATA';
  $004B : Result := 'WM_CANCELJOURNAL';
  $004E : Result := 'WM_NOTIFY';
  $0050 : Result := 'WM_INPUTLANGCHANGEREQUEST';
  $0051 : Result := 'WM_INPUTLANGCHANGE';
  $0052 : Result := 'WM_TCARD';
  $0053 : Result := 'WM_HELP';
  $0054 : Result := 'WM_USERCHANGED';
  $0055 : Result := 'WM_NOTIFYFORMAT';
  $007B : Result := 'WM_CONTEXTMENU';
  $007C : Result := 'WM_STYLECHANGING';
  $007D : Result := 'WM_STYLECHANGED';
  $007E : Result := 'WM_DISPLAYCHANGE';
  $007F : Result := 'WM_GETICON';
  $0080 : Result := 'WM_SETICON';
  $0081 : Result := 'WM_NCCREATE';
  $0082 : Result := 'WM_NCDESTROY';
  $0083 : Result := 'WM_NCCALCSIZE';
  $0084 : Result := 'WM_NCHITTEST';
  $0085 : Result := 'WM_NCPAINT';
  $0086 : Result := 'WM_NCACTIVATE';
  $0087 : Result := 'WM_GETDLGCODE';
  $00A0 : Result := 'WM_NCMOUSEMOVE';
  $00A1 : Result := 'WM_NCLBUTTONDOWN';
  $00A2 : Result := 'WM_NCLBUTTONUP';
  $00A3 : Result := 'WM_NCLBUTTONDBLCLK';
  $00A4 : Result := 'WM_NCRBUTTONDOWN';
  $00A5 : Result := 'WM_NCRBUTTONUP';
  $00A6 : Result := 'WM_NCRBUTTONDBLCLK';
  $00A7 : Result := 'WM_NCMBUTTONDOWN';
  $00A8 : Result := 'WM_NCMBUTTONUP';
  $00A9 : Result := 'WM_NCMBUTTONDBLCLK';
  $0100 : Result := 'WM_KEYFIRST or WM_KEYDOWN';
  $0101 : Result := 'WM_KEYUP';
  $0102 : Result := 'WM_CHAR';
  $0103 : Result := 'WM_DEADCHAR';
  $0104 : Result := 'WM_SYSKEYDOWN';
  $0105 : Result := 'WM_SYSKEYUP';
  $0106 : Result := 'WM_SYSCHAR';
  $0107 : Result := 'WM_SYSDEADCHAR';
  $0108 : Result := 'WM_KEYLAST';
  $010D : Result := 'WM_IME_STARTCOMPOSITION';
  $010E : Result := 'WM_IME_ENDCOMPOSITION';
  $010F : Result := 'WM_IME_COMPOSITION or WM_IME_KEYLAST';
  $0110 : Result := 'WM_INITDIALOG';
  $0111 : Result := 'WM_COMMAND';
  $0112 : Result := 'WM_SYSCOMMAND';
  $0113 : Result := 'WM_TIMER';
  $0114 : Result := 'WM_HSCROLL';
  $0115 : Result := 'WM_VSCROLL';
  $0116 : Result := 'WM_INITMENU';
  $0117 : Result := 'WM_INITMENUPOPUP';
  $011F : Result := 'WM_MENUSELECT';
  $0120 : Result := 'WM_MENUCHAR';
  $0121 : Result := 'WM_ENTERIDLE';
  $0122 : Result := 'WM_MENURBUTTONUP';
  $0123 : Result := 'WM_MENUDRAG';
  $0124 : Result := 'WM_MENUGETOBJECT';
  $0125 : Result := 'WM_UNINITMENUPOPUP';
  $0126 : Result := 'WM_MENUCOMMAND';
  $0132 : Result := 'WM_CTLCOLORMSGBOX';
  $0133 : Result := 'WM_CTLCOLOREDIT';
  $0134 : Result := 'WM_CTLCOLORLISTBOX';
  $0135 : Result := 'WM_CTLCOLORBTN';
  $0136 : Result := 'WM_CTLCOLORDLG';
  $0137 : Result := 'WM_CTLCOLORSCROLLBAR';
  $0138 : Result := 'WM_CTLCOLORSTATIC';
  $0200 : Result := 'WM_MOUSEFIRST or WM_MOUSEMOVE';
  $0201 : Result := 'WM_LBUTTONDOWN';
  $0202 : Result := 'WM_LBUTTONUP';
  $0203 : Result := 'WM_LBUTTONDBLCLK';
  $0204 : Result := 'WM_RBUTTONDOWN';
  $0205 : Result := 'WM_RBUTTONUP';
  $0206 : Result := 'WM_RBUTTONDBLCLK';
  $0207 : Result := 'WM_MBUTTONDOWN';
  $0208 : Result := 'WM_MBUTTONUP';
  $0209 : Result := 'WM_MBUTTONDBLCLK';
  $020A : Result := 'WM_MOUSEWHEEL or WM_MOUSELAST';
  $0210 : Result := 'WM_PARENTNOTIFY';
  $0211 : Result := 'WM_ENTERMENULOOP';
  $0212 : Result := 'WM_EXITMENULOOP';
  $0213 : Result := 'WM_NEXTMENU';
  $0214 : Result := 'WM_SIZING';
  $0215 : Result := 'WM_CAPTURECHANGED';
  $0216 : Result := 'WM_MOVING';
  $0218 : Result := 'WM_POWERBROADCAST';
  $0219 : Result := 'WM_DEVICECHANGE';
  $0220 : Result := 'WM_MDICREATE';
  $0221 : Result := 'WM_MDIDESTROY';
  $0222 : Result := 'WM_MDIACTIVATE';
  $0223 : Result := 'WM_MDIRESTORE';
  $0224 : Result := 'WM_MDINEXT';
  $0225 : Result := 'WM_MDIMAXIMIZE';
  $0226 : Result := 'WM_MDITILE';
  $0227 : Result := 'WM_MDICASCADE';
  $0228 : Result := 'WM_MDIICONARRANGE';
  $0229 : Result := 'WM_MDIGETACTIVE';
  $0230 : Result := 'WM_MDISETMENU';
  $0231 : Result := 'WM_ENTERSIZEMOVE';
  $0232 : Result := 'WM_EXITSIZEMOVE';
  $0233 : Result := 'WM_DROPFILES';
  $0234 : Result := 'WM_MDIREFRESHMENU';
  $0281 : Result := 'WM_IME_SETCONTEXT';
  $0282 : Result := 'WM_IME_NOTIFY';
  $0283 : Result := 'WM_IME_CONTROL';
  $0284 : Result := 'WM_IME_COMPOSITIONFULL';
  $0285 : Result := 'WM_IME_SELECT';
  $0286 : Result := 'WM_IME_CHAR';
  $0288 : Result := 'WM_IME_REQUEST';
  $0290 : Result := 'WM_IME_KEYDOWN';
  $0291 : Result := 'WM_IME_KEYUP';
  $02A1 : Result := 'WM_MOUSEHOVER';
  $02A3 : Result := 'WM_MOUSELEAVE';
  $0300 : Result := 'WM_CUT';
  $0301 : Result := 'WM_COPY';
  $0302 : Result := 'WM_PASTE';
  $0303 : Result := 'WM_CLEAR';
  $0304 : Result := 'WM_UNDO';
  $0305 : Result := 'WM_RENDERFORMAT';
  $0306 : Result := 'WM_RENDERALLFORMATS';
  $0307 : Result := 'WM_DESTROYCLIPBOARD';
  $0308 : Result := 'WM_DRAWCLIPBOARD';
  $0309 : Result := 'WM_PAINTCLIPBOARD';
  $030A : Result := 'WM_VSCROLLCLIPBOARD';
  $030B : Result := 'WM_SIZECLIPBOARD';
  $030C : Result := 'WM_ASKCBFORMATNAME';
  $030D : Result := 'WM_CHANGECBCHAIN';
  $030E : Result := 'WM_HSCROLLCLIPBOARD';
  $030F : Result := 'WM_QUERYNEWPALETTE';
  $0310 : Result := 'WM_PALETTEISCHANGING';
  $0311 : Result := 'WM_PALETTECHANGED';
  $0312 : Result := 'WM_HOTKEY';
  $0317 : Result := 'WM_PRINT';
  $0318 : Result := 'WM_PRINTCLIENT';
  $0358 : Result := 'WM_HANDHELDFIRST';
  $035F : Result := 'WM_HANDHELDLAST';
  $0380 : Result := 'WM_PENWINFIRST';
  $038F : Result := 'WM_PENWINLAST';
  $0390 : Result := 'WM_COALESCE_FIRST';
  $039F : Result := 'WM_COALESCE_LAST';
  $03E0 : Result := 'WM_DDE_FIRST or WM_DDE_INITIATE';
  $03E1 : Result := 'WM_DDE_TERMINATE';
  $03E2 : Result := 'WM_DDE_ADVISE';
  $03E3 : Result := 'WM_DDE_UNADVISE';
  $03E4 : Result := 'WM_DDE_ACK';
  $03E5 : Result := 'WM_DDE_DATA';
  $03E6 : Result := 'WM_DDE_REQUEST';
  $03E7 : Result := 'WM_DDE_POKE';
  $03E8 : Result := 'WM_DDE_EXECUTE or WM_DDE_LAST';
  $0400 : Result := 'WM_USER';
  $8000 : Result := 'WM_APP';
 Else
  Result := 'Unknown WM_Message = $' + IntToHex(WM_Message, 4);
 End; {Case}
End;

Function PropEnumProps(Window: Hwnd; Str: PChar; Data: Handle): BOOL; StdCall;
Begin
  Assert(False, 'Trace: PropEnumProps - Start');
  Assert(False, Format('Trace: Property %S (with value %S) from window %S removed', [String(Str), IntToHex(Integer(GetProp(Window, Str)), 4), IntToHex(Window, 4)]));
  RemoveProp(Window, Str);
  Result := True;
  Assert(False, 'Trace: PropEnumProps - Exit');
End;

{------------------------------------------------------------------------------}
{       Window Handler                                                           }
{------------------------------------------------------------------------------}
Function WindowProc(Window: HWnd; Mess: UInt; WPar: WParam; LPar: LParam): LResult; Export; StdCall;
Var
  OwnerObject: TObject;
Begin
  Assert(False, 'Trace:WindowProc - Start');
  Result := 0;
  OwnerObject := TObject(GetProp(Window, 'Lazarus'));
  Assert(False, 'Trace:WindowProc - Getting Object With Callback Procedure');
  Assert(False, 'Trace:WindowProc - Checking Proc');
  Assert(False, 'Trace:WindowProc - Value of Mess is ' + WM_ToString(Mess));
  Case Mess of
    WM_Create:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_Create');
      Exit;
    End;
    WM_Destroy:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_Destroy');
      EnumProps(Window, @PropEnumProps);
      PostQuitMessage(0);
      Exit;
    End;
    WM_ShowWindow:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_ShowWindow');
      Exit;
    End;
    WM_NCLButtonDown:
    Begin
      Assert(False, 'Trace:WindowProc - Got WM_NCLButtonDown');
    End;
  End; {Case}
  Result := DefWindowProc(Window, Mess, WPar, LPar);
  Assert(False, 'Trace:WindowProc - Exit');
End;

{------------------------------------------------------------------------------}
{       Register Window Class                                                  }
{------------------------------------------------------------------------------}
Function TWin32Object.WinRegister: Boolean;

Var
 WindowClass : WndClass;

Begin
 Assert(False, 'Trace:WinRegister - Start');
 With WindowClass Do
  Begin
   Style         := cs_hRedraw or cs_vRedraw;
   lpfnWndProc   := WndProc(@WindowProc);
   cbClsExtra    := 1;
   cbWndExtra    := 1;
   hInstance     := System.HInstance;
   hIcon         := LoadIcon(0, idi_Application);
   hCursor       := LoadCursor(0, idc_Arrow);
   hbrBackground := GetStockObject(GRAY_BRUSH);
   lpszMenuName  := nil;
   lpszClassName := ClsName;
  End;
 Result := Windows.RegisterClass(@WindowClass) <> 0;
 Assert(False, 'Trace:WinRegister - Exit');
End;

{------------------------------------------------------------------------------}
{       TWin32Object HandleEvents                                                }
{       *Note: Passes execution control to the GTK engine                      }
{------------------------------------------------------------------------------}
procedure TWin32Object.HandleEvents;
begin
  While GetMessage(@FMessage, HWnd(Nil), 0, 0) Do
  Begin
    If TranslateAccelerator(FParentWindow, FAccelGroup, @FMessage) = 0 Then
    Begin
      TranslateMessage(@FMessage);
      DispatchMessage(@FMessage);
    End;
  End;
end;

const
  BOOL_RESULT: array[Boolean] of String = ('False', 'True');

{------------------------------------------------------------------------------
  Method: TWin32Object.Create
  Params:  None
  Returns: Nothing

  Contructor for the class.
 ------------------------------------------------------------------------------}
constructor TWin32Object.Create;
Var
  AcTbl: Array[1..50] Of ACCEL;
begin
  inherited Create;
  Try
    FKeyStateList := TList.Create;
    Assert(False, 'FKeyStateList initialized');
    FDeviceContexts := TDynHashArray.Create(-1);
    Assert(False, 'FDeviceContexts initialized');
    FGDIObjects := TDynHashArray.Create(-1);
    Assert(False, 'FGDIObjects initialized');
    FMessageQueue := TList.Create;
    Assert(False, 'FMessageQueue initialized');
    Assert(False, 'TODO: [TWin32Object.Create] Create an accelerator group');
    FAccelGroup := CreateAcceleratorTable(LPACCEL(@AcTbl), High(AcTbl));
    Assert(False, 'FAccelGroup initialized');
    FTimerData  := TList.Create;
    Assert(False, 'Created Class Name');
    FromCBProc := False;
    Assert(False, 'FromCBProc negated');
  Except
    Exit;
  End;
end;

destructor TWin32Object.Destroy;
const
  GDITYPENAME: array[TGDIType] of String = ('gdiBitmap', 'gdiBrush'
    ,'gdiFont', 'gdiPen', 'gdiRegion');
var
  n: Integer;
  p: PMsg;
  pTimerInfo  : PWin32ITimerInfo;
  GDITypeCount: array[TGDIType] of Integer;
  GDIType: TGDIType;
  HashItem: PDynHashArrayItem;

begin
  // tidy up the messages
  n:=FMessageQueue.Count-1;
  while (n>=0) do begin
    p := PMsg(FMessageQueue.Items[n]);
    if p^.Message=LM_PAINT then begin
      //writeln('[TgtkObject.Destroy]  freeing unused paint message ',HexStr(p^.WParam,8));
      ReleaseDC(0,P^.WParam);
      Dispose(p);
      FMessageQueue.Delete(n);
    end;
    dec(n);
  end;

  if (FDeviceContexts.Count > 0)
  then begin
    WriteLN(Format('[TgtkObject.Destroy] WARNING: There are %d unreleased DCs, a detailed dump follows:' ,[FDeviceContexts.Count]));
    
    n:=0;
    write('[TgtkObject.Destroy]   DCs:  ');
    HashItem:=FDeviceContexts.FirstHashItem;
    while (n<7) and (HashItem<>nil) do 
    begin
      write(' ',HexStr(Cardinal(HashItem^.Item),8));
      HashItem:=HashItem^.Next;
      inc(n);
    end;
    writeln();
  end;

  if (FGDIObjects.Count > 0)  
  then begin  
    WriteLN(Format('[TgtkObject.Destroy] WARNING: There are %d unreleased GDIObjects, a detailed dump follows:' ,[FGDIObjects.Count]));  
    for GDIType := Low(GDIType) to High(GDIType) do 
    begin
      for GDIType := Low(GDIType) to High(GDIType) do 
        GDITypeCount[GDIType] := 0;
        
      n:=0;
      write('[TgtkObject.Destroy]   GDIOs:');
      HashItem := FGDIObjects.FirstHashItem;
      while (HashItem <> nil) do 
      begin
        if n < 7 
        then write(' ',HexStr(Cardinal(HashItem^.Item),8));
        
        Inc(GDITypeCount[PGdiObject(HashItem^.Item)^.GDIType]);
        HashItem := HashItem^.Next;
        Inc(n);
      end; 
      Writeln();
      
      for GDIType := Low(GDIType) to High(GDIType) do 
        if GDITypeCount[GDIType] > 0 
        then WriteLN(Format('[TgtkObject.Destroy]   %s: %d', [GDITYPENAME[GDIType], GDITypeCount[GDIType]]));
    end;
  end;

  if FMessageQueue.Count > 0 
  then begin
    WriteLN(Format('[TgtkObject.Destroy] WARNING: There are %d messages left in the queue! I''ll free them' ,[FMessageQueue.Count]));
    for n := 0 to FMessageQueue.Count  - 1 do
    begin
      p := PMsg(FMessageQueue.Items[n]);
      Dispose(P);
    end;
  end;

  n := FTimerData.Count;
  if (n > 0) then
  begin
    WriteLN(Format('[TgtkObject.Destroy] WARNING: There are %d TimerInfo structures left, I''ll free them' ,[n]));
    while (n > 0) do
    begin
      dec (n);
      pTimerInfo := PWin32ITimerInfo (FTimerData.Items[n]);
      Dispose (pTimerInfo);
      FTimerData.Delete (n);
    end;
  end;

  FMessageQueue.Free;
  FDeviceContexts.Free;
  FGDIObjects.Free;
  FKeyStateList.Free;
  FTimerData.Free;
  DestroyAcceleratorTable(FAccelGroup);

  inherited Destroy;
end;

Procedure TWin32Object.SetCursor(Sender : TObject);
Begin
  LoadCursor(FParentWindow, MakeIntResource(Word(Integer(((Sender As TControl).Cursor)))));
End;

procedure TWin32Object.DoEvents;
Var
  RetVal: BOOL;
begin
  While True Do
  Begin
    RetVal := PeekMessage(TagMsg(FMessage), FParentWindow, 0, 0, PM_Remove);
    With FMessage Do
    Begin
      If (RetVal = BOOL(True)) And ((Message <> 0) Or ((WParam <> 0) And (LParam <> 0))) Then
        SendMessage(HWnd, Message, WParam, LParam)
      Else
        Break;
      Message := 0;
      WParam := 0;
      LParam := 0;
    End;
  End;
end;

{------------------------------------------------------------------------------}
{       TWin32Object AppTerminate                                                }
{       *Note: Tells GTK Engine to halt and destroy                            }
{------------------------------------------------------------------------------}
procedure TWin32Object.AppTerminate;
begin
 StrDispose(FormClassName);
 { ? } PostQuitMessage(0);
end;

{------------------------------------------------------------------------------}
{       TWin32Object Init                                                        }
{       *Note: Initializes GTK Engine                                          }
{------------------------------------------------------------------------------}
procedure TWin32Object.Init;

Var
 AMessage : Msg;
 hWindow  : HWnd;
 LogBrush : TLOGBRUSH;

Begin
 Assert(False, 'Trace:Win32Object.Init - Start');
 AppName := ArgV[0];
 FormClassName := StrAlloc(Length('TForm') + 1);
 StrPCopy(FormClassName, 'TForm');
 If Not WinRegister then
  Begin
   Assert(False, 'Trace:Win32Object.Init - Register Failed');
   Exit;
  End;
  
 FToolTipWindow := CreateWindowEx(WS_EX_TOPMOST, TOOLTIPS_CLASS, NULL, WS_POPUP Or TTS_NOPREFIX Or TTS_ALWAYSTIP, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, FParentWindow, HMENU(NULL), HInstance, NULL);
 Windows.SendMessage(FParentWindow, TTM_ACTIVATE, WPARAM(BOOL(TRUE)), 0);
 
   //Init stock objects;
   LogBrush.lbStyle := BS_NULL; 
   FStockNullBrush := CreateBrushIndirect(LogBrush);
   LogBrush.lbStyle := BS_SOLID; 
   LogBrush.lbColor := $000000;
   FStockBlackBrush := CreateBrushIndirect(LogBrush);
   LogBrush.lbColor := $C0C0C0;
   FStockLtGrayBrush := CreateBrushIndirect(LogBrush);
   LogBrush.lbColor := $808080;
   FStockGrayBrush := CreateBrushIndirect(LogBrush);
   LogBrush.lbColor := $404040;
   FStockDkGrayBrush := CreateBrushIndirect(LogBrush);
   LogBrush.lbColor := $FFFFFF;
   FStockWhiteBrush := CreateBrushIndirect(LogBrush);
   
 Assert(False, 'Trace:Win32Object.Init - Exit');
End;

function  TWin32Object.RecreateWnd(Sender: TObject): Integer;
var
  aParent : TWinControl;
Begin
  //could we just call IntSendMessage??

  //destroy old widget
  if TWinControl(sender).HANDLE<>0 then
    DestroyWindow(TWinControl(sender).HANDLE);

  aParent := TWinControl(sender).Parent;
  aParent.RemoveControl(TControl(sender));

  TWincontrol(sender).parent := nil;
  TWincontrol(sender).parent := aParent;

  ResizeChild(Sender,TWinControl(sender).Left,TWinControl(sender).Top,
     TWinControl(sender).Width,TWinControl(sender).Height);
  ShowHide(sender);
  Result:=0;
End;

{------------------------------------------------------------------------------}
{       TWin32Object IntSendMessage2                                             }
{       *Note: Message Processing Function                                     }
{------------------------------------------------------------------------------}
Function TWin32Object.IntSendMessage2( LM_Message : Integer; Parent,Child,Data : Pointer) : Integer;
Var
   R : TRect;
Begin
 Assert(False, 'Trace:IntSendMessage2 - Start, Received (' + LM_ToString(LM_Message) + ')');
 case LM_Message of
  LM_SetSize        : Begin
                       Assert(False, 'Trace:IntSendMessage2 - Resizing a control');
{!}//                       R := pTRect(Data)^;
{!}//                       ResizeChild(Parent,Child,pTRect(Data)^.Left,pTRect(Data)^.Top,pTRect(Data)^.Right,pTRect(Data)^.Bottom);
                      End;
  LM_AssignSelf     : AssignSelf(Child,Data);
  LM_SetName        : SetName(Child,Data);
  LM_AddPage        : AddNBPage(Parent, Child, Integer(Data));
  LM_RemovePage     : RemoveNBPage(Parent, Child, Integer(Data));
  LM_ShowTabs       : ;
  LM_SetTabPosition : Begin
                      End;
 End; {Case}

// START These messages were added by Michal Bukovjan

{!}// If TObject(Parent) is TControl then
  Case LM_Message of
   LM_GETITEMS      : Begin
                      End;
   LM_GETTEXT       : Begin
                      End;
   LM_GETITEMINDEX  : Begin
                      End;
   LM_SETITEMINDEX  : Begin
                      End;
   LM_GETSELSTART   : Begin
                      End;
   LM_GETSELLEN     : Begin
                      End;
   LM_GETLIMITTEXT  : Begin
                      End;
   LM_GETSELCOUNT   : Begin
                      End;
   LM_GETSEL        : Begin
                      End;
  End; {Case}
 Assert(False, 'Trace:IntSendMessage2 - Exit');
end;

{------------------------------------------------------------------------------}
{       TWin32Object IntSendMessage3                                             }
{       *Note: Message Processing                                              }
{------------------------------------------------------------------------------}
function TWin32Object.IntSendMessage3( LM_Message : Integer;
                                       Sender : TObject;
                                       data : pointer) : integer;
var
   handle    : hwnd;                    // handle of sender        
   pStr      : PChar;                   // temporary string pointer, must be allocated/disposed when used!
   Widget    : HWND;                    // pointer to gtk-widget (local use when neccessary)
   AParent   : TWinControl;             // only used twice, replace with typecasts!
   Pixmap    : HBITMAP;
   box1      : HWND;                    // currently only used for TBitBtn
   pixmapwid : HWND;                    // currently only used for TBitBtn, possibly replace with pixmap!!!!
   pLabel    : HWND;                    // currently only used as extra label-widget for TBitBtn
   Num       : Integer;                 // currently only used for LM_INSERTTOOLBUTTON
   pStr2     : PChar;                   // currently only used for LM_INSERTTOOLBUTTON
   GList     : Pointer;                 // Only used for listboxes, replace with widget!!!!!
   SelectionMode : DWORD;               // currently only used for listboxes
   ListItem  : HWND;                    // currently only used for listboxes
   Rect      : TRect;
   AOwner    : TControl;
   R         : Windows.Rect;
   tbb       : Array[0..100] Of TBBUTTON; // Limited to 101 buttons at present
   cbi       : COMBOBOXINFO;
Begin
 Result := 0;   //default value just in case nothing sets it
 Assert(False, 'Trace:IntSendMessage3 - Start, Received (' + LM_ToString(LM_Message) + ')');
 Assert(False, 'Trace:IntSendMessage3 - Value of Sender = $' + IntToHex(LongInt(Sender), 8));
   case LM_Message of
      LM_CREATE   : CreateComponent(Sender);

      LM_SETCOLOR : SetColor(Sender);

      LM_SETPixel : SetPixel(Sender,Data); 

      LM_GETPixel : GetPixel(Sender,Data); 

      LM_ShowHide :
      begin
         Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> Show/Hide', [Sender.ClassNAme]));
         ShowHide(Sender);
      end;

      LM_SetCursor : SetCursor(Sender);

      LM_SetLabel  : SetLabel(Sender,Data);

      LM_GETVALUE  : Result := GetValue (Sender, data);

      LM_SETVALUE  : Result := SetValue (Sender, data);
     
      LM_SETPROPERTIES: Result := SetProperties(Sender);

      LM_RECREATEWND : Result := RecreateWnd(sender);
      
      LM_ATTACHMENU: AttachMenu(Sender);
   else begin
      handle := ObjectToHWND(Sender);
//???   if handle = nil then assert (false, Format ('Trace:  [TgtkObject.IntSendMessage3] %s --> got handle=nil',[Sender.ClassName])); 
      Case LM_Message of

      LM_SetText  : SetText(@Handle, Data); 

      LM_AddChild :
      begin
         Assert(False, 'Trace:Adding a child to Parent');
         If (TWinControl(Sender).Parent is TToolbar) then
         Begin
//          Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> %s   ---calling INSERTBUTTON from Add Child', [AParent.ClassName, Sender.ClassNAme]));
            exit;
         end
         else Begin
            AParent := (Sender as TWinControl).Parent;
            Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> Calling Add Child: %s', [AParent.ClassName, Sender.ClassNAme]));
            AddChild(Pointer(AParent.Handle), @Handle, AParent.Left, AParent.Top);
         end;
      end;

      LM_BRINGTOFRONT:
      begin
         Assert(False, 'Trace:TODO:bringtofront');
         //For now just hide and show again.
	 if (Sender is TControl) then begin
	   TControl(Sender).Parent.RemoveControl(TControl(Sender));
	   writeln('Removed control ', TControl(Sender).Name);
	   TControl(Sender).Parent.InsertControl(TControl(Sender));
	   writeln('Inserted control ', TControl(Sender).Name);
	 end;
      end;

      LM_BTNDEFAULT_CHANGED :
      Begin
        if (TButton(Sender).Default) and (Windows.SendMessage(TWinControl(Sender).Handle, BM_GETSTATE, 0, 0) = BST_FOCUS) 
           then Windows.SendMessage(TWinControl(Sender).Handle, BM_SETSTATE, WPARAM(TRUE), 0);
      end;

      LM_DESTROY :
      begin
         if (Sender is TWinControl) or (Sender is TCommonDialog) then 
         begin
           if Handle<>0 then
             DestroyWindow(Handle);
         end
         else
           Assert (False, Format ('Trace:Dont know how to destroy component %s', [sender.classname]));
      end;

      LM_DRAGINFOCHANGED :
      Begin
      end;

      //TBitBtn
      LM_IMAGECHANGED, LM_LAYOUTCHANGED :
      Begin
              Assert(False, 'Trace:********************');
         Assert(False, 'Trace:1');
         Windows.GetClientRect(Handle, @R);
         box1 := R.Right - R.Left;
         if box1 <> Integer(nil) then
         begin
            Assert(False, 'Trace:REMOVING THE HBOX');
            DestroyWindow(Handle);
         end;

         Assert(False, 'Trace:2');
         pixmap := LoadBitmap(GetWindowLong(TWinControl(Sender).Parent.Handle, GWL_HINSTANCE), MAKEINTRESOURCE(PGDIObject(TBitBtn(Sender).Glyph.MaskHandle)^.GDIBitmapObject));
         Assert(False, 'Trace:3');

         Assert(False, 'Trace:4');
         pStr := StrAlloc(length(TBitBtn(Sender).Caption) + 1);
         StrPCopy(pStr, TBitBtn(Sender).Caption);
         Windows.GetClientRect(Handle, @R);
         pLabel := CreateWindow('STATIC', pstr, WS_Child Or WS_Visible, R.Right, R.Bottom, R.Right - R.Left, R.Bottom - R.Top, Handle, HWnd(Nil), System.HInstance, Nil);
         MoveWindow(HWnd(pixmap), 0, 0, 0, 0, True);
         StrDispose(pStr);
         Assert(False, 'Trace:5');
         Assert(False, 'Trace:********************');
      end;

//SH: think of TBitmap.handle!!!!
      LM_LOADXPM:
         Begin
            if (sender is TBitmap) then
            Begin
               Assert(False, 'Trace:pixmap name '+strpas(data));
               pixmap := LoadImage(0, LPCTSTR(Data), IMAGE_BITMAP, 0, 0, LR_DefaultSize Or LR_LoadFromFile);
               Assert(False, 'Trace:1');
               if Pixmap = HBITMAP(nil)
                  then Assert(False, 'Trace:PIXMAP NOT LOADED!');
               PdeviceContext(TBitmap(sender).handle)^.CurrentBitmap :=pgdiObject(pixmap);
            end;
         end;


      LM_POPUPSHOW :
        Begin 
              TrackPopupMenu(HMENU(TWinControl(Sender).Handle), TPM_LEFTALIGN, TControl(Sender).Left, TControl(Sender).Top, TWinControl(Sender).Parent.Handle, 0, Windows.PRECT(NULL)^);	
{Displays a menu and makes it available for selection. Applications can use this function to display context-sensitive menus, 
and will typically supply NULL for the parent_menu_shell, parent_menu_item, func and data parameters. 
The default menu positioning function will position the menu at the current pointer position.
menu :	a GtkMenu.	
parent_menu_shell :	the menu shell containing the triggering menu item.	
parent_menu_item :	the menu item whose activation triggered the popup.	
func :	a user supplied function used to position the menu.	
data :	user supplied data to be passed to func.	
button :	the button which was pressed to initiate the event.	
activate_time :	the time at which the activation event occurred. 	
}

        end;

      LM_SETFILTER :
      begin 
         pStr := StrAlloc(length(TFileDialog(Sender).Filter) + 1); 
         StrPCopy(pStr, TFileDialog(Sender).Filter);
         StrDispose(pStr); 
      end; 
 
      LM_SETFILENAME : 
      begin 
         pStr := StrAlloc(length(TFileDialog(Sender).Filename) + 1); 
         StrPCopy(pStr, TFileDialog(Sender).Filename); 
         StrDispose(pStr); 
      end; 

      LM_SETFOCUS:
      Begin
        if Windows.GetFocus = Handle then
        Begin
          Windows.SetFocus(Handle);
        End
        else
        Begin
          Writeln('This control '+TControl(Sender).name+' can not get focus');
        End;
      End;

      LM_SetSize :
      begin
         Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> LM_SetSize(%d, %d, %d, %d)', [Sender.ClassNAme, PRect(Data)^.Left,PRect(Data)^.Top,PRect(Data)^.Right,PRect(Data)^.Bottom]));
//writeln('[IntSendMessage3.lm_setsize] ',PRect(Data)^.Left,',',PRect(Data)^.Top,',',
//             PRect(Data)^.Right,',',PRect(Data)^.Bottom);
         ResizeChild(Sender,PRect(Data)^.Left,PRect(Data)^.Top,
             PRect(Data)^.Right,PRect(Data)^.Bottom);
      end;

      LM_ShowModal :
      begin
         if Sender is TCommonDialog then
         begin
            // Should be done elsewhere (eg via SetLabel) not here!
            pStr:= StrAlloc(Length(TCommonDialog(Sender).Title) + 1);
            try
               StrPCopy(pStr, TCommonDialog(Sender).Title);
               SetWindowText(handle, pStr);
            finally
               StrDispose(pStr);
            end;
         end;
         SetWindowLong(Handle, GWL_STYLE, WS_DLGFRAME);
         ShowWindow(Handle, SW_Show);
      end;

      LM_TB_BUTTONCOUNT:
      begin
         if (Sender is TToolbar)
            then begin
              Result := Windows.SendMessage(Handle, TB_BUTTONCOUNT, 0, 0);
            end
            else Result := -1;
      end;

//SH: think of TCanvas.handle!!!!
      LM_ReDraw : 
      begin 
         Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> Redraw', [Sender.ClassName]));
         if (Sender is TCanvas) then 
            ReDraw(Pointer((Sender as TCanvas).Handle))
         else
          if not (sender is TSpeedbutton) then
            ReDraw(Pointer(Handle))
         else
          if (sender is TSpeedButton) then
            If TSpeedbutton(sender).Visible then (Sender as TSpeedButton).perform(LM_PAINT,0,0)
              else
              Begin
              Rect := TSpeedButton(sender).BoundsRect;
              InvalidateRect(TSpeedButton(sender).Parent.Handle,@Rect,True);

              end;
      end;

      LM_AddPage :
      begin
         Assert(False, Format('Trace:  [TgtkObject.IntSendMessage3] %s --> Add NB page: %s', [Sender.ClassName, TLMNotebookEvent(Data^).Child.ClassName]));
         AddNBPage(TControl(Sender), TLMNotebookEvent(Data^).Child, TLMNotebookEvent(Data^).Page);
      end;

      LM_RemovePage :
      begin
         RemoveNBPage(TControl(Sender), TLMNotebookEvent(Data^).Page);
      end;

      LM_ShowTabs :
      begin
         Result := Ord(True);
         //gtk_notebook_set_show_tabs(PGtkNotebook(Handle), Boolean(Integer(TLMNotebookEvent(Data^).ShowTabs)));
      end;

      LM_SetTabPosition :
      begin
         case TTabPosition(TLMNotebookEvent(Data^).TabPosition^) of
            
            tpTop:
            begin
              R.Top := 0;
              TabCtrl_AdjustRect(Handle, True, Windows.RECT(R));
            end;
            tpBottom:
            begin
              R.Bottom := 0;
              TabCtrl_AdjustRect(Handle, True, Windows.RECT(R));
            end;
            tpLeft:
            begin
              R.Left := 0;
              TabCtrl_AdjustRect(Handle, True, Windows.RECT(R));
            end;
            tpRight:
            begin
              R.Right := 0;
              TabCtrl_AdjustRect(Handle, True, Windows.RECT(R));
            end;
         end;
      end;


      LM_INSERTTOOLBUTTON:
      begin
      Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
      Assert(False, 'Trace:Toolbutton being inserted');
      Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
         If (SENDER is TWINCONTROL) Then
         Begin
            pStr := StrAlloc(Length(ttoolbutton(SENDER).Caption)+1);
            StrPCopy(pStr,ttoolbutton(SENDER).Caption);
            pStr2 := StrAlloc(Length(tcontrol(Sender).Hint)+1);
            StrPCopy(pStr2,tcontrol(Sender).Hint);
         end 
         else Begin
            raise Exception.Create('Can not assign this control to the toolbar');
            exit;
         end;
    
         num := TToolbar(TWinControl(Sender).parent).Buttonlist.IndexOf(TControl(Sender));
         if num < 0 then Num := TToolbar(TWinControl(Sender).parent).Buttonlist.Count+1;
         Assert(False, Format('Trace:NUM = %d in INSERTBUTTON',[num]));

         {Make sure it's created!!}
         if handle = 0 
            then IntSendMessage3(LM_CREATE,Sender,nil);

         With tbb[Num] Do
         Begin
           iBitmap := Num;
           idCommand := Num;
           fsState := TBSTATE_ELLIPSES Or TBSTATE_ENABLED;
           iString := Integer(PStr);
         End;
         Windows.SendMessage(Handle, TB_ADDBUTTONS, WParam(UInt(IntSendMessage3(LM_TB_BUTTONCOUNT, Sender, Nil) + 1)), LParam(LPTBButton(@tbb)));
         {gtk_toolbar_insert_widget(pGTKToolbar(TWinControl(sender).parent.Handle),
              pgtkwidget(handle),pstr,pStr2,Num);}
         StrDispose(pStr);
         StrDispose(pStr2);
      Assert(False, 'Trace:!!!!!!!!!!!!!!!!!!!!!!!!!');
      end;

      LM_DELETETOOLBUTTON:
      Begin
         Windows.SendMessage(HWND(TToolbar(TWinControl(Sender).parent).handle), TB_DELETEBUTTON, WParam(Data^), 0); // Assuming Data is the button to remove
      end;

      LM_Invalidate : 
      begin 
         Assert(False, 'Trace:Trying to invalidate window... !!!');
        Windows.InvalidateRect(Handle, Windows.PRECT(NULL)^, True);
      end; 

      LM_SCREENINIT :
      begin
        WriteLN('LM_SCREENINIT called --> should go to GTKObject.Init');
        WriteLN('TODO: check this');
      end;  

      LM_GETITEMS :
      begin
        if (Sender as TControl).fCompStyle = csCListBox
        then begin
          Widget := HWND(GetCoreChildControl(Sender));
          Data := TWin32CListStringList.Create(Sender);
          Result := integer(Data);
        end 
        else begin
          case (Sender as TControl).fCompStyle of
            csComboBox :
            begin
              GetComboBoxInfo(Handle, @cbi);
              Widget := cbi.hwndList;
            end;
            csListBox : Widget:= HWND(GetCoreChildControl(Sender));
          else 
            raise Exception.Create('Message LM_GETITEMS - Not implemented');
          end;
          Data:= TWin32ListStringList.Create(PObject(Widget)^);
          Result:= Integer(Data);      
        end;
      end;

      LM_GETTEXT :
      begin
         Assert (true, 'WARNING:[TgtkObject.IntSendMessage3] usage of LM_GETTEXT superfluous, use interface-function GetText instead'); 
         Result := integer (nil);
      end;

      LM_GETITEMINDEX :
      begin
        case (Sender as TControl).fCompStyle of
           csListBox, csCListBox: 
             begin
                if TListBox(Sender).MultiSelect then
                begin
                    Result := Windows.SendMessage(Handle, LB_GETSELITEMS, 0, LParam(@Result));
                end
                else begin
                   Result := Windows.SendMessage(Handle, LB_GETCURSEL, 0, 0);
                   If Result = LB_ERR Then
                   Begin
                     Assert(False, '[IntSendMessage3] Could not retrieve item index via LM_GETITEMINDEX; try selecting an item first');
                     Result := -1;
                   End;
               end;
             end;
           csNotebook:
             begin
                TLMNotebookEvent(Data^).Page := Windows.SendMessage(Handle, TCM_GETCURSEL, 0, 0);
             end;
        end;
      end;
          
      LM_SETITEMINDEX :
      begin
         case (Sender as TControl).fCompStyle of
            csComboBox: Windows.SendMessage(Handle, CB_SETCURSEL, WParam(Integer(Data)), 0);
            csListBox, csCListBox:
            begin
              if TListBox(Sender).MultiSelect then
                Windows.SendMessage(Handle, LB_SETSEL, WPARAM(TRUE), LParam(Integer(Data)))
              else
                Windows.SendMessage(Handle, LB_SETCURSEL, WParam(Integer(Data)), 0);
            end;
            csNotebook: begin
                           Assert(False, 'Trace:Setting Page to ' + IntToStr(TLMNotebookEvent(Data^).Page));
                           Windows.SendMessage(Handle, TCM_SETCURSEL, LParam(Integer(Data)), 0); //gtk_notebook_set_page(PGtkNotebook(Handle), TLMNotebookEvent(Data^).Page);
                        end;
         end;
      end;
          
      LM_GETSELSTART :
      begin
         if (Sender as TControl).fCompStyle = csComboBox then
         begin
            Result := Low(Windows.SendMessage(Handle, CB_GETEDITSEL, WPARAM(NULL), LPARAM(NULL)));           
         end;
      end;
          
      LM_GETSELLEN :
      begin
         if (Sender as TControl).fCompStyle = csComboBox then
         begin
            Result := Windows.SendMessage(Handle, CB_GETEDITSEL, WPARAM(NULL), LPARAM(NULL));
            Result := High(Result) - Low(Result);
         end;
      end;
  
      LM_GETLIMITTEXT :
      begin
         if (Sender as TControl).fCompStyle = csComboBox then
         begin
            Result := Windows.SendMessage(Handle, CB_GETLBTEXTLEN, WParam(Integer(Data)), 0) + 1; // The max length must be greater than the length of the text
            if Result < 30000 then Result := 30000;
         end;
      end;
  
      LM_SETSELSTART : 
      begin
         if (Sender is TControl) and (TControl(Sender).fCompStyle = csComboBox) then
         begin
            Windows.SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Word(Integer(Data)), -1));
         end;
      end;
          
      LM_SETSELLEN :
      begin
         if (Sender is TControl) and (TControl(Sender).fCompStyle = csComboBox) then
         begin
            Windows.SendMessage(Handle, CB_SETCURSEL, WParam(Data), 0);
         end;
      end;  
 
      LM_GetLineCount :
      begin

      end;

      LM_GETSELCOUNT :
      begin
         case (Sender as TControl).fCompStyle of
            csListBox, csCListBox : Result := Windows.SendMessage(Handle, LB_GETSELCOUNT, 0, 0);
         end;
      end;
  
      LM_GETSEL :
      begin
         if ((Sender as TWinControl).fCompStyle = csListBox) or ((Sender as TControl).fCompStyle = csCListBox) then
         begin
           ListItem := IntSendMessage3(LM_GETITEMINDEX, Sender, Data);
           Result := Windows.SendMessage(Handle, LB_GETSEL, WParam(Data), 0);
         end
      end;
  
      LM_SETLIMITTEXT :
      begin
         if (Sender is TControl) and (TControl(Sender).fCompStyle = csComboBox)
            then Windows.SendMessage(Handle, CB_LIMITTEXT, WParam(Data), 0);
      end;
 
      LM_SORT :
      begin
         if  (Sender is TControl) and assigned (data) then
         begin
            case TControl(Sender).fCompStyle of
               csComboBox, 
               csListBox   : TWin32ListStringList(TLMSort(Data^).List).Sorted:= TLMSort(Data^).IsSorted;
               csCListBox  : TWin32CListStringList(TLMSort(Data^).List).Sorted := TLMSort(Data^).IsSorted;
            end
         end
      end;
 
      LM_SETSEL :
      begin
         if (Sender is TControl) and 
            (TControl(Sender).fCompStyle in [csListBox, csCListBox]) and
            assigned (data) then 
         begin
            if (TControl(Sender).fCompStyle = csListBox) then 
            begin
               if TLMSetSel(Data^).Selected 
                  then Windows.SendMessage(Handle, LB_SELITEMRANGE, WParam(Ord(True)), MakeLParam(0, 0))
                  else Windows.SendMessage(Handle, LB_SELITEMRANGE, WParam(Ord(False)), MakeLParam(0, 0));
            end
         end;  
      end;
 
      LM_SETSELMODE :
      begin
         if (Sender is TControl) and 
            (TControl(Sender).fCompStyle in [csListBox, csCListBox]) and
            assigned (data) then 
         begin
            if TLMSetSelMode(Data^).MultiSelect then 
            begin
               if TLMSetSelMode(Data^).ExtendedSelect 
                  then SelectionMode:= LBS_EXTENDEDSEL
                  else SelectionMode:= LBS_MULTIPLESEL;
            end 
            else 
               SelectionMode:= 0;
            SetWindowLong(Handle, GWL_STYLE, SelectionMode);
         end;
      end;
 
      LM_SETBORDER :
      begin
         if (Sender is TControl) then
         begin
            if (TControl(Sender).fCompStyle = csListBox) or (TControl(Sender).fCompStyle = csCListBox) then
            begin
               { In TempWidget, a viewport is stored }
               Widget:= Handle;
               if TListBox(Sender).BorderStyle = TBorderStyle(bsSingle) 
                  then SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) Or WS_EX_CLIENTEDGE)
                       //gtk_viewport_set_shadow_type(PGtkViewPort(Widget), GTK_SHADOW_IN)
                  else SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) Or Not WS_EX_CLIENTEDGE);
                       //gtk_viewport_set_shadow_type(PGtkViewPort(Widget), GTK_SHADOW_NONE);
            end
         end;
      end;

      else
         Assert(True, Format ('WARNING: Unhandled message %d in IntSendMessage3 send by %s --> message:Redraw', [LM_Message, Sender.ClassName]));
         // unhandled message
      end; // end of 2nd case
   end;   // end of else-part of 1st case
   end;   // end of 1st case
end;

{------------------------------------------------------------------------------}
{       TWin32Object IntSendMessgae                                              }
{       *Note: Message Processing                                              }
{------------------------------------------------------------------------------}
Procedure TWin32Object.IntSendMessage(LM_Message : Integer; CompStyle : Integer; Var P : Pointer; Val1 : Integer; Var Str1 : String);
Begin
 Assert(False, 'Trace:IntSendMessage - Start, Received (' + LM_ToString(LM_Message) + ')');
 Case LM_Message of
  //LM_SetLabel : SetLabel(CompStyle, P, Str1);
  LM_GetLabel : Str1 := GetLabel(CompStyle,P);
 Else
  Assert(False, 'Trace:IntSendMessage - ERROR DETECTED - The message sent was invalid -'+Inttostr(LM_Message));
 End; {Case}
 Assert(False, 'Trace:IntSendMessage - Exit');
end;

{------------------------------------------------------------------------------}
{       TWin32Object ResizeChild                                                 }
{       *Note: Resize a child widget on the parents fixed widget               }
{------------------------------------------------------------------------------}
procedure TWin32Object.ResizeChild(Sender : TObject; Left ,Top, Width, Height : Integer);
Var
  Handle: HWND;
  R: Windows.Rect;
begin
  Handle := HWND(PWin32Control(@Sender)^.Window);
  If GetParent(Handle) <> HWND(Nil) Then
    MoveWindow(Handle, Left, Top, Width, Height, True)
  Else
  Begin
    Windows.GetClientRect(Handle, @R);
    MoveWindow(Handle, R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top, True);
  End;
end;

{------------------------------------------------------------------------------}
{       TWin32Object AddChild                                                    }
{       *Note: Adds A Child to a Parent Widget                                 }
{------------------------------------------------------------------------------}
procedure TWin32Object.AddChild(Parent,Child : Pointer; Left,Top: Integer);
begin
 Assert(False, 'Trace:AddChild - Parent Window Handle is $' + IntToHex(LongInt(Parent), 8));
 Assert(False, 'Trace:AddChild - Child Window Handle is $' + IntToHex(LongInt(Child), 8));
 SetParent(HWnd(Child), HWnd(Parent));
end;

Procedure TWin32Object.SetText(Child,Data : Pointer);
type
   pMsg = ^TLMSetControlText;
var
   num : Integer;
begin
   case pMsg(Data)^.fCompStyle of
      csStatusBar : begin
                    Windows.SendMessage(HWND(Child), SB_SETTEXT, WParam(pMsg(Data)^.panel), LParam(LPSTR(pMsg(Data)^.UserData)));
                    end
   else        
      writeln ('STOPPOK: [TGtkObject.SetText] Possible superfluous use of SetText, use SetLabel instead!');
   end;
{STOPPOK: Code seems superfluous, see SetLabel instead}

end;

Function TWin32Object.GetText(Sender: TControl; Var Data: String): Boolean;
Var
  CapLen: Cardinal;
  Caption: PChar;
Begin
  Assert(False, 'Trace:TWin32Object.GetText - start');
  Result := True;
  Case Sender.FCompStyle Of
    csComboBox, csEdit, csMemo:
    Begin
      CapLen := GetWindowTextLength((Sender As TWinControl).Handle);
      GetWindowText((Sender As TWinControl).Handle, Caption, CapLen);
    End;
    Else
      Result := False;
  End;
  Data := StrPas(Caption);
End;

{------------------------------------------------------------------------------}
{       TWin32Object SetLabel                                                    }
{       *Note: Sets the label text on a widget                                 }
{------------------------------------------------------------------------------}
procedure TWin32Object.SetLabel(Sender : TObject; Data : Pointer);
var
   P : Pointer;
   pLabel: pchar;
   Wnd: HWND;
   tbbi: TBBUTTONINFO;
   tci: TC_ITEM;
const
  TermChar: PChar = #0#0;
begin
   if Sender is TWinControl
      then Assert(False, Format('Trace:  [TgtkObject.SetLabel] %s --> label %s', [Sender.ClassName, TControl(Sender).Caption]))
      else Assert(False, Format('Trace:WARNING: [TgtkObject.SetLabel] %s --> No Decendant of TWinControl', [Sender.ClassName]));

   P := Pointer(TWinControl(Sender).Handle);
   Wnd := PWin32Control(@Sender)^.Window;
   Assert(p = nil, 'Trace:WARNING: [TgtkObject.SetLabel] --> got nil pointer');
   Assert(False, 'Trace:Setting Str1 in SetLabel');
   pLabel := pchar(Data);
   
   case TControl(Sender).fCompStyle of
      csBitBtn      : IntSendMessage3(LM_IMAGECHANGED,SENDER,nil);

      csToolButton  : begin
                         pLabel := StrCat(pLabel, TermChar);
                         Windows.SendMessage(Wnd, TB_ADDSTRING, WParam(Nil), LParam(MakeLong(Word(Integer(pLabel)), 0)));
                      end;
      csPage        :
      begin
         tci.mask := TCIF_TEXT;
         tci.pszText := PChar(PTabInfo(Data)^.Caption);
         Windows.SendMessage(Wnd, TCM_SETITEM, WParam(PTabInfo(Data)^.Index), LPARAM(@tci));
      end;
      else SetWindowText(Wnd, pLabel);

   Assert(False, Format('trace:  [TgtkObject.SetLabel] %s --> END', [Sender.ClassName]));
 end;
end;

{------------------------------------------------------------------------------}
{       TWin32Object SetColor                                                 }
{       *Note: Changes the form's default background color                                 }
{------------------------------------------------------------------------------}
Procedure TWin32Object.SetColor(Sender : TObject);
Var
  DC: HDC;
Begin
  With Sender Do
  Begin
    If Sender Is TWincontrol Then
    Begin
      With TWincontrol(Sender) Do
      Begin
        // Temphack to set backcolor, till better solution
        If HandleAllocated Then
        Begin
          DC := GetDC(Handle);
          SetBKColor(Handle, ColorToRGB(Color));
          ReleaseDC(Handle, DC);
        End;
      End;
    End;
  End;
  //NOT USED RIGHT NOW..........CAUSES ALL FORMS TO USE THESE COLORS!!!!!!
End;

{------------------------------------------------------------------------------}
{       TWin32Object SetCallback                                                 }
{       *Note: Applies a Message to the sender                                 }
{------------------------------------------------------------------------------}
procedure TWin32Object.SetCallback(Msg : LongInt; Sender : TObject);
var
 winObject : HWND;
 signalFunc : Pointer;
 gSignal : PChar;
 I : Integer;
 Signal : String;
 Mess: UINT;
 MessFunc: TCBFunc;
 WPar: WParam;
 LPar: LParam;
 
Procedure SignalConnect(Message: UINT; Func: TCBFunc);
Begin
  // Func;
End;

Function CallWndProc(Code: Integer; WP: ULong; LP: LParam): LResult; StdCall;
Begin
  SignalConnect(Mess, MessFunc);
  CallNextHookEx(FHkProc, Code, WP, LP);
  Exit;
End;

Begin
 If Sender is TControlCanvas then
  winObject := (Sender as TControlCanvas).Handle
 Else If Sender Is TCustomForm Then
   winObject := FParentWindow
 else
  winObject := (Sender as TWinControl).Handle;
 signalFunc := nil;
 Signal := '';
 case Msg of
  LM_SHOWWINDOW : Begin
             Mess := WM_SHOWWINDOW;
             WPar := WParam(Not Boolean(WPar));
             signalFunc := @Win32showCB;
            End;
  LM_DESTROY : Begin
                Mess := WM_DESTROY;
                signalFunc := @Win32destroyCB;
               End;
    LM_CLOSEQUERY :
    begin
      Mess := WM_CLOSE;
      signalFunc := @Win32DeleteCB;
    end;

  LM_ACTIVATE,
  LM_ACTIVATEITEM : Begin
                 Mess := WM_ACTIVATE;
                 signalFunc := @Win32activateCB;
                End;
  LM_CHANGED      : Begin
                     //Mess := WM_EDITCHANGED;
                     signalFunc := @Win32changedCB;
                    End;
  LM_CLICKED      : Begin
                     Assert(False, 'Trace:SetCallBack - LM_CLICKED');
                     //Mess := WM_MBUTTONUPDOWN;
                     signalFunc := @Win32clickedCB;
                    End;
  LM_CONFIGUREEVENT : Begin
                       signal := 'configure-event';
                       signalFunc := @Win32configureevent;
                      End;
  LM_PAINT          : Begin
                       signal := 'paint';
                       signalFunc := @Win32draw;
                       signal := 'expose-event';
                       signalFunc := @Win32exposeevent;
                      END;
  LM_EXPOSEEVENT    : Begin
                       signal := 'expose-event';
                       signalFunc := @Win32exposeevent;
                      End;
  LM_FOCUS          : Begin
                       SetFocus(WinObject);
                       signalFunc := @Win32focusCB;
                      End;
    LM_KEYDOWN,
    LM_CHAR,
    LM_KEYUP,
    LM_SYSKEYDOWN,
    LM_SYSCHAR,
    LM_SYSKEYUP:
    begin
      Mess := WM_KEYUP;
      Mess := WM_KEYDOWN;
      SignalFunc := @Win32KeyUpDown;
    end;
  LM_PRESSED        : Begin
                       Mess := WM_KEYUP;
                       signalFunc := @Win32pressedCB;
                      End;
  LM_RELEASED       : Begin
                       signal := 'released';
                       SendMessage(WinObject, WM_KEYDOWN, 0, 0);
                      End;
  LM_MOVECURSOR     : Begin
                       signal := 'move-cursor';
                       signalFunc := @Win32movecursorCB;
                      End;
    LM_LBUTTONDOWN,
    LM_RBUTTONDOWN,
    LM_MBUTTONDOWN,
    LM_MOUSEWHEEL :
    begin
      Mess := WM_MBUTTONDOWN;
      signalFunc := @win32mousebtnpress;
    end;
   LM_LBUTTONUP,
    LM_RBUTTONUP,
    LM_MBUTTONUP:
    begin
      Mess := WM_MBUTTONUP;
      signalFunc := @win32mousebtnrelease;
    end;
  LM_ENTER           : Begin
                        if Sender is TButton then
                        begin
                          signal := 'enter';
                          signalFunc := @Win32enterCB;
                        end
                        else
                        begin
                          Mess := WM_SETFOCUS;
                          signalFunc := @Win32FocusInNotifyCB;
                        end;
                       End;
    LM_EXIT, LM_LEAVE :
    begin
      SetFocus(HWND(Nil));
    end;
  LM_WINDOWPOSCHANGED: Begin
                        Mess := WM_WINDOWPOSCHANGED;
                        signalFunc := @Win32size_allocateCB;
                       End;
  LM_CHECKRESIZE     : Begin
                        signal := 'check-resize';
                        signalFunc := @Win32resizeCB;
                       End;
  LM_INSERTTEXT      : Begin
                        signal := 'insert-text';
                        signalFunc := @Win32inserttext;
                       End;
  LM_DELETETEXT      : Begin
                        signal := 'delete-text';
                        signalFunc := @Win32deletetext;
                       End;
  LM_SETEDITABLE     : Begin
                        signal := 'set-editable';
                        signalFunc := @Win32seteditable;
                       End;
  LM_MOVEWORD        : Begin
                        signal := 'move-word';
                        signalFunc := @Win32moveword;
                       End;
  LM_MOVEPAGE        : Begin
                        signal := 'move-page';
                        signalFunc := @Win32movepage;
                       End;
  LM_MOVETOROW       : Begin
                        signal := 'move-to-row';
                        signalFunc := @Win32movetorow;
                       End;
  LM_MOVETOCOLUMN    : Begin
                        signal := 'move-to-column';
                        signalFunc := @Win32movetocolumn;
                       End;
  LM_KILLCHAR        : Begin
                        signal := 'kill-char';
                        signalFunc := @Win32killchar;
                       End;
  LM_KILLWORD        : Begin
                        signal := 'kill-word';
                        signalFunc := @Win32killword;
                       End;
  LM_KILLLINE        : Begin
                        signal := 'kill-line';
                        signalFunc := @Win32killline;
                       End;
  LM_CUTTOCLIP       : Begin
                        signal := 'cut-clipboard';
                        signalFunc := @Win32cuttoclip;
                       End;
  LM_COPYTOCLIP      : Begin
                        signal := 'copy-clipboard';
                        signalFunc := @Win32copytoclip;
                       End;
  LM_PASTEFROMCLIP   : Begin
                        signal := 'paste-clipboard';
                        signalFunc := @Win32pastefromclip;
                       End;
    LM_HSCROLL:
    begin
      Mess := WM_HSCROLL;
      signalFunc := @Win32HScrollCB;
    end;

    LM_VSCROLL:
    begin
      Mess := WM_VSCROLL;
      signalFunc := @Win32VScrollCB;
    end;
   Else
   Begin
    Assert(False, 'Trace:SetCallback - ERROR:  Signal "'+signal+'" not found!');
    Exit;
   End;
 End; {Case}
 FromCBProc := True;
 // FHkProc := Windows.SetWindowsHookEx(WH_CALLWNDPROC, @CallWndProc, HInstance, 0);
 gSignal:= StrAlloc(length(Signal) + 1);
 StrPCopy(gSignal, signal);
 StrDispose(gsignal);
end;

{------------------------------------------------------------------------------}
{       TWin32Object RemoveCallBacks                                             }
{       *Note: Removes Call Back Signals from the sender                       }
{------------------------------------------------------------------------------}
procedure TWin32Object.RemoveCallbacks(Sender : TControl);
begin
  FromCBProc := False;
  UnhookWindowsHookEx(FHkProc);
end;

{------------------------------------------------------------------------------}
{       TWin32Object CreateComponent                                             }
{       *Note: Tells GTK Engine to create a Win32Control                             }
{------------------------------------------------------------------------------}
procedure TWin32Object.CreateComponent(Sender : TObject);
Type
  TCustomColors = Array[1..16] Of ColorRef;
Var
 caption : string;
 StrTemp, pStr : PChar;
 p : HWnd;
 CompStyle : Integer;
 ParentWindow : HWnd;
 ParentControl : TObject;
 OpenFile: OpenFileName;
 ColorSelect: TChooseColor;
 CustomColors: TCustomColors;
 RGBIO: DWord;
 Flags: Integer;
 tci: TC_ITEM;
Const
  Ext: PChar = 'txt';
  Filter: PChar = 'Pascal Files (*.pas)'#0'*.pas'#0'All Files(*.*)'#0'*.*'#0#0;
  StFl: Pchar = #0'';
Begin
 Assert(False, 'Trace:CreateComponent - Start');
 Assert(False, 'Trace:CreateComponent - Value of Sender is $' + IntToHex(LongInt(Sender), 8));

 If (Sender is TControl) then
  caption := (Sender as TControl).caption
 else
  caption := 'Unknown';

 // the following is for debug only
 if caption = '' then
  caption := Inttostr((Sender as TControl).FCompstyle);

 Assert(False, 'Trace:CreateComponent - ----------------------Creating component in TWin32Object-   STR = '+caption+'-');

 // until here remove when debug not needed
 if caption = '' then
  caption := 'Blank';

 strTemp := StrAlloc(length(caption) + 1);
 StrPCopy(strTemp, caption);
 Assert(False, 'Trace:CreateComponent - 1');
   p := HWND(Nil);

   CompStyle := csNone;
   Caption   := Sender.ClassName;
   if (Sender is TControl) then
   begin
      caption := TControl(Sender).caption;
      CompStyle := TControl(Sender).FCompStyle
   end
   else if (Sender is TMenuItem) then
   begin
      caption := TMenuItem(Sender).caption;
      CompStyle := TMenuItem(Sender).FCompStyle;
   end
   else if (Sender is TMenu) or (Sender is TPopupMenu)
      then CompStyle := TMenu(Sender).FCompStyle
   else if (Sender is TCommonDialog)
      then CompStyle := TCommonDialog(Sender).FCompStyle;

 Assert(False, 'Trace:CreateComponent - Control Style is ' + CS_To_String(CompStyle));

 Case CompStyle of
  csAlignment      : Begin
                     End;
  csBitBtn         : Begin
                       P := CreateWindow('BUTTON', strTemp, WS_CHILD Or WS_VISIBLE Or BS_BITMAP, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
  csButton         : Begin
                      Assert(False, 'Trace:CreateComponent - Creating Button');
                      Assert(False, 'Trace:CreateComponent - Value of Button Parent is $' + IntToHex(LongInt((Sender as TControl).Parent), 8));
                      Assert(False, 'Trace:CreateComponent - Value of Button Owner is $' + IntToHex(LongInt((Sender as TControl).Owner), 8));
                      ParentControl := (Sender as TControl).Owner;
                      Assert(False, 'Trace:CreateComponent - Value of ParentControl is $' + IntToHex(LongInt(ParentControl), 8));
                      Assert(False, 'Trace:CreateComponent - Value of Button Parent Window is $' + IntToHex(LongInt(ParentWindow), 8));
                      Assert(False, 'Trace:CreateComponent - Value of Button Left   is $' + IntToHex((Sender as TControl).Left  , 4));
                      Assert(False, 'Trace:CreateComponent - Value of Button Top    is $' + IntToHex((Sender as TControl).Top   , 4));
                      Assert(False, 'Trace:CreateComponent - Value of Button Width  is $' + IntToHex((Sender as TControl).Width , 4));
                      Assert(False, 'Trace:CreateComponent - Value of Button Height is $' + IntToHex((Sender as TControl).Height, 4));
                      P := CreateWindow('BUTTON', strTemp,
                                   WS_VISIBLE OR WS_CHILD OR BS_DEFPUSHBUTTON,
                                   (Sender as TControl).Left,
                                   (Sender as TControl).Top,
                                   (Sender as TControl).Width,
                                   (Sender as TControl).Height,
                                    FParentWindow, HMENU(Nil), HInstance,
                                   nil);
                      Assert(False, 'Trace:CreateComponent - Button Window Handle Value = $' + IntToHex(LongInt(P), 8));
                      Assert(False, 'Trace:CreateComponent - Creating a Button - SetProp');
                      if P <> HWND(nil) then
                       SetProp(HWND(P), 'Lazarus', @Sender);
                      SetName(Pointer(p),StrTemp);
                     End;
  csCanvas         : Begin
                       Assert(False, 'TODO: Code TWin32Object.CreateComponent: style csCanvas');
                     End;
  csCheckbox       : Begin
                       CreateWindow('BUTTON', strTemp, WS_CHILD Or WS_VISIBLE Or BS_CHECKBOX, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
  csComboBox       : Begin
                      P := CreateWindow('COMBOBOX', Nil, WS_CHILD Or WS_VISIBLE Or CBS_AUTOHSCROLL Or CBS_DROPDOWN, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                      SendMessage(HWND(P), CB_ADDSTRING, 0, LPARAM(LPCTSTR(strTemp)));
                     End;
  csListBox        : Begin
                       P := CreateWindow('LISTBOX', Nil, WS_CHILD Or WS_VISIBLE, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Width, FParentWindow, HMENU(Nil), HInstance, Nil);
                       SendMessage(HWND(P), LB_ADDSTRING, 0, LPARAM(LPCTSTR(strTemp)));
                     End;
  csCListBox       : Begin
                       P := CreateWindow('LISTBOX', Nil, WS_CHILD Or WS_VISIBLE Or LBS_MULTICOLUMN, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Width, FParentWindow, HMENU(Nil), HInstance, Nil);
                       SendMessage(HWND(P), LB_SETCOLUMNWIDTH, WPARAM((Sender As TCListBox).Width Div ((Sender As TCListBox).ListColumns)), 0);
                       SendMessage(HWND(P), LB_ADDSTRING, 0, LPARAM(LPCTSTR(strTemp)));
                     End;
  csEdit           : Begin
                       P := CreateWindow('EDIT', strTemp, WS_CHILD OR WS_VISIBLE Or ES_AUTOHSCROLL, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
  csFileDialog     : Begin
                       Assert(False, 'TRACE: CreateComponent - Creating a file-selection dialog');
                       ZeroMemory(@OpenFile, SizeOf(OpenFile));
                       With OpenFile Do
                       Begin
                         LStructSize := SizeOf(OpenFile);
                         HWndOwner := ParentWindow;
                         LPStrFilter := Filter;
                         LPStrFile := StFl;
                         LPStrTitle := strTemp;
                         NMaxFile := Max_Path;
                         Flags := OFN_Explorer Or OFN_AllowMultiSelect Or OFN_CreatePrompt Or OFN_HideReadOnly Or OFN_OverwritePrompt;
                         LPStrDefExt := Ext;
                       End;
                       GetOpenFileName(@OpenFile);
                       Win32DialogOKclickedCB(Pointer(P), Sender);
                       Win32DialogOKclickedCB(Pointer(P), Sender);
                       Win32DialogOKclickedCB(Pointer(P), Sender);
                     End;
  csColorDialog    : Begin
                       Assert(False, 'TRACE: CreateComponent - Creating a color-selection dialog');
                       ZeroMemory(@ColorSelect, SizeOf(ColorSelect));
                       With ColorSelect Do
                       Begin
                         LStructSize := SizeOf(ColorSelect);
                         HWndOwner := ParentWindow;
                         RGBResult := RGBIO;
                         LPCustColors := LPDWord(@CustomColors);
                         Flags := CC_FullOpen Or CC_RGBInit;
                       End;
                       ChooseColor(@ColorSelect);
                       Win32DialogOKclickedCB(Pointer(P), Sender);
                       Win32DialogOKclickedCB(Pointer(P), Sender);
                       Win32DialogOKclickedCB(Pointer(P), Sender);
                     End;
  csFontDialog     :
                     Begin
                       Assert(False, 'TODO: Write code to make a font dialog when component style csFontDialog is specified.  No dialog created.');
                     End;
  csFixed          : Begin
                       Assert((@ParentWindow = Nil), 'TODO: Figure out what component style fsFixed is and code the component.  No component created.');
                     End;
  csFont           : Begin
                      Assert(False, 'Trace:CreateComponent - Creating a font');
                     End;
  csForm           : Begin
                      Assert(False, 'Trace:CreateComponent - Creating a Form Window');
                      P := CreateWindow(ClsName, strTemp, ws_OverlappedWindow,
                                                cw_usedefault, cw_usedefault,
                                                cw_usedefault, cw_usedefault,
                                                HWnd(Nil), HMENU(Nil), HInstance, nil);
                      FParentWindow := HWND(P);
                      Assert(False, 'Trace:CreateComponent - Form Window Handle Value = $' + IntToHex(LongInt(P), 8));
                      Assert(False, 'Trace:Creating a Form - SetProp');
                      SetProp(HWnd(P), 'Lazarus', @Sender);
                      If LongInt(P) = 0 then
                       Begin
                        Windows.MessageBox(0, 'csForm CreateWindow Failed', nil, mb_Ok);
                        Exit;
                       End;
                      SetName(Pointer(p), StrTemp);
                     End;
  csMainForm       : Begin
                      Assert(False, 'Trace:CreateComponent - Creating a MainForm for Win32 --------------------------------------');
                      pStr := StrAlloc(length('TForm') + 1);
                      StrPCopy(pStr, 'TForm');
                      P := CreateWindow(pStr, AppName, ws_OverlappedWindow,
                                                cw_usedefault, cw_usedefault,
                                                cw_usedefault, cw_usedefault,
                                                FParentWindow, HMENU(Nil), HInstance, nil);
                      FParentWindow := HWND(P);
                      StrDispose(pStr);
                      Assert(False, 'Trace:CreateComponent - MainForm Window Handle Value = $' + IntToHex(LongInt(P), 8));
                      Assert(False, 'Trace:Creating a Form - MainForm SetProp');
                      SetProp(HWnd(P), 'Lazarus', @Sender);
                      If LongInt(P) = 0 then
                       Begin
                        Windows.MessageBox(0, 'csMainForm CreateWindow Failed', nil, mb_Ok);
                        Exit;
                       End;
                      SetName(Pointer(p), StrTemp);
                     End;
  csFrame          : Begin
                       P := CreateWindowEx(Integer(Nil), 'BUTTON', strTemp, WS_CHILD Or WS_VISIBLE Or BS_GROUPBOX, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
  csLabel         : Begin
                       P := CreateWindow('STATIC', strTemp, WS_CHILD Or WS_VISIBLE Or SS_LEFT Or SS_SIMPLE, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
  csMemo           : Begin
                      Assert(False, 'Creating a MEMO...');
                      Flags := WS_CHILD Or WS_VISIBLE Or ES_AUTOHSCROLL Or ES_AUTOVSCROLL Or ES_MULTILINE Or ES_WANTRETURN;
                      If (Sender As TMemo).ReadOnly Then
                        Flags := Flags Or ES_READONLY;
                      Case (Sender As TCustomMemo).Scrollbars Of
                        ssHorizontal : Flags := Flags Or WS_HSCROLL;
                        ssVertical   : Flags := Flags Or WS_VSCROLL;
                        ssBoth       : Flags := Flags Or WS_HSCROLL  Or WS_VSCROLL;
                      End;
                      P := CreateWindow('EDIT', strTemp, Flags, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
  csMenuBar       : Begin
                       P := HWND(CreateMenu);
                       FMenu := HMENU(P);
                     End;
  csMenuItem       : Begin
                       Assert(False, 'Trace: CreateComponent - Creating component menu item.');
                       P := HWND(CreateMenu);
                     End;
  csNotebook       : Begin
                       P := CreateWindow(WC_TABCONTROL, Nil, WS_CHILD Or WS_VISIBLE Or WS_CLIPSIBLINGS, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
  csRadioButton    : Begin
                       P := CreateWindow('BUTTON', strTemp, WS_CHILD Or WS_VISIBLE Or BS_RADIOBUTTON, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
  csScrollBar      : Begin
                       Flags := WS_CHILD Or WS_Visible;
                       Case TScrollBar(Sender).Kind Of
                         sbHorizontal : Flags := Flags Or SBS_HORZ;
                         sbVertical   : Flags := Flags Or SBS_VERT;
                       End;
                       P := CreateWindow('SCROLLBAR', Nil, Flags, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
  csScrolledWindow : Begin
                       Assert(False, 'TRACE: CreateComponent - creating a scrolled window');
                       P := CreateWindow(AppName, strTemp, WS_OVERLAPPEDWINDOW Or WS_HSCROLL Or WS_VSCROLL Or WS_Visible, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, HWND(Nil), HMENU(Nil), HInstance, Nil);
                     End;
  csSpeedButton    : Begin
                       Assert(False, 'TODO: Code the speed button control');
                     End;
  csSpinEdit       : Begin
                      Assert(False, 'TODO: Create a spin edit control.  What is a spin edit contol anyway?');
                      //this needs to be created in the actual code because it requires a gtkadjustment Win32Control
                      Assert(False, 'TRACE: Spin edit control not created');
                     End;
  csSTATUSBAR      : Begin
                       Assert(False, 'TRACE: CreateComponent - Creating Status Bar');
                       CreateStatusWindow(WS_CHILD Or WS_VISIBLE, strTemp, FParentWindow, FControlIndex);
                       Inc(FControlIndex);
                     End;
  csgtkTable       : Begin
                       Assert(False, 'TODO: Create GTK Table (ie, spreadsheet) control.  I''m not sure how to do this, but I assume an array (or TList) of records containing the rows and columns and the properties(x, y, width, height, etc) of everything.  If you think you can help, be my guest.');
                       Assert(@Sender <> Nil, 'TRACE: GTK Table not created');
                     End;
  csToggleBox      : Begin
                       Assert(False, 'TRACE: CreateComponent - Creating toggle box');
                       P := CreateWindow('BUTTON', strTemp, WS_CHILD Or WS_VISIBLE Or BS_AUTOCHECKBOX Or BS_PUSHLIKE, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
  csToolBar        : Begin
                       P := CreateWindow(TOOLBARCLASSNAME, LPSTR(Nil), WS_CHILD Or WS_VISIBLE OR CCS_ADJUSTABLE, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
  csToolButton     : Begin
                       P := IntSendMessage3(LM_INSERTTOOLBUTTON, Sender, Pointer((Sender As TToolButton).Index));
                     End;
  csGroupBox       : Begin
                       Assert(False, 'TODO: Code csGroupBox.  Is this the same as csFrame?');
                     End;
  csPage           : Begin   // TPage - Notebook page 
                       Assert(False, 'TODO: Create a csPage component.');
                       Assert(False, 'Trace: Going to try it here.  I''m guaranteeing nothing.');
                       With tci Do
                       Begin
                         Mask := TCIF_TEXT;
                         PSzText := strTemp;
                       End;
                       TabCtrl_InsertItem(HWND((Sender As TWinControl).Handle), -1, tci);
                     End;
 csPopupMenu       : Begin
                       P := HWND(CreatePopupMenu);
                       FSubMenu := HMENU(P);
                     End;
 csProgressBar       : Begin
                       P := CreateWindow(PROGRESS_CLASS, strTemp, WS_CHILD Or WS_VISIBLE, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
 csTrackBar:         Begin
                       Assert(False, 'TRACE: CreateComponent - Creating a Track Bar (if we''re lucky)');
                       P := CreateWindowEx(DWord(Nil), strTemp, TRACKBAR_CLASS, WS_CHILD Or WS_VISIBLE, (Sender As TControl).Left, (Sender As TControl).Top, (Sender As TControl).Width, (Sender As TControl).Height, FParentWindow, HMENU(Nil), HInstance, Nil);
                     End;
 end; {Case}
 
   if (Sender is TWinControl) then 
   begin
     TWinControl(Sender).Handle := THandle(p);
     if p <> HWND(nil) 
        then FSender := Sender;
   end
   else if (Sender is TMenuItem)
     then TMenuItem(Sender).Handle := HMenu(p)
   else if (Sender is TMenu)
     then TMenu(Sender).Items.Handle := HMenu(p)
   else if (Sender is TCommonDialog)
     then TCommonDialog(Sender).Handle:= THandle(p);
  if (Sender is TControl) then
  Begin
   Assert(False, 'Trace:CreateComponent - Assigning window to TControl');
   (Sender as TWinControl).Handle := HWND(p);
  End
 else
  if (Sender is TControlCanvas) then
   Begin
    Assert(False, 'Trace:CreateComponent - Assigning window to TControlCanvas');
    (Sender as TControlCanvas).Handle := HWND(p);
   End
  else
   if (Sender is TFont) then
    Begin
     Assert(False, 'Trace:CreateComponent - Assigning P to TFont');
     (Sender as TFont).Handle := HWND(p);
    End;
 
 SetLCLObject(HWND(p), TObject(Sender));
 StrDispose(StrTemp);
 
 Assert(False, 'Trace:Leaving CreateComponent');
end;

{------------------------------------------------------------------------------}
{       TWin32Object GetLabel                                                    }
{       *Note: Returns a Win32Controls lable value                                   }
{------------------------------------------------------------------------------}
function TWin32Object.GetLabel(CompStyle: Integer; P : Pointer) : String;
Var
 Value : PChar;
 pLabel: Pointer;
 ValLen: Integer;
 Wnd   : HWND;

begin
 Assert(False, 'TRACE: [TWin32Object.GetLabel] getting label.');
 pLabel := Pointer((TObject(PObject(P)^) As TWinControl).Handle);
 Wnd := HWND(pLabel);
 ValLen := GetWindowTextLength(Wnd);
 GetWindowText(Wnd, @Value, (ValLen + 1));
 Result := Value;
end;

{------------------------------------------------------------------------------}
{       TWin32Object AssignSelf                                                  }
{       *Note: Assigns a pointer to self on a Win32Control                           }
{------------------------------------------------------------------------------}
procedure TWin32Object.AssignSelf(Child,Data : Pointer);
begin
  Assert(False, 'Trace: [TWin32Object.AssignSelf] Trying to code it.  It''s probably wrong.');
  SetProp(HWND(Child), 'Self', Data);
end;

{------------------------------------------------------------------------------}
{       TGtkObject ShowHide                                                    }
{       *Note: Show or hide a widget                                           }
{------------------------------------------------------------------------------}
procedure TWin32Object.ShowHide(Sender : TObject);
begin
   if TControl(Sender).Visible then begin
      Assert(False, 'Trace: [TWin32Object.ShowHide] Showing the window');
      ShowWindow(HWND(TWinControl(Sender).Handle), SW_SHOW);
      end
   else Begin
        Assert(False, 'TRACE: [TWin32Object.ShowHide] Hiding the window');
        ShowWindow(HWND(TWinControl(Sender).Handle), SW_HIDE);
        end;
end;

procedure TWin32Object.AddNBPage(Parent, Child: TObject; Index: Integer);
var
  pStr: PCHar;
  Wnd: HWND;
  tci: TC_ITEM;
begin
  Assert(false, 'Trace:Adding a notebook page');

  pStr := StrAlloc(Length(TWinControl(Child).Caption) + 1);
  try
    StrPCopy(pStr, TWinControl(Child).Caption);

  With tci Do
  Begin
    Mask := TCIF_TEXT;
    PSzText := pStr;
  End;
  SendMessage((Parent As TWinControl).Handle, TCM_INSERTITEM, Index, LPARAM(@tci));
  SetParent((Child As TWinControl).Parent.Handle, (Parent As TWinControl).Handle);
  TControl(Child).Visible := True;
  ShowHide(TObject(Child));
  finally
    strDispose(pStr);
  end;

end;

{------------------------------------------------------------------------------}
{       TGtkObject RemoveNBPage                                                }
{       *Note: Remove Notebook Page                                            }
{------------------------------------------------------------------------------}
procedure TWin32Object.RemoveNBPage(Parent: TObject; Index: Integer);
begin
   Assert(false, 'Trace:Removing a notebook page');
   SendMessage((Parent As TWinControl).Handle, TCM_DELETEITEM, WPARAM(Index), 0);
end;

{------------------------------------------------------------------------------}
{       TGtkObject ReDraw                                                      }
{       *Note:                                                                 }
{------------------------------------------------------------------------------}
procedure TWin32Object.ReDraw(Child : Pointer);
begin
  Assert(False, 'TRACE: [TWin32Object.ReDraw] Redrawing...');
  Assert(False, 'TRACE: Invalidating the window');
  IntSendMessage3(LM_INVALIDATE, TObject(Child), Nil);
  Assert(False, 'TRACE: Updating the window');
  UpdateWindow(HWND(Child));
  Assert(False, 'TRACE: [TWin32Object.ReDraw] Finished redrawing');
end;

procedure TWin32Object.SetPixel(Sender : TObject; Data : Pointer);
type
  tbmi = array[0..SizeOf(Integer)] of char;
var
  bm: Windows.BITMAP;
  bmi: tbmi;
  DC: HDC;
begin
  Assert(False, 'TODO: Implement TWin32Object.SetPixel');
  DC := GetDC((Sender As TWinControl).Handle);
  Windows.SetPixel(DC, Lo(Integer(Data)), Hi(Integer(Data)), (Sender As TWinControl).Color);
  ReleaseDC((Sender As TWinControl).Handle, DC);
end;

procedure TWin32Object.GetPixel(Sender : TObject; Data : Pointer);
begin
  Assert(False, 'TODO: Implement TWin32Object.GetPixel');
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.GetValue
  Params:  Sender : the lcl object which called this func via SenMessage
           Data   : pointer to component specific variable
  Returns: currently always 0

  Depending on the compStyle, this function will get the current value
  of a GTK object and save it in the variable referenced by 'data'.
  
  This function should be used to synchronize the state of an lcl-object
  with the corresponding GTK-object.
 ------------------------------------------------------------------------------}
function TWin32Object.GetValue (Sender : TObject; data : pointer) : integer;
var
   Handle    : Pointer;
begin
   result := 0;     // default if nobody sets it

   if Sender is TWinControl 
     then Assert(False, Format('Trace:  [TgtkObject.GetValue] %s', [Sender.ClassName]))
     else Assert(False, Format('Trace:WARNING: [TgtkObject.GetValue] %s --> No Decendant of TWinControl', [Sender.ClassName]));

   Handle    := Pointer(TWinControl(Sender).Handle);
   Assert (Handle = nil, 'WARNING: [TgtkObject.GetValue] --> got nil pointer (no gtkobject)');
   
   case TControl(Sender).fCompStyle of
      csTrackbar   :
          if (handle <> nil) then begin
            integer(data^) := round(SendMessage(HWND(handle), TBM_GETRANGEMAX, 0, 0) - SendMessage(HWND(handle), TBM_GETRANGEMIN, 0, 0));
          end else
            integer(data^) := 0;


      csRadiobutton,
      csCheckbox   :
        if SendMessage(HWND(handle), BM_GETSTATE, 0, 0) = BST_CHECKED then
          TCheckBoxState(data^) := cbChecked;
        else if SendMessage(HWND(handle), BM_GETSTATE, 0, 0) = BST_UNCHECKED then
          TCheckBoxState(data^) := cbUnChecked
   else
      Assert (true, Format ('WARNING:[TgtkObject.GetValue] failed for %s', [Sender.ClassName]));
   end;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.SetValue
  Params:  Sender : the lcl object which called this func via SenMessage
           Data   : pointer to component specific variable
  Returns: currently always 0

  Depending on the compStyle, this function will apply the parameter 'data'
  to the GTK object repesenting the lcl-object which called the function.

  This function should for be used in cases where the most common property
  of an object has changed (e.g. the position of a trackbar). If more than
  one property changed use the SetProperties function instead;
 ------------------------------------------------------------------------------}
function TWin32Object.SetValue (Sender : TObject; data : pointer) : integer;
var
   Handle    : Pointer;
begin
   result := 0;     // default if nobody sets it

   if Sender is TWinControl 
     then Assert(False, Format('Trace:  [TgtkObject.SetValue] %s', [Sender.ClassName]))
     else Assert(False, Format('Trace:WARNING: [TgtkObject.SetValue] %s --> No Decendant of TWinControl', [Sender.ClassName]));

   Handle    := Pointer(TWinControl(Sender).Handle);
   Assert (Handle = nil, 'WARNING: [TgtkObject.SetValue] --> got nil pointer (no gtkobject)');
   
   case TControl(Sender).fCompStyle of
      csProgressBar: SendMessage(HWND(Handle), PBM_SETPOS, WPARAM(data^), 0);
   
      csTrackbar   : begin
                        if Handle = nil then Exit;
                        Assert(False, 'TRACE: Setting the track bar value.');
                        SendMessage(HWND(Handle), TBM_SETPOS, WPARAM(TRUE), LPARAM(data^));
                     end;

      csRadiobutton,
      csCheckbox   : begin
                       if TCheckBoxState (data^) = cbChecked
                         then SendMessage(HWND(Handle), BM_SETCHECK, BST_CHECKED, 0)
                       else SendMessage(HWND(Handle), BM_SETCHECK, BST_UNCHECKED, 0);
                     end;
   else
      Assert (true, Format ('WARNING:[TgtkObject.SetValue] failed for %s', [Sender.ClassName]));
   end;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.SetProperties
  Params:  Sender : the lcl object which called this func via SenMessage
  Returns: currently always 0

  Depending on the compStyle, this function will apply all properties of
  the calling object to the corresponding GTK object.
 ------------------------------------------------------------------------------}
function TWin32Object.SetProperties (Sender : TObject) : integer;
var
   Handle     : Pointer;
   Control    : HWND;
   xAlign     : Integer;
   yAlign     : Integer;
   Style      : DWORD;
begin
   result := 0;     // default if nobody sets it

   if Sender is TWinControl 
     then Assert(False, Format('Trace:  [TgtkObject.SetProperties] %s', [Sender.ClassName]))
     else Assert(False, Format('Trace:WARNING: [TgtkObject.SetProperties] %s --> No Decendant of TWinControl', [Sender.ClassName]));

   Handle    := Pointer(TWinControl(Sender).Handle);
   Assert (Handle = nil, 'WARNING: [TgtkObject.SetProperties] --> got nil pointer');
   
   case TControl(Sender).fCompStyle of
      csEdit :
          with (TCustomEdit(Sender)) do
             Begin
               SendMessage(HWND(Handle), EM_SETREADONLY, WPARAM(ReadOnly), 0);
               SendMessage(HWND(Handle), EM_LIMITTEXT, MaxLength, 0);
             end;

      csProgressBar :
         with (TProgressBar (Sender)) do
         begin
            SendMessage(HWND(Handle), PBM_SETRANGE, 0, MakeLParam(SendMessage(HWND(Handle), PBM_GETRANGE, Ord(True), LPARAM(Nil)), LPARAM(Nil)));
            SendMessage(HWND(Handle), PBM_SETPOS, Position, 0);
            if Smooth 
               then SetWindowLong(HWND(Handle), GWL_STYLE, GetWindowLong(HWND(Handle), GWL_STYLE) Or PBS_SMOOTH)
               else SetWindowLong(HWND(Handle), GWL_STYLE, GetWindowLong(HWND(Handle), GWL_STYLE) Or Not PBS_SMOOTH);
                    //gtk_progress_bar_set_bar_style (GTK_PROGRESS_BAR (handle), GTK_PROGRESS_DISCRETE);
            case Orientation of
               pbVertical      : SetWindowLong(HWND(Handle), GWL_STYLE, PBS_VERTICAL);
               pbRightToLeft   : Begin
                                   Assert(False, 'TRACE: TRYING to create a right-to-left progress bar');
                                   SetWindowLong(HWND(Handle), GWL_EXSTYLE, GetWindowLong(HWND(Handle), GWL_EXSTYLE) Or Not WS_EX_LTRREADING);
                                 End;
               pbTopDown       : Begin
                                   Assert(False, 'TRACE: TRYING to create a vertical, top-to-bottom progress bar');
                                   SetWindowLong(HWND(Handle), GWL_STYLE, GetWindowLong(HWND(Handle), GWL_STYLE) Or PBS_VERTICAL);
                                   SetWindowLong(HWND(Handle), GWL_EXSTYLE, GetWindowLong(HWND(Handle), GWL_EXSTYLE) Or Not WS_EX_LTRLEADING);
                                 End;
            else { pbHorizontal is default }
            begin
               SetWindowLong(HWND(Handle), GWL_STYLE, GetWindowLong(HWND(Handle), GWL_STYLE) Or Not PBS_VERTICAL);
               SetWindowLong(HWND(Handle), GWL_EXSTYLE, GetWindowLong(HWND(Handle), GWL_EXSTYLE) Or WS_EX_LTRLEADING);
              end;
            end;
            if BarShowText then 
            begin
               SetWindowText(HWND(Handle), StrToPChar((Sender As TControl).Caption));
            end
            else
               SetWindowText(HWND(Handle), Nil);
         end;


      csScrollBar:
         with (TScrollBar (Sender)) do
             begin
               SendMessage(HWND(Handle), SBM_SETRANGE, Min, Max);
               SendMessage(HWND(Handle), SBM_SETPOS, Position, LPARAM(Ord(True)));
               Assert(False, 'TODO: [TWin32Object.SetProperties] Set up step and page increments for csScrollBar');
             end;
   
      csTrackbar   : 
         with (TTrackBar (Sender)) do
         begin
            SendMessage(HWND(Handle), TBM_SETRANGEMIN, Ord(True), Min);
            SendMessage(HWND(Handle), TBM_SETRANGEMIN, Ord(True), Min);
            SendMessage(HWND(Handle), TBM_SETPOS, Ord(True), Position);
            SendMessage(HWND(Handle), TBM_SETLINESIZE, 0, LineSize);
            SendMessage(HWND(Handle), TBM_SETPAGESIZE, 0, PageSize);

            if ShowScale then
            begin
               case ScalePos of
                  trLeft  : SetWindowLong(HWND(Handle), GWL_STYLE, GetWindowLong(HWND(Handle), GWL_STYLE) Or TBS_LEFT Or TBS_VERT);
                  trRight : SetWindowLong(HWND(Handle), GWL_STYLE, GetWindowLong(HWND(Handle), GWL_STYLE) Or TBS_RIGHT Or TBS_VERT);
                  trTop   : SetWindowLong(HWND(Handle), GWL_STYLE, GetWindowLong(HWND(Handle), GWL_STYLE) Or TBS_TOP Or TBS_HORZ);
                  trBottom: SetWindowLong(HWND(Handle), GWL_STYLE, GetWindowLong(HWND(Handle), GWL_STYLE) Or TBS_BOTTOM Or TBS_HORZ);
               end;
            end;
            //Not here (Delphi compatibility)
         end;

      csLabel :
         with TLabel(Sender) do
         begin
           case Alignment of
             taLeftJustify  : Style := Style Or SS_LEFT;
             taCenter       : Style := Style Or SS_CENTER;
             taRightJustify : Style := Style Or SS_CENTER;
           else 
             Style := STYLE Or SS_LEFT; // default, shouldn't happen
           end;

           case Layout of
             tlTop    : Style := Style Or BS_TOP;
             tlCenter : Style := Style Or BS_VCENTER;
             tlBottom : Style := Style Or BS_BOTTOM;
           else 
             Style := Style Or BS_BOTTOM; //default, shouldn't happen
           end;

           SetWindowLong(HWND(Handle), GWL_STYLE, GetWindowLong(HWND(Handle), GWL_STYLE) Or Style);
           Assert(False, 'TRACE: Wordwrapping of labels is not currently implemented');
           Assert(False, 'TODO: Code wordwrapping labels');
         end;
      else
         Assert (true, Format ('WARNING:[TgtkObject.SetProperties] failed for %s', [Sender.ClassName]));
   end;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.UpdateHint
  Params:  Sender : the lcl object which called this func 
  Returns: currently always 0

  Sets the tooltip text of the sending control.
 ------------------------------------------------------------------------------}
function TWin32Object.UpdateHint(Sender: TObject) : integer;
var
  StrTemp : PChar;
  ti: TOOLINFO;
begin
  Result := 0;     // default if nobody sets it
  if Sender is TWinControl then
  with Sender as TWinControl do
  begin
    if (Length(Hint) > 0) and (ShowHint or (csDesigning in ComponentState))
    then begin
      strTemp := StrAlloc(Length(Hint) + 1);
      try
        StrPCopy(strTemp, Hint);
        // ?? TODO something with short and long hints ??
        Assert(False, 'TRACE: TRYING to update the hint');
        With ti Do
        Begin
          cbSize := SizeOf(ti);
          hwnd := Handle;
          lpSzText := strTemp;
        End;
        Assert(False, 'TRACE: Updating the hint to ' + StrPas(strTemp));
        SendMessage(FToolTipWindow, TTM_UPDATETIPTEXT, 0, LPARAM(@ti));
      finally
        StrDispose(strTemp);
      end;
    end 
    else
    begin
      With ti Do
      Begin
        cbSize := SizeOf(ti);
        hwnd := Handle;
        lpSzText := Nil;
      End;
      SendMessage(FToolTipWindow, TTM_UPDATETIPTEXT, 0, LPARAM(@ti));
    end;
  end;
end;

{------------------------------------------------------------------------------
  Method: TGtkObject.AttachMenu
  Params:  Sender : the lcl object which called this func 
  Returns: nothing

  Attaches the calling Menu to its Parent
 ------------------------------------------------------------------------------}
procedure TWin32Object.AttachMenu(Sender: TObject);
var
  AccelKey: Integer;
  AccelGroup: HACCEL;
  MenuParent, MenuItem: HMENU;
begin
  Assert(False, 'TODO: [TWin32Object.AttachMenu] Get accelerators up');
  with (Sender as TMenuItem) do
  begin
    MenuItem := Handle;
    
    if (Parent.GetParentMenu <> nil) and
       (Parent.GetParentMenu.Items.IndexOf(TMenuItem(Sender)) <> -1) then //mainmenu
    begin
      MenuParent := Parent.Handle;
      AppendMenu(Parent.Handle, MF_POPUP, Handle, StrToPChar(Caption));
      SetMenu(FParentWindow, Parent.Handle);
      DrawMenuBar(FParentWindow);
    end
    else begin
      // find the menu container
      MenuParent := HMENU(GetProp(Parent.Handle, 'ContainerMenu'));
      if MenuParent = HMENU(Nil) then
      begin
        MenuParent := CreateMenu;
        SetProp(Parent.Handle, 'ContainerMenu', Pointer(MenuParent));
        
        SetMenu(Parent.Handle, MenuParent);
        
        AccelGroup := CreateAcceleratorTable(LPACCEL(Nil), 0);
      end;
      Windows.AppendMenu(HMENU(HMENU(Nil)), MF_POPUP, FSubMenu, StrToPChar(Caption));
    end;

    AccelGroup := GetAccelGroup(MenuParent);
    AccelKey := GetAccelKey(MenuItem);
    if (AccelGroup <> HAccel(nil)) and (AccelKey <> 0)
       then begin
       end;
  end;
end;


{------------------------------------------------------------------------------
  Function: IsValidDC
  Params:  DC: a (LCL) devicecontext
  Returns: True if valid

  Checks if the given DC is valid.
 ------------------------------------------------------------------------------}
function TWin32Object.IsValidDC(const DC: HDC): Boolean;
begin
  Result := FDeviceContexts.Contains(Pointer(DC));
  Assert(False, Format('Trace:  [TgtkObject.IsValidDC] DC: 0x%x --> %s', [Integer(DC), BOOL_RESULT[Result]]));
end;

{------------------------------------------------------------------------------
  Function: IsValidGDIObject
  Params:  GDIObject: a (LCL) gdiObject
  Returns: True if valid

  Checks if the given GDIObject is valid
 ------------------------------------------------------------------------------}
function TWin32Object.IsValidGDIObject(const GDIObject: HGDIOBJ): Boolean;
begin
  Result := FGDIObjects.Contains(Pointer(GDIObject));
  if Result then
  try
    with PGdiObject(GDIObject)^ do
      case GDIType of
        gdiBitmap : begin
           case GDIBitmapType of
             gbPixmap: Result := GDIPixmapObject <> Integer(nil);
             gbBitmap: Result := GDIBitmapObject <> Integer(nil);
             gbImage:  Result := GDIRawImageObject <> nil;
           else
             Result := False; 
           end;
        end;
        gdiBrush  : Result := True; //Result := GDIBrushPixmap <> nil; //GDIBrushPixmap may be nil
        gdiFont   : Result := GDIFontObject <> Integer(nil);
        gdiPen    : Result := True;
      else
        Result := False; 
      end;
  except
    on Exception do Result := False;
  end;
  Assert(False, Format('Trace:  [TgtkObject.IsValidGDIObject] GDIObject: 0x%x --> %s', [Integer(GDIObject), BOOL_RESULT[Result]]));
end;

{------------------------------------------------------------------------------
  Function: IsValidGDIObjectType
  Params:  GDIObject: a (LCL) gdiObject
           GDIType: the requested type
  Returns: True if valid

  Checks if the given GDIObject is valid and the GDItype is the requested type
 ------------------------------------------------------------------------------}
function TWin32Object.IsValidGDIObjectType(const GDIObject: HGDIOBJ; const GDIType: TGDIType): Boolean;
begin
  Result := IsValidGDIObject(GDIObject) and (PGdiObject(GDIObject)^.GDIType = GDIType);
end;

{------------------------------------------------------------------------------
  Function: NewDC
  Params:  none
  Returns: a gtkwinapi DeviceContext

  Creates an initial DC
 ------------------------------------------------------------------------------}
function TWin32Object.NewDC: PDeviceContext;
begin
  Assert(False, Format('Trace:> [TgtkObject.NewDC]', []));
  New(Result);
  with Result^ do
  begin
    hWnd := 0;
    GC := ULONG(nil);
    Drawable := nil;
    PenPos.X := 0;
    PenPos.Y := 0;
    CurrentBitmap := nil;
    CurrentFont := nil;
    CurrentPen := nil;
    CurrentBrush := nil;
    SavedContext := nil;
    CurrentTextColor := 0;
    CurrentBackColor := $FFFFFF;
  end;
  FDeviceContexts.Add(Result);
end;

{------------------------------------------------------------------------------
  Function: NewGDIObject
  Params:  none
  Returns: a gtkwinapi DeviceContext

  Creates an initial DC
 ------------------------------------------------------------------------------}
function TWin32Object.NewGDIObject(const GDIType: TGDIType): PGdiObject;
begin
  Assert(False, Format('Trace:> [TgtkObject.NewGDIObject]', []));
  New(Result);
  FillChar(Result^, SizeOf(TGDIObject), 0);
  Result^.GDIType := GDIType;
  FGDIObjects.Add(Result);
  Assert(False, Format('Trace:< [TgtkObject.NewGDIObject] FGDIObjects --> 0x%p', [Result]));
end;

{------------------------------------------------------------------------------
  Function: CreateDefaultBrush
  Params:  none
  Returns: a Brush GDIObject

  Creates an default brush, used for initial values
 ------------------------------------------------------------------------------}
function TWin32Object.CreateDefaultBrush: PGdiObject;
begin
  Result := NewGDIObject(gdiBrush);
  Result^.GDIBrushFill := RGB(255, 255, 255);
  CreateSolidBrush(Result^.GDIBrushFill);
end;

{------------------------------------------------------------------------------
  Function: CreateDefaultFont
  Params:  none
  Returns: a Font GDIObject

  Creates an default font, used for initial values
 ------------------------------------------------------------------------------}
function TWin32Object.CreateDefaultFont: PGdiObject;
type
  tfontarr = Array[0..63] Of Byte;
  pFontArr = ^tfontarr;
var
  elf: Windows.ENUMLOGFONTEX;
  fn: PChar;
  lf: TAGLOGFONTA;
begin
  With lf Do
  Begin
    lfWeight := FW_BOLD;
    lfCharSet := DEFAULT_CHARSET;
    lfFaceName := 'Helvetica';
  End;
  Result := NewGDIObject(gdiFont);
  Result^.GDIFontObject := CreateFontIndirect(lf);
end;

{------------------------------------------------------------------------------
  Function: CreateDefaultPen
  Params:  none
  Returns: a Pen GDIObject

  Creates an default pen, used for initial values
 ------------------------------------------------------------------------------}
function TWin32Object.CreateDefaultPen: PGdiObject;
Var
 GO: PGDIObject;
begin
  New(GO);
  Result := GO;
  With Result^ Do
  Begin
    GDIPenStyle := PS_SOLID;
    GDIPenColor := RGB(0, 0, 0);
    CreatePen(GDIPenStyle, (PS_DASH + 1), GDIPenColor);
  End;
  Dispose(GO);
end;



{------------------------------------------------------------------------------}
{       TWin32Object SetName                                                     }
{       *Note: Sets the gtk engine internal name for a Win32Control                  }
{------------------------------------------------------------------------------}
procedure TWin32Object.SetName(Child,Data : Pointer);
begin
  SetProp(HWND(Child), 'Name', Data);
end;

{------------------------------------------------------------------------------}
{       TWin32Object ShowHide                                                    }
{       *Note: Show or hide a Win32Control                                           }
{------------------------------------------------------------------------------}
procedure TWin32Object.ShowHide(CompStyle : Integer; P : Pointer ; visible : boolean);
begin
 Assert(False, 'Trace:ShowHide - Start');
 Assert(False, 'Trace:ShowHide - Value of Pointer P = $' + IntToHex(LongInt(P), 8));
 If LongInt(P) <> 0 then
  Begin
   if visible then
     ShowWindow(HWnd(P), SW_SHOW)
   else
     ShowWindow(HWnd(P), SW_HIDE);
  End;
 Assert(False, 'Trace:ShowHide - End');
end;

{------------------------------------------------------------------------------}
{       TWin32Object AddNBPage                                                   }
{       *Note: Add Notebook Page                                               }
{------------------------------------------------------------------------------}
procedure TWin32Object.AddNBPage(Parent,Child : Pointer; Index: Integer);
begin
 Assert(false, 'Trace:AddNBPage - Start, Adding a notebook page');
end;

{------------------------------------------------------------------------------}
{       TWin32Object RemoveNBPage                                                }
{       *Note: Remove Notebook Page                                            }
{------------------------------------------------------------------------------}
procedure TWin32Object.RemoveNBPage(Parent,Child : Pointer; Index: Integer);
begin
 Assert(false, 'Trace:RemoveNBPage - Removing a notebook page');
end;

{------------------------------------------------------------------------------}
{       TWin32Object DrawFillRect                                                }
{       *Note: Draws a filled rectangle on the child Win32Control                    }
{------------------------------------------------------------------------------}
Procedure TWin32Object.DrawFillRect(Child,Data : Pointer);
Var
  R: Windows.RECT;
  Wnd: HWND;
Begin
  Wnd := HWND((TObject(PObject(Child)^) As TWinControl).Handle);
  Windows.GetClientRect(Wnd, @R);
  Windows.FillRect(GetDC(Wnd), R, PGDIObject(Data)^.GDIPenColor);
end;

{------------------------------------------------------------------------------}
{       TWin32Object DrawRect                                                    }
{       *Note: Draw Rectangle on child Win32Control                                  }
{------------------------------------------------------------------------------}
Procedure TWin32Object.DrawRect(Child,Data : Pointer);
Var
  R: Windows.RECT;
  Wnd: HWND;
Begin
  Wnd := (TObject(PObject(Child)^) As TWinControl).Handle;
  Windows.GetClientRect(Wnd, @R);
  Windows.Rectangle(GetDC(Wnd), R.Left, R.Top, R.Right, R.Bottom);
end;

{------------------------------------------------------------------------------}
{       TWin32Object DrawLine                                                    }
{       *Note: DrawLine on child Win32Control                                        }
{------------------------------------------------------------------------------}
Procedure TWin32Object.DrawLine(Child,Data : Pointer);
Begin
  Assert(False, 'TODO: Code DrawLine');
end;

{------------------------------------------------------------------------------}
{       TWin32Object DrawText                                                    }
{       *Note: Draws Text on Child Win32Control                                      }
{------------------------------------------------------------------------------}
Procedure TWin32Object.DrawText(Child, Data : Pointer);
Var
  PStr: PChar;
  R: Windows.Rect;
  Wnd: HWND;
Begin
  PStr := StrToPChar((TObject(PObject(Child)^) As TWinControl).Caption);
  Wnd := (TObject(PObject(Child)^) As TWinControl).Handle;
  Windows.GetClientRect(Wnd, @R);
  Windows.DrawText(GetDC(Wnd), PStr, Length(StrPas(PStr)), @R, UINT(Data));
end;

Procedure TWin32Object.GetFontinfo(Sender : TObject; Data : Pointer);
Begin
  Assert(False, 'TODO: Code TWin32Object.GetFontinfo.');
End;

Procedure TWin32Object.MessageBox(Message, Title: String; Flags: Cardinal);
Begin
  Windows.MessageBox(FParentWindow, StrToPChar(Message), StrToPChar(Title), Flags);
End;

 {
  $Log$
  Revision 1.2  2001/08/02 12:58:35  lazarus
  MG: win32 interface patch from Keith Bowes

  Revision 1.1  2000/07/13 10:28:31  michael
  + Initial import

  Revision 1.1  2000/03/30 22:53:37  lazarus
  MWE:
    Moved form ../..

  Revision 1.2  1999/08/19 01:13:22  lazarus
  Makes button visible, still no OnClick behavior

 }
