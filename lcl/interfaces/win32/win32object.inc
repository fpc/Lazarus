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
  csgLabel         : Result := 'csgLabel';
  csMemo           : Result := 'csMemo';
  csMenu           : Result := 'csMenu';
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
  csTimer          : Result := 'csTimer';
  csPage           : Result := 'csPage';
 end; {Case}
End;

Function LM_ToString(LM_Message: Integer): String;
Begin
 Case LM_Message of
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
          
{------------------------------------------------------------------------------}
{       Window Handler                                                           }
{------------------------------------------------------------------------------}
Function WindowProc( Window: HWnd; AMessage, WParam,
                     LParam: LongInt): LongInt; stdcall; export;
Var
 DC   : HDC;
 PS   : PaintStruct;
 R    : TRect;
 DataHandle: Handle;
 OwnerObject : Pointer;
 LMessage : Integer;
Begin
 Assert(False, 'Trace:WindowProc - Start');
 WindowProc := 0;
 Assert(False, 'Trace:WindowProc - Getting Object With Callback Procedure');
 OwnerObject := Pointer(GetProp(Window, 'Lazarus'));
 Assert(False, 'Trace:WindowProc - Checking Proc');
 Assert(False, 'Trace:WindowProc - Value of OwnerObject is $' + IntToHex(LongInt(OwnerObject), 8));
 Assert(False, 'Trace:WindowProc - Value of AMessage is ' + WM_ToString(AMessage));
 Case AMessage of
  wm_create     : Begin
                   LMessage := LM_CREATE;
                   Assert(False, 'Trace:WindowProc - Got wm_create');
                   If LongInt(OwnerObject) <> 0 then
                    TControl(OwnerObject).Dispatch(LMessage);
                   Exit;
                  End;
  wm_Destroy    : Begin
                   Assert(False, 'Trace:WindowProc - Got wm_destroy');
                   RemoveProp(Window, 'Lazarus');
                   LMessage := LM_DESTROY;
                   If LongInt(OwnerObject) <> 0 then
                    TControl(OwnerObject).Dispatch(LMessage);
                   PostQuitMessage(0);
                   Exit;
                  End;
  wm_showwindow : Begin
                   Assert(False, 'Trace:WindowProc - Got wm_showwindow');
                   LMessage := LM_SHOW;
                   If LongInt(OwnerObject) <> 0 then
                    TControl(OwnerObject).Dispatch(LMessage);
                   Exit;
                  End;
  WM_NCLBUTTONDOWN : Begin
                      Assert(False, 'Trace:WindowProc - Got WM_NCLBUTTONDOWN');
                      LMessage := LM_CLICKED;
                      Win32ClickedCB(OwnerObject, AMessage, wParam, lParam);
{!}//                      If LongInt(DataHandle) <> 0 then
{!}//                       TControl(DataHandle).Dispatch(LMessage);
                      Exit;
                     End;
 End; {Case}
 WindowProc := DefWindowProc(Window, AMessage, WParam, LParam);
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
   cbClsExtra    := 0;
   cbWndExtra    := 0;
   hInstance     := system.MainInstance;
   hIcon         := LoadIcon(0, idi_Application);
   hCursor       := LoadCursor(0, idc_Arrow);
   hbrBackground := GetStockObject(GRAY_BRUSH);
   lpszMenuName  := nil;
   lpszClassName := AppName;
  End;
 Result := Windows.RegisterClass(WindowClass) <> 0;
 Assert(False, 'Trace:WinRegister - Exit');
End;

{------------------------------------------------------------------------------}
{       TWin32Object HandleEvents                                                }
{       *Note: Passes execution control to the GTK engine                      }
{------------------------------------------------------------------------------}
procedure TWin32Object.HandleEvents;
Var
 AMessage : Msg;
begin
 While GetMessage(@AMessage, 0, 0, 0) do
  Begin
   TranslateMessage(AMessage);
   DispatchMessage(AMessage);
  End;
end;

{------------------------------------------------------------------------------}
{       TWin32Object HandleEvents                                                }
{       *Note: Tells GTK Engine to process pending events                      }
{------------------------------------------------------------------------------}
procedure TWin32Object.DoEvents;
begin
 
end;

{------------------------------------------------------------------------------}
{       TWin32Object AppTerminate                                                }
{       *Note: Tells GTK Engine to halt and destroy                            }
{------------------------------------------------------------------------------}
procedure TWin32Object.AppTerminate;
begin
 StrDispose(FormClassName);
end;

{------------------------------------------------------------------------------}
{       TWin32Object Init                                                        }
{       *Note: Initializes GTK Engine                                          }
{------------------------------------------------------------------------------}
procedure TWin32Object.Init;

Var
 AMessage : Msg;
 hWindow  : HWnd;

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
 Assert(False, 'Trace:Win32Object.Init - Exit');
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
   pStr : PChar;
   AOwner : TControl;
Begin
 Result := 0;   //default value just in case nothing sets it
 Assert(False, 'Trace:IntSendMessage3 - Start, Received (' + LM_ToString(LM_Message) + ')');
 Assert(False, 'Trace:IntSendMessage3 - Value of Sender = $' + IntToHex(LongInt(Sender), 8));
 Case LM_Message of
  LM_Create        : Begin
                      Assert(False, 'Trace:IntSendMessage3 - Calling CreateComponent');
                      CreateComponent(Sender);
                      Assert(False, 'Trace:IntSendMessage3 - Called CreateComponent');
                      if (Sender is TControl) then
                       Begin
                        If (Sender as TControl).Caption <> '' then
                         SetLabel((Sender as TControl).FCompStyle, (Sender as TControl).FComponent, (Sender as TControl).Caption);
                       End;
                     End;
  LM_AddChild      : Begin
                      Assert(False, 'Trace:IntSendMessage3 - Adding a child to Parent');
                      AOwner := TControl((Sender as TControl).Owner);
                      AddChild(AOwner.fComponent, (Sender as TControl).fComponent, AOwner.Left, AOwner.Top);
                     End;
  LM_ShowHide      : ShowHide((Sender as TControl).fCompStyle, (Sender as TControl).FComponent, (Sender as TControl).Visible);
  LM_ShowModal     : Begin
                     End;
  LM_DESTROY       : Begin
                      If (Sender is TTimer) then
                       Begin
                        Assert(False, 'Trace:IntSendMessage3 - removing timer!!!');
                        // remove timer here
                       End
                      Else
                       Begin
                        Assert(False, 'Trace:IntSendMessage3 - Destroying a window');
                        DestroyWindow(Hwnd((Sender as TControl).FComponent));
                       End;
                     End;
  LM_FONTGETSIZE   : GetFontInfo(Sender, Data);
  LM_SETCOLOR      : SetColor(Sender);
  LM_DrawFillRect  : DrawFillRect((Sender as TCanvas).fComponent, Data);
  LM_DrawRect      : DrawRect((Sender as TCanvas).fComponent, Data);
  LM_DrawLine      : DrawLine((Sender as TCanvas).fComponent, Data);
  LM_DrawText      : DrawText((Sender as TCanvas).fComponent, Data);
  LM_ReDraw        : ReDraw((Sender as TCanvas).fComponent);
  LM_SETFILTER     : Begin
                      pStr := StrAlloc(length(TFileDialog(Sender).Filter) + 1);
                      StrPCopy(pStr, TFileDialog(Sender).Filter);
                      StrDispose(pStr);
                     End;
  LM_SETFILENAME   : Begin
                      pStr := StrAlloc(length(TFileDialog(Sender).Filename) + 1);
                      StrPCopy(pStr, TFileDialog(Sender).Filename);
                      // Set file name here
                      StrDispose(pStr);
                     End;
  LM_SetText       : SetText((Sender as TControl).fComponent, Data);

  { /START These messages were added by Michal Bukovjan }

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
  LM_SETSELSTART   : Begin
                     End;
  LM_SETSELLEN     : Begin
                     End;
  LM_GetLineCount  : ;
  LM_GETSELCOUNT   : Begin
                     End;
  LM_GETSEL        : Begin
                     End;
  LM_SETLIMITTEXT  : Begin
                     End;
  LM_SORT          : Begin
                     End;
  LM_SETSEL        : Begin
                     End;
  LM_SETSELMODE    : Begin
                     End;
 End; {Case}
 Assert(False, 'Trace:IntSendMessage3 - Exit');
end;

{------------------------------------------------------------------------------}
{       TWin32Object IntSendMessgae                                              }
{       *Note: Message Processing                                              }
{------------------------------------------------------------------------------}
Procedure TWin32Object.IntSendMessage(LM_Message : Integer; CompStyle : Integer; Var P : Pointer; Val1 : Integer; Var Str1 : String);
Begin
 Assert(False, 'Trace:IntSendMessage - Start, Received (' + LM_ToString(LM_Message) + ')');
 Case LM_Message of
  LM_SetLabel : SetLabel(CompStyle, P, Str1);
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
procedure TWin32Object.ResizeChild(Parent,Child: Pointer;Left,Top,Width,Height : Integer);
begin
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
begin
end;

{------------------------------------------------------------------------------}
{       TWin32Object SetLabel                                                    }
{       *Note: Sets the label text on a widget                                 }
{------------------------------------------------------------------------------}
procedure TWin32Object.SetLabel(CompStyle : Integer; Var P : Pointer; Str1 : String);
var pLabel: pchar;
begin
 pLabel := StrAlloc(length(Str1) + 1);
 StrPCopy(pLabel,Str1);
 Case CompStyle of
  csButton      : Begin
                  End;
  csForm,
  csMainForm,
  csFileDialog,
  csColorDialog : Begin
                  End;
  csgLabel      : Begin
                  End;
  csCheckBox    : Begin
                  End;
  csGroupBox    : Begin
                  End;
  csEdit        : Begin
                  End;
 End; {Case}
 StrDispose(pLabel);
end;

{------------------------------------------------------------------------------}
{       TWin32Object SetColor                                                 }
{       *Note: Changes the form's default background color                                 }
{------------------------------------------------------------------------------}
Procedure TWin32Object.SetColor(Sender : TObject);
Begin
 //NOT USED RIGHT NOW..........CAUSES ALL FORMS TO USE THESE COLORS!!!!!!
End;

{------------------------------------------------------------------------------}
{       TWin32Object SetCallback                                                 }
{       *Note: Applies a Message to the sender                                 }
{------------------------------------------------------------------------------}
procedure TWin32Object.SetCallback(Msg : LongInt; Sender : TObject);
var
 winObject : TObject;
 signalFunc : Pointer;
 gSignal : PChar;
 I : Integer;
 Signal : String;
Begin
 If Sender is TControlCanvas then
  winObject := TObject((Sender as TControlCanvas).fcomponent)
 else
  winObject := TObject((Sender as TControl).fcomponent);
 signalFunc := nil;
 Signal := '';
 case Msg of
  LM_SHOW : Begin
             signal := 'show';
             signalFunc := @Win32showCB;
            End;
  LM_DESTROY : Begin
                signal := 'destroy';
                signalFunc := @Win32destroyCB;
               End;
  LM_ACTIVATE : Begin
                 signal := 'activate';
                 signalFunc := @Win32activateCB;
                End;
  LM_ACTIVATEITEM : Begin
                     signal := 'activate-item';
                     signalFunc := @Win32activateCB;
                    End;
  LM_CHANGED      : Begin
                     signal := 'changed';
                     signalFunc := @Win32changedCB;
                    End;
  LM_CLICKED      : Begin
                     Assert(False, 'Trace:SetCallBack - LM_CLICKED');
                     signal := 'clicked';
                     signalFunc := @Win32clickedCB;
                    End;
  LM_CONFIGUREEVENT : Begin
                       signal := 'configure-event';
                       signalFunc := @Win32configureevent;
                      End;
  LM_DRAW           : Begin
                       signal := 'draw';
                       signalFunc := @Win32draw;
                      End;
  LM_EXPOSEEVENT    : Begin
                       signal := 'expose-event';
                       signalFunc := @Win32exposeevent;
                      End;
  LM_FOCUS          : Begin
                       signal := 'focus';
                       signalFunc := @Win32focusCB;
                      End;
  LM_KEYDOWN        : Begin
                       signal := 'key-press-event';
                       signalfunc := @Win32keydown;
                      End;
  LM_KEYUP          : Begin
                       signal := 'key-release-event';
                       signalfunc := @Win32keyup;
                      End;
  LM_PRESSED        : Begin
                       signal := 'pressed';
                       signalFunc := @Win32pressedCB;
                      End;
  LM_RELEASED       : Begin
                       signal := 'released';
                       signalFunc := @Win32releasedCB;
                      End;
  LM_MOVECURSOR     : Begin
                       signal := 'move-cursor';
                       signalFunc := @Win32movecursorCB;
                      End;      
  LM_MOUSEBTNPRESS  : Begin
                       signal := 'button-press-event';
                       signalFunc := @Win32mousebtnpress;
                      End;
  LM_MOUSEBTNRELEASE : Begin
                        signal := 'button-release-event';
                        signalFunc := @Win32mousebtnrelease;
                       End;
  LM_ENTER           : Begin
                        signal := 'enter';
                        signalFunc := @Win32enterCB;
                       End;
  LM_LEAVE           : Begin
                        signal := 'leave';
                        signalFunc := @Win32leaveCB;
                       End;
  LM_SIZEALLOCATE,
  LM_RESIZE          : Begin
                        signal := 'size-allocate';
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
  LM_MOVERESIZE      : Begin
                        signal := 'move_resize';
                        signalFunc := @Win32moveresize;
                       End;
 Else
  Assert(False, 'Trace:SetCallback - ERROR:  Signal "'+signal+'" not found!');
 End; {Case}
 gSignal:= StrAlloc(length(Signal) + 1);
 StrPCopy(gSignal, signal);
 if signalFunc <> nil then
  Begin
   // Connect Signal Here
  End;
 StrDispose(gsignal);
end;

{------------------------------------------------------------------------------}
{       TWin32Object RemoveCallBacks                                             }
{       *Note: Removes Call Back Signals from the sender                       }
{------------------------------------------------------------------------------}
procedure TWin32Object.RemoveCallbacks(Sender : TControl);
begin
 // Destroy handlers here
end;

{------------------------------------------------------------------------------}
{       TWin32Object CreateComponent                                             }
{       *Note: Tells GTK Engine to create a Win32Control                             }
{------------------------------------------------------------------------------}
procedure TWin32Object.CreateComponent(Sender : TObject);
Var
 caption : string;
 StrTemp, pStr : PChar;
 p : pointer;
 CompStyle : Integer;
 ParentWindow : HWnd;
 ParentControl : TObject;
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
 If (Sender is TControl) then
  Begin
   Assert(False, 'Trace:CreateComponent - Setting Control Style to Sender.FCompStyle');   
   CompStyle := (Sender as TControl).FCompStyle
  End
 else if (Sender is TControlCanvas) then
  Begin
   Assert(False, 'Trace:CreateComponent - Setting Control Style to csCanvas');   
   CompStyle := csCanvas
  End
 else if (Sender is TFont) then
  Begin
   Assert(False, 'Trace:CreateComponent - Setting Control Style to csFont');   
   CompStyle := csFont
  End
 else if (Sender is ttimer) then
  Begin
   Assert(False, 'Trace:CreateComponent - Setting Control Style to csTimer');   
   CompStyle := csTimer;
  End;

 Assert(False, 'Trace:CreateComponent - Control Style is ' + CS_To_String(CompStyle));

 Case CompStyle of
  csAlignment      : Begin
                     End;
  csButton         : Begin
                      Assert(False, 'Trace:CreateComponent - Creating Button');
                      Assert(False, 'Trace:CreateComponent - Value of Button Parent is $' + IntToHex(LongInt((Sender as TControl).Parent), 8));
                      Assert(False, 'Trace:CreateComponent - Value of Button Owner is $' + IntToHex(LongInt((Sender as TControl).Owner), 8));
{!}//                      ParentWindow := GetProp(HWnd((Sender as TControl).Parent.FComponent), 'Lazarus');
                      ParentControl := (Sender as TControl).Owner;
                      Assert(False, 'Trace:CreateComponent - Value of ParentControl is $' + IntToHex(LongInt(ParentControl), 8));
                      Assert(False, 'Trace:CreateComponent - Value of (ParentControl as TControl).FComponent is $' + IntToHex(LongInt((ParentControl as TControl).FComponent), 8));
                      ParentWindow := HWnd((ParentControl as TControl).FComponent);
                      Assert(False, 'Trace:CreateComponent - Value of Button Parent Window is $' + IntToHex(LongInt(ParentWindow), 8));
                      Assert(False, 'Trace:CreateComponent - Value of Button Left   is $' + IntToHex((Sender as TControl).Left  , 4));
                      Assert(False, 'Trace:CreateComponent - Value of Button Top    is $' + IntToHex((Sender as TControl).Top   , 4));
                      Assert(False, 'Trace:CreateComponent - Value of Button Width  is $' + IntToHex((Sender as TControl).Width , 4));
                      Assert(False, 'Trace:CreateComponent - Value of Button Height is $' + IntToHex((Sender as TControl).Height, 4));
                      P := Pointer(CreateWindow('BUTTON', 'OK',
                                   WS_VISIBLE OR WS_CHILD OR BS_DEFPUSHBUTTON,
                                   (Sender as TControl).Left,
                                   (Sender as TControl).Top,
                                   (Sender as TControl).Width,
                                   (Sender as TControl).Height,
                                   ParentWindow, 0,
                                   GetWindowLong(ParentWindow, GWL_HINSTANCE),
                                   nil));
                      Assert(False, 'Trace:CreateComponent - Button Window Handle Value = $' + IntToHex(LongInt(P), 8));
                      Assert(False, 'Trace:CreateComponent - Creating a Button - SetProp');
                      if P <> nil then
                       SetProp(HWND(P), 'Lazarus', LongInt(Sender));
                      SetName(p,StrTemp);
                     End;
  csCanvas         : Begin
{!}//                      Assert(False, 'Trace:CreateComponent - Canvas being created!!!!!!!!!!!!!!!!!!!!!');
{!}//                      Assert(False, 'Trace:CreateComponent - 1');
{!}//                      Assert(False, 'Trace:CreateComponent - Nothing really done');
{!}//                      p := TControl((Sender as TControlCanvas).Control).fComponent;
                     End;
  csCheckbox       : Begin
                     End;
  csComboBox       : Begin
                      { Connect the changed signal to its entry }
                     End;
  csListBox        : Begin
                     End;
  csEdit           : Begin
                     End;
  csFont           : Begin
                      Assert(False, 'Trace:CreateComponent - Creating a font');
                     End;
  csForm           : Begin
                      Assert(False, 'Trace:CreateComponent - Creating a Form Window');
                      P := Pointer(CreateWindow(AppName, 'Fill this in later Worm!', ws_OverlappedWindow,
                                                cw_usedefault, cw_usedefault,
                                                cw_usedefault, cw_usedefault,
                                                0, 0, system.MainInstance, nil));
                      Assert(False, 'Trace:CreateComponent - Form Window Handle Value = $' + IntToHex(LongInt(P), 8));
                      Assert(False, 'Trace:Creating a Form - SetProp');
                      SetProp(HWnd(P), 'Lazarus', LongInt(Sender));
                      If LongInt(P) = 0 then
                       Begin
                        MessageBox(0, 'csForm CreateWindow Failed', nil, mb_Ok);
                        Exit;
                       End;
                      SetName(p,StrTemp);
                     End;
  csMainForm       : Begin
                      Assert(False, 'Trace:CreateComponent - Creating a MainForm for Win32 --------------------------------------');
                      pStr := StrAlloc(length('TForm') + 1);
                      StrPCopy(pStr, 'TForm');
                      P := Pointer(CreateWindow(pStr, AppName, ws_OverlappedWindow,
                                                cw_usedefault, cw_usedefault,
                                                cw_usedefault, cw_usedefault,
                                                0, 0, system.MainInstance, nil));
                      StrDispose(pStr);
                      Assert(False, 'Trace:CreateComponent - MainForm Window Handle Value = $' + IntToHex(LongInt(P), 8));
                      Assert(False, 'Trace:Creating a Form - MainForm SetProp');
                      SetProp(HWnd(P), 'Lazarus', LongInt(Sender));
                      If LongInt(P) = 0 then
                       Begin
                        MessageBox(0, 'csMainForm CreateWindow Failed', nil, mb_Ok);
                        Exit;
                       End;
                      SetName(p,StrTemp);
                     End;
  csFrame          : Begin
                     End;
  csgLabel         : Begin
                     End;
  csMemo           : Begin
                      // Writeln('Creating a MEMO...');
                     End;
  csMenu           : Begin
                     End;
  csMenuBar        : Begin
                     End;
  csMenuItem       : Begin
                     End;
  csNotebook       : Begin
                     End;
  csFileDialog     : Begin
                     End;
  csColorDialog    : Begin
                     End;
  csRadioButton    : Begin
                     End;
  csScrolledWindow : Begin
                     End;
  csSpinEdit       : Begin
                      //this needs to be created in the actual code because it requires a gtkadjustment Win32Control
                     End;
  csSTATUSBAR      : Begin
                     End;
  csgtkTable       : Begin
                     End;
  csToggleBox      : Begin
                     End;
  csGroupBox       : Begin
                     End;
  csTimer          : Begin
                      Assert(False, 'Trace:CreateComponent - Creating a timer in CreateComponent');
                     End;
  csPage           : Begin   // TPage - Notebook page 
                     End;
 end; {Case}
 if (Sender is TControl) then
  Begin
   Assert(False, 'Trace:CreateComponent - Assigning window to TControl');
   (Sender as TControl).FComponent := p;
  End
 else
  if (Sender is TControlCanvas) then
   Begin
    Assert(False, 'Trace:CreateComponent - Assigning window to TControlCanvas');
    (Sender as TControlCanvas).FComponent := p;
   End
  else
   if (Sender is TFont) then
    Begin
     Assert(False, 'Trace:CreateComponent - Assigning P to TFont');
     (Sender as TFont).fComponent:= p;
    End;
 StrDispose(StrTemp);
end;

{------------------------------------------------------------------------------}
{       TWin32Object GetLabel                                                    }
{       *Note: Returns a Win32Controls lable value                                   }
{------------------------------------------------------------------------------}
function TWin32Object.GetLabel(CompStyle: Integer; P : Pointer) : String;
Var
 Value : String;

begin
 Value := 'Label';
 // Get label here
 GetLabel := Value;
end;

{------------------------------------------------------------------------------}
{       TWin32Object AssignSelf                                                  }
{       *Note: Assigns a pointer to self on a Win32Control                           }
{------------------------------------------------------------------------------}
procedure TWin32Object.AssignSelf(Child,Data : Pointer);
begin
end;

{------------------------------------------------------------------------------}
{       TWin32Object SetName                                                     }
{       *Note: Sets the gtk engine internal name for a Win32Control                  }
{------------------------------------------------------------------------------}
procedure TWin32Object.SetName(Child ,Data : Pointer);
begin
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
Begin
end;

{------------------------------------------------------------------------------}
{       TWin32Object DrawRect                                                    }
{       *Note: Draw Rectangle on child Win32Control                                  }
{------------------------------------------------------------------------------}
Procedure TWin32Object.DrawRect(Child,Data : Pointer);
Begin
end;

{------------------------------------------------------------------------------}
{       TWin32Object DrawLine                                                    }
{       *Note: DrawLine on child Win32Control                                        }
{------------------------------------------------------------------------------}
Procedure TWin32Object.DrawLine(Child,Data : Pointer);
Begin
end;

{------------------------------------------------------------------------------}
{       TWin32Object DrawText                                                    }
{       *Note: Draws Text on Child Win32Control                                      }
{------------------------------------------------------------------------------}
Procedure TWin32Object.DrawText(Child,Data : Pointer);
Begin
end;

Procedure TWin32Object.ReDraw(Child : Pointer);
Begin
end;

Procedure TWin32Object.GetFontinfo(Sender : TObject; Data : Pointer);
Begin
End;

 {
  $Log$
  Revision 1.1  2000/07/13 10:28:31  michael
  + Initial import

  Revision 1.1  2000/03/30 22:53:37  lazarus
  MWE:
    Moved form ../..

  Revision 1.2  1999/08/19 01:13:22  lazarus
  Makes button visible, still no OnClick behavior

 }
