{%MainUnit ../gdk2x.pas}

{$IFDEF read_interface_rest}

type
   PMotifWmHints = ^TMotifWmHints;
   TMotifWmHints = record
        flags : dword;
        functions : dword;
        decorations : dword;
        input_mode : longint;
        status : dword;
     end;
   TMwmHints = TMotifWmHints;
   PMwmHints = ^TMwmHints;

const
   MWM_HINTS_FUNCTIONS = 1 shl 0;
   MWM_HINTS_DECORATIONS = 1 shl 1;
   MWM_HINTS_INPUT_MODE = 1 shl 2;
   MWM_HINTS_STATUS = 1 shl 3;
   MWM_FUNC_ALL = 1 shl 0;
   MWM_FUNC_RESIZE = 1 shl 1;
   MWM_FUNC_MOVE = 1 shl 2;
   MWM_FUNC_MINIMIZE = 1 shl 3;
   MWM_FUNC_MAXIMIZE = 1 shl 4;
   MWM_FUNC_CLOSE = 1 shl 5;
   MWM_DECOR_ALL = 1 shl 0;
   MWM_DECOR_BORDER = 1 shl 1;
   MWM_DECOR_RESIZEH = 1 shl 2;
   MWM_DECOR_TITLE = 1 shl 3;
   MWM_DECOR_MENU = 1 shl 4;
   MWM_DECOR_MINIMIZE = 1 shl 5;
   MWM_DECOR_MAXIMIZE = 1 shl 6;
   MWM_INPUT_MODELESS = 0;
   MWM_INPUT_PRIMARY_APPLICATION_MODAL = 1;
   MWM_INPUT_SYSTEM_MODAL = 2;
   MWM_INPUT_FULL_APPLICATION_MODAL = 3;
   MWM_INPUT_APPLICATION_MODAL = MWM_INPUT_PRIMARY_APPLICATION_MODAL;
   MWM_TEAROFF_WINDOW = 1 shl 0;

{ atoms}
   _XA_MOTIF_BINDINGS = '_MOTIF_BINDINGS';
   _XA_MOTIF_WM_HINTS = '_MOTIF_WM_HINTS';
   _XA_MOTIF_WM_MESSAGES = '_MOTIF_WM_MESSAGES';
   _XA_MOTIF_WM_OFFSET = '_MOTIF_WM_OFFSET';
   _XA_MOTIF_WM_MENU = '_MOTIF_WM_MENU';
   _XA_MOTIF_WM_INFO = '_MOTIF_WM_INFO';
   _XA_MWM_HINTS = _XA_MOTIF_WM_HINTS;
   _XA_MWM_MESSAGES = _XA_MOTIF_WM_MESSAGES;
   _XA_MWM_MENU = _XA_MOTIF_WM_MENU;
   _XA_MWM_INFO = _XA_MOTIF_WM_INFO;

{ _MWM_INFO property }

type
   PMotifWmInfo = ^TMotifWmInfo;
   TMotifWmInfo = record
        flags : longint;
        wm_window : TWindow;
     end;

   PMwmInfo = ^TMwmInfo;
   TMwmInfo = TMotifWmInfo;

const
   MWM_INFO_STARTUP_STANDARD = 1 shl 0;
   MWM_INFO_STARTUP_CUSTOM = 1 shl 1;

{ _MWM_HINTS property }
type
   PPropMotifWmHints = ^TPropMotifWmHints;
   TPropMotifWmHints = record
        flags : dword;
        functions : dword;
        decorations : dword;
        inputMode : longint;
        status : dword;
     end;

   PPropMwmHints = ^TPropMwmHints;
   TPropMwmHints = TPropMotifWmHints;

const
   PROP_MOTIF_WM_HINTS_ELEMENTS = 5;
   PROP_MWM_HINTS_ELEMENTS = PROP_MOTIF_WM_HINTS_ELEMENTS;

{ _MWM_INFO property, slight return }
type
   PPropMotifWmInfo = ^TPropMotifWmInfo;
   TPropMotifWmInfo = record
        flags : dword;
        wmWindow : dword;
     end;

   PPropMwmInfo = ^TPropMwmInfo;
   TPropMwmInfo = TPropMotifWmInfo;

const
   PROP_MOTIF_WM_INFO_ELEMENTS = 2;
   PROP_MWM_INFO_ELEMENTS = PROP_MOTIF_WM_INFO_ELEMENTS;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
{$ENDIF read_implementation}

