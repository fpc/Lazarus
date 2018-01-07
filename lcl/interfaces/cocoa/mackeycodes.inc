(*
About Mac Key codes:
unfortunately, mac key codes are keyboard specific:
that is, there is no universal VK_A, but every keyboard has its code for VK_A
Key codes depend on physical key position on the keyboard: considering a
QWERTY keyboard and an AZERTY one, keycode(Q) of first one = keycode(A) of
the second one, and so on.
For "printable" keys we can rely on kEventParamKeyMacCharCodes and
kEventParamKeyUnicodes event parameters to obtain an ascii/unicode value
that we can translate to the appropriate VK_ code
For non printable keys (Function, ins, arrow and so on...) we use the raw
keycodes, since it looks like they are constant across all keyboards

So, here are constants for non-printable keys (MK means "Mac Key").
These constants were extracted using KeyCodes program by Peter Maurer
(http://www.petermaurer.de/nasi.php?section=keycodes)

Some keys were taken from the ancient "Macintosh Toolbox Essentials", page 87
http://developer.apple.com/documentation/mac/pdf/MacintoshToolboxEssentials.pdf
*)

const
  MK_ENTER     = $24;
  MK_SPACE     = $31;
  MK_ESC       = $35;
  MK_F1        = $7A;
  MK_F2        = $78;
  MK_F3        = $63;
  MK_F4        = $76;
  MK_F5        = $60;
  MK_F6        = $61;
  MK_F7        = $62;
  MK_F8        = $64;
  MK_F9        = $65;
  MK_F10       = $6D;
  MK_F11       = $67;
  MK_F12       = $6F;
  MK_F13       = $69; MK_PRNSCR  = MK_F13;  //Print screen = F13
  MK_F14       = $6B; MK_SCRLOCK = MK_F14;  //Scroll Lock = F14
  MK_F15       = $71; MK_PAUSE   = MK_F15;  //Pause = F15
  MK_POWER     = $7F7F;
  MK_TAB       = $30;
  MK_INS       = $72; MK_HELP    = MK_INS;  //old macs call this key "help"
  MK_DEL       = $75;
  MK_HOME      = $73;
  MK_END       = $77;
  MK_PAGUP     = $74;
  MK_PAGDN     = $79;
  MK_UP        = $7E;
  MK_DOWN      = $7D;
  MK_LEFT      = $7B;
  MK_RIGHT     = $7C;
  MK_NUMLOCK   = $47;
  MK_NUMPAD0   = $52;
  MK_NUMPAD1   = $53;
  MK_NUMPAD2   = $54;
  MK_NUMPAD3   = $55;
  MK_NUMPAD4   = $56;
  MK_NUMPAD5   = $57;
  MK_NUMPAD6   = $58;
  MK_NUMPAD7   = $59;
  MK_NUMPAD8   = $5b;
  MK_NUMPAD9   = $5c;
  MK_PADEQUALS = $51; //only present in old mac keyboards?
  MK_PADDIV    = $4B;
  MK_PADMULT   = $43;
  MK_PADSUB    = $4E;
  MK_PADADD    = $45;
  MK_PADDEC    = $41;
  MK_PADENTER  = $4C; //enter on numeric keypad
  MK_BACKSPACE = $33;
  MK_CAPSLOCK  = $39;


//Modifiers codes - you'll never get these directly

  MK_SHIFTKEY  = $38;
  MK_CTRL      = $3B;
  MK_ALT       = $3A; MK_OPTION = MK_ALT;
  MK_COMMAND   = $37; MK_APPLE  = MK_COMMAND;

  MK_TILDE        = 50; // `/~ key
  MK_MINUS        = 27; // -/_ key
  MK_EQUAL        = 24; // =/+ key
  MK_BACKSLASH    = 42; // \ | key
  MK_LEFTBRACKET  = 33; // [ { key
  MK_RIGHTBRACKET = 30; // ] } key
  MK_SEMICOLON    = 41; // ; : key
  MK_QUOTE        = 39; // ' " key
  MK_COMMA        = 43; // , < key
  MK_PERIOD       = 47; // . > key
  MK_SLASH        = 44; // / ? key

  // ANSI US (QWERTY) Keyboard

  MK_QWERTY_Q = 12;
  MK_QWERTY_W = 13;
  MK_QWERTY_E = 14;
  MK_QWERTY_R = 15;
  MK_QWERTY_T = 17;
  MK_QWERTY_Y = 16;
  MK_QWERTY_U = 32;
  MK_QWERTY_I = 34;
  MK_QWERTY_O = 31;
  MK_QWERTY_P = 35;
  MK_QWERTY_LEFTBR = 33;
  MK_QWERTY_RIGHTBR = 30;
  MK_QWERTY_BACKSLASH = 42;

  MK_QWERTY_A = 0;
  MK_QWERTY_S = 1;
  MK_QWERTY_D = 2;
  MK_QWERTY_F = 3;
  MK_QWERTY_G = 5;
  MK_QWERTY_H = 4;
  MK_QWERTY_J = 38;
  MK_QWERTY_K = 40;
  MK_QWERTY_L = 37;
  MK_QWERTY_SEMICOLON = 41;
  MK_QWERTY_QUOTE = 39;
  MK_QWERTY_ENTER = 36;

  MK_QWERTY_Z = 6;
  MK_QWERTY_X = 7;
  MK_QWERTY_C = 8;
  MK_QWERTY_V = 9;
  MK_QWERTY_B = 11;
  MK_QWERTY_N = 45;
  MK_QWERTY_M = 46;
  MK_QWERTY_COMMA = 43;
  MK_QWERTY_PERIOD = 47;
  MK_QWERTY_FRWSLASH = 44;

  MK_QWERTY_TILDE = 50;
  MK_QWERTY_1 = 18;
  MK_QWERTY_2 = 19;
  MK_QWERTY_3 = 20;
  MK_QWERTY_4 = 21;
  MK_QWERTY_5 = 23;
  MK_QWERTY_6 = 22;
  MK_QWERTY_7 = 26;
  MK_QWERTY_8 = 28;
  MK_QWERTY_9 = 25;
  MK_QWERTY_0 = 29;
  MK_QWERTY_MINUS = 27;
  MK_QWERTY_EQUAL = 24;
  MK_QWERTY_BACKSP = 51;

  MK_QWERTY_NUMSLASH = 75;
  MK_QWERTY_NUMSTAR  = 67;
  MK_QWERTY_NUMMINUS = 78;
  MK_QWERTY_NUMPLUS  = 69;
  MK_QWERTY_NUMENTER = 76;

  MK_QWERTY_CONTEXTMENU = 110;

