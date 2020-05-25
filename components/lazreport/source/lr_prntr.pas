
{*****************************************}
{                                         }
{             FastReport v2.3             }
{              Printer info               }
{                                         }
{  Copyright (c) 1998-99 by Tzyganenko A. }
{                                         }
{*****************************************}

{.$define DbgPrinter}

unit LR_Prntr;

interface

{$I LR_Vers.inc}

uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Printers,LCLType,LCLProc,

  LR_Class, LR_Const;

type

  { TfrPrinter }

  TfrPrinter = class
  private
    FDevice: PChar;
    FDriver: PChar;
    FPort: PChar;
    //FDeviceMode: THandle;
    FPrinter: TPrinter;
    FPaperNames: TStringList;
    FPrinters: TStringList;
    FPrinterIndex: Integer;
    FDefaultPrinter: Integer;
    procedure GetSettings(PrinterChanged: boolean = true);
    procedure SetSettings;
    procedure SetPrinter(Value: TPrinter);
    procedure SetPrinterIndex(Value: Integer);
    function  GetPaperNames: TStringList;
    function  MatchPrinterPaper(const aWidth, aHeight: Integer): integer;
  public
    Orientation: TPrinterOrientation;
    PaperSize: Integer;
    PaperWidth: Integer;
    PaperHeight: Integer;
    PaperSizes: Array[0..255] of Integer; // bumped to integer for more flexibility //Word;
    PaperSizesNum: Integer;
    constructor Create;
    destructor Destroy; override;
    procedure FillPrnInfo(var p: TfrPrnInfo);
    procedure SetPrinterInfo(pgSize, pgWidth, pgHeight: Integer;
      pgOr: TPrinterOrientation);
    function IsEqual(pgSize, pgWidth, pgHeight: Integer;
      pgOr: TPrinterOrientation): Boolean;
    function GetArrayPos(pgSize: Integer): Integer;
    function DefaultPaperIndex: Integer;
    function DefaultPageSize: Integer;
    function UseVirtualPrinter: boolean;
    {$IFDEF DbgPrinter}
    procedure DumpPrinterInfo;
    {$ENDIF}
    
    property PaperNames: TStringList read GetPaperNames;
    property Printer: TPrinter read FPrinter write SetPrinter;
    property Printers: TStringList read FPrinters;
    property PrinterIndex: Integer read FPrinterIndex write SetPrinterIndex;
  end;
  
function  Prn : TfrPrinter;

const
  MAX_TYP_KNOWN = 118;

implementation

var
  GlobalPrn: TfrPrinter = nil;

type
  TPaperInfo = record
    Typ: Integer;
    Name: String;
    X, Y: Integer;
  end;

const
  PAPERCOUNT    = 117;
  //OLDPAPERCOUNT = 67;  // show only that much paper names when using virtual printer
  
  PaperInfo: Array[0..PAPERCOUNT - 1] of TPaperInfo = (
    (Typ:1;  Name: ''; X:2159; Y:2794),
    (Typ:2;  Name: ''; X:2159; Y:2794),
    (Typ:3;  Name: ''; X:2794; Y:4318),
    (Typ:4;  Name: ''; X:4318; Y:2794),
    (Typ:5;  Name: ''; X:2159; Y:3556),
    (Typ:6;  Name: ''; X:1397; Y:2159),
    (Typ:7;  Name: ''; X:1842; Y:2667),
    (Typ:8;  Name: ''; X:2970; Y:4200),
    (Typ:9;  Name: ''; X:2100; Y:2970),
    (Typ:10; Name: ''; X:2100; Y:2970),
    (Typ:11; Name: ''; X:1480; Y:2100),
    (Typ:12; Name: ''; X:2570; Y:3640), // Nota 1
    (Typ:13; Name: ''; X:1820; Y:2570),
    (Typ:14; Name: ''; X:2159; Y:3302), // Nota 3
    (Typ:15; Name: ''; X:2150; Y:2750),
    (Typ:16; Name: ''; X:2540; Y:3556),
    (Typ:17; Name: ''; X:2794; Y:4318),
    (Typ:18; Name: ''; X:2159; Y:2794),
    (Typ:19; Name: ''; X:984;  Y:2254),
    (Typ:20; Name: ''; X:1048; Y:2413),
    (Typ:21; Name: ''; X:1143; Y:2635),
    (Typ:22; Name: ''; X:1207; Y:2794),
    (Typ:23; Name: ''; X:1270; Y:2921),
    (Typ:24; Name: ''; X:4318; Y:5588),
    (Typ:25; Name: ''; X:5588; Y:8636),
    (Typ:26; Name: ''; X:8636; Y:11176),
    (Typ:27; Name: ''; X:1100; Y:2200),
    (Typ:28; Name: ''; X:1620; Y:2290),
    (Typ:29; Name: ''; X:3240; Y:4580),
    (Typ:30; Name: ''; X:2290; Y:3240),
    (Typ:31; Name: ''; X:1140; Y:1620),
    (Typ:32; Name: ''; X:1140; Y:2290),
    (Typ:33; Name: ''; X:2500; Y:3530),
    (Typ:34; Name: ''; X:1760; Y:2500),
    (Typ:35; Name: ''; X:1760; Y:1250),
    (Typ:36; Name: ''; X:1100; Y:2300),
    (Typ:37; Name: ''; X:984;  Y:1905),
    (Typ:38; Name: ''; X:920;  Y:1651),
    (Typ:39; Name: ''; X:3778; Y:2794),
    (Typ:40; Name: ''; X:2159; Y:3048),
    (Typ:41; Name: ''; X:2159; Y:3302),
    (Typ:42; Name: ''; X:2500; Y:3530),
    (Typ:43; Name: ''; X:1000; Y:1480),
    (Typ:44; Name: ''; X:2286; Y:2794),
    (Typ:45; Name: ''; X:2540; Y:2794),
    (Typ:46; Name: ''; X:3810; Y:2794),
    (Typ:47; Name: ''; X:2200; Y:2200),
    (Typ:50; Name: ''; X:2355; Y:3048),
    (Typ:51; Name: ''; X:2355; Y:3810),
    (Typ:52; Name: ''; X:2969; Y:4572),
    (Typ:53; Name: ''; X:2354; Y:3223),
    (Typ:54; Name: ''; X:2101; Y:2794),
    (Typ:55; Name: ''; X:2100; Y:2970),
    (Typ:56; Name: ''; X:2355; Y:3048),
    (Typ:57; Name: ''; X:2270; Y:3560),
    (Typ:58; Name: ''; X:3050; Y:4870),
    (Typ:59; Name: ''; X:2159; Y:3223),
    (Typ:60; Name: ''; X:2100; Y:3300),
    (Typ:61; Name: ''; X:1480; Y:2100),
    (Typ:62; Name: ''; X:1820; Y:2570),
    (Typ:63; Name: ''; X:3220; Y:4450),
    (Typ:64; Name: ''; X:1740; Y:2350),
    (Typ:65; Name: ''; X:2010; Y:2760),
    (Typ:66; Name: ''; X:4200; Y:5940),
    (Typ:67; Name: ''; X:2970; Y:4200),
    (Typ:68; Name: ''; X:3220; Y:4450),
    // Nota 2
    (Typ:69; Name: ''; X:2000; Y:1480),
    (Typ:70; Name: ''; X:1050; Y:1480),
    (Typ:71; Name: ''; X:2400; Y:1320),
    (Typ:72; Name: ''; X:2160; Y:2770),
    (Typ:73; Name: ''; X:1200; Y:2350),
    (Typ:74; Name: ''; X:900;  Y:2050),
    (Typ:75; Name: ''; X:2794; Y:2159),
    (Typ:76; Name: ''; X:4200; Y:2970),
    (Typ:77; Name: ''; X:2970; Y:2100),
    (Typ:78; Name: ''; X:2100; Y:1480),
    (Typ:79; Name: ''; X:3640; Y:2570),
    (Typ:80; Name: ''; X:2570; Y:1820),
    (Typ:81; Name: ''; X:1480; Y:1000),
    (Typ:82; Name: ''; X:1480; Y:2000),
    (Typ:83; Name: ''; X:1480; Y:1050),
    (Typ:84; Name: ''; X:3320; Y:2400),
    (Typ:85; Name: ''; X:2770; Y:2160),
    (Typ:86; Name: ''; X:2350; Y:1200),
    (Typ:87; Name: ''; X:2050; Y:900 ),
    (Typ:88; Name: ''; X:1280; Y:1820),
    (Typ:89; Name: ''; X:1820; Y:1280),
    (Typ:90; Name: ''; X:3048; Y:2794),
    (Typ:91; Name: ''; X:1050; Y:2350),
    (Typ:92; Name: ''; X:2350; Y:1050),
    (Typ:93; Name: ''; X:1460; Y:2150),
    (Typ:94; Name: ''; X:970;  Y:1510),
    (Typ:95; Name: ''; X:970;  Y:1510),
    (Typ:96; Name: ''; X:1020; Y:1650),
    (Typ:97; Name: ''; X:1020; Y:1760),
    (Typ:98; Name: ''; X:1250; Y:1760),
    (Typ:99; Name: ''; X:1100; Y:2080),
    (Typ:100; Name: ''; X:1100; Y:2200),
    (Typ:101; Name: ''; X:1200; Y:2300),
    (Typ:102; Name: ''; X:1600; Y:2300),
    (Typ:103; Name: ''; X:1200; Y:3090),
    (Typ:104; Name: ''; X:2290; Y:3240),
    (Typ:105; Name: ''; X:3240; Y:4580),
    (Typ:106; Name: ''; X:2150; Y:1460),
    (Typ:107; Name: ''; X:1510; Y:970 ),
    (Typ:108; Name: ''; X:1510; Y:970 ),
    (Typ:109; Name: ''; X:1650; Y:1020),
    (Typ:110; Name: ''; X:1760; Y:1020),
    (Typ:111; Name: ''; X:1760; Y:1250),
    (Typ:112; Name: ''; X:2080; Y:1100),
    (Typ:113; Name: ''; X:2200; Y:1100),
    (Typ:114; Name: ''; X:2300; Y:1200),
    (Typ:115; Name: ''; X:2300; Y:1600),
    (Typ:116; Name: ''; X:3090; Y:1200),
    (Typ:117; Name: ''; X:3240; Y:2290),
    (Typ:118; Name: ''; X:4580; Y:3240),
    (Typ:256; Name: ''; X:0;    Y:0));

{$IFNDEF MSWINDOWS}
const
  PPDPaperInfo: Array[0..PAPERCOUNT - 1] of TPaperInfo = (
    (Typ:1;   Name: 'Letter';                 X:612;  Y:792 ),
    (Typ:2;   Name: 'LetterSmall';            X:612;  Y:792 ),
    (Typ:3;   Name: 'Tabloid';                X:792;  Y:1224),
    (Typ:4;   Name: 'Ledger';                 X:1224; Y:792 ),
    (Typ:5;   Name: 'Legal';                  X:612;  Y:1008),
    (Typ:6;   Name: 'Statement';              X:396;  Y:612 ),
    (Typ:7;   Name: 'Executive';              X:522;  Y:756 ),
    (Typ:8;   Name: 'A3';                     X:842;  Y:1191),
    (Typ:9;   Name: 'A4';                     X:595;  Y:842 ),
    (Typ:10;  Name: 'A4Small';                X:595;  Y:842 ),
    (Typ:11;  Name: 'A5';                     X:420;  Y:595 ),
    (Typ:12;  Name: 'B4';                     X:729;  Y:1032),
    (Typ:13;  Name: 'B5';                     X:516;  Y:729 ),
    (Typ:14;  Name: 'Folio';                  X:595;  Y:936 ), // note 4
    (Typ:15;  Name: 'Quarto';                 X:610;  Y:780 ), // note 5
    (Typ:16;  Name: '10x14';                  X:720;  Y:1008),
    (Typ:17;  Name: '11x17';                  X:792;  Y:1224), // no ppd name for this
    (Typ:18;  Name: 'Note';                   X:612;  Y:792 ),
    (Typ:19;  Name: 'Env9';                   X:279;  Y:639 ),
    (Typ:20;  Name: 'Env10';                  X:297;  Y:684 ),
    (Typ:21;  Name: 'Env11';                  X:324;  Y:747 ),
    (Typ:22;  Name: 'Env12';                  X:342;  Y:792 ),
    (Typ:23;  Name: 'Env14';                  X:360;  Y:828 ),
    (Typ:24;  Name: 'ARCHC';                  X:1296; Y:1728), // note 5, 18"x24"
    (Typ:25;  Name: 'ARCHD';                  X:1728; Y:2592), // note 5  24"x36"
    (Typ:26;  Name: 'ARCHE';                  X:2592; Y:3456), // note 5  36"x48"
    (Typ:27;  Name: 'EnvDL';                  X:312;  Y:624 ),
    (Typ:28;  Name: 'EnvC5';                  X:459;  Y:649 ),
    (Typ:29;  Name: 'EnvC3';                  X:918;  Y:1298), // sim note 4, 458mm=1298pt not 1296
    (Typ:30;  Name: 'EnvC4';                  X:649;  Y:918 ),
    (Typ:31;  Name: 'EnvC6';                  X:323;  Y:459 ),
    (Typ:32;  Name: 'EnvC65';                 X:323;  Y:649 ), // sim note 4, 229mm=649pt not 648
    (Typ:33;  Name: 'EnvISOB4';               X:708;  Y:1001), // note 6
    (Typ:34;  Name: 'EnvISOB5';               X:499;  Y:709 ),
    (Typ:35;  Name: 'EnvISOB6';               X:499;  Y:354 ),
    (Typ:36;  Name: 'EnvItalian';             X:312;  Y:652 ),
    (Typ:37;  Name: 'EnvMonarch';             X:279;  Y:540 ),
    (Typ:38;  Name: 'EnvPersonal';            X:261;  Y:468 ),
    (Typ:39;  Name: 'FanFoldUS';              X:1071; Y:792 ),
    (Typ:40;  Name: 'FanFoldGerman';          X:612;  Y:864 ),
    (Typ:41;  Name: 'FanFoldGermanLegal';     X:612;  Y:936 ),
    (Typ:42;  Name: 'ISOB4';                  X:709;  Y:1001),
    (Typ:43;  Name: 'Postcard';               X:284;  Y:419 ), // note 6
    (Typ:44;  Name: '9x11';                   X:648;  Y:792 ),
    (Typ:45;  Name: '10x11';                  X:720;  Y:792 ),
    (Typ:46;  Name: '15x11';                  X:1080; Y:792 ),
    (Typ:47;  Name: 'EnvInvite';              X:624;  Y:624 ),
    (Typ:50;  Name: 'LetterExtra';            X:684;  Y:864 ), // note 6
    (Typ:51;  Name: 'LegalExtra';             X:684;  Y:1080), // note 6
    (Typ:52;  Name: 'TabloidExtra';           X:842;  Y:1296),
    (Typ:53;  Name: 'A4Extra';                X:667;  Y:914 ),
    (Typ:54;  Name: 'Letter.Transverse';      X:612;  Y:792 ), // note 6
    (Typ:55;  Name: 'A4.Transverse';          X:595;  Y:842 ),
    (Typ:56;  Name: 'LetterExtra.Transverse'; X:684;  Y:864 ), // note 6
    (Typ:57;  Name: 'SuperA';                 X:643;  Y:1009),
    (Typ:58;  Name: 'SuperB';                 X:864;  Y:1380), // note 6
    (Typ:59;  Name: 'LetterPlus';             X:612;  Y:914 ), // Y:913.4
    (Typ:60;  Name: 'A4Plus';                 X:595;  Y:936 ), // note 6
    (Typ:61;  Name: 'A5.Transverse';          X:420;  Y:595 ),
    (Typ:62;  Name: 'B5.Transverse';          X:516;  Y:729 ),
    (Typ:63;  Name: 'A3Extra';                X:913;  Y:1262), // note 6
    (Typ:64;  Name: 'A5Extra';                X:493;  Y:668 ), // note 6
    (Typ:65;  Name: 'ISOB5Extra';             X:570;  Y:782 ), // X:569.7
    (Typ:66;  Name: 'A2';                     X:1191; Y:1684),
    (Typ:67;  Name: 'A3.Transverse';          X:842;  Y:1191),
    (Typ:68;  Name: 'A3Extra.Transverse';     X:913;  Y:1262), // note 6
    (Typ:69;  Name: 'DoublePostcard';         X:567;  Y:420 ), //Y:419.5
    (Typ:70;  Name: 'A6';                     X:297;  Y:420 ), // note 6
    (Typ:71;  Name: 'EnvKaku2';               X:680;  Y:941 ), // note 6
    (Typ:72;  Name: 'EnvKaku3';               X:612;  Y:785 ),
    (Typ:73;  Name: 'EnvChou3';               X:340;  Y:666 ),
    (Typ:74;  Name: 'EnvChou4';               X:255;  Y:581 ),
    (Typ:75;  Name: 'LetterRotated';          X:792;  Y:612 ),
    (Typ:76;  Name: 'A3Rotated';              X:1191; Y:842 ),
    (Typ:77;  Name: 'A4Rotated';              X:842;  Y:595 ),
    (Typ:78;  Name: 'A5Rotated';              X:595;  Y:420 ),
    (Typ:79;  Name: 'B4Rotated';              X:1032; Y:729 ),
    (Typ:80;  Name: 'B5Rotated';              X:729;  Y:516 ),
    (Typ:81;  Name: 'PostcardRotated';        X:419;  Y:284 ), // note 6
    (Typ:82;  Name: 'DoublePostcardRotated';  X:420;  Y:567 ), //X:419.5
    (Typ:83;  Name: 'A6Rotated';              X:420;  Y:297 ), // note 6
    (Typ:84;  Name: 'EnvKaku2Rotated';        X:941;  Y:680 ),
    (Typ:85;  Name: 'EnvKaku3Rotated';        X:785;  Y:612 ),
    (Typ:86;  Name: 'EnvChou3Rotated';        X:666;  Y:340 ),
    (Typ:87;  Name: 'EnvChou4Rotated';        X:581;  Y:255 ),
    (Typ:88;  Name: 'B6';                     X:363;  Y:516 ),
    (Typ:89;  Name: 'B6Rotated';              X:516;  Y:363 ),
    (Typ:90;  Name: '12x11';                  X:864;  Y:792 ),
    (Typ:91;  Name: 'EnvYou4';                X:298;  Y:666 ),
    (Typ:92;  Name: 'EnvYouRotated';          X:666;  Y:298 ),
    (Typ:93;  Name: 'PRC16K';                 X:414;  Y:610 ), // note 6
    (Typ:94;  Name: 'PRC32K';                 X:275;  Y:428 ),
    (Typ:95;  Name: 'PRC32KBig';              X:275;  Y:428 ),
    (Typ:96;  Name: 'EnvPRC1';                X:289;  Y:468 ),
    (Typ:97;  Name: 'EnvPRC2';                X:289;  Y:499 ),
    (Typ:98;  Name: 'EnvPRC3';                X:354;  Y:499 ),
    (Typ:99;  Name: 'EnvPRC4';                X:312;  Y:590 ),
    (Typ:100; Name: 'EnvPRC5';                X:312;  Y:624 ),
    (Typ:101; Name: 'EnvPRC6';                X:340;  Y:652 ),
    (Typ:102; Name: 'EnvPRC7';                X:454;  Y:652 ),
    (Typ:103; Name: 'EnvPRC8';                X:340;  Y:876 ),
    (Typ:104; Name: 'EnvPRC9';                X:649;  Y:918 ),
    (Typ:105; Name: 'EnvPRC10';               X:918;  Y:1298),
    (Typ:106; Name: 'PRC16KRotated';          X:610;  Y:414 ), // note 6
    (Typ:107; Name: 'PRC32KRotated';          X:428;  Y:275 ),
    (Typ:108; Name: 'PRC32KBigRotated';       X:428;  Y:275 ),
    (Typ:109; Name: 'EnvPRC1Rotated';         X:468;  Y:289 ),
    (Typ:110; Name: 'EnvPRC2Rotated';         X:499;  Y:289 ),
    (Typ:111; Name: 'EnvPRC3Rotated';         X:499;  Y:354 ),
    (Typ:112; Name: 'EnvPRC4Rotated';         X:590;  Y:312 ),
    (Typ:113; Name: 'EnvPRC5Rotated';         X:624;  Y:312 ),
    (Typ:114; Name: 'EnvPRC6Rotated';         X:652;  Y:340 ),
    (Typ:115; Name: 'EnvPRC7Rotated';         X:652;  Y:454 ),
    (Typ:116; Name: 'EnvPRC8Rotated';         X:876;  Y:340 ),
    (Typ:117; Name: 'EnvPRC9Rotated';         X:918;  Y:649 ),
    (Typ:118; Name: 'EnvPRC10Rotated';        X:1298; Y:918 ),
    (typ:256; Name: ''; X:0;    Y:0));
    
//
// Notes
//
// 1.   Typ12, this is not ISOB4 which is Typ42, moreover, ISOB4 is
//      2500x3530 and not 2500x3540
//
// 2.   New paper were added from here, Additional mappings were
//      obtained from [1] appendix B Table B.1.
//      Numeric defines were obtained from [2]
//
// 3.   Folio for windows is probably 81/2"x13" (letter wide) while for [1]
//      it is 8.27"x13" (A4 wide). Here [1] based value will be used but
//      mapped to corresponding "windows folio" paper number DMPAPER_FOLIO(14)
//
// 4.   [1] folio value gives 594x935 points for 8.27"x13" but 13" are
//      exactly 936 points, the value 936 will be used though if ppd implementa
//      tor follows exactly table [1] B.1, it can give unmatched papers
//
// 5.   [1] and Windows doesn't match, same resolution than note 4.
//
// 6.   [1] and Windows doesn't match, [1] value was choosen
//
// References
//
// [1]  Adobe technote #5003: PPD spec v4.3
//      http://partners.adobe.com/public/developer/ps/index_specs.html
//
// [2]  Wine Project source code:
//      http://source.winehq.org/source/include/wingdi.h#L2927
//
{$ENDIF}



function Prn: TfrPrinter;
begin
if Assigned( GlobalPrn ) then
  Exit( GlobalPrn );

  GlobalPrn := TfrPrinter.Create;
  try
    GlobalPrn.Printer:=Printer;
  except
    on E: Exception do begin
      debugln('lazreport: unit lr_prntr: ',E.Message);
    end;
  end;
  Exit( GlobalPrn );
end;


{----------------------------------------------------------------------------}
constructor TfrPrinter.Create;
var
  i: Integer;
begin
  inherited Create;
  GetMem(FDevice, 128);
  GetMem(FDriver, 128);
  GetMem(FPort, 128);
  FPaperNames := TStringList.Create;
  FPrinters := TStringList.Create;
  i:=0;
  PaperInfo[i].Name := sPaper1; Inc(i);
  PaperInfo[i].Name := sPaper2; Inc(i);
  PaperInfo[i].Name := sPaper3; Inc(i);
  PaperInfo[i].Name := sPaper4; Inc(i);
  PaperInfo[i].Name := sPaper5; Inc(i);
  PaperInfo[i].Name := sPaper6; Inc(i);
  PaperInfo[i].Name := sPaper7; Inc(i);
  PaperInfo[i].Name := sPaper8; Inc(i);
  PaperInfo[i].Name := sPaper9; Inc(i);

  PaperInfo[i].Name := sPaper10; Inc(i);
  PaperInfo[i].Name := sPaper11; Inc(i);
  PaperInfo[i].Name := sPaper12; Inc(i);
  PaperInfo[i].Name := sPaper12; Inc(i);
  PaperInfo[i].Name := sPaper14; Inc(i);
  PaperInfo[i].Name := sPaper15; Inc(i);
  PaperInfo[i].Name := sPaper16; Inc(i);
  PaperInfo[i].Name := sPaper17; Inc(i);
  PaperInfo[i].Name := sPaper18; Inc(i);
  PaperInfo[i].Name := sPaper19; Inc(i);

  PaperInfo[i].Name := sPaper20; Inc(i);
  PaperInfo[i].Name := sPaper21; Inc(i);
  PaperInfo[i].Name := sPaper22; Inc(i);
  PaperInfo[i].Name := sPaper22; Inc(i);
  PaperInfo[i].Name := sPaper24; Inc(i);
  PaperInfo[i].Name := sPaper25; Inc(i);
  PaperInfo[i].Name := sPaper26; Inc(i);
  PaperInfo[i].Name := sPaper27; Inc(i);
  PaperInfo[i].Name := sPaper28; Inc(i);
  PaperInfo[i].Name := sPaper29; Inc(i);

  PaperInfo[i].Name := sPaper30; Inc(i);
  PaperInfo[i].Name := sPaper31; Inc(i);
  PaperInfo[i].Name := sPaper32; Inc(i);
  PaperInfo[i].Name := sPaper32; Inc(i);
  PaperInfo[i].Name := sPaper34; Inc(i);
  PaperInfo[i].Name := sPaper35; Inc(i);
  PaperInfo[i].Name := sPaper36; Inc(i);
  PaperInfo[i].Name := sPaper37; Inc(i);
  PaperInfo[i].Name := sPaper38; Inc(i);
  PaperInfo[i].Name := sPaper39; Inc(i);

  PaperInfo[i].Name := sPaper40; Inc(i);
  PaperInfo[i].Name := sPaper41; Inc(i);
  PaperInfo[i].Name := sPaper42; Inc(i);
  PaperInfo[i].Name := sPaper42; Inc(i);
  PaperInfo[i].Name := sPaper44; Inc(i);
  PaperInfo[i].Name := sPaper45; Inc(i);
  PaperInfo[i].Name := sPaper46; Inc(i);
  PaperInfo[i].Name := sPaper47; Inc(i);

  PaperInfo[i].Name := sPaper50; Inc(i);
  PaperInfo[i].Name := sPaper51; Inc(i);
  PaperInfo[i].Name := sPaper52; Inc(i);
  PaperInfo[i].Name := sPaper52; Inc(i);
  PaperInfo[i].Name := sPaper54; Inc(i);
  PaperInfo[i].Name := sPaper55; Inc(i);
  PaperInfo[i].Name := sPaper56; Inc(i);
  PaperInfo[i].Name := sPaper57; Inc(i);
  PaperInfo[i].Name := sPaper58; Inc(i);
  PaperInfo[i].Name := sPaper59; Inc(i);

  PaperInfo[i].Name := sPaper60; Inc(i);
  PaperInfo[i].Name := sPaper61; Inc(i);
  PaperInfo[i].Name := sPaper62; Inc(i);
  PaperInfo[i].Name := sPaper62; Inc(i);
  PaperInfo[i].Name := sPaper64; Inc(i);
  PaperInfo[i].Name := sPaper65; Inc(i);
  PaperInfo[i].Name := sPaper66; Inc(i);
  PaperInfo[i].Name := sPaper67; Inc(i);
  PaperInfo[i].Name := sPaper68; Inc(i);
  // new papers
  PaperInfo[i].Name := sPaper69; Inc(i);

  PaperInfo[i].Name := sPaper70; Inc(i);
  PaperInfo[i].Name := sPaper71; Inc(i);
  PaperInfo[i].Name := sPaper72; Inc(i);
  PaperInfo[i].Name := sPaper72; Inc(i);
  PaperInfo[i].Name := sPaper74; Inc(i);
  PaperInfo[i].Name := sPaper75; Inc(i);
  PaperInfo[i].Name := sPaper76; Inc(i);
  PaperInfo[i].Name := sPaper77; Inc(i);
  PaperInfo[i].Name := sPaper78; Inc(i);
  PaperInfo[i].Name := sPaper79; Inc(i);
  
  PaperInfo[i].Name := sPaper80; Inc(i);
  PaperInfo[i].Name := sPaper81; Inc(i);
  PaperInfo[i].Name := sPaper82; Inc(i);
  PaperInfo[i].Name := sPaper82; Inc(i);
  PaperInfo[i].Name := sPaper84; Inc(i);
  PaperInfo[i].Name := sPaper85; Inc(i);
  PaperInfo[i].Name := sPaper86; Inc(i);
  PaperInfo[i].Name := sPaper87; Inc(i);
  PaperInfo[i].Name := sPaper88; Inc(i);
  PaperInfo[i].Name := sPaper89; Inc(i);

  PaperInfo[i].Name := sPaper90; Inc(i);
  PaperInfo[i].Name := sPaper91; Inc(i);
  PaperInfo[i].Name := sPaper92; Inc(i);
  PaperInfo[i].Name := sPaper92; Inc(i);
  PaperInfo[i].Name := sPaper94; Inc(i);
  PaperInfo[i].Name := sPaper95; Inc(i);
  PaperInfo[i].Name := sPaper96; Inc(i);
  PaperInfo[i].Name := sPaper97; Inc(i);
  PaperInfo[i].Name := sPaper98; Inc(i);
  PaperInfo[i].Name := sPaper99; Inc(i);
  
  PaperInfo[i].Name := sPaper100; Inc(i);
  PaperInfo[i].Name := sPaper101; Inc(i);
  PaperInfo[i].Name := sPaper102; Inc(i);
  PaperInfo[i].Name := sPaper103; Inc(i);
  PaperInfo[i].Name := sPaper104; Inc(i);
  PaperInfo[i].Name := sPaper105; Inc(i);
  PaperInfo[i].Name := sPaper106; Inc(i);
  PaperInfo[i].Name := sPaper107; Inc(i);
  PaperInfo[i].Name := sPaper108; Inc(i);
  PaperInfo[i].Name := sPaper109; Inc(i);
  
  PaperInfo[i].Name := sPaper110; Inc(i);
  PaperInfo[i].Name := sPaper111; Inc(i);
  PaperInfo[i].Name := sPaper112; Inc(i);
  PaperInfo[i].Name := sPaper113; Inc(i);
  PaperInfo[i].Name := sPaper114; Inc(i);
  PaperInfo[i].Name := sPaper115; Inc(i);
  PaperInfo[i].Name := sPaper116; Inc(i);
  PaperInfo[i].Name := sPaper117; Inc(i);
  PaperInfo[i].Name := sPaper118; Inc(i);
end;

destructor TfrPrinter.Destroy;
begin
  FreeMem(FDevice, 128);
  FreeMem(FDriver, 128);
  FreeMem(FPort, 128);
  FPaperNames.Free;
  FPrinters.Free;
  inherited Destroy;
end;

{$IFNDEF MSWINDOWS}
 {
    DMPAPER_LETTER                  = 1;  sPaper1 = 'Letter, 8 1/2 x 11"';
    DMPAPER_LETTERSMALL             = 2;  sPaper2 = 'Letter small, 8 1/2 x 11"';
    DMPAPER_TABLOID                 = 3;  sPaper3 = 'Tabloid, 11 x 17"';
    DMPAPER_LEDGER                  = 4;  sPaper4 = 'Ledger, 17 x 11"';
    DMPAPER_LEGAL                   = 5;  sPaper5 = 'Legal, 8 1/2 x 14"';
    DMPAPER_STATEMENT               = 6;  sPaper6 = 'Statement, 5 1/2 x 8 1/2"';
    DMPAPER_EXECUTIVE               = 7;  sPaper7 = 'Executive, 7 1/4 x 10 1/2"';
    DMPAPER_A3                      = 8;  sPaper8 = 'A3 297 x 420 mm';
    DMPAPER_A4                      = 9;  sPaper9 = 'A4 210 x 297 mm';
    DMPAPER_A4SMALL                 = 10; sPaper10 = 'A4 small sheet, 210 x 297 mm';
    DMPAPER_A5                      = 11; sPaper11 = 'A5 148 x 210 mm';
    DMPAPER_B4                      = 12; sPaper12 = 'B4 250 x 354 mm';
    DMPAPER_B5                      = 13; sPaper13 = 'B5 182 x 257 mm';
    DMPAPER_FOLIO                   = 14; sPaper14 = 'Folio, 8 1/2 x 13"';
    DMPAPER_QUARTO                  = 15; sPaper15 = 'Quarto Sheet, 215 x 275 mm';
    DMPAPER_10X14                   = 16; sPaper16 = '10 x 14"';
    DMPAPER_11X17                   = 17; sPaper17 = '11 x 17"';
    DMPAPER_NOTE                    = 18; sPaper18 = 'Note, 8 1/2 x 11"';
    DMPAPER_ENV_9                   = 19; sPaper19 = '9 Envelope, 3 7/8 x 8 7/8"';
    DMPAPER_ENV_10                  = 20; sPaper20 = '#10 Envelope, 4 1/8  x 9 1/2"';

    DMPAPER_ENV_11                  = 21; sPaper21 = '#11 Envelope, 4 1/2 x 10 3/8"';
    DMPAPER_ENV_12                  = 22; sPaper22 = '#12 Envelope, 4 3/4 x 11"';
    DMPAPER_ENV_14                  = 23; sPaper23 = '#14 Envelope, 5 x 11 1/2"';
    DMPAPER_CSHEET                  = 24; sPaper24 = 'C Sheet, 17 x 22"';
    DMPAPER_DSHEET                  = 25; sPaper25 = 'D Sheet, 22 x 34"';
    DMPAPER_ESHEET                  = 26; sPaper26 = 'E Sheet, 34 x 44"';
    DMPAPER_ENV_DL                  = 27; sPaper27 = 'DL Envelope, 110 x 220 mm';
    DMPAPER_ENV_C5                  = 28; sPaper28 = 'C5 Envelope, 162 x 229 mm';
    DMPAPER_ENV_C3                  = 29; sPaper29 = 'C3 Envelope,  324 x 458 mm';
    DMPAPER_ENV_C4                  = 30; sPaper30 = 'C4 Envelope,  229 x 324 mm';
    DMPAPER_ENV_C6                  = 31; sPaper31 = 'C6 Envelope,  114 x 162 mm';
    DMPAPER_ENV_C65                 = 32; sPaper32 = 'C65 Envelope, 114 x 229 mm';
    DMPAPER_ENV_B4                  = 33; sPaper33 = 'B4 Envelope,  250 x 353 mm';
    DMPAPER_ENV_B5                  = 34; sPaper34 = 'B5 Envelope,  176 x 250 mm';
    DMPAPER_ENV_B6                  = 35; sPaper35 = 'B6 Envelope,  176 x 125 mm';
    DMPAPER_ENV_ITALY               = 36; sPaper36 = 'Italy Envelope, 110 x 230 mm';
    DMPAPER_ENV_MONARCH             = 37; sPaper37 = 'Monarch Envelope, 3 7/8 x 7 1/2"';
    DMPAPER_ENV_PERSONAL            = 38; sPaper38 = '6 3/4 Envelope, 3 5/8 x 6 1/2"';
    DMPAPER_FANFOLD_US              = 39; sPaper39 = 'US Std Fanfold, 14 7/8 x 11"';
    DMPAPER_FANFOLD_STD_GERMAN      = 40; sPaper40 = 'German Std Fanfold, 8 1/2 x 12"';
    DMPAPER_FANFOLD_LGL_GERMAN      = 41; sPaper41 = 'German Legal Fanfold, 8 1/2 x 13"';
     
    DMPAPER_ISO_B4                  = 42; sPaper42 = 'B4 (ISO) 250 x 353 mm';
    DMPAPER_JAPANESE_POSTCARD       = 43; sPaper43 = 'Japanese Postcard 100 x 148 mm';
    DMPAPER_9X11                    = 44; sPaper44 = '9 x 11"';
    DMPAPER_10X11                   = 45; sPaper45 = '10 x 11"';
    DMPAPER_15X11                   = 46; sPaper46 = '15 x 11"';
    DMPAPER_ENV_INVITE              = 47; sPaper47 = 'Envelope Invite 220 x 220 mm';
    DMPAPER_RESERVED_48             = 48; sPaper48 = '???? Reservado 48'
    DMPAPER_RESERVED_49             = 49; sPaper49 = '???? Reservado 49'
    DMPAPER_LETTER_EXTRA            = 50; sPaper50 = 'Letter Extra 9/275 x 12"';
    DMPAPER_LEGAL_EXTRA             = 51; sPaper51 = 'Legal Extra 9/275 x 15"';
    DMPAPER_TABLOID_EXTRA           = 52; sPaper52 = 'Tabloid Extra 11.69 x 18"';
    DMPAPER_A4_EXTRA                = 53; sPaper53 = 'A4 Extra 9.27 x 12.69"';
    DMPAPER_LETTER_TRANSVERSE       = 54; sPaper54 = 'Letter Transverse 8/275 x 11"';
    DMPAPER_A4_TRANSVERSE           = 55; sPaper55 = 'A4 Transverse 210 x 297 mm';
    DMPAPER_LETTER_EXTRA_TRANSVERSE = 56; sPaper56 = 'Letter Extra Transverse 9/275 x 12"';
    DMPAPER_A_PLUS                  = 57; sPaper57 = 'SuperASuperAA4 227 x 356 mm';
    DMPAPER_B_PLUS                  = 58; sPaper58 = 'SuperBSuperBA3 305 x 487 mm';
    DMPAPER_LETTER_PLUS             = 59; sPaper59 = 'Letter Plus 8.5 x 12.69"';
    DMPAPER_A4_PLUS                 = 60; sPaper60 = 'A4 Plus 210 x 330 mm';
    DMPAPER_A5_TRANSVERSE           = 61; sPaper61 = 'A5 Transverse 148 x 210 mm';
    DMPAPER_B5_TRANSVERSE           = 62; sPaper62 = 'B5 (JIS) Transverse 182 x 257 mm';
    DMPAPER_A3_EXTRA                = 63; sPaper63 = 'A3 Extra 322 x 445 mm';
    DMPAPER_A5_EXTRA                = 64; sPaper64 = 'A5 Extra 174 x 235 mm';
    DMPAPER_B5_EXTRA                = 65; sPaper65 = 'B5 (ISO) Extra 201 x 276 mm';
    DMPAPER_A2                      = 66; sPaper66 = 'A2 420 x 594 mm';
    DMPAPER_A3_TRANSVERSE           = 67; sPaper67 = 'A3 Transverse 297 x 420 mm';
    DMPAPER_A3_EXTRA_TRANSVERSE     = 68; sPaper68 = 'A3 Extra Transverse 322 x 445 mm';

    DMPAPER_DBL_JAPANESE_POSTCARD           = 69; // 200x148
    DMPAPER_A6                              = 70; // 105X148
    DMPAPER_JENV_KAKU2                      = 71; // 240X132
    DMPAPER_JENV_KAKU3                      = 72; // 216X277
    DMPAPER_JENV_CHOU3                      = 73; // 120X235
    DMPAPER_JENV_CHOU4                      = 74; // 90X205
    DMPAPER_LETTER_ROTATED                  = 75; // 279.4x215.9
    DMPAPER_A3_ROTATED                      = 76; // 420x297
    DMPAPER_A4_ROTATED                      = 77; // 297X210
    DMPAPER_A5_ROTATED                      = 78; // 210X148
    DMPAPER_B4_JIS_ROTATED                  = 79; // 364X257
    DMPAPER_B5_JIS_ROTATED                  = 80; // 257X182
    DMPAPER_JAPANESE_POSTCARD_ROTATED       = 81; // 148X100
    DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED   = 82; // 148X200
    DMPAPER_A6_ROTATED                      = 83; // 148X105
    DMPAPER_JENV_KAKU2_ROTATED              = 84; // 332X240
    DMPAPER_JENV_KAKU3_ROTATED              = 85; // 277X216
    DMPAPER_JENV_CHOU3_ROTATED              = 86; // 235X120
    DMPAPER_JENV_CHOU4_ROTATED              = 87; // 205X90
    DMPAPER_B6_JIS                          = 88; // 128X122
    DMPAPER_B6_JIS_ROTATED                  = 89; // 182X128
    DMPAPER_12X11                           = 90; // 304.8X279.4
    DMPAPER_JENV_YOU4                       = 91; // 105X235
    DMPAPER_JENV_YOU4_ROTATED               = 92; // 235X105
    DMPAPER_P16K                            = 93; // 146X215
    DMPAPER_P32K                            = 94; // 97X151
    DMPAPER_P32KBIG                         = 95; // 97X151
    DMPAPER_PENV_1                          = 96; // 102X165
    DMPAPER_PENV_2                          = 97; // 102X176
    DMPAPER_PENV_3                          = 98; // 125X176
    DMPAPER_PENV_4                          = 99; // 110X208
    DMPAPER_PENV_5                          = 100; // 110X220
    DMPAPER_PENV_6                          = 101; // 120X230
    DMPAPER_PENV_7                          = 102; // 160X230
    DMPAPER_PENV_8                          = 103; // 120X309
    DMPAPER_PENV_9                          = 104; // 229X324
    DMPAPER_PENV_10                         = 105; // 324X458
    DMPAPER_P16K_ROTATED                    = 106; // 215X146
    DMPAPER_P32K_ROTATED                    = 107; // 151X97
    DMPAPER_P32KBIG_ROTATED                 = 108; // 151X97
    DMPAPER_PENV_1_ROTATED                  = 109; // 165X102
    DMPAPER_PENV_2_ROTATED                  = 110; // 176X102
    DMPAPER_PENV_3_ROTATED                  = 111; // 176X125
    DMPAPER_PENV_4_ROTATED                  = 112; // 208X110
    DMPAPER_PENV_5_ROTATED                  = 113; // 220X110
    DMPAPER_PENV_6_ROTATED                  = 114; // 230X120
    DMPAPER_PENV_7_ROTATED                  = 115; // 230X160
    DMPAPER_PENV_8_ROTATED                  = 116; // 309X120
    DMPAPER_PENV_9_ROTATED                  = 117; // 324X229
    DMPAPER_PENV_10_ROTATED                 = 118; // 458X324
}

function MatchWindowsPaper(aPaperName: string): integer;
var
  i, aWidth, aHeight, BDeltaW, BDeltaH, BIndex,Cw,Ch: Integer;
  PaperRect: TPaperRect;
  ValidSize: Boolean;
  {$ifdef DbgPrinter_detail}
  BestDw, BestDh: Integer;

  function dbgsp: string;
  begin
    if i>=0 then
      result := format(' askW=%d askH=%d rspW=%d rspH=%d',
        [aWidth, aHeight, PPDPaperInfo[i].X, PPDPaperInfo[i].Y]);
  end;
  {$endif}
begin
  result := -1;

  ValidSize := true;
  try
    PaperRect := prn.Printer.PaperSize.PaperRectOf[aPaperName];
    aWidth := round((PaperRect.PhysicalRect.Right-PaperRect.PhysicalRect.Left) * 72 / prn.Printer.XDPI);
    aHeight := round((PaperRect.PhysicalRect.Bottom-PaperRect.PhysicalRect.Top) * 72 / prn.Printer.YDPI);
  except
    ValidSize := false;
  end;

  BIndex := -1;
  BDeltaW := 2013;
  BDeltaH := 2013;
  {$ifdef DbgPrinter_detail}
  BestDw := BDeltaW;
  BestDh := BDeltaH;
  {$endif}

  // name
  for i:=0 to PAPERCOUNT-1 do
  with PPDPaperInfo[i] do
  begin
    if CompareText(Name, aPaperName)=0 then
    begin
      // found
      {$ifdef DbgPrinter_detail}DebugLn('i=%d Perfect Name Match %s %s',[i, Name, dbgsp]);{$Endif}
      result := Typ;
      exit;
    end;
  end;

  // size match
  for i:=0 to PAPERCOUNT-1 do
  with PPDPaperInfo[i] do
  begin
    if ValidSize and (X>=aWidth) and (Y>=aHeight) then
    begin
      // only interested on papers that are same or bigger size than match paper
      Cw := X-aWidth;
      Ch := Y-aHeight;
      if (Cw=0) and (Ch=0) then
      begin
        // no need to look more, perfect match
        {$ifdef DbgPrinter_detail}DebugLn('i=%d Perfect Size Match w=%d h=%d "%s"->%s %s',[i,X,Y,aPaperName,Name, dbgsp]);{$Endif}
        BIndex := i;
        break;
      end else
      begin
        {$ifdef DbgPrinter_detail}
        if (Cw<BestDw) and (Ch<BestDh) then begin
          BestDw := Cw;
          BestDh := Ch;
        end;
        {$endif}
        if (Cw<6) and (Ch<6) and (Cw<=BDeltaW) and (Cw<=BDeltaH) then
        begin
          {$ifdef DbgPrinter_detail}DebugLn('i=%d Close Size cw=%d ch=%d  "%s"->%s %s',[i,cw,ch,aPaperName,Name, dbgsp]);{$endif}
          // we are interested only on differences with searched paper of
          // about 2 mm or less (1 mm is aprox 3 points)
          BIndex := i;
          BDeltaW := Cw;
          BDeltaH := CH;
        end
        {$ifdef DbgPrinter_detail}
        //else
        //  DebugLn('i=%d Missed cw=%d ch=%d %s',[i, cw, ch, Name])
        {$endif}
      end;
    end;
  end;

  if bIndex>=0 then
  begin
    result := PPDPaperInfo[bIndex].Typ
  end
  {$ifdef DbgPrinter_detail}
  else
    DebugLn('Matching Paper %s failed BestDw=%d BestDh=%d',[aPaperName, BestDw, BestDh])
  {$endif}
  ;
end;

{$ENDIF}

procedure TfrPrinter.GetSettings(PrinterChanged: boolean = true);
var
  i: Integer;
  n: Integer;
begin
  {$ifdef DbgPrinter}
  DebugLnEnter(['TfrPrinter.GetSettings INIT: PrinterChanged: ', PrinterChanged]);
  {$endif}
  if fPrinter.Printers.Count>0 then
  begin
    if PrinterChanged then begin
      fPaperNames.Assign(fPrinter.PaperSize.SupportedPapers);
      PaperSizesNum:=FPaperNames.Count;
    end;
    {$ifdef DbgPrinter}
    DebugLn(['Filling windows paper numbers for ', PaperSizesNum,' papers ....']);
    {$endif}
    {$IFNDEF MSWINDOWS}
    // Under no windows platforms, there is no unique number that indentify
    // papers, so we have to fill here our own numbers, this should be based
    // on windows numbers so stored page numbers could be used under any
    // platform.
    //
    // Under cups (ie, using ppd files), ppd file builders can add paper names
    // not included in ref [1], that difficult the selection of papers
    if PrinterChanged then
      for i:=0 to FPaperNames.Count-1 do
      begin

        n := MatchWindowsPaper(FPaperNames[i]);
        if n<0 then
          // it's a non windows standard paper, mark it
          // as custom size paper but one that we will be
          // able to recognize later as an index within the
          // list of papers for current printer
          n := 1000 + i;

        PaperSizes[i] := n;
        FPaperNames.Objects[i] := TObject(PtrInt(n)); // this is used under page options
                                              // dialog to show if the paper item
                                              // is a windows paper or other thing
      end;
    {$ELSE}
    for i:=0 to FPaperNames.Count-1 do
      PaperSizes[i] := PtrInt(FPaperNames.Objects[i]);
    {$ENDIF}
    
    {$IFDEF DbgPrinter_detail}
    DebugLn(['Dump printer List of papers:']);

    n := FPapernames.IndexOf(FPrinter.PaperSize.PaperName);
    if n<0 then
      // try to get the PaperIndex of the default paper
      n := DefaultPaperIndex();

    // don't update the paper size so custom papersizes as 1000's and 2000's
    // will be preserved and so, the right paper will be selected when choosing
    // the same printer.
    //
    //PaperSize := PaperSizes[n];

    /// Debug Information
    for i:=0 to FPaperNames.Count-1 do begin
      DbgOut('%4d ',[i]);
      if i=n then
        DbgOut('*')
      else
        DbgOut(' ');
      DebugLn(' WinNum=%5d Paper=%s', [PaperSizes[i], FPaperNames[i]]);
    end;
    {$Endif}
    {$IFDEF DbgPrinter}
    DebugLn('Current PaperSize is %d',[PaperSize]);
    {$ENDIF}

    try
      // update paper size in std pt units
      PaperWidth := round(fPrinter.PaperSize.Width * 72 / fPrinter.XDPI);
      PaperHeight := round(fPrinter.PaperSize.Height * 72 / fPrinter.YDPI);
      Orientation := fPrinter.Orientation;
    except
      PaperWidth:=1;
      PaperHeight:=1;
      // let it as it was ....
      raise
    end;
  end;
  {$ifdef DbgPrinter}
  DebugLnExit(['TfrPrinter.GetSettings DONE: PrinterChanged: ', PrinterChanged]);
  {$endif}
end;

procedure TfrPrinter.SetSettings;
var
  i, n: Integer;
  {$ifdef DbgPrinter}
  s: string;
  {$endif}
begin
  {$ifdef DbgPrinter}
  WriteStr(s, Orientation);
  DebugLnEnter(['TfrPrinter.SetSettings INIT: PrinterIndex=',FPrinterIndex]);
  DebugLn(['PaperSize  =', PaperSize]);
  DebugLn(['PaperWidth =', PaperWidth]);
  DebugLn(['PaperHeight=', PaperHeight]);
  DebugLn(['Orientation=', s]);
  {$Endif}
  // if selected printer is default printer, ie our virtual printer
  // then select our own set of papers
  if UseVirtualPrinter then
  begin
    (*
    // a papersize has been selected, maybe from a page recently loaded
    // or from a previous selected printer, the old PrinterIndex, is not
    // the new printer index.
    //
    // based on the old information, find a suitable paper within our own
    // custom paper list.
    FPaperNames.Clear;
    for i:=0 to PAPERCOUNT-1 do begin
      FPaperNames.Add(PaperInfo[i].Name);
      if (WinPaperSize<>256)and(WinPaperSize=PaperInfo[i].Typ) then begin

        PrinterIndex := i;

        {$IFDEF MSWINDOWS}
        PaperWidth := PaperInfo[i].X;
        PaperHeight:= PaperInfo[i].Y;
        {$ELSE}
        PaperWidth := PPDPaperInfo[i].X;
        PaperWidth := PPDPaperInfo[i].Y;
        {$ENDIF}

        if Orientation = poLandscape then
        begin
          n := PaperWidth;
          PaperWidth := PaperHeight;
          PaperHeight := n;
        end;

      end;
    end;
    *)
    {$ifdef DbgPrinter}
    DebugLn('DefaultPrinter, setting up defaultSet of Papers');
    {$endif}
    FPaperNames.Clear;
    for i := 0 to PAPERCOUNT - 1 do
    begin
      FPaperNames.AddObject(PaperInfo[i].Name, TObject(PtrInt(PaperInfo[i].Typ)));
      PaperSizes[i] := PaperInfo[i].Typ;
      if (PaperSize <> $100) and (PaperSize = PaperInfo[i].Typ) then
      begin
      {$ifdef DbgPrinter}
      DebugLn(['DefaultPrinter, PaperSize=',PaperSize,' Corresponds to ', PaperInfo[i].Name]);
      {$endif}
        PaperWidth := PaperInfo[i].X;
        PaperHeight := PaperInfo[i].Y;
        if Orientation = poLandscape then
        begin
          n := PaperWidth;
          PaperWidth := PaperHeight;
          PaperHeight := n;
        end;
      end;
    end;
    PaperSizesNum := PAPERCOUNT;
    {$IFDEF DbgPrinter}
    DebugLnExit('TfrPrinter.SetSettings: EXIT (default printer)');
    {$ENDIF}
    Exit;
  end;

  FPrinter.Orientation := Orientation;
  
  if PaperSize>=1000 then begin
    // paper sizes above 1000 have an encoded index
    // in order to use a real paper from the list instead of a custom
    // paper for not being a standard windows paper
    //
    i := PaperSize-1000;
    if (i>=0)and(i<FPaperNames.Count) then
      FPrinter.PaperSize.PaperName := FPaperNames[i];
    {$IFDEF DbgPrinter}
    DebugLn(['PaperSize (NoWin)CupsPaper requested: PaperSize=', PaperSize,' i=',i,' Paper=',FPrinter.PaperSize.PaperName]);
    {$ENDIF}
  end else
  if PaperSize=256 then begin
    // todo: real USER custom sized papers are handled here
    //    requested custom paper size currently is not
    //    supported by printer4lazarus
    DebugLn('SetCustomPaperSize REQUESTED, not yet supported...');
  end else begin
    // Standard paper sizes are handled here
    n := -1;
    for i:=0 to PaperSizesNum-1 do
      if PaperSizes[i]=PaperSize then begin
        n:=i;
        FPrinter.PaperSize.PaperName := PaperNames[i];
        break;
      end;
    if (n<0) and (PaperWidth>1) and (PaperHeight>1) then
    begin
      // this standard paperSize was not found by number
      // try to find a suitable paper size within the list
      // of printer papers based on Paper's width and height
      n := MatchPrinterPaper(PaperWidth, PaperHeight);
      if n>=0 then begin
        FPrinter.PaperSize.PaperName := FPrinter.PaperSize.SupportedPapers[n];
        // actually PaperSize is a better choice than PaperSizes[n], Update it
        PaperSizes[n] := PaperSize;
      end;
    end;

    {$IFDEF DbgPrinter}
    DebugLn(['PaperSize standard requested: PaperSize=', PaperSize,' i=',i,' Paper=', FPrinter.PaperSize.PaperName]);
    {$ENDIF}
  end;

  {FPrinter.GetPrinter(FDevice, FDriver, FPort, FDeviceMode);
  try
    FMode := GlobalLock(FDeviceMode);
    if PaperSize = $100 then
    begin
      FMode.dmFields := FMode.dmFields or DM_PAPERLENGTH or DM_PAPERWIDTH;
      FMode.dmPaperLength := PaperHeight;
      FMode.dmPaperWidth := PaperWidth;
    end;

    if (FMode.dmFields and DM_PAPERSIZE) <> 0 then
      FMode.dmPaperSize := PaperSize;

    if (FMode.dmFields and DM_ORIENTATION) <> 0 then
      if Orientation = poPortrait then
        FMode.dmOrientation := DMORIENT_PORTRAIT else
        FMode.dmOrientation := DMORIENT_LANDSCAPE;

    if (FMode.dmFields and DM_COPIES) <> 0 then
      FMode.dmCopies := 1;

    FPrinter.SetPrinter(FDevice, FDriver, FPort, FDeviceMode);
  finally
    GlobalUnlock(FDeviceMode);
  end;
  }
  
  GetSettings( False );
  {$IFDEF DbgPrinter}
  DebugLnExit('TfrPrinter.SetSettings DONE');
  {$ENDIF}
end;

procedure TfrPrinter.FillPrnInfo(var p: TfrPrnInfo);
var
  kx, ky: Double;
begin
  {$ifdef DbgPrinter}
  DebugLnEnter(['TfrPrinter.FillPrnInfo INIT IsDefaultPrinter=',UseVirtualPrinter]);
  {$endif}

  kx := 93 / 1.022;
  ky := 93 / 1.015;
  
  if UseVirtualPrinter then
  begin
    with p do
    begin
      Pgw := Round(PaperWidth * kx / 254);
      Pgh := Round(PaperHeight * ky / 254);
      Ofx := Round(50 * kx / 254);
      Ofy := Round(50 * ky / 254);
      Pw := Pgw - Ofx * 2;
      Ph := Pgh - Ofy * 2;

      //fix DPI for virtual printer
      ResX := 300;
      ResY := 300;
    end
  end
  else
  begin
    with p, FPrinter do
    begin
      kx := kx / XDPI; //GetDeviceCaps(Handle, LOGPIXELSX);
      ky := ky / YDPI; //GetDeviceCaps(Handle, LOGPIXELSY);
      
      // printer sizes
      with PaperSize.PaperRect do begin
        PPgw := PhysicalRect.Right-PhysicalRect.Left;
        Ppgh := PhysicalRect.Bottom-PhysicalRect.Top;
        POFx := WorkRect.Left;
        POFy := WorkRect.Top;
        PPw  := WorkRect.Right-WorkRect.Left; // this is the same as PageWidth
        PPh  := WorkRect.Bottom-WorkRect.Top; // this is the same as PageHeight
      end;
      
      // screen sizes
      Pgw := round(PPgw * kx);
      Pgh := round(PPgh * ky);
      Ofx := round(POfx * kx);
      Ofy := round(POfy * ky);
      Pw  := round(PPw  * kx);
      Ph  := round(PPh  * ky);

      ResX := XDPI;
      ResY := YDPI;
      
      {$IFDEF DbgPrinter}
      DebugLn(['[prn] PPgw/PPgh=', PPgw,'/',Ppgh,' [scr] Pgw/Pgh=', Pgw,'/',Pgh]);
      DebugLn(['[prn] POfx/POfy=', POfx,'/',Pofy,' [scr] Ofx/Ofy=', Ofx,'/',Ofy]);
      DebugLn(['[prn]  PPw/ PPh=', PPw,'/',PPh,  ' [scr]  Pw/ Ph=', Pw,'/',Ph]);
      {$ENDIF}
    end;
  end;
  {$ifdef DbgPrinter}
  DebugLnExit('TfrPrinter.FillPrnInfo END');
  {$endif}
end;

function TfrPrinter.IsEqual(pgSize, pgWidth, pgHeight: Integer;
  pgOr: TPrinterOrientation): Boolean;
begin
  if (PaperSize = pgSize) and (pgSize = $100) then
    Result := (PaperSize = pgSize) and (PaperWidth = pgWidth) and
     (PaperHeight = pgHeight) and (Orientation = pgOr)
  else
    Result := (PaperSize = pgSize) and (Orientation = pgOr);
end;

procedure TfrPrinter.SetPrinterInfo(pgSize, pgWidth, pgHeight: Integer;
  pgOr: TPrinterOrientation);
begin
  {$ifdef DbgPrinter}
  DebugLnEnter('TfrPrinter.SetPrinterInfo INIT pgSize=%d pgWidth=%d pgHeight=%d pgOr=%d',
    [pgSize, pgWidth, pgHeight, ord(pgOr)]);
  {$endif}
  if IsEqual(pgSize, pgWidth, pgHeight, pgOr) then
  begin
    {$ifdef DbgPrinter}
    DebugLnExit('TfrPrinter.SetPrinterInfo EXIT: same properties');
    {$endif}
    Exit;
  end;
  PaperSize:=PgSize;
  PaperWidth:= pgWidth;
  PaperHeight:=pgHeight;
  Orientation:=pgOr;
  SetSettings;
  {$ifdef DbgPrinter}
  DebugLnExit('TfrPrinter.SetPrinterInfo END');
  {$endif}
end;

function TfrPrinter.GetArrayPos(pgSize: Integer): Integer;
var
  i: Integer;
begin
  Result := PaperSizesNum - 1;
  for i := 0 to PaperSizesNum - 1 do
  begin
    if PaperSizes[i] = pgSize then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TfrPrinter.DefaultPaperIndex: Integer;
begin
  Result:= FPaperNames.IndexOf(FPrinter.PaperSize.DefaultPaperName);
  if Result<0 then
    Result:=0;
end;

function TfrPrinter.DefaultPageSize: Integer;
var
  Indx: Integer;
begin
  if FPaperNames.Count>0  then
  begin
    Indx := DefaultPaperIndex;
    result := PaperSizes[Indx];
  end else
    result := 9;
end;

function TfrPrinter.UseVirtualPrinter: boolean;
begin
  result := FPrinterIndex = FDefaultPrinter;
end;

{$IFDEF DbgPrinter}
procedure TfrPrinter.DumpPrinterInfo;
begin

  DbgOut(['PrinterIndex=',FPrinterIndex]);
  if (FPrinters<>nil)and(FPrinters.Count>0) then begin
    if FPrinterIndex>=0 then
      DbgOut([' (',FPrinters[FPrinterIndex],')'])
  end else
    DbgOut(' (no defined internal list of printers)');
  DebugLn([' Is Default(Virtual) printer=',UseVirtualPrinter]);
  if FPrinter=nil then
    DebugLn('SysPrinter is nil')
  else
    DebugLn(['Sys Printer: Index = ', FPrinter.PrinterIndex,' Name=',FPrinter.PrinterName]);
end;
{$ENDIF}

procedure TfrPrinter.SetPrinterIndex(Value: Integer);
begin
  {$IFDEF DbgPrinter}
  DebugLnEnter(['TfrPrinter.SetPrinterIndex INIT: Value=',Value,' IsDefaultPrinter=',Value=FDefaultPrinter]);
  {$ENDIF}
  FPrinterIndex := Value;
  if UseVirtualPrinter then
    SetSettings
  else
    if FPrinter.Printers.Count > 0 then
    begin
      FPrinter.PrinterIndex := Value;
      GetSettings;
    end;
  {$IFDEF DbgPrinter}
  DebugLnExit(['TfrPrinter.SetPrinterIndex DONE']);
  {$ENDIF}
end;

function TfrPrinter.GetPaperNames: TStringList;
begin
  result := FPaperNames;
end;

function TfrPrinter.MatchPrinterPaper(const aWidth, aHeight: Integer): integer;
var
  i,dw,dh: Integer;
begin
  result := -1;
  if FPrinter=nil then
    exit;

  with FPrinter.PaperSize do
    for i:=0 to SupportedPapers.Count-1 do
    begin
      try
        with PaperRectOf[SupportedPapers[i]].PhysicalRect do
        begin
          dw := round((Right-Left)*72/FPrinter.XDPI) - aWidth;
          dh := round((Bottom-Top)*72/FPrinter.YDPI) - aHeight;
          if (dw>=0)and(dw<=6) and (dh>=0)and(dh<=6) then begin
            result := i;
            exit;
          end;
        end;
      except
      end;
    end;
end;

procedure TfrPrinter.SetPrinter(Value: TPrinter);
begin
  {$ifdef DbgPrinter}
  DebugLnEnter('TfrPrinter.SetPrinter: INIT',[]);
  DumpPrinterInfo;
  {$endif}
  FPrinters.Clear;
  FPrinterIndex := 0;
  FPrinter:=Value;
  if FPrinter.Printers.Count > 0 then
  begin
    FPrinters.Assign(FPrinter.Printers);
    FPrinterIndex := FPrinter.PrinterIndex;
  end;
  try
    GetSettings;
  finally
    FPrinters.Add(sDefaultPrinter);
    FDefaultPrinter := FPrinters.Count - 1;
  end;
  {$ifdef DbgPrinter}
  DumpPrinterInfo;
  DebugLnExit('TfrPrinter.SetPrinter: DONE',[]);
  {$endif}
end;

{
procedure ExportLista;
var
  i: Integer;
  F: TextFile;
begin
  AssignFile(F,'Lista.pas');
  Rewrite(f);
  for i:=0 to PaperCount-2 do begin
    WriteLn(F,'    (Num:',PaperInfo[i].Typ,'; Name: ''''; X:',round(PPDPaperInfo[i].X*72/254),'; Y:',round(PPDPaperInfo[i].Y*72/254),'),');
  end;
  Close(f);
end;
}

{----------------------------------------------------------------------------}

initialization

finalization
  if Assigned( GlobalPrn ) then
    GlobalPrn.Free;

end.
