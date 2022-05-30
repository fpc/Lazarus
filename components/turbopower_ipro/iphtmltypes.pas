unit IpHtmlTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types;

const
  MAXINTS = 4096; {buffer size - this should be way more than needed}
  TINTARRGROWFACTOR = 64;
  IPMAXFRAMES = 256; {maximum number of frames in a single frameset}
  DEFAULT_PRINTMARGIN = 0.5; {inches}
  FONTSIZESVALUESARRAY : array[0..6] of integer = (8,10,12,14,18,24,36);
  MAXWORDS = 65536;
  DEFAULT_LINKS_UNDERLINED = false;

  ZOOM_TO_FIT = 0;
  ZOOM_TO_FIT_WIDTH = -1;
  ZOOM_TO_FIT_HEIGHT = -2;

  ShyChar = #1; {character used to represent soft-hyphen in strings}
  NbspChar = #2; {character used to represent no-break space in strings}
  NAnchorChar = #3 ; {character used to represent an Anchor }
  NbspUtf8 = #194#160;  {utf8 code of no-break space character}
  
  LF = #10;
  CR = #13;
  
type
  TElementType = (
    etWord, etObject, etSoftLF, etHardLF, etClearLeft, etClearRight, 
    etClearBoth, etIndent, etOutdent, etSoftHyphen
  );
  
  TIpHtmlAlign = (
    haDefault, haLeft, haCenter, haRight, haJustify, haChar, haUnknown
  );
  
  TIpHtmlBreakClear = (
    hbcNone, hbcLeft, hbcRight, hbcAll
  );

  TIpHtmlButtonType = (
    hbtSubmit, hbtReset, hbtButton
  );

  TIpHtmlCellScope = (
    hcsUnspec, hcsRow, hcsCol, hcsRowGroup, hcsColGroup
  );

  TIpHtmlDirection = (
    hdLTR, hdRTL
  );

  TIpHtmlElemMarginStyle = (
    hemsAuto, // use default
    hemsPx    // pixel
    );

  TIpHtmlFontStyles = (
    hfsTT, hfsI, hfsB, hfsU, hfsSTRIKE, hfsS, hfsBIG, hfsSMALL, hfsSUB, hfsSUP
  );

  TIpHtmlFormMethod = (
    hfmGet, hfmPost
  );

  TIpHtmlFrameProp = (
    hfVoid, hfAbove, hfBelow, hfHSides, hfLhs, hfRhs, hfvSides, hfBox, hfBorder
  );

  TIpHtmlFrameScrolling = (
    hfsAuto, hfsYes, hfsNo
  );
  
  TIpHtmlHeaderSize = 1..6;

  TIpHtmlImageAlign = (
    hiaTop, hiaMiddle, hiaBottom, hiaLeft, hiaRight, hiaCenter
  );
  
  TIpHtmlInputType = (
    hitText, hitPassword, hitCheckbox, hitRadio, hitSubmit, hitReset, 
    hitFile, hitHidden, hitImage, hitButton
  );

  TIpHtmlLengthType = (
    hlUndefined, hlAbsolute, hlPercent
  );
  
  TIpHtmlMapShape = (
    hmsDefault, hmsRect, hmsCircle, hmsPoly
  );
  
  TIpHtmlOLStyle = (
    olArabic, olLowerAlpha, olUpperAlpha, olLowerRoman, olUpperRoman
  );
  
  TIpHtmlPhraseStyle = (
    hpsEM, hpsSTRONG, hpsDFN, hpsCODE, hpsSAMP, hpsKBD, hpsVAR, hpsCITE, 
    hpsABBR, hpsACRONYM
  );

  TIpHtmlObjectValueType = (
    hovtData, hovtRef, hovtObject
  );
  
  TIpHtmlPixelsType = (
    hpUndefined, hpAbsolute
  );
  
  TIpHtmlRenderDevice = (
    rdScreen, rdPrinter, rdPreview
  );

  TIpHtmlRules = (
    hrNone, hrGroups, hrRows, hrCols, hrAll
  );
  
  TIpHtmlULType = (
    ulUndefined, ulDisc, ulSquare, ulCircle
  );
  
  TIpHtmlVAlign = (
    hvaTop, hvaMiddle, hvaBottom
  );
  
  TIpHtmlVAlign3 = (
    hva3Top, hva3Middle, hva3Bottom, hva3Baseline, hva3Default
  );
  
  TIpHtmlVAlignment2 = (
    hva2Top, hva2Bottom, hva2Left, hva2Right
  );
  
  TIpScrollAction = (
    hsaHome, hsaEnd, hsaPgUp, hsaPgDn, hsaLeft, hsaRight, hsaUp, hsaDown
  );
  
  
var
  // true during print preview only, public to let print preview unit access it
  ScaleBitmaps: Boolean = False;
  ScaleFonts : Boolean = False;
  Aspect: Double = 1.0;
  BWPrinter: Boolean;
  
implementation

end.

