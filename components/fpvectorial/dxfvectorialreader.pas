{
Reads DXF files

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho

DXF is composed by records written in ASCII with the following structure:

0
SECTION
section_number
SECTION_NAME
<data>
0
ENDSEC
0

after all sections there is:

EOF

}
unit dxfvectorialreader;

{$mode objfpc}{$H+}

{.$define FPVECTORIALDEBUG}
{.$define FPVECTORIALDEBUG_POLYLINE}
{.$define FPVECTORIALDEBUG_LINE}

interface

uses
  Classes, SysUtils, Math,
  fpcanvas, fpimage,
  fpvectorial, fpvutils,
  lconvencoding;

type
  TDXFToken = class;

  TDXFTokens = TFPList;// TDXFToken;

  TDXFToken = class
    GroupCode: Integer;
    StrValue: string;
    FloatValue: double;
    IntValue: Integer;
    Childs: TDXFTokens;
    constructor Create;
    Destructor Destroy; override;
  end;

  TPolylineElement = record
    X, Y: Double;
    Color: TFPColor;
  end;

  TSPLineElement = record
    X, Y: Double;
    KnotValue: Integer;
  end;

  TLWPOLYLINEElement = record
    X, Y: Double;
  end;

  { TDXFTokenizer }

  TDXFTokenizer = class
  public
    Tokens: TDXFTokens;
    constructor Create;
    Destructor Destroy; override;
    procedure ReadFromStrings(AStrings: TStrings);
    function  IsTABLES_Subsection(AStr: string): Boolean;
    function  IsBLOCKS_Subsection(AStr: string): Boolean;
    function  IsENTITIES_Subsection(AStr: string): Boolean;
  end;

  { TvDXFVectorialReader }

  TvDXFVectorialReader = class(TvCustomVectorialReader)
  private
    FPointSeparator: TFormatSettings;
    // HEADER data
    ANGBASE: Double;
    ANGDIR: Integer;
    INSBASE, EXTMIN, EXTMAX, LIMMIN, LIMMAX: T3DPoint;
    // Calculated HEADER data
    DOC_OFFSET: T3DPoint; // The DOC_OFFSET compensates for documents with huge coordinates
    ENCODING: string; // In the format utilized by lazutils.lconvencoding
    // For building the POLYLINE objects which is composed of multiple records
    IsReadingPolyline: Boolean;
    IsReadingAttrib: Boolean;
    Polyline: array of TPolylineElement;
    //
    procedure ReadHEADER(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadTABLES(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadTABLES_TABLE(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadBLOCKS(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadBLOCKS_BLOCK(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadBLOCKS_ENDBLK(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadENTITIES(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    function  ReadENTITIES_3DFACE(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvCircularArc;
    function  ReadENTITIES_LINE(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TPath;
    function  ReadENTITIES_ARC(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvCircularArc;
    function  ReadENTITIES_CIRCLE(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvCircle;
    function  ReadENTITIES_DIMENSION(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvEntity;
    function  ReadENTITIES_ELLIPSE(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvEllipse;
    function  ReadENTITIES_INSERT(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvInsert;
    function  ReadENTITIES_TEXT(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvText;
    function  ReadENTITIES_LWPOLYLINE(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TPath;
    function  ReadENTITIES_SPLINE(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TPath;
    procedure ReadENTITIES_ATTRIB(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadENTITIES_POLYLINE(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    procedure ReadENTITIES_VERTEX(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument);
    function  ReadENTITIES_SEQEND(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TPath;
    function  ReadENTITIES_MTEXT(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvText;
    function  ReadENTITIES_LEADER(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvArrow;
    function  ReadENTITIES_POINT(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvEntity;
    function  InternalReadENTITIES(ATokenStr: string; ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvEntity;
    function  GetCoordinateValue(AStr: shortstring): Double;
    function  ConvertDXFStringToUTF8(AStr: string): string;
    //
    function DXFColorIndexToFPColor(AColorIndex: Integer): TFPColor;
    procedure DXFCoordsToFPCoords(AInX, AInY, AInZ: Double; out AOutX, AOutY, AOutZ: Double);
  public
    { General reading methods }
    Tokenizer: TDXFTokenizer;
    constructor Create; override;
    Destructor Destroy; override;
    procedure ReadFromStrings(AStrings: TStrings; AData: TvVectorialDocument); override;
  end;

implementation

const
  // Items in the HEADER section

  // $ACADVER
  DXF_AUTOCAD_2010        = 'AC1024'; // AutoCAD 2011 and 2012 too
  DXF_AUTOCAD_2007        = 'AC1021'; // AutoCAD 2008 and 2009 too
  DXF_AUTOCAD_2004        = 'AC1018'; // AutoCAD 2005 and 2006 too
  DXF_AUTOCAD_2000        = 'AC1015'; // 1999  In some docs it is proposed as AC1500, but in practice I found AC1015
                                      // http://www.autodesk.com/techpubs/autocad/acad2000/dxf/
                                      // AutoCAD 2000i and 2002 too
  DXF_AUTOCAD_R14         = 'AC1014'; // 1997  http://www.autodesk.com/techpubs/autocad/acadr14/dxf/index.htm
  DXF_AUTOCAD_R13         = 'AC1012'; // 1994
  DXF_AUTOCAD_R11_and_R12 = 'AC1009'; // 1990
  DXF_AUTOCAD_R10         = 'AC1006'; // 1988
  DXF_AUTOCAD_R9          = 'AC1004';

  // Group Codes for ENTITIES
  DXF_ENTITIES_TYPE = 0;
  DXF_ENTITIES_HANDLE = 5;
  DXF_ENTITIES_LINETYPE_NAME = 6;
  DXF_ENTITIES_APPLICATION_GROUP = 102;
  DXF_ENTITIES_AcDbEntity = 100;
  DXF_ENTITIES_MODEL_OR_PAPER_SPACE = 67; // default=0=model, 1=paper
  DXF_ENTITIES_VISIBILITY = 60; // default=0 = Visible, 1 = Invisible

  // Obtained from http://www.generalcadd.com/pdf/LivingWithAutoCAD_v4.pdf
  // and from http://sub-atomic.com/~moses/acadcolors.html
  // Valid for DXF up to AutoCad 2004, after that RGB is available
  AUTOCAD_COLOR_PALETTE: array[0..255] of TFPColor =
  (
    { QBASIC palette
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 0 - Black
    (Red: $0000; Green: $0000; Blue: $8080; Alpha: alphaOpaque), // 1 - Dark blue
    (Red: $0000; Green: $8080; Blue: $0000; Alpha: alphaOpaque), // 2 - Dark green
    (Red: $0000; Green: $8080; Blue: $8080; Alpha: alphaOpaque), // 3 - Dark cyan
    (Red: $8080; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 4 - Dark red
    (Red: $8080; Green: $0000; Blue: $8080; Alpha: alphaOpaque), // 5 - Dark Magenta
    (Red: $8080; Green: $8080; Blue: $0000; Alpha: alphaOpaque), // 6 - Dark
    (Red: $c0c0; Green: $c0c0; Blue: $c0c0; Alpha: alphaOpaque), // 7 - Light Gray
    (Red: $8080; Green: $8080; Blue: $8080; Alpha: alphaOpaque), // 8 - Medium Gray
    (Red: $0000; Green: $0000; Blue: $ffff; Alpha: alphaOpaque), // 9 - Light blue
    (Red: $0000; Green: $ffff; Blue: $0000; Alpha: alphaOpaque), // 10 - Light green
    (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque), // 11 - Light cyan
    (Red: $ffff; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 12 - Light red
    (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaOpaque), // 13 - Light Magenta
    (Red: $ffff; Green: $ffff; Blue: $0000; Alpha: alphaOpaque), // 14 - Light Yellow
    (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque),  // 15 - White }
    // AutoCAD for DOS initial palette from http://www.generalcadd.com/pdf/LivingWithAutoCAD_v4.pdf
    {(Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 0 - Black
    (Red: $ffff; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 1 - Light red
    (Red: $ffff; Green: $ffff; Blue: $0000; Alpha: alphaOpaque), // 2 - Light Yellow
    (Red: $0000; Green: $ffff; Blue: $0000; Alpha: alphaOpaque), // 3 - Light green
    (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque), // 4 - Light cyan
    (Red: $0000; Green: $0000; Blue: $ffff; Alpha: alphaOpaque), // 5 - Light blue
    (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaOpaque), // 6 - Light Magenta
    (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque), // 7 - White
    (Red: $8080; Green: $8080; Blue: $8080; Alpha: alphaOpaque), // 8 - Medium Gray
    (Red: $8080; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 9 - Dark red
    (Red: $8080; Green: $8080; Blue: $0000; Alpha: alphaOpaque), // 10 - Dark Yellow
    (Red: $0000; Green: $8080; Blue: $0000; Alpha: alphaOpaque), // 11 - Dark green
    (Red: $0000; Green: $8080; Blue: $8080; Alpha: alphaOpaque), // 12 - Dark cyan
    (Red: $0000; Green: $0000; Blue: $8080; Alpha: alphaOpaque), // 13 - Dark blue
    (Red: $8080; Green: $0000; Blue: $8080; Alpha: alphaOpaque), // 14 - Dark Magenta
    (Red: $c0c0; Green: $c0c0; Blue: $c0c0; Alpha: alphaOpaque), // 15 - Light Gray}
    // Initial palette from http://sub-atomic.com/~moses/acadcolors.html
    // Agrees with http://gallery.proficad.eu/tools/AutoCAD-Viewer.aspx
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 0 - Black
    (Red: $ffff; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 1 - Light red
    (Red: $ffff; Green: $ffff; Blue: $0000; Alpha: alphaOpaque), // 2 - Light Yellow
    (Red: $0000; Green: $ffff; Blue: $0000; Alpha: alphaOpaque), // 3 - Light green
    (Red: $0000; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque), // 4 - Light cyan
    (Red: $0000; Green: $0000; Blue: $ffff; Alpha: alphaOpaque), // 5 - Light blue
    (Red: $ffff; Green: $0000; Blue: $ffff; Alpha: alphaOpaque), // 6 - Light Magenta
    (Red: $ffff; Green: $ffff; Blue: $ffff; Alpha: alphaOpaque), // 7 - White
    (Red: $4141; Green: $4141; Blue: $4141; Alpha: alphaOpaque), // 8 - Dark Gray
    (Red: $8080; Green: $8080; Blue: $8080; Alpha: alphaOpaque), // 9 - Gray
    (Red: $ffff; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 10 - Light red
    (Red: $ffff; Green: $AAAA; Blue: $AAAA; Alpha: alphaOpaque), // 11
    (Red: $BDBD; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 12
    (Red: $BDBD; Green: $7E7E; Blue: $7E7E; Alpha: alphaOpaque), // 13
    (Red: $8181; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 14
    (Red: $8181; Green: $5656; Blue: $5656; Alpha: alphaOpaque), // 15
    // expanded palette from http://sub-atomic.com/~moses/acadcolors.html
    (Red: $6868; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 16
    (Red: $6868; Green: $4545; Blue: $4545; Alpha: alphaOpaque), // 17
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 18
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 19
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 20
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 21
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 22
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 23
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 24
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 25
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 26
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 27
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 28
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 29
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 30
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 31
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 32
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 33
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 34
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 35
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 36
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 37
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 38
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 39
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 40
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 41
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 42
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 43
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 44
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 45
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 46
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 47
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 48
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 49
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 50
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 51
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 52
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 53
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 54
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 55
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 56
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 57
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 58
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 59
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 60
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 61
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 62
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 63
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 64
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 65
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 66
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 67
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 68
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 69
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 70
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 71
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 72
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 73
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 74
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 75
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 76
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 77
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 78
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 79
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 80
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 81
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 82
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 83
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 84
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 85
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 86
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 87
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 88
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 89
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 90
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 91
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 92
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 93
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 94
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 95
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 96
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 97
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 98
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 99
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 100
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 101
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 102
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 103
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 104
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 105
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 106
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 107
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 108
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 109
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 110
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 111
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 112
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 113
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 114
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 115
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 116
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 117
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 118
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 119
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 120
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 121
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 122
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 123
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 124
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 125
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 126
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 127
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 128
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 129
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 130
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 131
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 132
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 133
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 134
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 135
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 136
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 137
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 138
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 139
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 140
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 141
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 142
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 143
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 144
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 145
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 146
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 147
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 148
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 149
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 150
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 151
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 152
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 153
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 154
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 155
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 156
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 157
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 158
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 159
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 160
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 161
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 162
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 163
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 164
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 165
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 166
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 167
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 168
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 169
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 170
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 171
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 172
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 173
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 174
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 175
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 176
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 177
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 178
    (Red: $3535; Green: $3535; Blue: $4F4F; Alpha: alphaOpaque), // 179
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 180
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 181
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 182
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 183
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 184
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 185
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 186
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 187
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 188
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 189
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 190
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 191
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 192
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 193
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 194
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 195
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 196
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 197
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 198
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 199
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 200
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 201
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 202
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 203
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 204
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 205
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 206
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 207
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 208
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 209
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 210
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 211
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 212
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 213
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 214
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 215
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 216
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 217
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 218
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 219
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 220
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 221
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 222
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 223
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 224
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 225
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 226
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 227
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 228
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 229
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 230
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 231
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 232
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 233
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 234
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 235
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 236
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 237
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 238
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 239
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 240
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 241
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 242
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 243
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 244
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 245
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 246
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 247
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 248
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 249
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 250
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 251
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 252
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 253
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque), // 254
    (Red: $0000; Green: $0000; Blue: $0000; Alpha: alphaOpaque)  // 255
  );

{ TDXFToken }

constructor TDXFToken.Create;
begin
  inherited Create;

  Childs := TDXFTokens.Create;
end;

destructor TDXFToken.Destroy;
begin
  Childs.Free;

  inherited Destroy;
end;

{ TDXFTokenizer }

constructor TDXFTokenizer.Create;
begin
  inherited Create;

  Tokens := TDXFTokens.Create;
end;

destructor TDXFTokenizer.Destroy;
begin
  Tokens.Free;

  inherited Destroy;
end;

procedure TDXFTokenizer.ReadFromStrings(AStrings: TStrings);
var
  i: Integer;
  StrSectionGroupCode, StrSectionName: string;
  IntSectionGroupCode: Integer;
  CurTokenBase, NextTokenBase, SectionTokenBase, LastBlockToken: TDXFTokens;
  NewToken: TDXFToken;
  ParserState: Integer;
begin
  //  Tokens.ForEachCall(); deletecallback
  Tokens.Clear;

  CurTokenBase := Tokens;
  NextTokenBase := Tokens;
  i := 0;
  ParserState := 0;

  while i < AStrings.Count - 1 do
  begin
    CurTokenBase := NextTokenBase;

    // Now read and process the section name
    StrSectionGroupCode := AStrings.Strings[i];
    IntSectionGroupCode := StrToInt(Trim(StrSectionGroupCode));
    StrSectionName := AStrings.Strings[i+1];

    NewToken := TDXFToken.Create;
    NewToken.GroupCode := IntSectionGroupCode;
    NewToken.StrValue := StrSectionName;

    // Waiting for a section
    if ParserState = 0 then
    begin
      if (StrSectionName = 'SECTION') then
      begin
        ParserState := 1;
        NextTokenBase := NewToken.Childs;
      end
      else if (StrSectionName = 'EOF') then
      begin
        Exit;
      end
      // Comments can be in the beginning of the file and start with 999
      else if (IntSectionGroupCode = 999) then
      begin
        // nothing to be done, let it add the token
      end
      else
      begin
        raise Exception.Create(Format(
          'TDXFTokenizer.ReadFromStrings: Expected SECTION, but got: %s', [StrSectionname]));
      end;
    end
    // Processing the section name
    else if ParserState = 1 then
    begin
      if (StrSectionName = 'HEADER') or
        (StrSectionName = 'CLASSES') or
        (StrSectionName = 'OBJECTS') or
        (StrSectionName = 'THUMBNAILIMAGE') then
      begin
        ParserState := 2;
        SectionTokenBase := CurTokenBase;
      end
      else if (StrSectionName = 'BLOCKS') or (StrSectionName = 'TABLES') then
      begin
        ParserState := 4;
        SectionTokenBase := CurTokenBase;
      end
      else if (StrSectionName = 'ENTITIES') then
      begin
        ParserState := 3;
        SectionTokenBase := CurTokenBase;
      end
      else
      begin
        raise Exception.Create(Format(
          'TDXFTokenizer.ReadFromStrings: Invalid section name: %s', [StrSectionname]));
      end;
    end
    // Reading a generic section
    else if ParserState = 2 then
    begin
      if StrSectionName = 'ENDSEC' then
      begin
        ParserState := 0;
        CurTokenBase := SectionTokenBase;
        NextTokenBase := Tokens;
      end;
    end
    // Reading the ENTITIES section
    else if ParserState = 3 then
    begin
      if IsENTITIES_Subsection(StrSectionName) then
      begin
        CurTokenBase := SectionTokenBase;
        NextTokenBase := NewToken.Childs;
      end
      else if StrSectionName = 'ENDSEC' then
      begin
        ParserState := 0;
        CurTokenBase := SectionTokenBase;
        NextTokenBase := Tokens;
      end;
    end
    // Reading the TABLES or BLOCKS sections
    else if ParserState = 4 then
    begin
      // This orders the blocks themselves
      if IsTABLES_Subsection(StrSectionName) or IsBLOCKS_Subsection(StrSectionName) then
      begin
        CurTokenBase := SectionTokenBase;
        NextTokenBase := NewToken.Childs;
        LastBlockToken := NewToken.Childs;
      end
      // This orders the entities inside blocks
      else if IsENTITIES_Subsection(StrSectionName) and (LastBlockToken <> nil) then
      begin
        CurTokenBase := LastBlockToken;
        NextTokenBase := NewToken.Childs;
      end
      else if StrSectionName = 'ENDSEC' then
      begin
        ParserState := 0;
        CurTokenBase := SectionTokenBase;
        NextTokenBase := Tokens;
      end;
    end;

    CurTokenBase.Add(NewToken);

    Inc(i, 2);
  end;
end;

function TDXFTokenizer.IsTABLES_Subsection(AStr: string): Boolean;
begin
  Result :=
    (AStr = 'TABLE');
end;

function TDXFTokenizer.IsBLOCKS_Subsection(AStr: string): Boolean;
begin
  Result :=
    (AStr = 'BLOCK') or
    (AStr = 'ENDBLK');
end;

function TDXFTokenizer.IsENTITIES_Subsection(AStr: string): Boolean;
begin
  Result :=
    (AStr = '3DFACE') or
    (AStr = '3DSOLID') or
    (AStr = 'ACAD_PROXY_ENTITY') or
    (AStr = 'ARC') or
    (AStr = 'ATTDEF') or
    (AStr = 'ATTRIB') or
    (AStr = 'BODY') or
    (AStr = 'CIRCLE') or
    (AStr = 'DIMENSION') or
    (AStr = 'ELLIPSE') or
    (AStr = 'HATCH') or
    (AStr = 'IMAGE') or
    (AStr = 'INSERT') or
    (AStr = 'LEADER') or
    (AStr = 'LINE') or
    (AStr = 'LWPOLYLINE') or
    (AStr = 'MLINE') or
    (AStr = 'MTEXT') or
    (AStr = 'OLEFRAME') or
    (AStr = 'OLE2FRAME') or
    (AStr = 'POINT') or
    (AStr = 'POLYLINE') or
    (AStr = 'RAY') or
    (AStr = 'REGION') or
    (AStr = 'SEQEND') or
    (AStr = 'SHAPE') or
    (AStr = 'SOLID') or
    (AStr = 'SPLINE') or
    (AStr = 'TEXT') or
    (AStr = 'TOLERANCE') or
    (AStr = 'TRACE') or
    (AStr = 'VERTEX') or
    (AStr = 'VIEWPORT') or
    (AStr = 'XLINE');
end;

{ TvDXFVectorialReader }

procedure TvDXFVectorialReader.ReadHEADER(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  i, j: Integer;
  lStr: string;
  CurToken: TDXFToken;
  CurField: P3DPoint;
begin
  i := 0;
  while i < ATokens.Count do
  begin
    CurToken := TDXFToken(ATokens.Items[i]);
    if CurToken.StrValue = '$ANGBASE' then
    begin
      CurToken := TDXFToken(ATokens.Items[i+1]);
      ANGBASE := StrToFloat(CurToken.StrValue, FPointSeparator);
      Inc(i);
    end
    else if CurToken.StrValue = '$ANGDIR' then
    begin
      CurToken := TDXFToken(ATokens.Items[i+1]);
      ANGDIR := StrToInt(CurToken.StrValue);
      Inc(i);
    end
    // This indicates the size of the document
    else if (CurToken.StrValue = '$INSBASE') or
      (CurToken.StrValue = '$EXTMIN') or (CurToken.StrValue = '$EXTMAX') or
      (CurToken.StrValue = '$LIMMIN') or (CurToken.StrValue = '$LIMMAX') then
    begin
      if (CurToken.StrValue = '$INSBASE') then CurField := @INSBASE
      else if (CurToken.StrValue = '$EXTMIN') then CurField := @EXTMIN
      else if (CurToken.StrValue = '$EXTMAX') then CurField := @EXTMAX
      else if (CurToken.StrValue = '$LIMMIN') then CurField := @LIMMIN
      else if (CurToken.StrValue = '$LIMMAX') then CurField := @LIMMAX;

      // Check the next 2 items and verify if they are the values of the size of the document
      for j := 0 to 1 do
      begin
        CurToken := TDXFToken(ATokens.Items[i+1]);
        case CurToken.GroupCode of
        10:
        begin;
          CurField^.X := StrToFloat(CurToken.StrValue, FPointSeparator);
          Inc(i);
        end;
        20:
        begin
          CurField^.Y := StrToFloat(CurToken.StrValue, FPointSeparator);
          Inc(i);
        end;
        end;
      end;
    end
    else if CurToken.StrValue = '$DWGCODEPAGE' then
    begin
      // if we are forcing an encoding, don't use the value from the HEADER
      if ADoc.ForcedEncodingOnRead = '' then
      begin
        CurToken := TDXFToken(ATokens.Items[i+1]);
        lStr := CurToken.StrValue;
        if lStr = 'ANSI_1252' then ENCODING := 'CP1252';
      end;
      Inc(i);
    end;

    Inc(i);
  end;

  // After getting all the data, we can try to make some sense out of it

  // Sometimes EXTMIN comes as 10^20 and EXTMAX as -10^20, which makes no sence
  // In these cases we need to ignore them.
  if (EXTMIN.X > 10000000000) or (EXTMIN.X < -10000000000)
  or (EXTMAX.X > 10000000000) or (EXTMAX.X < -10000000000) then
  begin
    DOC_OFFSET.X := 0;
    DOC_OFFSET.Y := 0;

    AData.Width := LIMMAX.X;
    AData.Height := LIMMAX.Y;
  end
  else
  begin
    // The size of the document seams to be given by:
    // DOC_SIZE = min(EXTMAX, LIMMAX) - DOC_OFFSET;
    // if EXTMIN is <> -infinite then DOC_OFFSET = EXTMIN else DOC_OFFSET = (0, 0)
    // We will shift the whole document so that it has only positive coordinates and
    // DOC_OFFSET will be utilized for that

    if EXTMIN.X > -100 then
    begin
      DOC_OFFSET.X := EXTMIN.X;
      DOC_OFFSET.Y := EXTMIN.Y;
    end
    else FillChar(DOC_OFFSET, sizeof(T3DPoint), #0);

    AData.Width := min(EXTMAX.X, LIMMAX.X) - DOC_OFFSET.X;
    AData.Height := min(EXTMAX.Y, LIMMAX.Y) - DOC_OFFSET.Y;
  end;
end;

procedure TvDXFVectorialReader.ReadTABLES(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  i: Integer;
  CurToken: TDXFToken;
begin
  for i := 0 to ATokens.Count - 1 do
  begin
    CurToken := TDXFToken(ATokens.Items[i]);
    if CurToken.StrValue = 'TABLE' then ReadTABLES_TABLE(CurToken.Childs, AData, ADoc)
    else
    begin
      // ...
    end;
  end;
end;

procedure TvDXFVectorialReader.ReadTABLES_TABLE(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  CurToken: TDXFToken;
  i: Integer;
  // BLOCK data
  lName: string;
  PosX, PosY, PosZ: Double;
  lBlock: TvBlock = nil;
  lEntity: TvEntity = nil;
begin
  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      2: lName := CurToken.StrValue;
      10: PosX := CurToken.FloatValue;
      20: PosY := CurToken.FloatValue;
      30: PosZ := CurToken.FloatValue;
      0:
      begin
        if lBlock = nil then
          lBlock := AData.AddBlock(lName, PosX, PosY, PosZ);
        lEntity := InternalReadENTITIES(CurToken.StrValue, CurToken.Childs, AData, ADoc, True);

        if lEntity <> nil then
          lBlock.AddEntity(lEntity);
      end;
    end;
  end;
end;

procedure TvDXFVectorialReader.ReadBLOCKS(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  i: Integer;
  CurToken: TDXFToken;
begin
  for i := 0 to ATokens.Count - 1 do
  begin
    CurToken := TDXFToken(ATokens.Items[i]);
    if CurToken.StrValue = 'BLOCK' then ReadBLOCKS_BLOCK(CurToken.Childs, AData, ADoc)
    else if CurToken.StrValue = 'ENDBLK' then ReadBLOCKS_ENDBLK(CurToken.Childs, AData, ADoc)
    else
    begin
      // ...
    end;
  end;
end;

(*
The following group codes apply to block entities. For information about abbreviations and formatting used in this table, see "Formatting Conventions in This Reference."
Block group codes Group codes	Description
0 Entity type (BLOCK)
5 Handle
102 (optional) Start of application-defined group "{application_name". For example, "{ACAD_REACTORS" indicates the start of the AutoCAD persistent reactors group.
application-defined codes (optional) Codes and values within the 102 groups are application defined
102 (optional) End of group, "}"
330 Soft-pointer ID/handle to owner object
100 Subclass marker (AcDbEntity)
8 Layer name
100 Subclass marker (AcDbBlockBegin)
2 Block name
70 Block-type flags (bit coded values, may be combined):
1 = This is an anonymous block generated by hatching, associative dimensioning, other internal operations, or an application.
2 = This block has non-constant attribute definitions (this bit is not set if the block has any attribute definitions that are constant, or has no attribute definitions at all).
4 = This block is an external reference (xref).
8 = This block is an xref overlay.
16 = This block is externally dependent.
32 = This is a resolved external reference, or dependent of an external reference (ignored on input).
64 = This definition is a referenced external reference (ignored
on input).
10 Base point DXF: X value; APP: 3D point
20, 30 DXF: Y and Z values of base point
3 Block name
1 Xref path name
4 Block description (optional)
*)
procedure TvDXFVectorialReader.ReadBLOCKS_BLOCK(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  CurToken: TDXFToken;
  i: Integer;
  // BLOCK data
  lName: string;
  PosX, PosY, PosZ: Double;
  lBlock: TvBlock = nil;
  lEntity: TvEntity = nil;
begin
  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      2: lName := CurToken.StrValue;
      10: PosX := CurToken.FloatValue;
      20: PosY := CurToken.FloatValue;
      30: PosZ := CurToken.FloatValue;
      0:
      begin
        if lBlock = nil then
          lBlock := AData.AddBlock(lName, PosX, PosY, PosZ);
        lEntity := InternalReadENTITIES(CurToken.StrValue, CurToken.Childs, AData, ADoc, True);

        if lEntity <> nil then
          lBlock.AddEntity(lEntity);
      end;
    end;
  end;
end;

procedure TvDXFVectorialReader.ReadBLOCKS_ENDBLK(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
begin

end;

procedure TvDXFVectorialReader.ReadENTITIES(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  i: Integer;
  CurToken: TDXFToken;
  lEntity: TvEntity; // only to help debugging
begin
  IsReadingPolyline := False;

  for i := 0 to ATokens.Count - 1 do
  begin
    CurToken := TDXFToken(ATokens.Items[i]);
    lEntity := InternalReadENTITIES(CurToken.StrValue, CurToken.Childs, AData, ADoc);
  end;
end;

function TvDXFVectorialReader.ReadENTITIES_3DFACE(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean
  ): TvCircularArc;
var
  CurToken: TDXFToken;
  lPolygon: TvPolygon;
  i: Integer;
begin
  Result := nil;
  lPolygon := TvPolygon.Create(nil);
  SetLength(lPolygon.Points, 3);

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 11, 21, 31, 12, 22, 32, 13, 23, 33, 70] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      10: lPolygon.Points[0].X := CurToken.FloatValue;
      20: lPolygon.Points[0].Y := CurToken.FloatValue;
      30: lPolygon.Points[0].Z := CurToken.FloatValue;
      11: lPolygon.Points[1].X := CurToken.FloatValue;
      21: lPolygon.Points[1].Y := CurToken.FloatValue;
      31: lPolygon.Points[1].Z := CurToken.FloatValue;
      12: lPolygon.Points[2].X := CurToken.FloatValue;
      22: lPolygon.Points[2].Y := CurToken.FloatValue;
      32: lPolygon.Points[2].Z := CurToken.FloatValue;
      13:
      begin
        SetLength(lPolygon.Points, 4);
        lPolygon.Points[3].X := CurToken.FloatValue;
      end;
      23:
      begin
        SetLength(lPolygon.Points, 4);
        lPolygon.Points[3].Y := CurToken.FloatValue;
      end;
      33:
      begin
        SetLength(lPolygon.Points, 4);
        lPolygon.Points[3].Z := CurToken.FloatValue;
      end;
      {
        Invisible edge flags (optional; default = 0):
        1 = First edge is invisible
        2 = Second edge is invisible
        4 = Third edge is invisible
        8 = Fourth edge is invisible
      }
      70:
      begin
      end;
    end;
  end;

  DXFCoordsToFPCoords(lPolygon.Points[0].X, lPolygon.Points[0].Y, lPolygon.Points[0].Z,
    lPolygon.Points[0].X, lPolygon.Points[0].Y, lPolygon.Points[0].Z);
  DXFCoordsToFPCoords(lPolygon.Points[1].X, lPolygon.Points[1].Y, lPolygon.Points[1].Z,
    lPolygon.Points[1].X, lPolygon.Points[1].Y, lPolygon.Points[1].Z);
  DXFCoordsToFPCoords(lPolygon.Points[2].X, lPolygon.Points[2].Y, lPolygon.Points[2].Z,
    lPolygon.Points[2].X, lPolygon.Points[2].Y, lPolygon.Points[2].Z);
  if Length(lPolygon.Points) >= 4 then
    DXFCoordsToFPCoords(lPolygon.Points[3].X, lPolygon.Points[3].Y, lPolygon.Points[3].Z,
      lPolygon.Points[3].X, lPolygon.Points[3].Y, lPolygon.Points[3].Z);

  AData.AddEntity(lPolygon);
end;

function TvDXFVectorialReader.ReadENTITIES_LINE(ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TPath;
var
  CurToken: TDXFToken;
  i: Integer;
  // LINE
  LineStartX, LineStartY, LineStartZ: Double;
  LineEndX, LineEndY, LineEndZ: Double;
  LLineColor: TFPColor;
begin
  Result := nil;

  // Initial values
  LineStartX := 0;
  LineStartY := 0;
  LineStartZ := 0;
  LineEndX := 0;
  LineEndY := 0;
  LineEndZ := 0;
  LLineColor := colBlack;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 11, 21, 31, 62] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      10: LineStartX := CurToken.FloatValue;
      20: LineStartY := CurToken.FloatValue;
      30: LineStartZ := CurToken.FloatValue;
      11: LineEndX := CurToken.FloatValue;
      21: LineEndY := CurToken.FloatValue;
      31: LineEndZ := CurToken.FloatValue;
      62: LLineColor := DXFColorIndexToFPColor(Trunc(CurToken.FloatValue));
    end;
  end;

  // Position fixing for documents with negative coordinates
  LineStartX := LineStartX - DOC_OFFSET.X;
  LineStartY := LineStartY - DOC_OFFSET.Y;
  LineEndX := LineEndX - DOC_OFFSET.X;
  LineEndY := LineEndY - DOC_OFFSET.Y;

  // And now write it
  {$ifdef FPVECTORIALDEBUG_LINE}
  WriteLn(Format('Adding Line from %f^%f to %f^%f RGB=%d %d %d A=%d', [LineStartX, LineStartY, LineEndX, LineEndY,
    LLineColor.red div $100, LLineColor.Green div $100, LLineColor.Blue div $100, LLineColor.Alpha div $100]));
  {$endif}
  if AOnlyCreate then
  begin
    Result := TPath.Create(AData);
    Result.AppendMoveToSegment(LineStartX, LineStartY);
    Result.AppendLineToSegment(LineEndX, LineEndY);
  end
  else
  begin
    AData.StartPath(LineStartX, LineStartY);
    AData.AddLineToPath(LineEndX, LineEndY, LLineColor);
    AData.EndPath();
  end;
end;

{
Arcs are always counter-clockwise in DXF

100 Subclass marker (AcDbCircle)
39 Thickness (optional; default = 0)
10 Center point (in OCS) DXF: X value; APP: 3D point
20, 30 DXF: Y and Z values of center point (in OCS)
40 Radius
100 Subclass marker (AcDbArc)
50 Start angle (degrees)
51 End angle (degrees)
210 Extrusion direction. (optional; default = 0, 0, 1) DXF: X value; APP: 3D vector
220, 230 DXF: Y and Z values of extrusion direction (optional)
}
function TvDXFVectorialReader.ReadENTITIES_ARC(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvCircularArc;
var
  CurToken: TDXFToken;
  i: Integer;
  CenterX, CenterY, CenterZ, Radius, StartAngle, EndAngle: Double;
  LColor: TFPColor;
begin
  CenterX := 0.0;
  CenterY := 0.0;
  CenterZ := 0.0;
  Radius := 0.0;
  StartAngle := 0.0;
  EndAngle := 0.0;
  LColor := colBlack;
  Result := nil;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 40, 50, 51, 62] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      10: CenterX := CurToken.FloatValue;
      20: CenterY := CurToken.FloatValue;
      30: CenterZ := CurToken.FloatValue;
      40: Radius := CurToken.FloatValue;
      50: StartAngle := CurToken.FloatValue;
      51: EndAngle := CurToken.FloatValue;
      62: LColor := DXFColorIndexToFPColor(Trunc(CurToken.FloatValue));
    end;
  end;

  // In DXF the EndAngle is always greater then the StartAngle.
  // If it isn't then sum 360 to it to make sure we don't get wrong results
  if EndAngle < StartAngle then EndAngle := EndAngle + 360;

  // Position fixing for documents with negative coordinates
  CenterX := CenterX - DOC_OFFSET.X;
  CenterY := CenterY - DOC_OFFSET.Y;

  {$ifdef FPVECTORIALDEBUG}
  WriteLn(Format('Adding Arc Center=%f,%f Radius=%f StartAngle=%f EndAngle=%f AOnlyCreate=%d',
    [CenterX, CenterY, Radius, StartAngle, EndAngle, Integer(AOnlyCreate)]));
  {$endif}
  Result := AData.AddCircularArc(CenterX, CenterY, Radius, StartAngle, EndAngle, LColor, AOnlyCreate);
end;

{
Group codes	Description
100 Subclass marker (AcDbCircle)
39 Thickness (optional; default = 0)
10 Center point (in OCS) DXF: X value; APP: 3D point
20, 30 DXF: Y and Z values of center point (in OCS)
40 Radius
210 Extrusion direction (optional; default = 0, 0, 1) DXF: X value; APP: 3D vector
220, 230 DXF: Y and Z values of extrusion direction  (optional)
}
function TvDXFVectorialReader.ReadENTITIES_CIRCLE(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvCircle;
var
  CurToken: TDXFToken;
  i: Integer;
  CircleCenterX, CircleCenterY, CircleCenterZ, CircleRadius: Double;
begin
  CircleCenterX := 0.0;
  CircleCenterY := 0.0;
  CircleCenterZ := 0.0;
  CircleRadius := 0.0;
  Result := nil;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 40] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      10: CircleCenterX := CurToken.FloatValue;
      20: CircleCenterY := CurToken.FloatValue;
      30: CircleCenterZ := CurToken.FloatValue;
      40: CircleRadius := CurToken.FloatValue;
    end;
  end;

  // Position fixing for documents with negative coordinates
  CircleCenterX := CircleCenterX - DOC_OFFSET.X;
  CircleCenterY := CircleCenterY - DOC_OFFSET.Y;

  Result := AData.AddCircle(CircleCenterX, CircleCenterY, CircleRadius, AOnlyCreate);
end;

{
Group codes Description
100 Subclass marker (AcDbDimension)
2 Name of the block that contains the entities that make up the dimension picture
10 Definition point (in WCS) DXF: X value; APP: 3D point
20, 30 DXF: Y and Z values of definition point (in WCS)
11 Middle point of dimension text (in OCS) DXF: X value; APP: 3D point
21, 31 DXF: Y and Z values of middle point of dimension text (in OCS)
70 Dimension type.
  Values 0-6 are integer values that represent the dimension type.
  Values 32, 64, and 128 are bit values, which are added to the integer values
  (value 32 is always set in R13 and later releases).
  0 = Rotated, horizontal, or vertical; 1 = Aligned;
  2 = Angular; 3 = Diameter; 4 = Radius;
  5 = Angular 3 point; 6 = Ordinate;
  32 = Indicates that the block reference (group code 2) is referenced by this dimension only.
  64 = Ordinate type. This is a bit value (bit 7) used only with integer value 6.
    If set, ordinate is X-type; if not set, ordinate is Y-type.
  128 = This is a bit value (bit 8) added to the other group 70 values
    if the dimension text has been positioned at a user-defined location
    rather than at the default location.
71 Attachment point:
  1 = Top left; 2 = Top center; 3 = Top right;
  4 = Middle left; 5 = Middle center; 6 = Middle right;
  7 = Bottom left; 8 = Bottom center; 9 = Bottom right
72 Dimension text line spacing style (optional):
  1(or missing) = At least (taller characters will override)
  2 = Exact (taller characters will not override)
41 Dimension text line spacing factor (optional):
  Percentage of default (3-on-5) line spacing to be applied. Valid values range from 0.25 to 4.00.
42 Actual measurement (optional; read-only value)
1 Dimension text explicitly entered by the user. Optional; default is the measurement.
  If null or "<>", the dimension measurement is drawn as the text,
  if " " (one blank space), the text is suppressed. Anything else is drawn as the text.
53 The optional group code 53 is the rotation angle of the dimension
  text away from its default orientation (the direction of the dimension line)  (optional).
51 All dimension types have an optional 51 group code, which indicates the
  horizontal direction for the dimension entity. The dimension entity determines
  the orientation of dimension text and lines for horizontal, vertical, and
  rotated linear dimensions.
  This group value is the negative of the angle between the OCS X axis
  and the UCS X axis. It is always in the XY plane of the OCS.
210 Extrusion direction (optional; default = 0, 0, 1) DXF: X value; APP: 3D vector
220, 230 DXF: Y and Z values of extrusion direction  (optional)
3 Dimension style name

*Aligned Dimension Group Codes

100 Subclass marker (AcDbAlignedDimension)
12 Insertion point for clones of a dimension-Baseline and Continue (in OCS) DXF: X value; APP: 3D point
22, 32 DXF: Y and Z values of insertion point for clones of a dimension-Baseline and Continue (in OCS)
13 Definition point for linear and angular dimensions (in WCS) DXF: X value; APP: 3D point
23, 33 DXF: Y and Z values of definition point for linear and angular dimensions (in WCS)
14 Definition point for linear and angular dimensions (in WCS) DXF: X value; APP: 3D point
24, 34 DXF: Y and Z values of definition point for linear and angular dimensions (in WCS)

  |--text--|->10,20
  |        |
  |        |
  X->14,24 X->13,23

*Radial and Diameter Dimension Group Codes
http://www.autodesk.com/techpubs/autocad/acadr14/dxf/index.htm

100 Subclass marker (AcDbRadialDimension or AcDbDiametricDimension)
15  Definition point for diameter, radius, and angular dimensions (in WCS). DXF: X value; APP: 3D point
25, 35  DXF: Y and Z values of definition point for diameter, radius, and angular dimensions (in WCS).
40 Leader length for radius and diameter dimensions.

The point (15,25,35) specifies the first point of the dimension line on the circle/arc
and the point (10,20,30) specifies the point opposite the first point.
The point (11,21,31) specifies the midpoint of the dimension text.
}
function TvDXFVectorialReader.ReadENTITIES_DIMENSION(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvEntity;
type
  TDXFDimensionType = (ddtUnknown, ddtAligned, ddtRadial, ddtDiametric, ddt2LineAngular);
var
  CurToken: TDXFToken;
  i: Integer;
  // DIMENSION
  BaseLeft, BaseRight, DimensionRight, DimensionLeft, lCenter, lTextPos, TmpPoint: T3DPoint;
  TmpDimensionRight, TmpDimensionLeft: T3DPoint;
  Dim0, Dim1, Dim3, Dim4, Dim5, Dim6: T3DPoint;
  lAngleLeft, lAngleRight, al, bl, lRadius, lBaskaraDelta, ae, be, ce: Double;
  DXFDimensionType: TDXFDimensionType = ddtUnknown;
begin
  // Initial values
  BaseLeft.X := 0;
  BaseLeft.Y := 0;
  BaseRight.X := 0;
  BaseRight.X := 0;
  DimensionRight.X := 0;
  DimensionRight.Y := 0;
  DimensionLeft.X := 0;
  DimensionLeft.Y := 0;
  Result := nil;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 11, 21, 31, 13, 23, 33, 14, 24, 34, 15, 25, 35, 16, 26, 36] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      10: Dim0.X := CurToken.FloatValue;
      20: Dim0.Y := CurToken.FloatValue;
      30: Dim0.Z := CurToken.FloatValue;
      11: Dim1.X := CurToken.FloatValue;
      21: Dim1.Y := CurToken.FloatValue;
      31: Dim1.Z := CurToken.FloatValue;
      13: Dim3.X := CurToken.FloatValue;
      23: Dim3.Y := CurToken.FloatValue;
      33: Dim3.Z := CurToken.FloatValue;
      14: Dim4.X := CurToken.FloatValue;
      24: Dim4.Y := CurToken.FloatValue;
      34: Dim4.Z := CurToken.FloatValue;
      15: Dim5.X := CurToken.FloatValue;
      25: Dim5.Y := CurToken.FloatValue;
      35: Dim5.Z := CurToken.FloatValue;
      16: Dim6.X := CurToken.FloatValue;
      26: Dim6.Y := CurToken.FloatValue;
      36: Dim6.Z := CurToken.FloatValue;
      100:
      begin
        if CurToken.StrValue = 'AcDbAlignedDimension' then DXFDimensionType := ddtAligned
        else if CurToken.StrValue = 'AcDbRadialDimension' then DXFDimensionType := ddtRadial
        else if CurToken.StrValue = 'AcDbDiametricDimension' then DXFDimensionType := ddtDiametric
        else if CurToken.StrValue = 'AcDb2LineAngularDimension' then DXFDimensionType := ddt2LineAngular;
      end;
    end;
  end;

  // Position fixing for documents with negative coordinates
  Dim0.X := Dim0.X - DOC_OFFSET.X;
  Dim0.Y := Dim0.Y - DOC_OFFSET.Y;
  Dim1.X := Dim1.X - DOC_OFFSET.X;
  Dim1.Y := Dim1.Y - DOC_OFFSET.Y;
  Dim3.X := Dim3.X - DOC_OFFSET.X;
  Dim3.Y := Dim3.Y - DOC_OFFSET.Y;
  Dim4.X := Dim4.X - DOC_OFFSET.X;
  Dim4.Y := Dim4.Y - DOC_OFFSET.Y;
  Dim5.X := Dim5.X - DOC_OFFSET.X;
  Dim5.Y := Dim5.Y - DOC_OFFSET.Y;
  Dim6.X := Dim6.X - DOC_OFFSET.X;
  Dim6.Y := Dim6.Y - DOC_OFFSET.Y;

  // Standard value meaning
  DimensionRight := Dim0;
  BaseRight := Dim3;
  BaseLeft := Dim4;

  // And now write it
  {$ifdef FPVECTORIALDEBUG}
//  WriteLn(Format('Adding Line from %f,%f to %f,%f', [LineStartX, LineStartY, LineEndX, LineEndY]));
  {$endif}
  // -----------------------------------------------
  // Aligned dimension
  // -----------------------------------------------
  if DXFDimensionType = ddtAligned then
  begin
    // Now make sure that we actually that BaseLeft is to the left of BaseRight
    if BaseRight.X < BaseLeft.X then
    begin
      TmpPoint := BaseRight;
      BaseRight := BaseLeft;
      BaseLeft := TmpPoint;
    end;

    // Now check if we are a horizontal or vertical dimension

    // horizontal
    //
    //DL____ DR
    //  |  |
    //  |  |
    // BL  BR
    if DimensionRight.X = BaseRight.X then
    begin
      DimensionLeft.X := BaseLeft.X;
      DimensionLeft.Y := DimensionRight.Y;
    end
    // Inverted horizontal where DL was provided
    else if DimensionRight.X = BaseLeft.X then
    begin
      DimensionLeft.X := DimensionRight.X;
      DimensionLeft.Y := DimensionRight.Y;
      DimensionRight.X := BaseRight.X;
    end
    // vertical
    //
    // BL ----|DR
    //  BR  --|DL
    //
    // In this case we invert then DR and DL
    else if DimensionRight.Y = BaseLeft.Y then
    begin
      DimensionLeft := DimensionRight;
      DimensionRight.Y := BaseRight.Y;
    end
    // vertical
    //
    // BL ----|DL
    //  BR  --|DR
    //
    else if DimensionRight.Y = BaseRight.Y then
    begin
      DimensionLeft.X := DimensionRight.X;
      DimensionLeft.Y := BaseLeft.Y;
    end;

    Result := AData.AddAlignedDimension(BaseLeft, BaseRight, DimensionLeft, DimensionRight, AOnlyCreate);
  end
  // -----------------------------------------------
  // Radius and Diameters are very similar
  // -----------------------------------------------
  else if DXFDimensionType in [ddtRadial, ddtDiametric] then
  begin
    if DXFDimensionType = ddtRadial then
    begin
      lCenter := DimensionRight;
      DimensionLeft := Dim5;
    end
    else
    begin
      lCenter := Dim1;
      DimensionLeft := Dim5;
      DimensionRight.X := DimensionRight.X;
      DimensionRight.Y := DimensionRight.Y;
    end;

    Result := AData.AddRadialDimension(DXFDimensionType = ddtDiametric, lCenter, DimensionLeft, DimensionRight, AOnlyCreate);
  end
  // -----------------------------------------------
  // A arc dimension
  // -----------------------------------------------
  else if DXFDimensionType = ddt2LineAngular then
  begin
    lCenter := Dim6;
    lTextPos := Dim1;

    // Sometimes Dim0 and Dim3 are the base, but sometimes Dim4 and Dim5 are the base!
    // So we need to check which points are nearer o.O
    // I plotted the given points to come to this conclusion
    // and it doesn't match the provided example image:
    // http://docs.autodesk.com/ACD/2013/RUS/images/GUID-E2F42FD3-2684-4F50-88A9-3AF3A5824FF1-low.png
    // but it works. The pairs of points in the same line are 3,4 and 0,5
    if Abs(Dim0.X-Dim3.X)+Abs(Dim0.Y-Dim3.Y) < Abs(Dim5.X-Dim4.X)+Abs(Dim5.Y-Dim4.Y) then
    begin
      BaseLeft := Dim0;
      BaseRight := Dim3;
      TmpDimensionLeft := Dim5;
      TmpDimensionRight := Dim4;
    end
    else
    begin
      BaseLeft := Dim4;
      BaseRight := Dim5;
      TmpDimensionLeft := Dim3;
      TmpDimensionRight := Dim0;
    end;

    // Calculate where the arc hits the left and right lines to obtain DimensionLeft and DimensionRight
    // Left line is: Y = al.X + bl
    // Circle equation for the arc is (X - BaseLeft.X)^2 + (Y - BaseLeft.Y)^2 = R^2
    // This goes to a second degree equation of ae * X^2 + be * X + ce = 0
    lAngleLeft := arctan(Abs(BaseLeft.Y-TmpDimensionLeft.Y)/Abs(BaseLeft.X-TmpDimensionLeft.X));
    if TmpDimensionLeft.X<BaseLeft.X then lAngleLeft := Pi-lAngleLeft;
    lRadius := sqrt(sqr(lCenter.Y-BaseLeft.Y) + sqr(lCenter.X-BaseLeft.X));
    al := Tan(lAngleLeft);
    bl := BaseLeft.Y - al * BaseLeft.X;
    ae := 1 + al*al;
    be := -2 * BaseLeft.X + 2 * al * bl - 2 * al * BaseLeft.Y;
    ce := sqr(BaseLeft.X)+bl*bl-2*bl*BaseLeft.Y+sqr(BaseLeft.Y)-lRadius*lRadius;
    lBaskaraDelta := be*be-4*ae*ce;
    // The equation has 2 solutions, get the one nearest to TmpDimension
    DimensionLeft.X := (-be+sqrt(lBaskaraDelta)) / (2 * ae);
    DimensionLeft.Y := (-be-sqrt(lBaskaraDelta)) / (2 * ae);
    if Abs(DimensionLeft.X-TmpDimensionLeft.X)>Abs(DimensionLeft.Y-TmpDimensionLeft.X)then
      DimensionLeft.X := DimensionLeft.Y;
    DimensionLeft.Y := al * DimensionLeft.X + bl;

    lAngleRight := arctan(Abs(BaseRight.Y-TmpDimensionRight.Y)/Abs(BaseRight.X-TmpDimensionRight.X));
    if TmpDimensionRight.X<BaseRight.X then lAngleRight := Pi-lAngleRight;
    al := Tan(lAngleRight);
    bl := BaseRight.Y - al * BaseRight.X;
    ae := 1 + al*al;
    be := -2 * BaseRight.X + 2 * al * bl - 2 * al * BaseRight.Y;
    ce := sqr(BaseRight.X)+bl*bl-2*bl*BaseRight.Y+sqr(BaseRight.Y)-lRadius*lRadius;
    lBaskaraDelta := be*be-4*ae*ce;
    // The equation has 2 solutions, get the one nearest to TmpDimension
    DimensionRight.X := (-be+sqrt(lBaskaraDelta)) / (2 * ae);
    DimensionRight.Y := (-be-sqrt(lBaskaraDelta)) / (2 * ae);
    if Abs(DimensionRight.X-TmpDimensionRight.X)>Abs(DimensionRight.Y-TmpDimensionRight.X)then
      DimensionRight.X := DimensionRight.Y;
    DimensionRight.Y := al * DimensionRight.X + bl;

    Result := AData.AddArcDimension(180*Abs(lAngleRight-lAngleLeft)/Pi,
      lRadius, BaseLeft, BaseRight, DimensionLeft, DimensionRight, lTextPos, AOnlyCreate);
  end;
end;

{
100 Subclass marker (AcDbEllipse)
10 Center point (in WCS) DXF: X value; APP: 3D point
20, 30 DXF: Y and Z values of center point (in WCS)
11 Endpoint of major axis, relative to the center (in WCS) DXF: X value; APP: 3D point
21, 31 DXF: Y and Z values of endpoint of major axis, relative to the center (in WCS)
210 Extrusion direction (optional; default = 0, 0, 1) DXF: X value; APP: 3D vector
220, 230 DXF: Y and Z values of extrusion direction  (optional)
40 Ratio of minor axis to major axis
41 Start parameter (this value is 0.0 for a full ellipse)
42 End parameter (this value is 2pi for a full ellipse)

Example:
0
ELLIPSE
10
284.1193488089
20
246.9070153869
30
0.0
11
-0.0880072471
21
1.1224779542
31
0.0
40
0.6532930174
41
-0.0511763442
42
1.5196199825
}
function TvDXFVectorialReader.ReadENTITIES_ELLIPSE(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvEllipse;
var
  CurToken: TDXFToken;
  i: Integer;
  CenterX, CenterY, CenterZ, MajorAxisEndX, MajorAxisEndY, MajorAxisEndZ, MinorAxisRatio: Double;
  MajorHalfAxis, MinorHalfAxis, Angle: Double;
begin
  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      10: CenterX := CurToken.FloatValue;
      20: CenterY := CurToken.FloatValue;
      30: CenterZ := CurToken.FloatValue;
      11: MajorAxisEndX := CurToken.FloatValue;
      21: MajorAxisEndY := CurToken.FloatValue;
      31: MajorAxisEndZ := CurToken.FloatValue;
      40: MinorAxisRatio := CurToken.FloatValue;
      41: MinorAxisRatio := CurToken.FloatValue;
      42: MinorAxisRatio := CurToken.FloatValue;
    end;
  end;

  // Position fixing for documents with negative coordinates
  CenterX := CenterX - DOC_OFFSET.X;
  CenterY := CenterY - DOC_OFFSET.Y;

  // Calculate the axis info
  MajorHalfAxis := Sqrt(Sqr(MajorAxisEndX - CenterX)+Sqr(MajorAxisEndY - CenterY));
  MinorHalfAxis := MajorHalfAxis * MinorAxisRatio;
  Angle := 0.0;

  //
  Result := AData.AddEllipse(CenterX, CenterY, MajorHalfAxis, MinorHalfAxis, Angle, AOnlyCreate);
end;

{
The following group codes apply to insert (block reference) entities. In addition to the group codes described here, see "Common Group Codes for Entities." For information about abbreviations and formatting used in this table, see "Formatting Conventions in This Reference."
Insert group codes Group codes	Description

100 Subclass marker (AcDbBlockReference)
66 Variable attributes-follow flag (optional; default = 0); if the value of attributes-follow flag is 1, a series of attribute entities is expected to follow the insert, terminated by a seqend entity
2 Block name
10 Insertion point (in OCS)
  DXF: X value; APP: 3D point
20, 30 DXF: Y and Z values of insertion point (in OCS)
41 X scale factor (optional; default = 1)
42 Y scale factor (optional; default = 1)
43 Z scale factor (optional; default = 1)
50 Rotation angle (optional; default = 0)
70 Column count (optional; default = 1)
71 Row count (optional; default = 1)
44 Column spacing (optional; default = 0)
45 Row spacing (optional; default = 0)
210 Extrusion direction (optional; default = 0, 0, 1)
  DXF: X value; APP: 3D vector
220, 230 DXF: Y and Z values of extrusion direction (optional)
}
function TvDXFVectorialReader.ReadENTITIES_INSERT(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvInsert;
var
  CurToken: TDXFToken;
  i: Integer;
  lName: string;
  lBlock: TvBlock;
  PosX, PosY, PosZ: Double;
  lRotationAngle: Double = 0.0;
begin
  PosX := 0.0;
  PosY := 0.0;
  PosZ := 0.0;
  Result := nil;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 50] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      2: lName := CurToken.StrValue;
      10: PosX := CurToken.FloatValue;
      20: PosY := CurToken.FloatValue;
      30: PosZ := CurToken.FloatValue;
      50: lRotationAngle := -1 * CurToken.FloatValue * Pi / 180;
    end;
  end;

  // Find the block by its name
  lBlock := TvBlock(AData.FindEntityWithNameAndType(lName, TvBlock));
  if lBlock = nil then Exit;

  // write the data
  Result := TvInsert.Create(AData);
  Result.X := PosX;
  Result.Y := PosY;
  Result.Z := PosZ;
  Result.InsertEntity := lBlock;
  Result.RotationAngle := lRotationAngle;
  if not AOnlyCreate then AData.AddEntity(Result);
end;

{
100 Subclass marker (AcDbText)
39 Thickness (optional; default = 0)
10 First alignment point (in OCS) DXF: X value; APP: 3D point
20, 30 DXF: Y and Z values of first alignment point (in OCS)
40 Text height
1 Default value (the string itself)
50 Text rotation (optional; default = 0)
41 Relative X scale factor-width (optional; default = 1)
  This value is also adjusted when fit-type text is used.
51 Oblique angle (optional; default = 0)
7 Text style name (optional, default = STANDARD)
71 Text generation flags (optional, default = 0):
  2 = Text is backward (mirrored in X).
  4 = Text is upside down (mirrored in Y).
72 Horizontal text justification type (optional, default = 0) integer codes (not bit-coded)
  0 = Left; 1= Center; 2 = Right
  3 = Aligned (if vertical alignment = 0)
  4 = Middle (if vertical alignment = 0)
  5 = Fit (if vertical alignment = 0)
  See the Group 72 and 73 integer codes table for clarification.
11 Second alignment point (in OCS) (optional)
  DXF: X value; APP: 3D point
  This value is meaningful only if the value of a 72 or 73 group is nonzero (if the justification is anything other than baseline/left).
21, 31 DXF: Y and Z values of second alignment point (in OCS) (optional)
210 Extrusion direction (optional; default = 0, 0, 1)
  DXF: X value; APP: 3D vector
220, 230 DXF: Y and Z values of extrusion direction (optional)
73 Vertical text justification type (optional, default = 0): integer codes (not bit- coded):
  0 = Baseline; 1 = Bottom; 2 = Middle; 3 = Top
  See the Group 72 and 73 integer codes table for clarification.
}
function TvDXFVectorialReader.ReadENTITIES_TEXT(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvText;
var
  CurToken: TDXFToken;
  i: Integer;
  PosX: Double = 0.0;
  PosY: Double = 0.0;
  PosZ: Double = 0.0;
  FontSize: Double = 10.0;
  Str: string = '';
begin
  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 40] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      1:  Str := CurToken.StrValue;
      10: PosX := CurToken.FloatValue;
      20: PosY := CurToken.FloatValue;
      30: PosZ := CurToken.FloatValue;
      40: FontSize := CurToken.FloatValue;
    end;
  end;

  // Position fixing for documents with negative coordinates
  PosX := PosX - DOC_OFFSET.X;
  PosY := PosY - DOC_OFFSET.Y;

  // Convert the string if necessary
  Str := ConvertDXFStringToUTF8(Str);

  //
  Result := TvText.Create(AData);
  Result.Value.Text := Str;
  Result.X := PosX;
  Result.Y := PosY;
  Result.Font.Size := Round(FontSize);
  Result.Font.Color := colWhite;
  if not AOnlyCreate then AData.AddEntity(Result);
end;

{.$define FPVECTORIALDEBUG_LWPOLYLINE}
{
100 Subclass marker (AcDbPolyline)
90  Number of vertices
70  Polyline flag (bit-coded); default is 0:
    1 = Closed; 128 = Plinegen
43  Constant width (optional; default = 0). Not used if variable width (codes 40 and/or 41) is set
38  Elevation (optional; default = 0)
39  Thickness (optional; default = 0)
10  Vertex coordinates (in OCS), multiple entries; one entry for each vertex
    DXF: X value; APP: 2D point
20  DXF: Y value of vertex coordinates (in OCS), multiple entries; one entry for each vertex
40  Starting width (multiple entries; one entry for each vertex) (optional; default = 0; multiple entries). Not used if constant width (code 43) is set
41  End width (multiple entries; one entry for each vertex) (optional; default = 0; multiple entries). Not used if constant width (code 43) is set
42  Bulge (multiple entries; one entry for each vertex) (optional; default = 0)
210 Extrusion direction (optional; default = 0, 0, 1)
    DXF: X value; APP: 3D vector
220, 230 DXF: Y and Z values of extrusion direction (optional)
}
function TvDXFVectorialReader.ReadENTITIES_LWPOLYLINE(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TPath;
var
  CurToken: TDXFToken;
  i, curPoint: Integer;
  // LINE
  LWPolyline: array of TLWPOLYLINEElement;
  LWFlags: Integer = 0;
begin
  curPoint := -1;
  Result := nil;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 11, 21, 31, 70] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    // Loads the coordinates
    // With Position fixing for documents with negative coordinates
    case CurToken.GroupCode of
      10:
      begin
        // Starting a new point
        Inc(curPoint);
        SetLength(LWPolyline, curPoint+1);

        LWPolyline[curPoint].X := CurToken.FloatValue - DOC_OFFSET.X;
      end;
      20: LWPolyline[curPoint].Y := CurToken.FloatValue - DOC_OFFSET.Y;
      70: LWFlags := Round(CurToken.FloatValue);
    end;
  end;

  // In case of a Flag="Closed" then we need to close the line
  if LWFlags = 1 then
  begin
    Inc(curPoint);
    SetLength(LWPolyline, curPoint+1);
    LWPolyline[curPoint].X := LWPolyline[0].X;
    LWPolyline[curPoint].Y := LWPolyline[0].Y;
  end;

  // And now write it
  if curPoint >= 0 then // otherwise the polyline is empty of points
  begin
    AData.StartPath(LWPolyline[0].X, LWPolyline[0].Y);
    {$ifdef FPVECTORIALDEBUG_LWPOLYLINE}
    Write(Format('LWPOLYLINE ID=%d %f,%f', [AData.PathCount-1, LWPolyline[0].X, LWPolyline[0].Y]));
    {$endif}
    for i := 1 to curPoint do
    begin
      AData.AddLineToPath(LWPolyline[i].X, LWPolyline[i].Y);
      {$ifdef FPVECTORIALDEBUG_LWPOLYLINE}
       Write(Format(' %f,%f', [LWPolyline[i].X, LWPolyline[i].Y]));
      {$endif}
    end;
    {$ifdef FPVECTORIALDEBUG_LWPOLYLINE}
     WriteLn('');
    {$endif}
    Result := AData.EndPath(AOnlyCreate);
  end;
end;

{.$define FPVECTORIALDEBUG_SPLINE}
function TvDXFVectorialReader.ReadENTITIES_SPLINE(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TPath;
var
  CurToken: TDXFToken;
  i, curPoint: Integer;
  // LINE
  SPLine: array of TSPLineElement;
begin
  curPoint := -1;
  Result := nil;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 11, 21, 31] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    // Loads the coordinates
    // With Position fixing for documents with negative coordinates
    case CurToken.GroupCode of
      10:
      begin
        // Starting a new point
        Inc(curPoint);
        SetLength(SPLine, curPoint+1);

        SPLine[curPoint].X := CurToken.FloatValue - DOC_OFFSET.X;
      end;
      20: SPLine[curPoint].Y := CurToken.FloatValue - DOC_OFFSET.Y;
    end;
  end;

  // And now write it
  if curPoint >= 0 then // otherwise the polyline is empty of points
  begin
    AData.StartPath(SPLine[0].X, SPLine[0].Y);
    {$ifdef FPVECTORIALDEBUG_SPLINE}
    Write(Format('SPLINE ID=%d %f,%f', [AData.PathCount-1, SPLine[0].X, SPLine[0].Y]));
    {$endif}
    for i := 1 to curPoint do
    begin
      AData.AddLineToPath(SPLine[i].X, SPLine[i].Y);
      {$ifdef FPVECTORIALDEBUG_SPLINE}
       Write(Format(' %f,%f', [SPLine[i].X, SPLine[i].Y]));
      {$endif}
    end;
    {$ifdef FPVECTORIALDEBUG_SPLINE}
     WriteLn('');
    {$endif}
    Result := AData.EndPath(AOnlyCreate);
  end;
end;

procedure TvDXFVectorialReader.ReadENTITIES_ATTRIB(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
begin

end;

procedure TvDXFVectorialReader.ReadENTITIES_POLYLINE(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
begin
  SetLength(Polyline, 0);
end;

procedure TvDXFVectorialReader.ReadENTITIES_VERTEX(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument);
var
  CurToken: TDXFToken;
  i, curPoint: Integer;
begin
  if not IsReadingPolyline then raise Exception.Create('[TvDXFVectorialReader.ReadENTITIES_VERTEX] Unexpected record: VERTEX before a POLYLINE');

  curPoint := Length(Polyline);
  SetLength(Polyline, curPoint+1);
  Polyline[curPoint].X := 0;
  Polyline[curPoint].Y := 0;
  Polyline[curPoint].Color := colBlack;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 62] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    // Loads the coordinates
    // With Position fixing for documents with negative coordinates
    case CurToken.GroupCode of
      10: Polyline[curPoint].X := CurToken.FloatValue - DOC_OFFSET.X;
      20: Polyline[curPoint].Y := CurToken.FloatValue - DOC_OFFSET.Y;
      62: Polyline[curPoint].Color := DXFColorIndexToFPColor(Trunc(CurToken.FloatValue));
    end;
  end;
end;

function TvDXFVectorialReader.ReadENTITIES_SEQEND(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TPath;
var
  i: Integer;
begin
  Result := nil;
  if (not IsReadingPolyline) and (not IsReadingAttrib) then raise Exception.Create('[TvDXFVectorialReader.ReadENTITIES_SEQEND] Unexpected record: SEQEND before a POLYLINE or ATTRIB');

  if IsReadingPolyline then
  begin
    // Write the Polyline to the document
    if Length(Polyline) >= 0 then // otherwise the polyline is empty of points
    begin
      AData.StartPath(Polyline[0].X, Polyline[0].Y);
      {$ifdef FPVECTORIALDEBUG_POLYLINE}
       Write(Format('POLYLINE %f,%f', [Polyline[0].X, Polyline[0].Y]));
      {$endif}
      for i := 1 to Length(Polyline)-1 do
      begin
        AData.AddLineToPath(Polyline[i].X, Polyline[i].Y, Polyline[i].Color);
        {$ifdef FPVECTORIALDEBUG_POLYLINE}
         Write(Format(' %f,%f', [Polyline[i].X, Polyline[i].Y]));
        {$endif}
      end;
      {$ifdef FPVECTORIALDEBUG_POLYLINE}
       WriteLn('');
      {$endif}
      Result := AData.EndPath(AOnlyCreate);
    end;
  end;
end;

function TvDXFVectorialReader.ReadENTITIES_MTEXT(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvText;
var
  CurToken: TDXFToken;
  i: Integer;
  PosX: Double = 0.0;
  PosY: Double = 0.0;
  PosZ: Double = 0.0;
  FontSize: Double = 10.0;
  Str: string = '';
begin
  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 40] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      1:  Str := CurToken.StrValue;
      10: PosX := CurToken.FloatValue;
      20: PosY := CurToken.FloatValue;
      30: PosZ := CurToken.FloatValue;
      40: FontSize := CurToken.FloatValue;
    end;
  end;

  // Position fixing for documents with negative coordinates
  PosX := PosX - DOC_OFFSET.X;
  PosY := PosY + FontSize - DOC_OFFSET.Y;

  //
  Result := AData.AddText(PosX, PosY, 0, '', Round(FontSize), Str, AOnlyCreate);
  Result.Font.Color := colWhite;
end;

function TvDXFVectorialReader.ReadENTITIES_LEADER(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvArrow;
var
  CurToken: TDXFToken;
  i, curPoint: Integer;
  lValueX, lValueY: Double;
  lArrow: TvArrow;
  LElementColor: TFPColor;
begin
  lArrow := TvArrow.Create(AData);
  curPoint := 0;
  LElementColor := colBlack;
  Result := nil;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 11, 21, 31, 62] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    // Loads the coordinates
    // With Position fixing for documents with negative coordinates
    case CurToken.GroupCode of
      10:
      begin
        // Starting a new point
        Inc(curPoint);

        lValueX := CurToken.FloatValue - DOC_OFFSET.X;

        case curPoint of
        1: lArrow.X := lValueX;
        2: lArrow.Base.X := lValueX;
        3: lArrow.ExtraLineBase.X := lValueX;
        end;
      end;
      20:
      begin
        lValueY := CurToken.FloatValue - DOC_OFFSET.Y;

        case curPoint of
        1: lArrow.Y := lValueY;
        2: lArrow.Base.Y := lValueY;
        3: lArrow.ExtraLineBase.Y := lValueY;
        end;
      end;
      62: LElementColor := DXFColorIndexToFPColor(Trunc(CurToken.FloatValue));
    end;
  end;

  // Give a % of the line length to the arrow head
  lArrow.ArrowLength := 0.2 * sqrt(sqr(lArrow.Base.Y - lArrow.Y) + sqr(lArrow.Base.X - lArrow.X));
  lArrow.ArrowBaseLength := lArrow.ArrowLength / 2;

  // And now write it
  lArrow.HasExtraLine := True;
  lArrow.Pen.Color := LElementColor;
  lArrow.Brush.Style := bsSolid;
  lArrow.Brush.Color := LElementColor;
  Result := lArrow;
  if not AOnlyCreate then AData.AddEntity(lArrow);
end;

function TvDXFVectorialReader.ReadENTITIES_POINT(ATokens: TDXFTokens;
  AData: TvVectorialPage; ADoc: TvVectorialDocument; AOnlyCreate: Boolean = False): TvEntity;
var
  CurToken: TDXFToken;
  i: Integer;
  CircleCenterX, CircleCenterY, CircleCenterZ, CircleRadius: Double;
begin
  CircleCenterX := 0.0;
  CircleCenterY := 0.0;
  CircleCenterZ := 0.0;
  CircleRadius := 1.0;

  for i := 0 to ATokens.Count - 1 do
  begin
    // Now read and process the item name
    CurToken := TDXFToken(ATokens.Items[i]);

    // Avoid an exception by previously checking if the conversion can be made
    if CurToken.GroupCode in [10, 20, 30, 40] then
    begin
      CurToken.FloatValue :=  StrToFloat(Trim(CurToken.StrValue), FPointSeparator);
    end;

    case CurToken.GroupCode of
      10: CircleCenterX := CurToken.FloatValue;
      20: CircleCenterY := CurToken.FloatValue;
      30: CircleCenterZ := CurToken.FloatValue;
//      40: CircleRadius := CurToken.FloatValue;
    end;
  end;

  // Position fixing for documents with negative coordinates
  CircleCenterX := CircleCenterX - DOC_OFFSET.X;
  CircleCenterY := CircleCenterY - DOC_OFFSET.Y;

  Result := AData.AddCircle(CircleCenterX, CircleCenterY, CircleRadius, AOnlyCreate);
end;

function TvDXFVectorialReader.InternalReadENTITIES(ATokenStr: string;
  ATokens: TDXFTokens; AData: TvVectorialPage; ADoc: TvVectorialDocument;
  AOnlyCreate: Boolean = False): TvEntity;
begin
  Result := nil;
  case ATokenStr of
    '3DFACE':   Result := ReadENTITIES_3DFACE(ATokens, AData, ADoc, AOnlyCreate);
    'ARC':      Result := ReadENTITIES_ARC(ATokens, AData, ADoc, AOnlyCreate);
    'CIRCLE':   Result := ReadENTITIES_CIRCLE(ATokens, AData, ADoc, AOnlyCreate);
    'DIMENSION':Result := ReadENTITIES_DIMENSION(ATokens, AData, ADoc, AOnlyCreate);
    'ELLIPSE':  Result := ReadENTITIES_ELLIPSE(ATokens, AData, ADoc, AOnlyCreate);
    'INSERT':   Result := ReadENTITIES_INSERT(ATokens, AData, ADoc, AOnlyCreate);
    'LINE':     Result := ReadENTITIES_LINE(ATokens, AData, ADoc, AOnlyCreate);
    'TEXT':     Result := ReadENTITIES_TEXT(ATokens, AData, ADoc, AOnlyCreate);
    'LWPOLYLINE':Result := ReadENTITIES_LWPOLYLINE(ATokens, AData, ADoc, AOnlyCreate);
    'SPLINE':   Result := ReadENTITIES_SPLINE(ATokens, AData, ADoc, AOnlyCreate);
    'POINT':    Result := ReadENTITIES_POINT(ATokens, AData, ADoc, AOnlyCreate);
    'MTEXT':    Result := ReadENTITIES_MTEXT(ATokens, AData, ADoc, AOnlyCreate);
    'LEADER':   Result := ReadENTITIES_LEADER(ATokens, AData, ADoc, AOnlyCreate);
    // A Attribute can have multiple child objects
    'ATTRIB':
    begin
      IsReadingAttrib := True;
      ReadENTITIES_ATTRIB(ATokens, AData, ADoc);
    end;
    // A Polyline can have multiple child objects
    'POLYLINE':
    begin
      IsReadingPolyline := True;
      ReadENTITIES_POLYLINE(ATokens, AData, ADoc);
    end;
    'VERTEX': ReadENTITIES_VERTEX(ATokens, AData, ADoc);
    'SEQEND':
    begin
      Result := ReadENTITIES_SEQEND(ATokens, AData, ADoc, AOnlyCreate);
      IsReadingPolyline := False;
      IsReadingAttrib := False;
    end;
  end;
end;

function TvDXFVectorialReader.GetCoordinateValue(AStr: shortstring): Double;
begin
  Result := 0.0;

{  if Length(AStr) <= 1 then Exit;

  Result := StrToFloat(Copy(AStr, 2, Length(AStr) - 1), FPointSeparator);}
end;

function TvDXFVectorialReader.ConvertDXFStringToUTF8(AStr: string): string;
begin
  if (ENCODING = 'UTF-8') or (ENCODING = '') then
  begin
    Result := AStr;
    Exit;
  end;

  Result := ConvertEncoding(AStr, ENCODING, 'UTF-8');
end;

function TvDXFVectorialReader.DXFColorIndexToFPColor(AColorIndex: Integer): TFPColor;
begin
  if (AColorIndex >= 0) and (AColorIndex <= 255) then
    Result := AUTOCAD_COLOR_PALETTE[AColorIndex]
  else
    raise Exception.Create(Format('[TvDXFVectorialReader.DXFColorIndexToFPVColor] Invalid DXF Color Index: %d', [AColorIndex]));
end;

procedure TvDXFVectorialReader.DXFCoordsToFPCoords(AInX, AInY, AInZ: Double;
  out AOutX, AOutY, AOutZ: Double);
begin
  AOutX := AInX - DOC_OFFSET.X;
  AOutY := AInY - DOC_OFFSET.Y;
  AOutZ := AInZ;
end;

constructor TvDXFVectorialReader.Create;
begin
  inherited Create;

  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator

  Tokenizer := TDXFTokenizer.Create;
end;

destructor TvDXFVectorialReader.Destroy;
begin
  Tokenizer.Free;

  inherited Destroy;
end;

{@@
  The information of each separate path is lost in G-Code files
  Only one path uniting all of them is created when reading G-Code
}
procedure TvDXFVectorialReader.ReadFromStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
var
  i: Integer;
  CurToken, CurTokenFirstChild: TDXFToken;
  lPage: TvVectorialPage;
begin
  // Default HEADER data
  ANGBASE := 0.0; // Starts pointing to the right / east
  ANGDIR := 0; // counter-clock wise
  // The default encoding of DXF files if a ($DWGCODEPAGE) header is not present
  // See http://www.gdal.org/ogr/drv_dxf.html
  if AData.ForcedEncodingOnRead <> '' then ENCODING := AData.ForcedEncodingOnRead
  else ENCODING := 'cp1252';

  Tokenizer.ReadFromStrings(AStrings);

  lPage := AData.AddPage();
  lPage.BackgroundColor := colBlack;
  lPage.RenderInfo.AdjustPenColorToBackground := True;
  lPage.RenderInfo.BackgroundColor := colBlack;

  for i := 0 to Tokenizer.Tokens.Count - 1 do
  begin
    CurToken := TDXFToken(Tokenizer.Tokens.Items[i]);
    if (CurToken.Childs = nil) or (CurToken.Childs.Count=0) then Continue;
    CurTokenFirstChild := TDXFToken(CurToken.Childs.Items[0]);

    if CurTokenFirstChild.StrValue = 'HEADER' then
      ReadHEADER(CurToken.Childs, lPage, AData)
    else if CurTokenFirstChild.StrValue = 'TABLES' then
      ReadTABLES(CurToken.Childs, lPage, AData)
    else if CurTokenFirstChild.StrValue = 'BLOCKS' then
      ReadBLOCKS(CurToken.Childs, lPage, AData)
    else if CurTokenFirstChild.StrValue = 'ENTITIES' then
      ReadENTITIES(CurToken.Childs, lPage, AData);
  end;

  // Update properties from the document
  AData.Encoding := ENCODING;
end;

initialization

  RegisterVectorialReader(TvDXFVectorialReader, vfDXF);

end.

