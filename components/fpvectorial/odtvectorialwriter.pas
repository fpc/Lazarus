{
Writes an ODT Document

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

An OpenDocument document is a compressed ZIP file with the following files inside:

content.xml     - Actual contents
meta.xml        - Authoring data
settings.xml    - User persistent viewing information, such as zoom, cursor position, etc.
styles.xml      - Styles, which are the only way to do formatting
mimetype        - application/vnd.oasis.opendocument.text
META-INF\manifest.xml  - Describes the other files in the archive

Specifications obtained from:

http://docs.oasis-open.org/office/v1.1/OS/OpenDocument-v1.1.pdf

Example of content.xml structure:

<?xml version="1.0" encoding="UTF-8"?>
<office:document-content xmlns:office="urn:oasis:names:tc:opendocument:xmlns:office:1.0" .....>
<office:scripts/>
<office:automatic-styles>
  <style:style style:name="dp1" style:family="drawing-page"/>
  <style:style style:name="gr1" style:family="graphic" style:parent-style-name="standard">
  ....
</office:automatic-styles>
<office:body>
<office:drawing>
  <draw:page draw:name="page1" draw:style-name="dp1" draw:master-page-name="Oletus">
    <draw:ellipse draw:style-name="gr2" draw:text-style-name="P1" draw:layer="layout" svg:width="11cm" svg:height="3cm" svg:x="5.5cm" svg:y="6.5cm">
      <text:p/>
    </draw:ellipse>
    ... other elements in the page...
  </draw:page>
</office:drawing>
</office:body>
</office:document-content>

Validator for ODF 1.0
http://opendocumentfellowship.com/validator
Validator for ODF 1.2
http://odf-validator2.rhcloud.com/odf-validator2/

AUTHORS: Felipe Monteiro de Carvalho
}
unit odtvectorialwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  zipper, zstream, {NOTE: might require zipper from FPC 2.6.2+ }
  fpimage, fpcanvas,
  fpvectorial, fpvutils, lazutf8;

type
  { TvODTVectorialWriter }

  // Writes ODT 1.2
  TvODTVectorialWriter = class(TvCustomVectorialWriter)
  private
    FPointSeparator: TFormatSettings;
    // Strings with the contents of files
    FMeta, FSettings, FStyles, FContent, FMimetype: string;
    FAutomaticStyles, FMasterStyles: string; // built during writedocument, used during writestyle
    FAutomaticStyleID : Integer;
    FContentAutomaticStyles : string; // built during writedocument, used during writedocument
    FContentAutomaticStyleID : Integer;

    FNewPageSequence : Boolean;

    FMetaInfManifest, FManifestRDF: string;
    // helper routines
    function StyleNameToODTStyleName(AData: TvVectorialDocument; AStyleIndex: Integer; AToContentAutoStyle: Boolean = False): string; overload;
    function StyleNameToODTStyleName(AData: TvVectorialDocument; AStyle: TvStyle; AToContentAutoStyle: Boolean = False): string; overload;
    function FloatToODTText(AFloat: Double): string;
    function BordersToString(ATableBorders, ACellBorders: TvTableBorders; ATopCell,
      ABottomCell, ALeftCell, ARightCell: Boolean): String;
    // Routines to write those files
    procedure WriteMimetype;
    procedure WriteMetaInfManifest;
    procedure WriteManifestRDF;
    procedure WriteMeta;
    procedure WriteSettings;
    procedure WriteStyles(AData: TvVectorialDocument);
    procedure WriteDocument(AData: TvVectorialDocument);
    procedure WritePage(ACurPage: TvTextPageSequence; AData: TvVectorialDocument);
    //
    procedure WriteParagraph(AEntity: TvParagraph; ACurPage: TvTextPageSequence; AData: TvVectorialDocument);
    procedure WriteTable(ATable: TvTable; ACurPage: TvTextPageSequence;
      AData: TvVectorialDocument);
    procedure WriteTextSpan(AEntity: TvText; AParagraph: TvParagraph;
      ACurPage: TvTextPageSequence; AData: TvVectorialDocument);
    procedure WriteBulletList(AEntity: TvBulletList; ACurPage: TvTextPageSequence; AData: TvVectorialDocument);
    // Routines to write parts of those files
    function WriteStylesXMLAsString: string;
    //
  public
    { General reading methods }
    constructor Create; override;
    destructor Destroy; override;
    procedure WriteToFile(AFileName: string; AData: TvVectorialDocument); override;
    procedure WriteToStream(AStream: TStream; AData: TvVectorialDocument); override;
  end;

implementation

uses
  htmlelements;

const
  { OpenDocument general XML constants }
  XML_HEADER           = '<?xml version="1.0" encoding="utf-8" ?>';

  { OpenDocument Directory structure constants }
  OPENDOC_PATH_CONTENT   = 'content.xml';
  OPENDOC_PATH_META      = 'meta.xml';
  OPENDOC_PATH_SETTINGS  = 'settings.xml';
  OPENDOC_PATH_STYLES    = 'styles.xml';
  OPENDOC_PATH_MIMETYPE  = 'mimetype';
  OPENDOC_PATH_METAINF = 'META-INF' + '/';
  OPENDOC_PATH_METAINF_MANIFEST = 'META-INF' + '/' + 'manifest.xml';
  OPENDOC_PATH_MANIFESTRDF = 'manifest.rdf';

  { OpenDocument schemas constants }
  SCHEMAS_XMLNS          = 'http://schemas.openxmlformats.org/officeDocument/2006/extended-properties';
  SCHEMAS_XMLNS_CALCEXT  = 'urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0';
  SCHEMAS_XMLNS_CHART    = 'urn:oasis:names:tc:opendocument:xmlns:chart:1.0';
  SCHEMAS_XMLNS_CONFIG   = 'urn:oasis:names:tc:opendocument:xmlns:config:1.0';
  SCHEMAS_XMLNS_CSS3T    = 'http://www.w3.org/TR/css3-text/';
  SCHEMAS_XMLNS_DC       = 'http://purl.org/dc/elements/1.1/';
  SCHEMAS_XMLNS_DCTERMS  = 'http://purl.org/dc/terms/';
  SCHEMAS_XMLNS_DOM      = 'http://www.w3.org/2001/xml-events';
  SCHEMAS_XMLNS_DR3D     = 'urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0';
  SCHEMAS_XMLNS_DRAW     = 'urn:oasis:names:tc:opendocument:xmlns:drawing:1.0';
  SCHEMAS_XMLNS_DRAWOOO  = 'http://openoffice.org/2010/draw';
  SCHEMAS_XMLNS_FIELD    = 'urn:openoffice:names:experimental:ooo-ms-interop:xmlns:field:1.0';
  SCHEMAS_XMLNS_FO       = 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0';
  SCHEMAS_XMLNS_FORM     = 'urn:oasis:names:tc:opendocument:xmlns:form:1.0';
  SCHEMAS_XMLNS_FORMX    = 'urn:openoffice:names:experimental:ooxml-odf-interop:xmlns:form:1.0';
  SCHEMAS_XMLNS_GRDDL    = 'http://www.w3.org/2003/g/data-view#';
  SCHEMAS_XMLNS_MANIFEST = 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0';
  SCHEMAS_XMLNS_MATH     = 'http://www.w3.org/1998/Math/MathML';
  SCHEMAS_XMLNS_META     = 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0';
  SCHEMAS_XMLNS_NUMBER   = 'urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0';
  SCHEMAS_XMLNS_OF       = 'urn:oasis:names:tc:opendocument:xmlns:of:1.2';
  SCHEMAS_XMLNS_OFFICE   = 'urn:oasis:names:tc:opendocument:xmlns:office:1.0';
  SCHEMAS_XMLNS_OFFICEOOO= 'http://openoffice.org/2009/office';
  SCHEMAS_XMLNS_OOO      = 'http://openoffice.org/2004/office';
  SCHEMAS_XMLNS_OOOC     = 'http://openoffice.org/2004/calc';
  SCHEMAS_XMLNS_OOOW     = 'http://openoffice.org/2004/writer';
  SCHEMAS_XMLNS_RPT      = 'http://openoffice.org/2005/report';
  SCHEMAS_XMLNS_SCRIPT   = 'urn:oasis:names:tc:opendocument:xmlns:script:1.0';
  SCHEMAS_XMLNS_STYLE    = 'urn:oasis:names:tc:opendocument:xmlns:style:1.0';
  SCHEMAS_XMLNS_SVG      = 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0';
  SCHEMAS_XMLNS_TABLE    = 'urn:oasis:names:tc:opendocument:xmlns:table:1.0';
  SCHEMAS_XMLNS_TABLEOOO = 'http://openoffice.org/2009/table';
  SCHEMAS_XMLNS_TEXT     = 'urn:oasis:names:tc:opendocument:xmlns:text:1.0';
  SCHEMAS_XMLNS_V        = 'urn:schemas-microsoft-com:vml';
  SCHEMAS_XMLNS_XFORMS   = 'http://www.w3.org/2002/xforms';
  SCHEMAS_XMLNS_XHTML    = 'http://www.w3.org/1999/xhtml';
  SCHEMAS_XMLNS_XLINK    = 'http://www.w3.org/1999/xlink';
  SCHEMAS_XMLNS_XSD      = 'http://www.w3.org/2001/XMLSchema';
  SCHEMAS_XMLNS_XSI      = 'http://www.w3.org/2001/XMLSchema-instance';

  // SVG requires hardcoding a DPI value

  // The Opera Browser and Inkscape use 90 DPI, so we follow that

  // 1 Inch = 25.4 milimiters
  // 90 inches per pixel = (1 / 90) * 25.4 = 0.2822
  // FLOAT_MILIMETERS_PER_PIXEL = 0.3528; // DPI 72 = 1 / 72 inches per pixel

  FLOAT_MILIMETERS_PER_PIXEL = 0.2822; // DPI 90 = 1 / 90 inches per pixel
  FLOAT_PIXELS_PER_MILIMETER = 3.5433; // DPI 90 = 1 / 90 inches per pixel

function TvODTVectorialWriter.StyleNameToODTStyleName(
  AData: TvVectorialDocument; AStyleIndex: Integer; AToContentAutoStyle: Boolean): string;
var
  lStyle: TvStyle;
begin
  lStyle := AData.GetStyle(AStyleIndex);
  if AToContentAutoStyle then
  begin
    Result := 'P' + IntToStr(AStyleIndex);
  end
  else
  begin
    Result := StringReplace(lStyle.Name, ' ', '_', [rfReplaceAll, rfIgnoreCase]);
  end;
end;

function TvODTVectorialWriter.StyleNameToODTStyleName(
  AData: TvVectorialDocument; AStyle: TvStyle; AToContentAutoStyle: Boolean
  ): string;
var
  lStyleIndex: Integer;
begin
  lStyleIndex := AData.FindStyleIndex(AStyle);
  Result := StyleNameToODTStyleName(AData, lStyleIndex, AToContentAutoStyle);
end;

function TvODTVectorialWriter.FloatToODTText(AFloat: Double): string;
begin
  Result := FloatToStr(AFloat, FPointSeparator);
end;

procedure TvODTVectorialWriter.WriteMimetype;
begin
  FMimetype := 'application/vnd.oasis.opendocument.text';
end;

procedure TvODTVectorialWriter.WriteMetaInfManifest;
begin
  FMetaInfManifest :=
   XML_HEADER + LineEnding +
   '<manifest:manifest xmlns:manifest="' + SCHEMAS_XMLNS_MANIFEST + '"  manifest:version="1.2">' + LineEnding +
   '  <manifest:file-entry manifest:full-path="/" manifest:media-type="application/vnd.oasis.opendocument.text" />' + LineEnding + // manifest:version="1.2"
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="content.xml" />' + LineEnding +
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="styles.xml" />' + LineEnding +
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="meta.xml" />' + LineEnding +
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="settings.xml" />' + LineEnding +
   '  <manifest:file-entry manifest:full-path="manifest.rdf" manifest:media-type="application/rdf+xml"/>' + LineEnding +
   '</manifest:manifest>';
end;

procedure TvODTVectorialWriter.WriteManifestRDF;
begin
  FManifestRDF :=
   XML_HEADER + LineEnding +
   '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">' + LineEnding +
   '  <rdf:Description rdf:about="styles.xml">' + LineEnding +
   '    <rdf:type rdf:resource="http://docs.oasis-open.org/ns/office/1.2/meta/odf#StylesFile"/>' + LineEnding +
   '  </rdf:Description>' + LineEnding +
   '  <rdf:Description rdf:about="">' + LineEnding +
   '    <ns0:hasPart xmlns:ns0="http://docs.oasis-open.org/ns/office/1.2/meta/pkg#" rdf:resource="styles.xml"/>' + LineEnding +
   '  </rdf:Description>' + LineEnding +
   '  <rdf:Description rdf:about="content.xml">' + LineEnding +
   '    <rdf:type rdf:resource="http://docs.oasis-open.org/ns/office/1.2/meta/odf#ContentFile"/>' + LineEnding +
   '  </rdf:Description>' + LineEnding +
   '  <rdf:Description rdf:about="">' + LineEnding +
   '    <ns0:hasPart xmlns:ns0="http://docs.oasis-open.org/ns/office/1.2/meta/pkg#" rdf:resource="content.xml"/>' + LineEnding +
   '  </rdf:Description>' + LineEnding +
   '  <rdf:Description rdf:about="">' + LineEnding +
   '    <rdf:type rdf:resource="http://docs.oasis-open.org/ns/office/1.2/meta/pkg#Document"/>' + LineEnding +
   '  </rdf:Description>' + LineEnding +
   '</rdf:RDF>' + LineEnding;
end;

procedure TvODTVectorialWriter.WriteMeta;
begin
  FMeta :=
   XML_HEADER + LineEnding +
   '<office:document-meta xmlns:office="' + SCHEMAS_XMLNS_OFFICE + '"' +
     ' xmlns:xlink="' + SCHEMAS_XMLNS_XLINK + '"' +
     ' xmlns:dc="' + SCHEMAS_XMLNS_DC + '"' +
     ' xmlns:ooo="' + SCHEMAS_XMLNS_OOO + '"' +
     ' xmlns:grddl="' + SCHEMAS_XMLNS_GRDDL + '"' +
     ' xmlns:meta="' + SCHEMAS_XMLNS_META + '"' +
     ' xmlns="' + SCHEMAS_XMLNS + '"' +
     ' xmlns:ex="' + SCHEMAS_XMLNS + '" office:version="1.2">' + LineEnding +
   '  <office:meta>' + LineEnding +
//    <meta:creation-date>2013-07-21T09:29:41.06</meta:creation-date>
//    <dc:date>2013-07-21T20:13:32.29</dc:date>
   '    <meta:generator>FPVectorial Library</meta:generator>' + LineEnding +
//    <meta:document-statistic meta:table-count="0" meta:image-count="0" meta:object-count="0" meta:page-count="1" meta:paragraph-count="19" meta:word-count="312" meta:character-count="2028" meta:non-whitespace-character-count="2028" />
   '  </office:meta>' + LineEnding +
   '</office:document-meta>';
end;

procedure TvODTVectorialWriter.WriteSettings;
begin
  FSettings :=
   XML_HEADER + LineEnding +
   '<office:document-settings xmlns:office="' + SCHEMAS_XMLNS_OFFICE + '"' +
     ' xmlns:xlink="' + SCHEMAS_XMLNS_XLINK + '"' +
     ' xmlns:config="' + SCHEMAS_XMLNS_CONFIG + '"' +
     ' xmlns:ooo="' + SCHEMAS_XMLNS_OOO + '" office:version="1.2">' + LineEnding +
   '<office:settings>' + LineEnding +
   '  <config:config-item-set config:name="ooo:view-settings">' + LineEnding +
   '    <config:config-item config:name="ViewAreaTop" config:type="int">0</config:config-item>' + LineEnding +
   '    <config:config-item config:name="ViewAreaLeft" config:type="int">0</config:config-item>' + LineEnding +
   '    <config:config-item config:name="ViewAreaWidth" config:type="int">25534</config:config-item>' + LineEnding +
   '    <config:config-item config:name="ViewAreaHeight" config:type="int">9289</config:config-item>' + LineEnding +
{
      <config:config-item config:name="ShowRedlineChanges" config:type="boolean">true</config:config-item>
      <config:config-item config:name="InBrowseMode" config:type="boolean">false</config:config-item>
      <config:config-item-map-indexed config:name="Views">
        <config:config-item-map-entry>
          <config:config-item config:name="ViewId" config:type="string">view2</config:config-item>
          <config:config-item config:name="ViewLeft" config:type="int">4267</config:config-item>
          <config:config-item config:name="ViewTop" config:type="int">2925</config:config-item>
          <config:config-item config:name="VisibleLeft" config:type="int">0</config:config-item>
          <config:config-item config:name="VisibleTop" config:type="int">0</config:config-item>
          <config:config-item config:name="VisibleRight" config:type="int">25532</config:config-item>
          <config:config-item config:name="VisibleBottom" config:type="int">9287</config:config-item>
          <config:config-item config:name="ZoomType" config:type="short">0</config:config-item>
          <config:config-item config:name="ViewLayoutColumns" config:type="short">0</config:config-item>
          <config:config-item config:name="ViewLayoutBookMode" config:type="boolean">false</config:config-item>
          <config:config-item config:name="ZoomFactor" config:type="short">100</config:config-item>
          <config:config-item config:name="IsSelectedFrame" config:type="boolean">false</config:config-item>
        </config:config-item-map-entry>
      </config:config-item-map-indexed>
    </config:config-item-set>
}
   '  </config:config-item-set>' + LineEnding +
   '  <config:config-item-set config:name="ooo:configuration-settings">' + LineEnding +
   '    <config:config-item config:name="ChartAutoUpdate" config:type="boolean">true</config:config-item>' + LineEnding +
{
      <config:config-item config:name="IsLabelDocument" config:type="boolean">false</config:config-item>
      <config:config-item config:name="MathBaselineAlignment" config:type="boolean">true</config:config-item>
      <config:config-item config:name="Rsid" config:type="int">490666</config:config-item>
      <config:config-item config:name="OutlineLevelYieldsNumbering" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrintLeftPages" config:type="boolean">true</config:config-item>
      <config:config-item config:name="DoNotJustifyLinesWithManualBreak" config:type="boolean">false</config:config-item>
      <config:config-item config:name="ClippedPictures" config:type="boolean">false</config:config-item>
      <config:config-item config:name="AlignTabStopPosition" config:type="boolean">true</config:config-item>
      <config:config-item config:name="PrintTextPlaceholder" config:type="boolean">false</config:config-item>
      <config:config-item config:name="UseOldNumbering" config:type="boolean">false</config:config-item>
      <config:config-item config:name="CurrentDatabaseCommand" config:type="string" />
      <config:config-item config:name="ProtectForm" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrintBlackFonts" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrintProspectRTL" config:type="boolean">false</config:config-item>
      <config:config-item config:name="BackgroundParaOverDrawings" config:type="boolean">false</config:config-item>
      <config:config-item config:name="FloattableNomargins" config:type="boolean">false</config:config-item>
      <config:config-item config:name="SmallCapsPercentage66" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrintControls" config:type="boolean">true</config:config-item>
      <config:config-item config:name="EmbedSystemFonts" config:type="boolean">false</config:config-item>
      <config:config-item config:name="CharacterCompressionType" config:type="short">0</config:config-item>
      <config:config-item config:name="PrintHiddenText" config:type="boolean">false</config:config-item>
      <config:config-item config:name="UseFormerTextWrapping" config:type="boolean">false</config:config-item>
      <config:config-item config:name="IsKernAsianPunctuation" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrintProspect" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrintEmptyPages" config:type="boolean">false</config:config-item>
      <config:config-item config:name="UnbreakableNumberings" config:type="boolean">false</config:config-item>
      <config:config-item config:name="UseFormerObjectPositioning" config:type="boolean">false</config:config-item>
      <config:config-item config:name="ConsiderTextWrapOnObjPos" config:type="boolean">false</config:config-item>
      <config:config-item config:name="TableRowKeep" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrintReversed" config:type="boolean">false</config:config-item>
      <config:config-item config:name="TabsRelativeToIndent" config:type="boolean">true</config:config-item>
      <config:config-item config:name="PrintRightPages" config:type="boolean">true</config:config-item>
      <config:config-item config:name="PrintPaperFromSetup" config:type="boolean">false</config:config-item>
      <config:config-item config:name="AddFrameOffsets" config:type="boolean">false</config:config-item>
      <config:config-item config:name="AddParaSpacingToTableCells" config:type="boolean">true</config:config-item>
      <config:config-item config:name="UpdateFromTemplate" config:type="boolean">true</config:config-item>
      <config:config-item config:name="AddExternalLeading" config:type="boolean">true</config:config-item>
      <config:config-item config:name="PrintSingleJobs" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrinterIndependentLayout" config:type="string">high-resolution</config:config-item>
      <config:config-item config:name="RsidRoot" config:type="int">470846</config:config-item>
      <config:config-item config:name="LinkUpdateMode" config:type="short">1</config:config-item>
      <config:config-item config:name="PrintAnnotationMode" config:type="short">0</config:config-item>
      <config:config-item config:name="TabOverMargin" config:type="boolean">false</config:config-item>
      <config:config-item config:name="UseOldPrinterMetrics" config:type="boolean">false</config:config-item>
      <config:config-item config:name="RedlineProtectionKey" config:type="base64Binary" />
      <config:config-item config:name="PrinterSetup" config:type="base64Binary" />
      <config:config-item config:name="IgnoreFirstLineIndentInNumbering" config:type="boolean">false</config:config-item>
      <config:config-item config:name="CollapseEmptyCellPara" config:type="boolean">true</config:config-item>
      <config:config-item config:name="PrinterName" config:type="string" />
      <config:config-item config:name="EmbedFonts" config:type="boolean">false</config:config-item>
      <config:config-item config:name="InvertBorderSpacing" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrintPageBackground" config:type="boolean">true</config:config-item>
      <config:config-item config:name="DoNotCaptureDrawObjsOnPage" config:type="boolean">false</config:config-item>
      <config:config-item config:name="TabOverflow" config:type="boolean">true</config:config-item>
      <config:config-item config:name="ApplyUserData" config:type="boolean">true</config:config-item>
      <config:config-item config:name="TabAtLeftIndentForParagraphsInList" config:type="boolean">false</config:config-item>
      <config:config-item config:name="UnxForceZeroExtLeading" config:type="boolean">false</config:config-item>
      <config:config-item config:name="SaveVersionOnClose" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrintFaxName" config:type="string" />
      <config:config-item config:name="StylesNoDefault" config:type="boolean">false</config:config-item>
      <config:config-item config:name="AddParaTableSpacing" config:type="boolean">true</config:config-item>
      <config:config-item config:name="PrintDrawings" config:type="boolean">true</config:config-item>
      <config:config-item config:name="LoadReadonly" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrintGraphics" config:type="boolean">true</config:config-item>
      <config:config-item config:name="FieldAutoUpdate" config:type="boolean">true</config:config-item>
      <config:config-item config:name="AllowPrintJobCancel" config:type="boolean">true</config:config-item>
      <config:config-item config:name="UseFormerLineSpacing" config:type="boolean">false</config:config-item>
      <config:config-item config:name="SaveGlobalDocumentLinks" config:type="boolean">false</config:config-item>
      <config:config-item config:name="CurrentDatabaseDataSource" config:type="string" />
      <config:config-item config:name="IgnoreTabsAndBlanksForLineCalculation" config:type="boolean">false</config:config-item>
      <config:config-item config:name="CurrentDatabaseCommandType" config:type="int">0</config:config-item>
      <config:config-item config:name="DoNotResetParaAttrsForNumFont" config:type="boolean">false</config:config-item>
      <config:config-item config:name="ClipAsCharacterAnchoredWriterFlyFrames" config:type="boolean">false</config:config-item>
      <config:config-item config:name="PrintTables" config:type="boolean">true</config:config-item>
      <config:config-item config:name="AddParaTableSpacingAtStart" config:type="boolean">true</config:config-item>
    </config:config-item-set>
}
   '  </config:config-item-set>' + LineEnding +
   '  </office:settings>' + LineEnding +
   '</office:document-settings>';
end;

procedure TvODTVectorialWriter.WriteStyles(AData: TvVectorialDocument);
var
  i: Integer;
  CurStyle: TvStyle;
  lTextPropsStr, lParagraphPropsStr, lCurStyleTmpStr, CurStyleParent : string;
Const
  LU_ALIGN: Array [TvStyleAlignment] Of String =
    ('start', 'end', 'justify', 'center');
begin
  FStyles :=
   XML_HEADER + LineEnding +
   '<office:document-styles xmlns:office="' + SCHEMAS_XMLNS_OFFICE + '"' +
     ' xmlns:style="' + SCHEMAS_XMLNS_STYLE + '"' +
     ' xmlns:text="' + SCHEMAS_XMLNS_TEXT + '"' +
     ' xmlns:table="' + SCHEMAS_XMLNS_TABLE + '"' +
     ' xmlns:draw="' + SCHEMAS_XMLNS_DRAW + '"' +
     ' xmlns:fo="' + SCHEMAS_XMLNS_FO + '"' +
     ' xmlns:xlink="' + SCHEMAS_XMLNS_XLINK + '"' +
     ' xmlns:dc="' + SCHEMAS_XMLNS_DC + '"' +
     ' xmlns:meta="' + SCHEMAS_XMLNS_META + '"' +
     ' xmlns:number="' + SCHEMAS_XMLNS_NUMBER + '"' +
     ' xmlns:svg="' + SCHEMAS_XMLNS_SVG + '"' +
     ' xmlns:chart="' + SCHEMAS_XMLNS_CHART + '"' +
     ' xmlns:dr3d="' + SCHEMAS_XMLNS_DR3D + '"' +
     ' xmlns:math="' + SCHEMAS_XMLNS_MATH + '"' +
     ' xmlns:form="' + SCHEMAS_XMLNS_FORM + '"' +
     ' xmlns:script="' + SCHEMAS_XMLNS_SCRIPT + '"' +
     ' xmlns:ooo="' + SCHEMAS_XMLNS_OOO + '"' +
     ' xmlns:ooow="' + SCHEMAS_XMLNS_OOOW + '"' +
     ' xmlns:oooc="' + SCHEMAS_XMLNS_OOOC + '"' +
     ' xmlns:dom="' + SCHEMAS_XMLNS_DOM + '"' +
     ' xmlns:rpt="' + SCHEMAS_XMLNS_RPT + '"' +
     ' xmlns:of="' + SCHEMAS_XMLNS_OF + '"' +
     ' xmlns:xhtml="' + SCHEMAS_XMLNS_XHTML + '"' +
     ' xmlns:grddl="' + SCHEMAS_XMLNS_GRDDL + '"' +
     ' xmlns:officeooo="' + SCHEMAS_XMLNS_OFFICEOOO + '"' +
     ' xmlns:tableooo="' + SCHEMAS_XMLNS_TABLEOOO + '"' +
     ' xmlns:drawooo="' + SCHEMAS_XMLNS_DRAWOOO + '"' +
     ' xmlns:calcext="' + SCHEMAS_XMLNS_CALCEXT + '"' +
     ' xmlns:css3t="' + SCHEMAS_XMLNS_CSS3T + '"' +
     ' office:version="1.2">' + LineEnding;

  // TODO:  Parse Styles for Fonts not included in the list below...
  FStyles := FStyles +
   '<office:font-face-decls>' + LineEnding +
   '  <style:font-face style:name="Mangal1" svg:font-family="Mangal" />' + LineEnding +
   '  <style:font-face style:name="OpenSymbol" svg:font-family="OpenSymbol" />' + LineEnding +
   '  <style:font-face style:name="Times New Roman" svg:font-family="Times New Roman" style:font-family-generic="roman" style:font-pitch="variable" />' + LineEnding +
   '  <style:font-face style:name="Arial" svg:font-family="Arial" />' + LineEnding +
   '  <style:font-face style:name="Verdana" svg:font-family="Verdana" />' + LineEnding +
   '  <style:font-face style:name="Mangal" svg:font-family="Mangal" style:font-family-generic="system" style:font-pitch="variable" />' + LineEnding +
   '  <style:font-face style:name="Microsoft YaHei" svg:font-family="''Microsoft YaHei''" style:font-family-generic="system" style:font-pitch="variable" />' + LineEnding +
   '  <style:font-face style:name="SimSun" svg:font-family="SimSun" style:font-family-generic="system" style:font-pitch="variable" />' + LineEnding +
   '</office:font-face-decls>' + LineEnding;

  // ----------------------------
  // Styles
  // ----------------------------

  FStyles := FStyles +
   '<office:styles>' + LineEnding;

  FStyles := FStyles +
   '  <style:default-style style:family="graphic">' + LineEnding +
   '    <style:graphic-properties svg:stroke-color="#3465af" draw:fill-color="#729fcf" fo:wrap-option="no-wrap" draw:shadow-offset-x="0.3cm" draw:shadow-offset-y="0.3cm" draw:start-line-spacing-horizontal="0.283cm" draw:start-line-spacing-vertical="0.283cm" draw:end-line-spacing-horizontal="0.283cm" draw:end-line-spacing-vertical="0.283cm" style:flow-with-text="false" />' + LineEnding +
   '    <style:paragraph-properties style:text-autospace="ideograph-alpha" style:line-break="strict" style:writing-mode="lr-tb" style:font-independent-line-spacing="false">' + LineEnding +
   '      <style:tab-stops />' + LineEnding +
   '    </style:paragraph-properties>' + LineEnding +
   '    <style:text-properties style:use-window-font-color="true" fo:font-size="12pt" fo:language="fi" fo:country="FI" style:letter-kerning="true" style:font-size-asian="10.5pt" style:language-asian="zh" style:country-asian="CN" style:font-size-complex="12pt" style:language-complex="hi" style:country-complex="IN" />' + LineEnding +
   '  </style:default-style>' + LineEnding +
   '  <style:default-style style:family="paragraph">' + LineEnding +
   '    <style:paragraph-properties fo:hyphenation-ladder-count="no-limit" style:text-autospace="ideograph-alpha" style:punctuation-wrap="hanging" style:line-break="strict" style:tab-stop-distance="1.251cm" style:writing-mode="page" />' + LineEnding +
   '    <style:text-properties style:use-window-font-color="true" style:font-name="Times New Roman" fo:font-size="12pt" fo:language="fi" fo:country="FI" style:letter-kerning="true" style:font-name-asian="SimSun" style:font-size-asian="10.5pt" style:language-asian="zh" style:country-asian="CN" style:font-name-complex="Mangal" style:font-size-complex="12pt" style:language-complex="hi" style:country-complex="IN" fo:hyphenate="false" fo:hyphenation-remain-char-count="2" fo:hyphenation-push-char-count="2" />' + LineEnding +
   '  </style:default-style>' + LineEnding +
   '  <style:default-style style:family="table">' + LineEnding +
   '    <style:table-properties table:border-model="collapsing" />' + LineEnding +
   '  </style:default-style>' + LineEnding +
   '  <style:default-style style:family="table-row">' + LineEnding +
   '    <style:table-row-properties fo:keep-together="auto" />' + LineEnding +
   '  </style:default-style>' + LineEnding;

  FStyles := FStyles +
   '  <style:style style:name="Standard" style:family="paragraph" style:class="text" />' + LineEnding;

  for i := 0 to AData.GetStyleCount() - 1 do
  begin
    lTextPropsStr := '';
    lParagraphPropsStr := '';
    CurStyle := AData.GetStyle(i);

    if CurStyle.Parent = nil then CurStyleParent := 'Standard'
    else CurStyleParent := StyleNameToODTStyleName(AData, AData.FindStyleIndex(CurStyle.Parent), False);

    if spbfFontSize in CurStyle.SetElements then
    begin
      lTextPropsStr := lTextPropsStr + ' fo:font-size="'+IntToStr(CurStyle.Font.Size)+'pt" ';
      lTextPropsStr := lTextPropsStr + ' fo:font-size-asian="'+IntToStr(CurStyle.Font.Size)+'pt" ';
      lTextPropsStr := lTextPropsStr + ' fo:font-size-complex="'+IntToStr(CurStyle.Font.Size)+'pt" ';
    end;
    if spbfFontName in CurStyle.SetElements then
    begin
      lTextPropsStr := lTextPropsStr + ' style:font-name="'+CurStyle.Font.Name+'" ';
      lTextPropsStr := lTextPropsStr + ' style:font-name-asian="Microsoft YaHei" ';
      lTextPropsStr := lTextPropsStr + ' style:font-name-complex="Mangal" ';
    end;
    if (spbfFontBold in CurStyle.SetElements) then
    begin
      if CurStyle.Font.Bold then
      begin
        lTextPropsStr := lTextPropsStr + ' fo:font-weight="bold" ';
        lTextPropsStr := lTextPropsStr + ' style:font-weight-asian="bold" ';
        lTextPropsStr := lTextPropsStr + ' style:font-weight-complex="bold" ';
      end
      else
      begin
        lTextPropsStr := lTextPropsStr + ' fo:font-weight="normal" ';
        lTextPropsStr := lTextPropsStr + ' style:font-weight-asian="normal" ';
        lTextPropsStr := lTextPropsStr + ' style:font-weight-complex="normal" ';
      end;
    end;
    if (spbfFontItalic in CurStyle.SetElements) then
    begin
      if CurStyle.Font.Italic then
      begin
        lTextPropsStr := lTextPropsStr + ' fo:font-style="italic" ';
        lTextPropsStr := lTextPropsStr + ' style:font-style-asian="italic" ';
        lTextPropsStr := lTextPropsStr + ' style:font-style-complex="italic" ';
      end
      else
      begin
        // ToDo
      end;
    end;

    if CurStyle.GetKind() = vskTextSpan then
    begin
      {
      <style:style style:name="MT2" style:family="text">
        <style:text-properties fo:font-style="italic" fo:font-weight="normal" officeooo:rsid="0009f49c" style:font-style-asian="italic" style:font-weight-asian="normal" style:font-style-complex="italic" style:font-weight-complex="normal" />
      </style:style>
      }
      lCurStyleTmpStr := // tmp string to help see the text in the debugger
       '  <style:style style:name="'+StyleNameToODTStyleName(AData, i, False)+'" style:display-name="'+ CurStyle.Name +'" style:family="text" style:parent-style-name="'+CurStyleParent+'" >' + LineEnding +
       '    <style:text-properties '+lTextPropsStr+' />' + LineEnding +
       '  </style:style>' + LineEnding;
      FStyles := FStyles + lCurStyleTmpStr;
    end
    // Paragraph kind
    else
    begin
      lParagraphPropsStr := '';

      // If any one value in here is set, then ALL inherited values are overridden
      // In other words, we must fully define the style paragraph properties,
      // we can't rely on LibreOffice Style Inheritance...
      // TODO: Confirm if this applies to Text Properties as well...

      if sseMarginTop in CurStyle.SetElements then
        lParagraphPropsStr := lParagraphPropsStr + 'fo:margin-top="'+FloatToODTText(CurStyle.MarginTop)+'mm" ';
      if sseMarginBottom in CurStyle.SetElements then
        lParagraphPropsStr := lParagraphPropsStr + 'fo:margin-bottom="'+FloatToODTText(CurStyle.MarginBottom)+'mm" ';
      if sseMarginLeft in CurStyle.SetElements then
        lParagraphPropsStr := lParagraphPropsStr + 'fo:margin-left="'+FloatToODTText(CurStyle.MarginLeft)+'mm" ';
      if sseMarginRight in CurStyle.SetElements then
        lParagraphPropsStr := lParagraphPropsStr + 'fo:margin-right="'+FloatToODTText(CurStyle.MarginRight)+'mm" ';
      if (spbfAlignment in CurStyle.SetElements) then
        lParagraphPropsStr := lParagraphPropsStr + 'fo:text-align="'+LU_ALIGN[CurStyle.Alignment]+'" ';
      if CurStyle.SuppressSpacingBetweenSameParagraphs then
        lParagraphPropsStr := lParagraphPropsStr + 'style:contextual-spacing="true" ';
      //else
      //  lParagraphPropsStr := lParagraphPropsStr + 'style:contextual-spacing="false" ';

      lCurStyleTmpStr := // tmp string to help see the text in the debugger
       '  <style:style style:name="'+StyleNameToODTStyleName(AData, i, False)+'" style:display-name="'+ CurStyle.Name +'" style:family="paragraph" style:parent-style-name="'+CurStyleParent+'" style:class="text">' + LineEnding +
       '    <style:paragraph-properties '+lParagraphPropsStr+' />' + LineEnding +
       '    <style:text-properties '+lTextPropsStr+' />' + LineEnding +
       '  </style:style>' + LineEnding;
      FStyles := FStyles + lCurStyleTmpStr;
    end;
{
    <style:style style:name="Heading" style:family="paragraph" style:parent-style-name="Standard" style:next-style-name="Text_20_body" style:class="text">
      <style:paragraph-properties fo:margin-top="0.423cm" fo:margin-bottom="0.212cm" style:contextual-spacing="false" fo:keep-with-next="always" />
      <style:text-properties style:font-name="Arial" fo:font-size="14pt" style:font-name-asian="Microsoft YaHei" style:font-size-asian="14pt" style:font-name-complex="Mangal" style:font-size-complex="14pt" />
    </style:style>
    <style:style style:name="List" style:family="paragraph" style:parent-style-name="Text_20_body" style:class="list">
      <style:text-properties style:font-size-asian="12pt" style:font-name-complex="Mangal1" />
    </style:style>
    <style:style style:name="Caption" style:family="paragraph" style:parent-style-name="Standard" style:class="extra">
      <style:paragraph-properties fo:margin-top="0.212cm" fo:margin-bottom="0.212cm" style:contextual-spacing="false" text:number-lines="false" text:line-number="0" />
      <style:text-properties fo:font-size="12pt" fo:font-style="italic" style:font-size-asian="12pt" style:font-style-asian="italic" style:font-name-complex="Mangal1" style:font-size-complex="12pt" style:font-style-complex="italic" />
    </style:style>
    <style:style style:name="Index" style:family="paragraph" style:parent-style-name="Standard" style:class="index">
      <style:paragraph-properties text:number-lines="false" text:line-number="0" />
      <style:text-properties style:font-size-asian="12pt" style:font-name-complex="Mangal1" />
    </style:style>
    <style:style style:name="Heading_20_1" style:display-name="Heading 1" style:family="paragraph" style:parent-style-name="Heading" style:next-style-name="Text_20_body" style:default-outline-level="1" style:class="text">
      <style:text-properties fo:font-size="115%" fo:font-weight="bold" style:font-size-asian="115%" style:font-weight-asian="bold" style:font-size-complex="115%" style:font-weight-complex="bold" />
    </style:style>
    <style:style style:name="Heading_20_2" style:display-name="Heading 2" style:family="paragraph" style:parent-style-name="Heading" style:next-style-name="Text_20_body" style:default-outline-level="2" style:class="text">
      <style:text-properties fo:font-size="14pt" fo:font-style="italic" fo:font-weight="bold" style:font-size-asian="14pt" style:font-style-asian="italic" style:font-weight-asian="bold" style:font-size-complex="14pt" style:font-style-complex="italic" style:font-weight-complex="bold" />
    </style:style>
    <style:style style:name="Heading_20_3" style:display-name="Heading 3" style:family="paragraph" style:parent-style-name="Heading" style:next-style-name="Text_20_body" style:default-outline-level="3" style:class="text">
      <style:text-properties fo:font-size="14pt" fo:font-weight="bold" style:font-size-asian="14pt" style:font-weight-asian="bold" style:font-size-complex="14pt" style:font-weight-complex="bold" />
    </style:style>
    <style:style style:name="Internet_20_link" style:display-name="Internet link" style:family="text">
      <style:text-properties fo:color="#000080" fo:language="zxx" fo:country="none" style:text-underline-style="solid" style:text-underline-width="auto" style:text-underline-color="font-color" style:language-asian="zxx" style:country-asian="none" style:language-complex="zxx" style:country-complex="none" />
    </style:style>
    }
  end;

  FStyles := FStyles +
   '  <style:style style:name="Bullet_20_Symbols" style:display-name="Bullet Symbols" style:family="text">' + LineEnding +
   '    <style:text-properties style:font-name="OpenSymbol" style:font-name-asian="OpenSymbol" style:font-name-complex="OpenSymbol" />' + LineEnding +
   '  </style:style>' + LineEnding;

  FStyles := FStyles +
   '  <text:outline-style style:name="Outline">' + LineEnding +
   '    <text:outline-level-style text:level="1" style:num-format="">' + LineEnding +
   '      <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">' + LineEnding +
   '        <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="0.762cm" fo:text-indent="-0.762cm" fo:margin-left="0.762cm" />' + LineEnding +
   '      </style:list-level-properties>' + LineEnding +
   '    </text:outline-level-style>' + LineEnding +
   '    <text:outline-level-style text:level="2" style:num-format="">' + LineEnding +
   '      <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">' + LineEnding +
   '        <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="1.016cm" fo:text-indent="-1.016cm" fo:margin-left="1.016cm" />' + LineEnding +
   '      </style:list-level-properties>' + LineEnding +
   '    </text:outline-level-style>' + LineEnding +
   '    <text:outline-level-style text:level="3" style:num-format="">' + LineEnding +
   '      <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">' + LineEnding +
   '        <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="1.27cm" fo:text-indent="-1.27cm" fo:margin-left="1.27cm" />' + LineEnding +
   '      </style:list-level-properties>' + LineEnding +
   '    </text:outline-level-style>' + LineEnding +
   '    <text:outline-level-style text:level="4" style:num-format="">' + LineEnding +
   '      <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">' + LineEnding +
   '        <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="1.524cm" fo:text-indent="-1.524cm" fo:margin-left="1.524cm" />' + LineEnding +
   '      </style:list-level-properties>' + LineEnding +
   '    </text:outline-level-style>' + LineEnding +
   '    <text:outline-level-style text:level="5" style:num-format="">' + LineEnding +
   '      <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">' + LineEnding +
   '        <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="1.778cm" fo:text-indent="-1.778cm" fo:margin-left="1.778cm" />' + LineEnding +
   '      </style:list-level-properties>' + LineEnding +
   '    </text:outline-level-style>' + LineEnding +
   '    <text:outline-level-style text:level="6" style:num-format="">' + LineEnding +
   '      <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">' + LineEnding +
   '        <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="2.032cm" fo:text-indent="-2.032cm" fo:margin-left="2.032cm" />' + LineEnding +
   '      </style:list-level-properties>' + LineEnding +
   '    </text:outline-level-style>' + LineEnding +
   '    <text:outline-level-style text:level="7" style:num-format="">' + LineEnding +
   '      <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">' + LineEnding +
   '        <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="2.286cm" fo:text-indent="-2.286cm" fo:margin-left="2.286cm" />' + LineEnding +
   '      </style:list-level-properties>' + LineEnding +
   '    </text:outline-level-style>' + LineEnding +
   '    <text:outline-level-style text:level="8" style:num-format="">' + LineEnding +
   '      <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">' + LineEnding +
   '        <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="2.54cm" fo:text-indent="-2.54cm" fo:margin-left="2.54cm" />' + LineEnding +
   '      </style:list-level-properties>' + LineEnding +
   '    </text:outline-level-style>' + LineEnding +
   '    <text:outline-level-style text:level="9" style:num-format="">' + LineEnding +
   '      <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">' + LineEnding +
   '        <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="2.794cm" fo:text-indent="-2.794cm" fo:margin-left="2.794cm" />' + LineEnding +
   '      </style:list-level-properties>' + LineEnding +
   '    </text:outline-level-style>' + LineEnding +
   '    <text:outline-level-style text:level="10" style:num-format="">' + LineEnding +
   '      <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">' + LineEnding +
   '        <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="3.048cm" fo:text-indent="-3.048cm" fo:margin-left="3.048cm" />' + LineEnding +
   '      </style:list-level-properties>' + LineEnding +
   '    </text:outline-level-style>' + LineEnding +
   '  </text:outline-style>' + LineEnding;

  FStyles := FStyles +
   '  <text:notes-configuration text:note-class="footnote" style:num-format="1" text:start-value="0" text:footnotes-position="page" text:start-numbering-at="document" />' + LineEnding;
  FStyles := FStyles +
   '  <text:notes-configuration text:note-class="endnote" style:num-format="i" text:start-value="0" />' + LineEnding;
  FStyles := FStyles +
   '  <text:linenumbering-configuration text:number-lines="false" text:offset="0.499cm" style:num-format="1" text:number-position="left" text:increment="5" />' + LineEnding;
  FStyles := FStyles +
   '</office:styles>' + LineEnding;

  // ----------------------------
  // Automatic Styles
  // ----------------------------

  FStyles := FStyles +
   '<office:automatic-styles>' + LineEnding +
   FAutomaticStyles + LineEnding +
(*
   '  <style:page-layout style:name="Mpm1">' + LineEnding +
   '    <style:page-layout-properties fo:page-width="21.001cm" fo:page-height="29.7cm" style:num-format="1" style:print-orientation="portrait" fo:margin-top="2cm" fo:margin-bottom="2cm" fo:margin-left="2cm" fo:margin-right="2cm" style:writing-mode="lr-tb" style:footnote-max-height="0cm">' + LineEnding +
   '      <style:footnote-sep style:width="0.018cm" style:distance-before-sep="0.101cm" style:distance-after-sep="0.101cm" style:line-style="solid" style:adjustment="left" style:rel-width="25%" style:color="#000000" />' + LineEnding +
   '    </style:page-layout-properties>' + LineEnding +
   '    <style:header-style />' + LineEnding +
   '    <style:footer-style />' + LineEnding +
   '  </style:page-layout>' + LineEnding +
   '  <style:style style:name="List_0" style:family="paragraph" style:parent-style-name="Standard" style:list-style-name="L1">' + LineEnding +
   // <style:text-properties officeooo:rsid="00072f3e" officeooo:paragraph-rsid="00072f3e" />
   '  </style:style>' + LineEnding +
*)
   '</office:automatic-styles>' + LineEnding;

  FStyles := FStyles +
   '<office:master-styles>' + LineEnding +
   FMasterStyles + LineEnding +
(*
   '  <style:master-page style:name="Standard" style:page-layout-name="Mpm1" />' + LineEnding +
*)
   '</office:master-styles>' + LineEnding;

  FStyles := FStyles +
   '</office:document-styles>';
end;

procedure TvODTVectorialWriter.WriteDocument(AData: TvVectorialDocument);
var
  i: Integer;
  sPrefix : String;
  sAutomaticStyles : String;
  CurLevel: String;
  CurPage: TvPage;
  CurTextPage: TvTextPageSequence absolute CurPage;
  CurListStyle : TvListStyle;
begin
  // content.xml will be built up by
  //    sPrefix + sAutomaticStyles + FContent

  sPrefix :=
   XML_HEADER + LineEnding +
   '<office:document-content xmlns:office="' + SCHEMAS_XMLNS_OFFICE + '"' +
     ' xmlns:style="' + SCHEMAS_XMLNS_STYLE + '"' +
     ' xmlns:text="' + SCHEMAS_XMLNS_TEXT + '"' +
     ' xmlns:table="' + SCHEMAS_XMLNS_TABLE + '"' +
     ' xmlns:draw="' + SCHEMAS_XMLNS_DRAW + '"' +
     ' xmlns:fo="' + SCHEMAS_XMLNS_FO + '"' +
     ' xmlns:xlink="' + SCHEMAS_XMLNS_XLINK + '"' +
     ' xmlns:dc="' + SCHEMAS_XMLNS_DC + '"' +
     ' xmlns:meta="' + SCHEMAS_XMLNS_META + '"' +
     ' xmlns:number="' + SCHEMAS_XMLNS_NUMBER + '"' +
     ' xmlns:svg="' + SCHEMAS_XMLNS_SVG + '"' +
     ' xmlns:chart="' + SCHEMAS_XMLNS_CHART + '"' +
     ' xmlns:dr3D="' + SCHEMAS_XMLNS_DR3D + '"' +
     ' xmlns:math="' + SCHEMAS_XMLNS_MATH + '"' +
     ' xmlns:form="' + SCHEMAS_XMLNS_FORM + '"' +
     ' xmlns:script="' + SCHEMAS_XMLNS_SCRIPT + '"' +
     ' xmlns:ooo="' + SCHEMAS_XMLNS_OOO + '"' +
     ' xmlns:oooc="' + SCHEMAS_XMLNS_OOOC + '"' +
     ' xmlns:xforms="' + SCHEMAS_XMLNS_XFORMS + '"' +
     ' xmlns:xsi="' + SCHEMAS_XMLNS_XSI + '"' +
     ' xmlns:rpt="' + SCHEMAS_XMLNS_RPT + '"' +
     ' xmlns:of="' + SCHEMAS_XMLNS_OF + '"' +
     ' xmlns:xhtml="' + SCHEMAS_XMLNS_XHTML + '"' +
     ' xmlns:grddl="' + SCHEMAS_XMLNS_GRDDL + '"' +
     ' xmlns:officeooo="' + SCHEMAS_XMLNS_OFFICEOOO + '"' +
     ' xmlns:tableooo="' + SCHEMAS_XMLNS_TABLEOOO + '"' +
     ' xmlns:drawooo="' + SCHEMAS_XMLNS_DRAWOOO + '"' +
     ' xmlns:calcext="' + SCHEMAS_XMLNS_CALCEXT + '"' +
     ' xmlns:field="' + SCHEMAS_XMLNS_FIELD + '"' +
     ' xmlns:formx="' + SCHEMAS_XMLNS_FORMX + '"' +
     ' xmlns:css3t="' + SCHEMAS_XMLNS_CSS3T + '"' +
     ' office:version="1.2">' + LineEnding;
  sPrefix := sPrefix +
     '  <office:scripts />' + LineEnding;
  sPrefix := sPrefix +
     '  <office:font-face-decls>' + LineEnding +
     '    <style:font-face style:name="Mangal1" svg:font-family="Mangal" />' + LineEnding +
     '    <style:font-face style:name="OpenSymbol" svg:font-family="OpenSymbol" />' + LineEnding +
     '    <style:font-face style:name="Times New Roman" svg:font-family="''Times New Roman''" style:font-family-generic="roman" style:font-pitch="variable" />' + LineEnding +
     '    <style:font-face style:name="Arial" svg:font-family="Arial" style:font-family-generic="swiss" style:font-pitch="variable" />' + LineEnding +
     '    <style:font-face style:name="Mangal" svg:font-family="Mangal" style:font-family-generic="system" style:font-pitch="variable" />' + LineEnding +
     '    <style:font-face style:name="Microsoft YaHei" svg:font-family="''Microsoft YaHei''" style:font-family-generic="system" style:font-pitch="variable" />' + LineEnding +
     '    <style:font-face style:name="SimSun" svg:font-family="SimSun" style:font-family-generic="system" style:font-pitch="variable" />' + LineEnding +
     '  </office:font-face-decls>' + LineEnding;

  // Build the main content of the document
  FContent := '  <office:body>' + LineEnding;

  FContent := FContent +
     '    <office:text>' + LineEnding;

  FContent := FContent +
     '      <text:sequence-decls>' + LineEnding +
     '        <text:sequence-decl text:display-outline-level="0" text:name="Illustration" />' + LineEnding +
     '        <text:sequence-decl text:display-outline-level="0" text:name="Table" />' + LineEnding +
     '        <text:sequence-decl text:display-outline-level="0" text:name="Text" />' + LineEnding +
     '        <text:sequence-decl text:display-outline-level="0" text:name="Drawing" />' + LineEnding +
     '      </text:sequence-decls>' + LineEnding;

  FNewPageSequence := False;

  // During each WritePage (and nested calls) FContentAutomaticStyles gets built up
  for i := 0 to AData.GetPageCount()-1 do
  begin
    CurPage := AData.GetPage(i);
    if CurPage is TvTextPageSequence then
      WritePage(CurTextPage, AData);
  end;

  FContent := FContent +
     '    </office:text>' + LineEnding;
  FContent := FContent +
     '  </office:body>' + LineEnding;
  FContent := FContent +
     '</office:document-content>' + LineEnding;

  // Build up the automatic styles detailed in the content.xml
  sAutomaticStyles := sAutomaticStyles +
     '  <office:automatic-styles>' + LineEnding;

  // MJT 2013-08-24 - This is the code to cycle over the ListStyles.
  //                - This is verified working for Level 0
  //                - TvBulletList needs re-architecting to be a tree
  //                  to get deeper levels working
  //                  (see note in WriteBulletStyle)
  //                - As I understand tOpenDocument-v1.1.pdf the following list style
  //                  should work once we get nesting happening

  // TODO: Investigate if this should/could be moved into Styles.xml
  sAutomaticStyles := sAutomaticStyles + '    <text:list-style style:name="L1">' + LineEnding;
  For i := 0 To AData.GetListStyleCount-1 Do
  begin
    CurListStyle := AData.GetListStyle(i);
    CurLevel := IntToStr(CurListStyle.Level+1); // Note the +1...

    If CurListStyle.Kind=vlskBullet Then
      sAutomaticStyles := sAutomaticStyles + '      <text:list-level-style-bullet text:level="'+CurLevel+'" text:style-name="Bullet_20_Symbols" text:bullet-char="'+CurListStyle.Prefix+'">' + LineEnding +
         '        <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">' + LineEnding +
         '          <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="'+FloatToODTText(CurListStyle.MarginLeft/10)+'cm" fo:text-indent="-'+FloatToODTText(CurListStyle.HangingIndent/10)+'cm" fo:margin-left="'+FloatToODTText(CurListStyle.MarginLeft/10)+'cm" />' + LineEnding +
         '        </style:list-level-properties>' + LineEnding +
         '      </text:list-level-style-bullet>' + LineEnding;
  end;
  sAutomaticStyles := sAutomaticStyles +    '    </text:list-style>' + LineEnding;

  // Now add any Automatic Styles built during WritePage..
  sAutomaticStyles := sAutomaticStyles + FContentAutomaticStyles;

  sAutomaticStyles := sAutomaticStyles +
    '  </office:automatic-styles>' + LineEnding;

  // Now piece it all together
  FContent := sPrefix + sAutomaticStyles + FContent;
end;

procedure TvODTVectorialWriter.WritePage(ACurPage: TvTextPageSequence; AData: TvVectorialDocument);
var
  i: Integer;
  lCurEntity: TvEntity;
begin
  FNewPageSequence := True;
  for i := 0 to ACurPage.GetEntitiesCount()-1 do
  begin
    lCurEntity := ACurPage.GetEntity(i);

    if (lCurEntity is TvParagraph) then
      WriteParagraph(TvParagraph(lCurEntity), ACurPage, AData);
    if (lCurEntity is TvBulletList) then
      WriteBulletList(TvBulletList(lCurEntity), ACurPage, AData);
    if (lCurEntity is TvTable) then
      WriteTable(TvTable(lCurEntity), ACurPage, AData);
  end;
end;

procedure TvODTVectorialWriter.WriteParagraph(AEntity: TvParagraph;
  ACurPage: TvTextPageSequence; AData: TvVectorialDocument);
var
  EntityKindName, AEntityStyleName, lOutlineLevel: string;
  sAutoStyleName, sPageMasterName, sPageLayoutName : String;
  sOrientation : String;
  i: Integer;
  lCurEntity: TvEntity;
  dWidth, dHeight : Double;
begin
  lOutlineLevel := '';
  if AEntity.Style = nil then
  begin
    EntityKindName := 'p';
    AEntityStyleName := 'Standard';
  end
  else
  begin
    case AEntity.Style.GetKind() of
    vskHeading:
    begin
      EntityKindName := 'h';
      lOutlineLevel := 'text:outline-level="'+IntToStr(AEntity.Style.HeadingLevel)+'" ';
    end;
    else // vskTextBody;
      EntityKindName := 'p';
    end;

    AEntityStyleName := StyleNameToODTStyleName(AData, AEntity.Style, False);
  end;

  If FNewPageSequence Then
  begin
    // Create an automatic style in both content.xml and style.xml
    // and reference the newly created style in the text we're just
    // about to write
    // TODO: Find out how to deal with new Page Sequences with other
    //       objects at the start of the page...

    Inc(FAutomaticStyleID);
    i := AData.GetPageIndex(ACurPage);

    sAutoStyleName := AEntityStyleName+'_P' + IntToStr(FAutomaticStyleID);
    sPageMasterName := 'Page_Sequence_'+IntToStr(i+1);
    sPageLayoutName := 'MPM'+IntToStr(i+1);

    // Create an automatic style descended from AEntityStyleName
    FContentAutomaticStyles := FContentAutomaticStyles +
      '<style:style style:name="'+sAutoStyleName+'"' +
                  ' style:family="paragraph"' +
                  ' style:master-page-name="'+sPageMasterName+'"' +
                  ' style:parent-style-name="'+ AEntityStyleName +'">' +
                  LineEnding +
      '</style:style>'+ LineEnding;


    // Define the MasterStyles in Styles.xml
    // TODO: Add Header and Footer content to FMasterStyles
    FMasterStyles := FMasterStyles +
      '<style:master-page style:name="'+sPageMasterName+'" style:page-layout-name="'+sPageLayoutName+'"/>' + LineEnding;

    dWidth := ACurPage.Width;
    If dWidth=0 Then
      dWidth := AData.Width;
    If dWidth=0 Then
      dWidth := 210; // Default A4

    dHeight := ACurPage.Height;
    If dHeight=0 Then
      dHeight := AData.Height;
    If dHeight=0 Then
      dHeight := 297; // Default A4

    If dWidth>dHeight Then
      sOrientation := 'landscape'
    else
      sOrientation := 'portrait';

    // Define the page layout in Styles.xml
    // TODO: Add PAge Margins...
    FAutomaticStyles := FAutomaticStyles +
      '<style:page-layout style:name="'+sPageLayoutName+'">'+ LineEnding+
      '  <style:page-layout-properties '+
                ' fo:page-width="'+FloatToODTText(dWidth)+'mm"'+
                ' fo:page-height="'+FloatToODTText(dHeight)+'mm"'+
                ' style:print-orientation="'+sOrientation+'"'+
                ' style:num-format="1" fo:margin-top="0.7874in" fo:margin-bottom="0.7874in" fo:margin-left="0.7874in" fo:margin-right="0.7874in" style:writing-mode="lr-tb" style:footnote-max-height="0in">'+ LineEnding;

    FAutomaticStyles := FAutomaticStyles +
      '	  <style:footnote-sep style:width="0.0071in" style:distance-before-sep="0.0398in" style:distance-after-sep="0.0398in" style:line-style="solid" style:adjustment="left" style:rel-width="25%" style:color="#000000"/>'+ LineEnding+
      '	</style:page-layout-properties>'+ LineEnding+
      '  <style:header-style/>'+ LineEnding+
      '  <style:footer-style/>'+ LineEnding+
      '</style:page-layout>' + LineEnding;

    // Ensure the text is written out using the new Automatic Style
    AEntityStyleName:=sAutoStyleName;
    FNewPageSequence:=False;
  end;

  FContent := FContent +
    '    <text:'+EntityKindName+' text:style-name="'+AEntityStyleName+'" ' + lOutlineLevel +'>';

  for i := 0 to AEntity.GetEntitiesCount()-1 do
  begin
    lCurEntity := AEntity.GetEntity(i);

    if (lCurEntity is TvText) then
      WriteTextSpan(TvText(lCurEntity), AEntity, ACurPage, AData);
  end;

  FContent := FContent +
    '</text:'+EntityKindName+'>' + LineEnding;
{
      <text:h text:style-name="P2" text:outline-level="1">Laza<text:span text:style-name="T1">ru</text:span>s</text:h>
      <text:p text:style-name="P5">Lazarus is a free and open source development tool for the Free Pascal compiler, which is also free and open source.</text:p>
      <text:h text:style-name="P1" text:outline-level="2">Overview</text:h>
      <text:p text:style-name="P3">Lazarus is a free cross-platform visual integrated development environment (IDE) for rapid application development (RAD) using the Free Pascal compiler supported dialects of Object Pascal. Developers use Lazarus to create native code console and graphical user interface (GUI) applications for the desktop along with mobile devices, web applications, web services, and visual components and function libraries (.so, .dll, etc) for use by other programs for any platform the Free Pascal compiler supports( Mac, Unix, Linux, Windows, etc).</text:p>
      <text:p text:style-name="P3" />
      <text:p text:style-name="P3">Lazarus provides a highly visual development environment for the creation of rich user interfaces, application logic, and other supporting code artifacts. Along with the customary project management features, the Lazarus IDE also provides features that includes but are not limited to:</text:p>
      <text:p text:style-name="P3" />
      <text:list xml:id="list5792477270030595966" text:style-name="L1">
        <text:list-item>
          <text:p text:style-name="P4">A What You See Is What You Get (WYSIWYG) visual windows layout designer</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">An extensive set of GUI widgets or visual components such as edit boxes, buttons, dialogs, menus, etc.</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">An extensive set of non visual components for common behaviors such as persistence of application settings</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">A set of data connectivity components for MySQL, PostgresSQL, FireBird, Oracle, SQL Lite, Sybase, and others</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">Data aware widget set that allows the developer to see data in visual components in the designer to assist with development</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">Interactive code debugger</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">Code completion</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">Code templates</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">Syntax highlighting</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">Context sensitive help</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">Text resource manager for internationalization</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">Automatic code formatting</text:p>
        </text:list-item>
        <text:list-item>
          <text:p text:style-name="P4">The ability to create custom components</text:p>
        </text:list-item>
      </text:list>
      <text:p text:style-name="P3" />
      <text:p text:style-name="P3">Lazarus inherits three features from its use of the Free Pascal compiler: compile and execution speed, and cross-compilation. The Free Pascal compiler benefits from the Pascal language structure, which is rigid, and the steady advancements of Pascal compiler design, spanning several decades, to compile large applications quickly, often seconds.</text:p>
}
end;

procedure TvODTVectorialWriter.WriteTextSpan(AEntity: TvText; AParagraph: TvParagraph;
  ACurPage: TvTextPageSequence; AData: TvVectorialDocument);
var
  AEntityStyleName: string;
  lStyle: TvStyle;
  sText: String;
  i : Integer;
begin
  lStyle := AEntity.GetCombinedStyle(AParagraph);
  if lStyle = nil then
  begin
    AEntityStyleName := 'Standard';
  end
  else
  begin
    AEntityStyleName := StyleNameToODTStyleName(AData, lStyle, False);
  end;
  {
  <text:p text:style-name="P2">
    Lazaru
    <text:span text:style-name="T2">s is a fre</text:span>
    e and open sou
    <text:span text:style-name="T5">rce development tool for</text:span>
    the Free Pascal compiler, which is also free and open source.
  </text:p>
  }
  // Note that here we write only text spans!

  // MJT 2013-08-24  ODT Writer and DOCX writer were treating TvText.Value differently...
  //                This code synchronises handling between the two writers...

  sText :=  EscapeHTML(AEntity.Value.Text);

  // Trim extra CRLF appended by TStringList.Text
  If DefaultTextLineBreakStyle = tlbsCRLF Then
    sText := Copy(sText, 1, Length(sText) - 2)
  Else
    sText := Copy(sText, 1, Length(sText) - 1);


  sText := StringReplace(sText, '  ', ' <text:s/>', [rfReplaceAll]);
  sText := StringReplace(sText, #09, '<text:tab/>', [rfReplaceAll]);
  sText := StringReplace(sText, #13, '<text:line-break/>', [rfReplaceAll]);
  sText := StringReplace(sText, #10, '', [rfReplaceAll]);

  FContent := FContent + '<text:span text:style-name="'+AEntityStyleName+'">' +
    sText + '</text:span>';
end;

function TvODTVectorialWriter.BordersToString(ATableBorders, ACellBorders : TvTableBorders;
  ATopCell, ABottomCell, ALeftCell, ARightCell : Boolean):String;
Const
  LU_BORDERTYPE: Array[TvTableBorderType] Of String =
    ('solid', 'dashed', 'solid', 'none', 'default');
//  ('solid', 'dashed', 'double', 'none', 'default');
(*
  double requires a completely different configuration, so for now, we won't
  support it...

  <style:table-cell-properties style:vertical-align="middle"
                               style:border-line-width-left="0.28mm 0.28mm 0.28mm"
                               style:border-line-width-top="0.28mm 0.28mm 0.28mm"
                               fo:padding="0mm"
                               fo:border-left="2.35pt double #ff0000"
                               fo:border-right="0.5pt solid #ff0000"
                               fo:border-top="2.35pt double #ff0000"
                               fo:border-bottom="0.5pt solid #ff0000"/>

  From the OASIS Open Office Specification:
    The style:border-line-width specifies the line widths of all four sides,
    while the other attributes specify the line widths of one side only.

    The value of the attributes can be a list of three space-separated lengths,
    as follows:
      • The first value specifies the width of the inner line
      • The second value specified the distance between the two lines
      • The third value specifies the width of the outer line

    The result of specifying a border line width without specifying a border
    width style of double for the same border is undefined.
*)

  Function BorderToString(AAttrib : String; ABorder: TvTableBorder) : String;
  Begin
    Result := '';
    If ABorder.LineType<>tbtDefault Then
    Begin
      If ABorder.LineType=tbtNone Then
        Result := 'none'
      Else
      Begin
        If ABorder.Width <> 0 Then
          Result := Format('%s %smm', [Result, FloatToODTText(ABorder.Width)])
        Else
          Result := Format('%s 0.05pt', [Result]);

        Result := Format('%s %s', [Result, LU_BORDERTYPE[ABorder.LineType]]);

        Result := Format('%s #%s', [Result, FPColorToRGBHexString(ABorder.Color)]);
      end;

      Result := Format('%s="%s"', [AAttrib, Trim(Result)]);
    end;
  end;
Var
  sLeft, sRight, sTop, sBottom : String;
  sPadding : String;
Begin
(*
   OpenDocument does not support setting borders at the Table Level,
   only at the cell level.  For end user convenience, FPVectorial supports
   setting borders at the table level, but allows the end user fine control,
   if they prefer, by providing support for borders at the cell level as well.

   This means we're going to need to calculate actual border
   based on TvTable.Borders (which includes InsideHoriz and InsideVert) as
   default values, which can be overridden if specific TvTableCell.Borders
   are defined (ie LineType<>tbtDefault)

   Matters are complicated by the need to work out if we need to draw the right
   and top borders (if we always draw right borders then two lines will be visible
   on internal border, the left border from the cell to the right and the right
   border from this cell).  To deal with this, we only set the Right and Top
   borders if either the Cell.Borders specify (they overrule all), or if we're
   actually at the top or right cells (which the calling function will calculate
   for us)
*)

  sLeft := BorderToString('fo:border-left',   ACellBorders.Left);
  if sLeft='' then
  begin
    if ALeftCell then
      sLeft := BorderToString('fo:border-left',   ATableBorders.Left)
    else
      // Really need to look at cell to the left and determine if it has overriding Cell.Borders.Right :-(
      sLeft := BorderToString('fo:border-left',   ATableBorders.InsideVert);
  end;

  sRight := BorderToString('fo:border-right',   ACellBorders.Right);
  if sRight='' then
  begin
    if ARightCell then
      sRight := BorderToString('fo:border-right',   ATableBorders.Right)
    else
      sRight := 'fo:border-right="none"';
  end;

  sTop := BorderToString('fo:border-top',   ACellBorders.Top);
  if sTop='' then
  begin
    if ATopCell then
      sTop := BorderToString('fo:border-top',   ATableBorders.Top)
    else
      sTop := 'fo:border-top="none"';
  end;

  sBottom := BorderToString('fo:border-bottom',   ACellBorders.Bottom);
  if sBottom='' then
  begin
    if ABottomCell then
      sBottom := BorderToString('fo:border-bottom',   ATableBorders.Bottom)
    else
      // Really need to look at cell below, and determine if it has overriding Cell.Borders.Top :-(
      sBottom := BorderToString('fo:border-bottom',   ATableBorders.InsideHoriz);
  end;

  Result := Format('%s %s %s %s', [sLeft, sRight, sTop, sBottom]);
end;

procedure TvODTVectorialWriter.WriteTable(ATable: TvTable;
  ACurPage: TvTextPageSequence; AData: TvVectorialDocument);
  procedure AddBody(AString : String);
  begin
    FContent := FContent + '    ' + AString + LineEnding;
  end;
  procedure AddStyle(AString : String);
  begin
    FContentAutomaticStyles:=FContentAutomaticStyles + '    ' + AString + LineEnding;
  end;
Var
  iRow, iCell, iCol, k: Integer;
  oRow: TvTableRow;
  oCell: TvTableCell;
  lCurEntity: TvEntity;
  sTableName : String;
  iColCount : Integer;
  sTableStyle,
  sColStyle,
  sRowStyle,
  sCellStyle,
  sTemp, sTemp2: String;
  bInHeader: Boolean;
Const
  LU_V_ALIGN: Array[TvVerticalAlignment] Of String = ('top', 'bottom', 'middle', 'automatic');

Begin
  // TODO: Add support for TvTableBorder.Spacing
  // TODO: Add support for TvTableRow.CellSpacing
  // TODO: Add support for TvTable.CellSpacing

  if ATable.GetRowCount=0 Then
    Exit;

  // Style information stored in content.xml -> office:automatic-styles
  // Table information stored in content.xml -> office:body

  sTableName := Trim(ATable.Name);
  If sTableName='' Then
    sTableName := Format('Table_%d.%d', [AData.GetPageIndex(ACurPage)+1, ACurPage.GetEntityIndex(ATable)+1]);
  sTableStyle := sTableName;

  // Table meta properties
  AddStyle('<style:style style:name="'+sTableStyle+'" style:family="table">');
  Case ATable.PreferredWidth.Units of
    dimMillimeter: sTemp := 'style:width="'+FloatToODTText(ATable.PreferredWidth.Value)+'mm"';
    dimPoint:      sTemp := 'style:width="'+FloatToODTText(ATable.PreferredWidth.Value)+'pt"';
    dimPercent:    sTemp := 'style:rel-width="'+FloatToODTText(ATable.PreferredWidth.Value)+'%"';
  End;
  if ATable.BackgroundColor <> FPColor(0, 0, 0, 0) Then
    sTemp := sTemp + ' fo:background-color="#'+FPColorToRGBHexString(ATable.BackgroundColor)+'"';

  AddStyle('  <style:table-properties '+sTemp+' table:align="margins"/>');
  AddStyle('</style:style>');

  AddBody(Format('<table:table table:name="%s" table:style-name="%s">', [sTableName, sTableStyle]));

  // Now define any column specific properties
  If Length(ATable.ColWidths)>0 Then
    iColCount := Length(ATable.ColWidths)
  Else
    // No ColWidths defined means simple tables only (no merged cells)
    iColCount := TvTableRow(ATable.GetRow(0)).GetCellCount;

  For iCol := 0 To iColCount-1 Do
  Begin
    sColStyle := Format('%s.Col_%d', [sTableStyle, iCol+1]);

    If Length(ATable.ColWidths)>0 Then
    begin
      AddStyle('<style:style style:name="'+sColStyle+'" style:family="table-column">');
      Case ATable.ColWidthsUnits Of
        dimMillimeter: sTemp := 'style:column-width="'+FloatToODTText(ATable.ColWidths[iCol])+'mm"';
        dimPoint:      sTemp := 'style:column-width="'+FloatToODTText(ATable.ColWidths[iCol])+'pt"';
        dimPercent:    sTemp := 'style:rel-column-width="'+FloatToODTText(65535 * ATable.ColWidths[iCol] / 100)+'*"';
      End;

      AddStyle('  <style:table-column-properties '+sTemp+'/>');
      AddStyle('</style:style>');
    end;

    AddBody('  <table:table-column table:style-name="'+sColStyle+'" table:number-columns-repeated="1"/>');
  end;

  // Write out the table row by row, defining row and cell styles as we go..
  bInHeader := False;
  For iRow := 0 To ATable.GetRowCount-1 Do
  Begin
    oRow := ATable.GetRow(iRow);

    // Current Header functionality will only work
    // if header rows correctly defined...
    If (bInHeader) And not (oRow.Header) Then
    Begin
      bInHeader := False;
      // Close header rows...
      AddBody('  </table:table-header-rows>');
    end;

    If (oRow.Header) And (iRow=0) Then
    Begin
      bInHeader := True;
      // Open header rows
      AddBody('  <table:table-header-rows>');
    end;

    sTemp := '';
    sRowStyle := Format('%s.Row_%d', [sTableStyle, iRow+1]);

    if oRow.BackgroundColor <> FPColor(0, 0, 0, 0) Then
      sTemp := sTemp + ' fo:background-color="#'+FPColorToRGBHexString(oRow.BackgroundColor)+'"';

    If oRow.Height<>0 Then
      sTemp := sTemp + ' style:row-height="'+FloatToODTText(oRow.Height)+'mm"';

    if Not oRow.AllowSplitAcrossPage Then
      sTemp := sTemp + ' fo:keep-together="always"';
//    else
//      sTemp := sTemp + ' fo:keep-together="auto"';

    // Only define the style if it is required...
    If sTemp<>'' Then
    begin
      AddStyle('<style:style style:name="'+sRowStyle+'" style:family="table-row">');
      AddStyle('  <style:table-row-properties '+sTemp+'/>');
      AddStyle('</style:style>');

      AddBody('  <table:table-row table:style-name="'+sRowStyle+'">');
    end
    Else
      AddBody('  <table:table-row>');

    For iCell := 0 To oRow.GetCellCount-1 Do
    Begin
      oCell := oRow.GetCell(iCell);

      sTemp := '';
      sCellStyle := Format('%s.Cell_%dx%d', [sTableStyle, iRow + 1, iCell + 1]);

(*    // I cannot find a mechanism for setting cell width in ODT...
      If oCell.PreferredWidth.Value<>0 Then
      Begin
        Case oCell.PreferredWidth.Units Of
          dimMillimeter: sTemp := sTemp + 'style:cell-width="'+FloatToODTText(oCell.PreferredWidth)+'mm"';
          dimPoint:      sTemp := sTemp + 'style:cell-width="'+FloatToODTText(oCell.PreferredWidth)+'pt"';
          dimPercent:    sTemp := sTemp + 'style:rel-cell-width="'+FloatToODTText(65535 * oCell.PreferredWidth / 100)+'*"';
        End;
      end;
*)
      // Top is default in LibreOffice Write
      If oCell.VerticalAlignment<>vaTop Then
        sTemp := sTemp + ' style:vertical-align="'+LU_V_ALIGN[oCell.VerticalAlignment]+'"';

      if oCell.BackgroundColor <> FPColor(0, 0, 0, 0) Then
        sTemp := sTemp + ' fo:background-color="#'+FPColorToRGBHexString(oCell.BackgroundColor)+'"';

      sTemp := sTemp + ' ' + BordersToString(ATable.Borders, oCell.Borders,
                                             iRow=0, iRow=ATable.GetRowCount-1,
                                             iCell=0, iCell=oRow.GetCellCount-1);

      sTemp2 := '';

      If oCell.SpannedCols>1 Then
        sTemp2 := 'table:number-columns-spanned="'+IntToStr(oCell.SpannedCols)+'" ';

      // Only define the style if it is required...
      sTemp := Trim(sTemp);
      if sTemp<>'' Then
      begin
        AddStyle('<style:style style:name="'+sCellStyle+'" style:family="table-cell">');
        AddStyle('  <style:table-cell-properties '+sTemp+'/>');
        AddStyle('</style:style>');

        AddBody('    <table:table-cell table:style-name="'+sCellStyle+'" '+sTemp2+'office:value-type="string">');
      end
      Else
        AddBody('    <table:table-cell '+sTemp2+'office:value-type="string">');

      FContent := FContent + '          ';

      // oCell is a TvRichText descendant, so process it similarly...
      for k := 0 to oCell.GetEntitiesCount()-1 do
      begin
        lCurEntity := oCell.GetEntity(k);

        if (lCurEntity is TvParagraph) then
          WriteParagraph(TvParagraph(lCurEntity), ACurPage, AData);
        if (lCurEntity is TvBulletList) then
          WriteBulletList(TvBulletList(lCurEntity), ACurPage, AData);
        if (lCurEntity is TvTable) then
          WriteTable(TvTable(lCurEntity), ACurPage, AData);
      end;

      AddBody('    </table:table-cell>');

      // FPVectorial doesn't directly support covered (merged) cells,
      // instead they're implied by SpannedCols count > 1
      for k := 2 to oCell.SpannedCols Do
        AddBody('<table:covered-table-cell />');
    end;

    AddBody('  </table:table-row>');
  end;
  AddBody('</table:table>');
end;

procedure TvODTVectorialWriter.WriteBulletList(AEntity: TvBulletList;
  ACurPage: TvTextPageSequence; AData: TvVectorialDocument);
var
  i, j: Integer;
  lCurEntity, lCurSubEntity: TvEntity;
  lCurParagraph: TvParagraph;
begin
  // MJT 2013-08-24
  // Different levels are handled by nesting <test:list> inside parent <test:item>
  // Only way we can handle this is by treating TvBulletLists as a Tree
  // .Level then becomes a function returning the number of steps to root.
  // The code below there currently adds everything at level 0

  // See http://docs.oasis-open.org/office/v1.1/OS/OpenDocument-v1.1.pdf
  // page 75 "Example: Lists and sublists"

  FContent := FContent +
     '    <text:list  text:style-name="L1">' + LineEnding; // xml:id="list14840052221"

  for i := 0 to AEntity.GetEntitiesCount()-1 do
  begin
    lCurEntity := AEntity.GetEntity(i);

    if (lCurEntity is TvParagraph) then
    begin
      lCurParagraph := lCurEntity as TvParagraph;

      FContent := FContent +
          '      <text:list-item>' + LineEnding +
          '        <text:p>';

      for j := 0 to lCurParagraph.GetEntitiesCount()-1 do
      begin
        lCurSubEntity := lCurParagraph.GetEntity(j);

        if (lCurSubEntity is TvText) then
          WriteTextSpan(TvText(lCurSubEntity), lCurParagraph, ACurPage, AData);
      end;

      FContent := FContent +
        '</text:p>' + LineEnding +
        '      </text:list-item>' + LineEnding;
    end;
  end;

  FContent := FContent +
    '    </text:list>' + LineEnding;
end;

function TvODTVectorialWriter.WriteStylesXMLAsString: string;
begin

end;

constructor TvODTVectorialWriter.Create;
begin
  inherited Create;

  FPointSeparator := DefaultFormatSettings;
  FPointSeparator.DecimalSeparator := '.';
  FPointSeparator.ThousandSeparator := '#';// disable the thousand separator

  FAutomaticStyles := '';
  FMasterStyles := '';
end;

destructor TvODTVectorialWriter.Destroy;
begin

  inherited Destroy;
end;

procedure TvODTVectorialWriter.WriteToFile(AFileName: string;
  AData: TvVectorialDocument);
var
  FZip: TZipper;
  // Streams with the contents of files
  FSMeta, FSSettings, FSStyles, FSContent, FSMimetype: TStringStream;
  FSMetaInfManifest, FSManifestRDF: TStringStream;
begin
  { Fill the strings with the contents of the files }

  WriteMimetype();
  WriteMetaInfManifest();
  WriteManifestRDF();
  WriteMeta();
  WriteSettings();
  // Reversed order of Document and Styles to allow embedding Automatic Styles
  // built up during WriteDocument...
  WriteDocument(AData);
  WriteStyles(AData);

  { Write the data to streams }

  FSMeta := TStringStream.Create(FMeta);
  FSSettings := TStringStream.Create(FSettings);
  FSStyles := TStringStream.Create(FStyles);
  FSContent := TStringStream.Create(FContent);
  FSMimetype := TStringStream.Create(FMimetype);
  FSMetaInfManifest := TStringStream.Create(FMetaInfManifest);
  FSManifestRDF := TStringStream.Create(FManifestRDF);

  { Now compress the files }

  FZip := TZipper.Create;
  try
    FZip.FileName := AFileName;

    // MimeType must be first file, and should be uncompressed
    // TODO: CompressionLevel is not working.  Bug, or misuse?
    FZip.Entries.AddFileEntry(FSMimetype, OPENDOC_PATH_MIMETYPE).CompressionLevel:=clNone;

    FZip.Entries.AddFileEntry(FSMeta, OPENDOC_PATH_META);
    FZip.Entries.AddFileEntry(FSSettings, OPENDOC_PATH_SETTINGS);
    FZip.Entries.AddFileEntry(FSStyles, OPENDOC_PATH_STYLES);
    FZip.Entries.AddFileEntry(FSContent, OPENDOC_PATH_CONTENT);
    FZip.Entries.AddFileEntry(FSMetaInfManifest, OPENDOC_PATH_METAINF_MANIFEST);
    FZip.Entries.AddFileEntry(FSManifestRDF, OPENDOC_PATH_MANIFESTRDF);

    FZip.ZipAllFiles;
  finally
    FZip.Free;
    FSMeta.Free;
    FSSettings.Free;
    FSStyles.Free;
    FSContent.Free;
    FSMimetype.Free;
    FSMetaInfManifest.Free;
    FSManifestRDF.Free;
  end;
end;

procedure TvODTVectorialWriter.WriteToStream(AStream: TStream;
  AData: TvVectorialDocument);
begin
  // Not supported at the moment
  raise Exception.Create('TvODTVectorialWriter.WriteToStream not supported');
end;

initialization

  RegisterVectorialWriter(TvODTVectorialWriter, vfODT);

end.

