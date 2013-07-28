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

AUTHORS: Felipe Monteiro de Carvalho
}
unit odtvectorialwriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math,
  zipper, {NOTE: might require zipper from FPC 2.6.2+ }
  fpimage, fpcanvas, fgl,
  fpvectorial, fpvutils, lazutf8;

type
  { TvODTVectorialWriter }

  TvODTVectorialWriter = class(TvCustomVectorialWriter)
  private
    FPointSeparator: TFormatSettings;
    // Strings with the contents of files
    FMeta, FSettings, FStyles, FContent, FMimetype: string;
    FMetaInfManifest: string;
    // Routines to write those files
    procedure WriteMimetype;
    procedure WriteMetaInfManifest;
    procedure WriteMeta;
    procedure WriteSettings;
    procedure WriteStyles;
    procedure WriteDocument(AData: TvVectorialDocument);
    procedure WritePage(ACurPage: TvTextDocumentPage);
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

  { OpenDocument schemas constants }
  SCHEMAS_XMLNS_OFFICE   = 'urn:oasis:names:tc:opendocument:xmlns:office:1.0';
  SCHEMAS_XMLNS_DCTERMS  = 'http://purl.org/dc/terms/';
  SCHEMAS_XMLNS_META     = 'urn:oasis:names:tc:opendocument:xmlns:meta:1.0';
  SCHEMAS_XMLNS          = 'http://schemas.openxmlformats.org/officeDocument/2006/extended-properties';
  SCHEMAS_XMLNS_CONFIG   = 'urn:oasis:names:tc:opendocument:xmlns:config:1.0';
  SCHEMAS_XMLNS_OOO      = 'http://openoffice.org/2004/office';
  SCHEMAS_XMLNS_MANIFEST = 'urn:oasis:names:tc:opendocument:xmlns:manifest:1.0';
  SCHEMAS_XMLNS_FO       = 'urn:oasis:names:tc:opendocument:xmlns:xsl-fo-compatible:1.0';
  SCHEMAS_XMLNS_STYLE    = 'urn:oasis:names:tc:opendocument:xmlns:style:1.0';
  SCHEMAS_XMLNS_SVG      = 'urn:oasis:names:tc:opendocument:xmlns:svg-compatible:1.0';
  SCHEMAS_XMLNS_TABLE    = 'urn:oasis:names:tc:opendocument:xmlns:table:1.0';
  SCHEMAS_XMLNS_TEXT     = 'urn:oasis:names:tc:opendocument:xmlns:text:1.0';
  SCHEMAS_XMLNS_V        = 'urn:schemas-microsoft-com:vml';
  SCHEMAS_XMLNS_NUMBER   = 'urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0';
  SCHEMAS_XMLNS_CHART    = 'urn:oasis:names:tc:opendocument:xmlns:chart:1.0';
  SCHEMAS_XMLNS_DR3D     = 'urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0';
  SCHEMAS_XMLNS_MATH     = 'http://www.w3.org/1998/Math/MathML';
  SCHEMAS_XMLNS_FORM     = 'urn:oasis:names:tc:opendocument:xmlns:form:1.0';
  SCHEMAS_XMLNS_SCRIPT   = 'urn:oasis:names:tc:opendocument:xmlns:script:1.0';
  SCHEMAS_XMLNS_OOOW     = 'http://openoffice.org/2004/writer';
  SCHEMAS_XMLNS_OOOC     = 'http://openoffice.org/2004/calc';
  SCHEMAS_XMLNS_DOM      = 'http://www.w3.org/2001/xml-events';
  SCHEMAS_XMLNS_XFORMS   = 'http://www.w3.org/2002/xforms';
  SCHEMAS_XMLNS_XSD      = 'http://www.w3.org/2001/XMLSchema';
  SCHEMAS_XMLNS_XSI      = 'http://www.w3.org/2001/XMLSchema-instance';
  SCHEMAS_XMLNS_GRDDL    = 'http://www.w3.org/2003/g/data-view#';
  SCHEMAS_XMLNS_XLINK    = 'http://www.w3.org/1999/xlink';
  SCHEMAS_XMLNS_DC       = 'http://purl.org/dc/elements/1.1/';

  // SVG requires hardcoding a DPI value

  // The Opera Browser and Inkscape use 90 DPI, so we follow that

  // 1 Inch = 25.4 milimiters
  // 90 inches per pixel = (1 / 90) * 25.4 = 0.2822
  // FLOAT_MILIMETERS_PER_PIXEL = 0.3528; // DPI 72 = 1 / 72 inches per pixel

  FLOAT_MILIMETERS_PER_PIXEL = 0.2822; // DPI 90 = 1 / 90 inches per pixel
  FLOAT_PIXELS_PER_MILIMETER = 3.5433; // DPI 90 = 1 / 90 inches per pixel

procedure TvODTVectorialWriter.WriteMimetype;
begin
  FMimetype := 'application/vnd.oasis.opendocument.text';
end;

procedure TvODTVectorialWriter.WriteMetaInfManifest;
begin
  FMetaInfManifest :=
   XML_HEADER + LineEnding +
   '<manifest:manifest xmlns:manifest="' + SCHEMAS_XMLNS_MANIFEST + '" manifest:version="1.2" >' + LineEnding +
   '  <manifest:file-entry manifest:full-path="/" manifest:media-type="application/vnd.oasis.opendocument.text" />' + LineEnding + // manifest:version="1.2"
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="content.xml" />' + LineEnding +
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="styles.xml" />' + LineEnding +
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="meta.xml" />' + LineEnding +
   '  <manifest:file-entry manifest:media-type="text/xml" manifest:full-path="settings.xml" />' + LineEnding +
   '</manifest:manifest>';
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
     ' xmlns:ex="' + SCHEMAS_XMLNS + '">' + LineEnding + // office:version="1.2"
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
     ' xmlns:ooo="' + SCHEMAS_XMLNS_OOO + '">' + LineEnding + // office:version="1.2">
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

procedure TvODTVectorialWriter.WriteStyles;
begin
  FStyles :=
   XML_HEADER + LineEnding +
   '<office:document-styles xmlns:office="' + SCHEMAS_XMLNS_OFFICE + '"' +
     ' xmlns:fo="' + SCHEMAS_XMLNS_FO + '"' +
     ' xmlns:style="' + SCHEMAS_XMLNS_STYLE + '"' +
     ' xmlns:svg="' + SCHEMAS_XMLNS_SVG + '"' +
     ' xmlns:table="' + SCHEMAS_XMLNS_TABLE + '"' +
     ' xmlns:text="' + SCHEMAS_XMLNS_TEXT + '"' +
     ' xmlns:v="' + SCHEMAS_XMLNS_V + '"' +
     '>' + LineEnding +
{
xmlns:draw="urn:oasis:names:tc:opendocument:xmlns:drawing:1.0"
xmlns:xlink="http://www.w3.org/1999/xlink"
xmlns:dc="http://purl.org/dc/elements/1.1/"
xmlns:meta="urn:oasis:names:tc:opendocument:xmlns:meta:1.0"
xmlns:number="urn:oasis:names:tc:opendocument:xmlns:datastyle:1.0"
xmlns:chart="urn:oasis:names:tc:opendocument:xmlns:chart:1.0"
xmlns:dr3d="urn:oasis:names:tc:opendocument:xmlns:dr3d:1.0"
xmlns:math="http://www.w3.org/1998/Math/MathML"
xmlns:form="urn:oasis:names:tc:opendocument:xmlns:form:1.0"
xmlns:script="urn:oasis:names:tc:opendocument:xmlns:script:1.0"
xmlns:ooo="http://openoffice.org/2004/office"
xmlns:ooow="http://openoffice.org/2004/writer"
xmlns:oooc="http://openoffice.org/2004/calc"
xmlns:dom="http://www.w3.org/2001/xml-events"
xmlns:rpt="http://openoffice.org/2005/report"
xmlns:of="urn:oasis:names:tc:opendocument:xmlns:of:1.2"
xmlns:xhtml="http://www.w3.org/1999/xhtml"
xmlns:grddl="http://www.w3.org/2003/g/data-view#"
xmlns:officeooo="http://openoffice.org/2009/office"
xmlns:tableooo="http://openoffice.org/2009/table"
xmlns:drawooo="http://openoffice.org/2010/draw"
xmlns:calcext="urn:org:documentfoundation:names:experimental:calc:xmlns:calcext:1.0"
xmlns:css3t="http://www.w3.org/TR/css3-text/" office:version="1.2">}
   '<office:font-face-decls>' + LineEnding +
   '  <style:font-face style:name="Arial" svg:font-family="Arial" />' + LineEnding +
   '  <style:font-face style:name="Times New Roman" svg:font-family="Times New Roman" style:font-family-generic="roman" style:font-pitch="variable" />' + LineEnding +
   '</office:font-face-decls>' + LineEnding +

   // up to here done +/-

   '<office:styles>' + LineEnding +
   '  <style:style style:name="Default" style:family="table-cell">' + LineEnding +
   '    <style:text-properties fo:font-size="10" style:font-name="Arial" />' + LineEnding +
   '  </style:style>' + LineEnding +
   '</office:styles>' + LineEnding +
   '<office:automatic-styles>' + LineEnding +
   '  <style:page-layout style:name="pm1">' + LineEnding +
   '    <style:page-layout-properties fo:margin-top="1.25cm" fo:margin-bottom="1.25cm" fo:margin-left="1.905cm" fo:margin-right="1.905cm" />' + LineEnding +
   '    <style:header-style>' + LineEnding +
   '    <style:header-footer-properties fo:min-height="0.751cm" fo:margin-left="0cm" fo:margin-right="0cm" fo:margin-bottom="0.25cm" fo:margin-top="0cm" />' + LineEnding +
   '    </style:header-style>' + LineEnding +
   '    <style:footer-style>' + LineEnding +
   '    <style:header-footer-properties fo:min-height="0.751cm" fo:margin-left="0cm" fo:margin-right="0cm" fo:margin-top="0.25cm" fo:margin-bottom="0cm" />' + LineEnding +
   '    </style:footer-style>' + LineEnding +
   '  </style:page-layout>' + LineEnding +
   '</office:automatic-styles>' + LineEnding +
   '<office:master-styles>' + LineEnding +
   '  <style:master-page style:name="Default" style:page-layout-name="pm1">' + LineEnding +
   '    <style:header />' + LineEnding +
   '    <style:header-left style:display="false" />' + LineEnding +
   '    <style:footer />' + LineEnding +
   '    <style:footer-left style:display="false" />' + LineEnding +
   '  </style:master-page>' + LineEnding +
   '</office:master-styles>' + LineEnding +
   '</office:document-styles>';
{
  <office:styles>
    <style:default-style style:family="graphic">
      <style:graphic-properties svg:stroke-color="#3465af" draw:fill-color="#729fcf" fo:wrap-option="no-wrap" draw:shadow-offset-x="0.3cm" draw:shadow-offset-y="0.3cm" draw:start-line-spacing-horizontal="0.283cm" draw:start-line-spacing-vertical="0.283cm" draw:end-line-spacing-horizontal="0.283cm" draw:end-line-spacing-vertical="0.283cm" style:flow-with-text="false" />
      <style:paragraph-properties style:text-autospace="ideograph-alpha" style:line-break="strict" style:writing-mode="lr-tb" style:font-independent-line-spacing="false">
        <style:tab-stops />
      </style:paragraph-properties>
      <style:text-properties style:use-window-font-color="true" fo:font-size="12pt" fo:language="fi" fo:country="FI" style:letter-kerning="true" style:font-size-asian="10.5pt" style:language-asian="zh" style:country-asian="CN" style:font-size-complex="12pt" style:language-complex="hi" style:country-complex="IN" />
    </style:default-style>
    <style:default-style style:family="paragraph">
      <style:paragraph-properties fo:hyphenation-ladder-count="no-limit" style:text-autospace="ideograph-alpha" style:punctuation-wrap="hanging" style:line-break="strict" style:tab-stop-distance="1.251cm" style:writing-mode="page" />
      <style:text-properties style:use-window-font-color="true" style:font-name="Times New Roman" fo:font-size="12pt" fo:language="fi" fo:country="FI" style:letter-kerning="true" style:font-name-asian="SimSun" style:font-size-asian="10.5pt" style:language-asian="zh" style:country-asian="CN" style:font-name-complex="Mangal" style:font-size-complex="12pt" style:language-complex="hi" style:country-complex="IN" fo:hyphenate="false" fo:hyphenation-remain-char-count="2" fo:hyphenation-push-char-count="2" />
    </style:default-style>
    <style:default-style style:family="table">
      <style:table-properties table:border-model="collapsing" />
    </style:default-style>
    <style:default-style style:family="table-row">
      <style:table-row-properties fo:keep-together="auto" />
    </style:default-style>
    <style:style style:name="Standard" style:family="paragraph" style:class="text" />
    <style:style style:name="Heading" style:family="paragraph" style:parent-style-name="Standard" style:next-style-name="Text_20_body" style:class="text">
      <style:paragraph-properties fo:margin-top="0.423cm" fo:margin-bottom="0.212cm" style:contextual-spacing="false" fo:keep-with-next="always" />
      <style:text-properties style:font-name="Arial" fo:font-size="14pt" style:font-name-asian="Microsoft YaHei" style:font-size-asian="14pt" style:font-name-complex="Mangal" style:font-size-complex="14pt" />
    </style:style>
    <style:style style:name="Text_20_body" style:display-name="Text body" style:family="paragraph" style:parent-style-name="Standard" style:class="text">
      <style:paragraph-properties fo:margin-top="0cm" fo:margin-bottom="0.212cm" style:contextual-spacing="false" />
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
    <style:style style:name="Bullet_20_Symbols" style:display-name="Bullet Symbols" style:family="text">
      <style:text-properties style:font-name="OpenSymbol" style:font-name-asian="OpenSymbol" style:font-name-complex="OpenSymbol" />
    </style:style>
    <text:outline-style style:name="Outline">
      <text:outline-level-style text:level="1" style:num-format="">
        <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">
          <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="0.762cm" fo:text-indent="-0.762cm" fo:margin-left="0.762cm" />
        </style:list-level-properties>
      </text:outline-level-style>
      <text:outline-level-style text:level="2" style:num-format="">
        <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">
          <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="1.016cm" fo:text-indent="-1.016cm" fo:margin-left="1.016cm" />
        </style:list-level-properties>
      </text:outline-level-style>
      <text:outline-level-style text:level="3" style:num-format="">
        <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">
          <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="1.27cm" fo:text-indent="-1.27cm" fo:margin-left="1.27cm" />
        </style:list-level-properties>
      </text:outline-level-style>
      <text:outline-level-style text:level="4" style:num-format="">
        <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">
          <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="1.524cm" fo:text-indent="-1.524cm" fo:margin-left="1.524cm" />
        </style:list-level-properties>
      </text:outline-level-style>
      <text:outline-level-style text:level="5" style:num-format="">
        <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">
          <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="1.778cm" fo:text-indent="-1.778cm" fo:margin-left="1.778cm" />
        </style:list-level-properties>
      </text:outline-level-style>
      <text:outline-level-style text:level="6" style:num-format="">
        <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">
          <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="2.032cm" fo:text-indent="-2.032cm" fo:margin-left="2.032cm" />
        </style:list-level-properties>
      </text:outline-level-style>
      <text:outline-level-style text:level="7" style:num-format="">
        <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">
          <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="2.286cm" fo:text-indent="-2.286cm" fo:margin-left="2.286cm" />
        </style:list-level-properties>
      </text:outline-level-style>
      <text:outline-level-style text:level="8" style:num-format="">
        <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">
          <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="2.54cm" fo:text-indent="-2.54cm" fo:margin-left="2.54cm" />
        </style:list-level-properties>
      </text:outline-level-style>
      <text:outline-level-style text:level="9" style:num-format="">
        <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">
          <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="2.794cm" fo:text-indent="-2.794cm" fo:margin-left="2.794cm" />
        </style:list-level-properties>
      </text:outline-level-style>
      <text:outline-level-style text:level="10" style:num-format="">
        <style:list-level-properties text:list-level-position-and-space-mode="label-alignment">
          <style:list-level-label-alignment text:label-followed-by="listtab" text:list-tab-stop-position="3.048cm" fo:text-indent="-3.048cm" fo:margin-left="3.048cm" />
        </style:list-level-properties>
      </text:outline-level-style>
    </text:outline-style>
    <text:notes-configuration text:note-class="footnote" style:num-format="1" text:start-value="0" text:footnotes-position="page" text:start-numbering-at="document" />
    <text:notes-configuration text:note-class="endnote" style:num-format="i" text:start-value="0" />
    <text:linenumbering-configuration text:number-lines="false" text:offset="0.499cm" style:num-format="1" text:number-position="left" text:increment="5" />
  </office:styles>
  <office:automatic-styles>
    <style:page-layout style:name="Mpm1">
      <style:page-layout-properties fo:page-width="21.001cm" fo:page-height="29.7cm" style:num-format="1" style:print-orientation="portrait" fo:margin-top="2cm" fo:margin-bottom="2cm" fo:margin-left="2cm" fo:margin-right="2cm" style:writing-mode="lr-tb" style:footnote-max-height="0cm">
        <style:footnote-sep style:width="0.018cm" style:distance-before-sep="0.101cm" style:distance-after-sep="0.101cm" style:line-style="solid" style:adjustment="left" style:rel-width="25%" style:color="#000000" />
      </style:page-layout-properties>
      <style:header-style />
      <style:footer-style />
    </style:page-layout>
  </office:automatic-styles>
  <office:master-styles>
    <style:master-page style:name="Standard" style:page-layout-name="Mpm1" />
  </office:master-styles>
</office:document-styles>
}
end;

procedure TvODTVectorialWriter.WriteDocument(AData: TvVectorialDocument);
begin

end;

procedure TvODTVectorialWriter.WritePage(ACurPage: TvTextDocumentPage);
begin

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
  FSMetaInfManifest: TStringStream;
begin
  { Fill the strings with the contents of the files }

  WriteMimetype();
  WriteMetaInfManifest();
  WriteMeta();
  WriteSettings();
  WriteStyles();
  WriteDocument(AData);

  { Write the data to streams }

  FSMeta := TStringStream.Create(FMeta);
  FSSettings := TStringStream.Create(FSettings);
  FSStyles := TStringStream.Create(FStyles);
  FSContent := TStringStream.Create(FContent);
  FSMimetype := TStringStream.Create(FMimetype);
  FSMetaInfManifest := TStringStream.Create(FMetaInfManifest);

  { Now compress the files }

  FZip := TZipper.Create;
  try
    FZip.FileName := AFileName;

    FZip.Entries.AddFileEntry(FSMeta, OPENDOC_PATH_META);
    FZip.Entries.AddFileEntry(FSSettings, OPENDOC_PATH_SETTINGS);
    FZip.Entries.AddFileEntry(FSStyles, OPENDOC_PATH_STYLES);
    FZip.Entries.AddFileEntry(FSContent, OPENDOC_PATH_CONTENT);
    FZip.Entries.AddFileEntry(FSMimetype, OPENDOC_PATH_MIMETYPE);
    FZip.Entries.AddFileEntry(FSMetaInfManifest, OPENDOC_PATH_METAINF_MANIFEST);

    FZip.ZipAllFiles;
  finally
    FZip.Free;
    FSMeta.Free;
    FSSettings.Free;
    FSStyles.Free;
    FSContent.Free;
    FSMimetype.Free;
    FSMetaInfManifest.Free;
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

