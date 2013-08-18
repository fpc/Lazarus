{

docxvectorialwriter.pas

THIS IS A UNIT CURRENTLY UNDER DEVELOPEMENT

      Current Functionality
          - Text support (inc Tabs and CRs)
          - Paragraph/Character Style Support
          - Supports Section Breaks (via PageSequences)
          - Supports Header and Footer
          - Supports Portrait/Landscape

      TODO
          - Add following to both FPVectorial AND DOCXWriter
            - Add Table Support
            - Add Image Support (From file and from Stream, in Paragraph and in Table)
            - Add simple Work Fields (NumPage, PageCount, Filename (Full, part), DatePrinted)
            - Add TOC support
            - Consider Unicode Support (eek)

Writes an OOXML (Office Open XML) document

An OOXML document is a compressed ZIP file with the following files inside:

  [Content_Types].xml          - An index of all files, defines their content format
  _rels\.rels                  - Relationships between high level documents
  word\document.xml            - this is the main document.  Conforms to WordprocessingML
  word\_rels\document.xml.rels - defines relationships to files required by document.xml (ie styles.xml)
  word\styles.xml              - The Style Library

Specifications obtained from:

http://openxmldeveloper.org/default.aspx
Office Open XML Part 4 - Markup Language Reference.docx
  - First edition, downloaded from http://www.ecma-international.org/publications/standards/Ecma-376.htm

AUTHORS: Mike Thompson, Felipe Monteiro de Carvalho

Change History
 0.1 - minimal document.xml produced
     - experimental support for styles (not working, hidden behind INCLUDE_STYLES define)
 0.2 - Refactored for new VectorialElement TvParagraph
     - Removed INCLUDE_STYLES define
     - Added support for Styles - resulting .docx contains ONLY styles defined in VectorialDocument
       Only Paragraph Style supported for now
     - Added IndentedStringList simply to prettify the resulting XML
 0.3 - Added support for Character Styles (named TextSpan Styles within FPVectorial
     - If Style.Name not defined, then create one based on index
     - Refactored PrepareTextRunStyle out of Prepare Styles.  Resulting XML is perfectly valid
       inside Document.XML and Style.xml, though FPVectorial currently does not support this
     - Closed out minor TODO items
 0.4 - realised <>'"& would break the resulting XML.  Added EscapeHTML when
       User defined text is outputted into the HTML
     - Multiple PageSequences are now handled.
     - Added support for Portrait/Landscape (reverse Width/Height if you want Landscape)
     - Added generic support for multiple files.  Link IDs and Relationships between
       Files are automatically handled.  Currently works for XML files only
       but should be easily extended for Image Support (code tagged with TODO)
     - Added support for Header and Footer.  Can be defined per PageSequence resulting
       in multiple headers and footers or just on the first PageSequence
       (in which case the rest of the document will inherit the same Header / Footer)
       Couldn't get the inline Header/Footers to work, so implemented as separate files.
     - Added handling for #11 (tab), #10 (line feed), #13 (CR) and #13#10 in Text Support
     - Significant refactoring of code, and formatted code with Jedi Code Format.
       I intend to Jedi Code Format before each Patch from here on
     - Added support for Alignment to Styles
}

Unit docxvectorialwriter;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  zipper, {NOTE: might require zipper from FPC 2.6.2+ }
  fpimage, fpcanvas,
  fpvectorial, fpvutils, lazutf8;

Type

  { TIndentedStringList }

  // Here to just to ensure the resulting xml files are pretty :-)
  TIndentedStringList = Class(TStringList)
  Private
    FIndent: String;
    FIndentSteps: String;
  Public
    Constructor Create;
    Function Add(Const S: String): Integer; Override;
    Function Add(bIndent: Boolean; Const S: String): Integer;

    Procedure IncIndent;
    Procedure DecIndent;
  End;

  TFileType = (ftContentTypes, ftRelationships, ftDocRelationships);
  TFileTypes = Set Of TFileType;

  { TFileInformation }

  TFileInformation = Class(TObject)
  Private
    FStream: TStream;
    Function GetStream: TStream;
  Public
    Index: Integer;
    ContentType: String;
    Path: String;
    FileType: String;
    MentionedIn: TFileTypes;

    XML: TIndentedStringList;  // Free'd internally;
    //Image : TFPImage;  { TODO : How are we going to handle images? }

    Constructor Create;
    Destructor Destroy; Override;

    Function ID: String;
    Function Filename: String;

    Procedure FreeStream;

    Property Stream: TStream read GetStream;
    // This creates a TSream, Call .FreeStream to free
  End;

  { TODO : Can this be tidied with Generics? }

  { TFileList }

  TFileList = Class(TObject)
  Private
    FList: TList;
    Function GetFile(AIndex: Integer): TFileInformation;
  Public
    Constructor Create;
    Destructor Destroy; Override;

    Function AddXMLFile(AContentType: String; APath: String;
      AFileType: String; AMentionedIn: TFileTypes): TFileInformation;

    Function Count: Integer;
    Property FileInformation[AIndex: Integer]: TFileInformation read GetFile; Default;
  End;

  { TvDOCXVectorialWriter }


  TvDOCXVectorialWriter = Class(TvCustomVectorialWriter)
  Private
    FData: TvVectorialDocument;

    FFiles: TFileList;

    Function PrepareContentTypes: String;
    Function PrepareRelationships: String;
    Function PrepareDocRelationships: String;

    Procedure PrepareDocument;
    Procedure PrepareStyles;
    Procedure PrepareTextRunStyle(ADoc: TIndentedStringList; AStyle: TvStyle);
    Function StyleNameToStyleID(AStyle: TvStyle): String;
  Public
    { General reading methods }
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure WriteToFile(AFileName: String; AData: TvVectorialDocument); Override;
    Procedure WriteToStream(AStream: TStream; AData: TvVectorialDocument); Override;
  End;

// Generic Helper Units
Function PointsToTwipsS(APoints: Double): String;
Function mmToTwipsS(AMillimetres: Double): String;

Implementation

Uses
  strutils, htmlelements;

Const
  XML_HEADER = '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>';

  { OOXML general XML constants }
  // Note: No leading '/'.  Add where required
  OOXML_PATH_TYPES = '[Content_Types].xml';
  OOXML_PATH_RELS_RELS = '_rels/.rels';
  OOXML_PATH_DOCUMENT_RELS = 'word/_rels/document.xml.rels';
  OOXML_PATH_DOCUMENT = 'word/document.xml';
  OOXML_PATH_STYLES = 'word/styles.xml';
  OOXML_PATH_HEADER = 'word/header%d.xml'; // Use Format(OOXML_PATH_HEADER, [Index]);
  OOXML_PATH_FOOTER = 'word/footer%d.xml'; // Use Format(OOXML_PATH_HEADER, [Index]);

  OOXML_RELS = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships';
  OOXML_TYPE_DOCUMENT = OOXML_RELS + '/officeDocument';
  OOXML_TYPE_STYLES = OOXML_RELS + '/styles';
  OOXML_TYPE_HEADER = OOXML_RELS + '/header';
  OOXML_TYPE_FOOTER = OOXML_RELS + '/footer';

  OOXML_CONTENTTYPE = 'application/vnd.openxmlformats-officedocument.wordprocessingml';
  OOXML_CONTENTTYPE_DOCUMENT = OOXML_CONTENTTYPE + '.document.main+xml';
  OOXML_CONTENTTYPE_STYLES = OOXML_CONTENTTYPE + '.styles+xml';
  OOXML_CONTENTTYPE_HEADER = OOXML_CONTENTTYPE + '.header+xml';
  OOXML_CONTENTTYPE_FOOTER = OOXML_CONTENTTYPE + '.footer+xml';

  // Shared between document.xml and each header.xml/footer.xml
  OOXML_DOCUMENT_NAMESPACE =
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ' +
    'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ';

  TAG_HEADER = 'hdr';
  TAG_FOOTER = 'ftr';

Function PointsToTwipsS(APoints: Double): String;
Begin
  // 1 Twip = 1/20 of a point
  Result := IntToStr(Round(20 * APoints));
End;

Function mmToTwipsS(AMillimetres: Double): String;
Begin
  // 1 Twip = 1 / 1440 of an inch - sigh...
  Result := IntToStr(Round(0.0393701 * 1440 * AMillimetres));
End;

{ TFileInformation }

Constructor TFileInformation.Create;
Begin
  FStream := nil;

  XML := nil;
  Path := '';
  FileType := '';
  ContentType := '';
  Index := -1;
  MentionedIn := [];
End;

Destructor TFileInformation.Destroy;
Begin
  If Assigned(XML) Then
    XML.Free;

  Inherited Destroy;
End;

Function TFileInformation.ID: String;
Begin
  Result := 'rId' + IntToStr(Index);
End;

Function TFileInformation.Filename: String;
Var
  i: Integer;
Begin
  i := RPos('/', Path);

  If i > 0 Then
    Result := Copy(Path, i + 1, Length(Path) - i)
  Else
    Result := Path;
End;

Function TFileInformation.GetStream: TStream;
Begin
  If Not Assigned(FStream) Then
  Begin
    If Assigned(XML) Then
    Begin
      FStream := TMemoryStream.Create;
      XML.SaveToStream(FStream);
      FStream.Position := 0;
    End;
  (*  { TODO : How are we going to handle images? }
    Else If Assigned(Image) Then
    Begin

    end;
  *)
  End;

  Result := FStream;
End;

Procedure TFileInformation.FreeStream;
Begin
  If Assigned(FStream) Then
  Begin
    FStream.Free;
    FStream := nil;
  End;
End;

{ TFileList }

Function TFileList.GetFile(AIndex: Integer): TFileInformation;
Begin
  Result := TFileInformation(FList[AIndex]);
End;

Constructor TFileList.Create;
Begin
  FList := TList.Create;
End;

Destructor TFileList.Destroy;
Begin
  While (FList.Count > 0) Do
  Begin
    TFileInformation(FList.Last).Free;
    FList.Delete(FList.Count - 1);
  End;

  Inherited Destroy;
End;

Function TFileList.AddXMLFile(AContentType: String; APath: String;
  AFileType: String; AMentionedIn: TFileTypes): TFileInformation;
Begin
  Result := TFileInformation.Create;
  Result.ContentType := AContentType;
  Result.FileType := AFileType;
  Result.Path := APath;
  Result.MentionedIn := AMentionedIn;
  Result.XML := TIndentedStringList.Create;

  Result.Index := FList.Count;
  FList.Add(Result);
End;

Function TFileList.Count: Integer;
Begin
  Result := FList.Count;
End;

{ TIndentedStringList }

Constructor TIndentedStringList.Create;
Begin
  FIndent := '';
  FIndentSteps := '  ';
End;

Function TIndentedStringList.Add(Const S: String): Integer;
Begin
  Result := Inherited Add(FIndent + S);
End;

Function TIndentedStringList.Add(bIndent: Boolean; Const S: String): Integer;
Begin
  If bIndent Then
    IncIndent;

  Result := Inherited Add(FIndent + S);

  If Not bIndent Then
    DecIndent;
End;

Procedure TIndentedStringList.DecIndent;
Begin
  FIndent := Copy(FIndent, 1, Length(FIndent) - Length(FIndentSteps));
End;

Procedure TIndentedStringList.IncIndent;
Begin
  FIndent := FIndent + FIndentSteps;
End;

{ TvDOCXVectorialWriter }

Constructor TvDOCXVectorialWriter.Create;
Begin
  Inherited Create;

  FFiles := TFileList.Create;
End;

Destructor TvDOCXVectorialWriter.Destroy;
Begin
  // Free's all the XML IndentedStringLists automagically
  FFiles.Free;
  FFiles := nil;

  Inherited Destroy;
End;

Function TvDOCXVectorialWriter.PrepareContentTypes: String;
Var
  i: Integer;
Begin
  Result := XML_HEADER + LineEnding +
    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">' +
    LineEnding +
    '  <Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>'
    + LineEnding + '  <Default Extension="xml" ContentType="application/xml"/>' +
    LineEnding;

  For i := 0 To FFiles.Count - 1 Do
    If ftContentTypes In FFiles[i].MentionedIn Then
      Result := Result + '  <Override PartName="/' + FFiles[i].Path +
        '" ContentType="' + FFiles[i].ContentType + '"/>' + LineEnding;

  Result := Result + '</Types>';
End;

Function TvDOCXVectorialWriter.PrepareRelationships: String;
Var
  i: Integer;
Begin
  Result := XML_HEADER + LineEnding +
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
    + LineEnding;

  For i := 0 To FFiles.Count - 1 Do
    If ftRelationships In FFiles[i].MentionedIn Then
      Result := Result + '  <Relationship Id="' + FFiles[i].ID +
        '" Type="' + FFiles[i].FileType + '" Target="/' + FFiles[i].Path +
        '"/>' + LineEnding;

  Result := Result + '</Relationships>';
End;

Function TvDOCXVectorialWriter.PrepareDocRelationships: String;
Var
  i: Integer;
Begin
  Result := XML_HEADER + LineEnding +
    '<Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">'
    + LineEnding;

  For i := 0 To FFiles.Count - 1 Do
    If ftDocRelationships In FFiles[i].MentionedIn Then
      Result := Result + '    <Relationship Id="' + FFiles[i].ID +
        '" Type="' + FFiles[i].FileType + '" Target="' + FFiles[i].Filename +
        '"/>' + LineEnding;

  Result := Result + '</Relationships>';
End;

Procedure TvDOCXVectorialWriter.PrepareDocument;
Var
  // Generally this is document.xml, may also be header.xml or footer.xml though..
  oDocXML: TIndentedStringList;

  Procedure AddParagraphProperties(AStyle: TvStyle);
  Begin
    oDocXML.Add(True, '<w:pPr>');
    oDocXML.Add('  <w:pStyle w:val="' + StyleNameToStyleID(AStyle) + '"/>');
    oDocXML.Add(False, '</w:pPr>');
  End;

  Procedure AddRunProperties(AStyle: TvStyle);
  Begin
    oDocXML.Add(True, '<w:rPr>');
    oDocXML.Add('  <w:rStyle w:val="' + StyleNameToStyleID(AStyle) + '"/>');
    oDocXML.Add(False, '</w:rPr>');
  End;

  Procedure AddTextRun(sText: String; AStyle: TvStyle);
  Var
    iLen, iStart, i: Integer;
    sTemp: String;
  Begin
    // Don't both writing null Text Runs..
    If sText <> '' Then
    Begin
      i := 1;
      iStart := 1;
      iLen := Length(sText);

      // Place all text between Tabs and CRs into individual Text Runs,
      // and render the Tabs and CRs appropriately
      While i <= iLen Do
      Begin
        If (sText[i] In [#10, #11, #13]) Or (i = iLen) Then
        Begin
          // Add the text before this point into a single Text Run
          If i > iStart Then
          Begin
            // If end of line AND end of line isn't a special char, then
            // inc(i) to ensure the math in the Copy works :-)
            If (i = iLen) And Not (sText[i] In [#10, #11, #13]) Then
              Inc(i);

            sTemp := Copy(sText, iStart, i - iStart);

            oDocXML.Add(True, '<w:r>');

            If Assigned(AStyle) Then
              AddRunProperties(AStyle);

            oDocXML.Add('  <w:t>' + EscapeHTML(sTemp) + '</w:t>');
            oDocXML.Add(False, '</w:r>');
          End;

          // Deal with the Tabs, LF and CRs appropriately
          If sText[i] = #11 Then
            oDocXML.Add('  <w:r><w:tab/></w:r>')
          Else If sText[i] In [#10, #13] Then
          Begin
            oDocXML.Add('  <w:r><w:br/></w:r>');

            // Now deal with CRLF by skipping over any trailing LF
            If (i < iLen) And (sText[i] = #13) Then
              If sText[i + 1] = #10 Then
                Inc(i);
          End;

          iStart := i + 1;
        End;

        Inc(i);
      End;
    End;
  End;

  Procedure ProcessParagraph(AParagraph: TvParagraph);
  Var
    i: Integer;
    oEntity: TvEntity;
    sTemp: String;
  Begin
    oDocXML.Add(True, '<w:p>');

    If Assigned(AParagraph.Style) Then
      AddParagraphProperties(AParagraph.Style);

    For i := 0 To AParagraph.GetEntitiesCount - 1 Do
    Begin
      oEntity := AParagraph.GetEntity(i);

      // Adding the TvText like this means each line in the StringList
      // will result in a <BR/> adding to the XML
      If oEntity Is TvText Then
      Begin
        sTemp := TvText(oEntity).Value.Text;

        // Strip out the trailing line break
        // added by TStringList.Text
        If DefaultTextLineBreakStyle = tlbsCRLF Then
          sTemp := Copy(sTemp, 1, Length(sTemp) - 2)
        Else
          sTemp := Copy(sTemp, 1, Length(sTemp) - 1);

        AddTextRun(sTemp, TvText(oEntity).Style);
      End
      Else
        { TODO : What other entities in TvParagraph do I need to process }
        Raise Exception.Create('Unsupported Entity: ' + oEntity.ClassName);
    End;

    oDocXML.Add(False, '</w:p>');
  End;

  Procedure ProcessRichText(ARichText: TvRichText);
  Var
    i: Integer;
    oEntity: TvEntity;
  Begin
    For i := 0 To ARichText.GetEntitiesCount - 1 Do
    Begin
      oEntity := ARichText.GetEntity(i);

      If oEntity Is TvParagraph Then
        ProcessParagraph(TvParagraph(oEntity))
      Else
        Raise Exception.Create('Unsupported entity ' + oEntity.ClassName);
    End;
  End;

  Procedure ProcessHeaderFooter(AElement: TvRichText; ATag: String);
  Var
    oTemp: TIndentedStringList;
    oFile: TFileInformation;
  Begin
    // If Header or Footer contains no elements, don't add them...
    If AElement.GetEntitiesCount > 0 Then
    Begin
      // Create additional XML files for each Header and Footer
      If ATag = TAG_HEADER Then
        oFile := FFiles.AddXMLFile(OOXML_CONTENTTYPE_HEADER,
          Format(OOXML_PATH_HEADER, [FFiles.Count]), OOXML_TYPE_HEADER,
          [ftContentTypes, ftDocRelationships])
      Else
        oFile := FFiles.AddXMLFile(OOXML_CONTENTTYPE_FOOTER,
          Format(OOXML_PATH_FOOTER, [FFiles.Count]), OOXML_TYPE_FOOTER,
          [ftContentTypes, ftDocRelationships]);

      // For the next few steps, all the ProcessXXX routines will be working on the header or footer...
      oTemp := oDocXML;
      oDocXML := oFile.XML;

      oDocXML.Add(XML_HEADER);
      oDocXML.Add(Format('<w:%s %s>', [ATag, OOXML_DOCUMENT_NAMESPACE +
        'xml:space="preserve"']));

      ProcessRichText(AElement);

      // Close the new xml file...
      oDocXML.Add(Format('</w:%s>', [ATag]));

      // ProcessXXX routines will now resume working on document.xml
      oDocXML := oTemp;

      { TODO : FPVectorial currently doesn't support Odd or Even Page Header/Footers }
      // Add the reference to the newly created Header or Footer into the main document
      If ATag = TAG_HEADER Then
        oDocXML.Add('<w:headerReference w:type="default" r:id="' + oFile.ID + '"/>')
      Else
        oDocXML.Add('<w:footerReference w:type="default" r:id="' + oFile.ID + '"/>');
    End;
  End;

  Procedure FinalisePage(APageSequence: TvTextPageSequence; ALastPage: Boolean);
  Var
    dWidth, dHeight: Double;
    sTemp: String;
  Begin
    // For the final pagesequence only w:sectPr shouldn't be wrapped inside w:p or w:pPr
    If Not ALastPage Then
    Begin
      oDocXML.Add(True, '<w:p>');
      oDocXML.Add(True, '<w:pPr>');
    End;

    oDocXML.Add(True, '<w:sectPr>');
    oDocXML.IncIndent;

    ProcessHeaderFooter(APageSequence.Header, TAG_HEADER);
    ProcessHeaderFooter(APageSequence.Footer, TAG_FOOTER);

    // Define the Page Layout
    dWidth := APageSequence.Width;
    If dWidth = 0 Then
      dWidth := FData.Width;

    dHeight := APageSequence.Height;
    If dHeight = 0 Then
      dHeight := FData.Height;

    If ((dWidth <> 0) And (dHeight <> 0)) Then
    Begin
      sTemp := Format('<w:pgSz w:w="%s" w:h="%s"',
        [mmToTwipsS(dWidth), mmToTwipsS(dHeight)]);

      If dWidth < dHeight Then
        sTemp := sTemp + '/>'
      Else
        sTemp := sTemp + ' w:orient="landscape"/>';

      oDocXML.Add(sTemp);
    End;

    { TODO : Ensure these other properties can be set }
    //<w:pgMar w:top="1440" w:right="1440" w:bottom="1440" w:left="1440" w:header="708" w:footer="708" w:gutter="0"/>
    //<w:cols w:space="708"/>
    //<w:docGrid w:linePitch="360"/>
    oDocXML.DecIndent;
    oDocXML.Add(False, '</w:sectPr>');

    If Not ALastPage Then
    Begin
      oDocXML.Add(False, '</w:pPr>');
      oDocXML.Add(False, '</w:p>');
    End;
  End;

Var
  oPage: TvPage;
  oPageSequence: TvTextPageSequence;
  oPageEntity: TvEntity;
  iPage, i: Integer;
  oFile: TFileInformation;
Begin
  oFile := FFiles.AddXMLFile(OOXML_CONTENTTYPE_DOCUMENT, OOXML_PATH_DOCUMENT,
    OOXML_TYPE_DOCUMENT, [ftContentTypes, ftRelationships]);
  oDocXML := oFile.XML;

  oDocXML.Clear;
  oDocXML.Add(XML_HEADER);
  oDocXML.Add(Format('<w:document %s>', [OOXML_DOCUMENT_NAMESPACE +
    'xml:space="preserve"']));

  oDocXML.Add(True, '<w:body>');

  For iPage := 0 To FData.GetPageCount - 1 Do
  Begin
    oPage := FData.GetPageAsText(iPage);

    If oPage Is TvTextPageSequence Then
    Begin
      oPageSequence := TvTextPageSequence(oPage);

      // Process the page contents
      For i := 0 To oPageSequence.GetEntitiesCount - 1 Do
      Begin
        oPageEntity := oPageSequence.GetEntity(i);

        If oPageEntity Is TvParagraph Then
          ProcessParagraph(TvParagraph(oPageEntity))
        Else If oPageEntity Is TvRichText Then
          ProcessRichText(TvRichText(oPageEntity))
        Else
          { TODO : What other entities in TvTextPageSequence do we need to process? }
          Raise Exception.Create('Unsupported Entity: ' + oPageEntity.ClassName);
      End;

      // Add any dimensions, headers, footers etc
      FinalisePage(oPageSequence, iPage = FData.GetPageCount - 1);
    End;
  End;

  oDocXML.Add(False, '</w:body>');
  oDocXML.Add('</w:document>');
End;

Function TvDOCXVectorialWriter.StyleNameToStyleID(AStyle: TvStyle): String;
Begin
  If Trim(AStyle.Name) <> '' Then
    Result := StringReplace(AStyle.Name, ' ', '', [rfReplaceAll, rfIgnoreCase])
  Else
  Begin
    Result := Format('StyleID%d', [FData.FindStyleIndex(AStyle)]);
    AStyle.Name := Result;   // Saves having to do the FindIndex later...
  End;
End;

Procedure TvDOCXVectorialWriter.PrepareTextRunStyle(ADoc: TIndentedStringList;
  AStyle: TvStyle);
Const
  BoolAsString: Array[Boolean] Of String = ('off', 'on');

Begin
  ADoc.Add(True, '<w:rPr>');
  ADoc.IncIndent;

  If (spbfFontName In AStyle.SetElements) And (AStyle.Font.Name <> '') Then
    ADoc.Add('<w:rFonts w:ascii="' + AStyle.Font.Name + '" w:hAnsi="' +
      AStyle.Font.Name + '"/>');

  If spbfFontSize In AStyle.SetElements Then
    { TODO : Where does the magic Font.Size*2 come from?  Confirm... }
    ADoc.Add('<w:sz w:val="' + IntToStr(2 * AStyle.Font.Size) + '"/>');

  If spbfFontBold In AStyle.SetElements Then
    ADoc.Add('<w:b w:val="' + BoolAsString[AStyle.Font.Bold] + '"/>');

  If spbfFontItalic In AStyle.SetElements Then
    ADoc.Add('<w:i w:val="' + BoolAsString[AStyle.Font.Italic] + '"/>');

  If spbfFontUnderline In AStyle.SetElements Then
    ADoc.Add('<w:u w:val="' + BoolAsString[AStyle.Font.Underline] + '"/>');

  If spbfFontStrikeThrough In AStyle.SetElements Then
    ADoc.Add('<w:strike w:val="' + BoolAsString[AStyle.Font.StrikeThrough] + '"/>');

  If CompareColors(AStyle.Font.Color, FPColor(0, 0, 0, 0)) <> 0 Then
    ADoc.Add('<w:color w:val="' + FPColorToRGBHexString(AStyle.Font.Color) + '"/>');

  // Can Word handled re-oriented text?  I hope not...
  If AStyle.Font.Orientation <> 0 Then;

  ADoc.DecIndent;

  // Don't bother adding an empty tag..
  If ADoc[ADoc.Count - 1] <> '<w:rPr>' Then
    ADoc.Add(False, '</w:rPr>')
  Else
  Begin
    ADoc.Delete(ADoc.Count - 1);
    ADoc.DecIndent;
  End;
End;

Procedure TvDOCXVectorialWriter.PrepareStyles;
Var
  oStyleXML: TIndentedStringList;

  Procedure PrepareParagraphStyle(AStyle: TvStyle; AType: String);
  Const
    AlignmentAsString: Array [TvStyleAlignment] Of String =
      ('left', 'right', 'both', 'center');
  Var
    sTemp: String;
  Begin
    oStyleXML.Add(True, '<w:style w:type="' + AType + '" w:styleId="' +
      StyleNameToStyleID(AStyle) + '">');

    // Add the name and inheritance values
    oStyleXML.Add('  <w:name w:val="' + AStyle.Name + '"/>');

    If Assigned(AStyle.Parent) Then
      oStyleXML.Add('  <w:basedOn w:val="' + StyleNameToStyleID(
        AStyle.Parent) + '"/> ');

    { TODO : <w:qFormat/> doesn't always need to be set, but I don't yet understand the rules...  }
    oStyleXML.Add('  <w:qFormat/> ');   // Latent Style Primary Style Setting.

    { TODO : Specification states you CANNOT redeclare a identical property
             declared in a parent. At the moment code is relying on Styles
             correctly defined up through hierarchy }
    If AType = 'paragraph' Then
    Begin
      // Add the Paragraph Properties
      oStyleXML.Add(True, '<w:pPr>');
      oStyleXML.IncIndent;

      sTemp := '';
      If AStyle.MarginTop <> 0 Then
        sTemp := sTemp + ' w:before="' + mmToTwipsS(AStyle.MarginTop) + '"';

      If AStyle.MarginBottom <> 0 Then
        sTemp := sTemp + ' w:after="' + mmToTwipsS(AStyle.MarginBottom) + '"';

      If sTemp <> '' Then
        oStyleXML.Add('<w:spacing' + sTemp + '/>');

      sTemp := '';
      If AStyle.MarginLeft <> 0 Then
        sTemp := sTemp + ' w:left="' + mmToTwipsS(AStyle.MarginLeft) + '"';

      If AStyle.MarginRight <> 0 Then
        sTemp := sTemp + ' w:right="' + mmToTwipsS(AStyle.MarginRight) + '"';

      If sTemp <> '' Then
        oStyleXML.Add('<w:ind' + sTemp + '/>');

      If spbfAlignment In AStyle.SetElements Then
        oStyleXML.Add('<w:jc w:val="' + AlignmentAsString[AStyle.Alignment] + '"/>');

      oStyleXML.DecIndent;

      If oStyleXML[oStyleXML.Count - 1] <> '<w:pPr>' Then
        oStyleXML.Add(False, '</w:pPr>')
      Else
      Begin
        oStyleXML.Delete(oStyleXML.Count - 1);
        oStyleXML.DecIndent;
      End;
    End;

    // Now add the actual formatting (rPr = Run Properties).
    PrepareTextRunStyle(oStyleXML, AStyle);

    oStyleXML.Add(False, '</w:style>  ');
  End;

Var
  i: Integer;
  oStyle: TvStyle;
  oFile: TFileInformation;
Begin
  // Only add this file if there are any styles defined...
  If FData.GetStyleCount > 0 Then
  Begin
    oFile := FFiles.AddXMLFile(OOXML_CONTENTTYPE_STYLES, OOXML_PATH_STYLES,
      OOXML_TYPE_STYLES, [ftContentTypes, ftDocRelationships]);
    oStyleXML := oFile.XML;

    oStyleXML.Clear;
    oStyleXML.Add(XML_HEADER);
    oStyleXML.Add(Format('<w:styles %s>', [OOXML_DOCUMENT_NAMESPACE]));

    For i := 0 To FData.GetStyleCount - 1 Do
    Begin
      oStyle := FData.GetStyle(i);

      If oStyle.GetKind In [vskTextBody, vskHeading] Then
        PrepareParagraphStyle(oStyle, 'paragraph')
      Else If oStyle.GetKind In [vskTextSpan] Then
        PrepareParagraphStyle(oStyle, 'character')
      Else
        { TODO : handle the other StyleKinds }
        Raise Exception.Create('Unsupported StyleKind in ' + oStyle.Name);
    End;

    oStyleXML.Add('</w:styles>');
  End;
End;

Procedure TvDOCXVectorialWriter.WriteToFile(AFileName: String;
  AData: TvVectorialDocument);
Var
  oStream: TFileStream;
Begin
  If ExtractFileExt(AFilename) = '' Then
    AFilename := AFilename + '.docx';

  oStream := TFileStream.Create(AFileName, fmCreate);
  Try
    WriteToStream(oStream, AData);
  Finally
    FreeAndNil(oStream);
  End;
End;

Procedure TvDOCXVectorialWriter.WriteToStream(AStream: TStream;
  AData: TvVectorialDocument);
Var
  oContentTypes, oRelsRels, oDocumentRels: TStringStream;
  oZip: TZipper;
  i: Integer;
Begin
  FData := AData;

  PrepareStyles;
  PrepareDocument;

  // These documents need building up with details of all included files,
  //   not worth the overhead of handling as StringLists
  oContentTypes := TStringStream.Create(PrepareContentTypes);
  oRelsRels := TStringStream.Create(PrepareRelationships);
  oDocumentRels := TStringStream.Create(PrepareDocRelationships);

  oZip := TZipper.Create;
  Try
    oZip.Entries.AddFileEntry(oContentTypes, OOXML_PATH_TYPES);
    oZip.Entries.AddFileEntry(oRelsRels, OOXML_PATH_RELS_RELS);
    oZip.Entries.AddFileEntry(oDocumentRels, OOXML_PATH_DOCUMENT_RELS);

    For i := 0 To FFiles.Count - 1 Do
      oZip.Entries.AddFileEntry(FFiles[i].Stream, FFiles[i].Path);

    oZip.SaveToStream(AStream);
  Finally
    oZip.Free;

    For i := 0 To FFiles.Count - 1 Do
      FFiles[i].FreeStream;

    oContentTypes.Free;
    oRelsRels.Free;
    oDocumentRels.Free;
  End;
End;

Initialization
  RegisterVectorialWriter(TvDOCXVectorialWriter, vfDOCX);

End.
