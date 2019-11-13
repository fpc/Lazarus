{

docxvectorialwriter.pas

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

THIS IS A UNIT CURRENTLY UNDER DEVELOPEMENT

      Current Functionality
          - Text support (inc Tabs and CRs)
          - Paragraph/Character Style Support
          - Supports Section Breaks (via PageSequences)
          - Supports Header and Footer
          - Supports Portrait/Landscape
          - Support Tables

      TODO
          - Add following to both FPVectorial AND DOCXWriter
            - Add Image Support (From file and from Stream, in Paragraph and in Table)
            - Add simple Work Fields (NumPage, PageCount, Filename (Full, part), DatePrinted)
            - Add TOC support
            - Consider Unicode Support (eek)

Writes an OOXML (Office Open XML) document

An OOXML document is a compressed ZIP file with the following files inside:

  [Content_Types].xml          - An index of all files, defines their content format
  _rels\.rels                  - Relationships between high level documents
  word\_rels\document.xml.rels - defines relationships to files required by document.xml (ie styles.xml)
  word\document.xml            - this is the main document.  Conforms to WordprocessingML
  word\styles.xml              - The Style Library
  word\header%d.xml            - One file per header
  word\footer%d.xml            - One file per footer
  word\numbering.xml           - Header and List numbering details
  media\*.[png, jpg, etc]      - Images

Specifications and examples obtained from:

http://openxmldeveloper.org/default.aspx
http://officeopenxml.com/
http://www.ecma-international.org/publications/standards/Ecma-376.htm
            - Office Open XML Part 4 - Markup Language Reference.docx (First edition)

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
 0.5 - Support TvParagraph margins
     - Added numbering.xml
 0.6 - Changed #11 (vert tab) to #09 (horiz tab, what it always should have been)
     - Added Table Support
     - Bug fix - Margin support in Styles fixed...
 0.7 - Added experimental LocalAlignment support to TvParagraph


}

Unit docxvectorialwriter;

{$mode objfpc}{$H+}

Interface

Uses
  Classes, SysUtils,
  zipper, {NOTE: might require zipper from FPC 2.6.2+ }
  fpimage, fpcanvas,
  fpvectorial, fpvutils, lazutf8, Math;

Type
  TIndentOption = (indInc, indDec, indNone);

  { TIndentedStringList }

  // Here to just to ensure the resulting xml files are pretty :-)
  { TODO : Replace this with a genuine XML handler }
  TIndentedStringList = Class(TStringList)
  Private
    FIndent: String;
    FIndentSteps: String;
  Public
    Constructor Create;
    Function Add(indBefore: TIndentOption = indNone; S: String = '';
      indAfter: TIndentOption = indNone): Integer;
    Function Add(Const S: String): Integer; Override;
    Function Add(Const S: String; indAfter: TIndentOption): Integer;

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
    //Image : TFPImage;  { TODO: How are we going to handle images? }

    Constructor Create;
    Destructor Destroy; Override;

    Function ID: String;
    Function Filename: String;

    Procedure FreeStream;

    Property Stream: TStream read GetStream;
    // This creates a TSream, Call .FreeStream to free
  End;

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
    Procedure PrepareStyles;    // Only created if FData has Styles defined
    Procedure PrepareNumbering; // Only created if Numbered Styles exist

    Procedure PrepareTextRunStyle(ADoc: TIndentedStringList; AStyle: TvStyle);
    Function StyleNameToStyleID(AStyle: TvStyle): String;
  Public
    { General reading methods }
    Constructor Create; Override;
    Destructor Destroy; Override;
    Procedure WriteToFile(AFileName: String; AData: TvVectorialDocument); Override;
    Procedure WriteToStream(AStream: TStream; AData: TvVectorialDocument); Override;
  End;

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
  OOXML_PATH_NUMBERING = 'word/numbering.xml';

  OOXML_RELS = 'http://schemas.openxmlformats.org/officeDocument/2006/relationships';
  OOXML_TYPE_DOCUMENT = OOXML_RELS + '/officeDocument';
  OOXML_TYPE_STYLES = OOXML_RELS + '/styles';
  OOXML_TYPE_HEADER = OOXML_RELS + '/header';
  OOXML_TYPE_FOOTER = OOXML_RELS + '/footer';
  OOXML_TYPE_NUMBERING = OOXML_RELS + '/numbering';

  OOXML_CONTENTTYPE = 'application/vnd.openxmlformats-officedocument.wordprocessingml';
  OOXML_CONTENTTYPE_DOCUMENT = OOXML_CONTENTTYPE + '.document.main+xml';
  OOXML_CONTENTTYPE_STYLES = OOXML_CONTENTTYPE + '.styles+xml';
  OOXML_CONTENTTYPE_HEADER = OOXML_CONTENTTYPE + '.header+xml';
  OOXML_CONTENTTYPE_FOOTER = OOXML_CONTENTTYPE + '.footer+xml';
  OOXML_CONTENTTYPE_NUMBERING = OOXML_CONTENTTYPE + '.numbering+xml';

  // Shared between document.xml and each header.xml/footer.xml
  OOXML_DOCUMENT_NAMESPACE =
    'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ' +
    'xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ';

  TAG_HEADER = 'hdr';
  TAG_FOOTER = 'ftr';

  // Lookups...
  LU_ALIGN: Array [TvStyleAlignment] Of String =
    ('left', 'right', 'both', 'center');

  LU_NUMBERFORMAT: Array [TvNumberFormat] Of String =
    ('decimal', 'lowerLetter', 'lowerRoman', 'upperLetter', 'upperRoman');

  LU_NUMBERFORMATFORUMLA: Array [TvNumberFormat] Of String =
    ('Arabic', 'alphabetic', 'roman', 'ALPHABETIC', 'Roman');

  LU_ON_OFF: Array[Boolean] Of String = ('off', 'on');

  LU_BORDERTYPE: Array[TvTableBorderType] Of String =
    ('single', 'dashed', 'double', 'none', 'default');

  LU_V_ALIGN: Array[TvVerticalAlignment] Of String = ('top', 'bottom', 'center', 'both');

//ONE_POINT_IN_MM = 0.35278;

// Generic Helper Units

Function PointsToTwipsS(APoints: Double): String;
Begin
  // 1 Twip = 1/20 of a point
  Result := IntToStr(Round(20 * APoints));
End;

Function mmToPointS(AMillimetres: Double): String;
Begin
  Result := IntToStr(Round((0.0393701 * 1440 * AMillimetres) / 20));
End;

Function mmToTwipsS(AMillimetres: Double): String;
Begin
  // 1 Twip = 1 / 1440 of an inch - sigh...
  Result := IntToStr(Round(0.0393701 * 1440 * AMillimetres));
End;

Function DimAttribs(ADimension: TvDimension; AValueTag: String = 'w:w';
  ATypeTag: String = 'w:type'): String;
Var
  iValue: Integer;
  sType: String;
Begin
  Case ADimension.Units Of
    dimMillimeter:
    Begin
      iValue := Round(0.0393701 * 1440 * ADimension.Value);
      sType := 'dxa';  // most values in docx must be in twips
    End;
    dimPercent:
    Begin
      iValue := Round(50 * ADimension.Value);
      sType := 'pct';  // 50ths of a percent
    End;
    dimPoint:
    Begin
      iValue := Round(20 * ADimension.Value);
      sType := 'dxa';  // most values in docx must be in twips
    End;
  End;

  Result := Format(' %s="%d" %s="%s" ', [AValueTag, iValue, ATypeTag, sType]);
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

Function TIndentedStringList.Add(indBefore: TIndentOption; S: String;
  indAfter: TIndentOption): Integer;
Begin
  If indBefore = indInc Then
    IncIndent
  Else If indAfter = indDec Then
    DecIndent;

  Result := Inherited Add(FIndent + S);

  If indAfter = indInc Then
    IncIndent
  Else If indBefore = indDec Then
    DecIndent;
End;

Function TIndentedStringList.Add(Const S: String): Integer;
Begin
  Result := Inherited Add(FIndent + S);
End;

Function TIndentedStringList.Add(Const S: String; indAfter: TIndentOption): Integer;
Begin
  Result := Add(indNone, S, indAfter);
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
  iPage: Integer;

  Procedure ProcessRichText(ARichText: TvRichText); Forward;

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
        If (sText[i] In [#10, #09, #13]) Or (i = iLen) Then
        Begin
          // Add the text before this point into a single Text Run
          If i >= iStart Then
          Begin
            // If end of line AND end of line isn't a special char, then
            // inc(i) to ensure the math in the Copy works :-)
            If (i = iLen) And Not (sText[i] In [#10, #09, #13]) Then
              Inc(i);

            sTemp := Copy(sText, iStart, i - iStart);

            oDocXML.Add(indInc, '<w:r>');

            If Assigned(AStyle) Then
            Begin
              oDocXML.Add(indInc, '<w:rPr>');
              oDocXML.Add('  <w:rStyle w:val="' + StyleNameToStyleID(AStyle) + '"/>');
              oDocXML.Add(indDec, '</w:rPr>');
            End;

            oDocXML.Add('  <w:t>' + EscapeHTML(sTemp) + '</w:t>');
            oDocXML.Add(indDec, '</w:r>');
          End;

          // Deal with the Tabs, LF and CRs appropriately
          If sText[i] = #09 Then
            oDocXML.Add('  <w:r><w:tab/></w:r>')
          Else If sText[i] In [#10, #11, #13] Then
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

  Procedure AddField(AField : TvField);
  var
    sInstruction: String;
    sDefault: String;
  Begin
    sInstruction := '';
    sDefault := '';

    Case AField.Kind of
      vfkNumPages:
      Begin
        sInstruction := ' NUMPAGES  \* '+LU_NUMBERFORMATFORUMLA[AField.NumberFormat]+'  \* MERGEFORMAT ';
        sDefault := IntToStr(FData.GetPageCount);
       End;
      vfkPage:
      Begin
        sInstruction := ' PAGE  \* '+LU_NUMBERFORMATFORUMLA[AField.NumberFormat]+'  \* MERGEFORMAT ';
        sDefault := IntToStr(iPage+1);
      End;
      vfkAuthor:
      Begin
        sInstruction := ' AUTHOR  \* Caps  \* MERGEFORMAT ';
        sDefault := 'FPVECTORIAL';
      End;
      vfkDateCreated:
      Begin
        sInstruction := ' CREATEDATE  \@ "'+AField.DateFormat+'"  \* MERGEFORMAT ';
        sDefault := DateToStr(Now);
      End;
      vfkDate:
      Begin
        sInstruction := ' DATE  \@ "'+AField.DateFormat+'"  \* MERGEFORMAT ';
        sDefault := DateToStr(Now);
      End;
    end;

    If sInstruction<>'' Then
    Begin
      If Assigned(AField.Style) Then
      Begin
        oDocXML.Add(indInc, '<w:rPr>');
        oDocXML.Add('  <w:rStyle w:val="' + StyleNameToStyleID(AField.Style) + '"/>');
        oDocXML.Add(indDec, '</w:rPr>');
      End;

      // Start the Formula
      oDocXML.Add('<w:r><w:fldChar w:fldCharType="begin"/></w:r>');

      // Add the Instruction
      oDocXML.Add('<w:r><w:instrText xml:space="preserve">'+
                          sInstruction+
                          '</w:instrText></w:r>');

      // SEPARATE the Field (above) from the result (below)
      oDocXML.Add('<w:r><w:fldChar w:fldCharType="separate"/></w:r>');

      // Add the default text
      oDocXML.Add('<w:r><w:t>'+sDefault+'</w:t></w:r>');

      // End the Forumla
      oDocXML.Add('<w:r><w:fldChar w:fldCharType="end"/></w:r>');
    end;
  end;

  Procedure ProcessParagraph(AParagraph: TvParagraph; AListLevel : integer = -1; ANumID : Integer = -1);
  Var
    i: Integer;
    oEntity: TvEntity;
    sTemp: String;
  Begin
    oDocXML.Add(indInc, '<w:p>');

    // Add the Paragraph Properties
    oDocXML.Add(indInc, '<w:pPr>', indInc);

    If Assigned(AParagraph.Style) Then
      oDocXML.Add(Format('<w:pStyle w:val="%s"/>',
        [StyleNameToStyleID(AParagraph.Style)]));

    If (AListLevel<>-1) Then
    Begin
      oDocXML.Add('<w:numPr>');
      oDocXML.Add(indInc, Format('<w:ilvl w:val="%d"/>', [AListLevel]));
      oDocXML.Add(indDec, Format('<w:numId w:val="%d"/>', [ANumID]));
      oDocXML.Add('</w:numPr>');
    End;

    oDocXML.Add(indDec, '</w:pPr>', indDec);

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
      Else If oEntity is TvField Then
        AddField(TvField(oEntity))
      Else
        { TODO : What other entities in TvParagraph do I need to process }
        Raise Exception.Create('Unsupported Entity: ' + oEntity.ClassName);
    End;

    oDocXML.Add(indDec, '</w:p>');
  End;

  Procedure ProcessList(AList: TvList);
  Var
    i: Integer;
    oEntity: TvEntity;
  Begin
    For i := 0 To AList.GetEntitiesCount - 1 Do
    Begin
      oEntity := AList.GetEntity(i);

      If oEntity Is TvParagraph Then
      Begin
        If Not Assigned(TvParagraph(oEntity).Style) Then
          TvParagraph(oEntity).Style := AList.Style;

        ProcessParagraph(TvParagraph(oEntity), AList.GetLevel(), FData.FindListStyleIndex(AList.ListStyle) + 1);
      End
      Else If oEntity Is TvList Then
        ProcessList(TvList(oEntity))
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
      oDocXML.Add(indInc, '<w:p>');
      oDocXML.Add(indInc, '<w:pPr>');
    End;

    oDocXML.Add(indInc, '<w:sectPr>', indInc);

    ProcessHeaderFooter(APageSequence.Header, TAG_HEADER);
    ProcessHeaderFooter(APageSequence.Footer, TAG_FOOTER);

    // Define the Page Layout
    dWidth := APageSequence.Width;
    If dWidth = 0 Then
      dWidth := FData.Width;
    If dWidth=0 Then
      dWidth := 210; // Default A4

    dHeight := APageSequence.Height;
    If dHeight = 0 Then
      dHeight := FData.Height;
    If dHeight=0 Then
      dHeight := 297; // Default A4

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
    oDocXML.Add(indDec, '</w:sectPr>', indDec);

    If Not ALastPage Then
    Begin
      oDocXML.Add(indDec, '</w:pPr>');
      oDocXML.Add(indDec, '</w:p>');
    End;
  End;

  Procedure AddTableBorderProperty(ATag: String; ABorder: TvTableBorder);
  Var
    sAttrib: String;
  Begin
    If ABorder.LineType <> tbtDefault Then
    Begin
      sAttrib := '';

      If ABorder.LineType <> tbtNone Then
        sAttrib := sAttrib + 'w:val="' + LU_BORDERTYPE[ABorder.LineType] + '" ';

      sAttrib := sAttrib + 'w:space="' + mmToPointS(ABorder.Spacing) + '" ';

      If ABorder.Width <> 0 Then
        // Eights of a Point??  Really, they're just making this up...
        sAttrib := sAttrib + 'w:sz="' + mmToPointS(Min(96, Max(2, 8 * ABorder.Width))) + '" '
      Else
        // 4 is the minimum
        sAttrib := sAttrib + 'w:sz="4" ';

      If ABorder.Color <> FPColor(0, 0, 0, 0) Then
        sAttrib := sAttrib + 'w:color="' + FPColorToRGBHexString(ABorder.Color) + '" '
      Else
        sAttrib := sAttrib + 'w:color="auto" ';

      oDocXML.Add(Format('<%s %s/>', [ATag, Trim(sAttrib)]));
    End;
  End;

  Procedure ProcessTable(ATable: TvTable);
  Var
    i, j, k: Integer;
    oRow: TvTableRow;
    oCell: TvTableCell;
    //sTemp : String;
  Begin
    oDocXML.Add(indInc, '<w:tbl>');

    // Add the table properties
    oDocXML.Add(indInc, '<w:tblPr>', indInc);

    If ATable.PreferredWidth.Value <> 0 Then
      oDocXML.Add(Format('<w:tblW %s />', [DimAttribs(ATable.PreferredWidth)]));

    oDocXML.Add(indNone, '<w:tblBorders>', indInc);

    AddTableBorderProperty('w:left', ATable.Borders.Left);
    AddTableBorderProperty('w:right', ATable.Borders.Right);
    AddTableBorderProperty('w:top', ATable.Borders.Top);
    AddTableBorderProperty('w:bottom', ATable.Borders.Bottom);
    AddTableBorderProperty('w:insideH', ATable.Borders.InsideHoriz);
    AddTableBorderProperty('w:insideV', ATable.Borders.InsideVert);

    oDocXML.Add(indNone, '</w:tblBorders>', indDec);

    If Assigned(ATable.Style) Then
      oDocXML.Add('<w:tblStyle w:val="' + StyleNameToStyleID(ATable.Style) + '" />');

    If ATable.SpacingBetweenCells <> 0 Then
      oDocXML.Add('<w:tblCellSpacing w:w="' + mmToTwipsS(ATable.SpacingBetweenCells) +
        '" w:type="dxa" />');

    If ATable.BackgroundColor <> FPColor(0, 0, 0, 0) Then
      oDocXML.Add(Format('<w:shd w:val="clear" w:color="auto" w:fill="%s"/>',
        [FPColorToRGBHexString(ATable.BackgroundColor)]));

    oDocXML.Add(indDec, '</w:tblPr>', indDec);

    // Define the grid.  Grid is used to determine cell widths
    // and boundaries
    oDocXML.Add(indInc, '<w:tblGrid>', indInc);

    // Percent cannot be set here, only absolutes
    If ATable.ColWidthsUnits = dimMillimeter Then
      For k := Low(ATable.ColWidths) To High(ATable.ColWidths) Do
        oDocXML.Add(Format('<w:gridCol w:w="%s" />', [mmToTwipsS(ATable.ColWidths[k])]))
    Else If ATable.ColWidthsUnits = dimPoint Then
      For k := Low(ATable.ColWidths) To High(ATable.ColWidths) Do
        oDocXML.Add(Format('<w:gridCol w:w="%s" />',
          [IntToStr(Round(20 * ATable.ColWidths[k]))]));

    oDocXML.Add(indDec, '</w:tblGrid>', indDec);

    For i := 0 To ATable.GetRowCount - 1 Do
    Begin
      oRow := ATable.GetRow(i);
      oDocXML.Add(indInc, '<w:tr>');

      // Add the Row Properties
      oDocXML.Add(indInc, '<w:trPr>', indInc);

      If oRow.Header Then
        oDocXML.Add('<w:tblHeader />');

      If Not oRow.AllowSplitAcrossPage Then
        oDocXML.Add('<w:cantSplit />');

      If oRow.CellSpacing <> 0 Then
        oDocXML.Add('<w:tblCellSpacing w:w="' + mmToTwipsS(oRow.CellSpacing) +
          '" w:type="dxa" />');

      { TODO : w:hRule="exact", "auto" }
      If oRow.Height <> 0 Then
        oDocXML.Add('<w:trHeight w:val="' + mmToTwipsS(oRow.Height) +
          '" w:hRule="atLeast"/>');

      // Row Background Colour can't be applied here, have to apply to each cell in turn...

      oDocXML.Add(indDec, '</w:trPr>', indDec);

      For j := 0 To oRow.GetCellCount - 1 Do
      Begin
        oCell := oRow.GetCell(j);

        oDocXML.Add(indInc, '<w:tc>');

        // Add the Cell Properties
        oDocXML.Add(indInc, '<w:tcPr>', indInc);

        oDocXML.Add(indNone, '<w:tcBorders>', indInc);

        AddTableBorderProperty('w:left', oCell.Borders.Left);
        AddTableBorderProperty('w:right', oCell.Borders.Right);
        AddTableBorderProperty('w:top', oCell.Borders.Top);
        AddTableBorderProperty('w:bottom', oCell.Borders.Bottom);

        oDocXML.Add(indNone, '</w:tcBorders>', indDec);

        // Row background color can't be applied at the row level, so we'll
        // apply it to each cell in turn (only if the cell doesn't have it's
        // own value assigned)
        If oCell.BackgroundColor <> FPColor(0, 0, 0, 0) Then
          oDocXML.Add(Format('<w:shd w:val="clear" w:color="auto" w:fill="%s"/>',
            [FPColorToRGBHexString(oCell.BackgroundColor)]))
        Else If oRow.BackgroundColor <> FPColor(0, 0, 0, 0) Then
          oDocXML.Add(Format('<w:shd w:val="clear" w:color="auto" w:fill="%s"/>',
            [FPColorToRGBHexString(oRow.BackgroundColor)]));

        // Either use Cell Preferred Width, or ColWidths if defined as %
        If oCell.PreferredWidth.Value <> 0 Then
          oDocXML.Add(Format('<w:tcW %s />', [DimAttribs(oCell.PreferredWidth)]))
        Else If (j <= High(ATable.ColWidths)) Then
          oDocXML.Add(Format('<w:tcW %s />',
            [DimAttribs(Dimension(ATable.ColWidths[j],
            ATable.ColWidthsUnits))]));

        If ATable.ColWidthsUnits <> dimPercent Then
          oDocXML.Add('<w:gridSpan w:val="' + IntToStr(oCell.SpannedCols) + '" />');

        oDocXML.Add('<w:vAlign w:val="' + LU_V_ALIGN[oCell.VerticalAlignment] + '" />');

        oDocXML.Add(indDec, '</w:tcPr>', indDec);

        ProcessRichText(oCell);

        oDocXML.Add(indDec, '</w:tc>');
      End;

      oDocXML.Add(indDec, '</w:tr>');
    End;

    oDocXML.Add(indDec, '</w:tbl>');
  End;

(*
  Procedure ProcessImage(AImage : TvImage);
  begin

  end;
*)
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
      Else If oEntity Is TvList Then
        ProcessList(TvList(oEntity))
      Else If oEntity Is TvTable Then
        ProcessTable(TvTable(oEntity))
      Else If oEntity Is TvRichText Then
        ProcessRichText(TvRichText(oEntity))
(*
      Else If oEntity Is TvImage Then
        ProcessImage(TvImage(oEntity))
*)
      Else
        Raise Exception.Create('Unsupported entity ' + oEntity.ClassName);
    End;
  End;

Var
  oPage: TvPage;
  oPageSequence: TvTextPageSequence;
  oFile: TFileInformation;
Begin
  oFile := FFiles.AddXMLFile(OOXML_CONTENTTYPE_DOCUMENT, OOXML_PATH_DOCUMENT,
    OOXML_TYPE_DOCUMENT, [ftContentTypes, ftRelationships]);
  oDocXML := oFile.XML;

  oDocXML.Clear;
  oDocXML.Add(XML_HEADER);
  oDocXML.Add(Format('<w:document %s>', [OOXML_DOCUMENT_NAMESPACE +
    'xml:space="preserve"']));

  oDocXML.Add(indInc, '<w:body>');

  For iPage := 0 To FData.GetPageCount - 1 Do
  Begin
    oPage := FData.GetPageAsText(iPage);

    If oPage Is TvTextPageSequence Then
    Begin
      oPageSequence := TvTextPageSequence(oPage);

      ProcessRichText(oPageSequence.MainText);

      // Add any dimensions, headers, footers etc
      FinalisePage(oPageSequence, iPage = FData.GetPageCount - 1);
    End;
  End;

  oDocXML.Add(indDec, '</w:body>');
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
Begin
  ADoc.Add(indInc, '<w:rPr>', indInc);

  If (spbfFontName In AStyle.SetElements) And (AStyle.Font.Name <> '') Then
    ADoc.Add('<w:rFonts w:ascii="' + AStyle.Font.Name + '" w:hAnsi="' +
      AStyle.Font.Name + '"/>');

  If spbfFontSize In AStyle.SetElements Then
    { TODO : Where does the magic Font.Size*2 come from?  Confirm... }
    ADoc.Add('<w:sz w:val="' + IntToStr(2 * AStyle.Font.Size) + '"/>');

  If spbfFontBold In AStyle.SetElements Then
    ADoc.Add('<w:b w:val="' + LU_ON_OFF[AStyle.Font.Bold] + '"/>');

  If spbfFontItalic In AStyle.SetElements Then
    ADoc.Add('<w:i w:val="' + LU_ON_OFF[AStyle.Font.Italic] + '"/>');

  If spbfFontUnderline In AStyle.SetElements Then
    ADoc.Add('<w:u w:val="' + LU_ON_OFF[AStyle.Font.Underline] + '"/>');

  If spbfFontStrikeThrough In AStyle.SetElements Then
    ADoc.Add('<w:strike w:val="' + LU_ON_OFF[AStyle.Font.StrikeThrough] + '"/>');

  If CompareColors(AStyle.Font.Color, FPColor(0, 0, 0, 0)) <> 0 Then
    ADoc.Add('<w:color w:val="' + FPColorToRGBHexString(AStyle.Font.Color) + '"/>');

  // Can Word handled re-oriented text?  I hope not...
  If AStyle.Font.Orientation <> 0 Then;

  ADoc.DecIndent;

  // Don't bother adding an empty tag..
  If ADoc[ADoc.Count - 1] <> '<w:rPr>' Then
    ADoc.Add(indDec, '</w:rPr>')
  Else
  Begin
    ADoc.Delete(ADoc.Count - 1);
    ADoc.DecIndent;
  End;
End;

Procedure TvDOCXVectorialWriter.PrepareStyles;
Var
  sType, sTemp: String;
  i: Integer;
  oXML: TIndentedStringList;
  oStyle: TvStyle;
  oFile: TFileInformation;

Begin
  // Only add this file if there are any styles defined...
  If FData.GetStyleCount > 0 Then
  Begin
    oFile := FFiles.AddXMLFile(OOXML_CONTENTTYPE_STYLES, OOXML_PATH_STYLES,
      OOXML_TYPE_STYLES, [ftContentTypes, ftDocRelationships]);
    oXML := oFile.XML;

    oXML.Clear;
    oXML.Add(XML_HEADER);
    oXML.Add(Format('<w:styles %s>', [OOXML_DOCUMENT_NAMESPACE]));

    For i := 0 To FData.GetStyleCount - 1 Do
    Begin
      oStyle := FData.GetStyle(i);

      If oStyle.GetKind In [vskTextBody, vskHeading] Then
        sType := 'paragraph'
      Else If oStyle.GetKind In [vskTextSpan] Then
        sType := 'character'
      Else
        { TODO : handle the other StyleKinds }
        Raise Exception.Create('Unsupported StyleKind in ' + oStyle.Name);

      oXML.Add(indInc, '<w:style w:type="' + sType + '" w:styleId="' +
        StyleNameToStyleID(oStyle) + '">');

      // Add the name and inheritance values
      oXML.Add('  <w:name w:val="' + oStyle.Name + '"/>');

      If Assigned(oStyle.Parent) Then
        oXML.Add('  <w:basedOn w:val="' + StyleNameToStyleID(
          oStyle.Parent) + '"/> ');

      { TODO : <w:qFormat/> doesn't always need to be set, but I don't yet understand the rules...  }
      oXML.Add('  <w:qFormat/> ');   // Latent Style Primary Style Setting.

      { TODO : Specification states you CANNOT redeclare a identical property
               declared in a parent. At the moment code is relying on Styles
               correctly defined up through hierarchy }
      If sType = 'paragraph' Then
      Begin
        // Add the Paragraph Properties
        oXML.Add(indInc, '<w:pPr>', indInc);

        // Define Before and After spacing
        If (sseMarginTop In oStyle.SetElements) Or
          (sseMarginBottom In oStyle.SetElements) Then
        Begin
          sTemp := '';

          If sseMarginTop In oStyle.SetElements Then
            sTemp := sTemp + ' w:before="' + mmToTwipsS(oStyle.MarginTop) + '"';

          If sseMarginBottom In oStyle.SetElements Then
            sTemp := sTemp + ' w:after="' + mmToTwipsS(oStyle.MarginBottom) + '"';

          oXML.Add(Format('<w:spacing %s/>', [sTemp]));
        End;

        If (sseMarginLeft In oStyle.SetElements) Or
          (sseMarginRight In oStyle.SetElements) Then
        Begin
          sTemp := '';

          If sseMarginLeft In oStyle.SetElements Then
            sTemp := sTemp + ' w:left="' + mmToTwipsS(oStyle.MarginLeft) + '"';

          If sseMarginRight In oStyle.SetElements Then
            sTemp := sTemp + ' w:right="' + mmToTwipsS(oStyle.MarginRight) + '"';

          oXML.Add(Format('<w:ind %s/>', [sTemp]));
        End;


        // Alignment
        If spbfAlignment In oStyle.SetElements Then
          oXML.Add('<w:jc w:val="' + LU_ALIGN[oStyle.Alignment] + '"/>');

        // Suppress Spacing between identical paragraphs...
        If oStyle.SuppressSpacingBetweenSameParagraphs Then
          oXML.Add('<w:contextualSpacing/>');

        oXML.DecIndent;

        If oXML[oXML.Count - 1] <> '<w:pPr>' Then
          oXML.Add(indDec, '</w:pPr>')
        Else
        Begin
          oXML.Delete(oXML.Count - 1);
          oXML.DecIndent;
        End;
      End;

      // Now add the actual formatting (rPr = Run Properties).
      PrepareTextRunStyle(oXML, oStyle);

      oXML.Add(indDec, '</w:style>  ');
    End;

    oXML.Add('</w:styles>');
  End;
End;

Procedure TvDOCXVectorialWriter.PrepareNumbering;
Var
  oXML: TIndentedStringList;
  oStyle: TvListStyle;
  oFile: TFileInformation;
  i: Integer;
  j: Integer;
  oListLevelStyle: TvListLevelStyle;
  sTotalLeader: String;
  sCurrentLeader: String;
  slvlText: String;
Begin
  // Only add this file if there are any List styles defined...
  If FData.GetListStyleCount > 0 Then
  Begin
    oFile := FFiles.AddXMLFile(OOXML_CONTENTTYPE_NUMBERING,
      OOXML_PATH_NUMBERING, OOXML_TYPE_NUMBERING, [ftContentTypes, ftDocRelationships]);
    oXML := oFile.XML;

    oXML.Clear;
    oXML.Add(XML_HEADER);
    oXML.Add(Format('<w:numbering %s>', [OOXML_DOCUMENT_NAMESPACE]));

    For i := 0 To FData.GetListStyleCount - 1 Do
    Begin
      oStyle := FData.GetListStyle(i);

      // abstractNumID allows us to group different list styles together.
      // The way fpvectorial uses it, there will be a one to one relationship
      // between abstractNumID and numID.
      //   abstractNumId is 0 based
      //   numID is 1 based.   Go figure...
      oXML.Add(indInc, Format('<w:abstractNum w:abstractNumId="%d">', [i]), indInc);

      sTotalLeader := '';

      For j := 0 To oStyle.GetListLevelStyleCount-1 Do
      Begin
        oListLevelStyle := oStyle.GetListLevelStyle(j);

        oXML.Add(Format('<w:lvl w:ilvl="%d">', [oListLevelStyle.Level]), indInc);

        with oListLevelStyle do
          sCurrentLeader := Format('%s%s%d%s', [Prefix, '%', Level + 1, Suffix]);

        sTotalLeader := sTotalLeader + sCurrentLeader;

        If oListLevelStyle.Kind=vlskBullet Then
          slvlText := oListLevelStyle.Bullet
        Else If oListLevelStyle.DisplayLevels Then
          slvlText := sTotalLeader
        Else
          slvlText := sCurrentLeader;

        If oListLevelStyle.Kind=vlskBullet Then
          oXML.Add('<w:numFmt w:val="bullet"/>')
        Else
        Begin // Numbered Lists
          oXML.Add(Format('<w:start w:val="%d"/>', [oListLevelStyle.Start]));

          oXML.Add('<w:numFmt w:val="' + LU_NUMBERFORMAT[oListLevelStyle.NumberFormat] + '"/>');
        End;

        oXML.Add('<w:lvlText w:val="' + slvlText + '"/>');
        oXML.Add('<w:lvlJc w:val="' + LU_ALIGN[oListLevelStyle.Alignment] + '"/>');

        oXML.Add('<w:pPr>');
        oXML.Add(Format('  <w:ind w:left="%s" w:hanging="%s"/>',
          [mmToTwipsS(oListLevelStyle.MarginLeft), mmToTwipsS(oListLevelStyle.HangingIndent)]));
        oXML.Add('</w:pPr>');

        oXML.Add('<w:rPr>');
        oXML.Add(Format('  <w:rFonts w:ascii="%s" w:hAnsi="%s"/>',
          [oListLevelStyle.LeaderFontName, oListLevelStyle.LeaderFontName]));
        oXML.Add('</w:rPr>');

        oXML.Add('</w:lvl>', indDec);
      end;

      oXML.Add(indDec, '</w:abstractNum>', indDec);
    End;

    For i := 0 To FData.GetListStyleCount - 1 Do
    begin
      oXML.Add(indInc, Format('<w:num w:numId="%d">', [i + 1]));
      oXML.Add(Format('  <w:abstractNumId w:val="%d"/>', [i]));
      oXML.Add(indDec, '</w:num>');
    end;

    oXML.Add('</w:numbering>');
  End;
End;

Procedure TvDOCXVectorialWriter.WriteToFile(AFileName: String;
  AData: TvVectorialDocument);
Var
  oStream: TFileStream;
Begin
  If ExtractFileExt(AFilename) = '' Then
    AFilename := AFilename + STR_DOCX_EXTENSION;

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
  PrepareNumbering;

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
